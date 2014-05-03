{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Data.CfnGraph where

import Control.Applicative

import Data.ByteString  (ByteString)
import Data.Map         (Map, (!), fromList)
import Data.Reify
import Data.Text        (Text)
import Data.Traversable (traverse)

--------------------------------------------------------------------------------
type Port = Int
type Timeout = Int

type Name = Text

data Resource a where
  Stack :: [Resource AutoScalingGroup] -> Resource Stack

  ASG :: Capacity
      -> [Tag]
      -> Resource LaunchConfig
      -> [Resource LoadBalancer]
      -> Resource AutoScalingGroup

  LC  :: KeyName
      -> ImageId
      -> UserData
      -> [Resource SecurityGroup]
      -> Resource LaunchConfig

  Ing :: IpProtocol
      -> Port
      -> Port
      -> Resource IngressSource
      -> Resource Ingress

  IngSrc :: ResIngressSource a -> Resource IngressSource

  SG  :: [Resource Ingress] -> Resource SecurityGroup

  LB  :: [Listener]
      -> HealthCheck
      -> [Resource SecurityGroup]
      -> Resource LoadBalancer

fromResource :: Resource a -> a
fromResource res = case res of
  SG ings -> SecurityGroup $ map fromResource ings
  Ing ip from to src -> Ingress ip from to $ fromResource src
  IngSrc (ResIpSource cIp) -> IpSource cIp
  IngSrc (ResSGSource sg) -> SGSource $ fromResource sg
  LB ls hc sgs -> LoadBalancer ls hc $ map fromResource sgs
  ASG cap tags lconf lbs ->
    AutoScalingGroup cap tags (fromResource lconf) (map fromResource lbs)
  LC key ami udata sgs ->
    LaunchConfig key ami udata $ map fromResource sgs
  Stack asgs -> St $ map fromResource asgs

data ResIngressSource a where
  ResIpSource :: CidrIp -> ResIngressSource CidrIp
  ResSGSource :: Resource SecurityGroup -> ResIngressSource SecurityGroup

data ResourceGraph a s where

  GraphSt :: [ResourceGraph AutoScalingGroup s]
          -> ResourceGraph Stack s

  GraphASG :: Capacity
           -> [Tag]
           -> ResourceGraph LaunchConfig s
           -> [ResourceGraph LoadBalancer s]
           -> ResourceGraph AutoScalingGroup s

  GraphLC :: KeyName
          -> ImageId
          -> UserData
          -> [ResourceGraph SecurityGroup s]
          -> ResourceGraph LaunchConfig s

  GraphIng :: IpProtocol
           -> Port
           -> Port
           -> ResourceGraph IngressSource s
           -> ResourceGraph Ingress s

  GraphIngSrc :: IngressSourceGraph a s -> ResourceGraph a s

  GraphSG :: [ResourceGraph Ingress s]
          -> ResourceGraph SecurityGroup s

  GraphLB :: [Listener]
          -> HealthCheck
          -> [ResourceGraph SecurityGroup s]
          -> ResourceGraph LoadBalancer s

  Ref :: ResourceType a -> s -> ResourceGraph a s


data IngressSourceGraph a s where
  GraphIpSrc :: CidrIp -> IngressSourceGraph IngressSource s
  GraphSGSrc :: ResourceGraph SecurityGroup s
             -> IngressSourceGraph IngressSource s

data WrappedGraph s where
  Wrap :: ResourceType a -> ResourceGraph a s -> WrappedGraph s

instance MuRef (Resource a) where
  type DeRef (Resource a) = WrappedGraph

  mapDeRef f a = Wrap (getResourceType a) <$> mapDeRef' f a where
    mapDeRef' :: Applicative f
              => (forall b. (MuRef b, WrappedGraph ~ DeRef b) => b -> f u)
              -> Resource a
              -> f (ResourceGraph a u)
    mapDeRef' g (SG ings) =
      GraphSG <$> traverse (fmap (Ref IngType) . g) ings
    mapDeRef' g (Ing prot from to source) =
      GraphIng prot from to <$> (Ref IngSrcType <$> g source)
    mapDeRef' g (IngSrc src) = GraphIngSrc <$> case src of
      ResIpSource cIp -> GraphIpSrc <$> pure cIp
      ResSGSource sg  -> GraphSGSrc <$> (Ref SGType <$> g sg)
    mapDeRef' g (LB ls hc sgs) =
      GraphLB ls hc <$> traverse (fmap (Ref SGType) . g) sgs
    mapDeRef' g (ASG cap tags lconf lbs) =
      GraphASG cap tags <$> (Ref LCType <$> g lconf)
                        <*> traverse (fmap (Ref LBType) . g) lbs
    mapDeRef' g (LC key ami udata sgs) = 
      GraphLC key ami udata <$> traverse (fmap (Ref SGType) . g) sgs
    mapDeRef' g (Stack asgs) =
      GraphSt <$> traverse (fmap (Ref ASGType) . g) asgs

data ResourceType a where
  ASGType :: ResourceType AutoScalingGroup
  LCType  :: ResourceType LaunchConfig
  LBType  :: ResourceType LoadBalancer
  IngType :: ResourceType Ingress
  IngSrcType :: ResourceType IngressSource
  SGType  :: ResourceType SecurityGroup
  StType  :: ResourceType Stack

getResourceType :: Resource a -> ResourceType a
getResourceType st = case st of
  ASG    {} -> ASGType
  LC     {} -> LCType
  Ing    {} -> IngType
  IngSrc {} -> IngSrcType
  SG     {} -> SGType
  LB     {} -> LBType
  Stack  {} -> StType

data Refl a b where Refl :: Refl a a

typeEq :: ResourceType a -> ResourceType b -> Maybe (Refl a b)
typeEq LBType LBType   = Just Refl
typeEq LCType LCType   = Just Refl
typeEq IngType IngType = Just Refl
typeEq IngSrcType IngSrcType = Just Refl
typeEq SGType SGType   = Just Refl
typeEq ASGType ASGType = Just Refl
typeEq StType StType   = Just Refl
typeEq _      _        = Nothing

type RefName = Unique

getRef :: Map RefName (WrappedGraph RefName)
       -> ResourceType a
       -> RefName
       -> Maybe (ResourceGraph a RefName)
getRef m t n = case m ! n of
  Wrap t' e -> (\Refl -> e) <$> typeEq t t'

conv :: Resource a
     -> IO (Map RefName (WrappedGraph RefName), Maybe (ResourceGraph a RefName))
conv e = do
  Graph l n <- reifyGraph e
  let m = fromList l
  return (m, getRef m (getResourceType e) n)

--------------------------------------------------------------------------------
data Stack = St [AutoScalingGroup]

data AutoScalingGroup = AutoScalingGroup Capacity [Tag] LaunchConfig [LoadBalancer]

data Capacity = Capacity Int Int Int
                deriving Show

data Tag = Tag Text Text Bool
           deriving Show

type KeyName = Text
type InstanceType = Text
type ImageId = Text
type UserData = ByteString

data LaunchConfig = LaunchConfig KeyName ImageId UserData [SecurityGroup]
                    deriving Show

data LoadBalancer = LoadBalancer [Listener] HealthCheck [SecurityGroup]
                    deriving Show

data SecurityGroup = SecurityGroup [Ingress] deriving Show

data Ingress = Ingress IpProtocol Port Port IngressSource
               deriving Show

data IngressSource = IpSource CidrIp
                   | SGSource SecurityGroup
                     deriving Show

data IpProtocol = TCP | UDP deriving Show

data Protocol = HTTP | HTTPS deriving Show

data CidrIp = CIp Text Int -- TODO improve this
              deriving Show

data Listener = Listener Port Port Protocol
                deriving Show

data HealthCheck = HealthCheck Protocol Port FilePath Timeout
                   deriving Show

--------------------------------------------------------------------------------
instance Show (ResourceType a) where
  show SGType = "SecurityGroup"
  show LBType = "LoadBalancer"
  show LCType = "LaunchConfig"
  show ASGType = "AutoScalingGroup"
  show IngType = "Ingress"
  show IngSrcType = "IngressSource"
  show StType = "Stack"

instance Show (WrappedGraph RefName) where
  show (Wrap t a) = concat ["(", show a, " :: ", show t, ")"]

instance Show s => Show (ResourceGraph a s) where
  show (Ref t n)    = unwords ["(Ref", show t, show n] ++ ")"
  show (GraphSG is) = "(GraphSG " ++ show is ++ ")"
  show (GraphLB ls hc sgs) =
    unwords ["(GraphLB", show ls, show hc, show sgs] ++ ")"
  show (GraphASG cap tags lconf lbs) =
    unwords ["(GraphASG", show cap, show tags, show lconf, show lbs] ++ ")"
  show (GraphSt asgs) = "(GraphStack " ++ show asgs ++ ")"
  show (GraphLC key ami udata sgs) =
    unwords ["(GraphLC", show key, show ami, show udata, show sgs] ++ ")"
  show (GraphIng prot from to source) =
    unwords ["(GraphIng", show prot, show from, show to, show source] ++ ")"
  show (GraphIngSrc src) = "(GraphIngSrc " ++ show src ++ ")"

instance Show s => Show (IngressSourceGraph a s) where
  show (GraphIpSrc cidrIp) = "(GraphIpSrc " ++ show cidrIp ++ ")"
  show (GraphSGSrc sg) = "(GraphSGSrc " ++ show sg ++ ")"
