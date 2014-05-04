{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Data.CfnGraph where

import qualified Data.CfnGraph.Template as T

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
type Description = Text

data Resource a where
  Stack :: Description -> [Resource AutoScalingGroup] -> Resource Stack

  ASG :: Name
      -> Capacity
      -> [Tag]
      -> Resource LaunchConfig
      -> [Resource LoadBalancer]
      -> Resource AutoScalingGroup

  LC  :: Name
      -> KeyName
      -> ImageId
      -> UserData
      -> [Resource SecurityGroup]
      -> Resource LaunchConfig

  SG  :: Name -> [ResIngress] -> Resource SecurityGroup

  LB  :: Name
      -> [Listener]
      -> HealthCheck
      -> [Resource SecurityGroup]
      -> Resource LoadBalancer

data ResIngress = ResIngress IpProtocol Port Port ResIngressSource

data ResIngressSource = ResIpSource CidrIp
                      | ResSGSource (Resource SecurityGroup)

fromResource :: Resource a -> a
fromResource res = case res of
  SG name ings -> SecurityGroup name $ map (\(ResIngress prot from to src) ->
    Ingress prot from to $ case src of
      ResIpSource cIp -> IpSource cIp
      ResSGSource sg  -> SGSource $ fromResource sg) ings
  LB name ls hc sgs -> LoadBalancer name ls hc $ map fromResource sgs
  ASG name cap tags lconf lbs ->
    AutoScalingGroup name cap tags (fromResource lconf) (map fromResource lbs)
  LC name key ami udata sgs ->
    LaunchConfig name key ami udata $ map fromResource sgs
  Stack desc asgs -> St desc $ map fromResource asgs

data ResourceGraph a s where

  GraphSt :: Description
          -> [ResourceGraph AutoScalingGroup s]
          -> ResourceGraph Stack s

  GraphASG :: Name
           -> Capacity
           -> [Tag]
           -> ResourceGraph LaunchConfig s
           -> [ResourceGraph LoadBalancer s]
           -> ResourceGraph AutoScalingGroup s

  GraphLC :: Name
          -> KeyName
          -> ImageId
          -> UserData
          -> [ResourceGraph SecurityGroup s]
          -> ResourceGraph LaunchConfig s

  GraphSG :: Name
          -> [IngressGraph s]
          -> ResourceGraph SecurityGroup s

  GraphLB :: Name
          -> [Listener]
          -> HealthCheck
          -> [ResourceGraph SecurityGroup s]
          -> ResourceGraph LoadBalancer s

  Ref :: ResourceType a -> s -> ResourceGraph a s

data IngressGraph s where
  IngGraph :: IpProtocol -> Port -> Port -> IngressSourceGraph s -> IngressGraph s

data IngressSourceGraph s where
  GraphIpSrc :: CidrIp -> IngressSourceGraph s
  GraphSGSrc :: ResourceGraph SecurityGroup s -> IngressSourceGraph s

data WrappedGraph s where
  Wrap :: ResourceType a -> ResourceGraph a s -> WrappedGraph s

instance MuRef (Resource a) where
  type DeRef (Resource a) = WrappedGraph

  mapDeRef f a = Wrap (getResourceType a) <$> mapDeRef' f a where
    mapDeRef' :: Applicative f
              => (forall b. (MuRef b, WrappedGraph ~ DeRef b) => b -> f u)
              -> Resource a
              -> f (ResourceGraph a u)
    mapDeRef' g (SG name ings) =
      GraphSG name <$> traverse (\(ResIngress prot from to src) ->
        IngGraph prot from to <$> case src of
          ResIpSource cIp -> GraphIpSrc <$> pure cIp
          ResSGSource sg  -> GraphSGSrc <$> (Ref SGType <$> g sg)
        ) ings
    mapDeRef' g (LB name ls hc sgs) =
      GraphLB name ls hc <$> traverse (fmap (Ref SGType) . g) sgs
    mapDeRef' g (ASG name cap tags lconf lbs) =
      GraphASG name cap tags <$> (Ref LCType <$> g lconf)
                        <*> traverse (fmap (Ref LBType) . g) lbs
    mapDeRef' g (LC name key ami udata sgs) = 
      GraphLC name key ami udata <$> traverse (fmap (Ref SGType) . g) sgs
    mapDeRef' g (Stack desc asgs) =
      GraphSt desc <$> traverse (fmap (Ref ASGType) . g) asgs

data ResourceType a where
  ASGType :: ResourceType AutoScalingGroup
  LCType  :: ResourceType LaunchConfig
  LBType  :: ResourceType LoadBalancer
  SGType  :: ResourceType SecurityGroup
  StType  :: ResourceType Stack

getResourceType :: Resource a -> ResourceType a
getResourceType st = case st of
  ASG    {} -> ASGType
  LC     {} -> LCType
  SG     {} -> SGType
  LB     {} -> LBType
  Stack  {} -> StType

data Refl a b where Refl :: Refl a a

typeEq :: ResourceType a -> ResourceType b -> Maybe (Refl a b)
typeEq LBType LBType   = Just Refl
typeEq LCType LCType   = Just Refl
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
data Stack = St Description [AutoScalingGroup] deriving Show

data AutoScalingGroup =
  AutoScalingGroup
    Name
    Capacity
    [Tag]
    LaunchConfig
    [LoadBalancer]
  deriving Show

data Capacity = Capacity Int Int Int
                deriving Show

data Tag = Tag Text Text Bool
           deriving Show

type KeyName = Text
type InstanceType = Text
type ImageId = Text
type UserData = ByteString

data LaunchConfig = LaunchConfig Name KeyName ImageId UserData [SecurityGroup]
                    deriving Show

data LoadBalancer = LoadBalancer Name [Listener] HealthCheck [SecurityGroup]
                    deriving Show

data SecurityGroup = SecurityGroup Name [Ingress] deriving Show

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
  show StType = "Stack"

instance Show (WrappedGraph RefName) where
  show (Wrap t a) = concat ["(", show a, " :: ", show t, ")"]

instance Show s => Show (ResourceGraph a s) where
  show (Ref t n)    = unwords ["(Ref", show t, show n] ++ ")"
  show (GraphSG name is) = unwords ["(GraphSG", show name, show is] ++ ")"
  show (GraphLB name ls hc sgs) =
    unwords ["(GraphLB", show name, show ls, show hc, show sgs] ++ ")"
  show (GraphASG name cap tags lconf lbs) =
    unwords ["(GraphASG", show name, show cap, show tags, show lconf, show lbs] ++ ")"
  show (GraphSt desc asgs) =
    unwords ["(GraphStack", show desc, show asgs] ++ ")"
  show (GraphLC name key ami udata sgs) =
    unwords ["(GraphLC", show name, show key, show ami, show udata, show sgs] ++ ")"

instance Show s => Show (IngressGraph s) where
  show (IngGraph prot from to src) =
    unwords ["(IngressGraph", show prot, show from, show to, show src] ++ ")"

instance Show s => Show (IngressSourceGraph s) where
  show (GraphIpSrc cidrIp) = "(GraphIpSrc " ++ show cidrIp ++ ")"
  show (GraphSGSrc sg) = "(GraphSGSrc " ++ show sg ++ ")"
