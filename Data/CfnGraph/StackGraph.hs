{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Data.CfnGraph.StackGraph where

import Data.CfnGraph.Stack
import Control.Applicative

import Data.Map         (Map, (!), fromList)
import Data.Reify
import Data.Traversable (traverse)

--------------------------------------------------------------------------------
data StackGraph a s where

  StackNode :: Description
          -> [StackGraph AutoScalingGroup s]
          -> StackGraph Stack s

  ASGNode :: Name
          -> Capacity
          -> [Tag]
          -> StackGraph LaunchConfig s
          -> [StackGraph LoadBalancer s]
          -> StackGraph AutoScalingGroup s

  LConfNode :: Name
            -> KeyName
            -> ImageId
            -> UserData
            -> [StackGraph SecurityGroup s]
            -> StackGraph LaunchConfig s

  SGNode :: Name
         -> [IngressNode s]
         -> StackGraph SecurityGroup s

  LBNode :: Name
         -> [Listener]
         -> HealthCheck
         -> [StackGraph SecurityGroup s]
         -> StackGraph LoadBalancer s

  Ref :: ResourceType a -> s -> StackGraph a s

data IngressNode s = IngressNode IpProtocol Port Port (IngressSourceNode s)

data IngressSourceNode s = IpSourceNode CidrIp
                         | SGSourceNode (StackGraph SecurityGroup s)

data WrappedGraph s where
  Wrap :: ResourceType a -> StackGraph a s -> WrappedGraph s

instance MuRef Stack where
  type DeRef a = WrappedGraph
  mapDeRef f a = Wrap getResourceType <$> mapDeRef' f a where
    mapDeRef' g (Stack desc asgs) =
      StackNode desc <$> traverse (fmap (Ref ASGType) . g) asgs

instance MuRef SecurityGroup where
  type DeRef a = WrappedGraph
  mapDeRef f a = Wrap getResourceType <$> mapDeRef' f a where
    mapDeRef' g (SecurityGroup name ings) =
      SGNode name <$> traverse (\(Ingress prot from to src) ->
        IngressNode prot from to <$> case src of
          IpSource cIp -> IpSourceNode <$> pure cIp
          SGSource sg  -> SGSourceNode <$> (Ref SGType <$> g sg)
        ) ings

instance MuRef LoadBalancer where
  type DeRef a = WrappedGraph
  mapDeRef f a = Wrap getResourceType <$> mapDeRef' f a where
    mapDeRef' g (LoadBalancer name ls hc sgs) =
      LBNode name ls hc <$> traverse (fmap (Ref SGType) . g) sgs

instance MuRef AutoScalingGroup where
  type DeRef a = WrappedGraph
  mapDeRef f a = Wrap getResourceType <$> mapDeRef' f a where
    mapDeRef' g (ASG name cap tags lconf lbs) =
      ASGNode name cap tags <$> (Ref LCType <$> g lconf)
                             <*> traverse (fmap (Ref LBType) . f) lbs

instance MuRef LaunchConfig where
  type DeRef a = WrappedGraph
  mapDeRef f a = Wrap getResourceType <$> mapDeRef' f a where
    mapDeRef' g (LaunchConfig name key ami udata sgs) =
      LConfNode name key ami udata <$> traverse (fmap (Ref SGType) . g) sgs

data ResourceType a where
  ASGType :: ResourceType AutoScalingGroup
  LCType  :: ResourceType LaunchConfig
  LBType  :: ResourceType LoadBalancer
  SGType  :: ResourceType SecurityGroup
  StType  :: ResourceType Stack

class MuRef a => Resource a where
  getResourceType :: ResourceType a

instance Resource Stack where
  getResourceType = StType

instance Resource AutoScalingGroup where
  getResourceType = ASGType

instance Resource LaunchConfig where
  getResourceType = LCType

instance Resource SecurityGroup where
  getResourceType = SGType

instance Resource LoadBalancer where
  getResourceType = LBType

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
       -> Maybe (StackGraph a RefName)
getRef m t n = case m ! n of
  Wrap t' e -> (\Refl -> e) <$> typeEq t t'

conv :: Resource a
     => a
     -> IO (Map RefName (WrappedGraph RefName), Maybe (StackGraph a RefName))
conv e = do
  Graph l n <- reifyGraph e
  let m = fromList l
  return (m, getRef m getResourceType n)

--------------------------------------------------------------------------------
instance Show (ResourceType a) where
  show SGType = "SecurityGroup"
  show LBType = "LoadBalancer"
  show LCType = "LaunchConfig"
  show ASGType = "AutoScalingGroup"
  show StType = "Stack"

instance Show (WrappedGraph RefName) where
  show (Wrap t a) = concat ["(", show a, " :: ", show t, ")"]

instance Show s => Show (StackGraph a s) where
  show (Ref t n)    = unwords ["(Ref", show t, show n] ++ ")"
  show (SGNode name is) = unwords ["(SGNode", show name, show is] ++ ")"
  show (LBNode name ls hc sgs) =
    unwords ["(LBNode", show name, show ls, show hc, show sgs] ++ ")"
  show (ASGNode name cap tags lconf lbs) =
    unwords ["(ASGNode", show name, show cap, show tags, show lconf, show lbs] ++ ")"
  show (StackNode desc asgs) =
    unwords ["(StackNode", show desc, show asgs] ++ ")"
  show (LConfNode name key ami udata sgs) =
    unwords ["(LConfNode", show name, show key, show ami, show udata, show sgs] ++ ")"

instance Show s => Show (IngressNode s) where
  show (IngressNode prot from to src) =
    unwords ["(IngressNode", show prot, show from, show to, show src] ++ ")"

instance Show s => Show (IngressSourceNode s) where
  show (IpSourceNode cidrIp) = "(IpSourceNode " ++ show cidrIp ++ ")"
  show (SGSourceNode sg) = "(SGSourceNode " ++ show sg ++ ")"
