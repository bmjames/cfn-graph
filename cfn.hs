{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative

import Data.Map
import Data.Reify
import Data.Text        (Text)
import Data.Traversable (traverse)

--------------------------------------------------------------------------------
type Port = Int
type Timeout = Int

data Resource a where

  SG  :: [Ingress] -> Resource SecurityGroup

  LB  :: [Listener]
      -> HealthCheck
      -> [Resource SecurityGroup]
      -> Resource LoadBalancer

data ResourceGraph a s where

  GraphSG :: [Ingress] -> ResourceGraph SecurityGroup s

  GraphLB :: [Listener]
          -> HealthCheck
          -> [ResourceGraph SecurityGroup s]
          -> ResourceGraph LoadBalancer s

  Ref :: ResourceType a -> s -> ResourceGraph a s

data WrappedGraph s where
  Wrap :: ResourceType a -> ResourceGraph a s -> WrappedGraph s

instance MuRef (Resource a) where
  type DeRef (Resource a) = WrappedGraph

  mapDeRef f a = Wrap (getResourceType a) <$> mapDeRef' f a where
    mapDeRef' :: Applicative f
              => (forall b. (MuRef b, WrappedGraph ~ DeRef b) => b -> f u)
              -> Resource a
              -> f (ResourceGraph a u)
    mapDeRef' _ (SG igs) = pure $ GraphSG igs
    mapDeRef' g (LB ls hc sgs) =
      GraphLB ls hc <$> traverse (fmap (Ref SGType) . g) sgs

data ResourceType a where
  LBType :: ResourceType LoadBalancer
  SGType :: ResourceType SecurityGroup

getResourceType :: Resource a -> ResourceType a
getResourceType st = case st of
  SG {} -> SGType
  LB {} -> LBType

data Refl a b where Refl :: Refl a a

typeEq :: ResourceType a -> ResourceType b -> Maybe (Refl a b)
typeEq LBType LBType = Just Refl
typeEq SGType SGType = Just Refl
typeEq _      _      = Nothing

type RefName = Unique

getRef :: Map RefName (WrappedGraph RefName)
       -> ResourceType a
       -> RefName
       -> Maybe (ResourceGraph a RefName)
getRef m t n = case m ! n of
  Wrap t' e -> (\Refl -> e) <$> typeEq t t'

conv :: Resource a -> IO (Map RefName (WrappedGraph RefName), Maybe (ResourceGraph a RefName))
conv e = do
  Graph l n <- reifyGraph e
  let m = fromList l
  return (m, getRef m (getResourceType e) n)

--------------------------------------------------------------------------------
data LoadBalancer = LoadBalancer [Listener] HealthCheck [SecurityGroup]
                    deriving Show

data SecurityGroup = SecurityGroup [Ingress] deriving Show

data Ingress = Ingress IpProtocol Port Port CidrIp
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

instance Show (WrappedGraph RefName) where
  show (Wrap t a) = concat ["(", show a, " :: ", show t, ")"]

instance Show s => Show (ResourceGraph a s) where
  show (Ref t n)    = unwords ["(Ref", show t, show n] ++ ")"
  show (GraphSG is) = "(GraphSG " ++ show is ++ ")"
  show (GraphLB ls hc sgs) =
    unwords ["(GraphLB", show ls, show hc, show sgs] ++ ")" 

--------------------------------------------------------------------------------
myStack :: Resource LoadBalancer
myStack =
  LB [httpListener] healthCheck [sshSecGroup]

  where
    httpListener = Listener 9000 80 HTTP
    healthCheck  = HealthCheck HTTP 9000 "/management/healthcheck" 5
    sshSecGroup  = SG [httpIngress]
    httpIngress  = Ingress TCP 80 9000 (CIp "77.91.248.0" 21)

main :: IO ()
main = conv myStack >>= print
