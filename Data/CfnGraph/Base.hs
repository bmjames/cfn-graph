
module Data.CfnGraph.Base where

import Data.Text       (Text)
import Data.ByteString (ByteString)


type Name        = Text
type Description = Text
type Port        = Int
type Timeout     = Int

data Stack = St Description [AutoScalingGroup] deriving Show

data AutoScalingGroup =
  AutoScalingGroup
    Name
    Capacity
    [Tag]
    LaunchConfig
    [LoadBalancer]
  deriving Show

data Capacity = Capacity Int Int Int deriving Show

data Tag = Tag Text Text Bool deriving Show

type KeyName      = Text
type InstanceType = Text
type ImageId      = Text
type UserData     = ByteString

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
