{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CfnGraph
import Data.Map  (toList)
import Data.Text (Text)


myStack :: Resource Stack
myStack =
  Stack [asg]

  where
    asg          = ASG (Capacity 1 2 1) [] launchConf [loadBalancer]
    launchConf   = LC "my-key" "ami-foobar" udata [sshSecGroup, appSecGroup]
    udata        = ""
    loadBalancer = LB [httpListener] healthCheck [lbSecGroup]
    httpListener = Listener 9000 80 HTTP
    healthCheck  = HealthCheck HTTP 9000 "/management/healthcheck" 5
    
    sshSecGroup  = SG [sshIngress]
    lbSecGroup   = SG []
    appSecGroup  = SG appIngress
    
    appIngress  = [ Ing TCP 9000 9000 ipSource
                  , Ing TCP 9000 9000 (fromGroup lbSecGroup)
                  ]
    sshIngress  = Ing TCP 22 22 ipSource

    ipSource = fromIp "77.91.248.0" 21

fromIp :: Text -> Int -> Resource IngressSource
fromIp ip bits = IngSrc . ResIpSource $ CIp ip bits

fromGroup :: Resource SecurityGroup -> Resource IngressSource
fromGroup = IngSrc . ResSGSource

main :: IO ()
main = conv myStack >>= mapM_ print . toList . fst
