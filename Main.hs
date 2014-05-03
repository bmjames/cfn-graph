{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CfnGraph
import Data.Map  (toList)


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
                  , Ing TCP 9000 9000 (ResSGSource lbSecGroup)
                  ]
    sshIngress  = Ing TCP 22 22 ipSource

    ipSource = ResIpSource $ CIp "77.91.248.0" 21

main :: IO ()
main = conv myStack >>= mapM_ print . toList . fst
