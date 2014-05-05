{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CfnGraph

import Data.Map  (toList)
import Data.ByteString (ByteString)


myStack = Stack "My Stack" [myAsg]

myAsg = ASG "MyASG" (Capacity 1 2 1) [] launchConf [loadBalancer]

launchConf = LC "MyLaunchConf"
                "my-key"
                "ami-foobar"
                userData
                [sshSecGroup, appSecGroup]

userData = ("" :: ByteString)

loadBalancer = LB "ELB" [httpListener] healthCheck [lbSecGroup]

httpListener = Listener 9000 80 HTTP

healthCheck = HealthCheck HTTP 9000 "/management/healthcheck" 5

sshSecGroup = SG "SSH" [sshIngress]
lbSecGroup  = SG "ELB" []
appSecGroup = SG "App" appIngress

appIngress  = [ ResIngress TCP 9000 9000 ipSource
              , ResIngress TCP 9000 9000 (ResSGSource lbSecGroup)
              ]
sshIngress  = ResIngress TCP 22 22 ipSource

ipSource = ResIpSource $ CIp "77.91.248.0" 21
    
main :: IO ()
main = conv myStack >>= mapM_ print . toList . fst
