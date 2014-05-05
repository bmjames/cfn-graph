{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CfnGraph

import Data.Map  (toList)
import Data.ByteString (ByteString)


myStack = Stack "My Stack" [myAsg]

myAsg = ASG "MyASG" (Capacity 1 2 1) [] launchConf [loadBalancer]

launchConf = LaunchConfig "MyLaunchConf"
                "my-key"
                "ami-foobar"
                userData
                [sshSecGroup, appSecGroup]

userData = ("" :: ByteString)

loadBalancer = LoadBalancer "ELB" [httpListener] healthCheck [lbSecGroup]

httpListener = Listener 9000 80 HTTP

healthCheck = HealthCheck HTTP 9000 "/management/healthcheck" 5

sshSecGroup = SecurityGroup "SSH" [sshIngress]
lbSecGroup  = SecurityGroup "ELB" []
appSecGroup = SecurityGroup "App" appIngress

appIngress  = [ Ingress TCP 9000 9000 ipSource
              , Ingress TCP 9000 9000 (SGSource lbSecGroup)
              ]
sshIngress  = Ingress TCP 22 22 ipSource

ipSource = IpSource $ CIp "77.91.248.0" 21
    
main :: IO ()
main = conv myStack >>= mapM_ print . toList . fst
