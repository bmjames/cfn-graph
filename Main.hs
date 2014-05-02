{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CfnGraph


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
