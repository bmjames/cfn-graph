{-# LANGUAGE OverloadedStrings #-}

module Data.CfnGraph.Template where

import Data.Aeson
import Data.Map  (Map)
import Data.Text (Text)

type Version = Text
type Description = Text
type Type = Text
type Name = Text
type Resource = Value

data Template =
  Template
    Version
    Description
    (Map Name Resource)
  deriving Show

instance ToJSON Template where
  toJSON (Template ver desc resources) = object 
    [ "AWSTemplateFormatVersion" .= ver
    , "Description" .= desc
    , "Resources" .= resources
    ]
