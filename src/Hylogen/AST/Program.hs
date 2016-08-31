{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hylogen.AST.Program where

import Hylogen.AST.Expr
import Hylogen.AST.Statement

newtype FId = FId Int

newtype Program = Program [Function]
  deriving (Monoid, Show)


newtype Function = Function CodeBlock
  deriving (Monoid, Show)


