{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonoLocalBinds          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}

module Hylogen
       ( module Hylogen
       , module Types
       , module Globals
       )
       where

import           Data.Monoid
import           Types
import           Globals

toGLSL :: Vec4 -> String
toGLSL x = unlines $ [ boiler
                     , "void main() {"
                     , "    gl_FragColor = " <> show x <> ";"
                     , "}"
                     ]
  where
    boiler = unlines $ [ "precision mediump float;"
                       , "uniform float time;"
                       , "uniform vec3 mouse;"
                       , "const float PI = 3.141592653589793238462643383; "
                       , "varying vec3 uv;"
                       ]

run :: Vec4 -> IO()
run = writeFile "./app/src/shader.js" . toJS . toGLSL
  where
    toJS = ("module.exports = `\n"<>) . (<>"`;")
