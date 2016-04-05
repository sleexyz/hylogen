{-# LANGUAGE DataKinds                 #-}
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
{-# LANGUAGE TypeFamilies              #-}

module Hylogen
       ( module Hylogen
       , module Hylogen.Types
       , module Hylogen.Globals
       )
       where

import           Data.Monoid
import           Hylogen.Types
import           Hylogen.Globals

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
                       , "uniform vec4 audio;"
                       , "const float PI = 3.141592653589793238462643383; "
                       , "varying vec3 uv;"
                       ]
