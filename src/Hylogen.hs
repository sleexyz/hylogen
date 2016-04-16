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

import           Hylogen.Types ( Vec1 (X, Y, Z, W)
                               , Vec2
                               , Vec3
                               , Vec4
                               , Vec (select, fromVec1, toList)
                               )
import           Hylogen.Globals

toGLSL :: Vec4 -> String
toGLSL x = unlines $ [ "void main() {"
                     , "    gl_FragColor = " ++ show x ++ ";"
                     , "}"
                     ]
