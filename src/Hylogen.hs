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
       , module Hylogen.CSE
       )
       where

import           Hylogen.CSE
import           Hylogen.Globals
import           Hylogen.Types   (Vec (fromVec1, select, toList),
                                  Vec1 (W, X, Y, Z), Vec2, Vec3, Vec4)

toGLSL :: Vec4 -> String
toGLSL x = unlines $ [ "void main() {"
                     , "    gl_FragColor = " ++ show x ++ ";"
                     , "}"
                     ]
