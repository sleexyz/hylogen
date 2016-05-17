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
import           Data.List
import           Hylogen.Globals
import           Hylogen.Types

toGLSL' :: Vec4 -> String
toGLSL' v = unlines [ "void main() {"
                    , "    gl_FragColor = " <> show v <> ";"
                    , "}"
                    ]


toGLSL :: Vec4 -> String
toGLSL = show . toProgram . toMono
