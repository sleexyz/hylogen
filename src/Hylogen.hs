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
import           Hylogen.CSE     (glslToAssignments, getTopLevel, genGLSL)
import           Hylogen.Globals
import           Hylogen.Types   (Vec (fromVec1, select, toList),
                                  Vec1 (W, X, Y, Z), Vec2, Vec3, Vec4)

toGLSL' :: Vec4 -> String
toGLSL' v = unlines [ "void main() {"
                    , "    gl_FragColor = " <> show v<> ";"
                    , "}"
                    ]


toGLSL :: Vec4 -> String
toGLSL v = unlines [ "void main() {"
                   , assignments
                   , ""
                   , "    gl_FragColor = " <> show topLevel <> ";"
                   , "}"
                   ]
  where
    assignments = mconcat . fmap ("\n    "<>) $ glslToAssignments glsl
    glsl = genGLSL v
    topLevel = getTopLevel glsl

