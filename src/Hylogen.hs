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
import           Hylogen.CSE     (contextToAssignments, getTopLevel, genContext)
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
    assignments = mconcat . fmap ("\n    "<>) $ contextToAssignments glsl
    glsl = genContext v
    topLevel = getTopLevel glsl

