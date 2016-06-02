{-# LANGUAGE LambdaCase #-}
module GenSwizz where

import Data.Monoid
import Data.List

data Field = X | Y | Z | W
instance Show Field where
  show = \case
    X -> "x"
    Y -> "y"
    Z -> "z"
    W -> "w"

newtype Swizz = Swizz [Field]
instance Show Swizz where
  show (Swizz xs) = mconcat $ (show<$>) xs




swizzles :: [Swizz]
swizzles = mconcat $ ((genSwizzles . pure)<$>) [X, Y, Z, W]
  where
    genSwizzles :: [Field] -> [Swizz]
    genSwizzles expr
      | length expr < 4 = mconcat [ genSwizzles (X:expr)
                                  , genSwizzles (Y:expr)
                                  , genSwizzles (Z:expr)
                                  , genSwizzles (W:expr)
                                  , [Swizz expr]
                                  ]
      | otherwise       = [Swizz expr]

calcMin :: Swizz -> Int
calcMin (Swizz xs) = maximum . (minDim<$>) $ xs
  where
    minDim :: Field -> Int
    minDim = \case
      X -> 2
      Y -> 2
      Z -> 3
      W -> 4
calcOutDim :: Swizz -> Int
calcOutDim (Swizz xs) = length xs

-- x_ :: forall n . (Veccable n, n >= 2) => Vec n -> Vec 1

genText :: Swizz -> String
genText swiz = unlines [line1, line2]
  where
    line1 = mconcat [ name
                    , " :: "
                    , "forall n. (Veccable n, n >= "
                    , show (calcMin swiz)
                    , ") => Vec n -> Vec "
                    , show (calcOutDim swiz)
                    ]
    line2 = mconcat [ name
                    , " = mkSwizz "
                    , "\""
                    , show swiz
                    ,  "\""
                    ]

    name = show swiz <> "_"





main :: IO ()
main = putStrLn . unlines . (genText <$>) $ swizzles
