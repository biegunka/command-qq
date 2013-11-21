{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
 -- Haskell values embedding
module System.Command.QQ.Embed
  ( Embed(..)
  ) where

import Control.Applicative
import Data.Int
import Data.Word


-- | Embed haskell values into external commands
--
-- I recommend using @-XExtendedDefaultRules@ for modules
-- where you want to embed values, it would save for annoying
-- type annotations for numeric literals
--
-- @
-- embed . embed = embed
-- @
class Embed a where
  embed :: a -> String
  default embed :: Show a => a -> String
  embed = show

-- |
-- >>> embed 4
-- "4"
--
-- >>> embed (7 :: Integer)
-- "7"
instance Embed Integer

-- |
-- >>> embed (7 :: Int)
-- "7"
instance Embed Int
instance Embed Int8
instance Embed Int16
instance Embed Int32
instance Embed Int64
instance Embed Word
instance Embed Word8
instance Embed Word16
instance Embed Word32
instance Embed Word64

-- |
-- >>> embed (7 :: Float)
-- "7.0"
instance Embed Float

-- |
-- >>> embed 4.0
-- "4.0"
--
-- >>> embed (7 :: Double)
-- "7.0"
instance Embed Double

-- |
-- >>> embed 'c'
-- "c"
instance Embed Char where
  embed = pure

-- |
-- >>> embed "hi"
-- "hi"
instance Embed String where
  embed = id
