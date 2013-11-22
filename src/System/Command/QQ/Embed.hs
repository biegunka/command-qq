{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
 -- Haskell values embedding
module System.Command.QQ.Embed
  ( Embed(..)
  ) where

import Control.Applicative
import Data.Int
import Data.Ratio (Ratio)
import Data.Word
import Foreign.C.Types

-- $setup
-- >>> import Data.Ratio


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

instance Embed Integer
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
instance Embed Float
instance Embed Double

instance Embed CChar
instance Embed CSChar
instance Embed CUChar
instance Embed CShort
instance Embed CUShort
instance Embed CInt
instance Embed CUInt
instance Embed CLong
instance Embed CULong
instance Embed CSize
instance Embed CLLong
instance Embed CULLong
instance Embed CFloat
instance Embed CDouble

-- |
-- >>> embed (3 % 5)
-- "0.6"
instance a ~ Integer => Embed (Ratio a) where
  embed = embed . (fromRational :: Rational -> Double)

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
