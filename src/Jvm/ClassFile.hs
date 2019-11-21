module Jvm.ClassFile where

import Data.Word (Word8)

u1ToInt :: [Word8] -> Int
u1ToInt (i : _) = fromIntegral i

-- TODO
u2ToInt :: [Word8] -> Int
u2ToInt (_ : i : _) = fromIntegral i