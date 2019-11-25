module Jvm.ClassFile.ConstantPool where

import Data.Map.Strict as Map hiding (drop, map, take)
import Data.Word (Word8)
import Jvm.ClassFile

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4-140
readConstantInfo :: [Word8] -> ([Word8], ConstantInfo)
readConstantInfo b = r $ drop 1 b
    where r = case u1ToInt b of
                1   -> readUtf8Info
                3   -> error "IntegerInfo"
                4   -> error "FloatInfo"
                5   -> error "LongInfo"
                6   -> error "DoubleInfo"
                7   -> readClassInfo
                8   -> readStringInfo
                9   -> readFieldRefInfo
                10  -> readMethodRefInfo
                11  -> readInterfaceMethodRefInfo
                12  -> readNameAndTypeInfo
                15  -> error "MethodHandleInfo"
                16  -> error "MethodTypeInfo"
                18  -> error "InvokeDynamicInfo"

data ConstantInfo =
    ClassInfo Int
  | FieldRefInfo Int Int
  | MethodRefInfo Int Int
  | InterfaceMethodRefInfo Int Int
  | StringInfo Int
  | Utf8Info Int String
  | NameAndTypeInfo Int Int
  | IntegerInfo Int
  | FloatInfo Int
  | LongInfo Int Int
  | DoubleInfo Int Int
  | MethodHandleInfo Int Int
  | MethodTypeInfo Int
  | InvokeDynamicInfo Int Int deriving Show

readUtf8Info :: [Word8] -> ([Word8], ConstantInfo)
readUtf8Info w = (w', Utf8Info l s)
    where w'    = drop l x
          l     = u2ToInt w
          s     = map (toEnum . fromIntegral) b
          b     = take l x
          x     = drop 2 w

readIndexedInfo :: (Int -> ConstantInfo) -> [Word8] -> ([Word8], ConstantInfo)
readIndexedInfo f w = (w', f i)
    where w'    = drop 2 w
          i     = u2ToInt w

readClassInfo   = readIndexedInfo ClassInfo
readStringInfo  = readIndexedInfo StringInfo

readFieldRefInfo            = readRefInfo FieldRefInfo
readMethodRefInfo           = readRefInfo MethodRefInfo
readInterfaceMethodRefInfo  = readRefInfo InterfaceMethodRefInfo
readNameAndTypeInfo         = readRefInfo NameAndTypeInfo

readRefInfo :: (Int -> Int -> ConstantInfo) -> [Word8] -> ([Word8], ConstantInfo)
readRefInfo f w = (w', f c n)
    where w'    = drop 2 x
          c     = u2ToInt w
          n     = u2ToInt x
          x     = drop 2 w

readConstantInfos :: Int -> [Word8] -> [ConstantInfo]
readConstantInfos s b = Map.elems $ readConstantInfoMap s b

readConstantInfoMap :: Int -> [Word8] -> Map Int ConstantInfo
readConstantInfoMap s b = go 1 b Map.empty
    where go i b m
            | i < s     = go (i + 1) b' (Map.insert i c m)
            | otherwise = m
            where (b', c) = readConstantInfo b