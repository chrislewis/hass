module Jvm.ClassFile.ConstantPool where

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
  | Utf8Info String
  | NameAndTypeInfo Int Int
  | IntegerInfo Int
  | FloatInfo Int
  | LongInfo Int Int
  | DoubleInfo Int Int
  | MethodHandleInfo Int Int
  | MethodTypeInfo Int
  | InvokeDynamicInfo Int Int deriving Show

readUtf8Info :: [Word8] -> ([Word8], ConstantInfo)
readUtf8Info w = (w', Utf8Info s)
    where w'    = drop (l + 2) w
          l     = u2ToInt w
          b     = take l $ drop 2 w
          s     = map (toEnum . fromIntegral) b

readIndexedInfo :: (Int -> ConstantInfo) -> [Word8] -> ([Word8], ConstantInfo)
readIndexedInfo f w = (w', ClassInfo i)
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
    where w'    = drop 4 w
          n     = u2ToInt $ drop 2 w
          c     = u2ToInt w

readConstantInfos :: Int -> [Word8] -> [ConstantInfo]
readConstantInfos s b
    | s > 1     = c : (readConstantInfos (s - 1) b')
    | otherwise = []
    where (b', c) = readConstantInfo b