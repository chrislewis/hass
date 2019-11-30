module Jvm.ClassFile.ConstantPool where

import Data.Map.Strict as Map
import Data.Word (Word8)
import Jvm.ClassFile

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4-140
readConstantInfo :: [Word8] -> ([Word8], ConstantInfo)
readConstantInfo b = r $ Prelude.drop 1 b
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

data ConstantEntry =
    ClassEntry              String Int
  | FieldRefEntry           String String String Int Int
  | MethodRefEntry          String String String Int Int
  | InterfaceMethodRefEntry String String String Int Int
  | StringEntry             String Int
  | Utf8Entry               String Int
  | NameAndTypeEntry        String String Int Int deriving Show

readUtf8Info :: [Word8] -> ([Word8], ConstantInfo)
readUtf8Info w = (w', Utf8Info l s)
    where w'    = Prelude.drop l x
          l     = u2ToInt w
          s     = Prelude.map (toEnum . fromIntegral) b
          b     = Prelude.take l x
          x     = Prelude.drop 2 w

readIndexedInfo :: (Int -> ConstantInfo) -> [Word8] -> ([Word8], ConstantInfo)
readIndexedInfo f w = (w', f i)
    where w'    = Prelude.drop 2 w
          i     = u2ToInt w

readClassInfo   = readIndexedInfo ClassInfo
readStringInfo  = readIndexedInfo StringInfo

readFieldRefInfo            = readRefInfo FieldRefInfo
readMethodRefInfo           = readRefInfo MethodRefInfo
readInterfaceMethodRefInfo  = readRefInfo InterfaceMethodRefInfo
readNameAndTypeInfo         = readRefInfo NameAndTypeInfo

data ConstantPool = ConstantPool Int [ConstantEntry]

readRefInfo :: (Int -> Int -> ConstantInfo) -> [Word8] -> ([Word8], ConstantInfo)
readRefInfo f w = (w', f c n)
    where w'    = Prelude.drop 2 x
          c     = u2ToInt w
          n     = u2ToInt x
          x     = Prelude.drop 2 w

resolveUtf8 :: Maybe ConstantInfo -> String
resolveUtf8 c = case c of
    Just (Utf8Info i str)    -> str

resolveNameAndType :: Maybe ConstantInfo -> Map Int ConstantInfo -> (String, String)
resolveNameAndType i c = case i of
    Just (NameAndTypeInfo n d)    -> (resolveUtf8 (Map.lookup n c),
                                      resolveUtf8 (Map.lookup d c))

resolveClass :: Maybe ConstantInfo -> Map Int ConstantInfo -> String
resolveClass i c = case i of
    Just (ClassInfo i)    -> resolveUtf8 (Map.lookup i c)

readConstantInfos2 :: Int -> [Word8] -> [ConstantEntry]
readConstantInfos2 s b = Map.elems $ Map.mapWithKey resolve c
    where c             = readConstantInfoMap s b
          resolve idx info  = case info of
              ClassInfo i                   -> ClassEntry (resolveUtf8 (get i)) i
              Utf8Info i str                -> Utf8Entry str i
              StringInfo i                  -> StringEntry (resolveUtf8 (get i)) i
              FieldRefInfo d n              -> FieldRefEntry klass name desc d n
                                               where klass          = resolveClass (get d) c
                                                     (name, desc)   = get2 (get n)
              MethodRefInfo d n             -> MethodRefEntry klass name desc d n
                                               where klass          = resolveClass (get d) c
                                                     (name, desc)   = get2 (get n)
              InterfaceMethodRefInfo d n    -> InterfaceMethodRefEntry klass name desc d n
                                               where klass          = resolveClass (get d) c
                                                     (name, desc)   = get2 (get n)
              NameAndTypeInfo n d           -> NameAndTypeEntry (resolveUtf8 (get n))
                                                                    (resolveUtf8 (get d)) n d
              where get2    = flip resolveNameAndType c
                    get             = flip Map.lookup c

readConstantPool :: Int -> [Word8] -> ConstantPool
readConstantPool s b = ConstantPool s (readConstantInfos2 s b)

readConstantInfoMap :: Int -> [Word8] -> Map Int ConstantInfo
readConstantInfoMap s b = go 1 b Map.empty
    where go i b m
            | i < s     = go (i + 1) b' (Map.insert i c m)
            | otherwise = m
            where (b', c) = readConstantInfo b