module Main where

import Data.ByteString.Lazy as BS hiding (drop, putStrLn)
import Data.Word (Word8)
import Jvm.ClassFile
import Jvm.ClassFile.ConstantPool
import System.Environment (getArgs)

main :: IO ()
main =
    do
        (file : _) <- getArgs
        putStrLn $ "Using classfile " ++ (show file)
        bs      <- readClassBytes file
        cpsize  <- pure $ u2ToInt $ drop 8 bs
        infos   <- pure $ readConstantInfos cpsize $ drop 10 bs
        mapM_ print infos
        

-- Read class bytes from a file on the filesystem.
readClassBytes :: String -> IO [Word8]
readClassBytes fp = do
    contents <- BS.readFile fp
    return $ unpack contents