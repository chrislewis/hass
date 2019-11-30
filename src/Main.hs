module Main where

import Data.ByteString.Lazy as BS hiding (drop, putStrLn)
import Data.Word (Word8)
import Jvm.ClassFile
import Jvm.ClassFile.ConstantPool
import System.Environment (getArgs)
import UI

main :: IO ()
main =
    do
        (file : _)  <- getArgs
        putStrLn $ "Using classfile " ++ (show file)
        bs          <- readClassBytes file
        cpsize      <- pure $ u2ToInt $ drop 8 bs
        pool        <- pure $ readConstantPool cpsize $ drop 10 bs
        mapM_ putStrLn $ render pool        

-- Read class bytes from a file on the filesystem.
readClassBytes :: String -> IO [Word8]
readClassBytes fp = do
    contents <- BS.readFile fp
    return $ unpack contents