module UI where

import Jvm.ClassFile.ConstantPool

{-
    8  |        19        |  15  |               .....
+------+------------------+------+------------------------------------------+    
   #1 = Methodref          #6.#15         // java/lang/Object."<init>":()V
-}

render :: ConstantPool -> [String]
render (ConstantPool s c) = snd $ foldr
                                    (\a (i, s) -> (i - 1, renderLine i a : s))
                                    (s - 1, [])
                                    c

renderLine :: Int -> ConstantEntry -> String
renderLine i c = entryCol ++ nameCol ++ detailsCol ++ commentCol
    where entryCol                  = getEntryNumberColumn i
          nameCol                   = padLeft name 19
          detailsCol                = padLeft details 15
          commentCol                = foldl (\b a -> "// " ++ a) "" comment
          (name, details, comment)  = prerenderEntry c

data Justification = Left | Right deriving (Eq, Show)

padColumn :: Justification -> String -> Int -> String
padColumn j s w
    | j == UI.Right = p ++ s
    | otherwise     = s ++ p
    where p = replicate (w - length s) ' '

padRight = padColumn UI.Right
padLeft = padColumn UI.Left

getEntryNumberColumn :: Int -> String
getEntryNumberColumn i = padRight line 8
    where l = '#' : (show i)
          line  = l ++ " = "

prerenderEntry :: ConstantEntry -> (String, String, Maybe String)
prerenderEntry c = case c of
    ClassEntry s i                      -> ("Class", '#' : (show i), Just s)
    Utf8Entry s _                       -> ("Utf8", s, Nothing)
    StringEntry s i                     -> ("String", '#' : (show i), Just s)
    FieldRefEntry c f t ci fi           -> ("Fieldref",
                                            thing "#" "." ci fi,
                                            Just (c ++ ('.' : f) ++ (':' : t)))
    MethodRefEntry c f t ci fi          -> ("Methodref",
                                            thing "#" "." ci fi,
                                            Just (c ++ ('.' : f) ++ (':' : t)))
    InterfaceMethodRefEntry c f t ci fi -> ("Methodref",
                                            thing "#" "." ci fi,
                                            Just (c ++ ('.' : f) ++ (':' : t)))
    NameAndTypeEntry n d ni di          -> ("NameAndType",
                                            thing "#" ":" ni di,
                                            Just (n ++ (':' : d)))

thing :: (Show a, Show b) => String -> String -> a -> b -> String
thing p d a b = p ++ show a ++ d ++ p ++ show b