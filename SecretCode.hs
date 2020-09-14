module SecretCode where

import Data.Char
import Data.Maybe
import Test.HUnit

code :: [(Char,Char)]
code = zip ['a' .. 'z'] cypher ++ zip ['A' .. 'Z'] (map toUpper cypher)
  where
    cypher :: String
    cypher = "thequickbrownfxjmpsvlazydg"

encodeChar :: Char -> Char
encodeChar c =
    case lookup c code of
    Just v -> v
    Nothing -> c

testEncodeChar = runTestTT $ TestList [ encodeChar 'a' ~?= 't',
                                        encodeChar '.' ~?= '.']

encodeLine :: String -> String
encodeLine [] = []
encodeLine (x:xs) = encodeChar x : encodeLine xs

testEncodeLine = runTestTT $ TestList [encodeLine "abc defgh" ~?= "the quick"]

encodeContent :: String -> String
encodeContent str = (concat' . (map (\x -> encodeLine x)) . reverse . lines) str

concat' :: [String] -> String
concat' [] = []
concat' ([]:xss) = '\n' : concat' xss
concat' ((x:xs):xss) = x : concat' (xs:xss)

testEncodeContent = runTestTT $
  encodeContent "abc\n defgh\n" ~?= " quick\nthe\n"

encodeFile :: FilePath -> IO ()
encodeFile f = do
  str <- readFile f
  let outFile = f ++ ".code"
  writeFile outFile (encodeContent str)

main :: IO ()
main = do putStrLn "What file shall I encode?"
          fn <- getLine
          encodeFile fn
          putStrLn "All done!"
