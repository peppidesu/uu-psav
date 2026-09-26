{-# OPTIONS_GHC -Wno-x-partial #-}

module Z3Conf (
getPlain,
getConf
)

where

import qualified Z3.Opts as Z3
import Data.Char
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Data.List.Extra (mconcatMap)

type ConfPath = String
type Error = String

parseLine :: String -> Either Error (String, String)
parseLine [] = Left "Read empty line when no empty lines should reach this file"
parseLine line =
    let (key,rest) = break (== '=') $ dropWhile isSpace line
        b1 = \str -> not (null str) && all (\c -> 'a' <= c && c <= 'z' || c == '.' || c == '_') str
        b2 = \str -> or [
            str == "true"
           ,str == "false"
           ,isJust (readMaybe str :: Maybe Double)
           ,isJust (readMaybe str :: Maybe Int)
           ]
    in case rest of
      [] -> Left "missing ="
      ('=':rest') -> if b1 key && b2 rest'
                    then Right $ (key, rest')
                    else Left "invalid identifiers"
      _ -> undefined

parseFile :: String -> Either Error [(String, String)]
parseFile fileStr = mapM parseLine
                    $ filter (\line -> (not $ null line) && head line /= '#')
                    $ map (dropWhile isSpace)
                    $ lines fileStr


getPlain :: Maybe ConfPath -> IO [(String, String)]
getPlain Nothing = return $ []
getPlain (Just confPath) = do
          optList <- parseFile <$> readFile confPath
          case optList of
            Left errorStr -> error errorStr
            Right opts -> return $ opts

getConf :: Maybe String ->  IO Z3.Opts
getConf mStr = mconcatMap (uncurry Z3.opt) <$> getPlain mStr
          
