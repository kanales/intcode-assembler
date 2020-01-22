module Main where

import           Lib
import           System.Environment
import           System.IO                      ( hPrint
                                                , stderr
                                                )
import           Data.List
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec () String

data Line = Tag String | Instruction (Opcode Param) deriving Show

parseLine :: Parser Line
parseLine = (Tag <$> parseTag) <|> (Instruction <$> parseOpcode)

parseTag :: Parser String
parseTag = (:) <$> letterChar <*> some alphaNumChar <* char ':'

parseInteger :: Parser Integer
parseInteger = do
    sign   <- optional (char '-')
    number <- some digitChar
    return . read $ case sign of
        Just x  -> x : number
        Nothing -> number

inBrackets :: Parser a -> Parser a
inBrackets x = char '[' *> x <* char ']'

parseParam :: Parser Param
parseParam = space *> p <* space
  where
    p =
        (Rel <$> try (char 'R' >> parseInteger))
            <|> (Pos <$> inBrackets parseInteger)
            <|> (Imm <$> parseInteger)

parseOpcode :: Parser (Opcode Param)
parseOpcode =
    let p = parseParam
    in  (string "ADD" >> Add <$> p <*> p <*> p)
            <|> (string "MUL" >> Mul <$> p <*> p <*> p)
            <|> (string "INP" >> Inp <$> p)
            <|> (string "OUT" >> Out <$> p)
            <|> (string "JNZ" >> Jnz <$> p <*> p)
            <|> (string "JZ" >> Jz <$> p <*> p)
            <|> (string "LT" >> Lt <$> p <*> p <*> p)
            <|> (string "EQ" >> Equ <$> p <*> p <*> p)
            <|> (string "RBS" >> Rbs <$> p)
            <|> (string "HLT" >> return Hlt)

main :: IO ()
main = do
    (x : _) <- getArgs
    xs      <- readFile x
    let res = parse parseOpcode "" `traverse` (filter (/= []) . lines) xs
    case res of
        Right ops -> putStr . intercalate "," . fmap show $ ops >>= repr
        Left  err -> hPrint stderr err
