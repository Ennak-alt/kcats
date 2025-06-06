module KCATS.Parser where

import KCATS.AST
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try, MonadParsec (takeWhile1P, takeWhileP)
  )
import Text.Megaparsec.Char (space, newline, hspace)
import Data.Int (Int64)
import Control.Applicative (Const(Const))

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* hspace

lInteger :: Parser Int64
lInteger =
  choice [
    lString "-" *> (negate <$> lNumber),
    lNumber
  ]

lNumber :: Parser Int64 
lNumber = lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

rest :: Parser ()
rest = 
    choice [
        lString ";" *> takeWhileP Nothing (/= '\n') *> newline *> space, 
        newline *> space
    ]

p0arg :: String -> INST -> Parser INST
p0arg instStr inst = inst <$ lString instStr <* rest

p1arg :: String -> (Int64 -> INST) -> Parser INST
p1arg instStr inst = inst <$> (lString instStr *> lInteger) <* rest

p2arg :: String -> (Int64 -> Int64 -> INST) -> Parser INST
p2arg instStr inst = inst <$> (lString instStr *> lInteger) <*> lInteger <* rest

p3arg :: String -> (Int64 -> Int64 -> Int64 -> INST) -> Parser INST
p3arg instStr inst = inst <$> (lString instStr *> lInteger) <*> lInteger <*> lInteger <* rest

pInst :: Parser [INST]
pInst =
  choice [
    lString ";" *> takeWhileP Nothing (/= '\n') *> newline *> space *> pInst,

    ((:) <$> p1arg "ADDI" ADDI) <*> pInst,
    ((:) <$> p1arg "XORI" XORI) <*> pInst,
    ((:) <$> p1arg "RLI" RLI) <*> pInst,
    ((:) <$> p1arg "RRI" RRI) <*> pInst,
    ((:) <$> p1arg "BRA" BRA) <*> pInst,
    ((:) <$> p1arg "RBRA" RBRA) <*> pInst,
    ((:) <$> p1arg "BGTZ" BGTZ) <*> pInst,
    ((:) <$> p1arg "BLEZ" BLEZ) <*> pInst,
    ((:) <$> p1arg "BEQ" BEQ) <*> pInst,
    ((:) <$> p1arg "BNE" BNE) <*> pInst,
    ((:) <$> p1arg "EXCHI" EXCHI) <*> pInst,

    ((:) <$> p3arg "BGTZI" BGTZI) <*> pInst,
    ((:) <$> p3arg "BLEZI" BLEZI) <*> pInst,
    ((:) <$> p3arg "BEQI" BEQI) <*> pInst,
    ((:) <$> p3arg "BNEI" BNEI) <*> pInst,

    ((:) <$> p0arg "START" START) <*> pInst,
    ((:) <$> p0arg "HALT" HALT) <*> pInst,
    ((:) <$> p0arg "ADD" ADD) <*> pInst,
    ((:) <$> p0arg "SUB" SUB) <*> pInst,
    ((:) <$> p0arg "NEG" NEG) <*> pInst,
    ((:) <$> p0arg "SWAPBR" SWAPBR) <*> pInst,
    ((:) <$> p0arg "SWAP" SWAP) <*> pInst,
    ((:) <$> p0arg "EXCH" EXCH) <*> pInst,
    ((:) <$> p0arg "TS" TS) <*> pInst,
    ((:) <$> p0arg "FS" FS) <*> pInst,
    ((:) <$> p0arg "TR" TR) <*> pInst,
    ((:) <$> p0arg "FR" FR) <*> pInst,

    pure []
  ]


parseKCATS :: FilePath -> String -> Either String [INST]
parseKCATS fname s = case parse (space *> pInst <* eof) fname (s ++ "\n") of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
