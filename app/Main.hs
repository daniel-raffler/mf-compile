{-# LANGUAGE OverloadedStrings #-}

-- mf-compile
--
-- Licensed under Creative Commons Legal Code
-- Daniel Raffler 2022
--
-- Compiler for a lazy functional language
-- Based on the paper "Übersetzerbau – Abstrakte Maschinen"
-- by François Bry and Norbert Eisinger

module Main where

import qualified Data.Text.Lazy  as Text
import qualified Data.Map        as Map
import qualified Data.List       as List
import qualified Data.Maybe      as Maybe

import qualified Options.Applicative as OptA
import qualified System.FilePath as FilePath
import qualified Text.Megaparsec.Char.Lexer as Lex

import qualified MachineF as MachineF
import MachineF (
  Program (..),
  Info (..),
  Opcode (..),
  Addr,
  Id,
  Value(..)
  )
  
import Data.Void
import Data.Text.Lazy (Text)
import Data.Map (Map, (!))

import Text.Megaparsec
import Text.Megaparsec.Char

data LForm = LForm Id [Id] Expr
  deriving (Show)

data Expr
  = Var Id
  | Lit Value
  | App Expr Expr
--  | Let
  deriving (Show)

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = Lex.space space1 (Lex.skipLineComment "//") (Lex.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = Lex.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = Lex.signed spaceConsumer unsigned
  where unsigned = lexeme Lex.decimal

keyword :: Text -> Parser Text
keyword k = lexeme reserved
  where reserved =
          do p <- string k
             notFollowedBy alphaNumChar
             return p

{-
Programm ::= Definition ";" { Definition ";"} .
Definition ::= Variable {Variable} "=" Ausdruck .

Lokaldefinitionen ::= Lokaldefinition { ";" Lokaldefinition } .
Lokaldefinition ::= Variable "=" Ausdruck .

Ausdruck
  ::= "let" Lokaldefinitionen "in" Ausdruck
    | "if" Ausdruck "then" Ausdruck "else" Ausdruck
    | Ausdruck BinärOp Ausdruck
    | UnärOp Ausdruck
    | Ausdruck Ausdruck
    | "(" Ausdruck ")"
    | AtomarerAusdruck.

BinärOp ::= "&" | "|" | "==" | "<" | "+" | "−" | "∗" | "/" .
UnärOp ::=  "not" | "−" .

AtomarerAusdruck ::= Variable | Zahl | Wahrheitswert .
Variable ::= Name .
-}

name :: Parser Text
name = lexeme var 
  where var =
          do p1 <- letterChar
             px <- many alphaNumChar
             return $ Text.pack (p1:px)

pVar :: Parser Expr
pVar = Var <$> name

pLit :: Parser Expr
pLit = Lit <$> (number <|> boolean)
  where boolean = BVal <$> (true <|> false)
          where true  = keyword "true"  >> return True
                false = keyword "false" >> return False
        
        number = IVal <$> integer

pAtom :: Parser Expr
pAtom = pLit <|> pVar

pLDef :: Parser LForm
pLDef =
  do v0 <- name
     symbol "="
     p0 <- pExpr
     return $ LForm v0 [] p0

pExpr :: Parser Expr
pExpr = {-pLet <|> pIte <|> -} parens pExpr <|> pTimes
  where pLet =
          do keyword "let"
             px <- some (pLDef <* symbol ";")
             keyword "in"
             p0 <- pExpr
             return undefined
        
        pIte =
          do keyword "if"
             p0 <- pExpr
             keyword "then"
             p1 <- pExpr
             keyword "else"
             p2 <- pExpr
             return $ App (App (App (Var "if") p0) p1) p2
        
        mknode op a b = App (App (Var op) a) b
        
        pTimes = foldl1 (mknode "*") <$> times
          where times =
                  do p0 <- pApp
                     p1 <- optional times'
                     return $ maybe [p0] (p0:) p1
                
                times' =
                  do symbol "*" 
                     p0 <- times
                     return p0
        
        pApp = foldl1 App <$> app
          where app =
                  do p0 <- pExpr'
                     p1 <- optional app
                     return $ maybe [p0] (p0:) p1
        
        pExpr' = parens pExpr <|> pAtom

pGDef :: Parser LForm
pGDef =
  do v0 <- name
     vx <- many name
     symbol "="
     p0 <- pExpr
     return $ LForm v0 vx p0

pProg = some (pGDef <* symbol ";")

data CForm = CForm Id Int [Opcode]
  deriving (Show)

mkEnv :: [Id] -> Map Id Int
mkEnv vals = Map.fromList $ zip vals [1..]

shiftBy :: Map Id Int -> Int -> Map Id Int
shiftBy env i = Map.fromList
  [(k,v+i) | (k,v) <- Map.assocs env]

genPre :: Text -> [Text] -> [CForm]
genPre fx args = [entry, unary, binary, ite]
  where apply a ops = [Pushfun a] ++ ops ++ [Makeapp]
        
        entry = CForm ".entry" 0 $
          [Reset] ++
          foldr apply [Pushfun fx] args ++
          [Unwind,
           Call,
           Halt]
        
        unary = CForm ".unary" 1 $
          [Pushparam 2,
           Unwind,
           Call,
           Operator 1,
           Update (-1),
           Return]
        
        binary = CForm ".binary" 2 $
          [Pushparam 2,
           Unwind,
           Call,
           Pushparam 4,
           Unwind,
           Call,
           Operator 2,
           Update (-1),
           Return]
        
        ite = CForm ".ite" 3 $
          [Pushparam 1,
           Unwind,
           Call,
           Operator (-1),
           Update (-1),
           Unwind,
           Call,
           Return]

genExpr :: Expr -> Map Id Int -> [Opcode]
genExpr (Var v) env
  | Map.member v env             = [Pushparam (env!v)]
  | v `elem` MachineF.operators  = [Pushpre v]
  | otherwise                    = [Pushfun v]
genExpr (Lit c) env   = [Pushval c]
genExpr (App a b) env =
  genExpr b env ++
  genExpr a (shiftBy env 1) ++
  [Makeapp]

genDef :: LForm -> CForm
genDef (LForm fx args body) = gen (mkEnv args) (length args)
  where gen env n = CForm fx n $
          genExpr body env ++
          [Update n,
           Slide (n+1),
           Unwind,
           Call,   
           Return]

genAll :: Text -> [LForm] -> [CForm]
genAll fx lfs = genPre fx args ++ map genDef lfs
  where args = concat [map prefix [1..length k] | LForm f0 k _ <- lfs, f0 == fx]
          where prefix k = Text.pack ("." ++ show k)

linkTable :: [CForm] -> [Info]
linkTable cfs = snd $ List.mapAccumL mkEntry 0 cfs
  where mkEntry k (CForm fx args ops) = (k + length ops, Info fx args k)

linkCode :: [CForm] -> [Opcode]
linkCode cfs = concat [ops | CForm fx args ops <- cfs]

linkAll :: [CForm] -> Program
linkAll cfs = Program MachineF.version (linkTable cfs) (linkCode cfs)

compile :: Text -> [LForm] -> Program
compile fx = linkAll . genAll fx

data Options = Options Text FilePath FilePath

withArgs (Options entrypoint output input) = runP =<< readFile input
  where path = if output /= "" then output else FilePath.dropExtension input ++ ".mf"
        runP =
          MachineF.writeArchive path .
          MachineF.toArchive .
          compile entrypoint .
          Maybe.fromJust .
          parseMaybe pProg .
          Text.pack
 
commandline
  = Options
    <$> OptA.option OptA.str (
          OptA.long "with-main" <>
          OptA.short 'm' <>
          OptA.metavar "FUNCTION" <>
          OptA.help "Name of the main function" <>
          OptA.value "main"
          )
    <*> OptA.option OptA.str (
          OptA.long "output" <>
          OptA.short 'o' <>
          OptA.metavar "FILE" <>
          OptA.help "Path to the output file" <>
          OptA.value ""
          )
    <*> OptA.argument OptA.str (
          OptA.metavar "FILE" <>
          OptA.help "Path to the input file"
          )

main :: IO ()
main = withArgs =<< OptA.execParser opts
  where opts = OptA.info (OptA.helper <*> commandline) (
                 OptA.fullDesc <>
                 OptA.header "mf-compile - compiler for f programs" <>
                 OptA.progDesc "Compile the input file"
                 )
