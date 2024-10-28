{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parse where

import Control.Applicative (optional, (<|>))
import Control.Exception ()
import Data.Functor (($>))
import Data.Void (Void)
import Raw (Def (..), Lit (..), Prog (Prog), Pttrn (..), Tm (..), Ty (..))
import Text.Megaparsec (MonadParsec (notFollowedBy, parseError, try), ParseErrorBundle (..), Parsec, anySingleBut, between, choice, many, sepBy, some)
import qualified Text.Megaparsec as L
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, lowerChar, newline, space1, string, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
import Text.Megaparsec.Error (ParseError (TrivialError))
import Tm (Prim (..))
import Utils (FI (FI), tr)

preserved :: [String]
preserved =
  [ "let",
    "function",
    "enum",
    "if",
    "else",
    "return",
    "as",
    "type",
    "true",
    "false"
  ]

type Parser = Parsec Void String

-- -------------
-- Basic Parsers
-- -------------
ws :: Parser ()
ws = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser String -> Parser String
lexeme = L.lexeme ws

symbol :: String -> Parser String
symbol = L.symbol ws

camelCase :: Parser String
camelCase = do
  c <- lowerChar
  cs <- many alphaNumChar
  let s = c : cs
  if s `elem` preserved
    then fail $ "Keyword " ++ s ++ " is preserved"
    else return s

pascalCase :: Parser String
pascalCase = do
  c <- upperChar
  cs <- many alphaNumChar
  let s = c : cs
  if s `elem` preserved
    then fail $ "Keyword " ++ s ++ " is preserved"
    else return s

paren :: Parser a -> Parser a
paren = between (symbol "(") (symbol ")")

bracket :: Parser a -> Parser a
bracket = between (symbol "[") (symbol "]")

brace :: Parser a -> Parser a
brace = between (symbol "{") (symbol "}")

withFI :: Parser a -> Parser (FI a)
withFI p = do
  start <- L.getOffset
  tm <- p
  end <- L.getOffset
  return $ FI (start, end) tm

withoutFI :: Parser (FI a) -> Parser a
withoutFI p = do
  FI _ tm <- p
  return tm

-------------
-- Definition Parsing
-------------

parseProg :: Parser Prog
parseProg = Prog <$> many parseDef

parseDef :: Parser (FI Def)
parseDef = choice (withFI <$> [pValDef, pTyLet, pFuncDef, pEnumDef])

pValDef :: Parser Def
pValDef =
  ValDef
    <$> (symbol "let" *> lexeme camelCase)
    <*> (symbol "=" *> parseTm 0)

pTyLet :: Parser Def
pTyLet =
  TyLet
    <$> (symbol "type" *> lexeme pascalCase)
    <*> (symbol "=" *> parseTy 0)

pFuncDef :: Parser Def
pFuncDef =
  FuncDef
    <$> (symbol "function" *> lexeme camelCase)
    <*> paren (sepBy (parsePttrn 0) (symbol ","))
    <*> optional (symbol "=>" *> parseTy 0)
    <*> parseTm 0

pEnumDef :: Parser Def
pEnumDef =
  EnumDef
    <$> (symbol "enum" *> lexeme pascalCase)
    <*> ((symbol "<" *> sepBy (lexeme pascalCase) (symbol ",") <* symbol ">") <|> pure [])
    <*> (symbol "=" *> sepBy parseFld (symbol "|"))
  where
    parseFld =
      (,)
        <$> lexeme camelCase
        <*> (paren (sepBy (parseTy 0) (symbol ",")) <|> pure [])

-- -------------
-- Term Parsing
-- -------------
type FITm = FI Tm

d :: (Show a) => Parser a -> Parser a
d = dbg "Parsing::\n"

parseTm :: Int -> Parser FITm
parseTm p = choice l
  where
    l = drop p $ try . withFI <$> [pLet, pApp, pLam, pCond, pAnn, pProj, pTuple, pRcd, pSeq, pMacro, pLit, pVar, pParen]

pParen :: Parser Tm
pParen =
  (symbol "(" <* notFollowedBy (symbol ")"))
    *> (withoutFI . parseTm) 0
    <* symbol ")"
    <* notFollowedBy (symbol "=>")

pVar :: Parser Tm
pVar = Var <$> lexeme camelCase

pLit :: Parser Tm
pLit = Lit <$> (choice . fmap try) [pNum, pBool, pStr, pUnit]
  where
    pNum = LitNum . read <$> lexeme (some digitChar)
    pBool =
      LitBool
        <$> (symbol "true" $> True <|> symbol "false" $> False)
        <* notFollowedBy alphaNumChar
    pStr = LitStr <$> lexeme (char '"' *> many (anySingleBut '"') <* char '"')
    pUnit = LitUnit <$ symbol "()"

pLam :: Parser Tm
pLam =
  Lam
    <$> paren (sepBy (parsePttrn 0) (symbol ","))
    <*> (symbol "=>" *> optional (parseTy 0))
    <*> parseTm 1

pSeq :: Parser Tm
pSeq = Seq <$> brace (sepBy (parseTm 0) (symbol ";"))

pApp :: Parser Tm
pApp = App <$> parseTm 8 <*> paren (sepBy (parseTm 1) (symbol ","))

pLet :: Parser Tm
pLet =
  Let
    <$> (symbol "let" *> parsePttrn 0)
    <*> (symbol "=" *> parseTm 1)
    <*> (ws *> parseTm 0)

pCond :: Parser Tm
pCond =
  Cond
    <$> (symbol "if" *> paren (parseTm 0))
    <*> parseTm 0
    <*> (optional newline *> symbol "else" *> parseTm 0)

pTuple :: Parser Tm
pTuple = Tuple <$> bracket (sepBy (parseTm 1) (symbol ","))

pProj :: Parser Tm
pProj = Proj <$> parseTm 6 <*> (symbol "." *> lexeme camelCase)

pAnn :: Parser Tm
pAnn = Ann <$> parseTm 5 <*> (symbol "as" *> parseTy 0)

pRcd :: Parser Tm
pRcd = Rcd <$> brace (sepBy parseFld (symbol ","))
  where
    parseFld = (,) <$> lexeme camelCase <*> (symbol "=" *> parseTm 1)

pMacro :: Parser Tm
pMacro =
  Macro
    <$> lexeme camelCase
    <*> (symbol "!(" *> parseTm 0 <* symbol ")")

-- -------------
-- Pattern Parsing
-- -------------
type FIPttrn = FI Pttrn

parsePttrn :: Int -> Parser FIPttrn
parsePttrn p = choice $ drop p $ try . withFI <$> [pPttrnAnn, pPttrnTuple, pPttrnAtom]

pPttrnAtom :: Parser Pttrn
pPttrnAtom = PttrnAtom <$> lexeme camelCase

pPttrnTuple :: Parser Pttrn
pPttrnTuple = PttrnTuple <$> bracket (sepBy (parsePttrn 0) (symbol ","))

pPttrnAnn :: Parser Pttrn
pPttrnAnn = PttrnAnn <$> parsePttrn 1 <*> (symbol ":" *> parseTy 0)

-- -------------
-- Type Parsing
-- -------------
type FITy = FI Ty

parseTy :: Int -> Parser FITy
parseTy p = choice $ drop p $ try . withFI <$> [pTyArrow, pTyApp, pTyTuple, pTyRcd, pTyPrim, pTyVar]

pTyVar :: Parser Ty
pTyVar = TyVar <$> lexeme pascalCase

pTyPrim :: Parser Ty
pTyPrim =
  TyPrim
    <$> choice
      [ PrimNum <$ symbol "Number",
        PrimBool <$ symbol "Bool",
        PrimStr <$ symbol "String",
        PrimUnit <$ symbol "Unit"
      ]

pTyArrow :: Parser Ty
pTyArrow =
  TyArrow
    <$> paren (sepBy (parseTy 0) (symbol ","))
    <*> (symbol "->" *> parseTy 0)

pTyTuple :: Parser Ty
pTyTuple = TyTuple <$> bracket (sepBy (parseTy 0) (symbol ","))

pTyRcd :: Parser Ty
pTyRcd = TyRcd <$> brace (sepBy parseFld (symbol ","))
  where
    parseFld = (,) <$> lexeme camelCase <*> (symbol ":" *> parseTy 0)

pTyApp :: Parser Ty
pTyApp =
  TyApp
    <$> parseTy 2
    <*> between (symbol "<") (symbol ">") (sepBy (parseTy 0) (symbol ","))
