module Parse where

import Control.Applicative (optional, (<|>))
import Control.Exception ()
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (notFollowedBy, try), Parsec, anySingleBut, between, choice, many, sepBy, some)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, lowerChar, newline, space1, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
import Tm (Def (FuncDef, TyLet, ValDef), Lit (..), Prim (..), Prog (Prog), Pttrn (..), Tm (..), Ty (..))

preserved :: [String]
preserved =
  [ "let",
    "function",
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

-- -------------
-- Definition Parsing
-- -------------

parseProg :: Parser Prog
parseProg = do
  defs <- many parseDef
  return $ Prog defs

parseDef :: Parser Def
parseDef = pValDef <|> pTyLet <|> pFuncDef

pValDef :: Parser Def
pValDef =
  ValDef
    <$> (symbol "let" *> camelCase)
    <*> (symbol "=" *> parseTm 0 <* newline)

pTyLet :: Parser Def
pTyLet =
  TyLet
    <$> (symbol "type" *> pascalCase)
    <*> (symbol "=" *> parseTy 0 <* newline)

pFuncDef :: Parser Def
pFuncDef =
  FuncDef
    <$> (symbol "function" *> camelCase)
    <*> paren (sepBy (parsePttrn 0) (symbol ","))
    <*> optional (symbol "=>" *> parseTy 0)
    <*> (parseTm 0 <* newline)

-- -------------
-- Term Parsing
-- -------------
d :: (Show a) => Parser a -> Parser a
d = dbg "Parsing::\n"

parseTm :: Int -> Parser Tm
parseTm p = try (paren (parseTm 0) <* notFollowedBy (symbol "=>")) <|> choice l
  where
    l = drop p $ try <$> [pLet, pLam, pApp, pCond, pAnn, pProj, pTuple, pRcd, pSeq, pLit, pVar]

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
pApp = App <$> parseTm 5 <*> paren (sepBy (parseTm 0) (symbol ","))

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
pProj = Proj <$> parseTm 6 <*> (symbol "." *> camelCase)

pAnn :: Parser Tm
pAnn = Ann <$> parseTm 5 <*> (symbol "as" *> parseTy 0)

pRcd :: Parser Tm
pRcd = Rcd <$> brace (sepBy parseFld (symbol ","))
  where
    parseFld = (,) <$> camelCase <*> (symbol ":" *> parseTm 0)

-- -------------
-- Pattern Parsing
-- -------------

parsePttrn :: Int -> Parser Pttrn
parsePttrn p = choice $ drop p $ try <$> [pPttrnAnn, pPttrnTuple, pPttrnAtom]

pPttrnAtom :: Parser Pttrn
pPttrnAtom = PttrnAtom <$> lexeme camelCase

pPttrnTuple :: Parser Pttrn
pPttrnTuple = PttrnTuple <$> bracket (sepBy (parsePttrn 0) (symbol ","))

pPttrnAnn :: Parser Pttrn
pPttrnAnn = PttrnAnn <$> parsePttrn 1 <*> (symbol ":" *> parseTy 0)

-- -------------
-- Type Parsing
-- -------------

parseTy :: Int -> Parser Ty
parseTy p = choice $ drop p $ try <$> [pTyArrow, pTyApp, pTyTuple, pTyRcd, pTyVar, pTyPrim]

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
pTyArrow = TyArrow <$> parseTy 1 <*> (symbol "->" *> parseTy 0)

pTyTuple :: Parser Ty
pTyTuple = TyTuple <$> bracket (sepBy (parseTy 0) (symbol ","))

pTyRcd :: Parser Ty
pTyRcd = TyRcd <$> brace (sepBy parseFld (symbol ","))
  where
    parseFld = (,) <$> camelCase <*> (symbol ":" *> parseTy 0)

pTyApp :: Parser Ty
pTyApp = TyApp <$> (symbol "<" *> parseTy 2) <*> (symbol ">" *> parseTy 0)
