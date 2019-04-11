module Main where

import KidAST
import KidPrettyPrinter
import Data.Char
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit

type Parser a
   = Parsec String Int a

lexer :: Q.TokenParser Int
lexer
   = Q.makeTokenParser
     (emptyDef
     { Q.commentLine     = "#"
     , Q.nestedComments  = True
     , Q.identStart      = letter
     , Q.opStart         = oneOf "+-*:"
     , Q.opLetter        = oneOf "+-*:"
     , Q.reservedNames   = myReserved
     , Q.reservedOpNames = myOpnames
     })

whiteSpace = Q.whiteSpace lexer
lexeme     = Q.lexeme lexer
natural    = Q.natural lexer
identifier = Q.identifier lexer
colon      = Q.colon lexer
semi       = Q.semi lexer
comma      = Q.comma lexer
parens     = Q.parens lexer
squares    = Q.squares lexer
reserved   = Q.reserved lexer
reservedOp = Q.reservedOp lexer

myReserved, myOpnames :: [String]

myReserved
  = ["begin", "bool", "end", "false", "int", "proc", "read", "true", "write","call"]

myOpnames 
  = ["+", "-", "*", ":=", "[", "]", ","]

-----------------------------------------------------------------
--  pProg is the topmost parsing function. It looks for a program
--  header "proc main()", followed by the program body.
-----------------------------------------------------------------

pProg :: Parser KidProgram
pProg
  = do
      reserved "proc"
      reserved "main"
      parens (return ())
      (decls,stmts) <- pProgBody
      return (Program decls stmts)
      
-----------------------------------------------------------------
--  pProgBody looks for a sequence of declarations followed by a
--  sequence of statements.
-----------------------------------------------------------------

pProgBody :: Parser ([Decl],[Stmt])
pProgBody
  = do
      decls <- many pDecl
      reserved "begin"
      stmts <- many1 pStmt
      reserved "end"
      return (decls,stmts)

pDecl, pDecl0, pDecl1, pDecl2 :: Parser Decl

pDecl
  = do
      d <- (try pDecl2 <|> try pDecl1 <|> pDecl0)
      return d

pDecl0
  = do
      basetype <- pBaseType
      ident <- identifier
      whiteSpace
      semi
      return (Decl0 ident basetype)

pDecl1
  = do
      basetype <- pBaseType
      ident <- identifier
      whiteSpace
      n <- squares pNum  
      semi
      return (Decl1 ident basetype n)

pDecl2
  = do
      basetype <- pBaseType
      ident <- identifier
      whiteSpace
      reservedOp "["
      n <- pNum
      reservedOp ","
      m <- pNum
      reservedOp "]" 
      semi
      return (Decl2 ident basetype n m)

pBaseType :: Parser BaseType
pBaseType
  = do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType }
 
-----------------------------------------------------------------
--  pStmt is the main parser for statements. It wants to recognise
--  read and write statements, and assignments.
-----------------------------------------------------------------

pStmt, pRead, pWrite, pAsg :: Parser Stmt

pStmt 
  = try pRead <|> try pWrite <|> try pAsg <|> try pCall <|> try pIf 
  <|> try pIfElse <|> try pWhile

pRead
  = do 
      reserved "read"
      lvalue <- pLvalue
      semi
      return (Read lvalue)

pWrite
  = do 
      reserved "write"
      exp <- (pString <|> pExp)
      semi
      return (Write exp)

pAsg
  = do
      lvalue <- pLvalue
      reservedOp ":="
      rvalue <- pExp
      semi
      return (Assign lvalue rvalue)

pCall
  = do
    reserved "call"
    ident <- identifier
    reservedOp "("
    expList <- sepBy1 pExp comma
    reservedOp ")" 
    semi
    return ( Call ident expList ) 

pIf
  = do
  reserved "if"
  n <- pExp
  reserved "then"
  stmts <- many1 pStmt
  reserved "fi"
  return ( If n stmts)

pIfElse
  = do
    reserved "if"
    n <- pExp
    reserved "then"
    stmts <- many1 pStmt
    reserved "else" 
    stmts1 <- many1 pStmt
    reserved "fi"
    return (IfElse n stmts stmts1)

pWhile
  = do
    reserved "while"
    n <- pExp
    reserved "do"
    stmts <- many1 pStmt
    reserved "od"
    return ( While n stmts)

    
-----------------------------------------------------------------
--  pExp is the main parser for expressions. It takes into account
--  the operator precedences and the fact that the binary operators
--  are left-associative.

-- buildExpressionParser is a helper function from module Text.Parsec.Expr
-- that allows easy parsing of expressions. table, prefix, binary,
-- and relation define the possible operators, their precedence
-- and associativity.
-----------------------------------------------------------------

pExp, pFactor, pNum, pLValExpr, pString :: Parser Expr

pExp 
  = pString <|> (buildExpressionParser table pFactor)
    <?>
    "expression"

pFactor
  = try pLValExpr <|> try (parens pExp) <|> try pNum
    <?> 
    "\"factor\""

table = [ [ prefix "-" UnaryMinus ]
        , [binary "*" Mul, binary "/" Div]
        , [binary "+" Add, binary "-" Sub]
        , [relation "=" Eq, relation "!=" NotEq, relation "<=" LessEqThan, 
          relation ">=" GreaterEqThan, relation "<" LessThan, 
          relation ">" GreaterThan]
        , [prefix "!" Negation]
        , [binary "&&" Conj]
        , [binary "||" Disj] ]

prefix name fun
  = Prefix (do { reservedOp name; return fun })
binary name op
  = Infix (do { reservedOp name; return op }) AssocLeft
relation name rel
  = Infix (do { reservedOp name; return rel }) AssocNone

pString 
  = do
      char '"'
      str <- many (satisfy (/= '"'))
      char '"'
      return (StrConst str)
    <?>
    "string"

pNum
  = do
      n <- natural <?> ""
      return (IntConst (fromInteger n :: Int))
    <?>
    "number"

pLValExpr
  = do
    lvalue <- try pLvalue2 <|> try pLvalue1 <|> pLvalue
    return (LValExpr lvalue)

-----------------------------------------------------------------
-- pLValue
-- represents one of the following:
-- <id>
-- <id> [ expr ]
-- <id> [ expr ]
-- can be used in an expression or on the LHS of a read or assignment.
-----------------------------------------------------------------

pLvalue, pLvalue0, pLvalue1, pLvalue2 :: Parser Lvalue

pLvalue
  = do
      val <- try pLvalue2 <|> try pLvalue1 <|> pLvalue0
      return val

pLvalue0
  = do
      ident <- identifier
      return (LId ident)
    <?>
    "lvalue"

pLvalue1
  = do
      ident <- identifier
      whiteSpace
      reservedOp "["
      n <- pExp
      reservedOp "]"
      return (LId1 ident n)
    <?>
    "lvalue"

pLvalue2
  = do
      ident <- identifier
      whiteSpace
      reservedOp "["
      n <- pExp
      reservedOp ","
      m <- pExp
      reservedOp "]"
      return (LId2 ident n m)
    <?>
    "lvalue"
      
-----------------------------------------------------------------
-- main
-- Parses the input file to create the appriopriate data structure,
-- throwing an error if the structure is incorrect. The data structure
-- is printed "as is" then "pretty printed" according to the spec. 
-----------------------------------------------------------------

pMain :: Parser KidProgram
pMain
  = do
      whiteSpace
      p <- pProg
      eof
      return p

main :: IO ()
main
  = do { progname <- getProgName
       ; args <- getArgs
       ; checkArgs progname args
       ; input <- readFile (head args)
       ; let output = runParser pMain 0 "" input
       ; case output of
           Right ast -> do { print ast
                           ; putStr (showProgram ast)
                           }
           Left  err -> do { putStr "Parse error at "
                           ; print err
                           }
       }

checkArgs :: String -> [String] -> IO ()
checkArgs _ [filename]
   = return ()
checkArgs progname _
   = do { putStrLn ("Usage: " ++ progname ++ " filename\n\n")
       ; exitWith (ExitFailure 1)
       }

