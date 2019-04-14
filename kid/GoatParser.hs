{-

Team name: 3A team
Austin Lancaster 539708
Angelica Adorno 1059183
Peiyu Huang 1038665

-}

module Main where

import GoatAST
import GoatPrettyPrinter
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
    funcs <- many1 pFunc
    return (KidProgram funcs)

pFunc :: Parser Function
pFunc
  = do
      reserved "proc"
      name <- identifier
      reservedOp "("
      params <- sepBy pParameter comma
      reservedOp ")"
      (decls,stmts) <- pProgBody
      return (Function name params decls stmts)
      <?>
      "program"
      
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
    <?>
    "program body"

pParameter :: Parser Param
pParameter
  = do
      indicator<-pIndiType
      basetype <- pBaseType
      ident <- identifier
      return (Param ident indicator basetype)

pIndiType :: Parser IndiType
pIndiType    
  = do {reserved "val" ; return ValueType}
  <|>
    do {reserved "ref" ; return ReferType}

pDecl, pDecl0, pDecl1, pDecl2 :: Parser Decl

pDecl
  = do
    d <- (try pDecl2 <|> try pDecl1 <|> pDecl0)
    return d
    <?>
    "declaration"

pDecl0
  = do
    basetype <- pBaseType
    ident <- identifier
    whiteSpace
    semi
    return (Decl0 ident basetype)
    <?>
    "declaration with no shape indicator"

pDecl1
  = do
    basetype <- pBaseType
    ident <- identifier
    whiteSpace
    n <- squares pNum  
    semi
    return (Decl1 ident basetype n)
    <?>
    "declaration with one shape indicator"

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
    <?>
    "declaration with two shape indicators"

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
  <?>
  "statement"

pRead
  = do 
    reserved "read"
    lvalue <- pLvalue
    semi
    return (Read lvalue)
    <?>
    "read"

pWrite
  = do 
    reserved "write"
    exp <- (pString <|> pExp)
    semi
    return (Write exp)
    <?>
    "write"

pAsg
  = do
    lvalue <- pLvalue
    reservedOp ":="
    rvalue <- pExp
    semi
    return (Assign lvalue rvalue)
    <?>
    "assign"

pCall
  = do
    reserved "call"
    ident <- identifier
    reservedOp "("
    expList <- sepBy1 pExp comma
    reservedOp ")" 
    semi
    return ( Call ident expList ) 
    <?>
    "call"

pIf
  = do
    reserved "if"
    n <- pExp
    reserved "then"
    stmts <- many1 pStmt
    reserved "fi"
    return ( If n stmts)
    <?>
    "if"

pIfElse
  = do
    reserved "if"
    n <- pExp
    reserved "then"
    stmts1 <- many1 pStmt
    reserved "else" 
    stmts2 <- many1 pStmt
    reserved "fi"
    return (IfElse n stmts1 stmts2)
    <?>
    "if else"

pWhile
  = do
    reserved "while"
    n <- pExp
    reserved "do"
    stmts <- many1 pStmt
    reserved "od"
    return ( While n stmts)
    <?>
    "while"

    
-----------------------------------------------------------------
--  pExp is the main parser for expressions. It takes into account
--  the operator precedences and the fact that the binary operators
--  are left-associative.

-- buildExpressionParser is a helper function from module Text.Parsec.Expr
-- that allows easy parsing of expressions. table, prefix, binary,
-- and relation define the possible operators, their precedence
-- and associativity.
-----------------------------------------------------------------

pExp, pFactor, pNum, pBool, pLValExpr, pString :: Parser Expr
pTrue, pFalse :: Parser Bool

pExp 
  = pString <|> (buildExpressionParser table pFactor)
    <?>
    "expression"

pFactor
  = try pLValExpr <|> try (parens pExp) <|> try pFloat <|> try pNum <|> try pBool
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

pFloat = do
    ws <- many1 digit
    char '.'
    ds <- many1 digit 
    whiteSpace
    let val = read (ws ++ ('.' : ds)) :: Float
    return (FloatConst val)
    <?>
    "float"

pBool
  = do
    bool <- pTrue <|> pFalse
    return (BoolConst bool)
    <?>
    "boolean"

pTrue
  = do
    reserved "true"
    return True
    <?>
    "true"
      
pFalse
  = do
    reserved "false"
    return False
    <?>
    "false"

pLValExpr
  = do
    lvalue <- pLvalue
    return (LValExpr lvalue)
    <?>
    "LValExpr"

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
    <?>
    "lvalue"

pLvalue0
  = do
      ident <- identifier
      return (LId ident)
    <?>
    "lvalue with no shape indicator"

pLvalue1
  = do
      ident <- identifier
      whiteSpace
      reservedOp "["
      n <- pExp
      reservedOp "]"
      return (LId1 ident n)
    <?>
    "lvalue with one shape indicator"

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
    "lvalue with two shape indicators"
      
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

