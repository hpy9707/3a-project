module KidPrettyPrinter where

import KidAST
import Data.List

tab = "    "

showProgram :: KidProgram -> String
showProgram (KidProgram funcs) = intercalate "\n" (map showFunc funcs)

showFunc :: Function -> String
showFunc (Function name decls stmts)
 = "proc " ++ name ++ "()" ++ "\n" ++ (concat (map showDecl decls)) ++ "begin\n" ++ (intercalate "\n" (map (showStmt 1) stmts)) ++ "\nend\n"
 -- TODO: deal with program arguments
 -- TODO: make sure declarations are limited to bool, float, int

showDecl :: Decl -> String
showDecl (Decl0 id baseType) = tab ++ (showBaseType baseType) 
    ++ " " ++ id ++ ";\n"
showDecl (Decl1 id baseType int) = tab ++ (showBaseType baseType) ++ 
    " " ++ id ++ "[" ++ (showExpr int) ++ "]" ++ ";\n"
showDecl (Decl2 id baseType int1 int2) = tab ++  (showBaseType baseType) ++ 
    " " ++ id ++ "[" ++ (showExpr int1) ++ ", " ++ (showExpr int2) ++ "]" ++ ";\n"

showBaseType :: BaseType -> String
showBaseType BoolType = "bool"
showBaseType IntType = "int"
showBaseType FloatType = "float"
-- TODO: add floattype in KidParser.hs

-- statements pass around an Int variable representing the number of tabs
showStmt :: Int -> Stmt -> String
showStmt t d = (concat $ replicate t tab) ++ (showStmt2 t d)

showStmt2 :: Int -> Stmt -> String
showStmt2 t (Assign lvalue expr) = (showLvalue lvalue) ++ " := " ++ (showExpr expr) ++ ";"
showStmt2 t (Read lvalue) = "read " ++ (showLvalue lvalue) ++ ";"
showStmt2 t (Write expr) = "write " ++ (showExpr expr) ++ ";"
showStmt2 t (Call id args) = "call " ++ id ++ "(" ++ 
    (intercalate ", " (map showExpr args)) ++ ");"
showStmt2 t (If expr stmts) = "if " ++ (showExpr expr) ++ " then\n" ++ 
    (intercalate "\n" (map (showStmt (t + 1)) stmts)) ++ "\n" ++
    (showStmt t (Word "fi"))
showStmt2 t (IfElse expr stmts1 stmts2) = "if " ++ (showExpr expr) ++ 
    " then\n" ++ (intercalate "\n" (map (showStmt (t + 1)) stmts1)) ++ 
    "\n" ++ (showStmt t (Word "else")) ++ "\n" ++ 
    (intercalate "\n" (map (showStmt (t + 1)) stmts2)) ++ "\n" ++ 
    (showStmt t (Word "fi"))
showStmt2 t (While expr stmts) = "while " ++ (showExpr expr) ++ " do\n" ++ 
    (intercalate "\n" (map (showStmt (t + 1)) stmts)) ++ "\n" ++ 
    (showStmt t (Word "od"))
showStmt2 t (Word str) = str

showLvalue :: Lvalue -> String
showLvalue (LId id) = id
showLvalue (LId1 id expr) = id ++ "[" ++ (showExpr expr) ++ "]"
showLvalue (LId2 id expr1 expr2) = id ++ "[" ++ (showExpr expr1) ++ ", " ++ (showExpr expr2) ++ "]"

showExpr :: Expr -> String
showExpr (BoolConst b) = show b
showExpr (IntConst i) = show i
showExpr (StrConst s) = s
-- TODO: add and test FloatConst
showExpr (LValExpr lval) = showLvalue lval
showExpr (Negation expr) = "!" ++ (showExpr2 expr)
showExpr (UnaryMinus expr) = "-" ++ (showExpr2 expr)
showExpr (Add a b) = (showExpr2 a) ++ " + " ++ (showExpr2 b)
showExpr (Sub a b) = (showExpr2 a) ++ " - " ++ (showExpr2 b)
showExpr (Mul a b) = (showExpr2 a) ++ " * " ++ (showExpr2 b)
showExpr (Div a b) = (showExpr2 a) ++ " / " ++ (showExpr2 b)
showExpr (Eq a b) = (showExpr2 a) ++ " = " ++ (showExpr2 b)
showExpr (NotEq a b) = (showExpr2 a) ++ " != " ++ (showExpr2 b)
showExpr (LessThan a b) = (showExpr2 a) ++ " < " ++ (showExpr2 b)
showExpr (LessEqThan a b) = (showExpr2 a) ++ " <= " ++ (showExpr2 b)
showExpr (GreaterThan a b) = (showExpr2 a) ++ " > " ++ (showExpr2 b)
showExpr (GreaterEqThan a b) = (showExpr2 a) ++ " >= " ++ (showExpr2 b)
showExpr (Conj a b) = (showExpr2 a) ++ " && " ++ (showExpr2 b)
showExpr (Disj a b) = (showExpr2 a) ++ " || " ++ (showExpr2 b)

-- This function deals with recursively nested expressions. 
-- Brackets are printed around the inner expression if it is binary.
showExpr2 :: Expr -> String
showExpr2 (Add a b) = "(" ++ (showExpr (Add a b))  ++ ")"
showExpr2 (Sub a b) = "(" ++ (showExpr (Sub a b))  ++ ")"
showExpr2 (Mul a b) = "(" ++ (showExpr (Mul a b))  ++ ")"
showExpr2 (Div a b) = "(" ++ (showExpr (Div a b))  ++ ")"
showExpr2 (Eq a b) = "(" ++ (showExpr (Eq a b))  ++ ")"
showExpr2 (NotEq a b) = "(" ++ (showExpr (NotEq a b))  ++ ")"
showExpr2 (LessThan a b) = "(" ++ (showExpr (LessThan a b))  ++ ")"
showExpr2 (LessEqThan a b) = "(" ++ (showExpr (LessEqThan a b))  ++ ")"
showExpr2 (GreaterThan a b) = "(" ++ (showExpr (GreaterThan a b))  ++ ")"
showExpr2 (GreaterEqThan a b) = "(" ++ (showExpr (GreaterEqThan a b))  ++ ")"
showExpr2 (Conj a b) = "(" ++ (showExpr (Conj a b))  ++ ")"
showExpr2 (Disj a b) = "(" ++ (showExpr (Disj a b))  ++ ")"
showExpr2 rest = showExpr rest