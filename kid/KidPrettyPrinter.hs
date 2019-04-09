module KidPrettyPrinter where

import KidAST
import Data.List

showProgram :: KidProgram -> String
showProgram (Program decls stmts)
 = (intercalate "\n" (map showStmt stmts)) ++ "\n"
 -- TODO: do something with decls

showStmt :: Stmt -> String
showStmt (Assign lvalue expr)
 = (showLvalue lvalue) ++ " := " ++ (showExpr expr)
-- TODO: add more definitions of stmt here...

showLvalue :: Lvalue -> String
showLvalue (LId id) = id
-- TODO: add more...

showExpr :: Expr -> String
showExpr (IntConst i) = show i
showExpr (Id id) = id
showExpr (Add a b) = (showExpr2 a) ++ " + " ++ (showExpr2 b)
showExpr (Sub a b) = (showExpr2 a) ++ " - " ++ (showExpr2 b)
showExpr (Mul a b) = (showExpr2 a) ++ " * " ++ (showExpr2 b)
showExpr (Div a b) = (showExpr2 a) ++ " / " ++ (showExpr2 b)
-- TODO: add more...

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