module KidAST where

-----------------------------------
-- Specification of an AST for Goat 
-----------------------------------

type Ident = String
 
data BaseType 
  = BoolType | IntType | FloatType
    deriving (Show, Eq)

data Lvalue 
  = LId Ident
    deriving (Show, Eq)

data Expr
  = BoolConst Bool
  | IntConst Int
  | StrConst String
  | Id Ident
  | Negation Expr
  | UnaryMinus Expr
  | Eq Expr Expr
  | NotEq Expr Expr
  | LessThan Expr Expr
  | LessEqThan Expr Expr
  | GreaterThan Expr Expr
  | GreaterEqThan Expr Expr  
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Conj Expr Expr
  | Disj Expr Expr
    deriving (Show, Eq)

data Decl 
  = Decl0 Ident BaseType
  | Decl1 Ident BaseType Expr
  | Decl2 Ident BaseType Expr Expr 
    deriving (Show, Eq)

data Stmt 
  = Assign Lvalue Expr
  | Read Lvalue
  | Write Expr
  | Call Ident [Expr]
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
    deriving (Show, Eq)

data KidProgram
  = Program [Decl] [Stmt]
    deriving (Show, Eq)

