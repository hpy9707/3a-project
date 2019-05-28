{-

Team name: 3A team
Austin Lancaster 539708
Angelica Adorno 1059183
Peiyu Huang 1038665

-}

module GoatCompiler where

import GoatAST

import qualified Data.Map as Map
import Control.Monad
import Data.List (intersperse)

-- state used for compilation of the program
data State = State
    -- string to store generated code
    { code :: String,
    -- dictionary to store procedures {id: [argtypes]}
      procedures :: Map.Map String [(ParMode, BaseType)],
    -- dictionary to store variables {id: (slotnumber, type)}
      variableStack :: [Map.Map String (Int, ParMode, GoatType)],
    -- register counter
      regCount :: Int,
    -- label counter
      labelCount :: Int,
    -- slot counter
      slotCount :: Int
    }

-- data, instances and functions relating to the state monad

data Update a = Update (State -> (a, State))

instance Functor Update where
    fmap = Control.Monad.liftM

instance Applicative Update where
    pure = return
    (<*>) = Control.Monad.ap

instance Monad Update where
    return code = Update (\state -> (code, state))
    Update gen >>= f
        = Update (\st0 -> let
                                (code, st1) = gen st0
                                Update gen' = f code
                            in gen' st1
                    )

getState :: Update State
getState = Update (\st -> (st, st))

runState :: Update a -> State -> a
runState (Update gen) state = code where
    (code, _) = gen state

-- on Goat function call call this to update the variable environment
-- with an empty variable map
pushVarStack :: Update ()
pushVarStack = Update (\st ->
    let oldStack = (variableStack st) in
        ((), st { variableStack = (Map.empty : oldStack) }))

-- on Goat function completion call this to return to the calling function's
-- variable environment
popVarStack :: Update ()
popVarStack = Update (\st ->
    let (x:vars) = (variableStack st) in
        ((), st { variableStack = vars }))

-- call this function to add a variable to the current environment
putVariable :: String -> Int -> ParMode -> GoatType -> Update ()
putVariable id i mode t = Update (\st ->
    let (x:vars) = variableStack st
        newMap = if Map.member id x
            then error $ "multiple declarations for " ++ id
            else Map.insert id (i, mode, t) x in
        ((), st { variableStack = (newMap:vars) }))

-- call this function to get a variable's slot number and type
getVariable :: String -> Update (Int, ParMode, GoatType)
getVariable id = do
    st <- getState
    map <- return (head (variableStack st))
    val <- return (Map.lookup id map)
    case val of
        (Just (i, p, t)) -> return (i, p, t)
        Nothing -> error $ "variable " ++ id ++ " is referenced without declaration"

-- call this function to append an instruction to the generated code
putCode :: [String] -> Update ()
putCode (x:strings) = Update (\st ->
    let c = (code st) in
    ((), st { code = c ++ "    " ++ x ++ " " ++ (concat (intersperse ", " strings)) ++ "\n"}))

-- increment label by 1
incrementLabel :: Update ()
incrementLabel = Update (\st ->
    let l = (labelCount st) in
    ((), st { labelCount = l + 1 }))

-- create a label, supplying the name
putLabelWithName :: String -> Update ()
putLabelWithName name = Update (\st ->
    let c = (code st) in
    ((), st { code = c ++ name ++ "\n"}))

-- create a label, with the name automatically generated using the next
-- label number
getLabelNext :: Update (String)
getLabelNext = do
    st <- getState
    incrementLabel
    return ("label_" ++ (show (labelCount st)))

putLabel :: String -> Update ()
putLabel label = 
    Update (\st ->
    let c = (code st) in
    ((), st { code = c ++ label ++ ":\n"}))

-- increment slotCount by 1
incrementSlot :: Update ()
incrementSlot = Update (\st ->
    let l = (slotCount st) in
    ((), st { slotCount = l + 1 }))

-- get the next slot number and increment
getSlotNext :: Update Int
getSlotNext = do
    st <- getState
    s <- return $ slotCount st
    incrementSlot
    return s

getSlotCurrent :: Update Int
getSlotCurrent = do
    st <- getState
    s <- return $ slotCount st
    return s

-- reset slot number to 0
resetSlot :: Update ()
resetSlot = Update (\st ->
    ((), st { slotCount = 0 }))

-- call this function to add a procedure to the current environment
putProcedure :: String -> [(ParMode, BaseType)] -> Update ()
putProcedure id args = Update (\st ->
        let newMap = if Map.member id (procedures st)
            then error $ "more than one procedure with name " ++ id
            else Map.insert id args (procedures st) in
        ((), st { procedures = newMap }))

-- find a procedure by id and return its type signature
getProcedure :: String -> Update([(ParMode, BaseType)])
getProcedure id = do
    st <- getState
    map <- return (procedures st)
    val <- return(Map.lookup id map)
    case val of
        (Just types) -> return types
        Nothing -> error $ "function with name " ++ id ++ " does not exist"

-- print string representation of procedure map (for debugging)
printProcMap :: Update ()
printProcMap = do
    st <- getState
    fmap <- return (procedures st)
    putCode $ return (show fmap)

-- print string representation of current stack of variable map (for debugging)
printVarMap :: Update ()
printVarMap = do
    st <- getState
    vstack <- return (variableStack st)
    putCode $ return (show (head vstack))

-- reset regCount back to 0
resetRegister :: Update ()
resetRegister = Update (\st ->
    let r = (regCount st) in
    ((), st { regCount = 0 }))

-- increase regCount by 1
incrementRegister :: String -> Update ()
incrementRegister (r:num) = Update (\st ->
    let i = read num::Int in
    ((), st { regCount = i + 1 }))

-- call this function to receive the next register
allocateRegister :: Update (String)
allocateRegister = do
    st <- getState
    r <- return $ regCount st
    return ("r" ++ (show r))

-- compile the program and return a string of the Oz code compiled from a Goat program.
compileProgram :: Program -> String
compileProgram (Program procs) =
    let
        state = State
            { code = "",
            procedures = Map.empty,
            variableStack = [Map.empty],
            labelCount = 0,
            regCount = 0,
            slotCount = 0
            }
        gen =  do
            putCode ["call", "proc_main"]
            putCode ["halt"]
            putProcedureList procs
            compileProcedureList procs
            st <- getState
            return (code st)
        result = runState gen state
    in
        result

-- store all procedure names and check for duplicates
putProcedureList :: [Procedure] -> Update ()
putProcedureList ((Procedure pos id args _ _):procs) = do
    putProcedure id (getArgTypeList args)
    putProcedureList procs
putProcedureList [] = do return ()

-- takes a list of FormalArgSpec and returns list of their base types
getArgTypeList :: [FormalArgSpec] -> [(ParMode, BaseType)]
getArgTypeList ((FormalArgSpec _ pm t _):args) = ((pm, t):(getArgTypeList args))
getArgTypeList [] = []

-- compile a list of procedures
compileProcedureList :: [Procedure] -> Update ()
compileProcedureList (x:procs) = do
    compileProcedure x
    compileProcedureList procs
compileProcedureList [] = do return ()

-- compile a procedure
compileProcedure :: Procedure -> Update ()
compileProcedure (Procedure pos id args decls stmts) = do
    putLabelWithName ("proc_" ++ id ++ ":")
    stackSize <- return (show ((length args) + (countDeclListStackSize decls)))
    putCode ["push_stack_frame", stackSize]
    resetSlot
    pushVarStack
    resetRegister
    compileArgList args
    compileDeclList decls
    compileStmtList stmts
    putCode ["pop_stack_frame", stackSize]
    putCode ["return"]
    popVarStack

countDeclListStackSize :: [Decl] -> Int
countDeclListStackSize (x:decls) = (countDeclStackSize x)
 + (countDeclListStackSize decls)
countDeclListStackSize [] = 0

countDeclStackSize :: Decl -> Int
countDeclStackSize (Decl _ _ goatType) = countBaseStackSize goatType

countBaseStackSize :: GoatType -> Int
countBaseStackSize (Base _) = 1
countBaseStackSize (Array _ n) = n
countBaseStackSize (Matrix _ m n) = m * n

--compile a list of procedure arguments
compileArgList :: [FormalArgSpec] -> Update ()
compileArgList (x:args) = do
    reg <- allocateRegister
    compileArg x
    incrementRegister reg
    compileArgList args
compileArgList [] = do return ()

--compile procedure argument
compileArg :: FormalArgSpec -> Update ()
compileArg (FormalArgSpec pos mode t id) = do
    reg <- allocateRegister
    slotNum <- getSlotNext
    putVariable id slotNum mode (Base t)
    putCode ["store", (show slotNum), reg]

--compile a list of variable declarations
compileDeclList :: [Decl] -> Update ()
compileDeclList (x:decls) = do
    compileDecl x
    compileDeclList decls
compileDeclList [] = do return ()

-- compile a variable declaration
compileDecl :: Decl -> Update ()
compileDecl (Decl pos ident t) = do
    resetRegister
    reg <- allocateRegister
    slot <- getSlotCurrent
    -- initialise numerical variables to 0
    baseType <- case t of
        Base bt -> return bt
        Array bt n -> return bt
        Matrix bt m n -> return bt
    (f, val) <- case baseType of
        FloatType -> return ("real_const", "0.0")
        _ -> return ("int_const", "0")
    case t of
        Base _ -> do initialiseVars f reg 1 val
        Array _ n -> do initialiseVars f reg n val
        Matrix _ m n -> do initialiseVars f reg (m * n) val    
    putVariable ident slot Val t

-- repeatedly generate code for initialising variables n times
initialiseVars :: String -> String -> Int -> String -> Update ()
initialiseVars _ _ 0 val = do return ()
initialiseVars func reg n val = do
    s <- getSlotNext
    putCode [func, reg, val]
    putCode ["store", (show s), reg]
    initialiseVars func reg (n - 1) val

-- compile a list of statments
compileStmtList :: [Stmt] -> Update ()
compileStmtList (x:stmts) = do
    resetRegister
    compileStmt x
    compileStmtList stmts
compileStmtList [] = do return ()

-- compile a statement
compileStmt :: Stmt -> Update ()
compileStmt (Write pos expr) = do
    (reg, baseType) <- compileExpr expr
    func <- case baseType of
        BoolType -> return "print_bool"
        IntType -> return "print_int"
        FloatType -> return "print_real"
        StringType -> return "print_string"
    -- value must be moved to r0 because this is where builtin function takes
    -- its argument from
    putCode ["move", "r0", reg]
    putCode ["call_builtin", func]
compileStmt (Read pos lvalue) = do
    -- value is stored in r0 because this is where builtin function takes
    -- its argument from
    reg0 <- allocateRegister
    baseType <- compileLvalue lvalue reg0
    func <- case baseType of
        BoolType -> return "read_bool"
        IntType -> return "read_int"
        FloatType -> return "read_real"
    putCode ["call_builtin", func]
compileStmt (ProcCall pos id exprs) = do
    sig <- (getProcedure id)
    if length exprs /= length sig then
        error $ (show pos) ++ " function " ++ id ++ " must be called with "
         ++ (show $ length $ sig) ++ " arguments, only " ++ (show $ length $ exprs) ++ " supplied"
    else do 
        resetRegister
        compileExprList exprs sig
        putCode ["call", "proc_" ++ id]
-- compile assignment statement
compileStmt (Assign pos lval expr) = do
    (reg0, expr_t) <- compileExpr expr
    id <- case lval of
        LId _ ident -> return ident
        LArrayRef _ ident _ -> return ident
        LMatrixRef _ ident _ _ -> return ident
    (slotNum, mode, goat_t) <- getVariable id
    -- make sure expression matches the Lvalue type (arrays must be accessed as arrays etc)
    decl_t <- case goat_t of
        Base t -> return t
        Array t _ -> return t
        Matrix t _ _ -> return t
    -- check that types are compatible
    if (expr_t /= decl_t && not (expr_t == IntType && decl_t == FloatType)) then
        error $ "cannot assign type " ++ (show expr_t) ++ " to type " ++ (show decl_t)
    else
        return ()
    -- convert int to float if necessary
    if (expr_t == IntType && decl_t == FloatType) then
        putCode ["int_to_real", reg0, reg0]
    else
        return ()
    -- generate code for assigning r0 to lval
    compileLvalue lval reg0
    return ()

compileLvalue :: Lvalue -> String -> Update (BaseType)
compileLvalue (LId _ id) reg0 = do
    (slotNum, mode, goat_t) <- getVariable id
    -- make sure LH expression matches the stored identifier's type (arrays must be accessed as arrays etc)
    baseType <- case goat_t of
        Base baseType -> return baseType
        _ -> error $ "single variables cannot by accessed like arrays or matrices"
    -- generate code to store reg0 into the Lvalue
    incrementRegister reg0
    case mode of
        Ref -> do
            reg1 <- allocateRegister
            putCode ["load", reg1, (show slotNum)]
            putCode ["store_indirect", reg1, reg0]
        Val -> do
                putCode ["store", (show slotNum), reg0]
    return (baseType)
compileLvalue (LArrayRef _ id expr0) reg0 = do
    (slotNum, mode, goat_t) <- getVariable id
    -- make sure LH expression matches the stored identifier's type (arrays must be accessed as arrays etc)
    baseType <- case goat_t of
        Array baseType _ -> return baseType
        _ -> error $ "array variables must be accessed with a single index"
    -- generate code to store reg0 into the Lvalue
    incrementRegister reg0
    (reg1, expr_t0) <- compileExpr expr0
    if (expr_t0 /= IntType) then
        error $ "Array index must be type Int"
    else do return ()
    incrementRegister reg1
    reg2 <- allocateRegister
    putCode["load_address", reg2, show slotNum]
    putCode["sub_offset", reg1, reg2, reg1]
    putCode["store_indirect", reg1, reg0]
    return (baseType)
compileLvalue (LMatrixRef _ id expr0 expr1) reg0 = do
    (slotNum, mode, goat_t) <- getVariable id
    -- make sure LH expression matches the stored identifier's type (arrays must be accessed as arrays etc)
    (baseType, j) <- case goat_t of
        Matrix baseType _ j -> return (baseType, j)
        _ -> error $ "array variables must be accessed with a single index"
    -- generate code to store reg0 into the Lvalue
    incrementRegister reg0
    (reg1, expr_t0) <- compileExpr expr0
    incrementRegister reg1
    (reg2, expr_t1) <- compileExpr expr1
    if (expr_t0 /= IntType || expr_t1 /= IntType) then
        error $ "Matrix index must be type Int"
    else do return ()
    incrementRegister reg2
    reg3 <- allocateRegister
    putCode["int_const", reg3, (show j)]
    putCode["mul_int", reg1, reg1, reg3]
    putCode["add_int", reg1, reg1, reg2]
    putCode["load_address", reg2, (show slotNum)]
    putCode["sub_offset", reg1, reg2, reg1]
    putCode["store_indirect", reg1, reg0]
    return (baseType)

-- compile list of expressions and check their types match a given signature
compileExprList :: [Expr] -> [(ParMode, BaseType)] -> Update ()
compileExprList (e:exprs) ((sigMode, sigT):sig) = do
    reg <- allocateRegister
    (rg, exprT) <- (compileExpr e)
    if (sigT /= exprT && not (sigT == FloatType && exprT == IntType)) then
        error $ "expression type " ++ (show exprT) ++ 
            " does not match the type required by the function signature, "
             ++ (show sigT)
    else do
        if sigMode == Ref then do
            -- check that the expression is the correct form i.e. a scalar: 
            -- either a variable, array reference or matrix
            case e of
                (Id _ id) -> do
                    (slotNum, _, _) <- getVariable id
                    putCode ["load_address", reg, show slotNum] 
                (ArrayRef _ id expr0) -> do
                    (slotNum, _, _) <- getVariable id
                    putCode ["load_address", reg, (show slotNum)]
                    incrementRegister reg
                    reg1 <- allocateRegister
                    compileExpr expr0
                    putCode ["sub_offset", reg, reg, reg1]
                (MatrixRef _ id expr0 expr1) -> do
                    (slotNum, _, t) <- getVariable id
                    case t of
                        (Matrix _ _ j) -> do
                            compileExpr expr0
                            incrementRegister reg
                            reg1 <- allocateRegister
                            compileExpr expr1
                            incrementRegister reg1 
                            reg2 <- allocateRegister
                            putCode ["int_const", reg2, (show j)]
                            putCode ["mul_int", reg, reg, reg2]
                            putCode ["add_int", reg, reg, reg1]
                            putCode ["load_address", reg1, (show slotNum)]
                            putCode ["sub_offset", reg, reg1, reg]
                _ -> error $ "arguments passed by reference must be scalar"
            return ()
        else do
            -- convert int to float if necessary
            if (sigT == FloatType && exprT == IntType) then
                putCode ["int_to_real", rg, rg]
            else 
                return ()
    incrementRegister reg
    compileExprList exprs sig
compileExprList [] [] = do return ()

-- Compile an expression and return its register number, type and value
compileExpr :: Expr -> Update (String, BaseType)
compileExpr (StrCon pos val) = do
    reg <- allocateRegister
    putCode ["string_const", reg, "\"" ++ val ++ "\""]
    return (reg, StringType)
compileExpr (IntCon pos val) = do
    reg <- allocateRegister
    putCode ["int_const", reg, (show val)]
    return (reg, IntType)
compileExpr (FloatCon pos val) = do
    reg <- allocateRegister
    putCode ["real_const", reg, (show val)]
    return (reg, FloatType)
compileExpr (BoolCon pos val) = do
    reg <- allocateRegister
    bool <- case val of
        True -> return 1
        False -> return 0
    putCode ["int_const", reg, (show bool)]
    return (reg, BoolType)
compileExpr (And pos expr expr1) = do
    reg <- allocateRegister
    (rg, ty) <- compileExpr expr
    if (ty /= BoolType)
        then error $ "Logical operator arguments must have type bool"
        else do
            label0 <- getLabelNext
            putCode["branch_on_false", reg, label0]
            incrementRegister reg
            (rg1, ty1) <- compileExpr expr1 
            if ty1 /= BoolType
                then error $ "Logical operator arguments must have type bool"
                else do 
                    putCode ["and", reg, rg, rg1]
                    putLabel label0
                    return (reg, BoolType)
compileExpr (Or pos expr expr1) = do
    reg <- allocateRegister
    (rg, ty) <- compileExpr expr
    if (ty /= BoolType)
        then error $ "Logical operator arguments must have type bool"
        else do
            label0 <- getLabelNext
            putCode["branch_on_true", reg, label0]
            incrementRegister reg
            (rg1, ty1) <- compileExpr expr1 
            if ty1 /= BoolType
                then error $ "Logical operator arguments must have type bool"
                else do 
                    putCode ["or", reg, rg, rg1]
                    putLabel label0
                    return (reg, BoolType)
compileExpr (Not pos expr) = do
    reg <- allocateRegister
    (rg, ty) <- compileExpr expr
    if ty /= BoolType
        then error $ "Logical operator arguments must have type bool"
        else do
            putCode ["not", reg, rg]
            return (reg, BoolType)
compileExpr (Rel pos relop expr expr1) = do
    reg <- allocateRegister
    (rg, ty) <- compileExpr expr
    incrementRegister reg
    (rg1, ty1) <- compileExpr expr1
    
    if (relop == Op_eq || relop == Op_ne) then
        if ty /= ty1
            then error $ "arguments for Equal and NotEqual must have the same type"
        else return ()
    else if (ty == IntType || ty == FloatType) && (ty1 /= IntType && ty1 /= FloatType)
        then error $ "Comparison between incomensurable types, " ++ (show ty) ++ "and " ++ (show ty1)
    else return ()
    f1 <- case relop of
        Op_eq -> return "cmp_eq"
        Op_ne -> return "cmp_ne"
        Op_ge -> return "cmp_ge"
        Op_le -> return "cmp_le"
        Op_gt -> return "cmp_gt"
        Op_lt -> return "cmp_lt"  
    f2 <- case (ty == FloatType || ty1 == FloatType) of
    -- if either argument are float, the result is float
        True -> return "_real"
    -- int, boolean and string are compared as int
        False -> return "_int" 
    -- convert int to float if necessary
    if (ty == FloatType && ty1 == IntType) then
        putCode ["int_to_real", rg1, rg1]
    else return ()
    if (ty1 == FloatType && ty == IntType) then
        putCode ["int_to_real", rg, rg]
    else return ()
    putCode [f1++f2, reg, rg, rg1]
    return (reg, BoolType)
compileExpr (Id pos id) = do
    reg <- allocateRegister
    (slotNum, mode, goatType) <- getVariable id
    t <- case goatType of
        (Base t) -> return t
        (Array _ _) -> error $ "Array must be referenced with an index"
        (Matrix _ _ _) -> error $ "Matrix must be referenced with two indices"
    case mode of
        Ref -> do
            -- First load the reference address then use load_indirect to load the actual value
            putCode ["load", reg, (show slotNum)]
            putCode ["load_indirect", reg, reg]
        Val -> do
            putCode ["load", reg, (show slotNum)]            
    return (reg, t)
compileExpr (ArrayRef pos id expr) = do
    (slotnumber, mode, ty) <- getVariable id
    -- TODO: handle other cases than IntCon 
    case ty of
        (Array t i) -> do
            reg <- allocateRegister
            (_, expr_t) <- compileExpr expr
            if (expr_t /= IntType) then
                error $ "array index must be type int"
            else return ()
            incrementRegister reg
            reg1 <- allocateRegister
            putCode ["load_address", reg1, (show slotnumber)]
            putCode ["sub_offset", reg, reg1, reg]
            putCode ["load_indirect", reg, reg]
            return (reg, t)
        (Base _) -> error $ "A single index is used to access variable but the variable is not an array"
        (Matrix _ _ _) -> error $ "A single index is supplied but two are needed for matrix access"
-- TODO: test this (don't I need to compile expr and expr1?)
compileExpr (MatrixRef pos id expr expr1) = do
    (slotnumber, mode, ty) <- getVariable id 
    case ty of
        (Matrix t i j) -> do
            reg <- allocateRegister
            (_, expr_t) <- compileExpr expr
            if (expr_t /= IntType) then
                error $ "array index must be type int"
            else return ()
            incrementRegister reg
            reg1 <- allocateRegister
            (_, expr_t) <- compileExpr expr1
            if (expr_t /= IntType) then
                error $ "array index must be type int"
            else return ()
            incrementRegister reg1
            reg2 <- allocateRegister
            putCode ["int_const", reg2, (show j)]
            putCode ["mul_int", reg, reg, reg2]
            putCode ["add_int", reg, reg, reg1]
            putCode ["load_address", reg1, (show slotnumber)]
            putCode ["sub_offset", reg, reg1, reg]
            putCode ["load_indirect", reg, reg]
            return (reg, t)
        (Base _) -> error $ "Two indices are used to access variable but the variable is not a matrix"
        (Array _ _) -> error $ "Two indices are used to access variable but only one is needed for an array"
compileExpr (BinOpExp pos binop expr expr1) = do
    reg <- allocateRegister
    (rg, ty) <- compileExpr expr
    incrementRegister reg
    (rg1, ty1) <- compileExpr expr1
    (f1, f_haskell) <- case binop of
        Op_add -> return ("add", (+))
        Op_sub -> return ("sub", (-))
        Op_mul -> return ("mul", (*))
        Op_div -> return ("div", (/))

    f2 <- case (ty == FloatType || ty1 == FloatType) of
    -- if either argument are float, the result is float
        True -> return "_real"
    -- int, boolean and string are compared as int
        False -> return "_int" 
    case ty of
        FloatType -> return ()
        IntType -> return ()
        _ -> error $ "binary operations must have arguments of type float or int"
    case ty1 of
        FloatType -> return ()
        IntType -> return ()
        _ -> error $ "binary operations must have arguments of type float or int"
    -- convert float to int if necessary
    if (ty == FloatType && ty1 == IntType) then
        putCode ["int_to_real", rg1, rg1]
    else return ()
    if (ty1 == FloatType && ty == IntType) then
        putCode ["int_to_real", rg, rg]
    else return ()
    putCode [f1++f2, reg, rg, rg1]
    resultType <- if ty == IntType && ty1 == IntType then
            return IntType
        else
            return FloatType
    return (reg, resultType)
compileExpr (UnaryMinus pos expr) = do
    (rg, ty) <- compileExpr expr
    f <- case ty of
        FloatType -> return "real"
        IntType -> return "int"
        _ -> error $ "unary minus must have argument of type float of int"
    putCode ["neg_" ++ f, rg, rg]
    return (rg, ty)
