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

-- XXX call this function to add a variable to the current environment
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

-- XXX call this function to append an instruction to the generated code
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

-- XXX create a label, with the name automatically generated using the next
-- label number
putLabelNext :: Update ()
putLabelNext = do
    putLabelNextHelper
    incrementLabel

putLabelNextHelper :: Update ()
putLabelNextHelper = 
    Update (\st ->
    let c = (code st) 
        l = (labelCount st) in
    ((), st { code = c ++ "label_" ++ (show l) ++ ":\n"}))

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
incrementRegister :: Update ()
incrementRegister = Update (\st ->
    let r = (regCount st) in
    ((), st { regCount = r + 1 }))

-- call this function to receive the next register
allocateRegister :: Update (String)
allocateRegister = do
    st <- getState
    r <- return $ regCount st
    incrementRegister
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
    compileArg x
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
    initialiseVars f reg 1 val
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
compileStmt (ProcCall pos id exprs) = do
    sig <- (getProcedure id)
    if length exprs /= length sig then
        error $ (show pos) ++ " function " ++ id ++ " must be called with "
         ++ (show $ length $ sig) ++ " arguments, only " ++ (show $ length $ exprs) ++ " supplied"
    else do 
        resetRegister
        compileExprList exprs sig
        putCode ["call", "proc_" ++ id]
 -- TODO: complete this (incl for arrays/matrices) (check types, int->float, see declmulffloat)
compileStmt (Assign pos (LId _ id) expr) = do
    (reg1, t) <- compileExpr expr
    (slotNum, mode, (Base t)) <- getVariable id
    case mode of
        Ref -> do
            reg2 <- allocateRegister
            putCode ["load", reg2, (show slotNum)]
            putCode ["store_indirect", reg2, reg1]
        Val -> do
            putCode ["store", (show slotNum), reg1]
    

-- compile list of expressions and check their types match a given signature
compileExprList :: [Expr] -> [(ParMode, BaseType)] -> Update ()
compileExprList (e:exprs) ((sigMode, sigT):sig) = do
    -- TODO: make sure calling compileExpr successifly results in consequtive register numbers
    (reg, exprT) <- (compileExpr e)
    if (sigT /= exprT) then
        error $ "expression type " ++ (show exprT) ++ 
            " does not match the type required by the function signature, "
             ++ (show sigT)
    else do
        if sigMode == Ref then do
            -- check that the expression is the correct form i.e. a scalar: 
            -- either a variable, array reference or matrix
            exprIsScalar <- case e of
                (Id _ id) -> return True -- TODO: change this to (Just id)
                (ArrayRef _ id _) -> return True
                (MatrixRef _ id _ _) -> return True
                _ -> return False --- change this to Nothing
            if not exprIsScalar then
                error $ "arguments passed by reference must be scalar"
            else do
                -- get slot number of expression
                -- TODO: make slotnumber getting work for arrays and matrices
                case e of 
                    (Id _ id) -> do
                        (slotNum, _, _) <- getVariable id
                        putCode ["load_address", reg, show slotNum] 
        else do
            -- TODO: this will be handled by compileExpr (Id...) etc
            case e of
                (Id _ id) -> do
                    (slotNum, _, _) <- getVariable id
                    putCode ["load", reg, show slotNum] -- don't need this
    compileExprList exprs sig
compileExprList [] [] = do return ()

-- Compile an expression and return its register number and type
compileExpr :: Expr -> Update (String, BaseType)
compileExpr (StrCon pos val) = do
    reg <- allocateRegister
    putCode ["string_const", reg, "\"" ++ val ++ "\""]
    return (reg, StringType)
compileExpr (IntCon pos val) = do
    reg <- allocateRegister
    putCode ["int_const", reg, (show val)]
    return (reg, IntType)
-- TODO: either convert val so it has a decimal, or throw error, or convert int to float
compileExpr (FloatCon pos val) = do
    reg <- allocateRegister
    putCode ["real_const", reg, (show val)]
    return (reg, FloatType)
-- TODO: deal with arrays/matrices
compileExpr (Id pos id) = do
    reg <- allocateRegister
    (slotNum, mode, (Base t)) <- getVariable id
    case mode of
        Ref -> do
            -- First load the reference address then use load_indirect to load the actual value
            putCode ["load", reg, (show slotNum)]
            putCode ["load_indirect", reg, reg]
        Val -> do
            putCode ["load", reg, (show slotNum)]            
    return (reg, t)

