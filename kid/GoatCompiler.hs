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
      procedures :: Map.Map String [BaseType],
    -- dictionary to store variables {id: (slotnumber, type)}
      variables :: Map.Map String (Int, GoatType),
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

-- XXX call this function to append an instruction to the generated code
putCode :: [String] -> Update ()
putCode strings = Update (\st ->
    let c = (code st) in
    ((), st { code = c ++ "    " ++ (concat (intersperse " " strings))
     ++ "\n"}))

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
    incrementLabel
    putLabelNextHelper
    

putLabelNextHelper :: Update ()
putLabelNextHelper = 
    Update (\st ->
    let c = (code st) 
        l = (labelCount st) in
    ((), st { code = c ++ "label_" ++ (show l) ++ ":\n"}))

-- call this function to add a procedure to the current environment
putProcedure :: String -> [BaseType] -> Update ()
putProcedure id args = Update (\st ->
        let newMap = if Map.member id (procedures st)
            then error $ "more than one procedure with name " ++ id
            else Map.insert id args (procedures st) in
        ((), st { procedures = newMap }))

-- XXX call this function to add a variable to the current environment
putVariable :: String -> Int -> GoatType -> Update ()
putVariable id slotnumber t = Update (\st ->
    let newMap = if Map.member id (variables st)
            then error $ "multiple declarations for " ++ id
            else Map.insert id (slotnumber, t) (variables st) in
        ((), st { variables = newMap }))

-- call this function to get a variable's slot number and type
getVariable :: String -> Update (Int, GoatType)
getVariable id = do
    st <- getState
    map <- return (variables st)
    val <- return (Map.lookup id map)
    case val of
        (Just (i, t)) -> return (i, t)
        Nothing -> error $ "variable " ++ id ++ " is referenced without declaration"

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
getLabelCurrent ::Update Int
getLabelCurrent= do 
    st<- getState
    s<- return $ labelCount st
    return s

-- compile the program and return a string of the Oz code compiled from a Goat program.
-- TODO: handle more than one procedure
--Take if as an eample:
--suppose it starts with 0 if true turn to label 0(no change) if not goto uncond label 1(create) 
--in label 1 there is another is statement if true goto label 1. It is illegal.
--as a result set lable count to -1 is a good choice.
compileProgram :: Program -> String
compileProgram (Program procs) =
    let
        state = State
            { code = "",
            procedures = Map.empty,
            variables = Map.empty,
            labelCount = -1,
            regCount = 0,
            slotCount = 0
            }
        gen =  do
            putCode ["call", "proc_main"]
            putCode ["halt"]
            compileProcedureList procs
            st <- getState
            return (code st)
        result = runState gen state
    in
        result

-- compile a list of procedures
compileProcedureList :: [Procedure] -> Update ()
compileProcedureList (x:procs) = do
    compileProcedure x
    compileProcedureList procs
compileProcedureList [] = Update (\st -> ((), st))

-- compile a procedure
-- TODO: process arguments and declarations
compileProcedure :: Procedure -> Update ()
compileProcedure (Procedure pos id args decls stmts) = do
    putLabelWithName ("proc_" ++ id ++ ":")
    putCode ["push_stack_frame" ++ (show ((length args) + (length decls)))]
    compileStmtList stmts

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
    time <- case t of 
        Base bt -> return 1
        Array bt n -> return n
        Matrix bt m n -> return (n*m)
    initialiseVars f reg time val
    putVariable ident slot t

-- repeatedly generate code for initialising variables n times
initialiseVars :: String -> String -> Int -> String -> Update ()
initialiseVars _ _ 0 val = do return ()
initialiseVars func reg n val = do
    s <- getSlotNext
    putCode [func, reg, val]
    putCode ["store", (show s), reg]
    initialiseVars func reg (n - 1) val
--compile lvalue    
compileLvalue ::Lvalue->Update()
compileLvalue (LId pos iden)= do
    (slotnum,goattype)<- getVariable  iden
    putCode["store",show slotnum,"r0"]

parseLvalue ::Lvalue->Update(BaseType)
parseLvalue (LId pos iden) = do
    (slotnum,goattype)<- getVariable iden
    basetype <- case goattype of
        Base bt -> return bt
        Array bt n -> return bt
        Matrix bt m n -> return bt
    return basetype
-- compile a list of statments
compileStmtList :: [Stmt] -> Update ()
compileStmtList (x:stmts) = do
    compileStmt x
    compileStmtList stmts
compileStmtList [] = Update (\st -> ((), st))

-- compile a statement
compileStmt :: Stmt -> Update ()
-- compile write
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



--compile read
compileStmt (Read pos lvalue)= do
    basetype<- parseLvalue lvalue
    func<- case basetype of 
        BoolType-> return "read_bool" 
        IntType -> return "read_int"
        FloatType -> return "read_real"
    putCode ["call_builtin",func]
    compileLvalue lvalue    
 
--compile assignment reamining to be done: depending on the data structure of expr
compileStmt(Assign pos lvalue expr)=do
    compileExpr expr
    compileLvalue lvalue   


--compile if   
compileStmt(If pos expr stmt) = do 
    compileExpr expr
    incrementLabel--label 0
    currentNum <- getLabelCurrent 
    putCode["branch_on true","r0,","label_"++ show currentNum]
    putCode["branch_uncond","label_"++ show (currentNum + 1)]
    putLabelNextHelper
    compileStmtList stmt
    putLabelNext --label 1 +reamining part
    
--compile ifelse
compileStmt(IfElse pos expr stmt1 stmt2)=do
    compileExpr expr
    incrementLabel
    currentNum <- getLabelCurrent
    putCode["branch_on ture","r0,","label_"++ show currentNum]
    
    putCode["branch_on false","r0","label_"++ show (currentNum+1)]
  
    putCode["branch_uncond","label_"++ show (currentNum+2)]
    putLabelNextHelper
    compileStmtList stmt1
    putLabelNext
    compileStmtList stmt2
    putLabelNext

--compile while
compileStmt(While pos expr stmt)=do
    incrementLabel
    putLabelNextHelper
    compileExpr expr
    incrementLabel
    currentNum <- getLabelCurrent
    
    putCode["branch_on true","r0,","label_"++ show currentNum]
 
    putCode["branch_uncond","label_"++ show (currentNum + 1)]
    putLabelNextHelper
    compileStmtList stmt
    putLabelNext


    -- Compile an expression and return its register number and type
compileExpr :: Expr -> Update (String, BaseType)
compileExpr (StrCon pos val) = do
    reg <- allocateRegister
    putCode ["string_const", reg, "\"" ++ val ++ "\""]

    return (reg, StringType)