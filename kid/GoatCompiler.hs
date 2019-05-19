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
      variables :: Map.Map String GoatType,
    -- register counter
      regCount :: Int,
    -- label counter
      labelCount :: Int
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
    putLabelNextHelper
    incrementLabel

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
putVariable :: String -> GoatType -> Update ()
putVariable id t = Update (\st ->
    let newMap = if Map.member id (variables st)
            then error $ "multiple declarations for " ++ id
            else Map.insert id t (variables st) in
        ((), st { variables = newMap }))

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
-- TODO: handle more than one procedure
compileProgram :: Program -> String
compileProgram (Program procs) =
    let
        state = State
            { code = "",
            procedures = Map.empty,
            variables = Map.empty,
            labelCount = 0,
            regCount = 0
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
    compileDeclList decls
    compileStmtList stmts

--compile a list of delcartions 
compileDeclList ::[Decl] -> Update()
compileDeclList (x:decls)=do
    compileDecl x
    compileDeclList decls
compileDeclList [] = Update (\st -> ((), st))

--compile one declaration
compileDecl :: Decl -> Update()
compileDecl (Decl pos id goattype)= do
    putVariable id goattype
    compilegoattype goattype

--this should be done: search the slotnumber by a specific id 
-- create relative space for array/matrix? 
 --compileGoatType:: GoatType -> Update()



-- compile a list of statments
compileStmtList :: [Stmt] -> Update ()
compileStmtList (x:stmts) = do
    compileStmt x
    compileStmtList stmts
compileStmtList [] = Update (\st -> ((), st))

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

 -- question? how to get the slotnumber of variable and the logic of array/ matrix
-- compileLvalue ::Lvalue->Update(Slotnumber,BaseType)
-- compileLvalue lvalue= do

compileStmt (Read pos lvalue)= do
    (slotnum,basetype) <- compileLvalue lvalue
    func,_ case basetype of 
        BoolType-> return "read_bool" 
        IntType -> return "read_int"
        FloatType -> return "read_real"
        StringType -> return "read_string"
    putcode ["call_builtin",func]
    putcode ["store",slotnum,"r0"]


-- Compile an expression and return its register number and type
compileExpr :: Expr -> Update (String, BaseType)
compileExpr (StrCon pos val) = do
    reg <- allocateRegister
    putCode ["string_const", reg, "\"" ++ val ++ "\""]

    return (reg, StringType)