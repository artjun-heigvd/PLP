module Codegen where

import Pipi
import Instruction

codegen :: Program -> [Instruction]
codegen = codegenProg

codegenProg :: Program -> [Instruction]
codegenProg = concatMap codegenStmt

codegenStmt :: Stmt -> [Instruction]
codegenStmt (Print expr) = codegenExpr expr ++ [Display]
codegenStmt (Assign name expr) =  codegenExpr expr ++ [Store name]
codegenStmt (If cond thenn elze) = 
    let
        codeCond = codegenExpr cond
        codeThenn = concatMap codegenStmt thenn
        codeElze = concatMap codegenStmt elze
    in
        codeCond ++ [JMPC (length codeThenn + 1)] ++ codeThenn ++ [JMPI (length codeElze)] ++ codeElze
codegenStmt (While cond exec) =
    let
        codeCond = codegenExpr cond
        codeExec = concatMap codegenStmt exec
        lenExec = length codeExec
    in
        codeCond ++ [JMPC (lenExec + 1)] ++ codeExec ++ [JMPI (-(lenExec + length codeCond + 1))]


codegenExpr :: Expr -> [Instruction]
codegenExpr (Ident name) = [Load name]
codegenExpr (Number value) = [Push value]
codegenExpr (Add left right) = codegenExpr left ++ codegenExpr right ++ [Plus]
codegenExpr (Sub left right) = codegenExpr left ++ codegenExpr right ++ [Minus]
