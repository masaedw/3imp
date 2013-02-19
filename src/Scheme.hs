{-# LANGUAGE OverloadedStrings #-}

module Scheme where

import Data.ByteString (ByteString)

-- a: the accumulator
-- x: the next expression
-- e: the current environment
-- r: the current value rib
-- s: the current stack

data Object = SDouble Double
            | SInteger Integer
            | SString ByteString
            | SBool Bool
            deriving (Show)

type Variable = ByteString

data Expression = EObject Object
                | EVariable Variable
                | EList [Expression]
                deriving (Show)

isVariable :: Expression -> Bool
isVariable (EVariable _) = True
isVariable _ = False

data StackValue = SV -- ??
                deriving (Show)

data AssemblyCode = AHalt
                  | ARefer Variable AssemblyCode
                  | AConstant Expression AssemblyCode
                  | AClose [Variable] AssemblyCode AssemblyCode
                  | ATest AssemblyCode AssemblyCode
                  | AAssign Variable AssemblyCode
                  | AConti AssemblyCode
                  | ANuate StackValue AssemblyCode
                  | AFrame AssemblyCode AssemblyCode
                  | AArgument AssemblyCode
                  | Apply
                  | Return
                  deriving (Show)

isTail :: AssemblyCode -> Bool
isTail Return = True
isTail _ = False

compile :: Expression -> AssemblyCode -> AssemblyCode
compile o@(EObject _) next = AConstant o next
compile (EVariable v) next = ARefer v next
compile (EList []) next = error "should not be here"
compile (EList [EVariable "quote", o]) next = AConstant o next
compile (EList [EVariable "lambda", EList vs, body]) next
  | all isVariable vs = AClose (map (\(EVariable v) -> v) vs) (compile body Return) next
compile (EList [EVariable "if", test, tcase, fcase]) next = compile test $ ATest (compile tcase next) (compile fcase next)
compile (EList [EVariable "set!", EVariable v]) next = AAssign v next
compile (EList [EVariable "call/cc", x]) next =
  if isTail next
  then c
  else AFrame next c
  where
    c = AConti . AArgument $ compile x Apply
compile (EList (x:xs)) next = loop xs $ compile x Apply
  where
    loop :: [Expression] -> AssemblyCode -> AssemblyCode
    loop [] c = if isTail next
                then c
                else AFrame next c
    loop (a:as) c = loop as $ compile a $ AArgument c
