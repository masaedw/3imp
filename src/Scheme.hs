{-# LANGUAGE OverloadedStrings #-}

module Scheme where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC -- needed by pattern match
import Prelude hiding (lookup)

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
                  | ANuate [StackValue] Variable
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


data Environment = Env -- ??
                 deriving (Show)

type Rib = [Expression]

-- a: the accumulator
-- x: the next expression
-- e: the current environment
-- r: the current valur rib
-- s: the current stack

vm :: Expression -> AssemblyCode -> Environment -> Rib -> [StackValue] -> Expression
vm a x e r s =
  case x of
    AHalt -> a
    ARefer var x' -> vm a' x' e r s
      where a' = lookup var e
    AConstant obj x' -> vm obj x' e r s
    AClose vars body x' -> vm a' x' e r s
      where a' = closure body e vars
    ATest tcase fcase -> vm a x' e r s
      where x' = sif a tcase fcase
    AAssign var x' -> vm a x' e' r s
      where e' = insert var e a
    AConti x' -> vm a' x e r s
      where a' = continuation s
    ANuate s' var -> vm a' x' e r s'
      where a' = lookup var e
            x' = Return
    AFrame ret x' -> vm a x' e r' s'
      where r' = []
            s' = callFrame ret e r s
    AArgument x' -> vm a x' e (a:r) s
    Apply -> let EList (body:e:vars) = a
             in
              vm a body
    Return -> undefined

lookup :: Variable -> Environment -> Expression
lookup = undefined

insert :: Variable -> Environment -> Expression -> Environment
insert = undefined

closure :: AssemblyCode -> Environment -> [Variable] -> Expression
closure = undefined

sif :: Expression -> a -> a -> a
sif a tcase fcase = if isTrueOfScheme a
                    then tcase
                    else fcase

isTrueOfScheme :: Expression -> Bool
isTrueOfScheme = undefined

continuation :: [StackValue] -> Expression
continuation = undefined

callFrame :: AssemblyCode -> Environment -> Rib -> [StackValue] -> [StackValue]
callFrame = undefined
