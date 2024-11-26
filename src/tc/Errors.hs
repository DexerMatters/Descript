{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Errors where

import qualified Tm as T
import qualified Val as V
import           Utils (Constraint, Name)

data VTError = BadCast V.Ty V.Ty
             | NotAFunctionType V.Ty
             | NotATypeFunctionType V.Ty
             | BadMatchedBorder V.Ty [Constraint T.Ty]
             | Unimplemented String
             | BadConstraint (Constraint T.Ty)
             | NonDeducibleArgumentType V.Ty
             | DissatisfiedTypeParameterCount Int
             | AmbiguousType V.Ty
  deriving (Show)

data RTError = UnboundVar Name
             | UnboundType Name
             | MissingLabel Name
             | BadPattern T.Pttrn T.Ty
             | NonApplicableType T.Ty
             | NonProjectableType T.Ty
             | DissatisfiedParameterCount Int
  deriving (Show)

data ProgError = DuplicatedValueDefinition Name
               | DuplicatedTypeDefinition Name