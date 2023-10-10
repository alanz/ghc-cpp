module GHC.Cpp.Types where

import Data.Map (Map)
import qualified Data.Map as Map

-- ---------------------------------------------------------------------

data CppDirective
    = CppInclude String
    | CppDefine String [String]
    | CppIfdef String
    | CppIfndef String
    | CppElse
    | CppEndif
    deriving (Show, Eq)

-- ---------------------------------------------------------------------

type MacroArgs = [String]
data MacroName = MacroName String (Maybe MacroArgs)
    deriving (Show)
type MacroDef = [String]

data MacroState = MacroState {pp_defines :: !(Map MacroName MacroDef)}
    deriving (Show)

initMacroState :: MacroState
initMacroState = MacroState{pp_defines = Map.empty}

type Input = String
type Output = CppDirective
