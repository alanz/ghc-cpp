module GHC.Cpp.Types where

import Data.Map (Map)
import qualified Data.Map as Map

-- ---------------------------------------------------------------------

data CppDirective
    = CppInclude String
    | CppDefine String [String]
    | CppIfdef String
    | CppIfndef String
    | CppIf [String]
    | CppElse
    | CppEndif
    deriving (Show, Eq)

-- ---------------------------------------------------------------------

type MacroArgs = [String]
data MacroName = MacroName String (Maybe MacroArgs)
    deriving (Show, Eq, Ord)
type MacroDef = [String]

data MacroState = MacroState
    { pp_defines :: !(Map MacroName MacroDef)
    , pp_accepting :: !Bool
    }
    deriving (Show)

initMacroState :: MacroState
initMacroState = MacroState{pp_defines = Map.empty, pp_accepting = True}

type Input = String
type Output = CppDirective
