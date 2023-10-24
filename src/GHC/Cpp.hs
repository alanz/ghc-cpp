module GHC.Cpp (
    process,
    MacroState (..),
    MacroName(..),
    initMacroState,
    Input,
    Output,
) where

import GHC.Cpp.Macro
import GHC.Cpp.Types (Input, MacroState (..), Output, initMacroState, MacroName(..))
