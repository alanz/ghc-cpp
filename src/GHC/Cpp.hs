module GHC.Cpp (
    process,
    PpState (..),
    MacroName(..),
    initPpState,
    Input,
    Output,
) where

import GHC.Cpp.Macro
import GHC.Cpp.Types (Input, PpState (..), Output, initPpState, MacroName(..))
