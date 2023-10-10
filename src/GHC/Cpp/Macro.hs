{-# LANGUAGE EmptyCase #-}

module GHC.Cpp.Macro where

-- From https://gcc.gnu.org/onlinedocs/cpp/Macros.html

{-

A macro is a fragment of code which has been given a name. Whenever
the name is used, it is replaced by the contents of the macro. There
are two kinds of macros. They differ mostly in what they look like
when they are used. Object-like macros resemble data objects when
used, function-like macros resemble function calls.

... the preprocessor operator `defined` can never be defined as a macro

If the expansion of a macro contains its own name, either directly or
via intermediate macros, it is not expanded again when the expansion
is examined for more macros. See
https://gcc.gnu.org/onlinedocs/cpp/Self-Referential-Macros.html for
details

-}

-- TODO: Parse tokens with original locations in them.

import qualified Data.Map as Map
import GHC.Cpp.Parse
import GHC.Cpp.Types

-- ---------------------------------------------------------------------

process :: MacroState -> Input -> (MacroState, Output)
process s str = (s0, o)
  where
    o = case regularParse cppDirective str of
        Left _ -> undefined
        Right r -> r
    s0 = case o of
        CppDefine name toks -> define s name toks
        CppInclude _ -> undefined
        CppIfdef name -> ifdef s name
        CppIfndef _ -> undefined
        CppElse -> undefined
        CppEndif -> undefined

-- ---------------------------------------------------------------------

define :: MacroState -> String -> MacroDef -> MacroState
define s name toks = s{pp_defines = Map.insert (MacroName name Nothing) toks (pp_defines s)}

ifdef :: MacroState -> String -> MacroState
ifdef s name =
    case Map.lookup (MacroName name Nothing) (pp_defines s) of
        Just _ -> s{pp_accepting = True}
        _ -> s

-- ---------------------------------------------------------------------

m0 = do
    let (s, _) = process initMacroState "#define FOO 3"
    process s "#ifdef FOO"
