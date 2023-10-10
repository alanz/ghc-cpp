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

import GHC.Cpp.Types
import GHC.Cpp.Parse

-- ---------------------------------------------------------------------

process :: MacroState -> Input -> (MacroState, Output)
process s str = (s0, o)
    where
      (s0, o) = case parseMacroState s cppDirective str of
          Left _ -> undefined
          Right r -> r

-- ---------------------------------------------------------------------

m0 :: (MacroState, Output)
m0 = process initMacroState "#define FOO 3"
