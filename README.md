The part for the GHC_CPP extension to actually process the CPP directives.

See [the proposal](https://github.com/ghc-proposals/ghc-proposals/pull/616)

This is intended to be integrated into GHC itself, and the work is on [this branch](https://gitlab.haskell.org/ghc/ghc/-/tree/wip/az/ghc-cpp)

The interface to GHC works as follows

1. A modified lexer recognises and emits two additional token types
   - A prepreocessor directive line
   - A CPP comment (needed for backward compatibility)

2. We have a preprocessor interposed between the GHC parser and the
   GHC lexer. This processes the directive lines, and keeps its own
   state as to whether to pass on or ignore normal tokens.

This part deals with processing the preprocessor directive lines, and
providing a state which can answer the question: should this token be
ignored or not.
