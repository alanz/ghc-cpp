{
module GHC.Cpp.Lexer (lex_tok, lexCppTokenStream) where

import GHC.Cpp.ParserM (
                St, init_pos,
                ParserM (..), Action, mkT, mkTv, Token(..), start_code,
                set_start_code,
                show_pos, position,
                AlexInput(..), alexGetByte)
import qualified GHC.Cpp.ParserM as ParserM (input)
import Control.Monad

-- The lexer is based on
-- https://timsong-cpp.github.io/cppwp/n4140/lex.pptoken
}

words :-

    <0>         $white+              ;
---------------------------------------
    <0>         "{"                  { mkT TOpenBrace }
    <0>         "}"                  { mkT TCloseBrace }
    <0>         "["                  { mkT TOpenBracket }
    <0>         "]"                  { mkT TCloseBracket }
    <0>         "#"                  { mkT THash }
    <0>         "##"                 { mkT THashHash }
    <0>         "("                  { mkT TOpenParen }
    <0>         ")"                  { mkT TCloseParen }
    <0>         "<:"                 { mkT TLtColon }
    <0>         ":>"                 { mkT TColonGt}
    <0>         "<%"                 { mkT TLtPercent }
    <0>         "%>"                 { mkT TPercentGt }
    <0>         "%:"                 { mkT TPercentColon }
    <0>         "%:%:"               { mkT TPercentColonTwice }
    <0>         ";"                  { mkT TSemi }
    <0>         ":"                  { mkT TColon }
    <0>         "..."                { mkT TDotDotDot }
    <0>         "new"                { mkT TNew }
    <0>         "delete"             { mkT TDelete }
    <0>         "?"                  { mkT TQuestion }
    <0>         "::"                 { mkT TColonColon}
    <0>         "."                  { mkT TDot }
    <0>         ".*"                 { mkT TDotStar }
    <0>         "+"                  { mkT TPlus }
    <0>         "-"                  { mkT TMinus }
    <0>         "*"                  { mkT TStar }
    <0>         "/"                  { mkT TSlash }
    <0>         "%"                  { mkT TPercent }
    <0>         "^"                  { mkT TUpArrow }
    <0>         "&"                  { mkT TAmpersand }
    <0>         "|"                  { mkT TPipe }
    <0>         "~"                  { mkT TTilde }
    <0>         "!"                  { mkT TExclamation }
    <0>         "="                  { mkT TEqual }
    <0>         "<"                  { mkT TOpenAngle }
    <0>         ">"                  { mkT TCloseAngle }
    <0>         "+="                 { mkT TPlusEqual }
    <0>         "-="                 { mkT TMinusEqual }
    <0>         "*="                 { mkT TStarEqual }
    <0>         "/="                 { mkT TSlashEqual }
    <0>         "%="                 { mkT TPercentEqual }
    <0>         "^="                 { mkT TUpEqual }
    <0>         "&="                 { mkT TAmpersandEqual }
    <0>         "|="                 { mkT TPipeEqual }
    <0>         "<<"                 { mkT TLtLt }
    <0>         ">>"                 { mkT TGtGt }
    <0>         ">>="                { mkT TGtGtEqual }
    <0>         "<<="                { mkT TLtLtEqual }
    <0>         "=="                 { mkT TEqualEqual }
    <0>         "!="                 { mkT TExclaimEqual }
    <0>         "<="                 { mkT TLtEqual }
    <0>         ">="                 { mkT TGtEqual }
    <0>         "&&"                 { mkT TAmpersandTwice }
    <0>         "||"                 { mkT TPipePipe }
    <0>         "++"                 { mkT TPlusPlus }
    <0>         "--"                 { mkT TMinusMinus }
    <0>         ","                  { mkT TComma }
    <0>         "->*"                { mkT TMinusGtStar }
    <0>         "->"                 { mkT TMinusGt }
    <0>         "and"                { mkT TAnd }
    <0>         "and_eq"             { mkT TAndEq }
    <0>         "bitand"             { mkT TBitand }
    <0>         "bitor"              { mkT TBitor }
    <0>         "compl"              { mkT TCompl }
    <0>         "not"                { mkT TNot }
    <0>         "not_eq"             { mkT TNotEq }
    <0>         "or"                 { mkT TOr }
    <0>         "or_eq"              { mkT TOrEq }
    <0>         "xor"                { mkT TXor }
    <0>         "xor_eq"             { mkT TXorEq }
----------------------------------------
    <0>         [a-z][a-zA-Z0-9\#_]* { mkTv TLowerName }
    <0>         [A-Z][a-zA-Z0-9\#_]* { mkTv TUpperName }
    <0>         \-? [0-9][0-9]*      { mkTv (TInteger . read) }
    <0>         \" [^\"]* \"         { mkTv (TString . tail . init) }
    <0>         ()                   { begin other }

    <other>     .+                   { \i -> do {set_start_code 0;
                                                 mkTv TOther i} }

{

begin :: Int -> Action
begin sc _str =
  do set_start_code sc
     get_tok

get_tok :: ParserM Token
get_tok = ParserM $ \i st ->
   case alexScan i (start_code st) of
       AlexEOF -> Right (i, st, TEOF)
       AlexError _ -> Left ("Lexical error at " ++ show_pos (position i))
       AlexSkip i' _ -> case get_tok of
                            ParserM f -> f i' st
       AlexToken i' l a -> case a $ take l $ ParserM.input i of
                               ParserM f -> f i' st

lex_tok :: (Token -> ParserM a) -> ParserM a
lex_tok cont = get_tok >>= cont

lexCppTokenStream :: String -> St -> Either String (AlexInput, St, [Token])
lexCppTokenStream s = unParserM go (AlexInput init_pos [] s)
    where
    go = do
      ltok <- lex_tok return
      case ltok of
        TEOF -> return []
        _ -> liftM (ltok:) go
}
