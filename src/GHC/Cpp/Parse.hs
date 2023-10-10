module GHC.Cpp.Parse where

import Data.Char

import Control.Monad (void)
import Data.Functor.Identity
import Debug.Trace
import GHC.Cpp.Types
import GHC.Parser.Errors.Ppr ()
import Text.Parsec
import qualified Text.Parsec as Parsec
import Text.Parsec.Char as PS
import Text.Parsec.Combinator as PS
import qualified Text.Parsec.Expr as E
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim as PS hiding (token)
import qualified Text.Parsec.Token as P

-- ---------------------------------------------------------------------

type CppParser = Parsec String MacroState
-- type CppParser = Parsec String ()

parseDirective :: String -> Either Parsec.ParseError CppDirective
parseDirective = regularParse cppDirective

-- regularParse :: CppParser a -> String -> Either Parsec.ParseError (MacroState, a)
-- regularParse p = PS.parse p ""

regularParse :: CppParser a -> String -> Either Parsec.ParseError a
regularParse p str = do
    case parseMacroState initMacroState p str of
        Left e ->Left e
        Right (_,r) -> Right r

parseMacroState :: MacroState -> Parsec String MacroState a -> String -> Either ParseError (MacroState, a)
parseMacroState s p = Parsec.runParser p' s "source"
  where
    p' = do
        r <- p
        s' <- Parsec.getState
        return (s', r)

hCountParser :: Parsec.Parsec String Int ()
hCountParser = do
    void $ Parsec.char 'h'
    c <- Parsec.getState
    let c' = c + 1
    Parsec.putState c'
    return ()

foo :: Either ParseError Int
foo = Parsec.runParser (Parsec.many hCountParser >> Parsec.getState) 0 "" "hhhhhhhhhhhhellooo"

-- TODO: delete this
cppDefinition :: CppParser (String, [String])
cppDefinition = do
    _ <- PS.char '#'
    _ <- whiteSpace
    eToken "define"
    name <- cppToken
    definition <- cppTokens
    return (name, definition)


cppDirective :: CppParser CppDirective
cppDirective = do
    _ <- PS.char '#'
    _ <- whiteSpace
    choice
        [ cppKw "define" >> cmdDefinition
        , try $ cppKw "include" >> cmdInclude
        , try $ cppKw "ifdef" >> cmdIfdef
        , cppKw "ifndef" >> cmdIfndef
        , try $ cppKw "else" >> return CppElse
        , cppKw "endif" >> return CppEndif
        -- , cppKw "if" CppIfKw
        -- , cppKw "elif" CppElifKw
        -- , cppKw "undef" CppUndefKw
        -- , cppKw "error" CppErrorKw
        ]

cmdInclude :: CppParser CppDirective
cmdInclude = do
    _ <- string "\""
    filename <- many1 (satisfy (\c -> not (isSpace c || c == '"')))
    _ <- string "\""
    return $ CppInclude filename

cmdDefinition :: CppParser CppDirective
cmdDefinition = do
    name <- cppToken
    CppDefine name <$> cppTokens

cmdIfdef :: CppParser CppDirective
cmdIfdef = CppIfdef <$> cppToken

cmdIfndef :: CppParser CppDirective
cmdIfndef = CppIfndef <$> cppToken

cppKw :: String -> CppParser ()
cppKw kw = void $ lexeme (PS.string kw)

cppComment :: CppParser ()
cppComment = do
    _ <- PS.string "/*"
    _ <- PS.manyTill PS.anyChar (PS.try (PS.string "*/"))
    return ()

whiteSpace :: CppParser ()
whiteSpace = do
    _ <- PS.many (PS.choice [cppComment, void PS.space])
    return ()

lexeme :: CppParser a -> CppParser a
lexeme p = p <* whiteSpace

cppToken :: CppParser String
cppToken = lexeme (PS.many1 (PS.satisfy (not . isSpace)))

cppTokens :: CppParser [String]
cppTokens = PS.many cppToken

-- token :: String -> CppParser ()
-- token str = do
--     _ <- lexeme (PS.string str)
--     return ()

-- ---------------------------------------------------------------------
-- Expression language
-- NOTE: need to take care of macro expansion while parsing. Or perhaps before?

lexer :: P.TokenParser MacroState
lexer = P.makeTokenParser exprDef

exprDef :: P.LanguageDef st
exprDef =
    emptyDef
        { P.commentStart = "/*"
        , P.commentEnd = "*/"
        , P.commentLine = "//"
        , P.nestedComments = False
        , P.identStart = letter <|> char '_'
        , P.identLetter = alphaNum <|> oneOf "_'"
        , P.opStart = P.opLetter exprDef
        , P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , P.reservedOpNames = []
        , P.reservedNames = []
        , P.caseSensitive = True
        }

data Expr
    = Parens Expr
    | Var String
    | IntVal Int
    | Plus Expr Expr
    | Times Expr Expr
    | BinOp Op Expr Expr
    deriving (Show)

data Op
    = LogicalOr
    | LogicalAnd
    | CmpEqual
    | CmpGt
    | CmpGtE
    | CmpLt
    | CmpLtE
    deriving (Show)

-- -------------------------------------

plusTimesExpr :: CppParser Expr
plusTimesExpr = E.buildExpressionParser eTable eTerm

eTable :: [[E.Operator String MacroState Data.Functor.Identity.Identity Expr]]
eTable =
    -- Via https://learn.microsoft.com/en-us/cpp/cpp/cpp-built-in-operators-precedence-and-associativity?view=msvc-170
    [ [E.Infix (Times <$ symbol "*") E.AssocLeft]
    , [E.Infix (Plus <$ symbol "+") E.AssocLeft]
    ,
        [ E.Infix (try $ BinOp CmpLtE <$ symbol "<=") E.AssocLeft
        , E.Infix (try $ BinOp CmpGtE <$ symbol ">=") E.AssocLeft
        , E.Infix (BinOp CmpLt <$ symbol "<") E.AssocLeft
        , E.Infix (BinOp CmpGt <$ symbol ">") E.AssocLeft
        ]
    , [E.Infix (BinOp CmpEqual <$ symbol "==") E.AssocLeft]
    , [E.Infix (BinOp LogicalAnd <$ symbol "&&") E.AssocLeft]
    , [E.Infix (BinOp LogicalOr <$ symbol "||") E.AssocLeft]
    ]

eTerm :: CppParser Expr
eTerm =
    eVariable -- <|> pteNum
        <|> pteParens
        <|> eInteger

pteParens :: CppParser Expr
pteParens = Parens <$> between (symbol "(") (symbol ")") plusTimesExpr

symbol :: String -> CppParser String
symbol s = lexeme $ string s

-- -------------------------------------

eExpr :: CppParser Expr
eExpr = choice [eParens, eBinOp, eVariable]

eParens :: CppParser Expr
eParens = P.parens lexer $ do
    Parens <$> eExpr

eBinOp :: CppParser Expr
eBinOp = do
    e1 <- eExpr
    op <- eOp
    -- _ <- cppToken
    -- let op = Or
    BinOp op e1 <$> eExpr

eOp :: CppParser Op
eOp = do
    -- op <- P.operator lexer
    op <- P.operator (trace "foo" lexer)
    return $ trace ("op=" ++ show op) LogicalOr

eVariable :: CppParser Expr
eVariable = do
    v <- P.identifier lexer
    return $ Var v

eToken :: String -> CppParser ()
eToken = P.reserved lexer

eInteger :: CppParser Expr
eInteger = IntVal <$> integer

integer :: CppParser Int
integer = read <$> lexeme (many1 digit)

-- ---------------------------------------------------------------------

doTest :: String -> Either Parsec.ParseError CppDirective
doTest str = regularParse cppDirective str

t0 :: Either Parsec.ParseError CppDirective
t0 = doTest "#define FOO(m1,m2,m) ((m1) <  1 || (m1) == 1 && (m2) <  7 || (m1) == 1 && (m2) == 7 && (m) <= 0)"

t1 :: Either Parsec.ParseError Expr
t1 = regularParse plusTimesExpr "(m < 1)"

t2 :: Either Parsec.ParseError Expr
t2 = regularParse plusTimesExpr "((m1) <  1 || (m1) == 1 && (m2) <  7 || (m1) == 1 && (m2) == 7 && (m) <= 0)"

-- (Parens
--  (BinOp LogicalOr
--         (BinOp LogicalOr
--                (BinOp CmpLt (Parens (Var "m1")) (IntVal 1))
--                (BinOp LogicalAnd
--                       (BinOp CmpEqual (Parens (Var "m1")) (IntVal 1))
--                       (BinOp CmpLt (Parens (Var "m2")) (IntVal 7))))
--         (BinOp LogicalAnd
--                (BinOp LogicalAnd
--                       (BinOp CmpEqual (Parens (Var "m1")) (IntVal 1))
--                       (BinOp CmpEqual (Parens (Var "m2")) (IntVal 7)))
--                (BinOp CmpLtE (Parens (Var "m")) (IntVal 0)))))
