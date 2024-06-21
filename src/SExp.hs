module SExp (SExp (..), Identifier (..), atom, sexpParser) where

import Data.Scientific
import RIO hiding (bool, many, some, try)
import Test.QuickCheck (oneof, resize, sized)
import Test.QuickCheck.Arbitrary
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype Identifier = Identifier {id :: String} deriving (Show, Eq, Ord)

instance Arbitrary Identifier where
    arbitrary = do
        s <- arbitrary
        return $ Identifier{id = s}

data SExp
    = SSExp [SExp] -- (foo "hello" 42 (bar false))
    | SInteger Integer -- 42
    | SString String -- "hello, world"
    | SBool Bool -- false, true
    | SId Identifier -- foo
    | SDouble Double -- 42f, 23.5f, -1.25f
    deriving (Show, Eq, Ord)

instance Arbitrary SExp where
    arbitrary = sized sexp'
      where
        sexp' 0 = oneof atomGenerartors
        sexp' n = oneof $ (SSExp <$> resize (n `div` 2) arbitrary) : atomGenerartors
        atomGenerartors =
            [ SInteger <$> arbitrary
            , SDouble <$> arbitrary
            , SString <$> arbitrary
            , SId <$> arbitrary
            , SBool <$> arbitrary
            ]

type Parser =
    Parsec
        Void -- type of custom error messages
        String -- Input stream type

skipSpace :: Parser ()
skipSpace = L.space space1 (L.skipLineComment ";;") (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

bool :: Parser Bool
bool = lexeme $ False <$ string "false" <|> True <$ string "true"

numeric :: Parser SExp
numeric = label "numeric" $ lexeme $ do
    value <- L.scientific
    case floatingOrInteger value of
        Left d -> SDouble d <$ char' 'f'
        Right i -> do
            f <- optional $ char' 'f'
            pure $ case f of
                -- Note: 1.0 is an integer, but 1.1 is a parser error.
                Nothing -> SInteger i
                Just _ -> SDouble $ fromIntegral i

identifier :: Parser Identifier
identifier = label "identifier" $ lexeme $ do
    firstChar <- letterChar <|> char '+'
    otherChars <- many alphaNumChar
    return Identifier{id = firstChar : otherChars}

str :: Parser String
str = label "string" $ lexeme $ char '"' *> manyTill L.charLiteral (char '"')

sexp :: Parser [SExp]
sexp = label "S expression" $ lexeme $ between (char '(') (char ')') (many atom)

atom :: Parser SExp
atom =
    choice
        [ SBool <$> bool
        , numeric
        , SId <$> identifier
        , SString <$> str
        , SSExp <$> sexp
        ]
sexpParser :: Parser SExp
sexpParser = between skipSpace eof atom
