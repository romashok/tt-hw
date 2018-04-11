{-# LANGUAGE FlexibleContexts #-}

module Expr where

import           Control.Applicative ((*>), (<*))
import           Data.Char           (isAlpha, isDigit, isSpace)
import           Text.Parsec
import           Text.Parsec.Char    (char)

data Expr
    = Lambda Var Expr
    | Expr :$: Expr
    | Var Var
    deriving (Eq, Ord)
infixl 9 :$:
type Var = String

instance Show Expr where
    show (Lambda var expr) = embrace $ "\\" ++ var ++ "." ++ show expr
    show (expr1 :$: expr2) = embrace $ show expr1 ++ " " ++ show expr2
    show (Var var)         = var

embrace :: String -> String
embrace str = "(" ++ str ++ ")"

expressionParser :: Parsec String () Expr
expressionParser = do
    _ <- many $ satisfy isSpace
    application <- optionMaybe applicationParser
    abstraction <- optionMaybe abstractionParser
    case (application, abstraction) of
        (Just appl, Just abstr) -> pure $ appl :$: abstr
        (Just appl, Nothing   ) -> pure appl
        (Nothing  , Just abstr) -> pure abstr
        (Nothing  , Nothing   ) -> fail ""

abstractionParser :: Parsec String () Expr
abstractionParser = do
    _ <- token' $ char '\\'
    vars <- many1 $ token' varParser
    _ <- token' $ string "."
    expr <- token' expressionParser
    pure $ foldr Lambda expr vars

applicationParser :: Parsec String () Expr
applicationParser = chainl1 (token' atomParser) (pure (:$:))

atomParser :: Parsec String () Expr
atomParser = (token' (string "(") *> expressionParser <* token' (string ")")) <|> do
    var <- token' varParser
    pure $ Var var

varParser :: Parsec String () Var
varParser = do
    l <- satisfy isAlpha <?> "variable"
    ls <- many (satisfy isAlpha <|> satisfy isDigit <|> char '\'') <?> "variable suffix"
    pure (l:ls)

token' :: Parsec String () a -> Parsec String () a
token' parser = parser <* many (satisfy isSpace)
