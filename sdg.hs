import Control.Applicative
import Control.Monad
import Data.Char
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data Bond   = Single
            | Double
            | Triple
            | Aromatic
    deriving (Show, Eq)
data Token  = TokBond Bond
            | TokAtom String
    deriving (Show, Eq)

-- Lexer
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : rest)
  | isBond c  = TokBond (toBond c) : tokenize rest
  | isUpper c =
    if rest == [] then (TokAtom [c]) : []
    else
      if isLower (head rest) then TokAtom [c, head rest] : tokenize rest
      else TokAtom [c] : tokenize rest

toBond :: Char -> Bond
toBond c
  | c == '-' = Single
  | c == '=' = Double
  | c == '#' = Triple
  | c == ':' = Aromatic

showContent :: Token -> String
showContent (TokBond bond) = bondToStr bond
showContent (TokAtom atom) = atom

isBond :: Char -> Bool
isBond c = c `elem` "-=#:"

bondToStr :: Bond -> String
bondToStr Single    = "-"
bondToStr Double    = "="
bondToStr Triple    = "#"
bondToStr Aromatic  = ":"

main :: IO ()
main = do
  smiles <- getLine
  putStrLn $ unwords $ fmap showContent $ tokenize smiles
