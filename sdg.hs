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
            | TokBranch [Token]
    deriving (Show, Eq)

-- lexer
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : rest)
  | c == '(' = TokBranch (tokenize $ fst splitRest) : (tokenize $ snd splitRest)
  | isBond c  = TokBond (toBond c) : tokenize rest
  | isUpper c =
    if rest == [] then (TokAtom [c]) : []
    else
      if isLower (head rest) then TokAtom [c, head rest] : tokenize rest
      else TokAtom [c] : tokenize rest
  where splitRest = removePivotChar ')' rest

removePivotChar :: Char -> String -> (String, String)
remotePivotChar _ [] = []
removePivotChar c str = (front, back)
  where front = takeWhile (/= c) str
        back  = if (length (dropWhile (/= c) str) > 1) then tail $ dropWhile (/= c) str
                else
                  if (length (dropWhile (/= c) str) == 1) then []
                  else error "Unterminated branch in input"

toBond :: Char -> Bond
toBond c
  | c == '-' = Single
  | c == '=' = Double
  | c == '#' = Triple
  | c == ':' = Aromatic

showContent :: Token -> String
showContent (TokBond bond) = bondToStr bond
showContent (TokAtom atom) = atom
showContent (TokBranch tokens) = "( " ++ (concatTokenStrs $ fmap showContent tokens) ++ " )"

concatTokenStrs :: [String] -> String
concatTokenStrs = unwords

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
  putStrLn $ concatTokenStrs $ fmap showContent $ tokenize smiles
