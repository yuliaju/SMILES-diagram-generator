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
    deriving (Eq)

instance Show Bond where
  show Single    = "-"
  show Double    = "="
  show Triple    = "#"
  show Aromatic  = ":"

data Token  = TokBond Bond
            | TokAtom String
            | TokBranch [Token]
    deriving (Eq)

instance Show Token where
  show (TokBond bond) = show bond
  show (TokAtom atom) = atom
  show (TokBranch tokens) = "( " ++ (concatTokenStrs $ fmap show tokens) ++ " )"

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
  | otherwise = error $ "Cannot tokenize " ++ [c]
  where splitRest = removePivotChar ')' rest

removePivotChar :: Char -> String -> (String, String)
remotePivotChar _ [] = []
removePivotChar c str = (front, back)
  where front = takeWhile (/= c) str
        back  = if (lenAfterC > 1) then tail afterC
                else
                  if (lenAfterC == 1) then []
                  else error "Unterminated branch in input"
        afterC      = dropWhile (/= c) str
        lenAfterC   = length afterC

toBond :: Char -> Bond
toBond c
  | c == '-' = Single
  | c == '=' = Double
  | c == '#' = Triple
  | c == ':' = Aromatic

concatTokenStrs :: [String] -> String
concatTokenStrs = unwords

isBond :: Char -> Bool
isBond = (`elem` "-=#:")

main :: IO ()
main = do
  smiles <- getLine
  putStrLn $ concatTokenStrs $ fmap show $ tokenize smiles
