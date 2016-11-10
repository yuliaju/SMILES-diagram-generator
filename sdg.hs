import Control.Monad
import Data.Char
import Data.Functor
import Data.Traversable
import System.IO

data Bond = Single
          | Double
          | Triple
          | Aromatic
    deriving (Eq)

instance Show Bond where
  show Single    = "-"
  show Double    = "="
  show Triple    = "#"
  show Aromatic  = ":"

data Token  = TokAtom String
            | TokBond Bond
            | TokBranch [Token]
    deriving (Eq)

instance Show Token where
  show (TokAtom atom) = atom
  show (TokBond bond) = show bond
  show (TokBranch tokens) = "( " ++ (concatTokenStrs $ fmap show tokens) ++ " )"

data Molecule = Nil
              | MolAtom String
              | MolBond Bond
              | MolList [Molecule]
    deriving (Eq)

instance Show Molecule where
  show (MolAtom atom) = atom
  show (MolBond bond) = show bond
  show (MolList mols) = concatTokenStrs $ fmap show mols

-- parser
moleculeP :: [Token] -> Molecule
moleculeP [] = Nil
moleculeP [token] = tokenP token
moleculeP tokens = MolList $ fmap tokenP tokens

tokenP :: Token -> Molecule
tokenP (TokAtom atom) = MolAtom atom
tokenP (TokBond bond) = MolBond bond
tokenP (TokBranch []) = Nil
tokenP (TokBranch (x:xs)) = MolList $ tokenP x : fmap tokenP xs

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

interactish :: (String -> String) -> IO ()
interactish f = getLine >>= (putStrLn . f)

main :: IO ()
main = do
  forever $ interactish $ show . moleculeP . tokenize
