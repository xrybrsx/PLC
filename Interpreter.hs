module Main where
import Tokens
import Grammar
import System.IO
import Control.Monad ()
import Control.Exception
import Data.List
import System.Environment ()
import System.Environment
import Data.Maybe

main :: IO()
main = catch main' noParse


main' :: IO ()
main' = do 
     (fileName : _) <- getArgs
     showParse fileName

readCSV:: FilePath -> IO [[String]]
readCSV file = do
      content <- readFile file
      return $ map (`proccesSingleLine` []) $ lines content

proccesSingleLine :: String -> String -> [String]
proccesSingleLine [] word = [word]
proccesSingleLine (',':xs) word = word:proccesSingleLine xs []
proccesSingleLine (' ':xs) word = proccesSingleLine xs word
proccesSingleLine (x:xs) word = proccesSingleLine xs (word++[x])

showParse :: FilePath -> IO ()
showParse fileName = do
           sourceText <- readFile fileName
           let parsedProg = reverse $ parseTree (alexScanTokens sourceText)
           (_, env') <- evalLoop(parsedProg, [], [])
           return ()

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

data Frame = AddVarFrame String | InsertFrame String | SelectFrame [Int] |
             WhereFrame0 [Int] Exp Exp | WhereFrame1 [Int] [[String]] Exp | WhereFrame2 [Int] [[String]] [[String]] |
             WhereExists1 [Int] Exp | WhereExists2 [Int] [[String]] |
             MergeFrame1 Exp | MergeFrame2 [[String]] |
             JoinFrame1 Exp | JoinFrame2 [[String]] |
             LeftJoinFrame1 Exp | LeftJoinFrame2 [[String]] |
             JoinConstFrame String
      deriving (Show, Eq)

type Environment = [ (String,Exp) ]

type Kontinuation = [ Frame ]

type State = (Exp,Environment,Kontinuation)

evalLoop :: ([Exp], Environment, Kontinuation) -> IO ([Exp], Environment)
evalLoop ([], env, _) = return ([], env)
evalLoop (x:xs, env, k) =
      do
            (e', env', k') <- eval1 (x, env, k)
            if (e' == x) && isValue e' && null k then do
            evalLoop (xs, env', k')
            else do evalLoop (e':xs, env', k')

-- Look up a value in an environment and unpack it
getValueBinding :: String -> Environment -> Exp
getValueBinding x [] = error "Variable binding not found"
getValueBinding x ((y,e):env) | x == y  = e
                              | otherwise = getValueBinding x env

update :: Environment -> String -> Exp -> Environment
update env x e = (x,e) : env

-- Checks for terminated expressions
isValue :: Exp -> Bool
isValue (PepInt _) = True
isValue (StringMatrix  _) = True
isValue _ = False

--Small step evaluation function
eval1 :: State -> IO State
eval1 (Var x,env,k) = return (e',env,k)
                    where e' = getValueBinding x env

-- Rule for terminated evaluations
eval1 (v,env,[]) | isValue v = return (v,env,[])

eval1 (AddVar e, env, AddVarFrame varName:k) = return (PepInt 0, update env varName e, k)

-- import
eval1 (Imp (Var var), env, k) = readCSV (var++".csv") >>= \content -> return (StringMatrix content, env, k)

-- import as
eval1 (ImpAs (Var var1) (Var var2), env, k) = readCSV (var1++".csv") >>= \content -> return (AddVar $ StringMatrix content, env, AddVarFrame var2:k)

--insertInto
eval1 (Insert (Var var) e2, env, k) = return (e2, env, InsertFrame var:k)
eval1 (e@(StringMatrix _), env, InsertFrame var:k) = return (AddVar e, env, AddVarFrame var:k)

-- select
eval1 (Select list e EndSelect, env, k) = return (e, env, SelectFrame (reverse list):k)
eval1 (StringMatrix matrix, env, SelectFrame list:k) = do
      let b = extractColumns matrix list
      return (StringMatrix b, env, k)

-- where equal
eval1 (Select list e (WhereEqual e1 e2), env, k) = return (e, env, WhereFrame0 list e1 e2:k) 
eval1 (StringMatrix original, env, WhereFrame0 list e1 e2:k) = return (e1, env, WhereFrame1 list original e2:k)
eval1 (PepInt n, env, f@(WhereFrame1 _ original _):k) = return (StringMatrix $ extractColumns original [n], env, f:k)
eval1 (StringMatrix matrix1, env, WhereFrame1 list original e2:k) = return (e2, env, WhereFrame2 list original matrix1:k)
eval1 (PepInt m, env, f@(WhereFrame2 _ original _):k) = return (StringMatrix $ extractColumns original [m], env, f:k)
eval1 (StringMatrix matrix2, env, WhereFrame2 list original matrix1:k) = do
      let equalElements = keepOnEqualElements matrix1 matrix2 original
      return (Select list (StringMatrix equalElements) EndSelect, env, k)      

-- where exists
eval1 (Select list e (WhereExists e1), env, k) = return (e, env, WhereExists1 list e1:k)
eval1 (StringMatrix original, env, WhereExists1 list e:k) = return (e, env, WhereExists2 list original:k)
eval1 (PepInt n, env, f@(WhereExists2 _ original):k) = return (StringMatrix $ extractColumns original [n], env, f:k)
eval1 (StringMatrix matrix, env, WhereExists2 list original:k) = do
      let existElements = keepOnExists matrix original
      return (Select list (StringMatrix existElements) EndSelect, env, k)

-- merge
eval1 (Select list e (Merge e1), env, k) = return (Select list e EndSelect, env, MergeFrame1 e1:k)
eval1 (StringMatrix matrix1, env, MergeFrame1 e1:k) = return (e1, env, MergeFrame2 matrix1:k)
eval1 (StringMatrix matrix2, env, MergeFrame2 matrix1:k) = return (StringMatrix $ mergeTwo matrix1 matrix2, env, k)

-- join
eval1 (Select list e (Join e1), env, k) = return (Select list e EndSelect, env, JoinFrame1 e1:k)
eval1 (StringMatrix matrix1, env, JoinFrame1 e1:k) = return (e1, env, JoinFrame2 matrix1:k)
eval1 (StringMatrix matrix2, env, JoinFrame2 matrix1:k) = return (StringMatrix $ joinTwo matrix1 matrix2, env, k)

-- join const
eval1 (Select list e (ConstString string), env, k) = return (Select list e EndSelect, env, JoinConstFrame string:k)
eval1 (StringMatrix matrix1, env, JoinConstFrame string:k) = do
      let added = map (++[string]) matrix1
      return (StringMatrix added, env, k)

-- left join
eval1 (Select list e (LeftJoin e1), env, k) = return (Select list e EndSelect, env, LeftJoinFrame1 e1:k)
eval1 (StringMatrix matrix1, env, LeftJoinFrame1 e1:k) = return (e1, env, LeftJoinFrame2 matrix1:k)
eval1 (StringMatrix matrix2, env, LeftJoinFrame2 matrix1:k) = return (StringMatrix $ leftJoinTwo matrix1 matrix2, env, k)

--export
eval1 (Export (Var var), env, k) = do
      putStrLn $ contentsForExport var env
      return (PepInt 0, env, k)

eval1 (e, env, k) =
      do
            print "Runtime error in expression below."
            print e
            print env
            print k
            error "Unhandled expression"

mergeTwo :: [[String]] -> [[String]] -> [[String]]
mergeTwo _ [] = []
mergeTwo [] _ = []
mergeTwo (x:xs) ys = map (x++) ys ++ mergeTwo xs ys

joinTwo :: [[String]] -> [[String]] -> [[String]]
joinTwo _ [] = []
joinTwo [] _ = []
joinTwo (x:xs) (y:ys) = (x++y):joinTwo xs ys

leftJoinTwo :: [[String]] -> [[String]] -> [[String]]
leftJoinTwo _ [] = []
leftJoinTwo [] _ = []
leftJoinTwo (x:xs) (y:ys) = leftJoinRow x y : leftJoinTwo xs ys

leftJoinRow :: [String] -> [String] -> [String]
leftJoinRow _ [] = []
leftJoinRow [] _ = []
leftJoinRow (x:xs) (y:ys)
      | null x = y : leftJoinRow xs ys
      | otherwise = x : leftJoinRow xs ys

keepOnEqualElements :: [[String]] -> [[String]] -> [[String]] -> [[String]]
keepOnEqualElements (x:xs) (y:ys) (z:zs)
      | x == y = z:keepOnEqualElements xs ys zs
      | otherwise = keepOnEqualElements xs ys zs
keepOnEqualElements _ _ _ = []

keepOnExists :: [[String]] -> [[String]]  -> [[String]]
keepOnExists (x:xs) (y:ys)
      | null (head x) = keepOnExists xs ys
      | otherwise = y:keepOnExists xs ys  
keepOnExists _ _ = []

extractColumns :: [[String]] -> [Int] -> [[String]]
extractColumns matrix list = transpose $ extractColumns' matrix list

extractColumns' :: [[String]] -> [Int] -> [[String]]
extractColumns' _ [] = []
extractColumns' matrix (x:xs) = map (!!(x-1)) matrix: extractColumns' matrix xs

contentsForExport :: String -> Environment -> String
contentsForExport var env = intercalate "\n" (sort $ map (intercalate ",") a)
      where
      StringMatrix a = snd $ head [a | a <- env, fst a == var]