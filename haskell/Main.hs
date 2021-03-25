module Main where

import Backtracking (backtracking, parseCaseBacktracking)
import BranchAndBound (branchAndBound, parseCaseBNB)
import Criterion.Measurement (getCPUTime)
import System.Environment (getArgs)
import Text.Printf (printf)

-- | Parsea argumentos em um algoritmo, nomes de arquivos em uma lista de nomes de arquivos e
--  conteúdos dos mesmos.
parse ::
  -- | Argumentos da linha de comando
  [String] ->
  IO [(String, (String, String))]
parse (x : xs) = mapM getContent xs
  where
    getContent f = do
      content <- readFile f
      return (x, (f, content))

-- | Converte uma lista de duas strings contendo representações de doubles em uma tupla de doubles
parseTuple ::
  -- | Lista de strings contendo dois doubles
  [String] ->
  (Double, Double)
parseTuple [x, y] = (read x, read y)

-- | Parsea e aplica o algoritmo em um único caso. Mede também o tempo e imprime
-- uma linha de um arquivo csv no formato algoritmo;arquivo;resultado;explorados;tempo
singleCase ::
  -- | Algoritmo, nome do arquivo e conteúdo do arquivo
  (String, (String, String)) ->
  IO ()
singleCase (alg, (name, contents)) = do
  start <- getCPUTime
  (result, nodes) <- run
  end <- getCPUTime

  printf "Pedro Tavares de Carvalho;2017014499;%s;%s;%f;%d;%f\n" alg name result nodes (end - start)
  where
    run :: IO (Double, Int)
    run = return $ chooseAlgorithm alg $ map (parseTuple . words) $ lines contents

-- | Escolhe o algoritmo que será aplicado no problema
chooseAlgorithm ::
  -- | Nome do algoritmo
  String ->
  -- | Função que converte uma array de tuplas sendo o arquivo parseado
  -- e retorna a solução do problema da mochila contido nessa representação.
  [(Double, Double)] ->
  (Double, Int)
chooseAlgorithm name
  | name == "bnb" = branchAndBound . parseCaseBNB
  | name == "back" = backtracking . parseCaseBacktracking

main :: IO ()
main = getArgs >>= parse >>= mapM_ singleCase
