{-# LANGUAGE TupleSections #-}

module BranchAndBound (branchAndBound, parseCaseBNB) where

import Data.List (sortOn)
import Knapsack (Bound, Item, Knapsack, Possibilities)

-- | Entrada para o algoritmo de resolução do problema da mochila por branch and bound.
data Input = Input [(Knapsack, Int)] Bound Possibilities

-- | Resolve o problema da mochila com uma approach branch and bound.
branchAndBound ::
  -- | Lista de estados a ser explorados, e a bound atual
  Input ->
  (Double, Int)
-- Caso não existam estados a ser explorados, o espaço de busca acabou
-- e a bound representa o melhor estado da busca
branchAndBound (Input [] b _) = (b, 0)
-- estado inicial, achando uma bound válida inicial para o problema
branchAndBound (Input [(knapsack, -1)] _ itemPossibilities) =
  branchAndBound (Input [(knapsack, 0)] feasibleBound itemPossibilities)
  where
    feasibleBound = getFeasible knapsack itemPossibilities
branchAndBound (Input (state : remainingStates) bound items) =
  -- o valor ótimo é atualizado a cada chamada
  (best, nodes + 1)
  where
    (ksCurrent, level) = state
    currentPossibilities
      | length items /= level = drop level items
      | otherwise = [(0, 0)]
    (currentItem : ps) = currentPossibilities
    -- nós exploráveis a partir do estado atual
    exp = getExplorable ksCurrent currentItem level (length items)
    expState = map (,level + 1) exp
    -- nós exploráveis que são melhores que a bound e transformados em estados de exploração
    edge =
      let explorableStates = expState ++ remainingStates
          f (expKnapsack, stateLevel) = theoreticBest expKnapsack expItems > bound
            where
              expItems = drop stateLevel items
       in filter f explorableStates
    -- deconstrução da mochila
    (capacity, ksValue, _) = ksCurrent
    (newValue, nodes) = branchAndBound (Input edge newBound items)
    newBound = max ksValue bound
    best = max newValue ksValue

-- | Algoritmo para determinar a bound. O algoritmo resolve o problema da mochila
--  fracionário, que é resolvido por um algoritmo guloso.
theoreticBest ::
  -- | Mochila
  Knapsack ->
  -- | Possibilidades de itens a serem adicionados
  Possibilities ->
  Double
-- caso todos os itens tenham sido explorados, o valor da mochila é o valor máximo
theoreticBest (_, ksWeight, _) [] = ksWeight
theoreticBest knapsack@(capacity, ksValue, ksWeight) (item@(iValue, iWeight) : ps)
  -- se ainda há espaço na mochila, adicione o estado na mochila e continue
  | iWeight + ksWeight < capacity = theoreticBest (knapsackIncludingItem knapsack item) ps
  -- caso a mochila vá encher com esse estado, retorne o valor contido na mochila com o estado parcial ou completa
  | otherwise = ksValue + ratio * iValue
  where
    ratio = (capacity - ksWeight) / iWeight

-- | Converte uma lista de tuplas em uma entrada para um caso de problema da mochila para ser
-- resolvido pelo algoritmo de branch and bound. Ordena os itens por densidade de valor por peso,
-- para melhorar a complexidade do algoritmo de heurística.
parseCaseBNB ::
  -- | Lista de tuplas gerada pelo arquivo.
  [(Double, Double)] ->
  Input
parseCaseBNB ((_, capacity) : xs) = Input [((capacity, 0, 0), -1)] 0 sortedPossibilities
  where
    sortedPossibilities = sortOn (\(a, b) -> b / a) xs

-- | Gera estados exploráveis a partir de um nó. Não leva em conta a bound.
getExplorable ::
  -- | Estado da mochila a ser explorada
  Knapsack ->
  -- | Item a ser adicionado ou não no estado
  Item ->
  Int ->
  Int ->
  [Knapsack]
getExplorable knapsack@(capacity, _, ksWeight) item@(_, iWeight) level nItems
  | level == nItems || ksWeight == capacity = []
  | ksWeight + iWeight <= capacity = [knapsack, knapsackIncludingItem knapsack item]
  | otherwise = [knapsack]

-- função para encontrar uma solução viável e atualizar a bound
getFeasible :: (Ord a, Num p, Num a) => (a, p, a) -> [(p, a)] -> p
getFeasible knapsack items
  | ksWeight + iWeight > capacity = ksValue
  | otherwise =
    let newKnapsack = (capacity, ksValue + iValue, ksWeight + iWeight)
     in getFeasible newKnapsack remainingItems
  where
    ((iValue, iWeight) : remainingItems) = items
    (capacity, ksValue, ksWeight) = knapsack

knapsackIncludingItem :: Knapsack -> Item -> Knapsack
knapsackIncludingItem (capacity, ksValue, ksWeight) (iValue, iWeight) = (capacity, ksValue + iValue, ksWeight + iWeight)
