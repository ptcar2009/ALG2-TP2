module Backtracking (backtracking, parseCaseBacktracking) where

import Knapsack (Capacity, Knapsack, Possibilities)

-- | Resolve o problema da mochila utilizando a técnica do backtracking
backtracking ::
  -- | Entrada do algoritmo, considerando os itens e a capacidade da mochila
  (Possibilities, Capacity) ->
  (Double, Int)
backtracking (items, capacity) =
  recursiveBacktracking (capacity, 0, 0) items

-- | Backtracking recursivo auxiliar para resolver o problema da mochila.
recursiveBacktracking ::
  -- | Mochila
  Knapsack ->
  -- | Itens a serem adicionados na mochila
  Possibilities ->
  (Double, Int)
-- Caso não existam itens a serem explorados, retorne o valor atual e um nó explorado
recursiveBacktracking (_, currentValue, _) [] = (currentValue, 1)
recursiveBacktracking knapsack@(capacity, currentValue, knapsackWeight) ((itemValue, itemWeight) : remaining)
  -- Caso o tem não caiba na mochila, retorne o valor da busca sem o item
  | itemWeight + knapsackWeight > capacity = (excludingValue, 1 + enodes)
  -- Caso ele caiba, retorne o máximo entre a busca sem o item e com o item
  | otherwise = (max includingValue excludingValue, inodes + enodes + 1)
  where
    knapsackWithItem = (capacity, n, knapsackWeight + itemWeight)
    n = currentValue + itemValue
    (includingValue, inodes) = recursiveBacktracking knapsackWithItem remaining
    (excludingValue, enodes) = recursiveBacktracking knapsack remaining

-- | Parseia um caso de problema da mochila para ser resolvido por backtracking
parseCaseBacktracking ::
  -- | Lista de tuplas representando um arquivo parseado
  [(Double, Double)] ->
  (Possibilities, Double)
-- pega a capacidade do primeiro item e os itens do resto da lista
parseCaseBacktracking ((_, capacity) : items) = (items, capacity)
