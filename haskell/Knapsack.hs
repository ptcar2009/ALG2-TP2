module Knapsack where

-- | Capacidade de uma mochila
type Capacity = Double

-- | Bound inferior para o algoritmo.
type Bound = Double

-- | Peso de um estado ou da soma de itens.
type Weight = Double

-- | Valor de um estado ou da soma de itens.
type Value = Double

-- | Representação de um estado
type Item = (Value, Weight)

-- | Possibilidades de itens a serem explorados
type Possibilities = [Item]

-- | Mochila.
type Knapsack = (Capacity, Value, Weight)
