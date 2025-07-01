data Expr = Val Int | Add Expr Expr deriving (Show, Eq)
avaliar :: Expr -> Int
avaliar (Val n) = n
avaliar (Add esquerda direita) = avaliar esquerda + avaliar direita
