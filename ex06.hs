data Expr = Val Int | Op Expr Expr deriving (Show, Eq)
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Op esquerda direita) = g (folde f g esquerda) (folde f g direita)
