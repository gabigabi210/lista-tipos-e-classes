data Ordering = LT | EQ | GT

compare :: Ord a => a -> a -> Ordering
compare a b | a < b = LT
            | a > b = GT    
            | otherwise = EQ

data Arvore a = Vazia | No a (Arvore a) (Arvore a) deriving (Show, Eq)

existe :: Ord a => a -> Arvore a -> Bool
existe _ Vazia = False
existe x (No valor esquerda direita) | compare x valor == LT = existe x esquerda
                                     | compare x valor == EQ = True
                                     | compare x valor == GT = existe x direita
