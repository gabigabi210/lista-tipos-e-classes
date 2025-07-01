data Arvore a = Folha a | No (Arvore a) (Arvore a) deriving (Show, Eq)
contarFolhas :: Arvore a -> Int
contarFolhas (Folha _) = 1
contarFolhas (No esquerda direita) = contarFolhas esquerda + contarFolhas direita

balanceada :: Arvore a -> Bool
balanceada (Folha _) = True
balanceada (No esquerda direita) = abs (contarFolhas esquerda - contarFolhas direita) <= 1 && balanceada esquerda && balanceada direita
