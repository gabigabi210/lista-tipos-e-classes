data Arvore a = Folha a | No (Arvore a) (Arvore a) deriving (Show, Eq)
dividirMeio :: [a] -> ([a], [a])
dividirMeio lista = (take metade1 lista, drop metade1 lista)
    where
        n = length lista
        metade1 = n `div` 2

balancear :: [a] -> Arvore a
balancear [x] = Folha x
balancear xs = 
    let (esquerda, direita) = dividirMeio xs in No (balancear esquerda) (balancear direita)
