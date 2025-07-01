data Nat = Zero | Suc Nat deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Suc n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Suc(int2nat (n-1))

somar :: Nat -> Nat -> Nat
somar m n = int2nat (nat2int m + nat2int n)

mult :: Nat -> Nat -> Nat 
mult m Zero = Zero
mult m (Suc n') = somar n (mult m n')
