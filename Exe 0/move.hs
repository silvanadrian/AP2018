type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move South (x,y) = (x, y-1)
move East  (x,y) = (x+1, y)

moves :: [Direction] -> Pos -> Pos
moves [] (x,y) = (x,y)
moves (xa:xs) (x,y) = moves xs (move xa (x,y))

data Nat = Zero | Succ Nat
    deriving (Eq, Show, Read, Ord)

addNat :: Nat -> Nat -> Nat
addNat Zero n = n
addNat n Zero = n
addNat a (Succ b) = addNat (Succ a) b

mulNat :: Nat -> Nat -> Nat
mulNat Zero _ = Zero
mulNat _ Zero = Zero
mulNat x (Succ y) = addNat (mulNat x y) x

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ a) = 1 + nat2int a

-- nat2intHelper :: Nat -> Int -> Int
-- nat2intHelper Zero b = b
-- nat2intHelper (Succ a) b = nat2intHelper a b+1

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat a = Succ (int2nat (a-1))