module Turing where

----------------------

type Symbol = Bool

blankSymbol :: Symbol
blankSymbol = False

----------------------

-- tape with head
data Tape = Tape
        { pos :: Int                -- head position
        , left :: [Symbol]          -- elems before the head
        , right :: [Symbol]         -- elem at the head and elems after the head
        }

eq :: Tape -> Tape -> Bool
eq t t' = goToMiddle t == goToMiddle t'

instance Eq Tape where
    (==) (Tape p l r) (Tape p' l' r') 
        = p == p' && eq l l' && eq r r'
     where
        eq [] [] = True
        eq (h:t) (h':t') = h==h' && eq t t'
        eq left right = eq (extend left) (extend right)

extend :: [Symbol] -> [Symbol]
extend [] = [blankSymbol]
extend l = l

symbolAtHead :: Tape -> Symbol
symbolAtHead t = head $ extend $ right t

blankTape :: Tape
blankTape = Tape 0 [] []

showTape :: Int -> Tape -> String
showTape i (Tape p l r)
    | n > 0 = replicate n ' ' ++ l'
    | otherwise = drop (-n) l'
  where
    n = i - length l + p
    l' = map tr $ reverse l ++ r
    tr x = if x then '1' else ' '

showTapeCentered :: (Char, Char, Char, Char) -> Int -> Tape -> String
showTapeCentered (t0, t1, h0, h1) w (Tape _ l r) =
    reverse ls ++ hs : rs
  where
    ls = map (showBool t0 t1) $ cut lw l
    hs = showBool h0 h1 $ head rest
    rs = map (showBool t0 t1) $ tail rest
    rest = cut rw r
    lw = w `div` 2
    rw = w - lw
    showBool f t b = if b then t else f
    cut n str =
        if len < n then str ++ replicate (n - len) False else take n str
      where len = length str

--------------------

data Shift = L | R deriving (Eq, Ord, Show, Read)

shift :: Shift -> Tape -> Tape
shift L (Tape p l r) = case extend l of h:t -> Tape (p-1) t (h: r)
shift R (Tape p l r) = case extend r of h:t -> Tape (p+1) (h: l) t

write :: Symbol -> Tape -> Tape
write s (Tape p l r) = case extend r of _:t -> Tape p l (s:t)

goToMiddle :: Tape -> Tape
goToMiddle t = case compare (pos t) 0 of
    EQ  -> t
    LT  -> goToMiddle $ shift R t
    GT  -> goToMiddle $ shift L t

-----------------------

type State = Char

initialState, haltingState :: State
initialState = 'A'
haltingState = 'H'

-----------

type Transition = ((State, Symbol),  (Symbol, Shift, State))

type Machine = [Transition]

run :: Machine -> State -> Tape -> [Tape]
run _ s _ | s == haltingState   = []
run m s t = case lookup (s, symbolAtHead t) m of
        Nothing                 -> []
        Just    (sym, sh, next) -> let t' = write sym t in t': run m next (shift sh t')
