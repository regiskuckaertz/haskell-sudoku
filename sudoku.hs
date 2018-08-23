module Sudoku where

type Row a = [a]
type Matrix a = [Row a]
type Digit = Char
type Grid = Matrix Digit

-- sample3 is unsolvable
sample1, sample2, sample3 :: Grid
sample1 = ["2....1.38"
          ,"........5"
          ,".7...6..."
          ,".......13"
          ,".981..257"
          ,"31....8.."
          ,"9..8...2."
          ,".5..69784"
          ,"4..25...."]

sample2 = [".9.7..86."
          ,".31..5.2."
          ,"8.6......"
          ,"..7.5...6"
          ,"...3.7..."
          ,"5...1.7.."
          ,"......1.9"
          ,".2.6..35."
          ,".54..8.7."]

sample3 = ["1..9.7..3"
          ,".8.....7."
          ,"..9...6.."
          ,"..72.94.."
          ,"41.....95"
          ,"..85.43.."
          ,"..3...7.."
          ,".5.....4."
          ,"2..8.6..9"]

digits :: [Digit]
digits = ['1'..'9']

sudoku :: Grid -> [Grid]
sudoku = filter valid . expand . choices

valid :: Grid -> Bool
valid g = all nodups (rows g)
       && all nodups (cols g)
       && all nodups (boxs g)

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [r] = [ [d] | d <- r ]
cols (r:rs) = zipWith (:) r (cols rs)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneWith boxs . pruneWith rows . pruneWith cols

pruneWith :: ( [[a]] -> [[a]] ) -> Matrix [Digit] -> Matrix [Digit]
pruneWith f = f . map reduce . f

singular :: [a] -> Bool
singular [a] = True
singular _   = False

diff :: [a] -> [a] -> [a]
diff [a] b
  | elem a b = []
  | otherwise = [a]
diff [] _ = []
diff (a:as) b = (diff [a] b) ++ (diff as b)

trim :: [[a]] -> [[a]]
trim ass =
  let sings = concat (filter singular ass) in
    ass minus sings

-- we can check that `boxs` is an involution, assuming `cols` is:
-- boxs . boxs = map ungroup . ungroup . map cols . group . map group .
--               map ungroup . ungroup . map cols . group . map group
-- { functor laws }
--             = map ungroup . ungroup . map cols . group . map (group . ungroup) .
--               ungroup . map cols . group . map group
-- { group . ungroup = id, twice }
--             = map ungroup . ungroup . map cols . map cols . group . map group
-- { cols . cols = id and functor law }
--             = map ungroup . ungroup . group . map group
-- { group . ungroup = id, twice and functor law }
--             = id

group :: [a] -> [[a]]
group [] = []
group r = take 3 r : group (drop 3 r)

ungroup :: [[a]] -> [a]
ungroup = concat

-- this is O(n^2) but since n = 9, it beats any constant
-- factor of a logarithmic solution
nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = nodups xs && x `notElem` xs

-- naive choices: each blank cell can receive
-- any valid digits, while filled cells can only
-- receive what they already have
choices :: Grid -> Matrix [Digit]
choices = map (map choose)
  where choose '.' = digits
        choose d   = [d]

-- naive expansion: we just compute the cartesian product
-- for each row, giving us a grid where each row is made of
-- all possible rows, then we compute the cartesian product
-- of all the rows and get the list of all possible grids
expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp

-- cartesian product, e.g.
-- cp [[a,b],[c,d]] = [[a,c],[a,d],[b,c],[b,d]]
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [ y:ys | y <- xs, ys <- xss' ] where xss' = cp xss
