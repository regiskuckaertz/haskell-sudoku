type Row a = [a]
type Matrix a = [Row a]
type Digit = Char
type Grid = Matrix Digit

-- sample3 is unsolvable
sample1, sample2, sampl3 :: Grid
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
cols (r:rs) = zipWith (:) r (columns rs)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group

group :: [a] -> [[a]]
group [] = []
group r = take 3 r : group (drop 3 r)

ungroup :: [[a]] -> [a]
ungroup = concat

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = nodups xs && all (/= x) xs

choices :: Grid -> Matrix [Digit]
choices = map (map choose)
  where choose '.' = digits
        choose d   = [d]

expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp

-- cartesian product, e.g.
-- cp [[a,b],[c,d]] = [[a,c],[a,d],[b,c],[b,d]]
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [ y:ys | y <- xs, ys <- xss' ] where xss' = cp xss