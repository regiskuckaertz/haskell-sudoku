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
sudoku = (filter valid) . expand

valid :: Grid -> Bool
valid g = a && b && c

groupValid :: Row Digit -> Bool

rows :: Grid -> Grid
rows = id

columns :: Grid -> Grid
columns [r] = [ [d] | d <- r ]
columns (r:rs) = zipWith (:) r (columns rs)

boxes :: Grid -> Grid
boxes = _

group :: [a] -> [[a]]
group [] = []
group r = take 3 r : group (drop 3 r)

expand :: Grid -> [Grid]
