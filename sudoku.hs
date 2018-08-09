type Row a = [a]
type Matrix a = [Row a]
type Digit = Int
type Grid = Matrix Digit

digits = [1..9]

sudoku :: Grid -> [Grid]
sudoku = (filter valid) . expand

valid :: Grid -> Bool
valid g = a && b && c

groupValid :: Row Digit -> Bool

rows :: Grid -> [Row Digit]
rows = id

columns :: Grid -> [Row Digit]
columns [r] = [ [d] | d <- r ]
columns (r:rs) = map prepend zipped
  where zipped = r `zip` columns rs
        prepend (d, ds) = d:ds


boxes :: Grid -> [Row Digit]
boxes =  . map groupsOfThree

groupsOfThree :: Row a -> [[a]]
groupsOfThree [] = []
groupsOfThree r = take 3 r : groupsOfThree (drop 3 r)

expand :: Grid -> [Grid]
