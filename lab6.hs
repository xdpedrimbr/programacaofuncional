--EX1:
paridade :: Integral a => [a] -> [Bool]
paridade lista = map (even) lista

--EX2:
prefixos :: [String] -> [String]
prefixos lista = map (take 3) lista

--EX3:
saudacao :: [[Char]] -> [[Char]]
saudacao lista = map ("Oi " ++) lista

--EX4:
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar func (x : xs)
  | func x = x : filtrar func xs
  | otherwise = filtrar func xs

filtrarLis :: (a -> Bool) -> [a] -> [a]
filtrarLis func lista = [x | x <- lista, func x]

--EX5:
pares :: Integral a => [a] -> [a]
pares lista = filter (even) lista

--EX6:
solucoes :: (Ord a, Num a) => [a] -> [a]
solucoes l = filter (\x -> ((5 * x) + 6) < (x * x)) l

--EX7?
maior :: (Foldable t, Ord a) => t a -> a
maior lista = foldr1 max lista

--EX8:
menor_min10 :: (Foldable t, Ord b, Num b) => t b -> b
menor_min10 lista = foldr (min) 10 lista

--EX9:
junta_silabasPlural :: Foldable t => t [Char] -> [Char]
junta_silabasPlural lista = foldr (++) "s" lista

--EX10:
lst1 :: [Integer]
lst1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

lst2 :: [Integer]
lst2 = [20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

lst3 :: [Integer]
lst3 = [11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

lst4 :: [Integer]
lst4 = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11]

lst5 :: [Integer]
lst5 = [11, 12, 13, 14, 15, 5, 4, 3, 2, 1, 16, 17, 18, 19, 20, 10, 9, 8, 7, 6]

lst6 :: [Integer]
lst6 = [1, 12, 3, 14, 5, 15, 4, 13, 2, 11, 6, 17, 8, 19, 20, 10, 9, 18, 7, 16]

lst7 :: [Integer]
lst7 = [1 .. 1000]

lst8 :: [Integer]
lst8 = [1000, 999 .. 1]

lst9 :: [Integer]
lst9 = lst1 ++ [0]

lst10 :: [Integer]
lst10 = [0] ++ lst3

lst11 :: [Integer]
lst11 = lst1 ++ [0] ++ lst3

lst12 :: [Integer]
lst12 = lst3 ++ [0] ++ lst1

------------------------------------------------
bubblesort :: Ord a => [a] -> [a]
bubblesort [] = []
bubblesort lista = bubbleOrd lista (length lista)

bubbleOrd :: (Num t, Ord a, Eq t) => [a] -> t -> [a]
bubbleOrd lista 0 = lista
bubbleOrd lista n = bubbleOrd (catch lista) (n -1)

catch :: Ord a => [a] -> [a]
catch [x] = [x]
catch (x : y : zs)
  | x > y = y : catch (x : zs)
  | otherwise = x : catch (y : zs)


selectionsort :: Ord a => [a] -> [a]
selectionsort [] = []
selectionsort xs = [x] ++ selectionsort (remove x xs)
  where
    x = minimo xs

remove :: Eq t => t -> [t] -> [t]
remove a [] = []
remove a (x : xs)
  | a == x = xs
  | otherwise = x : (remove a xs)

minimo :: Ord a => [a] -> a
minimo [] = undefined
minimo [x] = x
minimo (x : xs)
  | x <= (minimo xs) = x
  | otherwise = minimo xs


isertionsort :: (Ord a) => [a] -> [a]
isertionsort [] = []
isertionsort (x : xs) = insereOrd x (isertionsort xs)

insereOrd :: Ord t => t -> [t] -> [t]
insereOrd x [] = [x]
insereOrd x (y : ys)
  | x <= y = (x : y : ys)
  | otherwise = y : (insereOrd x ys)


isertionsort :: (Ord a) => [a] -> [a]
isertionsort [] = []
isertionsort (s : xs) = isertionsort [x | x <- xs, x < s] ++ [s] ++ isertionsort [x | x <- xs, x >= s]

--EX11:
bubble2 :: (Ord a) => [a] -> ([a], Int)
bubble2 [] = ([], 0)
bubble2 lista = bubbleOrd2 (lista, 0) (length lista)

bubbleOrd2 :: (Ord a, Num t, Num b, Eq t) => ([a], b) -> t -> ([a], b)
bubbleOrd2 (lista, count) 0 = (lista, count)
bubbleOrd2 (lista, count) n = bubbleOrd2 (catch2 (lista, count)) (n -1)

catch2 :: (Ord a, Num b) => ([a], b) -> ([a], b)
catch2 ([x], cont) = ([x], cont)
catch2 ((x : y : zs), cont) =
  if x > y
    then add (catch2 ((x : zs), cont + 1)) y
    else add (catch2 ((y : zs), cont + 1)) x
  where
    add (lista, count) a = (a : lista, count)

----------------------------------------------------------

selectionsort2 :: Ord a => [a] -> ([a], Int)
selectionsort2 lista = selection2 lista 0

selection2 :: (Ord a) => [a] -> Int -> ([a], Int)
selection2 [] n = ([], n)
selection2 (x : xs) n =
  let (least, n_num) = minimo2 (x : xs) n

      remove2 _ [] = []
      remove2 n (h : t) =
        if (n == h)
          then t
          else h : (remove2 n t)

      add (lst, n) y = (y : lst, n)
   in add (selection2 (remove2 least (x : xs)) n_num) least

minimo2 :: (Ord a) => [a] -> Int -> (a, Int)
minimo2 [] _ = undefined
minimo2 [x] cont = (x, cont)
minimo2 (x : y : xs) cont
  | x > y = minimo2 (y : xs) (cont + 1)
  | otherwise = minimo2 (x : xs) (cont + 1)

----------------------------------------------------------

isertionsort2 :: (Ord a) => [a] -> ([a], Int)
isertionsort2 [] = ([], 0)
isertionsort2 [x] = ([x], 0)
isertionsort2 (h : t) =
  let (sorted_tail, n) = isertionsort2 t

      (lst, n1) = insereOrd2 h sorted_tail n
   in (lst, n1)

insereOrd2 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insereOrd2 x [] n = ([x], n)
insereOrd2 x (h : t) n =
  if (x <= h)
    then ((x : h : t), n + 1)
    else add (insereOrd2 x t (n + 1)) h
  where
    add (list, n) y = (y : list, n)

----------------------------------------------------------

quick2 :: [a] -> Int -> (a -> Bool) -> ([a], Int)
quick2 [] n _ = ([], n)
quick2 (x : xs) n cond =
  if (cond x)
    then add (quick2 xs (n + 1) cond) x
    else quick2 xs (n + 1) cond
  where
    add (list, n) y = (y : list, n)

----------------------------------------------------------

isertionsort2 :: (Ord a) => [a] -> ([a], Int)
isertionsort2 [] = ([], 0)
isertionsort2 (piv : xs) =
  let (left, n_L) = quick2 xs 0 (<= piv)
      (right, n_R) = quick2 xs 0 (> piv)
      (sorted_L, n1_L) = isertionsort2 left
      (sorted_R, n1_R) = isertionsort2 right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)

--EX12:

bubble3 :: (Ord a) => [a] -> ([a], Int)
bubble3 [] = ([], 0)
bubble3 lista = bubbleOrd3 (lista, 0) (length lista)

bubbleOrd3 :: (Ord a, Num t, Num b, Eq t) => ([a], b) -> t -> ([a], b)
bubbleOrd3 (lista, count) 0 = (lista, count)
bubbleOrd3 (lista, count) n = bubbleOrd3 (catch3 (lista, count)) (n -1)

catch3 :: (Ord a, Num b) => ([a], b) -> ([a], b)
catch3 ([x], cont) = ([x], cont)
catch3 ((x : y : zs), cont) =
  if x > y
    then add (catch3 ((y : zs), cont + 1)) x
    else add (catch3 ((x : zs), cont + 1)) y
  where
    add (lista, count) a = (a : lista, count)

----------------------------------------------------------

selectionsort3 :: Ord a => [a] -> ([a], Int)
selectionsort3 lista = selection21 lista 0

selection21 :: (Ord a) => [a] -> Int -> ([a], Int)
selection21 [] n = ([], n)
selection21 (x : xs) n =
  let (least, n_num) = minimo3 (x : xs) n

      remove3 _ [] = []
      remove3 n (h : t) =
        if (n == h)
          then t
          else h : (remove3 n t)

      add (lst, n) y = (y : lst, n)
   in add (selection21 (remove3 least (x : xs)) n_num) least

minimo3 :: (Ord a) => [a] -> Int -> (a, Int)
minimo3 [] _ = undefined
minimo3 [x] cont = (x, cont)
minimo3 (x : y : xs) cont
  | x > y = minimo3 (x : xs) (cont + 1)
  | otherwise = minimo3 (y : xs) (cont + 1)

----------------------------------------------------------

isertionsort3 :: (Ord a) => [a] -> ([a], Int)
isertionsort3 [] = ([], 0)
isertionsort3 [x] = ([x], 0)
isertionsort3 (h : t) =
  let (sorted_tail, n) = isertionsort3 t

      (lst, n1) = insereOrd3 h sorted_tail n
   in (lst, n1)

insereOrd3 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insereOrd3 x [] n = ([x], n)
insereOrd3 x (h : t) n =
  if (x >= h)
    then ((x : h : t), n + 1)
    else add (insereOrd3 x t (n + 1)) h
  where
    add (list, n) y = (y : list, n)

----------------------------------------------------------

quick21 :: [a] -> Int -> (a -> Bool) -> ([a], Int)
quick21 [] n _ = ([], n)
quick21 (x : xs) n cond =
  if (cond x)
    then quick21 xs (n + 1) cond
    else add (quick21 xs (n + 1) cond) x
  where
    add (list, n) y = (y : list, n)


isertionsort3 :: (Ord a) => [a] -> ([a], Int)
isertionsort3 [] = ([], 0)
isertionsort3 (piv : xs) =
  let (left, n_L) = quick21 xs 0 (<= piv)
      (right, n_R) = quick21 xs 0 (> piv)
      (sorted_L, n1_L) = isertionsort3 left
      (sorted_R, n1_R) = isertionsort3 right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)