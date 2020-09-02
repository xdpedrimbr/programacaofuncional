--EX1:

--EX2:
distancias :: [(Float,Float)] -> [Float]
distancias [(a,b)] = [x | x <- [sqrt(a**2 + b**2)]]

--EX3:
--era pra abrir no GHCI

--EX4:
fatorialg :: Int -> Int
fatorialg n
 | n==0 = 1
 | otherwise = n * fatorialg (n-1)
 
--EX5:
fibog :: Int -> Int
fibog n
 | n == 1 = 1
 | n == 2 = 1
 | otherwise = fibog (n - 2) + fibog (n - 1)
 
--EX6:
n_tri :: Int -> Int
n_tri x
    | x == 1 = 1
    | x == 0 = 0
    | otherwise = n_tri(x) + n_tri(x-1) 
	
--EX7:
fibo2 :: Int -> Int
fibo2 1 = 1
fibo2 2 = 1
fibo2 n = fibo2(n-2)+fibo2(n-1)

retornaFibo :: Int->[Int]
retornaFibo x = [fibo x | x <- [1..50]]

retornaFibo2 :: Int->[Int]
retornaFibo2 x = [fibo2 x | x <- [1..50]]
	
--EX8:
potencia2 :: Int -> Int
potencia2 x
    | x == 0 = 1
    | otherwise = 2^potencia2(x-1)*2
	
--EX9:

--NAO TEM 10 NA LISTA

--EX11:


--EX12:
mdcg :: (Int,Int) -> Int
mdcg (m,n)
    | n == 0 = m
    | otherwise = mdcg (n, (mod m n))
                                     --usando casamento de padroes
mdc :: (Int,Int) -> Int
mdc (m,0) = m
mdc (m,n) = mdc (n, (mod m n))

--EX13:
binomialg :: (Int,Int) -> Int
binomialg (n,0)  = 1
binomialg (n,k)
| k == 0 = 1
| k == n = 1
| otherwise = binomialg (n-1,k) + binomialg (n-1,k-1)

                                     --usando casamento de padroes
binomial :: (Int,Int) -> Int
binomial (n,0)  = 1
binomial (n,k) = if (k == n) then 1
                             else binomial (n-1,k) + binomial (n-1,k-1)

--EX14:








