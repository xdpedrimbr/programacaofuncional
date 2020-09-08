--Integrantes da Dupla:
--Pedro Henrique Goncalves Teixeira-11821BCC008
--Maríllia Soares Rodrigues-11821BCC020


-- EX1:
triangulo :: Int -> Int -> Int -> String
triangulo a b c | a == b && a == c && b == c = "Equilatero"
				| a == b && a /= c = "isoceles"
				| a == c && a /= c = "isoceles"
				| b ==  c && a /= c = "isoceles"
				| b /= a && a /= c && b /= c = "escaleno"
				| a >= b+c || b >= a+c || c >= a+b = "Nao eh triangulo"
----------------------------------------------------------------------------------------------------------------------				

--EX2: 
delta :: Float -> Float -> Float -> Float
delta a b c = sqrt( (b^2) - (4 * a * c) )

equacao :: Float -> Float -> Float -> [(Float, Float)]
equacao a b c = [( ( (-b) + (delta a b c) ) / 2 * a, ( (-b) - (delta a b c) ) / 2 * a )]
----------------------------------------------------------------------------------------------------------------------

--EX3:
preco :: Float -> Float -> Float
preco p i | i >= 60 = p * 0.6
		  | i <= 10 && i > 2 = p * 0.5
		  | i <= 2 = p * 0.1
		  | otherwise = p
----------------------------------------------------------------------------------------------------------------------		  
		  
--EX4: 
listaPadrão :: [Integer]
listaPadrão = [1 .. 15]

gera1 :: [Integer]
gera1 = [x ^ 2 | x <- listaPadrão, odd x, x > 4, x < 15]

gera2 :: [(Integer, Integer)]
gera2 = [(x, y) | x <- listaPadrão, x <= 4, y <- [x .. x * 2]]

gera3 :: [[Integer]]
gera3 = [[1 .. y] | y <- listaPadrão, y >= 10, y <= 15]

gera4 :: [(Integer, Integer)]
gera4 = [(x, x + 1) | x <- listaPadrão, odd x]


gera5 :: [Integer]
gera5 = [x + y | (x, y) <- gera4]
----------------------------------------------------------------------------------------------------------------------

--EX5: 
neg :: [Int] -> [Int]
neg n = [n1 | n1 <- n, n1 < 0, mod n1 2 == 0]

neg2 :: [Int] -> [Int]
neg2 n = [x | x <- n, x < 0, x `mod` 2 == 0]
----------------------------------------------------------------------------------------------------------------------

--EX6:
dis :: [(Float,Float)] -> [Float]
dis [(a,b)] = [d | d <- [sqrt(a**2 + b**2)]]
----------------------------------------------------------------------------------------------------------------------

--EX7: 
fatores :: Int -> [Int]
fatores n = [x | x <- [1 .. n], n `mod` x == 0]

primos :: Int -> Int -> [Int]
primos ini fim = [n | n <- [ini .. fim], (fatores n) == [1, n]]
----------------------------------------------------------------------------------------------------------------------

--EX8: 
mdc::Int->Int->Int
mdc a b | a < b = mdc b a
 | b == 0 = a
 | otherwise = mdc b (mod a b)

mmc::Int->Int->Int
mmc x y = (x * y) `div` (mdc x y)

mmc3n::Int->Int->Int->Int
mmc3n x y z = mmc x (mmc y z)

----------------------------------------------------------------------------------------------------------------------

--EX9: 
serie :: Float -> Int -> Float
serie x n
  | n == 1 = 1 / x
  | even n = (x / fromIntegral (n)) + (serie x (n -1))
  | otherwise = (fromIntegral (n) / x) + (serie x (n -1))

----------------------------------------------------------------------------------------------------------------------


--EX10:
fizz :: Int -> String
fizz x
   | x `mod` 15 == 0 = "FizzBuzz"
   | x `mod` 5 == 0 = "Buzz"
   | x `mod` 3 == 0 = "Fizz"
   | otherwise = show x
----------------------------------------------------------------------------------------------------------------------

--EX11: 
contaOc::Int->[Int]->Int
contaOc a [] = 0
contaOc a (x:xs) | x == a = 1 + contaOc a xs
                 | otherwise = contaOc a xs
----------------------------------------------------------------------------------------------------------------------
	   
--EX12: 
unicaOcor::Int -> [Int] -> Bool
unicaOcor a [] = False
unicaOcor a (x:xs) | x == a = if (unicaOcor a xs) then False
 else True
 | otherwise = unicaOcor a xs
 
 ----------------------------------------------------------------------------------------------------------------------
 
--EX13:
intercala::[Int]->[Int]->[Int]
intercala x [] = x
intercala [] x = x
intercala (a:xs) (b:ys) = a: b: intercala xs ys
----------------------------------------------------------------------------------------------------------------------
 
 --EX14: 
type Contato = (String, String, String, String)

contatos :: [Contato]
contatos = [("Nome1", "Ender1", "Tel1", "Email1"),("Nome2", "Ender2", "Tel2", "Email2"),("Nome3", "Ender3", "Tel3", "Email3")]

encontraContato1 :: [Contato] -> String -> String
encontraContato1 [] email = "Email nao encontrado"
encontraContato1 ((nome, _, _, email) : n) emailn
  | email == emailn = nome
  | otherwise = encontraContato1 n email

encontraContato :: String -> String
encontraContato emailEncontrar = encontraContato1 contatos emailEncontrar

----------------------------------------------------------------------------------------------------------------------

--EX15: 
type Pessoa = (String, Float, Int, Char)

pessoas :: [Pessoa]
pessoas =
  [ ("João", 1.85, 26, 'C'),
    ("Maria", 1.55, 62, 'S'),
    ("Jose", 1.78, 42, 'C'),
    ("Paulo", 1.93, 25, 'S'),
    ("Clara", 1.70, 33, 'C'),
    ("Bob", 1.45, 21, 'C'),
    ("Rosana", 1.58, 39, 'S'),
    ("Daniel", 1.74, 72, 'S'),
    ("Jocileide", 1.69, 18, 'S') ]
	
mediah :: [Pessoa] -> Float
mediah lista = (somah lista) / fromIntegral (length lista) :: Float

somah :: [Pessoa] -> Float
somah [] = 0
somah ((nome, altura, idade, estado) : tail) = altura + (somah tail)

idades :: [Pessoa] -> [Int]
idades [] = []
idades ((nome, altura, idade, estado) : tail) = [id | id <- idade : (idades tail)]

nova :: [Pessoa] -> Int
nova lista = minimum (idades lista)

velha :: [Pessoa] -> Int
velha lista = maximum (idades lista)

encontraVelho :: [Pessoa] -> Int -> (String, Char)
encontraVelho [] idadeIn = ("", ' ')
encontraVelho((nome, _, idade, estadoCiv) : n) idadeIn
  | idadeIn == idade = (nome, estadoCiv)
  | otherwise = encontraVelho n idadeIn

velho :: [Pessoa] -> (String, Char)
velho lista = encontraVelho lista (velha lista)

maioridade lista =[(nome, altura, idade, estado) | (nome, altura, idade, estado) <- lista, idade >= 50]

casadasMaior lista x =
  [(nome, altura, idade, estado) | (nome, altura, idade, estado) <- lista, idade >= x, estado == 'C']

----------------------------------------------------------------------------------------------------------------------

--EX16:
insereOrdenado :: Ord a => a -> [a] -> [a]
insereOrdenado e [] = [e]
insereOrdenado e (x:xs)
 | e<=x = e : x : xs
 | otherwise = x : insereOrdenado e xs
---------------------------------------------------------------------------------------------------------------------- 

--EX17:
reverse1 :: [int] -> [int]
reverse1 [] = []
reverse1 (x:xs) = reverse1(xs) ++ (x:[])
----------------------------------------------------------------------------------------------------------------------

--EX18:
removeDups :: (Eq a) => [a] -> [a]
removeDups []             =  []
removeDups (xs : [])      =  [xs]
removeDups (x1:x2:xs)
          | x1 == x2     =      removeDups (x2 : xs)
          | otherwise    = x1 : removeDups (x2 : xs)
----------------------------------------------------------------------------------------------------------------------

--EX19:
disponiveis :: [Int]
disponiveis = [1, 2, 5, 10, 20, 50, 100]

notasTroco :: Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco valor = [head : tail | head <- disponiveis, head <= valor, tail <- notasTroco (valor - head)]

----------------------------------------------------------------------------------------------------------------------

--EX20:




----------------------------------------------------------------------------------------------------------------------

































