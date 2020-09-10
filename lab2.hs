conv :: (Float)->(Float,Float,Float)
conv (x)=(x,x*3.96,x*4.45)



bissexto :: Int -> Bool
bissexto ano | mod ano 4 == 0 && (mod ano 100 /= 0 || mod ano 400 == 0) = True
             | otherwise = False



type Data = (Int, Int, Int)
bissexto2 :: Data -> Bool
bissexto2 (x, y, ano) = bissexto ano



valida::Data->Bool
valida (d,m,a)
     | d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 ||
           m == 7 || m == 8 || m == 10 || m == 12) = True
     | d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 ||
          m == 11) = True
     | d >= 1 && d <= 28 && m == 2 && not (bissexto a) = True
     | d >= 1 && d <= 29 && m == 2 && (bissexto a) = True
     | otherwise = False
	

precede :: Data -> Data -> Bool
precede (d1, m1, a1) (d2, m2, a2) 
     |a1 < a2 = True
	 |a1 == a2 && m1 < m2 = True
	 |a1 == a2 && m1 == m2 && d1 < d2 = True
     |otherwise = False


	
type Livro = (Int, String, String, String, Int)
type Aluno = (Int, String, String, Int)
type Emprestimo = (String, String, Data, Data, String)


biblioteca :: Emprestimo -> Data -> String
biblioteca (e1, e2, (de, me, ae), (ddd, md, ad) (d, m, a)
    |e5 == "aberto" = show "Emprestimo em aberto"
	|otherwise = show "emprestimo ok"





	 
	 
