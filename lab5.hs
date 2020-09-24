--EX1:

bissexto :: Int -> Bool
bissexto ano = x || (y && w)
  where
    x = (mod ano 400 == 0)
    y = (mod ano 4 == 0)
    w = (mod ano 100 /= 0)

type Data = (Int, Int, Int)

valida :: Data -> Bool
valida (dia, mes, ano) = x || y || w || z
  where
    x = dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12)
    y = dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11)
    w = dia >= 1 && dia <= 28 && mes == 2 && not (bissexto ano)
    z = dia >= 1 && dia <= 29 && mes == 2 && (bissexto ano)

--
bissextos :: [Int] -> [Int]
bissextos lista = x
  where
    x = [x | x <- lista, bissexto x]

--
type Emprestimo = (String, String, Data, Data, String)

type Emprestimos = [Emprestimo]

bdEmprestimo :: Emprestimos
bdEmprestimo =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

procede :: Data -> Data -> Bool
procede (dia, mes, ano) (dia2, mes2, ano2) = not (x || y || w || z)
  where
    x = not (valida (dia, mes, ano)) || not (valida (dia2, mes2, ano2))
    y = ano > ano2
    w = ano == ano2 && mes > mes2
    z = ano == ano2 && mes == mes && dia > dia2

emprestimoEmDia :: Data -> Emprestimo -> Bool
emprestimoEmDia dataAtual (codLivro, codAluno, dataEmprest, dataDevo, status) = x
  where
    x = procede dataAtual dataDevo

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados listaEmprestimos dataAtual = x
  where
    x = [x | x <- listaEmprestimos, not (emprestimoEmDia dataAtual x)]

--
passo :: (Int, Int) -> (Int, Int)
passo (x, y) = x
  where
    x = (y, x + y)

fibo2 :: Int -> (Int, Int)
fibo2 0 = (0, 1)
fibo2 n = x
  where
    x = passo (fibo2 (n -1))

--
prodIntervalo :: Int -> Int -> Int
prodIntervalo m n = if (m >= n) then x else y
  where
    x = n
    y = (m * (prodIntervalo (m + 1) n))

fatInter :: Int -> Int
fatInter n = x
  where
    x = prodIntervalo 1 n

--EX2:
--
bissexto :: Int -> Bool
bissexto ano =
  let x = (mod ano 400 == 0)
      y = (mod ano 4 == 0)
      w = (mod ano 100 /= 0)
   in x || (y && w)

type Data = (Int, Int, Int)

valida :: Data -> Bool
valida (dia, mes, ano) =
  let x = dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12)
      y = dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11)
      w = dia >= 1 && dia <= 28 && mes == 2 && not (bissexto ano)
      z = dia >= 1 && dia <= 29 && mes == 2 && (bissexto ano)
   in x || y || w || z

--
bissextos :: [Int] -> [Int]
bissextos lista =
  let x = [x | x <- lista, bissexto x]
   in x

--
type Emprestimo = (String, String, Data, Data, String)

type Emprestimos = [Emprestimo]

bdEmprestimo :: Emprestimos
bdEmprestimo =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

procede :: Data -> Data -> Bool
procede (dia, mes, ano) (dia2, mes2, ano2) =
  let x = not (valida (dia, mes, ano)) || not (valida (dia2, mes2, ano2))
      y = ano > ano2
      w = ano == ano2 && mes > mes2
      z = ano == ano2 && mes == mes && dia > dia2
   in not (x || y || w || z)

emprestimoEmDia:: Data -> Emprestimo -> Bool
emprestimoEmDia dataAtual (codLivro, codAluno, dataEmprest, dataDevo, status) =
  let x = procede dataAtual dataDevo
   in x

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados listaEmprestimos dataAtual =
  let x = [x | x <- listaEmprestimos, not (emprestimoEmDia dataAtual x)]
   in x

--
aux :: (Int, Int) -> (Int, Int)
aux (x, y) =
  let x = (y, x + y)
   in x

fibo2 :: Int -> (Int, Int)
fibo2 0 = (0, 1)
fibo2 n =
  let x = aux (fibo2Let (n -1))
   in x

--
prodIntervalo :: Int -> Int -> Int
prodIntervalo m n =
  let x =
        if (m >= n)
          then n
          else (m * (prodIntervalo (m + 1) n))
   in x

fat :: Int -> Int
fat n =
  let x = prodIntervalo 1 n
   in x

--EX3: 
--EX4: 

--EX5: