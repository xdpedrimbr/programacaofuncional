dobro :: Int -> Int
dobro x = x * 2

dobro2 :: Int -> Int
dobro2 y = dobro y * 2

hip :: Float -> Float -> Float
hip cat1 cat2 = sqrt ((cat1 ^ 2) + (cat2 ^ 2))

dis :: Float -> Float -> Float -> Float -> Float 
dis x0 y0 x1 y1 = sqrt(((x1 - x0) ^ 2) + ((y1 - y0) ^ 2)) 

par :: Int -> Bool
par x = if mod x  2 == 0 then True 
                         else False

impar :: Int -> Bool
impar x = if mod x  2 == 0 then False 
                         else True
				
conv::Float->Float 
conv f = (f-32.0) /1.8

maior :: Int -> Int -> Int 
maior x y = if x > y then x
					 else y
					 
maior3 :: Int -> Int -> Int -> Int
maior3 x y z = if x > y && x > z then x
								else if y > x && y > z then y
								else z
								
teste :: Int -> Int 
teste x = if x < 0 then -1
				   else if x > 0 then 1
				   else 0
				   
ehDigito :: Char -> Bool
ehDigito c = if c >= '0' && c <= '9' then "digito"
									 else "nao eh digito"
									 
cat :: Int -> Int -> Int -> Int
cat z x y = if z*z - x*x + y*y == 0 then 1
									else 0






