{- ********** fibonacci  ************************

Devuelve el n-ésimo término de la sucesión de Fibonacci

    problema: fibonacci (n : Z) : Z {
    requiere: { n >= 0 }
    asegura : { res = fib(n) }
    } 
-}


fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

{-  ************** parteEntera *********************

Devuelve la parte entera del número. 
 
    problema: parteEntera ( x:R ) : Z {
    requiere: {True}
    asegura : { res <= x <= res +1 }
    }
 -}

parteEntera :: Float -> Integer
parteEntera x
    | x < 1 = 0
    | otherwise = 1 + parteEntera(x-1)
 
{- ************** esDivisible ********************
 
Determina si un número es divisible por otro. 

    problema: esDivisible (a: Z, b: z): Bool {
    requiere: {a > 0 & b > 0}
    asegura : { res = True sii existe k in Z / a = kb}
    }
-}

esDivisible :: Integer -> Integer -> Bool
esDivisible a b 
    | a == b = True
    | a < b  = False
    | otherwise = esDivisible (a-b) b

{- ************ medioFact *************************
 
Calcula el medio factorial. Es decir n(n-2)(n-4) ...

    problema: medioFact (n: Z) : Z {
    requiere: { n >= 0 }
    asegura: { res = n!! }
    }
-}

medioFact :: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * medioFact (n-2)

{- ********* todosDigitosIguales ******************

Determina si todos los dígitos de un número son iguales. 
 
    problema: todosDigitosIguales (n:Z) : Bool {
    requiere: { n > 0 }
    asegura: { res = True sii todos los dígitos son iguales }
    }
-}

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n 
    | n < 10 = True 
    | otherwise = ultimosIguales n && todosDigitosIguales (div n 10) 

{- ******** ultimosIguales *************************
 
Determina si los dos últimos dígitos de un número son iguales. 

    problema: ultimosIguales ( n: Z ) : Bool {
    requiere: {True}
    asegura: { res = True sii los últimos dos dígitos son iguales }
    }
-}

ultimosIguales :: Integer -> Bool
ultimosIguales n =  mod n 10 == mod (div n 10) 10

{- ******** menorDivisor **************************

Encuentra el mínimo del conjunto de divisores positivos de un número natural. 
 
    problema: menorDivisor (n: Z): Z {
    requiere: {n > 0}
    asegura: { res|n && ( k|n -> res <= k )
    }
-}

menorDivisor :: Integer -> Integer
menorDivisor n  = buscarDivisor 2 n

{- ******** buscarDivisor ************************

Para determinado número, encuentra el menor divisor que es mayor
a otro número dado. 

    problema: buscarDivisor (k:Z, n: Z): Z {
    requiere: {n >= 0}
    asegura: { res = el menor de los divisores mayores que k }
    }
-}

buscarDivisor :: Integer -> Integer -> Integer 
buscarDivisor k n 
    | mod n k == 0 = k   
    | otherwise = buscarDivisor (k+1) n 

{- ******** iesimoDigito ***********************

Encuentra el dígito que está en cierta posición. 

    problema: iesimoDigito (n:Z , i:Z): Z {
    requiere: { n > 0 && i > 0}
    asegura: { res es el dígito de n que se encuentra en la posición i}
    }
-}


iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i
    | i == numeroDeDigitos(n) = mod n 10
    | otherwise = iesimoDigito (div n 10) i

{- *********** numeroDeDigitos *******************

Encuentra la cantidad de dígitos de un número entero n

    problema: numeroDeDigitos (n: Z): Z {
    requiere: {n>0}
    asegura: { res = cantidad de dígitos de n}
    }
-}

numeroDeDigitos :: Integer -> Integer 
numeroDeDigitos n
    | n < 10 = 1
    | otherwise = 1 + numeroDeDigitos(div n 10)
    
{- *********** sumaDigitos ***********************

Calcula la suma de los dígitos de un número entero

    problema: sumaDigitos (n: Z) : Z  {
    requiere: {Z > 
    asegura: { res = suma de los dígitos de n)}
    }
-}

sumaDigitos :: Integer -> Integer
sumaDigitos n
    | n < 10 = n
    | otherwise = mod n 10 + sumaDigitos (div n 10)


{- ********** esCapicua ***************************
 
Determina si un número entero es capicúa

    problema: esCapicua (n: Z) : Bool {
    requiere: {n > 0}
    asegura: { res = True sii n es capicúa }

-}
esCapicua :: Integer -> Bool
esCapicua n
    | n < 10 = True
    | otherwise = primerDigito n == ultimoDigito n && esCapicua (losDelMedio n)
  
  where  
    primerDigito :: Integer -> Integer
    primerDigito n = div n (10^(numeroDeDigitos n -1)) 

    ultimoDigito :: Integer -> Integer
    ultimoDigito n = mod n 10

    losDelMedio :: Integer -> Integer
    losDelMedio n = div n 10 `mod` (10^(numeroDeDigitos n -2))

{- *********** f1 f2 f3 f4  **************************************

f1(n) =  sum_2^n (2^i)
f2(n,q) = sum_1^n (q^i)
f3(n,q) = sum_1^(2n) (q^i)
f4(n,q) = sum_n^(2n) (q^i)

    problema f_k (n,q) (n:Int, q:float) -> float {
    requiere: { }
    asegura: { res = f_i (n, q) }
    }
-}

f1 :: Integer -> Integer 
f1 n 
    | n == 0 = 1
    | otherwise = 2^n + f1(n-1)   
    
f2 :: Integer -> Float -> Float
f2 n q 
    | n == 1 = q
    | otherwise = q^n + f2 (n-1) q
    
f3 :: Integer -> Float -> Float
f3 n q = f2 (2*n) q

f4 :: Integer -> Float -> Float
f4 n q = f2 (2*n) q - f2 (n-1) q
    
    
{- *************** raizDeDosAprox *++++++++++++++++++++ 

    Aproxima la raiz de 2 mediante a_n -1, donde a_1 = 2 y a_n = 2 + 1/a_{n-1}
    
    problema : raizDeDosAprox (n: Z) : Float
    requiere: {n>0}
    asegura: { |res[n+1] - sqrt(2)| < |res[n] -sqrt(2)|

-}
raizDeDosAprox :: Integer -> Float
raizDeDosAprox n = sucesion_a n -1
  where
    sucesion_a :: Integer -> Float
    sucesion_a 1 = 1
    sucesion_a n = 2 + 1/sucesion_a (n-1)
    
    
    
    
    
    
    
    
