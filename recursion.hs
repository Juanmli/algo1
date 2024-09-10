fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

parteEntera :: Float -> Integer
parteEntera x   | x < 1 = 0
		| otherwise = 1 + parteEntera(x-1)
		
	 	
