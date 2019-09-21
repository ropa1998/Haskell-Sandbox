myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast = last . init

myButLastRopa [] = error "Empty list"
myButLastRopa [x] = error "Too few elements"
myButLastRopa (x:xs) =
		if length xs == 1 then x
		else myButLastRopa xs

main = do
   let intList = [1,2,3,4]
   print (myLast intList)
   let stringList = ["Hola", "Mundo", "!"]
   print (myLast stringList)

   print (myButLast intList)
   print (myButLast stringList)

   print (myButLastRopa intList)
   print (myButLastRopa stringList)

