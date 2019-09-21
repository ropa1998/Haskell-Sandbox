myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs

main = do
   let intList = [1,2,3,4]
   print (myLast intList)
   let stringList = ["Hola", "Mundo", "!"]
   print (myLast stringList)

