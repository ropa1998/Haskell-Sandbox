myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast = last . init

elementAt :: [a] -> Int -> a
elementAt list i    = list !! (i-1)

myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = length xs + 1

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = reverse xs ++ [x]

palindrome :: (Eq a) => [a] -> Bool
palindrome [] = False
palindrome [x] = False
palindrome xs = xs == reverse xs



myButLastRopa [] = error "Empty list"
myButLastRopa [x] = error "Too few elements"
myButLastRopa (x:xs) =
		if length xs == 1 then x
		else myButLastRopa xs

main = do
   let intList = [1,2,3,4]
   let stringList = ["Hello", "World", "!"]
   let string = "Hello World!"

   print (myLast stringList)
   print (myLast intList)

   print (myButLast intList)
   print (myButLast stringList)

   print (myButLastRopa intList)
   print (myButLastRopa stringList)

   print (elementAt intList 1)
   print (elementAt stringList 1)

   print (myLength intList)
   print (myLength stringList)

   print(show(myReverse intList))
   print(show(myReverse stringList))

   let palindromeString = "xamax"
   let nonPalindromeString  = "hello"

   print (palindrome palindromeString)
   print (palindrome nonPalindromeString)



