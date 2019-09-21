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

compress :: (Eq a) => [a] -> [a]
compress (x:ys@(y:_))               -- the @ tells me that ys must have at least one element and anything in the tail
    | x == y    = compress ys
    | otherwise = x : compress ys
compress ys = ys

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

--encode :: Eq a => [a] -> [(Int, a)]
--encode = map ((,) <$> length <*> head) . pack

encode xs = map (\x -> (length x,head x)) (pack xs)

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x

decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
    where
      decodeHelper (Single x)     = [x]
      decodeHelper (Multiple n x) = replicate n x

duplicate :: (Eq a) => [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x:x:duplicate xs




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

   let uncompressedString = "aaaaaaaabbbaaabbbbbbbsssssgjjjjj"

   print (compress uncompressedString)

   print (show(pack(uncompressedString)))

   print (show(encode(uncompressedString)))

   let encodeResult = encodeModified uncompressedString

   print(encodeResult)

   print(decodeModified encodeResult)

   print(duplicate intList)



