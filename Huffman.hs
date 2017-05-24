module Huffman where

-- Tree Data Type | L = Letter, V = Frequency Value - OK
data T = N [Char] Double T T | Empty
	deriving(Show)
instance Eq T where
    (N _ a _ _) == (N _ b _ _) = a == b
instance Ord T where
  (N _ x _ _) `compare` (N _ y _ _) = x `compare` y

--Sort a list of nodes - OK
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

--Combine nodes
combine (N c1 v1 tl1 tr1) (N c2 v2 tl2 tr2)		| v1 > v2 = (N (c1++c2) (v1+v2) (N c2 v2 tl2 tr2) (N c1 v1 tl1 tr1))
												| otherwise = (N (c1++c2) (v1+v2) (N c1 v1 tl1 tr1) (N c2 v2 tl2 tr2))

--Checks if the letter is in the list
contains a [] = False
contains a (b:x)		| a==b = True
						| otherwise = contains a x

--Get list size
sizeAux [] n = n
sizeAux (a:x) n = sizeAux x (n+1)
size list = sizeAux list 0

--Verify if you should continue down the tree or stop | x = letter, l = list
proceed x (N l _ _ _) 		| size l == 1 = False
							| otherwise = contains x l

--Generate Tree
gen (a:(b:x))		| x==[] = combine a b
					| otherwise = (gen (quicksort ((combine a b):x)))

--Generate Huffman Tree
huffmanTree = (gen (quicksort (alphabet "abcdefghijklmnopqrstuvwxyz" [])))

--Get the letter value based on the Huffman Tree
lValue l (N _ v tl tr)		| (proceed l tl) = "0"++(lValue l tl)
								| (proceed l tr) = "1"++(lValue l tr)
								| otherwise = []

--Huffman
huffAux [] _ = []
huffAux (a:x) (N l v tl tr)	= (lValue a (N l v tl tr))++(huffAux x (N l v tl tr))

huffman word = huffAux word huffmanTree



--Gets a node for each letter with its frequency value
letter x 			| x=='e' = (N [x] 12.702 Empty Empty)
					| x=='t' = (N [x] 9.056 Empty Empty)
					| x=='a' = (N [x] 8.167 Empty Empty)
					| x=='o' = (N [x] 7.507 Empty Empty)
					| x=='i' = (N [x] 6.966 Empty Empty)
					| x=='n' = (N [x] 6.749 Empty Empty)
					| x=='s' = (N [x] 6.327 Empty Empty)
					| x=='h' = (N [x] 6.094 Empty Empty)
					| x=='r' = (N [x] 5.987 Empty Empty)
					| x=='d' = (N [x] 4.253 Empty Empty)
					| x=='l' = (N [x] 4.025 Empty Empty)
					| x=='c' = (N [x] 2.782 Empty Empty)
					| x=='u' = (N [x] 2.758 Empty Empty)
					| x=='m' = (N [x] 2.406 Empty Empty)
					| x=='w' = (N [x] 2.360 Empty Empty)
					| x=='f' = (N [x] 2.228 Empty Empty)
					| x=='g' = (N [x] 2.015 Empty Empty)
					| x=='y' = (N [x] 1.974 Empty Empty)
					| x=='p' = (N [x] 1.929 Empty Empty)
					| x=='b' = (N [x] 1.492 Empty Empty)
					| x=='v' = (N [x] 0.978 Empty Empty)
					| x=='k' = (N [x] 0.772 Empty Empty)
					| x=='j' = (N [x] 0.153 Empty Empty)
					| x=='x' = (N [x] 0.150 Empty Empty)
					| x=='q' = (N [x] 0.095 Empty Empty)
					| otherwise = (N [x] 0.07 Empty Empty) --z

alphabet [] list = list
alphabet (a:x) list = alphabet x (letter a:list)
