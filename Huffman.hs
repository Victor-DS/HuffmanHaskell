module Huffman where

-- L = Letter, V = Frequency Value
data T = F [Char] Double | N [Char] Double T T | Empty
	deriving(Show)

--returns the node with each letter and its frequency.
letter x	|x=='a' = (F [x] 8.167)
			|x=='b' = 1.492

--Get the node with the lowest accumulated frequency.
lowest [] T = T
lowest ((F L1 V1):x) (F L2 V2)	| V1 < V2 = (lowest x (F L1 V1))
								| otherwise = (lowest x (F L2 V2))

--Remove node from list
remove OriginalList Node = removeAux OriginalList [] Node
removeAux [] NewList _ = NewList
removeAux ((F L1 V1):x) NewList (F L2 V2) 	| L1==L2 = removeAux x NewList (F L2 V2)
											| otherwise removeAux x (F L1 V1)++NewList (F L2 V2)

--COmbine nodes --Leafs + Leafs or Nodes + Nodes, etc
combine (F L1 V1) (F L2 V2)		| V1 < V2 = (N L1++L2 V1+V2 (F L1 V1) (F L2 V2))
								| otherwise = (N L1++L2 V1+V2 (F L2 V2) (F L1 V1))

--Is this letter in this Leaf/Node?
containsAux a [] = False
containsAux a (b:x)		| a==b = True
						| otherwise = containsAux a x

contains a (F c v) = containsAux a c
contains a (N c v _ _) = containsAux a c

--Generate Huffman Tree

--Merge Sort - Sorts a list of Nodes
mergesort [] = []
mergesort [x] = [x]
mergesort l = merge (mergesort left) (mergesort right)
			where left ... right ...
--Merge
--Split

