module ValGraph (
    ValGraph,
    valgraph,
    addVertice,
    addVertices,
    addEdge,
    addEdges
) where

import Set

type ValGraph a b = (Set a, Set (a, a, b))


-----------------------Construtor----------------------------------

valgraph::ValGraph a b
valgraph = (eset, eset)

-------------------------------------------------------------------

-----------------------AddVerticesOps------------------------------

addVertice::(Eq a, Ord a)=>a -> ValGraph a b -> ValGraph a b
addVertice x (y, z) = (add x y, z)

addVertices::(Eq a, Ord a)=>[a] -> ValGraph a b -> ValGraph a b
addVertices [] x = x
addVertices (x:xs) (y, z) = addVertices xs (addVertice x (y, z))  

-------------------------------------------------------------------

-----------------------AddEdgesOps---------------------------------

addEdge::(Eq a, Eq b, Ord a, Ord b)=>(a,a,b) -> ValGraph a b -> ValGraph a b
addEdge (m, n, o) (x, y) = if (exists m x) && (exists n x) then (x, add (m,n,o) y) else (x,y)

addEdges::(Eq a, Eq b, Ord a, Ord b)=>[(a,a,b)] -> ValGraph a b -> ValGraph a b
addEdges [] x = x
addEdges (x:xs) y = addEdges xs (addEdge x y)

-------------------------------------------------------------------

-----------------------TESTE---------------------------------------

g = addEdges [(1,2,10),(1,3,10),(2,4,10),(2,6,10),(3,2,10),(3,4,10),(3,6,10),(3,8,10),(4,5,10),(4,6,10),(5,7,10),(6,8,10),(6,9,10),(6,5,10),(6,9,10),(7,9,10),(8,7,10),(8,9,10)] $ addVertices [1,2,3,4,5,6,7,8,9] $ valgraph