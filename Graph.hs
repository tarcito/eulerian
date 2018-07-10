module Graph (
   Graph,
   graph,
   addVertice,
   addVertices,
   addEdge,
   addEdges
    ) where

import Set

type Graph a = (Set a, Set (a, a))

graph::Graph a
graph = (eset, eset)

addVertice::(Eq a, Ord a)=>a->Graph a->Graph a
addVertice x (n, y) = (add x n, y)

addEdge::(Eq a, Ord a)=>(a,a)->Graph a->Graph a
addEdge (x, v) (n, y) = if exists x n && exists v n then (n, add (x,v) y) else (n,y)

removeEdge::(Eq a)=>(a,a)->Graph a->Graph a
removeEdge x (n, e) = (n, remove x e) 

addVertices::(Eq a, Ord a)=>[a]->Graph a->Graph a
addVertices [] (x,y) = (x,y)
addVertices (x:xs) (n, y) = addVertices xs (addVertice x (n,y))

addEdges::(Eq a, Ord a)=>[(a,a)]->Graph a->Graph a
addEdges [] (x,y) = (x,y)
addEdges (x:xs) (n, v) = addEdges xs (addEdge x (n,v))

removeEdges::(Eq a, Ord a)=>[(a,a)]->Graph a->Graph a
removeEdges [] (x,y) = (x,y)
removeEdges (x:xs) (n, v) = removeEdges xs (removeEdge x (n,v))

sEntrada2::(Eq a, Ord a)=>Graph a->Set a
sEntrada2 (Set x, Set []) = Set []
sEntrada2 (Set [], Set x) = Set []
sEntrada2 (Set (x:xs), Set (y:ys)) = if exists (snd y) (Set (x:xs)) then add (snd y) (sEntrada2 (Set (x:xs), Set ys)) else sEntrada2 (Set (x:xs), Set ys)

sEntrada::(Eq a, Ord a)=>Graph a->Set a
sEntrada x = dif (fst x) (sEntrada2 x)

ord::(Eq a, Ord a)=>Graph a->[a]
ord x = ord2 x (sEntrada x) where
ord2::(Eq a, Ord a)=>Graph a->Set a->[a]
ord2 x (Set []) = []
ord2 x (Set (y:ys)) = [y]++(ord2 (removeEdges (to x y) x) (remove y (ord3 x (to x y) (Set (y:ys)))))

to::(Eq a)=>Graph a->a->[(a,a)]
to (x, Set []) y = []
to (x, Set (y:ys)) v = if v==(fst y) then [y]++(to (x, Set ys) v) else to (x, Set ys) v

ord3::(Eq a, Ord a)=>Graph a->[(a,a)]->Set a->Set a
ord3 x [] v = v
ord3 (x, v) (y:ys) k = if exists (snd y) (sEntrada (removeEdge y (x, v))) then (ord3 (removeEdge y (x, v)) ys (addr (snd y) k)) else ord3 (removeEdge y (x, v)) ys k


g = addEdges [(1,2),(1,3),(2,4),(2,6),(3,2),(3,4),(3,6),(3,8),(4,5),(4,6),(5,7),(6,8),(6,9),(6,5),(6,9),(7,9),(8,7),(8,9)] $ addVertices [1,2,3,4,5,6,7,8,9] $ graph




