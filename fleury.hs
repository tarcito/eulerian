--implementação do Algoritmo de Fleury com paradgma funcional

import Graph
import Set


eh_Digrafo :: (Eq a) => (Set a, Set (a, a)) -> Bool
eh_Digrafo ((Set xs) , (Set [])) = True
eh_Digrafo ((Set xs) , (Set (y:ys))) = ( exists (snd y, fst y) (Set ys)) && eh_Digrafo ((Set xs) , Set (filter ( /= (snd y, fst y)) ys))


eh_Conexo ::  (Eq a) => (Set a, Set (a, a)) -> Bool
eh_Conexo ((Set []), (Set ys)) = True
eh_Conexo ((Set (x:xs)), (Set ys)) = (((length (filter (\z -> (fst z == x)) ys)) > 0 ) ||  ((length (filter (\z -> (snd z == x)) ys)) > 0 )) && 
                                      eh_Conexo ((Set xs), (Set ys) )


eh_Ponte :: Eq a => (a, a) -> (Set a, Set (a, a)) -> Bool 
eh_Ponte (a, b) ((Set xs), (Set ys)) = not (eh_Conexo ((Set xs), (Set (filter (\z -> (z /= (b, a)) || (z /= (a, b))) ys))) )


--eh_Euleriano ::  (Eq a) => (Set a, Set (a, a)) -> Bool
eh_Euleriano ((Set []), (Set ys)) = True
eh_Euleriano ((Set (x:xs)),(Set ys)) =  (mod (length (filter (\z -> fst z == x) ys)) 2 == 0) &&  (mod (length (filter (\z -> snd z == x) ys)) 2 == 0) && 
              eh_Euleriano ((Set xs), (Set ys))


{-- To Do ...
depth_First_Search ((Set []), (Set ys)) = [] 
depth_First_Search ((Set (x:xs)), (Set ys)) = depth_First_Search((Set d_F_S x (filter  ys), (Set ys))

d_F_S v (y:ys) = v: d_F_S filter  ys
--}


-- To Do ...
eulerian_Path :: (Eq a) => (Set a, Set (a, a)) -> [a]
eulerian_Path grafo |((eh_Conexo grafo) && (eh_Digrafo grafo) && (eh_Euleriano grafo)) = eulerian_Path' grafo
                    |otherwise = error "Grafo não euleriano"

eulerian_Path' ((Set []), (Set [])) = []
eulerian_Path' ((Set xs), (Set [])) = []
--eulerian_Path' ((Set (x:xs)), (Set (y:ys))) = Nothing



-- Exemplo Grafo euleriano simples
g = addEdges [(1,2),(2,1),(1,5),(5,1),(2,3),(3,2),(3,4),(4,3),(4,5),(5,4)] (addVertices [1,2,3,4,5] graph)

