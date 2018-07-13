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


eh_Euleriano ::  (Eq a) => (Set a, Set (a, a)) -> Bool
eh_Euleriano ((Set []), (Set ys)) = True
eh_Euleriano ((Set (x:xs)),(Set ys)) =  ((length (filter (\z -> fst z == x) ys)) >= 2) &&  ((length (filter (\z -> snd z == x) ys)) >= 2) && 
              eh_Euleriano ((Set xs), (Set ys))


-- To Do ...

--eulerian_Path :: (Eq a) => (Set a, Set (a, a)) -> Set a
eulerian_Path ((Set []), (Set [])) = Set []
eulerian_Path ((Set (x:xs)), (Set (y:ys))) = 




-- Exemplo Grafo euleriano simples
g = addEdges [(1,2),(2,1),(1,5),(5,1),(2,3),(3,2),(3,4),(4,3),(4,5),(5,4)] (addVertices [1,2,3,4,5] graph)
