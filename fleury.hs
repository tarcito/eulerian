--implementação do Algoritmo de Fleury com paradgma funcional

import Graph
import Set

eh_Digrafo :: (Eq a) => (Set a, Set (a, a)) -> Bool
eh_Digrafo ((Set xs) , (Set [])) = True
eh_Digrafo ((Set xs) , (Set (y:ys))) = ( exists (snd y, fst y) (Set ys)) && eh_Digrafo ((Set xs) , Set (filter ( /= (snd y, fst y)) ys))

eh_Conexo ::  (Eq a) => (Set a, Set (a, a)) -> Bool
eh_Conexo ((Set []), (Set ys)) = True
eh_Conexo ((Set (x:xs)), (Set ys)) = ((length (filter (\z -> (fst z == x)) ys)) > 0 ) &&  ((length (filter (\z -> (snd z == x)) ys)) > 0 ) && 
                                      eh_Conexo ((Set xs), (Set ys) )

-- To Do ...

-- eh_Euleriano ::  (Eq a) => (Set a, Set (a, a)) -> Bool
eh_Euleriano ((Set (x:xs)),(Set ys)) =  ((length (filter (\z -> fst z == x))) >= 2) &&  


