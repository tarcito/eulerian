{-
  implementação de algoritmo que checa a existencia de grafo euleriano ou semi-euleriano, com paradgma funcional (Haskell)
  authors: Alekssandro e Tárcito
-}

import Graph
import Set

-- verifica se o grafo é digrafo (para cada cada vertice esta ligado em ambas direções)
eh_Digrafo :: Eq a => (Set a, Set (a, a)) -> Bool
eh_Digrafo ((Set xs) , (Set [])) = True
eh_Digrafo ((Set xs) , (Set (y:ys))) = ( exists (snd y, fst y) (Set ys)) && eh_Digrafo ((Set xs) , Set (filter ( /= (snd y, fst y)) ys))

-- verifica se o os vertices estão conectados a outros vertices (se o numero de aresta de cada vertice é maior que 0)
eh_Conexo :: Eq a => (Set a, Set (a, a)) -> Bool
eh_Conexo ((Set []), (Set ys)) = True
eh_Conexo ((Set (x:xs)), (Set ys)) = (((length (filter (\z -> (fst z == x)) ys)) > 0 ) ||  ((length (filter (\z -> (snd z == x)) ys)) > 0 )) && 
                                      eh_Conexo ((Set xs), (Set ys) )

-- verifica se uma aresta é ponte (se ao retirar a aresta, o grafo fica desconexo)
eh_Ponte :: Eq a => (a, a) -> (Set a, Set (a, a)) -> Bool 
eh_Ponte (a, b) ((Set xs), (Set ys)) = not (eh_Conexo ((Set xs), (Set (filter (\z -> (z /= (b, a)) || (z /= (a, b))) ys))) )

-- verifica se no grafo de entrada contém um caminho euleriano (tem exatamente dois vertices com grau 1, e todos outros com grau par)
eh_Semi_Euleriano :: Eq a => (Set a, Set (a, a)) -> Bool
eh_Semi_Euleriano ((Set xs), (Set ys)) = eh_Semi_Euleriano' 0 [] ((Set xs),(Set ys))

-- função auxiliar que verifica se existe um caminho euleriano, contando os vertices de grau 1 e ao mesmo tempo formando a lista com os vertices que possivelmente contém um caminho euleriano
eh_Semi_Euleriano' z zs ((Set []), (Set ys)) = (z == 2) && eh_Euleriano ((Set zs), (Set ys))
eh_Semi_Euleriano' z zs ((Set (x:xs)),(Set ys)) =  if( ((length (filter (\z -> fst z == x) ys)) == 1) ||  ((length (filter (\z -> snd z == x) ys)) == 1) ) then eh_Semi_Euleriano' (z+1) zs ((Set xs), (Set ys))
              else eh_Semi_Euleriano' z (zs++[x]) ((Set xs), (Set ys))

-- verifica se o grafo de entrada é um circuito euleriano (todos vertices tiverem grau par)
eh_Euleriano ::  Eq a => (Set a, Set (a, a)) -> Bool
eh_Euleriano ((Set []), (Set ys)) = True
eh_Euleriano ((Set (x:xs)),(Set ys)) =  ((mod (length (filter (\z -> fst z == x) ys)) 2 == 0) || (mod (length (filter (\z -> snd z == x) ys)) 2 == 0)) && 
              eh_Euleriano ((Set xs), (Set ys))

-- retorna uma lista de tuplas no formato (vertice, grau)
grau_Vertices :: Eq a => (Set a, Set (a, a)) -> [(a, Int)]
grau_Vertices ((Set []), (Set ys)) = []
grau_Vertices ((Set (x:xs)),(Set ys)) = (x, (length (filter (\z -> fst z == x) ys)) ) : grau_Vertices ((Set xs),(Set ys)) 

{--
-- To Do ...
depth_First_Search ((Set []), (Set ys)) = [] 
depth_First_Search ((Set (x:xs)), (Set ys)) = depth_First_Search((Set d_F_S x (filter  ys), (Set ys))

d_F_S v (y:ys) = v: d_F_S filter  ys

-- To Do ...
eulerian_Path :: Eq a => (Set a, Set (a, a)) -> [a]
eulerian_Path grafo |((eh_Conexo grafo) && (eh_Digrafo grafo) && (eh_Euleriano grafo)) = eulerian_Path' grafo
                    |otherwise = error "Grafo não euleriano"

eulerian_Path' ((Set []), (Set [])) = []
eulerian_Path' ((Set xs), (Set [])) = []
eulerian_Path' ((Set (x:xs)), (Set ys)) |((length (arestas x ys)) > 1) = Nothing
   where arestas  v as = filter (\y -> fst y == v) as
         all_Aresta v as = (arestas v as) ++ (filter (\y -> snd y == v) as) 
--}

-- Exemplo Grafo euleriano simples
g1 = addEdges [(1,2),(2,1),(1,5),(5,1),(2,3),(3,2),(3,4),(4,3),(4,5),(5,4)] (addVertices [1,2,3,4,5] graph)

-- Exemplo Grafo semi-euleriano simples
g2 = addEdges [(1,2),(1,3),(2,1),(1,5),(5,1),(1,6),(6,1),(2,3),(3,1),(3,2),(3,4),(4,3),(3,7),(7,3),(4,5),(5,4)] (addVertices [1,2,3,4,5,6,7] graph)
