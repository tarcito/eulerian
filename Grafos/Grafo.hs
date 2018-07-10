module Grafos.Grafo (
   Grafo,
   grafo,
   addVertice,
   addVertices,
   addEdge,
   addEdges
    ) where

import Graph


type Grafo a = Graph a

grafo::Grafo a
grafo = graph 


addVertice = Graph.addVertice

addEdge = Graph.addEdge

removeEdge =  Graph.removeEdge

addVertices = Graph.addVertices

addEdges = Graph.addEdges

removeEdges = Graph.removeEdges
