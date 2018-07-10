data Vertice v [e] = Vertice v | Vertice v [e]

data Grafo (Vertice v) = [] | Vertice v : (Grafo (Vertice v)) deriving (Eq,Show)
