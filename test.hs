import qualified Data.PQueue as PQ


data Edge = Ed {
    vertexName :: String, 
    price      :: Double
}

data Vertex = Vt {
    name  :: String,
    x     :: Double,
    y     :: Double,
    edges :: [Edge]
}

data Path = Pt {
    vertexPath :: [String], 
    totalPrice :: Double
}

data Vertex' = Vtx {
    vertex    :: Vertex, 
    path      :: Path, 
    isVisited ::Bool
}

data Nodelist = Nls [Vertex]

graph1 = (Nls [
                (Vt "A" 0 0 [(Ed "B" 2), (Ed "C" 4)]),
                (Vt "B" 1 1 [(Ed "A" 2), (Ed "C" 2), (Ed "D" 3)]),
                (Vt "C" 2 2 [(Ed "A" 4), (Ed "B" 2), (Ed "D" 2)]),
                (Vt "D" 1 3 [(Ed "B" 3), (Ed "C" 2)]),
                (Vt "E" 0 0 [])
                ])


astar :: Nodelist -> String -> String -> Double -> [String]
astar (Nls nodes) start _ _ | (null(filter (\x -> name x == start) nodes))  = ["Wrong start vertex!"]
astar (Nls nodes) _ goal _ | (null(filter (\x -> name x == goal) nodes)) = ["Wrong goal vertex!"]
astar (Nls node) start goal maxCost = ["Right"]

