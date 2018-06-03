import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashMap.Lazy as HM


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

graph = (Nls [
                (Vt "A" 0 0 [(Ed "B" 2), (Ed "C" 4)]),
                (Vt "B" 1 1 [(Ed "A" 2), (Ed "C" 2), (Ed "D" 3)]),
                (Vt "C" 2 2 [(Ed "A" 4), (Ed "B" 2), (Ed "D" 2)]),
                (Vt "D" 1 3 [(Ed "B" 3), (Ed "C" 2)]),
                (Vt "E" 0 0 [])
                ])


astarSearch :: Nodelist -> String -> String -> Double -> [String]
astarSearch (Nls nodes) start _ _ | (null(filter (\x -> name x == start) nodes))  = ["Wrong start vertex!"]
astarSearch (Nls nodes) _ goal _ | (null(filter (\x -> name x == goal) nodes)) = ["Wrong goal vertex!"]
astarSearch  (Nls node) start goal maxCost = ["Right"]

astar :: PQ.MinPQueue dk vtv -> Nls ->  HM.Map sk sk-> HM.Map sk dk-> Vertex -> Vertex -> HM.Map sk sk
astar frontier nodes came_from cost_so_far current goal | (name current) == (name goal = came_from
astar frontier nodes came_from cost_so_far current goal | PQ.null frontier = came_from
aster frontier nodes came_from cost_so_far current goal = if 
							 


heuristic :: Vertex -> Vertex -> Double
heuristic (Vt _ x1 y1 _) (Vt _ x2 y2 _) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)


