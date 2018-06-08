-- Data types
-------------
data Edge = Ed String Double
data Vertex = Vt String Double Double [Edge]
data Path = Pt [String] Double
data Vertex' = Vtx Vertex Path Bool
data Nodelist = Nls [Vertex]
data Mainlist = Mls [Vertex'] Bool
-- Graph definitions
--------------------

graph1 = (Nls [
                (Vt "A" 0 0 [(Ed "B" 2), (Ed "C" 4)]),
                (Vt "B" 1 1 [(Ed "A" 2), (Ed "C" 2), (Ed "D" 3)]),
                (Vt "C" 2 2 [(Ed "A" 4), (Ed "B" 2), (Ed "D" 2)]),
                (Vt "D" 1 3 [(Ed "B" 3), (Ed "C" 2)]),
                (Vt "E" 0 0 [])
                ])

-- Main function
main :: IO ()
main = do
        putStrLn "Start point: "
        start <- getLine
        putStrLn "End point: "
        end <- getLine
        putStrLn "Max cost: "
        costStr <- getLine
	let cost = read costStr :: Double
	mapM_ putStr $ astar graph1 start end cost
	putStrLn ""


----------------
-- Returns a path
astar :: Nodelist -> String -> String -> Double -> [String]
astar (Nls nodes) start _ _ | (null(filter (samename start) nodes))  = ["Wrong start vertex!"]
astar (Nls nodes) _ goal _ | (null(filter (samename goal) nodes)) = ["Wrong goal vertex!"]
astar (Nls nodes) start goal maxcost = reverse (printpath (expansionblock (Nls nodes)
                (Mls [(Vtx (getvert nodes start) (Pt [start] 0) False)] True) (Vtx (getvert nodes goal) (Pt [] 0) False) maxcost) (getvert nodes goal))

-- Print functions
------------------
-- Turns a list into a path of vertices as a string
-- принимает список вершин и возвращает список имен вершин
printvertlist :: [Vertex'] -> [String]
printvertlist [] = []
-- при каждом рекурсивном вызове происход склека головы
printvertlist (v:vs) = (printvert v : printvertlist vs)

-- Prints a vertex name (ToString())
-- возвращает имя вершины Vertex' типа String
printvert :: Vertex' -> String
printvert (Vtx (Vt n _ _ _) _ _) = n

-- Prints a path
printpath :: Mainlist -> Vertex -> [String]
printpath m goal | notreached (Vtx goal (Pt [] 0) False) m = ["No path available!"]
printpath (Mls vs _) goal = getpath (getvert' vs (printvert (Vtx goal (Pt [] 0) False)))
getpath :: Vertex' -> [String]
getpath (Vtx _ (Pt p _) _) = p

-- Block function
-----------------
-- Block for expanding nodes, verifies against matches and updates as needed
expansionblock :: Nodelist -> Mainlist -> Vertex' -> Double -> Mainlist
expansionblock (Nls nodes) (Mls open changed) goal cost =
                let minv = (getminvert open goal)
                in if (isworthexpanding minv goal cost) && changed
                                then (expansionblock (Nls nodes) (addexpanded (Mls (markexpanded open minv) False) (expandall nodes minv)) goal (getnewcost open (printvert goal) cost))
                                else (Mls open False)

-- Main list related functions
------------------------------
-- Returns a vertex by its name
-- Возвращает вершину по имени
-- В агрумент функции передается список вершин и имя вершины
getvert :: [Vertex] -> String -> Vertex
-- Guard если имя вершины совпадает, то возвращает вершину
getvert ((Vt name x y e) : vs) name1 | name1 == name = (Vt name x y e)
-- иначе рекурсивный вызов с хвостом
getvert (_ : vs) name = getvert vs name

-- Возвращает вершину по именм
-- В аргумент функции передается список вершин и имя вершины
getvert' :: [Vertex'] -> String -> Vertex'
-- Guard, если имя вершиный совпадает, то возвращаем вершину
getvert' (v : vs) name | (printvert v) == name = v
-- иначе рекурсивный вызов с хвостом
getvert' (_ : vs) name = getvert' vs name

-- Removes a vertex by its name, returns a new list
-- Удаляет вершины из списка по ее имени
remvert :: [Vertex'] -> String -> [Vertex']
-- в filter передается функция, которая возвращает True, если имя вершины отличается. 
-- в итоге получаем список вершин, которые не содержат имя s
remvert vx s = filter (diffname' s) vx

-- Returns a vertex with minimal g + h
-- Находит вершину с минимальным путем до этой вершины + прямой путь до вершины назначения
-- Первый аргумент - список вершин
-- Второй аргумент - конечная вершина. (используется для нахождения длины прямого пути) 
getminvert :: [Vertex'] -> Vertex' -> Vertex'
getminvert [] ve = ve
-- если вершина была пройдена, функция рекурсивно вызывает себя с хвостом списка вершин
getminvert (v:vs) ve | not(notexpanded v) = getminvert vs ve
-- для нахождения максимального g + h используется функция foldl в аккамуляторе которой будет находится вершина
-- с самым меньшим g + h расстоянием
getminvert (v:vs) ve = foldl (\acc cur -> if (totalcost cur ve) < (totalcost acc ve) && (notexpanded cur)
then cur else acc) v vs

-- Возвращает новые вершины, до которых можно добраться из переданной вершины в аргументе
-- первый аргумент - список вершин
-- второй аргумент вершина, из которой нужно будет пройти до след. вершин
-- возвращает список вершин
expandall :: [Vertex] -> Vertex' -> [Vertex']
expandall vs (Vtx (Vt a b c e) p r) = map (expandone vs (Vtx (Vt a b c e) p r)) e

-- Возвращает новую пройденную вершину
-- принимает в аргументы список вершин, вершину, ребро, по которуму добираемся до новой вершины
-- возвращаем новую вершину 
expandone :: [Vertex] -> Vertex' -> Edge -> Vertex'
expandone vs (Vtx _ (Pt p x) _) (Ed n c) = (Vtx (getvert vs n) (Pt (n:p) (c + x)) False)

-- Udates list, setting present node to expanded
markexpanded :: [Vertex'] -> Vertex' -> [Vertex']
markexpanded (v:exp) v1 | (printvert v) == (printvert v1) = ((setexpanded v):exp)
markexpanded (e:exp) v = (e:(markexpanded exp v))

setexpanded :: Vertex' -> Vertex'
setexpanded (Vtx v p r) = (Vtx v p True)

-- Udates list, adding expanded nodes and checking distances
addexpanded :: Mainlist -> [Vertex'] -> Mainlist
addexpanded m [] = m
addexpanded m (e:exp) = addexpanded (cheaperpath m e) exp

-- Replace a vertex path by a cheaper path if available
cheaperpath ::  Mainlist -> Vertex' ->  Mainlist
cheaperpath (Mls [] _) e = (Mls [e] True)
cheaperpath (Mls (v:vx) b) e | (printvert e) == (printvert v) =
                if (getcheapest e v) then (Mls (e:vx) True) else (Mls (v:vx) b)
cheaperpath (Mls (v:vx) b) e = addtomainlist (cheaperpath (Mls vx b) e) v

-- Returns a Vertex' list from a Mainlist
addtomainlist :: Mainlist -> Vertex' ->  Mainlist
addtomainlist (Mls vs b) v = (Mls (v:vs) b)

prn :: Mainlist -> Bool
prn (Mls m b) = b

-- Name filter predicates
-------------------------
-- Returns True if string matches the Vertex' name
-- Возвращает True если строка совпадает с именет вершины Vertex'
-- аргументами принимает строку с именем, при помощи которой будет сравниваться
-- и саму вершину. Сравнивается при помощи Pattern Matching
samename :: String -> Vertex -> Bool
samename n (Vt name _ _ _) = if (n == name) then True else False



-- Returns True if string matches the Vertex' name
samename' :: String -> Vertex' -> Bool
samename' n1 n = if (n1 == (printvert n)) then True else False

-- Returns True if string does not match the Vertex name
diffname' :: String -> Vertex' -> Bool
diffname' a b = not (samename' a b)

-- Returns True if node has not been reached yet
notreached :: Vertex' -> Mainlist -> Bool
notreached v (Mls open _) = (null(filter (samename' (printvert v)) open))

-- Returns True if node has not been expanded yet
-- Возвращает True, если вершина еще не была пройдена
notexpanded :: Vertex' -> Bool
notexpanded (Vtx _ (Pt p _) b) = (not b)

-- Cost-related functions
-------------------------
-- Returns the value of g(current)
-- Возвращает расстояние до пройденной вершины Vertex'
cost :: Vertex' -> Double
cost (Vtx _ (Pt _ x) _) = x

-- Returns the value of h(current, goal)
-- Принимает две вершины Vertex' и расчитывает прямой путь от первой вершины, ко второй
heuri :: Vertex' -> Vertex' -> Double
heuri (Vtx (Vt _ x1 y1 _) _ _) (Vtx (Vt _ x2 y2 _) _ _) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- Returns g(current) + h(current, goal)
-- Принимает две вершины Vertex' возвращает сумму прямого пути до второй вершины
-- и сколько было пройдено пути до первой вершины
totalcost :: Vertex' -> Vertex' -> Double
totalcost a b = (cost a) + (heuri a b)

-- Returns true if the first vertex is cheaper than the second
getcheapest :: Vertex' -> Vertex' -> Bool
getcheapest (Vtx _ (Pt _ x) _) (Vtx _ (Pt _ y) _) = if x < y then True else False

-- Tests if the vertex is potentially worth expanding
isworthexpanding :: Vertex' -> Vertex' -> Double -> Bool
isworthexpanding a b c = if (totalcost a b) < c then True else False

-- Returns new cost after a cycle, leaves intact if no path found
getnewcost :: [Vertex'] -> String -> Double -> Double
getnewcost [] _ x = x
getnewcost ((Vtx (Vt name _ _ _) (Pt p x) _): vs) name1 oldcost | name1 == name && x < oldcost = getnewcost vs name1 x
getnewcost (_ : vs) vert oldcost = getnewcost vs vert oldcost
