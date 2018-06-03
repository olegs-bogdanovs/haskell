main = do
                              calcDensity

calcDensity :: IO()
calcDensity = do
                              putStrLn "Input height: "
                              height <- getArg
                              putStrLn "Input weight: "
                              weight <- getArg
                              putStrLn "Input depth: "
                              depth <- getArg
                              let density = height + weight + depth 
                              putStrLn "Density :"
                              print density 
                              

getArg :: IO Double
getArg = do 
        line <- getLine
        if (numOk line && (read line :: Double) > 0)
            then return (read line :: Double)
            else do
            putStrLn "Please input proper Double"
            getArg


numOk :: String -> Bool
numOk line = if (head line /= '.' && dotcount line <= 1  && (null $ filter(`notElem` ['0' .. '9'] ++ ['.']) line))
                               then True
                               else False

dotcount :: String -> Int
dotcount line = length $ filter(== '.') line
