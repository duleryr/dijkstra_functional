unicos [] = []
unicos (x:xs) = x : (unicos (filter (/=x) xs))

grafo::  [(Int,Int,Int)]
grafo = [(1,2,10),(1,3,20),(2,4,15),(4,3,10),(3,5,5),(1,5,30),(5,4,60),(5,6,10),(2,6,40)]

grafo1::  [(Int,Int,Int)]
grafo1 = [(1,2,10),(1,3,100),(2,4,50),(4,3,10),(1,5,30),(5,4,60),(5,3,60)]

grafo2::  [(Int,Int,Int)]
grafo2 = [(1,2,2),(1,4,1),(2,3,3),(2,5,10),(3,1,4),(3,4,5),(4,3,2),(4,5,2),(4,6,8),(4,7,4),(5,7,6),(7,6,1)]

prim (a,b,c) = a
segu (a,b,c) = b
terc (a,b,c) = c

origen::(Int,Int,Int) -> Int
origen (a,b,c) = a

destino::(Int,Int,Int) -> Int
destino (a,b,c) = b

costo::(Int,Int,Int) -> Int
costo (a,b,c) = c

vertices:: [(Int,Int,Int)] -> [Int]
vertices [] = []
vertices (x:xs) = unicos $ (origen x) : ((destino x) : vertices xs)

sacar x = filter (/= x)

computable::[Int] -> [Int] -> (Int,Int,Int) -> Bool
computable cs ncs a = (elem (origen a) cs) && (elem (destino a) ncs)

computables::[Int] -> [Int] -> [(Int,Int,Int)] -> [(Int,Int,Int)]
computables cs ncs as = filter (computable cs ncs) as

costoini::Int -> [Int] -> [(Int,Int,Int)]
costoini a [] = []
costoini a (x:xs) 
    | x ==a     = (a,0,a):(costoini a xs)
    | otherwise = (x,1000,-1):(costoini a xs)

getcosto::[(Int,Int,Int)] -> Int -> Int
getcosto [] x = error "no existe el costo"
getcosto (x:xs) o 
        | origen x == o = destino x
        | otherwise = getcosto xs o

difver (a,b,b1) (c,d,d1) = a /= c

updatecos:: [(Int,Int,Int)] -> (Int,Int,Int) -> [(Int,Int,Int)]
updatecos xs (a,b,c) 
         | ((getcosto xs a) + c) < (getcosto xs b) = (b,(getcosto xs a) + c,a):(filter (difver (b,0,0) ) xs) 
         | otherwise  =  xs
         
updatetodos:: [(Int,Int,Int)] -> [(Int,Int,Int)] -> [(Int,Int,Int)]
updatetodos cost [] = cost
updatetodos cost (c:cmpbls) = updatetodos (updatecos cost c) cmpbls

menordista (a,b,b1) (c,d,d1) = if (b < d ) then (a,b,b1) else (c,d,d1)

getnewbest:: [(Int,Int,Int)] -> [Int] -> Int
getnewbest cost vnc = elmenor $ filter (\x -> elem (prim x) vnc) cost 
    where elmenor (a:as) = prim $ foldl menordista a as


dijkstra:: [(Int,Int,Int)] -> Int -> [(Int,Int,Int)]
dijkstra as ini = dijaux as ( (ini:[]), (sacar ini $ vertices as), (costoini ini $ vertices as))

dijaux:: [(Int,Int,Int)] -> ([Int], [Int], [(Int,Int,Int)]) ->  [(Int,Int,Int)]
dijaux as (vcs,[],csts) = csts
dijaux as (vcs,vncs,csts) = dijaux as ((m:vcs), (sacar m vncs), nuevocsts )
    where nuevocsts = updatetodos csts (computables vcs vncs as)
          m = getnewbest nuevocsts vncs
