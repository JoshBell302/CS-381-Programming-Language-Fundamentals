-- Homework 1

import Data.List (nub,sort)
norm :: Ord a => [a] -> [a]
norm = sort . nub
type Bag a = [(a,Int)]
type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]
type Number = Int
type Point = (Number,Number)
type Length = Number
data Shape = Pt Point
    | Circle Point Length
    | Rect Point Length Length
    deriving Show
type Figure = [Shape]
type BBox = (Point,Point)

-- Exercise 1. Programming with Lists
-- (a): Define the function ins that inserts an element into a multiset.

ins :: Eq a => a -> Bag a -> Bag a
ins n [] = (n,1):[]
ins n (x:xs) 
    | n == fst x = (fst x, snd x + 1) : xs 
    | otherwise = x : ins n xs


-- (b): Define the function del that removes an element from a multiset.

checkDel :: (a, Int) -> Bag a -> Bag a
checkDel (x,y) z = 
    if (y >=1)
        then (x,y) : z
        else z

del :: Eq a => a -> Bag a -> Bag a
del n [] = []
del n (x:xs)
    | n == fst x = checkDel (fst x, snd x - 1) xs
    | otherwise = x : del n xs


-- (c): Define a function bag that takes a list of values and produces a multiset representation.

bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x (bag xs)


-- (d): Define a function subbag that determines whether or not its first argument bag is contained in the second.

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] [] = True
subbag (x:xs) (y:ys) = x == y && subbag xs ys
subbag _ _ = False


-- (e): Define a function isbag that computes the intersection of two multisets.

valueCheck :: (Int, Int) -> Int
valueCheck (x, y) =
    if (x == y)
        then x
        else abs (x - y)

isCheck :: Eq a => (a,Int) -> Bag a ->  Bag a
isCheck x [] = []
isCheck x (y:ys) = 
    if (fst x == fst y)
        then (fst x, (valueCheck (snd x, snd y))) : isCheck x ys
        else isCheck x ys

isbag :: Eq a => Bag a -> Bag a -> Bag a
isbag [] a = []
isbag (x:xs) (y:ys) = isCheck x (y:ys) ++ isbag xs (y:ys)


-- (f): Define a function size that computes the number of elements contained in a bag.

size :: Bag a -> Int
size [] = 0
size (x:xs) = snd x + size xs



-- Exercise 2. Graphs
-- (a): Define the function nodes :: Graph -> [Node] that computes the list of nodes contained in a given graph.

nodes :: Graph -> [Node]
nodes [] = []
nodes (x:xs) = nub (fst x:nodes xs)


-- (b): Define the function suc :: Node -> Graph -> [Node] that computes the list of successors for a node in agiven graph. 

suc :: Node -> Graph -> [Node]
suc n [] = []
suc n (x:xs)
    | n == fst x = snd x : suc n xs
    | otherwise = suc n xs


-- (c): Define the function detach :: Node -> Graph -> Graph that removes a node together with all of its incident edges from a graph.  

detach :: Node -> Graph -> Graph
detach n [] = []
detach n (x:xs)
    | n == fst x = detach n xs
    | n == snd x = detach n xs
    | otherwise = x : detach n xs


-- (d): Define the function cyc :: Int -> Graph that creates a cycle of any given number. 

cycCheck :: Int -> Int -> Edge
cycCheck x y =
    if (x == y)
        then (y,1)
        else (y, y+1)

cycEdge :: Int -> Int -> Graph
cycEdge x y =
    if (x == y)
        then []
        else cycCheck x (y+1) : cycEdge x (y+1)

cyc :: Int -> Graph
cyc x = cycEdge x 0


-- Exercise 3. Programming with Data Types
-- (a): Define the function width that computes the width of a shape.

width :: Shape -> Length
width (Pt _) = 0
width (Circle _ r) = 2 * r
width (Rect _ l _) = l


-- (b): Define the function bbox that computes the bounding box of a shape.

bbox  :: Shape -> BBox
bbox (Pt x) = (x,x)
bbox (Circle y r) = ((fst y - r, snd y - r), (fst y + r, snd y + r))
bbox (Rect z l w) = (z, (fst z + l, snd z + w))


-- (c)Define the function minX that computes the minimum x coordinate of a shape.

minX :: Shape -> Number
minX (Pt x) = fst x
minX (Circle y r) = fst y - r
minX (Rect z l w) = fst z


-- (d): Define a function move that moves the position of a shape by a vector given by a point as its second argument.

addPt :: Point -> Point -> Point
addPt (a, b) (c, d) = (a + c, b + d)

move :: Shape -> Point -> Shape
move (Pt x) p = Pt (addPt x p)
move (Circle y r) p = Circle (addPt y p) r
move (Rect z l w) p = Rect (addPt z p) l w


-- (e): Define a function alignLeft that transforms one figure into another one in which all shapes have the same minX coordinate but are otherwise unchanged.

findMinX :: Figure -> Number
findMinX x = minimum (map minX x)

fixShape :: Shape -> Int -> Shape
fixShape (Pt x) m = Pt (m, snd x)
fixShape (Circle y r) m = Circle (m+r, snd y) r
fixShape (Rect z l w) m = Rect (m, snd z) l w

createFigure :: Int -> Figure -> Figure
createFigure m [] = []
createFigure m (x:xs) = fixShape x m : createFigure m xs

alignLeft :: Figure -> Figure
alignLeft x = createFigure (findMinX (x)) x


-- (f): Define a function inside that checks whether one shape is inside of another one, that is, whether the area covered by the first shape is also covered by the second shape.

rectForm :: Point -> Length -> Length -> [Point]
rectForm _ (-1) _ = []
rectForm p l w = ((fst p + l), (snd p + w)) : rectForm p (l - 1) w

circleFormL :: Point -> Length -> Int -> Int -> [Point]
circleFormL _ _ 0 _ = []
circleFormL p r a t = ((fst p - r), snd p) :  circleFormL (fst p, (snd p - 1)) r (a-1) t

circleFormLR :: Point -> Length -> Int -> Int -> [Point]
circleFormLR _ _ _ 0 = []
circleFormLR p r a t = circleFormL p r a t ++ circleFormLR ((fst p + 1), (snd p + 1)) r (a+2) (t-1)

circleFormR :: Point -> Length -> Int -> Int -> [Point]
circleFormR _ _ 0 _ = []
circleFormR p r a t = ((fst p + r), snd p) :  circleFormR (fst p, (snd p - 1)) r (a-1) t

circleFormRR :: Point -> Length -> Int -> Int -> [Point]
circleFormRR _ _ _ 0 = []
circleFormRR p r a t = circleFormR p r a t ++ circleFormRR ((fst p - 1), (snd p + 1)) r (a+2) (t-1)

circleFormM :: Point -> Length -> Int -> [Point]
circleFormM _ _ 0 = []
circleFormM p r a = (fst p, (snd p + r)) : circleFormM p (r-1) (a-1) 

circleForm :: Point -> Length -> Int -> Int -> [Point]
circleForm p r a t = circleFormLR p r a t ++ circleFormM p r (((2*r) + 1)) ++ circleFormRR p r a t

createPointList :: Shape -> Int -> Int -> [Point]
createPointList (Pt x) t a = [x]
createPointList (Circle p r) a t =  circleForm p r a t
createPointList (Rect _ _ (-1)) t a = []
createPointList (Rect z l w) t a = rectForm z l w ++ createPointList (Rect z l (w-1)) t a

insideCheck :: [Point] -> [Point] -> Bool
insideCheck [] _ = False
insideCheck (x:xs) y = 
    if (x `elem` y)
        then True
        else insideCheck xs y

inside :: Shape -> Shape -> Bool
inside (Rect x l w) (Rect y a b) = insideCheck (createPointList (Rect x l w) 1 1) (createPointList (Rect y a b) 1 1)
inside (Rect x l w) (Circle y r) = insideCheck (createPointList (Rect x l w) 1 1) (createPointList (Circle y r) 1 r)
inside (Rect x l w) (Pt y) = insideCheck (createPointList (Rect x l w) 1 1) (createPointList (Pt y) 1 1)
inside (Circle x r) (Rect y l w) = insideCheck (createPointList (Circle x r) 1 r) (createPointList (Rect y l w) 1 1)
inside (Circle x r) (Circle y a) = insideCheck (createPointList (Circle x r) 1 r) (createPointList (Circle y a) 1 r)
inside (Circle x r) (Pt y) = insideCheck (createPointList (Circle x r) 1 r) (createPointList (Pt y) 1 1)
inside (Pt x) (Rect y l w) = x `elem` createPointList (Rect y l w) 1 1
inside (Pt x) (Circle y r) = x `elem` createPointList (Circle y r) 1 r
inside (Pt x) (Pt y) = x `elem` createPointList (Pt y) 1 1