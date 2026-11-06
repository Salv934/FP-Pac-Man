-- maze/level layout
module Maze where 

import Graphics.Gloss (Point)

data Tile = Wall | Floor | Pellet | PowerPellet deriving (Eq, Show)

type Cell = (Int, Int)
type Maze = [[Tile]]

fieldWidth, fieldHeight :: Int 
fieldWidth = 25
fieldHeight = 23
tileSize :: Float
tileSize = 22

pacmanStart :: Point
pacmanStart = tileCenter (12, 19)

tileCenter :: Cell -> Point
tileCenter (x,y)
    = (fromIntegral (x - fieldWidth `div` 2)* tileSize
      , fromIntegral (fieldHeight`div` 2 - y)* tileSize)

pointCell :: Point -> Cell
pointCell(x,y) = 
    (round (x/ tileSize)+ fieldWidth `div` 2, fieldHeight `div` 2 - round (y/tileSize))

inBounds :: Cell -> Bool
inBounds (x, y) = x >= 0 && y >= 0 && x < fieldWidth && y < fieldHeight

tileAt :: Maze -> Cell -> Tile 
tileAt field (x, y) | inBounds (x,y) = (field !! y) !! x | otherwise = Wall

isOpen :: Maze -> Cell -> Bool
isOpen field cell = tileAt field cell /= Wall

allTiles :: Maze -> [(Cell, Tile) ]
allTiles field = 
    [((x,y) , tile) | (y, row) <- zip [0..] field, (x, tile) <- zip [0..] row]

changeTile :: Maze -> Cell -> Tile -> Maze
changeTile field (cx, cy) newTile =
    [ if y == cy
        then
            [if x == cx then newTile else tile| (x,tile) <- zip [0..] row]
        else row | (y, row) <- zip [0..] field]

remainingPellets :: Maze -> Int 
remainingPellets field = 
    length [()| (_, tile) <- allTiles field, tile == Pellet || tile == PowerPellet]

collectPellet :: Maze -> Point -> (Maze, Int, Bool)
collectPellet field pos = 
    let cell = pointCell pos
    in case tileAt field cell of
        Pellet -> (changeTile field cell Floor, 10, False)
        PowerPellet -> (changeTile field cell Floor, 50, True)
        _ -> (field, 0, False)

nextGhostCell :: Maze -> Cell -> Cell -> Cell
nextGhostCell field start requested | not (isOpen field start) = start 
    | otherwise = search [(start, start)] [start]
  where
    neighbors (x, y) =
        [ (x + 1, y)
        , (x - 1, y)
        , (x, y + 1)
        , (x, y - 1)
        ]
    freeNeighbors c = filter (isOpen field) (neighbors c)
    dist (x, y) (a, b) = abs (x - a) + abs (y - b)
    target = if isOpen field requested then requested
        else fst $
            minimumByDistance
                [ (c, dist c requested)| (c, tile) <- allTiles field 
                , tile /= Wall]
    minimumByDistance (v:vs) =
        foldl
            (\best@(_, bd) candidate@(_, cd) ->
                if cd < bd then candidate else best)
            v
            vs
    minimumByDistance [] = (start, 0)
    search [] _ = start
    search ((cur, first):rest) visited | cur == target = first
        | otherwise =
            let new =
                    filter (`notElem` visited)
                        (freeNeighbors cur)
                queued =
                    [ (n, if cur == start then n else first) | n <- new]
            in search
                (rest ++ queued)
                (visited ++ new)

mazeLevel :: Int -> Maze
mazeLevel n =
    map (map toTile) layout
  where
    layout =
        case ((n - 1) `mod` 3) + 1 of
            1 -> levelOne
            2 -> levelTwo
            _ -> levelThree

    toTile c =
        case c of
            '#' -> Wall
            '.' -> Pellet
            'o' -> PowerPellet
            _   -> Floor

-- # = wall
-- . = pellet
-- o = power pellet
-- S = Pac-Man start location
-- Space = empty floor

levelOne :: [String]
levelOne =
    [ "#########################"
    , "#o.....................o#"
    , "#.......................#"
    , "#..###..##.....##..###..#"
    , "#..###..##.....##..###..#"
    , "#..###..##.....##..###..#"
    , "#..###..##.....##..###..#"
    , "#.......................#"
    , "#..###.#####.#####.###..#"
    , "#.......................#"
    , "#.........#   #.........#"
    , "#..###..###   ###..###..#"
    , "#..###..###   ###..###..#"
    , "#..###..#########..###..#"
    , "#..###..##.....##..###..#"
    , "#.......................#"
    , "#..###.#####.#####.###..#"
    , "#.......................#"
    , "#..###..##.....##..###..#"
    , "#..###..##..S..##..###..#"
    , "#.......................#"
    , "#o.....................o#"
    , "#########################"
    ]

levelTwo :: [String]
levelTwo =
    [ "#########################"
    , "#o.....................o#"
    , "#.......................#"
    , "#..###..##.....##..###..#"
    , "#..###..##.....##..###..#"
    , "#..###..##.#.#.##..###..#"
    , "#..###..##.....##..###..#"
    , "#.......................#"
    , "#..####.####.####.####..#"
    , "#.......................#"
    , "#.........#   #.........#"
    , "#..###....#   ###..###..#"
    , "#..###....#   ###..###..#"
    , "#..###....#######..###..#"
    , "#..###.........##..###..#"
    , "#.......................#"
    , "#..####.####.####.####..#"
    , "#.......................#"
    , "#..###..##.....##..###..#"
    , "#..###..##..S..##..###..#"
    , "#.......................#"
    , "#o.....................o#"
    , "#########################"
    ]

levelThree :: [String]
levelThree =
    [ "#########################"
    , "#o.....................o#"
    , "#.......................#"
    , "#..###..##.....##..###..#"
    , "#..###..##.....##..###..#"
    , "#..###..##.....##..###..#"
    , "#..###..##.....##..###..#"
    , "#.......................#"
    , "#..##.######.######.##..#"
    , "#.......................#"
    , "#.........#   #.........#"
    , "#..###..###   ###..###..#"
    , "#..###..###   ###..###..#"
    , "#..###..#########..###..#"
    , "#..###..##.....##..###..#"
    , "#.......................#"
    , "#..##.######.######.##..#"
    , "#.......................#"
    , "#..###..##.....##.......#"
    , "#..###..##..S..##.......#"
    , "#......##.##.##.##......#"
    , "#o.....................o#"
    , "#########################"
    ]