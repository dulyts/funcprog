module TicTacToe where
    import Data.Maybe
    import Data.List

    data Player = PlayerX | PlayerO deriving (Eq)
    type Cell = Maybe Player
    data State = Running | GameOver (Maybe Player)
    data Board = Board [[Cell]]
    data Game = Game Board Player State

    gameBoard :: Game -> Board
    gameBoard (Game board _ _) = board

    gamePlayer :: Game -> Player
    gamePlayer (Game _ player _) = player

    gameState :: Game -> State
    gameState (Game _ _ state) = state

    size :: Int
    size = 3

    initGame :: Game
    initGame = Game (Board [[(Nothing :: Maybe Player) | cell <- [1..size]] | row <- [1..size]]) PlayerX Running

    switchPlayer :: Game -> Game
    switchPlayer (Game b PlayerO s) = Game b PlayerX s
    switchPlayer (Game b PlayerX s) = Game b PlayerO s

    countEmptyCells :: Board -> Int
    countEmptyCells (Board cells) = sum [ 1 | row <- cells, cell <- row, isNothing cell]

    full :: [Cell] -> Maybe Player
    full xs 
        | len == 1  = head xs
        | otherwise = Nothing
        where len = (length (nub xs))
    -- nem tudom eldönteni melyik a rosszabb megoldás :(
    -- full [x]        = x
    -- full (x:y:xs)
    --     | x == y    = full (y:xs)
    --     | otherwise = Nothing

    winner :: Board -> Maybe Player
    winner (Board cells) = head ((filter (\x -> isJust x) (getFulls cells)) ++ [Nothing])
        where
            getFulls :: [[Cell]] -> [Maybe Player]
            getFulls cells = [full c | c <- cells] ++ 
                             [full c | c<-[(map (!! r) cells)| r<-[0..(size-1)]]] ++ 
                             [full (zipWith (!!) cells [0..(size-1)])] ++ 
                             [full (zipWith (!!) cells [(size-1), (size-2)..0])]
    
    checkGameOver :: Game -> Game
    checkGameOver gameState@(Game b@(Board cells) p s)
        | Just r <- (winner b)             = Game b p (GameOver (winner b))
        | Nothing `notElem` (concat cells) = Game b p (GameOver Nothing)
        | otherwise                        = gameState

    replaceAt :: Int -> a -> [a] -> [a]
    replaceAt i to list = (take (i) list ) ++ to : (drop (i+1) list)

    playerTurn :: Game -> (Int,Int) -> Game
    playerTurn game@(Game (Board cells) p Running) (x,y)
        | x >= 0 && x < size && y >= 0 && y < size && isNothing ((cells !! x) !! y) = 
            checkGameOver (switchPlayer (Game (Board (replaceAt x (replaceAt y (Just p) (cells !! x)) cells)) p Running))
        | otherwise = game
    playerTurn game@(Game _ _ (GameOver _)) _ = game

    isGameOver :: Game -> Bool
    isGameOver (Game _ _ (GameOver _)) = True
    isGameOver _ = False
    
