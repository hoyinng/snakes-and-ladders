{- -------------------------------------------------
--	Name:   Ho Yin Ng
--	Course: CPS506, Winter 2017, Assignment #3
--	Due:    2017.03.30 
--	Credit: This is entirely my own work
------------------------------------------------- -}
-- Comment : test case can be tested as such
-- $> ghci 
-- Main> :load assign3.hs
-- Main> runTestTT tests
-- Cases: 9  Tried: 9  Errors: 0  Failures: 0
-- Counts {cases = 9, tried = 9, errors = 0, failures = 0}
------------------------------------------------
import System.IO
import Data.Char
import Debug.Trace
import Test.HUnit

------------------------------------------------
-- Player Data Type
data Player = Player {name::String,es::Bool, av::Bool, double::Bool, loc::Int} deriving Show

-- Board will be an array will fill with 0s or player number
data GameData = GameData {board::[Int],players :: [Player] ,
	ladder :: [(Int,Int)], snake :: [(Int,Int)], turn :: Int, 
	seqs :: [Int], player ::Int, boardX :: Int, boardY :: Int, 
	powerup :: [(Int, Char)],diceCounter ::Int, won :: Bool}

instance Show GameData where
	show myData = printData myData

empty = GameData{board = [], players = [], ladder= [], 
	snake = [], turn =0, seqs = [], player =0, boardX = 0, boardY = 0,
	powerup = [],diceCounter =0, won = False}

------------------------------------------------
main = do
	input <- getContents
	putStr $ show $ readFrom input
------------------------------------------------

-- Return a simulated game with input aStr
readFrom :: String -> GameData 
readFrom aStr = simulateGame (initalizeGame (lines(aStr)) empty)

initalizeGame :: [String] -> GameData -> GameData
initalizeGame [] gd = gd
initalizeGame (x:s) gd = initalizeGame s (coreFunc(words(x)) gd)

processTurns :: GameData -> GameData
processTurns p@(GameData _ _ _ _ _ _ _ _ _ _ _ True) = p{turn=0}
processTurns p@(GameData _ _ _ _ 0 _ _ _ _ _ _ _) = p 
processTurns gd = do
	let tgd = foldl (\agd pnum -> (forwardPlayerBy agd (cycle (seqs agd) !! (diceCounter agd)) pnum ){diceCounter = ((diceCounter agd)+1)}) gd [1..(player gd)]
		in  processTurns tgd{ turn = ((turn gd) - 1)}

simulateGame :: GameData -> GameData
simulateGame gd = do
	let ggd = initalizePlayersAndBoard $ initalizePlayers $ initalizeBoard $ gd 
	let pgd = processTurns ggd
		in pgd

-- Parse String With Pattern Match
coreFunc :: [String] -> GameData -> GameData
coreFunc ["board",x,y] gd = gd{boardX= read x :: Int, boardY = read y :: Int}
coreFunc ["players",x] gd = gd{player= read x :: Int }
coreFunc ["turns",x] gd = gd {turn = read x :: Int}
coreFunc ("dice":x) gd = gd {seqs = map (\tmp -> read tmp :: Int) x}
coreFunc ["snake", x,y] gd = gd { snake = (snake gd) ++ [(read x :: Int, read y :: Int)]}
coreFunc ["ladder", x,y] gd = gd { ladder = (ladder gd) ++ [(read x :: Int, read y :: Int)]}
coreFunc ("powerup":kw:locations) gd = gd {powerup= (powerup gd) ++  
	map (\x->( read x :: Int , head kw) ) locations}
coreFunc [_] gd = gd
		
-- Game_functions ( gf )-- 
-- Initalize Board 
initalizeBoard :: GameData -> GameData
initalizeBoard gd = gd{board = map (\_ -> 0) [1..(boardX gd) * (boardY gd)]}

-- Initalize_Players 
initalizePlayers :: GameData -> GameData
initalizePlayers gd = gd {players = map (\n->Player{name= ([chr (64+n)]) ,av=False,es=False, double=False,loc =0}) [1..(player gd)] }

-- Place all the player in to the board
initalizePlayersAndBoard :: GameData -> GameData
initalizePlayersAndBoard gd = foldl (\tgd pnum -> (movePlayer tgd 1 pnum)) gd [1..(player gd)]

-- Moves player by 1
forwardPlayer :: GameData -> Int -> GameData
forwardPlayer gd aPlayer = movePlayer gd ( (getPlayerLoc gd aPlayer) +1) aPlayer

-- Move a player with a given amount
forwardPlayerBy :: GameData -> Int -> Int-> GameData
forwardPlayerBy gd 0 _ = gd
forwardPlayerBy gd amount aPlayer = movePlayer gd (amount+ (getPlayerLoc gd aPlayer)) aPlayer

-- MovePlayer 
movePlayer :: GameData -> Int -> Int-> GameData
movePlayer gd aLoc aPlayer 
	| aLoc ==  (boardX gd) * (boardY gd)= do 
		-- Remove Where The Player Was In Board Map 
		let removedPrevSpotBoard =  (myReplaceAt (board gd) 0 ((getPlayerLoc gd aPlayer)))
		let boardPlacedPlayer = myReplaceAt (removedPrevSpotBoard) aPlayer aLoc
			in gd { board = boardPlacedPlayer,
				players = myReplaceAt (players gd) (getPlayer gd aPlayer){loc=aLoc} aPlayer,
				won = True
				}
	| aLoc <= ( (boardX gd) * (boardY gd)) = 
		if (board gd)!! (aLoc-1) == 0
		then -- place player , modify player list with player{loc = new Location}
			do	let removedPrevSpotBoard =  myReplaceAt (board gd) 0 ((getPlayerLoc gd aPlayer))
				let cpb =
					if (lookup aLoc (powerup gd) /= Nothing)
					then gd {players = (myReplaceAt (players gd) (modifyPowerUp (getPlayer gd aPlayer) (lookup aLoc (powerup gd)) True) aPlayer)}
					else gd
				let boardPlacedPlayer = myReplaceAt (removedPrevSpotBoard) aPlayer aLoc
					in checkSNL (cpb{ board = boardPlacedPlayer,
					players = myReplaceAt (players cpb) (getPlayer cpb aPlayer){loc=aLoc} aPlayer
					}) aPlayer
		
		else do --BUMP
			--BUMP LOGIC : Move Other Player By 1, (Remove him from board), place him on board, modify his record, Move Me on the free spot 
			let occupant = playerAt gd aLoc
			let gdWithOccupantMoved = movePlayer gd (aLoc+1) occupant
			let gdWithCurPlayer = movePlayer gdWithOccupantMoved aLoc aPlayer
				in gdWithCurPlayer
	| aLoc > ((boardX gd) * (boardY gd)) = (gd)
	-- if player step over bound
	|otherwise = (trace "ERROR HOOOOMAN" ) (gd)
-- Check for snakes and ladders
checkSNL :: GameData -> Int -> GameData
checkSNL gd aPlayer 
	| (lookup (getPlayerLoc gd aPlayer) (snake gd) /= Nothing) = do
		let dst = eliminate (lookup (getPlayerLoc gd aPlayer) (snake gd))	
		let pObj = (getPlayer gd aPlayer)
		if  ((av pObj) == False) 
			then (movePlayer gd dst aPlayer)
			else gd{players= (myReplaceAt (players gd) pObj{av=False} aPlayer) }
	| (lookup (getPlayerLoc gd aPlayer) (ladder gd) /= Nothing) = do
		let dst = eliminate (lookup (getPlayerLoc gd aPlayer) (ladder gd))	
		let pObj = (getPlayer gd aPlayer)	
		let displacement = dst - (loc pObj)
		if  ((es pObj) == False) 
			then (movePlayer gd dst aPlayer)
			else forwardPlayerBy (gd{players= (myReplaceAt (players gd) pObj{es=False} aPlayer) }) (displacement *2) aPlayer
	| otherwise = (gd)
---------------------------------------------------------------------------
------ Printing Data -------
printData :: GameData -> String
printData gd = (foldl (\x y -> (x ++ ( (printNumberBlocks (y * boardX gd +1) ( (y+1) * (boardX gd) ) (cycle ['l','r'] !!  y ) ) ++ "\n" ++ (printRow gd y (cycle ['l','r'] !!  y ) ) ++ (printBoarder (boardX gd)) ))) (printBoarder (boardX gd)) (reverse [0..(boardY gd)-1])) ++ (printWinCondition gd)

printWinCondition :: GameData -> String
printWinCondition gd 
		| ((won gd) == True) = ("Player " ++ (name (getPlayer gd ( playerAt gd ( (boardX gd) * (boardY gd) ) )) ) ++ " won\n")
		| otherwise = ""

printRow :: GameData -> Int -> Char -> String
printRow gd row 'l' = (foldl (\x y-> (x ++ (printGameChunk gd y) ++ "|") ) "|" [ (1 + (boardX gd) * (row) ).. ((row)  * (boardX gd) + (boardX gd) )] ) ++ "\n"
printRow gd row 'r' = (foldl (\x y-> (x ++ (printGameChunk gd y) ++ "|") ) "|" (reverse [ (1 + (boardX gd) * (row) ).. ((row)  * (boardX gd) + (boardX gd) )] )) ++ "\n"

printBoarder :: Int -> String
printBoarder n = (foldl (++) "+" (take n (cycle (["---+"])))) ++ "\n"

printNumberBlocks :: Int -> Int -> Char -> String
printNumberBlocks a b 'l' = (foldl (\acc x -> acc ++ (take (3- (length (show x))) "   " ) ++ (show x) ++ "|")  "|" [a..b]  )
printNumberBlocks a b 'r' = (foldl (\acc x -> acc ++ (take (3- (length (show x))) "   " ) ++ (show x) ++ "|")  "|" (reverse [a..b])  )

printGameChunk :: GameData -> Int -> String
printGameChunk gd cell = do
	let n = cell 
	let playerIdentifier =  ((board gd) !! (n-1) )
	let playerBit = if (playerIdentifier /= 0)
			then (name (getPlayer gd playerIdentifier))
			else " "
	let ladderBit = if ((lookup n (snake gd)) /= Nothing)
			then "S"
			else if ((lookup n (ladder gd)) /= Nothing)
			then "L"
			else " "
	let pwrUpBit = if ((lookup n (powerup gd)) /= Nothing)
			then [(eliminate (lookup n (powerup gd)))]
			else " "
		in playerBit ++ pwrUpBit ++ ladderBit
---------------------------------------------------------------------------
-- Helper Functions --hf
eliminate :: (Maybe t) -> t
eliminate (Just c) = c

giveKey prmpt 
	| prmpt == 'a' = av 
	| prmpt == 'd' = double 
	| prmpt == 'e' = es
	| otherwise = es

modifyPowerUp :: Player -> Maybe Char -> Bool -> Player
modifyPowerUp aPlayer powerupType aBool
	| powerupType == Just 'a' = aPlayer{av=aBool}
	| powerupType == Just 'e' = aPlayer{es=aBool}
	| powerupType == Just 'd' = aPlayer{double=aBool}
	| otherwise = aPlayer

myReplaceAt :: [t] -> t -> Int -> [t]
myReplaceAt aList element 0 = [element] ++ (drop 1 aList)
myReplaceAt aList element index = take (index-1) aList ++ [element] ++ drop index aList 

getPlayerLoc :: GameData -> Int -> Int
getPlayerLoc gd aPlayer = (loc ((players gd) !! (aPlayer -1)))

getPlayer :: GameData -> Int ->Player
getPlayer gd index = (players gd) !! (index-1)

-- Board 1,2,3..n -> return 0,1,..n-1 (Array index)
playerAt :: GameData -> Int -> Int
playerAt gd i = (board gd) !! (i-1)

------------------------
 --- TESTING STUFF ----
------------------------
-- Custom Test (IGNORE ME!!!!)
cTest1 = readFrom "board 3 4\nplayers 2\ndice 1 2 2 2 2\nladder 5 11\nsnake 8 4\npowerup escalator 6 9\npowerup antivenom 7\npowerup double 4\nturns 10"
cTest2 = readFrom "board 3 4\n\
                      \players 2\n\
                      \dice 1 2\n\
                      \turns 5"
-- Given Test
tests = test [
        "tiny board" ~:
              "+---+---+\n\
              \|  4|  3|\n\
              \|   |   |\n\
              \+---+---+\n\
              \|  1|  2|\n\
              \|   |   |\n\
              \+---+---+\n" ~=?
            (show $ readFrom "board 2 2")

        ,"small board" ~:
              "+---+---+\n\
              \|  5|  6|\n\
              \|   |   |\n\
              \+---+---+\n\
              \|  4|  3|\n\
              \|   |   |\n\
              \+---+---+\n\
              \|  1|  2|\n\
              \|   |   |\n\
              \+---+---+\n" ~=?
            (show $ readFrom "board 2 3")

        ,"small board with players" ~:
              "+---+---+\n\
              \|  5|  6|\n\
              \|   |   |\n\
              \+---+---+\n\
              \|  4|  3|\n\
              \|   |   |\n\
              \+---+---+\n\
              \|  1|  2|\n\
              \|B  |A  |\n\
              \+---+---+\n" ~=?
            (show $ readFrom "board 2 3\n\
                      \players 2")

        ,"read0" ~:
              "+---+---+---+\n\
              \| 12| 11| 10|\n\
              \|   |   |   |\n\
              \+---+---+---+\n\
              \|  7|  8|  9|\n\
              \|   |   |   |\n\
              \+---+---+---+\n\
              \|  6|  5|  4|\n\
              \|   |   |   |\n\
              \+---+---+---+\n\
              \|  1|  2|  3|\n\
              \|C  |B  |A  |\n\
              \+---+---+---+\n" ~=?
            (show $ readFrom "board 3 4\n\
                      \players 3")

        ,"read1" ~:
              "+---+---+---+\n\
              \| 12| 11| 10|\n\
              \|   |   |   |\n\
              \+---+---+---+\n\
              \|  7|  8|  9|\n\
              \|   |   |   |\n\
              \+---+---+---+\n\
              \|  6|  5|  4|\n\
              \|A  |B  |   |\n\
              \+---+---+---+\n\
              \|  1|  2|  3|\n\
              \|   |   |   |\n\
              \+---+---+---+\n" ~=?
            (show $ readFrom "board 3 4\n\
                      \players 2\n\
                      \dice 1\n\
                      \turns 4")

        ,"read2" ~:
              "+---+---+---+\n\
              \| 12| 11| 10|\n\
              \|A  |B  |   |\n\
              \+---+---+---+\n\
              \|  7|  8|  9|\n\
              \|   |   |   |\n\
              \+---+---+---+\n\
              \|  6|  5|  4|\n\
              \|   |   |   |\n\
              \+---+---+---+\n\
              \|  1|  2|  3|\n\
              \|   |   |   |\n\
              \+---+---+---+\n\
              \Player A won\n" ~=?
            (show $ readFrom "board 3 4\n\
                      \players 2\n\
                      \dice 1 2\n\
                      \turns 5")


          ,"read3 no Snakes" ~:
                "+---+---+---+\n\
                \| 12| 11| 10|\n\
                \|B  |   |   |\n\
                \+---+---+---+\n\
                \|  7|  8|  9|\n\
                \|   |A  |   |\n\
                \+---+---+---+\n\
                \|  6|  5|  4|\n\
                \|   |  L|   |\n\
                \+---+---+---+\n\
                \|  1|  2|  3|\n\
                \|   |   |   |\n\
                \+---+---+---+\n\
                \Player B won\n" ~=?
              (show $ readFrom "board 3 4\n\
                        \players 2\n\
                        \dice 1 2 2 2 2\n\
                        \ladder 5 11\n\
                        \turns 5")

          ,"read3" ~:
                "+---+---+---+\n\
                \| 12| 11| 10|\n\
                \|B  |   |   |\n\
                \+---+---+---+\n\
                \|  7|  8|  9|\n\
                \|   |  S|   |\n\
                \+---+---+---+\n\
                \|  6|  5|  4|\n\
                \|   |  L|A  |\n\
                \+---+---+---+\n\
                \|  1|  2|  3|\n\
                \|   |   |   |\n\
                \+---+---+---+\n\
                \Player B won\n" ~=?
              (show $ readFrom "board 3 4\n\
                        \players 2\n\
                        \dice 1 2 2 2 2\n\
                        \ladder 5 11\n\
                        \snake 8 4\n\
                        \turns 5")

          ,"read4" ~:
                "+---+---+---+\n\
                \| 12| 11| 10|\n\
                \|B  |   |   |\n\
                \+---+---+---+\n\
                \|  7|  8|  9|\n\
                \| a |  S| e |\n\
                \+---+---+---+\n\
                \|  6|  5|  4|\n\
                \| e |  L|Ad |\n\
                \+---+---+---+\n\
                \|  1|  2|  3|\n\
                \|   |   |   |\n\
                \+---+---+---+\n\
                \Player B won\n" ~=?
              (show $ readFrom "board 3 4\n\
                        \players 2\n\
                        \dice 1 2 2 2 2\n\
                        \ladder 5 11\n\
                        \snake 8 4\n\
                        \powerup escalator 6 9\n\
                        \powerup antivenom 7\n\
                        \powerup double 4\n\
                        \turns 10")

          ]
