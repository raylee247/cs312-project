-- CPSC 312 Project 1
-- Raymond Lee 35832112

board3 = "WWW-WW-------BB-BBB"

--Starting the game
cluster_i5a8 board player minimax dimension = generateBoard_i5a8 board player minimax dimension

--Change board into hexagon
generateBoard_i5a8 board player minimax dimension = reverse(generateNewBoard_i5a8 newboard player minimax dimension)
	where 
		newboard = 	if (dimension == 0) then [] 
					else convertBoard_i5a8 board dimension --if the dimension is 0, then we should return a blank board

--Convert board to a hexagon
convertBoard_i5a8 board n = addPadding(addSpaces(separateRowsInc board n n)) n n


-- Separates the board into rows
separateRowsInc :: String -> Int -> Int -> [String]
separateRowsInc board n count 
	| null board				= []  
	| ((2*n)-1) == count		= (take count board):(separateRowsDec (drop count board) n (count - 1))
	| otherwise					= (take count board):(separateRowsInc (drop count board) n (count + 1)) -- that is, whenever n =< count 
-- pretty much a helper function for when our count reaches (2*n)-1
separateRowsDec :: String -> Int -> Int -> [String]
separateRowsDec board n count
	| null board = []
	| n == count	= [(take n board)]
	| n < count 	= (take count board):(separateRowsDec (drop count board) n (count - 1))
	
-- add * spaces (cons the whole thing together for each element) 
addSpaces :: [String] -> [String]
addSpaces board 
	| null board	= []
	| otherwise		= (addSpaces_helper (head board)):(addSpaces(tail board))
	
-- puts * inbetween (focuses on an element) 
addSpaces_helper :: String -> String
addSpaces_helper row 
	| null row					= []
	| (length row == 1)			= (head row):[]
	| otherwise					= (head row):'*':(addSpaces_helper(tail row))
	
addPadding :: [String] -> Int -> Int -> [String] 
addPadding board n count
	| null board || count == 0 	= ["null"]
 -- | (2*((2*count)-1) - 1) == length(head board)		= (head board):(addPaddingDec (tail board) n (count+1)) -- max length of the board 
	| count /= 1							= (addPadding_helper (count - 1)++(head board)++ addPadding_helper (count-1)):(addPadding (tail board) n (count - 1))
	| otherwise	= (head board):(addPaddingDec (tail board) n (count+1)) -- max length of the board 

addPaddingDec :: [String] -> Int -> Int -> [String]
addPaddingDec board n count 
	| null board = []
	-- | n < count 		= (addPadding_helper (count - 1)++(head board)++(addPadding_helper(count-1))):(addPaddingDec (tail board) n (count - 1))
	| otherwise			= (addPadding_helper (count - 1)++(head board)++(addPadding_helper(count-1))):(addPaddingDec (tail board) n (count+1))
	
addPadding_helper :: Int -> String 
addPadding_helper count
	| count == 0		= []
	| otherwise		= '*':(addPadding_helper (count-1))

------------------------------------------------------------------------------	

--NOTE: Using board as a replacement so that I can test other components
--Initiating the game
generateNewBoard_i5a8 board player minimax dimension
	| null newboard = board
	| minimax == 1 = board--need to make minimax
	| otherwise = board --generateNewBoard board player (minimax - 1) dimension --reduction step here?
	where 
		player = 	if (player == 'w') then 'b' else 'w' --changing turns 
		newboard = board -- generateNewState board player [] --should be this
		
		
--------------------------------------------------------------------------------

--get character at x,y *******SOLELY USED FOR TESTING PURPOSES**********
getCoord :: [String] -> Int -> Int -> Char
getCoord board x y = getCol (getRow board y) x 

getRow :: [String] -> Int -> String
getRow board x
	| null board = []
	| x == 0 = (head board)
	| otherwise = getRow (tail board) (x - 1)
	
--	[**W*W*W**]
--	[*-*W*W*-*]
getCol :: String -> Int -> Char
getCol board y
	| null board = 'n'
	| y == 0 = head board
	| otherwise = getCol (tail board) (y - 1)


	
	
--MOVEMENT STUFF TO MAKE NEW BOARD (states)
--Make move, loc1 and loc2 are (x,y) coords

--makeMove board loc1 loc2 player
--	| null board = []
--	| (((fst loc1) > (fst loc2)) && ((snd loc1) == (snd loc2))) && (validSpot_i5a8 loc1 loc2) = moveLeft board loc1 loc2 player
--	| (((fst loc1) < (fst loc2)) && ((snd loc1) == (snd loc2))) && (validSpot_i5a8 loc1 loc2) = moveRight board loc1 loc2 player
--	| (((fst loc1) > (fst loc2)) && ((snd loc1) > (snd loc2))) && (validSpot_i5a8 loc1 loc2) = moveDownLeft board loc1 loc2 player
--	| (((fst loc1) < (fst loc2)) && ((snd loc1) > (snd loc2))) && (validSpot_i5a8 loc1 loc2) = moveDownRight board loc1 loc2 player

--move to the left, only returns the row, need to put it back to the board	

moveLeft board loc1 loc2 player = 	replace board --replaces old board with changes
										(clearSpace  --clear the old location
											(replaceListElem (getRow board (snd loc1)) ((fst loc1) - 2) player) --changes validspot into new player spot
										(fst loc1)) 
									(snd loc1) 0
				
moveRight board loc1 loc2 player = 	replace board 
										(clearSpace 
											(replaceListElem (getRow board (snd loc1)) ((fst loc1) + 2) player) 
										(fst loc1)) 
									(snd loc1) 0

moveDownLeft board loc1 loc2 player = 	replace board --replaces old board with changes
											(clearSpace --clear old position
												(getRow
													(replace board --replace old board with changes from moving to new location
														(replaceListElem (getRow board ((snd loc1) + 1)) ((fst loc1) - 1) player) --changes validspot into new player spot 
													((snd loc1) + 1) 0)
												(snd loc1))	
											(fst loc1)) 
										(snd loc1) 0

moveDownRight board loc1 loc2 player = 	replace board 
											(clearSpace 
												(getRow
													(replace board 
														(replaceListElem (getRow board ((snd loc1) + 1)) ((fst loc1) + 1) player) 
													((snd loc1) + 1) 0)
												(snd loc1))	
											(fst loc1)) 
										(snd loc1) 0

moveUpLeft board loc1 loc2 player = 	replace board 
											(clearSpace 
												(getRow
													(replace board 
														(replaceListElem (getRow board ((snd loc1) - 1)) ((fst loc1) - 1) player) 
													((snd loc1) - 1) 0)
												(snd loc1))	
											(fst loc1)) 
										(snd loc1) 0

moveUpRight board loc1 loc2 player = 	replace board 
											(clearSpace 
												(getRow
													(replace board 
														(replaceListElem (getRow board ((snd loc1) - 1)) ((fst loc1) + 1) player)
													((snd loc1) - 1) 0)
												(snd loc1))	
											(fst loc1)) 
										(snd loc1) 0

--FLIP IS NOT IN USE, CHANGED IDEA FOR NOW
--reverse the board, and change loc1 and loc2's Y values, change player or move "up"
--then flips the board so that we know we're on the other players turn
--play who is playing will be on top when visualizing, but will flip back over once done, so when added to state it
--should be all the same

--ex. W's turn, moves to the left
--[**W*W*W**]
--[*-*W*W*-*]
--[-*-*-*-*-]
--[*-*B*B*-*]
--[**B*B*B**]

--[**W*W*W**]
--[*W*-*W*-*]
--[-*-*-*-*-]
--[*-*B*B*-*]
--[**B*B*B**]
--End turn

--Flip, B's turn, moves right
--[**B*B*B**]
--[*-*B*B*-*]
--[-*-*-*-*-]
--[*W*-*W*-*]
--[**W*W*W**]
--flipBoard board loc1 loc2 player = flipBoard_helper board loc1 loc2 player []

--flipBoard_helper board loc1 loc2 player acc
--	| null board = makeMove acc (fst loc1):((length board) - (snd loc1)) (fst loc2):((length board) - (snd loc2)) player
--	| otherwise = flipBoard_helper (tail board) loc1 loc2 player((head board):acc)

--replace piece that moved with a -
clearSpace str1 index = replaceListElem str1 index '-'
	
--replaces index with that element	
replaceListElem list index elem
	| null list = []
	| index == 0	= elem : (tail list)
	| otherwise	= (head list) : replaceListElem (tail list) (index - 1) elem

	
--check if space is empty
isEmpty_i5a8 space
	| space == '-' = True
	| otherwise = False

--Check if you can move from loc1 to loc2
validSpot_i5a8 loc1 loc2
	= (not (isEmpty_i5a8 loc1)) && (isEmpty_i5a8 loc2)
	
--Check if you can capture an opposing piece from loc1 to loc2 (jumping)
capture_i5a8 loc1 loc2
	| ((loc1 == 'w') && (loc2 == 'b')) || ((loc1 == 'b') && (loc2 == 'w')) = True
	| otherwise = False
	
--change board with new row, start = 0
replace board line row start
	| null board = []
	| start == row = line:(tail board)
	| otherwise = (head board) : replace (tail board) line row (start + 1)

-- -----------------------------------------ONLINE STUFF-------------------------------------------	
-- -- moves a given pawn left and forward relative to direction of travel
-- -- this is the only pawn moving function that contains real implementaiton, other pawn moving functions makes use of this default function with representation adjusted appropriately
-- generateNewStatePawnMvLeftDefault :: [String] -> Char -> (Int, Int) -> Bool -> [String]
-- generateNewStatePawnMvLeftDefault currBoard player pawnloc isJump
	-- | ((fst pawnloc) >= (length currBoard) - 1) || null (fst newStr2) = []
	-- | not isJump = replaceListElem (replaceListElem currBoard (fst pawnloc) (generateStr1 (currBoard !! (fst pawnloc)) (snd pawnloc))) (fst pawnloc + 1) (fst newStr2)
	-- | otherwise = generateNewStatePawnMvLeftDefault (replaceListElem (replaceListElem currBoard (fst pawnloc) (generateStr1 (currBoard !! (fst pawnloc)) (snd pawnloc))) (fst pawnloc + 1) (fst newStr2))
				-- player ((fst pawnloc + 1), snd newStr2) False
	-- where 
		-- newStr2 = generateStr2MvLeft (currBoard !! (fst pawnloc)) (currBoard !! (fst pawnloc + 1)) (snd pawnloc) isJump

-- -- moves given pawn left and forward simply by passing params into generateNewStatePawnMvLeftDefault
-- -- this funciton exists for consistency in funciton naming and organization
-- generateNewStatePawnMvLeft :: [String] -> Char -> (Int, Int) -> [String]
-- generateNewStatePawnMvLeft currBoard player pawnloc = generateNewStatePawnMvLeftDefault currBoard player pawnloc False

-- -- moves given pawn right and forward
-- generateNewStatePawnMvRight :: [String] -> Char -> (Int, Int) -> [String]
-- generateNewStatePawnMvRight currBoard player pawnloc = reverseBoard (generateNewStatePawnMvLeftDefault (reverseBoard currBoard) player (fst pawnloc, (length (currBoard !! (fst pawnloc)) - (snd pawnloc + 1))) False)

-- -- jumps given pawn left and forward where appropriate (removing an opponene pawn)
-- generateNewStatePawnJumpLeft :: [String] -> Char -> (Int, Int) -> [String]
-- generateNewStatePawnJumpLeft currBoard player pawnloc
-- = generateNewStatePawnMvLeftDefault currBoard player pawnloc True

-- -- jumps given pawn right and forward where appropriate (removing an opponene pawn)
-- generateNewStatePawnJumpRight :: [String] -> Char -> (Int, Int) -> [String]
-- generateNewStatePawnJumpRight currBoard player pawnloc
-- = reverseBoard (generateNewStatePawnMvLeftDefault (reverseBoard currBoard) player (fst pawnloc, (length (currBoard !! (fst pawnloc)) - (snd pawnloc + 1))) True)

-- -- generates resulting str1 from generating new state
-- -- used to replace str1 in current state
-- generateStr1 :: String -> Int -> String
-- generateStr1 str1 index = replaceListElem str1 index '-'

-- -- generates resulting str2 from generating new state when moving pawn left and forward
-- -- used to replace str2 in current state
-- generateStr2MvLeft :: String -> String -> Int -> Bool -> (String, Int)
-- generateStr2MvLeft str1 str2 index isJump
	-- | ((length str1 - length str2) == 1) && (index > 0) = makeMove str2 (index - 1) (str1 !! index) isJump
	-- | ((length str1 - length str2) == -1) && (index < length str1) = makeMove str2 index (str1 !! index) isJump
	-- | otherwise	= ([], 0)

-- -- called by generateStr1MvLeft to actually move pawns
-- makeMove :: String -> Int -> Char -> Bool -> (String, Int)
-- makeMove str2 index pawn isJump
	-- | (isJump && canJump pawn (str2 !! index)) || (not isJump && canMove pawn (str2 !! index)) = ((replaceListElem str2 index pawn), index)
	-- | otherwise	= ([], 0)



