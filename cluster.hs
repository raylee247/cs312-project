-- CPSC 312 Project 1
-- Raymond Lee 35832112

board1 = "WWW-WW-------BB-BBB"
board2 = "WWW-----WW---BB-BBB"
testboard = convertBoard_i5a8 board1 3 --[**W*W*W**,*-*W*W*-*,-*-*-*-*-,*-*B*B*-*,**B*B*B**]
testboard2 = convertBoard_i5a8 board2 3 --[**W*W*W**,*-*-*-*-*,-*W*W*-*-,*-*B*B*-*,**B*B*B**]
--[**W*W*W**]
--[*-*W*W*-*]
--[-*-*-*-*-]
--[*-*B*B*-*]
--[**B*B*B**]
testrow = getRow testboard 2
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
getCoord board x y = getCol (getRow board x) y 

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


	
	
-------------------------MOVEMENT STUFF TO MAKE NEW BOARD (states)------------------------------------------
--Make move, loc1 and loc2 are (x,y) coords
--UNCOMMMENT WHEN GENERATE STATES && MINIMAX IS DONE, THIS WILL DETERMINE IF MOVES ARE POSSIBLE

--Make a move, check if the spot is valid, and also if the generated board has not already been generated
--when calling makeMove, history initially needs to be an empty list (it keeps track of all used boards, we cannot have the same board twice

--makeMove board loc1 loc2 player history
--	| null board = []
--	| (((fst loc1) > (fst loc2)) && ((snd loc1) == (snd loc2))) && (validSpot_i5a8 loc1 loc2) = if (elem (moveLeft board loc1 player) history) then history else (moveLeft board loc1 player) : history
--	| (((fst loc1) < (fst loc2)) && ((snd loc1) == (snd loc2))) && (validSpot_i5a8 loc1 loc2) = if (elem (moveRightt board loc1 player) history) then history else (moveRight board loc1 player) : history
--	| (((fst loc1) > (fst loc2)) && ((snd loc1) < (snd loc2))) && (validSpot_i5a8 loc1 loc2) = if (elem (moveDownLeft board loc1 player) history) then history else (moveDownLeft board loc1 player) : history
--	| (((fst loc1) < (fst loc2)) && ((snd loc1) < (snd loc2))) && (validSpot_i5a8 loc1 loc2) = if (elem (moveDownRight board loc1 player) history) then history else (moveDownRight board loc1 player) : history
--	| (((fst loc1) > (fst loc2)) && ((snd loc1) > (snd loc2))) && (validSpot_i5a8 loc1 loc2) = if (elem (moveUpLeft board loc1 player) history) then history else (moveUpLeft board loc1 player) : history
--	| (((fst loc1) < (fst loc2)) && ((snd loc1) > (snd loc2))) && (validSpot_i5a8 loc1 loc2) = if (elem (moveUpRight board loc1 player) history) then history else (moveUpRight board loc1 player) : history
--	| otherwise = history

--Makes a left slide
moveLeft board loc1 player = 	replace board --replaces old board with changes
									(clearSpace  --clear the old location
										(replaceListElem (getRow board (snd loc1)) ((fst loc1) - 2) player) --changes validspot into new player spot
									(fst loc1)) 
								(snd loc1) 0
--TEST CASE 
--moveLeft testboard (3,1) (2,1) "W"
--[**W*W*W**]
--[*W*-*W*-*]
--[-*-*-*-*-]
--[*-*B*B*-*]
--[**B*B*B**]

--Makes a right slide				
moveRight board loc1 player = 	replace board 
									(clearSpace 
										(replaceListElem (getRow board (snd loc1)) ((fst loc1) + 2) player) 
									(fst loc1)) 
								(snd loc1) 0

--TEST CASE 
--moveLeft testboard (5,1) (7,1) "W"
--[**W*W*W**]
--[*-*W*-*W*]
--[-*-*-*-*-]
--[*-*B*B*-*]
--[**B*B*B**]									

--Makes a down and left slide				
moveDownLeft board loc1  player = 	replace newboard --replaces old board with changes
										(clearSpace --clear old position
											(getRow newboard (snd loc1))	
										(fst loc1)) 
									(snd loc1) 0
									where
										newboard = 	(replace board --replace old board with changes from moving to new location
														(replaceListElem (getRow board ((snd loc1) + 1)) ((fst loc1) - 1) player) --changes validspot into new player spot 
													((snd loc1) + 1) 0)
--TEST CASE 
--moveDownLeft testboard (5,1) "W"
--[**W*W*W**]
--[*-*W*-*-*]
--[-*-*W*-*-]
--[*-*B*B*-*]
--[**B*B*B**]													
					
--Makes a down and right slide					
moveDownRight board loc1  player = 	replace newboard 
										(clearSpace 
											(getRow newboard (snd loc1))	
										(fst loc1)) 
									(snd loc1) 0
									where
										newboard = 	(replace board 
														(replaceListElem (getRow board ((snd loc1) + 1)) ((fst loc1) + 1) player) 
													((snd loc1) + 1) 0)
--TEST CASE 
--moveDownRight testboard (5,1) "W"
--[**W*W*W**]
--[*-*W*-*-*]
--[-*-*-*W*-]
--[*-*B*B*-*]
--[**B*B*B**]	

--Makes a up and left slide
moveUpLeft board loc1  player = 	replace newboard
										(clearSpace 
											(getRow
												newboard
											(snd loc1))	
										(fst loc1)) 
									(snd loc1) 0
									where
										newboard = 	(replace board 
														(replaceListElem (getRow board ((snd loc1) - 1)) ((fst loc1) - 1) player) 
													((snd loc1) - 1) 0)
--TEST CASE 
--moveUpLeft testboard (5,3) "B"
--[**W*W*W**]
--[*-*W*Q*-*]
--[-*-*B*-*-]
--[*-*B*-*-*]
--[**B*B*B**]	


--Makes a up and right slide
moveUpRight board loc1  player = 	replace newboard 
										(clearSpace 
											(getRow newboard (snd loc1))	
										(fst loc1)) 
									(snd loc1) 0
									where
										newboard = 	(replace board 
														(replaceListElem (getRow board ((snd loc1) - 1)) ((fst loc1) + 1) player)
													((snd loc1) - 1) 0)
--TEST CASE 
--moveUpRight testboard (5,1) "B"
--[**W*W*W**]
--[*-*W*W*-*]
--[-*-*-*B*-]
--[*-*B*-*-*]
--[**B*B*B**]	

----------------------------JUMPING--------------------------------------------

--SAME COMMENTS AS THE makeMove

--makeJump board loc1 loc2 player history
--	| null board = []
--	| (((fst loc1) > (fst loc2)) && ((snd loc1) == (snd loc2))) && (validSpot_i5a8 loc1 loc2) && (capture_i5a8 loc1 loc2)= if (elem (jumpLeft board loc1 player) history) then history else (jumpLeft board loc1 player) : history
--	| (((fst loc1) < (fst loc2)) && ((snd loc1) == (snd loc2))) && (validSpot_i5a8 loc1 loc2) && (capture_i5a8 loc1 loc2)= if (elem (jumpRight board loc1 player) history) then history else (jumpRight board loc1 player) : history
--	| (((fst loc1) > (fst loc2)) && ((snd loc1) < (snd loc2))) && (validSpot_i5a8 loc1 loc2) && (capture_i5a8 loc1 loc2)= if (elem (jumpDownLeft board loc1 player) history) then history else (jumpDownLeft board loc1 player) : history
--	| (((fst loc1) < (fst loc2)) && ((snd loc1) < (snd loc2))) && (validSpot_i5a8 loc1 loc2) && (capture_i5a8 loc1 loc2)= if (elem (jumpDownRight board loc1 player) history) then history else (jumpDownRight board loc1 player) : history
--	| (((fst loc1) > (fst loc2)) && ((snd loc1) > (snd loc2))) && (validSpot_i5a8 loc1 loc2) && (capture_i5a8 loc1 loc2)= if (elem (jumpUpLeft board loc1 player) history) then history else (jumpUpLeft board loc1 player) : history
--	| (((fst loc1) < (fst loc2)) && ((snd loc1) > (snd loc2))) && (validSpot_i5a8 loc1 loc2) && (capture_i5a8 loc1 loc2)= if (elem (jumpUpRight board loc1 player) history) then history else (jumpUpRight board loc1 player) : history
--	| otherwise = history

jumpLeft board loc1 player = replace board --replaces old board with changes
								(clearSpace  --clear the old location
									(replaceListElem (getRow board (snd loc1)) ((fst loc1) - 4) player) --changes validspot into new player spot
								(fst loc1)) 
							(snd loc1) 0
--TEST CASE
--jumpLeft testboard2 (4,2) 'W'
--[**W*W*W**]
--[*-*-*-*-*]
--[W*W*-*-*-]
--[*-*B*B*-*]
--[**B*B*B**]	
							
jumpRight board loc1 player = replace board --replaces old board with changes
								(clearSpace  --clear the old location
									(replaceListElem (getRow board (snd loc1)) ((fst loc1) + 4) player) --changes validspot into new player spot
								(fst loc1)) 
							(snd loc1) 0
--TEST CASE
--jumpLeft testboard2 (2,2) 'W'
--[**W*W*W**]
--[*-*-*-*-*]
--[-*-*W*W*-]
--[*-*B*B*-*]
--[**B*B*B**]

jumpDownLeft board loc1  player = 	replace newboard --replaces old board with changes
										(clearSpace --clear old position
											(getRow newboard (snd loc1))	
										(fst loc1)) 
									(snd loc1) 0
									where
										newboard = 	(replace board --replace old board with changes from moving to new location
														(replaceListElem (getRow board ((snd loc1) + 2)) ((fst loc1) - 2) player) --changes validspot into new player spot 
													((snd loc1) + 2) 0)
--TEST CASE
--jumpDownLeft testboard (4,0) 'W'
--[**W*-*W**]
--[*-*W*W*-*]
--[-*W*-*-*-]
--[*-*B*B*-*]
--[**B*B*B**]

jumpDownRight board loc1  player = 	replace newboard --replaces old board with changes
										(clearSpace --clear old position
											(getRow newboard (snd loc1))	
										(fst loc1)) 
									(snd loc1) 0
									where
										newboard = 	(replace board --replace old board with changes from moving to new location
														(replaceListElem (getRow board ((snd loc1) + 2)) ((fst loc1) + 2) player) --changes validspot into new player spot 
													((snd loc1) + 2) 0)
--TEST CASE
--jumpDownRight testboard (4,0) 'W'
--[**W*-*W**]
--[*-*W*W*-*]
--[-*-*-*W*-]
--[*-*B*B*-*]
--[**B*B*B**]

jumpUpLeft board loc1  player = 	replace newboard --replaces old board with changes
										(clearSpace --clear old position
											(getRow newboard (snd loc1))	
										(fst loc1)) 
									(snd loc1) 0
									where
										newboard = 	(replace board --replace old board with changes from moving to new location
														(replaceListElem (getRow board ((snd loc1) - 2)) ((fst loc1) - 2) player) --changes validspot into new player spot 
													((snd loc1) - 2) 0)
--TEST CASE
--jumpUpLeft testboard2 (4,4) 'B'
--[**W*W*W**]
--[*-*W*W*-*]
--[-*B*-*-*-]
--[*-*B*B*-*]
--[**B*-*B**]

jumpUpRight board loc1  player = 	replace newboard --replaces old board with changes
										(clearSpace --clear old position
											(getRow newboard (snd loc1))	
										(fst loc1)) 
									(snd loc1) 0
									where
										newboard = 	(replace board --replace old board with changes from moving to new location
														(replaceListElem (getRow board ((snd loc1) - 2)) ((fst loc1) + 2) player) --changes validspot into new player spot 
													((snd loc1) - 2) 0)
--TEST CASE
--jumpUpLeft testboard2 (4,4) 'B'
--[**W*W*W**]
--[*-*W*W*-*]
--[-*-*-*B*-]
--[*-*B*B*-*]
--[**B*-*B**]


--------------------------FLIP IS NOT IN USE, CHANGED IDEA FOR NOW--------------------------------------------
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
----------------------------FLIP IS NOT USED-------------------------------------------------

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




