-- CPSC 312 Project 1
-- Raymond Lee 35832112

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
	


--Initiating the game
generateNewBoard_i5a8 board player minimax dimension
	| (board == []) || (dimension == 0) = board
	| minimax == 1 = board--need to make minimax
	| otherwise = board --generateNewBoard board player (minimax - 1) dimension --reduction step here?
	where 
		player = 	if (player == 'w') then 'b' --changing turns
					else 'w' 
	



--MOVEMENT STUFF
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
	
	
