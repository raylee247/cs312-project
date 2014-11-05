-- CPSC 312 Project 1
-- Raymond Lee 35832112

import Math.Geometry.Grid

--Starting the game
cluster_i5a8 board player minimax dimension = generateBoard_i5a8 board player minimax dimension

--Change board into hexagon
generateBoard_i5a8 board player minimax dimension = reverse(generateNewBoard_i5a8 newboard player minimax dimension)
	where 
		newboard = 	if (dimension == 0) then [] 
					else convertBoard_i5a8 newboard dimension --if the dimension is 0, then we should return a blank board

--Convert board to a hexagon
convertBoard_i5a8 new board dimension
	| dimension 


--Initiating the game
generateNewBoard_i5a8 board player minimax dimension
	| (board == []) || (dimension == 0) = board
	| minimax == 1 = board--need to make minimax
	| otherwise = generateNewBoard board player (minimax - 1) dimension --reduction step here?
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
	
	
