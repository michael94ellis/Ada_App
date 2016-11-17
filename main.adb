--Checkers main program
with Ada.Text_IO; use Ada.Text_IO;
with GameBoard; use GameBoard;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with JEWL.Simple_Windows; use JEWL.Simple_Windows;

procedure main is
	timeKeeper					: Timer;    	           -- will cause an instance of the timer task to exist, do not make more than one
	-- Variables
	gameOver					: Boolean  		:= False;  -- Set to true when game is over
	playerTurn					: Integer 		:= 1;      -- 1 or 2 represents which player's turn it is (1-User, 2-Computer)
	spotIndex1, spotIndex2		: Integer		:= 0;
	indexErase					: Integer		:= 0;
	procedure nextTurn is	
	begin
		if playerTurn = 1 then
			Set_Text(WhosMoveLabel,"Turn: Player 2!");
			playerTurn := 2;
		elsif playerTurn = 2 then
			Set_Text(WhosMoveLabel,"Turn: Player 1!");
			playerTurn := 1;
		end if;
	end nextTurn;
	
	function getClick return Integer is
		spotIndex				: Integer 		:= 0;
		xFinder, yFinder		: Integer  		:= 0;
		lastClick				: Point_Type;
	begin
		while spotIndex = 0 loop
			xFinder   := 0; -- these two are just temp vars
			yFinder   := 0;
			lastClick := Start_Point(BoardCanvas); -- capture that stored data
			while xFinder <= lastClick.x loop --this will find the x value of the 1..64 grid coordinate
				xFinder := xFinder + 80;
				spotIndex := spotIndex + 1;
			end loop;
			while yFinder <= lastClick.y loop --this will find the y value of the 1..64 grid coordinate
				yFinder := yFinder + 80;
				if yFinder > 80 then
					spotIndex := spotIndex + 8;
				end if;
			end loop;
			-- Validation of location choice
			if spotIndex / 8 mod 2 = 1 and spotIndex mod 2 = 0 then
				if spotIndex mod 8 /= 0 then
					spotIndex := 0;
				end if;
			elsif spotIndex / 8 mod 2 = 0 and spotIndex mod 2 = 1 then
				spotIndex := 0;
			end if;
		end loop; --spotIndex1 now holds the array index of the piece to move
		if spotIndex / 8 mod 2 = 1 and spotIndex mod 8 /= 0 then
			spotIndex := spotIndex / 2 + 1; -- odd row
		else
			spotIndex := spotIndex / 2; -- even row
		end if;
		return spotIndex;
	end getClick;
	
begin
   --Loop through frame
	while Valid(AppFrame) loop
		--End game button is not visible
		endGame.Hide;
		startGame.Show; --Start button is visible
		--calls JEWL functions from the gameboard package to init the GUI
		-- game wont start until S button is pressed
		drawBoardGUI;
		-- begin playing
		--Begin timer
		timeKeeper.start;
		endGame.Show;
		while gameOver = False loop
		--	if playerTurn = 1 then
			case Next_Command is
				when 'C' => -- this will happen anytime that checkerboard image is clicked
					-- initiaize these indexs
					if spotIndex1 = 0 then
						spotIndex1 := getClick;			 -- first click
						spotIndex2 := 0;
						put(Integer'Image(spotIndex1));
					elsif spotIndex2 = 0 then 
						spotIndex2 := getClick;					 -- second click
						put(Integer'Image(spotIndex2));
						if board(spotIndex1).pieceValue /= 0 AND board(spotIndex2).pieceValue = 0 then
							--make the move
							if spotIndex1 = 0 or spotIndex2 = 0 then
								exit;
							end if;
							if isValidMove(spotIndex1,spotIndex2, playerTurn) = True then
								movePiece(spotIndex1, spotIndex2);			
								if spotIndex2 < 5 OR spotIndex2 > 28 then
									makeKing(spotIndex2);
								end if;
								nextTurn; 
							else
								indexErase := isValidJump(spotIndex1,spotIndex2,playerTurn);
								if indexErase /= 0 then -- then a jump is possible
									board(spotIndex2).pieceValue := board(spotIndex1).pieceValue;
									board(spotIndex1).pieceValue := 0;
									board(indexErase).pieceValue := 0;
									erasePiece(spotIndex1);
									erasePiece(indexErase);
									drawPiece(spotIndex2);
									if playerTurn = 1 then --player 1 score ++
										player1Score := player1Score + 1;
										Set_Text(PlayerScoreResult,Integer'Image(player1Score));
									elsif playerTurn = 2 then --player 2 score ++
										player2Score := player2Score + 1;
										Set_Text(ComputerScoreResult,Integer'Image(player2Score));
									end if;
									if  spotIndex2 < 5 OR spotIndex2 > 28 then
										makeKing(spotIndex2);
									end if;
									nextTurn;
								end if;
							end if;
							Put_Line("----------");
							for i in 1..32 loop
								put(board(i).pieceValue);
								if i mod 4 = 0 then
									New_Line;
								end if;
							end loop;
						else
							put("Invalid Input");
						end if;
						spotIndex1 := 0; -- reset the clicks for next player's turn
					end if;						
-----------------------------------------------------------------------------------------------------------------------------------------
				-- add Set_Text(scoreLabel, Integer'Image(intvar));
				when 'E' =>
					gameOver := True;
					timeKeeper.stop;
					Set_Text(WhosMoveLabel,"Game Over");
					exit;
				when others => null; -- does not but is required
			end case;
			-- Check for end game conditions
			if getP1Score = 12 then
				Put_Line("Player Wins!");
				gameOver := True;
			elsif getP2Score = 12 then
				Put_Line("Computer Wins!");
				gameOver := True;
			end if;
		end loop; -- main game loop
	end loop; -- frame loop
end main;
