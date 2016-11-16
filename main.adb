--Checkers main program
with Ada.Text_IO; use Ada.Text_IO;
with GameBoard; use GameBoard;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with JEWL.Simple_Windows; use JEWL.Simple_Windows;
with Computer; use Computer;

procedure main is
	timeKeeper					: Timer;    	           -- will cause an instance of the timer task to exist, do not make more than one
	-- Variables
	gameOver					: Boolean  		:= False;  -- Set to true when game is over
	xFinder, yFinder			: Integer  		:= 0;
	playerTurn					: Integer 		:= 1;      -- 1 or 2 represents which player's turn it is (1-User, 2-Computer)
	playerScore, computerScore	: Integer 		:= 0;      -- Scores are 0 by default, Score of 12 wins
	spotIndex1, spotIndex2		: Integer		:= 0;
	firstClick, secondClick		: Point_Type;
	procedure nextTurn is
	begin
		if playerTurn = 1 then
			Set_Text(WhosMoveLabel,"Turn: Player 1!");
			playerTurn := 2;
		elsif playerTurn = 2 then
			Set_Text(WhosMoveLabel,"Turn: Player 2!");
			playerTurn := 1;
		end if;
	end nextTurn;
begin
   --Loop through frame
	while Valid(AppFrame) loop
		--End game button is not visible
		endGame.Hide;
		startGame.Show; --Start button is visible
		--calls JEWL functions from the gameboard package to init the GUI
		-- game wont start until S button is pressed
		loop 
			drawBoardGUI;
			case Next_Command is
				-- begin playing
				when 'S' =>
					--Begin timer
					timeKeeper.start;
					--Remove start button and display end button
					startGame.Hide;
					endGame.Show;
					exit;
				when 'Q' =>
					Close(AppFrame);
					exit;
				when others => null;
			end case;
		end loop;

		while gameOver = False loop
		--	if playerTurn = 1 then
			
				case Next_Command is
					when 'C' => -- this will happen anytime that checkerboard image is clicked
						-- initiaize these indexs
						spotIndex1 := 0; -- first click
						spotIndex2 := 0; -- second click
						while spotIndex1 = 0 loop
							xFinder   := 0; -- these two are just temp vars
							yFinder   := 0;
							firstClick := Start_Point(BoardCanvas); -- capture that stored data
							while xFinder <= firstClick.x loop --this will find the x value of the 1..64 grid coordinate
								xFinder := xFinder + 80;
								spotIndex1 := spotIndex1 + 1;
							end loop;
							while yFinder <= firstClick.y loop --this will find the y value of the 1..64 grid coordinate
								yFinder := yFinder + 80;
								if yFinder > 80 then
									spotIndex1 := spotIndex1 + 8;
								end if;
							end loop;
							-- Validation of location choice
							if spotIndex1 / 8 mod 2 = 1 and spotIndex1 mod 2 = 0 then
								if spotIndex1 mod 8 /= 0 then
									spotIndex1 := 0;
								end if;
							elsif spotIndex1 / 8 mod 2 = 0 and spotIndex1 mod 2 = 1 then
								spotIndex1 := 0;
							end if;
						end loop; --spotIndex1 now holds the array index of the piece to move
						if spotIndex2 = 0 then--This is the beginning of the second click(place to move to)
							case Next_Command is
								when 'C' => -- capture next click
									while spotIndex2 = 0 loop
										xFinder   := 0;
										yFinder   := 0;
										secondClick := Start_Point(BoardCanvas);
										while xFinder <= secondClick.x loop --this will find the x value of the 1..64 grid coordinate
											xFinder := xFinder + 80;
											spotIndex2 := spotIndex2 + 1;
										end loop;
										while yFinder <= secondClick.y loop --this will find the y value of the 1..64 grid coordinate
											yFinder := yFinder + 80;
											if yFinder > 80 then
												spotIndex2 := spotIndex2 + 8;
											end if;
										end loop;
									end loop; --spotIndex1 now holds the array index of the piece to move	
								when others => null;
							end case;
							-- validation of location choice
							if (spotIndex2 / 8) mod 2 = 1 and spotIndex2 mod 2 = 0 then
								if spotIndex2 mod 8 /= 0 then
									spotIndex2 := 0;
								end if;
							elsif (spotIndex2 / 8) mod 2 = 0 and spotIndex2 mod 2 = 1 then
								spotIndex2 := 0;
							end if;
						end if; -- spotIndex2 now holds the array index of the spot to attempt to move to
						
						if spotIndex1 / 8 mod 2 = 1 and spotIndex1 mod 8 /= 0 then
							spotIndex1 := spotIndex1 / 2 + 1; -- odd row
						else
							spotIndex1 := spotIndex1 / 2; -- even row
						end if;
						if spotIndex2 / 8 mod 2 = 1 and spotIndex2 mod 8 /= 0 then
							spotIndex2 := spotIndex2 / 2 + 1; -- odd row
						else
							spotIndex2 := spotIndex2 / 2; -- even row
						end if;
						put(Integer'Image(spotIndex1));
						put(Integer'Image(spotIndex2));
	-----------------------------------------------------------------------------------------------------------------------------------------
						if board(spotIndex1).pieceValue /= 0 AND board(spotIndex2).pieceValue = 0 then
							--make the move
							if isValidMove(spotIndex1,spotIndex2, playerTurn) = True then
								movePiece(spotIndex1, spotIndex2);
								nextTurn; -- changes playerTurn to 1 if 2, and 2 if 1
							--end if;
							--if isValidJump(spotIndex1,spotIndex2,playerTurn) = True then
							else
								isValidJump(spotIndex1,spotIndex2,playerTurn);
								nextTurn;
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
						
	-----------------------------------------------------------------------------------------------------------------------------------------
					
					when 'E' =>
						gameOver := True;
						timeKeeper.stop;
						Set_Text(WhosMoveLabel,"Game Over");
						exit;
					when others =>
						--Display scores in labels
						Set_Text(PlayerScoreResult,Integer'Image(playerScore));
						Set_Text(ComputerScoreResult,Integer'Image(computerScore));
						--Display whose turn it is
						put(Next_Command);
						exit;
				end case;
			--else
			-- computer turn to do stuff
			--null;
			--end if;

			-- Check for end game conditions
			if playerScore = 12 then
				Put_Line("Player Wins!");
				gameOver := True;
			elsif computerScore = 12 then
				Put_Line("Computer Wins!");
				gameOver := True;
			end if;
		end loop; -- main game loop
	end loop; -- frame loop
end main;
