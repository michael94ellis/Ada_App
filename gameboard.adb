--Checkers Board package func/procedure definitions
with Ada.Text_IO; use Ada.Text_IO;
package body GameBoard is
	procedure drawBoardGUI is 
	num							: Integer			:= 0;
	isBlack						: Boolean			:= False;
	begin
             --Generate Game Board, best possible method for stack use? 
	for i in 1..8 loop
		for j in 1..8 loop
            if i mod 2 = 0 then
				if j mod 2 = 0 then
					Set_Fill(BoardCanvas,Red);
					isBlack := False;
				else
					Set_Fill(BoardCanvas,Black);
					num := num + 1;
					isBlack := True;
				end if;
            else
				if j mod 2 = 0 then
					Set_Fill(BoardCanvas,Black);
					num := num + 1;
					isBlack := True;
				else
					Set_Fill(BoardCanvas,Red);
					isBlack := False;
				end if;
            end if;
            --Draw each checker square
            Draw_Rectangle(BoardCanvas,BoardLocation,80,80);
            if isBlack = True then
				if num <= 12 then
					board(num).pieceValue := 1;
				elsif num >= 21 and num <= 32 then
					board(num).pieceValue := 3;
				else
					board(num).pieceValue := 0;
				end if;
				board(num).location := num;
				board(num).point 	:= (BoardLocation.X + 40,BoardLocation.Y + 40);
            end if;
            --Move along x-axis
            BoardLocation := (BoardLocation.X + 80,BoardLocation.Y);
			end loop;
			--Move down y-axis
			BoardLocation := (0,BoardLocation.Y + 80);
		end loop;
		--Draw circles based on gameBoard checkers
		for i in 1..32 loop
			--Player pieces will be Red, Computer pieces will be White
			if board(i).pieceValue /= 0 then
				if board(i).pieceValue = 1 then
					drawPiece(i);
				elsif board(i).pieceValue = 3 then
					drawPiece(i);
				end if;
			end if;
		end loop;
	end drawBoardGUI; -- Stop generating the initial game board
	
	
	procedure erasePiece(index: in Integer) is
	begin
		Set_Fill(BoardCanvas,Black);
        Draw_Rectangle(BoardCanvas,(board(index).point.x-40,board(index).point.y-40),80,80,(0,0));
	end erasePiece;
	
	
	procedure drawPiece(index: in Integer) is
	point: Point_Type := board(index).point;
	procedure giveTexture is
	begin
		point.x := point.x - 1;
		point.y := point.y - 1;
		Draw_Circle(BoardCanvas,point,25);
	end giveTexture;
	begin
		--Choose which player, set the correct color: red or white
		if board(index).pieceValue = 1 OR board(index).pieceValue = 2 then
			Set_Fill(BoardCanvas,Red);
		elsif board(index).pieceValue = 3 OR board(index).pieceValue = 4 then
			Set_Fill(BoardCanvas,White);
		else
			Set_Fill(BoardCanvas,Blue);
		end if;
		--Draw the actual circle
		Draw_Circle(BoardCanvas,point,25);
		for i in 1..4 loop
			giveTexture;
		end loop;
	end drawPiece;
	
	procedure movePiece(index1, index2: in Integer) is
	begin
		board(index2).pieceValue := board(index1).pieceValue;
		board(index1).pieceValue := 0;
		erasePiece(index1);
		drawPiece(index2);
	end movePiece;
	
	
	task body Timer is
		min,sec: Integer := 0;
	begin -- this begins running at run time on a separate thread for each instance of this task type
		accept start;
			loop
				select -- Select statement is just magic somehow for entry points
					accept stop;
						exit; -- exit the timer loop
					ELSE
						sec := sec + 1;
						if sec > 60 then
							sec := 0;
							min := min + 1;
						end if;
						delay 1.0;
						-- set text to appropriate labels and cast to string
						set_text(Timer_Label_Sec, Integer'Image(sec));
						set_text(Timer_Label_Min, Integer'Image(min));
				end select;
			end loop;
	end Timer;
	
	procedure makeKing(newKing: Integer) is
	begin
		if board(newKing).pieceValue = 1 then
			board(newKing).pieceValue := 3;
		elsif board(newKing).pieceValue = 2 then
			board(newKing).pieceValue := 4;
		end if;
	end makeKing;
	
	procedure isValidJump(spot1, spot2, player: in Integer) is
		isValid: Boolean := False;
		indexErase: Integer := 0;
	begin
		
		if player = 1 then		
			if board(spot1).pieceValue = 1 or board(spot1).pieceValue = 2 then
				case spot1 mod 4 is
					when 1 => 						
						if spot1 / 4 mod 2 = 1 then
							--5,13,21,29
							if spot1 + 9 = spot2 then
								if board(spot1+4).pieceValue = 3 or board(spot1+4).pieceValue = 4 then
									isValid := True;
									indexErase := spot1+4;
								end if;
							end if;
						else
							--1,9,17,25
							if spot1 + 9 = spot2 then
								if board(spot1+5).pieceValue = 3 or board(spot1+5).pieceValue = 4 then
									isValid := True;
									indexErase := spot1+5;
								end if;
							end if;
						end if;
					when 2 => 
						if spot1 / 4 mod 2 = 1 then
							--6,14,22,30
							if spot1 + 9 = spot2 then
								-- jump left
								if board(spot1+4).pieceValue = 3 or board(spot1+4).pieceValue = 4 then
									isValid := True;
									indexErase := spot1+4;
								end if;
							elsif spot1 + 7 = spot2 then
								-- jump right
								if board(spot1+3).pieceValue = 3 or board(spot1+3).pieceValue = 4 then
									isValid := True;
									indexErase := spot1+3;
								end if;
							end if;
						else
							--2,10,18,26
							if spot1 + 9 = spot2 then
								-- jump left
								if board(spot1+5).pieceValue = 3 or board(spot1+5).pieceValue = 4 then
									isValid := True;
									indexErase := spot1+4;
								end if;
							elsif spot1 + 7 = spot2 then
								-- jump right
								if board(spot1+4).pieceValue = 3 or board(spot1+4).pieceValue = 4 then
									isValid := True;
									indexErase := spot1+4;
								end if;
							end if;
						end if;
					when 3 => 
						if spot1 / 4 mod 2 = 1 then
							--7,15,23,31
							if spot1 + 9 = spot2 then
								if board(spot1+4).pieceValue = 3 or board(spot1+4).pieceValue = 4 then
									isValid := True;
									indexErase := spot1+4;
								end if;
							elsif spot1 + 7 = spot2 then
								if board(spot1+3).pieceValue = 3 or board(spot1+3).pieceValue = 4 then
									isValid := True;
									indexErase := spot1+3;
								end if;
							end if;
						else
							--3,11,19,27
							if spot1 + 9 = spot2 then
								if board(spot1+5).pieceValue = 3 or board(spot1+5).pieceValue = 4 then
									isValid := True;
									indexErase := spot1+5;
								end if;
							elsif spot1 + 7 = spot2 then
								if board(spot1+4).pieceValue = 3 or board(spot1+4).pieceValue = 4 then
									isValid := True;
									indexErase := spot1+4;
								end if;
							end if;
						end if;
					when 0 => 
						if spot1 / 4 mod 2 = 1 then
							--4,12,20,28
							if spot1 + 7 = spot2 then
								if board(spot1+4).pieceValue = 3 or board(spot1+4).pieceValue = 4 then
									isValid := True;
									indexErase := spot1+4;
								end if;
							end if;
						else
							--8,16,24,32
							if spot1 + 7 = spot2 then
								if board(spot1+3).pieceValue = 3 or board(spot1+3).pieceValue = 4 then
									isValid := True;
									indexErase := spot1+3;
								end if;
							end if;
						end if;
					when others => null;
				end case;
			end if;
		elsif player = 2 then
			if board(spot1).pieceValue = 3 or board(spot1).pieceValue = 4 then
				case spot1 mod 4 is
					when 0 => 
						if spot1 / 4 mod 2 = 1 then
							--8,16,24,32
							if spot1 - 9 = spot2 then
								if board(spot1-5).pieceValue = 1 or board(spot1-5).pieceValue = 2 then
									isValid := True;
									indexErase := spot1-5;
								end if;
							end if;
						else
							--4,12,20,28
							if spot1 - 9 = spot2 then
								if board(spot1-5).pieceValue = 1 or board(spot1-5).pieceValue = 2 then
									isValid := True;
									indexErase := spot1-5;
								end if;
							end if;
						end if;
					when 1 => 
						
						if spot1 / 4 mod 2 = 1 then
							--5,13,21,29
							if spot1 - 7 = spot2 then
								if board(spot1-4).pieceValue = 1 or board(spot1-4).pieceValue = 2 then
									isValid := True;
									indexErase := spot1-4;
								end if;
							end if;
						else
							--1,9,17,25
							if spot1 - 7 = spot2 then
								if board(spot1-4).pieceValue = 1 or board(spot1-4).pieceValue = 2 then
									isValid := True;
									indexErase := spot1-4;
								end if;
							end if;
						end if;
					when 2 => 
						if spot1 / 4 mod 2 = 1 then
							--6,14,22,30
							if spot1 - 9 = spot2 then
								if board(spot1-5).pieceValue = 1 or board(spot1-5).pieceValue = 2 then
									isValid := True;
									indexErase := spot1-5;
								end if;
							elsif spot1 - 7 = spot2 then
								if board(spot1-4).pieceValue = 1 or board(spot1-4).pieceValue = 2 then
									isValid := True;
									indexErase := spot1-4;
								end if;
							end if;
						else
							--2,10,18,26
							if spot1 - 9 = spot2 then
								if board(spot1-4).pieceValue = 1 or board(spot1-4).pieceValue = 2 then
									isValid := True;
									indexErase := spot1-4;
								end if;
							elsif spot1 - 7 = spot2 then
								if board(spot1-3).pieceValue = 1 or board(spot1-3).pieceValue = 2 then
									isValid := True;
									indexErase := spot1-3;
								end if;
							end if;
						end if;
					when 3 => 
						if spot1 / 4 mod 2 = 1 then
							--7,15,23,31
							if spot1 - 9 = spot2 then
								if board(spot1-5).pieceValue = 1 or board(spot1-5).pieceValue = 2 then
									isValid := True;
									indexErase := spot1-5;
								end if;
							elsif spot1 - 7 = spot2 then
								if board(spot1-4).pieceValue = 1 or board(spot1-4).pieceValue = 2 then
									isValid := True;
									indexErase := spot1-4;
								end if;
							end if;
						else
							--3,11,19,27
							if spot1 - 9 = spot2 then
								if board(spot1-4).pieceValue = 1 or board(spot1-4).pieceValue = 2 then
									isValid := True;
									indexErase := spot1-4;
								end if;
							elsif spot1 - 7 = spot2 then
								if board(spot1-3).pieceValue = 1 or board(spot1-3).pieceValue = 2 then
									isValid := True;
									indexErase := spot1-3;
								end if;
							end if;
						end if;
					when others => null;
				end case;
			end if;
		end if;
		if isValid then
			board(spot2).pieceValue := board(spot1).pieceValue;
			board(spot1).pieceValue := 0;
			board(indexErase).pieceValue := 0;
			erasePiece(spot1);
			erasePiece(indexErase);
			drawPiece(spot2);
		end if;
	end isValidJump;
	
	
	function isValidMove(spot1, spot2, player: in Integer) return Boolean is
		--Function variables
		isValid: Boolean := False; --Value that is returned at the end of the function to tell the prgm is the attempted move is valid
	begin
		if player = 1 then		
			if board(spot1).pieceValue = 1 or board(spot1).pieceValue = 2 then
				case spot1 mod 4 is
					when 1 => 
					-- 1,5,9,13,17..
						if spot1 / 4 mod 2 = 1 then
							--5,13,21,29
							if spot1 + 4 = spot2 then
								-- does not check if there is a piece there
								isValid := True;
							end if;
						else
							--1,9,17,25
							if spot1 + 4 = spot2 or spot1 + 5 = spot2 then
								isValid := True;
							end if;
						end if;
					when 2 => 
						if spot1 / 4 mod 2 = 1 then
							--6,14,22,30
							if spot1 + 3 = spot2 or spot1 + 4 = spot2 then
								-- does not check if there is a piece there
								isValid := True;
							end if;
						else
							--2,10,18,26
							if spot1 + 4 = spot2 or spot1 + 5 = spot2 then
								isValid := True;
							end if;
						end if;
					when 3 => 
						if spot1 / 4 mod 2 = 1 then
							--7,15,23,31
							if spot1 + 3 = spot2 or spot1 + 4 = spot2 then
								-- does not check if there is a piece there
								isValid := True;
							end if;
						else
							--3,11,19,27
							if spot1 + 4 = spot2 or spot1 + 5 = spot2 then
								isValid := True;
							end if;
						end if;
					when 0 => 
						if spot1 / 4 mod 2 = 1 or spot1 mod 4 = 0 then
							--8,16,24,32
							if spot1 + 3 = spot2 or spot1 + 4 = spot2 then
								isValid := True;
							end if;
						else
							--4,12,20,28
							if spot1 + 4 = spot2 then
								isValid := True;
							end if;
						end if;
					when others => null;
				end case;
			end if;
		elsif player = 2 then
			if board(spot1).pieceValue = 3 or board(spot1).pieceValue = 4 then
				case spot1 mod 4 is
					when 0 => 
						if spot1 / 4 mod 2 = 1 or spot1 mod 4 = 0 then
							--8,16,24,32
							if spot1 - 4 = spot2 or spot1 - 5 = spot2 then
								isValid := True;
							end if;
						else
							--4,12,20,28
							if spot1 - 4 = spot2 then
								isValid := True;
							end if;
						end if;
					when 1 => 
					-- 1,5,9,13,17..
						if spot1 / 4 mod 2 = 1 then
							--5,13,21,29
							if spot1 - 4 = spot2 then
								-- does not check if there is a piece there
								isValid := True;
							end if;
						else
							--1,9,17,25
							if spot1 - 3 = spot2 or spot1 - 4 = spot2 then
								isValid := True;
							end if;
						end if;
					
					when 2 => 
						if spot1 / 4 mod 2 = 1 then
							--6,14,22,30
							if spot1 - 4 = spot2 or spot1 - 5 = spot2 then
								-- does not check if there is a piece there
								isValid := True;
							end if;
						else
							--2,10,18,26
							if spot1 - 3 = spot2 or spot1 - 4 = spot2 then
								isValid := True;
							end if;
						end if;
					when 3 => 
						if spot1 / 4 mod 2 = 1 then
							--7,15,23,31
							if spot1 - 4 = spot2 or spot1 - 5 = spot2 then
								-- does not check if there is a piece there
								isValid := True;
							end if;
						else
							--3,11,19,27
							if spot1 - 3 = spot2 or spot1 - 4 = spot2 then
								isValid := True;
							end if;
						end if;
					when others => null;
				end case;
			end if;
		end if;
		return isValid;
	end isValidMove;


end GameBoard;
