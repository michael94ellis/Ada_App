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
					drawPiece(i, 1);
				elsif board(i).pieceValue = 3 then
					drawPiece(i, 2);
				end if;
			end if;
		end loop;
	end drawBoardGUI; -- Stop generating the initial game board
	
	
	procedure erasePiece(index: in Integer) is
	begin
		if index mod 2 = 0 and board(index).pieceValue /= 0 then
			Set_Fill(BoardCanvas,Black);
            Draw_Rectangle(BoardCanvas,board(index).point,80,80);
		end if;
	end erasePiece;
	
	
	procedure drawPiece(index: in Integer; player: in Integer) is
	point: Point_Type := board(index).point;
	procedure giveTexture is
	begin
		point.x := point.x - 1;
		point.y := point.y - 1;
		Draw_Circle(BoardCanvas,point,25);
	end giveTexture;
	begin
		--Choose which player, set the correct color: red or white
		if player = 1 or player = 2 then
			Set_Fill(BoardCanvas,Red);
		else
			Set_Fill(BoardCanvas,White);
		end if;
		--Draw the actual circle
		Draw_Circle(BoardCanvas,point,25);
		for i in 1..4 loop
			giveTexture;
		end loop;
	end drawPiece;
   
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

	function isValidMove(spot1, spot2, player: in Integer) return Boolean is
		--Function variables
		isValid: Boolean; --Value that is returned at the end of the function to tell the prgm is the attempted move is valid
	begin
		if spot1 + 4 = spot2 OR spot1 + 5 = spot2 then
			if player = 1 then
				isValid := True;
			else
				isValid := False;
			end if;
		end if;
		return isValid;
	end isValidMove;


end GameBoard;
