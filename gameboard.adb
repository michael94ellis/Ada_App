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
					board(num).point := (BoardLocation.X + 40,BoardLocation.Y + 40);
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
				drawPiece(BoardCanvas,board(i).point,board(i).pieceValue);
			end if;
		end loop;
	end drawBoardGUI; -- Stop generating the initial game board

	procedure erasePiece(BoardCanvas: in out Canvas_Type; index: in Integer) is
	xFinder,yFinder: Integer := 0;
	tempPoint: Point_Type := board(index).point;
	begin
		if index mod 2 = 0 and board(index).pieceValue /= 0 then
			Set_Fill(BoardCanvas,Black);
			tempPoint.x := tempPoint.x - 40;
			tempPoint.y := tempPoint.y - 40;
            Draw_Rectangle(BoardCanvas,BoardLocation,80,80);
		end if;
	end erasePiece;
	
	
	procedure drawPiece(BoardCanvas: in out Canvas_Type; pointTemp: in Point_Type; player: in Integer) is
	point: Point_Type := pointTemp;
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

   function isValidMove(piece, location: Integer) return Boolean is
      --Function variables
      isValid: Boolean; --Value that is returned at the end of the function to tell the prgm is the attempted move is valid
   begin
      --Set removeChecker to zero
      removeChecker := 0;

      --Check if move location is empty
      if board(location).pieceValue = 0 then
         --Location is empty
         --If player piece
         if board(piece).pieceValue = 1 then --Player piece
            if location <= board(piece).location then
               isValid := False;
            else
               --Check for special cases (these are the spots along the side of the board)
               if board(piece).location = 4 or board(piece).location = 5 or board(piece).location = 12
                 or board(piece).location = 13 or board(piece).location = 20 or board(piece).location = 21
                 or board(piece).location = 28 or board(piece).location = 29 then
                  --Making a single move
                  if location = board(piece).location + 4 then
                     isValid := True;
                     --Jumping over another piece from an edge piece
                  elsif (location = board(piece).location + 7 and (board(piece + 4).pieceValue = 2 or board(piece + 4).pieceValue = 4)) or
                    (location = board(piece).location + 9 and (board(piece + 4).pieceValue = 2 or board(piece + 4).pieceValue = 4)) then --Jump over piece
                     --Remove jumped over piece
                     board(piece + 4).pieceValue := 0;

                     --Remove jumped over piece in GUI
                     removeChecker := piece + 4;
                     isValid := True;
                  else
                     isValid := False;
                  end if;
               else
                  --Making a single move
                  if location = board(piece).location + 3 or location = board(piece).location + 4 or
                    location = board(piece).location + 5 or location = board(piece).location + 6 then
                     isValid := True;
                     --Jumping over another piece - left diagonal
                  elsif location = board(piece).location + 7 then --Jump over piece
                     if location = 9 or location = 10 or location = 11 or location = 12 or location = 17 or
                       location = 18 or location = 19 or location = 20 or location = 25 or location = 26 or
                       location = 27 or location = 28 then
                        --Make sure the piece being jumped over is an enemy piece
                        if board(location - 3).pieceValue = 2 or board(location - 3).pieceValue = 4 then
                           --Remove jumped over piece
                           board(location - 3).pieceValue := 0;

                           --Remove jumped over piece in GUI
                           removeChecker := location - 3;
                           isValid := True;
                        else
                           isValid := False;
                        end if;
                     else
                        --Make sure piece being jumped over is enemy piece
                        if board(location - 4).pieceValue = 2 or board(location - 4).pieceValue = 4 then
                           --Remove jumped over piece
                           board(location - 4).pieceValue := 0;

                           --Remove jumped over piece in GUI
                           removeChecker := location - 4;
                           isValid := True;
                        else
                           isValid := False;
                        end if;
                     end if;
                     --Jumping over another piece - right diagonal
                  elsif location = board(piece).location + 9 then --Jump over piece
                     if location = 1 or location = 2 or location = 3 or location = 4 or location = 9 or
                       location = 10 or location = 11 or location = 12 or location = 17 or location = 18 or
                       location = 19 or location = 20 or location = 25 or location = 26 or location = 27 or
                       location = 28 then
                        --Make sure piece being jumped over is enemy piece
                        if board(location - 4).pieceValue = 2 or board(location - 4).pieceValue = 4 then
                           --Remove piece
                           board(location - 4).pieceValue := 0;

                           --Remove jumped over piece in GUI
                           removeChecker := location - 4;
                           isValid := True;
                        else
                           isValid := False;
                        end if;
                     else
                        --Make sure it is enemy piece
                        if board(location - 5).pieceValue = 2 or board(location - 5).pieceValue = 4 then
                           --Remove piece
                           board(location - 5).pieceValue := 0;

                           --Remove jumped over piece in GUI
                           removeChecker := location - 5;
                           isValid := True;
                        else
                           isValid := False;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         else --Computer piece
            --If location to move to is >= the piece's current location, return false
            if location >= board(piece).location then
               isValid := False;
            else -- > piece's current location
                 --Check for special cases (these are the spots along the side of the board)
               if board(piece).location = 4 or board(piece).location = 5 or board(piece).location = 12
                 or board(piece).location = 13 or board(piece).location = 20 or board(piece).location = 21
                 or board(piece).location = 28 or board(piece).location = 29 then
                  --Making a single move
                  if location = board(piece).location - 4 then
                     isValid := True;
                     --Jumping over another piece from an edge piece
                  elsif (location = board(piece).location - 7 and (board(piece - 4).pieceValue = 1 or board(piece - 4).pieceValue = 3)) or
                    (location = board(piece).location - 9 and (board(piece - 4).pieceValue = 1 or board(piece - 4).pieceValue = 3)) then --Jump over piece
                     --Remove jumped over piece
                     board(piece - 4).pieceValue := 0;

                     --Remove jumped over piece in GUI
                     removeChecker := piece - 4;
                     isValid := True;
                  else
                     isValid := False;
                  end if;
               else --Normal spots
                    --Making a single move
                  if location = board(piece).location - 3 or location = board(piece).location - 4 or
                    location = board(piece).location - 5 or location = board(piece).location - 6 then
                     isValid := True;
                     --Jumping over another piece - right diagonal
                  elsif location = board(piece).location - 7 then --Jump over piece
                     if location = 9 or location = 10 or location = 11 or location = 12 or location = 17 or
                       location = 18 or location = 19 or location = 20 or location = 25 or location = 26 or
                       location = 27 or location = 28 then
                        --Make sure the piece being jumped over is an enemy piece
                        if board(location + 4).pieceValue = 1 or board(location + 4).pieceValue = 3 then
                           --Remove jumped over piece
                           board(location + 4).pieceValue := 0;

                           --Remove jumped over piece in GUI
                           removeChecker := location + 4;
                           isValid := True;
                        else
                           isValid := False;
                        end if;
                     else
                        if board(location + 3).pieceValue = 1 or board(location + 3).pieceValue = 3 then
                           --Remove jumped over piece
                           board(location + 3).pieceValue := 0;

                           --Remove jumped over piece in GUI
                           removeChecker := location + 3;
                           isValid := True;
                        else
                           isValid := False;
                        end if;
                     end if;
                  --Jumping over another piece - left diagonal
                  elsif location = board(piece).location - 9 then --Jump over piece
                     if location = 1 or location = 2 or location = 3 or location = 4 or location = 9 or
                       location = 10 or location = 11 or location = 12 or location = 17 or location = 18 or
                       location = 19 or location = 25 or location = 26 or location = 27 or location = 28 then

                        if board(location + 5).pieceValue = 1 or board(location + 5).pieceValue = 3 then
                           --Remove piece
                           board(location + 5).pieceValue := 0;

                           --Remove jumped over piece in GUI
                           removeChecker := location + 5;
                           isValid := True;
                        else
                           isValid := False;
                        end if;
                     else
                        --Make sure piece being jumped over is enemy piece
                        if board(location + 4).pieceValue = 1 or board(location + 4).pieceValue = 3 then
                           --Remove piece
                           board(location + 4).pieceValue := 0;

                           --Remove jumped over piece in GUI
                           removeChecker := location + 4;
                           isValid := True;
                        else
                           isValid := False;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      else
         isValid := False;
      end if;

      return isValid;
   end isValidMove;

   function returnRemovedValue return Integer is
   begin
      return removeChecker;
   end returnRemovedValue;
end GameBoard;
