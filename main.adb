--Checkers main program
with Ada.Text_IO; use Ada.Text_IO;
with GameBoard; use GameBoard;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with JEWL.Simple_Windows; use JEWL.Simple_Windows;

procedure main is
   -- Will cause an instance of the timer task to exist, do not make more than one
   timeKeeper					: Timer;
   -- Variables
   -- Set to true when game is over
   gameOver					: Boolean  		:= False;  
   -- 1 or 2 represents which player's turn it is (1-User, 2-Computer)
   playerTurn					: Integer 		:= 1;      
   spotIndex1, spotIndex2		        : Integer		:= 0;
   indexErase, canJump			        : Integer		:= 0;
	
   procedure nextTurn is	
   begin
      if playerTurn = 1 then
         Set_Text(WhosMoveLabel,"Player 2's Turn!");
         playerTurn := 2;
         InvalidMoveLabel.Hide;
      elsif playerTurn = 2 then
         Set_Text(WhosMoveLabel,"Player 1's Turn!");
         playerTurn := 1;
         InvalidMoveLabel.Hide;
      end if;
   end nextTurn;
	
   procedure givePoint is
   begin
      if playerTurn = 1 then --player 1 score ++
         player1Score := player1Score + 1;
         Set_Text(PlayerScoreResult,Integer'Image(player1Score));
      elsif playerTurn = 2 then --player 2 score ++
         player2Score := player2Score + 1;
         Set_Text(ComputerScoreResult,Integer'Image(player2Score));
      end if;
   end givePoint;
   
   -- Gets a super clean 1..32 integer click	
   function getClick return Integer is 
      spotIndex				: Integer 		:= 0;
      xFinder, yFinder		        : Integer  		:= 0;
      lastClick				: Point_Type;
   begin
      while spotIndex = 0 loop
         --These two are just temp vars
         xFinder   := 0; 
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
	
   function staleMateCheck return Boolean is
      canMove: Boolean := False;
   begin
      for i in 1..32 loop -- Strongest algorithm known to man, Brute Force
         for j in 1..32 loop
            if isValidJump(i,j,playerTurn) /= 0 or isValidMove(i,j,playerTurn) = True then
               -- A MOVE IS POSSIBLE AND NOT BEING MADE IF THIS IS EXECUTED
               canMove := True;
            end if;
         end loop;
      end loop;
      return canMove;
   end staleMateCheck;
	
begin
   --Loop through frame I.E. while a window that the program opens is still open
   while Valid(AppFrame) loop
      drawBoardGUI; -- Draw the game
      --Hide invalid move label by default
         InvalidMoveLabel.Hide;
      
      --Begin timer because the game has started
      timeKeeper.start;
      endGameButton.Show;
      while gameOver = False loop
         --	if playerTurn = 1 then
         if staleMateCheck = False then
            gameOver := True;
         end if;
         
	 --Do actions based on input while the game is running		
         case Next_Command is
         when 'C' => -- this will happen anytime that checkerboard image is clicked
            -- initiaize these indexs
            if spotIndex1 = 0 then
               spotIndex1 := getClick; -- first click
               spotIndex2 := 0;
            elsif spotIndex2 = 0 then 
               spotIndex2 := getClick; -- second click
               if board(spotIndex1).pieceValue /= 0 AND board(spotIndex2).pieceValue = 0 then  -- make the simple move
                  if isValidMove(spotIndex1,spotIndex2, playerTurn) = True then
                     canJump := 0;
                     for i in 1..32 loop
                        for j in 1..32 loop
                           if isValidJump(i,j,playerTurn) /= 0 then
                              -- A MOVE IS POSSIBLE AND NOT BEING MADE IF THIS IS EXECUTED
                              canJump := isValidJump(i,j,playerTurn);
                           else 
                              InvalidMoveLabel.Show;
                           end if;
                        end loop;
                     end loop;
                     if canJump = 0 then	
                        movePiece(spotIndex1, spotIndex2);			
                        if spotIndex2 < 5 OR spotIndex2 > 28 then
                           makeKing(spotIndex2);
                        end if;
                        nextTurn;
                     end if;
                  else --jump area
                     -- 0 if cant jump, otherwise it's the index of the piece that is jumped(to erase it)
                     indexErase := isValidJump(spotIndex1,spotIndex2,playerTurn); 
                     if indexErase /= 0 then -- then a jump is possible
                        jumpPiece(spotIndex1,spotIndex2,indexErase);
                        givePoint;
                        for j in 1..32 loop
                           if board(j).pieceValue = 0 and isValidJump(spotIndex2,j,playerTurn) /= 0 then
                              canJump := isValidJump(spotIndex2,j,playerTurn);
                              jumpPiece(spotIndex2,j,canJump);
                              givePoint;
                              spotIndex1 := spotIndex2;
                              spotIndex2 := j;
                           else --Move is invalid
                              InvalidMoveLabel.Show;
                           end if;
                        end loop;
                        
                        --If piece reaches the end of the board, make it a King piece
                        if  spotIndex2 < 5 OR spotIndex2 > 28 then
                           makeKing(spotIndex2);
                        end if;
                        nextTurn;
                     else 
                        InvalidMoveLabel.Show;
                     end if;
                  end if;
               else --Move is invalid
                  InvalidMoveLabel.Show;
               end if;
               spotIndex1 := 0; --reset the clicks for next player's turn
            end if;						
         when 'G' => 
            if Execute(RulesDialog) = 'K' then 
               RulesDialog.Hide;
            end if;
         when 'R' =>
            Show_Message("Created by Michael Ellis, Candace Allison, Kevin Lee, Trent Harrell, and Pratik Acharya.","Authors");
         when 'E' =>
            gameOver := True;
            timeKeeper.stop;
            Set_Text(WhosMoveLabel,"Game Over");
         when 'Q' =>
            Close(AppFrame);
            exit;
         when others => null; -- does nothing but is required
         end case;
         -- Check for end game conditions
         if getP1Score = 12 then
            --Put_Line("Player 1 Wins!");
            gameOver := True;
         elsif getP2Score = 12 then
            --Put_Line("Player 2 Wins!");
            gameOver := True;
         end if;
         
         --Erase gameboard on Game Over
         --Draw Game Over
         --Draw winner
         if gameOver = True then 
            Erase(BoardCanvas);
            Draw_Text(BoardCanvas,(275,250),"Game Over!");
         
            if getP1Score = 12 then 
               Draw_Text(BoardCanvas,(275,300),"Player 1 Wins!");
            elsif getP2Score = 12 then
               Draw_Text(BoardCanvas,(275,300),"Player 2 Wins!");
            else
               Draw_Text(BoardCanvas,(275,300),"It's a Draw!");
            end if;
         end if;
      end loop; -- main game loop
      timeKeeper.stop;
      exit;
   end loop; -- frame loop
end main;
