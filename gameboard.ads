with JEWL.Simple_Windows; use JEWL.Simple_Windows;

--Define the game piece object
package GameBoard is
   -- GUI VARIABLES
   --Declare a frame of 1000 x 1000 pixels
   AppFrame					: Frame_Type 		:= Frame(1000,750,"Ada - Checkers",'Q');
   --Declare a point-type
   BoardLocation				: Point_Type 		:= (0,0);
   --Declare a canvas
   BoardCanvas					: Canvas_Type 		:= Canvas(AppFrame,BoardLocation,640,640,'C');
   --Declare buttons
   endGameButton				: Button_Type 		:= Button(AppFrame,(760,25),100,50,"End Game",'E');
   --Declare a panel to hold scores in
   PlayerScorePanel                             : Panel_Type            := Panel(AppFrame,(710,200),200,160,"Scores");
   --Declare labels for player/p2 (called computer) scores
   PlayerScorePrompt			        : Label_Type 		:= Label(AppFrame,(730,250),100,50,"Player 1 Score:",Left);
   PlayerScoreResult			        : Label_Type 		:= Label(AppFrame,(840,250),50,50,"0",Left);
   ComputerScorePrompt			        : Label_Type 		:= Label(AppFrame,(730,300),100,50,"Player 2 Score:",Left);
   ComputerScoreResult			        : Label_Type 		:= Label(AppFrame,(840,300),50,50,"0",Left);
   --Declare labels for who's move it is
   WhosMoveLabel				: Label_Type 		:= Label(AppFrame,(700,400),200,50,"Player 1 starts the game.",Center);
   --Declare a label to show if a move is invalid or not
   InvalidMoveLabel                             : Label_Type            := Label(AppFrame,(750,450),100,50,"Invalid Move",Center);
   --Declare a basic menu
   MenuFile					: Menu_Type 		:= Menu(AppFrame,"File");
   MenuAbout					: Menu_Type 		:= Menu(AppFrame,"About");
   File_Exit					: Menuitem_Type 	:= Menuitem(MenuFile,"Exit",'Q');
   About_GameRules                              : Menuitem_Type         := Menuitem(MenuAbout,"Rules",'G');
   About_Credit                                 : Menuitem_Type         := Menuitem(MenuAbout,"Credit",'R');
   --Create Dialog Windows for the Game Rules
   RulesDialog                                  : Dialog_Type           := Dialog(400,500,"Game Rules",'D');
   ---------------------------
   --Rules Dialog Components--
   ---------------------------
   RulesLabel                                   : Label_Type            := Label(RulesDialog,(40,20),300,25,"THE RULES OF THE GAME:",Center);
   Rules1Label                                  : Label_Type            := Label(RulesDialog,(40,60),300,150,"Each player starts out with 12 pieces. " &
                                                                                   "Player 1 has red pieces and Player 2 has white pieces. Player 1 goes first.",Center);
   Rules2Label                                  : Label_Type            := Label(RulesDialog,(40,120),300,150,"Every player must make a move before the next player can" &
                                                                                   " have their turn. If a jump is possible, it MUST be done. The game will refuse all " &
                                                                                   "moves until this jump has been performed.",Center);
   Rules3Label                                  : Label_Type            := Label(RulesDialog,(40,200),300,150,"The goal of the game is for each player to get to the opposite " &
                                                                                   "side of the board to crown a King. Kings are not restricted to two diagonals like regular pieces" &
                                                                                   " - Kings can move in all 4 directions.",Center);
   Rules4Label                                  : Label_Type            := Label(RulesDialog,(40,290),300,150,"To win the game a Player must earn 12 points by jumping over all of " &
                                                                                   "their opponent's pieces. Whichever player reaches 12 points first wins the game. If the game is " &
                                                                                   "ended early, it is a draw by default.",Center);
   RulesButton                                  : Button_Type           := Button(RulesDialog,(140,400),100,50,"OK",'K');

   --Labels for Timer
   TimerLabel                                   : Label_Type            := Label(AppFrame,(760,125),50,20,"Timer:",Left);
   TimerColon                                   : Label_Type            := Label(AppFrame,(830,125),20,20,":",Left);
   Timer_Label_Min    			        : Label_Type   		:= Label(AppFrame,(805,125),20,20,"0",Left);
   Timer_Label_Sec    			        : Label_Type	        := Label(AppFrame,(840,125),20,20,"0",Left);

   -- This will cause the board and pieces to be drawn on the GUI
   procedure drawBoardGUI;
   procedure erasePiece(index: in Integer);
   procedure drawPiece(index:in Integer);
   procedure movePiece(index1, index2:in Integer);
   procedure jumpPiece(index1, index2, indexErase:in Integer);
   ----------------------------------------------------
   ----------------------------------------------------
   --------- The non GUI board components  ------------
   ----------------------------------------------------
   ----------------------------------------------------
   -- Timer Task definition
   task type Timer is -- can put entry points and stuff in here
      entry start;
      entry stop;
   end Timer;
   -- need to add entry points for like the individual player's total move time clock/counts
   -- need to add entry to start and stop the main/only timer

   --Create record type
   type Spot is record
      --Player 1 -> User -> Black Team -> 1/2 (2 for king)
      --Player 2 -> Computer -> Red Team -> 3/4 (4 for king)
      pieceValue: Integer range 0..4;
      --Value representing numerical location
      location: Integer range 1..32; --Matches index location of array
      --Value representing the (X,Y) coordinates where the checker is on the canvas
      point: Point_Type;
   end record;
   --Board Variable of 32 Spots
   board: array (1..32) of Spot;
   --Declare player scores
   player1Score, player2Score: Integer := 0;
   --Set up game board procedure
   procedure makeKing(newKing: Integer);
   function isValidJump(spot1, spot2, player: in Integer) return Integer;
   function isValidMove(spot1, spot2, player: in Integer) return Boolean;
   --procedure isValidJump
   --function isValidKingMove
   function getP1Score return Integer;
   function getP2Score return Integer;
end GameBoard;
