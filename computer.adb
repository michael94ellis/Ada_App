with Ada.Numerics.Discrete_Random;
with GameBoard; use GameBoard;

package body Computer is

   procedure generateMove is
      subtype pieces is Integer range 1..12;
      subtype spots is Integer range 1..32;
      package randomValue is new Ada.Numerics.Discrete_Random(pieces);
      use randomValue;
      G: Generator;
      comPieces: array(1..12) of Integer := (others => 0);
      j,location: Integer;
      pieceGen: Spot;
   begin
      --Reset Generator and set piece to random integer value between 1 and 12
      Reset(G);
      piece := Random(G);
      j := 1;
      --Find the location of the pieces that belong to the computer
      for i in 1..32 loop
         if board(i).pieceValue = 2 or board(i).pieceValue = 4 then
            comPieces(j) := board(i).location;
         end if;
      end loop;

      pieceGen := board(comPieces(piece));

      --Check if the piece is regular or king since this determines where it looks to move
      if pieceGen.pieceValue = 2 then --regular piece
         --if this is an edge piece
         if pieceGen.location = 5 or pieceGen.location = 12 or pieceGen.location = 13 or
           pieceGen.location = 20 or pieceGen.location = 21 or pieceGen.location = 28 or
           pieceGen.location = 29 then
            --Check if location 4 spots from the chosen piece to move if empty
            if board(pieceGen.location - 4).location = 0 then
               --Set this as the location to move to
               location := board(pieceGen.location - 4).location;
            end if;
         else --regular piece
            if pieceGen.location = 9 or pieceGen.location = 10 or pieceGen.location = 11 or
              pieceGen.location = 17 or pieceGen.location = 18 or pieceGen.location = 19 or
              pieceGen.location = 25 or pieceGen.location = 26 or pieceGen.location = 27 then
               --Check if there is an empty piece to the right
               if board(pieceGen.location - 3).location = 0 then
                  --Set this as location to move to
                  location := board(pieceGen.location - 3).location;
               --Check if there is an empty piece to the left
               elsif board(pieceGen.location - 4).location = 0 then
                  --Set this as location to move to
                  location := board(pieceGen.location - 4).location;
               end if;
            else --Other pieces
               --Check if there is an empty piece to the right and move there if so
               if board(pieceGen.location - 4).location = 0 then
                  location := board(pieceGen.location - 4).location;
               --Check if there is an empty piece to the left and move there if so
               elsif board(pieceGen.location - 5).location = 0 then
                  location := board(pieceGen.location - 5).location;
               end if;
            end if;
         end if;
      --elsif pieceGen.pieceValue = 4 then --King piece

      end if;
   end generateMove;


   function generatePiece return Integer is
   begin
      return piece;
   end generatePiece;


   function generateLocation return Integer is
   begin
      return location;
   end generateLocation;


end Computer;
