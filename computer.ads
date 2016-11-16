with Ada.Numerics.Discrete_Random;

package Computer is

   piece: Integer;
   location: Integer;

   procedure generateMove;
   function generatePiece return Integer;
   function generateLocation return Integer;

end Computer;
