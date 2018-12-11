with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with NT_Console;

procedure December_11 is

   package Natural_IO is new Ada.Text_IO.Integer_IO (Natural);

   subtype Coordinates is Positive range 1 .. 300;
   type Fuel_Cells is array (Coordinates, Coordinates) of Integer;

   procedure Initialise (Grid_Serial_Number : in Natural;
                         Fuel_Cell : out Fuel_Cells) is

      Rack_ID : Natural;

   begin -- Initialise
      for X in Coordinates loop
         Rack_ID := X + 10;
         for Y in Coordinates loop
            Fuel_Cell (X, Y) := (Rack_ID * Y + Grid_Serial_Number) * Rack_ID;
            Fuel_Cell (X, Y) := (Fuel_Cell (X, Y) / 100) mod 10 - 5;
         end loop; -- Y in Coordinates
      end loop; -- X in Coordinates
   end Initialise;

   procedure Find_Maximum (Fuel_Cell : in Fuel_Cells;
                           X_Max, Y_Max : out Coordinates;
                           Maximum : out Integer;
                           Size : in Positive := 3) is

      subtype Offsets is Natural range 0 .. Size - 1;

      Sum : Integer;

   begin -- Find_Maximum
      Maximum := Integer'First;
      for X in Coordinates range
        Coordinates'First .. Coordinates'Last - Offsets'Last loop
         for Y in Coordinates range
           Coordinates'First .. Coordinates'Last - Offsets'Last loop
            Sum := 0;
            for Ox in Offsets loop
               for Oy in offsets loop
                  Sum := Sum + Fuel_Cell (X + Ox, Y + Oy);
               end loop; -- Oy in offsets
            end loop; -- Ox in Offsets
            if Sum > Maximum then
               Maximum := Sum;
               X_Max := X;
               Y_Max := Y;
            end if; -- Sum > Maximum
         end loop; -- Y
      end loop; -- X
   end Find_Maximum;

   Input_File : File_Type;
   Grid_Serial_Number : Natural;
   Fuel_Cell : Fuel_Cells;
   X, Y, Saved_X, Saved_Y : Coordinates;
   Maximum : Integer;
   Saved_Maximum : Integer := Integer'First;
   Saved_Size : Positive;

begin -- Dercember_10
   Open (Input_File, In_File, "December_11.txt");
   Natural_IO.Get (Input_File, Grid_Serial_Number);
   Close (Input_File);
   Put_Line ("Grid Serial Number:" & Natural'Image (Grid_Serial_Number));
   Initialise (Grid_Serial_Number, Fuel_Cell);
   Find_Maximum (Fuel_Cell, X, Y, Maximum);
   Put_Line ("Fuel Cell Coordinates:" & Coordinates'Image (X) & "," &
               Coordinates'Image (Y));
   for Size in Positive range Positive'First .. Coordinates'Last loop
      Find_Maximum (Fuel_Cell, X, Y, Maximum, Size);
      if Maximum > Saved_Maximum then
         Saved_Maximum := Maximum;
         Saved_X := X;
         Saved_Y := Y;
         Saved_Size := Size;
      end if; -- Maximum > Saved_Maximum
   end loop; -- Size in Positive range Positive'First .. Coordinates'Last
   Put_Line ("Fuel Cell Coordinates:" & Coordinates'Image (Saved_X) & "," &
      Coordinates'Image (Saved_Y) & "," & Positive'Image (Saved_Size));
end December_11;
