with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Generic_Constrained_Array_Sort;
with NT_Console;

procedure December_15 is

   Elf : constant Character := 'E';
   Goblin : constant Character := 'G';

   subtype Unit_Type is Character with Static_Predicate => Unit_Type
     in Elf | Goblin ;
   Unit_Set : constant Character_Set :=
     To_Set (Elf & Goblin);

   Field_Wall : constant Character := '#';
   Field_Open : constant Character := '.';

   subtype Field_Elements is Character with Static_Predicate => Field_Elements
     in Field_Wall | Field_Open;

   procedure Find_Limits (Input_File : in out File_Type;
                          Field_X_Size, Field_Y_Size, Unit_Count
                          : out Natural) is

      Text : Unbounded_String;

   begin -- Find_Limits
      Field_X_Size := 0;
      Field_Y_Size := 0;
      Unit_Count := 0;
      Reset (Input_File);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Field_Y_Size := Field_Y_Size + 1;
         if Length (Text) > Field_X_Size then
            Field_X_Size := Length (Text);
         end if; -- Length (Text) > Field_X_Size
         Unit_Count := Unit_Count +
           Ada.Strings.Unbounded.Count (Text, Unit_Set);
      end loop; -- not End_Of_File (Input_File
   end Find_Limits;

   procedure Solve (Input_File : in out File_Type;
                    Field_Size, Unit_Count : in Natural) is

      package Screen is new NT_Console (Field_Size, Field_Size);
      use Screen;

      subtype Coordinates is Natural range 0 .. Field_Size - 1;
      type Fields is array (Coordinates, Coordinates) of Field_Elements;

      type Units is record
         Unit : Unit_Type;
         X, Y : Coordinates;
         Destroyed : Boolean;
      end record; -- Unit_Elements

      subtype Unit_Indices is positive range 1 .. Unit_Count;

      type Unit_Arrays is array (Unit_Indices) of Units;

      function "<" (Left, Right : Units) return Boolean is

      begin -- "<"
         return Left.Y < Right.Y or else
           (Left.Y = Right.Y and then Left.X < Right.X);
      end "<";

      procedure Sort_Units is new
        Ada.Containers.Generic_Constrained_Array_Sort
          (Index_Type => Unit_Indices,
           Element_Type => Units,
           Array_Type => Units_Arrays);

      procedure Get_Input (Input_File : in out File_Type;
                           Field : out Fields;
                           Unit_Array : out Unit_Arrays) is

         Text : Unbounded_String;
         Unit_Count, Line_Count : Natural := 0;

      begin -- Get_Input
         for X in Coordinates loop
            for Y in Coordinates loop
               Field (X, Y) := Field_Wall;
            end loop; -- Y in Coordinates
         end loop; -- X in Coordinates
         Reset (Input_File);
         while not End_Of_File (Input_File) loop
            Get_Line (Input_File, Text);
            Line_Count := Line_Count + 1;
            for Xs in Positive range 1 .. Length (Text) loop
               if Element (Text, Xs) in Unit_Type then
                  Unit_Count := Unit_Count + 1;
                  Unit_Array (Unit_Count).Unit := Element (Text, Xs);
                  Unit_Array (Unit_Count).X := Xs - 1;
                  Unit_Array (Unit_Count).Y := Line_Count - 1;
                  Unit_Array (Unit_Count).Destroyed := False;
                  Field (Xs - 1, Line_Count - 1) := Field_Open;
               else
                  Field (Xs - 1, Line_Count - 1) := Element (Text, Xs);
               end if; -- Element (Text, Xs) in Units
            end loop; -- Xs in Positive range 1 .. Length (Text)
         end loop; -- not End_Of_File (Input_File)
      end Get_Input;

      procedure Put (Field : Fields; Unit_Array : Unit_Arrays) is

      begin -- Put
         Clear_Screen;
         for Y in Coordinates loop
            Goto_XY (0, Y);
            for X in Coordinates loop
               Put (Field (X, Y));
            end loop; -- X in Coordinates
         end loop; -- Y in Coordinates
         for U in Unit_Indices loop
            if not Unit_Array (U).Destroyed then
               Goto_XY (Unit_Array (U).X, Unit_Array (U).Y);
               Put (Unit_Array (U).Unit);
            end if; -- not Unit_Array (U).Destroyed
         end loop; -- U in Unit_Indices
         Goto_XY (X_Pos'Last, Y_Pos'Last);
      end Put;

      function Remaining_Units (Unit_Array : in Unit_Arrays)
                                   return Natural is

         Result : Natural := 0;

      begin -- Remaining_Units
         for I in Unit_Indices loop
            if not Unit_Array (I).Destroyed then
               Result := Result + 1;
            end if; -- not Unit_Array (I).Destroyed
         end loop; -- I in Unit_Indices
         return Result;
      end Remaining_Units;

      Field : Fields;
      Unit_Array : Unit_Arrays;

   begin -- Solve
      Get_Input (Input_File, Field, Unit_Array);
      Put (Field, Unit_Array);
   end Solve;

   Input_File : File_Type;
   Text : Unbounded_String;
   Field_X_Size, Field_Y_Size, Field_Size, Unit_Count : Natural;

begin -- December_15
   Open (Input_File, In_File, "December_15.txt");
   Find_Limits (Input_File, Field_X_Size, Field_Y_Size, Unit_Count);
   if Field_X_Size > Field_Y_Size then
      Field_Size := Field_X_Size;
   else
      Field_Size := Field_Y_Size;
   end if;
   Solve (Input_File, Field_Size, Unit_Count);
   Close (Input_File);
end December_15;
