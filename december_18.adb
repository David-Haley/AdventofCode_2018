with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_18 is

   Lumberyard : constant Character := '#';
   Open : constant Character := '.';
   Trees : constant Character := '|';

   subtype Field_Elements is Character with Static_Predicate => Field_Elements
     in Lumberyard | Open | Trees;

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
      end loop; -- not End_Of_File (Input_File
   end Find_Limits;

   procedure Solve (Input_File : in out File_Type; Field_Size : in Natural) is

      subtype Coordinates is Natural range 0 .. Field_Size - 1;
      type Fields is array (Coordinates, Coordinates) of Field_Elements;

      procedure Get_Input (Input_File : in out File_Type;
                           Field : out Fields) is

         Text : Unbounded_String;
         Line_Count : Natural := 0;

      begin -- Get_Input
         Reset (Input_File);
         while not End_Of_File (Input_File) loop
            Get_Line (Input_File, Text);
            Line_Count := Line_Count + 1;
            for Xs in Positive range 1 .. Length (Text) loop
               Field (Xs - 1, Line_Count - 1) := Element (Text, Xs);
            end loop; -- Xs in Positive range 1 .. Length (Text)
         end loop; -- not End_Of_File (Input_File)
      end Get_Input;

      procedure Count (Field : in Fields;
                       N_Trees, N_Lumberyards : out Natural) is
      begin -- Count
         N_Trees := 0;
         N_Lumberyards := 0;
         for Y in Coordinates loop
            for X in Coordinates loop
               if Field (X, Y) = Trees then
                  N_Trees := N_Trees + 1;
               elsif Field (X, Y) = Lumberyard then
                  N_Lumberyards := N_Lumberyards + 1;
               end if; -- Field (X, Y) = Tree
            end loop; -- X in Coordinates
         end loop; -- Y in Coordinates
      end Count;

      procedure Update (Field : in out Fields) is

         subtype Neighbour_Counts is Natural range 0 .. 8;

         procedure Count (Field : in Fields; X0, Y0 : in Coordinates;
                          N_Trees, N_Lumberyards : out Neighbour_Counts) is

            subtype Offsets is Integer range -1 .. 1;

         begin -- Count
            N_Trees := 0;
            N_Lumberyards := 0;
            for X in Offsets loop
               for Y in Offsets loop
                  if X0 + X in Coordinates and Y0 + Y in Coordinates
                    and (X /= 0 or Y /= 0) then
                     if Field (X0 + X, Y0 + Y) = Trees then
                        N_Trees := N_Trees + 1;
                     elsif Field (X0 + X, Y0 + Y) = Lumberyard then
                        N_Lumberyards := N_Lumberyards + 1;
                     end if; -- Field (X0 + X, Y0 + Y) = Trees
                  end if; -- valid coordinates
               end loop; -- X in Offsets
            end loop; -- Y in Offset
         end Count;

         Future_Field : Fields := Field;
         N_Trees, N_lumberyards : Neighbour_Counts;

      begin -- Update
         for Y in Coordinates loop
            for X in Coordinates loop
               Count (Field, X, Y, N_Trees, N_lumberyards);
               if Field (X, Y) = Open and N_Trees >= 3 then
                  Future_Field (X, Y) := Trees;
               end if; -- Field (X, Y) = Open and N_Trees >= 3

               if Field (X, Y) = Trees and N_lumberyards >= 3 then
                  Future_Field (X, Y) := Lumberyard;
               end if; -- Field (X, Y) = Trees and N_Trees >= 3
               if Field (X, Y) = Lumberyard then
                  if N_Trees = 0 or N_lumberyards = 0 then
                     Future_Field (X, Y) := Open;
                  end if; -- not (N_Trees >= 1 and N_lumberyards >= 1)
               end if; -- Field (X, Y) = Lumberyard
            end loop; -- X in Coordinates
         end loop; -- Y in Coordinates
         Field := Future_Field;
      end Update;

      procedure Put (Field : Fields) is

      begin -- Put
         for Y in Coordinates loop
            for X in Coordinates loop
               Put (Field (X, Y));
            end loop; -- X in Coordinates
            New_Line;
         end loop; -- Y in Coordinates
      end Put;

      Field : Fields;
      N_Trees, N_lumberyards : Natural;

   begin -- Solve
      Get_Input (Input_File, Field);
      Put (Field);
      for I in Positive range 1 .. 10 loop
         Update (Field);
         Put_Line ("Minute:" & Positive'Image (I));
         Put (Field);
      end loop; -- I in Positive range 1 .. 10
      Count (Field, N_Trees, N_lumberyards);
      Put_Line ("Product of trees and lumberyards:" &
                  Natural'Image (N_Trees * N_lumberyards));
      Get_Input (Input_File, Field);
      Put (Field);
      for I in Positive range 1 .. 1000 loop
         Update (Field);
         Put (Field);
         Count (Field, N_Trees, N_lumberyards);
         Put_Line ("Minute:" & Positive'Image (I) &
                     "Product of trees and lumberyards:" &
                     Natural'Image (N_Trees * N_lumberyards));
      end loop; -- I in Positive range 1 .. 1000
      Count (Field, N_Trees, N_lumberyards);
   end Solve;

   Input_File : File_Type;
   Text : Unbounded_String;
   Field_X_Size, Field_Y_Size, Field_Size, Unit_Count : Natural;

begin -- December_18
   Ada.Text_IO.Open (Input_File, In_File, "December_18.txt");
   -- Ada.Text_IO.Open (Input_File, In_File, "Example_18.txt");
   Find_Limits (Input_File, Field_X_Size, Field_Y_Size, Unit_Count);
   if Field_X_Size > Field_Y_Size then
      Field_Size := Field_X_Size;
   else
      Field_Size := Field_Y_Size;
   end if;
   Solve (Input_File, Field_Size);
   Close (Input_File);
end December_18;
