with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_22 is

   procedure Get_Input (Cave_Depth, Target_X, Target_Y : out Positive) is

      Depth_String : constant String := "depth:";
      Target_String : constant String := "target:";

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;

   begin -- Get_Input
      Open (Input_File, In_File, "December_22.txt");
      -- Open (Input_File, In_File, "Example_22.txt");
      Get_Line (Input_File, Text);
      Start_At := 1;
      Last := Index (Text, Depth_String, Start_At);
      Assert (Last > 0, Depth_String & " not found");
      Start_At := Last + Depth_String'Length;
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Cave_Depth := Positive'Value (Slice (Text, First, Last));
      Get_Line (Input_File, Text);
      Start_At := 1;
      Last := Index (Text, Target_String, Start_At);
      Assert (Last > 0, Target_String & " not found");
      Start_At := Last + Target_String'Length;
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Target_X := Positive'Value (Slice (Text, First, Last));
      Assert (Element (Text, Last + 1) = ',', ", not found");
      Start_At := Last + 1;
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Target_Y := Positive'Value (Slice (Text, First, Last));
      Close (Input_File);
   end Get_Input;

   procedure Solve (Cave_Depth, Target_X, Target_Y : in Positive) is

      Search_Limit : constant Natural := 7;
      Tool_Change : constant Matural := 7;
      -- Probably the same as the search limit but defined separately in case
      -- it makes sense to use different values.
      Margin : constant Natural := 2;
      subtype Coordinates is Natural range 0 .. Target_X + Target_Y
        + Search_Limit;
      Geo_X_Multiplier : constant Natural := 16807;
      -- Problem definition
      Geo_Y_Multiplier : constant Natural := 48271;
      -- Problem definition

      subtype Geo_Indices is Natural;
      type Erosion_Levels is mod 20183; -- Problem definition
      type Ground_Types is mod 3; -- Problem definition
      Rocky : constant Ground_Types := 0;
      Wet : constant Ground_Types := 1;
      Narrow : constant Ground_Types := 2;
      type Tools : (Climbing, Nothing, Torch);

      type Cave_Elements is record
         Geo_Index : Geo_Indices;
         Erosion_Level :  Erosion_Levels;
         Ground_Type : Ground_Types;
      end record; -- Cave_Elements

      type Caves is array (Coordinates, Coordinates) of Cave_Elements;

      procedure Initialise_Cave (Cave_Depth, Target_X, Target_Y : in Positive;
                                 Cave : out Caves) is

      begin -- Initialise_Cave
         for I in Coordinates loop
            Cave (I, 0).Geo_Index := I * Geo_X_Multiplier;
            Cave (I, 0).Erosion_Level :=
              Erosion_Levels'Mod (Cave (I, 0).Geo_Index + Cave_Depth);
              Cave (0, I).Geo_Index := I * Geo_Y_Multiplier;
            Cave (0, I).Erosion_Level :=
              Erosion_Levels'Mod (Natural (Cave (0, I).Geo_Index) + Cave_Depth);
         end loop; -- I in Coordinates
         Cave (0, 0).Geo_Index := 0; -- Problem definition
         Cave (Target_X, Target_Y).Geo_Index := 0; -- Problem definition
         for Y in Coordinates range 1 .. Coordinates'Last loop
            Put (Coordinates'Image (Y) & ": ");
            for X in Coordinates range 1 .. Y loop
               if X /= Target_X or Y - X + 1 /= Target_Y then
                  Cave (X, Y - X + 1).Geo_Index :=
                    Geo_Indices (Cave (X - 1, Y - X + 1).Erosion_Level) *
                      Geo_Indices (Cave (X, Y - X).Erosion_Level);
                  Cave (X, Y - X + 1).Erosion_Level :=
                    Erosion_Levels'Mod (Natural (Cave (X, Y - X + 1).Geo_Index)
                                        + Cave_Depth);
               end if; -- X /= Target_X or Y /= Target_Y
            end loop; -- X in Coordinates range 1 .. Y
            New_Line;
         end loop; -- Y in Coordinates range 1 .. Coordinates'Last
         for X in Coordinates loop
            for Y in Coordinates loop
               Cave (X, Y).Ground_Type :=
                 Ground_Types'Mod (Cave (X, Y).Erosion_Level);
            end loop; -- Cave (X, Y).Erosion_Level
         end loop; -- X in Coordinates
      end Initialise_Cave;

      procedure Straight_Time (Cave : in Caves;
                               Target_X, Target_Y : in Positive) is

         Tool : Tools := Torch;
         X, Y, Step_Limit : Coordinates;
         X_1, Y_1 : Coordinates := 0;
         Tool_Changes : Natural := 0;

      begin -- Straight_Time
         if Target_Y > Target_X then
            Step_limit := Target_Y;
         else
            Step_Limit := Target_X;
         end if; -- Target_Y > Target
         for I in Coordinates range 1 .. Step_Limit loop
            if Target_Y > Target_X then
               Y := I;
               X := Target_X * Y / Target_Y;
            else
               X := I;
               Y := Target_Y * X / Target_X;
            end if; -- I in Coordinates
            if
         end; -- Coordinates range 1 .. Step_Limit
         Assert (X = Target_X and Y = Target_Y,
                 "Straight did not reach target");
      end Straight_Time;

      procedure Put (Cave : in Caves) is

      begin -- Put
         for Y in Coordinates loop
            for X in Coordinates loop
               case Cave (X, Y).Ground_Type is
                  when Rocky => Put ('.');
                  when Wet => Put ('=');
                  when Narrow => Put ('|');
               end case; -- Cave (X, Y).Ground_Type
            end loop; -- X in Coordinates
            New_Line;
         end loop; -- Y in Y_Coordinate
      end Put;

      Cave : Caves;
      Risk_Level : Natural := 0;

   begin -- Solve
      Initialise_Cave (Cave_Depth, Target_X, Target_Y, Cave);
      Put (Cave);
      For X in Coordinates range 0 .. Target_X loop
         for Y in Coordinates range 0 .. Target_Y loop
            Risk_Level := Risk_Level + Natural (Cave (X, Y).Ground_Type);
         end loop; -- Y in Coordinates range 0 .. Target_Y
      end loop; -- X in Coordinates range 0 .. Target_X
      Put_Line ("Risc level:" & Natural'Image (Risk_Level));
   end Solve;

   Cave_Depth, Target_X, Target_Y : Positive;

begin -- December_22
   Get_Input (Cave_Depth, Target_X, Target_Y);
   Solve (Cave_Depth, Target_X, Target_Y);
end December_22;
