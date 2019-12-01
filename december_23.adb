with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_23 is

   type Nano_Bots is record
      X, Y, Z : Integer;
      R : Natural;
      Neighbours : Natural := 0;
   end record; -- Nan_Bots

   package Nano_Bot_Lists is new Ada.Containers.Vectors (Index_Type => Natural,
                                                     Element_Type => Nano_Bots);
   use Nano_Bot_Lists;

   function R_Order (Left, Right : Nano_Bots) return Boolean is

   begin -- R_Order
      return Left.R < Right.R;
   end R_Order;

   package Range_Sorts is new
     Nano_Bot_Lists.Generic_Sorting ("<" => R_Order);

   function Neighbour_Order (Left, Right : Nano_Bots) return Boolean is

      -- sort by neighbour count and then range closest to origin

   begin -- Neighbour_Order
      return Left.Neighbours < Right.Neighbours or else
        (Left.Neighbours = Right.Neighbours and
         abs (Left.X) + abs (Left.Y) + abs (Left.Z) - Left.R <
         abs (Right.X) + abs (Right.Y) + abs (Right.Z) - Right.R);
   end Neighbour_Order;

   package Neighbour_Sorts is new
     Nano_Bot_Lists.Generic_Sorting ("<" => Neighbour_Order);

   procedure Get_Input (Nano_Bot_List : out Nano_Bot_Lists.Vector) is

      Pos_String : constant String := "pos=<";
      R_string : constant String := "r=";
      Integer_Set : constant Character_Set := To_Set ("-0123456789");
      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Nano_Bot : Nano_Bots;

   begin -- Get_Input
      Open (Input_File, In_File, "December_23.txt");
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Start_At := Index (Text, Pos_String, Start_At) +
           Pos_String'Length;
         Assert (Start_At > Pos_String'Length, Pos_String & " not found");
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Nano_Bot.X := Integer'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Assert (Element (Text, Start_At) = ',', ", after X");
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Nano_Bot.Y := Integer'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Assert (Element (Text, Start_At) = ',', ", after X");
         Find_Token (Text, Integer_Set, Start_At, Inside, First,Last);
         Nano_Bot.Z := Integer'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Assert (Element (Text, Start_At) = '>', "> after Z");
         Start_At := Index (Text, R_String, Start_At) +
           R_String'Length;
         Assert (Start_At > Pos_String'Length, R_String & " not found");
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Nano_Bot.R := Natural'Value (Slice (Text, First, Last));
         Append (Nano_Bot_List, Nano_Bot);
      end loop; -- not End_Of_File (Input_File
      Close (Input_File);
   end Get_Input;

   function Man_Distance (Nano_Bot_List : in Nano_Bot_Lists.Vector;
                          Bot_1, Bot_2 : in Nano_Bot_Lists.Cursor)
                          return Natural is
   begin -- Man_Distance
      return Abs (Nano_Bot_List (Bot_1).X - Nano_Bot_List (Bot_2).X) +
      Abs (Nano_Bot_List (Bot_1).Y - Nano_Bot_List (Bot_2).Y) +
      Abs (Nano_Bot_List (Bot_1).Z - Nano_Bot_List (Bot_2).Z);
   end Man_Distance;

--     function Man_Distance (Nano_Bot_List : in Nano_Bot_Lists.Vector;
--                            Bot : in Nano_Bot_Lists.Cursor;
--                           X, Y, Z : in Integer) return Natural is
--     begin -- Man_Distance
--        return Abs (Nano_Bot_List (Bot).X - X) +
--        Abs (Nano_Bot_List (Bot).Y - Y) +
--        Abs (Nano_Bot_List (Bot).Z - Z);
--     end Man_Distance;

--     Function Count_In_Range (Nano_Bot_List : in Nano_Bot_Lists.Vector;
--                              X, Y, Z : in Integer) return Natural is
--
--        Result : Natural := 0;
--
--     begin -- Count_In_Range
--        for I in Nano_Bot_List.Iterate loop
--           if Man_Distance (Nano_Bot_List, I, X, Y, Z) <=
--             Nano_Bot_List (I).R then
--              Result := Result + 1;
--           end if; -- in range
--        end loop; -- I in Nano_Bot_List.Iterate
--        return Result;
--     end Count_In_Range;

   Function Count_In_Range (Nano_Bot_List : in Nano_Bot_Lists.Vector;
                            Bot : in Nano_Bot_Lists.Cursor) return Natural is

      Result : Natural := 0;

   begin -- Count_In_Range
      for I in Nano_Bot_List.Iterate loop
         if Man_Distance (Nano_Bot_List, I, Bot) <=
           Nano_Bot_List (I).R + Nano_Bot_List (Bot).R and I /= Bot then
            Result := Result + 1;
         end if; -- in range
      end loop; -- I in Nano_Bot_List.Iterate
      return Result;
   end Count_In_Range;

--     procedure Iterate_Man_Sphere (Radius : in Natural;
--                                  Nano_Bot_List : in Nano_Bot_Lists.Vector) is
--
--     begin -- Iterate_Man_Sphere
--        for X in Integer range - Radius .. Radius loop
--           for Y in Integer range - Radius + abs (X) .. Radius - abs (X) loop
--              for Z in Integer range - Radius + abs (X) + abs (Y) ..
--                Radius - abs (X) - abs (Y) loop
--                 Put_Line (Integer'Image (X) & Integer'Image (Y) &
--                             Integer'Image (Z) &
--                             Natural'Image (Count_In_Range (Nano_Bot_List,
--                             X, Y, Z)));
--              end loop; -- Z in Integer range ...
--           end loop; -- Y in Integer range ...
--        end loop; -- X in Integer range - Radius .. Radius
--     end Iterate_Man_Sphere;

   procedure Intersect (Nano_Bot_List : in Nano_Bot_Lists.Vector;
                        Bot_1, Bot_2 : in Nano_Bot_Lists.Cursor;
                        X, Y, Z : out Integer) is

   begin -- Intersect

   end Intersect;

   procedure Least_Overlap (Nano_Bot_List : in Nano_Bot_Lists.Vector) is
      -- assumes list is sorted least to most neighbours

      Last_Bot : Nano_Bot_Lists.Cursor := Last (Nano_Bot_List);
      Max_Neighbours : constant Natural :=
        Nano_Bot_List (Last (Nano_Bot_List)).Neighbours;
      Min_Overlap : Natural := Natural'Last;
      Min_Overlap_Bot : Nano_Bot_Lists.Cursor;

   begin -- Least_Overlap
      while Nano_Bot_List (Last_Bot).Neighbours = Max_Neighbours loop
         for I in Nano_Bot_List.Iterate loop
            if Man_Distance (Nano_Bot_List, I, Last_Bot) <=
              Nano_Bot_List (I).R + Nano_Bot_List (Last_Bot).R and
              I /= Last_Bot then
               if Nano_Bot_List (I).R + Nano_Bot_List (Last_Bot).R -
                 Man_Distance (Nano_Bot_List, I, Last_Bot) < Min_Overlap then
                  Min_Overlap := Nano_Bot_List (I).R +
                    Nano_Bot_List (Last_Bot).R -
                    Man_Distance (Nano_Bot_List, I, Last_Bot);
                  Min_Overlap_Bot := Last_Bot;
               end if; -- smaller overlap
            end if; -- in range
         end loop; -- I in Nano_Bot_List.Iterate
         Previous (Last_Bot);
      end loop; -- Nano_Bot_List (Last_Bot) = Max_Neighbours
      Put_Line ("Least Overlap:" & Natural'Image (Min_Overlap));
      for I in Nano_Bot_List.Iterate loop
         if Nano_Bot_List (I).R + Nano_Bot_List (Min_Overlap_Bot).R -
           Man_Distance (Nano_Bot_List, I , Min_Overlap_Bot) = Min_Overlap
           and I /= Min_Overlap_Bot then
            Put_Line ("(" & Integer'Image (Nano_Bot_List (I).X) & "," &
                        Integer'Image (Nano_Bot_List (I).Y) & "," &
                        Integer'Image (Nano_Bot_List (I).Z) & ")" &
                        Natural'Image (Nano_Bot_List (I).R) &
                        Natural'Image (Nano_Bot_List (I).Neighbours));
            Put_Line ("Distance to origin: " &
                        Natural'Image (abs (Nano_Bot_List (Min_Overlap_Bot).X) +
                        abs (Nano_Bot_List (Min_Overlap_Bot).Y) +
                        abs (Nano_Bot_List (Min_Overlap_Bot).Z) -
                        (Nano_Bot_List (Min_Overlap_Bot).R - Min_Overlap)));
         end if; -- ... = Min_Overlap
      end loop; -- I in Nano_Bot_List.Iterate
   end Least_Overlap;

   Nano_Bot_List : Nano_Bot_Lists.Vector;
   Last_Bot : Nano_Bot_Lists.Cursor;
   Nano_Bot_Count : Natural := 0;

begin -- December_23
   Get_Input (Nano_Bot_List);
   Range_Sorts.Sort (Nano_Bot_List);
   Assert (Range_Sorts.Is_Sorted (Nano_Bot_List), "not sorted");
   Last_Bot := Last (Nano_Bot_List);
   for I in Nano_Bot_List.Iterate loop
      if Man_Distance (Nano_Bot_List, Last_Bot, I) <=
        Nano_Bot_List (Last_Bot).R then
         Nano_Bot_Count := Nano_Bot_Count + 1;
      end if; -- in range
      Nano_Bot_List (I).Neighbours := Count_In_Range (Nano_Bot_List, I);
   end loop; -- I in Nano_Bot_List.Iterate
   Put_Line ("In range nanobots:" & Natural'Image (Nano_Bot_Count));
   Neighbour_Sorts.Sort (Nano_Bot_List);
   Assert (Neighbour_Sorts.Is_Sorted (Nano_Bot_List), "not sorted");
--     for I in Nano_Bot_List.Iterate loop
--        Put_Line ("(" & Integer'Image (Nano_Bot_List (I).X) & "," &
--                    Integer'Image (Nano_Bot_List (I).Y) & "," &
--                    Integer'Image (Nano_Bot_List (I).Z) & ")" &
--                    Natural'Image (Nano_Bot_List (I).R) &
--                    Natural'Image (Nano_Bot_List (I).Neighbours));
--     end loop; -- I in Nano_Bot_List.Iterate
   Least_Overlap (Nano_Bot_List);
end December_23;
