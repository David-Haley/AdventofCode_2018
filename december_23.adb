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

   Xc, Yc, Zc : Natural; -- Global variable used by R_C_Order

   function R_C_Order (Left, Right : Nano_Bots) return Boolean is

      -- Ordering based on distance that signal from Xc, Yc, Zc global

   begin -- R_C_Order
      return abs (Left.X - Xc ) + abs (Left.Y - Yc) + abs (Left.Z - Zc) - Left.R
        <
      abs (Right.X - Xc) + abs (Right.Y - Yc) + abs (Right.Z - Zc) - Right.R;
   end R_C_Order;

   package Range_C_Sorts is new
     Nano_Bot_Lists.Generic_Sorting ("<" => R_C_Order);

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

   function Man_Distance (Nano_Bot_List : in Nano_Bot_Lists.Vector;
                          Bot : in Nano_Bot_Lists.Cursor;
                         X, Y, Z : in Integer) return Natural is
   begin -- Man_Distance
      return Abs (Nano_Bot_List (Bot).X - X) +
      Abs (Nano_Bot_List (Bot).Y - Y) +
      Abs (Nano_Bot_List (Bot).Z - Z);
   end Man_Distance;

   procedure Centroid (Nano_Bot_List : in Nano_Bot_Lists.Vector;
                       X, Y, Z : out Integer) is

      Xc, Yc, Zc : Long_Long_Integer := 0;

   begin -- Centroid
      Xc := 0;
      Yc := 0;
      Zc := 0;
      for I in Nano_Bot_List.Iterate loop
         Xc := Xc + Long_Long_Integer (Nano_Bot_List (I).X);
         Yc := Yc + Long_Long_Integer (Nano_Bot_List (I).Y);
         Zc := Zc + Long_Long_Integer (Nano_Bot_List (I).Z);
      end loop; -- I in Nano_Bot_List.Iterate
      X := Natural (Xc / Long_Long_Integer (Length (Nano_Bot_List)));
      Y := Natural (Yc / Long_Long_Integer (Length (Nano_Bot_List)));
      Z := Natural (Zc / Long_Long_Integer (Length (Nano_Bot_List)));
   end Centroid;

   Function Count_In_Range (Nano_Bot_List : in Nano_Bot_Lists.Vector;
                            X, Y, Z : in Integer) return Natural is

      Result : Natural := 0;

   begin -- Count_In_Range
   for I in Nano_Bot_List.Iterate loop
      if Man_Distance (Nano_Bot_List, I, X, Y, Z) <=
        Nano_Bot_List (I).R then
            Result := Result + 1;
         end if; -- in range
      end loop;
      return Result;
   end Count_In_Range;

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
   end loop; -- I in Nano_Bot_List.Iterate
   Put_Line ("In range nanobots:" & Natural'Image (Nano_Bot_Count));
   Centroid (Nano_Bot_List, Xc, Yc, Zc);
   Put_Line ("Count in range of (" & Integer'Image (Xc) & Integer'Image (Yc) &
               Integer'Image (Zc) & "):" &
               Natural'Image (Count_In_Range (Nano_Bot_List, Xc, Yc, Zc)));
   Range_C_Sorts.Sort (Nano_Bot_List);
   Assert (Range_C_Sorts.Is_Sorted (Nano_Bot_List), "not sorted range from C");
   for I in Nano_Bot_List.Iterate loop
      if Man_Distance (Nano_Bot_List, I, Xc, Yc, Zc) <=
        Nano_Bot_List (I).R then
         Put ("In Range: ");
      else
         Put ("Out of Range:");
      end if; -- I in Nano_Bot_List.Iterate
      Put_Line (Natural'Image (Man_Distance (Nano_Bot_List, I, Xc, Yc, Zc)
                - Nano_Bot_List (I).R) & " (" &
                  Integer'Image (Nano_Bot_List (I).X) & "," &
                  Integer'Image (Nano_Bot_List (I).Y) & "," &
                  Integer'Image (Nano_Bot_List (I).Z) & ")");
   end loop; -- I in Nano_Bot_List.Iterate
end December_23;
