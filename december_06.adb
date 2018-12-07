with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Solver_06;

procedure December_06 is

   procedure Solve (Input_File : in out File_Type;
                    Min_X, Max_X, Min_Y, Max_Y, Dangerous_Count : in Natural) is

      package Solver is new
        Solver_06 (Min_X, Max_X, Min_Y, Max_Y, Dangerous_Count);
      use Solver;

      Text : Unbounded_String;
      X, Y : Natural;
      Start_At, First : Positive;
      Last : Natural;

   begin -- Solve
      Reset (Input_File);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         X := Natural'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Start_At := Start_At + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Y := Natural'Value (Slice (Text, First, Last));
         Add_Dangerous (X, Y);
      end Loop; --  not End_Of_File (Input_File)
      Put_Line ("Largest Area:" & Natural'Image (Largest_Area));
      Put_Line ("Region Size (part two)" & Natural'Image (Distance_Count));
   end Solve;

   Input_File : File_Type;
   Text : Unbounded_String;
   Max_X, Max_Y, Dangerous_Count : Natural := 0;
   Min_X, Min_Y : Natural := Natural'Last;
   X, Y : Natural;
   Start_At, First : Positive;
   Last : Natural;


begin -- Dercember_06
   Open (Input_File, In_File, "December_06.txt");
   while not End_Of_File (Input_File) loop
      Dangerous_Count := Dangerous_Count + 1;
      Start_At := 1;
      Get_Line (Input_File, Text);
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      X := Natural'Value (Slice (Text, First, Last));
      Start_At := Last + 1;
      Assert (Element (Text, Start_At) = ',', "',' expected");
      Start_At := Start_At + 1;
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Y := Natural'Value (Slice (Text, First, Last));
      if X > Max_X then
         Max_X := X;
      end if; -- X > Max_X
      if X < Min_X then
         Min_X := X;
      end if; --  X < Min_X
      if Y > Max_Y then
         Max_Y := Y;
      end if; -- Y > Max_Y
      if Y < Min_Y then
         Min_Y := Y;
      end if; -- Y < Min_Y
   end Loop; --  not End_Of_File (Input_File)
   Solve (Input_File, Min_X, Max_X, Min_Y, Max_Y, Dangerous_Count);
   Close (Input_File);
end December_06;
