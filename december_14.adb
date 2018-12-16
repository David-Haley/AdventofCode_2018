with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_14 is

   subtype Scores is Natural range 0 .. 9;
   type Score_Elements;
   type Score_Pointers is access Score_Elements;
   type Score_Elements is record
      Clockwise, Anticlockwise : Score_Pointers;
      Score : Scores;
   end record; -- Score_Elements

   type Control_Blocks is record
      Scoreboard_Start, Scoreboard_End : Score_Pointers;
      Current_Recipe_1, Current_Recipe_2 : Score_Pointers;
      Recipe_Count : Natural;
      Trial_Recipies : Positive;
      Trial_Pointer : Score_Pointers;
   end record; -- Control_Blocks

   procedure Append (Control_Block : in out Control_Blocks;
                     Score : in Scores) is

      Temp : Score_Pointers;

   begin -- Append
      Temp := new Score_Elements;
      Temp.Score := Score;
      Temp.Anticlockwise := Control_Block.Scoreboard_End;
      Temp.Clockwise := Control_Block.Scoreboard_End.Clockwise;
      -- setting temp's pointers
      Temp.Anticlockwise.Clockwise := Temp;
      Temp.Clockwise.Anticlockwise := Temp;
      -- set adjoining pointers
      Control_Block.Scoreboard_End := Temp;
      Control_Block.Recipe_Count := Control_Block.Recipe_Count + 1;
      if Control_Block.Recipe_Count = Control_Block.Trial_Recipies + 1 then
         Control_Block.Trial_Pointer := Control_Block.Scoreboard_End;
      end if; -- Control_Block.Recipe_Count = Control_Block.Trial_Recipies
   end Append;

   procedure Create_Scoreboard (Control_Block : in out Control_Blocks;
                               Trial_Recipies : in Positive) is

   begin -- Create_Scoreboard
      Control_Block.Scoreboard_Start := new Score_Elements;
      Control_Block.Scoreboard_End := Control_Block.Scoreboard_Start;
      Control_Block.Scoreboard_Start.Clockwise :=
        Control_Block.Scoreboard_Start;
      Control_Block.Scoreboard_Start.Anticlockwise :=
        Control_Block.Scoreboard_Start;
      -- value below is from the problem definition, not individual input
      Control_Block.Scoreboard_Start.Score := 3;
      Control_Block.Current_Recipe_1 := Control_Block.Scoreboard_Start;
      Control_Block.Recipe_Count := 1;
      -- value below is from the problem definition, not individual input
      Append (Control_Block, 7);
      Control_Block.Current_Recipe_2 := Control_Block.Scoreboard_End;
      Control_Block.Trial_Recipies := Trial_Recipies;
      Control_Block.Trial_Pointer := null;
   end Create_Scoreboard;

   procedure Put (Control_Block : in Control_Blocks) is

      Current : Score_Pointers := Control_Block.Scoreboard_Start;

   begin -- Put
      loop
         Put (Scores'Image (Current.Score));
         Current := Current.Clockwise;
         exit when Current = Control_Block.Scoreboard_Start;
      end loop; -- Current /= Control_Block.Scoreboard_End
      New_Line;
   end Put;

   procedure Do_Trial (Control_Block : in out Control_Blocks) is

      Elf_1_Score, Elf_2_Score : Scores;
      Sum_of_Scores : Natural;

   begin -- Do_Trial
      Sum_of_Scores := Control_Block.Current_Recipe_1.Score +
        Control_Block.Current_Recipe_2.Score;
      Elf_1_Score := Sum_of_Scores mod 10;
      Elf_2_Score := Sum_of_Scores / 10;
      if Elf_2_Score > 0 then
         Append (Control_Block, Elf_2_Score);
      end if;
      Append (Control_Block, Elf_1_Score);
      for I in Positive range 1 .. Control_Block.Current_Recipe_1.Score + 1 loop
         Control_Block.Current_Recipe_1 :=
           Control_Block.Current_Recipe_1.Clockwise;
      end loop; -- steps forward for Elf 1
      for I in Positive range 1 .. Control_Block.Current_Recipe_2.Score + 1 loop
         Control_Block.Current_Recipe_2 :=
           Control_Block.Current_Recipe_2.Clockwise;
      end loop; -- steps forward for Elf 2
   end Do_Trial;

   function Last_10 (Control_Block : in Control_Blocks)
                     return Unbounded_String is

      Result : Unbounded_String := Null_Unbounded_String;
      Current : Score_Pointers := Control_Block.Trial_Pointer;

   begin -- Last_10
      for I in Positive range 1 .. 10 loop
         Result := Result &
           Delete (To_Unbounded_String (Scores'Image (Current.Score)), 1, 1);
         Current := Current.Clockwise;
      end loop; -- I in Positive range 1 .. 10
      return Result;
   end Last_10;

   function Find_Input_Sequence (Control_Block : in out Control_Blocks;
                       Text : in Unbounded_String) return Natural is

      Result : Natural := 0;
      Test_String : Unbounded_String := Null_Unbounded_String;
      Current : Score_Pointers := Control_Block.Scoreboard_Start;

   begin -- Find_Input_Sequence
      while Control_Block.Recipe_Count < Length (Text) + 2 loop
         Do_Trial (Control_Block);
      end loop; -- Control_Block.Recipe_Count <= Length (Text)
      -- Ensures at least length + 2 scores are available
      for I in Positive range 1 .. Length (Text) loop
         Test_String := Test_String &
           Delete (To_Unbounded_String (Scores'Image (Current.Score)), 1, 1);
         Current := Current.Clockwise;
      end loop; -- Length (Test_String) > Length (Text)
      while Test_String /= Text loop
         while Control_Block.Recipe_Count < Result + Length (Text) + 2 loop
            Do_Trial (Control_Block);
         end loop; -- Control_Block.Recipe_Count < Result + Length (Text)
         Test_String := Delete (Test_String, 1, 1);
         Test_String := Test_String &
           Delete (To_Unbounded_String (Scores'Image (Current.Score)), 1, 1);
         Result := Result + 1;
         Current := Current.Clockwise;
         assert (Result < Control_Block.Recipe_Count, "Not Found");
         assert (Current /= Control_Block.Scoreboard_Start, "Wrapped around");
      end loop; -- Test_String /= Text
      return Result;
   end Find_Input_Sequence;

   Input_File : File_Type;
   Text : Unbounded_String;
   Start_At, First : Positive;
   Last : Natural;
   Trial_Recipies : Positive;
   Part_1, Part_2 : Control_Blocks;

begin -- December_14
   Open (Input_File, In_File, "December_14.txt");
   Get_Line (Input_File, Text);
   Close (Input_File);
   Start_At := 1;
   Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
   Trial_Recipies := Positive'Value (Slice (Text, First, Last));
   Put_Line ("Trial recipies:" & Positive'Image (Trial_Recipies));
   Create_Scoreboard (Part_1, Trial_Recipies);
   while Part_1.Recipe_Count < Part_1.Trial_Recipies + 10 loop
      Do_Trial (Part_1);
   end loop; -- Part_1.Recipe_Count < Part_1.Trial_Recipies + 10
   Put_Line ("Last ten: " & Last_10 (Part_1));
   Create_Scoreboard (Part_2, Natural'Last - 1);
   Put_Line ("Recipies to left (part two):" &
               Natural'Image (Find_Input_Sequence (Part_2, Text)));
end December_14;
