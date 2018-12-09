with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_09 is

   function Winner (Number_of_Elves : in Positive;
                    Marbles : in Positive) return Long_Long_Integer is


      subtype Marble_Numbers is Positive range 1 .. Marbles;
      subtype Marble_Counts is Natural range 0 .. Marbles;

      type Circle_Elements;
      type Circle_Pointers is access Circle_Elements;
      type Circle_Elements is record
         Marble : Marble_Counts;
         Clockwise, Anticlockwise : Circle_Pointers;
      end record; -- Circle_Elements
      type Control_Blocks is record
         Circle_0, Current_Position : Circle_Pointers;
         Marble_Count : Marble_Counts;
      end record; -- Control_Blocks

      procedure Create_Circle (Control_Block : in out Control_Blocks) is

      begin -- Create_Circle
         Control_Block.Circle_0 := new Circle_Elements;
         Control_Block.Current_Position := Control_Block.Circle_0;
         Control_Block.Circle_0.Marble := 0;
         Control_Block.Circle_0.Clockwise := Control_Block.Circle_0;
         Control_Block.Circle_0.Anticlockwise := Control_Block.Circle_0;
         Control_Block.Marble_Count := 0;
      end Create_Circle;

      procedure Insert (Control_Block : in out Control_Blocks;
                        Marble : in Marble_Numbers;
                        Score : out Long_Long_Integer) is

         Temp : Circle_Pointers;

      begin -- Insert
         Temp := new Circle_Elements;
         Temp.Marble := Marble;
         if Marble mod 23 /= 0 then
            Control_Block.Current_Position :=
              Control_Block.Current_Position.Clockwise;
            Temp.Anticlockwise := Control_Block.Current_Position;
            Temp.Clockwise := Control_Block.Current_Position.Clockwise;
            -- setting temp's pointers
            Temp.Anticlockwise.Clockwise := Temp;
            Temp.Clockwise.Anticlockwise := Temp;
            -- set adjoining pointers
            Control_Block.Current_Position := Temp;
            Control_Block.Marble_Count := Control_Block.Marble_Count + 1;
            Score := 0;
         else
            Temp := Control_Block.Current_Position;
            for I in Positive range 1 .. 7 loop
               Temp := Temp.Anticlockwise;
            end loop; -- I in Positive range 1 .. 7
            -- unlink Temp, no deallocation
            Temp.Anticlockwise.Clockwise := Temp.Clockwise;
            Temp.Clockwise.Anticlockwise := Temp.Anticlockwise;
            Score := Long_Long_Integer (Marble + Temp.Marble);
            Control_Block.Current_Position := Temp.Clockwise;
            Control_Block.Marble_Count := Control_Block.Marble_Count - 1;
         end if; -- Marble mod 23 = 0
      end Insert;

      subtype Elf_Indices is Positive range 1 .. Number_Of_Elves;
      type Elf_Scores is array (Elf_Indices) of Long_Long_Integer;

      Elf_Score : Elf_Scores := (others => 0);
      Elf_Index : Elf_Indices := 1;

      Control_Block : Control_Blocks;
      Score : Long_Long_Integer;

   begin -- Winner
      Create_Circle (Control_Block);
      for Marble in Marble_Numbers loop
         Insert (Control_Block, Marble, Score);
         Elf_Score (Elf_Index) := Elf_Score (Elf_Index) +
           Long_Long_Integer (Score);
         if Elf_Index < Elf_Indices'Last then
            Elf_Index := Elf_Index + 1;
         else
            Elf_Index := Elf_Indices'First;
         end if; -- Elf_Index < Elf_Indices'Last
      end loop;
      Score := 0;
      for I in Elf_Indices loop
         if Elf_Score (I) > Score then
            Score := Elf_Score (I);
         end if; -- Elf_Score (I) > Score
      end loop; -- I in Elf_Indices
      return Score;
   end Winner;

   Input_File : File_Type;
   Text : Unbounded_String;
   Start_At, First : Positive;
   Last : Natural;
   Elves, Marbles : Positive;

begin -- Dercember_09
   Open (Input_File, In_File, "December_09.txt");
   Get_Line (Input_File, Text);
   Close (Input_File);
   Start_At := 1;
   Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
   Elves := Positive'Value (Slice (Text, First, Last));
   Start_At := Last + 1;
   Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
   Marbles := Positive'Value (Slice (Text, First, Last));
   Put_Line (Natural'Image (Natural'Last));
   Put_Line ("Elves:" & Positive'Image (Elves) & " Marbles:" &
               Positive'Image (Marbles));
   Put_Line ("Winning Score:" &
               Long_Long_Integer'Image (Winner (Elves, Marbles)));
   Marbles := Marbles * 100;
   Put_Line ("Elves:" & Positive'Image (Elves) & " Marbles:" &
               Positive'Image (Marbles));
   Put_Line ("Winning Score (part two):" &
               Long_Long_Integer'Image (Winner (Elves, Marbles)));
end December_09;
