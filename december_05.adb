with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure December_05 is

   procedure Reduce (Text : in out Unbounded_String) is

   Changes : Boolean;
   Index : Positive;

   begin -- Reduce
       loop -- Repeat Sweep
         Index := 1;
         Changes := False;
         while Index < Length (Text) loop
            if ((Is_Lower (Element (Text, Index)) and
                   Is_Upper (Element (Text, Index + 1))) and then
                Element (Text, Index) = To_Lower (Element (Text, Index + 1)))
              or else
              ((Is_Upper (Element (Text, Index)) and
                    Is_Lower (Element (Text, Index + 1))) and then
               To_Lower (Element (Text, Index)) = Element (Text, Index + 1))
            then
               Changes := True;
               Delete (Text, Index, Index + 1);
            else
               Index := Index + 1; -- only move on if no characters deleted
            end if; -- adjoining upper and lower case;
         end loop; -- Index < Length (Text)
         exit when not Changes;
      end loop; -- Repeat Sweep
   end Reduce;

   Input_File : File_Type;
   Text : Unbounded_String;
   Index : Positive;
   Minimum_Length : Positive := Positive'Last;

begin -- Dercember_05
   Open (Input_File, In_File, "December_05.txt");
   Get_line (Input_File, Text);
   Reduce (Text);
   Put_Line ("Remaining Units:" & Positive'Image (Length (Text)));
   for To_Remove in Character range 'a' .. 'z' loop
      Reset (Input_File);
      Get_line (Input_File, Text);
      Index := 1;
      while Index <= Length (Text) loop
         if To_Remove = To_Lower (Element (Text, Index)) then
            Delete (Text, Index, Index);
         else
            Index := Index + 1; -- only move on if no character deleted
         end if; -- To_Remove = To_Lower (Element (Text, Index))
      end loop; -- Index <= Length (Text)
      Reduce (Text);
      if Length (Text) < Minimum_Length then
         Minimum_Length := Length (Text);
      end if; -- Length (Text) < Minimum_Length
   end loop; -- To_Remove in Character range 'a' .. 'z'
   Put_Line ("Minimum Length (part two):" & Positive'Image (Minimum_Length));
   Close (Input_File);
end December_05;
