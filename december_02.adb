with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;

procedure December_02 is

   Input_File : File_Type;
   Text : Unbounded_String;
   Set : Character_Set;
   Two_Count, Three_Count : Natural := 0;
   Contains_Two, Contains_Three : Boolean;
   subtype Lower_Case_Letters is Character range 'a' .. 'z';

   package String_Sets is new Ada.Containers.Ordered_Sets (Unbounded_String);
   use String_Sets;

   String_Set : String_Sets.Set := Empty_Set;
   I, J : String_Sets.Cursor;
   Search_String : Unbounded_String;
   Found : Boolean := False;

   function Delete (Text : in Unbounded_String;
                    Index : in Positive) return Unbounded_String is

   begin -- Delete
      if Index = 1 then
         return Unbounded_Slice (Text, Index + 1, Length (Text));
      elsif Index = Length (Text) then
         return Unbounded_Slice (Text, 1, Length (Text) - 1);
      else
         return Unbounded_Slice (Text, 1, Index - 1) &
           Unbounded_Slice (Text, Index + 1, Length (Text));
      end if; -- Index = 1
   end Delete;

begin -- Dercember_02
   Open (Input_File, In_File, "December_02.txt");
   while not End_Of_File (Input_File) loop
      Get_Line (Input_File, Text);
      Insert (String_Set, Text); -- used in part two
      Contains_Two := False;
      Contains_Three := False;
      for I in Lower_Case_Letters loop
         Set := To_Set (I);
         Contains_Two := Contains_Two or
           Ada.Strings.Unbounded.Count (Text, Set) = 2;
         Contains_Three := Contains_Three or
           Ada.Strings.Unbounded.Count (Text, Set) = 3;
      end loop; -- I in Lower_Case_Letters
      if Contains_Two then
         Two_Count := Two_Count + 1;
      end if; -- Contains_Two
      if Contains_Three then
         Three_Count := Three_Count + 1;
      end if; -- Contains_Three
   end Loop; --  not End_Of_File (Input_File)
   Close (Input_File);
   Put_Line ("Checksum: " & Natural'Image (Two_Count * Three_Count));
   I := First (String_Set);
   while I /= Last (String_Set) and not Found loop
      J := Next (I);
      loop
         for I_Delete in Positive range 1 ..
           Length (String_Set (I)) loop
            Search_String := Delete (String_Set (I), I_Delete);
            Found := Search_String = Delete (String_Set (J), I_Delete);
            exit when Found;
         end loop; -- I_Delete
         exit when J = Last (String_Set) or Found;
         Next (J);
      end loop; -- not Found
      Next (I);
   end loop; -- I /= Last (String_Set)
   if Found then
      Put_Line ("Matching box label (part two):" & Search_String);
   else
      Put_Line ("Not found (part two)");
   end if; -- Found
end December_02;
