with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_19_2 is

   Number_to_Factor : constant Natural := 10551418;
   I : Natural := 2;
   Sum_of_Factors : Natural := 1 + Number_to_Factor;

begin -- December_19_2
   Put_Line (Natural'Image (Number_to_Factor));
   while I < Number_to_Factor loop
      if Number_to_Factor mod I = 0 then
         Put_Line (Natural'Image (I));
         Sum_of_Factors := Sum_of_Factors + I;
      end if; -- Number_to_Factor
      I := I + 1;
   end loop; -- I <= Number_to_Factor
   Put_Line ("Sum of factors (part two):" & Natural'Image (Sum_of_Factors));
end December_19_2;
