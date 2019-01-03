with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

procedure December_25 is

   Max_Distance : constant Natural := 3;

   package Neighbour_Sets is New Ada.Containers.Ordered_Sets (Natural);
   use Neighbour_Sets;

   type Stars is record
      X, Y, Z, T : Integer;
      Neighbour_Set : Neighbour_Sets.Set := Empty_Set;
   end record; -- Nan_Bots

   package Star_Lists is new Ada.Containers.Vectors (Index_Type => Natural,
                                                     Element_Type => Stars);
   use Star_Lists;

   package Constellations is new
     Ada.Containers.Vectors (Natural, Neighbour_Sets.Set);
   use Constellations;

   procedure Get_Input (Star_List : out Star_Lists.Vector) is

      Integer_Set : constant Character_Set := To_Set ("-0123456789");
      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Star : Stars;

   begin -- Get_Input
      Open (Input_File, In_File, "December_25.txt");
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Star.X := Integer'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Assert (Element (Text, Start_At) = ',', ", after X");
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Star.Y := Integer'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Assert (Element (Text, Start_At) = ',', ", after Y");
         Find_Token (Text, Integer_Set, Start_At, Inside, First,Last);
         Star.Z := Integer'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Assert (Element (Text, Start_At) = ',', ", after Z");
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Star.T := Integer'Value (Slice (Text, First, Last));
         Append (Star_List, Star);
      end loop; -- not End_Of_File (Input_File
      Close (Input_File);
   end Get_Input;

   function Man_Distance (Star_List : in Star_Lists.Vector;
                          Star_1, Star_2 : in Star_Lists.Cursor)
                          return Natural is
   begin -- Man_Distance
      return abs (Star_List (Star_1).X - Star_List (Star_2).X) +
      abs (Star_List (Star_1).Y - Star_List (Star_2).Y) +
      abs (Star_List (Star_1).Z - Star_List (Star_2).Z) +
      abs (Star_List (Star_1).T - Star_List (Star_2).T);
   end Man_Distance;

   Star_List : Star_Lists.Vector;
   Constellation : Constellations.Vector := Constellations.Empty_Vector;
   Constelation_Members : Neighbour_Sets.Set;
   J : Star_Lists.Cursor;

begin -- December_25
   Put_Line ("Reading Input");
   Get_Input (Star_List);
   Put_Line ("Including directly close stars");
   for I in Star_List.Iterate loop
      Include (Star_List (I).Neighbour_Set, To_Index (I));
      -- Add star as its own Neighbour
      if I /= Last (Star_List) then
         J := Next (I);
         loop -- search for stars directly in close proximity
            if Man_Distance (Star_List, I, J) <= Max_Distance then
               Include (Star_List (I).Neighbour_Set, To_Index (J));
               Include (Star_List (J).Neighbour_Set, To_Index (I));
            end if; -- Man_Distance (I, J) <= Man_Distance
            exit when J = Last (Star_List);
            Next (J);
         end loop; -- search for stars directly in close proximity
      end if; -- I /= Last (Star_List)
   end loop; -- I in Star_List.Iterate
   -- Neighbour_Set is now the set of stars directly within the specified
   -- distance of another star.
   Put_Line ("Combining constellations with at least one close star");
   for I in Star_List.Iterate loop
      for J in Star_List.Iterate loop
         if I /= J and then Overlap (Star_List (I).Neighbour_Set,
                                     Star_List (J).Neighbour_Set) then
            Star_List (I).Neighbour_Set :=
              Union (Star_List (I).Neighbour_Set,
                     Star_List (J).Neighbour_Set);
            Star_List (J).Neighbour_Set := Star_List (I).Neighbour_Set;
         end if; -- one or more stars common to the two Neighbour_sets
      end loop; -- for J in Star_List.Iterate loop
   end loop; -- I in Star_List.Iterate
   -- Neighbour_Set is now the set of all atars directly and indirectly within
   -- the specified distsnce
   Put_Line ("Counting unique constellations");
   for I in Star_List.Iterate loop
      if not Contains (Constellation, Star_List (I).Neighbour_Set) then
         Append (Constellation, Star_List (I).Neighbour_Set);
      end if; -- not Contains (Constellation, Star_List (I).Neighbour_Set)
   end loop; -- I in Star_List.Iterate
   Put_Line ("Constelations:" &
               Count_Type'Image (Length (Constellation)));
end December_25;
