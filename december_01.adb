with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;

procedure December_01 is

   Input_File : File_Type;
   Total, Item : integer;

   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   package Integer_Lists is new Ada.Containers.Ordered_Sets (Integer);
   use Integer_Lists;

   Integer_List : Integer_Lists.Set := Empty_Set;
   Found : Boolean := False;

begin -- Dercember_01
   Total := 0;
   Open (Input_File, In_File, "December_01.txt");
   while not End_Of_File (Input_File) loop
      Int_IO.Get (Input_File, Item);
      Total := Total + Item;
      Skip_Line (Input_File);
   end Loop; --  not End_Of_File (Input_File)
   Put ("Total: ");
   Int_IO.Put (Total, 0);
   New_Line;
   Total := 0;
   Insert (Integer_List, Total);
   while not Found loop
      Reset (Input_File);
      while not End_Of_File (Input_File) and not Found loop
         Int_IO.Get (Input_File, Item);
         Total := Total + Item;
         Found := Contains (Integer_List, Total);
         if not Found then
            Insert (Integer_List, Total);
         end if; -- not Found
         Skip_Line (Input_File);
      end Loop; --  not End_Of_File (Input_File)
   end loop; -- not Found
   Put ("Repeated total (part two): ");
   Int_IO.Put (Total, 0);
   New_Line;
   Close (Input_File);
end December_01;
