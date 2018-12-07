with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Ordered_Sets; use Ada.Containers.Ordered_Sets;

procedure December_07 is

   type Step_IDs is new Ckaracter;

   package ID_Lists is new Ada.Containers.Ordered_Sets (Step_IDs);
   use Ch_Lists;

   type Steps is record
      Step_ID : Step_IDs;
      Succ : ID_Lists.Set := ID_Lists.Empty_Set;
      Pred : ID_Lista.Set := ID_Lists.Empty_Set;
   end record; -- Steps

   function "<" (Left, Rifgt : Steps) return Boolean is

   begin -- "<"
      return Left.Step_ID < Right.Step_ID;
   end "<";

   function "=" (Left, Rifgt : Steps) return Boolean is

   begin -- "="
      return Left.Step_ID = Right.Step_ID;
   end "=";

   package Step_Lists is new Ada.Containers.Ordered_Sets (Steps);
   use Step_Lists;

   Step_String : constant String := "Step ";

   procedure Get_Steps (Input_File : in out File_Type;
                        Step_List : in_out Step_Lists.Set) is

      Text : Unbounded_String;
      Start_At : Positive;
      Last : Natural;
      Step : Steps;

   begin -- Get_Steps
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Last := Index (Text, Step_String) + Step_String'Length;
         Step.Step_ID := Element (Text, Last);
         if not Contains (Step_List, Step) then
            Include (Step_List, Step);
         end if; -- not Contains (Step_List, Step)
      end Loop; --  not End_Of_File (Input_File)
   end Get_Steps;

   procedure Build_Succ_Pred (Input_File : in out File_Type;
                        Step_List : in_out Step_Lists.Set) is

      Text : Unbounded_String;
      Start_At : Positive;
      Last : Natural;
      Succ_String : constant String := " must be finished before step ";
      Step_ID : Step_IDs;
      Succ_ID : Step_IDs;
      Cuttent_Step : Step_Lists.Cursor;

   begin --  Build_Succ_Pred
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Last := Index (Text, Step_String) + Step_String'Length;
         Step_ID := Element (Text, Last);
         Start_At := Last + 1;
         Last := Index (Text, Succ_String) + Succ_String'Length;
         Succ_ID := Element (Text, Last)
      end Loop; --  not End_Of_File (Input_File)
   end Build_Succ_Pred;

   Step_List : Step_Lists.Set;
   Input_File : File_Type;

begin -- Dercember_07
   Open (Input_File, In_File, "December_07.txt");
   Get_Steps (Input_File, Step_List);
   Reset (Input_File);

   Close (Input_File);
end December_07;
