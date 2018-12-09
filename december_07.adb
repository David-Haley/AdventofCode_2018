with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Ordered_Sets;

procedure December_07 is

   subtype Step_IDs is Character range 'A' .. 'Z';
   subtype Ticks is Natural;
   Minimum_Step_Time : constant Ticks := 60;
   type Elf_Indices is (Elf_A, Elf_B, Elf_C, Elf_D);


   package ID_Lists is new Ada.Containers.Ordered_Sets (Step_IDs);
   use ID_Lists;

   type Steps is record
      Step_ID : Step_IDs;
      Succ : ID_Lists.Set := ID_Lists.Empty_Set;
      Pred : ID_Lists.Set := ID_Lists.Empty_Set;
      Remaining : Ticks;
      Started : Boolean := False;
      Assigned_To : Elf_Indices := Elf_Indices'First;
   end record; -- Steps

   function "<" (Left, Right : Steps) return Boolean is

   begin -- "<"
      return Left.Step_ID < Right.Step_ID;
   end "<";

   function "=" (Left, Right : Steps) return Boolean is

   begin -- "="
      return Left.Step_ID = Right.Step_ID;
   end "=";

   package Step_Lists is new Ada.Containers.Ordered_Sets (Steps);
   use Step_Lists;

   function Key (Step : Steps) return Step_IDs is

   begin -- Key
      return Step.Step_ID;
   end Key;

   package Step_Keys is New Step_Lists.Generic_Keys (Key_Type => Step_IDs,
                                                    Key => Key);
   use Step_Keys;

   type Elf_Free_Lists is array (Elf_Indices) of Boolean;

   procedure Get_Steps (Input_File : in out File_Type;
                        Step_List : in out Step_Lists.Set) is

      Step_String : constant String := "Step ";
      Succ_String : constant String := " must be finished before step ";
      Text : Unbounded_String;
      Start_At : Positive;
      Last : Natural;
      Step : Steps;
      Succ_ID : Step_IDs;
      Current_Step : Step_Lists.Cursor;

   begin -- Get_Steps
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Last := Index (Text, Step_String) + Step_String'Length;
         Step.Step_ID := Element (Text, Last);
         if not Contains (Step_List, Step) then
            Include (Step_List, Step);
         end if; -- not Contains (Step_List, Step)
         Start_At := Last + 1;
         Last := Index (Text, Succ_String) + Succ_String'Length;
         Succ_ID := Element (Text, Last);
         Current_Step := Find (Step_List, Step.Step_ID);
         Include (Step_List(Current_Step).Succ, Succ_ID);
         Current_Step := Find (Step_List, Succ_ID);
         if Current_Step = Step_Lists.No_Element then
            Step.Step_ID := Succ_ID;
            Include (Step_List, Step);
            -- Ensures that final step is included
         end if;
      end Loop; --  not End_Of_File (Input_File)
   end Get_Steps;

   procedure Build_Pred (Step_List : in out Step_Lists.Set) is

      -- initialises predecessors and task duration

      Current_Step : Step_Lists.Cursor;
      Step_Duration : Ticks := Minimum_Step_Time;

   begin --  Build_Pred
      for I in iterate (Step_List) loop
         Step_Duration := Step_Duration + 1;
         Step_List (I).Remaining := Step_Duration;
         For S in Iterate (Step_List (I).Succ) loop
            Current_Step := Find (Step_List, Step_List (I).Succ (S));
            Include (Step_List (Current_Step).Pred, Step_List (I).Step_ID);
         end loop; -- S in Iterate (Step_List.Succ)
      end loop; -- I in iterate (Step_List)
   end Build_Pred;

   function Sequence (Main_List : Step_Lists.Set) return Unbounded_String is

      Step_List : Step_Lists.Set := Copy (Main_List);
      All_Completed : Boolean;
      Result : Unbounded_String := Null_Unbounded_String;
      Completed_Step_ID : Step_IDs;
      Completed_Step, Successor : Step_Lists.Cursor;

   begin -- Sequence
      loop -- One Step
         All_Completed := True;
         for I in Step_List.Iterate loop
            if Step_List (I).Pred = ID_Lists.Empty_Set
              and Step_List (I).Remaining > 0 then
               Completed_Step_ID := Step_List (I).Step_ID;
               Step_List (I).Remaining := 0;
               All_Completed := False;
               exit;
            end if; -- no predecessors and remaining time
         end loop; --  I in Step_List.Iterate
         Completed_Step := Find (Step_List, Completed_Step_ID);
         for S in Step_List (Completed_Step).Succ.Iterate loop
            Successor := Find (Step_List,
                                 Step_List (Completed_Step).Succ (S));
            Delete (Step_List (Successor).Pred, Completed_Step_ID);
         end loop; -- S in Step_List (Completed_Step).Succ.Iterate
         exit when All_Completed;
         Result := Result & Completed_Step_ID;
      end loop; -- One Step
      return Result;
   end Sequence;

   function Completion_Time (Main_List : Step_Lists.Set) return Ticks is

      function All_Completed (Step_List : in Step_Lists.Set) return Boolean is

         Result : Boolean := True;

      begin -- All_Completed
         for I in Step_List.Iterate loop
            Result := Result and Step_List (I).Remaining = 0;
         end loop; -- I in Step_List.Iterate
         return Result;
      end All_Completed;

      procedure Start_Step (Elf_Index : in Elf_Indices;
                            Elf_Free_List : in out Elf_Free_Lists;
                            Step_List : in out Step_Lists.Set) is

      begin -- Start_Step
         for I in Step_List.Iterate loop
            if Step_List (I).Pred = ID_Lists.Empty_Set
              and not Step_List (I).Started
              and Step_List (I).Remaining > 0 then
               Step_List (I).Started := True;
               Step_List (I).Assigned_To := Elf_Index;
               Elf_Free_List (Elf_Index) := False;
               exit;
            end if; -- no predecessors and not started and not completed
         end loop; --  I in Step_List.Iterate
      end Start_Step;

      procedure Update (Elf_Free_List : in out Elf_Free_Lists;
                        Step_List : in out Step_Lists.Set) is

         Successor : Step_Lists.Cursor;

      begin -- Update
         for I in Step_List.Iterate loop
            if Step_List (I).Started then
               Step_List (I).Remaining := Step_List (I).Remaining - 1;
               if Step_List (I).Remaining = 0 then
                  Step_List (I).Started := False;
                  Elf_Free_List (Step_List (I).Assigned_To) := True;
                  for S in Step_List (I).Succ.Iterate loop
                     Successor := Find (Step_List, Step_List (I).Succ (S));
                     Delete (Step_List (Successor).Pred, Step_List (I).Step_ID);
                  end loop; -- S in Step_List (Completed_Step).Succ.Iterate
               end if; -- Step_List.Remaining = 0
            end if; -- Step_List (I).Started
         end loop; -- I in Step_List.Iterate
      end Update;

      Step_List : Step_Lists.Set := Copy (Main_List);
      Elf_Free_List : Elf_Free_Lists := (others => True);
      Tick : Ticks := Ticks'First;

   begin -- Completion_Time
      while not All_Completed (Step_List) loop
         for E in Elf_Indices loop
            if Elf_Free_List (E) then
               Start_Step (E, Elf_Free_List, Step_List);
            end if; -- Elf_Free_List (E)
         end loop; --E in Elf_Indices
         Update (Elf_Free_List, Step_List);
         Tick := Tick + 1;
      end loop; -- not All_Completed (Step_List)
      return Tick;
   end Completion_Time;

   Step_List : Step_Lists.Set;
   Input_File : File_Type;

begin -- Dercember_07
   Open (Input_File, In_File, "December_07.txt");
   Get_Steps (Input_File, Step_List);
   Close (Input_File);
   Build_Pred (Step_List);
   Put_Line ("Sequence: " & Sequence (Step_List));
   Put_Line ("Completion Time:" & Ticks'Image (Completion_Time (Step_List)));
end December_07;
