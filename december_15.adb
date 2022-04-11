with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Unbounded_Priority_Queues;
with NT_Console;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_15 is

   Elf : constant Character := 'E';
   Goblin : constant Character := 'G';

   subtype Unit_Types is Character with Static_Predicate => Unit_Types
     in Elf | Goblin ;
   Unit_Set : constant Character_Set :=
     To_Set (Elf & Goblin);

   Field_Wall : constant Character := '#';
   Field_Open : constant Character := '.';

   subtype Field_Elements is Character with Static_Predicate => Field_Elements
     in Field_Wall | Field_Open;

   subtype Attacks is Natural;
   Default_Attack : constant Attacks := 3;

   procedure Find_Limits (Input_File : in out File_Type;
                          Field_Size : out Natural) is

      Text : Unbounded_String;
      Field_X_Size, Field_Y_Size : Natural := 0;

   begin -- Find_Limits
      Reset (Input_File);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Field_Y_Size := Field_Y_Size + 1;
         if Length (Text) > Field_X_Size then
            Field_X_Size := Length (Text);
         end if; -- Length (Text) > Field_X_Size
      end loop; -- not End_Of_File (Input_File
      if Field_X_Size > Field_Y_Size then
         Field_Size := Field_X_Size;
      else
         Field_Size := Field_Y_Size;
      end if; -- Field_X_Size > Field_Y_Size
   end Find_Limits;

   procedure Battle (Input_File : in out File_Type;
                     Field_Size : in Natural;
                     Outcome : out Natural;
                     No_Dead_Elves : out Boolean;
                     Elf_Attack : in Attacks := Default_Attack) is

      package Screen is new NT_Console (Field_Size, Field_Size + 1);
      use Screen;

      subtype Field_Limits is Natural range 0 .. Field_Size - 1;
      type Fields is array (Field_Limits, Field_Limits) of Field_Elements;

      type Coordinates is record
         X, Y : Field_Limits;
      end record; -- Coordinates

      function "<" (Left, Right : Coordinates) return Boolean is

      begin -- "<"
         return Left.Y < Right.Y or
           (Left.Y = Right.Y and Left.X < Right.X);
      end "<";

      function "=" (Left, Right : Coordinates) return Boolean is

      begin -- "="
         return Left.Y = Right.Y and Left.X = Right.X;
      end "=";

      package Coordinate_Sets is new Ada.Containers.Ordered_Sets (Coordinates);
      use Coordinate_Sets;

      subtype Hits is Natural range 0 .. 200;

      type Units is record
         Unit_Type : Unit_Types;
         X, Y : Field_Limits;
         Attack : Attacks := Default_Attack;
         Hit : Hits := Hits'Last;
      end record; -- Units

      subtype Unit_Indices is Positive;

      package Unit_Stores is new
        Ada.Containers.Ordered_Maps (Unit_Indices, Units);
      use Unit_Stores;

      type Action_Orders is record
         Unit_Index : Unit_Indices;
         Coordinate : Coordinates;
      end record; -- Action_Orders

      procedure Get_Input (Input_File : in out File_Type;
                           Field : out Fields;
                           Unit_Store : out Unit_Stores.Map) is

         Text : Unbounded_String;
         Unit : Units;
         Y : Field_Limits;
         Xe : positive;
         Unit_Index : Unit_Indices := Unit_Indices'First;

      begin -- Get_Input
         Field := (others => (others => Field_Wall));
         Reset (Input_File);
         while not End_Of_File (Input_File) loop
            Get_Line (Input_File, Text);
            Y := Field_Limits (Line (Input_File) - 2);
            for X in Field_Limits loop
               Xe := X + 1;
               if Element (Text, Xe) in Unit_Types then
                  Unit.Unit_Type := Element (Text, Xe);
                  Unit.X := X;
                  Unit.Y := Y;
                  Field (X, Y) := Field_Open;
                  Insert (Unit_Store, Unit_Index, Unit);
                  Unit_Index := Unit_Index + 1;
               else
                  Field (X, Y) := Element (Text, Xe);
               end if; -- Element (Text, Xe) in Unit_Types
            end loop; -- X in Field_Limits
         end loop; -- not End_Of_File (Input_File)
      end Get_Input;

      procedure Put (Field : Fields; Unit_Store : in Unit_Stores.Map) is

      begin -- Put
         for Y in Field_Limits loop
            Goto_XY (0, Y);
            for X in Field_Limits loop
               Put (Field (X, Y));
            end loop; -- X in Field_Limits
         end loop; -- Y in Field_Limits
         for U in Iterate (Unit_Store) loop
            Goto_XY (Unit_Store (U).X, Unit_Store (U).Y);
            Put (Unit_Store (U).Unit_Type);
         end loop; -- U in Unit_Indices
      end Put;

      procedure Find_Targets (Field : in Fields;
                              Unit_Store : in Unit_Stores.Map;
                              Current_Unit : in Unit_Indices;
                              Target_Set : out Coordinate_Sets.Set) is

         function Is_Open (Field : in Fields;
                           Unit_Store : in Unit_Stores.Map;
                           X, Y : in Field_Limits) return Boolean is

            Result : Boolean;

         begin -- Is_Open
            Result := Field (X, Y) = Field_Open;
            for U in Iterate (Unit_Store) loop
               Result := Result and
                 (Unit_Store (U).X /= X or Unit_Store (U).Y /= Y);
            end loop; -- U in Iterate (Unit_Store)
            return Result;
         end Is_Open;

      begin -- Find_Targets
         Target_Set := Coordinate_Sets.Empty_Set;
         for U in Iterate (Unit_Store) loop
            if Unit_Store (U).Unit_type /= Unit_Store (Current_Unit).Unit_Type
            then
               if Is_Open (Field, Unit_Store, Unit_Store (U).X,
                           Unit_Store (U).Y - 1) then
                  Include (Target_Set,
                           (Unit_Store (U).X, Unit_Store (U).Y - 1));
               end if; -- Is_Open (Field, Unit_Store, Unit_Store (U).X ...
               if Is_Open (Field, Unit_Store, Unit_Store (U).X - 1,
                           Unit_Store (U).Y) then
                  Include (Target_Set,
                           (Unit_Store (U).X - 1, Unit_Store (U).Y));
               end if; -- Is_Open (Field, Unit_Store, Unit_Store (U).X - 1 ...
               if Is_Open (Field, Unit_Store, Unit_Store (U).X + 1,
                           Unit_Store (U).Y) then
                  Include (Target_Set,
                           (Unit_Store (U).X + 1, Unit_Store (U).Y));
               end if; -- Is_Open (Field, Unit_Store, Unit_Store (U).X + 1 ...
               if Is_Open (Field, Unit_Store, Unit_Store (U).X,
                           Unit_Store (U).Y + 1) then
                  Include (Target_Set,
                           (Unit_Store (U).X, Unit_Store (U).Y + 1));
               end if; -- Is_Open (Field, Unit_Store, Unit_Store (U).X ...
            end if; -- Unit_Store (U).Unit_type /= ...
         end loop; -- U in Iterate (Unit_Store)
      end Find_Targets;

      function Can_Attack (Field : in Fields;
                           Unit_Store : in Unit_Stores.Map;
                           Current_Unit : in Unit_Indices) return Boolean is

         Result : Boolean := False;

      begin -- Can_Attack
         for U in Iterate (Unit_Store) loop
            if Unit_Store (U).Unit_Type /=
              Unit_Store (Current_Unit).Unit_Type then
               Result := Result or
                 (Unit_Store (U).X = Unit_Store (Current_Unit).X and
                      Unit_Store (U).Y = Unit_Store (Current_Unit).Y - 1) or
                 (Unit_Store (U).X = Unit_Store (Current_Unit).X - 1 and
                      Unit_Store (U).Y = Unit_Store (Current_Unit).Y) or
                 (Unit_Store (U).X = Unit_Store (Current_Unit).X + 1 and
                      Unit_Store (U).Y = Unit_Store (Current_Unit).Y) or
                 (Unit_Store (U).X = Unit_Store (Current_Unit).X and
                      Unit_Store (U).Y = Unit_Store (Current_Unit).Y + 1);
            end if; -- Unit_Store (U).Unit_Type /= ...
         end loop; -- U in Iterate (Unit_Store)
         return Result;
      end Can_Attack;

      procedure Attack (Field : in Fields;
                        Unit_Store : in out Unit_Stores.Map;
                        Current_Unit : in Unit_Indices;
                        No_Dead_Elves : in out Boolean) is

         type Target_Properties is record
            Hit : Hits;
            X, Y : Field_Limits;
         end record; -- Targets

         function "<" (Left, Right : Target_Properties) return Boolean is

         begin -- "<"
            return Left.Hit < Right.Hit or
              (Left.Hit = Right.Hit and (Left.Y < Right.Y or
                                             (Left.Y = Right.Y and
                                                Left.X < Right.X)));
         end "<";

         function "=" (Left, Right : Target_Properties) return Boolean is

         begin -- "="
            return Left.Hit = Right.Hit and Left.Y = Right.Y and
              Left.X = Right.X;
         end "=";

         package Attack_Targets is new
           Ada.Containers.Ordered_Maps (Target_Properties, Unit_Indices);
         use Attack_Targets;

         Attack_Target : Attack_Targets.Map := Attack_Targets.Empty_Map;
         Attacked : Unit_Indices;
         Saved_Foreground : Color_Type;
         Attack_Delay : Duration;

      begin -- Attack
         if Argument_Count > 0 then
            Attack_Delay := Duration'Value (Argument (1));
         else
            Attack_Delay := 0.0;
         end if; -- Argument_Count > 0
         for U in Iterate (Unit_Store) loop
            if Unit_Store (U).Unit_Type /=
              Unit_Store (Current_Unit).Unit_Type and then
              ((Unit_Store (U).X = Unit_Store (Current_Unit).X and
                  Unit_Store (U).Y = Unit_Store (Current_Unit).Y - 1) or
                 (Unit_Store (U).X = Unit_Store (Current_Unit).X - 1 and
                      Unit_Store (U).Y = Unit_Store (Current_Unit).Y) or
                   (Unit_Store (U).X = Unit_Store (Current_Unit).X + 1 and
                          Unit_Store (U).Y = Unit_Store (Current_Unit).Y) or
                 (Unit_Store (U).X = Unit_Store (Current_Unit).X and
                      Unit_Store (U).Y = Unit_Store (Current_Unit).Y + 1)) then
               Insert (Attack_Target,
                       (Unit_Store (U).Hit, Unit_Store (U).X, Unit_Store (U).Y),
                       Key (U));
            end if; -- Unit_Store (U).Unit_Type /= ...
         end loop; -- U in Iterate (Unit_Store)
         Attacked := First_Element (Attack_Target);
         Saved_Foreground := Get_Foreground;
         Goto_XY (Unit_Store (Current_Unit).X, Unit_Store (Current_Unit).Y);
         Set_Foreground (Cyan);
         Put (Unit_Store (Current_Unit).Unit_Type);
         Goto_XY (Unit_Store (Attacked).X, Unit_Store (Attacked).Y);
         if Unit_Store (Attacked).Hit < Unit_Store (Current_Unit).Attack then
            Set_Foreground (Red);
            Put (Unit_Store (Attacked).Unit_Type);
            delay Attack_Delay * 5;
            No_Dead_Elves := No_Dead_Elves and
              Unit_Store (Attacked).Unit_Type /= Elf;
            Set_Foreground (Saved_Foreground);
            Goto_XY (Unit_Store (Attacked).X, Unit_Store (Attacked).Y);
            Put (Field_Open);
            Delete (Unit_Store, Attacked);
         else
            Set_Foreground (Yellow);
            Put (Unit_Store (Attacked).Unit_Type);
            delay Attack_Delay;
            Unit_Store (Attacked).Hit :=
              Unit_Store (Attacked).Hit - Unit_Store (Current_Unit).Attack;
            Set_Foreground (Saved_Foreground);
            Goto_XY (Unit_Store (Attacked).X, Unit_Store (Attacked).Y);
            Put (Unit_Store (Attacked).Unit_Type);
         end if; -- Unit_Store (Attacked).Hit < Unit_Store (Current_Unit).Attack
         Goto_XY (Unit_Store (Current_Unit).X, Unit_Store (Current_Unit).Y);
         Put (Unit_Store (Current_Unit).Unit_Type);
      end Attack;

      function First_Step (Field : in Fields;
                           Unit_Store : in Unit_Stores.Map;
                           Current_Unit : in Unit_Indices;
                           Target_Set : in Coordinate_Sets.Set)
                           return Coordinates is

         type Steps is record
            First_Step : Coordinates;
            X, Y : Field_Limits;
            Step_Count : Natural := 0;
         end record; -- Steps

         package QI is new Ada.Containers.Synchronized_Queue_Interfaces (Steps);

         Current_Target : Coordinates;

         package Queues is new
           Ada.Containers.Unbounded_Synchronized_Queues (QI);
         use Queues;

         Nearest : Natural := Natural'Last;
         Starting_Step : Coordinates := (Unit_Store (Current_Unit).X,
                                         Unit_Store (Current_Unit).Y);
         -- return unit's coordinates if no move found

      begin -- First_Step
         for T in Iterate (Target_Set) loop
            declare -- new data structures for each search
               Queue : Queues.Queue;
               To_Search : array (Field_Limits, Field_Limits) of Boolean :=
                 (others => (others => True));
               Found : Boolean := False;
               Too_Long : boolean;
               Step, Next_Step : Steps;
            begin -- new data structures for each search
               Current_Target := Target_Set (T);
               for U in Iterate (Unit_Store) loop
                  To_Search (Unit_Store (U).X, Unit_Store (U).Y) := False;
                  -- Don't search through units!
               end loop; -- U in Iterate (Unit_Store)
               Step := ((Unit_Store (Current_Unit).X,
                        Unit_Store (Current_Unit).Y - 1),
                        Unit_Store (Current_Unit).X,
                        Unit_Store (Current_Unit).Y - 1, 1);
               If To_Search (Step.X, Step.Y) and
                 Field (Step.X, Step.Y) = Field_Open then
                  To_Search (Step.X, Step.Y) := False;
                  Queue.Enqueue (Step);
               end if; -- Above
               Step := ((Unit_Store (Current_Unit).X - 1,
                        Unit_Store (Current_Unit).Y),
                        Unit_Store (Current_Unit).X - 1,
                        Unit_Store (Current_Unit).Y, 1);
               If To_Search (Step.X, Step.Y) and
                 Field (Step.X, Step.Y) = Field_Open then
                  To_Search (Step.X, Step.Y) := False;
                  Queue.Enqueue (Step);
               end if; -- Left
               Step := ((Unit_Store (Current_Unit).X + 1,
                        Unit_Store (Current_Unit).Y),
                        Unit_Store (Current_Unit).X + 1,
                        Unit_Store (Current_Unit).Y, 1);
               If To_Search (Step.X, Step.Y) and
                 Field (Step.X, Step.Y) = Field_Open then
                  To_Search (Step.X, Step.Y) := False;
                  Queue.Enqueue (Step);
               end if; -- Right
               Step := ((Unit_Store (Current_Unit).X,
                        Unit_Store (Current_Unit).Y + 1),
                        Unit_Store (Current_Unit).X,
                        Unit_Store (Current_Unit).Y + 1, 1);
               If To_Search (Step.X, Step.Y) and
                 Field (Step.X, Step.Y) = Field_Open then
                  To_Search (Step.X, Step.Y) := False;
                  Queue.Enqueue (Step);
               end if; -- Below
               while not Found and Queue.Current_Use > 0 loop
                  Queue.Dequeue (Step);
                  Found := Step.X = Current_Target.X and
                    Step.Y = Current_Target.Y;
                  Too_Long := Step.Step_Count >= Nearest;
                  if Found then
                     if Step.Step_Count < Nearest then
                        Nearest := Step.Step_Count;
                        Starting_Step := Step.First_Step;
                     end if; -- Step.Step_Count < Nearest
                  elsif not Too_Long then
                     Next_Step.First_Step := Step.First_Step;
                     Next_Step.Step_Count := Step.Step_Count + 1;
                     Next_Step.X := Step.X;
                     Next_Step.Y := Step.Y - 1;
                     if To_Search (Next_Step.X, Next_Step.Y) and
                       Field (Next_Step.X, Next_Step.Y) = Field_Open then
                        To_Search (Next_Step.X, Next_Step.Y) := False;
                        Queue.Enqueue (Next_Step);
                     end if; -- Above;
                     Next_Step.X := Step.X - 1;
                     Next_Step.Y := Step.Y;
                     if To_Search (Next_Step.X, Next_Step.Y) and
                       Field (Next_Step.X, Next_Step.Y) = Field_Open then
                        To_Search (Next_Step.X, Next_Step.Y) := False;
                        Queue.Enqueue (Next_Step);
                     end if; -- Left;
                     Next_Step.X := Step.X + 1;
                     Next_Step.Y := Step.Y;
                     if To_Search (Next_Step.X, Next_Step.Y) and
                       Field (Next_Step.X, Next_Step.Y) = Field_Open then
                        To_Search (Next_Step.X, Next_Step.Y) := False;
                        Queue.Enqueue (Next_Step);
                     end if; -- Right;
                     Next_Step.X := Step.X;
                     Next_Step.Y := Step.Y + 1;
                     if To_Search (Next_Step.X, Next_Step.Y) and
                       Field (Next_Step.X, Next_Step.Y) = Field_Open then
                        To_Search (Next_Step.X, Next_Step.Y) := False;
                        Queue.Enqueue (Next_Step);
                     end if; -- Below;
                  end if; -- Found
               end loop; -- not Found and < Queue.Current_Use > 0
            end; -- new data structures for each search
         end loop; -- T in Iterate (Target_Set)
         return Starting_Step;
      end First_Step;

      function is_Enemy (Unit_Store : in Unit_Stores.Map ) return Boolean is

         Elves, Goblins : Natural := 0;

      begin -- Is_Enemy
         for U in Iterate (Unit_Store) loop
            if Unit_Store (U).Unit_Type = Elf then
               Elves := Elves + 1;
            elsif Unit_Store (U).Unit_Type = Goblin then
               Goblins := Goblins + 1;
            end if; --Unit_Store (U).Unit_Type = Elf
         end loop; -- U in Iterate (Unit_Store)
         return Elves > 0 and Goblins > 0;
      end Is_Enemy;

      function Get_Priority (Action_Order : Action_Orders)
                             return Coordinates is

      begin -- Get_Priority
         return Action_Order.Coordinate;
      end Get_Priority;

      function Before (Left, Right : Coordinates) return Boolean is

      begin -- Before
         return Left.Y < Right.Y or
           (Left.Y = Right.Y and Left.X < Right.X);
      end Before;

      package QI is new
        Ada.Containers.Synchronized_Queue_Interfaces (Action_Orders);

      package Action_Queues is new Ada.Containers.Unbounded_Priority_Queues
        (Queue_Interfaces => QI,
         Queue_Priority => Coordinates);

      Field : Fields;
      Unit_Store : Unit_Stores.Map;
      Action_Order : Action_Orders;
      Action_Queue : Action_Queues.Queue;
      Target_Set : Coordinate_Sets.Set;
      Move_To : Coordinates;
      No_Action : Boolean;
      Round : Natural := 0;

   begin -- Battle
      Get_Input (Input_File, Field, Unit_Store);
      -- Initialise Elf attack
      for U in Iterate (Unit_Store) loop
         if Unit_Store (U).Unit_Type = Elf then
            Unit_Store (U).Attack := Elf_Attack;
         end if; -- Unit_Store (U).Unit_Type = Elf
      end loop; -- U in Iterate (Unit_Store)
      Outcome := 0;
      No_Dead_Elves := True;
      Clear_Screen;
      Set_Cursor (False);
      Goto_XY (X_Pos'First, Y_Pos'Last - 1);
      Put ("Elf attack points:" & Elf_Attack'Img);
      Put (Field, Unit_Store);
      loop -- until no action
         No_Action := True;
         -- Enqueue units in priority que sorted by coordinates
         for U in Iterate (Unit_Store) loop
            Action_Order := (Key (U), (Unit_Store (U).X, Unit_Store (U).Y));
            Action_Queue.Enqueue (Action_Order);
         end loop; -- U in Iterate (Unit_Store)
         while Action_Queue.Current_Use > 0 and is_Enemy (Unit_Store) loop
            Action_Queue.Dequeue (Action_Order);
            if Contains (Unit_Store, Action_Order.Unit_Index) then
               -- Unit is still alive, it existed when put in the queue but it
               -- may have been killed since and removed from Unit_Store!
               if not Can_Attack (Field, Unit_Store,
                                  Action_Order.Unit_Index) then
                  Find_Targets (Field, Unit_Store, Action_Order.Unit_Index,
                                Target_Set);
                  Move_To := First_Step (Field, Unit_Store,
                                         Action_Order.Unit_Index, Target_Set);
                  if Move_To.X /= Action_Order.Coordinate.X or
                    Move_To.Y /= Action_Order.Coordinate.Y then
                     Goto_XY (Action_Order.Coordinate.X,
                              Action_Order.Coordinate.Y);
                     Put (Field_Open);
                     Unit_Store (Action_Order.Unit_Index).X := Move_To.X;
                     Unit_Store (Action_Order.Unit_Index).Y := Move_To.Y;
                     Goto_XY (Move_To.X, Move_To.Y);
                     Put (Unit_Store (Action_Order.Unit_Index).Unit_Type);
                     No_Action := False;
                  end if; -- Move_To.X /= Action_Order.Coordinate.X or ...
               end if; -- not Can_Attack (Field, Unit_Store,
               if Can_Attack (Field, Unit_Store, Action_Order.Unit_Index) then
                  No_Action := False;
                  Attack (Field, Unit_Store, Action_Order.Unit_Index,
                          No_Dead_Elves);
               end if; -- Can_Attack (Field, Unit_Store, Action_Order ...
            end if; -- Contains (Unit_Store, Action_Order.Unit_Index)
         end loop; --  Action_Queue.Current_Use > 0 and is_Enemy (Unit_Store)
         exit when No_Action or Action_Queue.Current_Use > 0;
         -- If Action_Queue.Current_Use > 0 then the round was incomplete, don't
         -- Increment Round Count.
         Round := Round + 1;
         Goto_XY (X_Pos'First, Y_Pos'Last);
         Put ("Round:" & Round'Img);
      end loop; -- until no action
      Goto_XY (X_Pos'First, Y_Pos'Last);
      Put_Line ("Round:" & Round'Img);
      Set_Cursor (True);
      for U in Iterate (Unit_Store) loop
         Outcome := Outcome + Unit_Store (U).Hit;
      end loop;
      Outcome := Outcome * Round;
   end Battle;

   Input_File : File_Type;
   Text : Unbounded_String;
   Field_Size : Natural;
   Part_One_Outcome, Part_Two_Outcome : Natural;
   No_Dead_Elves : Boolean;
   Elf_Attack : Attacks := Default_Attack + 1;

begin -- December_15
   if Argument_Count < 2 then
      Open (Input_File, In_File, "December_15.txt");
   else
      Open (Input_File, In_File, Argument (2));
   end if; -- Argument_Count < 2
   Find_Limits (Input_File, Field_Size);
   Battle (Input_File, Field_Size, Part_One_Outcome, No_Dead_Elves);
   loop -- find mimimum Elf attack
      Battle (Input_File, Field_Size, Part_Two_Outcome, No_Dead_Elves,
              Elf_Attack);
      exit when No_Dead_Elves;
      Elf_Attack := Elf_Attack + 1;
   end loop; -- find mimimum Elf attack
   Put_Line ("Part one outcome: " & Part_One_Outcome'Img);
   Put_Line ("Part two outcome: " & Part_Two_Outcome'Img);
   Put_CPU_Time;
   Close (Input_File);
end December_15;
