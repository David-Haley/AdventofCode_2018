with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Ordered_Sets;

procedure December_12 is

   Pot_Empty : constant Character := '.';
   Pot_Plant : constant Character := '#';
   subtype Pots is Character
     with Static_Predicate => Pots in Pot_Empty | Pot_Plant;
   subtype Pot_Indices is Integer range -1000 .. 1000;
   -- subtype Pot_Indices is Integer range -40 .. 40;
   type Pot_Arrays is array (Pot_Indices) of Pots;

   subtype States is String (1 .. 5);

   type Rules is record
      State : States;
      Result : Pots;
   end record; -- Rules

   function "<" (Left, Right : Rules) return Boolean is

   begin -- "<"
      return Left.State < Right.State;
   end "<";

   function "=" (Left, Right : Rules) return Boolean is

   begin -- "="
      return Left.State = Right.State;
   end "=";

   package Rule_Sets is new Ada.Containers.Ordered_Sets (Rules);
   use Rule_Sets;

   function Key (Rule : Rules) return States is

   begin -- Key
      return Rule.State;
   end Key;

   package Step_Keys is New Rule_Sets.Generic_Keys (Key_Type => States,
                                                    Key => Key);
   use Step_Keys;

   procedure Get_Pots (Input_File : in out File_Type;
                       Pot_Array : out Pot_Arrays) is

      Pot_String : constant String := "initial state: ";
      Text : Unbounded_String;
      First : Positive;
      Last : Natural;

   begin -- Get_Pots
      Pot_Array := (others => Pot_empty);
      Get_Line (Input_File, Text);
      First := Index (Text, Pot_String);
      Last := First + Pot_String'Length - 1;
      Delete (Text, First, Last);
      for I in Positive range 1 .. Length (Text) loop
         Pot_Array (I - 1) := Element (Text, I);
      end loop; -- I in Positive range 1 .. Length (Text)
   end Get_Pots;

   procedure Get_Rules (Input_File : in out File_Type;
                        Rule_Set : out Rule_Sets.Set) is

      Product_String : constant String :=" => ";
      Pot_Set : Character_Set := To_Set (Pot_Empty & Pot_Plant);
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Rule : Rules;

   begin -- Get_Rules
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         if Length (Text) > 0 then
            Start_At := 1;
            Find_Token (Text, Pot_Set, Start_At, Inside, First, Last);
            Rule.State := Slice (Text, First, Last);
            Start_At := Last + 1;
            First := Index (Text, Product_String, Start_At) +
              Product_String'Length;
            Rule.Result := Element (Text, First);
            Include (Rule_Set, Rule);
         end if; -- Length (Text) > 0
      end loop; -- not End_Of_File (Input_File)
   end Get_Rules;

   function Next_Generation (Pot_Array : in Pot_Arrays;
                             Rule_Set : in Rule_Sets.Set)
                             return Pot_Arrays is

      State_Centre : constant Positive := (States'Last + 1) / 2;
      Next_Pot_Array : Pot_Arrays := (others => Pot_Empty);
      Current_State : States;

   begin -- Next_Generation
      for I in Pot_Indices range Pot_Indices'First + State_Centre - 1 ..
        Pot_Indices'Last - (State_Centre - 1) loop
         for S in Positive range 1 .. States'Last loop
            Current_State (S) := Pot_Array (I - State_Centre + S);
         end loop; -- S in Positive range 1 .. States'Last
         Next_Pot_Array (I) := Element (Rule_Set, Current_State).Result;
      end loop; -- I
      Assert (Next_Pot_Array (Pot_Indices'First + State_Centre - 1) =
                Pot_Empty, "lower bound reached");
      Assert (Next_Pot_Array (Pot_Indices'Last - (State_Centre - 1)) =
                Pot_Empty, "upper bound reached");
      return Next_Pot_Array;
   end Next_Generation;

   function Sum_Pots (Pot_Array : in Pot_Arrays) return Integer is

      Pot_Sum : Integer := 0;

   begin -- Sum_Pots
      for I in Pot_Indices loop
         if Pot_Array (I) = Pot_Plant then
            Pot_Sum := Pot_Sum + I;
         end if; -- Pot_Array (I) = Pot_Plant
      end loop; -- I in Pot_Indices
      return Pot_Sum;
   end Sum_Pots;

   Input_File : File_Type;
   Pot_Array : Pot_Arrays;
   Rule_Set : Rule_Sets.Set;
   Pot_Sum, Pot_Sum_1, Pot_Sum_2 : Integer := 0;
   Generation_Count : Positive := 1;
   Large_Generations : constant Long_Long_Integer := 50000000000;
   Large_Pot_Sum, M, C : Long_Long_Integer;

begin -- December_12
   Open (Input_File, In_File, "December_12.txt");
   -- Open (Input_File, In_File, "Example_12.txt");
   Get_Pots (Input_File, Pot_Array);
   Get_Rules (Input_File, Rule_Set);
   for Generation in Positive range 1 .. 20 loop
      Pot_Array := Next_Generation (Pot_Array, Rule_Set);
   end loop; -- Generation in Positive range 1 to 20 loop
   Put_Line ("Pot sum:" & Natural'Image (Sum_Pots (Pot_Array)));
   Reset (Input_File);
   Get_Pots (Input_File, Pot_Array);
   Get_Rules (Input_File, Rule_Set);
   loop
      Pot_Array := Next_Generation (Pot_Array, Rule_Set);
      Pot_Sum := Sum_Pots (Pot_Array);
      exit when Pot_Sum - Pot_Sum_1 = Pot_Sum_1 - Pot_Sum_2;
      Pot_Sum_2 := Pot_Sum_1;
      Pot_Sum_1 := Pot_Sum;
      Generation_Count := Generation_Count + 1;
   end loop;
   M := Long_Long_Integer (Pot_Sum - Pot_Sum_1);
   C := Long_Long_Integer (Pot_Sum) - Long_Long_Integer (Generation_Count) * M;
   Large_Pot_Sum := Large_Generations * M + C;
   Put_Line ("Pot sum (part two):" &
               Long_Long_Integer'Image (Large_Pot_Sum));
   Close (Input_File);
end December_12;
