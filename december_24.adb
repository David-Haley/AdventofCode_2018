with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_24 is

   type Armies is (Immune_System, Infection);
   subtype Values is Positive;

   package Property_Sets is new Ada.Containers.Ordered_Sets (Unbounded_String);
   use Property_Sets;

   type Groups is record
      Army : Armies;
      Quantity, Hit, Damage, Initiative : Values;
      Damage_Type : Unbounded_String;
      Immunity, Weakness : Property_Sets.Set := Property_Sets.Empty_Set;
   end record; -- Groups

   subtype Group_Indices is Positive;

   package Group_Stores is new
     Ada.Containers.Ordered_Maps (Group_Indices, Groups);
   use Group_Stores;

   type Attack_Elements is record
     Attacker, Defender : Group_Indices;
   end record;

   package Attack_Lists is new
     Ada.Containers.Ordered_Maps (Values, Attack_Elements);
   use Attack_Lists;

   Input_Error : Exception;

   procedure Get_Input (Group_Store : out Group_Stores.Map) is

      Hit_String : constant String := "hit points";
      Weak_String : constant String := "weak to";
      Immune_String : constant String := "immune to";
      Damage_String : constant String := "damage";
      Set_Set : constant Character_Set := To_Set (" ,;)");
      Space_Set : constant Character_Set := To_Set (" ");
      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last, End_Hit, Property_Start : Natural;
      Group_Index : Group_Indices := 1;
      Group : Groups;


   begin -- Get_Input
      Group_Store := Group_Stores.Empty_Map;
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_24.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         if Text = "Immune System:" then
            Group.Army := Immune_System;
         elsif text = "Infection:" then
            Group.Army := Infection;
         else
            raise Input_Error with ": Unknown group type:" & To_String (Text);
         end if; -- Text = "Immune System:"
         Get_Line (Input_File, Text);
         while Length (Text) /= 0 loop
            Clear (Group.Immunity);
            Clear (Group.Weakness);
            Start_At := 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Group.Quantity := Values'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Group.Hit := Values'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            End_Hit := Index (Text, Hit_String, Start_At);
            if End_Hit = 0 then
               raise Input_Error with Hit_String & " not found";
            end if; -- End_Hit = 0
            End_Hit := End_Hit + Hit_String'Length;
            Property_Start := Index (Text, Weak_String, End_Hit);
            Start_At := Property_Start + Weak_String'Length;
            while Property_Start /= 0 and Element (Text, Start_At) /= ')' and
              Element (Text, Start_At) /= ';' loop
               Find_Token (Text, Set_Set, Start_At, Outside, First, Last);
               Insert (Group.Weakness, Unbounded_Slice (Text, First, Last));
               Start_At := Last + 1;
            end loop; -- Property_Start /= 0 and Element (Text, Start_At) /= ')'
            Property_Start := Index (Text, Immune_String, End_Hit);
            Start_At := Property_Start + Immune_String'Length;
            while Property_Start /= 0 and Element (Text, Start_At) /= ')' and
              Element (Text, Start_At) /= ';' loop
               Find_Token (Text, Set_Set, Start_At, Outside, First, Last);
               Insert (Group.Immunity, Unbounded_Slice (Text, First, Last));
               Start_At := Last + 1;
            end loop; -- Property_Start /= 0 and Element (Text, Start_At) /= ')'
            Start_At := End_Hit;
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Group.Damage := Values'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Find_Token (Text, Space_Set, Start_At, Outside, First, Last);
            Group.Damage_Type := Unbounded_Slice (Text, First, Last);
            Start_At := Last + 1;
            Find_Token (Text, Space_Set, Start_At, Outside, First, Last);
            if Slice (Text, First, Last) /= Damage_String then
               raise Input_Error with "Expected " & Damage_String &
                 " and found """ & Slice (Text, First, Last) & '"';
            end if; -- Slice (Text, First, Last) /= Damage_String
            Start_At := Last + 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Group.Initiative := Values'Value (Slice (Text, First, Last));
            Insert (Group_Store, Group_Index, Group);
            Group_Index := Group_Index + 1;
            if End_Of_File (Input_File) then
               Text := Null_Unbounded_String;
            else
               Get_Line (Input_File, Text);
            end if; -- End_Of_File (Input_File)
         end loop; -- Length (Text) /= 0
      end loop; -- not End_Of_File (Input_File
      Close (Input_File);
   exception
      when E: others =>
         Put_Line (Positive_Count'Image (Line (Input_File) - 1) & ": " &
                     Exception_Message (E));
         raise;
   end Get_Input;

   procedure Put (Group_Store : in Group_Stores.Map) is

   begin -- Put
      for G in Iterate (Group_Store) loop
         Put (Key (G)'Img & ' ' & Group_Store (G).Army'Img &
                " Quantity:" & Group_Store (G).Quantity'Img &
                " Hit" & Group_Store (G).Hit'Img & " Immunity ");
         for I in Iterate (Group_Store (G).Immunity) loop
            Put (Group_Store (G).Immunity (I) & ' ');
         end loop; -- I in Iterate (Group_Store (G).Immunity)
         Put ("Weakness ");
         for W in Iterate (Group_Store (G).Weakness) loop
            Put (Group_Store (G).Weakness (W) & ' ');
         end loop; -- W in Iterate (Group_Store (G).Weakness)
         Put_Line ("Damage " & Group_Store (G).Damage_Type &
                     Group_Store (G).Damage'Img &
                     " Initiative" & Group_Store (G).Initiative'Img);
      end loop; -- G in Iterate (Group_Store)
   end Put;

   procedure Select_Targets (Group_Store : in Group_Stores.Map;
                             Attack_List : out Attack_Lists.Map) is

      type Group_Priorities is record
         Effective_Power, Initiative : Values;
      end record; -- Group_Priorities;

      function "<" (Left, Right : Group_Priorities) return Boolean is

      begin -- "<"
         return Left.Effective_Power < Right.Effective_Power or
           (Left.Effective_Power = Right.Effective_Power and
              Left.Initiative > Right.Initiative);
      end "<";

      function "=" (Left, Right : Group_Priorities) return Boolean is

      begin -- "="
         return Left.Effective_Power = Right.Effective_Power and
           Left.Initiative = Right.Initiative;
      end "=";

      package Choice_Lists is new
        Ada.Containers.Ordered_Maps (Group_Priorities, Group_Indices);
      use Choice_Lists;

      type Target_Priorities is record
         Damage, Effective_Power, Initiative : Values;
      end record; -- Target_Priorities

      function "<" (Left, Right : Target_Priorities) return Boolean is

      begin -- "<"
         return Left.Damage < Right.Damage or
           (Left.Damage = Right.Damage and
              (Left.Effective_Power < Right.Effective_Power or
                   (Left.Effective_Power = Right.Effective_Power and
                        Left.Initiative < Right.Initiative)));
      end "<";

      function "=" (Left, Right : Target_Priorities) return Boolean is

      begin -- "="
         return Left.Damage = Right.Damage and
           Left.Effective_Power = Right.Effective_Power and
           Left.Initiative = Right.Initiative;
      end "=";

      package Target_Lists is new
        Ada.Containers.Ordered_Maps (Target_Priorities, Group_Indices);
      use Target_Lists;

      package Target_Sets is new Ada.Containers.Ordered_Sets (Group_Indices);
      use Target_Sets;

      Target_Set : Target_Sets.Set := Target_Sets.Empty_Set;
      Choice_List : Choice_Lists.Map := Choice_Lists.Empty_Map;
      A : Group_Indices;
      Target_Priority : Target_Priorities;
      Target_List : Target_Lists.Map := Target_Lists.Empty_Map;

   begin -- Select_Targets
      Attack_List := Attack_Lists.Empty_Map;
      -- Determine order in which groups choose targets
      for G in Iterate (Group_Store) loop
         insert (Choice_List,
                 (Group_Store (G).Quantity * Group_Store (G).Damage,
                  Group_Store (G).Initiative), Key (G));
      end loop; -- G in Iterate (Group_Store)
      for C in reverse Iterate (Choice_List) loop
         A := Choice_List (C); -- Group index of attacker
         Clear (Target_List);
         for D in Iterate (Group_Store) loop
            if Group_Store (A).Army /= Group_Store (D).Army and
              not Contains (Group_Store (D).Immunity,
                            Group_Store (A).Damage_Type) and
              not Contains (Target_Set, Key (D)) then
               -- Opposing armies and not immune
               if (Contains (Group_Store (D).Weakness,
                             Group_Store (A).Damage_Type)) then
                  Target_Priority.Damage := Group_Store (A).Quantity *
                    Group_Store (A).Damage * 2;
               else
                  Target_Priority.Damage := Group_Store (A).Quantity *
                    Group_Store (A).Damage;
               end if; -- (Contains (Group_Store (D).Weakness, ...
               Target_Priority.Effective_Power :=
                 Group_Store (D).Quantity * Group_Store (D).Damage;
               Target_Priority.Initiative := Group_Store (D).Initiative;
               insert (Target_List, Target_Priority, Key (D));
            end if; -- Group_Store (A).Army /= Group_Store (D).Army and ...
         end loop; -- D in Iterate (Group_Store)
         if not Is_Empty (Target_List) then
            Insert (Target_Set, Last_Element (Target_List));
            insert (Attack_List, Group_Store (A).Initiative,
                    (A, Last_Element (Target_List)));
         end if; -- not Is_Empty (Target_List)
      end loop; -- A in Iterate (Choice_List)
   end Select_Targets;

   procedure Battle (Group_Store : in out Group_Stores.Map;
                     Attack_List : in Attack_Lists.Map;
                     No_Change : out Boolean) is

      A, D : Group_Indices;
      Destroys : Natural;

   begin -- Battle
      No_Change := True;
      for I in reverse Iterate (Attack_List) loop
         -- in order of decreasing initiative
         if Contains (Group_Store, Attack_List (I).Attacker) then
            -- It is possible for an attacker to no longer exist if it was the
            -- the target of an earlier attack. The defender must still exixt as
            -- only one group is permmitted to attack in each round.
            A := Attack_List (I).Attacker;
            D := Attack_List (I).Defender;
            if (Contains (Group_Store (D).Weakness,
                          Group_Store (A).Damage_Type)) then
               Destroys := Group_Store (A).Quantity * Group_Store (A).Damage * 2
                 / Group_Store (D).Hit;
            else
               Destroys := Group_Store (A).Quantity * Group_Store (A).Damage
                 / Group_Store (D).Hit;
            end if; -- (Contains (Group_Store (D).Weakness, ...
                    -- integer division reaults in floor
            No_Change := No_Change and Destroys = 0;
            -- no effective attack
            if Destroys >= Group_Store (D).Quantity then
               Delete (Group_Store, D);
            else
               Group_Store (D).Quantity := Group_Store (D).Quantity - Destroys;
            end if; -- Destroys >= Group_Store (D).Quantity
         end if; -- Contains (Group_Store, Attack_List (I).Attacker)
      end loop; -- Al in Iterate (Attack_List)
   end Battle;

   Group_Store : Group_Stores.Map := Group_Stores.Empty_Map;
   Attack_List : Attack_Lists.Map;
   Unit_Count : Natural;
   No_Change : Boolean;
   Boost : Values := 1;

begin -- December_24
   Get_Input (Group_Store);
   Put (Group_Store);
   loop -- until nothing left to attack
      Select_Targets (Group_Store, Attack_List);
      exit when Is_Empty (Attack_List);
      Battle (Group_Store, Attack_List, No_Change);
   end loop; -- until nothing left to attack
   Unit_Count := 0;
   for G in Iterate (Group_Store) loop
      Unit_Count := Unit_Count + Group_Store (G).Quantity;
   end loop; -- G in Iterate (Group_Store)
   Put_Line ("Part one, remaining units:" & Unit_Count'Img);
   Put_CPU_Time;
   loop -- until minimum boost
      Get_Input (Group_Store);
      for G in Iterate (Group_Store) loop
         if Group_Store (G).Army = Immune_System then
            Group_Store (G).Damage := Group_Store (G).Damage + Boost;
         end if; -- Group_Store (G).Army = Immune_System
      end loop; -- G in Iterate (Group_Store)
      No_Change := False;
      -- provide initial value so there is atleast one battle
      loop -- until nothing left to attack
         Select_Targets (Group_Store, Attack_List);
         exit when Is_Empty (Attack_List) or No_Change;
         Battle (Group_Store, Attack_List, No_Change);
      end loop; -- until nothing left to attack
      Unit_Count := 0;
      if Is_Empty (Attack_List) then
         -- ended with one army having no units left
         for G in Iterate (Group_Store) loop
            if Group_Store (G).Army = Immune_System then
               Unit_Count := Unit_Count + Group_Store (G).Quantity;
            end if; -- Group_Store (G).Army = Immune_System
         end loop; -- G in Iterate (Group_Store)
      end if; -- Is_Empty (Attack_List)
      exit when Unit_Count > 0;
      Boost := Boost + 1;
   end loop; -- until minimum boost
   Put_Line ("Part two, remaining immune system units:" & Unit_Count'Img);
   Put_CPU_Time;
end December_24;
