with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Unbounded_Priority_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_22 is

   procedure Get_Input (Cave_Depth, Target_X, Target_Y : out Positive) is

      Depth_String : constant String := "depth:";
      Target_String : constant String := "target:";

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "December_22.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Get_Line (Input_File, Text);
      Start_At := 1;
      Last := Index (Text, Depth_String, Start_At);
      Assert (Last > 0, Depth_String & " not found");
      Start_At := Last + Depth_String'Length;
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Cave_Depth := Positive'Value (Slice (Text, First, Last));
      Get_Line (Input_File, Text);
      Start_At := 1;
      Last := Index (Text, Target_String, Start_At);
      Assert (Last > 0, Target_String & " not found");
      Start_At := Last + Target_String'Length;
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Target_X := Positive'Value (Slice (Text, First, Last));
      Assert (Element (Text, Last + 1) = ',', ", not found");
      Start_At := Last + 1;
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Target_Y := Positive'Value (Slice (Text, First, Last));
      Close (Input_File);
   end Get_Input;

   procedure Solve (Cave_Depth, Target_X, Target_Y, Limit : in Positive) is

      subtype Times is Natural;

      Search_Limit : constant Natural := 7;
      subtype Coordinates is Natural range 0 .. Limit + Search_Limit;
      Geo_X_Multiplier : constant Natural := 16807;
      -- Problem definition
      Geo_Y_Multiplier : constant Natural := 48271;
      -- Problem definition

      subtype Geo_Indices is Natural;
      type Erosion_Levels is mod 20183; -- Problem definition
      type Ground_Types is mod 3; -- Problem definition
      Rocky : constant Ground_Types := 0;
      Wet : constant Ground_Types := 1;
      Narrow : constant Ground_Types := 2;
      type Tools is (Climbing, Neither, Torch);

      type Cave_Elements is record
         Geo_Index : Geo_Indices;
         Erosion_Level :  Erosion_Levels;
         Ground_Type : Ground_Types;
      end record; -- Cave_Elements

      type Caves is array (Coordinates, Coordinates) of Cave_Elements;

      procedure Initialise_Cave (Cave_Depth, Target_X, Target_Y : in Positive;
                                 Cave : out Caves) is

      begin -- Initialise_Cave
         for I in Coordinates loop
            Cave (I, 0).Geo_Index := I * Geo_X_Multiplier;
            Cave (I, 0).Erosion_Level :=
              Erosion_Levels'Mod (Cave (I, 0).Geo_Index + Cave_Depth);
            Cave (0, I).Geo_Index := I * Geo_Y_Multiplier;
            Cave (0, I).Erosion_Level :=
              Erosion_Levels'Mod (Natural (Cave (0, I).Geo_Index) + Cave_Depth);
         end loop; -- I in Coordinates
         Cave (0, 0).Geo_Index := 0; -- Problem definition
         Cave (0, 0).Erosion_Level :=
           Erosion_Levels'Mod (Cave (0, 0).Geo_Index + Cave_Depth);
         Cave (Target_X, Target_Y).Geo_Index := 0; -- Problem definition
         Cave (Target_X, Target_Y).Erosion_Level :=
           Erosion_Levels'Mod (Cave (Target_X, Target_Y).Geo_Index +
                                   Cave_Depth);
         for Y in Coordinates range 1 .. Coordinates'Last loop
            for X in Coordinates range 1 .. Coordinates'Last loop
               if X /= Target_X or Y /= Target_Y then
                  Cave (X, Y).Geo_Index :=
                    Geo_Indices (Cave (X - 1, Y).Erosion_Level) *
                      Geo_Indices (Cave (X, Y - 1).Erosion_Level);
                  Cave (X, Y).Erosion_Level :=
                    Erosion_Levels'Mod (Cave (X, Y).Geo_Index + Cave_Depth);
               end if; -- X /= Target_X or Y /= Target_Y
            end loop; -- X in Coordinates range 1 .. Y
         end loop; -- Y in Coordinates range 1 .. Coordinates'Last
         for X in Coordinates loop
            for Y in Coordinates loop
               Cave (X, Y).Ground_Type :=
                 Ground_Types'Mod (Cave (X, Y).Erosion_Level);
            end loop; -- Cave (X, Y).Erosion_Level
         end loop; -- X in Coordinates
      end Initialise_Cave;

      function Find_Path (Cave : in out Caves;
                          Target_X, Target_Y : in Coordinates) return Times is

         -- A* search for (Target_X, Target_Y)
         -- It is not sufficient to check that position has not been previously
         -- visited, the Tool currently in use has to be considered. It may also
         -- be necessary to check that the time taken to get there is not
         -- shorter. The path taken is not recorded as this is not required.

         Tool_Change : constant Times := 7;

         type Directions is (Up, Down, Left, Right);

         subtype Offsets is Integer range -1 .. 1;

         type Path_Elements is record
            X, Y : Coordinates;
            Tool : Tools;
            Accumulated_Time : Times;
         end record; -- Path_Elements

         type Search_Keys is record
            X, Y : Coordinates;
            Tool : Tools;
         end record; -- Serach_Keys

         function "<" (Left, Right : Search_Keys) return Boolean is

         begin -- "<"
            return Left.X < Right.X or
              (Left.X = Right.X and (Left.Y < Right.Y or
                                         (Left.Y = Right.Y and
                                            Left.Tool < Right.Tool)));
         end "<";

         function "=" (Left, Right : Search_Keys) return Boolean is

         begin -- "="
            return Left.X = Right.X and Left.Y = Right.Y and
              Left.Tool = Right.Tool;
         end "=";

         package Search_Maps is new
           Ada.Containers.Ordered_Maps (Search_Keys, Times);
         use Search_Maps;

         package QI is new
           Ada.Containers.Synchronized_Queue_Interfaces (Path_Elements);

         function Get_Priority (Path_Element : Path_Elements) return Times is

            -- The basic time remaining is based on Manhattan distance which is
            -- guaranteed to under estimate the remaining time except for the
            -- step to the target. The final step is a special case in that it
            -- is needs more time if arriving without a Torch. For the last step
            -- if multiple pathss are available one with longer accumulated time
            -- may be better if it arrives with a Torch!

            Man_Distance : Natural := abs (Target_X - Path_Element.X) +
            abs (Target_Y - Path_Element.Y);
            Result : Positive := Man_Distance + Path_Element.Accumulated_Time;

         begin -- Get_Priority
            if Man_Distance = 1 and then Path_Element.Tool /= Torch then
               Result := Result + Tool_Change;
            end if; -- if Man_Distance > 1 and then Path_Element.Tool /= Torch
            return Result;
         end Get_Priority;

         function Before (Left, Right : Times) return Boolean is

         begin -- Before
            return Left < Right;
         end Before;

         package Queues is new Ada.Containers.UnBounded_Priority_Queues
           (Queue_Interfaces => QI, Queue_Priority => Times);

         procedure Step (Cave : in out Caves;
                         Target_X, Target_Y : in Coordinates;
                         Path_Element : in Path_Elements;
                         Search_Map : in out Search_Maps.Map;
                         Queue : in out Queues.Queue) is

            Step_Time : constant Times := 1;
            Next_Path : Path_Elements;
            Xo, Yo : Offsets;
            Search_Key : Search_Keys;

         begin -- Step
            for D in Directions loop
               case D is
                  when Up =>
                     Xo := 0;
                     Yo := -1;
                  when Down =>
                     Xo := 0;
                     Yo := 1;
                  when Left =>
                     Xo := -1;
                     Yo := 0;
                  when Right =>
                     Xo := 1;
                     Yo := 0;
               end case; -- D
               if (Path_Element.X + Xo in Coordinates and
                     Path_Element.Y + Yo in Coordinates) then
                  Next_Path.X := Path_Element.X + Xo;
                  Next_Path.Y := Path_Element.Y + Yo;
                  Next_Path.Tool := Path_Element.Tool;
                  Next_Path.Accumulated_Time :=
                    Path_Element.Accumulated_Time + Step_Time;
                  case Cave (Path_Element.X, Path_Element.Y).Ground_Type is
                     when Narrow =>
                        case Cave (Next_Path.X, Next_Path.Y).Ground_Type is
                        when Narrow =>
                           null;
                        when Rocky =>
                           if Path_Element.Tool /= Torch then
                              Next_Path.Accumulated_Time :=
                                Path_Element.Accumulated_Time + Step_Time +
                                  Tool_Change;
                              Next_Path.Tool := Torch;
                           end if; -- Path_Element.Tool /= Torch
                        when Wet =>
                           if Path_Element.Tool /= Neither then
                              Next_Path.Accumulated_Time :=
                                Path_Element.Accumulated_Time + Step_Time +
                                  Tool_Change;
                              Next_Path.Tool := Neither;
                           end if; -- Path_Element.Tool /= Neither
                        end case; -- Cave (Next_Path.X, Next_Path.Y).Ground_Type
                     when Rocky =>
                        case Cave (Next_Path.X, Next_Path.Y).Ground_Type is
                        when Narrow =>
                           if Path_Element.Tool /= Torch then
                              Next_Path.Accumulated_Time :=
                                Path_Element.Accumulated_Time + Step_Time +
                                  Tool_Change;
                              Next_Path.Tool := Torch;
                           end if; -- Path_Element.Tool /= Torch
                        when Rocky =>
                           null;
                        when Wet =>
                           if Path_Element.Tool /= Climbing then
                              Next_Path.Accumulated_Time :=
                                Path_Element.Accumulated_Time + Step_Time +
                                  Tool_Change;
                              Next_Path.Tool := Climbing;
                           end if; -- Path_Element.Tool /= Climbing
                        end case; -- Cave (Next_Path.X, Next_Path.Y).Ground_Type
                     when Wet =>
                        case Cave (Next_Path.X, Next_Path.Y).Ground_Type is
                        when Narrow =>
                           if Path_Element.Tool /= Neither then
                              Next_Path.Accumulated_Time :=
                                Path_Element.Accumulated_Time + Step_Time +
                                  Tool_Change;
                              Next_Path.Tool := Neither;
                           end if; -- Path_Element.Tool /= Neither
                        when Rocky =>
                           if Path_Element.Tool /= Climbing then
                              Next_Path.Accumulated_Time :=
                                Path_Element.Accumulated_Time + Step_Time +
                                  Tool_Change;
                              Next_Path.Tool := Climbing;
                           end if; -- Path_Element.Tool = Climbing
                        when Wet =>
                           null;
                        end case; -- Cave (Next_Path.X, Next_Path.Y).Ground_Type
                  end case; -- Cave (Path_Element.X, Path_Element.Y) ...
                  Search_Key := (Next_Path.X, Next_Path.Y, Next_Path.Tool);
                  if not Contains (Search_Map, Search_Key) then
                     -- This position has not been visited with the same Tool in
                     -- use.
                     Queue.Enqueue (Next_Path);
                     Include (Search_Map, Search_Key,
                              Next_Path.Accumulated_Time);
                  elsif Search_Map (Search_Key) >
                    Next_Path.Accumulated_Time then
                     -- Include in search if a different path got here quicker.
                     Search_Map (Search_Key) := Next_Path.Accumulated_Time;
                     Queue.Enqueue (Next_Path);
                  end if; -- not Contains (Search_Map, Search_Key)
               end if; -- (Path_Element.X + Xo in Coordinates and ...
            end loop; -- D in Directions loop
         end Step;

         Queue : Queues.Queue;
         Path_Element : Path_Elements;
         Search_Map : Search_Maps.Map := Search_Maps.Empty_Map;
         Found : Boolean := False;

      begin -- Find_Path
         Path_Element.X := 0;
         Path_Element.Y := 0;
         Path_Element.Tool := Torch;
         Path_Element.Accumulated_Time := 0;
         Queue.Enqueue (Path_Element);
         Include (Search_Map, (0, 0, Torch), 0);
         while Queue.Current_Use > 0 and not Found loop
            Queue.Dequeue (Path_Element);
            Found := Target_X = Path_Element.X and Target_Y = Path_Element.Y;
            if not Found then
               Step (Cave, Target_X, Target_Y, Path_Element, Search_Map, Queue);
            end if; -- Found
         end loop; -- Queue.Current_Use > 0 and not Found
         if Found then
            if Path_Element.Tool /= Torch then
               Path_Element.Tool := Torch;
               Path_Element.Accumulated_Time :=
                 Path_Element.Accumulated_Time + Tool_Change;
            end if; -- Path_Element.Tool /= Torch
            Put_Line ("Peak queue usage:" & Queue.Peak_Use'Img);
            return Path_Element.Accumulated_Time;
         else
            return Times'Last;
         end if; -- Found
      end Find_Path;

      Cave : Caves;
      Risk_Level : Natural := 0;

   begin -- Solve
      Initialise_Cave (Cave_Depth, Target_X, Target_Y, Cave);
      For X in Coordinates range 0 .. Target_X loop
         for Y in Coordinates range 0 .. Target_Y loop
            Risk_Level := Risk_Level + Natural (Cave (X, Y).Ground_Type);
         end loop; -- Y in Coordinates range 0 .. Target_Y
      end loop; -- X in Coordinates range 0 .. Target_X
      Put_Line ("Part one, risk level:" & Risk_Level'Img);
      Put_CPU_Time;
      Put_Line ("Part two, time:" & Find_Path (Cave, Target_X, Target_Y)'Img);
      Put_CPU_Time;
   end Solve;

   Cave_Depth, Target_X, Target_Y, Limit: Positive;

begin -- December_22
   Get_Input (Cave_Depth, Target_X, Target_Y);
   if Target_X > Target_Y then
      Limit := Target_X;
   else
      Limit := Target_Y;
   end if; -- Target_X > Target_Y
   Solve (Cave_Depth, Target_X, Target_Y, Limit);
end December_22;
