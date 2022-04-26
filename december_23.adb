with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_23 is

   subtype Coordinates is Integer;
   subtype Ranges is Natural;

   type Nano_Bots is record
      X, Y, Z : Coordinates;
      R : Ranges;
   end record; -- Nan_Bots

   subtype Bot_Indices is Positive;

   package Nano_Bot_Lists is new
     Ada.Containers.Vectors (Bot_Indices, Nano_Bots);
   use Nano_Bot_Lists;

   function "<" (Left, Right : Nano_Bots) return Boolean is

   begin -- "<"
      return Left.R < Right.R;
   end "<";

   package Range_Sorts is new Nano_Bot_Lists.Generic_Sorting;

   procedure Get_Input (Nano_Bot_List : out Nano_Bot_Lists.Vector) is

      Pos_String : constant String := "pos=<";
      R_string : constant String := "r=";
      Integer_Set : constant Character_Set := To_Set ("-0123456789");
      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Nano_Bot : Nano_Bots;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "December_23.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0 then
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Start_At := Index (Text, Pos_String, Start_At) +
           Pos_String'Length;
         Assert (Start_At > Pos_String'Length, Pos_String & " not found");
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Nano_Bot.X := Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Assert (Element (Text, Start_At) = ',', ", after X");
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Nano_Bot.Y := Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Assert (Element (Text, Start_At) = ',', ", after X");
         Find_Token (Text, Integer_Set, Start_At, Inside, First,Last);
         Nano_Bot.Z := Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Assert (Element (Text, Start_At) = '>', "> after Z");
         Start_At := Index (Text, R_String, Start_At) +
           R_String'Length;
         Assert (Start_At > Pos_String'Length, R_String & " not found");
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Nano_Bot.R := Ranges'Value (Slice (Text, First, Last));
         Append (Nano_Bot_List, Nano_Bot);
      end loop; -- not End_Of_File (Input_File
      Close (Input_File);
   end Get_Input;

   function Man_Distance (Bot_1, Bot_2 : in Nano_Bots)  return Ranges is

   begin -- Man_Distance
      return abs (Bot_1.X - Bot_2.X) + abs (Bot_1.Y - Bot_2.Y) +
      abs (Bot_1.Z - Bot_2.Z);
   end Man_Distance;

   function Part_Two (Nano_Bot_List : in Nano_Bot_Lists.Vector)
                      return Natural is

      -- Note this is not a general solution and relies in the input being
      -- flawed. It "works" by converting the three dimensional problem into one
      -- dimension by taking the nanhattan distance to the nanobot and
      -- counting the number of line segments that overlap based on that
      -- distance plus and minus the range. the line segments are limited to
      -- starting at 0. This is a shameless translation of one of the Megatrend
      -- solutions.
      -- In the manhattan space, the face of the octahedron nearest to the
      -- origin is a constant distance from the origin. The distance where
      -- Near_End is True in my solution, this even if there is a unique point
      -- where the in-range count is maximum it must lie on one such face.
      -- Unless the input data is constructed with a fairly even distribution of
      -- nanobots this flawed strategy stands a reasonable chance of working!
      -- I had a solution based on scaling which got the correct answer for the
      -- example but failed for with my input; however it demonstrated that
      -- something like 98% on the nanobots were in range of the point I found.
      -- Effectively the nanobots are clumpped in one direction relative to the
      -- origin. This also contrinutes to flawed solutions like this working

      type Queue_Elements is record
         Distance_O : Ranges;
         Near_End : Boolean;
      end record; -- Queue_Elements;

      package QI is new
         Ada.Containers.Synchronized_Queue_Interfaces (Queue_Elements);

      function Get_Priority (Queue_Element : Queue_Elements)
                             return Ranges is

      begin -- Get_Priority
         return Queue_Element.Distance_O;
      end Get_Priority;

      function Before (Left, Right : Ranges) return Boolean is

      begin -- Before
         return Left < Right;
      end Before;

      package Queues is new
         Ada.Containers.Unbounded_Priority_Queues (QI, Ranges);

      Queue : Queues.Queue;
      Queue_Element : Queue_Elements;
      Overlap_Count, Maximum_Overlap : Natural := 0;
      Distance : Ranges;

   begin -- Part_Two
      -- Enque the ends of the line segments representing the range of each
      -- nanobots
      for N in Iterate (Nano_Bot_List) loop
         Distance := abs (Nano_Bot_List (N).X) + abs (Nano_Bot_List (N).Y) +
         abs (Nano_Bot_List (N).Z);
         if Nano_Bot_List (N).R > Distance then
            Queue_Element.Distance_O := 0;
         else
            Queue_Element.Distance_O := Distance - Nano_Bot_List (N).R;
         end if; -- Nano_Bot_List (N).R > Distance
         Queue_Element.Near_End := True;
         Queue.Enqueue (Queue_Element);
         Queue_Element.Distance_O := Distance + Nano_Bot_List (N).R;
         Queue_Element.Near_End := False;
         Queue.Enqueue (Queue_Element);
      end loop; -- N in Iterate (Nano_Bot_List)
      -- The ends are now removed from the queue in range order starting closest
      -- to the origin and maintaining a count of the number of overlapping
      -- line segments.
      while Queue.Current_Use > 0 loop
         Queue.Dequeue (Queue_Element);
         if Queue_Element.Near_End then
            Overlap_Count := Overlap_Count + 1;
            if Overlap_Count > Maximum_Overlap then
               Maximum_Overlap := Overlap_Count;
               -- The distance with the greatest number of overlaps.
               Distance := Queue_Element.Distance_O;
            end if; -- Overlap_Count > Maximum_Overlap
         else
            Overlap_Count := Overlap_Count - 1;
         end if; -- Queue_Element.Near_End
      end loop; -- Queue.Current_Use > 0
      return Distance;
   end Part_Two;

   Nano_Bot_List : Nano_Bot_Lists.Vector;
   Last_Bot : Nano_Bots;
   Nano_Bot_Count : Natural := 0;

begin -- December_23
   Get_Input (Nano_Bot_List);
   Range_Sorts.Sort (Nano_Bot_List);
   Assert (Range_Sorts.Is_Sorted (Nano_Bot_List), "not sorted");
   Last_Bot := Last_Element (Nano_Bot_List);
   for B in Nano_Bot_List.Iterate loop
      if Man_Distance (Nano_Bot_List (B), Last_Bot) <= Last_Bot.R then
         Nano_Bot_Count := Nano_Bot_Count + 1;
      end if; -- in range
   end loop; -- I in Nano_Bot_List.Iterate
   Put_Line ("Part one, nanobots in range:" &
               Natural'Image (Nano_Bot_Count));
   Put_CPU_Time;
   Put_Line ("Part two, distance to maximum bots:" &
               Part_Two (Nano_Bot_List)'Img);
   Put_CPU_Time;
end December_23;
