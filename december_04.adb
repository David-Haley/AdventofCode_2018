with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Doubly_Linked_Lists;

procedure December_04 is

   type Event_Types is (Start_Shift, Start_Sleep, End_Sleep);
   subtype Minutes is Natural range 0 .. 23 * 60 + 59;
   subtype Dates is String (1 .. 10);
   type Sleep_Maps is Array (Minutes) of Natural;
   subtype Guard_IDs is Positive range 1 .. 9999;

   type Events is record
      Event_Type : Event_Types;
      Date : Dates;
      Minute : Minutes;
      Guard_ID : Guard_IDs;
   end record; -- Events

   package Event_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Events);
   use Event_Lists;

   function "<" (Left, Right : Events) return Boolean is

   begin -- "<"
      if Left.Date < Right.Date then
         return True;
      elsif Left.Date = Right.Date and then Left.Minute < Right.Minute then
         return True;
      elsif Left.Date = Right.Date and then Left.Minute = Right.Minute and then
        Right.Event_Type > Left.Event_Type then
         return True;
      else
         return False;
      end if; -- Left.Date < Right.Date
   end "<";

   package Event_Sort is new Event_Lists.Generic_Sorting ("<");
   use Event_Sort;

   type Guards is record
      Guard_ID : Guard_IDs;
      Total_Sleep : Natural := 0;
      Sleep_Map : Sleep_Maps := (others => 0);
   end record; -- Guards;

   package Guard_Lists is new Ada.Containers.Doubly_Linked_Lists (Guards);
   use Guard_Lists;

   procedure Get_History (Input_File : in out File_Type;
                          Event_List : in out Event_Lists.List) is

      procedure Append_Event (Event_List : in out Event_Lists.List;
                              Event_Type : in Event_Types;
                              Current_Date : in Dates;
                              Current_Minute : in Minutes;
                              Guard_ID : in Guard_IDs := Guard_IDs'Last) is

         -- Default initialisation of Guard_IDs is used for Start_Sleep and
         -- End_Sleep, the actual Guard ID is not known until the events are
         -- sorted. Guard_IDs'Last is not used as a sentenel value and would be
         -- acceptable as an actual Guard_ID.

         Event : Events := (Event_Type, Current_Date, Current_Minute, Guard_ID);

      begin -- Append_Event
         Append (Event_List, Event);
      end Append_Event;

      Date_Delimiters : constant Character_Set := To_Set ("[ ");
      Hour_Delimiters : constant Character_Set := To_Set (" :");
      Minute_Delimeters : constant Character_Set := To_Set (":]");
      Guard_Delimeters : constant Character_Set := To_Set ("# ");
      Guard_String : constant String := "Guard #";

      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Current_Guard_ID : Guard_IDs;
      Current_Date : Dates;
      Current_Minute : Minutes;

   begin -- Get_History
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Date_Delimiters, Start_At, Outside, First, Last);
         Current_Date := Slice (Text, First, Last);
         Start_At := Last + 1;
         Find_Token (Text, Hour_Delimiters, Start_At, Outside, First, Last);
         Current_Minute := Minutes'Value (Slice (Text, First, Last)) * 60;
         Start_At := Last + 1;
         Find_Token (Text, Minute_Delimeters, Start_At, Outside, First, Last);
         Current_Minute := Current_Minute +
           Minutes'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         if Index (Text, Guard_String, Start_At) > 0 then
            Start_At := Index (Text, Guard_String, Start_At) +
              Guard_String'Length;
            Find_Token (Text, Guard_Delimeters, Start_At, Outside, First, Last);
            Current_Guard_ID := Guard_IDs'Value (Slice (Text, First, Last));
            Append_Event (Event_List, Start_Shift, Current_Date, Current_Minute,
                          Current_Guard_ID);
            -- Assumes shift starts before 00:00 minutes in 23:xx problematic
         elsif Index (Text, "falls asleep", Start_At) > 0 then
            Append_Event (Event_List, Start_Sleep, Current_Date,
                          Current_Minute);
         elsif Index (Text, "wakes up", Start_At) > 0 then
            Append_Event (Event_List, End_Sleep, Current_Date,
                          Current_Minute);
         else
            Assert (False, "Unknown entry type");
         end if; -- Index (Text, "Guard", Start_At) /= 0
      end loop; --not End_Of_File (Input_File)
   end Get_History;

   procedure Update_Guard_ID (Event_List : in out Event_Lists.List) is

      Current_Guard_ID : Guard_IDs;

   begin -- Update_Guard_ID
      Sort (Event_List);
      for I in Event_List.Iterate loop
         if Event_List (I).Event_Type = Start_Shift then
            Current_Guard_ID := Event_List (I).Guard_ID;
         else
            Event_List (I).Guard_ID := Current_Guard_ID;
         end if; -- Guard_List (I).Event_Type = Start_Shift
      end loop; -- I in Event_List.Iterate
   end Update_Guard_ID;

   procedure Build_Guard_List (Guard_List : in out Guard_Lists.List;
                               Event_List : in out Event_Lists.List) is

      function Find_Guard (Guard_List : in Guard_Lists.List;
                           Guard_ID : in Guard_IDs) return Guard_Lists.Cursor is

         Result : Guard_Lists.Cursor := First (Guard_List);

      begin -- Find_Guard
         while Result /= Guard_Lists.No_Element and then
           Guard_List (Result).Guard_ID /= Guard_Id loop
            Next (Result);
         end loop; -- searching
         return Result;
      end Find_Guard;

      procedure Append_Guard (Guard_List : in out Guard_Lists.List;
                              Guard_ID : in Guard_IDs;
                              Current_Guard : out Guard_Lists.Cursor) is

         New_Guard : Guards;

      begin -- Append_Guard
         New_Guard.Guard_ID := Guard_ID;
         Append (Guard_List, New_Guard);
         Current_Guard := Last (Guard_List);
      end Append_Guard;

      procedure Update_Sleep (Guard_List : in out Guard_Lists.List;
                              Current_Guard : in Guard_Lists.Cursor;
                              Sleep, Wake : in Minutes) is

      begin -- Update_Sleep
         for I in Minutes range Sleep .. Wake - 1 loop
            Guard_List (Current_Guard).Sleep_Map (I) :=
              Guard_List (Current_Guard).Sleep_Map (I) + 1;
         end loop; -- I in Minutes range Sleep .. Wake - 1
         Guard_List (Current_Guard).Total_Sleep :=
           Guard_List (Current_Guard).Total_Sleep + Wake - Sleep;
      end Update_Sleep;

      procedure Update_Sleep (Guard_List : in out Guard_Lists.List;
                              Current_Guard : in Guard_Lists.Cursor;
                              Sleep : in Minutes) is

      begin -- Update_Sleep
         for I in Minutes range Sleep .. Minutes'Last loop
            Guard_List (Current_Guard).Sleep_Map (I) :=
              Guard_List (Current_Guard).Sleep_Map (I) + 1;
         end loop; --I in Minutes range Sleep .. Minutes'Last
         Guard_List (Current_Guard).Total_Sleep :=
           Guard_List (Current_Guard).Total_Sleep + Minutes'Last - Sleep + 1;
      end Update_Sleep;

      Current_Event : Event_Lists.Cursor := First (Event_List);
      Current_Guard : Guard_Lists.Cursor := Guard_Lists.No_Element;
      Previous_Date : Dates := Event_List (Current_Event).Date;
      Previous_Guard_ID : Guard_IDs := Event_List (Current_Event).Guard_ID;
      Guard_Asleep : Boolean := False;
      Start_Minute : Minutes := Minutes'Last; -- initialised to avoid warning

   begin -- Build_Guard_List
      while Current_Event /= Event_Lists.No_Element loop
         Current_Guard := Find_Guard (Guard_List,
                                      Event_List (Current_Event).Guard_ID);
         case Event_List (Current_Event).Event_Type is
            when Start_Shift =>
               if Previous_Guard_ID /= Event_List (Current_Event).Guard_ID
                 and Guard_Asleep then
                  -- Assumed that Guard already in list
                  Update_Sleep (Guard_List, Current_Guard, Start_Minute);
                  Guard_Asleep := False;
               end if; -- previously asleep
               Current_Guard :=
                 Find_Guard (Guard_List, Event_List (Current_Event).Guard_ID);
               if Current_Guard = Guard_Lists.No_Element then
                  Append_Guard (Guard_List,
                                Event_List (Current_Event).Guard_ID,
                                Current_Guard);
               end if; -- Current_Guard = Guard_Lists.No_Element
               Previous_Guard_ID := Event_List (Current_Event).Guard_ID;
               Previous_Date := Event_List (Current_Event).Date;
            when Start_Sleep =>
               if Guard_Asleep and
                 Previous_Date /= Event_List (Current_Event).Date then
                  Update_Sleep (Guard_List, Current_Guard, Start_Minute);
                  Guard_Asleep := False;
               end if; -- sleeping on previous shift
               Guard_Asleep := True;
               Start_Minute := Event_List (Current_Event).Minute;
               Previous_Date := Event_List (Current_Event).Date;
               Assert (Previous_Guard_ID = Event_List (Current_Event).Guard_ID,
                       "inconsistent Guard_ID at start of sleep");
            when End_Sleep =>
               Assert (Previous_Date = Event_List (Current_Event).Date,
                       "end of sleep without start");
               Assert (Previous_Guard_ID = Event_List (Current_Event).Guard_ID,
                       "inconsistent Guard_ID at end of sleep");
               Assert (Guard_Asleep, "guard not currently asleep");
               Update_Sleep (Guard_List, Current_Guard, Start_Minute,
                             Event_List (Current_Event).Minute);
               Guard_Asleep := False;
         end case; -- Event_List (Current_Event).Event_Type
         Next (Current_Event);
      end loop; -- Current_Event /= Event_Lists.No_Element
   end Build_Guard_List;

   Input_File : File_Type;
   Event_List : Event_Lists.List := Event_Lists.Empty_List;
   Guard_List : Guard_Lists.List := Guard_Lists.Empty_List;
   Sleepy_Guard : Guard_Lists.Cursor;
   Greatest_Total_Sleep : Natural := 0;
   Highest_Frequency : Natural := 0;
   Most_Frequent_Minute : Minutes;

begin -- Dercember_04s
   Open (Input_File, In_File, "December_04.txt");
   Get_History (Input_File, Event_List);
   Close (Input_File);
   Update_Guard_ID (Event_List);
   Build_Guard_List (Guard_List, Event_List);
   for I in Guard_List.Iterate loop
      if Guard_List (I).Total_Sleep > Greatest_Total_Sleep then
         Greatest_Total_Sleep := Guard_List (I).Total_Sleep;
         Sleepy_Guard := I;
      end if; -- Guard_List (I).Total_Sleep > Greatest_Total_Sleep
   end loop; --  I in Guard_List.Iterate
   for M in Minutes loop
      if Guard_List (Sleepy_Guard).Sleep_Map (M) > Highest_Frequency then
         Highest_Frequency := Guard_List (Sleepy_Guard).Sleep_Map (M);
         Most_Frequent_Minute := M;
      end if; -- Guard_List (Sleepy_Guard).Sleep_Map (M) > Highest_Frequency
   end loop; -- M in Minutes
   Put_Line ("Guard ID:" &
               Guard_IDs'Image (Guard_List (Sleepy_Guard).Guard_ID) &
               " * Most Frequent Minute:" &
               Minutes'Image (Most_Frequent_Minute));
   Put_Line ("Product:" & Natural'Image (Natural (Most_Frequent_Minute) *
               Natural (Guard_List (Sleepy_Guard).Guard_ID)));
   Highest_Frequency := 0;
   for G in Guard_List.Iterate loop
      for M in Minutes loop
         if Guard_List (G).Sleep_Map (M) > Highest_Frequency then
            Highest_Frequency := Guard_List (G).Sleep_Map (M);
            Most_Frequent_Minute := M;
            Sleepy_Guard := G;
         end if; -- Guard_List (Sleepy_Guard).Sleep_Map (M) > Highest_Frequency
      end loop; -- M in Minutes
   end loop; -- G in Guard_List.Iterate
   Put_Line ("Guard ID (part two):" &
               Guard_IDs'Image (Guard_List (Sleepy_Guard).Guard_ID) &
               " * Most Frequent Minute:" &
               Minutes'Image (Most_Frequent_Minute));
   Put_Line ("Product:" & Natural'Image (Natural (Most_Frequent_Minute) *
               Natural (Guard_List (Sleepy_Guard).Guard_ID)));
end December_04;
