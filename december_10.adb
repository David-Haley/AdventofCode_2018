with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with NT_Console;

procedure December_10 is

   procedure Solve (Input_File : in out File_Type; Pixel_Count : in Natural) is

      type Pixels is record
         X, Y, X_Velocity, Y_Velocity : Integer;
      end record; -- Pixels

      subtype Pixel_Indices is Positive range 1 .. Pixel_Count;

      type Pixel_Arrays is array (Pixel_Indices) of Pixels;

      subtype Ticks is Integer;

      package Screen is new NT_Console;
      use Screen;

      procedure Get_Pixels (Input_File : in out File_Type;
                            Pixel_Array : out Pixel_Arrays) is

         Position_String : constant String := "position=<";
         Velocity_String : constant String := "velocity=<";
         Integer_Set : constant Character_Set := To_Set ("-0123456789");

         Text : Unbounded_String;
         Start_At, First : Positive;
         Last : Natural;

      begin -- Get_Pixels
         for I in Pixel_Indices loop
            Get_Line (Input_File, Text);
            Start_At := 1;
            Last := Index (Text, Position_String, Start_At) +
              Position_String'Length;
            Start_At := Last;
            Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
            Pixel_Array (I).X := Integer'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Assert (Element (Text, Start_At) = ',', "Position X ',' not found");
            Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
            Pixel_Array (I).Y := Integer'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Assert (Element (Text, Start_At) = '>', "Position Y '>' not found");
            Last := Index (Text, Velocity_String, Start_At) +
              Velocity_String'Length;
            Start_At := Last;
            Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
            Pixel_Array (I).X_Velocity :=
              Integer'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Assert (Element (Text, Start_At) = ',', "Velocity X ',' not found");
            Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
            Pixel_Array (I).Y_Velocity :=
              Integer'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Assert (Element (Text, Start_At) = '>', "Velocity Y '>' not found");
         end loop; -- I in Pixel_Indices
      end Get_Pixels;

      procedure Update_Pixels (Pixel_Array : in out Pixel_Arrays;
                               Step : in Ticks := 1) is

      begin -- Update_Pixels
         for I in Pixel_Indices loop
            Pixel_Array (I).X := Pixel_Array (I).X +
              Pixel_Array (I).X_Velocity * Step;
            Pixel_Array (I).Y := Pixel_Array (I).Y +
              Pixel_Array (I).Y_Velocity * Step;
         end loop; --
      end Update_Pixels;

      Procedure Limits (Pixel_Array : in Pixel_Arrays;
                        Min_X, Min_Y, Max_X, Max_Y : out Integer) is

      begin -- Limits
         Min_X := Integer'Last;
         Min_Y := Integer'Last;
         Max_X := Integer'First;
         Max_Y := Integer'First;
         for I in Pixel_Indices loop
            if Pixel_Array (I).X < Min_X then
               Min_X := Pixel_Array (I).X;
            end if; -- Pixel_Array (I).X < Min_X
            if Pixel_Array (I).Y < Min_Y then
               Min_Y := Pixel_Array (I).Y;
            end if; -- Pixel_Array (I).Y < Min_Y
            if Pixel_Array (I).X > Max_X then
               Max_X := Pixel_Array (I).X;
            end if; -- Pixel_Array (I).X > Max_X
            if Pixel_Array (I).Y > Max_Y then
               Max_Y := Pixel_Array (I).Y;
            end if; -- Pixel_Array (I).Y > Max_X
         end loop; -- I in Pixel_Indices
      end Limits;

      function Minimum_Area_Time (Main_Array : in Pixel_Arrays) return Ticks is

         Pixel_Array : Pixel_Arrays := Main_Array;
         Last_Perimeter : Integer := Integer'Last;
         Perimeter : Integer := Last_Perimeter - 1;
         Min_X, Min_Y : Integer;
         Max_X, Max_Y : Integer;
         Tick : Ticks := 0;

      begin -- Minimum_Area_Time
         while Perimeter < Last_Perimeter loop
            Last_Perimeter := Perimeter;
            Update_Pixels (Pixel_Array);
            Limits (Pixel_Array, Min_X, Min_Y, Max_X, Max_Y);
            Perimeter := (Max_X - Min_X) + (Max_Y - Min_Y);
            Tick := Tick + 1;
         end loop; -- Area < Last_Area
         return Tick - 1;
      end Minimum_Area_Time;

      Procedure Display (Main_Array : in Pixel_Arrays; Tick : in Ticks) is

         Pixel_Array : Pixel_Arrays := Main_Array;
         Min_X, Min_Y, Max_X, Max_Y : Integer;

      begin -- Display
         Update_Pixels (Pixel_Array, Tick);
         Limits (Pixel_Array, Min_X, Min_Y, Max_X, Max_Y);
         Clear_Screen;
         for I in Pixel_Indices loop
            Goto_XY (Pixel_Array (I).X - Min_X, Pixel_Array (I).Y - Min_Y);
            Put ('#');
         end loop; -- I in Pixel_Indices
         Goto_XY (0, 23);
         Put ("Time:" & Ticks'Image (Tick));
      end Display;

      Pixel_Array : Pixel_Arrays;
      Ch : Character := ' ';
      Tick : Ticks;

   begin -- Solve
      Get_Pixels (Input_File, Pixel_Array);
      Tick := Minimum_Area_Time (Pixel_Array);
      while Ch /= 'x' loop
         Display (Pixel_Array, Tick);
         Get_Immediate (Ch);
         case Ch is
            when '+' =>
               Tick := Tick + 1;
            when '-' =>
               Tick := Tick - 1;
            when others =>
               null;
         end case; -- Ch
      end loop; -- Ck /= 'x'
      Clear_Screen;
   end Solve;

   Input_File : File_Type;
   Text : Unbounded_String;
   Pixel_Count : Natural:= 0;

begin -- Dercember_10
   Open (Input_File, In_File, "December_10.txt");
   Get_Line (Input_File, Text);
   while not End_Of_File (Input_File) loop
      Get_Line (Input_File, Text);
      Pixel_Count := Pixel_Count + 1;
   end loop; -- not End_Of_File (Input_File)
   Reset (Input_File);
   Solve (Input_File, Pixel_Count);
   Close (Input_File);
end December_10;
