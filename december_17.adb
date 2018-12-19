with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_17 is

   X_String : constant String := "x=";
   Y_String : constant String := "y=";
   Spring_X : constant Natural := 500;
   Spring_Y : constant Natural := 0;
   X_Margin : constant Natural := 2;

   procedure Find_Limits (Input_File : in out File_Type;
                         X_Min, X_Max, Y_Min, Y_Max : out Natural) is

      Text : Unbounded_String;
      Start_At, First : Positive;
      Last, X, Y : Natural;

   begin -- Find_Limits
      X_Max := Natural'First;
      X_Min := Natural'Last;
      Y_Max := Natural'First;
      Y_Min := Natural'Last;
      Reset (Input_File);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         if Index (Text, X_String, Start_At) <
           Index (Text, Y_String, Start_At) then
            Start_At := Index (Text, X_String, Start_At) + X_String'Length;
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            X := Natural'Value (Slice (Text, First, Last));
            if X > X_Max then
               X_Max := X;
            end if; -- X > X_Max
            if X < X_Min then
               X_Min := X;
            end if; -- X < X_Min
            Start_At := Last + 1;
            Assert (Element (Text, Start_At) = ',', "X pass 1 ,");
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Y := Natural'Value (Slice (Text, First, Last));
            if Y > Y_Max then
               Y_Max := Y;
            end if; -- Y > Y_Max
            if Y < Y_Min then
               Y_Min := Y;
            end if; -- Y < Y_Min
            Start_At := Last + 1;
            Assert (Element (Text, Start_At) = '.', "Y range pass 1 .");
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Y := Natural'Value (Slice (Text, First, Last));
            if Y > Y_Max then
               Y_Max := Y;
            end if; -- Y > Y_Max
            if Y < Y_Min then
               Y_Min := Y;
            end if; -- Y < Y_Min
         else
            Start_At := Index (Text, Y_String, Start_At) + Y_String'Length;
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Y := Natural'Value (Slice (Text, First, Last));
            if Y > Y_Max then
               Y_Max := Y;
            end if; -- Y > Y_Max
            if Y < Y_Min then
               Y_Min := Y;
            end if; -- Y < Y_Min
            Start_At := Last + 1;
            Assert (Element (Text, Start_At) = ',', "Y pass 1 ,");
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            X := Natural'Value (Slice (Text, First, Last));
            if X > X_Max then
               X_Max := X;
            end if; -- X > X_Max
            if X < X_Min then
               X_Min := X;
            end if; -- X < X_Min
            Start_At := Last + 1;
            Assert (Element (Text, Start_At) = '.', "X range pass1 .");
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            X := Natural'Value (Slice (Text, First, Last));
            if X > X_Max then
               X_Max := X;
            end if; -- X > X_Max
            if X < X_Min then
               X_Min := X;
            end if; -- X < X_Min
         end if; -- X index < Y index
      end loop; -- not End_Of_File (Input_File)
      X_Min := X_Min - X_Margin;
      X_Max := X_Max + X_Margin;
   end Find_Limits;

   procedure Solve (Input_File : in out File_Type;
                    X_Min, X_Max, Y_Min_Spring, Y_Max, Y_min : in Natural) is

      Clay : constant Character := '#';
      Spring : constant Character := '+';
      Stream : constant Character := '|';
      Water : constant Character := '~';
      Sand : constant Character := '.';

      subtype Ground_Elements is Character with Static_Predicate =>
        Ground_Elements in Clay | Spring | Stream | Water | Sand;
      subtype X_Coordinates is Natural range X_Min .. X_Max;
      subtype Y_Coordinates is Natural range Y_Min_Spring .. Y_Max;
      type Grounds is array (X_Coordinates, Y_Coordinates) of Ground_Elements;

      procedure Get_Input (Input_File : in out File_Type;
                           Ground : out Grounds) is

         Text : Unbounded_String;
         Start_At, First : Positive;
         Last, X, X1, X2, Y, Y1, Y2 : Natural;

      begin -- Get_Input
         for X in X_Coordinates loop
            for Y in Y_Coordinates loop
               Ground (X, Y) := Sand;
            end loop; -- Y in Y_Coordinates
         end loop; -- X in X_Coordinates
         Ground (Spring_X, Spring_Y) := Spring;
         Reset (Input_File);
         while not End_Of_File (Input_File) loop
            Get_Line (Input_File, Text);
            Start_At := 1;
            if Index (Text, X_String, Start_At) <
              Index (Text, Y_String, Start_At) then
               Start_At := Index (Text, X_String, Start_At) +
                 X_String'Length;
               Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                           Last);
               X := Natural'Value (Slice (Text, First, Last));
               Start_At := Last + 1;
               Assert (Element (Text, Start_At) = ',', "X pass 2 ,");
               Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                           Last);
               Y1 := Natural'Value (Slice (Text, First, Last));
               Start_At := Last + 1;
               Assert (Element (Text, Start_At) = '.', "Y range pass 2 .");
               Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                           Last);
               Y2 := Natural'Value (Slice (Text, First, Last));
               for Y in Y_Coordinates range Y1 .. Y2 loop
                  Ground (X, Y) := Clay;
               end loop; -- Y in Y_Coordinates range Y1 .. Y2
            else
               Start_At := Index (Text, Y_String, Start_At) +
                 Y_String'Length;
               Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                           Last);
               Y := Natural'Value (Slice (Text, First, Last));
               Start_At := Last + 1;
               Assert (Element (Text, Start_At) = ',', "Y pass 2 ,");
               Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                           Last);
               X1 := Natural'Value (Slice (Text, First, Last));
               Start_At := Last + 1;
               Assert (Element (Text, Start_At) = '.', "X range pass 2,");
               Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                           Last);
               X2 := Natural'Value (Slice (Text, First, Last));
               for X in X_Coordinates range X1 .. X2 loop
                  Ground (X, Y) := Clay;
               end loop; -- X in X_Coordinates range X1 .. X2
            end if; -- X index < Y index
         end loop; -- not End_Of_File (Input_File
      end Get_Input;

      procedure Put (Ground : Grounds) is

         Text : Unbounded_String;

      begin -- Put
         Put_Line ("Putting Ground X range" &
                     X_Coordinates'Image (X_Coordinates'First) & " .." &
                     X_Coordinates'Image (X_Coordinates'Last));
         for Y in Y_Coordinates loop
            Text := Null_Unbounded_String;
            for X in X_Coordinates loop
               Text := Text & Ground (X, Y);
            end loop; -- X in X_Coordinates
            Put_Line (Text & Y_Coordinates'Image (Y));
         end loop; -- Y in Y_Coordinates
      end Put;

      procedure Do_Fill (Ground : in out Grounds;
                         X0 : in X_Coordinates; Y0 : in Y_Coordinates);

      procedure Do_Stream (Ground : in out Grounds;
                           X0 : in X_Coordinates; Y0 : in Y_Coordinates) is

         Y : Y_Coordinates := Y0;

      begin -- Do_Stream
         loop -- until clay or end of world
            Ground (X0, Y) := Stream;
            if Y < Y_Coordinates'Last then
               Y := Y + 1;
            end if; -- Y < Y_Coordinates'Last
            exit when Ground (X0, Y)= Clay or Y >= Y_Coordinates'Last;
         end loop; -- Ground (X0, Y)= Clay or Y >= Y_Coordinates'Last
         if Ground (X0, Y) = Clay then
            Do_Fill (Ground, X0, Y - 1);
         elsif Y >= Y_Coordinates'Last then
            Ground (X0, Y) := Stream;
         end if; -- Ground (X0, Y) = Clay
      end Do_Stream;

      procedure Do_Fill (Ground : in out Grounds;
                         X0 : in X_Coordinates; Y0 : in Y_Coordinates) is

         X_Left, X_Right : X_Coordinates;
         Y : Y_Coordinates := Y0;
         Overflow : Boolean := False;
         Previous_Left_Clay, Previous_Right_Clay : Boolean := True;

      begin -- Do_Fill
         loop -- Add one Layer
            X_Left := X0;
            while X_Left >= X_Coordinates'First and Ground (X_Left, Y) /= Clay
              and
                (Ground (X_Left, Y + 1) = Clay or
                   ((Ground (X_Left, Y + 1) = Water or
                       Ground (X_Left, Y + 1) = Stream) and
                      Previous_Left_Clay)) loop
               Ground (X_Left, Y) := Water;
               if X_Left > X_Coordinates'First then
                  X_Left := X_Left - 1;
               else
                  exit;
               end if; -- X_Left > X_Coordinates'First
            end loop; -- fill left
            if Ground (X_Left, Y + 1) /= Clay and
              Ground (X_Left + 1, Y + 1) = Clay then
               Overflow := True;
               Do_Stream (Ground, X_Left, Y);
            end if; -- Left edge of clay
            Previous_Left_Clay := Ground (X_Left, Y) = Clay;
            X_Right := X0;
            while X_Right <= X_Coordinates'Last and Ground (X_Right, Y) /= Clay
              and
                (Ground (X_Right, Y + 1) = Clay or
                   ((Ground (X_Right, Y + 1) = Water or
                       Ground (X_Right, Y + 1) = Stream) and
                      Previous_Right_Clay)) loop
               Ground (X_Right, Y) := Water;
               if X_Right < X_Coordinates'Last then
                  X_Right := X_Right + 1;
               else
                  exit;
               end if; -- X_Right > X_Coordinates'First
            end loop; -- fill Right
            if Ground (X_Right, Y + 1) /= Clay and
              Ground (X_Right - 1, Y + 1) = Clay then
               Overflow := True;
               Do_Stream (Ground, X_Right, Y);
            end if; -- Right edge of clay
            Previous_Right_Clay := Ground (X_Right, Y) = Clay;
            exit when Overflow or
              not (Previous_Left_Clay and Previous_Right_Clay);
            Y := Y - 1;
         end loop; -- Add one Layer
      end Do_Fill;

      procedure Do_Surface (Ground: in out Grounds) is

         X_Left, X_Right : X_Coordinates;

      begin -- Do_Surface
         for Y in reverse Y_Coordinates range Y_Min .. Y_Coordinates'Last loop
            for X in X_Coordinates loop
               if Ground (X, Y) = Water and Ground (X, Y - 1) = Stream then
                  X_Left := X;
                  X_Right := X;
                  while Ground (X_left, Y) = Water or
                    Ground (X_left, Y) = Stream loop
                     Ground (X_left, Y) := Stream;
                     X_Left := X_Left - 1;
                  end loop; -- left to edge of water
                  while Ground (X_Right, Y) = Water or
                    Ground (X_Right, Y) = Stream loop
                     Ground (X_Right, Y) := Stream;
                     X_Right := X_Right + 1;
                  end loop; -- right to edge of water
               end if; -- surface of water
            end loop; -- X in X_Coordinates loop
         end loop; -- Y reverse input range
      end Do_Surface;

      function Count_Water (Ground : in Grounds;
                            Stream_Switch : in Boolean := True)
                            return Natural is

         Result : Natural := 0;

      begin -- Count_Water
         for Y in Y_Coordinates range Y_Min .. Y_Coordinates'Last loop
            for X in X_Coordinates loop
               if Ground (X, Y) = Water or
                 (Ground (X, Y) = Stream and Stream_Switch) then
                  Result := Result + 1;
               end if; -- water or stream when selected
            end loop; -- X in X_Coordinates
         end loop; -- Y in Y input range
         return Result;
      end Count_Water;

      Ground : Grounds;

   begin -- Solve
      Get_Input (Input_File, Ground);
      Put (Ground);
      Do_Stream (Ground, Spring_X, Spring_Y + 1);
      Do_Surface (Ground);
      Put (Ground);
      Put_Line ("Water count (including stream):" &
                  Natural'Image(Count_Water (Ground)));
      Put_Line ("Water count (Part two):" &
                  Natural'Image(Count_Water (Ground, False)));
   end Solve;

   Input_File : File_Type;
   X_Min, X_Max, Y_Min, Y_Min_Spring, Y_Max : Natural;

begin -- December_17
   Open (Input_File, In_File, "December_17.txt");
   Find_Limits (Input_File, X_Min, X_Max, Y_Min, Y_Max);
   if Spring_Y < Y_Min then
      Y_Min_Spring := Spring_Y;
   else
      Y_Min_Spring := Y_Min;
   end if; -- Spring_Y < Y_Min
   Solve (Input_File, X_Min, X_Max, Y_Min_Spring, Y_Max, Y_Min);
   Close (Input_File);
end December_17;
