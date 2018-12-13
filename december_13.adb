with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Generic_Constrained_Array_Sort;

procedure December_13 is

   Cart_Up : constant Character := '^';
   Cart_Right : constant Character := '>';
   Cart_Down : constant Character := 'v';
   Cart_Left : constant Character := '<';

   subtype Cart_Directions is Character with Static_Predicate => Cart_Directions
     in Cart_Up | Cart_Right | Cart_Down | Cart_Left;
   Cart_Set : constant Character_Set :=
     To_Set (Cart_Up & Cart_Right & Cart_Down & Cart_Left);

   Track_Vertical : constant Character := '|';
   Track_Fs : constant Character := '/';
   Track_Horizontal : constant Character := '-';
   Track_Bs : constant Character := '\';
   Track_Intersection : constant Character := '+';
   Track_No_Track : constant Character := ' ';

   subtype Track_Elements is Character with Static_Predicate => Track_Elements
     in Track_Vertical | Track_Fs | Track_Horizontal | Track_Bs |
       Track_Intersection | Track_No_Track;

   procedure Find_Limits (Input_File : in out File_Type;
                          Track_X_Size, Track_Y_Size, Cart_Count
                          : out Natural) is

      Text : Unbounded_String;

   begin -- Find_Limits
      Track_X_Size := 0;
      Track_Y_Size := 0;
      Cart_Count := 0;
      Reset (Input_File);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Track_Y_Size := Track_Y_Size + 1;
         if Length (Text) > Track_X_Size then
            Track_X_Size := Length (Text);
         end if; -- Length (Text) > Track_X_Size
         Cart_Count := Cart_Count +
           Ada.Strings.Unbounded.Count (Text, Cart_Set);
      end loop; -- not End_Of_File (Input_File
   end Find_Limits;

   procedure Solve (Input_File : in out File_Type;
                    Track_Size, Cart_Count : in Natural) is

      subtype Coordinates is Natural range 0 .. Track_Size - 1;
      type Tracks is array (Coordinates, Coordinates) of Track_Elements;

      type Intersection_Counts is mod 3;
      type Carts is record
         Direction : Cart_Directions;
         X, Y : Coordinates;
         Intersection_Count : Intersection_Counts;
         Destroyed : Boolean;
      end record; -- Cart_Elements

      subtype Cart_Indices is positive range 1 .. Cart_Count;

      type Cart_Arrays is array (Cart_Indices) of Carts;

      function "<" (Left, Right : Carts) return Boolean is

      begin -- "<"
         return Left.Y < Right.Y;
      end "<";

      procedure Get_Input (Input_File : in out File_Type;
                           Track : out Tracks;
                           Cart_Array : out Cart_Arrays) is

         Text : Unbounded_String;
         Cart_Count, Line_Count : Natural := 0;

      begin -- Get_Input
         for X in Coordinates loop
            for Y in Coordinates loop
               Track (X, Y) := Track_No_Track;
            end loop; -- Y in Coordinates
         end loop; -- X in Coordinates
         Reset (Input_File);
         while not End_Of_File (Input_File) loop
            Get_Line (Input_File, Text);
            Line_Count := Line_Count + 1;
            for Xs in Positive range 1 .. Length (Text) loop
               if Element (Text, Xs) in Cart_Directions then
                  Cart_Count := Cart_Count + 1;
                  Cart_Array (Cart_Count).Direction := Element (Text, Xs);
                  Cart_Array (Cart_Count).X := Xs - 1;
                  Cart_Array (Cart_Count).Y := Line_Count - 1;
                  Cart_Array (Cart_Count).Intersection_Count := 0;
                  Cart_Array (Cart_Count).Destroyed := False;
                  case Element (Text, Xs) is
                     when Cart_Up | Cart_Down =>
                        Track (Xs - 1, Line_Count - 1) := Track_Vertical;
                     when Cart_Left | Cart_Right =>
                        Track (Xs - 1, Line_Count - 1) := Track_Horizontal;
                     when others =>
                        null; -- should be unreachable
                  end case; -- Element (Text, Xs)
               else
                  Track (Xs - 1, Line_Count - 1) := Element (Text, Xs);
               end if; -- Element (Text, Xs) in Cart_Directions
            end loop; -- Xs in Positive range 1 .. Length (Text)
         end loop; -- not End_Of_File (Input_File)
      end Get_Input;

      procedure Update_Carts (Cart_Array : in out Cart_Arrays;
                              Track : in Tracks;
                              Collision : out Boolean;
                              Xc, Yc : out Coordinates;
                              Part_One : Boolean := True) is

         procedure Sort_Carts is new
           Ada.Containers.Generic_Constrained_Array_Sort
             (Index_Type => Cart_Indices,
              Element_Type => Carts,
              Array_Type => Cart_Arrays);

         procedure Do_Intersection (Cart : in out Carts) is

         begin -- Do_Intersection
            case Cart.Intersection_Count is
               when 0 =>
                  -- turn left
                  case Cart.Direction is
                     when Cart_Up =>
                        Cart.X := Cart.X - 1;
                        Cart.Direction := Cart_Left;
                     when Cart_Left =>
                        Cart.Y := Cart.Y + 1;
                        Cart.Direction := Cart_Down;
                     when Cart_Down =>
                        Cart.X := Cart.X + 1;
                        Cart.Direction := Cart_Right;
                     when Cart_Right =>
                        Cart.Y := Cart.Y - 1;
                        Cart.Direction := Cart_Up;
                  end case; -- Cart.Direction
               when 1 =>
                  case Cart.Direction is
                     when Cart_Up =>
                        Cart.Y := Cart.Y - 1;
                     when Cart_Left =>
                        Cart.X := Cart.X - 1;
                     when Cart_Down =>
                        Cart.Y := Cart.Y + 1;
                     when Cart_Right =>
                        Cart.X := Cart.X + 1;
                  end case; -- Cart.Direction
               when 2 =>
                  -- turn right
                  case Cart.Direction is
                     when Cart_Up =>
                        Cart.X := Cart.X + 1;
                        Cart.Direction := Cart_Right;
                     when Cart_Left =>
                        Cart.Y := Cart.Y - 1;
                        Cart.Direction := Cart_Up;
                     when Cart_Down =>
                        Cart.X := Cart.X - 1;
                        Cart.Direction := Cart_Left;
                     when Cart_Right =>
                        Cart.Y := Cart.Y + 1;
                        Cart.Direction := Cart_Down;
                  end case; -- Cart.Direction
            end case; -- Cart.Intersection_Count
            Cart.Intersection_Count := Cart.Intersection_Count + 1;
         end Do_Intersection;

      begin -- Update_Carts
         Sort_Carts (Cart_Array);
         for I in Cart_Indices loop
            case Cart_Array (I).Direction is
               when Cart_Up =>
                  case Track (Cart_Array (I).X, Cart_Array (I).Y) is
                     when Track_Vertical =>
                        Cart_Array (I).Y := Cart_Array (I).Y - 1;
                     when Track_Fs =>
                        Cart_Array (I).X := Cart_Array (I).X + 1;
                        Cart_Array (I).Direction := Cart_Right;
                     when Track_Bs =>
                        Cart_Array (I).X := Cart_Array (I).X - 1;
                        Cart_Array (I).Direction := Cart_Left;
                     when Track_Intersection =>
                        Do_Intersection (Cart_Array (I));
                     when others =>
                        Assert (False, "Derailed up" & Cart_Indices'Image (I));
                  end case; -- Track (Cart_Array (I).X, Cart_Array (I).Y)
               when Cart_Left =>
                  case Track (Cart_Array (I).X, Cart_Array (I).Y) is
                     when Track_Horizontal =>
                        Cart_Array (I).X := Cart_Array (I).X - 1;
                     when Track_Fs =>
                        Cart_Array (I).Y := Cart_Array (I).Y + 1;
                        Cart_Array (I).Direction := Cart_Down;
                     when Track_Bs =>
                        Cart_Array (I).Y := Cart_Array (I).Y - 1;
                        Cart_Array (I).Direction := Cart_Up;
                     when Track_Intersection =>
                        Do_Intersection (Cart_Array (I));
                     when others =>
                        Assert (False, "Derailed left" &
                                  Cart_Indices'Image (I));
                  end case; -- Track (Cart_Array (I).X, Cart_Array (I).Y)
               when Cart_Down =>
                  case Track (Cart_Array (I).X, Cart_Array (I).Y) is
                     when Track_Vertical =>
                        Cart_Array (I).Y := Cart_Array (I).Y + 1;
                     when Track_Fs =>
                        Cart_Array (I).X := Cart_Array (I).X - 1;
                        Cart_Array (I).Direction := Cart_Left;
                     when Track_Bs =>
                        Cart_Array (I).X := Cart_Array (I).X + 1;
                        Cart_Array (I).Direction := Cart_Right;
                     when Track_Intersection =>
                        Do_Intersection (Cart_Array (I));
                     when others =>
                        Assert (False, "Derailed down" &
                                  Cart_Indices'Image (I));
                  end case; -- Track (Cart_Array (I).X, Cart_Array (I).Y)
               when Cart_Right =>
                  case Track (Cart_Array (I).X, Cart_Array (I).Y) is
                     when Track_Horizontal =>
                        Cart_Array (I).X := Cart_Array (I).X + 1;
                     when Track_Fs =>
                        Cart_Array (I).Y := Cart_Array (I).Y - 1;
                        Cart_Array (I).Direction := Cart_Up;
                     when Track_Bs =>
                        Cart_Array (I).Y := Cart_Array (I).Y + 1;
                        Cart_Array (I).Direction := Cart_Down;
                     when Track_Intersection =>
                        Do_Intersection (Cart_Array (I));
                     when others =>
                        Assert (False, "Derailed right" &
                                  Cart_Indices'Image (I));
                  end case; -- Track (Cart_Array (I).X, Cart_Array (I).Y)
            end case; -- Cart_Array (I).Direction
            for Ic in Cart_Indices loop
               Collision := I /= Ic and then
                 (Cart_Array (I).X = Cart_Array (Ic).X and
                      Cart_Array (I).Y = Cart_Array (Ic).Y and
                      not Cart_Array (I).Destroyed and
                      not Cart_Array (Ic).Destroyed);
               -- Destroyed ghost carts continue to run in part two
               if Collision then
                  Cart_Array (I).Destroyed := True;
                  Cart_Array (Ic).Destroyed := True;
                  Xc := Cart_Array (I).X;
                  Yc :=Cart_Array (I).Y;
                  exit when Part_One;
               end if; -- Collision
            end loop; --  Ic in Cart_Indices
            exit when Collision and Part_One;
         end loop; -- I in Cart_Indices
      end Update_Carts;

      function Remaining_Carts (Cart_Array : in Cart_Arrays)
                                   return Natural is

         Result : Natural := 0;

      begin -- Remaining_Carts
         for I in Cart_Indices loop
            if not Cart_Array (I).Destroyed then
               Result := Result + 1;
            end if; -- not Cart_Array (I).Destroyed
         end loop; -- I in Cart_Indices
         return Result;
      end Remaining_Carts;

      procedure Last_Cart_Position (Cart_Array : in Cart_Arrays;
                                   X_Last, Y_Last : out Coordinates) is

      begin -- Last_Cart_Position
         for I in Cart_Indices loop
            if not Cart_Array (I).Destroyed then
               X_Last := Cart_Array (I).X;
               Y_Last := Cart_Array (I).Y;
            end if; -- not Cart_Array (I).Destroyed
         end loop; -- I in Cart_Indices
      end Last_Cart_Position;

      Track : Tracks;
      Cart_Array : Cart_Arrays;
      Collision : Boolean;
      Xc, Yc : Coordinates;

   begin -- Solve
      Get_Input (Input_File, Track, Cart_Array);
      loop -- move carts
         Update_Carts (Cart_Array, Track, Collision, Xc, Yc);
         exit when Collision;
      end loop; -- move carts
      Put_Line ("First collision:" & Coordinates'Image (Xc) & ',' &
                  Coordinates'Image (Yc));
      Get_Input (Input_File, Track, Cart_Array);
      loop -- moving carts part 2
         Update_Carts (Cart_Array, Track, Collision, Xc, Yc, False);
         exit when Remaining_Carts (Cart_Array) = 1;
      end loop; -- moving carts part 2
      Last_Cart_Position (Cart_Array, Xc, Yc);
      Put_Line ("Last Cart (Part_Two):" & Coordinates'Image (Xc) & ',' &
                  Coordinates'Image (Yc));
   end Solve;

   Input_File : File_Type;
   Text : Unbounded_String;
   Track_X_Size, Track_Y_Size, Track_Size, Cart_Count : Natural;

begin -- December_13
   Open (Input_File, In_File, "December_13.txt");
   Find_Limits (Input_File, Track_X_Size, Track_Y_Size, Cart_Count);
   if Track_X_Size > Track_Y_Size then
      Track_Size := Track_X_Size;
   else
      Track_Size := Track_Y_Size;
   end if;
   Solve (Input_File, Track_Size, Cart_Count);
   Close (Input_File);
end December_13;
