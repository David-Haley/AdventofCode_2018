with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Ordered_Sets;

procedure December_20 is

   subtype Map_Elements is Character with Static_Predicate =>
     Map_Elements in '^' | '$' | '(' | ')' | '|' | 'N' | 'S' | 'E' | 'W';

   type Rooms is record
      X, Y : Integer;
      Length : Natural;
   end record; -- Rooms

   function "<" (Left, Right : Rooms) return Boolean is

   begin -- "<"
      return Left.X < Right.X or else (Left.X = Right.X and Left.Y < Right.Y);
   end "<";

   function "=" (Left, Right : Rooms) return Boolean is

   begin -- "="
      return Left.X = Right.X and Left.Y = Right.Y;
   end "=";

   package Room_Sets is new Ada.Containers.Ordered_Sets (Rooms);
   use Room_Sets;

   procedure Parse (Input_File : in out File_Type;
                    X0, Y0 : in Integer; Length0 : in Natural;
                    Maximum_Length : in out Natural;
                    Room_Set : in out Room_Sets.Set) is

      Ch, Next_Ch : Map_Elements;
      In_Reverse, EoLn : Boolean := False;
      Room : Rooms;

   begin -- Parse
      Room.X := X0;
      Room.Y := Y0;
      Room.Length := Length0;
      loop -- process one step
         Get_Immediate (Input_File, Ch);
         case Ch is
            when '^' =>
               null;
            when 'N' | 'S' | 'E' | 'W' =>
               if In_Reverse then
                  Room.Length := Room.Length - 1;
               else
                  Room.Length := Room.Length + 1;
               end if; -- In_Reverse
               case Ch is
                  when 'N' => Room.Y := Room.Y - 1;
                  when 'S' => Room.Y := Room.Y + 1;
                  when 'E' => Room.X := Room.X + 1;
                  when 'W' => Room.X := Room.X - 1;
                  when others => null;
                     -- Unreachable required to keep the compiler happy
               end case; -- upsate coordinates
               Look_Ahead (Input_File, Next_Ch, EoLn);
               if (Ch = 'N' and Next_Ch = 'S') or
                 (Ch = 'S' and Next_Ch = 'N') or
                 (Ch = 'E' and Next_Ch = 'W') or
                 (Ch = 'W' and Next_Ch = 'E') then
                  In_Reverse := True;
                  if Room.Length > Maximum_Length then
                     Maximum_Length := Room.Length;
                  end if; -- if Room.Length > Maximum_Length then
               end if; -- change of direction
               if not Contains (Room_Set, Room) then
                  Include (Room_Set, Room);
               else
                  if Room.Length >
                    Room_Set (Find (Room_Set, Room)).Length then
                     Replace (Room_Set, Room);
                  end if; -- shorter length to same room
               end if; -- not Contains (Room_Set, Room)
            when '(' =>
               if Room.Length > Maximum_Length then
                  Maximum_Length := Room.Length;
               end if; -- Room.Length > Maximum_Length
               Parse (Input_File, Room.X, Room.Y, Room.Length, Maximum_Length,
                      Room_Set);
            when ')' =>
               if Room.Length > Maximum_Length then
                  Maximum_Length := Room.Length;
               end if; -- Null_Option
            when '|' =>
               if Room.Length > Maximum_Length then
                  Maximum_Length := Room.Length;
               end if; -- Room.Length > Maximum_Length
               Room.X := X0;
               Room.Y := Y0;
               Room.Length := Length0;
               In_Reverse := False;
            when '$' =>
               if Room.Length > Maximum_Length then
                  Maximum_Length := Room.Length;
               end if; -- Room.Length > Maximum_Length
         end case; -- Ch
         exit when Ch = ')' or Ch = '$';
      end loop; -- process one step
   end Parse;

   Input_File : File_Type;
   Maximum_Length : Natural := 0;
   Room_Set : Room_Sets.Set := Empty_Set;
   Room_Count : Natural := 0;

begin -- December_20
   Ada.Text_IO.Open (Input_File, In_File, "December_20.txt");
   Parse (Input_File, 0, 0, 0, Maximum_Length, Room_Set);
   Close (Input_File);
   Put_Line ("Maximum Length:" & Natural'Image (Maximum_Length));
   for I in Room_Set.Iterate loop
      if Room_Set (I).Length >= 1000 then
         Room_Count := Room_Count + 1;
      end if; -- Room_Set (I).Length
   end loop; -- I in Room_Set.Iterate
   Put_Line ("Rooms 1000 or more doors:" & Natural'Image (Room_Count));
end December_20;
