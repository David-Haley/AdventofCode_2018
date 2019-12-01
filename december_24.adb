with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_24 is

   type Unit_Types : (

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
      Open (Input_File, In_File, "December_23.txt");
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Start_At := Index (Text, Pos_String, Start_At) +
           Pos_String'Length;
         Assert (Start_At > Pos_String'Length, Pos_String & " not found");
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Nano_Bot.X := Integer'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Assert (Element (Text, Start_At) = ',', ", after X");
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Nano_Bot.Y := Integer'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Assert (Element (Text, Start_At) = ',', ", after X");
         Find_Token (Text, Integer_Set, Start_At, Inside, First,Last);
         Nano_Bot.Z := Integer'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Assert (Element (Text, Start_At) = '>', "> after Z");
         Start_At := Index (Text, R_String, Start_At) +
           R_String'Length;
         Assert (Start_At > Pos_String'Length, R_String & " not found");
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Nano_Bot.R := Natural'Value (Slice (Text, First, Last));
         Append (Nano_Bot_List, Nano_Bot);
      end loop; -- not End_Of_File (Input_File
      Close (Input_File);
   end Get_Input;

begin -- December_24
   Get_Input (Nano_Bot_List);
   end loop; -- I in Nano_Bot_List.Iterate
end December_24;
