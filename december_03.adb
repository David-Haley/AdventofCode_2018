with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_03 is

   subtype Coordinates is Natural range 0 .. 1000;

   type Claims is record
      Claim_Number : Positive;
      X1, X2, Y1, Y2 : Coordinates;
   end record; -- Claims

   type Fabrics is Array (Coordinates, Coordinates) of Natural;

   function Get_Claim (Input_File : in out File_Type) return Claims is

      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Result : Claims;

   begin -- Get_Claim
      Get_Line (Input_File, Text);
      Start_At := 1;
      Assert (Element (Text, Start_At) = '#', "Missing #");
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Result.Claim_Number := Positive'Value (Slice (Text, First, Last));
      Last := Index (Text, "@", Last);
      Assert (Element (Text, Last) = '@', "Missing @");
      Start_At := Last + 1;
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Result.X1 := Natural'Value (Slice (Text, First, Last));
      Start_At := Last + 1;
      Assert (Element (Text, Start_At) = ',', "Missing ,");
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Result.Y1 := Natural'Value (Slice (Text, First, Last));
      Start_At := Last + 1;
      Assert (Element (Text, Start_At) = ':', "Missing :");
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Result.X2 := Result.X1 - 1 + Natural'Value (Slice (Text, First, Last));
      Start_At := Last + 1;
      Assert (Element (Text, Start_At) = 'x', "Missing x");
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Result.Y2 := Result.Y1 - 1 + Natural'Value (Slice (Text, First, Last));
      return Result;
   end Get_Claim;

   procedure Mark_Claim (Fabric : in out Fabrics; Claim : in Claims) is

   begin -- Mark_Claim
      for X in Coordinates range Claim.X1 .. Claim.X2 loop
         for Y in Coordinates range Claim.Y1 .. Claim.Y2 loop
            Fabric (X, Y) := Fabric (X, Y) + 1;
         end loop; -- Y in Coordinates range Claim.Y1 .. Claim.Y2
      end loop; -- X in Coordinates range Claim.X1 .. Claim.X2
   end Mark_Claim;

   function Duplicate_Count (Fabric : in Fabrics) return Natural is

      Result : Natural := 0;

   begin -- Duplicate_Count
      for X in Coordinates loop
         for Y in Coordinates loop
            if Fabric (X, Y) >= 2 then
               Result := Result + 1;
            end if; -- Fabric (X, Y) >= 2
         end loop; -- Y in Coordinates
      end loop; -- X in Coordinates
      return Result;
   end Duplicate_Count;

   function Find_Unique (Fabric : in Fabrics;
                         Input_File : in out File_Type) return Positive is

      Claim : Claims;
      ALL_One : Boolean;
      Result  : Positive := Positive'Last;

   begin -- Find_Unique
      while not End_Of_File (Input_File) loop
         Claim := Get_Claim (Input_File);
         All_One := True;
         for X in Coordinates range Claim.X1 .. Claim.X2 loop
            for Y in Coordinates range Claim.Y1 .. Claim.Y2 loop
               All_One := All_One and Fabric (X, Y) = 1;
            end loop; -- Y in Coordinates range Claim.Y1 .. Claim.Y2
         end loop; -- X in Coordinates range Claim.X1 .. Claim.X2
         if ALL_One then
            Assert (Result = Positive'Last, "Non overlaping not unique");
            Result := Claim.Claim_Number;
         end if; -- All_One
      end Loop; --  not End_Of_File (Input_File)
      return Result;
   end Find_Unique;

   Input_File : File_Type;
   Fabric : Fabrics;

begin -- Dercember_03
   for X in Coordinates loop
      for Y in Coordinates loop
         Fabric (X, Y) := 0;
      end loop; -- Y in Coordinates
   end loop; -- X in Coordinates
   Open (Input_File, In_File, "December_03.txt");
   while not End_Of_File (Input_File) loop
      Mark_Claim (Fabric, Get_Claim (Input_File));
   end Loop; --  not End_Of_File (Input_File)
   Put_Line ("Duplicate Claim Area:" &
               Natural'Image (Duplicate_Count (Fabric)));
   Reset (Input_File);
   Put_Line ("Non Overlaping ID:" &
               Positive'Image (Find_Unique (Fabric, Input_File)));
   Close (Input_File);
end December_03;
