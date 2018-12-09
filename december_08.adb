with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_08 is

   package Node_Value_Arrays is new
     Ada.Containers.Vectors (Element_Type => Natural,
                            Index_Type => Natural);
   use Node_Value_Arrays;

   procedure Process_Tree (Text : in out Unbounded_String;
                           Start_At : in out Positive;
                           Total : Out Natural) is

      Child_Count, Meta_Data_Count, Meta_Data, Returned_Total, Last : Natural;
      First : Positive;

   begin -- Process_Tree
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Child_Count := Natural'Value (Slice (Text, First, Last));
      Start_At := Last + 1;
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Meta_Data_Count := Natural'Value (Slice (Text, First, Last));
      Start_At := Last + 1;
      Total := 0;
      for I in Natural range 1 .. Child_Count loop
         Process_Tree (Text, Start_At, Returned_Total);
         Total := Total + Returned_Total;
      end loop; -- I in Natural range 1 .. Child_Count
      for I in Natural Range 1 .. Meta_Data_Count loop
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Meta_Data := Natural'Value (Slice (Text, First, Last));
         Total := Total + Meta_Data;
         Start_At := Last + 1;
      end loop; -- I in Natural Range 1 .. Meta_Data_Count
   end Process_Tree;

   procedure Process_Tree_2 (Text : in out Unbounded_String;
                           Start_At : in out Positive;
                           Node_Value : Out Natural) is

      Child_Count, Meta_Data_Count, Meta_Data, Returned_Node_Value,
      Last : Natural;
      First : Positive;

      Node_Value_Array : Node_Value_Arrays.Vector;

   begin -- Process_Tree_2
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Child_Count := Natural'Value (Slice (Text, First, Last));
      Start_At := Last + 1;
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Meta_Data_Count := Natural'Value (Slice (Text, First, Last));
      Start_At := Last + 1;
      Set_Length (Node_Value_Array, Count_Type (Child_Count));
      for I in Natural range 1 .. Child_Count loop
         Process_Tree_2 (Text, Start_At, Returned_Node_Value);
         Insert (Node_Value_Array, I, Returned_Node_Value);
      end loop; -- I in Natural range 1 .. Child_Count
      Node_Value := 0;
      if Child_Count = 0 then
         for I in Natural Range 1 .. Meta_Data_Count loop
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Meta_Data := Natural'Value (Slice (Text, First, Last));
            Node_Value := Node_Value + Meta_Data;
            Start_At := Last + 1;
         end loop; -- I in Natural Range 1 .. Meta_Data_Count
      else
         for I in Natural Range 1 .. Meta_Data_Count loop
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Meta_Data := Natural'Value (Slice (Text, First, Last));
            if Meta_Data <= Child_Count then
               Node_Value := Node_Value + Element (Node_Value_Array,Meta_Data);
            end if; -- Meta_Data <= Child_Count
            Start_At := Last + 1;
         end loop; -- I in Natural Range 1 .. Meta_Data_Count
      end if; -- Child_Count = 0
   end Process_Tree_2;

   Input_File : File_Type;
   Text : Unbounded_String;
   Start_At : Positive;
   Total, Node_Value : Natural;

begin -- Dercember_08
   Open (Input_File, In_File, "December_08.txt");
   Get_Line (Input_File, Text);
   Close (Input_File);
   Start_At := 1;
   Process_Tree (Text, Start_At, Total);
   Put_Line ("Meta data total:" & Natural'Image (Total));
   Start_At := 1;
   Process_Tree_2 (Text, Start_At, Node_Value);
   Put_Line ("Meta data total:" & Natural'Image (Node_Value));
end December_08;
