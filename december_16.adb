with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Interfaces; use Interfaces;
with Ada.Containers.Vectors;

procedure December_16 is
   File_Name : constant String := "December_16.txt";
   subtype Register_Indices is Natural range 0 .. 3;
   subtype Opcodes is Natural range 0 .. 15;
   type Sequences is (Before, After);
   type CPU_States is array (Register_Indices) of Unsigned_32;
   type Mnemonics is (Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori,
                      Setr, Seti, Gtri, Gtir, Gtrr, Eqri, Eqir, Eqrr);

   type CPU_Samples is record
      Before, After : CPU_States;
      Opcode : Opcodes;
      A, B, C : Unsigned_32;
      Match_Count : Natural := 0;
   end record; -- CPU_Samples

   type Histograms is array (Opcodes, Mnemonics) of Natural;

   package Sample_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural,
                             Element_Type => CPU_Samples);
   use Sample_Vectors;

   procedure Get_Input (Sample_Vector : out Sample_Vectors.Vector;
                       Last_Sample : out Positive_Count) is

      Before_String : constant String := "Before";
      After_String : constant String := "After";
      Sequence_Set : constant Character_Set :=
        To_Set (Before_String & After_String);
      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      CPU_Sample : CPU_Samples;

   begin -- Get_Input
      Open (Input_File, In_File, File_Name);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         while not End_Of_File (Input_File) and then
           index (Text, Before_String) = 0 loop
            Get_Line (Input_File, Text);
         end loop; -- Before not found
         exit when  End_Of_File (Input_File);
         Start_At := index (Text, Before_String) + Before_String'Length + 2;
         for I in Register_Indices loop
            Find_Token (Text,Decimal_Digit_Set, Start_At, Inside, First, Last);
            CPU_Sample.Before (I) :=
              Unsigned_32'Value (Slice (Text, First, Last));
            Start_At := Last + 2; -- skip ','
         end loop; -- I in Register_Indices
         Get_Line (Input_File, Text); -- Second line
         Start_At := 1;
         Find_Token (Text,Decimal_Digit_Set, Start_At, Inside, First, Last);
         CPU_Sample.Opcode := Opcodes'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text,Decimal_Digit_Set, Start_At, Inside, First, Last);
         CPU_Sample.A := Unsigned_32'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text,Decimal_Digit_Set, Start_At, Inside, First, Last);
         CPU_Sample.B := Unsigned_32'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text,Decimal_Digit_Set, Start_At, Inside, First, Last);
         CPU_Sample.C := Unsigned_32'Value (Slice (Text, First, Last));
         Get_Line (Input_File, Text); -- Third line
         Last_Sample := Line (Input_File);
         Start_At := 1;
         Find_Token (Text, Sequence_Set, Start_At, Inside, First, Last);
         Assert (Slice (Text, First, Last) = After_String, "After not found");
         Start_At := Last + 2; -- skip ':'
         for I in Register_Indices loop
            Find_Token (Text,Decimal_Digit_Set, Start_At, Inside, First, Last);
            CPU_Sample.After (I) :=
              Unsigned_32'Value (Slice (Text, First, Last));
            Start_At := Last + 2; -- skip ','
         end loop; -- I in Register_Indices
         Append (Sample_Vector, CPU_Sample);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   procedure Addr (CPU_State : in out CPU_States; A, B, C : in Unsigned_32) is

   begin -- Addr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        + CPU_State (Register_Indices (B));
   end Addr;

   procedure Addi (CPU_State : in out CPU_States; A, B, C : in Unsigned_32) is

   begin -- Addi
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A)) + B;
   end Addi;

   procedure Mulr (CPU_State : in out CPU_States; A, B, C : in Unsigned_32) is

   begin -- Mulr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        * CPU_State (Register_Indices (B));
   end Mulr;

   procedure Muli (CPU_State : in out CPU_States; A, B, C : in Unsigned_32) is

   begin -- Muli
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A)) * B;
   end Muli;

   procedure Banr (CPU_State : in out CPU_States; A, B, C : in Unsigned_32) is

   begin -- Banr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        and CPU_State (Register_Indices (B));
   end Banr;

   procedure Bani (CPU_State : in out CPU_States; A, B, C : in Unsigned_32) is

   begin -- Bani
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        and B;
   end Bani;

   procedure Borr (CPU_State : in out CPU_States; A, B, C : in Unsigned_32) is

   begin -- Borr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        or CPU_State (Register_Indices (B));
   end Borr;

   procedure Bori (CPU_State : in out CPU_States; A, B, C : in Unsigned_32) is

   begin -- Bori
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A)) or B;
   end Bori;

   procedure Setr (CPU_State : in out CPU_States; A, C : in Unsigned_32) is

   begin -- Setr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A));
   end Setr;

   procedure Seti (CPU_State : in out CPU_States; A, C : in Unsigned_32) is

   begin -- Seti
      CPU_State (Register_Indices (C)) := A;
   end Seti;

   procedure Gtir (CPU_State : in out CPU_States; A, B, C : in Unsigned_32) is

   begin -- Gtir
      if A > CPU_State (Register_Indices (B)) then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Gtir;

   procedure Gtri (CPU_State : in out CPU_States; A, B, C : in Unsigned_32) is

   begin -- Gtri
      if CPU_State (Register_Indices (A)) > B then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Gtri;

   procedure Gtrr (CPU_State : in out CPU_States; A, B, C : in Unsigned_32) is

   begin -- Gtrr
      if CPU_State (Register_Indices (A))
        > CPU_State (Register_Indices (B)) then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Gtrr;

   procedure Eqir (CPU_State : in out CPU_States; A, B, C : in Unsigned_32) is

   begin -- Eqir
      if A = CPU_State (Register_Indices (B)) then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Eqir;

   procedure Eqri (CPU_State : in out CPU_States; A, B, C : in Unsigned_32) is

   begin -- Eqri
      if CPU_State (Register_Indices (A)) = B then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Eqri;

   procedure Eqrr (CPU_State : in out CPU_States; A, B, C : in Unsigned_32) is

   begin -- Eqrr
      if CPU_State (Register_Indices (A))
        = CPU_State (Register_Indices (B)) then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Eqrr;

   procedure Apply (Sample_Vector : in out Sample_Vectors.Vector;
                   Histogram : in out Histograms) is
      -- updates Match_Count based on trying all instructions

      procedure Check (CPU_State : in CPU_States;
                       Sample_Vector : in out Sample_Vectors.Vector;
                       I : in Sample_Vectors.Cursor;
                       Mnemonic : in Mnemonics;
                       Histogram : in out Histograms) is

         procedure Update (CPU_Sample : in out CPU_Samples) is

         begin -- Update
            CPU_Sample.Match_Count := CPU_Sample.Match_Count + 1;
         end Update;

      begin -- Check
         if CPU_State = Sample_Vector (I).After then
            Update_Element (Sample_Vector, I, Update'Access);
            Histogram (Sample_Vector (I).Opcode, Mnemonic) :=
              Histogram (Sample_Vector (I).Opcode, Mnemonic) + 1;
         end if; -- CPU_State = CPU_Sample.After
      end Check;

      CPU_State :  CPU_States;

   begin -- Apply
      for O in Opcodes loop
         for M in Mnemonics loop
            Histogram (O, M) := 0;
         end loop; -- O in Opcodes
      end loop; -- M in Mnemonics
      for I in Sample_Vector.Iterate loop
         if Sample_Vector (I).B <= Unsigned_32 (Register_Indices'Last) then
            CPU_State := Sample_Vector (I).Before;
            Addr (CPU_State, Sample_Vector (I).A, Sample_Vector (I).B,
                  Sample_Vector (I).C);
            Check (CPU_State, Sample_Vector, I, Addr, Histogram);
            CPU_State := Sample_Vector (I).Before;
            Mulr (CPU_State, Sample_Vector (I).A, Sample_Vector (I).B,
                  Sample_Vector (I).C);
            Check (CPU_State, Sample_Vector, I, Mulr, Histogram);
            CPU_State := Sample_Vector (I).Before;
            Banr (CPU_State, Sample_Vector (I).A, Sample_Vector (I).B,
                  Sample_Vector (I).C);
            Check (CPU_State, Sample_Vector, I, Banr, Histogram);
            CPU_State := Sample_Vector (I).Before;
            Borr (CPU_State, Sample_Vector (I).A, Sample_Vector (I).B,
                  Sample_Vector (I).C);
            Check (CPU_State, Sample_Vector, I, Borr, Histogram);
         end if; -- Sample_Vector (I).B <= Unsigned (Register_Indices'Last)
         CPU_State := Sample_Vector (I).Before;
         Addi (CPU_State, Sample_Vector (I).A, Sample_Vector (I).B,
                  Sample_Vector (I).C);
         Check (CPU_State, Sample_Vector, I, Addi, Histogram);
         CPU_State := Sample_Vector (I).Before;
         Muli (CPU_State, Sample_Vector (I).A, Sample_Vector (I).B,
                  Sample_Vector (I).C);
         Check (CPU_State, Sample_Vector, I, Muli, Histogram);
         CPU_State := Sample_Vector (I).Before;
         Bani (CPU_State, Sample_Vector (I).A, Sample_Vector (I).B,
                  Sample_Vector (I).C);
         Check (CPU_State, Sample_Vector, I, Bani, Histogram);
         CPU_State := Sample_Vector (I).Before;
         Bori (CPU_State, Sample_Vector (I).A, Sample_Vector (I).B,
                  Sample_Vector (I).C);
         Check (CPU_State, Sample_Vector, I, Bori, Histogram);
         if Sample_Vector (I).A <= Unsigned_32 (Register_Indices'Last) then
            CPU_State := Sample_Vector (I).Before;
            Setr (CPU_State, Sample_Vector (I).A, Sample_Vector (I).C);
            Check (CPU_State, Sample_Vector, I, Setr, Histogram);
         end if; -- Sample_Vector (I).A <= Unsigned (Register_Indices'Last)
         CPU_State := Sample_Vector (I).Before;
         Seti (CPU_State, Sample_Vector (I).A, Sample_Vector (I).C);
         Check (CPU_State, Sample_Vector, I, Seti, Histogram);
         if Sample_Vector (I).B <= Unsigned_32 (Register_Indices'Last) then
            CPU_State := Sample_Vector (I).Before;
            Gtir (CPU_State, Sample_Vector (I).A, Sample_Vector (I).B,
                  Sample_Vector (I).C);
            Check (CPU_State, Sample_Vector, I, Gtir, Histogram);
            CPU_State := Sample_Vector (I).Before;
            Eqir (CPU_State, Sample_Vector (I).A, Sample_Vector (I).B,
                  Sample_Vector (I).C);
            Check (CPU_State, Sample_Vector, I, Eqir, Histogram);
         end if; -- Sample_Vector (I).B <= Unsigned (Register_Indices'Last)
         if Sample_Vector (I).A <= Unsigned_32 (Register_Indices'Last) then
            CPU_State := Sample_Vector (I).Before;
            Gtri (CPU_State, Sample_Vector (I).A, Sample_Vector (I).B,
                  Sample_Vector (I).C);
            Check (CPU_State, Sample_Vector, I, Gtri, Histogram);
            CPU_State := Sample_Vector (I).Before;
            Eqri (CPU_State, Sample_Vector (I).A, Sample_Vector (I).B,
                  Sample_Vector (I).C);
            Check (CPU_State, Sample_Vector, I, Eqri, Histogram);
         end if; -- Sample_Vector (I).A <= Unsigned (Register_Indices'Last)
         if  Sample_Vector (I).A <= Unsigned_32 (Register_Indices'Last) and
           Sample_Vector (I).B <= Unsigned_32 (Register_Indices'Last) then
            CPU_State := Sample_Vector (I).Before;
            Gtrr (CPU_State, Sample_Vector (I).A, Sample_Vector (I).B,
                  Sample_Vector (I).C);
            Check (CPU_State, Sample_Vector, I, Gtrr, Histogram);
            CPU_State := Sample_Vector (I).Before;
            Eqrr (CPU_State, Sample_Vector (I).A, Sample_Vector (I).B,
                  Sample_Vector (I).C);
            Check (CPU_State, Sample_Vector, I, Eqrr, Histogram);
         end if; -- both A and B could be registers
      end loop; -- I in Sample_Vector.Iterate
   end Apply;

   Procedure Interpret (Last_Sample : in Positive_Count) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Opcode : Opcodes;
      A, B, C : Unsigned_32;
      CPU_State : CPU_States := (others => 0);

   begin -- Interpret
      Open (Input_File, In_File, File_Name);
      while Line (Input_File) <= Last_Sample loop
         Get_Line (Input_File, Text);
      end loop;
      while Length (Text) = 0 loop
         Get_Line (Input_File, Text);
      end loop;
      loop -- process one instruction
         Start_At := 1;
         Find_Token (Text,Decimal_Digit_Set, Start_At, Inside, First, Last);
         Opcode := Opcodes'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text,Decimal_Digit_Set, Start_At, Inside, First, Last);
         A := Unsigned_32'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text,Decimal_Digit_Set, Start_At, Inside, First, Last);
         B := Unsigned_32'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text,Decimal_Digit_Set, Start_At, Inside, First, Last);
         C := Unsigned_32'Value (Slice (Text, First, Last));
         case Opcode is
            when 0 =>
               Muli (CPU_State, A, B, C);
            when 1 =>
               Bani (CPU_State, A, B, C);
            when 2 =>
               Addi (CPU_State, A, B, C);
            when 3 =>
               Seti (CPU_State, A, C);
            when 4 =>
               Eqrr (CPU_State, A, B, C);
            when 5 =>
               Eqir (CPU_State, A, B, C);
            when 6 =>
               Setr (CPU_State, A, C);
            when 7 =>
               Bori (CPU_State, A, B, C);
            when 8 =>
               Gtri (CPU_State, A, B, C);
            when 9 =>
               Eqri (CPU_State, A, B, C);
            when 10 =>
               Gtir (CPU_State, A, B, C);
            when 11 =>
               Borr (CPU_State, A, B, C);
            when 12 =>
               Addr (CPU_State, A, B, C);
            when 13 =>
               Gtrr (CPU_State, A, B, C);
            when 14 =>
               Mulr (CPU_State, A, B, C);
            when 15 =>
               Banr (CPU_State, A, B, C);
         end case; -- Opcode
         exit when End_Of_File (Input_File);
         Get_Line (Input_File, Text);
      end loop; -- process one instruction
      Put_Line ("Register 0 (Part two):" & Unsigned_32'Image (CPU_State (0)));
   end Interpret;

   Sample_Vector : Sample_Vectors.Vector;
   Three_Count : Natural := 0;
   Histogram : Histograms;
   Maximum : Natural;
   Last_Sample : Positive_Count;

begin -- December_16
   Get_Input (Sample_Vector, Last_Sample);
   Apply (Sample_Vector, Histogram);
   for I in Sample_Vector.Iterate loop
      if Sample_Vector (I).Match_Count >= 3 then
         Three_Count := Three_Count + 1;
      end if; -- Sample_Vector (I).Match_Count >= 3
   end loop; -- I in Sample_Vector.Iterate
   Put_Line ("Sample count:" & Natural'Image (Three_Count));
   for O in Opcodes loop
      Put_line ("Opcode:" & Opcodes'Image (O));
      Maximum := 0;
      for M in Mnemonics loop
         if Histogram (O, M) > Maximum then
            Maximum := Histogram (O, M);
         end if; -- Histogram (O, M) > Maximum
      end loop; -- O in Opcodes
      for M in Mnemonics loop
         if Histogram (O, M) = Maximum then
            Put (Mnemonics'Image (M) & ", ");
         end if; -- Histogram (O, M) = Maximum
      end loop; -- O in Opcodes
      New_Line;
   end loop; -- M in Mnemonics
   Interpret (Last_Sample);
end December_16;
