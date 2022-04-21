with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Interfaces; use Interfaces;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_21 is
   subtype Register_Indices is Natural range 0 .. 5;
   type Sequences is (Before, After);
   subtype Operands is Unsigned_32;
   type CPU_States is array (Register_Indices) of Operands;
   type Mnemonics is (Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori,
                      Setr, Seti, Gtri, Gtir, Gtrr, Eqri, Eqir, Eqrr);

   type Instructions is record
      Mnemonic : Mnemonics;
      A, B : Operands;
      C : Register_Indices;
   end record; -- CPU_Samples

   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   package Reg_IO is new Ada.Text_IO.Modular_IO (Operands);
   package Index_IO is new Ada.Text_IO.Integer_IO (Register_Indices);

   package Program_Stores is new
     Ada.Containers.Vectors (Natural, Instructions);
   use Program_Stores;

   procedure Get_Input (Program_Store : out Program_Stores.Vector;
                        IP_Index : out Register_Indices) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Instruction : Instructions;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "December_21.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Get_Line (Input_File, Text);
      Start_At := 1;
      Find_Token (Text,Decimal_Digit_Set, Start_At, Inside, First, Last);
      IP_Index := Register_Indices'Value (Slice (Text, First, Last));
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Letter_Set, Start_At, Inside, First, Last);
         Instruction.Mnemonic := Mnemonics'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Instruction.A := Operands'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Instruction.B := Operands'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Instruction.C :=  Register_Indices'Value (Slice (Text, First, Last));
         Append (Program_Store, Instruction);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   procedure Addr (CPU_State : in out CPU_States; A, B : in Operands;
                   C : in Register_Indices) is

   begin -- Addr
      CPU_State (C) := CPU_State (Register_Indices (A))
        + CPU_State (Register_Indices (B));
   end Addr;

   procedure Addi (CPU_State : in out CPU_States; A, B : in Operands;
                   C : in Register_Indices) is

   begin -- Addi
      CPU_State (C) := CPU_State (Register_Indices (A)) + B;
   end Addi;

   procedure Mulr (CPU_State : in out CPU_States; A, B : in Operands;
                   C : in Register_Indices) is

   begin -- Mulr
      CPU_State (C) := CPU_State (Register_Indices (A))
        * CPU_State (Register_Indices (B));
   end Mulr;

   procedure Muli (CPU_State : in out CPU_States; A, B : in Operands;
                   C : in Register_Indices) is

   begin -- Muli
      CPU_State (C) := CPU_State (Register_Indices (A)) * B;
   end Muli;

   procedure Banr (CPU_State : in out CPU_States; A, B : in Operands;
                   C : in Register_Indices) is

   begin -- Banr
      CPU_State (C) := CPU_State (Register_Indices (A))
        and CPU_State (Register_Indices (B));
   end Banr;

   procedure Bani (CPU_State : in out CPU_States; A, B : in Operands;
                   C : in Register_Indices) is

   begin -- Bani
      CPU_State (C) := CPU_State (Register_Indices (A)) and B;
   end Bani;

   procedure Borr (CPU_State : in out CPU_States; A, B : in Operands;
                   C : in Register_Indices) is

   begin -- Borr
      CPU_State (C) := CPU_State (Register_Indices (A))
        or CPU_State (Register_Indices (B));
   end Borr;

   procedure Bori (CPU_State : in out CPU_States; A, B : in Operands;
                   C : in Register_Indices) is

   begin -- Bori
      CPU_State (C) := CPU_State (Register_Indices (A)) or B;
   end Bori;

   procedure Setr (CPU_State : in out CPU_States; A : in Operands;
                   C : in Register_Indices) is

   begin -- Setr
      CPU_State (C) := CPU_State (Register_Indices (A));
   end Setr;

   procedure Seti (CPU_State : in out CPU_States; A : in Operands;
                   C : in Register_Indices) is

   begin -- Seti
      CPU_State (C) := A;
   end Seti;

   procedure Gtir (CPU_State : in out CPU_States; A, B : in Operands;
                   C : in Register_Indices) is

   begin -- Gtir
      if A > CPU_State (Register_Indices (B)) then
         CPU_State (C) := 1;
      else
         CPU_State (C) := 0;
      end if;
   end Gtir;

   procedure Gtri (CPU_State : in out CPU_States; A, B : in Operands;
                   C : in Register_Indices) is

   begin -- Gtri
      if CPU_State (Register_Indices (A)) > B then
         CPU_State (C) := 1;
      else
         CPU_State (C) := 0;
      end if;
   end Gtri;

   procedure Gtrr (CPU_State : in out CPU_States; A, B : in Operands;
                   C : in Register_Indices) is

   begin -- Gtrr
      if CPU_State (Register_Indices (A))
        > CPU_State (Register_Indices (B)) then
         CPU_State (C) := 1;
      else
         CPU_State (C) := 0;
      end if;
   end Gtrr;

   procedure Eqir (CPU_State : in out CPU_States; A, B : in Operands;
                   C : in Register_Indices) is

   begin -- Eqir
      if A = CPU_State (Register_Indices (B)) then
         CPU_State (C) := 1;
      else
         CPU_State (C) := 0;
      end if;
   end Eqir;

   procedure Eqri (CPU_State : in out CPU_States; A, B : in Operands;
                   C : in Register_Indices) is

   begin -- Eqri
      if CPU_State (Register_Indices (A)) = B then
         CPU_State (C) := 1;
      else
         CPU_State (C) := 0;
      end if;
   end Eqri;

   procedure Eqrr (CPU_State : in out CPU_States; A, B : in Operands;
                   C : in Register_Indices) is

   begin -- Eqrr
      if CPU_State (Register_Indices (A))
        = CPU_State (Register_Indices (B)) then
         CPU_State (C) := 1;
      else
         CPU_State (C) := 0;
      end if;
   end Eqrr;

   pragma Inline_Always (Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori,
                         Setr, Seti, Gtri, Gtir, Gtrr, Eqri, Eqir, Eqrr);

   procedure Interpret (Program_Store : in Program_Stores.Vector;
                        IP_Index : in Register_Indices;
                        Part_One, Part_Two : out Operands;
                        Trace_On : in Boolean := False) is

      package Termination_Sets is new Ada.Containers.Ordered_Sets (Operands);
      use Termination_Sets;

      Op_Width : constant Positive := 10;
      Reg_Width : constant Positive := 11;
      IP_Width : constant Positive := 2;
      Index_Width : constant Positive := 2;
      IP : Integer := 0;
      CPU_State : CPU_States := (others => 0);
      Trace_File : File_Type;
      Termination_Set : Termination_Sets.Set := Termination_Sets.Empty_Set;
      Comp_Instr : constant Integer := 28;
      Comp_Reg : constant Register_Indices :=
        Register_Indices (Program_Store (Comp_Instr).A);

   begin -- Interpret
      if Trace_On then
         Create (Trace_File, Out_File, "December_21_Trace.txt");
         Put_Line (Trace_File, "IP bound to register:" & IP_Index'Img);
         for I in Natural range 0 .. Last_Index (Program_Store) loop
            Int_IO.Put (Trace_File, I, IP_Width);
            Put (Trace_File,": " &
                   Mnemonics'Image (Program_Store (I).Mnemonic));
            Reg_IO.Put (Trace_File, Program_Store (I).A, Op_Width);
            Reg_IO.Put (Trace_File, Program_Store (I).B, Op_Width);
            Index_IO.Put (Trace_File, Program_Store (I).C, Index_Width);
            New_Line (Trace_File);
         end loop; -- I in Natural range 0 .. Last_Index (Program_Store)
      end if; -- Trace_On
      while IP >= 0 and IP <= Last_Index (Program_Store) loop
         CPU_State (IP_Index) := Operands (IP);
         -- Find register 0 values that cause termination.
         if IP = Comp_Instr then
            if Is_Empty (Termination_Set) then
               Part_One := CPU_State (Comp_Reg);
            end if; -- Is_Empty (Termination_Set)
            if Contains (Termination_Set, CPU_State (Comp_Reg)) then
               CPU_State (0) := CPU_State (Comp_Reg);
               -- Force termination
            else
               Insert (Termination_Set, CPU_State (Comp_Reg));
               Part_Two := CPU_State (Comp_Reg);
               -- Save the last non repeating value
            end if; -- Contains (Termination_Set, CPU_State (Comp_Reg))
         end if; -- IP = Comp_Instr
         case Program_Store (IP).Mnemonic is
            when Addr => Addr (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).B, Program_Store (IP).C);
            when Addi => Addi (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).B, Program_Store (IP).C);
            when Mulr => Mulr (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).B, Program_Store (IP).C);
            when Muli => Muli (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).B, Program_Store (IP).C);
            when Banr => Banr (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).B, Program_Store (IP).C);
            when Bani => Bani (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).B, Program_Store (IP).C);
            when Borr => Borr (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).B, Program_Store (IP).C);
            when Bori => Bori (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).B, Program_Store (IP).C);
            when Setr => Setr (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).C);
            when Seti => Seti (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).C);
            when Gtri => Gtri (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).B, Program_Store (IP).C);
            when Gtir => Gtir (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).B, Program_Store (IP).C);
            when Gtrr => Gtrr (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).B, Program_Store (IP).C);
            when Eqri => Eqri (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).B, Program_Store (IP).C);
            when Eqir => Eqir (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).B, Program_Store (IP).C);
            when Eqrr => Eqrr (CPU_State, Program_Store (IP).A,
                               Program_Store (IP).B, Program_Store (IP).C);
         end case; -- Program_Store (IP).Mnemonic
         if Trace_On then
            Int_IO.Put (Trace_File, IP, IP_Width);
            Put (Trace_File, ": " & Program_Store (IP).Mnemonic'Img);
            Reg_IO.Put (Trace_File, Program_Store (IP).A, Op_Width);
            Reg_IO.Put (Trace_File, Program_Store (IP).B, Op_Width);
            Index_IO.Put (Trace_File, Program_Store (IP).C, Index_Width);
            Put (Trace_File," [");
            for I in Register_Indices loop
               Reg_IO.Put (Trace_File, CPU_State (I), Reg_Width);
            end loop; -- I in Register_Indices
            Put_Line (Trace_File, "]");
         end if; -- Start_Trace
         IP := Integer (CPU_State (IP_Index));
         IP := IP + 1;
      end loop; -- IP >= 0 and IP <= Last_Index (Program_Store))
      if Trace_On then
         Put_Line (Trace_File, "** Halted **");
         Close (Trace_File);
      end if; -- Trace_On
      Put_Line ("Number of values that cause termination:" &
                  Length (Termination_Set)'Img);
   end Interpret;

   Program_Store : Program_Stores.Vector;
   IP_Index : Register_Indices;
   Part_One, Part_Two : Operands;

begin -- December_21
   Get_Input (Program_Store, IP_Index);
   Interpret (Program_Store, IP_Index, Part_One, Part_Two);
   Put_Line ("Part one:" & Part_One'Img);
   Put_Line ("Part two:" & Part_Two'Img);
   Put_CPU_Time;
end December_21;
