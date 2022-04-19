with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Interfaces; use Interfaces;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_19 is
   subtype Register_Indices is Natural range 0 .. 5;
   type Sequences is (Before, After);
   subtype Operands is Unsigned_32;
   type CPU_States is array (Register_Indices) of Operands;
   type Mnemonics is (Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori,
                      Setr, Seti, Gtri, Gtir, Gtrr, Eqri, Eqir, Eqrr);

   type Instructions is record
      Mnemonic : Mnemonics;
      A, B, C : Operands;
   end record; -- CPU_Samples

   subtype Instruction_Pointers is Natural;

   package Program_Stores is new
     Ada.Containers.Vectors (Instruction_Pointers, Instructions);
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
         Open (Input_File, In_File, "December_19.txt");
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
         Instruction.C := Operands'Value (Slice (Text, First, Last));
         Append (Program_Store, Instruction);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   procedure Addr (CPU_State : in out CPU_States; A, B, C : in Operands) is

   begin -- Addr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        + CPU_State (Register_Indices (B));
   end Addr;

   procedure Addi (CPU_State : in out CPU_States; A, B, C : in Operands) is

   begin -- Addi
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A)) + B;
   end Addi;

   procedure Mulr (CPU_State : in out CPU_States; A, B, C : in Operands) is

   begin -- Mulr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        * CPU_State (Register_Indices (B));
   end Mulr;

   procedure Muli (CPU_State : in out CPU_States; A, B, C : in Operands) is

   begin -- Muli
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A)) * B;
   end Muli;

   procedure Banr (CPU_State : in out CPU_States; A, B, C : in Operands) is

   begin -- Banr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        and CPU_State (Register_Indices (B));
   end Banr;

   procedure Bani (CPU_State : in out CPU_States; A, B, C : in Operands) is

   begin -- Bani
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        and B;
   end Bani;

   procedure Borr (CPU_State : in out CPU_States; A, B, C : in Operands) is

   begin -- Borr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        or CPU_State (Register_Indices (B));
   end Borr;

   procedure Bori (CPU_State : in out CPU_States; A, B, C : in Operands) is

   begin -- Bori
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A)) or B;
   end Bori;

   procedure Setr (CPU_State : in out CPU_States; A, C : in Operands) is

   begin -- Setr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A));
   end Setr;

   procedure Seti (CPU_State : in out CPU_States; A, C : in Operands) is

   begin -- Seti
      CPU_State (Register_Indices (C)) := A;
   end Seti;

   procedure Gtir (CPU_State : in out CPU_States; A, B, C : in Operands) is

   begin -- Gtir
      if A > CPU_State (Register_Indices (B)) then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Gtir;

   procedure Gtri (CPU_State : in out CPU_States; A, B, C : in Operands) is

   begin -- Gtri
      if CPU_State (Register_Indices (A)) > B then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Gtri;

   procedure Gtrr (CPU_State : in out CPU_States; A, B, C : in Operands) is

   begin -- Gtrr
      if CPU_State (Register_Indices (A))
        > CPU_State (Register_Indices (B)) then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Gtrr;

   procedure Eqir (CPU_State : in out CPU_States; A, B, C : in Operands) is

   begin -- Eqir
      if A = CPU_State (Register_Indices (B)) then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Eqir;

   procedure Eqri (CPU_State : in out CPU_States; A, B, C : in Operands) is

   begin -- Eqri
      if CPU_State (Register_Indices (A)) = B then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Eqri;

   procedure Eqrr (CPU_State : in out CPU_States; A, B, C : in Operands) is

   begin -- Eqrr
      if CPU_State (Register_Indices (A))
        = CPU_State (Register_Indices (B)) then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Eqrr;

   pragma Inline_Always (Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori,
                         Setr, Seti, Gtri, Gtir, Gtrr, Eqri, Eqir, Eqrr);

   function Interpret (Program_Store : in Program_Stores.Vector;
                       IP_Index : in Register_Indices;
                       Initial_Register_0 : in Operands := 0)
                       return CPU_States is

      IP : Integer := 0;
      CPU_State : CPU_States := (0 => Initial_Register_0, others => 0);
      Previous_Register_0 : Operands := CPU_State (0);

   begin -- Interpret
      for I in Instruction_Pointers range 0 .. Last_Index (Program_Store) loop
         Put_Line (I'Img & ": " & Program_Store (I).Mnemonic'Img &
                     Program_Store (I).A'Img & Program_Store (I).B'Img &
                     Program_Store (I).C'Img);
      end loop; -- I in Instruction_Pointers range 0 .. Last_Index ...
      while IP >= 0 and IP <= Last_Index (Program_Store) loop
         CPU_State (IP_Index) := Operands (IP);
         case Constant_Reference (Program_Store, IP).Mnemonic is
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
         end case; -- Constant_Reference (Program_Store, IP).Mnemonic)
         if Initial_Register_0 = 1 and
           Previous_Register_0 /= CPU_State (0) then
            -- Part two early termination
            return CPU_State;
         end if; -- Initial_Register_0 = 1 and
         IP := Integer (CPU_State (IP_Index));
         IP := IP + 1;
      end loop; -- IP >= 0 and IP <= Last_Index (Program_Store)
      return CPU_State;
   end Interpret;

   function Factor (Number : in Operands) return Positive is

   -- Observation when register 0 is modified register 1 contains a factor of
   -- the value in register 2. Register 0 contains the sum of factors of of the
   -- value in register 2.

      I : Positive := 2;
      Sum_of_Factors : Positive := 1 + Positive (Number);


   begin -- Factor
      while I <= Positive (Number / 2) loop
         if Positive (Number) mod I = 0 then
            Sum_of_Factors := Sum_of_Factors + I;
         end if; -- Positive (Number) mod I = 0
         I := I + 1;
      end loop; -- I <= Positive (Number / 2)
      return Sum_of_Factors;
   end Factor;

   Program_Store : Program_Stores.Vector;
   IP_Index : Register_Indices;
   CPU_State : CPU_States;

begin -- December_19
   Get_Input (Program_Store, IP_Index);
   CPU_State := Interpret (Program_Store, IP_Index);
   Put_Line ("Part one:" & CPU_State (0)'Img);
   Put_CPU_Time;
   CPU_State := Interpret (Program_Store, IP_Index, 1);
   Put_Line ("Part two " & Factor (CPU_State (2))'Img);
   Put_CPU_Time;
end December_19;
