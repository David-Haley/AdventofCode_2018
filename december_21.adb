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

procedure December_21 is
   File_Name : constant String := "December_21.txt";
   subtype Register_Indices is Natural range 0 .. 5;
   type Sequences is (Before, After);
   type CPU_States is array (Register_Indices) of Unsigned_64;
   type Mnemonics is (Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori,
                      Setr, Seti, Gtri, Gtir, Gtrr, Eqri, Eqir, Eqrr);

   type Instructions is record
      Mnemonic : Mnemonics;
      A, B, C : Unsigned_64;
   end record; -- CPU_Samples

   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   package U64_IO is new Ada.Text_IO.Modular_IO (Unsigned_64);

   package Program_Stores is new
     Ada.Containers.Vectors (Index_Type => Natural,
                             Element_Type => Instructions);
   use Program_Stores;

   procedure Get_Input (Program_Store : out Program_Stores.Vector;
                       IP_Index : out Register_Indices) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Instruction : Instructions;

   begin -- Get_Input
      Open (Input_File, In_File, File_Name);
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
         Instruction.A := Unsigned_64'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Instruction.B := Unsigned_64'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Instruction.C := Unsigned_64'Value (Slice (Text, First, Last));
         Append (Program_Store, Instruction);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   procedure Addr (CPU_State : in out CPU_States; A, B, C : in Unsigned_64) is

   begin -- Addr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        + CPU_State (Register_Indices (B));
   end Addr;

   procedure Addi (CPU_State : in out CPU_States; A, B, C : in Unsigned_64) is

   begin -- Addi
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A)) + B;
   end Addi;

   procedure Mulr (CPU_State : in out CPU_States; A, B, C : in Unsigned_64) is

   begin -- Mulr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        * CPU_State (Register_Indices (B));
   end Mulr;

   procedure Muli (CPU_State : in out CPU_States; A, B, C : in Unsigned_64) is

   begin -- Muli
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A)) * B;
   end Muli;

   procedure Banr (CPU_State : in out CPU_States; A, B, C : in Unsigned_64) is

   begin -- Banr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        and CPU_State (Register_Indices (B));
   end Banr;

   procedure Bani (CPU_State : in out CPU_States; A, B, C : in Unsigned_64) is

   begin -- Bani
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        and B;
   end Bani;

   procedure Borr (CPU_State : in out CPU_States; A, B, C : in Unsigned_64) is

   begin -- Borr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A))
        or CPU_State (Register_Indices (B));
   end Borr;

   procedure Bori (CPU_State : in out CPU_States; A, B, C : in Unsigned_64) is

   begin -- Bori
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A)) or B;
   end Bori;

   procedure Setr (CPU_State : in out CPU_States; A, C : in Unsigned_64) is

   begin -- Setr
      CPU_State (Register_Indices (C)) := CPU_State (Register_Indices (A));
   end Setr;

   procedure Seti (CPU_State : in out CPU_States; A, C : in Unsigned_64) is

   begin -- Seti
      CPU_State (Register_Indices (C)) := A;
   end Seti;

   procedure Gtir (CPU_State : in out CPU_States; A, B, C : in Unsigned_64) is

   begin -- Gtir
      if A > CPU_State (Register_Indices (B)) then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Gtir;

   procedure Gtri (CPU_State : in out CPU_States; A, B, C : in Unsigned_64) is

   begin -- Gtri
      if CPU_State (Register_Indices (A)) > B then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Gtri;

   procedure Gtrr (CPU_State : in out CPU_States; A, B, C : in Unsigned_64) is

   begin -- Gtrr
      if CPU_State (Register_Indices (A))
        > CPU_State (Register_Indices (B)) then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Gtrr;

   procedure Eqir (CPU_State : in out CPU_States; A, B, C : in Unsigned_64) is

   begin -- Eqir
      if A = CPU_State (Register_Indices (B)) then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Eqir;

   procedure Eqri (CPU_State : in out CPU_States; A, B, C : in Unsigned_64) is

   begin -- Eqri
      if CPU_State (Register_Indices (A)) = B then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Eqri;

   procedure Eqrr (CPU_State : in out CPU_States; A, B, C : in Unsigned_64) is

   begin -- Eqrr
      if CPU_State (Register_Indices (A))
        = CPU_State (Register_Indices (B)) then
         CPU_State (Register_Indices (C)) := 1;
      else
         CPU_State (Register_Indices (C)) := 0;
      end if;
   end Eqrr;

   Procedure Interpret (Program_Store : in Program_Stores.Vector;
                        IP_Index : in Register_Indices) is

      IP : Integer := 0;
      A, B, C : Unsigned_64;
      CPU_State : CPU_States := (others => 0);
      Start_Trace : Boolean := False;
      Break_Point : Integer := Integer'Value (Argument (2));
      Break_Number : Positive := Positive'Value (Argument (3));
      Break_Count : Positive := 1;

   begin -- Interpret
      for I in Natural range 0 .. Natural (Length (Program_Store) - 1) loop
         Int_IO.Put (I, 2);
         Put (": " & Mnemonics'Image
              (Constant_Reference (Program_Store, I).Mnemonic));
         U64_IO.Put (Constant_Reference (Program_Store, I).A, 11);
         U64_IO.Put (Constant_Reference (Program_Store, I).B, 11);
         U64_IO.Put (Constant_Reference (Program_Store, I).C, 11);
         New_Line;
      end loop; -- I in Natural range 0 .. Natural (Length (Program_Store) - 1)
      CPU_State (0) := Unsigned_64'Value (Argument (1));
      while IP >= 0 and IP <= Integer (Length (Program_Store) - 1) loop
         A := Constant_Reference (Program_Store, IP).A;
         B := Constant_Reference (Program_Store, IP).B;
         C := Constant_Reference (Program_Store, IP).C;
         CPU_State (IP_Index) := Unsigned_64 (IP);
         case Constant_Reference (Program_Store, IP).Mnemonic is
            when Addr => Addr (CPU_State, A, B, C);
            when Addi => Addi (CPU_State, A, B, C);
            when Mulr => Mulr (CPU_State, A, B, C);
            when Muli => Muli (CPU_State, A, B, C);
            when Banr => Banr (CPU_State, A, B, C);
            when Bani => Bani (CPU_State, A, B, C);
            when Borr => Borr (CPU_State, A, B, C);
            when Bori => Bori (CPU_State, A, B, C);
            when Setr => Setr (CPU_State, A, C);
            when Seti => Seti (CPU_State, A, C);
            when Gtri => Gtri (CPU_State, A, B, C);
            when Gtir => Gtir (CPU_State, A, B, C);
            when Gtrr => Gtrr (CPU_State, A, B, C);
            when Eqri => Eqri (CPU_State, A, B, C);
            when Eqir => Eqir (CPU_State, A, B, C);
            when Eqrr => Eqrr (CPU_State, A, B, C);
         end case; -- Constant_Reference (Program_Store, IP).Mnemonic)
         if IP = Break_Point then
            Start_Trace := Start_Trace or Break_Count = Break_Number;
            Put_Line ("Break Count:" & Positive'Image (Break_Count) &
                       " Trigger:" & Positive'Image (Break_Number));
            Break_Count := Break_Count + 1;
         end if; -- IP = Break_Point
         if Start_Trace or IP = Break_Point then
            Int_IO.Put (IP, 2);
            Put (": " & Mnemonics'Image
                 (Constant_Reference (Program_Store, IP).Mnemonic));
            U64_IO.Put (Constant_Reference (Program_Store, IP).A, 11);
            U64_IO.Put (Constant_Reference (Program_Store, IP).B, 11);
            U64_IO.Put (Constant_Reference (Program_Store, IP).C, 11);
            Put (" [");
            for I in Register_Indices loop
               U64_IO.Put (CPU_State (I), 11);
            end loop; -- I in Register_Indices
            Put_Line ("]");
            exit when Break_Count > Break_Number + 1;
         end if; -- Start_Trace
         IP := Integer (CPU_State (IP_Index));
         IP := IP + 1;
      end loop; -- IP >= 0 and IP <= Integer (Length (Program_Store) - 1)
      Put_Line ("Register 0 :" & Unsigned_64'Image (CPU_State (0)));
   end Interpret;

   Program_Store : Program_Stores.Vector;
   IP_Index : Register_Indices;

begin -- December_21
   Get_Input (Program_Store, IP_Index);
   Interpret (Program_Store, IP_Index);
end December_21;
