with Ada.Assertions; use Ada.Assertions;

package body Solver_06 is

   subtype X_Coordinates is Natural range Min_X .. Max_X;
   subtype Y_Coordinates is Natural range Min_Y .. Max_Y;
   subtype Danger_Indices is Positive range 1 .. Dangerous_Count;
   subtype Owner_IDs is Natural range 0 .. Danger_Indices'Last;
   No_Owner : constant Owner_IDs := 0;
   Distance_Limit : constant Natural := 10000;

   type Dangers is record
      X : X_Coordinates;
      Y : Y_Coordinates;
      Area : Natural := 0;
      Is_Infinite : Boolean := False;
   end record; -- Dangers

   type Danger_Lists is array (Danger_Indices) of Dangers;

   type Owners is array (X_Coordinates, Y_Coordinates) of Owner_IDs;

   function Distance (Danger_List : in Danger_Lists;
                      Danger_Index : in Danger_Indices;
                      X : in X_Coordinates;
                      Y : in Y_Coordinates) return Natural is

   begin -- Distance
      return abs (Danger_List (Danger_Index).X - X) +
        abs (Danger_List (Danger_Index).Y - Y);
   end  Distance;

   procedure Build_Owners (Danger_List : in Danger_Lists;
                           Owner : in out Owners) is

      Shortest : Natural;
      Distance_Table : array (Danger_Indices) of Natural;

   begin -- Build_Owners
      for Y in Y_Coordinates loop
         for X in X_Coordinates loop
            Shortest := Natural'Last;
            for D in Danger_Indices loop
               Distance_Table (D) := Distance (Danger_List, D, X, Y);
               if Distance_Table (D) < Shortest then
                  Shortest := Distance_Table (D);
                  Owner (X, Y) := D;
               end if; -- Distance_Table (D) < Shortest
            end loop; -- D in Danger_Indices
            for D in Danger_Indices loop
               if D /= Owner (X, Y) and Shortest = Distance_Table (D) then
                  Owner (X, Y) := No_Owner;
               end if; --  D /= Owner (X, Y) and Shortest = Distance_Table D)
            end loop; -- D in Danger_Indices
         end loop; -- X in X_Coordinates
      end loop; -- Y in Y_Coordinates
   end Build_Owners;

   procedure Update_Danger (Danger_List : in out Danger_Lists;
                           Owner : in Owners) is

   begin -- Update_Danger
      for X in X_Coordinates loop
         if Owner (X, Y_Coordinates'First) /= No_Owner then
            Danger_List (Owner (X,  Y_Coordinates'First)).Is_Infinite := True;
         end if; -- Owner (X, Y_Coordinates'First) /= No_Owner
         if Owner (X, Y_Coordinates'Last) /= No_Owner then
            Danger_List (Owner (X, Y_Coordinates'Last)).Is_Infinite := True;
         end if; -- Owner (X, Y_Coordinates'Last) /= No_Owner
      end loop; -- X in X_Coordinates
      -- infinite if it has an area including the "Owner" array bounds
      for Y in Y_Coordinates loop
         if Owner (X_Coordinates'First, Y) /= No_Owner then
            Danger_List (Owner (X_Coordinates'First, Y)).Is_Infinite := True;
         end if; -- Owner (X_Coordinates'First, Y) /= No_Owner
         if Owner (X_Coordinates'Last, Y) /= No_Owner then
            Danger_List (Owner (X_Coordinates'Last, Y)).Is_Infinite := True;
         end if; -- Owner (X_Coordinates'Last, Y) /= No_Owner
      end loop; -- X in X_Coordinates
      for X in X_Coordinates loop
         for Y in Y_Coordinates loop
            if Owner (X, Y) /= No_Owner then
               Danger_List (Owner (X, Y)).Area :=
                 Danger_List (Owner (X, Y)).Area + 1;
            end if; -- Owner (X, Y) /= No_Owner
         end loop; -- Y in Y_Coordinates
      end loop; -- X in X_Coordinates
   end Update_Danger;

   Danger_Count : Natural := 0;
   Danger_List : Danger_Lists;
   Owner : Owners;

   procedure Add_Dangerous (X : in Natural; Y : in Natural) is

   begin -- Add_Dangerous
      Danger_Count := Danger_Count + 1;
      Danger_List (Danger_Count).X := X;
      Danger_List (Danger_Count).Y := Y;
   end Add_Dangerous;

   function Largest_Area return Natural is

      Result : Natural := 0;

   begin -- Largest_Area
      Assert (Danger_Count = Danger_Indices'Last, "Danger List not Full");
      Build_Owners (Danger_List, Owner);
      Update_Danger (Danger_List, Owner);
      for D in Danger_Indices loop
         if Result < Danger_List (D).Area and
           not Danger_List (D).Is_Infinite then
            Result := Danger_List (D).Area;
         end if; -- find maximum
      end loop; -- D in Danger_Indices
      return Result;
   end Largest_Area;

   function Distance_Count return Natural is

      Total_Distance : Natural;
      Count : Natural := 0;

   begin -- Distance_Count
      Assert (Danger_Count = Danger_Indices'Last, "Danger List not Full");
      for Y in Y_Coordinates loop
         for X in X_Coordinates loop
            Total_Distance := 0;
            for D in Danger_Indices loop
               Total_Distance := Total_Distance +
                 Distance (Danger_List, D, X, Y);
            end loop; -- D in Danger_Indices
            if Total_Distance < Distance_Limit then
               Count := Count + 1;
            end if; --  Total_Distance < Distance_Limit
         end loop; -- X in X_Coordinates
      end loop; -- Y in Y_Coordinates
      return Count;
   end Distance_Count;

begin -- Solver
   for X in X_Coordinates loop
      for Y in Y_Coordinates loop
         Owner (X, Y) := No_Owner;
      end loop; -- X in X_Coordinates
   end loop; -- Y in Y_Coordinates
end Solver_06;
