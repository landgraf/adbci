--
--  (c) Copyright 2011, John Vinters
--
--  ADBCI is free software; you can redistribute it and/or 
--  modify it under the terms of the GNU Lesser General Public License 
--  as published by the Free Software Foundation; either version 3, or 
--  (at your option) any later version.  
--
--  ADBCI is distributed in the hope that it will be useful, but WITHOUT ANY 
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
--  FOR A PARTICULAR PURPOSE.
--
--  You should have received a copy of the GNU Lesser General Public License
--  along with this library; if not, see <http://www.gnu.org/licenses/>
--
--    db-active_record-models-queries.adb   jvinters   17-January-2011
--

with DB.Errors;

package body DB.Active_Record.Models.Queries is

   Model                : Model_Type;

   -----------
   -- Count --
   -----------

   function Count (This : in Query_Result) return Natural is
   begin
      return This.Count;
   end Count;

   ----------
   -- Find --
   ----------

   function Find
     (Connection        : in DB.Connector.Connection;
      Criteria          : in DB.Active_Record.Fields.Field_Criteria;
      For_Update        : in Boolean := False;
      Read_Only         : in Boolean := False;
      Ordering          : in DB.Active_Record.Fields.Order_Criteria :=
                            DB.Active_Record.Fields.Null_Order_Criteria)
     return Query_Result
   is
      use DB.Active_Record.Fields;
      Order             : constant String := To_String (Ordering);
      Query_SQL         : Unbounded_String;
      Tables            : Unbounded_String;
      Where_Clause      : Unbounded_String;
   begin
      Set_Unbounded_String (Query_SQL, "SELECT ");
      Append (Query_SQL, Model.Get_Name & '.' & Model.Get_Id_Name);
      Append (Query_SQL, " FROM ");
      To_Query (Criteria, Connection, Tables, Where_Clause);
      Append (Query_SQL, Tables);
      Append (Query_SQL, " WHERE ");
      Append (Query_SQL, Where_Clause);
      if For_Update then
         Append (Query_SQL, "FOR UPDATE");
      end if;

      Append (Query_SQL, " ORDER BY ");
      if Order'Length = 0 then
         Append (Query_SQL, Model.Get_Name & '.' & Model.Get_Id_Name);
      else
         Append (Query_SQL, Order);
      end if;

      return SQL_Query
        (Connection, 
         DB.Types.SQL_String (To_String (Query_SQL)), 
         Lazy_Fetch     => True, 
         Read_Only      => Read_Only);
   end Find;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (This              : in Query_Result;
      Connection        : in DB.Connector.Connection;
      Handler           : not null Iterator)
   is
      Found             : Boolean;
      Stop              : Boolean := False;
      Temp              : Model_Type;
   begin
      for i in 1 .. This.Count loop
         begin
            Temp := Item (This, Connection, i);
            Found := True;
         exception
            when DB.Errors.OBJECT_NOT_FOUND =>
               Found := False;
         end;

         if Found then
            Handler (Temp, Stop);
         end if;

         exit when Stop;
      end loop;
   end Iterate;

   ----------
   -- Item --
   ----------

   function Item
     (This              : in Query_Result;
      Connection        : in DB.Connector.Connection;
      Index             : in Positive) return Model_Type
   is
      Id                : DB.Types.Object_Id;
      Temp              : Model_Type;
      Column_Id         : constant String := Temp.Get_Id_Name;
   begin
      if Index > This.Count then
         raise CONSTRAINT_ERROR with "attempt to read past end of results";
      else
         if This.Lazy_Fetched then
            Id := This.Items.Get_Object_Id_At
              (Column_Id, Tuple_Index (Index), True, 0);
            if Id /= 0 then
               Temp.Get (Connection, Id);
            else
               null;    --  return an empty object if column is NULL
            end if;
         else
            declare
               R        : DB.Connector.Result_Set := This.Items;
            begin
               Temp.Load_From (Connection, R, Tuple_Index (Index));
            end;
         end if;
         Temp.Set_Read_Only (This.Read_Only);
         return Temp;
      end if;
   end Item;

   ---------------
   -- SQL_Query --
   ---------------

   function SQL_Query
     (Connection        : in DB.Connector.Connection;
      Query_SQL         : in DB.Types.SQL_String;
      Lazy_Fetch        : in Boolean := True;
      Read_Only         : in Boolean := False) return Query_Result
   is
      Result            : Query_Result;
   begin
      Result.Count := 0;
      Result.Items := Connection.Execute (Query_SQL);
      Result.Count := Result.Items.Count;
      Result.Lazy_Fetched := Lazy_Fetch;
      Result.Read_Only := Read_Only;
      return Result;
   end SQL_Query;

end DB.Active_Record.Models.Queries;
