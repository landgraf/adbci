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

with DB.Connector;                     use DB.Connector;
with DB.Errors;

package body DB.Active_Record.Models.Queries is

   -----------
   -- Count --
   -----------

   function Count (This : in Query_Result) return Natural is
   begin
      return This.Count;
   end Count;

   function Count
     (Connection        : in DB.Connector.Connection;
      Criteria          : in DB.Active_Record.Fields.Field_Criteria;
      First             : in DB.Types.Object_Id := 0;
      Last              : in DB.Types.Object_Id := 0)
     return Natural
   is
      use DB.Active_Record.Fields;
      Query_SQL         : Unbounded_String;
   begin
      if First > Last then
         return 0;
      else
         Set_Unbounded_String (Query_SQL, "SELECT COUNT (");
         Append (Query_SQL, Object.Get_Name & '.' & Object.Get_Id_Name);
         Append (Query_SQL, ')');
         Append (Query_SQL, String
           (To_SQL_Criteria (Connection, Criteria, False, False,
                             Null_Order_Criteria, First, Last, 
                             No_Order => True)));
         declare
            Query       : constant DB.Types.SQL_String :=
              DB.Types.SQL_String (To_String (Query_SQL));
            R           : DB.Connector.Result_Set;
         begin
            R := Connection.Execute (Query);
            return Natural (R.Get_Bigint ("count"));
         end;
      end if;
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
                            DB.Active_Record.Fields.Null_Order_Criteria;
      First             : in DB.Types.Object_Id := 0;
      Last              : in DB.Types.Object_Id := 0)
     return Query_Result
   is
      use DB.Active_Record.Fields;
      Order             : constant String := To_String (Ordering);
      Query_SQL         : Unbounded_String;
   begin
      if First > Last then
         declare
            Null_Result : Query_Result;
         begin
            Null_Result.Count := 0;
            Null_Result.Lazy_Fetched := True;
            Null_Result.Read_Only := Read_Only;
            return Null_Result;
         end;
      else
         Set_Unbounded_String (Query_SQL, "SELECT ");
         Append (Query_SQL, Object.Get_Name & '.' & Object.Get_Id_Name);
         Append (Query_SQL, String
           (To_SQL_Criteria (Connection, Criteria, For_Update, Read_Only,
                             Ordering, First, Last)));

         return SQL_Query
           (Connection, 
            DB.Types.SQL_String (To_String (Query_SQL)), 
            Lazy_Fetch     => True, 
            Read_Only      => Read_Only);
      end if;
   end Find;

   -------------
   -- Get_SQL --
   -------------

   function Get_SQL (This : in Query_Result) return DB.Types.SQL_String is
   begin
      return DB.Types.SQL_String (To_String (This.Query));
   end Get_SQL;

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
              (Column_Id, 
               Tuple_Index (Index) + Tuple_Index (This.First), 
               True, 
               0);
            if Id /= 0 then
               Temp.Get (Connection, Id);
            else
               null;    --  return an empty object if column is NULL
            end if;
         else
            declare
               R        : DB.Connector.Result_Set := This.Items;
            begin
               Temp.Load_From (Connection, 
                               R, 
                               Tuple_Index (Index) + Tuple_Index (This.First));
            end;
         end if;
         Temp.Set_Read_Only (This.Read_Only);
         return Temp;
      end if;
   end Item;

   -----------
   -- Slice --
   -----------

   function Slice
     (This              : in Query_Result;
      First             : in DB.Types.Object_Id;
      Last              : in DB.Types.Object_Id) return Query_Result
   is
      Count             : constant DB.Types.Object_Id :=
        DB.Types.Object_Id (This.Count);
      Result            : Query_Result;
   begin
      if First = 0 then
         raise CONSTRAINT_ERROR;
      elsif First > Last then
         null;          --  first > last, so return empty result set.
      else
         if First > Count or else (Last - First) + 1 > Count then
            raise CONSTRAINT_ERROR;
         end if;

         Result := This;
         Result.First := Result.First + (First - 1);
         Result.Count := Natural (Last - First) + 1;
      end if;
      return Result;
   end Slice;

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
      Result.First := 0;
      Result.Items := Connection.Execute (Query_SQL);
      Result.Count := Result.Items.Count;
      Result.Lazy_Fetched := Lazy_Fetch;
      Result.Read_Only := Read_Only;
      Set_Unbounded_String (Result.Query, String (Query_SQL));
      return Result;
   end SQL_Query;

   ---------------------
   -- To_SQL_Criteria --
   ---------------------

   function To_SQL_Criteria
     (Connection        : in DB.Connector.Connection;
      Criteria          : in DB.Active_Record.Fields.Field_Criteria;
      For_Update        : in Boolean := False;
      Read_Only         : in Boolean := False;
      Ordering          : in DB.Active_Record.Fields.Order_Criteria :=
                            DB.Active_Record.Fields.Null_Order_Criteria;
      First             : in DB.Types.Object_Id := 0;
      Last              : in DB.Types.Object_Id := 0;
      No_Order          : in Boolean := False)
     return DB.Types.SQL_String
   is
      use DB.Active_Record.Fields;
      Order             : constant String := To_String (Ordering);
      Query_SQL         : Unbounded_String;
      Tables            : Unbounded_String;
      Where_Clause      : Unbounded_String;
   begin
      if First > Last then
         return "";
      else
         if not Is_Empty (Criteria) then
            Append (Query_SQL, " FROM ");
            To_Query (Criteria, Connection, Tables, Where_Clause);
            Append (Query_SQL, Tables);
            Append (Query_SQL, " WHERE ");
            Append (Query_SQL, Where_Clause);
         else
            Append (Query_SQL, " FROM ");
            Append (Query_SQL, Object.Get_Name);
         end if;

         if For_Update then
            Append (Query_SQL, "FOR UPDATE");
         end if;

         if No_Order = False then
            Append (Query_SQL, " ORDER BY ");
            if Order'Length = 0 then
               --  if no ordering is specified, order by Object Id
               Append (Query_SQL, Object.Get_Name & '.' & Object.Get_Id_Name);
            else
               Append (Query_SQL, Order);
            end if;
         end if;

         if Last > 0 then
            declare
               --  requires Last > First (see check at top of routine)
               Row_Count   : constant DB.Types.Object_Id := (Last - First) + 1;
            begin
               Append (Query_SQL, " LIMIT" & 
                                  DB.Types.Object_Id'IMage (Row_Count));
            end;
         end if;

         if First > 0 then
            Append (Query_SQL, " OFFSET" & 
                               DB.Types.Object_Id'Image (First - 1));
         end if;

         return DB.Types.SQL_String (To_String (Query_SQL));
      end if;
   end To_SQL_Criteria;

end DB.Active_Record.Models.Queries;
