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

   -----------
   -- Count --
   -----------

   function Count (This : in Query_Result) return Natural is
   begin
      return This.Count;
   end Count;

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
