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
--    db-active_record-models-queries.ads   jvinters   17-January-2011
--

with DB.Active_Record.Fields;

generic

   type Model_Type is new DB.Active_Record.Models.Model with private;

package DB.Active_Record.Models.Queries is

   Object               : Model_Type;

   type Iterator is access procedure
     (Item              : in out Model_Type;
      Stop              : in out Boolean);

   type Query_Result is private;

   function Count (This : in Query_Result) return Natural;
   --  Returns the number of items in the result set.

   function Count
     (Connection        : in DB.Connector.Connection;
      Criteria          : in DB.Active_Record.Fields.Field_Criteria;
      First             : in DB.Types.Object_Id := 0;
      Last              : in DB.Types.Object_Id := 0)
     return Natural;
   --  Returns the number of results that would be returned by a query.
   --  Executes using an SQL COUNT instead of fetching all rows.

   function Find
     (Connection        : in DB.Connector.Connection;
      Criteria          : in DB.Active_Record.Fields.Field_Criteria;
      For_Update        : in Boolean := False;
      Read_Only         : in Boolean := False;
      Ordering          : in DB.Active_Record.Fields.Order_Criteria :=
                            DB.Active_Record.Fields.Null_Order_Criteria;
      First             : in DB.Types.Object_Id := 0;
      Last              : in DB.Types.Object_Id := 0)
     return Query_Result;
   --  Searches for items using specified criteria.
   --  If First or Last is non-zero, then they are used to slice the results.

   function Get_SQL (This : in Query_Result) return DB.Types.SQL_String;
   --  Returns the SQL used to generate the result set.

   function Item
     (This              : in Query_Result;
      Connection        : in DB.Connector.Connection;
      Index             : in Positive;
      Load_Foreign_Keys : in Boolean := True) return Model_Type;
   --  Fetches the Index'th item from the result set.

   procedure Iterate
     (This              : in Query_Result;
      Connection        : in DB.Connector.Connection;
      Handler           : not null Iterator);
   --  Iterates through the result set, loading each object in turn.

   function Slice
     (This              : in Query_Result;
      First             : in DB.Types.Object_Id;
      Last              : in DB.Types.Object_Id) return Query_Result;
   --  Slices a result set.  If First > Last, then an empty set will be
   --  returned.  If Last > the number of elements, then CONSTRAINT_ERROR
   --  will be raised.

   function SQL_Query
     (Connection        : in DB.Connector.Connection;
      Query_SQL         : in DB.Types.SQL_String;
      Lazy_Fetch        : in Boolean := True;
      Read_Only         : in Boolean := False) return Query_Result;
   --  Runs an SQL query, returning a result.
   --  If Lazy_Fetch is true, then the SQL should return a list of Object Ids
   --  to fetch.  If Lazy_Fetch is false, then the SQL should return ALL the
   --  object fields.
   --  The result is optionally read-only.

private

   type Query_Result is record
      Count             : Natural := 0;
      First             : DB.Types.Object_Id := 0;
      Items             : DB.Connector.Result_Set;
      Lazy_Fetched      : Boolean := True;
      Query             : Unbounded_String;
      Read_Only         : Boolean := False;
   end record;

   function To_SQL_Criteria
     (Connection        : in DB.Connector.Connection;
      Criteria          : in DB.Active_Record.Fields.Field_Criteria;
      For_Update        : in Boolean := False;
      Ordering          : in DB.Active_Record.Fields.Order_Criteria :=
                            DB.Active_Record.Fields.Null_Order_Criteria;
      First             : in DB.Types.Object_Id := 0;
      Last              : in DB.Types.Object_Id := 0;
      No_Order          : in Boolean := False)
     return DB.Types.SQL_String;
   --  Converts search criteria into SQL query criteria.

end DB.Active_Record.Models.Queries;
