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

   type Query_Result is private;

   function Count (This : in Query_Result) return Natural;
   --  Returns the number of items in the result set.

   function Find
     (Connection        : in DB.Connector.Connection;
      Criteria          : in DB.Active_Record.Fields.Field_Criteria;
      For_Update        : in Boolean := False;
      Read_Only         : in Boolean := False) return Query_Result;
   --  Searches for items using specified criteria.

   function Item
     (This              : in Query_Result;
      Connection        : in DB.Connector.Connection;
      Index             : in Positive) return Model_Type;
   --  Fetches the Index'th item from the result set.

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
      Items             : DB.Connector.Result_Set;
      Lazy_Fetched      : Boolean := True;
      Read_Only         : Boolean := False;
   end record;

end DB.Active_Record.Models.Queries;
