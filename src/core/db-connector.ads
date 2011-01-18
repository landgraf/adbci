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
--    db-connector.ads   jvinters   16-January-2011
--

with Ada.Finalization;
with DB.Driver;
with DB.Types;

package DB.Connector is

   type Connection is new Ada.Finalization.Limited_Controlled with private;
   type Result_Set is new Ada.Finalization.Controlled with private;

   function Connect
     (Driver            : in String;
      Hostname          : in String;
      Database          : in String;
      Username          : in String := "";
      Password          : in String := "";
      Options           : in String := "") return Connection;
   --  Connects to the database.  Driver specifies the registered driver name
   --  to use, for example "postgresql".  Not all drivers will use all
   --  parameters.

   function Count (This : in Result_Set) return Natural;
   --  Returns the number of tuples in the result set.

   procedure Disconnect (This : in out Connection);
   --  Disconnects from the database.

   function Execute
     (This              : in Connection'Class;
      SQL               : in DB.Types.SQL_String) return Result_Set;
   --  Executes an SQL query, returning a result set or raising and exception.

   function Get_Bigint
     (This              : in Result_Set;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Bigint := 0)
     return DB.Types.DB_Bigint;
   --  Gets BIGINT data at specified column, optionally replacing null values.

   function Get_Bigint
     (This              : in Result_Set;
      Column            : in String;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Bigint := 0)
     return DB.Types.DB_Bigint;
   --  Gets BIGINT data at specified column, optionally replacing null values.

   function Get_Boolean
     (This              : in Result_Set;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in Boolean := False) return Boolean;
   --  Gets BOOLEAN data at specified column, optionally replacing null values.

   function Get_Boolean
     (This              : in Result_Set;
      Column            : in String;
      Replace_Null      : in Boolean := False;
      Replacement       : in Boolean := False) return Boolean;
   --  Gets BOOLEAN data at specified column, optionally replacing null values.

   function Get_Driver
     (This              : in Connection) return DB.Driver.Driver_Handle;
   --  Gets the connection's underlying driver.

   function Get_Integer
     (This              : in Result_Set;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Integer := 0) 
     return DB.Types.DB_Integer;
   --  Gets INTEGER data at specified column, optionally replacing null values.

   function Get_Integer
     (This              : in Result_Set;
      Column            : in String;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Integer := 0)
     return DB.Types.DB_Integer;
   --  Gets INTEGER data at specified column, optionally replacing null values.

   function Get_Is_Null
     (This              : in Result_Set;
      Column            : in Column_Index) return Boolean;
   --  Returns true if the given column is NULL.

   function Get_Is_Null
     (This              : in Result_Set;
      Column            : in String) return Boolean;
   --  Returns true if the given column is NULL.

   function Get_Object_Id
     (This              : in Result_Set;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.Object_Id := 0) 
     return DB.Types.Object_Id;
   --  Gets Id data at specified column, optionally replacing null values.

   function Get_Object_Id_At
     (This              : in Result_Set;
      Column            : in Column_Index;
      Tuple             : in Tuple_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.Object_Id := 0)
     return DB.Types.Object_Id;
   --  Gets Id data at specified column and tuple, optionally replacing null
   --  values.

   function Get_Object_Id_At
     (This              : in Result_Set;
      Column            : in String;
      Tuple             : in Tuple_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.Object_Id := 0)
     return DB.Types.Object_Id;
   --  Gets Id data at specified column and tuple, optionally replacing null
   --  values.

   function Get_Object_Id
     (This              : in Result_Set;
      Column            : in String;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.Object_Id := 0)
     return DB.Types.Object_Id;
   --  Gets Id data at specified column, optionally replacing null values.

   function Get_Smallint
     (This              : in Result_Set;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Smallint := 0)
     return DB.Types.DB_Smallint;
   --  Gets SMALLINT data at specified column, optionally replacing null values.

   function Get_Smallint
     (This              : in Result_Set;
      Column            : in String;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Smallint := 0)
     return DB.Types.DB_Smallint;
   --  Gets SMALLINT data at specified column, optionally replacing null values.

   function Get_String
     (This              : in Result_Set;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in String := "") return String;
   --  Gets String data at specified column, optionally replacing null values.

   function Get_String
     (This              : in Result_Set;
      Column            : in String;
      Replace_Null      : in Boolean := False;
      Replacement       : in String := "") return String;
   --  Gets String data at specified column, optionally replacing null values.

   function Get_Tuple (This : in Result_Set) return Tuple_Index;
   --  Returns the current Tuple index for the result set.

   procedure Jump_Tuple
     (This              : in out Result_Set;
      Tuple             : in     Tuple_Index);
   --  Jumps to a particular tuple in the result set.  If the required
   --  tuple isn't the next tuple, then the result set must be Random Access.

   procedure Next_Tuple (This : in out Result_Set);
   --  Advances to the next tuple, or raises END_OF_RESULT exception if last
   --  tuple passed.

   procedure Previous_Tuple (This : in out Result_Set);
   --  Goes back one tuple, or raises END_OF_RESULT_EXCEPTION if first tuple
   --  passed.  Requires the result set to be Random Access.

   function Quote_Identifier
     (This              : in Connection'Class;
      Identifier        : in String) return DB.Types.SQL_String;
   --  Quotes an SQL identifier.  The identifier is returned enclosed by
   --  quotes.

   function Quote_Value
     (This              : in Connection'Class;
      Value             : in String) return DB.Types.SQL_String;
   --  Quotes a value for use in an SQL query.  The value returned WITHOUT
   --  enclosing quotes.

private

   type Connection is new Ada.Finalization.Limited_Controlled with record
      Driver            : DB.Driver.Driver_Handle;
      In_Transaction    : Boolean := False;
   end record;

   overriding procedure Finalize (This : in out Connection);

   type Result_Record is record
      Data              : DB.Driver.Result_Handle := null;
      Driver            : DB.Driver.Driver_Handle := null;
      Reference_Count   : Natural := 0;
   end record;

   type Result_Record_Access is access all Result_Record;

   type Result_Set is new Ada.Finalization.Controlled with record
      Results           : Result_Record_Access := null;
      Tuple             : Tuple_Index := INVALID_TUPLE;
   end record;

   overriding procedure Adjust (This : in out Result_Set);

   overriding procedure Finalize (This : in out Result_Set);

end DB.Connector;

