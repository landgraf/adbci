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
--    db-driver.ads   jvinters   15-January-2011
--

with Ada.Unchecked_Deallocation;
with DB.Types;

package DB.Driver is

   type Abstract_Driver_Type is abstract tagged null record;
   type Driver_Handle is access all Abstract_Driver_Type'Class;

   type Abstract_Result_Type is abstract tagged null record;
   type Result_Handle is access all Abstract_Result_Type'Class;

   type Allocate_Function is access function return Driver_Handle;
   type Deallocate_Function is access procedure (This : in out Driver_Handle);

   type Driver_Capabilities is record
      Insert_Id_Func    : Boolean := False;  --  can get inserted id using func
      Random_Access     : Boolean := False;  --  result sets are random access
      Returning_Clause  : Boolean := False;  --  supports SQL RETURNING clauses
   end record;

   procedure Connect
     (Driver            : in out Abstract_Driver_Type;
      Hostname          : in     String;
      Database          : in     String;
      Username          : in     String := "";
      Password          : in     String := "";
      Options           : in     String := "") is abstract;
   --  Connects to the database.  Not all database drivers will require all
   --  parameters.  Either successfully connects to the database, or raises
   --  an exception.  If already connected to the database, drivers should
   --  disconnect and then attempt reconnection with the new settings.

   procedure Disconnect
     (Driver            : in out Abstract_Driver_Type) is abstract;
   --  Disconnects from the database.  Has no effect if not connected.
   --  Any uncommited data will be lost.  It is safe to call Free_Driver
   --  on a handle that has been disconnected using this procedure.

   procedure Execute_SQL
     (Driver            : in     Abstract_Driver_Type;
      Result            :    out Result_Handle;
      Query             : in     DB.Types.SQL_String) is abstract;
   --  Executes an SQL query, returning a handle to the result dataset.
   --  NOTE: certain SQL command won't return any data, so the returned
   --        handle could well be null.
   --  Any errors will result in an exception being raised.

   function Find_Column_By_Name
     (Result            : in Abstract_Result_Type;
      Name              : in String) return Column_Index is abstract;
   --  Finds the column index of the column with the given name, or raises
   --  an exception if it can't be found (DB.Errors.COLUMN_NOT_FOUND).

   procedure Free_Result
     (Driver            : in     Abstract_Driver_Type;
      Result            : in out Result_Handle) is abstract;
   --  Frees memory used by result handle.

   function Get_Capabilities
     (This              : in Abstract_Driver_Type) return Driver_Capabilities
   is abstract;
   --  Returns the driver capabilities.

   function Get_Column_Count
     (Result            : in Abstract_Result_Type) return Natural is abstract;
   --  Returns the number of columns (fields) in the result set.

   function Get_Data_Bigint
     (Result            : in Abstract_Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Bigint := 0) return DB.Types.DB_Bigint
   is abstract;
   --  Returns BIGINT data at specified tuple and column, or raises an
   --  exception.

   function Get_Data_Boolean
     (Result            : in Abstract_Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in Boolean := False) return Boolean is abstract;
   --  Returns BOOLEAN data at specified tuple and column, or raises an
   --  exception.

   function Get_Data_Integer
     (Result            : in Abstract_Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Integer := 0)
   return DB.Types.DB_Integer is abstract;
   --  Returns BOOLEAN data at specified tuple and column, or raises an
   --  exception.

   function Get_Data_Is_Null
     (Result            : in Abstract_Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index) return Boolean is abstract;
   --  Returns true if the data at Tuple, Column is NULL, or raises an
   --  exception on error.

   function Get_Data_Object_Id
     (Result            : in Abstract_Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.Object_Id := 0) return DB.Types.Object_Id
   is abstract;
   --  Returns Object_Id data at specified tuple and column, or raises
   --  an exception.

   function Get_Data_Smallint
     (Result            : in Abstract_Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Smallint := 0) 
   return DB.Types.DB_Smallint is abstract;
   --  Returns SMALLINT data at specified tuple and column, or raises
   --  an exception.

   function Get_Data_String
     (Result            : in Abstract_Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in String := "") return String is abstract;
   --  Returns string (CHAR/TEXT/VARCHAR) data at specified tuple and column, 
   --  or raises an exception.

   function Get_Foreign_Key_SQL (This : in Abstract_Driver_Type)
   return DB.Types.SQL_String is abstract;
   --  Returns the SQL Data type used to reference foreign keys.

   function Get_Id_SQL (This : in Abstract_Driver_Type) 
   return DB.Types.SQL_String is abstract;
   --  Returns the SQL Data type used to identify objects.

   function Get_Inserted_Row_id
     (Result            : in Abstract_Result_Type) 
   return DB.Types.Object_Id is abstract;
   --  For databases which support getting the inserted row id via a function,
   --  returns the inserted row id.

   function Get_Text_Type
     (This              : in Abstract_Driver_Type;
      Maximum_Size      : in Natural) return DB.Types.SQL_String is abstract;
   --  Returns the SQL Data type that can store at least Maximum_Size chars.

   function Get_Tuple_Count
     (Result            : in Abstract_Result_Type) return Natural is abstract;
   --  Returns the number of tuples (rows) in the result set.

   function Is_Connected
     (Driver            : in Abstract_Driver_Type) return Boolean is abstract;
   --  Returns true if the driver is connected to the database.

   function Is_Random_Access
     (Result            : in Abstract_Result_Type) return Boolean is abstract;
   --  Returns true if the result set is random access.

   function Quote_Identifier
     (Driver            : in Abstract_Driver_Type;
      Identifier        : in String) return DB.Types.SQL_String is abstract;
   --  Quotes an identifier (e.g. table or field name).
   --  NOTE: the returned string WILL be enclosed by double quotes.

   function Quote_Value
     (Driver            : in Abstract_Driver_Type;
      Value             : in String) return DB.Types.SQL_String is abstract;
   --  Quotes a string so that it may be safely used as part of an SQL query.
   --  NOTE: the returned string will NOT be enclosed in quotes.

   procedure Free_Driver is new Ada.Unchecked_Deallocation
     (Abstract_Driver_Type'Class, Driver_Handle);
   --  Frees allocated driver - this doesn't do any disconnection or cleanup
   --  of the underlying driver, so the connection should be closed etc.
   --  before this is called.

end DB.Driver;

