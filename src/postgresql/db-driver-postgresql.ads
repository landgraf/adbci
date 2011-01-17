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
--    db-driver-postgresql.ads   jvinters   15-January-2011
--

with System;

package DB.Driver.PostgreSQL is

   pragma Linker_Options ("-lpq");
   pragma Linker_Options ("-L/usr/local/lib");

   type Driver_Type is new Abstract_Driver_Type with private;
   type Result_Type is new Abstract_Result_Type with private;
   type Result_Access is access all Result_Type;

   function Alloc return Driver_Handle;

   overriding procedure Connect
     (Driver            : in out Driver_Type;
      Hostname          : in     String;
      Database          : in     String;
      Username          : in     String := "";
      Password          : in     String := "";
      Options           : in     String := "");

   overriding procedure Disconnect
     (Driver            : in out Driver_Type);

   overriding procedure Execute_SQL
     (Driver            : in     Driver_Type;
      Result            :    out Result_Handle;
      Query             : in     DB.Types.SQL_String);

   overriding function Find_Column_By_Name
     (Result            : in Result_Type;
      Name              : in String) return Column_Index;

   overriding procedure Free_Result
     (Driver            : in     Driver_Type;
      Result            : in out Result_Handle);

   overriding function Get_Capabilities
     (This              : in Driver_Type) return Driver_Capabilities;

   overriding function Get_Column_Count
     (Result            : in Result_Type) return Natural;

   overriding function Get_Data_Bigint
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Bigint := 0) 
   return DB.Types.DB_Bigint;

   overriding function Get_Data_Boolean
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in Boolean := False) return Boolean;

   overriding function Get_Data_Integer
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Integer := 0)
   return DB.Types.DB_Integer;

   overriding function Get_Data_Is_Null
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index) return Boolean;

   overriding function Get_Data_Object_Id
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.Object_Id := 0) 
   return DB.Types.Object_Id;

   overriding function Get_Data_Smallint
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Smallint := 0) 
   return DB.Types.DB_Smallint;

   overriding function Get_Data_String
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in String := "") return String;

   overriding function Get_Foreign_Key_SQL (This : in Driver_Type)
   return DB.Types.SQL_String;

   overriding function Get_Id_SQL (This : in Driver_Type) 
   return DB.Types.SQL_String;

   overriding function Get_Inserted_Row_id
     (Result            : in Result_Type) return DB.Types.Object_Id;

   overriding function Get_Tuple_Count
     (Result            : in Result_Type) return Natural;

   overriding function Is_Connected
     (Driver            : in Driver_Type) return Boolean;

   overriding function Is_Random_Access
     (Result            : in Result_Type) return Boolean;

   overriding function Quote_Identifier
     (Driver            : in Driver_Type;
      Identifier        : in String) return DB.Types.SQL_String;

   overriding function Quote_Value
     (Driver            : in Driver_Type;
      Value             : in String) return DB.Types.SQL_String;

private

   type PG_Connection is new System.Address;

   Null_Connection      : constant PG_Connection := 
     PG_Connection (System.Null_Address);

   type PG_Result is new System.Address;

   Null_Result          : constant PG_Result := PG_Result (System.Null_Address);

   type Driver_Type is new Abstract_Driver_Type with record
      Connection        : PG_Connection := Null_Connection;
   end record;

   type Result_Type is new Abstract_Result_Type with record
      Column_Count      : Natural := 0;
      Results           : PG_Result := Null_Result;
      Tuple_Count       : Natural := 0;
   end record;

   function Last_Error (Driver : in Driver_Type) return String;
   --  Returns the last driver error as a string.

end DB.Driver.PostgreSQL;

