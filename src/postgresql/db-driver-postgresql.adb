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
--    db-driver-postgresql.adb   jvinters   15-January-2011
--

with Ada.Characters.Handling;		use Ada.Characters.Handling;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with DB.Driver_Manager;
with DB.Errors;
with DB.Types;				use DB.Types;
with Interfaces.C;			use Interfaces.C;
with Interfaces.C.Strings;		use Interfaces.C.Strings;

pragma Elaborate_All (DB.Driver_Manager);

package body DB.Driver.PostgreSQL is

   type PG_Connection_Status_Type is
     (CONNECTION_OK,
      CONNECTION_BAD);
   for PG_Connection_Status_Type'Size use Interfaces.C.Int'Size;
   pragma Convention (C, PG_Connection_Status_Type);

   for PG_Connection_Status_Type use
     (CONNECTION_OK     => 0,
      CONNECTION_BAD    => 1);

   type PG_Result_Status_Type is
     (PGRES_EMPTY_QUERY,
      PGRES_COMMAND_OK,
      PGRES_TUPLES_OK,
      PGRES_COPY_OUT,
      PGRES_COPY_IN,
      PGRES_BAD_RESPONSE,
      PGRES_NONFATAL_ERROR,
      PGRES_FATAL_ERROR);
   for PG_Result_Status_Type'Size use Interfaces.C.Int'Size;
   pragma Convention (C, PG_Result_Status_Type);

   for PG_Result_Status_Type use
     (PGRES_EMPTY_QUERY    => 0,
      PGRES_COMMAND_OK     => 1,
      PGRES_TUPLES_OK      => 2,
      PGRES_COPY_OUT       => 3,
      PGRES_COPY_IN        => 4,
      PGRES_BAD_RESPONSE   => 5,
      PGRES_NONFATAL_ERROR => 6,
      PGRES_FATAL_ERROR    => 7);

   function PQ_Is_Null
     (Results           : in PG_Result;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index) return Interfaces.C.Int;
   pragma Import (C, PQ_Is_Null, "PQgetisnull");

   function PQ_Result_Status
     (This              : in PG_Result) return PG_Result_Status_Type;
   pragma Import (C, PQ_Result_Status, "PQresultStatus");

   function PQ_Status
     (This              : in PG_Connection) return PG_Connection_Status_Type;
   pragma Import (C, PQ_Status, "PQstatus");

   procedure Notice_Processor
     (Arg               : in System.Address;
      Message           : in Interfaces.C.Strings.Chars_Ptr);
   pragma Convention (C, Notice_Processor);
   --  A dummy notice processor to hide messages.

   type Notice_Processor_Ptr is access procedure
     (Arg               : in System.Address;
      Message           : in Interfaces.C.Strings.Chars_Ptr);
   pragma Convention (C, Notice_Processor_Ptr);

   procedure PQ_Set_Notice_Processor
     (Connection        : in PG_Connection;
      Processor         : in Notice_Processor_Ptr;
      Arg               : in System.Address);
   pragma Import (C, PQ_Set_Notice_Processor, "PQsetNoticeProcessor");

   -----------
   -- Alloc --
   -----------

   function Alloc return Driver_Handle is
   begin
      return new Driver_Type;
   end Alloc;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Driver            : in out Driver_Type;
      Hostname          : in     String;
      Database          : in     String;
      Username          : in     String := "";
      Password          : in     String := "";
      Options           : in     String := "")
   is
      function PQ_Connect_Db
        (Connect_Str    : in Interfaces.C.Char_Array) return PG_Connection;
      pragma Import (C, PQ_Connect_Db, "PQconnectdb");

      Connect_Str       : Unbounded_String;
      Init_Result       : Result_Handle;
   begin
      if Driver.Connection /= Null_Connection then
         Disconnect (Driver);
      end if;

      if Hostname'Length = 0 then
         Set_Unbounded_String (Connect_Str, "host=localhost");
      else
         Set_Unbounded_String (Connect_Str, "host=" & Hostname);
      end if;

      if Database'Length /= 0 then
         Append (Connect_Str, " dbname=" & Database);
      end if;

      if Username'Length /= 0 then
         Append (Connect_Str, " user=" & Username);
      end if;

      if Password'Length /= 0 then
         Append (Connect_Str, " password=" & Password);
      end if;

      if Options'Length /= 0 then
         Append (Connect_Str, ' ');
         Append (Connect_Str, Options);
      end if;

      Driver.Connection := PQ_Connect_Db (To_C (To_String (Connect_Str)));
      if Driver.Connection = Null_Connection then
         raise DB.Errors.CONNECT_ERROR with
           "unable to connect to server -- probably out of memory";
      end if;

      if PQ_Status (Driver.Connection) /= CONNECTION_OK then
         raise DB.Errors.CONNECT_ERROR with Last_Error (Driver);
      end if;

      --  Set the connection to some sensible defaults.
      begin
         Execute_SQL (Driver, Init_Result, "SET DATESTYLE TO ISO, YMD");
         Execute_SQL (Driver, Init_Result, "SET CLIENT_ENCODING TO UTF8");
         Free_Result (Driver, Init_Result);
         PQ_Set_Notice_Processor
           (Driver.Connection, Notice_Processor'Access, System.Null_Address);
      exception
         when others =>
            Disconnect (Driver);
            raise;
      end;
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect
     (Driver            : in out Driver_Type)
   is
      procedure PQ_Finish (This : in PG_Connection);
      pragma Import (C, PQ_Finish, "PQfinish");
   begin
      if Driver.Connection /= Null_Connection then
         PQ_Finish (Driver.Connection);
         Driver.Connection := Null_Connection;
      end if;
   end Disconnect;

   -----------------
   -- Execute_SQL --
   -----------------

   procedure Execute_SQL
     (Driver            : in     Driver_Type;
      Result            :    out Result_Handle;
      Query             : in     DB.Types.SQL_String)
   is
      function PQ_Exec
        (Connection     : in PG_Connection;
         Query          : in Interfaces.C.Char_Array) return PG_Result;
      pragma Import (C, PQ_Exec, "PQexec");

      function PQ_N_Fields
        (Result         : in PG_Result) return Interfaces.C.Int;
      pragma Import (C, PQ_N_Fields, "PQnfields");

      function PQ_N_Tuples
        (Result         : in PG_Result) return Interfaces.C.Int;
      pragma Import (C, PQ_N_Tuples, "PQntuples");

      Query_Result      : PG_Result;
      Result_Code       : PG_Result_Status_Type;
   begin
      if Result /= Null then
         Free_Result (Driver, Result);
      end if;
      Result := null;

      Query_Result := PQ_Exec (Driver.Connection, To_C (String (Query)));
      if Query_Result = Null_Result then
         raise DB.Errors.SQL_Error with Last_Error (Driver);
      end if;
      Result_Code := PQ_Result_Status (Query_Result);

      case Result_Code is
         when PGRES_EMPTY_QUERY =>
            raise DB.Errors.SQL_ERROR with "empty query";
         when PGRES_COMMAND_OK =>
            --  no result set, but SQL executed OK.
            Result := null;
         when PGRES_TUPLES_OK =>
            --  SQL executed OK, and returned a result set.
            declare
               R        : constant Result_Access := new Result_Type;
            begin
               R.all.Results := Query_Result;
               R.all.Column_Count := Natural (PQ_N_Fields (Query_Result));
               R.all.Tuple_Count := Natural (PQ_N_Tuples (Query_Result));
               Result := Result_Handle (R);
            end;
         when PGRES_COPY_OUT | PGRES_COPY_IN =>
            raise DB.Errors.CONNECT_ERROR with "unexpected response from server";
         when PGRES_BAD_RESPONSE =>
            raise DB.Errors.CONNECT_ERROR with "bad response from server";
         when PGRES_NONFATAL_ERROR | PGRES_FATAL_ERROR =>
            --  XXX FIXME: need to do more here! XXX
            raise DB.Errors.SQL_ERROR with Last_Error (Driver);
         end case;
      end Execute_SQL;

   -------------------------
   -- Find_Column_By_Name --
   -------------------------

   function Find_Column_By_Name
     (Result            : in Result_Type;
      Name              : in String) return Column_Index
   is
      function PQ_F_Number
        (Results        : in PG_Result;
         Field_Name     : in Interfaces.C.Char_Array) return Interfaces.C.Int;
      pragma Import (C, PQ_F_Number, "PQfnumber");

      Index             : Interfaces.C.Int;
   begin
      if Result.Results /= Null_Result then
         Index := PQ_F_Number (Result.Results, To_C (Name));
         if Index >= 0 then
            return Column_Index (Index + 1);
         end if;
      end if;
      raise DB.Errors.COLUMN_NOT_FOUND with "column '" & Name & "' not found";
   end Find_Column_By_Name;

   -----------------
   -- Free_Result --
   -----------------

   procedure Free_Result
     (Driver            : in     Driver_Type;
      Result            : in out Result_Handle)
   is
      procedure Free_Storage is new Ada.Unchecked_Deallocation
        (Result_Type, Result_Access);

      procedure PQ_Clear (This : in PG_Result);
      pragma Import (C, PQ_Clear, "PQclear");
   begin
      pragma Unreferenced (Driver);

      if Result /= null then
         if Result_Type (Result.all).Results /= Null_Result then
            PQ_Clear (Result_Type (Result.all).Results);
            Result_Type (Result.all).Results := Null_Result;
         end if;
         Free_Storage (Result_Access (Result));
      end if;
   end Free_Result;

   ----------------------
   -- Get_Capabilities --
   ----------------------

   function Get_Capabilities
     (This              : in Driver_Type) return Driver_Capabilities
   is
      pragma Unreferenced (This);
   begin
      return (
         Insert_Id_Func    => False,
         Random_Access     => True,
         Returning_Clause  => True
      );
   end Get_Capabilities;

   ----------------------
   -- Get_Column_Count --
   ----------------------

   function Get_Column_Count
     (Result            : in Result_Type) return Natural
   is
   begin
      return Result.Column_Count;
   end Get_Column_Count;

   ---------------------
   -- Get_Data_Bigint --
   ---------------------

   function Get_Data_Bigint
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Bigint := 0)
     return DB.Types.DB_Bigint
   is
      S : constant String :=
        (Get_Data_String (Result, Tuple, Column, True, ""));
   begin
      if S /= "" then
         return DB.Types.DB_Bigint'Value (S);
      else
         if Replace_Null then
            return Replacement;
         else
            raise DB.Errors.COLUMN_IS_NULL;
         end if;
      end if;
   end Get_Data_Bigint;

   ----------------------
   -- Get_Data_Boolean --
   ----------------------

   function Get_Data_Boolean
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in Boolean := False) return Boolean
   is
      S : constant String := To_Upper
        (Get_Data_String (Result, Tuple, Column, True, ""));
   begin
      if S /= "" then
         return S = "TRUE" or else S = "T";
      else
         if Replace_Null then
            return Replacement;
         else
            raise DB.Errors.COLUMN_IS_NULL;
         end if;
      end if;
   end Get_Data_Boolean;

   ----------------------
   -- Get_Data_Integer --
   ----------------------

   function Get_Data_Integer
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Integer := 0)
     return DB.Types.DB_Integer
   is
      S : constant String :=
        Get_Data_String (Result, Tuple, Column, True, "");
   begin
      if S /= "" then
         return DB.Types.DB_Integer'Value (S);
      else
         if Replace_Null then
            return Replacement;
         else
            raise DB.Errors.COLUMN_IS_NULL;
         end if;
      end if;
   end Get_Data_Integer;

   ----------------------
   -- Get_Data_Is_Null --
   ----------------------

   overriding function Get_Data_Is_Null
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index) return Boolean
   is
   begin
      if Tuple = 0 or else Tuple > Tuple_Index (Result.Tuple_Count) then
         raise DB.Errors.TUPLE_NOT_FOUND;
      elsif Column = 0 or else Column > Column_Index (Result.Column_Count) then
         raise DB.Errors.COLUMN_NOT_FOUND;
      elsif Result.Results /= Null_Result then
         return PQ_Is_Null (Result.Results, Tuple - 1, Column - 1) /= 0;
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_Data_Is_Null;

   ------------------------
   -- Get_Data_Object_Id --
   ------------------------

   function Get_Data_Object_Id
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.Object_Id := 0)
     return DB.Types.Object_Id
   is
      S	: constant String := Get_Data_String (Result, Tuple, Column);
   begin
      if S /= "" then
         return DB.Types.Object_Id'Value (S);
      else
         if Replace_Null then
            return Replacement;
         else
            raise DB.Errors.COLUMN_IS_NULL;
         end if;
      end if;
   end Get_Data_Object_Id;

   -----------------------
   -- Get_Data_Smallint --
   -----------------------

   function Get_Data_Smallint
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Smallint := 0)
     return DB.Types.DB_Smallint
   is
      S	: constant String := Get_Data_String (Result, Tuple, Column);
   begin
      if S /= "" then
         return DB.Types.DB_Smallint'Value (S);
      else
         if Replace_Null then
            return Replacement;
         else
            raise DB.Errors.COLUMN_IS_NULL;
         end if;
      end if;
   end Get_Data_Smallint;

   ---------------------
   -- Get_Data_String --
   ---------------------

   function Get_Data_String
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in String := "") return String
   is
      function PQ_Get_Value
        (Results	: in PG_Result;
         Tuple		: in Tuple_Index;
         Column		: in Column_Index) return Interfaces.C.Strings.Chars_Ptr;
      pragma Import (C, PQ_Get_Value, "PQgetvalue");
   begin
      if Tuple = 0 or else Tuple > Tuple_Index (Result.Tuple_Count) then
         raise DB.Errors.TUPLE_NOT_FOUND;
      elsif Column = 0 or else Column > Column_Index (Result.Column_Count) then
         raise DB.Errors.COLUMN_NOT_FOUND;
      elsif Result.Results /= Null_Result then
         if PQ_Is_Null (Result.Results, Tuple - 1, Column - 1) /= 0 then
            if Replace_Null then
               return Replacement;
            else
               raise DB.Errors.COLUMN_IS_NULL;
            end if;
         else
            declare
               S	: constant Interfaces.C.Strings.Chars_Ptr :=
                 PQ_Get_Value (Result.Results, Tuple - 1, Column - 1);
            begin
               if S = Null_Ptr then
                  raise DB.Errors.END_OF_RESULT_SET;
               else
                  declare
                     Result : constant String := Value (S);
                  begin
                     return Result;
                  end;
               end if;
            end;
         end if;
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_Data_String;

   -------------------------
   -- Get_Foreign_Key_SQL --
   -------------------------

   function Get_Foreign_Key_SQL (This : in Driver_Type)
     return DB.Types.SQL_String
   is
      pragma Unreferenced (This);
   begin
      return "BIGINT";
   end Get_Foreign_Key_SQL;

   ----------------
   -- Get_Id_SQL --
   ----------------

   function Get_Id_SQL (This : in Driver_Type) return DB.Types.SQL_String is
      pragma Unreferenced (This);
   begin
      return "BIGSERIAL";
   end Get_Id_SQL;

   -------------------------
   -- Get_Inserted_Row_Id --
   -------------------------

   function Get_Inserted_Row_id
     (Driver            : in Driver_Type) return DB.Types.Object_Id
   is
   begin
      raise DB.Errors.NOT_SUPPORTED with
        "postgresql doesn't do this -- use a RETURNING clause in your query";
      return 0;
   end Get_Inserted_Row_Id;

   ------------------------
   -- Get_Server_Version --
   ------------------------

   function Get_Server_Version (This : in Driver_Type) return String is
      Version_Result	: Result_Handle;
   begin
      Execute_SQL (This, Version_Result, "SELECT version()");
      declare
         Version_String	: constant String :=
           Get_Data_String
             (Result       => Result_Type (Version_Result.all),
              Tuple        => 1,
              Column       => 1,
              Replace_Null => True,
              Replacement  => "");
      begin
         Free_Result (This, Version_Result);
         return Version_String;
      end;
   end Get_Server_Version;

   -------------------
   -- Get_Text_Type --
   -------------------

   function Get_Text_Type
     (This              : in Driver_Type;
      Maximum_Size      : in Natural) return DB.Types.SQL_String
   is
      pragma Unreferenced (Maximum_Size);
      pragma Unreferenced (This);
   begin
      return "TEXT";
   end Get_Text_Type;

   ---------------------
   -- Get_Tuple_Count --
   ---------------------

   function Get_Tuple_Count
     (Result            : in Result_Type) return Natural
   is
   begin
      return Result.Tuple_Count;
   end Get_Tuple_Count;

   ------------------
   -- Is_Connected --
   ------------------

   function Is_Connected
     (Driver            : in Driver_Type) return Boolean
   is
   begin
      return Driver.Connection /= Null_Connection
        and then PQ_Status (Driver.Connection) = CONNECTION_OK;
   end Is_Connected;

   ----------------------
   -- Is_Random_Access --
   ----------------------

   function Is_Random_Access
     (Result            : in Result_Type) return Boolean
   is
   begin
      pragma Unreferenced (Result);
      return True;
   end Is_Random_Access;

   ----------------
   -- Last_Error --
   ----------------

   function Last_Error (Driver : in Driver_Type) return String is
      function PQ_Error_Message
        (Connection	: in PG_Connection)
         return Interfaces.C.Strings.Chars_Ptr;
      pragma Import (C, PQ_Error_Message, "PQerrorMessage");
   begin
      return Value (PQ_Error_Message (Driver.Connection));
   end Last_Error;

   ----------------------
   -- Notice_Processor --
   ----------------------

   procedure Notice_Processor
     (Arg               : in System.Address;
      Message           : in Interfaces.C.Strings.Chars_Ptr)
   is
   begin
      pragma Unreferenced (Arg);
      pragma Unreferenced (Message);
      null;
   end Notice_Processor;

   ----------------------
   -- Quote_Identifier --
   ----------------------

   function Quote_Identifier
     (Driver            : in Driver_Type;
      Identifier        : in String) return DB.Types.SQL_String
   is
      Temp		: DB.Types.SQL_String := Quote_Value (Driver, Identifier);
   begin
      Temp (Temp'First) := '"';
      Temp (Temp'Last) := '"';
      return Temp;
   end Quote_Identifier;

   -----------------
   -- Quote_Value --
   -----------------

   function Quote_Value
     (Driver            : in Driver_Type;
      Value             : in String) return DB.Types.SQL_String
   is
      function PQ_Escape_String_Conn
        (Connection	: in PG_Connection;
         To		: in System.Address;
         From		: in Interfaces.C.Char_Array;
         Length		: in Interfaces.C.Size_t;
         Error		: in System.Address)
        return Interfaces.C.Size_t;
      pragma Import (C, PQ_Escape_String_Conn, "PQescapeStringConn");

      Error		: aliased Interfaces.C.Int := 0;
      Result		: Interfaces.C.Size_t;
      To		: aliased String (1 .. (Value'Length * 2) + 1);
   begin
      Result := PQ_Escape_String_Conn
        (Driver.Connection, To (1)'Address, To_C (Value), To'Length, Error'Address);
      if Error /= 0 then
         raise STORAGE_ERROR;
      else
         return ''' & DB.Types.SQL_String (To (1 .. Natural (Result))) & ''';
      end if;
   end Quote_Value;

begin
   DB.Driver_Manager.Register_Driver ("postgresql", Alloc'Access);
end DB.Driver.PostgreSQL;
