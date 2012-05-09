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
--  db-driver-mysql.adb
--

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with DB.Driver_Manager;
with DB.Errors;

pragma Elaborate_All (DB.Driver_Manager);

package body DB.Driver.MySQL is

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

   overriding procedure Connect
     (Driver            : in out Driver_Type;
      Hostname          : in     String;
      Database          : in     String;
      Username          : in     String := "";
      Password          : in     String := "";
      Options           : in     String := "")
   is
      pragma Unreferenced (Options);

       -- MySQL : MySQL_Access := Driver.Connection;
      CLIENT_COMPRESS   : constant Interfaces.C.Unsigned_Long := 32;
      Opt_Status        : Integer;
      MySQL_Socket      : constant Socket_Access := null;
      Init_Result       : Result_Handle;

      function MySQL_Init (MySQL : in MySQL_Access) return MySQL_Access;
      pragma Import (C, MySQL_Init, "mysql_init");
      -- int mysql_options(MYSQL *mysql, enum mysql_option option, const char *arg)

      function Get_Options
        (MySQL          : in MySQL_Access;
         Options        : in MySQL_Options;
         Args           : in Interfaces.C.Char_Array) return Integer;
      pragma Import (C, Get_Options, "mysql_options");

      function Real_Connect
        (MySQL          : MySQL_Access;
         Hostname       : in Interfaces.C.Char_Array;
         Username       : in Interfaces.C.Char_Array;
         Password       : in Interfaces.C.Char_Array;
         Database       : in Interfaces.C.Char_Array;
         DB_Port        : in Interfaces.C.Unsigned;
         MySQL_Socket   : in Socket_Access;
         Flag           : in Interfaces.C.unsigned_long) return Mysql_Access;
      pragma Import (C, Real_Connect, "mysql_real_connect");

   begin
      if Driver.Connection /= Null_Connection then
         Disconnect (Driver);
      end if;

      -- FIXME Add some checks here

      Driver.Connection := MySQL_Init(Driver.Connection);

      Opt_Status  := Get_Options
        (MySQL   => Driver.Connection,
         Options => MYSQL_READ_DEFAULT_FILE,
         Args    => "odbc");

      Opt_Status  := Get_Options
        (MySQL   => Driver.Connection,
         Options => MYSQL_READ_DEFAULT_GROUP,
         Args    => "odbc");

      pragma Unreferenced (Opt_Status);

      Driver.Connection := Real_Connect
        (MySQL        => Driver.Connection,
         Hostname     => C.To_C (Hostname),
         Username     => C.To_C (Username),
         Password     => C.To_C (Password),
         Database     => C.To_C (Database),
         DB_Port      => DEFAULT_MYSQL_PORT,
         MySQL_Socket => MySQL_Socket,
         Flag         => CLIENT_COMPRESS);

      if Driver.Connection = Null then
         raise DB.Errors.CONNECT_ERROR with Last_Error (Driver);
      end if;

      Free_Result (Driver,Init_Result);
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   overriding procedure Disconnect
     (Driver            : in out Driver_Type)
   is
      procedure MySQL_Close(MySQL : MySQL_Access);
      pragma Import (C, MySQL_Close, "mysql_close");
   begin
      if Driver.Connection /= Null then
         MySQL_Close (Driver.Connection);
         Driver.Connection := Null_Connection;
      end if;
   end Disconnect;

   -----------------
   -- Execute_SQL --
   -----------------

   overriding procedure Execute_SQL
     (Driver            : in     Driver_Type;
      Result            :    out Result_Handle;
      Query             : in     DB.Types.SQL_String)
   is
      function MySQL_Real_Query
        (MySQL          : in MySQL_Access;
         Q_C            : in C.Char_Array;
         Length_C       : in C.Unsigned_Long) return C.Unsigned_Long;
      pragma Import (C, MySQL_Real_Query, "mysql_real_query");

      function MySQL_Store_Result
        (MySQL          : in Mysql_Access) return Mysql_Result_Access;
      pragma Import (C, MySQL_Store_Result, "mysql_store_result");

      function MySQL_Num_Fields
        (Result         : in MySQL_Result_Access) return Natural;
      pragma Import (C,MySQL_Num_Fields, "mysql_num_fields");

      function MySQL_Num_Rows
        (Result         : in MySQL_Result_Access) return Natural;
      pragma Import (C, MySQL_Num_Rows, "mysql_num_rows");

      function Mysql_Affected_Rows
        (MySQL          : in MySQL_Access) return Integer;
      pragma Import (C, MySQL_Affected_Rows, "mysql_affected_rows");

      Query_Result      : MySQL_Result_Access;
      Result_Code       : C.Unsigned_Long;
      Length            : constant C.Unsigned_Long := Query'Length;
   begin
      if Result /= Null then
         Free_Result (Driver,Result);
      end if;
      Result := Null;

      Result_Code := MySQL_Real_Query
        (Driver.Connection, C.To_C (String (Query)), Length);

      if Integer (Result_Code) /= 0 then
         raise DB.Errors.CONNECT_ERROR with Last_Error (Driver);
      end if;

      if MySQL_Affected_Rows (Driver.Connection) /= -1 then
          declare
              R : constant Result_Access := new Result_Type;
          begin
              Query_Result := MySQL_Store_Result (Driver.Connection);
              if Query_Result /= Null then
                  R.all.Results := Query_Result;
                  R.all.Field_Count := MySQL_Num_Fields (Query_Result);
                  R.all.Row_Count := MySQL_Num_Rows (Query_Result);
                  Result := Result_Handle (R);
              end if;
          end;
      else
          Result := null;
      end if;
   end Execute_SQL;

   -------------------------
   -- Find_Column_By_Name --
   -------------------------

   overriding function Find_Column_By_Name
       (Result          : in Result_Type;
        Name            : in String) return Column_Index
   is
      Field             : MySQL_Field;
      IName             : constant String :=
        Translate (Name, Ada.Strings.Maps.Constants.Upper_Case_Map);
   begin
      for Tmp in 0 .. Result.Field_count - 1 loop
          Field := Get_Field_Direct (Result.Results,Tmp);

         if Index (Translate (Value (Field.Name),
           Ada.Strings.Maps.Constants.Upper_Case_Map), IName) = 1 then
            return Column_Index(Tmp);
         end if;
      end loop;
      raise DB.Errors.COLUMN_NOT_FOUND;
   end Find_Column_By_Name;

   -----------------
   -- Free_Result --
   -----------------

   overriding procedure Free_Result
     (Driver            : in     Driver_Type;
      Result            : in out Result_Handle)
   is
      pragma Unreferenced (Driver);

      procedure Free_Storage is new Ada.Unchecked_Deallocation
        (Result_Type, Result_Access);

      procedure MySQL_Free_Result
        (Result         : in MySQL_Result_Access);
      pragma Import (C, MySQL_Free_Result, "mysql_free_result");

   begin
      if Result /= null then
         if Result_Type (Result.all).Results /= Null_Result then
            MySQL_Free_Result (Result_Type (Result.all).Results);
            Result_Type (Result.all).Results := Null_Result;
         end if;

         Free_Storage (Result_Access (Result));
      end if;
   end Free_Result;

   ----------------------
   -- Get_Capabilities --
   ----------------------

   overriding function Get_Capabilities
     (This              : in Driver_Type) return Driver_Capabilities
   is
      pragma Unreferenced (This);
   begin
      return
        (Insert_Id_Func   => True,
         Random_Access    => True,
         Returning_Clause => False,
         Has_ilike        => False,
         Count_Name       => False);
   end Get_Capabilities;

   ----------------------
   -- Get_Column_Count --
   ----------------------

   overriding function Get_Column_Count
     (Result            : in Result_Type) return Natural
   is
   begin
      return Result.Field_Count;
   end Get_Column_Count;

   ---------------------
   -- Get_Data_Bigint --
   ---------------------

   overriding function Get_Data_Bigint
     (Result            : in Result_Type;
      Row               : in Tuple_Index;
      Field             : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Bigint := 0) return DB.Types.DB_Bigint
   is
      S : constant String := (Get_Data_String (Result, Row, Field, True, ""));
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

   overriding function Get_Data_Boolean
     (Result            : in Result_Type;
      Row               : in Tuple_Index;
      Field             : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in Boolean := False) return Boolean
   is
      S : constant String := To_Upper
        (Get_Data_String (Result, Row, Field, True, ""));
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

   overriding function Get_Data_Integer
     (Result            : in Result_Type;
      Row               : in Tuple_Index;
      Field             : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Integer := 0) return DB.Types.DB_Integer
   is
      S : constant String := Get_Data_String (Result, Row, Field, True, "");
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
      Row               : in Tuple_Index;
      Field             : in Column_Index) return Boolean
   is
   begin
      Mysql_Data_Seek (Result.Results, Integer (Row-1));
      if Get_Data_Length (Result.Results, Field) = 0 then
         return True;
      else
         return False;
      end if;
   end Get_Data_Is_Null;

   ---------------------
   -- Get_Data_Length --
   ---------------------

   function Get_Data_Length
     (Result            : in MySQL_Result_Access;
      Field             : in Column_Index) return Integer
   is
      Lengths           : Lengths_Type;
      Lnt               : C.Unsigned_long;
      Current_Row       : constant MySQL_Row := MySQL_Fetch_Row (Result);
      pragma Unreferenced (Current_Row);
   begin
      Lengths := MySQL_Fetch_Lengths(Result);

      for j in 0 .. Field loop
         Lnt := Lengths.all;
         Unsigned_Long_Array_Ptr.Increment (Lengths);
      end loop;
      return Integer (Lnt);
   end Get_Data_Length;

   ------------------------
   -- Get_Data_Object_Id --
   ------------------------

   overriding function Get_Data_Object_Id
     (Result            : in Result_Type;
      Row               : in Tuple_Index;
      Field             : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.Object_Id := 0) return DB.Types.Object_Id
   is
      S : constant String := Get_Data_String (Result, Row, Field);
   begin
      if S /= "" then
         return DB.Types.Object_Id'Value (S);
      else
         if Replace_Null then
            return Replacement;
         else
            raise DB.Errors.Column_IS_NULL;
         end if;
      end if;
   end Get_Data_Object_Id;

   -----------------------
   -- Get_Data_Smallint --
   -----------------------

   overriding function Get_Data_Smallint
     (Result            : in Result_Type;
      Row               : in Tuple_Index;
      Field             : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Smallint := 0)
     return DB.Types.DB_Smallint
   is
      S : constant String := Get_Data_String (Result, Row, Field);
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

   overriding function Get_Data_String
     (Result            : in Result_Type;
      Row               : in Tuple_Index;
      Field             : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in String := "")
     return String
   is
      Lengths           : Lengths_Type;
      Current_Row       : MySQL_Row;
      Res_Text          : Unbounded_String;
   begin
      MySQL_Data_Seek (Result.Results, Integer (Row - 1));
      Current_Row := MySQL_Fetch_Row (Result.Results);
      Lengths := MySQL_Fetch_Lengths (Result.Results);

      for i in 0 .. Field loop
         if Lengths.all /= 0 then
            Res_Text :=  To_Unbounded_String
              (Interfaces.C.Strings.Value (Current_Row.all));
         end if;

         if Field /= 0 then
            MySQL_Row_Type.Increment (Current_Row);
            Unsigned_Long_Array_Ptr.Increment (Lengths);
         end if;
      end loop;

      return To_String (Res_Text);
   end Get_Data_String;

   ----------------------
   -- Get_Field_Direct --
   ----------------------

   function Get_Field_Direct
     (Result            : MySQL_Result_Access;
      Field_Index       : Integer) return MySQL_Field
   is
      function MySQL_Fetch_Field_Direct
        (Result         : in MySQL_Result_Access;
         Index          : in Integer) return MySQL_Field;
      pragma Import (C, MySQL_Fetch_Field_Direct, "mysql_fetch_field_direct");
   begin
      return MySQL_Fetch_Field_Direct (Result, Field_Index);
   end Get_Field_Direct;

   -------------------------
   -- Get_Foreign_Key_SQL --
   -------------------------

   overriding function Get_Foreign_Key_SQL
     (This              : in Driver_Type) return DB.Types.SQL_String
   is
      pragma Unreferenced (This);
   begin
      return "BIGINT";
   end Get_Foreign_Key_SQL;

   ----------------
   -- Get_Id_SQL --
   ----------------

   overriding function Get_Id_SQL
     (This              : in Driver_Type) return DB.Types.SQL_String
   is
      pragma Unreferenced (This);
   begin
      return "BIGINT AUTO_INCREMENT";
   end Get_Id_SQL;

   -------------------------
   -- Get_Inserted_Row_id --
   -------------------------

   overriding function Get_Inserted_Row_id
     (Driver            : in Driver_Type) return DB.Types.Object_Id
   is
      function MySQL_Insert_Id
        (MySQL          : in MySQL_Access) return Integer;
      pragma Import (C, MySQL_Insert_Id, "mysql_insert_id");
   begin
      -- FIXME Check it
      return DB_Bigserial (MySQL_Insert_Id (Driver.Connection));
   end Get_Inserted_Row_id;

   ------------------------
   -- Get_Server_Version --
   ------------------------

   overriding function Get_Server_Version
     (This              : in Driver_Type) return String
   is
      Version_Result    : Result_Handle;
   begin
      Execute_SQL (This, Version_Result, "SELECT version()");
      declare
         Version_String : constant String :=
           Get_Data_String
             (Result       => Result_Type (Version_Result.all),
              Row          => 1,
              Field        => 1,
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
       return "Text";
   end Get_Text_Type;

   ---------------------
   -- Get_Tuple_Count --
   ---------------------

   overriding function Get_Tuple_Count
     (Result            : in Result_Type) return Natural
   is
   begin
      return Result.Row_Count;
   end Get_Tuple_Count;

   ------------------
   -- Is_Connected --
   ------------------

   overriding function Is_Connected
     (Driver            : in Driver_Type) return Boolean
   is
      function MySQL_Stat
        (MySQL          : in MySQL_Access) return C.Strings.Chars_Ptr;
      pragma Import(C, MySQL_Stat, "mysql_stat");
   begin
       if Driver.Connection /= Null then
         if MySQL_Stat(Driver.Connection) = C.Strings.Null_Ptr then
            return True;
         end if;
      end if;
      return False;
   end Is_Connected;

   ----------------------
   -- Is_Random_Access --
   ----------------------

   overriding function Is_Random_Access
     (Result            : in Result_Type) return Boolean
   is
   begin
      pragma Unreferenced (Result);
      return True;
   end Is_Random_Access;

   ----------------------
   -- Quote_Identifier --
   ----------------------

   overriding function Quote_Identifier
     (Driver            : in Driver_Type;
      Identifier        : in String) return DB.Types.SQL_String
   is
      Temp              : DB.Types.SQL_String := Quote_Value (Driver, Identifier);
   begin
      Temp (Temp'First) := '"';
      Temp (Temp'Last) := '"';
      return Temp;
   end Quote_Identifier;

   -----------------
   -- Quote_Value --
   -----------------

   overriding function Quote_Value
     (Driver            : in Driver_Type;
      Value             : in String) return DB.Types.SQL_String
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (C.char_array, C.Strings.char_array_access);
      -- FIXME Deallocation here
      -- Data       : constant Object_Data_Access := Instance (Obj, "escape");
      -- allocating buffer for encoding the buffer

      Buffer : C.Strings.Char_Array_Access :=
        new C.Char_Array (1 .. Value'Length * 2 + 1);

      function MySQL_Real_Escape_String
        (MySQL          : MySQL_Access;
         To             : C.Strings.Chars_Ptr;
         From           : C.Strings.Chars_Ptr;
         Length         : C.Unsigned_Long) return C.Unsigned;
      pragma Import (C, MySQL_Real_Escape_String, "mysql_real_escape_string");
   begin
      declare
         Buffer_Ptr     : constant C.Strings.Chars_Ptr :=
           C.Strings.To_Chars_Ptr (Buffer);
         Escape         : constant C.Unsigned  :=
           MYSQL_Real_Escape_String
             (MySQL     => Driver.Connection,
              To        => Buffer_Ptr,
              From      => New_Char_Array (C.To_C (Value)),
              Length    => C.Unsigned_Long (Value'Length));
         pragma Unreferenced (Escape);
         S              : constant Unbounded_String :=
           To_Unbounded_String (Strings.Value (Buffer_Ptr));
      begin
         Free (Buffer);
         return "'" & SQL_String (To_String (S)) & "'";
      end;
   end Quote_Value;

   ----------------
   -- Last_Error --
   ----------------

   function Last_Error
     (Driver            : in Driver_Type) return String
   is
      function MySQL_Error
        (Connection     : in MySQL_Access) return C.Strings.Chars_Ptr;
      pragma Import (C, MySQL_Error, "mysql_error");
   begin
      return C.Strings.Value (MySQL_Error (Driver.Connection));
   end Last_Error;

begin
   DB.Driver_Manager.Register_Driver ("mysql", Alloc'Access);
end DB.Driver.MySQL;
