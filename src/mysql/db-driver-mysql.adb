with DB.Driver_Manager; use DB.Driver_Manager;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps.Constants;
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
       -- MySQL               : MySQL_Access             := Driver.Connection;
       CLIENT_COMPRESS     : constant Interfaces.C.unsigned_long := 32;
       Opt_status          : Integer;
       MySQL_Socket        : Socket_Access;
       Init_Result         : Result_Handle;
       function MySQL_Init(MySQL : MySQL_Access) return MySQL_Access;
           pragma Import (C,MySQL_Init,"mysql_init");
       -- int mysql_options(MYSQL *mysql, enum mysql_option option, const char *arg)
       function Get_Options
           (MySQL : MySQL_Access;
            Options      : MySQL_Options ;
            args         : Interfaces.C.Char_Array
           ) return Integer;
       pragma Import (C, Get_Options, "mysql_options");
       function Real_Connect
           (MySQL :  MySQL_Access;
           HostName     : in Interfaces.C.Char_Array;
           Username         : in Interfaces.C.Char_Array;
           Password     : in Interfaces.C.Char_Array;
           Database     : in Interfaces.C.Char_Array;
           DB_Port      : in Interfaces.C.unsigned := 3306;
           MySQL_Socket : Socket_Access;
           Flag         : in Interfaces.C.unsigned_long) return Mysql_Access;
           pragma Import (C, Real_Connect, "mysql_real_connect");

   begin
       if Driver.Connection /= Null_Connection
       then
           Disconnect(Driver);
       end if;
       -- FIXME Add some checks here
       Driver.Connection := MySQL_Init(Driver.Connection);
       Opt_status  := Get_Options
           (
               MySQL   => Driver.Connection,
               Options => MYSQL_READ_DEFAULT_FILE,
               args    => "odbc"
               );
       Opt_status  := Get_Options
           (
               MySQL   => Driver.Connection,
               Options => MYSQL_READ_DEFAULT_GROUP,
               args    => "odbc"
               );
       Driver.Connection    := Real_Connect
           (
               MySQL       => Driver.Connection,
               HostName    => C.To_C(HostName),
               Username    => C.To_C(Username),
               Password    => C.To_C(Password),
               Database    => C.To_C(Database),
               MySQL_Socket => MySQL_Socket ,
               Flag        => CLIENT_COMPRESS
               );
       if Driver.Connection = Null
       then
           raise DB.Errors.CONNECT_ERROR with Last_Error (Driver);
       end if;
       Free_Result(Driver,Init_Result);
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   overriding procedure Disconnect
     (Driver            : in out Driver_Type)
   is
     procedure MySQL_Close(MySQL : MySQL_Access);
         pragma Import (C,MySQL_Close,"mysql_close");
   begin
       if Driver.Connection /= Null
       then
           MySQL_Close(Driver.Connection);
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
        function mysql_real_query(
            MySQL    : MySQL_Access;
            q_c      : C.char_array;
            length_C : C.Unsigned_long) return C.Unsigned_Long;
            pragma Import (C, mysql_real_query, "mysql_real_query");
        function mysql_store_result(Mysql : Mysql_Access) return Mysql_Result_Access;
            pragma Import (C,mysql_store_result,"mysql_store_result");
        function mysql_num_fields (Result : Mysql_Result_Access) return Natural;
            pragma Import (C,mysql_num_fields,"mysql_num_fields");
        function mysql_num_rows (Result : Mysql_Result_Access) return Natural;
            pragma Import (C,mysql_num_rows,"mysql_num_rows");
        function Mysql_Affected_Rows(MySQL : MySQL_Access) return Integer;
            pragma Import (C,mysql_affected_rows,"mysql_affected_rows");
        Query_Result    : MySQL_Result_Access;
        Result_Code     : C.Unsigned_Long;
        Length          : C.Unsigned_Long := Query'Length;
        Tmp             : Natural;
   begin
      if Result /= Null
      then
         Free_Result(Driver,Result);
      end if;
      Result := Null;
      put_line("Execute: " &  String(Query));
      Result_Code   := MySQL_Real_Query(Driver.Connection, C.To_C(String(Query)) , Length);
      if Integer(Result_Code) /= 0
      then
          raise DB.Errors.CONNECT_ERROR with Last_Error (Driver);
      end if;
      if MySQL_Affected_Rows(Driver.Connection) = -1 then
          declare
              R : constant Result_Access := new Result_Type;
          begin
              Query_Result := MySQL_Store_Result(Driver.Connection);
              R.all.Results := Query_Result;
              R.all.Field_Count := mysql_num_fields(Query_Result);
              R.all.Row_Count    := mysql_num_rows(Query_Result);
              Result := Result_Handle (R);
          end;
      else
          Result := null;
      end if;
   end Execute_SQL;

   ------------------------
   -- Find_Column_By_Name --
   ------------------------
   overriding function Find_Column_By_Name
       (Result          : in Result_Type;
        Name            : in String) return Column_Index
   is
       Field : MySQL_Field;
       IName : constant String  := Translate(Name, Ada.Strings.Maps.Constants.Upper_Case_Map);
   begin
      put_line("Result.Field_Count" & Result.Field_count'Img & "Find for count " & IName);
      for Tmp in 0..Result.Field_count-1 loop
          Field := Get_Field_Direct(Result.Results,Tmp);
          put_line("Field name is:  " & Value(Field.Name));
          if Index(Translate(Value(Field.Name),Ada.Strings.Maps.Constants.Upper_Case_Map) ,IName) = 1 then
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
          procedure Free_Storage is new Ada.Unchecked_Deallocation
              (Result_Type, Result_Access);
          procedure MySQL_Free_Result(result : in MySQL_Result_Access);
              pragma Import (C,MySQL_Free_Result,"mysql_free_result");
      begin
          pragma Unreferenced (Driver);
          if Result /= null then
              if Result_Type (Result.all).Results /= Null_Result then
                  MySQL_Free_Result(Result_Type (Result.all).Results);
                  Result_Type (Result.all).Results := Null_Result;
              end if;
              put_line("Free result");
              Free_Storage (Result_Access (Result));
          end if;
   end Free_Result;

   ----------------------
   -- Get_Capabilities --
   ----------------------

   overriding function Get_Capabilities
     (This              : in Driver_Type)
      return Driver_Capabilities
   is
      pragma Unreferenced (This);
   begin
       -- FIXME Check this shit
      return (
         Insert_Id_Func    => True,
         Random_Access     => False,
         Returning_Clause  => False,
         Has_ilike         => False,
         Count_Name        => False
      );
   end Get_Capabilities;

   ---------------------
   -- Get_Column_Count --
   ---------------------

   overriding function Get_Column_Count
     (Result            : in Result_Type)
      return Natural
   is
   begin
      return Result.Field_Count;
   end Get_Column_Count;

   ---------------------
   -- Get_Data_Bigint --
   ---------------------

   overriding function Get_Data_Bigint
       (
           Result          : in Result_Type;
           Row             : in Tuple_Index;
           Field           : in Column_Index;
           Replace_Null    : in Boolean := False;
           Replacement     : in DB.Types.DB_Bigint := 0
       ) return DB.Types.DB_Bigint
    is
        S : constant String :=
            (Get_Data_String (Result, Row, Field, True, ""));
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
      Row             : in Tuple_Index;
      Field            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in Boolean := False)
      return Boolean
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
      Row             : in Tuple_Index;
      Field            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Integer := 0)
      return DB.Types.DB_Integer
   is
      S : constant String :=
        Get_Data_String (Result, Row, Field, True, "");
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
      Row             : in Tuple_Index;
      Field            : in Column_Index)
      return Boolean
   is
   begin
       Mysql_Data_Seek(Result.Results,Integer(Row-1));
       if Get_Data_Length(Result.Results, Field) = 0 then
            put_line("Is Null");
            return True;
       else
           return False;
       end if;
   end Get_Data_Is_Null;


   ------------------------
   -- Get_Data_Object_Id --
   ------------------------

   overriding function Get_Data_Object_Id
     (Result            : in Result_Type;
      Row             : in Tuple_Index;
      Field            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.Object_Id := 0)
      return DB.Types.Object_Id
   is
      S	: constant String := Get_Data_String (Result, Row, Field);
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
      Row             : in Tuple_Index;
      Field            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Smallint := 0)
      return DB.Types.DB_Smallint
   is
      S	: constant String := Get_Data_String (Result, Row, Field);
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
       function mysql_fetch_Field(Result : Mysql_Result_Access) return MySQL_Field;
           pragma Import (C, mysql_fetch_field,"mysql_fetch_field");
       Lnt              : Integer;
       Lenghts          : Lenghts_Type;
       Current_Row      : MySQL_Row;
       res_text         : Unbounded_String;

   begin
       put_line("Row: " & Row'Img & "   Field:" & Field'Img);
       put_line("Row_Count: " & Result.Row_Count'Img & "   Field_Count:" & Result.Field_Count'Img);
       Mysql_Data_Seek(Result.Results,Integer(Row-1));
       Current_Row := MySQL_Fetch_Row(Result.Results);
       Lenghts      := MySQL_Fetch_Lengths(Result.Results);
       for I in 0..Field loop
           if Lenghts.all /= 0 then
               res_text :=  To_Unbounded_String(Interfaces.C.Strings.Value (Current_Row.all));
               put_line(to_string(res_text));
           end if;
           if Field /= 0 then
               MySQL_Row_Type.Increment(Current_Row);
               Unsigned_Long_Array_Ptr.Increment(Lenghts);
           end if;
       end loop;
       return To_String(res_text);
   end Get_Data_String;

   -------------------------
   -- Get_Foreign_Key_SQL --
   -------------------------

   overriding function Get_Foreign_Key_SQL
     (This : in Driver_Type)
      return DB.Types.SQL_String
   is
      pragma Unreferenced (This);
   begin
      return "BIGINT";
   end Get_Foreign_Key_SQL;

   ----------------
   -- Get_Id_SQL --
   ----------------

   overriding function Get_Id_SQL
     (This : in Driver_Type)
      return DB.Types.SQL_String
   is
      pragma Unreferenced (This);
   begin
      return "BIGINT AUTO_INCREMENT";
   end Get_Id_SQL;

   -------------------------
   -- Get_Inserted_Row_id --
   -------------------------

   overriding function Get_Inserted_Row_id
     (Driver           : in Driver_Type)
      return DB.Types.Object_Id
   is
       Id : Integer;
       function MySQL_Insert_Id(MySQL : MySQL_Access) return Integer;
            pragma Import (C, MySQL_Insert_Id,"mysql_insert_id");
   begin
      -- FIXME Check it
      return DB_Bigserial(MySQL_Insert_Id(Driver.Connection));
   end Get_Inserted_Row_id;

   ------------------------
   -- Get_Server_Version --
   ------------------------

   overriding function Get_Server_Version
     (This : in Driver_Type)
      return String
   is
      Version_Result	: Result_Handle;
   begin
      Execute_SQL (This, Version_Result, "SELECT version()");
      declare
         Version_String	: constant String :=
           Get_Data_String
             (Result       => Result_Type (Version_Result.all),
              Row        => 1,
              Field       => 1,
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
      Maximum_Size      : in Natural)
      return DB.Types.SQL_String
   is
      pragma Unreferenced (Maximum_Size);
      pragma Unreferenced (This);
   begin
       return "Text";
   end Get_Text_Type;

   -------------------
   -- Get_Tuple_Count --
   -------------------

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
     (Driver            : in Driver_Type)
      return Boolean
   is
       function MySQL_Stat(MySQL : MySQL_Access)  return C.Strings.Chars_Ptr;
       pragma Import(C,MySQL_Stat,"mysql_stat");
   begin
       if Driver.Connection /= Null
       then
           if MySQL_Stat(Driver.Connection) = C.Strings.Null_Ptr
           then
               return True;
           end if;
       end if;
       return False;
   end Is_Connected;

   ----------------------
   -- Is_Random_Access --
   ----------------------

   overriding function Is_Random_Access
       (Result            : in Result_Type)
       return Boolean
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
      Identifier        : in String)
      return DB.Types.SQL_String
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

   overriding function Quote_Value
     (Driver            : in Driver_Type;
      Value             : in String)
      return DB.Types.SQL_String
   is
       procedure Free is new Ada.Unchecked_Deallocation (C.char_array, C.Strings.char_array_access);
       -- FIXME Deallocation here
       -- Data       : constant Object_Data_Access := Instance (Obj, "escape");
       -- allocating buffer for encoding the buffer
       Buffer : C.Strings.char_array_access := new C.char_array (1 .. Value'Length * 2 + 1);
       function MySQL_Real_Escape_String
           (
               MySQL    : MySQL_Access;
               To       : C.Strings.Chars_ptr;
               From     : C.Strings.Chars_ptr;
               Length   : C.Unsigned_Long
           ) return C.Unsigned;
           pragma Import(C,MySQL_Real_Escape_String,"mysql_real_escape_string");
   begin
      declare
          Buffer_Ptr : constant C.Strings.chars_ptr := C.Strings.To_Chars_Ptr (Buffer);
          Escape     : C.unsigned  := MYSQL_Real_Escape_String
              (Driver.Connection,
              Buffer_Ptr,
             New_Char_Array(C.To_C(Value)),
              C.unsigned_long (Value'Length));
          pragma Unreferenced (Escape);
          S : constant Unbounded_String := To_Unbounded_String (Strings.Value (Buffer_Ptr));
      begin
          Free (Buffer);
          return "'" & SQL_String(To_String(S)) & "'";
      end;
   end Quote_Value;

   ----------------
   -- Last_Error --
   ----------------

   function Last_Error (Driver : in Driver_Type) return String is
       function MySQL_Error
           (
               Connection : MySQL_Access
           ) return C.Strings.Chars_Ptr;
           pragma Import (C,MySQL_Error,"mysql_error");
   begin
      return C.Strings.Value(MySQL_Error(Driver.Connection));
   end Last_Error;
   ----------------------
   -- Get_Field_Direct --
   ----------------------
   function Get_Field_Direct(Result : MySQL_Result_Access; Field_Index : Integer) return MySQL_Field 
   is
       Field : MySQL_Field;
       function MySQL_Fetch_Field_Direct(Result: MySQL_Result_Access; Index : Integer) return MySQL_Field;
           pragma Import (C,MySQL_Fetch_Field_Direct,"mysql_fetch_field_direct");
   begin
       return MySQL_Fetch_Field_Direct(Result, Field_Index);
   end Get_Field_Direct;

   function Get_Data_Length
          (Result             : in MySQL_Result_Access;
          Field            : in Column_Index)
          return Integer
   is
            Lengths : Lenghts_Type;
            Lnt     : C.Unsigned_long;
            Current_Row : constant MySQL_Row := MySQL_Fetch_Row(Result);
   begin
       Lengths      := MySQL_Fetch_Lengths(Result);
       for J in 0..Field loop
           Lnt := Lengths.all;
           Unsigned_Long_Array_Ptr.Increment(Lengths);
       end loop;
       return Integer(Lnt);
   end Get_Data_Length;

begin
       DB.Driver_Manager.Register_Driver ("mysql", Alloc'Access);
end DB.Driver.MySQL;
