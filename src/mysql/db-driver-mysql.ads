with System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;
with Ada.Text_io; use Ada.Text_IO;
with Interfaces.C.Strings;use Interfaces.C.Strings;
with DB.Errors;
with DB.Driver_Manager; use DB.Driver_Manager;
with DB.Types; use DB.Types;
with Ada.Characters.Handling;       use Ada.Characters.Handling;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
package DB.Driver.MySQL is
    -- Move linker to project
    --
    pragma Linker_Options ("-lmysqlclient");
    pragma Linker_Options ("-L/usr/lib64/mysql");

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
      Row             : in Tuple_Index;
      Field            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Bigint := 0)
   return DB.Types.DB_Bigint;

   overriding function Get_Data_Boolean
     (Result            : in Result_Type;
      Row             : in Tuple_Index;
      Field            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in Boolean := False) return Boolean;

   overriding function Get_Data_Integer
     (Result            : in Result_Type;
      Row             : in Tuple_Index;
      Field            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Integer := 0)
   return DB.Types.DB_Integer;

   overriding function Get_Data_Is_Null
     (Result            : in Result_Type;
      Row             : in Tuple_Index;
      Field            : in Column_Index) return Boolean;

   overriding function Get_Data_Object_Id
     (Result            : in Result_Type;
      Row             : in Tuple_Index;
      Field            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.Object_Id := 0)
   return DB.Types.Object_Id;

   overriding function Get_Data_Smallint
     (Result            : in Result_Type;
      Row             : in Tuple_Index;
      Field            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Smallint := 0)
   return DB.Types.DB_Smallint;

   overriding function Get_Data_String
     (Result            : in Result_Type;
      Row             : in Tuple_Index;
      Field            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in String := "") return String;

   overriding function Get_Foreign_Key_SQL (This : in Driver_Type)
   return DB.Types.SQL_String;

   overriding function Get_Id_SQL (This : in Driver_Type)
   return DB.Types.SQL_String;

   overriding function Get_Inserted_Row_id
     (Driver            : in Driver_Type) return DB.Types.Object_Id;

   overriding function Get_Server_Version (This : in Driver_Type) return String;

   function Get_Text_Type
     (This              : in Driver_Type;
      Maximum_Size      : in Natural) return DB.Types.SQL_String;

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
    function Last_Error (Driver : in Driver_Type) return String;
    package C renames Interfaces.C;

    type MySQL is limited null record;
    type MySQL_Access      is access all MySQL;
    Null_Connection        : constant MySQL_Access := Null;
    -- subtype MySQL_Result is System.Address;
    type MySQL_Result      is limited null record;
    type MySQL_Result_Access is access all MySQL_Result;
    Null_Result            : constant MySQL_Result_Access := Null;

    type Driver_Type is new Abstract_Driver_Type with record
        Connection        : MySQL_Access := Null_Connection;
    end record;

     type Result_Type is new Abstract_Result_Type with record
         Field_Count      : Natural := 0;
         Results          : MySQL_Result_Access := Null_Result;
         Row_Count        : Natural := 0;
     end record;
    type MySQL_Row_Array is array (C.size_t range <>)
        of aliased C.Strings.chars_ptr;
    package MySQL_Row_Type is
        new C.Pointers
        (
            Index                   => C.size_t,
            Element                 => C.Strings.chars_ptr,
            Element_Array           => MySQL_Row_Array,
            Default_Terminator      => Null
        );
    type Unsigned_Long_Array is array (Natural range <>)
        of aliased C.unsigned_long;
        Unsigned_Long_Null : aliased constant C.unsigned_long := 0;
    package Unsigned_Long_Array_Ptr is
            new  C.Pointers
        (
            Index               => Natural,
            Element             => C.Unsigned_long,
            Element_Array       => Unsigned_Long_Array,
            Default_Terminator  => Unsigned_Long_Null);
    subtype Lenghts_Type is Unsigned_Long_Array_Ptr.Pointer;
    package Char_Ptrs is
        new C.Pointers
            (
                Index               => C.size_t,
                Element             => C.char,
                Element_Array       => C.char_array,
                Default_Terminator  => C.nul);
    use Char_Ptrs;
    use MySQL_Row_Type;
    subtype MySQL_Row is MySQL_Row_Type.Pointer;
     type Socket_Access is access C.Strings.chars_ptr;

    -- MySQL_Options
    type MySQL_Options is (MYSQL_OPT_CONNECT_TIMEOUT, MYSQL_OPT_COMPRESS, MYSQL_OPT_NAMED_PIPE,
        MYSQL_INIT_COMMAND, MYSQL_READ_DEFAULT_FILE, MYSQL_READ_DEFAULT_GROUP,
        MYSQL_SET_CHARSET_DIR, MYSQL_SET_CHARSET_NAME, MYSQL_OPT_LOCAL_INFILE,
        MYSQL_OPT_PROTOCOL, MYSQL_SHARED_MEMORY_BASE_NAME, MYSQL_OPT_READ_TIMEOUT,
        MYSQL_OPT_WRITE_TIMEOUT, MYSQL_OPT_USE_RESULT,
        MYSQL_OPT_USE_REMOTE_CONNECTION, MYSQL_OPT_USE_EMBEDDED_CONNECTION,
        MYSQL_OPT_GUESS_CONNECTION, MYSQL_SET_CLIENT_IP, MYSQL_SECURE_AUTH,
        MYSQL_REPORT_DATA_TRUNCATION, MYSQL_OPT_RECONNECT,
        MYSQL_OPT_SSL_VERIFY_SERVER_CERT);
    for MySQL_Options'Size use C.Int'Size;
    pragma Convention (C, MYSQL_Options);
    subtype MySQL_Error is Interfaces.C.unsigned_long;
        CR_MIN_ERROR : constant MySQL_Error	:=	2000;
        CR_MAX_ERROR   : constant MySQL_Error   :=		2999;
        CLIENT_ERRMAP  : constant MySQL_Error   :=		2	;
        CR_ERROR_FIRST     : constant MySQL_Error   :=	2000 ;
        CR_UNKNOWN_ERROR   : constant MySQL_Error   :=	2000;
        CR_SOCKET_CREATE_ERROR : constant MySQL_Error   :=	2001;
        CR_CONNECTION_ERROR    : constant MySQL_Error   :=	2002;
        CR_CONN_HOST_ERROR : constant MySQL_Error   :=	2003;
        CR_IPSOCK_ERROR    : constant MySQL_Error   :=		2004;
        CR_UNKNOWN_HOST    : constant MySQL_Error   :=		2005;
        CR_SERVER_GONE_ERROR   : constant MySQL_Error   :=	2006;
        CR_VERSION_ERROR   : constant MySQL_Error   :=	2007;
        CR_OUT_OF_MEMORY   : constant MySQL_Error   :=	2008;
        CR_WRONG_HOST_INFO : constant MySQL_Error   :=	2009;
        CR_LOCALHOST_CONNECTION : constant MySQL_Error :=   2010;
        CR_TCP_CONNECTION	: constant MySQL_Error :=    2011;
        CR_SERVER_HANDSHAKE_ERR : constant MySQL_Error :=   2012;
        CR_SERVER_LOST		: constant MySQL_Error :=    2013;
        CR_COMMANDS_OUT_OF_SYNC : constant MySQL_Error :=   2014;
        CR_NAMEDPIPE_CONNECTION : constant MySQL_Error :=   2015;
        CR_NAMEDPIPEWAIT_ERROR  : constant MySQL_Error :=   2016;
        CR_NAMEDPIPEOPEN_ERROR  : constant MySQL_Error :=   2017;
        CR_NAMEDPIPESETSTATE_ERROR : constant MySQL_Error :=    2018;
        CR_CANT_READ_CHARSET	: constant MySQL_Error :=    2019;
        CR_NET_PACKET_TOO_LARGE : constant MySQL_Error :=   2020;
        CR_EMBEDDED_CONNECTION	: constant MySQL_Error :=    2021;
        CR_PROBE_SLAVE_STATUS   : constant MySQL_Error :=   2022;
        CR_PROBE_SLAVE_HOSTS    : constant MySQL_Error :=   2023;
        CR_PROBE_SLAVE_CONNECT  : constant MySQL_Error :=   2024;
        CR_PROBE_MASTER_CONNECT : constant MySQL_Error :=   2025;
        CR_SSL_CONNECTION_ERROR : constant MySQL_Error :=   2026;
        CR_MALFORMED_PACKET     : constant MySQL_Error :=   2027;
        CR_WRONG_LICENSE	: constant MySQL_Error :=    2028;
        CR_NULL_POINTER		: constant MySQL_Error :=    2029;
        CR_NO_PREPARE_STMT	: constant MySQL_Error :=    2030;
        CR_PARAMS_NOT_BOUND	: constant MySQL_Error :=    2031;
        CR_DATA_TRUNCATED	: constant MySQL_Error :=    2032;
        CR_NO_PARAMETERS_EXISTS : constant MySQL_Error :=   2033;
        CR_INVALID_PARAMETER_NO : constant MySQL_Error :=   2034;
        CR_INVALID_BUFFER_USE	: constant MySQL_Error :=    2035;
        CR_UNSUPPORTED_PARAM_TYPE : constant MySQL_Error := 2036;
        CR_SHARED_MEMORY_CONNECTION             : constant MySQL_Error :=   2037;
        CR_SHARED_MEMORY_CONNECT_REQUEST_ERROR  : constant MySQL_Error :=   2038;
        CR_SHARED_MEMORY_CONNECT_ANSWER_ERROR   : constant MySQL_Error :=   2039;
        CR_SHARED_MEMORY_CONNECT_FILE_MAP_ERROR : constant MySQL_Error :=   2040;
        CR_SHARED_MEMORY_CONNECT_MAP_ERROR      : constant MySQL_Error :=   2041;
        CR_SHARED_MEMORY_FILE_MAP_ERROR         : constant MySQL_Error :=   2042;
        CR_SHARED_MEMORY_MAP_ERROR              : constant MySQL_Error :=   2043;
        CR_SHARED_MEMORY_EVENT_ERROR     	: constant MySQL_Error :=    2044;
        CR_SHARED_MEMORY_CONNECT_ABANDONED_ERROR : constant MySQL_Error :=  2045;
        CR_SHARED_MEMORY_CONNECT_SET_ERROR      : constant MySQL_Error :=   2046;
        CR_CONN_UNKNOW_PROTOCOL 		: constant MySQL_Error :=    2047;
        CR_INVALID_CONN_HANDLE			: constant MySQL_Error :=    2048;
        CR_SECURE_AUTH                          : constant MySQL_Error :=   2049;
        CR_FETCH_CANCELED                       : constant MySQL_Error :=   2050;
        CR_NO_DATA                              : constant MySQL_Error :=   2051;
        CR_NO_STMT_METADATA                     : constant MySQL_Error :=   2052;
        CR_NO_RESULT_SET                        : constant MySQL_Error :=   2053;
        CR_NOT_IMPLEMENTED                      : constant MySQL_Error :=   2054;
        CR_SERVER_LOST_EXTENDED			: constant MySQL_Error :=    2055;
        CR_STMT_CLOSED				: constant MySQL_Error :=    2056;
        CR_NEW_STMT_METADATA                    : constant MySQL_Error :=   2057;
        CR_ERROR_LAST    : constant MySQL_Error :=  2057;
    type MYSQL_Field_Types is (            -- mysql_com.h:136
        MYSQL_TYPE_DECIMAL,
        MYSQL_TYPE_TINY,
        MYSQL_TYPE_SHORT,
        MYSQL_TYPE_LONG,
        MYSQL_TYPE_FLOAT,
        MYSQL_TYPE_DOUBLE,
        MYSQL_TYPE_NULL,
        MYSQL_TYPE_TIMESTAMP,
        MYSQL_TYPE_LONGLONG,
        MYSQL_TYPE_INT24,
        MYSQL_TYPE_DATE,
        MYSQL_TYPE_TIME,
        MYSQL_TYPE_DATETIME,
        MYSQL_TYPE_YEAR,
        MYSQL_TYPE_NEWDATE,
        MYSQL_TYPE_VARCHAR,
        MYSQL_TYPE_BIT,
        MYSQL_TYPE_NEWDECIMAL,
        MYSQL_TYPE_ENUM,
        MYSQL_TYPE_SET,
        MYSQL_TYPE_TINY_BLOB,
        MYSQL_TYPE_MEDIUM_BLOB,
        MYSQL_TYPE_LONG_BLOB,
        MYSQL_TYPE_BLOB,
        MYSQL_TYPE_VAR_STRING,
        MYSQL_TYPE_STRING,
        MYSQL_TYPE_GEOMETRY);
    for MYSQL_Field_Types use (
        MYSQL_TYPE_DECIMAL      =>  0,
        MYSQL_TYPE_TINY         =>  1,
        MYSQL_TYPE_SHORT        =>  2,
        MYSQL_TYPE_LONG         =>  3,
        MYSQL_TYPE_FLOAT        =>  4,
        MYSQL_TYPE_DOUBLE       =>  5,
        MYSQL_TYPE_NULL         =>  6,
        MYSQL_TYPE_TIMESTAMP    =>  7,
        MYSQL_TYPE_LONGLONG     =>  8,
        MYSQL_TYPE_INT24        =>  9,
        MYSQL_TYPE_DATE         =>  10,
        MYSQL_TYPE_TIME         =>  11,
        MYSQL_TYPE_DATETIME     =>  12,
        MYSQL_TYPE_YEAR         =>  13,
        MYSQL_TYPE_NEWDATE      =>  14,
        MYSQL_TYPE_VARCHAR      =>  15,
        MYSQL_TYPE_BIT          =>  16,
        MYSQL_TYPE_NEWDECIMAL   =>  246,
        MYSQL_TYPE_ENUM         =>  247,
        MYSQL_TYPE_SET          =>  248,
        MYSQL_TYPE_TINY_BLOB    =>  249,
        MYSQL_TYPE_MEDIUM_BLOB  =>  250,
        MYSQL_TYPE_LONG_BLOB    =>  251,
        MYSQL_TYPE_BLOB         =>  252,
        MYSQL_TYPE_VAR_STRING   =>  253,
        MYSQL_TYPE_STRING       =>  254,
        MYSQL_TYPE_GEOMETRY     =>  255
        );
    for MYSQL_Field_Types'Size use 27;
    type St_Mysql_Field is                      -- mysql.h:92
        record
            Name                : C.Strings.chars_ptr;    -- Name of column
            Orig_Name           : C.Strings.chars_ptr;    -- Original column name,
            Table               : C.Strings.chars_ptr;    -- Table of column
            Orig_Table          : C.Strings.chars_ptr;    -- Org table name,
            Database            : C.Strings.chars_ptr;    -- Database name
            Catalog             : C.Strings.chars_ptr;    -- Def /* I don't know
            Def                 : C.Strings.chars_ptr;    --
            Length              : C.unsigned_Long;             -- Width of column
            Max_Length          : C.unsigned_long;            -- mysql.h:98
            name_length         : C.Int;
            org_name_length     : C.Int ; 
            table_length        : C.Int ; 
            org_table_length    : C.Int ; 
            db_length           : C.Int ; 
            catalog_length      : C.Int ; 
            def_length          : C.Int ; 
            flags               : C.Int ; 
            decimals            : C.Int ; 
            charsetnr           : C.Int ; 
            C_Type              : MYSQL_Field_Types;                 -- mysql.h:96
        end record;
    pragma Convention (C, St_Mysql_Field);
    type MYSQL_Field is access all St_Mysql_Field;

   function Get_Field_Direct(Result : MySQL_Result_Access; Field_Index : Integer) return MySQL_Field; 
   function Get_Data_Length(Result : in MySQL_Result_Access; Field : in Column_Index) return Integer;
   procedure mysql_data_seek(Result : Mysql_Result_Access; Offset : Integer);
           pragma Import (C,mysql_data_seek,"mysql_data_seek");
   function mysql_fetch_row(Result : Mysql_Result_Access) return MySQL_Row;
       pragma Import (C, mysql_fetch_row,"mysql_fetch_row");
   function mysql_fetch_lengths(Result : Mysql_Result_Access ) return Lenghts_Type;
       pragma Import (C,mysql_fetch_lengths,"mysql_fetch_lengths");

end DB.Driver.MySQL;
