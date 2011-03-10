with System;
with DB.Types;              use DB.Types;
with Ada.Characters.Handling;       use Ada.Characters.Handling;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with DB.Driver_Manager;
with DB.Errors;
with DB.Types;              use DB.Types;
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;      use Interfaces.C.Strings;

package DB.Driver.MySQL is

   pragma Linker_Options ("-lpq");
   pragma Linker_Options ("-L/usr/lib/mysql"); -- FIXME 64bit
   type Driver_Type is new Abstract_Driver_Type with private;

    overriding procedure Connect
        (
              Driver            : in out Driver_Type;
              Hostname          : in     String;
              Database          : in     String;
              Username          : in     String := "";
              Password          : in     String := "";
              Options           : in     String := ""
        );

    overriding procedure Disconnect
        (
            Driver            : in out Driver_Type
        );

    overriding procedure Execute_SQL
        (
            Driver  : in  Driver_Type;
            Result  : out Result_Handle;
            Query   : in  DB.Types.SQL_String
        );

   overriding procedure Free_Result
        (
            Driver            : in     Driver_Type;
            Result            : in out Result_Handle
        );
   overriding function Get_Capabilities
        (
            This              : in Driver_Type
        ) return Driver_Capabilities;
   overriding function Get_Foreign_Key_SQL 
           (
               This : in Driver_Type
           ) return DB.Types.SQL_String;
   overriding function Get_Id_SQL 
       (
          This : in Driver_Type
       ) return DB.Types.SQL_String;
   overriding function Get_Server_Version 
       (
            This : in Driver_Type
       ) return String;
   function Get_Text_Type
            (This              : in Driver_Type;
             Maximum_Size      : in Natural) return DB.Types.SQL_String;
   overriding function Is_Connected
        (Driver            : in Driver_Type) return Boolean;
   overriding function Quote_Identifier
        (Driver            : in Driver_Type;
         Identifier        : in String) return DB.Types.SQL_String;

private
        type MYSQL is new System.Address; 

        Null_Connection      : constant MySQL :=
            MySQL (System.Null_Address);

        type Driver_Type is new Abstract_Driver_Type with record
          Connection        :  MySQL:= Null_Connection;
        end record;


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
        for MySQL_Options'Size use Interfaces.C.Int'Size;
        pragma Convention (C, MYSQL_Options);



        subtype Port is Integer range 1..2**16;
        type Socket is new Interfaces.C.Strings.chars_ptr;
        type Socket_Access is access Interfaces.C.Strings.chars_ptr;

        function Get_Result(
            MySQL : Driver_Type
            ) return  Result_Handle;

end DB.Driver.MySQL;
