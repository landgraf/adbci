with System;

package body DB.Driver.MySQL is

   pragma Linker_Options ("-lpq");
   pragma Linker_Options ("-L/usr/lib/mysql"); -- FIXME 64bit

    overriding procedure Connect
        (
              Driver            : in out Driver_Type;
              Hostname          : in     String;
              Database          : in     String;
              Username          : in     String := "";
              Password          : in     String := "";
              Options           : in     String := ""
        ) 
        is
            -- MYSQL *mysql_init(MYSQL *mysql) 
            function Init
                (
                    MySQL : Driver_Type
                ) return Driver_Type;
                pragma Import (C, Init, "mysql_init");
            -- MYSQL *mysql_real_connect(MYSQL *mysql, const char *host, 
            -- const char *user, const char *passwd, const char *db, u
            -- nsigned int port, const char *unix_socket, unsigned long client_flag)
            function Real_Connect 
                (
                    MySQL    : Driver_Type ;
                    HostName : in Interfaces.C.Char_Array;
                    User     : in Interfaces.C.Char_Array;
                    Password : in Interfaces.C.Char_Array;
                    Database : in Interfaces.C.Char_Array;
                    DB_Port  : in Interfaces.C.unsigned;
                    MySQL_Socket   : Socket_Access;
                    Flag     : in Interfaces.C.unsigned_long
                ) return Mysql_Access;
                 pragma Import (C, Real_Connect, "mysql_real_connect");
            -- int mysql_options(MYSQL *mysql, enum mysql_option option, const char *arg)
            function Get_Options
                (
                    MySQL : Driver_Type; 
                    Options : MySQL_Options ; 
                    args : Interfaces.C.Char_Array
                ) return Integer;
                pragma Import (C, Get_Options, "mysql_options");
        begin
            Driver  := Init(MySQL => Driver);
    end Connect;

    overriding procedure Disconnect
         (
            Driver  : in out Driver_Type
         ) is 
        begin
            null;
    end Disconnect;

    overriding procedure Execute_SQL
            (
                Driver  : in  Driver_Type;
                Result  : out Result_Handle;
                Query   : in  DB.Types.SQL_String
            ) is 
        begin
            null;
    end Execute_SQL;

    overriding procedure Free_Result
            (
                Driver            : in     Driver_Type;
                Result            : in out Result_Handle
            ) is
        begin
            null;
    end Free_Result;

    overriding function Get_Capabilities
             (
                 This              : in Driver_Type
             ) return Driver_Capabilities is
        begin
            return new Driver_Capabilities;
    end Get_Capabilities;

    overriding function Get_Foreign_Key_SQL 
            (
                This : in Driver_Type
            ) return DB.Types.SQL_String is 
        begin
            null;
            return new DB.Types.SQL_String;
    end Get_Foreign_Key_SQL;
    overriding function Get_Id_SQL 
        (
            This : in Driver_Type
        )return DB.Types.SQL_String is 
    begin
        return new DB.Types.SQL_String;
    end Get_Id_SQL;

    overriding function Get_Server_Version
        (
            This : in Driver_Type
        ) return String is 
        begin 
            return new String;
        end Get_Server_Version;
   function Get_Text_Type
        (This              : in Driver_Type;
         Maximum_Size      : in Natural) return DB.Types.SQL_String is 
   begin
       return new DB.Types.SQL_String;
   end Get_Text_Type;
   overriding function Is_Connected
        (Driver            : in Driver_Type) return Boolean is 
   begin
        return True;
   end Is_Connected;

   overriding function Quote_Identifier
       (Driver            : in Driver_Type;
        Identifier        : in String) return DB.Types.SQL_String is 
    begin
            return new DB.Types.SQL_String;
   end Quote_Identifier;


end DB.Driver.MySQL;
