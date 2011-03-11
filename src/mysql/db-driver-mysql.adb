package body DB.Driver.MySQL is

   -----------
   -- Alloc --
   -----------

   function Alloc return Driver_Handle is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Alloc unimplemented");
      raise Program_Error with "Unimplemented function Alloc";
      return Alloc;
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
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Connect unimplemented");
      raise Program_Error with "Unimplemented procedure Connect";
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   overriding procedure Disconnect
     (Driver            : in out Driver_Type)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Disconnect unimplemented");
      raise Program_Error with "Unimplemented procedure Disconnect";
   end Disconnect;

   -----------------
   -- Execute_SQL --
   -----------------

   overriding procedure Execute_SQL
     (Driver            : in     Driver_Type;
      Result            :    out Result_Handle;
      Query             : in     DB.Types.SQL_String)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Execute_SQL unimplemented");
      raise Program_Error with "Unimplemented procedure Execute_SQL";
   end Execute_SQL;

   -------------------------
   -- Find_Column_By_Name --
   -------------------------

   overriding function Find_Column_By_Name
     (Result            : in Result_Type;
      Name              : in String)
      return Column_Index
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Find_Column_By_Name unimplemented");
      raise Program_Error with "Unimplemented function Find_Column_By_Name";
      return Find_Column_By_Name (Result, Name);
   end Find_Column_By_Name;

   -----------------
   -- Free_Result --
   -----------------

   overriding procedure Free_Result
     (Driver            : in     Driver_Type;
      Result            : in out Result_Handle)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Free_Result unimplemented");
      raise Program_Error with "Unimplemented procedure Free_Result";
   end Free_Result;

   ----------------------
   -- Get_Capabilities --
   ----------------------

   overriding function Get_Capabilities
     (This              : in Driver_Type)
      return Driver_Capabilities
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Capabilities unimplemented");
      raise Program_Error with "Unimplemented function Get_Capabilities";
      return Get_Capabilities (This);
   end Get_Capabilities;

   ----------------------
   -- Get_Column_Count --
   ----------------------

   overriding function Get_Column_Count
     (Result            : in Result_Type)
      return Natural
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Column_Count unimplemented");
      raise Program_Error with "Unimplemented function Get_Column_Count";
      return Get_Column_Count (Result);
   end Get_Column_Count;

   ---------------------
   -- Get_Data_Bigint --
   ---------------------

   overriding function Get_Data_Bigint
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Bigint := 0)
      return DB.Types.DB_Bigint
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Data_Bigint unimplemented");
      raise Program_Error with "Unimplemented function Get_Data_Bigint";
      return Get_Data_Bigint (Result, Tuple, Column, Replace_Null, Replacement);
   end Get_Data_Bigint;

   ----------------------
   -- Get_Data_Boolean --
   ----------------------

   overriding function Get_Data_Boolean
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in Boolean := False)
      return Boolean
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Data_Boolean unimplemented");
      raise Program_Error with "Unimplemented function Get_Data_Boolean";
      return Get_Data_Boolean (Result, Tuple, Column, Replace_Null,
         Replacement);
   end Get_Data_Boolean;

   ----------------------
   -- Get_Data_Integer --
   ----------------------

   overriding function Get_Data_Integer
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Integer := 0)
      return DB.Types.DB_Integer
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Data_Integer unimplemented");
      raise Program_Error with "Unimplemented function Get_Data_Integer";
      return Get_Data_Integer (Result, Tuple, Column, Replace_Null,
         Replacement);
   end Get_Data_Integer;

   ----------------------
   -- Get_Data_Is_Null --
   ----------------------

   overriding function Get_Data_Is_Null
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index)
      return Boolean
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Data_Is_Null unimplemented");
      raise Program_Error with "Unimplemented function Get_Data_Is_Null";
      return Get_Data_Is_Null (Result, Tuple, Column);
   end Get_Data_Is_Null;

   ------------------------
   -- Get_Data_Object_Id --
   ------------------------

   overriding function Get_Data_Object_Id
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.Object_Id := 0)
      return DB.Types.Object_Id
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Data_Object_Id unimplemented");
      raise Program_Error with "Unimplemented function Get_Data_Object_Id";
      return Get_Data_Object_Id (Result, Tuple, Column, Replace_Null,
         Replacement);
   end Get_Data_Object_Id;

   -----------------------
   -- Get_Data_Smallint --
   -----------------------

   overriding function Get_Data_Smallint
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Smallint := 0)
      return DB.Types.DB_Smallint
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Data_Smallint unimplemented");
      raise Program_Error with "Unimplemented function Get_Data_Smallint";
      return Get_Data_Smallint (Result, Tuple, Column, Replace_Null,
         Replacement);
   end Get_Data_Smallint;

   ---------------------
   -- Get_Data_String --
   ---------------------

   overriding function Get_Data_String
     (Result            : in Result_Type;
      Tuple             : in Tuple_Index;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in String := "")
      return String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Data_String unimplemented");
      raise Program_Error with "Unimplemented function Get_Data_String";
      return Get_Data_String (Result, Tuple, Column, Replace_Null, Replacement);
   end Get_Data_String;

   -------------------------
   -- Get_Foreign_Key_SQL --
   -------------------------

   overriding function Get_Foreign_Key_SQL
     (This : in Driver_Type)
      return DB.Types.SQL_String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Foreign_Key_SQL unimplemented");
      raise Program_Error with "Unimplemented function Get_Foreign_Key_SQL";
      return Get_Foreign_Key_SQL (This);
   end Get_Foreign_Key_SQL;

   ----------------
   -- Get_Id_SQL --
   ----------------

   overriding function Get_Id_SQL
     (This : in Driver_Type)
      return DB.Types.SQL_String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Id_SQL unimplemented");
      raise Program_Error with "Unimplemented function Get_Id_SQL";
      return Get_Id_SQL (This);
   end Get_Id_SQL;

   -------------------------
   -- Get_Inserted_Row_id --
   -------------------------

   overriding function Get_Inserted_Row_id
     (Result            : in Result_Type)
      return DB.Types.Object_Id
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Inserted_Row_id unimplemented");
      raise Program_Error with "Unimplemented function Get_Inserted_Row_id";
      return Get_Inserted_Row_id (Result);
   end Get_Inserted_Row_id;

   ------------------------
   -- Get_Server_Version --
   ------------------------

   overriding function Get_Server_Version
     (This : in Driver_Type)
      return String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Server_Version unimplemented");
      raise Program_Error with "Unimplemented function Get_Server_Version";
      return Get_Server_Version (This);
   end Get_Server_Version;

   ---------------------
   -- Get_Tuple_Count --
   ---------------------

   overriding function Get_Tuple_Count
     (Result            : in Result_Type)
      return Natural
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Tuple_Count unimplemented");
      raise Program_Error with "Unimplemented function Get_Tuple_Count";
      return Get_Tuple_Count (Result);
   end Get_Tuple_Count;

   ------------------
   -- Is_Connected --
   ------------------

   overriding function Is_Connected
     (Driver            : in Driver_Type)
      return Boolean
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Is_Connected unimplemented");
      raise Program_Error with "Unimplemented function Is_Connected";
      return Is_Connected (Driver);
   end Is_Connected;

   ----------------------
   -- Is_Random_Access --
   ----------------------

   overriding function Is_Random_Access
     (Result            : in Result_Type)
      return Boolean
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Is_Random_Access unimplemented");
      raise Program_Error with "Unimplemented function Is_Random_Access";
      return Is_Random_Access (Result);
   end Is_Random_Access;

   ----------------------
   -- Quote_Identifier --
   ----------------------

   overriding function Quote_Identifier
     (Driver            : in Driver_Type;
      Identifier        : in String)
      return DB.Types.SQL_String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Quote_Identifier unimplemented");
      raise Program_Error with "Unimplemented function Quote_Identifier";
      return Quote_Identifier (Driver, Identifier);
   end Quote_Identifier;

   -----------------
   -- Quote_Value --
   -----------------

   overriding function Quote_Value
     (Driver            : in Driver_Type;
      Value             : in String)
      return DB.Types.SQL_String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Quote_Value unimplemented");
      raise Program_Error with "Unimplemented function Quote_Value";
      return Quote_Value (Driver, Value);
   end Quote_Value;

   -------------------
   -- Get_Text_Type --
   -------------------

   function Get_Text_Type
     (This              : in Driver_Type;
      Maximum_Size      : in Natural)
      return DB.Types.SQL_String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Get_Text_Type unimplemented");
      raise Program_Error with "Unimplemented function Get_Text_Type";
      return Get_Text_Type (This, Maximum_Size);
   end Get_Text_Type;

   ----------------
   -- Last_Error --
   ----------------

   function Last_Error (Driver : in Driver_Type) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "Last_Error unimplemented");
      raise Program_Error with "Unimplemented function Last_Error";
      return Last_Error (Driver);
   end Last_Error;

end DB.Driver.MySQL;
