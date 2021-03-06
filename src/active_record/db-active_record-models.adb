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
--    db-active_record-models.adb   jvinters   17-January-2011
--

with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Ada.Strings;                      use Ada.Strings;
with Ada.Strings.Fixed;                use Ada.Strings.Fixed;
with Ada.Tags;
with DB.Active_Record.Fields.Model_Operations;
with DB.Driver;
with DB.Errors;


package body DB.Active_Record.Models is

   type ACS is access constant String;
   type Boolean_String_Array is array (Boolean range <>) of ACS;

   For_Update_Strings   : constant Boolean_String_Array :=
     (False             => new String'(" "),
      True              => new String'(" FOR UPDATE "));

   ---------
   -- "=" --
   ---------

   function "="
     (Left              : in Model;
      Right             : in Model)
     return DB.Active_Record.Fields.Field_Criteria
   is
      use DB.Active_Record.Fields;
      Temp              : Field_Criteria;
   begin
      Set_Criteria
        (Temp, Left.Id, EQUAL, DB.Types.Object_Id'Image (Right.Get_Id));
      return Temp;
   end "=";

   function "="
     (Left              : in Model;
      Right             : in DB.Types.Object_Id)
     return DB.Active_Record.Fields.Field_Criteria
   is
      use DB.Active_Record.Fields;
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left.Id, EQUAL, DB.Types.Object_Id'Image (Right));
      return Temp;
   end "=";

   -----------
   -- Clean --
   -----------

   procedure Clean
     (This              : in out Model;
      Connection        : in out DB.Connector.Connection)
   is
      pragma Unreferenced (Connection);
      pragma Unreferenced (This);
   begin
      null;
   end Clean;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Model'Class) is
      procedure Clear_Field (Field : in out DB.Active_Record.Fields.Field'Class)
      is
      begin
         Field.Clear;
      end Clear_Field;
   begin
      This.Changed := False;
      This.Store := STORE_INSERT;
      Iterate_Fields (This, Clear_Field'Access);
      This.On_After_Initialize;
   end Clear;

   --------------
   -- Clear_Id --
   --------------

   procedure Clear_Id (This : in out Model'Class) is
   begin
      This.Changed := True;
      This.Store := STORE_INSERT;
      This.Id.Clear;
   end Clear_Id;

   ------------
   -- Create --
   ------------

   procedure Create
     (This              : in out Model'Class;
      Connection        : in out DB.Connector.Connection)
   is
      Command           : constant DB.Types.SQL_String :=
        This.Create_SQL (Connection);
      Temp_Result       : DB.Connector.Result_Set;
      pragma Unreferenced (Temp_Result);
   begin
      Temp_Result := Connection.Execute (Command);
   end Create;

   ----------------
   -- Create_SQL --
   ----------------

   function Create_SQL
     (This              : not null access Model'Class;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
      Current_Field     : Natural := 0;
      Field_Count       : constant Natural := This.all.Field_Count;
      Generated_SQL     : Unbounded_String;
      Has_Indices	: Boolean := False;

      procedure Generate_Field (F : in out DB.Active_Record.Fields.Field'Class)
      is
      begin
         Current_Field := Current_Field + 1;
         Append (Generated_SQL, "   ");
         Append (Generated_SQL, String (F.Field_SQL (Connection)));
         if Current_Field /= Field_Count then
            Append (Generated_SQL, ',' & ASCII.LF);
         else
            Append (Generated_SQL, ASCII.LF);
         end if;
         if not F.Is_Unique and not F.Is_Primary_Key and F.Is_Indexed then
            --  most databases auto-create indices for UNIQUE and PRIMARY KEY
            --  columns, so skip index creation for these types of columns.
            Has_Indices := True;
         end if;
      end Generate_Field;

      procedure Generate_Field_Index (F : in out DB.Active_Record.Fields.Field'Class)
      is
      begin
         if not F.Is_Unique and not F.Is_Primary_Key and F.Is_Indexed then
            Append (Generated_SQL, "CREATE INDEX ");
            Append (Generated_SQL, F.Get_Index_Name);
            Append (Generated_SQL, " ON ");
            Append (Generated_SQL, This.all.Model_Name);
            Append (Generated_SQL, " (");
            Append (Generated_SQL, F.Get_Name);
            Append (Generated_SQL, ");" & ASCII.LF);
         end if;
      end Generate_Field_Index;
   begin
      Set_Unbounded_String (Generated_SQL, "CREATE TABLE ");
      Append (Generated_SQL, This.all.Model_Name);
      Append (Generated_SQL, " (" & ASCII.LF);
      This.all.Iterate_Fields (Generate_Field'Access);
      Append (Generated_SQL, ");");

      if Has_Indices then
         This.all.Iterate_Fields (Generate_Field_Index'Access);
      end if;

      return DB.Types.SQL_String (To_String (Generated_SQL));
   end Create_SQL;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (This              : in out Model;
      Connection        : in out DB.Connector.Connection)
   is
   begin
      This.On_Before_Delete (Connection);

      if This.Store = STORE_UPDATE then
         declare
            Id_Name     : constant String := To_String (This.Id_Name);
            Model_Name  : constant String := To_String (This.Model_Name);
            SQL         : constant DB.Types.SQL_String :=
              DB.Types.SQL_String ("DELETE FROM " & Model_Name & " WHERE " &
                                   Id_Name & "=") & This.Id.To_SQL (Connection);
            Result      : DB.Connector.Result_Set;
            pragma Unreferenced (Result);
         begin
            This.Clear;
            Result := Connection.Execute (SQL);
         end;
      end if;
   end Delete;

   ----------
   -- Drop --
   ----------

   procedure Drop
     (This              : in out Model'Class;
      Connection        : in out DB.Connector.Connection)
   is
      Command           : constant DB.Types.SQL_String :=
        This.Drop_SQL (Connection);
      Temp_Result       : DB.Connector.Result_Set;
      pragma Unreferenced (Temp_Result);
   begin
      Temp_Result := Connection.Execute (Command);
   end Drop;

   --------------
   -- Drop_SQL --
   --------------

   function Drop_SQL
     (This              : not null access Model'Class;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
      pragma Unreferenced (Connection);
   begin
      return "DROP TABLE "
        & DB.Types.SQL_String (To_String (This.all.Model_Name) & ';');
   end Drop_SQL;

   ---------
   -- Get --
   ---------

   procedure Get
     (This              : in out Model'Class;
      Connection        : in     DB.Connector.Connection;
      Id                : in     DB.Types.Object_Id;
      For_Update        : in     Boolean := False;
      Load_Foreign_Keys : in     Boolean := True)
   is
      For_Update_Str    : constant DB.Types.SQL_String :=
        DB.Types.SQL_String (For_Update_Strings (For_Update).all);
      Id_Name           : constant String := To_String (This.Id_Name);
      Id_Str            : constant String :=
        Trim (DB.Types.Object_Id'Image (Id), Both);
      Model_Name        : constant String := To_String (This.Model_Name);
      Query_SQL         : constant DB.Types.SQL_String :=
        DB.Types.SQL_String ("SELECT * FROM " &
                             Model_Name & " WHERE " & Id_Name & "=" & Id_Str) &
                             ' ' & For_Update_Str;
      Query_Result      : constant DB.Connector.Result_Set :=
        Connection.Execute (Query_SQL);

      procedure Load_Field (F : in out DB.Active_Record.Fields.Field'Class) is
      begin
         F.Load_From (Connection, Query_Result, Load_Foreign_Keys);
      end Load_Field;
   begin
      This.Clear;
      if Query_Result.Count /= 1 then
         raise DB.Errors.OBJECT_NOT_FOUND with
           Model_Name & " with id '" & Id_Str & "' not found";
      else
         This.Store := STORE_UPDATE;
         This.Iterate_Fields (Load_Field'Access);
      end if;
   end Get;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (This : in Model'Class) return DB.Types.Object_Id is
   begin
      return This.Id.Get;
   end Get_Id;

   ------------------
   -- Get_Id_Field --
   ------------------

   function Get_Id_Field (This : in Model'Class)
                          return DB.Active_Record.Fields.Id_Field
   is
   begin
      return This.Id;
   end Get_Id_Field;

   -----------------
   -- Get_Id_Name --
   -----------------

   function Get_Id_Name (This : in Model'Class) return String is
   begin
      return To_String (This.Id_Name);
   end Get_Id_Name;

   -------------------
   -- Get_Id_String --
   -------------------

   function Get_Id_String (This : in Model'Class) return String is
   begin
      return Trim (DB.Types.Object_Id'Image (This.Id.Get), Both);
   end Get_Id_String;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (This : in Model'Class) return String is
   begin
      return To_String (This.Model_Name);
   end Get_Name;

   -------------------
   -- Get_Read_Only --
   -------------------

   function Get_Read_Only (This : in Model'Class) return Boolean is
   begin
      return This.Read_Only;
   end Get_Read_Only;

   ----------------------
   -- Get_Store_Method --
   ----------------------

   function Get_Store_Method (This : in Model'Class) return Store_Method is
   begin
      return This.Store;
   end Get_Store_Method;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Model) is
      Expanded_Name     : constant String :=
        Ada.Tags.Expanded_Name (Model'Class (This)'Tag);
   begin
      if not This.Initialized then
         for i in reverse Expanded_Name'Range loop
            if Expanded_Name (i) = '.' then
               This.Set_Name (To_Lower
                 (Expanded_Name (i + 1 .. Expanded_Name'Last)));
               exit;
            end if;
         end loop;

         This.Set_Id_Name (This.Get_Name & "_id");
         Initialize_Model (Model'Class (This));
         Prepare_Fields (This);
         This.Initialized := True;
         This.On_After_Initialize;
      end if;
   end Initialize;

   ----------------
   -- Is_Changed --
   ----------------

   function Is_Changed (This : in Model'Class) return Boolean is
   begin
      return This.Changed;
   end Is_Changed;

   ----------------------
   -- Initialize_Model --
   ----------------------

   procedure Initialize_Model (This : in out Model) is
   begin
      null;    --  by default, does nothing...
   end Initialize_Model;

   --------------------
   -- Iterate_Fields --
   --------------------

   procedure Iterate_Fields
     (This              : in out Model'Class;
      Handler           : not null access procedure
        (Field : in out DB.Active_Record.Fields.Field'Class))
   is
   begin
      Handler.all (This.Id);
      This.Iterate_Custom_Fields (Handler);
   end Iterate_Fields;

   ---------------
   -- Load_From --
   ---------------

   procedure Load_From
     (This              : in out Model'Class;
      Connection        : in     DB.Connector.Connection;
      Result            : in out DB.Connector.Result_Set;
      Tuple             : in     DB.Tuple_Index)
   is
      procedure Load_Field (Field : in out DB.Active_Record.Fields.Field'Class)
      is
      begin
         Field.Load_From (Connection, Result);
      end Load_Field;
   begin
      Result.Jump_Tuple (Tuple);
      This.Clear;
      This.Store := STORE_UPDATE;
      This.Iterate_Fields (Load_Field'Access);

      This.On_After_Load (Connection);
   end Load_From;

   -------------------------
   -- On_After_Initialize --
   -------------------------

   procedure On_After_Initialize
     (This              : in out Model)
   is
      pragma Unreferenced (This);
   begin
      null;
   end On_After_Initialize;

   -------------------
   -- On_After_Load --
   -------------------

   procedure On_After_Load
     (This              : in out Model;
      Connection        : in     DB.Connector.Connection)
   is
      pragma Unreferenced (Connection);
      pragma Unreferenced (This);
   begin
      null;
   end On_After_Load;

   -------------------
   -- On_After_Save --
   -------------------

   procedure On_After_Save
     (This              : in out Model;
      Connection        : in out DB.Connector.Connection)
   is
      pragma Unreferenced (Connection);
      pragma Unreferenced (This);
   begin
      null;
   end On_After_Save;

   ----------------------
   -- On_Before_Delete --
   ----------------------

   procedure On_Before_Delete
     (This              : in out Model;
      Connection        : in out DB.Connector.Connection)
   is
      pragma Unreferenced (Connection);
      pragma Unreferenced (This);
   begin
      null;
   end On_Before_Delete;

   --------------------
   -- On_Before_Save --
   --------------------

   procedure On_Before_Save
     (This              : in out Model;
      Connection        : in out DB.Connector.Connection)
   is
      pragma Unreferenced (Connection);
      pragma Unreferenced (This);
   begin
      null;
   end On_Before_Save;

   --------------
   -- Pre_Save --
   --------------

   procedure Pre_Save (This : in out Model'Class) is
      procedure Check_Field (Field : in out DB.Active_Record.Fields.Field'Class)
      is
      begin
         if Field.Is_Changed then
            This.Changed := True;
         end if;
      end Check_Field;
   begin
      This.Changed := False;
      This.Iterate_Fields (Check_Field'Access);
   end Pre_Save;

   --------------------
   -- Prepare_Fields --
   --------------------

   procedure Prepare_Fields (This : in out Model'Class) is
      Model_Name        : constant String := This.Get_Name;

      procedure Count_and_Clear (F : in out DB.Active_Record.Fields.Field'Class)
      is
      begin
         DB.Active_Record.Fields.Model_Operations.Set_Model_Name
           (F, Model_Name);
         This.Field_Count := This.Field_Count + 1;
         F.Clear;
      end Count_and_Clear;
   begin
      This.Field_Count := 0;
      Iterate_Fields (This, Count_and_Clear'Access);
   end Prepare_Fields;

   ------------
   -- Reload --
   ------------

   procedure Reload
     (This              : in out Model'Class;
      Connection        : in     DB.Connector.Connection;
      For_Update        : in     Boolean := False;
      Load_Foreign_Keys : in     Boolean := True)
   is
   begin
      Get (This              => This,
           Connection        => Connection,
           Id                => This.Id.Get,
           For_Update        => For_Update,
           Load_Foreign_Keys => Load_Foreign_Keys);
   end Reload;

   ----------
   -- Save --
   ----------

   procedure Save
     (This              : in out Model'Class;
      Connection        : in out DB.Connector.Connection;
      Force_Save        : in     Boolean := False)
   is
      procedure Clear_Validation_Errors
        (F : in out DB.Active_Record.Fields.Field'Class)
      is
      begin
         F.Clear_Validation;
      end Clear_Validation_Errors;

      procedure Detect_Validation_Errors
        (F : in out DB.Active_Record.Fields.Field'Class)
      is
      begin
         --  Throw an exception at the first field we find that
         --  has a validation error.  Calling code can search the fields
         --  manually for further information if required.
         if F.Validation_Failed then
            raise DB.Errors.VALIDATION_ERROR with F.Validation_Error;
         end if;
      end Detect_Validation_Errors;

      procedure Pre_Save_Prepare
        (F : in out DB.Active_Record.Fields.Field'Class)
      is
      begin
         F.Pre_Save;
      end Pre_Save_Prepare;
   begin
      --  First, check to see if item is read only.
      if This.Read_Only then
         raise DB.Errors.OBJECT_READ_ONLY with
           "object '" & This.Get_Name & "' with id '" & This.Get_Id_String &
           "' is read only";
      end if;

      if This.Id.Is_Changed then
         --  Id has been overwritten, so treat as a new object -- force an
         --  INSERT rather than an UPDATE.
         This.Store := STORE_INSERT;
      end if;

      --  Allow any clean-up of fields before validation takes place.
      This.Clean (Connection);

      --  Validate the model before we save; validators should mark fields
      --  as having failed validation if required.  VALIDATION_ERROR will
      --  be raised if any of the custom fields in the model have errors.
      This.Iterate_Custom_Fields (Clear_Validation_Errors'Access);
      This.Iterate_Custom_Fields (Pre_Save_Prepare'Access);
      This.Validate_Constraints;
      This.Validate (Connection);
      This.Iterate_Custom_Fields (Detect_Validation_Errors'Access);

      --  If validation is successful, trigger the On_Before_Save event.
      This.On_Before_Save (Connection);

      --  determine if any fields need to be saved; if there are no changes
      --  and we are not forcing a save, then nothing will be written back
      --  to the database.
      if not Force_Save then
         Pre_Save (This);
      end if;

      if This.Changed or else Force_Save then
         if This.Store = STORE_INSERT then
            Save_Insert (This, Connection);
         else
            Save_Update (This, Connection);
         end if;

         This.On_After_Save (Connection);
      end if;
   end Save;

   -----------------
   -- Save_Insert --
   -----------------

   procedure Save_Insert
     (This              : in out Model'Class;
      Connection        : in out DB.Connector.Connection)
   is
      Conn_Driver       : constant DB.Driver.Driver_Handle :=
        Connection.Get_Driver;
      Driver_Caps       : constant DB.Driver.Driver_Capabilities :=
        Conn_Driver.all.Get_Capabilities;
      Insert_SQL        : Unbounded_String;
      Field_Count       : Natural := 1;
      Field_List        : Unbounded_String;
      Field_Values      : Unbounded_String;
      Total_Fields      : constant Positive := This.Field_Count;
      Id_Inserted       : DB_Bigserial;

      procedure Handle_Field (F : in out DB.Active_Record.Fields.Field'Class) is
         Field_SQL      : constant DB.Types.SQL_String := F.To_SQL (Connection);
      begin
         Field_Count := Field_Count + 1;
         Append (Field_List, F.Get_Name);
         Append (Field_Values, String (Field_SQL));
         if Field_Count /= Total_Fields then
            Append (Field_List, ", ");
            Append (Field_Values, ", ");
         end if;
      end Handle_Field;
   begin
      Set_Unbounded_String (Insert_SQL, "INSERT INTO ");
      Append (Insert_SQL, This.Model_Name);
      Append (Insert_SQL, " (");
      This.Iterate_Custom_Fields (Handle_Field'Access);
      Append (Insert_SQL, Field_List);
      Append (Insert_SQL, ") VALUES (");
      Append (Insert_SQL, Field_Values);
      Append (Insert_SQL, ")");

      if Driver_Caps.Returning_Clause then
         Append (Insert_SQL, " RETURNING " & This.Get_Id_Name);
      end if;

      declare
         SQL_Command    : constant DB.Types.SQL_String :=
           DB.Types.SQL_String (To_String (Insert_SQL));
         Results        : constant DB.Connector.Result_Set :=
           Connection.Execute (SQL_Command);
      begin
         This.Store := STORE_UPDATE;
         if Driver_Caps.Returning_Clause then
             This.Id.Load_From (Connection, Results);
         else
              Id_Inserted := Conn_Driver.Get_Inserted_Row_id;

              Set_Unbounded_String (Insert_SQL, "SELECT * FROM ");
              Append(Insert_SQL, This.Model_Name);
              Append(Insert_SQL, " WHERE ");
              Append(Insert_SQL, This.Get_Id_Name);
              Append(Insert_SQL, "=");
              Append(Insert_SQL, Trim(Id_Inserted'Img,Both));
              declare
                  SQL_Command    : constant DB.Types.SQL_String :=
                      DB.Types.SQL_String (To_String (Insert_SQL));
                  Results        : constant DB.Connector.Result_Set :=
                      Connection.Execute (SQL_Command);
              begin
               This.Id.Load_From (Connection, Results);
              end;
         end if;
         --  XXX FIXME: this only works for RETURNING clauses... XXX
      end;
   end Save_Insert;

   -----------------
   -- Save_Update --
   -----------------

   procedure Save_Update
     (This              : in out Model'Class;
      Connection        : in out DB.Connector.Connection)
   is
      Update_SQL        : Unbounded_String;
      Field_Count       : Natural := 1;
      Field_Values      : Unbounded_String;
      Total_Fields      : constant Positive := This.Field_Count;

      procedure Handle_Field (F : in out DB.Active_Record.Fields.Field'Class) is
         Field_SQL      : constant DB.Types.SQL_String := F.To_SQL (Connection);
      begin
         Field_Count := Field_Count + 1;
         Append (Field_Values, F.Get_Name);
         Append (Field_Values, "=");
         Append (Field_Values, String (Field_SQL));
         if Field_Count /= Total_Fields then
            Append (Field_Values, ", ");
         end if;
      end Handle_Field;
   begin
      Set_Unbounded_String (Update_SQL, "UPDATE ");
      Append (Update_SQL, This.Model_Name);
      Append (Update_SQL, " SET ");
      This.Iterate_Custom_Fields (Handle_Field'Access);
      Append (Update_SQL, Field_Values);
      Append (Update_SQL, " WHERE " & This.Get_Id_Name & "=");
      Append (Update_SQL, String (This.Id.To_SQL (Connection)));
      Append (Update_SQL, ';');

      declare
         SQL_Command    : constant DB.Types.SQL_String :=
           DB.Types.SQL_String (To_String (Update_SQL));
         Results        : constant DB.Connector.Result_Set :=
           Connection.Execute (SQL_Command);
         pragma Unreferenced (Results);
      begin
         This.Store := STORE_UPDATE;
      end;
   end Save_Update;

   ------------
   -- Set_Id --
   ------------

   procedure Set_Id
     (This		: in out Model'Class;
      Id		: in     DB.Types.Object_Id)
   is
   begin
      This.Id.Set (Id);
   end Set_Id;

   -----------------
   -- Set_Id_Name --
   -----------------

   procedure Set_Id_Name
     (This              : in out Model'Class;
      Name              : in     String)
   is
   begin
      Set_Unbounded_String (This.Id_Name, Name);
      DB.Active_Record.Fields.Model_Operations.Set_Field_Name (This.Id, Name);
   end Set_Id_Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (This              : in out Model'Class;
      Name              : in     String)
   is
      Lower_Name        : constant String := To_Lower (Name);
   begin
      if Validate_Model_Name (Lower_Name) then
         Set_Unbounded_String (This.Model_Name, Lower_Name);
      else
         raise CONSTRAINT_ERROR with "invalid model name";
      end if;
   end Set_Name;

   -------------------
   -- Set_Read_Only --
   -------------------

   procedure Set_Read_Only
     (This              : in out Model'Class;
      Value             : in     Boolean)
   is
   begin
      This.Read_Only := Value;
   end Set_Read_Only;

   --------------
   -- Validate --
   --------------

   procedure Validate
     (This              : in out Model;
      Connection        : in out DB.Connector.Connection)
   is
      pragma Unreferenced (This);
   begin
      null;
   end Validate;

   --------------------------
   -- Validate_Constraints --
   --------------------------

   procedure Validate_Constraints
     (This              : in out Model'Class)
   is
      procedure Detect_Validation_Errors
        (F : in out DB.Active_Record.Fields.Field'Class)
      is
      begin
         if F.Is_Not_Null and then not F.Is_Not_Null_Or_Default then
            F.Set_Validation_Failed (F.Get_Display_Name & " can't be empty");
         elsif F.Is_Blank and then not F.Is_Allow_Blank then
            F.Set_Validation_Failed (F.Get_Display_Name & " can't be empty");
         end if;
         if not F.Validation_Failed then
            F.Validate_Field;
         end if;
      end Detect_Validation_Errors;
   begin
      This.Iterate_Custom_Fields (Detect_Validation_Errors'Access);
   end Validate_Constraints;

   -------------------------
   -- Validate_Model_Name --
   -------------------------

   function Validate_Model_Name (This : in String) return Boolean is
   begin
      if This'Length = 0 or else This'Length > 63 then
         return False;
      end if;

      for i in This'Range loop
         case This (i) is
            when 'A'..'Z' =>
               null;
            when 'a'..'z' =>
               null;
            when '0'..'9' =>
               null;
            when '_' =>
               null;
            when others =>
               return False;
         end case;
      end loop;
      return True;
   end Validate_Model_Name;

end DB.Active_Record.Models;

