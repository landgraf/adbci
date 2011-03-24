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
--    db-active_record-fields.adb   jvinters   16-January-2011
--

with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Ada.Containers.Hashed_Sets;
with Ada.Strings;                      use Ada.Strings;
with Ada.Strings.Fixed;                use Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;
with DB.Errors;

package body DB.Active_Record.Fields is

   -----------
   -- Alloc --
   -----------

   procedure Alloc (This : in out Field_Criteria) is
   begin
      if This.Data = null then
         This.Data := new Field_Criteria_Data;
         This.Data.all.Reference_Count := 1;
      end if;
   end Alloc;

   ---------
   -- "=" --
   ---------

   function "="
     (Left              : in Id_Field'Class;
      Right             : in DB.Types.Object_Id) return Field_Criteria
   is
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, EQUAL, DB.Types.Object_Id'Image (Right));
      end return;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Left              : in Id_Field'Class;
      Right             : in Null_Value_Type) return Field_Criteria
   is
      pragma Unreferenced (Right);
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, IS_OPERATOR, "NULL", False);
      end return;
   end "=";

   ----------
   -- "/=" --
   ----------

   function "/="
     (Left              : in Id_Field'Class;
      Right             : in DB.Types.Object_Id) return Field_Criteria
   is
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, NOT_EQUAL, DB.Types.Object_Id'Image (Right));
      end return;
   end "/=";

   ----------
   -- "/=" --
   ----------

   function "/="
     (Left              : in Id_Field'Class;
      Right             : in Null_Value_Type) return Field_Criteria
   is
      pragma Unreferenced (Right);
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, IS_NOT_OPERATOR, "NULL", False);
      end return;
   end "/=";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left		: in Order_Criteria;
      Right		: in Order_Criteria) return Order_Criteria
   is
      Temp		: Order_Criteria;
   begin
      Set_Unbounded_String (Temp.Ordering, To_String (Left.Ordering));
      Append (Temp.Ordering, ", ");
      Append (Temp.Ordering, To_String (Right.Ordering));
      return Temp;
   end "&";

   -----------
   -- "and" --
   -----------

   function "and"
     (Left              : in Field_Criteria;
      Right             : in Field_Criteria) return Field_Criteria
   is
   begin
      return New_Tree : Field_Criteria do
         Alloc (New_Tree);
         New_Tree.Data.all.Operator := SQL_AND;
         New_Tree.Data.all.Left_Subtree := Left;
         New_Tree.Data.all.Right_Subtree := Right;
      end return;
   end "and";

   ----------
   -- "or" --
   ----------

   function "or"
     (Left              : in Field_Criteria;
      Right             : in Field_Criteria) return Field_Criteria
   is
   begin
      return New_Tree : Field_Criteria do
         Alloc (New_Tree);
         New_Tree.Data.all.Operator := SQL_OR;
         New_Tree.Data.all.Left_Subtree := Left;
         New_Tree.Data.all.Right_Subtree := Right;
      end return;
   end "or";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out Field_Criteria) is
   begin
      if This.Data /= null then
         This.Data.all.Reference_Count := This.Data.all.Reference_Count + 1;
      end if;
   end Adjust;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Id_Field) is
   begin
      This.Changed := True;
      This.Loaded := True;
      This.Value := 0;

      if This.Has_Default then
         This.Value := This.Default_Value;
         This.Is_Null := False;
      else
         This.Is_Null := True;
      end if;
   end Clear;

   ----------------------
   -- Clear_Validation --
   ----------------------

   procedure Clear_Validation (This : in out Field'Class) is
   begin
      This.Validation_Failed := False;
      Set_Unbounded_String (This.Validation_Error, "");
   end Clear_Validation;

   -----------------
   -- Config_Name --
   -----------------

   procedure Config_Name
     (This              : in out Field'Class;
      Name              : in     String;
      Display_Name      : in     String)
   is
      Lower_Name        : constant String := To_Lower (Name);
   begin
      if not Validate_Field_Name (Lower_Name) then
         raise CONSTRAINT_ERROR with "invalid field name";
      else
         Set_Unbounded_String (This.Field_Name, Lower_Name);
         if Display_Name /= "" then
            Set_Unbounded_String (This.Display_Name, Display_Name);
         else
            Set_Unbounded_String (This.Display_Name, Lower_Name);
         end if;
      end if;
   end Config_Name;

   ---------------
   -- Configure --
   ---------------

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Has_Default       : in Boolean := True;
      Default_Value     : in DB.Types.Object_Id := DB.Types.Null_Object_Id)
     return Id_Field
   is
   begin
      return Temp : Id_Field do
         Config_Name (Temp, Name, Display_Name);
         Temp.Not_Null := Not_Null;
         Temp.Unique := Unique;
         Temp.Has_Default := Has_Default;
         Temp.Default_Value := Default_Value;
      end return;
   end Configure;

   ---------------------
   -- Constraints_SQL --
   ---------------------

   function Constraints_SQL (This : in Field'Class) return DB.Types.SQL_String
   is
      Temp              : Unbounded_String;
   begin
      if This.Not_Null then
         Append (Temp, " NOT NULL");
      end if;
      if This.Unique then
         Append (Temp, " UNIQUE");
      end if;
      if This.Primary_Key then
         Append (Temp, " PRIMARY KEY");
      end if;
      return DB.Types.SQL_String (To_String (Temp));
   end Constraints_SQL;

   ---------------
   -- Field_SQL --
   ---------------

   function Field_SQL
     (This              : in Id_Field;
      Connector         : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
      Constraints       : constant DB.Types.SQL_String :=
        Constraints_SQL (This);
      Field_Name        : constant String := To_String (This.Field_Name);
   begin
      return DB.Types.SQL_String (Field_Name & ' ') &
        Connector.Get_Driver.Get_Id_SQL & Constraints;
   end Field_SQL;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Field_Criteria) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Field_Criteria_Data, Field_Criteria_Access);
   begin
      if This.Data /= null then
         This.Data.all.Reference_Count := This.Data.all.Reference_Count - 1;
         if This.Data.all.Reference_Count = 0 then
            Unchecked_Free (This.Data);
         end if;
      end if;
   end Finalize;

   -----------------
   -- From_String --
   -----------------

   procedure From_String
     (This		: in out Id_Field;
      Value		: in     String;
      Empty_As_Default	: in     Boolean := True)
   is
   begin
      if Value = "" then
         if Empty_As_Default and then This.Has_Default then
            This.Value := This.Default_Value;
            This.Is_Null := True;
         else
            Set_Validation_Failed (This, "Invalid Object id");
            This.Is_Null := True;
         end if;
      else
         This.Is_Null := True;
         This.Value := DB.Types.Object_Id'Value (Value);
         This.Is_Null := False;
      end if;
   exception
      when CONSTRAINT_ERROR =>
         Set_Validation_Failed (This, "Invalid Object id");
   end From_String;

   ---------
   -- Get --
   ---------

   function Get (This : in Id_Field) return DB.Types.Object_Id is
   begin
      if This.Loaded then
         return This.Value;
      else
         raise DB.Errors.NOT_LOADED;
      end if;
   end Get;

   ----------------------
   -- Get_Display_Name --
   ----------------------

   function Get_Display_Name (This : in Field'Class) return String is
   begin
      return To_String (This.Display_Name);
   end Get_Display_Name;

   -------------------
   -- Get_Full_Name --
   -------------------

   function Get_Full_Name (This : in Field'Class) return String is
   begin
      return To_String (This.Model_Name) & '.' & To_String (This.Field_Name);
   end Get_Full_Name;

   --------------------
   -- Get_Model_Name --
   --------------------

   function Get_Model_Name (This : in Field'Class) return String is
   begin
      return To_String (This.Model_Name);
   end Get_Model_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (This : in Field'Class) return String is
   begin
      return To_String (This.Field_Name);
   end Get_Name;

   --------------------
   -- Is_Allow_Blank --
   --------------------

   function Is_Allow_Blank (This : in Field) return Boolean is
   begin
      return This.Allow_Blank;
   end Is_Allow_Blank;

   --------------
   -- Is_Blank --
   --------------

   function Is_Blank (This : in Field) return Boolean is
   begin
      return This.Is_Null;
   end Is_Blank;

   ----------------
   -- Is_Changed --
   ----------------

   function Is_Changed (This : in Field'Class) return Boolean is
   begin
      return This.Changed;
   end Is_Changed;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : in Field_Criteria) return Boolean is
   begin
      return This.Data = null;
   end Is_Empty;

   --------------------
   -- Is_Foreign_Key --
   --------------------

   function Is_Foreign_Key (This : in Field) return Boolean is
   begin
      --  This should be overridden for foreign key fields!
      pragma Unreferenced (This);
      return False;
   end Is_Foreign_Key;

   ---------------
   -- Is_Loaded --
   ---------------

   function Is_Loaded (This : in Field'Class) return Boolean is
   begin
      return This.Loaded;
   end Is_Loaded;

   -----------------
   -- Is_Not_Null --
   -----------------

   function Is_Not_Null (This : in Field'Class) return Boolean is
   begin
      return This.Not_Null;
   end Is_Not_Null;

   ----------------------------
   -- Is_Not_Null_Or_Default --
   ----------------------------

   function Is_Not_Null_Or_Default (This : in Field) return Boolean is
   begin
      return not This.Is_Null;
   end Is_Not_Null_Or_Default;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (This : in Field) return Boolean is
   begin
      if This.Loaded then
         return This.Is_Null;
      else
         raise DB.Errors.NOT_LOADED;
      end if;
   end Is_Null;

   ---------------
   -- Is_Unique --
   ---------------

   function Is_Unique (This : in Field'Class) return Boolean is
   begin
      return This.Unique;
   end Is_Unique;

   ---------------
   -- Load_From --
   ---------------

   procedure Load_From
     (This              : in out Id_Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set;
      Load_Foreign_Keys : in     Boolean := False)
   is
      pragma Unreferenced (Connection);
      pragma Unreferenced (Load_Foreign_Keys);
      Field_Name        : constant String := This.Get_Name;
   begin
      This.Loaded := False;
      This.Validation_Failed := False;
      if Results.Get_Is_Null (Field_Name) then
         if This.Has_Default then
            This.Value := This.Default_Value;
            This.Is_Null := False;
         else
            This.Value := 0;
            This.Is_Null := True;
         end if;
      else
         This.Value := Results.Get_Object_Id (This.Get_Name, False);
         This.Is_Null := False;
      end if;
      This.Changed := False;
      This.Loaded := True;
   end Load_From;

   --------------
   -- Order_By --
   --------------

   function Order_By
     (Order_Field       : in Field'Class;
      Ascending         : in Boolean := True) return Order_Criteria
   is
   begin
      if Ascending then
         return (
            Ordering => To_Unbounded_String (Order_Field.Get_Full_Name)
         );
      else
         return (
            Ordering => To_Unbounded_String
                          (Order_Field.Get_Full_Name & " DESC")
         );
      end if;
   end Order_By;

   function Order_By
     (Ordering          : in Order_Criteria;
      Order_Field       : in Field'Class;
      Ascending         : in Boolean := True) return Order_Criteria
   is
   begin
      if Ascending then
         if Length (Ordering.Ordering) > 0 then
            return (Ordering => Ordering.Ordering &
                                To_Unbounded_String
                                  (", " & Order_Field.Get_Full_Name));
         else
            return (Ordering => To_Unbounded_String (Order_Field.Get_Full_Name));
         end if;
      else
         if Length (Ordering.Ordering) > 0 then
            return (Ordering => Ordering.Ordering &
                                To_Unbounded_String
                                  (", " & Order_Field.Get_Full_Name & " DESC"));
         else
            return (Ordering => To_Unbounded_String
                                  (Order_Field.Get_Full_Name & " DESC"));
         end if;
      end if;
   end Order_By;

   --------------
   -- Pre_Save --
   --------------

   procedure Pre_Save (This : in out Field) is
      pragma Unreferenced (This);
   begin
      null;
   end Pre_Save;

   ---------
   -- Set --
   ---------

   procedure Set
     (This              : in out Id_Field;
      Value             : in     DB.Types.Object_Id)
   is
   begin
      This.Value := Value;
      This.Changed := True;
      This.Is_Null := False;
      This.Loaded := True;
   end Set;

   ------------------
   -- Set_Criteria --
   ------------------

   procedure Set_Criteria
     (This              : in out Field_Criteria;
      Source_Field      : in     Field'Class;
      Operator          : in     SQL_Operator;
      Str               : in     String;
      Requires_Quoting  : in     Boolean := False)
   is
   begin
      Alloc (This);
      This.Data.all.Model_Name := Source_Field.Model_Name;
      This.Data.all.Field_Name := Source_Field.Field_Name;
      This.Data.all.Operator := Operator;
      Set_Unbounded_String (This.Data.all.SQL_Criteria, Trim (Str, Both));
      This.Data.all.Requires_Quoting := Requires_Quoting;
   end Set_Criteria;

   ----------------
   -- Set_Loaded --
   ----------------

   procedure Set_Loaded
     (This              : in out Field'Class;
      Value             : in     Boolean)
   is
   begin
      This.Loaded := Value;
   end Set_Loaded;

   ---------------------------
   -- Set_Validation_Failed --
   ---------------------------

   procedure Set_Validation_Failed
     (This              : in out Field'Class;
      Message           : in     String)
   is
   begin
      This.Validation_Failed := True;
      Set_Unbounded_String (This.Validation_Error, Message);
   end Set_Validation_Failed;

   ------------------------
   -- To_Extracted_Query --
   ------------------------

   function To_Extracted_Query
     (This              : in Field_Criteria;
      Database          : in DB.Connector.Connection) return String
   is
      Value             : Unbounded_String;
      Conn_Driver       : constant DB.Driver.Driver_Handle :=
          Database.Get_Driver;
      Driver_Caps       : constant DB.Driver.Driver_Capabilities :=
          Conn_Driver.all.Get_Capabilities;
   begin
      Value := This.Data.all.Model_Name;
      Append (Value, '.');
      Append (Value, This.Data.all.Field_Name);

      case This.Data.all.Operator is
          when EQUAL =>
              Append (Value, " = ");
          when NOT_EQUAL =>
              Append (Value, " <> ");
          when LESS_THAN =>
              Append (Value, " < ");
          when LESS_THAN_OR_EQUAL =>
              Append (Value, " <= ");
          when GREATER_THAN =>
              Append (Value, " > ");
          when GREATER_THAN_OR_EQUAL =>
              Append (Value, " >= ");
          when ILIKE =>
              if Driver_Caps.Has_Ilike then
                  Append (Value, " ILIKE ");
              else
                  Append (Value, " LIKE ");
              end if;
          when LIKE =>
            Append (Value, " LIKE ");
         when IS_OPERATOR =>
            Append (Value, " IS ");
         when IS_NOT_OPERATOR =>
            Append (Value, " IS NOT ");
         when others =>
            raise PROGRAM_ERROR with "incorrect usage";
      end case;

      if This.Data.all.Requires_Quoting then
         Append
          (Value,
           String (Database.Quote_Value (To_String (This.Data.all.SQL_Criteria))));
      else
         Append (Value, This.Data.all.SQL_Criteria);
      end if;

      return To_String (Value);
   end To_Extracted_Query;

   package Table_Set is new Ada.Containers.Hashed_Sets
     (Element_Type         => Unbounded_String,
      Hash                 => Ada.Strings.Unbounded.Hash,
      Equivalent_Elements  => "=",
      "="                  => "=");

   --------------
   -- To_Query --
   --------------

   procedure To_Query
     (This              : in     Field_Criteria;
      Database          : in     DB.Connector.Connection;
      Table_List        : in out Unbounded_String;
      Where_Conditions  : in out Unbounded_String)
   is
      use Table_Set;
      Table_Cursor      : Table_Set.Cursor;
      Included_Tables   : Table_Set.Set;

      function Traverse
        (Root           : in     Field_Criteria;
         Database       : in     DB.Connector.Connection) return String
      is
      begin
         case Root.Data.all.Operator is
            when SQL_AND =>
               return "(" & Traverse (Root.Data.all.Left_Subtree, Database) &
                 ") AND (" & Traverse (Root.Data.all.Right_Subtree, Database) & ")";
            when SQL_OR =>
               return "(" & Traverse (Root.Data.all.Left_Subtree, Database) &
                 ") OR (" & Traverse (Root.Data.all.Right_Subtree, Database) & ")";
            when others =>
               Included_Tables.Include (Root.Data.all.Model_Name);
               return To_Extracted_Query (Root, Database);
         end case;
      end Traverse;
   begin
      Set_Unbounded_String (Where_Conditions, Traverse (This, Database));
      Set_Unbounded_String (Table_List, "");

      Table_Cursor := Included_Tables.First;
      while Table_Cursor /= No_Element loop
         if Length (Table_List) > 0 then
            Append (Table_List, ", ");
         end if;
         Append (Table_List, Element (Table_Cursor));
         Table_Cursor := Next (Table_Cursor);
      end loop;
   end To_Query;

   ------------
   -- To_SQL --
   ------------

   function To_SQL
     (This              : in Id_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
   begin
      if not This.Loaded then
         raise DB.Errors.NOT_LOADED;
      end if;

      if This.Is_Null then
         return "NULL";
      else
         declare
            Value_Str         : constant String :=
              Trim (DB.Types.Object_Id'Image (This.Value), Both);
         begin
            return Connection.Quote_Value (Value_Str);
         end;
      end if;
   end To_SQL;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : in Id_Field) return String is
   begin
      if not This.Loaded then
         raise DB.Errors.NOT_LOADED;
      end if;

      if This.Is_Null then
         return "";
      else
         return Trim (DB.Types.Object_Id'Image (This.Value), Both);
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (This              : in Order_Criteria) return String
   is
   begin
      return To_String (This.Ordering);
   end To_String;

   ----------------------
   -- Validation_Error --
   ----------------------

   function Validation_Error
     (This              : in Field'Class) return String
   is
   begin
      if This.Validation_Failed then
         return To_String (This.Validation_Error);
      else
         return "";
      end if;
   end Validation_Error;

   -----------------------
   -- Validation_Failed --
   -----------------------

   function Validation_Failed
     (This              : in Field'Class) return Boolean
   is
   begin
      return This.Validation_Failed;
   end Validation_Failed;

   -------------------------
   -- Validate_Field_Name --
   -------------------------

   function Validate_Field_Name (This : in String) return Boolean is
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
   end Validate_Field_Name;

end DB.Active_Record.Fields;
