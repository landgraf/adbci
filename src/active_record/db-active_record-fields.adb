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

package body DB.Active_Record.Fields is

   pragma Inline (Set_Criteria);
   procedure Set_Criteria
     (This              : in out Field_Criteria;
      Source_Field      : in     Field'Class;
      Operator          : in     SQL_Operator;
      Str               : in     String;
      Requires_Quoting  : in     Boolean := False)
   is
   begin
      Alloc (This);
      This.Data.Model_Name := Source_Field.Model_Name;
      This.Data.Field_Name := Source_Field.Field_Name;
      This.Data.Operator := Operator;
      Set_Unbounded_String (This.Data.SQL_Criteria, Trim (Str, Both));
      This.Data.Requires_Quoting := Requires_Quoting;
   end Set_Criteria;

   ---------
   -- "=" --
   ---------

   function "="
     (Left              : in Bigint_Field'Class;
      Right             : in DB.Types.DB_Bigint) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, EQUAL, DB.Types.DB_Bigint'Image (Right));
      return Temp;
   end "=";

   function "="
     (Left              : in Boolean_Field'Class;
      Right             : in Boolean) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      if Right then
         Set_Criteria (Temp, Left, EQUAL, "true");
      else
         Set_Criteria (Temp, Left, EQUAL, "false");
      end if;
      return Temp;
   end "=";

   function "="
     (Left              : in Id_Field'Class;
      Right             : in DB.Types.Object_Id) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, EQUAL, DB.Types.Object_Id'Image (Right));
      return Temp;
   end "=";

   function "="
     (Left              : in Integer_Field'Class;
      Right             : in DB.Types.DB_Integer) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, EQUAL, DB.Types.DB_Integer'Image (Right));
      return Temp;
   end "=";

   function "="
     (Left              : in Integer_Field'Class;
      Right             : in String) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, EQUAL, Right, True);
      return Temp;
   end "=";

   ----------
   -- "/=" --
   ----------

   function "/="
     (Left              : in Bigint_Field'Class;
      Right             : in DB.Types.DB_Bigint) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, NOT_EQUAL, DB.Types.DB_Bigint'Image (Right));
      return Temp;
   end "/=";

   function "/="
     (Left              : in Boolean_Field'Class;
      Right             : in Boolean) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      if not Right then
         Set_Criteria (Temp, Left, NOT_EQUAL, "true");
      else
         Set_Criteria (Temp, Left, NOT_EQUAL, "false");
      end if;
      return Temp;
   end "/=";

   function "/="
     (Left              : in Id_Field'Class;
      Right             : in DB.Types.Object_Id) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, NOT_EQUAL, DB.Types.Object_Id'Image (Right));
      return Temp;
   end "/=";

   function "/="
     (Left              : in Integer_Field'Class;
      Right             : in DB.Types.DB_Integer) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, NOT_EQUAL, DB.Types.DB_Integer'Image (Right));
      return Temp;
   end "/=";

   function "/="
     (Left              : in String_Field'Class;
      Right             : in String) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, NOT_EQUAL, Right, True);
      return Temp;
   end "/=";

   ---------
   -- "<" --
   ---------

   function "<"
     (Left              : in Bigint_Field'Class;
      Right             : in DB.Types.DB_Bigint) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, LESS_THAN, DB.Types.DB_Bigint'Image (Right));
      return Temp;
   end "<";

   function "<"
     (Left              : in Integer_Field'Class;
      Right             : in DB.Types.DB_Integer) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, LESS_THAN, DB.Types.DB_Integer'Image (Right));
      return Temp;
   end "<";

   function "<"
     (Left              : in String_Field'Class;
      Right             : in String) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, LESS_THAN, Right, True);
      return Temp;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="
     (Left              : in Bigint_Field'Class;
      Right             : in DB.Types.DB_Bigint) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, LESS_THAN_OR_EQUAL, DB.Types.DB_Bigint'Image (Right));
      return Temp;
   end "<=";

   function "<="
     (Left              : in Integer_Field'Class;
      Right             : in DB.Types.DB_Integer) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, LESS_THAN_OR_EQUAL, DB.Types.DB_Integer'Image (Right));
      return Temp;
   end "<=";

   function "<="
     (Left              : in String_Field'Class;
      Right             : in String) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, LESS_THAN_OR_EQUAL, Right, True);
      return Temp;
   end "<=";

   ----------
   -- ">=" --
   ----------

   function ">="
     (Left              : in Bigint_Field'Class;
      Right             : in DB.Types.DB_Bigint) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, GREATER_THAN_OR_EQUAL, DB.Types.DB_Bigint'Image (Right));
      return Temp;
   end ">=";

   function ">="
     (Left              : in Integer_Field'Class;
      Right             : in DB.Types.DB_Integer) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, GREATER_THAN_OR_EQUAL, DB.Types.DB_Integer'Image (Right));
      return Temp;
   end ">=";

   function ">="
     (Left              : in String_Field'Class;
      Right             : in String) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, GREATER_THAN_OR_EQUAL, Right, True);
      return Temp;
   end ">=";

   ---------
   -- ">" --
   ---------

   function ">"
     (Left              : in Bigint_Field'Class;
      Right             : in DB.Types.DB_Bigint) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, GREATER_THAN, DB.Types.DB_Bigint'Image (Right));
      return Temp;
   end ">";

   function ">"
     (Left              : in Integer_Field'Class;
      Right             : in DB.Types.DB_Integer) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, GREATER_THAN, DB.Types.DB_Integer'Image (Right));
      return Temp;
   end ">";

   function ">"
     (Left              : in String_Field'Class;
      Right             : in String) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, GREATER_THAN, Right, True);
      return Temp;
   end ">";

   -----------
   -- "and" --
   -----------

   function "and"
     (Left              : in Field_Criteria;
      Right             : in Field_Criteria) return Field_Criteria
   is
      New_Tree          : Field_Criteria;
   begin
      Alloc (New_Tree);
      New_Tree.Data.Operator := SQL_AND;
      New_Tree.Data.Left_Subtree := Left;
      New_Tree.Data.Right_Subtree := Right;
      return New_Tree;
   end "and";

   ----------
   -- "or" --
   ----------

   function "or"
     (Left              : in Field_Criteria;
      Right             : in Field_Criteria) return Field_Criteria
   is
      New_Tree          : Field_Criteria;
   begin
      Alloc (New_Tree);
      New_Tree.Data.Operator := SQL_OR;
      New_Tree.Data.Left_Subtree := Left;
      New_Tree.Data.Right_Subtree := Right;
      return New_Tree;
   end "or";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out Field_Criteria) is
   begin
      if This.Data /= null then
         This.Data.Reference_Count := This.Data.Reference_Count + 1;
      end if;
   end Adjust;

   -----------
   -- Alloc --
   -----------

   procedure Alloc (This : in out Field_Criteria) is
   begin
      if This.Data = null then
         This.Data := new Field_Criteria_Data;
         This.Data.Reference_Count := 1;
      end if;
   end Alloc;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Bigint_Field) is
   begin
      This.Changed := True;
      This.Value := 0;

      if This.Has_Default then
         This.Value := This.Default_Value;
         This.Is_Null := False;
      else
         This.Is_Null := True;
      end if;
   end Clear;

   procedure Clear (This : in out Boolean_Field) is
   begin
      This.Changed := True;
      This.Value := False;

      if This.Has_Default then
         This.Value := This.Default_Value;
         This.Is_Null := False;
      else
         This.Is_Null := True;
      end if;
   end Clear;

   procedure Clear (This : in out Id_Field) is
   begin
      This.Changed := True;
      This.Value := 0;

      if This.Has_Default then
         This.Value := This.Default_Value;
         This.Is_Null := False;
      else
         This.Is_Null := True;
      end if;
   end Clear;

   procedure Clear (This : in out Integer_Field) is
   begin
      This.Changed := True;
      This.Value := 0;

      if This.Has_Default then
         This.Value := This.Default_Value;
         This.Is_Null := False;
      else
         This.Is_Null := True;
      end if;
   end Clear;

   procedure Clear (This : in out String_Field) is
   begin
      This.Changed := True;
      Set_Unbounded_String (This.Value, "");

      if This.Has_Default then
         This.Value := This.Default_Value;
         This.Is_Null := False;
      else
         This.Is_Null := True;
      end if;
   end Clear;

   ---------------
   -- Configure --
   ---------------

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Has_Default       : in Boolean := True;
      Default_Value     : in DB.Types.DB_Bigint := 0) return Bigint_Field
   is
      Lower_Name        : constant String := To_Lower (Name);
      Temp              : Bigint_Field;
   begin
      if not Validate_Field_Name (Lower_Name) then
         raise CONSTRAINT_ERROR with "invalid field name";
      else
         Set_Unbounded_String (Temp.Field_Name, Lower_Name);
         if Display_Name /= "" then
            Set_Unbounded_String (Temp.Display_Name, Display_Name);
         else
            Set_Unbounded_String (Temp.Display_Name, Lower_Name);
         end if;

         Temp.Not_Null := Not_Null;
         Temp.Unique := Unique;
         Temp.Has_Default := Has_Default;
         Temp.Default_Value := Default_Value;
         return Temp;
      end if;
   end Configure;

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Has_Default       : in Boolean := True;
      Default_Value     : in Boolean := False) return Boolean_Field
   is
      Lower_Name        : constant String := To_Lower (Name);
      Temp              : Boolean_Field;
   begin
      if not Validate_Field_Name (Lower_Name) then
         raise CONSTRAINT_ERROR with "invalid field name";
      else
         Set_Unbounded_String (Temp.Field_Name, Lower_Name);
         if Display_Name /= "" then
            Set_Unbounded_String (Temp.Display_Name, Display_Name);
         else
            Set_Unbounded_String (Temp.Display_Name, Lower_Name);
         end if;

         Temp.Not_Null := Not_Null;
         Temp.Unique := Unique;
         Temp.Has_Default := Has_Default;
         Temp.Default_Value := Default_Value;
         return Temp;
      end if;
   end Configure;

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Has_Default       : in Boolean := True;
      Default_Value     : in DB.Types.Object_Id := 0) return Id_Field
   is
      Lower_Name        : constant String := To_Lower (Name);
      Temp              : Id_Field;
   begin
      if not Validate_Field_Name (Lower_Name) then
         raise CONSTRAINT_ERROR with "invalid field name";
      else
         Set_Unbounded_String (Temp.Field_Name, Lower_Name);
         if Display_Name /= "" then
            Set_Unbounded_String (Temp.Display_Name, Display_Name);
         else
            Set_Unbounded_String (Temp.Display_Name, Lower_Name);
         end if;

         if Lower_Name = "id" then
            Temp.Primary_Key := True;     --  special case
         end if;

         Temp.Not_Null := Not_Null;
         Temp.Unique := Unique;
         Temp.Has_Default := Has_Default;
         Temp.Default_Value := Default_Value;
         return Temp;
      end if;
   end Configure;

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Has_Default       : in Boolean := True;
      Default_Value     : in DB.Types.DB_Integer := 0) return Integer_Field
   is
      Lower_Name        : constant String := To_Lower (Name);
      Temp              : Integer_Field;
   begin
      if not Validate_Field_Name (Lower_Name) then
         raise CONSTRAINT_ERROR with "invalid field name";
      else
         Set_Unbounded_String (Temp.Field_Name, Lower_Name);
         if Display_Name /= "" then
            Set_Unbounded_String (Temp.Display_Name, Display_Name);
         else
            Set_Unbounded_String (Temp.Display_Name, Lower_Name);
         end if;

         Temp.Not_Null := Not_Null;
         Temp.Unique := Unique;
         Temp.Has_Default := Has_Default;
         Temp.Default_Value := Default_Value;
         return Temp;
      end if;
   end Configure;

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Maximum_Length    : in Positive := 255;
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Has_Default       : in Boolean := True;
      Default_Value     : in String := "") return String_Field
   is
      Lower_Name        : constant String := To_Lower (Name);
      Temp              : String_Field;
   begin
      if not Validate_Field_Name (Lower_Name) then
         raise CONSTRAINT_ERROR with "invalid field name";
      else
         Set_Unbounded_String (Temp.Field_Name, Lower_Name);
         if Display_Name /= "" then
            Set_Unbounded_String (Temp.Display_Name, Display_Name);
         else
            Set_Unbounded_String (Temp.Display_Name, Lower_Name);
         end if;

         Temp.Not_Null := Not_Null;
         Temp.Unique := Unique;
         Temp.Has_Default := Has_Default;
         Temp.Maximum_Length := Maximum_Length;
         Set_Unbounded_String (Temp.Default_Value, Default_Value);
         return Temp;
      end if;
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
     (This              : in Bigint_Field;
      Connector         : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
      Constraints       : constant DB.Types.SQL_String := 
        Constraints_SQL (This);
      Field_Name        : constant String := To_String (This.Field_Name);
   begin
      return DB.Types.SQL_String (Field_Name & " BIGINT") & Constraints;
   end Field_SQL;

   function Field_SQL
     (This              : in Boolean_Field;
      Connector         : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
      Constraints       : constant DB.Types.SQL_String := 
        Constraints_SQL (This);
      Field_Name        : constant String := To_String (This.Field_Name);
   begin
      return DB.Types.SQL_String (Field_Name & " BOOLEAN") & Constraints;
   end Field_SQL;

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

   function Field_SQL
     (This              : in Integer_Field;
      Connector         : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
      Constraints       : constant DB.Types.SQL_String := 
        Constraints_SQL (This);
      Field_Name        : constant String := To_String (This.Field_Name);
   begin
      return DB.Types.SQL_String (Field_Name & " INTEGER") & Constraints;
   end Field_SQL;

   function Field_SQL
     (This              : in String_Field;
      Connector         : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
      Constraints       : constant DB.Types.SQL_String := 
        Constraints_SQL (This);
      Field_Name        : constant String := To_String (This.Field_Name);
      Length_Str        : constant DB.Types.SQL_String :=
        DB.Types.SQL_String (Trim (Positive'Image (This.Maximum_Length), Both));
   begin
      return DB.Types.SQL_String (Field_Name & " VARCHAR(") &
        Length_Str & ")" & Constraints;
   end Field_SQL;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Field_Criteria) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Field_Criteria_Data, Field_Criteria_Access);
   begin
      if This.Data /= null then
         This.Data.Reference_Count := This.Data.Reference_Count - 1;
         if This.Data.Reference_Count = 0 then
            Unchecked_Free (This.Data);
         end if;
      end if;
   end Finalize;

   ---------
   -- Get --
   ---------

   function Get (This : in Bigint_Field) return DB.Types.DB_Bigint is
   begin
      return This.Value;
   end Get;

   function Get (This : in Boolean_Field) return Boolean is
   begin
      return This.Value;
   end Get;

   function Get (This : in Id_Field) return DB.Types.Object_Id is
   begin
      return This.Value;
   end Get;

   function Get (This : in Integer_Field) return DB.Types.DB_Integer is
   begin
      return This.Value;
   end Get;

   function Get (This : in String_Field) return String is
   begin
      return To_String (This.Value);
   end Get;

   function Get (This : in String_Field) return Unbounded_String is
   begin
      return This.Value;
   end Get;

   ----------------------
   -- Get_Display_Name --
   ----------------------

   function Get_Display_Name (This : in Field'Class) return String is
   begin
      return To_String (This.Display_Name);
   end Get_Display_Name;

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

   -----------
   -- ILike --
   -----------

   function ILike
     (Left              : in String_Field'Class;
      Right             : in String) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, ILIKE, Right, True);
      return Temp;
   end ILike;

   ----------------
   -- Is_Changed --
   ----------------

   function Is_Changed (This : in Field'Class) return Boolean is
   begin
      return This.Changed;
   end Is_Changed;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (This : in Field'Class) return Boolean is
   begin
      return This.Is_Null;
   end Is_Null;

   ----------
   -- Like --
   ----------

   function Like
     (Left              : in String_Field'Class;
      Right             : in String) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, LIKE, Right, True);
      return Temp;
   end Like;

   ---------------
   -- Load_From --
   ---------------

   procedure Load_From
     (This              : in out Bigint_Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set)
   is
      Field_Name        : constant String := This.Get_Name;
   begin
      if Results.Get_Is_Null (Field_Name) then
         if This.Has_Default then
            This.Value := This.Default_Value;
            This.Is_Null := False;
         else
            This.Value := 0;
            This.Is_Null := True;
         end if;
      else
         This.Value := Results.Get_Bigint (This.Get_Name, False);
         This.Is_Null := False;
      end if;
      This.Changed := False;
   end Load_From;

   procedure Load_From
     (This              : in out Boolean_Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set)
   is
      Field_Name        : constant String := This.Get_Name;
   begin
      if Results.Get_Is_Null (Field_Name) then
         if This.Has_Default then
            This.Value := This.Default_Value;
            This.Is_Null := False;
         else
            This.Value := False;
            This.Is_Null := True;
         end if;
      else
         This.Value := Results.Get_Boolean (This.Get_Name, False);
         This.Is_Null := False;
      end if;
      This.Changed := False;
   end Load_From;

   procedure Load_From
     (This              : in out Id_Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set)
   is
      Field_Name        : constant String := This.Get_Name;
   begin
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
   end Load_From;

   procedure Load_From
     (This              : in out Integer_Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set)
   is
      Field_Name        : constant String := This.Get_Name;
   begin
      if Results.Get_Is_Null (Field_Name) then
         if This.Has_Default then
            This.Value := This.Default_Value;
            This.Is_Null := False;
         else
            This.Value := 0;
            This.Is_Null := True;
         end if;
      else
         This.Value := Results.Get_Integer (This.Get_Name, False);
         This.Is_Null := False;
      end if;
      This.Changed := False;
   end Load_From;

   procedure Load_From
     (This              : in out String_Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set)
   is
      Field_Name        : constant String := This.Get_Name;
   begin
      if Results.Get_Is_Null (Field_Name) then
         if This.Has_Default then
            This.Value := This.Default_Value;
            This.Is_Null := False;
         else
            Set_Unbounded_String (This.Value, "");
            This.Is_Null := True;
         end if;
      else
         Set_Unbounded_String
           (This.Value, Results.Get_String (This.Get_Name, False));
         This.Is_Null := False;
      end if;
      This.Changed := False;
   end Load_From;

   ---------
   -- Set --
   ---------

   procedure Set
     (This              : in out Bigint_Field;
      Value             : in     DB.Types.DB_Bigint)
   is
   begin
      This.Value := Value;
      This.Changed := True;
      This.Is_Null := False;
   end Set;

   procedure Set
     (This              : in out Boolean_Field;
      Value             : in     Boolean)
   is
   begin
      This.Value := Value;
      This.Changed := True;
      This.Is_Null := False;
   end Set;

   procedure Set
     (This              : in out Id_Field;
      Value             : in     DB.Types.Object_Id)
   is
   begin
      This.Value := Value;
      This.Changed := True;
      This.Is_Null := False;
   end Set;

   procedure Set
     (This              : in out Integer_Field;
      Value             : in     DB.Types.DB_Integer)
   is
   begin
      This.Value := Value;
      This.Changed := True;
      This.Is_Null := False;
   end Set;

   procedure Set
     (This              : in out String_Field;
      Value             : in     String)
   is
   begin
      if Value'Length > This.Maximum_Length then
         raise CONSTRAINT_ERROR with "string too long";
      else
         Set_Unbounded_String (This.Value, Value);
         This.Changed := True;
         This.Is_Null := False;
      end if;
   end Set;

   procedure Set
     (This              : in out String_Field;
      Value             : in     Unbounded_String)
   is
   begin
      if Length (Value) > This.Maximum_Length then
         raise CONSTRAINT_ERROR with "string too long";
      else
         This.Value := Value;
         This.Changed := True;
         This.Is_Null := False;
      end if;
   end Set;

   ------------------------
   -- To_Extracted_Query --
   ------------------------

   function To_Extracted_Query
     (This              : in Field_Criteria;
      Database          : in DB.Connector.Connection) return String
   is
      Value             : Unbounded_String;
   begin
      Value := This.Data.Model_Name;
      Append (Value, '.');
      Append (Value, This.Data.Field_Name);

      case This.Data.Operator is
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
            Append (Value, " ILIKE ");
         when LIKE =>
            Append (Value, " LIKE ");
         when others =>
            raise PROGRAM_ERROR with "incorrect usage";
      end case;

      if This.Data.Requires_Quoting then
         Append (Value, "'" &
           String (Database.Quote_Value (To_String (This.Data.SQL_Criteria)))
           & "'");
      else
         Append (Value, This.Data.SQL_Criteria);
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
         case Root.Data.Operator is
            when SQL_AND =>
               return "(" & Traverse (Root.Data.Left_Subtree, Database) & 
                 ") AND (" & Traverse (Root.Data.Right_Subtree, Database) & ")";
            when SQL_OR =>
               return "(" & Traverse (Root.Data.Left_Subtree, Database) & 
                 ") OR (" & Traverse (Root.Data.Right_Subtree, Database) & ")";
            when others =>
               Included_Tables.Include (Root.Data.Model_Name);
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
     (This              : in Bigint_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
   begin
      if This.Is_Null then
         return "NULL";
      else
         declare
            Value_Str         : constant String :=
              Trim (DB.Types.DB_Bigint'Image (This.Value), Both);
         begin
            return Connection.Quote_Value (Value_Str);
         end;
      end if;
   end To_SQL;

   function To_SQL
     (This              : in Boolean_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
   begin
      if This.Is_Null then
         return "NULL";
      else
         if This.Value then
            return "'true'";
         else
            return "'false'";
         end if;
      end if;
   end To_SQL;

   function To_SQL
     (This              : in Id_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
   begin
      if This.Is_Null then
         return "NULL";
      else
         declare
            Value_Str         : constant String :=
              Trim (DB.Types.Object_Id'Image (This.Value), Both);
         begin
            return "'" & Connection.Quote_Value (Value_Str) & "'";
         end;
      end if;
   end To_SQL;

   function To_SQL
     (This              : in Integer_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
   begin
      if This.Is_Null then
         return "NULL";
      else
         declare
            Value_Str         : constant String :=
              Trim (DB.Types.DB_Integer'Image (This.Value), Both);
         begin
            return Connection.Quote_Value (Value_Str);
         end;
      end if;
   end To_SQL;

   function To_SQL
     (This              : in String_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
   begin
      if This.Is_Null then
         return "NULL";
      else
         return "'" & Connection.Quote_Value (To_String (This.Value)) & "'";
      end if;
   end To_SQL;

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

