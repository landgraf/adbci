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
with Ada.Strings;                      use Ada.Strings;
with Ada.Strings.Fixed;                use Ada.Strings.Fixed;

package body DB.Active_Record.Fields is

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

   ---------
   -- Get --
   ---------

   function Get (This : in Bigint_Field) return DB.Types.DB_Bigint is
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
            This.Is_Null := True;
         end if;
      else
         This.Value := Results.Get_Bigint (This.Get_Name, False);
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
            This.Is_Null := True;
         end if;
      else
         This.Value := Results.Get_Integer (This.Get_Name, False);
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

