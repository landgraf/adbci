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
--    db-active_record-fields-generic_fixed.ads   jvinters   21-January-2011
--

with Ada.Strings;                      use Ada.Strings;
with Ada.Strings.Fixed;                use Ada.Strings.Fixed;
with DB.Errors;

package body DB.Active_Record.Fields.Generic_Fixed is

   ---------
   -- "=" --
   ---------

   function "="
     (Left              : in Field'Class;
      Right             : in Fixed_Type) return Field_Criteria
   is
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, EQUAL, Fixed_Type'Image (Right));
      end return;
   end "=";

   function "="
     (Left              : in Field'Class;
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
     (Left              : in Field'Class;
      Right             : in Fixed_Type) return Field_Criteria
   is
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, NOT_EQUAL, Fixed_Type'Image (Right));
      end return;
   end "/=";

   function "/="
     (Left              : in Field'Class;
      Right             : in Null_Value_Type) return Field_Criteria
   is
      pragma Unreferenced (Right);
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, IS_NOT_OPERATOR, "NULL", False);
      end return;
   end "/=";

   ---------
   -- "<" --
   ---------

   function "<"
     (Left              : in Field'Class;
      Right             : in Fixed_Type) return Field_Criteria
   is
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, LESS_THAN, Fixed_Type'Image (Right));
      end return;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="
     (Left              : in Field'Class;
      Right             : in Fixed_Type) return Field_Criteria
   is
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, LESS_THAN_OR_EQUAL, Fixed_Type'Image (Right));
      end return;
   end "<=";

   ----------
   -- ">=" --
   ----------

   function ">="
     (Left              : in Field'Class;
      Right             : in Fixed_Type) return Field_Criteria
   is
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, GREATER_THAN_OR_EQUAL, Fixed_Type'Image (Right));
      end return;
   end ">=";

   ---------
   -- ">" --
   ---------

   function ">"
     (Left              : in Field'Class;
      Right             : in Fixed_Type) return Field_Criteria
   is
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, GREATER_THAN, Fixed_Type'Image (Right));
      end return;
   end ">";

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Field) is
   begin
      This.Changed := True;
      This.Value := 0.0;

      if This.Has_Default then
         This.Value := This.Default_Value;
         This.Is_Null := False;
      else
         This.Is_Null := True;
      end if;

      This.Loaded := True;
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
      Default_Value     : in Fixed_Type := Initialization_Value) return Field
   is
   begin
      return Temp : Field do
         Config_Name (Temp, Name, Display_Name);
         Temp.Not_Null := Not_Null;
         Temp.Unique := Unique;
         Temp.Has_Default := Has_Default;
         Temp.Default_Value := Default_Value;
      end return;
   end Configure;

   ---------------
   -- Field_SQL --
   ---------------

   function Field_SQL
     (This              : in Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
      pragma Unreferenced (Connection);
      Constraints       : constant DB.Types.SQL_String :=
        Constraints_SQL (This);
      Field_Name        : constant String := To_String (This.Field_Name);
      Scale             : constant String :=
        Trim (Natural'Image (Fixed_Type'Aft), Both);
      Precision         : constant String :=
        Trim (Natural'Image (Fixed_Type'Digits), Both);
   begin
      return DB.Types.SQL_String
        (Field_Name & " DECIMAL(" & Precision & ',' & Scale &')') &
        Constraints;
   end Field_SQL;

   -----------------
   -- From_String --
   -----------------

   procedure From_String
     (This             : in out Field;
      Value            : in     String;
      Empty_As_Default : in     Boolean := True)
   is
   begin
      if Value = "" then
         if Empty_As_Default and then This.Has_Default then
            This.Value := This.Default_Value;
            This.Is_Null := False;
         else
            This.Is_Null := True;
            Set_Validation_Failed (This, "Invalid Fixed Point");
         end if;
      else
         This.Is_Null := True;
         This.Value := Fixed_Type'Value (Value);
         This.Is_Null := False;
      end if;
   exception
      when CONSTRAINT_ERROR =>
         Set_Validation_Failed (This, "Invalid Fixed Point");
   end From_String;

   ---------
   -- Get --
   ---------

   function Get (This : in Field) return Fixed_Type is
   begin
      if This.Loaded then
         return This.Value;
      else
         raise DB.Errors.NOT_LOADED;
      end if;
   end Get;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (This : in Field) return String is
      Temp              : constant String :=
        Trim (Fixed_Type'Image (This.Value), Both);
   begin
      if This.Loaded then
         return Temp;
      else
         raise DB.Errors.NOT_LOADED;
      end if;
   end Get_String;

   ---------------
   -- Load_From --
   ---------------

   procedure Load_From
     (This              : in out Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set;
      Load_Foreign_Keys : in     Boolean := False)
   is
      pragma Unreferenced (Connection);
      pragma Unreferenced (Load_Foreign_Keys);
      Field_Name        : constant String := This.Get_Name;
   begin
      This.Loaded := False;
      if Results.Get_Is_Null (Field_Name) then
         if This.Has_Default then
            This.Value := This.Default_Value;
            This.Is_Null := False;
         else
            This.Value := 0.0;
            This.Is_Null := True;
         end if;
      else
         This.Value := Fixed_Type'Value
           (Results.Get_String (This.Get_Name, False));
         This.Is_Null := False;
      end if;
      This.Changed := False;
      This.Loaded := True;
   end Load_From;

   ---------
   -- Set --
   ---------

   procedure Set
     (This              : in out Field;
      Value             : in     Fixed_Type)
   is
   begin
      This.Value := Value;
      This.Changed := True;
      This.Is_Null := False;
      This.Loaded := True;
   end Set;

   procedure Set
     (This              : in out Field;
      Value             : in     String)
   is
   begin
      This.Value := Fixed_Type'Value (Value);
      This.Changed := True;
      This.Is_Null := False;
      This.Loaded := True;
   end Set;

   ------------
   -- To_SQL --
   ------------

   function To_SQL
     (This              : in Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
   begin
      if not This.Loaded then
         raise DB.Errors.NOT_LOADED;
      elsif This.Is_Null then
         return "NULL";
      else
         declare
            Value_Str         : constant String :=
              Trim (Fixed_Type'Image (This.Value), Both);
         begin
            return Connection.Quote_Value (Value_Str);
         end;
      end if;
   end To_SQL;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : in Field) return String is
   begin
      if This.Is_Null then
         return "";
      else
         return Get_String (This);
      end if;
   end To_String;

end DB.Active_Record.Fields.Generic_Fixed;
