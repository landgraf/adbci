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
--    db-active_record-fields-boolean_type.adb   jvinters   22-January-2011
--

with Ada.Characters.Handling;		use Ada.Characters.Handling;
with DB.Errors;

package body DB.Active_Record.Fields.Boolean_Type is

   package Body Bool is

      ---------
      -- "=" --
      ---------

      function "="
        (Left           : in Field'Class;
         Right          : in Boolean) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            if Right then
               Set_Criteria (Temp, Left, EQUAL, "true");
            else
               Set_Criteria (Temp, Left, EQUAL, "false");
            end if;
         end return;
      end "=";

      function "="
        (Left           : in Field'Class;
         Right          : in Null_Value_Type) return Field_Criteria
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
        (Left           : in Field'Class;
         Right          : in Boolean) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            if Right then
               Set_Criteria (Temp, Left, NOT_EQUAL, "true");
            else
               Set_Criteria (Temp, Left, NOT_EQUAL, "false");
            end if;
         end return;
      end "/=";

      function "/="
        (Left           : in Field'Class;
         Right          : in Null_Value_Type) return Field_Criteria
      is
         pragma Unreferenced (Right);
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, IS_NOT_OPERATOR, "NULL");
         end return;
      end "/=";

      -----------
      -- Clear --
      -----------

      procedure Clear (This : in out Field) is
      begin
         This.Changed := True;
         This.Loaded := True;
         This.Value := False;

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
        (Name           : in String;
         Display_Name   : in String := "";
         Not_Null       : in Boolean := False;
         Has_Default    : in Boolean := True;
         Default_Value  : in Boolean := False;
         Indexed        : in Boolean := False) return Field
      is
      begin
         return Temp : Field do
            Config_Name (Temp, Name, Display_Name);
            Temp.Not_Null := Not_Null;
            Temp.Unique := False;
            Temp.Has_Default := Has_Default;
            Temp.Default_Value := Default_Value;
            Temp.Indexed := Indexed;
         end return;
      end Configure;

      ---------------
      -- Field_SQL --
      ---------------

      function Field_SQL
        (This           : in Field;
         Connector      : in DB.Connector.Connection)
        return DB.Types.SQL_String
      is
         pragma Unreferenced (Connector);
         Constraints    : constant DB.Types.SQL_String :=
           Constraints_SQL (This);
         Field_Name     : constant String := To_String (This.Field_Name);
      begin
         return DB.Types.SQL_String (Field_Name & " BOOLEAN") & Constraints;
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
               Set_Validation_Failed (This, "Invalid Boolean");
            end if;
         else
            declare
               Up_Value	: constant String := To_Lower (Value);
            begin
               This.Is_Null := True;
               This.Changed := True;
               if Up_Value = "yes"
                 or else Up_Value = "y"
                 or else Up_Value = "true"
                 or else Up_Value = "t"
                 or else Up_Value = "1"
                 or else Up_Value = "on"
                 or else Up_Value = "set"
               then
                  This.Value := True;
                  This.Is_Null := False;
               elsif Up_Value = "no"
                 or else Up_Value = "n"
                 or else Up_Value = "false"
                 or else Up_Value = "f"
                 or else Up_Value = "0"
                 or else Up_Value = ""
                 or else Up_Value = "unset"
               then
                  This.Value := False;
                  This.Is_Null := False;
               else
                  Set_Validation_Failed (This, "Invalid Boolean");
               end if;
            end;
         end if;
      end From_String;

      ---------
      -- Get --
      ---------

      function Get (This : in Field) return Boolean is
      begin
         if This.Loaded then
            return This.Value;
         else
            raise DB.Errors.NOT_LOADED;
         end if;
      end Get;

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
         Field_Name     : constant String := This.Get_Name;
      begin
         This.Loaded := False;
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
         This.Loaded := True;
      end Load_From;

      ---------
      -- Set --
      ---------

      procedure Set
        (This           : in out Field;
         Value          : in     Boolean)
      is
      begin
         This.Value := Value;
         This.Changed := True;
         This.Is_Null := False;
         This.Loaded := True;
      end Set;

      ------------
      -- To_SQL --
      ------------

      function To_SQL
        (This           : in Field;
         Connection     : in DB.Connector.Connection)
        return DB.Types.SQL_String
      is
         pragma Unreferenced (Connection);
      begin
         if not This.Loaded then
            raise DB.Errors.NOT_LOADED;
         elsif This.Is_Null then
            return "NULL";
         else
            if This.Value then
               return "'true'";
            else
               return "'false'";
            end if;
         end if;
      end To_SQL;

      ---------------
      -- To_String --
      ---------------

      function To_String (This : in Field) return String is
      begin
         if not This.Loaded then
            raise DB.Errors.NOT_LOADED;
         elsif This.Is_Null then
            return "";
         else
            if This.Value then
               return "true";
            else
               return "false";
            end if;
         end if;
      end To_String;

   end Bool;

end DB.Active_Record.Fields.Boolean_Type;
