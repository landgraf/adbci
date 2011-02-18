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
--    db-active_record-fields-date_time_types.adb   jvinters   22-January-2011
--

with Ada.Calendar.Formatting;
with DB.Errors;

package body DB.Active_Record.Fields.Date_Time_Types is

   package body Date is

      ---------
      -- "=" --
      ---------

      function "="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, EQUAL, Date_Image (Right));
         end return;
      end "=";

      ----------
      -- "/=" --
      ----------

      function "/="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, NOT_EQUAL, Date_Image (Right));
         end return;
      end "/=";

      ---------
      -- "<" --
      ---------

      function "<"
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, LESS_THAN, Date_Image (Right));
         end return;
      end "<";

      ----------
      -- "<=" --
      ----------

      function "<="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, LESS_THAN_OR_EQUAL, Date_Image (Right));
         end return;
      end "<=";

      ----------
      -- ">=" --
      ----------

      function ">="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, GREATER_THAN_OR_EQUAL, Date_Image (Right));
         end return;
      end ">=";

      ---------
      -- ">" --
      ---------

      function ">"
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, GREATER_THAN, Date_Image (Right));
         end return;
      end ">";

      -----------
      -- Clear --
      -----------

      procedure Clear (This : in out Field) is
      begin
         This.Changed := True;
         This.Value := Null_Date;

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
        (Name           : in String;
         Display_Name   : in String := "";
         Auto_Now       : in Boolean := False;
         Auto_Now_Add   : in Boolean := False;
         Not_Null       : in Boolean := False;
         Unique         : in Boolean := False;
         Has_Default    : in Boolean := True;
         Default_Value  : in DB.Types.DB_Date := Null_Date) return Field
      is
      begin
         return Temp : Field do
            Config_Name (Temp, Name, Display_Name);
            Temp.Not_Null := Not_Null;
            Temp.Unique := Unique;
            Temp.Has_Default := Has_Default;
            Temp.Auto_Now := Auto_Now;
            Temp.Auto_Now_Add := Auto_Now_Add;
            Temp.Default_Value := Default_Value;
         end return;
      end Configure;

      ----------------
      -- Date_Image --
      ----------------

      function Date_Image (This : in DB.Types.DB_Date) return String is
         Img            : constant String :=
           Ada.Calendar.Formatting.Image (This);
      begin
         return Img (Img'First .. Img'First + 9);
      end Date_Image;

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
         return DB.Types.SQL_String (Field_Name & " DATE") & Constraints;
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
               Set_Validation_Failed (This, "Invalid Date");
            end if;
         else
            This.Is_Null := True;
            This.Value := Ada.Calendar.Formatting.Value (Value & " 12:00:00");
            This.Is_Null := False;
         end if;
      exception
         when CONSTRAINT_ERROR =>
            Set_Validation_Failed (This, "Invalid Date");
      end From_String;

      ---------
      -- Get --
      ---------

      function Get (This : in Field) return Ada.Calendar.Time is
      begin
         if This.Loaded then
            return This.Value;
         else
            raise DB.Errors.NOT_LOADED;
         end if;
      end Get;

      function Get (This : in Field) return String is
      begin
         if This.Loaded then
            return Date_Image (This.Value);
         else
            raise DB.Errors.NOT_LOADED;
         end if;
      end Get;

      ----------------------------
      -- Is_Not_Null_Or_Default --
      ----------------------------

      function Is_Not_Null_Or_Default (This : in Field) return Boolean is
      begin
         if not This.Is_Null or else This.Auto_Now or else This.Auto_Now_Add then
            return True;
         else
            return False;
         end if;
      end Is_Not_Null_Or_Default;

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
               This.Value := Null_Date;
               This.Is_Null := True;
            end if;
         else
            declare
               Str      : constant String :=
                 Results.Get_String (This.Get_Name, False) & " 12:00:00";
            begin
               This.Value := Ada.Calendar.Formatting.Value (Str);
               This.Is_Null := False;
            end;
         end if;
         This.Changed := False;
         This.Loaded := True;
      end Load_From;

      ---------
      -- Set --
      ---------

      procedure Set
        (This           : in out Field;
         Value          : in     Ada.Calendar.Time)
      is
      begin
         This.Value := Value;
         This.Changed := True;
         This.Is_Null := False;
         This.Loaded := True;
      end Set;

      procedure Set
        (This           : in out Field;
         Value          : in     String)
      is
      begin
         This.Value := Ada.Calendar.Formatting.Value (Value & " 12:00:00");
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
      begin
         if not This.Loaded then
            raise DB.Errors.NOT_LOADED;
         elsif This.Auto_Now or else
           (This.Auto_Now_Add and then This.Is_Null) then
            declare
               Value_Str         : constant String :=
                 Date_Image (Ada.Calendar.Clock);
            begin
               return Connection.Quote_Value (Value_Str);
            end;
         else
            if This.Is_Null then
               return "NULL";
            else
               declare
                  Value_Str      : constant String := Date_Image (This.Value);
               begin
                  return Connection.Quote_Value (Value_Str);
               end;
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
         else
            if This.Is_Null then
               return "";
            else
               return Date_Image (This.Value);
            end if;
         end if;
      end To_String;

   end Date;


   package body Timestamp is

      ---------
      -- "=" --
      ---------

      function "="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Timestamp) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, EQUAL, Timestamp_Image (Right));
         end return;
      end "=";

      ----------
      -- "/=" --
      ----------

      function "/="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Timestamp) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, NOT_EQUAL, Timestamp_Image (Right));
         end return;
      end "/=";

      ---------
      -- "<" --
      ---------

      function "<"
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Timestamp) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, LESS_THAN, Timestamp_Image (Right));
         end return;
      end "<";

      ----------
      -- "<=" --
      ----------

      function "<="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Timestamp) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria
              (Temp, Left, LESS_THAN_OR_EQUAL, Timestamp_Image (Right));
         end return;
      end "<=";

      ----------
      -- ">=" --
      ----------

      function ">="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Timestamp) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria
              (Temp, Left, GREATER_THAN_OR_EQUAL, Timestamp_Image (Right));
         end return;
      end ">=";

      ---------
      -- ">" --
      ---------

      function ">"
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Timestamp) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, GREATER_THAN, Timestamp_Image (Right));
         end return;
      end ">";

      -----------
      -- Clear --
      -----------

      procedure Clear (This : in out Field) is
      begin
         This.Changed := True;
         This.Value := Null_Timestamp;

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
        (Name           : in String;
         Display_Name   : in String := "";
         Auto_Now       : in Boolean := False;
         Auto_Now_Add   : in Boolean := False;
         Not_Null       : in Boolean := False;
         Unique         : in Boolean := False;
         Has_Default    : in Boolean := True;
         Default_Value  : in DB.Types.DB_Timestamp := Null_Timestamp)
        return Field
      is
      begin
         return Temp : Field do
            Config_Name (Temp, Name, Display_Name);
            Temp.Not_Null := Not_Null;
            Temp.Unique := Unique;
            Temp.Has_Default := Has_Default;
            Temp.Auto_Now := Auto_Now;
            Temp.Auto_Now_Add := Auto_Now_Add;
            Temp.Default_Value := Default_Value;
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
         return DB.Types.SQL_String (Field_Name & " TIMESTAMP") & Constraints;
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
               Set_Validation_Failed (This, "Invalid Timestamp");
            end if;
         else
            This.Is_Null := True;
            This.Value := Ada.Calendar.Formatting.Value (Value);
            This.Is_Null := False;
         end if;
      exception
         when CONSTRAINT_ERROR =>
            Set_Validation_Failed (This, "Invalid Timestamp");
      end From_String;

      ---------
      -- Get --
      ---------

      function Get (This : in Field) return Ada.Calendar.Time is
      begin
         if This.Loaded then
            return This.Value;
         else
            raise DB.Errors.NOT_LOADED;
         end if;
      end Get;

      function Get (This : in Field) return String is
      begin
         if This.Loaded then
            return Timestamp_Image (This.Value);
         else
            raise DB.Errors.NOT_LOADED;
         end if;
      end Get;

      ----------------------------
      -- Is_Not_Null_Or_Default --
      ----------------------------

      function Is_Not_Null_Or_Default (This : in Field) return Boolean is
      begin
         if not This.Is_Null or else This.Auto_Now or else This.Auto_Now_Add then
            return True;
         else
            return False;
         end if;
      end Is_Not_Null_Or_Default;

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
         if Results.Get_Is_Null (Field_Name) then
            if This.Has_Default then
               This.Value := This.Default_Value;
               This.Is_Null := False;
            else
               This.Value := Null_Timestamp;
               This.Is_Null := True;
            end if;
         else
            declare
               Str      : constant String :=
                 Results.Get_String (This.Get_Name, False);
            begin
               This.Value := Ada.Calendar.Formatting.Value (Str);
               This.Is_Null := False;
            end;
         end if;
         This.Changed := False;
         This.Loaded := True;
      end Load_From;

      ---------
      -- Set --
      ---------

      procedure Set
        (This           : in out Field;
         Value          : in     Ada.Calendar.Time)
      is
      begin
         This.Value := Value;
         This.Changed := True;
         This.Is_Null := False;
         This.Loaded := True;
      end Set;

      procedure Set
        (This           : in out Field;
         Value          : in     String)
      is
      begin
         This.Value := Ada.Calendar.Formatting.Value (Value);
         This.Changed := True;
         This.Is_Null := False;
         This.Loaded := True;
      end Set;

      ---------------------
      -- Timestamp_Image --
      ---------------------

      function Timestamp_Image (This : in DB.Types.DB_Timestamp) return String
      is
      begin
         return Ada.Calendar.Formatting.Image
           (This, Include_Time_Fraction => False);
      end Timestamp_Image;

      ------------
      -- To_SQL --
      ------------

      function To_SQL
        (This           : in Field;
         Connection     : in DB.Connector.Connection)
        return DB.Types.SQL_String
      is
      begin
         if not This.Loaded then
            raise DB.Errors.NOT_LOADED;
         elsif This.Auto_Now or else
           (This.Auto_Now_Add and then This.Is_Null) then
            declare
               Value_Str         : constant String :=
                 Timestamp_Image (Ada.Calendar.Clock);
            begin
               return Connection.Quote_Value (Value_Str);
            end;
         else
            if This.Is_Null then
               return "NULL";
            else
               declare
                  Value_Str      : constant String :=
                    Timestamp_Image (This.Value);
               begin
                  return Connection.Quote_Value (Value_Str);
               end;
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
         else
            if This.Is_Null then
               return "";
            else
               return Timestamp_Image (This.Value);
            end if;
         end if;
      end To_String;

   end Timestamp;

end DB.Active_Record.Fields.Date_Time_Types;
