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

package body DB.Active_Record.Fields.Date_Time_Types is

   package body Date is

      ---------
      -- "=" --
      ---------

      function "="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, EQUAL, Date_Image (Right));
         return Temp;
      end "=";

      ----------
      -- "/=" --
      ----------

      function "/="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, NOT_EQUAL, Date_Image (Right));
         return Temp;
      end "/=";

      ---------
      -- "<" --
      ---------

      function "<"
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, LESS_THAN, Date_Image (Right));
         return Temp;
      end "<";

      ----------
      -- "<=" --
      ----------

      function "<="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, LESS_THAN_OR_EQUAL, Date_Image (Right));
         return Temp;
      end "<=";

      ----------
      -- ">=" --
      ----------

      function ">="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, GREATER_THAN_OR_EQUAL, Date_Image (Right));
         return Temp;
      end ">=";

      ---------
      -- ">" --
      ---------

      function ">"
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, GREATER_THAN, Date_Image (Right));
         return Temp;
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
      end Clear;

      ---------------
      -- Configure --
      ---------------

      function Configure
        (Name           : in String;
         Display_Name   : in String := "";
         Auto_Now       : in Boolean := False;
         Not_Null       : in Boolean := False;
         Unique         : in Boolean := False;
         Has_Default    : in Boolean := True;
         Default_Value  : in DB.Types.DB_Date := Null_Date) return Field
      is
         Temp           : Field;
      begin
         Config_Name (Temp, Name, Display_Name);
         Temp.Not_Null := Not_Null;
         Temp.Unique := Unique;
         Temp.Has_Default := Has_Default;
         Temp.Auto_Now := Auto_Now;
         Temp.Default_Value := Default_Value;
         return Temp;
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
         Constraints    : constant DB.Types.SQL_String := 
           Constraints_SQL (This);
         Field_Name     : constant String := To_String (This.Field_Name);
      begin
         return DB.Types.SQL_String (Field_Name & " DATE") & Constraints;
      end Field_SQL;

      ---------
      -- Get --
      ---------

      function Get (This : in Field) return Ada.Calendar.Time is
      begin
         return This.Value;
      end Get;

      function Get (This : in Field) return String is
      begin
         return Date_Image (This.Value);
      end Get;

      ---------------
      -- Load_From --
      ---------------

      procedure Load_From
        (This           : in out Field;
         Connection     : in     DB.Connector.Connection;
         Results        : in     DB.Connector.Result_Set)
      is
         Field_Name     : constant String := This.Get_Name;
      begin
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
      end Set;

      procedure Set
        (This           : in out Field;
         Value          : in     String)
      is
      begin
         This.Value := Ada.Calendar.Formatting.Value (Value & " 12:00:00");
         This.Changed := True;
         This.Is_Null := False;
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
         if This.Is_Null then
            return "NULL";
         else
            if not This.Auto_Now then
               declare
                  Value_Str      : constant String := Date_Image (This.Value);
               begin
                  return "'" & Connection.Quote_Value (Value_Str) & "'";
               end;
            else
               declare
                  Value_Str      : constant String :=
                    Date_Image (Ada.Calendar.Clock);
               begin
                  return "'" & Connection.Quote_Value (Value_Str) & "'";
               end;
            end if;
         end if;
      end To_SQL;

   end Date;


   package body Timestamp is

      ---------
      -- "=" --
      ---------

      function "="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Timestamp) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, EQUAL, Timestamp_Image (Right));
         return Temp;
      end "=";

      ----------
      -- "/=" --
      ----------

      function "/="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Timestamp) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, NOT_EQUAL, Timestamp_Image (Right));
         return Temp;
      end "/=";

      ---------
      -- "<" --
      ---------

      function "<"
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Timestamp) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, LESS_THAN, Timestamp_Image (Right));
         return Temp;
      end "<";

      ----------
      -- "<=" --
      ----------

      function "<="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Timestamp) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria
           (Temp, Left, LESS_THAN_OR_EQUAL, Timestamp_Image (Right));
         return Temp;
      end "<=";

      ----------
      -- ">=" --
      ----------

      function ">="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Timestamp) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria
           (Temp, Left, GREATER_THAN_OR_EQUAL, Timestamp_Image (Right));
         return Temp;
      end ">=";

      ---------
      -- ">" --
      ---------

      function ">"
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Timestamp) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, GREATER_THAN, Timestamp_Image (Right));
         return Temp;
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
      end Clear;

      ---------------
      -- Configure --
      ---------------

      function Configure
        (Name           : in String;
         Display_Name   : in String := "";
         Auto_Now       : in Boolean := False;
         Not_Null       : in Boolean := False;
         Unique         : in Boolean := False;
         Has_Default    : in Boolean := True;
         Default_Value  : in DB.Types.DB_Timestamp := Null_Timestamp)
        return Field
      is
         Temp           : Field;
      begin
         Config_Name (Temp, Name, Display_Name);
         Temp.Not_Null := Not_Null;
         Temp.Unique := Unique;
         Temp.Has_Default := Has_Default;
         Temp.Auto_Now := Auto_Now;
         Temp.Default_Value := Default_Value;
         return Temp;
      end Configure;

      ---------------
      -- Field_SQL --
      ---------------

      function Field_SQL
        (This           : in Field;
         Connector      : in DB.Connector.Connection)
        return DB.Types.SQL_String
      is
         Constraints    : constant DB.Types.SQL_String := 
           Constraints_SQL (This);
         Field_Name     : constant String := To_String (This.Field_Name);
      begin
         return DB.Types.SQL_String (Field_Name & " TIMESTAMP") & Constraints;
      end Field_SQL;

      ---------
      -- Get --
      ---------

      function Get (This : in Field) return Ada.Calendar.Time is
      begin
         return This.Value;
      end Get;

      function Get (This : in Field) return String is
      begin
         return Timestamp_Image (This.Value);
      end Get;

      ---------------
      -- Load_From --
      ---------------

      procedure Load_From
        (This           : in out Field;
         Connection     : in     DB.Connector.Connection;
         Results        : in     DB.Connector.Result_Set)
      is
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
      end Set;

      procedure Set
        (This           : in out Field;
         Value          : in     String)
      is
      begin
         This.Value := Ada.Calendar.Formatting.Value (Value);
         This.Changed := True;
         This.Is_Null := False;
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
         if This.Is_Null then
            return "NULL";
         else
            if not This.Auto_Now then
               declare
                  Value_Str   : constant String := Timestamp_Image (This.Value);
               begin
                  return "'" & Connection.Quote_Value (Value_Str) & "'";
               end;
            else
               declare
                  Value_Str   : constant String :=
                    Timestamp_Image (Ada.Calendar.Clock);
               begin
                  return "'" & Connection.Quote_Value (Value_Str) & "'";
               end;
            end if;
         end if;
      end To_SQL;

   end Timestamp;

end DB.Active_Record.Fields.Date_Time_Types;
