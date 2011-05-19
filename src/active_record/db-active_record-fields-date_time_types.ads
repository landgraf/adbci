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
--    db-active_record-fields-date_time_types.ads   jvinters   22-January-2011
--

with Ada.Calendar;
with DB.Connector;
with DB.Types;

package DB.Active_Record.Fields.Date_Time_Types is

   package Date is

      Null_Date         : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (1970, 1, 1, 0.0);

      type Field is new DB.Active_Record.Fields.Field with private;

      function "="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria;

      function "="
        (Left           : in Field'Class;
         Right          : in Null_Value_Type) return Field_Criteria;

      function "/="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria;

      function "/="
        (Left           : in Field'Class;
         Right          : in Null_Value_Type) return Field_Criteria;

      function "<"
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria;

      function "<="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria;

      function ">="
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria;

      function ">"
        (Left           : in Field'Class;
         Right          : in DB.Types.DB_Date) return Field_Criteria;

      overriding procedure Clear (This : in out Field);

      function Configure
        (Name           : in String;
         Display_Name   : in String := "";
         Auto_Now       : in Boolean := False;
         Auto_Now_Add   : in Boolean := False;
         Not_Null       : in Boolean := False;
         Unique         : in Boolean := False;
         Has_Default    : in Boolean := True;
         Default_Value  : in DB.Types.DB_Date := Null_Date;
         Indexed        : in Boolean := False) return Field;

      function Date_Image (This : in DB.Types.DB_Date) return String;
      --  Converts Date to ISO formatted date string.

      overriding function Field_SQL
        (This           : in Field;
         Connector      : in DB.Connector.Connection)
        return DB.Types.SQL_String;

      overriding procedure From_String
        (This             : in out Field;
         Value            : in     String;
         Empty_As_Default : in     Boolean := True);

      function Get (This : in Field) return Ada.Calendar.Time;

      function Get_String (This : in Field) return String;
      --  alternative - converts date to string before returning.

      overriding function Is_Not_Null_Or_Default
        (This		: in Field) return Boolean;

      overriding procedure Load_From
        (This              : in out Field;
         Connection        : in     DB.Connector.Connection;
         Results           : in     DB.Connector.Result_Set;
         Load_Foreign_Keys : in     Boolean := False);

      overriding procedure Pre_Save (This : in out Field);

      procedure Set
        (This           : in out Field;
         Value          : in     Ada.Calendar.Time);

      procedure Set
        (This           : in out Field;
         Value          : in     String);
      --  Accepts ISO formatted date.

      overriding function To_SQL
        (This           : in Field;
         Connection     : in DB.Connector.Connection)
        return DB.Types.SQL_String;

      overriding function To_String (This : in Field) return String;

   private

      type Field is new DB.Active_Record.Fields.Field with record
         Auto_Now          : Boolean := False;
         Auto_Now_Add      : Boolean := False;
         Default_Value     : DB.Types.DB_Date := Null_Date;
         Value             : DB.Types.DB_Date := Null_Date;
      end record;

   end Date;


   package Timestamp is

      Null_Timestamp    : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of (1970, 1, 1, 0.0);

      type Field is new DB.Active_Record.Fields.Field with private;

      function "="
        (Left              : in Field'Class;
         Right             : in DB.Types.DB_Timestamp) return Field_Criteria;

      function "="
        (Left              : in Field'Class;
         Right             : in Null_Value_Type) return Field_Criteria;

      function "/="
        (Left              : in Field'Class;
         Right             : in DB.Types.DB_Timestamp) return Field_Criteria;

      function "/="
        (Left              : in Field'Class;
         Right             : in Null_Value_Type) return Field_Criteria;

      function "<"
        (Left              : in Field'Class;
         Right             : in DB.Types.DB_Timestamp) return Field_Criteria;

      function "<="
        (Left              : in Field'Class;
         Right             : in DB.Types.DB_Timestamp) return Field_Criteria;

      function ">="
        (Left              : in Field'Class;
         Right             : in DB.Types.DB_Timestamp) return Field_Criteria;

      function ">"
        (Left              : in Field'Class;
         Right             : in DB.Types.DB_Timestamp) return Field_Criteria;

      overriding procedure Clear (This : in out Field);

      function Configure
        (Name              : in String;
         Display_Name      : in String := "";
         Auto_Now          : in Boolean := False;
         Auto_Now_Add      : in Boolean := False;
         Not_Null          : in Boolean := False;
         Unique            : in Boolean := False;
         Has_Default       : in Boolean := True;
         Default_Value     : in DB.Types.DB_Timestamp := Null_Timestamp;
         Indexed           : in Boolean := False)
        return Field;

      overriding function Field_SQL
        (This              : in Field;
         Connector         : in DB.Connector.Connection)
        return DB.Types.SQL_String;

      overriding procedure From_String
        (This             : in out Field;
         Value            : in     String;
         Empty_As_Default : in     Boolean := True);

      function Get (This : in Field) return DB.Types.DB_Timestamp;

      function Get_String (This : in Field) return String;

      overriding function Is_Not_Null_Or_Default
        (This		: in Field) return Boolean;

      overriding procedure Load_From
        (This              : in out Field;
         Connection        : in     DB.Connector.Connection;
         Results           : in     DB.Connector.Result_Set;
         Load_Foreign_Keys : in     Boolean := False);

      overriding procedure Pre_Save (This : in out Field);

      procedure Set
        (This              : in out Field;
         Value             : in     DB.Types.DB_Timestamp);

      procedure Set
        (This              : in out Field;
         Value             : in     String);
      --  Accepts ISO formatted date and time.

      function Timestamp_Image (This : in DB.Types.DB_Timestamp) return String;
      --  Converts Timestamp to ISO formatted date/time string.

      overriding function To_SQL
        (This              : in Field;
         Connection        : in DB.Connector.Connection)
        return DB.Types.SQL_String;

      overriding function To_String (This : in Field) return String;

   private

      type Field is new DB.Active_Record.Fields.Field with record
         Auto_Now          : Boolean := False;
         Auto_Now_Add      : Boolean := False;
         Default_Value     : DB.Types.DB_Timestamp := Null_Timestamp;
         Value             : DB.Types.DB_Timestamp := Null_Timestamp;
      end record;

   end Timestamp;

end DB.Active_Record.Fields.Date_Time_Types;

