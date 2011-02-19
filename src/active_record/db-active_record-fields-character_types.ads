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
--    db-active_record-fields-character_types.ads   jvinters   16-January-2011
--

with DB.Connector;
with DB.Types;

package DB.Active_Record.Fields.Character_Types is

   package Text is

      type Field is new DB.Active_Record.Fields.Field with private;

      function "="
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      function "="
        (Left              : in Field'Class;
         Right             : in Null_Value_Type) return Field_Criteria;

      function "/="
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      function "/="
        (Left              : in Field'Class;
         Right             : in Null_Value_Type) return Field_Criteria;

      function "<"
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      function "<="
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      function ">="
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      function ">"
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      procedure Append
        (This              : in out Field;
         Str               : in     String);

      overriding procedure Clear (This : in out Field);

      function Configure
        (Name              : in String;
         Display_Name      : in String := "";
         Maximum_Length    : in Positive := 255;
         Not_Null          : in Boolean := False;
         Unique            : in Boolean := False;
         Allow_Blank       : in Boolean := True;
         Has_Default       : in Boolean := True;
         Default_Value     : in String := "") return Field;

      overriding function Field_SQL
        (This              : in Field;
         Connector         : in DB.Connector.Connection)
        return DB.Types.SQL_String;

      procedure From_String
        (This              : in out Field;
         Value             : in     String;
         Empty_As_Default  : in     Boolean := True);

      function Get (This : in Field) return String;

      function Get (This : in Field) return Unbounded_String;

      function ILike
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      overriding function Is_Blank (This : in Field) return Boolean;

      function Like
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      overriding procedure Load_From
        (This              : in out Field;
         Connection        : in     DB.Connector.Connection;
         Results           : in     DB.Connector.Result_Set;
         Load_Foreign_Keys : in     Boolean := False);

      procedure Set
        (This              : in out Field;
         Value             : in     String);

      procedure Set
        (This              : in out Field;
         Value             : in     Unbounded_String);

      overriding function To_SQL
        (This              : in Field;
         Connection        : in DB.Connector.Connection)
        return DB.Types.SQL_String;

      overriding function To_String (This : in Field) return String;

   private

      type Field is new DB.Active_Record.Fields.Field with record
         Default_Value     : Unbounded_String;
         Maximum_Length    : Positive := 255;
         Value             : Unbounded_String;
      end record;

   end Text;

   package Varchar is

      type Field is new DB.Active_Record.Fields.Field with private;

      function "="
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      function "="
        (Left              : in Field'Class;
         Right             : in Null_Value_Type) return Field_Criteria;

      function "/="
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      function "/="
        (Left              : in Field'Class;
         Right             : in Null_Value_Type) return Field_Criteria;

      function "<"
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      function "<="
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      function ">="
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      function ">"
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      procedure Append
        (This              : in out Field;
         Str               : in     String);

      overriding procedure Clear (This : in out Field);

      function Configure
        (Name              : in String;
         Display_Name      : in String := "";
         Maximum_Length    : in Positive := 255;
         Not_Null          : in Boolean := False;
         Unique            : in Boolean := False;
         Allow_Blank       : in Boolean := True;
         Has_Default       : in Boolean := True;
         Default_Value     : in String := "") return Field;

      overriding function Field_SQL
        (This              : in Field;
         Connector         : in DB.Connector.Connection)
        return DB.Types.SQL_String;

      overriding procedure From_String
        (This              : in out Field;
         Value             : in     String;
         Empty_As_Default  : in     Boolean := True);

      function Get (This : in Field) return String;

      function Get (This : in Field) return Unbounded_String;

      function ILike
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      overriding function Is_Blank (This : in Field) return Boolean;

      function Like
        (Left              : in Field'Class;
         Right             : in String) return Field_Criteria;

      overriding procedure Load_From
        (This              : in out Field;
         Connection        : in     DB.Connector.Connection;
         Results           : in     DB.Connector.Result_Set;
         Load_Foreign_Keys : in     Boolean := False);

      procedure Set
        (This              : in out Field;
         Value             : in     String);

      procedure Set
        (This              : in out Field;
         Value             : in     Unbounded_String);

      overriding function To_SQL
        (This              : in Field;
         Connection        : in DB.Connector.Connection)
        return DB.Types.SQL_String;

      overriding function To_String (This : in Field) return String;

   private

      type Field is new DB.Active_Record.Fields.Field with record
         Default_Value     : Unbounded_String;
         Maximum_Length    : Positive := 255;
         Value             : Unbounded_String;
      end record;

   end Varchar;

end DB.Active_Record.Fields.Character_Types;
