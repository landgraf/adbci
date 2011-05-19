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
--    db-active_record-fields-foreign_keys.ads   jvinters   17-January-2011
--

with DB.Active_Record.Models;
with DB.Connector;

generic

   type Model_Type is new DB.Active_Record.Models.Model with private;

package DB.Active_Record.Fields.Foreign_Keys is

   type Options is private;

   type Field is new DB.Active_Record.Fields.Field with record
      FK                : Model_Type;
      FK_Options        : Options;
   end record;

   function "="
     (Left              : in Field;
      Right             : in DB.Types.Object_Id) return Field_Criteria;

   function "="
     (Left              : in Field;
      Right             : in Model_Type) return Field_Criteria;

   function "="
     (Left              : in Field;
      Right             : in Null_Value_Type) return Field_Criteria;

   function "/="
     (Left              : in Field;
      Right             : in DB.Types.Object_Id) return Field_Criteria;

   function "/="
     (Left              : in Field;
      Right             : in Model_Type) return Field_Criteria;

   function "/="
     (Left              : in Field;
      Right             : in Null_Value_Type) return Field_Criteria;

   overriding procedure Clear (This : in out Field);

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Cascade_Delete    : in Boolean := False;
      Indexed           : in Boolean := False) return Field;

   overriding function Field_SQL
     (This              : in Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String;

   overriding procedure From_String
     (This              : in out Field;
      Value             : in     String;
      Empty_As_Default  : in     Boolean := True);

   function Get (This : in Field) return Model_Type;

   function Get_Id (This : in Field) return DB.Types.Object_Id;

   function Get_String (This : in Field) return String;

   overriding function Is_Foreign_Key
     (This              : in Field) return Boolean;

   overriding procedure Load_From
     (This              : in out Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set;
      Load_Foreign_Keys : in     Boolean := False);

   procedure Load_Now
     (This              : in out Field;
      Connection        : in     DB.Connector.Connection;
      Recurse           : in     Boolean := False);
   --  Forces load of foreign key object (and optionally, all objects
   --  below).

   procedure Set
     (This              : in out Field;
      Value             : in     Model_Type);

   procedure Set_Id
     (This		: in out Field;
      Value		: in     DB.Types.Object_Id);

   overriding function To_SQL
     (This              : in Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String;

   overriding function To_String (This : in Field) return String;

private

   type Options is record
      Cascade_Delete    : Boolean := False;
      Results           : DB.Connector.Result_Set :=
                            DB.Connector.Null_Result_Set;
   end record;

end DB.Active_Record.Fields.Foreign_Keys;

