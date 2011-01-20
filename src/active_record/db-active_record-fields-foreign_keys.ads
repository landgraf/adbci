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

   type Foreign_Key_Field is new DB.Active_Record.Fields.Field with record
      FK                : Model_Type;
      FK_Options        : Options;
   end record;

   function "="
     (Left              : in Foreign_Key_Field;
      Right             : in DB.Types.Object_Id) return Field_Criteria;

   function "="
     (Left              : in Foreign_Key_Field;
      Right             : in Model_Type) return Field_Criteria;

   function "/="
     (Left              : in Foreign_Key_Field;
      Right             : in DB.Types.Object_Id) return Field_Criteria;

   function "/="
     (Left              : in Foreign_Key_Field;
      Right             : in Model_Type) return Field_Criteria;

   overriding procedure Clear (This : in out Foreign_Key_Field);

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Cascade_Delete    : in Boolean := False) return Foreign_Key_Field;

   overriding function Field_SQL
     (This              : in Foreign_Key_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String;

   function Get (This : in Foreign_Key_Field) return Model_Type;

   overriding procedure Load_From
     (This              : in out Foreign_Key_Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set);

   procedure Set
     (This              : in out Foreign_Key_Field;
      Value             : in     Model_Type);

   overriding function To_SQL
     (This              : in Foreign_Key_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String;

private

   type Options is record
      Cascade_Delete    : Boolean := False;
   end record;

end DB.Active_Record.Fields.Foreign_Keys;

