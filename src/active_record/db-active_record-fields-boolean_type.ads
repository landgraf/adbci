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
--    db-active_record-fields-boolean_type.ads   jvinters   22-January-2011
--

with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with Ada.Finalization;
with DB.Connector;                     use DB.Connector;

package DB.Active_Record.Fields.Boolean_Type is

   type Boolean_Field is new Field with private;

   function "="
     (Left              : in Boolean_Field'Class;
      Right             : in Boolean) return Field_Criteria;

   function "/="
     (Left              : in Boolean_Field'Class;
      Right             : in Boolean) return Field_Criteria;

   overriding procedure Clear (This : in out Boolean_Field);

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Has_Default       : in Boolean := True;
      Default_Value     : in Boolean := False) return Boolean_Field;

   overriding function Field_SQL
     (This              : in Boolean_Field;
      Connector         : in DB.Connector.Connection)
     return DB.Types.SQL_String;

   function Get (This : in Boolean_Field) return Boolean;

   overriding procedure Load_From
     (This              : in out Boolean_Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set);

   procedure Set
     (This              : in out Boolean_Field;
      Value             : in     Boolean);

   overriding function To_SQL
     (This              : in Boolean_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String;

private

   type Boolean_Field is new Field with record
      Default_Value     : Boolean := False;
      Value             : Boolean := False;
   end record;

end DB.Active_Record.Fields.Boolean_Type;

