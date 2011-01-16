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
--    db-active_record-fields.ads   jvinters   16-January-2011
--

with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with DB.Connector;                     use DB.Connector;
with DB.Types;

package DB.Active_Record.Fields is

   type Field is abstract tagged private;
   type Field_Handler is not null access Procedure (This : in out Field'Class);

   procedure Clear (This : in out Field) is abstract;
   --  Clears the field - if the field has a default, sets the field to the
   --  default value, otherwise sets it to NULL.

   function Field_SQL
     (This              : in Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String is abstract;
   --  Returns the SQL to define the field.

   function Get_Display_Name (This : in Field'Class) return String;
   --  Returns the display (human-readable) name of the field.

   function Get_Model_Name (This : in Field'Class) return String;
   --  Returns the name of the field's parent model.

   function Get_Name (This : in Field'Class) return String;
   --  Returns the field name.

   function Is_Changed (This : in Field'Class) return Boolean;
   --  Returns the field change status flag.

   procedure Load_From
     (This              : in out Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set) is abstract;
   --  Loads the field value from a result set.  If the field is NULL in
   --  the result set, and the field has a defined default, then the field
   --  is set to the default value.

   function To_SQL
     (This              : in Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String is abstract;
   --  Returns the SQL representation of the current field value.

private

   type Field is abstract tagged record
      Changed           : Boolean := False;     --  Has field been modified?
      Display_Name      : Unbounded_String;     --  Human readable name
      Field_Name        : Unbounded_String;     --  Db table column name
      Has_Default       : Boolean := False;     --  Does field have a default?
      Is_Null           : Boolean := True;      --  Is field currently NULL?
      Model_Name        : Unbounded_String;     --  Parent model name
      Not_Null          : Boolean := False;     --  Is field NOT NULL?
      Primary_Key       : Boolean := False;     --  Is field PRIMARY KEY?
      Unique            : Boolean := False;     --  Is field UNIQUE?
   end record;

end DB.Active_Record.Fields;

