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
with DB.Types;                         use DB.Types;

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

   function Is_Null (This : in Field'Class) return Boolean;
   --  Returns true if field is NULL.

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
   --  Returns the SQL representation of the current field value.  The value
   --  if not NULL, is enclosed by single quotes.

   type Id_Field is new Field with private;
   type Integer_Field is new Field with private;

   overriding procedure Clear (This : in out Id_Field);

   overriding procedure Clear (This : in out Integer_Field);

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Has_Default       : in Boolean := True;
      Default_Value     : in DB.Types.Object_Id := 0) return Id_Field;

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Has_Default       : in Boolean := True;
      Default_Value     : in DB.Types.DB_Integer := 0) return Integer_Field;

   overriding function Field_SQL
     (This              : in Id_Field;
      Connector         : in DB.Connector.Connection)
     return DB.Types.SQL_String;

   overriding function Field_SQL
     (This              : in Integer_Field;
      Connector         : in DB.Connector.Connection)
     return DB.Types.SQL_String;

   function Get (This : in Id_Field) return DB.Types.Object_Id;

   function Get (This : in Integer_Field) return DB.Types.DB_Integer;

   overriding procedure Load_From
     (This              : in out Id_Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set);

   overriding procedure Load_From
     (This              : in out Integer_Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set);

   procedure Set
     (This              : in out Id_Field;
      Value             : in     DB.Types.Object_Id);

   procedure Set
     (This              : in out Integer_Field;
      Value             : in     DB.Types.DB_Integer);

   overriding function To_SQL
     (This              : in Id_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String;

   overriding function To_SQL
     (This              : in Integer_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String;

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

   type Id_Field is new Field with record
      Default_Value     : DB.Types.Object_Id;
      Value             : DB.Types.Object_Id;
   end record;

   type Integer_Field is new Field with record
      Default_Value     : DB.Types.DB_Integer;
      Value             : DB.Types.DB_Integer;
   end record;

   function Constraints_SQL (This : in Field'Class) return DB.Types.SQL_String;
   --  Returns field constraints as SQL string.

   function Validate_Field_Name (This : in String) return Boolean;
   --  Validates field name - returns true if valid, false if invalid.

end DB.Active_Record.Fields;

