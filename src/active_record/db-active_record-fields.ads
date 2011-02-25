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
with Ada.Finalization;
with DB.Connector;                     use DB.Connector;
with DB.Types;                         use DB.Types;

package DB.Active_Record.Fields is

   type Field is abstract tagged private;
   type Field_Handler is not null access Procedure (This : in out Field'Class);
   type Field_Criteria is private;
   type Order_Criteria is private;

   Null_Field_Criteria	: constant Field_Criteria;
   Null_Order_Criteria  : constant Order_Criteria;

   type Null_Value_Type is private;

   NULL_VALUE		: constant Null_Value_Type;

   type SQL_Operator is
     (EQUAL,                                    --  =
      NOT_EQUAL,                                --  <>
      LESS_THAN,                                --  <
      LESS_THAN_OR_EQUAL,                       --  <=
      GREATER_THAN,                             --  >
      GREATER_THAN_OR_EQUAL,                    --  >=
      LIKE,                                     --  LIKE
      ILIKE,                                    --  ILIKE
      SQL_AND,                                  --  AND (requires subtrees)
      SQL_OR,                                   --  OR (requires subtrees)
      IS_OPERATOR,				--  IS
      IS_NOT_OPERATOR);				--  IS NOT

   procedure Clear (This : in out Field) is abstract;
   --  Clears the field - if the field has a default, sets the field to the
   --  default value, otherwise sets it to NULL.

   procedure Clear_Validation (This : in out Field'Class);
   --  Clears validation results for field.

   function Field_SQL
     (This              : in Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String is abstract;
   --  Returns the SQL to define the field.

   procedure From_String
     (This		: in out Field;
      Value		: in     String;
      Empty_As_Default	: in     Boolean := True) is abstract;
   --  Loads field content from string.  If the string doesn't contain a valid
   --  value, then the field will be set to have failed validation.
   --  If Empty_As_Default is true, then an empty string will result in the
   --  field being set to the default value (if any), or NULL (if none).

   function Get_Display_Name (This : in Field'Class) return String;
   --  Returns the display (human-readable) name of the field.

   function Get_Full_Name (This : in Field'Class) return String;
   --  Returns the field's full name (<model_name>.<field_name>).

   function Get_Model_Name (This : in Field'Class) return String;
   --  Returns the name of the field's parent model.

   function Get_Name (This : in Field'Class) return String;
   --  Returns the field name.

   function Is_Allow_Blank (This : in Field) return Boolean;
   --  Returns true if field can be blank.

   function Is_Blank (This : in Field) return Boolean;
   --  Returns true if the field is blank

   function Is_Changed (This : in Field'Class) return Boolean;
   --  Returns the field change status flag.

   function Is_Empty (This : in Field_Criteria) return Boolean;
   --  Returns true if the criteria tree is empty.

   function Is_Foreign_Key (This : in Field) return Boolean;
   --  Returns true if the field is a foreign key field.

   function Is_Loaded (This : in Field'Class) return Boolean;
   --  Returns true if the field has been loaded/is available, false
   --  if the field has not been loaded from the database.

   function Is_Not_Null (This : in Field'Class) return Boolean;
   --  Returns true if the field has a NOT NULL constraint.

   function Is_Not_Null_Or_Default (This : in Field) return Boolean;
   --  Returns true if field is not null, or provides a default value.

   function Is_Null (This : in Field) return Boolean;
   --  Returns true if field is NULL.

   function Is_Unique (This : in Field'Class) return Boolean;
   --  Returns true if the field has a UNIQUE constraint.

   procedure Load_From
     (This              : in out Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set;
      Load_Foreign_Keys : in     Boolean := False) is abstract;
   --  Loads the field value from a result set.  If the field is NULL in
   --  the result set, and the field has a defined default, then the field
   --  is set to the default value.

   function Order_By
     (Order_Field       : in Field'Class;
      Ascending         : in Boolean := True) return Order_Criteria;

   function Order_By
     (Ordering          : in Order_Criteria;
      Order_Field       : in Field'Class;
      Ascending         : in Boolean := True) return Order_Criteria;

   procedure Pre_Save
     (This		: in out Field);
   --  Called to prepare field for saving.

   procedure Set_Loaded
     (This              : in out Field'Class;
      Value             : in     Boolean);
   --  Sets/Clears the loaded status for the field.  Normally only used
   --  by internal code.

   procedure Set_Validation_Failed
     (This              : in out Field'Class;
      Message           : in     String);
   --  Sets validation failure status.

   function To_SQL
     (This              : in Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String is abstract;
   --  Returns the SQL representation of the current field value.  The value
   --  if not NULL, is enclosed by single quotes.

   function To_String (This : in Field) return String is abstract;
   --  Converts the field value to a string.  NULLs are returned as an empty
   --  string.

   function To_String
     (This              : in Order_Criteria) return String;

   function Validation_Error
     (This              : in Field'Class) return String;
   --  Returns the validation error message.

   function Validation_Failed
     (This              : in Field'Class) return Boolean;
   --  Returns true if the field has failed validation.

   type Id_Field is new Field with private;

   function "="
     (Left              : in Id_Field'Class;
      Right             : in DB.Types.Object_Id) return Field_Criteria;

   function "="
     (Left              : in Id_Field'Class;
      Right             : in Null_Value_Type) return Field_Criteria;

   function "/="
     (Left              : in Id_Field'Class;
      Right             : in DB.Types.Object_Id) return Field_Criteria;

   function "/="
     (Left              : in Id_Field'Class;
      Right             : in Null_Value_Type) return Field_Criteria;

   function "and"
     (Left              : in Field_Criteria;
      Right             : in Field_Criteria) return Field_Criteria;

   pragma Warnings (Off);
   function "&"
     (Left              : in Field_Criteria;
      Right             : in Field_Criteria) return Field_Criteria renames "and";
   pragma Warnings (On);

   function "or"
     (Left              : in Field_Criteria;
      Right             : in Field_Criteria) return Field_Criteria;

   overriding procedure Clear (This : in out Id_Field);

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Has_Default       : in Boolean := True;
      Default_Value     : in DB.Types.Object_Id := DB.Types.Null_Object_Id)
     return Id_Field;

   overriding function Field_SQL
     (This              : in Id_Field;
      Connector         : in DB.Connector.Connection)
     return DB.Types.SQL_String;

   procedure From_String
     (This		: in out Id_Field;
      Value		: in     String;
      Empty_As_Default	: in     Boolean := True);

   function Get (This : in Id_Field) return DB.Types.Object_Id;

   overriding procedure Load_From
     (This              : in out Id_Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set;
      Load_Foreign_Keys : in     Boolean := False);

   procedure Set
     (This              : in out Id_Field;
      Value             : in     DB.Types.Object_Id);

   procedure Set_Criteria
     (This              : in out Field_Criteria;
      Source_Field      : in     Field'Class;
      Operator          : in     SQL_Operator;
      Str               : in     String;
      Requires_Quoting  : in     Boolean := False);

   procedure To_Query
     (This              : in     Field_Criteria;
      Database          : in     DB.Connector.Connection;
      Table_List        : in out Unbounded_String;
      Where_Conditions  : in out Unbounded_String);
   --  Converts criteria tree into table list and conditions ready for
   --  use in a query.

   overriding function To_SQL
     (This              : in Id_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String;

   overriding function To_String (This : in Id_Field) return String;

private

   type Null_Value_Type is null record;
   NULL_VALUE		: constant Null_Value_Type := (null record);

   type Field is abstract tagged record
      Allow_Blank       : Boolean := True;      --  Allow blank content?
      Changed           : Boolean := False;     --  Has field been modified?
      Display_Name      : Unbounded_String;     --  Human readable name
      Field_Name        : Unbounded_String;     --  Db table column name
      Has_Default       : Boolean := False;     --  Does field have a default?
      Is_Null           : Boolean := True;      --  Is field currently NULL?
      Model_Name        : Unbounded_String;     --  Parent model name
      Not_Null          : Boolean := False;     --  Is field NOT NULL?
      Primary_Key       : Boolean := False;     --  Is field PRIMARY KEY?
      Unique            : Boolean := False;     --  Is field UNIQUE?
      Validation_Failed : Boolean := False;     --  Has field failed validation?
      Validation_Error  : Unbounded_String;     --  Validation Error (if any)
      Loaded            : Boolean := True;      --  Field Loaded?
   end record;

   type Id_Field is new Field with record
      Default_Value     : DB.Types.Object_Id := DB.Types.Null_Object_Id;
      Value             : DB.Types.Object_Id := DB.Types.Null_Object_Id;
   end record;

   type Field_Criteria_Data;
   type Field_Criteria_Access is access all Field_Criteria_Data;

   type Field_Criteria_Data is record
      Reference_Count   : Natural := 1;
      Model_Name        : Unbounded_String;
      Field_Name        : Unbounded_String;
      Operator          : SQL_Operator;
      Requires_Quoting  : Boolean := False;
      SQL_Criteria      : Unbounded_String;
      Left_Subtree      : Field_Criteria;
      Right_Subtree     : Field_Criteria;
   end record;

   type Field_Criteria is new Ada.Finalization.Controlled with record
      Data              : Field_Criteria_Access := null;
   end record;

   type Order_Criteria is record
      Ordering          : Unbounded_String;
   end record;

   Null_Order_Criteria  : constant Order_Criteria :=
     (Ordering => Null_Unbounded_String);

   procedure Alloc (This : in out Field_Criteria);
   pragma Inline (Alloc);

   overriding procedure Adjust (This : in out Field_Criteria);

   overriding procedure Finalize (This : in out Field_Criteria);

   procedure Config_Name
     (This              : in out Field'Class;
      Name              : in     String;
      Display_Name      : in     String);
   --  Configures name part of fields.

   function Constraints_SQL (This : in Field'Class) return DB.Types.SQL_String;
   --  Returns field constraints as SQL string.

   function Validate_Field_Name (This : in String) return Boolean;
   --  Validates field name - returns true if valid, false if invalid.

   Null_Field_Criteria	: constant Field_Criteria :=
     (Ada.Finalization.Controlled with Data => null);

end DB.Active_Record.Fields;

