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
--    db-active_record-models.ads   jvinters   17-January-2011
--

with Ada.Finalization;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with DB.Active_Record.Fields;
with DB.Connector;
with DB.Types;                         use DB.Types;

package DB.Active_Record.Models is

   type Model is abstract new Ada.Finalization.Controlled with private;
   type Model_Access is access all Model'Class;

   type Store_Method is
     (STORE_INSERT,     --  store using an SQL INSERT (a new item)
      STORE_UPDATE);    --  store using an SQL UPDATE (an existing item)

   function "="
     (Left              : in Model;
      Right             : in Model)
     return DB.Active_Record.Fields.Field_Criteria;

   function "="
     (Left              : in Model;
      Right             : in DB.Types.Object_Id)
     return DB.Active_Record.Fields.Field_Criteria;

   procedure Clean
     (This              : in out Model;
      Connection        : in out DB.Connector.Connection);
   --  Cleans fields before validation and save.

   procedure Clear (This : in out Model'Class);
   --  Clears model fields.

   procedure Clear_Id (This : in out Model'Class);
   --  Clears the id field, and forces the next save to be an insert
   --  using a new id.

   procedure Create
     (This              : in out Model'Class;
      Connection        : in out DB.Connector.Connection);
   --  Creates model table in database.

   procedure Delete
     (This              : in out Model;
      Connection        : in out DB.Connector.Connection);
   --  Deletes the object from the database.

   procedure Drop
     (This              : in out Model'Class;
      Connection        : in out DB.Connector.Connection);
   --  Drops model table in database.

   procedure Get
     (This              : in out Model'Class;
      Connection        : in     DB.Connector.Connection;
      Id                : in     DB.Types.Object_Id;
      For_Update        : in     Boolean := False;
      Load_Foreign_Keys : in     Boolean := True);
   --  Fetches object with specified id from the database.  If For_Update is
   --  true, then the object is fetched for immediate update (you shouldn't
   --  use this unless you are going to _immediately_ update the object).
   --  OBJECT_NOT_FOUND will be raised if the object can't be located.
   --  If Load_Foreign_Keys is True, then any foreign keys will be loaded as
   --  well.

   function Get_Id (This : in Model'Class) return DB.Types.Object_Id;
   --  Gets Model Id (or 0 if none).

   function Get_Id_Field (This : in Model'Class)
                          return DB.Active_Record.Fields.Id_Field;
   --  Returns a copy of the id field (for ordering etc).

   function Get_Id_Name (This : in Model'Class) return String;
   --  Returns the name of the id field.

   function Get_Id_String (This : in Model'Class) return String;
   --  Gets Model Id as string (or "0" if none).

   function Get_Name (This : in Model'Class) return String;
   --  Returns the model name.

   function Get_Read_Only (This : in Model'Class) return Boolean;
   --  Returns the read-only status of the model.

   function Get_Store_Method (This : in Model'Class) return Store_Method;
   --  Returns the storage method.

   procedure Initialize_Model (This : in out Model);
   --  Initializes the model.

   function Is_Changed (This : in Model'Class) return Boolean;
   --  Returns true if the model has changed.

   procedure Iterate_Custom_Fields
     (This              : in out Model;
      Handler           : not null access procedure
        (Field : in out DB.Active_Record.Fields.Field'Class)) is abstract;
   --  Iterates custom fields (i.e. anything added in a descendent of Model).

   procedure Iterate_Fields
     (This              : in out Model'Class;
      Handler           : not null access procedure
        (Field : in out DB.Active_Record.Fields.Field'Class));
   --  Iterates all fields (i.e. custom fields and the id field).

   procedure Load_From
     (This              : in out Model'Class;
      Connection        : in     DB.Connector.Connection;
      Result            : in out DB.Connector.Result_Set;
      Tuple             : in     DB.Tuple_Index);
   --  Loads the model from a particular result tuple.

   procedure On_After_Initialize
     (This              : in out Model);
   --  Called after model instance is initialized, and after Clear.

   procedure On_After_Load
     (This              : in out Model;
      Connection        : in     DB.Connector.Connection);
   --  Called after model instance is successfully loaded from database.

   procedure On_After_Save
     (This              : in out Model;
      Connection        : in out DB.Connector.Connection);
   --  Called after model instance is saved (only called if save didn't raise
   --  any exceptions).

   procedure On_Before_Delete
     (This              : in out Model;
      Connection        : in out DB.Connector.Connection);
   --  Called before model instance is deleted.

   procedure On_Before_Save
     (This              : in out Model;
      Connection        : in out DB.Connector.Connection);
   --  Called before model instance is saved.

   procedure Reload
     (This              : in out Model'Class;
      Connection        : in     DB.Connector.Connection;
      For_Update        : in     Boolean := False;
      Load_Foreign_Keys : in     Boolean := True);
   --  Reloads the model.

   procedure Save
     (This              : in out Model'Class;
      Connection        : in out DB.Connector.Connection;
      Force_Save        : in     Boolean := False);
   --  Saves the model to the database, using either an INSERT or an
   --  UPDATE.  If Force_Save is true, then the save will be done regardless
   --  of whether there appears to be any changes in the model.

   procedure Set_Id
     (This		: in out Model'Class;
      Id		: in     DB.Types.Object_Id);
   --  Sets object id - for internal use only!

   procedure Set_Id_Name
     (This              : in out Model'Class;
      Name              : in     String);
   --  Sets the name of the Id field (normally 'id' by default) -- this should
   --  be called ONLY from Initialize_Model.

   procedure Set_Name
     (This              : in out Model'Class;
      Name              : in     String);
   --  Sets the model name - not normally required, unless you wish to override
   --  the default.

   procedure Set_Read_Only
     (This              : in out Model'Class;
      Value             : in     Boolean);
   --  Sets the model read-only status.

   procedure Validate
     (This              : in out Model;
      Connection        : in out DB.Connector.Connection);
   --  Validates the model - should be overridden if validation is required.

private

   type Model is abstract new Ada.Finalization.Controlled with record
      Changed           : Boolean := False;
      Field_Count       : Natural := 0;
      Id                : DB.Active_Record.Fields.Id_Field :=
        DB.Active_Record.Fields.Configure
          ("id", Not_Null => True, Unique => True, Has_Default => False);
      Id_Name           : Unbounded_String;
      Initialized       : Boolean := False;
      Model_Name        : Unbounded_String;
      Read_Only         : Boolean := False;
      Store             : Store_Method := STORE_INSERT;
   end record;

   function Create_SQL
     (This              : not null access Model'Class;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String;
   --  Returns the SQL to create the model table.

   function Drop_SQL
     (This              : not null access Model'Class;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String;
   --  Returns the SQL to drop the model table.

   overriding procedure Initialize (This : in out Model);
   --  Initializes the model.

   procedure Pre_Save (This : in out Model'Class);
   --  Called when saving model -- checks to see if fields have changed.

   procedure Prepare_Fields (This : in out Model'Class);
   --  Prepares fields after Initialize_Model called.  Counts the number of
   --  fields, and clears each one (which sets up any default values etc).

   procedure Save_Insert
     (This              : in out Model'Class;
      Connection        : in out DB.Connector.Connection);
   --  Saves the model using an SQL INSERT.

   procedure Save_Update
     (This              : in out Model'Class;
      Connection        : in out DB.Connector.Connection);
   --  Saves the model using an SQL UPDATE.

   procedure Validate_Constraints (This : in out Model'Class);
   --  Validates field constraints.

   function Validate_Model_Name (This : in String) return Boolean;
   --  Validates a model name -- return true if OK, false if invalid.

end DB.Active_Record.Models;

