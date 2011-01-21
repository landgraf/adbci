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
--    db-active_record-fields-generic_fixed.ads   jvinters   21-January-2011
--

with DB.Active_Record.Fields;

generic

   type Fixed_Type is delta <> digits <>;

package DB.Active_Record.Fields.Generic_Fixed is

   type Fixed_Field is new DB.Active_Record.Fields.Field with private;

   function "="
     (Left              : in Fixed_Field'Class;
      Right             : in Fixed_Type) return Field_Criteria;

   function "/="
     (Left              : in Fixed_Field'Class;
      Right             : in Fixed_Type) return Field_Criteria;

   function "<"
     (Left              : in Fixed_Field'Class;
      Right             : in Fixed_Type) return Field_Criteria;

   function "<="
     (Left              : in Fixed_Field'Class;
      Right             : in Fixed_Type) return Field_Criteria;

   function ">="
     (Left              : in Fixed_Field'Class;
      Right             : in Fixed_Type) return Field_Criteria;

   function ">"
     (Left              : in Fixed_Field'Class;
      Right             : in Fixed_Type) return Field_Criteria;

   overriding procedure Clear (This : in out Fixed_Field);

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Has_Default       : in Boolean := True;
      Default_Value     : in Fixed_Type := 0.0) return Fixed_Field;

   overriding function Field_SQL
     (This              : in Fixed_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String;

   function Get
     (This              : in Fixed_Field) return Fixed_Type;

   function Get
     (This              : in Fixed_Field) return String;

   overriding procedure Load_From
     (This              : in out Fixed_Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set);

   procedure Set
     (This              : in out Fixed_Field;
      Value             : in     Fixed_Type);

   procedure Set
     (This              : in out Fixed_Field;
      Value             : in     String);

   overriding function To_SQL
     (This              : in Fixed_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String;

private

   type Fixed_Field is new DB.Active_Record.Fields.Field with record
      Default_Value     : Fixed_Type := 0.0;
      Value             : Fixed_Type := 0.0;
   end record;

end DB.Active_Record.Fields.Generic_Fixed;
