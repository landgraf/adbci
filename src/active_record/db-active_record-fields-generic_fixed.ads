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
   Initialization_Value : Fixed_Type := 0.0;

package DB.Active_Record.Fields.Generic_Fixed is

   type Field is new DB.Active_Record.Fields.Field with private;

   function "="
     (Left              : in Field'Class;
      Right             : in Fixed_Type) return Field_Criteria;

   function "="
     (Left              : in Field'Class;
      Right             : in Null_Value_Type) return Field_Criteria;

   function "/="
     (Left              : in Field'Class;
      Right             : in Fixed_Type) return Field_Criteria;

   function "/="
     (Left              : in Field'Class;
      Right             : in Null_Value_Type) return Field_Criteria;

   function "<"
     (Left              : in Field'Class;
      Right             : in Fixed_Type) return Field_Criteria;

   function "<="
     (Left              : in Field'Class;
      Right             : in Fixed_Type) return Field_Criteria;

   function ">="
     (Left              : in Field'Class;
      Right             : in Fixed_Type) return Field_Criteria;

   function ">"
     (Left              : in Field'Class;
      Right             : in Fixed_Type) return Field_Criteria;

   overriding procedure Clear (This : in out Field);

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Has_Default       : in Boolean := True;
      Default_Value     : in Fixed_Type := Initialization_Value) return Field;

   overriding function Field_SQL
     (This              : in Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String;

   overriding procedure From_String
     (This		: in out Field;
      Value		: in     String;
      Empty_As_Default	: in     Boolean := True);

   function Get
     (This              : in Field) return Fixed_Type;

   function Get_String
     (This              : in Field) return String;

   overriding procedure Load_From
     (This              : in out Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set;
      Load_Foreign_Keys : in     Boolean := False);

   procedure Set
     (This              : in out Field;
      Value             : in     Fixed_Type);

   procedure Set
     (This              : in out Field;
      Value             : in     String);

   overriding function To_SQL
     (This              : in Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String;

   overriding function To_String (This : in Field) return String;

private

   type Field is new DB.Active_Record.Fields.Field with record
      Default_Value     : Fixed_Type := Initialization_Value;
      Value             : Fixed_Type := Initialization_Value;
   end record;

end DB.Active_Record.Fields.Generic_Fixed;
