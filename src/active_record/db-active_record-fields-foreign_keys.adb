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
--    db-active_record-fields-foreign_keys.adb   jvinters   17-January-2011
--

with Ada.Characters.Handling;          use Ada.Characters.Handling;

package body DB.Active_Record.Fields.Foreign_Keys is

   ---------
   -- "=" --
   ---------

   function "="
     (Left              : in Foreign_Key_Field;
      Right             : in DB.Types.Object_Id) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, EQUAL, DB.Types.Object_Id'Image (Right));
      return Temp;
   end "=";

   function "="
     (Left              : in Foreign_Key_Field;
      Right             : in Model_Type) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, EQUAL, Right.Get_Id);
      return Temp;
   end "=";

   ----------
   -- "/=" --
   ----------

   function "/="
     (Left              : in Foreign_Key_Field;
      Right             : in DB.Types.Object_Id) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, NOT_EQUAL, DB.Types.Object_Id'Image (Right));
      return Temp;
   end "/=";

   function "/="
     (Left              : in Foreign_Key_Field;
      Right             : in Model_Type) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, NOT_EQUAL, Right.Get_Id);
      return Temp;   
   end "/=";

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Foreign_Key_Field) is
   begin
      This.Changed := True;
      This.Is_Null := True;
      This.FK.Clear;
   end Clear;

   ---------------
   -- Configure --
   ---------------

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Cascade_Delete    : in Boolean := False) return Foreign_Key_Field
   is
      Lower_Name        : constant String := To_Lower (Name);
      Temp              : Foreign_Key_Field;
   begin
      if not Validate_Field_Name (Lower_Name) then
         raise CONSTRAINT_ERROR with "invalid field name";
      else
         Set_Unbounded_String (Temp.Field_Name, Lower_Name);
         if Display_Name /= "" then
            Set_Unbounded_String (Temp.Display_Name, Display_Name);
         else
            Set_Unbounded_String (Temp.Display_Name, Lower_Name);
         end if;

         Temp.Not_Null := Not_Null;
         Temp.Unique := Unique;
         Temp.Has_Default := False;
         Temp.FK.Clear;
         Temp.FK_Options.Cascade_Delete := Cascade_Delete;
         return Temp;
      end if;
   end Configure;

   ---------------
   -- Field_SQL --
   ---------------

   function Field_SQL
     (This              : in Foreign_Key_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
      Constraints       : constant DB.Types.SQL_String := 
        Constraints_SQL (This);
      Field_Name        : constant String := To_String (This.Field_Name);
      Foreign_Model     : constant DB.Types.SQL_String :=
        DB.Types.SQL_String (This.FK.Get_Name);
      Id_SQL            : constant DB.Types.SQL_String :=
        Connection.Get_Driver.Get_Id_SQL;
      Result            : constant DB.Types.SQL_String :=
        DB.Types.SQL_String (Field_Name & ' ') &
        Id_SQL & " REFERENCES " & Foreign_Model & "(" &
        DB.Types.SQL_String (This.FK.Get_Id_Name) & ")" & Constraints;
   begin
      if This.FK_Options.Cascade_Delete then
         return Result & " ON DELETE CASCADE";
      else
         return Result;
      end if;
   end Field_SQL;

   ---------
   -- Get --
   ---------

   function Get (This : in Foreign_Key_Field) return Model_Type is
   begin
      return This.FK;
   end Get;

   ---------------
   -- Load_From --
   ---------------

   procedure Load_From
     (This              : in out Foreign_Key_Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set)
   is
      Field_Name        : constant String := This.Get_Name;
   begin
      if not Results.Get_Is_Null (Field_Name) then
         declare
            Item_FK     : constant DB.Types.Object_Id :=
              Results.Get_Object_Id (Field_Name);
         begin
            This.FK.Get (Connection, Item_FK);
         end;
      else
         This.FK.Clear;
      end if;
   end Load_From;

   ---------
   -- Set --
   ---------

   procedure Set
     (This              : in out Foreign_Key_Field;
      Value             : in     Model_Type)
   is
   begin
      This.FK := Value;
      This.Changed := True;
      This.Is_Null := False;
   end Set;

   ------------
   -- To_SQL --
   ------------

   function To_SQL
     (This              : in Foreign_Key_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
   begin
      if This.FK.Get_Id /= 0 then
         declare
            Id          : constant String := This.FK.Get_Id;
         begin
            return DB.Types.SQL_String (Id);
         end;
      else
         return "NULL";
      end if;
   end To_SQL;

end DB.Active_Record.Fields.Foreign_Keys;

