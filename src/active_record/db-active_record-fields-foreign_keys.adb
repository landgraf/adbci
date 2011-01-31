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
with DB.Errors;

package body DB.Active_Record.Fields.Foreign_Keys is

   ---------
   -- "=" --
   ---------

   function "="
     (Left              : in Field;
      Right             : in DB.Types.Object_Id) return Field_Criteria
   is
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, EQUAL, DB.Types.Object_Id'Image (Right));
      end return;
   end "=";

   function "="
     (Left              : in Field;
      Right             : in Model_Type) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, EQUAL, Right.Get_Id);
      end return;
   end "=";

   ----------
   -- "/=" --
   ----------

   function "/="
     (Left              : in Field;
      Right             : in DB.Types.Object_Id) return Field_Criteria
   is
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, NOT_EQUAL, DB.Types.Object_Id'Image (Right));
      end return;
   end "/=";

   function "/="
     (Left              : in Field;
      Right             : in Model_Type) return Field_Criteria
   is
   begin
      return Temp : Field_Criteria do
         Set_Criteria (Temp, Left, NOT_EQUAL, Right.Get_Id);
      end return;
   end "/=";

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Field) is
   begin
      This.Changed := True;
      This.Is_Null := True;
      This.FK.Clear;
      This.FK_Options.FK_Id := 0;
      This.Loaded := True;
   end Clear;

   ---------------
   -- Configure --
   ---------------

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Cascade_Delete    : in Boolean := False) return Field
   is
      Lower_Name        : constant String := To_Lower (Name);
   begin
      return Temp : Field do
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
         end if;
      end return;
   end Configure;

   ---------------
   -- Field_SQL --
   ---------------

   function Field_SQL
     (This              : in Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
      Constraints       : constant DB.Types.SQL_String :=
        Constraints_SQL (This);
      Field_Name        : constant String := To_String (This.Field_Name);
      Foreign_Model     : constant DB.Types.SQL_String :=
        DB.Types.SQL_String (This.FK.Get_Name);
      Id_SQL            : constant DB.Types.SQL_String :=
        Connection.Get_Driver.Get_Foreign_Key_SQL;
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

   function Get (This : in Field) return Model_Type is
   begin
      if This.Loaded then
         return This.FK;
      else
         raise DB.Errors.NOT_LOADED;
      end if;
   end Get;

   function Get (This : in Field) return DB.Types.Object_Id is
   begin
      return This.FK_Options.FK_Id;
   end Get;

   --------------------
   -- Is_Foreign_Key --
   --------------------

   function Is_Foreign_Key (This : in Field) return Boolean is
   begin
      pragma Unreferenced (This);
      return True;
   end Is_Foreign_Key;

   ---------------
   -- Load_From --
   ---------------

   procedure Load_From
     (This              : in out Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set;
      Load_Foreign_Keys : in     Boolean := False)
   is
      procedure Set_Not_Loaded
        (F : in out DB.Active_Record.Fields.Field'Class)
      is
      begin
         F.Set_Loaded (False);
      end Set_Not_Loaded;

      Field_Name        : constant String := This.Get_Name;
      Is_Null           : constant Boolean := Results.Get_Is_Null (Field_Name);
   begin
      This.Loaded := False;
      if not Is_Null and then Load_Foreign_Keys then
         declare
            Item_FK     : constant DB.Types.Object_Id :=
              Results.Get_Object_Id (Field_Name);
         begin
            This.FK_Options.Results := DB.Connector.Null_Result_Set;
            This.FK.Get (Connection, Item_FK, Load_Foreign_Keys);
            This.FK_Options.FK_Id := Item_FK;
            This.Loaded := True;
         end;
      else
         if not Is_Null then
            declare
               Item_FK  : constant DB.Types.Object_Id :=
                 Results.Get_Object_Id (Field_Name);
            begin
               This.FK_Options.Results := Results;
               DB.Active_Record.Models.Iterate_Fields
                 (This.FK, Set_Not_Loaded'Access);
               This.FK_Options.FK_Id := Item_FK;
            end;
         else
            This.FK.Clear;
         end if;
      end if;
   end Load_From;

   --------------
   -- Load_Now --
   --------------

   procedure Load_Now
     (This              : in out Field;
      Connection        : in     DB.Connector.Connection;
      Recurse           : in     Boolean := False)
   is
      Field_Name        : constant String := This.Get_Name;
      Item_FK           : constant DB.Types.Object_Id :=
        This.FK_Options.Results.Get_Object_Id (Field_Name);
   begin
      if This.FK_Options.Results /= DB.Connector.Null_Result_Set then
         This.FK.Get (Connection, Item_FK, Recurse);
         This.FK_Options.FK_Id := Item_FK;
         This.Loaded := True;
      end if;
   end Load_Now;

   ---------
   -- Set --
   ---------

   procedure Set
     (This              : in out Field;
      Value             : in     Model_Type)
   is
   begin
      This.FK := Value;
      This.FK_Options.FK_Id := This.FK.Get_Id;
      This.Changed := True;
      This.Is_Null := False;
      This.Loaded := True;
   end Set;

   ------------
   -- To_SQL --
   ------------

   function To_SQL
     (This              : in Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
      Id                : DB.Types.Object_Id;
   begin
      if This.Loaded then
         Id := This.FK.Get_Id;
      else
         Id := This.FK_Options.FK_Id;
      end if;

      if Id /= 0 then
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
