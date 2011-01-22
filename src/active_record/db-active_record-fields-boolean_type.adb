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
--    db-active_record-fields-boolean_type.adb   jvinters   22-January-2011
--

package body DB.Active_Record.Fields.Boolean_Type is

   ---------
   -- "=" --
   ---------

   function "="
     (Left              : in Boolean_Field'Class;
      Right             : in Boolean) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      if Right then
         Set_Criteria (Temp, Left, EQUAL, "true");
      else
         Set_Criteria (Temp, Left, EQUAL, "false");
      end if;
      return Temp;
   end "=";

   ----------
   -- "/=" --
   ----------

   function "/="
     (Left              : in Boolean_Field'Class;
      Right             : in Boolean) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      if Right then
         Set_Criteria (Temp, Left, NOT_EQUAL, "true");
      else
         Set_Criteria (Temp, Left, NOT_EQUAL, "false");
      end if;
      return Temp;
   end "/=";

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Boolean_Field) is
   begin
      This.Changed := True;
      This.Value := False;

      if This.Has_Default then
         This.Value := This.Default_Value;
         This.Is_Null := False;
      else
         This.Is_Null := True;
      end if;
   end Clear;

   ---------------
   -- Configure --
   ---------------

   function Configure
     (Name              : in String;
      Display_Name      : in String := "";
      Not_Null          : in Boolean := False;
      Unique            : in Boolean := False;
      Has_Default       : in Boolean := True;
      Default_Value     : in Boolean := False) return Boolean_Field
   is
      Temp              : Boolean_Field;
   begin
      Config_Name (Temp, Name, Display_Name);
      Temp.Not_Null := Not_Null;
      Temp.Unique := Unique;
      Temp.Has_Default := Has_Default;
      Temp.Default_Value := Default_Value;
      return Temp;
   end Configure;

   ---------------
   -- Field_SQL --
   ---------------

   function Field_SQL
     (This              : in Boolean_Field;
      Connector         : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
      Constraints       : constant DB.Types.SQL_String := 
        Constraints_SQL (This);
      Field_Name        : constant String := To_String (This.Field_Name);
   begin
      return DB.Types.SQL_String (Field_Name & " BOOLEAN") & Constraints;
   end Field_SQL;

   ---------
   -- Get --
   ---------

   function Get (This : in Boolean_Field) return Boolean is
   begin
      return This.Value;
   end Get;

   ---------------
   -- Load_From --
   ---------------

   procedure Load_From
     (This              : in out Boolean_Field;
      Connection        : in     DB.Connector.Connection;
      Results           : in     DB.Connector.Result_Set)
   is
      Field_Name        : constant String := This.Get_Name;
   begin
      if Results.Get_Is_Null (Field_Name) then
         if This.Has_Default then
            This.Value := This.Default_Value;
            This.Is_Null := False;
         else
            This.Value := False;
            This.Is_Null := True;
         end if;
      else
         This.Value := Results.Get_Boolean (This.Get_Name, False);
         This.Is_Null := False;
      end if;
      This.Changed := False;
   end Load_From;

   ---------
   -- Set --
   ---------

   procedure Set
     (This              : in out Boolean_Field;
      Value             : in     Boolean)
   is
   begin
      This.Value := Value;
      This.Changed := True;
      This.Is_Null := False;
   end Set;

   ------------
   -- To_SQL --
   ------------

   function To_SQL
     (This              : in Boolean_Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
   begin
      if This.Is_Null then
         return "NULL";
      else
         if This.Value then
            return "'true'";
         else
            return "'false'";
         end if;
      end if;
   end To_SQL;

end DB.Active_Record.Fields.Boolean_Type;

