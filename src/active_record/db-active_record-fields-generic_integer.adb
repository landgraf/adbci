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
--    db-active_record-fields-generic_Integer.ads   jvinters   21-January-2011
--

with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Ada.Strings;                      use Ada.Strings;
with Ada.Strings.Fixed;                use Ada.Strings.Fixed;

package body DB.Active_Record.Fields.Generic_Integer is

   type Field_Size_Type is (BITS_16, BITS_32, BITS_64);
   Field_Size           : Field_Size_Type := BITS_16;

   function "="
     (Left              : in Field'Class;
      Right             : in Integer_Type) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, EQUAL, Integer_Type'Image (Right));
      return Temp;
   end "=";

   function "/="
     (Left              : in Field'Class;
      Right             : in Integer_Type) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, NOT_EQUAL, Integer_Type'Image (Right));
      return Temp;
   end "/=";

   function "<"
     (Left              : in Field'Class;
      Right             : in Integer_Type) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, LESS_THAN, Integer_Type'Image (Right));
      return Temp;
   end "<";

   function "<="
     (Left              : in Field'Class;
      Right             : in Integer_Type) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, LESS_THAN_OR_EQUAL, Integer_Type'Image (Right));
      return Temp;
   end "<=";

   function ">="
     (Left              : in Field'Class;
      Right             : in Integer_Type) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, GREATER_THAN_OR_EQUAL, Integer_Type'Image (Right));
      return Temp;
   end ">=";

   function ">"
     (Left              : in Field'Class;
      Right             : in Integer_Type) return Field_Criteria
   is
      Temp              : Field_Criteria;
   begin
      Set_Criteria (Temp, Left, GREATER_THAN, Integer_Type'Image (Right));
      return Temp;
   end ">";

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Field) is
   begin
      This.Changed := True;
      This.Value := Initialization_Value;

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
      Default_Value     : in Integer_Type := Initialization_Value)
     return Field
   is
      Lower_Name        : constant String := To_Lower (Name);
      Temp              : Field;
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
     (This              : in Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
      Constraints       : constant DB.Types.SQL_String := 
        Constraints_SQL (This);
      Field_Name        : constant String := To_String (This.Field_Name);
   begin
      case Field_Size is
         when BITS_16 =>
            return DB.Types.SQL_String (Field_Name & " SMALLINT") & Constraints;
         when BITS_32 =>
            return DB.Types.SQL_String (Field_Name & " INTEGER") & Constraints;
         when BITS_64 =>
            return DB.Types.SQL_String (Field_Name & " BIGINT") & Constraints;
      end case;
   end Field_SQL;

   ---------
   -- Get --
   ---------

   function Get (This : in Field) return Integer_Type is
   begin
      return This.Value;
   end Get;

   function Get (This : in Field) return String is
      Temp              : constant String :=
        Trim (Integer_Type'Image (This.Value), Both);
   begin
      return Temp;
   end Get;

   ---------------
   -- Load_From --
   ---------------

   procedure Load_From
     (This              : in out Field;
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
            This.Value := Initialization_Value;
            This.Is_Null := True;
         end if;
      else
         This.Value := Integer_Type'Value
           (Results.Get_String (This.Get_Name, False));
         This.Is_Null := False;
      end if;
      This.Changed := False;
   end Load_From;

   ---------
   -- Set --
   ---------

   procedure Set
     (This              : in out Field;
      Value             : in     Integer_Type)
   is
   begin
      This.Value := Value;
      This.Changed := True;
      This.Is_Null := False;
   end Set;

   procedure Set
     (This              : in out Field;
      Value             : in     String)
   is
   begin
      This.Value := Integer_Type'Value (Value);
      This.Changed := True;
      This.Is_Null := False;
   end Set;

   ------------
   -- To_SQL --
   ------------

   function To_SQL
     (This              : in Field;
      Connection        : in DB.Connector.Connection)
     return DB.Types.SQL_String
   is
   begin
      if This.Is_Null then
         return "NULL";
      else
         declare
            Value_Str         : constant String :=
              Trim (Integer_Type'Image (This.Value), Both);
         begin
            return Connection.Quote_Value (Value_Str);
         end;
      end if;
   end To_SQL;

begin
   --  Pick the smallest integer type that will hold the entire range.
   
   if Integer_Type'First < -2**63 or else Integer_Type'Last >= 2**63 then
      raise CONSTRAINT_ERROR with "can't support more than 64-bit integers";
   elsif Integer_Type'First < -2**31 or else Integer_Type'Last >= 2**31 then
      Field_Size := BITS_64;
   elsif Integer_Type'First < -2**15 or else Integer_Type'Last >= 2**15 then
      Field_Size := BITS_32;
   else
      Field_Size := BITS_16;
   end if;
end DB.Active_Record.Fields.Generic_Integer;
