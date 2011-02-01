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
--    db-active_record-fields-character_fields.ads   jvinters   16-January-2011
--

with Ada.Strings;                      use Ada.Strings;
with Ada.Strings.Fixed;                use Ada.Strings.Fixed;
with DB.Errors;

package body DB.Active_Record.Fields.Character_Types is

   package body Text is

      ---------
      -- "=" --
      ---------

      function "="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, EQUAL, Right, True);
         end return;
      end "=";

      ----------
      -- "/=" --
      ----------

      function "/="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, NOT_EQUAL, Right, True);
         end return;
      end "/=";

      ---------
      -- "<" --
      ---------

      function "<"
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, LESS_THAN, Right, True);
         end return;
      end "<";

      ----------
      -- "<=" --
      ----------

      function "<="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, LESS_THAN_OR_EQUAL, Right, True);
         end return;
      end "<=";

      ----------
      -- ">=" --
      ----------

      function ">="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, GREATER_THAN_OR_EQUAL, Right, True);
         end return;
      end ">=";

      ---------
      -- ">" --
      ---------

      function ">"
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, GREATER_THAN, Right, True);
         end return;
      end ">";

      ------------
      -- Append --
      ------------

      procedure Append
        (This           : in out Field;
         Str            : in     String)
      is
      begin
         if not This.Loaded then
            raise DB.Errors.NOT_LOADED;
         else
            Append (This.Value, Str);
            This.Changed := True;
            This.Is_Null := False;
         end if;
      end Append;

      -----------
      -- Clear --
      -----------

      procedure Clear (This : in out Field) is
      begin
         This.Changed := True;
         Set_Unbounded_String (This.Value, "");

         if This.Has_Default then
            This.Value := This.Default_Value;
            This.Is_Null := False;
         else
            This.Is_Null := True;
         end if;

         This.Loaded := True;
      end Clear;

      ---------------
      -- Configure --
      ---------------

      function Configure
        (Name           : in String;
         Display_Name   : in String := "";
         Maximum_Length : in Positive := 255;
         Not_Null       : in Boolean := False;
         Unique         : in Boolean := False;
         Allow_Blank    : in Boolean := True;
         Has_Default    : in Boolean := True;
         Default_Value  : in String := "") return Field
      is
      begin
         return Temp : Field do
            Config_Name (Temp, Name, Display_Name);
            Temp.Allow_Blank := Allow_Blank;
            Temp.Not_Null := Not_Null;
            Temp.Unique := Unique;
            Temp.Has_Default := Has_Default;
            Temp.Maximum_Length := Maximum_Length;
            Set_Unbounded_String (Temp.Default_Value, Default_Value);
         end return;
      end Configure;

      ---------------
      -- Field_SQL --
      ---------------

      function Field_SQL
        (This           : in Field;
         Connector      : in DB.Connector.Connection)
        return DB.Types.SQL_String
      is
         Constraints    : constant DB.Types.SQL_String :=
           Constraints_SQL (This);
         Field_Name     : constant String := To_String (This.Field_Name);
         Field_Type     : constant DB.Types.SQL_String :=
            Connector.Get_Driver.all.Get_Text_Type (This.Maximum_Length);
      begin
         return DB.Types.SQL_String (Field_Name & ' ') & Field_Type & Constraints;
      end Field_SQL;

      ---------
      -- Get --
      ---------

      function Get (This : in Field) return String is
      begin
         if This.Loaded then
            return To_String (This.Value);
         else
            raise DB.Errors.NOT_LOADED;
         end if;
      end Get;

      function Get (This : in Field) return Unbounded_String is
      begin
         if This.Loaded then
            return This.Value;
         else
            raise DB.Errors.NOT_LOADED;
         end if;
      end Get;

      -----------
      -- ILike --
      -----------

      function ILike
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, ILIKE, Right, True);
         end return;
      end ILike;

      --------------
      -- Is_Blank --
      --------------

      function Is_Blank (This : in Field) return Boolean is
      begin
         return Length (This.Value) = 0;
      end Is_Blank;

      ----------
      -- Like --
      ----------

      function Like
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, ILIKE, Right, True);
         end return;
      end Like;

      ---------------
      -- Load_From --
      ---------------

      procedure Load_From
        (This              : in out Field;
         Connection        : in     DB.Connector.Connection;
         Results           : in     DB.Connector.Result_Set;
         Load_Foreign_Keys : in     Boolean := False)
      is
         pragma Unreferenced (Connection);
         pragma Unreferenced (Load_Foreign_Keys);
         Field_Name     : constant String := This.Get_Name;
      begin
         This.Loaded := False;
         if Results.Get_Is_Null (Field_Name) then
            if This.Has_Default then
               This.Value := This.Default_Value;
               This.Is_Null := False;
            else
               Set_Unbounded_String (This.Value, "");
               This.Is_Null := True;
            end if;
         else
            Set_Unbounded_String
              (This.Value, Results.Get_String (This.Get_Name, False));
            This.Is_Null := False;
         end if;
         This.Changed := False;
         This.Loaded := True;
      end Load_From;

      ---------
      -- Set --
      ---------

      procedure Set
        (This           : in out Field;
         Value          : in     String)
      is
      begin
         if Value'Length > This.Maximum_Length then
            raise CONSTRAINT_ERROR with "string too long";
         else
            Set_Unbounded_String (This.Value, Value);
            This.Changed := True;
            This.Is_Null := False;
            This.Loaded := True;
         end if;
      end Set;

      procedure Set
        (This           : in out Field;
         Value          : in     Unbounded_String)
      is
      begin
         if Length (Value) > This.Maximum_Length then
            raise CONSTRAINT_ERROR with "string too long";
         else
            This.Value := Value;
            This.Changed := True;
            This.Is_Null := False;
            This.Loaded := True;
         end if;
      end Set;

      ------------
      -- To_SQL --
      ------------

      function To_SQL
        (This           : in Field;
         Connection     : in DB.Connector.Connection)
        return DB.Types.SQL_String
      is
      begin
         if not This.Loaded then
            raise DB.Errors.NOT_LOADED;
         elsif This.Is_Null then
            return "NULL";
         else
            return "'" & Connection.Quote_Value (To_String (This.Value)) & "'";
         end if;
      end To_SQL;

   end Text;


   package body Varchar is

      ---------
      -- "=" --
      ---------

      function "="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, EQUAL, Right, True);
         end return;
      end "=";

      ----------
      -- "/=" --
      ----------

      function "/="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, NOT_EQUAL, Right, True);
         end return;
      end "/=";

      ---------
      -- "<" --
      ---------

      function "<"
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, LESS_THAN, Right, True);
         end return;
      end "<";

      ----------
      -- "<=" --
      ----------

      function "<="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, LESS_THAN_OR_EQUAL, Right, True);
         end return;
      end "<=";

      ----------
      -- ">=" --
      ----------

      function ">="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, GREATER_THAN_OR_EQUAL, Right, True);
         end return;
      end ">=";

      ---------
      -- ">" --
      ---------

      function ">"
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, GREATER_THAN, Right, True);
         end return;
      end ">";

      ------------
      -- Append --
      ------------

      procedure Append
        (This           : in out Field;
         Str            : in     String)
      is
      begin
         if not This.Loaded then
            Append (This.Value, Str);
            This.Changed := True;
            This.Is_Null := False;
            This.Loaded := True;
         else
            raise DB.Errors.NOT_LOADED;
         end if;
      end Append;

      -----------
      -- Clear --
      -----------

      procedure Clear (This : in out Field) is
      begin
         This.Changed := True;
         Set_Unbounded_String (This.Value, "");

         if This.Has_Default then
            This.Value := This.Default_Value;
            This.Is_Null := False;
         else
            This.Is_Null := True;
         end if;

         This.Loaded := True;
      end Clear;

      ---------------
      -- Configure --
      ---------------

      function Configure
        (Name           : in String;
         Display_Name   : in String := "";
         Maximum_Length : in Positive := 255;
         Not_Null       : in Boolean := False;
         Unique         : in Boolean := False;
         Allow_Blank    : in Boolean := True;
         Has_Default    : in Boolean := True;
         Default_Value  : in String := "") return Field
      is
      begin
         return Temp : Field do
            Config_Name (Temp, Name, Display_Name);
            Temp.Allow_Blank := Allow_Blank;
            Temp.Not_Null := Not_Null;
            Temp.Unique := Unique;
            Temp.Has_Default := Has_Default;
            Temp.Maximum_Length := Maximum_Length;
            Set_Unbounded_String (Temp.Default_Value, Default_Value);
         end return;
      end Configure;

      ---------------
      -- Field_SQL --
      ---------------

      function Field_SQL
        (This           : in Field;
         Connector      : in DB.Connector.Connection)
        return DB.Types.SQL_String
      is
         pragma Unreferenced (Connector);
         Constraints    : constant DB.Types.SQL_String :=
           Constraints_SQL (This);
         Field_Name     : constant String := To_String (This.Field_Name);
         Length_Str     : constant DB.Types.SQL_String :=
           DB.Types.SQL_String (Trim (Positive'Image (This.Maximum_Length), Both));
      begin
         return DB.Types.SQL_String (Field_Name & " VARCHAR(") &
           Length_Str & ")" & Constraints;
      end Field_SQL;

      ---------
      -- Get --
      ---------

      function Get (This : in Field) return String is
      begin
         if This.Loaded then
            return To_String (This.Value);
         else
            raise DB.Errors.NOT_LOADED;
         end if;
      end Get;

      function Get (This : in Field) return Unbounded_String is
      begin
         if This.Loaded then
            return This.Value;
         else
            raise DB.Errors.NOT_LOADED;
         end if;
      end Get;

      -----------
      -- ILike --
      -----------

      function ILike
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, ILIKE, Right, True);
         end return;
      end ILike;

      --------------
      -- Is_Blank --
      --------------

      function Is_Blank (This : in Field) return Boolean is
      begin
         return This.Is_Null or else Length (This.Value) = 0;
      end Is_Blank;

      ----------
      -- Like --
      ----------

      function Like
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
      begin
         return Temp : Field_Criteria do
            Set_Criteria (Temp, Left, ILIKE, Right, True);
         end return;
      end Like;

      ---------------
      -- Load_From --
      ---------------

      procedure Load_From
        (This              : in out Field;
         Connection        : in     DB.Connector.Connection;
         Results           : in     DB.Connector.Result_Set;
         Load_Foreign_Keys : in     Boolean := False)
      is
         pragma Unreferenced (Connection);
         pragma Unreferenced (Load_Foreign_Keys);
         Field_Name     : constant String := This.Get_Name;
      begin
         if Results.Get_Is_Null (Field_Name) then
            if This.Has_Default then
               This.Value := This.Default_Value;
               This.Is_Null := False;
            else
               Set_Unbounded_String (This.Value, "");
               This.Is_Null := True;
            end if;
         else
            Set_Unbounded_String
              (This.Value, Results.Get_String (This.Get_Name, False));
            This.Is_Null := False;
         end if;
         This.Changed := False;
         This.Loaded := True;
      end Load_From;

      ---------
      -- Set --
      ---------

      procedure Set
        (This           : in out Field;
         Value          : in     String)
      is
      begin
         if Value'Length > This.Maximum_Length then
            raise CONSTRAINT_ERROR with "string too long";
         else
            Set_Unbounded_String (This.Value, Value);
            This.Changed := True;
            This.Is_Null := False;
            This.Loaded := True;
         end if;
      end Set;

      procedure Set
        (This           : in out Field;
         Value          : in     Unbounded_String)
      is
      begin
         if Length (Value) > This.Maximum_Length then
            raise CONSTRAINT_ERROR with "string too long";
         else
            This.Value := Value;
            This.Changed := True;
            This.Is_Null := False;
            This.Loaded := True;
         end if;
      end Set;

      ------------
      -- To_SQL --
      ------------

      function To_SQL
        (This           : in Field;
         Connection     : in DB.Connector.Connection)
        return DB.Types.SQL_String
      is
      begin
         if not This.Loaded then
            raise DB.Errors.NOT_LOADED;
         elsif This.Is_Null then
            return "NULL";
         else
            return "'" & Connection.Quote_Value (To_String (This.Value)) & "'";
         end if;
      end To_SQL;

   end Varchar;

end DB.Active_Record.Fields.Character_Types;

