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

with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Ada.Strings;                      use Ada.Strings;
with Ada.Strings.Fixed;                use Ada.Strings.Fixed;

package body DB.Active_Record.Fields.Character_Types is

   package body Text is

      ---------
      -- "=" --
      ---------

      function "="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, EQUAL, Right);
         return Temp;
      end "=";

      ----------
      -- "/=" --
      ----------

      function "/="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, NOT_EQUAL, Right);
         return Temp;
      end "/=";

      ---------
      -- "<" --
      ---------

      function "<"
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, LESS_THAN, Right);
         return Temp;
      end "<";

      ----------
      -- "<=" --
      ----------

      function "<="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, LESS_THAN_OR_EQUAL, Right);
         return Temp;
      end "<=";

      ----------
      -- ">=" --
      ----------

      function ">="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, GREATER_THAN_OR_EQUAL, Right);
         return Temp;
      end ">=";

      ---------
      -- ">" --
      ---------

      function ">"
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, GREATER_THAN, Right);
         return Temp;
      end ">";

      ------------
      -- Append --
      ------------

      procedure Append
        (This           : in out Field;
         Str            : in     String)
      is
      begin
         Append (This.Value, Str);
         This.Changed := True;
         This.Is_Null := False;
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
         Has_Default    : in Boolean := True;
         Default_Value  : in String := "") return Field
      is
         Lower_Name        : constant String := To_Lower (Name);
         Temp              : Field;
      begin
         Config_Name (Temp, Name, Display_Name);
         Temp.Not_Null := Not_Null;
         Temp.Unique := Unique;
         Temp.Has_Default := Has_Default;
         Temp.Maximum_Length := Maximum_Length;
         Set_Unbounded_String (Temp.Default_Value, Default_Value);
         return Temp;
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
            Connector.Get_Driver.Get_Text_Type (This.Maximum_Length);
      begin
         return DB.Types.SQL_String (Field_Name & ' ') & Field_Type & Constraints;
      end Field_SQL;

      ---------
      -- Get --
      ---------

      function Get (This : in Field) return String is
      begin
         return To_String (This.Value);
      end Get;

      function Get (This : in Field) return Unbounded_String is
      begin
         return This.Value;
      end Get;

      -----------
      -- ILike --
      -----------

      function ILike
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, ILIKE, Right, True);
         return Temp;
      end ILike;

      ----------
      -- Like --
      ----------

      function Like
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, ILIKE, Right, True);
         return Temp;
      end Like;

      ---------------
      -- Load_From --
      ---------------

      procedure Load_From
        (This           : in out Field;
         Connection     : in     DB.Connector.Connection;
         Results        : in     DB.Connector.Result_Set)
      is
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
         if This.Is_Null then
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
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, EQUAL, Right);
         return Temp;
      end "=";

      ----------
      -- "/=" --
      ----------

      function "/="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, NOT_EQUAL, Right);
         return Temp;
      end "/=";

      ---------
      -- "<" --
      ---------

      function "<"
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, LESS_THAN, Right);
         return Temp;
      end "<";

      ----------
      -- "<=" --
      ----------

      function "<="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, LESS_THAN_OR_EQUAL, Right);
         return Temp;
      end "<=";

      ----------
      -- ">=" --
      ----------

      function ">="
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, GREATER_THAN_OR_EQUAL, Right);
         return Temp;
      end ">=";

      ---------
      -- ">" --
      ---------

      function ">"
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, GREATER_THAN, Right);
         return Temp;
      end ">";

      ------------
      -- Append --
      ------------

      procedure Append
        (This           : in out Field;
         Str            : in     String)
      is
      begin
         Append (This.Value, Str);
         This.Changed := True;
         This.Is_Null := False;
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
         Has_Default    : in Boolean := True;
         Default_Value  : in String := "") return Field
      is
         Lower_Name        : constant String := To_Lower (Name);
         Temp              : Field;
      begin
         Config_Name (Temp, Name, Display_Name);
         Temp.Not_Null := Not_Null;
         Temp.Unique := Unique;
         Temp.Has_Default := Has_Default;
         Temp.Maximum_Length := Maximum_Length;
         Set_Unbounded_String (Temp.Default_Value, Default_Value);
         return Temp;
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
         return To_String (This.Value);
      end Get;

      function Get (This : in Field) return Unbounded_String is
      begin
         return This.Value;
      end Get;

      -----------
      -- ILike --
      -----------

      function ILike
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, ILIKE, Right, True);
         return Temp;
      end ILike;

      ----------
      -- Like --
      ----------

      function Like
        (Left           : in Field'Class;
         Right          : in String) return Field_Criteria
      is
         Temp           : Field_Criteria;
      begin
         Set_Criteria (Temp, Left, ILIKE, Right, True);
         return Temp;
      end Like;

      ---------------
      -- Load_From --
      ---------------

      procedure Load_From
        (This           : in out Field;
         Connection     : in     DB.Connector.Connection;
         Results        : in     DB.Connector.Result_Set)
      is
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
         if This.Is_Null then
            return "NULL";
         else
            return "'" & Connection.Quote_Value (To_String (This.Value)) & "'";
         end if;
      end To_SQL;

   end Varchar;

end DB.Active_Record.Fields.Character_Types;

