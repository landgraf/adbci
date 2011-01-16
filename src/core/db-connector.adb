--
--  (c) Copyright 2011, John Vinters
--
--  ADBC is free software; you can redistribute it and/or 
--  modify it under the terms of the GNU Lesser General Public License 
--  as published by the Free Software Foundation; either version 3, or 
--  (at your option) any later version.  
--
--  ADBC is distributed in the hope that it will be useful, but WITHOUT ANY 
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
--  FOR A PARTICULAR PURPOSE.
--
--  You should have received a copy of the GNU Lesser General Public License
--  along with this library; if not, see <http://www.gnu.org/licenses/>
--
--    db-connector.adb   jvinters   16-January-2011
--

with DB.Driver_Manager;
with DB.Errors;

package body DB.Connector is

   use type DB.Driver.Driver_Handle;
   use type DB.Driver.Result_Handle;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out Result_Set) is
   begin
      if This.Results /= null then
         This.Results.Reference_Count := This.Results.Reference_Count + 1;
      end if;
   end Adjust;

   -------------
   -- Connect --
   -------------

   function Connect
     (Driver            : in String;
      Hostname          : in String;
      Database          : in String;
      Username          : in String := "";
      Password          : in String := "";
      Options           : in String := "") return Connection
   is
      Reqd_Driver       : DB.Driver.Driver_Handle;
   begin
      Reqd_Driver := DB.Driver_Manager.Get_Driver (Driver);
      Reqd_Driver.Connect (Hostname, Database, Username, Password, Options);

      return (Ada.Finalization.Limited_Controlled with
        Driver          => Reqd_Driver,
        In_Transaction  => False
      );
   end Connect;

   -----------
   -- Count --
   -----------

   function Count (This : in Result_Set) return Natural is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         return This.Results.Data.Get_Tuple_Count;
      else
         return 0;
      end if;
   end Count;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (This : in out Connection) is
   begin
      if This.Driver /= null then
         This.Driver.Disconnect;
         DB.Driver.Free_Driver (This.Driver);
      end if;
   end Disconnect;

   -------------
   -- Execute --
   -------------

   function Execute
     (This              : in Connection'Class;
      SQL               : in DB.Types.SQL_String) return Result_Set
   is
      R                 : DB.Driver.Result_Handle;
      Result_Rec        : Result_Record_Access := null;
      Tuple             : Tuple_Index := INVALID_TUPLE;
   begin
      This.Driver.Execute_SQL (R, SQL);

      Result_Rec := new Result_Record;
      Result_Rec.Data := R;
      Result_Rec.Driver := This.Driver;
      Result_Rec.Reference_Count := 1;

      if R /= null and then R.Get_Tuple_Count > 0 then
         Tuple := 1;
      end if;

      return (Ada.Finalization.Controlled with
        Results         => Result_Rec,
        Tuple           => Tuple
      );
   end Execute;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Connection) is
   begin
      Disconnect (This);
   end Finalize;

   procedure Finalize (This : in out Result_Set) is
   begin
      if This.Results /= null then
         if This.Results.Reference_Count > 0 then
            This.Results.Reference_Count := 
              This.Results.Reference_Count - 1;
            if This.Results.Reference_Count = 0 then
               DB.Driver.Free_Result
                 (This.Results.Driver.all, This.Results.Data);
            end if;
         end if;
      end if;
   end Finalize;

   ----------------
   -- Get_Bigint --
   ----------------

   function Get_Bigint
     (This              : in Result_Set;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Bigint := 0)
     return DB.Types.DB_Bigint
   is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         return This.Results.Data.Get_Data_Bigint
           (This.Tuple, Column, Replace_Null, Replacement);
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_Bigint;

   function Get_Bigint
     (This              : in Result_Set;
      Column            : in String;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Bigint := 0)
     return DB.Types.DB_Bigint
   is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         declare
            Reqd_Column : constant Column_Index :=
              This.Results.Data.Find_Column_By_Name (Column);
         begin
            return This.Results.Data.Get_Data_Bigint
              (This.Tuple, Reqd_Column, Replace_Null, Replacement);
         end;
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_Bigint;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean
     (This              : in Result_Set;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in Boolean := False) return Boolean
   is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         return This.Results.Data.Get_Data_Boolean
           (This.Tuple, Column, Replace_Null, Replacement);
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_Boolean;

   function Get_Boolean
     (This              : in Result_Set;
      Column            : in String;
      Replace_Null      : in Boolean := False;
      Replacement       : in Boolean := False) return Boolean
   is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         declare
            Reqd_Column : constant Column_Index :=
              This.Results.Data.Find_Column_By_Name (Column);
         begin
            return This.Results.Data.Get_Data_Boolean
              (This.Tuple, Reqd_Column, Replace_Null, Replacement);
         end;
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_Boolean;

   ----------------
   -- Get_Driver --
   ----------------

   function Get_Driver
     (This              : in Connection) return DB.Driver.Driver_Handle
   is
   begin
      return This.Driver;
   end Get_Driver;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer
     (This              : in Result_Set;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Integer := 0)
     return DB.Types.DB_Integer
   is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         return This.Results.Data.Get_Data_Integer
           (This.Tuple, Column, Replace_Null, Replacement);
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_Integer;

   function Get_Integer
     (This              : in Result_Set;
      Column            : in String;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Integer := 0)
     return DB.Types.DB_Integer
   is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         declare
            Reqd_Column : constant Column_Index :=
              This.Results.Data.Find_Column_By_Name (Column);
         begin
            return This.Results.Data.Get_Data_Integer
              (This.Tuple, Reqd_Column, Replace_Null, Replacement);
         end;
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_Integer;

   -----------------
   -- Get_Is_Null --
   -----------------

   function Get_Is_Null
     (This              : in Result_Set;
      Column            : in Column_Index) return Boolean
   is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         return This.Results.Data.Get_Data_Is_Null (This.Tuple, Column);
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_Is_Null;

   function Get_Is_Null
     (This              : in Result_Set;
      Column            : in String) return Boolean
   is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         declare
            Reqd_Column : constant Column_Index :=
              This.Results.Data.Find_Column_By_Name (Column);
         begin
            return This.Results.Data.Get_Data_Is_Null
              (This.Tuple, Reqd_Column);
         end;
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_Is_Null;

   -------------------
   -- Get_Object_Id --
   -------------------

   function Get_Object_Id
     (This              : in Result_Set;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.Object_Id := 0)
     return DB.Types.Object_Id
   is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         return This.Results.Data.Get_Data_Object_Id
           (This.Tuple, Column, Replace_Null, Replacement);
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_Object_Id;

   function Get_Object_Id
     (This              : in Result_Set;
      Column            : in String;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.Object_Id := 0)
     return DB.Types.Object_Id
   is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         declare
            Reqd_Column : constant Column_Index :=
              This.Results.Data.Find_Column_By_Name (Column);
         begin
            return This.Results.Data.Get_Data_Object_Id
              (This.Tuple, Reqd_Column, Replace_Null, Replacement);
         end;
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_Object_Id;

   ------------------
   -- Get_Smallint --
   ------------------

   function Get_Smallint
     (This              : in Result_Set;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Smallint := 0)
     return DB.Types.DB_Smallint
   is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         return This.Results.Data.Get_Data_Smallint
           (This.Tuple, Column, Replace_Null, Replacement);
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_Smallint;

   function Get_Smallint
     (This              : in Result_Set;
      Column            : in String;
      Replace_Null      : in Boolean := False;
      Replacement       : in DB.Types.DB_Smallint := 0)
     return DB.Types.DB_Smallint
   is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         declare
            Reqd_Column : constant Column_Index :=
              This.Results.Data.Find_Column_By_Name (Column);
         begin
            return This.Results.Data.Get_Data_Smallint
              (This.Tuple, Reqd_Column, Replace_Null, Replacement);
         end;
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_Smallint;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (This              : in Result_Set;
      Column            : in Column_Index;
      Replace_Null      : in Boolean := False;
      Replacement       : in String := "") return DB.Types.DB_String
   is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         return This.Results.Data.Get_Data_String
           (This.Tuple, Column, Replace_Null, Replacement);
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_String;

   function Get_String
     (This              : in Result_Set;
      Column            : in String;
      Replace_Null      : in Boolean := False;
      Replacement       : in String := "") return DB.Types.DB_String
   is
   begin
      if This.Results /= null and then This.Results.Data /= null then
         declare
            Reqd_Column : constant Column_Index :=
              This.Results.Data.Find_Column_By_Name (Column);
         begin
            return This.Results.Data.Get_Data_String
              (This.Tuple, Reqd_Column, Replace_Null, Replacement);
         end;
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Get_String;

   ---------------
   -- Get_Tuple --
   ---------------

   function Get_Tuple (This : in Result_Set) return Tuple_Index is
   begin
      return This.Tuple;
   end Get_Tuple;

   ----------------
   -- Jump_Tuple --
   ----------------

   procedure Jump_Tuple
     (This              : in out Result_Set;
      Tuple             : in     Tuple_Index)
   is
      Tuple_Count       : constant Tuple_Index :=
        Tuple_Index (This.Results.Data.Get_Tuple_Count);
   begin
      if Tuple = This.Tuple + 1 then
         Next_Tuple (This);
      elsif Tuple = This.Tuple - 1 then
         Previous_Tuple (This);
      else
         if This.Results.Data.Is_Random_Access then
            declare
               Tuple_Count : constant Tuple_Index :=
                 Tuple_Index (This.Results.Data.Get_Tuple_Count);
            begin
               if Tuple >= 1 and then Tuple < Tuple_Count then
                  This.Tuple := Tuple;
               else
                  raise DB.Errors.END_OF_RESULT_SET;
               end if;
            end;
         else
            raise DB.Errors.NOT_RANDOM_ACCESS;
         end if;
      end if;
   end Jump_Tuple;

   ----------------
   -- Next_Tuple --
   ----------------

   procedure Next_Tuple (This : in out Result_Set) is
      Tuple_Count       : constant Tuple_Index :=
        Tuple_Index (This.Results.Data.Get_Tuple_Count);
   begin
      if This.Tuple /= INVALID_TUPLE and then This.Tuple < Tuple_Count then
         This.Tuple := This.Tuple + 1;
      else
         raise DB.Errors.END_OF_RESULT_SET;
      end if;
   end Next_Tuple;

   --------------------
   -- Previous_Tuple --
   --------------------

   procedure Previous_Tuple (This : in out Result_Set) is
      Tuple_Count       : constant Tuple_Index :=
        Tuple_Index (This.Results.Data.Get_Tuple_Count);
   begin
      if This.Results.Data.Is_Random_Access then
         if This.Tuple /= INVALID_TUPLE and then This.Tuple > 1 then
            This.Tuple := This.Tuple - 1;
         else
            raise DB.Errors.END_OF_RESULT_SET;
         end if;
      else
         raise DB.Errors.NOT_RANDOM_ACCESS;
      end if;
   end Previous_Tuple;

end DB.Connector;

