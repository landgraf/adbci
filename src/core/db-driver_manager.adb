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
--    db-driver_manager.adb   jvinters   15-January-2011
--

with DB.Errors;

package body DB.Driver_Manager is

   use type DB.Driver.Driver_Handle;

   type Driver_Record is record
      Name              : String_Access := null;
      Alloc_Func        : DB.Driver.Allocate_Function := null;
   end record;

   Driver_Registry      : Array (1 .. MAX_DRIVERS) of Driver_Record;
   Num_Drivers          : Natural := 0;

   -----------------
   -- Free_Driver --
   -----------------

   procedure Free_Driver (This : in out DB.Driver.Driver_Handle) is
   begin
      if This /= null then
         This.Disconnect;
      end if;
      DB.Driver.Free_Driver (This);
   end Free_Driver;

   ----------------
   -- Get_Driver --
   ----------------

   function Get_Driver (Name : in String) return DB.Driver.Driver_Handle is
   begin
      for i in 1 .. Num_Drivers loop
         if Driver_Registry (i).Name.all = Name then
            return Driver_Registry (i).Alloc_Func.all;
         end if;
      end loop;
      raise DB.Errors.NO_DRIVER with "driver '" & Name & "' not found";
   end Get_Driver;

   ---------------------
   -- Register_Driver --
   ---------------------

   procedure Register_Driver
     (Name              : in String;
      Alloc_Func        : in DB.Driver.Allocate_Function)
   is
   begin
      if Num_Drivers >= MAX_DRIVERS then
         raise STORAGE_ERROR with "can't register driver -- too many drivers";
      else
         Num_Drivers := Num_Drivers + 1;
         Driver_Registry (Num_Drivers).Name := new String'(Name);
         Driver_Registry (Num_Drivers).Alloc_Func := Alloc_Func;
      end if;
   end Register_Driver;

end DB.Driver_Manager;

