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
--    db-driver_manager.ads   jvinters   15-January-2011
--

with DB.Driver;

package DB.Driver_Manager is

   procedure Free_Driver (This : in out DB.Driver.Driver_Handle);
   --  Frees storage used by driver.

   function Get_Driver (Name : in String) return DB.Driver.Driver_Handle;
   --  Gets a driver by name, or raises an exception.

   procedure Register_Driver
     (Name              : in String;
      Alloc_Func        : in DB.Driver.Allocate_Function);
   --  Registers a new driver.

end DB.Driver_Manager;

