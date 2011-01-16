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
--    db-active_record-fields.adb   jvinters   16-January-2011
--

package body DB.Active_Record.Fields is

   ----------------------
   -- Get_Display_Name --
   ----------------------

   function Get_Display_Name (This : in Field'Class) return String is
   begin
      return To_String (This.Display_Name);
   end Get_Display_Name;

   --------------------
   -- Get_Model_Name --
   --------------------

   function Get_Model_Name (This : in Field'Class) return String is
   begin
      return To_String (This.Model_Name);
   end Get_Model_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (This : in Field'Class) return String is
   begin
      return To_String (This.Field_Name);
   end Get_Name;

   ----------------
   -- Is_Changed --
   ----------------

   function Is_Changed (This : in Field'Class) return Boolean is
   begin
      return This.Changed;
   end Is_Changed;

end DB.Active_Record.Fields;

