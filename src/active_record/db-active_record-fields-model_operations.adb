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
--    db-active_record-fields-model_operations.ads   jvinters   16-January-2011
--

package body DB.Active_Record.Fields.Model_Operations is

   --------------------
   -- Set_Field_Name --
   --------------------

   procedure Set_Field_Name
     (This              : in out Field'Class;
      Field_Name        : in     String)
   is
   begin
      Set_Unbounded_String (This.Field_Name, Field_Name);
   end Set_Field_Name;

   --------------------
   -- Set_Model_Name --
   --------------------

   procedure Set_Model_Name
     (This              : in out Field'Class;
      Model_Name        : in     String)
   is
   begin
      Set_Unbounded_String (This.Model_Name, Model_Name);
   end Set_Model_Name;

end DB.Active_Record.Fields.Model_Operations;

