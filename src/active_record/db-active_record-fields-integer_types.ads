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
--    db-active_record-fields-generic_fixed.ads   jvinters   21-January-2011
--

with DB.Active_Record.Fields.Generic_Integer;

pragma Elaborate_All (DB.Active_Record.Fields.Generic_Integer);

package DB.Active_Record.Fields.Integer_Types is

   package Bigint is new DB.Active_Record.Fields.Generic_Integer
     (Integer_Type         => DB.Types.DB_Bigint,
      Initialization_Value => 0);

   package Int is new DB.Active_Record.Fields.Generic_Integer
     (Integer_Type         => DB.Types.DB_Integer,
      Initialization_Value => 0);

   package Smallint is new DB.Active_Record.Fields.Generic_Integer
     (Integer_Type         => DB.Types.DB_Smallint,
      Initialization_Value => 0);

end DB.Active_Record.Fields.Integer_Types;
