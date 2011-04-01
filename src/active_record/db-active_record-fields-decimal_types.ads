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
--    db-active_record-fields-decimal_types.ads   jvinters   21-January-2011
--

with DB.Active_Record.Fields.Generic_Decimal;

pragma Elaborate_All (DB.Active_Record.Fields.Generic_Decimal);

package DB.Active_Record.Fields.Decimal_Types is

   type Currency_Type is delta 0.01 digits 18;
   type Percentage_Type is delta 0.01 digits 5;

   package Currency is new DB.Active_Record.Fields.Generic_Decimal
     (Fixed_Type           => Currency_Type,
      Initialization_Value => 0.0);

   package Percentage is new DB.Active_Record.Fields.Generic_Decimal
     (Fixed_Type           => Percentage_Type,
      Initialization_Value => 0.0);

end DB.Active_Record.Fields.Decimal_Types;
