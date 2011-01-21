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
--    customer.adb   jvinters   17-January-2011
--

package body Customer is

   ---------------------------
   -- Iterate_Custom_Fields --
   ---------------------------

   procedure Iterate_Custom_Fields
     (This           : in out Customer_Model;
      Handler        : not null access Procedure
                         (Field : in out DB.Active_Record.Fields.Field'Class))
   is
   begin
      Handler.all (This.Customer_Name);
      Handler.all (This.Address_1);
      Handler.all (This.Address_2);
      Handler.all (This.Address_3);
      Handler.all (This.County);
      Handler.all (This.Postcode);
      Handler.all (This.Country);
      Handler.all (This.Phone);
      Handler.all (This.No_Marketing);
      Handler.all (This.Last_Updated);
      Handler.all (This.Credit_Limit);
   end Iterate_Custom_Fields;

end Customer;

