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
--    customer.ads   jvinters   17-January-2011
--

with DB.Active_Record.Fields;          use DB.Active_Record.Fields;
with DB.Active_Record.Models;

package Customer is

   type Customer_Model is new DB.Active_Record.Models.Model with record
     Customer_Name   : String_Field := Configure
       (Name => "customer_name", Maximum_Length => 64, Not_Null => True, Unique => True);
     Address_1       : String_Field := Configure
       (Name => "address_1", Maximum_Length => 32, Has_Default => False);
     Address_2       : String_Field := Configure
       (Name => "address_2", Maximum_Length => 32, Has_Default => False);
     Address_3       : String_Field := Configure
       (Name => "address_3", Maximum_Length => 32, Has_Default => False);
     County          : String_Field := Configure
       (Name => "county", Maximum_Length => 48, Has_Default => False);
     Postcode        : String_Field := Configure
       (Name => "postcode", Maximum_Length => 16, Has_Default => False);
     Country         : String_Field := Configure
       (Name => "country", Maximum_Length => 32, Default_Value => "UK");
     Phone           : String_Field := Configure
       (Name => "phone", Maximum_Length => 32, Has_Default => False);
     No_Marketing    : Boolean_Field := Configure
       (Name => "no_marketing", Default_Value => False);
     Last_Updated    : Timestamp_Field := Configure
       (Name => "last_updated", Auto_Now => True);
   end record;

   overriding procedure Iterate_Custom_Fields
     (This           : in out Customer_Model;
      Handler        : not null access Procedure
                         (Field : in out DB.Active_Record.Fields.Field'Class));

end Customer;

