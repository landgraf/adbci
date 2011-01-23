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
with DB.Active_Record.Fields.Boolean_Type;
                                       use DB.Active_Record.Fields.Boolean_Type;
with DB.Active_Record.Fields.Character_Types;
                                       use DB.Active_Record.Fields.Character_Types;
with DB.Active_Record.Fields.Date_Time_Types;
                                       use DB.Active_Record.Fields.Date_Time_Types;
with DB.Active_Record.Fields.Fixed_Types;
                                       use DB.Active_Record.Fields.Fixed_Types;
with DB.Active_Record.Models;

package Customer is

   type Customer_Model is new DB.Active_Record.Models.Model with record
     Customer_Name   : Varchar.Field := Varchar.Configure
       (Name            => "customer_name",
        Maximum_Length  => 64, 
        Not_Null        => True, 
        Unique          => True);
     Address_1       : Varchar.Field := Varchar.Configure
       (Name            => "address_1", 
        Maximum_Length  => 32, 
        Has_Default     => False);
     Address_2       : Varchar.Field := Varchar.Configure
       (Name            => "address_2", 
        Maximum_Length  => 32, 
        Has_Default     => False);
     Address_3       : Varchar.Field := Varchar.Configure
       (Name            => "address_3", 
        Maximum_Length  => 32, 
        Has_Default     => False);
     County          : Varchar.Field := Varchar.Configure
       (Name            => "county",
        Maximum_Length  => 48, 
        Has_Default     => False);
     Postcode        : Varchar.Field := Varchar.Configure
       (Name            => "postcode",
        Maximum_Length  => 16, 
        Has_Default     => False);
     Country         : Varchar.Field := Varchar.Configure
       (Name            => "country",
        Maximum_Length  => 32,
        Default_Value   => "UK");
     Phone           : Varchar.Field := Varchar.Configure
       (Name            => "phone",
        Maximum_Length  => 32,
        Has_Default     => False);
     No_Marketing    : Bool.Field := Bool.Configure
       (Name            => "no_marketing",
        Default_Value   => False);
     Last_Updated    : Timestamp.Field := Timestamp.Configure
       (Name            => "last_updated",
        Auto_Now        => True);
     Credit_Limit    : Currency.Field := Currency.Configure
       (Name            => "credit_limit",
        Has_Default     => True,
        Default_Value => 100.00);
     Notes           : Text.Field := Text.Configure
       (Name            => "notes",
        Maximum_Length  => 32768);        
   end record;

   overriding procedure Iterate_Custom_Fields
     (This           : in out Customer_Model;
      Handler        : not null access Procedure
                         (Field : in out DB.Active_Record.Fields.Field'Class));

end Customer;

