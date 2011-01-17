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
--    active_record.adb   jvinters   17-January-2011
--

with Ada.Text_IO;                      use Ada.Text_IO;
with Customer;
with DB.Connector;
with DB.Driver;
with DB.Driver.PostgreSQL;

procedure Active_Record is
   Database             : DB.Connector.Connection :=
     DB.Connector.Connect ("postgresql", "localhost", "adbci", "adbci", "adbci");
   Customer_1           : Customer.Customer_Model;
   Customer_2           : Customer.Customer_Model;
begin
   begin
      Customer_1.Drop (Database);      --  drop the customers table
   exception                           --  and...
      when others =>                   --  silently swallow any exceptions
         null;
   end;

   Customer_1.Create (Database);       --  create the customer table

   --  Create an example customer...
   Customer_1.Customer_Name.Set ("An example customer");
   Customer_1.Address_1.Set ("Address Line 1");
   Customer_1.Address_2.Set ("Address Line 2");
   Customer_1.Address_3.Set ("Address Line 3");
   Customer_1.Postcode.Set ("Postcode");
   Customer_1.Save (Database);

   Put_Line ("Customer record was inserted with id: " & Customer_1.Get_Id);

   --  Fetch customer using the Id
   Customer_2.Get (Database, Customer_1.Get_Id);
   Put_Line ("Customer Name: " & Customer_2.Customer_Name.Get);

   --  Now make a change, and update the record...
   Customer_2.Customer_Name.Set ("This is the New Customer Name");
   Customer_2.Save (Database);

   --  Fetch again, and show the result...
   Customer_1.Get (Database, Customer_2.Get_Id);
   Put_Line ("Customer Name: " & Customer_1.Customer_Name.Get);

   --  Now delete the customer from the database...
   Customer_1.Delete (Database);

   --  Finished, so disconnect.
   Database.Disconnect;
end Active_Record;