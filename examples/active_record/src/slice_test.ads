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
--    slice_test.ads   jvinters   23-January-2011
--

with DB.Active_Record.Fields;          use DB.Active_Record.Fields;
with DB.Active_Record.Fields.Date_Time_Types;
                                       use DB.Active_Record.Fields.Date_Time_Types;
with DB.Active_Record.Models;
with DB.Connector;

package Slice_Test is

   type Slice_Example is new DB.Active_Record.Models.Model with record
      Created        : Timestamp.Field := Timestamp.Configure
        (Name => "created", Auto_Now => True, Not_Null => True);
   end record;         

   overriding procedure Iterate_Custom_Fields
     (This           : in out Slice_Example;
      Handler        : not null access Procedure
                         (Field : in out DB.Active_Record.Fields.Field'Class));

   procedure Run
     (Database       : in out DB.Connector.Connection);
   --  Runs the test.

private

   procedure Run_Test (Database : in out DB.Connector.Connection);
   --  Runs the test of the prepared table.

   procedure Setup_Tables (Database : in out DB.Connector.Connection);
   --  Sets up required tables.

end Slice_Test;

