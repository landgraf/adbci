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
--    slice_test.adb   jvinters   23-January-2011
--

with Ada.Text_IO;                      use Ada.Text_IO;
with DB.Active_Record.Models.Queries;
with DB.Types;                         use DB.Types;

package body Slice_Test is

   package Slice_Query is new DB.Active_Record.Models.Queries
     (Model_Type => Slice_Example);

   -----------------
   -- Drop_Tables --
   -----------------

   procedure Drop_Tables (Database : in out DB.Connector.Connection) is
   begin
      Slice_Query.Object.Drop (Database);
   end Drop_Tables;

   ---------------------------
   -- Iterate_Custom_Fields --
   ---------------------------

   procedure Iterate_Custom_Fields
     (This           : in out Slice_Example;
      Handler        : not null access Procedure
                         (Field : in out DB.Active_Record.Fields.Field'Class))
   is
   begin
      Handler.all (This.Created);
   end Iterate_Custom_Fields;

   ---------
   -- Run --
   ---------

   procedure Run
     (Database       : in out DB.Connector.Connection)
   is
   begin
      Setup_Tables (Database);
      Run_Test (Database);
      Drop_Tables (Database);
   end Run;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Database : in out DB.Connector.Connection) is
      QR : Slice_Query.Query_Result;
      SQ : Slice_Query.Query_Result;
      S2 : Slice_Query.Query_Result;
      NC : DB.Active_Record.Fields.Field_Criteria;
      I  : Slice_Example;
   begin
      Put ("Getting all of the entries... ");
      QR := Slice_Query.Find (Database, NC);
      if Slice_Query.Count (QR) /= 100 then
         Put_Line ("FAILED");
         return;
      else
         Put_Line ("OK");
      end if;

      Put ("Checking first and last ids... ");
      I := Slice_Query.Item (QR, Database, 1);
      if I.Get_Id /= 1 then
         Put_Line ("FAILED");
         return;
      end if;
      I := Slice_Query.Item (QR, Database, 100);
      if I.Get_Id /= 100 then
         Put_Line ("FAILED");
         return;
      end if;
      Put_Line ("OK");

      Put ("Slicing results 10..89... ");
      SQ := Slice_Query.Slice (QR, 10, 89);
      if Slice_Query.Count (SQ) /= 80 then
         Put_Line ("FAILED");
         return;
      else
         Put_Line ("OK");
      end if;

      Put ("Checking first and last ids... ");
      I := Slice_Query.Item (SQ, Database, 1);
      if I.Get_Id /= 10 then
         Put_Line ("FAILED");
         return;
      end if;
      I := Slice_Query.Item (SQ, Database, 80);
      if I.Get_Id /= 89 then
         Put_Line ("FAILED");
         return;
      end if;
      Put_Line ("OK");

      Put ("Sub-Slicing results 1..2... ");
      S2 := Slice_Query.Slice (SQ, 1, 2);
      if Slice_Query.Count (S2) /= 2 then
         Put_Line ("FAILED");
         return;
      else
         Put_Line ("OK");
      end if;

      Put ("Checking first and last ids... ");
      I := Slice_Query.Item (S2, Database, 1);
      if I.Get_Id /= 10 then
         Put_Line ("FAILED");
         return;
      end if;
      I := Slice_Query.Item (S2, Database, 2);
      if I.Get_Id /= 11 then
         Put_Line ("FAILED");
         return;
      end if;
      Put_Line ("OK");

      Put ("Checking out of bound fails... ");
      begin
         I := Slice_Query.Item (S2, Database, 3);
         Put_Line ("FAILED");
         return;
      exception
         when CONSTRAINT_ERROR =>
            Put_Line ("OK");
      end;

      Put ("Checking subset search works...");
      S2 := Slice_Query.Find (Database, NC, First => 1, Last => 10);
      if Slice_Query.Count (S2) /= 10 then
         Put_Line ("FAILED");
         return;
      else
         Put_Line ("OK");
      end if;
   end Run_Test;

   ------------------
   -- Setup_Tables --
   ------------------

   procedure Setup_Tables (Database : in out DB.Connector.Connection) is
      Temp           : Slice_Example;
   begin
      begin
         Temp.Drop (Database);
      exception
         when others =>
            null;
      end;
      Temp.Create (Database);

      Put ("Creating slice table entries...");
      for i in 1 .. 100 loop
         Temp.Save (Database);
         Temp.Clear;
      end loop;
      Put_Line ("Done");
   end Setup_Tables;

end Slice_Test;

