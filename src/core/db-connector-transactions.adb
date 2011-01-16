--
--  (c) Copyright 2011, John Vinters
--
--  ADBC is free software; you can redistribute it and/or 
--  modify it under the terms of the GNU Lesser General Public License 
--  as published by the Free Software Foundation; either version 3, or 
--  (at your option) any later version.  
--
--  ADBC is distributed in the hope that it will be useful, but WITHOUT ANY 
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
--  FOR A PARTICULAR PURPOSE.
--
--  You should have received a copy of the GNU Lesser General Public License
--  along with this library; if not, see <http://www.gnu.org/licenses/>
--
--    db-connector-transactions.adb   jvinters   16-January-2011
--

with DB.Errors;

package body DB.Connector.Transactions is

   -----------------------
   -- Begin_Transaction --
   -----------------------

   procedure Begin_Transaction (This : in out Connection'Class) is
   begin
      if This.In_Transaction then
         raise DB.Errors.TRANSACTION_ERROR with "already in transaction";
      else
         declare
            Temp        : Result_Set := This.Execute ("BEGIN");
         begin
            This.In_Transaction := True;
         end;
      end if;
   end Begin_Transaction;

   ------------------------
   -- Commit_Transaction --
   ------------------------

   procedure Commit_Transaction (This : in out Connection'Class) is
   begin
      if not This.In_Transaction then
         raise DB.Errors.TRANSACTION_ERROR with "not in transaction";
      else
         This.In_Transaction := False;

         declare
            Temp        : Result_Set := This.Execute ("COMMIT");
         begin
            null;
         end;
      end if;
   end Commit_Transaction;

   --------------------------
   -- Rollback_Transaction --
   --------------------------

   procedure Rollback_Transaction (This : in out Connection'Class) is
   begin
      if not This.In_Transaction then
         raise DB.Errors.TRANSACTION_ERROR with "not in transaction";
      else
         This.In_Transaction := False;

         declare
            Temp        : Result_Set := This.Execute ("ROLLBACK");
         begin
            null;
         end;
      end if;
   end Rollback_Transaction;

end DB.Connector.Transactions;

