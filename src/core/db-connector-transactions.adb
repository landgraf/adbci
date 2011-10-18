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
--    db-connector-transactions.adb   jvinters   16-January-2011
--

with DB.Errors;

package body DB.Connector.Transactions is

   -----------------------
   -- Begin_Transaction --
   -----------------------

   procedure Begin_Transaction (This : in out Connection'Class) is
   begin
      if This.Data.all.In_Transaction then
         raise DB.Errors.TRANSACTION_ERROR with "already in transaction";
      else
         declare
            Temp        : Result_Set;
            pragma Unreferenced (Temp);
         begin
            This.Data.all.In_Transaction := True;
            Temp := This.Execute ("BEGIN");
         end;
      end if;
   end Begin_Transaction;

   ------------------------
   -- Commit_Transaction --
   ------------------------

   procedure Commit_Transaction (This : in out Connection'Class) is
   begin
      if not This.Data.all.In_Transaction then
         raise DB.Errors.TRANSACTION_ERROR with "not in transaction";
      else
         This.Data.all.In_Transaction := False;

         declare
            Temp        : Result_Set;
            pragma Unreferenced (Temp);
         begin
            Temp := This.Execute ("COMMIT");
         end;
      end if;
   end Commit_Transaction;

   --------------------
   -- Error_Rollback --
   --------------------

   procedure Error_Rollback (This : in out Connection'Class) is
   begin
      if This.Data.all.In_Transaction then
         Rollback_Transaction (This);
      end if;
   end Error_Rollback;

   --------------------
   -- In_Transaction --
   --------------------

   function In_Transaction (This : in Connection'Class) return Boolean is
   begin
      return This.Data.all.In_Transaction;
   end In_Transaction;

   --------------------------
   -- Rollback_Transaction --
   --------------------------

   procedure Rollback_Transaction (This : in out Connection'Class) is
   begin
      if not This.Data.all.In_Transaction then
         raise DB.Errors.TRANSACTION_ERROR with "not in transaction";
      else
         This.Data.all.In_Transaction := False;

         declare
            Temp        : Result_Set;
            pragma Unreferenced (Temp);
         begin
            Temp := This.Execute ("ROLLBACK");
         end;
      end if;
   end Rollback_Transaction;

end DB.Connector.Transactions;
