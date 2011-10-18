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
--    db-connector-transactions.ads   jvinters   16-January-2011
--

package DB.Connector.Transactions is

   procedure Begin_Transaction (This : in out Connection'Class);
   --  Starts a transaction.

   procedure Commit_Transaction (This : in out Connection'Class);
   --  Commits a transaction.

   procedure Error_Rollback (This : in out Connection'Class);
   --  Performs a rollback if a transaction is in progress, but doesn't raise
   --  an exception if there isn't -- useful for error handling.

   function In_Transaction (This : in Connection'Class) return Boolean;
   --  Returns true if we are currently in a transaction.

   procedure Rollback_Transaction (This : in out Connection'Class);
   --  Rolls back a transaction.

end DB.Connector.Transactions;

