--
--  (c) Copyright 2011, John Vinters
--
--  This library is free software; you can redistribute it and/or 
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
--    db.ads   jvinters   15-January-2011
--

with Ada.Unchecked_Deallocation;

package DB is

   type Column_Index is new Natural;
   type Tuple_Index is new Natural;

   type String_Access is access all String;

   procedure Free_String is new Ada.Unchecked_Deallocation
     (String, String_Access);

   INVALID_COLUMN       : constant Column_Index := 0;
   INVALID_TUPLE        : constant Tuple_Index := 0;
   MAX_DRIVERS          : constant := 16;

end DB;

