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
--    db-types.ads   jvinters   15-January-2011
--

package DB.Types is

   type DB_Smallint is range -2**15 .. 2**15 - 1;
   type DB_Integer is range -2**31 .. 2**31 - 1;
   type DB_Bigint is range -2**63 .. 2**63 - 1;

   type DB_Serial is range 0 .. 2**31 - 1;
   Invalid_Serial       : constant DB_Serial := 0;

   type DB_Bigserial is range 0 .. 2**63 - 1;
   Invalid_Bigserial    : constant DB_Bigserial := 0;

   subtype Object_Id is DB_Bigserial;

   type DB_String is new String;
   type SQL_String is new String;

   type Field_Type is
     (BIGINT,
      BIGSERIAL,
      BOOLEAN,
      INTEGER,
      SERIAL,
      SMALLINT,
      TEXT,
      VARCHAR,
      UNKNOWN);

end DB.Types;

