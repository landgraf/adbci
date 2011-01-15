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
--    db-errors.ads   jvinters   15-January-2011
--

package DB.Errors is

   COLUMN_NOT_FOUND     : Exception;   --  column (field) couldn't be found
   COLUMN_IS_NULL       : Exception;   --  column data is null
   CONNECT_ERROR        : Exception;   --  problem connecting to database
   END_OF_RESULT_SET    : Exception;   --  reached end of results
   INTEGRITY_ERROR      : Exception;   --  constraint violation (usually)
   NO_DRIVER            : Exception;   --  driver not found
   NOT_CONNECTED        : Exception;   --  not connected to database
   NOT_RANDOM_ACCESS    : Exception;   --  result set isn't random access
   NOT_SUPPORTED        : Exception;   --  operation not supported
   OBJECT_NOT_FOUND     : Exception;   --  object not found
   OBJECT_READ_ONLY     : Exception;   --  object is write protected
   SQL_ERROR            : Exception;   --  SQL syntax error
   TOO_MANY_RESULTS     : Exception;   --  too many open results
   TUPLE_NOT_FOUND      : Exception;   --  tuple (row) not found
   VALIDATION_ERROR     : Exception;   --  validation failed

end DB.Errors;

