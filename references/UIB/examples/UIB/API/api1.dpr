(*
 *    Program type:  API Interface
 *
 *    Description:
 *        This program creates a new database, given an SQL statement
 *        string.  The newly created database is accessed after its
 *        creation, and a sample table is added.
 *
 *        The SQLCODE is extracted from the status vector and is used
 *        to check whether the database already exists.
 *
 * The contents of this file are subject to the Interbase Public
 * License Version 1.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy
 * of the License at:
 * http://www.borland.com/devsupport/interbase/opensource/IPL.html
 *
 * Software distributed under the License is distributed on an
 * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
 * or implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code was created by Pierre Yager.
 *
 * Contributor(s):
 *   Borland : Aaron Ruddick InterBase QA, Borland Software Corp.
 *             Dan Mikhayltsa  InterBase QA, Borland Software Corp.
 *   Henri Gourvest.
 *)

program api1;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uibase,
  uibLib;

const
  SQL_DIALECT = 3;

const
  create_tbl  = 'CREATE TABLE dbinfo (when_created DATE)';
  insert_date = 'INSERT INTO dbinfo VALUES (''NOW'')';

var
  newdb  : IscDbHandle = nil;          (* database handle *)
  trans  : IscTrHandle = nil;          (* transaction handle *)
  create_db : string;                  (* 'create database' statement *)
  new_dbname: String;
  UL: TUIBLibrary;

begin
  if (ParamCount > 1) then
    new_dbname := ParamStr(1)
  else
    new_dbname := ExtractFilePath(ParamStr(0))+'new.gdb';

  if FileExists(new_dbname) then
    DeleteFile(new_dbname);

  // Construct a 'create database' statement.
  // The database name could have been passed as a parameter.
  create_db := Format('CREATE DATABASE "%s" USER "SYSDBA" PASSWORD "masterkey"' ,[new_dbname]);

  // Create a new database.
  // The database handle is zero.

  UL := TUIBLibrary.Create;
  try
    UL.Load;
    try
      UL.DSQLExecuteImmediate(newdb, trans, create_db, SQL_DIALECT, nil);
    except
      on E: Exception do
      begin
        WriteLn('Error: ',E.Message);
        ReadLn;
      end;
    end;

    WriteLn(format('Created database ''%s''.',[new_dbname]));

    // Connect to the new database and create a sample table.
    (* newdb will be set to null on success *)
    UL.DetachDatabase(newdb);

    UL.AttachDatabase(new_dbname, newdb, 'user_name = SYSDBA; password = masterkey');

    (* Create a sample table. *)
    UL.TransactionStart(trans, newdb);
    UL.DSQLExecuteImmediate(newdb, trans, create_tbl, SQL_DIALECT, nil);

    //isc_commit_transaction(@status, @trans);
    UL.TransactionCommit(trans);

    (* Insert 1 row into the new table. *)
    UL.TransactionStart(trans, newdb);
    UL.DSQLExecuteImmediate(newdb, trans, insert_date, SQL_DIALECT, nil);
    UL.TransactionCommit(trans);

    WriteLn('Successfully accessed the newly created database.');

    UL.DetachDatabase(newdb);
  finally
    FreeAndNil(UL);
  end;

  ReadLn;
end.

