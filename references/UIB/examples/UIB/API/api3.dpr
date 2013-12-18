(*
 *    Program type:  API Interface
 *
 *    Description:
 *        This program displays employee names and phone extensions.
 *
 *        It allocates an output SQLDA, prepares and executes a statement,
 *        and loops fetching multiple rows.
 *
 *        The SQLCODE returned by fetch is checked.
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

program api3;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uibase,
  uibLib;

const
  SQL_DIALECT = 3;
  EMP_DB_PATH = 'C:\Program Files\Firebird\examples\employee.fdb';

(* This macro is used to declare structures representing SQL VARCHAR types *)
//function SQL_VARCHAR(len) struct {short vary_length; char vary_string[(len)+1];}

var
  stmt: IscStmtHandle = nil; // statement handle
  DB: IscDbHandle = nil; // database handle
  trans: IscTrHandle = nil; // transaction handle
  sqlda: TSQLResult;
  empdb: string;
  
  sel_str: string = 'SELECT last_name, first_name, phone_ext FROM phone_list' + #13#10 +
                    'WHERE location = ''Monterey''' + #13#10 +
                    'ORDER BY last_name, first_name';

  UL: TUIBLibrary;

begin
  if (ParamCount > 1) then
    empdb := paramstr(1)
  else
    empdb := EMP_DB_PATH;

  (* Allocate an output SQLDA. *)
  sqlda := TSQLResult.Create(3);

  UL := TUIBLibrary.Create;
  try
    UL.Load;
    UL.AttachDatabase(empdb, DB, 'user_name=SYSDBA;password=masterkey');

    UL.TransactionStart(trans, DB);

    (* Allocate a statement. *)
    UL.DSQLAllocateStatement(DB, stmt);

    (* Prepare the statement. *)
    try
      UL.DSQLPrepare(DB, trans, stmt, sel_str, SQL_DIALECT, sqlda);
    except
      on E: Exception do
      begin
        WriteLn('Error: ',E.Message);
        ReadLn;
        raise;
      end;
    end;

    (* Execute the statement. *)
    UL.DSQLExecute(trans, stmt, SQL_DIALECT, nil);

    (*
     *    Fetch and print the records.
     *)
    while UL.DSQLFetch(DB, trans, stmt, 1, sqlda) do
      Writeln(format('%s %s %s', [sqlda.AsString[0], sqlda.AsString[1], sqlda.AsString[2]]));

    (* Free statement handle. *)
    UL.DSQLFreeStatement(stmt, DSQL_close);
    UL.TransactionCommit(trans);
    UL.DetachDatabase(DB);
  finally
    sqlda.Free;
    FreeAndNil(UL);
  end;

  Readln;
end.

