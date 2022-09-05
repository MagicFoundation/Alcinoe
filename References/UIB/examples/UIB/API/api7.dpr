(*
 *  Program type:  API Interface
 *
 *    Description:
 *      This program selects a blob data type.
 *      A set of project descriptions is printed.
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
 
program api7;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  uibase,
  uibLib;

const
  SQL_DIALECT = 3;
  EMP_DB_PATH = 'C:\Program Files\Firebird\examples\employee.fdb';

var
  sel_str      : string;
  blob_handle  : IscBlobHandle = nil;
  DB           : IscDbHandle = nil;    // database handle
  trans        : IscTrHandle = nil;    // transaction handle
  stmt         : IscStmtHandle = nil;  // statement handle
  sqlda        : TSQLResult;
  empdb        : string;
  str: string;

  UL: TUIBLibrary;

begin
  if (ParamCount > 1) then
    empdb := ParamStr(1)
  else
    empdb := EMP_DB_PATH;

  sel_str := 'SELECT proj_name, proj_desc, product' + #13#10 +
             'FROM project' + #13#10 +
             'WHERE product IN (''software'', ''hardware'', ''other'')' + #13#10 +
             'ORDER BY proj_name';

  UL := TUIBLibrary.Create;
  try
    UL.Load;
    UL.AttachDatabase(empdb, DB, 'user_name=SYSDBA;password=masterkey');

    UL.TransactionStart(trans, DB);

    UL.DSQLAllocateStatement(DB, stmt);

    sqlda := TSQLResult.Create(3);

    try
      UL.DSQLPrepare(DB, trans, stmt, sel_str, SQL_DIALECT, sqlda);

      UL.DSQLExecute(trans, stmt, SQL_DIALECT);

      (*
       *    For each project in the select statement, get and display
       *    project descriptions.
       *)

      while UL.DSQLFetch(DB, trans, stmt, SQL_DIALECT, sqlda) do //sqlda
      begin
        WriteLn(Format('PROJECT:  %s   TYPE:  %s', [sqlda.AsString[0], sqlda.AsString[2]]));
        UL.BlobOpen(DB, trans, blob_handle, sqlda.AsQuad[1]);
        str := UL.BlobReadString(blob_handle);
        WriteLn(str);
        UL.BlobClose(blob_handle);
      end;
      UL.DSQLFreeStatement(stmt, DSQL_close);
      UL.TransactionCommit(trans);
      UL.DetachDatabase(DB);
    finally
      sqlda.Free;
    end;
  finally
    FreeAndNil(UL);
  end;

  ReadLn;
end.
