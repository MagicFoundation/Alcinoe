(*
 *    Program type:  API Interface
 *
 *    Desription:
 *        This program demonstrates the reallocation of SQLDA
 *        and the 'isc_dsql_describe' statement.  After a query
 *        is examined with 'isc_dsql_describe', an SQLDA of correct
 *        size is reallocated, and some information is printed about
 *        the query:  its type (select, non-select), the number
 *        of columns, etc.
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

program api5;

uses
  SysUtils,
  uibase,
  uibLib;

{$APPTYPE CONSOLE}

const
  SQL_DIALECT = 3;
  EMP_DB_PATH = 'C:\Program Files\Firebird\examples\employee.fdb';

const
  sel_str = 'SELECT department, mngr_no, location, head_dept' + #13#10 +
            'FROM department WHERE head_dept in (''100'', ''900'', ''600'')';

var
  DB     : IscDbHandle = nil;         // database handle
  trans  : IscTrHandle  = nil;        // transaction handle

  i     : integer;
  stmt  : IscStmtHandle = nil;
  sqlda : TSQLResult;
  empdb : string;

  UL: TUIBLibrary;

begin
  if (ParamCount > 1) then
    empdb := ParamStr(1)
  else
    empdb := EMP_DB_PATH;

  UL := TUIBLibrary.Create;
  try
    UL.Load;
    UL.AttachDatabase(empdb, DB, 'user_name=SYSDBA;password=masterkey');

    (* Allocate SQLDA of an arbitrary size. *)
    sqlda := TSQLResult.Create;
    try
      UL.TransactionStart(trans,DB);

      (* Allocate a statement. *)
      UL.DSQLAllocateStatement(DB, stmt);

      (* Prepare the statement. *)
      UL.DSQLPrepare(DB, trans, stmt, sel_str, SQL_DIALECT, sqlda);  // automaticaly change sqlda size  and describe if necessary

      (* This is a select statement, print more information about it. *)

      WriteLn('Query Type:  SELECT');
      WriteLn(Format('Number of columns selected:  %d', [sqlda.FieldCount]));

      (* List column names, types, and lengths. *)
      for i := 0 to sqlda.FieldCount - 1 do
      begin
        WriteLn(Format('Column SQL name  : %s', [sqlda.sqlname[i]]));
        WriteLn(Format('Column Alias name: %s', [sqlda.AliasName[i]]));
        WriteLn(Format('Column Rel name  : %s', [sqlda.RelName[i]]));
        WriteLn(Format('Column Own name  : %s', [sqlda.OwnName[i]]));
        WriteLn(Format('Column Type      : %d', [sqlda.sqltype[i]]));
        WriteLn(Format('Column Length    : %d', [sqlda.sqllen[i]]));
        WriteLn('');
      end;

      UL.DSQLFreeStatement(stmt, DSQL_drop);

      UL.TransactionCommit(trans);
      UL.DetachDatabase(DB);
    finally
      sqlda.free;
    end;
  finally
    FreeAndNil(UL);
  end;

  ReadLn;
end.
