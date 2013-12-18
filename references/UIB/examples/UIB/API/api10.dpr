(*
 *    Program type:  API
 *
 *    Description:
 *        This program selects and updates an array type.
 *        Projected head count is displayed and updated for
 *        a set of projects.
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
 *)

program api10;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uibase,
  uibLib;

const
  SQL_DIALECT = 3;
  EMP_DB_PATH = 'C:\Program Files\Firebird\examples\employee.fdb';

const
  sel_str =
    'SELECT dept_no, quart_head_cnt FROM proj_dept_budget p' + #13#10 +
    'WHERE fiscal_year = 1994 AND proj_id = ''VBASE'''+ #13#10 +
    'FOR UPDATE of quart_head_cnt';

  upd_str =
    'UPDATE proj_dept_budget SET' + #13#10 +
    '  quart_head_cnt = ?' + #13#10 +
    'WHERE CURRENT OF S';

var
  hcnt     : array[0..3] of Integer;
  desc     : TArrayDesc;
  len      : Integer;
  DB       : IscDbHandle   = nil;
  trans    : IscTrHandle   = nil;
  stmt     : IscStmtHandle = nil;
  ustmt    : IscStmtHandle = nil;
  cursor   : string = 'S';
  osqlda   : TSQLResult;
  isqlda   : TSQLParams;
  i        : Smallint;
  empdb    : string;
  arrayID  : GDS_QUAD;

  UL: TUIBLibrary;

begin
  if (ParamCount > 1) then
    empdb := ParamStr(1)
  else
    empdb := EMP_DB_PATH;

  UL := TUIBLIbrary.Create;
  try
    UL.Load;

    UL.AttachDatabase(empdb, DB, 'user_name=SYSDBA;password=masterkey');

    UL.TransactionStart(trans, DB);

    // Set up the array description structure
    desc := UL.ArrayLookupBounds(DB, trans, 'PROJ_DEPT_BUDGET', 'QUART_HEAD_CNT');

    // Set-up the select statement.
    osqlda := TSQLResult.Create(2);
    // Set-up the update statement.
    isqlda := TSQLParams.Create;

    try
      UL.DSQLAllocateStatement(DB, stmt);
      UL.DSQLAllocateStatement(DB, ustmt);

      (* Prepare and execute query *)
      UL.DSQLPrepare(DB, trans, stmt, sel_str, SQL_DIALECT, osqlda);
      UL.DSQLExecute(trans, stmt, SQL_DIALECT);

      (* Needed for update current *)
      UL.DSQLSetCursorName(stmt, cursor);

      (* Use describe_bind to set up input sqlda *)
      UL.DSQLPrepare(DB, trans, ustmt, upd_str, SQL_DIALECT);

      isqlda.AddFieldType('ARRAY',uftQuad);

      UL.DSQLDescribeBind(ustmt, SQL_DIALECT, isqlda);

      (*
       *    Fetch the head count for each department's 4 quarters;
       *    increase the head count by 1 for each quarter;
       *    and save the new head count.
       *)

      while UL.DSQLFetch(DB, trans, stmt, SQL_DIALECT, osqlda) do
      begin
        (* Get the current array values. *)
        if not osqlda.IsNull[1] then
        begin
          len := sizeof(hcnt);
          UL.ArrayGetSlice(DB, trans, osqlda.AsQuad[1], desc, @hcnt, len);
          //dept_no [osqlda->sqlvar[0].sqllen] = '\0';
          WriteLn(Format('Department #:  %s', [osqlda.AsString[0]]));
          WriteLn(Format('Current counts: %d %d %d %d', [hcnt[0], hcnt[1], hcnt[2], hcnt[3]]));

          (* Add 1 to each count. *)
          for i := 0 to 3 do
              hcnt[i] := hcnt[i] + 1;

          isqlda.AsQuad[0] := osqlda.AsQuad[1];

          (* Save new array values. *)

          arrayID := isqlda.AsQuad[0];
          UL.ArrayPutSlice(DB, trans, arrayID, desc, @hcnt, len);

          (* Update the array handle. *)
          UL.DSQLExecute(trans, ustmt, SQL_DIALECT, isqlda);

          WriteLn(Format('New counts    : %d %d %d %d', [hcnt[0], hcnt[1], hcnt[2], hcnt[3]]));
        end;
      end;

      UL.DSQLFreeStatement(stmt, DSQL_close);
      UL.DSQLFreeStatement(ustmt, 0);

      (* Do a rollback to keep from updating the sample db *)

      UL.TransactionRollback(trans);
      //UL.TransactionCommit(trans);

      UL.DetachDatabase(DB);
    finally
      osqlda.Free;
      isqlda.Free;
    end;
  finally
    FreeAndNil(UL);
  end;

  ReadLn;
end.
