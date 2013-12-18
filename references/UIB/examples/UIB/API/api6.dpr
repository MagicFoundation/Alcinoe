(*
 *    Program type:  API Interface
 *
 *    Description:
 *        This program performs a positioned update.
 *        Department budgets are examined and updated using some
 *        percent increase factor, determined at run-time.
 *
 *        The update statement is constructed using a dynamic cursor
 *        name.  The statement handle is freed and re-used by the
 *        update cursor, after being used by another statement.
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
 
program api6;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uibase,
  uibLib;

const
  SQL_DIALECT = 3;
  EMP_DB_PATH = 'C:\Program Files\Firebird\examples\employee.fdb';

(*
 *    Determine a percent increase for the department's budget.
 *)
function increase_factor(budget: Double): single;
begin
  if (budget < 100000) then result := 0.15 else
  if (budget < 500000) then result := 0.10 else
    result := 0.5;
end;

const
  DEPTLEN     =  3;
  PROJLEN     =  5;
  BUFLEN      =  256;

//float increase_factor (double budget);

(*
 *  A cursor is declared on this select statement, allowing for
 *  the update of projected_budget field.
 *)
  sel_str = 'SELECT proj_id, dept_no, projected_budget '+
     'FROM proj_dept_budget WHERE fiscal_year = 1994 '+
     'FOR UPDATE OF projected_budget;';

(* This query is executed prior to the positioned update. *)
  tot_str =
    'SELECT SUM(projected_budget) FROM proj_dept_budget WHERE fiscal_year = 1994';

var
  upd_str    : string;
  budget     : double;
  DB         : IscDbHandle = nil;   // Database handle
  trans      : IscTrHandle = nil;   // transaction handle
  cursor     : string = 'budget';   // dynamic cursor name
  stmt       : IscStmtHandle = nil; // statement handle
  isqlda     : TSQLParams;
  osqlda     : TSQLResult;
  empdb      : String;

  UL : TUIBLibrary;

begin
  if (ParamCount > 1) then
    empdb := ParamStr(1)
  else
    empdb := EMP_DB_PATH;

  UL := TUIBLibrary.Create;
  try
    UL.Load;
    UL.AttachDatabase(empdb, DB, 'user_name=SYSDBA;password=masterkey');

    (*
     *    Prepare and execute the first select statement.
     *    Free the statement handle, when done.
     *)

    UL.DSQLAllocateStatement(DB, stmt);
    osqlda := TSQLResult.Create;
    try
      UL.TransactionStart(trans, db);

      UL.DSQLPrepare(DB, trans, stmt, tot_str, SQL_DIALECT, osqlda);

      UL.DSQLExecute(trans, stmt, SQL_DIALECT);

      UL.DSQLFetch(DB, trans, stmt, SQL_DIALECT, osqlda);

      Writeln(format('Total budget:  %16.2f', [osqlda.AsDouble[0]]));

      UL.DSQLFreeStatement(stmt, DSQL_close);

      UL.TransactionCommit(trans);

      (*
       *    Prepare and execute the positioned update.
       *    Re-use the statement handle as the select cursor.
       *)

      upd_str := format('UPDATE proj_dept_budget SET projected_budget = ? WHERE CURRENT OF %s',[cursor]);

      (* Allocate an input SQLDA for the update statement. *)
      isqlda := TSQLParams.Create;
      try
        isqlda.AddFieldType('BUDGET',uftDoublePrecision);

        UL.TransactionStart(trans, DB);
        (* Zero the statement handle. *)
        stmt := nil;

        UL.DSQLAllocateStatement(DB, stmt);
        UL.DSQLPrepare(DB, trans, stmt, sel_str, SQL_DIALECT, osqlda);

        (* Declare the cursor. *)

        UL.DSQLSetCursorName(stmt, cursor);

        UL.DSQLExecute(trans, stmt, SQL_DIALECT);

        WriteLn(Format('%-15s%-10s%-18s%-18s',['PROJ', 'DEPT', ' CURRENT BUDGET',  '  CHANGED TO']));

        (*
         *    Fetch and update department budgets.
         *)

        while UL.DSQLFetch(DB, trans, stmt, SQL_DIALECT, osqlda) do
        begin
            (* Determine the increase percentage. *)
          budget := osqlda.AsDouble[2];

          Write(Format('%-15s%-10s%15.2f',[osqlda.AsString[0], osqlda.AsString[1], budget]));
          budget := budget + budget * increase_factor(budget);
          isqlda.AsDouble[0] := budget;
          WriteLn(format('%15.2f'#13, [budget]));

          (* Increase the budget. *)
          try
            UL.DSQLExecImmed2(DB, trans, upd_str, SQL_DIALECT, isqlda, nil);
          except
            on E: EUIBError do
            begin
              if (E.SQLCode = -625) then
              begin
                WriteLn('Exceeded budget limit -- not updated.');
                Continue;
              end
              else
                raise;
            end;
          end;
        end;
        UL.DSQLFreeStatement(stmt, DSQL_close);
        UL.TransactionRollback(trans);
        UL.DetachDatabase(DB);
      finally
        isqlda.Free;
      end;
    finally
      osqlda.Free;
    end;
  finally
    FreeAndNil(UL);
  end;

  ReadLn;
end.
