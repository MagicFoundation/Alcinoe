(*
 *  Program type:  API Interface
 *
 *  Desription:
 *      This program updates departments' budgets, given
 *      the department and the new budget information parameters.
 *
 *      An input SQLDA is allocated for the update query
 *      with parameter markers.
 *      Note that all updates are rolled back in this version.
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

program api4;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uibase,
  uibLib;

const
  SQL_DIALECT = 3;
  EMP_DB_PATH = 'C:\Program Files\Firebird\examples\employee.fdb';

var
  Dept_data    : array[0..4] of string = ('622', '100', '116', '900', '');
  Percent_data : array[0..4] of Double = (0.05,  1.00,  0.075,  0.10, 0);
  Input_ptr : Integer = 0;
  updstr: string = 'UPDATE department SET budget = ? * budget + budget WHERE dept_no = ?';

  DB   : IscDbHandle = nil; // database handle
  trans: IscTrHandle = nil; // transaction handle


  dept_no : string;
  percent_inc: Double;
  flag0 : Smallint = 0;
  flag1 : Smallint = 0;
  sqlda : TSQLParams;
  empdb : String;

  UL: TUIBLibrary;

(*
 *  Get the department and percent parameters.
 *)

function get_input (out dept_no: string; out percent: double): boolean;
begin
  Result := False;
  if (Dept_data[Input_ptr] = '') then  Exit;
  dept_no := Dept_data[Input_ptr];
  inc(Input_ptr);
  percent := Percent_data[Input_ptr];
  Result := true;
end;


begin
  if (ParamCount > 1) then
    empdb := ParamStr(1)
  else
    empdb := EMP_DB_PATH;

  UL := TUIBLibrary.Create;
  try
    UL.Load;
    UL.AttachDatabase(empdb, DB, 'user_name=SYSDBA;password=masterkey');

    (* Allocate an input SQLDA.  There are two unknown parameters. *)
    sqlda := TSQLParams.Create;
    try
      sqlda.AddFieldType('PERCENT_INC',uftDoublePrecision);
      sqlda.AddFieldType('DEPT_NO',uftChar,3);

     (*
      *  Get the next department-percent increase input pair.
      *)
      while get_input(dept_no, percent_inc) do
      begin
        sqlda.AsDouble[0] := percent_inc;
        sqlda.AsString[1] := dept_no;
        Writeln(Format('Increasing budget for department:  %s  by %5.2f percent.',[dept_no, percent_inc]));
        UL.TransactionStart(trans, DB);

        (* Update the budget. *)
        try
          UL.DSQLExecuteImmediate(DB, trans, updstr, SQL_DIALECT, sqlda);
        except
          on E: EUIBError do
          begin
            if (E.SqlCode <> 0) then
            begin
              (* Don't save the update, if the new budget exceeds the limit. *)
              if (E.sqlcode = -625) then
              begin
                WriteLn('Exceeded budget limit -- not updated.');
                UL.TransactionRollback(trans);
                Continue;
              end;
              (* Undo all changes, in case of an error. *)
            end
            else
            begin
              WriteLn('Error: SQLCode=',E.SqlCode,',',E.Message);
              UL.TransactionRollback(trans);
              raise;
            end;
          end;
        end;

        (* Save each department's update independently.
        ** Change to CommitTransaction to see changes
         *)
        UL.TransactionRollback(trans);
      end;

      UL.DetachDatabase(DB);
    finally
      sqlda.Free;
    end;
  finally
    FreeAndNil(UL);
  end;

  Readln;
end.

