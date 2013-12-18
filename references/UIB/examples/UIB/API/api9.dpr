(*
 *    Program type:  API Interface
 *
 *    Description:
 *        This program uses a filter to display a blob data type.
 *        Job descriptions are read through a filter, which formats
 *        the text into 40-character-wide paragraphs.
 *
 *    IMPORTANT NOTE!
 *        The server side file, api9f.c, must have been compiled
 *        and linked and must be running on the server before
 *        this example can be run.
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
 

program api9;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uibase,
  uibLib;

const
  SQL_DIALECT = 3;
  EMP_DB_PATH = 'C:\Program Files\Firebird\examples\employee.fdb';

const
  COUNTRYLEN = 15;
  CODELEN    =  5;

  sel_str    = 'SELECT job_requirement, job_code, job_grade, job_country' + #13#10 +
               'FROM job WHERE job_code = ''SRep''';

  bpb: string = ('target_type=-4;source_type=2'); //doit donner [1,2,2,-4,-1,1,2,1,0];

var
  DB         : IscDbHandle = nil;
  trans      : IscTrHandle = nil;
  stmt       : IscStmtHandle = nil;
  sqlda      : TSQLResult;
  blob_handle: IscBlobHandle = nil;

  empdb: string;
  blob_str: string;

  UL: TUIBLibrary;

begin
  if (ParamCount > 1) then
    empdb := ParamStr(1)
  else
    empdb := EMP_DB_PATH;

  UL := TUIBLibrary.Create;
  try
    UL.Load;
    UL.AttachDatabase(empdb,DB,'user_name=SYSDBA;password=masterkey');

    UL.TransactionStart(trans,DB);

    (*
     *    Allocate and prepare the select statement.
     *)

    sqlda := TSQLResult.Create(4);

    UL.DSQLAllocateStatement(DB,stmt);
    UL.DSQLPrepare(DB,trans,stmt,sel_str,SQL_DIALECT,sqlda);
    UL.DSQLExecute(trans,stmt,SQL_DIALECT);

    (*
     *  Display job descriptions.
     *)

    while UL.DSQLFetch(Db,trans,stmt,SQL_DIALECT,sqlda) do
    begin
      WriteLn(Format('JOB CODE: %s  GRADE: %d ',[sqlda.AsString[1], sqlda.AsInteger[2]]));
      WriteLn(Format('  COUNTRY: %s',[sqlda.AsString[3]]));

      WriteLn('UNFILTERED');
      UL.BlobOpen(DB,trans,blob_handle,sqlda.AsQuad[0]);
      blob_str := UL.BlobReadString(blob_handle);
      WriteLn(blob_str);
      UL.BlobClose(blob_handle);

      WriteLn; WriteLn('FILTERED');
      UL.BlobOpen(DB,trans,blob_handle,sqlda.AsQuad[0],bpb);
      blob_str := Ul.BlobReadString(blob_handle);
      WriteLn(blob_str);
      UL.BlobClose(blob_handle);

      WriteLn;
    end;

    UL.DSQLFreeStatement(stmt, DSQL_drop);
    UL.TransactionCommit(trans);
    UL.DetachDatabase(DB);
  finally
    FreeAndNil(UL);
  end;

  ReadLn;
end.
