(*
 *    Program type:  API Interface
 *
 *    Description:
 *        This program updates a blob data type.
 *        Project descriptions are added for a set of projects.
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
 
program api8;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uibase,
  uibLib;

const
  SQL_DIALECT = 3;
  EMP_DB_PATH = 'C:\Program Files\Firebird\examples\employee.fdb';

const
  Proj_data: array[0..21] of string =
    ('VBASE',
        'Design a video data base management system for ',
        'controlling on-demand video distribution.',
        '',
    'DGPII',
        'Develop second generation digital pizza maker ',
        'with flash-bake heating element and ',
        'digital ingredient measuring system.',
        '',
    'GUIDE',
        'Develop a prototype for the automobile version of ',
        'the hand-held map browsing device.',
        '',
    'MAPDB',
        'Port the map browsing database software to run ',
        'on the automobile model.',
        '',
    'HWRII',
        'Integrate the hand-writing recognition module into the ',
        'universal language translator.',
        '',
    '');

var
  Inp_ptr : Integer = 0;

  function get_line: string;
  begin
    result := Proj_data[Inp_ptr];
    inc(Inp_ptr);
  end;

var
  upd_stmt    : string;
  blob_handle : IscBlobHandle = nil;
  DB          : IscDbHandle   = nil; (* database handle *)
  trans       : IscTrHandle   = nil; (* transaction handle *)
  sqlda       : TSQLParams;
  line        : string;
  rec_cnt     : Integer = 0;
  empdb       : String;

  UL : TUIBLibrary;

begin
  if (ParamCount > 1) then
    empdb := ParamStr(1)
  else
    empdb := EMP_DB_PATH;

  upd_stmt := 'UPDATE project SET' + #13#10 +
              '  proj_desc = ?' + #13#10 +
              'WHERE proj_id = ?';

  UL := TUIBLibrary.Create;
  try
    UL.Load;
    UL.AttachDatabase(empdb, DB, 'user_name=SYSDBA;password=masterkey');

    //  Set-up the SQLDA for the update statement.

    sqlda := TSQLParams.Create;
    try
      sqlda.AddFieldType('DESCRIPTION',uftBlob);
      sqlda.AddFieldType('LINE',uftChar,5);

      UL.TransactionStart(trans, DB);

      // Get the next project id and update the project description.

      line := get_line;
      while (line <> '') do
      begin
        sqlda.AsString[1] := line;
        WriteLn(format('Updating description for project:  %s', [line]));

        blob_handle := nil;
        sqlda.AsQuad[0] := UL.BlobCreate(DB, trans, blob_handle);

        line := get_line;
        while(line <> '') do
        begin
          WriteLn(Format('  Inserting segment:  %s', [line]));
          UL.BlobWriteString(blob_handle, line);
          line := get_line;
        end;
        UL.BlobClose(blob_handle);
        UL.DSQLExecuteImmediate(DB, trans, upd_stmt, SQL_DIALECT, sqlda);
        Inc(rec_cnt);
        line := get_line;
      end;

      UL.TransactionRollback(trans); // change to TransactionCommit to apply updates
      WriteLn(format('Added %d project descriptions.', [rec_cnt]));

      UL.DetachDatabase(DB);
    finally
      sqlda.Free;
    end;
  finally
    FreeAndNil(UL);
  end;

  ReadLn;
end.
