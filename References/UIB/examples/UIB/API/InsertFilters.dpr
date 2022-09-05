program InsertFilters;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uibase,
  uibLib;

const
  SQL_DIALECT = 3;
  BLB_DB_PATH = 'C:\Program Files\Firebird\examples\blob_filters.fdb';

const
  ins_str     = 'INSERT INTO BLOBS (blb_text, blb_inverted, blb_zipped)' + #13#10 +
                'VALUES (?,?,?)';

const
  inv_bpb = 'target_type=-2;source_type=1';
  zip_bpb = 'target_type=-3;source_type=1';

function make_line: string;
var
  i: Integer;
begin
  SetLength(Result,1024 + Random(7168));
  for i := 1 to Length(Result) do
  begin
    if (i mod (20 + Random(50))) = 0 then
      Result[i] := #10
    else
      Result[i] := Char(Random(26) + Ord('A'));
  end;
  Result := '[D]' + Result + '[F]';
end;

var
  DB         : IscDbHandle = nil;
  trans      : IscTrHandle = nil;
  stmt       : IscStmtHandle = nil;
  sqlpa      : TSQLParams;
  blob_handle: IscBlobHandle = nil;

  empdb: string;
  blob_str: string;

  UL: TUIBLibrary;

begin
  Randomize;

  if (ParamCount > 1) then
    empdb := ParamStr(1)
  else
    empdb := BLB_DB_PATH;

  UL := TUIBLibrary.Create;
  try
    UL.Load;
    UL.AttachDatabase(empdb,DB,'user_name=SYSDBA;password=masterkey');

    sqlpa := TSQLParams.Create;
    try
      sqlpa.AddFieldType('TEXT',uftBlob);
      sqlpa.AddFieldType('INVERT',uftBlob);
      sqlpa.AddFieldType('ZIP',uftBlob);

      UL.TransactionStart(trans, DB);

      WriteLn('Creating blobs');

      blob_str := make_line;

      sqlpa.AsQuad[0] := UL.BlobCreate(DB,trans,blob_handle);
      WriteLn('Writing NORMAL TEXT Blob Data');
      UL.BlobWriteString(blob_handle,blob_str);
      UL.BlobClose(blob_handle);

      blob_handle := nil;
      sqlpa.AsQuad[1] := UL.BlobCreate(DB,trans,blob_handle,inv_bpb);

      WriteLn('Writing INVERTED Blob Data');
      UL.BlobWriteString(blob_handle,blob_str);
      UL.BlobClose(blob_handle);

      blob_handle := nil;
      sqlpa.AsQuad[2] := UL.BlobCreate(DB,trans,blob_handle,zip_bpb);

      WriteLn('Writing ZIPPED Blob Data');
      UL.BlobWriteString(blob_handle,blob_str);
      UL.BlobClose(blob_handle);

      WriteLn('Executing Insert Statement');
      UL.DSQLExecuteImmediate(DB,trans,ins_str,SQL_DIALECT,sqlpa);
      Ul.TransactionCommit(trans);

      WriteLn('Done');
    finally
      FreeAndNil(sqlpa);
      WriteLn;
    end;

    UL.DetachDatabase(DB);
  finally
    FreeAndNil(UL);
  end;

  WriteLn('Press any key to continue...');
  ReadLn;
end.
