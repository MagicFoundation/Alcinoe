(*
 *    Program type:  API
 *
 *    Description:
 *        This program show how tu use blob filters.
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
 *
 *)


program filters;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uibase,
  uibLib;

const
  SQL_DIALECT = 3;
  BLB_DB_PATH = 'C:\Program Files\Firebird\examples\blob_filters.fdb';

const
  sel_str     = 'SELECT blb_id, blb_text, blb_inverted, blb_zipped FROM blobs';

const
  bpb = 'target_type=1;source_type=%d';

procedure WriteSeparator;
const
  LINE = #196;
begin
  WriteLn(StringOfChar(LINE,70));
end;

procedure WriteLnCentered(const Text: String);
var
  S: Integer;
begin
  S := (80 - Length(Text)) div 2;
  WriteLn(StringOfChar(' ',S),Text);
end;

procedure WriteBuffer(size: Cardinal; buffer: PChar);
var
  str_hex, str_chr: string;
  P: PChar;
  i: Integer;
begin
  str_chr := '     ';
  for i := 0 to 15 do
    str_chr := str_chr + ' ' + IntToHex(i,2);

  WriteLn(str_chr,'  ','   A S C I I');

  P := Buffer;
  while Cardinal(p - buffer) < size do
  begin
    if ((p - buffer) mod 16) = 0 then
    begin
      if (str_hex <> '') then
        WriteLn(str_hex,'  ',str_chr);

      str_hex := IntToHex(Integer(p - buffer) div 16,4) + '  ' + IntToHex(Byte(P^),2);
      if Byte(P^) in [32..127] then
        str_chr := Char(P^)
      else
        str_chr := '.';
    end
    else
    begin
      str_hex := str_hex + ' ' + IntToHex(Byte(P^),2);
      if Byte(P^) in [32..127] then
        str_chr := str_chr + Char(P^)
      else
        str_chr := str_chr + '.';
    end;
    Inc(P);
  end;

  if (str_hex <> '') then
    WriteLn(str_hex + StringOfChar(' ',53 - Length(str_hex)),'  ',str_chr);
end;

procedure DumpTextBlob(Lib: TUIBLibrary; DB: IscDBHandle; Trans: IscTrHandle; BlobID: TISCQuad);
var
  blob_handle: IscBlobHandle;
  blob_str: String;
begin
  WriteLn(' > Reading BLOB Data');
  WriteSeparator;

  blob_handle := nil;
  Lib.BlobOpen(DB,trans,blob_handle,BlobID);
  blob_str := Lib.BlobReadString(blob_handle);
  Lib.BlobClose(blob_handle);
  WriteLn(blob_str);
  WriteSeparator;
end;

procedure DumpBlob(Lib: TUIBLibrary; DB: IscDBHandle; Trans: IscTrHandle; BlobID: TISCQuad; FilterSubType: ShortInt);
var
  blob_handle: IscBlobHandle;
  blob_str: String;
  blob_buff: Pointer;
  blob_buff_size: Cardinal;
  blob_bpb: string;
begin
  WriteLn(' > Reading BLOB Data - UNFILTERED');
  WriteSeparator;

  blob_handle := nil;
  Lib.BlobOpen(DB,trans,blob_handle,BlobID);
  if FilterSubType = -2 then
  begin
    blob_str := Lib.BlobReadString(blob_handle);
    Lib.BlobClose(blob_handle);
    WriteLn(blob_str)
  end
  else
  begin
    Lib.BlobReadBuffer(blob_handle,blob_buff_size,blob_buff);
    Lib.BlobClose(blob_handle);
    WriteBuffer(blob_buff_size, blob_buff);
    FreeMem(blob_buff,blob_buff_size);
  end;
  WriteSeparator;

  WriteLn(' > Reading BLOB Data - FILTERED');
  WriteSeparator;

  blob_bpb := Format(bpb,[FilterSubType]);
  Lib.BlobOpen(DB,trans,blob_handle,BlobID,blob_bpb);
  blob_str := Lib.BlobReadString(blob_handle);
  Lib.BlobClose(blob_handle);

  WriteLn(blob_str);
  WriteSeparator;
end;

var
  DB         : IscDbHandle = nil;
  trans      : IscTrHandle = nil;
  stmt       : IscStmtHandle = nil;
  sqlda      : TSQLResult;

  empdb: string;

  UL: TUIBLibrary;

begin
  if (ParamCount > 1) then
    empdb := ParamStr(1)
  else
    empdb := BLB_DB_PATH;

  UL := TUIBLibrary.Create;
  try
    UL.Load;
    UL.AttachDatabase(empdb,DB,'user_name=SYSDBA;password=masterkey');

    sqlda := TSQLResult.Create(4);
    try
      UL.TransactionStart(trans,DB);

      WriteLn('Preparing Select Statement');
      UL.DSQLAllocateStatement(DB,stmt);
      UL.DSQLPrepare(DB,trans,stmt,sel_str,SQL_DIALECT,sqlda);
      UL.DSQLExecute(trans,stmt,SQL_DIALECT);

      while UL.DSQLFetch(DB,trans,stmt,SQL_DIALECT,sqlda) do
      begin
        WriteLn;
        WriteLn('Fetching a row [ID=',sqlda.AsInteger[0],']');
        WriteLn;

        WriteLnCentered('Dumping Normal Text Blob');
        WriteSeparator;
        if not sqlda.IsNull[1] then
          DumpTextBlob(UL,DB,trans,sqlda.AsQuad[1])
        else
        begin
          WriteLn(' BLOB IS NULL');
          WriteSeparator;
        end;

        WriteLnCentered('Dumping Inverted Blob');
        WriteSeparator;
        if not sqlda.IsNull[2] then
          DumpBlob(UL,DB,trans,sqlda.AsQuad[2],-2)
        else
        begin
          WriteLn(' BLOB IS NULL');
          WriteSeparator;
        end;

        WriteLn;
        WriteLnCentered('Dumping Zipped Blob');
        WriteSeparator;
        if not sqlda.IsNull[3] then
          DumpBlob(UL,DB,trans,sqlda.AsQuad[3],-3)
        else
        begin
          WriteLn(' BLOB IS NULL');
          WriteSeparator;
        end;
      end;

      WriteLn('Freeing Select Statement');
      UL.DSQLFreeStatement(stmt, DSQL_drop);
    finally
      FreeAndNil(sqlda);
    end;

    WriteLn('Done');
    UL.TransactionCommit(trans);
    UL.DetachDatabase(DB);
  finally
    FreeAndNil(UL);
  end;

  WriteLn('Press any key to continue...');
  ReadLn;
end.
