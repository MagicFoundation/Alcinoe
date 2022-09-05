(*
    "The contents of this file are subject to the Mozilla Public License
    Version 1.1 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at
    http://www.mozilla.org/MPL/

    Software distributed under the License is distributed on an "AS IS"
    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
    License for the specific language governing rights and limitations
    under the License.

    The Initial Developer of the Original Code is
      Henri Gourvest <hgourvest@progdigy.com>.
*)

unit kbUIBLoader;

interface

uses
  SysUtils, Windows, Classes,
  DB, FmtBcd,
  kbmMemTable, uiblib;

type
  TkbUIBLoader = class(TkbmMemTable)
  public
    procedure LoadFromSQLResult(F: TSQLResult);
  end;

procedure Register;

implementation

uses uibase, uibconst;

procedure Register;
begin
  RegisterComponents('UIB', [TkbUIBLoader]);
end;

{ TkbUIBLoader }

type
  TUIBBuffer = record
    case byte of
      0: (Currency: Currency);
      1: (BCD: TBCD);
      2: (Int64: Int64);
      3: (Double: Double);
      4: (Smallint: Smallint);
      5: (TimeStamp: TTimeStamp);
      6: (Integer: Integer);
  end;

procedure TkbUIBLoader.LoadFromSQLResult(F: TSQLResult);
var
  i, j: Integer;
  fcount: Integer;
  TmpName: string;
  Buffer: TUIBBuffer;
  FieldNo: integer;
  FieldType: TUIBFieldType;
  Str: string;
  Stream: TMemoryStream;
  Accept: boolean;
  SqlVar: PUIBSQLVar;
begin
  Filtered := False;
  Filter := '';
  Close;

  FieldDefs.BeginUpdate;
  try
    FieldDefs.Clear;

    for i := 0 to F.FieldCount - 1 do

      with FieldDefs.AddFieldDef, F do
      begin
        fcount := 1;
        TmpName := AliasName[i];
        while TDefCollection(Collection).IndexOf(TmpName) >= 0 do
        begin
          TmpName := TmpName + IntToStr(fcount);
          inc(fcount);
        end;
        Name := TmpName;

        FieldNo := i;
        Required := not IsNullable[i];
        case FieldType[i] of
          uftNumeric:
            begin
              case SQLType[i] of
                SQL_SHORT:
                  begin
                    DataType := ftFMTBcd; // else
                    Size := -Data.sqlvar[i].SqlScale;
                    if Size = 4 then
                      Precision := 5
                    else
                      Precision := 4;
                  end;
                SQL_LONG:
                  begin
                    DataType := ftFMTBcd; // else
                    Size := -Data.sqlvar[i].SqlScale;
                    if Size = 9 then
                      Precision := 10
                    else
                      Precision := 9;
                  end;
                SQL_INT64,
                  SQL_QUAD:
                  begin
                    DataType := ftFMTBcd; // else
                    Size := -Data.sqlvar[i].SqlScale;
                    if Size = 18 then
                      Precision := 19
                    else
                      Precision := 18;
                  end;
                SQL_DOUBLE:
                  DataType := ftFloat; // possible
              end;
            end;
          uftChar,
            uftCstring,
            uftVarchar:
            begin
              DataType := ftString;
              Size := SQLLen[i];
            end;
          uftSmallint: DataType := ftSmallint;
          uftInteger: DataType := ftInteger;
          uftFloat,
            uftDoublePrecision: DataType := ftFloat;
          uftTimestamp: DataType := ftDateTime;
          uftBlob, uftBlobId:
            begin
              if Data.sqlvar[i].SqlSubType = 1 then
                DataType := ftMemo
              else
                DataType := ftBlob;
              Size := SizeOf(TIscQuad);
            end;
          uftDate: DataType := ftDate;
          uftTime: DataType := ftTime;
          uftInt64: DataType := ftLargeint;
          uftArray: DataType := ftArray;
        else
          DataType := ftUnknown;
        end;
      end;
  finally
    FieldDefs.EndUpdate;
  end;
  Open;
  for j := 0 to F.RecordCount - 1 do
  begin
    F.GetRecord(j);
    Append;
    for i := 0 to Fields.Count - 1 do
      if Fields[i].FieldKind = fkData then
      begin
        FieldNo := Fields[i].FieldNo - 1;
        FieldType := F.FieldType[FieldNo];
        SqlVar := @F.Data.sqlvar[FieldNo];
        if F.IsNull[FieldNo] then
          Fields[i].Clear
        else
          case FieldType of
            uftNumeric:
              begin
                case F.SQLType[FieldNo] of
                  SQL_SHORT:
                    Buffer.bcd := StrToBcd(FloatToStr(PSmallint(SqlVar.sqldata)^ / scaledivisor[SqlVar.sqlscale]));
                  SQL_LONG:
                    Buffer.BCD := StrToBcd(FloatToStr(PInteger(SqlVar.sqldata)^ / scaledivisor[SqlVar.sqlscale]));
                  SQL_INT64,
                    SQL_QUAD:
                    Buffer.BCD := StrToBcd(FloatToStr(PInt64(SqlVar.sqldata)^ / scaledivisor[SqlVar.sqlscale]));
                  SQL_DOUBLE:
                    Buffer.Double := PDouble(SqlVar.sqldata)^;
                else
                  raise Exception.Create(EUIB_UNEXPECTEDCASTERROR);
                end;
                SetFieldData(Fields[i], @Buffer);
              end;
            uftChar,
              uftCstring:
              begin
                SetLength(Str, SqlVar.SqlLen);
                Move(SqlVar.sqldata^, PChar(Str)^, SqlVar.SqlLen);
                SetFieldData(Fields[i], Pointer(Str));
              end;
            uftVarchar:
              begin
                SetLength(Str, SqlVar.SqlLen);
                Move(PVary(SqlVar.sqldata).vary_string, PChar(Str)^, PVary(SqlVar.sqldata).vary_length);
                if (Word(SqlVar.SqlLen)) > PVary(SqlVar.sqldata).vary_length then
                  PChar(str)[PVary(SqlVar.sqldata).vary_length] := #0;
                SetFieldData(Fields[i], Pointer(Str));
              end;
            uftSmallint, uftInteger, uftDoublePrecision, uftInt64:
              SetFieldData(Fields[i], SqlVar.sqldata);
            uftFloat:
              begin
                Buffer.Double := PSingle(SqlVar.sqldata)^;
                SetFieldData(Fields[i], @Buffer);
              end;
            uftTimestamp:
              begin
                DecodeTimeStamp(PIscTimeStamp(SqlVar.sqldata), Buffer.TimeStamp);
                Buffer.Double := TimeStampToMSecs(Buffer.TimeStamp);
                SetFieldData(Fields[i], @Buffer);
              end;
            uftBlob, uftBlobId:
              begin
                Stream := TMemoryStream.Create;
                try
                  F.ReadBlob(FieldNo, Stream);
                  Stream.Seek(0, soFromBeginning);
                  TBlobField(Fields[i]).LoadFromStream(Stream);
                finally
                  Stream.Free;
                end;
              end;
            uftDate:
              begin
                Buffer.Integer := PInteger(SqlVar.sqldata)^ - DateOffset + 693594;
                SetFieldData(Fields[i], @Buffer);
              end;
            uftTime:
              begin
                Buffer.Integer := PCardinal(SqlVar.sqldata)^ div 10;
                SetFieldData(Fields[i], @Buffer);
              end;
          else
            raise EUIBError.Create(EUIB_UNEXPECTEDERROR);
          end;

        if Assigned(OnLoad) then
          OnLoadField(Self, i, Fields[i]);
      end;

    Accept := True;
    if Assigned(OnLoadRecord) then
      OnLoadRecord(Self, Accept);
    Post;
  end;
end;

end.

