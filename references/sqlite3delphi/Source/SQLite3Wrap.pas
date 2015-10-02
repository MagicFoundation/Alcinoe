{*
 * SQLite for Delphi and FreePascal/Lazarus
 *
 * This unit contains easy-to-use object wrapper over SQLite3 API functions
 *
 * Copyright 2010-2013 Yury Plashenkov
 * http://plashenkov.github.io/sqlite/
 *
 * The MIT License (MIT)
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom
 * the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *}

unit SQLite3Wrap;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, SQLite3;

type
  ESQLite3Error = class(Exception);

  TSQLite3Statement = class;
  TSQLite3BlobHandler = class;

  { TSQLite3Database class }

  TSQLite3Database = class(TObject)
  private
    FHandle: PSQLite3;
    FStatementList: TList;
    FBlobHandlerList: TList;
    FTransactionOpen: Boolean;
    procedure Check(const ErrCode: Integer);
    procedure CheckHandle;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Open(const FileName: WideString);
    procedure Close;

    procedure Execute(const SQL: WideString);
    function LastInsertRowID: Int64;
    function Prepare(const SQL: WideString): TSQLite3Statement;
    function BlobOpen(const Table, Column: WideString; const RowID: Int64; const WriteAccess: Boolean = True): TSQLite3BlobHandler;

    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;

    property Handle: PSQLite3 read FHandle;
    property TransactionOpen: Boolean read FTransactionOpen;
  end;

  { TSQLite3Statement class }

  TSQLite3Statement = class(TObject)
  private
    FHandle: PSQLite3Stmt;
    FOwnerDatabase: TSQLite3Database;
    function ParamIndexByName(const ParamName: WideString): Integer;
  public
    constructor Create(OwnerDatabase: TSQLite3Database; const SQL: WideString);
    destructor Destroy; override;

    procedure BindInt(const ParamIndex: Integer; const Value: Integer); overload;
    procedure BindInt64(const ParamIndex: Integer; const Value: Int64); overload;
    procedure BindDouble(const ParamIndex: Integer; const Value: Double); overload;
    procedure BindText(const ParamIndex: Integer; const Value: WideString); overload;
    procedure BindNull(const ParamIndex: Integer); overload;
    procedure BindBlob(const ParamIndex: Integer; Data: Pointer; const Size: Integer); overload;
    procedure BindZeroBlob(const ParamIndex: Integer; const Size: Integer); overload;
    procedure BindInt(const ParamName: WideString; const Value: Integer); overload;
    procedure BindInt64(const ParamName: WideString; const Value: Int64); overload;
    procedure BindDouble(const ParamName: WideString; const Value: Double); overload;
    procedure BindText(const ParamName: WideString; const Value: WideString); overload;
    procedure BindNull(const ParamName: WideString); overload;
    procedure BindBlob(const ParamName: WideString; Data: Pointer; const Size: Integer); overload;
    procedure BindZeroBlob(const ParamName: WideString; const Size: Integer); overload;
    procedure ClearBindings;

    function Step: Integer;
    procedure Reset;
    function StepAndReset: Integer;

    function ColumnCount: Integer;
    function ColumnName(const ColumnIndex: Integer): WideString;
    function ColumnType(const ColumnIndex: Integer): Integer;
    function ColumnInt(const ColumnIndex: Integer): Integer;
    function ColumnInt64(const ColumnIndex: Integer): Int64;
    function ColumnDouble(const ColumnIndex: Integer): Double;
    function ColumnText(const ColumnIndex: Integer): WideString;
    function ColumnBlob(const ColumnIndex: Integer): Pointer;
    function ColumnBytes(const ColumnIndex: Integer): Integer;

    property Handle: PSQLite3Stmt read FHandle;
    property OwnerDatabase: TSQLite3Database read FOwnerDatabase;
  end;

  { TSQLite3BlobHandler class }

  TSQLite3BlobHandler = class(TObject)
  private
    FHandle: PSQLite3Blob;
    FOwnerDatabase: TSQLite3Database;
  public
    constructor Create(OwnerDatabase: TSQLite3Database; const Table, Column: WideString; const RowID: Int64; const WriteAccess: Boolean = True);
    destructor Destroy; override;

    function Bytes: Integer;
    procedure Read(Buffer: Pointer; const Size, Offset: Integer);
    procedure Write(Buffer: Pointer; const Size, Offset: Integer);

    property Handle: PSQLite3Blob read FHandle;
    property OwnerDatabase: TSQLite3Database read FOwnerDatabase;
  end;

implementation

uses
  SQLite3Utils;

resourcestring
  SErrorMessage = 'SQLite3 error: %s';
  SDatabaseNotConnected = 'SQLite3 error: database is not connected.';
  STransactionAlreadyOpen = 'Transaction is already opened.';
  SNoTransactionOpen = 'No transaction is open';

{ TSQLite3Database }

procedure TSQLite3Database.BeginTransaction;
begin
  if not FTransactionOpen then
  begin
    Execute('BEGIN TRANSACTION;');
    FTransactionOpen := True;
  end
  else
    raise ESQLite3Error.Create(STransactionAlreadyOpen);
end;

function TSQLite3Database.BlobOpen(const Table, Column: WideString;
  const RowID: Int64; const WriteAccess: Boolean): TSQLite3BlobHandler;
begin
  Result := TSQLite3BlobHandler.Create(Self, Table, Column, RowID, WriteAccess);
end;

procedure TSQLite3Database.Check(const ErrCode: Integer);
begin
  if ErrCode <> SQLITE_OK then
    raise ESQLite3Error.CreateFmt(SErrorMessage, [UTF8ToStr(sqlite3_errmsg(FHandle))]);
end;

procedure TSQLite3Database.CheckHandle;
begin
  if FHandle = nil then
    raise ESQLite3Error.Create(SDatabaseNotConnected);
end;

procedure TSQLite3Database.Close;
var
  I: Integer;
begin
  if FHandle <> nil then
  begin
    if FTransactionOpen then
      Rollback;
    // Delete all statements
    for I := FStatementList.Count - 1 downto 0 do
      TSQLite3Statement(FStatementList[I]).Free;
    // Delete all blob handlers
    for I := FBlobHandlerList.Count - 1 downto 0 do
      TSQLite3BlobHandler(FBlobHandlerList[I]).Free;
    sqlite3_close(FHandle);
    FHandle := nil;
  end;
end;

procedure TSQLite3Database.Commit;
begin
  if FTransactionOpen then
  begin
    Execute('COMMIT;');
    FTransactionOpen := False;
  end
  else
    raise ESQLite3Error.Create(SNoTransactionOpen);
end;

constructor TSQLite3Database.Create;
begin
  FHandle := nil;
  FStatementList := TList.Create;
  FBlobHandlerList := TList.Create;
end;

destructor TSQLite3Database.Destroy;
begin
  Close;
  FBlobHandlerList.Free;
  FStatementList.Free;
  inherited;
end;

procedure TSQLite3Database.Execute(const SQL: WideString);
begin
  CheckHandle;
  Check(sqlite3_exec(FHandle, PAnsiChar(StrToUTF8(SQL)), nil, nil, nil));
end;

function TSQLite3Database.LastInsertRowID: Int64;
begin
  CheckHandle;
  Result := sqlite3_last_insert_rowid(FHandle);
end;

procedure TSQLite3Database.Open(const FileName: WideString);
begin
  Close;
  Check(sqlite3_open(PAnsiChar(StrToUTF8(FileName)), FHandle));
end;

function TSQLite3Database.Prepare(const SQL: WideString): TSQLite3Statement;
begin
  Result := TSQLite3Statement.Create(Self, SQL);
end;

procedure TSQLite3Database.Rollback;
begin
  if FTransactionOpen then
  begin
    Execute('ROLLBACK;');
    FTransactionOpen := False;
  end
  else
    raise ESQLite3Error.Create(SNoTransactionOpen);
end;

{ TSQLite3Statement }

procedure TSQLite3Statement.BindBlob(const ParamIndex: Integer; Data: Pointer;
  const Size: Integer);
begin
  FOwnerDatabase.Check(sqlite3_bind_blob(FHandle, ParamIndex, Data, Size, SQLITE_TRANSIENT));
end;

procedure TSQLite3Statement.BindDouble(const ParamIndex: Integer;
  const Value: Double);
begin
  FOwnerDatabase.Check(sqlite3_bind_double(FHandle, ParamIndex, Value));
end;

procedure TSQLite3Statement.BindInt(const ParamIndex, Value: Integer);
begin
  FOwnerDatabase.Check(sqlite3_bind_int(FHandle, ParamIndex, Value));
end;

procedure TSQLite3Statement.BindInt64(const ParamIndex: Integer;
  const Value: Int64);
begin
  FOwnerDatabase.Check(sqlite3_bind_int64(FHandle, ParamIndex, Value));
end;

procedure TSQLite3Statement.BindNull(const ParamIndex: Integer);
begin
  FOwnerDatabase.Check(sqlite3_bind_null(FHandle, ParamIndex));
end;

procedure TSQLite3Statement.BindText(const ParamIndex: Integer;
  const Value: WideString);
var
  S: AnsiString; { UTF-8 string }
begin
  S := StrToUTF8(Value);
  FOwnerDatabase.Check(
    sqlite3_bind_text(FHandle, ParamIndex, PAnsiChar(S), Length(S), SQLITE_TRANSIENT)
  );
end;

procedure TSQLite3Statement.BindZeroBlob(const ParamIndex, Size: Integer);
begin
  FOwnerDatabase.Check(sqlite3_bind_zeroblob(FHandle, ParamIndex, Size));
end;

procedure TSQLite3Statement.ClearBindings;
begin
  FOwnerDatabase.Check(sqlite3_clear_bindings(FHandle));
end;

function TSQLite3Statement.ColumnBlob(const ColumnIndex: Integer): Pointer;
begin
  Result := sqlite3_column_blob(FHandle, ColumnIndex);
end;

function TSQLite3Statement.ColumnBytes(const ColumnIndex: Integer): Integer;
begin
  Result := sqlite3_column_bytes(FHandle, ColumnIndex);
end;

function TSQLite3Statement.ColumnCount: Integer;
begin
  Result := sqlite3_column_count(FHandle);
end;

function TSQLite3Statement.ColumnDouble(const ColumnIndex: Integer): Double;
begin
  Result := sqlite3_column_double(FHandle, ColumnIndex);
end;

function TSQLite3Statement.ColumnInt(const ColumnIndex: Integer): Integer;
begin
  Result := sqlite3_column_int(FHandle, ColumnIndex);
end;

function TSQLite3Statement.ColumnInt64(const ColumnIndex: Integer): Int64;
begin
  Result := sqlite3_column_int64(FHandle, ColumnIndex);
end;

function TSQLite3Statement.ColumnName(const ColumnIndex: Integer): WideString;
begin
  Result := UTF8ToStr(sqlite3_column_name(FHandle, ColumnIndex));
end;

function TSQLite3Statement.ColumnText(const ColumnIndex: Integer): WideString;
var
  Len: Integer;
begin
  Len := ColumnBytes(ColumnIndex);
  Result := UTF8ToStr(sqlite3_column_text(FHandle, ColumnIndex), Len);
end;

function TSQLite3Statement.ColumnType(const ColumnIndex: Integer): Integer;
begin
  Result := sqlite3_column_type(FHandle, ColumnIndex);
end;

constructor TSQLite3Statement.Create(OwnerDatabase: TSQLite3Database;
  const SQL: WideString);
begin
  FOwnerDatabase := OwnerDatabase;
  FOwnerDatabase.CheckHandle;
  FOwnerDatabase.Check(
    sqlite3_prepare_v2(FOwnerDatabase.Handle, PAnsiChar(StrToUTF8(SQL)), -1, FHandle, nil)
  );
  FOwnerDatabase.FStatementList.Add(Self);
end;

destructor TSQLite3Statement.Destroy;
begin
  FOwnerDatabase.FStatementList.Remove(Self);
  sqlite3_finalize(FHandle);
  inherited;
end;

function TSQLite3Statement.ParamIndexByName(const ParamName: WideString): Integer;
begin
  Result := sqlite3_bind_parameter_index(FHandle, PAnsiChar(StrToUTF8(ParamName)));
end;

procedure TSQLite3Statement.Reset;
begin
  sqlite3_reset(FHandle);
end;

function TSQLite3Statement.Step: Integer;
begin
  Result := sqlite3_step(FHandle);
end;

function TSQLite3Statement.StepAndReset: Integer;
begin
  Result := Step;
  Reset;
end;

procedure TSQLite3Statement.BindBlob(const ParamName: WideString; Data: Pointer;
  const Size: Integer);
begin
  BindBlob(ParamIndexByName(ParamName), Data, Size);
end;

procedure TSQLite3Statement.BindDouble(const ParamName: WideString;
  const Value: Double);
begin
  BindDouble(ParamIndexByName(ParamName), Value);
end;

procedure TSQLite3Statement.BindInt(const ParamName: WideString;
  const Value: Integer);
begin
  BindInt(ParamIndexByName(ParamName), Value);
end;

procedure TSQLite3Statement.BindInt64(const ParamName: WideString;
  const Value: Int64);
begin
  BindInt64(ParamIndexByName(ParamName), Value);
end;

procedure TSQLite3Statement.BindNull(const ParamName: WideString);
begin
  BindNull(ParamIndexByName(ParamName));
end;

procedure TSQLite3Statement.BindText(const ParamName, Value: WideString);
begin
  BindText(ParamIndexByName(ParamName), Value);
end;

procedure TSQLite3Statement.BindZeroBlob(const ParamName: WideString;
  const Size: Integer);
begin
  BindZeroBlob(ParamIndexByName(ParamName), Size);
end;

{ TSQLite3BlobHandler }

function TSQLite3BlobHandler.Bytes: Integer;
begin
  Result := sqlite3_blob_bytes(FHandle);
end;

constructor TSQLite3BlobHandler.Create(OwnerDatabase: TSQLite3Database; const Table,
  Column: WideString; const RowID: Int64; const WriteAccess: Boolean);
begin
  FOwnerDatabase := OwnerDatabase;
  FOwnerDatabase.CheckHandle;
  FOwnerDatabase.Check(
    sqlite3_blob_open(FOwnerDatabase.FHandle, 'main', PAnsiChar(StrToUTF8(Table)),
      PAnsiChar(StrToUTF8(Column)), RowID, Ord(WriteAccess), FHandle)
  );
  FOwnerDatabase.FBlobHandlerList.Add(Self);
end;

destructor TSQLite3BlobHandler.Destroy;
begin
  FOwnerDatabase.FBlobHandlerList.Remove(Self);
  sqlite3_blob_close(FHandle);
  inherited;
end;

procedure TSQLite3BlobHandler.Read(Buffer: Pointer; const Size,
  Offset: Integer);
begin
  FOwnerDatabase.Check(sqlite3_blob_read(FHandle, Buffer, Size, Offset));
end;

procedure TSQLite3BlobHandler.Write(Buffer: Pointer; const Size,
  Offset: Integer);
begin
  FOwnerDatabase.Check(sqlite3_blob_write(FHandle, Buffer, Size, Offset));
end;

end.
