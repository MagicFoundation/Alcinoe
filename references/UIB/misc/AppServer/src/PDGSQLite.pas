unit PDGSQLite;

interface
uses PDGDB, SuperObject;

type
  TPDGSQLiteConnection = class(TPDGConnection)
  private
    FDbHandle: Pointer;
  protected
    procedure ExecuteImmediate(const Options: SOString); override;
    function newContext(const Options: ISuperObject = nil): IPDGContext; override;
  public
    constructor Create(const Options: ISuperObject); reintroduce; overload;
    constructor Create(const Options: string); reintroduce; overload;
    destructor Destroy; override;
  end;

  TPDGSQLiteContext = class(TPDGContext)
  private
    FConnection: TPDGSQLiteConnection;
  protected
    procedure ExecuteImmediate(const Options: SOString); override;
    function newCommand(const Options: ISuperObject = nil): IPDGCommand; override;
  public
    constructor Create(Connection: TPDGSQLiteConnection; Options: ISuperObject); reintroduce;
    destructor Destroy; override;
  end;

  TPDGSQLiteCommand = class(TPDGCommand)
  private
    FNeedReset: boolean;
    FStHandle: Pointer;
    FConnection: TPDGSQLiteConnection;
  protected
    function Execute(const params: ISuperObject = nil; const context: IPDGContext = nil): ISuperObject; override;
    function GetInputMeta: ISuperObject; override;
    function GetOutputMeta: ISuperObject; override;
  public
    constructor Create(const Connection: TPDGSQLiteConnection; const Context: TPDGSQLiteContext; Options: ISuperObject); reintroduce;
    destructor Destroy; override;
  end;

implementation
uses SysUtils;
const

{$DEFINE UNICODE}

{$IFDEF MSWINDOWS}
  Sqlite3Lib = 'sqlite3.dll';
{$else}
  Sqlite3Lib = 'libsqlite3.so';
{$endif}

  SQLITE_OK           = 0;   (* Successful result *)
  (* beginning-of-error-codes *)
  SQLITE_ERROR        = 1;   (* SQL error or missing database *)
  SQLITE_INTERNAL     = 2;   (* Internal logic error in SQLite *)
  SQLITE_PERM         = 3;   (* Access permission denied *)
  SQLITE_ABORT        = 4;   (* Callback routine requested an abort *)
  SQLITE_BUSY         = 5;   (* The database file is locked *)
  SQLITE_LOCKED       = 6;   (* A table in the database is locked *)
  SQLITE_NOMEM        = 7;   (* A malloc() failed *)
  SQLITE_READONLY     = 8;   (* Attempt to write a readonly database *)
  SQLITE_INTERRUPT    = 9;   (* Operation terminated by sqlite3_interrupt()*)
  SQLITE_IOERR       = 10;   (* Some kind of disk I/O error occurred *)
  SQLITE_CORRUPT     = 11;   (* The database disk image is malformed *)
  SQLITE_NOTFOUND    = 12;   (* NOT USED. Table or record not found *)
  SQLITE_FULL        = 13;   (* Insertion failed because database is full *)
  SQLITE_CANTOPEN    = 14;   (* Unable to open the database file *)
  SQLITE_PROTOCOL    = 15;   (* NOT USED. Database lock protocol error *)
  SQLITE_EMPTY       = 16;   (* Database is empty *)
  SQLITE_SCHEMA      = 17;   (* The database schema changed *)
  SQLITE_TOOBIG      = 18;   (* String or BLOB exceeds size limit *)
  SQLITE_CONSTRAINT  = 19;   (* Abort due to constraint violation *)
  SQLITE_MISMATCH    = 20;   (* Data type mismatch *)
  SQLITE_MISUSE      = 21;   (* Library used incorrectly *)
  SQLITE_NOLFS       = 22;   (* Uses OS features not supported on host *)
  SQLITE_AUTH        = 23;   (* Authorization denied *)
  SQLITE_FORMAT      = 24;   (* Auxiliary database format error *)
  SQLITE_RANGE       = 25;   (* 2nd parameter to sqlite3_bind out of range *)
  SQLITE_NOTADB      = 26;   (* File opened that is not a database file *)
  SQLITE_ROW         = 100;  (* sqlite3_step() has another row ready *)
  SQLITE_DONE        = 101;  (* sqlite3_step() has finished executing *)

  SQLITE_INTEGER = 1;
  SQLITE_FLOAT   = 2;
  SQLITE_TEXT    = 3;
  SQLITE_BLOB    = 4;
  SQLITE_NULL    = 5;

function sqlite3_open(filename: PSOChar; var dbHandle: Pointer): Integer; cdecl;
  external Sqlite3Lib {$ifdef UNICODE} name 'sqlite3_open16' {$endif};
function sqlite3_close(dbHandle: Pointer): Integer; cdecl;
  external Sqlite3Lib;

function sqlite3_prepare_v2(db: Pointer; sql: PSOChar; len: Integer; var stmt: Pointer; tail: Pointer): Integer; cdecl;
  external Sqlite3Lib {$IFDEF UNICODE}name 'sqlite3_prepare16_v2'{$endif};
function sqlite3_finalize(stmt: Pointer): Integer; cdecl;
  external Sqlite3Lib;
function sqlite3_exec(db: Pointer; sql: PAnsiChar; callback: Pointer; callback_arg: pointer; errmsg: PPAnsiChar): Integer; cdecl;
  external Sqlite3Lib;
function sqlite3_reset(stm: Pointer): Integer; cdecl;
  external Sqlite3Lib;

function sqlite3_errmsg(db: Pointer): PSOChar; cdecl;
  external Sqlite3Lib {$ifdef UNICODE} name 'sqlite3_errmsg16' {$endif};

function sqlite3_bind_parameter_count(stm: Pointer): Integer; cdecl;
  external Sqlite3Lib;
function sqlite3_bind_parameter_name(stm: Pointer; index: longint): PAnsiChar; cdecl;
  external Sqlite3Lib;
function sqlite3_bind_parameter_index(stm: Pointer; name: PAnsiChar): Integer; cdecl;
  external Sqlite3Lib;

function sqlite3_column_count(stm: Pointer): Integer; cdecl;
  external Sqlite3Lib;
function sqlite3_column_type(stm: Pointer; col: Integer): Integer; cdecl;
  external Sqlite3Lib;
function sqlite3_column_name(stm: Pointer; col: Integer): PSOChar; cdecl;
  external Sqlite3Lib {$IFDEF UNICODE} name 'sqlite3_column_name16'{$ENDIF};
function sqlite3_column_decltype(stm: Pointer; col: Integer): PSOChar; cdecl;
  external Sqlite3Lib {$IFDEF UNICODE} name 'sqlite3_column_decltype16'{$ENDIF};
function sqlite3_column_text(stm: Pointer; col:longint): PSOChar; cdecl;
  external Sqlite3Lib {$IFDEF UNICODE} name 'sqlite3_column_text16'{$ENDIF};
function sqlite3_column_int64(stm: Pointer; col: Integer): Int64; cdecl;
  external Sqlite3Lib;
function sqlite3_column_double(stm: Pointer; col: Integer): Double; cdecl;
  external Sqlite3Lib;
function sqlite3_column_blob(stm: Pointer; col: Integer): Pointer; cdecl;
  external Sqlite3Lib;
function sqlite3_column_bytes(stm: Pointer; col: Integer): Integer; cdecl;
  external Sqlite3Lib;
function sqlite3_step(stm: Pointer): Integer; cdecl;
  external Sqlite3Lib;

type
  TBindDestructorCallback = procedure(p: Pointer); cdecl;

function sqlite3_bind_blob(stm: Pointer; col: Integer;
  const p: Pointer; n: Integer; dest: TBindDestructorCallback): Integer; cdecl;
  external Sqlite3Lib;
function sqlite3_bind_double(stm: Pointer; col: Integer; value: double): Integer; cdecl;
  external Sqlite3Lib;
function sqlite3_bind_int(stm: Pointer; col, value: Integer): Integer; cdecl;
  external Sqlite3Lib;
function sqlite3_bind_int64(stm: Pointer; col: Integer; value: Int64): Integer; cdecl;
  external Sqlite3Lib;
function sqlite3_bind_null(stm: Pointer; col: Integer): Integer; cdecl;
  external Sqlite3Lib;
function sqlite3_bind_text(stm: Pointer; col: Integer; value: PSOChar; n: Integer; dest: TBindDestructorCallback): Integer; cdecl;
  external Sqlite3Lib {$IFDEF UNICODE} name 'sqlite3_bind_text16'{$ENDIF};
function sqlite3_bind_zeroblob(stm: Pointer; col, n: Integer): Integer; cdecl;
  external Sqlite3Lib;

procedure sqlite3_free(p: Pointer); cdecl;
begin
  FreeMem(p);
end;

procedure CheckError(code: Integer; db: Pointer);
  procedure RaiseError;
  begin
    if db <> nil then
      raise Exception.Create(sqlite3_errmsg(db)) else
      case code of
        SQLITE_ERROR       : raise Exception.Create('SQL error or missing database');
        SQLITE_INTERNAL    : raise Exception.Create('Internal logic error in SQLite');
        SQLITE_PERM        : raise Exception.Create('Access permission denied');
        SQLITE_ABORT       : raise Exception.Create('Callback routine requested an abort');
        SQLITE_BUSY        : raise Exception.Create('The database file is locked');
        SQLITE_LOCKED      : raise Exception.Create('A table in the database is locked');
        SQLITE_NOMEM       : raise Exception.Create('A malloc() failed');
        SQLITE_READONLY    : raise Exception.Create('Attempt to write a readonly database');
        SQLITE_INTERRUPT   : raise Exception.Create('Operation terminated by sqlite3_interrupt()');
        SQLITE_IOERR       : raise Exception.Create('Some kind of disk I/O error occurred');
        SQLITE_CORRUPT     : raise Exception.Create('The database disk image is malformed');
        SQLITE_NOTFOUND    : raise Exception.Create('NOT USED. Table or record not found');
        SQLITE_FULL        : raise Exception.Create('Insertion failed because database is full');
        SQLITE_CANTOPEN    : raise Exception.Create('Unable to open the database file');
        SQLITE_PROTOCOL    : raise Exception.Create('NOT USED. Database lock protocol error');
        SQLITE_EMPTY       : raise Exception.Create('Database is empty');
        SQLITE_SCHEMA      : raise Exception.Create('The database schema changed');
        SQLITE_TOOBIG      : raise Exception.Create('String or BLOB exceeds size limit');
        SQLITE_CONSTRAINT  : raise Exception.Create('Abort due to constraint violation');
        SQLITE_MISMATCH    : raise Exception.Create('Data type mismatch');
        SQLITE_MISUSE      : raise Exception.Create('Library used incorrectly');
        SQLITE_NOLFS       : raise Exception.Create('Uses OS features not supported on host');
        SQLITE_AUTH        : raise Exception.Create('Authorization denied');
        SQLITE_FORMAT      : raise Exception.Create('Auxiliary database format error');
        SQLITE_RANGE       : raise Exception.Create('2nd parameter to sqlite3_bind out of range');
        SQLITE_NOTADB      : raise Exception.Create('File opened that is not a database file');
        SQLITE_ROW         : raise Exception.Create('sqlite3_step() has another row ready');
        SQLITE_DONE        : raise Exception.Create('sqlite3_step() has finished executing');
      end;
  end;
begin
  if Code > SQLITE_OK then
    RaiseError;
end;

const
  MAX_BUSY_TIME = 5000;

{ TPDGSQLiteConnection }

constructor TPDGSQLiteConnection.Create(const Options: ISuperObject);
var
  param: ISuperObject;
begin
  inherited Create(stObject);
  FDbHandle := nil;

  DataPtr := Self;
  Merge(Options, true);

  param := O['databasename'];
  if param <> nil then
    CheckError(sqlite3_open(PSOChar(param.AsString), FDbHandle), nil) else
    FDbHandle := nil;
end;

constructor TPDGSQLiteConnection.Create(const Options: string);
begin
  Create(SO(Options));
end;

destructor TPDGSQLiteConnection.Destroy;
begin
  if FDbHandle <> nil then
    CheckError(sqlite3_close(FDbHandle), FDbHandle);
  inherited;
end;

procedure TPDGSQLiteConnection.ExecuteImmediate(const Options: SOString);
begin
  CheckError(sqlite3_exec(FDbHandle, PAnsiChar(UTF8Encode(Options)), nil, nil, nil), FDbHandle);
end;

function TPDGSQLiteConnection.newContext(const Options: ISuperObject): IPDGContext;
begin
  Result := TPDGSQLiteContext.Create(Self, Options);
end;

{ TPDGSQLiteContext }

constructor TPDGSQLiteContext.Create(Connection: TPDGSQLiteConnection;
  Options: ISuperObject);
var
  sql: string;
begin
  inherited Create(stObject);
  DataPtr := Self;
  Merge(Options, true);
  FConnection := Connection;
  AsObject['connection'] := Connection;
  if ObjectIsType(Options, stString) then
    sql := SysUtils.format('BEGIN %s TRANSACTION', [Options.AsString]) else
    sql := 'BEGIN TRANSACTION';

  CheckError(sqlite3_exec(FConnection.FDbHandle, PAnsiChar(AnsiString(sql)), nil, nil, nil), FConnection.FDbHandle);
end;

destructor TPDGSQLiteContext.Destroy;
var
  obj: ISuperObject;
begin
  obj := AsObject['rollback'];
  if ObjectIsType(obj, stBoolean) and obj.AsBoolean then
    CheckError(sqlite3_exec(FConnection.FDbHandle, 'ROLLBACK', nil, nil, nil), FConnection.FDbHandle) else
    CheckError(sqlite3_exec(FConnection.FDbHandle, 'COMMIT', nil, nil, nil), FConnection.FDbHandle);
  inherited;
end;

procedure TPDGSQLiteContext.ExecuteImmediate(const Options: SOString);
begin
  CheckError(sqlite3_exec(FConnection.FDbHandle, PAnsiChar(UTF8Encode(Options)), nil, nil, nil), FConnection.FDbHandle);
end;

function TPDGSQLiteContext.newCommand(const Options: ISuperObject): IPDGCommand;
begin
  Result := TPDGSQLiteCommand.Create(FConnection, Self, Options);
end;

{ TPDGSQLiteCommand }

constructor TPDGSQLiteCommand.Create(const Connection: TPDGSQLiteConnection;
  const Context: TPDGSQLiteContext; Options: ISuperObject);
begin
  inherited Create(stObject);
  FNeedReset := false;
  FStHandle := nil;
  DataPtr := Self;
  if ObjectIsType(Options, stString) then
    O['sql'] := Options else
    Merge(Options, true);
  FConnection := Connection;
  AsObject['connection'] := Connection;
  CheckError(sqlite3_prepare_v2(FConnection.FDbHandle, PSOChar(S['sql']), -1, FStHandle, nil), FConnection.FDbHandle);
end;

destructor TPDGSQLiteCommand.Destroy;
begin
  if FStHandle <> nil then
    CheckError(sqlite3_finalize(FStHandle), FConnection.FDbHandle);
  inherited;
end;

function TPDGSQLiteCommand.Execute(const params: ISuperObject;
  const context: IPDGContext): ISuperObject;
var
  dfArray, dfFirstOne: boolean;
  ctx: IPDGContext;

  function getone: ISuperObject;
  var
    i: integer;
    blob: IPDGBlob;
    count: Integer;
  begin
    count := sqlite3_column_count(FStHandle);
    if dfArray then
    begin
      Result := TSuperObject.Create(stArray);
      for i := 0 to count - 1 do
        case sqlite3_column_type(FStHandle, i) of
          SQLITE_TEXT: Result.AsArray.Add(TSuperObject.Create(sqlite3_column_text(FStHandle, i)));
          SQLITE_INTEGER: Result.AsArray.Add(TSuperObject.Create(sqlite3_column_int64(FStHandle, i)));
          SQLITE_FLOAT: Result.AsArray.Add(TSuperObject.Create(sqlite3_column_double(FStHandle, i)));
          SQLITE_BLOB:
            begin
              blob := TPDGBinary.Create;
              blob.getData.Write(sqlite3_column_blob(FStHandle, i)^, sqlite3_column_bytes(FStHandle, i));
              Result.AsArray.Add(blob as ISuperObject);
            end;
         else
           Result.AsArray.Add(nil);
         end;
    end else
    begin
      Result := TSuperObject.Create(stObject);
      for i := 0 to count - 1 do
        case sqlite3_column_type(FStHandle, i) of
          SQLITE_TEXT: Result[sqlite3_column_name(FStHandle, i)] := TSuperObject.Create(sqlite3_column_text(FStHandle, i));
          SQLITE_INTEGER: Result[sqlite3_column_name(FStHandle, i)] := TSuperObject.Create(sqlite3_column_int64(FStHandle, i));
          SQLITE_FLOAT: Result[sqlite3_column_name(FStHandle, i)] := TSuperObject.Create(sqlite3_column_double(FStHandle, i));
          SQLITE_BLOB:
            begin
              blob := TPDGBinary.Create;
              blob.getData.Write(sqlite3_column_blob(FStHandle, i)^, sqlite3_column_bytes(FStHandle, i));
              Result[sqlite3_column_name(FStHandle, i)] := blob as ISuperObject;
            end;
         else
           Result[sqlite3_column_name(FStHandle, i)] := nil;
         end;
    end;
  end;

  procedure SetParam(index: Integer; value: ISuperObject);
  var
    blob: IPDGBlob;
    p: Pointer;
    len: Integer;
    str: SOString;
  begin
    if index > 0 then
      if (Value <> nil) and (value.QueryInterface(IPDGBlob, blob) = 0) then
        with blob.getData do
        begin
          len := size;
          GetMem(p, len);
          Write(p^, len);
          sqlite3_bind_blob(FStHandle, index, p, len, @sqlite3_free);
        end else
        case ObjectGetType(value) of
        stNull: sqlite3_bind_null(FStHandle, index);
        stBoolean: sqlite3_bind_int(FStHandle, index, ord(value.AsBoolean));
        stDouble: sqlite3_bind_double(FStHandle, index, value.AsDouble);
        stInt: sqlite3_bind_int64(FStHandle, index, value.AsInteger);
        {stObject, stArray,} stString:
          begin
            str := value.AsString;
            len := (Length(str) + 1) * SizeOf(SOChar);
            GetMem(p, len);
            Move(PSOChar(str)^, p^, len);
            sqlite3_bind_text(FStHandle, index, p, len, @sqlite3_free)
          end;
        end;
  end;

  procedure Process;
  var
    count: Integer;
    return: Integer;
    busy: Cardinal;
  begin
      count := sqlite3_column_count(FStHandle);
      if (not dfFirstOne) and (count > 0) then
        Result := TSuperObject.Create(stArray) else
        Result := nil;
      busy := 0;
      while True do
      begin
        return := sqlite3_step(FStHandle);
        case return of
          SQLITE_BUSY:
            begin
              if (busy > MAX_BUSY_TIME) then
                CheckError(return, FConnection.FDbHandle);
              inc(busy, 10);
              sleep(10);
            end;
          SQLITE_DONE: Break;
          SQLITE_ROW:
            begin
              if (not dfFirstOne) then
                Result.AsArray.Add(getone) else
                begin
                  Result := getone;
                  Break;
                end;
              busy := 0;
            end;
        else
          CheckError(return, FConnection.FDbHandle);
        end;
      end;
  end;
var
  j, count: integer;
  f: TSuperObjectIter;
begin
  if FNeedReset then
    sqlite3_reset(FStHandle) else
    FNeedReset := true;

  ctx := context;
  dfFirstOne := B['firstone'];
  dfArray := B['array'];

  if ctx = nil then
    ctx := FConnection.newContext;

  count := sqlite3_bind_parameter_count(FStHandle);
  for j := 1 to count do
     sqlite3_bind_null(FStHandle, j);
  try
    if count > 0 then
    begin
      if ObjectIsType(params, stArray) then
      begin
        with params.AsArray do
        begin
          if (Length = count) and not(ObjectGetType(O[0]) in [stObject, stArray]) then
          begin
            for j := 0 to Length - 1 do
              SetParam(j+1, O[j]);
            Process;
          end else
            if count > 0 then
            begin
              Result := TSuperObject.Create(stArray);
              for j := 0 to Length - 1 do
                Result.AsArray.Add(Execute(O[j], ctx));
            end else
            begin
              for j := 0 to Length - 1 do
                Execute(O[j], ctx);
            end;
        end;
      end else
      if ObjectIsType(params, stObject) then
      begin
        if ObjectFindFirst(params, f) then
        repeat
          SetParam(sqlite3_bind_parameter_index(FStHandle, PAnsiChar(UTF8Encode(':' + f.key))), f.val);
        until not ObjectFindNext(f);
        ObjectFindClose(f);
        Process;
      end else
      begin
        SetParam(1, params);
        Process;
      end;
    end else
      Process;
  except
    (ctx as ISuperObject).B['rollback'] := true;
    raise;
  end;
end;


function TPDGSQLiteCommand.GetInputMeta: ISuperObject;
var
  j: Integer;
  count: Integer;
  rec: ISuperObject;
  name: PAnsiChar;
begin
  count := sqlite3_bind_parameter_count(FStHandle);
  if count > 0 then
  begin
    Result := TSuperObject.Create(stArray);
    with Result.AsArray do
      for j := 1 to count do
      begin
        rec := TSuperObject.Create(stObject);
        name := sqlite3_bind_parameter_name(FStHandle, j);
        if name <> nil then
{$if sizeof(char) = 1}
          rec.S['name'] := UTF8Decode(name);
{$else}
          rec.S['name'] := UTF8ToString(name);
{$ifend}
        add(rec);
      end;
  end else
    Result := nil;
end;

function TPDGSQLiteCommand.GetOutputMeta: ISuperObject;
var
  j, count: Integer;
  rec: ISuperObject;
  dfArray: Boolean;
  sType: SOString;
  v: Integer;
begin
  count := sqlite3_column_count(FStHandle);
  if count > 0 then
  begin
    dfArray := B['array'];
    if dfArray then
      Result := TSuperObject.Create(stArray) else
      Result := TSuperObject.Create(stObject);

      for j := 0 to count - 1 do
      begin
        rec := TSuperObject.Create(stObject);
        if dfArray then
        begin
          rec.S['name'] := sqlite3_column_name(FStHandle, j);
          Result.asArray.add(rec);
        end else
          Result.AsObject[sqlite3_column_name(FStHandle, j)] := rec;
        sType := sqlite3_column_decltype(FStHandle, j);
        v := pos('(', sType);
        if v = 0 then
          rec.S['type'] := trim(sType) else
          begin
            rec.S['type'] := trim(Copy(sType, 1, v - 1));
            sType := Copy(sType, v+1, length(sType) - v);
            v := pos(',', sType);
            if v = 0 then
            begin
              v := pos(')', sType);
              if v > 0 then
                rec.I['length'] := StrToInt(copy(sType, 1, v-1));
            end;
          end;
      end;
  end else
    Result := nil;
end;

end.
