unit UIBCodeGen;
{$I uib.inc}
interface
uses uib, uiblib, Classes, Contnrs;

type

  TUIBCodeGenerator = class
  private
    FUnitName: string;
    FTransaction: TUIBTransaction;
    FSQLObjects: TObjectList;
  public
    constructor Create(const AUnitName: string; Transaction: TUIBTransaction); virtual;
    destructor Destroy; override;
    procedure AddStatement(const Name, SQL: string; paramtypes: array of TUIBFieldType;
      QInsert: string = ''; QUpdate: string = ''; QDelete: string = '');
    procedure SaveToStream(stream: TStream);
    procedure SaveToFile(const FileName: string);
    property UnitName: string read FUnitName write FUnitName;
  end;

implementation
uses sysutils, uibmetadata;


const
  SQLDelphiType: array[TUIBFieldType] of string =
    ('','Double','string','string','','Smallint','Integer','TStream','Single',
     'Double','TDateTime','TStream','','TDate','TTime','Int64','Array'
     {$IFDEF IB7_UP},'Boolean'{$ENDIF});

  SQLManaged: set of TUIBFieldType = [
    uftNumeric, uftChar, uftVarchar, uftSmallint,
    uftInteger, uftQuad, uftFloat, uftDoublePrecision, uftTimestamp, uftBlob,
    uftDate, uftTime, uftInt64 {$IFDEF IB7_UP}, uftBoolean{$ENDIF}];

  SQLString: set of TUIBFieldType = [uftChar, uftVarchar];
  SQLBlob: set of TUIBFieldType = [uftQuad, uftBlob];


type
  TQueryCodeGen = class(TUIBQuery)
  public
    QInsert, QUpdate, QDelete: string;
  end;


{ TUIBCodeGenerator }

procedure TUIBCodeGenerator.AddStatement(const Name, SQL: string; paramtypes: array of TUIBFieldType; QInsert, QUpdate, QDelete: string);
var
  Q: TQueryCodeGen;
  i, j: integer;
begin
  Q := TQueryCodeGen.Create(nil);
  Q.Name := Name;
  Q.SQL.Text := SQL;
  // set param types
  j := 0;
  for i := 0 to Q.Params.ParamCount - 1 do
  begin
    if j >= length(paramtypes) then
      raise Exception.Create('set data types for parametters !');
    if Q.Params.FieldType[i] = uftUnKnown then
    begin
      case paramtypes[j] of
        uftNumeric          : Q.Params.AsDouble[i] := 0;
        uftChar             : Q.Params.AsString[i] := '';
        uftVarchar          : Q.Params.AsString[i] := '';
        uftSmallint         : Q.Params.AsSmallint[i] := 0;
        uftInteger          : Q.Params.AsInteger[i] := 0;
        uftFloat            : Q.Params.AsSingle[i] := 0;
        uftDoublePrecision  : Q.Params.AsDouble[i] := 0;
        uftTimestamp        : Q.Params.AsDateTime[i] := 0;
        uftBlob, uftQuad    : Q.Params.AsQuad[i] := QuadNull;
        uftDate             : Q.Params.AsDate[i] := 0;
        uftTime             : Q.Params.AsTime[i] := 0;
        uftInt64            : Q.Params.AsInt64[i] := 0;
      {$IFDEF IB7_UP}
        uftBoolean          : Q.Params.AsBoolean[i] := false;
      {$ENDIF}
      else
        raise Exception.Create('unexpected');
      end;
      Q.Params.IsNull[i] := true;
      inc(j);
    end;
  end;
  Q.Transaction := FTransaction;
  Q.QInsert := QInsert;
  Q.QUpdate := QUpdate;
  Q.QDelete := QDelete;
  FSQLObjects.Add(Q);
end;

constructor TUIBCodeGenerator.Create(const AUnitName: string; Transaction: TUIBTransaction);
begin
  FUnitName := AUnitName;
  FSQLObjects := TObjectList.Create;
  FTransaction := Transaction;
end;

destructor TUIBCodeGenerator.Destroy;
begin
  FSQLObjects.Free;
  inherited;
end;

procedure TUIBCodeGenerator.SaveToFile(const FileName: string);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TUIBCodeGenerator.SaveToStream(stream: TStream);
var
  meta: TMetaDataBase;

  procedure write(str: string); overload;
  var
    len: integer;
  begin
    len := length(str);
    if len > 0 then
      stream.Write(str[1], len);
  end;
  procedure write(str: string; params: array of const); overload;
  begin
    write(format(str, params));
  end;
  procedure writeln(str: string = ''); overload;
  begin
    write(str + #13#10);
  end;
  procedure writeln(str: string; params: array of const); overload;
  begin
    write(str + #13#10, params);
  end;

  function FieldExist(const table, field: string): boolean;
  var
    t: TMetaTable;
    f: TMetaTableField;
  begin
    Result := False;
    t := meta.FindTableName(table);
    if t <> nil then
    begin
      f := t.FindFieldName(field);
      Result := f.ComputedSource = '';
    end;
  end;

  procedure WriteInterface;
  var
    i, j: integer;
    Q: TQueryCodeGen;
  begin
    writeln('interface');
    writeln('uses Classes, Contnrs, uib;', [FUnitName]);
    writeln;
    writeln('type');
    for i := 0 to FSQLObjects.Count - 1 do
    begin
      writeln;
      Q := TQueryCodeGen(FSQLObjects[i]);
      Q.Prepare;
      writeln('  T%sList = class;', [Q.Name]);
      writeln('  T%s = class(TPersistent)', [Q.Name]);
      with Q.Fields do
      begin
        writeln('  private');
        writeln('    FData: Pointer;');
        writeln('    FState: integer;');
        for j := 0 to FieldCount - 1 do
          if FieldType[j] in SQLManaged then
            writeln('    F%s: %s;', [AliasName[j], SQLDelphiType[FieldType[j]]]);
        for j := 0 to FieldCount - 1 do
          if (FieldType[j] in SQLManaged) and IsNullable[j] then
             writeln('    F%s_IsNull: boolean;', [AliasName[j]]);
        for j := 0 to FieldCount - 1 do
          if (FieldType[j] in SQLManaged) and FieldExist(RelName[j], SqlName[j]) then
            if (FieldType[j] in SQLString) then
              writeln('    procedure Set_%s(const A%0:s: string);', [AliasName[j]]) else
              writeln('    procedure Set_%0:s(A%0:s: %1:s);', [AliasName[j], SQLDelphiType[FieldType[j]]]);
        for j := 0 to FieldCount - 1 do
          if (FieldType[j] in SQLManaged) and IsNullable[j] then
            writeln('    procedure Set_%s_IsNull(AValue: boolean);', [AliasName[j]]);
        writeln('  protected');
        writeln('    procedure AssignTo(Dest: TPersistent); override;');
        writeln('  public');
        writeln('    constructor Create; virtual;');
        writeln('    constructor CreateFromStream(stream: TStream);');
        writeln('    destructor Destroy; override;');
        writeln('    procedure SaveToStream(stream: TStream);');
        writeln('    property Data: Pointer read FData write FData;');
        writeln('  published');
        for j := 0 to FieldCount - 1 do
          if FieldType[j] in SQLManaged then
            if FieldType[j] in SQLManaged then
            begin
              if FieldExist(RelName[j], SqlName[j]) then
                writeln('    property %0:s: %1:s read F%0:s write Set_%0:s;', [AliasName[j], SQLDelphiType[FieldType[j]]]) else
                writeln('    property %0:s: %1:s read F%0:s;', [AliasName[j], SQLDelphiType[FieldType[j]]]);
            end else
              writeln('    property %0:s: %1:s read F%0:s;', [AliasName[j], SQLDelphiType[FieldType[j]]]);
        for j := 0 to FieldCount - 1 do
          if (FieldType[j] in SQLManaged) and IsNullable[j] then
             writeln('    property %s_IsNull: boolean read F%0:s_IsNull write Set_%0:s_IsNull;', [AliasName[j]]);
      end;
      writeln('  end;');

      // OBJECT LIST
      writeln;
      writeln('  T%0:sList = class(TList)', [Q.Name]);
      writeln('  private');
      writeln('    FDeleted: TObjectList;');
      writeln('    FUpdating: boolean;');
      writeln('    function Get%0:s(index: integer): T%0:s;', [Q.Name]);
      writeln('    procedure Set%0:s(index: integer; A%0:s: T%0:s);', [Q.Name]);
      writeln('  protected');
      writeln('    procedure Notify(Ptr: Pointer; Action: TListNotification); override;');
      writeln('  public');
      writeln('    constructor Create; overload;');
      writeln('    destructor Destroy; override;');
      write('    procedure LoadFromDatabase(Transaction: TUIBTransaction');
      with Q.Params do
        for j := 0 to ParamCount - 1 do
          write('; %s: %s', [FieldName[j], SQLDelphiType[FieldType[j]]]);
      writeln(');');
      writeln('    procedure SaveToStream(stream: TStream; changes: boolean = false);');
      writeln('    procedure SaveToFile(const FileName: string; changes: boolean = false);');
      writeln('    procedure LoadFromStream(stream: TStream);');
      writeln('    procedure LoadFromFile(const FileName: string);');
      writeln('    function Apply(Transaction: TUIBTransaction): boolean;');
      writeln('    property Items[Index: Integer]: T%0:s read Get%0:s write Set%0:s; default;', [Q.Name]);
      writeln('  end;');
    end;
  end;

  procedure WriteImplementation;
  var
    i, j: integer;
    Q: TQueryCodeGen;
    QInsert, QUpdate, QDelete: TUIBQuery;
  begin
    QInsert := TUIBQuery.Create(nil);
    QUpdate := TUIBQuery.Create(nil);
    QDelete := TUIBQuery.Create(nil);
    try
      QInsert.Transaction := FTransaction;
      QUpdate.Transaction := FTransaction;
      QDelete.Transaction := FTransaction;

      writeln;
      writeln('implementation');
      writeln('uses SysUtils;');
      for i := 0 to FSQLObjects.Count - 1 do
      begin
        writeln;
        Q := TQueryCodeGen(FSQLObjects[i]);
        writeln('{ T%s }', [Q.Name]);
        with Q.Fields do
        begin

          // CONSTRUCTOR
          writeln;
          writeln('constructor T%0:s.create;', [Q.Name]);
          writeln('begin');
          writeln('  FData := nil;');
          writeln('  FState := 0;');
          for j := 0 to FieldCount - 1 do
            if FieldType[j] in SQLManaged then
            case FieldType[j] of
              uftChar, uftVarchar: writeln('  F%s := '''';', [AliasName[j]]);
              uftBlob, uftQuad: writeln('  F%s := nil;', [AliasName[j]]);
            {$IFDEF IB7_UP}
              uftBoolean: writeln('  F%s := false;', [AliasName[j]]);
            {$ENDIF}
            else
              writeln('  F%s := 0;', [AliasName[j]]);
            end;
          for j := 0 to FieldCount - 1 do
            if (FieldType[j] in SQLManaged) and IsNullable[j] then
              writeln('  F%s_isNull := true;', [AliasName[j]]);
          writeln('end;');

          writeln;
          writeln('constructor T%s.CreateFromStream(stream: TStream);', [Q.Name]);
          writeln('var');
          writeln('  len: integer;');
          writeln('begin');
          writeln('  FData := nil;');
          writeln('  stream.Read(FState, sizeof(FState));');
          with Q.Fields do
          for j := 0 to FieldCount - 1 do
            if FieldType[j] in SQLManaged then
            begin
              if IsNullable[j] then
              begin
                writeln('  stream.Read(F%s_IsNull, sizeof(F%0:s_IsNull));', [AliasName[j]]);
                writeln('  if not F%s_IsNull then', [AliasName[j]]);
              end;
                case FieldType[j] of
                  uftChar, uftVarchar:
                    begin
                      if IsNullable[j] then
                      begin
                        writeln('  begin');
                        writeln('    stream.Read(len, sizeof(len));');
                        writeln('    if len > 0 then');
                        writeln('    begin');
                        writeln('      setLength(F%s, len);', [AliasName[j]]);
                        writeln('      stream.Read(F%s[1], len);', [AliasName[j]]);
                        writeln('    end;');
                        writeln('  end;');
                      end else
                      begin
                        writeln('  stream.Read(len, sizeof(len));');
                        writeln('  if len > 0 then');
                        writeln('  begin');
                        writeln('    setLength(F%s, len);', [AliasName[j]]);
                        writeln('    stream.Read(F%s[1], len);', [AliasName[j]]);
                        writeln('  end;');
                      end;
                    end;
                  uftBlob, uftQuad:
                    begin
                      if IsNullable[j] then
                      begin
                        writeln('  begin');
                        writeln('    stream.Read(len, sizeof(len));');
                        writeln('    if len > 0 then');
                        writeln('    begin');
                        writeln('      F%s := TMemoryStream.Create;', [AliasName[j]]);
                        writeln('      F%s.CopyFrom(stream, len);', [AliasName[j]]);
                        writeln('    end;');
                        writeln('  end;');
                      end else
                      begin
                        writeln('  stream.Read(len, sizeof(len));');
                        writeln('  if len > 0 then');
                        writeln('  begin');
                        writeln('    F%s := TMemoryStream.Create;', [AliasName[j]]);
                        writeln('    F%s.CopyFrom(stream, len);', [AliasName[j]]);
                        writeln('  end;');
                      end;
                    end;
                else
                    writeln('  stream.Read(F%s, sizeof(F%0:s));', [AliasName[j]]);
                end;
            end;
          writeln('end;');

          //DESTRUCTOR
          writeln;
          writeln('destructor T%0:s.destroy;', [Q.Name]);
          writeln('begin');
          writeln('  inherited destroy;');
          for j := 0 to FieldCount - 1 do
            if FieldType[j] in [uftBlob, uftQuad] then
              writeln('  if F%s <> nil then F%0:s.Free;', [AliasName[j]]);
          writeln('end;');

          //SETTERS
            for j := 0 to FieldCount - 1 do
            if (FieldType[j] in SQLManaged) and FieldExist(RelName[j], SqlName[j]) then
            if (FieldType[j] in SQLString) then
              begin
                writeln;
                writeln('procedure T%0:s.Set_%1:s(const A%1:s: string);', [Q.Name, AliasName[j]]);
                writeln('begin');
                writeln('  if Length(%s) > %d then', [AliasName[j], SQLLen[j]]);
                writeln('    raise ERangeError.Create(''range error'');');
                if IsNullable[j] then
                  writeln('  F%s_IsNull := false;', [AliasName[j]]);
                writeln('  F%s := A%0:s;', [AliasName[j]]);
                writeln('  if FState = 0 then FState := 1;');
                writeln('end;');
              end else
              if FieldType[j] in [uftBlob, uftQuad] then
              begin
                writeln;
                writeln(
                'procedure T%0:s.Set_%1:s(A%1:s: TStream);'#13#10 +
                'begin'#13#10 +
                '  if A%1:s = nil then'#13#10 +
                '  begin'#13#10 +
                '    if F%1:s <> nil then'#13#10 +
                '      FreeAndNil(F%1:s);'#13#10 +
                '  end else'#13#10 +
                '  begin'#13#10 +
                '    if F%1:s = nil then'#13#10 +
                '      F%1:s := TMemoryStream.Create;'#13#10 +
                '    TMemoryStream(F%1:s).LoadFromStream(A%1:s);'#13#10 +
                '  end;', [Q.Name, AliasName[j]]);
                if IsNullable[j] then
                begin
                  writeln(
                  '  if F%0:s = nil then'#13#10+
                  '    F%0:s_IsNull := true else'#13#10 +
                  '    F%0:s_IsNull := false;', [AliasName[j]]);
                  writeln('  if FState = 0 then FState := 1;');
                end;
                writeln('end;');
              end else
              begin
                writeln;
                writeln('procedure T%0:s.Set_%1:s(A%1:s: %2:s);', [Q.Name, AliasName[j], SQLDelphiType[FieldType[j]]]);
                writeln('begin');
                if IsNullable[j] then
                  writeln('  F%s_IsNull := false;', [AliasName[j]]);
                writeln('  F%s := A%0:s;', [AliasName[j]]);
                writeln('  if FState = 0 then FState := 1;');
                writeln('end;');
              end;
          for j := 0 to FieldCount - 1 do
            if (FieldType[j] in SQLManaged) and IsNullable[j] then
              begin
                writeln;
                writeln('procedure T%s.Set_%s_IsNull(AValue: boolean);', [Q.Name, AliasName[j]]);
                writeln('begin');
                writeln('  F%s_IsNull := AValue;', [AliasName[j]]);
                writeln('  if FState = 0 then FState := 1;');
                writeln('end;');
              end;

          writeln;
          writeln('procedure T%s.SaveToStream(stream: TStream);', [Q.Name]);
          writeln('var');
          writeln('  len: integer;');
          writeln('begin');
          writeln('  stream.Write(FState, sizeof(FState));');
          with Q.Fields do
            for j := 0 to FieldCount - 1 do
              if (FieldType[j] in SQLManaged) then
              begin
                if IsNullable[j] then
                begin
                  writeln('  stream.write(F%s_IsNull, sizeof(F%0:s_IsNull));', [AliasName[j]]);
                  writeln('  if not F%s_IsNull then', [AliasName[j]]);
                end;
                case FieldType[j] of
                  uftChar, uftVarchar:
                    begin
                      if IsNullable[j] then
                      begin
                        writeln('  begin');
                        writeln('    len := Length(F%s);', [AliasName[j]]);
                        writeln('    stream.Write(len, sizeof(len));');
                        writeln('    if len > 0 then');
                        writeln('      stream.Write(F%s[1], len);', [AliasName[j]]);
                        writeln('  end;');
                      end else
                      begin
                        writeln('  len := Length(F%s);', [AliasName[j]]);
                        writeln('  stream.Write(len, sizeof(len));');
                        writeln('  if len > 0 then');
                        writeln('    stream.Write(F%s[1], len);', [AliasName[j]]);
                      end;
                    end;
                  uftBlob, uftQuad:
                    begin
                      writeln('  if F%s <> nil then', [AliasName[j]]);
                      writeln('  begin');
                      writeln('    len := F%s.Size;', [AliasName[j]]);
                      writeln('    stream.Write(len, sizeof(len));');
                      writeln('    if len > 0 then');
                      writeln('      TMemoryStream(F%s).SaveToStream(stream);', [AliasName[j]]);
                      writeln('  end else');
                      writeln('  begin');
                      writeln('    len := 0;');
                      writeln('    stream.Write(len, sizeof(len));');
                      writeln('  end;');
                    end;
                else
                    writeln('  stream.write(F%s, sizeof(F%0:s));', [AliasName[j]]);
                end;
              end;
          writeln('end;');


          writeln;
          writeln('procedure T%s.AssignTo(Dest: TPersistent);', [Q.Name]);
          writeln('begin');
          writeln('  with Dest as T%s do', [Q.Name]);
          writeln('  begin');
          with Q.Fields do
            for j := 0 to FieldCount - 1 do
              if (FieldType[j] in SQLManaged) then
              begin
                if isnullable[j] then
                begin
                  if FieldExist(RelName[j], SqlName[j]) then
                  begin
                    writeln('    %s_IsNull := Self.F%0:s_IsNull;', [AliasName[j]]);
                    writeln('    if not %s_IsNull then', [AliasName[j]]);
                  end else
                  begin
                    writeln('    F%s_IsNull := Self.F%0:s_IsNull;', [AliasName[j]]);
                    writeln('    if not F%s_IsNull then', [AliasName[j]]);
                  end;
                end;
                if FieldExist(RelName[j], SqlName[j]) then
                  writeln('    %s := Self.F%0:s;', [AliasName[j]]) else
                  writeln('    F%s := Self.F%0:s;', [AliasName[j]]);
              end;
          writeln('  end;');
          writeln('end;');

          writeln;
          writeln('{ T%sList }', [Q.Name]);
          writeln;
          writeln('function T%0:sList.Get%0:s(index: integer): T%0:s;', [Q.Name]);
          writeln('begin');
          writeln('  Result := T%0:s(inherited Get(index));', [Q.Name]);
          writeln('end;');
          writeln;
          writeln('procedure T%0:sList.Set%0:s(index: integer; A%0:s: T%0:s);', [Q.Name]);
          writeln('begin');
          writeln('  inherited Put(index, A%0:s);', [Q.Name]);
          writeln('end;');

          writeln;
          writeln('constructor T%sList.Create;', [Q.Name]);
          writeln('begin');
          writeln('  inherited Create;');
          writeln('  FUpdating := false;');
          writeln('  FDeleted := TObjectList.Create;');
          writeln('end;');

          writeln;
          writeln('destructor T%sList.Destroy;', [Q.Name]);
          writeln('begin');
          writeln('  inherited Destroy;');
          writeln('  FDeleted.Free;');
          writeln('end;');


          // LOADFROMDATABASE
          writeln;
          write('procedure T%sList.LoadFromDatabase(Transaction: TUIBTransaction', [Q.Name]);
          with Q.Params do
            for j := 0 to ParamCount - 1 do
              write('; %s: %s', [FieldName[j], SQLDelphiType[FieldType[j]]]);
          writeln(');');
          writeln('var');
          writeln('  Q: TUIBQuery;');
          writeln('  O: T%s;', [Q.Name]);
          with Q.Fields do
            for j := 0 to FieldCount - 1 do
              if (FieldType[j] in SQLManaged) then
                writeln('  %s_index: integer;', [AliasName[j]]);

          writeln('begin');
          writeln('  FUpdating := true;');
          writeln('  Q := TUIBQuery.Create(nil);');
          writeln('  try');
          writeln('    Clear;');
          writeln('    FDeleted.Clear;');
          writeln('    Q.Transaction := Transaction;');
          writeln('    Q.SQL.Text := ''%s'';', [trim(Q.SQL.Text)]);
          writeln('    Q.CachedFetch := False;');
          with Q.Params do
          for j := 0 to ParamCount - 1 do
            case FieldType[j] of
              uftNumeric          : writeln('    Q.Params.ByNameAsDouble[''%s''] := %0:s;', [FieldName[j]]);
              uftChar             : writeln('    Q.Params.ByNameAsString[''%s''] := %0:s;', [FieldName[j]]);
              uftVarchar          : writeln('    Q.Params.ByNameAsString[''%s''] := %0:s;', [FieldName[j]]);
              uftSmallint         : writeln('    Q.Params.ByNameAsSmallint[''%s''] := %0:s;', [FieldName[j]]);
              uftInteger          : writeln('    Q.Params.ByNameAsInteger[''%s''] := %0:s;', [FieldName[j]]);
              uftFloat            : writeln('    Q.Params.ByNameAsSingle[''%s''] := %0:s;', [FieldName[j]]);
              uftDoublePrecision  : writeln('    Q.Params.ByNameAsDouble[''%s''] := %0:s;', [FieldName[j]]);
              uftTimestamp        : writeln('    Q.Params.ByNameAsDateTime[''%s''] := %0:s;', [FieldName[j]]);
              uftBlob, uftQuad    : writeln('    Q.ParamsSetBlob(''%s'', %0:s);', [FieldName[j]]);
              uftDate             : writeln('    Q.Params.ByNameAsDate[''%s''] := %0:s;', [FieldName[j]]);
              uftTime             : writeln('    Q.Params.ByNameAsTime[''%s''] := %0:s;', [FieldName[j]]);
              uftInt64            : writeln('    Q.Params.ByNameAsInt64[''%s''] := %0:s;', [FieldName[j]]);
            {$IFDEF IB7_UP}
              uftBoolean          : writeln('    Q.Params.ByNameAsBoolean[''%s''] := %0:s;', [FieldName[j]]);
            {$ENDIF}
            end;
          writeln('    Q.Open;');
          writeln('    with Q.Fields do');
          writeln('    begin');
          with Q.Fields do
            for j := 0 to FieldCount - 1 do
              if (FieldType[j] in SQLManaged) then
                writeln('      %s_index := getFieldIndex(''%0:s'');', [AliasName[j]]);

          writeln('    end;');
          writeln('    while not Q.Eof do');
          writeln('    with Q.Fields do');
          writeln('    begin');
          writeln('      O := T%s.create;', [Q.Name]);
          with Q.Fields do
            for j := 0 to FieldCount - 1 do
              if (FieldType[j] in SQLManaged) then
              begin
                if IsNullable[j] then
                begin
                  writeln('      if not IsNull[%s_index] then', [AliasName[j]]);
                  writeln('      begin');
                  write('  ');
                end;
                case FieldType[j] of
                  uftNumeric          : writeln('      O.F%s := AsDouble[%0:s_index];', [AliasName[j]]);
                  uftChar             : writeln('      O.F%s := AsString[%0:s_index];', [AliasName[j]]);
                  uftVarchar          : writeln('      O.F%s := AsString[%0:s_index];', [AliasName[j]]);
                  uftSmallint         : writeln('      O.F%s := AsSmallint[%0:s_index];', [AliasName[j]]);
                  uftInteger          : writeln('      O.F%s := AsInteger[%0:s_index];', [AliasName[j]]);
                  uftFloat            : writeln('      O.F%s := AsSingle[%0:s_index];', [AliasName[j]]);
                  uftDoublePrecision  : writeln('      O.F%s := AsDouble[%0:s_index];', [AliasName[j]]);
                  uftTimestamp        : writeln('      O.F%s := AsDateTime[%0:s_index];', [AliasName[j]]);
                  uftBlob, uftQuad    :
                    begin
                      writeln('      O.F%s := TMemoryStream.Create;', [AliasName[j]]);
                      if IsNullable[j] then write('  ');
                      writeln('      Q.ReadBlob(%s_index, O.F%0:s);', [AliasName[j]]);
                    end;
                  uftDate             : writeln('      O.F%s := AsDate[%0:s_index];', [AliasName[j]]);
                  uftTime             : writeln('      O.F%s := AsTime[%0:s_index];', [AliasName[j]]);
                  uftInt64            : writeln('      O.F%s := AsInt64[%0:s_index];', [AliasName[j]]);
                {$IFDEF IB7_UP}
                  uftBoolean          : writeln('      O.F%s := AsBoolean[%0:s_index];', [AliasName[j]]);
                {$ENDIF}
                end;
                if IsNullable[j] then
                begin
                  writeln('        O.F%s_IsNull := false;', [AliasName[j]]);
                  writeln('      end;');
                end;
              end;
          writeln('      Add(O);');
          writeln('      Q.Next;');
          writeln('    end;');
          writeln('  finally');
          writeln('    FUpdating := False;');
          writeln('    Q.Free;');
          writeln('  end;');
          writeln('end;');

          //SAVETOFILE
          writeln;
          writeln('procedure T%sList.SaveToFile(const FileName: string; changes: boolean);', [Q.Name]);
          writeln('var');
          writeln('  stream: TFileStream;');
          writeln('begin');
          writeln('  stream := TFileStream.Create(FileName, fmCreate);');
          writeln('  try');
          writeln('    SaveToStream(stream, changes);');
          writeln('  finally');
          writeln('    stream.Free');
          writeln('  end;');
          writeln('end;');

          //SAVETOSTREAM
          writeln;
          writeln('procedure T%sList.SaveToStream(stream: TStream; changes: boolean);', [Q.Name]);
          writeln('var');
          writeln('  i, len: integer;');
          writeln('begin');
          writeln('  len := FDeleted.Count;');
          writeln('  if changes then');
          writeln('  begin');
          writeln('    for i := 0 to Count - 1 do');
          writeln('      if Items[i].FState <> 0 then');
          writeln('        inc(len);');
          writeln('  end else');
          writeln('    inc(len, count);');
          writeln('  stream.Write(len, sizeof(len));');
          writeln('  for i := 0 to Count - 1 do');
          writeln('    if not changes or (Items[i].FState <> 0) then');
          writeln('      Items[i].SaveToStream(stream);');
          writeln('  for i := 0 to FDeleted.Count - 1 do');
          writeln('    T%s(FDeleted[i]).SaveToStream(stream);', [Q.Name]);
          writeln('end;');

          //LOADFROMSTREAM
          writeln;
          writeln('procedure T%sList.LoadFromStream(stream: TStream);', [Q.Name]);
          writeln('var');
          writeln('  i: integer;');
          writeln('  O: T%s;', [Q.Name]);
          writeln('begin');
          writeln('  FUpdating := true;');
          writeln('  try');
          writeln('    Clear;');
          writeln('    FDeleted.Clear;');
          writeln('    stream.Read(i, sizeof(i));');
          writeln('    for i := 0 to i - 1 do');
          writeln('    begin');
          writeln('      O := T%s.CreateFromStream(stream);', [Q.Name]);
          writeln('      if O.FState = -1 then');
          writeln('        FDeleted.Add(O) else');
          writeln('        Add(O);');
          writeln('    end;');
          writeln('  finally');
          writeln('    FUpdating := false;');
          writeln('  end;');
          writeln('end;');

          //LOADFROMFILE
          writeln;
          writeln('procedure T%sList.LoadFromFile(const FileName: string);', [Q.Name]);
          writeln('var');
          writeln('  stream: TFileStream;');
          writeln('begin');
          writeln('  stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);');
          writeln('  try');
          writeln('    LoadFromStream(stream);');
          writeln('  finally');
          writeln('    stream.Free');
          writeln('  end;');
          writeln('end;');

          //NOTIFY
          writeln;
          writeln('procedure T%sList.Notify(Ptr: Pointer; Action: TListNotification);', [Q.Name]);
          writeln('var');
          writeln('  O: T%s;', [Q.Name]);
          writeln('begin');
          writeln('  if FUpdating then exit;');
          writeln('  case Action of');
          writeln('    lnDeleted:');
          writeln('      begin');
          writeln('        if (T%s(Ptr).FState <> 2) then', [Q.Name]);
          writeln('        begin');
          writeln('          T%s(Ptr).FState := -1;', [Q.Name]);
          writeln('          FDeleted.Add(Ptr);');
          writeln('        end else');
          writeln('          TObject(Ptr).Free;');
          writeln('      end;');
          writeln('    lnExtracted:');
          writeln('      begin');
          writeln('        if (T%s(Ptr).FState <> 2) then', [Q.Name]);
          writeln('        begin');
          writeln('          O := T%s.Create;', [Q.Name]);
          writeln('          T%s(Ptr).AssignTo(O);', [Q.Name]);
          writeln('          O.FState := -1;');
          writeln('          FDeleted.Add(O);');
          writeln('        end;');
          writeln('      end;');
          writeln('    lnAdded:');
          writeln('      T%s(Ptr).FState := 2;', [Q.Name]);
          writeln('  end;');
          writeln('end;');

          writeln;
          QInsert.SQL.Text := Q.QInsert;
          QUpdate.SQL.Text := Q.QUpdate;
          QDelete.SQL.Text := Q.QDelete;

          if QInsert.SQL.Text <> '' then QInsert.Prepare;
          if QUpdate.SQL.Text <> '' then QUpdate.Prepare;
          if QDelete.SQL.Text <> '' then QDelete.Prepare;

          writeln('function T%sList.Apply(Transaction: TUIBTransaction): boolean;', [Q.Name]);
          writeln('var');
          writeln('  QInsert, QUpdate, QDelete: TUIBQuery;');
          writeln('  i: integer;');
          writeln('  expected, done: integer;');
          writeln('  procedure doObject(O: T%s);', [Q.Name]);
          writeln('  begin');
          writeln('    inc(expected);');
          writeln('    case O.FState of');
          writeln('      -1: with QDelete, Params do');
          writeln('          begin');
          if QDelete.SQL.Text <> '' then
          begin
            with QDelete do
            for j := 0 to Params.ParamCount - 1 do
            begin
              if Q.Fields.ByNameIsNullable[Params.FieldName[j]] then
              begin
                writeln('            if O.%s_IsNull then', [Params.FieldName[j]]);
                writeln('            ByNameIsNull[''%s''] := true else', [Params.FieldName[j]]);
              end;
              case Q.Fields.FieldType[Q.Fields.GetFieldIndex(Params.FieldName[j])] of
                uftInteger          : writeln('            ByNameAsInteger[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftNumeric          : writeln('            ByNameAsDouble[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftChar             : writeln('            ByNameAsString[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftVarchar          : writeln('            ByNameAsString[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftSmallint         : writeln('            ByNameAsSmallint[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftFloat            : writeln('            ByNameAsSingle[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftDoublePrecision  : writeln('            ByNameAsDouble[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftTimestamp        : writeln('            ByNameAsDateTime[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftBlob, uftQuad    : writeln('            ParamsSetBlob(''%s'', O.%0:s);', [Params.FieldName[j]]);
                uftDate             : writeln('            ByNameAsDate[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftTime             : writeln('            ByNameAsTime[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftInt64            : writeln('            ByNameAsInt64[''%s''] := O.%0:s;', [Params.FieldName[j]]);
              {$IFDEF IB7_UP}
                uftBoolean          : writeln('            ByNameAsBoolean[''%s''] := O.%0:s;', [Params.FieldName[j]]);
              {$ENDIF}
              end;
            end;
            writeln('            Execute;');
            writeln('            if RowsAffected > 0 then');
            writeln('              inc(done);');
          end;
          writeln('          end;');
          writeln('       1: with QUpdate, Params do');
          writeln('          begin');
          if QUpdate.SQL.Text <> '' then
          begin
            with QUpdate do
            for j := 0 to Params.ParamCount - 1 do
            begin
              if Q.Fields.ByNameIsNullable[Params.FieldName[j]] then
              begin
                writeln('            if O.%s_IsNull then', [Params.FieldName[j]]);
                writeln('            ByNameIsNull[''%s''] := true else', [Params.FieldName[j]]);
              end;
              case Q.Fields.FieldType[Q.Fields.GetFieldIndex(Params.FieldName[j])] of
                uftInteger          : writeln('            ByNameAsInteger[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftNumeric          : writeln('            ByNameAsDouble[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftChar             : writeln('            ByNameAsString[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftVarchar          : writeln('            ByNameAsString[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftSmallint         : writeln('            ByNameAsSmallint[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftFloat            : writeln('            ByNameAsSingle[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftDoublePrecision  : writeln('            ByNameAsDouble[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftTimestamp        : writeln('            ByNameAsDateTime[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftBlob, uftQuad    : writeln('            ParamsSetBlob(''%s'', O.%0:s);', [Params.FieldName[j]]);
                uftDate             : writeln('            ByNameAsDate[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftTime             : writeln('            ByNameAsTime[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftInt64            : writeln('            ByNameAsInt64[''%s''] := O.%0:s;', [Params.FieldName[j]]);
              {$IFDEF IB7_UP}
                uftBoolean          : writeln('            ByNameAsBoolean[''%s''] := O.%0:s;', [Params.FieldName[j]]);
              {$ENDIF}
              end;
            end;
            writeln('            Execute;');
            writeln('            if RowsAffected > 0 then');
            writeln('              inc(done);');
          end;
          writeln('          end;');
          writeln('       2: with QInsert, Params do');
          writeln('          begin');
          if QInsert.SQL.Text <> '' then
          begin
            with QInsert do
            for j := 0 to Params.ParamCount - 1 do
            begin
              if Q.Fields.ByNameIsNullable[Params.FieldName[j]] then
              begin
                writeln('            if O.%s_IsNull then', [Params.FieldName[j]]);
                writeln('            ByNameIsNull[''%s''] := true else', [Params.FieldName[j]]);
              end;
              case Q.Fields.FieldType[Q.Fields.GetFieldIndex(Params.FieldName[j])] of
                uftInteger          : writeln('            ByNameAsInteger[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftNumeric          : writeln('            ByNameAsDouble[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftChar             : writeln('            ByNameAsString[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftVarchar          : writeln('            ByNameAsString[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftSmallint         : writeln('            ByNameAsSmallint[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftFloat            : writeln('            ByNameAsSingle[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftDoublePrecision  : writeln('            ByNameAsDouble[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftTimestamp        : writeln('            ByNameAsDateTime[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftBlob, uftQuad    : writeln('            ParamsSetBlob(''%s'', O.%0:s);', [Params.FieldName[j]]);
                uftDate             : writeln('            ByNameAsDate[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftTime             : writeln('            ByNameAsTime[''%s''] := O.%0:s;', [Params.FieldName[j]]);
                uftInt64            : writeln('            ByNameAsInt64[''%s''] := O.%0:s;', [Params.FieldName[j]]);
              {$IFDEF IB7_UP}
                uftBoolean          : writeln('            ByNameAsBoolean[''%s''] := O.%0:s;', [Params.FieldName[j]]);
              {$ENDIF}
              end;
            end;
            writeln('            Execute;');
            writeln('            if RowsAffected > 0 then');
            writeln('              inc(done);');
          end;
          writeln('          end;');
          writeln('    end;');
          writeln('  end;');
          writeln('begin');
          writeln('  expected := 0;');
          writeln('  done := 0;');
          writeln('  if Transaction <> nil then');
          writeln('  begin');
          writeln('    QInsert := TUIBQuery.Create(nil);');
          writeln('    QUpdate := TUIBQuery.Create(nil);');
          writeln('    QDelete := TUIBQuery.Create(nil);');
          writeln('    try');
          writeln('      QInsert.Transaction := Transaction;');
          writeln('      QUpdate.Transaction := Transaction;');
          writeln('      QDelete.Transaction := Transaction;');
          writeln('      QInsert.SQL.Text := ''%s'';', [Q.QInsert]);
          writeln('      QUpdate.SQL.Text := ''%s'';', [Q.QUpdate]);
          writeln('      QDelete.SQL.Text := ''%s'';', [Q.QDelete]);
          writeln('      for i := 0 to FDeleted.Count - 1 do');
          writeln('        doObject(T%s(FDeleted[i]));', [Q.Name]);
          writeln('      for i := 0 to Count - 1 do');
          writeln('        if Items[i].FState <> 0 then');
          writeln('          doObject(Items[i]);');
          writeln('    finally');
          writeln('      QInsert.Free;');
          writeln('      QUpdate.Free;');
          writeln('      QDelete.Free;');
          writeln('    end;');
          writeln('  end;');
          writeln('  FDeleted.Clear;');
          writeln('  for i := 0 to Count - 1 do');
          writeln('    Items[i].FState := 0;');
          writeln('  Result := expected = done;');
          writeln('end;');

        end;
      end;
    finally
      QInsert.Free;
      QUpdate.Free;
      QDelete.Free;
    end;
  end;

begin
  meta := TMetaDataBase.Create(nil, -1);
  try
    meta.LoadFromDatabase(FTransaction);
    writeln('unit %s;', [FUnitName]);
    WriteInterface;
    WriteImplementation;
    writeln;
    writeln('end.');
  finally
    meta.Free;
  end;
end;

end.

