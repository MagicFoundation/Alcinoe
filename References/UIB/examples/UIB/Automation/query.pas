unit query;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, UIB_TLB, StdVcl, uib, InternalItf;

type

  TOleQuery = class(TUIBQuery)
  private
    FTransaction: ITransaction;
  protected
    procedure SetTransaction(const Transaction: TUIBTransaction); override;
  end;

  TQuery = class(TAutoObject, IQuery)
  private
    FQuery: TOleQuery;
  protected
    function Get_SQL: WideString; safecall;
    procedure Set_SQL(const Value: WideString); safecall;
    function Get_FetchBlobs: WordBool; safecall;
    procedure Set_FetchBlobs(Value: WordBool); safecall;
    function Get_CachedFetch: WordBool; safecall;
    procedure Set_CachedFetch(Value: WordBool); safecall;
    function Get_QuickScript: WordBool; safecall;
    procedure Set_QuickScript(Value: WordBool); safecall;
    function Get_EOF: WordBool; safecall;
    procedure Open; safecall;
    procedure ExecSQL; safecall;
    procedure Close(Mode: Integer); safecall;
    procedure Next; safecall;
    procedure FetchAll; safecall;
    procedure SQLAddLine(const Str: WideString); safecall;
    function Get_Transaction: ITransaction; safecall;
    procedure Set_Transaction(const Value: ITransaction); safecall;
    function Get_AsVariant(Index: Word): OleVariant; safecall;
    function Get_AsInteger(Index: Word): Integer; safecall;
    function Get_AsSmallint(Index: Word): Smallint; safecall;
    function Get_AsSingle(Index: Word): Single; safecall;
    function Get_AsDouble(Index: Word): Double; safecall;
    function Get_AsInt64(Index: Word): Int64; safecall;
    function Get_AsString(Index: Word): WideString; safecall;
    function Get_ByNameAsSmallint(const Name: WideString): Smallint; safecall;
    function Get_ByNameAsInteger(const Name: WideString): Integer; safecall;
    function Get_ByNameAsSingle(const Name: WideString): Single; safecall;
    function Get_ByNameAsDouble(const Name: WideString): Double; safecall;
    function Get_ByNameAsInt64(const Name: WideString): Int64; safecall;
    function Get_ByNameAsString(const Name: WideString): WideString; safecall;
    function Get_ByNameAsVariant(const Name: WideString): OleVariant; safecall;
    function Get_FieldCount: Integer; safecall;
    function Get_AliasName(Index: Word): WideString; safecall;
    function Get_AsBoolean(Index: Word): WordBool; safecall;
    function Get_ByNameAsBoolean(const Name: WideString): WordBool; safecall;
    function Get_AsDateTime(Index: Word): TDateTime; safecall;
    function Get_ByNameAsDateTime(const Name: WideString): TDateTime; safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses ComServ, uibLib;

{ TOleQuery }

procedure TOleQuery.SetTransaction(const Transaction: TUIBTransaction);
begin
  inherited;
  if (Transaction = nil) then FTransaction := nil; // if database disconnect transaction
end;

{ TQuery }

function TQuery.Get_SQL: WideString;
begin
  Result := FQuery.SQL.Text;
end;

procedure TQuery.Set_SQL(const Value: WideString);
begin
  FQuery.SQL.Text := Value;
end;

function TQuery.Get_FetchBlobs: WordBool;
begin
  Result := FQuery.FetchBlobs;
end;

procedure TQuery.Set_FetchBlobs(Value: WordBool);
begin
  FQuery.FetchBlobs := Value;
end;

function TQuery.Get_CachedFetch: WordBool;
begin
  Result := FQuery.CachedFetch;
end;

procedure TQuery.Set_CachedFetch(Value: WordBool);
begin
  FQuery.CachedFetch := Value;
end;

function TQuery.Get_QuickScript: WordBool;
begin
  Result := FQuery.QuickScript;
end;

procedure TQuery.Set_QuickScript(Value: WordBool);
begin
  FQuery.QuickScript := Value;
end;

function TQuery.Get_EOF: WordBool;
begin
  Result := FQuery.EOF;
end;

procedure TQuery.Open;
begin
  FQuery.Open;
end;

procedure TQuery.ExecSQL;
begin
  FQuery.ExecSQL;
end;

procedure TQuery.Close(Mode: Integer);
begin
  FQuery.Close(TEndTransMode(Mode));
end;

procedure TQuery.Next;
begin
  FQuery.Next;
end;

procedure TQuery.FetchAll;
begin
  FQuery.FetchAll;
end;

procedure TQuery.SQLAddLine(const Str: WideString);
begin
  FQuery.SQL.Add(Str);
end;

destructor TQuery.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TQuery.Initialize;
begin
  inherited;
  FQuery := TOleQuery.Create(nil);
end;

function TQuery.Get_Transaction: ITransaction;
begin
  Result := FQuery.FTransaction;
end;

procedure TQuery.Set_Transaction(const Value: ITransaction);
begin
  if (Value = nil) then
    FQuery.SetTransaction(nil) else
    FQuery.SetTransaction((value as IDataPointer).Data);
end;

function TQuery.Get_AsVariant(Index: Word): OleVariant;
begin
  Result := FQuery.Fields.AsVariant[Index];
end;

function TQuery.Get_AsInteger(Index: Word): Integer;
begin
  Result := FQuery.Fields.AsInteger[Index];
end;

function TQuery.Get_AsSmallint(Index: Word): Smallint;
begin
  Result := FQuery.Fields.AsSmallint[Index];
end;

function TQuery.Get_AsSingle(Index: Word): Single;
begin
  Result := FQuery.Fields.AsSingle[Index];
end;

function TQuery.Get_AsDouble(Index: Word): Double;
begin
  Result := FQuery.Fields.AsDouble[Index];
end;

function TQuery.Get_AsInt64(Index: Word): Int64;
begin
  Result := FQuery.Fields.AsInt64[Index];
end;

function TQuery.Get_AsString(Index: Word): WideString;
begin
  Result := FQuery.Fields.AsString[Index];
end;

function TQuery.Get_ByNameAsSmallint(const Name: WideString): Smallint;
begin
  Result := FQuery.Fields.ByNameAsSmallint[Name];
end;

function TQuery.Get_ByNameAsInteger(const Name: WideString): Integer;
begin
  Result := FQuery.Fields.ByNameAsInteger[Name];
end;

function TQuery.Get_ByNameAsSingle(const Name: WideString): Single;
begin
  Result := FQuery.Fields.ByNameAsSingle[Name];
end;

function TQuery.Get_ByNameAsDouble(const Name: WideString): Double;
begin
  Result := FQuery.Fields.ByNameAsDouble[Name];
end;

function TQuery.Get_ByNameAsInt64(const Name: WideString): Int64;
begin
  Result := FQuery.Fields.ByNameAsInt64[Name];
end;

function TQuery.Get_ByNameAsString(const Name: WideString): WideString;
begin
  Result := FQuery.Fields.ByNameAsString[Name];
end;

function TQuery.Get_ByNameAsVariant(const Name: WideString): OleVariant;
begin
  Result := FQuery.Fields.ByNameAsVariant[Name];
end;

function TQuery.Get_FieldCount: Integer;
begin
  Result := FQuery.Fields.FieldCount;
end;

function TQuery.Get_AliasName(Index: Word): WideString;
begin
  Result := FQuery.Fields.AliasName[Index];
end;

function TQuery.Get_AsBoolean(Index: Word): WordBool;
begin
  Result := FQuery.Fields.AsBoolean[Index];
end;

function TQuery.Get_ByNameAsBoolean(const Name: WideString): WordBool;
begin
  Result := FQuery.Fields.ByNameAsBoolean[Name];
end;

function TQuery.Get_AsDateTime(Index: Word): TDateTime;
begin
  Result := FQuery.Fields.AsDateTime[Index];
end;

function TQuery.Get_ByNameAsDateTime(const Name: WideString): TDateTime;
begin
  Result := FQuery.Fields.ByNameAsDateTime[Name];
end;

initialization
  TAutoObjectFactory.Create(ComServer, TQuery, Class_Query,
    ciMultiInstance, tmApartment);
end.
