{******************************************}
{                                          }
{             FastReport v4.x              }
{          UIB enduser components          }
{                                          }
{         Copyright (c) 2005-2007          }
{            by Pierre Yager.              }
{                                          }
{******************************************}

unit frxUIBComponents;

interface

{$I frx.inc}

{$DEFINE UIB_IMAGES}

uses
  Windows, SysUtils, Graphics, Classes, 
  frxClass, frxrcUIB,
  DB, frxCustomDB,
  uib, uibLib, uibDataset
{$IFDEF Delphi6}
, Variants
{$ENDIF}
{$IFDEF QBUILDER}
, fqbClass
, uibMetadata
{$ENDIF};


type
  TfrxUIBComponents = class(TfrxDBComponents)
  private
    FDefaultDatabase: TUIBDatabase;
    FDefaultTransaction: TUIBTransaction;
    FOldComponents: TfrxUIBComponents;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDescription: String; override;
  published
    property DefaultDatabase: TUIBDatabase read FDefaultDatabase write FDefaultDatabase;
    property DefaultTransaction: TUIBTransaction read FDefaultTransaction write FDefaultTransaction;
  end;

  TfrxUIBDatabase = class(TfrxCustomDatabase)
  private
    FDatabase: TUIBDatabase;
    function GetSQLDialect: Integer;
    procedure SetSQLDialect(const Value: Integer);
  protected
    procedure SetConnected(Value: Boolean); override;
    procedure SetDatabaseName(const Value: String); override;
    procedure SetLoginPrompt(Value: Boolean); override;
    procedure SetParams(Value: TStrings); override;
    function GetConnected: Boolean; override;
    function GetDatabaseName: String; override;
    function GetLoginPrompt: Boolean; override;
    function GetParams: TStrings; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetDescription: String; override;
    procedure SetLogin(const Login, Password: String); override;
    property Database: TUIBDatabase read FDatabase;
  published
    property DatabaseName;
    property LoginPrompt;
    property Params;
    property SQLDialect: Integer read GetSQLDialect write SetSQLDialect;
    property Connected;
  end;

  TfrxUIBTransaction = class(TfrxDialogComponent)
  private
    FDatabase: TfrxUIBDatabase;
    FTransaction: TUIBTransaction;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDatabase(const Value: TfrxUIBDatabase);
    procedure SetReadOnly(const Value: Boolean);
    function GetReadOnly: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    constructor DesignCreate(AOwner: TComponent; Flags: Word); override;
    class function GetDescription: String; override;
    property Transaction: TUIBTransaction read FTransaction;
  published
    property Database: TfrxUIBDatabase read FDatabase write SetDatabase;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  end;

  TfrxUIBQuery = class(TfrxCustomQuery)
  private
    FDatabase: TfrxUIBDatabase;
    FTransaction: TfrxUIBTransaction;
    FQuery: TUIBDataset;
    procedure SetDatabase(const Value: TfrxUIBDatabase);
    procedure SetTransaction(const Value: TfrxUIBTransaction);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetSQL(Value: TStrings); override;
    function GetSQL: TStrings; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor DesignCreate(AOwner: TComponent; Flags: Word); override;
    class function GetDescription: String; override;
    procedure BeforeStartReport; override;
    procedure UpdateParams; override;
{$IFDEF QBUILDER}
    function QBEngine: TfqbEngine; override;
{$ENDIF}
    property Query: TUIBDataset read FQuery;
  published
    property Database: TfrxUIBDatabase read FDatabase write SetDatabase;
    property Transaction: TfrxUIBTransaction read FTransaction write SetTransaction;
  end;

{$IFDEF QBUILDER}
  TfrxEngineUIB = class(TfqbEngine)
  private
    FDatabase: TUIBDatabase;
    FTransaction: TUIBTransaction;
    FDataset: TUIBDataSet;
    FOldSystemObjects: Boolean;
    procedure SetDatabase(const Value: TUIBDatabase);
    function ReadMetadatas: TMetaDataBase;
  protected
    property Database: TUIBDatabase read FDatabase write SetDatabase;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadTableList(ATableList: TStrings); override;
    procedure ReadFieldList(const ATableName: string; var AFieldList: TfqbFieldList); override;
    function ResultDataSet: TDataSet; override;
    procedure SetSQL(const Value: string); override;
  end;
{$ENDIF}

var
  UIBComponents: TfrxUIBComponents;

implementation

uses
  frxUIBRTTI,
{$IFNDEF NO_EDITORS}
  frxUIBEditor,
  uibConst,
{$ENDIF}
  frxDsgnIntf, frxRes;

{$R frxUIB_FR4.res}

{ frxParamsToUIBParams }

procedure frxParamsToUIBParams(Query: TfrxCustomQuery; aParams: TSQLParams);
var
  i: Integer;
  Item: TfrxParamItem;
begin
  for i := 0 to aParams.ParamCount - 1 do
  begin
    if Query.Params.IndexOf(aParams.FieldName[i]) <> -1 then
    begin
      Item := Query.Params.Find(aParams.FieldName[i]);
      if Item <> nil  then
      begin
        if Trim(Item.Expression) <> '' then
          if not (Query.IsLoading or Query.IsDesigning) then
          begin
            Query.Report.CurObject := Query.Name;
            Item.Value := Query.Report.Calc(Item.Expression);
          end;
        if not VarIsEmpty(Item.Value) then
        try
          if Item.DataType in [ftDate, ftTime, ftDateTime]	then
            aParams.AsDateTime[i] := VarToDateTime(Item.Value)
          else
            aParams.AsString[i] := VarToStr(Item.Value);
        except
          aParams.AsString[i] := Item.Value;
        end;
      end;
    end;
  end;
end;

{ TfrxUIBomponents }

constructor TfrxUIBComponents.Create(AOwner: TComponent);
begin
  inherited;
  FOldComponents := UIBComponents;
  UIBComponents := Self;
end;

destructor TfrxUIBComponents.Destroy;
begin
  if UIBComponents = Self then
    UIBComponents := FOldComponents;
  inherited;
end;

function TfrxUIBComponents.GetDescription: String;
begin
  Result := 'UIB';
end;

{ TfrxUIBDatabase }

constructor TfrxUIBDatabase.Create(AOwner: TComponent);
begin
  inherited;
  FDatabase := TUIBDatabase.Create(nil);
  Component := FDatabase;
end;

class function TfrxUIBDatabase.GetDescription: String;
begin
  Result := frxResources.Get('obUIBDB');
end;

function TfrxUIBDatabase.GetConnected: Boolean;
begin
  Result := FDatabase.Connected;
end;

function TfrxUIBDatabase.GetDatabaseName: String;
begin
  Result := FDatabase.DatabaseName;
end;

function TfrxUIBDatabase.GetLoginPrompt: Boolean;
begin
  Result := false;
end;

function TfrxUIBDatabase.GetParams: TStrings;
begin
  Result := FDatabase.Params;
end;

function TfrxUIBDatabase.GetSQLDialect: Integer;
begin
  Result := FDatabase.SQLDialect;
end;

procedure TfrxUIBDatabase.SetConnected(Value: Boolean);
begin
  FDatabase.Connected := Value;
end;

procedure TfrxUIBDatabase.SetDatabaseName(const Value: String);
begin
  FDatabase.DatabaseName := Value;
end;

procedure TfrxUIBDatabase.SetLoginPrompt(Value: Boolean);
begin
  // not supported...
end;

procedure TfrxUIBDatabase.SetParams(Value: TStrings);
begin
  FDatabase.Params := Value;
end;

procedure TfrxUIBDatabase.SetSQLDialect(const Value: Integer);
begin
  FDatabase.SQLDialect := Value;
end;

procedure TfrxUIBDatabase.SetLogin(const Login, Password: String);
begin
  FDatabase.UserName := Login;
  FDatabase.PassWord := Password;
end;

{ TfrxUIBTransaction }

constructor TfrxUIBTransaction.Create(AOwner: TComponent);
begin
  inherited;
  FTransaction := TUIBTransaction.Create(nil);
  Component := FTransaction;
  SetDatabase(nil);
end;

constructor TfrxUIBTransaction.DesignCreate(AOwner: TComponent; Flags: Word);
var
  i: Integer;
  l: TList;
begin
  inherited;
  l := Report.AllObjects;
  for i := 0 to l.Count - 1 do
    if TObject(l[i]) is TfrxUIBDatabase then
    begin
      SetDatabase(TfrxUIBDatabase(l[i]));
      Break;
    end;
end;

class function TfrxUIBTransaction.GetDescription: String;
begin
  Result := frxResources.Get('obUIBT');
end;

function TfrxUIBTransaction.GetReadOnly: Boolean;
begin
  Result := (tpRead in FTransaction.Options) or (not (tpWrite in FTransaction.Options));
end;

procedure TfrxUIBTransaction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = FDatabase) then
      SetDatabase(nil);
  end;
end;

procedure TfrxUIBTransaction.SetDatabase(const Value: TfrxUIBDatabase);
begin
  FDatabase := Value;
  if Value <> nil then
    FTransaction.DataBase := Value.Database
  else if UIBComponents <> nil then
    FTransaction.DataBase := UIBComponents.DefaultDatabase
  else
    FTransaction.Database := nil;
end;

procedure TfrxUIBTransaction.SetReadOnly(const Value: Boolean);
begin
  if Value then
  begin
    FTransaction.Options := FTransaction.Options - [tpWrite];
    FTransaction.Options := FTransaction.Options + [tpRead];
  end
  else
  begin
    FTransaction.Options := FTransaction.Options - [tpRead];
    FTransaction.Options := FTransaction.Options + [tpWrite];
  end;
end;

{ TfrxUIBQuery }

constructor TfrxUIBQuery.Create(AOwner: TComponent);
begin
  FQuery := TUIBDataset.Create(nil);
  Dataset := FQuery;
  SetTransaction(nil);
//  SetDatabase(nil);
  inherited;
end;

constructor TfrxUIBQuery.DesignCreate(AOwner: TComponent; Flags: Word);
var
  i: Integer;
  l: TList;
begin
  inherited;
  l := Report.AllObjects;

  { Search for the first available transaction }
  for i := 0 to l.Count - 1 do
    if TObject(l[i]) is TfrxUIBTransaction then
    begin
      SetTransaction(TfrxUIBTransaction(l[i]));
      Break;
    end;

  { If no FDatabase has been set by SetTransaction choose one from available
    database connection }
  if (FDatabase = nil) then
    for i := 0 to l.Count - 1 do
      if TObject(l[i]) is TfrxUIBDatabase then
      begin
        SetDatabase(TfrxUIBDatabase(l[i]));
        Break;
      end;
end;

class function TfrxUIBQuery.GetDescription: String;
begin
  Result := frxResources.Get('obUIBQ');
end;

procedure TfrxUIBQuery.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = FDatabase) then
      SetDatabase(nil)
    else if (AComponent = FTransaction) then
      SetTransaction(nil);
  end;
end;

procedure TfrxUIBQuery.SetDatabase(const Value: TfrxUIBDatabase);
begin
  FDatabase := Value;

  if Assigned(FQuery.Transaction) then
  begin
    if Value <> nil then
      FQuery.Transaction.DataBase := Value.Database
    else if UIBComponents <> nil then
      FQuery.Transaction.DataBase := UIBComponents.DefaultDatabase
    else
      FQuery.Transaction.Database := nil;

    DBConnected := FQuery.Transaction.Database <> nil;
  end
  else
    DBConnected := false;
end;

procedure TfrxUIBQuery.SetTransaction(const Value: TfrxUIBTransaction);
begin
  FTransaction := Value;
  if Value <> nil then
    FQuery.Transaction := Value.Transaction
  else if UIBComponents <> nil then
    FQuery.Transaction := UIBComponents.DefaultTransaction
  else
    FQuery.Transaction := nil;

  if (FTransaction <> nil) then
  begin
    if FDatabase <> nil then
      SetDatabase(FDatabase)
    else
      SetDatabase(FTransaction.Database);
  end;

  DBConnected := (FQuery.Transaction <> nil) and (FQuery.Transaction.Database <> nil);
end;

procedure TfrxUIBQuery.SetSQL(Value: TStrings);
begin
  FQuery.SQL := Value;
end;

function TfrxUIBQuery.GetSQL: TStrings;
begin
  Result := FQuery.SQL;
end;

procedure TfrxUIBQuery.UpdateParams;
begin
  frxParamsToUIBParams(Self, FQuery.Params);
end;

procedure TfrxUIBQuery.BeforeStartReport;
begin
  SetTransaction(FTransaction);
end;

{$IFDEF QBUILDER}
function TfrxUIBQuery.QBEngine: TfqbEngine;
begin
  Result := TfrxEngineUIB.Create(nil);
  TfrxEngineUIB(Result).Database := Self.FTransaction.Database.Database;
end;
{$ENDIF}

{$IFDEF QBUILDER}
constructor TfrxEngineUIB.Create(AOwner: TComponent);
begin
  inherited;
  FTransaction := TUIBTransaction.Create(nil);
  FTransaction.Options := [tpConcurrency, tpNoWait, tpRead];

  FDataset := TUIBDataset.Create(nil);
  FDataset.Transaction := FTransaction;
end;

destructor TfrxEngineUIB.Destroy;
begin
  FTransaction.Free;
  FDataset.Free;
  inherited;
end;

procedure TfrxEngineUIB.SetDatabase(const Value: TUIBDatabase);
begin
  FOldSystemObjects := ShowSystemTables;
  FDatabase := Value;

  if Assigned(FDatabase) then
  begin
    { Set MetaDataOptions to retrieve only tables/view with their fields }
    with FDatabase.MetaDataOptions do
    begin
      Objects := [OIDTable, OIDView];
      Tables := [OIDTableField];
      Views := [OIDViewField];
      SysInfos := ShowSystemTables;
    end;
  end;

  FTransaction.Database := FDatabase;
end;

function TfrxEngineUIB.ReadMetadatas: TMetaDataBase;
var
  ForceRefresh: Boolean;
begin
  if Assigned(FDatabase) then
  begin
    if ShowSystemTables <> FOldSystemObjects then
    begin
      FDatabase.MetaDataOptions.SysInfos := ShowSystemTables;
      FOldSystemObjects := ShowSystemTables;
      ForceRefresh := true;
    end
    else
      ForceRefresh := false;

    Result := TMetaDataBase(FDatabase.GetMetadata(ForceRefresh));
  end
  else
    Result := nil;
end;

procedure TfrxEngineUIB.ReadFieldList(const ATableName: string;
  var AFieldList: TfqbFieldList);
var
  MetaDatas: TMetaDataBase;
  MTable: TMetaTable;
  i: Integer;
  tmpField: TfqbField;
begin
  AFieldList.Clear;

  { Add a special "all" fields }
  tmpField:= TfqbField(AFieldList.Add);
  tmpField.FieldName := '*';

  { Read the metadata }
  MetaDatas := ReadMetadatas;
  MTable := MetaDatas.FindTableName(ATableName);
  for i := 0 to MTable.FieldsCount - 1 do
  begin
    tmpField := TfqbField(AFieldList.Add);
    tmpField.FieldName := MTable.Fields[i].Name;

    case MTable.Fields[i].FieldType of
      uftUnKnown  : tmpField.FieldType := Ord(ftUnknown);
      uftNumeric  : tmpField.FieldType := Ord(ftCurrency);
      uftChar,
      uftVarchar,
      uftCstring  : tmpField.FieldType := Ord(ftString);
      uftSmallint : tmpField.FieldType := Ord(ftSmallint);
      uftInteger  : tmpField.FieldType := Ord(ftInteger);
      uftQuad     : tmpField.FieldType := Ord(ftUnknown); // ?
      uftFloat,
      uftDoublePrecision : tmpField.FieldType := Ord(ftFloat);
      uftTimestamp: tmpField.FieldType := Ord(ftDateTime);
      uftBlob     :
        begin
          if MTable.Fields[i].SubType = 1 then
            tmpField.FieldType := Ord(ftMemo)
          else
            tmpField.FieldType := Ord(ftBlob);
        end;
      uftBlobId : tmpField.FieldType := Ord(ftUnknown); // ?
      uftDate : tmpField.FieldType := Ord(ftDate);
      uftTime : tmpField.FieldType := Ord(ftTime);
      uftInt64 : tmpField.FieldType := Ord(ftInteger); // ?
      uftArray : tmpField.FieldType := Ord(ftUnknown); // ?
    end;
  end;
end;

procedure TfrxEngineUIB.ReadTableList(ATableList: TStrings);
var
  MetaDatas: TMetaDataBase;
  i: Integer;
begin
  ATableList.Clear;

  MetaDatas := ReadMetadatas;
  for i := 0 to MetaDatas.TablesCount - 1 do
    ATableList.Add(MetaDatas.Tables[i].Name);
end;

function TfrxEngineUIB.ResultDataSet: TDataSet;
begin
  Result := FDataset;
end;

procedure TfrxEngineUIB.SetSQL(const Value: string);
begin
  FDataset.SQL.Text := Value;
end;
{$ENDIF}

var
  UIBComponentsBmp
{$IFDEF UIB_IMAGES}
, UIBDatabaseBmp, UIBTransactionBmp, UIBQueryBmp
{$ENDIF}: TBitmap;

initialization
  { Load UIB Components Images from resources }
  UIBComponentsBmp := TBitmap.Create;
  UIBComponentsBmp.LoadFromResourceName(HInstance,'frxUIBMenu');

{$IFDEF UIB_IMAGES}
  UIBDatabaseBmp := TBitmap.Create;
  UIBDatabaseBmp.LoadFromResourceName(HInstance,'TfrxUIBDataBase');

  UIBTransactionBmp := TBitmap.Create;
  UIBTransactionBmp.LoadFromResourceName(HInstance,'TfrxUIBTransaction');

  UIBQueryBmp := TBitmap.Create;
  UIBQueryBmp.LoadFromResourceName(HInstance,'TfrxUIBQuery');
{$ENDIF}

  { Register UIB Components category button }
  frxObjects.RegisterCategory('UIB', UIBComponentsBmp, '');

  { Register UIB Components into UIB Category }
{$IFDEF UIB_IMAGES}
  frxObjects.RegisterObject(TfrxUIBDatabase, UIBDatabaseBmp, 'UIB');
  frxObjects.RegisterObject(TfrxUIBTransaction, UIBTransactionBmp, 'UIB');
  frxObjects.RegisterObject(TfrxUIBQuery, UIBQueryBmp, 'UIB');
{$ELSE}
  frxObjects.RegisterObject1(TfrxUIBDatabase, nil, '', 'UIB', 0, 37);
  frxObjects.RegisterObject1(TfrxUIBTransaction, nil, '', 'UIB', 0, 37);
  frxObjects.RegisterObject1(TfrxUIBQuery, nil, '', 'UIB', 0, 39);
{$ENDIF}

finalization
{$IFDEF UIB_IMAGES}
  UIBQueryBmp.Free;
  UIBTransactionBmp.Free;
  UIBDatabaseBmp.Free;
{$ENDIF}

  UIBComponentsBmp.Free;

end.
