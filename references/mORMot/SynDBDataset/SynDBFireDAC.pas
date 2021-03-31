/// FireDAC/AnyDAC-based classes for SynDB units
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynDBFireDAC;

{
  This file is part of Synopse framework.

  Synopse framework. Copyright (C) 2021 Arnaud Bouchez
  Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - delphinium (louisyeow)
  - Oleg Tretyakov

  
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows, SysUtils,
  {$IFNDEF DELPHI5OROLDER}
  Variants,
  {$ENDIF}
  Classes, Contnrs,
  SynCommons,
  SynTable,
  SynLog,
  SynDB,
  SynDBDataset,
  {$ifdef ISDELPHIXE5}
  FireDAC.Comp.Client, FireDAC.Stan.Param;
  {$else}
  uADCompClient, uADStanParam;
  {$endif}



{ -------------- FireDAC/AnyDAC database access }

type
  /// Exception type associated to FireDAC/AnyDAC database access
  ESQLDBFireDAC = class(ESQLDBDataset);


  ///	connection properties definition using FireDAC/AnyDAC database access
  TSQLDBFireDACConnectionProperties = class(TSQLDBDatasetConnectionProperties)
  protected
    fFireDACOptions: TStringList;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - do nothing by now (FireDAC metadata may be used in the future)
    procedure GetForeignKeys; override;
  public
    /// initialize the properties to connect via FireDAC/AnyDAC database access
    // - aServerName shall contain the FireDAC provider DriverID, e.g. 'Ora', and
    // some optional parameters (e.g. remote server name if needed), after a '?'
    // and separated by ';' - for instance:
    // ! Create('Ora','TNSNAME','User','Password');
    // ! Create('Ora?CharacterSet=cl8mswin1251','TNSNAME','User','Password');
    // ! Create('MSSQL?Server=127.0.0.1\SQLEXPRESS','Northwind','User','Password');
    // ! Create('MSSQL?Server=.\SQLEXPRESS;OSAuthent=Yes','','','');
    // ! Create('MSAcc','c:\data\access.mdb','','');
    // ! Create('MySQL?Server=127.0.0.1;Port=3306','MyDB','User','Password');
    // ! Create('SQLite','c:\data\myapp.db3','','');
    // ! Create('SQLite',SQLITE_MEMORY_DATABASE_NAME,'','');
    // ! Create('IB','127.0.0.1:C:\ib\ADDEMO_IB2007.IB','User','Password');
    // ! Create('IB?Server=my_host/3055','C:\ib\ADDEMO_IB2007.IB','User','Password');
    // ! Create('IB?CreateDatabase=Yes','127.0.0.1:C:\ib\ADDEMO_IB2007.IB','User','Password');
    // ! Create('DB2?Server=localhost;Port=50000','SAMPLE','db2admin','db2Password');
    // ! Create('PG?Server=localhost;Port=5432','postgres','postgres','postgresPassword');
    // ! Create('MySQL?Server=localhost;Port=3306','test','root','');
    // - aDatabaseName shall contain the database server name
    // - note that you need to link the FireDAC driver by including the
    // expected uADPhys*.pas / FireDAC.Phy.*.pas units into a uses clause
    // of your application, e.g. uADPhysOracle, uADPhysMSSQL, uADPhysMSAcc,
    // uADPhysMySQL, uADPhysSQLite, uADPhysIB or uADPhysDB2 (depending on the
    // expected provider) - or FireDAC.Phys.Oracle, FireDAC.Phys.MSAcc,
    // FireDAC.Phys.MSSQL, FireDAC.Phys.SQLite, FireDAC.Phys.IB, FireDAC.Phys.PG
    // or FireDAC.Phys.DB2 since Delphi XE5 namespace modifications
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    /// release internal structures
    destructor Destroy; override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSQLDBFireDACConnection instance
    function NewConnection: TSQLDBConnection; override;

    /// retrieve the column/field layout of a specified table
    // - this overridden method will use FireDAC metadata to retrieve the information
    procedure GetFields(const aTableName: RawUTF8; out Fields: TSQLDBColumnDefineDynArray); override;
    /// get all table names
    // - this overridden method will use FireDAC metadata to retrieve the information
    procedure GetTableNames(out Tables: TRawUTF8DynArray); override;
    /// retrieve the advanced indexed information of a specified Table
    // - this overridden method will use FireDAC metadata to retrieve the information
    procedure GetIndexes(const aTableName: RawUTF8; out Indexes: TSQLDBIndexDefineDynArray); override;

    /// allow to set the options specific to a FireDAC driver
    // - by default, ServerName, DatabaseName, UserID and Password are set by
    // the Create() constructor according to the underlying FireDAC driver
    // - you can add some additional options here
    property Parameters: TStringList read fFireDACOptions;
  end;


  ///	implements a direct connection via FireDAC/AnyDAC database access
  TSQLDBFireDACConnection = class(TSQLDBConnectionThreadSafe)
  protected
    fDatabase: {$ifdef ISDELPHIXE5}TFDConnection{$else}TADConnection{$endif};
  public
    /// prepare a connection for a specified FireDAC/AnyDAC database access
    constructor Create(aProperties: TSQLDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the specified database server using FireDAC
    // - should raise an ESQLDBFireDAC on error
    procedure Connect; override;
    /// stop connection to the specified database server using FireDAC
    // - should raise an ESQLDBFireDAC on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// create a new statement instance
    function NewStatement: TSQLDBStatement; override;
    /// begin a Transaction for this connection
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// access to the associated FireDAC connection instance
    property Database: {$ifdef ISDELPHIXE5}TFDConnection{$else}TADConnection{$endif} read fDatabase;
  end;

  ///	implements a statement via a FireDAC connection
  // - this specific version will handle the FireDAC specific parameter classes
  // - it will also handle Array DML commands, if possible
  TSQLDBFireDACStatement = class(TSQLDBDatasetStatementAbstract)
  protected
    fQueryParams: {$ifdef ISDELPHIXE5}TFDParams{$else}TADParams{$endif};
    fPreparedUseArrayDML: boolean;
    /// initialize and set fQuery: TUniQuery internal field as expected
    procedure DatasetCreate; override;
    /// set fQueryParams internal field as expected
    function DatasetPrepare(const aSQL: string): boolean; override;
    /// execute underlying TUniQuery.ExecSQL
    procedure DatasetExecSQL; override;
    /// bind SQLDBParam to TQuery-like param using fQueryParams: DB.TParams
    procedure DataSetBindSQLParam(const aArrayIndex, aParamIndex: integer;
      const aParam: TSQLDBParam); override;
    /// set the returned parameter after a stored proc execution
    procedure DataSetOutSQLParam(const aParamIndex: integer;
      var aParam: TSQLDBParam); override;
  public
    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESQLDBFireDAC on any error
    procedure Prepare(const aSQL: RawUTF8; ExpectResults: boolean = false); overload; override;
  end;


const
  /// FireDAC DriverID values corresponding to SynDB recognized SQL engines
  {$ifdef ISDELPHIXE5}
  FIREDAC_PROVIDER: array[dOracle..high(TSQLDBDefinition)] of RawUTF8 = (
    'Ora','MSSQL','MSAcc','MySQL','SQLite','FB','','PG','DB2','Infx');

  {$else}
  FIREDAC_PROVIDER: array[dOracle..high(TSQLDBDefinition)] of RawUTF8 = (
    'Ora','MSSQL','MSAcc','MySQL','SQLite','IB','','PG','DB2','Infx');
  {$endif}

  
implementation

uses
  {$ifdef ISDELPHIXE5}
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.DApt, FireDAC.Stan.Async;

type
  TADConnection = TFDConnection;
  TADQuery = TFDQuery;
  TADMetaInfoQuery = TFDMetaInfoQuery;
  TADParam = TFDParam;
  TADParams = TFDParams;
  TADPhysMetaInfoKind = TFDPhysMetaInfoKind;

  {$else}
  uADPhysIntf, uADStanDef, uADDAptManager, uADStanAsync;
  {$endif}


{ TSQLDBFireDACConnectionProperties }

constructor TSQLDBFireDACConnectionProperties.Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8);
var p: TSQLDBDefinition;
    server,options,namevalue: RawUTF8;
    opt: PUTF8Char;
begin
  Split(aServerName,'?',server,options);
  if server<>'' then
    for p := Low(FIREDAC_PROVIDER) to high(FIREDAC_PROVIDER) do
      if SameTextU(FIREDAC_PROVIDER[p],server) then begin
        fDBMS := p;
        break;
      end;
  inherited Create(server,aDatabaseName,aUserID,aPassWord);
  fOnBatchInsert := nil; // MultipleValuesInsert is slower than FireDAC ArrayDML 
  fFireDACOptions := TStringList.Create;
  if ((fDBMS<low(FIREDAC_PROVIDER)) or (fDBMS>high(FIREDAC_PROVIDER))) and
     (fDBMS<>dNexusDB) then
    if SameTextU(server,'ASA') then
      fDBMS := dMSSQL else begin
      for p := Low(FIREDAC_PROVIDER) to high(FIREDAC_PROVIDER) do
        namevalue := ' '+namevalue+FIREDAC_PROVIDER[p];
      raise ESQLDBFireDAC.CreateUTF8('%.Create: unknown provider - available:%',
        [self,namevalue]);
    end;
  if server='' then
    server := FIREDAC_PROVIDER[fDBMS];
  fFireDACOptions.Text := UTF8ToString(FormatUTF8(
    'DriverID=%'#13#10'User_Name=%'#13#10'Password=%'#13#10'Database=%',
    [server,fUserId,fPassWord,fDatabaseName]));
  opt := pointer(options);
  while opt<>nil do begin
    GetNextItem(opt,';',namevalue);
    if namevalue<>'' then
      fFireDACOptions.Add(UTF8ToString(namevalue));
  end;
  case fDBMS of
  dSQLite: begin
    if fFireDACOptions.Values['CharacterSet']='' then // force UTF-8 for SynDB
      fFireDACOptions.Values['CharacterSet'] := 'UTF8';
    {$ifdef UNICODE} // CreateUTF16 is the default value for Delphi 2009+
    if fFireDACOptions.Values['OpenMode']='' then // force UTF-8 for SynDB
      fFireDACOptions.Values['OpenMode'] := 'CreateUTF8';
    {$else}
    ForceUseWideString := true; // as expected by FireDAC when UTF-8 is enabled
    {$endif}
    fSQLCreateField[ftInt64] := ' BIGINT'; // SQLite3 INTEGER = 32bit for FireDAC
  end;
  dFirebird, dMySQL, dPostgreSQL, dDB2: begin
    if fFireDACOptions.Values['CharacterSet']='' then // force UTF-8 for SynDB
      fFireDACOptions.Values['CharacterSet'] := 'UTF8';
    {$ifndef UNICODE}
    ForceUseWideString := true; // as expected by FireDAC when UTF-8 is enabled
    {$endif}
  end;
  end;
end;

destructor TSQLDBFireDACConnectionProperties.Destroy;
begin
  fFireDACOptions.Free;
  inherited;
end;

procedure TSQLDBFireDACConnectionProperties.GetTableNames(
  out Tables: TRawUTF8DynArray);
var List: TStringList;
begin
  List := TStringList.Create;
  try
    (MainConnection as TSQLDBFireDACConnection).fDatabase.GetTableNames(
      '','','',List,[osMy],[tkTable]);
    StringListToRawUTF8DynArray(List,Tables);
    exit;
  finally
    List.Free;
  end;
  inherited;
end;

procedure TSQLDBFireDACConnectionProperties.GetFields(
  const aTableName: RawUTF8; out Fields: TSQLDBColumnDefineDynArray);
var meta: TADMetaInfoQuery;
    n: integer;
    F: TSQLDBColumnDefine;
    FA: TDynArray;
begin
  meta := TADMetaInfoQuery.Create(nil);
  try
    meta.Connection := (MainConnection as TSQLDBFireDACConnection).fDatabase;
    FA.Init(TypeInfo(TSQLDBColumnDefineDynArray),Fields,@n);
    FA.Compare := SortDynArrayAnsiStringI; // FA.Find() case insensitive
    FillChar(F,sizeof(F),0);
    meta.MetaInfoKind := mkTableFields;
    meta.ObjectName := UTF8ToString(UpperCase(aTableName));
    meta.Open;
    while not meta.Eof do begin
      F.ColumnName := StringToUTF8(meta.FieldByName('COLUMN_NAME').AsString);
      F.ColumnTypeNative := StringToUTF8(meta.FieldByName('COLUMN_TYPENAME').AsString);
      F.ColumnLength := meta.FieldByName('COLUMN_LENGTH').AsInteger;
      F.ColumnScale := meta.FieldByName('COLUMN_SCALE').AsInteger;
      F.ColumnPrecision := meta.FieldByName('COLUMN_PRECISION').AsInteger;
      { TODO : retrieve ColumnType from high-level FireDAC type information }
      F.ColumnType := ColumnTypeNativeToDB(F.ColumnTypeNative,F.ColumnScale);
      FA.Add(F);
      meta.Next;
    end;
    Setlength(Fields,n);
    GetIndexesAndSetFieldsColumnIndexed(aTableName,Fields);
  finally
    meta.Free;
  end;
end;

procedure TSQLDBFireDACConnectionProperties.GetIndexes(
  const aTableName: RawUTF8; out Indexes: TSQLDBIndexDefineDynArray);
var kind: boolean;
    meta, indexs: TADMetaInfoQuery;
    TableName: string;
    ColName: RawUTF8;
    F: TSQLDBIndexDefine;
    FA: TDynArray;
    n: integer;
const
  MASTER: array[boolean] of TADPhysMetaInfoKind = (mkPrimaryKey,mkIndexes);
  CHILD:  array[boolean] of TADPhysMetaInfoKind = (mkPrimaryKeyFields,mkIndexFields);
begin
  TableName := UTF8ToString(UpperCase(aTableName));
  FA.Init(TypeInfo(TSQLDBIndexDefineDynArray),Indexes,@n);
  fillchar(F,sizeof(F),0);
  meta := TADMetaInfoQuery.Create(nil);
  indexs := TADMetaInfoQuery.Create(nil);
  try
    meta.Connection := (MainConnection as TSQLDBFireDACConnection).fDatabase;
    indexs.Connection := meta.Connection;
    for kind := true to true do begin // primary keys may not be indexed
      meta.MetaInfoKind := MASTER[kind];
      meta.ObjectName := TableName;
      meta.Open;
      while not meta.Eof do begin
        indexs.MetaInfoKind := CHILD[kind];
        indexs.BaseObjectName := TableName;
        indexs.ObjectName := meta.FieldByName('INDEX_NAME').AsString;
        indexs.Open;
        F.IndexName := StringToUTF8(indexs.ObjectName);
        F.IsPrimaryKey := not kind;
        F.KeyColumns := '';
        while not indexs.Eof do begin
          ColName := StringToUTF8(indexs.FieldByName('COLUMN_NAME').AsString);
          if F.KeyColumns='' then
            F.KeyColumns := ColName else
            F.KeyColumns := F.KeyColumns+','+ColName;
          indexs.Next;
        end;
        FA.Add(F);
        indexs.Close;
        meta.Next;
      end;
      meta.Close;
    end;
    SetLength(Indexes,n);
  finally
    indexs.Free;
    meta.Free;
  end;
end;

procedure TSQLDBFireDACConnectionProperties.GetForeignKeys;
begin
  { TODO : get FOREIGN KEYS from FireDAC metadata using mkForeignKeys  }
end;

function TSQLDBFireDACConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TSQLDBFireDACConnection.Create(self);
end;


{ TSQLDBFireDACConnection }

procedure TSQLDBFireDACConnection.Commit;
begin
  inherited Commit;
  try
    fDatabase.Commit;
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

constructor TSQLDBFireDACConnection.Create(aProperties: TSQLDBConnectionProperties);
begin
  inherited Create(aProperties);
  fDatabase := TADConnection.Create(nil);
  fDatabase.ResourceOptions.SilentMode := True; // no need for wait cursor
  fDatabase.LoginPrompt := false;
  fDatabase.Params.Text :=
    (fProperties as TSQLDBFireDACConnectionProperties).fFireDACOptions.Text;
end;

procedure TSQLDBFireDACConnection.Connect;
var Log: ISynLog;
begin
  if fDatabase=nil then
    raise ESQLDBFireDAC.CreateUTF8('%.Connect(%): Database=nil',
      [self,fProperties.ServerName]);
  Log := SynDBLog.Enter('Connect to DriverID=% Database=%',
    [FIREDAC_PROVIDER[fProperties.DBMS],fProperties.DatabaseName],self);
  try
    fDatabase.Open;
    inherited Connect; // notify any re-connection 
    Log.Log(sllDB,'Connected to % (%)',
      [fDatabase.DriverName,fProperties.DatabaseName]);
  except
    on E: Exception do begin
      Log.Log(sllError,E);
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

procedure TSQLDBFireDACConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if fDatabase<>nil then
      fDatabase.Close;
  end;
end;

destructor TSQLDBFireDACConnection.Destroy;
begin
  try
   Disconnect;
  except
    on Exception do
  end;
  inherited;
  FreeAndNil(fDatabase);
end;

function TSQLDBFireDACConnection.IsConnected: boolean;
begin
  result := Assigned(fDatabase) and fDatabase.Connected;
end;

function TSQLDBFireDACConnection.NewStatement: TSQLDBStatement;
begin
  result := TSQLDBFireDACStatement.Create(self);
end;

procedure TSQLDBFireDACConnection.Rollback;
begin
  inherited Rollback;
  fDatabase.Rollback;
end;

procedure TSQLDBFireDACConnection.StartTransaction;
begin
  inherited StartTransaction;
  fDatabase.StartTransaction;
end;


{ TSQLDBFireDACStatement }

procedure TSQLDBFireDACStatement.DatasetCreate;
begin
  fQuery := TADQuery.Create(nil);
  TADQuery(fQuery).Connection := (fConnection as TSQLDBFireDACConnection).Database;
  fDatasetSupportBatchBinding := true;
end;

function TSQLDBFireDACStatement.DatasetPrepare(const aSQL: string): boolean;
begin
  (fQuery as TADQuery).SQL.Text := aSQL;
  fQueryParams := TADQuery(fQuery).Params;
  result := fQueryParams<>nil;
end;

procedure TSQLDBFireDACStatement.Prepare(const aSQL: RawUTF8;
  ExpectResults: boolean);
begin
  inherited;
  if fPreparedParamsCount<>fQueryParams.Count then
    raise ESQLDBFireDAC.CreateUTF8(
      '%.Prepare() expected % parameters in request, found % - [%]',
      [self,fPreparedParamsCount,fQueryParams.Count,aSQL]);
end;

procedure TSQLDBFireDACStatement.DatasetExecSQL;
begin
  if fPreparedUseArrayDML then
    (fQuery as TADQuery).Execute(fParamsArrayCount) else
    (fQuery as TADQuery).Execute;
end;

procedure TSQLDBFireDACStatement.DataSetBindSQLParam(const aArrayIndex,
  aParamIndex: integer; const aParam: TSQLDBParam);
var P: TADParam;
    i: integer;
    tmp: RawUTF8;
    StoreVoidStringAsNull: boolean;
begin
  if fDatasetSupportBatchBinding then
    fPreparedUseArrayDML := (aArrayIndex<0) and (fParamsArrayCount>0) else
    fPreparedUseArrayDML := false;
  if fPreparedUseArrayDML and (fQueryParams.ArraySize<>fParamsArrayCount) then
    fQueryParams.ArraySize := fParamsArrayCount;
  with aParam do begin
    P := fQueryParams[aParamIndex];
    P.ParamType := SQLParamTypeToDBParamType(VInOut);
    if VinOut <> paramInOut then
      case VType of
        SynTable.ftNull:
          if fPreparedUseArrayDML then
            for i := 0 to fParamsArrayCount-1 do
              P.Clear(i) else
            P.Clear;
        SynTable.ftInt64: begin
          if fPreparedUseArrayDML then
            for i := 0 to fParamsArrayCount-1 do
              if VArray[i]='null' then
                P.Clear(i) else
                P.AsLargeInts[i] := GetInt64(pointer(VArray[i])) else
          if aArrayIndex>=0 then
            if VArray[aArrayIndex]='null' then
              P.Clear else
              P.AsLargeInt := GetInt64(pointer(VArray[aArrayIndex])) else
            P.AsLargeInt := VInt64;
        end;
        SynTable.ftDouble:
          if fPreparedUseArrayDML then
            for i := 0 to fParamsArrayCount-1 do
              if VArray[i]='null' then
                P.Clear(i) else
                P.AsFloats[i] := GetExtended(pointer(VArray[i])) else
          if aArrayIndex>=0 then
            if VArray[aArrayIndex]='null' then
              P.Clear else
              P.AsFloat := GetExtended(pointer(VArray[aArrayIndex])) else
            P.AsFloat := PDouble(@VInt64)^;
        SynTable.ftCurrency:
          if fPreparedUseArrayDML then
            for i := 0 to fParamsArrayCount-1 do
              if VArray[i]='null' then
                P.Clear(i) else
                P.AsCurrencys[i] := StrToCurrency(pointer(VArray[i])) else
          if aArrayIndex>=0 then
            if VArray[aArrayIndex]='null' then
              P.Clear else
              P.AsCurrency := StrToCurrency(pointer(VArray[aArrayIndex])) else
            P.AsCurrency := PCurrency(@VInt64)^;
        SynTable.ftDate:
          if fPreparedUseArrayDML then
            for i := 0 to fParamsArrayCount-1 do
            if VArray[i]='null' then
              P.Clear(i) else begin
              UnQuoteSQLStringVar(pointer(VArray[i]),tmp);
              P.AsDateTimes[i] := Iso8601ToDateTime(tmp);
            end else
          if aArrayIndex>=0 then
            if VArray[aArrayIndex]='null' then
              P.Clear else begin
              UnQuoteSQLStringVar(pointer(VArray[aArrayIndex]),tmp);
              P.AsDateTime := Iso8601ToDateTime(tmp);
            end else
              P.AsDateTime := PDateTime(@VInt64)^;
        SynTable.ftUTF8:
          if fPreparedUseArrayDML then begin
            StoreVoidStringAsNull := fConnection.Properties.StoreVoidStringAsNull;
            for i := 0 to fParamsArrayCount-1 do
              if (VArray[i]='null') or
                 (StoreVoidStringAsNull and (VArray[i]=#39#39)) then
                P.Clear(i) else begin
              UnQuoteSQLStringVar(pointer(VArray[i]),tmp);
              {$ifdef UNICODE} // for FireDAC: TADWideString=UnicodeString
              P.AsWideStrings[i] := UTF8ToString(tmp);
              {$else}
              if fForceUseWideString then
                P.AsWideStrings[i] := UTF8ToWideString(tmp) else
                P.AsStrings[i] := UTF8ToString(tmp);
              {$endif}
            end
          end else
          if aArrayIndex>=0 then
            if (VArray[aArrayIndex]='null') or
               (fConnection.Properties.StoreVoidStringAsNull and
                (VArray[aArrayIndex]=#39#39)) then
              P.Clear else begin
              UnQuoteSQLStringVar(pointer(VArray[aArrayIndex]),tmp);
              {$ifdef UNICODE}
              P.AsWideString := UTF8ToString(tmp); // TADWideString=string
              {$else}
              if fForceUseWideString then
                P.AsWideString := UTF8ToWideString(tmp) else
                P.AsString := UTF8ToString(tmp);
              {$endif}
          end else
            if (VData='') and fConnection.Properties.StoreVoidStringAsNull then
              P.Clear else 
              {$ifdef UNICODE}
              P.AsWideString := UTF8ToString(VData); // TADWideString=string
              {$else}
              if (not fForceUseWideString) {or IsAnsiCompatible(VData)} then
                P.AsString := UTF8ToString(VData) else
                P.AsWideString := UTF8ToWideString(VData);
              {$endif}
        SynTable.ftBlob:
          if fPreparedUseArrayDML then
            for i := 0 to fParamsArrayCount-1 do
              if VArray[i]='null' then
                P.Clear(i) else
                P.AsBlobs[i] := VArray[i] else
          if aArrayIndex>=0 then
            if VArray[aArrayIndex]='null' then
              P.Clear else
              P.AsBlob := VArray[aArrayIndex] else
            P.AsBlob := VData;
        else
          raise ESQLDBFireDAC.CreateUTF8(
            '%.DataSetBindSQLParam: invalid type % on bound parameter #%',
            [Self,ord(VType),aParamIndex+1]);
        end;   
  end;
end;

procedure TSQLDBFireDACStatement.DataSetOutSQLParam(const aParamIndex: integer;
  var aParam: TSQLDBParam);
var Par: TADParam;
begin
  Par := fQueryParams[aParamIndex];
  case aParam.VType of
    SynTable.ftInt64:    aParam.VInt64 := Par.AsLargeInt;
    SynTable.ftDouble:   PDouble(@aParam.VInt64)^ := Par.AsFloat;
    SynTable.ftCurrency: PCurrency(@aParam.VInt64)^ := Par.AsCurrency;
    SynTable.ftDate:     PDateTime(@aParam.VInt64)^ := Par.AsDateTime;
    SynTable.ftUTF8:     aParam.VData := StringToUTF8(Par.AsString);
    SynTable.ftBlob:     aParam.VData := Par.AsBlob;
  end;
end;

initialization
  TSQLDBFireDACConnectionProperties.RegisterClassNameForDefinition;
end.
