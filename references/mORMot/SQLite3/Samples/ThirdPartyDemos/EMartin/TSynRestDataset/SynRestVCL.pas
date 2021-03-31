/// fill a VCL TClientDataset from SynVirtualDataset data access
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynRestVCL;

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
  - Esteban Martin (EMartin)
  - houdw2006

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

  Version 1.18
  - first public release, corresponding to Synopse mORMot Framework 1.18,
    which is an extraction from former SynDBVCL.pas unit.
  - Added that blob field updates they are made with AddJSONEscapeString.
  - bug fix when updating accentuated string fields.
  - bug fix with datetime fields
  - bug fix with length string fields
  - fixed Delphi XE3 compilation issue with PSExecuteStatement declaration (by houdw2006)
  - added sftSessionUserID to SQLFIELDTYPETODBFIELDTYPE and SQLFieldTypeToVCLDB

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  Classes,
{$ifndef DELPHI5OROLDER}
  Variants,
  {$ifndef FPC}
  MidasLib,
  {$endif}
{$endif}
  mORMot,
  mORMotHttpClient,
  SynCrtSock, // remover una vez implementado TSQLHttpClient
  SynCommons,
  SynTable,
  SynDB,
  SynDBVCL,
  DB,
  {$ifdef FPC}
  BufDataset
  {$else}
  Contnrs,
  DBClient,
  Provider,
  SqlConst
  {$endif};


type
  /// generic Exception type
  ESQLRestException = class(ESynException);

  /// URI signature event
  TOnGetURISignature = procedure(Sender: TObject; var aURI: string) of object;

  /// a TDataSet which allows to apply updates on a Restful connection
  // - typical usage may be for instance:
  // ! ds := TSynRestDataSet.Create(MainForm);
  // ! ds.Dataset.SQLModel := CreateModel; // The SQLModel is required
  // ! ds.CommandText := 'http://host:port/root/TableName?select=*&where=condition&sort=fieldname';
  // ! ds1.Dataset := ds; // assigning the rest dataset to TDatasource that can be associated a TDBGrid for example.
  // ! ds.Open;
  // ! // ... use ds as usual, including modifications
  // ! ds.ApplyUpdates(0);
  //   or using from a service returning a dataset:
  // ! ds := TSynRestDataSet.Create(MainForm);
  // ! ds.Dataset.SQLModel := CreateModel; // The SQLModel is required
  // ! the TSQLRecord associated should be defined with the same structure of the returned array from the service
  // ! ds.CommandText := 'http://host:port/root/ServiceName.Operation?paramname=:paramvalue';
  // ! ds.Params.ParamByName('paramname').Value := 'xyz';
  // ! ds1.Dataset := ds; // assigning the rest dataset to TDatasource that can be associated a TDBGrid for example.
  // ! ds.Open;
  // ! // ... use ds as usual, including modifications
  // ! ds.ApplyUpdates(0);
  TSynRestSQLDataSet = class(TSynBinaryDataSet)
  protected
    fBaseURL: RawUTF8;
    fCommandText: string;
    fDataSet: TSynBinaryDataSet;
    fOnGetURISignature: TOnGetURISignature;
    fParams: TParams;
    fProvider: TDataSetProvider;
    fRoot: RawUTF8;
    fSQLModel: TSQLModel;
    fTableName: RawUTF8;
    fURI: TURI;
    function BindParams(const aStatement: RawUTF8): RawUTF8;
    function BuildURI(const aURI: SockString): SockString;
    function GetSQLRecordClass: TSQLRecordClass;
    function GetTableName: string;
    // get the data
    procedure InternalInitFieldDefs; override;
    function InternalFrom(const aStatement: RawUTF8): RawByteString;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function IsTableFromService: Boolean;
    procedure ParseCommandText;
    // IProvider implementation
    procedure PSSetCommandText(const ACommandText: string); override;
    function PSGetTableName: string; override;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    {$ifdef ISDELPHIXE3}
    function PSExecuteStatement(const ASQL: string; AParams: TParams): Integer; overload; override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams; var ResultSet: TDataSet): Integer; overload; override;
    {$else}
    function PSExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer=nil): Integer; overload; override;
    {$endif}
    procedure SetCommandText(const Value: string);
  public
    /// the associated Model, if not defined an exception is raised.
    property SQLModel: TSQLModel read fSQLModel write fSQLModel;
  published
    /// the GET RESTful URI
    // - Statement will have the form http://host:port/root/tablename or
    //   http://host:port/root/servicename.operationname?paramname=:paramalias
    // examples:
    //   http://host:port/root/tablename?select=XXX or
    //   http://host:port/root/tablename?select=XXX&where=field1=XXX or field2=XXX
    //   http://host:port/root/service.operation?param=:param
    // if :param is used then before open assign the value: ds.Params.ParamByName('param').value := XXX
    property CommandText: string read fCommandText write fCommandText;
    /// the associated SynDB TDataSet, used to retrieve and update data
    property DataSet: TSynBinaryDataSet read fDataSet;
    /// event to get URI signature
    property OnGetURISignature: TOnGetURISignature write fOnGetURISignature;
  end;

// JSON columns to binary from a TSQLTableJSON, is not ideal because this code is a almost repeated code.
procedure JSONColumnsToBinary(const aTable: TSQLTableJSON; W: TFileBufferWriter;
  Null: pointer; const ColTypes: TSQLDBFieldTypeDynArray);
// convert to binary from a TSQLTableJSON, is not ideal because this code is a almost repeated code.
function JSONToBinary(const aTable: TSQLTableJSON; Dest: TStream; MaxRowCount: cardinal=0; DataRowPosition: PCardinalDynArray=nil;
  const DefaultDataType: TSQLDBFieldType = SynTable.ftUTF8; const DefaultFieldSize: Integer = 255): cardinal;

implementation

uses
  DBCommon,
  SynVirtualDataset;

const
  FETCHALLTOBINARY_MAGIC = 1;

  SQLFIELDTYPETODBFIELDTYPE: array[TSQLFieldType] of TSQLDBFieldType =
    (SynTable.ftUnknown,   // sftUnknown
     SynTable.ftUTF8,      // sftAnsiText
     SynTable.ftUTF8,      // sftUTF8Text
     SynTable.ftInt64,     // sftEnumerate
     SynTable.ftInt64,     // sftSet
     SynTable.ftInt64,     // sftInteger
     SynTable.ftInt64,     // sftID = TSQLRecord(aID)
     SynTable.ftInt64,     // sftRecord = TRecordReference
     SynTable.ftInt64,     // sftBoolean
     SynTable.ftDouble,    // sftFloat
     SynTable.ftDate,      // sftDateTime
     SynTable.ftInt64,     // sftTimeLog
     SynTable.ftCurrency,  // sftCurrency
     SynTable.ftUTF8,      // sftObject
{$ifndef NOVARIANTS}
     SynTable.ftUTF8,      // sftVariant
     SynTable.ftUTF8,      // sftNullable
{$endif}
     SynTable.ftBlob,      // sftBlob
     SynTable.ftBlob,      // sftBlobDynArray
     SynTable.ftBlob,      // sftBlobCustom
     SynTable.ftUTF8,      // sftUTF8Custom
     SynTable.ftUnknown,   // sftMany
     SynTable.ftInt64,     // sftModTime
     SynTable.ftInt64,     // sftCreateTime
     SynTable.ftInt64,     // sftTID
     SynTable.ftInt64,     // sftRecordVersion = TRecordVersion
     SynTable.ftInt64,     // sftSessionUserID
     SynTable.ftDate,      // sftDateTimeMS
     SynTable.ftInt64,     // sftUnixTime
     SynTable.ftInt64);    // sftUnixMSTime

  SQLFieldTypeToVCLDB: array[TSQLFieldType] of TFieldType =
    (DB.ftUnknown,           // sftUnknown
     DB.ftString,            // sftAnsiText
     DB.ftString,            // sftUTF8Text
     DB.ftLargeInt,          // sftEnumerate
     DB.ftLargeInt,          // sftSet
     DB.ftLargeInt,          // sftInteger
     DB.ftLargeInt,          // sftID = TSQLRecord(aID)
     DB.ftLargeInt,          // sftRecord = TRecordReference
     DB.ftLargeInt,          // sftBoolean
     DB.ftFloat,             // sftFloat
     DB.ftDateTime,          // sftDateTime
     DB.ftLargeInt,          // sftTimeLog
     DB.ftCurrency,          // sftCurrency
     DB.ftString,            // sftObject
{$ifndef NOVARIANTS}
     DB.ftString,            // sftVariant
     DB.ftString,            // sftNullable
{$endif}
     DB.ftBlob,              // sftBlob
     DB.ftBlob,              // sftBlobDynArray
     DB.ftBlob,              // sftBlobCustom
     DB.ftString,            // sftUTF8Custom
     DB.ftUnknown,           // sftMany
     DB.ftLargeInt,          // sftModTime
     DB.ftLargeInt,          // sftCreateTime
     DB.ftLargeInt,          // sftTID
     DB.ftLargeInt,          // sftRecordVersion = TRecordVersion
     DB.ftLargeInt,          // sftSessionUserID
     DB.ftDateTime,          // sftDateTime
     DB.ftLargeInt,          // sftUnixTime
     DB.ftLargeInt);         // sftUnixMSTime

  VCLDBFieldTypeSQLDB: array[0..23] of TSQLFieldType =
    (sftUnknown,        // ftUnknown
     sftAnsiText,       //  ftString
     sftUTF8Text,       // ftString
     sftEnumerate,      // ftInteger
     sftSet,            // ftInteger
     sftInteger,        // ftInteger
     sftID,             // ftLargeInt = TSQLRecord(aID)
     sftRecord,         // ftLargeInt
     sftBoolean,        // ftBoolean
     sftFloat,          // ftFloat
     sftDateTime,       // ftDate
     sftTimeLog,        // ftLargeInt
     sftCurrency,       // ftCurrency
     sftObject,         // ftString
{$ifndef NOVARIANTS}
     sftVariant,        // ftString
{$endif}
     sftBlob,           // ftBlob
     sftBlob,           // ftBlob
     sftBlob,           // ftBlob
     sftUTF8Custom,     // ftString
     sftMany,           // ftUnknown
     sftModTime,        // ftLargeInt
     sftCreateTime,     // ftLargeInt
     sftID,             // ftLargeInt
     sftRecordVersion); // ftLargeInt = TRecordVersion

{$ifndef FPC}


procedure JSONColumnsToBinary(const aTable: TSQLTableJSON; W: TFileBufferWriter;
  Null: pointer; const ColTypes: TSQLDBFieldTypeDynArray);
var F: integer;
    VDouble: double;
    VCurrency: currency absolute VDouble;
    VDateTime: TDateTime absolute VDouble;
    colType: TSQLDBFieldType;
begin
  for F := 0 to length(ColTypes)-1 do
    if not GetBitPtr(Null,F) then begin
      colType := ColTypes[F];
      if colType<ftInt64 then begin // ftUnknown,ftNull
        colType := SQLFIELDTYPETODBFIELDTYPE[aTable.FieldType(F)]; // per-row column type (SQLite3 only)
        W.Write1(ord(colType));
      end;
      case colType of
      ftInt64:
      begin
        W.WriteVarInt64(aTable.FieldAsInteger(F));
      end;
      ftDouble: begin
        VDouble := aTable.FieldAsFloat(F);
        W.Write(@VDouble,sizeof(VDouble));
      end;
      SynTable.ftCurrency: begin
        VCurrency := aTable.Field(F);
        W.Write(@VCurrency,sizeof(VCurrency));
      end;
      SynTable.ftDate: begin
        VDateTime := aTable.Field(F);
        W.Write(@VDateTime,sizeof(VDateTime));
      end;
      SynTable.ftUTF8:
      begin
        W.Write(aTable.FieldBuffer(F));
      end;
      SynTable.ftBlob:
      begin
        W.Write(aTable.FieldBuffer(F));
      end;
      else
      raise ESQLDBException.CreateUTF8('JSONColumnsToBinary: Invalid ColumnType(%)=%',
        [aTable.Get(0, F),ord(colType)]);
    end;
  end;
end;

function JSONToBinary(const aTable: TSQLTableJSON; Dest: TStream; MaxRowCount: cardinal=0; DataRowPosition: PCardinalDynArray=nil;
                      const DefaultDataType: TSQLDBFieldType = SynTable.ftUTF8; const DefaultFieldSize: Integer = 255): cardinal;
var F, FMax, FieldSize, NullRowSize: integer;
    StartPos: cardinal;
    Null: TByteDynArray;
    W: TFileBufferWriter;
    ColTypes: TSQLDBFieldTypeDynArray;
    FieldType: TSQLDBFieldType;
begin
  result := 0;
  W := TFileBufferWriter.Create(Dest);
  try
    W.WriteVarUInt32(FETCHALLTOBINARY_MAGIC);
    FMax := aTable.FieldCount;
    W.WriteVarUInt32(FMax);
    if FMax>0 then begin
      // write column description
      SetLength(ColTypes,FMax);
      dec(FMax);
      for F := 0 to FMax do begin
        W.Write(aTable.Get(0, F));
        FieldType := SQLFIELDTYPETODBFIELDTYPE[aTable.FieldType(F)];
        if (FieldType = SynTable.ftUnknown) and (DefaultDataType <> SynTable.ftUnknown) then
          FieldType := DefaultDataType;
        ColTypes[F] := FieldType;
        FieldSize := aTable.FieldLengthMax(F);
        if (FieldSize = 0) and (FieldType = DefaultDataType) and (DefaultFieldSize <> 0) then
          FieldSize := DefaultFieldSize;
        W.Write1(ord(ColTypes[F]));
        W.WriteVarUInt32(FieldSize);
      end;
      // initialize null handling
      SetLength(Null,(FMax shr 3)+1);
      NullRowSize := 0;
      // save all data rows
      StartPos := W.TotalWritten;
      if aTable.Step or (aTable.RowCount=1) then // Need step first or error is raised in Table.Field function.
      repeat
        // save row position in DataRowPosition[] (if any)
        if DataRowPosition<>nil then begin
          if Length(DataRowPosition^)<=integer(result) then
            SetLength(DataRowPosition^,result+result shr 3+256);
          DataRowPosition^[result] := W.TotalWritten-StartPos;
        end;
        // first write null columns flags
        if NullRowSize>0 then begin
          FillChar(Null[0],NullRowSize,0);
          NullRowSize := 0;
        end;
        for F := 0 to FMax do
        begin
          if VarIsNull(aTable.Field(F)) then begin
            SetBitPtr(pointer(Null),F);
            NullRowSize := (F shr 3)+1;
          end;
        end;
        W.WriteVarUInt32(NullRowSize);
        if NullRowSize>0 then
          W.Write(Null,NullRowSize);
        // then write data values
        JSONColumnsToBinary(aTable, W,Null,ColTypes);
        inc(result);
        if (MaxRowCount>0) and (result>=MaxRowCount) then
          break;
      until not aTable.Step;
    end;
    W.Write(@result,SizeOf(result)); // fixed size at the end for row count
    W.Flush;
  finally
    W.Free;
  end;
end;

{ TSynRestSQLDataSet }

function TSynRestSQLDataSet.BindParams(const aStatement: RawUTF8): RawUTF8;
var
  I: Integer;
  lParamName: string;
begin
  Result := aStatement;
  if (Pos(':', aStatement) = 0) and (fParams.Count = 0) then
    Exit;
  if ((Pos(':', aStatement) = 0) and (fParams.Count > 0)) or ((Pos(':', aStatement) > 0) and (fParams.Count = 0)) then
    raise ESQLRestException.CreateUTF8('Statement parameters (%) not match with Params (Count=%) property',
      [aStatement, fParams.Count]);
  for I := 0 to fParams.Count-1 do
  begin
    lParamName := ':' + fParams[I].Name;
    Result := StringReplace(Result, lParamName, fParams[I].AsString, [rfIgnoreCase]);
  end;
  // remove space before and after &
  Result := StringReplaceAll(Result, ' & ', '&');
end;

function TSynRestSQLDataSet.BuildURI(const aURI: SockString): SockString;
var
  lTmpURI: string;
begin
  lTmpURI := aURI;
  if Assigned(fOnGetURISignature) then
    fOnGetURISignature(Self, lTmpURI);
  Result := FormatUTF8('%%' , [fBaseURL, lTmpURI]);
  if fURI.Https and (Result[5] <> 's') then
    System.Insert('s', Result, 5);
end;

function TSynRestSQLDataSet.GetSQLRecordClass: TSQLRecordClass;
begin
  Result := fSQLModel.Table[GetTableName];
  if not Assigned(Result) then
    raise ESQLRestException.CreateUTF8('Table % not registered in SQL Model', [GetTableName]);
end;

function TSynRestSQLDataSet.GetTableName: string;
var
  I: Integer;
begin
  if not IsTableFromService then
    Result := PSGetTableName
  else
  begin
    Result := fTableName;
    for I := 1 to Length(Result) do
      if (Result[I] = '.') then
      begin
        Result[I] := '_';  // change only the firs found
        Break;
      end;
  end;
end;

procedure TSynRestSQLDataSet.InternalClose;
begin
  inherited InternalClose;
  FreeAndNil(fDataAccess);
  fData := '';
end;

function TSynRestSQLDataSet.InternalFrom(const aStatement: RawUTF8): RawByteString;

  procedure UpdateFields(aSQLTableJSON: TSQLTableJSON);
  var
    I, J: Integer;
    lFields: TSQLPropInfoList;
  begin
    lFields := GetSQLRecordClass.RecordProps.Fields;
    for I := 0 to aSQLTableJSON.FieldCount-1 do
    begin
      J := lFields.IndexByName(aSQLTableJSON.Get(0, I));
      if (J > -1) then
        aSQLTableJSON.SetFieldType(I, lFields.Items[J].SQLFieldType, Nil, lFields.Items[J].FieldWidth);
    end;
  end;

var
  lData: TRawByteStringStream;
  lSQLTableJSON: TSQLTableJSON;
  lStatement: RawUTF8;
  lDocVar: TDocVariantData;
  lTmp: RawUTF8;
  lResp: TDocVariantData;
  lErrMsg: RawUTF8;
  lURI: RawUTF8;
begin
  Result := '';
  lStatement := BindParams(aStatement);
  if (lStatement <> '') then
    lStatement := '?' + lStatement;
  lURI := BuildURI(fRoot + fTableName + lStatement);
  Result := TWinHTTP.Get(lURI);
  if (Result = '') then
    raise ESynException.CreateUTF8('Cannot get response (timeout?) from %', [lURI]);
  if (Result <> '') then
  begin
    lResp.InitJSON(Result);
    if (lResp.Kind = dvUndefined) then
      raise ESynException.CreateUTF8('Invalid JSON response' + sLineBreak + '%' + sLineBreak + 'from' + sLineBreak + '%',
                                     [Result, lURI]);
    if (lResp.Kind = dvObject) then
      if (lResp.GetValueIndex('errorCode') > -1) then
        if (lResp.GetValueIndex('errorText') > -1) then
        begin
          lErrMsg := AnyAnsiToUTF8(lResp.Value['errorText']);
          raise ESynException.CreateUTF8('Error' + sLineBreak + '%' + sLineBreak + 'from' + sLineBreak + '%',
                                         [lResp.Value['errorText'], lURI]);
        end
        else if (lResp.GetValueIndex('error') > -1) then
        begin
          lErrMsg := AnyAnsiToUTF8(lResp.Value['error']);
          raise ESynException.CreateUTF8('Error' + sLineBreak + '%' + sLineBreak + 'from' + sLineBreak + '%', [lErrMsg, lURI]);
        end;

    if IsTableFromService then // is the source dataset from a service ?
    begin
      lDocVar.InitJSON(Result);
      lTmp := lDocVar.Values[0];
      lDocVar.Clear;
      lDocVar.InitJSON(lTmp);
      if (lDocVar.Kind <> dvArray) then
        raise ESQLRestException.CreateUTF8('The service % not return an array: <%>', [fTableName, Result]);
      // if the array is empty, nothing to return
      Result := lDocVar.Values[0];
      if (Result = '') or (Result = '[]') or (Result = '{}') then
        raise ESQLRestException.CreateUTF8('Service % not return a valid array: <%>', [fTableName, Result]);
    end;
    lSQLTableJSON := TSQLTableJSON.CreateFromTables([GetSQLRecordClass], '', Result);
    // update info fields for avoid error conversion in JSONToBinary
    UpdateFields(lSQLTableJSON);
    lData := TRawByteStringStream.Create('');
    try
      JSONToBinary(lSQLTableJSON, lData);
      Result := lData.DataString
    finally
      FreeAndNil(lData);
      FreeAndNil(lSQLTableJSON);
    end;
  end;
end;

procedure TSynRestSQLDataSet.InternalInitFieldDefs;
var F: integer;
    lFields: TSQLPropInfoList;
    lFieldDef: TFieldDef;
    lOldSize: Int64;
begin
  inherited;
  if (GetTableName = '') then // JSON conversion to dataset ?
    Exit;
  // update field definitions from associated TSQLRecordClass of the table
  lFields := GetSQLRecordClass.RecordProps.Fields;
  for F := 0 to lFields.Count-1 do
  begin
    lFieldDef := TFieldDef(TDefCollection(FieldDefs).Find(lFields.Items[F].Name));
    if Assigned(lFieldDef) then
    begin
      if (lFieldDef.DataType <> SQLFieldTypeToVCLDB[lFields.Items[F].SQLFieldType]) then
      begin
        lOldSize := lFieldDef.Size; // DB.pas.TFieldDef.SetDataType change the size
        lFieldDef.DataType := SQLFieldTypeToVCLDB[lFields.Items[F].SQLFieldType];
      end;
      if (lFields.Items[F].FieldWidth > 0) and (lFieldDef.Size < lFields.Items[F].FieldWidth) then
        lFieldDef.Size := lFields.Items[F].FieldWidth
      else if (lOldSize > 0) and (lFieldDef.Size > 0) and (lOldSize <> lFieldDef.Size) then
        lFieldDef.Size := lOldSize;
    end;
  end;
end;

function TSynRestSQLDataSet.IsTableFromService: Boolean;
begin
  Result := (Pos('.', fTableName) > 0);
end;

procedure TSynRestSQLDataSet.InternalOpen;
var
  lData: RawByteString;
begin
  if (fCommandText='') and (not IsTableFromService) then begin
    if fData<>'' then // called e.g. after From() method
      inherited InternalOpen;
    exit;
  end;
  lData := InternalFrom(fCommandText);
  if (lData <> '') then
  begin
    From(lData);
    inherited InternalOpen;
  end;
end;

procedure TSynRestSQLDataSet.ParseCommandText;
var
  lSQL: RawUTF8;
begin
  // it is assumed http://host:port/root/tablename, the rest is optional: ?select=&where=&sort= etc.
  if not fURI.From(fCommandText) then
    raise ESynException.CreateUTF8('Invalid % command text. Must have the format protocol://host:port', [fCommandText]);
  if not fURI.Https then
    fBaseURL := FormatUTF8('http://%:%/', [fURI.Server, fURI.Port])
  else
    fBaseURL := FormatUTF8('https://%:%/', [fURI.Server, fURI.Port]);
  Split(fURI.Address, '/', fRoot, fTableName);
  if (fRoot = '') or (fTableName = '') then
    raise ESynException.CreateUTF8('Invalid % root. Must have the format protocol://host:port/root/tablename', [fCommandText]);
  fRoot := fRoot + '/';
  if (Pos('?', fTableName) > 0) then
    Split(fTableName, '?', fTableName, lSQL);
  if not Assigned(fSQLModel) then
    raise ESQLRestException.CreateUTF8('Error parsing command text. Empty Model.', []);
  fCommandText := lSQL
end;

{$ifdef ISDELPHIXE3}
function TSynRestSQLDataSet.PSExecuteStatement(const ASQL: string;
  AParams: TParams): Integer;
var DS: TDataSet;
begin
  DS := nil;
  result := PSExecuteStatement(ASQL,AParams,DS);
  DS.Free;
end;
function TSynRestSQLDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams; var ResultSet: TDataSet): Integer;
{$else}
function TSynRestSQLDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer): Integer;
{$endif}

  function Compute(const aJSON: SockString; const aOccasion: TSQLOccasion): SockString;
  var
    lRec: TSQLRecord;
    lRecBak: TSQLRecord; // backup for get modifications
    lJSON: TDocVariantData;
    I: Integer;
    lCount: Integer;
    lOccasion: TSQLEvent;
    lVarValue: Variant;
    lVarValueBak: Variant;
  begin
    lRec := GetSQLRecordClass.Create;
    lRecBak := GetSQLRecordClass.Create;
    try
      lJSON.InitJSON(aJSON);
      lCount := lJSON.Count;
      // update record fields
      for I := 0 to lCount-1 do
        lRec.SetFieldVariant(lJSON.Names[I], lJSON.Values[I]);
      lOccasion := seUpdate;
      if (aOccasion = soInsert) then
        lOccasion := seAdd;
      lRec.ComputeFieldsBeforeWrite(Nil, lOccasion);
      // get modified fields
      for I := 0 to lRec.RecordProps.Fields.Count-1 do
      begin
        lRec.RecordProps.Fields.Items[I].GetVariant(lRec, lVarValue);
        lRecBak.RecordProps.Fields.Items[I].GetVariant(lRecBak, lVarValueBak);
        if (lVarValue <> lVarValueBak) then
          lJSON.AddOrUpdateValue(lRec.RecordProps.Fields.Items[I].Name, lVarValue);
      end;
      Result := lJSON.ToJSON;
    finally
      lRec.Free;
      lRecBak.Free;
    end;
  end;

  function ExtractFields(const aSQL, aAfterStr, aBeforeStr: string): string;
  var
    lPosStart: Integer;
    lPosEnd: Integer;
    lSQL: string;
  begin
    lSQL := StringReplace(aSQL, sLineBreak, ' ', [rfReplaceAll]);
    lPosStart := Pos(aAfterStr, lSQL)+Length(aAfterStr);
    lPosEnd   := Pos(aBeforeStr, lSQL);
    Result := Trim(Copy(lSQL, lPosStart, lPosEnd-lPosStart));
  end;

  function SQLFieldsToJSON(const aSQLOccasion: TSQLOccasion; const aSQL, aAfterStr, aBeforeStr: string; aParams: TParams): SockString;
  var
    I: Integer;
    lLastPos: Integer;
    lFieldValues: TStrings;
    lBlob: TSQLRawBlob;
  begin
    lFieldValues := TStringList.Create;
    try
      ExtractStrings([','], [], PChar(ExtractFields(aSQL, aAfterStr, aBeforeStr)), lFieldValues);
      lLastPos := 0;
      with TTextWriter.CreateOwnedStream do
      begin
        Add('{');
        for I := 0 to lFieldValues.Count-1 do
        begin
          if (Pos('=', lFieldValues[I]) = 0) then
            lFieldValues[I] := lFieldValues[I] + '=';
          AddFieldName(Trim(lFieldValues.Names[I]));
          if (aParams[I].DataType <> ftBlob) then
          begin
            if (TVarData(aParams[I].Value).VType = varString) then
              AddVariant(StringToUTF8(aParams[I].Value))
            else
              AddVariant(aParams[I].Value);
          end
          else
          begin
            Add('"');
            lBlob :=  BlobToTSQLRawBlob(PUTF8Char(aParams[I].AsBlob));
            AddJSONEscapeString(lBlob);
            Add('"');
          end;
          Add(',');
          lLastPos := I;
        end;
        CancelLastComma;
        Add('}');
        Result := Text;
        Free;
      end;
      lFieldValues.Clear;
      // the first field after the where clause is the ID
      if (aSQLOccasion <> soInsert) then
        aParams[lLastPos+1].Name := 'ID';
    finally
      lFieldValues.Free;
    end;
  end;

  function GetSQLOccasion(const aSQL: string): TSQLOccasion;
  begin
    if IdemPChar(PUTF8Char(UpperCase(aSQL)), 'DELETE') then
      Result := soDelete
    else if IdemPChar(PUTF8Char(UpperCase(aSQL)), 'INSERT') then
      Result := soInsert
    else
      Result := soUpdate;
  end;

var
  lJSON: SockString;
  lOccasion: TSQLOccasion;
  lResult: SockString;
  lURI: SockString;
  lID: string;
begin // only execute writes in current implementation
  Result := -1;
  if IsTableFromService then
    DatabaseError('Cannot apply updates from a service');
  // build the RESTful URL
  lURI := FormatUTF8('%/%', [fSQLModel.Root, StringToUTF8(PSGetTableName)]);
  lOccasion := GetSQLOccasion(aSQL);
  case lOccasion of
    soDelete:
    begin
      lID := aParams[0].Value;
      lURI := lURI + '/' + lID;
      lResult := TWinHTTP.Delete(BuildURI(lURI), '');
      if (lResult = '') then
        Result := 1;
    end;
    soInsert:
    begin
      lJSON := SQLFieldsToJSON(soInsert, aSQL, '(', ') ', aParams);
      try
        lJSON := Compute(lJSON, soInsert);
      except
        Result := -1;
        lResult := Exception(ExceptObject).Message;
      end;
      lResult := TWinHTTP.Post(BuildURI(lURI), lJSON);
      if (lResult = '') then
        Result := 1;
    end;
    soUpdate:
    begin
      lJSON := SQLFieldsToJSON(soUpdate, aSQL, 'set ', 'where ', aParams);
      try
        lJSON := Compute(lJSON, soUpdate);
      except
        Result := -1;
        lResult := Exception(ExceptObject).Message;
      end;
      lID := aParams.ParamByName('ID').Value;
      lURI := lURI + '/' + lID;
      lResult := TWinHTTP.Put(BuildURI(lURI), lJSON);
      if (lResult = '') then
        Result := 1;
    end
  end;
  if (Result = -1) and (lResult <> '') then
    DatabaseError(lResult);
end;

function TSynRestSQLDataSet.PSGetTableName: string;
begin
  Result := fTableName;
end;

function TSynRestSQLDataSet.PSIsSQLBased: Boolean;
begin
  result := true;
end;

function TSynRestSQLDataSet.PSIsSQLSupported: Boolean;
begin
  result := true;
end;

procedure TSynRestSQLDataSet.PSSetCommandText(const ACommandText: string);
begin
  if (fCommandText <> ACommandText) then
    SetCommandText(ACommandText);
end;

function TSynRestSQLDataSet.PSUpdateRecord(UpdateKind: TUpdateKind;
  Delta: TDataSet): Boolean;
begin
  result := false;
end;

procedure TSynRestSQLDataSet.SetCommandText(const Value: string);
begin
  if (Value <> fCommandtext) then
  begin
    fCommandText := Value;
    ParseCommandText;
  end;
end;

{$endif FPC}

end.


