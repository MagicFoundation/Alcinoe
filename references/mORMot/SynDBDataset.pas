/// DB.pas TDataset-based direct access classes (abstract TQuery-like)
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynDBDataset; 

{
  This file is part of Synopse framework.

  Synopse framework. Copyright (C) 2020 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2020
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - itSDS


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
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  SysUtils,
  {$IFNDEF DELPHI5OROLDER}
  Variants,
  {$ENDIF}
  Classes, Contnrs,
  SynCommons,
  SynTable,
  SynLog,
  SynDB,
  {$ifdef ISDELPHIXE2}
  Data.DB,
  {$else}
  DB,
  {$endif}
  SynVirtualDataSet; // for AddBcd/BCDToCurr


{ -------------- DB.pas TDataSet (TQuery like) abstract connection }

type
  {$ifndef ISDELPHIXE4}
  TValueBuffer = Pointer;
  {$endif}

  /// Exception type associated to generic TDataSet / DB.pas unit Dataset connection
  ESQLDBDataset = class(ESQLDBException);

  ///	implement properties shared by via the DB.pas TQuery-like connections
  TSQLDBDatasetConnectionProperties = class(TSQLDBConnectionPropertiesThreadSafe)
  protected
    {$ifndef UNICODE}
    fForceInt64AsFloat: boolean;
    {$endif}
    fForceUseWideString: boolean;
  public
    /// initialize the properties to connect via TDataSet database access
    // - this overridden method will enable the BATCH process (emulated in
    // TSQLDBDatasetStatement.ExecutePrepared, native e.g. for FireDAC)
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    {$ifndef UNICODE}
    /// set to true to force all Int64 content to be processed as a truncated float
    // - by default, Int64 values will be bound either as an integer (if the
    // value is within expected range), either as Int64 variant
    // - on some versions of Delphi, and some version of TDataSet (e.g. BDE),
    // you may have to use a conversion to double to avoid a runtime error
    property ForceInt64AsFloat: boolean read fForceInt64AsFloat write fForceInt64AsFloat;
    {$endif}
    /// set to true to force all text content to be processed as WideString
    // instead of the default faster AnsiString, for pre-Unicode version of Delphi
    // - by default, UTF-8 text parameter or column will use an AnsiString value:
    // for pre-Unicode Delphi, avoiding WideString/OleStr content
    // will speed up the process a lot, if you are sure that the current
    // charset matches the expected one (which is very likely)
    // - set this property to TRUE so that WideString will be used when working
    // with the internal TDataSet, to avoid any character data loss:
    // the access to the property will be slower, but you won't have any
    // potential data loss
    // - if the text value contains only ASCII 7 bit characters, it won't be
    // converted to WideString (since it is not necessary)
    // - starting with Delphi 2009, the TEXT content will be processed as an
    // UnicodeString, so this property is not necessary for most cases,
    // but it appeared that some providers expects it to be defined
    property ForceUseWideString: boolean read fForceUseWideString write fForceUseWideString;
  end;

  ///	implements an abstract statement via the DB.pas TDataSet/TQuery-like
  // connection
  // - dedicated abstract class, able to use any TDataSet with any kind of
  // parameter linking (e.g. FireDAC/AnyDAC do have its own parameters type)
  TSQLDBDatasetStatementAbstract = class(TSQLDBStatementWithParamsAndColumns)
  protected
    fQuery: TDataSet;
    fPrepared: boolean;
    fDatasetSupportBatchBinding: boolean;
    fPreparedParamsCount: integer;
    fForceUseWideString: boolean;
  protected
    /// convert SQLDBParamType to a standard DB.TParamType to be used in TQuery.Param
    function SQLParamTypeToDBParamType(IO: TSQLDBParamInOutType): TParamType; virtual;
    /// convert DB.TFieldType into mORMot fieldtype
    function ColumnTypeNativeToDB(aNativeType: TFieldType): TSQLDBFieldType; virtual;
    /// retrieve a given column
    function DatasetField(col: Integer): TField; virtual;
  protected // inherited classes shall override those abstract virtual methods
    /// should initialize and set fQuery internal field as expected
    procedure DatasetCreate; virtual; abstract;
    /// should set the internal fQueryParams protected field
    function DatasetPrepare(const aSQL: string): boolean; virtual; abstract;
    /// execute underlying TQuery.ExecSQL
    procedure DatasetExecSQL; virtual; abstract;
    /// bind SQLDBParam to TQuery-like param
    // - aArrayIndex is >= 0 if array index should be used (in this case,
    // fDatasetSupportBatchBinding=false)
    // - if fDatasetSupportBatchBinding=true, should use array DML binding
    // - SQL Parameter to bind is aParam
    procedure DataSetBindSQLParam(const aArrayIndex, aParamIndex: integer;
      const aParam: TSQLDBParam); virtual; abstract;
    /// set the returned parameter after a stored proc execution
    procedure DataSetOutSQLParam(const aParamIndex: integer;
      var aParam: TSQLDBParam); virtual; abstract;
  public
    /// create a statement instance
    constructor Create(aConnection: TSQLDBConnection); override;
    /// release the prepared statement
    destructor Destroy; override;

    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESQLDBDataset on any error
    procedure Prepare(const aSQL: RawUTF8; ExpectResults: boolean = false); overload; override;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - this implementation will also loop through all internal bound array
    // of values (if any), to implement BATCH mode even if the database library
    // does not support array binding (only SynDBFireDAC does support it yet)
    // - this overridden method will log the SQL statement if sllSQL has been
    // enabled in SynDBLog.Family.Level
    // - raise an ESQLDBDataset on any error
    procedure ExecutePrepared; override;
    /// Reset the previous prepared statement
    // - this overridden implementation will reset all bindings and the cursor state
    // - raise an ESQLDBDataset on any error
    procedure Reset; override;

    /// access the next or first row of data from the SQL Statement result
    // - return true on success, with data ready to be retrieved by Column*() methods
    // - return false if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - if SeekFirst is TRUE, will put the cursor on the first row of results
    // - raise an ESQLDBDataset on any error
    function Step(SeekFirst: boolean = false): boolean; override;
    /// close the associated TQuery when ISQLDBStatement is back in cache
    procedure ReleaseRows; override;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: Integer): Int64; override;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: Integer): boolean; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: Integer): double; override;
    /// return a Column date and time value of the current Row, first Col is 0
    function ColumnDateTime(Col: Integer): TDateTime; override;
    /// return a Column currency value of the current Row, first Col is 0
    function ColumnCurrency(Col: Integer): currency; override;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUTF8(Col: Integer): RawUTF8; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    function ColumnBlob(Col: Integer): RawByteString; override;
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
    // format and contains true BLOB data
    procedure ColumnsToJSON(WR: TJSONWriter); override;
  end;

  ///	implements a statement via the DB.pas TDataSet/TQuery-like connection
  // - you should not use this abstract class directly, but one inherited
  // implementation with overridden Dataset*() protected methods to handle the
  // internal fQuery: TDataSet property
  TSQLDBDatasetStatement = class(TSQLDBDatasetStatementAbstract)
  protected
    fQueryParams: TParams;
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
    // - raise an ESQLDBDataset on any error
    procedure Prepare(const aSQL: RawUTF8; ExpectResults: boolean = false); overload; override;
  end;


implementation


const
  IsTLargeIntField = 1;
  IsTWideStringField = 2;


{ TSQLDBDatasetConnectionProperties }

constructor TSQLDBDatasetConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  inherited Create(aServerName,aDatabaseName,aUserID,aPassWord);
  fBatchSendingAbilities := [cCreate,cUpdate,cDelete]; // always emulated
end;

{ TSQLDBDatasetStatementAbstract }

function TSQLDBDatasetStatementAbstract.ColumnBlob(Col: Integer): RawByteString;
var Str: TStream;
begin
  result := '';
  CheckCol(Col);
  with fColumns[Col] do
    if TField(ColumnAttr).IsNull then
      exit else
    if TField(ColumnAttr).IsBlob then begin
      Str := TField(ColumnAttr).DataSet.CreateBlobStream(TField(ColumnAttr),bmRead);
      try
        if Str.Size>0 then begin
          SetLength(result,Str.Size);
          Str.Read(pointer(result)^,Str.Size);
        end;
      finally
        Str.Free;
      end;
    end else begin
      SetLength(result,TField(ColumnAttr).DataSize);
      TField(ColumnAttr).GetData(TValueBuffer(result));
    end;
end;

function TSQLDBDatasetStatementAbstract.ColumnCurrency(Col: Integer): currency;
begin
  CheckCol(Col);
  with fColumns[Col] do
    if TField(ColumnAttr).IsNull then
      result := 0 else
      if TField(ColumnAttr).DataType in [ftBCD,ftFMTBcd] then
        BCDToCurr(TField(ColumnAttr).AsBCD,result) else
        result := TField(ColumnAttr).AsCurrency;
end;

function TSQLDBDatasetStatementAbstract.ColumnDateTime(Col: Integer): TDateTime;
begin
  CheckCol(Col);
  with fColumns[Col] do
    if TField(ColumnAttr).IsNull then
      result := 0 else
      result := TField(ColumnAttr).AsDateTime;
end;

function TSQLDBDatasetStatementAbstract.ColumnDouble(Col: Integer): double;
begin
  CheckCol(Col);
  with fColumns[Col] do
    if TField(ColumnAttr).IsNull then
      result := 0 else
      result := TField(ColumnAttr).AsFloat;
end;

function TSQLDBDatasetStatementAbstract.ColumnInt(Col: Integer): Int64;
begin
  CheckCol(Col);
  with fColumns[Col] do
    if TField(ColumnAttr).IsNull then
      result := 0 else
    if TField(ColumnAttr).DataType=ftBoolean then
      result := ord(TField(ColumnAttr).AsBoolean) else
      {$ifdef UNICODE}
      result := TField(ColumnAttr).AsLargeInt;
      {$else}
      if ColumnValueDBType=IsTLargeIntField then
        result := TLargeintField(ColumnAttr).AsLargeInt else
        result := TField(ColumnAttr).AsInteger;
      {$endif}
end;

function TSQLDBDatasetStatementAbstract.ColumnNull(Col: Integer): boolean;
begin
  CheckCol(Col);
  result := TField(fColumns[Col].ColumnAttr).IsNull;
end;

function TSQLDBDatasetStatementAbstract.ColumnUTF8(Col: Integer): RawUTF8;
begin
  CheckCol(Col);
  with fColumns[Col] do
    if TField(ColumnAttr).IsNull then
      result := '' else
    {$ifndef UNICODE}
    if ColumnValueDBType=IsTWideStringField then
      result := WideStringToUTF8(TWideStringField(ColumnAttr).Value) else
    {$endif}
      result := StringToUTF8(TField(ColumnAttr).AsString);
end;

constructor TSQLDBDatasetStatementAbstract.Create(aConnection: TSQLDBConnection);
begin
  fForceUseWideString :=
    (aConnection.Properties as TSQLDBDatasetConnectionProperties).ForceUseWideString;
  inherited Create(aConnection);
  try
    DatasetCreate;
  except
    FreeAndNil(fQuery);
    raise;
  end;
end;

destructor TSQLDBDatasetStatementAbstract.Destroy;
begin
  FreeAndNil(fQuery);
  inherited;
end;

procedure TSQLDBDatasetStatementAbstract.Prepare(const aSQL: RawUTF8; ExpectResults: boolean);
var oSQL: RawUTF8;
begin
  SQLLogBegin(sllDB);
  if fPrepared then
    raise ESQLDBDataset.CreateUTF8('%.Prepare() shall be called once',[self]);
  inherited Prepare(aSQL,ExpectResults); // connect if necessary
  fPreparedParamsCount := ReplaceParamsByNames(aSQL,oSQL);
  fPrepared := DatasetPrepare(UTF8ToString(oSQL));
  SQLLogEnd;
  if not fPrepared then
    raise ESQLDBDataset.CreateUTF8('%.DatasetPrepare not prepared',[self]);
end;

procedure TSQLDBDatasetStatementAbstract.ExecutePrepared;
var i,p: Integer;
    lArrayIndex: integer;
    Field: TField;
begin
  SQLLogBegin(sllSQL);
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  // 1. bind parameters in fParams[] to fQuery.Params
  if fPreparedParamsCount<>fParamCount then
    raise ESQLDBDataset.CreateUTF8(
      '%.ExecutePrepared expected % bound parameters, got %',
      [self,fPreparedParamsCount,fParamCount]);
  lArrayIndex := -1; // either Bind() or BindArray() with no Array DML support
  repeat
    if (not fDatasetSupportBatchBinding) and (fParamsArrayCount>0) then
      inc(lArrayIndex); // enable BindArray() emulation
    for p := 0 to fParamCount-1 do
      DatasetBindSQLParam(lArrayIndex,p,fParams[p]);
    // 2. Execute query (within a loop for BATCH mode)
    if fExpectResults then begin
      fQuery.Open;
      fCurrentRow := -1;
      fColumnCount := 0;
      fColumn.ReHash;
      fColumn.Capacity := fQuery.FieldCount;
      for i := 0 to fQuery.FieldCount-1 do begin
        Field := DatasetField(i);
        with PSQLDBColumnProperty(fColumn.AddAndMakeUniqueName(
           StringToUTF8(Field.FieldName)))^ do begin
          ColumnAttr := PtrUInt(Field);
          ColumnType := ColumnTypeNativeToDB(Field.DataType);
          if Field.InheritsFrom(TLargeintField) then
            ColumnValueDBType := IsTLargeIntField else
          if Field.InheritsFrom(TWideStringField) then
            ColumnValueDBType := IsTWideStringField else
            ColumnValueDBType := 0;
        end;
      end;
    end else
      DatasetExecSQL;
  until fDatasetSupportBatchBinding or (lArrayIndex=fParamsArrayCount-1);
  // 3. handle out parameters
  if fParamCount>0 then
    if fParamsArrayCount>0 then
      for p := 0 to fParamCount-1 do
        fParams[p].VData := '' else
      // single statement mode -> return any stored procedure parameter
      for p := 0 to fParamCount-1 do
        if fParams[p].VInOut<>paramIn then
          DataSetOutSQLParam(p,fParams[p]);
  SQLLogEnd;
end;

function TSQLDBDatasetStatementAbstract.Step(SeekFirst: boolean): boolean;
begin
  if SeekFirst then begin
    fQuery.First;
    fCurrentRow := 1;
  end else
  if fCurrentRow>0 then begin
    fQuery.Next;
    inc(fCurrentRow);
  end else
    fCurrentRow := 1;
  result := not fQuery.Eof;
end;

procedure TSQLDBDatasetStatementAbstract.Reset;
begin
  ReleaseRows;
  inherited Reset;
end;

procedure TSQLDBDatasetStatementAbstract.ReleaseRows;
begin
  if (fQuery<>nil) and fQuery.Active then
    fQuery.Close;
  inherited ReleaseRows;
end;

function TSQLDBDatasetStatementAbstract.SQLParamTypeToDBParamType(IO: TSQLDBParamInOutType): TParamType;
begin
  case IO of
    paramIn:    result := ptInput;
    paramOut:   result := ptOutput;
    paramInOut: result := ptInputOutput;
      else      result := ptUnknown;
  end;
end;

function TSQLDBDatasetStatementAbstract.ColumnTypeNativeToDB(aNativeType: TFieldType): TSQLDBFieldType;
begin
  case aNativeType of
    {$ifdef UNICODE}
    ftLongWord,ftShortint,ftByte,
    {$endif}
    ftAutoInc,ftBoolean, ftSmallint,ftInteger,ftLargeint,ftWord:
      result := SynTable.ftInt64;
    {$ifdef UNICODE}
    ftSingle,ftExtended,
    {$endif}
    ftFloat:
      result := SynTable.ftDouble;
    ftCurrency, ftBCD, ftFMTBcd:
      result := SynTable.ftCurrency;
    {$ifdef UNICODE}
    ftOraTimeStamp,ftOraInterval,
    {$endif}
    ftDate,ftTime,ftDateTime,ftTimeStamp:
      result := SynTable.ftDate;
    ftBytes,ftVarBytes,ftBlob,ftGraphic,ftOraBlob:
      result := SynTable.ftBlob;
    {$ifdef UNICODE}
    ftFixedWideChar,ftWideMemo,
    {$endif}
    ftString,ftFixedChar,ftWideString,ftMemo,ftFmtMemo,ftOraClob,ftVariant,ftGuid:
      result := SynTable.ftUTF8;
  else // will use TEXT for other fields (any feedback is welcome!)
      result := SynTable.ftUTF8;
  end;
end;

function TSQLDBDatasetStatementAbstract.DatasetField(col: Integer): TField;
begin
  result := fQuery.Fields[col];
end;

procedure TSQLDBDatasetStatementAbstract.ColumnsToJSON(WR: TJSONWriter);
var col: integer;
    blob: RawByteString;
begin
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount-1 do
  with fColumns[col] do begin
    if WR.Expand then
      WR.AddFieldName(ColumnName); // add '"ColumnName":'
    if TField(ColumnAttr).IsNull then
      WR.AddShort('null') else
    case ColumnType of
      SynTable.ftNull:
        WR.AddShort('null');
      SynTable.ftInt64:
        if TField(ColumnAttr).DataType=ftBoolean then
          WR.Add(ord(TField(ColumnAttr).AsBoolean)) else
          {$ifdef UNICODE}
          WR.Add(TField(ColumnAttr).AsLargeInt);
          {$else}
          if ColumnValueDBType=IsTLargeIntField then
            WR.Add(TLargeintField(ColumnAttr).AsLargeInt) else
            WR.Add(TField(ColumnAttr).AsInteger);
          {$endif}
      SynTable.ftDouble:
        WR.AddDouble(TField(ColumnAttr).AsFloat);
      SynTable.ftCurrency:
        if TField(ColumnAttr).DataType in [ftBCD,ftFMTBcd] then
          AddBcd(WR,TField(ColumnAttr).AsBCD) else
          WR.AddCurr64(TField(ColumnAttr).AsCurrency);
      SynTable.ftDate: begin
        WR.Add('"');
        WR.AddDateTime(TField(ColumnAttr).AsDateTime,fForceDateWithMS);
        WR.Add('"');
      end;
      SynTable.ftUTF8: begin
        WR.Add('"');
        {$ifndef UNICODE}
        if ColumnValueDBType=IsTWideStringField then
          WR.AddJSONEscapeW(Pointer(TWideStringField(ColumnAttr).Value)) else
        {$endif}
          WR.AddJSONEscapeString(TField(ColumnAttr).AsString);
        WR.Add('"');
      end;
      SynTable.ftBlob:
        if fForceBlobAsNull then
          WR.AddShort('null') else begin
          blob := ColumnBlob(col);
          WR.WrBase64(pointer(blob),length(blob),true); // withMagic=true
        end;
      else raise ESQLDBException.CreateUTF8('%: Invalid ColumnType()=%',
            [self,ord(ColumnType)]);
    end;
    WR.Add(',');
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;


{ TSQLDBDatasetStatement }

procedure TSQLDBDatasetStatement.DataSetBindSQLParam(const aArrayIndex,
  aParamIndex: integer; const aParam: TSQLDBParam);
var P: TParam;
    I64: Int64;
    tmp: RawUTF8;
begin
  P := fQueryParams[aParamIndex];
  with aParam do begin
    P.ParamType := SQLParamTypeToDBParamType(VInOut);
    if VinOut <> paramInOut then
      case VType of
        SynTable.ftNull: begin
          P.Clear;
          {$ifdef UNICODE}
          P.AsBlob := nil; // avoid type errors when a blob field is adressed
          {$else}
          P.AsString := '';
          {$endif}
        end;
        SynTable.ftInt64: begin
          if aArrayIndex>=0 then
            I64 := GetInt64(pointer(VArray[aArrayIndex])) else
            I64 := VInt64;
          {$ifdef UNICODE}
          P.AsLargeInt := I64;
          {$else}
          if (PInt64Rec(@I64)^.Hi=0) or (PInt64Rec(@I64)^.Hi=Cardinal(-1)) then
            P.AsInteger := I64 else
            if TSQLDBDatasetConnectionProperties(Connection.Properties).
               fForceInt64AsFloat then
              P.AsFloat := I64 else
              P.Value := I64;
          {$endif}
        end;
        SynTable.ftDouble:
          if aArrayIndex>=0 then
            P.AsFloat := GetExtended(pointer(VArray[aArrayIndex])) else
            P.AsFloat := unaligned(PDouble(@VInt64)^);
        SynTable.ftCurrency:
          if aArrayIndex>=0 then
            P.AsCurrency := StrToCurrency(pointer(VArray[aArrayIndex])) else
            P.AsCurrency := PCurrency(@VInt64)^;
        SynTable.ftDate:
          if aArrayIndex>=0 then begin
            UnQuoteSQLStringVar(pointer(VArray[aArrayIndex]),tmp);
            P.AsDateTime := Iso8601ToDateTime(tmp);
          end else
            P.AsDateTime := PDateTime(@VInt64)^;
        SynTable.ftUTF8:
          if aArrayIndex>=0 then
            if (VArray[aArrayIndex]='') and
               fConnection.Properties.StoreVoidStringAsNull then
              P.Clear else begin
            UnQuoteSQLStringVar(pointer(VArray[aArrayIndex]),tmp);
            if fForceUseWideString then
              P.Value := UTF8ToWideString(tmp) else
              P.AsString := UTF8ToString(tmp);
          end else
            if (VData='') and fConnection.Properties.StoreVoidStringAsNull then
              P.Clear else
            if fForceUseWideString then
              P.Value := UTF8ToWideString(VData) else
              P.AsString := UTF8ToString(VData);
        SynTable.ftBlob:
          if aArrayIndex>=0 then
          {$ifdef UNICODE}
            P.SetBlobData(TValueBuffer(VArray[aArrayIndex]),Length(VArray[aArrayIndex])) else
            P.SetBlobData(TValueBuffer(VData),Length(VData));
          {$else}
            P.AsString := VArray[aArrayIndex] else
            P.AsString := VData;
          {$endif}
        else
          raise ESQLDBDataset.CreateFmt(
            '%.DataSetBindSQLParam: Invalid type % on bound parameter #%d',
            [self,ord(VType),aParamIndex+1]);
        end;
  end;
end;

procedure TSQLDBDatasetStatement.DataSetOutSQLParam(
  const aParamIndex: integer; var aParam: TSQLDBParam);
var Par: TParam;
{$ifdef UNICODE}
    tmpBytes: TBytes;
{$endif}
begin
  Par := fQueryParams[aParamIndex];
  case aParam.VType of
    SynTable.ftInt64:
      {$ifdef UNICODE}
      aParam.VInt64 := Par.AsLargeInt;
      {$else}
      aParam.VInt64 := trunc(Par.AsFloat);
      {$endif}
    SynTable.ftDouble:  unaligned(PDouble(@aParam.VInt64)^) := Par.AsFloat;
    SynTable.ftCurrency:PCurrency(@aParam.VInt64)^ := Par.AsCurrency;
    SynTable.ftDate:    PDateTime(@aParam.VInt64)^ := Par.AsDateTime;
    SynTable.ftUTF8:    aParam.VData := StringToUTF8(Par.AsString);
    SynTable.ftBlob: begin
      {$ifdef UNICODE}
      tmpBytes := Par.AsBlob;
      SetString(aParam.VData,PAnsiChar(pointer(tmpBytes)),Length(tmpBytes));
      {$else}
      aParam.VData := Par.AsString;
      {$endif}
    end;
  end;
end;

procedure TSQLDBDatasetStatement.Prepare(const aSQL: RawUTF8;
  ExpectResults: boolean);
begin
  inherited;
  if fPreparedParamsCount<>fQueryParams.Count then
    raise ESQLDBDataset.CreateUTF8('%.Prepare expected % parameters in request, found % - [%]',
      [self,fPreparedParamsCount,fQueryParams.Count,aSQL]);
end;

end.
