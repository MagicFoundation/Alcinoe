/// DB VCL read/only dataset from SynDB data access
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynDBVCL;

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
  - Alfred Glaenzer (alf)
  - Murat Ak

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
  {$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  Classes,
  Contnrs,
  SynCommons,
  SynTable,
  SynDB,
  DB,
  {$ifndef FPC}DBCommon,{$endif}
  SynVirtualDataSet;

type
  /// read-only virtual TDataSet able to access a binary buffer as returned
  // by TSQLStatement.FetchAllToBinary method or directly a TSQLStatement
  TSynBinaryDataSet = class(TSynVirtualDataSet)
  protected
    fData: RawByteString;
    fDataAccess: TSQLDBProxyStatementRandomAccess;
    fTemp64: Int64;
    procedure InternalInitFieldDefs; override;
    function GetRecordCount: Integer; override;
    function GetRowFieldData(Field: TField; RowIndex: integer;
      out ResultLen: Integer; OnlyCheckNull: boolean): Pointer; override;
  public
    /// initialize the virtual TDataSet from a FetchAllToBinary() buffer
    // - by default, ColumnDataSize would be computed from the supplied data,
    // unless you set IgnoreColumnDataSize=true to set the value to 0 (and
    // force e.g. SynDBVCL TSynBinaryDataSet.InternalInitFieldDefs define the
    // field as ftDefaultMemo) or you define some FieldDefs.Items[].Size values
    // for ftUTF8 column sizes before calling this From() method
    procedure From(const BinaryData: RawByteString;
      DataRowPosition: PCardinalDynArray=nil; IgnoreColumnDataSize: boolean=false); overload; virtual;
    /// initialize the virtual TDataSet from a SynDB TSQLDBStatement result set
    // - the supplied ISQLDBRows instance can safely be freed by the caller,
    // since a private binary copy will be owned by this instance (in Data)
    // - by default, ColumnDataSize would be computed from the supplied data,
    // unless you set IgnoreColumnDataSize=true to set the value to 0 (and
    // force e.g. SynDBVCL TSynBinaryDataSet.InternalInitFieldDefs define the
    // field as ftDefaultMemo) or you define some FieldDefs.Items[].Size values
    // for ftUTF8 column sizes before calling this From() method
    procedure From(Statement: TSQLDBStatement; MaxRowCount: cardinal=0;
      IgnoreColumnDataSize: boolean=false); overload; virtual;
    /// finalize the class instance
    destructor Destroy; override;
    /// read-only access to the internal binary buffer
    property Data: RawByteString read fData;
    /// read-only access to the internal SynDB data
    property DataAccess: TSQLDBProxyStatementRandomAccess read fDataAccess;
  end;

  /// TDataSet able to execute any SQL as SynDB's TSQLStatement result set
  // - this class is not meant to be used by itself, but via TSynDBDataSet,
  // defined in SynDBMidasVCL.pas, as a data provider able to apply updates to
  // the remote SynDB connection
  // - typical usage may be for instance over a SynDBRemote connection:
  // ! props := TSQLDBWinHTTPConnectionProperties.Create(....);
  // ! ds := TSynDBSQLDataSet.Create(MainForm);
  // ! ds.CommandText := 'select * from people';
  // ! ds.Open;
  // ! // ... use ds
  // ! ds.Close;
  // ! ds.CommandText := 'select * from customer where id=:(10):';
  // ! ds.Open;
  // ! // ... use ds
  TSynDBSQLDataSet = class(TSynBinaryDataSet)
  protected
    fConnection: TSQLDBConnectionProperties;
    fCommandText: string;
    procedure InternalOpen; override;
    procedure InternalClose; override;
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
  public
    /// initialize the internal TDataSet from a SynDB TSQLDBStatement result set
    // - the supplied TSQLDBStatement can then be freed by the caller, since
    // a private binary copy will be owned by this instance (in fDataSet.Data)
    // - by default, ColumnDataSize would be computed from the supplied data,
    // unless you set IgnoreColumnDataSize=true to set the value to 0 (and
    // force e.g. SynDBVCL TSynBinaryDataSet.InternalInitFieldDefs define the
    // field as ftDefaultMemo) or you define some FieldDefs.Items[].Size values
    // for ftUTF8 column sizes before calling this From() method
    procedure From(Statement: TSQLDBStatement; MaxRowCount: cardinal=0;
      IgnoreColumnDataSize: boolean=false); override;
    /// the associated connection properties
    property Connection: TSQLDBConnectionProperties read fConnection write fConnection;
  published
    /// the SQL statement to be executed
    // - since this statement will be executed via Connection.ExecuteInlined,
    // you can specify optionally inlined parameters to this SQL text
    property CommandText: string read fCommandText write fCommandText;
  end;


/// fetch a SynDB's TQuery result into a VCL DataSet
// - if aMaxRowCount>0, will return up to the specified number of rows
// - current implementation will return a TSynSQLStatementDataSet instance,
// using an optimized internal binary buffer: the supplied TQuery can be released
// - if you need a writable TDataSet, you can use the slower ToClientDataSet()
// function as defined in SynDBMidasVCL.pas
function ToDataSet(aOwner: TComponent; aStatement: SynDB.TQuery;
  aMaxRowCount: integer=0): TSynBinaryDataSet; overload;

/// fetch a SynDB's TSQLDBStatement result into a VCL DataSet
// - just a wrapper around TSynSQLStatementDataSet.Create + Open
// - if aMaxRowCount>0, will return up to the specified number of rows
// - current implementation will return a TSynSQLStatementDataSet instance, using
// an optimized internal binary buffer: the supplied statement can be released
// - if you need a writable TDataSet, you can use the slower ToClientDataSet()
// function as defined in SynDBMidasVCL.pas
function ToDataSet(aOwner: TComponent; aStatement: TSQLDBStatement;
  aMaxRowCount: integer=0): TSynBinaryDataSet; overload;

/// fetch a SynDB ISQLDBRows result set into a VCL DataSet
// - this overloaded function can use directly a result of the
// TSQLDBConnectionProperties.Execute() method, as such:
// ! ds1.DataSet := ToDataSet(self,props.Execute('select * from table',[]));
function ToDataSet(aOwner: TComponent; aStatement: ISQLDBRows;
  aMaxRowCount: integer=0): TSynBinaryDataSet; overload;

/// fetch a SynDB's TSQLDBStatement.FetchAllToBinary buffer into a VCL DataSet
// - just a wrapper around TSynBinaryDataSet.Create + Open
// - if you need a writable TDataSet, you can use the slower ToClientDataSet()
// function as defined in SynDBMidasVCL.pas
function BinaryToDataSet(aOwner: TComponent;
  const aBinaryData: RawByteString): TSynBinaryDataSet;


implementation


function ToDataSet(aOwner: TComponent; aStatement: SynDB.TQuery;
  aMaxRowCount: integer): TSynBinaryDataSet;
begin
  if aStatement=nil then
    result := nil else
    result := ToDataSet(aOwner,
      aStatement.PreparedSQLDBStatement.Instance,aMaxRowCount);
end;

function ToDataSet(aOwner: TComponent; aStatement: TSQLDBStatement;
  aMaxRowCount: integer): TSynBinaryDataSet;
begin
  result := TSynBinaryDataSet.Create(aOwner);
  result.From(aStatement,aMaxRowCount);
  result.Open;
end;

function ToDataSet(aOwner: TComponent; aStatement: ISQLDBRows;
  aMaxRowCount: integer): TSynBinaryDataSet;
begin
  if aStatement=nil then
    result  := nil else
    result := ToDataSet(aOwner,aStatement.Instance,aMaxRowCount);
end;

function BinaryToDataSet(aOwner: TComponent; const aBinaryData: RawByteString): TSynBinaryDataSet;
begin
  result := TSynBinaryDataSet.Create(aOwner);
  result.From(aBinaryData);
  result.Open;
end;


{ TSynBinaryDataSet }

procedure TSynBinaryDataSet.From(const BinaryData: RawByteString;
  DataRowPosition: PCardinalDynArray; IgnoreColumnDataSize: boolean);
begin
  fData := BinaryData;
  fDataAccess := TSQLDBProxyStatementRandomAccess.Create(
    pointer(fData),length(fData),DataRowPosition,IgnoreColumnDataSize);
end;

procedure TSynBinaryDataSet.From(Statement: TSQLDBStatement; MaxRowCount: cardinal;
  IgnoreColumnDataSize: boolean);
var DataStream: TRawByteStringStream;
    DataRowPosition: TCardinalDynArray;
begin
  DataStream := TRawByteStringStream.Create;
  try
    Statement.FetchAllToBinary(DataStream,MaxRowCount,@DataRowPosition);
    From(DataStream.DataString,@DataRowPosition,IgnoreColumnDataSize);
  finally
    DataStream.Free;
  end;
end;

destructor TSynBinaryDataSet.Destroy;
begin
  inherited;
  FreeAndNil(fDataAccess);
end;

function TSynBinaryDataSet.GetRecordCount: Integer;
begin
  if fDataAccess=nil then
    result := 0 else
    result := fDataAccess.DataRowCount;
end;

procedure TSynBinaryDataSet.InternalInitFieldDefs;
var F,custom: integer;
    DBType: TFieldType;
    ExistingName: TRawUTF8DynArray; // FieldDefs.Items[].Name
    ExistingSize: TIntegerDynArray; // FieldDefs.Items[].Size
begin
  if FieldDefs.Count>0 then begin // custom column sizes
    SetLength(ExistingName,FieldDefs.Count);
    SetLength(ExistingSize,FieldDefs.Count);
    for F := 0 to FieldDefs.Count-1 do
      with FieldDefs.Items[F] do begin
        ExistingName[F] := StringToUtf8(Name);
        ExistingSize[F] := Size;
      end;
  end;
  FieldDefs.Clear;
  if fDataAccess=nil then
    exit;
  for F := 0 to fDataAccess.ColumnCount-1 do
    with fDataAccess.Columns[F] do begin
      if ExistingName<>nil then begin
        custom := FindRawUTF8(ExistingName,ColumnName);
        if custom>=0 then // retrieve custom max column length from FieldDefs
          ColumnDataSize := ExistingSize[custom];
      end;
      case ColumnType of
      SynTable.ftInt64:
        DBType := ftLargeint;
      SynTable.ftDate:
        DBType := ftDateTime;
      SynTable.ftUTF8:
        if ColumnDataSize=0 then
          DBType := ftDefaultMemo else // no size
          DBType := ftWideString; // means UnicodeString for Delphi 2009+
      SynTable.ftBlob:
        DBType := ftBlob;
      SynTable.ftDouble, SynTable.ftCurrency:
        DBType := ftFloat;                  
      else
        raise EDatabaseError.CreateFmt(
          'GetFieldData ColumnType=%s',[TSQLDBFieldTypeToString(ColumnType)]);
      end;
      FieldDefs.Add(UTF8ToString(ColumnName),DBType,ColumnDataSize);
    end;
end;

function TSynBinaryDataSet.GetRowFieldData(Field: TField;
  RowIndex: integer; out ResultLen: Integer; OnlyCheckNull: boolean): Pointer;
var F: integer;
begin
  result := nil;
  F := Field.Index;
  if (fDataAccess=nil) or not fDataAccess.GotoRow(RowIndex) then
    exit;
  result := fDataAccess.ColumnData(F);
  if (result<>nil) and not OnlyCheckNull then
    case fDataAccess.Columns[F].ColumnType of
    SynTable.ftInt64: begin
      fTemp64 := FromVarInt64(PByte(result));
      result := @fTemp64;
    end;
    SynTable.ftCurrency: begin // ftFloat expects a DOUBLE value
      unaligned(PDouble(@fTemp64)^) := PCurrency(result)^;
      result := @fTemp64;
    end;
    SynTable.ftUTF8, SynTable.ftBlob:
      resultLen := FromVarUInt32(PByte(result));
    end; // other ColumnTypes are already in the expected format
end;


{ TSynDBSQLDataSet }

procedure TSynDBSQLDataSet.From(Statement: TSQLDBStatement; MaxRowCount: cardinal;
  IgnoreColumnDataSize: boolean);
begin
  inherited From(Statement,MaxRowCount,IgnoreColumnDataSize);
  fConnection := Statement.Connection.Properties;
end;

procedure TSynDBSQLDataSet.InternalClose;
begin
  inherited InternalClose;
  FreeAndNil(fDataAccess);
  fData := '';
end;

procedure TSynDBSQLDataSet.InternalOpen;
var Rows: ISQLDBRows;
begin
  if fCommandText='' then begin
    if fData<>'' then // called e.g. after From() method
      inherited InternalOpen;
    exit;
  end;
  Rows := fConnection.ExecuteInlined(StringToUTF8(fCommandText),true);
  if Rows<>nil then begin
    From(Rows.Instance);
    inherited InternalOpen;
  end;
end;

{$ifdef ISDELPHIXE3}

function TSynDBSQLDataSet.PSExecuteStatement(const ASQL: string;
  AParams: TParams): Integer;
var DS: TDataSet;
begin
  DS := nil;
  result := PSExecuteStatement(ASQL,AParams,DS);
  DS.Free;
end;

function TSynDBSQLDataSet.PSExecuteStatement(const ASQL: string;
  AParams: TParams; var ResultSet: TDataSet): Integer;
{$else}
function TSynDBSQLDataSet.PSExecuteStatement(const ASQL: string;
  AParams: TParams; ResultSet: Pointer): Integer;
{$endif}
var Stmt: ISQLDBStatement;
    blob: TBlobData;
    p: integer;
begin // only execute writes in current implementation
  if fConnection=nil then
    raise ESQLQueryException.CreateUTF8('%.PSExecuteStatement with Connection=nil',[self]);
  Stmt := fConnection.NewThreadSafeStatementPrepared(StringToUTF8(ASQL),false);
  if Stmt<>nil then
    try
      if AParams<>nil then
        for p := 0 to AParams.Count-1 do
          if aParams[p].DataType = ftBlob then begin
            blob := aParams[p].AsBlob;
            Stmt.BindBlob(p+1,pointer(blob),length(blob));
          end else
            Stmt.BindVariant(p+1,AParams[p].Value,False);
      Stmt.ExecutePrepared;
      result := Stmt.UpdateCount;
      if result=0 then
        result := 1; // optimistic result, even if SynDB returned 0
    except
      result := 0;
    end else
      result := 0;
end;

function TSynDBSQLDataSet.PSGetTableName: string;
begin
  // ToDo We miss GetTableNameFromSQL in FPC, Delphi function from DBCommon
  result := {$ifdef FPC}''{$else}GetTableNameFromSQL(fCommandText){$endif};
end;

function TSynDBSQLDataSet.PSIsSQLBased: Boolean;
begin
  result := true;
end;

function TSynDBSQLDataSet.PSIsSQLSupported: Boolean;
begin
  result := true;
end;

procedure TSynDBSQLDataSet.PSSetCommandText(const ACommandText: string);
begin
  inherited;
  fCommandText := ACommandText;
end;

function TSynDBSQLDataSet.PSUpdateRecord(UpdateKind: TUpdateKind;
  Delta: TDataSet): Boolean;
begin
  result := false;
end;

end.
