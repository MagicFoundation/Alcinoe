/// fill a VCL TClientDataset from SynDB data access
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynDBMidasVCL;

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
  - mingda
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
  {$ifdef ISDELPHIXE2}
  System.SysUtils,
  {$else}
  SysUtils,
  {$endif}
  Classes,
  {$ifndef DELPHI5OROLDER}
  Variants,
  {$ifndef FPC}
  MidasLib,
  {$endif}
  {$endif}
  SynCommons,
  SynDB, SynDBVCL,
  DB,
  {$ifdef FPC}
  BufDataset
  {$else}
  Contnrs,
  DBClient,
  Provider,
  SqlConst
  {$endif};


{$ifdef FPC}
type
  /// FPC's pure pascal in-memory buffer is used instead of TClientDataSet
  TClientDataSet = TBufDataset;

  /// wrapper functions will use FPC's pure pascal in-memory buffer
  TSynDBDataSet = TBufDataset;

{$else FPC}
type
  /// a TClientDataSet which allows to apply updates on a SynDB connection
  // - typical usage may be for instance over a SynDBRemote connection:
  // ! props := TSQLDBWinHTTPConnectionProperties.Create(....);
  // ! ds := TSynDBDataSet.Create(MainForm);
  // ! ds.CommandText := 'select * from people';
  // ! ds.Open;
  // ! // ... use ds as usual, including modifications
  // ! ds.ApplyUpdates(0);
  TSynDBDataSet = class(TCustomClientDataSet)
  protected
    fDataSet: TSynDBSQLDataSet;
    fProvider: TDataSetProvider;
    fIgnoreColumnDataSize: boolean;
    function GetConnection: TSQLDBConnectionProperties; virtual;
    procedure SetConnection(Value: TSQLDBConnectionProperties); virtual;
    // from TDataSet
    procedure OpenCursor(InfoQuery: Boolean); override;
    {$ifdef ISDELPHI2007ANDUP}
    // from IProviderSupport
    function PSGetCommandText: string; override;
    {$endif}
  public
    /// initialize the instance
    constructor Create(AOwner: TComponent); override;
    procedure FetchParams;
    /// initialize the internal TDataSet from a SynDB TSQLDBStatement result set
    // - the supplied TSQLDBStatement can then be freed by the caller, since
    // a private binary copy will be owned by this instance (in fDataSet.Data)
    procedure From(Statement: TSQLDBStatement; MaxRowCount: cardinal=0);
    /// if field sizes should be left unset, allowing further filling with
    // any data length
    // - by default, ColumnDataSize would be computed from the supplied data,
    // unless you set IgnoreColumnDataSize=true to set the value to 0 (and
    // force e.g. SynDBVCL TSynBinaryDataSet.InternalInitFieldDefs define the
    // field as ftDefaultMemo)
    property IgnoreColumnDataSize: boolean read fIgnoreColumnDataSize write fIgnoreColumnDataSize;
  published
    property CommandText;
    property Active;
    property Aggregates;
    property AggregatesActive;
    property AutoCalcFields;
    property Constraints;
    property DisableStringTrim;
    property FileName;
    property Filter;
    property Filtered;
    property FilterOptions;
    property FieldDefs;
    property IndexDefs;
    property IndexFieldNames;
    property IndexName;
    property FetchOnDemand;
    property MasterFields;
    property MasterSource;
    property ObjectView;
    property PacketRecords;
    property Params;
    property ReadOnly;
    property StoreDefs;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property OnReconcileError;
    property BeforeApplyUpdates;
    property AfterApplyUpdates;
    property BeforeGetRecords;
    property AfterGetRecords;
    property BeforeRowRequest;
    property AfterRowRequest;
    property BeforeExecute;
    property AfterExecute;
    property BeforeGetParams;
    property AfterGetParams;
    /// the associated SynDB connection
    property Connection: TSQLDBConnectionProperties read GetConnection write SetConnection;
    /// the associated SynDB TDataSet, used to retrieve and update data
    property DataSet: TSynDBSQLDataSet read fDataSet;
  end;

{$endif FPC}

/// fetch a SynDB TQuery result set into a new VCL TClientDataSet
// - if aMaxRowCount>0, will return up to the specified number of rows
// - current implementation will return a TClientDataSet instance, created from
// the supplied TQuery content
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
// - if you need a read/only TDataSet, you should better not use this function
// but ToDataSet() as defined in SynDBVCL which is much faster and uses
// much less resources
function ToClientDataSet(aOwner: TComponent; aStatement: SynDB.TQuery;
  aMaxRowCount: integer=0): TSynDBDataSet; overload;

{$ifndef FPC}
/// fetch a SynDB TSQLDBStatement result set into a new VCL TClientDataSet
// - if aMaxRowCount>0, will return up to the specified number of rows
// - current implementation will return a TClientDataSet instance, created from
// the supplied TSQLDBStatement content
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
// - if you need a read/only TDataSet, you should better not use this function
// but ToDataSet() function as defined in SynDBVCL which is much faster and uses
// much less resources
function ToClientDataSet(aOwner: TComponent; aStatement: TSQLDBStatement;
  aMaxRowCount: integer=0): TSynDBDataSet; overload;
{$endif FPC}

/// fetch a SynDB ISQLDBRows result set into a new VCL TClientDataSet
// - this overloaded function can use directly a result of the
// TSQLDBConnectionProperties.Execute() method, as such:
// ! ds1.DataSet := ToClientDataSet(self,props.Execute('select * from table',[]));
function ToClientDataSet(aOwner: TComponent; aStatement: ISQLDBRows;
  aMaxRowCount: integer=0): TSynDBDataSet; overload;


type
  /// how ToClientDataSet functions will
  // fill the TClientDataSet instance
  TClientDataSetMode = (cdsNew, cdsAppend, cdsReplace);


/// fetch a SynDB TQuery result into an existing VCL TClientDataSet
// - if aMaxRowCount>0, will return up to the specified number of rows
// - current implementation will fill an existing TClientDataSet instance, from
// the supplied TQuery content
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
function ToClientDataSet(aDataSet: TClientDataSet; aStatement: SynDB.TQuery;
  aMaxRowCount: integer=0; aMode: TClientDataSetMode=cdsReplace; aLogChange: boolean=false): boolean; overload;

/// fetch a SynDB TSQLDBStatement result into an existing VCL TClientDataSet
// - if aMaxRowCount>0, will return up to the specified number of rows
// - current implementation will fill an existing TClientDataSet instance, from
// the supplied TSQLDBStatement content
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
function ToClientDataSet(aDataSet: TClientDataSet; aStatement: TSQLDBStatement;
  aMaxRowCount: integer=0; aMode: TClientDataSetMode=cdsReplace; aLogChange: boolean=false): boolean; overload;


implementation

var
  GlobalDataSetCount: integer;

function ToClientDataSet(aDataSet: TClientDataSet; aStatement: SynDB.TQuery;
  aMaxRowCount: integer; aMode: TClientDataSetMode; aLogChange: boolean): boolean;
begin
  if aStatement=nil then
    result := false else
    result := ToClientDataSet(aDataSet,aStatement.PreparedSQLDBStatement.Instance,aMaxRowCount);
end;

function ToClientDataSet(aOwner: TComponent; aStatement: SynDB.TQuery;
  aMaxRowCount: integer): TSynDBDataSet;
begin
  if aStatement=nil then
    result := nil else
    result := ToClientDataSet(aOwner,aStatement.PreparedSQLDBStatement.Instance,aMaxRowCount);
end;

{$ifndef FPC}
function ToClientDataSet(aOwner: TComponent; aStatement: TSQLDBStatement;
  aMaxRowCount: integer): TSynDBDataSet;
begin
  result := TSynDBDataSet.Create(aOwner);
  try
    result.Name := 'SynDBDS'+IntToStr(GlobalDataSetCount); // unique name
    inc(GlobalDataSetCount);
    if aStatement<>nil then
      result.From(aStatement,aMaxRowCount);
  except
    on Exception do
      FreeAndNil(result);
  end;
end;
{$endif FPC}

function ToClientDataSet(aOwner: TComponent; aStatement: ISQLDBRows;
  aMaxRowCount: integer=0): TSynDBDataSet; overload;
begin
  if aStatement=nil then
    result := nil else
    result := ToClientDataSet(aOwner,aStatement.Instance,aMaxRowCount);
end;

function ToClientDataSet(aDataSet: TClientDataSet; aStatement: TSQLDBStatement;
  aMaxRowCount: integer; aMode: TClientDataSetMode; aLogChange: boolean): boolean; overload;
var Source: TSynBinaryDataSet;
    Columns: array of record
      Field: DB.TField;
      WasReadOnly: boolean;
      OnChange: TFieldNotifyEvent;
    end;
    Previous: record
      Active: Boolean;
      ReadOnly: Boolean;
      LogChanges: Boolean;
      AfterScroll: TDataSetNotifyEvent;
    end;
    row, f: integer;
    SourceStream,DestStream: TStream;
begin
  result := false;
  if (aDataSet=nil) or (aStatement=nil) then
    exit;
  fillchar(Previous,sizeof(Previous),0);
  if aDataSet.Active then begin
    Previous.Active := true;
    {$ifndef FPC}
    Previous.LogChanges := aDataSet.LogChanges;
    {$endif}
    Previous.ReadOnly := aDataSet.ReadOnly;
    Previous.AfterScroll := aDataSet.AfterScroll;
    aDataSet.AfterScroll := nil;
    aDataSet.ReadOnly := false;
    aDataSet.DisableControls;
  end;
  if aMode=cdsReplace then begin
    {$ifndef FPC}
    if Previous.LogChanges then
      aDataSet.LogChanges := false;
    aDataSet.EmptyDataSet;
    {$else}
    aDataSet.MergeChangeLog;
    aDataSet.Close;
    aDataSet.Open;
    {$endif}
  end;
  Source := TSynBinaryDataSet.Create(nil);
  try
    // load all data content into optimized in-memory buffer
    Source.From(aStatement,aMaxRowCount);
    Source.Open;
    // handle columns
    SetLength(Columns,Source.DataAccess.ColumnCount);
    if aMode=cdsNew then begin
      for f := 0 to high(Columns) do
        with Source.Fields[f] do
          aDataSet.FieldDefs.Add(FieldName,DataType,Size);
      aDataSet.CreateDataSet;
      for f := 0 to high(Columns) do
        Columns[f].Field := aDataSet.FieldByName(Source.Fields[f].FieldName);
    end else
      for f := 0 to high(Columns) do
      with Columns[f] do begin
        Field := aDataSet.FieldByName(Source.Fields[f].FieldName);
        if Field.ReadOnly then begin
          WasReadOnly := true;
          Field.ReadOnly := false;
        end;
        OnChange := Field.OnChange;
        Field.OnChange := nil;
      end;
    // append data
    {$ifndef FPC}
    aDataSet.LogChanges := aLogChange;
    {$endif}
    for row := 0 to Source.DataAccess.DataRowCount-1 do begin
      Source.DataAccess.GotoRow(row,true);
      aDataSet.Append;
      for f := 0 to high(Columns) do
        with Columns[f] do
        if Field<>nil then
        if Source.DataAccess.ColumnNull(f) then
          Field.Clear else
        case Field.DataType of
        ftBoolean:
          Field.AsBoolean := Source.DataAccess.ColumnInt(f)<>0;
        ftSmallint, ftInteger, ftWord:
          Field.AsInteger := Source.DataAccess.ColumnInt(f);
        ftLargeint:
          TLargeintField(Field).Value := Source.DataAccess.ColumnInt(f);
        ftFloat, DB.ftCurrency:
          Field.AsFloat := Source.DataAccess.ColumnDouble(f);
        DB.ftDate,ftDateTime,ftTime:
          Field.AsDateTime := Source.DataAccess.ColumnDateTime(f);
        ftString:
          Field.AsString := Source.DataAccess.ColumnString(f);
        ftWideString:
          TWideStringField(Field).Value := UTF8ToSynUnicode(Source.DataAccess.ColumnUTF8(f));
        {$ifdef ISDELPHI2007ANDUP}
        ftWideMemo,
        {$endif}
        ftMemo, DB.ftBlob: begin
          SourceStream := Source.GetBlobStream(Source.Fields[f],row);
          if SourceStream=nil then
            Field.Clear else
            try
              DestStream := aDataSet.CreateBlobStream(Field,bmWrite);
              try
                DestStream.CopyFrom(SourceStream,0);
              finally
                DestStream.Free;
              end;
            finally
              SourceStream.Free;
            end;
        end;
        else raise EDatabaseError.CreateFmt('Invalid Source.ColumnType for %s)',
          [Field.FieldName]);
        end;
      aDataSet.Post;
    end;
    aDataSet.First;
    result := true;
  finally
    Source.Free;
    if Previous.Active then begin
      {$ifndef FPC}
      aDataSet.LogChanges := Previous.LogChanges;
      {$endif}
      aDataSet.ReadOnly := Previous.ReadOnly;
      aDataSet.AfterScroll := Previous.AfterScroll;
      if Assigned(Previous.AfterScroll) then
        Previous.AfterScroll(aDataSet);
      aDataSet.EnableControls;
    end;
    if aMode<>cdsNew then begin
      for f := 0 to high(Columns) do
        with Columns[f] do
        if Field<>nil then begin
          Field.ReadOnly := WasReadOnly;
          Field.OnChange := OnChange;
        end;
    end;
  end;
end;


{$ifndef FPC}

{ TSynDBDataSet }

constructor TSynDBDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fProvider := TDataSetProvider.Create(Self);
  fProvider.Name := 'InternalProvider';                 { Do not localize }
  fProvider.SetSubComponent(True);
  fProvider.Options := fProvider.Options+[poAllowCommandText];
  SetProvider(fProvider);
  fDataSet := TSynDBSQLDataSet.Create(Self);
  fDataSet.Name := 'InternalDataSet';                   { Do not localize }
  fDataSet.SetSubComponent(True);
  fProvider.DataSet := fDataSet;
end;

procedure TSynDBDataSet.From(Statement: TSQLDBStatement; MaxRowCount: cardinal);
begin
  fDataSet.From(Statement,MaxRowCount,fIgnoreColumnDataSize);
  fDataSet.CommandText := ''; // ensure no SQL execution
  Open;
  fDataSet.CommandText := UTF8ToString(Statement.SQL); // assign it AFTER Open
end;

procedure TSynDBDataSet.FetchParams;
begin
  if not HasAppServer and Assigned(FProvider) then
    SetProvider(FProvider);
  inherited FetchParams;
end;

procedure TSynDBDataSet.OpenCursor(InfoQuery: Boolean);
begin
  if Assigned(fProvider) then
    SetProvider(fProvider);
  if fProvider.DataSet=self then
    raise ESQLDBException.Create(SCircularProvider);
  inherited OpenCursor(InfoQuery);
end;

{$ifdef ISDELPHI2007ANDUP}
function TSynDBDataSet.PSGetCommandText: string;
{$ifdef ISDELPHIXE3}
var IP: IProviderSupportNG;
begin
  if Supports(fDataSet, IProviderSupportNG, IP) then
{$else}
var IP: IProviderSupport;
begin
  if Supports(fDataSet, IProviderSupport, IP) then
{$endif}
    result := IP.PSGetCommandText else
    result := CommandText;
end;
{$endif ISDELPHI2007ANDUP}

function TSynDBDataSet.GetConnection: TSQLDBConnectionProperties;
begin
  result := fDataSet.Connection;
end;

procedure TSynDBDataSet.SetConnection(Value: TSQLDBConnectionProperties);
begin
  fDataSet.Connection := Value;
end;

{$endif FPC}

end.


