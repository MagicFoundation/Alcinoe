/// fill a VCL TClientDataset from SynRestVCL data access
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynRestMidasVCL;

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
  - Esteban Martin
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
    which is an extraction from former SynRestVCL.pas unit (which is faster
    but read/only)
  - introducing TSynRestDataSet (under Delphi), which allows to apply updates:
    will be used now for overloaded ToClientDataSet() functions result
  - fixed Delphi XE2 compilation issue with SetCommandText declaration
  - bug fix skipping first record
  - fix memory leak adding TSynRestDataset.destroy (by houdw2006)

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
  SynCommons,
  SynTable,
  SynDB,
  SynRestVCL,
  DB,
  {$ifdef FPC}
  BufDataset
  {$else}
  Contnrs,
  DBClient,
  Provider,
  SqlConst
  {$endif};


{$ifdef FPC} { TODO: duplicated code from SynDBMidasVCL }
type
  /// FPC's pure pascal in-memory buffer is used instead of TClientDataSet
  TClientDataSet = TBufDataset;

  /// wrapper functions will use FPC's pure pascal in-memory buffer
  TSynRestDataSet = TBufDataset;

{$else FPC}
type
  /// A TSynRestDataset, inherited from TCustomClientDataSet, which allows to apply updates on a TWinHTTP connection.
  //  The TSQLModel is required for getting column datatype and size and if the TSQLRecord has defined
  //  InternalDefineModel for validations they will be associated to a TField.OnValidate. Similary if the method
  //  ComputeBeforeWriteFields is overridden this will be used.
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
  TSynRestDataSet = class(TCustomClientDataSet)
  protected
    fDataSet: TSynRestSQLDataset;
    fProvider: TDataSetProvider;
    procedure DoOnFieldValidate(Sender: TField);
    procedure DoOnUpdateError(Sender: TObject; DataSet: TCustomClientDataSet; E: EUpdateError; UpdateKind: TUpdateKind; var Response: TResolverResponse);
    // from TDataSet
    procedure OpenCursor(InfoQuery: Boolean); override;
    {$ifdef ISDELPHI2007ANDUP}
    // from IProviderSupport
    function PSGetCommandText: string; override;
    {$endif}
    {$IFNDEF NEXTGEN}
    {$ifdef ISDELPHIXE}
    procedure SetCommandText(Value: WideString); override;
    {$else ISDELPHIXE}
    procedure SetCommandText(Value: String); override;
    {$endif ISDELPHIXE}
    {$ELSE}
    procedure SetCommandText(Value: String); override;
    {$ENDIF !NEXTGEN}
    procedure SetFieldValidateFromSQLRecordSynValidate;
  public
    /// initialize the instance
    constructor Create(AOwner: TComponent); override;
    /// destroy the instance
    destructor Destroy; override;
    /// initialize the internal TDataSet from a Rest statement result set
    // - Statement will have the form http://host:port/root/tablename or
    //   http://host:port/root/servicename.operationname?paramname=:paramalias
    // examples:
    //   http://host:port/root/tablename?select=XXX or
    //   http://host:port/root/tablename?select=XXX&where=field1=XXX or field2=XXX
    //   http://host:port/root/service.operation?param=:param
    procedure From(Statement: RawUTF8; MaxRowCount: cardinal=0);
    procedure FetchParams;
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
    /// the associated SynRestVCL TDataSet, used to retrieve and update data
    property DataSet: TSynRestSQLDataSet read fDataSet;
  end;

{$endif FPC}

/// Convert JSON array to REST TClientDataset
// - the dataset is created inside this function
function JSONToSynRestDataset(const aJSON: RawUTF8; const aSQLModel: TSQLModel): TSynRestDataset;

implementation

uses
  Dialogs;

type
  TSynRestSQLDatasetHack = class(TSynRestSQLDataset);
  TSynValidateRestHack = class(TSynValidateRest);

{$ifndef FPC}

function JSONToSynRestDataset(const aJSON: RawUTF8; const aSQLModel: TSQLModel): TSynRestDataset;
var
  lSQLTableJSON: TSQLTableJSON;
  lData: TRawByteStringStream;
begin
  Result := Nil;
  if (aJSON = '') then
    Exit;
  lSQLTableJSON := TSQLTableJSON.Create('', aJSON);
  lData := TRawByteStringStream.Create('');
  try
    JSONToBinary(lSQLTableJSON, lData);
    Result := TSynRestDataset.Create(Nil);
    Result.Dataset.SQLModel := aSQLModel;
    Result.DataSet.From(lData.DataString);
  finally
    FreeAndNil(lData);
    FreeAndNil(lSQLTableJSON);
  end;
end;

{ TSynRestDataSet }

constructor TSynRestDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fProvider := TDataSetProvider.Create(Self);
  fProvider.Name := 'InternalProvider';                 { Do not localize }
  fProvider.SetSubComponent(True);
  fProvider.Options := fProvider.Options+[poAllowCommandText];
  fProvider.OnUpdateError := DoOnUpdateError;
  SetProvider(fProvider);
  fDataSet := TSynRestSQLDataSet.Create(Self);
  fDataSet.Name := 'InternalDataSet';                   { Do not localize }
  fDataSet.SetSubComponent(True);
  fProvider.DataSet := fDataSet;
end;

destructor TSynRestDataSet.Destroy;
begin
  fProvider.DataSet := nil;
  FreeAndNil(fDataSet);
  FreeAndNil(fProvider);
  inherited;
end;

procedure TSynRestDataSet.DoOnFieldValidate(Sender: TField);
var
  lRec: TSQLRecord;
  F: Integer; // fields
  V: Integer; // validations
  Validate: TSynValidate;
  Value: RawUTF8;
  lErrMsg: string;
  lFields: TSQLPropInfoList;
  lwasTSynValidateRest: boolean;
  ValidateRest: TSynValidateRest absolute Validate;
begin
  lRec := TSynRestSQLDatasetHack(fDataset).GetSQLRecordClass.Create;
  try
    lFields := lRec.RecordProps.Fields;
    F := lFields.IndexByName(Sender.FieldName);
    // the field has not validation
    if (Length(lRec.RecordProps.Filters[F]) = 0) then
      Exit;

    if not (lFields.List[F].SQLFieldType in COPIABLE_FIELDS) then
      Exit;

    lRec.SetFieldValue(Sender.FieldName, PUTF8Char(VariantToUTF8(Sender.Value)));
    for V := 0 to Length(lRec.RecordProps.Filters[F])-1 do begin
      Validate := TSynValidate(lRec.RecordProps.Filters[F,V]);
      if Validate.InheritsFrom(TSynValidate) then begin
        Value := Sender.Value;
        lwasTSynValidateRest := Validate.InheritsFrom(TSynValidateRest);
        if lwasTSynValidateRest then begin // set additional parameters
            TSynValidateRestHack(ValidateRest).fProcessRec := lRec;
            TSynValidateRestHack(ValidateRest).fProcessRest := Nil; // no Rest for the moment
          end;
          try
            if not Validate.Process(F,Value,lErrMsg) then begin
              if lErrMsg='' then
                // no custom message -> show a default message
                lErrMsg := format(sValidationFailed,[GetCaptionFromClass(Validate.ClassType)])
              else
                raise ESQLRestException.CreateUTF8('Error % on field "%"', [lErrMsg, Sender.DisplayName]);
            end;
          finally
            if lwasTSynValidateRest then begin // reset additional parameters
              TSynValidateRestHack(ValidateRest).fProcessRec := nil;
              TSynValidateRestHack(ValidateRest).fProcessRest := nil;
            end;
          end;
        end;
    end;
  finally
    lRec.Free;
  end;
end;

procedure TSynRestDataSet.DoOnUpdateError(Sender: TObject; DataSet: TCustomClientDataSet; E: EUpdateError;
                                             UpdateKind: TUpdateKind; var Response: TResolverResponse);
begin
  Response := rrAbort;
  MessageDlg(E.OriginalException.Message, mtError, [mbOK], 0);
end;

procedure TSynRestDataSet.From(Statement: RawUTF8; MaxRowCount: cardinal);
begin
  fDataSet.From(Statement);
  fDataSet.CommandText := ''; // ensure no SQL execution
  Open;
  fDataSet.CommandText := UTF8ToString(Statement); // assign it AFTER Open
end;

procedure TSynRestDataSet.FetchParams;
begin
  if not HasAppServer and Assigned(FProvider) then
    SetProvider(FProvider);
  inherited FetchParams;
end;

procedure TSynRestDataSet.OpenCursor(InfoQuery: Boolean);
begin
  if Assigned(fProvider) then
    SetProvider(fProvider);
  if fProvider.DataSet=self then
    raise ESQLDBException.Create(SCircularProvider);
  inherited OpenCursor(InfoQuery);
  SetFieldValidateFromSQLRecordSynValidate;
end;

{$ifdef ISDELPHI2007ANDUP}
function TSynRestDataSet.PSGetCommandText: string;
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

{$IFNDEF NEXTGEN}
{$ifdef ISDELPHIXE}
procedure TSynRestDataSet.SetCommandText(Value: WideString);
{$else ISDELPHIXE}
procedure TSynRestDataSet.SetCommandText(Value: String);
{$endif ISDELPHIXE}
{$ELSE}
procedure TSynRestDataSet.SetCommandText(Value: String);
{$ENDIF !NEXTGEN}
begin
  TSynRestSQLDatasetHack(fDataset).SetCommandText(Value);
  inherited SetCommandText(fDataset.CommandText);
  // with this TSynRestSQLDataset can bind param values
  TSynRestSQLDatasetHack(fDataset).fParams := Params;
  if (Name = '') then
    Name := 'rds' + StringReplaceChars(TSynRestSQLDatasetHack(fDataset).fTableName, '.', '_');
end;

procedure TSynRestDataSet.SetFieldValidateFromSQLRecordSynValidate;
var
  F: Integer; // dataset fields
  V: Integer; // validation fields
  lProps: TSQLRecordProperties;
begin
  // if not TSQLRecord associated, nothing to do
  if (TSynRestSQLDatasetHack(fDataset).GetTableName = '') then
    Exit;
  lProps := TSynRestSQLDatasetHack(fDataset).GetSQLRecordClass.RecordProps;
  // if there isn't filters, bye
  if (Length(lProps.Filters) = 0) then
    Exit;
  for F := 0 to Fields.Count-1 do
  begin
    V := lProps.Fields.IndexByName(Fields[F].FieldName);
    if (V > -1) then
    begin
      if (Length(lProps.Filters[V]) > 0) then
        Fields[F].OnValidate := DoOnFieldValidate;
    end;
  end;
end;

{$endif FPC}

end.


