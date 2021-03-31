/// DB VCL dataset using TSQLTable/TSQLTableJSON data access
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotVCL;

{
    This file is part of Synopse mORmot framework.

    Synopse mORMot framework. Copyright (C) 2021 Arnaud Bouchez
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

  Version 1.17
  - first public release, corresponding to Synopse mORMot Framework 1.17

  Version 1.18
  - renamed SQLite3VCL.pas to mORMotVCL.pas
  - fixed ticket [9de8be5d9e] with some types like TEnumeration or TTimeLog
  - fixed process with Unicode content
  - introduced new aForceWideString optional parameter for ticket [2970335e40]

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  Classes,
  {$ifndef FPC}
  Contnrs,
  {$endif}
  DB,
  SynVirtualDataSet,
  SynCommons, mORMot;


type
  /// read-only virtual TDataSet able to access a TSQLTable
  TSynSQLTableDataSet = class(TSynVirtualDataSet)
  protected
    fTable: TSQLTable;
    {$ifndef UNICODE}
    fForceWideString: boolean;
    {$endif}
    fTableShouldBeFreed: boolean;
    fTemp64: Int64;
    fTempBlob: TSQLRawBlob;
    procedure InternalInitFieldDefs; override;
    function GetRecordCount: Integer; override;
    function GetRowFieldData(Field: TField; RowIndex: integer;
      out ResultLen: Integer; OnlyCheckNull: boolean): Pointer; override;
    function SearchForField(const aLookupFieldName: RawUTF8; const aLookupValue: variant;
      aOptions: TLocateOptions): integer; override;
  public
    /// initialize the virtual TDataSet from a TSQLTable
    // - WARNING: the supplied TSQLTable instance shall remain available
    // all the time the returned TSynSQLTableDataSet instance is used, unless
    // the TableShouldBeFreed property is set to true or CreateOwnedTable()
    // constructor is used instead
    // - with non-Unicode version of Delphi, you can set ForceWideString to
    // force the use of WideString fields instead of AnsiString, if needed
    // - the TDataSet will be opened once created
    constructor Create(Owner: TComponent; Table: TSQLTable
      {$ifndef UNICODE}; ForceWideString: boolean=false{$endif}); reintroduce;
    /// initialize the virtual TDataSet owning a TSQLTable
    // - this constructor will set TableShouldBeFreed to TRUE
    // - with non-Unicode version of Delphi, you can set ForceWideString to
    // force the use of WideString fields instead of AnsiString, if needed
    // - the TDataSet will be opened once created
    constructor CreateOwnedTable(Owner: TComponent; Table: TSQLTable
      {$ifndef UNICODE}; ForceWideString: boolean=false{$endif}); reintroduce;
    /// initialize the virtual TDataSet from a supplied JSON result
    // - this constructor will parse the supplied JSON content and create
    // an internal TSQLTableJSON instance to process the data, guessing the
    // column types from the JSON content
    // - with non-Unicode version of Delphi, you can set ForceWideString to
    // force the use of WideString fields instead of AnsiString, if needed
    // - the TDataSet will be opened once created
    constructor CreateFromJSON(Owner: TComponent; const JSON: RawUTF8
      {$ifndef UNICODE}; ForceWideString: boolean=false{$endif}); reintroduce; overload;
    /// initialize the virtual TDataSet from a supplied JSON result
    // - you can set the expected column types matching the results column layout
    // - this constructor will parse the supplied JSON content and create
    // an internal TSQLTableJSON instance to process the data
    // - with non-Unicode version of Delphi, you can set ForceWideString to
    // force the use of WideString fields instead of AnsiString, if needed
    // - the TDataSet will be opened once created
    constructor CreateFromJSON(Owner: TComponent; const JSON: RawUTF8;
      const ColumnTypes: array of TSQLFieldType
      {$ifndef UNICODE}; ForceWideString: boolean=false{$endif}); reintroduce; overload;
    /// initialize the virtual TDataSet from a supplied JSON ORM result
    // - you can set the TSQLRecord classes to retrieve the expected column types
    // - this constructor will parse the supplied JSON content and create
    // an internal TSQLTableJSON instance to process the data
    // - with non-Unicode version of Delphi, you can set ForceWideString to
    // force the use of WideString fields instead of AnsiString, if needed
    // - the TDataSet will be opened once created
    constructor CreateFromJSON(Owner: TComponent; const JSON: RawUTF8;
      const Tables: array of TSQLRecordClass
      {$ifndef UNICODE}; ForceWideString: boolean=false{$endif}); reintroduce; overload;
    /// finalize the class instance
    destructor Destroy; override;

    /// if the supplied TSQLTable instance should be released with this class
    // - Create() will left to FALSE (meaning that the TSQLTable instance shall
    // remain available all the time the TSynSQLTableDataSet instance is used)
    // - CreateOwnedTable() will set to TRUE if you want the TSQLTable to be
    //  freed when this TSynSQLTableDataSet instance will be released
    // - you can also set it after Create(), on purpose
    property TableShouldBeFreed: boolean read fTableShouldBeFreed write fTableShouldBeFreed;
    /// access to the internal TSQLTable[JSON] data
    // - you can use e.g. the SortFields() methods
    // - you may change the table content on the fly, if the column remains the same
    property Table: TSQLTable read fTable write fTable;
  end;

  /// store low-level DB.pas field information
  // - as used by GetDBFieldDef and GetDBFieldValue
  TDBFieldDef = record
    FieldName: string;
    DBType: TFieldType;
    DBSize: Integer;
    SQLType: TSQLFieldType;
    SQLIndex: integer;
    FieldType: PSQLTableFieldType;
  end;

/// get low-level DB.pas field information
// - ready to be added to a TDataset as:
// !  aDataSet.FieldDefs.Add(FieldName,DBType,DBSize);
procedure GetDBFieldDef(aTable: TSQLTable; aField: integer;
  out DBFieldDef: TDBFieldDef{$ifndef UNICODE}; aForceWideString: boolean=false{$endif});

/// fill a DB.pas field content
// - used e.g. by mORMotMidasVCL.ToClientDataSet
procedure GetDBFieldValue(aTable: TSQLTable; aRow: integer; aField: TField;
  aDataSet: TDataSet; const DBFieldDef: TDBFieldDef);

/// convert a JSON result into a VCL DataSet, guessing the field types from the JSON
// - this function is just a wrapper around TSynSQLTableDataSet.CreateFromJSON()
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - with Unicode version of Delphi (2009+), string/UnicodeString will be used
function JSONToDataSet(aOwner: TComponent; const aJSON: RawUTF8
  {$ifndef UNICODE}; aForceWideString: boolean=false{$endif}): TSynSQLTableDataSet; overload;
 {$ifdef HASINLINE}inline;{$endif}

/// convert a JSON ORM result into a VCL DataSet, following TSQLRecord field types
// - this function is just a wrapper around TSynSQLTableDataSet.CreateFromJSON()
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - with Unicode version of Delphi (2009+), string/UnicodeString will be used
function JSONTableToDataSet(aOwner: TComponent; const aJSON: RawUTF8;
  const Tables: array of TSQLRecordClass
  {$ifndef UNICODE}; aForceWideString: boolean=false{$endif}): TSynSQLTableDataSet;

/// convert a JSON result into a VCL DataSet, with a given set of column types
// - this function is just a wrapper around TSynSQLTableDataSet.CreateFromJSON()
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - with Unicode version of Delphi (2009+), string/UnicodeString will be used
function JSONToDataSet(aOwner: TComponent; const aJSON: RawUTF8;
  const ColumnTypes: array of TSQLFieldType
  {$ifndef UNICODE}; aForceWideString: boolean=false{$endif}): TSynSQLTableDataSet; overload;


implementation

function JSONToDataSet(aOwner: TComponent; const aJSON: RawUTF8
  {$ifndef UNICODE}; aForceWideString: boolean{$endif}): TSynSQLTableDataSet;
begin
  result := TSynSQLTableDataSet.CreateFromJSON(
    aOwner,aJSON{$ifndef UNICODE},aForceWideString{$endif});
end;

function JSONToDataSet(aOwner: TComponent; const aJSON: RawUTF8;
  const ColumnTypes: array of TSQLFieldType
  {$ifndef UNICODE}; aForceWideString: boolean{$endif}): TSynSQLTableDataSet;
begin
  result := TSynSQLTableDataSet.CreateFromJSON(
    aOwner,aJSON,ColumnTypes{$ifndef UNICODE},aForceWideString{$endif});
end;

function JSONTableToDataSet(aOwner: TComponent; const aJSON: RawUTF8;
  const Tables: array of TSQLRecordClass
  {$ifndef UNICODE}; aForceWideString: boolean{$endif}): TSynSQLTableDataSet;
begin
  result := TSynSQLTableDataSet.CreateFromJSON(
    aOwner,aJSON,Tables{$ifndef UNICODE},aForceWideString{$endif});
end;



{ TSynSQLTableDataSet }

constructor TSynSQLTableDataSet.Create(Owner: TComponent; Table: TSQLTable
  {$ifndef UNICODE}; ForceWideString: boolean{$endif});
begin
  inherited Create(Owner);
  {$ifndef UNICODE}
  fForceWideString := ForceWideString;
  {$endif}
  if Table<>nil then
    fTable := Table;
  Open;
end;

constructor TSynSQLTableDataSet.CreateOwnedTable(Owner: TComponent; Table: TSQLTable
  {$ifndef UNICODE}; ForceWideString: boolean{$endif});
begin
  Create(Owner,Table{$ifndef UNICODE},ForceWideString{$endif});
  if Table<>nil then
    fTableShouldBeFreed := true;
end;

constructor TSynSQLTableDataSet.CreateFromJSON(Owner: TComponent; const JSON: RawUTF8
  {$ifndef UNICODE}; ForceWideString: boolean{$endif});
var T: TSQLTable;
begin
  T := TSQLTableJSON.Create('',JSON);
  try
    CreateOwnedTable(Owner,T{$ifndef UNICODE},ForceWideString{$endif});
    T := nil;
  finally
    T.Free; // release temporary instance in case of TSynSQLTableDataSet error
  end;
end;

constructor TSynSQLTableDataSet.CreateFromJSON(Owner: TComponent; const JSON: RawUTF8;
  const ColumnTypes: array of TSQLFieldType
  {$ifndef UNICODE}; ForceWideString: boolean{$endif});
var T: TSQLTable;
begin
  T := TSQLTableJSON.CreateWithColumnTypes(ColumnTypes,'',JSON);
  try
    CreateOwnedTable(Owner,T{$ifndef UNICODE},ForceWideString{$endif});
    T := nil;
  finally
    T.Free; // release temporary instance in case of TSynSQLTableDataSet error
  end;
end;

constructor TSynSQLTableDataSet.CreateFromJSON(Owner: TComponent; const JSON: RawUTF8;
  const Tables: array of TSQLRecordClass
  {$ifndef UNICODE}; ForceWideString: boolean{$endif});
var T: TSQLTable;
begin
  T := TSQLTableJSON.CreateFromTables(Tables,'',JSON);
  try
    CreateOwnedTable(Owner,T{$ifndef UNICODE},ForceWideString{$endif});
    T := nil;
  finally
    T.Free; // release temporary instance in case of TSynSQLTableDataSet error
  end;
end;


destructor TSynSQLTableDataSet.Destroy;
begin
  inherited;
  if fTableShouldBeFreed then
    FreeAndNil(fTable);
end;

function TSynSQLTableDataSet.GetRecordCount: Integer;
begin
  if fTable<>nil then
    result := fTable.RowCount else
    result := 0;
end;

function TSynSQLTableDataSet.GetRowFieldData(Field: TField; RowIndex: integer;
  out ResultLen: Integer; OnlyCheckNull: boolean): Pointer;
var info: PSQLTableFieldType;
    F: integer;
    P: PUTF8Char;
label Txt;
begin
  result := nil;
  F := Field.Index;
  inc(RowIndex); // first TSQLTable row are field names
  P := fTable.Get(RowIndex,F);
  if P=nil then // null field or out-of-range RowIndex/F -> result := nil
    exit;
  result := @fTemp64; // let result point to Int64, Double or TDatetime
  if OnlyCheckNull then
    exit;
  case fTable.FieldType(F,info) of
  sftBoolean, sftInteger, sftID, sftTID:
    SetInt64(P,fTemp64);
  sftFloat, sftCurrency:
    unaligned(PDouble(@fTemp64)^) := GetExtended(P);
  sftEnumerate, sftSet:
    if info^.ContentTypeInfo=nil then
      SetInt64(P,fTemp64) else
      goto Txt;
  sftDateTime, sftDateTimeMS:
    unaligned(PDouble(@fTemp64)^) := Iso8601ToDateTimePUTF8Char(P,0);
  sftTimeLog, sftModTime, sftCreateTime:
    unaligned(PDouble(@fTemp64)^) := TimeLogToDateTime(GetInt64(P));
  sftUnixTime:
    unaligned(PDouble(@fTemp64)^) := UnixTimeToDateTime(GetInt64(P));
  sftUnixMSTime:
    unaligned(PDouble(@fTemp64)^) := UnixMSTimeToDateTime(GetInt64(P));
  sftBlob: begin
    fTempBlob := BlobToTSQLRawBlob(P);
    result := pointer(fTempBlob);
    resultLen := length(fTempBlob);
  end;
  else begin // e.g. sftUTF8Text
Txt:result := P;
    resultLen := StrLen(P);
  end;
  end;
end;

procedure GetDBFieldValue(aTable: TSQLTable; aRow: integer; aField: TField;
  aDataSet: TDataSet; const DBFieldDef: TDBFieldDef);
var blob: TSQLRawBlob;
    sstream,dstream: TStream;
    P: PUTF8Char;
begin
  if (aField<>nil) and (aRow>0) then
  with DBFieldDef do begin
    P := aTable.Get(aRow,SQLIndex);
    if P=nil then
      aField.Clear else
    case SQLType of
    sftBoolean:
      aField.AsBoolean := GetInt64(P)<>0;
    sftInteger, sftID, sftTID, sftSessionUserID:
      if aField.DataType=ftLargeInt then // handle Int64 values directly
        TLargeintField(aField).Value := GetInt64(P) else
        aField.AsInteger := GetInteger(P);
    sftFloat, sftCurrency:
      aField.AsFloat := GetExtended(P);
    sftEnumerate, sftSet:
      if FieldType^.ContentTypeInfo=nil then
        aField.AsInteger := GetInteger(P) else
        aField.AsString := aTable.GetString(aRow,SQLIndex);
    sftDateTime, sftDateTimeMS:
      aField.AsDateTime := Iso8601ToDateTimePUTF8Char(P,0);
    sftUnixTime:
      aField.AsDateTime := UnixTimeToDateTime(GetInt64(P));
    sftUnixMSTime:
      aField.AsDateTime := UnixMSTimeToDateTime(GetInt64(P));
    sftTimeLog, sftModTime, sftCreateTime:
      aField.AsDateTime := TimeLogToDateTime(GetInt64(P));
    sftBlob: begin
      blob := BlobToTSQLRawBlob(P);
      if (blob='') or (aDataSet=nil) then
        aField.Clear else begin
        sstream := TRawByteStringStream.Create(blob);
        try
          dstream := aDataSet.CreateBlobStream(aField,bmWrite);
          try
            dstream.CopyFrom(sstream,0);
          finally
            dstream.Free;
          end;
        finally
          sstream.Free;
        end;
      end;
    end;
    sftUTF8Text:
      if aField.DataType=ftWideString then
        TWideStringField(aField).Value := aTable.GetSynUnicode(aRow,SQLIndex) else
        aField.AsString := aTable.GetString(aRow,SQLIndex);
    else
      aField.AsVariant := aTable.GetVariant(aRow,SQLIndex);
    end;
  end;
end;

procedure GetDBFieldDef(aTable: TSQLTable; aField: integer;
  out DBFieldDef: TDBFieldDef{$ifndef UNICODE}; aForceWideString: boolean{$endif});
begin
  with DBFieldDef do begin
    DBSize := 0;
    SQLIndex := aField;
    FieldName := aTable.GetString(0,aField);
    if FieldName='' then begin
      DBType := DB.ftUnknown;
      SQLType := sftUnknown;
    end else begin
      SQLType := aTable.FieldType(aField,FieldType);
      case SQLType of
      sftBoolean:
        DBType := ftBoolean;
      sftInteger, sftID, sftTID:
        DBType := ftLargeint; // LargeInt=Int64
      sftFloat, sftCurrency:
        DBType := ftFloat;
      sftEnumerate, sftSet:
        if FieldType^.ContentTypeInfo=nil then
          DBType := ftInteger else begin
          DBSize := 64;
          DBType := ftDefaultVCLString;
        end;
      sftRecord: begin
          DBSize := 64;
          DBType := ftDefaultVCLString;
        end;
      sftDateTime, sftDateTimeMS, sftUnixTime, sftUnixMSTime,
      sftTimeLog, sftModTime, sftCreateTime:
        DBType := ftDateTime;
      sftBlob: begin
          DBSize := (aTable.FieldLengthMax(aField,true)*3) shr 2;
          DBType := DB.ftBlob;
        end;
      sftUTF8Text: begin
        DBSize := aTable.FieldLengthMax(aField,true);
        {$ifndef UNICODE} // for Delphi 2009+ TWideStringField = UnicodeString!
        if aForceWideString then
          DBType := ftWideString else
        {$endif}
          DBType := ftDefaultVCLString;
      end;
      else begin
        DBType := ftDefaultVCLString;
        DBSize := aTable.FieldLengthMax(aField,true);
      end;
      end;
    end;
  end;
end;

procedure TSynSQLTableDataSet.InternalInitFieldDefs;
var F: Integer;
    Def: TDBFieldDef;
begin
  FieldDefs.Clear;
  for F := 0 to fTable.FieldCount-1 do begin
    GetDBFieldDef(fTable,F,Def{$ifndef UNICODE},fForceWideString{$endif});
    FieldDefs.Add(Def.FieldName,Def.DBType,Def.DBSize);
  end;
end;

function TSynSQLTableDataSet.SearchForField(const aLookupFieldName: RawUTF8;
  const aLookupValue: variant; aOptions: TLocateOptions): integer;
var f: integer;
    val: RawUTF8;
begin
  f := Table.FieldIndex(aLookupFieldName);
  if f<0 then
    result := 0 else begin
    VariantToUTF8(aLookupValue,val);
    if loPartialKey in aOptions then
      result := Table.SearchFieldIdemPChar(val,f) else
      result := Table.SearchFieldEquals(val,f,1,not (loCaseInsensitive in aOptions));
  end;
end;

end.

