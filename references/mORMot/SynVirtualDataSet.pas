/// DB VCL read-only virtual dataset
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynVirtualDataSet;

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
  - Esteban Martin (EMartin)
  - mingda
  - Murat Ak
  - Valentin (StxLog)

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
  SysUtils,
  Classes,
  {$ifndef FPC}
  Contnrs,
  {$endif}
  {$ifndef NOVARIANTS}
  Variants,
  {$endif}
  SynCommons,
  SynTable,
  {$ifdef ISDELPHIXE2}
  System.Generics.Collections,
  Data.DB, Data.FMTBcd;
  {$else}
  DB, FMTBcd;
  {$endif}


type
  {$ifndef UNICODE} // defined as TRecordBuffer = PByte in newer DB.pas
  TRecordBuffer = PChar;
  {$endif UNICODE}
  PDateTimeRec = ^TDateTimeRec;

  /// read-only virtual TDataSet able to access any content
  TSynVirtualDataSet = class(TDataSet)
  protected
    fCurrentRow: integer;
    fIsCursorOpen: boolean;

    // TDataSet overridden methods
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    function GetCanModify: Boolean; override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalClose; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalLast; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetRecNo(Value: Integer); override;
    function GetRecNo: Integer; override;

    // classses should override all those following methods:
    // - to read the data e.g. into memory:
    procedure InternalOpen; override;
    // - to initialize FieldDefs:
    // procedure InternalInitFieldDefs; override;
    // - to return row count:
    // function GetRecordCount: Integer; override;
    // - result should point to Int64,Double,Blob,UTF8 data (if ResultLen<>nil)
    function GetRowFieldData(Field: TField; RowIndex: integer; out ResultLen: Integer;
      OnlyCheckNull: boolean): Pointer; virtual; abstract;
    // - to search for a field, returning RecNo (0 = not found by default)
    function SearchForField(const aLookupFieldName: RawUTF8; const aLookupValue: variant;
      aOptions: TLocateOptions): integer; virtual;
    {$ifndef NOVARIANTS}
    // used to serialize TBCDVariant as JSON - BcdRead will always fail
    class procedure BcdWrite(const aWriter: TTextWriter; const aValue);
    //class function BcdRead(P: PUTF8Char; var aValue; out aValid: Boolean): PUTF8Char;
    {$endif}
  public
    /// this overridden constructor will compute an unique Name property
    constructor Create(Owner: TComponent); override;
    /// get BLOB column data for the current active row
    // - handle ftBlob,ftMemo,ftWideMemo via GetRowFieldData()
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    /// get BLOB column data for a given row (may not the active row)
    // - handle ftBlob,ftMemo,ftWideMemo via GetRowFieldData()
    function GetBlobStream(Field: TField; RowIndex: integer): TStream;
    /// get column data for the current active row
    // - handle ftBoolean,ftInteger,ftLargeint,ftFloat,ftCurrency,ftDate,ftTime,
    // ftDateTime,ftString,ftWideString kind of fields via GetRowFieldData()
    {$ifdef ISDELPHIXE3}
    {$ifdef ISDELPHIXE4}
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean; override;
    {$else}
    function GetFieldData(Field: TField; Buffer: TValueBuffer): Boolean; override;
    {$endif}
    {$else}
    function GetFieldData(Field: TField; Buffer: pointer): Boolean; override;
    {$endif}
    {$ifndef UNICODE}
    function GetFieldData(Field: TField; Buffer: pointer; NativeFormat: Boolean): Boolean; override;
    {$endif}
    /// searching a dataset for a specified record and making it the active record
    // - will call SearchForField protected virtual method for actual lookup
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions) : boolean; override;
  published
    property Active;
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
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

  {$ifndef NOVARIANTS}
  /// read-only virtual TDataSet able to access a dynamic array of TDocVariant
  // - could be used e.g. from the result of TMongoCollection.FindDocs() to
  // avoid most temporary conversion into JSON or TClientDataSet buffers
  TDocVariantArrayDataSet = class(TSynVirtualDataSet)
  protected
    fValues: TVariantDynArray;
    fColumns: array of record
      Name: RawUTF8;
      FieldType: TSQLDBFieldType;
    end;
    fTemp64: Int64;
    fTempUTF8: RawUTF8;
    fTempBlob: RawByteString;
    procedure InternalInitFieldDefs; override;
    function GetRecordCount: Integer; override;
    function GetRowFieldData(Field: TField; RowIndex: integer;
      out ResultLen: Integer; OnlyCheckNull: boolean): Pointer; override;
    function SearchForField(const aLookupFieldName: RawUTF8; const aLookupValue: variant;
      aOptions: TLocateOptions): integer; override;
  public
    /// initialize the virtual TDataSet from a dynamic array of TDocVariant
    // - you can set the expected column names and types matching the results
    // document layout - if no column information is specified, the first
    // TDocVariant will be used as reference
    constructor Create(Owner: TComponent; const Data: TVariantDynArray;
      const ColumnNames: array of RawUTF8; const ColumnTypes: array of TSQLDBFieldType); reintroduce;
  end;
  {$endif}

const
  /// map the VCL string type, depending on the Delphi compiler version
  {$ifdef UNICODE}
  ftDefaultVCLString = ftWideString;
  {$else}
  ftDefaultVCLString = ftString;
  {$endif}

  /// map the best ft*Memo type available, depending on the Delphi compiler version
  {$ifdef ISDELPHI2007ANDUP}
  ftDefaultMemo = ftWideMemo;
  {$else}
  ftDefaultMemo = ftMemo;
  {$endif}


/// append a TBcd value as text to the output buffer
// - very optimized for speed
procedure AddBcd(WR: TTextWriter; const AValue: TBcd);

type
  /// a string buffer, used by InternalBCDToBuffer to store its output text
  TBCDBuffer = array[0..66] of AnsiChar;

/// convert a TBcd value as text to the output buffer
// - buffer is to be array[0..66] of AnsiChar
// - returns the resulting text start in PBeg, and the length as function result
// - does not handle negative sign and 0 value - see AddBcd() function use case
// - very optimized for speed
function InternalBCDToBuffer(const AValue: TBcd; out ADest: TBCDBuffer; var PBeg: PAnsiChar): integer;

/// convert a TBcd value into a currency
// - purepascal version included in latest Delphi versions is slower than this
function BCDToCurr(const AValue: TBcd; var Curr: Currency): boolean;

/// convert a TBcd value into a RawUTF8 text
// - will call fast InternalBCDToBuffer function
procedure BCDToUTF8(const AValue: TBcd; var result: RawUTF8); overload;

/// convert a TBcd value into a RawUTF8 text
// - will call fast InternalBCDToBuffer function
function BCDToUTF8(const AValue: TBcd): RawUTF8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a TBcd value into a VCL string text
// - will call fast InternalBCDToBuffer function
function BCDToString(const AValue: TBcd): string;


/// export all rows of a TDataSet into JSON
// - will work for any kind of TDataSet
function DataSetToJSON(Data: TDataSet): RawUTF8;

{$ifndef NOVARIANTS}
/// convert a dynamic array of TDocVariant result into a VCL DataSet
// - this function is just a wrapper around TDocVariantArrayDataSet.Create()
// - the TDataSet will be opened once created
function ToDataSet(aOwner: TComponent; const Data: TVariantDynArray;
  const ColumnNames: array of RawUTF8; const ColumnTypes: array of TSQLDBFieldType): TDocVariantArrayDataSet; overload;
{$endif}


implementation

function InternalBCDToBuffer(const AValue: TBcd; out ADest: TBCDBuffer; var PBeg: PAnsiChar): integer;
var i,DecimalPos: integer;
    P,Frac: PByte;
    PEnd: PAnsiChar;
begin
  result := 0;
  if AValue.Precision=0 then
    exit;
  DecimalPos := AValue.Precision-(AValue.SignSpecialPlaces and $3F);
  P := @ADest;
  Frac := @Avalue.Fraction;
  for i := 0 to AValue.Precision-1 do begin
    if i=DecimalPos then
      if i=0 then begin
        PWord(P)^ := ord('0')+ord('.')shl 8;
        inc(P,2);
      end else begin
        P^ := ord('.');
        inc(P);
      end;
    if (i and 1)=0 then
      P^ := ((Frac^ and $F0) shr 4)+ord('0') else begin
      P^ := ((Frac^ and $0F))+ord('0');
      inc(Frac);
    end;
    inc(P);
  end;
  // remove trailing 0 after decimal
  if AValue.Precision>DecimalPos then begin
    repeat dec(P) until (P^<>ord('0')) or (P=@ADest);
    PEnd := pointer(P);
    if PEnd^<>'.' then
      inc(PEnd);
  end else
    PEnd := pointer(P);
  PEnd^ := #0;
  // remove leading 0
  PBeg := @ADest;
  while (PBeg[0]='0') and (PBeg[1] in ['0'..'9']) do inc(PBeg);
  result := PEnd-PBeg;
end;

procedure AddBcd(WR: TTextWriter; const AValue: TBcd);
var len: integer;
    PBeg: PAnsiChar;
    tmp: TBCDBuffer;
begin
  len := InternalBCDToBuffer(AValue,tmp,PBeg);
  if len<=0 then
    WR.Add('0') else begin
    if AValue.SignSpecialPlaces and $80=$80 then
      WR.Add('-');
    WR.AddNoJSONEscape(PBeg,len);
  end;
end;

function BCDToCurr(const AValue: TBcd; var Curr: Currency): boolean;
var len: integer;
    PBeg: PAnsiChar;
    tmp: TBCDBuffer;
begin
  len := InternalBCDToBuffer(AValue,tmp,PBeg);
  if len<=0 then
    Curr := 0 else begin
    PInt64(@Curr)^ := StrToCurr64(pointer(PBeg));
    if AValue.SignSpecialPlaces and $80=$80 then
      Curr := -Curr;
  end;
  result := true;
end;

procedure BCDToUTF8(const AValue: TBcd; var result: RawUTF8);
var len: integer;
    PBeg: PAnsiChar;
    tmp: TBCDBuffer;
begin
  len := InternalBCDToBuffer(AValue,tmp,PBeg);
  SetString(result,PBeg,len);
end;

function BCDToUTF8(const AValue: TBcd): RawUTF8;
begin
  BCDToUTF8(AValue,result);
end;

function BCDToString(const AValue: TBcd): string;
var len: integer;
    PBeg: PAnsiChar;
    tmp: TBCDBuffer;
begin
  len := InternalBCDToBuffer(AValue,tmp,PBeg);
  Ansi7ToString(PWinAnsiChar(PBeg),len,result);
end;


var
  GlobalDataSetCount: integer;

type
  /// define how a single row is identified
  // - for TSynVirtualDataSet, it is just the row index (starting at 0)
  TRecInfoIdentifier = integer;

  PRecInfoIdentifier = ^TRecInfoIdentifier;

  /// pointer to an internal structure used to identify a row position
  PRecInfo = ^TRecInfo;

  /// internal structure used to identify a row position
  TRecInfo = record
    /// define how a single row is identified
    RowIndentifier: TRecInfoIdentifier;
    /// any associated bookmark
    Bookmark: TRecInfoIdentifier;
    /// any associated bookmark flag
    BookmarkFlag: TBookmarkFlag;
  end;


{ TSynVirtualDataSet }

function TSynVirtualDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  result := AllocMem(sizeof(TRecInfo));
end;

procedure TSynVirtualDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer);
  Buffer := nil;
end;

procedure TSynVirtualDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PRecInfoIdentifier(Data)^ := PRecInfo(Buffer)^.Bookmark;
end;

function TSynVirtualDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  result := PRecInfo(Buffer)^.BookmarkFlag;
end;

function TSynVirtualDataSet.GetCanModify: Boolean;
begin
  result := false; // we define a READ-ONLY TDataSet
end;

{$ifndef UNICODE}
function TSynVirtualDataSet.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
begin
  if Field.DataType in [ftWideString] then
    NativeFormat := true; // to force Buffer as PWideString
  Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;
{$endif}

{$ifdef ISDELPHIXE3}
{$ifdef ISDELPHIXE4}
function TSynVirtualDataSet.GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean;
{$else}
function TSynVirtualDataSet.GetFieldData(Field: TField; Buffer: TValueBuffer): Boolean;
{$endif}
{$else}
function TSynVirtualDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
{$endif}
var Data, Dest: pointer;
    RowIndex, DataLen, MaxLen: integer;
    Temp: RawByteString;
    OnlyTestForNull: boolean;
    TS: TTimeStamp;
begin
  OnlyTestForNull := (Buffer=nil);
  RowIndex := PRecInfo(ActiveBuffer).RowIndentifier;
  Data := GetRowFieldData(Field,RowIndex,DataLen,OnlyTestForNull);
  result := Data<>nil; // null field or out-of-range RowIndex/Field
  if OnlyTestForNull or not result then
    exit;
  Dest := pointer(Buffer); // works also if Buffer is [var] TValueBuffer
  case Field.DataType of // Data^ points to Int64,Double,Blob,UTF8
  ftBoolean:
    PWORDBOOL(Dest)^ := PBoolean(Data)^;
  ftInteger:
    PInteger(Dest)^ := PInteger(Data)^;
  ftLargeint, ftFloat, ftCurrency:
    PInt64(Dest)^ := PInt64(Data)^;
  ftDate, ftTime, ftDateTime:
    if PDateTime(Data)^=0 then // handle 30/12/1899 date as NULL
      result := false else begin  // inlined DataConvert(Field,Data,Dest,true)
      TS := DateTimeToTimeStamp(PDateTime(Data)^);
      case Field.DataType of
      ftDate:     PDateTimeRec(Dest)^.Date := TS.Date;
      ftTime:     PDateTimeRec(Dest)^.Time := TS.Time;
      ftDateTime:
        if (TS.Time<0) or (TS.Date<=0) then
          result := false else // matches ValidateTimeStamp() expectations
          PDateTimeRec(Dest)^.DateTime := TimeStampToMSecs(TS);
      end; // see NativeToDateTime/DateTimeToNative in TDataSet.DataConvert
    end;
  ftString: begin
    if DataLen<>0 then begin
      CurrentAnsiConvert.UTF8BufferToAnsi(Data,DataLen,Temp);
      DataLen := length(Temp);
      MaxLen := Field.DataSize-1; // without trailing #0
      if DataLen>MaxLen then
        DataLen := MaxLen;
      move(pointer(Temp)^,Dest^,DataLen);
    end;
    PAnsiChar(Dest)[DataLen] := #0;
  end;
  ftWideString: begin
    {$ifdef ISDELPHI2007ANDUP} // here Dest = PWideChar[] of DataSize bytes
    if DataLen=0 then
      PWideChar(Dest)^ := #0 else
      UTF8ToWideChar(Dest,Data,(Field.DataSize-2)shr 1,DataLen);
    {$else}          // here Dest is PWideString
    UTF8ToWideString(Data,DataLen,WideString(Dest^));
    {$endif}
  end;
  // ftBlob,ftMemo,ftWideMemo should be retrieved by CreateBlobStream()
  else raise EDatabaseError.CreateFmt('%s.GetFieldData unhandled DataType=%s (%d)',
         [ClassName,GetEnumName(TypeInfo(TFieldType),ord(Field.DataType))^,ord(Field.DataType)]);
  end;
end;

function TSynVirtualDataSet.GetBlobStream(Field: TField; RowIndex: integer): TStream;
var Data: pointer;
    DataLen: integer;
begin
  Data := GetRowFieldData(Field,RowIndex,DataLen,false);
  if Data=nil then // should point to Blob or UTF8 data
    result := nil else
    case Field.DataType of
    ftBlob:
      result := TSynMemoryStream.Create(Data,DataLen);
    ftMemo, ftString:
      result := TRawByteStringStream.Create(CurrentAnsiConvert.UTF8BufferToAnsi(Data,DataLen));
    {$ifdef ISDELPHI2007ANDUP} ftWideMemo, {$endif} ftWideString:
      result := TRawByteStringStream.Create(Utf8DecodeToRawUnicode(Data,DataLen));
    else raise EDatabaseError.CreateFmt('%s.CreateBlobStream DataType=%d',
      [ClassName,ord(Field.DataType)]);
    end;
end;

function TSynVirtualDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  if Mode<>bmRead then
    raise EDatabaseError.CreateFmt('%s BLOB should be ReadOnly',[ClassName]);
  result := GetBlobStream(Field,PRecInfo(ActiveBuffer).RowIndentifier);
  if result=nil then
    result := TSynMemoryStream.Create; // null BLOB returns a void TStream
end;

function TSynVirtualDataSet.GetRecNo: Integer;
begin
  result := fCurrentRow+1;
end;

function TSynVirtualDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  result := grOK;
  case GetMode of
    gmPrior:
      if fCurrentRow>0 then
        dec(fCurrentRow) else
        result := grBOF;
    gmCurrent:
      if fCurrentRow<0 then
        result := grBOF else
      if fCurrentRow>=GetRecordCount then
        result := grEOF;
    gmNext:
      if fCurrentRow<GetRecordCount-1 then
        inc(fCurrentRow) else
        result := grEOF;
  end;
  if result=grOK then
    with PRecInfo(Buffer)^ do begin
      RowIndentifier := fCurrentRow;
      BookmarkFlag := bfCurrent;
      Bookmark := fCurrentRow;
    end;
end;

function TSynVirtualDataSet.GetRecordSize: Word;
begin
  result := SizeOf(TRecInfoIdentifier); // excluding Bookmark information
end;

procedure TSynVirtualDataSet.InternalClose;
begin
  BindFields(false);
  {$ifdef ISDELPHIXE6}
  if not(lcPersistent in Fields.LifeCycles) then
  {$else}
  if DefaultFields then
  {$endif}
    DestroyFields;
  fIsCursorOpen := False;
end;

procedure TSynVirtualDataSet.InternalFirst;
begin
  fCurrentRow := -1;
end;

procedure TSynVirtualDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  fCurrentRow := PRecInfoIdentifier(Bookmark)^;
end;

procedure TSynVirtualDataSet.InternalHandleException;
begin
  if Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException(ExceptObject) else
    SysUtils.ShowException(ExceptObject,ExceptAddr);
end;

procedure TSynVirtualDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FillcharFast(Buffer^,sizeof(TRecInfo),0);
end;

procedure TSynVirtualDataSet.InternalLast;
begin
  fCurrentRow := GetRecordCount;
end;

procedure TSynVirtualDataSet.InternalOpen;
begin
  BookmarkSize := SizeOf(TRecInfo)-sizeof(TRecInfoIdentifier);
  InternalInitFieldDefs;
  {$ifdef ISDELPHIXE6}
  if not(lcPersistent in Fields.LifeCycles) then
  {$else}
  if DefaultFields then
  {$endif}
    CreateFields;
  BindFields(true);
  fCurrentRow := -1;
  fIsCursorOpen := True;
end;

procedure TSynVirtualDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  fCurrentRow := PRecInfo(Buffer).RowIndentifier;
end;

function TSynVirtualDataSet.IsCursorOpen: Boolean;
begin
  result := fIsCursorOpen;
end;

procedure TSynVirtualDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PRecInfo(Buffer)^.Bookmark := PRecInfoIdentifier(Data)^;
end;

procedure TSynVirtualDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TSynVirtualDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if Value<>RecNo then begin
    dec(Value);
    if cardinal(Value)>=cardinal(GetRecordCount) then
      raise ERangeError.CreateFmt('%s.SetRecNo(%d) with Count=%d',
        [ClassName,Value+1,GetRecordCount]);
    DoBeforeScroll;
    fCurrentRow := Value;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

constructor TSynVirtualDataSet.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  inc(GlobalDataSetCount);
  Name := ClassName+IntToStr(GlobalDataSetCount); // force unique name
end;

function TSynVirtualDataSet.SearchForField(const aLookupFieldName: RawUTF8;
  const aLookupValue: variant; aOptions: TLocateOptions): integer;
begin
  result := 0; // nothing found
end;

function TSynVirtualDataSet.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions) : boolean;
var i, l, h, found: Integer;
    {$ifdef ISDELPHIXE4}
    FieldList: TList<TField>;
    {$else}
    FieldList: TList;
    {$endif}
begin
  CheckActive;
  result := true;
  if not IsEmpty then
    if VarIsArray(KeyValues) then begin
      {$ifdef ISDELPHIXE4}
      FieldList := TList<TField>.Create;
      {$else}
      FieldList := TList.Create;
      {$endif}
      try
        GetFieldList(FieldList,KeyFields);
        l := VarArrayLowBound(KeyValues,1);
        h := VarArrayHighBound(KeyValues,1);
        if (FieldList.Count = 1) and (l < h) then begin
          found := SearchForField(StringToUTF8(KeyFields),KeyValues,Options);
          if found>0 then begin
            RecNo := found;
            exit;
          end;
        end
        else for i := 0 to FieldList.Count - 1 do begin
          found := SearchForField(StringToUTF8(TField(FieldList[i]).FieldName),
            KeyValues[l+i],Options);
          if found>0 then begin
            RecNo := found;
            exit;
          end;
        end;
      finally
        FieldList.Free;
      end;
    end else begin
      found := SearchForField(StringToUTF8(KeyFields),KeyValues,Options);
      if found>0 then begin
        RecNo := found;
        exit;
      end;
    end;
  result := false;
end;

{$ifndef NOVARIANTS}
type // as in FMTBcd.pas
  TFMTBcdData = class(TPersistent)
  private
    FBcd: TBcd;
  end;
  TFMTBcdVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VBcd: TFMTBcdData;
    Reserved4: Cardinal;
  end;

class procedure TSynVirtualDataSet.BcdWrite(const aWriter: TTextWriter; const aValue);
begin
  AddBCD(aWriter,TFMTBcdVarData(aValue).VBcd.FBcd);
end;
{$endif NOVARIANTS}


function DataSetToJSON(Data: TDataSet): RawUTF8;
var W: TJSONWriter;
    f: integer;
    blob: TRawByteStringStream;
begin
  result := 'null';
  if Data=nil then
    exit;
  Data.First;
  if Data.Eof then
    exit;
  W := TJSONWriter.Create(nil,true,false);
  try
    // get col names and types
    SetLength(W.ColNames,Data.FieldCount);
    for f := 0 to high(W.ColNames) do
      StringToUTF8(Data.FieldDefs[f].Name,W.ColNames[f]);
    W.AddColumns;
    W.Add('[');
    repeat
      W.Add('{');
      for f := 0 to Data.FieldCount-1 do begin
        W.AddString(W.ColNames[f]);
        with Data.Fields[f] do
        if IsNull then
          W.AddShort('null') else
        case DataType of
        ftBoolean:
          W.Add(AsBoolean);
        ftSmallint, ftInteger, ftWord, ftAutoInc:
          W.Add(AsInteger);
        ftLargeint:
          W.Add(TLargeintField(Data.Fields[f]).AsLargeInt);
        ftFloat, ftCurrency: // TCurrencyField is sadly a TFloatField
          W.Add(AsFloat,TFloatField(Data.Fields[f]).Precision);
        ftBCD:
          W.AddCurr64(AsCurrency);
        ftFMTBcd:
          AddBcd(W,AsBCD);
        ftTimeStamp, ftDate, ftTime, ftDateTime: begin
          W.Add('"');
          W.AddDateTime(AsDateTime);
          W.Add('"');
        end;
        ftString, ftFixedChar, ftMemo, ftGuid: begin
          W.Add('"');
          W.AddAnsiString({$ifdef UNICODE}AsAnsiString{$else}AsString{$endif},
            twJSONEscape);
          W.Add('"');
        end;
        ftWideString: begin
          W.Add('"');
          W.AddJSONEscapeW(pointer(TWideStringField(Data.Fields[f]).Value));
          W.Add('"');
        end;
        ftVariant:
          W.AddVariant(AsVariant);
        ftBytes, ftVarBytes, ftBlob, ftGraphic, ftOraBlob, ftOraClob: begin
          blob := TRawByteStringStream.Create;
          try
            (Data.Fields[f] as TBlobField).SaveToStream(blob);
            W.WrBase64(pointer(blob.DataString),length(blob.DataString),true);
          finally
            blob.Free;
          end;
        end;
        {$ifdef ISDELPHI2007ANDUP}
        ftWideMemo, ftFixedWideChar: begin
          W.Add('"');
          W.AddJSONEscapeW(pointer(AsWideString));
          W.Add('"');
        end;
        {$endif}
        {$ifdef UNICODE}
        ftShortint, ftByte:
          W.Add(AsInteger);
        ftLongWord:
          W.AddU(TLongWordField(Data.Fields[f]).Value);
        ftExtended:
          W.AddDouble(AsFloat);
        ftSingle:
          W.Add(AsFloat,SINGLE_PRECISION);
        {$endif}
        else W.AddShort('null'); // unhandled field type
        end;
        W.Add(',');
      end;
      W.CancelLastComma;
      W.Add('}',',');
      Data.Next;
    until Data.Eof;
    W.CancelLastComma;
    W.Add(']');
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ TDocVariantArrayDataSet }

constructor TDocVariantArrayDataSet.Create(Owner: TComponent;
  const Data: TVariantDynArray; const ColumnNames: array of RawUTF8;
  const ColumnTypes: array of TSQLDBFieldType);
var n,ndx,j: PtrInt;
    first: PDocVariantData;
begin
  fValues := Data;
  n := Length(ColumnNames);
  if n>0 then begin
    if n<>length(ColumnTypes) then
      raise ESynException.CreateUTF8('%.Create(ColumnNames<>ColumnTypes)',[self]);
    SetLength(fColumns,n);
    for ndx := 0 to n-1 do begin
      fColumns[ndx].Name := ColumnNames[ndx];
      fColumns[ndx].FieldType := ColumnTypes[ndx];
    end;
  end else
  if fValues<>nil then begin
    first := _Safe(fValues[0],dvObject);
    SetLength(fColumns,first^.Count);
    for ndx := 0 to first^.Count-1 do begin
      fColumns[ndx].Name := first^.Names[ndx];
      fColumns[ndx].FieldType := VariantTypeToSQLDBFieldType(first^.Values[ndx]);
      case fColumns[ndx].FieldType of
      SynTable.ftNull:
        fColumns[ndx].FieldType := SynTable.ftBlob;
      SynTable.ftCurrency:
        fColumns[ndx].FieldType := SynTable.ftDouble;
      SynTable.ftInt64: // ensure type coherency of whole column
        for j := 1 to first^.Count-1 do
          if j>=Length(fValues) then // check objects are consistent
            break else
            with _Safe(fValues[j],dvObject)^ do
            if (ndx<Length(Names)) and IdemPropNameU(Names[ndx],fColumns[ndx].Name) then
            if VariantTypeToSQLDBFieldType(Values[ndx]) in
                [SynTable.ftNull,SynTable.ftDouble,SynTable.ftCurrency] then begin
              fColumns[ndx].FieldType := SynTable.ftDouble;
              break;
            end;
      end;
    end;
  end;
  inherited Create(Owner);
end;

function TDocVariantArrayDataSet.GetRecordCount: Integer;
begin
  result := length(fValues);
end;

function TDocVariantArrayDataSet.GetRowFieldData(Field: TField;
  RowIndex: integer; out ResultLen: Integer; OnlyCheckNull: boolean): Pointer;
var F,ndx: integer;
    wasString: Boolean;
begin
  result := nil;
  F := Field.Index;
  if (cardinal(RowIndex)<cardinal(length(fValues))) and
     (cardinal(F)<cardinal(length(fColumns))) and
     not (fColumns[F].FieldType in [ftNull,SynTable.ftUnknown,SynTable.ftCurrency]) then
    with _Safe(fValues[RowIndex])^ do
    if (Kind=dvObject) and (Count>0) then begin
      if IdemPropNameU(fColumns[F].Name,Names[F]) then
        ndx := F else // optimistic match
        ndx := GetValueIndex(fColumns[F].Name);
      if ndx>=0 then
        if VarIsEmptyOrNull(Values[ndx]) then
          exit else begin
          result := @fTemp64;
          if not OnlyCheckNull then
          case fColumns[F].FieldType of
          ftInt64:
            VariantToInt64(Values[ndx],fTemp64);
          ftDouble,SynTable.ftDate:
            VariantToDouble(Values[ndx],unaligned(PDouble(@fTemp64)^));
          ftUTF8: begin
            VariantToUTF8(Values[ndx],fTempUTF8,wasString);
            result := pointer(fTempUTF8);
            ResultLen := length(fTempUTF8);
          end;
          SynTable.ftBlob: begin
            VariantToUTF8(Values[ndx],fTempUTF8,wasString);
            if Base64MagicCheckAndDecode(pointer(fTempUTF8),length(fTempUTF8),fTempBlob) then begin
              result := pointer(fTempBlob);
              ResultLen := length(fTempBlob);
            end;
          end;
          end;
        end;
    end;
end;

procedure TDocVariantArrayDataSet.InternalInitFieldDefs;
const TYPES: array[TSQLDBFieldType] of TFieldType = (
  // ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUTF8, ftBlob
  ftWideString,ftWideString,ftLargeint,ftFloat,ftFloat,ftDate,ftWideString,ftBlob);
var F,siz: integer;
begin
  FieldDefs.Clear;
  for F := 0 to high(fColumns) do begin
    if fColumns[F].FieldType=ftUTF8 then
      siz := 16 else
      siz := 0;
    FieldDefs.Add(UTF8ToString(fColumns[F].Name),TYPES[fColumns[F].FieldType],siz);
  end;
end;

function TDocVariantArrayDataSet.SearchForField(const aLookupFieldName: RawUTF8;
  const aLookupValue: variant; aOptions: TLocateOptions): integer;
var f: integer;
begin
  f := -1; // allows O(1) field lookup for invariant object columns
  for result := 1 to length(fValues) do
  with _Safe(fValues[result-1])^ do
    if (Kind=dvObject) and (Count>0) then begin
      if (cardinal(f)>=cardinal(Count)) or
         not IdemPropNameU(aLookupFieldName,Names[f]) then
        f := GetValueIndex(aLookupFieldName);
      if (f>=0) and (SortDynArrayVariantComp(TVarData(Values[f]),
         TVarData(aLookupValue),loCaseInsensitive in aOptions)=0) then
        exit;
   end;
  result := 0;
end;


function ToDataSet(aOwner: TComponent; const Data: TVariantDynArray;
  const ColumnNames: array of RawUTF8; const ColumnTypes: array of TSQLDBFieldType): TDocVariantArrayDataSet; overload;
begin
  result := TDocVariantArrayDataSet.Create(aOwner,Data,ColumnNames,ColumnTypes);
  result.Open;
end;

initialization
  {$ifndef NOVARIANTS}
  TTextWriter.RegisterCustomJSONSerializerForVariantByType(
    VarFMTBcd,nil,TSynVirtualDataSet.BcdWrite);
  {$endif}
end.
