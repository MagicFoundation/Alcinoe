(********************************************************************************)
(*                        UNIFIED INTERBASE (UIB)                               *)
(*                                                                              *)
(* The contents of this file are subject to the Mozilla Public License Version  *)
(* 1.1 (the "License"); you may not use this file except in compliance with the *)
(* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ *)
(*                                                                              *)
(* Software distributed under the License is distributed on an "AS IS" basis,   *)
(* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for *)
(* the specific language governing rights and limitations under the License.    *)
(*                                                                              *)
(* Unit owner : Henri Gourvest <hgourvest@progdigy.com>                         *)
(* Contributor:                                                                 *)
(*     Volkan Ceylan <volkance@hotmail.com>                                     *)
(*     Olivier Guilbaud <oguilb@free.fr>                                        *)
(********************************************************************************)

unit uibdataset;

{$I uib.inc}

interface

uses
  SysUtils, Classes, DB, uib, uiblib, uibase, uibconst;

type
{$IFNDEF UNICODE}
  TRecordBuffer = PAnsiChar;
{$ENDIF}


  TUIBBookMark = record
    Bookmark: Longint;
    BookmarkFlag: TBookmarkFlag;
  end;
  PUIBBookMark = ^TUIBBookMark;

{$IFNDEF FPC}
  TUIBFieldDef = class(TFieldDef)
  private
    FOrigin: string;
  public
    property Origin: string read FOrigin write FOrigin;
  end;
{$ENDIF}

{$IFNDEF FPC}
  TUIBBCDField = class(TBCDField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsCurrency: Currency; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Size default 8;
  end;
{$ENDIF}

  TUIBCustomDataSet = class(TDataSet)
  private
    FStatement: TUIBQuery;
    FOnClose: TEndTransMode;
    FIsLast, FIsFirst: boolean;
    FCurrentRecord: Integer;
    FComplete: boolean;
    FIsOpen: Boolean;
    FRecordSize : Integer;
    FRecordBufferSize: Integer;
    procedure OnStatementClose(Sender: TObject);
    function GetOnError: TEndTransMode;
    function GetSQL: TStrings;
    function GetTransaction: TUIBTransaction;
    function GetUniDirectional: boolean;
    procedure SetOnClose(const Value: TEndTransMode);
    procedure SetOnError(const Value: TEndTransMode);
    procedure SetSQL(const Value: TStrings);
    procedure SetTransaction(const Value: TUIBTransaction);
    procedure SetUniDirectional(const Value: boolean);
    function GetFetchBlobs: boolean;
    procedure SetFetchBlobs(const Value: boolean);
    procedure SetDatabase(const Value: TUIBDataBase);
    function GetDatabase: TUIBDataBase;
    function GetParams: TSQLParams;
    function GetInternalFields: TSQLResult;
    function GetBufferChunks: Cardinal;
    procedure SetBufferChunks(const Value: Cardinal);
    function GetRowsAffected: Cardinal;
    function InternalGetFieldData(FieldNo: Integer; Buffer: Pointer; Native: Boolean): Boolean;
  protected
  {$IFNDEF FPC}
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
  {$ENDIF}
    property BufferChunks: Cardinal read GetBufferChunks write SetBufferChunks default 1000;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function IsCursorOpen: Boolean; override;

    function AllocRecordBuffer: TRecordBuffer; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    function GetRecordSize: Word; override;

    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    function GetRecNo: Longint; override;
    function GetRecordCount: Longint; override;
    procedure SetRecNo(Value: Integer); override;

    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;

    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    function GetCanModify: Boolean; override;

    procedure InternalRefresh; override;

    {$IFNDEF FPC}
    procedure SetActive(Value: Boolean); override;
    procedure CreateFields; override;
    {$ENDIF}


    property Transaction: TUIBTransaction read GetTransaction write SetTransaction;
    property Database: TUIBDataBase read GetDatabase write SetDatabase;
    property UniDirectional: boolean read  GetUniDirectional write SetUniDirectional default False;
    property OnClose: TEndTransMode read FOnClose write SetOnClose default etmCommit;
    property OnError: TEndTransMode read GetOnError write SetOnError default etmRollback;
    property SQL: TStrings read GetSQL write SetSQL;
    property FetchBlobs: boolean read GetFetchBlobs write SetFetchBlobs default False;
    property Params: TSQLParams read GetParams;
    property RowsAffected: Cardinal read GetRowsAffected;

{$IFNDEF COMPILER5_UP}
    function BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean; {$IFNDEF FPC}override;{$ENDIF}
    function CurrToBCD(const Curr: Currency; BCD: Pointer; Precision,
      Decimals: Integer): Boolean; {$IFNDEF FPC}override;{$ENDIF}
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload; override;
    function GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean; overload;{$IFNDEF FPC} override; {$ENDIF}
  {$IFNDEF FPC}
    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean; overload; override;
  {$ENDIF}
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    procedure Execute;
    procedure ExecSQL;
    procedure BuildStoredProc(const StoredProc: string; forSelect: boolean);

    procedure ReadBlob(const Index: Word; Stream: TStream); overload;
    procedure ReadBlobB(const Index: Word; var str: RawByteString); overload;
    procedure ReadBlobA(const Index: Word; var str: AnsiString); overload;
    procedure ReadBlobW(const Index: Word; var str: UnicodeString); overload;
    procedure ReadBlob(const Index: Word; var str: string); overload;
    procedure ReadBlob(const Index: Word; var Value: Variant); overload;
    procedure ReadBlob(const name: string; Stream: TStream); overload;
    procedure ReadBlobB(const name: string; var str: RawByteString); overload;
    procedure ReadBlobA(const name: string; var str: AnsiString); overload;
    procedure ReadBlobW(const name: string; var str: UnicodeString); overload;
    procedure ReadBlob(const name: string; var str: string); overload;
    procedure ReadBlob(const name: string; var Value: Variant); overload;

    procedure ParamsSetBlob(const Index: Word; Stream: TStream); overload;
    procedure ParamsSetBlobB(const Index: Word; var str: RawByteString); overload;
    procedure ParamsSetBlobA(const Index: Word; var str: AnsiString); overload;
    procedure ParamsSetBlobW(const Index: Word; var str: UnicodeString); overload;
    procedure ParamsSetBlob(const Index: Word; var str: string); overload;
    procedure ParamsSetBlob(const Index: Word; Buffer: Pointer; Size: Word); overload;
    procedure ParamsSetBlob(const Name: string; Stream: TStream); overload;
    procedure ParamsSetBlobB(const Name: string; var str: RawByteString); overload;
    procedure ParamsSetBlobA(const Name: string; var str: AnsiString); overload;
    procedure ParamsSetBlobW(const Name: string; var str: UnicodeString); overload;
    procedure ParamsSetBlob(const Name: string; var str: string); overload;
    procedure ParamsSetBlob(const Name: string; Buffer: Pointer; Size: Word); overload;

    property InternalFields: TSQLResult read GetInternalFields;
  end;

  TUIBDataSet = class(TUIBCustomDataSet)
  public
    property Params;
    property RowsAffected;
  published
    property BufferChunks;
    property Transaction;
    property Database;
    property UniDirectional;
    property OnClose;
    property OnError;
    property SQL;
    property FetchBlobs;
    property Active;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeScroll;
    property AfterScroll;
  end;

implementation
{$IFDEF COMPILER6_UP}
  uses fmtbcd, Variants, Math;
{$ENDIF}

{ TUIBCustomDataSet }

procedure TUIBCustomDataSet.InternalOpen;
begin
  FRecordSize := SizeOf(Integer);
  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;
  BindFields (True);
  FStatement.Open(False);
  FCurrentRecord := -1;
  FComplete := False;
  FRecordBufferSize := FRecordSize + sizeof (TUIBBookMark);
  BookmarkSize := sizeOf (Integer);
  FIsOpen := True;
end;

procedure TUIBCustomDataSet.InternalClose;
begin
  BindFields (False);
  if DefaultFields then
    DestroyFields;
  FStatement.Close(FOnClose);
  FIsOpen := False;
  FCurrentRecord := -1;
  FComplete := False;
end;

function TUIBCustomDataSet.IsCursorOpen: Boolean;
begin
  Result := FIsOpen;
end;

function TUIBCustomDataSet.InternalGetFieldData(FieldNo: Integer;
  Buffer: Pointer; Native: Boolean): Boolean;
var
  aFieldType: TUIBFieldType;
{$IFDEF UNICODE}
  uniStr: UnicodeString;
{$ENDIF}
begin
  dec(FieldNo);
  Result := False;

  if (FStatement.StatementType = stSelect) then
  begin
    if (FCurrentRecord < 0) then
      Exit;
    FStatement.Fields.GetRecord(PInteger(ActiveBuffer)^);
  end;

  if FStatement.Fields.IsNull[FieldNo] then
    Exit;

  if Buffer = nil then
  begin
    Result := True;
    Exit;
  end;
  aFieldType := FStatement.Fields.FieldType[FieldNo];
  with FStatement.Fields, Data.sqlvar[FieldNo] do
    case aFieldType of
      uftNumeric:
        begin
          case FStatement.Fields.SQLType[FieldNo] of
            SQL_SHORT:
              begin
              {$IFDEF COMPILER6_UP}
                if (sqlScale >= -4) and not Native then
                  Currency(Buffer^) := PSmallint(sqldata)^ / scaledivisor[sqlscale] else
                  TBCD(Buffer^) := strToBcd(FormatFloat(ScaleFormat[sqlscale], PSmallint(sqldata)^ / scaledivisor[sqlscale]));
              {$ELSE}
                {$IFDEF COMPILER5_UP}
                  if (sqlScale >= -4) then
                    Currency(Buffer^) := PSmallint(sqldata)^ / scaledivisor[sqlscale] else
                    CurrToBcd(PSmallint(sqldata)^/scaledivisor[sqlscale], TBCD(Buffer^));
                {$ELSE}
                  {$IFDEF FPC}
                    if (sqlscale = -4) then
                      PInt64(Buffer)^ := PSmallint(sqldata)^ else
                      if sqlscale > -4 then
                        PInt64(Buffer)^ := PSmallint(sqldata)^ * CurrencyDivisor[sqlscale] else
                        PDouble(Buffer)^ := PSmallint(sqldata)^ / scaledivisor[sqlscale];
                  {$ELSE}
                     unexpected
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
              end;
            SQL_LONG:
              begin
              {$IFDEF COMPILER6_UP}
                if (sqlScale >= -4) and not Native then
                  Currency(Buffer^) := PInteger(sqldata)^ / scaledivisor[sqlscale] else
                  TBCD(Buffer^) := strToBcd(FormatFloat(ScaleFormat[sqlscale], PInteger(sqldata)^ / scaledivisor[sqlscale]));
              {$ELSE}
                {$IFDEF COMPILER5_UP}
                  if (sqlScale >= -4) then
                    Currency(Buffer^) := PInteger(sqldata)^ / scaledivisor[sqlscale] else
                    CurrToBcd(PInteger(sqldata)^/scaledivisor[sqlscale], TBCD(Buffer^));
                {$ELSE}
                  {$IFDEF FPC}
                    if (sqlscale = -4) then
                      PInt64(Buffer)^ := PInteger(sqldata)^ else
                      if sqlscale > -4 then
                        PInt64(Buffer)^ := PInteger(sqldata)^ * CurrencyDivisor[sqlscale] else
                        PDouble(Buffer)^ := PInteger(sqldata)^ / scaledivisor[sqlscale];
                  {$ELSE}
                    unexpected
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
              end;
            SQL_INT64,
            SQL_QUAD:
              begin
              {$IFDEF COMPILER6_UP}
                if (sqlscale = -4) and not Native then
                  PInt64(Buffer)^ := PInt64(sqldata)^ else
                  if (sqlscale > -4) and not Native then
                    PInt64(Buffer)^ := PInt64(sqldata)^ * CurrencyDivisor[sqlscale] else
                    TBCD(Buffer^) := strToBcd(FormatFloat(ScaleFormat[sqlscale], PInt64(sqldata)^ / scaledivisor[sqlscale]));
              {$ELSE}
                {$IFDEF COMPILER5_UP}
                if (sqlscale = -4) then
                  PInt64(Buffer)^ := PInt64(sqldata)^ else
                  if (sqlscale > -4) then
                    PInt64(Buffer)^ := PInt64(sqldata)^ * CurrencyDivisor[sqlscale] else
                    CurrToBcd(PInt64(sqldata)^/scaledivisor[sqlscale], TBCD(Buffer^));
                {$ELSE}
                  {$IFDEF FPC}
                    if (sqlscale = -4) then
                      PInt64(Buffer)^ := PInt64(sqldata)^ else
                      if sqlscale > -4 then
                        PInt64(Buffer)^ := PInt64(sqldata)^ * CurrencyDivisor[sqlscale] else
                        PDouble(Buffer)^ := PInt64(sqldata)^ / scaledivisor[sqlscale];
                  {$ELSE}
                    unexpected
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
              end;
            SQL_D_FLOAT,
            SQL_DOUBLE:
              PDouble(Buffer)^ := PDouble(sqldata)^;
          else
            raise Exception.Create(EUIB_UNEXPECTEDCASTERROR);
          end;
        end;
      uftChar,
      uftCstring:
        begin
        {$IFDEF UNICODE}
          MBUDecode(sqldata, SqlLen, CharacterSetCP[CharacterSet], PWideChar(buffer));
          PWideChar(buffer)[(SqlLen div BytesPerCharacter[CharacterSet])] := #0;
        {$ELSE}
          Move(sqldata^, Buffer^, SqlLen);
          PAnsiChar(Buffer)[SqlLen] := #0;
        {$ENDIF}
        end;
      uftVarchar:
        begin
        {$IFDEF UNICODE}
          MBUDecode(@PVary(sqldata).vary_string, PVary(sqldata).vary_length, CharacterSetCP[CharacterSet], PWideChar(buffer));
        {$ELSE}
          Move(PVary(sqldata).vary_string, Buffer^, PVary(sqldata).vary_length);
          PAnsiChar(Buffer)[PVary(sqldata).vary_length] := #0;
        {$ENDIF}
        end;
      uftSmallint: PSmallint(Buffer)^ := PSmallint(sqldata)^;
      uftInteger : PInteger(Buffer)^ := PInteger(sqldata)^;
      uftFloat:
          PDouble(Buffer)^ := PSingle(sqldata)^;
      uftDoublePrecision:
          PDouble(Buffer)^ := PDouble(sqldata)^;
      uftTimestamp:
        begin
          {$IFNDEF FPC}
            DecodeTimeStamp(PIscTimeStamp(sqldata), PDouble(Buffer)^);
          {$ELSE}
            DecodeTimeStamp(PIscTimeStamp(sqldata),  TTimeStamp(Buffer^));
            Double(Buffer^) := TimeStampToMSecs(TTimeStamp(Buffer^));
          {$ENDIF}
        end;
      uftBlob, uftBlobId:
        begin
          if Buffer <> nil then
          begin
          {$IFDEF UNICODE}
            if SqlSubType = 1 then
            begin
              FStatement.ReadBlob(FieldNo, uniStr);
              TStream(Buffer).Write(PWideChar(uniStr)^, Length(uniStr) * sizeof(WideChar));
              TStream(Buffer).Seek(0, soFromBeginning);
            end else
          {$ENDIF}
            begin
              FStatement.ReadBlob(FieldNo, TStream(Buffer));
              TStream(Buffer).Seek(0, soFromBeginning);
            end;
          end;
        end;
      uftDate:
        {$IFNDEF FPC}
          PDouble(Buffer)^ := PInteger(sqldata)^ - DateOffset;
        {$ELSE}
          PInteger(Buffer)^ := PInteger(sqldata)^ - DateOffset + 693594;
        {$ENDIF}
      uftTime:
        {$IFNDEF FPC}
          PDouble(Buffer)^ := PCardinal(sqldata)^ / TimeCoeff;
        {$ELSE}
          PInteger(Buffer)^ := PCardinal(sqldata)^ div 10;
        {$ENDIF}
      uftInt64:
        {.$IFDEF FPC}
          //PInteger(Buffer)^ := PInt64(sqldata)^;
        {.$ELSE}
          PInt64(Buffer)^ := PInt64(sqldata)^;
        {.$ENDIF}
    {$IFDEF IB7_UP}
      uftBoolean:
        {$IFDEF FPC}
          Boolean(Buffer^) := PSmallInt(sqldata)^ = ISC_TRUE;
        {$ELSE}
          WordBool(Buffer^) := PSmallInt(sqldata)^ = ISC_TRUE;
        {$ENDIF}
    {$ENDIF}
    else
      raise EUIBError.Create(EUIB_UNEXPECTEDERROR);
    end;
  Result := True;
end;


procedure TUIBCustomDataSet.InternalGotoBookmark (Bookmark: Pointer);
var
  ReqBookmark: Integer;
begin
  ReqBookmark := Integer(Bookmark^);
    FCurrentRecord := ReqBookmark
end;

procedure TUIBCustomDataSet.InternalSetToRecord (Buffer: TRecordBuffer);
var
  ReqBookmark: Integer;
begin
  ReqBookmark := PUIBBookMark(Buffer + FRecordSize).Bookmark;
  InternalGotoBookmark (@ReqBookmark);
end;

function TUIBCustomDataSet.GetBookmarkFlag (
  Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PUIBBookMark(Buffer + FRecordSize).BookmarkFlag;
end;

procedure TUIBCustomDataSet.SetBookmarkFlag (Buffer: TRecordBuffer;
  Value: TBookmarkFlag);
begin
  PUIBBookMark(Buffer + FRecordSize).BookmarkFlag := Value;
end;

procedure TUIBCustomDataSet.InternalFirst;
begin
  FStatement.First;
  FIsFirst := not FStatement.Eof;
  FCurrentRecord := 0;
end;

procedure TUIBCustomDataSet.InternalLast;
begin
  FStatement.Last;
  FIsLast := True;
  FComplete := True;
  FCurrentRecord := FStatement.Fields.RecordCount - 1;
end;

procedure TUIBCustomDataSet.GetBookmarkData (
  Buffer: TRecordBuffer; Data: Pointer);
begin
  Integer(Data^) :=
    PUIBBookMark(Buffer + FRecordSize).Bookmark;
end;

procedure TUIBCustomDataSet.SetBookmarkData (
  Buffer: TRecordBuffer; Data: Pointer);
begin
  PUIBBookMark(Buffer + FRecordSize).Bookmark :=
    Integer(Data^);
end;

function TUIBCustomDataSet.GetRecordCount: Longint;
begin
  CheckActive;
  Result := FStatement.Fields.RecordCount;
end;

function TUIBCustomDataSet.GetRecNo: Longint;
begin
  UpdateCursorPos;
  Result := FCurrentRecord + 1;
end;

procedure TUIBCustomDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if (Value >= 1) and (Value <= FStatement.Fields.RecordCount) then
  begin
    FCurrentRecord := Value - 1;
    Resync([]);
  end;
end;

function TUIBCustomDataSet.GetRecord(Buffer: TRecordBuffer;
  GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  if (FCurrentRecord <> -1) and FStatement.CachedFetch and
   (FCurrentRecord < FStatement.Fields.RecordCount) then
      FStatement.Fields.CurrentRecord := FCurrentRecord;

  Result := grOK;

  case GetMode of
    gmNext:
      begin
        if FIsFirst then
        begin
          FIsFirst := False;
        end else
          begin
            if (FCurrentRecord < FStatement.Fields.RecordCount - 1) then
            begin
              FStatement.Fields.CurrentRecord := FCurrentRecord + 1;
              inc(FCurrentRecord);
            end else
              if not FComplete then
              begin
                if (FStatement.StatementType = stSelect) then
                begin
                  FStatement.Next;
                  if FStatement.Eof then
                  begin
                    Result := grEOF;
                    FComplete := True;
                  end else
                    inc(FCurrentRecord);
                end else
                begin
                  FComplete := true;
                  Result := grEOF;
                end;
              end else
               Result := grEOF;
          end;
      end;
    gmPrior:
      begin
        if FIsLast then
          FIsLast := False else
        if FStatement.Fields.CurrentRecord <= 0 then
          Result := grBOF else
          begin
            FStatement.Prior;
            dec(FCurrentRecord);
          end;
      end;
    gmCurrent:
      begin
        if (FCurrentRecord >= FStatement.Fields.RecordCount) then
          result := grError 
      end;
  end;

  PInteger(Buffer)^ := FCurrentRecord;
  with PUIBBookMark(Buffer + FRecordSize)^ do
  begin
    case Result of
      grOK:  BookmarkFlag := bfCurrent;
      grBOF: BookmarkFlag := bfBOF;
      grEOF: BookmarkFlag := bfEOF;
    end;
    Bookmark := PInteger (Buffer)^;
  end;
end;

procedure TUIBCustomDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FillChar(Buffer^, FRecordBufferSize, #0);
end;

procedure TUIBCustomDataSet.FreeRecordBuffer (var Buffer: TRecordBuffer);
begin
  FreeMem (Buffer);
end;

function TUIBCustomDataSet.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TUIBCustomDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  GetMem(Result, FRecordBufferSize);
end;

procedure TUIBCustomDataSet.InternalHandleException;
begin
end;

function TUIBCustomDataSet.GetOnError: TEndTransMode;
begin
  Result := FStatement.OnError;
end;

function TUIBCustomDataSet.GetSQL: TStrings;
begin
  Result := FStatement.SQL;
end;

function TUIBCustomDataSet.GetTransaction: TUIBTransaction;
begin
  Result := FStatement.Transaction;
end;

function TUIBCustomDataSet.GetUniDirectional: boolean;
begin
  Result := not FStatement.CachedFetch;
end;

procedure TUIBCustomDataSet.SetOnClose(const Value: TEndTransMode);
begin
  FOnClose := Value;
end;

procedure TUIBCustomDataSet.SetOnError(const Value: TEndTransMode);
begin
  FStatement.OnError := Value;
end;

procedure TUIBCustomDataSet.SetSQL(const Value: TStrings);
begin
  CheckInactive;
  FStatement.SQL.Assign(Value);
  DataEvent(dePropertyChange, 0);
end;

procedure TUIBCustomDataSet.SetTransaction(
  const Value: TUIBTransaction);
begin
  FStatement.Transaction := Value;
end;

procedure TUIBCustomDataSet.SetUniDirectional(const Value: boolean);
begin
{$IFDEF COMPILER6_UP}
  inherited SetUniDirectional(Value);
{$ENDIF}
  FStatement.CachedFetch := not Value;
end;

constructor TUIBCustomDataSet.Create(AOwner: TComponent);
begin
  FStatement := TUIBQuery.Create(nil);
  FStatement.OnClose := OnStatementClose;
  FOnClose := etmCommit;
  inherited Create(AOwner);
  FIsLast := False;
  FIsFirst := False;
end;

destructor TUIBCustomDataSet.Destroy;
begin
  inherited Destroy;
  FStatement.Free;
end;

procedure TUIBCustomDataSet.InternalInitFieldDefs;
var
  i: Integer;
{$IFDEF FPC}
  aName    : string;
  FieldNo  : Integer;
  Required : Boolean;
  DataType : TFieldType;
  Size     : Word;
  Precision: Integer;
{$ELSE}
  count  : Integer;
  TmpName: string;
{$ENDIF}
begin
  FStatement.Prepare;
  {$IFNDEF FPC}
  FieldDefs.BeginUpdate;
  {$ENDIF}
  FieldDefs.Clear;
  try
    for i := 0 to FStatement.Fields.FieldCount - 1 do
    with {$IFNDEF FPC} TUIBFieldDef.Create(FieldDefs),{$ENDIF} FStatement.Fields do
    begin
    {$IFNDEF FPC}
      count := 1;
      TmpName := AliasName[i];
      while TDefCollection(Collection).IndexOf(TmpName) >= 0 do
      begin
        TmpName := TmpName + inttostr(count);
        inc(count);
      end;
      Name := TmpName;
      FOrigin := RelName[i] + '.' + SqlName[i];
    {$ELSE}
      AName := AliasName[i];
      Precision:=-1;
      Size := 0;
    {$ENDIF}
      FieldNo := i;
      Required := not IsNullable[i];
      case FieldType[i] of
        uftNumeric:
          begin
            case SQLType[i] of
              SQL_SHORT:
                begin
                  {$IFDEF COMPILER6_UP}
                  if -Data.sqlvar[i].SqlScale > 4 then
                    DataType := ftFMTBcd else
                  {$ENDIF}
                  {$IFDEF FPC}
                  if -Data.sqlvar[i].SqlScale > 4 then
                    DataType := ftFloat else
                  {$ENDIF}
                  DataType := ftBCD;
                  Size := -Data.sqlvar[i].SqlScale;
                  if Size = 4 then
                    Precision := 5 else
                    Precision := 4;
                end;
              SQL_LONG:
                begin
                  {$IFDEF COMPILER6_UP}
                  if -Data.sqlvar[i].SqlScale > 4 then
                    DataType := ftFMTBcd else
                  {$ENDIF}
                  {$IFDEF FPC}
                  if -Data.sqlvar[i].SqlScale > 4 then
                    DataType := ftFloat else
                  {$ENDIF}
                    DataType := ftBCD;
                  Size := -Data.sqlvar[i].SqlScale;
                  if Size = 9 then
                    Precision := 10 else
                    Precision := 9;
                end;
              SQL_INT64,
              SQL_QUAD:
                begin
                  {$IFDEF COMPILER6_UP}
                  if -Data.sqlvar[i].SqlScale > 4 then
                    DataType := ftFMTBcd else
                  {$ENDIF}
                  {$IFDEF FPC}
                  if -Data.sqlvar[i].SqlScale > 4 then
                    DataType := ftFloat else
                  {$ENDIF}
                    DataType := ftBCD;
                  Size := -Data.sqlvar[i].SqlScale;
                  if Size = 18 then
                    Precision := 19 else
                    Precision := 18;
                end;
              SQL_D_FLOAT,
              SQL_DOUBLE:
                DataType := ftFloat; // possible
            end;
          end;
        uftChar,
        uftCstring,
        uftVarchar:
          begin
          {$IFDEF UNICODE}
            DataType := ftWideString;
            Size := SQLLen[i] div BytesPerCharacter[CharacterSet];
          {$ELSE}
            DataType := ftString;
            Size := SQLLen[i];
          {$ENDIF}
          end;
        uftSmallint: DataType := ftSmallint;
        uftInteger : DataType := ftInteger;
        uftFloat,
        uftDoublePrecision: DataType := ftFloat;
        uftTimestamp: DataType := ftDateTime;
        uftBlob, uftBlobId:
          begin
            if Data.sqlvar[i].SqlSubType = 1 then
            {$IFDEF UNICODE}
              DataType := ftWideMemo else
            {$ELSE}
              DataType := ftMemo else
            {$ENDIF}
              DataType := ftBlob;
            Size := SizeOf(Pointer);
          end;
        uftDate : DataType := ftDate;
        uftTime : DataType := ftTime;
        uftInt64: DataType := ftLargeint;
      {$IFDEF IB7_UP}
        uftBoolean: DataType := ftBoolean;
      {$ENDIF}
      else
        DataType := ftUnknown;
      end;

      {$IFDEF FPC}
      //Add new defs
      FieldDefs.Add(aName,DataType,Size,Required);
      //If Precision is specified, update the definition
      if Precision<>-1 then
          FieldDefs.Items[FieldNo].Precision:=Precision;
      {$ENDIF}
    end; 
  finally
    {$IFNDEF FPC}
    FieldDefs.EndUpdate;
    {$ENDIF}
  end;
end;

function TUIBCustomDataSet.GetFieldData(FieldNo: Integer;
  Buffer: Pointer): Boolean;
begin
  Result := InternalGetFieldData(FieldNo, Buffer, True);
end;

function TUIBCustomDataSet.GetFieldData(Field: TField;
  Buffer: Pointer): Boolean;
begin
  CheckActive;
  Result := GetFieldData(Field.FieldNo, Buffer);
end;

function TUIBCustomDataSet.GetCanModify: Boolean;
begin
  Result := False;
end;

procedure TUIBCustomDataSet.OnStatementClose(Sender: TObject);
begin
  Close;
end;

function TUIBCustomDataSet.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  if (Mode = bmRead) then
  begin
    Result := TMemoryStream.Create;
    GetFieldData(Field, Result);
  end else
    Result := nil;
end;

function TUIBCustomDataSet.GetFetchBlobs: boolean;
begin
  Result := FStatement.FetchBlobs;
end;

procedure TUIBCustomDataSet.SetFetchBlobs(const Value: boolean);
begin
  FStatement.FetchBlobs := Value;
end;

procedure TUIBCustomDataSet.Execute;
begin
  FStatement.Execute;
end;

procedure TUIBCustomDataSet.ExecSQL;
begin
  FStatement.ExecSQL;
end;

{$IFNDEF FPC}
procedure TUIBCustomDataSet.SetActive(Value: Boolean);
begin
  inherited;
  if not Value then
    FStatement.Close(FOnClose);
end;
{$ENDIF}

{$IFNDEF COMPILER5_UP}
function TUIBCustomDataSet.BCDToCurr(BCD: Pointer;
  var Curr: Currency): Boolean;
begin
  Curr := PCurrency(BCD)^;
  result := True;
end;

function TUIBCustomDataSet.CurrToBCD(const Curr: Currency; BCD: Pointer;
  Precision, Decimals: Integer): Boolean;
begin
  PCurrency(BCD)^ := Curr;
  Result := True;
end;
{$ENDIF}

procedure TUIBCustomDataSet.SetDatabase(const Value: TUIBDataBase);
begin
  FStatement.DataBase := Value;
end;

function TUIBCustomDataSet.GetDatabase: TUIBDataBase;
begin
  Result := FStatement.DataBase;
end;

procedure TUIBCustomDataSet.ParamsSetBlob(const Name: string;
  Stream: TStream);
begin
  FStatement.ParamsSetBlob(Name, Stream);
end;

procedure TUIBCustomDataSet.ParamsSetBlobA(const Name: string;
  var str: AnsiString);
begin
  FStatement.ParamsSetBlobA(Name, str);
end;

procedure TUIBCustomDataSet.ParamsSetBlobB(const Index: Word;
  var str: RawByteString);
begin
  FStatement.ParamsSetBlobB(Index, str);
end;

procedure TUIBCustomDataSet.ParamsSetBlobB(const Name: string;
  var str: RawByteString);
begin
  FStatement.ParamsSetBlobB(Name, str);
end;

procedure TUIBCustomDataSet.ParamsSetBlob(const Name: string;
  Buffer: Pointer; Size: Word);
begin
  FStatement.ParamsSetBlob(Name, Buffer, Size);
end;

procedure TUIBCustomDataSet.ParamsSetBlobW(const Index: Word;
  var str: UnicodeString);
begin
  FStatement.ParamsSetBlobW(Index, str);
end;

procedure TUIBCustomDataSet.ParamsSetBlob(const Index: Word; var str: string);
begin
{$IFDEF UNICODE}
  ParamsSetBlobW(Index, str);
{$ELSE}
  ParamsSetBlobA(Index, str);
{$ENDIF}
end;

procedure TUIBCustomDataSet.ParamsSetBlobW(const Name: string;
  var str: UnicodeString);
begin
  FStatement.ParamsSetBlobW(Name, str);
end;

procedure TUIBCustomDataSet.ParamsSetBlob(const Name: string; var str: string);
begin
{$IFDEF UNICODE}
  ParamsSetBlobW(Name, str);
{$ELSE}
  ParamsSetBlobA(Name, str);
{$ENDIF}
end;

procedure TUIBCustomDataSet.ParamsSetBlob(const Index: Word;
  Stream: TStream);
begin
  FStatement.ParamsSetBlob(Index, Stream);
end;

procedure TUIBCustomDataSet.ParamsSetBlobA(const Index: Word;
  var str: AnsiString);
begin
  FStatement.ParamsSetBlobA(Index, str);
end;

procedure TUIBCustomDataSet.ParamsSetBlob(const Index: Word;
  Buffer: Pointer; Size: Word);
begin
  FStatement.ParamsSetBlob(Index, Buffer, Size);
end;

procedure TUIBCustomDataSet.ReadBlob(const name: string;
  Stream: TStream);
begin
  FStatement.ReadBlob(name, Stream);
end;

procedure TUIBCustomDataSet.ReadBlobA(const name: string; var str: AnsiString);
begin
  FStatement.ReadBlobA(name, str);
end;

procedure TUIBCustomDataSet.ReadBlobB(const name: string;
  var str: RawByteString);
begin
  FStatement.ReadBlobB(name, str);
end;

procedure TUIBCustomDataSet.ReadBlobB(const Index: Word;
  var str: RawByteString);
begin
  FStatement.ReadBlobB(Index, str);
end;

procedure TUIBCustomDataSet.ReadBlobW(const name: string; var str: UnicodeString);
begin
  FStatement.ReadBlobW(name, str);
end;

procedure TUIBCustomDataSet.ReadBlob(const name: string; var str: string);
begin
{$IFDEF UNICODE}
  ReadBlobW(name, str);
{$ELSE}
  ReadBlobA(name, str);
{$ENDIF}
end;

procedure TUIBCustomDataSet.ReadBlob(const name: string; var Value: Variant);
begin
  FStatement.ReadBlob(name, Value);
end;

procedure TUIBCustomDataSet.ReadBlob(const Index: Word; Stream: TStream);
begin
  FStatement.ReadBlob(Index, Stream);
end;

procedure TUIBCustomDataSet.ReadBlobA(const Index: Word; var str: AnsiString);
begin
  FStatement.ReadBlobA(Index, str);
end;

procedure TUIBCustomDataSet.ReadBlobW(const Index: Word; var str: UnicodeString);
begin
  FStatement.ReadBlobW(Index, str);
end;

procedure TUIBCustomDataSet.ReadBlob(const Index: Word; var str: string);
begin
{$IFDEF UNICODE}
  ReadBlobW(Index, str);
{$ELSE}
  ReadBlobA(Index, str);
{$ENDIF}
end;

procedure TUIBCustomDataSet.ReadBlob(const Index: Word;
  var Value: Variant);
begin
  FStatement.ReadBlob(Index, Value);
end;

function TUIBCustomDataSet.GetParams: TSQLParams;
begin
  Result := FStatement.Params;
end;

function TUIBCustomDataSet.GetInternalFields: TSQLResult;
begin
  Result := FStatement.Fields;
end;

function TUIBCustomDataSet.GetBufferChunks: Cardinal;
begin
  Result := FStatement.BufferChunks;
end;

procedure TUIBCustomDataSet.SetBufferChunks(const Value: Cardinal);
begin
  FStatement.BufferChunks := Value;
end;

function TUIBCustomDataSet.GetRowsAffected: Cardinal;
begin
  Result := FStatement.RowsAffected;
end;

procedure TUIBCustomDataSet.InternalRefresh;
var RecCount: Integer;
begin
  if FStatement.Fields <> nil then
    RecCount := FStatement.Fields.RecordCount else
    RecCount := 0;
  FStatement.Open;
  While (RecCount > 1) and not FStatement.Eof do
  begin
    FStatement.Next;
    dec(RecCount);
  end;
end;

{$IFNDEF FPC}
procedure TUIBCustomDataSet.CreateFields;
var
  i : Integer;
  fd: TFieldDef;
begin
  inherited;
  for i := 0 to Fields.Count - 1 do
    with Fields.Fields[i] do
    begin
      fd := FieldDefList.Find(FieldName);
      if (fd <> nil) and (fd is TUIBFieldDef) then
        Origin := TUIBFieldDef(fd).Origin;
    end;
end;
{$ENDIF}

procedure TUIBCustomDataSet.BuildStoredProc(const StoredProc: string;
  forSelect: boolean);
begin
  Close;
  FStatement.BuildStoredProc(StoredProc, forSelect);
end;

{$IFNDEF FPC}
function TUIBCustomDataSet.GetFieldClass(
  FieldType: TFieldType): TFieldClass;
begin
  if FieldType = ftBCD then
    result := TUIBBCDField else
    result := inherited GetFieldClass(FieldType);
end;
{$ENDIF}

{$IFNDEF FPC}
function TUIBCustomDataSet.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
begin
  Result := InternalGetFieldData(Field.FieldNo, Buffer, NativeFormat)
end;
{$ENDIF}

{ TUIBBCDField }

{$IFNDEF FPC}
constructor TUIBBCDField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Size := 8;
end;

class procedure TUIBBCDField.CheckTypeSize(Value: Integer);
begin
end;

function TUIBBCDField.GetAsCurrency: Currency;
begin
  if not GetValue(Result) then
    Result := 0;
end;

function TUIBBCDField.GetAsString: string;
var
  curr: System.Currency;
begin
  if GetValue(curr) then
    Result := CurrToStr(curr) else
    Result := '';
end;

function TUIBBCDField.GetAsVariant: Variant;
var
  curr: System.Currency;
begin
  if GetValue(curr) then
    Result := curr else
    Result := Null;
end;

function TUIBBCDField.GetDataSize: Integer;
begin
  Result := 8;
end;
{$ENDIF}

end.
