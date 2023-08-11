unit ALTestRtti;

interface

uses
  System.Classes,
  System.Diagnostics,
  System.Types,
  Alcinoe.Common,
  Alcinoe.RTTI,
  Alcinoe.StringUtils,
  DUnitX.TestFramework;

{$RTTI EXPLICIT METHODS([vcPublic, vcPublished])
                FIELDS([vcPublic, vcPublished])
                PROPERTIES([vcProtected, vcPublic, vcPublished])}

type

  [TestFixture]
  TALTestRtti = class
  strict private
    fStopWatchAlcinoe: TStopwatch;
    fStopWatchDELPHI: TStopwatch;
    procedure CheckExecutionTime(const ARatio: single = 1.2);
  public
    [Setup]
    procedure Setup;
    //[TearDown]
    //procedure TearDown;
    [Test]
    procedure TestALRttiAutoInit;
    procedure onClick(Sender: TObject);
  end;

  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom);

  TAlignSet = set of TAlign;

  TALAutoInitChildObject = class;

  TALAutoInitObject = class(TObject)
  private
    FDateTimeValue2: TDateTime;
    FAlignValues2: TAlignSet;
    FInt64Value2: Int64;
    FSingleValue2: Single;
    FStringValue2: String;
    FAlignValue2: TAlign;
    FStringList2: TStringList;
    FAnsiStringValue2: ansiString;
    FOnclick2: TnotifyEvent;
    FDoubleValue2: Double;
    FCharValue2: Char;
    FChildObject2: TALAutoInitChildObject;
    FBooleanValue2: Boolean;
    FInt32Value2: Int32;
    FAnsiCharValue2: ansiChar;
    fOwner2: TALTestRtti;
    fOwner3: TALTestRtti;
    FDateTimeValue3: TDateTime;
    FAlignValues3: TAlignSet;
    FInt64Value3: Int64;
    FSingleValue3: Single;
    FStringValue3: String;
    FAlignValue3: TAlign;
    FStringList3: TStringList;
    FChildObject3: TALAutoInitChildObject;
    FAnsiStringValue3: ansiString;
    FOnclick3: TnotifyEvent;
    FDoubleValue3: Double;
    FCharValue3: Char;
    FBooleanValue3: Boolean;
    FInt32Value3: Int32;
    FAnsiCharValue3: ansiChar;
    procedure SetAlignValue2(const Value: TAlign);
    procedure SetAlignValues2(const Value: TAlignSet);
    procedure SetAnsiCharValue2(const Value: ansiChar);
    procedure SetAnsiStringValue2(const Value: ansiString);
    procedure SetBooleanValue2(const Value: Boolean);
    procedure SetCharValue2(const Value: Char);
    procedure SeTALAutoInitChildObject2(const Value: TALAutoInitChildObject);
    procedure SetDateTimeValue2(const Value: TDateTime);
    procedure SetDoubleValue2(const Value: Double);
    procedure SetInt32Value2(const Value: Int32);
    procedure SetInt64Value2(const Value: Int64);
    procedure SetOnclick2(const Value: TnotifyEvent);
    procedure SetSingleValue2(const Value: Single);
    procedure SetStringList2(const Value: TStringList);
    procedure SetStringValue2(const Value: String);
    function GetOwner2: TALTestRtti;
  public
    AutoInit: Boolean;
    Owner: TALTestRtti;
    property Owner2: TALTestRtti read GetOwner2;
    property Owner3: TALTestRtti read fOwner3;
  public
    //----
    //auto init of member fields
    //----
    [TALInit('B')]
    CharValue: Char;
    [TALInit('e')]
    AnsiCharValue: ansiChar;
    [TALInit('abcdefg')]
    StringValue: String;
    [TALInit('abcdefg')]
    AnsiStringValue: ansiString;
    [TALInit('64')]
    Int64Value: Int64;
    [TALInit('32')]
    Int32Value: Int32;
    [TALInit('3.14159265359')]
    SingleValue: Single;
    [TALInit('3.14159265359')]
    DoubleValue: Double;
    [TALInit('now')]
    DateTimeValue: TDateTime;
    [TALInit('true')]
    BooleanValue: Boolean;
    [TALInit('alRight')]
    AlignValue: TAlign;
    [TALInit('[alTop,alRight,alClient]')]
    AlignValues: TAlignSet;
    [TALInit('QuoteChar:A;NameValueSeparator:B;Options:[soStrictDelimiter, soWriteBOM]')]
    StringList: TStringList;
    [TALInit('Owner.OnClick')]
    Onclick: TnotifyEvent;
    [TALInit('left:50;right:75')]
    Rect: TRect;
    [TALInit('')]
    ChildObject: TALAutoInitChildObject;
 public
    //----
    //auto init of property fields with setter
    //----
    [TALInit('B')]
    property CharValue2: Char read FCharValue2 write SetCharValue2;
    [TALInit('e')]
    property AnsiCharValue2: ansiChar read FAnsiCharValue2 write SetAnsiCharValue2;
    [TALInit('abcdefg')]
    property StringValue2: String read FStringValue2 write SetStringValue2;
    [TALInit('abcdefg')]
    property AnsiStringValue2: ansiString read FAnsiStringValue2 write SetAnsiStringValue2;
    [TALInit('64')]
    property Int64Value2: Int64 read FInt64Value2 write SetInt64Value2;
    [TALInit('32')]
    property Int32Value2: Int32 read FInt32Value2 write SetInt32Value2;
    [TALInit('3.14159265359')]
    property SingleValue2: Single read FSingleValue2 write SetSingleValue2;
    [TALInit('3.14159265359')]
    property DoubleValue2: Double read FDoubleValue2 write SetDoubleValue2;
    [TALInit('now')]
    property DateTimeValue2: TDateTime read FDateTimeValue2 write SetDateTimeValue2;
    [TALInit('true')]
    property BooleanValue2: Boolean read FBooleanValue2 write SetBooleanValue2;
    [TALInit('alRight')]
    property AlignValue2: TAlign read FAlignValue2 write SetAlignValue2;
    [TALInit('[alTop,alRight,alClient]')]
    property AlignValues2: TAlignSet read FAlignValues2 write SetAlignValues2;
    [TALInit('QuoteChar:A;NameValueSeparator:B;Options:[soStrictDelimiter, soWriteBOM]')]
    property StringList2: TStringList read FStringList2 write SetStringList2;
    [TALInit('Owner2.OnClick')]
    property Onclick2: TnotifyEvent read FOnclick2 write SetOnclick2;
    [TALInit('')]
    property ChildObject2: TALAutoInitChildObject read FChildObject2 write SeTALAutoInitChildObject2;
  public
    //----
    //auto init of property fields without setter
    //----
    [TALInit('B')]
    property CharValue3: Char read FCharValue3 write fCharValue3;
    [TALInit('e')]
    property AnsiCharValue3: ansiChar read FAnsiCharValue3 write fAnsiCharValue3;
    [TALInit('abcdefg')]
    property StringValue3: String read FStringValue3 write fStringValue3;
    [TALInit('abcdefg')]
    property AnsiStringValue3: ansiString read FAnsiStringValue3 write fAnsiStringValue3;
    [TALInit('64')]
    property Int64Value3: Int64 read FInt64Value3 write fInt64Value3;
    [TALInit('32')]
    property Int32Value3: Int32 read FInt32Value3 write fInt32Value3;
    [TALInit('3.14159265359')]
    property SingleValue3: Single read FSingleValue3 write fSingleValue3;
    [TALInit('3.14159265359')]
    property DoubleValue3: Double read FDoubleValue3 write fDoubleValue3;
    [TALInit('now')]
    property DateTimeValue3: TDateTime read FDateTimeValue3 write fDateTimeValue3;
    [TALInit('true')]
    property BooleanValue3: Boolean read FBooleanValue3 write fBooleanValue3;
    [TALInit('alRight')]
    property AlignValue3: TAlign read FAlignValue3 write fAlignValue3;
    [TALInit('[alTop,alRight,alClient]')]
    property AlignValues3: TAlignSet read FAlignValues3 write fAlignValues3;
    [TALInit('QuoteChar:A;NameValueSeparator:B;Options:[soStrictDelimiter, soWriteBOM]')]
    property StringList3: TStringList read FStringList3 write fStringList3;
    [TALInit('Owner2.OnClick')]
    property Onclick3: TnotifyEvent read FOnclick3 write FOnclick3;
    [TALInit('')]
    property ChildObject3: TALAutoInitChildObject read FChildObject3 write fChildObject3;
  public
    constructor Create(const aOwner: TALTestRtti; const AAutoInit: Boolean); virtual;
    destructor Destroy; override;
  End;

  [TALInit('BooleanValue3:false;Int32Value:52')]
  TALAutoInitObject2 = class(TALAutoInitObject)
  private
  public
    [TALInit('alLeft')]
    property AlignValue2;
   public
    constructor Create(const aOwner: TALTestRtti; const AAutoInit: Boolean); override;
  End;

  [TALInit('Int32Value:75')]
  TALAutoInitObject3 = class(TALAutoInitObject2)
  private
  public
    [TALInit('altop')]
    property AlignValue2;
   public
    constructor Create(const aOwner: TALTestRtti; const AAutoInit: Boolean); override;
  End;

  TALAutoInitChildObject = class(TObject)
  private
    fOwner: TALAutoInitObject;
  public
    constructor Create(const aOwner: TALAutoInitObject); virtual;
  End;

implementation

uses
  system.SysUtils,
  system.DateUtils,
  System.Math;

{**************************}
procedure TALTestRtti.Setup;
begin
  fStopWatchAlcinoe := TStopwatch.Create;
  fStopWatchDELPHI := TStopwatch.Create;
end;

{*******************************************************************}
procedure TALTestRtti.CheckExecutionTime(const ARatio: single = 1.2);
begin
  {$IF defined(debug) or defined(Win32)}
  //In debug we have overflow checking and range checking so that mean
  //that the execution time will be much slower that the Delphi RTL so skip it
  //In Win32 we remove all ASM (fastcode heritage) than the delphi RTL have
  //so we will be othen much more slower than the Delphi RTL
  Writeln(ALFormatW('CheckExecutionTime Skipped - %0.0f ms for Alcinoe vs %0.0f ms for Delphi (%0.1fx faster)', [fStopWatchAlcinoe.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds / fStopWatchAlcinoe.Elapsed.TotalMilliseconds], ALDefaultFormatSettingsW));
  {$ELSE}
  if fStopWatchAlcinoe.Elapsed.TotalMilliseconds > fStopWatchDELPHI.Elapsed.TotalMilliseconds * ARatio then
    Assert.Fail(ALFormatW('Time too long (%0.0f ms for Alcinoe vs %0.0f ms for Delphi)', [fStopWatchAlcinoe.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds], ALDefaultFormatSettingsW))
  else
    //https://github.com/VSoftTechnologies/DUnitX/issues/319
    Writeln(ALFormatW('%0.0f ms for Alcinoe vs %0.0f ms for Delphi (%0.1fx faster)', [fStopWatchAlcinoe.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds / fStopWatchAlcinoe.Elapsed.TotalMilliseconds], ALDefaultFormatSettingsW));
  {$ENDIF}
end;

{***************************************}
procedure TALTestRtti.TestALRttiAutoInit;
begin
  //--
  for var I := 0 to 100000 do begin
    fStopWatchAlcinoe.Start; {-}
      var LAutoInitObject := TALAutoInitObject.create(self, True);
      LAutoInitObject.Free;
      var LAutoInitObject2 := TALAutoInitObject2.create(self, True);
      LAutoInitObject2.Free;
      var LAutoInitObject3 := TALAutoInitObject3.create(self, True);
      LAutoInitObject3.Free;
      fStopWatchAlcinoe.Stop;
  end;
  //--
  for var I := 0 to 100000 do begin
    fStopWatchDelphi.Start; {-}
      var LAutoInitObject := TALAutoInitObject.create(self, false);
      LAutoInitObject.Free;
      var LAutoInitObject2 := TALAutoInitObject2.create(self, false);
      LAutoInitObject2.Free;
      var LAutoInitObject3 := TALAutoInitObject3.create(self, false);
      LAutoInitObject3.Free;
      fStopWatchDelphi.Stop;
  end;
  //--
  CheckExecutionTime(1.20{ARatio});
end;

{*********************************************}
procedure TALTestRtti.onClick(Sender: TObject);
begin
 //nothing to do here
end;

{****************************************************************************************}
constructor TALAutoInitObject.create(const aOwner: TALTestRtti; const AAutoInit: Boolean);
begin
  Owner := aOwner;
  FOwner2 := aOwner;
  FOwner3 := aOwner;
  AutoInit := AAutoInit;
  if AAutoInit then ALRttiInitializeInstance(self)
  else begin
    CharValue := 'B';
    AnsiCharValue := 'e';
    StringValue := 'abcdefg';
    AnsiStringValue := 'abcdefg';
    Int64Value := 64;
    Int32Value := 32;
    SingleValue := 3.14159265359;
    DoubleValue := 3.14159265359;
    DateTimeValue := now;
    BooleanValue := true;
    AlignValue := TAlign.alRight;
    AlignValues := [alTop,alRight,alClient];
    StringList:= TStringList.create;
    StringList.QuoteChar:='A';
    StringList.NameValueSeparator := 'B';
    StringList.Options := [soStrictDelimiter, soWriteBOM];
    Onclick := Owner.OnClick;
    Rect.left := 50;
    Rect.right := 75;
    ChildObject := TALAutoInitChildObject.create(self);
    //----
    CharValue2 := 'B';
    AnsiCharValue2 := 'e';
    StringValue2 := 'abcdefg';
    AnsiStringValue2 := 'abcdefg';
    Int64Value2 := 64;
    Int32Value2 := 32;
    SingleValue2 := 3.14159265359;
    DoubleValue2 := 3.14159265359;
    DateTimeValue2 := now;
    BooleanValue2 := true;
    AlignValue2 := TAlign.alRight;
    AlignValues2 := [alTop,alRight,alClient];
    StringList2 := TStringList.create;
    StringList2.QuoteChar:='A';
    StringList2.NameValueSeparator := 'B';
    StringList2.Options := [soStrictDelimiter, soWriteBOM];
    Onclick2 := Owner.OnClick;
    ChildObject2 := TALAutoInitChildObject.create(self);
    //----
    CharValue3 := 'B';
    AnsiCharValue3 := 'e';
    StringValue3 := 'abcdefg';
    AnsiStringValue3 := 'abcdefg';
    Int64Value3 := 64;
    Int32Value3 := 32;
    SingleValue3 := 3.14159265359;
    DoubleValue3 := 3.14159265359;
    DateTimeValue3 := now;
    BooleanValue3 := true;
    AlignValue3 := TAlign.alRight;
    AlignValues3 := [alTop,alRight,alClient];
    StringList3 := TStringList.create;
    StringList3.QuoteChar:='A';
    StringList3.NameValueSeparator := 'B';
    StringList3.Options := [soStrictDelimiter, soWriteBOM];
    Onclick3 := Owner.OnClick;
    ChildObject3 := TALAutoInitChildObject.create(self);
  end;
  //--
  if self.ClassType = TALAutoInitObject  then begin
    if CharValue <> 'B' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if AnsiCharValue <> 'e' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringValue <> 'abcdefg' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if AnsiStringValue <> 'abcdefg' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Int64Value <> 64 then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Int32Value <> 32 then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Not SameValue(SingleValue, single(3.14159265359)) then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Not SameValue(DoubleValue, 3.14159265359) then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Not SameDate(DateTimeValue, now) then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if BooleanValue <> true then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if AlignValue <> TAlign.alRight then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if AlignValues <> [alTop,alRight,alClient] then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringList.classname <> 'TStringList' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringList.QuoteChar<>'A' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringList.NameValueSeparator <> 'B' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringList.Options <> [soStrictDelimiter, soWriteBOM] then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    Onclick(self);
    if Rect.left <> 50 then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Rect.right <> 75 then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if ChildObject.classname <> 'TALAutoInitChildObject' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    //----
    if CharValue2 <> 'B' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if AnsiCharValue2 <> 'e' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringValue2 <> 'abcdefg' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if AnsiStringValue2 <> 'abcdefg' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Int64Value2 <> 64 then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Int32Value2 <> 32 then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Not SameValue(SingleValue2, single(3.14159265359)) then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Not SameValue(DoubleValue2, 3.14159265359) then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Not SameDate(DateTimeValue2, now) then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if BooleanValue2 <> true then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if AlignValue2 <> TAlign.alRight then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if AlignValues2 <> [alTop,alRight,alClient] then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringList2.classname <> 'TStringList' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringList2.QuoteChar<>'A' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringList2.NameValueSeparator <> 'B' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringList2.Options <> [soStrictDelimiter, soWriteBOM] then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    Onclick2(self);
    if ChildObject2.classname <> 'TALAutoInitChildObject' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    //----
    if CharValue3 <> 'B' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if AnsiCharValue3 <> 'e' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringValue3 <> 'abcdefg' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if AnsiStringValue3 <> 'abcdefg' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Int64Value3 <> 64 then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Int32Value3 <> 32 then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Not SameValue(SingleValue3, single(3.14159265359)) then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Not SameValue(DoubleValue3, 3.14159265359) then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if Not SameDate(DateTimeValue3, now) then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if BooleanValue3 <> true then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if AlignValue3 <> TAlign.alRight then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if AlignValues3 <> [alTop,alRight,alClient] then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringList3.classname <> 'TStringList' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringList3.QuoteChar<>'A' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringList3.NameValueSeparator <> 'B' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    if StringList3.Options <> [soStrictDelimiter, soWriteBOM] then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
    Onclick3(self);
    if ChildObject3.classname <> 'TALAutoInitChildObject' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
  end;
end;

{***********************************}
destructor TALAutoInitObject.Destroy;
begin
  if AutoInit then ALRttiFinalizeInstance(self)
  else begin
    ALFreeandNil(StringList);
    ALFreeandNil(ChildObject);
    ALFreeandNil(StringList2);
    ALFreeandNil(ChildObject2);
    ALFreeandNil(StringList3);
    ALFreeandNil(ChildObject3);
  end;
  inherited;
end;

{************************************************}
function TALAutoInitObject.GetOwner2: TALTestRtti;
begin
  result := FOwner2;
end;

{**************************************************************}
procedure TALAutoInitObject.SetAlignValue2(const Value: TAlign);
begin
  FAlignValue2 := Value;
end;

{******************************************************************}
procedure TALAutoInitObject.SetAlignValues2(const Value: TAlignSet);
begin
  FAlignValues2 := Value;
end;

{*******************************************************************}
procedure TALAutoInitObject.SetAnsiCharValue2(const Value: ansiChar);
begin
  FAnsiCharValue2 := Value;
end;

{***********************************************************************}
procedure TALAutoInitObject.SetAnsiStringValue2(const Value: ansiString);
begin
  FAnsiStringValue2 := Value;
end;

{*****************************************************************}
procedure TALAutoInitObject.SetBooleanValue2(const Value: Boolean);
begin
  FBooleanValue2 := Value;
end;

{***********************************************************}
procedure TALAutoInitObject.SetCharValue2(const Value: Char);
begin
  FCharValue2 := Value;
end;

{*****************************************************************************************}
procedure TALAutoInitObject.SeTALAutoInitChildObject2(const Value: TALAutoInitChildObject);
begin
  FChildObject2 := Value;
end;

{********************************************************************}
procedure TALAutoInitObject.SetDateTimeValue2(const Value: TDateTime);
begin
  FDateTimeValue2 := Value;
end;

{***************************************************************}
procedure TALAutoInitObject.SetDoubleValue2(const Value: Double);
begin
  FDoubleValue2 := Value;
end;

{*************************************************************}
procedure TALAutoInitObject.SetInt32Value2(const Value: Int32);
begin
  FInt32Value2 := Value;
end;

{*************************************************************}
procedure TALAutoInitObject.SetInt64Value2(const Value: Int64);
begin
  FInt64Value2 := Value;
end;

{*****************************************************************}
procedure TALAutoInitObject.SetOnclick2(const Value: TnotifyEvent);
begin
  FOnclick2 := Value;
end;

{***************************************************************}
procedure TALAutoInitObject.SetSingleValue2(const Value: Single);
begin
  FSingleValue2 := Value;
end;

{*******************************************************************}
procedure TALAutoInitObject.SetStringList2(const Value: TStringList);
begin
  FStringList2 := Value;
end;

{***************************************************************}
procedure TALAutoInitObject.SetStringValue2(const Value: String);
begin
  FStringValue2 := Value;
end;

{*****************************************************************************************}
constructor TALAutoInitObject2.Create(const aOwner: TALTestRtti; const AAutoInit: Boolean);
begin
  inherited create(aOwner, AAutoInit);
  if not AAutoInit then begin
    AlignValue2 := TAlign.alLeft;
    BooleanValue3 := false;
    Int32Value := 52;
  end;
  //--
  if self.ClassType = TALAutoInitObject2 then begin
    if AlignValue2 <> TAlign.alLeft then raise Exception.create('Error D09DA31D-11BF-4404-BF0A-92D38B82D23D');
    if BooleanValue3 <> false then raise Exception.create('Error D09DA31D-11BF-4404-BF0A-92D38B82D23D');
    if Int32Value <> 52 then raise Exception.create('Error D09DA31D-11BF-4404-BF0A-92D38B82D23D');
  end;
end;

{*****************************************************************************************}
constructor TALAutoInitObject3.Create(const aOwner: TALTestRtti; const AAutoInit: Boolean);
begin
  inherited create(aOwner, AAutoInit);
  if not AAutoInit then begin
    AlignValue2 := ALTop;
    Int32Value := 75;
  end;
  //--
  if self.ClassType = TALAutoInitObject3 then begin
    if AlignValue2 <> altop then raise Exception.create('Error D09DA31D-11BF-4404-BF0A-92D38B82D23D');
    if Int32Value <> 75 then raise Exception.create('Error D09DA31D-11BF-4404-BF0A-92D38B82D23D');
  end;
end;

{*************************************************************************}
constructor TALAutoInitChildObject.Create(const aOwner: TALAutoInitObject);
begin
  if aOwner = nil then
    raise Exception.Create('Error CCD0F741-D468-4CD1-A94F-9E5F8F680643');
  fOwner := aOwner;
end;

initialization
  ALRttiInitialization(
    ['ALTestRtti.*',
     'DUnitX.TestFramework.TLogLevel',
     'System.AnsiChar',
     'System.AnsiString',
     'System.Boolean',
     'System.Byte',
     'System.Cardinal',
     'System.Char',
     'System.Classes.IInterfaceList',
     'System.Classes.IStringsAdapter',
     'System.Classes.TAsyncConstArrayFunctionEvent',
     'System.Classes.TAsyncConstArrayProc',
     'System.Classes.TAsyncConstArrayProcedureEvent',
     'System.Classes.TAsyncFunctionEvent',
     'System.Classes.TAsyncProcedureEvent',
     'System.Classes.TBasicAction',
     'System.Classes.TBasicActionLink',
     'System.Classes.TComponent',
     'System.Classes.TComponentEnumerator',
     'System.Classes.TComponentName',
     'System.Classes.TComponentState',
     'System.Classes.TComponentStyle',
     'System.Classes.TGetDeltaStreamsEvent',
     'System.Classes.TNotifyEvent',
     'System.Classes.TObservers',
     'System.Classes.TObservers.TCanObserveEvent',
     'System.Classes.TObservers.TObserverAddedEvent',
     'System.Classes.TOperation',
     'System.Classes.TPersistent',
     'System.Classes.TSeekOrigin',
     'System.Classes.TStream',
     'System.Classes.TStringItemList',
     'System.Classes.TStringList',
     'System.Classes.TStringList.TOverridden',
     'System.Classes.TStringListSortCompare',
     'System.Classes.TStrings',
     'System.Classes.TStringsEnumerator',
     'System.Classes.TStringsOptions',
     'System.Double',
     'System.Extended',
     'System.Generics.Collections.TCollectionNotifyEvent<System.Classes.IInterfaceList>',
     'System.Generics.Collections.TCollectionNotifyEvent<System.Classes.TBasicActionLink>',
     'System.Generics.Collections.TCollectionNotifyEvent<System.Classes.TComponent>',
     'System.Generics.Collections.TCollectionNotifyEvent<System.Integer>',
     'System.Generics.Collections.TDictionary<System.Integer,System.Classes.IInterfaceList>',
     'System.Generics.Collections.TDictionary<System.Integer,System.Classes.IInterfaceList>.TItemArray',
     'System.Generics.Collections.TDictionary<System.Integer,System.Classes.IInterfaceList>.TKeyCollection',
     'System.Generics.Collections.TDictionary<System.Integer,System.Classes.IInterfaceList>.TKeyEnumerator',
     'System.Generics.Collections.TDictionary<System.Integer,System.Classes.IInterfaceList>.TPairEnumerator',
     'System.Generics.Collections.TDictionary<System.Integer,System.Classes.IInterfaceList>.TValueCollection',
     'System.Generics.Collections.TDictionary<System.Integer,System.Classes.IInterfaceList>.TValueEnumerator',
     'System.Generics.Collections.TEnumerable<System.Classes.IInterfaceList>',
     'System.Generics.Collections.TEnumerable<System.Classes.TBasicActionLink>',
     'System.Generics.Collections.TEnumerable<System.Classes.TComponent>',
     'System.Generics.Collections.TEnumerable<System.Generics.Collections.TPair<System.Integer,System.Classes.IInterfaceList>>',
     'System.Generics.Collections.TEnumerable<System.Integer>',
     'System.Generics.Collections.TEnumerator<System.Classes.IInterfaceList>',
     'System.Generics.Collections.TEnumerator<System.Classes.TBasicActionLink>',
     'System.Generics.Collections.TEnumerator<System.Classes.TComponent>',
     'System.Generics.Collections.TEnumerator<System.Generics.Collections.TPair<System.Integer,System.Classes.IInterfaceList>>',
     'System.Generics.Collections.TEnumerator<System.Integer>',
     'System.Generics.Collections.TList<System.Classes.TBasicActionLink>',
     'System.Generics.Collections.TList<System.Classes.TBasicActionLink>.ParrayofT',
     'System.Generics.Collections.TList<System.Classes.TBasicActionLink>.TEmptyFunc',
     'System.Generics.Collections.TList<System.Classes.TBasicActionLink>.TEnumerator',
     'System.Generics.Collections.TList<System.Classes.TBasicActionLink>.arrayofT',
     'System.Generics.Collections.TList<System.Classes.TComponent>',
     'System.Generics.Collections.TList<System.Classes.TComponent>.ParrayofT',
     'System.Generics.Collections.TList<System.Classes.TComponent>.TEmptyFunc',
     'System.Generics.Collections.TList<System.Classes.TComponent>.TEnumerator',
     'System.Generics.Collections.TList<System.Classes.TComponent>.arrayofT',
     'System.Generics.Collections.TListHelper.TInternalCompareFunc',
     'System.Generics.Collections.TListHelper.TInternalNotifyProc',
     'System.Generics.Collections.TPair<System.Integer,System.Classes.IInterfaceList>',
     'System.Generics.Defaults.IComparer<System.Classes.TBasicActionLink>',
     'System.Generics.Defaults.IComparer<System.Classes.TComponent>',
     'System.Generics.Defaults.IEqualityComparer<System.Integer>',
     'System.HRESULT',
     'System.IEnumerable',
     'System.IEnumerable<System.Classes.TBasicActionLink>',
     'System.IEnumerable<System.Classes.TComponent>',
     'System.IInterface',
     'System.Int64',
     'System.Integer',
     'System.NativeInt',
     'System.PAnsiChar',
     'System.PCurrency',
     'System.PExtended',
     'System.PInt64',
     'System.PInterfaceEntry',
     'System.PInterfaceTable',
     'System.PResStringRec',
     'System.PShortString',
     'System.PVariant',
     'System.PWideChar',
     'System.Pointer',
     'System.ShortInt',
     'System.ShortString',
     'System.Single',
     'System.SmallInt',
     'System.SysUtils.TEncoding',
     'System.SysUtils.TProc',
     'System.TArray<System.Byte>',
     'System.TArray<System.Char>',
     'System.TArray<System.Classes.IInterfaceList>',
     'System.TArray<System.Classes.IInterfaceList>',
     'System.TArray<System.Classes.TBasicActionLink>',
     'System.TArray<System.Classes.TComponent>',
     'System.TArray<System.Generics.Collections.TPair<System.Integer,System.Classes.IInterfaceList>>',
     'System.TArray<System.Integer>',
     'System.TArray<System.TObject>',
     'System.TArray<System.string>',
     'System.TClass',
     'System.TDateTime',
     'System.TExtended80Rec',
     'System.TFloatSpecial',
     'System.TGUID',
     'System.TObject',
     'System.TVarRec',
     'System.Types.IAsyncResult',
     'System.Types.TDirection',
     'System.Types.TDuplicates',
     'System.Types.TPoint',
     'System.Types.TRect',
     'System.Types.TSmallPoint',
     'System.Types.TSplitRectType',
     'System.UInt64',
     'System.Word',
     'System.string'],
    []);

finalization
  ALRttiFinalization;

end.
