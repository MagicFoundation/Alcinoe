unit Unit1;

interface

{.$define ALRTTIAnsiString}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Shellapi,
  System.Generics.Defaults, diagnostics, System.Generics.Collections, dateutils,
  Alcinoe.RTTI;

{$RTTI EXPLICIT METHODS([vcPublic, vcPublished])
                FIELDS([vcPublic, vcPublished])
                PROPERTIES([vcProtected, vcPublic, vcPublished])}

type
  TForm1 = class(TForm)
    ButtonCreateObjectWithAutoInit: TButton;
    ButtonCreateObjectClassicalWay: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    ButtonBenchTALRttiTypeGetFields: TButton;
    ButtonBenchTRttiTypeGetFields: TButton;
    ButtonBenchTALRttiTypeGetField: TButton;
    ButtonBenchTRttiTypeGetField: TButton;
    Memo1: TMemo;
    Panel2: TPanel;
    Label2: TLabel;
    procedure ButtonCreateObjectWithAutoInitClick(Sender: TObject);
    procedure ButtonCreateObjectClassicalWayClick(Sender: TObject);
    procedure ButtonBenchTALRttiTypeGetFieldsClick(Sender: TObject);
    procedure ButtonBenchTRttiTypeGetFieldsClick(Sender: TObject);
    procedure ButtonBenchTALRttiTypeGetFieldClick(Sender: TObject);
    procedure ButtonBenchTRttiTypeGetFieldClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
  protected
  private
  public
  end;

  TChildObject = class;

  TAutoInitObject = class(TObject)
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
    FChildObject2: TChildObject;
    FBooleanValue2: Boolean;
    FInt32Value2: Int32;
    FAnsiCharValue2: ansiChar;
    fOwner2: Tform1;
    fOwner3: TForm1;
    FDateTimeValue3: TDateTime;
    FAlignValues3: TAlignSet;
    FInt64Value3: Int64;
    FSingleValue3: Single;
    FStringValue3: String;
    FAlignValue3: TAlign;
    FStringList3: TStringList;
    FChildObject3: TChildObject;
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
    procedure SetChildObject2(const Value: TChildObject);
    procedure SetDateTimeValue2(const Value: TDateTime);
    procedure SetDoubleValue2(const Value: Double);
    procedure SetInt32Value2(const Value: Int32);
    procedure SetInt64Value2(const Value: Int64);
    procedure SetOnclick2(const Value: TnotifyEvent);
    procedure SetSingleValue2(const Value: Single);
    procedure SetStringList2(const Value: TStringList);
    procedure SetStringValue2(const Value: String);
    function GetOwner2: TForm1;
  public
    AutoInit: Boolean;
    Owner: TForm1;
    property Owner2: TForm1 read GetOwner2;
    property Owner3: TForm1 read fOwner3;
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
    [TALInit('Owner.FormClick')]
    Onclick: TnotifyEvent;
    [TALInit('left:50;right:75')]
    Rect: TRect;
    [TALInit('')]
    ChildObject: TChildObject;
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
    [TALInit('Owner2.FormClick')]
    property Onclick2: TnotifyEvent read FOnclick2 write SetOnclick2;
    [TALInit('')]
    property ChildObject2: TChildObject read FChildObject2 write SetChildObject2;
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
    [TALInit('Owner2.FormClick')]
    property Onclick3: TnotifyEvent read FOnclick3 write FOnclick3;
    [TALInit('')]
    property ChildObject3: TChildObject read FChildObject3 write fChildObject3;
  public
    constructor Create(const aOwner: Tform1; const AAutoInit: Boolean); virtual;
    destructor Destroy; override;
  End;

  [TALInit('BooleanValue3:false;Int32Value:52')]
  TAutoInitObject2 = class(TAutoInitObject)
  private
  public
    [TALInit('alLeft')]
    property AlignValue2;
   public
    constructor Create(const aOwner: Tform1; const AAutoInit: Boolean); override;
  End;

  [TALInit('Int32Value:75')]
  TAutoInitObject3 = class(TAutoInitObject2)
  private
  public
    [TALInit('altop')]
    property AlignValue2;
   public
    constructor Create(const aOwner: Tform1; const AAutoInit: Boolean); override;
  End;

  TChildObject = class(TObject)
  private
    fOwner: TAutoInitObject;
  public
    constructor Create(const aOwner: TAutoInitObject); virtual;
  End;

var Form1: TForm1;

implementation

uses
  System.Math,
  System.TypInfo,
  system.rtti,
  Alcinoe.Common,
  Alcinoe.StringUtils;

{$R *.dfm}

{********************************************************************}
procedure TForm1.ButtonCreateObjectWithAutoInitClick(Sender: TObject);
begin
  var LStopWatch := TstopWatch.StartNew;
  for var I := 0 to 1000000 do begin
    var LAutoInitObject := TAutoInitObject.create(self, True);
    LAutoInitObject.Free;
  end;
  var LAutoInitObject2 := TAutoInitObject2.create(self, True);
  LAutoInitObject2.Free;
  var LAutoInitObject3 := TAutoInitObject3.create(self, True);
  LAutoInitObject3.Free;
  LStopWatch.stop;
  Showmessage('1 000 000 TAutoInitObject created in '+AlformatFloatW('0.##', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW) + ' ms');
end;

{********************************************************************}
procedure TForm1.ButtonCreateObjectClassicalWayClick(Sender: TObject);
begin
  var LStopWatch := TstopWatch.StartNew;
  for var I := 0 to 1000000 do begin
    var LAutoInitObject := TAutoInitObject.create(self, false);
    LAutoInitObject.Free;
  end;
  var LAutoInitObject2 := TAutoInitObject2.create(self, false);
  LAutoInitObject2.Free;
  var LAutoInitObject3 := TAutoInitObject3.create(self, false);
  LAutoInitObject3.Free;
  LStopWatch.stop;
  Showmessage('1 000 000 TAutoInitObject created in '+AlformatFloatW('0.##', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW) + ' ms');
end;

{*********************************************************************}
procedure TForm1.ButtonBenchTALRttiTypeGetFieldsClick(Sender: TObject);
begin
  var LQualifiedClassName := {$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(Self.QualifiedClassName);
  var LStopWatch := TstopWatch.StartNew;
  var LRTTIType := ALGetRTTIType(LQualifiedClassName);
  for var I := 0 to 500000 do
    LRTTIType.GetFields(mvPublished);
  LStopWatch.stop;
  Showmessage('500 000 GetFields in '+AlformatFloatW('0.##', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW) + ' ms');
end;

{*******************************************************************}
procedure TForm1.ButtonBenchTRttiTypeGetFieldsClick(Sender: TObject);
begin
  var LStopWatch := TstopWatch.StartNew;
  var ctx := TRttiContext.Create;
  try
    var rt := ctx.GetType(TForm1);
    for var I := 0 to 500000 do
      rt.GetFields;
  finally
    ctx.Free;
  end;
  LStopWatch.stop;
  Showmessage('500 000 GetFields in '+AlformatFloatW('0.##', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW) + ' ms');
end;

{********************************************************************}
procedure TForm1.ButtonBenchTALRttiTypeGetFieldClick(Sender: TObject);
begin
  var LQualifiedClassName := {$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(Self.QualifiedClassName);
  var LStopWatch := TstopWatch.StartNew;
  var LRTTIType := ALGetRTTIType(LQualifiedClassName);
  for var I := 0 to 500000 do
    LRTTIType.GetField('Action', mvPublished);
  LStopWatch.stop;
  Showmessage('500 000 GetField in '+AlformatFloatW('0.##', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW) + ' ms');
end;

{******************************************************************}
procedure TForm1.ButtonBenchTRttiTypeGetFieldClick(Sender: TObject);
begin
  var LStopWatch := TstopWatch.StartNew;
  var ctx := TRttiContext.Create;
  try
    var rt := ctx.GetType(TForm1);
    for var I := 0 to 500000 do
      rt.GetField('Action');
  finally
    ctx.Free;
  end;
  LStopWatch.stop;
  Showmessage('500 000 GetField in '+AlformatFloatW('0.##', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW) + ' ms');
end;

{******************************************}
procedure TForm1.FormClick(Sender: TObject);
begin
 //nothing to do here
end;

{*********************************************************************************}
constructor TAutoInitObject.create(const aOwner: Tform1; const AAutoInit: Boolean);
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
    Onclick := Owner.FormClick;
    Rect.left := 50;
    Rect.right := 75;
    ChildObject := TChildObject.create(self);
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
    Onclick2 := Owner.FormClick;
    ChildObject2 := TChildObject.create(self);
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
    Onclick3 := Owner.FormClick;
    ChildObject3 := TChildObject.create(self);
  end;
  //--
  if self.ClassType = TAutoInitObject  then begin
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
    if ChildObject.classname <> 'TChildObject' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
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
    if ChildObject2.classname <> 'TChildObject' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
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
    if ChildObject3.classname <> 'TChildObject' then raise Exception.create('Error 3AC91D01-134D-4092-BAEC-D22217157CDE');
  end;
end;

{*********************************}
destructor TAutoInitObject.Destroy;
begin
  if AutoInit then ALRttiFinalizeInstance(self)
  else begin
    ALFreeandNil(StringList);
    ALFreeandNil(ChildObject);
  end;
  inherited;
end;

{*****************************************}
function TAutoInitObject.GetOwner2: TForm1;
begin
  result := FOwner2;
end;

{************************************************************}
procedure TAutoInitObject.SetAlignValue2(const Value: TAlign);
begin
  FAlignValue2 := Value;
end;

{****************************************************************}
procedure TAutoInitObject.SetAlignValues2(const Value: TAlignSet);
begin
  FAlignValues2 := Value;
end;

{*****************************************************************}
procedure TAutoInitObject.SetAnsiCharValue2(const Value: ansiChar);
begin
  FAnsiCharValue2 := Value;
end;

{*********************************************************************}
procedure TAutoInitObject.SetAnsiStringValue2(const Value: ansiString);
begin
  FAnsiStringValue2 := Value;
end;

{***************************************************************}
procedure TAutoInitObject.SetBooleanValue2(const Value: Boolean);
begin
  FBooleanValue2 := Value;
end;

{*********************************************************}
procedure TAutoInitObject.SetCharValue2(const Value: Char);
begin
  FCharValue2 := Value;
end;

{*******************************************************************}
procedure TAutoInitObject.SetChildObject2(const Value: TChildObject);
begin
  FChildObject2 := Value;
end;

{******************************************************************}
procedure TAutoInitObject.SetDateTimeValue2(const Value: TDateTime);
begin
  FDateTimeValue2 := Value;
end;

{*************************************************************}
procedure TAutoInitObject.SetDoubleValue2(const Value: Double);
begin
  FDoubleValue2 := Value;
end;

{***********************************************************}
procedure TAutoInitObject.SetInt32Value2(const Value: Int32);
begin
  FInt32Value2 := Value;
end;

{***********************************************************}
procedure TAutoInitObject.SetInt64Value2(const Value: Int64);
begin
  FInt64Value2 := Value;
end;

{***************************************************************}
procedure TAutoInitObject.SetOnclick2(const Value: TnotifyEvent);
begin
  FOnclick2 := Value;
end;

{*************************************************************}
procedure TAutoInitObject.SetSingleValue2(const Value: Single);
begin
  FSingleValue2 := Value;
end;

{*****************************************************************}
procedure TAutoInitObject.SetStringList2(const Value: TStringList);
begin
  FStringList2 := Value;
end;

{*************************************************************}
procedure TAutoInitObject.SetStringValue2(const Value: String);
begin
  FStringValue2 := Value;
end;

{**********************************************************************************}
constructor TAutoInitObject2.Create(const aOwner: Tform1; const AAutoInit: Boolean);
begin
  inherited create(aOwner, AAutoInit);
  if not AAutoInit then begin
    AlignValue2 := TAlign.alLeft;
    BooleanValue3 := false;
    Int32Value := 52;
  end;
  //--
  if self.ClassType = TAutoInitObject2 then begin
    if AlignValue2 <> TAlign.alLeft then raise Exception.create('Error D09DA31D-11BF-4404-BF0A-92D38B82D23D');
    if BooleanValue3 <> false then raise Exception.create('Error D09DA31D-11BF-4404-BF0A-92D38B82D23D');
    if Int32Value <> 52 then raise Exception.create('Error D09DA31D-11BF-4404-BF0A-92D38B82D23D');
  end;
end;

{**********************************************************************************}
constructor TAutoInitObject3.Create(const aOwner: Tform1; const AAutoInit: Boolean);
begin
  inherited create(aOwner, AAutoInit);
  if not AAutoInit then begin
    AlignValue2 := ALTop;
    Int32Value := 75;
  end;
  //--
  if self.ClassType = TAutoInitObject3 then begin
    if AlignValue2 <> altop then raise Exception.create('Error D09DA31D-11BF-4404-BF0A-92D38B82D23D');
    if Int32Value <> 75 then raise Exception.create('Error D09DA31D-11BF-4404-BF0A-92D38B82D23D');
  end;
end;

{*************************************************************}
constructor TChildObject.Create(const aOwner: TAutoInitObject);
begin
  if aOwner = nil then
    raise Exception.Create('Error CCD0F741-D468-4CD1-A94F-9E5F8F680643');
  fOwner := aOwner;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);
  ALRttiInitialization;

finalization
  ALRttiFinalization;

end.
