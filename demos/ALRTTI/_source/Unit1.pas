unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Shellapi,
  System.Generics.Defaults, diagnostics, System.Generics.Collections, dateutils;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    Button11: TButton;
    Button12: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    procedure Button11Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
  protected
  private
  public
  end;

var Form1: TForm1;

implementation

uses
  System.TypInfo,
  system.rtti,
  ALRtti,
  alcommon,
  ALString;

{$R *.dfm}

{***********************************************}
procedure TForm1.Button11Click(Sender: TObject);
var LRTTIType: TALRTTIType;
    LRttiProperty: TALRttiProperty;
    LName: ansiString;
    Lcount: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  Lcount := 0;
  for var I := 0 to 500000 do begin
    LRTTIType := ALGetRTTIType(ansiString(Self.QualifiedClassName));
    for LRttiProperty in LRTTIType.GetProperties(mvprotected) do begin
      if LRttiProperty.PropertyType.TypeKind = tkenumeration then begin
        LName := ALGetEnumName(TALRttiInstanceProperty(LRttiProperty).PropInfo, GetOrdProp(self, TALRttiInstanceProperty(LRttiProperty).PropInfo));
        inc(Lcount);
      end;
    end;
  end;
  Showmessage(intToStr(Lcount) + ' ALGetEnumName in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{***********************************************}
procedure TForm1.Button12Click(Sender: TObject);
var LRTTIType: TALRTTIType;
    LRttiProperty: TALRttiProperty;
    LName: String;
    Lcount: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  Lcount := 0;
  for var I := 0 to 500000 do begin
    LRTTIType := ALGetRTTIType(ansiString(Self.QualifiedClassName));
    for LRttiProperty in LRTTIType.GetProperties(mvprotected) do begin
      if LRttiProperty.PropertyType.TypeKind = tkenumeration then begin
        LName := GetEnumName(TALRttiInstanceProperty(LRttiProperty).PropInfo.PropType^, GetOrdProp(self, TALRttiInstanceProperty(LRttiProperty).PropInfo));
        inc(Lcount);
      end;
    end;
  end;
  Showmessage(intToStr(Lcount) + ' GetEnumName in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{*********************************************}
procedure TForm1.Button1Click(Sender: TObject);
var LRTTIType: TALRTTIType;
    LRttiProperty: TALRttiProperty;
    Lcount: integer;
    StartDate: TdateTime;
    EnumValue: integer;
begin
  StartDate := Now;
  Lcount := 0;
  for var I := 0 to 500000 do begin
    LRTTIType := ALGetRTTIType(ansiString(Self.QualifiedClassName));
    for LRttiProperty in LRTTIType.GetProperties(mvprotected) do begin
      if LRttiProperty.PropertyType.TypeKind = tkenumeration then begin
        ALTryGetEnumValue(TALRttiInstanceProperty(LRttiProperty).PropInfo, 'ALNone', EnumValue);
        inc(Lcount);
      end;
    end;
  end;
  Showmessage(intToStr(Lcount) + ' ALTryGetEnumValue in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{*********************************************}
procedure TForm1.Button2Click(Sender: TObject);
var LRTTIType: TALRTTIType;
    LRttiProperty: TALRttiProperty;
    Lcount: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  Lcount := 0;
  for var I := 0 to 500000 do begin
    LRTTIType := ALGetRTTIType(ansiString(Self.QualifiedClassName));
    for LRttiProperty in LRTTIType.GetProperties(mvprotected) do begin
      if LRttiProperty.PropertyType.TypeKind = tkenumeration then begin
        GetEnumValue(TALRttiInstanceProperty(LRttiProperty).PropInfo.PropType^, 'ALNone');
        inc(Lcount);
      end;
    end;
  end;
  Showmessage(intToStr(Lcount) + ' GetEnumValue in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{*********************************************}
procedure TForm1.Button3Click(Sender: TObject);
var LRTTIType: TALRTTIType;
    LRttiProperty: TALRttiProperty;
    Lcount: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  Lcount := 0;
  for var I := 0 to 500000 do begin
    LRTTIType := ALGetRTTIType(ansiString(Self.QualifiedClassName));
    for LRttiProperty in LRTTIType.GetProperties(mvprotected) do begin
      if LRttiProperty.PropertyType.TypeKind = tkset then begin
        ALSetToString(TALRttiInstanceProperty(LRttiProperty).PropInfo, 1);
        inc(Lcount);
      end;
    end;
  end;
  Showmessage(intToStr(Lcount) + ' ALSetToString in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{*********************************************}
procedure TForm1.Button4Click(Sender: TObject);
var LRTTIType: TALRTTIType;
    LRttiProperty: TALRttiProperty;
    Lcount: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  Lcount := 0;
  for var I := 0 to 500000 do begin
    LRTTIType := ALGetRTTIType(ansiString(Self.QualifiedClassName));
    for LRttiProperty in LRTTIType.GetProperties(mvprotected) do begin
      if LRttiProperty.PropertyType.TypeKind = tkset then begin
        SetToString(TALRttiInstanceProperty(LRttiProperty).PropInfo.PropType^, 1);
        inc(Lcount);
      end;
    end;
  end;
  Showmessage(intToStr(Lcount) + ' SetToString in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{*********************************************}
procedure TForm1.Button5Click(Sender: TObject);
var LRTTIType: TALRTTIType;
    LRttiProperty: TALRttiProperty;
    Lcount: integer;
    StartDate: TdateTime;
    EnumValue: integer;
begin
  StartDate := Now;
  Lcount := 0;
  for var I := 0 to 500000 do begin
    LRTTIType := ALGetRTTIType(ansiString(Self.QualifiedClassName));
    for LRttiProperty in LRTTIType.GetProperties(mvprotected) do begin
      if (LRttiProperty.PropertyType.TypeKind = tkset) and (LRttiProperty.Name = 'Anchors') then begin
        ALTryStringToSet(TALRttiInstanceProperty(LRttiProperty).PropInfo, '[AKLeft, AKRight]', EnumValue);
        inc(Lcount);
      end;
    end;
  end;
  Showmessage(intToStr(Lcount) + ' ALTryStringToSet in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{*********************************************}
procedure TForm1.Button6Click(Sender: TObject);
var LRTTIType: TALRTTIType;
    LRttiProperty: TALRttiProperty;
    Lcount: integer;
    StartDate: TdateTime;
begin
  StartDate := Now;
  Lcount := 0;
  for var I := 0 to 500000 do begin
    LRTTIType := ALGetRTTIType(ansiString(Self.QualifiedClassName));
    for LRttiProperty in LRTTIType.GetProperties(mvprotected) do begin
      if (LRttiProperty.PropertyType.TypeKind = tkset) and (LRttiProperty.Name = 'Anchors') then begin
        StringToSet(TALRttiInstanceProperty(LRttiProperty).PropInfo, '[AKLeft, AKRight]');
        inc(Lcount);
      end;
    end;
  end;
  Showmessage(intToStr(Lcount) + ' StringToSet in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{*********************************************}
procedure TForm1.Button7Click(Sender: TObject);
var LRTTIType: TALRTTIType;
    StartDate: TdateTime;
begin
  StartDate := Now;
  LRTTIType := ALGetRTTIType(ansiString(Self.QualifiedClassName));
  for var I := 0 to 500000 do
    LRTTIType.GetFields(mvPublished);
  Showmessage('500000 GetFields in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{*********************************************}
procedure TForm1.Button8Click(Sender: TObject);
var ctx: TRttiContext;
    rt: TRttiType;
    StartDate: TdateTime;
begin
  StartDate := Now;
  ctx := TRttiContext.Create;
  try
    rt := ctx.GetType(TForm1);
    for var I := 0 to 500000 do
      rt.GetFields;
  finally
    ctx.Free;
  end;
  Showmessage('500000 GetFields in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{*********************************************}
procedure TForm1.Button9Click(Sender: TObject);
var LRTTIType: TALRTTIType;
    StartDate: TdateTime;
begin
  StartDate := Now;
  LRTTIType := ALGetRTTIType(ansiString(Self.QualifiedClassName));
  for var I := 0 to 500000 do
    LRTTIType.GetField('Action', mvPublished);
  Showmessage('500000 GetField in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{**********************************************}
procedure TForm1.Button10Click(Sender: TObject);
var ctx: TRttiContext;
    rt: TRttiType;
    StartDate: TdateTime;
begin
  StartDate := Now;
  ctx := TRttiContext.Create;
  try
    rt := ctx.GetType(TForm1);
    for var I := 0 to 500000 do
      rt.GetField('Action');
  finally
    ctx.Free;
  end;
  Showmessage('500000 GetField in: ' + FormatDateTime('nn:ss.zzz',now-StartDate));
end;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  ALRttiInitialization;
end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  ALRttiFinalization;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
