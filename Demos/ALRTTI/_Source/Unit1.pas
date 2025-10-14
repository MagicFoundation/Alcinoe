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

  {~~~~~~~~~~~~~~~~~~~}
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    ButtonBenchTALRttiTypeGetFields: TButton;
    ButtonBenchTRttiTypeGetFields: TButton;
    ButtonBenchTALRttiTypeGetField: TButton;
    ButtonBenchTRttiTypeGetField: TButton;
    procedure ButtonBenchTALRttiTypeGetFieldsClick(Sender: TObject);
    procedure ButtonBenchTRttiTypeGetFieldsClick(Sender: TObject);
    procedure ButtonBenchTALRttiTypeGetFieldClick(Sender: TObject);
    procedure ButtonBenchTRttiTypeGetFieldClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
  protected
  private
  public
  end;

var
  Form1: TForm1;

implementation

uses
  System.Math,
  System.TypInfo,
  system.rtti,
  Alcinoe.Common,
  Alcinoe.Localization,
  Alcinoe.StringUtils;

{$R *.dfm}

{*********************************************************************}
procedure TForm1.ButtonBenchTALRttiTypeGetFieldsClick(Sender: TObject);
begin
  var LQualifiedClassName := {$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(Self.QualifiedClassName);
  var LStopWatch := TstopWatch.StartNew;
  var LRTTIType := ALGetRTTIType(LQualifiedClassName);
  for var I := 0 to 500000 do
    LRTTIType.GetFields(mvPublished);
  LStopWatch.stop;
  Showmessage('500 000 GetFields in '+AlformatFloatW('0.##', LStopWatch.Elapsed.TotalMilliseconds) + ' ms');
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
  Showmessage('500 000 GetFields in '+AlformatFloatW('0.##', LStopWatch.Elapsed.TotalMilliseconds) + ' ms');
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
  Showmessage('500 000 GetField in '+AlformatFloatW('0.##', LStopWatch.Elapsed.TotalMilliseconds) + ' ms');
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
  Showmessage('500 000 GetField in '+AlformatFloatW('0.##', LStopWatch.Elapsed.TotalMilliseconds) + ' ms');
end;

{******************************************}
procedure TForm1.FormClick(Sender: TObject);
begin
 //nothing to do here
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