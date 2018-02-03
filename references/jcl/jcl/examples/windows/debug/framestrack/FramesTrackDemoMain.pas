unit FramesTrackDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    mmLog: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    chkShowAllFrames: TCheckBox;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    procedure LogException(ExceptObj: TObject; ExceptAddr: Pointer; IsOS: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  JclDebug, JclHookExcept, TypInfo;

procedure TForm1.LogException(ExceptObj: TObject; ExceptAddr: Pointer; IsOS: Boolean);
var
  TmpS: string;
  ModInfo: TJclLocationInfo;
  I: Integer;
  ExceptionHandled: Boolean;
  HandlerLocation: Pointer;
  ExceptFrame: TJclExceptFrame;

begin
  TmpS := 'Exception ' + ExceptObj.ClassName;
  if ExceptObj is Exception then
    TmpS := TmpS + ': ' + Exception(ExceptObj).Message;
  if IsOS then
    TmpS := TmpS + ' (OS Exception)';
  mmLog.Lines.Add(TmpS);
  ModInfo := GetLocationInfo(ExceptAddr);
  mmLog.Lines.Add(Format(
    '  Exception occured at $%p (Module "%s", Procedure "%s", Unit "%s", Line %d)',
    [ModInfo.Address,
     ModInfo.UnitName,
     ModInfo.ProcedureName,
     ModInfo.SourceName,
     ModInfo.LineNumber]));
  if stExceptFrame in JclStackTrackingOptions then
  begin
    mmLog.Lines.Add('  Except frame-dump:');
    I := 0;
    ExceptionHandled := False;
    while (chkShowAllFrames.Checked or not ExceptionHandled) and
      (I < JclLastExceptFrameList.Count) do
    begin
      ExceptFrame := JclLastExceptFrameList.Items[I];
      ExceptionHandled := ExceptFrame.HandlerInfo(ExceptObj, HandlerLocation);
      if (ExceptFrame.FrameKind = efkFinally) or
          (ExceptFrame.FrameKind = efkUnknown) or
          not ExceptionHandled then
        HandlerLocation := ExceptFrame.CodeLocation;
      ModInfo := GetLocationInfo(HandlerLocation);
      TmpS := Format(
        '    Frame at $%p (type: %s',
        [ExceptFrame.FrameLocation,
         GetEnumName(TypeInfo(TExceptFrameKind), Ord(ExceptFrame.FrameKind))]);
      if ExceptionHandled then
        TmpS := TmpS + ', handles exception)'
      else
        TmpS := TmpS + ')';
      mmLog.Lines.Add(TmpS);
      if ExceptionHandled then
        mmLog.Lines.Add(Format(
          '      Handler at $%p',
          [HandlerLocation]))
      else
        mmLog.Lines.Add(Format(
          '      Code at $%p',
          [HandlerLocation]));
      mmLog.Lines.Add(Format(
        '      Module "%s", Procedure "%s", Unit "%s", Line %d',
        [ModInfo.UnitName,
         ModInfo.ProcedureName,
         ModInfo.SourceName,
         ModInfo.LineNumber]));
      Inc(I);
    end;
  end;
  mmLog.Lines.Add('');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  JclAddExceptNotifier(Form1.LogException);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  JclRemoveExceptNotifier(Form1.LogException);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  mmLog.Lines.Add(TButton(Sender).Caption);
  PChar(nil)^ := 'a';
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  mmLog.Lines.Clear;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  mmLog.Lines.Add(TButton(Sender).Caption);
  try
    PChar(nil)^ := 'a';
  except
    on E: Exception do
      ShowMessage('Error = ' + E.Message);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  mmLog.Lines.Add(TButton(Sender).Caption);
  try
    ShowMessage(IntToStr(StrToInt('a')));
  except
    on E: Exception do
      ShowMessage('Exception + ' + E.Message);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  mmLog.Lines.Add(TButton(Sender).Caption);
  try
    PChar(nil)^ := 'a';
  finally
    ShowMessage('finally!');
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  mmLog.Lines.Add(TButton(Sender).Caption);
  try
    try
      PChar(nil)^ := 'a';
    finally
      ShowMessage('Finally!');
    end;
  except
    ShowMessage('Except!');
  end;
end;

initialization

  JclStackTrackingOptions := JclStackTrackingOptions + [stExceptFrame];
  JclStartExceptionTracking;

end.
