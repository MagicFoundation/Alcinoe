unit FMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Messaging,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  ECustomException = class(Exception);

type
  TFormMain = class(TForm)
    ButtonAV: TButton;
    ButtonAVOverridden: TButton;
    ButtonDivByZero: TButton;
    ButtonFileNotFound: TButton;
    ButtonInvalidObjCCall: TButton;
    ButtonAVThread: TButton;
    ToolBar: TToolBar;
    LabelHeader: TLabel;
    ButtonCustomException: TButton;
    ButtonHandledFileNotFound: TButton;
    ButtonHandledFileNotFoundThread: TButton;
    ButtonInvalidJavaCall: TButton;
    procedure ButtonAVClick(Sender: TObject);
    procedure ButtonAVOverriddenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonDivByZeroClick(Sender: TObject);
    procedure ButtonFileNotFoundClick(Sender: TObject);
    procedure ButtonInvalidObjCCallClick(Sender: TObject);
    procedure ButtonAVThreadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonCustomExceptionClick(Sender: TObject);
    procedure ButtonHandledFileNotFoundThreadClick(Sender: TObject);
    procedure ButtonHandledFileNotFoundClick(Sender: TObject);
    procedure ButtonInvalidJavaCallClick(Sender: TObject);
  private
    { Private declarations }
    procedure HandleExceptionReport(const Sender: TObject; const M: TMessage);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  {$IF Defined(IOS)}
  Macapi.Helpers,
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  {$ELSEIF Defined(ANDROID)}
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  {$ENDIF}
  System.IOUtils,
  Grijjy.ErrorReporting,
  FReport,
  UThreads,
  UFoo;

{$R *.fmx}

procedure TFormMain.ButtonAVClick(Sender: TObject);
var
  P: PInteger;
  I: Integer;
begin
  { Create an Access Violation by accessing an invalid address. }
  P := Pointer(-1);
  I := P^;
  P^ := I + 1;
end;

procedure TFormMain.ButtonAVOverriddenClick(Sender: TObject);
var
  Foo: TFoo;
begin
  { Create an Access Violation in TFoo.ToString. }
  Foo := TFoo.Create;
  try
    Caption := Foo.ToString;
  finally
    Foo.DisposeOf;
  end;
end;

procedure TFormMain.ButtonAVThreadClick(Sender: TObject);
var
  Thread: TBadThread;
begin
  { Create an Access Violation in TBadThread.Execute.
    This exception will be raised from a separate thread. }
  Thread := TBadThread.Create(False);
  Thread.FreeOnTerminate := True;
end;

procedure TFormMain.ButtonCustomExceptionClick(Sender: TObject);
begin
  raise ECustomException.Create('A custom exception');
end;

procedure TFormMain.ButtonDivByZeroClick(Sender: TObject);
var
  A, B, C: Integer;
begin
  { Create integer division by zero exception }
  A := 1;
  B := 0;
  C := A div B;
  Caption := IntToStr(C);
end;

procedure TFormMain.ButtonFileNotFoundClick(Sender: TObject);
begin
  { Create File not Found exception. }
  TFileStream.Create('NonExistingFile', fmOpenRead);
end;

procedure TFormMain.ButtonHandledFileNotFoundClick(Sender: TObject);
begin
  { Create File not Found exception, but eat it. }
  try
    TFileStream.Create('NonExistingFile', fmOpenRead);
  except
    { Eat it }
  end;
end;

procedure TFormMain.ButtonHandledFileNotFoundThreadClick(Sender: TObject);
var
  Thread: TEatExceptionThread;
begin
  { Create a File not Found exception in TEatExceptionThread.Execute
    and eat it there. }
  Thread := TEatExceptionThread.Create(False);
  Thread.FreeOnTerminate := True;
end;

procedure TFormMain.ButtonInvalidJavaCallClick(Sender: TObject);
{$IF Defined(ANDROID)}
var
  S: JString;
  I: Integer;
begin
  { Create a Java string and access an invalid index into the string. }
  S := StringToJString('Foo');
  I := S.codePointAt(10);
  Caption := I.ToString;
{$ELSE}
begin
{$ENDIF}
end;

procedure TFormMain.ButtonInvalidObjCCallClick(Sender: TObject);
{$IF Defined(IOS)}
var
  S: NSString;
begin
  { Invalid call to NSLog that will make it crash }
  S :=  StrToNSStr('%@');
  NSLog((S as ILocalObject).GetObjectID, Pointer(16));
{$ELSE}
begin
{$ENDIF}
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Application.OnException := TgoExceptionReporter.ExceptionHandler;
  TMessageManager.DefaultManager.SubscribeToMessage(TgoExceptionReportMessage, HandleExceptionReport);

  {$IF not Defined(IOS)}
  ButtonInvalidObjCCall.Visible := False;
  {$ENDIF}

  {$IF not Defined(ANDROID)}
  ButtonInvalidJavaCall.Visible := False;
  {$ENDIF}
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  TMessageManager.DefaultManager.Unsubscribe(TgoExceptionReportMessage, HandleExceptionReport);
end;

procedure TFormMain.HandleExceptionReport(const Sender: TObject;
  const M: TMessage);
var
  Report: IgoExceptionReport;
begin
  Assert(M is TgoExceptionReportMessage);
  Report := TgoExceptionReportMessage(M).Report;

  { This message can be sent from any thread. So if we want to show the report
    in the UI, we need to synchronize it with the main thread. We use
    TThread.Queue here so it doesn't block. }
  TThread.Queue(nil,
    procedure
    begin
      ShowReport(Report.Report);
    end);
end;

end.
