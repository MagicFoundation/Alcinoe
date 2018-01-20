unit Project21HttpClientMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    lbledtServerAddress: TLabeledEdit;
    lbledtClientThreadCount: TLabeledEdit;
    lbledtClientPerThreadInstanceCount: TLabeledEdit;
    lbledtNumberOfObjectAdded: TLabeledEdit;
    btnStart: TButton;
    mmoInfo: TMemo;
    chkSocketAPI: TCheckBox;
    procedure lbledtClientPerThreadInstanceCountKeyPress(Sender: TObject;
      var Key: Char);
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  SynCommons,
  SynLog,
  SynTests,
  mORMot,
  SynSelfTests;

{$R *.dfm}

procedure TMainForm.lbledtClientPerThreadInstanceCountKeyPress(
  Sender: TObject; var Key: Char);
begin
  if (Key<'0') or (Key>'9') then
    Key := #0;
end;

procedure TMainForm.btnStartClick(Sender: TObject);
var Tests: TSynTestsLogged;
    Test: TTestMultiThreadProcess;
    ThreadCount, OperationCount, ClientPerThread: integer;
    Timer: TPrecisionTimer;
    txt: string;
begin
  if (not TryStrToInt(lbledtClientThreadCount.Text,ThreadCount)) or
     (ThreadCount<1) or (ThreadCount>500) then begin
    lbledtClientThreadCount.SetFocus;
    exit;
  end;
  if not TryStrToInt(lbledtNumberOfObjectAdded.Text,OperationCount) or
     (OperationCount<1) or (OperationCount>100000) then begin
    lbledtNumberOfObjectAdded.SetFocus;
    exit;
  end;
  if not TryStrToInt(lbledtClientPerThreadInstanceCount.Text,ClientPerThread) or
     (ClientPerThread<1) or (ClientPerThread>100) then begin
    lbledtClientPerThreadInstanceCount.SetFocus;
    exit;
  end;
  txt := mmoInfo.Text;
  btnStart.Enabled := false;
  try
    Tests := TSynTestsLogged.Create;
    Test := TTestMultiThreadProcess.Create(Tests);
    try
      Test.ClientOnlyServerIP := StringToAnsi7(lbledtServerAddress.Text);
      Test.MinThreads := ThreadCount;
      Test.MaxThreads := ThreadCount;
      Test.OperationCount := OperationCount;
      Test.ClientPerThread := ClientPerThread;
      Test.CreateThreadPool;
      txt := Format
        ('%s'#13#10#13#10'Test started with %d threads, %d client(s) per thread and %d rows to be inserted...',
        [txt,ThreadCount,ClientPerThread,OperationCount]);
      mmoInfo.Text := txt;
      mmoInfo.SelStart := length(txt);
      mmoInfo.SelLength := 0;
      Timer.Start;
      if chkSocketAPI.Checked then
        Test.SocketAPI else
        Test.WindowsAPI;
      txt := mmoInfo.Text+Format(#13#10'Assertion(s) failed: %d / %d'+
        #13#10'Number of clients connected at once: %d'+
        #13#10'Time to process: %s'#13#10'Operation per second: %d',
        [Test.AssertionsFailed,Test.Assertions,
         ThreadCount*ClientPerThread,Timer.Stop,Timer.PerSec(OperationCount*2)]);
      mmoInfo.Text := txt;
      mmoInfo.SelStart := length(txt);
      mmoInfo.SelLength := 0;
    finally
      Test.Free;
      Tests.Free;
    end;
  finally
    btnStart.Enabled := true;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // define the log level
  TSynLogTestLog := TSQLLog;
  with TSQLLog.Family do begin
    Level := LOG_STACKTRACE+[sllFail];
    PerThreadLog := ptIdentifiedInOnFile;
  end;
end;

end.
