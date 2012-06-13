unit Unit1;

interface

uses Windows,
     Messages,
     SysUtils,
     Variants,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     StdCtrls,
     shellapi,
     ExtCtrls,
     ComCtrls,
     SyncObjs,
     AlPhpRunner,
     OleCtrls,
     SHDocVw,
     ComObj;

type
  TForm1 = class(TForm)
    MainStatusBar: TStatusBar;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox9: TGroupBox;
    editScriptFileName: TEdit;
    Label4: TLabel;
    Label6: TLabel;
    MemoPostDataStrings: TMemo;
    GroupBox10: TGroupBox;
    Label2: TLabel;
    MemoResponseRawHeader: TMemo;
    MemoContentBody: TMemo;
    Label3: TLabel;
    Label1: TLabel;
    ButtonInitAndPost: TButton;
    ButtonInitAndGet: TButton;
    ButtonOpenInExplorer: TButton;
    MemoServerVariables: TMemo;
    Label5: TLabel;
    Label7: TLabel;
    GroupBox1: TGroupBox;
    Label11: TLabel;
    EditPhpCGIPath: TEdit;
    EditPhpIsapiDllPath: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    EditPhpFastCgiHost: TEdit;
    EditPhpFastCgiPort: TEdit;
    Label10: TLabel;
    RadioButtonPHPCGIRunnerEngineKind: TRadioButton;
    RadioButtonPHPNamedPipeFastCGIRunnerEngineKind: TRadioButton;
    RadioButtonPHPSocketFastCGIRunnerEngineKind: TRadioButton;
    RadioButtonPHPISAPIRunnerEngineKind: TRadioButton;
    GroupBox2: TGroupBox;
    ButtonBench: TButton;
    Label12: TLabel;
    EditThreadCount: TEdit;
    EditCycleCount: TEdit;
    Label13: TLabel;
    Label15: TLabel;
    MemoBenchResult: TMemo;
    ButtonExecute: TButton;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel2: TPanel;
    Label14: TLabel;
    Label35: TLabel;
    Panel6: TPanel;
    PanelWebBrowser: TPanel;
    procedure ButtonInitAndGetClick(Sender: TObject);
    procedure ButtonOpenInExplorerClick(Sender: TObject);
    procedure ButtonInitAndPostClick(Sender: TObject);
    procedure ButtonBenchClick(Sender: TObject);
    procedure ButtonExecuteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FThreadCount: Integer;
    FstartTime: dWord;
  public
  end;

  TBenchThread = Class(TThread)
  Public
    PhpRunnerEngine: TAlPHPRunnerEngine;
    NbCycle: Integer;
    serverVariable: Tstrings;
    Destructor Destroy; override;
    procedure Execute; override;
  end;


var
  Form1: TForm1;
  CS: TcriticalSection;

implementation

Uses ALMultiPartFormDataParser,
     AlFcnFile,
     AlFcnMisc,
     AlFcnMime,
     AlHttpCommon;

{$R *.dfm}
{***************************************************}
procedure TForm1.ButtonExecuteClick(Sender: TObject);
Var AResponseHeader: TALHTTPResponseHeader;
    AResponseStream: TStringStream;
var aPhpRunnerEngine: TalPhpRunnerEngine;
begin
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AResponseHeader := TALHTTPResponseHeader.Create;
  AResponseStream := TstringStream.Create('');
  try
    try
      if RadioButtonPhpCGIRunnerEngineKind.Checked then aPhpRunnerEngine := TalPhpCgiRunnerEngine.Create(EditPhpCGIPath.Text)
      else if RadioButtonPhpNamedPipeFastCGIRunnerEngineKind.Checked then aPhpRunnerEngine := TalPhpNamedPipeFastCgiRunnerEngine.Create(EditPhpCGIPath.Text)
      else if RadioButtonPhpSocketFastCGIRunnerEngineKind.Checked then aPhpRunnerEngine := TalPhpSocketFastCgiRunnerEngine.Create(EditPhpFastCgiHost.Text, strtoint(EditPhpFastCgiPort.text))
      else aPhpRunnerEngine := TalPhpIsapiRunnerEngine.Create(EditPhpIsapiDllPath.text);
      Try
        aPhpRunnerEngine.Execute(
                                 MemoServerVariables.Lines,
                                 nil,
                                 AResponseStream,
                                 AResponseHeader
                                );
      Finally
        aPhpRunnerEngine.Free;
      End;
      MemoContentBody.Lines.Text := AResponseStream.DataString;
      MemoResponseRawHeader.Lines.Text := AResponseHeader.RawHeaderText;

    except
      MemoContentBody.Lines.Text := AResponseStream.DataString;
      MemoResponseRawHeader.Lines.Text := AResponseHeader.RawHeaderText;
      Raise;
    end;
  finally
    AResponseHeader.Free;
    AResponseStream.Free;
  end;
end;

{******************************************************}
procedure TForm1.ButtonInitAndGetClick(Sender: TObject);
Var AResponseHeader: TALHTTPResponseHeader;
    AResponseStream: TStringStream;
var aPhpRunnerEngine: TalPhpRunnerEngine;
begin
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AResponseHeader := TALHTTPResponseHeader.Create;
  AResponseStream := TstringStream.Create('');
  try
    try
      MemoServerVariables.Lines.Values['REQUEST_METHOD'] := 'GET';
      MemoServerVariables.Lines.Values['PATH_INFO'] := '/' + ExtractFileName(editScriptFileName.Text);
      MemoServerVariables.Lines.Values['PATH_TRANSLATED'] := ExpandFileName(editScriptFileName.Text);
      MemoServerVariables.Lines.Values['SCRIPT_NAME'] := MemoServerVariables.Lines.Values['PATH_INFO'];
      MemoServerVariables.Lines.Values['SCRIPT_FILENAME'] := MemoServerVariables.Lines.Values['PATH_TRANSLATED'];
      MemoServerVariables.Lines.Values['DOCUMENT_ROOT'] := ExtractFilePath(editScriptFileName.Text);
      if RadioButtonPhpCGIRunnerEngineKind.Checked then aPhpRunnerEngine := TalPhpCgiRunnerEngine.Create(EditPhpCGIPath.Text)
      else if RadioButtonPhpNamedPipeFastCGIRunnerEngineKind.Checked then aPhpRunnerEngine := TalPhpNamedPipeFastCgiRunnerEngine.Create(EditPhpCGIPath.Text)
      else if RadioButtonPhpSocketFastCGIRunnerEngineKind.Checked then aPhpRunnerEngine := TalPhpSocketFastCgiRunnerEngine.Create(EditPhpFastCgiHost.Text, strtoint(EditPhpFastCgiPort.text))
      else aPhpRunnerEngine := TalPhpIsapiRunnerEngine.Create(EditPhpIsapiDllPath.text);
      Try
        aPhpRunnerEngine.Execute(
                                 MemoServerVariables.Lines,
                                 nil,
                                 AResponseStream,
                                 AResponseHeader
                                );
      Finally
        aPhpRunnerEngine.Free;
      End;
      MemoContentBody.Lines.Text := AResponseStream.DataString;
      MemoResponseRawHeader.Lines.Text := AResponseHeader.RawHeaderText;

    except
      MemoContentBody.Lines.Text := AResponseStream.DataString;
      MemoResponseRawHeader.Lines.Text := AResponseHeader.RawHeaderText;
      Raise;
    end;
  finally
    AResponseHeader.Free;
    AResponseStream.Free;
  end;
end;

{**********************************************************}
procedure TForm1.ButtonOpenInExplorerClick(Sender: TObject);
Var AFullPath: AnsiString;
begin
  AFullPath := ALGetModulePath + '~tmp.html';
  MemoContentBody.Lines.SaveToFile(AFullPath);
  ShellExecute(0,'OPEN',PAnsiChar(AFullPath),nil,nil,SW_SHOW)
end;

{*******************************************************}
procedure TForm1.ButtonInitAndPostClick(Sender: TObject);
Var AResponseHeader: TALHTTPResponseHeader;
    AResponseStream: TStringStream;
    aPhpRunnerEngine: TalPhpRunnerEngine;
begin
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AResponseHeader := TALHTTPResponseHeader.Create;
  AResponseStream := TstringStream.Create('');
  try
    Try

      MemoServerVariables.Lines.Values['REQUEST_METHOD'] := 'POST';
      MemoServerVariables.Lines.Values['PATH_INFO'] := '/' + ExtractFileName(editScriptFileName.Text);
      MemoServerVariables.Lines.Values['PATH_TRANSLATED'] := ExpandFileName(editScriptFileName.Text);
      MemoServerVariables.Lines.Values['SCRIPT_NAME'] := MemoServerVariables.Lines.Values['PATH_INFO'];
      MemoServerVariables.Lines.Values['SCRIPT_FILENAME'] := MemoServerVariables.Lines.Values['PATH_TRANSLATED'];
      MemoServerVariables.Lines.Values['DOCUMENT_ROOT'] := ExtractFilePath(editScriptFileName.Text);
      if RadioButtonPhpCGIRunnerEngineKind.Checked then aPhpRunnerEngine := TalPhpCgiRunnerEngine.Create(EditPhpCGIPath.Text)
      else if RadioButtonPhpNamedPipeFastCGIRunnerEngineKind.Checked then aPhpRunnerEngine := TalPhpNamedPipeFastCgiRunnerEngine.Create(EditPhpCGIPath.Text)
      else if RadioButtonPhpSocketFastCGIRunnerEngineKind.Checked then aPhpRunnerEngine := TalPhpSocketFastCgiRunnerEngine.Create(EditPhpFastCgiHost.Text, strtoint(EditPhpFastCgiPort.text))
      else aPhpRunnerEngine := TalPhpIsapiRunnerEngine.Create(EditPhpIsapiDllPath.text);
      try
        if MemoPostDataStrings.Lines.Count > 0 then
          aPhpRunnerEngine.ExecutePostURLEncoded(
                                                 MemoServerVariables.Lines,
                                                 MemoPostDataStrings.Lines,
                                                 AResponseStream,
                                                 AResponseHeader,
                                                 True
                                                )
        else aPhpRunnerEngine.Execute(
                                      MemoServerVariables.Lines,
                                      nil,
                                      AResponseStream,
                                      AResponseHeader
                                     );
      finally
        aPhpRunnerEngine.free;
      end;

      MemoContentBody.Lines.Text := AResponseStream.DataString;
      MemoResponseRawHeader.Lines.Text := AResponseHeader.RawHeaderText;

    Except
      MemoContentBody.Lines.Text := AResponseStream.DataString;
      MemoResponseRawHeader.Lines.Text := AResponseHeader.RawHeaderText;
      Raise;
    end;
  finally
    AResponseHeader.Free;
    AResponseStream.Free;
  end;
end;

{*************************************************}
procedure TForm1.ButtonBenchClick(Sender: TObject);
Var i: Integer;
    aBenchThread: TBenchThread;
    aPhpNamedPipeFastCgiManager: TalPhpNamedPipeFastCgiManager;
begin
  MemoServerVariables.Lines.Values['REQUEST_METHOD'] := 'GET';
  MemoServerVariables.Lines.Values['PATH_INFO'] := '/' + ExtractFileName(editScriptFileName.Text);
  MemoServerVariables.Lines.Values['PATH_TRANSLATED'] := ExpandFileName(editScriptFileName.Text);
  MemoServerVariables.Lines.Values['SCRIPT_NAME'] := MemoServerVariables.Lines.Values['PATH_INFO'];
  MemoServerVariables.Lines.Values['SCRIPT_FILENAME'] := MemoServerVariables.Lines.Values['PATH_TRANSLATED'];
  MemoServerVariables.Lines.Values['DOCUMENT_ROOT'] := ExtractFilePath(editScriptFileName.Text);

  FThreadCount := strtoint(EditThreadCount.text);
  if RadioButtonPhpNamedPipeFastCGIRunnerEngineKind.Checked then begin
    aPhpNamedPipeFastCgiManager := TalPhpNamedPipeFastCgiManager.Create(EditPhpCGIPath.Text);
    aPhpNamedPipeFastCgiManager.ProcessPoolSize := 8;
    aPhpNamedPipeFastCgiManager.MaxRequestCount := 450;
    aPhpNamedPipeFastCgiManager.Timeout := 10000;
  end
  else aPhpNamedPipeFastCgiManager := nil;
  Form1.MainStatusBar.Panels[0].Text := '0';
  application.ProcessMessages;
  FstartTime := GetTickCount;
  for i := 1 to FThreadCount do begin
    aBenchThread := TBenchThread.Create(true);
    try
      if RadioButtonPhpCGIRunnerEngineKind.Checked then aBenchThread.PhpRunnerEngine := TalPhpCgiRunnerEngine.Create(EditPhpCGIPath.Text)
      else if RadioButtonPhpNamedPipeFastCGIRunnerEngineKind.Checked then aBenchThread.PhpRunnerEngine := aPhpNamedPipeFastCgiManager
      else if RadioButtonPhpSocketFastCGIRunnerEngineKind.Checked then aBenchThread.PhpRunnerEngine := TalPhpSocketFastCgiRunnerEngine.Create(EditPhpFastCgiHost.Text, strtoint(EditPhpFastCgiPort.text))
      else aBenchThread.PhpRunnerEngine := TalPhpIsapiRunnerEngine.Create(EditPhpIsapiDllPath.text);
      aBenChThread.serverVariable := TstringList.Create;
      aBenChThread.serverVariable.Assign(MemoServerVariables.Lines);
      aBenchThread.NbCycle := strtoint(EditCycleCount.Text);
      aBenchThread.FreeOnTerminate := True;
      aBenchThread.Resume;
    Except
      aBenchThread.Free;
      raise;
    end;
  end;
  Form1.Enabled := False;
end;


{ TBenchThread }

{******************************}
destructor TBenchThread.Destroy;
begin
  serverVariable.free;
  inherited;
end;

{*****************************}
procedure TBenchThread.Execute;
Var AResponseHeader: TALHTTPResponseHeader;
    AResponseStream: TStringStream;
    i: integer;
begin
  Try

    for i := 1 to NbCycle do begin

      Try

        AResponseHeader := TALHTTPResponseHeader.Create;
        AResponseStream := TstringStream.Create('');
        try
          PhpRunnerEngine.Execute(
                                  serverVariable,
                                  nil,
                                  AResponseStream,
                                  AResponseHeader
                                 );
        finally
          AResponseHeader.Free;
          AResponseStream.Free;
        end;

        if (i mod 100) = 0 then begin
          CS.Acquire;
          try
            Form1.MainStatusBar.Panels[0].Text := inttostr(strtoint(Form1.MainStatusBar.Panels[0].Text) + 100);
          finally
            cs.release;
          end;
        end;

      Except
        On e: Exception do begin
          CS.Acquire;
          try
            Form1.MemoBenchResult.Lines.Add(E.Message);
          finally
            cs.release;
          end;
          Break;
        end;
      End;

    end;

  Finally

    CS.Acquire;
    try
      Dec(Form1.FThreadCount);
      if Form1.FThreadCount = 0 then begin
        Form1.MemoBenchResult.Lines.Add(PhpRunnerEngine.ClassName  + ': ' + inttostr(strtoint(Form1.EditCycleCount.Text) * strtoint(Form1.EditThreadCount.text)) + ' requests in ' + inttostr(GetTickCount - Form1.FstartTime) + ' ms ('+ formatFloat('0.##',(strtoint(Form1.EditCycleCount.Text) * strtoint(Form1.EditThreadCount.text)) / ((GetTickCount - Form1.FstartTime) / 1000)) +' request/second)');
        Form1.Enabled := True;
        PhpRunnerEngine.free;
      end
      else if not (PhpRunnerEngine is TalPhpNamedPipeFastCgiManager) then PhpRunnerEngine.free;

    finally
      cs.release;
    end;

  End;
end;




{-------------------}
var ie: IWebBrowser2;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
var Url, Flags, TargetFrameName, PostData, Headers: OleVariant;
begin
  ie := CreateOleObject('InternetExplorer.Application') as IWebBrowser2;
  SetWindowLong(ie.hwnd, GWL_STYLE, GetWindowLong(ie.hwnd, GWL_STYLE) and not WS_BORDER and not WS_SIZEBOX and not WS_DLGFRAME );
  SetWindowPos(ie.hwnd, HWND_TOP, Left, Top, Width, Height, SWP_FRAMECHANGED);
  windows.setparent(ie.hwnd, PanelWebBrowser.handle);
  ie.Left := maxint; // don't understand why it's look impossible to setup the position
  ie.Top  := maxint; // don't understand why it's look impossible to setup the position
  ie.Width := 100;
  ie.Height := 300;
  ie.MenuBar := false;
  ie.AddressBar := false;
  ie.Resizable := false;
  ie.StatusBar := false;
  ie.ToolBar := 0;
  Url := 'http://www.arkadia.com/html/alcinoe_like.html';
  ie.Navigate2(Url,Flags,TargetFrameName,PostData,Headers);
  ie.Visible := true;
end;

{********************************************************************}
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    ie.quit;
  except
  end;
  sleep(500);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
  CS := TcriticalSection.create;

finalization
  CS.free;

end.
