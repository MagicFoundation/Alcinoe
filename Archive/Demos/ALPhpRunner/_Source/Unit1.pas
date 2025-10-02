unit Unit1;

interface

uses
  Windows,
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
  Alcinoe.PhpRunner,
  Alcinoe.StringList;

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
    Label9: TLabel;
    EditPhpFastCgiHost: TEdit;
    EditPhpFastCgiPort: TEdit;
    Label10: TLabel;
    RadioButtonPHPCGIRunnerEngineKind: TRadioButton;
    RadioButtonPHPNamedPipeFastCGIRunnerEngineKind: TRadioButton;
    RadioButtonPHPSocketFastCGIRunnerEngineKind: TRadioButton;
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
    procedure ButtonInitAndGetClick(Sender: TObject);
    procedure ButtonOpenInExplorerClick(Sender: TObject);
    procedure ButtonInitAndPostClick(Sender: TObject);
    procedure ButtonBenchClick(Sender: TObject);
    procedure ButtonExecuteClick(Sender: TObject);
  private
    FThreadCount: Integer;
    FstartTime: dWord;
  public
  end;

  TBenchThread = Class(TThread)
  Public
    PhpRunnerEngine: TAlPHPRunnerEngine;
    NbCycle: Integer;
    serverVariable: TALStringsA;
    Destructor Destroy; override;
    procedure Execute; override;
  end;


var
  Form1: TForm1;
  CS: TcriticalSection;

implementation

Uses
  Alcinoe.MultiPartParser,
  Alcinoe.Files,
  Alcinoe.Common,
  Alcinoe.StringUtils,
  Alcinoe.HTTP.Client;

{$R *.dfm}
{***************************************************}
procedure TForm1.ButtonExecuteClick(Sender: TObject);
Var LResponseHeader: TALHTTPResponseHeader;
    LResponseStream: TALStringStreamA;
    LPhpRunnerEngine: TalPhpRunnerEngine;
    LServerVariablesLst: TALStringListA;
begin
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  LResponseHeader := TALHTTPResponseHeader.Create;
  LResponseStream := TALStringStreamA.Create('');
  LServerVariablesLst:= TALStringListA.Create;
  try
    try
      if RadioButtonPhpCGIRunnerEngineKind.Checked then LPhpRunnerEngine := TalPhpCgiRunnerEngine.Create(AnsiString(EditPhpCGIPath.Text))
      else if RadioButtonPhpNamedPipeFastCGIRunnerEngineKind.Checked then LPhpRunnerEngine := TalPhpNamedPipeFastCgiRunnerEngine.Create(AnsiString(EditPhpCGIPath.Text))
      else LPhpRunnerEngine := TalPhpSocketFastCgiRunnerEngine.Create(AnsiString(EditPhpFastCgiHost.Text), StrToInt(EditPhpFastCgiPort.text));
      Try
        LServerVariablesLst.Assign(MemoServerVariables.lines);
        LPhpRunnerEngine.Execute(
          LServerVariablesLst,
          nil,
          LResponseStream,
          LResponseHeader);
      Finally
        LPhpRunnerEngine.Free;
      End;
      MemoContentBody.Lines.Text := String(LResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := String(LResponseHeader.RawHeaderText);

    except
      MemoContentBody.Lines.Text := String(LResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := String(LResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    LResponseHeader.Free;
    LResponseStream.Free;
    LServerVariablesLst.Free;
  end;
end;

{******************************************************}
procedure TForm1.ButtonInitAndGetClick(Sender: TObject);
Var LResponseHeader: TALHTTPResponseHeader;
    LResponseStream: TALStringStreamA;
    LPhpRunnerEngine: TalPhpRunnerEngine;
    LServerVariablesLst: TALStringListA;
begin
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  LResponseHeader := TALHTTPResponseHeader.Create;
  LResponseStream := TALStringStreamA.Create('');
  LServerVariablesLst:= TALStringListA.Create;
  try
    try
      MemoServerVariables.Lines.Values['REQUEST_METHOD'] := 'GET';
      MemoServerVariables.Lines.Values['PATH_INFO'] := '/' + ExtractFileName(editScriptFileName.Text);
      MemoServerVariables.Lines.Values['PATH_TRANSLATED'] := ExpandFileName(editScriptFileName.Text);
      MemoServerVariables.Lines.Values['SCRIPT_NAME'] := MemoServerVariables.Lines.Values['PATH_INFO'];
      MemoServerVariables.Lines.Values['SCRIPT_FILENAME'] := MemoServerVariables.Lines.Values['PATH_TRANSLATED'];
      MemoServerVariables.Lines.Values['DOCUMENT_ROOT'] := ExtractFilePath(editScriptFileName.Text);
      MemoServerVariables.Lines.Values['REDIRECT_STATUS'] := '1';
      if RadioButtonPhpCGIRunnerEngineKind.Checked then LPhpRunnerEngine := TalPhpCgiRunnerEngine.Create(AnsiString(EditPhpCGIPath.Text))
      else if RadioButtonPhpNamedPipeFastCGIRunnerEngineKind.Checked then LPhpRunnerEngine := TalPhpNamedPipeFastCgiRunnerEngine.Create(AnsiString(EditPhpCGIPath.Text))
      else LPhpRunnerEngine := TalPhpSocketFastCgiRunnerEngine.Create(AnsiString(EditPhpFastCgiHost.Text), StrToInt(EditPhpFastCgiPort.text));
      Try
        LServerVariablesLst.Assign(MemoServerVariables.lines);
        LPhpRunnerEngine.Execute(
          LServerVariablesLst,
          nil,
          LResponseStream,
          LResponseHeader);
      Finally
        LPhpRunnerEngine.Free;
      End;
      MemoContentBody.Lines.Text := String(LResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := String(LResponseHeader.RawHeaderText);

    except
      MemoContentBody.Lines.Text := String(LResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := String(LResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    LServerVariablesLst.Free;
    LResponseHeader.Free;
    LResponseStream.Free;
  end;
end;

{**********************************************************}
procedure TForm1.ButtonOpenInExplorerClick(Sender: TObject);
Var LFullPath: AnsiString;
begin
  LFullPath := ALGetModulePathA + '~tmp.html';
  MemoContentBody.Lines.SaveToFile(String(LFullPath));
  ShellExecuteA(0,'OPEN',PAnsiChar(LFullPath),nil,nil,SW_SHOW)
end;

{*******************************************************}
procedure TForm1.ButtonInitAndPostClick(Sender: TObject);
Var LResponseHeader: TALHTTPResponseHeader;
    LResponseStream: TALStringStreamA;
    LPhpRunnerEngine: TalPhpRunnerEngine;
    LServerVariablesLst: TALStringListA;
    LPostDataStrings: TALStringListA;
begin
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  LResponseHeader := TALHTTPResponseHeader.Create;
  LResponseStream := TALStringStreamA.Create('');
  LServerVariablesLst:= TALStringListA.Create;
  LPostDataStrings:= TALStringListA.Create;
  try
    Try

      MemoServerVariables.Lines.Values['REQUEST_METHOD'] := 'POST';
      MemoServerVariables.Lines.Values['PATH_INFO'] := '/' + ExtractFileName(editScriptFileName.Text);
      MemoServerVariables.Lines.Values['PATH_TRANSLATED'] := ExpandFileName(editScriptFileName.Text);
      MemoServerVariables.Lines.Values['SCRIPT_NAME'] := MemoServerVariables.Lines.Values['PATH_INFO'];
      MemoServerVariables.Lines.Values['SCRIPT_FILENAME'] := MemoServerVariables.Lines.Values['PATH_TRANSLATED'];
      MemoServerVariables.Lines.Values['DOCUMENT_ROOT'] := ExtractFilePath(editScriptFileName.Text);
      MemoServerVariables.Lines.Values['REDIRECT_STATUS'] := '1';
      if RadioButtonPhpCGIRunnerEngineKind.Checked then LPhpRunnerEngine := TalPhpCgiRunnerEngine.Create(AnsiString(EditPhpCGIPath.Text))
      else if RadioButtonPhpNamedPipeFastCGIRunnerEngineKind.Checked then LPhpRunnerEngine := TalPhpNamedPipeFastCgiRunnerEngine.Create(AnsiString(EditPhpCGIPath.Text))
      else LPhpRunnerEngine := TalPhpSocketFastCgiRunnerEngine.Create(AnsiString(EditPhpFastCgiHost.Text), StrToInt(EditPhpFastCgiPort.text));
      try
        LServerVariablesLst.Assign(MemoServerVariables.lines);
        LPostDataStrings.Assign(MemoPostDataStrings.Lines);
        if MemoPostDataStrings.Lines.Count > 0 then
          LPhpRunnerEngine.ExecutePostURLEncoded(
            LServerVariablesLst,
            LPostDataStrings,
            LResponseStream,
            LResponseHeader,
            True)
        else LPhpRunnerEngine.Execute(
               LServerVariablesLst,
               nil,
               LResponseStream,
               LResponseHeader);
      finally
        LPhpRunnerEngine.free;
      end;

      MemoContentBody.Lines.Text := String(LResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := String(LResponseHeader.RawHeaderText);

    Except
      MemoContentBody.Lines.Text := String(LResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := String(LResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    LServerVariablesLst.Free;
    LResponseHeader.Free;
    LResponseStream.Free;
    LPostDataStrings.Free;
  end;
end;

{*************************************************}
procedure TForm1.ButtonBenchClick(Sender: TObject);
Var I: Integer;
    LBenchThread: TBenchThread;
    LPhpNamedPipeFastCgiManager: TalPhpNamedPipeFastCgiManager;
begin
  MemoServerVariables.Lines.Values['REQUEST_METHOD'] := 'GET';
  MemoServerVariables.Lines.Values['PATH_INFO'] := '/' + ExtractFileName(editScriptFileName.Text);
  MemoServerVariables.Lines.Values['PATH_TRANSLATED'] := ExpandFileName(editScriptFileName.Text);
  MemoServerVariables.Lines.Values['SCRIPT_NAME'] := MemoServerVariables.Lines.Values['PATH_INFO'];
  MemoServerVariables.Lines.Values['SCRIPT_FILENAME'] := MemoServerVariables.Lines.Values['PATH_TRANSLATED'];
  MemoServerVariables.Lines.Values['DOCUMENT_ROOT'] := ExtractFilePath(editScriptFileName.Text);
  MemoServerVariables.Lines.Values['REDIRECT_STATUS'] := '1';

  FThreadCount := StrToInt(EditThreadCount.text);
  if RadioButtonPhpNamedPipeFastCGIRunnerEngineKind.Checked then begin
    LPhpNamedPipeFastCgiManager := TalPhpNamedPipeFastCgiManager.Create(AnsiString(EditPhpCGIPath.Text));
    LPhpNamedPipeFastCgiManager.ProcessPoolSize := 8;
    LPhpNamedPipeFastCgiManager.MaxRequestCount := 450;
    LPhpNamedPipeFastCgiManager.Timeout := 10000;
  end
  else LPhpNamedPipeFastCgiManager := nil;
  Form1.MainStatusBar.Panels[0].Text := '0';
  application.ProcessMessages;
  FstartTime := GetTickCount;
  for I := 1 to FThreadCount do begin
    LBenchThread := TBenchThread.Create(true);
    try
      if RadioButtonPhpCGIRunnerEngineKind.Checked then LBenchThread.PhpRunnerEngine := TalPhpCgiRunnerEngine.Create(AnsiString(EditPhpCGIPath.Text))
      else if RadioButtonPhpNamedPipeFastCGIRunnerEngineKind.Checked then LBenchThread.PhpRunnerEngine := LPhpNamedPipeFastCgiManager
      else LBenchThread.PhpRunnerEngine := TalPhpSocketFastCgiRunnerEngine.Create(AnsiString(EditPhpFastCgiHost.Text), StrToInt(EditPhpFastCgiPort.text));
      LBenchThread.serverVariable := TALStringListA.Create;
      LBenchThread.serverVariable.Assign(MemoServerVariables.Lines);
      LBenchThread.NbCycle := StrToInt(EditCycleCount.Text);
      LBenchThread.FreeOnTerminate := True;
      {$IF CompilerVersion >= 23} {Delphi XE2}
      LBenchThread.Start;
      {$ELSE}
      aBenchThread.Resume;
      {$ENDIF}
    Except
      LBenchThread.Free;
      raise;
    end;
  end;
  Form1.Enabled := False;
end;

{******************************}
destructor TBenchThread.Destroy;
begin
  serverVariable.free;
  inherited;
end;

{*****************************}
procedure TBenchThread.Execute;
Var LResponseHeader: TALHTTPResponseHeader;
    LResponseStream: TALStringStreamA;
    I: integer;
begin
  Try

    for I := 1 to NbCycle do begin

      Try

        LResponseHeader := TALHTTPResponseHeader.Create;
        LResponseStream := TALStringStreamA.Create('');
        try
          PhpRunnerEngine.Execute(
            serverVariable,
            nil,
            LResponseStream,
            LResponseHeader);
        finally
          LResponseHeader.Free;
          LResponseStream.Free;
        end;

        if (I mod 100) = 0 then begin
          CS.Acquire;
          try
            Form1.MainStatusBar.Panels[0].Text := IntToStr(StrToInt(Form1.MainStatusBar.Panels[0].Text) + 100);
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
        Form1.MemoBenchResult.Lines.Add(PhpRunnerEngine.ClassName  + ': ' + IntToStr(StrToInt(Form1.EditCycleCount.Text) * StrToInt(Form1.EditThreadCount.text)) + ' requests in ' + IntToStr(GetTickCount - Form1.FstartTime) + ' ms ('+ formatFloat('0.##',(StrToInt(Form1.EditCycleCount.Text) * StrToInt(Form1.EditThreadCount.text)) / ((GetTickCount - Form1.FstartTime) / 1000)) +' request/second)');
        Form1.Enabled := True;
        PhpRunnerEngine.free;
      end
      else if not (PhpRunnerEngine is TalPhpNamedPipeFastCgiManager) then PhpRunnerEngine.free;

    finally
      cs.release;
    end;

  End;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);
  CS := TcriticalSection.create;

finalization
  CS.free;

end.
