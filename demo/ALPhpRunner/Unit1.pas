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
     AlPhpRunner;

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
    Panel1: TPanel;
    Label9: TLabel;
    MemoServerVariables: TMemo;
    ButtonExecute: TButton;
    Label5: TLabel;
    Label7: TLabel;
    procedure ButtonInitAndGetClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonOpenInExplorerClick(Sender: TObject);
    procedure ButtonInitAndPostClick(Sender: TObject);
    procedure ButtonExecuteClick(Sender: TObject);
  private
    FPhpRunnerEngine: TAlPhpRunnerEngine;
  public
  end;

var
  Form1: TForm1;

implementation

Uses ALMultiPartFormDataParser,
     AlFcnFile,
     AlFcnMisc,
     AlFcnMime,
     AlHttpCommon;

{$R *.dfm}

{***********************************************}
procedure TForm1.ButtonInitAndGetClick(Sender: TObject);
Var AResponseHeader: TALHTTPResponseHeader;
    AResponseStream: TStringStream;
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
      FPhpRunnerEngine.Execute(
                               MemoServerVariables.Lines,
                               nil,
                               AResponseStream,
                               AResponseHeader
                              );
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

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  FPhpRunnerEngine.Free;
end;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  FPhpRunnerEngine := TALPhpRunnerEngine.Create();
  FPhpRunnerEngine.LoadDLL('c:\program files\php\php5isapi.dll');
end;

{**********************************************************}
procedure TForm1.ButtonOpenInExplorerClick(Sender: TObject);
Var AFullPath: String;
begin
  AFullPath := ALGetModulePath + '~tmp.html';
  MemoContentBody.Lines.SaveToFile(AFullPath);
  ShellExecute(0,'OPEN',Pchar(AFullPath),nil,nil,SW_SHOW)
end;

{*******************************************************}
procedure TForm1.ButtonInitAndPostClick(Sender: TObject);
Var AResponseHeader: TALHTTPResponseHeader;
    AResponseStream: TStringStream;
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

      if MemoPostDataStrings.Lines.Count > 0 then
        FPhpRunnerEngine.ExecutePostURLEncoded(
                                               MemoServerVariables.Lines,
                                               MemoPostDataStrings.Lines,
                                               AResponseStream,
                                               AResponseHeader,
                                               True
                                              )
      else FPhpRunnerEngine.Execute(
                                    MemoServerVariables.Lines,
                                    nil,
                                    AResponseStream,
                                    AResponseHeader
                                   );

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

{***************************************************}
procedure TForm1.ButtonExecuteClick(Sender: TObject);
Var AResponseHeader: TALHTTPResponseHeader;
    AResponseStream: TStringStream;
begin
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AResponseHeader := TALHTTPResponseHeader.Create;
  AResponseStream := TstringStream.Create('');
  try
    try
      FPhpRunnerEngine.Execute(
                               MemoServerVariables.Lines,
                               nil,
                               AResponseStream,
                               AResponseHeader
                              );
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

end.
