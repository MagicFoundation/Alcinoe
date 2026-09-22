unit uMainForm;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ComCtrls, ExtCtrls;

type
  TMainForm = class(TForm)   
    OutputMemo: TMemo;
    MainMenu: TMainMenu;
    OpenDelphiUnit1: TMenuItem;
    OpenDialog: TOpenDialog;
    StatusBar: TStatusBar;    
    CheckBox1: TCheckBox;
    CommentsBox: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Label1: TLabel;
    Label2: TLabel;
    procedure OpenDelphiUnit1Click(Sender: TObject);  
  private
    procedure UpdateStatusBarText(const StatusText: string);
    procedure Parse(const FileName: string; UseStringInterning: Boolean);
  end;

var
  MainForm: TMainForm;

implementation

uses
  {$IFNDEF FPC}
    StringUsageLogging, FastMM4,
  {$ENDIF}
  StringPool,
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes,
  SimpleParser.Lexer.Types, IOUtils, Diagnostics,
  DelphiAST.SimpleParserEx;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

type
  TIncludeHandler = class(TInterfacedObject, IIncludeHandler)
  private
    FPath: string;
  public
    constructor Create(const Path: string);
    function GetIncludeFileContent(const ParentFileName, IncludeName: string;
      out Content: string; out FileName: string): Boolean;
  end;

{$IFNDEF FPC}
function MemoryUsed: Cardinal;
 var
   st: TMemoryManagerState;
   sb: TSmallBlockTypeState;
 begin
   GetMemoryManagerState(st);
   Result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
   for sb in st.SmallBlockTypeStates do
     Result := Result + sb.UseableBlockSize * sb.AllocatedBlockCount;
end;
{$ELSE}
function MemoryUsed: Cardinal;
begin
  Result := GetFPCHeapStatus.CurrHeapUsed;
end;
{$ENDIF}

procedure TMainForm.Parse(const FileName: string; UseStringInterning: Boolean);
var
  SyntaxTree: TSyntaxNode;
  memused: Cardinal;
  sw: TStopwatch;
  StringPool: TStringPool;
  OnHandleString: TStringEvent;
  Builder: TPasSyntaxTreeBuilder;
  StringStream: TStringStream;
  I: Integer;
begin
  OutputMemo.Clear;
  CommentsBox.Clear;

  try
    if UseStringInterning then
    begin
      StringPool := TStringPool.Create;
      OnHandleString := StringPool.StringIntern;
    end
    else
    begin
      StringPool := nil;
      OnHandleString := nil;
    end;

    memused := MemoryUsed;
    sw := TStopwatch.StartNew;
    try
      Builder := TPasSyntaxTreeBuilder.Create;
      try
        StringStream := TStringStream.Create;
        try
          StringStream.LoadFromFile(FileName);

          Builder.IncludeHandler := TIncludeHandler.Create(ExtractFilePath(FileName));
          Builder.OnHandleString := OnHandleString;
          StringStream.Position := 0;

          SyntaxTree := Builder.Run(StringStream);
          try
            OutputMemo.Lines.Text := TSyntaxTreeWriter.ToXML(SyntaxTree, True);
          finally
            SyntaxTree.Free;
          end;
        finally
          StringStream.Free;
        end;

        for I := 0 to Builder.Comments.Count - 1 do
          CommentsBox.Items.Add(Format('[Line: %d, Col: %d] %s',
            [Builder.Comments[I].Line, Builder.Comments[I].Col, Builder.Comments[I].Text]));
      finally
        Builder.Free;
      end
    finally
      if UseStringInterning then
        StringPool.Free;
    end;
    sw.Stop;

    UpdateStatusBarText(Format('Parsed file in %d ms - used memory: %d K',
      [sw.ElapsedMilliseconds, (MemoryUsed - memused) div 1024]));
  except
    on E: ESyntaxTreeException do
      OutputMemo.Lines.Text := Format('[%d, %d] %s', [E.Line, E.Col, E.Message]) + sLineBreak + sLineBreak +
         TSyntaxTreeWriter.ToXML(E.SyntaxTree, True);
  end;
end;

procedure TMainForm.OpenDelphiUnit1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
    Parse(OpenDialog.FileName, CheckBox1.Checked);
end;

{ TIncludeHandler }

constructor TIncludeHandler.Create(const Path: string);
begin
  inherited Create;
  FPath := Path;
end;

function TIncludeHandler.GetIncludeFileContent(const ParentFileName, IncludeName: string;
  out Content: string; out FileName: string): Boolean;
var
  FileContent: TStringList;
begin
  FileContent := TStringList.Create;
  try
    if not FileExists(TPath.Combine(FPath, IncludeName)) then
    begin
      Result := False;
      Exit;
    end;

    FileContent.LoadFromFile(TPath.Combine(FPath, IncludeName));
    Content := FileContent.Text;
    FileName := TPath.Combine(FPath, IncludeName);

    Result := True;
  finally
    FileContent.Free;
  end;
end;

procedure TMainForm.UpdateStatusBarText(const StatusText: string);
begin
  {$IFDEF FPC}
    StatusBar.SimpleText:= StatusText;
  {$ELSE}
    StatusBar.Panels[0].Text := StatusText;
  {$ENDIF}
end;

end.
