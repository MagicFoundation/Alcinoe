unit uMainForm;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  {$IFNDEF FPC}
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  {$ELSE}
    SysUtils, Variants, Classes, Controls, Forms, StdCtrls,
  {$ENDIF}
  SimpleParser.Lexer.Types;

type
  TIncludeHandler = class(TInterfacedObject, IIncludeHandler)
  private
    FPath: string;
  public
    constructor Create(const Path: string);
    function GetIncludeFileContent(const ParentFileName, IncludeName: string;
      out Content: string; out FileName: string): Boolean;
  end;

  TForm2 = class(TForm)
    memLog: TMemo;
    btnRun: TButton;
    procedure btnRunClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  FileCtrl, IOUtils, DelphiAST, DelphiAST.Classes;

{$R *.dfm}

procedure TForm2.btnRunClick(Sender: TObject);
var
  Path, FileName: string;
  SyntaxTree: TSyntaxNode;
begin
  memLog.Clear;

  Path := ExtractFilePath(Application.ExeName) + 'Snippets\';
  if not SelectDirectory('Select Folder', '', Path) then
    Exit;

  for FileName in TDirectory.GetFiles(Path, '*.pas', TSearchOption.soAllDirectories) do
  begin
    try
      SyntaxTree := TPasSyntaxTreeBuilder.Run(FileName, False, TIncludeHandler.Create(Path));
      try
        memLog.Lines.Add('OK:     ' + FileName);
      finally
        SyntaxTree.Free;
      end;
    except
      on E: Exception do
      begin
        memLog.Lines.Add('FAILED: ' + FileName);
        memLog.Lines.Add('        ' + E.ClassName);
        memLog.Lines.Add('        ' + E.Message);
        memLog.Repaint;
      end;
    end;
  end;
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
    FileName := TPath.Combine(FPath, IncludeName);
    FileContent.LoadFromFile(FileName);
    Content := FileContent.Text;
    Result := True;
  finally
    FileContent.Free;
  end;
end;

end.
