unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, ComCtrls, JclPCRE;

type
  TfrmMain = class(TForm)
    edRegExpr: TEdit;
    btnFind: TButton;
    btnFindNext: TButton;
    Label1: TLabel;
    reFile: TMemo;
    btnOpen: TButton;
    alMain: TActionList;
    acFind: TAction;
    acFindNext: TAction;
    acOpen: TAction;
    odOpen: TOpenDialog;
    chkIgnoreCase: TCheckBox;
    chkMultiLine: TCheckBox;
    chkDotAll: TCheckBox;
    chkExtended: TCheckBox;
    chkAnchored: TCheckBox;
    chkDollarEndOnly: TCheckBox;
    chkExtra: TCheckBox;
    chkNotBOL: TCheckBox;
    chkNotEOL: TCheckBox;
    chkUnGreedy: TCheckBox;
    chkNotEmpty: TCheckBox;
    chkUTF8: TCheckBox;
    sbMain: TStatusBar;
    procedure acOpenExecute(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
    procedure acFindNextExecute(Sender: TObject);
    procedure edRegExprChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }
    RE: TJclAnsiRegEx;
    FMatchIndex: integer;
    procedure SelectText(Offset: TJclAnsiCaptureOffset);
    procedure Match;
    function GetUIOptions: TJclAnsiRegExOptions;
    procedure UpdateUIOptions;
    procedure LoadFromFile(const Filename:string);
  protected
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses
  ShellAPI;

{$R *.dfm}

procedure TfrmMain.acOpenExecute(Sender: TObject);
begin
  if odOpen.Execute then
    LoadFromFile(odOpen.Filename);
end;

procedure TfrmMain.acFindExecute(Sender: TObject);
begin
  FreeAndNil(RE);
  RE := TJclAnsiRegEx.Create;
  RE.Options := GetUIOptions;
  RE.Compile(edRegExpr.Text, false, false);
  FMatchIndex := 1;
  Match;
end;

procedure TfrmMain.acFindNextExecute(Sender: TObject);
begin
  if RE = nil then
    acFind.Execute
  else
    Match;
end;

procedure TfrmMain.SelectText(Offset: TJclAnsiCaptureOffset);
begin
  reFile.SelStart := Offset.FirstPos;
  reFile.SelLength := Offset.LastPos - Offset.FirstPos;
end;

procedure TfrmMain.Match;
begin
  RE.Options := GetUIOptions;
  if not RE.Match(reFile.Lines.Text, FMatchIndex) then
  begin
    FreeAndNil(RE);
    MessageDlg('No matches found', mtInformation, [mbOK], 0);
  end
  else
  begin
    SelectText(RE.CaptureOffset[0]);
    FMatchIndex := RE.CaptureOffset[0].LastPos;
  end;
  UpdateUIOptions;
end;

function TfrmMain.GetUIOptions: TJclAnsiRegExOptions;
begin
  Result := [];
  if chkIgnoreCase.Checked then
    Include(Result, roIgnoreCase);
  if chkMultiLine.Checked then
    Include(Result, roMultiLine);
  if chkDotAll.Checked then
    Include(Result, roDotAll);
  if chkExtended.Checked then
    Include(Result, roExtended);
  if chkAnchored.Checked then
    Include(Result, roAnchored);
  if chkDollarEndOnly.Checked then
    Include(Result, roDollarEndOnly);
  if chkExtra.Checked then
    Include(Result, roExtra);
  if chkNotBOL.Checked then
    Include(Result, roNotBOL);
  if chkNotEOL.Checked then
    Include(Result, roNotEOL);
  if chkUngreedy.Checked then
    Include(Result, roUnGreedy);
  if chkNotEmpty.Checked then
    Include(Result, roNotEmpty);
  if chkUTF8.Checked then
    Include(Result, roUTF8);
end;

procedure TfrmMain.UpdateUIOptions;
var
  Options: TJclAnsiRegExOptions;
begin
  if RE = nil then Exit;
  Options := RE.Options;
  chkIgnoreCase.Checked := roIgnoreCase in Options;
  chkMultiLine.Checked := roMultiLine in Options;
  chkDotAll.Checked := roDotAll in Options;
  chkExtended.Checked := roExtended in Options;
  chkAnchored.Checked := roAnchored in Options;
  chkDollarEndOnly.Checked := roDollarEndOnly in Options;
  chkExtra.Checked := roExtra in Options;
  chkNotBOL.Checked := roNotBOL in Options;
  chkNotEOL.Checked := roNotEOL in Options;
  chkUngreedy.Checked := roUnGreedy in Options;
  chkNotEmpty.Checked := roNotEmpty in Options;
  chkUTF8.Checked := roUTF8 in Options;
end;

procedure TfrmMain.edRegExprChange(Sender: TObject);
begin
  FreeAndNil(RE);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(RE);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, True);
end;

procedure TfrmMain.WMDropFiles(var Message: TWMDropFiles);
var
  i:integer;
  buf:array [0..MAX_PATH] of char;
begin
  i := DragQueryFile(Message.Drop, $FFFFFFFF, nil, 0);
  if i > 0 then
  try
    DragQueryFile(Message.Drop, 0, buf, sizeof(buf));
    if FileExists(buf) then
     LoadFromFile(buf);
  finally
    DragFinish(Message.Drop);
  end;
end;

procedure TfrmMain.LoadFromFile(const Filename: string);
begin
  reFile.Lines.LoadFromFile(Filename);
  sbMain.Panels[0].Text := '  ' + Filename;
end;

end.

