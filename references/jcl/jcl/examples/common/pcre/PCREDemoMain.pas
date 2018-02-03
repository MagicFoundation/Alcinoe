unit PCREDemoMain;

{$I ..\..\..\source\include\jcl.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes, Forms, Dialogs, ActnList, ComCtrls, StdCtrls, Controls,
  JclPCRE, {System.Actions, Vcl.}ExtCtrls;

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
    sbMain: TStatusBar;
    GroupBoxMatchOptions: TGroupBox;
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
    GroupBoxCompileOptions: TGroupBox;
    chkStudy: TCheckBox;
    chkUserLocale: TCheckBox;
    chkJITCompile: TCheckBox;
    GroupBoxSystemOptions: TGroupBox;
    RadioButtonDefault: TRadioButton;
    RadioButtonUTF8: TRadioButton;
    RadioButtonUCS2: TRadioButton;
    RadioButtonUTF16: TRadioButton;
    RadioButtonANSI: TRadioButton;
    procedure acOpenExecute(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
    procedure acFindNextExecute(Sender: TObject);
    procedure edRegExprChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    { Private declarations }
    {$IFDEF JCL_PCRE}
    RE: TJclRegExBase;
    {$ENDIF JCL_PCRE}
    FMatchIndex: integer;
    procedure SelectText(const Range: TJclCaptureRange);
    procedure Match;
    function GetUIOptions: TJclRegExOptions;
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
  {$IFDEF JCL_PCRE}
  FreeAndNil(RE);
  {$IFDEF PCRE_8}
  if RadioButtonANSI.Checked or RadioButtonUTF8.Checked then
    RE := TJclAnsiRegEx.Create
  else
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  if RadioButtonUCS2.Checked or RadioButtonUTF16.Checked then
    RE := TJclWideRegEx.Create
  else
  {$ENDIF PCRE_16}
  if RadioButtonDefault.Checked then
    RE := TJclRegEx.Create;
  RE.Options := GetUIOptions;
  RE.Compile(edRegExpr.Text, chkStudy.Checked, chkUserLocale.Checked, chkJITCompile.Checked);
  FMatchIndex := 1;
  Match;
  {$ENDIF JCL_PCRE}
end;

procedure TfrmMain.acFindNextExecute(Sender: TObject);
begin
  {$IFDEF JCL_PCRE}
  if RE = nil then
    acFind.Execute
  else
    Match;
  {$ENDIF JCL_PCRE}
end;

procedure TfrmMain.SelectText(const Range: TJclCaptureRange);
begin
  reFile.SelStart := Range.FirstPos - 1;
  reFile.SelLength := Range.LastPos - Range.FirstPos + 1;
end;

procedure TfrmMain.Match;
begin
  {$IFDEF JCL_PCRE}
  RE.Options := GetUIOptions;
  if not RE.Match(reFile.Lines.Text, FMatchIndex) then
  begin
    FreeAndNil(RE);
    MessageDlg('No matches found', mtInformation, [mbOK], 0);
  end
  else
  begin
    SelectText(RE.CaptureRanges[0]);
    FMatchIndex := RE.CaptureRanges[0].LastPos + 1;
  end;
  UpdateUIOptions;
  {$ENDIF JCL_PCRE}
end;

function TfrmMain.GetUIOptions: TJclRegExOptions;
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
  if RadioButtonUTF8.Checked then
    Include(Result, roUTF8);
  if RadioButtonUTF16.Checked then
    Include(Result, roUTF16);
end;

procedure TfrmMain.UpdateUIOptions;
{$IFDEF JCL_PCRE}
var
  Options: TJclRegExOptions;
{$ENDIF JCL_PCRE}
begin
  {$IFDEF JCL_PCRE}
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
  {$IFDEF PCRE_8}
  if RE is TJclAnsiRegEx then
  begin
    if roUTF8 in Options then
      RadioButtonUTF8.Checked := True
    else
      RadioButtonAnsi.Checked := True;
  end;
  {$ENDIF PCRE_8}
  {$IFDEF PCRE_16}
  if RE is TJclWideRegEx then
  begin
    if roUTF16 in Options then
      RadioButtonUTF16.Checked := True
    else
      RadioButtonUCS2.Checked := True;
  end;
  {$ENDIF PCRE_16}
  {$ENDIF JCL_PCRE}
end;

procedure TfrmMain.edRegExprChange(Sender: TObject);
begin
  {$IFDEF JCL_PCRE}
  FreeAndNil(RE);
  {$ENDIF JCL_PCRE}
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  {$IFDEF JCL_PCRE}
  FreeAndNil(RE);
  {$ENDIF JCL_PCRE}
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, True);
  {$IFDEF JCL_PCRE}
  RadioButtonDefault.Checked := True;
  {$ELSE ~JCL_PCRE}
  RadioButtonDefault.Enabled := False;
  {$ENDIF ~JCL_PCRE}
  {$IFNDEF PCRE_8}
  RadioButtonANSI.Enabled := False;
  RadioButtonUTF8.Enabled := False;
  {$ENDIF ~PCRE_8}
  {$IFNDEF PCRE_16}
  RadioButtonUCS2.Enabled := False;
  RadioButtonUTF16.Enabled := False;
  {$ENDIF ~PCRE_16}
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

