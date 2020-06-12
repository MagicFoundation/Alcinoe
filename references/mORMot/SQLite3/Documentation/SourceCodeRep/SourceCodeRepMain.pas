unit SourceCodeRepMain;

{$I ../../../Synopse.inc}

interface

uses
  {$IFNDEF FPC}
  Windows,
  {$ELSE}
  LCLIntf,
  LCLType,
  LMessages,
  {$ENDIF}
  SynCommons,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Clipbrd;

const
  VERSION = '1.18';

type

  { TMainForm }

  TMainForm = class(TForm)
    mmoStatus: TMemo;
    lbl1: TLabel;
    lbl2: TLabel;
    mmoDescription: TMemo;
    btnFossilSynch: TButton;
    btnFullSynch: TButton;
    btnGitSynch: TButton;
    btnRefreshStatus: TButton;
    btnGitShell: TButton;
    btnFossilShell: TButton;
    btnTests: TButton;
    btnCopyLink: TButton;
    btnGitAll: TButton;
    btnSynProject: TButton;
    btnSynPdf: TButton;
    btnDMustache: TButton;
    btnLVCL: TButton;
    chkCopyLink: TCheckBox;
    chkFossilPush: TCheckBox;
    chkFossilPull: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnFullSynchClick(Sender: TObject);
    procedure btnFossilSynchClick(Sender: TObject);
    procedure btnGitSynchClick(Sender: TObject);
    procedure btnRefreshStatusClick(Sender: TObject);
    procedure btnGitShellClick(Sender: TObject);
    procedure btnFossilShellClick(Sender: TObject);
    procedure btnTestsClick(Sender: TObject);
    procedure btnCopyLinkClick(Sender: TObject);
  private
    fBatPath: TFileName;
    fFossilRepository: TFileName;
    fDevPath: TFileName;
    fGitExe: TFileName;
    fGitRepository: TFileName;
    function Exec(const folder, exe, arg1, arg2, arg3, arg4, arg5: TFileName;
      exeisshell: boolean=true; wait: boolean=true): boolean;
    procedure ReadStatus;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  mORMotService; // for cross-platform RunProcess()

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

{$ifdef MSWINDOWS}
  {$R ..\..\..\vista.RES} // includes Win10 manifest - use .RES for linux cross-compilation
const
  SHELL = '.bat';
  SHELLEXE = 'cmd.exe';
  GITDEF = 'git.exe';
  REPFOSSIL = 'd:\dev\fossil\lib';
  REPLIB = 'd:\dev\lib';
  REPGITHUB = 'd:\dev\github\';
{$else}
const
  SHELL = '.sh';
  GITDEF = '/usr/bin/git';
var
  REPFOSSIL: TFileName;
  REPLIB: TFileName;
  REPGITHUB: TFileName;
{$endif}

function TMainForm.Exec(const folder, exe, arg1, arg2, arg3, arg4, arg5: TFileName;
  exeisshell, wait: boolean): boolean;
var
  bak, path: TFileName;
  {$ifdef MSWINDOWS}
  function q(const a: TFileName): TFileName;
  begin
    result := '"' + a + '"'; // paranoid quote for safety
  end;
  {$else}
  type q = TFileName;
  {$endif}
begin
  if folder <> '' then begin
    bak := GetCurrentDir;
    SetCurrentDir(folder);
  end;
  if exeisshell then
    path := fBatPath + exe + SHELL
  else
    path := exe;
  screen.Cursor := crHourGlass;
  try
    result := RunProcess(path, q(arg1), wait, q(arg2), q(arg3), q(arg4), q(arg5)) = 0;
  finally
    if bak <> '' then
      SetCurrentDir(bak);
    screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ReadStatus;
var
  statusfile: TFileName;
  status: RawUTF8;
begin
  statusfile := fBatPath + 'status.txt';
  DeleteFile(statusfile);
  if not Exec(fFossilRepository, 'FossilStatus', statusfile, '', '', '', '') then
    status := 'error executing FossilStatus script'
  else
    status := StringFromFile(statusfile);
  {$ifdef MSWINDOWS}
  if PosEx(#13#10, status) = 0 then
    status := StringReplaceAll(status, #10, #13#10);
  {$endif}
  mmoStatus.Text := UTF8ToString(status);
end;

procedure TMainForm.FormCreate(Sender: TObject);
{$ifdef MSWINDOWS}
begin
{$else}
var
  dev: TFileName;
begin
  dev := GetSystemPath(spUserDocuments) + 'dev/';
  REPFOSSIL := dev + 'fossil/lib';
  REPLIB := dev + 'lib';
  REPGITHUB := dev + 'github/';
  btnFossilShell.caption := 'Fossil diff';
  btnGitShell.caption := 'Git diff';
{$endif MSWINDOWS}
  fBatPath := ExeVersion.ProgramFilePath;
  if not FileExists(fBatPath + 'FossilStatus' + SHELL) then // from exe sub-folder?
    fBatPath := ExtractFilePath(ExcludeTrailingPathDelimiter(fBatPath));
  if not FileExists(fBatPath + 'FossilStatus' + SHELL) then // from exe sub-folder?
    fBatPath := ExtractFilePath(ExcludeTrailingPathDelimiter(fBatPath));
  if not FileExists(fBatPath + 'FossilStatus' + SHELL) then
    ShowMessage('Missing *' + SHELL +' files');
  fFossilRepository := GetEnvironmentVariable('SYN_FOSSILREPO_PATH');
  if fFossilRepository = '' then
    fFossilRepository := REPFOSSIL;
  fDevPath := GetEnvironmentVariable('SYN_DEVPATH');
  if fDevPath = '' then
    if DirectoryExists(REPLIB) then
      fDevPath := REPLIB else
      fDevPath := fFossilRepository;
  fGitExe := GetEnvironmentVariable('GIT_PATH');
  if fGitExe = '' then begin
    {$ifdef MSWINDOWS}
    fGitExe := 'c:\Program Files (x86)\Git\bin\git.exe';
    if not FileExists(fGitExe) then
    {$endif}
      fGitExe := GITDEF;
  end;
  fGitRepository := GetEnvironmentVariable('SYN_GITREPO_PATH');
  if fGitRepository = '' then
    fGitRepository := REPGITHUB + 'mORMot';
  if not DirectoryExists(fFossilRepository) then begin
    ShowMessage('Please set Fossil Repository Name or environment variable SYN_FOSSILREPO_PATH');
    Close;
  end else if not DirectoryExists(fGitRepository) then begin
    ShowMessage('Please set Git Repository Path or environment variable SYN_GITREPO_PATH');
    Close;
  end else if ((fGitExe <> GITDEF) and not FileExists(fGitExe)) or
       ((fGitExe = GITDEF) and
        not Exec(fGitRepository, GITDEF, 'status', '', '', '', '', {isshell=}false)) then begin
    ShowMessage('Please install Git or set environment variable GIT_PATH');
    Close;
  end else
    ReadStatus;
end;

procedure TMainForm.btnFullSynchClick(Sender: TObject);
begin
  btnFossilSynch.Click;
  btnGitSynch.Click;
  if chkCopyLink.Checked then
    btnCopyLink.Click;
end;

procedure TMainForm.btnFossilSynchClick(Sender: TObject);
var
  Desc: string;
  DescFile: TFileName;
  VersionNumber: integer;
  VersionText: RawUTF8;
begin
  Desc := trim(mmoDescription.Text);
  if Desc = '' then begin
    ShowMessage('Missing description');
    mmoDescription.SetFocus;
    exit;
  end;
  if chkFossilPull.Checked then
    Exec(fFossilRepository, 'FossilUpdate', '', '', '', '', '');
  VersionText := UnQuoteSQLString(StringFromFile(fDevPath + PathDelim + 'SynopseCommit.inc'));
  VersionText := GetCSVItem(pointer(VersionText), 2, '.');
  VersionNumber := GetCardinalDef(pointer(VersionText), 255);
  inc(VersionNumber);
  VersionText := '''' + VERSION + '.' + UInt32ToUtf8(VersionNumber) + ''''#13#10;
  FileFromString(VersionText, fDevPath + PathDelim +'SynopseCommit.inc');
  FileFromString(VersionText, fFossilRepository + PathDelim + 'SynopseCommit.inc');
  DescFile := fBatPath + 'desc.txt';
  FileFromString('{' + ToUTF8(VersionNumber) + '} ' + Desc, DescFile);
  Exec(fFossilRepository, 'FossilCommit', DescFile, IntToStr(ord(chkFossilPush.Checked)), fFossilRepository, '', '');
  btnRefreshStatus.Click;
end;

procedure TMainForm.btnGitSynchClick(Sender: TObject);
var
  Desc, status: string;
  DescFile, BatchFile, GitHub: TFileName;
  i,n: integer;
begin
  Desc := trim(mmoDescription.Text);
  if Desc = '' then begin
    status := mmoStatus.Text;
    i := pos('comment:', status);
    if i > 0 then begin
      delete(status, 1, i + 8);
      with TStringList.Create do
      try
        Text := trim(status);
        status := Strings[0];
        for i := 1 to Count - 1 do
          if copy(Strings[i], 1, 3) = '   ' then
            status := status + ' ' + trim(Strings[i])
          else
            break;
      finally
        Free;
      end;
      i := pos('(user: ', status);
      if i > 0 then
        SetLength(status, i - 1);
      i := pos('} ', status);
      if (i > 0) and (i < 10) then
        delete(status, 1, i + 1); // trim left '{256} '
      mmoDescription.Text := trim(status);
    end
    else begin
      ShowMessage('Missing description');
      mmoDescription.SetFocus;
    end;
    exit;
  end;
  if not DirectoryExists(fGitRepository) then begin
    ShowMessage('Please set Git Repository Name');
    exit;
  end;
  DescFile := fBatPath + 'desc.txt';
  FileFromString(Desc, DescFile);
  GitHub := ExtractFilePath(fGitRepository);
  n := 0;
  if (Sender = btnGitAll) or (Sender = btnSynProject) then
    inc(n,SynchFolders(fFossilRepository, GitHub + 'SynProject', true, true, true));
  if (Sender = btnGitAll) or (Sender = btnSynPdf) then
    inc(n,SynchFolders(fFossilRepository, GitHub + 'SynPDF', false, true, true));
  if (Sender = btnGitAll) or (Sender = btnDMustache) then
    inc(n,SynchFolders(fFossilRepository, GitHub + 'dmustache', false, true, true));
  if (Sender = btnGitAll) or (Sender = btnLVCL) then
    inc(n,SynchFolders(fFossilRepository, GitHub + 'LVCL', false, true, true));
  if (Sender = btnGitAll) or (Sender = btnGitSynch) then
    inc(n,SynchFolders(fFossilRepository, GitHub + 'mORMot', true, true, true));
  {$I-} Writeln(n,' file(s) synched to GitHub'#13#10); {$I+}
  if Sender = btnGitAll then
    BatchFile := 'GitCommitAll'
  else if Sender = btnSynProject then
    BatchFile := 'GitCommitSynProject'
  else if Sender = btnSynPdf then
    BatchFile := 'GitCommitSynPdf'
  else if Sender = btnDMustache then
    BatchFile := 'GitCommitDMustache'
  else if Sender = btnLVCL then
    BatchFile := 'GitCommitLVCL'
  else
    BatchFile := 'GitCommit';
  Exec(fGitRepository, BatchFile, fFossilRepository, fGitRepository, fGitExe, DescFile, fDevPath);
  mmoDescription.SetFocus; // ReadStatus not necessary if git only
end;

procedure TMainForm.btnRefreshStatusClick(Sender: TObject);
begin
  ReadStatus;
  mmoDescription.SetFocus;
  mmoDescription.SelectAll;
end;

procedure TMainForm.btnGitShellClick(Sender: TObject);
begin
  {$ifdef MSWINDOWS}
  Exec(fGitRepository, SHELLEXE, '', '', '', '', '');
  {$else}
  Exec(fGitRepository, '/usr/bin/meld', fGitRepository, fDevPath, '', '', '', false, false);
  {$endif}
end;

procedure TMainForm.btnFossilShellClick(Sender: TObject);
begin
  {$ifdef MSWINDOWS}
  Exec(fFossilRepository, SHELLEXE, '', '', '', '', '');
  {$else}
  Exec(fFossilRepository, '/usr/bin/meld', fFossilRepository, fDevPath, '', '', '', false, false);
  {$endif}
end;

procedure TMainForm.btnTestsClick(Sender: TObject);
begin
  {$ifdef MSWINDOWS}
  Exec(fDevPath, 'compilpil', '', '', '', '', '');
  {$endif}
end;

procedure TMainForm.btnCopyLinkClick(Sender: TObject);
var
  i: integer;
  status: string;
begin
  status := mmoStatus.Lines.Text;
  i := pos('checkout:', status);
  if i < 0 then
    exit;
  inc(i, 10);
  while (i < length(status)) and (status[i] <= ' ') do
    inc(i);
  Clipboard.AsText := 'https://synopse.info/fossil/info/' + copy(status, i, 10);
end;

end.

