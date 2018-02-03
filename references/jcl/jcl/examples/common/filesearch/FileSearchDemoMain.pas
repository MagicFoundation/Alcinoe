//
// Robert Rossmair, 2003
//
unit FileSearchDemoMain;

{$INCLUDE jcl.inc}

interface

uses
  SysUtils, Classes,
  Graphics, StdCtrls, Controls, ExtCtrls, ComCtrls, Forms, Dialogs,
  JclStrings, JclFileUtils;

type
  TFileSearchForm = class(TForm)
    StatusBar: TStatusBar;
    FileList: TListView;
    Panel1: TPanel;
    Label1: TLabel;
    RootDirInput: TEdit;
    StartBtn: TButton;
    StopBtn: TButton;
    Label2: TLabel;
    DetailsPanel: TPanel;
    GroupBox1: TGroupBox;
    cbReadOnly: TCheckBox;
    cbHidden: TCheckBox;
    cbSystem: TCheckBox;
    cbDirectory: TCheckBox;
    cbSymLink: TCheckBox;
    cbNormal: TCheckBox;
    cbArchive: TCheckBox;
    DetailsBtn: TButton;
    FileMaskInput: TEdit;
    cbLastChangeAfter: TCheckBox;
    edLastChangeAfter: TEdit;
    cbLastChangeBefore: TCheckBox;
    edLastChangeBefore: TEdit;
    cbFileSizeMax: TCheckBox;
    edFileSizeMax: TEdit;
    cbFileSizeMin: TCheckBox;
    edFileSizeMin: TEdit;
    IncludeSubDirectories: TCheckBox;
    IncludeHiddenSubDirs: TCheckBox;
    cbDisplayLiveUpdate: TCheckBox;
    cbCaseInsensitiveSearch: TCheckBox;
    SaveBtn: TButton;
    SaveDialog: TSaveDialog;
    procedure StartBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbFileAttributeClick(Sender: TObject);
    procedure UpdateIncludeHiddenSubDirs(Sender: TObject);
    procedure IncludeHiddenSubDirsClick(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    { Private declarations }
    FFileEnumerator: TJclFileEnumerator;
    FDirCount: Integer;
    FTaskID: TFileSearchTaskID;
    FT0: TDateTime;
    FFileListLiveUpdate: Boolean;
    procedure DirectoryEntered(const Directory: string);
    procedure AddFile(const Directory: string; const FileInfo: TSearchRec);
    procedure TaskDone(const ID: TFileSearchTaskID; const Aborted: Boolean);
  end;

var
  FileSearchForm: TFileSearchForm;

implementation

{$R *.dfm}

procedure TFileSearchForm.FormCreate(Sender: TObject);
begin
  FFileEnumerator := TJclFileEnumerator.Create;
  FFileEnumerator.OnEnterDirectory := DirectoryEntered;
  FFileEnumerator.OnTerminateTask := TaskDone;
  FileMaskInput.Text := '*.pas;*.dfm;*.xfm;*.dpr;*.dpk*';
  RootDirInput.Text := ExpandFileName(FFileEnumerator.RootDirectory);
  edLastChangeAfter.Text := FFileEnumerator.LastChangeAfterAsString;
  edLastChangeBefore.Text := FFileEnumerator.LastChangeBeforeAsString;
  cbCaseInsensitiveSearch.Checked := not FFileEnumerator.CaseSensitiveSearch;
  {$IFDEF MSWINDOWS}
  cbSymLink.Visible := False;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  FileList.Columns.Add.Caption := 'Link';
  cbArchive.Visible := False;
  {$ENDIF UNIX}
end;

procedure TFileSearchForm.FormDestroy(Sender: TObject);
begin
  FFileEnumerator.Free;
  {
  FileList.Items.BeginUpdate;
  FileList.Items.Clear;
  FileList.Items.EndUpdate;
  }
end;

procedure TFileSearchForm.DirectoryEntered(const Directory: string);
begin
  Inc(FDirCount);
  StatusBar.Panels[0].Text := Format('%d files', [FileList.Items.Count]);
  StatusBar.Panels[1].Text := Format('%d directories', [FDirCount]);
  StatusBar.Panels[2].Text := Format('Processing %s...', [Directory]);
end;

procedure TFileSearchForm.AddFile(const Directory: string; const FileInfo: TSearchRec);
var
  ListItem: TListItem;
  FileDateTime: TDateTime;
begin
  ListItem := FileList.Items.Add;
  with ListItem do
  begin
    Caption := Directory + FileInfo.Name;
    SubItems.Add(IntToStr(GetSizeOfFile(FileInfo)));
    {$IFDEF RTL230_UP}
    FileDateTime := FileInfo.TimeStamp;
    {$ELSE ~RTL230_UP}
    FileDateTime := FileDateToDateTime(FileInfo.Time);
    {$ENDIF ~RTL230_UP}
    SubItems.Add(FormatDateTime(' yyyy-mm-dd hh:nn:ss ', FileDateTime));
    SubItems.Add(FileAttributesStr(FileInfo));
    {$IFDEF UNIX}
    if (FileInfo.Attr and faSymLink) <> 0 then
      SubItems.Add(SymbolicLinkTarget(Caption));
    {$ENDIF UNIX}
    SubItems.Add(FileGetOwnerName(Caption));
    SubItems.Add(FileGetGroupName(Caption));
  end;
end;

procedure TFileSearchForm.TaskDone(const ID: TFileSearchTaskID; const Aborted: Boolean);
begin
  if not FFileListLiveUpdate then
    FileList.Items.EndUpdate;
  StatusBar.Panels[0].Text := Format('%d files', [FileList.Items.Count]);
  if Aborted then
    StatusBar.Panels[2].Text := 'Prematurely aborted.'
  else
    StatusBar.Panels[2].Text := Format('...finished (%f seconds).', [(Now - FT0) * SecsPerDay]);
  StartBtn.Enabled := True;
  SaveBtn.Enabled := True;
  StopBtn.Enabled := False;
  ActiveControl := StartBtn;
end;

procedure TFileSearchForm.StartBtnClick(Sender: TObject);
var
  RootDirectories: TStrings;
begin
  RootDirInput.Text := PathCanonicalize(RootDirInput.Text);

  FFileEnumerator.SearchOption[fsLastChangeAfter] := cbLastChangeAfter.Checked;
  FFileEnumerator.SearchOption[fsLastChangeBefore] := cbLastChangeBefore.Checked;
  if FFileEnumerator.SearchOption[fsLastChangeAfter] then
    FFileEnumerator.LastChangeAfterAsString := edLastChangeAfter.Text;
  if FFileEnumerator.SearchOption[fsLastChangeBefore] then
    FFileEnumerator.LastChangeBeforeAsString := edLastChangeBefore.Text;
  RootDirectories := TStringList.Create;
  try
    StrToStrings(RootDirInput.Text, DirSeparator, RootDirectories, False);
    FFileEnumerator.RootDirectories := RootDirectories;
  finally
    RootDirectories.Free;
  end;
  FFileEnumerator.FileMask := FileMaskInput.Text;
  FFileEnumerator.SearchOption[fsMinSize] := cbFileSizeMin.Checked;
  FFileEnumerator.SearchOption[fsMaxSize] := cbFileSizeMax.Checked;
  FFileEnumerator.FileSizeMin := StrToInt64(edFileSizeMin.Text);
  FFileEnumerator.FileSizeMax := StrToInt64(edFileSizeMax.Text);
  FFileEnumerator.IncludeSubDirectories := IncludeSubDirectories.Checked;
  FFileEnumerator.IncludeHiddenSubDirectories := IncludeHiddenSubDirs.Checked;
  FFileEnumerator.CaseSensitiveSearch := not cbCaseInsensitiveSearch.Checked; 
  FDirCount := 0;

  StartBtn.Enabled := False;
  StopBtn.Enabled := True;
  SaveBtn.Enabled := False;
  ActiveControl := StopBtn;

  FFileListLiveUpdate := cbDisplayLiveUpdate.Checked;

  FileList.Items.Clear;
  if not FFileListLiveUpdate then
    FileList.Items.BeginUpdate;

  FT0 := Now;
  FTaskID := FFileEnumerator.ForEach(AddFile);
end;

procedure TFileSearchForm.StopBtnClick(Sender: TObject);
begin
  FFileEnumerator.StopTask(FTaskID);
end;

procedure TFileSearchForm.cbFileAttributeClick(Sender: TObject);
const
  Interest: array[TCheckBoxState] of TAttributeInterest = (aiRejected, aiRequired, aiIgnored);
  CBState: array[TAttributeInterest] of TCheckBoxState = (cbGrayed, cbUnchecked, cbChecked);
begin
  with FFileEnumerator.AttributeMask do
  begin
    with Sender as TCheckBox do
      Attribute[Tag] := Interest[State];
    cbReadOnly.State := CBState[ReadOnly];
    cbHidden.State := CBState[Hidden];
    cbSystem.State := CBState[System];
    cbDirectory.State := CBState[Directory];
    cbNormal.State := CBState[Normal];
{$IFDEF UNIX}
    cbSymLink.State := CBState[SymLink];
{$ENDIF def UNIX}
{$IFDEF MSWINDOWS}
    cbArchive.State := CBState[Archive];
{$ENDIF def MSWINDOWS}
  end;
end;

procedure TFileSearchForm.UpdateIncludeHiddenSubDirs(Sender: TObject);
begin
  IncludeHiddenSubDirs.AllowGrayed := not IncludeSubDirectories.Checked;
  if IncludeSubDirectories.Checked then
  begin
    if IncludeHiddenSubDirs.State = cbGrayed then
      IncludeHiddenSubDirs.State := cbChecked;
  end
  else
  begin
    if IncludeHiddenSubDirs.State = cbChecked then
      IncludeHiddenSubDirs.State := cbGrayed;
  end;
end;

procedure TFileSearchForm.IncludeHiddenSubDirsClick(Sender: TObject);
begin
  if not IncludeSubDirectories.Checked then
    if IncludeHiddenSubDirs.State = cbChecked then
        IncludeHiddenSubDirs.State := cbUnchecked;
end;

procedure TFileSearchForm.DetailsBtnClick(Sender: TObject);
const
  DetailsBtnCaptions: array[Boolean] of string = ('More >>', 'Less <<');
begin
  DetailsPanel.Visible := not DetailsPanel.Visible;
  DetailsBtn.Caption := DetailsBtnCaptions[DetailsPanel.Visible];
end;

procedure TFileSearchForm.SaveBtnClick(Sender: TObject);
var
  I: Integer;
begin
  if SaveDialog.Execute then
    with TStringList.Create do
    try
      for I := 0 to FileList.Items.Count - 1 do
        Add(FileList.Items[I].Caption);
      SaveToFile(SaveDialog.FileName);
    finally
      Free;
    end;
end;

end.

