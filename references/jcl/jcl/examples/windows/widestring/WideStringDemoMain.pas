unit WideStringDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Menus, ShellAPI,
  JclFileUtils, JclUnicode, JclSysInfo, JclFont;

type
  TForm1 = class(TForm)
    FileListView:     TListView;
    Panel1:           TPanel;
    FilterEncodingComboBox: TComboBox;
    IncludeSubDirectoriesCheckBox: TCheckBox;
    ConvertButton:    TButton;
    SearchButton:     TButton;
    Label3:           TLabel;
    FileMaskEdit:     TEdit;
    Label2:           TLabel;
    RootDirectoryEdit: TEdit;
    Label1:           TLabel;
    StatusBar1:       TStatusBar;
    FilePopupMenu:    TPopupMenu;
    OpenwithNotepad1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure AddFile(const Directory: string; const FileInfo: TSearchRec);
    procedure TaskDone(const ID: TFileSearchTaskID; const Aborted: boolean);
    procedure FileListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure OpenwithNotepad1Click(Sender: TObject);
    procedure ConvertButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1:           TForm1;
  FileListViewSortAscending: boolean;
  UTF8FileCount:   integer;
  FFileEnumerator: TJclFileEnumerator;
  FTaskID:         TFileSearchTaskID;
  FWideStringList: TWideStringList;

const
  SaveFormatValues: array[TSaveFormat] of string =
    ('Unicode', 'Unicode big endian', 'UTF-8', 'ANSI');

implementation

{$R *.dfm}

function CustomSortByColumn(Item1, Item2: TListItem; iData: integer): integer;
  stdcall;
var
  Str1, Str2: string;
begin
  if (Item1 = nil) or (Item2 = nil) then
  begin
    Result := 0;
    exit;
  end;

  try
    if (iData = 0) then
    begin
      Str1 := Item1.Caption;
      Str2 := Item2.Caption;
    end
    else
    begin
      if (iData >= 1) and (iData <= Item1.SubItems.Count) then
        Str1 := Item1.SubItems[iData - 1]
      else
        Str1 := '';
      if (iData >= 1) and (iData <= Item2.SubItems.Count) then
        Str2 := Item2.SubItems[iData - 1]
      else
        Str2 := '';
    end;
    Result := AnsiCompareStr(Str1, Str2);
  except
    Result := 0;
  end;

  if not FileListViewSortAscending then
    Result := -Result;
end;

procedure TForm1.AddFile(const Directory: string; const FileInfo: TSearchRec);
var
  ListItem: TListItem;
begin
  FWideStringList.LoadFromFile(Directory + FileInfo.Name);

  if (FilterEncodingComboBox.Text = '') or (FilterEncodingComboBox.Text =
    SaveFormatValues[FWideStringList.SaveFormat]) then
  begin
    ListItem := FileListView.Items.Add;
    with ListItem do
    begin
      Caption := FileInfo.Name;
      SubItems.Add(Directory);
      SubItems.Add(SaveFormatValues[FWideStringList.SaveFormat]);
      if (FWideStringList.SaveFormat = sfUTF8) then
        Inc(UTF8FileCount);
    end;
  end;
end;

procedure TForm1.SearchButtonClick(Sender: TObject);
begin
  FFileEnumerator.OnTerminateTask := TaskDone;
  FFileEnumerator.RootDirectory := RootDirectoryEdit.Text;
  FFileEnumerator.FileMask := FileMaskEdit.Text;
  FFileEnumerator.IncludeSubDirectories := IncludeSubDirectoriesCheckBox.Checked;

  FileListView.Items.BeginUpdate;
  FileListView.Items.Clear;
  Screen.Cursor := crHourGlass;
  UTF8FileCount := 0;
  FTaskID := FFileEnumerator.ForEach(AddFile);
end;

procedure TForm1.ConvertButtonClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to FileListView.Items.Count - 1 do
  begin
    if (FileListView.Items[i].SubItems[1] = 'UTF-8') then
    begin
      FWideStringList.LoadFromFile(PChar(FileListView.Items[i].SubItems[0] +
        FileListView.Items[i].Caption));
      if (FWideStringList.SaveFormat = sfUTF8) then
      begin
        FWideStringList.SaveFormat := sfAnsi;
        FWideStringList.SaveToFile(PChar(FileListView.Items[i].SubItems[0] +
          FileListView.Items[i].Caption));
          FileListView.Items[i].SubItems[1] := SaveFormatValues[FWideStringList.SaveFormat];
        Dec(UTF8FileCount);
      end;
    end;
  end;
  ConvertButton.Enabled := (UTF8FileCount > 0);
end;

procedure TForm1.FileListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
  if (Column.Index = TListView(Sender).Tag) then
    FileListViewSortAscending := not FileListViewSortAscending
  else
    TListView(Sender).Tag := Column.Index;
  TListView(Sender).CustomSort(@CustomSortByColumn, Column.Index);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFileEnumerator := TJclFileEnumerator.Create;
  FWideStringList := TWideStringList.Create;

  SetObjectFontToSystemFont(Form1);

  RootDirectoryEdit.Text := ExtractFilePath(Application.ExeName);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FWideStringList.Free;
  FFileEnumerator.Free;
end;

procedure TForm1.OpenwithNotepad1Click(Sender: TObject);
begin
  if (FileListView.Selected <> nil) then
    ShellExecute(Handle, 'open', PChar(GetWindowsFolder + '\notepad.exe'),
      PChar(FileListView.Selected.SubItems[0] + FileListView.Selected.Caption),
      nil, SW_SHOWNORMAL);
end;

procedure TForm1.TaskDone(const ID: TFileSearchTaskID; const Aborted: boolean);
begin
  FileListView.Items.EndUpdate;
  Screen.Cursor := crDefault;
  StatusBar1.SimpleText := Format('%d files', [FileListView.Items.Count]);
  ConvertButton.Enabled := (UTF8FileCount > 0);
end;

end.
