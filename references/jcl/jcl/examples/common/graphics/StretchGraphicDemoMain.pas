//
// Robert Rossmair, 2002-09-22
//          revised 2005-06-26
//

unit StretchGraphicDemoMain;

{$I jcl.inc}
{$I crossplatform.inc}

{$IFDEF VCL}
  {$DEFINE HasShellCtrls} // $(Delphi)\Demos\ShellControls\ShellCtrls.pas
{$ENDIF VCL}

interface

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows, Messages, JPEG, ShellAPI,
  {$ENDIF MSWINDOWS}
  Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Menus, ExtCtrls, ExtDlgs,
  JclGraphics,
  {$IFDEF HasShellCtrls}
  ShellCtrls,
  {$ENDIF HasShellCtrls}
  JclFileUtils;

type
  TStretchDemoForm = class(TForm)
    PageControl: TPageControl;
    OriginalPage: TTabSheet;
    StretchedPage: TTabSheet;
    StretchedImage: TImage;
    MainMenu: TMainMenu;
    Fil1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    ExitItem: TMenuItem;
    Filter1: TMenuItem;
    Box1: TMenuItem;
    Triangle1: TMenuItem;
    Hermite1: TMenuItem;
    Bell1: TMenuItem;
    Spline1: TMenuItem;
    Lanczos31: TMenuItem;
    Mitchell1: TMenuItem;
    Options1: TMenuItem;
    PreserveAspectRatio1: TMenuItem;
    PrevItem: TMenuItem;
    NextItem: TMenuItem;
    FilesPage: TTabSheet;
    ScrollBox: TScrollBox;
    StatusBar: TStatusBar;
    Bevel1: TBevel;
    OpenDialog: TOpenDialog;
    FileListView: TListView;
    OriginalImage: TImage;
    procedure FormCreate(Sender: TObject);
    {$IFDEF VCL}
    procedure FormDestroy(Sender: TObject);
    {$ENDIF VCL}
    procedure OpenFile(Sender: TObject);
    procedure SelectFilter(Sender: TObject);
    procedure PreserveAspectRatio1Click(Sender: TObject);
    procedure ExitApp(Sender: TObject);
    procedure PrevFile(Sender: TObject);
    procedure NextFile(Sender: TObject);
    procedure FileListViewClick(Sender: TObject);
    procedure LoadSelected;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StretchedPageShow(Sender: TObject);
    procedure StretchedPageResize(Sender: TObject);
    procedure PageControlChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure FileListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  {$IFDEF HasShellCtrls}
    procedure ShellChange;
  private
    FShellChangeNotifier: TShellChangeNotifier;
  {$ELSE}
  private
  {$ENDIF HasShellCtrls}
    FLastImagePage: TTabSheet;
    FFileName: string;
    FDir: string;
    FWidth: Integer;
    FHeight: Integer;
    FStretchTime: LongWord;
    FPreserveAspectRatio: Boolean;
    FResamplingFilter: TResamplingFilter;
    procedure AddToFileList(const Directory: string; const FileInfo: TSearchRec);
    procedure FileSearchTerminated(const ID: TFileSearchTaskID; const Aborted: Boolean);
    function ChangeDirectory: Boolean;
    procedure DoStretch;
    procedure LoadFile(const AFileName: string);
    procedure InvalidateStretched;
    procedure UpdateCaption;
    procedure UpdateFileList;
    procedure UpdateNavButtons;
    procedure UpdateStretched;
    function GetFileListIndex: Integer;
    procedure SetFileListIndex(const Value: Integer);
    procedure SetFileName(const Value: string);
    {$IFDEF VCL}
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DropFiles;
    {$ENDIF VCL}
  protected
    property FileListIndex: Integer read GetFileListIndex write SetFileListIndex;
    property FileName: string read FFileName write SetFileName;
  end;

var
  StretchDemoForm: TStretchDemoForm;

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}

var
  FileMask: string;

{$IFDEF MSWINDOWS}
type
  TWMDropFilesCallback = procedure (const FileName: string) of object;

procedure ProcessWMDropFiles(var Msg: TWMDropFiles; Callback: TWMDropFilesCallback; DropPoint: PPoint = nil); overload;
var
  i: Integer;
  FileName: array[0..MAX_PATH] of Char;
begin
  try
    // in case DropPoint is evaluated by callback method, get it first
    if DropPoint <> nil then
      DragQueryPoint(Msg.Drop, DropPoint^);
    if Assigned(Callback) then
      for i := 0 to DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0) - 1 do
      begin
        DragQueryFile(Msg.Drop, i, FileName, MAX_PATH);
        Callback(FileName);
      end;
    Msg.Result := 0;
  finally
    DragFinish(Msg.Drop);
  end;
end;

procedure ProcessWMDropFiles(var Msg: TWMDropFiles; FileNames: TStrings; DropPoint: PPoint = nil); overload;
begin
  ProcessWMDropFiles(Msg, FileNames.Append, DropPoint);
end;
{$ENDIF MSWINDOWS}

function IsGraphicFile(const FileName: string): Boolean; overload;
var
  Ext: string;
begin
  Ext := AnsiLowerCase(ExtractFileExt(FileName));
  Result := (Pos(Ext, FileMask) > 0);
end;

function IsGraphicFile(const Attr: Integer; const FileInfo: TSearchRec): Boolean; overload;
begin
  Result := IsGraphicFile(FileInfo.Name);
end;

procedure TStretchDemoForm.FormCreate(Sender: TObject);
begin
  StretchedPage.Brush.Color := clGray;
  {$IFDEF VCL}
  ScrollBox.DoubleBuffered := True;
  StretchedPage.DoubleBuffered := True;
  {$ENDIF VCL}
  FileMask := GraphicFileMask(TGraphic);
  //Format('%s;%s', [GraphicFileMask(TJPEGImage), GraphicFileMask(TBitmap)]);
  OpenDialog.Filter := GraphicFilter(TGraphic);
  FResamplingFilter := rfSpline; // rfLanczos3;
  FPreserveAspectRatio := True;
  UpdateNavButtons;
  {$IFDEF HasShellCtrls}
  FShellChangeNotifier := TShellChangeNotifier.Create(Self);
  with FShellChangeNotifier do
  begin
    WatchSubTree := False;
    OnChange := ShellChange;
    NotifyFilters := [
        nfFileNameChange,
        nfDirNameChange,
        //nfSizeChange,
        nfWriteChange,
        nfSecurityChange];
  end;
  {$ENDIF HasShellCtrls}
  {$IFDEF VCL}
  DragAcceptFiles(Handle, True);
  {$ENDIF VCL}
  if ParamCount > 0 then
  with OpenDialog do
  begin
    FileName := ParamStr(1);
    InitialDir := ExtractFileDir(FileName);
    LoadFile(FileName);
  end;
end;

{$IFDEF VCL}
procedure TStretchDemoForm.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
end;
{$ENDIF VCL}

procedure TStretchDemoForm.ExitApp(Sender: TObject);
begin
  Close;
end;

function TStretchDemoForm.ChangeDirectory: Boolean;
var
  Dir, D: string;
begin
  D := ExtractFileDir(FileName);
  Dir := PathAddSeparator(D);
  Result := (Dir <> FDir) and (Pos(FDir, Dir) <> 1);
  if Result then
  begin
    FDir := Dir;
    FilesPage.Caption := Format('Files in %s', [D]);
    OpenDialog.InitialDir := D;
    {$IFDEF HasShellCtrls}
    FShellChangeNotifier.Root := D;
    {$ELSE}
    UpdateFileList;
    {$ENDIF HasShellCtrls}
  end;
end;

procedure TStretchDemoForm.AddToFileList(const Directory: string; const FileInfo: TSearchRec);
begin
  with FileListView.Items.Add do
  begin
    Caption := Directory + FileInfo.Name;
  end;
end;

procedure TStretchDemoForm.FileSearchTerminated(const ID: TFileSearchTaskID; const Aborted: Boolean);
begin
  with FileListView do
    Selected := FindCaption(0, FileName, False, True, False);
  StatusBar.Panels[0].Text := Format('%d files', [FileListView.Items.Count]);
  UpdateNavButtons;
end;

procedure TStretchDemoForm.UpdateFileList;
begin
  FileListView.Items.Clear;
  with FileSearch do
  begin
    FileMask := GraphicFileMask(TGraphic);
    RootDirectory := FDir;
    OnTerminateTask := FileSearchTerminated;
    ForEach(AddToFileList);
  end;
end;

procedure TStretchDemoForm.LoadFile(const AFileName: string);
begin
  if not IsGraphicFile(AFileName) then
    Exit;
  FileName := AFileName;
  OriginalImage.Picture.LoadFromFile(FileName);
  if not ChangeDirectory then
    UpdateNavButtons;

  UpdateCaption;
  with FileListView do
    Selected := FindCaption(0, FileName, False, True, False);

  StretchedImage.Picture.Graphic := nil;
  InvalidateStretched;
  if PageControl.ActivePage = FilesPage then
  begin
    {$IFDEF VCL}
    if OriginalImage.Picture.Graphic is TMetaFile then
      PageControl.ActivePage := OriginalPage
    else
    {$ENDIF VCL}
      PageControl.ActivePage := FLastImagePage;
    FocusControl(PageControl);
  end;
end;

procedure TStretchDemoForm.OpenFile(Sender: TObject);
begin
  if OpenDialog.Execute then
    LoadFile(OpenDialog.FileName);
end;

procedure TStretchDemoForm.SelectFilter(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    Checked := True;
    FResamplingFilter := TResamplingFilter(Tag);
    InvalidateStretched;
  end;
end;

procedure TStretchDemoForm.DoStretch;
var
  W, H: Integer;
  T: LongWord;
begin
  with OriginalImage.Picture do
    if (Graphic = nil) {$IFDEF VCL} or (Graphic is TMetafile) {$ENDIF} then
      Exit;
  W := StretchedPage.Width-2;
  H := StretchedPage.Height-2;
  if FPreserveAspectRatio then
    with OriginalImage.Picture.Graphic do
    begin
      if W * Height > H * Width then
        W := H * Width div Height
      else
        H := W * Height div Width;
    end;
  if (FWidth <> W) or (FHeight <> H) then
  begin
    T := GetTickCount;
    StretchedImage.Picture.Graphic := nil;
    JclGraphics.Stretch(W, H, FResamplingFilter, 0, OriginalImage.Picture.Graphic,
      StretchedImage.Picture.Bitmap);
    with OriginalImage.Picture do
      StatusBar.Panels[0].Text := Format('Original: %d x %d', [Width, Height]);
    with StretchedImage.Picture do
      StatusBar.Panels[1].Text := Format('Resized: %d x %d', [Width, Height]);
    FWidth := W;
    FHeight := H;
    FStretchTime := GetTickCount - T;
    with StretchedImage.Picture do
      StatusBar.Panels[2].Text := Format('Resize time: %d msec', [FStretchTime]);
  end;
end;

procedure TStretchDemoForm.PreserveAspectRatio1Click(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    FPreserveAspectRatio := Checked;
    InvalidateStretched;
  end;
end;

procedure TStretchDemoForm.LoadSelected;
begin
  with FileListView do
    if Selected <> nil then
      LoadFile(Selected.Caption);
end;

procedure TStretchDemoForm.PrevFile(Sender: TObject);
begin
  if FileListIndex > 0 then
    FileListIndex  := FileListIndex - 1;
  LoadSelected;
end;

procedure TStretchDemoForm.NextFile(Sender: TObject);
begin
  if FileListIndex < FileListView.Items.Count - 1 then
    FileListIndex  := FileListIndex + 1;
  LoadSelected;
end;

procedure TStretchDemoForm.UpdateCaption;
begin
  if FileName <> '' then
    Caption := Format('JCL Picture Viewer - %s', [FileName]);
end;

procedure TStretchDemoForm.UpdateNavButtons;
begin
  PrevItem.Enabled := FileListIndex > 0;
  NextItem.Enabled := FileListIndex < FileListView.Items.Count - 1;
  PrevItem.Enabled := FileListIndex > 0;
  NextItem.Enabled := FileListIndex < FileListView.Items.Count - 1;
end;

procedure TStretchDemoForm.FileListViewClick(Sender: TObject);
begin
  LoadSelected;
end;

procedure TStretchDemoForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{$IFDEF VCL}
const
  Key_Prior = VK_PRIOR;
  Key_Next = VK_NEXT;
{$ENDIF VCL}
begin
  case Key of
    Key_Prior:
      begin
        PrevFile(Self);
        Key := 0;
      end;
    Key_Next:
      begin
        NextFile(Self);
        Key := 0;
      end;
  end;
end;

procedure TStretchDemoForm.StretchedPageShow(Sender: TObject);
begin
  UpdateStretched;
end;

procedure TStretchDemoForm.UpdateStretched;
begin
  if StretchedPage.Visible then
    DoStretch;
end;

procedure TStretchDemoForm.StretchedPageResize(Sender: TObject);
begin
  UpdateStretched;
end;

procedure TStretchDemoForm.InvalidateStretched;
begin
  FWidth := 0;
  FHeight := 0;
  UpdateStretched;
end;

{$IFDEF VCL}
procedure TStretchDemoForm.WMDropFiles(var Msg: TWMDropFiles);
begin
  ProcessWMDropFiles(Msg, LoadFile);
end;
{$ENDIF VCL}

procedure TStretchDemoForm.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if PageControl.ActivePage <> FilesPage then
    FLastImagePage := PageControl.ActivePage;
end;

{$IFDEF HasShellCtrls}
procedure TStretchDemoForm.ShellChange;
begin
  UpdateFileList;
end;
{$ENDIF HasShellCtrls}

function TStretchDemoForm.GetFileListIndex: Integer;
begin
  Result := -1;
  if FileListView.Selected <> nil then
    Result := FileListView.Selected.Index;
end;

procedure TStretchDemoForm.SetFileListIndex(const Value: Integer);
begin
  if Value < 0 then
  begin
    if FileListView.Selected <> nil then
    begin
      FileListView.Selected.Selected := False;
    end;
  end
  else
    FileListView.Items[Value].Selected := True;
end;

procedure TStretchDemoForm.FileListViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    LoadSelected;
end;

procedure TStretchDemoForm.SetFileName(const Value: string);
begin
  FFileName := PathGetLongName(Value);
end;

end.
