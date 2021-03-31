/// main form of the TSynLog .log file vizualizer
unit LogViewMain;

interface

{$I Synopse.inc}

uses
  {$ifdef MSWINDOWS}
  Windows,
  ShellAPI,
  {$endif}
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ImgList,
  StdCtrls,
  CheckLst,
  Menus,
  ExtCtrls,
  Grids,
  Clipbrd,
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  {$ifdef FPC}  // FPC compatibility by alf (alfred) - thanks for the patch!
  ShellCtrls,
  SynTaskDialog in '..\..\Samples\ThirdPartyDemos\Ondrej\SynTaskDialog4Lazarus\SynTaskDialog.pas',
  {$else}
  SynTaskDialog, // also fix QC 37403 for Delphi 6/7/2006
  {$endif}
  SynCommons,
  SynLog,
  {$ifndef FPC}
  SynMemoEx,
  {$endif}
  mORMotHttpServer;

type
   {$ifdef FPC}
   TFileListBox = TShellListView;
   TDirectoryListBox = TShellTreeView;
   {$endif}

  { TMainLogView }

  TMainLogView = class(TForm)
    PanelLeft: TPanel;
    PanelThread: TPanel;
    PanelBottom: TPanel;
    BtnBrowse: TButton;
    EventsList: TCheckListBox;
    FilterMenu: TPopupMenu;
    EditSearch: TEdit;
    BtnSearchNext: TButton;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    BtnStats: TButton;
    OpenDialog: TOpenDialog;
    BtnMapSearch: TButton;
    MergedProfile: TCheckBox;
    ProfileGroup: TRadioGroup;
    ImageLogo: TImage;
    List: TDrawGrid;
    ProfileList: TDrawGrid;
    ThreadGroup: TGroupBox;
    BtnThreadNext: TButton;
    ThreadListBox: TCheckListBox;
    Splitter1: TSplitter;
    BtnThreadShow: TButton;
    PanelBrowse: TPanel;
    {$ifndef FPC}
    Drive: TDriveComboBox;
    {$endif}
    Directory: TDirectoryListBox;
    Files: TFileListBox;
    Splitter4: TSplitter;
    ListMenu: TPopupMenu;
    ListMenuCopy: TMenuItem;
    BtnSearchPrevious: TButton;
    btnServerLaunch: TButton;
    lblServerRoot: TLabel;
    edtServerRoot: TEdit;
    lblServerPort: TLabel;
    edtServerPort: TEdit;
    tmrRefresh: TTimer;
    btnListClear: TButton;
    btnListSave: TButton;
    dlgSaveList: TSaveDialog;
    pnlThreadBottom: TPanel;
    lblThreadName: TLabel;
    btnThread0: TButton;
    btnThread1: TButton;
    btnThreadAll: TButton;
    btnThreadDown: TButton;
    btnThreadUp: TButton;
    lstDays: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnFilterClick(Sender: TObject);
    procedure EventsListClickCheck(Sender: TObject);
    procedure BtnSearchNextClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListClick(Sender: TObject);
    procedure ProfileListClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure BtnStatsClick(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnMapSearchClick(Sender: TObject);
    procedure MergedProfileClick(Sender: TObject);
    procedure ProfileGroupClick(Sender: TObject);
    procedure ImageLogoClick(Sender: TObject);
    procedure EventsListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure ProfileListDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure EventsListDblClick(Sender: TObject);
    procedure BtnThreadNextClick(Sender: TObject);
    procedure BtnThreadShowClick(Sender: TObject);
    procedure ThreadListBoxDblClick(Sender: TObject);
    procedure BtnThreadClick(Sender: TObject);
    procedure ThreadListBoxClickCheck(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure FilesClick(Sender: TObject);
    procedure ListMenuCopyClick(Sender: TObject);
    procedure btnServerLaunchClick(Sender: TObject);
    procedure tmrRefreshTimer(Sender: TObject);
    procedure btnListClearClick(Sender: TObject);
    procedure btnListSaveClick(Sender: TObject);
    procedure ThreadListBoxClick(Sender: TObject);
    procedure lstDaysDblClick(Sender: TObject);
    procedure PanelLeftResize(Sender: TObject);
    procedure btnThreadDownClick(Sender: TObject);
    procedure btnThreadUpClick(Sender: TObject);
    procedure PanelBottomResize(Sender: TObject);
  protected
    FLog: TSynLogFileView;
    FMainCaption: string;
    FMenuFilterAll: TMenuItem;
    FLogUncompressed: TMemoryStream;
    FThreadNames: TRawUTF8DynArray;
    FDays: TDateTimeDynArray;
    FLastSearch: RawUTF8;
    FLastSearchSender: TObject;
    FRemoteLogService: TSQLHTTPRemoteLogServer; // from mORMotHTTPServer
    FPanelThreadVisible: boolean;
    procedure SetLogFileName(const Value: TFileName);
    procedure SetListItem(Index: integer; const search: RawUTF8='');
    procedure BtnFilterMenu(Sender: TObject);
    procedure ThreadListCheckRefresh;
    procedure ThreadListNameRefresh(Index: integer);
    procedure ReceivedOne(const Text: RawUTF8);
  public
    {$ifdef FPC}
    MemoBottom: TMemo; // for LCL compatibility
    {$else}
    MemoBottom: TMemoEx;
    {$endif}
    destructor Destroy; override;
    property LogFileName: TFileName write SetLogFileName;
  end;


var
  MainLogView: TMainLogView;


implementation

{$ifdef FPC}
{$R *.lfm}
{$else}
{$R *.dfm}
{$endif}

{$ifdef FPC}
uses
  LCLIntf,
  Themes,
  LCLType;
{$endif}

resourcestring
  sEnterAddress = 'Enter an hexadecimal address:';
  sStats = #13#10+
   '%s'#13#10'%s'#13#10#13#10+
     'Started: %s'#13#10'Closed:  %s'#13#10'Time elapsed: %d.%s'#13#10+
     'Events: %d'#13#10'Methods: %d'#13#10'Threads: %d'#13#10'Size: %s'#13#10#13#10+
   'Executable'#13#10'----------'#13#10#13#10'Name: %s%s'#13#10+
     'Version: %s'#13#10'Build Date: %s'#13#10'Framework: %s'#13#10#13#10+
   'Host'#13#10'----'#13#10#13#10'Computer: %s'#13#10+
     'User: %s'#13#10'CPU: %s%s'#13#10'OS: %s'#13#10#13#10+
   'Events'#13#10'------'#13#10#13#10;
  sNoFile = 'No File';
  sRemoteLog = 'Remote Log';
  sUnknown = 'Unknown';
  sWindowsStats = 'Windows %s (service pack %d)'#13#10'Wow64: %s';
  sTimeInfo = '%d lines - time elapsed: %s';
  
{ TMainLogView }

procedure TMainLogView.SetLogFileName(const Value: TFileName);
var E: TSynLogInfo;
    i, y: integer;
begin
  {$ifdef FPC}
  if (Value<>'') and (GetFileNameExtIndex(Value,'log,synlz,txt')<0) then
    exit;
  {$endif}
  FreeAndNil(FLog);
  FreeAndNil(FLogUncompressed);
  ThreadListBox.Clear;
  List.RowCount := 0;
  EventsList.Items.Clear;
  if FileExists(Value) then
  try
    Screen.Cursor := crHourGlass;
    Caption := FMainCaption+ExpandFileName(Value);
    if SameText(ExtractFileExt(Value),'.synlz') then begin
      FLogUncompressed := StreamUnSynLZ(Value,LOG_MAGIC);
      if FLogUncompressed=nil then
        exit; // invalid file content
      FLog := TSynLogFileView.Create(FLogUncompressed.Memory,FLogUncompressed.Size);
      FLog.FileName := Value;
    end else
      FLog := TSynLogFileView.Create(Value);
    EventsList.Items.BeginUpdate;
    if FLog.EventLevel=nil then begin // if not a TSynLog file -> open as plain text
      List.ColCount := 1;
      List.ColWidths[0] := 2000;
    end else begin
      List.ColCount := 4;
      List.ColWidths[0] := 70;
      List.ColWidths[1] := 60;
      List.ColWidths[2] := 24;
      List.ColWidths[3] := 2000;
      for E := succ(sllNone) to high(E) do
        if E in FLog.EventLevelUsed then
          EventsList.Items.AddObject(ToCaption(E),pointer(ord(E)));
      for i := 1 to FilterMenu.Items.Count-1 do
        FilterMenu.Items[i].Visible :=
          LOG_FILTER[TSynLogFilter(FilterMenu.Items[i].Tag)]*FLog.EventLevelUsed<>[];
    end;
  finally
    EventsList.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end else
    Caption := FMainCaption+sNoFile;
  EventsList.Height := 8+EventsList.Count*(EventsList.ItemHeight{$ifndef MSWINDOWS}+4{$endif});
  ProfileGroup.Top := EventsList.Top+EventsList.Height+12;
  MergedProfile.Top := ProfileGroup.Top+ProfileGroup.Height+2;
  y := MergedProfile.Top+32;
  BtnStats.Top := y;
  BtnMapSearch.Top := y;
  inc(y,32);
  ThreadGroup.Visible := (FLog<>nil) and (FLog.EventThread<>nil);
  if ThreadGroup.Visible then begin
    FThreadNames := FLog.ThreadNames(-1);
    ThreadGroup.Top := y;
    inc(y,ThreadGroup.Height+8);
    ThreadListBox.Items.BeginUpdate;
    ThreadListBox.Items.Clear;
    for i := 0 to FLog.ThreadsCount-1 do begin
      ThreadListBox.Items.Add(UTF8ToString(FThreadNames[i]));
      ThreadListBox.Checked[i] := true;
    end;
    ThreadListBox.Items.EndUpdate;
  end;
  lstDays.Visible := (FLog<>nil) and (FLog.DayChangeIndex<>nil);
  if lstDays.Visible then begin
    FLog.GetDays(FDays);
    lstDays.Top := y;
    lstDays.Items.BeginUpdate;
    lstDays.Items.Clear;
    for i := 0 to high(FDays) do
      lstDays.Items.Add(Format('%s (%d rows)',[DateToStr(FDays[i]),
        FLog.DayCount[i]]));
    lstDays.Items.EndUpdate;
    lstDays.ItemIndex := 0;
    PanelLeftResize(nil);
  end;
  ProfileGroup.ItemIndex := 0;
  MergedProfile.Checked := false;
  BtnFilterMenu(FMenuFilterAll);
  EventsList.Visible := FLog<>nil;
  ProfileGroup.Visible := (FLog<>nil) and (FLog.LogProcCount<>0);
  MergedProfile.Visible := ProfileGroup.Visible;
  BtnStats.Visible:= (FLog<>nil) and (FLog.EventLevel<>nil);
  BtnMapSearch.Visible := FLog<>nil;
  EditSearch.Visible := FLog<>nil;
  if FLog<>nil then
    EditSearch.SetFocus;
  BtnSearchNext.Visible  := FLog<>nil;
  BtnSearchPrevious.Visible := FLog<>nil;
  lblServerRoot.Visible := FLog=nil;
  lblServerPort.Visible := FLog=nil;
  edtServerRoot.Visible := FLog=nil;
  edtServerPort.Visible := FLog=nil;
  btnServerLaunch.Visible := FLog=nil;
  btnListClear.Hide;
  btnListSave.Hide;
  List.Visible := FLog<>nil;
  EventsListClickCheck(nil);
end;

destructor TMainLogView.Destroy;
begin
  FRemoteLogService.Free;
  FLog.Free;
  FLogUncompressed.Free;
  inherited;
end;

procedure TMainLogView.FormCreate(Sender: TObject);
var F: TSynLogFilter;
    O: TLogProcSortOrder;
    M: TMenuItem;
begin
  FMainCaption := format(Caption,[SYNOPSE_FRAMEWORK_VERSION])+' ';
  for F := low(F) to high(F) do begin
    M := TMenuItem.Create(self);
    M.Caption := ToCaption(F);
    M.Tag := ord(F);
    M.OnClick := BtnFilterMenu;
    if F=lfAll then
      FMenuFilterAll := M;
    FilterMenu.Items.Add(M);
  end;
  for O := low(O) to high(O) do
    ProfileGroup.Items.AddObject(
      GetCaptionFromEnum(TypeInfo(TLogProcSortOrder),Ord(O)),TObject(O));
  ProfileList.ColWidths[0] := 60;
  ProfileList.ColWidths[1] := 1000;
  ProfileList.Hide;
  {$ifdef FPC}
  MemoBottom := TMemo.Create(self);
  {$else}
  MemoBottom := TMemoEx.Create(self);
  {$endif}
  MemoBottom.Parent := PanelBottom;
  MemoBottom.Align := alClient;
  MemoBottom.Font.Height := -11;
  if Screen.Fonts.IndexOf('Consolas') >= 0 then
    MemoBottom.Font.Name := 'Consolas' else
    MemoBottom.Font.Name := 'Courier New';
  MemoBottom.ReadOnly := true;
  MemoBottom.ScrollBars := ssVertical;
  MemoBottom.Text := '';
end;

procedure TMainLogView.FormShow(Sender: TObject);
var CmdLine: TFileName;
begin
  PanelThread.Width := 300;
  if ParamCount>0 then begin
    CmdLine := ParamStr(1);
    if SysUtils.DirectoryExists(CmdLine) then begin
      BtnBrowseClick(nil);
      {$ifdef FPC}
      Directory.Path := CmdLine;
      {$else}
      Directory.Directory := CmdLine;
      {$endif}
    end else
      LogFileName := CmdLine;
    end else begin
      {$ifdef FPC}
      Directory.Path := ExtractFileDir(ParamStr(0));
      {$endif}
      LogFileName := '';
    end;
  WindowState := wsMaximized;    
end;

procedure TMainLogView.BtnFilterClick(Sender: TObject);
var SenderBtn: TButton absolute Sender;
begin
  if Sender.InheritsFrom(TButton) then
    with ClientToScreen(SenderBtn.BoundsRect.TopLeft) do
      SenderBtn.PopupMenu.Popup(X,Y+SenderBtn.Height);
end;

procedure TMainLogView.BtnFilterMenu(Sender: TObject);
var F: TSynLogFilter;
    i: integer;
begin
  if not Sender.InheritsFrom(TMenuItem) then exit;
  F := TSynLogFilter(TMenuItem(Sender).Tag);
  for i := 0 to EventsList.Count-1 do
    EventsList.Checked[i] := TSynLogInfo(EventsList.Items.Objects[i]) in LOG_FILTER[F];
  EventsListClickCheck(nil);
end;

{$ifdef FPC}
  {$ifdef MSWINDOWS}
    {$define FPCCHECKBOXUNFIXED} // circumvent LCL issue under Windows
  {$endif}
{$endif}

procedure TMainLogView.EventsListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var E: TSynLogInfo;
   {$ifdef FPCCHECKBOXUNFIXED}
   BRect: TRect;
   aTheme: TThemedElementDetails;
   {$endif}
begin
  if Index<0 then
    exit;
  E := TSynLogInfo(EventsList.Items.Objects[Index]);
  inherited;
  with EventsList do begin
    Canvas.Brush.Color := LOG_LEVEL_COLORS[false,E];
    Canvas.Font.Color  := LOG_LEVEL_COLORS[true,E];
    {$ifdef FPC}
    Canvas.FillRect(Rect);
    {$ifdef FPCCHECKBOXUNFIXED}
    BRect.Left := Rect.Left + 1;
    BRect.Top := Rect.Top;
    BRect.Bottom := Rect.Bottom;
    BRect.Right := Rect.Left + (Rect.Bottom - Rect.Top) - 2;
    if Checked[Index] then
       aTheme := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal) else
       aTheme := ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);
    ThemeServices.DrawElement(Canvas.Handle, aTheme, BRect);
    Rect.Left := BRect.Right;
    {$endif}
    {$endif}
    Canvas.TextRect(Rect,Rect.Left+4,Rect.Top{$ifndef MSWINDOWS}+4{$endif},ToCaption(E));
  end;
end;

procedure TMainLogView.EventsListClickCheck(Sender: TObject);
var i: integer;
    Sets: TSynLogInfos;
begin
  if FLog=nil then
    List.RowCount := 0 else
  if FLog.EventLevel<>nil then begin
    integer(Sets) := 0;
    for i := 0 to EventsList.Count-1 do
      if EventsList.Checked[i] then
        Include(Sets,TSynLogInfo(EventsList.Items.Objects[i]));
    FLog.Events := Sets;
    i := FLog.Select(List.Row);
    if cardinal(i) < cardinal(FLog.SelectedCount) then
      List.Row := 0; // avoid "Grid Out Of Range" when setting RowCount
    List.RowCount := FLog.SelectedCount;
    if i>=0 then
      List.Row := i;
  end else
    List.RowCount := FLog.Count;
  SetListItem(List.Row);
  if List.Visible then begin
    List.Repaint;
    ListClick(nil);
  end;
end;

procedure TMainLogView.EventsListDblClick(Sender: TObject);
var i: integer;
begin
  if FLog.EventLevel=nil then // plain text file does not handle this
    exit;
  i := EventsList.ItemIndex;
  if i>=0 then
    i := fLog.SearchNextEvent(TSynLogInfo(EventsList.Items.Objects[i]),List.Row);
  if i>=0 then
    SetListItem(i);
end;

const
  TIME_FORMAT = 'hh:mm:ss.zzz';
  MAXLOGLINES = 300;

procedure TMainLogView.ListDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var txt: string;
    inverted: boolean;
    level: TSynLogInfo;
begin
  with List.Canvas do begin
    if FLog=nil then begin
      FillRect(Rect);
      exit;
    end;
    txt := FLog.GetCell(ACol,ARow,level);
    if FLog.EventLevel<>nil then begin
      Brush.Style := bsClear;
      if cardinal(ARow)<cardinal(FLog.SelectedCount) then begin
        inverted := (gdFocused in State) or (gdSelected in State);
        if inverted then
          Brush.Color := clBlack else
          Brush.Color := LOG_LEVEL_COLORS[inverted,level];
        Font.Color  := LOG_LEVEL_COLORS[not inverted,level];
      end else begin
        Brush.Color := clLtGray;
        FillRect(Rect);
        exit;
      end;
    end;
    {$ifdef FPC}
    FillRect(Rect);
    {$endif}
    TextRect(Rect,Rect.Left+4,Rect.Top,txt);
  end;
end;

procedure TMainLogView.BtnSearchNextClick(Sender: TObject);
var ndx: integer;
    s: RawUTF8;
begin
  s := UpperCase(StringToUTF8(EditSearch.Text));
  Screen.Cursor := crHourGlass;
  try
    if Sender=BtnSearchPrevious then
      ndx := FLog.SearchPreviousText(s,List.Row) else
    if Sender=EditSearch then
      ndx := FLog.SearchNextText(s,List.Row,0) else
      ndx := FLog.SearchNextText(s,List.Row,1); // e.g. BtnSearchNext
    if ndx>=0 then
      SetListItem(ndx,s);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainLogView.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_F3 then begin
    if Shift=[] then
      BtnSearchNextClick(nil) else
    if ssShift in Shift then
      BtnSearchNextClick(BtnSearchPrevious) else
      exit;
    List.SetFocus;
  end else
  if (Shift=[ssCtrl]) and (Key=ord('F')) then
    EditSearch.SetFocus;
end;

procedure TMainLogView.ProfileListClick(Sender: TObject);
var ndx,i: integer;
begin
  i := ProfileList.Row;
  if (FLog<>Nil) and (cardinal(i)<=cardinal(FLog.LogProcCount)) then begin
    ndx := FLog.LogProc[i].Index;
    i := IntegerScanIndex(pointer(FLog.Selected),FLog.SelectedCount,ndx);
    if i>=0 then begin
      SetListItem(i);
      List.SetFocus;
    end;
  end;
end;

procedure TMainLogView.ProfileListDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
type TProfileListCol = (colTime, colName);
var Tim: integer;
    s: string;
begin
  if (FLog<>Nil) and (cardinal(ARow)<cardinal(FLog.LogProcCount)) then
  with FLog.LogProc[ARow] do begin
    case TProfileListCol(ACol) of
    colTime: begin
      if FLog.LogProcOrder=soByProperTime then
        Tim := ProperTime else
        Tim := Time;
      s := Ansi7ToString(MicroSecToString(Tim));
    end;
    colName:
      s := FLog.EventString(Index);
    end;
    ProfileList.Canvas.TextRect(Rect,Rect.Left+4,Rect.Top,s);
  end;
end;

procedure TMainLogView.ListClick(Sender: TObject);
var i,ndx,found: integer;
    Selection: TGridRect;
    elapsed: TDateTime;
    s,tim: string;
begin
  i := List.Row;
  if cardinal(i)<cardinal(FLog.SelectedCount) then begin
    i := FLog.Selected[i];
    s := FLog.EventString(i,'',0,true);
    if FPanelThreadVisible and (FLog.EventThread<>nil) then begin
      ThreadListNameRefresh(i);
      ndx := FLog.EventThread[i]-1;
      if ndx<>ThreadListBox.ItemIndex then begin
        btnThread1.Caption := IntToStr(ndx+1);
        ThreadListBox.ItemIndex := ndx;
        ThreadListBoxClick(nil);
      end;
    end;
    if lstDays.Visible then begin
      ndx := lstDays.ItemIndex;
      if (cardinal(ndx)<cardinal(Length(FDays))) and
         (Trunc(FLog.EventDateTime(i))<>FDays[ndx]) then begin
        found := high(FLog.DayChangeIndex);
        for ndx := 1 to found do
          if FLog.DayChangeIndex[ndx]>i then begin
            found := ndx-1;
            break;
          end;
        lstDays.ItemIndex := found;
      end;
    end;
  end else
    if FLog<>nil then
      s := FLog.EventString(i,'',0,true) else
      s := '';
  Selection := List.Selection;
  if Selection.Bottom>Selection.Top then begin
    elapsed := FLog.EventDateTime(Selection.Bottom)-FLog.EventDateTime(Selection.Top);
    if FLog.Freq=0 then begin
      DateTimeToString(tim,TIME_FORMAT,elapsed);
      s := tim+#13#10+s;
    end else begin
      tim := IntToStr(trunc(elapsed*MSecsPerDay*1000) mod 1000);
      s := StringOfChar('0',3-length(tim))+tim+#13#10+s;
      DateTimeToString(tim,TIME_FORMAT,elapsed);
      s := tim+'.'+s;
    end;
    s := format(sTimeInfo,[Selection.Bottom-Selection.Top+1,s]);
  end;
  if Pos(#9, s) > 0 then
    s := StringReplace(s, #9, ' ', [rfReplaceAll]);
  MemoBottom.Text := s;
end;

procedure TMainLogView.ListDblClick(Sender: TObject);
var ndx: integer;
begin
  ndx := fLog.SearchEnterLeave(List.Row);
  if ndx>=0 then
    SetListItem(ndx);
end;

procedure TMainLogView.BtnThreadNextClick(Sender: TObject);
begin
  SetListItem(FLog.SearchNextThread(List.Row));
end;

procedure TMainLogView.btnThreadDownClick(Sender: TObject);
begin
  SetListItem(FLog.SearchNextSameThread(List.Row));
end;

procedure TMainLogView.btnThreadUpClick(Sender: TObject);
begin
  SetListItem(FLog.SearchPreviousSameThread(List.Row));
end;

procedure TMainLogView.ThreadListBoxDblClick(Sender: TObject);
var ID: cardinal;
begin
  ID := ThreadListBox.ItemIndex;
  if ID<FLog.ThreadsCount then
    SetListItem(FLog.SearchThread(ID+1,List.Row));
end;

procedure TMainLogView.BtnThreadShowClick(Sender: TObject);
begin
  FPanelThreadVisible := not FPanelThreadVisible;
  PanelThread.Visible := FPanelThreadVisible;
  Splitter3.Visible := FPanelThreadVisible;
  if FPanelThreadVisible then begin
    PanelThread.Left := ProfileList.Left+ProfileList.Width;
    Splitter3.Left := PanelThread.Left+PanelThread.Width;
    ListClick(nil);
  end else
    btnThread1.Caption := '1';
  btnThread0.Enabled := FPanelThreadVisible;
  btnThread1.Enabled := FPanelThreadVisible;
  btnThreadAll.Enabled := FPanelThreadVisible;
end;

procedure TMainLogView.BtnStatsClick(Sender: TObject);
var M: TMemo;
    F: TForm;
    s,ostext: string;
    sets: array[TSynLogInfo] of integer;
    i: integer;
    P: PUTF8Char;
    feat,line,name,value: RawUTF8;
    closed,elapsed: TDateTime;
begin
  F := TForm.Create(Application);
  try
    F.Caption := FMainCaption+BtnStats.Caption;
    if Screen.Fonts.IndexOf('Consolas')>=0 then
      F.Font.Name := 'Consolas' else
      F.Font.Name := 'Courier New';
    F.Position := poScreenCenter;
    F.Width := 700;
    F.Height := 600;
    M := TMemo.Create(F);
    M.Parent := F;
    M.Align := alClient;
    M.ScrollBars := ssVertical;
    M.WordWrap := true;
    M.ReadOnly := true;
    if (FLog<>nil) and (FLog.EventLevel<>nil) then
      with FLog do begin
        if InstanceName<>'' then
          s := ' / '+UTF8ToString(InstanceName);
        if OS<>wUnknown then
          ostext := format(sWindowsStats,[WINDOWS_NAME[OS],ServicePack,BOOL_STR[Wow64]]) else
          ostext := UTF8ToString(DetailedOS);
        feat := ToText(IntelCPU,' ');
        if feat<>'' then
          feat := '  ' + LowerCase(feat);
        closed := EventDateTime(Count-1);
        elapsed := closed-StartDateTime;
        s := format(sStats, [FileName,StringOfChar('-',length(FileName)),
           DateTimeToStr(StartDateTime),DateTimeToStr(closed),
           trunc(elapsed),FormatDateTime('hh:mm:ss',elapsed),
           Count,LogProcCount,ThreadsCount,KB(Map.Size),
           UTF8ToString(ExecutableName),s,ExecutableVersion,
           DateTimeToStr(ExecutableDate),Framework,UTF8ToString(ComputerHost),
           UTF8ToString(RunningUser),CPU,feat,ostext]);
        fillchar(sets,sizeof(sets),0);
        for i := 0 to Count-1 do
          inc(sets[EventLevel[i]]);
        for i := 0 to EventsList.Count-1 do
          s := s+EventsList.Items[i]+': '+
            IntToStr(sets[TSynLogInfo(EventsList.Items.Objects[i])])+#13#10;
        P := pointer(Headers);
        while (P<>nil) and (P^<>#0) do begin
          line := GetNextLine(P,P);
          Split(line,'=',name,value);
          if value<>'' then
            s := s+#13#10+UTF8ToString(name)+#13#10+StringOfChar('-',length(name))+
              #13#10#13#10+UTF8ToString(StringReplaceAll(value,#9,#13#10))+#13#10;
        end;
      end;
    M.Text := s;
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TMainLogView.BtnOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    LogFileName := OpenDialog.FileName;
end;

procedure TMainLogView.SetListItem(Index: integer; const search: RawUTF8='');
var i: integer;
    s,ss: string;
begin
  if (Index<0) or (FLog=nil) then
    MemoBottom.Text := '' else begin
    List.Row := Index;
    if (search='') and List.Visible then
      List.SetFocus;
    if FLog.EventLevel<>nil then
      Index := FLog.Selected[Index];
    s := FLog.EventString(Index,'',0,true);
    if Pos(#9, s) > 0 then
      s := StringReplace(s, #9, ' ', [rfReplaceAll]);
    MemoBottom.Text := s;
    if search<>'' then begin
      ss := UTF8ToString(search);
      i := Pos(ss,SysUtils.UpperCase(s));
      if i>0 then begin
        MemoBottom.SelStart := i;
        MemoBottom.SelLength := length(ss);
      end;
    end;
  end;
end;

procedure TMainLogView.BtnMapSearchClick(Sender: TObject);
var FN: TFileName;
    Addr: string;
    AddrInt, err: integer;
    Loc: RawUTF8;
    Map: TSynMapFile;
begin
  if (FLog<>nil) and (FLog.ExecutableName<>'') then begin
    FN := ChangeFileExt(ExtractFileName(UTF8ToString(FLog.ExecutableName)),'.map');
    FN := FN+';'+ChangeFileExt(FN,'.mab');
  end;
  with TOpenDialog.Create(Application) do
  try
    DefaultExt := '.map';
    Filter := '*.map;*.mab|*.map;*.mab';
    if FN<>'' then
      Filter := FN+'|'+FN+'|'+Filter;
    Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
    if not Execute then
      exit;
    Map := TSynMapFile.Create(FileName);
    try
      repeat
        if not InputQuery(BtnMapSearch.Hint,sEnterAddress,Addr) then
          Exit;
        Addr := SysUtils.Trim(Addr);
        if Addr='' then continue;
        if Addr[1]<>'$' then
          Addr := '$'+Addr;
        val(Addr,AddrInt,err);
        if err<>0 then
          continue;
        Loc := Map.FindLocation(AddrInt);
        if Loc<>'' then
          ShowMessage(Addr+#13#10+UTF8ToString(Loc));
      until false;
    finally
      Map.Free;
    end;
  finally
    Free;
  end;
end;

procedure TMainLogView.MergedProfileClick(Sender: TObject);
begin
  if FLog=nil then
    Exit;
  Screen.Cursor := crHourGlass;
  try
    FLog.LogProcMerged := MergedProfile.Checked;
    ProfileGroupClick(nil);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainLogView.ProfileGroupClick(Sender: TObject);
var O: TLogProcSortOrder;
begin
  O := TLogProcSortOrder(ProfileGroup.ItemIndex);
  if O<low(O) then
    O := low(O);
  if (FLog=nil) or (O=soNone) then begin
    Splitter1.Hide;
    ProfileList.Hide;
  end else begin
    Screen.Cursor := crHourGlass;
    FLog.LogProcSort(O);
    Screen.Cursor := crDefault;
    ProfileList.RowCount := FLog.LogProcCount;
    ProfileList.Show;
    Splitter1.Left := ProfileList.Left+ProfileList.Width;
    Splitter1.Show;
    ProfileList.Repaint;
  end;
end;

procedure TMainLogView.ImageLogoClick(Sender: TObject);
begin
{$ifdef FPC}
  OpenURL('https://synopse.info');
{$else}
  {$WARNINGS OFF}
  if DebugHook=0 then
  {$WARNINGS ON}
    ShellExecute(0,'open','https://synopse.info',nil,nil,SW_SHOWNORMAL);
{$endif}
end;

procedure TMainLogView.BtnThreadClick(Sender: TObject);
begin
  FLog.SetAllThreads(Sender=btnThreadAll);
  if Sender=BtnThread1 then
    FLog.Threads[ThreadListBox.ItemIndex+1] := true;
  ThreadListCheckRefresh;
  EventsListClickCheck(nil);
end;

procedure TMainLogView.ThreadListCheckRefresh;
var i: integer;
begin
  for i := 0 to ThreadListBox.Count-1 do
    ThreadListBox.Checked[i] := FLog.Threads[i+1];
end;

procedure TMainLogView.ThreadListBoxClickCheck(Sender: TObject);
var i: integer;
begin
  i := ThreadListBox.ItemIndex;
  if i>=0 then begin
    FLog.Threads[i+1] := ThreadListBox.Checked[i];
    EventsListClickCheck(nil);
  end;
end;

procedure TMainLogView.ThreadListNameRefresh(Index: integer);
var names: TRawUTF8DynArray;
    i: integer;
begin
  names := FLog.ThreadNames(Index);
  if names=nil then
    exit;
  for i := 0 to FLog.ThreadsCount-1 do
    if names[i]<>FThreadNames[i] then
      ThreadListBox.Items[i] := UTF8ToString(names[i]);
  FThreadNames := names;
end;

procedure TMainLogView.ThreadListBoxClick(Sender: TObject);
var i: integer;
begin
  i := ThreadListBox.ItemIndex;
  if i>=0 then begin
    lblThreadName.Caption := UTF8ToString(FThreadNames[i]);
    btnThread1.Caption := IntToStr(i+1);
  end;
end;

procedure TMainLogView.BtnBrowseClick(Sender: TObject);
begin
  PanelBrowse.Visible := (Sender=nil) or not PanelBrowse.Visible;
  Splitter4.Visible := PanelBrowse.Visible;
  if PanelBrowse.Visible then
    Splitter4.Left := PanelBrowse.Left+PanelBrowse.Width;
end;

procedure TMainLogView.FilesClick(Sender: TObject);
begin
  {$ifdef FPC}
  if (FLog=nil) or (FLog.FileName<>Files.Root) then
     if Files.Selected<>nil then
       LogFileName := Files.GetPathFromItem(Files.Selected);
  {$else}
  if (FLog=nil) or (FLog.FileName<>Files.FileName) then
    LogFileName := Files.FileName;
  {$endif}
end;

procedure TMainLogView.ListMenuCopyClick(Sender: TObject);
var Selection: TGridRect;
    i: integer;
    s: string;
begin
  Selection := List.Selection;
  for i := Selection.Top to Selection.Bottom do
    s := s+FLog.GetLineForClipboard(i)+sLineBreak;
  Clipboard.AsText := s;
end;

procedure TMainLogView.btnServerLaunchClick(Sender: TObject);
var E: TSynLogInfo;
    i, BestFitHeight: integer;
begin
  if FRemoteLogService=nil then
  try
    FRemoteLogService := TSQLHTTPRemoteLogServer.Create(
      StringToUTF8(edtServerRoot.Text),StrToInt(edtServerPort.Text),ReceivedOne);
    Caption := FMainCaption+sRemoteLog;
  except
    on E: Exception do begin
      ShowMessage(E.Message);
      exit;
    end;
  end;
  if FLog=nil then
    FLog := TSynLogFileView.Create;
  List.DoubleBuffered := true;
  List.ColCount := 4;
  List.ColWidths[0] := 70;
  List.ColWidths[1] := 60;
  List.ColWidths[2] := 24;
  List.ColWidths[3] := 2000;
  // Filtered remote log view support
  FLog.Events :=LOG_VERBOSE;
  EventsList.Items.Clear;
  EventsList.Items.BeginUpdate;
  try
    for E := succ(sllNone) to high(E) do begin
      EventsList.Items.AddObject(ToCaption(E),pointer(ord(E)));
      EventsList.Checked[EventsList.Count-1] := true;
    end;
    for i := 1 to FilterMenu.Items.Count-1 do
      FilterMenu.Items[i].Visible := true;
  finally
    EventsList.Items.EndUpdate;
  end;
  EventsList.Height := 8+EventsList.Count*EventsList.ItemHeight;
  EventsList.Show;
  btnListClear.Top := EventsList.Top+EventsList.Height+12;
  btnListClear.Width := EventsList.width div 2 - 2;
  btnListSave.Top := btnListClear.Top;
  btnListSave.Width := btnListClear.Width;
  btnListSave.Left := btnListClear.Left + btnListClear.Width + 4;
  if (btnListSave.Top + btnListSave.Height+12) > ImageLogo.top then begin
    BestFitHeight := height + btnListSave.Top + btnListSave.Height+12 - ImageLogo.top;
    if BestFitHeight > Screen.Height then
      height := Screen.Height else
      height := BestFitHeight;
  end;
  lblServerRoot.Hide;
  lblServerPort.Hide;
  edtServerRoot.Hide;
  edtServerPort.Hide;
  btnServerLaunch.Hide;
  btnListClear.Show;
  btnListSave.Show;
  EditSearch.Show;
  EditSearch.SetFocus;
  BtnSearchNext.Show;
  BtnSearchPrevious.Show;
  ReceivedOne(FormatUTF8(
    '%00 info  Remote Logging Server started on port % with root name "%"',
    [NowToString(false),FRemoteLogService.Port,FRemoteLogService.Server.Model.Root]));
  List.Show;
  tmrRefresh.Enabled := true;
end;

const
  TAG_NONE = 0;
  TAG_REFRESH = 1;

procedure TMainLogView.ReceivedOne(const Text: RawUTF8);
var P: PUTF8Char;
    line: RawUTF8;
begin
  // warning: this method is called from WebSockets thread, not UI thread
  P := pointer(Text);
  repeat // handle multiple log rows in the incoming text
    line := GetNextLine(P,P);
    if length(line)<24 then
      continue;
    FLog.AddInMemoryLine(line);
    tmrRefresh.Tag := TAG_REFRESH; // notify tmrRefreshTimer()
  until P=nil;
end;

procedure TMainLogView.tmrRefreshTimer(Sender: TObject);
begin
  if (tmrRefresh.Tag = TAG_NONE) or (fLog = nil) then
    exit; // ReceivedOne() did not happen
  List.RowCount := FLog.SelectedCount;
  if FLog.SelectedCount > 0 then
    List.TopRow := FLog.SelectedCount-List.VisibleRowCount;
  List.Invalidate;
  tmrRefresh.Tag := TAG_NONE;
end;

procedure TMainLogView.btnListClearClick(Sender: TObject);
begin
  FreeAndNil(FLog);
  btnServerLaunchClick(nil);
end;

procedure TMainLogView.btnListSaveClick(Sender: TObject);
begin
  dlgSaveList.FileName := 'Remote '+Utf8ToString(DateTimeToIso8601(Now,false,' '));
  if not dlgSaveList.Execute then
    exit;
  fLog.SaveToFile('temp~.log',
    StringToUTF8(ExeVersion.ProgramFileName)+' 0.0.0.0 ('+NowToString+')'#13+
    'Host=Remote User=Unknown CPU=Unknown OS=0.0=0.0.0 Wow64=0 Freq=1'#13+
    'LogView '+SYNOPSE_FRAMEWORK_VERSION+' Remote '+NowToString+#13#13);
  if dlgSaveList.FilterIndex=3 then
    FileSynLZ('temp~.log',dlgSaveList.FileName,LOG_MAGIC) else
    RenameFile('temp~.log',dlgSaveList.FileName);
end;

procedure TMainLogView.lstDaysDblClick(Sender: TObject);
var ndx: integer;
begin
  ndx := lstDays.ItemIndex;
  if cardinal(ndx)<cardinal(Length(FLog.DayChangeIndex)) then
    SetListItem(FLog.SearchNextSelected(FLog.DayChangeIndex[ndx]));
end;

procedure TMainLogView.PanelLeftResize(Sender: TObject);
begin
  lstDays.Height := PanelLeft.ClientHeight-lstDays.Top-48;
end;


procedure TMainLogView.PanelBottomResize(Sender: TObject);
var w: integer;
begin
  {$ifndef FPC}
  w := MemoBottom.CellRect.Width;
  if w > 0 then
    MemoBottom.RightMargin := (PanelBottom.ClientWidth div w) - 7;
  {$endif}
end;

end.
