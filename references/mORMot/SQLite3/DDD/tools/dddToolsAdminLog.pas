unit dddToolsAdminLog;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  CheckLst,
  Menus,
  Grids,
  SynCommons,
  SynLog,
  mORMot,
  mORMotDDD;

type
  TLogFrame = class(TFrame)
    pnlLeft: TPanel;
    pnlRight: TPanel;
    spl1: TSplitter;
    edtSearch: TEdit;
    chklstEvents: TCheckListBox;
    pmFilter: TPopupMenu;
    mmoBottom: TMemo;
    drwgrdEvents: TDrawGrid;
    btnStartLog: TButton;
    tmrRefresh: TTimer;
    edtExistingLogKB: TEdit;
    lblExistingLogKB: TLabel;
    btnStopLog: TButton;
    spl2: TSplitter;
    BtnSearchNext: TButton;
    BtnSearchPrevious: TButton;
    procedure chklstEventsDrawItem(Control: TWinControl; Index: Integer; Rect:
      TRect; State: TOwnerDrawState);
    procedure btnStartLogClick(Sender: TObject);
    procedure tmrRefreshTimer(Sender: TObject);
    procedure drwgrdEventsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect:
      TRect; State: TGridDrawState);
    procedure drwgrdEventsClick(Sender: TObject); virtual;
    procedure btnSearchNextClick(Sender: TObject);
    procedure chklstEventsDblClick(Sender: TObject);
    procedure btnStopLogClick(Sender: TObject);
    procedure chklstEventsClickCheck(Sender: TObject);
    procedure drwgrdEventsDblClick(Sender: TObject);
  protected
    FLog: TSynLogFileView;
    FMenuFilterAll, FMenuFilterNone: TMenuItem;
    FCallbackPattern: RawUTF8;
    FLogSafe: TSynLocker;
    procedure EventsCheckToLogEvents;
    procedure pmFilterClick(Sender: Tobject);
    procedure ReceivedOne(const Text: RawUTF8);
    procedure SetListItem(Index: integer; const search: RawUTF8 = '');
  public
    Admin: IAdministratedDaemon;
    Callback: ISynLogCallback;
    OnLogReceived: function(Sender: TLogFrame; Level: TSynLogInfo;
      const Text: RawUTF8): boolean of object;
    constructor Create(Owner: TComponent; const aAdmin: IAdministratedDaemon); reintroduce;
    constructor CreateCustom(Owner: TComponent; const aAdmin: IAdministratedDaemon;
      const aEvents, aPattern: RawUTF8); virtual;
    destructor Destroy; override;
    procedure LogFilter(F: TSynLogInfos);
    procedure Closing;
  end;

  TLogFrameClass = class of TLogFrame;

  TLogFrameDynArray = array of TLogFrame;

  TLogFrameChat = class(TLogFrame)
  protected
    procedure mmoChatKeyPress(Sender: TObject; var Key: Char);
  public
    mmoChat: TMemo;
    constructor CreateCustom(Owner: TComponent; const aAdmin:
      IAdministratedDaemon; const aEvents, aPattern: RawUTF8); override;
  end;

implementation

uses
  dddToolsAdminMain;

{$R *.dfm}

{ TLogFrameCallback }

type
  TLogFrameCallback = class(TInterfacedObject, ISynLogCallback)
  public
    Owner: TLogFrame;
    Pattern: RawUTF8;
    procedure Log(Level: TSynLogInfo; const Text: RawUTF8);
  end;

procedure TLogFrameCallback.Log(Level: TSynLogInfo; const Text: RawUTF8);
begin
  if (Pattern <> '') and (Level <> sllNone) then
    if PosI(pointer(Pattern), Text) = 0 then
      exit;
  Owner.ReceivedOne(Text);
  if Assigned(Owner.OnLogReceived) then
    Owner.OnLogReceived(Owner, Level, Text);
end;

procedure TLogFrame.chklstEventsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  E: TSynLogInfo;
begin
  if Index < 0 then
    exit;
  E := TSynLogInfo(chklstEvents.Items.Objects[Index]);
  with chklstEvents.Canvas do begin
    Brush.Color := LOG_LEVEL_COLORS[false, E];
    Font.Color := LOG_LEVEL_COLORS[true, E];
    TextRect(Rect, Rect.Left + 4, Rect.Top, ToCaption(E));
  end;
end;

var
  LogFrameCount: integer;

constructor TLogFrame.Create(Owner: TComponent; const aAdmin: IAdministratedDaemon);
var
  F: TSynLogFilter;
  M: TMenuItem;
begin
  inherited Create(Owner);
  FLogSafe.Init;
  Admin := aAdmin;
  Name := 'LogFrame' + IntToStr(LogFrameCount);
  inc(LogFrameCount);
  for F := low(F) to high(F) do begin
    M := TMenuItem.Create(self);
    M.Caption := ToCaption(F);
    M.Tag := ord(F);
    M.OnClick := pmFilterClick;
    if F = lfAll then
      FMenuFilterAll := M
    else if F = lfNone then
      FMenuFilterNone := M;
    pmFilter.Items.Add(M);
  end;
  btnStopLogClick(nil);
end;

constructor TLogFrame.CreateCustom(Owner: TComponent;
  const aAdmin: IAdministratedDaemon; const aEvents, aPattern: RawUTF8);
var
  P: PUTF8Char;
  e: integer;
begin
  Create(Owner, aAdmin);
  pmFilterClick(FMenuFilterNone);
  P := pointer(aEvents);
  while P <> nil do begin
    e := PTypeInfo(TypeInfo(TSynLogInfo))^.EnumBaseType.GetEnumNameValue(
      pointer(GetNextItem(P)));
    if e > 0 then // ignore e=0=sllNone
      chklstEvents.Checked[e - 1] := True;
  end;
  FCallbackPattern := UpperCase(aPattern);
  btnStartLogClick(self);
  btnStopLog.Hide; { TODO: allow event log closing }
end;

destructor TLogFrame.Destroy;
begin
  FLogSafe.Done;
  inherited;
end;

procedure TLogFrame.btnStopLogClick(Sender: TObject);
var
  E: TSynLogInfo;
begin
  chklstEvents.Top := 56;
  chklstEvents.Items.Clear;
  for E := succ(sllNone) to high(E) do begin
    if (Sender = Self) and not (E in FLog.Events) then
      continue; // from TLogFrame.CreateCustom()
    chklstEvents.Items.AddObject(ToCaption(E), pointer(ord(E)));
  end;
  chklstEvents.Height := 8 + chklstEvents.Count * chklstEvents.ItemHeight;
  pmFilterClick(FMenuFilterAll);
  if Sender = nil then
    exit;
  btnStartLog.Show;
  btnStopLog.Hide;
  edtExistingLogKB.Show;
  lblExistingLogKB.Show;
  edtSearch.Hide;
  btnSearchNext.Hide;
  BtnSearchPrevious.Hide;
  mmoBottom.Text := '';
  drwgrdEvents.Row := 0;
  drwgrdEvents.RowCount := 0;
  drwgrdEvents.Tag := 0;
  tmrRefresh.Enabled := false;
  (Owner as TAdminControl).EndLog(self);
end;

procedure TLogFrame.LogFilter(F: TSynLogInfos);
var
  i: integer;
begin
  for i := 0 to chklstEvents.Count - 1 do
    chklstEvents.Checked[i] := TSynLogInfo(chklstEvents.Items.Objects[i]) in F;
  chklstEventsClickCheck(nil);
end;

procedure TLogFrame.pmFilterClick(Sender: Tobject);
begin
  if Sender.InheritsFrom(TMenuItem) then
    LogFilter(LOG_FILTER[TSynLogFilter(TMenuItem(Sender).Tag)]);
end;

procedure TLogFrame.EventsCheckToLogEvents;
var
  i: integer;
  events: TSynLogInfos;
begin
 integer(events) := 0;
  for i := 0 to chklstEvents.Count - 1 do
    if chklstEvents.Checked[i] then
      Include(events, TSynLogInfo(chklstEvents.Items.Objects[i]));
  FLog.Events := events;
end;

procedure TLogFrame.btnStartLogClick(Sender: TObject);
var
  cb: TLogFrameCallback;
  kb, i: integer;
begin
  cb := TLogFrameCallback.Create;
  cb.Owner := Self;
  cb.Pattern := FCallbackPattern;
  Callback := cb;
  FLogSafe.Lock;
  try
    try
      FLog := TSynLogFileView.Create;
      drwgrdEvents.DoubleBuffered := true;
      drwgrdEvents.ColCount := 4;
      drwgrdEvents.ColWidths[0] := 70;
      drwgrdEvents.ColWidths[1] := 60;
      drwgrdEvents.ColWidths[2] := 24;
      drwgrdEvents.ColWidths[3] := 2000;
      if Sender = self then
        kb := 64 // from TLogFrame.CreateCustom
      else
        kb := StrToIntDef(edtExistingLogKB.Text, 0);
      EventsCheckToLogEvents; // fill FLog.Events
      Admin.SubscribeLog(FLog.Events, Callback, kb);
      chklstEvents.Top := lblExistingLogKB.Top;
      for i := chklstEvents.Count - 1 downto 0 do
        if not chklstEvents.Checked[i] then
          chklstEvents.Items.Delete(i);
      chklstEvents.Height := 8 + chklstEvents.Count * chklstEvents.ItemHeight;
      btnStopLog.Top := chklstEvents.Top + chklstEvents.Height + 8;
      btnStartLog.Hide;
      btnStopLog.Show;
      edtExistingLogKB.Hide;
      lblExistingLogKB.Hide;
      edtSearch.Show;
      btnSearchNext.Show;
      BtnSearchPrevious.Show;
      drwgrdEvents.Show;
      tmrRefresh.Enabled := true;
    except
      Callback := nil;
      FreeAndNil(FLog);
    end;
  finally
    fLogSafe.UnLock;
  end;
end;

const
  TAG_NONE = 0;
  TAG_REFRESH = 1;

procedure TLogFrame.ReceivedOne(const Text: RawUTF8);
var
  P: PUTF8Char;
  line: RawUTF8;
begin
  // warning: this method is called from WebSockets thread, not UI thread
  if Callback = nil then
    exit;
  FLogSafe.Lock;
  try
    if (FLog = nil) or (Text = '') then
      exit;
    P := pointer(Text);
    repeat // handle multiple log rows in the incoming text
      line := GetNextLine(P, P);
      if length(line) < 24 then
        continue;
      FLog.AddInMemoryLine(line);
      tmrRefresh.Tag := TAG_REFRESH; // notify tmrRefreshTimer()
    until P = nil;
  finally
    FLogSafe.UnLock;
  end;
end;

procedure TLogFrame.tmrRefreshTimer(Sender: TObject);
var
  moveToLast: boolean;
begin
  FLogSafe.Lock; // to protect tmrRefresh.Tag access from ReceivedOne()
  try
    if (tmrRefresh.Tag = TAG_NONE) or (fLog = nil) then
      exit;
    moveToLast := drwgrdEvents.Row = drwgrdEvents.RowCount - 1;
    drwgrdEvents.RowCount := FLog.SelectedCount;
    if FLog.SelectedCount > 0 then
      if (drwgrdEvents.Tag = 0) or moveToLast then begin
        drwgrdEvents.Row := FLog.SelectedCount - 1;
        drwgrdEvents.Tag := 1;
      end;
    drwgrdEvents.Invalidate;
    tmrRefresh.Tag := TAG_NONE;
  finally
    FLogSafe.UnLock;
  end;
end;

procedure TLogFrame.drwgrdEventsDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  txt: string;
  inverted: boolean;
  level: TSynLogInfo;
begin
  with drwgrdEvents.Canvas do begin
    Brush.Style := bsClear;
    FLogSafe.Lock;
    try
      txt := FLog.GetCell(ACol,ARow,level);
    finally
      FLogSafe.UnLock;
    end;
    if level=sllNone then
      Brush.Color := clLtGray else begin
      inverted := (gdFocused in State) or (gdSelected in State);
      if inverted then
        Brush.Color := clBlack else
        Brush.Color := LOG_LEVEL_COLORS[inverted,level];
      Font.Color  := LOG_LEVEL_COLORS[not inverted,level];
    end;
    FillRect(Rect);
    TextRect(Rect,Rect.Left+4,Rect.Top,txt);
  end;
end;

procedure TLogFrame.drwgrdEventsClick(Sender: TObject);
var
  row: integer;
  s: string;
  sel: TGridRect;
begin
  row := drwgrdEvents.Row;
  sel := drwgrdEvents.Selection;
  FLogSafe.Lock;
  try
    s := FLog.GetLineForMemo(row,sel.Top,sel.Bottom);
  finally
    FLogSafe.UnLock;
  end;
  mmoBottom.Text := s;
end;

procedure TLogFrame.btnSearchNextClick(Sender: TObject);
var
  s: RawUTF8;
  ndx: integer;
begin
  s := UpperCase(StringToUTF8(edtSearch.Text));
  FLogSafe.Lock;
  Screen.Cursor := crHourGlass;
  try
    if Sender=BtnSearchPrevious then
      ndx := FLog.SearchPreviousText(s,drwgrdEvents.Row) else
    if Sender=edtSearch then
      ndx := FLog.SearchNextText(s,drwgrdEvents.Row,0) else
      ndx := FLog.SearchNextText(s,drwgrdEvents.Row,1); // e.g. BtnSearchNext
    if ndx>=0 then
      SetListItem(ndx,s);
  finally
    FLogSafe.UnLock;
    Screen.Cursor := crDefault;
  end;
end;

procedure TLogFrame.SetListItem(Index: integer; const search: RawUTF8);
var
  i: integer;
  s, ss: string;
begin
  if (FLog = nil) or (cardinal(Index) >= cardinal(FLog.SelectedCount)) then
    mmoBottom.Text := ''
  else begin
    drwgrdEvents.Row := Index;
    if (search = '') and drwgrdEvents.Visible then
      drwgrdEvents.SetFocus;
    s := FLog.EventString(FLog.Selected[Index], '', 0, true);
    mmoBottom.Text := s;
    if search <> '' then begin
      ss := UTF8ToString(search);
      i := Pos(ss, SysUtils.UpperCase(s));
      if i > 0 then begin
        mmoBottom.SelStart := i - 1;
        mmoBottom.SelLength := length(ss);
        mmoBottom.SetFocus;
      end;
    end;
  end;
end;

procedure TLogFrame.Closing;
begin
  Callback := nil;
  FLogSafe.Lock;
  try
    FreeAndNil(fLog);
  finally
    FLogSafe.UnLock;
  end;
end;

procedure TLogFrame.chklstEventsDblClick(Sender: TObject);
var
  i: integer;
  E: TSynLogInfo;
begin
  if FLog.EventLevel = nil then // plain text file does not handle this
    exit;
  i := chklstEvents.ItemIndex;
  if i < 0 then
    exit;
  E := TSynLogInfo(chklstEvents.Items.Objects[i]);
  FLogSafe.Lock;
  try
    i := FLog.SearchNextEvent(E,drwgrdEvents.Row);
    if i>=0 then
      SetListItem(i);
  finally
    FLogSafe.UnLock;
  end;
end;

procedure TLogFrame.chklstEventsClickCheck(Sender: TObject);
var
  selected: integer;
begin
  if FLog = nil then
    exit;
  EventsCheckToLogEvents; // fill FLog.Events
  FLogSafe.Lock;
  try
    selected := FLog.Select(drwgrdEvents.Row);
    if cardinal(selected) < cardinal(FLog.SelectedCount) then
      drwgrdEvents.Row := 0; // avoid "Grid Out Of Range" when setting RowCount
    drwgrdEvents.RowCount := FLog.SelectedCount;
    if selected>=0 then
      SetListItem(selected);
  finally
    FLogSafe.UnLock;
  end;
  if drwgrdEvents.Visible then begin
    drwgrdEvents.Repaint;
    drwgrdEventsClick(nil);
  end;
end;

procedure TLogFrame.drwgrdEventsDblClick(Sender: TObject);
var ndx: integer;
begin
  ndx := fLog.SearchEnterLeave(drwgrdEvents.Row);
  if ndx>=0 then
    SetListItem(ndx);
end;


{ TLogFrameChat }

constructor TLogFrameChat.CreateCustom(Owner: TComponent; const aAdmin:
  IAdministratedDaemon; const aEvents, aPattern: RawUTF8);
begin
  inherited;
  chklstEvents.Enabled := false;
  mmoChat := TMemo.Create(self);
  mmoChat.Parent := self;
  mmoChat.Height := 40;
  mmoChat.Align := alTop;
  mmoChat.OnKeyPress := mmoChatKeyPress;
end;

procedure TLogFrameChat.mmoChatKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    if Assigned(Admin) then
      Admin.DatabaseExecute('', FormatUTF8('#chat % %',
        [ExeVersion.User, StringToUTF8(mmoChat.Text)]));
    mmoChat.Clear;
    Key := #0;
  end;
end;

end.

