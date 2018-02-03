unit LocalesDemoMain;

interface

{$I jcl.inc}
{$I windowsonly.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JclBase, JclLocales, ComCtrls, StdCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    LocalesListView: TListView;
    LocalesRadioGroup: TRadioGroup;
    DayNamesListBox: TListBox;
    MonthNamesListBox: TListBox;
    FormatsListBox: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Bevel1: TBevel;
    KeyblayoutsListBox: TListBox;
    Label4: TLabel;
    ActivateBtn: TButton;
    AvailableLayoutsListView: TListView;
    Label5: TLabel;
    LoadBtn: TButton;
    UnloadBtn: TButton;
    PrevBtn: TButton;
    NextBtn: TButton;
    CalendarsListBox: TListBox;
    Label6: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure LocalesRadioGroupClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LocalesListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ActivateBtnClick(Sender: TObject);
    procedure KeyblayoutsListBoxClick(Sender: TObject);
    procedure LocalesListViewCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure AvailableLayoutsListViewChange(Sender: TObject;
      Item: TListItem; Change: TItemChange);
    procedure LoadBtnClick(Sender: TObject);
    procedure UnloadBtnClick(Sender: TObject);
    procedure PrevBtnClick(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure AvailableLayoutsListViewCustomDrawItem(
      Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
  private
    LocalesList: TJclLocalesList;
    KeyboardLayoutList: TJclKeyboardLayoutList;
  public
    procedure CreateAvailableKeyLayoutsList;
    procedure CreateLocalesList;
    procedure UpdateView(ListItem: TListItem);
    procedure UpdateKeybLayouts(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses
  JclSysInfo, JclSysUtils;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CreateLocalesList;
  KeyboardLayoutList := TJclKeyboardLayoutList.Create;
  KeyboardLayoutList.OnRefresh := UpdateKeybLayouts;
  KeyboardLayoutList.Refresh;
  CreateAvailableKeyLayoutsList;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(LocalesList);
  FreeAndNil(KeyboardLayoutList);
end;

procedure TMainForm.CreateLocalesList;
var
  I: Integer;
begin
  FreeAndNil(LocalesList);
  case LocalesRadioGroup.ItemIndex of
    0: LocalesList := TJclLocalesList.Create(lkSupported);
    1: LocalesList := TJclLocalesList.Create(lkInstalled);
  end;
  with LocalesListView do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      for I := 0 to LocalesList.Count - 1 do
        with Items.Add, LocalesList[I] do
        begin
          Caption := EnglishCountryName;
          Data := LocalesList[I];
          SubItems.Add(Format('%.4x', [LocaleID]));
          SubItems.Add(EnglishLangName);
          SubItems.Add(AbbreviatedLangName);
          SubItems.Add(Format('%d', [CodePageANSI]));
          UseSystemACP := False;
          SubItems.Add(MonetarySymbolLocal);
          UseSystemACP := True;
          SubItems.Add(MonetarySymbolIntl);
          SubItems.Add(Format('%d', [CountryCode]));
        end;
      AlphaSort;
      Selected := Items[0];
      Selected.MakeVisible(False);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TMainForm.LocalesRadioGroupClick(Sender: TObject);
begin
  CreateLocalesList;
end;

procedure TMainForm.UpdateView(ListItem: TListItem);
var
  I: Integer;
begin
  if ListItem = nil then Exit;
  with TJclLocaleInfo(ListItem.Data) do
  begin
    UseSystemACP := False;
    with DayNamesListBox do
    begin
      Items.Clear;
      Font.Charset := FontCharset;
      for I := Low(TJclLocalesDays) to High(TJclLocalesDays) do
        Items.Add(Format('[%d.] %s', [I, LongDayNames[I]]));
    end;
    with MonthNamesListBox do
    begin
      Items.Clear;
      Font.Charset := FontCharset;
      for I := Low(TJclLocalesMonths) to High(TJclLocalesMonths) - 1 do
        Items.Add(Format('[%.2d.] %s', [I, LongMonthNames[I]]));
    end;
    with FormatsListBox do
    begin
      Font.Charset := FontCharset;
      Items.Clear;
      Items.Add('Long date formats:');
      Items.AddStrings(DateFormats[ldLong]);
      Items.Add('');
      Items.Add('Short date formats:');
      Items.AddStrings(DateFormats[ldShort]);
      if GetWindowsVersion >= wvWin2000 then
      begin
        Items.Add('');
        Items.Add('Year month formats:');
        Items.AddStrings(DateFormats[ldYearMonth]);
      end;
      Items.Add('');
      Items.Add('Time formats:');
      Items.AddStrings(TimeFormats);
    end;
    with CalendarsListBox do
    begin
      Font.Charset := FontCharset;
      Items.Assign(Calendars);
    end;  
    UseSystemACP := True;
  end;
end;

procedure TMainForm.LocalesListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected then UpdateView(Item);
end;

procedure TMainForm.UpdateKeybLayouts(Sender: TObject);
var
  I: Integer;
begin
  with KeyblayoutsListBox do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      for I := 0 to KeyboardLayoutList.Count - 1 do
        Items.AddObject(Format('[%.8x] %s', [ KeyboardLayoutList[I].Layout,
          KeyboardLayoutList[I].DisplayName]), KeyboardLayoutList[I]);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TMainForm.ActivateBtnClick(Sender: TObject);
begin
  with KeyblayoutsListBox do
    TJclKeyboardLayout(Items.Objects[ItemIndex]).Activate([klActivate]);
end;

procedure TMainForm.KeyblayoutsListBoxClick(Sender: TObject);
begin
  ActivateBtn.Enabled := KeyblayoutsListBox.ItemIndex >= 0;
  UnloadBtn.Enabled := ActivateBtn.Enabled;
end;

procedure TMainForm.LocalesListViewCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  with Sender.Canvas.Font do 
    if SubItem = 5 then
      Charset := TJclLocaleInfo(Item.Data).FontCharset
    else
      Charset := DEFAULT_CHARSET;
end;

procedure TMainForm.CreateAvailableKeyLayoutsList;
var
  I: Integer;
begin
  with AvailableLayoutsListView do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      for I := 0 to KeyboardLayoutList.AvailableLayoutCount - 1 do
        with Items.Add, KeyboardLayoutList.AvailableLayouts[I] do
        begin
          Caption := Name;
          Data := KeyboardLayoutList.AvailableLayouts[I];
          SubItems.Add(IdentifierName);
          SubItems.Add(Format('%.4x', [LayoutID]));
          SubItems.Add(LayoutFile);
        end;
      AlphaSort;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TMainForm.AvailableLayoutsListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  LoadBtn.Enabled := AvailableLayoutsListView.Selected <> nil;
end;

procedure TMainForm.LoadBtnClick(Sender: TObject);
begin
  Win32Check(TJclAvailableKeybLayout(AvailableLayoutsListView.Selected.Data).Load([]));
end;

procedure TMainForm.UnloadBtnClick(Sender: TObject);
begin
  with KeyblayoutsListBox do
    Win32Check(TJclKeyboardLayout(Items.Objects[ItemIndex]).Unload);
end;

procedure TMainForm.PrevBtnClick(Sender: TObject);
begin
  KeyboardLayoutList.ActivatePrevLayout;
end;

procedure TMainForm.NextBtnClick(Sender: TObject);
begin
  KeyboardLayoutList.ActivateNextLayout;
end;

procedure TMainForm.AvailableLayoutsListViewCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  with Sender do
    if not TJclAvailableKeybLayout(Item.Data).LayoutFileExists then
      Canvas.Font.Color := clInactiveCaption;
end;

end.
