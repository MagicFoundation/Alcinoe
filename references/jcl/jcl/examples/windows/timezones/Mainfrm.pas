unit Mainfrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, ComCtrls, ExtCtrls,
  JclTimeZones;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TimeZoneTab: TTabSheet;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ActionList1: TActionList;
    OKAction: TAction;
    CancelAction: TAction;
    ApplyAction: TAction;
    TimeZonesCombo: TComboBox;
    Image1: TImage;
    AutoAdjustcb: TCheckBox;
    SetDaylightSavingsOptionsAction: TAction;
    TabSheet1: TTabSheet;
    CurrentTimeslv: TListView;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Supported: TLabel;
    Period: TLabel;
    Label4: TLabel;
    GMTOffset: TLabel;
    TabSheet2: TTabSheet;
    Button5: TButton;
    TimeZonesCombo2: TComboBox;
    Label5: TLabel;
    GroupBox4: TGroupBox;
    Label12: TLabel;
    CheckMonthCombo: TComboBox;
    CheckDayCombo: TComboBox;
    Label6: TLabel;
    Label7: TLabel;
    StdStartDate: TLabel;
    Period2: TLabel;
    Label10: TLabel;
    DlStartDate: TLabel;
    Label8: TLabel;
    DateIsInDaylightSavings: TLabel;
    CheckYearEdit: TEdit;
    procedure ApplyActionUpdate(Sender: TObject);
    procedure OKActionExecute(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SetDaylightSavingsOptionsActionExecute(Sender: TObject);
    procedure ApplyActionExecute(Sender: TObject);
    procedure CurrentTimeslvSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure TimeZonesComboChange(Sender: TObject);
    procedure AutoAdjustcbClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    DateTimeModified: Boolean;
    TimeZoneModified: Boolean;
    AutoAdjustModified: Boolean;
    TimeZones: TJclTimeZones;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  {JclGraphUtils,} JclDateTime;

{$R *.dfm}

procedure TForm1.ApplyActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := DateTimeModified or TimeZoneModified or AutoAdjustModified;
end;

procedure TForm1.OKActionExecute(Sender: TObject);
begin
  ApplyAction.Execute;
  Close;
end;

procedure TForm1.CancelActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Year, Month, Day: Word;
  I: Integer;
  ListItem: TListItem;
begin
  PageControl1.ActivePageIndex := 0;

  DecodeDate(Now, Year, Month, Day);

  TimeZones := TJclTimeZones.Create;

  CurrentTimeslv.Items.BeginUpdate;
  try
    // Populate the combobox
    for I := 0 to TimeZones.Count - 1 do
    begin
      TimeZonesCombo.Items.Add(TimeZones[I].DisplayDescription);
      TimeZonesCombo2.Items.Add(TimeZones[I].DisplayDescription);
      
      Listitem := CurrentTimeslv.Items.Add;
      ListItem.Caption := TimeZones[I].DisplayDescription;
      ListItem.SubItems.Add(DateTimeToStr(TimeZones[I].CurrentDateTime));
    end;
  finally
    CurrentTimeslv.Items.EndUpdate;
  end;

  // Set the itemindex to the current timezone
  TimeZonesCombo.ItemIndex := TimeZonesCombo.Items.IndexOf(TimeZones.ActiveTimeZone.DisplayDescription);
  TimeZonesCombo2.ItemIndex := TimeZonesCombo2.Items.IndexOf(TimeZones.ActiveTimeZone.DisplayDescription);

  CurrentTimeslv.Selected := CurrentTimeslv.items[TimeZonesCombo.ItemIndex];
  CurrentTimeslv.Selected.MakeVisible(False);

  // Now set/unset the auto-adjust checkbox accordingly
  SetDaylightSavingsOptionsAction.Execute;

  AutoAdjustModified := False;
  DateTimeModified := False;
  TimeZoneModified := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(TimeZones);
end;

procedure TForm1.SetDaylightSavingsOptionsActionExecute(Sender: TObject);
begin
  AutoAdjustcb.Visible := TimeZones[TimeZonesCombo.ItemIndex].SupportsDaylightSavings;
  AutoAdjustcb.Checked := TimeZones.AutoAdjustEnabled;
end;

procedure TForm1.ApplyActionExecute(Sender: TObject);
begin
  // Set the timezone
  if TimeZoneModified then
    TimeZones[TimeZonesCombo.ItemIndex].ApplyTimeZone;

  if AutoAdjustModified then
    TimeZones.SetAutoAdjustEnabled(AutoAdjustcb.checked);

  DateTimeModified := False;
  TimeZoneModified := False;
  AutoAdjustModified := False;
end;

procedure TForm1.CurrentTimeslvSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if (Item.Index <> -1) and Assigned(TimeZones) then
  begin
    GMTOffset.Caption := TimeZones[Item.Index].GMTOffset;
    if TimeZones[Item.Index].SupportsDaylightSavings then
      Supported.Caption := 'Yes'
    else
      Supported.Caption := 'No';

    Period.Caption := TimeZones[Item.Index].DayLightSavingsPeriod;
  end;
end;

procedure TForm1.TimeZonesComboChange(Sender: TObject);
begin
  // TODO: Make the change in the listview
  // TODO: Move the map
  TimeZoneModified := True;
  SetDaylightSavingsOptionsAction.Execute;
end;

procedure TForm1.AutoAdjustcbClick(Sender: TObject);
begin
  AutoAdjustModified := True;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  TZ: TJclTimeZoneInfo;
  Date: TDateTime;
  d,m: word;
  y: integer;
begin
  if (TimeZonesCombo2.ItemIndex = -1) or (CheckDayCombo.ItemIndex= -1) or (CheckMonthCombo.ItemIndex= -1) then
    Exit;

  try
    y := StrToInt(CheckYearEdit.Text);
  except
    on E:EConvertError do
    begin
      Showmessage('Not a valid year');
      Exit;
    end;
  end;
  d := CheckDayCombo.ItemIndex + 1;
  m := CheckMonthCombo.ItemIndex + 1;

  try
    Date:=EncodeDate(y,m,d);
  except
    on E:EConvertError do
    begin
      Showmessage('Not a valid date');
      Exit;
    end;
  end;

  TZ:=TimeZones[TimeZonesCombo2.ItemIndex];
  if not TZ.SupportsDaylightSavings then
  begin
    Showmessage('This timezone does not support daylight savings');
    Exit;
  end;

  DLStartDate.Caption:=DateToStr(TZ.DaylightStartDateInYear(y));
  StdStartDate.Caption:=DateToStr(TZ.StandardStartDateInYear(y));

  Period2.Caption := TZ.DayLightSavingsPeriod;

  if TZ.DateTimeIsInDaylightSavings(Date) then
    DateIsInDaylightSavings.Caption:='Yes'
  else
    DateIsInDaylightSavings.Caption:='No';
end;

end.

