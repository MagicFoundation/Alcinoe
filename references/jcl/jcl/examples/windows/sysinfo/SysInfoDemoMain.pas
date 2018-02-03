unit SysInfoDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    pageSysInfo: TPageControl;
    btnUpdate: TButton;
    btnOk: TButton;
    tabSystemFolders: TTabSheet;
    edtCommonFiles: TEdit;
    Label1: TLabel;
    edtCurrentFolder: TEdit;
    Label2: TLabel;
    edtProgramFiles: TEdit;
    Label3: TLabel;
    edtWindowsFolder: TEdit;
    Label4: TLabel;
    edtSystemFolder: TEdit;
    Label5: TLabel;
    edtTempFolder: TEdit;
    Label6: TLabel;
    tabCommonDirectories: TTabSheet;
    Label30: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    edtCommonStartmenuFolder: TEdit;
    edtCommonProgramsFolder: TEdit;
    edtCommonDesktopDirectory: TEdit;
    Label11: TLabel;
    edtCommonFavoritesFolder: TEdit;
    Label15: TLabel;
    edtCommonStartupFolder: TEdit;
    tabCurrentUser: TTabSheet;
    Label7: TLabel;
    Label9: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    edtDesktopFolder: TEdit;
    edtProgramsFolder: TEdit;
    edtPersonalFolder: TEdit;
    edtFavoritesFolder: TEdit;
    edtStartupFolder: TEdit;
    Label8: TLabel;
    edtRecentFilesFolder: TEdit;
    Label16: TLabel;
    edtSendToFolder: TEdit;
    Label17: TLabel;
    edtStartMenuFolder: TEdit;
    Label24: TLabel;
    Label25: TLabel;
    edtAppdataFolder: TEdit;
    edtPrintHoodFolder: TEdit;
    Label10: TLabel;
    Label18: TLabel;
    edtDesktopDirectory: TEdit;
    edtNethoodFolder: TEdit;
    Label21: TLabel;
    edtTemplatesFolder: TEdit;
    Label20: TLabel;
    edtFontsFolder: TEdit;
    Label26: TLabel;
    edtInternetCacheFolder: TEdit;
    Label27: TLabel;
    edtCookiesFolder: TEdit;
    Label28: TLabel;
    edtHistoryFolder: TEdit;
    tabAPM: TTabSheet;
    edtBatteryLifeTime: TEdit;
    Label19: TLabel;
    Label29: TLabel;
    edtBatteryFullLifeTime: TEdit;
    Label31: TLabel;
    Label32: TLabel;
    edtBatteryLineStatus: TEdit;
    Label33: TLabel;
    edtBatteryFlag: TEdit;
    pgrsBatteryLife: TProgressBar;
    lblAPMPlatforms: TLabel;
    tabMemory: TTabSheet;
    Label34: TLabel;
    edtMaxAppAddress: TEdit;
    Label35: TLabel;
    edtMinAppAddress: TEdit;
    Label36: TLabel;
    pgrsMemLoad: TProgressBar;
    Label37: TLabel;
    edtSwapFileSize: TEdit;
    Label38: TLabel;
    pgrsSwapFileUsage: TProgressBar;
    Label39: TLabel;
    edtPhysicalTotal: TEdit;
    edtPhysicalFree: TEdit;
    Label40: TLabel;
    Label41: TLabel;
    edtVirtualTotal: TEdit;
    edtVirtualFree: TEdit;
    Label42: TLabel;
    Label43: TLabel;
    edtPageFileTotal: TEdit;
    edtPageFileFree: TEdit;
    Label44: TLabel;
    tabKeyboard: TTabSheet;
    Label45: TLabel;
    edtNumLockState: TEdit;
    Label46: TLabel;
    edtCapsLockState: TEdit;
    Label47: TLabel;
    edtScrollLockState: TEdit;
    tabIdentification: TTabSheet;
    grpBIOS: TGroupBox;
    Label48: TLabel;
    edtBIOSName: TEdit;
    Label49: TLabel;
    edtBIOSCopyright: TEdit;
    Label50: TLabel;
    edtBIOSExtendedInfo: TEdit;
    Label51: TLabel;
    edtBIOSDate: TEdit;
    grpNetwork: TGroupBox;
    Label52: TLabel;
    edtIPAddress: TEdit;
    Label53: TLabel;
    lbMACAddresses: TListBox;
    Label54: TLabel;
    edtDomain: TEdit;
    tabProcesses: TTabSheet;
    Label55: TLabel;
    lbProcesses: TListBox;
    GroupBox1: TGroupBox;
    Label56: TLabel;
    edtUserName: TEdit;
    Label57: TLabel;
    edtRegisteredCompany: TEdit;
    Label58: TLabel;
    edtRegisteredOwner: TEdit;
    TasksListBox: TListBox;
    Label59: TLabel;
    Bevel1: TBevel;
    Label60: TLabel;
    LabelSysResources: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure UpdateGUI;
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  JclSysInfo, Registry;

{$R *.DFM}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  pageSysInfo.ActivePage := tabSystemFolders;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateGUI;
end;

procedure TMainForm.btnOkClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.UpdateGUI;
begin
  // Directories
  edtCommonFiles.Text := GetCommonFilesFolder;
  edtCurrentFolder.Text := GetCurrentFolder;
  edtProgramFiles.Text := GetProgramFilesFolder;
  edtWindowsFolder.Text := GetWindowsFolder;
  edtSystemFolder.Text := GetWindowsSystemFolder;
  edtTempFolder.Text := GetWindowsTempFolder;

  edtDesktopFolder.Text := GetDesktopFolder;
  edtProgramsFolder.Text := GetProgramsFolder;
  edtPersonalFolder.Text := GetPersonalFolder;
  edtFavoritesFolder.Text := GetFavoritesFolder;
  edtStartupFolder.Text := GetStartupFolder;
  edtRecentFilesFolder.Text := GetRecentFolder;
  edtSendToFolder.Text := GetSendToFolder;
  edtStartMenuFolder.Text := GetStartmenuFolder;

  edtDesktopDirectory.Text := GetDesktopDirectoryFolder;
  edtNethoodFolder.Text := GetNethoodFolder;
  edtFontsFolder.Text := GetFontsFolder;
  edtTempFolder.Text := GetTemplatesFolder;
  edtCommonStartmenuFolder.Text := GetCommonStartmenuFolder;
  edtCommonProgramsFolder.Text := GetCommonProgramsFolder;
  edtCommonStartupFolder.Text := GetCommonStartupFolder;
  edtCommonDesktopDirectory.Text := GetCommonDesktopdirectoryFolder;
  edtAppdataFolder.Text := GetAppdataFolder;
  edtPrintHoodFolder.Text := GetPrinthoodFolder;
  edtCommonFavoritesFolder.Text := GetCommonFavoritesFolder;
  edtInternetCacheFolder.Text := GetInternetCacheFolder;
  edtCookiesFolder.Text := GetCookiesFolder;
  edtHistoryFolder.Text := GetHistoryFolder;

  // APM is only available on Windows 9x / Win2K / WinXP
  if GetWindowsVersion in [wvWinNT31, wvWinNT35, wvWinNT351, wvWinNT4] then
    lblAPMPlatforms.Visible := true
  else
  begin
    lblAPMPlatforms.Visible := false;
    edtBatteryLifetime.Text := IntToStr(GetAPMBatteryLifeTime);
    edtBatteryFullLifeTime.Text := IntToStr(GetAPMBatteryFullLifeTime);
    pgrsBatteryLife.Position := GetAPMBatteryLifePercent;
  end;

  // Memory
  edtMaxAppAddress.Text := IntToHex(GetMaxAppAddress, 8);
  edtMinAppAddress.Text := IntToHex(GetMinAppAddress, 8);
  pgrsMemLoad.Position := GetMemoryLoad;
  edtSwapFileSize.Text := IntToStr(GetSwapFileSize);
  pgrsSwapFileUsage.Position := GetSwapFileUsage;

  edtPhysicalTotal.Text := IntToStr(GetTotalPhysicalMemory);
  edtPhysicalFree.Text := IntToStr(GetFreePhysicalMemory);
  edtVirtualTotal.Text := IntToStr(GetTotalVirtualMemory);
  edtVirtualFree.Text := IntToStr(GetFreeVirtualMemory);
  edtPageFileTotal.Text := IntToStr(GetTotalPageFileMemory);
  edtPageFileFree.Text := IntToStr(GetFreePageFileMemory);

  if IsWinNT then
    LabelSysResources.Caption := 'System resources meter is not available on NT systems'
  else
  if not IsSystemResourcesMeterPresent then
    LabelSysResources.Caption := 'System resources meter tool is not installed'
  else
    with GetFreeSystemResources do
      LabelSysResources.Caption := Format('User: %d%%,  System: %d%%,  Gdi: %d%%', [UserRes, SystemRes, GdiRes]);

  // Keyboard
  if GetNumLockKeyState = true then
    edtNumLockState.Text := 'ON'
  else
    edtNumLockState.Text := 'OFF';

  if GetScrollLockKeyState = true then
    edtScrollLockState.Text := 'ON'
  else
    edtScrollLockState.Text := 'OFF';

  if GetCapsLockKeyState = true then
    edtCapsLockState.Text := 'ON'
  else
    edtCapsLockState.Text := 'OFF';

  // BIOS
  if IsWinNT then begin
    grpBIOS.Caption := ' BIOS (Currently only availabe under Windows 9x) ';
    edtBIOSDate.Text := DateToStr(GetBiosDate);
  end
  else begin
    edtBIOSName.Text := GetBIOSName;
    edtBIOSCopyright.Text := GetBiosCopyright;
    edtBIOSExtendedInfo.Text := GetBIOSExtendedInfo;
  end;

  // Network Identification
  edtIPAddress.Text := GetIPAddress(GetLocalComputerName);
  GetMacAddresses(GetLocalComputerName, lbMACAddresses.Items);
  edtDomain.Text := GetDomainName;

  // User Identification
  edtUserName.Text := GetLocalUserName;
  edtRegisteredCompany.Text := GetRegisteredCompany;
  edtRegisteredOwner.Text := GetRegisteredOwner;

  // Processes
  lbProcesses.Items.BeginUpdate;
  try
    lbProcesses.Items.Clear;
    RunningProcessesList(lbProcesses.Items);
  finally
    lbProcesses.Items.EndUpdate;
  end;

  // Tasks
  TasksListBox.Items.BeginUpdate;
  try
    TasksListBox.Items.Clear;
    GetTasksList(TasksListBox.Items);
  finally
    TasksListBox.Items.EndUpdate;
  end;
end;

procedure TMainForm.btnUpdateClick(Sender: TObject);
begin
  UpdateGUI;
end;

end.
