{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) - Delphi Tools                                                   }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is Main.pas.                                                                   }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date$                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit Main;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ImgList, StdCtrls, ToolWin, Menus, ActnList, ExtCtrls, IniFiles;

const
  UM_ACTIVATEMAINFORM = WM_USER + $100;

type
  TMainForm = class(TForm)
    ProcessListView: TListView;
    PriorityImagesList: TImageList;
    MainMenu: TMainMenu;
    ActionList1: TActionList;
    Exit1: TAction;
    ExitItem: TMenuItem;
    File1: TMenuItem;
    StatusBar: TStatusBar;
    Tools1: TMenuItem;
    Terminate1: TAction;
    TerminateItem: TMenuItem;
    Refresh1: TAction;
    RefreshItem: TMenuItem;
    About1: TAction;
    Help1: TMenuItem;
    AboutItem: TMenuItem;
    HotTrack1: TAction;
    HotTrackItem: TMenuItem;
    SaveToFile1: TAction;
    SaveItem: TMenuItem;
    N2: TMenuItem;
    FileProperties1: TAction;
    FilePropItem: TMenuItem;
    PopupMenu: TPopupMenu;
    RefreshItemP: TMenuItem;
    SaveItemP: TMenuItem;
    TerminateItemP: TMenuItem;
    PropertyItemP: TMenuItem;
    N3: TMenuItem;
    ChangePriority1: TAction;
    ChangePriorityItem: TMenuItem;
    N5: TMenuItem;
    ChangePriorityItemP: TMenuItem;
    BottomPanel: TPanel;
    ModulesListView: TListView;
    ThreadsListView: TListView;
    Splitter2: TSplitter;
    Splitter1: TSplitter;
    Views1: TMenuItem;
    N1: TMenuItem;
    Copy1: TAction;
    CopyItem: TMenuItem;
    CopyItemP: TMenuItem;
    DumpHeap1: TAction;
    DumpHeapItem: TMenuItem;
    DumpHeapItemP: TMenuItem;
    DumpMemory1: TAction;
    DumpMemory11: TMenuItem;
    MemoryList1: TMenuItem;
    Options1: TMenuItem;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    RefreshButton: TToolButton;
    HottrackButton: TToolButton;
    ToolButton7: TToolButton;
    CopyButton: TToolButton;
    SaveButton: TToolButton;
    ToolButton3: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ChangePriButton: TToolButton;
    KillButton: TToolButton;
    PropertyButton: TToolButton;
    ToolButton5: TToolButton;
    InfoTip1: TAction;
    ToolButton8: TToolButton;
    InfoTip2: TMenuItem;
    BeepOnChange1: TAction;
    ToolButton9: TToolButton;
    Beeponchange2: TMenuItem;
    CheckImageBase1: TAction;
    ToolButton11: TToolButton;
    CheckImageBase2: TMenuItem;
    DumpModules1: TAction;
    ToolButton6: TToolButton;
    Moduleslist1: TMenuItem;
    N4: TMenuItem;
    Moduleslist2: TMenuItem;
    DumpPE1: TAction;
    DumpPEfile1: TMenuItem;
    ToolButton10: TToolButton;
    DumpPEfile2: TMenuItem;
    SendMail1: TAction;
    Support1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ProcessListViewCompare(Sender: TObject; Item1,
      Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure ProcessListViewColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure Exit1Execute(Sender: TObject);
    procedure Terminate1Execute(Sender: TObject);
    procedure Refresh1Execute(Sender: TObject);
    procedure About1Execute(Sender: TObject);
    procedure Terminate1Update(Sender: TObject);
    procedure HotTrack1Execute(Sender: TObject);
    procedure SaveToFile1Update(Sender: TObject);
    procedure SaveToFile1Execute(Sender: TObject);
    procedure FileProperties1Update(Sender: TObject);
    procedure FileProperties1Execute(Sender: TObject);
    procedure ProcessListViewEnter(Sender: TObject);
    procedure ChangePriority1Execute(Sender: TObject);
    procedure Copy1Execute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StatusBarResize(Sender: TObject);
    procedure DumpHeap1Execute(Sender: TObject);
    procedure DumpMemory1Execute(Sender: TObject);
    procedure ProcessListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ModulesListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ProcessListViewInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: string);
    procedure ModulesListViewInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: string);
    procedure InfoTip1Execute(Sender: TObject);
    procedure BeepOnChange1Execute(Sender: TObject);
    procedure CheckImageBase1Execute(Sender: TObject);
    procedure ModulesListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure DumpModules1Execute(Sender: TObject);
    procedure DumpPE1Update(Sender: TObject);
    procedure ProcessListViewDblClick(Sender: TObject);
    procedure DumpPE1Execute(Sender: TObject);
    procedure SendMail1Execute(Sender: TObject);
    procedure CoolBar1Resize(Sender: TObject);
  private
    FDisableUpdate: Boolean;
    FProcess_Cnt, FThreads_Cnt, FModules_Cnt, FModules_Size: LongWord;
    FIniFile: TIniFile;
    procedure BuildModulesList(ProcessID: DWORD);
    procedure BuildProcessList(Rebuild: Boolean = False);
    procedure BuildThreadsList(ProcessID: DWORD);
    function CheckProcessesChange: Boolean;
    function FocusedFileName: TFileName;
    procedure KillProcess(ProcessID: DWORD);
    procedure LoadSettings;
    procedure RebuildViewsMenuHotKeys;
    procedure SaveSettings;
    function SummaryInfo: string;
    procedure TimerRefresh;
    procedure UpdateListViewsOptions;
    procedure UpdateStatusLine(SummaryOnly: Boolean = False);
    procedure ViewsMenuClick(Sender: TObject);
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMMenuChar(var Msg: TWMMenuChar); message WM_MENUCHAR;
    procedure UMActivateMainForm(var Msg: TMessage); message UM_ACTIVATEMAINFORM;
  public
    procedure AddToViewsMenu(AForm: TForm; const ACaption: string);
    procedure DeleteFromViewsMenu(AForm: TForm);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses
  TLHelp32, About, ShellAPI, ChangePriority, HeapDump, MemoryDump, Global,
  CommCtrl, JclShell, JclSysInfo, JclFileUtils, JclAppInst, ModulesDump,
  ToolsUtils, FindDlg, PsApi;

resourcestring
  sCantOpenForTerminate = 'Can''t open this process for terminate.';
  sKill = 'Do you really want to kill process "%s" ?';
  sNotFound = 'Not found';
  sSaveProcessesList = 'ToolHelp process list';
  sSaveModulesList = 'Modules used by process %s';
  sSaveThreadsList = 'Threads created by process %s';
  sWaitTimeout = 'Timeout.';
  sProcessesSummary = 'Processes: %d, Threads: %d';
  sModulesSummary = 'Cnt: %d, Tot.Size: %.0n';
  sNotRelocated = '[base]';

const
  PROCESS_CLASS_IDLE          = 4;
  PROCESS_CLASS_NORMAL        = 8;
  PROCESS_CLASS_HIGH          = 13;
  PROCESS_CLASS_TIMECRITICAL  = 24;

function GetPriorityIconIndex(Priority: DWORD): Integer;
begin
  case Priority of
    PROCESS_CLASS_IDLE: Result := 0;
    PROCESS_CLASS_HIGH: Result := 1;
    PROCESS_CLASS_TIMECRITICAL: Result := 2;
  else
    Result := -1;
  end;
end;

function GetProcessVersion(Version: DWORD): string;
var
  C: array[0..2] of Char;
begin
  C[0] := Chr(Lo(LOWORD(Version)));
  C[1] := Chr(Hi(LOWORD(Version)));
  if C[0] < #32 then C[0] := '_';
  if C[1] < #32 then C[1] := '_';
  C[2] := #0;
  Result := Format('%s %d.%d', [C, Hi(HIWORD(Version)), Lo(HIWORD(Version))]);
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  FileInfo: TSHFileInfo;
  ImageListHandle: THandle;
begin
  {$IFDEF COMPILER5_UP}
  ProcessListView.OnInfoTip := ProcessListViewInfoTip;
  ModulesListView.OnInfoTip := ModulesListViewInfoTip;
  {$ELSE COMPILER5_UP}
  InfoTip1.Visible := False;
  {$ENDIF COMPILER5_UP}
  FIniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  LoadSettings;
  ImageListHandle := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  SendMessage(ProcessListView.Handle, LVM_SETIMAGELIST, LVSIL_SMALL, ImageListHandle);
  SetTimer(Handle, 1, 500, nil);
  BuildProcessList;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SaveSettings;
  FIniFile.UpdateFile;
  FIniFile.Free;
  Win32Check(KillTimer(Handle, 1));
end;

procedure TMainForm.BuildProcessList(Rebuild: Boolean = False);
var
  SnapProcHandle, ProcessHandle: THandle;
  ProcessEntry: TProcessEntry32;
  Next: Boolean;
  FileInfo: TSHFileInfo;
  ProcessVersion: DWORD;
  FindItem: TListItem;
  I: Integer;
  ProcList: TList;
  Added, Changed: Boolean;

  procedure CheckChanged;
begin
  if ProcessListView.ItemFocused = FindItem then Changed := True;
end;

begin
  if FDisableUpdate then Exit;
  ProcList := TList.Create;
  Added := False;
  Changed := False;
  with ProcessListView do
  try
    FDisableUpdate := True;
    try
      if Rebuild then
      begin
        Screen.Cursor := crHourGlass;
        Items.BeginUpdate;
        Items.Clear;
        FProcess_Cnt := 0;
        FThreads_Cnt := 0;
      end else
        SendMessage(Handle, WM_SETREDRAW, 0, 0);
      SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
      if SnapProcHandle <> THandle(-1) then
      begin
        ProcessEntry.dwSize := Sizeof(ProcessEntry);
        Next := Process32First(SnapProcHandle, ProcessEntry);
        while Next do
        begin
          ProcList.Add(Pointer(ProcessEntry.th32ProcessID));
          FindItem := FindData(0, Pointer(ProcessEntry.th32ProcessID), True, False);
          with ProcessEntry do if FindItem = nil then
          begin // New Process
            Added := True;
            if GetWindowsVersion >= wvWin2000 then
            begin
              ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, th32ProcessID);
              if ProcessHandle <> 0 then
              begin
                if GetModuleFileNameEx(ProcessHandle, 0, szExeFile, SizeOf(szExeFile)) = 0 then
                  StrPCopy(szExeFile, '[Idle]');
                CloseHandle(ProcessHandle);
              end;
            end;
            ProcessVersion := SHGetFileInfo(szExeFile, 0, FileInfo, Sizeof(FileInfo), SHGFI_EXETYPE);
            SHGetFileInfo(szExeFile, 0, FileInfo, Sizeof(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
            with Items.Add, ProcessEntry do
            begin
              Caption := AnsiLowerCase(ExtractFileName(szExeFile));
              Data := Pointer(th32ProcessID);
              ImageIndex := FileInfo.iIcon;
              StateIndex := GetPriorityIconIndex(pcPriClassBase);
              SubItems.AddObject(Format('%.8x', [th32ProcessID]), Pointer(th32ProcessID));
              SubItems.AddObject(Format('%d', [pcPriClassBase]), Pointer(pcPriClassBase));
              SubItems.AddObject(Format('%d', [cntThreads]), Pointer(cntThreads));
              SubItems.AddObject(GetProcessVersion(ProcessVersion), Pointer(ProcessVersion));
              SubItems.Add(szExeFile);
              SubItems.AddObject(Format('(%.8x)', [th32ParentProcessID]), Pointer(th32ParentProcessID));
              Inc(FProcess_Cnt);
              Inc(FThreads_Cnt, cntThreads);
            end;
          end else
          with FindItem do
          begin // Any changes in existing process ?
            if SubItems.Objects[1] <> Pointer(pcPriClassBase) then
            begin
              SubItems.Objects[1] := Pointer(pcPriClassBase);
              SubItems.Strings[1] := Format('%d', [pcPriClassBase]);
              StateIndex := GetPriorityIconIndex(pcPriClassBase);
            end;
            if SubItems.Objects[2] <> Pointer(cntThreads) then
            begin
              Inc(FThreads_Cnt, cntThreads - DWORD(SubItems.Objects[2]));
              SubItems.Objects[2] := Pointer(cntThreads);
              SubItems.Strings[2] := Format('%d', [cntThreads]);
              CheckChanged;
            end;
          end;
          Next := Process32Next(SnapProcHandle, ProcessEntry);
        end;
        CloseHandle(SnapProcHandle);
      end;
      if Added then // find the names of parent processes
      begin
        for I := 0 to Items.Count - 1 do
        begin
          FindItem := FindData(0, Items[I].SubItems.Objects[5], True, False);
          if FindItem <> nil then Items[I].SubItems[5] := FindItem.Caption;
        end;
        AlphaSort;
      end;
      for I := Items.Count - 1 downto 0 do // delete non-existing processes
        if ProcList.IndexOf(Items[I].Data) = -1 then
        begin
          Dec(FProcess_Cnt);
          Dec(FThreads_Cnt, DWORD(Items[I].SubItems.Objects[2]));
          Items.Delete(I);
        end;
      if GetNextItem(nil, sdAll, [isSelected]) = nil then
      begin
        if ItemFocused = nil then ItemFocused := Items[0];
        ItemFocused.Selected := True;
      end else
        if Changed then BuildThreadsList(DWORD(ItemFocused.Data));
      UpdateStatusLine(True);
    finally
      if Rebuild then
        Items.EndUpdate
      else
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
    end;
  finally
    FDisableUpdate := False;
    ProcList.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.BuildThreadsList(ProcessID: DWORD);
var
  SnapProcHandle: THandle;
  ThreadEntry: TThreadEntry32;
  Next: Boolean;
begin
  with ThreadsListView do
  try
    Items.BeginUpdate;
    Items.Clear;
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
    if SnapProcHandle <> THandle(-1) then
    begin
      ThreadEntry.dwSize := Sizeof(ThreadEntry);
      Next := Thread32First(SnapProcHandle, ThreadEntry);
      while Next do
      begin
        if ThreadEntry.th32OwnerProcessID = ProcessID then
          with Items.Add, ThreadEntry do
          begin
            Caption := Format('%.8x', [th32ThreadID]);
            Data := Pointer(th32ThreadID);
            SubItems.AddObject(Format('%d', [tpDeltaPri]), Pointer(tpDeltaPri));
          end;
        Next := Thread32Next(SnapProcHandle, ThreadEntry);
      end;
      CloseHandle(SnapProcHandle);
    end;
    AlphaSort;
    ListViewFocusFirstItem(ThreadsListView);
  finally
    Items.EndUpdate;
  end;
end;

procedure TMainForm.BuildModulesList(ProcessID: DWORD);
var
  SnapProcHandle: THandle;
  ModuleEntry: TModuleEntry32;
  Next: Boolean;
  ImageBase: DWORD;
begin
  with ModulesListView do
  try
    Items.BeginUpdate;
    Items.Clear;
    FModules_Cnt := 0;
    FModules_Size := 0;
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, ProcessID);
    if SnapProcHandle <> THandle(-1) then
    begin
      ModuleEntry.dwSize := Sizeof(ModuleEntry);
      Next := Module32First(SnapProcHandle, ModuleEntry);
      while Next do
      begin
        with Items.Add, ModuleEntry do
        begin
          Caption := AnsiLowerCase(szModule);
          SubItems.AddObject(Format('%.8x', [th32ModuleID]), Pointer(th32ModuleID));
          if CheckImageBase1.Checked then
          begin
            ImageBase := GetImageBase(szExePath);
            if ImageBase = DWORD(modBaseAddr) then
              SubItems.AddObject(sNotRelocated, Pointer(0))
            else
             SubItems.AddObject(Format('%.8x', [ImageBase]), Pointer(ImageBase));
          end else
            SubItems.Add('');
          SubItems.AddObject(Format('%p', [modBaseAddr]), Pointer(modBaseAddr));
          SubItems.AddObject(Format('%.0n', [IntToExtended(modBaseSize)]), Pointer(modBaseSize));
          SubItems.AddObject(Format('%d', [GlblcntUsage]), Pointer(GlblcntUsage));
          SubItems.AddObject(Format('%d', [ProccntUsage]), Pointer(ProccntUsage));
          SubItems.AddObject(Format('%.8x', [hModule]), Pointer(hModule));
          SubItems.Add(szExePath);
          Inc(FModules_Cnt);
          Inc(FModules_Size, modBaseSize);
        end;
        Next := Module32Next(SnapProcHandle, ModuleEntry);
      end;
      CloseHandle(SnapProcHandle);
    end;
    AlphaSort;
    ListViewFocusFirstItem(ModulesListView);
  finally
    Items.EndUpdate;
  end;
end;

function TMainForm.CheckProcessesChange: Boolean;
var
  SnapProcHandle: THandle;
  ProcessEntry: TProcessEntry32;
  Next: Boolean;
  ProcessCount: Integer;
  FindItem: TListItem;
begin
  Result := False;
  ProcessCount := 0;
  SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if SnapProcHandle <> THandle(-1) then
  begin
    ProcessEntry.dwSize := Sizeof(ProcessEntry);
    Next := Process32First(SnapProcHandle, ProcessEntry);
    while Next and (not Result) do
    begin
      Inc(ProcessCount);
      FindItem := ProcessListView.FindData(0, Pointer(ProcessEntry.th32ProcessID), True, False);
      if FindItem = nil then
        Result := True
      else
      with FindItem do
        Result := (SubItems.Objects[1] <> Pointer(ProcessEntry.pcPriClassBase)) or
         (SubItems.Objects[2] <> Pointer(ProcessEntry.cntThreads));
      Next := Process32Next(SnapProcHandle, ProcessEntry);
    end;
    CloseHandle(SnapProcHandle);
  end;
  Result := Result or (ProcessCount <> ProcessListView.Items.Count);
end;

function TMainForm.FocusedFileName: TFileName;
begin
  if (ActiveControl = ProcessListView) and (ProcessListView.ItemFocused <> nil) then
    Result := ProcessListView.ItemFocused.SubItems[4] else
  if (ActiveControl = ModulesListView) and (ModulesListView.ItemFocused <> nil) then
    Result := ModulesListView.ItemFocused.SubItems[7] else
  Result := '';
end;

procedure TMainForm.KillProcess(ProcessID: DWORD);
var
  ProcessHandle: THandle;
begin
  ProcessHandle := OpenProcess(PROCESS_ALL_ACCESS{PROCESS_TERMINATE}, False, ProcessID);
  if ProcessHandle <> 0 then
  begin
    TerminateProcess(ProcessHandle, 0);
    if WaitForSingleObject(ProcessHandle, 10000) = WAIT_TIMEOUT then
      MessBox(sWaitTimeout, MB_ICONWARNING);
    CloseHandle(ProcessHandle);
    BuildProcessList;
  end else
    MessBox(sCantOpenForTerminate, MB_ICONERROR);
end;

function TMainForm.SummaryInfo: string;
begin
  if (ActiveControl = ProcessListView) then
    Result := Format(sProcessesSummary , [FProcess_Cnt, FThreads_Cnt]) else
  if (ActiveControl = ModulesListView) then
    Result := Format(sModulesSummary , [FModules_Cnt, IntToExtended(FModules_Size)]) else
  Result := '';
end;

procedure TMainForm.TimerRefresh;
begin
  if not Application.Terminated and IsWindowEnabled(Handle) and CheckProcessesChange then
  begin
    BuildProcessList;
    if BeepOnChange1.Checked then MessageBeep(MB_OK);
  end;
end;

procedure TMainForm.UpdateStatusLine(SummaryOnly: Boolean = False);
var
  FileName: TFileName;
begin
  FileName := FocusedFileName;
  with StatusBar.Panels do
  begin
    BeginUpdate;
    if not SummaryOnly then
    begin
      Items[0].Text := '';
      Items[1].Text := '';
      if VersionResourceAvailable(FileName) then
      try
        with TJclFileVersionInfo.Create(FileName) do
        try
          StatusBar.Panels.Items[0].Text := FileVersion;
          StatusBar.Panels.Items[1].Text := FileDescription;
        finally
          Free;
        end;
      except
      end else
        Items[0].Text := sNotFound;
    end;
    Items[2].Text := SummaryInfo;
    EndUpdate;
  end;
end;

procedure TMainForm.ProcessListViewCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  LVCompare(TListView(Sender), Item1, Item2, Compare);
end;

procedure TMainForm.ProcessListViewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  LVColumnClick(Column);
end;

procedure TMainForm.ProcessListViewEnter(Sender: TObject);
begin
  UpdateStatusLine;
end;

procedure TMainForm.Exit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.BeepOnChange1Execute(Sender: TObject);
begin
  with BeepOnChange1 do
    Checked := not Checked;
end;

procedure TMainForm.HotTrack1Execute(Sender: TObject);
begin
  with HotTrack1 do
  begin
    Checked := not Checked;
    UpdateListViewsOptions;
  end;
end;

procedure TMainForm.InfoTip1Execute(Sender: TObject);
begin
  with InfoTip1 do
  begin
    Checked := not Checked;
    UpdateListViewsOptions;
  end;
end;

procedure TMainForm.CheckImageBase1Execute(Sender: TObject);
begin
  with CheckImageBase1 do
  begin
    Checked := not Checked;
    ProcessListViewSelectItem(nil, ProcessListView.Selected, Assigned(ProcessListView.Selected));
  end;    
end;

procedure TMainForm.Terminate1Execute(Sender: TObject);
begin
  with ProcessListView do if (ItemFocused <> nil) and
    (MessBoxFmt(sKill, [ItemFocused.Caption], MB_ICONEXCLAMATION or MB_YESNO or MB_DEFBUTTON2) = ID_YES) then
      KillProcess(DWORD(ItemFocused.Data));
end;

procedure TMainForm.Refresh1Execute(Sender: TObject);
begin
  BuildProcessList(True);
end;

procedure TMainForm.About1Execute(Sender: TObject);
begin
  ShowToolsAboutBox;
end;

procedure TMainForm.ChangePriority1Execute(Sender: TObject);
begin
  with TChangePriorityDlg.Create(Application) do
  try
    ProcessID := DWORD(ProcessListView.ItemFocused.Data);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainForm.Terminate1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := (ActiveControl = ProcessListView) and
    (ProcessListView.ItemFocused <> nil);
end;

procedure TMainForm.SaveToFile1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveControl is TListView;
end;

procedure TMainForm.SaveToFile1Execute(Sender: TObject);
var
  FileName: string;
begin
  if ActiveControl = ProcessListView then
    FileName := sSaveProcessesList else
  if ActiveControl = ThreadsListView then
    FileName := Format(sSaveThreadsList, [ProcessListView.ItemFocused.Caption]) else
  if ActiveControl = ModulesListView then
    FileName := Format(sSaveModulesList, [ProcessListView.ItemFocused.Caption]);
  GlobalModule.ListViewToFile(ActiveControl as TListView, FileName);
end;

procedure TMainForm.FileProperties1Update(Sender: TObject);
begin
  FileProperties1.Enabled :=
    (ActiveControl = ProcessListView) or (ActiveControl = ModulesListView);
end;

procedure TMainForm.FileProperties1Execute(Sender: TObject);
begin
  DisplayPropDialog(Application.Handle, FocusedFileName);
end;

procedure TMainForm.AddToViewsMenu(AForm: TForm; const ACaption: string);
var
  Item: TMenuItem;
begin
  Item := TMenuItem.Create(Views1);
  Item.Caption := ACaption;
  Item.Tag := Integer(AForm);
  Item.OnClick := ViewsMenuClick;
  Views1.Add(Item);
  RebuildViewsMenuHotKeys;
end;

procedure TMainForm.DeleteFromViewsMenu(AForm: TForm);
var
  I: Integer;
begin
  with Views1 do
    for I := 0 to Count - 1 do
      if Pointer(Items[I].Tag) = AForm then
      begin
        Items[I].Free;
        System.Break;
      end;
  RebuildViewsMenuHotKeys;
end;

procedure TMainForm.ViewsMenuClick(Sender: TObject);
begin
  TForm(TMenuItem(Sender).Tag).BringToFront;
end;

procedure TMainForm.RebuildViewsMenuHotKeys;
var
  I: Integer;
begin
  for I := 0 to Views1.Count - 1 do
    if I < 9 then
      Views1.Items[I].ShortCut := ShortCut(I + 49, [ssAlt])
    else
      Views1.Items[I].ShortCut := 0;
  Views1.Visible := Views1.Count > 0;
end;

procedure TMainForm.Copy1Execute(Sender: TObject);
begin
  GlobalModule.ListViewToClipboard(ActiveControl as TListView);
end;

procedure TMainForm.WMTimer(var Msg: TWMTimer);
begin
  if Msg.TimerID = 1 then
  begin
    TimerRefresh;
    Msg.Result := 0;
  end else inherited;
end;

procedure TMainForm.WMMenuChar(var Msg: TWMMenuChar);
begin
  inherited;
  if Msg.Result = MNC_IGNORE then
    PostMessage(Handle, UM_ACTIVATEMAINFORM, 0, 0);
end;

procedure TMainForm.UMActivateMainForm(var Msg: TMessage);
begin
  BringToFront;
end;

procedure TMainForm.StatusBarResize(Sender: TObject);
begin
  with StatusBar do
    Panels[1].Width := Width - Panels[0].Width - Panels[2].Width;
end;

procedure TMainForm.DumpHeap1Execute(Sender: TObject);
begin
  FDisableUpdate := True;
  try
    with THeapDumpForm.Create(Application) do
    begin
      with ProcessListView.ItemFocused do SetParams(DWORD(Data), Caption);
      Show;
    end;
  finally
    FDisableUpdate := False;
  end;
end;

procedure TMainForm.DumpMemory1Execute(Sender: TObject);
begin
  FDisableUpdate := True;
  try
    with TMemoryDumpForm.Create(Application) do
    try
      with ProcessListView.ItemFocused do SetParams(DWORD(Data), Caption);
      Show;
    except
      Free;
      raise
    end;
  finally
    FDisableUpdate := False;
  end;
end;

procedure TMainForm.ProcessListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
  begin
    BuildThreadsList(DWORD(Item.Data));
    BuildModulesList(DWORD(Item.Data));
    UpdateStatusLine;
  end;
end;

procedure TMainForm.ModulesListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected and TWinControl(Sender).Focused then UpdateStatusLine;
end;

procedure TMainForm.ProcessListViewInfoTip(Sender: TObject;
  Item: TListItem; var InfoTip: string);
begin
  InfoTip := InfoTipVersionString(Item.SubItems[4]);
end;

procedure TMainForm.ModulesListViewInfoTip(Sender: TObject;
  Item: TListItem; var InfoTip: string);
begin
  InfoTip := InfoTipVersionString(Item.SubItems[7]);
end;

procedure TMainForm.LoadSettings;
begin
  with FIniFile do
  begin
    Left := ReadInteger(Name, 'Left', Left);
    Top := ReadInteger(Name, 'Top', Top);
    Width := ReadInteger(Name, 'Width', Width);
    Height := ReadInteger(Name, 'Height', Height);
    HotTrack1.Checked := ReadBool('Options', HotTrack1.Name, HotTrack1.Checked);
    InfoTip1.Checked := ReadBool('Options', InfoTip1.Name, InfoTip1.Checked);
    BeepOnChange1.Checked := ReadBool('Options', BeepOnChange1.Name, BeepOnChange1.Checked);
    CheckImageBase1.Checked := ReadBool('Options', CheckImageBase1.Name, CheckImageBase1.Checked);
  end;
  UpdateListViewsOptions;
end;

procedure TMainForm.SaveSettings;
begin
  with FIniFile do
  begin
    WriteInteger(Name, 'Left', Left);
    WriteInteger(Name, 'Top', Top);
    WriteInteger(Name, 'Width', Width);
    WriteInteger(Name, 'Height', Height);
    WriteBool('Options', HotTrack1.Name, HotTrack1.Checked);
    WriteBool('Options', InfoTip1.Name, InfoTip1.Checked);
    WriteBool('Options', BeepOnChange1.Name, BeepOnChange1.Checked);
    WriteBool('Options', CheckImageBase1.Name, CheckImageBase1.Checked);
  end;
end;

procedure TMainForm.UpdateListViewsOptions;
begin
  ProcessListView.HotTrack := HotTrack1.Checked;
  ThreadsListView.HotTrack := HotTrack1.Checked;
  ModulesListView.HotTrack := HotTrack1.Checked;
  ProcessListView.ShowHint := InfoTip1.Checked;
  ThreadsListView.ShowHint := InfoTip1.Checked;
  ModulesListView.ShowHint := InfoTip1.Checked;
end;

procedure TMainForm.ModulesListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Item.SubItems.Objects[1] <> nil then
    Sender.Canvas.Font.Style := [fsunderline];
end;

procedure TMainForm.DumpModules1Execute(Sender: TObject);
begin
  if not Assigned(ModulesDumpForm) then
    ModulesDumpForm := TModulesDumpForm.Create(Application);
  ModulesDumpForm.Show;
end;

procedure TMainForm.DumpPE1Update(Sender: TObject);
begin
  DumpPE1.Enabled := GlobalModule.PeViewerRegistred and (Length(FocusedFileName) > 0);
end;

procedure TMainForm.ProcessListViewDblClick(Sender: TObject);
begin
  DumpPE1.Execute; 
end;

procedure TMainForm.DumpPE1Execute(Sender: TObject);
begin
  GlobalModule.ViewPE(FocusedFileName);
end;

procedure TMainForm.SendMail1Execute(Sender: TObject);
begin
  SendEmail;
end;

procedure TMainForm.CoolBar1Resize(Sender: TObject);
begin
  D4FixCoolBarResizePaint(Sender);
end;

end.
