unit NtSvcDemoMain;

interface

{$I jcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ActnList, Menus, JclSvcCtrl;

type
  TfrmMain = class(TForm)
    lstSvc: TListView;
    lstActions: TActionList;
    actViewRefresh: TAction;
    mnuPopup: TPopupMenu;
    popViewRefresh: TMenuItem;
    actFileConnect: TAction;
    actFileExit: TAction;
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileConnect: TMenuItem;
    mnuFileLine1: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuView: TMenuItem;
    mnuViewRefreshStatus: TMenuItem;
    actHelpAbout: TAction;
    mnuHelp: TMenuItem;
    mnuHelpAbout: TMenuItem;
    barStatus: TStatusBar;
    actControlStart: TAction;
    actControlStop: TAction;
    actControlPause: TAction;
    actControlContinue: TAction;
    mnuControl: TMenuItem;
    mnuControlStart: TMenuItem;
    mnuControlStop: TMenuItem;
    mnuControlPause: TMenuItem;
    mnuControlContinue: TMenuItem;
    popLine1: TMenuItem;
    popControlStart: TMenuItem;
    popControlStop: TMenuItem;
    popControlPause: TMenuItem;
    popControlContinue: TMenuItem;
    actViewDependent: TAction;
    mnuViewDependent: TMenuItem;
    mnuViewLine1: TMenuItem;
    popLine2: TMenuItem;
    popViewDependent: TMenuItem;
    actViewGroups: TAction;
    mnuViewGroups: TMenuItem;
    popViewGroups: TMenuItem;
    actControlDelete: TAction;
    mnuControlLine1: TMenuItem;
    mnuControlDelete: TMenuItem;
    popLine0: TMenuItem;
    popControlDelete: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstSvcData(Sender: TObject; Item: TListItem);
    procedure actViewRefreshExecute(Sender: TObject);
    procedure lstSvcColumnClick(Sender: TObject; Column: TListColumn);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileConnectExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actControlStartExecute(Sender: TObject);
    procedure actControlStopExecute(Sender: TObject);
    procedure actControlPauseExecute(Sender: TObject);
    procedure actControlContinueExecute(Sender: TObject);
    procedure actControlStartUpdate(Sender: TObject);
    procedure actControlStopUpdate(Sender: TObject);
    procedure actControlPauseUpdate(Sender: TObject);
    procedure actControlContinueUpdate(Sender: TObject);
    procedure actControlDeleteExecute(Sender: TObject);
    procedure ActionItemSelected(Sender: TObject);
    procedure actViewDependentExecute(Sender: TObject);
    procedure actViewGroupsExecute(Sender: TObject);
    procedure lstSvcInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: String);
    procedure ApplicationHint(Sender: TObject);
    procedure actViewDependentUpdate(Sender: TObject);
    procedure actViewGroupsUpdate(Sender: TObject);
  private
    FSCManager: TJclSCManager;
    {$IFDEF DELPHI5_UP}
    m_fOrderAsc: Boolean;
    {$ENDIF DELPHI5_UP}
    function GetStatusHint: string;
    procedure SetStatusHint(const Value: string);
    function GetSelected: TJclNtService;
    procedure SelectService(const Svc: TJclNtService);
  public
    procedure Refresh(const Svc: TJclNtService = nil);

    property SCManager: TJclSCManager read FSCManager;
    property Selected: TJclNtService read GetSelected;

    property StatusHint: string read GetStatusHint write SetStatusHint;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses
  ShellApi, TypInfo, NtSvcDemoDependent, NtSvcDemoGroups,
  JclSysUtils;

const
  CRLF = #13#10;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSCManager := TJclSCManager.Create;
  FSCManager.Refresh(True);

  Application.OnHint := ApplicationHint;
  {$IFDEF DELPHI5_UP}
  lstSvc.OnInfoTip := lstSvcInfoTip;
  {$ELSE DELPHI5_UP}
  lstSvc.ColumnClick := False;
  {$ENDIF DELPHI5_UP}

  Refresh;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Application.OnHint := nil;

  FreeAndNil(FSCManager);
end;

function TfrmMain.GetStatusHint: string;
begin
  Result := barStatus.SimpleText;
end;

procedure TfrmMain.SetStatusHint(const Value: string);
begin
  barStatus.SimpleText := Value;
  Application.ProcessMessages;
end;

function TfrmMain.GetSelected: TJclNtService;
begin
  Result := SCManager.Services[lstSvc.Selected.Index];
end;

procedure TfrmMain.SelectService(const Svc: TJclNtService);
var
  Item: TListItem;
begin
  if Assigned(Svc) then
  begin
    Item := lstSvc.FindData(0, Svc, True, True);
    if Assigned(Item) then
    begin
      lstSvc.Selected := Item;
      Item.MakeVisible(False);
    end;
  end;
end;

procedure TfrmMain.Refresh(const Svc: TJclNtService = nil);
begin
  if Assigned(Svc) then
    Svc.Refresh
  else
    SCManager.Refresh;

  lstSvc.Items.Count := SCManager.ServiceCount;
  lstSvc.Invalidate;
end;

procedure TfrmMain.lstSvcData(Sender: TObject; Item: TListItem);
begin
  if not Assigned(SCManager) then
    Exit;
  with Item, SCManager.Services[Item.Index] do
  begin
    Caption := ServiceName;
    Data    := SCManager.Services[Item.Index];
    SubItems.Add(DisplayName);
    SubItems.Add(GetEnumName(TypeInfo(TJclServiceState), Integer(ServiceState)));
    SubItems.Add(GetEnumName(TypeInfo(TJclServiceStartType), Integer(StartType)));
    SubItems.Add(GetEnumName(TypeInfo(TJclServiceErrorControlType), Integer(ErrorControlType)));
    SubItems.Add(IntToStr(Win32ExitCode));
    SubItems.Add(Description);
    SubItems.Add(FileName);
    SubItems.Add(Group.Name);
  end;
end;

procedure TfrmMain.actViewRefreshExecute(Sender: TObject);
begin
  Refresh;
end;

procedure TfrmMain.lstSvcColumnClick(Sender: TObject; Column: TListColumn);
const
  SortOrderMapping: array[0..8] of TJclServiceSortOrderType =
  (sotServiceName, sotDisplayName, sotServiceState,
   sotStartType, sotErrorControlType, sotWin32ExitCode,
   sotDescription, sotFileName, sotLoadOrderGroup);
var
  {$IFDEF DELPHI5_UP}
  I: Integer;
  {$ENDIF DELPHI5_UP}
  NtSvcName: string;
  NtSvc: TJclNtService;
begin
  if Assigned(lstSvc.Selected) then
    NtSvcName := Selected.ServiceName
  else
    NtSvcName := '';

  {$IFDEF DELPHI5_UP}
  if Column.Tag = Ord(True) then
    m_fOrderAsc := not m_fOrderAsc
  else
    m_fOrderAsc := True;

  for I:=0 to lstSvc.Columns.Count-1 do
    lstSvc.Columns[I].Tag := Ord(lstSvc.Columns[I] = Column);

  SCManager.Sort(SortOrderMapping[Column.Index], m_fOrderAsc);
  {$ENDIF DELPHI5_UP}

  Refresh;

  if (NtSvcName <> '') and SCManager.FindService(NtSvcName, NtSvc) then
    SelectService(NtSvc);
end;

procedure TfrmMain.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actFileConnectExecute(Sender: TObject);
var
  ComputerName: string;
begin
  if InputQuery('Browse a computer', 'Computer name:', ComputerName) and
     (CompareText(ComputerName, SCManager.MachineName) <> 0) then
  begin
    FreeAndNil(FSCManager);

    StatusHint := 'Connecting to ' + ComputerName + '...';
    FSCManager := TJclSCManager.Create(ComputerName);
    FSCManager.Refresh(True);
    StatusHint := 'Connected to ' + ComputerName;

    Refresh;
  end;
end;

procedure TfrmMain.actHelpAboutExecute(Sender: TObject);
begin
  ShellAbout(Handle, PChar(Caption),
    PChar('JEDI Code Library (JCL)' + CRLF + 'http://delphi-jedi.org/'),
    Application.Icon.Handle);
end;

procedure TfrmMain.actControlStartExecute(Sender: TObject);
begin
  Selected.Start;
  Refresh(Selected);
end;

procedure TfrmMain.actControlStopExecute(Sender: TObject);
begin
  Selected.Stop;
  Refresh(Selected);
end;

procedure TfrmMain.actControlPauseExecute(Sender: TObject);
begin
  Selected.Pause;
  Refresh(Selected);
end;

procedure TfrmMain.actControlContinueExecute(Sender: TObject);
begin
  Selected.Continue;
  Refresh(Selected);
end;

procedure TfrmMain.actControlStartUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(lstSvc.Selected) and
                             (Selected.ServiceState in [ssStopped]);
end;

procedure TfrmMain.actControlStopUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(lstSvc.Selected) and
                             (Selected.ServiceState in [ssRunning]) and
                             (caStop in Selected.ControlsAccepted);
end;

procedure TfrmMain.actControlPauseUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(lstSvc.Selected) and
                             (Selected.ServiceState in [ssRunning]) and
                             (caPauseContinue in Selected.ControlsAccepted);
end;

procedure TfrmMain.actControlContinueUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(lstSvc.Selected) and
                             (Selected.ServiceState in [ssPaused]);
end;

procedure TfrmMain.actControlDeleteExecute(Sender: TObject);
begin
  if MessageDlg(Format('Are you sure to delete the [%s] service?', [Selected.ServiceName]),
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Selected.Delete;
    SCManager.Refresh(True);
    Refresh;
  end;
end;

procedure TfrmMain.ActionItemSelected(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(lstSvc.Selected);
end;

procedure TfrmMain.actViewDependentExecute(Sender: TObject);
begin
  SelectService(TfrmDependent.Execute(Selected));
end;

procedure TfrmMain.actViewDependentUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(lstSvc.Selected) and
                             ((Selected.DependentServiceCount <> 0) or
                              (Selected.DependentGroupCount <> 0) or
                              (Selected.DependentByServiceCount <> 0));
end;

procedure TfrmMain.actViewGroupsExecute(Sender: TObject);
begin
  SelectService(TfrmServiceGroups.Execute(Selected));
end;

procedure TfrmMain.actViewGroupsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(lstSvc.Selected) and
                             (Selected.Group.Name <> '')
end;

procedure TfrmMain.lstSvcInfoTip(Sender: TObject; Item: TListItem;
  var InfoTip: String);
  function FormatServiceTypes(const SvcTypes: TJclServiceTypes): string;
  var
    AType: TJclServiceType;
  begin
    Result := '';
    for AType:=Low(TJclServiceType) to High(TJclServiceType) do
      if AType in SvcTypes then
      begin
        if Result <> '' then
          Result := Result + ', ';
        Result := Result + GetEnumName(TypeInfo(TJclServiceType), Integer(AType));
      end;
  end;
  function FormatControlsAccepted(const CtrlAccepted: TJclServiceControlAccepteds): string;
  var
    ACtrl: TJclServiceControlAccepted;
  begin
    Result := '';
    for ACtrl:=Low(TJclServiceControlAccepted) to High(TJclServiceControlAccepted) do
      if ACtrl in CtrlAccepted then
      begin
        if Result <> '' then
          Result := Result + ', ';
        Result := Result + GetEnumName(TypeInfo(TJclServiceControlAccepted), Integer(ACtrl));
      end;
  end;
begin
  with TJclNtService(Item.Data) do
    InfoTip := Format('Service Name: %s' + CRLF +
                      'Display Name: %s' + CRLF +
                      'Description: %s' + CRLF +
                      'File Name: %s' + CRLF +
                      'Service Type: %s' + CRLF +
                      'Service State: %s' + CRLF +
                      'Start Type: %s' + CRLF +
                      'Error Control: %s' + CRLF +
                      'Win32 Exit Code: [%d] %s' + CRLF +
                      'Service Group: %s' + CRLF +
                      'Controls Accepted: %s',
                      [ServiceName,
                       DisplayName,
                       Description,
                       FileName,
                       FormatServiceTypes(ServiceTypes),
                       GetEnumName(TypeInfo(TJclServiceState), Integer(ServiceState)),
                       GetEnumName(TypeInfo(TJclServiceStartType), Integer(StartType)),
                       GetEnumName(TypeInfo(TJclServiceErrorControlType), Integer(ErrorControlType)),
                       Win32ExitCode, SysErrorMessage(Win32ExitCode),
                       Group.Name,
                       FormatControlsAccepted(ControlsAccepted)]);
end;

procedure TfrmMain.ApplicationHint(Sender: TObject);
begin
  StatusHint := GetLongHint(Application.Hint);
end;

end.
