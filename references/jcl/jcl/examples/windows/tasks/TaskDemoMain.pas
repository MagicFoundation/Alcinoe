unit TaskDemoMain;

{$INCLUDE jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, ExtCtrls, OleCtrls, SHDocVw,
  HTTPProd, HTTPApp;

type
  TfrmMain = class(TForm)
    barStatus: TStatusBar;
    lstTasks: TListView;
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuPopup: TPopupMenu;
    popTaskProp: TMenuItem;
    mnuTask: TMenuItem;
    mnuTaskProp: TMenuItem;
    SplitterV: TSplitter;
    WebBrowser: TWebBrowser;
    ppTaskInfo: TPageProducer;
    popTaskAdd: TMenuItem;
    popTaskDelete: TMenuItem;
    popLine0: TMenuItem;
    mnuTaskAdd: TMenuItem;
    mnuTaskDelete: TMenuItem;
    mnuTaskLine0: TMenuItem;
    mnuTaskLine2: TMenuItem;
    mnuTaskRefresh: TMenuItem;
    popLine2: TMenuItem;
    popTaskRefresh: TMenuItem;
    mnuTaskLine1: TMenuItem;
    mnuTaskRun: TMenuItem;
    mnuTaskStop: TMenuItem;
    popLine1: TMenuItem;
    popTaskRun: TMenuItem;
    popTaskStop: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure WebBrowserDocumentComplete(Sender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
    procedure lstTasksSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ppTaskInfoHTMLTag(Sender: TObject; Tag: TTag;
      const TagString: String; TagParams: TStrings;
      var ReplaceText: String);
    procedure FormDestroy(Sender: TObject);
  private
    FWebBrowserInitialized: Boolean;

    function SystemTimeToString(const SysTime: TSystemTime): string;
    function MsToStr(const MsTime: DWORD): string;

    procedure SetHtml(const wb: TWebBrowser; const Html: string);
    procedure OnRefresh(Sender: TObject);
  public
    procedure Refresh;
  end;

var
  frmMain: TfrmMain;

implementation

uses ActiveX, ComObj, TypInfo, MsHtml, TaskDemoDataModule, JclTask;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FWebBrowserInitialized := False;

  WebBrowser.Navigate('about:blank');

  Refresh;

  DM.OnRefresh := OnRefresh;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  DM.OnRefresh := nil;
end;

procedure TfrmMain.Refresh;
var
  I: Integer;
begin
  lstTasks.Clear;
  for I:=0 to DM.Task.TaskCount-1 do
  with lstTasks.Items.Add, DM.Task[I] do
  begin
    Caption := TaskName;
    Data    := DM.Task[I];
    SubItems.Add(SystemTimeToString(MostRecentRunTime));
    SubItems.Add(SystemTimeToString(NextRunTime));
    SubItems.Add(Comment);
  end;
end;

function TfrmMain.SystemTimeToString(const SysTime: TSystemTime): string;
begin
  if SysTime.wYear = 0 then
    Result := 'Never'
  else
    Result := DateTimeToStr(SystemTimeToDateTime(SysTime));
end;

function TfrmMain.MsToStr(const MsTime: DWORD): string;
var
  RealTime: TDateTime;
begin
  RealTime := MsTime / MSecsPerDay;
  Result := IntToStr(Trunc(RealTime)) + ' days ' + TimeToStr(RealTime);
end;

procedure TfrmMain.SetHtml(const wb: TWebBrowser; const Html: string);
var
  Stream: TStream;
  Adapter: TStreamAdapter;
  psi: IPersistStreamInit;
begin
  Stream := TStringStream.Create(Html);
  try
    Adapter := TStreamAdapter.Create(Stream);
    psi := wb.Document as IPersistStreamInit;
    OleCheck(psi.InitNew);
    OleCheck(psi.Load(Adapter));
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TfrmMain.OnRefresh(Sender: TObject);
begin
  Refresh;
end;

procedure TfrmMain.WebBrowserDocumentComplete(Sender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  if not FWebBrowserInitialized then
  begin
    FWebBrowserInitialized := True;

    (((pDisp as IWebBrowser2).Document as IHTMLDocument2).body as IHTMLBodyElement).scroll := 'no';
  end;
end;

procedure TfrmMain.lstTasksSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected and Assigned(Item) then
    SetHtml(WebBrowser, ppTaskInfo.Content);
end;

procedure TfrmMain.ppTaskInfoHTMLTag(Sender: TObject; Tag: TTag;
  const TagString: String; TagParams: TStrings; var ReplaceText: String);
  function TaskStatusToString(const Status: TJclScheduledTaskStatus): string;
  const
    StatusName: array[TJclScheduledTaskStatus] of string =
      ('Unknown', 'Ready', 'Running', 'Not Scheduled', 'Has Not Run');
  begin
    Result := StatusName[Status];
  end;
  function TaskFlagsToString(const Flags: TJclScheduledTaskFlags): string;
  var
    AFlag: TJclScheduledTaskFlag;
  begin
    for AFlag:=Low(TJclScheduledTaskFlag) to High(TJclScheduledTaskFlag) do
      if AFlag in Flags then
        Result := Result + GetEnumName(TypeInfo(TJclScheduledTaskFlag), Integer(AFlag)) + ' ';
    if Result = '' then
      Result := 'Empty';
  end;
  function TriggersToHtml(const Task: TJclScheduledTask): string;
  var
    I: Integer;
  begin
    for I:=0 to Task.TriggerCount-1 do
      Result := Format('%s<LI>%s</LI>', [Result, Task.Triggers[I].TriggerString]);
    Result := '<UL>' + Result + '</UL>';
  end;
begin
  with TJclScheduledTask(frmMain.lstTasks.Selected.Data) do
  try
    if CompareText(TagString, 'TaskName') = 0 then
      ReplaceText := TaskName
    else if CompareText(TagString, 'AccountName') = 0 then
      ReplaceText := AccountName
    else if CompareText(TagString, 'Comment') = 0 then
      ReplaceText := Comment
    else if CompareText(TagString, 'Creator') = 0 then
      ReplaceText := Creator
    else if CompareText(TagString, 'ErrorRetryCount') = 0 then
      ReplaceText := 'Unimplemented' // IntToStr(ErrorRetryCount)
    else if CompareText(TagString, 'ErrorRetryInterval') = 0 then
      ReplaceText := 'Unimplemented' // IntToStr(ErrorRetryInterval)
    else if CompareText(TagString, 'ExitCode') = 0 then
      ReplaceText := IntToStr(ExitCode)
    else if CompareText(TagString, 'Data') = 0 then
      ReplaceText := IntToStr(OwnerData.Size) + ' Bytes'
    else if CompareText(TagString, 'IdleMinutes') = 0 then
      ReplaceText := IntToStr(IdleMinutes) + ' Minutes'
    else if CompareText(TagString, 'DeadlineMinutes') = 0 then
      ReplaceText := IntToStr(DeadlineMinutes) + ' Minutes'
    else if CompareText(TagString, 'MostRecentRunTime') = 0 then
      ReplaceText := SystemTimeToString(MostRecentRunTime)
    else if CompareText(TagString, 'NextRunTime') = 0 then
      ReplaceText := SystemTimeToString(NextRunTime)
    else if CompareText(TagString, 'Status') = 0 then
      ReplaceText := TaskStatusToString(Status)
    else if CompareText(TagString, 'Flags') = 0 then
      ReplaceText := TaskFlagsToString(Flags)
    else if CompareText(TagString, 'ApplicationName') = 0 then
      ReplaceText := ApplicationName
    else if CompareText(TagString, 'WorkingDirectory') = 0 then
      ReplaceText := WorkingDirectory
    else if CompareText(TagString, 'MaxRunTime') = 0 then
      ReplaceText := MsToStr(MaxRunTime)
    else if CompareText(TagString, 'Parameters') = 0 then
      ReplaceText := Parameters
    else if CompareText(TagString, 'Priority') = 0 then
      ReplaceText := IntToStr(Priority)
    else if CompareText(TagString, 'TaskFlags') = 0 then
      ReplaceText := IntToHex(TaskFlags, 8)
    else if CompareText(TagString, 'Triggers') = 0 then
      ReplaceText := TriggersToHtml(TJclScheduledTask(frmMain.lstTasks.Selected.Data));
  except
    ReplaceText := 'Unknown';
  end;
end;

end.
