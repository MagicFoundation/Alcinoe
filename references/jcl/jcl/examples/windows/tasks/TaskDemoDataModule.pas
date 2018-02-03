unit TaskDemoDataModule;

interface

uses
  {$IFNDEF COMPILER6_UP}
  Forms,
  {$ENDIF}
  SysUtils, Classes, ActnList, ImgList, Controls, StdActns, JclTask;

type
  TDM = class(TDataModule)
    lstImage: TImageList;
    lstAction: TActionList;
    actFileExit: TAction;
    actTaskProp: TAction;
    actTaskAdd: TAction;
    actTaskDelete: TAction;
    actTaskRefresh: TAction;
    actTaskRun: TAction;
    actTaskStop: TAction;
    procedure actTaskPropUpdate(Sender: TObject);
    procedure actTaskPropExecute(Sender: TObject);
    procedure actTaskAddExecute(Sender: TObject);
    procedure actTaskDeleteExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure actTaskRefreshExecute(Sender: TObject);
    procedure actTaskRunExecute(Sender: TObject);
    procedure actTaskStopExecute(Sender: TObject);
    procedure actTaskStopUpdate(Sender: TObject);
    procedure actTaskRunUpdate(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
  private
    FTask: TJclTaskSchedule;
    FOnRefresh: TNotifyEvent;

    function GetSelectedTask: TJclScheduledTask;
  public
    property Task: TJclTaskSchedule read FTask;
    property SelectedTask: TJclScheduledTask read GetSelectedTask;

    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
  end;

var
  DM: TDM;

implementation

uses Windows, Dialogs, TaskDemoMain;

{$R *.dfm}

procedure TDM.DataModuleCreate(Sender: TObject);
begin
  try
    if not TJclTaskSchedule.IsRunning then
      TJclTaskSchedule.Start;
  except
    Application.HandleException(Self);
  end;

  FTask := TJclTaskSchedule.Create;
  FTask.Refresh;

  FOnRefresh := nil;
end;

procedure TDM.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FTask);
end;

procedure TDM.actTaskPropUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(frmMain.lstTasks.Selected);
end;

function TDM.GetSelectedTask: TJclScheduledTask;
begin
  Result := TJclScheduledTask(frmMain.lstTasks.Selected.Data);
end;

procedure TDM.actTaskPropExecute(Sender: TObject);
begin
  SelectedTask.ShowPage;
  SelectedTask.Save;
  SelectedTask.Refresh;
end;

procedure TDM.actTaskAddExecute(Sender: TObject);
var
  TaskName: string;
  ATask: TJclScheduledTask;
begin
  TaskName := 'unnnamed';
  if InputQuery('Please input a task name', 'Task Name', TaskName) then
  try
    ATask := Task.Add(TaskName);
    if ATask.ShowPage then
    begin
      ATask.Save;
      ATask.Refresh;
      if Assigned(FOnRefresh) then FOnRefresh(Self);
    end
    else
    begin
      Task.Remove(ATask);
    end;
  except
    on E: Exception do
      {$IFDEF COMPILER6_UP}
      ApplicationShowException(E);
      {$ELSE}
      Application.ShowException(E);
      {$ENDIF}
  end;
end;

procedure TDM.actTaskDeleteExecute(Sender: TObject);
begin
  Task.Remove(SelectedTask);
  if Assigned(FOnRefresh) then FOnRefresh(Self);
end;

procedure TDM.actTaskRefreshExecute(Sender: TObject);
begin
  FTask.Refresh;
  if Assigned(FOnRefresh) then FOnRefresh(Self);
end;

procedure TDM.actTaskRunExecute(Sender: TObject);
begin
  SelectedTask.Run;
end;

procedure TDM.actTaskStopExecute(Sender: TObject);
begin
  SelectedTask.Terminate;
end;

procedure TDM.actTaskStopUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(frmMain.lstTasks.Selected) and
                             (SelectedTask.Status = tsRunning);
end;

procedure TDM.actTaskRunUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(frmMain.lstTasks.Selected) and
                             (SelectedTask.Status <> tsRunning);
end;

procedure TDM.actFileExitExecute(Sender: TObject);
begin
  if Assigned(Application.MainForm) then
  begin
    Application.HelpCommand(HELP_QUIT, 0);
    Application.MainForm.Close;
  end;
end;

end.
