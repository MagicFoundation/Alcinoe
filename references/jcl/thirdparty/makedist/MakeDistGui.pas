unit MakeDistGui;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CheckLst,
  MakeDistMain;

type
  TMainForm = class(TForm)
    PanelFile: TPanel;
    ButtonNewDistribution: TButton;
    ButtonOpenDistribution: TButton;
    ButtonSaveDistribution: TButton;
    ButtonSaveDistributionAs: TButton;
    PanelTasks: TPanel;
    CheckListBoxTasks: TCheckListBox;
    LabelTaskList: TLabel;
    SplitterTasks: TSplitter;
    ButtonNewTask: TButton;
    ButtonDeleteTask: TButton;
    ButtonExecuteSelectedTask: TButton;
    ButtonExecuteCheckedTasks: TButton;
    PanelActions: TPanel;
    ListBoxActionList: TListBox;
    LabelActionList: TLabel;
    SplitterActions: TSplitter;
    ButtonAddActionToTask: TButton;
    PanelMiddle: TPanel;
    PanelConfiguration: TPanel;
    PanelTaskActions: TPanel;
    LabelTaskActionList: TLabel;
    ListBoxTaskActions: TListBox;
    ButtonMoveUp: TButton;
    ButtonMoveDown: TButton;
    ButtonDeleteAction: TButton;
    PanelMessages: TPanel;
    SplitterMessages: TSplitter;
    MemoMessages: TMemo;
    OpenDialogConfiguration: TOpenDialog;
    SaveDialogConfiguration: TSaveDialog;
    ButtonRenameTask: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxActionListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure ButtonAddActionToTaskClick(Sender: TObject);
    procedure ButtonNewTaskClick(Sender: TObject);
    procedure ButtonDeleteTaskClick(Sender: TObject);
    procedure ButtonExecuteSelectedTaskClick(Sender: TObject);
    procedure ButtonExecuteCheckedTasksClick(Sender: TObject);
    procedure ButtonMoveUpClick(Sender: TObject);
    procedure ButtonMoveDownClick(Sender: TObject);
    procedure ButtonDeleteActionClick(Sender: TObject);
    procedure ButtonNewDistributionClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckListBoxTasksClickCheck(Sender: TObject);
    procedure ListBoxTaskActionsClick(Sender: TObject);
    procedure ListBoxTaskActionsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure EditConfigurationChange(Sender: TObject);
    procedure ButtonOpenDistributionClick(Sender: TObject);
    procedure ButtonSaveDistributionClick(Sender: TObject);
    procedure ButtonSaveDistributionAsClick(Sender: TObject);
    procedure CheckListBoxTasksClick(Sender: TObject);
    procedure ButtonRenameTaskClick(Sender: TObject);
  private
    FConfigurationFileName: string;
    FDistActions: TDistActions;
    FDistribution: TDistribution;
    FWorkingDirectory: string;
    procedure OutputMessage(const Text: string);
    procedure RefreshConfig;
    procedure RefreshTaskActions;
    procedure RefreshTasks;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  JclFileUtils;

procedure TMainForm.ButtonAddActionToTaskClick(Sender: TObject);
var
  CurrentTask: TDistTask;
  ActionClass: TDistActionClass;
  TaskIndex, ActionIndex, NewActionIndex: Integer;
begin
  TaskIndex := CheckListBoxTasks.ItemIndex;
  ActionIndex := ListBoxActionList.ItemIndex;

  if (TaskIndex >= 0) and (ActionIndex >= 0) then
  begin
    CurrentTask := FDistribution.Tasks[TaskIndex];
    ActionClass := FDistActions.Actions[ActionIndex];
    NewActionIndex := CurrentTask.AddAction(ActionClass);
    RefreshTaskActions;
    ListBoxTaskActions.ItemIndex := NewActionIndex;
    RefreshConfig;
  end;
end;

procedure TMainForm.ButtonDeleteActionClick(Sender: TObject);
var
  TaskIndex, ActionIndex: Integer;
  Task: TDistTask;
begin
  TaskIndex := CheckListBoxTasks.ItemIndex;
  ActionIndex := ListBoxTaskActions.ItemIndex;

  if (TaskIndex >= 0) and (ActionIndex >= 0) then
  begin
    Task := FDistribution.Tasks[TaskIndex];
    Task.DeleteAction(ActionIndex);
    RefreshTaskActions;
    RefreshConfig;
  end;
end;

procedure TMainForm.ButtonDeleteTaskClick(Sender: TObject);
var
  TaskIndex: Integer;
begin
  TaskIndex := CheckListBoxTasks.ItemIndex;
  if TaskIndex >= 0 then
  begin
    FDistribution.DeleteTask(TaskIndex);
    RefreshTasks;
    RefreshTaskActions;
    RefreshConfig;
  end;
end;

procedure TMainForm.ButtonExecuteCheckedTasksClick(Sender: TObject);
begin
  FDistribution.WorkingDirectory := FWorkingDirectory;
  FDistribution.OnMessage := OutputMessage;
  MemoMessages.Clear;
  if FDistribution.ExecuteSelected then
    MemoMessages.Lines.Add('Success.')
  else
    MemoMessages.Lines.Add('Failure.');
end;

procedure TMainForm.ButtonExecuteSelectedTaskClick(Sender: TObject);
var
  TaskIndex: Integer;
begin
  TaskIndex := CheckListBoxTasks.ItemIndex;
  if TaskIndex >= 0 then
  begin
    FDistribution.WorkingDirectory := FWorkingDirectory;
    FDistribution.OnMessage := OutputMessage;
    MemoMessages.Clear;
    if FDistribution.ExecuteTask(TaskIndex) then
      MemoMessages.Lines.Add('Success.')
    else
      MemoMessages.Lines.Add('Failure.');
  end;
end;

procedure TMainForm.ButtonMoveDownClick(Sender: TObject);
var
  TaskIndex, ActionIndex: Integer;
  Task: TDistTask;
begin
  TaskIndex := CheckListBoxTasks.ItemIndex;
  ActionIndex := ListBoxTaskActions.ItemIndex;
  if (TaskIndex >= 0) and (ActionIndex >= 0) then
  begin
    Task := FDistribution.Tasks[TaskIndex];
    if Task.MoveAction(ActionIndex, ActionIndex + 1) then
    begin
      RefreshTaskActions;
      if ActionIndex < (ListBoxTaskActions.Count - 1) then
        ListBoxTaskActions.ItemIndex := ActionIndex + 1
      else
        ListBoxTaskActions.ItemIndex := ActionIndex;
      RefreshConfig;
    end;
  end;
end;

procedure TMainForm.ButtonMoveUpClick(Sender: TObject);
var
  TaskIndex, ActionIndex: Integer;
  Task: TDistTask;
begin
  TaskIndex := CheckListBoxTasks.ItemIndex;
  ActionIndex := ListBoxTaskActions.ItemIndex;
  if (TaskIndex >= 0) and (ActionIndex >= 0) then
  begin
    Task := FDistribution.Tasks[TaskIndex];
    if Task.MoveAction(ActionIndex, ActionIndex - 1) then
    begin
      RefreshTaskActions;
      if ActionIndex > 0 then
        ListBoxTaskActions.ItemIndex := ActionIndex - 1
      else
        ListBoxTaskActions.ItemIndex := ActionIndex;
      RefreshConfig;
    end;
  end;
end;

procedure TMainForm.ButtonNewDistributionClick(Sender: TObject);
begin
  FreeAndNil(FDistribution);
  FDistribution := TDistribution.Create;
  RefreshTasks;
  RefreshTaskActions;
  RefreshConfig;
end;

procedure TMainForm.ButtonNewTaskClick(Sender: TObject);
var
  TaskName: string;
  TaskIndex: Integer;
  Task: TDistTask;
begin
  TaskName := 'TaskName';
  if InputQuery('New task', 'Task name:', TaskName) then
  begin
    TaskIndex := FDistribution.AddTask;
    Task := FDistribution.Tasks[TaskIndex];
    Task.Name := TaskName;
    RefreshTasks;
    CheckListBoxTasks.ItemIndex := TaskIndex;
    RefreshTaskActions;
    RefreshConfig;
  end;
end;

procedure TMainForm.ButtonOpenDistributionClick(Sender: TObject);
begin
  if OpenDialogConfiguration.Execute then
  begin
    FConfigurationFileName := OpenDialogConfiguration.FileName;
    FreeAndNil(FDistribution);

    FDistribution := TDistribution.Create;
    FDistribution.LoadConfiguration(FConfigurationFileName);
    FWorkingDirectory := ExtractFilePath(FConfigurationFileName);

    RefreshTasks;
    RefreshTaskActions;
    RefreshConfig;
  end;
end;

procedure TMainForm.ButtonRenameTaskClick(Sender: TObject);
var
  TaskIndex: Integer;
  Task: TDistTask;
  TaskName: string;
begin
  TaskIndex := CheckListBoxTasks.ItemIndex;
  if TaskIndex >= 0 then
  begin
    Task := FDistribution.Tasks[TaskIndex];
    TaskName := Task.Name;
    if InputQuery(Application.Title, 'New name:', TaskName) then
    begin
      Task.Name := TaskName;
      RefreshTasks;
    end;
  end;
end;

procedure TMainForm.ButtonSaveDistributionAsClick(Sender: TObject);
begin
  if SaveDialogConfiguration.Execute then
  begin
    FConfigurationFileName := SaveDialogConfiguration.FileName;
    FDistribution.SaveConfiguration(FConfigurationFileName);
    FWorkingDirectory := ExtractFilePath(FConfigurationFileName);
  end;
end;

procedure TMainForm.ButtonSaveDistributionClick(Sender: TObject);
begin
  if FConfigurationFileName <> '' then
    FDistribution.SaveConfiguration(FConfigurationFileName)
  else
    ButtonSaveDistributionAsClick(ButtonSaveDistributionAs);
end;

procedure TMainForm.CheckListBoxTasksClick(Sender: TObject);
begin
  RefreshTaskActions;
  RefreshConfig;
end;

procedure TMainForm.CheckListBoxTasksClickCheck(Sender: TObject);
var
  CheckListBox: TCheckListBox;
  TaskIndex: Integer;
begin
  CheckListBox := Sender as TCheckListBox;
  TaskIndex := CheckListBox.ItemIndex;
  FDistribution.Tasks[TaskIndex].Selected := CheckListBox.Checked[TaskIndex];
end;

procedure TMainForm.EditConfigurationChange(Sender: TObject);
var
  Edit: TEdit;
  Task: TDistTask;
  Action: TDistAction;
  TaskIndex, ActionIndex, ConfigIndex: Integer;
  ActionRect: TRect;
begin
  Edit := Sender as TEdit;

  TaskIndex := CheckListBoxTasks.ItemIndex;
  ActionIndex := ListBoxTaskActions.ItemIndex;
  ConfigIndex := Edit.Tag;

  if (TaskIndex >= 0) and (ActionIndex >= 0) and (ConfigIndex >= 0) then
  begin
    Task := FDistribution.Tasks[TaskIndex];
    Action := Task.Actions[ActionIndex];

    // update configuration value
    Action.ConfigValues[ConfigIndex] := Edit.Text;

    // refresh caption of the action
    ActionRect := ListBoxTaskActions.ItemRect(ActionIndex);
    InvalidateRect(ListBoxTaskActions.Handle, @ActionRect, False);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Index, ConfigurationPos: Integer;
  ActionClass: TDistActionClass;
begin
  FDistActions := GetDistActions;

  FWorkingDirectory := GetCurrentDir;

  // fill actions
  for Index := 0 to FDistActions.ActionCount - 1 do
  begin
    ActionClass := FDistActions.Actions[Index];
    ListBoxActionList.Items.Add(ActionClass.ClassName);
  end;
  ListBoxActionList.ItemHeight := 3*ListBoxActionList.Canvas.TextHeight('Wg');

  ListBoxTaskActions.ItemHeight := 3*ListBoxTaskActions.Canvas.TextHeight('Wg');

  FDistribution := TDistribution.Create;

  // load distribution
  ConfigurationPos := ParamPos('c');
  if ConfigurationPos >= 1 then
  begin
    FConfigurationFileName := ParamValue(ConfigurationPos);
    FDistribution.LoadConfiguration(FConfigurationFileName);
    if ExtractFilePath(FConfigurationFileName) <> '' then
      FWorkingDirectory := ExtractFilePath(FConfigurationFileName);
  end;

  OpenDialogConfiguration.InitialDir := FWorkingDirectory;
  SaveDialogConfiguration.InitialDir := FWorkingDirectory;

  RefreshTasks;
  RefreshTaskActions;
  RefreshConfig;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FDistribution.Free;
end;

procedure TMainForm.ListBoxActionListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  AListBox: TListBox;
  ACanvas: TCanvas;
  ActionClass: TDistActionClass;
  TextHeight, SpaceHeight: Integer;
begin
  AListBox := Control as TListBox;
  ACanvas := AListBox.Canvas;
  ActionClass := FDistActions.Actions[Index];

  TextHeight := ACanvas.TextHeight('Hg');
  SpaceHeight := AListBox.ItemHeight - 2 * TextHeight;

  ACanvas.FillRect(Rect);
  ACanvas.Font.Style := [fsBold];
  ACanvas.TextOut(Rect.Left + SpaceHeight div 4, Rect.Top + SpaceHeight div 4, ActionClass.ClassName);
  ACanvas.Font.Style := [fsItalic];
  ACanvas.TextOut(Rect.Left + SpaceHeight div 2, Rect.Top + TextHeight + 3 * SpaceHeight div 4, ActionClass.GetDescription);
end;

procedure TMainForm.ListBoxTaskActionsClick(Sender: TObject);
begin
  RefreshConfig;
end;

procedure TMainForm.ListBoxTaskActionsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  AListBox: TListBox;
  ACanvas: TCanvas;
  Task: TDistTask;
  Action: TDistAction;
  TaskIndex, TextHeight, SpaceHeight: Integer;
begin
  AListBox := Control as TListBox;
  ACanvas := AListBox.Canvas;

  TextHeight := ACanvas.TextHeight('Hg');
  SpaceHeight := AListBox.ItemHeight - 2 * TextHeight;
  ACanvas.FillRect(Rect);

  TaskIndex := CheckListBoxTasks.ItemIndex;
  if TaskIndex >= 0 then
  begin
    Task := FDistribution.Tasks[TaskIndex];
    Action := Task.Actions[Index];

    ACanvas.Font.Style := [fsBold];
    ACanvas.TextOut(Rect.Left + SpaceHeight div 4, Rect.Top + SpaceHeight div 4,
      Format('[%d] %s', [Index + 1, Action.ClassName]));
    ACanvas.Font.Style := [fsItalic];
    ACanvas.TextOut(Rect.Left + SpaceHeight div 2, Rect.Top + TextHeight + 3 * SpaceHeight div 4, Action.Caption);
  end;
end;

procedure TMainForm.OutputMessage(const Text: string);
begin
  if (Length(Text) > 0) and (Text[1] = #0) then
    MemoMessages.Lines.Add(Copy(Text, 2, Length(Text) - 1))
  else
    MemoMessages.Lines.Add(Text);
  Application.ProcessMessages;
end;

procedure TMainForm.RefreshConfig;
var
  Task: TDistTask;
  Action: TDistAction;
  TaskIndex, ActionIndex, ConfigurationIndex, CaptionWidth, CaptionWidest, LineTop: Integer;
  AEdit: TEdit;
  ALabel: TLabel;
begin
  while PanelConfiguration.ControlCount > 0 do
    PanelConfiguration.Controls[0].Free;

  TaskIndex := CheckListBoxTasks.ItemIndex;
  ActionIndex := ListBoxTaskActions.ItemIndex;

  if (TaskIndex >= 0) and (ActionIndex >= 0) then
  begin
    Task := FDistribution.Tasks[TaskIndex];
    Action := Task.Actions[ActionIndex];

    if Action.ConfigCount > 0 then
    begin
      CaptionWidest := 0;
      for ConfigurationIndex := 0 to Action.ConfigCount - 1 do
      begin
        CaptionWidth := MainForm.Canvas.TextWidth(Action.ConfigCaptions[ConfigurationIndex]);
        if CaptionWidest < CaptionWidth then
          CaptionWidest := CaptionWidth;
      end;
      LineTop := 8;
      for ConfigurationIndex := 0 to Action.ConfigCount - 1 do
      begin
        AEdit := TEdit.Create(PanelConfiguration);
        AEdit.Parent := PanelConfiguration;
        AEdit.SetBounds(16 + CaptionWidest, LineTop, PanelConfiguration.Width - 24 - CaptionWidest, AEdit.Height);
        AEdit.Anchors := [akLeft, akTop, akRight];
        AEdit.Tag := ConfigurationIndex;
        AEdit.Text := Action.ConfigValues[ConfigurationIndex];
        AEdit.OnChange := EditConfigurationChange;

        ALabel := TLabel.Create(PanelConfiguration);
        ALabel.Parent := PanelConfiguration;
        ALabel.SetBounds(8, LineTop, CaptionWidest, ALabel.Height);
        ALabel.FocusControl := AEdit;
        ALabel.Caption := Action.ConfigCaptions[ConfigurationIndex];
        ALabel.Tag := ConfigurationIndex;

        if AEdit.Height > ALabel.Height then
          Inc(LineTop, AEdit.Height)
        else
          Inc(LineTop, ALabel.Height);
        Inc(LineTop, 8);
      end;

      PanelConfiguration.Height := LineTop;
    end
    else
      PanelConfiguration.Height := 0;
  end
  else
    PanelConfiguration.Height := 0;
end;

procedure TMainForm.RefreshTaskActions;
var
  Index: Integer;
  Task: TDistTask;
  Action: TDistAction;
begin
  ListBoxTaskActions.Clear;
  Index := CheckListBoxTasks.ItemIndex;
  if Index >= 0 then
  begin
    Task := FDistribution.Tasks[Index];
    for Index := 0 to Task.ActionCount - 1 do
    begin
      Action := Task.Actions[Index];
      ListBoxTaskActions.Items.Add(Action.Caption);
    end;
  end;
end;

procedure TMainForm.RefreshTasks;
var
  Index: Integer;
  Task: TDistTask;
begin
  CheckListBoxTasks.Clear;
  for Index := 0 to FDistribution.TaskCount - 1 do
  begin
    Task := FDistribution.Tasks[Index];
    CheckListBoxTasks.Items.Add(Task.Name);
    CheckListBoxTasks.Checked[Index] := Task.Selected;
  end;
end;

end.

