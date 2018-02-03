program MakeDist;

uses
  SysUtils,
  Windows,
  Forms,
  JclFileUtils,
  JclSysUtils,
  MakeDistGui in 'MakeDistGui.pas' {MainForm},
  MakeDistMain in 'MakeDistMain.pas',
  MakeDistActions in 'MakeDistActions.pas';

{$R *.res}
{$R ..\..\jcl\source\windows\JclCommCtrlAsInvoker.res}

type
  TDummyClass = class
    procedure WriteMessages(const Text: string);
  end;

procedure TDummyClass.WriteMessages(const Text: string);
begin
  if (Length(Text) > 0) and (Text[1] = #0) then
    WriteLn(Copy(Text, 2, Length(Text) - 1))
  else
    WriteLn(Text);
end;

function AttachConsole(dwProcessId: DWORD): BOOL; stdcall; external kernel32 name 'AttachConsole';

const
  ATTACH_PARENT_PROCESS = DWORD(-1);

procedure ExecuteCommandLine;
var
  ConfigurationPos, UnselectAllPos, UnselectPos, SelectPos, Index: Integer;
  Distribution: TDistribution;
  TaskList, ProjectFile: string;
begin
  try
    if not AttachConsole(ATTACH_PARENT_PROCESS) then
      AllocConsole;
    if ParamPos('q') <= 0 then
    begin
      WriteLn('Distribution maker, a file distribution helper');
      WriteLn('Copyright Florent Ouchet (c), February 2008');
      WriteLn;
    end;
    if (ParamPos('?') > 0) or (ParamPos('h') > 0) then
    begin
      WriteLn('Usage:');
      WriteLn('  MakeDist.exe [/cConfigFile] : launch the graphical GUI');
      WriteLn('  MakeDist.exe /h             : launch this help page');
      WriteLn('  MakeDist.exe /x [/s=Task1[,Task2]] [/u=Task1,[Task2]] [/a] [/q]');
      WriteLn('         /c=ConfigFile        : execute a project');
      WriteLn;
      WriteLn('Description of switches:');
      WriteLn('  /a               Unselect all tasks before considering -s');
      WriteLn('  /c=ConfigFile    Use this config file');
      WriteLn('  /h or /?         Show help');
      WriteLn('  /q               Do not show copyright');
      WriteLn('  /s=Task1[,Task2] Select these tasks');
      WriteLn('  /u=Task1[,Task2] Unselect these tasks');
      WriteLn('  /x               Execute selected tasks of a project');
      WriteLn;
      ReadLn;
    end;

    ConfigurationPos := ParamPos('c');
    UnselectAllPos := ParamPos('a');
    UnselectPos := ParamPos('u');
    SelectPos := ParamPos('s');

    Distribution := TDistribution.Create;
    try
      if ConfigurationPos <= 0 then
      begin
        WriteLn('Missing configuration file');
        Exit;
      end
      else
      begin
        ProjectFile := ParamValue(ConfigurationPos);
        Distribution.LoadConfiguration(ProjectFile);
      end;
      if UnselectAllPos > 0 then
        for Index := 0 to Distribution.TaskCount - 1 do
          Distribution.Tasks[Index].Selected := False;
      if SelectPos > 0 then
      begin
        TaskList := ParamValue(SelectPos);
        for Index := 0 to Distribution.TaskCount - 1 do
          if ListItemIndex(TaskList, ',', Distribution.Tasks[Index].Name) >= 0 then
            Distribution.Tasks[Index].Selected := True;
      end;
      if UnselectPos > 0 then
      begin
        TaskList := ParamValue(UnselectPos);
        for Index := 0 to Distribution.TaskCount - 1 do
          if ListItemIndex(TaskList, ',', Distribution.Tasks[Index].Name) >= 0 then
            Distribution.Tasks[Index].Selected := False;
      end;

      if ExtractFilePath(ProjectFile) <> '' then
        Distribution.WorkingDirectory := ExtractFilePath(ProjectFile)
      else
        Distribution.WorkingDirectory := GetCurrentDir;
      Distribution.OnMessage := TDummyClass(nil).WriteMessages;
      Distribution.ExecuteSelected;
      
    finally
      Distribution.Free;
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName + ': ' + E.Message);
  end;
end;

begin
  if (ParamPos('x') > 0) or (ParamPos('h') > 0) or (ParamPos('?') > 0) then
  begin
    ExecuteCommandLine;
    FreeConsole;
  end
  else
  begin
    Application.Initialize;
    Application.CreateForm(TMainForm, MainForm);
    Application.Run;
  end;
end.

