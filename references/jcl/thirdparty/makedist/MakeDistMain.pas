unit MakeDistMain;

interface

uses
  SysUtils, Classes, Contnrs, Windows,
  JclSysUtils, JclSimpleXml, JclStrings;

type
  TProgressionEvent = procedure (TaskIndex, ActionIndex: Integer; Finished, Success: Boolean) of object;

  TDistribution = class;

  TDistAction = class
  private
    FDistribution: TDistribution;
  protected
    function GetCaption: string; virtual; abstract;
    function GetConfigCount: Integer; virtual; abstract;
    function GetConfigCaption(Index: Integer): string; virtual; abstract;
    function GetConfigValue(Index: Integer): string; virtual; abstract;
    procedure SetConfigValue(Index: Integer; const Value: string); virtual; abstract;
  public
    class function GetDescription: string; virtual;

    constructor Create(ADistribution: TDistribution); virtual;

    function LoadConfiguration(ANode: TJclSimpleXMLElem): Boolean;
    function SaveConfiguration(ANode: TJclSimpleXMLElem): Boolean;

    function Execute(const AMessageHandler: TTextHandler): Boolean; virtual; abstract;

    property Caption: string read GetCaption;
    property ConfigCount: Integer read GetConfigCount;
    property ConfigCaptions[Index: Integer]: string read GetConfigCaption;
    property ConfigValues[Index: Integer]: string read GetConfigValue write SetConfigValue;
    property Distribution: TDistribution read FDistribution;
  end;

  TDistActionClass = class of TDistAction;

  TDistActions = class
  private
    FActionClasses: TList;
  protected
    function GetActionCount: Integer;
    function GetAction(Index: Integer): TDistActionClass;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterAction(AClass: TDistActionClass);
    procedure UnregisterAction(AClass: TDistActionClass);
    function FindActionClass(const AClassName: string): TDistActionClass;

    property ActionCount: Integer read GetActionCount;
    property Actions[Index: Integer]: TDistActionClass read GetAction;
  end;

  TDistTask = class
  private
    FActions: TObjectList;
    FDistribution: TDistribution;
    FName: string;
    FSelected: Boolean;
  protected
    function GetActionCount: Integer;
    function GetAction(Index: Integer): TDistAction;
  public
    constructor Create(ADistribution: TDistribution);
    destructor Destroy; override;

    function AddAction(AClass: TDistActionClass): Integer;
    function MoveAction(FromIndex, ToIndex: Integer): Boolean;
    procedure DeleteAction(Index: Integer);

    function LoadConfiguration(ANode: TJclSimpleXMLElem): Boolean;
    function SaveConfiguration(ANode: TJclSimpleXMLElem): Boolean;

    property ActionCount: Integer read GetActionCount;
    property Actions[Index: Integer]: TDistAction read GetAction;
    property Distribution: TDistribution read FDistribution;
    property Name: string read FName write FName;
    property Selected: Boolean read FSelected write FSelected;
  end;

  TDistribution = class
  private
    FLogFileName: TFileName;
    FLogAppend: Boolean;
    FOnMessage: TTextHandler;
    FOnProgress: TProgressionEvent;
    FTasks: TObjectList;
    FWorkingDirectory: string;
    FLog: TStrings;
  protected
    function GetTaskCount: Integer;
    function GetTask(Index: Integer): TDistTask;
    procedure OutputMessage(const Text: string);
    procedure SaveLog;
  public
    constructor Create;
    destructor Destroy; override;

    function AddTask: Integer;
    procedure DeleteTask(Index: Integer);

    function LoadConfiguration(const AFileName: string): Boolean; overload;
    function LoadConfiguration(ANode: TJclSimpleXMLElem): Boolean; overload;
    function SaveConfiguration(const AFileName: string): Boolean; overload;
    function SaveConfiguration(ANode: TJclSimpleXMLElem): Boolean; overload;

    function ExecuteSelected: Boolean;
    function ExecuteTask(Index: Integer): Boolean;

    property OnMessage: TTextHandler read FOnMessage write FOnMessage;
    property OnProgress: TProgressionEvent read FOnProgress write FOnProgress;
    property TaskCount: Integer read GetTaskCount;
    property Tasks[Index: Integer]: TDistTask read GetTask;
    property WorkingDirectory: string read FWorkingDirectory write FWorkingDirectory;
  end;

const
  LogClearCommand = #0'LogClean';
  LogSaveCommand = #0'LogSave ';
  LogAppendCommand = #0'LogAppend ';

// retreive a singleton object listing registered actions
function GetDistActions: TDistActions;

implementation

uses
  JclFileUtils;

var
  GlobalDistActions: TDistActions = nil;

//=== { TDistAction } ========================================================

constructor TDistAction.Create(ADistribution: TDistribution);
begin
  inherited Create;
  FDistribution := ADistribution;
  // override to customize
end;

class function TDistAction.GetDescription: string;
begin
  Result := '';
end;

function TDistAction.LoadConfiguration(ANode: TJclSimpleXMLElem): Boolean;
var
  Index: Integer;
  NodeElems: TJclSimpleXMLElems;
  SubElem: TJclSimpleXMLElem;
  ConfIndexProp, ValueProp: TJclSimpleXMLProp;
begin
  Result := True;

  NodeElems := ANode.Items;
  for Index := 0 to NodeElems.Count - 1 do
  begin
    SubElem := NodeElems.Item[Index];

    Result := AnsiSameText(SubElem.Name, 'configuration');
    if Result then
    begin
      ConfIndexProp := SubElem.Properties.ItemNamed['index'];
      ValueProp := SubElem.Properties.ItemNamed['value'];
      Result := (ConfIndexProp <> nil) and (ValueProp <> nil);

      if Result then
        ConfigValues[ConfIndexProp.IntValue] := ValueProp.Value;
    end;
    if not Result then
      Break;
  end;
end;

function TDistAction.SaveConfiguration(ANode: TJclSimpleXMLElem): Boolean;
var
  Index: Integer;
  NodeElems: TJclSimpleXMLElems;
  SubNode: TJclSimpleXMLElem;
begin
  Result := True;

  NodeElems := ANode.Items;

  for Index := 0 to ConfigCount - 1 do
  begin
    SubNode := NodeElems.Add('configuration');
    SubNode.Properties.Add('index', Index);
    SubNode.Properties.Add('caption', ConfigCaptions[Index]); // easier to be read
    SubNode.Properties.Add('value', ConfigValues[Index]);
  end;
end;

//=== { TDistActions } =======================================================

constructor TDistActions.Create;
begin
  inherited Create;
  FActionClasses := TList.Create;
end;

destructor TDistActions.Destroy;
begin
  FActionClasses.Free;
  inherited Destroy;
end;

function TDistActions.FindActionClass(const AClassName: string): TDistActionClass;
var
  Index: Integer;
  AClass: TDistActionClass;
begin
  Result := nil;
  for Index := 0 to FActionClasses.Count - 1 do
  begin
    AClass := TDistActionClass(FActionClasses.Items[Index]);
    if AnsiSameText(AClass.ClassName, AClassName) then
    begin
      Result := AClass;
      Break;
    end;
  end;
end;

function TDistActions.GetAction(Index: Integer): TDistActionClass;
begin
  Result := TDistActionClass(FActionClasses.Items[Index]);
end;

function TDistActions.GetActionCount: Integer;
begin
  Result := FActionClasses.Count;
end;

procedure TDistActions.RegisterAction(AClass: TDistActionClass);
begin
  FActionClasses.Add(AClass);
end;

procedure TDistActions.UnregisterAction(AClass: TDistActionClass);
begin
  FActionClasses.Remove(AClass);
end;

function GetDistActions: TDistActions;
begin
  if not Assigned(GlobalDistActions) then
    GlobalDistActions := TDistActions.Create;
  Result := GlobalDistActions;
end;

//=== { TDistTask } ==========================================================

function TDistTask.AddAction(AClass: TDistActionClass): Integer;
begin
  Result := FActions.Add(AClass.Create(Distribution));
end;

constructor TDistTask.Create(ADistribution: TDistribution);
begin
  inherited Create;
  FActions := TObjectList.Create(True);
  FDistribution := ADistribution;
end;

procedure TDistTask.DeleteAction(Index: Integer);
begin
  FActions.Delete(Index);
end;

destructor TDistTask.Destroy;
begin
  FActions.Free;
  inherited;
end;

function TDistTask.GetAction(Index: Integer): TDistAction;
begin
  Result := TDistAction(FActions.Items[Index]);
end;

function TDistTask.GetActionCount: Integer;
begin
  Result := FActions.Count;
end;

function TDistTask.LoadConfiguration(ANode: TJclSimpleXMLElem): Boolean;
var
  Index: Integer;
  NodeElems: TJclSimpleXMLElems;
  SubNode: TJclSimpleXMLElem;
  ActionClass: TDistActionClass;
  Action: TDistAction;
  NameProp, SelectedProp, ClassNameProp: TJclSimpleXMLProp;
begin
  Result := True;

  NameProp := ANode.Properties.ItemNamed['name'];
  if NameProp <> nil then
    FName := NameProp.Value;
  SelectedProp := ANode.Properties.ItemNamed['selected'];
  if SelectedProp <> nil then
    FSelected := SelectedProp.BoolValue;

  NodeElems := ANode.Items;

  for Index := 0 to NodeElems.Count - 1 do
  begin
    SubNode := NodeElems.Item[Index];
    Result := AnsiSameText(SubNode.Name, 'action');

    if Result then
    begin
      ClassNameProp := SubNode.Properties.ItemNamed['classname'];
      Result := ClassNameProp <> nil;

      if Result then
      begin
        ActionClass := GetDistActions.FindActionClass(ClassNameProp.Value);
        Result := ActionClass <> nil;

        if Result then
        begin
          Action := Actions[AddAction(ActionClass)];
          Result := Action.LoadConfiguration(SubNode);
        end;
      end;
    end;
    if not Result then
      Break;
  end;
end;

function TDistTask.MoveAction(FromIndex, ToIndex: Integer): Boolean;
begin
  Result := (FromIndex >= 0) and (FromIndex < FActions.Count)
        and (ToIndex >= 0) and (ToIndex < FActions.Count)
        and (FromIndex <> ToIndex);
  if Result then
    FActions.Move(FromIndex, ToIndex);
end;

function TDistTask.SaveConfiguration(ANode: TJclSimpleXMLElem): Boolean;
var
  Index: Integer;
  NodeElems: TJclSimpleXMLElems;
  SubNode: TJclSimpleXMLElem;
  Action: TDistAction;
begin
  Result := True;

  ANode.Properties.Add('name', FName);
  ANode.Properties.Add('selected', FSelected);

  NodeElems := ANode.Items;

  for Index := 0 to ActionCount - 1 do
  begin
    Action := Actions[Index];

    SubNode := NodeElems.Add('action');
    SubNode.Properties.Add('classname', Action.ClassName);
    Result := Action.SaveConfiguration(SubNode);
    if not Result then
      Break;
  end;
end;

//=== { TDistribution } ======================================================

function TDistribution.AddTask: Integer;
begin
  Result := FTasks.Add(TDistTask.Create(Self));
end;

constructor TDistribution.Create;
begin
  inherited Create;
  FTasks := TObjectList.Create(True);
  FLog := TStringList.Create;
end;

procedure TDistribution.DeleteTask(Index: Integer);
begin
  FLog.Free;
  FTasks.Delete(Index);
end;

destructor TDistribution.Destroy;
begin
  FTasks.Free;
  inherited Destroy;
end;

function TDistribution.ExecuteSelected: Boolean;
var
  TaskIndex: Integer;
begin
  Result := True;
  for TaskIndex := 0 to TaskCount - 1 do
    if Tasks[TaskIndex].Selected then
  begin
    Result := ExecuteTask(TaskIndex);
    if not Result then
      Break;
  end;
end;

function TDistribution.ExecuteTask(Index: Integer): Boolean;
var
  ActionIndex: Integer;
  CurrentTask: TDistTask;
  CurrentAction: TDistAction;
begin
  Result := True;
  CurrentTask := Tasks[Index];

  FLogFileName := '';
  FLogAppend := False;
  FLog.Clear;

  OutputMessage('### ' + TimeToStr(Now) + ' starting task: ' + CurrentTask.Name);

  if Assigned(FOnProgress) then
    FOnProgress(Index, -1, False, Result);

  for ActionIndex := 0 to CurrentTask.ActionCount - 1 do
  begin
    CurrentAction := CurrentTask.Actions[ActionIndex];

    OutputMessage('### ' + TimeToStr(Now) + ' starting action: ' + CurrentAction.Caption);
    if Assigned(FOnProgress) then
      FOnProgress(Index, ActionIndex, False, Result);

    try
      SetCurrentDir(WorkingDirectory);
      Result := CurrentAction.Execute(OutputMessage);
    except
      on E: Exception do
      begin
        OutputMessage('### ' + TimeToStr(Now) + ' action raised an exception of class ' + E.ClassName + ' with message ' + E.Message);
        Result := False;
      end;
    end;

    if Result then
      OutputMessage('### ' + TimeToStr(Now) + ' action success.')
    else
      OutputMessage('### ' + TimeToStr(Now) + ' action failure.');
    if Assigned(FOnProgress) then
      FOnProgress(Index, ActionIndex, True, Result);

    SaveLog;

    if not Result then
      Break;
  end;

  if Assigned(FOnProgress) then
    FOnProgress(Index, -1, True, Result);
end;

function TDistribution.GetTask(Index: Integer): TDistTask;
begin
  Result := TDistTask(FTasks.Items[Index]);
end;

function TDistribution.GetTaskCount: Integer;
begin
  Result := FTasks.Count;
end;

function TDistribution.LoadConfiguration(const AFileName: string): Boolean;
var
  XmlContent: TJclSimpleXML;
begin
  XmlContent := TJclSimpleXML.Create;
  try
    XmlContent.LoadFromFile(AFileName);
    Result := LoadConfiguration(XmlContent.Root);
  finally
    XmlContent.Free;
  end;
end;

function TDistribution.LoadConfiguration(ANode: TJclSimpleXMLElem): Boolean;
var
  Index: Integer;
  Task: TDistTask;
  NodeElems: TJclSimpleXMLElems;
  SubNode: TJclSimpleXMLElem;
begin
  Result := True;
  NodeElems := ANode.Items;

  for Index := 0 to NodeElems.Count - 1 do
  begin
    SubNode := NodeElems.Item[Index];
    Result := AnsiSameText(SubNode.Name, 'task');

    if Result then
    begin
      Task := Tasks[AddTask];
      Result := Task.LoadConfiguration(SubNode);
    end;

    if not Result then
      Break;
  end;
end;

procedure TDistribution.OutputMessage(const Text: string);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Text);

  if (Length(Text) > 0) and (Text[1] = #0) then
    FLog.Add(Copy(Text, 2, Length(Text) - 1))
  else
    FLog.Add(Text);

  if Copy(Text, 1, Length(LogSaveCommand)) = LogSaveCommand then
  begin
    FLogFileName := Copy(Text, Length(LogSaveCommand) + 1, Length(Text) - Length(LogSaveCommand));
    if not PathIsAbsolute(FLogFileName) then
      FLogFileName := PathGetRelativePath(GetCurrentDir, FLogFileName);
    FLogAppend := False;
  end
  else
  if Copy(Text, 1, Length(LogAppendCommand)) = LogAppendCommand then
  begin
    FLogFileName := Copy(Text, Length(LogAppendCommand) + 1, Length(Text) - Length(LogAppendCommand));
    if not PathIsAbsolute(FLogFileName) then
      FLogFileName := PathGetRelativePath(GetCurrentDir, FLogFileName);
    FLogAppend := True;
  end
  else
  if Text = LogClearCommand then
  begin
    SaveLog;
    FLog.Clear;
  end;
end;

function TDistribution.SaveConfiguration(const AFileName: string): Boolean;
var
  XmlContent: TJclSimpleXML;
begin
  XmlContent := TJclSimpleXML.Create;
  try
    Result := SaveConfiguration(XmlContent.Root);
    XmlContent.SaveToFile(AFileName);
  finally
    XmlContent.Free;
  end;
end;

function TDistribution.SaveConfiguration(ANode: TJclSimpleXMLElem): Boolean;
var
  Index: Integer;
  Task: TDistTask;
  NodeElems: TJclSimpleXMLElems;
  SubNode: TJclSimpleXMLElem;
begin
  Result := True;
  ANode.Name := 'makedist';
  ANode.Clear;
  NodeElems := ANode.Items;

  for Index := 0 to TaskCount - 1 do
  begin
    Task := Tasks[Index];
    SubNode := NodeElems.Add('task');
    Result := Task.SaveConfiguration(SubNode);

    if not Result then
      Break;
  end;
end;

procedure TDistribution.SaveLog;
var
  AStream: TFileStream;
begin
  if FLogFileName <> '' then
  begin
    if FLogAppend then
    begin
      if FileExists(FLogFileName) then
        AStream := TFileStream.Create(FLogFileName, fmOpenWrite or fmShareDenyNone)
      else
        AStream := TFileStream.Create(FLogFileName, fmCreate or fmShareDenyNone);
      try
        AStream.Seek(0, soEnd);
        FLog.SaveToStream(AStream);
      finally
        AStream.Free;
      end;
    end
    else
      FLog.SaveToFile(FLogFileName);
  end;
end;

initialization

finalization

  GlobalDistActions.Free;

end.

