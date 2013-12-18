unit FirmOS_GUITestRunner;
interface

uses
  TestFramework,

  Windows,
  Math,
  Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, ImgList, Buttons, Menus, ActnList,
  SysUtils, Classes, IniFiles, ToolWin;

const
  {: Section of the dunit.ini file where GUI information will be stored }
  cnConfigIniSection = 'GUITestRunner Config';

  {: Color constants for the progress bar and failure details panel }
  clOK      = clGreen;
  clFAILURE = clFuchsia;
  clERROR   = clRed;

  {: Indexes of the color images used in the test tree and failure list }
  imgNONE    = 0;
  imgRUNNING = 1;
  imgRUN     = 2;
  imgFAILED  = 3;
  imgERROR   = 4;

  {: Indexes of the images used for test tree checkboxes }
  imgDISABLED        = 1;
  imgPARENT_DISABLED = 2;
  imgENABLED         = 3;

type
  {: Function type used by the TDUnitDialog.ApplyToTests method
     @param item  The ITest instance on which to act
     @return true if processing should continue, false otherwise
  }
  TTestFunc = function (item :ITest):boolean of object;

  TFirmOS_TestRunner = class(TForm, ITestListener, ITestListenerX)
    StateImages: TImageList;
    RunImages: TImageList;
    DialogActions: TActionList;
    SelectAllAction: TAction;
    DeselectAllAction: TAction;
    SelectFailedAction: TAction;
    MainMenu: TMainMenu;
    TestTreeMenu: TMenuItem;
    SelectAllItem: TMenuItem;
    DeselectAllItem: TMenuItem;
    SelectFailedItem: TMenuItem;
    FileMenu: TMenuItem;
    SaveConfigurationAction: TAction;
    AutoSaveAction: TAction;
    SaveConfigurationItem: TMenuItem;
    AutoSaveItem: TMenuItem;
    RestoreSavedAction: TAction;
    RestoreSavedConfigurationItem: TMenuItem;
    ViewMenu: TMenuItem;
    HideErrorBoxItem: TMenuItem;
    BodyPanel: TPanel;
    ErrorBoxVisibleAction: TAction;
    TopPanel: TPanel;
    TreePanel: TPanel;
    TestTree: TTreeView;
    ResultsPanel: TPanel;
    ProgressPanel: TPanel;
    ResultsView: TListView;
    FailureListView: TListView;
    ErrorBoxPanel: TPanel;
    ErrorBoxSplitter: TSplitter;
    ResultsSplitter: TSplitter;
    AutoChangeFocusItem: TMenuItem;
    TopProgressPanel: TPanel;
    ProgressBar: TProgressBar;
    pnlProgresslabel: TPanel;
    ScorePanel: TPanel;
    ScoreLabel: TPanel;
    ScoreBar: TProgressBar;
    pmTestTree: TPopupMenu;
    pmiSelectAll: TMenuItem;
    pmiDeselectAll: TMenuItem;
    pmiSelectFailed: TMenuItem;
    HideTestNodesAction: TAction;
    CollapseLowestSuiteNodesItem: TMenuItem;
    CollapseLowestSuiteNodes1: TMenuItem;
    HideTestNodesOnOpenAction: TAction;
    HideTestNodesItem: TMenuItem;
    ExpandAllNodesAction: TAction;
    TestTreeMenuSeparator: TMenuItem;
    ExpandAllItem: TMenuItem;
    TestTreeLocalMenuSeparator: TMenuItem;
    ExpandAll2: TMenuItem;
    lblTestTree: TLabel;
    RunAction: TAction;
    ExitAction: TAction;
    BreakOnFailuresAction: TAction;
    BreakonFailuresItem: TMenuItem;
    ShowTestedNodeAction: TAction;
    SelectTestedNodeItem: TMenuItem;
    ErrorMessagePopup: TPopupMenu;
    CopyFailureMessage: TMenuItem;
    CopyMessageToClipboardAction: TAction;
    ActionsMenu: TMenuItem;
    CopyMessagetoCllipboardItem: TMenuItem;
    LbProgress: TLabel;
    UseRegistryAction: TAction;
    UseRegistryItem: TMenuItem;
    ErrorMessageRTF: TRichEdit;
    SelectCurrentAction: TAction;
    DeselectCurrentAction: TAction;
    SelectCurrent1: TMenuItem;
    DeselectCurrent1: TMenuItem;
    ActionsImages: TImageList;
    CloseItem: TMenuItem;
    RunItem: TMenuItem;
    StopAction: TAction;
    StopActionItem: TMenuItem;
    ToolBar1: TToolBar;
    SelectAllButton: TToolButton;
    DeselectAllButton: TToolButton;
    ToolButton1: TToolButton;
    SelectFailedButton: TToolButton;
    ToolButton2: TToolButton;
    SelectCurrentButton: TToolButton;
    DeselectCurrentButton: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    Alt_R_RunAction: TAction;
    Alt_S_StopAction: TAction;
    N1: TMenuItem;
    DeselectCurrent2: TMenuItem;
    SelectCurrent2: TMenuItem;
    N2: TMenuItem;
    CopyProcnameToClipboardAction: TAction;
    N3: TMenuItem;
    Copytestnametoclipboard1: TMenuItem;
    N4: TMenuItem;
    Copytestnametoclipboard2: TMenuItem;
    RunSelectedTestAction: TAction;
    N5: TMenuItem;
    Runcurrenttest1: TMenuItem;
    N6: TMenuItem;
    Runcurrenttest2: TMenuItem;
    RunSelectedTestItem: TMenuItem;
    RunSelectedTestButton: TToolButton;
    GoToNextSelectedTestAction: TAction;
    GoToPrevSelectedTestAction: TAction;
    N7: TMenuItem;
    GoToNextSelectedNode1: TMenuItem;
    GoToPreviousSelectedNode1: TMenuItem;
    N8: TMenuItem;
    GoToNextSelectedNode2: TMenuItem;
    GoToPreviousSelectedNode2: TMenuItem;
    bRunContinuos: TToolButton;
    ToolButton: TToolButton;
    ToolButtonStop: TToolButton;
    procedure ToolButtonStopClick(Sender: TObject);
    procedure bRunContinuosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TestTreeClick(Sender: TObject);
    procedure FailureListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FailureListViewClick(Sender: TObject);
    procedure TestTreeKeyPress(Sender: TObject; var Key: Char);
    procedure SelectAllActionExecute(Sender: TObject);
    procedure DeselectAllActionExecute(Sender: TObject);
    procedure SelectFailedActionExecute(Sender: TObject);
    procedure SaveConfigurationActionExecute(Sender: TObject);
    procedure RestoreSavedActionExecute(Sender: TObject);
    procedure AutoSaveActionExecute(Sender: TObject);
    procedure ErrorBoxVisibleActionExecute(Sender: TObject);
    procedure ErrorBoxSplitterMoved(Sender: TObject);
    procedure ErrorBoxPanelResize(Sender: TObject);
    procedure HideTestNodesActionExecute(Sender: TObject);
    procedure HideTestNodesOnOpenActionExecute(Sender: TObject);
    procedure ExpandAllNodesActionExecute(Sender: TObject);
    procedure RunActionExecute(Sender: TObject);
    procedure ExitActionExecute(Sender: TObject);
    procedure BreakOnFailuresActionExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShowTestedNodeActionExecute(Sender: TObject);
    procedure CopyMessageToClipboardActionExecute(Sender: TObject);
    procedure UseRegistryActionExecute(Sender: TObject);
    procedure RunActionUpdate(Sender: TObject);
    procedure CopyMessageToClipboardActionUpdate(Sender: TObject);
    procedure SelectCurrentActionExecute(Sender: TObject);
    procedure DeselectCurrentActionExecute(Sender: TObject);
    procedure StopActionExecute(Sender: TObject);
    procedure StopActionUpdate(Sender: TObject);
    procedure TestTreeChange(Sender: TObject; Node: TTreeNode);
    procedure CopyProcnameToClipboardActionExecute(Sender: TObject);
    procedure CopyProcnameToClipboardActionUpdate(Sender: TObject);
    procedure RunSelectedTestActionExecute(Sender: TObject);
    procedure RunSelectedTestActionUpdate(Sender: TObject);
    procedure TestTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GoToNextSelectedTestActionExecute(Sender: TObject);
    procedure GoToPrevSelectedTestActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure ResetProgress;
  protected
    FSuite:         ITest;
    FTestResult:    TTestResult;
    FRunning:       Boolean;
    FTests:         TInterfaceList;
    FSelectedTests: TInterfaceList;

    procedure Setup;
    procedure SetUpStateImages;
    procedure SetSuite(value: ITest);
    procedure ClearResult;
    procedure DisplayFailureMessage(Item :TListItem);
    procedure ClearFailureMessage;

    function  AddFailureItem(failure: TTestFailure): TListItem;
    procedure UpdateStatus(const fullUpdate:Boolean);

    procedure FillTestTree(RootNode: TTreeNode; ATest: ITest); overload;
    procedure FillTestTree(ATest: ITest);                      overload;

    procedure UpdateNodeImage(node: TTreeNode);
    procedure UpdateNodeState(node: TTreeNode);
    procedure SetNodeState(node: TTreeNode; enabled :boolean);
    procedure SwitchNodeState(node: TTreeNode);
    procedure UpdateTestTreeState;

    procedure MakeNodeVisible(node :TTreeNode);
    procedure SetTreeNodeImage(Node :TTReeNode; imgIndex :Integer);
    procedure SelectNode(node: TTreeNode);

    function  NodeToTest(node :TTreeNode) :ITest;
    function  TestToNode(test :ITest) :TTreeNode;
    function  SelectedTest :ITest;
    procedure ListSelectedTests;

    function  EnableTest(test :ITest) : boolean;
    function  DisableTest(test :ITest) : boolean;
    procedure ApplyToTests(root :TTreeNode; const func :TTestFunc);

    procedure EnableUI(enable :Boolean);
    procedure RunTheTest(aTest: ITest);

    procedure InitTree; virtual;

    function  IniFileName :string;
    function  GetIniFile( const FileName : string ) : tCustomIniFile;

    procedure LoadRegistryAction;
    procedure SaveRegistryAction;

    procedure LoadFormPlacement;
    procedure SaveFormPlacement;

    procedure SaveConfiguration;
    procedure LoadConfiguration;

    procedure LoadSuiteConfiguration;
    procedure AutoSaveConfiguration;

    function NodeIsGrandparent(ANode: TTreeNode): boolean;
    procedure CollapseNonGrandparentNodes(RootNode: TTreeNode);

    procedure ProcessClickOnStateIcon;
    procedure ClearStatusMessage;

    procedure CopyTestNametoClipboard(ANode: TTreeNode);

    procedure SetupCustomShortcuts;

    function SelectNodeIfTestEnabled(ANode: TTreeNode): boolean;
  public
    STOP:Boolean;
    {: implement the ITestListener interface }
    procedure AddSuccess(test: ITest);
    procedure AddError(failure: TTestFailure);
    procedure AddFailure(failure: TTestFailure);
    function  ShouldRunTest(test :ITest):boolean;
    procedure StartSuite(suite: ITest); virtual;
    procedure EndSuite(suite: ITest); virtual;
    procedure StartTest(test: ITest); virtual;
    procedure EndTest(test: ITest); virtual;
    procedure TestingStarts;
    procedure TestingEnds(TestResult :TTestResult);
    procedure Status(test :ITest; const Msg :string);
    procedure Warning(test :ITest; const Msg :string);

    {: The test suite to be run in this runner }
    property Suite: ITest read FSuite write SetSuite;
    {: The result of the last test run }
    property TestResult : TTestResult read FTestResult write FTestResult;

    class procedure RunTest(test: ITest);
    class procedure RunRegisteredTests;
  end;

procedure RunTest(test: ITest);
procedure RunRegisteredTests;

procedure RunTestModeless(test: ITest);
procedure RunRegisteredTestsModeless;

implementation
uses
  Registry, Clipbrd;

{$BOOLEVAL OFF}  // Required or you'll get an AV
{$R *.dfm}

type
  TProgressBarCrack = class(TProgressBar);

procedure RunTest(test: ITest);
begin
  with TFirmOS_TestRunner.Create(nil) do
  begin
    try
      Suite := test;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure RunTestModeless(test: ITest);
var
  GUI :TFirmOS_TestRunner;
begin
  Application.CreateForm(TFirmOS_TestRunner, GUI);
  GUI.Suite := test;
  GUI.Show;
end;

procedure RunRegisteredTests;
begin
   RunTest(registeredTests)
end;

procedure RunRegisteredTestsModeless;
begin
   RunTestModeless(registeredTests)
end;

{ TGUITestRunner }

procedure TFirmOS_TestRunner.InitTree;
begin
  FTests.Clear;
  FillTestTree(Suite);
  Setup;
  if HideTestNodesOnOpenAction.Checked then
    HideTestNodesAction.Execute
  else
    ExpandAllNodesAction.Execute;
  TestTree.Selected := TestTree.Items.GetFirstNode;
end;

function TFirmOS_TestRunner.NodeToTest(Node: TTreeNode): ITest;
var
  index: Integer;
begin
  assert(assigned(Node));

  index  := Integer(Node.data);
  assert((index >= 0) and (index < FTests.Count));
  result := FTests[index] as ITest;
end;

function TFirmOS_TestRunner.TestToNode(test: ITest): TTreeNode;
begin
  assert(assigned(test));

  Result := test.GUIObject as TTreeNode;

  assert(assigned(Result));
end;

function TFirmOS_TestRunner.ShouldRunTest(test: ITest): boolean;
begin
  if FSelectedTests = nil then
    Result := test.Enabled
  else
    Result := FSelectedTests.IndexOf(test as ITest) >= 0;
end;

procedure TFirmOS_TestRunner.StartTest(test: ITest);
var
  node :TTreeNode;
begin
  assert(assigned(TestResult));
  assert(assigned(test));
  node := TestToNode(test);
  assert(assigned(node));
  SetTreeNodeImage(node, imgRunning);
  if ShowTestedNodeAction.Checked then
  begin
    MakeNodeVisible(node);
    TestTree.Update;
  end;
  ClearStatusMessage;
  UpdateStatus(False);
end;

procedure TFirmOS_TestRunner.EndTest(test: ITest);
begin
  UpdateStatus(False);
end;

procedure TFirmOS_TestRunner.TestingStarts;
begin
  UpdateStatus(True);
  TProgressBarCrack(ScoreBar).Color := clOK;
  TProgressBarCrack(ScoreBar).RecreateWnd;
end;

procedure TFirmOS_TestRunner.AddSuccess(test: ITest);
begin
  assert(assigned(test));
  SetTreeNodeImage(TestToNode(Test), imgRun);
end;

procedure TFirmOS_TestRunner.AddError(failure: TTestFailure);
var
  ListItem: TListItem;
begin
  ListItem := AddFailureItem(failure);
  ListItem.ImageIndex := imgERROR;
  TProgressBarCrack(ScoreBar).Color := clERROR;
  TProgressBarCrack(ScoreBar).RecreateWnd;

  SetTreeNodeImage(TestToNode(failure.failedTest), imgERROR);
  UpdateStatus(False);
end;

procedure TFirmOS_TestRunner.AddFailure(failure: TTestFailure);
var
  ListItem: TListItem;
begin
  ListItem := AddFailureItem(failure);
  ListItem.ImageIndex := imgFAILED;
  if TestResult.errorCount = 0 then
  begin
    TProgressBarCrack(ScoreBar).Color := clFAILURE;
    TProgressBarCrack(ScoreBar).RecreateWnd;
  end;
  SetTreeNodeImage(TestToNode(failure.failedTest), imgFAILED);
  UpdateStatus(False);
end;

function TFirmOS_TestRunner.IniFileName: string;
const
  TEST_INI_FILE = 'dunit.ini';
begin
    result := ExtractFilePath(Application.ExeName) + TEST_INI_FILE
end;

procedure TFirmOS_TestRunner.LoadFormPlacement;
begin
  with GetIniFile( IniFileName ) do
  try
    Self.SetBounds(
                   ReadInteger(cnConfigIniSection, 'Left',   Left),
                   ReadInteger(cnConfigIniSection, 'Top',    Top),
                   ReadInteger(cnConfigIniSection, 'Width',  Width),
                   ReadInteger(cnConfigIniSection, 'Height', Height)
                   );
    if ReadBool(cnConfigIniSection, 'Maximized', False ) then
      WindowState := wsMaximized;
  finally
    Free;
  end;
end;

procedure TFirmOS_TestRunner.SaveFormPlacement;
begin
  with GetIniFile(IniFileName) do
    try
      WriteBool(cnConfigIniSection, 'AutoSave', AutoSaveAction.Checked);

      if WindowState <> wsMaximized then
      begin
        WriteInteger(cnConfigIniSection, 'Left',   Left);
        WriteInteger(cnConfigIniSection, 'Top',    Top);
        WriteInteger(cnConfigIniSection, 'Width',  Width);
        WriteInteger(cnConfigIniSection, 'Height', Height );
      end;

      WriteBool(cnConfigIniSection, 'Maximized', WindowState = wsMaximized );
    finally
      Free
    end;
end;

procedure TFirmOS_TestRunner.LoadConfiguration;
var
  i :Integer;
begin
  LoadRegistryAction;
  LoadFormPlacement;
  LoadSuiteConfiguration;
  with GetIniFile(IniFileName) do
  try
    with AutoSaveAction do
      Checked := ReadBool(cnConfigIniSection, 'AutoSave', Checked);

    { center splitter location }
    with ResultsPanel do
      Height := ReadInteger(cnConfigIniSection, 'ResultsPanel.Height', Height);

    { error splitter location }
    with ErrorBoxPanel do
      Height := ReadInteger(cnConfigIniSection, 'ErrorMessage.Height', Height);
    with ErrorBoxVisibleAction do
      Checked := ReadBool(cnConfigIniSection, 'ErrorMessage.Visible', Checked);

    ErrorBoxSplitter.Visible := ErrorBoxVisibleAction.Checked;
    ErrorBoxPanel.Visible    := ErrorBoxVisibleAction.Checked;

    { failure list configuration }
    with FailureListView do begin
      for i := 0 to Columns.Count-1 do
      begin
        Columns[i].Width := Max(4, ReadInteger(cnConfigIniSection,
                                        Format('FailureList.ColumnWidth[%d]', [i]),
                                        Columns[i].Width)
                                        );
      end;
    end;

    { other options }
    HideTestNodesOnOpenAction.Checked := ReadBool(cnConfigIniSection,
      'HideTestNodesOnOpen', HideTestNodesOnOpenAction.Checked);
    BreakOnFailuresAction.Checked := ReadBool(cnConfigIniSection,
      'BreakOnFailures', BreakOnFailuresAction.Checked);

    ShowTestedNodeAction.Checked := ReadBool(cnConfigIniSection,
      'SelectTestedNode', ShowTestedNodeAction.Checked);
  finally
    Free;
  end;

  if Suite <> nil then
    UpdateTestTreeState;
end;

procedure TFirmOS_TestRunner.AutoSaveConfiguration;
begin
  if AutoSaveAction.Checked then
    SaveConfiguration;
end;

procedure TFirmOS_TestRunner.SaveConfiguration;
var
  i :Integer;
begin
  if Suite <> nil then
    Suite.SaveConfiguration(IniFileName, UseRegistryAction.Checked, True);

  SaveFormPlacement;
  SaveRegistryAction;

  with GetIniFile(IniFileName) do
  try
    { center splitter location }
    WriteInteger(cnConfigIniSection, 'ResultsPanel.Height',
      ResultsPanel.Height);

    { error box }
    WriteInteger(cnConfigIniSection, 'ErrorMessage.Height',
      ErrorBoxPanel.Height);
    WriteBool(cnConfigIniSection, 'ErrorMessage.Visible',
      ErrorBoxVisibleAction.Checked);

    { failure list configuration }
    with FailureListView do begin
      for i := 0 to Columns.Count-1 do
      begin
       WriteInteger( cnConfigIniSection,
                     Format('FailureList.ColumnWidth[%d]', [i]),
                     Columns[i].Width);
      end;
    end;

    { other options }
    WriteBool(cnConfigIniSection, 'HideTestNodesOnOpen', HideTestNodesOnOpenAction.Checked);
    WriteBool(cnConfigIniSection, 'BreakOnFailures',     BreakOnFailuresAction.Checked);
    WriteBool(cnConfigIniSection, 'SelectTestedNode',     ShowTestedNodeAction.Checked);
  finally
    Free;
  end;
end;

procedure TFirmOS_TestRunner.TestingEnds(TestResult :TTestResult);
begin
end;

procedure TFirmOS_TestRunner.UpdateNodeState(node: TTreeNode);
var
  test: ITest;
begin
  assert(assigned(node));
  test := NodeToTest(node);
  assert(assigned(test));

  UpdateNodeImage(node);

  if node.HasChildren then
  begin
    node := node.getFirstChild;
    while node <> nil do
    begin
      UpdateNodeState(node);
      node := node.getNextSibling;
    end;
  end;
end;

procedure TFirmOS_TestRunner.SetNodeState(node: TTreeNode; enabled :boolean);
var
  MostSeniorChanged :TTReeNode;
begin
   assert(node <> nil);

   // update ancestors if enabling
   NodeToTest(Node).Enabled := enabled;

   MostSeniorChanged := Node;
   if enabled then
   begin
     while Node.Parent <> nil do
     begin
       Node := Node.Parent;
       if not NodeToTest(Node).Enabled then
       begin // changed
          NodeToTest(Node).Enabled := true;
          MostSeniorChanged := Node;
          UpdateNodeImage(Node);
       end
     end;
   end;
   TestTree.Items.BeginUpdate;
   try
     UpdateNodeState(MostSeniorChanged);
   finally
     TestTree.Items.EndUpdate;
   end
end;

procedure TFirmOS_TestRunner.SwitchNodeState(node: TTreeNode);
begin
   assert(node <> nil);

   SetNodeState(node, not NodeToTest(node).enabled);
end;

procedure TFirmOS_TestRunner.UpdateTestTreeState;
var
  node :TTreeNode;
begin
  if TestTree.Items.Count > 0 then
  begin
    TestTree.Items.BeginUpdate;
    try
      node := TestTree.Items.GetFirstNode;
      while node <> nil do
      begin
        UpdateNodeState(node);
        node := node.getNextSibling;
      end
    finally
      TestTree.Items.EndUpdate;
    end;
  end;
end;

procedure TFirmOS_TestRunner.UpdateStatus(const fullUpdate:Boolean);
var
  i :Integer;
  TestNumber: Integer;

   function FormatElapsedTime(milli: Int64):string;
   var
     h,nn,ss,zzz: Cardinal;
   begin
     h := milli div 3600000;
     milli := milli mod 3600000;
     nn := milli div 60000;
     milli := milli mod 60000;
     ss := milli div 1000;
     milli := milli mod 1000;
     zzz := milli;
     Result := Format('%d:%2.2d:%2.2d.%3.3d', [h, nn, ss, zzz]);
   end;
begin
  if ResultsView.Items.Count = 0 then Exit;

  if fullUpdate then
    if Assigned(Suite) then
      ResultsView.Items[0].SubItems[0] := IntToStr(Suite.countEnabledTestCases)
    else
      ResultsView.Items[0].SubItems[0] := '';

  if TestResult <> nil then
  begin
    // Save the test number as we use it a lot
    TestNumber := TestResult.runCount;

    // Only update every 8 tests to speed things up considerably
    if fullUpdate or ((TestNumber and 7) = 0) then
    begin
      with ResultsView.Items[0] do
      begin
        SubItems[1] := IntToStr(TestNumber);
        SubItems[2] := IntToStr(TestResult.failureCount);
        SubItems[3] := IntToStr(TestResult.errorCount);
        SubItems[4] := FormatElapsedTime(TestResult.TotalTime);
      end;
      with TestResult do
      begin
        ScoreBar.Position  := TestNumber - (failureCount + errorCount);
        ProgressBar.Position := TestNumber;

        // There is a possibility for zero tests
        if (TestNumber = 0) and (Suite.CountEnabledTestCases = 0) then
          LbProgress.Caption := '100%'
        else
          LbProgress.Caption := IntToStr((100 * ScoreBar.Position) div ScoreBar.Max) + '%';
      end;

      // Allow the display to catch up and check for key strokes
      Application.ProcessMessages;
    end;
  end
  else begin
    with ResultsView.Items[0] do
    begin
      for i := 1 to 3 do
        SubItems[i] := '';
      SubItems[4] := FormatElapsedTime(SelectedTest.ElapsedTestTime);
    end;

    ResetProgress;
  end;

  if fullUpdate then
    Update;
end;

procedure TFirmOS_TestRunner.ResetProgress;
begin
  TProgressBarCrack(ScoreBar).ParentColor := True;
  TProgressBarCrack(ScoreBar).RecreateWnd;
  ScoreBar.Position := 0;
  ProgressBar.Position := 0;
  LbProgress.Caption := '';
end;

function TFirmOS_TestRunner.AddFailureItem(failure: TTestFailure): TListItem;
var
  item : TListItem;
  node : TTreeNode;
begin
  assert(assigned(failure));
  item := FailureListView.Items.Add;
  item.data := Pointer(TestToNode(failure.failedTest));
  item.Caption := failure.failedTest.Name;
  item.SubItems.Add(failure.thrownExceptionName);
  item.SubItems.Add(failure.thrownExceptionMessage);
  item.SubItems.Add( failure.LocationInfo
                     + ' ' +
                     failure.AddressInfo
                     );
  item.SubItems.Add(failure.StackTrace);

  node := testToNode(failure.failedTest);
  while node <> nil do
  begin
    node.Expand(false);
    node := node.Parent;
  end;

  Result := item;
end;

procedure TFirmOS_TestRunner.FillTestTree(RootNode: TTreeNode; ATest: ITest);
var
  TestTests: IInterfaceList;
  i:     Integer;
  index: Integer;
begin
  if ATest = nil then
    EXIT;

  RootNode := TestTree.Items.AddChild(RootNode, ATest.Name);

  index := FTests.Add(ATest);
  RootNode.data := Pointer(index);

  TestTests := ATest.Tests;
  for i := 0 to TestTests.count - 1 do
  begin
    FillTestTree(RootNode, TestTests[i] as ITest);
  end;
end;

procedure TFirmOS_TestRunner.FillTestTree(ATest: ITest);
begin
  TestTree.Items.Clear;
  FTests.Clear;
  fillTestTree(nil, Suite);
end;

procedure TFirmOS_TestRunner.SetTreeNodeImage(Node :TTReeNode; imgIndex :Integer);
begin
  while Node <> nil do
  begin
    if imgIndex > Node.ImageIndex then
    begin
       Node.ImageIndex    := imgIndex;
       Node.SelectedIndex := imgIndex;
    end;
    if imgIndex = imgRunning then
      Node := nil
    else
      Node := Node.Parent;
  end;
end;

procedure TFirmOS_TestRunner.SetSuite(value: ITest);
begin
  FSuite := value;
  LoadSuiteConfiguration;
  EnableUI(FSuite <> nil);
  if (FSuite <> nil) then InitTree;
end;

procedure TFirmOS_TestRunner.DisplayFailureMessage(Item: TListItem);
var
  hlColor :TColor;
  Test    :ITest;
  Status  :string;
begin
  TestTree.Selected := TTreeNode(Item.data);
  Test := NodeToTest(TestTree.Selected);
  hlColor := clFAILURE;
  if Item.ImageIndex >= imgERROR then
     hlColor := clERROR;
  with ErrorMessageRTF do
    begin
      Clear;
      SelAttributes.Size  := self.Font.Size;
      SelAttributes.Style := [fsBold];
      SelText := Item.Caption + ': ';

      SelAttributes.Color := hlColor;
      SelAttributes.Style := [fsBold];
      SelText := Item.SubItems[0];

      Lines.Add('');
      SelAttributes.Color := clWindowText;
      SelAttributes.Style := [];
      SelText := 'at ' + Item.SubItems[2];

      if Item.SubItems[1] <> '' then
      begin
        SelAttributes.Color := clWindowText;
        Lines.Add('');
        SelAttributes.Size  := 12;
        SelAttributes.Style := [];
        SelText := Item.SubItems[1];
        SelAttributes.Size  := self.Font.Size;
      end;

      Status := Test.Status;
      if Status <> '' then
      begin
        Lines.Add('');
        Lines.Add('');
        SelAttributes.Style := [fsBold];
        Lines.Add('Status Messages');
        SelAttributes.Style := [];
        Lines.Add(Status);
      end;

      if Item.SubItems[3] <> '' then
      begin
        Lines.Add('');
        SelAttributes.Style := [fsBold];
        Lines.Add('StackTrace');
        SelAttributes.Style := [];
        SelText := Item.SubItems[3];
      end;
    end
end;

procedure TFirmOS_TestRunner.ClearFailureMessage;
begin
  ErrorMessageRTF.Clear;
end;

procedure TFirmOS_TestRunner.ClearResult;
begin
  if FTestResult <> nil then
  begin
    FTestResult.Free;
    FTestResult := nil;
    ClearFailureMessage;
  end;
end;

procedure TFirmOS_TestRunner.Setup;
var
  i: Integer;
  node: TTreeNode;
begin
  FailureListView.Items.Clear;
  ResetProgress;
  Update;

  with ResultsView.Items[0] do
  begin
    if Suite <> nil then
    begin
      SubItems[0] := IntToStr(Suite.countEnabledTestCases);
    end
    else
    begin
      SubItems[0] := '';
    end;
    SubItems[1] := '';
    SubItems[2] := '';
    SubItems[3] := '';
    SubItems[4] := '';
  end;

  if Suite <> nil then
  begin
    ProgressBar.Max := Suite.countEnabledTestCases;
  end
  else
  begin
    ProgressBar.Max:= 10000;
  end;
  ScoreBar.Max := ProgressBar.Max;

  for i := 0 to TestTree.Items.Count - 1 do
  begin
    node := TestTree.Items[i];
    node.ImageIndex    := imgNONE;
    node.SelectedIndex := imgNONE;
  end;
  UpdateTestTreeState;
end;

procedure TFirmOS_TestRunner.EnableUI(enable: Boolean);
begin
  SelectAllAction.Enabled    := enable;
  DeselectAllAction.Enabled  := enable;
  SelectFailedAction.Enabled := enable;
  SelectCurrentAction.Enabled := enable;
  DeselectCurrentAction.Enabled := enable;
  HideTestNodesAction.Enabled   := enable;
  ExpandAllNodesAction.Enabled  := enable;
end;

procedure TFirmOS_TestRunner.FormCreate(Sender: TObject);
begin
  inherited;
  FTests := TInterfaceList.Create;
  LoadConfiguration;

  TimeSeparator := ':';
  SetUpStateImages;
  SetupCustomShortcuts;
  TestTree.Items.Clear;
  EnableUI(false);
  ClearFailureMessage;
  Setup;
end;

procedure TFirmOS_TestRunner.FormDestroy(Sender: TObject);
begin
  ClearResult;
  AutoSaveConfiguration;
  Suite := nil;
  FTests.Free;
  FTests := nil;
  inherited;
end;

procedure TFirmOS_TestRunner.FormShow(Sender: TObject);
var
  node: TTreeNode;
  test: ITest;
begin
  // Set up the GUI nodes here because the tree and all its tree nodes get
  // recreated in TCustomForm.ShowModal in D8+ so we cannot do it sooner

  node := TestTree.Items.GetFirstNode;
  while assigned(node) do
  begin
    // Get and check the test for the tree node

    test := NodeToTest(node);
    assert(Assigned(test));

    // Save the tree node in the test and get the next tree node

    test.GUIObject := node;

    node := node.GetNext;
  end;
end;

procedure TFirmOS_TestRunner.TestTreeClick(Sender: TObject);
begin
  if FRunning then
    EXIT;

  ProcessClickOnStateIcon;
  TestTreeChange(Sender, TestTree.Selected);
end;

procedure TFirmOS_TestRunner.TestTreeChange(Sender: TObject; Node: TTreeNode);
var
  i : Integer;
begin
  if (Node <> nil) and (Node = TestTree.Selected) then
  begin
    FailureListView.Selected := nil;
    for i := 0 to FailureListView.Items.count - 1 do
    begin
      if TTreeNode(FailureListView.Items[i].Data) = Node then
      begin
        FailureListView.Selected := FailureListView.Items[i];
        break;
      end;
    end;
    UpdateStatus(True);
  end;
end;

procedure TFirmOS_TestRunner.FailureListViewClick(Sender: TObject);
begin
  if FailureListView.Selected <> nil then
  begin
    TestTree.Selected := TTreeNode(FailureListView.Selected.data);
  end;
end;

procedure TFirmOS_TestRunner.FailureListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if not Selected then
    ClearFailureMessage
  else
    DisplayFailureMessage(Item);
end;

function TFirmOS_TestRunner.DisableTest(test: ITest): boolean;
begin
  test.enabled := false;
  result := true;
end;

function TFirmOS_TestRunner.EnableTest(test: ITest): boolean;
begin
  test.enabled := true;
  result := true;
end;

procedure TFirmOS_TestRunner.ApplyToTests(root :TTreeNode; const func :TTestFunc);

  procedure DoApply(root :TTreeNode);
  var
    test: ITest;
    node: TTreeNode;
  begin
    if root <> nil then
    begin
      test := NodeToTest(root);
      if func(test) then
      begin
        node := root.getFirstChild;
        while node <> nil do
        begin
          DoApply(node);
          node := node.getNextSibling;
        end;
      end;
    end;
  end;
begin
  TestTree.Items.BeginUpdate;
  try
    DoApply(root)
  finally
    TestTree.Items.EndUpdate
  end;
  UpdateTestTreeState;
end;

procedure TFirmOS_TestRunner.TestTreeKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = ' ') and (TestTree.Selected <> nil) then
  begin
    SwitchNodeState(TestTree.Selected);
    UpdateStatus(True);
    Key := #0
  end;
end;

procedure TFirmOS_TestRunner.SelectAllActionExecute(Sender: TObject);
begin
  ApplyToTests(TestTree.Items.GetFirstNode, EnableTest);
  UpdateStatus(True);
end;

procedure TFirmOS_TestRunner.DeselectAllActionExecute(Sender: TObject);
begin
  ApplyToTests(TestTree.Items.GetFirstNode, DisableTest);
  UpdateStatus(True);
end;

procedure TFirmOS_TestRunner.SelectFailedActionExecute(Sender: TObject);
var
  i: integer;
  ANode: TTreeNode;
begin
  { deselect all }
  ApplyToTests(TestTree.Items[0], DisableTest);

  { select failed }
  for i := 0 to FailureListView.Items.Count - 1 do
  begin
    ANode := TTreeNode(FailureListView.Items[i].Data);
    SetNodeState(ANode, true);
  end;
  UpdateStatus(True);
end;

procedure TFirmOS_TestRunner.SaveConfigurationActionExecute(Sender: TObject);
begin
  SaveConfiguration
end;

procedure TFirmOS_TestRunner.RestoreSavedActionExecute(Sender: TObject);
begin
  LoadConfiguration
end;

procedure TFirmOS_TestRunner.AutoSaveActionExecute(Sender: TObject);
begin
  with AutoSaveAction do
  begin
    Checked := not Checked
  end;
  AutoSaveConfiguration;
end;

procedure TFirmOS_TestRunner.ErrorBoxVisibleActionExecute(Sender: TObject);
begin
   with ErrorBoxVisibleAction do
   begin
     Checked := not Checked;
     ErrorBoxSplitter.Visible := Checked;
     ErrorBoxPanel.Visible    := Checked;
     if Checked then
     begin
      // Solve bugs with Delphi4 resizing with constraints
       ErrorBoxSplitter.Top := ErrorBoxPanel.Top-8;
     end
   end;
end;

procedure TFirmOS_TestRunner.ErrorBoxSplitterMoved(Sender: TObject);
begin
  // Solve bugs with Delphi4 resizing with constraints
  ErrorBoxSplitter.Top := ErrorBoxPanel.Top-8;
  self.Update;
end;

procedure TFirmOS_TestRunner.ErrorBoxPanelResize(Sender: TObject);
begin
  // Solve bugs with Delphi4 resizing with constraints
  ErrorBoxSplitter.Top := ErrorBoxPanel.Top-8;
end;

function TFirmOS_TestRunner.NodeIsGrandparent(ANode: TTreeNode): boolean;
var
  AChildNode: TTreeNode;
begin
  Result := false;
  if ANode.HasChildren then
  begin
    AChildNode := ANode.GetFirstChild;
    while AChildNode <> nil do
    begin
      Result := AChildNode.HasChildren or Result;
      AChildNode := ANode.GetNextChild(AChildNode);
    end;
  end;
end;

procedure TFirmOS_TestRunner.CollapseNonGrandparentNodes(RootNode: TTreeNode);
var
  AChildNode: TTreeNode;
begin
  if not NodeIsGrandparent(RootNode) then
    RootNode.Collapse(false);

  AChildNode := RootNode.GetFirstChild;
  while AChildNode <> nil do
  begin
    CollapseNonGrandparentNodes(AChildNode);
    AChildNode := RootNode.GetNextChild(AChildNode);
  end;
end;

procedure TFirmOS_TestRunner.HideTestNodesActionExecute(Sender: TObject);
var
  ANode: TTreeNode;
begin
  inherited;
  if TestTree.Items.Count = 0 then
    EXIT;

  TestTree.Items.BeginUpdate;
  try
    ANode := TestTree.Items[0];
    if ANode <> nil then
    begin
      ANode.Expand(true);
      CollapseNonGrandparentNodes(ANode);
      SelectNode(ANode);
    end;
  finally
    TestTree.Items.EndUpdate;
  end;
end;

procedure TFirmOS_TestRunner.HideTestNodesOnOpenActionExecute(Sender: TObject);
begin
  HideTestNodesOnOpenAction.Checked := not HideTestNodesOnOpenAction.Checked;
end;

procedure TFirmOS_TestRunner.ExpandAllNodesActionExecute(Sender: TObject);
begin
  TestTree.FullExpand;
  if (TestTree.Selected <> nil) then
    MakeNodeVisible(TestTree.Selected)
  else if(TestTree.Items.Count > 0) then
    TestTree.Selected := TestTree.Items[0];
end;

procedure TFirmOS_TestRunner.RunTheTest(aTest : ITest);
begin
  if aTest = nil then
    EXIT;
  if FRunning then begin
    // warning: we're reentering this method if FRunning is true
    assert(FTestResult <> nil);
    FTestResult.Stop;
    EXIT;
  end;

  FRunning := true;
  try
    RunAction.Enabled  := False;
    StopAction.Enabled := True;

    CopyMessageToClipboardAction.Enabled := false;

    EnableUI(false);
    AutoSaveConfiguration;
    ClearResult;
    TestResult := TTestResult.create;
    try
      TestResult.addListener(self);
      TestResult.BreakOnFailures := BreakOnFailuresAction.Checked;
      aTest.run(TestResult);
    finally
      TestResult.Free;
      TestResult := nil;
    end;
  finally
      FRunning := false;
      EnableUI(true);
  end;
end;

procedure TFirmOS_TestRunner.RunActionExecute(Sender: TObject);
begin
  if Suite = nil then
    EXIT;
  Setup;
  RunTheTest(Suite);
end;

procedure TFirmOS_TestRunner.ExitActionExecute(Sender: TObject);
begin
  if FTestResult <> nil then
     FTestResult.stop;
  self.ModalResult := mrCancel;
  Close;
end;

procedure TFirmOS_TestRunner.BreakOnFailuresActionExecute(Sender: TObject);
begin
  with BreakOnFailuresAction do
   Checked := not Checked;
end;

procedure TFirmOS_TestRunner.bRunContinuosClick(Sender: TObject);
begin
 Stop:=False;
 if Suite = nil then begin
  EXIT;
 end;
 repeat
  Setup;
  RunTheTest(Suite);
  Application.ProcessMessages;
  if STOP then exit;
 until false;
end;

procedure TFirmOS_TestRunner.ShowTestedNodeActionExecute(Sender: TObject);
begin
  with ShowTestedNodeAction do
    Checked := not Checked;
end;

procedure TFirmOS_TestRunner.SetUpStateImages;
begin
    TestTree.Images             := RunImages;
    TestTree.StateImages        := StateImages;
    FailureListView.SmallImages := RunImages;
end;

procedure TFirmOS_TestRunner.LoadSuiteConfiguration;
begin
  if Suite <> nil then
    Suite.LoadConfiguration(IniFileName, UseRegistryAction.Checked, True);
end;

procedure TFirmOS_TestRunner.MakeNodeVisible(node: TTreeNode);
begin
  node.MakeVisible
end;

procedure TFirmOS_TestRunner.ProcessClickOnStateIcon;
var
  HitInfo: THitTests;
  node: TTreeNode;
  Pos: TPoint;
begin
  GetCursorPos(Pos);
  Pos := TestTree.ScreenToClient(Pos);
  with Pos do
  begin
    HitInfo := TestTree.GetHitTestInfoAt(X, Y);
    node := TestTree.GetNodeAt(X, Y);
  end;
  if (node <> nil) and (HtOnStateIcon in HitInfo) then
  begin
    SwitchNodeState(node);
  end;
end;

procedure TFirmOS_TestRunner.UpdateNodeImage(node: TTreeNode);
var
  test :ITest;
begin
  test := NodeToTest(node);
  if not test.enabled then
  begin
    node.StateIndex := imgDISABLED;
  end
  else if (node.Parent <> nil)
  and (node.Parent.StateIndex <= imgPARENT_DISABLED) then
  begin
    node.StateIndex := imgPARENT_DISABLED;
  end
  else
  begin
    node.StateIndex := imgENABLED;
  end;
end;

procedure TFirmOS_TestRunner.CopyMessageToClipboardActionExecute(Sender: TObject);
begin
  ErrorMessageRTF.SelectAll;
  ErrorMessageRTF.CopyToClipboard;
end;

procedure TFirmOS_TestRunner.UseRegistryActionExecute(Sender: TObject);
begin
  with UseRegistryAction do
    Checked := not Checked;
end;

function TFirmOS_TestRunner.GetIniFile(const FileName: string) : tCustomIniFile;
begin
  if UseRegistryAction.Checked then
    Result := tRegistryIniFile.Create( GetDUnitRegistryKey + FileName )
  else
    Result := tIniFile.Create( FileName );
end;

procedure TFirmOS_TestRunner.LoadRegistryAction;
begin
  with TIniFile.Create(IniFileName) do
  try
    UseRegistryAction.Checked := ReadBool(cnConfigIniSection,
      'UseRegistry', UseRegistryAction.Checked);
  finally
    Free;
  end;
end;

procedure TFirmOS_TestRunner.SaveRegistryAction;
begin
  if UseRegistryAction.Checked then
    DeleteFile( IniFileName );

  with TIniFile.Create(IniFileName) do
  try
    WriteBool(cnConfigIniSection, 'UseRegistry', UseRegistryAction.Checked);
  finally
    Free;
  end;
end;

procedure TFirmOS_TestRunner.RunActionUpdate(Sender: TObject);
begin
  RunAction.Enabled := not FRunning and assigned( Suite ) and (Suite.countEnabledTestCases > 0);
end;

procedure TFirmOS_TestRunner.CopyMessageToClipboardActionUpdate(Sender: TObject);
begin
  CopyMessageToClipboardAction.Enabled := FailureListView.Selected <> nil;
end;

procedure TFirmOS_TestRunner.SelectCurrentActionExecute(Sender: TObject);
begin
  ApplyToTests(TestTree.Selected, EnableTest);
  SetNodeState(TestTree.Selected, true);
  UpdateStatus(True);
end;

procedure TFirmOS_TestRunner.DeselectCurrentActionExecute(Sender: TObject);
begin
  ApplyToTests(TestTree.Selected, DisableTest);
  UpdateStatus(True);
end;

procedure TFirmOS_TestRunner.StopActionExecute(Sender: TObject);
begin
  if FTestResult <> nil then
     FTestResult.stop;
end;

procedure TFirmOS_TestRunner.StopActionUpdate(Sender: TObject);
begin
  StopAction.Enabled := FRunning and (FTestResult <> nil);
end;

procedure TFirmOS_TestRunner.Status(test: ITest; const Msg: string);
begin
  if ErrorMessageRTF.Lines.Count = 0 then
    ErrorMessageRTF.Lines.Add(test.Name + ':');

  ErrorMessageRTF.Lines.Add(Msg);

  ErrorMessageRTF.Update;
end;

procedure TFirmOS_TestRunner.Warning(test: ITest; const Msg: string);
begin
  if ErrorMessageRTF.Lines.Count = 0 then
    ErrorMessageRTF.Lines.Add(test.Name + ':');

  ErrorMessageRTF.Lines.Add(Msg);

  ErrorMessageRTF.Update;
end;

procedure TFirmOS_TestRunner.ClearStatusMessage;
begin
  ErrorMessageRTF.Lines.Clear;
end;

procedure TFirmOS_TestRunner.CopyProcnameToClipboardActionExecute(
  Sender: TObject);
begin
  CopyTestNametoClipboard(TestTree.Selected);
end;

procedure TFirmOS_TestRunner.CopyTestNametoClipboard(ANode: TTreeNode);
begin
  if Assigned(ANode) then
  begin
    Clipboard.AsText := ANode.Text;
  end;
end;

procedure TFirmOS_TestRunner.CopyProcnameToClipboardActionUpdate(
  Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TestTree.Selected)
                                 and isTestMethod(NodeToTest(TestTree.Selected));
end;

function TFirmOS_TestRunner.SelectedTest: ITest;
begin
  if TestTree.Selected = nil then
    Result := nil
  else
    Result := NodeToTest(TestTree.Selected);
end;

procedure TFirmOS_TestRunner.ListSelectedTests;
var
  aTest: ITest;
  aNode: TTreeNode;
begin
  FSelectedTests.Free;
  FSelectedTests := nil;
  FSelectedTests := TInterfaceList.Create;

  aNode := TestTree.Selected;

  while Assigned(aNode) do
  begin
    aTest := NodeToTest(aNode);
    FSelectedTests.Add(aTest as ITest);
    aNode := aNode.Parent;
  end;
end;

procedure TFirmOS_TestRunner.RunSelectedTestActionExecute(Sender: TObject);
begin
  Setup;
  ListSelectedTests;
  ProgressBar.Max := 1;
  ScoreBar.Max    := 1;
  RunTheTest(Suite);
  {$IFDEF VER130}
    FreeAndNil(FSelectedTests);
  {$ELSE}
    FSelectedTests.Free;
    FSelectedTests := nil;
  {$ENDIF}
end;

procedure TFirmOS_TestRunner.RunSelectedTestActionUpdate(Sender: TObject);
var
  aTest :ITest;
begin
  ATest := SelectedTest;
  RunSelectedTestAction.Enabled := (aTest <> nil) and (aTest.CountTestCases = 1);
end;

class procedure TFirmOS_TestRunner.RunTest(test: ITest);
var
  myform: TFirmOS_TestRunner;
begin
  Application.CreateForm(TFirmOS_TestRunner, MyForm);
  with MyForm do
  begin
    try
		  suite := test;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

class procedure TFirmOS_TestRunner.RunRegisteredTests;
begin
  RunTest(RegisteredTests);
end;

procedure TFirmOS_TestRunner.EndSuite(suite: ITest);
begin
  UpdateStatus(True);
end;

procedure TFirmOS_TestRunner.StartSuite(suite: ITest);
begin
end;

procedure TFirmOS_TestRunner.TestTreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  New: TTreeNode;
begin
  { a version of this code was in the pmTestTreePopup event, but it created
    an intermittent bug. OnPopup is executed if any of the ShortCut keys
    belonging to items on the popup menu are used. This caused weird behavior,
    with the selected node suddenly changing to whatever was under the mouse
    cursor (or AV-ing if the mouse cursor wasn't over the DUnit form) when
    the user executed one of the keyboard shortcuts.

    It was intermittent most likely because the ShortCuts belonged to
    Main Menu items as well (shared from the Action.ShortCut), and the bug
    dependended on the Popup menu items receiving the ShortCut Windows message
    first.

    This code ensures that node selection occurs prior to the popup menu
    appearing when the user right-clicks on a non-selected tree node. }

  if (Button = mbRight) and (htOnItem in TestTree.GetHitTestInfoAt(X, Y)) then
  begin
    New := TestTree.GetNodeAt(X, Y);
    if TestTree.Selected <> New then
      TestTree.Selected := New;
  end;
end;

procedure TFirmOS_TestRunner.ToolButtonStopClick(Sender: TObject);
begin
 STOP:=true;
end;

procedure TFirmOS_TestRunner.GoToNextSelectedTestActionExecute(
  Sender: TObject);
var
  aNode: TTreeNode;
begin
  if TestTree.Selected <> nil then
  begin
    aNode := TestTree.Selected.GetNext;
    while aNode <> nil do
    begin
      if SelectNodeIfTestEnabled(aNode) then
        break
      else
        aNode := aNode.GetNext;
    end;
  end;
end;

function TFirmOS_TestRunner.SelectNodeIfTestEnabled(ANode: TTreeNode): boolean;
var
  ATest: ITest;
begin
  ATest := NodeToTest(ANode);
  if (ATest.Enabled) and (IsTestMethod(ATest)) then
  begin
    Result := true;
    SelectNode(ANode);
  end
  else
    Result := false;
end;

procedure TFirmOS_TestRunner.GoToPrevSelectedTestActionExecute(
  Sender: TObject);
var
  aNode: TTreeNode;
begin
  if TestTree.Selected <> nil then
  begin
    aNode := TestTree.Selected.GetPrev;
    while aNode <> nil do
    begin
      if SelectNodeIfTestEnabled(aNode) then
        break
      else
        aNode := aNode.GetPrev;
    end;
  end;
end;

procedure TFirmOS_TestRunner.SelectNode(node: TTreeNode);
begin
  node.Selected := true;
  MakeNodeVisible(node);
end;

procedure TFirmOS_TestRunner.SetupCustomShortcuts;
begin
  { the following shortcuts are not offered as an option in the
    form designer, but can be setup here }
  GoToNextSelectedTestAction.ShortCut := ShortCut(VK_RIGHT, [ssCtrl]);
  GoToPrevSelectedTestAction.ShortCut := ShortCut(VK_LEFT, [ssCtrl]);
end;

end.

