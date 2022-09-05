{ $Id: QGUITestRunner.pas,v 1.15 2005/03/05 21:32:48 judc Exp $ }
{: DUnit: An XTreme testing framework for Delphi programs.
  @author  The DUnit Group.
  @version $Revision: 1.15 $ 2001/03/08 uberto
}
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo A±ez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2003.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Kenneth Semeijn <dunit@designtime.demon.nl>
 * Uberto Barbini <uberto@usa.net>
 * Brett Shearer <BrettShearer@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)
unit QGUITestRunner;

interface

uses
  TestFramework, Math,
  Types, QForms, QGraphics, QMenus, QTypes, QActnList, QImgList,
  QStdCtrls, QComCtrls, QControls, QExtCtrls,
  SysUtils, Classes, IniFiles;

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
  imgRangeEnabled         = 0;
  imgRangeDisabled        = 1;
  imgRangeParentDisabled  = 2;

type
  {: Function type used by the TDUnitDialog.ApplyToTests method
    @param item  The ITest instance on which to act
    @return true if processing should continue, false otherwise
  }
  TTestFunc = function (item :ITest):boolean of object;

  TGUITestRunner = class(TForm, ITestListener, ITestListenerX)
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
   HideTestNodesAction: TAction;
   CollapseLowestSuiteNodesItem: TMenuItem;
   HideTestNodesOnOpenAction: TAction;
   HideTestNodesItem: TMenuItem;
   ExpandAllNodesAction: TAction;
   TestTreeMenuSeparator: TMenuItem;
   ExpandAllItem: TMenuItem;
   lblTestTree: TLabel;
   RunAction: TAction;
   ExitAction: TAction;
   BreakOnFailuresAction: TAction;
   BreakonFailuresItem: TMenuItem;
   ShowTestedNodeAction: TAction;
   SelectTestedNodeItem: TMenuItem;
   pmErrorMessage: TPopupMenu;
   CopyFailureMessage: TMenuItem;
   CopyMessageToClipboardAction: TAction;
   ActionsMenu: TMenuItem;
   CopyMessagetoCllipboardItem: TMenuItem;
   LbProgress: TLabel;
   UseRegistryAction: TAction;
   UseRegistryItem: TMenuItem;
   SelectCurrentAction: TAction;
   DeselectCurrentAction: TAction;
   SelectCurrent1: TMenuItem;
   DeselectCurrent1: TMenuItem;
   ActionsImages: TImageList;
   CloseItem: TMenuItem;
   Run1: TMenuItem;
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
    FailureTitlePanel: TPanel;
    TestNameLabel: TLabel;
    ErrorTypeLabel: TLabel;
    MessageLabel: TLabel;
    RunSelectedTestAction: TAction;
    Runselectedtest1: TMenuItem;
    RunSelectedTestToolButton: TToolButton;
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
   procedure UseRegistryActionExecute(Sender: TObject);
   procedure RunActionUpdate(Sender: TObject);
   procedure CopyMessageToClipboardActionUpdate(Sender: TObject);
   procedure SelectCurrentActionExecute(Sender: TObject);
   procedure DeselectCurrentActionExecute(Sender: TObject);
   procedure StopActionExecute(Sender: TObject);
   procedure StopActionUpdate(Sender: TObject);
   procedure TestTreeChange(Sender: TObject; Node: TTreeNode);
    procedure RunSelectedTestActionExecute(Sender: TObject);
    procedure RunSelectedTestActionUpdate(Sender: TObject);
  private
   procedure ResetProgress;
  protected
    FSuite:       ITest;
    FTestResult:  TTestResult;
    FRunning:     Boolean;
    FTests:       TInterfaceList;
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
  public
   {: implement the ITestListener interface }
    procedure AddSuccess(test: ITest);
    procedure AddError(failure: TTestFailure);
    procedure AddFailure(failure: TTestFailure);
    function  ShouldRunTest(test :ITest):boolean;
    procedure StartSuite(suite: ITest); virtual;
    procedure EndSuite(suite: ITest); virtual;
    procedure StartTest(test: ITest);
    procedure EndTest(test: ITest);
    procedure TestingStarts;
    procedure TestingEnds(testResult :TTestResult);
    procedure Status(test :ITest; const Msg :string);
    procedure Warning(test :ITest; const Msg :string);

    class procedure RunTest(test: ITest);
    class procedure RunTestModeless(test: ITest);
    class procedure RunRegisteredTests;
    class procedure RunRegisteredTestsModeless;

   {: The test suite to be run in this runner }
   property Suite: ITest read FSuite write SetSuite;
   {: The result of the last test run }
   property TestResult : TTestResult read FTestResult write FTestResult;
  end;

procedure RunTest(test: ITest);
procedure RunRegisteredTests;

procedure RunTestModeless(test: ITest);
procedure RunRegisteredTestsModeless;

implementation

{$R *.xfm}

type
  TProgressBarCrack = class(TProgressBar);

const
  RUN_IMAGES_COUNT = 5;

function NodeRunImageIndex(node: TTreeNode): integer;
begin
  Result := node.ImageIndex mod RUN_IMAGES_COUNT;
end;

function NodeStateImageIndex(node: TTreeNode): integer;
begin
  Result := node.ImageIndex div RUN_IMAGES_COUNT;
end;

function CalcImageIndex(StateImageIndex, RunImageIndex: integer): integer;
begin
  Result := StateImageIndex * RUN_IMAGES_COUNT + RunImageIndex;
end;

procedure RunTest(test: ITest);
begin
  TGUITestRunner.RunTest(test);
end;

procedure RunRegisteredTests;
begin
  TGUITestRunner.RunTest(registeredTests)
end;

procedure RunTestModeless(test: ITest);
begin
  TGUITestRunner.RunTestModeless(test);
end;

procedure RunRegisteredTestsModeless;
begin
  TGUITestRunner.RunRegisteredTestsModeless;
end;

{ TGUITestRunner }
class procedure TGUITestRunner.RunTest(test: ITest);
begin
  with TGUITestRunner.Create(nil) do
  begin
   try
    suite := test;
    ShowModal;
   finally
    Free;
   end;
  end;
end;

class procedure TGUITestRunner.RunTestModeless(test: ITest);
var
  GUI :TGUITestRunner;
begin
  Application.CreateForm(TGUITestRunner, GUI);
  GUI.Suite := test;
  GUI.Show;
end;

class procedure TGUITestRunner.RunRegisteredTests;
begin
  RunTest(registeredTests);
end;

class procedure TGUITestRunner.RunRegisteredTestsModeless;
begin
  TGUITestRunner.RunTestModeless(registeredTests);
end;

procedure TGUITestRunner.EndSuite(suite: ITest);
begin
  UpdateStatus(True);
end;

procedure TGUITestRunner.StartSuite(suite: ITest);
begin
end;

procedure TGUITestRunner.InitTree;
begin
  FTests.Clear;
  FillTestTree(FSuite);
    Setup;
    if HideTestNodesOnOpenAction.Checked then
      HideTestNodesAction.Execute
    else
      ExpandAllNodesAction.Execute;
    TestTree.Selected := TestTree.Items.GetFirstNode;
  end;

function TGUITestRunner.NodeToTest(Node: TTreeNode): ITest;
var
  index: Integer;
begin
  assert(assigned(Node));

  index  := Integer(Node.data);
  assert((index >= 0) and (index < FTests.Count));
  result := FTests[index] as ITest;
end;

function TGUITestRunner.TestToNode(test: ITest): TTreeNode;
begin
  assert(assigned(test));

  Result := TTreeNode(test.GUIObject);

  assert(assigned(Result));
end;

function TGUITestRunner.SelectedTest: ITest;
begin
  if TestTree.Selected = nil then
    Result := nil
  else
    Result := NodeToTest(TestTree.Selected);
end;

procedure TGUITestRunner.ListSelectedTests;
var
  aTest: ITest;
  aNode: TTreeNode;
begin
  FreeAndNil(FSelectedTests);
  FSelectedTests := TInterfaceList.Create;

  aNode := TestTree.Selected;

  while Assigned(aNode) do
  begin
    aTest := NodeToTest(aNode);
    FSelectedTests.Add(aTest as ITest);
    aNode := aNode.Parent;
  end;
end;

function TGUITestRunner.ShouldRunTest(test: ITest): boolean;
begin
  if FSelectedTests = nil then
    Result := test.Enabled
  else
    Result := FSelectedTests.IndexOf(test as ITest) >= 0;
end;

procedure TGUITestRunner.StartTest(test: ITest);
var
  node :TTreeNode;
begin
  assert(assigned(testResult));
  assert(assigned(test));
  node := TestToNode(test);
  assert(assigned(node));
  SetTreeNodeImage(node, imgRunning);
  if ShowTestedNodeAction.Checked then
  begin
   MakeNodeVisible(node);
   TestTree.Update;
  end;
  UpdateStatus(False);
end;

procedure TGUITestRunner.EndTest(test: ITest);
begin
  UpdateStatus(False);
end;

procedure TGUITestRunner.TestingStarts;
begin
  UpdateStatus(True);
  TProgressBarCrack(ScoreBar).Color := clOK;
end;

procedure TGUITestRunner.AddSuccess(test: ITest);
begin
  assert(assigned(test));
  SetTreeNodeImage(TestToNode(Test), imgRun);
end;

procedure TGUITestRunner.AddError(failure: TTestFailure);
var
  ListItem: TListItem;
begin
  ListItem := AddFailureItem(failure);
  ListItem.ImageIndex := imgERROR;
  TProgressBarCrack(ScoreBar).Color := clERROR;

  SetTreeNodeImage(TestToNode(failure.failedTest), imgERROR);
  UpdateStatus(False);
end;

procedure TGUITestRunner.AddFailure(failure: TTestFailure);
var
  ListItem: TListItem;
begin
  ListItem := AddFailureItem(failure);
  ListItem.ImageIndex := imgFAILED;
  if testResult.errorCount = 0 then
  begin
    TProgressBarCrack(ScoreBar).Color := clFAILURE;
  end;
  SetTreeNodeImage(TestToNode(failure.failedTest), imgFAILED);
  UpdateStatus(False);
end;

function TGUITestRunner.IniFileName: string;
const
  TEST_INI_FILE = 'dunit.ini';
begin
   result := ExtractFilePath(ParamStr(0)) + TEST_INI_FILE
end;

procedure TGUITestRunner.LoadFormPlacement;
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

procedure TGUITestRunner.SaveFormPlacement;
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
  WriteBool(cnConfigIniSection, 'Maximized', WindowState = wsMaximized );
      end;
  finally
    Free
  end;
end;

procedure TGUITestRunner.LoadConfiguration;
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
    with FailureListView do
    begin
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

  if FSuite <> nil then
    UpdateTestTreeState;
end;

procedure TGUITestRunner.AutoSaveConfiguration;
begin
  if AutoSaveAction.Checked then
    SaveConfiguration;
end;

procedure TGUITestRunner.SaveConfiguration;
var
  i :Integer;
begin
  if FSuite <> nil then
    FSuite.SaveConfiguration(IniFileName, UseRegistryAction.Checked, True);

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

    UpdateFile;
  finally
   Free;
  end;
end;

procedure TGUITestRunner.TestingEnds(testResult :TTestResult);
begin
end;

procedure TGUITestRunner.UpdateNodeState(node: TTreeNode);
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

procedure TGUITestRunner.SetNodeState(node: TTreeNode; enabled :boolean);
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

procedure TGUITestRunner.SwitchNodeState(node: TTreeNode);
begin
  assert(node <> nil);

  SetNodeState(node, not NodeToTest(node).enabled);
end;

procedure TGUITestRunner.UpdateTestTreeState;
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

procedure TGUITestRunner.UpdateStatus(const fullUpdate:Boolean);
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
  if fullUpdate then
   if FSuite <> nil then
    ResultsView.Items[0].SubItems[0] := IntToStr(FSuite.countEnabledTestCases)
   else
    ResultsView.Items[0].SubItems[0] := '';

  if testResult <> nil then
  begin
   // Save the test number as we use it a lot
   TestNumber := TestResult.runCount;

   // Only update every 8 tests to speed things up considerably
   if fullUpdate or ((TestNumber and 7) = 0) then
   begin
    with ResultsView.Items[0] do
    begin
     SubItems[1] := IntToStr(TestNumber);
     SubItems[2] := IntToStr(testResult.failureCount);
     SubItems[3] := IntToStr(testResult.errorCount);
     SubItems[4] := FormatElapsedTime(testResult.TotalTime);
    end;
    with testResult do
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
  else
  begin
   with ResultsView.Items[0] do
   begin
    for i := 1 to 4 do
      SubItems[i] := ''
   end;
   ResetProgress;
  end;

  if fullUpdate then
    Update;
end;

procedure TGUITestRunner.ResetProgress;
begin
  TProgressBarCrack(ScoreBar).ParentColor := True;
  ScoreBar.Position := 0;
  ProgressBar.Position := 0;
  LbProgress.Caption := '';
end;

function TGUITestRunner.AddFailureItem(failure: TTestFailure): TListItem;
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
  item.SubItems.Add( PointerToLocationInfo(failure.ThrownExceptionAddress)
              + ' ' +
              PointerToAddressInfo(failure.ThrownExceptionAddress)
              );
              
  node := testToNode(failure.failedTest);
  while node <> nil do
  begin
   node.Expand(false);
   node := node.Parent;
  end;

  Result := item;
end;

procedure TGUITestRunner.FillTestTree(RootNode: TTreeNode; ATest: ITest);
var
  Tests: IInterfaceList;
  i:     Integer;
  index: Integer;
begin
  if ATest = nil then
   EXIT;

  RootNode := TestTree.Items.AddChild(RootNode, ATest.Name);

  index := FTests.Add(ATest);
  RootNode.data := Pointer(index);

  ATest.GUIObject := RootNode;

  Tests := ATest.Tests;
  for i := 0 to Tests.count - 1 do
  begin
   FillTestTree(RootNode, Tests[i] as ITest);
  end;
end;

procedure TGUITestRunner.FillTestTree(ATest: ITest);
begin
  TestTree.Items.Clear;
  FTests.Clear;
  fillTestTree(nil, FSuite);
end;

procedure TGUITestRunner.SetTreeNodeImage(Node :TTReeNode; imgIndex :Integer);
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

procedure TGUITestRunner.SetSuite(value: ITest);
begin
  FSuite := value;
  LoadSuiteConfiguration;
  EnableUI(FSuite <> nil);
  if (FSuite <> nil) then InitTree;
end;

procedure TGUITestRunner.DisplayFailureMessage(Item: TListItem);
begin
//  the followinf line has been moved
//  TestTree.Selected := TTreeNode(Item.data);

  TestNameLabel.Caption  := Item.Caption + ':  ';
  ErrorTypeLabel.Caption := Item.SubItems[0] + ' at ' + Item.SubItems[2];
  MessageLabel.Caption   := Item.SubItems[1];
  if Item.ImageIndex >= imgERROR then
     ErrorTypeLabel.Font.Color := clERROR
  else
     ErrorTypeLabel.Font.Color := clFAILURE;

end;

procedure TGUITestRunner.ClearFailureMessage;
begin
  if assigned(ErrorBoxPanel) then
  begin
  TestNameLabel.Caption  := '';
  ErrorTypeLabel.Caption := '';
  MessageLabel.Caption   := '';
  end;
end;

procedure TGUITestRunner.ClearResult;
begin
  if FTestResult <> nil then
  begin
   FTestResult.Free;
   FTestResult := nil;
   ClearFailureMessage;
  end;
end;

procedure TGUITestRunner.Setup;
var
  i: Integer;
  node: TTreeNode;
begin
  FailureListView.Items.Clear;
  ResetProgress;
  Update;

  with ResultsView.Items[0] do
  begin
   if FSuite <> nil then
   begin
    SubItems[0] := IntToStr(FSuite.countEnabledTestCases);
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

  if FSuite <> nil then
  begin
   ProgressBar.Max := FSuite.countEnabledTestCases;
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

procedure TGUITestRunner.EnableUI(enable: Boolean);
begin
  SelectAllAction.Enabled    := enable;
  DeselectAllAction.Enabled  := enable;
  SelectFailedAction.Enabled := enable;
  SelectCurrentAction.Enabled := enable;
  DeselectCurrentAction.Enabled := enable;
  HideTestNodesAction.Enabled   := enable;
  ExpandAllNodesAction.Enabled  := enable;
end;

procedure TGUITestRunner.RunTheTest(aTest : ITest);
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
      testResult.addListener(self);
      testResult.BreakOnFailures := BreakOnFailuresAction.Checked;
      aTest.run(testResult);
    finally
      testResult.Free;
      testResult := nil;
    end;
  finally
      FRunning := false;
      EnableUI(true);
  end;
end;

procedure TGUITestRunner.FormCreate(Sender: TObject);
begin
  inherited;
  FTests := TInterfaceList.Create;
  LoadConfiguration;

  TimeSeparator := ':';
  SetUpStateImages;
  TestTree.Items.Clear;
  EnableUI(false);
  ClearFailureMessage;
  Setup;
end;

procedure TGUITestRunner.FormDestroy(Sender: TObject);
begin
  ClearResult;
  AutoSaveConfiguration;
  Suite := nil;
  FTests.Free;
  inherited;
end;

procedure TGUITestRunner.TestTreeClick(Sender: TObject);
begin
  if FRunning then
    Exit;

  ProcessClickOnStateIcon;
  TestTreeChange(Sender, TestTree.Selected);
end;

procedure TGUITestRunner.TestTreeChange(Sender: TObject; Node: TTreeNode);
var
  i : Integer;
  SelectedFailure: TListItem;
begin
  if (Node <> nil) and (Node = TestTree.Selected) then
  begin
    SelectedFailure := nil;
    for i := 0 to FailureListView.Items.count - 1 do
    begin
      if TTreeNode(FailureListView.Items[i].Data) = Node then
      begin
        SelectedFailure := FailureListView.Items[i];
        break;
      end;
    end;
    FailureListView.Selected := SelectedFailure;
{$IF RTLVersion < 14.1}
  // workaround for a bug in early CLX
  // not allowing to set SelectedItem property to nil
  if not Assigned(SelectedFailure) then
    ClearFailureMessage
  else
    DisplayFailureMessage(SelectedFailure);
{$IFEND}
    UpdateStatus(True);
  end;
end;

procedure TGUITestRunner.FailureListViewClick(Sender: TObject);
begin
  if FailureListView.Selected <> nil then
  begin
    TestTree.Selected := TTreeNode(FailureListView.Selected.data);
  end;
end;

procedure TGUITestRunner.FailureListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if not Selected then
    ClearFailureMessage
  else
    DisplayFailureMessage(Item);
end;

function TGUITestRunner.DisableTest(test: ITest): boolean;
begin
  test.enabled := false;
  result := true;
end;

function TGUITestRunner.EnableTest(test: ITest): boolean;
begin
  test.enabled := true;
  result := true;
end;

procedure TGUITestRunner.ApplyToTests(root :TTreeNode; const func :TTestFunc);

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

procedure TGUITestRunner.TestTreeKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = ' ') and (TestTree.Selected <> nil) then
  begin
   SwitchNodeState(TestTree.Selected);
   UpdateStatus(True);
   Key := #0
  end;
end;

procedure TGUITestRunner.SelectAllActionExecute(Sender: TObject);
begin
  ApplyToTests(TestTree.Items.GetFirstNode, EnableTest);
  UpdateStatus(True);
end;

procedure TGUITestRunner.DeselectAllActionExecute(Sender: TObject);
begin
  ApplyToTests(TestTree.Items.GetFirstNode, DisableTest);
  UpdateStatus(True);
end;

procedure TGUITestRunner.SelectFailedActionExecute(Sender: TObject);
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

procedure TGUITestRunner.SaveConfigurationActionExecute(Sender: TObject);
begin
  SaveConfiguration
end;

procedure TGUITestRunner.RestoreSavedActionExecute(Sender: TObject);
begin
  LoadConfiguration
end;

procedure TGUITestRunner.AutoSaveActionExecute(Sender: TObject);
begin
  with AutoSaveAction do
  begin
   Checked := not Checked
  end;
  AutoSaveConfiguration;
end;

procedure TGUITestRunner.ErrorBoxVisibleActionExecute(Sender: TObject);
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

procedure TGUITestRunner.ErrorBoxSplitterMoved(Sender: TObject);
begin
  // Solve bugs with Delphi4 resizing with constraints
  ErrorBoxSplitter.Top := ErrorBoxPanel.Top-8;
  self.Update;
end;

procedure TGUITestRunner.ErrorBoxPanelResize(Sender: TObject);
begin
  // Solve bugs with Delphi4 resizing with constraints
  ErrorBoxSplitter.Top := ErrorBoxPanel.Top-8;
end;

function TGUITestRunner.NodeIsGrandparent(ANode: TTreeNode): boolean;
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

procedure TGUITestRunner.CollapseNonGrandparentNodes(RootNode: TTreeNode);
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

procedure TGUITestRunner.HideTestNodesActionExecute(Sender: TObject);
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
    ANode.Selected := true;
    MakeNodeVisible(ANode);
   end;
  finally
   TestTree.Items.EndUpdate;
  end;
end;

procedure TGUITestRunner.HideTestNodesOnOpenActionExecute(Sender: TObject);
begin
  HideTestNodesOnOpenAction.Checked := not HideTestNodesOnOpenAction.Checked;
end;

procedure TGUITestRunner.ExpandAllNodesActionExecute(Sender: TObject);
begin
  TestTree.FullExpand;
  if (TestTree.Selected <> nil) then
   MakeNodeVisible(TestTree.Selected)
  else if(TestTree.Items.Count > 0) then
   TestTree.Selected := TestTree.Items[0];
end;

procedure TGUITestRunner.RunActionExecute(Sender: TObject);
begin
  if FSuite = nil then
   EXIT;
  if FRunning then begin
   // warning: we're reentering this method if FRunning is true
   assert(FTestResult <> nil);
   FTestResult.Stop;
   EXIT;
  end;
  RunAction.Enabled  := False;
  StopAction.Enabled := True;
  
  CopyMessageToClipboardAction.Enabled := false;

  EnableUI(false);
  FRunning := true;
  try
   Setup;
   AutoSaveConfiguration;
   ClearResult;
   TestResult := TTestResult.create;
   try
    testResult.addListener(self);
    testResult.BreakOnFailures := BreakOnFailuresAction.Checked;
    suite.run(testResult);
   finally
    testResult.Free;
    testResult := nil;
   end;
  finally
    FRunning := false;
    EnableUI(true);
  end;
end;

procedure TGUITestRunner.ExitActionExecute(Sender: TObject);
begin
  if FTestResult <> nil then
    FTestResult.stop;
  self.ModalResult := mrCancel;
  Close;
end;

procedure TGUITestRunner.BreakOnFailuresActionExecute(Sender: TObject);
begin
  with BreakOnFailuresAction do
  Checked := not Checked;
end;

procedure TGUITestRunner.ShowTestedNodeActionExecute(Sender: TObject);
begin
  with ShowTestedNodeAction do
   Checked := not Checked;
end;

procedure TGUITestRunner.SetUpStateImages;
begin
  TestTree.Images             := RunImages;
end;

procedure TGUITestRunner.LoadSuiteConfiguration;
begin
  if FSuite <> nil then
   FSuite.LoadConfiguration(IniFileName, UseRegistryAction.Checked, True);
end;

procedure TGUITestRunner.MakeNodeVisible(node: TTreeNode);
begin
  {$IF RTLVersion >= 14.5}
  node.MakeVisible;
  {$ELSE}
  node.MakeVisible(true);
  {$IFEND}
end;

procedure TGUITestRunner.ProcessClickOnStateIcon;
const
  APROX_IMAGE_WIDTH = 40;
var
  Node: TTreeNode;
  Pos: TPoint;
  Rect: TRect;
begin
  GetCursorPos(Pos);
  Pos := TestTree.ScreenToClient(Pos);
  Node := TestTree.GetNodeAt(Pos.X, Pos.Y);
  if Assigned(Node) then
  begin
    Rect := Node.DisplayRect;
    if Pos.X < (Node.Level * TestTree.Indent + APROX_IMAGE_WIDTH) then
      SwitchNodeState(node);
  end;
end;

procedure TGUITestRunner.UpdateNodeImage(node: TTreeNode);
var
  test :ITest;
begin
  test := NodeToTest(node);
  if Test.Enabled then
    Node.ImageIndex := CalcImageIndex(0, NodeRunImageIndex(Node))
  else
    Node.ImageIndex := CalcImageIndex(1, NodeRunImageIndex(Node));

{  if not test.enabled then
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
  end; }
end;

procedure TGUITestRunner.UseRegistryActionExecute(Sender: TObject);
begin
  with UseRegistryAction do
   Checked := not Checked;
end;

function TGUITestRunner.GetIniFile(const FileName: string) : tCustomIniFile;
begin
  Result := TIniFile.Create( FileName );
end;

procedure TGUITestRunner.LoadRegistryAction;
begin
  with TIniFile.Create(IniFileName) do
  try
   UseRegistryAction.Checked := ReadBool(cnConfigIniSection,
    'UseRegistry', UseRegistryAction.Checked);
  finally
   Free;
  end;
end;

procedure TGUITestRunner.SaveRegistryAction;
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

procedure TGUITestRunner.RunActionUpdate(Sender: TObject);
begin
  RunAction.Enabled := not FRunning and assigned( fSuite ) and (FSuite.countEnabledTestCases > 0);
end;

procedure TGUITestRunner.CopyMessageToClipboardActionUpdate(Sender: TObject);
begin
  CopyMessageToClipboardAction.Enabled := FailureListView.Selected <> nil;
end;

procedure TGUITestRunner.SelectCurrentActionExecute(Sender: TObject);
begin
  ApplyToTests(TestTree.Selected, EnableTest);
  SetNodeState(TestTree.Selected, true);
  UpdateStatus(True);
end;

procedure TGUITestRunner.DeselectCurrentActionExecute(Sender: TObject);
begin
  ApplyToTests(TestTree.Selected, DisableTest);
  UpdateStatus(True);
end;

procedure TGUITestRunner.StopActionExecute(Sender: TObject);
begin
  if FTestResult <> nil then
    FTestResult.stop;
end;

procedure TGUITestRunner.StopActionUpdate(Sender: TObject);
begin
  StopAction.Enabled := FRunning and (FTestResult <> nil);
end;

procedure TGUITestRunner.Status(test: ITest; const Msg: string);
begin
  // Empty stub for now
end;

procedure TGUITestRunner.Warning(test: ITest; const Msg: string);
begin
  // Empty stub for now
end;

procedure TGUITestRunner.RunSelectedTestActionExecute(Sender: TObject);
begin
  Setup;
  ListSelectedTests;
  ProgressBar.Max := 1;
  ScoreBar.Max    := 1;
  RunTheTest(Suite);
  FreeAndNil(FSelectedTests);
end;

procedure TGUITestRunner.RunSelectedTestActionUpdate(Sender: TObject);
var
  aTest :ITest;
begin
  ATest := SelectedTest;
  RunSelectedTestAction.Enabled := (aTest <> nil) and (aTest.CountTestCases = 1);
end;

end.


