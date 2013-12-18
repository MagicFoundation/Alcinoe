program ztestall;

{$mode objfpc}{$H+}

uses
  custapp, sysutils, comctrls,
  Interfaces, Forms, GuiTestRunner, LResources,
  Classes, consoletestrunner, fpcunit, fpcunitreport, plaintestreport,
  ZTestConfig,
  ZSqlTestCase,
  //core
  ZTestSysUtils, ZTestVariant, ZTestTokenizer,
  ZTestList, ZTestFramework, ZTestExprToken, ZTestExpression, ZTestURL,
  //parsesql
  ZTestSybaseToken, ZTestSqLiteToken,
  ZTestSqlAnalyser, ZTestScriptParser, ZTestPostgreSqlToken, ZTestOracleToken,
  ZTestMySqlToken, ZTestInterbaseToken,
  //dbc
  ZTestDbcResultSet, ZTestDbcUtils, ZTestDbcCache,
  ZTestDbcCachedResultSet, ZTestDbcMetadata,ZTestDbcResultSetMetadata, ZTestDbcResolver,
  ZTestDbcSqLite, ZTestDbcPostgreSqlMetadata,  ZTestDbcPostgreSql, ZTestDbcOracle,
  ZTestDbcMySqlMetadata, ZTestDbcMySql, ZTestDbcMsSql, ZTestDbcInterbaseMetadata,
  ZTestDbcInterbase, ZTestDbcASA, ZTestDbcASAMetadata,
  //component
  ZTestSqlTypes, ZTestSqlStrings, ZTestSqlProcessor,
  ZTestSqlMetadata, ZTestSorting, ZTestMasterDetail, ZTestExecuteSql,
  ZTestDataSetGeneric, ZTestData, ZTestConnection,
  //bugreport
  ZTestBugDbcCore,
  ZTestBugDbcMySql, ZTestBugCompMySql,
  ZTestBugDbcOracle, ZTestBugCompOracle,
  ZTestBugDbcPostgreSql, ZTestBugCompPostgreSql,
  ZTestBugDbcInterbase, ZTestBugCompInterbase,
  ZTestBugDbcDbLib, ZTestBugCompDbLib,
  ZTestBugCompMSSql
;

type

  TTreeNodeState=(tsUnChecked, tsChecked);

  { TLazTestRunner }

  { TMyResultsWriter }

  TMyResultsWriter = class(TPlainResultsWriter)
  protected
  // override the protected methods of TCustomResultsWriter to customize its behavior
    procedure WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime); override;
    procedure WriteSuiteHeader(ATestSuite: TTestSuite; ALevel: integer); override;
    procedure WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer;
      ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer;
      ANumFailures: integer; ANumIgnores: integer); override;
  public
  end;

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
    procedure WriteCustomHelp; override;
    function GetShortOpts: string; override;
    function GetResultsWriter: TCustomResultsWriter; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TMyGUITestRunner }

  TMyGUITestRunner = class(TGUITestRunner)
  protected
  // override the protected methods of TGUITestRunner to customize its behavior
  public
    constructor Create(TheOwner: TComponent); override;
    function FindNode(NodeText : String):TTreeNode;
  end;

constructor TMyGUITestRunner.Create(TheOwner: TComponent);
var
  Suite : String;
  SuiteNode : TTreeNode;
  procedure ChangeCheck(aNode: TTreeNode; aCheck: TTreeNodeState);
  var
    i: integer;
    n: TTreeNode;
  begin
    if Assigned(aNode) then
    begin
      aNode.StateIndex := ord(aCheck);
      if (TTest(aNode.Data) is TTestSuite) then
        for i := 0 to aNode.Count - 1 do
        begin
          n := aNode.Items[i];
          ChangeCheck(n, aCheck);
        end;
    end;
  end;
begin
  inherited Create(TheOwner);
  if Application.HasOption('suite') then
    begin
      Suite := Application.GetOptionValue('suite');
      ActUncheckAllExecute(Self);
      SuiteNode := FindNode(suite);
      If SuiteNode <> nil then
        begin
          SuiteNode.selected := true;
          ChangeCheck(SuiteNode,tsChecked);
        end;
    end;
end;

function TMyGUITestRunner.FindNode(NodeText: String): TTreeNode;
var
  i: integer;
  Function CheckNodes (node:TTreeNode; ATestName:string):TTreeNode;
  var s, c, nodetext : string;
      I, p : integer;
  begin
    result := nil;
    nodetext := node.text;
      begin
      p := pos ('.', ATestName);
      if p > 0 then
        begin
        s := copy (ATestName, 1, p-1);
        c := copy (ATestName, p+1, maxint);
        end
      else
        begin
        s := '';
        c := ATestName;
        end;
      if comparetext(c, node.Text) = 0 then
        result := node
      else if (CompareText( s, node.Text) = 0) or (s = '') then
        for I := 0 to node.Count - 1 do
          begin
            result := CheckNodes(node.items[I], c);
            if result <> nil then exit;
          end;
      end
  end;
begin
  for i := 0 to TestTree.Items.Count -1 do
    begin
      Result := CheckNodes(TestTree.Items[i],NodeText);
      if result <> nil then exit;
    end;
end;

procedure TMyResultsWriter.WriteTestFooter(ATest: TTest; ALevel: integer;
  ATiming: TDateTime);
begin
  { //don't write the verbose test footer information
  inherited WriteTestFooter(ATest, ALevel, ATiming);}
end;

procedure TMyResultsWriter.WriteSuiteHeader(ATestSuite: TTestSuite;
  ALevel: integer);
begin
  { //don't write the verbose suite header information
  inherited WriteSuiteHeader(ATestSuite, ALevel);}
end;

procedure TMyResultsWriter.WriteSuiteFooter(ATestSuite: TTestSuite;
  ALevel: integer; ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer;
  ANumFailures: integer; ANumIgnores: integer);
begin
  { //don't write the verbose suite footer information
  inherited WriteSuiteFooter(ATestSuite, ALevel, ATiming, ANumRuns, ANumErrors,
    ANumFailures, ANumIgnores); }
end;

{ TMyTestRunner }

procedure TMyTestRunner.WriteCustomHelp;
begin
  inherited WriteCustomHelp;
  writeln('  -b or --batch             don''t run the GUI interface');
  writeln('  -v or --verbose           show full output (otherwise compact report is used)');
end;

function TMyTestRunner.GetShortOpts: string;
begin
  Result:=inherited GetShortOpts+'bv';
end;

function TMyTestRunner.GetResultsWriter: TCustomResultsWriter;
begin
  if (FormatParam = fPlain) and not Application.HasOption('v', 'verbose') then
    Result := TMyResultsWriter.Create(nil)
  else
    Result:=inherited GetResultsWriter;
end;

constructor TMyTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  longopts.Add('batch');
  longopts.Add('verbose');
end;

var
  Applicationc: TMyTestRunner;

{$IFDEF WINDOWS}{$R ztestall.rc}{$ENDIF}

begin
  {$I ztestall.lrs}
  TestGroup := COMMON_GROUP;
  RebuildTestDatabases;
  If Application.HasOption('b', 'batch') or Application.HasOption('h', 'help')then
  begin
    Applicationc := TMyTestRunner.Create(nil);
    Applicationc.Initialize;
    Applicationc.Run;
    Applicationc.Free;
  end
  else
  begin
    Application.Initialize;
    Application.CreateForm(TMyGuiTestRunner, TestRunner);
    Application.Run;
  end;
end.
