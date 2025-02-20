unit Unit1;

interface

uses
  Windows, Messages, Diagnostics, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StrUtils, ExtCtrls, StdCtrls, cxStyles,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxEdit,
  cxGridLevel, cxGridCustomTableView, cxGridTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, ComCtrls, Alcinoe.Sqlite3.Client, cxMemo, cxBlobEdit,
  Alcinoe.MySql.Client, cxDropDownEdit,
  Alcinoe.SphinxQL.Client, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore,
  dxSkinFoggy, dxSkinscxPCPainter, dxSkinsForm, Vcl.Menus, cxButtons,
  cxPCdxBarPopupMenu, cxPC, cxContainer, cxLabel, cxTextEdit, cxMaskEdit,
  cxButtonEdit, cxCheckBox, cxGroupBox, cxRadioGroup, Alcinoe.MemCached.Client,
  Alcinoe.MongoDB.Client, Alcinoe.JSONDoc, cxCheckGroup, cxNavigator, Shellapi,
  dxBarBuiltInMenu, dxDateRanges, dxScrollbarAnnotations, dxCore, dxUIAClasses,
  dxCoreGraphics;

type

  PPROCESS_MEMORY_COUNTERS = ^PROCESS_MEMORY_COUNTERS;
  PROCESS_MEMORY_COUNTERS = record
    cb : DWORD;
    PageFaultCount : DWORD;
    PeakWorkingSetSize : DWORD;
    WorkingSetSize : DWORD; //Task managers MemUsage number
    QuotaPeakPagedPoolUsage : DWORD;
    QuotaPagedPoolUsage : DWORD;
    QuotaPeakNonPagedPoolUsage : DWORD;
    QuotaNonPagedPoolUsage : DWORD;
    PagefileUsage : DWORD; //TaskMan's VM Size number
    PeakPagefileUsage : DWORD;
  end;
  TProcessMemoryCounters = PROCESS_MEMORY_COUNTERS;

  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    Panel3: TPanel;
    Panel5: TPanel;
    Splitter4: TSplitter;
    ALMemoResult: TcxMemo;
    PageControl1: TcxPageControl;
    MySQL: TcxTabSheet;
    SQLLite3: TcxTabSheet;
    Label3: TcxLabel;
    Label6: TcxLabel;
    Label7: TcxLabel;
    Label8: TcxLabel;
    Label10: TcxLabel;
    Label11: TcxLabel;
    Label9: TcxLabel;
    Label12: TcxLabel;
    Label32: TcxLabel;
    Label33: TcxLabel;
    Label34: TcxLabel;
    ALEditMySqlHost: TcxTextEdit;
    ALEditMySqlLogin: TcxTextEdit;
    ALEditMySqlPassword: TcxTextEdit;
    ALEditMySqlPort: TcxTextEdit;
    ALEditMySqlCharset: TcxTextEdit;
    ALEditMysqlLib: TcxButtonEdit;
    ALMemoMySqlQuery: TcxMemo;
    ALButtonMySQLSelect: TcxButton;
    ALEditMySqlDatabaseName: TcxTextEdit;
    ALEditMySqlNBLoop: TcxTextEdit;
    ALEditMySqlNbLoopBeforeCommit: TcxTextEdit;
    ALEditMySqlNBThread: TcxTextEdit;
    ALButtonMysqlUpdate: TcxButton;
    ALButtonMySqlLoopUpdate: TcxButton;
    ALButtonMysqlLoopSelect: TcxButton;
    Label24: TcxLabel;
    Label25: TcxLabel;
    Label19: TcxLabel;
    Label20: TcxLabel;
    Label21: TcxLabel;
    Label22: TcxLabel;
    Label23: TcxLabel;
    Label28: TcxLabel;
    ALEditSqlite3Lib: TcxButtonEdit;
    ALMemoSqlite3Query: TcxMemo;
    ALButtonSqlLite3Select: TcxButton;
    ALEditSqlite3Database: TcxButtonEdit;
    ALButtonSqlite3LoopSelect: TcxButton;
    ALButtonSqlite3Update: TcxButton;
    ALButtonSqlite3LoopUpdate: TcxButton;
    ALEditSQLite3NBLoop: TcxTextEdit;
    RadioGroupSqlite3Journal_Mode: TcxRadioGroup;
    RadioGroupSQLite3Temp_Store: TcxRadioGroup;
    RadioGroupSqlite3Synhcronous: TcxRadioGroup;
    ALEditSqlite3Cache_Size: TcxTextEdit;
    ALEditSqlite3Page_Size: TcxTextEdit;
    ALEditSQLite3NbLoopBeforeCommit: TcxTextEdit;
    ALEditSqlite3NBThread: TcxTextEdit;
    ALCheckBoxSqlite3SharedCache: TcxCheckBox;
    ALCheckBoxSqlite3ReadUncommited: TcxCheckBox;
    Sphinx: TcxTabSheet;
    Label36: TcxLabel;
    Label37: TcxLabel;
    Label38: TcxLabel;
    Label43: TcxLabel;
    Label44: TcxLabel;
    Label45: TcxLabel;
    Label46: TcxLabel;
    ALEditSphinxLib: TcxbuttonEdit;
    ALEditSphinxHost: TcxTextEdit;
    ALEditSphinxPort: TcxTextEdit;
    ALMemoSphinxQuery: TcxMemo;
    ALButtonSphinxSelect: TcxButton;
    ALButtonSphinxUpdate: TcxButton;
    ALButtonSphinxLoopSelect: TcxButton;
    ALButtonSphinxLoopUpdate: TcxButton;
    ALEditSphinxNBThread: TcxTextEdit;
    ALEditSphinxNBLoop: TcxTextEdit;
    ALEditSphinxNbLoopBeforeCommit: TcxTextEdit;
    Label39: TcxLabel;
    Label40: TcxLabel;
    PanelStats: TPanel;
    GridThread: TcxGrid;
    TableViewThread: TcxGridTableView;
    TableViewThreadNumber: TcxGridColumn;
    TableViewThreadCount: TcxGridColumn;
    TableViewThreadAveragePrepareTimeTaken: TcxGridColumn;
    TableViewThreadAverageExecuteTimeTaken: TcxGridColumn;
    TableViewThreadAverageCommitTimeTaken: TcxGridColumn;
    TableViewThreadErrorMsg: TcxGridColumn;
    levelThread: TcxGridLevel;
    MemCached: TcxTabSheet;
    Label41: TcxLabel;
    ALEditMemCachedHost: TcxTextEdit;
    Label42: TcxLabel;
    ALEditMemCachedPort: TcxTextEdit;
    ALMemoMemCachedData: TcxMemo;
    Label47: TcxLabel;
    ALButtonMemcachedGet: TcxButton;
    ALButtonMemcachedSet: TcxButton;
    ALButtonMemcachedLoopGet: TcxButton;
    ALButtonMemcachedLoopSet: TcxButton;
    ALEditMemcachedNBThread: TcxTextEdit;
    Label48: TcxLabel;
    Label49: TcxLabel;
    ALEditMemCachedNBLoop: TcxTextEdit;
    ALButtonMemcachedStats: TcxButton;
    ALButtonMemcachedStatsSettings: TcxButton;
    ALButtonMemcachedStatsItems: TcxButton;
    ALButtonMemcachedStatsSizes: TcxButton;
    ALButtonMemcachedStatsSlabs: TcxButton;
    dxSkinController1: TdxSkinController;
    cxStyleRepository1: TcxStyleRepository;
    cxStyle1: TcxStyle;
    ALComboBoxMySqlApiVer: TcxComboBox;
    ALComboBoxSphinxApiVer: TcxComboBox;
    ALButtonMemcachedFLush_ALL: TcxButton;
    ALButtonMemcachedVersion: TcxButton;
    ALEditMemCachedKey: TcxTextEdit;
    ALlabel123123: TcxLabel;
    ALLabel43234: TcxLabel;
    ALEditMemCachedFlags: TcxTextEdit;
    cxLabel5: TcxLabel;
    ALEditMemCachedExpTime: TcxTextEdit;
    ALButtonMemcachedDelete: TcxButton;
    ALButtonMemcachedLoopIncr: TcxButton;
    ALButtonMemcachedLoopDecr: TcxButton;
    MongoDB: TcxTabSheet;
    cxLabel3: TcxLabel;
    ALEditMongoDBHost: TcxTextEdit;
    cxLabel4: TcxLabel;
    ALEditMongoDBPort: TcxTextEdit;
    cxLabel9: TcxLabel;
    MemoMongoDBQuery: TcxMemo;
    ALButtonMongoDBSelect: TcxButton;
    ALButtonMongoDBINSERT: TcxButton;
    ALButtonMongoDBUpdate: TcxButton;
    ALButtonMongoDBDelete: TcxButton;
    ALButtonMongoDBLOOPSELECT: TcxButton;
    ALButtonMongoDBLOOPINSERT: TcxButton;
    ALButtonMongoDBLOOPUPDATE: TcxButton;
    ALEditMongoDBNBThread: TcxTextEdit;
    cxLabel10: TcxLabel;
    cxLabel11: TcxLabel;
    ALEditMongoDBNBLoop: TcxTextEdit;
    cxLabel6: TcxLabel;
    MemoMongoDBSelector: TcxMemo;
    cxLabel7: TcxLabel;
    EditMongoDBFullCollectionName: TcxTextEdit;
    cxLabel8: TcxLabel;
    EditMongoDBSkip: TcxTextEdit;
    cxLabel12: TcxLabel;
    EditMongoDBFirst: TcxTextEdit;
    cxLabel13: TcxLabel;
    cxLabel14: TcxLabel;
    cxLabel15: TcxLabel;
    cxLabel16: TcxLabel;
    CheckGroupMongoDBSelectFlags: TcxCheckGroup;
    CheckGroupMongoDBINSERTFlags: TcxCheckGroup;
    CheckGroupMongoDBUpdateFlags: TcxCheckGroup;
    CheckGroupMongoDBDeleteFlags: TcxCheckGroup;
    ALButtonMongoDBLOOPDELETE: TcxButton;
    procedure FormClick(Sender: TObject);
    procedure ALButtonMySqlSelectClick(Sender: TObject);
    procedure ALButtonSqlLite3SelectClick(Sender: TObject);
    procedure ALButtonSqlite3LoopUpdateClick(Sender: TObject);
    procedure ALButtonSqlite3UpdateClick(Sender: TObject);
    procedure ALButtonSqlite3LoopSelectClick(Sender: TObject);
    procedure ALButtonMysqlUpdateClick(Sender: TObject);
    procedure ALButtonMySqlLoopUpdateClick(Sender: TObject);
    procedure ALButtonMysqlLoopSelectClick(Sender: TObject);
    procedure ALButtonSphinxSelectClick(Sender: TObject);
    procedure ALButtonSphinxUpdateClick(Sender: TObject);
    procedure ALButtonSphinxLoopSelectClick(Sender: TObject);
    procedure ALButtonSphinxLoopUpdateClick(Sender: TObject);
    procedure ALButtonMemcachedStatsClick(Sender: TObject);
    procedure ALEditButtonFindFileClick(Sender: TObject; AButtonIndex: Integer);
    procedure ALButtonMemcachedStatsSettingsClick(Sender: TObject);
    procedure ALButtonMemcachedStatsItemsClick(Sender: TObject);
    procedure ALButtonMemcachedStatsSizesClick(Sender: TObject);
    procedure ALButtonMemcachedStatsSlabsClick(Sender: TObject);
    procedure ALButtonMemcachedFLush_ALLClick(Sender: TObject);
    procedure ALButtonMemcachedVersionClick(Sender: TObject);
    procedure ALButtonMemcachedSetClick(Sender: TObject);
    procedure ALButtonMemcachedGetClick(Sender: TObject);
    procedure ALButtonMemcachedDeleteClick(Sender: TObject);
    procedure ALButtonMemcachedLoopGetClick(Sender: TObject);
    procedure ALButtonMemcachedLoopSetClick(Sender: TObject);
    procedure ALButtonMemcachedLoopIncrClick(Sender: TObject);
    procedure ALButtonMemcachedLoopDecrClick(Sender: TObject);
    procedure ALButtonMongoDBSelectClick(Sender: TObject);
    procedure ALButtonMongoDBINSERTClick(Sender: TObject);
    procedure ALButtonMongoDBUpdateClick(Sender: TObject);
    procedure ALButtonMongoDBDeleteClick(Sender: TObject);
    procedure ALButtonMongoDBLOOPSELECTClick(Sender: TObject);
    procedure ALButtonMongoDBLOOPINSERTClick(Sender: TObject);
    procedure ALButtonMongoDBLOOPUPDATEClick(Sender: TObject);
    procedure ALButtonMongoDBLOOPDELETEClick(Sender: TObject);
  private
  public
    Sqlite3ConnectionPoolClient: TalSqlite3ConnectionPoolClient;
    MySqlConnectionPoolClient: TalMySqlConnectionPoolClient;
    MemcachedConnectionPoolClient: TALMemcachedConnectionPoolClient;
    MongoDBConnectionPoolClient: TALMongoDBConnectionPoolClient;
    NBActiveThread: integer;
    LoopStartDateTime: TdateTime;
  end;

  {--------------------------------------}
  TSqlite3BenchmarkThread = Class(Tthread)
  private
    fSQL: AnsiString;
    fUpdateSQL: Boolean;
    fOn: Boolean;
    fMaxLoop: integer;
    fNBLoopBeforeCommit: integer;
    FErrorMsg: AnsiString;
    FTotalExecuteTimeTaken: extended;
    FTotalCommitTimeTaken: extended;
    FTotalLoop: integer;
    fOwner: TWinControl;
    fRank: integer;
    Procedure UpdateGUI;
  protected
    procedure Execute; override;
  protected
  Public
    constructor Create(
                  CreateSuspended: Boolean;
                  AOwner: TwinControl;
                  aRank: integer;
                  aSQL: AnsiString;
                  aMaxLoop: integer;
                  aNBLoopBeforeCommit: integer;
                  aUpdateSQL: Boolean);
    destructor Destroy; override;
  End;

  {------------------------------------}
  TMySqlBenchmarkThread = Class(Tthread)
  private
    fSQL: AnsiString;
    fUpdateSQL: Boolean;
    fOn: Boolean;
    fMaxLoop: integer;
    fNBLoopBeforeCommit: integer;
    FErrorMsg: AnsiString;
    FTotalExecuteTimeTaken: extended;
    FTotalCommitTimeTaken: extended;
    FTotalLoop: integer;
    fOwner: TWinControl;
    fRank: integer;
    Procedure UpdateGUI;
  protected
    procedure Execute; override;
  protected
  Public
    constructor Create(
                  CreateSuspended: Boolean;
                  AOwner: TwinControl;
                  aRank: integer;
                  aSQL: AnsiString;
                  aMaxLoop: integer;
                  aNBLoopBeforeCommit: integer;
                  aUpdateSQL: Boolean);
    destructor Destroy; override;
  End;

  {----------------------------------------}
  TMemCachedBenchmarkThread = Class(Tthread)
  private
    fKey: AnsiString;
    fFlags: AnsiString;
    fExpTime: AnsiString;
    fData: AnsiString;
    fCmd: AnsiString;
    fOn: Boolean;
    fMaxLoop: integer;
    FErrorMsg: AnsiString;
    FTotalExecuteTimeTaken: extended;
    FTotalLoop: integer;
    fOwner: TWinControl;
    fRank: integer;
    Procedure UpdateGUI;
  protected
    procedure Execute; override;
  protected
  Public
    constructor Create(
                  CreateSuspended: Boolean;
                  AOwner: TwinControl;
                  aRank: integer;
                  aKey: AnsiString;
                  aFlags: AnsiString;
                  aExpTime: AnsiString;
                  aData: AnsiString;
                  aMaxLoop: integer;
                  aCmd: AnsiString);
    destructor Destroy; override;
  End;

  {--------------------------------------}
  TMongoDBBenchmarkThread = Class(Tthread)
  private
    fcmd: AnsiString;
    ffullCollectionName: AnsiString;
    fQuery: AnsiString;
    fSelector: AnsiString;
    fSkip: ansiString;
    fFirst: ansiString;
    fUpdateFlags: TALMongoDBClientUpdateDataFlags;
    fInsertFlags: TALMongoDBClientInsertDataFlags;
    fDeleteFlags: TALMongoDBClientDeleteDataFlags;
    fOn: Boolean;
    fMaxLoop: integer;
    FErrorMsg: AnsiString;
    FTotalExecuteTimeTaken: extended;
    FTotalLoop: integer;
    fOwner: TWinControl;
    fRank: integer;
    Procedure UpdateGUI;
  protected
    procedure Execute; override;
  protected
  Public
    constructor Create(
                  CreateSuspended: Boolean;
                  AOwner: TwinControl;
                  aRank: integer;
                  aMaxLoop: integer;
                  acmd: AnsiString;
                  afullCollectionName: AnsiString;
                  aQuery: AnsiString;
                  aSelector: AnsiString;
                  aInsertIfNotFound: boolean;
                  aMultiUpdate: boolean;
                  aContinueOnError: boolean;
                  aSingleRemove: boolean;
                  aSkip: ansiString;
                  aFirst: ansiString);
    destructor Destroy; override;
  End;


function GetProcessMemoryInfo(Process : THandle; var MemoryCounters : TProcessMemoryCounters; cb : DWORD) : BOOL; stdcall;
function ProcessMemoryUsage(ProcessID : DWORD): DWORD;

var Form1: TForm1;

implementation

uses
  SyncObjs,
  System.AnsiStrings,
  Alcinoe.WinApi.Common,
  Alcinoe.MySql.Wrapper,
  Alcinoe.Sqlite3.Wrapper,
  Alcinoe.AVLBinaryTree,
  Alcinoe.Common,
  Alcinoe.XMLDoc,
  Alcinoe.StringList,
  Alcinoe.StringUtils;

{$R *.dfm}

Var currentIncNumber: integer;
    currentIncNumberCR: TcriticalSection;

{**************************************************}
function GetProcessMemoryInfo; external 'psapi.dll';

{****************************************************}
function ProcessMemoryUsage(ProcessID : DWORD): DWORD;
var ProcessHandle : THandle;
    MemCounters   : TProcessMemoryCounters;
begin
  Result := 0;
  ProcessHandle := OpenProcess(
                     PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
                     false,
                     ProcessID);
  try
    if GetProcessMemoryInfo(
         ProcessHandle,
         MemCounters,
         sizeof(MemCounters))
    then Result := MemCounters.WorkingSetSize;
  finally
    CloseHandle(ProcessHandle);
  end;
end;

{***************************************************************************************************************************************}
function SQLFastTagReplaceFunct(const TagString: AnsiString; TagParams: TALStringsA; Context: pointer; Var Handled: Boolean): AnsiString;
Var LMin, LMax: integer;
begin

  Handled := True;
  if ALSameTextA(TagString,'randomchar') then result := ALRandomStrA(1)
  else if ALSameTextA(TagString,'randomstring') then begin
    if not ALTryStrToInt(TagParams.Values['MinLength'], LMin) then LMin := 1;
    if not ALTryStrToInt(TagParams.Values['MaxLength'], LMax) then LMax := 255;
    result := ALRandomStrA(LMin + random(LMax - LMin + 1));
  end
  else if ALSameTextA(TagString,'randomnumber') then begin
    if not ALTryStrToInt(TagParams.Values['Min'], LMin) then LMin := 1;
    if not ALTryStrToInt(TagParams.Values['Max'], LMax) then LMax := Maxint;
    result := ALIntToStrA(LMin + random(LMax - LMin + 1));
  end
  else if ALSameTextA(TagString,'incnumber') then begin
    if not ALTryStrToInt(TagParams.Values['min'], LMin) then LMin := 0;
    currentIncNumberCR.Acquire;
    try
      inc(currentIncNumber);
      if LMin > currentIncNumber then begin
        currentIncNumber := LMin;
        result := ALIntToStrA(currentIncNumber);
      end
      else result := ALIntToStrA(currentIncNumber);
    finally
      currentIncNumberCR.release;
    end;
    result := ALIntToStrA(ALStrToInt(result)+1);
  end
  else Handled := False;
end;

{*********************************************************}
procedure TForm1.ALButtonMySqlSelectClick(Sender: TObject);
Var LMySqlClient: TalMySqlClient;
    LXMLDATA: TalXmlDocument;
    LStopWatch: TstopWatch;
    LFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
    LMySQLAPiVersion: TALMySqlVersion_API;
begin

  case ALComboBoxMySqlApiVer.ItemIndex of
    1: LMySQLAPiVersion := MYSQL55;
    else LMySQLAPiVersion := MYSQL50;
  end;

  LFormatSettings := ALDefaultFormatSettingsA;
  Screen.Cursor := CrHourGlass;
  try

    LMySqlClient := TalMySqlClient.Create(LMySQLAPiVersion, AnsiString(ALEditMySqllib.Text));
    Try
      LMySqlClient.connect(
        AnsiString(ALEditMySqlHost.Text),
        StrToInt(ALEditMySqlPort.Text),
        AnsiString(ALEditMySqlDatabaseName.Text),
        AnsiString(ALEditMySqlLogin.Text),
        AnsiString(ALEditMySqlPassword.Text),
        AnsiString(ALEditMySqlCharset.Text),
        0);

      LXMLDATA := TALXmlDocument.create('root');
      Try

        With LXMLDATA Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
        end;

        S1 := ALFastTagReplaceA(
                AnsiString(AlMemoMySqlQuery.Lines.Text),
                '<#',
                '>',
                SQLFastTagReplaceFunct,
                True,
                nil);

        LStopWatch:= TstopWatch.StartNew;
        LMySqlClient.SelectData(
          S1,
          'rec',
           0,
           200,
          LXMLDATA.DocumentElement,
          LFormatSettings);
        LStopWatch.Stop;

        TableViewThread.DataController.RecordCount := 1;
        TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
        TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
        TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,LStopWatch.Elapsed.TotalMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
        StatusBar1.Panels[2].Text := '';
        AlMemoResult.Lines.Text := String(LXMLDATA.XML);

      Finally
        LXMLDATA.free;
      End;

    Finally
      LMySqlClient.disconnect;
      LMySqlClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{*********************************************************}
procedure TForm1.ALButtonMysqlUpdateClick(Sender: TObject);
Var LMySqlClient: TalMySqlClient;
    LTKExecute: TstopWatch;
    LTKCommit: TStopWatch;
    LLstSql: TALStringListA;
    S1: AnsiString;
    LMySQLAPiVersion: TALMySqlVersion_API;
begin

  case ALComboBoxMySqlApiVer.ItemIndex of
    1: LMySQLAPiVersion := MYSQL55;
    else LMySQLAPiVersion := MYSQL50;
  end;

  Screen.Cursor := CrHourGlass;
  try

    LMySqlClient := TalMySqlClient.Create(LMySQLAPiVersion, AnsiString(ALEditMySqllib.Text));
    LLstSql := TALStringListA.Create;
    Try
      LMySqlClient.connect(
        AnsiString(ALEditMySqlHost.Text),
        StrToInt(ALEditMySqlPort.Text),
        AnsiString(ALEditMySqlDatabaseName.Text),
        AnsiString(ALEditMySqlLogin.Text),
        AnsiString(ALEditMySqlPassword.Text),
        AnsiString(ALEditMySqlCharset.Text),
        0);

      S1 := ALFastTagReplaceA(
              AnsiString(AlMemoMySqlQuery.Lines.Text),
              '<#',
              '>',
              SQLFastTagReplaceFunct,
              True,
              nil);

      S1 := ALStringReplaceA(S1,#13#10,' ',[RfReplaceALL]);
      LLstSql.Text := ALTrim(ALStringReplaceA(S1,';',#13#10,[RfReplaceALL]));

      LTKExecute:= TstopWatch.StartNew;
      LMySqlClient.TransactionStart;
      try
        LMySqlClient.UpdateData(LLstSql);
        LTKExecute.Stop;
        LTKCommit := TStopWatch.StartNew;
        LMySqlClient.TransactionCommit;
        LTKCommit.Stop;
      Except
        LMySqlClient.TransactionRollBack;
        raise;
      end;

      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,LTKExecute.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,LTKCommit.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.Text := '';

    Finally
      LMySqlClient.disconnect;
      LMySqlClient.free;
      LLstSql.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{*************************************************************}
procedure TForm1.ALButtonMysqlLoopSelectClick(Sender: TObject);
Var LMySqlBenchmarkThread: TMySqlBenchmarkThread;
    LMySQLAPiVersion: TALMySqlVersion_API;
    I: integer;
begin

  case ALComboBoxMySqlApiVer.ItemIndex of
    1: LMySQLAPiVersion := MYSQL55;
    else LMySQLAPiVersion := MYSQL50;
  end;

  {init button action}
  If ALButtonMySqlLoopSelect.tag = 0 then begin
    ALButtonMySqlLoopSelect.Tag := 1;
    ALButtonMySqlLoopSelect.Caption := 'STOP';
  end
  else If ALButtonMySqlLoopSelect.tag = 1 then begin
    ALButtonMySqlLoopSelect.Tag := 2;
    ALButtonMySqlLoopSelect.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := StrToInt(ALEditMySqlNBThread.Text);

  //create the fMySqlConnectionPoolClient
  if not assigned(MySqlConnectionPoolClient) then begin
    MySqlConnectionPoolClient := TALMySqlConnectionPoolClient.Create(
                                   AnsiString(ALEditMySqlHost.Text),
                                   StrToInt(ALEditMySqlPort.Text),
                                   AnsiString(ALEditMySqlDatabaseName.Text),
                                   AnsiString(ALEditMySqlLogin.Text),
                                   AnsiString(ALEditMySqlPassword.Text),
                                   AnsiString(ALEditMySqlCharset.Text),
                                   LMySQLAPiVersion,
                                   AnsiString(ALEditMySqlLib.Text),
                                   0);
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for I := 1 to StrToInt(ALEditMySqlNBThread.Text) do begin
    TableViewThread.DataController.SetValue(I-1,TableViewThreadNumber.Index,ALIntToStrA(I) + ' (on)');
    LMySqlBenchmarkThread := TMySqlBenchmarkThread.Create(
                               True,
                               self,
                               I,
                               ALTrim(AnsiString(ALMemoMySqlQuery.Lines.Text)),
                               StrToInt(ALEditMySqlNBLoop.Text),
                               StrToInt(ALEditMySqlNbLoopBeforeCommit.Text),
                               false);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    LMySqlBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    LMySqlBenchmarkThread.Start;
    {$ELSE}
    aMySqlBenchmarkThread.Resume;
    {$ENDIF}

  end;

end;

{*************************************************************}
procedure TForm1.ALButtonMySqlLoopUpdateClick(Sender: TObject);
Var LMySqlBenchmarkThread: TMySqlBenchmarkThread;
    LMySQLAPiVersion: TALMySqlVersion_API;
    i: integer;
begin

  case ALComboBoxMySqlApiVer.ItemIndex of
    1: LMySQLAPiVersion := MYSQL55;
    else LMySQLAPiVersion := MYSQL50;
  end;

  {init button action}
  If ALButtonMySqlLoopUpdate.tag = 0 then begin
    ALButtonMySqlLoopUpdate.Tag := 1;
    ALButtonMySqlLoopUpdate.Caption := 'STOP';
  end
  else If ALButtonMySqlLoopUpdate.tag = 1 then begin
    ALButtonMySqlLoopUpdate.Tag := 2;
    ALButtonMySqlLoopUpdate.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := StrToInt(ALEditMySqlNBThread.Text);

  //create the fMySqlConnectionPoolClient
  if not assigned(MySqlConnectionPoolClient) then begin
    MySqlConnectionPoolClient := TALMySqlConnectionPoolClient.Create(
                                   AnsiString(ALEditMySqlHost.Text),
                                   StrToInt(ALEditMySqlPort.Text),
                                   AnsiString(ALEditMySqlDatabaseName.Text),
                                   AnsiString(ALEditMySqlLogin.Text),
                                   AnsiString(ALEditMySqlPassword.Text),
                                   AnsiString(ALEditMySqlCharset.Text),
                                   LMySQLAPiVersion,
                                   AnsiString(ALEditMySqlLib.Text),
                                   0);
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditMySqlNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    LMySqlBenchmarkThread := TMySqlBenchmarkThread.Create(
                               True,
                               self,
                               i,
                               ALTrim(AnsiString(ALMemoMySqlQuery.Lines.Text)),
                               StrToInt(ALEditMySqlNBLoop.Text),
                               StrToInt(ALEditMySqlNbLoopBeforeCommit.Text),
                               true);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    LMySqlBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    LMySqlBenchmarkThread.Start;
    {$ELSE}
    aMySqlBenchmarkThread.Resume;
    {$ENDIF}

  end;

end;

{************************************************************}
procedure TForm1.ALButtonSqlLite3SelectClick(Sender: TObject);
Var LSqlite3Client: TalSqlite3Client;
    LXMLDATA: TalXmlDocument;
    LStopWatch: TStopWatch;
    LFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
begin
  LFormatSettings := ALDefaultFormatSettingsA;
  Screen.Cursor := CrHourGlass;
  try

    LSqlite3Client := TalSqlite3Client.Create(AnsiString(ALEditSqlite3lib.Text));
    Try

      //enable or disable the shared cache
      LSqlite3Client.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);

      //connect
      LSqlite3Client.connect(AnsiString(ALEditSqlite3Database.text));

      //the pragma
      LSqlite3Client.UpdateData('PRAGMA page_size = '+AnsiString(ALEditSqlite3Page_Size.Text));
      LSqlite3Client.UpdateData('PRAGMA encoding = "UTF-8"');
      LSqlite3Client.UpdateData('PRAGMA legacy_file_format = 0');
      LSqlite3Client.UpdateData('PRAGMA auto_vacuum = NONE');
      LSqlite3Client.UpdateData('PRAGMA cache_size = '+AnsiString(ALEditSqlite3Cache_Size.Text));
      case RadioGroupSqlite3Journal_Mode.ItemIndex of
        0: LSqlite3Client.UpdateData('PRAGMA journal_mode = DELETE');
        1: LSqlite3Client.UpdateData('PRAGMA journal_mode = TRUNCATE');
        2: LSqlite3Client.UpdateData('PRAGMA journal_mode = PERSIST');
        3: LSqlite3Client.UpdateData('PRAGMA journal_mode = MEMORY');
        4: LSqlite3Client.UpdateData('PRAGMA journal_mode = WAL');
        5: LSqlite3Client.UpdateData('PRAGMA journal_mode = OFF');
      end;
      LSqlite3Client.UpdateData('PRAGMA locking_mode = NORMAL');
      If ALCheckBoxSqlite3ReadUncommited.Checked then LSqlite3Client.UpdateData('PRAGMA read_uncommitted = 1');
      case RadioGroupSqlite3Synhcronous.ItemIndex of
        0: LSqlite3Client.UpdateData('PRAGMA synchronous = OFF');
        1: LSqlite3Client.UpdateData('PRAGMA synchronous = NORMAL');
        2: LSqlite3Client.UpdateData('PRAGMA synchronous = FULL');
      end;
      case RadioGroupSQLite3Temp_Store.ItemIndex of
        0: LSqlite3Client.UpdateData('PRAGMA temp_store = DEFAULT');
        1: LSqlite3Client.UpdateData('PRAGMA temp_store = FILE');
        2: LSqlite3Client.UpdateData('PRAGMA temp_store = MEMORY');
      end;

      //the sql
      LXMLDATA := TALXmlDocument.create('root');
      Try

        With LXMLDATA Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
        end;

        S1 := ALFastTagReplaceA(
                AnsiString(AlMemoSQLite3Query.Lines.Text),
                '<#',
                '>',
                SQLFastTagReplaceFunct,
                True,
                nil);

        LStopWatch := TStopWatch.StartNew;
        LSqlite3Client.SelectData(
          S1,
          'rec',
           0,
           200,
          LXMLDATA.DocumentElement,
          LFormatSettings);
        LStopWatch.Stop;

        TableViewThread.DataController.RecordCount := 1;
        TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
        TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
        TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,LStopWatch.Elapsed.TotalMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
        StatusBar1.Panels[2].Text := '';
        AlMemoResult.Lines.Text := String(LXMLDATA.XML);

      Finally
        LXMLDATA.free;
      End;

    Finally
      LSqlite3Client.disconnect;
      LSqlite3Client.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{***********************************************************}
procedure TForm1.ALButtonSqlite3UpdateClick(Sender: TObject);
Var LSqlite3Client: TalSqlite3Client;
    LTKExecute: TStopWatch;
    LTKCommit: TStopWatch;
    LLstSql: TALStringListA;
    S1: AnsiString;
begin
  Screen.Cursor := CrHourGlass;
  try

    LSqlite3Client := TalSqlite3Client.Create(AnsiString(ALEditSqlite3lib.Text));
    LLstSql := TALStringListA.Create;
    Try

      //enable or disable the shared cache
      LSqlite3Client.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);

      //connect
      LSqlite3Client.connect(AnsiString(ALEditSqlite3Database.text));

      //the pragma
      LSqlite3Client.UpdateData('PRAGMA page_size = '+AnsiString(ALEditSqlite3Page_Size.Text));
      LSqlite3Client.UpdateData('PRAGMA encoding = "UTF-8"');
      LSqlite3Client.UpdateData('PRAGMA legacy_file_format = 0');
      LSqlite3Client.UpdateData('PRAGMA auto_vacuum = NONE');
      LSqlite3Client.UpdateData('PRAGMA cache_size = '+AnsiString(ALEditSqlite3Cache_Size.Text));
      case RadioGroupSqlite3Journal_Mode.ItemIndex of
        0: LSqlite3Client.UpdateData('PRAGMA journal_mode = DELETE');
        1: LSqlite3Client.UpdateData('PRAGMA journal_mode = TRUNCATE');
        2: LSqlite3Client.UpdateData('PRAGMA journal_mode = PERSIST');
        3: LSqlite3Client.UpdateData('PRAGMA journal_mode = MEMORY');
        4: LSqlite3Client.UpdateData('PRAGMA journal_mode = WAL');
        5: LSqlite3Client.UpdateData('PRAGMA journal_mode = OFF');
      end;
      LSqlite3Client.UpdateData('PRAGMA locking_mode = NORMAL');
      If ALCheckBoxSqlite3ReadUncommited.Checked then LSqlite3Client.UpdateData('PRAGMA read_uncommitted = 1');
      case RadioGroupSqlite3Synhcronous.ItemIndex of
        0: LSqlite3Client.UpdateData('PRAGMA synchronous = OFF');
        1: LSqlite3Client.UpdateData('PRAGMA synchronous = NORMAL');
        2: LSqlite3Client.UpdateData('PRAGMA synchronous = FULL');
      end;
      case RadioGroupSQLite3Temp_Store.ItemIndex of
        0: LSqlite3Client.UpdateData('PRAGMA temp_store = DEFAULT');
        1: LSqlite3Client.UpdateData('PRAGMA temp_store = FILE');
        2: LSqlite3Client.UpdateData('PRAGMA temp_store = MEMORY');
      end;

      //the sql
      S1 := ALFastTagReplaceA(
              AnsiString(AlMemoSQLite3Query.Lines.Text),
              '<#',
              '>',
              SQLFastTagReplaceFunct,
              True,
              nil);

      S1 := ALStringReplaceA(S1,#13#10,' ',[RfReplaceALL]);
      LLstSql.Text := ALTrim(ALStringReplaceA(S1,';',#13#10,[RfReplaceALL]));

      //do the job
      LTKExecute := TStopWatch.StartNew;
      LSqlite3Client.TransactionStart;
      try
        LSqlite3Client.UpdateData(LLstSql);
        LTKExecute.Stop;
        LTKCommit := TStopWatch.StartNew;
        LSqlite3Client.TransactionCommit;
        LTKCommit.Stop;
      Except
        LSqlite3Client.TransactionRollBack;
        raise;
      end;

      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,LTKExecute.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,LTKCommit.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.Text := '';

    Finally
      LSqlite3Client.disconnect;
      LSqlite3Client.free;
      LLstSql.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{**********************************************************}
procedure TForm1.ALButtonSphinxSelectClick(Sender: TObject);
Var LSphinxClient: TalSphinxQLClient;
    LXMLDATA1: TalXmlDocument;
    LXMLDATA2: TalXmlDocument;
    LStopWatch: TStopWatch;
    LFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
    LMySQLAPiVersion: TALMySqlVersion_API;
    i: integer;
begin

  case ALComboBoxSphinxApiVer.ItemIndex of
    1: LMySQLAPiVersion := MYSQL55;
    else LMySQLAPiVersion := MYSQL50;
  end;

  LFormatSettings := ALDefaultFormatSettingsA;
  Screen.Cursor := CrHourGlass;
  try

    LSphinxClient := TalSphinxQLClient.Create(LMySQLAPiVersion, AnsiString(ALEditSphinxlib.Text));
    Try
      LSphinxClient.connect(
        AnsiString(ALEditSphinxHost.Text),
        StrToInt(ALEditSphinxPort.Text));

      LXMLDATA1 := TALXmlDocument.create('root');
      LXMLDATA2 := TALXmlDocument.create('root');
      Try

        With LXMLDATA1 Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
        end;

        S1 := ALFastTagReplaceA(
                AnsiString(AlMemoSphinxQuery.Lines.Text),
                '<#',
                '>',
                SQLFastTagReplaceFunct,
                True,
                nil);

        LStopWatch := TStopWatch.StartNew;
        LSphinxClient.SelectData(
          S1,
          'rec',
           0,
           200,
          LXMLDATA1.DocumentElement,
          LFormatSettings);
        LStopWatch.Stop;


        ALMemoResult.Clear;
        ALMemoResult.Lines.Add('Time Taken: ' + string(ALFormatFloatA('0.#####', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsA)) + ' ms');
        LSphinxClient.SelectData(
          'SHOW META',
          'rec',
          LXMLDATA2.DocumentElement,
          LFormatSettings);
        ALMemoResult.Lines.Add('');
        for I := 0 to LXMLDATA2.DocumentElement.ChildNodes.Count - 1 do
          ALMemoResult.Lines.Add(String(LXMLDATA2.DocumentElement.ChildNodes[i].childnodes['variable_name'].Text) + ': ' + String(LXMLDATA2.DocumentElement.ChildNodes[i].childnodes['value'].Text));
        AlMemoResult.Lines.add('');
        AlMemoResult.Lines.add('**************');
        AlMemoResult.Lines.add('');
        AlMemoResult.Lines.Text := AlMemoResult.Lines.Text + String(LXMLDATA1.XML);

        TableViewThread.DataController.RecordCount := 1;
        TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
        TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
        TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,LStopWatch.Elapsed.TotalMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
        StatusBar1.Panels[2].Text := '';

      Finally
        LXMLDATA1.free;
        LXMLDATA2.free;
      End;

    Finally
      LSphinxClient.disconnect;
      LSphinxClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{**********************************************************}
procedure TForm1.ALButtonSphinxUpdateClick(Sender: TObject);
Var LSphinxClient: TalSphinxQLClient;
    LTKExecute: TStopWatch;
    LTKCommit: TStopWatch;
    LLstSql: TALStringListA;
    S1: AnsiString;
    LMySQLAPiVersion: TALMySqlVersion_API;
begin

  case ALComboBoxSphinxApiVer.ItemIndex of
    1: LMySQLAPiVersion := MYSQL55;
    else LMySQLAPiVersion := MYSQL50;
  end;

  Screen.Cursor := CrHourGlass;
  try

    LSphinxClient := TalSphinxQLClient.Create(LMySQLAPiVersion, AnsiString(ALEditSphinxlib.Text));
    LLstSql := TALStringListA.Create;
    Try
      LSphinxClient.connect(
        AnsiString(ALEditSphinxHost.Text),
        StrToInt(ALEditSphinxPort.Text));

      S1 := ALFastTagReplaceA(
              AnsiString(AlMemoSphinxQuery.Lines.Text),
              '<#',
              '>',
              SQLFastTagReplaceFunct,
              True,
              nil);

      S1 := ALStringReplaceA(S1,#13#10,' ',[RfReplaceALL]);
      LLstSql.Text := ALTrim(ALStringReplaceA(S1,';',#13#10,[RfReplaceALL]));

      LTKExecute := TStopWatch.StartNew;
      LSphinxClient.TransactionStart;
      try
        LSphinxClient.UpdateData(LLstSql);
        LTKExecute.Stop;
        LTKCommit := TStopWatch.StartNew;
        LSphinxClient.TransactionCommit;
        LTKCommit.Stop;
      Except
        LSphinxClient.TransactionRollBack;
        raise;
      end;

      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,LTKExecute.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,LTKCommit.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.Text := '';

    Finally
      LSphinxClient.disconnect;
      LSphinxClient.free;
      LLstSql.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{**************************************************************}
procedure TForm1.ALButtonSphinxLoopSelectClick(Sender: TObject);
Var LSphinxBenchmarkThread: TMYsqlBenchmarkThread;
    LMySQLAPiVersion: TALMySqlVersion_API;
    i: integer;
begin

  case ALComboBoxSphinxApiVer.ItemIndex of
    1: LMySQLAPiVersion := MYSQL55;
    else LMySQLAPiVersion := MYSQL50;
  end;

  {init button action}
  If ALButtonSphinxLoopSelect.tag = 0 then begin
    ALButtonSphinxLoopSelect.Tag := 1;
    ALButtonSphinxLoopSelect.Caption := 'STOP';
  end
  else If ALButtonSphinxLoopSelect.tag = 1 then begin
    ALButtonSphinxLoopSelect.Tag := 2;
    ALButtonSphinxLoopSelect.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := ALStrToInt(AnsiString(ALEditSphinxNBThread.Text));

  //create the fSphinxConnectionPoolClient
  if not assigned(MySQLConnectionPoolClient) then begin
    MySQLConnectionPoolClient := TALMySqlConnectionPoolClient.Create(
                                   AnsiString(ALEditSphinxHost.Text),
                                   StrToInt(ALEditSphinxPort.Text),
                                   '',
                                   '',
                                   '',
                                   '',
                                   LMySQLAPiVersion,
                                   AnsiString(ALEditSphinxLib.Text),
                                   0);
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditSphinxNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    LSphinxBenchmarkThread := TMySQLBenchmarkThread.Create(
                                True,
                                self,
                                i,
                                ALTrim(AnsiString(ALMemoSphinxQuery.Lines.Text)),
                                StrToInt(ALEditSphinxNBLoop.Text),
                                StrToInt(ALEditSphinxNbLoopBeforeCommit.Text),
                                false);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    LSphinxBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    LSphinxBenchmarkThread.Start;
    {$ELSE}
    aSphinxBenchmarkThread.Resume;
    {$ENDIF}

  end;

end;

{**************************************************************}
procedure TForm1.ALButtonSphinxLoopUpdateClick(Sender: TObject);
Var LSphinxBenchmarkThread: TMySqlBenchmarkThread;
    LMySQLAPiVersion: TALMySqlVersion_API;
    i: integer;
begin

  case ALComboBoxSphinxApiVer.ItemIndex of
    1: LMySQLAPiVersion := MYSQL55;
    else LMySQLAPiVersion := MYSQL50;
  end;

  {init button action}
  If ALButtonSphinxLoopUpdate.tag = 0 then begin
    ALButtonSphinxLoopUpdate.Tag := 1;
    ALButtonSphinxLoopUpdate.Caption := 'STOP';
  end
  else If ALButtonSphinxLoopUpdate.tag = 1 then begin
    ALButtonSphinxLoopUpdate.Tag := 2;
    ALButtonSphinxLoopUpdate.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := StrToInt(ALEditSphinxNBThread.Text);

  //create the fSphinxConnectionPoolClient
  if not assigned(MySQLConnectionPoolClient) then begin
    MySqlConnectionPoolClient := TALMySQLConnectionPoolClient.Create(
                                   AnsiString(ALEditSphinxHost.Text),
                                   StrToInt(ALEditSphinxPort.Text),
                                   '',
                                   '',
                                   '',
                                   '',
                                   LMySQLAPiVersion,
                                   AnsiString(ALEditSphinxLib.Text),
                                   0);
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditSphinxNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    LSphinxBenchmarkThread := TMysqlBenchmarkThread.Create(
                                True,
                                self,
                                i,
                                ALTrim(AnsiString(ALMemoSphinxQuery.Lines.Text)),
                                StrToInt(ALEditSphinxNBLoop.Text),
                                StrToInt(ALEditSphinxNbLoopBeforeCommit.Text),
                                true);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    LSphinxBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    LSphinxBenchmarkThread.Start;
    {$ELSE}
    aSphinxBenchmarkThread.Resume;
    {$ENDIF}

  end;

end;

{***************************************************************}
procedure TForm1.ALButtonSqlite3LoopSelectClick(Sender: TObject);
Var LSqlite3BenchmarkThread: TSqlite3BenchmarkThread;
    LPragmaStatements: AnsiString;
    i: integer;
begin

  {init button action}
  If ALButtonSqlite3LoopSelect.tag = 0 then begin
    ALButtonSqlite3LoopSelect.Tag := 1;
    ALButtonSqlite3LoopSelect.Caption := 'STOP';
  end
  else If ALButtonSqlite3LoopSelect.tag = 1 then begin
    ALButtonSqlite3LoopSelect.Tag := 2;
    ALButtonSqlite3LoopSelect.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := StrToInt(ALEditSqlite3NBThread.Text);

  //init the aPragmaStatements
  LPragmaStatements := '';
  LPragmaStatements := LPragmaStatements + 'PRAGMA page_size = '+AnsiString(ALEditSqlite3Page_Size.Text)+';';
  LPragmaStatements := LPragmaStatements + 'PRAGMA encoding = "UTF-8";';
  LPragmaStatements := LPragmaStatements + 'PRAGMA legacy_file_format = 0;';
  LPragmaStatements := LPragmaStatements + 'PRAGMA auto_vacuum = NONE;';
  LPragmaStatements := LPragmaStatements + 'PRAGMA cache_size = '+AnsiString(ALEditSqlite3Cache_Size.Text)+';';
  case RadioGroupSqlite3Journal_Mode.ItemIndex of
    0: LPragmaStatements := LPragmaStatements + 'PRAGMA journal_mode = DELETE;';
    1: LPragmaStatements := LPragmaStatements + 'PRAGMA journal_mode = TRUNCATE;';
    2: LPragmaStatements := LPragmaStatements + 'PRAGMA journal_mode = PERSIST;';
    3: LPragmaStatements := LPragmaStatements + 'PRAGMA journal_mode = MEMORY;';
    4: LPragmaStatements := LPragmaStatements + 'PRAGMA journal_mode = WAL;';
    5: LPragmaStatements := LPragmaStatements + 'PRAGMA journal_mode = OFF;';
  end;
  LPragmaStatements := LPragmaStatements + 'PRAGMA locking_mode = NORMAL;';
  If ALCheckBoxSqlite3ReadUncommited.Checked then LPragmaStatements := LPragmaStatements + 'PRAGMA read_uncommitted = 1;';
  case RadioGroupSqlite3Synhcronous.ItemIndex of
    0: LPragmaStatements := LPragmaStatements + 'PRAGMA synchronous = OFF;';
    1: LPragmaStatements := LPragmaStatements + 'PRAGMA synchronous = NORMAL;';
    2: LPragmaStatements := LPragmaStatements + 'PRAGMA synchronous = FULL;';
  end;
  case RadioGroupSQLite3Temp_Store.ItemIndex of
    0: LPragmaStatements := LPragmaStatements + 'PRAGMA temp_store = DEFAULT;';
    1: LPragmaStatements := LPragmaStatements + 'PRAGMA temp_store = FILE;';
    2: LPragmaStatements := LPragmaStatements + 'PRAGMA temp_store = MEMORY;';
  end;

  //create the fSqlite3ConnectionPoolClient
  if not assigned(Sqlite3ConnectionPoolClient) then begin
    Sqlite3ConnectionPoolClient := TALSqlite3ConnectionPoolClient.Create(
                                     AnsiString(ALEditSqlite3Database.text),
                                     SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE,
                                     LPragmaStatements,
                                     AnsiString(ALEditSqlite3lib.Text));

    //enable or disable the shared cache
    Sqlite3ConnectionPoolClient.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditSqlite3NBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    LSqlite3BenchmarkThread := TSqlite3BenchmarkThread.Create(
                                 True,
                                 self,
                                 i,
                                 ALTrim(AnsiString(ALMemoSqlite3Query.Lines.Text)),
                                 StrToInt(ALEditSqlite3NBLoop.Text),
                                 StrToInt(ALEditSQLite3NbLoopBeforeCommit.Text),
                                 false);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    LSqlite3BenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    LSqlite3BenchmarkThread.Start;
    {$ELSE}
    aSqlite3BenchmarkThread.Resume;
    {$ENDIF}

  end;

end;

{***************************************************************}
procedure TForm1.ALButtonSqlite3LoopUpdateClick(Sender: TObject);
Var LSqlite3BenchmarkThread: TSqlite3BenchmarkThread;
    LPragmaStatements: AnsiString;
    i: integer;
begin

  {init button action}
  If ALButtonSqlite3LoopUpdate.tag = 0 then begin
    ALButtonSqlite3LoopUpdate.Tag := 1;
    ALButtonSqlite3LoopUpdate.Caption := 'STOP';
  end
  else If ALButtonSqlite3LoopUpdate.tag = 1 then begin
    ALButtonSqlite3LoopUpdate.Tag := 2;
    ALButtonSqlite3LoopUpdate.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := StrToInt(ALEditSqlite3NBThread.Text);

  //init the aPragmaStatements
  LPragmaStatements := '';
  LPragmaStatements := LPragmaStatements + 'PRAGMA page_size = '+AnsiString(ALEditSqlite3Page_Size.Text)+';';
  LPragmaStatements := LPragmaStatements + 'PRAGMA encoding = "UTF-8";';
  LPragmaStatements := LPragmaStatements + 'PRAGMA legacy_file_format = 0;';
  LPragmaStatements := LPragmaStatements + 'PRAGMA auto_vacuum = NONE;';
  LPragmaStatements := LPragmaStatements + 'PRAGMA cache_size = '+AnsiString(ALEditSqlite3Cache_Size.Text)+';';
  case RadioGroupSqlite3Journal_Mode.ItemIndex of
    0: LPragmaStatements := LPragmaStatements + 'PRAGMA journal_mode = DELETE;';
    1: LPragmaStatements := LPragmaStatements + 'PRAGMA journal_mode = TRUNCATE;';
    2: LPragmaStatements := LPragmaStatements + 'PRAGMA journal_mode = PERSIST;';
    3: LPragmaStatements := LPragmaStatements + 'PRAGMA journal_mode = MEMORY;';
    4: LPragmaStatements := LPragmaStatements + 'PRAGMA journal_mode = WAL;';
    5: LPragmaStatements := LPragmaStatements + 'PRAGMA journal_mode = OFF;';
  end;
  LPragmaStatements := LPragmaStatements + 'PRAGMA locking_mode = NORMAL;';
  If ALCheckBoxSqlite3ReadUncommited.Checked then LPragmaStatements := LPragmaStatements + 'PRAGMA read_uncommitted = 1;';
  case RadioGroupSqlite3Synhcronous.ItemIndex of
    0: LPragmaStatements := LPragmaStatements + 'PRAGMA synchronous = OFF;';
    1: LPragmaStatements := LPragmaStatements + 'PRAGMA synchronous = NORMAL;';
    2: LPragmaStatements := LPragmaStatements + 'PRAGMA synchronous = FULL;';
  end;
  case RadioGroupSQLite3Temp_Store.ItemIndex of
    0: LPragmaStatements := LPragmaStatements + 'PRAGMA temp_store = DEFAULT;';
    1: LPragmaStatements := LPragmaStatements + 'PRAGMA temp_store = FILE;';
    2: LPragmaStatements := LPragmaStatements + 'PRAGMA temp_store = MEMORY;';
  end;

  //create the fSqlite3ConnectionPoolClient
  if not assigned(Sqlite3ConnectionPoolClient) then begin
    Sqlite3ConnectionPoolClient := TALSqlite3ConnectionPoolClient.Create(
                                     AnsiString(ALEditSqlite3Database.text),
                                     SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE,
                                     LPragmaStatements,
                                     AnsiString(ALEditSqlite3lib.Text));

    //enable or disable the shared cache
    Sqlite3ConnectionPoolClient.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditSqlite3NBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    LSqlite3BenchmarkThread := TSqlite3BenchmarkThread.Create(
                                 True,
                                 self,
                                 i,
                                 ALTrim(AnsiString(ALMemoSqlite3Query.Lines.Text)),
                                 StrToInt(ALEditSqlite3NBLoop.Text),
                                 StrToInt(ALEditSQLite3NbLoopBeforeCommit.Text),
                                 true);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    LSqlite3BenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    LSqlite3BenchmarkThread.Start;
    {$ELSE}
    aSqlite3BenchmarkThread.Resume;
    {$ENDIF}

  end;

end;

{*********************************************************************************}
procedure TForm1.ALEditButtonFindFileClick(Sender: TObject; AButtonIndex: Integer);
begin
  If OpenDialog1.Execute then (Sender as TcxButtonEdit).Text := OpenDialog1.FileName;
end;

{******************************************}
procedure TForm1.FormClick(Sender: TObject);
begin
  Windows.SetFocus(0);
end;

{*****************************************}
constructor TSqlite3BenchmarkThread.Create(
              CreateSuspended: Boolean;
              AOwner: TwinControl;
              aRank: integer;
              aSQL: AnsiString;
              aMaxLoop: integer;
              aNBLoopBeforeCommit: integer;
              aUpdateSQL: Boolean);
begin
  inherited Create(CreateSuspended);
  fSQL:= aSQL;
  fOn:= true;
  fMaxLoop:= aMaxLoop;
  if fMaxLoop <= 0 then fMaxLoop := MaxInt;
  fNBLoopBeforeCommit:= aNBLoopBeforeCommit;
  if fNBLoopBeforeCommit <= 0 then fNBLoopBeforeCommit := 1;
  FErrorMsg := '';
  FTotalExecuteTimeTaken := 0;
  FTotalCommitTimeTaken := 0;
  FTotalLoop := 0;
  fOwner := AOwner;
  fRank := aRank;
  fUpdateSQL := aUpdateSQL;
end;

{*****************************************}
destructor TSqlite3BenchmarkThread.Destroy;
begin
  fOn := False;
  Synchronize(UpdateGUI);
  inherited;
end;

{****************************************}
procedure TSqlite3BenchmarkThread.Execute;
Var LconnectionHandle: SQLite3;
    LStopWatch: TStopWatch;
    LLoopIndex: integer;
    LCommitLoopIndex: integer;
    LXMLDATA: TalXmlDocument;
    LFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
begin

  //init the aFormatSettings
  LFormatSettings := ALDefaultFormatSettingsA;

  //init the fNBLoopBeforeCommit
  if fNBLoopBeforeCommit <= 0 then fNBLoopBeforeCommit := 1;

  //create local object
  LXMLDATA := TALXmlDocument.create('root');
  Try

    //start the loop;
    LLoopIndex := 1;
    while LLoopIndex <= fMaxLoop do begin
      try

        //start the Transaction
        Tform1(fOwner).Sqlite3ConnectionPoolClient.TransactionStart(LconnectionHandle);
        try

          for LCommitLoopIndex := 1 to fNBLoopBeforeCommit do begin

            s1 := ALFastTagReplaceA(
                    fSQL,
                    '<#',
                    '>',
                    SQLFastTagReplaceFunct,
                    True,
                    nil);

            LXMLDATA.Clear('root');
            LStopWatch := TStopWatch.StartNew;
            if fUpdateSQL then Tform1(fOwner).Sqlite3ConnectionPoolClient.UpdateData(S1, LconnectionHandle)
            else Tform1(fOwner).Sqlite3ConnectionPoolClient.SelectData(
                   s1,
                   LXMLDATA.documentElement,
                   LFormatSettings,
                   LconnectionHandle);
            LStopWatch.Stop;
            FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + LStopWatch.Elapsed.TotalMilliseconds;

            inc(LLoopIndex);
            inc(FTotalLoop);
            if LLoopIndex > fMaxLoop then break;

          end;

          LStopWatch := TStopWatch.StartNew;
          Tform1(fOwner).Sqlite3ConnectionPoolClient.Transactioncommit(LconnectionHandle);
          LStopWatch.Stop;
          FTotalCommitTimeTaken := FTotalCommitTimeTaken + LStopWatch.Elapsed.TotalMilliseconds;

        Except
          Tform1(fOwner).Sqlite3ConnectionPoolClient.TransactionRollBack(LconnectionHandle);
          raise;
        end;

      Except
        on e: Exception do begin
          FErrorMsg := AnsiString(E.message);
          Synchronize(UpdateGUI);
          Exit;
        end;
      end;
      If ((not fUpdateSQL) and (Tform1(fOwner).ALButtonSqlite3LoopSelect.tag = 2)) or
         ((fUpdateSQL) and (Tform1(fOwner).ALButtonSqlite3LoopUpdate.tag = 2)) then begin
        Synchronize(UpdateGUI);
        Break;
      end;
    end;

  Finally
    LXMLDATA.free;
  End;
end;

{******************************************}
procedure TSqlite3BenchmarkThread.UpdateGUI;
begin
  if fUpdateSQL then begin
    TForm1(fOwner).TableViewThread.BeginUpdate;
    try
      if not fOn then begin
        dec(TForm1(fOwner).NBActiveThread);
        TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(TForm1(fOwner).NBActiveThread);
        TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,ALIntToStrA(frank) + ' (off)');
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).ALButtonSqlite3LoopUpdate.Tag := 0;
          TForm1(fOwner).ALButtonSqlite3LoopUpdate.Caption := 'Loop UPDATE';
          TForm1(fOwner).Sqlite3ConnectionPoolClient.Free;
          TForm1(fOwner).Sqlite3ConnectionPoolClient := nil;
          TForm1(fOwner).StatusBar1.Panels[2].Text := 'Total time taken: ' + String(ALFormatDateTimeA('hh:nn:ss.zzz',Now-TForm1(fOwner).LoopStartDateTime, ALDefaultFormatSettingsA));
        end;
      end;
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageExecuteTimeTaken.Index,FTotalExecuteTimeTaken / alifThen(FTotalLoop <> 0, FTotalLoop, 1) );
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / alifThen(FTotalLoop <> 0, FTotalLoop, 1) );
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadErrorMsg.Index,FErrorMsg);
      TForm1(fOwner).StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
    finally
      TForm1(fOwner).TableViewThread.EndUpdate;
    end;
  end
  else begin
    TForm1(fOwner).TableViewThread.BeginUpdate;
    try
      if not fOn then begin
        dec(TForm1(fOwner).NBActiveThread);
        TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(TForm1(fOwner).NBActiveThread);
        TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,ALIntToStrA(frank) + ' (off)');
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).ALButtonSqlite3LoopSelect.Tag := 0;
          TForm1(fOwner).ALButtonSqlite3LoopSelect.Caption := 'Loop SELECT';
          TForm1(fOwner).Sqlite3ConnectionPoolClient.Free;
          TForm1(fOwner).Sqlite3ConnectionPoolClient := nil;
          TForm1(fOwner).StatusBar1.Panels[2].Text := 'Total time taken: ' + String(ALFormatDateTimeA('hh:nn:ss.zzz',Now-TForm1(fOwner).LoopStartDateTime, ALDefaultFormatSettingsA));
        end;
      end;
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageExecuteTimeTaken.Index,FTotalExecuteTimeTaken / alifThen(FTotalLoop <> 0, FTotalLoop, 1) );
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / alifThen(FTotalLoop <> 0, FTotalLoop, 1) );
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadErrorMsg.Index,FErrorMsg);
      TForm1(fOwner).StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
    finally
      TForm1(fOwner).TableViewThread.EndUpdate;
    end;
  end;
  application.ProcessMessages;
end;

{***************************************}
constructor TMySqlBenchmarkThread.Create(
              CreateSuspended: Boolean;
              AOwner: TwinControl;
              aRank: integer;
              aSQL: AnsiString;
              aMaxLoop,
              aNBLoopBeforeCommit: integer;
              aUpdateSQL: Boolean);
begin
  inherited Create(CreateSuspended);
  fSQL:= aSQL;
  fOn:= true;
  fMaxLoop:= aMaxLoop;
  if fMaxLoop <= 0 then fMaxLoop := MaxInt;
  fNBLoopBeforeCommit:= aNBLoopBeforeCommit;
  if fNBLoopBeforeCommit <= 0 then fNBLoopBeforeCommit := 1;
  FErrorMsg := '';
  FTotalExecuteTimeTaken := 0;
  FTotalCommitTimeTaken := 0;
  FTotalLoop := 0;
  fOwner := AOwner;
  fRank := aRank;
  fUpdateSQL := aUpdateSQL;
end;

{***************************************}
destructor TMySqlBenchmarkThread.Destroy;
begin
  fOn := False;
  Synchronize(UpdateGUI);
  inherited;
end;

{**************************************}
procedure TMySqlBenchmarkThread.Execute;
Var LConnectionHandle: PMySql;
    LStopWatch: TStopWatch;
    LLoopIndex: integer;
    LCommitLoopIndex: integer;
    LXMLDATA: TalXmlDocument;
    LFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
begin

  //init the aFormatSettings
  LFormatSettings := ALDefaultFormatSettingsA;

  //init the fNBLoopBeforeCommit
  if fNBLoopBeforeCommit <= 0 then fNBLoopBeforeCommit := 1;

  //create local object
  LXMLDATA := TALXmlDocument.create('root');
  Try

    //start the loop;
    LLoopIndex := 1;
    while LLoopIndex <= fMaxLoop do begin
      try

        //start the Transaction
        LConnectionHandle := nil;
        Tform1(fOwner).MySqlConnectionPoolClient.TransactionStart(LConnectionHandle);
        try

          for LCommitLoopIndex := 1 to fNBLoopBeforeCommit do begin

            S1 := ALFastTagReplaceA(
                    fSQL,
                    '<#',
                    '>',
                    SQLFastTagReplaceFunct,
                    True,
                    nil);

            LXMLDATA.Clear('root');
            LStopWatch := TStopWatch.StartNew;
            if fUpdateSQL then Tform1(fOwner).MySqlConnectionPoolClient.UpdateData(S1, LConnectionHandle)
            else Tform1(fOwner).MySqlConnectionPoolClient.SelectData(
                   S1,
                   LXMLDATA.documentElement,
                   LFormatSettings,
                   LConnectionHandle);
            LStopWatch.Stop;
            FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + LStopWatch.Elapsed.TotalMilliseconds;
            inc(LLoopIndex);
            inc(FTotalLoop);
            if LLoopIndex > fMaxLoop then break;
          end;

          LStopWatch := TStopWatch.StartNew;
          Tform1(fOwner).MySqlConnectionPoolClient.Transactioncommit(LConnectionHandle);
          LStopWatch.Stop;
          FTotalCommitTimeTaken := FTotalCommitTimeTaken + LStopWatch.Elapsed.TotalMilliseconds;

        Except
          Tform1(fOwner).MySqlConnectionPoolClient.TransactionRollBack(LConnectionHandle);
          raise;
        end;

      Except
        on e: Exception do begin
          FErrorMsg := AnsiString(E.message);
          Synchronize(UpdateGUI);
          Exit;
        end;
      end;
      If ((not fUpdateSQL) and ((Tform1(fOwner).ALButtonMySqlLoopSelect.tag = 2) or (Tform1(fOwner).ALButtonSphinxLoopSelect.tag = 2))) or
         ((fUpdateSQL) and ((Tform1(fOwner).ALButtonMySqlLoopUpdate.tag = 2) or (Tform1(fOwner).ALButtonSphinxLoopUpdate.tag = 2))) then begin
        Synchronize(UpdateGUI);
        Break;
      end;
    end;

  Finally
    LXMLDATA.free;
  End;
end;

{****************************************}
procedure TMySqlBenchmarkThread.UpdateGUI;
begin
  if fUpdateSQL then begin
    TForm1(fOwner).TableViewThread.BeginUpdate;
    try
      if not fOn then begin
        dec(TForm1(fOwner).NBActiveThread);
        TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(TForm1(fOwner).NBActiveThread);
        TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,ALIntToStrA(frank) + ' (off)');
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).ALButtonMySqlLoopUpdate.Tag := 0;
          TForm1(fOwner).ALButtonMySqlLoopUpdate.Caption := 'Loop UPDATE';
          TForm1(fOwner).ALButtonSphinxLoopUpdate.Tag := 0;
          TForm1(fOwner).ALButtonSphinxLoopUpdate.Caption := 'Loop UPDATE';
          TForm1(fOwner).MySqlConnectionPoolClient.Free;
          TForm1(fOwner).MySqlConnectionPoolClient := nil;
          TForm1(fOwner).StatusBar1.Panels[2].Text := 'Total time taken: ' + String(ALFormatDateTimeA('hh:nn:ss.zzz',Now-TForm1(fOwner).LoopStartDateTime, ALDefaultFormatSettingsA));
        end;
      end;
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageExecuteTimeTaken.Index,FTotalExecuteTimeTaken / alifThen(FTotalLoop <> 0, FTotalLoop, 1) );
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / alifThen(FTotalLoop <> 0, FTotalLoop, 1) );
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadErrorMsg.Index,FErrorMsg);
      TForm1(fOwner).StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
    finally
      TForm1(fOwner).TableViewThread.EndUpdate;
    end;
  end
  else begin
    TForm1(fOwner).TableViewThread.BeginUpdate;
    try
      if not fOn then begin
        dec(TForm1(fOwner).NBActiveThread);
        TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(TForm1(fOwner).NBActiveThread);
        TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,ALIntToStrA(frank) + ' (off)');
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).ALButtonMySqlLoopSelect.Tag := 0;
          TForm1(fOwner).ALButtonMySqlLoopSelect.Caption := 'Loop SELECT';
          TForm1(fOwner).ALButtonSphinxLoopSelect.Tag := 0;
          TForm1(fOwner).ALButtonSphinxLoopSelect.Caption := 'Loop SELECT';
          TForm1(fOwner).MySqlConnectionPoolClient.Free;
          TForm1(fOwner).MySqlConnectionPoolClient := nil;
          TForm1(fOwner).StatusBar1.Panels[2].Text := 'Total time taken: ' + String(ALFormatDateTimeA('hh:nn:ss.zzz',Now-TForm1(fOwner).LoopStartDateTime, ALDefaultFormatSettingsA));
        end;
      end;
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageExecuteTimeTaken.Index,FTotalExecuteTimeTaken / alifThen(FTotalLoop <> 0, FTotalLoop, 1) );
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / alifThen(FTotalLoop <> 0, FTotalLoop, 1) );
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadErrorMsg.Index,FErrorMsg);
      TForm1(fOwner).StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
    finally
      TForm1(fOwner).TableViewThread.EndUpdate;
    end;
  end;
  application.ProcessMessages;
end;

{*******************************************}
constructor TMemcachedBenchmarkThread.Create(
              CreateSuspended: Boolean;
              AOwner: TwinControl;
              aRank: integer;
              aKey: AnsiString;
              aFlags: AnsiString;
              aExpTime: AnsiString;
              aData: AnsiString;
              aMaxLoop: integer;
              aCMD: ansiString);
begin
  inherited Create(CreateSuspended);
  fKey := aKey;
  fFlags := aFlags;
  fExpTime := aExpTime;
  fData := aData;
  fOn:= true;
  fMaxLoop:= aMaxLoop;
  if fMaxLoop <= 0 then fMaxLoop := MaxInt;
  FErrorMsg := '';
  FTotalExecuteTimeTaken := 0;
  FTotalLoop := 0;
  fOwner := AOwner;
  fRank := aRank;
  fCMD := aCMD;
end;

{*******************************************}
destructor TMemcachedBenchmarkThread.Destroy;
begin
  fOn := False;
  Synchronize(UpdateGUI);
  inherited;
end;

{******************************************}
procedure TMemcachedBenchmarkThread.Execute;
Var LStopWatch: TStopWatch;
    LKey: AnsiString;
    LFlags: integer;
    LExpTime: integer;
    LData: AnsiString;
begin

  //start the loop;
  while FTotalLoop < fMaxLoop do begin
    try

      //update the params
      LKey := ALFastTagReplaceA(
                fKey,
                '<#',
                '>',
                SQLFastTagReplaceFunct,
                True,
                nil);
      LFlags := alStrToInt(
                  ALFastTagReplaceA(
                    fflags,
                    '<#',
                    '>',
                    SQLFastTagReplaceFunct,
                    True,
                    nil));
      LExpTime := alStrToInt(
                    ALFastTagReplaceA(
                      fexpTime,
                      '<#',
                      '>',
                      SQLFastTagReplaceFunct,
                      True,
                      nil));
      LData := ALFastTagReplaceA(
                 fData,
                 '<#',
                 '>',
                 SQLFastTagReplaceFunct,
                 True,
                 nil);
      //update the data
      LStopWatch := TStopWatch.StartNew;
      if fCMD = 'SET' then Tform1(fOwner).MemcachedConnectionPoolClient._Set(LKey, LFlags, LExpTime, LData)
      else if fCMD = 'GET' then Tform1(fOwner).MemcachedConnectionPoolClient.Get(LKey, LFlags, LData)
      else if fCMD = 'INCR' then Tform1(fOwner).MemcachedConnectionPoolClient.Incr(LKey, 1)
      else if fCMD = 'DECR' then Tform1(fOwner).MemcachedConnectionPoolClient.Decr(LKey, 1);
      LStopWatch.Stop;
      FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + LStopWatch.Elapsed.TotalMilliseconds;

      //update FTotalLoop
      inc(FTotalLoop);

    Except
      on e: Exception do begin
        FErrorMsg := AnsiString(E.message);
        Synchronize(UpdateGUI);
        Exit;
      end;
    end;
    If (Tform1(fOwner).ALButtonMemcachedLoopGet.tag = 2) or
       (Tform1(fOwner).ALButtonMemcachedLoopSet.tag = 2) or
       (Tform1(fOwner).ALButtonMemcachedLoopIncr.tag = 2) or
       (Tform1(fOwner).ALButtonMemcachedLoopDecr.tag = 2) then begin
      Synchronize(UpdateGUI);
      Break;
    end;
  end;

end;

{********************************************}
procedure TMemcachedBenchmarkThread.UpdateGUI;
begin

  TForm1(fOwner).TableViewThread.BeginUpdate;
  try
    if not fOn then begin
      dec(TForm1(fOwner).NBActiveThread);
      TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(TForm1(fOwner).NBActiveThread);
      TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,ALIntToStrA(frank) + ' (off)');
      if TForm1(fOwner).NBActiveThread = 0 then begin
        if fCMD = 'SET' then begin
          TForm1(fOwner).ALButtonMemcachedLoopSET.Tag := 0;
          TForm1(fOwner).ALButtonMemcachedLoopSET.Caption := 'Loop SET';
        end
        else if fCMD = 'GET' then begin
          TForm1(fOwner).ALButtonMemcachedLoopGET.Tag := 0;
          TForm1(fOwner).ALButtonMemcachedLoopGET.Caption := 'Loop GET';
        end
        else if fCMD = 'INCR' then begin
          TForm1(fOwner).ALButtonMemcachedLoopINCR.Tag := 0;
          TForm1(fOwner).ALButtonMemcachedLoopINCR.Caption := 'Loop INCR';
        end
        else if fCMD = 'DECR' then begin
          TForm1(fOwner).ALButtonMemcachedLoopDECR.Tag := 0;
          TForm1(fOwner).ALButtonMemcachedLoopDECR.Caption := 'Loop DECR';
        end;
        TForm1(fOwner).MemcachedConnectionPoolClient.Free;
        TForm1(fOwner).MemcachedConnectionPoolClient := nil;
        TForm1(fOwner).StatusBar1.Panels[2].Text := 'Total time taken: ' + String(ALFormatDateTimeA('hh:nn:ss.zzz',Now-TForm1(fOwner).LoopStartDateTime, ALDefaultFormatSettingsA));
      end;
    end;
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageExecuteTimeTaken.Index,ALIfThen(FTotalLoop > 0, FTotalExecuteTimeTaken / alifThen(FTotalLoop <> 0, FTotalLoop, 1) , 0));
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,0/0);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadErrorMsg.Index,FErrorMsg);
    TForm1(fOwner).StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
  finally
    TForm1(fOwner).TableViewThread.EndUpdate;
  end;
  application.ProcessMessages;
end;

{*****************************************}
constructor TMongoDBBenchmarkThread.Create(
              CreateSuspended: Boolean;
              AOwner: TwinControl;
              aRank: integer;
              aMaxLoop: integer;
              acmd: AnsiString;
              afullCollectionName: AnsiString;
              aQuery: AnsiString;
              aSelector: AnsiString;
              aInsertIfNotFound: boolean;
              aMultiUpdate: boolean;
              aContinueOnError: boolean;
              aSingleRemove: boolean;
              aSkip: ansiString;
              aFirst: ansiString);
begin
  inherited Create(CreateSuspended);
  ffullCollectionName := afullCollectionName;
  fQuery := aQuery;
  fSelector := aSelector;
  fSkip := aSkip;
  fFirst := aFirst;
  fOn:= true;
  fMaxLoop:= aMaxLoop;
  if fMaxLoop <= 0 then fMaxLoop := MaxInt;
  FErrorMsg := '';
  FTotalExecuteTimeTaken := 0;
  FTotalLoop := 0;
  fOwner := AOwner;
  fRank := aRank;
  fCMD := aCMD;
  fUpdateFlags := [];
  fInsertFlags := [];
  fDeleteFlags := [];
  if aInsertIfNotFound then fUpdateFlags := fUpdateFlags + [ufUpsert];
  if aMultiUpdate then fUpdateFlags := fUpdateFlags + [ufMultiUpdate];
  if aContinueOnError then fInsertFlags := fInsertFlags + [ifContinueOnError];
  if aSingleRemove then fdeleteFlags := fdeleteFlags + [dfSingleRemove];
end;

{*****************************************}
destructor TMongoDBBenchmarkThread.Destroy;
begin
  fOn := False;
  Synchronize(UpdateGUI);
  inherited;
end;

{****************************************}
procedure TMongoDBBenchmarkThread.Execute;
Var LStopWatch: TStopWatch;
    LFullCollectionName: AnsiString;
    LQuery: AnsiString;
    LSelector: AnsiString;
    LSkip: integer;
    LFirst: integer;
    LJSONDATA: TALJSONNodeA;
begin

  LJSONDATA := TALJSONDocumentA.create;
  Try

    //start the loop;
    while FTotalLoop < fMaxLoop do begin
      try

        //update the params
        LFullCollectionName := ALFastTagReplaceA(
                                 fFullCollectionName,
                                 '<#',
                                 '>',
                                 SQLFastTagReplaceFunct,
                                 True,
                                 nil);
        LQuery := ALFastTagReplaceA(
                    fQuery,
                    '<#',
                    '>',
                    SQLFastTagReplaceFunct,
                    True,
                    nil);
        LSelector := ALFastTagReplaceA(
                       fSelector,
                       '<#',
                       '>',
                       SQLFastTagReplaceFunct,
                       True,
                       nil);
        if fSkip = '' then LSkip := 0
        else LSkip := ALStrToInt(
                        ALFastTagReplaceA(
                          fSkip,
                          '<#',
                          '>',
                          SQLFastTagReplaceFunct,
                          True,
                          nil));
        if fFirst = '' then LFirst := 0
        else LFirst := ALStrToInt(
                         ALFastTagReplaceA(
                           fFirst,
                           '<#',
                           '>',
                           SQLFastTagReplaceFunct,
                           True,
                           nil));

        //update the data
        LJSONDATA.ChildNodes.Clear;
        LStopWatch := TStopWatch.StartNew;
        if fCMD = 'SELECT' then Tform1(fOwner).MongoDBConnectionPoolClient.SelectData(
                                  LFullCollectionName,
                                  LQuery,
                                  LSelector,
                                  [],
                                  '', // rowtag
                                  LSkip,
                                  LFirst,
                                  LJSONDATA)
        else if fCMD = 'UPDATE' then Tform1(fOwner).MongoDBConnectionPoolClient.UpdateData(
                                       LFullCollectionName,
                                       LQuery,
                                       LSelector,
                                       fUpdateFlags)
        else if fCMD = 'INSERT' then Tform1(fOwner).MongoDBConnectionPoolClient.InsertData(
                                       LFullCollectionName,
                                       LQuery,
                                       fInsertFlags)
        else if fCMD = 'DELETE' then Tform1(fOwner).MongoDBConnectionPoolClient.DeleteData(
                                       LFullCollectionName,
                                       LQuery,
                                       fdeleteFlags);
        LStopWatch.Stop;
        FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + LStopWatch.Elapsed.TotalMilliseconds;

        //update FTotalLoop
        inc(FTotalLoop);

      Except
        on e: Exception do begin
          FErrorMsg := AnsiString(E.message);
          Synchronize(UpdateGUI);
          Exit;
        end;
      end;
      If (Tform1(fOwner).ALButtonMongoDBLoopSELECT.tag = 2) or
         (Tform1(fOwner).ALButtonMongoDBLoopINSERT.tag = 2) or
         (Tform1(fOwner).ALButtonMongoDBLoopUPDATE.tag = 2) or
         (Tform1(fOwner).ALButtonMongoDBLoopDELETE.tag = 2) then begin
        Synchronize(UpdateGUI);
        Break;
      end;
    end;

  Finally
    LJSONDATA.Free;
  End;

end;

{******************************************}
procedure TMongoDBBenchmarkThread.UpdateGUI;
begin

  TForm1(fOwner).TableViewThread.BeginUpdate;
  try
    if not fOn then begin
      dec(TForm1(fOwner).NBActiveThread);
      TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(TForm1(fOwner).NBActiveThread);
      TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,ALIntToStrA(frank) + ' (off)');
      if TForm1(fOwner).NBActiveThread = 0 then begin
        if fCMD = 'SELECT' then begin
          TForm1(fOwner).ALButtonMongoDBLoopSELECT.Tag := 0;
          TForm1(fOwner).ALButtonMongoDBLoopSELECT.Caption := 'Loop SELECT';
        end
        else if fCMD = 'UPDATE' then begin
          TForm1(fOwner).ALButtonMongoDBLoopUPDATE.Tag := 0;
          TForm1(fOwner).ALButtonMongoDBLoopUPDATE.Caption := 'Loop UPDATE';
        end
        else if fCMD = 'INSERT' then begin
          TForm1(fOwner).ALButtonMongoDBLoopINSERT.Tag := 0;
          TForm1(fOwner).ALButtonMongoDBLoopINSERT.Caption := 'Loop INSERT';
        end
        else if fCMD = 'DELETE' then begin
          TForm1(fOwner).ALButtonMongoDBLoopDELETE.Tag := 0;
          TForm1(fOwner).ALButtonMongoDBLoopDELETE.Caption := 'Loop DELETE';
        end;
        TForm1(fOwner).MongoDBConnectionPoolClient.Free;
        TForm1(fOwner).MongoDBConnectionPoolClient := nil;
        TForm1(fOwner).StatusBar1.Panels[2].Text := 'Total time taken: ' + String(ALFormatDateTimeA('hh:nn:ss.zzz',Now-TForm1(fOwner).LoopStartDateTime, ALDefaultFormatSettingsA));
      end;
    end;
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageExecuteTimeTaken.Index,ALIfThen(FTotalLoop > 0, FTotalExecuteTimeTaken / alifThen(FTotalLoop <> 0, FTotalLoop, 1) , 0));
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,0/0);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadErrorMsg.Index,FErrorMsg);
    TForm1(fOwner).StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
  finally
    TForm1(fOwner).TableViewThread.EndUpdate;
  end;
  application.ProcessMessages;
end;

{**************************************************************}
procedure TForm1.ALButtonMemcachedLoopGetClick(Sender: TObject);
Var LMemcachedBenchmarkThread: TMemcachedBenchmarkThread;
    i: integer;
begin

  {init button action}
  If ALButtonMemCachedLoopGet.tag = 0 then begin
    ALButtonMemCachedLoopGet.Tag := 1;
    ALButtonMemCachedLoopGet.Caption := 'STOP';
  end
  else If ALButtonMemCachedLoopGet.tag = 1 then begin
    ALButtonMemCachedLoopGet.Tag := 2;
    ALButtonMemCachedLoopGet.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := ALStrToInt(AnsiString(ALEditMemcachedNBThread.Text));

  //create the fMemcachedConnectionPoolClient
  if not assigned(MemcachedConnectionPoolClient) then
    MemcachedConnectionPoolClient := TALMemcachedConnectionPoolClient.Create(
                                       AnsiString(ALEditMemcachedHost.Text),
                                       StrToInt(ALEditMemcachedPort.Text));

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditMemcachedNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    LMemcachedBenchmarkThread := TMemcachedBenchmarkThread.Create(
                                   True,
                                   self,
                                   i,
                                   AnsiString(ALEditMemCachedKey.Text),
                                   AnsiString(ALEditMemCachedflags.Text),
                                   AnsiString(ALEditMemCachedexpTime.Text),
                                   AnsiString(ALMemoMemCachedData.Lines.Text),
                                   StrToInt(ALEditMemcachedNBLoop.Text),
                                   'GET');
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    LMemcachedBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    LMemcachedBenchmarkThread.Start;
    {$ELSE}
    aMemcachedBenchmarkThread.Resume;
    {$ENDIF}
  end;

end;

{**************************************************************}
procedure TForm1.ALButtonMemcachedLoopSetClick(Sender: TObject);
Var LMemcachedBenchmarkThread: TMemcachedBenchmarkThread;
    i: integer;
begin

  {init button action}
  If ALButtonMemCachedLoopSet.tag = 0 then begin
    ALButtonMemCachedLoopSet.Tag := 1;
    ALButtonMemCachedLoopSet.Caption := 'STOP';
  end
  else If ALButtonMemCachedLoopSet.tag = 1 then begin
    ALButtonMemCachedLoopSet.Tag := 2;
    ALButtonMemCachedLoopSet.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := ALStrToInt(AnsiString(ALEditMemcachedNBThread.Text));

  //create the fMemcachedConnectionPoolClient
  if not assigned(MemcachedConnectionPoolClient) then
    MemcachedConnectionPoolClient := TALMemcachedConnectionPoolClient.Create(
                                       AnsiString(ALEditMemcachedHost.Text),
                                       StrToInt(ALEditMemcachedPort.Text));

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditMemcachedNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    LMemcachedBenchmarkThread := TMemcachedBenchmarkThread.Create(
                                   True,
                                   self,
                                   i,
                                   AnsiString(ALEditMemCachedKey.Text),
                                   AnsiString(ALEditMemCachedflags.Text),
                                   AnsiString(ALEditMemCachedexpTime.Text),
                                   AnsiString(ALMemoMemCachedData.Lines.Text),
                                   StrToInt(ALEditMemcachedNBLoop.Text),
                                   'SET');
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    LMemcachedBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    LMemcachedBenchmarkThread.Start;
    {$ELSE}
    aMemcachedBenchmarkThread.Resume;
    {$ENDIF}
  end;

end;

{***************************************************************}
procedure TForm1.ALButtonMemcachedLoopIncrClick(Sender: TObject);
Var LMemcachedBenchmarkThread: TMemcachedBenchmarkThread;
    i: integer;
begin

  {init button action}
  If ALButtonMemCachedLoopINCR.tag = 0 then begin
    ALButtonMemCachedLoopINCR.Tag := 1;
    ALButtonMemCachedLoopINCR.Caption := 'STOP';
  end
  else If ALButtonMemCachedLoopINCR.tag = 1 then begin
    ALButtonMemCachedLoopINCR.Tag := 2;
    ALButtonMemCachedLoopINCR.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := ALStrToInt(AnsiString(ALEditMemcachedNBThread.Text));

  //create the fMemcachedConnectionPoolClient
  if not assigned(MemcachedConnectionPoolClient) then
    MemcachedConnectionPoolClient := TALMemcachedConnectionPoolClient.Create(
                                       AnsiString(ALEditMemcachedHost.Text),
                                       StrToInt(ALEditMemcachedPort.Text));

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditMemcachedNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    LMemcachedBenchmarkThread := TMemcachedBenchmarkThread.Create(
                                   True,
                                   self,
                                   i,
                                   AnsiString(ALEditMemCachedKey.Text),
                                   AnsiString(ALEditMemCachedflags.Text),
                                   AnsiString(ALEditMemCachedexpTime.Text),
                                   AnsiString(ALMemoMemCachedData.Lines.Text),
                                   StrToInt(ALEditMemcachedNBLoop.Text),
                                   'INCR');
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    LMemcachedBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    LMemcachedBenchmarkThread.Start;
    {$ELSE}
    aMemcachedBenchmarkThread.Resume;
    {$ENDIF}
  end;

end;

{***************************************************************}
procedure TForm1.ALButtonMemcachedLoopDecrClick(Sender: TObject);
Var LMemcachedBenchmarkThread: TMemcachedBenchmarkThread;
    i: integer;
begin

  {init button action}
  If ALButtonMemCachedLoopDECR.tag = 0 then begin
    ALButtonMemCachedLoopDECR.Tag := 1;
    ALButtonMemCachedLoopDECR.Caption := 'STOP';
  end
  else If ALButtonMemCachedLoopDECR.tag = 1 then begin
    ALButtonMemCachedLoopDECR.Tag := 2;
    ALButtonMemCachedLoopDECR.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := ALStrToInt(AnsiString(ALEditMemcachedNBThread.Text));

  //create the fMemcachedConnectionPoolClient
  if not assigned(MemcachedConnectionPoolClient) then
    MemcachedConnectionPoolClient := TALMemcachedConnectionPoolClient.Create(
                                       AnsiString(ALEditMemcachedHost.Text),
                                       StrToInt(ALEditMemcachedPort.Text));

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditMemcachedNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    LMemcachedBenchmarkThread := TMemcachedBenchmarkThread.Create(
                                   True,
                                   self,
                                   i,
                                   AnsiString(ALEditMemCachedKey.Text),
                                   AnsiString(ALEditMemCachedflags.Text),
                                   AnsiString(ALEditMemCachedexpTime.Text),
                                   AnsiString(ALMemoMemCachedData.Lines.Text),
                                   StrToInt(ALEditMemcachedNBLoop.Text),
                                   'DECR');
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    LMemcachedBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    LMemcachedBenchmarkThread.Start;
    {$ELSE}
    aMemcachedBenchmarkThread.Resume;
    {$ENDIF}
  end;

end;

{**********************************************************}
procedure TForm1.ALButtonMemcachedGetClick(Sender: TObject);
Var LMemCachedClient: TAlMemCachedClient;
    LStopWatch: TStopWatch;
    LKey: AnsiString;
    Lflags: integer;
    LData: AnsiString;
    LFound: boolean;
begin
  Screen.Cursor := CrHourGlass;
  try

    LMemCachedClient := TAlMemCachedClient.create;
    Try
      LMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
      LKey := ALFastTagReplaceA(
                AnsiString(ALEditMemCachedKey.Text),
                '<#',
                '>',
                SQLFastTagReplaceFunct,
                True,
                nil);
      LStopWatch := TStopWatch.StartNew;
      LFound := LMemCachedClient.get(LKey, Lflags, LData);
      LStopWatch.Stop;
      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,LStopWatch.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.Clear;
      if LFound then begin
        AlMemoResult.Lines.Add('Found: true');
        AlMemoResult.Lines.Add('Flags: ' + intToStr(Lflags));
        AlMemoResult.Lines.Add('Data: ' + string(LData));
      end
      else AlMemoResult.Lines.Add('Found: false');
      LMemCachedClient.disconnect;
    Finally
      LMemCachedClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{**********************************************************}
procedure TForm1.ALButtonMemcachedSetClick(Sender: TObject);
Var LMemCachedClient: TAlMemCachedClient;
    LStopWatch: TStopWatch;
    LKey: AnsiString;
    Lflags: integer;
    LExpTime: integer;
    LData: AnsiString;
begin
  Screen.Cursor := CrHourGlass;
  try

    LMemCachedClient := TAlMemCachedClient.create;
    Try
      LMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
      LKey := ALFastTagReplaceA(
                AnsiString(ALEditMemCachedKey.Text),
                '<#',
                '>',
                SQLFastTagReplaceFunct,
                True,
                nil);
      Lflags := alStrToInt(
                  ALFastTagReplaceA(
                    AnsiString(ALEditMemCachedflags.Text),
                    '<#',
                    '>',
                    SQLFastTagReplaceFunct,
                    True,
                    nil));
      LExpTime := alStrToInt(
                    ALFastTagReplaceA(
                      AnsiString(ALEditMemCachedexpTime.Text),
                      '<#',
                      '>',
                      SQLFastTagReplaceFunct,
                      True,
                      nil));
      LData := ALFastTagReplaceA(
                 AnsiString(ALMemoMemCachedData.Lines.Text),
                 '<#',
                 '>',
                 SQLFastTagReplaceFunct,
                 True,
                 nil);
      LStopWatch := TStopWatch.StartNew;
      LMemCachedClient._set(LKey, Lflags, LExpTime, LData);
      LStopWatch.Stop;
      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,LStopWatch.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.Text := '';
      LMemCachedClient.disconnect;
    Finally
      LMemCachedClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{*************************************************************}
procedure TForm1.ALButtonMemcachedDeleteClick(Sender: TObject);
Var LMemCachedClient: TAlMemCachedClient;
    LStopWatch: TStopWatch;
    LKey: AnsiString;
    LFound: boolean;
begin
  Screen.Cursor := CrHourGlass;
  try

    LMemCachedClient := TAlMemCachedClient.create;
    Try
      LMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
      LKey := ALFastTagReplaceA(
                AnsiString(ALEditMemCachedKey.Text),
                '<#',
                '>',
                SQLFastTagReplaceFunct,
                True,
                nil);
      LStopWatch := TStopWatch.StartNew;
      LFound := LMemCachedClient.delete(LKey);
      LStopWatch.Stop;
      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,LStopWatch.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.Clear;
      if LFound then AlMemoResult.Lines.Add('Deleted: true')
      else AlMemoResult.Lines.Add('Deleted: false');
      LMemCachedClient.disconnect;
    Finally
      LMemCachedClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{************************************************************}
procedure TForm1.ALButtonMemcachedStatsClick(Sender: TObject);
var LMemCachedClient: TAlMemCachedClient;
begin
  LMemCachedClient := TAlMemCachedClient.create;
  try
    LMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
    ALMemoResult.Clear;
    ALMemoResult.Lines.Add('stats');
    ALMemoResult.Lines.Add('-----');
    ALMemoResult.Lines.Add(String(LMemCachedClient.stats('')));
    LMemCachedClient.disconnect;
  finally
    LMemCachedClient.Free;
  end;
end;

{*****************************************************************}
procedure TForm1.ALButtonMemcachedStatsItemsClick(Sender: TObject);
var LMemCachedClient: TAlMemCachedClient;
begin
  LMemCachedClient := TAlMemCachedClient.create;
  try
    LMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
    ALMemoResult.Clear;
    ALMemoResult.Lines.Add('stats items');
    ALMemoResult.Lines.Add('-----------');
    ALMemoResult.Lines.Add(String(LMemCachedClient.stats('items')));
    LMemCachedClient.disconnect;
  finally
    LMemCachedClient.Free;
  end;
end;

{********************************************************************}
procedure TForm1.ALButtonMemcachedStatsSettingsClick(Sender: TObject);
var LMemCachedClient: TAlMemCachedClient;
begin
  LMemCachedClient := TAlMemCachedClient.create;
  try
    LMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
    ALMemoResult.Clear;
    ALMemoResult.Lines.Add('stats settings');
    ALMemoResult.Lines.Add('--------------');
    ALMemoResult.Lines.Add(String(LMemCachedClient.stats('settings')));
    LMemCachedClient.disconnect;
  finally
    LMemCachedClient.Free;
  end;
end;

{*****************************************************************}
procedure TForm1.ALButtonMemcachedStatsSizesClick(Sender: TObject);
var LMemCachedClient: TAlMemCachedClient;
begin
  LMemCachedClient := TAlMemCachedClient.create;
  try
    LMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
    ALMemoResult.Clear;
    ALMemoResult.Lines.Add('stats sizes');
    ALMemoResult.Lines.Add('-----------');
    ALMemoResult.Lines.Add(String(LMemCachedClient.stats('sizes')));
    LMemCachedClient.disconnect;
  finally
    LMemCachedClient.Free;
  end;
end;

{*****************************************************************}
procedure TForm1.ALButtonMemcachedStatsSlabsClick(Sender: TObject);
var LMemCachedClient: TAlMemCachedClient;
begin
  LMemCachedClient := TAlMemCachedClient.create;
  try
    LMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
    ALMemoResult.Clear;
    ALMemoResult.Lines.Add('stats slabs');
    ALMemoResult.Lines.Add('-----------');
    ALMemoResult.Lines.Add(String(LMemCachedClient.stats('slabs')));
    LMemCachedClient.disconnect;
  finally
    LMemCachedClient.Free;
  end;
end;

{**************************************************************}
procedure TForm1.ALButtonMemcachedVersionClick(Sender: TObject);
var LMemCachedClient: TAlMemCachedClient;
begin
  LMemCachedClient := TAlMemCachedClient.create;
  try
    LMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
    ALMemoResult.Clear;
    ALMemoResult.Lines.Add('version');
    ALMemoResult.Lines.Add('-------');
    ALMemoResult.Lines.Add(String(LMemCachedClient.version));
    LMemCachedClient.disconnect;
  finally
    LMemCachedClient.Free;
  end;
end;

{***********************************************************}
procedure TForm1.ALButtonMongoDBSelectClick(Sender: TObject);
Var LMongoDBClient: TAlMongoDBClient;
    LJSONDATA: TALJSONNodeA;
    LStopWatch: TstopWatch;
    LFlags: TALMongoDBClientSelectDataFlags;
begin

  Screen.Cursor := CrHourGlass;
  try

    LMongoDBClient := TAlMongoDBClient.create;
    Try
      LMongoDBClient.Connect(AnsiString(ALEditMongoDBHost.Text), StrToInt(ALEditMongoDBPort.Text));

      LJSONDATA := TALJSONDocumentA.create;
      Try

        LFlags := [];
        if CheckGroupMongoDBSelectFlags.States[0] = cbsChecked then LFlags := LFlags + [sfSlaveOk];
        if CheckGroupMongoDBSelectFlags.States[1] = cbsChecked then LFlags := LFlags + [sfPartial];

        LStopWatch:= TstopWatch.StartNew;
        LMongoDBClient.SelectData(
          ALFastTagReplaceA(
            AnsiString(EditMongoDBFullCollectionName.Text),
            '<#',
            '>',
            SQLFastTagReplaceFunct,
            True,
            nil),
          ALFastTagReplaceA(
            AnsiString(MemoMongoDBQuery.Lines.Text),
            '<#',
            '>',
            SQLFastTagReplaceFunct,
            True,
            nil),
          ALFastTagReplaceA(
            AnsiString(MemoMongoDBSelector.Lines.Text),
            '<#',
            '>',
            SQLFastTagReplaceFunct,
            True,
            nil),
          LFlags,
          'rec',
          ALstrToInt(
            ALFastTagReplaceA(
              AnsiString(EditMongoDBSkip.Text),
              '<#',
              '>',
              SQLFastTagReplaceFunct,
              True,
              nil)),
          ALstrToInt(
            ALFastTagReplaceA(
              AnsiString(EditMongoDBFirst.Text),
              '<#',
              '>',
              SQLFastTagReplaceFunct,
              True,
              nil)),
          LJSONDATA);
        LStopWatch.Stop;

        TableViewThread.DataController.RecordCount := 1;
        TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
        TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
        TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,LStopWatch.Elapsed.TotalMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
        StatusBar1.Panels[2].Text := '';
        AlMemoResult.Lines.Text := String(LJSONDATA.JSON);

      Finally
        LJSONDATA.free;
      End;

    Finally
      LMongoDBClient.disconnect;
      LMongoDBClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{***********************************************************}
procedure TForm1.ALButtonMongoDBINSERTClick(Sender: TObject);
Var LMongoDBClient: TAlMongoDBClient;
    LStopWatch: TstopWatch;
    Lflags: TALMongoDBClientInsertDataFlags;
begin

  Screen.Cursor := CrHourGlass;
  try

    LMongoDBClient := TAlMongoDBClient.create;
    Try
      LMongoDBClient.Connect(AnsiString(ALEditMongoDBHost.Text), StrToInt(ALEditMongoDBPort.Text));

      Lflags := [];
      if CheckGroupMongoDBINSERTFlags.States[0] = cbsChecked then Lflags := Lflags + [ifContinueOnError];

      LStopWatch:= TstopWatch.StartNew;
      LMongoDBClient.InsertData(
        ALFastTagReplaceA(
          AnsiString(EditMongoDBFullCollectionName.Text),
          '<#',
          '>',
          SQLFastTagReplaceFunct,
          True,
          nil),
        ALFastTagReplaceA(
          AnsiString(MemoMongoDBQuery.Lines.Text),
          '<#',
          '>',
          SQLFastTagReplaceFunct,
          True,
          nil),
        Lflags);
      LStopWatch.Stop;

      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,LStopWatch.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.Clear;

    Finally
      LMongoDBClient.disconnect;
      LMongoDBClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{***********************************************************}
procedure TForm1.ALButtonMongoDBUpdateClick(Sender: TObject);
Var LMongoDBClient: TAlMongoDBClient;
    LStopWatch: TstopWatch;
    LFlags: TALMongoDBClientUpdateDataFlags;
    LNumberOfDocumentsUpdatedOrRemoved: integer;
    LUpdatedExisting: Boolean;
    LUpserted: ansiString;
begin

  Screen.Cursor := CrHourGlass;
  try

    LMongoDBClient := TAlMongoDBClient.create;
    Try
      LMongoDBClient.Connect(AnsiString(ALEditMongoDBHost.Text), StrToInt(ALEditMongoDBPort.Text));

      LFlags := [];
      if CheckGroupMongoDBUpdateFlags.States[0] = cbsChecked then LFlags := LFlags + [ufUpsert];
      if CheckGroupMongoDBUpdateFlags.States[1] = cbsChecked then LFlags := LFlags + [ufMultiUpdate];


      LStopWatch:= TstopWatch.StartNew;
      LMongoDBClient.UpdateData(
        ALFastTagReplaceA(
          AnsiString(EditMongoDBFullCollectionName.Text),
          '<#',
          '>',
          SQLFastTagReplaceFunct,
          True,
          nil),
        ALFastTagReplaceA(
          AnsiString(MemoMongoDBQuery.Lines.Text),
          '<#',
          '>',
          SQLFastTagReplaceFunct,
          True,
          nil),
        ALFastTagReplaceA(
          AnsiString(MemoMongoDBSelector.Lines.Text),
          '<#',
          '>',
          SQLFastTagReplaceFunct,
          True,
          nil),
        LFlags,
        LNumberOfDocumentsUpdatedOrRemoved,
        LUpdatedExisting,
        LUpserted);
      LStopWatch.Stop;

      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,LStopWatch.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.clear;
      AlMemoResult.Lines.add('Number Of Documents Updated: ' + intToStr(LNumberOfDocumentsUpdatedOrRemoved));
      AlMemoResult.Lines.add('updated Existing: ' + String(ALBoolToStrA(LUpdatedExisting)));
      AlMemoResult.Lines.add('upserted: ' + string(ALBinToHexA(LUpserted)));

    Finally
      LMongoDBClient.disconnect;
      LMongoDBClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{***********************************************************}
procedure TForm1.ALButtonMongoDBDeleteClick(Sender: TObject);
Var LMongoDBClient: TAlMongoDBClient;
    LStopWatch: TstopWatch;
    LFlags: TALMongoDBClientDeleteDataFlags;
    LNumberOfDocumentsUpdatedOrRemoved: integer;
begin

  Screen.Cursor := CrHourGlass;
  try

    LMongoDBClient := TAlMongoDBClient.create;
    Try
      LMongoDBClient.Connect(AnsiString(ALEditMongoDBHost.Text), StrToInt(ALEditMongoDBPort.Text));

      LFlags := [];
      if CheckGroupMongoDBDELETEFlags.States[0] = cbsChecked then LFlags := LFlags + [dfSingleRemove];

      LStopWatch:= TstopWatch.StartNew;
      LMongoDBClient.DeleteData(
        ALFastTagReplaceA(
          AnsiString(EditMongoDBFullCollectionName.Text),
          '<#',
          '>',
          SQLFastTagReplaceFunct,
          True,
          nil),
        ALFastTagReplaceA(
          AnsiString(MemoMongoDBQuery.Lines.Text),
          '<#',
          '>',
          SQLFastTagReplaceFunct,
          True,
          nil),
        LFlags,
        LNumberOfDocumentsUpdatedOrRemoved);
      LStopWatch.Stop;

      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,LStopWatch.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.clear;
      AlMemoResult.Lines.add('Number Of Documents removed: ' + intToStr(LNumberOfDocumentsUpdatedOrRemoved));

    Finally
      LMongoDBClient.disconnect;
      LMongoDBClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{***************************************************************}
procedure TForm1.ALButtonMongoDBLOOPSELECTClick(Sender: TObject);
Var LMongoDBBenchmarkThread: TMongoDBBenchmarkThread;
    i: integer;
begin

  {init button action}
  If ALButtonMongoDBLoopSelect.tag = 0 then begin
    ALButtonMongoDBLoopSelect.Tag := 1;
    ALButtonMongoDBLoopSelect.Caption := 'STOP';
  end
  else If ALButtonMongoDBLoopSelect.tag = 1 then begin
    ALButtonMongoDBLoopSelect.Tag := 2;
    ALButtonMongoDBLoopSelect.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := StrToInt(ALEditMongoDBNBThread.Text);

  //create the fMongoDBConnectionPoolClient
  if not assigned(MongoDBConnectionPoolClient) then begin
    MongoDBConnectionPoolClient := TALMongoDBConnectionPoolClient.Create(
                                     AnsiString(ALEditMongoDBHost.Text),
                                     StrToInt(ALEditMongoDBPort.Text));
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditMongoDBNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    LMongoDBBenchmarkThread := TMongoDBBenchmarkThread.Create(
                                 True,
                                 self,
                                 i,
                                 StrToInt(ALEditMongoDBNBLoop.Text),
                                 'SELECT',
                                 ALTrim(AnsiString(EditMongoDBfullCollectionName.Text)),
                                 ALTrim(AnsiString(MemoMongoDBQuery.Lines.Text)),
                                 ALTrim(AnsiString(MemoMongoDBSelector.Lines.Text)),
                                 CheckGroupMongoDBUpdateFlags.States[0] = cbsChecked,
                                 CheckGroupMongoDBUpdateFlags.States[1] = cbsChecked,
                                 CheckGroupMongoDBINSERTFlags.States[0] = cbsChecked,
                                 CheckGroupMongoDBDELETEFlags.States[0] = cbsChecked,
                                 ALTrim(AnsiString(EditMongoDBSkip.Text)),
                                 ALTrim(AnsiString(EditMongoDBFirst.Text)));

    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    LMongoDBBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    LMongoDBBenchmarkThread.Start;
    {$ELSE}
    aMongoDBBenchmarkThread.Resume;
    {$ENDIF}

  end;

end;

{***************************************************************}
procedure TForm1.ALButtonMongoDBLOOPUPDATEClick(Sender: TObject);
Var LMongoDBBenchmarkThread: TMongoDBBenchmarkThread;
    i: integer;
begin

  {init button action}
  If ALButtonMongoDBLoopUpdate.tag = 0 then begin
    ALButtonMongoDBLoopUpdate.Tag := 1;
    ALButtonMongoDBLoopUpdate.Caption := 'STOP';
  end
  else If ALButtonMongoDBLoopUpdate.tag = 1 then begin
    ALButtonMongoDBLoopUpdate.Tag := 2;
    ALButtonMongoDBLoopUpdate.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := StrToInt(ALEditMongoDBNBThread.Text);

  //create the fMongoDBConnectionPoolClient
  if not assigned(MongoDBConnectionPoolClient) then begin
    MongoDBConnectionPoolClient := TALMongoDBConnectionPoolClient.Create(
                                     AnsiString(ALEditMongoDBHost.Text),
                                     StrToInt(ALEditMongoDBPort.Text));
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditMongoDBNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    LMongoDBBenchmarkThread := TMongoDBBenchmarkThread.Create(
                                 True,
                                 self,
                                 i,
                                 StrToInt(ALEditMongoDBNBLoop.Text),
                                 'UPDATE',
                                 ALTrim(AnsiString(EditMongoDBfullCollectionName.Text)),
                                 ALTrim(AnsiString(MemoMongoDBQuery.Lines.Text)),
                                 ALTrim(AnsiString(MemoMongoDBSelector.Lines.Text)),
                                 CheckGroupMongoDBUpdateFlags.States[0] = cbsChecked,
                                 CheckGroupMongoDBUpdateFlags.States[1] = cbsChecked,
                                 CheckGroupMongoDBINSERTFlags.States[0] = cbsChecked,
                                 CheckGroupMongoDBDELETEFlags.States[0] = cbsChecked,
                                 ALTrim(AnsiString(EditMongoDBSkip.Text)),
                                 ALTrim(AnsiString(EditMongoDBFirst.Text)));

    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    LMongoDBBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    LMongoDBBenchmarkThread.Start;
    {$ELSE}
    aMongoDBBenchmarkThread.Resume;
    {$ENDIF}

  end;

end;

{***************************************************************}
procedure TForm1.ALButtonMongoDBLOOPINSERTClick(Sender: TObject);
Var LMongoDBBenchmarkThread: TMongoDBBenchmarkThread;
    i: integer;
begin

  {init button action}
  If ALButtonMongoDBLoopInsert.tag = 0 then begin
    ALButtonMongoDBLoopInsert.Tag := 1;
    ALButtonMongoDBLoopInsert.Caption := 'STOP';
  end
  else If ALButtonMongoDBLoopInsert.tag = 1 then begin
    ALButtonMongoDBLoopInsert.Tag := 2;
    ALButtonMongoDBLoopInsert.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := StrToInt(ALEditMongoDBNBThread.Text);

  //create the fMongoDBConnectionPoolClient
  if not assigned(MongoDBConnectionPoolClient) then begin
    MongoDBConnectionPoolClient := TALMongoDBConnectionPoolClient.Create(
                                     AnsiString(ALEditMongoDBHost.Text),
                                     StrToInt(ALEditMongoDBPort.Text));
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditMongoDBNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    LMongoDBBenchmarkThread := TMongoDBBenchmarkThread.Create(
                                 True,
                                 self,
                                 i,
                                 StrToInt(ALEditMongoDBNBLoop.Text),
                                 'INSERT',
                                 ALTrim(AnsiString(EditMongoDBfullCollectionName.Text)),
                                 ALTrim(AnsiString(MemoMongoDBQuery.Lines.Text)),
                                 ALTrim(AnsiString(MemoMongoDBSelector.Lines.Text)),
                                 CheckGroupMongoDBUpdateFlags.States[0] = cbsChecked,
                                 CheckGroupMongoDBUpdateFlags.States[1] = cbsChecked,
                                 CheckGroupMongoDBINSERTFlags.States[0] = cbsChecked,
                                 CheckGroupMongoDBDELETEFlags.States[0] = cbsChecked,
                                 ALTrim(AnsiString(EditMongoDBSkip.Text)),
                                 ALTrim(AnsiString(EditMongoDBFirst.Text)));

    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    LMongoDBBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    LMongoDBBenchmarkThread.Start;
    {$ELSE}
    aMongoDBBenchmarkThread.Resume;
    {$ENDIF}

  end;

end;

{***************************************************************}
procedure TForm1.ALButtonMongoDBLOOPDELETEClick(Sender: TObject);
Var LMongoDBBenchmarkThread: TMongoDBBenchmarkThread;
    i: integer;
begin

  {init button action}
  If ALButtonMongoDBLoopDelete.tag = 0 then begin
    ALButtonMongoDBLoopDelete.Tag := 1;
    ALButtonMongoDBLoopDelete.Caption := 'STOP';
  end
  else If ALButtonMongoDBLoopDelete.tag = 1 then begin
    ALButtonMongoDBLoopDelete.Tag := 2;
    ALButtonMongoDBLoopDelete.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := StrToInt(ALEditMongoDBNBThread.Text);

  //create the fMongoDBConnectionPoolClient
  if not assigned(MongoDBConnectionPoolClient) then begin
    MongoDBConnectionPoolClient := TALMongoDBConnectionPoolClient.Create(
                                     AnsiString(ALEditMongoDBHost.Text),
                                     StrToInt(ALEditMongoDBPort.Text));
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditMongoDBNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    LMongoDBBenchmarkThread := TMongoDBBenchmarkThread.Create(
                                 True,
                                 self,
                                 i,
                                 StrToInt(ALEditMongoDBNBLoop.Text),
                                 'DELETE',
                                 ALTrim(AnsiString(EditMongoDBfullCollectionName.Text)),
                                 ALTrim(AnsiString(MemoMongoDBQuery.Lines.Text)),
                                 ALTrim(AnsiString(MemoMongoDBSelector.Lines.Text)),
                                 CheckGroupMongoDBUpdateFlags.States[0] = cbsChecked,
                                 CheckGroupMongoDBUpdateFlags.States[1] = cbsChecked,
                                 CheckGroupMongoDBINSERTFlags.States[0] = cbsChecked,
                                 CheckGroupMongoDBDELETEFlags.States[0] = cbsChecked,
                                 ALTrim(AnsiString(EditMongoDBSkip.Text)),
                                 ALTrim(AnsiString(EditMongoDBFirst.Text)));

    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    LMongoDBBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    LMongoDBBenchmarkThread.Start;
    {$ELSE}
    aMongoDBBenchmarkThread.Resume;
    {$ENDIF}

  end;

end;

{****************************************************************}
procedure TForm1.ALButtonMemcachedFLush_ALLClick(Sender: TObject);
var LMemCachedClient: TAlMemCachedClient;
begin
  LMemCachedClient := TAlMemCachedClient.create;
  try
    LMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
    ALMemoResult.Clear;
    LMemCachedClient.flush_all(0);
    ALMemoResult.Lines.Add('flush_all');
    ALMemoResult.Lines.Add('---------');
    ALMemoResult.Lines.Add('OK');
    LMemCachedClient.disconnect;
  finally
    LMemCachedClient.Free;
  end;
end;

initialization
  currentIncNumberCR := TcriticalSection.create;
  randomize;
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

finalization
  currentIncNumberCR.free;

end.
