unit Unit1;

interface

uses
  Windows, Messages, Diagnostics, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StrUtils, ExtCtrls, StdCtrls, cxStyles,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxEdit,
  cxGridLevel, cxGridCustomTableView, cxGridTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, ComCtrls, Alcinoe.Sqlite3.Client, cxMemo, cxBlobEdit,
  Alcinoe.FBX.Client, Alcinoe.MySql.Client, cxDropDownEdit,
  Alcinoe.SphinxQL.Client, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore,
  dxSkinFoggy, dxSkinscxPCPainter, dxSkinsForm, Vcl.Menus, cxButtons,
  cxPCdxBarPopupMenu, cxPC, cxContainer, cxLabel, cxTextEdit, cxMaskEdit,
  cxButtonEdit, cxCheckBox, cxGroupBox, cxRadioGroup, Alcinoe.MemCached.Client,
  Alcinoe.MongoDB.Client, Alcinoe.JSONDoc, cxCheckGroup, cxNavigator, Shellapi,
  dxBarBuiltInMenu, dxDateRanges, dxScrollbarAnnotations, dxCore;

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
    Firebird: TcxTabSheet;
    Label2: TcxLabel;
    Label4: TcxLabel;
    Label15: TcxLabel;
    Label16: TcxLabel;
    Label17: TcxLabel;
    Label18: TcxLabel;
    Label13: TcxLabel;
    Label26: TcxLabel;
    Label29: TcxLabel;
    Label30: TcxLabel;
    Label31: TcxLabel;
    ALButtonFirebirdSelect: TcxButton;
    ALEditFirebirdLogin: TcxTextEdit;
    ALEditFirebirdPassword: TcxTextEdit;
    ALEditFirebirdCharset: TcxTextEdit;
    ALEditFirebirdLib: TcxButtonEdit;
    ALMemoFireBirdQuery: TcxMemo;
    ALEditFirebirdDatabase: TcxButtonEdit;
    ALButtonFirebirdLoopSelect: TcxButton;
    ALButtonFirebirdUpdate: TcxButton;
    ALButtonFirebirdLoopUpdate: TcxButton;
    ALEditFirebirdNBLoop: TcxTextEdit;
    ALEditFirebirdNbLoopBeforeCommit: TcxTextEdit;
    ALEditFirebirdNBThread: TcxTextEdit;
    ALEditFireBirdNum_buffers: TcxTextEdit;
    ALButtonFirebirdCreateDatabase: TcxButton;
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
    ALMemoFireBirdParams: TcxMemo;
    Label1: TcxLabel;
    Label14: TcxLabel;
    Label27: TcxLabel;
    ALMemoFirebirdTPB: TcxMemo;
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
    ALComboBoxFirebirdapiVer: TcxComboBox;
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
    procedure ALButtonFirebirdSelectClick(Sender: TObject);
    procedure ALButtonFirebirdLoopSelectClick(Sender: TObject);
    procedure ALButtonFirebirdUpdateClick(Sender: TObject);
    procedure ALButtonFirebirdLoopUpdateClick(Sender: TObject);
    procedure ALButtonSqlLite3SelectClick(Sender: TObject);
    procedure ALButtonSqlite3LoopUpdateClick(Sender: TObject);
    procedure ALButtonSqlite3UpdateClick(Sender: TObject);
    procedure ALButtonSqlite3LoopSelectClick(Sender: TObject);
    procedure ALButtonFirebirdCreateDatabaseClick(Sender: TObject);
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
    FirebirdConnectionPoolClient: TalFBXConnectionPoolClient;
    MySqlConnectionPoolClient: TalMySqlConnectionPoolClient;
    MemcachedConnectionPoolClient: TALMemcachedConnectionPoolClient;
    MongoDBConnectionPoolClient: TALMongoDBConnectionPoolClient;
    NBActiveThread: integer;
    LoopStartDateTime: TdateTime;
  end;

  {---------------------------------------}
  TFirebirdBenchmarkThread = Class(Tthread)
  private
    fSQL: AnsiString;
    fParams: AnsiString;
    fTPB: AnsiString;
    fUpdateSQL: Boolean;
    fOn: Boolean;
    fMaxLoop: integer;
    fNBLoopBeforeCommit: integer;
    FErrorMsg: AnsiString;
    FTotalPrepareTimeTaken: extended;
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
                  aParams: AnsiString;
                  aTPB: AnsiString;
                  aMaxLoop: integer;
                  aNBLoopBeforeCommit: integer;
                  aUpdateSQL: Boolean);
    destructor Destroy; override;
  End;

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

uses SyncObjs,
     System.AnsiStrings,
     Alcinoe.WinApi.Common,
     Alcinoe.FBX.Base,
     Alcinoe.FBX.Lib,
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
function SQLFastTagReplaceFunct(const TagString: AnsiString; TagParams: TALStringsA; ExtData: pointer; Var Handled: Boolean): AnsiString;
Var aMin, aMax: integer;
begin

  Handled := True;
  if ALSameTextA(TagString,'randomchar') then result := ALRandomStrA(1)
  else if ALSameTextA(TagString,'randomstring') then begin
    if not ALTryStrToInt(TagParams.Values['MinLength'], aMin) then aMin := 1;
    if not ALTryStrToInt(TagParams.Values['MaxLength'], aMax) then aMax := 255;
    result := ALRandomStrA(aMin + random(aMax - aMin + 1));
  end
  else if ALSameTextA(TagString,'randomnumber') then begin
    if not ALTryStrToInt(TagParams.Values['Min'], aMin) then aMin := 1;
    if not ALTryStrToInt(TagParams.Values['Max'], aMax) then aMax := Maxint;
    result := ALIntToStrA(aMin + random(aMax - aMin + 1));
  end
  else if ALSameTextA(TagString,'incnumber') then begin
    if not ALTryStrToInt(TagParams.Values['min'], aMin) then aMin := 0;
    currentIncNumberCR.Acquire;
    try
      inc(currentIncNumber);
      if aMin > currentIncNumber then begin
        currentIncNumber := aMin;
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

{************************************************************}
procedure TForm1.ALButtonFirebirdSelectClick(Sender: TObject);
Var aFBXClient: TALFbxClient;
    aXMLDATA: TalXmlDocument;
    aTKMain: TStopWatch;
    aTKPrepare: TStopWatch;
    aTKSelect: TStopWatch;
    aTKCommit: TStopWatch;
    aFormatSettings: TALFormatSettingsA;
    aFBAPiVersion: TALFBXVersion_API;
    aQuery: TALFBXClientSelectDataQuery;
    aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    i: integer;
    aTPB: AnsiString;
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

  aTPB:= ALTrim(AnsiString(ALMemoFireBirdTPB.Lines.Text));
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_version3', isc_tpb_version3, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_read_committed', isc_tpb_read_committed, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_concurrency', isc_tpb_concurrency, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_consistency', isc_tpb_consistency, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_no_rec_version', isc_tpb_no_rec_version, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_rec_version', isc_tpb_rec_version, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_write', isc_tpb_write, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_read', isc_tpb_read, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_nowait', isc_tpb_nowait, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_wait', isc_tpb_wait, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, #13#10, '', [rfReplaceALL]);
  aTPB := ALStringReplaceA(aTPB, ' ', '', [rfReplaceALL]);

  aFormatSettings := ALDefaultFormatSettingsA;
  Screen.Cursor := CrHourGlass;
  try

    aFBXClient := TALFbxClient.Create(aFBAPiVersion,AnsiString(ALEditFirebirdLib.Text));
    Try
      aFBXClient.connect(
        AnsiString(ALEditFireBirdDatabase.Text),
        AnsiString(ALEditFireBirdLogin.text),
        AnsiString(ALEditFireBirdPassword.text),
        AnsiString(ALEditFireBirdCharset.Text),
        StrToInt(ALEditFireBirdNum_buffers.Text));

      aXMLDATA := TALXmlDocument.create('root');
      Try

        With aXMLDATA Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
        end;

        aQuery.SQL := ALFastTagReplaceA(
                        AnsiString(AlMemoFireBirdQuery.Lines.Text),
                        '<#',
                        '>',
                        SQLFastTagReplaceFunct,
                        True,
                        nil);

        if ALMemoFireBirdParams.Lines.Count > 0 then begin
          Setlength(aQuery.Params, ALMemoFireBirdParams.Lines.Count);
          for I := 0 to ALMemoFireBirdParams.Lines.Count - 1 do begin
            aQuery.Params[i].Value := ALFastTagReplaceA(
                                        AnsiString(ALMemoFireBirdParams.Lines[i]),
                                        '<#',
                                        '>',
                                        SQLFastTagReplaceFunct,
                                        True,
                                        nil);
            aQuery.Params[i].isnull := False;
          end;
        end
        else Setlength(aQuery.Params, 0);
        aQuery.RowTag := 'rec';
        aQuery.ViewTag := '';
        aQuery.Skip := 0;
        aQuery.First := 200;

        aFBXClient.GetMonitoringInfos(
          aFBXClient.ConnectionID,
          -1,
          '',
          aIOStats_1,
          aRecordStats_1,
          aMemoryUsage,
          False,
          False,
          True);
        aTKMain := TStopWatch.StartNew;
        aFBXClient.TransactionStart(aTPB);
        try

          aTKPrepare := TStopWatch.StartNew;
          aFBXClient.Prepare(aQuery.SQL);
          aTKPrepare.Stop;

          aTKSelect := TStopWatch.StartNew;
          aFBXClient.SelectData(
            aQuery,
            aXMLDATA.DocumentElement,
            aFormatSettings);
          aTKSelect.Stop;

          aTKCommit := TStopWatch.StartNew;
          aFBXClient.TransactionCommit;
          aTKCommit.Stop;

        Except
          aFBXClient.TransactionRollBack;
          Raise;
        end;
        aTKMain.Stop;
        ALMemoResult.Clear;
        ALMemoResult.Lines.Add('Time Taken: ' + string(ALFormatFloatA('0.#####', aTKMain.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsA)) + ' ms');
        aFBXClient.GetMonitoringInfos(
          aFBXClient.ConnectionID,
          -1,
          '',
          aIOStats_2,
          aRecordStats_2,
          aMemoryUsage);
        ALMemoResult.Lines.Add('');
        ALMemoResult.Lines.Add('page_reads:   ' + IntToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
        ALMemoResult.Lines.Add('page_writes:  ' + IntToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
        ALMemoResult.Lines.Add('page_fetches: ' + IntToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
        ALMemoResult.Lines.Add('page_marks:   ' + IntToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
        ALMemoResult.Lines.Add('');
        ALMemoResult.Lines.Add('record_idx_reads: ' + IntToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
        ALMemoResult.Lines.Add('record_seq_reads: ' + IntToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
        ALMemoResult.Lines.Add('record_inserts:   ' + IntToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
        ALMemoResult.Lines.Add('record_updates:   ' + IntToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
        ALMemoResult.Lines.Add('record_deletes:   ' + IntToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
        ALMemoResult.Lines.Add('record_backouts:  ' + IntToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
        ALMemoResult.Lines.Add('record_purges:    ' + IntToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
        ALMemoResult.Lines.Add('record_expunges:  ' + IntToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
        ALMemoResult.Lines.Add('');
        ALMemoResult.Lines.Add('memory_used:          ' + IntToStr(aMemoryUsage.memory_used));
        ALMemoResult.Lines.Add('memory_allocated:     ' + IntToStr(aMemoryUsage.memory_allocated));
        ALMemoResult.Lines.Add('max_memory_used:      ' + IntToStr(aMemoryUsage.max_memory_used));
        ALMemoResult.Lines.Add('max_memory_allocated: ' + IntToStr(aMemoryUsage.max_memory_allocated));
        AlMemoResult.Lines.add('');
        AlMemoResult.Lines.add('**************');
        AlMemoResult.Lines.add('');
        AlMemoResult.Lines.Text := AlMemoResult.Lines.Text + String(aXMLDATA.XML);

        TableViewThread.DataController.RecordCount := 1;
        TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
        TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
        TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,aTKPrepare.Elapsed.TotalMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aTKSelect.Elapsed.TotalMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,aTKCommit.ElapsedMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
        StatusBar1.Panels[2].Text := '';

      Finally
        aXMLDATA.free;
      End;

    Finally
      aFBXClient.disconnect;
      aFBXClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;

end;

{************************************************************}
procedure TForm1.ALButtonFirebirdUpdateClick(Sender: TObject);
Var aFBXClient: TALFbxClient;
    aTKMain: TStopWatch;
    aTKPrepare: TStopWatch;
    aTKUpdate: TStopWatch;
    aTKCommit: TStopWatch;
    aFormatSettings: TALFormatSettingsA;
    aFBAPiVersion: TALFBXVersion_API;
    aQuery: TALFBXClientUpdateDataQUERY;
    aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aLstSql: TALStringsA;
    i: integer;
    aTPB: AnsiString;
    S1: AnsiString;
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

  aTPB:= ALTrim(AnsiString(ALMemoFireBirdTPB.Lines.Text));
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_version3', isc_tpb_version3, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_read_committed', isc_tpb_read_committed, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_concurrency', isc_tpb_concurrency, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_consistency', isc_tpb_consistency, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_no_rec_version', isc_tpb_no_rec_version, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_rec_version', isc_tpb_rec_version, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_write', isc_tpb_write, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_read', isc_tpb_read, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_nowait', isc_tpb_nowait, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_wait', isc_tpb_wait, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, #13#10, '', [rfReplaceALL]);
  aTPB := ALStringReplaceA(aTPB, ' ', '', [rfReplaceALL]);

  aFormatSettings := ALDefaultFormatSettingsA;
  Screen.Cursor := CrHourGlass;
  try

    aFBXClient := TALFbxClient.Create(aFBAPiVersion,AnsiString(ALEditFirebirdLib.Text));
    aLstSql := TALStringListA.create;
    Try
      aFBXClient.connect(
        AnsiString(ALEditFireBirdDatabase.Text),
        AnsiString(ALEditFireBirdLogin.text),
        AnsiString(ALEditFireBirdPassword.text),
        AnsiString(ALEditFireBirdCharset.Text),
        StrToInt(ALEditFireBirdNum_buffers.Text));

      aQuery.SQL := ALFastTagReplaceA(
                      AnsiString(AlMemoFireBirdQuery.Lines.Text),
                      '<#',
                      '>',
                      SQLFastTagReplaceFunct,
                      True,
                      nil);

      if ALMemoFireBirdParams.Lines.Count > 0 then begin
        Setlength(aQuery.Params, ALMemoFireBirdParams.Lines.Count);
        for I := 0 to ALMemoFireBirdParams.Lines.Count - 1 do begin
          aQuery.Params[i].Value := ALFastTagReplaceA(
                                      AnsiString(ALMemoFireBirdParams.Lines[i]),
                                      '<#',
                                      '>',
                                      SQLFastTagReplaceFunct,
                                      True,
                                      nil);
          aQuery.Params[i].isnull := False;
        end;
      end
      else Setlength(aQuery.Params, 0);

      if ALMemoFireBirdParams.Lines.Count <= 0 then begin
        if (ALPosA('begin',AllowerCase(aQuery.SQL)) <= 0) or
           (ALPosA('end',AllowerCase(aQuery.SQL)) <= 0) then begin
          S1 := ALStringReplaceA(aQuery.SQL,#13#10,' ',[RfReplaceALL]);
          aLstSql.Text := ALTrim(ALStringReplaceA(S1,';',#13#10,[RfReplaceALL]));
        end
        else aLstSql.Add(aQuery.SQL);
      end;

      aFBXClient.GetMonitoringInfos(
        aFBXClient.ConnectionID,
        -1,
        '',
        aIOStats_1,
        aRecordStats_1,
        aMemoryUsage,
        False,
        False,
        True);
      aTKMain := TStopWatch.StartNew;
      aFBXClient.TransactionStart(aTPB);
      try

        aTKPrepare := TStopWatch.StartNew;
        if aLstSql.Count = 1 then aFBXClient.Prepare(aLstSql[0])
        else if aLstSql.Count <= 0 then aFBXClient.Prepare(aQuery.SQL);
        aTKPrepare.Stop;

        aTKUpdate := TStopWatch.StartNew;
        if aLstSql.Count > 0 then aFBXClient.UpdateData(aLstSql)
        else aFBXClient.UpdateData(aQuery);
        aTKUpdate.Stop;

        aTKCommit := TStopWatch.StartNew;
        aFBXClient.TransactionCommit;
        aTKCommit.Stop;

      Except
        aFBXClient.TransactionRollBack;
        Raise;
      end;
      aTKMain.Stop;
      ALMemoResult.Clear;
      ALMemoResult.Lines.Add('Time Taken: ' + string(ALFormatFloatA('0.#####', aTKMain.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsA)) + ' ms');
      aFBXClient.GetMonitoringInfos(
        aFBXClient.ConnectionID,
        -1,
        '',
        aIOStats_2,
        aRecordStats_2,
        aMemoryUsage);
      ALMemoResult.Lines.Add('');
      ALMemoResult.Lines.Add('page_reads:   ' + IntToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
      ALMemoResult.Lines.Add('page_writes:  ' + IntToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
      ALMemoResult.Lines.Add('page_fetches: ' + IntToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
      ALMemoResult.Lines.Add('page_marks:   ' + IntToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
      ALMemoResult.Lines.Add('');
      ALMemoResult.Lines.Add('record_idx_reads: ' + IntToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
      ALMemoResult.Lines.Add('record_seq_reads: ' + IntToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
      ALMemoResult.Lines.Add('record_inserts:   ' + IntToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
      ALMemoResult.Lines.Add('record_updates:   ' + IntToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
      ALMemoResult.Lines.Add('record_deletes:   ' + IntToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
      ALMemoResult.Lines.Add('record_backouts:  ' + IntToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
      ALMemoResult.Lines.Add('record_purges:    ' + IntToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
      ALMemoResult.Lines.Add('record_expunges:  ' + IntToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
      ALMemoResult.Lines.Add('');
      ALMemoResult.Lines.Add('memory_used:          ' + IntToStr(aMemoryUsage.memory_used));
      ALMemoResult.Lines.Add('memory_allocated:     ' + IntToStr(aMemoryUsage.memory_allocated));
      ALMemoResult.Lines.Add('max_memory_used:      ' + IntToStr(aMemoryUsage.max_memory_used));
      ALMemoResult.Lines.Add('max_memory_allocated: ' + IntToStr(aMemoryUsage.max_memory_allocated));

      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,aTKPrepare.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aTKUpdate.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,aTKCommit.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';

    Finally
      aLstSql.free;
      aFBXClient.disconnect;
      aFBXClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;

end;

{********************************************************************}
procedure TForm1.ALButtonFirebirdCreateDatabaseClick(Sender: TObject);
Var aFBXClient: TalFBXClient;
    aFBAPiVersion: TALFBXVersion_API;
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

  aFBXClient := TALFbxClient.Create(aFBAPiVersion,AnsiString(ALEditFirebirdLib.Text));
  Try
    aFBXClient.CreateDatabase(AnsiString(AlMemoFireBirdQuery.Lines.Text));
  Finally
    aFBXClient.free;
  End;

  AlMemoResult.Lines.Clear;

end;

{****************************************************************}
procedure TForm1.ALButtonFirebirdLoopSelectClick(Sender: TObject);
Var aFirebirdBenchmarkThread: TFirebirdBenchmarkThread;
    i: integer;
    aFBAPiVersion: TALFBXVersion_API;
    aTPB: AnsiString;
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

  aTPB:= ALTrim(AnsiString(ALMemoFireBirdTPB.Lines.Text));
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_version3', isc_tpb_version3, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_read_committed', isc_tpb_read_committed, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_concurrency', isc_tpb_concurrency, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_consistency', isc_tpb_consistency, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_no_rec_version', isc_tpb_no_rec_version, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_rec_version', isc_tpb_rec_version, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_write', isc_tpb_write, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_read', isc_tpb_read, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_nowait', isc_tpb_nowait, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_wait', isc_tpb_wait, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, #13#10, '', [rfReplaceALL]);
  aTPB := ALStringReplaceA(aTPB, ' ', '', [rfReplaceALL]);

  {init button action}
  If ALButtonFirebirdLoopSelect.tag = 0 then begin
    ALButtonFirebirdLoopSelect.Tag := 1;
    ALButtonFirebirdLoopSelect.Caption := 'STOP';
  end
  else If ALButtonFirebirdLoopSelect.tag = 1 then begin
    ALButtonFirebirdLoopSelect.Tag := 2;
    ALButtonFirebirdLoopSelect.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := ALStrToInt(AnsiString(ALEditFirebirdNBThread.Text));

  //create the fFirebirdConnectionPoolClient
  if not assigned(FirebirdConnectionPoolClient) then begin
    FirebirdConnectionPoolClient := TALFBXConnectionPoolClient.Create(
                                      AnsiString(ALEditFireBirdDatabase.Text),
                                      AnsiString(ALEditFireBirdLogin.text),
                                      AnsiString(ALEditFireBirdPassword.text),
                                      AnsiString(ALEditFireBirdCharset.Text),
                                      aFBAPiVersion,
                                      AnsiString(ALEditFirebirdLib.Text),
                                      StrToInt(ALEditFireBirdNum_buffers.Text));
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditFirebirdNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    aFirebirdBenchmarkThread := TFirebirdBenchmarkThread.Create(
                                  True,
                                  self,
                                  i,
                                  ALTrim(AnsiString(ALMemoFirebirdQuery.Lines.Text)),
                                  ALTrim(AnsiString(ALMemoFirebirdParams.Lines.Text)),
                                  aTPB,
                                  StrToInt(ALEditFirebirdNBLoop.Text),
                                  StrToInt(ALEditFirebirdNbLoopBeforeCommit.Text),
                                  false);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    aFirebirdBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aFirebirdBenchmarkThread.Start;
    {$ELSE}
    aFirebirdBenchmarkThread.Resume;
    {$IFEND}
  end;

end;

{****************************************************************}
procedure TForm1.ALButtonFirebirdLoopUpdateClick(Sender: TObject);
Var aFirebirdBenchmarkThread: TFirebirdBenchmarkThread;
    i: integer;
    aFBAPiVersion: TALFBXVersion_API;
    aTPB: AnsiString;
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

  aTPB:= ALTrim(AnsiString(ALMemoFireBirdTPB.Lines.Text));
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_version3', isc_tpb_version3, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_read_committed', isc_tpb_read_committed, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_concurrency', isc_tpb_concurrency, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_consistency', isc_tpb_consistency, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_no_rec_version', isc_tpb_no_rec_version, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_rec_version', isc_tpb_rec_version, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_write', isc_tpb_write, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_read', isc_tpb_read, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_nowait', isc_tpb_nowait, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, 'isc_tpb_wait', isc_tpb_wait, [rfIgnoreCase]);
  aTPB := ALStringReplaceA(aTPB, #13#10, '', [rfReplaceALL]);
  aTPB := ALStringReplaceA(aTPB, ' ', '', [rfReplaceALL]);

  {init button action}
  If ALButtonFirebirdLoopUpdate.tag = 0 then begin
    ALButtonFirebirdLoopUpdate.Tag := 1;
    ALButtonFirebirdLoopUpdate.Caption := 'STOP';
  end
  else If ALButtonFirebirdLoopUpdate.tag = 1 then begin
    ALButtonFirebirdLoopUpdate.Tag := 2;
    ALButtonFirebirdLoopUpdate.Caption := 'STOPPING';
    exit;
  end
  else exit;

  //init local variable
  TableViewThread.DataController.RecordCount := StrToInt(ALEditFirebirdNBThread.Text);

  //create the fFirebirdConnectionPoolClient
  if not assigned(FirebirdConnectionPoolClient) then begin
    FirebirdConnectionPoolClient := TALFBXConnectionPoolClient.Create(
                                      AnsiString(ALEditFireBirdDatabase.Text),
                                      AnsiString(ALEditFireBirdLogin.text),
                                      AnsiString(ALEditFireBirdPassword.text),
                                      AnsiString(ALEditFireBirdCharset.Text),
                                      aFBAPiVersion,
                                      AnsiString(ALEditFirebirdLib.Text),
                                      StrToInt(ALEditFireBirdNum_buffers.Text));
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  LoopStartDateTime := now;
  for i := 1 to StrToInt(ALEditFirebirdNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStrA(i) + ' (on)');
    aFirebirdBenchmarkThread := TFirebirdBenchmarkThread.Create(
                                  True,
                                  self,
                                  i,
                                  ALTrim(AnsiString(ALMemoFirebirdQuery.Lines.Text)),
                                  ALTrim(AnsiString(ALMemoFirebirdParams.Lines.Text)),
                                  aTPB,
                                  StrToInt(ALEditFirebirdNBLoop.Text),
                                  StrToInt(ALEditFirebirdNbLoopBeforeCommit.Text),
                                  true);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    aFirebirdBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aFirebirdBenchmarkThread.Start;
    {$ELSE}
    aFirebirdBenchmarkThread.Resume;
    {$IFEND}

  end;

end;

{*********************************************************}
procedure TForm1.ALButtonMySqlSelectClick(Sender: TObject);
Var aMySqlClient: TalMySqlClient;
    aXMLDATA: TalXmlDocument;
    aStopWatch: TstopWatch;
    aFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
    aMySQLAPiVersion: TALMySqlVersion_API;
begin

  case ALComboBoxMySqlApiVer.ItemIndex of
    1: aMySQLAPiVersion := MYSQL55;
    else aMySQLAPiVersion := MYSQL50;
  end;

  aFormatSettings := ALDefaultFormatSettingsA;
  Screen.Cursor := CrHourGlass;
  try

    aMySqlClient := TalMySqlClient.Create(aMySQLAPiVersion, AnsiString(ALEditMySqllib.Text));
    Try
      aMySqlClient.connect(
        AnsiString(ALEditMySqlHost.Text),
        StrToInt(ALEditMySqlPort.Text),
        AnsiString(ALEditMySqlDatabaseName.Text),
        AnsiString(ALEditMySqlLogin.Text),
        AnsiString(ALEditMySqlPassword.Text),
        AnsiString(ALEditMySqlCharset.Text),
        0);

      aXMLDATA := TALXmlDocument.create('root');
      Try

        With aXMLDATA Do Begin
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

        aStopWatch:= TstopWatch.StartNew;
        aMySqlClient.SelectData(
          S1,
          'rec',
           0,
           200,
          aXMLDATA.DocumentElement,
          aFormatSettings);
        aStopWatch.Stop;

        TableViewThread.DataController.RecordCount := 1;
        TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
        TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
        TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.Elapsed.TotalMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
        StatusBar1.Panels[2].Text := '';
        AlMemoResult.Lines.Text := String(aXMLDATA.XML);

      Finally
        aXMLDATA.free;
      End;

    Finally
      aMySqlClient.disconnect;
      aMySqlClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{*********************************************************}
procedure TForm1.ALButtonMysqlUpdateClick(Sender: TObject);
Var aMySqlClient: TalMySqlClient;
    aTKExecute: TstopWatch;
    aTKCommit: TStopWatch;
    LstSql: TALStringListA;
    S1: AnsiString;
    aMySQLAPiVersion: TALMySqlVersion_API;
begin

  case ALComboBoxMySqlApiVer.ItemIndex of
    1: aMySQLAPiVersion := MYSQL55;
    else aMySQLAPiVersion := MYSQL50;
  end;

  Screen.Cursor := CrHourGlass;
  try

    aMySqlClient := TalMySqlClient.Create(aMySQLAPiVersion, AnsiString(ALEditMySqllib.Text));
    LstSql := TALStringListA.Create;
    Try
      aMySqlClient.connect(
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
      LstSql.Text := ALTrim(ALStringReplaceA(S1,';',#13#10,[RfReplaceALL]));

      aTKExecute:= TstopWatch.StartNew;
      aMySqlClient.TransactionStart;
      try
        aMySqlClient.UpdateData(LstSql);
        aTKExecute.Stop;
        aTKCommit := TStopWatch.StartNew;
        aMySqlClient.TransactionCommit;
        aTKCommit.Stop;
      Except
        aMySqlClient.TransactionRollBack;
        raise;
      end;

      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aTKExecute.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,aTKCommit.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.Text := '';

    Finally
      aMySqlClient.disconnect;
      aMySqlClient.free;
      LstSql.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{*************************************************************}
procedure TForm1.ALButtonMysqlLoopSelectClick(Sender: TObject);
Var aMySqlBenchmarkThread: TMySqlBenchmarkThread;
    aMySQLAPiVersion: TALMySqlVersion_API;
    i: integer;
begin

  case ALComboBoxMySqlApiVer.ItemIndex of
    1: aMySQLAPiVersion := MYSQL55;
    else aMySQLAPiVersion := MYSQL50;
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
                                   aMySQLAPiVersion,
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
    aMySqlBenchmarkThread := TMySqlBenchmarkThread.Create(
                               True,
                               self,
                               i,
                               ALTrim(AnsiString(ALMemoMySqlQuery.Lines.Text)),
                               StrToInt(ALEditMySqlNBLoop.Text),
                               StrToInt(ALEditMySqlNbLoopBeforeCommit.Text),
                               false);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(NBActiveThread);
    StatusBar1.Repaint;
    aMySqlBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aMySqlBenchmarkThread.Start;
    {$ELSE}
    aMySqlBenchmarkThread.Resume;
    {$IFEND}

  end;

end;

{*************************************************************}
procedure TForm1.ALButtonMySqlLoopUpdateClick(Sender: TObject);
Var aMySqlBenchmarkThread: TMySqlBenchmarkThread;
    aMySQLAPiVersion: TALMySqlVersion_API;
    i: integer;
begin

  case ALComboBoxMySqlApiVer.ItemIndex of
    1: aMySQLAPiVersion := MYSQL55;
    else aMySQLAPiVersion := MYSQL50;
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
                                   aMySQLAPiVersion,
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
    aMySqlBenchmarkThread := TMySqlBenchmarkThread.Create(
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
    aMySqlBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aMySqlBenchmarkThread.Start;
    {$ELSE}
    aMySqlBenchmarkThread.Resume;
    {$IFEND}

  end;

end;

{************************************************************}
procedure TForm1.ALButtonSqlLite3SelectClick(Sender: TObject);
Var aSqlite3Client: TalSqlite3Client;
    aXMLDATA: TalXmlDocument;
    aStopWatch: TStopWatch;
    aFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
begin
  aFormatSettings := ALDefaultFormatSettingsA;
  Screen.Cursor := CrHourGlass;
  try

    aSqlite3Client := TalSqlite3Client.Create(AnsiString(ALEditSqlite3lib.Text));
    Try

      //enable or disable the shared cache
      aSqlite3Client.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);

      //connect
      aSqlite3Client.connect(AnsiString(ALEditSqlite3Database.text));

      //the pragma
      aSqlite3Client.UpdateData('PRAGMA page_size = '+AnsiString(ALEditSqlite3Page_Size.Text));
      aSqlite3Client.UpdateData('PRAGMA encoding = "UTF-8"');
      aSqlite3Client.UpdateData('PRAGMA legacy_file_format = 0');
      aSqlite3Client.UpdateData('PRAGMA auto_vacuum = NONE');
      aSqlite3Client.UpdateData('PRAGMA cache_size = '+AnsiString(ALEditSqlite3Cache_Size.Text));
      case RadioGroupSqlite3Journal_Mode.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA journal_mode = DELETE');
        1: aSqlite3Client.UpdateData('PRAGMA journal_mode = TRUNCATE');
        2: aSqlite3Client.UpdateData('PRAGMA journal_mode = PERSIST');
        3: aSqlite3Client.UpdateData('PRAGMA journal_mode = MEMORY');
        4: aSqlite3Client.UpdateData('PRAGMA journal_mode = WAL');
        5: aSqlite3Client.UpdateData('PRAGMA journal_mode = OFF');
      end;
      aSqlite3Client.UpdateData('PRAGMA locking_mode = NORMAL');
      If ALCheckBoxSqlite3ReadUncommited.Checked then aSqlite3Client.UpdateData('PRAGMA read_uncommitted = 1');
      case RadioGroupSqlite3Synhcronous.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA synchronous = OFF');
        1: aSqlite3Client.UpdateData('PRAGMA synchronous = NORMAL');
        2: aSqlite3Client.UpdateData('PRAGMA synchronous = FULL');
      end;
      case RadioGroupSQLite3Temp_Store.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA temp_store = DEFAULT');
        1: aSqlite3Client.UpdateData('PRAGMA temp_store = FILE');
        2: aSqlite3Client.UpdateData('PRAGMA temp_store = MEMORY');
      end;

      //the sql
      aXMLDATA := TALXmlDocument.create('root');
      Try

        With aXMLDATA Do Begin
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

        aStopWatch := TStopWatch.StartNew;
        aSqlite3Client.SelectData(
          S1,
          'rec',
           0,
           200,
          aXMLDATA.DocumentElement,
          aFormatSettings);
        aStopWatch.Stop;

        TableViewThread.DataController.RecordCount := 1;
        TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
        TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
        TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.Elapsed.TotalMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
        StatusBar1.Panels[2].Text := '';
        AlMemoResult.Lines.Text := String(aXMLDATA.XML);

      Finally
        aXMLDATA.free;
      End;

    Finally
      aSqlite3Client.disconnect;
      aSqlite3Client.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{***********************************************************}
procedure TForm1.ALButtonSqlite3UpdateClick(Sender: TObject);
Var aSqlite3Client: TalSqlite3Client;
    aTKExecute: TStopWatch;
    aTKCommit: TStopWatch;
    LstSql: TALStringListA;
    S1: AnsiString;
begin
  Screen.Cursor := CrHourGlass;
  try

    aSqlite3Client := TalSqlite3Client.Create(AnsiString(ALEditSqlite3lib.Text));
    LstSql := TALStringListA.Create;
    Try

      //enable or disable the shared cache
      aSqlite3Client.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);

      //connect
      aSqlite3Client.connect(AnsiString(ALEditSqlite3Database.text));

      //the pragma
      aSqlite3Client.UpdateData('PRAGMA page_size = '+AnsiString(ALEditSqlite3Page_Size.Text));
      aSqlite3Client.UpdateData('PRAGMA encoding = "UTF-8"');
      aSqlite3Client.UpdateData('PRAGMA legacy_file_format = 0');
      aSqlite3Client.UpdateData('PRAGMA auto_vacuum = NONE');
      aSqlite3Client.UpdateData('PRAGMA cache_size = '+AnsiString(ALEditSqlite3Cache_Size.Text));
      case RadioGroupSqlite3Journal_Mode.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA journal_mode = DELETE');
        1: aSqlite3Client.UpdateData('PRAGMA journal_mode = TRUNCATE');
        2: aSqlite3Client.UpdateData('PRAGMA journal_mode = PERSIST');
        3: aSqlite3Client.UpdateData('PRAGMA journal_mode = MEMORY');
        4: aSqlite3Client.UpdateData('PRAGMA journal_mode = WAL');
        5: aSqlite3Client.UpdateData('PRAGMA journal_mode = OFF');
      end;
      aSqlite3Client.UpdateData('PRAGMA locking_mode = NORMAL');
      If ALCheckBoxSqlite3ReadUncommited.Checked then aSqlite3Client.UpdateData('PRAGMA read_uncommitted = 1');
      case RadioGroupSqlite3Synhcronous.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA synchronous = OFF');
        1: aSqlite3Client.UpdateData('PRAGMA synchronous = NORMAL');
        2: aSqlite3Client.UpdateData('PRAGMA synchronous = FULL');
      end;
      case RadioGroupSQLite3Temp_Store.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA temp_store = DEFAULT');
        1: aSqlite3Client.UpdateData('PRAGMA temp_store = FILE');
        2: aSqlite3Client.UpdateData('PRAGMA temp_store = MEMORY');
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
      LstSql.Text := ALTrim(ALStringReplaceA(S1,';',#13#10,[RfReplaceALL]));

      //do the job
      aTKExecute := TStopWatch.StartNew;
      aSqlite3Client.TransactionStart;
      try
        aSqlite3Client.UpdateData(LstSql);
        aTKExecute.Stop;
        aTKCommit := TStopWatch.StartNew;
        aSqlite3Client.TransactionCommit;
        aTKCommit.Stop;
      Except
        aSqlite3Client.TransactionRollBack;
        raise;
      end;

      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aTKExecute.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,aTKCommit.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.Text := '';

    Finally
      aSqlite3Client.disconnect;
      aSqlite3Client.free;
      LstSql.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{**********************************************************}
procedure TForm1.ALButtonSphinxSelectClick(Sender: TObject);
Var aSphinxClient: TalSphinxQLClient;
    aXMLDATA1: TalXmlDocument;
    aXMLDATA2: TalXmlDocument;
    aStopWatch: TStopWatch;
    aFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
    aMySQLAPiVersion: TALMySqlVersion_API;
    i: integer;
begin

  case ALComboBoxSphinxApiVer.ItemIndex of
    1: aMySQLAPiVersion := MYSQL55;
    else aMySQLAPiVersion := MYSQL50;
  end;

  aFormatSettings := ALDefaultFormatSettingsA;
  Screen.Cursor := CrHourGlass;
  try

    aSphinxClient := TalSphinxQLClient.Create(aMySQLAPiVersion, AnsiString(ALEditSphinxlib.Text));
    Try
      aSphinxClient.connect(
        AnsiString(ALEditSphinxHost.Text),
        StrToInt(ALEditSphinxPort.Text));

      aXMLDATA1 := TALXmlDocument.create('root');
      aXMLDATA2 := TALXmlDocument.create('root');
      Try

        With aXMLDATA1 Do Begin
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

        aStopWatch := TStopWatch.StartNew;
        aSphinxClient.SelectData(
          S1,
          'rec',
           0,
           200,
          aXMLDATA1.DocumentElement,
          aFormatSettings);
        aStopWatch.Stop;


        ALMemoResult.Clear;
        ALMemoResult.Lines.Add('Time Taken: ' + string(ALFormatFloatA('0.#####', aStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsA)) + ' ms');
        aSphinxClient.SelectData(
          'SHOW META',
          'rec',
          aXMLDATA2.DocumentElement,
          aFormatSettings);
        ALMemoResult.Lines.Add('');
        for I := 0 to aXMLDATA2.DocumentElement.ChildNodes.Count - 1 do
          ALMemoResult.Lines.Add(String(aXMLDATA2.DocumentElement.ChildNodes[i].childnodes['variable_name'].Text) + ': ' + String(aXMLDATA2.DocumentElement.ChildNodes[i].childnodes['value'].Text));
        AlMemoResult.Lines.add('');
        AlMemoResult.Lines.add('**************');
        AlMemoResult.Lines.add('');
        AlMemoResult.Lines.Text := AlMemoResult.Lines.Text + String(aXMLDATA1.XML);

        TableViewThread.DataController.RecordCount := 1;
        TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
        TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
        TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.Elapsed.TotalMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
        StatusBar1.Panels[2].Text := '';

      Finally
        aXMLDATA1.free;
        aXMLDATA2.free;
      End;

    Finally
      aSphinxClient.disconnect;
      aSphinxClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{**********************************************************}
procedure TForm1.ALButtonSphinxUpdateClick(Sender: TObject);
Var aSphinxClient: TalSphinxQLClient;
    aTKExecute: TStopWatch;
    aTKCommit: TStopWatch;
    LstSql: TALStringListA;
    S1: AnsiString;
    aMySQLAPiVersion: TALMySqlVersion_API;
begin

  case ALComboBoxSphinxApiVer.ItemIndex of
    1: aMySQLAPiVersion := MYSQL55;
    else aMySQLAPiVersion := MYSQL50;
  end;

  Screen.Cursor := CrHourGlass;
  try

    aSphinxClient := TalSphinxQLClient.Create(aMySQLAPiVersion, AnsiString(ALEditSphinxlib.Text));
    LstSql := TALStringListA.Create;
    Try
      aSphinxClient.connect(
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
      LstSql.Text := ALTrim(ALStringReplaceA(S1,';',#13#10,[RfReplaceALL]));

      aTKExecute := TStopWatch.StartNew;
      aSphinxClient.TransactionStart;
      try
        aSphinxClient.UpdateData(LstSql);
        aTKExecute.Stop;
        aTKCommit := TStopWatch.StartNew;
        aSphinxClient.TransactionCommit;
        aTKCommit.Stop;
      Except
        aSphinxClient.TransactionRollBack;
        raise;
      end;

      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aTKExecute.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,aTKCommit.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.Text := '';

    Finally
      aSphinxClient.disconnect;
      aSphinxClient.free;
      LstSql.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{**************************************************************}
procedure TForm1.ALButtonSphinxLoopSelectClick(Sender: TObject);
Var aSphinxBenchmarkThread: TMYsqlBenchmarkThread;
    aMySQLAPiVersion: TALMySqlVersion_API;
    i: integer;
begin

  case ALComboBoxSphinxApiVer.ItemIndex of
    1: aMySQLAPiVersion := MYSQL55;
    else aMySQLAPiVersion := MYSQL50;
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
                                   aMySQLAPiVersion,
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
    aSphinxBenchmarkThread := TMySQLBenchmarkThread.Create(
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
    aSphinxBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aSphinxBenchmarkThread.Start;
    {$ELSE}
    aSphinxBenchmarkThread.Resume;
    {$IFEND}

  end;

end;

{**************************************************************}
procedure TForm1.ALButtonSphinxLoopUpdateClick(Sender: TObject);
Var aSphinxBenchmarkThread: TMySqlBenchmarkThread;
    aMySQLAPiVersion: TALMySqlVersion_API;
    i: integer;
begin

  case ALComboBoxSphinxApiVer.ItemIndex of
    1: aMySQLAPiVersion := MYSQL55;
    else aMySQLAPiVersion := MYSQL50;
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
                                   aMySQLAPiVersion,
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
    aSphinxBenchmarkThread := TMysqlBenchmarkThread.Create(
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
    aSphinxBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aSphinxBenchmarkThread.Start;
    {$ELSE}
    aSphinxBenchmarkThread.Resume;
    {$IFEND}

  end;

end;

{***************************************************************}
procedure TForm1.ALButtonSqlite3LoopSelectClick(Sender: TObject);
Var aSqlite3BenchmarkThread: TSqlite3BenchmarkThread;
    aPragmaStatements: AnsiString;
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
  aPragmaStatements := '';
  aPragmaStatements := aPragmaStatements + 'PRAGMA page_size = '+AnsiString(ALEditSqlite3Page_Size.Text)+';';
  aPragmaStatements := aPragmaStatements + 'PRAGMA encoding = "UTF-8";';
  aPragmaStatements := aPragmaStatements + 'PRAGMA legacy_file_format = 0;';
  aPragmaStatements := aPragmaStatements + 'PRAGMA auto_vacuum = NONE;';
  aPragmaStatements := aPragmaStatements + 'PRAGMA cache_size = '+AnsiString(ALEditSqlite3Cache_Size.Text)+';';
  case RadioGroupSqlite3Journal_Mode.ItemIndex of
    0: aPragmaStatements := aPragmaStatements + 'PRAGMA journal_mode = DELETE;';
    1: aPragmaStatements := aPragmaStatements + 'PRAGMA journal_mode = TRUNCATE;';
    2: aPragmaStatements := aPragmaStatements + 'PRAGMA journal_mode = PERSIST;';
    3: aPragmaStatements := aPragmaStatements + 'PRAGMA journal_mode = MEMORY;';
    4: aPragmaStatements := aPragmaStatements + 'PRAGMA journal_mode = WAL;';
    5: aPragmaStatements := aPragmaStatements + 'PRAGMA journal_mode = OFF;';
  end;
  aPragmaStatements := aPragmaStatements + 'PRAGMA locking_mode = NORMAL;';
  If ALCheckBoxSqlite3ReadUncommited.Checked then aPragmaStatements := aPragmaStatements + 'PRAGMA read_uncommitted = 1;';
  case RadioGroupSqlite3Synhcronous.ItemIndex of
    0: aPragmaStatements := aPragmaStatements + 'PRAGMA synchronous = OFF;';
    1: aPragmaStatements := aPragmaStatements + 'PRAGMA synchronous = NORMAL;';
    2: aPragmaStatements := aPragmaStatements + 'PRAGMA synchronous = FULL;';
  end;
  case RadioGroupSQLite3Temp_Store.ItemIndex of
    0: aPragmaStatements := aPragmaStatements + 'PRAGMA temp_store = DEFAULT;';
    1: aPragmaStatements := aPragmaStatements + 'PRAGMA temp_store = FILE;';
    2: aPragmaStatements := aPragmaStatements + 'PRAGMA temp_store = MEMORY;';
  end;

  //create the fSqlite3ConnectionPoolClient
  if not assigned(Sqlite3ConnectionPoolClient) then begin
    Sqlite3ConnectionPoolClient := TALSqlite3ConnectionPoolClient.Create(
                                     AnsiString(ALEditSqlite3Database.text),
                                     SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE,
                                     aPragmaStatements,
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
    aSqlite3BenchmarkThread := TSqlite3BenchmarkThread.Create(
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
    aSqlite3BenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aSqlite3BenchmarkThread.Start;
    {$ELSE}
    aSqlite3BenchmarkThread.Resume;
    {$IFEND}

  end;

end;

{***************************************************************}
procedure TForm1.ALButtonSqlite3LoopUpdateClick(Sender: TObject);
Var aSqlite3BenchmarkThread: TSqlite3BenchmarkThread;
    aPragmaStatements: AnsiString;
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
  aPragmaStatements := '';
  aPragmaStatements := aPragmaStatements + 'PRAGMA page_size = '+AnsiString(ALEditSqlite3Page_Size.Text)+';';
  aPragmaStatements := aPragmaStatements + 'PRAGMA encoding = "UTF-8";';
  aPragmaStatements := aPragmaStatements + 'PRAGMA legacy_file_format = 0;';
  aPragmaStatements := aPragmaStatements + 'PRAGMA auto_vacuum = NONE;';
  aPragmaStatements := aPragmaStatements + 'PRAGMA cache_size = '+AnsiString(ALEditSqlite3Cache_Size.Text)+';';
  case RadioGroupSqlite3Journal_Mode.ItemIndex of
    0: aPragmaStatements := aPragmaStatements + 'PRAGMA journal_mode = DELETE;';
    1: aPragmaStatements := aPragmaStatements + 'PRAGMA journal_mode = TRUNCATE;';
    2: aPragmaStatements := aPragmaStatements + 'PRAGMA journal_mode = PERSIST;';
    3: aPragmaStatements := aPragmaStatements + 'PRAGMA journal_mode = MEMORY;';
    4: aPragmaStatements := aPragmaStatements + 'PRAGMA journal_mode = WAL;';
    5: aPragmaStatements := aPragmaStatements + 'PRAGMA journal_mode = OFF;';
  end;
  aPragmaStatements := aPragmaStatements + 'PRAGMA locking_mode = NORMAL;';
  If ALCheckBoxSqlite3ReadUncommited.Checked then aPragmaStatements := aPragmaStatements + 'PRAGMA read_uncommitted = 1;';
  case RadioGroupSqlite3Synhcronous.ItemIndex of
    0: aPragmaStatements := aPragmaStatements + 'PRAGMA synchronous = OFF;';
    1: aPragmaStatements := aPragmaStatements + 'PRAGMA synchronous = NORMAL;';
    2: aPragmaStatements := aPragmaStatements + 'PRAGMA synchronous = FULL;';
  end;
  case RadioGroupSQLite3Temp_Store.ItemIndex of
    0: aPragmaStatements := aPragmaStatements + 'PRAGMA temp_store = DEFAULT;';
    1: aPragmaStatements := aPragmaStatements + 'PRAGMA temp_store = FILE;';
    2: aPragmaStatements := aPragmaStatements + 'PRAGMA temp_store = MEMORY;';
  end;

  //create the fSqlite3ConnectionPoolClient
  if not assigned(Sqlite3ConnectionPoolClient) then begin
    Sqlite3ConnectionPoolClient := TALSqlite3ConnectionPoolClient.Create(
                                     AnsiString(ALEditSqlite3Database.text),
                                     SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE,
                                     aPragmaStatements,
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
    aSqlite3BenchmarkThread := TSqlite3BenchmarkThread.Create(
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
    aSqlite3BenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aSqlite3BenchmarkThread.Start;
    {$ELSE}
    aSqlite3BenchmarkThread.Resume;
    {$IFEND}

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
Var aconnectionHandle: SQLite3;
    aStopWatch: TStopWatch;
    aLoopIndex: integer;
    aCommitLoopIndex: integer;
    aXMLDATA: TalXmlDocument;
    aFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
begin

  //init the aFormatSettings
  aFormatSettings := ALDefaultFormatSettingsA;

  //init the fNBLoopBeforeCommit
  if fNBLoopBeforeCommit <= 0 then fNBLoopBeforeCommit := 1;

  //create local object
  aXMLDATA := TALXmlDocument.create('root');
  Try

    //start the loop;
    aLoopIndex := 1;
    while aLoopIndex <= fMaxLoop do begin
      try

        //start the Transaction
        Tform1(fOwner).Sqlite3ConnectionPoolClient.TransactionStart(aconnectionHandle);
        try

          for aCommitLoopIndex := 1 to fNBLoopBeforeCommit do begin

            s1 := ALFastTagReplaceA(
                    fSQL,
                    '<#',
                    '>',
                    SQLFastTagReplaceFunct,
                    True,
                    nil);

            aXMLDATA.Clear('root');
            aStopWatch := TStopWatch.StartNew;
            if fUpdateSQL then Tform1(fOwner).Sqlite3ConnectionPoolClient.UpdateData(S1, aconnectionHandle)
            else Tform1(fOwner).Sqlite3ConnectionPoolClient.SelectData(
                   s1,
                   aXMLDATA.documentElement,
                   aFormatSettings,
                   aconnectionHandle);
            aStopWatch.Stop;
            FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + aStopWatch.Elapsed.TotalMilliseconds;

            inc(aLoopIndex);
            inc(FTotalLoop);
            if aLoopIndex > fMaxLoop then break;

          end;

          aStopWatch := TStopWatch.StartNew;
          Tform1(fOwner).Sqlite3ConnectionPoolClient.Transactioncommit(aconnectionHandle);
          aStopWatch.Stop;
          FTotalCommitTimeTaken := FTotalCommitTimeTaken + aStopWatch.Elapsed.TotalMilliseconds;

        Except
          Tform1(fOwner).Sqlite3ConnectionPoolClient.TransactionRollBack(aconnectionHandle);
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
    aXMLDATA.free;
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

{******************************************}
constructor TFirebirdBenchmarkThread.Create(
              CreateSuspended: Boolean;
              AOwner: TwinControl;
              aRank: integer;
              aSQL: AnsiString;
              aParams: AnsiString;
              aTPB: AnsiString;
              aMaxLoop,
              aNBLoopBeforeCommit: integer;
              aUpdateSQL: Boolean);
begin
  inherited Create(CreateSuspended);
  fSQL:= aSQL;
  fParams:= aParams;
  fTPB:= aTPB;
  fOn:= true;
  fMaxLoop:= aMaxLoop;
  if fMaxLoop <= 0 then fMaxLoop := MaxInt;
  fNBLoopBeforeCommit:= aNBLoopBeforeCommit;
  if fNBLoopBeforeCommit <= 0 then fNBLoopBeforeCommit := 1;
  FErrorMsg := '';
  FTotalPrepareTimeTaken := -1;
  FTotalExecuteTimeTaken := 0;
  FTotalCommitTimeTaken := 0;
  FTotalLoop := 0;
  fOwner := AOwner;
  fRank := aRank;
  fUpdateSQL := aUpdateSQL;
end;

{******************************************}
destructor TFirebirdBenchmarkThread.Destroy;
begin
  fOn := False;
  Synchronize(UpdateGUI);
  inherited;
end;

{*****************************************}
procedure TFirebirdBenchmarkThread.Execute;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function internalDoTagReplace(Str: AnsiString): AnsiString;
  Begin
    Str := ALFastTagReplaceA(
             Str,
             '<#',
             '>',
             SQLFastTagReplaceFunct,
             True,
             nil);
    Result := Str;
  End;

Var aDBHandle: IscDbHandle;
    aTraHandle: IscTrHandle;
    aStopWatch: TStopWatch;
    aLoopIndex: integer;
    aCommitLoopIndex: integer;
    aXMLDATA: TalXmlDocument;
    aSelectDataQueries: TalFBXClientSelectDataQUERIES;
    aUpdateDataQueries: TalFBXClientUpdateDataQUERIES;
    aTmpSelectDataQueries: TalFBXClientSelectDataQUERIES;
    aTmpUpdateDataQueries: TalFBXClientUpdateDataQUERIES;
    aStatementPool: TALFBXConnectionStatementPoolBinTree;
    aStatementPoolNode: TALStringKeyAVLBinaryTreeNode;
    aFormatSettings: TALFormatSettingsA;
    aStmtHandle: IscStmtHandle;
    aSqlda: TALFBXSQLResult;
    S1: AnsiString;
    j, k: integer;
    aLst1: TALStringListA;

begin

  //init the aFormatSettings
  aFormatSettings := ALDefaultFormatSettingsA;

  //init the fNBLoopBeforeCommit
  if fNBLoopBeforeCommit <= 0 then fNBLoopBeforeCommit := 1;

  //create local object
  aXMLDATA := TALXmlDocument.create('root');
  Try

    //start the loop;
    aLoopIndex := 1;
    while aLoopIndex <= fMaxLoop do begin
      try

        //update the aLstSql
        setlength(aSelectDataQueries,0);
        setlength(aUpdateDataQueries,0);
        if fParams = '' then begin
          aLst1 := TALStringListA.Create;
          Try
            S1 := ALStringReplaceA(fSQL,#13#10,' ',[RfReplaceALL]);
            aLst1.Text := ALTrim(ALStringReplaceA(S1,';',#13#10,[RfReplaceALL]));
            for J := 0 to aLst1.Count - 1 do begin
              if fUpdateSQL then begin
                setlength(aUpdateDataQueries,length(aUpdateDataQueries)+1);
                setlength(aUpdateDataQueries[length(aUpdateDataQueries)-1].Params,0);
                aUpdateDataQueries[length(aUpdateDataQueries)-1].Sql := S1;
              end
              else begin
                setlength(aSelectDataQueries,length(aSelectDataQueries)+1);
                setlength(aSelectDataQueries[length(aSelectDataQueries)-1].Params,0);
                aSelectDataQueries[length(aSelectDataQueries)-1].Sql := S1;
                aSelectDataQueries[length(aSelectDataQueries)-1].RowTag := '';
                aSelectDataQueries[length(aSelectDataQueries)-1].viewTag := '';
                aSelectDataQueries[length(aSelectDataQueries)-1].skip := 0;
                aSelectDataQueries[length(aSelectDataQueries)-1].First := 0;
              end;
            end;
          Finally
            aLst1.Free;
          End;
        end
        else begin
          aLst1 := TALStringListA.Create;
          Try
            aLst1.Text := ALTrim(fParams);
            if fUpdateSQL then begin
              setlength(aUpdateDataQueries,1);
              setlength(aUpdateDataQueries[0].Params,aLst1.Count);
              for J := 0 to aLst1.Count - 1 do begin
                aUpdateDataQueries[0].Params[j].Value := aLst1[j];
                aUpdateDataQueries[0].Params[j].isnull := false;
              end;
              aUpdateDataQueries[0].Sql := fSQL;
            end
            else begin
              setlength(aSelectDataQueries,1);
              setlength(aSelectDataQueries[0].Params,aLst1.Count);
              for J := 0 to aLst1.Count - 1 do begin
                aSelectDataQueries[0].Params[j].Value := aLst1[j];
                aSelectDataQueries[0].Params[j].isnull := false;
              end;
              aSelectDataQueries[0].Sql := fSQL;
              aSelectDataQueries[0].RowTag := '';
              aSelectDataQueries[0].viewTag := '';
              aSelectDataQueries[0].skip := 0;
              aSelectDataQueries[0].First := 0;
            end;
          Finally
            aLst1.Free;
          End;
        end;

        //start the Transaction
        Tform1(fOwner).FirebirdConnectionPoolClient.TransactionStart(aDbHandle, aTRAHandle, fTPB);
        try

          //Prepare the Transaction
          aStmtHandle := nil;
          aSqlda := nil;
          if fUpdateSQL then begin
            if (length(aUpdateDataQueries) = 1) and (ALPosA('<#', aUpdateDataQueries[0].SQL) <= 0) then begin
              aStopWatch := TStopWatch.StartNew;
              Tform1(fOwner).FirebirdConnectionPoolClient.Prepare(
                aUpdateDataQueries[0].SQL,
                aDBHandle,
                aTraHandle,
                aStmtHandle,
                aSqlda,
                fTPB);
              aStopWatch.Stop;
              if FTotalPrepareTimeTaken = -1 then FTotalPrepareTimeTaken := 0;
              FTotalPrepareTimeTaken := FTotalPrepareTimeTaken + aStopWatch.Elapsed.TotalMilliseconds;
            end;
          end
          else begin
            if (length(aSelectDataQueries) = 1) and (ALPosA('<#', aSelectDataQueries[0].SQL) <= 0) then begin
              aStopWatch := TStopWatch.StartNew;
              Tform1(fOwner).FirebirdConnectionPoolClient.Prepare(
                aSelectDataQueries[0].SQL,
                aDBHandle,
                aTraHandle,
                aStmtHandle,
                aSqlda,
                fTPB);
              aStopWatch.Stop;
              if FTotalPrepareTimeTaken = -1 then FTotalPrepareTimeTaken := 0;
              FTotalPrepareTimeTaken := FTotalPrepareTimeTaken + aStopWatch.Elapsed.TotalMilliseconds;
            end;
          end;
          try


            //Execute the SQL
            for aCommitLoopIndex := 1 to fNBLoopBeforeCommit do begin
              setlength(aTmpSelectDataQueries, length(aSelectDataQueries));
              for j := 0 to length(aTmpSelectDataQueries) - 1 do begin
                aTmpSelectDataQueries[j].SQL := internalDoTagReplace(aSelectDataQueries[j].SQL);
                setlength(aTmpSelectDataQueries[j].Params, length(aSelectDataQueries[j].Params));
                for k := 0 to length(aTmpSelectDataQueries[j].Params) - 1 do begin
                  aTmpSelectDataQueries[j].Params[k].Value := internalDoTagReplace(aSelectDataQueries[j].Params[k].Value);
                  aTmpSelectDataQueries[j].Params[k].isnull := aSelectDataQueries[j].Params[k].isnull;
                end;
                aTmpSelectDataQueries[j].RowTag := aSelectDataQueries[j].RowTag;
                aTmpSelectDataQueries[j].ViewTag := aSelectDataQueries[j].ViewTag;
                aTmpSelectDataQueries[j].Skip := aSelectDataQueries[j].Skip;
                aTmpSelectDataQueries[j].First := aSelectDataQueries[j].First;
              end;
              setlength(aTmpUpdateDataQueries, length(aUpdateDataQueries));
              for j := 0 to length(aTmpUpdateDataQueries) - 1 do begin
                aTmpUpdateDataQueries[j].SQL := internalDoTagReplace(aUpdateDataQueries[j].SQL);
                setlength(aTmpUpdateDataQueries[j].Params, length(aUpdateDataQueries[j].Params));
                for k := 0 to length(aTmpUpdateDataQueries[j].Params) - 1 do begin
                  aTmpUpdateDataQueries[j].Params[k].Value := internalDoTagReplace(aUpdateDataQueries[j].Params[k].Value);
                  aTmpUpdateDataQueries[j].Params[k].isnull := aUpdateDataQueries[j].Params[k].isnull;
                end;
              end;

              if assigned(aStmtHandle) then begin
                aStatementPool := TALFBXConnectionStatementPoolBinTree.Create;
                aStatementPoolNode := TALFBXConnectionStatementPoolBinTreeNode.Create;
                if fUpdateSQL then aStatementPoolNode.ID := aTmpUpdateDataQueries[0].SQL
                else aStatementPoolNode.ID := aTmpSelectDataQueries[0].SQL;
                TALFBXConnectionStatementPoolBinTreeNode(aStatementPoolNode).Lib := Tform1(fOwner).FirebirdConnectionPoolClient.Lib;
                TALFBXConnectionStatementPoolBinTreeNode(aStatementPoolNode).StmtHandle := aStmtHandle;
                TALFBXConnectionStatementPoolBinTreeNode(aStatementPoolNode).Sqlda := aSqlda;
                TALFBXConnectionStatementPoolBinTreeNode(aStatementPoolNode).OwnsObjects := False;
                if not aStatementPool.AddNode(aStatementPoolNode) then aStatementPoolNode.Free;
              end
              else aStatementPool := nil;
              try
                if fUpdateSQL then begin
                  aStopWatch := TStopWatch.StartNew;
                  if fUpdateSQL then Tform1(fOwner).FirebirdConnectionPoolClient.UpdateData(
                                       aTmpUpdateDataQueries,
                                       aDBHandle,
                                       aTraHandle,
                                       aStatementPool,
                                       fTPB);
                  aStopWatch.Stop;
                  FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + aStopWatch.Elapsed.TotalMilliseconds;
                end
                else begin
                  aXMLDATA.Clear('root');
                  aStopWatch := TStopWatch.StartNew;
                  Tform1(fOwner).FirebirdConnectionPoolClient.SelectData(
                    aTmpSelectDataQueries,
                    aXMLDATA.documentElement,
                    aFormatSettings,
                    aDbHandle,
                    aTraHandle,
                    aStatementPool,
                    fTPB);
                  aStopWatch.Stop;
                  FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + aStopWatch.Elapsed.TotalMilliseconds;
                end;
              finally
                if assigned(aStatementPool) then aStatementPool.Free;
              end;

              inc(FTotalLoop);
              inc(aLoopIndex);
              if aLoopIndex > fMaxLoop then break;

            end;

          finally

            //drop the statement
            try
              if assigned(aStmtHandle) then Tform1(fOwner).FirebirdConnectionPoolClient.Lib.DSQLFreeStatement(aStmtHandle, DSQL_drop);
            Except
              //what else we can do here ?
              //this can happen if connection lost for exemple
            end;
            if assigned(aSqlda) then aSqlda.free;
            aStmtHandle := nil;
            aSqlda := nil;

          end;

          //commit the data
          aStopWatch := TStopWatch.StartNew;
          Tform1(fOwner).FirebirdConnectionPoolClient.Transactioncommit(aDbHandle,aTraHandle);
          aStopWatch.Stop;
          FTotalCommitTimeTaken := FTotalCommitTimeTaken + aStopWatch.Elapsed.TotalMilliseconds;

        except

          //roolBack the data
          Tform1(fOwner).FirebirdConnectionPoolClient.TransactionRollBack(aDbHandle, aTraHandle);
          raise;

        end;

      Except
        on e: Exception do begin
          FErrorMsg := AnsiString(E.message);
          Synchronize(UpdateGUI);
          Exit;
        end;
      end;
      If ((not fUpdateSQL) and (Tform1(fOwner).ALButtonFirebirdLoopSelect.tag = 2)) or
         ((fUpdateSQL) and (Tform1(fOwner).ALButtonFirebirdLoopUpdate.tag = 2)) then begin
        Synchronize(UpdateGUI);
        Break;
      end;
    end;

  Finally
    aXMLDATA.free;
  End;
end;

{*******************************************}
procedure TFirebirdBenchmarkThread.UpdateGUI;
begin
  if fUpdateSQL then begin
    TForm1(fOwner).TableViewThread.BeginUpdate;
    try
      if not fOn then begin
        dec(TForm1(fOwner).NBActiveThread);
        TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + IntToStr(TForm1(fOwner).NBActiveThread);
        TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,ALIntToStrA(frank) + ' (off)');
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).ALButtonFirebirdLoopUpdate.Tag := 0;
          TForm1(fOwner).ALButtonFirebirdLoopUpdate.Caption := 'Loop UPDATE';
          TForm1(fOwner).FirebirdConnectionPoolClient.Free;
          TForm1(fOwner).FirebirdConnectionPoolClient := nil;
          TForm1(fOwner).StatusBar1.Panels[2].Text := 'Total time taken: ' + String(ALFormatDateTimeA('hh:nn:ss.zzz',Now-TForm1(fOwner).LoopStartDateTime, ALDefaultFormatSettingsA));
        end;
      end;
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
      if FTotalPrepareTimeTaken >= 0 then TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,FTotalPrepareTimeTaken / alifThen(FTotalLoop <> 0, FTotalLoop, 1) )
      else  TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
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
          TForm1(fOwner).ALButtonFirebirdLoopSelect.Tag := 0;
          TForm1(fOwner).ALButtonFirebirdLoopSelect.Caption := 'Loop SELECT';
          TForm1(fOwner).FirebirdConnectionPoolClient.Free;
          TForm1(fOwner).FirebirdConnectionPoolClient := nil;
          TForm1(fOwner).StatusBar1.Panels[2].Text := 'Total time taken: ' + String(ALFormatDateTimeA('hh:nn:ss.zzz',Now-TForm1(fOwner).LoopStartDateTime, ALDefaultFormatSettingsA));
        end;
      end;
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
      if FTotalPrepareTimeTaken >= 0 then TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,FTotalPrepareTimeTaken / alifThen(FTotalLoop <> 0, FTotalLoop, 1) )
      else  TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
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
Var aConnectionHandle: PMySql;
    aStopWatch: TStopWatch;
    aLoopIndex: integer;
    aCommitLoopIndex: integer;
    aXMLDATA: TalXmlDocument;
    aFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
begin

  //init the aFormatSettings
  aFormatSettings := ALDefaultFormatSettingsA;

  //init the fNBLoopBeforeCommit
  if fNBLoopBeforeCommit <= 0 then fNBLoopBeforeCommit := 1;

  //create local object
  aXMLDATA := TALXmlDocument.create('root');
  Try

    //start the loop;
    aLoopIndex := 1;
    while aLoopIndex <= fMaxLoop do begin
      try

        //start the Transaction
        aConnectionHandle := nil;
        Tform1(fOwner).MySqlConnectionPoolClient.TransactionStart(aConnectionHandle);
        try

          for aCommitLoopIndex := 1 to fNBLoopBeforeCommit do begin

            S1 := ALFastTagReplaceA(
                    fSQL,
                    '<#',
                    '>',
                    SQLFastTagReplaceFunct,
                    True,
                    nil);

            aXMLDATA.Clear('root');
            aStopWatch := TStopWatch.StartNew;
            if fUpdateSQL then Tform1(fOwner).MySqlConnectionPoolClient.UpdateData(S1, aConnectionHandle)
            else Tform1(fOwner).MySqlConnectionPoolClient.SelectData(
                   S1,
                   aXMLDATA.documentElement,
                   aFormatSettings,
                   aConnectionHandle);
            aStopWatch.Stop;
            FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + aStopWatch.Elapsed.TotalMilliseconds;
            inc(aLoopIndex);
            inc(FTotalLoop);
            if aLoopIndex > fMaxLoop then break;
          end;

          aStopWatch := TStopWatch.StartNew;
          Tform1(fOwner).MySqlConnectionPoolClient.Transactioncommit(aConnectionHandle);
          aStopWatch.Stop;
          FTotalCommitTimeTaken := FTotalCommitTimeTaken + aStopWatch.Elapsed.TotalMilliseconds;

        Except
          Tform1(fOwner).MySqlConnectionPoolClient.TransactionRollBack(aConnectionHandle);
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
    aXMLDATA.free;
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
Var aStopWatch: TStopWatch;
    aKey: AnsiString;
    aFlags: integer;
    aExpTime: integer;
    aData: AnsiString;
begin

  //start the loop;
  while FTotalLoop < fMaxLoop do begin
    try

      //update the params
      aKey := ALFastTagReplaceA(
                fKey,
                '<#',
                '>',
                SQLFastTagReplaceFunct,
                True,
                nil);
      aflags := alStrToInt(
                  ALFastTagReplaceA(
                    fflags,
                    '<#',
                    '>',
                    SQLFastTagReplaceFunct,
                    True,
                    nil));
      aexpTime := alStrToInt(
                    ALFastTagReplaceA(
                      fexpTime,
                      '<#',
                      '>',
                      SQLFastTagReplaceFunct,
                      True,
                      nil));
      aData := ALFastTagReplaceA(
                 fData,
                 '<#',
                 '>',
                 SQLFastTagReplaceFunct,
                 True,
                 nil);
      //update the data
      aStopWatch := TStopWatch.StartNew;
      if fCMD = 'SET' then Tform1(fOwner).MemcachedConnectionPoolClient._Set(aKey, aFlags, aExpTime, aData)
      else if fCMD = 'GET' then Tform1(fOwner).MemcachedConnectionPoolClient.Get(aKey, aFlags, aData)
      else if fCMD = 'INCR' then Tform1(fOwner).MemcachedConnectionPoolClient.Incr(aKey, 1)
      else if fCMD = 'DECR' then Tform1(fOwner).MemcachedConnectionPoolClient.Decr(aKey, 1);
      aStopWatch.Stop;
      FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + aStopWatch.Elapsed.TotalMilliseconds;

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
Var aStopWatch: TStopWatch;
    aFullCollectionName: AnsiString;
    aQuery: AnsiString;
    aSelector: AnsiString;
    aSkip: integer;
    aFirst: integer;
    aJSONDATA: TALJSONNodeA;
begin

  aJSONDATA := TALJSONDocumentA.create;
  Try

    //start the loop;
    while FTotalLoop < fMaxLoop do begin
      try

        //update the params
        aFullCollectionName := ALFastTagReplaceA(
                                 fFullCollectionName,
                                 '<#',
                                 '>',
                                 SQLFastTagReplaceFunct,
                                 True,
                                 nil);
        aQuery := ALFastTagReplaceA(
                    fQuery,
                    '<#',
                    '>',
                    SQLFastTagReplaceFunct,
                    True,
                    nil);
        aSelector := ALFastTagReplaceA(
                       fSelector,
                       '<#',
                       '>',
                       SQLFastTagReplaceFunct,
                       True,
                       nil);
        if fSkip = '' then aSkip := 0
        else aSkip := ALStrToInt(
                        ALFastTagReplaceA(
                          fSkip,
                          '<#',
                          '>',
                          SQLFastTagReplaceFunct,
                          True,
                          nil));
        if fFirst = '' then aFirst := 0
        else aFirst := ALStrToInt(
                         ALFastTagReplaceA(
                           fFirst,
                           '<#',
                           '>',
                           SQLFastTagReplaceFunct,
                           True,
                           nil));

        //update the data
        aJSONDATA.ChildNodes.Clear;
        aStopWatch := TStopWatch.StartNew;
        if fCMD = 'SELECT' then Tform1(fOwner).MongoDBConnectionPoolClient.SelectData(
                                  aFullCollectionName,
                                  aQuery,
                                  aSelector,
                                  [],
                                  '', // rowtag
                                  aSkip,
                                  aFirst,
                                  aJSONDATA)
        else if fCMD = 'UPDATE' then Tform1(fOwner).MongoDBConnectionPoolClient.UpdateData(
                                       aFullCollectionName,
                                       aQuery,
                                       aSelector,
                                       fUpdateFlags)
        else if fCMD = 'INSERT' then Tform1(fOwner).MongoDBConnectionPoolClient.InsertData(
                                       aFullCollectionName,
                                       aQuery,
                                       fInsertFlags)
        else if fCMD = 'DELETE' then Tform1(fOwner).MongoDBConnectionPoolClient.DeleteData(
                                       aFullCollectionName,
                                       aQuery,
                                       fdeleteFlags);
        aStopWatch.Stop;
        FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + aStopWatch.Elapsed.TotalMilliseconds;

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
    aJSONDATA.Free;
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
Var aMemcachedBenchmarkThread: TMemcachedBenchmarkThread;
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
    aMemcachedBenchmarkThread := TMemcachedBenchmarkThread.Create(
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
    aMemcachedBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aMemcachedBenchmarkThread.Start;
    {$ELSE}
    aMemcachedBenchmarkThread.Resume;
    {$IFEND}
  end;

end;

{**************************************************************}
procedure TForm1.ALButtonMemcachedLoopSetClick(Sender: TObject);
Var aMemcachedBenchmarkThread: TMemcachedBenchmarkThread;
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
    aMemcachedBenchmarkThread := TMemcachedBenchmarkThread.Create(
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
    aMemcachedBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aMemcachedBenchmarkThread.Start;
    {$ELSE}
    aMemcachedBenchmarkThread.Resume;
    {$IFEND}
  end;

end;

{***************************************************************}
procedure TForm1.ALButtonMemcachedLoopIncrClick(Sender: TObject);
Var aMemcachedBenchmarkThread: TMemcachedBenchmarkThread;
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
    aMemcachedBenchmarkThread := TMemcachedBenchmarkThread.Create(
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
    aMemcachedBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aMemcachedBenchmarkThread.Start;
    {$ELSE}
    aMemcachedBenchmarkThread.Resume;
    {$IFEND}
  end;

end;

{***************************************************************}
procedure TForm1.ALButtonMemcachedLoopDecrClick(Sender: TObject);
Var aMemcachedBenchmarkThread: TMemcachedBenchmarkThread;
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
    aMemcachedBenchmarkThread := TMemcachedBenchmarkThread.Create(
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
    aMemcachedBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aMemcachedBenchmarkThread.Start;
    {$ELSE}
    aMemcachedBenchmarkThread.Resume;
    {$IFEND}
  end;

end;

{**********************************************************}
procedure TForm1.ALButtonMemcachedGetClick(Sender: TObject);
Var aMemCachedClient: TAlMemCachedClient;
    aStopWatch: TStopWatch;
    aKey: AnsiString;
    aflags: integer;
    aData: AnsiString;
    aFound: boolean;
begin
  Screen.Cursor := CrHourGlass;
  try

    aMemCachedClient := TAlMemCachedClient.create;
    Try
      aMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
      aKey := ALFastTagReplaceA(
                AnsiString(ALEditMemCachedKey.Text),
                '<#',
                '>',
                SQLFastTagReplaceFunct,
                True,
                nil);
      aStopWatch := TStopWatch.StartNew;
      aFound := aMemCachedClient.get(aKey, aFlags, aData);
      aStopWatch.Stop;
      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.Clear;
      if aFound then begin
        AlMemoResult.Lines.Add('Found: true');
        AlMemoResult.Lines.Add('Flags: ' + intToStr(aFlags));
        AlMemoResult.Lines.Add('Data: ' + string(aData));
      end
      else AlMemoResult.Lines.Add('Found: false');
      aMemCachedClient.disconnect;
    Finally
      aMemCachedClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{**********************************************************}
procedure TForm1.ALButtonMemcachedSetClick(Sender: TObject);
Var aMemCachedClient: TAlMemCachedClient;
    aStopWatch: TStopWatch;
    aKey: AnsiString;
    aflags: integer;
    aexpTime: integer;
    aData: AnsiString;
begin
  Screen.Cursor := CrHourGlass;
  try

    aMemCachedClient := TAlMemCachedClient.create;
    Try
      aMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
      aKey := ALFastTagReplaceA(
                AnsiString(ALEditMemCachedKey.Text),
                '<#',
                '>',
                SQLFastTagReplaceFunct,
                True,
                nil);
      aflags := alStrToInt(
                  ALFastTagReplaceA(
                    AnsiString(ALEditMemCachedflags.Text),
                    '<#',
                    '>',
                    SQLFastTagReplaceFunct,
                    True,
                    nil));
      aexpTime := alStrToInt(
                    ALFastTagReplaceA(
                      AnsiString(ALEditMemCachedexpTime.Text),
                      '<#',
                      '>',
                      SQLFastTagReplaceFunct,
                      True,
                      nil));
      aData := ALFastTagReplaceA(
                 AnsiString(ALMemoMemCachedData.Lines.Text),
                 '<#',
                 '>',
                 SQLFastTagReplaceFunct,
                 True,
                 nil);
      aStopWatch := TStopWatch.StartNew;
      aMemCachedClient._set(aKey, aflags, aexpTime, aData);
      aStopWatch.Stop;
      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.Text := '';
      aMemCachedClient.disconnect;
    Finally
      aMemCachedClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{*************************************************************}
procedure TForm1.ALButtonMemcachedDeleteClick(Sender: TObject);
Var aMemCachedClient: TAlMemCachedClient;
    aStopWatch: TStopWatch;
    aKey: AnsiString;
    aFound: boolean;
begin
  Screen.Cursor := CrHourGlass;
  try

    aMemCachedClient := TAlMemCachedClient.create;
    Try
      aMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
      aKey := ALFastTagReplaceA(
                AnsiString(ALEditMemCachedKey.Text),
                '<#',
                '>',
                SQLFastTagReplaceFunct,
                True,
                nil);
      aStopWatch := TStopWatch.StartNew;
      aFound := aMemCachedClient.delete(aKey);
      aStopWatch.Stop;
      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.Clear;
      if aFound then AlMemoResult.Lines.Add('Deleted: true')
      else AlMemoResult.Lines.Add('Deleted: false');
      aMemCachedClient.disconnect;
    Finally
      aMemCachedClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{************************************************************}
procedure TForm1.ALButtonMemcachedStatsClick(Sender: TObject);
var aMemCachedClient: TAlMemCachedClient;
begin
  aMemCachedClient := TAlMemCachedClient.create;
  try
    aMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
    ALMemoResult.Clear;
    ALMemoResult.Lines.Add('stats');
    ALMemoResult.Lines.Add('-----');
    ALMemoResult.Lines.Add(String(aMemCachedClient.stats('')));
    aMemCachedClient.disconnect;
  finally
    aMemCachedClient.Free;
  end;
end;

{*****************************************************************}
procedure TForm1.ALButtonMemcachedStatsItemsClick(Sender: TObject);
var aMemCachedClient: TAlMemCachedClient;
begin
  aMemCachedClient := TAlMemCachedClient.create;
  try
    aMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
    ALMemoResult.Clear;
    ALMemoResult.Lines.Add('stats items');
    ALMemoResult.Lines.Add('-----------');
    ALMemoResult.Lines.Add(String(aMemCachedClient.stats('items')));
    aMemCachedClient.disconnect;
  finally
    aMemCachedClient.Free;
  end;
end;

{********************************************************************}
procedure TForm1.ALButtonMemcachedStatsSettingsClick(Sender: TObject);
var aMemCachedClient: TAlMemCachedClient;
begin
  aMemCachedClient := TAlMemCachedClient.create;
  try
    aMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
    ALMemoResult.Clear;
    ALMemoResult.Lines.Add('stats settings');
    ALMemoResult.Lines.Add('--------------');
    ALMemoResult.Lines.Add(String(aMemCachedClient.stats('settings')));
    aMemCachedClient.disconnect;
  finally
    aMemCachedClient.Free;
  end;
end;

{*****************************************************************}
procedure TForm1.ALButtonMemcachedStatsSizesClick(Sender: TObject);
var aMemCachedClient: TAlMemCachedClient;
begin
  aMemCachedClient := TAlMemCachedClient.create;
  try
    aMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
    ALMemoResult.Clear;
    ALMemoResult.Lines.Add('stats sizes');
    ALMemoResult.Lines.Add('-----------');
    ALMemoResult.Lines.Add(String(aMemCachedClient.stats('sizes')));
    aMemCachedClient.disconnect;
  finally
    aMemCachedClient.Free;
  end;
end;

{*****************************************************************}
procedure TForm1.ALButtonMemcachedStatsSlabsClick(Sender: TObject);
var aMemCachedClient: TAlMemCachedClient;
begin
  aMemCachedClient := TAlMemCachedClient.create;
  try
    aMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
    ALMemoResult.Clear;
    ALMemoResult.Lines.Add('stats slabs');
    ALMemoResult.Lines.Add('-----------');
    ALMemoResult.Lines.Add(String(aMemCachedClient.stats('slabs')));
    aMemCachedClient.disconnect;
  finally
    aMemCachedClient.Free;
  end;
end;

{**************************************************************}
procedure TForm1.ALButtonMemcachedVersionClick(Sender: TObject);
var aMemCachedClient: TAlMemCachedClient;
begin
  aMemCachedClient := TAlMemCachedClient.create;
  try
    aMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
    ALMemoResult.Clear;
    ALMemoResult.Lines.Add('version');
    ALMemoResult.Lines.Add('-------');
    ALMemoResult.Lines.Add(String(aMemCachedClient.version));
    aMemCachedClient.disconnect;
  finally
    aMemCachedClient.Free;
  end;
end;

{***********************************************************}
procedure TForm1.ALButtonMongoDBSelectClick(Sender: TObject);
Var aMongoDBClient: TAlMongoDBClient;
    aJSONDATA: TALJSONNodeA;
    aStopWatch: TstopWatch;
    aFlags: TALMongoDBClientSelectDataFlags;
begin

  Screen.Cursor := CrHourGlass;
  try

    aMongoDBClient := TAlMongoDBClient.create;
    Try
      aMongoDBClient.Connect(AnsiString(ALEditMongoDBHost.Text), StrToInt(ALEditMongoDBPort.Text));

      aJSONDATA := TALJSONDocumentA.create;
      Try

        aflags := [];
        if CheckGroupMongoDBSelectFlags.States[0] = cbsChecked then aflags := aflags + [sfSlaveOk];
        if CheckGroupMongoDBSelectFlags.States[1] = cbsChecked then aflags := aflags + [sfPartial];

        aStopWatch:= TstopWatch.StartNew;
        aMongoDBClient.SelectData(
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
          aflags,
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
          aJSONDATA);
        aStopWatch.Stop;

        TableViewThread.DataController.RecordCount := 1;
        TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
        TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
        TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.Elapsed.TotalMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
        StatusBar1.Panels[2].Text := '';
        AlMemoResult.Lines.Text := String(aJSONDATA.JSON);

      Finally
        aJSONDATA.free;
      End;

    Finally
      aMongoDBClient.disconnect;
      aMongoDBClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{***********************************************************}
procedure TForm1.ALButtonMongoDBINSERTClick(Sender: TObject);
Var aMongoDBClient: TAlMongoDBClient;
    aStopWatch: TstopWatch;
    aflags: TALMongoDBClientInsertDataFlags;
begin

  Screen.Cursor := CrHourGlass;
  try

    aMongoDBClient := TAlMongoDBClient.create;
    Try
      aMongoDBClient.Connect(AnsiString(ALEditMongoDBHost.Text), StrToInt(ALEditMongoDBPort.Text));

      aflags := [];
      if CheckGroupMongoDBINSERTFlags.States[0] = cbsChecked then aflags := aflags + [ifContinueOnError];

      aStopWatch:= TstopWatch.StartNew;
      aMongoDBClient.InsertData(
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
        aflags);
      aStopWatch.Stop;

      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.Clear;

    Finally
      aMongoDBClient.disconnect;
      aMongoDBClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{***********************************************************}
procedure TForm1.ALButtonMongoDBUpdateClick(Sender: TObject);
Var aMongoDBClient: TAlMongoDBClient;
    aStopWatch: TstopWatch;
    aFlags: TALMongoDBClientUpdateDataFlags;
    NumberOfDocumentsUpdatedOrRemoved: integer;
    updatedExisting: Boolean;
    upserted: ansiString;
begin

  Screen.Cursor := CrHourGlass;
  try

    aMongoDBClient := TAlMongoDBClient.create;
    Try
      aMongoDBClient.Connect(AnsiString(ALEditMongoDBHost.Text), StrToInt(ALEditMongoDBPort.Text));

      aflags := [];
      if CheckGroupMongoDBUpdateFlags.States[0] = cbsChecked then aflags := aflags + [ufUpsert];
      if CheckGroupMongoDBUpdateFlags.States[1] = cbsChecked then aflags := aflags + [ufMultiUpdate];


      aStopWatch:= TstopWatch.StartNew;
      aMongoDBClient.UpdateData(
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
        aFlags,
        NumberOfDocumentsUpdatedOrRemoved,
        updatedExisting,
        upserted);
      aStopWatch.Stop;

      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.clear;
      AlMemoResult.Lines.add('Number Of Documents Updated: ' + intToStr(NumberOfDocumentsUpdatedOrRemoved));
      AlMemoResult.Lines.add('updated Existing: ' + String(ALBoolToStrA(updatedExisting)));
      AlMemoResult.Lines.add('upserted: ' + string(ALBinToHexA(upserted)));

    Finally
      aMongoDBClient.disconnect;
      aMongoDBClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{***********************************************************}
procedure TForm1.ALButtonMongoDBDeleteClick(Sender: TObject);
Var aMongoDBClient: TAlMongoDBClient;
    aStopWatch: TstopWatch;
    aFlags: TALMongoDBClientDeleteDataFlags;
    NumberOfDocumentsUpdatedOrRemoved: integer;
begin

  Screen.Cursor := CrHourGlass;
  try

    aMongoDBClient := TAlMongoDBClient.create;
    Try
      aMongoDBClient.Connect(AnsiString(ALEditMongoDBHost.Text), StrToInt(ALEditMongoDBPort.Text));

      aflags := [];
      if CheckGroupMongoDBDELETEFlags.States[0] = cbsChecked then aflags := aflags + [dfSingleRemove];

      aStopWatch:= TstopWatch.StartNew;
      aMongoDBClient.DeleteData(
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
        aflags,
        NumberOfDocumentsUpdatedOrRemoved);
      aStopWatch.Stop;

      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.Elapsed.TotalMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      StatusBar1.Panels[2].Text := '';
      AlMemoResult.Lines.clear;
      AlMemoResult.Lines.add('Number Of Documents removed: ' + intToStr(NumberOfDocumentsUpdatedOrRemoved));

    Finally
      aMongoDBClient.disconnect;
      aMongoDBClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{***************************************************************}
procedure TForm1.ALButtonMongoDBLOOPSELECTClick(Sender: TObject);
Var aMongoDBBenchmarkThread: TMongoDBBenchmarkThread;
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
    aMongoDBBenchmarkThread := TMongoDBBenchmarkThread.Create(
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
    aMongoDBBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aMongoDBBenchmarkThread.Start;
    {$ELSE}
    aMongoDBBenchmarkThread.Resume;
    {$IFEND}

  end;

end;

{***************************************************************}
procedure TForm1.ALButtonMongoDBLOOPUPDATEClick(Sender: TObject);
Var aMongoDBBenchmarkThread: TMongoDBBenchmarkThread;
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
    aMongoDBBenchmarkThread := TMongoDBBenchmarkThread.Create(
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
    aMongoDBBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aMongoDBBenchmarkThread.Start;
    {$ELSE}
    aMongoDBBenchmarkThread.Resume;
    {$IFEND}

  end;

end;

{***************************************************************}
procedure TForm1.ALButtonMongoDBLOOPINSERTClick(Sender: TObject);
Var aMongoDBBenchmarkThread: TMongoDBBenchmarkThread;
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
    aMongoDBBenchmarkThread := TMongoDBBenchmarkThread.Create(
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
    aMongoDBBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aMongoDBBenchmarkThread.Start;
    {$ELSE}
    aMongoDBBenchmarkThread.Resume;
    {$IFEND}

  end;

end;

{***************************************************************}
procedure TForm1.ALButtonMongoDBLOOPDELETEClick(Sender: TObject);
Var aMongoDBBenchmarkThread: TMongoDBBenchmarkThread;
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
    aMongoDBBenchmarkThread := TMongoDBBenchmarkThread.Create(
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
    aMongoDBBenchmarkThread.FreeOnTerminate := True;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    aMongoDBBenchmarkThread.Start;
    {$ELSE}
    aMongoDBBenchmarkThread.Resume;
    {$IFEND}

  end;

end;

{****************************************************************}
procedure TForm1.ALButtonMemcachedFLush_ALLClick(Sender: TObject);
var aMemCachedClient: TAlMemCachedClient;
begin
  aMemCachedClient := TAlMemCachedClient.create;
  try
    aMemCachedClient.Connect(AnsiString(ALEditMemCachedHost.Text), StrToInt(ALEditMemCachedPort.Text));
    ALMemoResult.Clear;
    aMemCachedClient.flush_all(0);
    ALMemoResult.Lines.Add('flush_all');
    ALMemoResult.Lines.Add('---------');
    ALMemoResult.Lines.Add('OK');
    aMemCachedClient.disconnect;
  finally
    aMemCachedClient.Free;
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
