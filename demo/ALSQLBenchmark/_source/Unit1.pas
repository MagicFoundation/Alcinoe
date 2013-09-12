unit Unit1;

interface

uses
  Windows, Messages, Diagnostics, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StrUtils, ExtCtrls, StdCtrls, cxStyles,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxEdit,
  cxGridLevel, cxGridCustomTableView, cxGridTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, ComCtrls, AlSqlite3Client, cxMemo, cxBlobEdit,
  alFbxClient, almysqlClient, OleCtrls, SHDocVw, ComObj, cxDropDownEdit,
  ALSphinxQLClient, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore,
  dxSkinFoggy, dxSkinscxPCPainter, dxSkinsForm, Vcl.Menus, cxButtons,
  cxPCdxBarPopupMenu, cxPC, cxContainer, cxLabel, cxTextEdit, cxMaskEdit,
  cxButtonEdit, cxCheckBox, cxGroupBox, cxRadioGroup, AlMemCachedClient,
  ALMongoDBClient;

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
    Panel2: TPanel;
    Panel4: TPanel;
    PanelWebBrowser: TPanel;
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
    cxLabel1: TcxLabel;
    cxLabel2: TcxLabel;
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
    cxLabel6: TcxLabel;
    cxTextEdit3: TcxTextEdit;
    cxTextEdit4: TcxTextEdit;
    cxLabel7: TcxLabel;
    cxLabel8: TcxLabel;
    cxTextEdit5: TcxTextEdit;
    cxLabel9: TcxLabel;
    cxMemo1: TcxMemo;
    ALButtonMongoDBFind: TcxButton;
    cxButton2: TcxButton;
    cxButton3: TcxButton;
    cxButton4: TcxButton;
    cxButton5: TcxButton;
    cxButton6: TcxButton;
    cxButton7: TcxButton;
    cxTextEdit6: TcxTextEdit;
    cxLabel10: TcxLabel;
    cxLabel11: TcxLabel;
    cxTextEdit7: TcxTextEdit;
    cxButton8: TcxButton;
    cxButton9: TcxButton;
    cxButton10: TcxButton;
    cxButton11: TcxButton;
    cxButton12: TcxButton;
    cxButton13: TcxButton;
    cxButton14: TcxButton;
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
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
    procedure ALButtonMongoDBFindClick(Sender: TObject);
  private
  public
    Sqlite3ConnectionPoolClient: TalSqlite3ConnectionPoolClient;
    FirebirdConnectionPoolClient: TalFBXConnectionPoolClient;
    MySqlConnectionPoolClient: TalMySqlConnectionPoolClient;
    MemcachedConnectionPoolClient: TALMemcachedConnectionPoolClient;
    NBActiveThread: integer;
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
    FTotalPrepareTimeTaken: int64;
    FTotalExecuteTimeTaken: int64;
    FTotalCommitTimeTaken: int64;
    FTotalLoop: integer;
    fOwner: TWinControl;
    fRank: integer;
    Procedure UpdateGUI;
  protected
    procedure Execute; override;
  protected
  Public
    constructor Create(CreateSuspended: Boolean;
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
    FTotalExecuteTimeTaken: int64;
    FTotalCommitTimeTaken: int64;
    FTotalLoop: integer;
    fOwner: TWinControl;
    fRank: integer;
    Procedure UpdateGUI;
  protected
    procedure Execute; override;
  protected
  Public
    constructor Create(CreateSuspended: Boolean;
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
    FTotalExecuteTimeTaken: int64;
    FTotalCommitTimeTaken: int64;
    FTotalLoop: integer;
    fOwner: TWinControl;
    fRank: integer;
    Procedure UpdateGUI;
  protected
    procedure Execute; override;
  protected
  Public
    constructor Create(CreateSuspended: Boolean;
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
    FTotalExecuteTimeTaken: int64;
    FTotalLoop: integer;
    fOwner: TWinControl;
    fRank: integer;
    Procedure UpdateGUI;
  protected
    procedure Execute; override;
  protected
  Public
    constructor Create(CreateSuspended: Boolean;
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

function GetProcessMemoryInfo(Process : THandle; var MemoryCounters : TProcessMemoryCounters; cb : DWORD) : BOOL; stdcall;
function ProcessMemoryUsage(ProcessID : DWORD): DWORD;

var Form1: TForm1;

implementation

uses alWindows,
     ALFBXBase,
     ALFBXLib,
     ALMySqlWrapper,
     alSqlite3Wrapper,
     ALAVLBinaryTree,
     AlXmlDoc,
     alStringList,
     ALString;

{$R *.dfm}

Var currentIncNumber: integer;

function GetProcessMemoryInfo; external 'psapi.dll';

{****************************************************}
function ProcessMemoryUsage(ProcessID : DWORD): DWORD;
var ProcessHandle : THandle;
    MemCounters   : TProcessMemoryCounters;
begin
  Result := 0;
  ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
                               false,
                               ProcessID);
  try
    if GetProcessMemoryInfo(ProcessHandle,
                            MemCounters,
                            sizeof(MemCounters))
    then Result := MemCounters.WorkingSetSize;
  finally
    CloseHandle(ProcessHandle);
  end;
end;

{**************************************************************************************************************************************}
function SQLFastTagReplaceFunct(const TagString: AnsiString; TagParams: TALStrings; ExtData: pointer; Var Handled: Boolean): AnsiString;
Var aMin, aMax: integer;
begin

  Handled := True;
  if ALSameText(TagString,'randomchar') then result := AlRandomStr(1)
  else if ALSameText(TagString,'randomstring') then begin
    if not ALTryStrToInt(TagParams.Values['MinLength'], aMin) then aMin := 1;
    if not ALTryStrToInt(TagParams.Values['MaxLength'], aMax) then aMax := 255;
    result := AlRandomStr(aMin + random(aMax - aMin + 1));
  end
  else if ALSameText(TagString,'randomnumber') then begin
    if not ALTryStrToInt(TagParams.Values['Min'], aMin) then aMin := 1;
    if not ALTryStrToInt(TagParams.Values['Max'], aMax) then aMax := Maxint;
    result := ALIntToStr(aMin + random(aMax - aMin + 1));
  end
  else if ALSameText(TagString,'incnumber') then begin
    if not ALTryStrToInt(TagParams.Values['min'], aMin) then aMin := 0;
    result := ALIntToStr(InterlockedExchange(currentIncNumber, currentIncNumber + 1) + 1);
    if aMin > ALStrToInt(result) then result := ALIntToStr(InterlockedExchange(currentIncNumber, aMin));
    result := ALIntToStr(ALStrToInt(result)+1);
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
    aFormatSettings: TALFormatSettings;
    aFBAPiVersion: TALFBXVersion_API;
    aSQL: TALFBXClientSelectDataSQL;
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
  aTPB := AlStringReplace(aTPB, 'isc_tpb_version3', isc_tpb_version3, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_read_committed', isc_tpb_read_committed, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_concurrency', isc_tpb_concurrency, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_consistency', isc_tpb_consistency, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_no_rec_version', isc_tpb_no_rec_version, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_rec_version', isc_tpb_rec_version, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_write', isc_tpb_write, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_read', isc_tpb_read, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_nowait', isc_tpb_nowait, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_wait', isc_tpb_wait, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, #13#10, '', [rfReplaceALL]);
  aTPB := AlStringReplace(aTPB, ' ', '', [rfReplaceALL]);

  ALGetLocaleFormatSettings(1033, aFormatSettings);
  Screen.Cursor := CrHourGlass;
  try

    aFBXClient := TALFbxClient.Create(aFBAPiVersion,AnsiString(ALEditFirebirdLib.Text));
    Try
      aFBXClient.connect(AnsiString(ALEditFireBirdDatabase.Text),
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

        aSQL.SQL := ALFastTagReplace(AnsiString(AlMemoFireBirdQuery.Lines.Text),
                                     '<#',
                                     '>',
                                     SQLFastTagReplaceFunct,
                                     True,
                                     nil);

        if ALMemoFireBirdParams.Lines.Count > 0 then begin
          Setlength(aSQL.Params, ALMemoFireBirdParams.Lines.Count);
          for I := 0 to ALMemoFireBirdParams.Lines.Count - 1 do begin
            aSQL.Params[i].Value := ALFastTagReplace(AnsiString(ALMemoFireBirdParams.Lines[i]),
                                                     '<#',
                                                     '>',
                                                     SQLFastTagReplaceFunct,
                                                     True,
                                                     nil);
            aSQL.Params[i].isnull := False;
          end;
        end
        else Setlength(aSQL.Params, 0);
        aSQL.RowTag := 'rec';
        aSQL.ViewTag := '';
        aSQL.Skip := 0;
        aSQL.First := 200;

        aFBXClient.GetMonitoringInfos(aFBXClient.ConnectionID,
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
          aFBXClient.Prepare(aSQL.SQL);
          aTKPrepare.Stop;

          aTKSelect := TStopWatch.StartNew;
          aFBXClient.SelectData(aSQL,
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
        ALMemoResult.Lines.Add('Time Taken: ' + IntToStr(aTKMain.ElapsedMilliseconds) + ' ms');
        aFBXClient.GetMonitoringInfos(aFBXClient.ConnectionID,
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
        TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,aTKPrepare.ElapsedMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aTKSelect.ElapsedMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,aTKCommit.ElapsedMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';

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
    aFormatSettings: TALFormatSettings;
    aFBAPiVersion: TALFBXVersion_API;
    aSQL: TALFBXClientUpdateDataSQL;
    aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aLstSql: TALStrings;
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
  aTPB := AlStringReplace(aTPB, 'isc_tpb_version3', isc_tpb_version3, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_read_committed', isc_tpb_read_committed, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_concurrency', isc_tpb_concurrency, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_consistency', isc_tpb_consistency, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_no_rec_version', isc_tpb_no_rec_version, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_rec_version', isc_tpb_rec_version, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_write', isc_tpb_write, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_read', isc_tpb_read, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_nowait', isc_tpb_nowait, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_wait', isc_tpb_wait, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, #13#10, '', [rfReplaceALL]);
  aTPB := AlStringReplace(aTPB, ' ', '', [rfReplaceALL]);

  ALGetLocaleFormatSettings(1033, aFormatSettings);
  Screen.Cursor := CrHourGlass;
  try

    aFBXClient := TALFbxClient.Create(aFBAPiVersion,AnsiString(ALEditFirebirdLib.Text));
    aLstSql := TALStringList.create;
    Try
      aFBXClient.connect(AnsiString(ALEditFireBirdDatabase.Text),
                         AnsiString(ALEditFireBirdLogin.text),
                         AnsiString(ALEditFireBirdPassword.text),
                         AnsiString(ALEditFireBirdCharset.Text),
                         StrToInt(ALEditFireBirdNum_buffers.Text));

      aSQL.SQL := ALFastTagReplace(AnsiString(AlMemoFireBirdQuery.Lines.Text),
                                   '<#',
                                   '>',
                                   SQLFastTagReplaceFunct,
                                   True,
                                   nil);

      if ALMemoFireBirdParams.Lines.Count > 0 then begin
        Setlength(aSQL.Params, ALMemoFireBirdParams.Lines.Count);
        for I := 0 to ALMemoFireBirdParams.Lines.Count - 1 do begin
          aSQL.Params[i].Value := ALFastTagReplace(AnsiString(ALMemoFireBirdParams.Lines[i]),
                                                   '<#',
                                                   '>',
                                                   SQLFastTagReplaceFunct,
                                                   True,
                                                   nil);
          aSQL.Params[i].isnull := False;
        end;
      end
      else Setlength(aSQL.Params, 0);

      if ALMemoFireBirdParams.Lines.Count <= 0 then begin
        if (AlPos('begin',AllowerCase(aSQL.SQL)) <= 0) or
           (AlPos('end',AllowerCase(aSQL.SQL)) <= 0) then begin
          S1 := AlStringReplace(aSQL.SQL,#13#10,' ',[RfReplaceALL]);
          aLstSql.Text := ALTrim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));
        end
        else aLstSql.Add(aSQL.SQL);
      end;

      aFBXClient.GetMonitoringInfos(aFBXClient.ConnectionID,
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
        else if aLstSql.Count <= 0 then aFBXClient.Prepare(aSQL.SQL);
        aTKPrepare.Stop;

        aTKUpdate := TStopWatch.StartNew;
        if aLstSql.Count > 0 then aFBXClient.UpdateData(aLstSql)
        else aFBXClient.UpdateData(aSQL);
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
      ALMemoResult.Lines.Add('Time Taken: ' + IntToStr(aTKMain.ElapsedMilliseconds) + ' ms');
      aFBXClient.GetMonitoringInfos(aFBXClient.ConnectionID,
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
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,aTKPrepare.ElapsedMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aTKUpdate.ElapsedMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,aTKCommit.ElapsedMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';

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
  aTPB := AlStringReplace(aTPB, 'isc_tpb_version3', isc_tpb_version3, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_read_committed', isc_tpb_read_committed, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_concurrency', isc_tpb_concurrency, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_consistency', isc_tpb_consistency, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_no_rec_version', isc_tpb_no_rec_version, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_rec_version', isc_tpb_rec_version, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_write', isc_tpb_write, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_read', isc_tpb_read, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_nowait', isc_tpb_nowait, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_wait', isc_tpb_wait, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, #13#10, '', [rfReplaceALL]);
  aTPB := AlStringReplace(aTPB, ' ', '', [rfReplaceALL]);

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
    FirebirdConnectionPoolClient := TALFBXConnectionPoolClient.Create(AnsiString(ALEditFireBirdDatabase.Text),
                                                                      AnsiString(ALEditFireBirdLogin.text),
                                                                      AnsiString(ALEditFireBirdPassword.text),
                                                                      AnsiString(ALEditFireBirdCharset.Text),
                                                                      aFBAPiVersion,
                                                                      AnsiString(ALEditFirebirdLib.Text),
                                                                      StrToInt(ALEditFireBirdNum_buffers.Text));
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';

  //launch all the thread
  for i := 1 to StrToInt(ALEditFirebirdNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStr(i) + ' (on)');
    aFirebirdBenchmarkThread := TFirebirdBenchmarkThread.Create(True,
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
  aTPB := AlStringReplace(aTPB, 'isc_tpb_version3', isc_tpb_version3, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_read_committed', isc_tpb_read_committed, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_concurrency', isc_tpb_concurrency, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_consistency', isc_tpb_consistency, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_no_rec_version', isc_tpb_no_rec_version, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_rec_version', isc_tpb_rec_version, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_write', isc_tpb_write, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_read', isc_tpb_read, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_nowait', isc_tpb_nowait, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, 'isc_tpb_wait', isc_tpb_wait, [rfIgnoreCase]);
  aTPB := AlStringReplace(aTPB, #13#10, '', [rfReplaceALL]);
  aTPB := AlStringReplace(aTPB, ' ', '', [rfReplaceALL]);

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
    FirebirdConnectionPoolClient := TALFBXConnectionPoolClient.Create(AnsiString(ALEditFireBirdDatabase.Text),
                                                                      AnsiString(ALEditFireBirdLogin.text),
                                                                      AnsiString(ALEditFireBirdPassword.text),
                                                                      AnsiString(ALEditFireBirdCharset.Text),
                                                                      aFBAPiVersion,
                                                                      AnsiString(ALEditFirebirdLib.Text),
                                                                      StrToInt(ALEditFireBirdNum_buffers.Text));
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';

  //launch all the thread
  for i := 1 to StrToInt(ALEditFirebirdNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStr(i) + ' (on)');
    aFirebirdBenchmarkThread := TFirebirdBenchmarkThread.Create(True,
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
    aFormatSettings: TALFormatSettings;
    S1: AnsiString;
    aMySQLAPiVersion: TALMySqlVersion_API;
begin

  case ALComboBoxMySqlApiVer.ItemIndex of
    1: aMySQLAPiVersion := MYSQL55;
    else aMySQLAPiVersion := MYSQL50;
  end;

  ALGetLocaleFormatSettings(1033, aFormatSettings);
  Screen.Cursor := CrHourGlass;
  try

    aMySqlClient := TalMySqlClient.Create(aMySQLAPiVersion, AnsiString(ALEditMySqllib.Text));
    Try
      aMySqlClient.connect(AnsiString(ALEditMySqlHost.Text),
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

        S1 := ALFastTagReplace(AnsiString(AlMemoMySqlQuery.Lines.Text),
                               '<#',
                               '>',
                               SQLFastTagReplaceFunct,
                               True,
                               nil);

        aStopWatch:= TstopWatch.StartNew;
        aMySqlClient.SelectData(S1,
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
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.ElapsedMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
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
    LstSql: TALStringList;
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
    LstSql := TALStringList.Create;
    Try
      aMySqlClient.connect(AnsiString(ALEditMySqlHost.Text),
                           StrToInt(ALEditMySqlPort.Text),
                           AnsiString(ALEditMySqlDatabaseName.Text),
                           AnsiString(ALEditMySqlLogin.Text),
                           AnsiString(ALEditMySqlPassword.Text),
                           AnsiString(ALEditMySqlCharset.Text),
                           0);

      S1 := ALFastTagReplace(AnsiString(AlMemoMySqlQuery.Lines.Text),
                             '<#',
                             '>',
                             SQLFastTagReplaceFunct,
                             True,
                             nil);

      S1 := AlStringReplace(S1,#13#10,' ',[RfReplaceALL]);
      LstSql.Text := ALTrim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));

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
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aTKExecute.ElapsedMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,aTKCommit.ElapsedMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
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
    MySqlConnectionPoolClient := TALMySqlConnectionPoolClient.Create(AnsiString(ALEditMySqlHost.Text),
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

  //launch all the thread
  for i := 1 to StrToInt(ALEditMySqlNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStr(i) + ' (on)');
    aMySqlBenchmarkThread := TMySqlBenchmarkThread.Create(True,
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
    MySqlConnectionPoolClient := TALMySqlConnectionPoolClient.Create(AnsiString(ALEditMySqlHost.Text),
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

  //launch all the thread
  for i := 1 to StrToInt(ALEditMySqlNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStr(i) + ' (on)');
    aMySqlBenchmarkThread := TMySqlBenchmarkThread.Create(True,
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
    aFormatSettings: TALFormatSettings;
    S1: AnsiString;
begin
  ALGetLocaleFormatSettings(1033, aFormatSettings);
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

        S1 := ALFastTagReplace(AnsiString(AlMemoSQLite3Query.Lines.Text),
                               '<#',
                               '>',
                               SQLFastTagReplaceFunct,
                               True,
                               nil);

        aStopWatch := TStopWatch.StartNew;
        aSqlite3Client.SelectData(S1,
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
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.ElapsedMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
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
    LstSql: TALStringList;
    S1: AnsiString;
begin
  Screen.Cursor := CrHourGlass;
  try

    aSqlite3Client := TalSqlite3Client.Create(AnsiString(ALEditSqlite3lib.Text));
    LstSql := TALStringList.Create;
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
      S1 := ALFastTagReplace(AnsiString(AlMemoSQLite3Query.Lines.Text),
                             '<#',
                             '>',
                             SQLFastTagReplaceFunct,
                             True,
                             nil);

      S1 := AlStringReplace(S1,#13#10,' ',[RfReplaceALL]);
      LstSql.Text := ALTrim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));

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
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aTKExecute.ElapsedMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,aTKCommit.ElapsedMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
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
    aFormatSettings: TALFormatSettings;
    S1: AnsiString;
    aMySQLAPiVersion: TALMySqlVersion_API;
    i: integer;
begin

  case ALComboBoxSphinxApiVer.ItemIndex of
    1: aMySQLAPiVersion := MYSQL55;
    else aMySQLAPiVersion := MYSQL50;
  end;

  ALGetLocaleFormatSettings(1033, aFormatSettings);
  Screen.Cursor := CrHourGlass;
  try

    aSphinxClient := TalSphinxQLClient.Create(aMySQLAPiVersion, AnsiString(ALEditSphinxlib.Text));
    Try
      aSphinxClient.connect(AnsiString(ALEditSphinxHost.Text),
                            StrToInt(ALEditSphinxPort.Text));

      aXMLDATA1 := TALXmlDocument.create('root');
      aXMLDATA2 := TALXmlDocument.create('root');
      Try

        With aXMLDATA1 Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
        end;

        S1 := ALFastTagReplace(AnsiString(AlMemoSphinxQuery.Lines.Text),
                               '<#',
                               '>',
                               SQLFastTagReplaceFunct,
                               True,
                               nil);

        aStopWatch := TStopWatch.StartNew;
        aSphinxClient.SelectData(S1,
                                'rec',
                                 0,
                                 200,
                                aXMLDATA1.DocumentElement,
                                aFormatSettings);
        aStopWatch.Stop;


        ALMemoResult.Clear;
        ALMemoResult.Lines.Add('Time Taken: ' + IntToStr(aStopWatch.ElapsedMilliseconds) + ' ms');
        aSphinxClient.SelectData('SHOW META',
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
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.ElapsedMilliseconds);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';

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
    LstSql: TALStringList;
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
    LstSql := TALStringList.Create;
    Try
      aSphinxClient.connect(AnsiString(ALEditSphinxHost.Text),
                            StrToInt(ALEditSphinxPort.Text));

      S1 := ALFastTagReplace(AnsiString(AlMemoSphinxQuery.Lines.Text),
                             '<#',
                             '>',
                             SQLFastTagReplaceFunct,
                             True,
                             nil);

      S1 := AlStringReplace(S1,#13#10,' ',[RfReplaceALL]);
      LstSql.Text := ALTrim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));

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
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aTKExecute.ElapsedMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,aTKCommit.ElapsedMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
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
    MySQLConnectionPoolClient := TALMySqlConnectionPoolClient.Create(AnsiString(ALEditSphinxHost.Text),
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

  //launch all the thread
  for i := 1 to StrToInt(ALEditSphinxNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStr(i) + ' (on)');
    aSphinxBenchmarkThread := TMySQLBenchmarkThread.Create(True,
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
    MySqlConnectionPoolClient := TALMySQLConnectionPoolClient.Create(AnsiString(ALEditSphinxHost.Text),
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

  //launch all the thread
  for i := 1 to StrToInt(ALEditSphinxNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStr(i) + ' (on)');
    aSphinxBenchmarkThread := TMysqlBenchmarkThread.Create(True,
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
    Sqlite3ConnectionPoolClient := TALSqlite3ConnectionPoolClient.Create(AnsiString(ALEditSqlite3Database.text),
                                                                         SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE,
                                                                         aPragmaStatements,
                                                                         AnsiString(ALEditSqlite3lib.Text));

    //enable or disable the shared cache
    Sqlite3ConnectionPoolClient.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';

  //launch all the thread
  for i := 1 to StrToInt(ALEditSqlite3NBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStr(i) + ' (on)');
    aSqlite3BenchmarkThread := TSqlite3BenchmarkThread.Create(True,
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
    Sqlite3ConnectionPoolClient := TALSqlite3ConnectionPoolClient.Create(AnsiString(ALEditSqlite3Database.text),
                                                                         SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE,
                                                                         aPragmaStatements,
                                                                         AnsiString(ALEditSqlite3lib.Text));

    //enable or disable the shared cache
    Sqlite3ConnectionPoolClient.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';

  //launch all the thread
  for i := 1 to StrToInt(ALEditSqlite3NBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStr(i) + ' (on)');
    aSqlite3BenchmarkThread := TSqlite3BenchmarkThread.Create(True,
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

{******************************************************************}
constructor TSqlite3BenchmarkThread.Create(CreateSuspended: Boolean;
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
Var aconnectionHandle: PSQLite3;
    aStopWatch: TStopWatch;
    aLoopIndex: integer;
    aCommitLoopIndex: integer;
    aLstSql: TALStrings;
    aTmpLstSql: TALStrings;
    aXMLDATA: TalXmlDocument;
    aSelectDataSQLs: TalSqlite3ClientSelectDataSQLs;
    aFormatSettings: TALFormatSettings;
    S1: AnsiString;
    j: integer;
begin

  //init the aFormatSettings
  ALGetLocaleFormatSettings(1033, aFormatSettings);

  //init the fNBLoopBeforeCommit
  if fNBLoopBeforeCommit <= 0 then fNBLoopBeforeCommit := 1;

  //create local object
  aLstSql := TALStringList.create;
  aXMLDATA := TALXmlDocument.create('root');
  Try

    //start the loop;
    aLoopIndex := 1;
    while aLoopIndex <= fMaxLoop do begin
      try

        //update the aLstSql
        aLstSql.clear;
        setlength(aSelectDataSQLs,0);
        for aCommitLoopIndex := 1 to fNBLoopBeforeCommit do begin
          aTmpLstSql := TALStringList.Create;
          Try
            S1 := AlStringReplace(fSQL,#13#10,' ',[RfReplaceALL]);
            aTmpLstSql.Text := ALTrim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));
            for J := 0 to aTmpLstSql.Count - 1 do begin
              S1 := aTmpLstSql[j];
              S1 := ALFastTagReplace(S1,
                                     '<#',
                                     '>',
                                     SQLFastTagReplaceFunct,
                                     True,
                                     nil);
              aLstSql.Add(S1);
              setlength(aSelectDataSQLs,length(aSelectDataSQLs)+1);
              aSelectDataSQLs[length(aSelectDataSQLs)-1].Sql := S1;
              aSelectDataSQLs[length(aSelectDataSQLs)-1].RowTag := '';
              aSelectDataSQLs[length(aSelectDataSQLs)-1].viewTag := '';
              aSelectDataSQLs[length(aSelectDataSQLs)-1].skip := 0;
              aSelectDataSQLs[length(aSelectDataSQLs)-1].First := 0;
            end;
            inc(aLoopIndex);
            inc(FTotalLoop);
            if aLoopIndex > fMaxLoop then break;
          Finally
            aTmpLstSql.Free;
          End;
        end;

        //start the Transaction
        Tform1(fOwner).Sqlite3ConnectionPoolClient.TransactionStart(aconnectionHandle);
        try

          //update the data
          aStopWatch := TStopWatch.StartNew;
          if fUpdateSQL then Tform1(fOwner).Sqlite3ConnectionPoolClient.UpdateData(aLstSql, aconnectionHandle)
          else Tform1(fOwner).Sqlite3ConnectionPoolClient.SelectData(aSelectDataSQLs,
                                                                     aXMLDATA.documentElement,
                                                                     aFormatSettings,
                                                                     aconnectionHandle);
          aStopWatch.Stop;
          FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + aStopWatch.ElapsedMilliseconds;

          //commit the data
          aStopWatch := TStopWatch.StartNew;
          Tform1(fOwner).Sqlite3ConnectionPoolClient.Transactioncommit(aconnectionHandle);
          aStopWatch.Stop;
          FTotalCommitTimeTaken := FTotalCommitTimeTaken + aStopWatch.ElapsedMilliseconds;

        Except
          //roolBack the data
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
      Synchronize(UpdateGUI);
      If ((not fUpdateSQL) and (Tform1(fOwner).ALButtonSqlite3LoopSelect.tag = 2)) or
         ((fUpdateSQL) and (Tform1(fOwner).ALButtonSqlite3LoopUpdate.tag = 2)) then Break;
    end;

  Finally
    aLstSql.free;
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
        TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,ALIntToStr(frank) + ' (off)');
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).ALButtonSqlite3LoopUpdate.Tag := 0;
          TForm1(fOwner).ALButtonSqlite3LoopUpdate.Caption := 'Loop UPDATE';
        end;
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).Sqlite3ConnectionPoolClient.Free;
          TForm1(fOwner).Sqlite3ConnectionPoolClient := nil;
        end;
      end;
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageExecuteTimeTaken.Index,FTotalExecuteTimeTaken / FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FTotalLoop);
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
        TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,ALIntToStr(frank) + ' (off)');
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).ALButtonSqlite3LoopSelect.Tag := 0;
          TForm1(fOwner).ALButtonSqlite3LoopSelect.Caption := 'Loop SELECT';
        end;
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).Sqlite3ConnectionPoolClient.Free;
          TForm1(fOwner).Sqlite3ConnectionPoolClient := nil;
        end;
      end;
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageExecuteTimeTaken.Index,FTotalExecuteTimeTaken / FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadErrorMsg.Index,FErrorMsg);
      TForm1(fOwner).StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
    finally
      TForm1(fOwner).TableViewThread.EndUpdate;
    end;
  end;
  application.ProcessMessages;
end;

{*******************************************************************}
constructor TFirebirdBenchmarkThread.Create(CreateSuspended: Boolean;
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

  {---------------------------------------------------------}
  function internalDoTagReplace(Str: AnsiString): AnsiString;
  Begin
    Str := ALFastTagReplace(Str,
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
    aSelectDataSQLs: TalFBXClientSelectDataSQLs;
    aUpdateDataSQLs: TalFBXClientUpdateDataSQLs;
    aTmpSelectDataSQLs: TalFBXClientSelectDataSQLs;
    aTmpUpdateDataSQLs: TalFBXClientUpdateDataSQLs;
    aStatementPool: TALFBXConnectionStatementPoolBinTree;
    aStatementPoolNode: TALStringKeyAVLBinaryTreeNode;
    aFormatSettings: TALFormatSettings;
    aStmtHandle: IscStmtHandle;
    aSqlda: TALFBXSQLResult;
    S1: AnsiString;
    j, k: integer;
    aLst1: TALStringList;

begin

  //init the aFormatSettings
  ALGetLocaleFormatSettings(1033, aFormatSettings);

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
        setlength(aSelectDataSQLs,0);
        setlength(aUpdateDataSQLs,0);
        if fParams = '' then begin
          aLst1 := TALStringList.Create;
          Try
            S1 := AlStringReplace(fSQL,#13#10,' ',[RfReplaceALL]);
            aLst1.Text := ALTrim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));
            for J := 0 to aLst1.Count - 1 do begin
              if fUpdateSQL then begin
                setlength(aUpdateDataSQLs,length(aUpdateDataSQLs)+1);
                setlength(aUpdateDataSQLs[length(aUpdateDataSQLs)-1].Params,0);
                aUpdateDataSQLs[length(aUpdateDataSQLs)-1].Sql := S1;
              end
              else begin
                setlength(aSelectDataSQLs,length(aSelectDataSQLs)+1);
                setlength(aSelectDataSQLs[length(aSelectDataSQLs)-1].Params,0);
                aSelectDataSQLs[length(aSelectDataSQLs)-1].Sql := S1;
                aSelectDataSQLs[length(aSelectDataSQLs)-1].RowTag := '';
                aSelectDataSQLs[length(aSelectDataSQLs)-1].viewTag := '';
                aSelectDataSQLs[length(aSelectDataSQLs)-1].skip := 0;
                aSelectDataSQLs[length(aSelectDataSQLs)-1].First := 0;
              end;
            end;
          Finally
            aLst1.Free;
          End;
        end
        else begin
          aLst1 := TALStringList.Create;
          Try
            aLst1.Text := ALTrim(fParams);
            if fUpdateSQL then begin
              setlength(aUpdateDataSQLs,1);
              setlength(aUpdateDataSQLs[0].Params,aLst1.Count);
              for J := 0 to aLst1.Count - 1 do begin
                aUpdateDataSQLs[0].Params[j].Value := aLst1[j];
                aUpdateDataSQLs[0].Params[j].isnull := false;
              end;
              aUpdateDataSQLs[0].Sql := fSQL;
            end
            else begin
              setlength(aSelectDataSQLs,1);
              setlength(aSelectDataSQLs[0].Params,aLst1.Count);
              for J := 0 to aLst1.Count - 1 do begin
                aSelectDataSQLs[0].Params[j].Value := aLst1[j];
                aSelectDataSQLs[0].Params[j].isnull := false;
              end;
              aSelectDataSQLs[0].Sql := fSQL;
              aSelectDataSQLs[0].RowTag := '';
              aSelectDataSQLs[0].viewTag := '';
              aSelectDataSQLs[0].skip := 0;
              aSelectDataSQLs[0].First := 0;
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
            if (length(aUpdateDataSQLs) = 1) and (alPos('<#', aUpdateDataSQLs[0].SQL) <= 0) then begin
              aStopWatch := TStopWatch.StartNew;
              Tform1(fOwner).FirebirdConnectionPoolClient.Prepare(aUpdateDataSQLs[0].SQL,
                                                                  aDBHandle,
                                                                  aTraHandle,
                                                                  aStmtHandle,
                                                                  aSqlda,
                                                                  fTPB);
              aStopWatch.Stop;
              if FTotalPrepareTimeTaken = -1 then FTotalPrepareTimeTaken := 0;
              FTotalPrepareTimeTaken := FTotalPrepareTimeTaken + aStopWatch.ElapsedMilliseconds;
            end;
          end
          else begin
            if (length(aSelectDataSQLs) = 1) and (alPos('<#', aSelectDataSQLs[0].SQL) <= 0) then begin
              aStopWatch := TStopWatch.StartNew;
              Tform1(fOwner).FirebirdConnectionPoolClient.Prepare(aSelectDataSQLs[0].SQL,
                                                                  aDBHandle,
                                                                  aTraHandle,
                                                                  aStmtHandle,
                                                                  aSqlda,
                                                                  fTPB);
              aStopWatch.Stop;
              if FTotalPrepareTimeTaken = -1 then FTotalPrepareTimeTaken := 0;
              FTotalPrepareTimeTaken := FTotalPrepareTimeTaken + aStopWatch.ElapsedMilliseconds;
            end;
          end;
          try


            //Execute the SQL
            for aCommitLoopIndex := 1 to fNBLoopBeforeCommit do begin
              setlength(aTmpSelectDataSQLs, length(aSelectDataSQLs));
              for j := 0 to length(aTmpSelectDataSQLs) - 1 do begin
                aTmpSelectDataSQLs[j].SQL := internalDoTagReplace(aSelectDataSQLs[j].SQL);
                setlength(aTmpSelectDataSQLs[j].Params, length(aSelectDataSQLs[j].Params));
                for k := 0 to length(aTmpSelectDataSQLs[j].Params) - 1 do begin
                  aTmpSelectDataSQLs[j].Params[k].Value := internalDoTagReplace(aSelectDataSQLs[j].Params[k].Value);
                  aTmpSelectDataSQLs[j].Params[k].isnull := aSelectDataSQLs[j].Params[k].isnull;
                end;
                aTmpSelectDataSQLs[j].RowTag := aSelectDataSQLs[j].RowTag;
                aTmpSelectDataSQLs[j].ViewTag := aSelectDataSQLs[j].ViewTag;
                aTmpSelectDataSQLs[j].Skip := aSelectDataSQLs[j].Skip;
                aTmpSelectDataSQLs[j].First := aSelectDataSQLs[j].First;
              end;
              setlength(aTmpUpdateDataSQLs, length(aUpdateDataSQLs));
              for j := 0 to length(aTmpUpdateDataSQLs) - 1 do begin
                aTmpUpdateDataSQLs[j].SQL := internalDoTagReplace(aUpdateDataSQLs[j].SQL);
                setlength(aTmpUpdateDataSQLs[j].Params, length(aUpdateDataSQLs[j].Params));
                for k := 0 to length(aTmpUpdateDataSQLs[j].Params) - 1 do begin
                  aTmpUpdateDataSQLs[j].Params[k].Value := internalDoTagReplace(aUpdateDataSQLs[j].Params[k].Value);
                  aTmpUpdateDataSQLs[j].Params[k].isnull := aUpdateDataSQLs[j].Params[k].isnull;
                end;
              end;

              if assigned(aStmtHandle) then begin
                aStatementPool := TALFBXConnectionStatementPoolBinTree.Create;
                aStatementPoolNode := TALFBXConnectionStatementPoolBinTreeNode.Create;
                if fUpdateSQL then aStatementPoolNode.ID := aTmpUpdateDataSQLs[0].SQL
                else aStatementPoolNode.ID := aTmpSelectDataSQLs[0].SQL;
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
                  if fUpdateSQL then Tform1(fOwner).FirebirdConnectionPoolClient.UpdateData(aTmpUpdateDataSQLs,
                                                                                            aDBHandle,
                                                                                            aTraHandle,
                                                                                            aStatementPool,
                                                                                            fTPB);
                  aStopWatch.Stop;
                  FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + aStopWatch.ElapsedMilliseconds;
                end
                else begin
                  aStopWatch := TStopWatch.StartNew;
                  Tform1(fOwner).FirebirdConnectionPoolClient.SelectData(aTmpSelectDataSQLs,
                                                                         aXMLDATA.documentElement,
                                                                         aFormatSettings,
                                                                         aDbHandle,
                                                                         aTraHandle,
                                                                         aStatementPool,
                                                                         fTPB);
                  aStopWatch.Stop;
                  FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + aStopWatch.ElapsedMilliseconds;
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
          FTotalCommitTimeTaken := FTotalCommitTimeTaken + aStopWatch.ElapsedMilliseconds;

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
      Synchronize(UpdateGUI);
      If ((not fUpdateSQL) and (Tform1(fOwner).ALButtonFirebirdLoopSelect.tag = 2)) or
         ((fUpdateSQL) and (Tform1(fOwner).ALButtonFirebirdLoopUpdate.tag = 2)) then Break;
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
        TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,ALIntToStr(frank) + ' (off)');
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).ALButtonFirebirdLoopUpdate.Tag := 0;
          TForm1(fOwner).ALButtonFirebirdLoopUpdate.Caption := 'Loop UPDATE';
        end;
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).FirebirdConnectionPoolClient.Free;
          TForm1(fOwner).FirebirdConnectionPoolClient := nil;
        end;
      end;
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
      if FTotalPrepareTimeTaken >= 0 then TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,FTotalPrepareTimeTaken / FTotalLoop)
      else  TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageExecuteTimeTaken.Index,FTotalExecuteTimeTaken / FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FTotalLoop);
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
        TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,ALIntToStr(frank) + ' (off)');
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).ALButtonFirebirdLoopSelect.Tag := 0;
          TForm1(fOwner).ALButtonFirebirdLoopSelect.Caption := 'Loop SELECT';
        end;
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).FirebirdConnectionPoolClient.Free;
          TForm1(fOwner).FirebirdConnectionPoolClient := nil;
        end;
      end;
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
      if FTotalPrepareTimeTaken >= 0 then TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,FTotalPrepareTimeTaken / FTotalLoop)
      else  TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageExecuteTimeTaken.Index,FTotalExecuteTimeTaken / FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadErrorMsg.Index,FErrorMsg);
      TForm1(fOwner).StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
    finally
      TForm1(fOwner).TableViewThread.EndUpdate;
    end;
  end;
  application.ProcessMessages;
end;

{****************************************************************}
constructor TMySqlBenchmarkThread.Create(CreateSuspended: Boolean;
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
    aLstSql: TALStrings;
    aTmpLstSql: TALStrings;
    aXMLDATA: TalXmlDocument;
    aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
    aFormatSettings: TALFormatSettings;
    S1: AnsiString;
    j: integer;
begin

  //init the aFormatSettings
  ALGetLocaleFormatSettings(1033, aFormatSettings);

  //init the fNBLoopBeforeCommit
  if fNBLoopBeforeCommit <= 0 then fNBLoopBeforeCommit := 1;

  //create local object
  aLstSql := TALStringList.create;
  aXMLDATA := TALXmlDocument.create('root');
  Try

    //start the loop;
    aLoopIndex := 1;
    while aLoopIndex <= fMaxLoop do begin
      try

        //update the aLstSql
        aLstSql.clear;
        setlength(aSelectDataSQLs,0);
        for aCommitLoopIndex := 1 to fNBLoopBeforeCommit do begin
          aTmpLstSql := TALStringList.Create;
          Try
            S1 := AlStringReplace(fSQL,#13#10,' ',[RfReplaceALL]);
            aTmpLstSql.Text := ALTrim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));
            for J := 0 to aTmpLstSql.Count - 1 do begin
              S1 := aTmpLstSql[j];
              S1 := ALFastTagReplace(S1,
                                     '<#',
                                     '>',
                                     SQLFastTagReplaceFunct,
                                     True,
                                     nil);
              aLstSql.Add(S1);
              setlength(aSelectDataSQLs,length(aSelectDataSQLs)+1);
              aSelectDataSQLs[length(aSelectDataSQLs)-1].Sql := S1;
              aSelectDataSQLs[length(aSelectDataSQLs)-1].RowTag := '';
              aSelectDataSQLs[length(aSelectDataSQLs)-1].viewTag := '';
              aSelectDataSQLs[length(aSelectDataSQLs)-1].skip := 0;
              aSelectDataSQLs[length(aSelectDataSQLs)-1].First := 0;
            end;
            inc(aLoopIndex);
            inc(FTotalLoop);
            if aLoopIndex > fMaxLoop then break;
          Finally
            aTmpLstSql.Free;
          End;
        end;

        //start the Transaction
        Tform1(fOwner).MySqlConnectionPoolClient.TransactionStart(aConnectionHandle);
        try

          //update the data
          aStopWatch := TStopWatch.StartNew;
          if fUpdateSQL then Tform1(fOwner).MySqlConnectionPoolClient.UpdateData(aLstSql, aConnectionHandle)
          else Tform1(fOwner).MySqlConnectionPoolClient.SelectData(aSelectDataSQLs,
                                                                   aXMLDATA.documentElement,
                                                                   aFormatSettings,
                                                                   aConnectionHandle);
          aStopWatch.Stop;
          FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + aStopWatch.ElapsedMilliseconds;

          //commit the data
          aStopWatch := TStopWatch.StartNew;
          Tform1(fOwner).MySqlConnectionPoolClient.Transactioncommit(aConnectionHandle);
          aStopWatch.Stop;
          FTotalCommitTimeTaken := FTotalCommitTimeTaken + aStopWatch.ElapsedMilliseconds;

        Except
          //roolBack the data
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
      Synchronize(UpdateGUI); // <= it's seam to be a source of bottleneck with mysql !! MYSQL IT'S A BULLSHEET !!
      If ((not fUpdateSQL) and ((Tform1(fOwner).ALButtonMySqlLoopSelect.tag = 2) or (Tform1(fOwner).ALButtonSphinxLoopSelect.tag = 2))) or
         ((fUpdateSQL) and ((Tform1(fOwner).ALButtonMySqlLoopUpdate.tag = 2) or (Tform1(fOwner).ALButtonSphinxLoopUpdate.tag = 2))) then Break;
    end;

  Finally
    aLstSql.free;
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
        TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,ALIntToStr(frank) + ' (off)');
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).ALButtonMySqlLoopUpdate.Tag := 0;
          TForm1(fOwner).ALButtonMySqlLoopUpdate.Caption := 'Loop UPDATE';
          TForm1(fOwner).ALButtonSphinxLoopUpdate.Tag := 0;
          TForm1(fOwner).ALButtonSphinxLoopUpdate.Caption := 'Loop UPDATE';
        end;
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).MySqlConnectionPoolClient.Free;
          TForm1(fOwner).MySqlConnectionPoolClient := nil;
        end;
      end;
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageExecuteTimeTaken.Index,FTotalExecuteTimeTaken / FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FTotalLoop);
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
        TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,ALIntToStr(frank) + ' (off)');
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).ALButtonMySqlLoopSelect.Tag := 0;
          TForm1(fOwner).ALButtonMySqlLoopSelect.Caption := 'Loop SELECT';
          TForm1(fOwner).ALButtonSphinxLoopSelect.Tag := 0;
          TForm1(fOwner).ALButtonSphinxLoopSelect.Caption := 'Loop SELECT';
        end;
        if TForm1(fOwner).NBActiveThread = 0 then begin
          TForm1(fOwner).MySqlConnectionPoolClient.Free;
          TForm1(fOwner).MySqlConnectionPoolClient := nil;
        end;
      end;
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageExecuteTimeTaken.Index,FTotalExecuteTimeTaken / FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FTotalLoop);
      TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadErrorMsg.Index,FErrorMsg);
      TForm1(fOwner).StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
    finally
      TForm1(fOwner).TableViewThread.EndUpdate;
    end;
  end;
  application.ProcessMessages;
end;

{********************************************************************}
constructor TMemcachedBenchmarkThread.Create(CreateSuspended: Boolean;
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
      aKey := ALFastTagReplace(fKey,
                               '<#',
                               '>',
                               SQLFastTagReplaceFunct,
                               True,
                               nil);
      aflags := alStrToInt(ALFastTagReplace(fflags,
                                            '<#',
                                            '>',
                                            SQLFastTagReplaceFunct,
                                            True,
                                            nil));
      aexpTime := alStrToInt(ALFastTagReplace(fexpTime,
                                              '<#',
                                              '>',
                                              SQLFastTagReplaceFunct,
                                              True,
                                              nil));
      aData := ALFastTagReplace(fData,
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
      FTotalExecuteTimeTaken := FTotalExecuteTimeTaken + aStopWatch.ElapsedMilliseconds;

      //update FTotalLoop
      inc(FTotalLoop);

    Except
      on e: Exception do begin
        FErrorMsg := AnsiString(E.message);
        Synchronize(UpdateGUI);
        Exit;
      end;
    end;
    if (FTotalLoop mod 100 = 0) or (FTotalLoop = fMaxLoop) then Synchronize(UpdateGUI);
    If (Tform1(fOwner).ALButtonMemcachedLoopGet.tag = 2) or
       (Tform1(fOwner).ALButtonMemcachedLoopSet.tag = 2) or
       (Tform1(fOwner).ALButtonMemcachedLoopIncr.tag = 2) or
       (Tform1(fOwner).ALButtonMemcachedLoopDecr.tag = 2) then Break;
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
      TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,ALIntToStr(frank) + ' (off)');
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
        end
      end;
      if TForm1(fOwner).NBActiveThread = 0 then begin
        TForm1(fOwner).MemcachedConnectionPoolClient.Free;
        TForm1(fOwner).MemcachedConnectionPoolClient := nil;
      end;
    end;
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadCount.Index,FTotalLoop);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAveragePrepareTimeTaken.Index,0/0);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageExecuteTimeTaken.Index,ALIfThen(FTotalLoop > 0, FTotalExecuteTimeTaken / FTotalLoop, 0));
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
    MemcachedConnectionPoolClient := TALMemcachedConnectionPoolClient.Create(AnsiString(ALEditMemcachedHost.Text),
                                                                             StrToInt(ALEditMemcachedPort.Text));

  //clear the status bar
  StatusBar1.Panels[1].Text := '';

  //launch all the thread
  for i := 1 to StrToInt(ALEditMemcachedNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStr(i) + ' (on)');
    aMemcachedBenchmarkThread := TMemcachedBenchmarkThread.Create(True,
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
    MemcachedConnectionPoolClient := TALMemcachedConnectionPoolClient.Create(AnsiString(ALEditMemcachedHost.Text),
                                                                             StrToInt(ALEditMemcachedPort.Text));

  //clear the status bar
  StatusBar1.Panels[1].Text := '';

  //launch all the thread
  for i := 1 to StrToInt(ALEditMemcachedNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStr(i) + ' (on)');
    aMemcachedBenchmarkThread := TMemcachedBenchmarkThread.Create(True,
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
    MemcachedConnectionPoolClient := TALMemcachedConnectionPoolClient.Create(AnsiString(ALEditMemcachedHost.Text),
                                                                             StrToInt(ALEditMemcachedPort.Text));

  //clear the status bar
  StatusBar1.Panels[1].Text := '';

  //launch all the thread
  for i := 1 to StrToInt(ALEditMemcachedNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStr(i) + ' (on)');
    aMemcachedBenchmarkThread := TMemcachedBenchmarkThread.Create(True,
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
    MemcachedConnectionPoolClient := TALMemcachedConnectionPoolClient.Create(AnsiString(ALEditMemcachedHost.Text),
                                                                             StrToInt(ALEditMemcachedPort.Text));

  //clear the status bar
  StatusBar1.Panels[1].Text := '';

  //launch all the thread
  for i := 1 to StrToInt(ALEditMemcachedNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,ALIntToStr(i) + ' (on)');
    aMemcachedBenchmarkThread := TMemcachedBenchmarkThread.Create(True,
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
      aKey := ALFastTagReplace(AnsiString(ALEditMemCachedKey.Text),
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
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.ElapsedMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
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
      aKey := ALFastTagReplace(AnsiString(ALEditMemCachedKey.Text),
                               '<#',
                               '>',
                               SQLFastTagReplaceFunct,
                               True,
                               nil);
      aflags := alStrToInt(ALFastTagReplace(AnsiString(ALEditMemCachedflags.Text),
                                            '<#',
                                            '>',
                                            SQLFastTagReplaceFunct,
                                            True,
                                            nil));
      aexpTime := alStrToInt(ALFastTagReplace(AnsiString(ALEditMemCachedexpTime.Text),
                                              '<#',
                                              '>',
                                              SQLFastTagReplaceFunct,
                                              True,
                                              nil));
      aData := ALFastTagReplace(AnsiString(ALMemoMemCachedData.Lines.Text),
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
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.ElapsedMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
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
      aKey := ALFastTagReplace(AnsiString(ALEditMemCachedKey.Text),
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
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.ElapsedMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
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

{*********************************************************}
procedure TForm1.ALButtonMongoDBFindClick(Sender: TObject);
Var aMongoDBClient: TAlMongoDBClient;
    aStopWatch: TStopWatch;
    aKey: AnsiString;
    aflags: integer;
    aData: AnsiString;
    aFound: boolean;
begin
  Screen.Cursor := CrHourGlass;
  try

    aMongoDBClient := TAlMongoDBClient.create;
    Try
      aMongoDBClient.Connect(AnsiString(ALEditMongoDBHost.Text), StrToInt(ALEditMongoDBPort.Text));
      aStopWatch := TStopWatch.StartNew;
      aMongoDBClient.DoRunDBCommand(aMongoDBClient.FSocketDescriptor, 'mwx1.items', '');
      aStopWatch.Stop;
      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAveragePrepareTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageExecuteTimeTaken.Index,aStopWatch.ElapsedMilliseconds);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0/0);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Client Memory Usage: ' + IntToStr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
      AlMemoResult.Lines.Clear;
      if aFound then begin
        AlMemoResult.Lines.Add('Found: true');
        AlMemoResult.Lines.Add('Flags: ' + intToStr(aFlags));
        AlMemoResult.Lines.Add('Data: ' + string(aData));
      end
      else AlMemoResult.Lines.Add('Found: false');
      aMongoDBClient.disconnect;
    Finally
      aMongoDBClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
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






{-------------------}
var ie: IWebBrowser2;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
var Url, Flags, TargetFrameName, PostData, Headers: OleVariant;
begin
  ie := CreateOleObject('InternetExplorer.Application') as IWebBrowser2;
  SetWindowLong(ie.hwnd, GWL_STYLE, GetWindowLong(ie.hwnd, GWL_STYLE) and not WS_BORDER and not WS_SIZEBOX and not WS_DLGFRAME );
  SetWindowPos(ie.hwnd, HWND_TOP, Left, Top, Width, Height, SWP_FRAMECHANGED);
  windows.setparent(ie.hwnd, PanelWebBrowser.handle);
  ie.Left := maxint; // don't understand why it's look impossible to setup the position
  ie.Top  := maxint; // don't understand why it's look impossible to setup the position
  ie.MenuBar := false;
  ie.AddressBar := false;
  ie.Resizable := false;
  ie.StatusBar := false;
  ie.ToolBar := 0;
  ie.Width := 100;
  ie.Height := 300;
  Url := 'http://static.arkadia.com/html/alcinoe_like.html';
  ie.Navigate2(Url,Flags,TargetFrameName,PostData,Headers);
  ie.Visible := true;
end;

{********************************************************************}
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    ie.quit;
  except
  end;
  sleep(500);
end;

initialization
  randomize;
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
