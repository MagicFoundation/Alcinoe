unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, AlScrollBar, ALMemo, ALButton,
  ALComboBox, ALEdit, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData,
  cxDataStorage, cxEdit, cxGridLevel, cxGridCustomTableView, cxGridTableView,
  cxClasses, cxControls, cxGridCustomView, cxGrid, ComCtrls, AlSqlite3Client,
  cxMemo, cxBlobEdit, alFbxClient, almysqlClient, OleCtrls, SHDocVw, ComObj,
  cxDropDownEdit;

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
    Panel6: TPanel;
    Splitter4: TSplitter;
    GridThreadSelect: TcxGrid;
    TableViewThreadSelect: TcxGridTableView;
    TableViewThreadSelectNumber: TcxGridColumn;
    TableViewThreadSelectRequestCount: TcxGridColumn;
    TableViewThreadSelectAverageSelectTimeTaken: TcxGridColumn;
    TableViewThreadSelectAverageCommitTimeTaken: TcxGridColumn;
    TableViewThreadSelectErrorMsg: TcxGridColumn;
    levelThreadSelect: TcxGridLevel;
    Splitter2: TSplitter;
    ALMemoResult: TALMemo;
    GridThreadUpdate: TcxGrid;
    TableViewThreadUpdate: TcxGridTableView;
    TableViewThreadUpdateNumber: TcxGridColumn;
    TableViewThreadUpdateRequestCount: TcxGridColumn;
    TableViewThreadUpdateAverageUpdateTimeTaken: TcxGridColumn;
    TableViewThreadUpdateAverageCommitTimeTaken: TcxGridColumn;
    TableViewThreadUpdateErrorMsg: TcxGridColumn;
    levelThreadUpdate: TcxGridLevel;
    PageControl1: TPageControl;
    MySQL: TTabSheet;
    SQLLite3: TTabSheet;
    Firebird: TTabSheet;
    Label2: TLabel;
    Label4: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label13: TLabel;
    Label26: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    ALButtonFirebirdSelect: TALButton;
    ALEditFirebirdLogin: TALEdit;
    ALEditFirebirdPassword: TALEdit;
    ALEditFirebirdCharset: TALEdit;
    ALEditFirebirdLib: TALEdit;
    ALMemoFireBirdQuery: TALMemo;
    ALEditFirebirdDatabase: TALEdit;
    ALButtonFirebirdLoopSelect: TALButton;
    ALButtonFirebirdUpdate: TALButton;
    ALButtonFirebirdLoopUpdate: TALButton;
    ALEditFirebirdNBLoop: TALEdit;
    ALEditFirebirdNbLoopBeforeCommit: TALEdit;
    ALEditFirebirdNBThread: TALEdit;
    ALEditFireBirdNum_buffers: TALEdit;
    ALButtonFirebirdCreateDatabase: TALButton;
    ALComboBoxFirebirdapiVer: TALComboBox;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label9: TLabel;
    Label12: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    ALEditMySqlHost: TALEdit;
    ALEditMySqlLogin: TALEdit;
    ALEditMySqlPassword: TALEdit;
    ALEditMySqlPort: TALEdit;
    ALEditMySqlCharset: TALEdit;
    ALEditMysqlLib: TALEdit;
    ALMemoMySqlQuery: TALMemo;
    ALButtonMySQLSelect: TALButton;
    ALEditMySqlDatabaseName: TALEdit;
    ALEditMySqlNBLoop: TALEdit;
    ALEditMySqlNbLoopBeforeCommit: TALEdit;
    ALEditMySqlNBThread: TALEdit;
    ALButtonMysqlUpdate: TALButton;
    ALButtonMySqlLoopUpdate: TALButton;
    ALButtonMysqlLoopSelect: TALButton;
    Label24: TLabel;
    Label25: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label28: TLabel;
    ALEditSqlite3Lib: TALEdit;
    ALMemoSqlite3Query: TALMemo;
    ALButtonSqlLite3Select: TALButton;
    ALEditSqlite3Database: TALEdit;
    ALButtonSqlite3LoopSelect: TALButton;
    ALButtonSqlite3Update: TALButton;
    ALButtonSqlite3LoopUpdate: TALButton;
    ALEditSQLite3NBLoop: TALEdit;
    RadioGroupSqlite3Journal_Mode: TRadioGroup;
    RadioGroupSQLite3Temp_Store: TRadioGroup;
    RadioGroupSqlite3Synhcronous: TRadioGroup;
    ALEditSqlite3Cache_Size: TALEdit;
    ALEditSqlite3Page_Size: TALEdit;
    ALEditSQLite3NbLoopBeforeCommit: TALEdit;
    ALEditSqlite3NBThread: TALEdit;
    ALCheckBoxSqlite3SharedCache: TALCheckBox;
    ALCheckBoxSqlite3ReadUncommited: TALCheckBox;
    Panel2: TPanel;
    Label5: TLabel;
    Label35: TLabel;
    Panel4: TPanel;
    PanelWebBrowser: TPanel;
    ALMemoFireBirdParams: TALMemo;
    Label1: TLabel;
    Label14: TLabel;
    TableViewThreadSelectAveragePrepareTimeTaken: TcxGridColumn;
    TableViewThreadUpdateAveragePrepareTimeTaken: TcxGridColumn;
    Label27: TLabel;
    ALMemoFirebirdTPB: TALMemo;
    procedure ALButtonPaint(Sender: TObject; var continue: Boolean);
    procedure FormClick(Sender: TObject);
    procedure ALButtonMySqlSelectClick(Sender: TObject);
    procedure ALEditButtonFindFileClick(Sender: TObject);
    procedure ALEditPaint(Sender: TObject; var continue: Boolean);
    procedure ALMemoPaint(Sender: TObject; var continue: Boolean);
    procedure ALButtonFirebirdSelectClick(Sender: TObject);
    procedure ALButtonFirebirdLoopSelectClick(Sender: TObject);
    procedure ALButtonFirebirdUpdateClick(Sender: TObject);
    procedure ALButtonFirebirdLoopUpdateClick(Sender: TObject);
    procedure ALButtonSqlLite3SelectClick(Sender: TObject);
    procedure ALButtonSqlite3LoopUpdateClick(Sender: TObject);
    procedure ALButtonSqlite3UpdateClick(Sender: TObject);
    procedure ALComboBoxPaint(Sender: TObject; var continue: Boolean);
    procedure ALButtonSqlite3LoopSelectClick(Sender: TObject);
    procedure ALCheckBoxSqlite3SharedCachePaint(Sender: TObject; var continue: Boolean);
    procedure ALMemoPaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
    procedure ALButtonFirebirdCreateDatabaseClick(Sender: TObject);
    procedure ALButtonMysqlUpdateClick(Sender: TObject);
    procedure ALButtonMySqlLoopUpdateClick(Sender: TObject);
    procedure ALButtonMysqlLoopSelectClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
  public
    Sqlite3ConnectionPoolClient: TalSqlite3ConnectionPoolClient;
    FirebirdConnectionPoolClient: TalFBXConnectionPoolClient;
    MySqlConnectionPoolClient: TalMySqlConnectionPoolClient;
    NBSelectActiveThread: integer;
    NBUpdateActiveThread: integer;
  end;

  {---------------------------------------}
  TFirebirdBenchmarkThread = Class(Tthread)
  private
    fSQL: String;
    fParams: String;
    fTPB: String;
    fUpdateSQL: Boolean;
    fOn: Boolean;
    fMaxLoop: integer;
    fNBLoopBeforeCommit: integer;
    FErrorMsg: String;
    FTotalPrepareTimeTaken: int64;
    FTotalUpdateOrSelectTimeTaken: int64;
    FTotalCommitTimeTaken: int64;
    FTotalUpdateOrSelect: integer;
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
                       aSQL: String;
                       aParams: String;
                       aTPB: String;
                       aMaxLoop: integer;
                       aNBLoopBeforeCommit: integer;
                       aUpdateSQL: Boolean);
    destructor Destroy; override;
  End;

  {--------------------------------------}
  TSqlite3BenchmarkThread = Class(Tthread)
  private
    fSQL: String;
    fUpdateSQL: Boolean;
    fOn: Boolean;
    fMaxLoop: integer;
    fNBLoopBeforeCommit: integer;
    FErrorMsg: String;
    FTotalUpdateOrSelectTimeTaken: int64;
    FTotalCommitTimeTaken: int64;
    FTotalUpdateOrSelect: integer;
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
                       aSQL: String;
                       aMaxLoop: integer;
                       aNBLoopBeforeCommit: integer;
                       aUpdateSQL: Boolean);
    destructor Destroy; override;
  End;

  {------------------------------------}
  TMySqlBenchmarkThread = Class(Tthread)
  private
    fSQL: String;
    fUpdateSQL: Boolean;
    fOn: Boolean;
    fMaxLoop: integer;
    fNBLoopBeforeCommit: integer;
    FErrorMsg: String;
    FTotalUpdateOrSelectTimeTaken: int64;
    FTotalCommitTimeTaken: int64;
    FTotalUpdateOrSelect: integer;
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
                       aSQL: String;
                       aMaxLoop: integer;
                       aNBLoopBeforeCommit: integer;
                       aUpdateSQL: Boolean);
    destructor Destroy; override;
  End;


function GetProcessMemoryInfo(Process : THandle; var MemoryCounters : TProcessMemoryCounters; cb : DWORD) : BOOL; stdcall;
function ProcessMemoryUsage(ProcessID : DWORD): DWORD;

var Form1: TForm1;

implementation

uses alFcnSkin,
     alWindows,
     ALFBXBase,
     ALFBXLib,
     ALMySqlWrapper,
     alSqlite3Wrapper,
     AlXmlDoc,
     alFcnString;

{$R *.dfm}

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
                               ProcessID
                              );
  try
    if GetProcessMemoryInfo(
                            ProcessHandle,
                            MemCounters,
                            sizeof(MemCounters)
                           )
    then Result := MemCounters.WorkingSetSize;
  finally
    CloseHandle(ProcessHandle);
  end;
end;

{****************************************************************************************************************************}
function SQLFastTagReplaceFunct(const TagString: string; TagParams: TStrings; ExtData: pointer; Var Handled: Boolean): string;
Var aMin, aMax, aIndex: integer;
    aLstSavedData: TstringList;
begin

  Handled := True;
  if sametext(TagString,'randomchar') then begin
    if not trystrtoint(TagParams.Values['Index'], aIndex) then aIndex := -1;
    if aIndex >= 0 then begin
      aLstSavedData := TstringList(ExtData^);
      result := aLstSavedData.Values['randomchar_'+inttostr(aIndex)];
      if result = '' then begin
        result := AlRandomStr(1);
        aLstSavedData.Values['randomchar_'+inttostr(aIndex)] := result;
      end;
    end
    else result := AlRandomStr(1);
  end
  else if sametext(TagString,'randomstring') then begin
    if not trystrtoint(TagParams.Values['MinLength'], aMin) then aMin := 1;
    if not trystrtoint(TagParams.Values['MaxLength'], aMax) then aMax := 255;
    if not trystrtoint(TagParams.Values['Index'], aIndex) then aIndex := -1;
    if aIndex >= 0 then begin
      aLstSavedData := TstringList(ExtData^);
      result := aLstSavedData.Values['randomstring_'+inttostr(aIndex)];
      if result = '' then begin
        result := AlRandomStr(aMin + random(aMax - aMin + 1));
        aLstSavedData.Values['randomstring_'+inttostr(aIndex)] := result;
      end;
    end
    else result := AlRandomStr(aMin + random(aMax - aMin + 1));
  end
  else if sametext(TagString,'randomnumber') then begin
    if not trystrtoint(TagParams.Values['Min'], aMin) then aMin := 1;
    if not trystrtoint(TagParams.Values['Max'], aMax) then aMax := Maxint;
    if not trystrtoint(TagParams.Values['Index'], aIndex) then aIndex := -1;
    if aIndex >= 0 then begin
      aLstSavedData := TstringList(ExtData^);
      result := aLstSavedData.Values['randomnumber_'+inttostr(aIndex)];
      if result = '' then begin
        result := inttostr(aMin + random(aMax - aMin + 1));
        aLstSavedData.Values['randomnumber_'+inttostr(aIndex)] := result;
      end;
    end
    else result := inttostr(aMin + random(aMax - aMin + 1));
  end
  else Handled := False;

end;

{************************************************************}
procedure TForm1.ALButtonFirebirdSelectClick(Sender: TObject);
Var aFBXClient: TALFbxClient;
    aXMLDATA: TalXmlDocument;
    aMainStartDate: int64;
    aStartPrepareDate: int64;
    aEndPrepareDate: int64;
    aStartSelectDate: int64;
    aEndSelectDate: int64;
    aStartCommitDate: int64;
    aEndCommitDate: int64;
    aFormatSettings: TformatSettings;
    aFBAPiVersion: TALFBXVersion_API;
    aSQL: TALFBXClientSelectDataSQL;
    aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aLst1: TStringList;
    i: integer;
    aTPB: String;
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

  aTPB:= trim(ALMemoFireBirdTPB.Lines.Text);
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

  GetLocaleFormatSettings(1033, aFormatSettings);
  Screen.Cursor := CrHourGlass;
  try

    aFBXClient := TALFbxClient.Create(aFBAPiVersion,ALEditFirebirdLib.Text);
    Try
      aFBXClient.connect(ALEditFireBirdDatabase.Text,
                         ALEditFireBirdLogin.text,
                         ALEditFireBirdPassword.text,
                         ALEditFireBirdCharset.Text,
                         StrtoInt(ALEditFireBirdNum_buffers.Text));

      aXMLDATA := ALCreateEmptyXMLDocument('root');
      Try

        With aXMLDATA Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
        end;

        aLst1 := TstringList.create;
        try
          aSQL.SQL := ALFastTagReplace(AlMemoFireBirdQuery.Lines.Text,
                                       '<#',
                                       '>',
                                       SQLFastTagReplaceFunct,
                                       True,
                                       @aLst1)
        finally
          aLst1.free;
        end;

        if ALMemoFireBirdParams.Lines.Count > 0 then begin
          Setlength(aSQL.Params, 1);
          Setlength(aSQL.Params[0].fields, ALMemoFireBirdParams.Lines.Count);
          for I := 0 to ALMemoFireBirdParams.Lines.Count - 1 do begin
            aLst1 := TstringList.create;
            try
              aSQL.Params[0].fields[i].Value := ALFastTagReplace(ALMemoFireBirdParams.Lines[i],
                                                                 '<#',
                                                                 '>',
                                                                 SQLFastTagReplaceFunct,
                                                                 True,
                                                                 @aLst1)
            finally
              aLst1.free;
            end;
            aSQL.Params[0].fields[i].isnull := False;
            aSQL.Params[0].fields[i].isblob := False;
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
        aMainStartDate := ALGetTickCount64;
        aFBXClient.TransactionStart(aTPB);
        try

          aStartPrepareDate := ALGetTickCount64;
          aFBXClient.Prepare(aSQL.SQL);
          aendPrepareDate := ALGetTickCount64;

          aStartSelectDate := ALGetTickCount64;
          aFBXClient.SelectData(aSQL,
                                aXMLDATA.DocumentElement,
                                aFormatSettings);
          aendSelectDate := ALGetTickCount64;

          aStartCommitDate := ALGetTickCount64;
          aFBXClient.TransactionCommit;
          aendCommitDate := ALGetTickCount64;

        Except
          aFBXClient.TransactionRollBack;
          Raise;
        end;
        ALMemoResult.Clear;
        ALMemoResult.Lines.Add('Time Taken: ' + inttostr(ALGetTickCount64 - aMainStartDate) + ' ms');
        aFBXClient.GetMonitoringInfos(aFBXClient.ConnectionID,
                                      -1,
                                      '',
                                      aIOStats_2,
                                      aRecordStats_2,
                                      aMemoryUsage);
        ALMemoResult.Lines.Add('');
        ALMemoResult.Lines.Add('page_reads:   ' + intToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
        ALMemoResult.Lines.Add('page_writes:  ' + intToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
        ALMemoResult.Lines.Add('page_fetches: ' + intToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
        ALMemoResult.Lines.Add('page_marks:   ' + intToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
        ALMemoResult.Lines.Add('');
        ALMemoResult.Lines.Add('record_idx_reads: ' + intToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
        ALMemoResult.Lines.Add('record_seq_reads: ' + intToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
        ALMemoResult.Lines.Add('record_inserts:   ' + intToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
        ALMemoResult.Lines.Add('record_updates:   ' + intToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
        ALMemoResult.Lines.Add('record_deletes:   ' + intToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
        ALMemoResult.Lines.Add('record_backouts:  ' + intToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
        ALMemoResult.Lines.Add('record_purges:    ' + intToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
        ALMemoResult.Lines.Add('record_expunges:  ' + intToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
        ALMemoResult.Lines.Add('');
        ALMemoResult.Lines.Add('memory_used:          ' + intToStr(aMemoryUsage.memory_used));
        ALMemoResult.Lines.Add('memory_allocated:     ' + intToStr(aMemoryUsage.memory_allocated));
        ALMemoResult.Lines.Add('max_memory_used:      ' + intToStr(aMemoryUsage.max_memory_used));
        ALMemoResult.Lines.Add('max_memory_allocated: ' + intToStr(aMemoryUsage.max_memory_allocated));
        AlMemoResult.Lines.add('');
        AlMemoResult.Lines.add('**************');
        AlMemoResult.Lines.add('');
        AlMemoResult.Lines.Text := AlMemoResult.Lines.Text + aXMLDATA.XML.Text;


        TableViewThreadUpdate.DataController.RecordCount := 0;
        TableViewThreadSelect.DataController.RecordCount := 1;
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectNumber.Index, '1 (off)');
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectRequestCount.Index,1);
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectAveragePrepareTimeTaken.Index,aEndPrepareDate - aStartPrepareDate);
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectAverageSelectTimeTaken.Index,aEndSelectDate - aStartSelectDate);
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectAverageCommitTimeTaken.Index,aendCommitDate - aStartCommitDate);
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';

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
    aMainStartDate: int64;
    aStartPrepareDate: int64;
    aEndPrepareDate: int64;
    aStartUpdateDate: int64;
    aEndUpdateDate: int64;
    aStartCommitDate: int64;
    aEndCommitDate: int64;
    aFormatSettings: TformatSettings;
    aFBAPiVersion: TALFBXVersion_API;
    aSQL: TALFBXClientUpdateDataSQL;
    aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aLst1: TStringList;
    aLstSql: Tstrings;
    i: integer;
    aTPB: String;
    S1: string;
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

  aTPB:= trim(ALMemoFireBirdTPB.Lines.Text);
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

  GetLocaleFormatSettings(1033, aFormatSettings);
  Screen.Cursor := CrHourGlass;
  try

    aFBXClient := TALFbxClient.Create(aFBAPiVersion,ALEditFirebirdLib.Text);
    aLstSql := TstringList.create;
    Try
      aFBXClient.connect(ALEditFireBirdDatabase.Text,
                         ALEditFireBirdLogin.text,
                         ALEditFireBirdPassword.text,
                         ALEditFireBirdCharset.Text,
                         StrtoInt(ALEditFireBirdNum_buffers.Text));

      aLst1 := TstringList.create;
      try
        aSQL.SQL := ALFastTagReplace(AlMemoFireBirdQuery.Lines.Text,
                                     '<#',
                                     '>',
                                     SQLFastTagReplaceFunct,
                                     True,
                                     @aLst1)
      finally
        aLst1.free;
      end;

      if ALMemoFireBirdParams.Lines.Count > 0 then begin
        Setlength(aSQL.Params, 1);
        Setlength(aSQL.Params[0].fields, ALMemoFireBirdParams.Lines.Count);
        for I := 0 to ALMemoFireBirdParams.Lines.Count - 1 do begin
          aLst1 := TstringList.create;
          try
            aSQL.Params[0].fields[i].Value := ALFastTagReplace(ALMemoFireBirdParams.Lines[i],
                                                               '<#',
                                                               '>',
                                                               SQLFastTagReplaceFunct,
                                                               True,
                                                               @aLst1)
          finally
            aLst1.free;
          end;
          aSQL.Params[0].fields[i].isnull := False;
          aSQL.Params[0].fields[i].isblob := False;
        end;
      end
      else Setlength(aSQL.Params, 0);

      if ALMemoFireBirdParams.Lines.Count <= 0 then begin
        if (AlPos('begin',AllowerCase(aSQL.SQL)) <= 0) or
           (AlPos('end',AllowerCase(aSQL.SQL)) <= 0) then begin
          S1 := AlStringReplace(aSQL.SQL,#13#10,' ',[RfReplaceALL]);
          aLstSql.Text := Trim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));
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
      aMainStartDate := ALGetTickCount64;
      aFBXClient.TransactionStart(aTPB);
      try

        aStartPrepareDate := ALGetTickCount64;
        if aLstSql.Count = 1 then aFBXClient.Prepare(aLstSql[0])
        else if aLstSql.Count <= 0 then aFBXClient.Prepare(aSQL.SQL);
        aendPrepareDate := ALGetTickCount64;

        aStartUpdateDate := ALGetTickCount64;
        if aLstSql.Count > 0 then aFBXClient.UpdateData(aLstSql)
        else aFBXClient.UpdateData(aSQL);
        aendUpdateDate := ALGetTickCount64;

        aStartCommitDate := ALGetTickCount64;
        aFBXClient.TransactionCommit;
        aendCommitDate := ALGetTickCount64;

      Except
        aFBXClient.TransactionRollBack;
        Raise;
      end;
      ALMemoResult.Clear;
      ALMemoResult.Lines.Add('Time Taken: ' + inttostr(ALGetTickCount64 - aMainStartDate) + ' ms');
      aFBXClient.GetMonitoringInfos(aFBXClient.ConnectionID,
                                    -1,
                                    '',
                                    aIOStats_2,
                                    aRecordStats_2,
                                    aMemoryUsage);
      ALMemoResult.Lines.Add('');
      ALMemoResult.Lines.Add('page_reads:   ' + intToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
      ALMemoResult.Lines.Add('page_writes:  ' + intToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
      ALMemoResult.Lines.Add('page_fetches: ' + intToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
      ALMemoResult.Lines.Add('page_marks:   ' + intToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
      ALMemoResult.Lines.Add('');
      ALMemoResult.Lines.Add('record_idx_reads: ' + intToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
      ALMemoResult.Lines.Add('record_seq_reads: ' + intToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
      ALMemoResult.Lines.Add('record_inserts:   ' + intToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
      ALMemoResult.Lines.Add('record_updates:   ' + intToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
      ALMemoResult.Lines.Add('record_deletes:   ' + intToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
      ALMemoResult.Lines.Add('record_backouts:  ' + intToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
      ALMemoResult.Lines.Add('record_purges:    ' + intToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
      ALMemoResult.Lines.Add('record_expunges:  ' + intToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
      ALMemoResult.Lines.Add('');
      ALMemoResult.Lines.Add('memory_used:          ' + intToStr(aMemoryUsage.memory_used));
      ALMemoResult.Lines.Add('memory_allocated:     ' + intToStr(aMemoryUsage.memory_allocated));
      ALMemoResult.Lines.Add('max_memory_used:      ' + intToStr(aMemoryUsage.max_memory_used));
      ALMemoResult.Lines.Add('max_memory_allocated: ' + intToStr(aMemoryUsage.max_memory_allocated));

      TableViewThreadSelect.DataController.RecordCount := 0;
      TableViewThreadUpdate.DataController.RecordCount := 1;
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateNumber.Index, '1 (off)');
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateRequestCount.Index,1);
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateAveragePrepareTimeTaken.Index,aEndPrepareDate - aStartPrepareDate);
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateAverageUpdateTimeTaken.Index,aEndUpdateDate - aStartUpdateDate);
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateAverageCommitTimeTaken.Index,aendCommitDate - aStartCommitDate);
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';

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

  aFBXClient := TALFbxClient.Create(aFBAPiVersion,ALEditFirebirdLib.Text);
  Try
    aFBXClient.CreateDatabase(AlMemoFireBirdQuery.Lines.Text);
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
    aTPB: String;
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

  aTPB:= trim(ALMemoFireBirdTPB.Lines.Text);
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
  TableViewThreadUpdate.DataController.RecordCount := 0;
  TableViewThreadSelect.DataController.RecordCount := strtoint(ALEditFirebirdNBThread.Text);

  //create the fFirebirdConnectionPoolClient
  if not assigned(FirebirdConnectionPoolClient) then begin
    FirebirdConnectionPoolClient := TALFBXConnectionPoolClient.Create(ALEditFireBirdDatabase.Text,
                                                                      ALEditFireBirdLogin.text,
                                                                      ALEditFireBirdPassword.text,
                                                                      ALEditFireBirdCharset.Text,
                                                                      aFBAPiVersion,
                                                                      ALEditFirebirdLib.Text,
                                                                      StrtoInt(ALEditFireBirdNum_buffers.Text));
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';

  //launch all the thread
  for i := 1 to strtoint(ALEditFirebirdNBThread.Text) do begin
    TableViewThreadSelect.DataController.SetValue(i-1,TableViewThreadSelectNumber.Index,inttostr(i) + ' (on)');
    aFirebirdBenchmarkThread := TFirebirdBenchmarkThread.Create(True,
                                                                self,
                                                                i,
                                                                trim(ALMemoFirebirdQuery.Lines.Text),
                                                                trim(ALMemoFirebirdParams.Lines.Text),
                                                                aTPB,
                                                                strtoint(ALEditFirebirdNBLoop.Text),
                                                                strtoint(ALEditFirebirdNbLoopBeforeCommit.Text),
                                                                false);
    inc(NBSelectActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBSelectActiveThread) + inttostr(NBUpdateActiveThread);
    StatusBar1.Repaint;
    aFirebirdBenchmarkThread.FreeOnTerminate := True;
    aFirebirdBenchmarkThread.Resume;
  end;

end;

{****************************************************************}
procedure TForm1.ALButtonFirebirdLoopUpdateClick(Sender: TObject);
Var aFirebirdBenchmarkThread: TFirebirdBenchmarkThread;
    i: integer;
    aFBAPiVersion: TALFBXVersion_API;
    aTPB: String;
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

  aTPB:= trim(ALMemoFireBirdTPB.Lines.Text);
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
  TableViewThreadSelect.DataController.RecordCount := 0;
  TableViewThreadUpdate.DataController.RecordCount := strtoint(ALEditFirebirdNBThread.Text);

  //create the fFirebirdConnectionPoolClient
  if not assigned(FirebirdConnectionPoolClient) then begin
    FirebirdConnectionPoolClient := TALFBXConnectionPoolClient.Create(ALEditFireBirdDatabase.Text,
                                                                      ALEditFireBirdLogin.text,
                                                                      ALEditFireBirdPassword.text,
                                                                      ALEditFireBirdCharset.Text,
                                                                      aFBAPiVersion,
                                                                      ALEditFirebirdLib.Text,
                                                                      StrtoInt(ALEditFireBirdNum_buffers.Text));
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';

  //launch all the thread
  for i := 1 to strtoint(ALEditFirebirdNBThread.Text) do begin
    TableViewThreadUpdate.DataController.SetValue(i-1,TableViewThreadUpdateNumber.Index,inttostr(i) + ' (on)');
    aFirebirdBenchmarkThread := TFirebirdBenchmarkThread.Create(True,
                                                                self,
                                                                i,
                                                                trim(ALMemoFirebirdQuery.Lines.Text),
                                                                trim(ALMemoFirebirdParams.Lines.Text),
                                                                aTPB,
                                                                strtoint(ALEditFirebirdNBLoop.Text),
                                                                strtoint(ALEditFirebirdNbLoopBeforeCommit.Text),
                                                                true);
    inc(NBUpdateActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBSelectActiveThread) + inttostr(NBUpdateActiveThread);
    StatusBar1.Repaint;
    aFirebirdBenchmarkThread.FreeOnTerminate := True;
    aFirebirdBenchmarkThread.Resume;
  end;

end;

{*********************************************************}
procedure TForm1.ALButtonMySqlSelectClick(Sender: TObject);
Var aMySqlClient: TalMySqlClient;
    aXMLDATA: TalXmlDocument;
    aStartDate: int64;
    aEndDate: int64;
    aFormatSettings: TformatSettings;
    S1: String;
    aLst1: TstringList;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  Screen.Cursor := CrHourGlass;
  try

    aMySqlClient := TalMySqlClient.Create(MYSQL50, ALEditMySqllib.Text);
    Try
      aMySqlClient.connect(ALEditMySqlHost.Text,
                           strtoint(ALEditMySqlPort.Text),
                           ALEditMySqlDatabaseName.Text,
                           ALEditMySqlLogin.Text,
                           ALEditMySqlPassword.Text,
                           ALEditMySqlCharset.Text,
                           0);

      aXMLDATA := ALCreateEmptyXMLDocument('root');
      Try

        With aXMLDATA Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
        end;

        aLst1 := TstringList.create;
        try
          S1 := ALFastTagReplace(AlMemoMySqlQuery.Lines.Text,
                                 '<#',
                                 '>',
                                 SQLFastTagReplaceFunct,
                                 True,
                                 @aLst1)
        finally
          aLst1.free;
        end;

        aStartDate := ALGetTickCount64;
        aMySqlClient.SelectData(S1,
                                'rec',
                                 0,
                                 200,
                                aXMLDATA.DocumentElement,
                                aFormatSettings);
        aEndDate := ALGetTickCount64;

        TableViewThreadUpdate.DataController.RecordCount := 0;
        TableViewThreadSelect.DataController.RecordCount := 1;
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectNumber.Index, '1 (off)');
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectRequestCount.Index,1);
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectAveragePrepareTimeTaken.Index,0/0);
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectAverageSelectTimeTaken.Index,aEndDate - aStartDate);
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectAverageCommitTimeTaken.Index,0/0);
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
        AlMemoResult.Lines.Text := aXMLDATA.XML.Text;

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
    aStartDate: int64;
    aEndDate: int64;
    aStartCommitDate: int64;
    aEndCommitDate: int64;
    LstSql: TstringList;
    S1: String;
    aLst1: TstringList;
begin
  Screen.Cursor := CrHourGlass;
  try

    aMySqlClient := TalMySqlClient.Create(MYSQL50, ALEditMySqllib.Text);
    LstSql := TstringList.Create;
    Try
      aMySqlClient.connect(ALEditMySqlHost.Text,
                           strtoint(ALEditMySqlPort.Text),
                           ALEditMySqlDatabaseName.Text,
                           ALEditMySqlLogin.Text,
                           ALEditMySqlPassword.Text,
                           ALEditMySqlCharset.Text,
                           0);

      aLst1 := TstringList.create;
      try
        S1 := ALFastTagReplace(AlMemoMySqlQuery.Lines.Text,
                               '<#',
                               '>',
                               SQLFastTagReplaceFunct,
                               True,
                               @aLst1)
      finally
        aLst1.free;
      end;
      S1 := AlStringReplace(S1,#13#10,' ',[RfReplaceALL]);
      LstSql.Text := Trim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));

      aStartDate := ALGetTickCount64;
      aMySqlClient.TransactionStart;
      try
        aMySqlClient.UpdateData(LstSql);
        aEndDate := ALGetTickCount64;
        aStartCommitDate := ALGetTickCount64;
        aMySqlClient.TransactionCommit;
        aendCommitDate := ALGetTickCount64;
      Except
        aMySqlClient.TransactionRollBack;
        raise;
      end;

      TableViewThreadSelect.DataController.RecordCount := 0;
      TableViewThreadUpdate.DataController.RecordCount := 1;
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateNumber.Index, '1 (off)');
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateRequestCount.Index,1);
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateAveragePrepareTimeTaken.Index,0/0);
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateAverageUpdateTimeTaken.Index,aEndDate - aStartDate);
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateAverageCommitTimeTaken.Index,aendCommitDate - aStartCommitDate);
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
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
    i: integer;
begin

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
  TableViewThreadUpdate.DataController.RecordCount := 0;
  TableViewThreadSelect.DataController.RecordCount := strtoint(ALEditMySqlNBThread.Text);

  //create the fMySqlConnectionPoolClient
  if not assigned(MySqlConnectionPoolClient) then begin
    MySqlConnectionPoolClient := TALMySqlConnectionPoolClient.Create(ALEditMySqlHost.Text,
                                                                     StrToInt(ALEditMySqlPort.Text),
                                                                     ALEditMySqlDatabaseName.Text,
                                                                     ALEditMySqlLogin.Text,
                                                                     ALEditMySqlPassword.Text,
                                                                     ALEditMySqlCharset.Text,
                                                                     MYSQL50,
                                                                     ALEditMySqlLib.Text,
                                                                     0);
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';

  //launch all the thread
  for i := 1 to strtoint(ALEditMySqlNBThread.Text) do begin
    TableViewThreadSelect.DataController.SetValue(i-1,TableViewThreadSelectNumber.Index,inttostr(i) + ' (on)');
    aMySqlBenchmarkThread := TMySqlBenchmarkThread.Create(True,
                                                          self,
                                                          i,
                                                          trim(ALMemoMySqlQuery.Lines.Text),
                                                          strtoint(ALEditMySqlNBLoop.Text),
                                                          strtoint(ALEditMySqlNbLoopBeforeCommit.Text),
                                                          false);
    inc(NBSelectActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBSelectActiveThread) + inttostr(NBUpdateActiveThread);
    StatusBar1.Repaint;
    aMySqlBenchmarkThread.FreeOnTerminate := True;
    aMySqlBenchmarkThread.Resume;
  end;

end;

{*************************************************************}
procedure TForm1.ALButtonMySqlLoopUpdateClick(Sender: TObject);
Var aMySqlBenchmarkThread: TMySqlBenchmarkThread;
    i: integer;
begin

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
  TableViewThreadSelect.DataController.RecordCount := 0;
  TableViewThreadUpdate.DataController.RecordCount := strtoint(ALEditMySqlNBThread.Text);

  //create the fMySqlConnectionPoolClient
  if not assigned(MySqlConnectionPoolClient) then begin
    MySqlConnectionPoolClient := TALMySqlConnectionPoolClient.Create(ALEditMySqlHost.Text,
                                                                     StrToInt(ALEditMySqlPort.Text),
                                                                     ALEditMySqlDatabaseName.Text,
                                                                     ALEditMySqlLogin.Text,
                                                                     ALEditMySqlPassword.Text,
                                                                     ALEditMySqlCharset.Text,
                                                                     MYSQL50,
                                                                     ALEditMySqlLib.Text,
                                                                     0);
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';

  //launch all the thread
  for i := 1 to strtoint(ALEditMySqlNBThread.Text) do begin
    TableViewThreadUpdate.DataController.SetValue(i-1,TableViewThreadUpdateNumber.Index,inttostr(i) + ' (on)');
    aMySqlBenchmarkThread := TMySqlBenchmarkThread.Create(True,
                                                          self,
                                                          i,
                                                          trim(ALMemoMySqlQuery.Lines.Text),
                                                          strtoint(ALEditMySqlNBLoop.Text),
                                                          strtoint(ALEditMySqlNbLoopBeforeCommit.Text),
                                                          true);
    inc(NBUpdateActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBSelectActiveThread) + inttostr(NBUpdateActiveThread);
    StatusBar1.Repaint;
    aMySqlBenchmarkThread.FreeOnTerminate := True;
    aMySqlBenchmarkThread.Resume;
  end;

end;

{************************************************************}
procedure TForm1.ALButtonSqlLite3SelectClick(Sender: TObject);
Var aSqlite3Client: TalSqlite3Client;
    aXMLDATA: TalXmlDocument;
    aStartDate: int64;
    aEndDate: int64;
    aFormatSettings: TformatSettings;
    S1: String;
    aLst1: TstringList;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  Screen.Cursor := CrHourGlass;
  try

    aSqlite3Client := TalSqlite3Client.Create(ALEditSqlite3lib.Text);
    Try

      //enable or disable the shared cache
      aSqlite3Client.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);

      //connect
      aSqlite3Client.connect(ALEditSqlite3Database.text);

      //the pragma
      aSqlite3Client.UpdateData('PRAGMA page_size = '+ALEditSqlite3Page_Size.Text);
      aSqlite3Client.UpdateData('PRAGMA encoding = "UTF-8"');
      aSqlite3Client.UpdateData('PRAGMA legacy_file_format = 0');
      aSqlite3Client.UpdateData('PRAGMA auto_vacuum = NONE');
      aSqlite3Client.UpdateData('PRAGMA cache_size = '+ALEditSqlite3Cache_Size.Text);
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
      aXMLDATA := ALCreateEmptyXMLDocument('root');
      Try

        With aXMLDATA Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
        end;

        aLst1 := TstringList.create;
        try
          S1 := ALFastTagReplace(AlMemoSQLite3Query.Lines.Text,
                                 '<#',
                                 '>',
                                 SQLFastTagReplaceFunct,
                                 True,
                                 @aLst1)
        finally
          aLst1.free;
        end;

        aStartDate := ALGetTickCount64;
        aSqlite3Client.SelectData(S1,
                                  'rec',
                                   0,
                                   200,
                                  aXMLDATA.DocumentElement,
                                  aFormatSettings);
        aEndDate := ALGetTickCount64;

        TableViewThreadUpdate.DataController.RecordCount := 0;
        TableViewThreadSelect.DataController.RecordCount := 1;
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectNumber.Index, '1 (off)');
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectRequestCount.Index,1);
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectAveragePrepareTimeTaken.Index,0/0);
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectAverageSelectTimeTaken.Index,aEndDate - aStartDate);
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectAverageCommitTimeTaken.Index,0/0);
        TableViewThreadSelect.DataController.SetValue(0,TableViewThreadSelectErrorMsg.Index,'');
        StatusBar1.Panels[1].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
        AlMemoResult.Lines.Text := aXMLDATA.XML.Text;

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
    aStartDate: int64;
    aEndDate: int64;
    aStartCommitDate: int64;
    aEndCommitDate: int64;
    LstSql: TstringList;
    S1: String;
    aLst1: TstringList;
begin
  Screen.Cursor := CrHourGlass;
  try

    aSqlite3Client := TalSqlite3Client.Create(ALEditSqlite3lib.Text);
    LstSql := TstringList.Create;
    Try

      //enable or disable the shared cache
      aSqlite3Client.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);

      //connect
      aSqlite3Client.connect(ALEditSqlite3Database.text);

      //the pragma
      aSqlite3Client.UpdateData('PRAGMA page_size = '+ALEditSqlite3Page_Size.Text);
      aSqlite3Client.UpdateData('PRAGMA encoding = "UTF-8"');
      aSqlite3Client.UpdateData('PRAGMA legacy_file_format = 0');
      aSqlite3Client.UpdateData('PRAGMA auto_vacuum = NONE');
      aSqlite3Client.UpdateData('PRAGMA cache_size = '+ALEditSqlite3Cache_Size.Text);
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
      aLst1 := TstringList.create;
      try
        S1 := ALFastTagReplace(AlMemoSQLite3Query.Lines.Text,
                               '<#',
                               '>',
                               SQLFastTagReplaceFunct,
                               True,
                               @aLst1)
      finally
        aLst1.free;
      end;
      S1 := AlStringReplace(S1,#13#10,' ',[RfReplaceALL]);
      LstSql.Text := Trim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));

      //do the job
      aStartDate := ALGetTickCount64;
      aSqlite3Client.TransactionStart;
      try
        aSqlite3Client.UpdateData(LstSql);
        aEndDate := ALGetTickCount64;
        aStartCommitDate := ALGetTickCount64;
        aSqlite3Client.TransactionCommit;
        aendCommitDate := ALGetTickCount64;
      Except
        aSqlite3Client.TransactionRollBack;
        raise;
      end;

      TableViewThreadSelect.DataController.RecordCount := 0;
      TableViewThreadUpdate.DataController.RecordCount := 1;
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateNumber.Index, '1 (off)');
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateRequestCount.Index,1);
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateAveragePrepareTimeTaken.Index,0/0);
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateAverageUpdateTimeTaken.Index,aEndDate - aStartDate);
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateAverageCommitTimeTaken.Index,aendCommitDate - aStartCommitDate);
      TableViewThreadUpdate.DataController.SetValue(0,TableViewThreadUpdateErrorMsg.Index,'');
      StatusBar1.Panels[1].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
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

{***************************************************************}
procedure TForm1.ALButtonSqlite3LoopSelectClick(Sender: TObject);
Var aSqlite3BenchmarkThread: TSqlite3BenchmarkThread;
    aPragmaStatements: String;
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
  TableViewThreadUpdate.DataController.RecordCount := 0;
  TableViewThreadSelect.DataController.RecordCount := strtoint(ALEditSqlite3NBThread.Text);

  //init the aPragmaStatements
  aPragmaStatements := '';
  aPragmaStatements := aPragmaStatements + 'PRAGMA page_size = '+ALEditSqlite3Page_Size.Text+';';
  aPragmaStatements := aPragmaStatements + 'PRAGMA encoding = "UTF-8";';
  aPragmaStatements := aPragmaStatements + 'PRAGMA legacy_file_format = 0;';
  aPragmaStatements := aPragmaStatements + 'PRAGMA auto_vacuum = NONE;';
  aPragmaStatements := aPragmaStatements + 'PRAGMA cache_size = '+ALEditSqlite3Cache_Size.Text+';';
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
    Sqlite3ConnectionPoolClient := TALSqlite3ConnectionPoolClient.Create(ALEditSqlite3Database.text,
                                                                         SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE,
                                                                         aPragmaStatements,
                                                                         ALEditSqlite3lib.Text);

    //enable or disable the shared cache
    Sqlite3ConnectionPoolClient.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';

  //launch all the thread
  for i := 1 to strtoint(ALEditSqlite3NBThread.Text) do begin
    TableViewThreadSelect.DataController.SetValue(i-1,TableViewThreadSelectNumber.Index,inttostr(i) + ' (on)');
    aSqlite3BenchmarkThread := TSqlite3BenchmarkThread.Create(True,
                                                              self,
                                                              i,
                                                              trim(ALMemoSqlite3Query.Lines.Text),
                                                              strtoint(ALEditSqlite3NBLoop.Text),
                                                              strtoint(ALEditSQLite3NbLoopBeforeCommit.Text),
                                                              false);
    inc(NBSelectActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBSelectActiveThread) + inttostr(NBUpdateActiveThread);
    StatusBar1.Repaint;
    aSqlite3BenchmarkThread.FreeOnTerminate := True;
    aSqlite3BenchmarkThread.Resume;
  end;

end;

{***************************************************************}
procedure TForm1.ALButtonSqlite3LoopUpdateClick(Sender: TObject);
Var aSqlite3BenchmarkThread: TSqlite3BenchmarkThread;
    aPragmaStatements: String;
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
  TableViewThreadSelect.DataController.RecordCount := 0;
  TableViewThreadUpdate.DataController.RecordCount := strtoint(ALEditSqlite3NBThread.Text);

  //init the aPragmaStatements
  aPragmaStatements := '';
  aPragmaStatements := aPragmaStatements + 'PRAGMA page_size = '+ALEditSqlite3Page_Size.Text+';';
  aPragmaStatements := aPragmaStatements + 'PRAGMA encoding = "UTF-8";';
  aPragmaStatements := aPragmaStatements + 'PRAGMA legacy_file_format = 0;';
  aPragmaStatements := aPragmaStatements + 'PRAGMA auto_vacuum = NONE;';
  aPragmaStatements := aPragmaStatements + 'PRAGMA cache_size = '+ALEditSqlite3Cache_Size.Text+';';
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
    Sqlite3ConnectionPoolClient := TALSqlite3ConnectionPoolClient.Create(ALEditSqlite3Database.text,
                                                                         SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE,
                                                                         aPragmaStatements,
                                                                         ALEditSqlite3lib.Text);

    //enable or disable the shared cache
    Sqlite3ConnectionPoolClient.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);
  end;

  //clear the status bar
  StatusBar1.Panels[1].Text := '';

  //launch all the thread
  for i := 1 to strtoint(ALEditSqlite3NBThread.Text) do begin
    TableViewThreadUpdate.DataController.SetValue(i-1,TableViewThreadUpdateNumber.Index,inttostr(i) + ' (on)');
    aSqlite3BenchmarkThread := TSqlite3BenchmarkThread.Create(True,
                                                              self,
                                                              i,
                                                              trim(ALMemoSqlite3Query.Lines.Text),
                                                              strtoint(ALEditSqlite3NBLoop.Text),
                                                              strtoint(ALEditSQLite3NbLoopBeforeCommit.Text),
                                                              true);
    inc(NBUpdateActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBSelectActiveThread) + inttostr(NBUpdateActiveThread);
    StatusBar1.Repaint;
    aSqlite3BenchmarkThread.FreeOnTerminate := True;
    aSqlite3BenchmarkThread.Resume;
  end;

end;

{*********************************************************************}
procedure TForm1.ALButtonPaint(Sender: TObject; var continue: Boolean);
begin
  paintAlButtonBlueSkin(sender, Continue);
end;

{**********************************************************}
procedure TForm1.ALEditButtonFindFileClick(Sender: TObject);
begin
  If OpenDialog1.Execute then (Sender as TALEdit).Text := OpenDialog1.FileName;
end;

{*******************************************************************}
procedure TForm1.ALEditPaint(Sender: TObject; var continue: Boolean);
begin
  PaintAlEditBlueSkin(Sender, Continue);
end;

{*******************************************************************}
procedure TForm1.ALMemoPaint(Sender: TObject; var continue: Boolean);
begin
  PaintALMemoBlueSkin(Sender, continue);
end;

{****************************************************************************************************}
procedure TForm1.ALMemoPaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
begin
  PaintALMemoScrollBarBlueSkin(Sender, continue, Area);
end;

{******************************************}
procedure TForm1.FormClick(Sender: TObject);
begin
  Windows.SetFocus(0);
end;

{*****************************************************************************************}
procedure TForm1.ALCheckBoxSqlite3SharedCachePaint(Sender: TObject; var continue: Boolean);
begin
  paintAlCheckBoxBlueSkin(sender, Continue);
end;

{************************************************************************}
procedure TForm1.ALComboBoxPaint(Sender: TObject; var continue: Boolean);
begin
  paintAlComboBoxBlueSkin(sender, Continue);
end;




///////////////////////////////////
///// TSqlite3BenchmarkThread /////
///////////////////////////////////

{******************************************************************}
constructor TSqlite3BenchmarkThread.Create(CreateSuspended: Boolean;
                                           AOwner: TwinControl;
                                           aRank: integer;
                                           aSQL: String;
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
  FTotalUpdateOrSelectTimeTaken := 0;
  FTotalCommitTimeTaken := 0;
  FTotalUpdateOrSelect := 0;
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
    aStartDate: int64;
    aEndDate: int64;
    aLoopIndex: integer;
    aCommitLoopIndex: integer;
    aLstSql: Tstrings;
    aTmpLstSql: Tstrings;
    aXMLDATA: TalXmlDocument;
    aSelectDataSQLs: TalSqlite3ClientSelectDataSQLs;
    aFormatSettings: TformatSettings;
    S1: String;

    j: integer;
    aLst1: TstringList;
begin

  //init the aFormatSettings
  GetLocaleFormatSettings(1033, aFormatSettings);

  //init the fNBLoopBeforeCommit
  if fNBLoopBeforeCommit <= 0 then fNBLoopBeforeCommit := 1;

  //create local object
  aLstSql := TstringList.create;
  aXMLDATA := ALCreateEmptyXMLDocument('root');
  Try

    //start the loop;
    aLoopIndex := 1;
    while aLoopIndex <= fMaxLoop do begin
      try

        //update the aLstSql
        aLstSql.clear;
        setlength(aSelectDataSQLs,0);
        for aCommitLoopIndex := 1 to fNBLoopBeforeCommit do begin
          aTmpLstSql := TStringList.Create;
          Try
            S1 := AlStringReplace(fSQL,#13#10,' ',[RfReplaceALL]);
            aTmpLstSql.Text := Trim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));
            for J := 0 to aTmpLstSql.Count - 1 do begin
              S1 := aTmpLstSql[j];
              aLst1 := TstringList.create;
              try
                S1 := ALFastTagReplace(S1,
                                       '<#',
                                       '>',
                                       SQLFastTagReplaceFunct,
                                       True,
                                       @aLst1)
              finally
                aLst1.free;
              end;
              aLstSql.Add(S1);
              setlength(aSelectDataSQLs,length(aSelectDataSQLs)+1);
              aSelectDataSQLs[length(aSelectDataSQLs)-1].Sql := S1;
              aSelectDataSQLs[length(aSelectDataSQLs)-1].RowTag := '';
              aSelectDataSQLs[length(aSelectDataSQLs)-1].viewTag := '';
              aSelectDataSQLs[length(aSelectDataSQLs)-1].skip := 0;
              aSelectDataSQLs[length(aSelectDataSQLs)-1].First := 0;
            end;
            inc(aLoopIndex);
            inc(FTotalUpdateOrSelect);
            if aLoopIndex > fMaxLoop then break;
          Finally
            aTmpLstSql.Free;
          End;
        end;

        //start the Transaction
        Tform1(fOwner).Sqlite3ConnectionPoolClient.TransactionStart(aconnectionHandle);
        try

          //update the data
          aStartDate := ALGetTickCount64;
          if fUpdateSQL then Tform1(fOwner).Sqlite3ConnectionPoolClient.UpdateData(aLstSql, aconnectionHandle)
          else Tform1(fOwner).Sqlite3ConnectionPoolClient.SelectData(aSelectDataSQLs,
                                                                     aXMLDATA.documentElement,
                                                                     aFormatSettings,
                                                                     aconnectionHandle);
          aEndDate := ALGetTickCount64;
          FTotalUpdateOrSelectTimeTaken := FTotalUpdateOrSelectTimeTaken + aEndDate - aStartDate;

          //commit the data
          aStartDate := ALGetTickCount64;
          Tform1(fOwner).Sqlite3ConnectionPoolClient.Transactioncommit(aconnectionHandle);
          aEndDate := ALGetTickCount64;
          FTotalCommitTimeTaken := FTotalCommitTimeTaken + aEndDate - aStartDate

        Except
          //roolBack the data
          Tform1(fOwner).Sqlite3ConnectionPoolClient.TransactionRollBack(aconnectionHandle);
          raise;
        end;

      Except
        on e: Exception do begin
          FErrorMsg := E.message;
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
    TForm1(fOwner).TableViewThreadUpdate.BeginUpdate;
    try
      if not fOn then begin
        dec(TForm1(fOwner).NBUpdateActiveThread);
        TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(TForm1(fOwner).NBSelectActiveThread + TForm1(fOwner).NBUpdateActiveThread);
        TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadUpdateNumber.Index,inttostr(frank) + ' (off)');
        if TForm1(fOwner).NBUpdateActiveThread = 0 then begin
          TForm1(fOwner).ALButtonSqlite3LoopUpdate.Tag := 0;
          TForm1(fOwner).ALButtonSqlite3LoopUpdate.Caption := 'Loop UPDATE via Sqlite3';
        end;
        if TForm1(fOwner).NBUpdateActiveThread + TForm1(fOwner).NBSelectActiveThread = 0 then begin
          TForm1(fOwner).Sqlite3ConnectionPoolClient.Free;
          TForm1(fOwner).Sqlite3ConnectionPoolClient := nil;
        end;
      end;
      TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateRequestCount.Index,FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateAverageUpdateTimeTaken.Index,FTotalUpdateOrSelectTimeTaken / FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateErrorMsg.Index,FErrorMsg);
      TForm1(fOwner).StatusBar1.Panels[1].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
    finally
      TForm1(fOwner).TableViewThreadUpdate.EndUpdate;
    end;
  end
  else begin
    TForm1(fOwner).TableViewThreadSelect.BeginUpdate;
    try
      if not fOn then begin
        dec(TForm1(fOwner).NBSelectActiveThread);
        TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(TForm1(fOwner).NBSelectActiveThread + TForm1(fOwner).NBUpdateActiveThread);
        TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadSelectNumber.Index,inttostr(frank) + ' (off)');
        if TForm1(fOwner).NBSelectActiveThread = 0 then begin
          TForm1(fOwner).ALButtonSqlite3LoopSelect.Tag := 0;
          TForm1(fOwner).ALButtonSqlite3LoopSelect.Caption := 'Loop SELECT via Sqlite3';
        end;
        if TForm1(fOwner).NBUpdateActiveThread + TForm1(fOwner).NBSelectActiveThread = 0 then begin
          TForm1(fOwner).Sqlite3ConnectionPoolClient.Free;
          TForm1(fOwner).Sqlite3ConnectionPoolClient := nil;
        end;
      end;
      TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectRequestCount.Index,FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectAverageSelectTimeTaken.Index,FTotalUpdateOrSelectTimeTaken / FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectErrorMsg.Index,FErrorMsg);
      TForm1(fOwner).StatusBar1.Panels[1].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
    finally
      TForm1(fOwner).TableViewThreadSelect.EndUpdate;
    end;
  end;
  application.ProcessMessages;
end;




////////////////////////////////////
///// TFirebirdBenchmarkThread /////
////////////////////////////////////

{*******************************************************************}
constructor TFirebirdBenchmarkThread.Create(CreateSuspended: Boolean;
                                            AOwner: TwinControl;
                                            aRank: integer;
                                            aSQL: String;
                                            aParams: String;
                                            aTPB: String;
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
  FTotalUpdateOrSelectTimeTaken := 0;
  FTotalCommitTimeTaken := 0;
  FTotalUpdateOrSelect := 0;
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

  {-------------------------------------------------}
  function internalDoTagReplace(Str: String): String;
  var aLst1: TstringList;
  Begin
    aLst1 := TstringList.create;
    try
      Str := ALFastTagReplace(Str,
                              '<#',
                              '>',
                              SQLFastTagReplaceFunct,
                              True,
                              @aLst1);
      Result := Str;
    finally
      aLst1.free;
    end;
  End;


Var aDBHandle: IscDbHandle;
    aTraHandle: IscTrHandle;
    aStartDate: int64;
    aEndDate: int64;
    aLoopIndex: integer;
    aCommitLoopIndex: integer;
    aXMLDATA: TalXmlDocument;
    aSelectDataSQLs: TalFBXClientSelectDataSQLs;
    aUpdateDataSQLs: TalFBXClientUpdateDataSQLs;
    aTmpSelectDataSQLs: TalFBXClientSelectDataSQLs;
    aTmpUpdateDataSQLs: TalFBXClientUpdateDataSQLs;
    aFormatSettings: TformatSettings;
    aStmtHandle: IscStmtHandle;
    aSqlda: TALFBXSQLResult;
    S1: String;
    j, k, l: integer;
    aLst1: TstringList;

begin

  //init the aFormatSettings
  GetLocaleFormatSettings(1033, aFormatSettings);

  //init the fNBLoopBeforeCommit
  if fNBLoopBeforeCommit <= 0 then fNBLoopBeforeCommit := 1;

  //create local object
  aXMLDATA := ALCreateEmptyXMLDocument('root');
  Try

    //start the loop;
    aLoopIndex := 1;
    while aLoopIndex <= fMaxLoop do begin
      try

        //update the aLstSql
        setlength(aSelectDataSQLs,0);
        setlength(aUpdateDataSQLs,0);
        if fParams = '' then begin
          aLst1 := TStringList.Create;
          Try
            S1 := AlStringReplace(fSQL,#13#10,' ',[RfReplaceALL]);
            aLst1.Text := Trim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));
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
          aLst1 := TStringList.Create;
          Try
            aLst1.Text := Trim(fParams);
            if fUpdateSQL then begin
              setlength(aUpdateDataSQLs,1);
              setlength(aUpdateDataSQLs[0].Params,1);
              setlength(aUpdateDataSQLs[0].Params[0].fields,aLst1.Count);
              for J := 0 to aLst1.Count - 1 do begin
                aUpdateDataSQLs[0].Params[0].fields[j].Value := aLst1[j];
                aUpdateDataSQLs[0].Params[0].fields[j].isnull := false;
                aUpdateDataSQLs[0].Params[0].fields[j].isblob := False;
              end;
              aUpdateDataSQLs[0].Sql := fSQL;
            end
            else begin
              setlength(aSelectDataSQLs,1);
              setlength(aSelectDataSQLs[0].Params,1);
              setlength(aSelectDataSQLs[0].Params[0].fields,aLst1.Count);
              for J := 0 to aLst1.Count - 1 do begin
                aSelectDataSQLs[0].Params[0].fields[j].Value := aLst1[j];
                aSelectDataSQLs[0].Params[0].fields[j].isnull := false;
                aSelectDataSQLs[0].Params[0].fields[j].isblob := False;
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
              aStartDate := ALGetTickCount64;
              Tform1(fOwner).FirebirdConnectionPoolClient.Prepare(aUpdateDataSQLs[0].SQL,
                                                                  aDBHandle,
                                                                  aTraHandle,
                                                                  aStmtHandle,
                                                                  aSqlda,
                                                                  fTPB);
              aEndDate := ALGetTickCount64;
              if FTotalPrepareTimeTaken = -1 then FTotalPrepareTimeTaken := 0;              
              FTotalPrepareTimeTaken := FTotalPrepareTimeTaken + aEndDate - aStartDate;
            end;
          end
          else begin
            if (length(aSelectDataSQLs) = 1) and (alPos('<#', aSelectDataSQLs[0].SQL) <= 0) then begin
              aStartDate := ALGetTickCount64;
              Tform1(fOwner).FirebirdConnectionPoolClient.Prepare(aSelectDataSQLs[0].SQL,
                                                                  aDBHandle,
                                                                  aTraHandle,
                                                                  aStmtHandle,
                                                                  aSqlda,
                                                                  fTPB);
              aEndDate := ALGetTickCount64;
              if FTotalPrepareTimeTaken = -1 then FTotalPrepareTimeTaken := 0;
              FTotalPrepareTimeTaken := FTotalPrepareTimeTaken + aEndDate - aStartDate;
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
                  setlength(aTmpSelectDataSQLs[j].Params[k].fields, length(aSelectDataSQLs[j].Params[k].fields));
                  for l := 0 to length(aTmpSelectDataSQLs[j].Params[k].fields) - 1 do begin
                    aTmpSelectDataSQLs[j].Params[k].fields[l].Value := internalDoTagReplace(aSelectDataSQLs[j].Params[k].fields[l].Value);
                    aTmpSelectDataSQLs[j].Params[k].fields[l].isnull := aSelectDataSQLs[j].Params[k].fields[l].isnull;
                    aTmpSelectDataSQLs[j].Params[k].fields[l].isblob := aSelectDataSQLs[j].Params[k].fields[l].isblob;
                  end;
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
                  setlength(aTmpUpdateDataSQLs[j].Params[k].fields, length(aUpdateDataSQLs[j].Params[k].fields));
                  for l := 0 to length(aTmpUpdateDataSQLs[j].Params[k].fields) - 1 do begin
                    aTmpUpdateDataSQLs[j].Params[k].fields[l].Value := internalDoTagReplace(aUpdateDataSQLs[j].Params[k].fields[l].Value);
                    aTmpUpdateDataSQLs[j].Params[k].fields[l].isnull := aUpdateDataSQLs[j].Params[k].fields[l].isnull;
                    aTmpUpdateDataSQLs[j].Params[k].fields[l].isblob := aUpdateDataSQLs[j].Params[k].fields[l].isblob;
                  end;
                end;
              end;

              if fUpdateSQL then begin
                aStartDate := ALGetTickCount64;
                if fUpdateSQL then Tform1(fOwner).FirebirdConnectionPoolClient.UpdateData(aTmpUpdateDataSQLs,
                                                                                          aDBHandle,
                                                                                          aTraHandle,
                                                                                          aStmtHandle,
                                                                                          aSqlda,
                                                                                          fTPB);
                aEndDate := ALGetTickCount64;
                FTotalUpdateOrSelectTimeTaken := FTotalUpdateOrSelectTimeTaken + aEndDate - aStartDate;
              end
              else begin
                aStartDate := ALGetTickCount64;
                Tform1(fOwner).FirebirdConnectionPoolClient.SelectData(aTmpSelectDataSQLs,
                                                                       aXMLDATA.documentElement,
                                                                       aFormatSettings,
                                                                       aDbHandle,
                                                                       aTraHandle,
                                                                       aStmtHandle,
                                                                       aSqlda,
                                                                       fTPB);
                aEndDate := ALGetTickCount64;
                FTotalUpdateOrSelectTimeTaken := FTotalUpdateOrSelectTimeTaken + aEndDate - aStartDate;
              end;

              inc(FTotalUpdateOrSelect);
              inc(aLoopIndex);
              if aLoopIndex > fMaxLoop then break;

            end;

          finally

            if assigned(aStmtHandle) then begin
              try
                Tform1(fOwner).FirebirdConnectionPoolClient.lib.DSQLFreeStatement(aStmtHandle, DSQL_drop);
              Except
                //what else we can do here ?
                //this can happen if connection lost for exemple
                //i preferre to hide this exception to not hide previous exception
              end;
              aSqlda.free;
              aStmtHandle := nil;
              aSqlda := nil;
            end;

          end;

          //commit the data
          aStartDate := ALGetTickCount64;
          Tform1(fOwner).FirebirdConnectionPoolClient.Transactioncommit(aDbHandle,aTraHandle);
          aEndDate := ALGetTickCount64;
          FTotalCommitTimeTaken := FTotalCommitTimeTaken + aEndDate - aStartDate

        except

          //roolBack the data
          Tform1(fOwner).FirebirdConnectionPoolClient.TransactionRollBack(aDbHandle, aTraHandle);
          raise;

        end;

      Except
        on e: Exception do begin
          FErrorMsg := E.message;
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
    TForm1(fOwner).TableViewThreadUpdate.BeginUpdate;
    try
      if not fOn then begin
        dec(TForm1(fOwner).NBUpdateActiveThread);
        TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(TForm1(fOwner).NBSelectActiveThread + TForm1(fOwner).NBUpdateActiveThread);
        TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadUpdateNumber.Index,inttostr(frank) + ' (off)');
        if TForm1(fOwner).NBUpdateActiveThread = 0 then begin
          TForm1(fOwner).ALButtonFirebirdLoopUpdate.Tag := 0;
          TForm1(fOwner).ALButtonFirebirdLoopUpdate.Caption := 'Loop UPDATE via Firebird';
        end;
        if TForm1(fOwner).NBUpdateActiveThread + TForm1(fOwner).NBSelectActiveThread = 0 then begin
          TForm1(fOwner).FirebirdConnectionPoolClient.Free;
          TForm1(fOwner).FirebirdConnectionPoolClient := nil;
        end;
      end;
      TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateRequestCount.Index,FTotalUpdateOrSelect);
      if FTotalPrepareTimeTaken >= 0 then TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateAveragePrepareTimeTaken.Index,FTotalPrepareTimeTaken / FTotalUpdateOrSelect)
      else  TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateAverageUpdateTimeTaken.Index,FTotalUpdateOrSelectTimeTaken / FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateErrorMsg.Index,FErrorMsg);
      TForm1(fOwner).StatusBar1.Panels[1].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
    finally
      TForm1(fOwner).TableViewThreadUpdate.EndUpdate;
    end;
  end
  else begin
    TForm1(fOwner).TableViewThreadSelect.BeginUpdate;
    try
      if not fOn then begin
        dec(TForm1(fOwner).NBSelectActiveThread);
        TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(TForm1(fOwner).NBSelectActiveThread + TForm1(fOwner).NBUpdateActiveThread);
        TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadSelectNumber.Index,inttostr(frank) + ' (off)');
        if TForm1(fOwner).NBSelectActiveThread = 0 then begin
          TForm1(fOwner).ALButtonFirebirdLoopSelect.Tag := 0;
          TForm1(fOwner).ALButtonFirebirdLoopSelect.Caption := 'Loop SELECT via Firebird';
        end;
        if TForm1(fOwner).NBUpdateActiveThread + TForm1(fOwner).NBSelectActiveThread = 0 then begin
          TForm1(fOwner).FirebirdConnectionPoolClient.Free;
          TForm1(fOwner).FirebirdConnectionPoolClient := nil;
        end;
      end;
      TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectRequestCount.Index,FTotalUpdateOrSelect);
      if FTotalPrepareTimeTaken >= 0 then TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectAveragePrepareTimeTaken.Index,FTotalPrepareTimeTaken / FTotalUpdateOrSelect)
      else  TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectAverageSelectTimeTaken.Index,FTotalUpdateOrSelectTimeTaken / FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectErrorMsg.Index,FErrorMsg);
      TForm1(fOwner).StatusBar1.Panels[1].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
    finally
      TForm1(fOwner).TableViewThreadSelect.EndUpdate;
    end;
  end;
  application.ProcessMessages;
end;


/////////////////////////////////
///// TMySqlBenchmarkThread /////
/////////////////////////////////

{****************************************************************}
constructor TMySqlBenchmarkThread.Create(CreateSuspended: Boolean;
                                         AOwner: TwinControl;
                                         aRank: integer;
                                         aSQL: String;
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
  FTotalUpdateOrSelectTimeTaken := 0;
  FTotalCommitTimeTaken := 0;
  FTotalUpdateOrSelect := 0;
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
    aStartDate: int64;
    aEndDate: int64;
    aLoopIndex: integer;
    aCommitLoopIndex: integer;
    aLstSql: Tstrings;
    aTmpLstSql: Tstrings;
    aXMLDATA: TalXmlDocument;
    aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
    aFormatSettings: TformatSettings;
    aLastUpdateGUI: Int64;
    S1: String;
    j: integer;
    aLst1: TstringList;
begin

  //init the aFormatSettings
  GetLocaleFormatSettings(1033, aFormatSettings);

  //init the fNBLoopBeforeCommit
  if fNBLoopBeforeCommit <= 0 then fNBLoopBeforeCommit := 1;

  //init the aLastUpdateGUI
  aLastUpdateGUI := 0;

  //create local object
  aLstSql := TstringList.create;
  aXMLDATA := ALCreateEmptyXMLDocument('root');
  Try

    //start the loop;
    aLoopIndex := 1;
    while aLoopIndex <= fMaxLoop do begin
      try

        //update the aLstSql
        aLstSql.clear;
        setlength(aSelectDataSQLs,0);
        for aCommitLoopIndex := 1 to fNBLoopBeforeCommit do begin
          aTmpLstSql := TStringList.Create;
          Try
            S1 := AlStringReplace(fSQL,#13#10,' ',[RfReplaceALL]);
            aTmpLstSql.Text := Trim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));
            for J := 0 to aTmpLstSql.Count - 1 do begin
              S1 := aTmpLstSql[j];
              aLst1 := TstringList.create;
              try
                S1 := ALFastTagReplace(S1,
                                       '<#',
                                       '>',
                                       SQLFastTagReplaceFunct,
                                       True,
                                       @aLst1)
              finally
                aLst1.free;
              end;
              aLstSql.Add(S1);
              setlength(aSelectDataSQLs,length(aSelectDataSQLs)+1);
              aSelectDataSQLs[length(aSelectDataSQLs)-1].Sql := S1;
              aSelectDataSQLs[length(aSelectDataSQLs)-1].RowTag := '';
              aSelectDataSQLs[length(aSelectDataSQLs)-1].viewTag := '';
              aSelectDataSQLs[length(aSelectDataSQLs)-1].skip := 0;
              aSelectDataSQLs[length(aSelectDataSQLs)-1].First := 0;
            end;
            inc(aLoopIndex);
            inc(FTotalUpdateOrSelect);
            if aLoopIndex > fMaxLoop then break;
          Finally
            aTmpLstSql.Free;
          End;
        end;

        //start the Transaction
        Tform1(fOwner).MySqlConnectionPoolClient.TransactionStart(aConnectionHandle);
        try

          //update the data
          aStartDate := ALGetTickCount64;
          if fUpdateSQL then Tform1(fOwner).MySqlConnectionPoolClient.UpdateData(aLstSql, aConnectionHandle)
          else Tform1(fOwner).MySqlConnectionPoolClient.SelectData(aSelectDataSQLs,
                                                                   aXMLDATA.documentElement,
                                                                   aFormatSettings,
                                                                   aConnectionHandle);
          aEndDate := ALGetTickCount64;
          FTotalUpdateOrSelectTimeTaken := FTotalUpdateOrSelectTimeTaken + aEndDate - aStartDate;

          //commit the data
          aStartDate := ALGetTickCount64;
          Tform1(fOwner).MySqlConnectionPoolClient.Transactioncommit(aConnectionHandle);
          aEndDate := ALGetTickCount64;
          FTotalCommitTimeTaken := FTotalCommitTimeTaken + aEndDate - aStartDate

        Except
          //roolBack the data
          Tform1(fOwner).MySqlConnectionPoolClient.TransactionRollBack(aConnectionHandle);
          raise;
        end;

      Except
        on e: Exception do begin
          FErrorMsg := E.message;
          Synchronize(UpdateGUI);
          Exit;
        end;
      end;
      if ALGetTickCount64 - aLastUpdateGUI > 5000 then begin
        Synchronize(UpdateGUI); // <= it's seam to be a source of bottleneck with mysql !!
        aLastUpdateGUI := ALGetTickCount64;
      end;
      If ((not fUpdateSQL) and (Tform1(fOwner).ALButtonMySqlLoopSelect.tag = 2)) or
         ((fUpdateSQL) and (Tform1(fOwner).ALButtonMySqlLoopUpdate.tag = 2)) then Break;
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
    TForm1(fOwner).TableViewThreadUpdate.BeginUpdate;
    try
      if not fOn then begin
        dec(TForm1(fOwner).NBUpdateActiveThread);
        TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(TForm1(fOwner).NBSelectActiveThread + TForm1(fOwner).NBUpdateActiveThread);
        TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadUpdateNumber.Index,inttostr(frank) + ' (off)');
        if TForm1(fOwner).NBUpdateActiveThread = 0 then begin
          TForm1(fOwner).ALButtonMySqlLoopUpdate.Tag := 0;
          TForm1(fOwner).ALButtonMySqlLoopUpdate.Caption := 'Loop UPDATE via MySql';
        end;
        if TForm1(fOwner).NBUpdateActiveThread + TForm1(fOwner).NBSelectActiveThread = 0 then begin
          TForm1(fOwner).MySqlConnectionPoolClient.Free;
          TForm1(fOwner).MySqlConnectionPoolClient := nil;
        end;
      end;
      TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateRequestCount.Index,FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateAverageUpdateTimeTaken.Index,FTotalUpdateOrSelectTimeTaken / FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadUpdate.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadUpdateErrorMsg.Index,FErrorMsg);
      TForm1(fOwner).StatusBar1.Panels[1].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
    finally
      TForm1(fOwner).TableViewThreadUpdate.EndUpdate;
    end;
  end
  else begin
    TForm1(fOwner).TableViewThreadSelect.BeginUpdate;
    try
      if not fOn then begin
        dec(TForm1(fOwner).NBSelectActiveThread);
        TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(TForm1(fOwner).NBSelectActiveThread + TForm1(fOwner).NBUpdateActiveThread);
        TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadSelectNumber.Index,inttostr(frank) + ' (off)');
        if TForm1(fOwner).NBSelectActiveThread = 0 then begin
          TForm1(fOwner).ALButtonMySqlLoopSelect.Tag := 0;
          TForm1(fOwner).ALButtonMySqlLoopSelect.Caption := 'Loop SELECT via MySql';
        end;
        if TForm1(fOwner).NBUpdateActiveThread + TForm1(fOwner).NBSelectActiveThread = 0 then begin
          TForm1(fOwner).MySqlConnectionPoolClient.Free;
          TForm1(fOwner).MySqlConnectionPoolClient := nil;
        end;
      end;
      TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectRequestCount.Index,FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectAveragePrepareTimeTaken.Index,0/0);
      TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectAverageSelectTimeTaken.Index,FTotalUpdateOrSelectTimeTaken / FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FTotalUpdateOrSelect);
      TForm1(fOwner).TableViewThreadSelect.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadSelectErrorMsg.Index,FErrorMsg);
      TForm1(fOwner).StatusBar1.Panels[1].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
    finally
      TForm1(fOwner).TableViewThreadSelect.EndUpdate;
    end;
  end;
  application.ProcessMessages;
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
  ie.Width := 100;
  ie.Height := 300;
  ie.MenuBar := false;
  ie.AddressBar := false;
  ie.Resizable := false;
  ie.StatusBar := false;
  ie.ToolBar := 0;
  Url := 'http://www.arkadia.com/html/alcinoe_like.html';
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

end.
