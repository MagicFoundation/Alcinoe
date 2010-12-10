unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, AlScrollBar, ALMemo, ALButton,
  ALComboBox, ALEdit, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData,
  cxDataStorage, cxEdit, cxGridLevel, cxGridCustomTableView, cxGridTableView,
  cxClasses, cxControls, cxGridCustomView, cxGrid, ComCtrls, AlSqlite3Client,
  cxMemo, cxBlobEdit, alFbxClient, almysqlClient;

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
    ALButtonFirebirdSelect: TALButton;
    Label3: TLabel;
    ALEditMySqlHost: TALEdit;
    Label6: TLabel;
    ALEditMySqlLogin: TALEdit;
    Label7: TLabel;
    ALEditMySqlPassword: TALEdit;
    ALEditMySqlPort: TALEdit;
    Label8: TLabel;
    ALEditMySqlCharset: TALEdit;
    Label10: TLabel;
    ALEditMysqlLib: TALEdit;
    Label11: TLabel;
    ALMemoMySqlQuery: TALMemo;
    Label9: TLabel;
    ALButtonMySQLSelect: TALButton;
    OpenDialog1: TOpenDialog;
    Bevel2: TBevel;
    Label12: TLabel;
    ALEditMySqlDatabaseName: TALEdit;
    Label2: TLabel;
    ALEditFirebirdLogin: TALEdit;
    Label4: TLabel;
    ALEditFirebirdPassword: TALEdit;
    ALEditFirebirdCharset: TALEdit;
    Label15: TLabel;
    ALEditFirebirdLib: TALEdit;
    Label16: TLabel;
    ALMemoFireBirdQuery: TALMemo;
    Label17: TLabel;
    Label18: TLabel;
    ALEditFirebirdDatabase: TALEdit;
    Label1: TLabel;
    Label14: TLabel;
    ALButtonFirebirdLoopSelect: TALButton;
    ALButtonFirebirdUpdate: TALButton;
    ALButtonFirebirdLoopUpdate: TALButton;
    Label24: TLabel;
    Label25: TLabel;
    Bevel1: TBevel;
    Label27: TLabel;
    ALEditSqlite3Lib: TALEdit;
    ALMemoSqlite3Query: TALMemo;
    ALButtonSqlLite3Select: TALButton;
    ALEditSqlite3Database: TALEdit;
    Label19: TLabel;
    ALButtonSqlite3LoopSelect: TALButton;
    ALButtonSqlite3Update: TALButton;
    ALButtonSqlite3LoopUpdate: TALButton;
    ALEditSQLite3NBLoop: TALEdit;
    Label20: TLabel;
    RadioGroupSqlite3Journal_Mode: TRadioGroup;
    RadioGroupSQLite3Temp_Store: TRadioGroup;
    RadioGroupSqlite3Synhcronous: TRadioGroup;
    ALEditSqlite3Cache_Size: TALEdit;
    Label21: TLabel;
    Label22: TLabel;
    ALEditSqlite3Page_Size: TALEdit;
    Label23: TLabel;
    ALEditSQLite3NbLoopBeforeCommit: TALEdit;
    Label28: TLabel;
    ALEditSqlite3NBThread: TALEdit;
    StatusBar1: TStatusBar;
    ALCheckBoxSqlite3SharedCache: TALCheckBox;
    Panel2: TPanel;
    ALMemoResult: TALMemo;
    GridThread: TcxGrid;
    TableViewThread: TcxGridTableView;
    TableViewThreadNumber: TcxGridColumn;
    TableViewThreadRequestCount: TcxGridColumn;
    TableViewThreadAverageSelectUpdateTimeTaken: TcxGridColumn;
    TableViewThreadAverageCommitTimeTaken: TcxGridColumn;
    TableViewThreadErrorMsg: TcxGridColumn;
    levelThread: TcxGridLevel;
    Panel1: TPanel;
    Label5: TLabel;
    Splitter1: TSplitter;
    Label13: TLabel;
    Label26: TLabel;
    ALEditFirebirdNBLoop: TALEdit;
    ALEditFirebirdNbLoopBeforeCommit: TALEdit;
    Label29: TLabel;
    ALEditFirebirdNBThread: TALEdit;
    Label30: TLabel;
    ALEditFireBirdNum_buffers: TALEdit;
    ALButtonFirebirdCreateDatabase: TALButton;
    Label31: TLabel;
    ALComboBoxFirebirdapiVer: TALComboBox;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    ALEditMySqlNBLoop: TALEdit;
    ALEditMySqlNbLoopBeforeCommit: TALEdit;
    ALEditMySqlNBThread: TALEdit;
    ALButtonMysqlUpdate: TALButton;
    ALButtonMySqlLoopUpdate: TALButton;
    ALButtonMysqlLoopSelect: TALButton;
    ALCheckBoxSqlite3ReadUncommited: TALCheckBox;
    procedure ALButtonPaint(Sender: TObject; var continue: Boolean);
    procedure FormClick(Sender: TObject);
    procedure ALButtonMySqlClick(Sender: TObject);
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
  private
  public
    Sqlite3ConnectionPoolClient: TalSqlite3ConnectionPoolClient;
    FirebirdConnectionPoolClient: TalFBXConnectionPoolClient;
    MySqlConnectionPoolClient: TalMySqlConnectionPoolClient;
    NBActiveThread: integer;
    StartTime: int64;
  end;

  {---------------------------------------}
  TFirebirdBenchmarkThread = Class(Tthread)
  private
    fSQL: String;
    fUpdateSQL: Boolean;
    fOn: Boolean;
    fMaxLoop: integer;
    fNBLoopBeforeCommit: integer;
    FErrorMsg: String;
    FTotalUpdateTimeTaken: int64;
    FTotalCommitTimeTaken: int64;
    FtotalUpdate: integer;
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

  {--------------------------------------}
  TSqlite3BenchmarkThread = Class(Tthread)
  private
    fSQL: String;
    fUpdateSQL: Boolean;
    fOn: Boolean;
    fMaxLoop: integer;
    fNBLoopBeforeCommit: integer;
    FErrorMsg: String;
    FTotalUpdateTimeTaken: int64;
    FTotalCommitTimeTaken: int64;
    FtotalUpdate: integer;
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
    FTotalUpdateTimeTaken: int64;
    FTotalCommitTimeTaken: int64;
    FtotalUpdate: integer;
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

{************************************************************}
procedure TForm1.ALButtonFirebirdSelectClick(Sender: TObject);
Var aFBXClient: TALFbxClient;
    aXMLDATA: TalXmlDocument;
    aXMLDATA2: TalXmlDocument;
    aStartDate: int64;
    aEndDate: int64;
    aStartCommitDate: int64;
    aEndCommitDate: int64;
    aFormatSettings: TformatSettings;
    S1: String;
    i: integer;
    aFBAPiVersion: TALFBXVersion_API;
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

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
      aXMLDATA2 := ALCreateEmptyXMLDocument('root');
      Try

        With aXMLDATA Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
        end;

        S1 := AlMemoFireBirdQuery.Lines.Text;
        while AlPos('<#randomchar>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomchar>',AlRandomStr(1),[rfIgnoreCase]);
        while AlPos('<#randomnumber>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomnumber>',inttostr(random(10)),[rfIgnoreCase]);
        for i := 1 to maxint do begin
          if AlPos('<#randomnumber'+inttostr(i)+'>', AlLowerCase(S1)) > 0 then S1 := AlStringReplace(S1, '<#randomnumber'+inttostr(i)+'>',inttostr(random(10)),[rfIgnoreCase, rfReplaceAll])
          else break;
        end;

        aStartDate := ALGetTickCount64;
        aFBXClient.TransactionStart(True);
        try
          aFBXClient.SelectData(S1,
                                'rec',
                                 0,
                                 200,
                                aXMLDATA.DocumentElement,
                                aFormatSettings);
          aEndDate := ALGetTickCount64;

          aFBXClient.SelectData('SELECT '+
                                  'MON$RECORD_IDX_READS as IDX_READS, '+
                                  'MON$RECORD_SEQ_READS as SEQ_READS '+
                                'FROM '+
                                  'MON$RECORD_STATS '+
                                'JOIN MON$TRANSACTIONS ON MON$TRANSACTIONS.MON$STAT_ID=MON$RECORD_STATS.MON$STAT_ID '+
                                'WHERE '+
                                  'MON$TRANSACTIONS.MON$TRANSACTION_ID=current_transaction',
                                aXMLDATA2.DocumentElement,
                                aFormatSettings);
          AlMemoResult.Lines.Clear;
          AlMemoResult.Lines.add('Indexed Read: ' + aXMLDATA2.DocumentElement.ChildNodes['idx_reads'].Text);
          AlMemoResult.Lines.add('Non Indexed Read: ' + aXMLDATA2.DocumentElement.ChildNodes['seq_reads'].Text);
          AlMemoResult.Lines.add('');
          AlMemoResult.Lines.add('**************');
          AlMemoResult.Lines.add('');

          aStartCommitDate := ALGetTickCount64;
          aFBXClient.TransactionCommit;
          aendCommitDate := ALGetTickCount64;
        Except
          aFBXClient.TransactionRollBack;
          Raise;
        end;

        TableViewThread.DataController.RecordCount := 1;
        TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
        TableViewThread.DataController.SetValue(0,TableViewThreadRequestCount.Index,1);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageSelectUpdateTimeTaken.Index,aEndDate - aStartDate);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,aendCommitDate - aStartCommitDate);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[2].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
        AlMemoResult.Lines.Text := AlMemoResult.Lines.Text + aXMLDATA.XML.Text;

      Finally
        aXMLDATA.free;
        aXMLDATA2.free;
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
Var aFBXClient: TalFBXClient;
    aXMLDATA2: TalXmlDocument;
    aFormatSettings: TformatSettings;
    aStartDate: int64;
    aEndDate: int64;
    aStartCommitDate: int64;
    aEndCommitDate: int64;
    LstSql: TstringList;
    S1: String;
    i: integer;
    aFBAPiVersion: TALFBXVersion_API;
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;


  GetLocaleFormatSettings(1033, aFormatSettings);
  Screen.Cursor := CrHourGlass;
  try

    aFBXClient := TALFbxClient.Create(aFBAPiVersion,ALEditFirebirdLib.Text);
    LstSql := TstringList.Create;
    Try
      aFBXClient.connect(ALEditFireBirdDatabase.Text,
                         ALEditFireBirdLogin.text,
                         ALEditFireBirdPassword.text,
                         ALEditFireBirdCharset.Text,
                         StrtoInt(ALEditFireBirdNum_buffers.Text));


      aXMLDATA2 := ALCreateEmptyXMLDocument('root');
      Try

        S1 := AlMemoFireBirdQuery.Lines.Text;
        while AlPos('<#randomchar>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomchar>',AlRandomStr(1),[rfIgnoreCase]);
        while AlPos('<#randomnumber>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomnumber>',inttostr(random(10)),[rfIgnoreCase]);
        for i := 1 to maxint do begin
          if AlPos('<#randomnumber'+inttostr(i)+'>', AlLowerCase(S1)) > 0 then S1 := AlStringReplace(S1, '<#randomnumber'+inttostr(i)+'>',inttostr(random(10)),[rfIgnoreCase, rfReplaceAll])
          else break;
        end;
        if (AlPos('begin',AllowerCase(S1)) < 0) or
           (AlPos('end',AllowerCase(S1)) < 0) then begin
          S1 := AlStringReplace(S1,#13#10,' ',[RfReplaceALL]);
          LstSql.Text := Trim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));
        end
        else LstSql.Add(S1);

        aStartDate := ALGetTickCount64;
        aFBXClient.TransactionStart(False);
        try
          aFBXClient.UpdateData(LstSql);
          aEndDate := ALGetTickCount64;

          aFBXClient.SelectData('SELECT '+
                                  'MON$RECORD_IDX_READS as IDX_READS, '+
                                  'MON$RECORD_SEQ_READS as SEQ_READS, '+
                                  'MON$RECORD_INSERTS as INSERTS, '+
                                  'MON$RECORD_UPDATES as UPDATES, '+
                                  'MON$RECORD_DELETES as DELETES '+
                                'FROM '+
                                  'MON$RECORD_STATS '+
                                'JOIN MON$TRANSACTIONS ON MON$TRANSACTIONS.MON$STAT_ID=MON$RECORD_STATS.MON$STAT_ID '+
                                'WHERE '+
                                  'MON$TRANSACTIONS.MON$TRANSACTION_ID=current_transaction',
                                aXMLDATA2.DocumentElement,
                                aFormatSettings);
          AlMemoResult.Lines.Clear;
          AlMemoResult.Lines.add('Indexed Read: ' + aXMLDATA2.DocumentElement.ChildNodes['idx_reads'].Text);
          AlMemoResult.Lines.add('Non Indexed Read: ' + aXMLDATA2.DocumentElement.ChildNodes['seq_reads'].Text);
          AlMemoResult.Lines.add('Inserts: ' + aXMLDATA2.DocumentElement.ChildNodes['inserts'].Text);
          AlMemoResult.Lines.add('Updates: ' + aXMLDATA2.DocumentElement.ChildNodes['updates'].Text);
          AlMemoResult.Lines.add('Deletes: ' + aXMLDATA2.DocumentElement.ChildNodes['deletes'].Text);

          aStartCommitDate := ALGetTickCount64;
          aFBXClient.TransactionCommit;
          aendCommitDate := ALGetTickCount64;
        Except
          aFBXClient.TransactionRollBack;
          raise;
        end;


        TableViewThread.DataController.RecordCount := 1;
        TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
        TableViewThread.DataController.SetValue(0,TableViewThreadRequestCount.Index,1);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageSelectUpdateTimeTaken.Index,aEndDate - aStartDate);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,aendCommitDate - aStartCommitDate);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[2].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';

      Finally
        aXMLDATA2.Free;
      End;

    Finally
      aFBXClient.disconnect;
      aFBXClient.free;
      LstSql.free;
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
end;

{****************************************************************}
procedure TForm1.ALButtonFirebirdLoopSelectClick(Sender: TObject);
Var aFirebirdBenchmarkThread: TFirebirdBenchmarkThread;
    i: integer;
    aFBAPiVersion: TALFBXVersion_API;
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

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
  TableViewThread.DataController.RecordCount := strtoint(ALEditFirebirdNBThread.Text);
  StartTime := ALGetTickCount64;

  //create the fFirebirdConnectionPoolClient
  FirebirdConnectionPoolClient := TALFBXConnectionPoolClient.Create(ALEditFireBirdDatabase.Text,
                                                                    ALEditFireBirdLogin.text,
                                                                    ALEditFireBirdPassword.text,
                                                                    ALEditFireBirdCharset.Text,
                                                                    aFBAPiVersion,
                                                                    ALEditFirebirdLib.Text,
                                                                    StrtoInt(ALEditFireBirdNum_buffers.Text));

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  for i := 1 to strtoint(ALEditFirebirdNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,inttostr(i) + ' (on)');
    aFirebirdBenchmarkThread := TFirebirdBenchmarkThread.Create(True,
                                                                self,
                                                                i,
                                                                trim(ALMemoFirebirdQuery.Lines.Text),
                                                                strtoint(ALEditFirebirdNBLoop.Text),
                                                                strtoint(ALEditFirebirdNbLoopBeforeCommit.Text),
                                                                false);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBActiveThread);
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
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

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
  TableViewThread.DataController.RecordCount := strtoint(ALEditFirebirdNBThread.Text);
  StartTime := ALGetTickCount64;

  //create the fFirebirdConnectionPoolClient
  FirebirdConnectionPoolClient := TALFBXConnectionPoolClient.Create(ALEditFireBirdDatabase.Text,
                                                                    ALEditFireBirdLogin.text,
                                                                    ALEditFireBirdPassword.text,
                                                                    ALEditFireBirdCharset.Text,
                                                                    aFBAPiVersion,
                                                                    ALEditFirebirdLib.Text,
                                                                    StrtoInt(ALEditFireBirdNum_buffers.Text));

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  for i := 1 to strtoint(ALEditFirebirdNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,inttostr(i) + ' (on)');
    aFirebirdBenchmarkThread := TFirebirdBenchmarkThread.Create(True,
                                                                self,
                                                                i,
                                                                trim(ALMemoFirebirdQuery.Lines.Text),
                                                                strtoint(ALEditFirebirdNBLoop.Text),
                                                                strtoint(ALEditFirebirdNbLoopBeforeCommit.Text),
                                                                true);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBActiveThread);
    StatusBar1.Repaint;
    aFirebirdBenchmarkThread.FreeOnTerminate := True;
    aFirebirdBenchmarkThread.Resume;
  end;

end;

{***************************************************}
procedure TForm1.ALButtonMySqlClick(Sender: TObject);
Var aMySqlClient: TalMySqlClient;
    aXMLDATA: TalXmlDocument;
    aStartDate: int64;
    aEndDate: int64;
    aFormatSettings: TformatSettings;
    S1: String;
    i: integer;
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

        S1 := AlMemoMySqlQuery.Lines.Text;
        while AlPos('<#randomchar>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomchar>',AlRandomStr(1),[rfIgnoreCase]);
        while AlPos('<#randomnumber>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomnumber>',inttostr(random(10)),[rfIgnoreCase]);
        for i := 1 to maxint do begin
          if AlPos('<#randomnumber'+inttostr(i)+'>', AlLowerCase(S1)) > 0 then S1 := AlStringReplace(S1, '<#randomnumber'+inttostr(i)+'>',inttostr(random(10)),[rfIgnoreCase, rfReplaceAll])
          else break;
        end;

        aStartDate := ALGetTickCount64;
        aMySqlClient.SelectData(S1,
                                'rec',
                                 0,
                                 200,
                                aXMLDATA.DocumentElement,
                                aFormatSettings);
        aEndDate := ALGetTickCount64;

        TableViewThread.DataController.RecordCount := 1;
        TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
        TableViewThread.DataController.SetValue(0,TableViewThreadRequestCount.Index,1);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageSelectUpdateTimeTaken.Index,aEndDate - aStartDate);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[2].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
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
    i: integer;
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

      S1 := AlMemoMySqlQuery.Lines.Text;
      while AlPos('<#randomchar>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomchar>',AlRandomStr(1),[rfIgnoreCase]);
      while AlPos('<#randomnumber>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomnumber>',inttostr(random(10)),[rfIgnoreCase]);
      for i := 1 to maxint do begin
        if AlPos('<#randomnumber'+inttostr(i)+'>', AlLowerCase(S1)) > 0 then S1 := AlStringReplace(S1, '<#randomnumber'+inttostr(i)+'>',inttostr(random(10)),[rfIgnoreCase, rfReplaceAll])
        else break;
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

      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadRequestCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageSelectUpdateTimeTaken.Index,aEndDate - aStartDate);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,aendCommitDate - aStartCommitDate);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[2].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
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
  TableViewThread.DataController.RecordCount := strtoint(ALEditMySqlNBThread.Text);
  StartTime := ALGetTickCount64;

  //create the fMySqlConnectionPoolClient
  MySqlConnectionPoolClient := TALMySqlConnectionPoolClient.Create(ALEditMySqlHost.Text,
                                                                   StrToInt(ALEditMySqlPort.Text),
                                                                   ALEditMySqlDatabaseName.Text,
                                                                   ALEditMySqlLogin.Text,
                                                                   ALEditMySqlPassword.Text,
                                                                   ALEditMySqlCharset.Text,
                                                                   MYSQL50,
                                                                   ALEditMySqlLib.Text,
                                                                   0);

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  for i := 1 to strtoint(ALEditMySqlNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,inttostr(i) + ' (on)');
    aMySqlBenchmarkThread := TMySqlBenchmarkThread.Create(True,
                                                          self,
                                                          i,
                                                          trim(ALMemoMySqlQuery.Lines.Text),
                                                          strtoint(ALEditMySqlNBLoop.Text),
                                                          strtoint(ALEditMySqlNbLoopBeforeCommit.Text),
                                                          false);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBActiveThread);
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
  TableViewThread.DataController.RecordCount := strtoint(ALEditMySqlNBThread.Text);
  StartTime := ALGetTickCount64;

  //create the fMySqlConnectionPoolClient
  MySqlConnectionPoolClient := TALMySqlConnectionPoolClient.Create(ALEditMySqlHost.Text,
                                                                   StrToInt(ALEditMySqlPort.Text),
                                                                   ALEditMySqlDatabaseName.Text,
                                                                   ALEditMySqlLogin.Text,
                                                                   ALEditMySqlPassword.Text,
                                                                   ALEditMySqlCharset.Text,
                                                                   MYSQL50,
                                                                   ALEditMySqlLib.Text,
                                                                   0);
  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  for i := 1 to strtoint(ALEditMySqlNBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,inttostr(i) + ' (on)');
    aMySqlBenchmarkThread := TMySqlBenchmarkThread.Create(True,
                                                          self,
                                                          i,
                                                          trim(ALMemoMySqlQuery.Lines.Text),
                                                          strtoint(ALEditMySqlNBLoop.Text),
                                                          strtoint(ALEditMySqlNbLoopBeforeCommit.Text),
                                                          true);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBActiveThread);
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
    i: integer;
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

        S1 := AlMemoSQLite3Query.Lines.Text;
        while AlPos('<#randomchar>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomchar>',AlRandomStr(1),[rfIgnoreCase]);
        while AlPos('<#randomnumber>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomnumber>',inttostr(random(10)),[rfIgnoreCase]);
        for i := 1 to maxint do begin
          if AlPos('<#randomnumber'+inttostr(i)+'>', AlLowerCase(S1)) > 0 then S1 := AlStringReplace(S1, '<#randomnumber'+inttostr(i)+'>',inttostr(random(10)),[rfIgnoreCase, rfReplaceAll])
          else break;
        end;

        aStartDate := ALGetTickCount64;
        aSqlite3Client.SelectData(S1,
                                  'rec',
                                   0,
                                   200,
                                  aXMLDATA.DocumentElement,
                                  aFormatSettings);
        aEndDate := ALGetTickCount64;

        TableViewThread.DataController.RecordCount := 1;
        TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
        TableViewThread.DataController.SetValue(0,TableViewThreadRequestCount.Index,1);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageSelectUpdateTimeTaken.Index,aEndDate - aStartDate);
        TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,0);
        TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
        StatusBar1.Panels[2].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
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
    i: integer;
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
      S1 := AlMemoSQLite3Query.Lines.Text;
      while AlPos('<#randomchar>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomchar>',AlRandomStr(1),[rfIgnoreCase]);
      while AlPos('<#randomnumber>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomnumber>',inttostr(random(10)),[rfIgnoreCase]);
      for i := 1 to maxint do begin
        if AlPos('<#randomnumber'+inttostr(i)+'>', AlLowerCase(S1)) > 0 then S1 := AlStringReplace(S1, '<#randomnumber'+inttostr(i)+'>',inttostr(random(10)),[rfIgnoreCase, rfReplaceAll])
        else break;
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

      //init the visual component
      TableViewThread.DataController.RecordCount := 1;
      TableViewThread.DataController.SetValue(0,TableViewThreadNumber.Index, '1 (off)');
      TableViewThread.DataController.SetValue(0,TableViewThreadRequestCount.Index,1);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageSelectUpdateTimeTaken.Index,aEndDate - aStartDate);
      TableViewThread.DataController.SetValue(0,TableViewThreadAverageCommitTimeTaken.Index,aendCommitDate - aStartCommitDate);
      TableViewThread.DataController.SetValue(0,TableViewThreadErrorMsg.Index,'');
      StatusBar1.Panels[2].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb';
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
  TableViewThread.DataController.RecordCount := strtoint(ALEditSqlite3NBThread.Text);
  StartTime := ALGetTickCount64;

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
  Sqlite3ConnectionPoolClient := TALSqlite3ConnectionPoolClient.Create(ALEditSqlite3Database.text,
                                                                       SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE,
                                                                       aPragmaStatements,
                                                                       ALEditSqlite3lib.Text);

  //enable or disable the shared cache
  Sqlite3ConnectionPoolClient.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  for i := 1 to strtoint(ALEditSqlite3NBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,inttostr(i) + ' (on)');
    aSqlite3BenchmarkThread := TSqlite3BenchmarkThread.Create(True,
                                                              self,
                                                              i,
                                                              trim(ALMemoSqlite3Query.Lines.Text),
                                                              strtoint(ALEditSqlite3NBLoop.Text),
                                                              strtoint(ALEditSQLite3NbLoopBeforeCommit.Text),
                                                              false);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBActiveThread);
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
  TableViewThread.DataController.RecordCount := strtoint(ALEditSqlite3NBThread.Text);
  StartTime := ALGetTickCount64;

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
  Sqlite3ConnectionPoolClient := TALSqlite3ConnectionPoolClient.Create(ALEditSqlite3Database.text,
                                                                       SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE,
                                                                       aPragmaStatements,
                                                                       ALEditSqlite3lib.Text);

  //enable or disable the shared cache
  Sqlite3ConnectionPoolClient.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);

  //clear the status bar
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';

  //launch all the thread
  for i := 1 to strtoint(ALEditSqlite3NBThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,inttostr(i) + ' (on)');
    aSqlite3BenchmarkThread := TSqlite3BenchmarkThread.Create(True,
                                                              self,
                                                              i,
                                                              trim(ALMemoSqlite3Query.Lines.Text),
                                                              strtoint(ALEditSqlite3NBLoop.Text),
                                                              strtoint(ALEditSQLite3NbLoopBeforeCommit.Text),
                                                              true);
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBActiveThread);
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
  FTotalUpdateTimeTaken := 0;
  FTotalCommitTimeTaken := 0;
  FtotalUpdate := 0;
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
    aXMLDATA: TalXmlDocument;
    aSelectDataSQLs: TalSqlite3ClientSelectDataSQLs;
    aFormatSettings: TformatSettings;
    S1: String;
    i: integer;
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
          S1 := fSQL;
          while AlPos('<#randomchar>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomchar>',AlRandomStr(1),[rfIgnoreCase]);
          while AlPos('<#randomnumber>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomnumber>',inttostr(random(10)),[rfIgnoreCase]);
          for i := 1 to maxint do begin
            if AlPos('<#randomnumber'+inttostr(i)+'>', AlLowerCase(S1)) > 0 then S1 := AlStringReplace(S1, '<#randomnumber'+inttostr(i)+'>',inttostr(random(10)),[rfIgnoreCase, rfReplaceAll])
            else break;
          end;
          aLstSql.Add(S1);
          setlength(aSelectDataSQLs,length(aSelectDataSQLs)+1);
          aSelectDataSQLs[length(aSelectDataSQLs)-1].Sql := S1;
          aSelectDataSQLs[length(aSelectDataSQLs)-1].RowTag := '';
          aSelectDataSQLs[length(aSelectDataSQLs)-1].viewTag := '';
          aSelectDataSQLs[length(aSelectDataSQLs)-1].skip := 0;
          aSelectDataSQLs[length(aSelectDataSQLs)-1].First := 0;
          inc(aLoopIndex);
          inc(FTotalUpdate);
          if aLoopIndex > fMaxLoop then break;
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
          FTotalUpdateTimeTaken := FTotalUpdateTimeTaken + aEndDate - aStartDate;

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
      If (Tform1(fOwner).ALButtonSqlite3LoopSelect.tag = 2) or (Tform1(fOwner).ALButtonSqlite3LoopUpdate.tag = 2) then Break;
    end;

  Finally
    aLstSql.free;
    aXMLDATA.free;
  End;
end;

{******************************************}
procedure TSqlite3BenchmarkThread.UpdateGUI;
Var timeElapsed: Int64;
    RequestCount: Integer;
    i: integer;
begin
  TForm1(fOwner).TableViewThread.BeginUpdate;
  try
    if not fOn then begin
      dec(TForm1(fOwner).NBActiveThread);
      TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(TForm1(fOwner).NBActiveThread);
      TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,inttostr(frank) + ' (off)');
      if TForm1(fOwner).NBActiveThread = 0 then begin
        timeElapsed := ALGetTickCount64 - TForm1(fOwner).StartTime;
        RequestCount := 0;
        for I := 0 to TForm1(fOwner).TableViewThread.DataController.RecordCount - 1 do
          RequestCount := RequestCount + TForm1(fOwner).TableViewThread.DataController.GetValue(I,Tform1(fOwner).TableViewThreadRequestCount.Index);
        TForm1(fOwner).StatusBar1.Panels[1].Text := inttostr(RequestCount) + ' Requests in ' + inttostr(round(timeElapsed / 1000)) + ' seconds (' + FormatFloat('0.##',RequestCount / (timeElapsed / 1000)) + ' Request/second';
        if fUpdateSQL then begin
          TForm1(fOwner).ALButtonSqlite3LoopUpdate.Tag := 0;
          TForm1(fOwner).ALButtonSqlite3LoopUpdate.Caption := 'Loop UPDATE via Sqlite3';
        end
        else begin
          TForm1(fOwner).ALButtonSqlite3LoopSelect.Tag := 0;
          TForm1(fOwner).ALButtonSqlite3LoopSelect.Caption := 'Loop SELECT via Sqlite3';
        end;
        TForm1(fOwner).Sqlite3ConnectionPoolClient.Free;
        TForm1(fOwner).Sqlite3ConnectionPoolClient := nil;
      end;
    end;
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadRequestCount.Index,fTotalUpdate);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageSelectUpdateTimeTaken.Index,FTotalUpdateTimeTaken / FtotalUpdate);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FtotalUpdate);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadErrorMsg.Index,FErrorMsg);
    TForm1(fOwner).StatusBar1.Panels[2].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
  finally
    TForm1(fOwner).TableViewThread.EndUpdate;
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
  FTotalUpdateTimeTaken := 0;
  FTotalCommitTimeTaken := 0;
  FtotalUpdate := 0;
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
Var aDBHandle: IscDbHandle;
    aTraHandle: IscTrHandle;
    aStartDate: int64;
    aEndDate: int64;
    aLoopIndex: integer;
    aCommitLoopIndex: integer;
    aLstSql: Tstrings;
    aXMLDATA: TalXmlDocument;
    aSelectDataSQLs: TalFBXClientSelectDataSQLs;
    aFormatSettings: TformatSettings;
    S1: String;
    i: integer;
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
          S1 := fSQL;
          while AlPos('<#randomchar>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomchar>',AlRandomStr(1),[rfIgnoreCase]);
          while AlPos('<#randomnumber>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomnumber>',inttostr(random(10)),[rfIgnoreCase]);
          for i := 1 to maxint do begin
            if AlPos('<#randomnumber'+inttostr(i)+'>', AlLowerCase(S1)) > 0 then S1 := AlStringReplace(S1, '<#randomnumber'+inttostr(i)+'>',inttostr(random(10)),[rfIgnoreCase, rfReplaceAll])
            else break;
          end;
          aLstSql.Add(S1);
          setlength(aSelectDataSQLs,length(aSelectDataSQLs)+1);
          aSelectDataSQLs[length(aSelectDataSQLs)-1].Sql := S1;
          aSelectDataSQLs[length(aSelectDataSQLs)-1].RowTag := '';
          aSelectDataSQLs[length(aSelectDataSQLs)-1].viewTag := '';
          aSelectDataSQLs[length(aSelectDataSQLs)-1].skip := 0;
          aSelectDataSQLs[length(aSelectDataSQLs)-1].First := 0;
          inc(aLoopIndex);
          inc(FTotalUpdate);
          if aLoopIndex > fMaxLoop then break;
        end;

        //start the Transaction
        Tform1(fOwner).FirebirdConnectionPoolClient.TransactionStart(aDbHandle, aTRAHandle);
        try

          //update the data
          aStartDate := ALGetTickCount64;
          if fUpdateSQL then Tform1(fOwner).FirebirdConnectionPoolClient.UpdateData(aLstSql, aDBHandle, aTraHandle)
          else Tform1(fOwner).FirebirdConnectionPoolClient.SelectData(aSelectDataSQLs,
                                                                      aXMLDATA.documentElement,
                                                                      aFormatSettings,
                                                                      aDbHandle,
                                                                      aTraHandle);
          aEndDate := ALGetTickCount64;
          FTotalUpdateTimeTaken := FTotalUpdateTimeTaken + aEndDate - aStartDate;

          //commit the data
          aStartDate := ALGetTickCount64;
          Tform1(fOwner).FirebirdConnectionPoolClient.Transactioncommit(aDbHandle,aTraHandle);
          aEndDate := ALGetTickCount64;
          FTotalCommitTimeTaken := FTotalCommitTimeTaken + aEndDate - aStartDate

        Except
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
      If (Tform1(fOwner).ALButtonFirebirdLoopSelect.tag = 2) or (Tform1(fOwner).ALButtonFirebirdLoopUpdate.tag = 2) then Break;
    end;

  Finally
    aLstSql.free;
    aXMLDATA.free;
  End;
end;

{*******************************************}
procedure TFirebirdBenchmarkThread.UpdateGUI;
Var timeElapsed: Int64;
    RequestCount: Integer;
    i: integer;
begin
  TForm1(fOwner).TableViewThread.BeginUpdate;
  try
    if not fOn then begin
      dec(TForm1(fOwner).NBActiveThread);
      TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(TForm1(fOwner).NBActiveThread);
      TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,inttostr(frank) + ' (off)');
      if TForm1(fOwner).NBActiveThread = 0 then begin
        timeElapsed := ALGetTickCount64 - TForm1(fOwner).StartTime;
        RequestCount := 0;
        for I := 0 to TForm1(fOwner).TableViewThread.DataController.RecordCount - 1 do
          RequestCount := RequestCount + TForm1(fOwner).TableViewThread.DataController.GetValue(I,Tform1(fOwner).TableViewThreadRequestCount.Index);
        TForm1(fOwner).StatusBar1.Panels[1].Text := inttostr(RequestCount) + ' Requests in ' + inttostr(round(timeElapsed / 1000)) + ' seconds (' + FormatFloat('0.##',RequestCount / (timeElapsed / 1000)) + ' Request/second';
        if fUpdateSQL then begin
          TForm1(fOwner).ALButtonFirebirdLoopUpdate.Tag := 0;
          TForm1(fOwner).ALButtonFirebirdLoopUpdate.Caption := 'Loop UPDATE via Firebird';
        end
        else begin
          TForm1(fOwner).ALButtonFirebirdLoopSelect.Tag := 0;
          TForm1(fOwner).ALButtonFirebirdLoopSelect.Caption := 'Loop SELECT via Firebird';
        end;
        TForm1(fOwner).FirebirdConnectionPoolClient.Free;
        TForm1(fOwner).FirebirdConnectionPoolClient := nil;
      end;
    end;
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadRequestCount.Index,fTotalUpdate);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageSelectUpdateTimeTaken.Index,FTotalUpdateTimeTaken / FtotalUpdate);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FtotalUpdate);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadErrorMsg.Index,FErrorMsg);
    TForm1(fOwner).StatusBar1.Panels[2].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
  finally
    TForm1(fOwner).TableViewThread.EndUpdate;
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
  FTotalUpdateTimeTaken := 0;
  FTotalCommitTimeTaken := 0;
  FtotalUpdate := 0;
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
    aXMLDATA: TalXmlDocument;
    aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
    aFormatSettings: TformatSettings;
    aLastUpdateGUI: Int64;
    S1: String;
    i: integer;
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
          S1 := fSQL;
          while AlPos('<#randomchar>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomchar>',AlRandomStr(1),[rfIgnoreCase]);
          while AlPos('<#randomnumber>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomnumber>',inttostr(random(10)),[rfIgnoreCase]);
          for i := 1 to maxint do begin
            if AlPos('<#randomnumber'+inttostr(i)+'>', AlLowerCase(S1)) > 0 then S1 := AlStringReplace(S1, '<#randomnumber'+inttostr(i)+'>',inttostr(random(10)),[rfIgnoreCase, rfReplaceAll])
            else break;
          end;
          aLstSql.Add(S1);
          setlength(aSelectDataSQLs,length(aSelectDataSQLs)+1);
          aSelectDataSQLs[length(aSelectDataSQLs)-1].Sql := S1;
          aSelectDataSQLs[length(aSelectDataSQLs)-1].RowTag := '';
          aSelectDataSQLs[length(aSelectDataSQLs)-1].viewTag := '';
          aSelectDataSQLs[length(aSelectDataSQLs)-1].skip := 0;
          aSelectDataSQLs[length(aSelectDataSQLs)-1].First := 0;
          inc(aLoopIndex);
          inc(FTotalUpdate);
          if aLoopIndex > fMaxLoop then break;
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
          FTotalUpdateTimeTaken := FTotalUpdateTimeTaken + aEndDate - aStartDate;

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
      If (Tform1(fOwner).ALButtonMySqlLoopSelect.tag = 2) or (Tform1(fOwner).ALButtonMySqlLoopUpdate.tag = 2) then Break;
    end;

  Finally
    aLstSql.free;
    aXMLDATA.free;
  End;
end;

{****************************************}
procedure TMySqlBenchmarkThread.UpdateGUI;
Var timeElapsed: Int64;
    RequestCount: Integer;
    i: integer;
begin
  TForm1(fOwner).TableViewThread.BeginUpdate;
  try
    if not fOn then begin
      dec(TForm1(fOwner).NBActiveThread);
      TForm1(fOwner).StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(TForm1(fOwner).NBActiveThread);
      TForm1(fOwner).TableViewThread.DataController.SetValue(frank-1,TForm1(fOwner).TableViewThreadNumber.Index,inttostr(frank) + ' (off)');
      if TForm1(fOwner).NBActiveThread = 0 then begin
        timeElapsed := ALGetTickCount64 - TForm1(fOwner).StartTime;
        RequestCount := 0;
        for I := 0 to TForm1(fOwner).TableViewThread.DataController.RecordCount - 1 do
          RequestCount := RequestCount + TForm1(fOwner).TableViewThread.DataController.GetValue(I,Tform1(fOwner).TableViewThreadRequestCount.Index);
        TForm1(fOwner).StatusBar1.Panels[1].Text := inttostr(RequestCount) + ' Requests in ' + inttostr(round(timeElapsed / 1000)) + ' seconds (' + FormatFloat('0.##',RequestCount / (timeElapsed / 1000)) + ' Request/second';
        if fUpdateSQL then begin
          TForm1(fOwner).ALButtonMySqlLoopUpdate.Tag := 0;
          TForm1(fOwner).ALButtonMySqlLoopUpdate.Caption := 'Loop UPDATE via MySql';
        end
        else begin
          TForm1(fOwner).ALButtonMySqlLoopSelect.Tag := 0;
          TForm1(fOwner).ALButtonMySqlLoopSelect.Caption := 'Loop SELECT via MySql';
        end;
        TForm1(fOwner).MySqlConnectionPoolClient.Free;
        TForm1(fOwner).MySqlConnectionPoolClient := nil;
      end;
    end;
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadRequestCount.Index,fTotalUpdate);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageSelectUpdateTimeTaken.Index,FTotalUpdateTimeTaken / FtotalUpdate);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadAverageCommitTimeTaken.Index,FTotalCommitTimeTaken / FtotalUpdate);
    TForm1(fOwner).TableViewThread.DataController.SetValue(fRank-1,Tform1(fOwner).TableViewThreadErrorMsg.Index,FErrorMsg);
    TForm1(fOwner).StatusBar1.Panels[2].Text := 'Memory usage: ' + inttostr(round((ProcessMemoryUsage(GetCurrentProcessID) / 1024) / 1024)) + ' Mb'
  finally
    TForm1(fOwner).TableViewThread.EndUpdate;
  end;
  application.ProcessMessages;
end;

initialization
  randomize;
  ReportMemoryLeaksOnShutdown := True;

end.
