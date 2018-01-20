unit SynDBExplorerMain;

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

{.$define USEZEOS}

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Grids,
  ExtCtrls,
  StdCtrls,
  {$ifndef FPC}
  Consts,
  {$ifdef HASINLINE}  XPMan,
  Contnrs, {$endif}
  {$endif}
  {$ifdef ISDELPHIXE}
  SynSQLite3RegEx, // use direct PCRE library as available since Delphi XE
  {$endif}
  SynCommons,
  SynZip,
  mORMot,
  SynSQLite3,
  SynSQLite3Static,
  mORMotUI,
  mORMotUIEdit,
  mORMotUILogin,
  mORMotToolBar,
  SynTaskDialog, // also fix QC 37403 for Delphi 6/7/2006
  SynTable, // for TSynFilter and TSynValidate
  SynDB,
  SynDBOracle,
  SynOleDB,
  SynDBSQLite3,
  SynDBODBC,
  SynDBRemote,
  {$ifdef USEZEOS}
  SynDBZeos,
  {$endif}
  SynDBExplorerClasses,
  SynDBExplorerFrame,
  ComCtrls;

type
  TDbExplorerMain = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    MainCaption: string;
    Connection: TExpConnectionType;
    Page: TSynPager;
    PageNew: TSynPage;
    TempFileName: TFileName;
    procedure PageChange(Sender: TObject);
    procedure PageDblClick(Sender: TObject);
  public
    ConnectionName: string;
    Tables: TStringList;
    Props: TSQLDBConnectionProperties;
    function CreateFrame: TDBExplorerFrame;
  end;

var
  DbExplorerMain: TDbExplorerMain;

  
resourcestring
  sSelectAConnection = 'Select a connection';
  sNew = 'New Connection';
  sNewOne = 'New';
  sConnectionHints = 'Display name|Database type|Server name '+
    '(for "Generic OLEDB", use ADO-like connection string, and ignore other fields; '+
    'for SQLite3 or Jet, specify the full file name)|'+
    'Database name (unneeded for Oracle/SQLite3/Jet/ODBC)|User login|'+
    'User password (set ? for prompt)';
  sSelectOrCreateAConnection = 'Select a connection to be used, or\n'+
    'click on "New connection" to create one.';
  sPleaseWaitN = 'Connecting to %s...';
  sPleaseWaitSynLz = 'Decompressing %s...';
  sUpdateConnection = 'Update connection settings';
  sPassPromptN = 'Please enter password for %s@%s:';


implementation

uses
  SynDBExplorerServer;


{$ifndef HASINLINE}
  {$R Vista.res}
{$endif}

{$R *.dfm}

procedure TDbExplorerMain.FormDestroy(Sender: TObject);
begin
  Tables.Free;
  Props.Free;
  if TempFileName<>'' then
    DeleteFile(TempFileName);
end;

function Crypt(const s: RawUTF8): RawUTF8;
var i: integer;
begin // just not to be written in plain ascii in .config file
  SetLength(result,length(s));
  for i := 0 to length(s)-1 do
    PByteArray(result)[i] := PByteArray(s)[i] xor (i+137);
end;

procedure TDbExplorerMain.FormCreate(Sender: TObject);
var Conns: TSQLRestStorageInMemory;
function TryConnect(C: TSQLConnection; LoadTableNames: boolean): boolean;
const CONN_CLASSES: array[TExpConnectionType] of TSQLDBConnectionPropertiesClass =
  (TSQLDBOracleConnectionProperties,
   TOleDBOracleConnectionProperties,TOleDBMSOracleConnectionProperties,
   TOleDBMSSQLConnectionProperties,TOleDBConnectionProperties,
   TSQLDBSQLite3ConnectionProperties,
   {$ifdef WIN64}
   nil, // no JET/MSAccess available under Win64
   {$else}
   TOleDBJetConnectionProperties,
   {$endif}
   TODBCConnectionProperties,
   TSQLDBWinHTTPConnectionProperties,
   {$ifdef USEZEOS}TSQLDBZeosConnectionProperties{$else}nil{$endif}
  );
var i: integer;
    Pass: RawUTF8;
begin
  result := false;
  if CONN_CLASSES[C.Connection]=nil then begin
    {$ifndef USEZEOS}
    if C.Connection=ctZEOS then
      ShowMessage('USEZEOS conditional should be defined in SynDBExplorerMain.pas',
        'Zeos/ZDBC not available',true);
    {$endif} 
    exit;
  end;
  try
    Pass := Crypt(C.Password);
    if Pass='?' then 
      Pass := StringToUTF8(InputBox(Caption,format(sPassPromptN,
        [UTF8ToString(C.UserName),UTF8ToString(C.Server)]),'',true));
    if C.Connection=ctGenericOLEDB then begin
      Props := TOleDBConnectionProperties.Create('','','','');
      with TOleDBConnectionProperties(Props) do begin
        ConnectionString := UTF8ToWideString(C.Server);
        if LoadTableNames then begin
          ConnectionStringDialogExecute(Handle);
          C.Server := WideStringToUTF8(ConnectionString);
        end;
      end;
    end else 
      Props := CONN_CLASSES[C.Connection].Create(C.Server,C.Database,C.UserName,Pass);
    ConnectionName := UTF8ToString(C.Ident);
    with CreateTempForm(format(sPleaseWaitN,[ConnectionName]),nil,True) do
    try
      Connection := C.Connection;
      MainCaption := format('%s %s (compiled with %s) - %s',
        [MainCaption,SYNOPSE_FRAMEWORK_VERSION,GetDelphiCompilerVersion,ConnectionName]);
      if LoadTableNames or                      // retrieve all needed info from DB
         (C.Connection=ctRemoteHTTP) then begin
        Props.GetTableNames(C.fTableNames);     // retrieve and set table names
        C.ForeignKeys := CompressString(Props.ForeignKeysData); // foreign keys
        if Conns<>nil then
          Conns.Modified := true;
      end else begin
        Props.ThreadSafeConnection.Connect;
        Props.ForeignKeysData := UncompressString(C.ForeignKeys); 
      end;
      for i := 0 to High(C.TableNames) do
        Tables.Add(UTF8ToString(C.TableNames[i]));
{$ifdef ISDELPHIXE}
     with TSQLDBSQLite3Connection(Props.ThreadSafeConnection) do
     if InheritsFrom(TSQLDBSQLite3Connection) then
       CreateRegExpFunction(DB.DB);
{$endif}
      result := true;
    finally
      Screen.Cursor := crDefault;
      Free;
    end;
  except
    on E: Exception do begin
      ShowMessage(E.Message,true);
      FreeAndNil(Props);
    end;
  end;
end;
var Btns: TCommonButtons;
    Task: TTaskDialog;
    C: TSQLConnection;
    CmdLine: TExpConnectionType;
    FN, msg, FN2: string;
    i, res: Integer;
    tmp: array[0..MAX_PATH] of char;
begin
  Conns := nil;
  DefaultFont.Name := 'Tahoma';
  DefaultFont.Size := 9;
  Tables := TStringList.Create;
  C := nil;
  with TSQLConnection.RecordProps do begin
    AddFilterOrValidate('Ident',TSynFilterTrim.Create);
    AddFilterOrValidate('Ident',TSynValidateText.Create);
    //AddFilterOrValidate('Server',TSynValidateText.Create);
  end;
  MainCaption := Caption;
  if (ParamCount=1) and FileExists(paramstr(1)) then begin
    FN := paramstr(1);
    CmdLine := ctOracleDirectOCI;
    if IsJetFile(FN) then
      CmdLine := ctJet_mdbOLEDB else
    if IsSQLite3File(FN) then
      CmdLine := ctSqlite3 else
    if TSQLDataBase.IsBackupSynLZFile(FN) then begin
      FN2 := ExtractFileName(FN);
      SetString(TempFileName, tmp, GetTempPath(SizeOf(tmp), tmp));
      TempFileName := TempFileName+FN2+'.db';
      DeleteFile(TempFileName);
      with CreateTempForm(format(sPleaseWaitSynLz,[FN2]),nil,True) do
      try
        if TSQLDatabase.BackupUnSynLZ(FN, TempFileName) then begin
          CmdLine := ctSqlite3;
          FN := TempFileName;
        end;
      finally
        Screen.Cursor := crDefault;
        Free;
      end;
    end;
    if CmdLine=ctOracleDirectOCI then begin
      ShowMessage(FN+'?',True);
      exit;
    end;
    C := TSQLConnection.Create;
    try
      C.Connection := CmdLine;
      C.Ident := StringToUTF8(FN);
      C.Server := C.Ident;
      TryConnect(C,True);
    finally
      C.Free;
    end;
  end else begin
    Conns := TSQLRestStorageInMemory.Create(
      TSQLConnection,nil,ChangeFileExt(ExeVersion.ProgramFileName,'.config'),false);
    try
      Conns.ExpandedJSON := true; // for better human reading and modification
      Task.Title := MainCaption;
      Task.Inst := sSelectAConnection;
      Task.Content := sSelectOrCreateAConnection;
      if Conns.Count=0 then
        Btns := [cbCancel] else begin
        for i := 0 to Conns.Count-1 do
          Task.Selection := Task.Selection+UTF8ToString(TSQLConnection(Conns[i]).Ident)+#10;
        Btns := [cbOk,cbCancel];
        Task.Query := UTF8ToString(TSQLConnection(Conns[0]).Ident);
        Task.Verify := sUpdateConnection;
      end;
      Task.VerifyChecked := false;
      Task.Buttons := sNew;
      res := Task.Execute(Btns,0,[],tiQuestion);
      case res of
      mrOk:
        if Task.VerifyChecked then begin
          C := TSQLConnection(Conns[Task.SelectionRes]);
          msg := Task.Verify;
        end else
          TryConnect(TSQLConnection(Conns[Task.SelectionRes]),false);
      mrBtn1: begin
        C := TSQLConnection.Create;
        msg := sNew;
      end;
      end;
      if C<>nil then
        with TRecordEditForm.Create(self) do
        try
          C.Password := Crypt(C.Password);
          SetRecord(nil,C,nil,nil,sConnectionHints,0,msg);
          if ShowModal=mrOk then begin
            C.Password := Crypt(C.Password);
            if TryConnect(C,true) and (res=mrBtn1) then
              Conns.AddOne(C,false,'') else
              if res=mrBtn1 then
                FreeAndNil(C);
          end else
            if res=mrBtn1 then
              FreeAndNil(C);
        finally
          Free;
        end;
    finally
      Conns.Free;
    end;
  end;
  Page := TSynPager.Create(self);
  Page.ControlStyle := Page.ControlStyle+[csClickEvents]; // enable OnDblClick
  Page.Parent := self;
  Page.Align := alClient;
  PageNew := TSynPage.Create(self);
  PageNew.Caption := sNewOne;
  PageNew.PageControl := Page;
  Page.OnChange := PageChange;
  Page.OnDblClick := PageDblClick;
end;

procedure TDbExplorerMain.FormShow(Sender: TObject);
begin
  if Props=nil then begin
    Close;
    exit;
  end;
  Caption := MainCaption;
  SetStyle(self);
  CreateFrame;
end;

procedure TDbExplorerMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    ord('T'):
      if Shift=[ssCtrl] then
        CreateFrame;
    VK_F9:
      with Page.ActivePage do
        if TObject(Tag).InheritsFrom(TDBExplorerFrame) then
          with TDBExplorerFrame(Tag) do
          if Shift=[] then
            BtnExecClick(BtnExec) else
          if ssShift in Shift then
            BtnExecClick(BtnExecToTab);
  end;
end;

var
  FrameID: integer;
  
function TDbExplorerMain.CreateFrame: TDBExplorerFrame;
var P: TSynPage;
begin
  P := TSynPage.Create(self);
  P.PageControl := Page;
  P.PageIndex := Page.PageCount-2;
  result := TDBExplorerFrame.Create(P);
  result.Parent := P;
  result.Align := alClient;
  result.Tables := Tables;
  result.Props := Props;
  result.EditTableChange(nil);
  inc(FrameID);
  result.Name := 'P'+IntToStr(FrameID);
  P.Tag := PtrInt(result);
  Page.ActivePage := P;
  result.EditTable.SetFocus;
  SetStyle(P);
end;

procedure TDbExplorerMain.PageChange(Sender: TObject);
begin
  if Page.ActivePage=PageNew then
    CreateFrame;
end;

procedure TDbExplorerMain.PageDblClick(Sender: TObject);
var n, i: Integer;
begin
  i := Page.ActivePageIndex;
  n := Page.PageCount-2;
  if n>0 then begin
    Page.ActivePage.Free;
    if i=n then
      Page.ActivePageIndex := n-1;
  end;
end;

end.
