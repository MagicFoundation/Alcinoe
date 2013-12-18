
unit main;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, SysUtils, Forms, ActiveX, Classes, ComObj, ShlObj, ShellAPI;

type

  TShellFile = class(TComObject, IShellExtInit)
  private
    FFilenames: array of PChar;
    { IShellExtInit }
    function IShellExtInit.Initialize = ShellInit;
    function ShellInit(pidlFolder: PItemIDList; lpdobj: IDataObject;
      hKeyProgID: HKEY): HResult; stdcall;
  public
    destructor Destroy; override;
  end;

  TShellDatabase = class(TShellFile, IContextMenu)
  protected
    { IContextMenu }
    function QueryContextMenu(Menu: HMENU;
      indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HResult; stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;
    function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
      pszName: LPSTR; cchMax: UINT): HResult; stdcall;
  end;

  TShellBackup = class(TShellFile, IContextMenu)
  protected
    { IContextMenu }
    function QueryContextMenu(Menu: HMENU;
      indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HResult; stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;
    function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
      pszName: LPSTR; cchMax: UINT): HResult; stdcall;
  end;

  TShellDatabaseFactory = class(TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

  TShellBackupFactory = class(TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

  TOnInvokeCommand = function(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;

  TShellMenuItem = record
    Name, Verb, Help: string;
    Command: TOnInvokeCommand;
  end;

function Backup(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;
function Restore(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;
function DBInfos(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;
function Shutdown(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;
function Clone(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;
function Pump(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;
function Options(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;


const
  Class_ShellDatabase: TGUID = '{23485D05-6E1E-4586-A47F-116514B9B4C8}';
  Class_ShellBackup: TGUID = '{DEA167C0-4707-43EB-8791-EB15752EB919}';

  DatabaseNameSpace = 'UIBDatabase';
  BackupNameSpace = 'UIBBackup';

  DatabaseItems: array[0..5] of TShellMenuItem = (
    (Name: 'Backup'; Verb: ''; Help: 'Backup database'; Command: Backup),
    (Name: 'Clone'; Verb: ''; Help: 'Clone database'; Command: Clone),
    (Name: 'Pump'; Verb: ''; Help: 'Pump database'; Command: Pump),
    (Name: 'Informations'; Verb: ''; Help: 'Database informations'; Command: DBInfos),
    (Name: 'Shutdown'; Verb: ''; Help: 'Shutdown database'; Command: Shutdown),
    (Name: 'Options'; Verb: ''; Help: 'Options'; Command: Options));

  BackupItems: array[0..1] of TShellMenuItem = (
    (Name: 'Restore'; Verb: ''; Help: 'Restore database'; Command: Restore),
    (Name: 'Options'; Verb: ''; Help: 'Options'; Command: Options));

implementation

uses ComServ, ComConst, Backup, Restore, Infos, Shutdown, Clone, Pump, Options;

function Backup(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;
var
  f: TBackupForm;
  s, e: string;
  i: Integer;
begin
  for i := 0 to Length(sender.FFilenames) - 1 do
  begin
    f := TBackupForm.Create(Application);
    s := sender.FFilenames[i];
    f.UIBBackup.Database := s;
    f.Caption := 'Backup: ' + s;

    e := ExtractFileExt(s);
    s := copy(s, 1, length(s) - length(e));
    if CompareText(e, '.IB') = 0 then s := s + '.IBK' else
    if CompareText(e, '.FDB') = 0 then s := s + '.FBK' else
      s := s + '.GBK';
    f.edBackupFile.Text := s;
    f.Show;
  end;
  Result := NOERROR;
end;

function Restore(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;
var
  f: TRestoreForm;
  s, e: string;
  i: integer;
begin
  for i := 0 to Length(Sender.FFilenames) - 1 do
  begin
    f := TRestoreForm.Create(Application);
    s := sender.FFilenames[i];
    f.UIBRestore.BackupFiles.Text := s;
    f.Caption := 'Restore: ' + s;

    e := ExtractFileExt(s);
    s := copy(s, 1, length(s) - length(e));
    if CompareText(e, '.IBK') = 0 then s := s + '.IB' else
    if CompareText(e, '.FBK') = 0 then s := s + '.FDB' else
      s := s + '.GDB';
    f.edRestoreFile.Text := s;
    f.Show;
  end;
  Result := NOERROR;
end;

function DBInfos(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;
var
  f: TInfosForm;
  i: integer;
begin
  for i := 0 to Length(Sender.FFilenames) - 1 do
  begin
    f := TInfosForm.Create(Application);
    f.Database.DatabaseName := sender.FFilenames[i];
    f.Show;
  end;
  Result := NOERROR;
end;

function Shutdown(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;
var
  f: TShutDownForm;
begin
  f := TShutDownForm.Create(Application);
  f.Config.DatabaseName := sender.FFilenames[0];;
  f.Show;
  Result := NOERROR;
end;

function Clone(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;
var
  f: TCloneForm;
  s, e: string;
  i: integer;
begin
  for i := 0 to Length(Sender.FFilenames) - 1 do
  begin
    f := TCloneForm.Create(Application);
    s := sender.FFilenames[i];
    f.Source.DatabaseName := s;
    f.Caption := 'Clone: ' + s;

    e := ExtractFileExt(s);
    s := copy(s, 1, length(s) - length(e)) + '_Clone' + e;
    f.edCloneFile.Text := s;
    f.Show;
  end;
  Result := NOERROR;
end;

function Pump(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;
var
  f: TPumpForm;
  s, e: string;
  i: integer;
begin
  for i := 0 to Length(Sender.FFilenames) - 1 do
  begin
    f := TPumpForm.Create(Application);
    s := sender.FFilenames[i];
    f.Source.DatabaseName := s;
    f.Caption := 'Pump from ' + s;

    e := ExtractFileExt(s);
    s := copy(s, 1, length(s) - length(e)) + '_Empty' + e;
    f.edPumpFile.Text := s;
    f.Show;
  end;
  Result := NOERROR;
end;


function Options(Sender: TShellFile; var lpici: TCMInvokeCommandInfo): HResult;
var
  f: TOptionsForm;
begin
  f := TOptionsForm.Create(Application);
  f.Show;
  Result := NOERROR;
end;

///////////////////


procedure DeleteRegValue(Key, Value: string; RootKey: Cardinal = HKEY_LOCAL_MACHINE);
var
  Handle: HKEY;
begin
  RegOpenKeyEx(RootKey, PChar(Key), 0, KEY_SET_VALUE, Handle);
  RegDeleteValue(Handle, PChar(Value));
  RegCloseKey(Handle);
end;

procedure CreateRegKeyDWord(const Key, ValueName: string; Value: DWord; RootKey: DWord = HKEY_CLASSES_ROOT);
var
  Handle: HKey;
  Status, Disposition: Integer;
begin
  Status := RegCreateKeyEx(RootKey, PChar(Key), 0, '',
    REG_OPTION_NON_VOLATILE, KEY_READ or KEY_WRITE, nil, Handle,
    @Disposition);
  if Status = 0 then
  begin
    Status := RegSetValueEx(Handle, PChar(ValueName), 0, REG_DWORD,
      @Value, SizeOf(Value));
    RegCloseKey(Handle);
  end;
  if Status <> 0 then
    raise EOleRegistrationError.CreateRes(@SCreateRegKeyError);
end;

{ TShellFileFactory }

procedure TShellDatabaseFactory.UpdateRegistry(Register: Boolean);
begin
  if register then
  begin
    CreateRegKey('.gdb', '', DatabaseNameSpace);
    CreateRegKey('.ib', '', DatabaseNameSpace);
    CreateRegKey('.fdb', '', DatabaseNameSpace);
    CreateRegKey(DatabaseNameSpace + '\shellex\ContextMenuHandlers\' + GUIDToString(ClassID), '', '');
    CreateRegKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\Approved',
      GUIDToString(ClassID), DatabaseNameSpace + ' Extension', HKEY_LOCAL_MACHINE);
  end
  else
  begin
    DeleteRegKey('.gdb');
    DeleteRegKey('.ib');
    DeleteRegKey('.fdb');
    DeleteRegKey(DatabaseNameSpace + '\shellex\ContextMenuHandlers\' + GUIDToString(ClassID));
    DeleteRegKey(DatabaseNameSpace + '\shellex\ContextMenuHandlers\');
    DeleteRegKey(DatabaseNameSpace + '\shellex\');
    DeleteRegKey(DatabaseNameSpace + '\shell\explore\command');
    DeleteRegKey(DatabaseNameSpace + '\shell\explore');
    DeleteRegKey(DatabaseNameSpace + '\shell\');

    DeleteRegKey(DatabaseNameSpace + '\');

    DeleteRegValue('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\Approved\', GUIDToString(ClassID));
  end;
  inherited;
end;

{ TShellFile }

function TShellFile.ShellInit(pidlFolder: PItemIDList;
  lpdobj: IDataObject; hKeyProgID: HKEY): HResult;
var
  StgMedium: TStgMedium;
  FormatEtc: TFormatEtc;
  filesnum, i: integer;
  filename: PChar;
  nameLength: UInt;
begin
  if lpdobj = nil then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  with FormatEtc do
  begin
    cfFormat := CF_HDROP;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;

  Result := lpdobj.GetData(FormatEtc, StgMedium);
  if Failed(Result) then
    Exit;
  try
    filesnum := DragQueryFile(StgMedium.hGlobal, $FFFFFFFF, nil, 0);
    if filesnum > 0 then
    begin
      SetLength(FFileNames, filesnum);
      for i := 0 to filesnum - 1 do
      begin
        nameLength := DragQueryFile(StgMedium.hGlobal, i, nil, 0) + 1;
        filename := StrAlloc(nameLength);
        DragQueryFile(StgMedium.hGlobal, i, filename, nameLength);
        FFilenames[i] := filename;
        Result := NOERROR;
      end;
    end
    else
    begin
      SetLength(FFileNames, 0);
      Result := E_FAIL;
    end;
  finally
    ReleaseStgMedium(StgMedium);
  end;
end;

destructor TShellFile.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(FFilenames) - 1 do
    StrDispose(FFilenames[i]);
  inherited;
end;

{ TShellDatabase }

function TShellDatabase.GetCommandString(idCmd, uType: UINT;
  pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HResult;
begin
  Result := NOERROR;
  if (idCmd > High(DatabaseItems)) then
    Result := S_FALSE
  else
    case uType of
      GCS_VERBA: lstrcpynA(pszName, PChar(DatabaseItems[idCmd].Verb), cchMax);
      GCS_HELPTEXTA: lstrcpynA(pszName, PChar(DatabaseItems[idCmd].Help), cchMax);
      GCS_VERBW: lstrcpynW(PWideChar(pszName), PWideChar(WideString(DatabaseItems[idCmd].Verb)), cchMax);
      GCS_HELPTEXTW: lstrcpynW(PWideChar(pszName), PWideChar(WideString(DatabaseItems[idCmd].Help)), cchMax);
      GCS_VALIDATEW, GCS_VALIDATEA: Result := S_OK;
    else
      Result := S_FALSE;
    end;
end;

function TShellDatabase.InvokeCommand(
  var lpici: TCMInvokeCommandInfo): HResult;
var
  idCmd: UINT;
begin
  Result := E_INVALIDARG;
  if (not HIWORD(Cardinal(lpici.lpVerb))) <> 0 then
  begin
    idCmd := LOWORD(Cardinal(lpici.lpVerb));
    if idCmd <= high(DatabaseItems) then
      Result := DatabaseItems[idCmd].Command(Self, lpici);
  end;
end;

function TShellDatabase.QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst,
  idCmdLast, uFlags: UINT): HResult;
var
  Cmd: Cardinal;
  i: Integer;
begin
  Cmd := idCmdFirst;
  for i := low(DatabaseItems) to High(DatabaseItems) do
  begin
    InsertMenu(Menu, indexMenu, MF_STRING or MF_BYPOSITION, Cmd, PChar(DatabaseItems[i].Name));
    inc(indexMenu);
    inc(Cmd);
  end;
  InsertMenu(Menu, indexMenu, MF_SEPARATOR or MF_BYPOSITION, 0, nil);
  Result := Cmd - idCmdFirst;
end;

{ TShellBackup }

function TShellBackup.GetCommandString(idCmd, uType: UINT;
  pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HResult;
begin
  Result := NOERROR;
  if (idCmd > High(BackupItems)) then
    Result := S_FALSE
  else
    case uType of
      GCS_VERBA: lstrcpynA(pszName, PChar(BackupItems[idCmd].Verb), cchMax);
      GCS_HELPTEXTA: lstrcpynA(pszName, PChar(BackupItems[idCmd].Help), cchMax);
      GCS_VERBW: lstrcpynW(PWideChar(pszName), PWideChar(WideString(BackupItems[idCmd].Verb)), cchMax);
      GCS_HELPTEXTW: lstrcpynW(PWideChar(pszName), PWideChar(WideString(BackupItems[idCmd].Help)), cchMax);
      GCS_VALIDATEW, GCS_VALIDATEA: Result := S_OK;
    else
      Result := S_FALSE;
    end;
end;

function TShellBackup.InvokeCommand(
  var lpici: TCMInvokeCommandInfo): HResult;
var
  idCmd: UINT;
begin
  Result := E_INVALIDARG;
  if (not HIWORD(Cardinal(lpici.lpVerb))) <> 0 then
  begin
    idCmd := LOWORD(Cardinal(lpici.lpVerb));
    if idCmd <= high(BackupItems) then
      Result := BackupItems[idCmd].Command(Self, lpici);
  end;
end;

function TShellBackup.QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst,
  idCmdLast, uFlags: UINT): HResult;
var
  Cmd: Cardinal;
  i: Integer;
begin
  Cmd := idCmdFirst;
  for i := low(BackupItems) to High(BackupItems) do
  begin
    InsertMenu(Menu, indexMenu, MF_STRING or MF_BYPOSITION, Cmd, PChar(BackupItems[i].Name));
    inc(indexMenu);
    inc(Cmd);
  end;
  InsertMenu(Menu, indexMenu, MF_SEPARATOR or MF_BYPOSITION, 0, nil);
  Result := Cmd - idCmdFirst;
end;

{ TShellBackupFactory }

procedure TShellBackupFactory.UpdateRegistry(Register: Boolean);
begin
  if register then
  begin
    CreateRegKey('.gbk', '', BackupNameSpace);
    CreateRegKey('.ibk', '', BackupNameSpace);
    CreateRegKey('.fbk', '', BackupNameSpace);
    CreateRegKey(BackupNameSpace + '\shellex\ContextMenuHandlers\' + GUIDToString(ClassID), '', '');
    CreateRegKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\Approved',
      GUIDToString(ClassID), BackupNameSpace + ' Extension', HKEY_LOCAL_MACHINE);
  end
  else
  begin
    DeleteRegKey('.gbk');
    DeleteRegKey('.ibk');
    DeleteRegKey('.fbk');
    DeleteRegKey(BackupNameSpace + '\shellex\ContextMenuHandlers\' + GUIDToString(ClassID));
    DeleteRegKey(BackupNameSpace + '\shellex\ContextMenuHandlers\');
    DeleteRegKey(BackupNameSpace + '\shellex\');
    DeleteRegKey(BackupNameSpace + '\shell\explore\command');
    DeleteRegKey(BackupNameSpace + '\shell\explore');
    DeleteRegKey(BackupNameSpace + '\shell\');
    DeleteRegKey(BackupNameSpace + '\');
    DeleteRegValue('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\Approved\', GUIDToString(ClassID));
  end;
  inherited;
end;

initialization
  TShellDatabaseFactory.Create(ComServer, TShellDatabase, Class_ShellDatabase,
    DatabaseNameSpace, '', ciMultiInstance, tmApartment);
    
  TShellBackupFactory.Create(ComServer, TShellBackup, Class_ShellBackup,
    BackupNameSpace, '', ciMultiInstance, tmApartment);

end.

