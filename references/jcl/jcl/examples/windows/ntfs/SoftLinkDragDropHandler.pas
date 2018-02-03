//
// Robert Rossmair, 2001, 2004
//
// Adds "create junction here" entry to explorer context menu, when a directory
// is dragged & dropped onto a NTFS volume. When selected, it creates a NTFS
// junction to the source directory, instead of copying it to the new location.
//
// The name of the junction is prefixed with a "~" to mark it as different from
// a normal directory, since dumb ol' Explorer doesn't know nothing about NTFS
// junctions.
//
// This unit is based on $(DELPHI)\Demos\ActiveX\ShellExt\ContextM
//
unit SoftLinkDragDropHandler;

{$I jcl.inc}

interface

uses
  Windows, ActiveX, ComObj, ShlObj,
  JclBase, JclStrings, JclFileUtils, JclShell, JclNTFS;

type
  TDirDropContextMenu = class(TComObject, IShellExtInit, IContextMenu)
  private
    FLinkTarget: string;
    FLinkPath: string;
    FIsRootDirectory: Boolean;
  protected
    { IShellExtInit }
    function IShellExtInit.Initialize = SEIInitialize; // Avoid compiler warning
    function SEIInitialize(pidlFolder: PItemIDList; lpdobj: IDataObject;
      hKeyProgID: HKEY): HResult; stdcall;
    { IContextMenu }
    function QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst, idCmdLast,
      uFlags: UINT): HResult; stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;
    {$IFDEF RTL230_UP}
    function GetCommandString(idCmd: UINT_PTR; uFlags: UINT; pwReserved: PUINT;
      pszName: LPSTR; cchMax: UINT): HResult; stdcall;
    {$ELSE ~RTL230_UP}
    function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
      pszName: LPSTR; cchMax: UINT): HResult; stdcall;
    {$ENDIF ~RTL230_UP}
  end;

const
  Class_ContextMenu: TGUID = '{DDE0E099-9901-4507-9A47-3DC66B13AB6B}';

implementation

uses ComServ, SysUtils, ShellApi, Registry;

resourcestring
  SDescription = 'JEDI SoftLinks Shell Extension';
  SRegKeyDir = 'Directory\shellex\DragDropHandlers\JEDISoftLinks';
  SRegKeyDrive = 'Drive\shellex\DragDropHandlers\JEDISoftLinks';
  SMenuItem = 'Create junction here';
const
  SMenuHelp = AnsiString('Create an NTFS junction point');

const
  Prefix = '~';

function OnNtfsVolume(const FileName: string): Boolean;
begin
  Result := NtfsReparsePointsSupported(ExtractFileDrive(FileName));
end;

function TDirDropContextMenu.SEIInitialize(pidlFolder: PItemIDList; lpdobj: IDataObject;
  hKeyProgID: HKEY): HResult;
var
  FileName: string;
  LinkDir: string;
  Volume: string;
  StgMedium: TStgMedium;
  FormatEtc: TFormatEtc;
  Count, N: Integer;
begin
  FLinkPath := '';

  if (lpdobj = nil) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  with FormatEtc do
  begin
    cfFormat := CF_HDROP;
    ptd      := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex   := -1;
    tymed    := TYMED_HGLOBAL;
  end;

  // Render the data referenced by the IDataObject pointer to an HGLOBAL
  // storage medium in CF_HDROP format.
  Result := lpdobj.GetData(FormatEtc, StgMedium);
  if Failed(Result) then
    Exit;

  // If only one file is selected, retrieve the file name and store it in
  // FLinkTarget. Otherwise fail the call.
  Count := DragQueryFile(StgMedium.hGlobal, $FFFFFFFF, nil, 0);
  Result := E_FAIL;
  if Count = 1 then
  begin
    SetLength(FLinkTarget, DragQueryFile(StgMedium.hGlobal, 0, nil, 0) + 1);
    DragQueryFile(StgMedium.hGlobal, 0, PChar(FLinkTarget), Length(FLinkTarget));
    if DirectoryExists(FLinkTarget) then
    begin
      LinkDir := PidlToPath(pidlFolder);
      if OnNtfsVolume(LinkDir) then
      begin
        FileName := ExtractFileName(FLinkTarget);
        StrResetLength(FileName);
        FIsRootDirectory := FileName = '';
        if FIsRootDirectory then
        begin
          Volume := ExtractFileDrive(FLinkTarget);
          N := Pos(':', Volume);
          if N > 0 then
            SetLength(Volume, N - 1);
          FileName := Volume;
        end;
        FLinkPath := Format('%s' + Prefix + '%.175s', [PathAddSeparator(LinkDir), FileName]);
        Result := NOERROR;
      end;
    end;
  end;
  ReleaseStgMedium(StgMedium);
end;

function TDirDropContextMenu.QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst,
          idCmdLast, uFlags: UINT): HResult;
begin
  Result := 0; // or use MakeResult(SEVERITY_SUCCESS, FACILITY_NULL, 0);

  if FLinkPath = '' then
    Exit;

  if ((uFlags and $0000000F) = CMF_NORMAL) or
     ((uFlags and CMF_EXPLORE) <> 0) then
  begin
    // Add one menu item to context menu
    InsertMenu(Menu, indexMenu, MF_STRING or MF_BYPOSITION, idCmdFirst, PChar(SMenuItem));

    // Return number of menu items added
    Result := 1; // or use MakeResult(SEVERITY_SUCCESS, FACILITY_NULL, 1)
  end;
end;

function TDirDropContextMenu.InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult;
var
  Success: Boolean;
begin
  Result := E_FAIL;
  if (HiWord(Integer(lpici.lpVerb)) <> 0) then
  begin
    // We are called by an application
    Exit;
  end;

  if (LoWord(lpici.lpVerb) <> 0) then
  begin
    // invalid argument number
    Result := E_INVALIDARG;
    Exit;
  end;

  if (not DirectoryExists(FLinkPath) and CreateDir(FLinkPath)) {or DirectoryIsEmpty(FLinkPath)} then
  begin
    Success := NtfsCreateJunctionPoint(FLinkPath, FLinkTarget);
    if Success then
      SHChangeNotify(SHCNE_MKDIR, SHCNF_PATH, PChar(FLinkPath), nil);
  end;
end;

{$IFDEF RTL230_UP}
function TDirDropContextMenu.GetCommandString(idCmd: UINT_PTR; uFlags: UINT; pwReserved: PUINT;
  pszName: LPSTR; cchMax: UINT): HResult; stdcall;
{$ELSE ~RTL230_UP}
function TDirDropContextMenu.GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
  pszName: LPSTR; cchMax: UINT): HRESULT;
{$ENDIF ~RTL230_UP}
begin
  if (idCmd = 0) then
  begin
    if ({$IFDEF RTL230_UP}uFlags{$ELSE}uType{$ENDIF} = GCS_HELPTEXT) then
      // return help string for menu item
      StrCopy(pszName, PAnsiChar(SMenuHelp));
    Result := NOERROR;
  end
  else
    Result := E_INVALIDARG;
end;

type
  TDirDropContextMenuFactory = class(TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

procedure TDirDropContextMenuFactory.UpdateRegistry(Register: Boolean);
var
  ClassID: string;
begin
  if Register then
  begin
    inherited UpdateRegistry(Register);

    ClassID := GUIDToString(Class_ContextMenu);
    CreateRegKey(SRegKeyDir, '', ClassID);
    CreateRegKey(SRegKeyDrive, '', ClassID);

    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      with TRegistry.Create do
        try
          RootKey := HKEY_LOCAL_MACHINE;
          OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions', True);
          OpenKey('Approved', True);
          WriteString(ClassID, SDescription);
        finally
          Free;
        end;
  end
  else
  begin
    DeleteRegKey(SRegKeyDir);
    DeleteRegKey(SRegKeyDrive);
    inherited UpdateRegistry(Register);
  end;
end;

initialization
  TDirDropContextMenuFactory.Create(ComServer, TDirDropContextMenu, Class_ContextMenu,
    '', SDescription, ciMultiInstance,
    tmApartment);
end.
