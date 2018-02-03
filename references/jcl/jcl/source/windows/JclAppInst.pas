{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclAppInst.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) Petr Vones. All Rights Reserved.                                                   }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains a class and support routines for controlling the number of concurrent         }
{ instances of your application that can exist at any time. In addition there is support for       }
{ simple interprocess communication between these instance including a notification mechanism.     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclAppInst;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  Winapi.Windows, System.Classes, Winapi.Messages,
  {$ELSE ~HAS_UNITSCOPE}
  Windows, Classes, Messages,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase, JclFileUtils, JclSynch, JclWin32;

// Message constants and types
type
  TJclAppInstDataKind = Integer;

const
  AI_INSTANCECREATED = $0001;
  AI_INSTANCEDESTROYED = $0002;
  AI_USERMSG = $0003;

  AppInstDataKindNoData = -1;
  AppInstCmdLineDataKind = 1;

// Application instances manager class
type
  TJclAppInstances = class(TObject)
  private
    FCPID: DWORD;
    FAllMapping: TJclSwapFileMapping;
    FAllMappingView: TJclFileMappingView;
    FSessionMapping: TJclSwapFileMapping;
    FSessionMappingView: TJclFileMappingView;
    FUserMapping: TJclSwapFileMapping;
    FUserMappingView: TJclFileMappingView;
    FMessageID: DWORD;
    FOptex: TJclOptex;
    FUniqueAppID: string;
    function GetAllAppWnds(Index: Integer): THandle;
    function GetAllInstanceCount: Integer;
    function GetAllInstanceIndex(ProcessID: DWORD): Integer;
    function GetAllProcessIDs(Index: Integer): DWORD;
    function GetInstanceCount(MappingView: TJclFileMappingView): Integer;
    function GetInstanceIndex(MappingView: TJclFileMappingView; ProcessID: DWORD): Integer;
    function GetProcessIDs(MappingView: TJclFileMappingView; Index: Integer): DWORD;
    function GetSessionAppWnds(Index: Integer): THandle;
    function GetSessionInstanceCount: Integer;
    function GetSessionInstanceIndex(ProcessID: DWORD): Integer;
    function GetSessionProcessIDs(Index: Integer): DWORD;
    function GetUserAppWnds(Index: Integer): THandle;
    function GetUserInstanceCount: Integer;
    function GetUserInstanceIndex(ProcessID: DWORD): Integer;
    function GetUserProcessIDs(Index: Integer): DWORD;
  protected
    procedure InitData;
    procedure InitAllData;
    procedure InitSessionData;
    procedure InitUserData;
    procedure NotifyInstances(const W, L: Longint);
    procedure RemoveInstance(MappingView: TJclFileMappingView);
    procedure SecurityFree(UserInfo: PTokenUser; SID: PSID; ACL: PACL;
      SecurityDescriptor: PSecurityDescriptor; SecurityAttributes: PSecurityAttributes);
    procedure SecurityGetAllUsers(out UserInfo: PTokenUser; out SID: PSID; out ACL: PACL;
      out SecurityDescriptor: PSecurityDescriptor; out SecurityAttributes: PSecurityAttributes);
    procedure SecurityGetCurrentUser(out UserInfo: PTokenUser; out SID: PSID; out ACL: PACL;
      out SecurityDescriptor: PSecurityDescriptor; out SecurityAttributes: PSecurityAttributes);
    procedure SecurityGetCurrentUserInfo(out UserInfo: PTokenUser);
    procedure SecurityGetSecurityAttributes(OwnerSID, AccessSID: PSID; out ACL: PACL;
      out SecurityDescriptor: PSecurityDescriptor; out SecurityAttributes: PSecurityAttributes);
  public
    constructor Create;
    destructor Destroy; override;
    class function BringAppWindowToFront(const Wnd: THandle): Boolean;
    class function GetApplicationWnd(const ProcessID: DWORD): THandle;
    class procedure KillInstance;
    class function SetForegroundWindow98(const Wnd: THandle): Boolean;
    function CheckInstance(MaxInstances: Word; MaxSessionInstances: Word = 0;
      MaxUserInstances: Word = 0): Boolean;
    procedure CheckMultipleInstances(MaxInstances: Word; MaxSessionInstances: Word = 0;
      MaxUserInstances: Word = 0);
    procedure CheckSingleInstance;
    function SendCmdLineParams(const WindowClassName: string; const OriginatorWnd: THandle): Boolean;
    function SendData(const WindowClassName: string; const DataKind: TJclAppInstDataKind;
      Data: Pointer; const Size: Integer;
      OriginatorWnd: THandle): Boolean;
    function SendString(const WindowClassName: string; const DataKind: TJclAppInstDataKind;
      const S: string; OriginatorWnd: THandle): Boolean;
    function SendStrings(const WindowClassName: string; const DataKind: TJclAppInstDataKind;
      const Strings: TStrings; OriginatorWnd: THandle): Boolean;
    function SessionSwitchTo(Index: Integer): Boolean;
    function SwitchTo(Index: Integer): Boolean;
    function UserSwitchTo(Index: Integer): Boolean;
    procedure UserNotify(Param: Longint);
    property AppWnds[Index: Integer]: THandle read GetAllAppWnds;
    property InstanceIndex[ProcessID: DWORD]: Integer read GetAllInstanceIndex;
    property InstanceCount: Integer read GetAllInstanceCount;
    property MessageID: DWORD read FMessageID;
    property ProcessIDs[Index: Integer]: DWORD read GetAllProcessIDs;
    property SessionAppWnds[Index: Integer]: THandle read GetSessionAppWnds;
    property SessionInstanceIndex[ProcessID: DWORD]: Integer read GetSessionInstanceIndex;
    property SessionInstanceCount: Integer read GetSessionInstanceCount;
    property SessionProcessIDs[Index: Integer]: DWORD read GetSessionProcessIDs;
    property UserAppWnds[Index: Integer]: THandle read GetUserAppWnds;
    property UserInstanceIndex[ProcessID: DWORD]: Integer read GetUserInstanceIndex;
    property UserInstanceCount: Integer read GetUserInstanceCount;
    property UserProcessIDs[Index: Integer]: DWORD read GetUserProcessIDs;
  end;

function JclAppInstances: TJclAppInstances; overload;
function JclAppInstances(const UniqueAppIdGuidStr: string): TJclAppInstances; overload;

// Interprocess communication routines
function ReadMessageCheck(var Message: TMessage; const IgnoredOriginatorWnd: THandle): TJclAppInstDataKind;
procedure ReadMessageData(const Message: TMessage; var Data: Pointer; var Size: Integer);
procedure ReadMessageString(const Message: TMessage; out S: string);
procedure ReadMessageStrings(const Message: TMessage; const Strings: TStrings);

function SendData(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const Data: Pointer; const Size: Integer): Boolean;
function SendStrings(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const Strings: TStrings): Boolean;
function SendCmdLineParams(const Wnd, OriginatorWnd: HWND): Boolean;
function SendString(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const S: string): Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\windows';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  JclSecurity,
  JclStrings;

{$IFDEF FPC}  // missing declaration from unit Messages
type
  TWMCopyData = record
      Msg: UINT;
      From: THandle;
      CopyDataStruct: PCopyDataStruct;
      Result : LRESULT;
    End;
{$ENDIF FPC}

const
  { strings to form a unique name for file mapping and optex objects }
  JclAIPrefix = 'Jcl';
  JclAIOptex = '_Otx';
  JclAIAllMapping = '_All';
  JclAISessionMapping = '_Session_';
  JclAIUserMapping = '_User_';

  { window message used for communication between instances }
  JclAIMessage = '_Msg';

  { maximum number of instance that may exist at any time }
  JclAIMaxInstances = 256;

  { name of the application window class }
  ClassNameOfTApplication = 'TApplication';

type
  { management data to keep track of application instances. this data is shared amongst all instances
    and must be appropriately protected from concurrent access at all time }

  PJclAISharedData = ^TJclAISharedData;
  TJclAISharedData = packed record
    MaxInst: Word;
    Count: Word;
    ProcessIDs: array [0..JclAIMaxInstances] of DWORD;
  end;

var
  { the single global TJclAppInstance instance }
  AppInstances: TJclAppInstances;
  ExplicitUniqueAppId: string;

//=== { TJclAppInstances } ===================================================

constructor TJclAppInstances.Create;
begin
  inherited Create;
  FCPID := GetCurrentProcessId;
  InitData;
end;

destructor TJclAppInstances.Destroy;
begin
  if FAllMapping <> nil then
    RemoveInstance(FAllMappingView);
  if FSessionMapping <> nil then
    RemoveInstance(FSessionMappingView);
  if FUserMapping <> nil then
    RemoveInstance(FUserMappingView);

  NotifyInstances(AI_INSTANCEDESTROYED, Integer(FCPID));

  FreeAndNil(FAllMapping);
  FreeAndNil(FSessionMapping);
  FreeAndNil(FUserMapping);
  FreeAndNil(FOptex);
  inherited Destroy;
end;

class function TJclAppInstances.BringAppWindowToFront(const Wnd: THandle): Boolean;
begin
  if IsIconic(Wnd) then
    SendMessage(Wnd, WM_SYSCOMMAND, SC_RESTORE, 0);
  Result := SetForegroundWindow98(Wnd);
end;

function TJclAppInstances.CheckInstance(MaxInstances, MaxSessionInstances, MaxUserInstances: Word): Boolean;
var
  SharedData: PJclAISharedData;
  CurrentProcessId: DWORD;
begin
  CurrentProcessId := GetCurrentProcessId;
  FOptex.Enter;
  try
    // check all instances
    SharedData := PJclAISharedData(FAllMappingView.Memory);
    if SharedData^.MaxInst = 0 then
      SharedData^.MaxInst := MaxInstances;
    Result := (SharedData^.MaxInst = 0) or (SharedData^.Count < SharedData^.MaxInst);
    SharedData^.ProcessIDs[SharedData^.Count] := CurrentProcessId;
    Inc(SharedData^.Count);

    // check session instances
    SharedData := PJclAISharedData(FSessionMappingView.Memory);
    if SharedData^.MaxInst = 0 then
      SharedData^.MaxInst := MaxSessionInstances;
    Result := Result and ((SharedData^.MaxInst = 0) or (SharedData^.Count < SharedData^.MaxInst));
    SharedData^.ProcessIDs[SharedData^.Count] := CurrentProcessId;
    Inc(SharedData^.Count);

    // check user instances
    SharedData := PJclAISharedData(FUserMappingView.Memory);
    if SharedData^.MaxInst = 0 then
      SharedData^.MaxInst := MaxUserInstances;
    Result := Result and ((SharedData^.MaxInst = 0) or (SharedData^.Count < SharedData^.MaxInst));
    SharedData^.ProcessIDs[SharedData^.Count] := CurrentProcessId;
    Inc(SharedData^.Count);
  finally
    FOptex.Leave;
  end;
  if Result then
    NotifyInstances(AI_INSTANCECREATED, Integer(FCPID));
end;

procedure TJclAppInstances.CheckMultipleInstances(MaxInstances, MaxSessionInstances, MaxUserInstances: Word);
begin
  if not CheckInstance(MaxInstances, MaxSessionInstances, MaxUserInstances) then
  begin
    SwitchTo(0);
    KillInstance;
  end;
end;

procedure TJclAppInstances.CheckSingleInstance;
begin
  CheckMultipleInstances(1);
end;

type
  PTopLevelWnd = ^TTopLevelWnd;
  TTopLevelWnd = record
    ProcessID: DWORD;
    Wnd: THandle;
  end;

function EnumApplicationWinProc(Wnd: THandle; Param: PTopLevelWnd): BOOL; stdcall;
var
  PID: DWORD;
  C: array [0..Length(ClassNameOfTApplication) + 1] of Char;
begin
  GetWindowThreadProcessId(Wnd, @PID);
  if (PID = Param^.ProcessID) and (GetClassName(Wnd, C, Length(C)) > 0) and (C = ClassNameOfTApplication) then
  begin
    Result := False;
    Param^.Wnd := Wnd;
  end
  else
  begin
    Result := True;
  end;
end;

class function TJclAppInstances.GetApplicationWnd(const ProcessID: DWORD): THandle;
var
  TopLevelWnd: TTopLevelWnd;
begin
  TopLevelWnd.ProcessID := ProcessID;
  TopLevelWnd.Wnd := 0;
  EnumWindows(@EnumApplicationWinProc, LPARAM(@TopLevelWnd));
  Result := TopLevelWnd.Wnd;
end;

function TJclAppInstances.GetAllAppWnds(Index: Integer): THandle;
begin
  Result := GetApplicationWnd(GetAllProcessIDs(Index));
end;

function TJclAppInstances.GetAllInstanceCount: Integer;
begin
  Result := GetInstanceCount(FAllMappingView);
end;

function TJclAppInstances.GetAllInstanceIndex(ProcessID: DWORD): Integer;
begin
  Result := GetInstanceIndex(FAllMappingView, ProcessID);
end;

function TJclAppInstances.GetAllProcessIDs(Index: Integer): DWORD;
begin
  Result := GetProcessIDs(FAllMappingView, Index);
end;

function TJclAppInstances.GetInstanceCount(MappingView: TJclFileMappingView): Integer;
begin
  FOptex.Enter;
  try
    Result := PJclAISharedData(MappingView.Memory)^.Count;
  finally
    FOptex.Leave;
  end;
end;

function TJclAppInstances.GetInstanceIndex(MappingView: TJclFileMappingView; ProcessID: DWORD): Integer;
var
  I: Integer;
  SharedData: PJclAISharedData;
begin
  Result := -1;
  FOptex.Enter;
  try
    SharedData := PJclAISharedData(MappingView.Memory);
    for I := 0 to SharedData^.Count - 1 do
      if SharedData^.ProcessIDs[I] = ProcessID then
      begin
        Result := I;
        Break;
      end;
  finally
    FOptex.Leave;
  end;
end;

function TJclAppInstances.GetProcessIDs(MappingView: TJclFileMappingView; Index: Integer): DWORD;
var
  SharedData: PJclAISharedData;
begin
  FOptex.Enter;
  try
    SharedData := PJclAISharedData(MappingView.Memory);
    if Index >= SharedData^.Count then
      Result := 0
    else
      Result := SharedData^.ProcessIDs[Index];
  finally
    FOptex.Leave;
  end;
end;

function TJclAppInstances.GetSessionAppWnds(Index: Integer): THandle;
begin
  Result := GetApplicationWnd(GetProcessIDs(FSessionMappingView, Index));
end;

function TJclAppInstances.GetSessionInstanceCount: Integer;
begin
  Result := GetInstanceCount(FSessionMappingView);
end;

function TJclAppInstances.GetSessionInstanceIndex(ProcessID: DWORD): Integer;
begin
  Result := GetInstanceIndex(FSessionMappingView, ProcessID);
end;

function TJclAppInstances.GetSessionProcessIDs(Index: Integer): DWORD;
begin
  Result := GetProcessIDs(FSessionMappingView, Index);
end;

function TJclAppInstances.GetUserAppWnds(Index: Integer): THandle;
begin
  Result := GetApplicationWnd(GetProcessIDs(FUserMappingView, Index));
end;

function TJclAppInstances.GetUserInstanceCount: Integer;
begin
  Result := GetInstanceCount(FUserMappingView);
end;

function TJclAppInstances.GetUserInstanceIndex(ProcessID: DWORD): Integer;
begin
  Result := GetInstanceIndex(FUserMappingView, ProcessID);
end;

function TJclAppInstances.GetUserProcessIDs(Index: Integer): DWORD;
begin
  Result := GetProcessIDs(FUserMappingView, Index);
end;

const
  ACL_REVISION = 2;

type
  _ACE_HEADER = record
    AceType: BYTE;
    AceFlags: BYTE;
    AceSize: WORD;
  end;
  ACE_HEADER = _ACE_HEADER;
  PACE_HEADER = ^_ACE_HEADER;

  _ACCESS_ALLOWED_ACE = record
    Header: ACE_HEADER;
    Mask: ACCESS_MASK;
    SidStart: DWORD;
  end;

  ACCESS_ALLOWED_ACE = _ACCESS_ALLOWED_ACE;
  PACCESS_ALLOWED_ACE = ^_ACCESS_ALLOWED_ACE;
  
procedure TJclAppInstances.InitData;
begin
  if ExplicitUniqueAppId <> '' then
    FUniqueAppID := JclAIPrefix + ExplicitUniqueAppId
  else
    FUniqueAppID := AnsiUpperCase(JclAIPrefix + ParamStr(0));

  CharReplace(FUniqueAppID, '\', '_');

  FMessageID := RegisterWindowMessage(PChar(FUniqueAppID + JclAIMessage));

  FOptex := TJclOptex.Create(FUniqueAppID + JclAIOptex, 4000);

  InitAllData;
  InitSessionData;
  InitUserData;
end;

procedure TJclAppInstances.InitAllData;
var
  UserInfo: PTokenUser;
  ACL: PACL;
  SID: PSID;
  SecurityAttributes: PSecurityAttributes;
  SecurityDescriptor: PSecurityDescriptor;
begin
  UserInfo := nil;
  ACL := nil;
  SID := nil;
  SecurityDescriptor := nil;
  SecurityAttributes := nil;
  try
    SecurityGetAllUsers(UserInfo, SID, ACL, SecurityDescriptor, SecurityAttributes);

    FOptex.Enter;
    try
      FAllMapping := TJclSwapFileMapping.Create(FUniqueAppID + JclAIAllMapping,
        PAGE_READWRITE, SizeOf(TJclAISharedData), SecurityAttributes);
      FAllMappingView := FAllMapping.Views[FAllMapping.Add(FILE_MAP_ALL_ACCESS, SizeOf(TJclAISharedData), 0)];
      if not FAllMapping.Existed then
        FillChar(FAllMappingView.Memory^, SizeOf(TJclAISharedData), #0);
    finally
      FOptex.Leave;
    end;
  finally
    SecurityFree(UserInfo, SID, ACL, SecurityDescriptor, SecurityAttributes);
  end;
end;

procedure TJclAppInstances.InitSessionData;
var
  UserInfo: PTokenUser;
  ACL: PACL;
  SID: PSID;
  SecurityAttributes: PSecurityAttributes;
  SecurityDescriptor: PSecurityDescriptor;
  SessionID: DWORD;
begin
  UserInfo := nil;
  ACL := nil;
  SID := nil;
  SecurityDescriptor := nil;
  SecurityAttributes := nil;
  try
    SecurityGetAllUsers(UserInfo, SID, ACL, SecurityDescriptor, SecurityAttributes);

    SessionID := 0;
    ProcessIdToSessionId(GetCurrentProcessId, SessionID); // RESULT
    FOptex.Enter;
    try
      FSessionMapping := TJclSwapFileMapping.Create(FUniqueAppID + JclAISessionMapping + IntToStr(SessionID),
        PAGE_READWRITE, SizeOf(TJclAISharedData), SecurityAttributes);
      FSessionMappingView := FSessionMapping.Views[FSessionMapping.Add(FILE_MAP_ALL_ACCESS, SizeOf(TJclAISharedData), 0)];
      if not FSessionMapping.Existed then
        FillChar(FSessionMappingView.Memory^, SizeOf(TJclAISharedData), #0);
    finally
      FOptex.Leave;
    end;
  finally
    SecurityFree(UserInfo, SID, ACL, SecurityDescriptor, SecurityAttributes);
  end;
end;

procedure TJclAppInstances.InitUserData;
var
  UserInfo: PTokenUser;
  ACL: PACL;
  SID: PSID;
  SecurityAttributes: PSecurityAttributes;
  SecurityDescriptor: PSecurityDescriptor;
  UserName, GroupName: WideString;
begin
  UserInfo := nil;
  ACL := nil;
  SID := nil;
  SecurityDescriptor := nil;
  SecurityAttributes := nil;
  try
    SecurityGetCurrentUser(UserInfo, SID, ACL, SecurityDescriptor, SecurityAttributes);
    LookupAccountBySid(UserInfo.User.Sid, UserName, GroupName);

    FOptex.Enter;
    try
      FUserMapping := TJclSwapFileMapping.Create(FUniqueAppID + JclAIUserMapping + UserName + '_' + GroupName,
        PAGE_READWRITE, SizeOf(TJclAISharedData), SecurityAttributes);
      FUserMappingView := FUserMapping.Views[FUserMapping.Add(FILE_MAP_ALL_ACCESS, SizeOf(TJclAISharedData), 0)];
      if not FUserMapping.Existed then
        FillChar(FUserMappingView.Memory^, SizeOf(TJclAISharedData), #0);
    finally
      FOptex.Leave;
    end;
  finally
    SecurityFree(UserInfo, SID, ACL, SecurityDescriptor, SecurityAttributes);
  end;
end;

class procedure TJclAppInstances.KillInstance;
begin
  Halt(0);
end;

function EnumNotifyWinProc(Wnd: THandle; Message: PMessage): BOOL; stdcall;
begin
  SendNotifyMessage(Wnd, Message^.Msg, Message^.WParam, Message^.LParam);
  Result := True;
end;

procedure TJclAppInstances.NotifyInstances(const W, L: Integer);
var
  I: Integer;
  Wnd: THandle;
  TID: DWORD;
  Msg: TMessage;
  SharedData: PJclAISharedData;
begin
  FOptex.Enter;
  try
    SharedData := PJclAISharedData(FAllMappingView.Memory);
    for I := 0 to SharedData^.Count - 1 do
    begin
      Wnd := GetApplicationWnd(SharedData^.ProcessIDs[I]);
      TID := GetWindowThreadProcessId(Wnd, nil);
      while Wnd <> 0 do
      begin // Send message to TApplication queue
        if PostThreadMessage(TID, FMessageID, W, L) or
          (GetLastError = ERROR_INVALID_THREAD_ID) then
          Break;
        Sleep(1);
      end;
      Msg.Msg := FMessageID;
      Msg.WParam := W;
      Msg.LParam := L;
      EnumThreadWindows(TID, @EnumNotifyWinProc, LPARAM(@Msg));
    end;
  finally
    FOptex.Leave;
  end;
end;

procedure TJclAppInstances.RemoveInstance(MappingView: TJclFileMappingView);
var
  I: Integer;
  SharedData: PJclAISharedData;
begin
  FOptex.Enter;
  try
    SharedData := PJclAISharedData(MappingView.Memory);
    for I := 0 to SharedData^.Count - 1 do
      if SharedData^.ProcessIDs[I] = FCPID then
      begin
        SharedData^.ProcessIDs[I] := 0;
        Move(SharedData^.ProcessIDs[I + 1], SharedData^.ProcessIDs[I], (SharedData^.Count - I) * SizeOf(DWORD));
        Dec(SharedData^.Count);
        Break;
      end;
  finally
    FOptex.Leave;
  end;
end;

procedure TJclAppInstances.SecurityFree(UserInfo: PTokenUser; SID: PSID; ACL: PACL;
  SecurityDescriptor: PSecurityDescriptor; SecurityAttributes: PSecurityAttributes);
begin
  if Assigned(UserInfo) then
    FreeMem(UserInfo);
  if Assigned(SID) then
    FreeSID(SID);
  if Assigned(ACL) then
    FreeMem(ACL);
  if Assigned(SecurityDescriptor) then
    FreeMem(SecurityDescriptor);
  if Assigned(SecurityAttributes) then
    FreeMem(SecurityAttributes);
end;

procedure TJclAppInstances.SecurityGetAllUsers(out UserInfo: PTokenUser; out SID: PSID; out ACL: PACL;
  out SecurityDescriptor: PSecurityDescriptor; out SecurityAttributes: PSecurityAttributes);
var
  WorldAuth: {$IFDEF HAS_UNITSCOPE}WinApi.{$ENDIF HAS_UNITSCOPE}Windows.SID_IDENTIFIER_AUTHORITY;
begin
  UserInfo := nil;
  ACL := nil;
  SID := nil;
  SecurityDescriptor := nil;
  SecurityAttributes := nil;

  SecurityGetCurrentUserInfo(UserInfo);

    // Retrieve the SID of the Everyone group.
  WorldAuth := JclWin32.SECURITY_WORLD_SID_AUTHORITY;
  AllocateAndInitializeSid(WorldAuth, 1, SECURITY_WORLD_RID, 0, 0, 0, 0, 0, 0, 0, SID); // RESULT

  SecurityGetSecurityAttributes(UserInfo^.User.Sid, SID, ACL, SecurityDescriptor, SecurityAttributes);
end;

procedure TJclAppInstances.SecurityGetCurrentUser(out UserInfo: PTokenUser; out SID: PSID; out ACL: PACL; 
  out SecurityDescriptor: PSecurityDescriptor; out SecurityAttributes: PSecurityAttributes);
begin
  UserInfo := nil;
  ACL := nil;
  SID := nil;
  SecurityDescriptor := nil;
  SecurityAttributes := nil;
  SecurityGetCurrentUserInfo(UserInfo);
  SecurityGetSecurityAttributes(UserInfo^.User.Sid, UserInfo.User.Sid, ACL, SecurityDescriptor, SecurityAttributes);
end;

procedure TJclAppInstances.SecurityGetCurrentUserInfo(out UserInfo: PTokenUser);
var
  ProcessToken: THandle;
  TokenInfoSize: DWORD;
  HaveToken: Boolean;
begin
  UserInfo := nil;
  ProcessToken := 0;
  try
    HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, ProcessToken);
    if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
      HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, ProcessToken);
    if not HaveToken then
      RaiseLastOSError;

    if GetTokenInformation(ProcessToken, TokenUser, nil, 0, TokenInfoSize) or
     (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
       RaiseLastOSError;
    UserInfo := PTokenUser(AllocMem(TokenInfoSize));
    Win32Check(GetTokenInformation(ProcessToken, TokenUser, UserInfo, TokenInfoSize, TokenInfoSize));
  finally
    if ProcessToken <> 0 then
      CloseHandle(ProcessToken);
  end;
end;

procedure TJclAppInstances.SecurityGetSecurityAttributes(OwnerSID, AccessSID: PSID; out ACL: PACL;
  out SecurityDescriptor: PSecurityDescriptor; out SecurityAttributes: PSecurityAttributes);
var
  ACLSize: SizeInt;
begin
  // create the ACL
  ACLSize := SizeOf(TACL) + SizeOf(ACCESS_ALLOWED_ACE) + SizeOf(DWORD) + GetLengthSid(AccessSID);
  ACL := AllocMem(ACLSize);
  Win32Check(InitializeAcl(ACL^, ACLSize, ACL_REVISION));
  Win32Check(AddAccessAllowedAce(ACL{$IFDEF BORLAND}^{$ENDIF}, ACL_REVISION, FILE_MAP_ALL_ACCESS, AccessSID));
  Assert(IsValidAcl(ACL{$IFNDEF RTL230_UP}^{$ENDIF})); // QC #102231

  // create the security descriptor
  SecurityDescriptor := AllocMem(SECURITY_DESCRIPTOR_MIN_LENGTH);
  Win32Check(InitializeSecurityDescriptor(SecurityDescriptor, SECURITY_DESCRIPTOR_REVISION));
  Win32Check(SetSecurityDescriptorSacl(SecurityDescriptor, False, nil, True));
  Win32Check(SetSecurityDescriptorOwner(SecurityDescriptor, OwnerSID, False));
  Win32Check(SetSecurityDescriptorGroup(SecurityDescriptor, OwnerSID, False));
  Win32Check(SetSecurityDescriptorDacl(SecurityDescriptor, True, ACL, False));
  Assert(IsValidSecurityDescriptor(SecurityDescriptor));

  // create the security attributes
  SecurityAttributes := AllocMem(SizeOf(SecurityAttributes^));
  SecurityAttributes^.nLength := SizeOf(SecurityAttributes^);
  SecurityAttributes^.lpSecurityDescriptor := SecurityDescriptor;
  SecurityAttributes^.bInheritHandle := False;
end;

function TJclAppInstances.SendCmdLineParams(const WindowClassName: string; const OriginatorWnd: THandle): Boolean;
var
  TempList: TStringList;
  I: Integer;
begin
  TempList := TStringList.Create;
  try
    for I := 1 to ParamCount do
      TempList.Add(ParamStr(I));
    Result := SendStrings(WindowClassName, AppInstCmdLineDataKind, TempList, OriginatorWnd);
  finally
    TempList.Free;
  end;
end;

type
  PEnumWinRec = ^TEnumWinRec;
  TEnumWinRec = record
    WindowClassName: PChar;
    OriginatorWnd: THandle;
    CopyData: TCopyDataStruct;
    Self: TJclAppInstances;
  end;

function EnumWinProc(Wnd: THandle; Data: PEnumWinRec): BOOL; stdcall;
var
  ClassName: array [0..200] of Char;
  I: Integer;
  PID: DWORD;
  Found: Boolean;
  SharedData: PJclAISharedData;
begin
  if (GetClassName(Wnd, ClassName, Length(ClassName) - 1) > 0) and
    (StrComp(ClassName, Data.WindowClassName) = 0) then
  begin
    GetWindowThreadProcessId(Wnd, @PID);
    Found := False;
    Data.Self.FOptex.Enter;
    try
      SharedData := PJclAISharedData(Data.Self.FAllMappingView.Memory);
      for I := 0 to SharedData^.Count - 1 do
        if SharedData^.ProcessIDs[I] = PID then
        begin
          Found := True;
          Break;
        end;
    finally
      Data.Self.FOptex.Leave;
    end;
    if Found then
      SendMessage(Wnd, WM_COPYDATA, Data.OriginatorWnd, LPARAM(@Data.CopyData));
  end;
  Result := True;
end;

function TJclAppInstances.SendData(const WindowClassName: string;
  const DataKind: TJclAppInstDataKind;
  Data: Pointer; const Size: Integer;
  OriginatorWnd: THandle): Boolean;
var
  EnumWinRec: TEnumWinRec;
begin
  Assert(DataKind <> AppInstDataKindNoData);
  EnumWinRec.WindowClassName := PChar(WindowClassName);
  EnumWinRec.OriginatorWnd := OriginatorWnd;
  EnumWinRec.CopyData.dwData := DataKind;
  EnumWinRec.CopyData.cbData := Size;
  EnumWinRec.CopyData.lpData := Data;
  EnumWinRec.Self := Self;
  Result := EnumWindows(@EnumWinProc, LPARAM(@EnumWinRec));
end;

function TJclAppInstances.SendString(const WindowClassName: string;
  const DataKind: TJclAppInstDataKind; const S: string;
  OriginatorWnd: THandle): Boolean;
begin
  Result := SendData(WindowClassName, DataKind, PChar(S), Length(S) * SizeOf(Char), OriginatorWnd);
end;

function TJclAppInstances.SendStrings(const WindowClassName: string;
  const DataKind: TJclAppInstDataKind; const Strings: TStrings;
  OriginatorWnd: THandle): Boolean;
begin
  Result := SendString(WindowClassName, DataKind, Strings.Text, OriginatorWnd);
end;

function TJclAppInstances.SessionSwitchTo(Index: Integer): Boolean;
begin
  Result := BringAppWindowToFront(SessionAppWnds[Index]);
end;

class function TJclAppInstances.SetForegroundWindow98(const Wnd: THandle): Boolean;
var
  ForeThreadID, NewThreadID: DWORD;
begin
  if GetForegroundWindow <> Wnd then
  begin
    ForeThreadID := GetWindowThreadProcessId(GetForegroundWindow, nil);
    NewThreadID := GetWindowThreadProcessId(Wnd, nil);
    if ForeThreadID <> NewThreadID then
    begin
      AttachThreadInput(ForeThreadID, NewThreadID, True);
      Result := SetForegroundWindow(Wnd);
      AttachThreadInput(ForeThreadID, NewThreadID, False);
      if Result then
        Result := SetForegroundWindow(Wnd);
    end
    else
      Result := SetForegroundWindow(Wnd);
  end
  else
    Result := True;
end;

function TJclAppInstances.SwitchTo(Index: Integer): Boolean;
begin
  Result := BringAppWindowToFront(AppWnds[Index]);
end;

procedure TJclAppInstances.UserNotify(Param: Longint);
begin
  NotifyInstances(AI_USERMSG, Param);
end;

function TJclAppInstances.UserSwitchTo(Index: Integer): Boolean;
begin
  Result := BringAppWindowToFront(UserAppWnds[Index]);
end;

function JclAppInstances: TJclAppInstances;
begin
  if AppInstances = nil then
    AppInstances := TJclAppInstances.Create;
  Result := AppInstances;
end;

function JclAppInstances(const UniqueAppIdGuidStr: string): TJclAppInstances;
begin
  Assert(AppInstances = nil);
  ExplicitUniqueAppId := UniqueAppIdGuidStr;
  Result := JclAppInstances;
end;

// Interprocess communication routines
function ReadMessageCheck(var Message: TMessage; const IgnoredOriginatorWnd: THandle): TJclAppInstDataKind;
begin
  if (Message.Msg = WM_COPYDATA) and (TWMCopyData(Message).From <> IgnoredOriginatorWnd) then
  begin
    Message.Result := 1;
    Result := TJclAppInstDataKind(TWMCopyData(Message).CopyDataStruct^.dwData);
  end
  else
  begin
    Message.Result := 0;
    Result := AppInstDataKindNoData;
  end;
end;

procedure ReadMessageData(const Message: TMessage; var Data: Pointer; var Size: Integer);
begin
  if TWMCopyData(Message).Msg = WM_COPYDATA then
  begin
    Size := TWMCopyData(Message).CopyDataStruct^.cbData;
    GetMem(Data, Size);
    Move(TWMCopyData(Message).CopyDataStruct^.lpData^, Data^, Size);
  end;
end;

procedure ReadMessageString(const Message: TMessage; out S: string);
begin
  if TWMCopyData(Message).Msg = WM_COPYDATA then
    SetString(S, PChar(TWMCopyData(Message).CopyDataStruct^.lpData), TWMCopyData(Message).CopyDataStruct^.cbData div SizeOf(Char));
end;

procedure ReadMessageStrings(const Message: TMessage; const Strings: TStrings);
var
  S: string;
begin
  if TWMCopyData(Message).Msg = WM_COPYDATA then
  begin
    ReadMessageString(Message, S);
    Strings.Text := S;
  end;
end;

function SendData(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const Data: Pointer; const Size: Integer): Boolean;
var
  CopyData: TCopyDataStruct;
begin
  CopyData.dwData := DataKind;
  CopyData.cbData := Size;
  CopyData.lpData := Data;
  Result := Boolean(SendMessage(Wnd, WM_COPYDATA, OriginatorWnd, LPARAM(@CopyData)));
end;

function SendStrings(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const Strings: TStrings): Boolean;
begin
  Result := SendString(Wnd, OriginatorWnd, DataKind, Strings.Text);
end;

function SendCmdLineParams(const Wnd, OriginatorWnd: HWND): Boolean;
var
  TempList: TStringList;
  I: Integer;
begin
  TempList := TStringList.Create;
  try
    for I := 1 to ParamCount do
      TempList.Add(ParamStr(I));
    Result := SendStrings(Wnd, OriginatorWnd, AppInstCmdLineDataKind, TempList);
  finally
    TempList.Free;
  end;
end;

function SendString(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const S: string): Boolean;
begin
  Result := SendData(Wnd, OriginatorWnd, DataKind, PChar(S), Length(S) * SizeOf(Char));
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  FreeAndNil(AppInstances);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
