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
{ The Original Code is DelphiInstall.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Andreas Hausladen (ahuser)                                                                     }
{   Florent Ouchet (outchy)                                                                        }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair) - crossplatform & BCB support                                      }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclHelpUtils;

{$I jcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  MSHelpServices_TLB,
  {$IFDEF HAS_UNITSCOPE}
  System.Classes, System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  Classes, SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase, JclSysUtils;

// Various definitions
type
  EJclHelpUtilsException = class(EJclError);

type
  TJclBorlandOpenHelp = class
  private
    FHelpPrefix: string;
    FRootDirectory: string;
    function GetContentFileName: string;
    function GetIndexFileName: string;
    function GetLinkFileName: string;
    function GetGidFileName: string;
    function GetProjectFileName: string;
    function ReadFileName(const FormatName: string): string;
  public
    constructor Create(const ARootDirectory, AHelpPrefix: string);
    function AddHelpFile(const HelpFileName, IndexName: string): Boolean;
    function RemoveHelpFile(const HelpFileName, IndexName: string): Boolean;
    property ContentFileName: string read GetContentFileName;
    property GidFileName: string read GetGidFileName;
    property HelpPrefix: string read FHelpPrefix;
    property IndexFileName: string read GetIndexFileName;
    property LinkFileName: string read GetLinkFileName;
    property ProjectFileName: string read GetProjectFileName;
    property RootDirectory: string read FRootDirectory;
  end;

  TJclHelp2Object = (hoRegisterSession, hoRegister, hoPlugin);
  TJclHelp2Objects = set of TJclHelp2Object;

  TJclHelp2Manager = class
  private
    FHxRegisterSession: IHxRegisterSession;
    FHxRegister: IHxRegister;
    FHxPlugin: IHxPlugIn;
    FIdeNameSpace: WideString;
    function RequireObject(HelpObjects: TJclHelp2Objects): Boolean;
    function GetHxPlugin: IHxPlugin;
    function GetHxRegister: IHxRegister;
    function GetHxRegisterSession: IHxRegisterSession;
  public
    constructor Create(IDEVersionNumber: Integer);
    destructor Destroy; override;
    function CreateTransaction: Boolean;
    function CommitTransaction: Boolean;
    function RegisterNameSpace(const Name, Collection, Description: WideString): Boolean;
    function UnregisterNameSpace(const Name: WideString): Boolean;
    function RegisterHelpFile(const NameSpace, Identifier: WideString;
      const LangId: Integer; const HxSFile, HxIFile: WideString): Boolean;
    function UnregisterHelpFile(const NameSpace, Identifier: WideString;
      const LangId: Integer): Boolean;
    function PlugNameSpaceIn(const SourceNameSpace,
      TargetNameSpace: WideString): Boolean;
    function UnPlugNameSpace(const SourceNameSpace,
      TargetNameSpace: WideString): Boolean;
    function PlugNameSpaceInBorlandHelp(const NameSpace: WideString): Boolean;
    function UnPlugNameSpaceFromBorlandHelp(const NameSpace: WideString): Boolean;
    property HxRegisterSession: IHxRegisterSession read GetHxRegisterSession;
    property HxRegister: IHxRegister read GetHxRegister;
    property HxPlugin: IHxPlugin read GetHxPlugin;
    property IdeNamespace: WideString read FIdeNameSpace;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'jcl\source\windows';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  Winapi.Windows,
  {$ELSE ~HAS_UNITSCOPE}
  Windows,
  {$ENDIF ~HAS_UNITSCOPE}
  JclRegistry,
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  JclFileUtils, JclDevToolsResources;

type
  TBDSVersionInfo = record
    Name: PResStringRec;
    VersionStr: string;
    Version: Integer;
    CoreIdeVersion: string;
    Supported: Boolean;
  end;

const
  MSHelpSystemKeyName = '\SOFTWARE\Microsoft\Windows\Help';

  HelpContentFileName        = '%s\Help\%s.ohc';
  HelpIndexFileName          = '%s\Help\%s.ohi';
  HelpLinkFileName           = '%s\Help\%s.ohl';
  HelpProjectFileName        = '%s\Help\%s.ohp';
  HelpGidFileName            = '%s\Help\%s.gid';

//=== { TJclBorlandOpenHelp } ================================================

function TJclBorlandOpenHelp.AddHelpFile(const HelpFileName, IndexName: string): Boolean;
var
  CntFileName, HelpName, CntName: string;
  List: TStringList;

  procedure AddToList(const FileName, Text: string);
  var
    I, Attr: Integer;
    Found: Boolean;
  begin
    List.LoadFromFile(FileName);
    Found := False;
    for I := 0 to List.Count - 1 do
      if AnsiSameText(Trim(List[I]), Text) then
      begin
        Found := True;
        Break;
      end;
    if not Found then
    begin
      List.Add(Text);
      Attr := FileGetAttr(FileName);
      FileSetAttr(FileName, faArchive);
      List.SaveToFile(FileName);
      FileSetAttr(FileName, Attr);
    end;
  end;

begin
  CntFileName := ChangeFileExt(HelpFileName, '.cnt');
  Result := FileExists(HelpFileName) and FileExists(CntFileName);
  if Result then
  begin
    HelpName := ExtractFileName(HelpFileName);
    CntName := ExtractFileName(CntFileName);
    RegWriteString(HKLM, MSHelpSystemKeyName, HelpName, ExtractFilePath(HelpFileName));
    RegWriteString(HKLM, MSHelpSystemKeyName, CntName, ExtractFilePath(CntFileName));
    List := TStringList.Create;
    try
      AddToList(ContentFileName, Format(':Include %s', [CntName]));
      AddToList(LinkFileName, Format(':Link %s', [HelpName]));
      AddToList(IndexFileName, Format(':Index %s=%s', [IndexName, HelpName]));
      SetFileLastWrite(ProjectFileName, Now);
      FileDelete(GidFileName);
    finally
      List.Free;
    end;
  end;
end;

constructor TJclBorlandOpenHelp.Create(const ARootDirectory,
  AHelpPrefix: string);
begin
  inherited Create;
  FHelpPrefix := AHelpPrefix;
  FRootDirectory := ARootDirectory;
end;

function TJclBorlandOpenHelp.GetContentFileName: string;
begin
  Result := ReadFileName(HelpContentFileName);
end;

function TJclBorlandOpenHelp.GetGidFileName: string;
begin
  Result := ReadFileName(HelpGidFileName);
end;

function TJclBorlandOpenHelp.GetIndexFileName: string;
begin
  Result := ReadFileName(HelpIndexFileName);
end;

function TJclBorlandOpenHelp.GetLinkFileName: string;
begin
  Result := ReadFileName(HelpLinkFileName);
end;

function TJclBorlandOpenHelp.GetProjectFileName: string;
begin
  Result := ReadFileName(HelpProjectFileName);
end;

function TJclBorlandOpenHelp.ReadFileName(const FormatName: string): string;
begin
  if HelpPrefix <> '' then
    Result := Format(FormatName, [RootDirectory, HelpPrefix])
  else
    raise EJclHelpUtilsException.CreateRes(@RsENoOpenHelp);
end;

function TJclBorlandOpenHelp.RemoveHelpFile(const HelpFileName, IndexName: string): Boolean;
var
  CntFileName, HelpName, CntName: string;
  List: TStringList;

  procedure RemoveFromList(const FileName, Text: string);
  var
    I, Attr: Integer;
    Found: Boolean;
  begin
    List.LoadFromFile(FileName);
    Found := False;
    for I := 0 to List.Count - 1 do
      if AnsiSameText(Trim(List[I]), Text) then
      begin
        Found := True;
        List.Delete(I);
        Break;
      end;
    if Found then
    begin
      Attr := FileGetAttr(FileName);
      FileSetAttr(FileName, faArchive);
      List.SaveToFile(FileName);
      FileSetAttr(FileName, Attr);
    end;
  end;

begin
  CntFileName := ChangeFileExt(HelpFileName, '.cnt');
  Result := FileExists(HelpFileName) and FileExists(CntFileName);
  if Result then
  begin
    HelpName := ExtractFileName(HelpFileName);
    CntName := ExtractFileName(CntFileName);
    //RegDeleteEntry(HKEY_LOCAL_MACHINE, MSHelpSystemKeyName, HelpName);
    //RegDeleteEntry(HKEY_LOCAL_MACHINE, MSHelpSystemKeyName, CntName);
    List := TStringList.Create;
    try
      RemoveFromList(ContentFileName, Format(':Include %s', [CntName]));
      RemoveFromList(LinkFileName, Format(':Link %s', [HelpName]));
      RemoveFromList(IndexFileName, Format(':Index %s=%s', [IndexName, HelpName]));
      SetFileLastWrite(ProjectFileName, Now);
      FileDelete(GidFileName);
    finally
      List.Free;
    end;
  end;
end;

//== { TJclHelp2Manager } ====================================================

const
  Help2BorlandNameSpace = 'Borland.BDS%d';
  Help2DefaultKeyWord   = '_DEFAULT';

constructor TJclHelp2Manager.Create(IDEVersionNumber: Integer);
begin
  inherited Create;
  FHxRegisterSession := nil;
  FHxRegister := nil;
  FHxPlugin := nil;
  if IDEVersionNumber > 0 then
  begin
    if (IDEVersionNumber = 12) then
      FIdeNameSpace := 'embarcadero.rs_xe5'
    else
    if (IDEVersionNumber = 11) then
      FIdeNameSpace := 'embarcadero.rs_xe4'
    else
    if (IDEVersionNumber = 10) then
      FIdeNameSpace := 'embarcadero.rs_xe3'
    else
    if (IDEVersionNumber = 9) then
      FIdeNameSpace := 'embarcadero.rs_xe2'
    else
    if (IDEVersionNumber = 8) then
      FIdeNameSpace := 'embarcadero.rs_xe'
    else
    if (IDEVersionNumber = 7) then
      FIdeNameSpace := 'embarcadero.rs2010'
    else
    if (IDEVersionNumber = 6) then
      FIdeNameSpace := 'embarcadero.rs2009'
    else
      FIdeNameSpace := Format(Help2BorlandNameSpace, [IDEVersionNumber]);
  end
  else
    FIdeNameSpace := '';
end;

destructor TJclHelp2Manager.Destroy;
begin
  FHxRegisterSession := nil;
  FHxRegister := nil;
  FHxPlugin := nil;
  inherited Destroy;
end;

function TJclHelp2Manager.CommitTransaction: Boolean;
begin
  Result := RequireObject([hoRegisterSession]);
  if Result then
  begin
    try
      FHxRegisterSession.CommitTransaction;
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.CreateTransaction: Boolean;
begin
  Result := RequireObject([hoRegisterSession]);
  if Result then
  begin
    try
      FHxRegisterSession.CreateTransaction('');
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.GetHxPlugin: IHxPlugin;
begin
  RequireObject([hoPlugin]);
  Result := FHxPlugin;
end;

function TJclHelp2Manager.GetHxRegister: IHxRegister;
begin
  RequireObject([hoRegister]);
  Result := FHxRegister;
end;

function TJclHelp2Manager.GetHxRegisterSession: IHxRegisterSession;
begin
  RequireObject([hoRegisterSession]);
  Result := FHxRegisterSession;
end;

function TJclHelp2Manager.PlugNameSpaceIn(const SourceNameSpace, TargetNameSpace: WideString): Boolean;
var
  Help2Default: WideString;
begin
  Result := RequireObject([hoPlugin]);
  if Result then
  begin
    try
      Help2Default := Help2DefaultKeyWord;
      FHxPlugin.RegisterHelpPlugIn(TargetNameSpace, Help2Default,
        SourceNameSpace, Help2Default, '', 0);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.PlugNameSpaceInBorlandHelp(
  const NameSpace: WideString): Boolean;
begin
  Result := (IdeNamespace <> '') and PlugNameSpaceIn(NameSpace, IdeNamespace);
end;

function TJclHelp2Manager.RegisterHelpFile(const NameSpace, Identifier: WideString;
  const LangId: Integer; const HxSFile, HxIFile: WideString): Boolean;
begin
  Result := RequireObject([hoRegister]);
  if Result then
  begin
    try
      FHxRegister.RegisterHelpFileSet(NameSpace, Identifier, LangId, HxSFile,
        HxIFile, '', '', 0, 0, 0, 0);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.RegisterNameSpace(const Name, Collection, Description: WideString): Boolean;
begin
  Result := RequireObject([hoRegister]);
  if Result then
  begin
    try
      FHxRegister.RegisterNamespace(Name, Collection, Description);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.RequireObject(HelpObjects: TJclHelp2Objects): Boolean;
begin
  // dependencies
  if (hoRegister in HelpObjects) or (hoPlugin in HelpObjects) then
    Include(HelpObjects, hoRegisterSession);

  Result := True;

  if (hoRegisterSession in HelpObjects) and not Assigned(FHxRegisterSession) then
  begin
    try
      FHxRegisterSession := CoHxRegisterSession.Create;
    except
      Result := False;
    end;
  end;

  if Result and (hoRegister in HelpObjects) and not Assigned(FHxRegister) then
  begin
    try
      Result := Supports(FHxRegisterSession.GetRegistrationObject(HxRegisterSession_IHxRegister),
        IHxRegister, FHxRegister);
    except
      Result := False;
    end;
  end;

  if Result and (hoPlugin in HelpObjects) and not Assigned(FHxPlugin) then
  begin
    try
      Result := Supports(FHxRegisterSession.GetRegistrationObject(HxRegisterSession_IHxPlugIn),
        IHxPlugin, FHxPlugin);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.UnPlugNameSpace(const SourceNameSpace, TargetNameSpace: WideString): Boolean;
var
  Help2Default: WideString;
begin
  Result := RequireObject([hoPlugin]);
  if Result then
  begin
    try
      Help2Default := Help2DefaultKeyWord;
      FHxPlugin.RemoveHelpPlugIn(TargetNameSpace, Help2Default,
        SourceNameSpace, Help2Default, '');
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.UnPlugNameSpaceFromBorlandHelp(const NameSpace: WideString): Boolean;
begin
  Result := (IdeNamespace <> '') and UnPlugNameSpace(NameSpace, IdeNamespace);
end;

function TJclHelp2Manager.UnregisterHelpFile(const NameSpace, Identifier: WideString;
  const LangId: Integer): Boolean;
begin
  Result := RequireObject([hoRegister]);
  if Result then
  begin
    try
      FHxRegister.RemoveHelpFile(NameSpace, Identifier, LangId);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.UnregisterNameSpace(const Name: WideString): Boolean;
begin
  Result := RequireObject([hoRegister]);
  if Result then
  begin
    try
      FHxRegister.RemoveNamespace(Name);
    except
      Result := False;
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

