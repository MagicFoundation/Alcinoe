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
{ The Original Code is JclVersionCtrlSVNImpl.pas                                                   }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet.                          }
{ Portions created by Elahn Ientile are Copyright (C) of Elahn Ientile.                            }
{                                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclVersionCtrlSVNImpl;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils, Winapi.Windows, System.Classes, Vcl.Graphics,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils, Windows, Classes, Graphics,
  {$ENDIF ~HAS_UNITSCOPE}
  JclVersionControl;

type
  TSvnDirVersion = (sdvNone, sdv10, sdv17);
  TJclVersionControlSVN = class (TJclVersionControlPlugin)
  private
    FSVNStatusCmd: string;
    FTortoiseSVNProc: string;
  protected
    function GetSupportedActionTypes: TJclVersionControlActionTypes; override;
    function GetFileActions(const FileName: TFileName): TJclVersionControlActionTypes; override;
    function GetSandboxActions(const SdBxName: TFileName): TJclVersionControlActionTypes; override;
    function GetEnabled: Boolean; override;
    function GetSVNBaseDir(const FileName: TFileName): string;
    function GetName: string; override;
    function SVNSupportedDirVersion(const FileDir: String): TSvnDirVersion;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetSandboxNames(const FileName: TFileName; SdBxNames: TStrings): Boolean; override;
    function ExecuteAction(const FileName: TFileName;
      const Action: TJclVersionControlActionType): Boolean; override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\vcl';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclVclResources,
  JclFileUtils, JclSysInfo, JclSysUtils, JclRegistry, JclStrings, JclSimpleXml;

const
  JclVersionCtrlRegKeyName = 'SOFTWARE\TortoiseSVN';
  JclVersionCtrlRegValueName = 'ProcPath';
  JclVersionCtrlSVNExeName = 'svn.exe';
  
  JclVersionCtrlSVNAddVerb = 'add';
  JclVersionCtrlSVNBlameVerb = 'blame';
  JclVersionCtrlSVNBranchVerb = 'copy';
  JclVersionCtrlSVNCheckOutVerb = 'checkout';
  JclVersionCtrlSVNCommitVerb = 'commit';
  JclVersionCtrlSVNDiffVerb = 'diff';
  JclVersionCtrlSVNGraphVerb = 'revisiongraph';
  JclVersionCtrlSVNLogVerb = 'log';
  JclVersionCtrlSVNLockVerb = 'lock';
  JclVersionCtrlSVNMergeVerb = 'merge';
  JclVersionCtrlSVNRenameVerb = 'rename';
  JclVersionCtrlSVNRepoBrowserVerb = 'repobrowser';
  JclVersionCtrlSVNRevertVerb = 'revert';
  JclVersionCtrlSVNStatusVerb = 'repostatus';
  JclVersionCtrlSVNTagVerb = 'copy';
  JclVersionCtrlSVNUpdateVerb = 'update';
  JclVersionCtrlSVNUpdateToParam = '/rev';
  JclVersionCtrlSVNUnlockVerb = 'unlock';

//  JclVersionCtrlSVNTortoiseDLL = 'TortoiseSVN.dll';
  JclVersionCtrlSVNDirectory1 = '.svn\';
  JclVersionCtrlSVNDirectory2 = '_svn\';
  JclVersionCtrlSVNEntryFile = 'entries';
  JclVersionCtrlSVNDbFile = 'wc.db';

  JclVersionCtrlSVNStatusElementName = 'status';
  JclVersionCtrlSVNTargetElementName = 'target';
  JclVersionCtrlSVNEntryElementName = 'entry';
  JclVersionCtrlSVNWCStatusElementName = 'wc-status';
  //JclVersionCtrlSVNPropsPropertyName = 'props';
  JclVersionCtrlSVNItemPropertyName = 'item';
  JclVersionCtrlSVNPathPropertyName = 'path';
  JclVersionCtrlSVNUnversionedPropertyValue = 'unversioned';

  JclVersionCtrlSVNDirectories: array [0..1] of string =
   ( JclVersionCtrlSVNDirectory1, JclVersionCtrlSVNDirectory2 );

//=== TJclVersionControlSVN ==================================================

constructor TJclVersionControlSVN.Create;
var
  SaveAcc: TJclRegWOW64Access;
begin
  inherited Create;

  if IsWindows64 then
  begin
    // on 64 bit machines look in the 64bit section of registy for tortoise SVN (64bit) registry stuff
    SaveAcc := RegGetWOW64AccessMode;
    try
      RegSetWOW64AccessMode(ra64Key);
      FTortoiseSVNProc := RegReadStringDef(HKLM, JclVersionCtrlRegKeyName, JclVersionCtrlRegValueName, '');
    finally
      RegSetWOW64AccessMode(SaveAcc);
    end;
    if FTortoiseSVNProc = '' then // when the 64bit Version is not found try to find the 32bit version
      FTortoiseSVNProc := RegReadStringDef(HKLM, JclVersionCtrlRegKeyName, JclVersionCtrlRegValueName, '');
  end
  else
    FTortoiseSVNProc := RegReadStringDef(HKLM, JclVersionCtrlRegKeyName, JclVersionCtrlRegValueName, '');

  FSVNStatusCmd := PathAddSeparator(ExtractFilePath(FTortoiseSVNProc)) + JclVersionCtrlSVNExeName;
  if FileExists(FSVNStatusCmd) then
    FSVNStatusCmd := FSVNStatusCmd + ' status --xml --non-interactive --depth=empty --verbose '
  else
    FSVNStatusCmd := '';
end;

destructor TJclVersionControlSVN.Destroy;
begin
  inherited Destroy;
end;

function TJclVersionControlSVN.ExecuteAction(const FileName: TFileName;
  const Action: TJclVersionControlActionType): Boolean;

  function CallTortoiseSVNProc(const ActionName: string;
    const Param: string = ''): Boolean;
  var
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    CurrentDir, CommandLine: string;
  begin
    ResetMemory(StartupInfo, SizeOf(TStartupInfo));
    ResetMemory(ProcessInfo, SizeOf(TProcessInformation));
    startupInfo.cb := SizeOf(TStartupInfo);
    startupInfo.dwFlags := STARTF_USESHOWWINDOW;
    startupInfo.wShowWindow := SW_SHOW;

    if FileName = '' then
      raise EJclVersionControlError.Create(RsEEmptyFileName);
    if not Enabled then
      raise EJclVersionControlError.Create(RsENoTortoiseSVN);

    if FileName[Length(FileName)] = DirDelimiter then
      CurrentDir := FileName
    else
      CurrentDir := ExtractFilePath(FileName);
    CommandLine := Format('%s /command:%s /path:"%s" %s /notempfile', [FTortoiseSVNProc, ActionName, FileName, Param]);

    Result := CreateProcess(nil, PChar(CommandLine), nil,
      nil, False, 0, nil, PChar(CurrentDir), StartupInfo, ProcessInfo);

    if Result then
    begin
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
  end;

begin
  case Action of
    vcaAdd,
    vcaAddSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNAddVerb);
    vcaBlame :
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNBlameVerb);
    vcaBranch,
    vcaBranchSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNBranchVerb);
    vcaCheckOutSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNCheckOutVerb);
    vcaCommit,
    vcaCommitSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNCommitVerb);
    vcaDiff:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNDiffVerb);
    vcaGraph:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNGraphVerb);
    vcaLog,
    vcaLogSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNLogVerb);
    vcaLock,
    vcaLockSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNLockVerb);
    vcaMerge,
    vcaMergeSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNMergeVerb);
    vcaRename:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNRenameVerb);
    vcaRepoBrowser:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNRepoBrowserVerb);
    vcaRevert,
    vcaRevertSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNRevertVerb);
    vcaStatus,
    vcaStatusSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNStatusVerb);
    vcaTag,
    vcaTagSandBox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNTagVerb);
    vcaUpdate,
    vcaUpdateSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNUpdateVerb);
    vcaUpdateTo,
    vcaUpdateSandboxTo:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNUpdateVerb, JclVersionCtrlSVNUpdateToParam);
    vcaUnlock,
    vcaUnlockSandbox:
      Result := CallTortoiseSVNProc(JclVersionCtrlSVNUnlockVerb);
    else
      Result := inherited ExecuteAction(FileName, Action);
  end;
end;

function TJclVersionControlSVN.GetEnabled: Boolean;
begin
  Result := FTortoiseSVNProc <> '';
end;

function TJclVersionControlSVN.GetFileActions(const FileName: TFileName): TJclVersionControlActionTypes;

  // internal method to get file actions (not complete and not compatible with SVN 1.7 wc
  function GetSVNInternalFileActions(const FileName: TFileName; var Actions: TJclVersionControlActionTypes): Boolean;
  var
    EntryLine: string;
    EntryFileName, UpperCaseFileName, XmlFileNameValue: TFileName;
    Entries: TJclAnsiMappedTextReader;
    IndexDir: Integer;
  begin
    UpperCaseFileName := StrUpper(ExtractFileName(FileName));
    XmlFileNameValue := Format('NAME="%s"', [UpperCaseFileName]);

    for IndexDir := Low(JclVersionCtrlSVNDirectories) to High(JclVersionCtrlSVNDirectories) do
    begin
      Result := False;

      EntryFileName := PathAddSeparator(ExtractFilePath(FileName))
        + JclVersionCtrlSVNDirectories[IndexDir] + JclVersionCtrlSVNEntryFile;

      if FileExists(EntryFileName) then
      begin
        Entries := TJclAnsiMappedTextReader.Create(EntryFileName);
        try
          while not Entries.Eof do
          begin
            EntryLine := string(Entries.ReadLn);
            // old SVN entries file (xml-like)
            if Pos(XmlFileNameValue, StrUpper(EntryLine)) > 0 then
            begin
              Result := True;
              // TODO: check modifications
              Actions := Actions + [vcaBlame, vcaBranch, vcaCommit, vcaDiff, vcaGraph,
                vcaLog, vcaLock, vcaMerge, vcaRename, vcaRevert, vcaRepoBrowser,
                vcaStatus, vcaTag, vcaUpdate, vcaUpdateTo, vcaUnlock];
              FreeAndNil(Entries);
              Exit;
            end;
            // new SVN entries file (flat-style)
            if EntryLine = NativeFormFeed then
            begin
              EntryLine := string(Entries.ReadLn);
              if StrSame(UpperCaseFileName, StrUpper(EntryLine)) then
              begin
                Result := True;
                // TODO: check modifications
                Actions := Actions + [vcaBlame, vcaBranch, vcaCommit, vcaDiff, vcaGraph,
                  vcaLog, vcaLock, vcaMerge, vcaRename, vcaRevert, vcaRepoBrowser,
                  vcaStatus, vcaTag, vcaUpdate, vcaUpdateTo, vcaUnlock];
                FreeAndNil(Entries);
                Exit;
              end;
            end;
          end;
        finally
          Entries.Free;
        end;
        Actions := Actions + [vcaAdd];
      end;
    end;
  end;

  // external method: rely on svn.exe to get file actions
  function GetSVNExecutableFileActions(const FileName: TFileName; var Actions: TJclVersionControlActionTypes): Boolean;
  var
    SVNOutput, SVNError: string;
    XML: TJclSimpleXML;
    TargetElement, EntryElement, WcStatusElement: TJclSimpleXMLElem;
    ItemProp: TJclSimpleXMLProp;
  begin
    Result := False;
    if Execute(FSVNStatusCmd + FileName, SVNOutput, SVNError) <> 0 then
      Exit;

    XML := TJclSimpleXML.Create;
    try
      XML.LoadFromString(SVNOutput);
      XML.Options := XML.Options - [sxoAutoCreate];
      if XML.Root.Name <> JclVersionCtrlSVNStatusElementName then
        Exit;

      TargetElement := XML.Root.Items.ItemNamed[JclVersionCtrlSVNTargetElementName];
      if not Assigned(TargetElement) then
        Exit;

      EntryElement := TargetElement.Items.ItemNamed[JclVersionCtrlSVNEntryElementName];
      if not Assigned(EntryElement) then
        Exit;

      WcStatusElement := EntryElement.Items.ItemNamed[JclVersionCtrlSVNWcStatusElementName];
      if not Assigned(WcStatusElement) then
        Exit;

      ItemProp := WcStatusElement.Properties.ItemNamed[JclVersionCtrlSVNItemPropertyName];
      if not Assigned(ItemProp) then
        Exit;

      Result := True;

      if ItemProp.Value = JclVersionCtrlSVNUnversionedPropertyValue then
        Actions := Actions + [vcaAdd, vcaRepoBrowser, vcaStatus]
      else
        // TODO: check modifications
        Actions := Actions + [vcaBlame, vcaBranch, vcaCommit, vcaDiff, vcaGraph,
          vcaLog, vcaLock, vcaMerge, vcaRename, vcaRevert, vcaRepoBrowser,
          vcaStatus, vcaTag, vcaUpdate, vcaUpdateTo, vcaUnlock];
    finally
      XML.Free;
    end;
  end;

var
  Found: Boolean;
  SupportedDirVersion: TSvnDirVersion;
begin
  Result := inherited GetFileActions(FileName);

  Found := not Enabled;

  // SVN 1.7 repos: invoke SVN to query the working copy
  if (not Found) and (FSVNStatusCmd <> '') then
    Found := GetSVNExecutableFileActions(FileName, Result);

  // SVN 1.6 repos: direct queries on the working copy
  if not Found then
  begin
    SupportedDirVersion := SVNSupportedDirVersion(ExtractFilePath(FileName));
    if SupportedDirVersion = sdv17 then
    begin
      Result := GetSupportedActionTypes;
      Exit;
    end
    else if SupportedDirVersion = sdvNone then
      Exit;

    //Found := GetSVNInternalFileActions(FileName, Result);
    GetSVNInternalFileActions(FileName, Result);
  end;
end;

function TJclVersionControlSVN.GetSVNBaseDir(const FileName: TFileName): string;
var
  DirectoryName: String;
  IndexFileName: Integer;
  IndexDir: Integer;
begin
  Result := '';
  if Enabled then
    for IndexFileName := Length(FileName) downto 1 do
      if FileName[IndexFileName] = DirDelimiter then
      begin
        DirectoryName := Copy(FileName, 1, IndexFileName);
        for IndexDir := Low(JclVersionCtrlSVNDirectories) to High(JclVersionCtrlSVNDirectories) do
          if FileExists(DirectoryName + JclVersionCtrlSVNDirectories[IndexDir] + JclVersionCtrlSVNEntryFile) then
          begin
            Result := DirectoryName;
            Exit;
          end;
        end;
end;

function TJclVersionControlSVN.GetSupportedActionTypes: TJclVersionControlActionTypes;
begin
  Result := inherited GetSupportedActionTypes;
  if Enabled then
    Result := Result + [vcaAdd, vcaAddSandbox, vcaBlame, vcaBranch,
    vcaBranchSandbox, vcaCheckOutSandbox, vcaCommit, vcaCommitSandbox, vcaDiff,
    vcaGraph, vcaLog, vcaLogSandbox, vcaLock, vcaLockSandbox, vcaMerge,
    vcaMergeSandbox, vcaRename, vcaRepoBrowser, vcaRevert, vcaRevertSandbox,
    vcaStatus, vcaStatusSandbox, vcaTag, vcaTagSandBox, vcaUpdate,
    vcaUpdateSandbox, vcaUpdateTo, vcaUpdateSandboxTo, vcaUnlock, vcaUnlockSandbox];
end;

function TJclVersionControlSVN.GetName: string;
begin
  Result := LoadResString(@RsVersionCtrlSVNName);
end;

function TJclVersionControlSVN.GetSandboxActions(
  const SdBxName: TFileName): TJclVersionControlActionTypes;

  function GetSVNInternalSandboxActions(const SdBxName: TFileName; out Actions: TJclVersionControlActionTypes): Boolean;
  var
    SvnDirectory: string;
    IndexDir: Integer;
  begin
    Result := False;
    // not in a sandbox
    Actions := Actions + [vcaCheckOutSandbox];

    for IndexDir := Low(JclVersionCtrlSVNDirectories) to High(JclVersionCtrlSVNDirectories) do
    begin
      SvnDirectory := sdBxName + JclVersionCtrlSVNDirectories[IndexDir];

      if DirectoryExists(SvnDirectory) then
      begin
        Result := True;
        Actions := Actions + [vcaAddSandbox, vcaBranchSandbox, vcaCommitSandbox,
          vcaLogSandbox, vcaLockSandbox, vcaMergeSandbox, vcaRevertSandbox,
          vcaStatusSandbox, vcaTagSandBox, vcaUpdateSandbox, vcaUpdateSandboxTo,
          vcaUnlockSandbox] - [vcaCheckOutSandbox];
        Exit;
      end;
    end;
  end;

  function GetSVNExecutableSandboxActions(const SdBxName: TFileName; out Actions: TJclVersionControlActionTypes): Boolean;
  var
    SVNOutput, SVNError: string;
    XML: TJclSimpleXML;
    TargetElement, EntryElement, WcStatusElement: TJclSimpleXMLElem;
    ItemProp: TJclSimpleXMLProp;
  begin
    Result := False;
    // not in a sandbox
    Actions := Actions + [vcaCheckOutSandbox];

    if Execute(FSVNStatusCmd + SdBxName, SVNOutput, SVNError) <> 0 then
      Exit;

    XML := TJclSimpleXML.Create;
    try
      XML.LoadFromString(SVNOutput);
      XML.Options := XML.Options - [sxoAutoCreate];
      if XML.Root.Name <> JclVersionCtrlSVNStatusElementName then
        Exit;

      TargetElement := XML.Root.Items.ItemNamed[JclVersionCtrlSVNTargetElementName];
      if not Assigned(TargetElement) then
        Exit;

      EntryElement := TargetElement.Items.ItemNamed[JclVersionCtrlSVNEntryElementName];
      if not Assigned(EntryElement) then
        Exit;

      WcStatusElement := EntryElement.Items.ItemNamed[JclVersionCtrlSVNWcStatusElementName];
      if not Assigned(WcStatusElement) then
        Exit;

      ItemProp := WcStatusElement.Properties.ItemNamed[JclVersionCtrlSVNItemPropertyName];
      if not Assigned(ItemProp) then
        Exit;

      if ItemProp.Value <> JclVersionCtrlSVNUnversionedPropertyValue then
      begin
        Result := True;
        Actions := Actions + [vcaAddSandbox, vcaBranchSandbox, vcaCommitSandbox,
          vcaLogSandbox, vcaLockSandbox, vcaMergeSandbox, vcaRevertSandbox,
          vcaStatusSandbox, vcaTagSandBox, vcaUpdateSandbox, vcaUpdateSandboxTo,
          vcaUnlockSandbox] - [vcaCheckOutSandbox];
      end;
    finally
      XML.Free;
    end;
  end;

var
  Found: Boolean;
begin
  Result := inherited GetSandboxActions(SdBxName);

  Found := not Enabled;
  if (not Found) and (FSVNStatusCmd <> '') then
    Found := GetSVNExecutableSandboxActions(SdBxName, Result);
  if not Found then
    // Found := GetSVNInternalSandboxActions(SdBxName, Result);
    GetSVNInternalSandboxActions(SdBxName, Result);
end;

function TJclVersionControlSVN.GetSandboxNames(const FileName: TFileName;
  SdBxNames: TStrings): Boolean;

  // internal method, not complete, not compatible with SVN 1.7
  function GetSVNInternalSandboxNames(const FileName: TFileName; SdBxNames: TStrings): Boolean;
  var
    DirectoryName: string;
    IndexDir, IndexFileName: Integer;
  begin
    SdBxNames.BeginUpdate;
    try
      SdBxNames.Clear;

      for IndexFileName := Length(FileName) downto 1 do
        if FileName[IndexFileName] = DirDelimiter then
      begin
        DirectoryName := Copy(FileName, 1, IndexFileName);
        for IndexDir := Low(JclVersionCtrlSVNDirectories) to High(JclVersionCtrlSVNDirectories) do
        begin
          if DirectoryExists(DirectoryName + JclVersionCtrlSVNDirectories[IndexDir]) then
            SdBxNames.Add(DirectoryName);
        end;
      end;
    finally
      SdBxNames.EndUpdate;
    end;

    Result := SdBxNames.Count > 0;
  end;

  function GetSVNExecutableSandboxNames(const FileName: TFileName; SdBxNames: TStrings): Boolean;
  var
    ParentDirectories: TStrings;
    Index: Integer;
    SVNOutput, SVNError: string;
    XML: TJclSimpleXML;
    TargetElements: TJclSimpleXMLNamedElems;
    TargetElement, EntryElement, WcStatusElement: TJclSimpleXMLElem;
    PathProp, ItemProp: TJclSimpleXMLProp;
  begin
    Result := False;

    ParentDirectories := TStringList.Create;
    try
      for Index := Length(FileName) downto 1 do
        if FileName[Index] = DirDelimiter then
          ParentDirectories.Add(Copy('"' + FileName, 1, Index) + '"');

      if Execute(FSVNStatusCmd + StringsToStr(ParentDirectories, NativeSpace, False), SVNOutput, SVNError) <> 0 then
        Exit;
    finally
      ParentDirectories.Free;
    end;

    XML := TJclSimpleXML.Create;
    try
      XML.LoadFromString(SVNOutput);
      XML.Options := XML.Options - [sxoAutoCreate];
      if XML.Root.Name <> JclVersionCtrlSVNStatusElementName then
        Exit;

      TargetElements := XML.Root.Items.NamedElems[JclVersionCtrlSVNTargetElementName];

      SdBxNames.BeginUpdate;
      try
        SdBxNames.Clear;

        for Index := 0 to TargetElements.Count - 1 do
        begin
          TargetElement := TargetElements.Item[Index];
          if not Assigned(TargetElement) then
            Continue;

          EntryElement := TargetElement.Items.ItemNamed[JclVersionCtrlSVNEntryElementName];
          if not Assigned(EntryElement) then
            Continue;

          PathProp := EntryElement.Properties.ItemNamed[JclVersionCtrlSVNPathPropertyName];
          if not Assigned(PathProp) then
            Continue;

          WcStatusElement := EntryElement.Items.ItemNamed[JclVersionCtrlSVNWcStatusElementName];
          if not Assigned(WcStatusElement) then
            Continue;

          ItemProp := WcStatusElement.Properties.ItemNamed[JclVersionCtrlSVNItemPropertyName];
          if not Assigned(ItemProp) then
            Continue;

          if ItemProp.Value <> JclVersionCtrlSVNUnversionedPropertyValue then
            SdBxNames.Add(PathProp.Value);
        end;
      finally
        SdBxNames.EndUpdate;
      end;
    finally
      XML.Free;
    end;

    Result := SdBxNames.Count > 0;
  end;

begin
  Result := not Enabled;

  if (not Result) and (FSVNStatusCmd <> '') then
    Result := GetSVNExecutableSandboxNames(FileName, SdBxNames);

  if not Result then
    Result := GetSVNInternalSandboxNames(FileName, SdBxNames);

  if SdBxNames.Count = 0 then
    Result := inherited GetSandboxNames(FileName, SdBxNames);
end;

function TJclVersionControlSVN.SVNSupportedDirVersion(const FileDir: String): TSvnDirVersion;
var
  BaseDir: String;
  //IgnoreList: TStringList;
  //IgnoreDir : String;
  //i: Integer;
  IndexDir: Integer;
begin
  Result := sdvNone;
  if Enabled then
  begin
    BaseDir := GetSVNBaseDir(FileDir);
    if (BaseDir <> '') then
    begin
      Result := sdv10;
      for IndexDir := Low(JclVersionCtrlSVNDirectories) to High(JclVersionCtrlSVNDirectories) do
        if FileExists(BaseDir + JclVersionCtrlSVNDirectories[IndexDir] + JclVersionCtrlSVNDbFile) then
        begin
          Result := sdv17;
          exit;
        end
    end;
  end;
end;

initialization

  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  RegisterVersionControlPluginClass(TJclVersionControlSVN);

finalization

  UnregisterVersionControlPluginClass(TJclVersionControlSVN);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
