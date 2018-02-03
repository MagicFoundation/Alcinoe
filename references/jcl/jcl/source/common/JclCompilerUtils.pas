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

unit JclCompilerUtils;

{$I jcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  System.Classes, System.SysUtils, System.IniFiles,
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils, IniFiles,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase, JclSysUtils;

type
  EJclCompilerUtilsException = class(EJclError);

  TJclCompilerSettingsFormat = (csfDOF, csfBDSProj, csfMsBuild);

  TJclBorlandCommandLineTool = class;
  TJclBorlandCommandLineToolEvent = procedure(Sender:TJclBorlandCommandLineTool) of object;

  TJclBorlandCommandLineTool = class(TInterfacedObject, IJclCommandLineTool)
  private
    FBinDirectory: string;
    FCompilerSettingsFormat: TJclCompilerSettingsFormat;
    FLongPathBug: Boolean;
    FOptions: TStringList;
    FOutputCallback: TTextHandler;
    FOutput: string;
    FOnAfterExecute: TJclBorlandCommandLineToolEvent;
    FOnBeforeExecute: TJclBorlandCommandLineToolEvent;
  protected
    procedure CheckOutputValid;
    function GetFileName: string;
    function InternalExecute(const CommandLine: string): Boolean;
  public
    constructor Create(const ABinDirectory: string; ALongPathBug: Boolean;
      ACompilerSettingsFormat: TJclCompilerSettingsFormat);
    destructor Destroy; override;
    { IJclCommandLineTool }
    function GetExeName: string; virtual;
    function GetOptions: TStrings;
    function GetOutput: string;
    function GetOutputCallback: TTextHandler;
    procedure AddPathOption(const Option, Path: string);
    function Execute(const CommandLine: string): Boolean; virtual;
    procedure SetOutputCallback(const CallbackMethod: TTextHandler);
    property BinDirectory: string read FBinDirectory;
    property CompilerSettingsFormat: TJclCompilerSettingsFormat read FCompilerSettingsFormat;
    property ExeName: string read GetExeName;
    property LongPathBug: Boolean read FLongPathBug;
    property Options: TStrings read GetOptions;
    property OutputCallback: TTextHandler write SetOutputCallback;
    property Output: string read GetOutput;

    property FileName: string read GetFileName;
    property OnAfterExecute: TJclBorlandCommandLineToolEvent read FOnAfterExecute write FOnAfterExecute;
    property OnBeforeExecute: TJclBorlandCommandLineToolEvent read FOnBeforeExecute write FOnBeforeExecute;
  end;

  TJclBCC32 = class(TJclBorlandCommandLineTool)
  public
    class function GetPlatform: string; virtual;
    function GetExeName: string; override;
  end;

  TJclBCC64 = class(TJclBCC32)
  public
    class function GetPlatform: string; override;
    function GetExeName: string; override;
  end;

  TProjectOptions = record
    UsePackages: Boolean;
    UnitOutputDir: string;
    SearchPath: string;
    DynamicPackages: string;
    SearchDcpPath: string;
    Conditionals: string;
    Namespace: string;
  end;

  TJclStringsGetterFunction = function: TStrings of object;

  TJclDCC32 = class(TJclBorlandCommandLineTool)
  private
    FDCPSearchPath: string;
    FLibrarySearchPath: string;
    FLibraryDebugSearchPath: string;
    FCppSearchPath: string;
    FOnEnvironmentVariables: TJclStringsGetterFunction;
    FSupportsNoConfig: Boolean;
    FSupportsPlatform: Boolean;
    FDCCVersion: Single;
  protected
    procedure AddProjectOptions(const ProjectFileName, DCPPath: string);
    function Compile(const ProjectFileName: string): Boolean;
  public
    class function GetPlatform: string; virtual;
    constructor Create(const ABinDirectory: string; ALongPathBug: Boolean; ADCCVersion: Single;
      ACompilerSettingsFormat: TJclCompilerSettingsFormat; ASupportsNoConfig, ASupportsPlatform: Boolean;
      const ADCPSearchPath, ALibrarySearchPath, ALibraryDebugSearchPath, ACppSearchPath: string);
    function GetExeName: string; override;
    function Execute(const CommandLine: string): Boolean; override;
    function MakePackage(const PackageName, BPLPath, DCPPath: string;
      ExtraOptions: string = ''; ADebug: Boolean = False): Boolean;
    function MakeProject(const ProjectName, OutputDir, DcpSearchPath: string;
      ExtraOptions: string = ''; ADebug: Boolean = False): Boolean;
    procedure SetDefaultOptions(ADebug: Boolean); virtual;
    function AddBDSProjOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
    function AddDOFOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
    function AddDProjOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
    property CppSearchPath: string read FCppSearchPath;
    property DCPSearchPath: string read FDCPSearchPath;
    property LibrarySearchPath: string read FLibrarySearchPath;
    property LibraryDebugSearchPath: string read FLibraryDebugSearchPath;
    property OnEnvironmentVariables: TJclStringsGetterFunction read FOnEnvironmentVariables write FOnEnvironmentVariables;
    property SupportsNoConfig: Boolean read FSupportsNoConfig;
    property SupportsPlatform: Boolean read FSupportsPlatform;
    property DCCVersion: Single read FDCCVersion;
  end;

  TJclDCC64 = class(TJclDCC32)
  public
    class function GetPlatform: string; override;
    function GetExeName: string; override;
  end;

  TJclDCCOSX32 = class(TJclDCC32)
  public
    class function GetPlatform: string; override;
    function GetExeName: string; override;
  end;

  {$IFDEF MSWINDOWS}
  TJclDCCIL = class(TJclDCC32)
  private
    FMaxCLRVersion: string;
  protected
    function GetMaxCLRVersion: string;
  public
    function GetExeName: string; override;
    function MakeProject(const ProjectName, OutputDir, ExtraOptions: string;
      ADebug: Boolean = False): Boolean; reintroduce;
    procedure SetDefaultOptions(ADebug: Boolean); override;
    property MaxCLRVersion: string read GetMaxCLRVersion;
  end;
  {$ENDIF MSWINDOWS}

  TJclBpr2Mak = class(TJclBorlandCommandLineTool)
  public
    function GetExeName: string; override;
  end;

  TJclBorlandMake = class(TJclBorlandCommandLineTool)
  public
    function GetExeName: string; override;
  end;

const
  AsmExeName                = 'tasm32.exe';
  BCC32ExeName              = 'bcc32.exe';
  BCC64ExeName              = 'bcc64.exe';
  DCC32ExeName              = 'dcc32.exe';
  DCC64ExeName              = 'dcc64.exe';
  DCCOSX32ExeName           = 'dccosx.exe';
  DCCILExeName              = 'dccil.exe';
  Bpr2MakExeName            = 'bpr2mak.exe';
  MakeExeName               = 'make.exe';

  BinaryExtensionPackage       = '.bpl';
  BinaryExtensionLibrary       = '.dll';
  BinaryExtensionExecutable    = '.exe';
  SourceExtensionDelphiPackage = '.dpk';
  SourceExtensionBCBPackage    = '.bpk';
  SourceExtensionDelphiProject = '.dpr';
  SourceExtensionBCBProject    = '.bpr';
  SourceExtensionDProject      = '.dproj';
  SourceExtensionBDSProject    = '.bdsproj';
  SourceExtensionDOFProject    = '.dof';
  SourceExtensionConfiguration = '.cfg';

function BinaryFileName(const OutputPath, ProjectFileName: string): string;

function IsDelphiPackage(const FileName: string): Boolean;
function IsDelphiProject(const FileName: string): Boolean;
function IsBCBPackage(const FileName: string): Boolean;
function IsBCBProject(const FileName: string): Boolean;

procedure GetDPRFileInfo(const DPRFileName: string; out BinaryExtension: string;
  const LibSuffix: PString = nil);
procedure GetBPRFileInfo(const BPRFileName: string; out BinaryFileName: string;
  const Description: PString = nil);
procedure GetDPKFileInfo(const DPKFileName: string; out RunOnly: Boolean;
  const LibSuffix: PString = nil; const Description: PString = nil);
procedure GetBPKFileInfo(const BPKFileName: string; out RunOnly: Boolean;
  const BinaryFileName: PString = nil; const Description: PString = nil);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.SysConst,
  {$ELSE ~HAS_UNITSCOPE}
  SysConst,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  JclFileUtils,
  JclDevToolsResources,
  JclIDEUtils,
  JclAnsiStrings,
  JclWideStrings,
  JclStrings,
  JclSysInfo,
  JclSimpleXml,
  JclMsBuild;

const
  // DOF options
  DOFDirectoriesSection = 'Directories';
  DOFUnitOutputDirKey   = 'UnitOutputDir';
  DOFSearchPathName     = 'SearchPath';
  DOFConditionals       = 'Conditionals';
  DOFLinkerSection      = 'Linker';
  DOFPackagesKey        = 'Packages';
  DOFCompilerSection    = 'Compiler';
  DOFPackageNoLinkKey   = 'PackageNoLink';
  // injection of new compiler options to workaround L1496 internal error of Delphi 5 and C++Builder 5
  // adding -B switch to the compiler command line forces units to be built
  DOFAdditionalSection  = 'Additional';
  DOFOptionsKey         = 'Options';

  // BDSProj options
  BDSProjPersonalityInfoNodeName = 'PersonalityInfo';
  BDSProjOptionNodeName = 'Option';
  BDSProjNameProperty = 'Name';
  BDSProjPersonalityValue = 'Personality';
  BDSProjUnitOutputDirValue = 'UnitOutputDir';
  BDSProjSearchPathValue = 'SearchPath';
  BDSProjPackagesValue = 'Packages';
  BDSProjConditionalsValue = 'Conditionals';
  BDSProjUsePackagesValue = 'UsePackages';
  BDSProjDirectoriesNodeName = 'Directories';

  // DProj options
  DProjPersonalityNodeName = 'Borland.Personality';
  DProjDelphiPersonalityValue = 'Delphi.Personality';
  DProjDelphiDotNetPersonalityValue = 'DelphiDotNet.Personality';
  DProjUsePackageNodeName = 'DCC_UsePackage';
  DProjDcuOutputDirNodeName = 'DCC_DcuOutput';
  DProjUnitSearchPathNodeName = 'DCC_UnitSearchPath';
  DProjDefineNodeName = 'DCC_Define';
  DProjNamespaceNodeName = 'DCC_Namespace';

  DelphiLibSuffixOption   = '{$LIBSUFFIX ''';
  DelphiDescriptionOption = '{$DESCRIPTION ''';
  DelphiRunOnlyOption     = '{$RUNONLY}';
  DelphiBinaryExtOption   = '{$E ';
  BCBLFlagsOption     = '<LFLAGS ';
  BCBDSwitchOption    = '-D';
  BCBGprSwitchOption  = '-Gpr';
  BCBProjectOption    = '<PROJECT ';

function AnsiStartsText(const SubStr, S: string): Boolean;
begin
  if Length(SubStr) <= Length(S) then
    Result := AnsiStrLIComp(PChar(S), PChar(SubStr), Length(SubStr)) = 0
  else
    Result := False;
end;

procedure GetDPRFileInfo(const DPRFileName: string; out BinaryExtension: string;
  const LibSuffix: PString = nil);
var
  Index: Integer;
  S: string;
  DPRFile: TStrings;
const
  ProgramText = 'program';
  LibraryText = 'library';
begin
  DPRFile := TStringList.Create;
  try
    DPRFile.LoadFromFile(DPRFileName);

    if Assigned(LibSuffix) then
      LibSuffix^ := '';

    BinaryExtension := '';

    for Index := 0 to DPRFile.Count - 1 do
    begin
      S := TrimRight(DPRFile.Strings[Index]);
      if AnsiStartsText(ProgramText, S) and (BinaryExtension = '') then
        BinaryExtension := BinaryExtensionExecutable;
      if AnsiStartsText(LibraryText, S) and (BinaryExtension = '') then
        BinaryExtension := BinaryExtensionLibrary;
      if AnsiStartsText(DelphiBinaryExtOption, S) then
        BinaryExtension :=
          StrTrimQuotes(Copy(S, Length(DelphiBinaryExtOption), Length(S) - Length(DelphiBinaryExtOption)));
      if Assigned(LibSuffix) and AnsiStartsText(DelphiLibSuffixOption, S) then
        LibSuffix^ :=
          StrTrimQuotes(Copy(S, Length(DelphiLibSuffixOption), Length(S) - Length(DelphiLibSuffixOption)));
    end;
  finally
    DPRFile.Free;
  end;
end;

procedure GetBPRFileInfo(const BPRFileName: string; out BinaryFileName: string;
  const Description: PString = nil);
var
  I, J: Integer;
  S, SubS1, SubS2, SubS3: string;
  BPKFile: TStringList;
  LProjectPos, BinaryFileNamePos, EndFileNamePos, LFlagsPos, DSwitchPos: Integer;
  SemiColonPos, AmpPos: Integer;
begin
  BPKFile := TStringList.Create;
  try
    BPKFile.LoadFromFile(BPRFileName);
    BinaryFileName := '';
    if Assigned(Description) then
      Description^ := '';
    for I := 0 to BPKFile.Count - 1 do
    begin
      S := BPKFile[I];

      LProjectPos := Pos(BCBProjectOption, S);
      if LProjectPos > 0 then
      begin
        SubS1 := Copy(S, LProjectPos, Length(S));
        J := 1;
        while (Pos('>', SubS1) = 0) and ((I + J) < BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I + J];
          Inc(J);
        end;

        BinaryFileNamePos := Pos('"', SubS1);
        if BinaryFileNamePos > 0 then
        begin
          SubS2 := Copy(SubS1, BinaryFileNamePos + 1, Length(SubS1) - BinaryFileNamePos);
          EndFileNamePos := Pos('"', SubS2);

          if EndFileNamePos > 0 then
            BinaryFileName := Copy(SubS2, 1, EndFileNamePos - 1);
        end;
      end;

      LFlagsPos := Pos(BCBLFlagsOption, S);
      if LFlagsPos > 0 then
      begin
        SubS1 := Copy(S, LFlagsPos, Length(S));
        J := 1;
        while (Pos('>', SubS1) = 0) and ((I + J) < BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I + J];
          Inc(J);
        end;
        DSwitchPos := Pos(BCBDSwitchOption, SubS1);
        if DSwitchPos > 0 then
        begin
          SubS2 := Copy(SubS1, DSwitchPos, Length(SubS1));
          SemiColonPos := Pos(';', SubS2);
          if SemiColonPos > 0 then
          begin
            SubS3 := Copy(SubS2, SemiColonPos + 1, Length(SubS2));
            AmpPos := Pos('&', SubS3);
            if (Description <> nil) and (AmpPos > 0) then
              Description^ := Copy(SubS3, 1, AmpPos - 1);
          end;
        end;
      end;
    end;
  finally
    BPKFile.Free;
  end;
end;

procedure GetDPKFileInfo(const DPKFileName: string; out RunOnly: Boolean;
  const LibSuffix: PString = nil; const Description: PString = nil);
var
  I: Integer;
  S: string;
  DPKFile: TStringList;
begin
  DPKFile := TStringList.Create;
  try
    DPKFile.LoadFromFile(DPKFileName);
    if Assigned(Description) then
      Description^ := '';
    if Assigned(LibSuffix) then
      LibSuffix^ := '';
    RunOnly := False;
    for I := 0 to DPKFile.Count - 1 do
    begin
      S := TrimRight(DPKFile.Strings[I]);
      if Assigned(Description) and (Pos(DelphiDescriptionOption, S) = 1) then
        Description^ := Copy(S, Length(DelphiDescriptionOption), Length(S) - Length(DelphiDescriptionOption))
      else
      if Assigned(LibSuffix) and (Pos(DelphiLibSuffixOption, S) = 1) then
        LibSuffix^ := StrTrimQuotes(Copy(S, Length(DelphiLibSuffixOption), Length(S) - Length(DelphiLibSuffixOption)))
      else
      if Pos(DelphiRunOnlyOption, S) = 1 then
        RunOnly := True;
    end;
  finally
    DPKFile.Free;
  end;
end;

procedure GetBPKFileInfo(const BPKFileName: string; out RunOnly: Boolean;
  const BinaryFileName: PString = nil; const Description: PString = nil);
var
  I, J: Integer;
  S, SubS1, SubS2, SubS3: string;
  BPKFile: TStringList;
  LFlagsPos, DSwitchPos, SemiColonPos, AmpPos, GprPos: Integer;
  LProjectPos, BinaryFileNamePos, EndFileNamePos: Integer;
begin
  BPKFile := TStringList.Create;
  try
    BPKFile.LoadFromFile(BPKFileName);
    if Assigned(Description) then
      Description^ := '';
    if Assigned(BinaryFileName) then
      BinaryFileName^ := '';
    RunOnly := False;
    for I := 0 to BPKFile.Count - 1 do
    begin
      S := BPKFile[I];

      LProjectPos := Pos(BCBProjectOption, S);
      if Assigned(BinaryFileName) and (LProjectPos > 0) then
      begin
        SubS1 := Copy(S, LProjectPos, Length(S));
        J := 1;
        while (Pos('>', SubS1) = 0) and ((I + J) < BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I + J];
          Inc(J);
        end;

        BinaryFileNamePos := Pos('"', SubS1);
        if BinaryFileNamePos > 0 then
        begin
          SubS2 := Copy(SubS1, BinaryFileNamePos + 1, Length(SubS1) - BinaryFileNamePos);
          EndFileNamePos := Pos('"', SubS2);

          if EndFileNamePos > 0 then
            BinaryFileName^ := Copy(SubS2, 1, EndFileNamePos - 1);
        end;
      end;

      LFlagsPos := Pos(BCBLFlagsOption, S);
      if LFlagsPos > 0 then
      begin
        SubS1 := Copy(S, LFlagsPos, Length(S));
        J := 1;
        while (Pos('>', SubS1) = 0) and ((I + J) < BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I + J];
          Inc(J);
        end;
        DSwitchPos := Pos(BCBDSwitchOption, SubS1);
        GprPos := Pos(BCBGprSwitchOption, SubS1);
        if DSwitchPos > 0 then
        begin
          SubS2 := Copy(SubS1, DSwitchPos, Length(SubS1));
          SemiColonPos := Pos(';', SubS2);
          if SemiColonPos > 0 then
          begin
            SubS3 := Copy(SubS2, SemiColonPos + 1, Length(SubS2));
            AmpPos := Pos('&', SubS3);
            if (Description <> nil) and (AmpPos > 0) then
              Description^ := Copy(SubS3, 1, AmpPos - 1);
          end;
        end;
        if GprPos > 0 then
          RunOnly := True;
      end;
    end;
  finally
    BPKFile.Free;
  end;
end;

function BinaryFileName(const OutputPath, ProjectFileName: string): string;
var
  ProjectExtension, LibSuffix, BinaryExtension: string;
  RunOnly: Boolean;
begin
  ProjectExtension := ExtractFileExt(ProjectFileName);
  if SameText(ProjectExtension, SourceExtensionDelphiPackage) then
  begin
    GetDPKFileInfo(ProjectFileName, RunOnly, @LibSuffix);
    Result := PathExtractFileNameNoExt(ProjectFileName) + LibSuffix + BinaryExtensionPackage;
  end
  else
  if SameText(ProjectExtension, SourceExtensionDelphiProject) then
  begin
    GetDPRFileInfo(ProjectFileName, BinaryExtension, @LibSuffix);
    Result := PathExtractFileNameNoExt(ProjectFileName) + LibSuffix + BinaryExtension;
  end
  else
  if SameText(ProjectExtension, SourceExtensionBCBPackage) then
    GetBPKFileInfo(ProjectFileName, RunOnly, @Result)
  else
  if SameText(ProjectExtension, SourceExtensionBCBProject) then
    GetBPRFileInfo(ProjectFileName, Result)
  else
    raise EJclCompilerUtilsException.CreateResFmt(@RsEUnknownProjectExtension, [ProjectExtension]);

  Result := PathAddSeparator(OutputPath) + Result;
end;

function IsDelphiPackage(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionDelphiPackage);
end;

function IsDelphiProject(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionDelphiProject);
end;

function IsBCBPackage(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionBCBPackage);
end;

function IsBCBProject(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionBCBProject);
end;

{$IFDEF MSWINDOWS}
type
  TFindResStartRec = record
    StartStr: WideString;
    MatchStr: WideString;
  end;
  PFindResStartRec = ^TFindResStartRec;

// helper function to check strings starting "StartStr" in current string table
function FindResStartCallBack(hModule: HMODULE; lpszType, lpszName: PChar;
  lParam: PFindResStartRec): BOOL; stdcall;
var
  ResInfo, ResHData, ResSize, ResIndex: Cardinal;
  ResData: PWord;
  StrLength: Word;
  MatchLen: Integer;
begin
  Result := True;
  MatchLen := Length(lParam^.StartStr);

  ResInfo := FindResource(hModule, lpszName, lpszType);
  if ResInfo <> 0 then
  begin
    ResHData := LoadResource(hModule, ResInfo);
    if ResHData <> 0 then
    begin
      ResData := LockResource(ResHData);
      if Assigned(ResData) then
      begin
        // string tables are a concatenation of maximum 16 prefixed-length widestrings
        ResSize := SizeofResource(hModule, ResInfo) div 2;
        ResIndex := 0;
        // iterate all concatenated strings
        while ResIndex < ResSize do
        begin
          StrLength := ResData^;
          Inc(ResData);
          Inc(ResIndex);
          if (StrLength >= MatchLen) and
            (StrLICompW(PWideChar(lParam^.StartStr), PWideChar(ResData), MatchLen) = 0) then
          begin
            // we have a match
            SetLength(lParam^.MatchStr, StrLength);
            Move(ResData^, lParam^.MatchStr[1], StrLength * SizeOf(lParam^.MatchStr[1]));
            Result := False;
            Break;
          end;
          Inc(ResData, StrLength);
          Inc(ResIndex, StrLength);
        end;
      end;
    end;
  end;
end;

// find in specified module "FileName" a resourcestring starting with StartStr
function FindResStart(const FileName: string; const StartStr: WideString): WideString;
var
  H: HMODULE;
  FindResRec: TFindResStartRec;
begin
  FindResRec.StartStr := StartStr;
  FindResRec.MatchStr := '';

  H := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
  if H <> 0 then
    try
      EnumResourceNames(H, RT_STRING, @FindResStartCallBack, LPARAM(@FindResRec));
    finally
      FreeLibrary(H);
    end;

  Result := FindResRec.MatchStr;
end;
{$ENDIF MSWINDOWS}

//=== { TJclBorlandCommandLineTool } =========================================

constructor TJclBorlandCommandLineTool.Create(const ABinDirectory: string; ALongPathBug: Boolean;
  ACompilerSettingsFormat: TJclCompilerSettingsFormat);
begin
  inherited Create;
  FBinDirectory := ABinDirectory;
  FLongPathBug := ALongPathBug;
  FCompilerSettingsFormat := ACompilerSettingsFormat;
  FOptions := TStringList.Create;
end;

destructor TJclBorlandCommandLineTool.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TJclBorlandCommandLineTool.AddPathOption(const Option, Path: string);
var
  S: string;

  // path before Delphi 2005 must be shortened
  // to avoid the 126 character limit of DCC32 (and eventually other command line tools)
  // which shows up with misleading error messages ("Fatal: System.pas not found") or
  // might even cause AVs
  procedure ConvertToShortPathNames(var Paths: string);
  var
    List: TStringList;
    I: Integer;
  begin
    {$IFDEF MSWINDOWS}
    if LongPathBug then
    begin
      List := TStringList.Create;
      try
        StrToStrings(Paths, PathSep, List);
        for I := 0 to List.Count - 1 do
          List[I] := PathGetShortName(List[I]);
        Paths := StringsToStr(List, PathSep);
      finally
        List.Free;
      end;
    end;
    {$ENDIF MSWINDOWS}
  end;

begin
  S := PathRemoveSeparator(Path);
  ConvertToShortPathNames(S);
  { TODO : If we were sure that options are always case-insensitive
           for Borland tools, we could use UpperCase(Option) below. }
  S := Format('-%s"%s"', [Option, S]);
  // avoid duplicate entries
  if Options.IndexOf(S) = -1 then
    Options.Add(S);
end;

procedure TJclBorlandCommandLineTool.CheckOutputValid;
begin
  if Assigned(FOutputCallback) then
    raise EJclCommandLineToolError.CreateResFmt(@RsECmdLineToolOutputInvalid, [GetExeName]);
end;

function TJclBorlandCommandLineTool.Execute(const CommandLine: string): Boolean;
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);

  Result := InternalExecute(CommandLine);

  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Self);
end;

function TJclBorlandCommandLineTool.GetExeName: string;
begin
  Result := '';
  {$IFDEF MSWINDOWS}
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']); // BCB doesn't support abstract keyword
  {$ENDIF MSWINDOWS}
end;

function TJclBorlandCommandLineTool.GetFileName: string;
begin
  Result := BinDirectory + GetExeName;
  if Pos(' ', Result) > 0 then
    Result := AnsiQuotedStr(Result, '"');
end;

function TJclBorlandCommandLineTool.GetOptions: TStrings;
begin
  Result := FOptions;
end;

function TJclBorlandCommandLineTool.GetOutput: string;
begin
  CheckOutputValid;
  Result := FOutput;
end;

function TJclBorlandCommandLineTool.GetOutputCallback: TTextHandler;
begin
  Result := FOutputCallback;
end;

function TJclBorlandCommandLineTool.InternalExecute(const CommandLine: string): Boolean;
var
  LaunchCommand: string;
  Options: TJclExecuteCmdProcessOptions;
begin
  LaunchCommand := Format('%s %s', [FileName, CommandLine]);

  Options := TJclExecuteCmdProcessOptions.Create(LaunchCommand);
  try
    if Assigned(FOutputCallback) then
    begin
      Options.OutputLineCallback := FOutputCallback;
      FOutputCallback(LaunchCommand);
      Result := ExecuteCmdProcess(Options) and (Options.ExitCode = 0);
    end
    else
    begin
      Result := ExecuteCmdProcess(Options) and (Options.ExitCode = 0);
      FOutput := FOutput + Options.Output;
    end;
  finally
    Options.Free;
  end;
end;

procedure TJclBorlandCommandLineTool.SetOutputCallback(const CallbackMethod: TTextHandler);
begin
  FOutputCallback := CallbackMethod;
end;

//=== { TJclBCC32 } ============================================================

function TJclBCC32.GetExeName: string;
begin
  Result := BCC32ExeName;
end;

class function TJclBCC32.GetPlatform: string;
begin
  Result := BDSPlatformWin32;
end;

//=== { TJclBCC64 } ============================================================

function TJclBCC64.GetExeName: string;
begin
  Result := BCC64ExeName;
end;

class function TJclBCC64.GetPlatform: string;
begin
  Result := BDSPlatformWin64;
end;

//=== { TJclDCC32 } ============================================================

function TJclDCC32.AddDProjOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
var
  DProjFileName, PersonalityName: string;
  MsBuildOptions: TJclMsBuildParser;
  ProjectExtensionsNode, PersonalityNode: TJclSimpleXMLElem;
begin
  DProjFileName := ChangeFileExt(ProjectFileName, SourceExtensionDProject);
  Result := FileExists(DProjFileName) and (CompilerSettingsFormat = csfMsBuild);
  if Result then
  begin
    MsBuildOptions := TJclMsBuildParser.Create(DProjFileName);
    try
      MsBuildOptions.Init;
      if SupportsPlatform then
        MsBuildOptions.Properties.GlobalProperties.Values['Platform'] := GetPlatform;

      if Assigned(FOnEnvironmentVariables) then
        MsBuildOptions.Properties.EnvironmentProperties.Assign(FOnEnvironmentVariables);

      MsBuildOptions.Parse;

      PersonalityName := '';
      ProjectExtensionsNode := MsBuildOptions.ProjectExtensions;
      if Assigned(ProjectExtensionsNode) then
      begin
        PersonalityNode := ProjectExtensionsNode.Items.ItemNamed[DProjPersonalityNodeName];
        if Assigned(PersonalityNode) then
          PersonalityName := PersonalityNode.Value;
      end;
      if StrHasPrefix(PersonalityName, [DProjDelphiPersonalityValue]) or
        AnsiSameText(PersonalityName, DProjDelphiDotNetPersonalityValue) then
      begin
        ProjectOptions.DynamicPackages := MsBuildOptions.Properties.Values[DProjUsePackageNodeName];
        ProjectOptions.UsePackages := ProjectOptions.DynamicPackages <> '';
        ProjectOptions.UnitOutputDir := MsBuildOptions.Properties.Values[DProjDcuOutputDirNodeName];
        ProjectOptions.SearchPath := MsBuildOptions.Properties.Values[DProjUnitSearchPathNodeName];
        ProjectOptions.Conditionals := MsBuildOptions.Properties.Values[DProjDefineNodeName];
        ProjectOptions.Namespace := MsBuildOptions.Properties.Values[DProjNamespaceNodeName];
      end;
    finally
      MsBuildOptions.Free;
    end;
  end;
end;

function TJclDCC32.AddBDSProjOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
var
  BDSProjFileName, PersonalityName: string;
  OptionsXmlFile: TJclSimpleXML;
  PersonalityInfoNode, OptionNode, ChildNode, PersonalityNode, DirectoriesNode: TJclSimpleXMLElem;
  NodeIndex: Integer;
  NameProperty: TJclSimpleXMLProp;
begin
  BDSProjFileName := ChangeFileExt(ProjectFileName, SourceExtensionBDSProject);
  Result := FileExists(BDSProjFileName);
  if Result then
  begin
    OptionsXmlFile := TJclSimpleXML.Create;
    try
      OptionsXmlFile.LoadFromFile(BDSProjFileName);
      OptionsXmlFile.Options := OptionsXmlFile.Options - [sxoAutoCreate];
      PersonalityInfoNode := OptionsXmlFile.Root.Items.ItemNamed[BDSProjPersonalityInfoNodeName];
      PersonalityName := '';
      if Assigned(PersonalityInfoNode) then
      begin
        OptionNode := PersonalityInfoNode.Items.ItemNamed[BDSProjOptionNodeName];
        if Assigned(OptionNode) then
          for NodeIndex := 0 to OptionNode.Items.Count - 1 do
          begin
            ChildNode := OptionNode.Items.Item[NodeIndex];
            if SameText(ChildNode.Name, BDSProjOptionNodeName) then
            begin
              NameProperty := ChildNode.Properties.ItemNamed[BDSProjNameProperty];
              if Assigned(NameProperty) and SameText(NameProperty.Value, BDSProjPersonalityValue) then
              begin
                PersonalityName := ChildNode.Value;
                Break;
              end;
            end;
          end;
      end;
      if PersonalityName <> '' then
      begin
        PersonalityNode := OptionsXmlFile.Root.Items.ItemNamed[PersonalityName];
        if Assigned(PersonalityNode) then
        begin
          DirectoriesNode := PersonalityNode.Items.ItemNamed[BDSProjDirectoriesNodeName];
          if Assigned(DirectoriesNode) then
            for NodeIndex := 0 to DirectoriesNode.Items.Count - 1 do
            begin
              ChildNode := DirectoriesNode.Items.Item[NodeIndex];
              if SameText(ChildNode.Name, BDSProjDirectoriesNodeName) then
              begin
                NameProperty := ChildNode.Properties.ItemNamed[BDSProjNameProperty];
                if Assigned(NameProperty) then
                begin
                  if SameText(NameProperty.Value, BDSProjUnitOutputDirValue) then
                    ProjectOptions.UnitOutputDir := ChildNode.Value
                  else
                  if SameText(NameProperty.Value, BDSProjSearchPathValue) then
                    ProjectOptions.SearchPath := ChildNode.Value
                  else
                  if SameText(NameProperty.Value, BDSProjPackagesValue) then
                    ProjectOptions.DynamicPackages := ChildNode.Value
                  else
                  if SameText(NameProperty.Value, BDSProjConditionalsValue) then
                    ProjectOptions.Conditionals := ChildNode.Value
                  else
                  if SameText(NameProperty.Value, BDSProjUsePackagesValue) then
                    ProjectOptions.UsePackages := StrToBoolean(ChildNode.Value);
                  ProjectOptions.Namespace := '';
                end;
              end;
            end;
        end;
      end;
    finally
      OptionsXmlFile.Free;
    end;
  end;
end;

function TJclDCC32.AddDOFOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
var
  DOFFileName: string;
  OptionsFile: TIniFile;
begin
  DOFFileName := ChangeFileExt(ProjectFileName, SourceExtensionDOFProject);
  Result := FileExists(DOFFileName);
  if Result then
  begin
    OptionsFile := TIniFile.Create(DOFFileName);
    try
      ProjectOptions.SearchPath := OptionsFile.ReadString(DOFDirectoriesSection, DOFSearchPathName, '');
      ProjectOptions.UnitOutputDir := OptionsFile.ReadString(DOFDirectoriesSection, DOFUnitOutputDirKey, '');
      ProjectOptions.Conditionals := OptionsFile.ReadString(DOFDirectoriesSection, DOFConditionals, '');
      ProjectOptions.UsePackages := OptionsFile.ReadString(DOFCompilerSection, DOFPackageNoLinkKey, '') = '1';
      ProjectOptions.DynamicPackages := OptionsFile.ReadString(DOFLinkerSection, DOFPackagesKey, '');
      ProjectOptions.Namespace := '';
    finally
      OptionsFile.Free;
    end;
  end;
end;

procedure TJclDCC32.AddProjectOptions(const ProjectFileName, DCPPath: string);
var
  ProjectOptions: TProjectOptions;
begin
  ProjectOptions.UsePackages := False;
  ProjectOptions.UnitOutputDir := '';
  ProjectOptions.SearchPath := '';
  ProjectOptions.DynamicPackages := '';
  ProjectOptions.SearchDcpPath := '';
  ProjectOptions.Conditionals := '';
  ProjectOptions.Namespace := '';

  if AddDProjOptions(ProjectFileName, ProjectOptions) or
     AddBDSProjOptions(ProjectFileName, ProjectOptions) or
     AddDOFOptions(ProjectFileName, ProjectOptions) then
  begin
    if ProjectOptions.UnitOutputDir <> '' then
    begin
      if DCCVersion >= 24.0 then // XE3+
        AddPathOption('NU', ProjectOptions.UnitOutputDir)
      else
        AddPathOption('N', ProjectOptions.UnitOutputDir);
    end;
    if ProjectOptions.SearchPath <> '' then
    begin
      AddPathOption('I', ProjectOptions.SearchPath);
      AddPathOption('R', ProjectOptions.SearchPath);
    end;
    if ProjectOptions.Conditionals <> '' then
      Options.Add(Format('-D%s', [ProjectOptions.Conditionals]));
    if SamePath(DCPPath, DCPSearchPath) then
      ProjectOptions.SearchDcpPath := DCPPath
    else
      ProjectOptions.SearchDcpPath := StrEnsureSuffix(PathSep, DCPPath) + DCPSearchPath;
    AddPathOption('U', StrEnsureSuffix(PathSep, ProjectOptions.SearchDcpPath) + ProjectOptions.SearchPath);
    if ProjectOptions.UsePackages and (ProjectOptions.DynamicPackages <> '') then
      Options.Add(Format('-LU"%s"', [ProjectOptions.DynamicPackages]));
    if ProjectOptions.Namespace <> '' then
    Options.Add('-ns' + ProjectOptions.Namespace);
  end;
end;

function TJclDCC32.Compile(const ProjectFileName: string): Boolean;
begin
  // Note: PathGetShortName may not return the short path if it's a network
  // drive. Hence we always double quote the path, regardless of the compiling
  // environment.
  Result := Execute(StrDoubleQuote(StrTrimQuotes(ProjectFileName)));
end;

constructor TJclDCC32.Create(const ABinDirectory: string; ALongPathBug: Boolean; ADCCVersion: Single;
  ACompilerSettingsFormat: TJclCompilerSettingsFormat; ASupportsNoConfig, ASupportsPlatform: Boolean;
  const ADCPSearchPath, ALibrarySearchPath, ALibraryDebugSearchPath, ACppSearchPath: string);
begin
  inherited Create(ABinDirectory, ALongPathBug, ACompilerSettingsFormat);
  FDCCVersion := ADCCVersion;
  FSupportsNoConfig := ASupportsNoConfig;
  FSupportsPlatform := ASupportsPlatform;
  FDCPSearchPath := ADCPSearchPath;
  FLibrarySearchPath := ALibrarySearchPath;
  FLibraryDebugSearchPath := ALibraryDebugSearchPath;
  FCppSearchPath := ACppSearchPath;
  SetDefaultOptions(False); // in case $(DELPHI)\bin\dcc32.cfg (replace as appropriate) is invalid
end;

function TJclDCC32.Execute(const CommandLine: string): Boolean;

  function IsPathOption(const S: string; out Len: Integer): Boolean;
  begin
    Result := False;
    if (Length(S) >= 2) and (S[1] = '-') then
      case UpCase(S[2]) of
        'E', 'I', 'O', 'R', 'U':
          begin
            Result := True;
            Len := 2;
          end;
        'L':
          if Length(S) >= 3 then
          begin
            case UpCase(S[3]) of
              'E', 'e',
              'N', 'n':
                Result := True;
            else
              Result := False;
            end;
            Len := 3;
          end;
        'N':
          begin
            Result := True;
            if Length(S) >= 3 then
            begin
              case Upcase(S[3]) of
                'U', 'X': // -NU<dcupath> -NX<xmlpath>
                  if DCCVersion >= 24.0 then // XE3+
                    Len := 3
                  else
                    Len := 2;
                '0'..'9',
                'H', 'O', 'B':
                  Len := 3;
                'S': // -NS<namespace>
                  if DCCVersion >= 23.0 then // XE2+
                    Len := 3
                  else
                    Len := 2;
              else
                Len := 2;
              end;
            end;
          end;
      end;
  end;

var
  OptionIndex, PathIndex, SwitchLen: Integer;
  PathList: TStrings;
  Option, Arguments, CurrentFolder: string;
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);

  FOutput := '';
  Arguments := '';
  CurrentFolder := PathGetShortName(GetCurrentFolder); // used if LongPathBug is True

  PathList := TStringList.Create;
  try
    for OptionIndex := 0 to Options.Count - 1 do
    begin
      Option := Options.Strings[OptionIndex];
      if IsPathOption(Option, SwitchLen) then
      begin
        StrToStrings(StrTrimQuotes(Copy(Option, SwitchLen + 1, Length(Option) - SwitchLen)), PathSep, PathList);
        if LongPathBug then
          // change to relative paths to avoid DCC32 126 character path limit
          for PathIndex := 0 to PathList.Count - 1 do
            PathList.Strings[PathIndex] := PathGetRelativePath(CurrentFolder, ExpandFileName(PathList[PathIndex]));
        if PathList.Count > 0 then
          Arguments := Format('%s %s"%s"', [Arguments, Copy(Option, 1, SwitchLen),
            StringsToStr(PathList, PathSep)]);
      end
      else
        Arguments := Format('%s %s', [Arguments, Option]);
    end;
  finally
    PathList.Free;
  end;

  Result := InternalExecute(CommandLine + Arguments);

  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Self);
end;

function TJclDCC32.GetExeName: string;
begin
  Result := DCC32ExeName;
end;

class function TJclDCC32.GetPlatform: string;
begin
  Result := BDSPlatformWin32;
end;

function TJclDCC32.MakePackage(const PackageName, BPLPath, DCPPath: string; ExtraOptions: string = ''; ADebug: Boolean = False): Boolean;
var
  SaveDir: string;
  ConfigurationFileName, BackupFileName: string;
begin
  SaveDir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(PackageName) + '.');
  try
    // backup existing configuration file, if any
    ConfigurationFileName := ChangeFileExt(PackageName, SourceExtensionConfiguration);
    if FileExists(ConfigurationFileName) then
      FileBackup(ConfigurationFileName, True);

    Options.Clear;
    SetDefaultOptions(ADebug);
    AddProjectOptions(PackageName, DCPPath);
    try
      AddPathOption('LN', DCPPath);
      AddPathOption('LE', BPLPath);
      Options.Add(ExtraOptions);
      Result := Compile(PackageName);
    finally
      // restore existing configuration file, if any
      BackupFileName := GetBackupFileName(ConfigurationFileName);
      if FileExists(BackupFileName) then
        FileMove(BackupFileName, ConfigurationFileName, True);
    end;
  finally
    SetCurrentDir(SaveDir);
  end;
end;

function TJclDCC32.MakeProject(const ProjectName, OutputDir, DcpSearchPath: string;
  ExtraOptions: string = ''; ADebug: Boolean = False): Boolean;
var
  SaveDir: string;
  ConfigurationFileName, BackupFileName: string;
begin
  SaveDir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(ProjectName) + '.');
  try
    // backup existing configuration file, if any
    ConfigurationFileName := ChangeFileExt(ProjectName, SourceExtensionConfiguration);
    if FileExists(ConfigurationFileName) then
      FileBackup(ConfigurationFileName, True);

    Options.Clear;
    SetDefaultOptions(ADebug);
    AddProjectOptions(ProjectName, DcpSearchPath);
    try
      AddPathOption('E', OutputDir);
      Options.Add(ExtraOptions);
      Result := Compile(ProjectName);
    finally
      // restore existing configuration file, if any
      BackupFileName := GetBackupFileName(ConfigurationFileName);
      if FileExists(BackupFileName) then
        FileMove(BackupFileName, ConfigurationFileName, True);
    end;
  finally
    SetCurrentDir(SaveDir);
  end;
end;

procedure TJclDCC32.SetDefaultOptions(ADebug: Boolean);
begin
  Options.Clear;
  if SupportsNoConfig then
    Options.Add('--no-config');
  if ADebug then
    AddPathOption('U', LibraryDebugSearchPath);
  AddPathOption('U', LibrarySearchPath);
  if CppSearchPath <> '' then
  begin
    AddPathOption('U', CppSearchPath);
    Options.Add('-LUrtl');
  end;
end;

//=== { TJclDCC64 } ==========================================================

class function TJclDCC64.GetPlatform: string;
begin
  Result := BDSPlatformWin64;
end;

function TJclDCC64.GetExeName: string;
begin
  Result := DCC64ExeName;
end;

//=== { TJclDCCOSX32 } =======================================================

class function TJclDCCOSX32.GetPlatform: string;
begin
  Result := BDSPlatformOSX32;
end;

function TJclDCCOSX32.GetExeName: string;
begin
  Result := DCCOSX32ExeName;
end;

{$IFDEF MSWINDOWS}
//=== { TJclDCCIL } ==========================================================

function TJclDCCIL.GetExeName: string;
begin
  Result := DCCILExeName;
end;

function TJclDCCIL.GetMaxCLRVersion: string;
var
  StartPos, EndPos: Integer;
begin
  if FMaxCLRVersion <> '' then
  begin
    Result := FMaxCLRVersion;
    Exit;
  end;

  Result := FindResStart(BinDirectory + GetExeName, '  --clrversion');

  StartPos := Pos(':', Result);
  if StartPos = 0 then
    StartPos := Pos('=', Result);

  if StartPos > 0 then
    Result := Copy(Result, StartPos + 1, Length(Result) - StartPos);

  EndPos := Pos(' ', Result);
  if EndPos > 0 then
    SetLength(Result, EndPos - 1);

  if Result = '' then
    Result := 'v1.1.4322'; // do not localize

  FMaxCLRVersion := Result;
end;

function TJclDCCIL.MakeProject(const ProjectName, OutputDir,
  ExtraOptions: string; ADebug: Boolean = False): Boolean;
var
  SaveDir: string;
begin
  SaveDir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(ProjectName) + '.');
  try
    Options.Clear;
    SetDefaultOptions(ADebug);
    AddProjectOptions(ProjectName, '');
    AddPathOption('E', OutputDir);
    Options.Add(ExtraOptions);
    Result := Compile(ProjectName);
  finally
    SetCurrentDir(SaveDir);
  end;
end;

procedure TJclDCCIL.SetDefaultOptions(ADebug: Boolean);
begin
  Options.Clear;
  if ADebug then
    AddPathOption('U', LibraryDebugSearchPath);
  AddPathOption('U', LibrarySearchPath);
end;

{$ENDIF MSWINDOWS}

//=== { TJclBorlandMake } ====================================================

function TJclBorlandMake.GetExeName: string;
begin
  Result := MakeExeName;
end;

//=== { TJclBpr2Mak } ========================================================

function TJclBpr2Mak.GetExeName: string;
begin
  Result := Bpr2MakExeName;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

