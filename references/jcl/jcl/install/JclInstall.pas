{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclInstall.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Robert Rossmair - crossplatform & BCB support, refactoring                                     }
{   Florent Ouchet (outchy) - New installer core, resource refactorings                            }
{   Jean-Fabien Connault (cycocrew)                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclInstall;

interface

{$I jcl.inc}
{$I crossplatform.inc}

uses
  SysUtils, Classes, Contnrs,
  JclSysUtils, JclIDEUtils, JediInstall;

type
  TInstallerOption = (
    joJediCodeLibrary,
      joJCLDef,
        joJCLDefMath,
        joJCLDefDebug,
        joJCLDefWrappers,
        joJCLDefPCRE,
        joJCLDefBZip2,
        joJCLDefZLib,
        joJCLDefUnicode,
        joJCLDefContainer,
        joJCLDef7z,
        joJCLDefThreadSafe,
        joJCLDefDropObsoleteCode,
        joJCLDefUnitVersioning,
        joJCLDefMathPrecSingle,
        joJCLDefMathPrecDouble,
        joJCLDefMathPrecExtended,
        joJCLDefMathExtremeValues,
        joJCLDefHookDllExceptions,
        joJCLDefDebugNoBinary,
        joJCLDefDebugNoTD32,
        joJCLDefDebugNoMap,
        joJCLDefDebugNoExports,
        joJCLDefDebugNoSymbols,
        joJCLDefPCREStaticLink,
        joJCLDefPCRELinkDLL,
        joJCLDefPCRELinkOnRequest,
        joJCLDefPCRERTL,
        joJCLDefPCRE8,
        joJCLDefPCRE16,
        joJCLDefPCREPrefer16,
        joJCLDefBZip2StaticLink,
        joJCLDefBZip2LinkDLL,
        joJCLDefBZip2LinkOnRequest,
        joJCLDefZLibStaticLink,
        joJCLDefZLibLinkDLL,
        joJCLDefZLibLinkOnRequest,
        joJCLDefZLibRTL,
        joJCLDefUnicodeRTLDatabase,
        joJCLDefUnicodeSilentFailure,
        joJCLDefUnicodeRawData,
        joJCLDefUnicodeZLibData,
        joJCLDefUnicodeBZip2Data,
        joJCLDefContainerAnsiStr,
        joJCLDefContainerWideStr,
        joJCLDefContainerUnicodeStr,
        joJCLDefContainerNoStr,
        //joJCLDef7zStaticLink,
        joJCLDef7zLinkDLL,
        joJCLDef7zLinkOnRequest,
      joJCLEnvironment,
        joJCLEnvLibPath,
        joJCLEnvBrowsingPath,
        joJCLEnvDebugDCUPath,
        joJCLEnvIncludePath,
      joJCLMake,
        joJCLMakeRelease,
        joJCLMakeDebug,
        joJCLCopyHppFiles,
        joJCLCheckHppFiles,
      joJCLPackages,
        joJCLDualPackages,
        joJCLCopyPackagesHppFiles,
        joJCLMapCreate,
          joJCLJdbgCreate,
          joJCLJdbgInsert,
          joJCLMapDelete,
        joJCLExperts,
          joJCLExpertsDsgnPackages,
          joJCLExpertsDLL,
          joJCLExpertDebug,
          joJCLExpertAnalyzer,
          joJCLExpertFavorite,
          joJCLExpertRepository,
          joJCLExpertThreadNames,
          joJCLExpertUses,
          joJCLExpertSimdView,
          joJCLExpertVersionControl,
          joJCLExpertStackTraceViewer,
      joJCLExceptDlg,
        joJCLExceptDlgVCL,
        joJCLExceptDlgVCLSnd,
      joJCLHelp,
        joJCLHelpHlp,
        joJCLHelpChm,
        joJCLHelpHxS,
        joJCLHelpHxSPlugin,
      joJCLMakeDemos);

const
  JclDefineNames: array [joJCLDefThreadSafe..joJCLDef7zLinkOnRequest] of string =
    ( 'THREADSAFE', 'DROP_OBSOLETE_CODE', 'UNITVERSIONING',
      'MATH_SINGLE_PRECISION', 'MATH_DOUBLE_PRECISION', 'MATH_EXTENDED_PRECISION',
      'MATH_EXT_EXTREMEVALUES',  'HOOK_DLL_EXCEPTIONS',
      'DEBUG_NO_BINARY', 'DEBUG_NO_TD32', 'DEBUG_NO_MAP', 'DEBUG_NO_EXPORTS',
      'DEBUG_NO_SYMBOLS', 'PCRE_STATICLINK', 'PCRE_LINKDLL',
      'PCRE_LINKONREQUEST', 'PCRE_RTL', 'PCRE_8', 'PCRE_16', 'PCRE_PREFER_16',
      'BZIP2_STATICLINK', 'BZIP2_LINKDLL', 'BZIP2_LINKONREQUEST', 'ZLIB_STATICLINK',
      'ZLIB_LINKDLL', 'ZLIB_LINKONREQUEST', 'ZLIB_RTL', 'UNICODE_RTL_DATABASE',
      'UNICODE_SILENT_FAILURE', 'UNICODE_RAW_DATA', 'UNICODE_ZLIB_DATA',
      'UNICODE_BZIP2_DATA', 'CONTAINER_ANSISTR', 'CONTAINER_WIDESTR',
      'CONTAINER_UNICODESTR', 'CONTAINER_NOSTR', {'7ZIP_STATICLINK',}
      '7ZIP_LINKDLL', '7ZIP_LINKONREQUEST' );

type
  TJclDistribution = class;

  TJclInstallation = class
  private
    // identification
    FDistribution: TJclDistribution;
    FTarget: TJclBorRADToolInstallation;
    FTargetName: string;
    FTargetPlatform: TJclBDSPlatform;
    FIncludeFileName: string;
    FGUIPage: IJediInstallPage;
    FGUI: IJediInstallGUI;
    FGUIBPLPathIndex: Integer;
    FGUIDCPPathIndex: Integer;
    FGUIHPPPathIndex: Integer;
    FLibDebugDir: string;
    FLibReleaseDir: string;
    FJclDcpPath: string;
    FDemoList: TStringList;
    FLogLines: TJclSimpleLog;
    FDemoSectionName: string;
    FLogFileName: string;
    FSilent: Boolean;
    FRuntimeInstallation: Boolean;
    FProfilesTargets: TObjectList;
    FInstallSuccess: Boolean;
    procedure AddDemo(const Directory: string; const FileInfo: TSearchRec);
    procedure AddDemos(const Directory: string);
    function GetDemoList: TStringList;
    function MakePath(const FormatStr: string): string;
    procedure WriteLog(const Msg: string);
    function GetEnabled: Boolean;
    function GetIsProfileEnabled(Index: Integer): Boolean;
    function GetProfilesTarget(Index: Integer): TJclBorRADToolInstallation;
    function GetTargetSupportsCBuilder: Boolean;
    function GetTargetSupportsDelphi: Boolean;
  protected
    constructor Create(JclDistribution: TJclDistribution;
      InstallTarget: TJclBorRADToolInstallation; ATargetPlatform: TJclBDSPlatform;
      const AGUIPage: IJediInstallPage);
    function CompileLibraryUnits(const SubDir: string; Debug: Boolean): Boolean;
    function CompilePackage(const Name: string): Boolean;
    function CompileApplication(FileName: string): Boolean;
    function DeletePackage(const Name: string): Boolean;
    procedure ConfigureBpr2Mak(const PackageFileName: string);
    {$IFDEF MSWINDOWS}
    function CompileExpert(const Name: string): Boolean;
    {$ENDIF MSWINDOWS}

    function GetBplPath: string;
    function GetDcpPath: string;
    function GetHppPath: string;
    function GetOptionChecked(Option: TInstallerOption): Boolean; overload;
    function GetOptionCheckedById(Id: Integer): Boolean; overload;
    function GetPlatformStr: string;
    procedure MarkOptionBegin(Id: Integer); overload;
    procedure MarkOptionBegin(Option: TInstallerOption); overload;
    procedure MarkOptionEnd(Id: Integer; Success: Boolean); overload;
    procedure MarkOptionEnd(Option: TInstallerOption; Success: Boolean); overload;
  public
    destructor Destroy; override;
    procedure Close;
    procedure Init;
    function RemoveSettings: Boolean;
    function Install: Boolean;
    function Uninstall(AUninstallHelp: Boolean): Boolean;

    property Distribution: TJclDistribution read FDistribution;
    property Target: TJclBorRADToolInstallation read FTarget;
    property TargetSupportsDelphi: Boolean read GetTargetSupportsDelphi;
    property TargetSupportsCBuilder: Boolean read GetTargetSupportsCBuilder;
    property TargetName: string read FTargetName;
    property IncludeFileName: string read FIncludeFileName;
    property GUIPage: IJediInstallPage read FGUIPage;
    property GUI: IJediInstallGUI read FGUI;
    property TargetPlatform: TJclBDSPlatform read FTargetPlatform;
    property Enabled: Boolean read GetEnabled;
    property OptionCheckedById[Id: Integer]: Boolean read GetOptionCheckedById;
    property OptionChecked[Option: TInstallerOption]: Boolean read GetOptionChecked;
    property LogFileName: string read FLogFileName;
    property Silent: Boolean read FSilent write FSilent;
    property RuntimeInstallation: Boolean read FRuntimeInstallation; // false for C#Builder 1 and Delphi 8 targets
    property InstallSuccess: Boolean read FInstallSuccess; // True if Install has been called and returned True

    property IsProfileEnabled[Index: Integer]: Boolean read GetIsProfileEnabled;
    property ProfileTargets[Index: Integer]: TJclBorRADToolInstallation read GetProfilesTarget;
  end;

  TJclDistribution = class (TInterfacedObject, IJediProduct)
  private
    FJclPath: string;
    FJclBinDir: string;
    FJclBin64Dir: string;
    FLibReleaseDirMask: string;
    FLibDebugDirMask: string;
    FJclIncludeDir: string;
    FJclIncludeTemplate: string;
    FJclSourcePath: string;
    FJclOldSourcePath: string;
    FJclExamplesDir: string;
    FVclDialogFileName: string;
    FVclDialogSendFileName: string;
    FVclDialogIconFileName: string;
    FVclDialogSendIconFileName: string;
    FJclOldChmHelpFileName: string;
    FJclChmHelpFileName: string;
    FJclContainersChmHelpFileName: string;
    FJclDeveloperToolsChmHelpFileName: string;
    FJclVclChmHelpFileName: string;
    FJclOldHlpHelpFileName: string;
    FJclHlpHelpFileName: string;
    FJclContainersHlpHelpFileName: string;
    FJclDeveloperToolsHlpHelpFileName: string;
    FJclVclHlpHelpFileName: string;
    FJclOldHxSHelpFileName: string;
    FJclHxSHelpFileName: string;
    FJclContainersHxSHelpFileName: string;
    FJclDeveloperToolsHxSHelpFileName: string;
    FJclVclHxSHelpFileName: string;
    FJclReadmeFileName: string;
    FJclLicenseFileName: string;
    FGUI: IJediInstallGUI;
    FReadMePage: IJediTextPage;
    FLicensePage: IJediTextPage;
    FNbEnabled: Integer;
    FNbInstalled: Integer;
    {$IFDEF MSWINDOWS}
    FRegHelpCommands: TStrings;
    {$ENDIF MSWINDOWS}
    FRadToolInstallations: TJclBorRADToolInstallations;
    FTargetInstalls: TObjectList;
    FProfilesPage: IJediProfilesPage;
    function GetVersion: string;
    property Version: string read GetVersion;
    function CreateInstall(Target: TJclBorRADToolInstallation): Boolean;
    function GetTargetInstall(Index: Integer): TJclInstallation;
    function GetTargetInstallCount: Integer;
    {$IFDEF MSWINDOWS}
    procedure RegHelpInternalAdd(Command: Integer; Arguments: string; DoNotRepeatCommand: Boolean);
    function RegHelpExecuteCommands(DisplayErrors: Boolean): Boolean;
    procedure RegHelpClearCommands;
    {$ENDIF MSWINDOWS}
  public
    constructor Create;
    destructor Destroy; override;

    {$IFDEF MSWINDOWS}
    procedure RegHelpCreateTransaction;
    procedure RegHelpCommitTransaction;
    procedure RegHelpRegisterNameSpace(const Name, Collection, Description: WideString);
    procedure RegHelpUnregisterNameSpace(const Name: WideString);
    procedure RegHelpRegisterHelpFile(const NameSpace, Identifier: WideString;
      const LangId: Integer; const HxSFile, HxIFile: WideString);
    procedure RegHelpUnregisterHelpFile(const NameSpace, Identifier: WideString;
      const LangId: Integer);
    procedure RegHelpPlugNameSpaceIn(const SourceNameSpace, TargetNameSpace: WideString);
    procedure RegHelpUnPlugNameSpace(const SourceNameSpace, TargetNameSpace: WideString);
    {$ENDIF MSWINDOWS}

    // IJediProduct
    procedure Init;
    function Install(InstallPage: IJediInstallPage = nil): Boolean;
    function Uninstall(InstallPage: IJediInstallPage = nil): Boolean;
    procedure Close;

    property JclPath: string read FJclPath;
    property JclBinDir: string read FJclBinDir;
    property JclBin64Dir: string read FJclBin64Dir;
    property LibReleaseDirMask: string read FLibReleaseDirMask;
    property LibDebugDirMask: string read FLibDebugDirMask;
    property JclIncludeDir: string read FJclIncludeDir;
    property JclIncludeTemplate: string read FJclIncludeTemplate;
    property JclSourcePath: string read FJclSourcePath;
    property JclOldSourcePath: string read FJclOldSourcePath;
    property JclExamplesDir: string read FJclExamplesDir;
    property VclDialogFileName: string read FVclDialogFileName;
    property VclDialogSendFileName: string read FVclDialogSendFileName;
    property VclDialogIconFileName: string read FVclDialogIconFileName;
    property VclDialogSendIconFileName: string read FVclDialogSendIconFileName;
    property JclOldChmHelpFileName: string read FJclOldChmHelpFileName;
    property JclChmHelpFileName: string read FJclChmHelpFileName;
    property JclContainersChmHelpFileName: string read FJclContainersChmHelpFileName;
    property JclDeveloperToolsChmHelpFileName: string read FJclDeveloperToolsChmHelpFileName;
    property JclVclChmHelpFileName: string read FJclVclChmHelpFileName;
    property JclOldHlpHelpFileName: string read FJclOldHlpHelpFileName;
    property JclHlpHelpFileName: string read FJclHlpHelpFileName;
    property JclContainersHlpHelpFileName: string read FJclContainersHlpHelpFileName;
    property JclDeveloperToolsHlpHelpFileName: string read FJclDeveloperToolsHlpHelpFileName;
    property JclVclHlpHelpFileName: string read FJclVclHlpHelpFileName;
    property JclOldHxSHelpFileName: string read FJclOldHxSHelpFileName;
    property JclHxSHelpFileName: string read FJclHxSHelpFileName;
    property JclContainersHxSHelpFileName: string read FJclContainersHxSHelpFileName;
    property JclDeveloperToolsHxSHelpFileName: string read FJclDeveloperToolsHxSHelpFileName;
    property JclVclHxSHelpFileName: string read FJclVclHxSHelpFileName;
    property JclReadmeFileName: string read FJclReadmeFileName;
    property JclLicenseFileName: string read FJclLicenseFileName;
    property RadToolInstallations: TJclBorRADToolInstallations read FRadToolInstallations;
    property TargetInstalls[Index: Integer]: TJclInstallation read GetTargetInstall;
    property TargetInstallCount: Integer read GetTargetInstallCount;

    property GUI: IJediInstallGUI read FGUI;
    property ReadMePage: IJediTextPage read FReadMePage;
    property LicensePage: IJediTextPage read FLicensePage;
    property NbEnabled: Integer read FNbEnabled;
    property NbInstalled: Integer read FNbInstalled;

    property ProfilesPage: IJediProfilesPage read FProfilesPage;
  end;

implementation

uses
  TypInfo,
  JclBase, JclResources, JclSysInfo,
  {$IFDEF MSWINDOWS}
  Windows,
  JclPeImage,
  JclRegistry,
  JclDebug,
  JclSecurity,
  JediRegInfo,
  JclShell,
  {$ENDIF MSWINDOWS}
  JclFileUtils, JclStrings,
  JclSimpleXML, JclStreams,
  JclCompilerUtils,
  JclContainerIntf,
  JclPreProcessorParser,
  JclDevToolsResources,
  JediInstallResources,
  JclInstallResources;

type
  TOptionRec = record
    Id: Integer;
    Caption: PResStringRec;
    Hint: PResStringRec;
  end;

var
  OptionData: array[TInstallerOption] of TOptionRec =
    (
      (Id: -1; Caption: @RsCaptionLibrary;                 Hint: @RsHintLibrary), // joLibrary
      (Id: -1; Caption: @RsCaptionDef;                     Hint: @RsHintDef), // joDef
      (Id: -1; Caption: @RsCaptionDefMath;                 Hint: @RsHintDefMath), // joDefMath
      (Id: -1; Caption: @RsCaptionDefDebug;                Hint: @RsHintDefDebug), // joDefDebug
      (Id: -1; Caption: @RsCaptionDefWrappers;             Hint: @RsHintDefWrappers), // joDefWrappers
      (Id: -1; Caption: @RsCaptionDefPCRE;                 Hint: @RsHintDefPCRE), // joDefPCRE
      (Id: -1; Caption: @RsCaptionDefBZip2;                Hint: @RsHintDefBZip2), // joDefBZip2
      (Id: -1; Caption: @RsCaptionDefZLib;                 Hint: @RsHintDefZLib), // joDefZLib
      (Id: -1; Caption: @RsCaptionDefUnicode;              Hint: @RsHintDefUnicode), // joDefUnicode
      (Id: -1; Caption: @RsCaptionDefContainer;            Hint: @RsHintDefContainer), // joDefContainer
      (Id: -1; Caption: @RsCaptionDef7z;                   Hint: @RsHintDef7z), // joDef7z
      (Id: -1; Caption: @RsCaptionDefThreadSafe;           Hint: @RsHintDefThreadSafe), // joDefThreadSafe
      (Id: -1; Caption: @RsCaptionDefDropObsoleteCode;     Hint: @RsHintDefDropObsoleteCode), // joDefDropObsoleteCode
      (Id: -1; Caption: @RsCaptionDefUnitVersioning;       Hint: @RsHintDefUnitVersioning), // joDefUnitVersioning
      (Id: -1; Caption: @RsCaptionDefMathPrecSingle;       Hint: @RsHintDefMathPrecSingle), // ioDefMathPrecSingle
      (Id: -1; Caption: @RsCaptionDefMathPrecDouble;       Hint: @RsHintDefMathPrecDouble), // joDefMathPrecDouble
      (Id: -1; Caption: @RsCaptionDefMathPrecExtended;     Hint: @RsHintDefMathPrecExtended), // joDefMathPrecExtended
      (Id: -1; Caption: @RsCaptionDefMathExtremeValues;    Hint: @RsHintDefMathExtremeValues), // joDefMathExtremeValues
      (Id: -1; Caption: @RsCaptionDefHookDllExceptions;    Hint: @RsHintDefHookDllExceptions), // joDefHookDllExceptions
      (Id: -1; Caption: @RsCaptionDefDebugNoBinary;        Hint: @RsHintDefDebugNoBinary), // joDefDebugNoBinary
      (Id: -1; Caption: @RsCaptionDefDebugNoTD32;          Hint: @RsHintDefDebugNoTD32), // joDefDebugNoTD32
      (Id: -1; Caption: @RsCaptionDefDebugNoMap;           Hint: @RsHintDefDebugNoMap), // joDefDebugNoMap
      (Id: -1; Caption: @RsCaptionDefDebugNoExports;       Hint: @RsHintDefDebugNoExports), // joDefDebugNoExports
      (Id: -1; Caption: @RsCaptionDefDebugNoSymbols;       Hint: @RsHintDefDebugNoSymbols), // joDefDebugNoSymbols
      (Id: -1; Caption: @RsCaptionDefPCREStaticLink;       Hint: @RsHintDefPCREStaticLink), // joDefPCREStaticLink
      (Id: -1; Caption: @RsCaptionDefPCRELinkDLL;          Hint: @RsHintDefPCRELinkDLL), // joDefPCRELinkDLL
      (Id: -1; Caption: @RsCaptionDefPCRELinkOnRequest;    Hint: @RsHintDefPCRELinkOnRequest), // joDefPCRELinkOnRequest
      (Id: -1; Caption: @RsCaptionDefPCRERTL;              Hint: @RsHintDefPCRERTL), // joDefPCRERTL
      (Id: -1; Caption: @RsCaptionDefPCRE8;                Hint: @RsHintDefPCRE8), // joDefPCRE8
      (Id: -1; Caption: @RsCaptionDefPCRE16;               Hint: @RsHintDefPCRE16), // joDefPCRE16
      (Id: -1; Caption: @RsCaptionDefPCREPrefer16;         Hint: @RsHintDefPCREPrefer16), // joDefPCREPrefer16
      (Id: -1; Caption: @RsCaptionDefBZip2StaticLink;      Hint: @RsHintDefBZip2StaticLink), // joDefBZip2StaticLink
      (Id: -1; Caption: @RsCaptionDefBZip2LinkDLL;         Hint: @RsHintDefBZip2LinkDLL), // joDefBZip2LinkDLL
      (Id: -1; Caption: @RsCaptionDefBZip2LinkOnRequest;   Hint: @RsHintDefBZip2LinkOnRequest), // joDefBZip2LinkOnRequest
      (Id: -1; Caption: @RsCaptionDefZLibStaticLink;       Hint: @RsHintDefZLibStaticLink), // joDefZLibStaticLink
      (Id: -1; Caption: @RsCaptionDefZLibLinkDLL;          Hint: @RsHintDefZLibLinkDLL), // joDefZLibLinkDLL
      (Id: -1; Caption: @RsCaptionDefZLibLinkOnRequest;    Hint: @RsHintDefZLibLinkOnRequest), // joDefZLibLinkOnRequest
      (Id: -1; Caption: @RsCaptionDefZLibRTL;              Hint: @RsHintDefZLibRTL), // joDefZLibRTL
      (Id: -1; Caption: @RsCaptionDefUnicodeRTLDatabase;   Hint: @RsHintDefUnicodeRTLDatabase), // joDefUnicodeSilentFailure
      (Id: -1; Caption: @RsCaptionDefUnicodeSilentFailure; Hint: @RsHintDefUnicodeSilentFailure), // joDefUnicodeSilentFailure
      (Id: -1; Caption: @RsCaptionDefUnicodeRawData;       Hint: @RsHintDefUnicodeRawData), // joDefUnicodeRawData
      (Id: -1; Caption: @RsCaptionDefUnicodeZLibData;      Hint: @RsHintDefUnicodeZLibData), // joDefUnicodeZLibData
      (Id: -1; Caption: @RsCaptionDefUnicodeBZip2Data;     Hint: @RsHintDefUnicodeBZip2Data), // joDefUnicodeBZip2Data
      (Id: -1; Caption: @RsCaptionDefContainerAnsiStr;     Hint: @RsHintDefContainerAnsiStr), // joDefContainerAnsiStr
      (Id: -1; Caption: @RsCaptionDefContainerWideStr;     Hint: @RsHintDefContainerWideStr), // joDefContainerWideStr
      (Id: -1; Caption: @RsCaptionDefContainerUnicodeStr;  Hint: @RsHintDefContainerUnicodeStr), // joDefContainerUnicodeStr
      (Id: -1; Caption: @RsCaptionDefContainerNoStr;       Hint: @RsHintDefContainerNoStr), // joDefContainerNoStr
      //(Id: -1; Caption: @RsCaptionDef7zStaticLink;         Hint: @RsHintDef7zStaticLink), // joDef7zStaticLink
      (Id: -1; Caption: @RsCaptionDef7zLinkDLL;            Hint: @RsHintDef7zLinkDLL), // joDef7zLinkDLL
      (Id: -1; Caption: @RsCaptionDef7zLinkOnRequest;      Hint: @RsHintDef7zLinkOnRequest), // joDef7zLinkOnRequest
      (Id: -1; Caption: @RsCaptionEnvironment;             Hint: @RsHintEnvironment), // joEnvironment
      (Id: -1; Caption: @RsCaptionEnvLibPath;              Hint: @RsHintEnvLibPath), // joEnvLibPath
      (Id: -1; Caption: @RsCaptionEnvBrowsingPath;         Hint: @RsHintEnvBrowsingPath), // joEnvBrowsingPath
      (Id: -1; Caption: @RsCaptionEnvDebugDCUPath;         Hint: @RsHintEnvDebugDCUPath), // joEnvDebugDCUPath
      (Id: -1; Caption: @RsCaptionEnvIncludePath;          Hint: @RsHintEnvIncludePath),  // joEnvIncludePath
      (Id: -1; Caption: @RsCaptionMake;                    Hint: @RsHintMake), // joMake
      (Id: -1; Caption: @RsCaptionMakeRelease;             Hint: @RsHintMakeRelease), // joMakeRelease
      (Id: -1; Caption: @RsCaptionMakeDebug;               Hint: @RsHintMakeDebug), // joMakeDebug
      (Id: -1; Caption: @RsCaptionCopyHppFiles;            Hint: @RsHintCopyHppFiles), // joCopyHppFiles
      (Id: -1; Caption: @RsCaptionCheckHppFiles;           Hint: @RsHintCheckHppFiles), // joCheckHppFiles
      (Id: -1; Caption: @RsCaptionPackages;                Hint: @RsHintPackages), // joPackages
      (Id: -1; Caption: @RsCaptionDualPackages;            Hint: @RsHintDualPackages), // joDualPackages
      (Id: -1; Caption: @RsCaptionCopyPackagesHppFiles;    Hint: @RsHintCopyPackagesHppFiles), // joCopyPackagesHppFiles
      (Id: -1; Caption: @RsCaptionMapCreate;               Hint: @RsHintMapCreate), // joMapCreate
      (Id: -1; Caption: @RsCaptionJdbgCreate;              Hint: @RsHintJdbgCreate), // joJdbgCreate
      (Id: -1; Caption: @RsCaptionJdbgInsert;              Hint: @RsHintJdbgInsert), // joJdbgInsert
      (Id: -1; Caption: @RsCaptionMapDelete;               Hint: @RsHintMapDelete), // joMapDelete
      (Id: -1; Caption: @RsCaptionExperts;                 Hint: @RsHintExperts), // joExperts
      (Id: -1; Caption: @RsCaptionExpertsDsgnPackages;     Hint: @RsHintExpertsDsgnPackages), // joExpertsDsgnPackages
      (Id: -1; Caption: @RsCaptionExpertsDLL;              Hint: @RsHintExpertsDLL), // joExpertsDLL
      (Id: -1; Caption: @RsCaptionExpertDebug;             Hint: @RsHintExpertDebug), // joExpertDebug
      (Id: -1; Caption: @RsCaptionExpertAnalyzer;          Hint: @RsHintExpertAnalyzer), // joExpertAnalyzer
      (Id: -1; Caption: @RsCaptionExpertFavorite;          Hint: @RsHintExpertFavorite), // joExpertFavorite
      (Id: -1; Caption: @RsCaptionExpertRepository;        Hint: @RsHintExpertRepository), // joExpertRepository
      (Id: -1; Caption: @RsCaptionExpertThreadNames;       Hint: @RsHintExpertThreadNames), // joExpertThreadNames
      (Id: -1; Caption: @RsCaptionExpertUses;              Hint: @RsHintExpertUses), // joExpertUses
      (Id: -1; Caption: @RsCaptionExpertSimdView;          Hint: @RsHintExpertSimdView), // joExpertSimdView
      (Id: -1; Caption: @RsCaptionExpertVersionControl;    Hint: @RsHintExpertVersionControl), // joExpertVersionControl
      (Id: -1; Caption: @RsCaptionExpertStackTraceViewer;  Hint: @RsHintExpertStackTraceViewer), //joExpertStackTraceViewer
      (Id: -1; Caption: @RsCaptionExceptDlg;               Hint: @RsHintExceptDlg), // joExceptDlg
      (Id: -1; Caption: @RsCaptionExceptDlgVCL;            Hint: @RsHintExceptDlgVCL), // joExceptDlgVCL
      (Id: -1; Caption: @RsCaptionExceptDlgVCLSnd;         Hint: @RsHintExceptDlgVCLSnd), // joExceptDlgVCLSnd
      (Id: -1; Caption: @RsCaptionHelp;                    Hint: @RsHintHelp), // joHelp
      (Id: -1; Caption: @RsCaptionHelpHlp;                 Hint: @RsHintHelpHlp), // joHelpHlp
      (Id: -1; Caption: @RsCaptionHelpChm;                 Hint: @RsHintHelpChm), // joHelpChm
      (Id: -1; Caption: @RsCaptionHelpHxS;                 Hint: @RsHintHelpHxS), // joHelpHxS
      (Id: -1; Caption: @RsCaptionHelpHxSPlugin;           Hint: @RsHintHelpHxSPlugin), // joHelpHxSPlugin
      (Id: -1; Caption: @RsCaptionMakeDemos;               Hint: @RsHintMakeDemos) // joMakeDemos
    );

const
  // Names
  OptionNameBPLPath = 'BPL-Path';
  OptionNameDCPPath = 'DCP-Path';
  OptionNameBPIPath = 'BPI-Path';
  OptionNameHPPPath = 'HPP-Path';

  VersionDir = '\%s';
  VersionDirExp = '\%%s';

  // native packages
  JclPackage               = 'Jcl';
  JclContainersPackage     = 'JclContainers';
  JclDeveloperToolsPackage = 'JclDeveloperTools';
  JclVclPackage            = 'JclVcl';

  JclExpertBase             = 'JclBaseExpert';
  JclExpertDebug            = 'JclDebugExpert';
  JclExpertAnalyzer         = 'JclProjectAnalysisExpert';
  JclExpertFavorite         = 'JclFavoriteFoldersExpert';
  JclExpertRepository       = 'JclRepositoryExpert';
  JclExpertThrNames         = 'JclThreadNameExpert';
  JclExpertUses             = 'JclUsesExpert';
  JclExpertSimdView         = 'JclSIMDViewExpert';
  JclExpertVersionControl   = 'JclVersionControlExpert';
  JclExpertStackTraceViewer = 'JclStackTraceViewerExpert';

  SupportedExperts: array [joJCLExperts..joJCLExpertStackTraceViewer] of string =
    (
      JclExpertBase, '', '', JclExpertDebug, JclExpertAnalyzer,
      JclExpertFavorite, JclExpertRepository, JclExpertThrNames,
      JclExpertUses, JclExpertSimdView, JclExpertVersionControl, JclExpertStackTraceViewer
    );

  OldExperts: array [0..6] of string =
    ( 'JclDebugIde', 'ProjectAnalyzer', 'IdeOpenDlgFavorite', 'ThreadNameExpert', 'JediUses', 'JclSIMDView', 'JclVersionControl' );

  JclSrcDirWindows      = 'source' + DirDelimiter + 'windows';
  JclSrcDirUnix         = 'source' + DirDelimiter + 'unix';
  JclSrcDirVcl          = 'source' + DirDelimiter + 'vcl';
  JclSrcDirCommon       = 'source' + DirDelimiter + 'common';
  JclOldIncludeDir      = 'source';
  JclOldJppDir          = 'devtools' + DirDelimiter + 'jpp';
  JClOldJppDirTemplates = 'devtools' + DirDelimiter + 'jpp' + DirDelimiter + 'Templates';

  BCBIncludePath = '%s' + DirSeparator + '%s' + DirSeparator + '$(BCB)' + DirDelimiter + 'include;$(BCB)' + DirDelimiter + 'include' + DirDelimiter + 'vcl';
  {$IFDEF MSWINDOWS}
  BCBObjectPath  = '%s;%s;$(BCB)\Lib\Obj';
  // to be compiled and added to IDE browsing path
  JclSrcPaths: array[0..2] of string = (JclSrcDirCommon, JclSrcDirWindows, JclSrcDirVcl);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  BCBObjectPath  = BCBIncludePath;
  JclSrcPaths: array[0..1] of string = (JclSrcDirCommon, JclSrcDirUnix);
  {$ENDIF UNIX}
  JclOldSrcPaths: array[0..2] of string = (JclOldIncludeDir, JclOldJppDir, JclOldJppDirTemplates);

  JclIncludeTemplateFileName = 'jcl.template.inc';
  JclIncludeMask             = 'jcl%s.inc';

  ExceptDlgPath = 'experts' + DirDelimiter + 'repository' + DirDelimiter + 'ExceptionDialog' + DirDelimiter + 'StandardDialogs' + DirDelimiter;
  ExceptDlgVclFileName    = 'ExceptDlg.pas';
  ExceptDlgVclSndFileName = 'ExceptDlgMail.pas';

  ExceptIcoPath = 'experts' + DirDelimiter + 'repository' + DirDelimiter + 'ExceptionDialog' + DirDelimiter + 'Icons' + DirDelimiter;
  ExceptIcoVclFileName    = 'ExceptDlg.ico';
  ExceptIcoVclSndFileName = 'ExceptDlgMail.ico';

  JclOldChmHelpFile            = 'help' + DirDelimiter + 'JCLHelp.chm';
  JclChmHelpFile               = 'help' + DirDelimiter + JclPackage + '.chm';
  JclContainersChmHelpFile     = 'help' + DirDelimiter + JclContainersPackage + '.chm';
  JclDeveloperToolsChmHelpFile = 'help' + DirDelimiter + JclDeveloperToolsPackage + '.chm';
  JclVclChmHelpFile            = 'help' + DirDelimiter + JclVclPackage + '.chm';
  JclOldHlpHelpFile            = 'help' + DirDelimiter + 'JCLHelp.hlp';
  JclHlpHelpFile               = 'help' + DirDelimiter + JclPackage + '.hlp';
  JclContainersHlpHelpFile     = 'help' + DirDelimiter + JclContainersPackage + '.hlp';
  JclDeveloperToolsHlpHelpFile = 'help' + DirDelimiter + JclDeveloperToolsPackage + '.hlp';
  JclVclHlpHelpFile            = 'help' + DirDelimiter + JclVclPackage + '.hlp';
  JclOldHxSHelpFile            = 'help' + DirDelimiter + 'JCLHelp.HxS';
  JclHxSHelpFile               = 'help' + DirDelimiter + JclPackage + '.HxS';
  JclContainersHxSHelpFile     = 'help' + DirDelimiter + JclContainersPackage + '.HxS';
  JclDeveloperToolsHxSHelpFile = 'help' + DirDelimiter + JclDeveloperToolsPackage + '.HxS';
  JclVclHxSHelpFile            = 'help' + DirDelimiter + JclVclPackage + '.HxS';

  JclHelp2NameSpace                = 'Jedi.Jcl';
  JclHelp2Collection               = 'JclHelp.HxC';
  JclHelp2Description              = 'JEDI Code Library';
  JclHelp2LangId                   = 1033;         // en/english
  JclOldHelp2Identifier            = 'JCLHelp';
  JclHelp2Identifier               = 'Jcl';
  JclContainersHelp2Identifier     = 'JclContainers';
  JclDeveloperToolsHelp2Identifier = 'JclDeveloperTools';
  JclVclHelp2Identifier            = 'JclVcl';
  JclOldHelp2HxSFile               = 'JCLHelp.HxS';
  JclOldHelp2HxIFile               = 'JCLHelp.HxI';
  JclHelp2HxSFile                  = 'Jcl.HxS';
  JclHelp2HxIFile                  = 'Jcl.HxI';
  JclContainersHelp2HxSFile        = 'JclContainers.HxS';
  JclContainersHelp2HxIFile        = 'JclContainers.HxI';
  JclDeveloperToolsHelp2HxSFile    = 'JclDeveloperTools.HxS';
  JclDeveloperToolsHelp2HxIFile    = 'JclDeveloperTools.HxI';
  JclVclHelp2HxSFile               = 'JclVcl.HxS';
  JclVclHelp2HxIFile               = 'JclVcl.HxI';

  JclHelpTitle                   = 'JCL %d.%d Help';
  JclHelpIndexName               = 'JEDI Code Library Reference';
  JclContainersHelpTitle         = 'JCL %d.%d Containers Help';
  JclContainersHelpIndexName     = 'JEDI Code Library Containers Reference';
  JclDeveloperToolsHelpTitle     = 'JCL %d.%d Developer Tools Help';
  JclDeveloperToolsHelpIndexName = 'JEDI Code Library Developer Tools Reference';
  JclVclHelpTitle                = 'JCL %d.%d VCL Help';
  JclVclHelpIndexName            = 'JEDI Code Library VCL Package Reference';
  HHFileName = 'HH.EXE';

  ReadmeFileName = 'Readme.txt';
  LicenseFileName = 'LICENSE.txt';

  DailyRevisionFileName = 'jcl-revision.txt';
  EntriesFileName1      = '.svn' + DirDelimiter + 'entries';
  EntriesFileName2      = '_svn' + DirDelimiter + 'entries';

  RsJclVersionMask     = 'JCL %d.%d %s %s %d';
  RsJclVersionBuild    = 'Build';
  RsJclVersionRevision = 'Revision';
  RsJclVersionTesting  = 'Testing';
  RsJclVersionRelease  = 'Release';

  {$IFDEF MSWINDOWS}
  Bcb2MakTemplate = 'packages\BCB.bmk';
  {$ENDIF MSWINDOWS}

  PathEnvironmentVar = 'PATH';
  RegHKCUEnvironmentVar = 'Environment';
  RegHKLMEnvironmentVar = 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';

  ProfilesSectionName = 'Profiles';

function FullPackageFileName(Target: TJclBorRADToolInstallation; const BaseName: string): string;
const
  S = 'packages' + VersionDir + DirDelimiter + '%s';
begin
  with Target do
  begin
    if SupportsLibSuffix then
      Result := Format(S + '%s', [VersionNumberStr, BaseName, PackageSourceFileExtension])
    else
      Result := Format(S + '%s0%3:s', [VersionNumberStr, BaseName, VersionNumberStr, PackageSourceFileExtension]);
  end;
end;

function FullLibraryFileName(Target: TJclBorRADToolInstallation; const BaseName: string): string;
const
  S = 'packages' + VersionDir + DirDelimiter + '%s';
begin
  with Target do
    if SupportsLibSuffix then
      Result := Format(S + 'DLL%s', [VersionNumberStr, BaseName, ProjectSourceFileExtension])
    else
      Result := Format(S + 'DLL%s0%3:s', [VersionNumberStr, BaseName, VersionNumberStr, ProjectSourceFileExtension]);
end;

function SortFileNameA2Z(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareText(List.Strings[Index1], List.Strings[Index2]);
end;

function SortFileNameZ2A(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := -CompareText(List.Strings[Index1], List.Strings[Index2]);
end;

//=== { TJclInstallation } ===================================================

constructor TJclInstallation.Create(JclDistribution: TJclDistribution;
  InstallTarget: TJclBorRADToolInstallation; ATargetPlatform: TJclBDSPlatform;
  const AGUIPage: IJediInstallPage);
begin
  inherited Create;

  FTarget := InstallTarget;
  if not Target.Valid then
    Abort;

  FDistribution := JclDistribution;
  FTargetPlatform := ATargetPlatform;
  FTargetName := Target.Name;

  if (Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber >= 9) then
    FIncludeFileName := PathAddSeparator(Distribution.JclIncludeDir) + Format(JclIncludeMask, [Target.IDEVersionNumberStr + AnsiLowerCase(GetPlatformStr)])
  else
    FIncludeFileName := PathAddSeparator(Distribution.JclIncludeDir) + Format(JclIncludeMask, [Target.IDEVersionNumberStr]);

  // exclude C#Builder 1 and Delphi 8 targets
  FRunTimeInstallation := (Target.RadToolKind <> brBorlandDevStudio)
    or ((Target.VersionNumber >= 3) and TargetSupportsDelphi);

  if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 9) then
  begin
    case TargetPlatform of
      bpWin32:
        FTargetName := Format('%s %s', [FTargetName, Personality32Bit]);
      bpWin64:
        FTargetName := Format('%s %s', [FTargetName, Personality64Bit]);
      bpOSX32:
        raise EJclBorRADException.CreateRes(@RsEOSXPlatformNotValid);
    else
      raise EJclBorRADException.CreateRes(@RsEPlatformNotValid);
    end;
  end;

  FLibReleaseDir := MakePath(Distribution.LibReleaseDirMask);
  FLibDebugDir := MakePath(Distribution.LibDebugDirMask);
  FJclDcpPath := PathAddSeparator(MakePath(Distribution.LibReleaseDirMask)); // packages are release

  FDemoSectionName := TargetName + ' demos';
  FLogFileName := Format('%sbin%s%s.log', [Distribution.JclPath, DirDelimiter, TargetName]);
  FLogLines := TJclSimpleLog.Create(FLogFileName);

  FProfilesTargets := TObjectList.Create;
  FProfilesTargets.Count := InstallCore.ProfilesManager.ProfileCount;
  FProfilesTargets.OwnsObjects := False;
end;

destructor TJclInstallation.Destroy;
var
  Index: Integer;
begin
  if Assigned(FProfilesTargets) then
    for Index := 0 to FProfilesTargets.Count - 1 do
      if FProfilesTargets.Items[Index] <> Target then
        FProfilesTargets.Items[Index].Free;
  FProfilesTargets.Free;
  FDemoList.Free;
  FLogLines.Free;
  FGUI := nil;
  FGUIPage := nil;

  inherited Destroy;
end;

function TJclInstallation.GetEnabled: Boolean;
begin
  Result := OptionCheckedById[OptionData[joJediCodeLibrary].Id];
end;

function TJclInstallation.GetIsProfileEnabled(Index: Integer): Boolean;
var
  AProfilesPage: IJediProfilesPage;
  ASettings: IJediConfiguration;
begin
  AProfilesPage := FDistribution.ProfilesPage;
  ASettings := InstallCore.Configuration;
  if AProfilesPage <> nil then
    Result := AProfilesPage.IsProfileEnabled[Index]
  else
  if ASettings <> nil then
    Result := ASettings.OptionAsBoolByName[ProfilesSectionName, InstallCore.ProfilesManager.ProfileNames[Index]]
  else
    Result := True;
end;

function TJclInstallation.GetOptionChecked(Option: TInstallerOption): Boolean;
begin
  Result := OptionCheckedById[OptionData[Option].Id];
end;

function TJclInstallation.GetOptionCheckedById(Id: Integer): Boolean;
var
  AConfiguration: IJediConfiguration;
begin
  if Assigned(GUIPage) then
    Result := GUIPage.OptionChecked[Id]
  else
  begin
    AConfiguration := InstallCore.Configuration;
    if Assigned(AConfiguration) then
      Result := AConfiguration.OptionAsBool[TargetName, Id]
    else
      Result := False;
  end;
end;

function TJclInstallation.GetPlatformStr: string;
begin
  case FTargetPlatform of
    bpWin32:
      Result := 'Win32';
    bpWin64:
      Result := 'Win64';
    bpOSX32:
      raise EJclBorRADException.CreateRes(@RsEOSXPlatformNotValid);
  else
    raise EJclBorRADException.CreateRes(@RsEPlatformNotValid);
  end;
end;

function TJclInstallation.GetProfilesTarget(Index: Integer): TJclBorRADToolInstallation;
{$IFDEF MSWINDOWS}
var
  RootKey: LongWord;
begin
  if FProfilesTargets.Items[Index] = nil then
  begin
    RootKey := InstallCore.ProfilesManager.GetProfileKey(Index);
    if RootKey <> HKCU then
    begin
      FProfilesTargets.Items[Index] := TJclBorRADToolInstallationClass(Target.ClassType).Create(Target.ConfigDataLocation, RootKey);
      TJclBorRADToolInstallation(FProfilesTargets.Items[Index]).OutputCallback := Target.OutputCallback;
    end
    else
      FProfilesTargets.Items[Index] := Target;
  end;
  Result := FProfilesTargets.Items[Index] as TJclBorRADToolInstallation;
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
begin
  Result := nil;
end;
{$ENDIF LINUX}

function TJclInstallation.GetTargetSupportsCBuilder: Boolean;
begin
  Result := ((bpBCBuilder32 in Target.Personalities) and (TargetPlatform = bpWin32)) or
            ((bpBCBuilder64 in Target.Personalities) and (TargetPlatform = bpWin64));
  if Result then
  begin
    // If we don't have a command line C++ compiler we can't compile
    // (fake BCB Personality from the Web Installer)
    if TargetPlatform = bpWin32 then
      Result := clBcc32 in Target.CommandLineTools
    else if TargetPlatform = bpWin64 then
      Result := clBcc64 in Target.CommandLineTools;
  end;
end;

function TJclInstallation.GetTargetSupportsDelphi: Boolean;
begin
  Result := ((bpDelphi32 in Target.Personalities) and (TargetPlatform = bpWin32)) or
            ((bpDelphi64 in Target.Personalities) and (TargetPlatform = bpWin64));
  if Result then
  begin
    // If we don't have a command line C++ compiler we can't compile
    // (fake Delphi Personality from the Web Installer)
    if TargetPlatform = bpWin32 then
      Result := clDcc32 in Target.CommandLineTools
    else if TargetPlatform = bpWin64 then
      Result := clDcc64 in Target.CommandLineTools;
  end;
end;

procedure TJclInstallation.MarkOptionBegin(Id: Integer);
begin
  if Assigned(GUIPage) then
    GUIPage.MarkOptionBegin(Id);
  if Assigned(GUI) then
    GUI.Status := InstallCore.InstallOptionName[Id];
end;

procedure TJclInstallation.MarkOptionBegin(Option: TInstallerOption);
begin
  if Assigned(GUIPage) then
    GUIPage.MarkOptionBegin(OptionData[Option].Id);
  if Assigned(GUI) then
    GUI.Status := LoadResString(@OptionData[Option].Hint);
end;

procedure TJclInstallation.MarkOptionEnd(Id: Integer; Success: Boolean);
begin
  if Assigned(GUIPage) then
  begin
    GUIPage.MarkOptionEnd(Id, not Success);
    if Assigned(GUI) then
      GUI.Progress := Round(100 * (Distribution.NbInstalled + GUIPage.Progress / 100) / Distribution.NbEnabled);
  end;
end;

procedure TJclInstallation.MarkOptionEnd(Option: TInstallerOption; Success: Boolean);
begin
  if Assigned(GUIPage) then
  begin
    GUIPage.MarkOptionEnd(OptionData[Option].Id, not Success);
    if Assigned(GUI) then
      GUI.Progress := Round(100 * (Distribution.NbInstalled + GUIPage.Progress / 100) / Distribution.NbEnabled);
  end;
end;

procedure TJclInstallation.Init;
  procedure AddOption(Option: TInstallerOption; GUIOptions: TJediInstallGUIOptions;
    Parent: Integer; const Caption, Hint: string); overload;
  begin
    GUIPage.AddInstallOption(OptionData[Option].Id, GUIOptions, Caption, Hint, Parent);
  end;

  procedure AddOption(Option: TInstallerOption; GUIOptions: TJediInstallGUIOptions;
    Parent: Integer; Caption, Hint: PResStringRec); overload;
  begin
    AddOption(Option, GUIOptions, Parent, LoadResString(Caption), LoadResString(Hint));
  end;

  procedure AddOption(Option: TInstallerOption; GUIOptions: TJediInstallGUIOptions;
    Parent: Integer); overload;
  begin
    AddOption(Option, GUIOptions, Parent, OptionData[Option].Caption, OptionData[Option].Hint);
  end;

  procedure AddOption(Option: TInstallerOption; GUIOptions: TJediInstallGUIOptions;
    Parent: TInstallerOption); overload;
  begin
    AddOption(Option, GUIOptions, OptionData[Parent].Id, OptionData[Option].Caption, OptionData[Option].Hint);
  end;

  procedure AddDefOptions(Parent: TInstallerOption);
  begin
    AddOption(joJCLDefThreadSafe, [goChecked], Parent);
    AddOption(joJCLDefDropObsoleteCode, [goChecked], Parent);
    if (Target.RadToolKind <> brBorlandDevStudio) or (Target.IDEVersionNumber <> 3) then
      // Delphi 2005 has a compiler internal failure when compiling the JCL with UNITVERSIONING enabled
      AddOption(joJCLDefUnitVersioning, [{goChecked}], Parent); // UnitVersioning isn't threadsafe and causes crashes in COM-Servers

    AddOption(joJCLDefMath, [goChecked], Parent);
    AddOption(joJCLDefMathPrecSingle, [goRadioButton], joJCLDefMath);
    AddOption(joJCLDefMathPrecDouble, [goRadioButton], joJCLDefMath);
    AddOption(joJCLDefMathPrecExtended, [goRadioButton, goChecked], joJCLDefMath);
    AddOption(joJCLDefMathExtremeValues, [goChecked], joJCLDefMath);

    AddOption(joJCLDefContainer, [goChecked], Parent);
    if (Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber >= 6) then
    begin
      AddOption(joJCLDefContainerAnsiStr, [goRadioButton], joJCLDefContainer);
      AddOption(joJCLDefContainerWideStr, [goRadioButton], joJCLDefContainer);
      AddOption(joJCLDefContainerUnicodeStr, [goRadioButton, goChecked], joJCLDefContainer);
    end
    else
    begin
      AddOption(joJCLDefContainerAnsiStr, [goRadioButton, goChecked], joJCLDefContainer);
      AddOption(joJCLDefContainerWideStr, [goRadioButton], joJCLDefContainer);
      AddOption(joJCLDefContainerUnicodeStr, [goRadioButton], joJCLDefContainer);
    end;
    AddOption(joJCLDefContainerNoStr, [goRadioButton], joJCLDefContainer);

    {$IFDEF MSWINDOWS}
    // debug options
    AddOption(joJCLDefDebug, [goNoAutoCheck], Parent);
    AddOption(joJCLDefHookDllExceptions, [goNoAutoCheck], joJCLDefDebug);
    AddOption(joJCLDefDebugNoBinary, [goNoAutoCheck], joJCLDefDebug);
    AddOption(joJCLDefDebugNoTD32, [goNoAutoCheck], joJCLDefDebug);
    AddOption(joJCLDefDebugNoMap, [goNoAutoCheck], joJCLDefDebug);
    AddOption(joJCLDefDebugNoExports, [goNoAutoCheck], joJCLDefDebug);
    AddOption(joJCLDefDebugNoSymbols, [goNoAutoCheck], joJCLDefDebug);
    {$ENDIF MSWINDOWS}
  end;

  procedure AddWrapperOptions(Parent: TInstallerOption);
  begin
    // PCRE options
    AddOption(joJCLDefPCRE, [goChecked], Parent);
    if Target.RadToolKind = brBorlandDevStudio then
    begin
      AddOption(joJCLDefPCREStaticLink, [goRadioButton, goChecked], joJCLDefPCRE);
      AddOption(joJCLDefPCRELinkOnRequest, [goRadioButton], joJCLDefPCRE);
    end
    else
      AddOption(joJCLDefPCRELinkOnRequest, [goRadioButton, goChecked], joJCLDefPCRE);
    AddOption(joJCLDefPCRELinkDLL, [goRadioButton], joJCLDefPCRE);
    if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 8) then
      // Delphi XE and newer have a licensed version of JCL's pcre.pas named RegularExpressionsAPI
      AddOption(joJCLDefPCRERTL, [goRadioButton], joJCLDefPCRE);
    AddOption(joJCLDefPCRE8, [goChecked], joJCLDefPCRE);
    AddOption(joJCLDefPCRE16, [goStandaloneParent], joJCLDefPCRE);
    AddOption(joJCLDefPCREPrefer16, [], joJCLDefPCRE16);
    // BZip2 options
    AddOption(joJCLDefBZip2, [goChecked], Parent);
    AddOption(joJCLDefBZip2StaticLink, [goRadioButton, goChecked], joJCLDefBZip2);
    AddOption(joJCLDefBZip2LinkOnRequest, [goRadioButton], joJCLDefBZip2);
    AddOption(joJCLDefBZip2LinkDLL, [goRadioButton], joJCLDefBZip2);
    // ZLib options
    AddOption(joJCLDefZLib, [goChecked], Parent);
    AddOption(joJCLDefZLibStaticLink, [goRadioButton, goChecked], joJCLDefZLib);
    AddOption(joJCLDefZLibLinkOnRequest, [goRadioButton], joJCLDefZLib);
    AddOption(joJCLDefZLibLinkDLL, [goRadioButton], joJCLDefZLib);
    if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 9) then
      // Delphi XE2 ZLib is up-to-date and can directly be used by the JCL
      AddOption(joJCLDefZLibRTL, [goRadioButton], joJCLDefZLib);
    // Unicode options
    AddOption(joJCLDefUnicode, [goChecked], Parent);
    if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 6) then
      // Delphi 2009 and newer have unit "Character"
      AddOption(joJCLDefUnicodeRTLDatabase, [goRadioButton], joJCLDefUnicode);
    AddOption(joJCLDefUnicodeRawData, [goRadioButton, goChecked], joJCLDefUnicode);
    AddOption(joJCLDefUnicodeZLibData, [goRadioButton], joJCLDefUnicode);
    AddOption(joJCLDefUnicodeBZip2Data, [goRadioButton], joJCLDefUnicode);
    AddOption(joJCLDefUnicodeSilentFailure, [goChecked], joJCLDefUnicode);
    {$IFDEF MSWINDOWS}
    // Sevenzip options
    AddOption(joJCLDef7z, [goChecked], Parent);
    //AddOption(joJCLDef7zStaticLink, [goRadioButton], joDef7z);
    AddOption(joJCLDef7zLinkOnRequest, [goRadioButton, goChecked], joJCLDef7z);
    AddOption(joJCLDef7zLinkDLL, [goRadioButton], joJCLDef7z);
    {$ENDIF MSWINDOWS}
  end;

  procedure AddEnvOptions(Parent: TInstallerOption);
  begin
    AddOption(joJCLEnvLibPath, [goChecked], Parent);
    AddOption(joJCLEnvBrowsingPath, [goChecked], Parent);
    if not Target.IsTurboExplorer then
      AddOption(joJCLEnvDebugDCUPath, [goChecked], Parent);
    if (Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber >= 5) and TargetSupportsCBuilder then
      AddOption(joJCLEnvIncludePath, [goChecked], Parent);
  end;

  procedure AddMakeOptions(Parent: TInstallerOption);
  var
    HppDirectory: string;
  begin
    AddOption(joJCLMakeRelease, [goStandAloneParent, goExpandable, goChecked], Parent);
    AddOption(joJCLMakeDebug, [goStandAloneParent, goExpandable, goChecked], Parent);

    if TargetSupportsCBuilder then
    begin
      if (Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber >= 5) then
        HppDirectory := LoadResString(@RsCaptionHppDirectory)
      else
        HppDirectory := Target.VclIncludeDir[TargetPlatform];
      AddOption(joJCLCopyHppFiles, [goChecked], OptionData[joJCLMake].Id,
        Format(LoadResString(OptionData[joJCLCopyHppFiles].Caption), [HppDirectory]),
        LoadResString(OptionData[joJCLCopyHppFiles].Hint));
      AddOption(joJCLCheckHppFiles, [goChecked], joJCLMake);
    end;
  end;

  procedure AddHelpOptions(Parent: TInstallerOption);
  begin
    {$IFDEF MSWINDOWS}
    if Target.RadToolKind = brBorlandDevStudio then
    begin
      // TODO: expert help
      if (Target.VersionNumber >= 3) and (Distribution.JclHxSHelpFileName <> '') and (TargetPlatform = bpWin32) then
      begin
        AddOption(joJCLHelp, [goChecked], Parent);
        AddOption(joJCLhelpHxS, [goStandaloneParent,goChecked], joJCLHelp);
        AddOption(joJCLHelpHxSPlugin, [goNoAutoCheck], joJCLHelpHxS);
      end;
    end
    else
    begin
      if (Distribution.JclHlpHelpFileName <> '') or (Distribution.JclChmHelpFileName <> '') then
      begin
        AddOption(joJCLHelp, [goChecked], Parent);
        if Distribution.JclHlpHelpFileName <> '' then
          AddOption(joJCLHelpHlp, [goChecked], joJCLHelp);
        if Distribution.JclChmHelpFileName <> '' then
          AddOption(joJCLHelpChm, [goChecked], joJCLHelp);
      end;
    end;
    {$ENDIF MSWINDOWS}
  end;

  procedure AddRepositoryOptions(Parent: TInstallerOption);
  begin
    // BDS has an expert for objects in the repository
    if Target.RadToolKind <> brBorlandDevStudio then
    begin
      AddOption(joJCLExceptDlg, [], Parent);
      if Target.SupportsVCL then
      begin
        AddOption(joJCLExceptDlgVCL, [], joJCLExceptDlg);
        {$IFDEF MSWINDOWS}
        AddOption(joJCLExceptDlgVCLSnd, [], joJCLExceptDlg);
        {$ENDIF MSWINDOWS}
      end;
    end;
  end;

  procedure AddPackageOptions(Parent: TInstallerOption);
  var
    HppDirectory: string;
  begin
    if RunTimeInstallation and TargetSupportsCBuilder then
    begin
      if (Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber >= 5) then
        HppDirectory := LoadResString(@RsCaptionHppDirectory)
      else
        HppDirectory := Target.VclIncludeDir[TargetPlatform];

      if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 4) then
      begin
        AddOption(joJCLDualPackages, [goStandAloneParent, goChecked], Parent);
        AddOption(joJCLCopyPackagesHppFiles, [goChecked], OptionData[joJCLDualPackages].Id,
          Format(LoadResString(OptionData[joJCLCopyPackagesHppFiles].Caption), [HppDirectory]),
          LoadResString(OptionData[joJCLCopyPackagesHppFiles].Hint));
      end
      else
        AddOption(joJCLCopyPackagesHppFiles, [goChecked], OptionData[Parent].Id,
          Format(LoadResString(OptionData[joJCLCopyPackagesHppFiles].Caption), [HppDirectory]),
          LoadResString(OptionData[joJCLCopyPackagesHppFiles].Hint));
    end;

    AddOption(joJCLMapCreate, [goExpandable, goStandaloneParent, goNoAutoCheck], Parent);

    {$IFDEF MSWINDOWS}
    AddOption(joJCLJdbgCreate, [goExpandable, goStandaloneParent], joJCLMapCreate);
    AddOption(joJCLJdbgInsert, [goNoAutoCheck], joJCLMapCreate);
    AddOption(joJCLMapDelete, [goNoAutoCheck], joJCLMapCreate);

    {if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber = 3)
      and (Target.Edition = deStd) then
      CopyFakeXmlRtlPackage;
    TODO: CopyFakeXmlRtlPackage
    }
    {$ENDIF MSWINDOWS}
  end;

  procedure AddExpertOptions(Parent: TInstallerOption);
  {$IFDEF MSWINDOWS}
  var
    ExpertOptions: TJediInstallGUIOptions;
  {$ENDIF MSWINDOWS}
  begin
    // TODO :
    //  It has been reported that IDE experts don't work under Win98.
    //  Leave these options unchecked for Win9x/WinME until that has been examined.
    {$IFDEF MSWINDOWS}
    if IsWinNT then
      ExpertOptions := [goChecked]
    else
      ExpertOptions := [];

    AddOption(joJCLExperts, [goExpandable, goChecked], Parent);

    if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber <= 2) then
      // design packages are not loaded by C#Builder 1 and Delphi 8
      AddOption(joJCLExpertsDLL, [goRadioButton, goChecked], joJCLExperts)
    else
    if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 3) then
      // expert DLLs are unstable on Delphi 2005 and BDS 2006
      // (problems while adding menu items in menu not loaded yet)
      AddOption(joJCLExpertsDsgnPackages, [goRadioButton, goChecked], joJCLExperts)
    else
    begin
      AddOption(joJCLExpertsDLL, [goRadioButton], joJCLExperts);
      AddOption(joJCLExpertsDsgnPackages, [goRadioButton, goChecked], joJCLExperts);
    end;

    if RunTimeInstallation then
    begin
      AddOption(joJCLExpertDebug, ExpertOptions, joJCLExperts);
      AddOption(joJCLExpertAnalyzer, ExpertOptions, joJCLExperts);
      if Target.RadToolKind <> brBorlandDevStudio then
        AddOption(joJCLExpertUses, ExpertOptions, joJCLExperts);
      AddOption(joJCLExpertSimdView, ExpertOptions, joJCLExperts);
      AddOption(joJCLExpertRepository, ExpertOptions, joJCLExperts);
      // the Stack Tracer Viewer experts is for Delphi targets only
      if (Target.RadToolKind = brDelphi) or
        ((Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 3)) then
        AddOption(joJCLExpertStackTraceViewer, ExpertOptions, joJCLExperts);
    end;
    AddOption(joJCLExpertFavorite, ExpertOptions, joJCLExperts);
    AddOption(joJCLExpertVersionControl, [goNoAutoCheck], joJCLExperts);
    if (Target.RadToolKind <> brBorlandDevStudio) and (Target.VersionNumber <= 6) then
      AddOption(joJCLExpertThreadNames, ExpertOptions, joJCLExperts);
    {$ENDIF MSWINDOWS}
  end;

  procedure AddDemoNodes;
  var
    I: Integer;
    ADemoList: TStrings;
    DemoOption: Integer;
    FileName: string;
  begin
    AddOption(joJCLMakeDemos, [goNoAutoCheck], joJediCodeLibrary);
    ADemoList := GetDemoList;
    for I := 0 to ADemoList.Count - 1 do
    begin
      FileName := ExtractRelativePath(Distribution.JclExamplesDir, ADemoList.Strings[I]);
      DemoOption := InstallCore.AddInstallOption(FileName);
      ADemoList.Objects[I] := TObject(DemoOption);
      GUIPage.AddInstallOption(DemoOption, [], ExtractFileName(FileName), FileName, OptionData[joJCLMakeDemos].Id);
    end;
  end;

  procedure LoadStaticValues(AConfiguration: IJediConfiguration);
  var
    IncludeContent: AnsiString;
    DefineName: string;
    JppState: TPppState;
    JppParser: TJppParser;
    Option: TInstallerOption;
  begin
    if FileExists(IncludeFileName) then
    begin
      IncludeContent := FileToString(IncludeFileName);
      JppState := TPppState.Create;
      try
        JppState.Options := [poProcessDefines];
        JppParser := TJppParser.Create(string(IncludeContent), JppState);
        try
          JppParser.Parse;
        finally
          JppParser.Free;
        end;
        for Option := Low(JclDefineNames) to High(JclDefineNames) do
        begin
          DefineName := JclDefineNames[Option];
          if DefineName <> '' then
            AConfiguration.OptionAsBool[TargetName, OptionData[Option].Id] := JppState.Defines[DefineName] = ttDefined;
        end;
      finally
        JppState.Free;
      end;
    end;
  end;

  procedure LoadValues;
  var
    AConfiguration: IJediConfiguration;
    Option: TInstallerOption;
    Id, Index: Integer;
    StoredValue: string;
    ADemoList: TStrings;
    ResetDefaultValue: Boolean;
  begin
    AConfiguration := InstallCore.Configuration;
    if not Assigned(AConfiguration) then
      Exit;
    if AConfiguration.SectionExists(TargetName) then
    begin
      ResetDefaultValue := not AConfiguration.OptionAsBool[TargetName, OptionData[joJediCodeLibrary].Id];
      for Option := Low(TInstallerOption) to High(TInstallerOption) do
      begin
        Id := OptionData[Option].Id;
        if AConfiguration.ValueExists(TargetName, Id) then
          GUIPage.OptionChecked[Id] := AConfiguration.OptionAsBool[TargetName, Id]
        else
        if ResetDefaultValue then
          GUIPage.OptionChecked[Id] := False;
      end;
      if not ResetDefaultValue then
        // options in included files jcl/source/include/jclXX.inc overrides stored settings
        LoadStaticValues(AConfiguration);
    end
    else
    begin
      GUIPage.OptionChecked[OptionData[joJediCodeLibrary].Id] := True;
      // options in included files jcl/source/include/jclXX.inc overrides stored settings
      LoadStaticValues(AConfiguration);
    end;

    if not Target.IsTurboExplorer then
    begin
      if FRunTimeInstallation then
      begin
        ADemoList := GetDemoList;
        if AConfiguration.SectionExists(FDemoSectionName) then
          for Index := 0 to ADemoList.Count - 1 do
        begin
          Id := Integer(ADemoList.Objects[Index]);
          GUIPage.OptionChecked[Id] := AConfiguration.OptionAsBool[FDemoSectionName, Id];
        end;
      end;

      StoredValue := AConfiguration.OptionAsStringByName[TargetName, OptionNameBPLPath];
      if StoredValue = '' then
        StoredValue := Target.BPLOutputPath[FTargetPlatform];
      GUIPage.Directories[FGUIBPLPathIndex] := StoredValue;

      if Target.RadToolKind = brCppBuilder then
        StoredValue := AConfiguration.OptionAsStringByName[TargetName, OptionNameBPIPath]
      else
        StoredValue := AConfiguration.OptionAsStringByName[TargetName, OptionNameDCPPath];
      if StoredValue = '' then
        StoredValue := FJclDcpPath;
      GUIPage.Directories[FGUIDCPPathIndex] := StoredValue;

      if FGUIHPPPathIndex >= 0 then
      begin
        StoredValue := AConfiguration.OptionAsStringByName[TargetName, OptionNameHPPPath];
        if StoredValue = '' then
          StoredValue := Target.VclIncludeDir[FTargetPlatform];
        GUIPage.Directories[FGUIHPPPathIndex] := StoredValue;
      end;
    end;
  end;

begin
  FGUI := InstallCore.InstallGUI;
  if not Assigned(GUI) then
    Exit;

  FGUIPage := GUI.CreateInstallPage;
  GUIPage.Caption := TargetName;
  GUIPage.SetIcon(Target.IdeExeFileName);

  AddOption(joJediCodeLibrary, [goExpandable, goChecked], JediTargetOption);

  if RunTimeInstallation then
  begin
    // conditional defines
    AddOption(joJCLDef, [goExpandable, goChecked], OptionData[joJediCodeLibrary].Id);
    AddDefOptions(joJCLDef);

    // wrapper options
    AddOption(joJCLDefWrappers, [goChecked], OptionData[joJCLDef].Id);
    AddWrapperOptions(joJCLDefWrappers);

    AddOption(joJCLEnvironment, [goExpandable, goChecked], OptionData[joJediCodeLibrary].Id);
    AddEnvOptions(joJCLEnvironment);

    if not Target.IsTurboExplorer then
    begin
      AddOption(joJCLMake, [goExpandable, goChecked], OptionData[joJediCodeLibrary].Id);
      AddMakeOptions(joJCLMake);
    end;

    AddHelpOptions(joJediCodeLibrary);
    AddRepositoryOptions(joJediCodeLibrary);
  end;

  if not Target.IsTurboExplorer then
  begin
    AddOption(joJCLPackages, [goStandAloneParent, goExpandable, goChecked], joJediCodeLibrary);
    AddPackageOptions(joJCLPackages);

    {$IFDEF MSWINDOWS}
    if TargetPlatform = bpWin32 then //there is no 64-bit IDE yet
      AddExpertOptions(joJCLPackages);
    {$ENDIF MSWINDOWS}
    if RunTimeInstallation then
      AddDemoNodes;
  end;

  GUIPage.InitDisplay;

  if not Target.IsTurboExplorer then
  begin
    FGUIBPLPathIndex := GUIPage.AddDirectory(LoadResString(@RsCaptionBPLPath));

    if Target.RadToolKind = brCppBuilder then
      FGUIDCPPathIndex := GUIPage.AddDirectory(LoadResString(@RsCaptionBPIPath))
    else
      FGUIDCPPathIndex := GUIPage.AddDirectory(LoadResString(@RsCaptionDCPPath));

    if (Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber >= 5) and TargetSupportsCBuilder then
      FGUIHPPPathIndex := GUIPage.AddDirectory(LoadResString(@RsCaptionHPPPath))
    else
      FGUIHPPPathIndex := -1;
  end;

  LoadValues;
end;

function TJclInstallation.Install: Boolean;
var
  AProfilesManager: IJediProfilesManager;

  procedure WriteIntroduction;
  var
    Personality: TJclBorPersonality;
    Index: Integer;
  begin
    WriteLog(StrRepeat('=', 80));
    WriteLog(Distribution.Version);
    WriteLog('');
    WriteLog(StrPadRight(StrRepeat('=', 10) + TargetName, 80, '='));
    WriteLog('');
    WriteLog(LoadResString(@RsLogInstalledPersonalities));
    for Personality := Low(TJclBorPersonality) to High(TJclBorPersonality) do
      if Personality in Target.Personalities then
    begin
      WriteLog(JclBorPersonalityDescription[Personality]);
    end;
    WriteLog('');
    WriteLog(StrRepeat('=', 80));
    WriteLog('');
    WriteLog(LoadResString(@RsLogMultipleProfile));
    if AProfilesManager.MultipleProfileMode then
    begin
      for Index := 0 to AProfilesManager.ProfileCount - 1 do
        if IsProfileEnabled[Index] then
          WriteLog(AProfilesManager.ProfileNames[Index]);
    end
    else
      WriteLog(LoadResString(@RsLogSingleProfile));
    WriteLog('');
    WriteLog(StrRepeat('=', 80));
    WriteLog('');
  end;

  function CheckDirectories: Boolean;
    function CheckDirectory(const Directory: string; ErrorMessage: PResStringRec): Boolean;
    begin
      Result := DirectoryExists(Directory);
      if not Result then
      begin
        if not Assigned(GUI) then
          WriteLog(Format(LoadResString(ErrorMessage), [Directory]))
        else
        if GUI.Dialog(Format(LoadResString(@RsWarningCreatePath), ['BPL']), dtWarning, [drYes, drNo]) = drYes then
        begin
          Result := ForceDirectories(Directory);
          if not Result then
            GUI.Dialog(Format(LoadResString(@RsErrorCantCreatePath), [Directory]), dtError, [drCancel]);
        end;
      end;
    end;
  begin
    Result := True;

    {$IFDEF MSWINDOWS}
    if not OptionChecked[joJCLPackages] and Assigned(GUI) and not Target.IsTurboExplorer then
      Result := GUI.Dialog(LoadResString(@RsWarningPackageNodeNotSelected), dtConfirmation, [drYes, drNo]) = drYes;
    {$ENDIF MSWINDOWS}

    if Result and OptionChecked[joJCLPackages] then
    begin
      Result := CheckDirectory(GetBplPath, @RsLogInvalidBplPath)
        and CheckDirectory(GetDcpPath, @RsLogInvalidDcpPath);
      if OptionChecked[joJCLCopyHppFiles] or OptionChecked[joJCLCopyPackagesHppFiles] then
        Result := Result and CheckDirectory(GetHppPath, @RsLogInvalidHppPath);
    end;
  end;

  function SetStaticOptions: Boolean;

    function SaveDefines(Defines: TStrings): Boolean;
    var
      IncludeLine, Symbol: string;
      IncludeFile: TStrings;
      IndexLine, DefinePos, SymbolEnd: Integer;
      Defined, NotDefined: Boolean;
    const
      DefineText = '$DEFINE';
      NotDefineText = '.' + DefineText;
    begin
      WriteLog(LoadResString(@RsLogConditionalDefines));
      Result := True;
      try
        IncludeFile := TStringList.Create;
        try
          IncludeFile.LoadFromFile(Distribution.JclIncludeTemplate);
          WriteLog(Format(LoadResString(@RsLogLoadTemplate), [Distribution.JclIncludeTemplate]));

          for IndexLine := 0 to IncludeFile.Count - 1 do
          begin
            IncludeLine := IncludeFile.Strings[IndexLine];
            DefinePos := AnsiPos(DefineText, UpperCase(IncludeLine));
            if DefinePos > 1 then
            begin
              Defined := IncludeLine[DefinePos - 1] = '{';
              NotDefined := IncludeLine[DefinePos - 1] = '.';
              if Defined or NotDefined then
              begin
                Inc(DefinePos, Length(DefineText));
                while CharIsWhiteSpace(IncludeLine[DefinePos]) do
                  Inc(DefinePos);
                SymbolEnd := DefinePos;
                while CharIsValidIdentifierLetter(IncludeLine[SymbolEnd]) do
                  Inc(SymbolEnd);
                Symbol := Copy(IncludeLine, DefinePos, SymbolEnd - DefinePos);
                DefinePos := Defines.IndexOf(Symbol);

                if (DefinePos >= 0) and NotDefined then
                  IncludeLine := StringReplace(IncludeLine, NotDefineText, DefineText, [rfIgnoreCase]);
                if (DefinePos < 0) and Defined then
                  IncludeLine := StringReplace(IncludeLine, DefineText, NotDefineText, [rfIgnoreCase]);

                IncludeFile.Strings[IndexLine] := IncludeLine;
              end;
            end;
          end;
          IncludeFile.SaveToFile(IncludeFileName);
          WriteLog(Format(LoadResString(@RsLogSaveIncludeFile), [IncludeFileName]));
        finally
          IncludeFile.Free;
        end;
      except
        Result := False;
      end;
    end;

  var
    Option: TInstallerOption;
    Defines: TStrings;
  begin
    Defines := TStringList.Create;
    try
      if OptionChecked[joJCLDef] then
      begin
        MarkOptionBegin(joJCLDef);
        for Option := Low(JclDefineNames) to High(JclDefineNames) do
          if OptionChecked[Option] then
        begin
          MarkOptionBegin(Option);
          Defines.Add(JclDefineNames[Option]);
          MarkOptionEnd(Option, True);
        end;
        MarkOptionEnd(joJCLDef, True);
      end;
      if OptionChecked[joJCLMapCreate] then
      begin
        MarkOptionBegin(joJCLMapCreate);
        Target.MapCreate := True;
        MarkOptionEnd(joJCLMapCreate, True);
      end
      else
        Target.MapCreate := False;
      {$IFDEF MSWINDOWS}
      if OptionChecked[joJCLJdbgCreate] then
      begin
        MarkOptionBegin(joJCLJdbgCreate);
        Target.JdbgCreate := True;
        MarkOptionEnd(joJCLJdbgCreate, True);
      end
      else
        Target.JdbgCreate := False;
      if OptionChecked[joJCLJdbgInsert] then
      begin
        MarkOptionBegin(joJCLJdbgInsert);
        Target.JdbgInsert := True;
        MarkOptionEnd(joJCLJdbgInsert, True);
      end
      else
        Target.JdbgInsert := False;
      if OptionChecked[joJCLMapDelete] then
      begin
        MarkOptionBegin(joJCLMapDelete);
        Target.MapDelete := True;
        MarkOptionEnd(joJCLMapDelete, True);
      end
      else
        Target.MapDelete := False;
      if Target is TJclBDSInstallation then
      begin
        if OptionChecked[joJCLDualPackages] then
        begin
          MarkOptionBegin(joJCLDualPackages);
          TJclBDSInstallation(Target).DualPackageInstallation := True;
          if OptionChecked[joJCLCopyPackagesHppFiles] then
          begin
            MarkOptionBegin(joJCLCopyPackagesHppFiles);
            MarkOptionEnd(joJCLCopyPackagesHppFiles, True);
          end;
          MarkOptionEnd(joJCLDualPackages, True);
        end
        else
          TJclBDSInstallation(Target).DualPackageInstallation := False;
      end;
      {$ENDIF MSWINDOWS}

      // no conditional defines for C#Builder 1 and Delphi 8
      Result := ((Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber <= 2)) or SaveDefines(Defines);
    finally
      Defines.Free;
    end;
  end;

  function SetEnvironment(ATarget: TJclBorRADToolInstallation): Boolean;
  begin
    Result := True;
    if OptionChecked[joJCLEnvironment] then
    begin
      MarkOptionBegin(joJCLEnvironment);

      if OptionChecked[joJCLEnvLibPath] then
      begin
        MarkOptionBegin(joJCLEnvLibPath);
        Result := ATarget.AddToLibrarySearchPath(FLibReleaseDir, FTargetPlatform) and ATarget.AddToLibrarySearchPath(Distribution.JclIncludeDir, FTargetPlatform);
        if Result then
        begin
          WriteLog(Format(LoadResString(@RsLogAddLibrarySearchPath2), [FLibReleaseDir, Distribution.JclIncludeDir]));
          {$IFDEF MSWINDOWS}
          if (ATarget.RadToolKind = brBorlandDevStudio) and OptionChecked[joJCLDualPackages] and TargetSupportsCBuilder then
            with TJclBDSInstallation(ATarget) do
          begin
            Result := AddToCppSearchPath(FLibReleaseDir, FTargetPlatform) and AddToCppSearchPath(Distribution.JclIncludeDir, FTargetPlatform) and
                      ((IDEVersionNumber < 5) or AddToCppLibraryPath(FLibReleaseDir, FTargetPlatform));
            if Result then
              WriteLog(Format(LoadResString(@RsLogAddCppSearchPath2), [FLibReleaseDir, Distribution.JclIncludeDir]))
            else
              WriteLog(LoadResString(@RsLogFailedAddCppSearchPath));
          end;
          {$ENDIF MSWINDOWS}
          if ATarget.IsTurboExplorer then
          begin
            Result := ATarget.AddToLibrarySearchPath(Distribution.JclSourcePath, FTargetPlatform);
            if Result then
              WriteLog(Format(LoadResString(@RsLogAddLibrarySearchPath1), [Distribution.JclSourcePath]))
            else
              WriteLog(LoadResString(@RsLogFailedAddLibrarySearchPath));
          end;
        end
        else
          WriteLog(LoadResString(@RsLogFailedAddLibrarySearchPath));
        MarkOptionEnd(joJCLEnvLibPath, Result);
      end;

      if Result and OptionChecked[joJCLEnvBrowsingPath] then
      begin
        MarkOptionBegin(joJCLEnvBrowsingPath);
        Result := ATarget.AddToLibraryBrowsingPath(Distribution.JclSourcePath, FTargetPlatform);
        if Result then
        begin
          WriteLog(Format(LoadResString(@RsLogAddLibraryBrowsingPath), [Distribution.JclSourcePath]));
          {$IFDEF MSWINDOWS}
          if (ATarget.RadToolKind = brBorlandDevStudio) and TargetSupportsCBuilder and OptionChecked[joJCLDualPackages] then
            with TJclBDSInstallation(ATarget) do
          begin
            Result := AddToCppBrowsingPath(Distribution.JclSourcePath, FTargetPlatform);
            if Result then
              WriteLog(Format(LoadResString(@RsLogAddCppBrowsingPath), [Distribution.JclSourcePath]))
            else
              WriteLog(LoadResString(@RsLogFailedAddCppBrowsingPath));
          end;
          {$ENDIF MSWINDOWS}
        end
        else
          WriteLog(LoadResString(@RsLogFailedAddLibraryBrowsingPath));
        MarkOptionEnd(joJCLEnvBrowsingPath, Result);
      end;

      if Result and OptionChecked[joJCLEnvDebugDCUPath] then
      begin
        MarkOptionBegin(joJCLEnvDebugDCUPath);
        Result := ATarget.AddToDebugDCUPath(FLibDebugDir, FTargetPlatform);
        if Result then
          WriteLog(Format(LoadResString(@RsLogAddDebugDCUPath), [FLibDebugDir]))
        else
          WriteLog(LoadResString(@RsLogFailedAddDebugDCUPath));
        MarkOptionEnd(joJCLEnvDebugDCUPath, Result);
      end;

      if Result and OptionChecked[joJCLEnvIncludePath] then
      begin
        MarkOptionBegin(joJCLEnvIncludePath);
        if (OptionChecked[joJCLCopyHppFiles] or OptionChecked[joJCLCopyPackagesHppFiles]) and
           (GetHppPath <> ATarget.VclIncludeDir[TargetPlatform]) then
        begin
          Result := (ATarget as TJclBDSInstallation).AddToCppIncludePath(GetHppPath, TargetPlatform);
          if Result then
            WriteLog(Format(LoadResString(@RsLogAddIncludePath), [GetHppPath]))
          else
            WriteLog(LoadResString(@RsLogFailedAddIncludePath));
        end
        else
          WriteLog(Format(LoadResString(@RsLogIgnoreAddIncludePath), [GetHppPath]));
        MarkOptionEnd(joJCLEnvIncludePath, Result);
      end;

      MarkOptionEnd(joJCLEnvironment, Result);
    end;
  end;

  function MakeUnits: Boolean;
    function CheckHppFiles: Boolean;
    var
      SaveDir, Options: string;
      UnitList, CppFile: TStrings;
      I: Integer;
    begin
      SaveDir := GetCurrentDir;
      try
        SetCurrentDir(Distribution.JclPath);

        UnitList := TStringList.Create;
        CppFile := TStringList.Create;
        try
          for I := Low(JclSrcPaths) to High(JclSrcPaths) do
            BuildFileList(PathAddSeparator(JclSrcPaths[I]) + '*.pas', faAnyFile, UnitList, False);
          // these headers are known to fail
          UnitList.Delete(UnitList.IndexOf('JclDotNet.pas'));
          UnitList.Delete(UnitList.IndexOf('JclNTFS.pas'));
          UnitList.Delete(UnitList.IndexOf('mscorlib_TLB.pas'));
          if FTargetPlatform = bpWin64 then
          begin
            UnitList.Delete(UnitList.IndexOf('pcre.pas')); // compiler: 'PCRE not supported on WIN64: use standard header'
            UnitList.Delete(UnitList.IndexOf('JclPCRE.pas')); // uses pcre.pas => same "not supported" error
            UnitList.Delete(UnitList.IndexOf('JclStringLists.pas')); // uses JclPCRE.pas => same "not supported" error
          end;

          SetCurrentDir(Format('%sinstall%sHeaderTest', [Distribution.JclPath, DirDelimiter]));

          TStringList(UnitList).CustomSort(SortFileNameA2Z);
          CppFile.Clear;
          CppFile.Add('#pragma hdrstop');
          for I := 0 to UnitList.Count - 1 do
            CppFile.Add(Format('#include <%s>', [ChangeFileExt(UnitList.Strings[I], '.hpp')]));
          CppFile.SaveToFile('jcl_a2z.cpp');

          TStringList(UnitList).CustomSort(SortFileNameZ2A);
          CppFile.Clear;
          CppFile.Add('#pragma hdrstop');
          for I := 0 to UnitList.Count - 1 do
            CppFile.Add(Format('#include <%s>', [ChangeFileExt(UnitList.Strings[I], '.hpp')]));
          CppFile.SaveToFile('jcl_z2a.cpp');
        finally
          UnitList.Free;
          CppFile.Free;
        end;

        Target.BCC.Options.Clear;
        Target.BCC.Options.Add('-c'); // compile only
        if FTargetPlatform <> bpWin64 then
        begin
          Target.BCC.Options.Add('-Ve'); // compatibility
          Target.BCC.Options.Add('-b'); // enum to be at least 4 bytes
          Target.BCC.Options.Add('-k-'); // no standard stack frame
          Target.BCC.Options.Add('-X'); // no autodependencies
          Target.BCC.Options.Add('-a8'); // data alignment
          {$IFDEF MSWINDOWS}
          Target.BCC.Options.Add('-tWM'); // code format
          {$ELSE ~ MSWINDOWS}
          Target.BCC.Options.Add('-tC'); // code format
          {$ENDIF ~MSWINDOWS}
          Target.BCC.Options.Add('-w-par'); // warning
          Target.BCC.Options.Add('-w-aus'); // warning
          Target.BCC.AddPathOption('I', Format('%s%s%s%sinclude%s%s', [Distribution.JclSourcePath, DirSeparator, Target.RootDir, DirDelimiter, DirSeparator, Target.VclIncludeDir[FTargetPlatform]]));
          Target.BCC.AddPathOption('I', ExcludeTrailingPathDelimiter(GetHppPath));
        end
        else
        begin
          Target.BCC.Options.Add('-D _DEBUG');
          Target.BCC.Options.Add('-g');
          Target.BCC.Options.Add('-fno-limit-debug-info');
          Target.BCC.Options.Add('-fborland-extensions');
          Target.BCC.Options.Add('-nobuiltininc');
          Target.BCC.Options.Add('-fexceptions');
          Target.BCC.Options.Add('-fcxx-exceptions');
          Target.BCC.Options.Add('-mstackrealign');
          Target.BCC.Options.Add('-fno-spell-checking');
          Target.BCC.Options.Add('-fno-use-cxa-atexit');
          Target.BCC.Options.Add('-x c++');
          Target.BCC.Options.Add('-std=c++11');
          Target.BCC.Options.Add('-O0');
          Target.BCC.Options.Add('-tC');
          Target.BCC.Options.Add('-tM');

          Target.BCC.Options.Add('-I "' + Distribution.JclSourcePath + '"');
          Target.BCC.Options.Add('-I "' + Target.RootDir + '"');
          Target.BCC.Options.Add('-I "' + ExcludeTrailingPathDelimiter(GetHppPath) + '"');
          Target.BCC.Options.Add('-isystem "' + Target.VclIncludeDir[FTargetPlatform] + '"');
        end;
        Options := StringsToStr(Target.BCC.Options, NativeSpace);
        Result := Target.BCC.Execute(Options + ' "jcl_a2z.cpp"')
          and Target.BCC.Execute(Options + ' "jcl_z2a.cpp"');
      finally
        DeleteFile('jcl_a2z.cpp');
        DeleteFile('jcl_a2z.obj');
        DeleteFile('jcl_z2a.cpp');
        DeleteFile('jcl_z2a.obj');
        SetCurrentDir(SaveDir);
      end;
      if not Result and Assigned(GUI) then
        Result := GUI.Dialog(LoadResString(@RsHppCheckFailure), dtWarning, [drYes, drNo]) = drYes;
    end;
  var
    I: Integer;
  begin
    // As people may only buy Delphi (without C++ Builder), we must make sure that bcc(32-64) is available before accessing the property
    if FTargetPlatform = bpWin64 then
    begin
      if clBcc64 in Target.CommandLineTools then 
        Target.BCC := (Target as TJclBDSInstallation).BCC64
    end
    else if clBcc32 in Target.CommandLineTools then 
    begin
      Target.BCC := Target.BCC32;
    end;

    Result := True;
    if OptionChecked[joJCLMake] then
    begin
      MarkOptionBegin(joJCLMake);

      if OptionChecked[joJCLMakeRelease] then
      begin
        MarkOptionBegin(joJCLMakeRelease);
        for I := Low(JclSrcPaths) to High(JclSrcPaths) do
          Result := Result and CompileLibraryUnits(JclSrcPaths[I], False);
        MarkOptionEnd(joJCLMakeRelease, Result);
      end;

      if Result and OptionChecked[joJCLMakeDebug] then
      begin
        MarkOptionBegin(joJCLMakeDebug);
        for I := Low(JclSrcPaths) to High(JclSrcPaths) do
          Result := Result and CompileLibraryUnits(JclSrcPaths[I], True);
        MarkOptionEnd(joJCLMakeDebug, Result);
      end;

      if Result and OptionChecked[joJCLCheckHppFiles] then
      begin
        // Only check the HPP files if we have a C++ Compiler
        if FileExists(Target.BCC.BinDirectory + Target.BCC.GetExeName) then
        begin
          MarkOptionBegin(joJCLCheckHppFiles);
          WriteLog('Checking .hpp files');
          Result := Result and CheckHppFiles;
          MarkOptionEnd(joJCLCheckHppFiles, Result);
        end;
      end;

      MarkOptionEnd(joJCLMake, Result);
    end;
  end;

  function CompilePackages: Boolean;
  begin
    Result := True;
    if OptionChecked[joJCLPackages] then
    begin
      MarkOptionBegin(joJCLPackages);

      if (Target is TJclBDSInstallation) and (Target.IDEVersionNumber >= 9) and (FTargetPlatform = bpWin64) then
        Target.DCC := (Target as TJclBDSInstallation).DCC64
      else
        Target.DCC := Target.DCC32;
      Result := CompilePackage(FullPackageFileName(Target, JclPackage))
        and CompilePackage(FullPackageFileName(Target, JclContainersPackage))
        and CompilePackage(FullPackageFileName(Target, JclDeveloperToolsPackage));

      if Result and Target.SupportsVCL then
        Result := Result and CompilePackage(FullPackageFileName(Target, JclVclPackage));

      MarkOptionEnd(joJCLPackages, Result);
    end;
  end;

  function RegisterPackages(ATarget: TJclBorRADToolInstallation): Boolean;
  {$IFDEF MSWINDOWS}
  var
    PathEnvVar: string;
  {$ENDIF MSWINDOWS}
  begin
    {$IFDEF MSWINDOWS}
    InstallJediRegInformation(ATarget.ConfigDataLocation, 'JCL', Iff((Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber >= 9), GetPlatformStr, ''),
      Format('%d.%d.%d.%d', [JclVersionMajor, JclVersionMinor, JclVersionRelease, JclVersionBuild]),
      GetDcpPath, GetBplPath, Distribution.JclPath, ATarget.RootKey);

    PathEnvVar := RegReadStringDef(ATarget.RootKey, RegHKCUEnvironmentVar, PathEnvironmentVar, '');
    PathListIncludeItems(PathEnvVar, RegReadStringDef(HKLM, RegHKLMEnvironmentVar, PathEnvironmentVar, ''));
    ExpandEnvironmentVar(PathEnvVar);
    if (PathListItemIndex(PathEnvVar, GetBplPath) = -1) and (PathListItemIndex(PathEnvVar, PathAddSeparator(GetBplPath)) = -1)
      and Assigned(GUI) and (FTargetPlatform = bpWin32) and (GUI.Dialog(LoadResString(@RsWarningAddPathToEnvironment), dtWarning, [drYes, drNo]) = drYes) then
    begin
      PathEnvVar := RegReadStringDef(ATarget.RootKey, RegHKCUEnvironmentVar, PathEnvironmentVar, '');
      PathListIncludeItems(PathEnvVar, GetBplPath);
      RegWriteString(ATarget.RootKey, RegHKCUEnvironmentVar, PathEnvironmentVar, PathEnvVar);
    end;
    {$ENDIF MSWINDOWS}
    Result := True;
  end;

  {$IFDEF MSWINDOWS}
  function CompileExperts: Boolean;
  var
    Option: TInstallerOption;
    DLLExperts: Boolean;
  begin
    Result := True;
    if OptionChecked[joJCLExperts] then
    begin
      MarkOptionBegin(joJCLExperts);
      DLLExperts := False;
      // dual packages useless for experts
      if Target.RadToolKind = brBorlandDevStudio then
        TJclBDSInstallation(Target).DualPackageInstallation := False;
      for Option := Low(SupportedExperts) to High(SupportedExperts) do
        if OptionChecked[Option] then
      begin
        MarkOptionBegin(Option);
        if Option = joJCLExpertsDsgnPackages then
          // nothing, default value
        else
        if Option = joJCLExpertsDLL then
          DLLExperts := OptionChecked[Option]
        else
        if DLLExperts then
          Result := CompileExpert(FullLibraryFileName(Target, SupportedExperts[Option]))
        else
          Result := CompilePackage(FullPackageFileName(Target, SupportedExperts[Option]));
        MarkOptionEnd(Option, Result);
        if not Result then
          Break;
      end;
      MarkOptionEnd(joJCLExperts, Result);
    end;
  end;

  function RegisterExperts(ATarget: TJclBorRADToolInstallation): Boolean;
  var
    Option: TInstallerOption;
    DLLExperts: Boolean;
    ProjectFileName: string;
  begin
    Result := True;
    if OptionChecked[joJCLExperts] then
    begin
      MarkOptionBegin(joJCLExperts);
      DLLExperts := False;
      // dual packages useless for experts
      if ATarget.RadToolKind = brBorlandDevStudio then
        TJclBDSInstallation(ATarget).DualPackageInstallation := False;
      for Option := Low(SupportedExperts) to High(SupportedExperts) do
        if OptionChecked[Option] then
      begin
        MarkOptionBegin(Option);
        if Option = joJCLExpertsDsgnPackages then
          // nothing, default value
        else
        if Option = joJCLExpertsDLL then
          DLLExperts := OptionChecked[Option]
        else
        if DLLExperts then
        begin
          ProjectFileName := Distribution.JclPath + FullLibraryFileName(ATarget, SupportedExperts[Option]);
          Result := ATarget.RegisterExpert(ProjectFileName, GetBplPath, PathExtractFileNameNoExt(ProjectFileName));
        end
        else
        begin
          ProjectFileName := Distribution.JclPath + FullPackageFileName(ATarget, SupportedExperts[Option]);
          Result := ATarget.RegisterPackage(ProjectFileName, GetBplPath, PathExtractFileNameNoExt(ProjectFileName));
        end;
        MarkOptionEnd(Option, Result);
        if not Result then
          Break;
      end;
      MarkOptionEnd(joJCLExperts, Result);
    end;
  end;
  {$ENDIF MSWINDOWS}

  function InstallRepository: Boolean;
    function AddDialogToRepository(const DialogName: string;
      const DialogFileName: string; const DialogIconFileName: string;
      const Designer: string): Boolean;
    begin
      Result := True;
      try
        WriteLog(Format(LoadResString(@RsLogInstalling), [DialogName]));
        Target.Repository.AddObject(DialogFileName, BorRADToolRepositoryFormTemplate,
          Target.Repository.FindPage(LoadResString(@RsExceptDlgPage), 1), DialogName, DialogIconFileName,
          LoadResString(@RsExceptDlgDescription), LoadResString(@RsExceptDlgAuthor), BorRADToolRepositoryDesignerDfm);
        WriteLog('-> ' + DialogFileName);
        WriteLog('-> ' + DialogIconFileName);
        WriteLog(LoadResString(@RsLogDone));
      except
        Result := False;
      end;
    end;
  begin
    Result := True;
    if OptionChecked[joJCLExceptDlg] then
    begin
      MarkOptionBegin(joJCLExceptDlg);
      {$IFDEF MSWINDOWS}
      if OptionChecked[joJCLExceptDlgVCL] then
      begin
        MarkOptionBegin(joJCLExceptDlgVCL);
        Result := AddDialogToRepository(LoadResString(@RsExceptDlgVclName), Distribution.VclDialogFileName,
          Distribution.VclDialogIconFileName, BorRADToolRepositoryDesignerDfm);
        MarkOptionEnd(joJCLExceptDlgVCL, Result);
      end;
      if Result and OptionChecked[joJCLExceptDlgVCLSnd] then
      begin
        MarkOptionBegin(joJCLExceptDlgVCLSnd);
        Result := AddDialogToRepository(LoadResString(@RsExceptDlgVclSndName), Distribution.VclDialogSendFileName,
          Distribution.VclDialogSendIconFileName, BorRADToolRepositoryDesignerDfm);
        MarkOptionEnd(joJCLExceptDlgVCLSnd, Result);
      end;
      {$ENDIF MSWINDOWS}
      MarkOptionEnd(joJCLExceptDlg, Result);
    end;
  end;

  {$IFDEF MSWINDOWS}
  function InstallHelpFiles: Boolean;
    function AddHelpToIdeTools: Boolean;
      function AddFile(const Title, FileName: string): Boolean;
      var
        ToolsIndex: Integer;
        IdeTool: TJclBorRADToolIdeTool;
      begin
        Result := True;
        try
          IdeTool := Target.IdeTools;
          ToolsIndex := IdeTool.Count;
          IdeTool.Count := ToolsIndex + 4;

          IdeTool.Title[ToolsIndex] := Format(Title, [JclVersionMajor, JclVersionMinor]);
          IdeTool.Path[ToolsIndex] := HHFileName;
          IdeTool.Parameters[ToolsIndex] := StrDoubleQuote(FileName);
          IdeTool.WorkingDir[ToolsIndex] := Distribution.JclPath;
        except
          Result := False;
        end;
        if Result then
          WriteLog(Format(LoadResString(@RsLogAddIDETools), [FileName, Target.RADToolName]))
        else
          WriteLog(LoadResString(@RsLogFailedAddIDETools));
      end;
    begin
      Result := AddFile(JclHelpTitle, Distribution.JclChmHelpFileName) and
                AddFile(JclContainersHelpTitle, Distribution.JclContainersChmHelpFileName) and
                AddFile(JclDeveloperToolsHelpTitle, Distribution.JclDeveloperToolsChmHelpFileName) and
                AddFile(JclVclHelpTitle, Distribution.JclVclChmHelpFileName);
    end;

    function AddHelpToOpenHelp: Boolean;
      function AddFile(const FileName, IndexName: string): Boolean;
      begin
        Result := Target.OpenHelp.AddHelpFile(FileName, IndexName);
        if Result then
          WriteLog(Format(LoadResString(@RsLogAddOpenHelp), [FileName, Target.RADToolName]))
        else
          WriteLog(LoadResString(@RsLogFailedAddOpenHelp));
      end;
    begin
      Result := AddFile(Distribution.JclHlpHelpFileName, JclHelpIndexName) and
                AddFile(Distribution.JclContainersHlpHelpFileName, JclContainersHelpIndexName) and
                AddFile(Distribution.JclDeveloperToolsHlpHelpFileName, JclDeveloperToolsHelpIndexName) and
                AddFile(Distribution.JclVclHlpHelpFileName, JclVclHelpIndexName);
    end;

    function RegisterHelp2Files: Boolean;
    var
      JclNameSpace, JclCollection, JclDescription,
      JclIdentifier, JclHxSFile, JclHxIFile,
      JclContainersIdentifier, JclContainersHxSFile, JclContainersHxIFile,
      JclDeveloperToolsIdentifier, JclDeveloperToolsHxSFile, JclDeveloperToolsHxIFile,
      JclVclIdentifier, JclVclHxSFile, JclVclHxIFile: WideString;
      JclLangId: Integer;
    begin
      Result := True;
      if (Target.RadToolKind <> brBorlandDevStudio) or (Target.VersionNumber < 3) then
        Exit;

      WriteLog(LoadResString(@RsLogAddHelp2Files));

      // to avoid Write AV, data have to be copied in data segment
      JclNameSpace := JclHelp2NameSpace;
      JclCollection := JclHelp2Collection;
      JclDescription := JclHelp2Description;
      JclLangId := JclHelp2LangId;
      JclIdentifier := JclHelp2Identifier;
      JclHxSFile := JclHelp2HxSFile;
      JclHxIFile := JclHelp2HxIFile;
      JclContainersIdentifier := JclContainersHelp2Identifier;
      JclContainersHxSFile := JclContainersHelp2HxSFile;
      JclContainersHxIFile := JclContainersHelp2HxIFile;
      JclDeveloperToolsIdentifier := JclDeveloperToolsHelp2Identifier;
      JclDeveloperToolsHxSFile := JclDeveloperToolsHelp2HxSFile;
      JclDeveloperToolsHxIFile := JclDeveloperToolsHelp2HxIFile;
      JclVclIdentifier := JclVclHelp2Identifier;
      JclVclHxSFile := JclVclHelp2HxSFile;
      JclVclHxIFile := JclVclHelp2HxIFile;

      Distribution.RegHelpCreateTransaction;
      Distribution.RegHelpRegisterNameSpace(JclNameSpace, JclCollection, JclDescription);
      Distribution.RegHelpRegisterHelpFile(JclNameSpace, JclIdentifier, JclLangId, JclHxSFile, JclHxIFile);
      Distribution.RegHelpRegisterHelpFile(JclNameSpace, JclContainersIdentifier, JclLangId, JclContainersHxSFile, JclContainersHxIFile);
      Distribution.RegHelpRegisterHelpFile(JclNameSpace, JclDeveloperToolsIdentifier, JclLangId, JclDeveloperToolsHxSFile, JclDeveloperToolsHxIFile);
      Distribution.RegHelpRegisterHelpFile(JclNameSpace, JclVclIdentifier, JclLangId, JclVclHxSFile, JclVclHxIFile);
      if OptionChecked[joJCLHelpHxSPlugin] then
      begin
        MarkOptionBegin(joJCLHelpHxSPlugin);
        Distribution.RegHelpPlugNameSpaceIn(JclNameSpace, TJclBDSInstallation(Target).Help2Manager.IdeNamespace);
        MarkOptionEnd(joJCLHelpHxSPlugin, Result);
      end;

      Distribution.RegHelpCommitTransaction;

      WriteLog(LoadResString(@RsLogDefered));
    end;
  begin
    Result := True;
    if OptionChecked[joJCLHelp] then
    begin
      MarkOptionBegin(joJCLHelp);

      if OptionChecked[joJCLHelpHlp] then
      begin
        MarkOptionBegin(joJCLHelpHlp);
        Result := AddHelpToOpenHelp;
        MarkOptionEnd(joJCLHelpHlp, Result);
      end;

      if Result and OptionChecked[joJCLHelpChm] then
      begin
        MarkOptionBegin(joJCLHelpChm);
        Result := AddHelpToIdeTools;
        MarkOptionEnd(joJCLHelpChm, Result);
      end;

      if Result and OptionChecked[joJCLHelpHxS] then
      begin
        MarkOptionBegin(joJCLHelpHxS);
        Result := RegisterHelp2Files;
        MarkOptionEnd(joJCLHelpHxS, Result);
      end;

      MarkOptionEnd(joJCLHelp, Result);
    end;
  end;
  {$ENDIF MSWINDOWS}

  function MakeDemos: Boolean;
  var
    SaveDir: string;
    Index, ID: Integer;
    ADemoList: TStrings;
    DemoResult: Boolean;
  begin
    Result := True;
    if OptionChecked[joJCLMakeDemos] then
    begin
      MarkOptionBegin(joJCLMakeDemos);
      SaveDir := GetCurrentDir;
      try
        ADemoList := GetDemoList;
        for Index := 0 to ADemoList.Count - 1 do
        begin
          ID := Integer(ADemoList.Objects[Index]);
          if OptionCheckedById[ID] then
          begin
            MarkOptionBegin(ID);
            DemoResult := CompileApplication(ADemoList.Strings[Index]);
            MarkOptionEnd(ID, DemoResult);
            // ahuser: The installation shouldn't fail if some demos can't be compiled like
            //         outdated demos. Otherwise the JVCL Installer will
            //         have a hard time finding a valid JCL installation
            //Result := Result and DemoResult;
          end;
        end;
      finally
        SetCurrentDir(SaveDir);
      end;

      MarkOptionEnd(joJCLMakeDemos, Result);
    end;
  end;

var
  Index: Integer;
  ATarget: TJclBorRADToolInstallation;
begin
  Result := False;
  FInstallSuccess := False;
  FLogLines.OpenLog;
  AProfilesManager := InstallCore.ProfilesManager;
  try
    Target.OutputCallback := WriteLog;

    if Assigned(GUI) then
      GUI.Status := Format(LoadResString(@RsLogInstallingJCL), [TargetName]);

    if Assigned(GUIPage) then
    begin
      GUIPage.Show;
      GUIPage.BeginInstall;
    end;

    FLogLines.ClearLog;

    WriteIntroduction;
    Result := CheckDirectories and SetStaticOptions and MakeUnits and CompilePackages and InstallRepository
      and MakeDemos {$IFDEF MSWINDOWS}and CompileExperts and InstallHelpFiles{$ENDIF MSWINDOWS};
    if Result then
    begin
      if AProfilesManager.MultipleProfileMode then
      begin
        for Index := 0 to AProfilesManager.ProfileCount - 1 do
          if IsProfileEnabled[Index] then
        begin
          ATarget := ProfileTargets[Index];
          if ATarget.Valid then
          begin
            WriteLog(StrPadRight(StrRepeat('=', 10) + InstallCore.ProfilesManager.ProfileNames[Index], 80, '='));
            Result := Result and SetEnvironment(ATarget) and RegisterPackages(ATarget)
              {$IFDEF MSWINDOWS}and RegisterExperts(ATarget){$ENDIF MSWINDOWS};
          end;
        end;
      end
      else
        Result := Result and SetEnvironment(Target) and RegisterPackages(Target)
          {$IFDEF MSWINDOWS}and RegisterExperts(Target){$ENDIF MSWINDOWS};
    end;

    if not Result then
    begin
      Silent := True;
      Uninstall(False);
    end;

    FLogLines.CloseLog;
  finally
    FInstallSuccess := Result;
    Target.OutputCallback := nil;
    WriteLog('');
    if Assigned(GUIPage) then
      GUIPage.EndInstall;
    FLogLines.CloseLog;
  end;
end;

function TJclInstallation.MakePath(const FormatStr: string): string;
var
  VersionStr: string;
begin
  VersionStr := Target.VersionNumberStr;
  if (Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber >= 9) then
    VersionStr := VersionStr + DirDelimiter + AnsiLowerCase(GetPlatformStr);
  Result := Format(FormatStr, [VersionStr]);
  {$IFDEF MSWINDOWS}
  if (Target.RadToolKind <> brBorlandDevStudio) or (Target.VersionNumber < 3) then
    Result := PathGetShortName(Result);
  {$ENDIF MSWINDOWS}
end;

function TJclInstallation.RemoveSettings: Boolean;
{$IFDEF MSWINDOWS}
var
  JclSettingsKey: string;
{$ENDIF MSWINDOWS}
begin
{$IFDEF MSWINDOWS}
  JclSettingsKey := Target.ConfigDataLocation + '\Jedi\JCL';
  if (Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber >= 9) then
    JclSettingsKey := JclSettingsKey + '\' + GetPlatformStr;
  if RegKeyExists(HKCU, JclSettingsKey) then
    Result := RegDeleteKeyTree(HKCU, JclSettingsKey)
  else
{$ENDIF MSWINDOWS}
    Result := True;
end;

function TJclInstallation.Uninstall(AUninstallHelp: Boolean): Boolean;
  procedure RemoveEnvironment(ATarget: TJclBorRADToolInstallation);
  begin
    //ioJclEnvLibPath
    if ATarget.RemoveFromLibrarySearchPath(FLibReleaseDir, FTargetPlatform) and
       ATarget.RemoveFromLibrarySearchPath(Distribution.JclIncludeDir, FTargetPlatform) then
      WriteLog(Format(LoadResString(@RsLogDelLibrarySearchPath2), [FLibReleaseDir, Distribution.JclIncludeDir]))
    else
      WriteLog(LoadResString(@RsLogFailedDelLibrarySearchPath));
    {$IFDEF MSWINDOWS}
    if (ATarget.RadToolKind = brBorlandDevStudio) and TargetSupportsCBuilder then
      with TJclBDSInstallation(ATarget) do
    begin
      if RemoveFromCppSearchPath(FLibReleaseDir, FTargetPlatform) and
         RemoveFromCppSearchPath(Distribution.JclIncludeDir, FTargetPlatform) and
         ((IDEVersionNumber < 5) or RemoveFromCppLibraryPath(FLibReleaseDir, FTargetPlatform)) then
        WriteLog(Format(LoadResString(@RsLogDelCppSearchPath2), [FLibReleaseDir, Distribution.JclIncludeDir]))
      else
        WriteLog(LoadResString(@RsLogFailedDelCppSearchPath));
    end;
    {$ENDIF MSWINDOWS}

    //ioJclEnvBrowsingPath
    if ATarget.RemoveFromLibraryBrowsingPath(Distribution.JclSourcePath, FTargetPlatform) then
      WriteLog(Format(LoadResString(@RsLogDelLibraryBrowsingPath), [Distribution.JclSourcePath]))
    else
      WriteLog(LoadResString(@RsLogFailedDelLibraryBrowsingPath));
    if ATarget.RemoveFromLibraryBrowsingPath(Distribution.JclOldSourcePath, FTargetPlatform) then
      WriteLog(Format(LoadResString(@RsLogDelLibraryBrowsingPath), [Distribution.JclOldSourcePath]))
    else
      WriteLog(LoadResString(@RsLogFailedDelLibraryBrowsingPath));
    {$IFDEF MSWINDOWS}
    if (ATarget.RadToolKind = brBorlandDevStudio) and TargetSupportsCBuilder then
      with TJclBDSInstallation(ATarget) do
    begin
      if RemoveFromCppBrowsingPath(Distribution.JclSourcePath, FTargetPlatform) then
        WriteLog(Format(LoadResString(@RsLogDelCppBrowsingPath), [Distribution.JclSourcePath]))
      else
        WriteLog(LoadResString(@RsLogFailedDelCppBrowsingPath));
      if RemoveFromCppBrowsingPath(Distribution.JclOldSourcePath, FTargetPlatform) then
        WriteLog(Format(LoadResString(@RsLogDelCppBrowsingPath), [Distribution.JclOldSourcePath]))
      else
        WriteLog(LoadResString(@RsLogFailedDelCppBrowsingPath));
    end;
    {$ENDIF MSWINDOWS}

    //ioJclEnvDebugDCUPath
    if ATarget.RemoveFromDebugDCUPath(FLibDebugDir, FTargetPlatform) then
      WriteLog(Format(LoadResString(@RsLogDelDebugDCUPath), [FLibDebugDir]))
    else
    begin
      if ATarget.RemoveFromDebugDCUPath(FLibDebugDir, FTargetPlatform) then
        WriteLog(Format(LoadResString(@RsLogDelDebugDCUPath), [FLibDebugDir]))
      else
        WriteLog(LoadResString(@RsLogFailedDelDebugDCUPath));
    end;

    //joJclEnvIncludePath
    if (Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber >= 5) and TargetSupportsCBuilder then
    begin
      if (GetHppPath <> ATarget.VclIncludeDir[TargetPlatform]) then
      begin
        if (ATarget as TJclBDSInstallation).RemoveFromCppIncludePath(GetHppPath, TargetPlatform) then
          WriteLog(Format(LoadResString(@RsLogDelIncludePath), [GetHppPath]))
        else
          WriteLog(LoadResString(@RsLogFailedDelIncludePath));
      end
      else
        WriteLog(Format(LoadResString(@RsLogIgnoreDelIncludePath), [GetHppPath]));
    end;
  end;

  procedure RemoveMake;
    procedure RemoveFileMask(const Directory, Extension: string);
    var
      FileList: TStrings;
      Index: Integer;
    begin
      FileList := TStringList.Create;
      try
        BuildFileList(Format('%s*%s', [PathAddSeparator(Directory), Extension]), faAnyFile, FileList);
        for Index := 0 to FileList.Count - 1 do
          FileDelete(PathAddSeparator(Directory) + FileList.Strings[Index]);
      finally
        FileList.Free;
      end;
    end;
  begin
    RemoveFileMask(FLibReleaseDir, '.dcu');
    RemoveFileMask(FLibDebugDir, '.dcu');
    if TargetSupportsCBuilder then
    begin
      RemoveFileMask(FLibReleaseDir, '.obj'); // compatibility
      RemoveFileMask(FLibDebugDir, '.obj'); // compatibility
    end;
    //ioJclCopyHppFiles: ; // TODO : Delete copied files
    //ioJclCheckHppFiles: ; // nothing to do
  end;

  procedure UnregisterPackages(ATarget: TJclBorRADToolInstallation);
  var
    ABDSTarget: TJclBDSInstallation;
  begin
    if ATarget.RadToolKind = brBorlandDevStudio then
    begin
      ABDSTarget := ATarget as TJclBDSInstallation;
      ABDSTarget.CleanPackageCache(BinaryFileName(GetBPLPath, Distribution.JclPath + FullPackageFileName(ATarget, JclPackage)));
      ABDSTarget.CleanPackageCache(BinaryFileName(GetBPLPath, Distribution.JclPath + FullPackageFileName(ATarget, JclContainersPackage)));
      ABDSTarget.CleanPackageCache(BinaryFileName(GetBPLPath, Distribution.JclPath + FullPackageFileName(ATarget, JclDeveloperToolsPackage)));
      if RuntimeInstallation and ATarget.SupportsVCL then
        ABDSTarget.CleanPackageCache(BinaryFileName(GetBPLPath, Distribution.JclPath + FullPackageFileName(ATarget, JclVclPackage)));
    end;
    //ioJclPackages
    ATarget.UnregisterPackage(Distribution.JclPath + FullPackageFileName(ATarget, JclPackage), GetBplPath);
    ATarget.UnregisterPackage(Distribution.JclPath + FullPackageFileName(ATarget, JclContainersPackage), GetBplPath);
    ATarget.UnregisterPackage(Distribution.JclPath + FullPackageFileName(ATarget, JclDeveloperToolsPackage), GetBplPath);
    if RuntimeInstallation and ATarget.SupportsVCL then
      ATarget.UnregisterPackage(Distribution.JclPath + FullPackageFileName(ATarget, JclVclPackage), GetBplPath);
    {$IFDEF MSWINDOWS}
    RemoveJediRegInformation(Target.ConfigDataLocation, 'JCL', Iff((Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber >= 9), GetPlatformStr, ''), ATarget.RootKey);
    {$ENDIF MSWINDOWS}
  end;

  procedure DeletePackages;
  begin
    DeletePackage(FullPackageFileName(Target, JclPackage));
    DeletePackage(FullPackageFileName(Target, JclContainersPackage));
    DeletePackage(FullPackageFileName(Target, JclDeveloperToolsPackage));
    if RuntimeInstallation and Target.SupportsVCL then
      DeletePackage(FullPackageFileName(Target, JclVclPackage));
  end;
  {$IFDEF MSWINDOWS}
  procedure UnregisterExperts(ATarget: TJclBorRADToolInstallation);
    procedure UnregisterExpert(const Name: string);
    var
      Index: Integer;
      FileName, ShortFileName: string;
    begin
      for Index := ATarget.IdePackages.Count - 1 downto 0 do
      begin
        FileName := ATarget.IdePackages.PackageFileNames[Index];
        ShortFileName := ChangeFileExt(ExtractFileName(FileName), '');
        if StrMatches(Name, ShortFileName)
          or StrMatches(Format('%sDLL%s', [Name, StrUpper(ATarget.VersionNumberStr)]), ShortFileName)
          or StrMatches(Format('%sDLL%d', [Name, ATarget.VersionNumber]), ShortFileName)
          or StrMatches(Format('%sDLL%s0', [Name, StrUpper(ATarget.VersionNumberStr)]), ShortFileName)
          or StrMatches(Format('%sDLL%d0', [Name, ATarget.VersionNumber]), ShortFileName) then
          ATarget.UnregisterPackage(FileName);
      end;
      for Index := ATarget.IdePackages.ExpertCount - 1 downto 0 do
      begin
        FileName := ATarget.IdePackages.ExpertFileNames[Index];
        ShortFileName := ChangeFileExt(ExtractFileName(FileName), '');
        if StrMatches(Name, ShortFileName)
          or StrMatches(Format('%sDLL%s', [Name, StrUpper(ATarget.VersionNumberStr)]), ShortFileName)
          or StrMatches(Format('%sDLL%d', [Name, ATarget.VersionNumber]), ShortFileName)
          or StrMatches(Format('%sDLL%s0', [Name, StrUpper(ATarget.VersionNumberStr)]), ShortFileName)
          or StrMatches(Format('%sDLL%d0', [Name, ATarget.VersionNumber]), ShortFileName) then
          ATarget.UnregisterExpert(FileName);
      end;
    end;
  var
    Option: TInstallerOption;
    IndexOldExpert: Integer;
  begin
    for Option := Low(SupportedExperts) to High(SupportedExperts) do
      if not (Option in [joJCLExpertsDsgnPackages, joJCLExpertsDLL]) then
        UnregisterExpert(SupportedExperts[Option]);
    for IndexOldExpert := Low(OldExperts) to High(OldExperts) do
      UnregisterExpert(OldExperts[IndexOldExpert]);
  end;

  procedure DeleteExperts;
  var
    Option: TInstallerOption;
    ProjectFileName: string;
  begin
    for Option := Low(SupportedExperts) to High(SupportedExperts) do
      if not (Option in [joJCLExpertsDsgnPackages, joJCLExpertsDLL]) then
    begin
      ProjectFileName := Distribution.JclPath + FullPackageFileName(Target, SupportedExperts[Option]);
      if FileExists(ProjectFileName) then
        Target.UninstallPackage(ProjectFileName, GetBplPath, GetDcpPath);
      ProjectFileName := Distribution.JclPath + FullLibraryFileName(Target, SupportedExperts[Option]);
      if FileExists(ProjectFileName) then
        Result := FileDelete(BinaryFileName(GetBplPath, ProjectFileName));
    end;
  end;

  procedure UninstallHelp;
    procedure RemoveHelpFromIdeTools;
      procedure RemoveIdeTool(const Title: string);
      var
        HelpIndex: Integer;
      begin
        HelpIndex := Target.IdeTools.IndexOfTitle(Title);
        if HelpIndex <> -1 then
        begin
          Target.IdeTools.RemoveIndex(HelpIndex);
          WriteLog(Format(LoadResString(@RsLogDelIdeTools), [Title, Target.RADToolName]))
        end
        else
          WriteLog(LoadResString(@RsLogFailedDelIdeTools));
      end;
    begin
      RemoveIdeTool(Format(JclHelpTitle, [JclVersionMajor, JclVersionMinor]));
      RemoveIdeTool(Format(JclContainersHelpTitle, [JclVersionMajor, JclVersionMinor]));
      RemoveIdeTool(Format(JclDeveloperToolsHelpTitle, [JclVersionMajor, JclVersionMinor]));
      RemoveIdeTool(Format(JclVclHelpTitle, [JclVersionMajor, JclVersionMinor]));
    end;

    procedure RemoveHelpFromOpenHelp;
      procedure RemoveFile(const FileName, IndexName: string);
      begin
        if Target.OpenHelp.RemoveHelpFile(FileName, IndexName) then
          WriteLog(Format(LoadResString(@RsLogDelOpenHelp), [FileName, Target.RADToolName]))
        else
          WriteLog(LoadResString(@RsLogFailedDelOpenHelp));
      end;
    begin
      RemoveFile(Distribution.JclOldHlpHelpFileName, JclHelpIndexName);
      RemoveFile(Distribution.JclHlpHelpFileName, JclHelpIndexName);
      RemoveFile(Distribution.JclContainersHlpHelpFileName, JclContainersHelpIndexName);
      RemoveFile(Distribution.JclDeveloperToolsHlpHelpFileName, JclDeveloperToolsHelpIndexName);
      RemoveFile(Distribution.JclVclHlpHelpFileName, JclVclHelpIndexName);
    end;

    procedure UnregisterHelp2Files;
    var
      JclNameSpace, JclOldIdentifier, JclIdentifier, JclContainersIdentifier,
      JclDeveloperToolsIdentifier, JclVclIdentifier: WideString;
      JclLangId: Integer;
    begin
      if (Target.RadToolKind <> brBorlandDevStudio) or (Target.VersionNumber < 3) then
        Exit;

      WriteLog(LoadResString(@RsLogDelHelp2Files));

      // to avoid Write AV, data has to be copied in data segment
      JclNameSpace := JclHelp2NameSpace;
      JclLangId := JclHelp2LangId;
      JclOldIdentifier := JclOldHelp2Identifier;
      JclIdentifier := JclHelp2Identifier;
      JclContainersIdentifier := JclContainersHelp2Identifier;
      JclDeveloperToolsIdentifier := JclDeveloperToolsHelp2Identifier;
      JclVclIdentifier := JclVclHelp2Identifier;

      Distribution.RegHelpCreateTransaction;
      Distribution.RegHelpUnPlugNameSpace(JclNameSpace, TJclBDSInstallation(Target).Help2Manager.IdeNamespace);
      Distribution.RegHelpUnregisterHelpFile(JclNameSpace, JclOldIdentifier, JclLangId);
      Distribution.RegHelpUnregisterHelpFile(JclNameSpace, JclIdentifier, JclLangId);
      Distribution.RegHelpUnregisterHelpFile(JclNameSpace, JclContainersIdentifier, JclLangId);
      Distribution.RegHelpUnregisterHelpFile(JclNameSpace, JclDeveloperToolsIdentifier, JclLangId);
      Distribution.RegHelpUnregisterHelpFile(JclNameSpace, JclVclIdentifier, JclLangId);
      Distribution.RegHelpUnregisterNameSpace(JclNameSpace);
      Distribution.RegHelpCommitTransaction;

      WriteLog(LoadResString(@RsLogDefered));
    end;

  begin
    if Target.RadToolKind <> brBorlandDevStudio then
    begin
      RemoveHelpFromOpenHelp;
      RemoveHelpFromIdeTools;
    end
    else
      UnregisterHelp2Files;
  end;
  {$ENDIF MSWINDOWS}
  procedure UninstallRepository;
    procedure RemoveDialogFromRepository(const DialogName, DialogFileName: string);
    begin
      Target.Repository.RemoveObjects(ExceptDlgPath, DialogFileName, BorRADToolRepositoryFormTemplate);
      WriteLog(Format(LoadResString(@RsLogUninstalling), [DialogName]));
    end;
  begin
    if Target.RadToolKind <> brBorlandDevStudio then
    begin
      {$IFDEF MSWINDOWS}
      // ioJclExcDialog
      // ioJclExcDialogVCL
      RemoveDialogFromRepository(LoadResString(@RsExceptDlgVclName), Distribution.VclDialogFileName);
      //ioJclExcDialogVCLSnd
      RemoveDialogFromRepository(LoadResString(@RsExceptDlgVclSndName), Distribution.VclDialogSendFileName);
      {$ENDIF MSWINDOWS}
    end;
  end;

var
  Index: Integer;
  AProfilesManager: IJediProfilesManager;
  ATarget: TJclBorRADToolInstallation;
begin
  FLogLines.OpenLog;
  AProfilesManager := InstallCore.ProfilesManager;
  try
    Target.OutputCallback := WriteLog;
    if Assigned(GUI) then
      GUI.Status := Format(LoadResString(@RsLogUninstallingJCL), [TargetName]);
    if Assigned(GUIPage) then
      GUIPage.Show;

    if AProfilesManager.MultipleProfileMode then
    begin
      for Index := 0 to AProfilesManager.ProfileCount - 1 do
        if IsProfileEnabled[Index] then
      begin
        ATarget := ProfileTargets[Index];
        if ATarget.Valid then
        begin
          RemoveEnvironment(ATarget);
          {$IFDEF MSWINDOWS}
          if not Target.IsTurboExplorer then
            UnregisterExperts(ATarget);
          {$ENDIF MSWINDOWS}
          if not Target.IsTurboExplorer then
            UnregisterPackages(ATarget);
        end;
      end;
    end
    else
    begin
      RemoveEnvironment(Target);
      {$IFDEF MSWINDOWS}
      if not Target.IsTurboExplorer then
        UnregisterExperts(Target);
      {$ENDIF MSWINDOWS}
      if not Target.IsTurboExplorer then
        UnregisterPackages(Target);
    end;

    RemoveMake;
    if not Target.IsTurboExplorer then
      DeletePackages;
    {$IFDEF MSWINDOWS}
    DeleteExperts;
    if AUninstallHelp then
      UninstallHelp;
    {$ENDIF MSWINDOWS}
    // TODO: ioJclCopyPackagesHppFiles
    UninstallRepository;
    // TODO: ioJclMakeDemos:
  finally
    Target.OutputCallback := nil;
    FLogLines.CloseLog;
  end;

  Result := True;
end;

procedure TJclInstallation.WriteLog(const Msg: string);
var
  Line: string;
  LineType: TCompileLineType;
begin
  if not Silent then
  begin
    Line := InstallCore.ProcessLogLine(Msg, LineType, GUIPage);
    if Line <> '' then
      FLogLines.Write(Line);
  end;
end;

function TJclInstallation.GetBplPath: string;
var
  AConfiguration: IJediConfiguration;
begin
  if Assigned(GUIPage) then
    Result := GUIPage.Directories[FGUIBPLPathIndex]
  else
  begin
    AConfiguration := InstallCore.Configuration;
    if Assigned(AConfiguration) then
      Result := AConfiguration.OptionAsStringByName[TargetName, OptionNameBPLPath]
    else
      Result := Target.BPLOutputPath[FTargetPlatform];
  end;
  {$IFDEF MSWINDOWS}
  if (Target.RadToolKind <> brBorlandDevStudio) or (Target.VersionNumber < 3) then
    Result := PathGetShortName(Result);
  {$ENDIF MSWINDOWS}
end;

function TJclInstallation.GetDcpPath: string;
var
  AConfiguration: IJediConfiguration;
begin
  if Assigned(GUIPage) then
    Result := GUIPage.Directories[FGUIDCPPathIndex]
  else
  begin
    AConfiguration := InstallCore.Configuration;
    if Assigned(AConfiguration) then
      Result := AConfiguration.OptionAsStringByName[TargetName, OptionNameDCPPath]
    else
      Result := FJclDcpPath;
  end;
  {$IFDEF MSWINDOWS}
  if (Target.RadToolKind <> brBorlandDevStudio) or (Target.VersionNumber < 3) then
    Result := PathGetShortName(Result);
  {$ENDIF MSWINDOWS}
end;

function TJclInstallation.GetHppPath: string;
var
  AConfiguration: IJediConfiguration;
begin
  if (Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber >= 5) and TargetSupportsCBuilder then
  begin
    if Assigned(GUIPage) then
      Result := GUIPage.Directories[FGUIHPPPathIndex]
    else
    begin
      AConfiguration := InstallCore.Configuration;
      if Assigned(AConfiguration) then
        Result := AConfiguration.OptionAsStringByName[TargetName, OptionNameHPPPath]
      else
        Result := Target.VclIncludeDir[TargetPlatform];
    end;
  end
  else
    Result := Target.VclIncludeDir[TargetPlatform];
end;

procedure TJclInstallation.Close;
  procedure SaveOptions;
  var
    AConfiguration: IJediConfiguration;
    Option: TInstallerOption;
    Id, Index: Integer;
    ADemoList: TStrings;
  begin
    AConfiguration := InstallCore.Configuration;
    if not (Assigned(AConfiguration) and Assigned(GUIPage)) then
      Exit;

    // clean section before saving options
    AConfiguration.DeleteSection(TargetName);
    AConfiguration.DeleteSection(FDemoSectionName);

    for Option := Low(TInstallerOption) to High(TInstallerOption) do
    begin
      Id := OptionData[Option].Id;
      AConfiguration.OptionAsBool[TargetName, Id] := GUIPage.OptionChecked[Id];
    end;

    if not Target.IsTurboExplorer then
    begin
      if FRuntimeInstallation then
      begin
        ADemoList := GetDemoList;
        for Index := 0 to ADemoList.Count - 1 do
        begin
          Id := Integer(ADemoList.Objects[Index]);
          AConfiguration.OptionAsBool[FDemoSectionName, Id] := GUIPage.OptionChecked[Id];
        end;
      end;

      AConfiguration.OptionAsStringByName[TargetName, OptionNameBPLPath] := GUIPage.Directories[FGUIBPLPathIndex];
      if Target.RadToolKind = brCppBuilder then
        AConfiguration.OptionAsStringByName[TargetName, OptionNameBPIPath] := GUIPage.Directories[FGUIDCPPathIndex]
      else
        AConfiguration.OptionAsStringByName[TargetName, OptionNameDCPPath] := GUIPage.Directories[FGUIDCPPathIndex];
      if FGUIHPPPathIndex >= 0 then
        AConfiguration.OptionAsStringByName[TargetName, OptionNameHPPPath] := GUIPage.Directories[FGUIHPPPathIndex];
    end;
  end;
begin
  SaveOptions;

  FGUIPage := nil;
  FGUI := nil;
end;

function TJclInstallation.CompileLibraryUnits(const SubDir: string; Debug: Boolean): Boolean;

  function CopyFiles(Files: TStrings; const TargetDir: string; Overwrite: Boolean = True): Boolean;
  var
    I: Integer;
    FileName: string;
  begin
    Result := True;
    for I := 0 to Files.Count - 1 do
    begin
      FileName := Files[I];
      Result := Result and FileCopy(FileName, PathAddSeparator(TargetDir) + ExtractFileName(FileName), Overwrite);
    end;
  end;

  procedure CopyResFiles(TargetDir: string);
  var
    FileList: TStringList;
  begin
    FileList := TStringList.Create;
    try
      if BuildFileList('*.res', faAnyFile, FileList) then
        CopyFiles(FileList, TargetDir);
    finally
      FileList.Free;
    end;
  end;

  function CopyHppFiles(UnitList: TStrings; const TargetDir: string): Boolean;
  var
    I: Integer;
    TargetDirectory, FileName: string;
  begin
    Result := True;
    TargetDirectory := PathAddSeparator(TargetDir);
    for I := 0 to UnitList.Count - 1 do
    begin
      FileName := UnitList[I] + '.hpp';
      if FileExists(FileName) then
      begin
        Result := Result and FileCopy(FileName, TargetDirectory + FileName, True);

        // Always remove once copied because if they are left in place they
        // will clutter the source folder and might even prevent compilation
        // when multiple versions of C++ Builder are installed on the same
        // computer. The easiest way to see this is when checking HPP files.
        FileDelete(FileName);
      end;
      if (CompareText(UnitList[I], 'zlibh') = 0) and (Target.RadToolKind = brCppBuilder) and (Target.VersionNumber = 6) then
      begin
        Result := Result and FileCopy('zlib.h', TargetDirectory + 'zlib.h', True)
          and FileCopy('zconf.h', TargetDirectory + 'zconf.h', True);
      end;
    end;
  end;

var
  UnitType, LibDescriptor, SaveDir, UnitOutputDir, Path, ExclusionFileName: string;
  Index, ExcIndex: Integer;
  UnitList, Exclusions: TStrings;
  Compiler: TJclDCC32;
begin
  Result := True;
  if Debug then
    UnitType := 'debug ';
  LibDescriptor := Format(LoadResString(@RsLogLibDescriptor), [SubDir, UnitType, TargetName]);
  WriteLog(Format(LoadResString(@RsLogBuilding), [LibDescriptor]));
  Path := Distribution.JclPath + SubDir;

  UnitList := TStringList.Create;
  try
    BuildFileList(PathAddSeparator(Path) + '*.pas', faAnyFile, UnitList);
    ExclusionFileName := PathAddSeparator(FLibReleaseDir) + SubDir + '.exc';
    if FileExists(ExclusionFileName) then
    begin
      Exclusions := TStringList.Create;
      try
        Exclusions.LoadFromFile(ExclusionFileName);
        for Index := 0 to Exclusions.Count - 1 do
        begin
          ExcIndex := UnitList.IndexOf(Exclusions.Strings[Index]);
          if ExcIndex >= 0 then
            UnitList.Delete(ExcIndex);
        end;
      finally
        Exclusions.Free;
      end;
    end;
    if UnitList.Count = 0 then
      Exit;
    for Index := 0 to UnitList.Count - 1 do
      UnitList.Strings[Index] := ChangeFileExt(UnitList.Strings[Index], '');

    case TargetPlatform of
      bpWin32:
        Compiler := Target.DCC32;
      bpWin64:
        Compiler := (Target as TJclBDSInstallation).DCC64;
      bpOSX32:
        raise EJclBorRADException.CreateRes(@RsEOSXPlatformNotValid);
    else
      raise EJclBorRADException.CreateRes(@RsEPlatformNotValid);
    end;

    Compiler.SetDefaultOptions(Debug);
    //Options.Add('-D' + StringsToStr(Defines, ';'));
    Compiler.Options.Add('-M');   // make modified units
    Compiler.Options.Add('-$X+'); // extended syntax
    Compiler.Options.Add('-$G+'); // imported data
    Compiler.Options.Add('-$H+'); // long strings
    Compiler.Options.Add('-$P+'); // open string params
    Compiler.Options.Add('-$U-'); // safe divide
    Compiler.Options.Add('-$T-'); // typed address
    Compiler.Options.Add('-$V+'); // strict var strings
    Compiler.Options.Add('-$J+'); // writeable constants
    Compiler.Options.Add('-$Z1'); // minimum enum size
    Compiler.Options.Add('-$L+'); // local symbols
    Compiler.Options.Add('-$Y+'); // symbol reference info

    Compiler.Options.Add('-$J+'); // writable constants
    if Debug then
    begin
      Compiler.Options.Add('-$C+'); // assertions
      Compiler.Options.Add('-$D+'); // debug informations
      Compiler.Options.Add('-$I+'); // I/O checking
      Compiler.Options.Add('-$O-'); // optimizations
      Compiler.Options.Add('-$Q+'); // overflow checking
      Compiler.Options.Add('-$R+'); // range checking
      Compiler.Options.Add('-$W+'); // stack frames
    end
    else
    begin
      Compiler.Options.Add('-$C-'); // assertions
      Compiler.Options.Add('-$D-'); // debug informations
      Compiler.Options.Add('-$I-'); // I/O checking
      Compiler.Options.Add('-$O+'); // optimizations
      Compiler.Options.Add('-$Q-'); // overflow checking
      Compiler.Options.Add('-$R-'); // range checking
      Compiler.Options.Add('-$W-'); // stack frames
    end;

    if Debug then
      UnitOutputDir := FLibDebugDir
    else
      UnitOutputDir := FLibReleaseDir;

    if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 4) then
      Compiler.AddPathOption('N0', UnitOutputDir) // .dcu files
    else
      Compiler.AddPathOption('N', UnitOutputDir); // .dcu files

    if TargetSupportsCBuilder then
    begin
      Compiler.Options.Add('-D_RTLDLL' + DirSeparator + 'NO_STRICT' + DirSeparator + 'USEPACKAGES'); // $(SYSDEFINES)

      if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 4) then
      begin
        //Compiler.AddPathOption('NH', FIncludeDir);   // .hpp files
        Compiler.AddPathOption('NO', UnitOutputDir); // .obj files
        if TJclBDSInstallation(Target).DualPackageInstallation and OptionChecked[joJCLCopyPackagesHppFiles] then
          Compiler.AddPathOption('N1', GetHppPath);
      end
      else
      begin
        //Compiler.AddPathOption('N1', FIncludeDir);   // .hpp files
        Compiler.AddPathOption('N2', UnitOutputDir); // .obj files
      end;
      Compiler.Options.Add('-JPHNE');
      Compiler.Options.Add('--BCB');
      //Compiler.AddPathOption('O', Format(BCBIncludePath, [Distribution.JclIncludeDir, Distribution.JclSourcePath]));
      //Compiler.AddPathOption('U', Format(BCBObjectPath, [Distribution.JclIncludeDir, Distribution.JclSourcePath]));
    end;

    if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 9) then
      Compiler.Options.Add('-nsSystem;System.Win;WinAPI;Vcl;Vcl.Imaging');

    Compiler.AddPathOption('I', Distribution.JclIncludeDir);
    Compiler.AddPathOption('U', Distribution.JclSourcePath);
    Compiler.AddPathOption('R', Distribution.JclSourcePath);

    SaveDir := GetCurrentDir;
    Result := SetCurrentDir(Path);
    {$IFDEF WIN32}
    Win32Check(Result);
    {$ELSE}
    if Result then
    {$ENDIF}
    try
      WriteLog('');
      Result := Result and Compiler.Execute(StringsToStr(UnitList, ' '));
      CopyResFiles(UnitOutputDir);
      if OptionChecked[joJCLCopyHppFiles] then
      begin
        MarkOptionBegin(joJCLCopyHppFiles);
        Result := Result and CopyHppFiles(UnitList, GetHppPath);
        MarkOptionEnd(joJCLCopyHppFiles, Result);
      end;
    finally
      SetCurrentDir(SaveDir);
    end;
  finally
    UnitList.Free;
  end;

  if not Result then
    WriteLog(LoadResString(@RsLogFailed));
end;

function TJclInstallation.CompilePackage(const Name: string): Boolean;
var
  PackageFileName: string;
  DpkPackageFileName: string;
begin
  PackageFileName := PathAddSeparator(Distribution.JclPath) + Name;
  WriteLog(Format(LoadResString(@RsLogBuilding), [PackageFileName]));

  if Assigned(GUIPage) then
    GUIPage.CompilationStart(ExtractFileName(Name));

  if IsDelphiPackage(PackageFileName) and TargetSupportsDelphi then
  begin
    if Target.RadToolKind = brBorlandDevStudio then
      (Target as TJclBDSInstallation).CleanPackageCache(BinaryFileName(GetBplPath, PackageFileName));
    Result := Target.CompilePackage(PackageFileName, GetBplPath, GetDcpPath);
  end
  else
  if IsBCBPackage(PackageFileName) and TargetSupportsCBuilder then
  begin
    ConfigureBpr2Mak(PackageFileName);
    if Target.RadToolKind = brBorlandDevStudio then
      (Target as TJclBDSInstallation).CleanPackageCache(BinaryFileName(GetBplPath, PackageFileName));

    // to satisfy JVCL (and eventually other libraries), create a .dcp file;
    // Note: it is put out to .bpl path to make life easier for JVCL
    DpkPackageFileName := ChangeFileExt(PackageFileName, SourceExtensionDelphiPackage);
    Result := ((not FileExists(DpkPackageFileName))
               or Target.CompilePackage(DpkPackageFileName, GetBplPath, GetDcpPath))
              and Target.CompilePackage(PackageFileName, GetBplPath, GetDcpPath);
  end
  else
  begin
    Result := False;
    WriteLog(Format(LoadResString(@RsLogNoPersonalityExtension), [ExtractFileExt(PackageFileName)]));
  end;

  if Result then
    WriteLog(LoadResString(@RsLogDone))
  else
    WriteLog(LoadResString(@RsLogFailed));
end;

function TJclInstallation.CompileApplication(FileName: string): Boolean;
var
  OldDirectory, NewDirectory: string;
  Compiler: TJclDCC32;
begin
  NewDirectory := ExtractFileDir(FileName);
  FileName := ExtractFileName(FileName);
  WriteLog(Format(LoadResString(@RsLogBuilding), [FileName]));
  OldDirectory := GetCurrentDir;
  try
    SetCurrentDir(NewDirectory);
    if (Target is TJclBDSInstallation) and (Target.IDEVersionNumber >= 9) and (FTargetPlatform = bpWin64) then
      Compiler := (Target as TJclBDSInstallation).DCC64
    else
      Compiler := Target.DCC32;
    Compiler.Options.Clear;
    Compiler.SetDefaultOptions(False);
    if (Target is TJclBDSInstallation) and (Target.IDEVersionNumber >= 9) and (FTargetPlatform = bpWin64) then
      Compiler.AddPathOption('E', Distribution.JclBin64Dir)
    else
      Compiler.AddPathOption('E', Distribution.JclBinDir);
    Compiler.AddPathOption('N', '.');
    Compiler.AddPathOption('U', FLibReleaseDir + DirSeparator + Distribution.JclSourcePath);
    Compiler.AddPathOption('I', Distribution.JclIncludeDir);
    if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 9) then
      Compiler.Options.Add('-nsSystem;System.Win;WinAPI;Vcl;Vcl.Imaging;Vcl.Samples;Vcl.Shell;Web');
    Result := Compiler.Execute(FileName);
  finally
    SetCurrentDir(OldDirectory);
  end;
end;

function TJclInstallation.DeletePackage(const Name: string): Boolean;
var
  PackageFileName: string;
  BPLFileName: string;
begin
  WriteLog(Format(LoadResString(@RsLogUninstalling), [Name]));
  PackageFileName := Distribution.JclPath + Format(Name, [Target.VersionNumberStr]);

  BPLFileName := BinaryFileName(GetBplPath, PackageFileName);

  Result := FileDelete(BPLFileName);
  Result := FileDelete(ChangeFileExt(BPLFileName, CompilerExtensionMAP)) or Result;

  // delete DCP files that were created to bpl path (old behavior)
  Result := FileDelete(PathAddSeparator(GetBPLPath) + PathExtractFileNameNoExt(Name) + CompilerExtensionDCP) or Result;
  // delete DCP files that were created to target dcp path (old behavior)
  Result := FileDelete(PathAddSeparator(Target.DCPOutputPath[FTargetPlatform]) + PathExtractFileNameNoExt(Name) + CompilerExtensionDCP) or Result;
  // delete BPI files that were created to target dcp path (old behavior)
  Result := FileDelete(PathAddSeparator(Target.DCPOutputPath[FTargetPlatform]) + PathExtractFileNameNoExt(Name) + CompilerExtensionBPI) or Result;
  // delete LIB files that were created to target dcp path (old behaviour)
  Result := FileDelete(PathAddSeparator(Target.DCPOutputPath[FTargetPlatform]) + PathExtractFileNameNoExt(Name) + CompilerExtensionLIB) or Result;

  // TODO : evtl. remove .HPP Files
  if Result then
    WriteLog(LoadResString(@RsLogDone))
  else
    WriteLog(LoadResString(@RsLogFailed));
end;

procedure TJclInstallation.ConfigureBpr2Mak(const PackageFileName: string);
var
  PackageDirectory: string;
begin
  PackageDirectory := PathAddSeparator(ExtractFileDir(PackageFileName));
  if clProj2Mak in Target.CommandLineTools then
  begin
    Target.Bpr2Mak.Options.Clear;
    Target.Bpr2Mak.AddPathOption('t', ExtractRelativePath(PackageDirectory,Distribution.JclPath + Bcb2MakTemplate));
  end;
  if clMake in Target.CommandLineTools then
  begin
    Target.Make.Options.Clear;
    Target.Make.AddPathOption('DBPILIBDIR=', GetDcpPath);
    Target.Make.AddPathOption('DBPLDIR=', GetBplPath);
    if OptionChecked[joJCLCopyPackagesHppFiles] then
    //begin
    //  MarkOptionBegin(joJCLCopyPackagesHppFiles);
      Target.Make.AddPathOption('DHPPDIR=', GetHppPath);
    //  MarkOptionEnd(joJCLCopyPackagesHppFiles, True);
    //end;
  end;
end;

{$IFDEF MSWINDOWS}
function TJclInstallation.CompileExpert(const Name: string): Boolean;
var
  ProjectFileName, ProjectBinaryFileName, ProjectDEFFileName,
  ProjectDescription: string;
  LibraryPeImage: TJclPeImage;
  ExportFuncList: TJclPeExportFuncList;
  Index: Integer;
  DEFFile: TStrings;
  FirstCompilationOk: Boolean;
const
  WizardEntryPoint = 'INITWIZARD0001';
  InternalEntryPoint = '@JCLWizardInit$';
begin
  ProjectFileName := PathAddSeparator(Distribution.JclPath) + Name;

  WriteLog(Format(LoadResString(@RsLogInstalling), [ProjectFileName]));

  if Assigned(GUIPage) then
    GUIPage.CompilationStart(ExtractFileName(Name));

  if IsDelphiProject(ProjectFileName) and TargetSupportsDelphi then
    Result := Target.CompileProject(ProjectFileName, GetBplPath, GetDcpPath)
  else
  if IsBCBProject(ProjectFileName) and TargetSupportsCBuilder then
  begin
    ConfigureBpr2Mak(ProjectFileName);
    // the compilation is done in 2 steps:
    //   - first compilation without changes, we try to find the internal export name
    //     for the wizard entry point function
    //   - second compilation with creation of an alias between the internal export name
    //     and the excepted export name

    ProjectDEFFileName := ChangeFileExt(ProjectFileName, CompilerExtensionDEF);
    // first compilation
    DEFFile := TStringList.Create;
    try
      // the linker doesn't like empty def files
      DEFFile.Add('EXPORTS');
      DEFFile.SaveToFile(ProjectDEFFileName);
    finally
      DEFFile.Free;
    end;

    Result := Target.CompileProject(ProjectFileName, GetBplPath, GetDcpPath);

    if Result then
    begin
      WriteLog(LoadResString(@RsLogFirstCompilationOk));
      LibraryPeImage := TJclPeImage.Create;
      try
        GetBPRFileInfo(ProjectFileName, ProjectBinaryFileName, @ProjectDescription);
        ProjectBinaryFileName := PathAddSeparator(GetBplPath) + ProjectBinaryFileName;

        WriteLog(Format(LoadResString(@RsLogSearchingExpertEntryPoint), [ProjectBinaryFileName, WizardEntryPoint]));
        LibraryPeImage.FileName := ProjectBinaryFileName;
        ExportFuncList := LibraryPeImage.ExportList;

        FirstCompilationOk := Assigned(ExportFuncList.ItemFromName[WizardEntryPoint]);
        // the expected export name doesn't exist
        if not FirstCompilationOk then
        begin
          Result := False;
          WriteLog(LoadResString(@RsLogEntryPointNotFound));

          // try to find the decorated entry point
          // export names for pascal functions are:
          // @UnitName@FunctionName$ParameterSignature

          for Index := 0 to ExportFuncList.Count - 1 do
            if Pos(StrUpper(InternalEntryPoint), StrUpper(ExportFuncList.Items[Index].Name)) > 0 then
          begin
            WriteLog(Format(LoadResString(@RsLogEntryPointFound), [ExportFuncList.Items[Index].Name]));
            DEFFile := TStringList.Create;
            try
              DEFFile.Add('EXPORTS');
              DEFFile.Add(Format('%s=%s', [WizardEntryPoint, ExportFuncList.Items[Index].Name]));
              DEFFile.SaveToFile(ProjectDEFFileName);
            finally
              DEFFile.Free;
            end;
            Result := True;
            Break;
          end;
        end
        else
        begin
          WriteLog(Format(LoadResString(@RsLogRegistering), [ProjectBinaryFileName]));
          Target.RegisterExpert(ProjectBinaryFileName, ProjectDescription);
        end;
      finally
        LibraryPeImage.Free;
      end;

      if Result and (not FirstCompilationOk) then
        // second compilation
        Result := Target.CompileProject(ProjectFileName, GetBplPath, GetDcpPath)
      else
      if not Result then
        WriteLog(LoadResString(@RsLogEntryPointNotFound));
    end
    else
      WriteLog(LoadResString(@RsLogFirstCompilationFailed));
  end
  else
    Result := False;

  if Result then
    WriteLog(LoadResString(@RsLogDone))
  else
    WriteLog(LoadResString(@RsLogFailed));
end;
{$ENDIF MSWINDOWS}

function DemoNameCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  Name1, Name2: string;
begin
  Name1 := ExtractFileName(List[Index1]);
  Name2 := ExtractFileName(List[Index2]);
  Result := CompareText(Name1, Name2);
end;

procedure TJclInstallation.AddDemo(const Directory: string; const FileInfo: TSearchRec);
begin
  if not StrSame(ExtractFileExt(FileInfo.Name), '.dproj') then
    FDemoList.Append(Directory + FileInfo.Name);
end;

procedure TJclInstallation.AddDemos(const Directory: string);
begin
  EnumFiles(Directory + '*.dpr', AddDemo);
end;

function TJclInstallation.GetDemoList: TStringList;
  procedure ProcessExcludeFile(const ExcFileName: string);
  var
    DemoExclusionList: TStrings;
    ExclusionFileName, FileName, RequiredList, RequiredItem: string;
    IndexExc, IndexDemo, SepPos, IndexReq: Integer;
    ExcludeDemo: Boolean;
  begin
    DemoExclusionList := TStringList.Create;
    try
      ExclusionFileName := MakePath(PathAddSeparator(Distribution.JclExamplesDir) + ExcFileName);
      if FileExists(ExclusionFileName) then
      begin
        DemoExclusionList.LoadFromFile(ExclusionFileName);
        for IndexExc := 0 to DemoExclusionList.Count - 1 do
        begin
          FileName := DemoExclusionList.Strings[IndexExc];
          SepPos := Pos('=', FileName);
          if SepPos > 0 then
          begin
            ExcludeDemo := False;
            RequiredList := Copy(FileName, SepPos + 1, Length(FileName) - SepPos);
            SetLength(FileName, SepPos - 1);
            for IndexReq := 0 to PathListItemCount(RequiredList) - 1 do
            begin
              RequiredItem := PathListGetItem(RequiredList, IndexReq);
              if AnsiSameText(ExtractFileExt(RequiredItem), '.dcu') then
              begin
                ExcludeDemo := not FileExists(PathAddSeparator(Target.LibFolderName[FTargetPlatform]) + RequiredItem);
                if ExcludeDemo then
                  Break;
              end;
            end;
          end
          else
            ExcludeDemo := True;

          if ExcludeDemo then
          begin
            if AnsiSameText(ExtractFileExt(FileName), '.exc') then
              ProcessExcludeFile(FileName)
            else
            begin
              for IndexDemo := FDemoList.Count - 1  downto 0 do
                if StrMatches(PathAddSeparator(Distribution.JclExamplesDir) + FileName, FDemoList.Strings[IndexDemo]) then
                  FDemoList.Delete(IndexDemo);
            end;
          end;
        end;
      end;
    finally
      DemoExclusionList.Free;
    end;
  end;
begin
  if not Assigned(FDemoList) then
  begin
    FDemoList := TStringList.Create;
    EnumDirectories(Distribution.JclExamplesDir, AddDemos);
    FDemoList.CustomSort(DemoNameCompare);

    ProcessExcludeFile('%s.exc');
  end;
  Result := FDemoList;
end;
{
function TJclInstallation.Run: Boolean;
  procedure EnsureDirectoryExists(const DirectoryName, DisplayName: string);
  begin
    if not DirectoryExists(DirectoryName) then
    begin
      if (MessageDlg(Format(RsCreatePath, [DisplayName]), mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
        Abort;
      if not ForceDirectories(DirectoryName) then
      begin
        MessageDlg(Format(RsCantCreatePath, [DirectoryName]), mtError, [mbAbort], 0);
        Abort;
      end;
    end;
  end;
var
  PathEnvVar: string;
begin
  Result := True;
  if OptionSelected(ioJCL) then
  begin
    if not OptionSelected(ioJclPackages)
      and (MessageDlg(RsPackageNodeNotSelected, mtWarning, [mbYes, mbNo], 0) <> mrYes) then
      Abort;

    EnsureDirectoryExists(BplPath, 'BPL');
    EnsureDirectoryExists(DcpPath, 'DCP');

    {$IFDEF MSWINDOWS
    PathEnvVar := RegReadStringDef(HKCU, RegHKCUEnvironmentVar, PathEnvironmentVar, '');
    PathListIncludeItems(PathEnvVar, RegReadStringDef(HKLM, RegHKLMEnvironmentVar, PathEnvironmentVar, ''));
    if (PathListItemIndex(PathEnvVar, BplPath) = -1) and (PathListItemIndex(PathEnvVar, PathAddSeparator(BplPath)) = -1)
      and (MessageDlg(RsAddPathToEnvironment, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      PathEnvVar := RegReadStringDef(HKCU, RegHKCUEnvironmentVar, PathEnvironmentVar, '');
      PathListIncludeItems(PathEnvVar, BplPath);
      RegWriteString(HKCU, RegHKCUEnvironmentVar, PathEnvironmentVar, PathEnvVar);
    end;
    {$ENDIF MSWINDOWS

    InstallationStarted;
    try
      Result := InstallSelectedOptions;
    finally
      InstallationFinished;
    end;
  end;
  SaveOptions;
end;
}

//=== { TJclDistribution } ===================================================

procedure TJclDistribution.Close;
var
  I: Integer;
  Settings: IJediConfiguration;
begin
  Settings := InstallCore.Configuration;
  if Assigned(Settings) and Assigned(FProfilesPage) then
    for I := 0 to InstallCore.ProfilesManager.ProfileCount - 1 do
      Settings.OptionAsBoolByName[ProfilesSectionName, InstallCore.ProfilesManager.ProfileNames[I]] := FProfilesPage.IsProfileEnabled[I];
  for I := 0 to TargetInstallCount - 1 do
    TargetInstalls[I].Close;
  FGUI := nil;
end;

constructor TJclDistribution.Create;
  procedure RegisterJclOptions;
  var
    Option: TInstallerOption;
    AInstallCore: TJediInstallCore;
    OptionName: string;
  begin
    AInstallCore := InstallCore;
    for Option := Low(TInstallerOption) to High(TInstallerOption) do
    begin
      OptionName := GetEnumName(TypeInfo(TInstallerOption), Integer(Option));
      OptionName := 'Jcl' + Copy(OptionName, 3, Length(OptionName) - 2);
      OptionData[Option].Id := AInstallCore.AddInstallOption(OptionName);
    end;
  end;
begin
  inherited Create;

  RegisterJclOptions;

  {$IFDEF MSWINDOWS}
  FRegHelpCommands := TStringList.Create;
  {$ENDIF MSWINDOWS}
  FRadToolInstallations := TJclBorRADToolInstallations.Create;

  FTargetInstalls := TObjectList.Create;
  FTargetInstalls.OwnsObjects := True;
end;

function TJclDistribution.CreateInstall(Target: TJclBorRADToolInstallation): Boolean;
  function Supported: Boolean;
  begin
    case Target.RadToolKind of
      brDelphi :
        Result := Target.VersionNumber in [6, 7];
      brCppBuilder :
        Result := Target.VersionNumber in [6];
      brBorlandDevStudio :
        Result := ((Target.VersionNumber in [1, 2]) and (bpDelphi32 in Target.Personalities))
          or (Target.VersionNumber in [3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19]);
      else
        Result := False;
    end;
    Result := Result and (Target.Personalities * [bpDelphi32, bpDelphi64, bpBCBuilder32, bpBCBuilder64] <> []);
  end;
var
  Inst: TJclInstallation;
begin
  if Supported then
  try
    Inst := TJclInstallation.Create(Self, Target, bpWin32, nil);
    FTargetInstalls.Add(Inst);
    // Win64 "virtual" target
    if (Target is TJclBDSInstallation) and (Target.IDEVersionNumber >= 9) and (not Target.IsTurboExplorer)
      and (bpDelphi64 in Target.Personalities) then
    begin
      Inst := TJclInstallation.Create(Self, Target, bpWin64, nil);
      FTargetInstalls.Add(Inst);
    end;
  except
  end;
  Result := True;
end;

destructor TJclDistribution.Destroy;
begin
  {$IFDEF MSWINDOWS}
  FRegHelpCommands.Free;
  {$ENDIF MSWINDOWS}

  FRadToolInstallations.Free;

  FTargetInstalls.Free;

  inherited Destroy;
end;

function TJclDistribution.GetTargetInstall(Index: Integer): TJclInstallation;
begin
  Result := TJclInstallation(FTargetInstalls.Items[Index]);
end;

function TJclDistribution.GetTargetInstallCount: Integer;
begin
  Result := FTargetInstalls.Count;
end;

function TJclDistribution.GetVersion: string;
  function GetRevision: Integer;
  var
    DailyFileName, SvnEntriesFileName, RevisionText: string;
    TextFile: TJclAnsiMappedTextReader;
  begin
    Result := 0;

    DailyFileName := JclPath + DailyRevisionFileName;
    if FileExists(DailyFileName) then
    begin
      // directory from a daily zip
      TextFile := TJclAnsiMappedTextReader.Create(DailyFileName);
      try
        RevisionText := string(TextFile.ReadLn);
        Result := StrToIntDef(RevisionText, 0);
      finally
        TextFile.Free;
      end;
    end;

    if Result = 0 then
    begin
      SvnEntriesFileName := JclPath + EntriesFileName1;
      if not FileExists(SvnEntriesFileName) then
        SvnEntriesFileName := JclPath + EntriesFileName2;
      if FileExists(SvnEntriesFileName) then
      begin
        // directory from subversion
        TextFile := TJclAnsiMappedTextReader.Create(SvnEntriesFileName);
        try
          TextFile.ReadLn;
          TextFile.ReadLn;
          TextFile.ReadLn;
          RevisionText := string(TextFile.ReadLn);
          Result := StrToIntDef(RevisionText, 0);
        finally
          TextFile.Free;
        end;
      end;
    end;
  end;
var
  StableText, Source: string;
  Revision: Integer;
begin
  if JclVersionRelease = 0 then
  begin
    Revision := GetRevision;
    StableText := RsJclVersionTesting;
  end
  else
  begin
    Revision := 0;
    StableText := RsJclVersionRelease;
  end;

  if Revision = 0 then
  begin
    Source := RsJclVersionBuild;
    Revision := JclVersionBuild;
  end
  else
    Source := RsJclVersionRevision;

  Result := Format(RsJclVersionMask, [JclVersionMajor, JclVersionMinor, StableText, Source, Revision])
end;

procedure TJclDistribution.Init;
  procedure InitDistribution;
  var
    ExceptDialogsPath, InstallerFileName, ProfileName: string;
    Index: Integer;
    Settings: IJediConfiguration;
  begin
    InstallerFileName := ParamStr(0);

    FJclPath := PathAddSeparator(ExpandFileName(PathExtractFileDirFixed(InstallerFileName) + '..'));
    FLibReleaseDirMask := Format('%slib' + VersionDirExp, [JclPath]);
    FLibDebugDirMask := FLibReleaseDirMask + DirDelimiter + 'debug';
    FJclBinDir := JclPath + 'bin';
    FJclBin64Dir := JclPath + 'bin64';
    FJclIncludeDir := PathAddSeparator(JclPath + 'source') + 'include';
    FJclIncludeTemplate := PathAddSeparator(FJclIncludeDir) + JclIncludeTemplateFileName;
    FJclExamplesDir := JclPath + 'examples';
    FJclSourcePath := '';
    for Index := Low(JclSrcPaths) to High(JclSrcPaths) do
      ListAddItems(FJclSourcePath, DirSeparator, JclPath + JclSrcPaths[Index]);
    FJclOldSourcePath := '';
    for Index := Low(JclOldSrcPaths) to High(JclOldSrcPaths) do
      ListAddItems(FJclOldSourcePath, DirSeparator, JclPath + JclOldSrcPaths[Index]);

    ExceptDialogsPath := JclPath + ExceptDlgPath;
    FVclDialogFileName := ExceptDialogsPath + ExceptDlgVclFileName;
    FVclDialogSendFileName := ExceptDialogsPath + ExceptDlgVclSndFileName;
    ExceptDialogsPath := JclPath + ExceptIcoPath;
    FVclDialogIconFileName := ExceptDialogsPath + ExceptIcoVclFileName;
    FVclDialogSendIconFileName := ExceptDialogsPath + ExceptIcoVclSndFileName;
    FJclOldChmHelpFileName := JclPath + JclOldChmHelpFile;
    FJclChmHelpFileName := JclPath + JclChmHelpFile;
    FJclContainersChmHelpFileName := JclPath + JclContainersChmHelpFile;
    FJclDeveloperToolsChmHelpFileName := JclPath + JclDeveloperToolsChmHelpFile;
    FJclVclChmHelpFileName := JclPath + JclVclChmHelpFile;
    FJclOldHlpHelpFileName := JclPath + JclOldHlpHelpFile;
    FJclHlpHelpFileName := JclPath + JclHlpHelpFile;
    FJclContainersHlpHelpFileName := JclPath + JclContainersHlpHelpFile;
    FJclDeveloperToolsHlpHelpFileName := JclPath + JclDeveloperToolsHlpHelpFile;
    FJclVclHlpHelpFileName := JclPath + JclVclHlpHelpFile;
    FJclOldHxSHelpFileName := JclPath + JclOldHxSHelpFile;
    FJclHxSHelpFileName := JclPath + JclHxSHelpFile;
    FJclContainersHxSHelpFileName := JclPath + JclContainersHxSHelpFile;
    FJclDeveloperToolsHxSHelpFileName := JclPath + JclDeveloperToolsHxSHelpFile;
    FJclVclHxSHelpFileName := JclPath + JclVclHxSHelpFile;
    if not FileExists(FJclChmHelpFileName) then
    begin
      FJclChmHelpFileName := '';
      FJclContainersChmHelpFileName := '';
      FJclDeveloperToolsChmHelpFileName := '';
      FJclVclChmHelpFileName := '';
    end;
    if not FileExists(FJclHlpHelpFileName) then
    begin
      FJclHlpHelpFileName := '';
      FJclContainersHlpHelpFileName := '';
      FJclDeveloperToolsHlpHelpFileName := '';
      FJclVclHlpHelpFileName := '';
    end;
    if not FileExists(FJclHxSHelpFileName) then
    begin
      FJclHxSHelpFileName := '';
      FJclContainersHxSHelpFileName := '';
      FJclDeveloperToolsHxSHelpFileName := '';
      FJclVclHxSHelpFileName := '';
    end;
    {$IFDEF MSWINDOWS}
    // Reset ReadOnly flag for dialog forms
    FileSetAttr(FVclDialogFileName, faArchive);
    FileSetAttr(ChangeFileExt(FVclDialogFileName, '.dfm'), faArchive);
    FileSetAttr(FVclDialogSendFileName, faArchive);
    FileSetAttr(ChangeFileExt(FVclDialogSendFileName, '.dfm'), faArchive);
    {$ENDIF MSWINDOWS}
    FJclReadmeFileName := JclPath + 'docs' + DirDelimiter + ReadmeFileName;
    FJclLicenseFileName := JclPath + LicenseFileName;
    if Assigned(GUI) then
    begin
      FReadMePage := GUI.CreateTextPage;
      FReadMePage.Caption := Version;
      FReadMePage.TextFileName := FJclReadmeFileName;

      FLicensePage := GUI.CreateTextPage;
      FLicensePage.Caption := LoadResString(@RsCaptionLicense);
      FLicensePage.TextFileName := FJclLicenseFileName;
      FLicensePage.AddOption(LoadResString(@RsCaptionLicenseAgreement));

      if InstallCore.ProfilesManager.MultipleProfileMode then
      begin
        FProfilesPage := GUI.CreateProfilesPage;
        FProfilesPage.Caption := 'Profiles';

        Settings := InstallCore.Configuration;
        if Settings <> nil then
          for Index := 0 to InstallCore.ProfilesManager.ProfileCount - 1 do
        begin
          ProfileName := InstallCore.ProfilesManager.ProfileNames[Index];
          if Settings.ValueExists(ProfilesSectionName, ProfileName) then
            FProfilesPage.IsProfileEnabled[Index] := Settings.OptionAsBoolByName[ProfilesSectionName, ProfileName];
        end;
      end;
    end;
  end;

  procedure CreateInstallations;
  begin
    if not RADToolInstallations.Iterate(CreateInstall) then
      raise EJediInstallInitFailure.CreateRes(@RsNoInstall);
  end;

  procedure InitInstallations;
  var
    I: Integer;
  begin
    for I := 0 to TargetInstallCount - 1 do
      TargetInstalls[I].Init;
  end;
begin
  FGUI := InstallCore.InstallGUI;

  InitDistribution;
  CreateInstallations;
  InitInstallations;
end;

function TJclDistribution.Install(InstallPage: IJediInstallPage): Boolean;
var
  I: Integer;
  KeepSettings: Boolean;
  AInstallation: TJclInstallation;
  XML: TJclSimpleXML;
  AInstallationElem: TJclSimpleXMLElem;
  LogContent: TStringList;
begin
  try
    KeepSettings := True;

    if RadToolInstallations.AnyInstanceRunning and (not Assigned(GUI) or not GUI.IgnoreRunningIDE) {$IFDEF MSWINDOWS} and not IsDebuggerAttached {$ENDIF} then
    begin
      if Assigned(GUI) then
        GUI.Dialog(LoadResString(@RsCloseRADTool), dtError, [drCancel]);
      Result := False;
      Exit;
    end;

    if Assigned(LicensePage) and not LicensePage.Options[0] and (not Assigned(GUI) or not GUI.AutoAcceptMPL) then
    begin
      if Assigned(GUI) then
        GUI.Dialog(LoadResString(@RsMissingLicenseAgreement), dtError, [drCancel]);
      LicensePage.Show;
      Result := False;
      Exit;
    end;

    {$IFDEF MSWINDOWS}
    if Assigned(GUI) then
    begin
      for I := 0 to TargetInstallCount - 1 do
      begin
        AInstallation := TargetInstalls[I];
        if AInstallation.Enabled then
        begin
          if (InstallPage = nil) or (AInstallation.GUIPage = InstallPage) then
          begin
            if Assigned(AInstallation.GUIPage) then
              AInstallation.GUIPage.Show;
            KeepSettings := GUI.Dialog(LoadResString(@RsKeepExpertSettings),
              dtConfirmation, [drYes, drNo]) = drYes;
            Break;
          end;
        end;
      end;
    end;
    RegHelpClearCommands;
    {$ENDIF MSWINDOWS}

    FNbEnabled := 0;
    FNbInstalled := 0;

    for I := 0 to TargetInstallCount - 1 do
    begin
      AInstallation := TargetInstalls[I];
      if (InstallPage = nil) or (AInstallation.GUIPage = InstallPage) then
      begin
        if AInstallation.Enabled then
          Inc(FNbEnabled);
        if GUI.DeletePreviousLogFiles then
          SysUtils.DeleteFile(AInstallation.LogFileName);
      end;
    end;

    Result := True;
    for I := 0 to TargetInstallCount - 1 do
    begin
      AInstallation := TargetInstalls[I];
      if AInstallation.Enabled then
      begin
        if (InstallPage = nil) or (AInstallation.GUIPage = InstallPage) then
        begin
          AInstallation.Silent := False;
          if not KeepSettings then
            AInstallation.RemoveSettings;
          AInstallation.Uninstall(False);
          Result := AInstallation.Install;
          if not Result and (not Assigned(GUI) or not GUI.ContinueOnTargetError) then
            Break;
          Inc(FNbInstalled);
        end;
      end;
    end;

    {$IFDEF MSWINDOWS}
    Result := Result and RegHelpExecuteCommands(True);
    {$ENDIF MSWINDOWS}
  finally
    if Assigned(GUI) and (GUI.XMLResultFileName <> '') then
    begin
      XML := TJclSimpleXML.Create;
      try
        XML.Options := [sxoAutoCreate, sxoAutoIndent, sxoAutoEncodeValue, sxoAutoEncodeEntity];
        XML.Root.Name := 'JclInstall';
        for I := 0 to TargetInstallCount - 1 do
        begin
          AInstallation := TargetInstalls[I];
          AInstallationElem := XML.Root.Items.Add('Installation');

          case AInstallation.TargetPlatform of
            bpWin64:
              AInstallationElem.Properties.Add('Target', AInstallation.Target.VersionNumberStr + '_x64');
            else
              AInstallationElem.Properties.Add('Target', AInstallation.Target.VersionNumberStr);
          end;
          AInstallationElem.Properties.Add('TargetName', AInstallation.TargetName);
          AInstallationElem.Properties.Add('Enabled', AInstallation.Enabled);
          AInstallationElem.Properties.Add('InstallAttempted', I <= FNbInstalled);
          AInstallationElem.Properties.Add('InstallSuccess', AInstallation.InstallSuccess);
          AInstallationElem.Properties.Add('LogFileName', Iff(FileExists(AInstallation.LogFileName), AInstallation.LogFileName, ''));
          if GUI.IncludeLogFilesInXML and FileExists(AInstallation.LogFileName) then
          begin
            LogContent := TStringList.Create;
            try
              LogContent.LoadFromFile(AInstallation.LogFileName{$IFDEF UNICODE}, TEncoding.UTF8{$ENDIF UNICODE});
              AInstallationElem.Items.Add('LogFile').Items.AddCData('', {$IFNDEF UNICODE}UTF8Decode{$ENDIF UNICODE}(LogContent.Text));
            finally
              LogContent.Free;
            end;
          end;
        end;
        XML.SaveToFile(GUI.XMLResultFileName, JclStreams.seUTF8);
      finally
        XML.Free;
      end;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
const
  // Reg Helper constant (chronological order)
  RHCreateTransaction   = 1;
  RHRegisterNameSpace   = 2;
  RHRegisterFile        = 3;
  RHPlugNameSpace       = 4;
  RHUnplugNameSpace     = 5;
  RHUnregisterFile      = 6;
  RHUnregisterNameSpace = 7;
  RHCommitTransaction   = 8;

procedure TJclDistribution.RegHelpClearCommands;
begin
  FRegHelpCommands.Clear;
end;

procedure TJclDistribution.RegHelpCommitTransaction;
begin
  RegHelpInternalAdd(RHCommitTransaction, 'commit', True);
end;

procedure TJclDistribution.RegHelpCreateTransaction;
begin
  RegHelpInternalAdd(RHCreateTransaction, 'create', True);
end;

function TJclDistribution.RegHelpExecuteCommands(DisplayErrors: Boolean): Boolean;
var
  Index: Integer;
  Parameters, LogFileName, ProgramResult, Verb: string;
  ResultLines: TJclAnsiMappedTextReader;
  TargetInstall: TJclInstallation;
begin
  Result := True;
  if FRegHelpCommands.Count = 0 then
    Exit;

  // step 1: compile the RegHelper utility

  for Index := TargetInstallCount - 1 downto 0 do // from the end (newer releases ready for vista)
  begin
    TargetInstall := TargetInstalls[Index];
    if (TargetInstall.Enabled) and (TargetInstall.FTargetPlatform = bpWin32) then
    begin
      Result := TargetInstall.CompileApplication(JclPath + 'install' + DirDelimiter + 'RegHelper.dpr');
      if not Result then
      begin
        if Assigned(GUI) then
          GUI.Dialog(RsLogRegHelperFailedCompile, dtError, [drOK]);
        Exit;
      end;
      Break;
    end;
  end;

  // step 2: create parameters for the RegHelper utility

  LogFileName := JclBinDir + DirDelimiter + 'RegHelper.log';
  if FileExists(LogFileName) then
    FileDelete(LogFileName);
  Parameters := Format('-c -o"%s"', [LogFileName]);
  for Index := 0 to FRegHelpCommands.Count - 1 do
  begin
    case Integer(FRegHelpCommands.Objects[Index]) of
      RHCreateTransaction:
        Parameters := Format('%s Create', [Parameters]);
      RHRegisterNameSpace:
        Parameters := Format('%s "RegNameSpace;%s"', [Parameters, FRegHelpCommands.Strings[Index]]);
      RHRegisterFile:
        Parameters := Format('%s "RegHelpFile;%s"', [Parameters, FRegHelpCommands.Strings[Index]]);
      RHPlugNameSpace:
        Parameters := Format('%s "PlugNameSpace;%s"', [Parameters, FRegHelpCommands.Strings[Index]]);
      RHUnplugNameSpace:
        Parameters := Format('%s "UnplugNameSpace;%s"', [Parameters, FRegHelpCommands.Strings[Index]]);
      RHUnregisterFile:
        Parameters := Format('%s "UnregHelpFile;%s"', [Parameters, FRegHelpCommands.Strings[Index]]);
      RHUnregisterNameSpace:
        Parameters := Format('%s "UnregNameSpace;%s"', [Parameters, FRegHelpCommands.Strings[Index]]);
      RHCommitTransaction:
        Parameters := Format('%s Commit', [Parameters]);
    else
      if Assigned(GUI) then
        GUI.Dialog(LoadResString(@RsLogRegHelperUnknownCommand), dtError, [drOK]);
      Exit;
    end;
  end;

  // step 3:  inform the user and execute RegHelper

  // simple dialog explaining user why we need credentials
  if Assigned(GUI) and not IsElevated then
    GUI.Dialog(LoadResString(@RsHTMLHelp2Credentials), dtInformation, [drOK]);

  // RegHelper.exe manifest requires elevation on Windows Vista/7/8/8.1 and Windows Server 2008/2008R2/2012/2012R2
  if IsAdministrator or IsWinVista or IsWinServer2008 or IsWin7 or IsWinServer2008R2 or
    IsWin8 or IsWinServer2012 or IsWin81 or IsWinServer2012R2 then
    Verb := 'open'
  else
    Verb := 'runas';

  Result := JclShell.ShellExecAndWait(JclBinDir + DirDelimiter + 'RegHelper.exe', Parameters, Verb, SW_HIDE, JclPath + 'help' + DirDelimiter);

  // step 4: examine output
  if Result then
  begin
    if not DisplayErrors then
      Exit;
    Sleep(500); // wait possible antivirus lock
    ResultLines := TJclAnsiMappedTextReader.Create(LogFileName);
    try
      while not ResultLines.Eof do
      begin
        ProgramResult := string(ResultLines.ReadLn);
        if AnsiPos('ERROR', AnsiUpperCase(ProgramResult)) > 0 then
        begin
          Result := False;
          if Assigned(GUI) then
            GUI.Dialog(LoadResString(@RsLogRegHelperError) + NativeLineBreak + ProgramResult, dtError, [drCancel]);
        end;
      end;
    finally
      ResultLines.Free;
    end;
  end
  else
    GUI.Dialog(LoadResString(@RsLogRegHelperFailedExecute), dtError, [drOK]);
end;

procedure TJclDistribution.RegHelpInternalAdd(Command: Integer;
  Arguments: string; DoNotRepeatCommand: Boolean);
var
  Index: Integer;
  AObject: TObject;
begin
  Index := 0;
  while Index <= FRegHelpCommands.Count do
  begin
    if Index = FRegHelpCommands.Count then
    begin
      FRegHelpCommands.AddObject(Arguments, TObject(Command));
      Break;
    end;
    AObject := FRegHelpCommands.Objects[Index];
    if (Integer(AObject) = Command) and
      (DoNotRepeatCommand or (FRegHelpCommands.Strings[Index] = Arguments)) then
      Break;
    if Integer(AObject) > Command then
    begin
      FRegHelpCommands.InsertObject(Index, Arguments, TObject(Command));
      Break;
    end;
    Inc(Index);
  end;
end;

procedure TJclDistribution.RegHelpPlugNameSpaceIn(const SourceNameSpace,
  TargetNameSpace: WideString);
begin
  RegHelpInternalAdd(RHPlugNameSpace, Format('%s;%s', [SourceNameSpace, TargetNameSpace]), False);
end;

procedure TJclDistribution.RegHelpRegisterHelpFile(const NameSpace,
  Identifier: WideString; const LangId: Integer; const HxSFile,
  HxIFile: WideString);
begin
  RegHelpInternalAdd(RHRegisterFile, Format('%s;%s;%d;%s;%s', [NameSpace, Identifier, LangId, HxSFile, HxIFile]), False);
end;

procedure TJclDistribution.RegHelpRegisterNameSpace(const Name, Collection,
  Description: WideString);
begin
  RegHelpInternalAdd(RHRegisterNameSpace, Format('%s;%s;%s', [Name, Collection, Description]), False);
end;

procedure TJclDistribution.RegHelpUnPlugNameSpace(const SourceNameSpace,
  TargetNameSpace: WideString);
begin
  RegHelpInternalAdd(RHUnplugNameSpace, Format('%s;%s', [SourceNameSpace, TargetNameSpace]), False);
end;

procedure TJclDistribution.RegHelpUnregisterHelpFile(const NameSpace,
  Identifier: WideString; const LangId: Integer);
begin
  RegHelpInternalAdd(RHUnregisterFile, Format('%s;%s;%d', [NameSpace, Identifier, LangId]), False);
end;

procedure TJclDistribution.RegHelpUnregisterNameSpace(const Name: WideString);
begin
  RegHelpInternalAdd(RHUnregisterNameSpace, Name, False);
end;
{$ENDIF MSWINDOWS}

function TJclDistribution.Uninstall(InstallPage: IJediInstallPage): Boolean;
var
  I: Integer;
  AInstallation: TJclInstallation;
begin
  if RadToolInstallations.AnyInstanceRunning and (not Assigned(GUI) or not GUI.IgnoreRunningIDE) {$IFDEF MSWINDOWS} and not IsDebuggerAttached {$ENDIF} then
  begin
    if Assigned(GUI) then
      GUI.Dialog(LoadResString(@RsCloseRADTool), dtError, [drCancel]);
    Result := False;
    Exit;
  end;

  {$IFDEF MSWINDOWS}
  RegHelpClearCommands;
  {$ENDIF MSWINDOWS}

  Result := True;
  for I := 0 to TargetInstallCount - 1 do
  begin
    AInstallation := TargetInstalls[I];
    if (InstallPage = nil) or (AInstallation.GUIPage = InstallPage) then
    begin
      AInstallation.Silent := False;
      if AInstallation.Enabled and ((not AInstallation.RemoveSettings) or not AInstallation.Uninstall(True)) then
        Result := False;
    end;
  end;

  {$IFDEF MSWINDOWS}
  RegHelpExecuteCommands(False);
  {$ENDIF MSWINDOWS}
end;

initialization
  JediInstall.InstallCore.AddProduct(TJclDistribution.Create);

end.
