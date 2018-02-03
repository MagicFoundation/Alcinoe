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
{ The Original Code is JclBorlandToolsExtensions.pas.                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Uwe Schuster. Portions created by Uwe Schuster are }
{ Copyright (C) of Uwe Schuster. All Rights Reserved.                                              }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains extended wrapper classes for borlands tools                                             }
{                                                                                                  }
{ Known Issues:                                                                                    }
{   This is a preview - class and functionnames might be changed and this unit might me merged     }
{   into JclBorlandTools.pas.                                                                      }
{                                                                                                  }
{ Unit owner: Uwe Schuster                                                                         }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

{
Todo:
- IFDEF OS specific compiler switches and defines?
}

unit JclBorlandToolsExtensions;

{$I jcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc, Types,
  {$ENDIF LINUX}
  {$IFNDEF RTL140_UP}
  JclBase,
  {$ENDIF ~RTL140_UP}
  SysUtils, Classes, IniFiles, Contnrs, JclSysUtils, JclIDEUtils;

type
  TJclDCCMessageKind = (mkUnknown, mkHint, mkWarning, mkError, mkFatal);
  TJclDCCMapFileLevel = (mfloff, mflsegments, mflpublics, mfldetailed);

  TJclDCCSwitch = class(TObject)
  protected
    FSwitchChar: Char;
    function DefaultSetAsString(const AValue: string): Boolean;
    function GetAsBoolean: Boolean; virtual;
    function GetAsInteger: Integer; virtual;
    function GetAsString: string; virtual;
    procedure SetAsBoolean(AValue: Boolean); virtual;
    procedure SetAsInteger(AValue: Integer); virtual;
    procedure SetAsString(AValue: string); virtual;
    constructor Create(ASwitchChar: Char);
  public
    procedure SetDefaultValue; virtual;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
    property SwitchChar: Char read FSwitchChar;
  end;

  TJclDCCSwitchAValues = (aOn, aOff, a1, a2, a4, a8);

  TJclDCCASwitch = class(TJclDCCSwitch)
  private
    FDefaultValue: TJclDCCSwitchAValues;
    FValue: TJclDCCSwitchAValues;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsString(AValue: string); override;
    constructor Create;
  public
    procedure SetDefaultValue; override;
    property Value: TJclDCCSwitchAValues read FValue write FValue;
  end;

  TJclDCCBooleanSwitch = class(TJclDCCSwitch)
  private
    FDefaultValue: Boolean;
    FValue: Boolean;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsString(AValue: string); override;
    constructor Create(ASwitchChar: Char; ADefaultValue: Boolean);
  public
    procedure SetDefaultValue; override;  
    property Value: Boolean read FValue write FValue;
  end;

  TJclDCCSwitchYValues = (yOn, yOff, yD);

  TJclDCCYSwitch = class(TJclDCCSwitch)
  private
    FDefaultValue: TJclDCCSwitchYValues;
    FValue: TJclDCCSwitchYValues;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsString(AValue: string); override;
    constructor Create;
  public
    procedure SetDefaultValue; override;  
    property Value: TJclDCCSwitchYValues read FValue write FValue;
  end;

  TJclDCCSwitchZValue = (zOn, zOff, z1, z2, z4);

  TJclDCCZSwitch = class(TJclDCCSwitch)
  private
    FDefaultValue: TJclDCCSwitchZValue;
    FValue: TJclDCCSwitchZValue;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsString(AValue: string); override;
    constructor Create;
  public
    procedure SetDefaultValue; override;
    property Value: TJclDCCSwitchZValue read FValue write FValue;
  end;

  //(usc) what about E, F, K, N, S (can be found in .cfg and .dof but not in dcc32 help output)
  TJclDCCSwitches = class(TObject)
  private
    FItems: TObjectList;
    FAlignedRecordFields: TJclDCCASwitch;
    FFullBooleanEvaluation: TJclDCCBooleanSwitch;
    FEvaluateAssertionsAtRuntime: TJclDCCBooleanSwitch;
    FDebugInformation: TJclDCCBooleanSwitch;
    FUseImportedDataReferences: TJclDCCBooleanSwitch;
    FUseLongStringsByDefault: TJclDCCBooleanSwitch;
    FIOChecking: TJclDCCBooleanSwitch;
    FWriteableStructuredConsts: TJclDCCBooleanSwitch;
    FLocalDebugSymbols: TJclDCCBooleanSwitch;
    FRuntimeTypeInfo: TJclDCCBooleanSwitch;
    FOptimization: TJclDCCBooleanSwitch;
    FOpenStringParams: TJclDCCBooleanSwitch;
    FIntegerOverflowChecking: TJclDCCBooleanSwitch;
    FRangeChecking: TJclDCCBooleanSwitch;
    FTypedAtOperator: TJclDCCBooleanSwitch;
    FPentiumTMSafeDivide: TJclDCCBooleanSwitch;
    FStrictVarStrings: TJclDCCBooleanSwitch;
    FGenerateStackFrames: TJclDCCBooleanSwitch;
    FExtendedSyntax: TJclDCCBooleanSwitch;
    FSymbolReferenceInfo: TJclDCCYSwitch;
    FMinimumSizeOfEnumTypes: TJclDCCZSwitch;
    procedure CreateSwitches;
    function GetAsString: string;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TJclDCCSwitch;
    procedure SetAsString(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(ADCCSwitches: TJclDCCSwitches);
    procedure SetDefaultValues;

    property A: TJclDCCASwitch read FAlignedRecordFields;
    property AlignedRecordFields: TJclDCCASwitch read FAlignedRecordFields;
    property AsString: string read GetAsString write SetAsString;
    property B: TJclDCCBooleanSwitch read FFullBooleanEvaluation;
    property C: TJclDCCBooleanSwitch read FEvaluateAssertionsAtRuntime;
    property Count: Integer read GetCount;
    property D: TJclDCCBooleanSwitch read FDebugInformation;
    property DebugInformation: TJclDCCBooleanSwitch read FDebugInformation;
    property EvaluateAssertionsAtRuntime: TJclDCCBooleanSwitch read FEvaluateAssertionsAtRuntime;
    property ExtendedSyntax: TJclDCCBooleanSwitch read FExtendedSyntax;
    property FullBooleanEvaluation: TJclDCCBooleanSwitch read FFullBooleanEvaluation;
    property G: TJclDCCBooleanSwitch read FUseImportedDataReferences;
    property GenerateStackFrames: TJclDCCBooleanSwitch read FGenerateStackFrames;
    property H: TJclDCCBooleanSwitch read FUseLongStringsByDefault;
    property I: TJclDCCBooleanSwitch read FIOChecking;
    property IntegerOverflowChecking: TJclDCCBooleanSwitch read FIntegerOverflowChecking;
    property Items[AIndex: Integer]: TJclDCCSwitch read GetItems; default;
    property J: TJclDCCBooleanSwitch read FWriteableStructuredConsts;
    property IOChecking: TJclDCCBooleanSwitch read FIOChecking;
    property L: TJclDCCBooleanSwitch read FLocalDebugSymbols;
    property LocalDebugSymbols: TJclDCCBooleanSwitch read FLocalDebugSymbols;
    property M: TJclDCCBooleanSwitch read FRuntimeTypeInfo;
    property MinimumSizeOfEnumTypes: TJclDCCZSwitch read FMinimumSizeOfEnumTypes;
    property O: TJclDCCBooleanSwitch read FOptimization;
    property OpenStringParams: TJclDCCBooleanSwitch read FOpenStringParams;
    property Optimization: TJclDCCBooleanSwitch read FOptimization;
    property P: TJclDCCBooleanSwitch read FOpenStringParams;
    property PentiumTMSafeDivide: TJclDCCBooleanSwitch read FPentiumTMSafeDivide;
    property Q: TJclDCCBooleanSwitch read FIntegerOverflowChecking;
    property R: TJclDCCBooleanSwitch read FRangeChecking;
    property RangeChecking: TJclDCCBooleanSwitch read FRangeChecking;
    property RuntimeTypeInfo: TJclDCCBooleanSwitch read FRuntimeTypeInfo;
    property StrictVarStrings: TJclDCCBooleanSwitch read FStrictVarStrings;
    property SymbolReferenceInfo: TJclDCCYSwitch read FSymbolReferenceInfo;
    property T: TJclDCCBooleanSwitch read FTypedAtOperator;
    property TypedAtOperator: TJclDCCBooleanSwitch read FTypedAtOperator;
    property U: TJclDCCBooleanSwitch read FPentiumTMSafeDivide;
    property UseImportedDataReferences: TJclDCCBooleanSwitch read FUseImportedDataReferences;
    property UseLongStringsByDefault: TJclDCCBooleanSwitch read FUseLongStringsByDefault;
    property V: TJclDCCBooleanSwitch read FStrictVarStrings;
    property W: TJclDCCBooleanSwitch read FGenerateStackFrames;
    property WriteableStructuredConsts: TJclDCCBooleanSwitch read FWriteableStructuredConsts;
    property X: TJclDCCBooleanSwitch read FExtendedSyntax;
    property Y: TJclDCCYSwitch read FSymbolReferenceInfo;
    property Z: TJclDCCZSwitch read FMinimumSizeOfEnumTypes;
  end;

  TJclConfigStringMode = (csmWindows, csmKylix);

  TJclConfigStringList = class(TStringList)
  private
    FDefaultStringMode: TJclConfigStringMode;
    function GetAsDefaultString: string;
    function GetAsString(AMode: TJclConfigStringMode): string;
    function GetAsStringWithSeparator(Separator: Char): string;
    function GetSeparator(AMode: TJclConfigStringMode): Char;
    procedure SetAsString(AString: string);
  public
    constructor Create;
    property AsString[AMode: TJclConfigStringMode]: string read GetAsString;
    property AsDefaultString: string read GetAsDefaultString write SetAsString;
    property DefaultStringMode: TJclConfigStringMode read FDefaultStringMode write
      FDefaultStringMode;
  end;

  TJclDCCLinkerOutput = (loDCU, loCObj, loCPPObj);
  TJclDCCLinkerOutputCPPOption = (locoIncludeNamespaces, locoExportAllSymbols);
  TJclDCCLinkerOutputCPPOptions = set of TJclDCCLinkerOutputCPPOption;

  //(usc) need option "MakeModifiedUnits" (-M)?
  //   I guess -M is opposite of -B and it is handled this way in TJclDCCEx.Compile
  TJclCustomDCCConfig = class(TObject)
  private
    FBPLOutputDirectory: string;
    FBuildAllUnits: Boolean;
    FCompilerSwitches: TJclDCCSwitches;
    FCompileWithPackages: Boolean;
    FConditionalDefines: string;
    FConsoleApplication: Boolean;
    FDCPOutputDirectory: string;
    FDCUOutputDir: string;
    FEXEOutputDir: string;
    FImageBaseAddr: DWord;
    FIncludeDirectories: TJclConfigStringList;
    FLinkerOutput: TJclDCCLinkerOutput;
    FLinkerOutputCPPOptions: TJclDCCLinkerOutputCPPOptions;
    FMapFileLevel: TJclDCCMapFileLevel;
    FMaxStackSize: DWord;
    FMinStackSize: DWord;
    FObjectDirectories: TJclConfigStringList;
    FOutputHints: Boolean;
    FOutputWarnings: Boolean;
    FPackages: TJclConfigStringList;
    FRemoteDebugSymbols: Boolean;
    FResourceDirectories: TJclConfigStringList;
    FSearchPaths: TJclConfigStringList;
    FTD32DebugInfo: Boolean;
    FUnitAliases: string;
    FUnitDirectories: TJclConfigStringList;
    procedure HandleChangeSearchPaths(ASender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(ACustomConfig: TJclCustomDCCConfig);

    property BPLOutputDirectory: string read FBPLOutputDirectory write FBPLOutputDirectory;
    property BuildAllUnits: Boolean read FBuildAllUnits write FBuildAllUnits;
    property CompilerSwitches: TJclDCCSwitches read FCompilerSwitches;
    property CompileWithPackages: Boolean read FCompileWithPackages write FCompileWithPackages;
    property ConditionalDefines: string read FConditionalDefines write FConditionalDefines;
    property ConsoleApplication: Boolean read FConsoleApplication write FConsoleApplication;
    property DCPOutputDirectory: string read FDCPOutputDirectory write FDCPOutputDirectory;
    property DCUOutputDirectory: string read FDCUOutputDir write FDCUOutputDir;
    property EXEOutputDirectory: string read FEXEOutputDir write FEXEOutputDir;
    property ImageBaseAddr: DWord read FImageBaseAddr write FImageBaseAddr;
    property IncludeDirectories: TJclConfigStringList read FIncludeDirectories;
    property LinkerOutput: TJclDCCLinkerOutput read FLinkerOutput write FLinkerOutput;
    property LinkerOutputCPPOptions: TJclDCCLinkerOutputCPPOptions read FLinkerOutputCPPOptions write FLinkerOutputCPPOptions;
    property MapFileLevel: TJclDCCMapFileLevel read FMapFileLevel write FMapFileLevel;
    property MaxStackSize: DWord read FMaxStackSize write FMaxStackSize;
    property MinStackSize: DWord read FMinStackSize write FMinStackSize;
    property ObjectDirectories: TJclConfigStringList read FObjectDirectories;
    property OutputHints: Boolean read FOutputHints write FOutputHints;
    property OutputWarnings: Boolean read FOutputWarnings write FOutputWarnings;
    property Packages: TJclConfigStringList read FPackages write FPackages;
    property RemoteDebugSymbols: Boolean read FRemoteDebugSymbols write FRemoteDebugSymbols;
    property ResourceDirectories: TJclConfigStringList read FResourceDirectories;
    property SearchPaths: TJclConfigStringList read FSearchPaths;
    property TD32DebugInfo: Boolean read FTD32DebugInfo write FTD32DebugInfo;
    property UnitAliases: string read FUnitAliases write FUnitAliases;
    property UnitDirectories: TJclConfigStringList read FUnitDirectories;
  end;

  TJclDCCConfigFile = class(TJclCustomDCCConfig)
  public
    procedure LoadFromFile(AFileName: string);
  end;

  TJclDOFSaveQueryEvent = function(ASender: TObject; const ASection, AIdent: string): Boolean of object;

  TJclDOFFile = class(TJclCustomDCCConfig)
  private
    FContent: TStringList;
    function StrippedSaveQuery(ASender: TObject; const ASection, AIdent: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
    procedure SaveToFileQuery(AFileName: string; AQueryProc: TJclDOFSaveQueryEvent);
    procedure SaveToFileStripped(AFileName: string);
  end;

  TJclKOFFile = class(TJclDOFFile)
  public
    constructor Create;
  end;

  TJclDCCMessage = class(TObject)
  private
    FFileName: string;
    FFileNameAndLineNumberEmpty: Boolean;
    FKind: TJclDCCMessageKind;
    FLineNumber: Integer;
    FMessageStr: string;
    function GetText: string;
  public
    constructor Create(AKind: TJclDCCMessageKind; AMessageStr: string; AFileNameAndLineNumberEmpty: Boolean = True;
      AFileName: string = ''; ALineNumber: Integer = -1);
    property FileName: string read FFileName;
    property FileNameAndLineNumberEmpty: Boolean read FFileNameAndLineNumberEmpty;
    property Kind: TJclDCCMessageKind read FKind;
    property LineNumber: Integer read FLineNumber;
    property MessageStr: string read FMessageStr;
    property Text: string read GetText;
  end;

  TJclDCCMessages = class(TObject)
  private
    FErrorCount: Integer;
    FFatalCount: Integer;
    FHintCount: Integer;
    FItems: TObjectList;
    FWarnCount: Integer;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TJclDCCMessage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AKind: TJclDCCMessageKind; AMessageStr: string; AFileNameAndLineNumberEmpty: Boolean = True;
      AFileName: string = ''; ALineNumber: Integer = -1);
    procedure Clear;
    property Count: Integer read GetCount;
    property ErrorCount: Integer read FErrorCount;
    property FatalCount: Integer read FFatalCount;
    property HintCount: Integer read FHintCount;
    property Items[AIndex: Integer]: TJclDCCMessage read GetItems; default;
    property WarnCount: Integer read FWarnCount;
  end;

  TJclDCCEx = class(TObject)
  private
    FConfig: TJclCustomDCCConfig;
    FCurrentFile: string;
    FCurrentLineNo: Integer;
    FExeName: string;
    FFileToCompile: string;
    FMessages: TJclDCCMessages;
    FOnCompileProgress: TNotifyEvent;
    FOnMessage: TNotifyEvent;
    FPlainOutput: TStringList;
    FQuietCompile: Boolean;
    procedure CaptureLine(const Line: string);
    procedure ClearValues;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(AbortPtr: PBoolean = nil): Boolean;
    property Config: TJclCustomDCCConfig read FConfig;
    property CurrentFile: string read FCurrentFile;
    property CurrentLineNo: Integer read FCurrentLineNo;
    property ExeName: string read FExeName write FExeName;
    property FileToCompile: string read FFileToCompile write FFileToCompile;
    property Messages: TJclDCCMessages read FMessages;
    property OnCompileProgress: TNotifyEvent read FOnCompileProgress write FOnCompileProgress;
    property OnMessage: TNotifyEvent read FOnMessage write FOnMessage;
    property PlainOutput: TStringList read FPlainOutput;
    property QuietCompile: Boolean read FQuietCompile write FQuietCompile;
  end;

  TJclBorRADToolIdeExperts = class(TJclBorRADToolInstallationObject)
  private
    FExperts: TStringList;
    function GetCount: Integer;
    function GetFileNames(Index: Integer): string;
    function GetNames(Index: Integer): string;
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation);
    procedure ReadExperts;
  public
    destructor Destroy; override;
    function AddExpert(const AName, AFileName: string): Boolean;
    function RemoveExpert(const AName: string): Boolean;
    property Count: Integer read GetCount;
    property FileNames[Index: Integer]: string read GetFileNames;
    property Names[Index: Integer]: string read GetNames;
  end;

  TJclBorRADToolInstallationEx = class(TJclBorRADToolInstallation)
  private
    FIdeExperts: TJclBorRADToolIdeExperts;
    function GetIdeExperts: TJclBorRADToolIdeExperts;
    function GetVCSManager: string;
    procedure SetVCSManager(const Value: string);
  protected
    constructor Create(const AConfigDataLocation: string); override;
  public
    destructor Destroy; override;
    function SupportsVCSManager: Boolean;
    property IdeExperts: TJclBorRADToolIdeExperts read GetIdeExperts;
    property VCSManager: string read GetVCSManager write SetVCSManager;
  end;

implementation

uses
  {$IFDEF COMPILER5}
  FileCtrl,
  {$ENDIF COMPILER5}
  JclStrings;

//=== { TJclDCCSwitch } ======================================================

constructor TJclDCCSwitch.Create(ASwitchChar: Char);
begin
  inherited Create;
  FSwitchChar := ASwitchChar;
end;

function TJclDCCSwitch.DefaultSetAsString(const AValue: string): Boolean;
begin
  Result := (Length(AValue) > 2) and (AValue[1] = '$') and (AValue[2] = SwitchChar);
  if Result then
    SetDefaultValue;
end;

function TJclDCCSwitch.GetAsBoolean: Boolean;
begin
  Result := False;
end;

function TJclDCCSwitch.GetAsInteger: Integer;
begin
  Result := 0;
end;

function TJclDCCSwitch.GetAsString: string;
begin
  Result := '';
end;

procedure TJclDCCSwitch.SetAsBoolean(AValue: Boolean);
begin
//
end;

procedure TJclDCCSwitch.SetAsInteger(AValue: Integer);
begin
//
end;

procedure TJclDCCSwitch.SetAsString(AValue: string);
begin
//
end;

procedure TJclDCCSwitch.SetDefaultValue;
begin
//
end;

//=== { TJclDCCASwitch } =====================================================

constructor TJclDCCASwitch.Create;
begin
  inherited Create('A');
  FDefaultValue := a8;
  FValue := FDefaultValue;
end;

function TJclDCCASwitch.GetAsBoolean: Boolean;
begin
  Result := not (FValue in [aOff, a1]);
end;

function TJclDCCASwitch.GetAsInteger: Integer;
begin
  case FValue of
    aOff: Result := 0;
    a2: Result := 2;
    a4: Result := 4;
    a8: Result := 8;
    else
      Result := 1;
  end;
end;

function TJclDCCASwitch.GetAsString: string;
begin
  Result := '$' + FSwitchChar;
  case FValue of
    aOn: Result := Result + '+';
    aOff: Result := Result + '-';
    a1: Result := Result + '1';
    a2: Result := Result + '2';
    a4: Result := Result + '4';
    a8: Result := Result + '8';
  end;
end;

procedure TJclDCCASwitch.SetAsBoolean(AValue: Boolean);
begin
  if AValue then
    FValue := aOn
  else
    FValue := aOff;
end;

procedure TJclDCCASwitch.SetAsInteger(AValue: Integer);
begin
  case AValue of
    0: FValue := aOff;
    1: FValue := a1;
    2: FValue := a2;
    4: FValue := a4;
    else
      FValue := a8;
  end;
end;

procedure TJclDCCASwitch.SetAsString(AValue: string);
begin
  if DefaultSetAsString(AValue) then
    case AValue[3] of
      '+': FValue := aOn;
      '-': FValue := aOff;
      '1': FValue := a1;
      '2': FValue := a2;
      '4': FValue := a4;
      '8': FValue := a8;
    end;
end;

procedure TJclDCCASwitch.SetDefaultValue;
begin
  FValue := FDefaultValue;
end;

//=== { TJclDCCBooleanSwitch } ===============================================

constructor TJclDCCBooleanSwitch.Create(ASwitchChar: Char; ADefaultValue: Boolean);
begin
  inherited Create(ASwitchChar);
  FDefaultValue := ADefaultValue;
  FValue := ADefaultValue;
end;

function TJclDCCBooleanSwitch.GetAsBoolean: Boolean;
begin
  Result := FValue;
end;

function TJclDCCBooleanSwitch.GetAsInteger: Integer;
begin
  Result := 0;
  if FValue then
    Result := 1;
end;

function TJclDCCBooleanSwitch.GetAsString: string;
begin
  Result := '$' + FSwitchChar;
  if FValue then
    Result := Result + '+'
  else
    Result := Result + '-';
end;

procedure TJclDCCBooleanSwitch.SetAsBoolean(AValue: Boolean);
begin
  FValue := AValue;
end;

procedure TJclDCCBooleanSwitch.SetAsInteger(AValue: Integer);
begin
  FValue := AValue = 1;
end;

procedure TJclDCCBooleanSwitch.SetAsString(AValue: string);
begin
  if DefaultSetAsString(AValue) then
    case AValue[3] of
      '+': FValue := True;
      '-': FValue := False;
    end;
end;

procedure TJclDCCBooleanSwitch.SetDefaultValue;
begin
  FValue := FDefaultValue;
end;

//=== { TJclDCCYSwitch } =====================================================

constructor TJclDCCYSwitch.Create;
begin
  inherited Create('Y');
  FDefaultValue := yD;
  FValue := FDefaultValue;
end;

function TJclDCCYSwitch.GetAsBoolean: Boolean;
begin
  Result := FValue <> yOff;
end;

function TJclDCCYSwitch.GetAsInteger: Integer;
begin
  Result := 0;//(usc) avoid hint
  case FValue of
    yOff: Result := 0;
    yOn: Result := 2;
    yD: Result := 1;
  end;
end;

function TJclDCCYSwitch.GetAsString: string;
begin
  Result := '$' + FSwitchChar;
  case FValue of
    yOn: Result := Result + '+';
    yOff: Result := Result + '-';
    yD: Result := Result + 'D';
  end;
end;

procedure TJclDCCYSwitch.SetAsBoolean(AValue: Boolean);
begin
  if AValue then
    FValue := yD
  else
    FValue := yOff;
end;

procedure TJclDCCYSwitch.SetAsInteger(AValue: Integer);
begin
  case AValue of
    1: FValue := yD;
    2: FValue := yOn;
    else
      FValue := yOff;
  end;
end;

procedure TJclDCCYSwitch.SetAsString(AValue: string);
begin
  if DefaultSetAsString(AValue) then
    case AValue[3] of
      '+': FValue := yOn;
      '-': FValue := yOff;
      'D': FValue := yD;
    end;
end;

procedure TJclDCCYSwitch.SetDefaultValue;
begin
  FValue := FDefaultValue;
end;

//=== { TJclDCCZSwitch } =====================================================

constructor TJclDCCZSwitch.Create;
begin
  inherited Create('Z');
  FDefaultValue := z1;
  FValue := FDefaultValue;
end;

function TJclDCCZSwitch.GetAsBoolean: Boolean;
begin
  Result := not (FValue in [zOff, z1]);
end;

function TJclDCCZSwitch.GetAsInteger: Integer;
begin
  case FValue of
    zOff, z1: Result := 1;
    z2: Result := 2;
    else
      Result := 4;
  end;
end;

function TJclDCCZSwitch.GetAsString: string;
begin
  Result := '$' + FSwitchChar;
  case FValue of
    zOn: Result := Result + '+';
    zOff: Result := Result + '-';
    z1: Result := Result + '1';
    z2: Result := Result + '2';
    z4: Result := Result + '4';
  end;
end;

procedure TJclDCCZSwitch.SetAsBoolean(AValue: Boolean);
begin
  if AValue then
    FValue := zOn
  else
    FValue := zOff;
end;

procedure TJclDCCZSwitch.SetAsInteger(AValue: Integer);
begin
  case AValue of
    0: FValue := zOff;
    2: FValue := z2;
    4: FValue := z4;
    else
      FValue := z1;
  end;
end;

procedure TJclDCCZSwitch.SetAsString(AValue: string);
begin
  if DefaultSetAsString(AValue) then
    case AValue[3] of
      '+': FValue := zOn;
      '-': FValue := zOff;
      '1': FValue := z1;
      '2': FValue := z2;
      '4': FValue := z4;
    end;
end;

procedure TJclDCCZSwitch.SetDefaultValue;
begin
  FValue := FDefaultValue;
end;

//=== { TJclDCCSwitches } ====================================================

constructor TJclDCCSwitches.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
  CreateSwitches;
end;

destructor TJclDCCSwitches.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TJclDCCSwitches.Assign(ADCCSwitches: TJclDCCSwitches);
var
  I: Integer;
begin
  if Count = ADCCSwitches.Count then
    for I := 0 to Pred(Count) do
      Items[I].AsString := ADCCSwitches[I].AsString;
end;

procedure TJclDCCSwitches.CreateSwitches;
  function CreateJclDCCBooleanSwitchAndAdd(ASwitchChar: Char; ADefaultValue: Boolean): TJclDCCBooleanSwitch;
  begin
    FItems.Add(TJclDCCBooleanSwitch.Create(ASwitchChar, ADefaultValue));
    Result := TJclDCCBooleanSwitch(FItems.Last);
  end;
begin
  FItems.Add(TJclDCCASwitch.Create);
  FAlignedRecordFields := TJclDCCASwitch(FItems.Last);
  FFullBooleanEvaluation := CreateJclDCCBooleanSwitchAndAdd('B', False);
  FEvaluateAssertionsAtRuntime := CreateJclDCCBooleanSwitchAndAdd('C', True);
  FDebugInformation := CreateJclDCCBooleanSwitchAndAdd('D', True);
  FUseImportedDataReferences := CreateJclDCCBooleanSwitchAndAdd('G', True);
  FUseLongStringsByDefault := CreateJclDCCBooleanSwitchAndAdd('H', True);
  FIOChecking := CreateJclDCCBooleanSwitchAndAdd('I', True);
  FWriteableStructuredConsts := CreateJclDCCBooleanSwitchAndAdd('J', False);
  FLocalDebugSymbols := CreateJclDCCBooleanSwitchAndAdd('L', True);
  FRuntimeTypeInfo := CreateJclDCCBooleanSwitchAndAdd('M', False);
  FOptimization := CreateJclDCCBooleanSwitchAndAdd('O', True);
  FOpenStringParams := CreateJclDCCBooleanSwitchAndAdd('P', True);
  FIntegerOverflowChecking := CreateJclDCCBooleanSwitchAndAdd('Q', False);
  FRangeChecking := CreateJclDCCBooleanSwitchAndAdd('R', False);
  FTypedAtOperator := CreateJclDCCBooleanSwitchAndAdd('T', False);
  FPentiumTMSafeDivide := CreateJclDCCBooleanSwitchAndAdd('U', False);
  FStrictVarStrings := CreateJclDCCBooleanSwitchAndAdd('V', True);
  FGenerateStackFrames := CreateJclDCCBooleanSwitchAndAdd('W', False);
  FExtendedSyntax := CreateJclDCCBooleanSwitchAndAdd('X', True);
  FItems.Add(TJclDCCYSwitch.Create);
  FSymbolReferenceInfo := TJclDCCYSwitch(FItems.Last);
  FItems.Add(TJclDCCZSwitch.Create);
  FMinimumSizeOfEnumTypes := TJclDCCZSwitch(FItems.Last);
end;

function TJclDCCSwitches.GetAsString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + '-' + Items[I].AsString;
  end;
end;

function TJclDCCSwitches.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclDCCSwitches.GetItems(AIndex: Integer): TJclDCCSwitch;
begin
  Result := TJclDCCSwitch(FItems[AIndex]);
end;

procedure TJclDCCSwitches.SetAsString(AValue: string);
var
  ValueTokens: TStringList;
  S: string;
  I, J: Integer;
begin
  ValueTokens := TStringList.Create;
  try
    StrTokenToStrings(AValue, ' ', ValueTokens);
    for I := 0 to Pred(ValueTokens.Count) do
    begin
      S := ValueTokens[I];
      if (Length(S) > 2) and (Pos('-$', S) = 1) then
      begin
        for J := 0 to Pred(Count) do
          if Items[J].SwitchChar = S[3] then
          begin
            Delete(S, 1, 1);
            Items[J].AsString := S;
            Break;
          end;
      end;
    end;
  finally
    ValueTokens.Free;
  end;
end;

procedure TJclDCCSwitches.SetDefaultValues;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].SetDefaultValue;
end;

//=== { TJclConfigStringList } ===============================================

constructor TJclConfigStringList.Create;
begin
  inherited Create;
  FDefaultStringMode := {$IFDEF KYLIX} csmKylix {$ELSE} csmWindows {$ENDIF};
end;

function TJclConfigStringList.GetAsDefaultString: string;
begin
  Result := GetAsStringWithSeparator(GetSeparator(FDefaultStringMode));
end;

function TJclConfigStringList.GetAsString(AMode: TJclConfigStringMode): string;
begin
  Result := GetAsStringWithSeparator(GetSeparator(AMode));
end;

function TJclConfigStringList.GetAsStringWithSeparator(Separator: Char): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if I > 0 then
      Result := Result + Separator;
    Result := Result + Strings[I];
  end;
end;

function TJclConfigStringList.GetSeparator(AMode: TJclConfigStringMode): Char;
begin
  if AMode = csmKylix then
    Result := ':'
  else
    Result := ';';
end;

procedure TJclConfigStringList.SetAsString(AString: string);
var
  NextDelimiterPos: Integer;
  S, tempStr: string;
begin
  BeginUpdate;
  try
    Clear;
    tempStr := AString;
    while tempStr <> '' do
    begin
      NextDelimiterPos := Pos(';', tempStr);
      if NextDelimiterPos = 0 then
        NextDelimiterPos := Pos(',', tempStr);
      if (NextDelimiterPos = 0) and (FDefaultStringMode = csmKylix) then
        NextDelimiterPos := Pos(':', tempStr);
      if NextDelimiterPos = 0 then
      begin
        S := tempStr;
        tempStr := '';
      end
      else
      begin
        S := Copy(tempStr, 1, NextDelimiterPos - 1);
        System.Delete(tempStr, 1, NextDelimiterPos);
      end;
      S := Trim(S);
      Add(S);
    end;
  finally
    EndUpdate;
  end;
end;

//=== { TJclCustomDCCConfig } ================================================

constructor TJclCustomDCCConfig.Create;
begin
  inherited Create;
  FBPLOutputDirectory := '';
  FBuildAllUnits := True;
  FCompilerSwitches := TJclDCCSwitches.Create;
  FCompileWithPackages := False;
  FConditionalDefines := '';
  FConsoleApplication := False;
  FDCPOutputDirectory := '';
  FDCUOutputDir:= '';
  FEXEOutputDir:= '';
  FImageBaseAddr := $400000;
  FIncludeDirectories := TJclConfigStringList.Create;
  FLinkerOutput := loDCU;
  FLinkerOutputCPPOptions := [];
  FMapFileLevel := mfloff;
  FMaxStackSize := 1048576; //old TJclDCCExValue $100000 (same value as hex)
  FMinStackSize := 16384; //old TJclDCCExValue $4000 (same value as hex)
  FObjectDirectories := TJclConfigStringList.Create;
  FOutputHints := True; //old TJclDCCExValue False
  FOutputWarnings := True; //old TJclDCCExValue False
  FPackages := TJclConfigStringList.Create;
  FRemoteDebugSymbols := False;
  FResourceDirectories := TJclConfigStringList.Create;
  FSearchPaths := TJclConfigStringList.Create;
  FTD32DebugInfo := False;
  FUnitAliases := '';
  FUnitDirectories := TJclConfigStringList.Create;
  FSearchPaths.OnChange := HandleChangeSearchPaths;
end;

destructor TJclCustomDCCConfig.Destroy;
begin
  FUnitDirectories.Free;
  FResourceDirectories.Free;
  FIncludeDirectories.Free;
  FObjectDirectories.Free;
  FSearchPaths.Free;
  FPackages.Free;
  FCompilerSwitches.Free;
  inherited Destroy;
end;

procedure TJclCustomDCCConfig.Assign(ACustomConfig: TJclCustomDCCConfig);
begin
  FBPLOutputDirectory := ACustomConfig.BPLOutputDirectory;
  FBuildAllUnits := ACustomConfig.BuildAllUnits;
  FCompilerSwitches := ACustomConfig.CompilerSwitches;
  FCompileWithPackages := ACustomConfig.CompileWithPackages;
  FConditionalDefines := ACustomConfig.ConditionalDefines;
  FConsoleApplication := ACustomConfig.ConsoleApplication;
  FDCPOutputDirectory := ACustomConfig.DCPOutputDirectory;
  FDCUOutputDir := ACustomConfig.DCUOutputDirectory;
  FEXEOutputDir := ACustomConfig.EXEOutputDirectory;
  FImageBaseAddr := ACustomConfig.ImageBaseAddr;
  FIncludeDirectories.Assign(ACustomConfig.IncludeDirectories);
  FLinkerOutput := ACustomConfig.LinkerOutput;
  FLinkerOutputCPPOptions := ACustomConfig.LinkerOutputCPPOptions;
  FMapFileLevel := ACustomConfig.MapFileLevel;
  FMaxStackSize := ACustomConfig.MaxStackSize;
  FMinStackSize := ACustomConfig.MinStackSize;
  FObjectDirectories.Assign(ACustomConfig.ObjectDirectories);
  FOutputHints := ACustomConfig.OutputHints;
  FOutputWarnings := ACustomConfig.OutputWarnings;
  FPackages.Assign(ACustomConfig.Packages);
  FRemoteDebugSymbols := ACustomConfig.RemoteDebugSymbols;
  FResourceDirectories.Assign(ACustomConfig.ResourceDirectories);
  FSearchPaths.Assign(ACustomConfig.SearchPaths);
  FTD32DebugInfo := ACustomConfig.TD32DebugInfo;
  FUnitAliases := ACustomConfig.UnitAliases;
  FUnitDirectories.Assign(ACustomConfig.UnitDirectories);
end;

procedure TJclCustomDCCConfig.HandleChangeSearchPaths(ASender: TObject);
begin
  if ASender is TStrings then
  begin
    FUnitDirectories.Assign(TStrings(ASender));
    FObjectDirectories.Assign(TStrings(ASender));
    FIncludeDirectories.Assign(TStrings(ASender));
    FResourceDirectories.Assign(TStrings(ASender));
  end;
end;

procedure TJclDCCConfigFile.LoadFromFile(AFileName: string);

  function CheckPathOption(const AOptionPrefix: string; AOption: string; var ADestPath: string): Boolean;
  begin
    Result := Pos(AOptionPrefix, AOption) = 1;
    if Result then
    begin
      Delete(AOption, 1, Length(AOptionPrefix));
      ADestPath := StrTrimQuotes(AOption);
    end;
  end;

var
  ConfigStrings: TStringList;
  I: Integer;
  S, S2: string;
  CompilerSwitchesStr: string;
  LOCPPOptions: TJclDCCLinkerOutputCPPOptions;
begin
  ConfigStrings := TStringList.Create;
  try
    ConfigStrings.LoadFromFile(AFileName);
    MapFileLevel := mfloff;
    LinkerOutput := loDCU;
    LinkerOutputCPPOptions := [];
    CompilerSwitchesStr := '';
    for I := 0 to Pred(ConfigStrings.Count) do
    begin
      S := ConfigStrings[I];
      if (Pos('-$', S) = 1) and (Length(S) = 4) and (Pos(S, CompilerSwitchesStr) = 0) then
      begin
        if CompilerSwitchesStr <> '' then
          CompilerSwitchesStr := CompilerSwitchesStr + ' ';
        CompilerSwitchesStr := CompilerSwitchesStr + S;
      end
      else
      if Pos('-GS', S) = 1 then
        MapFileLevel := mflsegments
      else
      if Pos('-GP', S) = 1 then
        MapFileLevel := mflpublics
      else
      if Pos('-GD', S) = 1 then
        MapFileLevel := mfldetailed
      else
      if S = '-J' then
        LinkerOutput := loCObj
      else
      if Pos('-JP', S) = 1 then
      begin
        LinkerOutput := loCPPObj;
        LOCPPOptions := LinkerOutputCPPOptions;
        if Pos('N', S) > 0 then
          Include(LOCPPOptions, locoIncludeNamespaces);
        if Pos('E', S) > 0 then
          Include(LOCPPOptions, locoExportAllSymbols);
        LinkerOutputCPPOptions := LOCPPOptions;
      end
      else
      if Pos('-cc', S) = 1 then
        ConsoleApplication := True
      else
      if Pos('-cg', S) = 1 then
        ConsoleApplication := False
      else
      if Pos('-vn', S) = 1 then
        TD32DebugInfo := True
      else
      if Pos('-vr', S) = 1 then
        RemoteDebugSymbols := True
      else
      if Pos('-A', S) = 1 then
      begin
        Delete(S, 1, 2);
        UnitAliases := S;
      end
      else
      if Pos('-H', S) = 1 then
        OutputHints := Pos('+', S) = 3
      else
      if Pos('-W', S) = 1 then
        OutputWarnings := Pos('+', S) = 3
      else
      if Pos('-M', S) = 1 then
        BuildAllUnits := False
      else
      if Pos('-B', S) = 1 then //(usc) not found in .cfg file - but used as opposite of -M?
        BuildAllUnits := True
      else
      if (Pos('-$M', S) = 1) and (Pos('-$M-', S) = 0) and (Pos('-$M+', S) = 0) and
        (Pos(',', S) > 3) then
      begin
        Delete(S, 1, 3);
        S2 := Copy(S, 1, Pos(',', S) - 1);
        MinStackSize := StrToIntDef(S2, MinStackSize);
        S2 := S;
        Delete(S2, 1, Pos(',', S));
        MaxStackSize := StrToIntDef(S2, MaxStackSize);
      end
      else
      if Pos('-K', S) = 1 then
      begin
        Delete(S, 1, 2);
        ImageBaseAddr := StrToIntDef(S, ImageBaseAddr);
      end
      else
      if CheckPathOption('-E', S, S2) then
        EXEOutputDirectory := S2
      else
      if CheckPathOption('-N', S, S2) then
        DCUOutputDirectory := S2
      else
      if CheckPathOption('-LE', S, S2) then
        BPLOutputDirectory := S2
      else
      if CheckPathOption('-LN', S, S2) then
        DCPOutputDirectory := S2
      else
      if CheckPathOption('-U', S, S2) then
        UnitDirectories.AsDefaultString := S2
      else
      if CheckPathOption('-O', S, S2) then
        ObjectDirectories.AsDefaultString := S2
      else
      if CheckPathOption('-I', S, S2) then
        IncludeDirectories.AsDefaultString := S2
      else
      if CheckPathOption('-R', S, S2) then
        ResourceDirectories.AsDefaultString := S2
      else
      if Pos('-D', S) = 1 then
      begin
        Delete(S, 1, 2);
        ConditionalDefines := S;
      end
      else
      if Pos('-LU', S) = 1 then
      begin
        Delete(S, 1, 3);
        Packages.AsDefaultString := S;
        CompileWithPackages := S <> '';
      end;
    end;
  finally
    ConfigStrings.Free;
  end;
  CompilerSwitches.AsString := CompilerSwitchesStr;
end;

//=== { TJclDOFFile } ========================================================

const
  JclDOFCompilerSection = 'Compiler';
  JclDOFLinkerSection = 'Linker';
  JclDOFDirectoriesSection = 'Directories';

  JclDOFUnitAliasesEntry = 'UnitAliases';
  JclDOFShowHintsEntry = 'ShowHints';
  JclDOFShowWarningsEntry = 'ShowWarnings';

  JclDOFMapFileEntry = 'MapFile';
  JclDOFOutputObjsEntry = 'OutputObjs';
  JclDOFConsoleAppEntry = 'ConsoleApp';
  JclDOFDebugInfoEntry = 'DebugInfo';
  JclDOFRemoteSymbolsEntry = 'RemoteSymbols';
  JclDOFMinStackSizeEntry = 'MinStackSize';
  JclDOFMaxStackSizeEntry = 'MaxStackSize';
  JclDOFImageBaseEntry = 'ImageBase';

  JclDOFOutputDirEntry = 'OutputDir';
  JclDOFUnitOutputDirEntry = 'UnitOutputDir';
  JclDOFPackageDLLOutputDirEntry = 'PackageDLLOutputDir';
  JclDOFPackageDCPOutputDirEntry = 'PackageDCPOutputDir';
  JclDOFSearchPathEntry = 'SearchPath';
  JclDOFConditionalsEntry = 'Conditionals';
  JclDOFPackagesEntry = 'Packages';
  JclDOFUsePackagesEntry = 'UsePackages';

constructor TJclDOFFile.Create;
begin
  inherited Create;
  FContent := TStringList.Create;
end;

destructor TJclDOFFile.Destroy;
begin
  FContent.Free;
  inherited Destroy;
end;

procedure TJclDOFFile.LoadFromFile(AFileName: string);

  function CheckPathOption(const AOptionPrefix: string; AOption: string; var ADestPath: string): Boolean;
  begin
    Result := Pos(AOptionPrefix, AOption) = 1;
    if Result then
    begin
      Delete(AOption, 1, Length(AOptionPrefix));
      ADestPath := StrTrimQuotes(AOption);
    end;
  end;

var
  I: Integer;
  DOFFile: TMemIniFile;
  OutputObjsValue: Integer;
  LOCPPOptions: TJclDCCLinkerOutputCPPOptions;
begin
  DOFFile := TMemIniFile.Create(AFileName);
  try
    FContent.Clear;
    DOFFile.GetStrings(FContent);
    for I := 0 to Pred(CompilerSwitches.Count) do
      CompilerSwitches[I].AsInteger := DOFFile.ReadInteger(JclDOFCompilerSection, CompilerSwitches[I].SwitchChar, CompilerSwitches[I].AsInteger);
    case DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFMapFileEntry, 0) of
      0: MapFileLevel := mfloff;
      1: MapFileLevel := mflsegments;
      2: MapFileLevel := mflpublics;
      3: MapFileLevel := mfldetailed;
    end;
    OutputObjsValue := DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFOutputObjsEntry, 0);
    case OutputObjsValue of
      9:
         begin
           LinkerOutput := loCObj;
           LinkerOutputCPPOptions := [];
         end;
      10, 14, 26, 30:
         begin
           LinkerOutput := loCPPObj;
           LOCPPOptions := [];
           if OutputObjsValue and 4 <> 0 then
             Include(LOCPPOptions, locoIncludeNamespaces);
           if OutputObjsValue and 16 <> 0 then
             Include(LOCPPOptions, locoExportAllSymbols);
           LinkerOutputCPPOptions := LOCPPOptions;
         end;
      else
      begin
        LinkerOutput := loDCU;
        LinkerOutputCPPOptions := [];
      end;
    end;
    ConsoleApplication := DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFConsoleAppEntry, 0) = 1;
    TD32DebugInfo := DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFDebugInfoEntry, 0) = 1;
    RemoteDebugSymbols := DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFRemoteSymbolsEntry, 0) = 1;

    UnitAliases := DOFFile.ReadString(JclDOFCompilerSection, JclDOFUnitAliasesEntry, '');
    OutputHints := DOFFile.ReadInteger(JclDOFCompilerSection, JclDOFShowHintsEntry, 0) = 1;
    OutputWarnings := DOFFile.ReadInteger(JclDOFCompilerSection, JclDOFShowWarningsEntry, 0) = 1;

    //.dof file seams to have no -M or -B equivalent

    MinStackSize := DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFMinStackSizeEntry, MinStackSize);
    MaxStackSize := DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFMaxStackSizeEntry, MaxStackSize);
    ImageBaseAddr := DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFImageBaseEntry, ImageBaseAddr);

    EXEOutputDirectory := DOFFile.ReadString(JclDOFDirectoriesSection, JclDOFOutputDirEntry, '');
    DCUOutputDirectory := DOFFile.ReadString(JclDOFDirectoriesSection, JclDOFUnitOutputDirEntry, '');
    BPLOutputDirectory := DOFFile.ReadString(JclDOFDirectoriesSection, JclDOFPackageDLLOutputDirEntry, '');
    DCPOutputDirectory := DOFFile.ReadString(JclDOFDirectoriesSection, JclDOFPackageDCPOutputDirEntry, '');

    SearchPaths.AsDefaultString := DOFFile.ReadString(JclDOFDirectoriesSection, JclDOFSearchPathEntry, '');

    ConditionalDefines := DOFFile.ReadString(JclDOFDirectoriesSection, JclDOFConditionalsEntry, '');
    Packages.AsDefaultString := DOFFile.ReadString(JclDOFDirectoriesSection, JclDOFPackagesEntry, '');
    CompileWithPackages := DOFFile.ReadInteger(JclDOFDirectoriesSection, JclDOFUsePackagesEntry, 0) = 1;
  finally
    DOFFile.Free;
  end;
end;

procedure TJclDOFFile.SaveToFile(AFileName: string);
begin
  SaveToFileQuery(AFileName, nil);
end;

procedure TJclDOFFile.SaveToFileQuery(AFileName: string; AQueryProc: TJclDOFSaveQueryEvent);

  function DoQuery(const ASection, AIdent: string): Boolean;
  begin
    Result := (not Assigned(AQueryProc)) or AQueryProc(Self, ASection, AIdent);
  end;

var
  DOFFile: TMemIniFile;
  tempInt: Integer;
  I, J: Integer;
  Sections, Idents: TStringList;
begin
  DOFFile := TMemIniFile.Create(AFileName);
  try
    DOFFile.SetStrings(FContent);
    DOFFile.WriteString(JclDOFCompilerSection, JclDOFUnitAliasesEntry, UnitAliases);
    DOFFile.WriteBool(JclDOFCompilerSection, JclDOFShowHintsEntry, OutputHints);
    DOFFile.WriteBool(JclDOFCompilerSection, JclDOFShowWarningsEntry, OutputWarnings);
    case MapFileLevel of
      mfloff: tempInt := 0;
      mflsegments: tempInt := 1;
      mflpublics: tempInt := 2;
      mfldetailed: tempInt := 3;
      else
        tempInt := 0;
    end;
    DOFFile.WriteInteger(JclDOFLinkerSection, JclDOFMapFileEntry, tempInt);
    DOFFile.WriteBool(JclDOFLinkerSection, JclDOFConsoleAppEntry, ConsoleApplication);
    DOFFile.WriteBool(JclDOFLinkerSection, JclDOFDebugInfoEntry, TD32DebugInfo);
    DOFFile.WriteBool(JclDOFLinkerSection, JclDOFRemoteSymbolsEntry, RemoteDebugSymbols);
    DOFFile.WriteInteger(JclDOFLinkerSection, JclDOFMinStackSizeEntry, MinStackSize);
    DOFFile.WriteInteger(JclDOFLinkerSection, JclDOFMaxStackSizeEntry, MaxStackSize);
    DOFFile.WriteInteger(JclDOFLinkerSection, JclDOFImageBaseEntry, ImageBaseAddr);

    DOFFile.WriteString(JclDOFDirectoriesSection, JclDOFOutputDirEntry, EXEOutputDirectory);
    DOFFile.WriteString(JclDOFDirectoriesSection, JclDOFUnitOutputDirEntry, DCUOutputDirectory);
    DOFFile.WriteString(JclDOFDirectoriesSection, JclDOFPackageDLLOutputDirEntry, BPLOutputDirectory);
    DOFFile.WriteString(JclDOFDirectoriesSection, JclDOFPackageDCPOutputDirEntry, DCPOutputDirectory);
    DOFFile.WriteString(JclDOFDirectoriesSection, JclDOFSearchPathEntry, SearchPaths.AsDefaultString);
    DOFFile.WriteString(JclDOFDirectoriesSection, JclDOFConditionalsEntry, ConditionalDefines);
    DOFFile.WriteString(JclDOFDirectoriesSection, JclDOFPackagesEntry, Packages.AsDefaultString);
    DOFFile.WriteBool(JclDOFLinkerSection, JclDOFUsePackagesEntry, CompileWithPackages);

    Sections := TStringList.Create;
    try
      DOFFile.ReadSections(Sections);
      for I := 0 to Sections.Count - 1 do
        if not DoQuery(Sections[I], '') then
          DOFFile.EraseSection(Sections[I])
        else
        begin
          Idents := TStringList.Create;
          try
            DOFFile.ReadSection(Sections[I], Idents);
            for J := 0 to Idents.Count - 1 do
              if not DoQuery(Sections[I], Idents[J]) then
                DOFFile.DeleteKey(Sections[I], Idents[J]);
          finally
            Idents.Free;
          end;
        end;
    finally
      Sections.Free;
    end;
    DOFFile.UpdateFile;
  finally
    DOFFile.Free;
  end;
end;

procedure TJclDOFFile.SaveToFileStripped(AFileName: string);
begin
  SaveToFileQuery(AFileName, StrippedSaveQuery);
end;

function TJclDOFFile.StrippedSaveQuery(ASender: TObject; const ASection, AIdent: string): Boolean;
begin
  Result := False;
  if ASection = JclDOFDirectoriesSection then
  begin
    if AIdent = '' then
      Result := True
    else
    if AIdent = JclDOFOutputDirEntry then
      Result := EXEOutputDirectory <> ''
    else
    if AIdent = JclDOFUnitOutputDirEntry then
      Result := DCUOutputDirectory <> ''
    else
    if AIdent = JclDOFPackageDLLOutputDirEntry then
      Result := BPLOutputDirectory <> ''
    else
    if AIdent = JclDOFPackageDCPOutputDirEntry then
      Result := DCPOutputDirectory <> ''
    else
    if AIdent = JclDOFSearchPathEntry then
      Result := SearchPaths.Count > 0
    else
    if AIdent = JclDOFConditionalsEntry then
      Result := ConditionalDefines <> ''
    else
    if (AIdent = JclDOFPackagesEntry) or (AIdent = JclDOFUsePackagesEntry) then
      Result := (Packages.Count > 0) and CompileWithPackages;
  end;
end;

//=== { TJclKOFFile } ========================================================

constructor TJclKOFFile.Create;
begin
  inherited Create;
end;

//=== { TJclDCCMessage } =====================================================

type
  TJclMessageConversionRec = record
    MessageString: string;
    MessageKind: TJclDCCMessageKind;
  end;

const
  MessageConversionArray: array [0..9] of TJclMessageConversionRec =
  (
   //english
   (MessageString: 'hint: '; MessageKind: mkHint), //do not localize
   //german
   (MessageString: 'hinweis: '; MessageKind: mkHint), //do not localize
   //french
   (MessageString: 'suggestion: '; MessageKind: mkHint), //do not localize
   //english
   (MessageString: 'warning: '; MessageKind: mkWarning), //do not localize
   //german
   (MessageString: 'warnung: '; MessageKind: mkWarning), //do not localize
   //french
   (MessageString: 'avertissement: '; MessageKind: mkWarning), //do not localize
   //english
   (MessageString: 'error: '; MessageKind: mkError), //do not localize
   //german
   (MessageString: 'fehler: '; MessageKind: mkError), //do not localize
   //french
   (MessageString: 'erreur: '; MessageKind: mkError), //do not localize
   //english, german, french
   (MessageString: 'fatal: '; MessageKind: mkFatal) //do not localize
  );

function GetMessageFromLine(const ALine: string; const ACheckValue: string;
  var AMessage: string): Boolean;
var
  p1: Integer;
begin
  Result := False;
  p1 := Pos(ACheckValue, AnsiLowerCase(ALine));
  if p1 > 0 then
  begin
    p1 := p1 + Length(ACheckValue);
    AMessage := Copy(ALine, p1, Length(ALine) - p1 + 1);
    Result := True;
  end;
end;

function CheckLineForMessages(const ALine: string; var AMessage: string): TJclDCCMessageKind;
var
  I: Integer;
begin
  Result := mkUnknown;
  for I := Low(MessageConversionArray) to High(MessageConversionArray) do
    with MessageConversionArray[I] do
      if GetMessageFromLine(ALine, MessageString, AMessage) then
      begin
        Result := MessageKind;
        Break;
      end;
end;

function GetMessagePos(const ALine: string): Integer;
var
  I: Integer;
begin
  for I := Low(MessageConversionArray) to High(MessageConversionArray) do
    with MessageConversionArray[I] do
    begin
      Result := Pos(MessageString, AnsiLowerCase(ALine));
      if Result > 0 then
        Break;
    end;
end;

constructor TJclDCCMessage.Create(AKind: TJclDCCMessageKind; AMessageStr: string;
  AFileNameAndLineNumberEmpty: Boolean = True; AFileName: string = ''; ALineNumber: Integer = -1);
begin
  inherited Create;
  if not AFileNameAndLineNumberEmpty then
    FFileName := AFileName
  else
    FFileName := '';
  FFileNameAndLineNumberEmpty := AFileNameAndLineNumberEmpty;
  FKind := AKind;
  if not AFileNameAndLineNumberEmpty then
    FLineNumber := ALineNumber
  else
    FLineNumber := -1;
  FMessageStr := AMessageStr;
end;

function TJclDCCMessage.GetText: string;
var
  KindStr, FilePartStr: string;
begin
  case FKind of
    mkHint: KindStr :='[Hint]';
    mkWarning: KindStr := '[Warning]';
    mkError: KindStr := '[Error]';
    mkFatal: KindStr := '[Fatal Error]';
    else
      KindStr := '';
  end;

  FilePartStr := '';
  if not FFileNameAndLineNumberEmpty then
    FilePartStr := Format('%s(%d): ', [ExtractFileName(FFileName), FLineNumber]);
  Result := Format('%s %s%s', [KindStr, FilePartStr, FMessageStr]);
end;

//=== { TJclDCCMessages } ====================================================

constructor TJclDCCMessages.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
  Clear;
end;

destructor TJclDCCMessages.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TJclDCCMessages.Add(AKind: TJclDCCMessageKind; AMessageStr: string; AFileNameAndLineNumberEmpty: Boolean = True;
  AFileName: string = ''; ALineNumber: Integer = -1);
var
  DCCMessage: TJclDCCMessage;
begin
  DCCMessage := TJclDCCMessage.Create(AKind, AMessageStr, AFileNameAndLineNumberEmpty, AFileName, ALineNumber);
  FItems.Add(DCCMessage);

  case AKind of
    mkHint: Inc(FHintCount);
    mkWarning: Inc(FWarnCount);
    mkError: Inc(FErrorCount);
    mkFatal: Inc(FErrorCount);
  end;
end;

procedure TJclDCCMessages.Clear;
begin
  FItems.Clear;
  FErrorCount := 0;
  FHintCount := 0;
  FWarnCount := 0;
  FFatalCount := 0;
end;

function TJclDCCMessages.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclDCCMessages.GetItems(AIndex: Integer): TJclDCCMessage;
begin
  Result := TJclDCCMessage(FItems[AIndex]);
end;

//=== { TJclDCCEx } ==========================================================

constructor TJclDCCEx.Create;
begin
  inherited Create;
  FPlainOutput := TStringList.Create;
  FMessages := TJclDCCMessages.Create;
  ClearValues;
  FOnCompileProgress := nil;
  FQuietCompile := False;
  FOnMessage := nil;
  FConfig := TJclCustomDCCConfig.Create;
end;

destructor TJclDCCEx.Destroy;
begin
  FConfig.Free;
  FMessages.Free;
  FPlainOutput.Free;
  inherited Destroy;
end;

procedure TJclDCCEx.CaptureLine(const Line: string);
var
  p1, p2: Integer;
  S: string;
  LineErr: TJclDCCMessageKind;
  HasPos: Boolean;
  LineMsg: string;
begin
  FPlainOutput.Add(Line);
  HasPos := False;
  p1 := Pos('(', Line);
  if p1 > 0 then
  begin
    p2 := Pos(')', Line);
    if (p2 > p1) and
      ((p2 + 1 = Length(Line)) or (GetMessagePos(Line) - 2 = p2)) then
    begin
      S := Line;
      FCurrentFile := Copy(S, 1, p1 - 1);
      S := Copy(S, p1 + 1, p2 - p1 - 1);
      FCurrentLineNo := StrToIntDef(S, 0);
      HasPos := True;
    end;
  end;
  LineErr := CheckLineForMessages(Line, LineMsg);
  if LineErr <> mkUnknown then
  begin
    FMessages.Add(LineErr, LineMsg, not HasPos, FCurrentFile, FCurrentLineNo);
    if Assigned(FOnMessage) then
      FOnMessage(nil);
  end;
  if Assigned(FOnCompileProgress) and HasPos then
    FOnCompileProgress(nil);
end;

procedure TJclDCCEx.ClearValues;
begin
  FPlainOutput.Clear;
  FCurrentFile := '';
  FCurrentLineNo := 0;
  FMessages.Clear;
end;

function TJclDCCEx.Compile(AbortPtr: PBoolean = nil): Boolean;
var
  Arguments, S: string;
  DCCExitCode: Integer;
begin
{(usc)
 open arguments
 - F
 - P
 - Z
}
  ClearValues;
  Result := False;
  if FileExists(FFileToCompile) and (FExeName <> '') then
  begin
    Arguments := FFileToCompile;
    if (FConfig.EXEOutputDirectory <> '') and DirectoryExists(FConfig.EXEOutputDirectory) then
      Arguments := Arguments + Format(' -E%s', [FConfig.EXEOutputDirectory]);
    if (FConfig.DCUOutputDirectory <> '') and DirectoryExists(FConfig.DCUOutputDirectory) then
      Arguments := Arguments + Format(' -N%s', [FConfig.DCUOutputDirectory]);
    Arguments := Arguments + Format(' -O%s', [FConfig.ObjectDirectories.AsDefaultString]);
    Arguments := Arguments + Format(' -I%s', [FConfig.IncludeDirectories.AsDefaultString]);
    Arguments := Arguments + Format(' -R%s', [FConfig.ResourceDirectories.AsDefaultString]);
    Arguments := Arguments + Format(' -U%s', [FConfig.UnitDirectories.AsDefaultString]);
    if FConfig.CompileWithPackages and (FConfig.Packages.Count > 0) then
      Arguments := Arguments + Format(' -LU%s', [FConfig.Packages]);
    if (FConfig.BPLOutputDirectory <> '') and DirectoryExists(FConfig.BPLOutputDirectory) then
      Arguments := Arguments + Format(' -LE%s', [FConfig.BPLOutputDirectory]);
    if (FConfig.DCPOutputDirectory <> '') and DirectoryExists(FConfig.DCPOutputDirectory) then
      Arguments := Arguments + Format(' -LN%s', [FConfig.DCPOutputDirectory]);
    if FConfig.ConditionalDefines <> '' then
      Arguments := Arguments + Format(' -D%s', [FConfig.ConditionalDefines]);
    case FConfig.MapFileLevel of
      mflsegments: Arguments := Arguments + ' -GS';
      mflpublics: Arguments := Arguments + ' -GP';
      mfldetailed: Arguments := Arguments + ' -GD';
    end;
    if FConfig.LinkerOutput = loCObj then
      Arguments := Arguments + ' -J'
    else
    if FConfig.LinkerOutput = loCPPObj then
    begin
      S := ' -JP';
      if locoIncludeNamespaces in FConfig.LinkerOutputCPPOptions then
        S := S + 'N';
      if locoExportAllSymbols in FConfig.LinkerOutputCPPOptions then
        S := S + 'E';
      Arguments := Arguments + S;
    end;
    if FConfig.BuildAllUnits then
      Arguments := Arguments + ' -B'
    else
      Arguments := Arguments + ' -M';
    Arguments := Arguments + ' ' + FConfig.CompilerSwitches.AsString;
    if FConfig.UnitAliases <> '' then
      Arguments := Arguments + ' -A' + FConfig.UnitAliases;
    if FConfig.OutputWarnings then
      Arguments := Arguments + ' -W+'
    else
      Arguments := Arguments + ' -W-';    
    if FConfig.OutputHints then
      Arguments := Arguments + ' -H+'
    else
      Arguments := Arguments + ' -H-';    
    if FConfig.ConsoleApplication then
      Arguments := Arguments + ' -CC'
    else
      Arguments := Arguments + ' -CG';
    //-V does add TD32 debug informations as well but the executable size is a bit lower
    // and the IDE always writes -vn into the .cfg file
    if FConfig.TD32DebugInfo then
      Arguments := Arguments + ' -VN';
    if FConfig.RemoteDebugSymbols then
      Arguments := Arguments + ' -VR';

    //values must be decimal
    Arguments := Arguments + Format(' -$M%d,%d', [FConfig.MinStackSize, FConfig.MaxStackSize]);
    //value must be hexadecimal
    Arguments := Arguments + Format(' -K%.8x', [FConfig.ImageBaseAddr]);

    if FQuietCompile then
      Arguments := Arguments + ' -Q';

    DCCExitCode := JclSysUtils.Execute(FExeName + ' ' + Arguments, CaptureLine, True, AbortPtr);
    Result := (DCCExitCode = 0) and (FMessages.ErrorCount = 0) and (FMessages.FatalCount = 0);
  end;
end;

//=== { TJclBorRADToolExperts } ==============================================

const
  ExpertsKeyName = 'Experts';
  VersionControlKeyName = 'Version Control';
  VersionControlVCSManager = 'VCSManager';

constructor TJclBorRADToolIdeExperts.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  FExperts := TStringList.Create;
  ReadExperts;
end;

destructor TJclBorRADToolIdeExperts.Destroy;
begin
  FreeAndNil(FExperts);
  inherited Destroy;
end;

function TJclBorRADToolIdeExperts.AddExpert(const AName, AFileName: string): Boolean;
begin
  Result := True;
  Installation.ConfigData.WriteString(ExpertsKeyName, AName, AFileName);
  ReadExperts;
end;

function TJclBorRADToolIdeExperts.GetCount: Integer;
begin
  Result := FExperts.Count;
end;

function TJclBorRADToolIdeExperts.GetFileNames(Index: Integer): string;
begin
  Result := FExperts.Values[FExperts.Names[Index]];
end;

function TJclBorRADToolIdeExperts.GetNames(Index: Integer): string;
begin
  Result := FExperts.Names[Index];
end;

procedure TJclBorRADToolIdeExperts.ReadExperts;
begin
  Installation.ConfigData.ReadSectionValues(ExpertsKeyName, FExperts);
end;

function TJclBorRADToolIdeExperts.RemoveExpert(const AName: string): Boolean;
begin
  Result := Installation.ConfigData.ValueExists(ExpertsKeyName, AName);
  if Result then
  begin
    Installation.ConfigData.DeleteKey(ExpertsKeyName, AName);
    ReadExperts;
  end;
end;

//=== { TJclBorRADToolInstallationEx } =======================================

constructor TJclBorRADToolInstallationEx.Create(const AConfigDataLocation: string);
begin
  inherited Create(AConfigDataLocation);
  FIdeExperts := nil;
end;

destructor TJclBorRADToolInstallationEx.Destroy;
begin
  FreeAndNil(FIdeExperts);
  inherited Destroy;
end;

function TJclBorRADToolInstallationEx.GetIdeExperts: TJclBorRADToolIdeExperts;
begin
  if not Assigned(FIdeExperts) then
    FIdeExperts := TJclBorRADToolIdeExperts.Create(Self);
  Result := FIdeExperts;
end;

function TJclBorRADToolInstallationEx.GetVCSManager: string;
begin
  Result := '';
  if SupportsVCSManager then
    Result := ConfigData.ReadString(VersionControlKeyName, VersionControlVCSManager, '');
end;

procedure TJclBorRADToolInstallationEx.SetVCSManager(const Value: string);
begin
  if SupportsVCSManager then
    ConfigData.WriteString(VersionControlKeyName, VersionControlVCSManager, Value);
end;

function TJclBorRADToolInstallationEx.SupportsVCSManager: Boolean;
begin
  {$IFDEF KYLIX}
  Result := False;
  {$ELSE}
  Result := VersionNumber <= 7;
  {$ENDIF KYLIX}
end;

// History:

// $Log$
// Revision 1.9  2005/12/25 20:32:59  uschuster
// Linker output options and compiler switches; some changes in TJclDCCEx.Compile; minor JCL style changes
//

end.



