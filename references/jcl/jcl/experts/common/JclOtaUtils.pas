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
{ The Original Code is JclOtaUtils.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.                                  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}
unit JclOtaUtils;

interface

{$I jcl.inc}
{$I crossplatform.inc}

uses
  SysUtils, Classes, Windows,
  Controls, Forms,
  JclBase,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  JclDebug,
  {$ENDIF MSWINDOWS}
  JclIDEUtils,
  ToolsAPI;

type
// note to developers
// to avoid JCL exceptions to be reported as Borland's exceptions in automatic
// bug reports, all entry points should be protected with this code model:
// uses
//   JclOtaUtils;
// try
//   <code to execute here>
// except
//   on ExceptionObj: TObject do
//   begin
//     JclExpertShowExceptionDialog(ExceptionObj);
//   end;
// end;
// entry points for experts are usually:
//  - initialization sections
//  - finalization sections
//  - Register procedures
//  - expert entry point
//  - Action update events
//  - Action execute events
//  - notifier callback functions
//  - ... (non exhaustive list)

  EJclExpertException = class (EJclError)
  {$IFDEF MSWINDOWS}
  private
    FStackInfo: TJclStackInfoList;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    property StackInfo: TJclStackInfoList read FStackInfo;
  {$ENDIF MSWINDOWS}
  end;

  TJclOTASettings = class (TObject)
  private
    FKeyName: string;
    FBaseKeyName: string;
  public
    constructor Create(ExpertName: string);
    function LoadBool(Name: string; Def: Boolean): Boolean;
    function LoadString(Name: string; Def: string): string;
    function LoadInteger(Name: string; Def: Integer): Integer;
    procedure LoadStrings(Name: string; List: TStrings);
    procedure SaveBool(Name: string; Value: Boolean);
    procedure SaveString(Name: string; Value: string);
    procedure SaveInteger(Name: string; Value: Integer);
    procedure SaveStrings(Name: string; List: TStrings);
    property KeyName: string read FKeyName;
    property BaseKeyName: string read FBaseKeyName;
  end;

  {$IFDEF BDS8_UP}
  TJclOTAExpertBase = class;

  TJclOTAExpertOptions = class(TInterfacedObject, INTAAddinOptions)
  private
    FExpert: TJclOTAExpertBase;
  public
    constructor Create(AExpert: TJclOTAExpertBase);
    { INTAAddinOptions }
    function GetArea: string;
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    procedure DialogClosed(Accepted: Boolean);
    function ValidateContents: Boolean;
    function GetHelpContext: Integer;
    function IncludeInIDEInsight: Boolean;
  end;
  {$ENDIF BDS8_UP}

  // Note: we MUST use an interface as the type of the Expert parameter
  // and not an object to avoid a bug in C++ Builder 6 (or lower) compiler. If we 
  // used an object, the compiler would crash or give internal error GH4148
  // being obviously lost trying to resolve almost circular references 
  // between this unit and the JclOtaConfigurationForm unit.
  IJclOTAOptionsCallback = interface;

  TJclOTAAddPageFunc = procedure (Expert: IJclOTAOptionsCallback) of object;

  IJclOTAOptionsCallback = interface
    procedure AddConfigurationPages(AddPageFunc: TJclOTAAddPageFunc);
    procedure DialogClosed(SaveChanges: Boolean);
    function GetPageName: string;
    function GetFrameClass: TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    function ValidateContents: Boolean;
  end;

  TJclOTAExpertBase = class(TInterfacedObject, IJclOTAOptionsCallback)
  private
    FRootDir: string;
    FJCLRootDir: string;
    FSettings: TJclOTASettings;
    FJCLSettings: TStrings;
    {$IFDEF BDS8_UP}
    FOptions: INTAAddinOptions;
    {$ENDIF BDS8_UP}
    function GetModuleHInstance: Cardinal;
    function GetRootDir: string;
    function GetJCLRootDir: string;
    function GetJCLSettings: TStrings;
    function GetRADInstallation: TJclBorRADToolInstallation;
    procedure ReadEnvVariables(EnvVariables: TStrings);
    function GetActivePersonality: TJclBorPersonality;
    function GetDesigner: string;
  public
    class function GetNTAServices: INTAServices;
    class function GetOTAServices: IOTAServices;
    class function GetOTADebuggerServices: IOTADebuggerServices;
    class function GetOTAEditorServices: IOTAEditorServices;
    class function GetOTAModuleServices: IOTAModuleServices;
    class function GetOTAPackageServices: IOTAPackageServices;
    {$IFDEF BDS}
    class function GetOTAPersonalityServices: IOTAPersonalityServices;
    class function GetOTAGalleryCategoryManager: IOTAGalleryCategoryManager;
    {$ENDIF BDS}
    {$IFDEF BDS4_UP}
    class function GetOTAProjectManager: IOTAProjectManager;
    {$ENDIF BDS4_UP}
    class function GetOTAMessageServices: IOTAMessageServices;
    class function GetOTAWizardServices: IOTAWizardServices;
    {$IFDEF BDS8_UP}
    class function GetNTAEnvironmentOptionsServices: INTAEnvironmentOptionsServices;
    {$ENDIF BDS8_UP}
    class function GetActiveProject: IOTAProject;
    class function GetProjectGroup: IOTAProjectGroup;
    class function GetActiveEditBuffer: IOTAEditBuffer;
    class function IsPersonalityLoaded(const PersonalityName: string): Boolean;
    class procedure AddExpert(AExpert: TJclOTAExpertBase);
    class procedure RemoveExpert(AExpert: TJclOTAExpertBase);
    class function GetExpertCount: Integer;
    class function GetExpert(Index: Integer): TJclOTAExpertBase;
    class function ConfigurationDialog(StartName: string = ''): Boolean;
  public
    function GetPageName: string; virtual;
    { INTAAddinOptions }
    function GetArea: string; virtual;
    function GetCaption: string; virtual;
    function GetFrameClass: TCustomFrameClass; virtual;
    procedure FrameCreated(AFrame: TCustomFrame); virtual;
    procedure DialogClosed(Accepted: Boolean); virtual;
    function ValidateContents: Boolean; virtual;
    function GetHelpContext: Integer; virtual;
    function IncludeInIDEInsight: Boolean; virtual;
  public
    constructor Create(AName: string); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function FindExecutableName(const MapFileName: TFileName; const OutputDirectory: string;
      var ExecutableFileName: TFileName): Boolean;
    function GetDrcFileName(const Project: IOTAProject): TFileName;
    function GetMapFileName(const Project: IOTAProject): TFileName;
    function GetOutputDirectory(const Project: IOTAProject): string;
    function IsInstalledPackage(const Project: IOTAProject): Boolean;
    function IsPackage(const Project: IOTAProject): Boolean;

    { IJclOTAOptionsCallback }
    procedure AddConfigurationPages(AddPageFunc: TJclOTAAddPageFunc); virtual;

    procedure RegisterCommands; virtual;
    procedure UnregisterCommands; virtual;

    property Settings: TJclOTASettings read FSettings;
    property JCLRootDir: string read GetJCLRootDir;
    property JCLSettings: TStrings read GetJCLSettings;
    property RootDir: string read GetRootDir;
    property ActivePersonality: TJclBorPersonality read GetActivePersonality;
    property Designer: string read GetDesigner;

    property ModuleHInstance: Cardinal read GetModuleHInstance;
  end;

  TJclOTAExpert = class(TJclOTAExpertBase, IOTAWizard, IOTANotifier)
  public
    { IOTANotifier }
    procedure AfterSave; virtual;
    procedure BeforeSave; virtual;
    procedure Destroyed; virtual;
    procedure Modified; virtual;
    { IOTAWizard }
    procedure Execute; virtual;
    function GetIDString: string; virtual;
    function GetName: string; virtual;
    function GetState: TWizardState; virtual;
  end;

  {$IFDEF BDS7_UP}
  TJclOTALocalMenu = class(TInterfacedObject, IOTANotifier, IOTALocalMenu)
  private
    FCaption: string;
    FChecked: Boolean;
    FEnabled: Boolean;
    FHelpContext: Integer;
    FName: string;
    FParent: string;
    FPosition: Integer;
    FVerb: string;
  public
    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  public
    { IOTALocalMenu }
    function GetCaption: string;
    function GetChecked: Boolean;
    function GetEnabled: Boolean;
    function GetHelpContext: Integer;
    function GetName: string;
    function GetParent: string;
    function GetPosition: Integer;
    function GetVerb: string;
    procedure SetCaption(const Value: string);
    procedure SetChecked(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetHelpContext(Value: Integer);
    procedure SetName(const Value: string);
    procedure SetParent(const Value: string);
    procedure SetPosition(Value: Integer);
    procedure SetVerb(const Value: string);
    property Caption: string read GetCaption write SetCaption;
    property Checked: Boolean read GetChecked write SetChecked;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property HelpContext: Integer read GetHelpContext write SetHelpContext;
    property Name: string read GetName write SetName;
    property Parent: string read GetParent write SetParent;
    property Position: Integer read GetPosition write SetPosition;
    property Verb: string read GetVerb write SetVerb;
  end;

  TJclProjectManagerMenuExecuteEvent = procedure (const MenuContextList: IInterfaceList) of object;

  TJclOTAProjectManagerMenu = class(TJclOTALocalMenu, IOTANotifier, IOTALocalMenu, IOTAProjectManagerMenu)
  private
    FIsMultiSelectable: Boolean;
    FOnExecute: TJclProjectManagerMenuExecuteEvent;
  public
    { IOTAProjectManagerMenu }
    function GetIsMultiSelectable: Boolean;
    procedure SetIsMultiSelectable(Value: Boolean);
    procedure Execute(const MenuContextList: IInterfaceList); overload;
    function PreExecute(const MenuContextList: IInterfaceList): Boolean;
    function PostExecute(const MenuContextList: IInterfaceList): Boolean;
    property IsMultiSelectable: Boolean read GetIsMultiSelectable write SetIsMultiSelectable;
    property OnExecute: TJclProjectManagerMenuExecuteEvent read FOnExecute write FOnExecute;
  end;
  {$ENDIF BDS7_UP}

// procedure SaveOptions(const Options: IOTAOptions; const FileName: string);
function JclExpertShowExceptionDialog(AExceptionObj: TObject): Boolean;
{$IFDEF BDS}
function PersonalityTextToId(const PersonalityText: string): TJclBorPersonality;
{$ENDIF BDS}

{$IFDEF BDS}
procedure RegisterSplashScreen;
procedure RegisterAboutBox;
{$ENDIF BDS}

// properties are stored as "// PropID PropValue" in project file
// they have to be placed before any identifiers and after comments at the beginning of the file
function GetProjectProperties(const AProject: IOTAProject; const PropIDs: TDynAnsiStringArray): TDynAnsiStringArray;
function SetProjectProperties(const AProject: IOTAProject; const PropIDs, PropValues: TDynAnsiStringArray): Integer;

// set to true to temporary disable experts that alter compiled files after they were compiled
var
  JclDisablePostCompilationProcess: Boolean = False;

// design package entry point
procedure Register;

// expert DLL entry point
function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var TerminateProc: TWizardTerminateProc): Boolean; stdcall;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Variants,
  Graphics, Dialogs, ActiveX, FileCtrl, IniFiles,
  JediRegInfo,
  {$IFDEF MSWINDOWS}
  ImageHlp, JclRegistry,
  {$ENDIF MSWINDOWS}
  {$IFDEF BDS8_UP}
  JclOtaAddinOptions,
  {$ENDIF BDS8_UP}
  JclFileUtils, JclStrings, JclSysInfo, JclSimpleXml, JclCompilerUtils,
  JclOtaConsts, JclOtaResources, JclOtaExceptionForm, JclOtaConfigurationForm,
  JclOtaWizardForm, JclOtaWizardFrame,
  JclOTAUnitVersioning, JclOTAActions;

{$R 'JclImages.res'}

var
  JCLUnitVersioningWizardIndex: Integer = -1;
  JCLActionsWizardIndex: Integer = -1;

procedure Register;
begin
  try
    {$IFDEF BDS}
    RegisterSplashScreen;
    RegisterAboutBox;
    {$ENDIF BDS}
    RegisterPackageWizard(TJclOTAUnitVersioningExpert.Create);
    RegisterPackageWizard(TJclOTAActionExpert.Create);
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure JclWizardTerminate;
begin
  try
    if JCLUnitVersioningWizardIndex <> -1 then
      TJclOTAExpertBase.GetOTAWizardServices.RemoveWizard(JCLUnitVersioningWizardIndex);
    if JCLActionsWizardIndex <> -1 then
      TJclOTAExpertBase.GetOTAWizardServices.RemoveWizard(JCLActionsWizardIndex);
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc; var TerminateProc: TWizardTerminateProc): Boolean stdcall;
begin
  try
    TerminateProc := JclWizardTerminate;

    {$IFDEF BDS}
    RegisterSplashScreen;
    RegisterAboutBox;
    {$ENDIF BDS}
    JCLUnitVersioningWizardIndex := TJclOTAExpertBase.GetOTAWizardServices.AddWizard(TJclOTAUnitVersioningExpert.Create);
    JCLActionsWizardIndex := TJclOTAExpertBase.GetOTAWizardServices.AddWizard(TJclOTAActionExpert.Create);

    Result := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;

var
  GlobalExpertList: TList = nil;

function JclExpertShowExceptionDialog(AExceptionObj: TObject): Boolean;
var
  AJclExpertExceptionForm: TJclExpertExceptionForm;
begin
  AJclExpertExceptionForm := TJclExpertExceptionForm.Create(Application);
  try
    AJclExpertExceptionForm.ShowException(AExceptionObj);
    Result := AJclExpertExceptionForm.Execute;
  finally
    AJclExpertExceptionForm.Free;
  end;
end;

{$IFDEF BDS}
function PersonalityTextToId(const PersonalityText: string): TJclBorPersonality;
begin
  if SameText(PersonalityText, sDelphiPersonality) then
    Result := bpDelphi32
  else if SameText(PersonalityText, sDelphiDotNetPersonality) then
    Result := bpDelphiNet32
  else if SameText(PersonalityText, sCBuilderPersonality) then
    Result := bpBCBuilder32
  else if SameText(PersonalityText, sCSharpPersonality) then
    Result := bpCSBuilder32
  else if SameText(PersonalityText, sVBPersonality) then
    Result := bpVisualBasic32
  {$IFDEF COMPILER10_UP}
  else if SameText(PersonalityText, sDesignPersonality) then
    Result := bpDesign
  {$ENDIF COMPILER10_UP}
  else
    Result := bpUnknown;
end;
{$ENDIF BDS}

// result[] > 0: the property was found, result is the position of the first char of the property value
// result[] <= 0: the property was not found, -result is the position where the property could be inserted
function InternalLocateProperties(const AReader: IOTAEditReader; const PropIDs: TDynAnsiStringArray): TDynIntegerArray;
const
  BufferSize = 4096;
var
  Buffer, Line: AnsiString;
  BufferStart, BufferCount, BufferPosition, LineStart, Position, PropIndex, PropCount, PropMatches: Integer;
  InsideLineComment, InsideComment, InsideBrace: Boolean;
  procedure LoadNextBuffer;
  begin
    Line := Line + Copy(Buffer, LineStart - BufferStart + 1, Position - LineStart);
    BufferStart := Position;
    LineStart := BufferStart;
    BufferCount := AReader.GetText(BufferStart, PAnsiChar(Buffer), BufferSize);
    BufferPosition := Position - BufferStart;
  end;
begin
  Line := '';
  BufferStart := 0;
  BufferCount := 0;
  LineStart := 0;
  Position := 0;
  PropMatches := 0;
  InsideLineComment := False;
  InsideComment := False;
  InsideBrace := False;
  PropCount := Length(PropIDs);
  SetLength(Result, PropCount);
  for PropIndex := 0 to PropCount - 1 do
    Result[PropIndex] := -1;

  SetLength(Buffer, BufferSize);
  repeat
    BufferPosition := Position - BufferStart;

    if BufferPosition >= BufferCount then
      LoadNextBuffer;

    case Buffer[BufferPosition + 1] of
      NativeLineFeed,
      NativeCarriageReturn:
        begin
          if InsideLineComment and not (InsideComment or InsideBrace) then
          begin
            // process line
            InsideLineComment := False;
            if (LineStart - BufferStart) < 0 then
              raise EJclExpertException.CreateRes(@RsELineTooLong);
            Line := Line + Copy(Buffer, LineStart - BufferStart + 1, Position - LineStart);
            for PropIndex := 0 to PropCount - 1 do
              if Pos(PropIDs[PropIndex], Line) = 4 then
            begin
              Result[PropIndex] := LineStart + Length(PropIDs[PropIndex]) + 4;
              Inc(PropMatches);
            end;
            Line := '';
          end;
          LineStart := Position + 1;
        end;
      '/':
        begin
          if BufferPosition >= BufferCount then
            LoadNextBuffer;
          if (BufferPosition + 1) < BufferCount then
          begin
            if not (InsideLineComment or InsideComment or InsideBrace) then
            begin
              if (Buffer[BufferPosition + 2] = '/') then
              begin
                Inc(Position);
                InsideLineComment := True;
              end
              else
                // end of comments
                Break;
            end;
          end
          else
            // end of file
            Break;
        end;
      '(':
        begin
          if BufferPosition >= BufferCount then
            LoadNextBuffer;
          if (BufferPosition + 1) < BufferCount then
          begin
            if not (InsideLineComment or InsideComment or InsideBrace) then
            begin
              if (Buffer[BufferPosition + 2] = '*') then
              begin
                Inc(Position);
                InsideComment := True;
              end
              else
                // end of comments
                Break;
            end;
          end
          else
            // end of file
            Break;
        end;
      '*':
        begin
          if BufferPosition >= BufferCount then
            LoadNextBuffer;
          if (BufferPosition + 1) < BufferCount then
          begin
            if InsideComment then
            begin
              if (Buffer[BufferPosition + 2] = ')') then
              begin
                Inc(Position);
                InsideComment := False;
              end;
            end
            else
            if not (InsideLineComment or InsideBrace) then
              // end of comments
              Break;
          end
          else
            // end of file
            Break;
        end;
      '{':
        if not (InsideLineComment or InsideComment or InsideBrace) then
          InsideBrace := True;
      '}':
        if InsideBrace then
          InsideBrace := False
        else
        if not (InsideLineComment or InsideComment) then
          // end of comments
          Break;
    else
      if not CharIsWhiteSpace(Char(Buffer[BufferPosition + 1])) and not InsideLineComment
        and not InsideComment and not InsideBrace then
        // end of comments
        Break;
    end;
    Inc(Position);
  until (BufferCount = 0) or (PropMatches = PropCount);
//  if InsideLineComment or InsideComment or InsideBrace then
//    raise EJclExpertException.CreateRes(@RsEUnterminatedComment);  Don't throw an exception if the source isn't correct (Mantis #6425)
  for PropIndex := 0 to PropCount - 1 do
    if Result[PropIndex] = -1 then
      Result[PropIndex] := -Position;
end;

function GetProjectProperties(const AProject: IOTAProject; const PropIDs: TDynAnsiStringArray): TDynAnsiStringArray;
const
  BufferSize = 4096;
var
  FileIndex, PropCount, PropIndex, BufferIndex: Integer;
  AEditor: IOTAEditor;
  FileExtension: string;
  PropLocations: TDynIntegerArray;
  AReader: IOTAEditReader;
begin
  PropCount := Length(PropIDs);
  SetLength(Result, PropCount);
  SetLength(PropLocations, 0);
  for FileIndex := 0 to AProject.GetModuleFileCount - 1 do
  begin
    AEditor := AProject.GetModuleFileEditor(FileIndex);
    // some modules do not have text editors
    if Assigned(AEditor) then
    begin
      FileExtension := ExtractFileExt(AEditor.FileName);
      if AnsiSameText(FileExtension, '.dpr') or AnsiSameText(FileExtension, '.dpk')
        or AnsiSameText(FileExtension, '.bpf') or AnsiSameText(FileExtension, '.cpp') then
      begin
        AReader := (AEditor as IOTASourceEditor).CreateReader;
        try
          PropLocations := InternalLocateProperties(AReader, PropIDs);
          for PropIndex := 0 to PropCount - 1 do
            if PropLocations[PropIndex] > 0 then
            begin
              SetLength(Result[PropIndex], BufferSize);
              SetLength(Result[PropIndex], AReader.GetText(PropLocations[PropIndex], PAnsiChar(Result[PropIndex]), BufferSize));
              for BufferIndex := 1 to Length(Result[PropIndex]) do
                if CharIsWhiteSpace(Char(Result[PropIndex][BufferIndex])) then
                begin
                  SetLength(Result[PropIndex], BufferIndex - 1);
                  Break;
                end;
            end;
        finally
          AReader := nil;
        end;
        Break;
      end;
    end;
  end;
end;

function SetProjectProperties(const AProject: IOTAProject; const PropIDs, PropValues: TDynAnsiStringArray): Integer;
const
  BufferSize = 4096;
var
  FileIndex, PropCount, PropIndex, BufferIndex, PropSize: Integer;
  AEditor: IOTAEditor;
  ASourceEditor: IOTASourceEditor;
  FileExtension: string;
  Buffer: AnsiString;
  PropLocations: TDynIntegerArray;
  AReader: IOTAEditReader;
  AWriter: IOTAEditWriter;
  S: AnsiString;
  ABuffer: IOTAEditBuffer;
begin
  PropCount := Length(PropIDs);
  Result := 0;
  for FileIndex := 0 to AProject.GetModuleFileCount - 1 do
  begin
    AEditor := AProject.GetModuleFileEditor(FileIndex);
    FileExtension := ExtractFileExt(AEditor.FileName);
    if AnsiSameText(FileExtension, '.dpr') or AnsiSameText(FileExtension, '.dpk')
      or AnsiSameText(FileExtension, '.bpf') or AnsiSameText(FileExtension, '.cpp') then
    begin
      ASourceEditor := AEditor as IOTASourceEditor;
      ABuffer := ASourceEditor as IOTAEditBuffer;
      if not ABuffer.IsReadOnly then
      begin
        for PropIndex := 0 to PropCount - 1 do
        begin
          SetLength(PropLocations, 0);
          PropSize := 0;
          AReader := ASourceEditor.CreateReader;
          try
            PropLocations := InternalLocateProperties(AReader, Copy(PropIDs, PropIndex, 1));
            if PropLocations[0] > 0 then
            begin
              SetLength(Buffer, BufferSize);
              SetLength(Buffer, AReader.GetText(PropLocations[0], PAnsiChar(Buffer), BufferSize));
              for BufferIndex := 1 to Length(Buffer) do
                if CharIsWhiteSpace(Char(Buffer[BufferIndex])) then
                begin
                  PropSize := BufferIndex - 1;
                  Break;
                end;
            end;
          finally
            // release the reader before allocating the writer
            AReader := nil;
          end;

          AWriter := ASourceEditor.CreateUndoableWriter;
          try
            if PropLocations[0] > 0 then
            begin
              AWriter.CopyTo(PropLocations[0]);
              AWriter.DeleteTo(PropLocations[0] + PropSize);
              AWriter.Insert(PAnsiChar(PropValues[PropIndex]));
            end
            else
            begin
              AWriter.CopyTo(-PropLocations[0]);
              S := AnsiString(Format('// %s %s%s', [PropIDs[PropIndex], PropValues[PropIndex], NativeLineBreak]));
              AWriter.Insert(PAnsiChar(S));
            end;
          finally
            // release the writter before allocating the reader
            AWriter := nil;
          end;
          Inc(Result);
        end;
      end;
      Break;
    end;
  end;
end;

//=== { EJclExpertException } ================================================

{$IFDEF MSWINDOWS}
procedure EJclExpertException.AfterConstruction;
begin
  inherited AfterConstruction;
  FStackInfo := JclCreateStackList(True, 0, nil, False);
end;

destructor EJclExpertException.Destroy;
begin
  FreeAndNil(FStackInfo);
  inherited Destroy;
end;
{$ENDIF MSWINDOWS}

//=== { TJclOTASettings } ====================================================

constructor TJclOTASettings.Create(ExpertName: string);
var
  OTAServices: IOTAServices;
begin
  inherited Create;

  Supports(BorlandIDEServices,IOTAServices,OTAServices);
  if not Assigned(OTAServices) then
    raise EJclExpertException.CreateRes(@RsENoOTAServices);

  FBaseKeyName := StrEnsureSuffix(NativeBackSlash, OTAServices.GetBaseRegistryKey);
  
  FKeyName := BaseKeyName + RegJclIDEKey + ExpertName;
end;

function TJclOTASettings.LoadBool(Name: string; Def: Boolean): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := RegReadBoolDef(HKCU, KeyName, Name, Def);
  {$ELSE MSWINDOWS}
  Result := Def;
  {$ENDIF MSWINDOWS}
end;

function TJclOTASettings.LoadInteger(Name: string; Def: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}

  Result := RegReadIntegerDef(HKCU, KeyName, Name, Def);
  {$ELSE MSWINDOWS}
  Result := Def;
  {$ENDIF MSWINDOWS}
end;

function TJclOTASettings.LoadString(Name, Def: string): string;
begin
  {$IFDEF MSWINDOWS}
  Result := RegReadStringDef(HKCU, KeyName, Name, Def);
  {$ELSE MSWINDOWS}
  Result := Def;
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTASettings.LoadStrings(Name: string; List: TStrings);
begin
  {$IFDEF MSWINDOWS}
  RegLoadList(HKCU, KeyName, Name, List);
  {$ELSE MSWINDOWS}
  List.Clear;
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTASettings.SaveBool(Name: string; Value: Boolean);
begin
  {$IFDEF MSWINDOWS}
  RegWriteBool(HKCU, KeyName, Name, Value);
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTASettings.SaveInteger(Name: string; Value: Integer);
begin
  {$IFDEF MSWINDOWS}
  RegWriteInteger(HKCU, KeyName, Name, Value);
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTASettings.SaveString(Name, Value: string);
begin
  {$IFDEF MSWINDOWS}
  RegWriteString(HKCU, KeyName, Name, Value);
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTASettings.SaveStrings(Name: string; List: TStrings);
begin
  {$IFDEF MSWINDOWS}
  RegSaveList(HKCU, KeyName, Name, List);
  {$ENDIF MSWINDOWS}
end;

//=== { TJclOTAExpertOptions } ===============================================

{$IFDEF BDS8_UP}
constructor TJclOTAExpertOptions.Create(AExpert: TJclOTAExpertBase);
begin
  inherited Create;
  FExpert := AExpert;
end;

function TJclOTAExpertOptions.GetArea: string;
begin
  Result := FExpert.GetArea;
end;

function TJclOTAExpertOptions.GetCaption: string;
begin
  Result := FExpert.GetCaption;
end;

function TJclOTAExpertOptions.GetFrameClass: TCustomFrameClass;
begin
  Result := FExpert.GetFrameClass;
end;

procedure TJclOTAExpertOptions.FrameCreated(AFrame: TCustomFrame);
begin
  FExpert.FrameCreated(AFrame);
end;

procedure TJclOTAExpertOptions.DialogClosed(Accepted: Boolean);
begin
  FExpert.DialogClosed(Accepted);
end;

function TJclOTAExpertOptions.ValidateContents: Boolean;
begin
  Result := FExpert.ValidateContents;
end;

function TJclOTAExpertOptions.GetHelpContext: Integer;
begin
  Result := FExpert.GetHelpContext;
end;

function TJclOTAExpertOptions.IncludeInIDEInsight: Boolean;
begin
  Result := FExpert.IncludeInIDEInsight;
end;
{$ENDIF BDS8_UP}

//=== { TJclOTAExpertBase } ==================================================

class function TJclOTAExpertBase.ConfigurationDialog(
  StartName: string): Boolean;
var
  OptionsForm: TJclOtaOptionsForm;
  Index: Integer;
  Expert: TJclOTAExpertBase;
  FrameClass: TCustomFrameClass;
begin
  {$IFDEF BDS8_UP}
  //no resourcestring here, because this message will be removed
  if MessageBox(0, 'The JCL options can now be found in the Third party section of the environment options and ' +
    'this menu item will be removed some time in the future.' + #13#10#13#10 +
    'Press ENTER/Yes to open the enviroment options or No to open the old options dialog.',
    'JCL', MB_ICONASTERISK or MB_YESNO or MB_DEFBUTTON1) = IDYES then
  begin
    (BorlandIDEServices as IOTAServices).GetEnvironmentOptions.EditOptions('',
      RsProjectJEDIJclAddinOptionsCaption);
    Result := True;
  end
  else
  begin
  {$ENDIF BDS8_UP}
  OptionsForm := TJclOtaOptionsForm.Create(nil);
  try
    for Index := 0 to GetExpertCount - 1 do
    begin
      Expert := GetExpert(Index);
      FrameClass := Expert.GetFrameClass;
      if Assigned(FrameClass) then
        Expert.AddConfigurationPages(OptionsForm.AddPage);
    end;
    Result := OptionsForm.Execute(StartName);
  finally
    OptionsForm.Free;
  end;
  {$IFDEF BDS8_UP}
  end;
  {$ENDIF BDS8_UP}
end;

class function TJclOTAExpertBase.GetExpert(Index: Integer): TJclOTAExpertBase;
begin
  if Assigned(GlobalExpertList) then
    Result := TJclOTAExpertBase(GlobalExpertList.Items[Index])
  else
    Result := nil;
end;

class function TJclOTAExpertBase.GetExpertCount: Integer;
begin
  if Assigned(GlobalExpertList) then
    Result := GlobalExpertList.Count
  else
    Result := 0;
end;

function TJclOTAExpertBase.GetFrameClass: TCustomFrameClass;
begin
  // override to customize
  Result := nil;
end;

function TJclOTAExpertBase.GetHelpContext: Integer;
begin
  // override to customize
  Result := 0;
end;

function TJclOTAExpertBase.GetJCLRootDir: string;
var
  IDERegKey, JclVersion, JclDcpDir, JclBplDir, PlatformStr: string;
begin
  if FJCLRootDir = '' then
  begin
    IDERegKey := StrEnsureNoSuffix('\', GetOTAServices.GetBaseRegistryKey);
    {$IFDEF BDS9_UP}
    PlatformStr := 'Win32';
    {$ENDIF BDS9_UP}
    if not ReadJediRegInformation(IDERegKey, 'JCL', PlatformStr, JclVersion, JclDcpDir, JclBplDir, FJCLRootDir)
       or (FJCLRootDir = '') then
    begin
      if SelectDirectory(LoadResString(@RsBrowseToJCLRootDir), '', FJCLRootDir)
         and DirectoryExists(FJCLRootDir) then
      begin
        FJCLRootDir := PathRemoveSeparator(FJCLRootDir);
        JclVersion := Format('%d.%d.%d.%d', [JclVersionMajor, JclVersionMinor, JclVersionRelease, JclVersionBuild]);
        JclDcpDir := JCLSettings.Values['DCP-Path'];
        JclBplDir := JCLSettings.Values['BPL-Path'];
        InstallJediRegInformation(IDERegKey, 'JCL', PlatformStr, JclVersion, JclDcpDir, JclBplDir, FJCLRootDir);
      end
      else
        raise EJclExpertException.CreateRes(@RsENoRootDir);
    end;
  end;
  Result := FJCLRootDir;
end;

function TJclOTAExpertBase.GetJCLSettings: TStrings;
var
  ConfigIni: TIniFile;
  Installation: TJclBorRADToolInstallation;
const
  JclConfigIni = 'bin\JCL-install.ini';
begin
  if not Assigned(FJCLSettings) then
  begin
    Installation := GetRADInstallation;
    try
      ConfigIni := TIniFile.Create(PathAddSeparator(FJCLRootDir) + JclConfigIni);
      try
        FJCLSettings := TStringList.Create;
        ConfigIni.ReadSectionValues(Installation.Name, FJCLSettings);
      finally
        ConfigIni.Free;
      end;
    finally
      Installation.Free;
    end;
  end;
  Result := FJCLSettings;
end;

class procedure TJclOTAExpertBase.AddExpert(AExpert: TJclOTAExpertBase);
begin
  if not Assigned(GlobalExpertList) then
    GlobalExpertList := TList.Create;
  GlobalExpertList.Add(AExpert);
end;

procedure TJclOTAExpertBase.AfterConstruction;
begin
  inherited AfterConstruction;

  RegisterCommands;
  AddExpert(Self);
  {$IFDEF BDS8_UP}
  if GetFrameClass <> nil then
  begin
    FOptions := TJclOTAExpertOptions.Create(Self);
    GetNTAEnvironmentOptionsServices.RegisterAddInOptions(FOptions);
  end;
  {$ENDIF BDS8_UP}
end;

procedure TJclOTAExpertBase.BeforeDestruction;
begin
  {$IFDEF BDS8_UP}
  if GetFrameClass <> nil then
  begin
    GetNTAEnvironmentOptionsServices.UnregisterAddInOptions(FOptions);
    FOptions := nil;
  end;
  {$ENDIF BDS8_UP}
  RemoveExpert(Self);
  UnregisterCommands;

  inherited BeforeDestruction;
end;

class procedure TJclOTAExpertBase.RemoveExpert(AExpert: TJclOTAExpertBase);
begin
  if Assigned(GlobalExpertList) then
    GlobalExpertList.Remove(AExpert);
end;

procedure TJclOTAExpertBase.AddConfigurationPages(
  AddPageFunc: TJclOTAAddPageFunc);
begin
  // AddPageFunc uses '\' as a separator in PageName to build a tree
  AddPageFunc(Self);
end;

constructor TJclOTAExpertBase.Create(AName: string);
begin
  inherited Create;
  FSettings := TJclOTASettings.Create(AName);
end;

destructor TJclOTAExpertBase.Destroy;
begin
  FreeAndNil(FSettings);
  FreeAndNil(FJCLSettings);
  inherited Destroy;
end;

procedure TJclOTAExpertBase.DialogClosed(Accepted: Boolean);
begin
  // override to customize
end;

function TJclOTAExpertBase.FindExecutableName(const MapFileName: TFileName;
  const OutputDirectory: string; var ExecutableFileName: TFileName): Boolean;
var
  Se: TSearchRec;
  Res: Integer;
  {$IFDEF RTL220_UP}
  LatestTime: TDateTime;
  {$ELSE ~RTL220_UP}
  LatestTime: Integer;
  {$ENDIF ~RTL220_UP}
  FileName: TFileName;
  {$IFDEF MSWINDOWS}
  LI: LoadedImage;
  {$ENDIF MSWINDOWS}
begin
  LatestTime := 0;
  ExecutableFileName := '';
  // the latest executable file is very likely our file
  Res := SysUtils.FindFirst(ChangeFileExt(MapFileName, '.*'), faArchive, Se);
  while Res = 0 do
  begin
    FileName := PathAddSeparator(OutputDirectory) + Se.Name;
    {$IFDEF MSWINDOWS}
    // possible loss of data
    if MapAndLoad(PAnsiChar(AnsiString(FileName)), nil, @LI, False, True) then
    begin
      {$IFDEF RTL220_UP}
      if (not LI.fDOSImage) and (Se.TimeStamp > LatestTime) then
      begin
        ExecutableFileName := FileName;
        LatestTime := Se.TimeStamp;
      end;
      {$ELSE ~RTL220_UP}
      if (not LI.fDOSImage) and (Se.Time > LatestTime) then
      begin
        ExecutableFileName := FileName;
        LatestTime := Se.Time;
      end;
      {$ENDIF ~RTL220_UP}
      UnMapAndLoad(@LI);
    end;
    {$ELSE}
    if Se.Time > LatestTime then
    begin
      ExecutableFileName := FileName;
      LatestTime := Se.Time;
    end;
    {$ENDIF MSWINDOWS}
    Res := SysUtils.FindNext(Se);
  end;
  SysUtils.FindClose(Se);
  Result := (ExecutableFileName <> '');
end;

procedure TJclOTAExpertBase.FrameCreated(AFrame: TCustomFrame);
begin
  // override to customize
end;

class function TJclOTAExpertBase.GetActiveEditBuffer: IOTAEditBuffer;
var
  OTAEditorServices: IOTAEditorServices;
begin
  OTAEditorServices := GetOTAEditorServices;
  Result := OTAEditorServices.TopBuffer;
end;

class function TJclOTAExpertBase.GetActiveProject: IOTAProject;
var
  ProjectGroup: IOTAProjectGroup;
  OTAModuleServices: IOTAModuleServices;
  Index: Integer;
begin
  Result := nil;
  ProjectGroup := GetProjectGroup;
  OTAModuleServices := GetOTAModuleServices;

  if Assigned(ProjectGroup) then
    Result := ProjectGroup.ActiveProject
  else
    for Index := 0 to OTAModuleServices.ModuleCount - 1 do
      if Supports(OTAModuleServices.Modules[Index], IOTAProject, Result) then
        Exit;
end;

function TJclOTAExpertBase.GetArea: string;
begin
  // override to customize
  Result := '';
end;

function TJclOTAExpertBase.GetCaption: string;
begin
  // override to customize
  Result := LoadResString(@RsProjectJEDIAddinOptionsCaptionPrefix) + StrReplaceChar(GetPageName, '\', '.');
end;

function TJclOTAExpertBase.GetDesigner: string;
begin
  Result := GetOTAServices.GetActiveDesignerType;
end;

function TJclOTAExpertBase.GetDrcFileName(const Project: IOTAProject): TFileName;
begin
  if not Assigned(Project) then
    raise EJclExpertException.CreateRes(@RsENoActiveProject);

  Result := ChangeFileExt(Project.FileName, CompilerExtensionDRC);
end;

function TJclOTAExpertBase.GetMapFileName(const Project: IOTAProject): TFileName;
var
  ProjectFileName: TFileName;
  OutputDirectory, LibPrefix, LibSuffix: string;
begin
  if not Assigned(Project) then
    raise EJclExpertException.CreateRes(@RsENoActiveProject);

  ProjectFileName := Project.FileName;
  OutputDirectory := GetOutputDirectory(Project);
  if not Assigned(Project.ProjectOptions) then
    raise EJclExpertException.CreateRes(@RsENoProjectOptions);
  LibPrefix := Trim(VarToStr(Project.ProjectOptions.Values[LIBPREFIXOptionName]));
  LibSuffix := Trim(VarToStr(Project.ProjectOptions.Values[LIBSUFFIXOptionName]));
  {$IFDEF BDS}
  if Project.Personality = JclCBuilderPersonality then
  begin
    // C++Builder 2007 does not support lib prefix and lib suffix
    LibPrefix := '';
    LibSuffix := '';
  end;
  {$ENDIF BDS}
  Result := PathAddSeparator(OutputDirectory) + LibPrefix +
    PathExtractFileNameNoExt(ProjectFileName) + LibSuffix + CompilerExtensionMAP;
end;

function TJclOTAExpertBase.GetModuleHInstance: Cardinal;
begin
  Result := FindClassHInstance(ClassType);
  if Result = 0 then
    raise EJclExpertException.CreateRes(@RsBadModuleHInstance);
end;

{$IFDEF BDS8_UP}
class function TJclOTAExpertBase.GetNTAEnvironmentOptionsServices: INTAEnvironmentOptionsServices;
begin
  Supports(BorlandIDEServices, INTAEnvironmentOptionsServices, Result);
  if not Assigned(Result) then
    raise EJclExpertException.CreateRes(@RsENoNTAEnvironmentOptionsServices);
end;
{$ENDIF BDS8_UP}

class function TJclOTAExpertBase.GetNTAServices: INTAServices;
begin
  Supports(BorlandIDEServices, INTAServices, Result);
  if not Assigned(Result) then
    raise EJclExpertException.CreateRes(@RsENoNTAServices);
end;

{$IFDEF BDS}
class function TJclOTAExpertBase.GetOTAGalleryCategoryManager: IOTAGalleryCategoryManager;
begin
  Supports(BorlandIDEServices, IOTAGalleryCategoryManager, Result);
  if not Assigned(Result) then
    raise EJclExpertException.CreateRes(@RsENoOTAGalleryCategoryManager);
end;
{$ENDIF BDS}

class function TJclOTAExpertBase.GetOTADebuggerServices: IOTADebuggerServices;
begin
  Supports(BorlandIDEServices, IOTADebuggerServices, Result);
  if not Assigned(Result) then
    raise EJclExpertException.CreateRes(@RsENoDebuggerServices);
end;

class function TJclOTAExpertBase.GetOTAEditorServices: IOTAEditorServices;
begin
  Supports(BorlandIDEServices, IOTAEditorServices, Result);
  if not Assigned(Result) then
    raise EJclExpertException.CreateRes(@RsENoEditorServices);
end;

class function TJclOTAExpertBase.GetOTAMessageServices: IOTAMessageServices;
begin
  Supports(BorlandIDEServices, IOTAMessageServices, Result);
  if not Assigned(Result) then
    raise EJclExpertException.CreateRes(@RsENoOTAMessageServices);
end;

class function TJclOTAExpertBase.GetOTAModuleServices: IOTAModuleServices;
begin
  Supports(BorlandIDEServices, IOTAModuleServices, Result);
  if not Assigned(Result) then
    raise EJclExpertException.CreateRes(@RsENoOTAModuleServices);
end;

class function TJclOTAExpertBase.GetOTAPackageServices: IOTAPackageServices;
begin
  Supports(BorlandIDEServices, IOTAPackageServices, Result);
  if not Assigned(Result) then
    raise EJclExpertException.CreateRes(@RsENoOTAPackageServices);
end;

{$IFDEF BDS}
class function TJclOTAExpertBase.GetOTAPersonalityServices: IOTAPersonalityServices;
begin
  Supports(BorlandIDEServices, IOTAPersonalityServices, Result);
  if not Assigned(Result) then
    raise EJclExpertException.CreateRes(@RsENoOTAPersonalityServices);
end;
{$ENDIF BDS}

{$IFDEF BDS4_UP}
class function TJclOTAExpertBase.GetOTAProjectManager: IOTAProjectManager;
begin
  Supports(BorlandIDEServices, IOTAProjectManager, Result);
  if not Assigned(Result) then
    raise EJclExpertException.CreateRes(@RsENoOTAProjectManager);
end;
{$ENDIF BDS4_UP}

class function TJclOTAExpertBase.GetOTAServices: IOTAServices;
begin
  Supports(BorlandIDEServices, IOTAServices, Result);
  if not Assigned(Result) then
    raise EJclExpertException.CreateRes(@RsENoOTAServices);
end;

class function TJclOTAExpertBase.GetOTAWizardServices: IOTAWizardServices;
begin
  Supports(BorlandIDEServices, IOTAWizardServices, Result);
  if not Assigned(Result) then
    raise EJclExpertException.CreateRes(@RsENoOTAWizardServices);
end;

function TJclOTAExpertBase.GetOutputDirectory(const Project: IOTAProject): string;
var
  {$IFDEF BDS8_UP}
  Configurations: IOTAProjectOptionsConfigurations;
  {$ENDIF BDS8_UP}
  EnvironmentOptions: IOTAEnvironmentOptions;
  OptionValue: Variant;
  EnvVariables: TStrings;
  Name: string;
  I: Integer;
begin
  if not Assigned(Project) then
    raise EJclExpertException.CreateRes(@RsENoActiveProject);
  if not Assigned(Project.ProjectOptions) then
      raise EJclExpertException.CreateRes(@RsENoProjectOptions);

  Result := '';

  if IsPackage(Project) then
  begin
    OptionValue := Project.ProjectOptions.Values[PkgDllDirOptionName];

    if VarIsStr(OptionValue) then
      Result := VarToStr(OptionValue);

    {$IFDEF BDS5}
    if (Project.Personality = JclCBuilderPersonality) and (Result = 'false') then
      Result := '';
    {$ENDIF BDS5}

    if Result = '' then
    begin
      EnvironmentOptions := GetOTAServices.GetEnvironmentOptions;
      if not Assigned(EnvironmentOptions) then
        raise EJclExpertException.CreateRes(@RsENoEnvironmentOptions);
      OptionValue := EnvironmentOptions.Values[BPLOutputDirOptionName];
      if VarIsStr(OptionValue) then
        Result := VarToStr(OptionValue);
    end;
  end
  else
  begin
    OptionValue := Project.ProjectOptions.Values[OutputDirOptionName];

    if VarIsStr(OptionValue) then
      Result := VarToStr(OptionValue);

    {$IFDEF BDS5}
    if (Project.Personality = JclCBuilderPersonality) and (Result = 'false') then
      Result := '';
    {$ENDIF BDS5}

    if Result = '' then
    begin
      OptionValue := Project.ProjectOptions.Values[FinalOutputDirOptionName];
      if VarIsStr(OptionValue) then
        Result := VarToStr(OptionValue);
    end;
  end;

  {$IFDEF BDS5}
  if (Project.Personality = JclCBuilderPersonality) and (Result = 'false') then
    Result := '';
  {$ENDIF BDS5}

  Result := Trim(Result);

  EnvVariables := TStringList.Create;
  try
    ReadEnvVariables(EnvVariables);
    {$IFDEF BDS8_UP}
    // add the config environment variable
    if Supports(Project.ProjectOptions, IOTAProjectOptionsConfigurations, Configurations) then
    begin
      EnvVariables.Values['Config'] := Configurations.ActiveConfigurationName;
      {$IFDEF BDS9_UP}
      EnvVariables.Values['Platform'] := Configurations.ActivePlatformName;
      {$ELSE}
      EnvVariables.Values['Platform'] := 'Win32';
      {$ENDIF BDS9_UP}
    end;
    {$ENDIF BDS8_UP}
    while Pos('$(', Result) > 0 do
    begin
      for I := 0 to EnvVariables.Count - 1 do
      begin
        Name := EnvVariables.Names[I];
        Result := StringReplace(Result, Format('$(%s)', [Name]),
          EnvVariables.Values[Name], [rfReplaceAll, rfIgnoreCase]);
      end;
    end;
  finally
    EnvVariables.Free;
  end;

  while Pos('\\', Result) > 0 do
    Result := StringReplace(Result, '\\', DirDelimiter, [rfReplaceAll]);

  if Result = '' then
    Result := ExtractFilePath(Project.FileName)
  else
  if not PathIsAbsolute(Result) then
    Result := PathGetRelativePath(ExtractFilePath(Project.FileName), Result);
end;

function TJclOTAExpertBase.GetActivePersonality: TJclBorPersonality;
{$IFDEF BDS}
var
  PersonalityText: string;
  OTAPersonalityServices: IOTAPersonalityServices;
  {$IFDEF COMPILER9_UP}
  ActiveProject: IOTAProject;
  {$ENDIF COMPILER9_UP}
begin
  {$IFDEF COMPILER9_UP}
  ActiveProject := ActiveProject;
  if Assigned(ActiveProject) then
    PersonalityText := ActiveProject.Personality
  else
  {$ENDIF COMPILER9_UP}
  OTAPersonalityServices := GetOTAPersonalityServices;
  PersonalityText := OTAPersonalityServices.CurrentPersonality;
  Result := PersonalityTextToId(PersonalityText);
end;
{$ELSE BDS}
begin
  {$IFDEF DELPHI}
  Result := bpDelphi32;
  {$ENDIF DELPHI}
  {$IFDEF BCB}
  Result := bpBCBuilder32;
  {$ENDIF BCB}
end;
{$ENDIF BDS}

function TJclOTAExpertBase.GetPageName: string;
begin
  // override to customize
  Result := '';
end;

class function TJclOTAExpertBase.GetProjectGroup: IOTAProjectGroup;
var
  OTAModuleServices: IOTAModuleServices;
  AModule: IOTAModule;
  I: Integer;
begin
  OTAModuleServices := GetOTAModuleServices;
  for I := 0 to OTAModuleServices.ModuleCount - 1 do
  begin
    AModule := OTAModuleServices.Modules[I];
    if not Assigned(AModule) then
      raise EJclExpertException.CreateRes(@RsENoModule);
    if AModule.QueryInterface(IOTAProjectGroup, Result) = S_OK then
      Exit;
  end;
  Result := nil;
end;

function TJclOTAExpertBase.GetRADInstallation: TJclBorRADToolInstallation;
var
  Installations: TJclBorRADToolInstallations;
  Installation: TJclBorRADToolInstallation;
  I: Integer;
  IDERegKey: string;
begin
  Result := nil;
  IDERegKey := StrEnsureNoSuffix('\', GetOTAServices.GetBaseRegistryKey);
  Installations := TJclBorRADToolInstallations.Create;
  try
    for I := 0 to Installations.Count - 1 do
    begin
      Installation := Installations.Installations[I];
      if StrSame(IDERegKey, StrEnsureNoSuffix('\', Installation.ConfigDataLocation)) then
        Result := TJclBorRADToolInstallationClass(Installation.ClassType).Create(Installation.ConfigDataLocation, Installation.RootKey);
    end;
  finally
    Installations.Free;
  end;
end;

function TJclOTAExpertBase.GetRootDir: string;
begin
  if FRootDir = '' then
  begin
    //(usc) another possibility for D7 or higher is to use IOTAServices.GetRootDirectory
    {$IFDEF MSWINDOWS}
    FRootDir := RegReadStringDef(HKEY_LOCAL_MACHINE, Settings.BaseKeyName, DelphiRootDirKeyValue, '');
    // (rom) bugfix if using -r switch of D9 by Dan Miser
    if FRootDir = '' then
      FRootDir := RegReadStringDef(HKEY_CURRENT_USER, Settings.BaseKeyName, DelphiRootDirKeyValue, '');
    {$ENDIF MSWINDOWS}
    if FRootDir = '' then
      raise EJclExpertException.CreateRes(@RsENoRootDir);
  end;
  Result := FRootDir;
end;

function TJclOTAExpertBase.IncludeInIDEInsight: Boolean;
begin
  // override to customize
  Result := True;
end;

function TJclOTAExpertBase.IsInstalledPackage(const Project: IOTAProject): Boolean;
var
  PackageFileName, ExecutableNameNoExt: TFileName;
  OTAPackageServices: IOTAPackageServices;
  I: Integer;
begin
  if not Assigned(Project) then
    raise EJclExpertException.CreateRes(@RsENoActiveProject);

  Result := IsPackage(Project);
  if Result then
  begin
    Result := False;

    if not Assigned(Project.ProjectOptions) then
      raise EJclExpertException.CreateRes(@RsENoProjectOptions);

    if not Project.ProjectOptions.Values[RuntimeOnlyOptionName] then
    begin
      ExecutableNameNoExt := ChangeFileExt(GetMapFileName(Project), '');
      OTAPackageServices := GetOTAPackageServices;

      for I := 0 to OTAPackageServices.PackageCount - 1 do
      begin
        PackageFileName := ChangeFileExt(OTAPackageServices.PackageNames[I], BinaryExtensionPackage);
        PackageFileName := GetModulePath(GetModuleHandle(PChar(PackageFileName)));
        if AnsiSameText(ChangeFileExt(PackageFileName, ''), ExecutableNameNoExt) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

function TJclOTAExpertBase.IsPackage(const Project: IOTAProject): Boolean;
var
  FileName: TFileName;
  FileExtension: string;
  Index: Integer;
  ProjectFile: TJclSimpleXML;
  PersonalityNode, SourceNode, ProjectExtensions, ProjectTypeNode: TJclSimpleXMLElem;
  NameProp: TJclSimpleXMLProp;
begin
  if not Assigned(Project) then
    raise EJclExpertException.CreateRes(@RsENoActiveProject);

  FileName := Project.FileName;
  FileExtension := ExtractFileExt(FileName);

  if AnsiSameText(FileExtension, SourceExtensionDProject) and FileExists(FileName) then
  begin
    Result := False;
    ProjectFile := TJclSimpleXML.Create;
    try
      ProjectFile.Options := ProjectFile.Options - [sxoAutoCreate];
      ProjectFile.LoadFromFile(FileName);
      ProjectExtensions := ProjectFile.Root.Items.ItemNamed['ProjectExtensions'];
      if Assigned(ProjectExtensions) then
      begin
        ProjectTypeNode := ProjectExtensions.Items.ItemNamed['Borland.ProjectType'];
        if Assigned(ProjectTypeNode) then
          Result := AnsiSameText(ProjectTypeNode.Value, 'Package');
      end;
    finally
      ProjectFile.Free;
    end;
  end
  else
  if AnsiSameText(FileExtension, SourceExtensionBDSProject) and FileExists(FileName) then
  begin
    Result := False;
    ProjectFile := TJclSimpleXML.Create;
    try
      ProjectFile.Options := ProjectFile.Options - [sxoAutoCreate];
      ProjectFile.LoadFromFile(FileName);
      PersonalityNode := ProjectFile.Root.Items.ItemNamed['Delphi.Personality'];
      if not Assigned(PersonalityNode) then
        PersonalityNode := ProjectFile.Root.Items.ItemNamed['CPlusPlusBuilder.Personality'];

      if Assigned(PersonalityNode) then
      begin
        SourceNode := PersonalityNode.Items.ItemNamed['Source'];
        if Assigned(SourceNode) then
        begin
          for Index := 0 to SourceNode.Items.Count - 1 do
            if AnsiSameText(SourceNode.Items.Item[0].Name, 'Source') then
          begin
            NameProp := SourceNode.Items.Item[0].Properties.ItemNamed['Name'];
            if Assigned(NameProp) and AnsiSameText(NameProp.Value, 'MainSource') then
            begin
              Result := AnsiSameText(ExtractFileExt(SourceNode.Items.Item[0].Value), SourceExtensionDelphiPackage);
              Break;
            end;
          end;
        end;
      end;
    finally
      ProjectFile.Free;
    end;
  end
  else
    Result := AnsiSameText(FileExtension, SourceExtensionDelphiPackage);
end;

class function TJclOTAExpertBase.IsPersonalityLoaded(
  const PersonalityName: string): Boolean;
{$IFDEF BDS}
var
  OTAPersonalityServices: IOTAPersonalityServices;
  Index: Integer;
begin
  OTAPersonalityServices := GetOTAPersonalityServices;
  Result := False;

  for Index := 0 to OTAPersonalityServices.PersonalityCount - 1 do
    if SameText(OTAPersonalityServices.Personalities[Index], PersonalityName) then
  begin
    Result := True;
    Break;
  end;
end;
{$ELSE BDS}
begin
  Result := True;
end;
{$ENDIF BDS}

procedure TJclOTAExpertBase.ReadEnvVariables(EnvVariables: TStrings);
var
  I: Integer;
  EnvNames: TStringList;
  {$IFDEF MSWINDOWS}
  EnvVarKeyName: string;
  {$ENDIF MSWINDOWS}
begin
  EnvVariables.Clear;

  // read user and system environment variables
  GetEnvironmentVars(EnvVariables, False);

  // read Delphi environment variables
  EnvNames := TStringList.Create;
  try
    {$IFDEF MSWINDOWS}
    EnvVarKeyName := Settings.BaseKeyName + EnvironmentVarsKey;
    if RegKeyExists(HKEY_CURRENT_USER, EnvVarKeyName) and
      RegGetValueNames(HKEY_CURRENT_USER, EnvVarKeyName, EnvNames) then
      for I := 0 to EnvNames.Count - 1 do
        EnvVariables.Values[EnvNames[I]] :=
          RegReadStringDef(HKEY_CURRENT_USER, EnvVarKeyName, EnvNames[I], '');
    {$ENDIF MSWINDOWS}
  finally
    EnvNames.Free;
  end;

  // add the Delphi directory
  EnvVariables.Values[DelphiEnvironmentVar] := RootDir;
end;

procedure TJclOTAExpertBase.RegisterCommands;
begin
  // override to add actions and menu items
end;

procedure TJclOTAExpertBase.UnregisterCommands;
begin
  // override to remove actions and menu items
end;

function TJclOTAExpertBase.ValidateContents: Boolean;
begin
  // override to customize
  Result := True;
end;

//=== { TJclOTAExpert } ======================================================

procedure TJclOTAExpert.AfterSave;
begin
end;

procedure TJclOTAExpert.BeforeSave;
begin
end;

procedure TJclOTAExpert.Destroyed;
begin
end;

procedure TJclOTAExpert.Execute;
begin
end;

function TJclOTAExpert.GetIDString: string;
begin
  Result := 'Jedi.' + ClassName;
end;

function TJclOTAExpert.GetName: string;
begin
  Result := ClassName;
end;

function TJclOTAExpert.GetState: TWizardState;
begin
  Result := [];
end;

procedure TJclOTAExpert.Modified;
begin

end;

{$IFDEF BDS7_UP}

//=== { TJclOTALocalMenu } ===================================================

procedure TJclOTALocalMenu.AfterSave;
begin

end;

procedure TJclOTALocalMenu.BeforeSave;
begin

end;

procedure TJclOTALocalMenu.Destroyed;
begin

end;

procedure TJclOTALocalMenu.Modified;
begin

end;

function TJclOTALocalMenu.GetCaption: string;
begin
  Result := FCaption;
end;

function TJclOTALocalMenu.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TJclOTALocalMenu.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TJclOTALocalMenu.GetHelpContext: Integer;
begin
  Result := FHelpContext;
end;

function TJclOTALocalMenu.GetName: string;
begin
  Result := FName;
end;

function TJclOTALocalMenu.GetParent: string;
begin
  Result := FParent;
end;

function TJclOTALocalMenu.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TJclOTALocalMenu.GetVerb: string;
begin
  Result := FVerb;
end;

procedure TJclOTALocalMenu.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TJclOTALocalMenu.SetChecked(Value: Boolean);
begin
  FChecked := Value;
end;

procedure TJclOTALocalMenu.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TJclOTALocalMenu.SetHelpContext(Value: Integer);
begin
  FHelpContext := Value;
end;

procedure TJclOTALocalMenu.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TJclOTALocalMenu.SetParent(const Value: string);
begin
  FParent := Value;
end;

procedure TJclOTALocalMenu.SetPosition(Value: Integer);
begin
  FPosition := Value;
end;

procedure TJclOTALocalMenu.SetVerb(const Value: string);
begin
  FVerb := Value;
end;

//=== { TJclOTAProjectManagerMenu } ==========================================

function TJclOTAProjectManagerMenu.GetIsMultiSelectable: Boolean;
begin
  Result := FIsMultiSelectable;
end;

procedure TJclOTAProjectManagerMenu.SetIsMultiSelectable(Value: Boolean);
begin
  FIsMultiSelectable := Value;
end;

procedure TJclOTAProjectManagerMenu.Execute(const MenuContextList: IInterfaceList);
begin
  if Assigned(FOnExecute) then
    FOnExecute(MenuContextList);
end;

function TJclOTAProjectManagerMenu.PreExecute(const MenuContextList: IInterfaceList): Boolean;
begin
  Result := True;
end;

function TJclOTAProjectManagerMenu.PostExecute(const MenuContextList: IInterfaceList): Boolean;
begin
  Result := True;
end;

{$ENDIF BDS7_UP}

{$IFDEF BDS}
function JclTitleVersion: string;
begin
  Result := Format(LoadResString(@RsAboutTitle), [JclVersionMajor, JclVersionMinor, JclVersionRelease, JclVersionBuild]);
end;

var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer = -1;
  SplashScreenInitialized: Boolean = False;

procedure RegisterAboutBox;
var
  ProductImage: HBITMAP;
begin
  if AboutBoxIndex = -1 then
  begin
    Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
    if not Assigned(AboutBoxServices) then
      raise EJclExpertException.CreateRes(@RsENoOTAAboutServices);
    ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'JCLSPLASH');
    if ProductImage = 0 then
      raise EJclExpertException.CreateRes(@RsENoBitmapResources);
    AboutBoxIndex := AboutBoxServices.AddPluginInfo(JclTitleVersion, LoadResString(@RsAboutDescription),
      ProductImage, False, LoadResString(@RsAboutLicenceStatus));
  end;
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> -1) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := -1;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterSplashScreen;
var
  ProductImage: HBITMAP;
begin
  if Assigned(SplashScreenServices) and not SplashScreenInitialized then
  begin
    ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'JCLSPLASH');
    if ProductImage = 0 then
      raise EJclExpertException.CreateRes(@RsENoBitmapResources);
    SplashScreenServices.AddPluginBitmap(JclTitleVersion, ProductImage,
      False, LoadResString(@RsAboutLicenceStatus));
    SplashScreenInitialized := True;
  end;
end;

{$ENDIF BDS}

initialization

try
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  Classes.RegisterClass(TJclWizardForm);
  Classes.RegisterClass(TJclWizardFrame);
except
  on ExceptionObj: TObject do
  begin
    JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

finalization

try
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  {$IFDEF BDS}
  UnregisterAboutBox;
  {$ENDIF BDS}
  FreeAndNil(GlobalExpertList);
except
  on ExceptionObj: TObject do
  begin
    JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

end.
