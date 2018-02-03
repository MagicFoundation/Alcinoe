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
{ The Original Code is JclMsBuild.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All Rights Reserved.     }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Routines around MsBuild project files.                                                           }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclMsBuild;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows,
  SysUtils,
  Classes,
  Contnrs,
  JclBase,
  JclFileUtils,
  JclRegistry,
  JclStreams,
  JclSimpleXml,
  JclSysInfo;

(* simple test procedure: load Jcl.dproj and emit custom properties and files to be compiled

procedure Test;
var
  MsBuildParser: TJclMsBuildParser;
begin
  MsBuildParser := TJclMsBuildParser.Create('C:\dev\jcl\jcl\packages\d11\Jcl.dproj');
  try
    MsBuildParser.Init;
    MsBuildParser.Parse;
    WriteLn(MsBuildParser.Properties.CustomProperties.Text);
    WriteLn(MsBuildParser.EvaluateString('@(DCCReference->''%(FullPath)'')'));
  finally
    MsBuildParser.Free;
  end;
end; *)

type
  EJclMsBuildError = class(EJclError);

  TJclMsBuildItem = class
  private
    FItemName: string;
    FItemInclude: string;
    FItemFullInclude: string; //full path
    FItemExclude: string;
    FItemRemove: string;
    FItemMetaData: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    property ItemName: string read FItemName;
    property ItemInclude: string read FItemInclude;
    property ItemFullInclude: string read FItemFullInclude;
    property ItemExclude: string read FItemExclude;
    property ItemRemove: string read FItemRemove;
    property ItemMetaData: TStrings read FItemMetaData;
  end;

  TJclMsBuildTaskOutput = class
  private
    FTaskParameter: string;
    FPropertyName: string;
    FItemName: string;
  public
    property TaskParameter: string read FTaskParameter;
    property PropertyName: string read FPropertyName;
    property ItemName: string read FItemName;
  end;

  TJclMsBuildParameter = class
  private
    FParameterName: string;
    FParameterType: string;
    FOutput: Boolean;
    FRequired: Boolean;
  public
    property ParameterName: string read FParameterName;
    property ParameterType: string read FParameterType;
    property Output: Boolean read FOutput;
    property Required: Boolean read FRequired;
  end;

  TJclMsBuildUsingTask = class
  private
    FAssemblyName: string;
    FAssemblyFile: string;
    FTaskFactory: string;
    FTaskName: string;
    FParameters: TObjectList;
    FTaskBody: string;
    function AddParameter(Parameter: TJclMsBuildParameter): Integer;
    function GetParameterCount: Integer;
    function GetParameter(Index: Integer): TJclMsBuildParameter;
  public
    constructor Create;
    destructor Destroy; override;
    property AssemblyName: string read FAssemblyName;
    property AssemblyFile: string read FAssemblyFile;
    property TaskFactory: string read FTaskFactory;
    property TaskName: string read FTaskName;
    property ParameterCount: Integer read GetParameterCount;
    property Parameters[Index: Integer]: TJclMsBuildParameter read GetParameter;
    property TaskBody: string read FTaskBody;
  end;

  TJclMsBuildTask = class
  private
    FTaskName: string;
    FContinueOnError: Boolean;
    FParameters: TStrings;
    FOutputs: TObjectList;
    function AddOutput(AOutput: TJclMsBuildTaskOutput): Integer;
    function GetOutputCount: Integer;
    function GetOutput(Index: Integer): TJclMsBuildTaskOutput;
  public
    constructor Create;
    destructor Destroy; override;
    property TaskName: string read FTaskName;
    property ContinueOnError: Boolean read FContinueOnError;
    property Parameters: TStrings read FParameters;
    property OutputCount: Integer read GetOutputCount;
    property Outputs[Index: Integer]: TJclMsBuildTaskOutput read GetOutput;
  end;

  TJclMsBuildTarget = class
  private
    FTargetName: string;
    FDepends: TStrings;
    FReturns: TStrings;
    FInputs: TStrings;
    FOutputs: TStrings;
    FBeforeTargets: TStrings;
    FAfterTargets: TStrings;
    FKeepDuplicateOutputs: Boolean;
    FTasks: TObjectList;
    FErrorTargets: TStrings;
    function AddTask(Task: TJclMsBuildTask): Integer;
    function GetTaskCount: Integer;
    function GetTask(Index: Integer): TJclMsBuildTask;
  public
    constructor Create;
    destructor Destroy; override;
    property TargetName: string read FTargetName;
    property Depends: TStrings read FDepends;
    property Returns: TStrings read FReturns;
    property Inputs: TStrings read FInputs;
    property Outputs: TStrings read FOutputs;
    property BeforeTargets: TStrings read FBeforeTargets;
    property AfterTargets: TStrings read FAfterTargets;
    property KeepDuplicateOutputs: Boolean read FKeepDuplicateOutputs;
    property TaskCount: Integer read GetTaskCount;
    property Tasks[Index: Integer]: TJclMsBuildTask read GetTask;
    property ErrorTargets: TStrings read FErrorTargets;
  end;

  TJclMsBuildParser = class;

  // TStrings wrapper for all the MsBuild properties, values are searched
  // in the following ordered property classes:
  //  - reserved properties defined by MsBuild,
  //      their value cannot be overriden or an error will be raised
  //  - properties taken from the emulated "command line"
  //      their value cannot be overriden, no error is raised
  //  - custom properties defined by the script
  //  - environment properties taken from the environment variables
  //      their value can be silently overriden
  //  - registry properties handled by event
  //  - function properties handled by event

  TJclMsBuildProperties = class(TStrings)
  private
    FParser: TJclMsBuildParser;
    FReservedProperties: TStrings;
    FGlobalProperties: TStrings;
    FCustomProperties: TStrings;
    FEnvironmentProperties: TStrings;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    function GetRawValue(const Name: string): string;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetRawValue(const Name, Value: string);
  public
    constructor Create(AParser: TJclMsBuildParser);
    destructor Destroy; override;

    property Parser: TJclMsBuildParser read FParser;

    procedure Clear; override;

    procedure Delete(Index: Integer); override;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;

    procedure MergeEnvironmentProperties(SystemEnvironmentProperties: TStrings);

    property ReservedProperties: TStrings read FReservedProperties;
    property EnvironmentProperties: TStrings read FEnvironmentProperties;
    property GlobalProperties: TStrings read FGlobalProperties;
    property CustomProperties: TStrings read FCustomProperties;

    property RawValues[const Name: string]: string read GetRawValue write SetRawValue;
  end;

  TJclMsBuildImportEvent = procedure (Sender: TJclMsBuildParser; var FileName: TFileName;
    var SubXml: TJclSimpleXml; var SubOwnsXml: Boolean) of object;
  TJclMsBuildToolsVersionEvent = procedure (Sender: TJclMsBuildParser; const ToolsVersion: string) of object;
  TJclMsBuildRegistryPropertyEvent = function (Sender: TJclMsBuildParser; Root: HKEY;
    const Path, Name: string; out Value: string): Boolean of object;
  TJclMsBuildFunctionPropertyEvent = function (Sender: TJclMsBuildParser; const Command: string;
    out Value: string): Boolean of object;

  TJclMsBuildParser = class
  private
    FCurrentFileName: TFileName;
    FProjectFileName: TFileName;
    FXml: TJclSimpleXml;
    FOwnsXml: Boolean;
    FProperties: TJclMsBuildProperties;
    FItems: TObjectList;
    FItemDefinitions: TObjectList;
    FTargets: TObjectList;
    FUsingTasks: TObjectList;
    FInitialTargets: TStrings;
    FDefaultTargets: TStrings;
    FToolsVersion: string;
    FDotNetVersion: string;
    FIgnoreFunctionProperties: Boolean;
    FWorkingDirectory: string;
    FFirstPropertyGroup: TJclSimpleXMLElem;
    FProjectExtensions: TJclSimpleXMLElem;
    FOnImport: TJclMsBuildImportEvent;
    FOnToolsVersion: TJclMsBuildToolsVersionEvent;
    FOnRegistryProperty: TJclMsBuildRegistryPropertyEvent;
    FOnFunctionProperty: TJclMsBuildFunctionPropertyEvent;
    function GetItemCount: Integer;
    function GetItem(Index: Integer): TJclMsBuildItem;
    function GetItemDefinitionCount: Integer;
    function GetItemDefinition(Index: Integer): TJclMsBuildItem;
    function GetTargetCount: Integer;
    function GetTarget(Index: Integer): TJclMsBuildTarget;
    function GetUsingTaskCount: Integer;
    function GetUsingTask(Index: Integer): TJclMsBuildUsingTask;
    procedure ParseChoose(XmlElem: TJclSimpleXmlElem);
    procedure ParseImport(XmlElem: TJclSimpleXmlElem);
    procedure ParseImportGroup(XmlElem: TJclSimpleXmlElem);
    procedure ParseItem(XmlElem: TJclSimpleXmlElem; Definition: Boolean);
    procedure ParseItemDefinitionGroup(XmlElem: TJclSimpleXmlElem);
    procedure ParseItemGroup(XmlElem: TJclSimpleXmlElem);
    procedure ParseItemMetaData(XmlElem: TJclSimpleXmlElem; ItemMetaData: TStrings);
    procedure ParseOnError(XmlElem: TJclSimpleXMLElem; Target: TJclMsBuildTarget);
    function ParseOtherwise(XmlElem: TJclSimpleXmlElem; Skip: Boolean): Boolean;
    procedure ParseOutput(XmlElem: TJclSimpleXMLElem; Task: TJclMsBuildTask);
    procedure ParseParameter(XmlElem: TJclSimpleXMLElem; UsingTask: TJclMsBuildUsingTask);
    procedure ParseParameterGroup(XmlElem: TJclSimpleXMLElem; UsingTask: TJclMsBuildUsingTask);
    procedure ParseProject(XmlElem: TJclSimpleXmlElem);
    procedure ParseProperty(XmlElem: TJclSimpleXmlElem);
    procedure ParsePropertyGroup(XmlElem: TJclSimpleXmlElem);
    procedure ParseTarget(XmlElem: TJclSimpleXmlElem);
    procedure ParseTask(XmlElem: TJclSimpleXMLElem; Target: TJclMsBuildTarget);
    procedure ParseTaskBody(XmlElem: TJclSimpleXMLElem; UsingTask: TJclMsBuildUsingTask);
    procedure ParseUsingTask(XmlElem: TJclSimpleXmlElem);
    function ParseWhen(XmlElem: TJclSimpleXmlElem; Skip: Boolean): Boolean;
    procedure ParseXml(AXml: TJclSimpleXML);
  protected
    function GetPropertyValue(const Name: string): string; virtual;
    procedure SetPropertyValue(const Name, Value: string); virtual;
  public
    // evaluate known MsBuild properties
    // http://msdn.microsoft.com/en-us/library/ms171458.aspx
    function EvaluateFunctionProperty(const Command: string): string;
    function EvaluateList(const Name: string): string;
    function EvaluateRegistryProperty(Root: HKEY; const Path, Name: string): string;
    function EvaluateString(const S: string): string;
    function EvaluateTransform(ItemList: TStrings; const Transform: string): string;
  public
    // this function parses MsBuild condition as described at:
    // http://msdn.microsoft.com/en-us/library/7szfhaft.aspx
    function ParseCondition(const Condition: string): Boolean;
    function ParseConditionLength(const Condition: string; var Position: Integer; Len: Integer): Boolean;
    function ParseConditionOperand(const Condition: string; var Position: Integer; Len: Integer): Boolean;
    function ParseConditionString(const Condition: string; var Position: Integer; Len: Integer): string;
  public
    constructor Create(const AFileName: TFileName; AXml: TJclSimpleXml; AOwnsXml: Boolean = False); overload;
    constructor Create(const AFileName: TFileName; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP); overload;
    constructor Create(const AFileName: TFileName; ExtraImportsFileName: array of string; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure ClearItems;
    procedure ClearItemDefinitions;
    procedure ClearTargets;

    procedure Parse;
    procedure Save;

    procedure FindItemIncludes(const ItemName: string; List: TStrings);
    function FindItemDefinition(const ItemName: string): TJclMsBuildItem;
    function FindTarget(const TargetName: string): TJclMsBuildTarget;

    class function SameItemName(const ItemName1, ItemName2: string): Boolean;

    procedure Init;
    procedure InitEnvironmentProperties;
    procedure InitReservedProperties;

    // encode just <, > and &
    procedure XMLEncodeValue(Sender: TObject; var Value: string);
    // decode all entities
    procedure XMLDecodeValue(Sender: TObject; var Value: string);

    property CurrentFileName: TFileName read FCurrentFileName;
    property ProjectFileName: TFileName read FProjectFileName;
    property Xml: TJclSimpleXml read FXml;
    property OwnsXml: Boolean read FOwnsXml write FOwnsXml;
    property Properties: TJclMsBuildProperties read FProperties;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TJclMsBuildItem read GetItem;
    property ItemDefinitionCount: Integer read GetItemDefinitionCount;
    property ItemDefinitions[Index: Integer]: TJclMsBuildItem read GetItemDefinition;
    property TargetCount: Integer read GetTargetCount;
    property Targets[Index: Integer]: TJclMsBuildTarget read GetTarget;
    property UsingTaskCount: Integer read GetUsingTaskCount;
    property UsingTasks[Index: Integer]: TJclMsBuildUsingTask read GetUsingTask;
    property ProjectExtensions: TJclSimpleXMLElem read FProjectExtensions;
    property InitialTargets: TStrings read FInitialTargets;
    property DefaultTargets: TStrings read FDefaultTargets;
    property ToolsVersion: string read FToolsVersion;
    property DotNetVersion: string read FDotNetVersion write FDotNetVersion;
    property IgnoreFunctionProperties: Boolean read FIgnoreFunctionProperties write FIgnoreFunctionProperties;
    property WorkingDirectory: string read FWorkingDirectory write FWorkingDirectory;
    property OnImport: TJclMsBuildImportEvent read FOnImport write FOnImport;
    property OnToolsVersion: TJclMsBuildToolsVersionEvent read FOnToolsVersion write FOnToolsVersion;
    property OnRegistryProperty: TJclMsBuildRegistryPropertyEvent read FOnRegistryProperty write FOnRegistryProperty;
    property OnFunctionProperty: TJclMsBuildFunctionPropertyEvent read FOnFunctionProperty write FOnFunctionProperty;
  end;

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
  SysConst,
  JclWin32,
  JclDotNet,
  JclShell,
  JclStrings,
  JclDevToolsResources;

//=== { TJclMsBuildItem } ====================================================

constructor TJclMsBuildItem.Create;
begin
  inherited Create;
  FItemMetaData := TStringList.Create;
end;

destructor TJclMsBuildItem.Destroy;
begin
  FItemMetaData.Free;
  inherited Destroy;
end;

//=== { TJclMsBuildUsingTask } ===============================================

function TJclMsBuildUsingTask.AddParameter(Parameter: TJclMsBuildParameter): Integer;
begin
  Result := FParameters.Add(Parameter);
end;

constructor TJclMsBuildUsingTask.Create;
begin
  inherited Create;
  FParameters := TObjectList.Create(True);
end;

destructor TJclMsBuildUsingTask.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

function TJclMsBuildUsingTask.GetParameter(
  Index: Integer): TJclMsBuildParameter;
begin
  Result := TJclMsBuildParameter(FParameters.Items[Index]);
end;

function TJclMsBuildUsingTask.GetParameterCount: Integer;
begin
  Result := FParameters.Count;
end;

//=== { TJclMsBuildTask } ====================================================

constructor TJclMsBuildTask.Create;
begin
  inherited Create;
  FParameters := TStringList.Create;
  FOutputs := TObjectList.Create(True);
end;

destructor TJclMsBuildTask.Destroy;
begin
  FOutputs.Free;
  FParameters.Free;
  inherited Destroy;
end;

function TJclMsBuildTask.AddOutput(AOutput: TJclMsBuildTaskOutput): Integer;
begin
  Result := FOutputs.Add(AOutput);
end;

function TJclMsBuildTask.GetOutput(Index: Integer): TJclMsBuildTaskOutput;
begin
  Result := TJclMsBuildTaskOutput(FOutputs.Items[Index]);
end;

function TJclMsBuildTask.GetOutputCount: Integer;
begin
  Result := FOutputs.Count;
end;

//=== { TJclMsBuildTarget } ==================================================

constructor TJclMsBuildTarget.Create;
begin
  inherited Create;
  FDepends := TStringList.Create;
  FReturns := TStringList.Create;
  FInputs := TStringList.Create;
  FOutputs := TStringList.Create;
  FBeforeTargets := TStringList.Create;
  FAfterTargets := TStringList.Create;
  FTasks := TObjectList.Create(True);
  FErrorTargets := TStringList.Create;
end;

destructor TJclMsBuildTarget.Destroy;
begin
  FErrorTargets.Free;
  FTasks.Free;
  FAfterTargets.Free;
  FBeforeTargets.Free;
  FOutputs.Free;
  FInputs.Free;
  FReturns.Free;
  FDepends.Free;
  inherited Destroy;
end;

function TJclMsBuildTarget.AddTask(Task: TJclMsBuildTask): Integer;
begin
  Result := FTasks.Add(Task);
end;

function TJclMsBuildTarget.GetTask(Index: Integer): TJclMsBuildTask;
begin
  Result := TJclMsBuildTask(FTasks.Items[Index]);
end;

function TJclMsBuildTarget.GetTaskCount: Integer;
begin
  Result := FTasks.Count;
end;

//=== { TJclMsBuildProperties } ==============================================

constructor TJclMsBuildProperties.Create(AParser: TJclMsBuildParser);
begin
  inherited Create;
  FParser := AParser;
  FReservedProperties := TStringList.Create;
  FGlobalProperties := TStringList.Create;
  FCustomProperties := TStringList.Create;
  FEnvironmentProperties := TStringList.Create;
end;

destructor TJclMsBuildProperties.Destroy;
begin
  FEnvironmentProperties.Free;
  FCustomProperties.Free;
  FGlobalProperties.Free;
  FReservedProperties.Free;
  inherited Destroy;
end;

procedure TJclMsBuildProperties.Clear;
begin
  ReservedProperties.Clear;
  CustomProperties.Clear;
  EnvironmentProperties.Clear;
  GlobalProperties.Clear;
end;

procedure TJclMsBuildProperties.Delete(Index: Integer);
begin
  //  - reserved properties defined by MsBuild,
  //      their value cannot be overriden or an error will be raised
  if (Index >= 0) and (Index < FReservedProperties.Count) then
    raise EJclMsBuildError.CreateRes(@RsEReservedProperty);
  Dec(Index, FReservedProperties.Count);

  //  - properties taken from the emulated "command line"
  //      their value cannot be overriden, no error is raised
  if (Index >= 0) and (Index < FGlobalProperties.Count) then
    Exit;
  Dec(Index, FGlobalProperties.Count);
  
  //  - custom properties defined by the script
  if (Index >= 0) and (Index < FCustomProperties.Count) then
  begin
    FCustomProperties.Delete(Index);
    Exit;
  end;
  Dec(Index, FCustomProperties.Count);

  //  - environment properties taken from the environment variables
  //      their value can be silently overriden
  if (Index >= 0) and (Index < FEnvironmentProperties.Count) then
  begin
    FEnvironmentProperties.Delete(Index);
    Exit;
  end;
  
  raise EJclMsBuildError.CreateRes(@SRangeError);
end;

function TJclMsBuildProperties.Get(Index: Integer): string;
begin
  //  - reserved properties defined by MsBuild,
  if (Index >= 0) and (Index < FReservedProperties.Count) then
  begin
    Result := FReservedProperties.Strings[Index];
    Exit;
  end;
  Dec(Index, FReservedProperties.Count);

  //  - properties taken from the emulated "command line"
  if (Index >= 0) and (Index < FGlobalProperties.Count) then
  begin
    Result := FGlobalProperties.Strings[Index];
    Exit;
  end;
  Dec(Index, FGlobalProperties.Count);

  //  - custom properties defined by the script
  if (Index >= 0) and (Index < FCustomProperties.Count) then
  begin
    Result := FCustomProperties.Strings[Index];
    Exit;
  end;
  Dec(Index, FCustomProperties.Count);

  //  - environment properties taken from the environment variables
  if (Index >= 0) and (Index < FEnvironmentProperties.Count) then
  begin
    Result := FEnvironmentProperties.Strings[Index];
    Exit;
  end;
  
  raise EJclMsBuildError.CreateRes(@SRangeError);
end;

function TJclMsBuildProperties.GetCount: Integer;
begin
  Result := FReservedProperties.Count + FGlobalProperties.Count +
    FCustomProperties.Count + FEnvironmentProperties.Count;
end;

function TJclMsBuildProperties.GetObject(Index: Integer): TObject;
begin
  //  - reserved properties defined by MsBuild,
  if (Index >= 0) and (Index < FReservedProperties.Count) then
  begin
    Result := FReservedProperties.Objects[Index];
    Exit;
  end;
  Dec(Index, FReservedProperties.Count);

  //  - properties taken from the emulated "command line"
  if (Index >= 0) and (Index < FGlobalProperties.Count) then
  begin
    Result := FGlobalProperties.Objects[Index];
    Exit;
  end;
  Dec(Index, FGlobalProperties.Count);

  //  - custom properties defined by the script
  if (Index >= 0) and (Index < FCustomProperties.Count) then
  begin
    Result := FCustomProperties.Objects[Index];
    Exit;
  end;
  Dec(Index, FCustomProperties.Count);

  //  - environment properties taken from the environment variables
  if (Index >= 0) and (Index < FEnvironmentProperties.Count) then
  begin
    Result := FEnvironmentProperties.Objects[Index];
    Exit;
  end;
  
  raise EJclMsBuildError.CreateRes(@SRangeError);
end;

function TJclMsBuildProperties.GetRawValue(const Name: string): string;
var
  Index: Integer;
  XmlElem: TJclSimpleXmlElem;
begin
  Index := IndexOfName(Name);
  XmlElem := nil;
  if Index >= 0 then
    XmlElem := TJclSimpleXMLElem(Objects[Index]);
  if Assigned(XmlElem) then
    Result := XmlElem.Value
  else
    Result := '';
end;

function TJclMsBuildProperties.IndexOf(const S: string): Integer;
begin
  //  - reserved properties defined by MsBuild,
  Result := FReservedProperties.IndexOf(S);
  if Result >= 0 then
    Exit;

  //  - properties taken from the emulated "command line"
  Result := FGlobalProperties.IndexOf(S);
  if Result >= 0 then
  begin
    Inc(Result, FReservedProperties.Count);
    Exit;
  end;

  //  - custom properties defined by the script
  Result := FCustomProperties.IndexOf(S);
  if Result >= 0 then
  begin
    Inc(Result, FReservedProperties.Count);
    Inc(Result, FGlobalProperties.Count);
    Exit;
  end;

  //  - environment properties taken from the environment variables
  Result := FEnvironmentProperties.IndexOf(S);
  if Result >= 0 then
  begin
    Inc(Result, FReservedProperties.Count);
    Inc(Result, FGlobalProperties.Count);
    Inc(Result, FCustomProperties.Count);
    Exit;
  end;
end;

procedure TJclMsBuildProperties.Insert(Index: Integer; const S: string);
begin
  //  - reserved properties defined by MsBuild,
  //      their value cannot be overriden or an error will be raised
  if (Index >= 0) and (Index < FReservedProperties.Count) then
    raise EJclMsBuildError.CreateRes(@RsEReservedProperty);
  Dec(Index, FReservedProperties.Count);

  //  - properties taken from the emulated "command line"
  //      their value cannot be overriden, no error is raised
  if (Index >= 0) and (Index < FGlobalProperties.Count) then
    Exit;
  Dec(Index, FGlobalProperties.Count);

  //  - custom properties defined by the script
  if (Index >= 0) and (Index < FCustomProperties.Count) then
  begin
    FCustomProperties.Insert(Index, S);
    Exit;
  end;
  Dec(Index, FCustomProperties.Count);

  //  - environment properties taken from the environment variables
  //      their value can be silently overriden
  if (Index >= 0) and (Index <= FEnvironmentProperties.Count) then
  begin
    FCustomProperties.Add(S);
    Exit;
  end;
  
  raise EJclMsBuildError.CreateRes(@SRangeError);
end;

procedure TJclMsBuildProperties.MergeEnvironmentProperties(
  SystemEnvironmentProperties: TStrings);
var
  I: Integer;
  PropName: string;
  SystemValue: string;
begin
  for I := 0 to FEnvironmentProperties.Count - 1 do
  begin
    PropName := FEnvironmentProperties.Names[I];
    SystemValue := SystemEnvironmentProperties.Values[PropName];
    if SystemValue <> '' then
      FEnvironmentProperties.Values[PropName] := SystemValue;
  end;
end;

procedure TJclMsBuildProperties.Put(Index: Integer; const S: string);
begin
  //  - reserved properties defined by MsBuild,
  //      their value cannot be overriden or an error will be raised
  if (Index >= 0) and (Index < FReservedProperties.Count) then
    raise EJclMsBuildError.CreateRes(@RsEReservedProperty);
  Dec(Index, FReservedProperties.Count);

  //  - properties taken from the emulated "command line"
  //      their value cannot be overriden, no error is raised
  if (Index >= 0) and (Index < FGlobalProperties.Count) then
    Exit;
  Dec(Index, FGlobalProperties.Count);

  //  - custom properties defined by the script
  if (Index >= 0) and (Index < FCustomProperties.Count) then
  begin
    FCustomProperties.Strings[Index] := S;
    Exit;
  end;
  Dec(Index, FCustomProperties.Count);

  //  - environment properties taken from the environment variables
  //      their value can be silently overriden
  if (Index >= 0) and (Index < FEnvironmentProperties.Count) then
  begin
    if (FCustomProperties.Count > 0) and (FCustomProperties.Strings[FCustomProperties.Count - 1] = '') then
      FCustomProperties.Strings[FCustomProperties.Count - 1] := S
    else
      FCustomProperties.Add(S);
    Exit;
  end;
  
  raise EJclMsBuildError.CreateRes(@SRangeError);
end;

procedure TJclMsBuildProperties.PutObject(Index: Integer; AObject: TObject);
begin
  //  - reserved properties defined by MsBuild,
  if (Index >= 0) and (Index < FReservedProperties.Count) then
  begin
    FReservedProperties.Objects[Index] := AObject;
    Exit;
  end;
  Dec(Index, FReservedProperties.Count);

  //  - properties taken from the emulated "command line"
  if (Index >= 0) and (Index < FGlobalProperties.Count) then
  begin
    FGlobalProperties.Objects[Index] := AObject;
    Exit;
  end;
  Dec(Index, FGlobalProperties.Count);

  //  - custom properties defined by the script
  if (Index >= 0) and (Index < FCustomProperties.Count) then
  begin
    FCustomProperties.Objects[Index] := AObject;
    Exit;
  end;
  Dec(Index, FCustomProperties.Count);

  //  - environment properties taken from the environment variables
  if (Index >= 0) and (Index < FEnvironmentProperties.Count) then
  begin
    FEnvironmentProperties.Objects[Index] := AObject;
    Exit;
  end;
  
  raise EJclMsBuildError.CreateRes(@SRangeError);
end;

procedure TJclMsBuildProperties.SetRawValue(const Name, Value: string);
var
  Index: Integer;
  XmlElem: TJclSimpleXmlElem;
begin
  Index := IndexOfName(Name);
  XmlElem := nil;
  if Index >= 0 then
    XmlElem := TJclSimpleXMLElem(Objects[Index]);
  if Assigned(XmlElem) then
    XmlElem.Value := Value
  else
  if Assigned(Parser.FFirstPropertyGroup) then
    Parser.FFirstPropertyGroup.Items.Add(Name, Value)
  else
    raise EJclMsBuildError.CreateResFmt(@RsELocateXmlElem, [Name]);
end;

//=== { TJclMsBuildParser } ==================================================

constructor TJclMsBuildParser.Create(const AFileName: TFileName; Encoding: TJclStringEncoding; CodePage: Word);
var
  AXml: TJclSimpleXML;
begin
  AXml := TJclSimpleXML.Create;
  try
    AXml.Options := AXml.Options - [sxoAutoEncodeValue,sxoAutoEncodeEntity];
    AXml.OnEncodeValue := XMLEncodeValue;
    AXml.OnDecodeValue := XMLDecodeValue;
    AXml.LoadFromFile(AFileName, Encoding, CodePage);
  except
    AXml.Free;
    raise;
  end;
  Create(AFileName, AXml, True);
end;

constructor TJclMsBuildParser.Create(const AFileName: TFileName; ExtraImportsFileName: array of string; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
var
  I: Integer;
begin
  Create(AFileName, Encoding, CodePage);

  for I := Low(ExtraImportsFileName) to High(ExtraImportsFileName) do
    FXML.Root.Items.Insert('Import', I - Low(ExtraImportsFileName)).Properties.Add('Project', ExtraImportsFileName[I]);
end;

constructor TJclMsBuildParser.Create(const AFileName: TFileName;
  AXml: TJclSimpleXml; AOwnsXml: Boolean);
begin
  inherited Create;
  FProjectFileName := AFileName;
  FXml := AXml;
  FOwnsXml := AOwnsXml;
  FProperties := TJclMsBuildProperties.Create(Self);
  FItems := TObjectList.Create(True);
  FItemDefinitions := TObjectList.Create(True);
  FTargets := TObjectList.Create(True);
  FUsingTasks := TObjectList.Create(True);
  FInitialTargets := TStringList.Create;
  FDefaultTargets := TStringList.Create;
  FIgnoreFunctionProperties := True;
end;

destructor TJclMsBuildParser.Destroy;
begin
  FDefaultTargets.Free;
  FInitialTargets.Free;
  FUsingTasks.Free;
  FTargets.Free;
  FItemDefinitions.Free;
  FItems.Free;
  FProperties.Free;
  if FOwnsXml then
    FXml.Free;
  inherited Destroy;
end;

procedure TJclMsBuildParser.Clear;
begin
  ClearItems;
  ClearItemDefinitions;
  ClearTargets;
  FProperties.Clear;
  FInitialTargets.Clear;
  FDefaultTargets.Clear;
  FProjectExtensions := nil;
end;

procedure TJclMsBuildParser.ClearItemDefinitions;
begin
  FItemDefinitions.Clear;
end;

procedure TJclMsBuildParser.ClearItems;
begin
  FItems.Clear;
end;

procedure TJclMsBuildParser.ClearTargets;
begin
  FTargets.Clear;
end;

function TJclMsBuildParser.EvaluateString(const S: string): string;

  function FindClosingBrace(const R: string; var Position: Integer): Boolean;
  var
    Index, Len, BraceCount: Integer;
    Quotes: string;
  begin
    Len := Length(R);
    BraceCount := 0;
    Quotes := '';
    while (Position <= Len) do
    begin
      // handle quotes first
      if (R[Position] = NativeSingleQuote) then
      begin
        Index := JclStrings.CharPos(Quotes, NativeSingleQuote);
        if Index >= 0 then
          SetLength(Quotes, Index - 1)
        else
          Quotes := Quotes + NativeSingleQuote;
      end;

      if (R[Position] = NativeDoubleQuote) then
      begin
        Index := JclStrings.CharPos(Quotes, NativeDoubleQuote);
        if Index >= 0 then
          SetLength(Quotes, Index - 1)
        else
          Quotes := Quotes + NativeDoubleQuote;
      end;

      if (R[Position] = '`') then
      begin
        Index := JclStrings.CharPos(Quotes, '`');
        if Index >= 0 then
          SetLength(Quotes, Index - 1)
        else
          Quotes := Quotes + '`';
      end;

      if Quotes = '' then
      begin
        if R[Position] = ')' then
        begin
          Dec(BraceCount);
          if BraceCount = 0 then
            Break;
        end
        else
        if R[Position] = '(' then
          Inc(BraceCount);
      end;
      Inc(Position);
    end;
    Result := Position <= Len;

//    Delphi XE's CodeGear.Delphi.Targets has a bug where the closing paran is missing
//    "'$(DelphiWin32DebugDCUPath'!=''". But it is still a valid string and not worth
//    an exception.
//
//    if Position > Len then
//      raise EJclMsBuildError.CreateResFmt(@RsEEndOfString, [S]);
  end;

var
  Start, Position, Index: Integer;
  PropertyName, PropertyValue, Path, Name: string;
  Prop, Reg: Boolean;
  Root: THandle;
begin
  Result := S;
  if Result <> '' then
  begin
    repeat
      // start with the last match in order to convert $(some$(other))
      // evaluate properties
      Start := StrLastPos('$(', Result);
      if Start > 0 then
      begin
        Position := Start;
        if not FindClosingBrace(Result, Position) then
          Break;
        PropertyName := Copy(Result, Start + 2, Position - Start - 2);

        Prop := True;
        for Index := 1 to Length(PropertyName) do
          if not CharIsValidIdentifierLetter(PropertyName[Index]) then
        begin
          Prop := False;
          Break;
        end;
        if Prop then
          PropertyValue := GetPropertyValue(PropertyName)
        else
        begin
          Reg := Copy(PropertyName, 1, 9) = 'registry:';
          if Reg then
          begin
            PropertyName := Copy(PropertyName, 10, Length(PropertyName) - 9);
            Index := CharPos(PropertyName, '\');
            Root := RootKeyValue(Copy(PropertyName, 1, Index - 1));
            PropertyName := Copy(PropertyName, Index + 1, Length(PropertyName) - Index);
            Index := CharPos(PropertyName, '@');
            if Index >= 0 then
            begin
              Path := Copy(PropertyName, 1, Index - 1);
              Name := Copy(PropertyName, Index + 1, Length(PropertyName) - Index);
            end
            else
            begin
              Path := PropertyName;
              Name := '';
            end;
            PropertyValue := EvaluateRegistryProperty(Root, Path, Name);
          end
          else
            PropertyValue := EvaluateFunctionProperty(PropertyName);
        end;
        StrReplace(Result,
                   Copy(Result, Start, Position - Start + 1), // $(PropertyName)
                   PropertyValue,
                   [rfReplaceAll, rfIgnoreCase])
      end;
      if Start = 0 then
      begin
        // evaluate item list
        Start := StrLastPos('@(', Result);
        if Start > 0 then
        begin
          Position := Start;
          if not FindClosingBrace(Result, Position) then
            raise EJclMsBuildError.CreateResFmt(@RsEEndOfString, [Result]);
          PropertyName := Copy(Result, Start + 2, Position - Start - 2);

          PropertyValue := EvaluateList(PropertyName);

          StrReplace(Result,
                     Copy(Result, Start, Position - Start + 1), // @(PropertyName...)
                     PropertyValue,
                     [rfReplaceAll, rfIgnoreCase])
        end;
      end;
    until Start = 0;
    // convert hexa to decimal
    if Copy(Result, 1, 2) = '0x' then
      Result := IntToStr(StrToInt64('$' + Copy(Result, 3, Length(Result) - 2)));
  end;
end;

function TJclMsBuildParser.EvaluateTransform(ItemList: TStrings; const Transform: string): string;
type
  TVarRecArray = array of TVarRec;
const
  WellKnownItemMetadataCount = 11;
var
  UserDefinedMetadataNames: TStrings;

  function GetTransformPattern(const Transform: string): string;
  var
    Index, EndIndex, Num: Integer;
    MetaDataName: string;
  begin
    Result := Transform;
    StrReplace(Result, '%(FullPath)', '%0:s', [rfReplaceAll, rfIgnoreCase]);
    StrReplace(Result, '%(RootDir)', '%1:s', [rfReplaceAll, rfIgnoreCase]);
    StrReplace(Result, '%(Filename)', '%2:s', [rfReplaceAll, rfIgnoreCase]);
    StrReplace(Result, '%(Extension)', '%3:s', [rfReplaceAll, rfIgnoreCase]);
    StrReplace(Result, '%(RelativeDir)', '%4:s', [rfReplaceAll, rfIgnoreCase]);
    StrReplace(Result, '%(Directory)', '%5:s', [rfReplaceAll, rfIgnoreCase]);
    StrReplace(Result, '%(RecursiveDir)', '%6:s', [rfReplaceAll, rfIgnoreCase]);
    StrReplace(Result, '%(Identity)', '%7:s', [rfReplaceAll, rfIgnoreCase]);
    StrReplace(Result, '%(ModifiedTime)', '%8:s', [rfReplaceAll, rfIgnoreCase]);
    StrReplace(Result, '%(CreatedTime)', '%9:s', [rfReplaceAll, rfIgnoreCase]);
    StrReplace(Result, '%(AccessedTime)', '%10:s', [rfReplaceAll, rfIgnoreCase]);

    // replace user defined metadata
    Num := WellKnownItemMetadataCount;
    Index := Pos('%(', Result);
    while Index <> 0 do
    begin
      EndIndex := StrSearch(')', Result, Index + 2);
      MetaDataName := Copy(Result, Index + 2, EndIndex - Index - 2);
      UserDefinedMetadataNames.Add(MetaDataName);
      StrReplace(Result, '%(' + MetaDataName + ')', '%' + IntToStr(Num) + ':s', [rfReplaceAll, rfIgnoreCase]);
      Inc(Num);
      Index := StrSearch('%(', Result, Index);
    end;
  end;

  procedure GetTransformParameters(Item: TJclMsBuildItem; var Storage: TDynStringArray;
    var Formats: TVarRecArray);
  const
    DateTimeFormat = 'yyyy-mm-dd hh:nn:ss.zzz';
  var
    Index, DotIdx: Integer;
    ItemFullInclude: string;
    LocalDateTime: TDateTime;
  begin
    if Length(Formats) <> WellKnownItemMetadataCount + UserDefinedMetadataNames.Count then
    begin
      SetLength(Formats, WellKnownItemMetadataCount + UserDefinedMetadataNames.Count);
      for Index := Low(Formats) to High(Formats) do
      begin
        {$IFDEF SUPPORTS_UNICODE}
        Formats[Index].VType := vtPWideChar;
        Formats[Index].VPWideChar := nil;
        {$ELSE ~SUPPORTS_UNICODE}
        Formats[Index].VType := vtPChar;
        Formats[Index].VPChar := nil;
        {$ENDIF ~SUPPORTS_UNICODE}
      end;
    end;

    if Length(Storage) <> WellKnownItemMetadataCount + UserDefinedMetadataNames.Count then
      SetLength(Storage, WellKnownItemMetadataCount + UserDefinedMetadataNames.Count);

    ItemFullInclude := Item.ItemFullInclude;

    // %(FullPath) Contains the full path of the item. For example:
    Storage[0] := ItemFullInclude;

    // %(RootDir) Contains the root directory of the item. For example:
    if PathIsAbsolute(ItemFullInclude) and not PathIsUNC(ItemFullInclude) and
       (ItemFullInclude <> '') and CharIsDriveLetter(ItemFullInclude[1]) then
      Storage[1] := ItemFullInclude[1] + ':\'
    else
      Storage[1] := '';

    // %(Filename) Contains the file name of the item, without the extension. For example:
    Storage[2] := ChangeFileExt(ExtractFileName(Item.ItemInclude), '');

    // %(Extension) Contains the file name extension of the item. For example:
    Storage[3] := ExtractFileExt(Item.ItemInclude);

    // %(RelativeDir) Contains the path specified in the Include attribute, up to the final backslash (\). For example:
    Storage[4] := PathAddSeparator(ExtractFilePath(Item.ItemInclude));

    // %(Directory) Contains the directory of the item, without the root directory. For example:
    Index := CharPos(ItemFullInclude, '\');
    if Index > 0 then
      // skip the root
      Index := CharPos(ItemFullInclude, '\', Index + 1);
    if Index > 0 then
      Storage[5] := Copy(ItemFullInclude, Index + 1, Length(ItemFullInclude) - Index)
    else
      Storage[5] := '';

    // %(RecursiveDir)
    Storage[6] := ''; // TODO: path expansion

    // %(Identity) The item specified in the Include attribute.. For example:
    Storage[7] := Item.ItemInclude;

    // %(ModifiedTime) Contains the timestamp from the last time the item was modified. For example:
    if GetFileLastWrite(ItemFullInclude, LocalDateTime) then
      Storage[8] := FormatDateTime(DateTimeFormat, LocalDateTime)
    else
      Storage[8] := '';

    // %(CreatedTime) Contains the timestamp from when the item was created. For example:
    if GetFileCreation(ItemFullInclude, LocalDateTime) then
      Storage[9] := FormatDateTime(DateTimeFormat, LocalDateTime)
    else
      Storage[9] := '';

    // %(AccessedTime) Contains the timestamp from the last time the time was accessed.
    if GetFileLastAccess(ItemFullInclude, LocalDateTime) then
      Storage[10] := FormatDateTime(DateTimeFormat, LocalDateTime)
    else
      Storage[10] := '';

    for Index := 0 to UserDefinedMetadataNames.Count - 1 do
    begin
      DotIdx := Pos('.', UserDefinedMetadataNames[Index]);
      if DotIdx <> 0 then // references different item => batch
      begin
        Storage[WellKnownItemMetadataCount + Index] := ''; // not implemented yet. Outer loop must iterator over this item
      end
      else
        Storage[WellKnownItemMetadataCount + Index] := Item.ItemMetaData.Values[UserDefinedMetadataNames[Index]];
    end;

    for Index := Low(Formats) to High(Formats) do
      {$IFDEF SUPPORTS_UNICODE}
      Formats[Index].VPWideChar := PChar(Storage[Index]);
      {$ELSE ~SUPPORTS_UNICODE}
      Formats[Index].VPChar := PChar(Storage[Index]);
      {$ENDIF ~SUPPORTS_UNICODE}
  end;

var
  Index: Integer;
  TransformPattern, TransformResult: string;
  TransformParameters: TVarRecArray;
  TransformStorage: TDynStringArray;
begin
  UserDefinedMetadataNames := TStringList.Create;
  try
    TransformPattern := GetTransformPattern(Transform);

    Result := '';
    for Index := 0 to ItemList.Count - 1 do
    begin
      GetTransformParameters(TJclMsBuildItem(ItemList.Objects[Index]), TransformStorage, TransformParameters);
      TransformResult := Format(TransformPattern, TransformParameters);
      if Result <> '' then
        Result := Result + ';' + TransformResult
      else
        Result := TransformResult;
    end;
  finally
    UserDefinedMetadataNames.Free;
  end;
end;

function TJclMsBuildParser.EvaluateFunctionProperty(const Command: string): string;
begin
  if (not Assigned(FOnFunctionProperty) or not FOnFunctionProperty(Self, Command, Result)) and
     (not IgnoreFunctionProperties) then
    raise EJclMsBuildError.CreateResFmt(@RsEFunctionProperty, [Command]);
end;

function TJclMsBuildParser.EvaluateList(const Name: string): string;
var
  Index: Integer;
  Transform: string;
  List: TStrings;
begin
  Index := Pos('->', Name);
  if Index = 0 then
  begin
    // no transformation
    List := TStringList.Create;
    try
      FindItemIncludes(Name, List);
      Result := StringsToStr(List, ';', False);
    finally
      List.Free;
    end;
  end
  else
  begin
    Transform := Copy(Name, Index + 2, Length(Name) - Index - 1);
    Transform := StrTrimCharLeft(StrTrimCharRight(Transform, NativeSingleQuote), NativeSingleQuote);
    List := TStringList.Create;
    try
      FindItemIncludes(Copy(Name, 1, Index - 1), List);
      Result := EvaluateTransform(List, Transform);
    finally
      List.Free;
    end;
  end;
end;

function TJclMsBuildParser.EvaluateRegistryProperty(Root: HKEY; const Path, Name: string): string;
begin
  if (not Assigned(FOnRegistryProperty) or not FOnRegistryProperty(Self, Root, Path, Name, Result)) and
     (not RegReadStringEx(Root, Path, Name, Result, False)) then
    raise EJclMsBuildError.CreateResFmt(@RsERegistryProperty, [RootKeyName(Root), Path, Name]);
end;

function TJclMsBuildParser.FindItemDefinition(
  const ItemName: string): TJclMsBuildItem;
var
  Index: Integer;
begin
  for Index := 0 to FItemDefinitions.Count - 1 do
  begin
    Result := TJclMsBuildItem(FItemDefinitions.Items[Index]);
    if SameItemName(Result.ItemName, ItemName) then
      Exit;
  end;
  Result := nil;
end;

procedure TJclMsBuildParser.FindItemIncludes(const ItemName: string; List: TStrings);
var
  Index: Integer;
  Item: TJclMsBuildItem;
begin
  List.Clear;
  List.BeginUpdate;
  try
    for Index := 0 to FItems.Count - 1 do
    begin
      Item := TJclMsBuildItem(FItems.Items[Index]);
      if SameItemName(Item.ItemName, ItemName) then
        List.AddObject(Item.ItemInclude, Item);
    end;
  finally
    List.EndUpdate;
  end;
end;

function TJclMsBuildParser.FindTarget(const TargetName: string): TJclMsBuildTarget;
var
  Index: Integer;
begin
  for Index := 0 to FTargets.Count - 1 do
  begin
    Result := TJclMsBuildTarget(FTargets.Items[Index]);
    if Result.TargetName = TargetName then
      Exit;
  end;
  Result := nil;
end;

class function TJclMsBuildParser.SameItemName(const ItemName1, ItemName2: string): Boolean;
begin
  Result := SameText(ItemName1, ItemName2);
end;

function TJclMsBuildParser.GetItem(Index: Integer): TJclMsBuildItem;
begin
  Result := TJclMsBuildItem(FItems.Items[Index]);
end;

function TJclMsBuildParser.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclMsBuildParser.GetItemDefinition(
  Index: Integer): TJclMsBuildItem;
begin
  Result := TJclMsBuildItem(FItemDefinitions.Items[Index]);
end;

function TJclMsBuildParser.GetItemDefinitionCount: Integer;
begin
  Result := FItemDefinitions.Count;
end;

function TJclMsBuildParser.GetPropertyValue(const Name: string): string;
begin
  Result := Properties.Values[Name];
end;

function TJclMsBuildParser.GetTarget(Index: Integer): TJclMsBuildTarget;
begin
  Result := TJclMsBuildTarget(FTargets.Items[Index]);
end;

function TJclMsBuildParser.GetTargetCount: Integer;
begin
  Result := FTargets.Count;
end;

function TJclMsBuildParser.GetUsingTask(Index: Integer): TJclMsBuildUsingTask;
begin
  Result := TJclMsBuildUsingTask(FUsingTasks.Items[Index]);
end;

function TJclMsBuildParser.GetUsingTaskCount: Integer;
begin
  Result := FUsingTasks.Count;
end;

procedure TJclMsBuildParser.Init;
begin
  InitReservedProperties;
  InitEnvironmentProperties;
end;

procedure TJclMsBuildParser.InitEnvironmentProperties;
begin
  Properties.EnvironmentProperties.Clear;
  GetEnvironmentVars(Properties.EnvironmentProperties, True);

  // from http://msdn.microsoft.com/en-us/library/ms164309.aspx

  // MSBuildExtensionsPath
  // can be overriden, not used by Embarcadero's project files

  // MSBuildExtensionsPath32
  // can be overriden, not used by Embarcadero's project files

  // MSBuildExtensionsPath64
  // can be overriden, not used by Embarcadero's project files
end;

procedure TJclMsBuildParser.InitReservedProperties;
var
  Index: Integer;
  Path: string;
  DotNetVersions: TStrings;
begin
  Properties.ReservedProperties.Clear;

  // from http://msdn.microsoft.com/en-us/library/ms164309.aspx

  // MSBuildProjectDirectory
  Properties.ReservedProperties.Values['MSBuildProjectDirectory'] := PathRemoveSeparator(ExtractFileDir(ProjectFileName));

  // MSBuildProjectFile
  Properties.ReservedProperties.Values['MSBuildProjectFile'] := ExtractFileName(ProjectFileName);

  // MSBuildProjectExtension
  Properties.ReservedProperties.Values['MSBuildProjectExtension'] := ExtractFileExt(ProjectFileName);

  // MSBuildProjectFullPath
  Properties.ReservedProperties.Values['MSBuildProjectFullPath'] := ProjectFileName;

  // MSBuildProjectName
  Properties.ReservedProperties.Values['MSBuildProjectName'] := ChangeFileExt(ExtractFileName(ProjectFileName), '');

  DotNetVersions := TStringList.Create;
  try
    TJclClrHost.GetClrVersions(DotNetVersions);
    for Index := DotNetVersions.Count - 1 downto 0 do
    begin
      Path := DotNetVersions.Values[DotNetVersions.Names[Index]];
      if not FileExists(PathAddSeparator(Path) + 'MSBuild.exe') then
        DotNetVersions.Delete(Index);
    end;

    if DotNetVersion <> '' then
    begin
      Path := DotNetVersions.Values[DotNetVersion];
      // MSBuildToolsVersion
      Properties.ReservedProperties.Values['MSBuildToolsVersion'] := DotNetVersion;
    end
    else
    if DotNetVersions.Count > 0 then
    begin
      Path := DotNetVersions.Values[DotNetVersions.Names[0]];
      // MSBuildToolsVersion
      Properties.ReservedProperties.Values['MSBuildToolsVersion'] := DotNetVersions.Names[0];
    end
    else
      Path := '';

    if Path <> '' then
    begin
      // MSBuildBinPath
      Properties.ReservedProperties.Values['MSBuildBinPath'] := Path;
      // MSBuildToolsPath
      Properties.ReservedProperties.Values['MSBuildToolsPath'] := Path;
    end
    else
      raise EJclMsBuildError.CreateRes(@RsEMSBuildPath);
  finally
    DotNetVersions.Free;
  end;

  // MSBuildProjectDefaultTargets
  // postponed to the ParseProject

  // MSBuildExtensionsPath
  // in the environment variables

  // MSBuildExtensionsPath32
  // in the environment variables

  // MSBuildExtensionsPath64
  // in the environment variables

  // MSBuildStartupDirectory
  if WorkingDirectory <> '' then
    Path := PathRemoveSeparator(WorkingDirectory)
  else
    Path := PathRemoveSeparator(ExtractFilePath(ProjectFileName));
  Properties.ReservedProperties.Values['MSBuildStartupDirectory'] := Path;

  // MSBuildNodeCount
  Properties.ReservedProperties.Values['MSBuildNodeCount'] := '1';

  // MSBuildLastTaskResult
  Properties.ReservedProperties.Values['MSBuildLastTaskResult'] := 'true';

  // MSBuildOverrideTasksPath
  // supported only in .net 4.0

  // MSBuildProgramFiles32
  Path := GetSpecialFolderLocation(CSIDL_PROGRAM_FILESX86);
  if Path = '' then
    Path := GetSpecialFolderLocation(CSIDL_PROGRAM_FILES);
  Properties.ReservedProperties.Values['MSBuildProgramFiles32'] := Path;

  // MSBuildProjectDirectoryNoRoot
  Path := PathRemoveSeparator(ExtractFilePath(ProjectFileName));
  if PathIsAbsolute(Path) and not PathIsUNC(Path) and (Path <> '') and CharIsDriveLetter(Path[1]) then
    Path := Copy(Path, 3, Length(Path) - 2);
  Properties.ReservedProperties.Values['MSBuildProjectDirectoryNoRoot'] := Path;

  // MSBuildThisFile
  Properties.ReservedProperties.Values['MSBuildThisFile'] := CurrentFileName;

  // MSBuildThisFileDirectory
  Properties.ReservedProperties.Values['MSBuildThisFileDirectory'] := PathRemoveSeparator(ExtractFilePath(CurrentFileName));
end;

procedure TJclMsBuildParser.Parse;
begin
  FFirstPropertyGroup := nil;
  FProjectExtensions := nil;
  FCurrentFileName := FProjectFileName;
  ParseXml(FXml);
end;

procedure TJclMsBuildParser.ParseChoose(XmlElem: TJclSimpleXmlElem);
var
  Index: Integer;
  Prop: TJclSimpleXmlProp;
  SubElem: TJclSimpleXmlElem;
  Executed, _Otherwise: Boolean;
begin
  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  Executed := False;
  _Otherwise := False;
  
  for Index := 0 to XmlElem.ItemCount - 1 do
  begin
    SubElem := XmlElem.Items.Item[Index];
    if SubElem.Name = 'When' then
      Executed := ParseWhen(SubElem, Executed)
    else
    if SubElem.Name = 'Otherwise' then
    begin
      if _Otherwise then
        raise EJclMsBuildError.CreateRes(@RsEMultipleOtherwise);
      _Otherwise := True;
      Executed := ParseOtherwise(SubElem, Executed);
    end
    else
    if not (SubElem is TJclSimpleXMLElemComment) then
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownElement, [SubElem.Name]);
  end;
end;

function TJclMsBuildParser.ParseCondition(const Condition: string): Boolean;
var
  Position, Len: Integer;
begin
  Len := Length(Condition);
  Position := 1;
  Result := ParseConditionLength(Condition, Position, Len);
end;

function TJclMsBuildParser.ParseConditionLength(const Condition: string; var Position: Integer; Len: Integer): Boolean;
type
  TOperator = (opUnknown, opAnd, opOr);
var
  LeftOperand, RightOperand: Boolean;
  MiddleOperator: TOperator;
begin
  Result := True;
  if Condition <> '' then
  begin
    // read first word
    LeftOperand := ParseConditionOperand(Condition, Position, Len);
    if (Position <= Len) and (Condition[Position] = '(') then
    begin
      // skip opening parenthesis
      Inc(Position);
      LeftOperand := ParseConditionLength(Condition, Position, Len);
      while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
        Inc(Position);
      if Condition[Position] <> ')' then
        raise EJclMsBuildError.CreateResFmt(@RsEMissingParenthesis, [Condition]);
      // skip closing parenthesis
      Inc(Position);
    end;
    while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
      Inc(Position);
    while True do
    begin
      // read infix operator
      MiddleOperator := opUnknown;
      if ((Position + 2) <= Len) and (MiddleOperator = opUnknown) then
      begin
        if ((Condition[Position] = 'A') or (Condition[Position] = 'a')) and
           ((Condition[Position + 1] = 'N') or (Condition[Position + 1] = 'n')) and
           ((Condition[Position + 2] = 'D') or (Condition[Position + 2] = 'd'))  then
          MiddleOperator := opAnd;
        if MiddleOperator <> opUnknown then
          Inc(Position, 3);
      end;
      if ((Position + 1) <= Len) and (MiddleOperator = opUnknown) then
      begin
        if ((Condition[Position] = 'O') or (Condition[Position] = 'o')) and
           ((Condition[Position + 1] = 'R') or (Condition[Position + 1] = 'r')) then
          MiddleOperator := opOr;
        if MiddleOperator <> opUnknown then
          Inc(Position, 2);
      end;
      if MiddleOperator <> opUnknown then
      begin
        // read right operand if any
        RightOperand := ParseConditionOperand(Condition, Position, Len);
        if (Position <= Len) and (Condition[Position] = '(') then
        begin
          // skip opening parenthesis
          Inc(Position);
          RightOperand := ParseConditionLength(Condition, Position, Len);
          while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
            Inc(Position);
          if Condition[Position] <> ')' then
            raise EJclMsBuildError.CreateResFmt(@RsEMissingParenthesis, [Condition]);
          // skip closing parenthesis
          Inc(Position);
        end;
        while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
          Inc(Position);

        case MiddleOperator of
          opUnknown:
            raise EJclMsBuildError.CreateResFmt(@RsEUnknownOperator, [Condition]);
          opAnd:
            LeftOperand := LeftOperand and RightOperand;
          opOr:
            LeftOperand := LeftOperand or RightOperand;
        end;
      end
      else
        Break;
    end;
    // no second word
    Result := LeftOperand
  end;
end;

function TJclMsBuildParser.ParseConditionOperand(const Condition: string; var Position: Integer; Len: Integer): Boolean;
type
  TOperator = (opUnknown, opEqual, opNotEqual, opLess, opLessOrEqual, opGreater, OpGreaterOrEqual);
var
  LeftString, RightString: string;
  MiddleOperator: TOperator;
begin
  Result := True;
  if Condition <> '' then
  begin
    // read first word
    LeftString := ParseConditionString(Condition, Position, Len);
    if (LeftString = '') and (Position <= Len) and (Condition[Position] = '(') then
    begin
      // skip opening parenthesis
      Inc(Position);
      LeftString := BoolToStr(ParseConditionLength(Condition, Position, Len), True);
      while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
        Inc(Position);
      if Condition[Position] <> ')' then
        raise EJclMsBuildError.CreateResFmt(@RsEMissingParenthesis, [Condition]);
      // skip closing parenthesis
      Inc(Position);
    end;
    while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
      Inc(Position);
    // read infix operator
    MiddleOperator := opUnknown;
    if ((Position + 1) <= Len) and (MiddleOperator = opUnknown) then
    begin
      if (Condition[Position] = '=') and (Condition[Position + 1] = '=') then
        MiddleOperator := opEqual
      else
      if (Condition[Position] = '!') and (Condition[Position + 1] = '=') then
        MiddleOperator := opNotEqual
      else
      if (Condition[Position] = '<') and (Condition[Position + 1] = '=') then
        MiddleOperator := opLessOrEqual
      else
      if (Condition[Position] = '>') and (Condition[Position + 1] = '=') then
        MiddleOperator := OpGreaterOrEqual;
      if MiddleOperator <> opUnknown then
        Inc(Position, 2);
    end;
    if (Position <= Len) and (MiddleOperator = opUnknown) then
    begin
      if Condition[Position] = '<' then
        MiddleOperator := opLess
      else
      if Condition[Position] = '>' then
        MiddleOperator := opGreater;
      if MiddleOperator <> opUnknown then
        Inc(Position);
    end;
    if MiddleOperator <> opUnknown then
    begin
      // read right operand if needed
      RightString := ParseConditionString(Condition, Position, Len);
      if (RightString = '') and (Position <= Len) and (Condition[Position] = '(') then
      begin
        // skip opening parenthesis
        Inc(Position);
        RightString := BoolToStr(ParseConditionLength(Condition, Position, Len), True);
        while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
          Inc(Position);
        if Condition[Position] <> ')' then
          raise EJclMsBuildError.CreateResFmt(@RsEMissingParenthesis, [Condition]);
        // skip closing parenthesis
        Inc(Position);
      end;
      while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
        Inc(Position);

      case MiddleOperator of
        opUnknown:
          raise EJclMsBuildError.CreateResFmt(@RsEUnknownOperator, [Condition]);
        opEqual:
          Result := LeftString = RightString;
        opNotEqual:
          Result := LeftString <> RightString;
        opLess:
          Result := StrToInt64(LeftString) < StrToInt64(RightString);
        opLessOrEqual:
          Result := StrToInt64(LeftString) <= StrToInt64(RightString);
        opGreater:
          Result := StrToInt64(LeftString) > StrToInt64(RightString);
        OpGreaterOrEqual:
          Result := StrToInt64(LeftString) >= StrToInt64(RightString);
      end;
    end
    else
    if LeftString = '' then
      Result := True
    else
      // no second word
      Result := StrToBool(LeftString)
  end;
end;

function TJclMsBuildParser.ParseConditionString(const Condition: string; var Position: Integer; Len: Integer): string;
var
  StartPos, EndPos: Integer;
  HasQuote: Boolean;
  FileOrDirectory: string;
begin
  // skip heading spaces
  while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
    Inc(Position);
  StartPos := Position;
  HasQuote := Condition[Position] = NativeSingleQuote;
  if HasQuote then
  begin
    // skip heading quote
    Inc(StartPos);
    Inc(Position);
    // quoted string
    while (Position <= Len) and (Condition[Position] <> NativeSingleQuote) do
      Inc(Position);
    if Position > Len then
      raise EJclMsBuildError.CreateResFmt(@RsEEndOfString, [Condition]);
    EndPos := Position;
    // skip closing quote
    Inc(Position);
  end
  else
  begin
    // alphanumeric strings do not need to be quoted
    while (Position <= Len) and CharIsValidIdentifierLetter(Condition[Position]) do
      Inc(Position);
    EndPos := Position;
  end;
  Result := Copy(Condition, StartPos, EndPos - StartPos);

  // evaluate builtin operators and functions
  if (Result = '') and (Condition[StartPos] = '!') and not HasQuote then
  begin
    Inc(Position);
    Result := BoolToStr(not ParseConditionLength(Condition, Position, Len));
  end
  else
  if (CompareText(Result, 'Exists') = 0) and not HasQuote then
  begin
    while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
      Inc(Position);
    // skip opening parenthesis
    if Condition[Position] <> '(' then
      raise EJclMsBuildError.CreateResFmt(@RsEMissingParenthesis, [Condition]);
    Inc(Position);
    FileOrDirectory := ParseConditionString(Condition, Position, Len);
    Result := BoolToStr(FileExists(FileOrDirectory) or DirectoryExists(FileOrDirectory), True);
    while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
      Inc(Position);
    // skip inner $(xxxx) inside an outer Exists( )  - Necessary, starting with XE8.
    if (Position <= Len) and (Condition[Position] = '$') then
       Inc(Position);
    if (Position <= Len) and (Condition[Position] = '(') then
      while (Position <= Len) do
      begin
        Inc(Position);
        if Condition[Position-1] = ')' then
          Break;
      end;
    // skip closing parenthesis
    if Condition[Position] <> ')' then
      raise EJclMsBuildError.CreateResFmt(@RsEMissingParenthesis, [Condition]);
    Inc(Position);
  end
  else
  if (CompareText(Result, 'HasTrailingSlash') = 0) and not HasQuote then
  begin
    while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
      Inc(Position);
    // skip opening parenthesis
    if Condition[Position] <> '(' then
      raise EJclMsBuildError.CreateResFmt(@RsEMissingParenthesis, [Condition]);
    Inc(Position);
    FileOrDirectory := ParseConditionString(Condition, Position, Len);
    Result := BoolToStr((FileOrDirectory <> '') and (FileOrDirectory[Length(FileOrDirectory)] = PathDelim), True);
    while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
      Inc(Position);
    // skip closing parenthesis
    if Condition[Position] <> ')' then
      raise EJclMsBuildError.CreateResFmt(@RsEMissingParenthesis, [Condition]);
    Inc(Position);
  end
  else
    Result := EvaluateString(Result);
  // skip tailing spaces
  while (Position <= Len) and CharIsWhiteSpace(Condition[Position]) do
    Inc(Position);
end;

procedure TJclMsBuildParser.ParseImport(XmlElem: TJclSimpleXmlElem);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem, OldProjectExtensions: TJclSimpleXmlElem;
  Condition: Boolean;
  Project: TFileName;
  SubXml: TJclSimpleXml;
  SubOwnsXml: Boolean;
  OldCurrentFileName: TFileName;
begin
  Condition := True;

  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Condition' then
      Condition := ParseCondition(Prop.Value)
    else
    if Prop.Name = 'Project' then
      Project := TFileName(EvaluateString(Prop.Value))
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  for Index := 0 to XmlElem.ItemCount - 1 do
  begin
    SubElem := XmlElem.Items.Item[Index];
    if not (SubElem is TJclSimpleXMLElemComment) then
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownElement, [SubElem.Name]);
  end;

  if Condition then
  begin
    SubXml := nil;
    SubOwnsXml := False;

    if not PathIsAbsolute(Project) then
      Project := PathCanonicalize(PathGetRelativePath(ExtractFilePath(CurrentFileName), Project));

    if Assigned(FOnImport) then
      FOnImport(Self, Project, SubXml, SubOwnsXml);

    if (Project <> '') or (SubXml <> nil) then // abort if both are not assigned
    try
      if not Assigned(SubXml) then
      begin
        SubXml := TJclSimpleXML.Create;
        SubXml.Options := SubXml.Options - [sxoAutoEncodeValue,sxoAutoEncodeEntity];
        SubXml.OnEncodeValue := XMLEncodeValue;
        SubXml.OnDecodeValue := XMLDecodeValue;
        SubXml.LoadFromFile(Project);
        SubOwnsXml := True;
      end;

      OldCurrentFileName := CurrentFileName;
      OldProjectExtensions := ProjectExtensions;
      try
        FCurrentFileName := Project;
        FProjectExtensions := nil;
        InitReservedProperties;
        ParseXml(SubXml);
      finally
        FCurrentFileName := OldCurrentFileName;
        FProjectExtensions := OldProjectExtensions;
        InitReservedProperties;
      end;

    finally
      if SubOwnsXml then
        SubXml.Free;
    end;
    
  end;
end;

procedure TJclMsBuildParser.ParseImportGroup(XmlElem: TJclSimpleXmlElem);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem: TJclSimpleXmlElem;
  Condition: Boolean;
begin
  Condition := True;

  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Condition' then
      Condition := ParseCondition(Prop.Value)
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  for Index := 0 to XmlElem.ItemCount - 1 do
  begin
    SubElem := XmlElem.Items.Item[Index];
    if SubElem.Name = 'Import' then
    begin
      if Condition then
        ParseImport(SubElem);
    end
    else
    if not (SubElem is TJclSimpleXMLElemComment) then
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownElement, [SubElem.Name]);
  end;
end;

procedure TJclMsBuildParser.ParseItem(XmlElem: TJclSimpleXmlElem; Definition: Boolean);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem: TJclSimpleXmlElem;
  Condition: Boolean;
  Item: TJclMsBuildItem;
  ItemName, ItemExclude, ItemInclude, ItemRemove: string;
begin
  Condition := True;

  ItemName := XmlElem.Name;

  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Condition' then
      Condition := ParseCondition(Prop.Value)
    else
    if Prop.Name = 'Exclude' then
      ItemExclude := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'Include' then
      ItemInclude := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'Remove' then
      ItemRemove := EvaluateString(Prop.Value)
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  if Condition then
  begin
    Item := TJclMsBuildItem.Create;
    Item.FItemName := ItemName;
    Item.FItemInclude := ItemInclude;
    Item.FItemExclude := ItemExclude;
    Item.FItemRemove := ItemRemove;

    if PathIsAbsolute(ItemInclude) then
      Item.FItemFullInclude := ItemInclude
    else
    if PathIsAbsolute(CurrentFileName) then
      Item.FItemFullInclude := PathCanonicalize(PathGetRelativePath(ExtractFilePath(CurrentFileName), ItemInclude))
    else
      Item.FItemFullInclude := PathCanonicalize(PathGetRelativePath(WorkingDirectory, ItemInclude));
    if not FileExists(Item.FItemFullInclude) then
      Item.FItemFullInclude := Item.FItemInclude;

    if Definition then
      FItemDefinitions.Add(Item)
    else
      FItems.Add(Item);
      
    for Index := 0 to XmlElem.ItemCount - 1 do
    begin
      SubElem := XmlElem.Items.Item[Index];
      ParseItemMetaData(SubElem, Item.ItemMetaData);
    end;
  end;
end;

procedure TJclMsBuildParser.ParseItemDefinitionGroup(XmlElem: TJclSimpleXmlElem);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem: TJclSimpleXmlElem;
  Condition: Boolean;
begin
  Condition := True;

  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Condition' then
      Condition := ParseCondition(Prop.Value)
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  if Condition then
    for Index := 0 to XmlElem.ItemCount - 1 do
    begin
      SubElem := XmlElem.Items.Item[Index];
      ParseItem(SubElem, True);
    end;
end;

procedure TJclMsBuildParser.ParseItemGroup(XmlElem: TJclSimpleXmlElem);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem: TJclSimpleXmlElem;
  Condition: Boolean;
begin
  Condition := True;

  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Condition' then
      Condition := ParseCondition(Prop.Value)
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  if Condition then
    for Index := 0 to XmlElem.ItemCount - 1 do
    begin
      SubElem := XmlElem.Items.Item[Index];
      ParseItem(SubElem, False);
    end;
end;

procedure TJclMsBuildParser.ParseItemMetaData(XmlElem: TJclSimpleXmlElem; ItemMetaData: TStrings);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  Condition: Boolean;
begin
  Condition := True;

  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Condition' then
      Condition := ParseCondition(Prop.Value)
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  if Condition then
    ItemMetaData.Values[XmlElem.Name] := EvaluateString(XmlElem.Value);
end;

procedure TJclMsBuildParser.ParseOnError(XmlElem: TJclSimpleXMLElem; Target: TJclMsBuildTarget);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem: TJclSimpleXmlElem;
  Condition: Boolean;
  ExecuteTargets: string;
  TempStrings: TStrings;
begin
  Condition := True;

  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Condition' then
      Condition := ParseCondition(Prop.Value)
    else
    if Prop.Name = 'ExecuteTargets' then
      ExecuteTargets := EvaluateString(Prop.Value)
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  for Index := 0 to XmlElem.ItemCount - 1 do
  begin
    SubElem := XmlElem.Items.Item[Index];
    if not (SubElem is TJclSimpleXMLElemComment) then
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownElement, [SubElem.Name]);
  end;

  if Condition then
  begin
    TempStrings := TStringList.Create;
    try
      StrToStrings(ExecuteTargets, ';', TempStrings, False);
      Target.ErrorTargets.AddStrings(TempStrings);
    finally
      TempStrings.Free;
    end;
  end;
end;

function TJclMsBuildParser.ParseOtherwise(XmlElem: TJclSimpleXmlElem; Skip: Boolean): Boolean;
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem: TJclSimpleXmlElem;
begin
  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  Result := not Skip;

  for Index := 0 to XmlElem.ItemCount - 1 do
  begin
    SubElem := XmlElem.Items.Item[Index];
    if SubElem.Name = 'Choose' then
    begin
      if Result then
        ParseChoose(SubElem);
    end
    else
    if SubElem.Name = 'ItemGroup' then
    begin
      if Result then
        ParseItemGroup(SubElem);
    end
    else
    if SubElem.Name = 'PropertyGroup' then
    begin
      if Result then
        ParsePropertyGroup(SubElem);
    end
    else
    if not (SubElem is TJclSimpleXMLElemComment) then
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownElement, [SubElem.Name]);
  end;
end;

procedure TJclMsBuildParser.ParseOutput(XmlElem: TJclSimpleXMLElem; Task: TJclMsBuildTask);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem: TJclSimpleXmlElem;
  Condition: Boolean;
  TaskParameter, PropertyName, ItemName: string;
  TaskOutput: TJclMsBuildTaskOutput;
begin
  Condition := True;

  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Condition' then
      Condition := ParseCondition(Prop.Value)
    else
    if Prop.Name = 'TaskParameter' then
      TaskParameter := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'PropertyName' then
      PropertyName := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'ItemName' then
      ItemName := EvaluateString(Prop.Value)
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  for Index := 0 to XmlElem.ItemCount - 1 do
  begin
    SubElem := XmlElem.Items.Item[Index];
    if not (SubElem is TJclSimpleXMLElemComment) then
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownElement, [SubElem.Name]);
  end;

  if TaskParameter = '' then
    raise EJclMsBuildError.CreateRes(@RsEMissingTaskParameter);

  if (PropertyName = '') and (ItemName = '') then
    raise EJclMsBuildError.CreateRes(@RsEMissingOutputName);

  if Condition then
  begin
    TaskOutput := TJclMsBuildTaskOutput.Create;
    TaskOutput.FTaskParameter := TaskParameter;
    TaskOutput.FPropertyName := PropertyName;
    TaskOutput.FItemName := ItemName;
    Task.AddOutput(TaskOutput);
  end;
end;

procedure TJclMsBuildParser.ParseParameter(XmlElem: TJclSimpleXMLElem; UsingTask: TJclMsBuildUsingTask);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem: TJclSimpleXmlElem;
  ParameterName, ParameterType: string;
  Output, Required: Boolean;
  Parameter: TJclMsBuildParameter;
begin
  Output := False;
  Required := False;

  ParameterName := XmlElem.Name;

  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'ParameterType' then
      ParameterType := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'Output' then
      Output := Prop.BoolValue
    else
    if Prop.Name = 'Required' then
      Required := Prop.BoolValue
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  for Index := 0 to XmlElem.ItemCount - 1 do
  begin
    SubElem := XmlElem.Items.Item[Index];
    if not (SubElem is TJclSimpleXMLElemComment) then
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownElement, [SubElem.Name]);
  end;

  Parameter := TJclMsBuildParameter.Create;
  Parameter.FParameterName := ParameterName;
  Parameter.FParameterType := ParameterType;
  Parameter.FOutput := Output;
  Parameter.FRequired := Required;
  UsingTask.AddParameter(Parameter);
end;

procedure TJclMsBuildParser.ParseParameterGroup(XmlElem: TJclSimpleXMLElem; UsingTask: TJclMsBuildUsingTask);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem: TJclSimpleXmlElem;
begin
  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  for Index := 0 to XmlElem.ItemCount - 1 do
  begin
    SubElem := XmlElem.Items.Item[Index];
    ParseParameter(SubElem, UsingTask);
  end;
end;

procedure TJclMsBuildParser.ParseProject(XmlElem: TJclSimpleXmlElem);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem: TJclSimpleXmlElem;
  S: string;
begin
  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'InitialTargets' then
      StrToStrings(EvaluateString(Prop.Value), ';', FInitialTargets, False)
    else
    if Prop.Name = 'DefaultTargets' then
    begin
      S := EvaluateString(Prop.Value);
      Properties.ReservedProperties.Values['MSBuildProjectDefaultTargets'] := S;
      StrToStrings(S, ';', FDefaultTargets, False)
    end
    else
    if Prop.Name = 'ToolsVersion' then
    begin
      S := EvaluateString(Prop.Value);
      if Assigned(FOnToolsVersion) then
        FOnToolsVersion(Self, S);
      FToolsVersion := S;
    end
    else
    if Prop.Name = 'xmlns' then
    begin
      if Prop.Value <> 'http://schemas.microsoft.com/developer/msbuild/2003' then
        raise EJclMsBuildError.CreateResFmt(@RsEUnknownSchema, [Prop.Value]);
    end
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;
  
  for Index := 0 to XmlElem.ItemCount - 1 do
  begin
    SubElem := XmlElem.Items.Item[Index];
    if SubElem.Name = 'Choose' then
      ParseChoose(SubElem)
    else
    if SubElem.Name = 'Import' then
      ParseImport(SubElem)
    else
    if SubElem.Name = 'ImportGroup' then
      ParseImportGroup(SubElem)
    else
    if SubElem.Name = 'ItemDefinitionGroup' then
      ParseItemDefinitionGroup(SubElem)
    else
    if SubElem.Name = 'ItemGroup' then
      ParseItemGroup(SubElem)
    else
    if SubElem.Name = 'ProjectExtensions' then
    begin
      if Assigned(FProjectExtensions) then
        raise EJclMsBuildError.CreateRes(@RsEMultipleProjectExtensions);
      FProjectExtensions := SubElem;
    end
    else
    if SubElem.Name = 'PropertyGroup' then
    begin
      if (CurrentFileName = ProjectFileName) and not Assigned(FFirstPropertyGroup) then
        FFirstPropertyGroup := SubElem;
      ParsePropertyGroup(SubElem)
    end
    else
    if SubElem.Name = 'Target' then
      ParseTarget(SubElem)
    else
    if SubElem.Name = 'UsingTask' then
      ParseUsingTask(SubElem)
    else
    if not (SubElem is TJclSimpleXMLElemComment) then
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownElement, [SubElem.Name]);
  end;
end;

procedure TJclMsBuildParser.ParseProperty(XmlElem: TJclSimpleXmlElem);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem: TJclSimpleXmlElem;
  Condition: Boolean;
begin
  Condition := True;

  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Condition' then
      Condition := ParseCondition(Prop.Value)
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  for Index := 0 to XmlElem.ItemCount - 1 do
  begin
    SubElem := XmlElem.Items.Item[Index];
    if not (SubElem is TJclSimpleXMLElemComment) then
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownElement, [SubElem.Name]);
  end;

  if Condition then
  begin
    SetPropertyValue(XmlElem.Name, EvaluateString(XmlElem.Value));
    // store the XML element for further modifications in the current file
    if CurrentFileName = ProjectFileName then
    begin
      Index := Properties.IndexOfName(XmlElem.Name);
      if Index >= 0 then
        Properties.Objects[Index] := XmlElem;
    end;
  end;
end;

procedure TJclMsBuildParser.ParsePropertyGroup(XmlElem: TJclSimpleXmlElem);
var
  Index: Integer;
  Prop: TJclSimpleXmlProp;
  SubElem: TJclSimpleXmlElem;
  Condition: Boolean;
begin
  Condition := True;

  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Condition' then
      Condition := ParseCondition(Prop.Value)
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  if Condition then
    for Index := 0 to XmlElem.ItemCount - 1 do
  begin
    SubElem := XmlElem.Items.Item[Index];
    ParseProperty(SubElem);
  end;
end;

procedure TJclMsBuildParser.ParseTarget(XmlElem: TJclSimpleXMLElem);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem: TJclSimpleXmlElem;
  Condition, KeepDuplicateOutput: Boolean;
  TargetName, Depends, Returns, Inputs, Outputs, BeforeTargets, AfterTargets: string;
  Target: TJclMsBuildTarget;
begin
  Condition := True;
  KeepDuplicateOutput := False;

  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Condition' then
      Condition := ParseCondition(Prop.Value)
    else
    if Prop.Name = 'Name' then
      TargetName := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'DependsOnTargets' then
      Depends := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'Returns' then
      Returns := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'Inputs' then
      Inputs := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'Outputs' then
      Outputs := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'BeforeTargets' then
      BeforeTargets := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'AfterTargets' then
      AfterTargets := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'KeepDuplicateOutputs' then
      KeepDuplicateOutput := Prop.BoolValue
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  if TargetName = '' then
    raise EJclMsBuildError.CreateRes(@RsEMissingTargetName);

  Target := TJclMsBuildTarget.Create;
  FTargets.Add(Target);

  Target.FTargetName := TargetName;
  StrToStrings(Depends, ';', Target.FDepends, False);
  StrToStrings(Returns, ';', Target.FReturns, False);
  StrToStrings(Inputs, ';', Target.FInputs, False);
  StrToStrings(Outputs, ';', Target.FOutputs, False);
  StrToStrings(BeforeTargets, ';', Target.FBeforeTargets, False);
  StrToStrings(AfterTargets, ';', Target.FAfterTargets, False);
  Target.FKeepDuplicateOutputs := KeepDuplicateOutput;

  if Condition then
    for Index := 0 to XmlElem.ItemCount - 1 do
  begin
    SubElem := XmlElem.Items.Item[Index];
    if SubElem.Name = 'OnError' then
      ParseOnError(SubElem, Target)
    else
    if (SubElem.Name = 'PropertyGroup') or (SubElem.Name = 'ItemGroup') then
      // apparently targets can contain several other nodes
      // see comments in http://msdn.microsoft.com/en-us/library/t50z2hka.aspx
      // they should be ignored during the static analysis implemented in this parser
    else
      ParseTask(SubElem, Target);
  end;
end;

procedure TJclMsBuildParser.ParseTask(XmlElem: TJclSimpleXMLElem; Target: TJclMsBuildTarget);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem: TJclSimpleXmlElem;
  TaskName: string;
  Condition, ContinueOnError: Boolean;
  Task: TJclMsBuildTask;
begin
  Condition := True;
  ContinueOnError := False;

  TaskName := XmlElem.Name;
  
  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Condition' then
      Condition := ParseCondition(Prop.Value)
    else
    if Prop.Name = 'ContinueOnError' then
      ContinueOnError := Prop.BoolValue;
  end;

  if Condition then
  begin
    Task := TJclMsBuildTask.Create;
    Task.FTaskName := TaskName;
    Task.FContinueOnError := ContinueOnError;
    Target.AddTask(Task);

    for Index := 0 to XmlElem.PropertyCount - 1 do
    begin
      Prop := XmlElem.Properties.Item[Index];
      if (Prop.Name <> 'Condition') and (Prop.Name <> 'ContinueOnError') then
        Task.Parameters.Values[Prop.Name] := EvaluateString(Prop.Value);
    end;

    for Index := 0 to XmlElem.ItemCount - 1 do
    begin
      SubElem := XmlElem.Items.Item[Index];
      if SubElem.Name = 'Output' then
        ParseOutput(SubElem, Task)
      else
      if not (SubElem is TJclSimpleXMLElemComment) then
        raise EJclMsBuildError.CreateResFmt(@RsEUnknownElement, [SubElem.Name]);
    end;

  end;
end;

procedure TJclMsBuildParser.ParseTaskBody(XmlElem: TJclSimpleXMLElem; UsingTask: TJclMsBuildUsingTask);
var
  Index: Integer;
  Prop: TJclSimpleXMLProp;
  SubElem: TJclSimpleXmlElem;
  Evaluate: Boolean;
begin
  Evaluate := False;
  
  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Evaluate' then
      Evaluate := Prop.BoolValue
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  for Index := 0 to XmlElem.ItemCount - 1 do
  begin
    SubElem := XmlElem.Items.Item[Index];
    if not (SubElem is TJclSimpleXMLElemComment) then
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownElement, [SubElem.Name]);
  end;

  if Evaluate then
    UsingTask.FTaskBody := EvaluateString(XmlElem.Value)
  else
    UsingTask.FTaskBody := XmlElem.Value;
end;

procedure TJclMsBuildParser.ParseUsingTask(XmlElem: TJclSimpleXmlElem);
var
  Index: Integer;
  Prop: TJclSimpleXmlProp;
  SubElem: TJclSimpleXmlElem;
  Condition: Boolean;
  AssemblyName, AssemblyFile, TaskFactory, TaskName: string;
  UsingTask: TJclMsBuildUsingTask;
begin
  Condition := True;

  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Condition' then
      Condition := ParseCondition(Prop.Value)
    else
    if Prop.Name = 'TaskName' then
      TaskName := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'TaskFactory' then
      TaskFactory := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'AssemblyName' then
      AssemblyName := EvaluateString(Prop.Value)
    else
    if Prop.Name = 'AssemblyFile' then
      AssemblyFile := EvaluateString(Prop.Value)
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  if TaskName = '' then
    raise EJclMsBuildError.CreateRes(@RsEMissingTaskName);

  if (AssemblyName = '') and (AssemblyFile = '') then
    raise EJclMsBuildError.CreateRes(@RsEMissingAssembly);

  if Condition then
  begin
    UsingTask := TJclMsBuildUsingTask.Create;
    UsingTask.FAssemblyName := AssemblyName;
    UsingTask.FAssemblyFile := AssemblyFile;
    UsingTask.FTaskFactory := TaskFactory;
    UsingTask.FTaskName := TaskName;
    for Index := 0 to XmlElem.ItemCount - 1 do
    begin
      SubElem := XmlElem.Items.Item[Index];
      if SubElem.Name = 'ParameterGroup' then
        ParseParameterGroup(SubElem, UsingTask)
      else
      if SubElem.Name = 'TaskBody' then
        ParseTaskBody(SubElem, UsingTask)
      else
      if not (SubElem is TJclSimpleXMLElemComment) then
        raise EJclMsBuildError.CreateResFmt(@RsEUnknownElement, [SubElem.Name]);
    end;
    FUsingTasks.Add(UsingTask);
  end;
end;

function TJclMsBuildParser.ParseWhen(XmlElem: TJclSimpleXmlElem; Skip: Boolean): Boolean;
var
  Index: Integer;
  Prop: TJclSimpleXmlProp;
  SubElem: TJclSimpleXmlElem;
begin
  Result := False;

  for Index := 0 to XmlElem.PropertyCount - 1 do
  begin
    Prop := XmlElem.Properties.Item[Index];
    if Prop.Name = 'Condition' then
      Result := Skip or ParseCondition(Prop.Value)
    else
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownProperty, [Prop.Name]);
  end;

  if XmlElem.PropertyCount <> 1 then
    raise EJclMsBuildError.CreateRes(@RsEConditionNotUnique);

  for Index := 0 to XmlElem.ItemCount - 1 do
  begin
    SubElem := XmlElem.Items.Item[Index];
    if SubElem.Name = 'Choose' then
    begin
      if Result then
        ParseChoose(SubElem);
    end
    else
    if SubElem.Name = 'ItemGroup' then
    begin
      if Result then
        ParseItemGroup(SubElem);
    end
    else
    if SubElem.Name = 'PropertyGroup' then
    begin
      if Result then
        ParsePropertyGroup(SubElem);
    end
    else
    if not (SubElem is TJclSimpleXMLElemComment) then
      raise EJclMsBuildError.CreateResFmt(@RsEUnknownElement, [SubElem.Name]);
  end;
end;

procedure TJclMsBuildParser.ParseXml(AXml: TJclSimpleXML);
begin
  if AXml.Root.Name <> 'Project' then
    raise EJclMsBuildError.CreateResFmt(@RsENoProjectElem, [AXml.Root.Name]);
  ParseProject(AXml.Root);
end;

procedure TJclMsBuildParser.Save;
begin
  Xml.SaveToFile(ProjectFileName);
end;

procedure TJclMsBuildParser.SetPropertyValue(const Name, Value: string);
begin
  Properties.Values[Name] := Value;
end;

procedure TJclMsBuildParser.XMLDecodeValue(Sender: TObject; var Value: string);
begin
  Value := XMLDecode(Value);
end;

procedure TJclMsBuildParser.XMLEncodeValue(Sender: TObject; var Value: string);
begin
  StrReplace(Value, '&', '&amp;', [rfReplaceAll]);
  StrReplace(Value, '<', '&lt;', [rfReplaceAll]);
  StrReplace(Value, '>', '&gt;', [rfReplaceAll]);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
