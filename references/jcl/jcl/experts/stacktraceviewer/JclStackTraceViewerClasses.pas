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
{ The Original Code is JclStackTraceViewerClasses.pas.                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Uwe Schuster.                                      }
{ Portions created by Uwe Schuster are Copyright (C) 2009 Uwe Schuster. All rights reserved.       }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStackTraceViewerClasses;

{$I jcl.inc}

interface

uses
  Windows, Classes, Contnrs,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclDebug, JclDebugSerialization, JclStackTraceViewerAPI;

type
  TJclStackTraceViewerLocationInfo = class(TJclLocationInfoEx, IJclLocationInfo, IJclPreparedLocationInfo)
  private
    FFoundFile: Boolean;
    FFileName: string;
    FProjectName: string;
    FRevision: string;
    FTranslatedLineNumber: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IJclLocationInfo }
    function GetAddress: Pointer;
    function GetBinaryFileName: string;
    function GetLineNumber: Integer;
    function GetLineNumberOffsetFromProcedureStart: Integer;
    function GetModuleName: string;
    function GetOffsetFromLineNumber: Integer;
    function GetOffsetFromProcName: Integer;
    function GetProcedureName: string;
    function GetSourceName: string;
    function GetSourceUnitName: string;
    function GetUnitVersionDateTime: TDateTime;
    function GetUnitVersionExtra: string;
    function GetUnitVersionLogPath: string;
    function GetUnitVersionRCSfile: string;
    function GetUnitVersionRevision: string;
    function GetVAddress: Pointer;
    function GetValues: Integer;
    property Address: Pointer read GetAddress;
    property BinaryFileName: string read GetBinaryFileName;
    property LineNumber: Integer read GetLineNumber;
    property LineNumberOffsetFromProcedureStart: Integer read GetLineNumberOffsetFromProcedureStart;
    property ModuleName: string read GetModuleName;
    property OffsetFromLineNumber: Integer read GetOffsetFromLineNumber;
    property OffsetFromProcName: Integer read GetOffsetFromProcName;
    property ProcedureName: string read GetProcedureName;
    property SourceName: string read GetSourceName;
    property SourceUnitName: string read GetSourceUnitName;
    property UnitVersionDateTime: TDateTime read GetUnitVersionDateTime;
    property UnitVersionExtra: string read GetUnitVersionExtra;
    property UnitVersionLogPath: string read GetUnitVersionLogPath;
    property UnitVersionRCSfile: string read GetUnitVersionRCSfile;
    property UnitVersionRevision: string read GetUnitVersionRevision;
    property VAddress: Pointer read GetVAddress;
    property Values: Integer read GetValues;
    { IJclPreparedLocationInfo }
    function GetFileName: string;
    function GetFoundFile: Boolean;
    function GetProjectName: string;
    function GetRevision: string;
    function GetTranslatedLineNumber: Integer;
    procedure SetFileName(AValue: string);
    procedure SetFoundFile(AValue: Boolean);
    procedure SetProjectName(AValue: string);
    procedure SetRevision(AValue: string);
    procedure SetTranslatedLineNumber(AValue: Integer);
    property FileName: string read FFileName write FFileName;
    property FoundFile: Boolean read FFoundFile write FFoundFile;
    property ProjectName: string read FProjectName write FProjectName;
    property Revision: string read FRevision write FRevision;
    property TranslatedLineNumber: Integer read FTranslatedLineNumber write FTranslatedLineNumber;
  end;

  TJclStackTraceViewerLocationInfoList = class(TJclCustomLocationInfoList, IJclLocationInfoList,
    IJclPreparedLocationInfoList)
  private
    FPrepared: Boolean;
    FModuleInfoList: IJclModuleInfoList;
    function GetItems(AIndex: Integer): TJclStackTraceViewerLocationInfo;
  public
    constructor Create; override;
    function Add(Addr: Pointer): TJclStackTraceViewerLocationInfo;
    property Items[AIndex: Integer]: TJclStackTraceViewerLocationInfo read GetItems;

    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IJclLocationInfoList }
    function GetCount: Integer;
    function GetLocationItems(AIndex: Integer): IJclLocationInfo;
    property Count: Integer read GetCount;
    property LocationItems[AIndex: Integer]: IJclLocationInfo read GetLocationItems; default;
    { IJclPreparedLocationInfoList }
    function GetPrepared: Boolean;
    procedure SetPrepared(AValue: Boolean);
    function GetModuleInfoList: IJclModuleInfoList;
    property ModuleInfoList: IJclModuleInfoList read FModuleInfoList write FModuleInfoList;
    property Prepared: Boolean read FPrepared write FPrepared;
  end;

  TJclStackTraceViewerThreadInfo = class(TJclCustomThreadInfo)
  private
    function GetStack(const AIndex: Integer): TJclStackTraceViewerLocationInfoList;
  protected
    function GetStackClass: TJclCustomLocationInfoListClass; override;
  public
    property CreationStack: TJclStackTraceViewerLocationInfoList index 1 read GetStack;
    property Stack: TJclStackTraceViewerLocationInfoList index 2 read GetStack;
  end;

  TJclStackTraceViewerThreadInfoList = class(TObject)
  private
    FItems: TObjectList;
    function GetItems(AIndex: Integer): TJclStackTraceViewerThreadInfo;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TJclStackTraceViewerThreadInfo;
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TJclStackTraceViewerThreadInfo read GetItems; default;
  end;

  TJclStackTraceViewerModuleModuleInfo = class(TJclSerializableModuleInfo, IJclModuleInfo)
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IJclModuleInfo }
    function GetBinFileVersion: string;
    function GetModuleName: string;
    property BinFileVersion: string read GetBinFileVersion;
    property ModuleName: string read GetModuleName;
  end;

  TJclStackTraceViewerModuleInfoList = class(TInterfacedObject, IInterface, IJclModuleInfoList)
  private
    FItems: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TJclStackTraceViewerModuleModuleInfo;
    procedure Clear;

    { IInterface }
    // function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IJclModuleInfoList }
    function GetModuleCount: Integer;
    function GetModuleInfo(AIndex: Integer): IJclModuleInfo;
    property Count: Integer read GetModuleCount;
    property Items[AIndex: Integer]: IJclModuleInfo read GetModuleInfo; default;
  end;

  TJclStackTraceViewerExceptionInfo = class(TObject)
  private
    FException: TJclSerializableException;
    FThreadInfoList: TJclStackTraceViewerThreadInfoList;
    FModules: TJclStackTraceViewerModuleInfoList;
    procedure AddModuleListToStacks;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AssignExceptionInfo(AExceptionInfo: TJclSerializableExceptionInfo);
    property ThreadInfoList: TJclStackTraceViewerThreadInfoList read FThreadInfoList;
    property Exception: TJclSerializableException read FException;
    property Modules: TJclStackTraceViewerModuleInfoList read FModules;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\stacktraceviewer';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

//=== { TJclStackTraceViewerLocationInfoList } ===============================

function TJclStackTraceViewerLocationInfoList.Add(Addr: Pointer): TJclStackTraceViewerLocationInfo;
begin
  Result := TJclStackTraceViewerLocationInfo(InternalAdd(Addr));
end;

constructor TJclStackTraceViewerLocationInfoList.Create;
begin
  inherited Create;
  FItemClass := TJclStackTraceViewerLocationInfo;
  FOptions := [];
  FPrepared := False;
end;

function TJclStackTraceViewerLocationInfoList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclStackTraceViewerLocationInfoList.GetItems(AIndex: Integer): TJclStackTraceViewerLocationInfo;
begin
  Result := TJclStackTraceViewerLocationInfo(FItems[AIndex]);
end;

function TJclStackTraceViewerLocationInfoList.GetLocationItems(AIndex: Integer): IJclLocationInfo;
begin
  FItems[AIndex].GetInterface(IJclLocationInfo, Result);
end;

function TJclStackTraceViewerLocationInfoList.GetModuleInfoList: IJclModuleInfoList;
begin
  Result := FModuleInfoList;
end;

function TJclStackTraceViewerLocationInfoList.GetPrepared: Boolean;
begin
  Result := FPrepared;
end;

function TJclStackTraceViewerLocationInfoList.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TJclStackTraceViewerLocationInfoList.SetPrepared(AValue: Boolean);
begin
  FPrepared := AValue;
end;

function TJclStackTraceViewerLocationInfoList._AddRef: Integer;
begin
  Result := -1;
end;

function TJclStackTraceViewerLocationInfoList._Release: Integer;
begin
  Result := -1;
end;

//=== { TJclStackTraceViewerThreadInfo } =====================================

function TJclStackTraceViewerThreadInfo.GetStack(const AIndex: Integer): TJclStackTraceViewerLocationInfoList;
begin
  case AIndex of
    1: Result := TJclStackTraceViewerLocationInfoList(FCreationStack);
    2: Result := TJclStackTraceViewerLocationInfoList(FStack);
    else
      Result := nil;
  end;
end;

function TJclStackTraceViewerThreadInfo.GetStackClass: TJclCustomLocationInfoListClass;
begin
  Result := TJclStackTraceViewerLocationInfoList;
end;

//=== { TJclStackTraceViewerThreadInfoList } =================================

constructor TJclStackTraceViewerThreadInfoList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TJclStackTraceViewerThreadInfoList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJclStackTraceViewerThreadInfoList.Add: TJclStackTraceViewerThreadInfo;
begin
  FItems.Add(TJclStackTraceViewerThreadInfo.Create);
  Result := TJclStackTraceViewerThreadInfo(FItems.Last);
end;

procedure TJclStackTraceViewerThreadInfoList.Clear;
begin
  FItems.Clear;
end;

function TJclStackTraceViewerThreadInfoList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclStackTraceViewerThreadInfoList.GetItems(AIndex: Integer): TJclStackTraceViewerThreadInfo;
begin
  Result := TJclStackTraceViewerThreadInfo(FItems[AIndex]);
end;

//=== { TJclStackTraceViewerExceptionInfo } ==================================

constructor TJclStackTraceViewerExceptionInfo.Create;
begin
  inherited Create;
  FException := TJclSerializableException.Create;
  FThreadInfoList := TJclStackTraceViewerThreadInfoList.Create;
  FModules := TJclStackTraceViewerModuleInfoList.Create;
end;

destructor TJclStackTraceViewerExceptionInfo.Destroy;
begin
  FModules.Free;
  FException.Free;
  FThreadInfoList.Free;
  inherited Destroy;
end;

procedure TJclStackTraceViewerExceptionInfo.AddModuleListToStacks;
var
  I: Integer;
begin
  for I := 0 to FThreadInfoList.Count - 1 do
    FThreadInfoList[I].CreationStack.ModuleInfoList := FModules;
  for I := 0 to FThreadInfoList.Count - 1 do
    FThreadInfoList[I].Stack.ModuleInfoList := FModules;
end;

procedure TJclStackTraceViewerExceptionInfo.AssignExceptionInfo(AExceptionInfo: TJclSerializableExceptionInfo);
var
  I: Integer;
begin
  FException.Assign(AExceptionInfo.Exception);
  FThreadInfoList.Clear;
  for I := 0 to AExceptionInfo.ThreadInfoList.Count - 1 do
    FThreadInfoList.Add.Assign(AExceptionInfo.ThreadInfoList[I]);
  FModules.Clear;
  for I := 0 to AExceptionInfo.Modules.Count - 1 do
    FModules.Add.Assign(AExceptionInfo.Modules[I]);
  AddModuleListToStacks;
end;

{ TJclStackTraceViewerLocationInfo }

function TJclStackTraceViewerLocationInfo.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TJclStackTraceViewerLocationInfo._AddRef: Integer;
begin
  Result := -1;
end;

function TJclStackTraceViewerLocationInfo._Release: Integer;
begin
  Result := -1;
end;

procedure TJclStackTraceViewerLocationInfo.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TJclStackTraceViewerLocationInfo then
  begin
    TJclStackTraceViewerLocationInfo(Dest).FFoundFile := FFoundFile;
    TJclStackTraceViewerLocationInfo(Dest).FFileName := FFileName;
    TJclStackTraceViewerLocationInfo(Dest).FProjectName := FProjectName;
    TJclStackTraceViewerLocationInfo(Dest).FRevision := FRevision;
    TJclStackTraceViewerLocationInfo(Dest).FTranslatedLineNumber := FTranslatedLineNumber;
  end;
end;

function TJclStackTraceViewerLocationInfo.GetAddress: Pointer;
begin
  Result := Address;
end;

function TJclStackTraceViewerLocationInfo.GetBinaryFileName: string;
begin
  Result := BinaryFileName;
end;

function TJclStackTraceViewerLocationInfo.GetFileName: string;
begin
  Result := FFileName;
end;

function TJclStackTraceViewerLocationInfo.GetFoundFile: Boolean;
begin
  Result := FFoundFile;
end;

function TJclStackTraceViewerLocationInfo.GetLineNumber: Integer;
begin
  Result := LineNumber;
end;

function TJclStackTraceViewerLocationInfo.GetLineNumberOffsetFromProcedureStart: Integer;
begin
  Result := LineNumberOffsetFromProcedureStart;
end;

function TJclStackTraceViewerLocationInfo.GetModuleName: string;
begin
  Result := ModuleName;
end;

function TJclStackTraceViewerLocationInfo.GetOffsetFromLineNumber: Integer;
begin
  Result := OffsetFromLineNumber;
end;

function TJclStackTraceViewerLocationInfo.GetOffsetFromProcName: Integer;
begin
  Result := OffsetFromProcName;
end;

function TJclStackTraceViewerLocationInfo.GetProcedureName: string;
begin
  Result := ProcedureName;
end;

function TJclStackTraceViewerLocationInfo.GetProjectName: string;
begin
  Result := FProjectName;
end;

function TJclStackTraceViewerLocationInfo.GetRevision: string;
begin
  Result := FRevision;
end;

function TJclStackTraceViewerLocationInfo.GetSourceName: string;
begin
  Result := SourceName;
end;

function TJclStackTraceViewerLocationInfo.GetSourceUnitName: string;
begin
  Result := SourceUnitName;
end;

function TJclStackTraceViewerLocationInfo.GetTranslatedLineNumber: Integer;
begin
  Result := FTranslatedLineNumber;
end;

function TJclStackTraceViewerLocationInfo.GetUnitVersionDateTime: TDateTime;
begin
  Result := UnitVersionDateTime;
end;

function TJclStackTraceViewerLocationInfo.GetUnitVersionExtra: string;
begin
  Result := UnitVersionExtra;
end;

function TJclStackTraceViewerLocationInfo.GetUnitVersionLogPath: string;
begin
  Result := UnitVersionLogPath;
end;

function TJclStackTraceViewerLocationInfo.GetUnitVersionRCSfile: string;
begin
  Result := UnitVersionRCSfile;
end;

function TJclStackTraceViewerLocationInfo.GetUnitVersionRevision: string;
begin
  Result := UnitVersionRevision;
end;

function TJclStackTraceViewerLocationInfo.GetVAddress: Pointer;
begin
  Result := VAddress;
end;

function TJclStackTraceViewerLocationInfo.GetValues: Integer;
begin
  Result := 0;
  if lievLocationInfo in (inherited Values) then
    Inc(Result, livLocationInfo);
  if lievProcedureStartLocationInfo in (inherited Values) then
    Inc(Result, livProcedureStartLocationInfo);
  if lievUnitVersionInfo in (inherited Values) then
    Inc(Result, livUnitVersionInfo);
end;

procedure TJclStackTraceViewerLocationInfo.SetFileName(AValue: string);
begin
  FFileName := AValue;
end;

procedure TJclStackTraceViewerLocationInfo.SetFoundFile(AValue: Boolean);
begin
  FFoundFile := AValue;
end;

procedure TJclStackTraceViewerLocationInfo.SetProjectName(AValue: string);
begin
  FProjectName := AValue;
end;

procedure TJclStackTraceViewerLocationInfo.SetRevision(AValue: string);
begin
  FRevision := AValue;
end;

procedure TJclStackTraceViewerLocationInfo.SetTranslatedLineNumber(AValue: Integer);
begin
  FTranslatedLineNumber := AValue;
end;


{ TJclStackTraceViewerModuleModuleInfo }

function TJclStackTraceViewerModuleModuleInfo.GetBinFileVersion: string;
begin
  Result := BinFileVersion;
end;

function TJclStackTraceViewerModuleModuleInfo.GetModuleName: string;
begin
  Result := ModuleName;
end;

function TJclStackTraceViewerModuleModuleInfo.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TJclStackTraceViewerModuleModuleInfo._AddRef: Integer;
begin
  Result := -1;
end;

function TJclStackTraceViewerModuleModuleInfo._Release: Integer;
begin
  Result := -1;
end;

{ TJclStackTraceViewerModuleInfoList }

function TJclStackTraceViewerModuleInfoList.Add: TJclStackTraceViewerModuleModuleInfo;
begin
  FItems.Add(TJclStackTraceViewerModuleModuleInfo.Create);
  Result := TJclStackTraceViewerModuleModuleInfo(FItems.Last);
end;

procedure TJclStackTraceViewerModuleInfoList.Clear;
begin
  FItems.Clear;
end;

constructor TJclStackTraceViewerModuleInfoList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TJclStackTraceViewerModuleInfoList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJclStackTraceViewerModuleInfoList.GetModuleCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclStackTraceViewerModuleInfoList.GetModuleInfo(AIndex: Integer): IJclModuleInfo;
begin
  FItems[AIndex].GetInterface(IJclModuleInfo, Result);
end;

function TJclStackTraceViewerModuleInfoList._AddRef: Integer;
begin
  Result := -1;
end;

function TJclStackTraceViewerModuleInfoList._Release: Integer;
begin
  Result := -1;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
