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
{ The Original Code is JclOtaExcDlgRepository.pas.                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{         <outchy att users dott sourceforge dott net>                                             }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All rights reserved.     }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclPreProcessorExcDlgTemplates;

interface

{$I jcl.inc}

uses
  {$IFDEF HAS_UNITSCOPE}
  System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclIDEUtils,
  JclPreProcessorTemplates;

type
  TJclExcDlgParams = class(TJclTemplateParams)
  private
    FHookDll: Boolean;
    FFileName: string;
    FCodeDetails: Boolean;
    FModuleName: Boolean;
    FModuleOffset: Boolean;
    FDelayedTrace: Boolean;
    FFormName: string;
    FLogFile: Boolean;
    FLogFileName: string;
    FAutoSaveWorkingDirectory: Boolean;
    FAutoSaveApplicationDirectory: Boolean;
    FAutoSaveDesktopDirectory: Boolean;
    FLogSaveDialog: Boolean;
    FAddressOffset: Boolean;
    FVirtualAddress: Boolean;
    FActivePersonality: TJclBorPersonality;
    FLanguage: TJclBorPersonality;
    FLanguages: TJclBorPersonalities;
    FRawData: Boolean;
    FSendEMail: Boolean;
    FEMailAddress: string;
    FFormAncestor: string;
    FModalDialog: Boolean;
    FSizeableDialog: Boolean;
    FEMailSubject: string;
    FDesigner: TJclBorDesigner;
    FModuleList: Boolean;
    FUnitVersioning: Boolean;
    FOSInfo: Boolean;
    FActiveControls: Boolean;
    FDisableIfDebuggerAttached: Boolean;
    FStackList: Boolean;
    FAutoScrollBars: Boolean;
    FCatchMainThread: Boolean;
    FAllThreads: Boolean;
    FAllRegisteredThreads: Boolean;
    FMainExceptionThreads: Boolean;
    FExceptionThread: Boolean;
    FMainThread: Boolean;
    FTraceEAbort: Boolean;
    FIgnoredExceptions: TStrings;
    FIgnoredExceptionsIndex: Integer;
    FTraceAllExceptions: Boolean;
    function GetIgnoredExceptionsCount: Integer;
    function GetReportAllThreads: Boolean;
    function GetReportExceptionThread: Boolean;
    function GetReportMainThread: Boolean;
    function GetIgnoredException: string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override; 
  published
    // file options
    property Language: TJclBorPersonality read FLanguage write FLanguage;
    property Languages: TJclBorPersonalities read FLanguages write FLanguages;
    property ActivePersonality: TJclBorPersonality read FActivePersonality
      write FActivePersonality;
    property FileName: string read FFileName write FFileName;
    property FormName: string read FFormName write FFormName;
    property FormAncestor: string read FFormAncestor write FFormAncestor;
    property Designer: TJclBorDesigner read FDesigner write FDesigner;
    // form options
    property ModalDialog: Boolean read FModalDialog write FModalDialog;
    property SendEMail: Boolean read FSendEMail write FSendEMail;
    property EMailAddress: string read FEMailAddress write FEMailAddress;
    property EMailSubject: string read FEMailSubject write FEMailSubject;
    property SizeableDialog: Boolean read FSizeableDialog write FSizeableDialog;
    property AutoScrollBars: Boolean read FAutoScrollBars write FAutoScrollBars;
    // system options
    property DelayedTrace: Boolean read FDelayedTrace write FDelayedTrace;
    property HookDll: Boolean read FHookDll write FHookDll;
    property OSInfo: Boolean read FOSInfo write FOSInfo;
    property ModuleList: Boolean read FModuleList write FModuleList;
    property UnitVersioning: Boolean read FUnitVersioning write FUnitVersioning;
    property ActiveControls: Boolean read FActiveControls write FActiveControls;
    property CatchMainThread: Boolean read FCatchMainThread write FCatchMainThread;
    property DisableIfDebuggerAttached: Boolean read FDisableIfDebuggerAttached write FDisableIfDebuggerAttached;
    // log options
    property LogFile: Boolean read FLogFile write FLogFile;
    property LogFileName: string read FLogFileName write FLogFileName;
    property AutoSaveWorkingDirectory: Boolean read FAutoSaveWorkingDirectory write FAutoSaveWorkingDirectory;
    property AutoSaveApplicationDirectory: Boolean read FAutoSaveApplicationDirectory write FAutoSaveApplicationDirectory;
    property AutoSaveDesktopDirectory: Boolean read FAutoSaveDesktopDirectory write FAutoSaveDesktopDirectory;
    property LogSaveDialog: Boolean read FLogSaveDialog write FLogSaveDialog;
    // ignored exceptions
    property TraceAllExceptions: Boolean read FTraceAllExceptions
      write FTraceAllExceptions;
    property TraceEAbort: Boolean read FTraceEAbort write FTraceEAbort;
    property IgnoredException: string read GetIgnoredException;
    property IgnoredExceptions: TStrings read FIgnoredExceptions write FIgnoredExceptions;
    property IgnoredExceptionsIndex: Integer read FIgnoredExceptionsIndex write FIgnoredExceptionsIndex;
    property IgnoredExceptionsCount: Integer read GetIgnoredExceptionsCount;
    // trace options
    property StackList: Boolean read FStackList write FStackList;
    property RawData: Boolean read FRawData write FRawData;
    property ModuleName: Boolean read FModuleName write FModuleName;
    property ModuleOffset: Boolean read FModuleOffset write FModuleOffset;
    // thread options (mutually exclusives)
    property AllThreads: Boolean read FAllThreads write FAllThreads;
    property AllRegisterThreads: Boolean read FAllRegisteredThreads write FAllRegisteredThreads;
    property MainExceptionThreads: Boolean read FMainExceptionThreads write FMainExceptionThreads;
    property ExceptionThread: Boolean read FExceptionThread write FExceptionThread;
    property MainThread: Boolean read FMainThread write FMainThread;
    // composite properties
    property ReportMainThread: Boolean read GetReportMainThread;
    property ReportAllThreads: Boolean read GetReportAllThreads;
    property ReportExceptionThread: Boolean read GetReportExceptionThread; 
    //property AddressOffset: Boolean read FAddressOffset write FAddressOffset;
    property CodeDetails: Boolean read FCodeDetails write FCodeDetails;
    property VirtualAddress: Boolean read FVirtualAddress write FVirtualAddress;
  end;

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

//=== { TJclExcDlgParams } ===================================================

constructor TJclExcDlgParams.Create;
begin
  inherited Create;

  FHookDll := True;
  FLanguage := bpUnknown;
  FLanguages := [bpUnknown];
  FFileName := '';
  FCodeDetails := True;
  FModuleName := True;
  FModuleOffset := False;
  FDelayedTrace := True;
  FFormName := 'ExceptionDialog';
  FFormAncestor := 'TForm';
  FLogFile := False;
  FLogFileName := 'ExtractFileName(Application.ExeName) + ''-exception-'' + FormatDateTime(''yyyy-mm-dd'', Date) + ''.log''';
  FAutoSaveWorkingDirectory := False;
  FAutoSaveApplicationDirectory := False;
  FAutoSaveDesktopDirectory := False;
  FLogSaveDialog := False;
  FAddressOffset := True;
  FVirtualAddress := False;
  FActivePersonality := bpUnknown;
  FRawData := False;
  FSendEMail := False;
  FEMailAddress := '';
  FEMailSubject := '';
  FModalDialog := True;
  FSizeableDialog := False;
  FDesigner := bdVCL;
  FModuleList := True;
  FUnitVersioning := True;
  FOSInfo := True;
  FActiveControls := True;
  FDisableIfDebuggerAttached := False;
  FStackList := True;
  FAutoScrollBars := True;
  FCatchMainThread := False;
  FTraceEAbort := False;
  FTraceAllExceptions := False;
  FIgnoredExceptions := TStringList.Create;
  FAllThreads := True;
  FAllRegisteredThreads := False;
  FMainExceptionThreads := False;
  FExceptionThread := False;
  FMainThread := False;
end;

destructor TJclExcDlgParams.Destroy;
begin
  FIgnoredExceptions.Free;
  inherited Destroy;
end;

function TJclExcDlgParams.GetIgnoredException: string;
begin
  Result := FIgnoredExceptions.Strings[FIgnoredExceptionsIndex];
end;

function TJclExcDlgParams.GetIgnoredExceptionsCount: Integer;
begin
  Result := FIgnoredExceptions.Count;
end;

function TJclExcDlgParams.GetReportAllThreads: Boolean;
begin
  Result := FAllThreads or FAllRegisteredThreads;
end;

function TJclExcDlgParams.GetReportExceptionThread: Boolean;
begin
  Result := FExceptionThread or FMainExceptionThreads;
end;

function TJclExcDlgParams.GetReportMainThread: Boolean;
begin
  Result := FMainThread or FMainExceptionThreads or FAllThreads or FAllRegisteredThreads;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
