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
{ The Original Code is JclStackTraceViewerAPI.pas.                                                 }
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

unit JclStackTraceViewerAPI;

{$I jcl.inc}

interface

uses
  Classes, ActiveX,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Forms;

const
  livLocationInfo = 1;
  livProcedureStartLocationInfo = 2;
  livUnitVersionInfo = 4;

  dfStack = 1;

type
  IJclLineNumberTranslator = interface
  ['{01E06940-49AE-464B-AC47-D65DFBC41396}']
    function GetIDString: string;
    function GetName: string;
    function TranslateLineNumbers(ARevisionContent, ACurrentContent: IStream;
      ARevisionLineNumbers: TList; ACurrentLineNumbers: TList): Integer;

    property Name: string read GetName;
    property IDString: string read GetIDString;
  end;

  IJclRevisionProvider = interface
  ['{8127FF3C-083D-47FD-855D-6C68EC7CBFB9}']
    function GetIDString: string;
    function GetName: string;
    function GetRevisionContent(const AFileName, ARevision: string; AContent: IStream): Boolean;

    property Name: string read GetName;
    property IDString: string read GetIDString;
  end;

  IJclLocationInfo = interface
  ['{888244F1-FC29-4330-B2DE-FAB4ED15DC3E}']
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
  end;

  IJclPreparedLocationInfo = interface(IJclLocationInfo)
  ['{B03E4506-221A-46B6-9668-E32FFAF17736}']
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

    property FileName: string read GetFileName write SetFileName;
    property FoundFile: Boolean read GetFoundFile write SetFoundFile;
    property ProjectName: string read GetProjectName write SetProjectName;
    property Revision: string read GetRevision write SetRevision;
    property TranslatedLineNumber: Integer read GetTranslatedLineNumber write SetTranslatedLineNumber;
  end;

  IJclLocationInfoList = interface
  ['{0A24DD15-0A3E-4584-88FE-B3E56E5DFA41}']
    function GetCount: Integer;
    function GetLocationItems(AIndex: Integer): IJclLocationInfo;

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: IJclLocationInfo read GetLocationItems; default;
  end;

  IJclModuleInfoList = interface;

  IJclPreparedLocationInfoList = interface(IJclLocationInfoList)
  ['{CC153034-5275-414C-B454-06BE66A9E3E0}']
    function GetModuleInfoList: IJclModuleInfoList;
    function GetPrepared: Boolean;
    procedure SetPrepared(AValue: Boolean);

    property ModuleInfoList: IJclModuleInfoList read GetModuleInfoList;
    property Prepared: Boolean read GetPrepared write SetPrepared;
  end;

  IJclModuleInfo = interface
  ['{5103F01E-6B33-434B-B0A9-DAEA30C01E2C}']
    function GetBinFileVersion: string;
    function GetModuleName: string;

    property BinFileVersion: string read GetBinFileVersion;
    property ModuleName: string read GetModuleName;
  end;

  IJclModuleInfoList = interface
  ['{1351C9C7-67A1-4A65-83B5-DC6814C300FF}']
    function GetModuleCount: Integer;
    function GetModuleInfo(AIndex: Integer): IJclModuleInfo;

    property Count: Integer read GetModuleCount;
    property Items[AIndex: Integer]: IJclModuleInfo read GetModuleInfo; default;
  end;

  IJclStackTraceViewerStackProcessorServices = interface
  ['{B4E7053D-AC74-4ED9-8B6D-6EA93EE3FB96}']
    function GetModuleInfoList: IJclModuleInfoList;
    procedure PrepareLocationInfoList(AStack: IJclPreparedLocationInfoList; AForce: Boolean = False);
    procedure SetModuleInfoList(AValue: IJclModuleInfoList);

    property ModuleList: IJclModuleInfoList read GetModuleInfoList write SetModuleInfoList;
  end;

  IJclStackTraceViewerTreeViewLink = interface
  ['{A87C6CD8-7253-4D2A-8C82-441C49CA591F}']
     procedure DoShow(AFrame: TCustomFrame);
     function GetCount: Integer;
     function GetFrameClass: TCustomFrameClass;
     function GetItems(AIndex: Integer): IJclStackTraceViewerTreeViewLink;
     function GetText: string;

     property Count: Integer read GetCount;
     property FrameClass: TCustomFrameClass read GetFrameClass;
     property Items[AIndex: Integer]: IJclStackTraceViewerTreeViewLink read GetItems; default;
     property Text: string read GetText;
  end;

  IJclStackTraceViewerStackFrame = interface
  ['{5868BC94-D24A-42EB-8A4A-9AB411702407}']
    function GetStackList: IJclLocationInfoList;
    procedure SetStackList(const AValue: IJclLocationInfoList);
    procedure UpdateView;
  end;

  IJclStackTraceViewerPreparableStackFrame = interface
  ['{E1E3D9FF-AE1C-43AD-8273-1A440B5C46C1}']
    function GetPreparableLocationInfoListCount: Integer;
    function GetPreparableLocationInfoList(AIndex: Integer): IJclPreparedLocationInfoList;
    procedure UpdateViews;

    property PreparableLocationInfoListCount: Integer read GetPreparableLocationInfoListCount;
    property PreparableLocationInfoList[AIndex: Integer]: IJclPreparedLocationInfoList read GetPreparableLocationInfoList;
  end;

  IJclStackTraceViewerStackSelection = interface
  ['{1FA53A21-9724-414D-BB56-17FB635794A0}']
    function GetSelected: IJclLocationInfo;

    property Selected: IJclLocationInfo read GetSelected;
  end;

  IJclStackTraceViewerStackServices = interface
  ['{1A59CDD1-6A8F-4D29-8678-314718CF995B}']
    function GetDefaultFrameClass(const AFrameClassID: Integer): TCustomFrameClass;
    procedure ShowTree(ARootLink: IJclStackTraceViewerTreeViewLink);
    procedure UnregisterFrameClass(AFrameClass: TCustomFrameClass);
  end;

var
  RegisterLineNumberTranslatorProc: function(const ATranslator: IJclLineNumberTranslator): Integer = nil;
  UnregisterLineNumberTranslatorProc: procedure(AIndex: Integer) = nil;
  RegisterRevisionProviderProc: function(const ATranslator: IJclRevisionProvider): Integer = nil;
  UnregisterRevisionProviderProc: procedure(AIndex: Integer) = nil;
  StackTraceViewerStackServices: IJclStackTraceViewerStackServices = nil;
  StackTraceViewerStackProcessorServices: IJclStackTraceViewerStackProcessorServices = nil;

function RegisterLineNumberTranslator(const ATranslator: IJclLineNumberTranslator): Integer;
procedure UnregisterLineNumberTranslator(AIndex: Integer);

function RegisterRevisionProvider(const ATranslator: IJclRevisionProvider): Integer;
procedure UnregisterRevisionProvider(AIndex: Integer);

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

function RegisterLineNumberTranslator(const ATranslator: IJclLineNumberTranslator): Integer;
begin
  Result := RegisterLineNumberTranslatorProc(ATranslator);
end;

procedure UnregisterLineNumberTranslator(AIndex: Integer);
begin
  UnregisterLineNumberTranslatorProc(AIndex);
end;

function RegisterRevisionProvider(const ATranslator: IJclRevisionProvider): Integer;
begin
  Result := RegisterRevisionProviderProc(ATranslator);
end;

procedure UnregisterRevisionProvider(AIndex: Integer);
begin
  UnregisterRevisionProviderProc(AIndex);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
