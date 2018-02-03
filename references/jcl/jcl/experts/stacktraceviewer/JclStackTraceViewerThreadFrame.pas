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
{ The Original Code is JclStackTraceViewerThreadFrame.pas.                                         }
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

unit JclStackTraceViewerThreadFrame;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, IniFiles,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclDebugSerialization, JclStackTraceViewerAPI, JclStackTraceViewerClasses, JclStackTraceViewerStackFrame, JclStackTraceViewerExceptInfoFrame;

type
  TfrmThread = class(TFrame, IJclStackTraceViewerPreparableStackFrame, IJclStackTraceViewerStackSelection)
    pnlExceptInfo: TPanel;
    pnlCreationStack: TPanel;
    pnlStack: TPanel;
    splCreationStack: TSplitter;
  private
    FCreationStackFrame: TfrmStack;
    FExceptionFrame: TfrmException;
    FStackFrame: TfrmStack;
    FCreationStackList: TJclStackTraceViewerLocationInfoList;
    FStackList: TJclStackTraceViewerLocationInfoList;
    FException: TJclSerializableException;
    FLastStackFrame: TObject;
    FCreationStackHeight: Integer;
    FStackInterfaceList: TInterfaceList;
    procedure SaveSplitterState;
    procedure SetCreationStackList(const Value: TJclStackTraceViewerLocationInfoList);
    procedure SetException(const Value: TJclSerializableException);
    procedure SetStackList(const Value: TJclStackTraceViewerLocationInfoList);
    procedure HandleStackSelection(ASender: TObject);
    procedure UpdateSplitterState;
    procedure UpdatePreparableLocationInfoLists;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadState(AIni: TCustomIniFile; const ASection: string);
    procedure SaveState(AIni: TCustomIniFile; const ASection: string);
    property CreationStackList: TJclStackTraceViewerLocationInfoList read FCreationStackList write SetCreationStackList;
    property Exception: TJclSerializableException read FException write SetException;
    property StackList: TJclStackTraceViewerLocationInfoList read FStackList write SetStackList;

    { IJclStackTraceViewerPreparableStackFrame }
    function GetPreparableLocationInfoListCount: Integer;
    function GetPreparableLocationInfoList(AIndex: Integer): IJclPreparedLocationInfoList;
    procedure UpdateViews;
    property PreparableLocationInfoListCount: Integer read GetPreparableLocationInfoListCount;
    property PreparableLocationInfoList[AIndex: Integer]: IJclPreparedLocationInfoList read GetPreparableLocationInfoList;
    { IJclStackTraceViewerStackSelection }
    function GetSelected: IJclLocationInfo;
    property Selected: IJclLocationInfo read GetSelected;
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

{$R *.dfm}

{ TfrmThread }

constructor TfrmThread.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCreationStackFrame := TfrmStack.Create(Self);
  FCreationStackFrame.Name := 'ThreadCreationStackFrame';
  FCreationStackFrame.Parent := pnlCreationStack;
  FCreationStackFrame.Align := alClient;
  FCreationStackFrame.OnSelectStackLine := HandleStackSelection;
  FExceptionFrame := TfrmException.Create(Self);
  FExceptionFrame.Parent := pnlExceptInfo;
  FExceptionFrame.Align := alClient;
  FStackFrame := TfrmStack.Create(Self);
  FStackFrame.Name := 'ThreadStackFrame';
  FStackFrame.Parent := pnlStack;
  FStackFrame.Align := alClient;
  FStackFrame.OnSelectStackLine := HandleStackSelection;
  FCreationStackHeight := pnlCreationStack.Height;
  FLastStackFrame := nil;
  FStackInterfaceList := TInterfaceList.Create;
end;

destructor TfrmThread.Destroy;
begin
  FStackInterfaceList.Free;
  inherited Destroy;
end;

function TfrmThread.GetPreparableLocationInfoList(AIndex: Integer): IJclPreparedLocationInfoList;
begin
  if FStackInterfaceList[AIndex].QueryInterface(IJclPreparedLocationInfoList, Result) <> S_OK then
    Result := nil;
end;

function TfrmThread.GetPreparableLocationInfoListCount: Integer;
begin
  Result := FStackInterfaceList.Count;
end;

function TfrmThread.GetSelected: IJclLocationInfo;
var
  StackTraceViewerStackSelection: IJclStackTraceViewerStackSelection;
begin
  if (FLastStackFrame = FStackFrame) and FStackFrame.Visible and
    (FStackFrame.GetInterface(IJclStackTraceViewerStackSelection, StackTraceViewerStackSelection)) and
    Assigned(StackTraceViewerStackSelection.Selected) then
    Result := StackTraceViewerStackSelection.Selected
  else
  if (FLastStackFrame = FCreationStackFrame) and FCreationStackFrame.Visible and
    (FCreationStackFrame.GetInterface(IJclStackTraceViewerStackSelection, StackTraceViewerStackSelection)) and
    Assigned(StackTraceViewerStackSelection.Selected) then
    Result := StackTraceViewerStackSelection.Selected
  else
    Result := nil;
end;

procedure TfrmThread.HandleStackSelection(ASender: TObject);
begin
  FLastStackFrame := ASender;
end;

procedure TfrmThread.LoadState(AIni: TCustomIniFile; const ASection: string);
begin
  FCreationStackHeight := AIni.ReadInteger(ASection, 'CreationStackFrameHeight', FCreationStackHeight);
  UpdateSplitterState;
  FStackFrame.LoadState(AIni, ASection, 'StackFrameThread');
  FCreationStackFrame.LoadState(AIni, ASection, 'CreationStackFrameThread');
end;

procedure TfrmThread.SaveSplitterState;
begin
  if pnlStack.Visible and pnlCreationStack.Visible then
    FCreationStackHeight := pnlCreationStack.Height;
end;

procedure TfrmThread.SaveState(AIni: TCustomIniFile; const ASection: string);
begin
  SaveSplitterState;
  AIni.WriteInteger(ASection, 'CreationStackFrameHeight', FCreationStackHeight);
  FStackFrame.SaveState(AIni, ASection, 'StackFrameThread');
  FCreationStackFrame.SaveState(AIni, ASection, 'CreationStackFrameThread');
end;

procedure TfrmThread.SetCreationStackList(const Value: TJclStackTraceViewerLocationInfoList);
begin
  FCreationStackList := Value;
  FCreationStackFrame.StackList := FCreationStackList;
  UpdatePreparableLocationInfoLists;
  SaveSplitterState;
  pnlCreationStack.Visible := Assigned(FCreationStackList);
  UpdateSplitterState;
end;

procedure TfrmThread.SetException(const Value: TJclSerializableException);
begin
  FException := Value;
  FExceptionFrame.Exception := FException;
  pnlExceptInfo.Visible := Assigned(FException);
end;

procedure TfrmThread.SetStackList(const Value: TJclStackTraceViewerLocationInfoList);
begin
  FStackList := Value;
  FStackFrame.StackList := FStackList;
  UpdatePreparableLocationInfoLists;
  SaveSplitterState;
  pnlStack.Visible := Assigned(FStackList);
  UpdateSplitterState;
end;

procedure TfrmThread.UpdatePreparableLocationInfoLists;
var
  PreparedLocationInfoList: IJclPreparedLocationInfoList;
begin
  FStackInterfaceList.Clear;
  if Assigned(FCreationStackList) then
    if FCreationStackList.GetInterface(IJclPreparedLocationInfoList, PreparedLocationInfoList) then
      FStackInterfaceList.Add(PreparedLocationInfoList);
  if Assigned(FStackList) then
    if FStackList.GetInterface(IJclPreparedLocationInfoList, PreparedLocationInfoList) then
      FStackInterfaceList.Add(PreparedLocationInfoList);
end;

procedure TfrmThread.UpdateSplitterState;
begin
  splCreationStack.Visible := pnlStack.Visible and pnlCreationStack.Visible;
  if splCreationStack.Visible then
  begin
    pnlCreationStack.Height := FCreationStackHeight;
    splCreationStack.Top := pnlCreationStack.Top - 1;
  end;
end;

procedure TfrmThread.UpdateViews;
var
  StackTraceViewerPreparableStackFrame: IJclStackTraceViewerPreparableStackFrame;
begin
  if FStackFrame.Visible and
    (FStackFrame.GetInterface(IJclStackTraceViewerPreparableStackFrame, StackTraceViewerPreparableStackFrame)) then
    StackTraceViewerPreparableStackFrame.UpdateViews;
  if FCreationStackFrame.Visible and
    (FCreationStackFrame.GetInterface(IJclStackTraceViewerPreparableStackFrame, StackTraceViewerPreparableStackFrame)) then
    StackTraceViewerPreparableStackFrame.UpdateViews;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
