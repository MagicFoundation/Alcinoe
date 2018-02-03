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
{ The Original Code is JclStackTraceViewerStackFrame.pas.                                          }
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

unit JclStackTraceViewerStackFrame;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, IniFiles,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclDebug, JclStackTraceViewerAPI, JclStackTraceViewerStackCodeUtils;

type
  TfrmStack = class(TFrame, IJclStackTraceViewerStackFrame, IJclStackTraceViewerPreparableStackFrame,
    IJclStackTraceViewerStackSelection)
    lv: TListView;
    procedure lvDblClick(Sender: TObject);
    procedure lvChange(Sender: TObject; Item: TListItem; Change: TItemChange);
  private
    FStackList: IJclLocationInfoList;
    FOnSelectStackLine: TNotifyEvent;
    procedure DoSelectStackLine;
    procedure UpdateListView;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadState(AIni: TCustomIniFile; const ASection, APrefix: string);
    procedure SaveState(AIni: TCustomIniFile; const ASection, APrefix: string);
    { IJclStackTraceViewerStackFrame }
    function GetStackList: IJclLocationInfoList;
    procedure SetStackList(const Value: IJclLocationInfoList);
    procedure UpdateView;
    { IJclStackTraceViewerPreparableStackFrame }
    function GetPreparableLocationInfoListCount: Integer;
    function GetPreparableLocationInfoList(AIndex: Integer): IJclPreparedLocationInfoList;
    procedure UpdateViews;
    property StackList: IJclLocationInfoList read FStackList write SetStackList;
    property OnSelectStackLine: TNotifyEvent read FOnSelectStackLine write FOnSelectStackLine;
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

uses
  JclOtaResources;

{ TfrmStack }

constructor TfrmStack.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  lv.Columns.Items[0].Caption := LoadResString(@RsStackModuleName);
  lv.Columns.Items[0].Caption := LoadResString(@RsSourceUnitName);
  lv.Columns.Items[0].Caption := LoadResString(@RsProcedureName);
  lv.Columns.Items[0].Caption := LoadResString(@RsSourceName);
  lv.Columns.Items[0].Caption := LoadResString(@RsLineNumber);
  lv.Columns.Items[0].Caption := LoadResString(@RsLineNumberOffsetFromProcedureStart);
  lv.Columns.Items[0].Caption := LoadResString(@RsRevision);
  lv.Columns.Items[0].Caption := LoadResString(@RsProjectFile);
  lv.Columns.Items[0].Caption := LoadResString(@RsTranslatedLineNumber);
end;

procedure TfrmStack.DoSelectStackLine;
begin
  if Assigned(FOnSelectStackLine) then
    FOnSelectStackLine(Self);
end;

function TfrmStack.GetPreparableLocationInfoList(AIndex: Integer): IJclPreparedLocationInfoList;
begin
  if AIndex = 0 then
  begin
    if FStackList.QueryInterface(IJclPreparedLocationInfoList, Result) <> S_OK then
      Result := nil;
  end
  else
    Result := nil;
end;

function TfrmStack.GetPreparableLocationInfoListCount: Integer;
var
  Dummy: IJclPreparedLocationInfoList;
begin
  if Assigned(FStackList) and (FStackList.QueryInterface(IJclPreparedLocationInfoList, Dummy) = S_OK) then
    Result := 1
  else
    Result := 0;
end;

function TfrmStack.GetSelected: IJclLocationInfo;
begin
  if not (Assigned(lv.Selected) and Assigned(lv.Selected.Data) and
    (IUnknown(lv.Selected.Data).QueryInterface(IJclLocationInfo, Result) = S_OK)) then
    Result := nil;
end;

function TfrmStack.GetStackList: IJclLocationInfoList;
begin
  Result := FStackList;
end;

procedure TfrmStack.LoadState(AIni: TCustomIniFile; const ASection, APrefix: string);
var
  I: Integer;
begin
  for I := 0 to lv.Columns.Count - 1 do
    lv.Columns.Items[I].Width := AIni.ReadInteger(ASection,
      Format(APrefix + 'ColumnWidth%d', [I]), lv.Columns.Items[I].Width);
end;

procedure TfrmStack.lvChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  DoSelectStackLine;
end;

procedure TfrmStack.lvDblClick(Sender: TObject);
begin
  JumpToCode(GetSelected);
end;

procedure TfrmStack.SaveState(AIni: TCustomIniFile; const ASection, APrefix: string);
var
  I: Integer;
begin
  for I := 0 to lv.Columns.Count - 1 do
    AIni.WriteInteger(ASection, Format(APrefix + 'ColumnWidth%d', [I]), lv.Columns.Items[I].Width);
end;

procedure TfrmStack.SetStackList(const Value: IJclLocationInfoList);
begin
  FStackList := Value;
  UpdateListView;
end;

procedure TfrmStack.UpdateListView;
var
  I: Integer;
  ListItem: TListItem;
  S: string;
  PreparedLocationInfo: IJclPreparedLocationInfo;
  LocationInfo: IJclLocationInfo;
  FixProcedureName: Boolean;
begin
  lv.Items.BeginUpdate;
  try
    lv.Items.Clear;
    if Assigned(FStackList) then
    begin
      FixProcedureName := True;
      for I := 0 to FStackList.Count - 1 do
      begin
        LocationInfo := FStackList[I];
        if (LocationInfo.SourceUnitName <> '') and
          (Pos(LocationInfo.SourceUnitName + '.', LocationInfo.ProcedureName) <> 1) then
        begin
          FixProcedureName := False;
          Break;
        end;
      end;
      for I := 0 to FStackList.Count - 1 do
      begin
        ListItem := lv.Items.Add;
        ListItem.Caption := FStackList[I].ModuleName;
        ListItem.SubItems.Add(FStackList[I].SourceUnitName);
        S := FStackList[I].ProcedureName;
        if FixProcedureName and (FStackList[I].SourceUnitName <> '') then
          Delete(S, 1, Length(FStackList[I].SourceUnitName) + 1);
        ListItem.SubItems.Add(S);
        ListItem.SubItems.Add(FStackList[I].SourceName);
        if FStackList[I].LineNumber > 0 then
          S := IntToStr(FStackList[I].LineNumber)
        else
          S := '';
        ListItem.SubItems.Add(S);
        if FStackList[I].Values and livProcedureStartLocationInfo <> 0 then
          S := IntToStr(FStackList[I].LineNumberOffsetFromProcedureStart)
        else
          S := '';
        ListItem.SubItems.Add(S);
        if FStackList[I].QueryInterface(IJclPreparedLocationInfo, PreparedLocationInfo) = S_OK then
        begin
          ListItem.SubItems.Add(PreparedLocationInfo.Revision);
          if PreparedLocationInfo.ProjectName <> '' then
            S := ExtractFileName(PreparedLocationInfo.ProjectName)
          else
            S := ExtractFileName(PreparedLocationInfo.FileName);
          ListItem.SubItems.Add(S);
          if PreparedLocationInfo.TranslatedLineNumber > 0 then
            S := IntToStr(PreparedLocationInfo.TranslatedLineNumber)
          else
            S := '';
          ListItem.SubItems.Add(S);
        end
        else
        begin
          ListItem.SubItems.Add('');
          ListItem.SubItems.Add('');
          ListItem.SubItems.Add('');
        end;
        ListItem.Data := Pointer(FStackList[I]);
      end;
    end;
  finally
    lv.Items.EndUpdate;
  end;
end;

procedure TfrmStack.UpdateView;
begin
  UpdateListView;
end;

procedure TfrmStack.UpdateViews;
begin
  UpdateView;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
