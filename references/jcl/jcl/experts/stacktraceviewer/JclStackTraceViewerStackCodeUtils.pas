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
{ The Original Code is JclStackTraceViewerStackCodeUtils.pas.                                      }
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

unit JclStackTraceViewerStackCodeUtils;

{$I jcl.inc}

interface

uses
  SysUtils,
  {$IFNDEF BDS}
  Windows, Classes,
  {$ENDIF ~BDS}
  ActiveX, ToolsAPI,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOtaUtils,
  JclStackTraceViewerAPI;

function FindModule(const AFileName: string): string;
function FindModuleAndProject(const AFileName: string; var AProjectName: string): string;
function GetFileEditorContent(const AFileName: string): IStream;
procedure JumpToCode(AStackViewItem: IJclLocationInfo);

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

function FindModuleInfoInProject(AProject: IOTAProject; const AFileName: string): IOTAModuleInfo;
var
  I, P: Integer;
  ModuleInfo: IOTAModuleInfo;
  S, S2: string;
begin
  Result := nil;
  if AProject.GetModuleCount > 0 then
  begin
    S := UpperCase(AFileName);
    for I := 0 to Pred(AProject.GetModuleCount) do
    begin
      ModuleInfo := AProject.GetModule(I);
      if Assigned(ModuleInfo) then
      begin
        S2 := UpperCase(ModuleInfo.FileName);
        P := Pos(S, S2);
        if (P > 0) and (P = Length(S2) - Length(S) + 1) then
        begin
          Result := ModuleInfo;
          Break;
        end;
      end;
    end;
  end;
end;

function FindModule(const AFileName: string): string;
var
  Dummy: string;
begin
  Result := FindModuleAndProject(AFilename, Dummy);
end;

function FindModuleInfoAndProject(const AFileName: string; var AProjectName: string): IOTAModuleInfo;
var
  I: Integer;
  ProjectGroup: IOTAProjectGroup;
begin
  Result := nil;
  AProjectName := '';
  {$IFDEF BDS}
  ProjectGroup := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
  {$ELSE ~BDS}
  ProjectGroup := TJclOTAExpertBase.GetProjectGroup;
  {$ENDIF ~BDS}
  if Assigned(ProjectGroup) then
    for I := 0 to ProjectGroup.ProjectCount - 1 do
    begin
      Result := FindModuleInfoInProject(ProjectGroup.Projects[I], AFileName);
      if Assigned(Result) then
      begin
        AProjectName := ProjectGroup.Projects[I].FileName;
        Break;
      end;
    end;
end;

function FindModuleAndProject(const AFileName: string; var AProjectName: string): string;
var
  ModuleInfo: IOTAModuleInfo;
begin
  ModuleInfo := FindModuleInfoAndProject(AFileName, AProjectName);
  if Assigned(ModuleInfo) then
    Result := ModuleInfo.FileName
  else
    Result := '';
end;

function GetFileEditorContent(const AFileName: string): IStream;
var
  I: Integer;
  Module: IOTAModule;
  {$IFDEF BDS}
  EditorContent: IOTAEditorContent;
  {$ELSE ~BDS}
  ContentPos, ReadCount, BufferSize: Integer;
  Buffer: Pointer;
  ModuleSourceEditor: IOTASourceEditor;
  ModuleReader: IOTAEditReader;
  S: TStream;
  SA: TStreamAdapter;
  {$ENDIF ~BDS}
begin
  Result := nil;
  Module := (BorlandIDEServices as IOTAModuleServices).FindModule(AFileName);
  if Assigned(Module) then
  begin
    {$IFDEF BDS}
    for I := 0 to Module.ModuleFileCount - 1 do
      if Supports(Module.ModuleFileEditors[I], IOTAEditorContent, EditorContent) then
      begin
        Result := EditorContent.Content;
        Break;
      end;
    {$ELSE ~BDS}
    for I := 0 to Module.GetModuleFileCount - 1 do
      if Supports(Module.GetModuleFileEditor(I), IOTASourceEditor, ModuleSourceEditor) then
      begin
        ModuleReader := ModuleSourceEditor.CreateReader;
        if Assigned(ModuleReader) then
        begin
          ContentPos := 0;
          BufferSize := 4096;
          SA := TStreamAdapter.Create(TMemoryStream.Create, soOwned);
          S := SA.Stream;
          Result := SA;
          GetMem(Buffer, BufferSize);
          try
            ReadCount := BufferSize;
            while ReadCount = BufferSize do
            begin
              ReadCount := ModuleReader.GetText(ContentPos, Buffer, ReadCount);
              if ReadCount > 0 then
              begin
                Inc(ContentPos, BufferSize);
                S.Write(Buffer^, ReadCount);
              end;
            end;
          finally
            FreeMem(Buffer);
          end;
        end;
        Break;
      end;
    {$ENDIF ~BDS}
  end;
end;

procedure JumpToCode(AStackViewItem: IJclLocationInfo);
var
  S, FileName, ProjectName: string;
  Module: IOTAModule;
  ModuleInfo: IOTAModuleInfo;  

  SourceEditor: IOTASourceEditor;
  I, LineNumber: Integer;
  EditPos: TOTAEditPos;
  PreparedLocationInfo: IJclPreparedLocationInfo;
begin
  if Assigned(AStackViewItem) then
  begin
    if AStackViewItem.QueryInterface(IJclPreparedLocationInfo, PreparedLocationInfo) <> S_OK then
      PreparedLocationInfo := nil;
    FileName := AStackViewItem.SourceName;
    ModuleInfo := FindModuleInfoAndProject(FileName, ProjectName);
    if Assigned(ModuleInfo) then
    begin
      S := ModuleInfo.FileName;
      if (S <> '') and Assigned(BorlandIDEServices) then
      begin
        {$IFDEF BDS}
        Module := (BorlandIDEServices as IOTAModuleServices).OpenModule(S);
        {$ELSE ~BDS}
        Module := ModuleInfo.OpenModule;
        {$ENDIF ~BDS}
      end;
    end
    else
    if Assigned(PreparedLocationInfo) and PreparedLocationInfo.FoundFile then
    begin
      {$IFDEF BDS}
      Module := (BorlandIDEServices as IOTAModuleServices).OpenModule(PreparedLocationInfo.FileName);
      {$ELSE ~BDS}
      (BorlandIDEServices as IOTAActionServices).OpenFile(PreparedLocationInfo.FileName);
      Module := (BorlandIDEServices as IOTAModuleServices).FindModule(PreparedLocationInfo.FileName);
      {$ENDIF ~BDS}
    end;
    if Assigned(Module) then
    begin
      {$IFDEF BDS}
      Module.Show;
      {$ENDIF BDS}
      for I := 0 to Module.GetModuleFileCount - 1 do
        if Supports(Module.GetModuleFileEditor(I), IOTASourceEditor, SourceEditor) then
        begin
          SourceEditor.Show;
          if SourceEditor.EditViewCount > 0 then
          begin
            if Assigned(PreparedLocationInfo) and (PreparedLocationInfo.TranslatedLineNumber > 0) then
              LineNumber := PreparedLocationInfo.TranslatedLineNumber
            else
              LineNumber := AStackViewItem.LineNumber;
            if LineNumber > 0 then
            begin
              SourceEditor.EditViews[0].Center(LineNumber, 1);
              EditPos.Line := LineNumber;
              EditPos.Col := 1;
              SourceEditor.EditViews[0].CursorPos := EditPos;
            end;
          end;
          Break;
        end;
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.