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
{ The Original Code is JclStackTraceViewerModuleFrame.pas.                                         }
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

unit JclStackTraceViewerModuleFrame;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, IniFiles,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclDebugSerialization;

type
  TfrmModule = class(TFrame)
    lv: TListView;
  private
    FModuleList: TJclSerializableModuleInfoList;
    procedure SetModuleList(const Value: TJclSerializableModuleInfoList);
  public
    constructor Create(AOwner: TComponent); override;
    property ModuleList: TJclSerializableModuleInfoList read FModuleList write SetModuleList;
    procedure LoadState(AIni: TCustomIniFile; const ASection: string);
    procedure SaveState(AIni: TCustomIniFile; const ASection: string);
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

{ TfrmModule }

constructor TfrmModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  lv.Columns.Items[0].Caption := LoadResString(@RsStartAddr);
  lv.Columns.Items[1].Caption := LoadResString(@RsEndAddr);
  lv.Columns.Items[2].Caption := LoadResString(@RsSystemModule);
  lv.Columns.Items[3].Caption := LoadResString(@RsBinFileName);
  lv.Columns.Items[4].Caption := LoadResString(@RsBinFileVersion);
  lv.Columns.Items[5].Caption := LoadResString(@RsFileVersion);
  lv.Columns.Items[6].Caption := LoadResString(@RsFileDescription);
end;

procedure TfrmModule.LoadState(AIni: TCustomIniFile; const ASection: string);
var
  I: Integer;
begin
  for I := 0 to lv.Columns.Count - 1 do
    lv.Columns.Items[I].Width := AIni.ReadInteger(ASection,
      Format('ModuleFrameColumnWidth%d', [I]), lv.Columns.Items[I].Width);
end;

procedure TfrmModule.SaveState(AIni: TCustomIniFile; const ASection: string);
var
  I: Integer;
begin
  for I := 0 to lv.Columns.Count - 1 do
    AIni.WriteInteger(ASection, Format('ModuleFrameColumnWidth%d', [I]), lv.Columns.Items[I].Width);
end;

procedure TfrmModule.SetModuleList(const Value: TJclSerializableModuleInfoList);
var
  I: Integer;
  ListItem: TListItem;
begin
  FModuleList := Value;
  lv.Items.Clear;
  for I := 0 to FModuleList.Count - 1 do
  begin
    ListItem := lv.Items.Add;
    ListItem.Caption := FModuleList[I].StartStr;
    ListItem.SubItems.Add(FModuleList[I].EndStr);
    ListItem.SubItems.Add(FModuleList[I].SystemModuleStr);
    ListItem.SubItems.Add(ExtractFileName(FModuleList[I].ModuleName));
    ListItem.SubItems.Add(FModuleList[I].BinFileVersion);
    ListItem.SubItems.Add(FModuleList[I].FileVersion);
    ListItem.SubItems.Add(FModuleList[I].FileDescription);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
