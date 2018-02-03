{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) - Delphi Tools                                                   }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is ModulesDump.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date$                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit ModulesDump;

{$I JCL.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ViewTemplate, Menus, ActnList, ComCtrls, ToolWin;

type
  TModulesDumpForm = class(TViewForm)
    StatusBar: TStatusBar;
    ModulesListView: TListView;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    Refresh2: TMenuItem;
    N1: TMenuItem;
    Copy2: TMenuItem;
    Selectall2: TMenuItem;
    N2: TMenuItem;
    Selectall3: TMenuItem;
    FileProp1: TAction;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    Properties1: TMenuItem;
    DumpPe1: TAction;
    ToolButton9: TToolButton;
    DumpPE2: TMenuItem;
    ToolButton10: TToolButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Refresh1Execute(Sender: TObject);
    procedure ModulesListViewColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure ModulesListViewCompare(Sender: TObject; Item1,
      Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure FileProp1Update(Sender: TObject);
    procedure FileProp1Execute(Sender: TObject);
    procedure ModulesListViewInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: String);
    procedure DumpPe1Execute(Sender: TObject);
    procedure DumpPe1Update(Sender: TObject);
  private
    function SelectedFileName: TFileName;
  public
    procedure BuildContent; override;
    procedure BuildModulesList;
  end;

var
  ModulesDumpForm: TModulesDumpForm;

implementation

{$R *.DFM}

uses
  ToolsUtils, TLHelp32, JclShell, Global;

resourcestring
  sModulesCount = 'Modules: %d';

procedure TModulesDumpForm.BuildContent;
begin
  BuildModulesList;
end;

procedure TModulesDumpForm.BuildModulesList;
type
  TProcessData = packed record
    UsageCnt: Word;
    RelocateCnt: Word;
    {$IFDEF CPU64}
    //Padding byte to workaround typecast error
    Padding: DWORD;
    {$ENDIF CPU64}
  end;
var
  ML: TStringList;
  SnapProcHandle, SnapModuleHandle: THandle;
  ProcessEntry: TProcessEntry32;
  ModuleEntry: TModuleEntry32;
  ProcessNext, ModuleNext: Boolean;
  I: Integer;
  PD: TProcessData;
begin
  ML := TStringList.Create;
  Screen.Cursor := crHourGlass;
  try
    ML.Sorted := True;
    ML.Duplicates := dupIgnore;

    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if SnapProcHandle <> THandle(-1) then
    begin
      ProcessEntry.dwSize := Sizeof(ProcessEntry);
      ProcessNext := Process32First(SnapProcHandle, ProcessEntry);
      while ProcessNext do
      begin
        SnapModuleHandle := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, ProcessEntry.th32ProcessID);
        if SnapModuleHandle <> THandle(-1) then
        begin
          ModuleEntry.dwSize := Sizeof(ModuleEntry);
          ModuleNext := Module32First(SnapModuleHandle, ModuleEntry);
          while ModuleNext do
          begin
            I := ML.Add(ModuleEntry.szExePath);
            PD := TProcessData(ML.Objects[I]);
            Inc(PD.UsageCnt);
            if GetImageBase(ModuleEntry.szExePath) <> DWORD(ModuleEntry.modBaseAddr) then
              Inc(PD.RelocateCnt);
            ML.Objects[I] := Pointer(PD);
            ModuleNext := Module32Next(SnapModuleHandle, ModuleEntry);
          end;
          CloseHandle(SnapModuleHandle);
        end;
        ProcessNext := Process32Next(SnapProcHandle, ProcessEntry);
      end;
      CloseHandle(SnapProcHandle);
    end;

    with ModulesListView do
    begin
      Items.BeginUpdate;
      Items.Clear;
      for I := 0 to ML.Count - 1 do
        with Items.Add do
        begin
          Caption := AnsiLowerCase(ExtractFileName(ML[I]));
          PD := TProcessData(ML.Objects[I]);
          if PD.RelocateCnt = 0 then
            ImageIndex := 20
          else
            ImageIndex := 19;  
          with SubItems do
          begin
            Add(IntToStr(PD.UsageCnt));
            if PD.RelocateCnt = 0 then Add('-') else Add(IntToStr(PD.RelocateCnt));
            Add(ML[I]);
          end;  
        end;
       AlphaSort;
       Items.EndUpdate;
    end;

    with StatusBar do
    begin
      Panels.BeginUpdate;
      Panels[0].Text := Format(sModulesCount, [ML.Count]);
      Panels.EndUpdate;
    end;

  finally
    ML.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TModulesDumpForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  ModulesDumpForm := nil;
end;

procedure TModulesDumpForm.FormShow(Sender: TObject);
begin
  inherited;
  PostBuildContentMessage;
end;

function TModulesDumpForm.SelectedFileName: TFileName;
begin
  Result := ModulesListView.Selected.SubItems[2];
end;

procedure TModulesDumpForm.Refresh1Execute(Sender: TObject);
begin
  BuildModulesList;
end;

procedure TModulesDumpForm.ModulesListViewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  LVColumnClick(Column);
end;

procedure TModulesDumpForm.ModulesListViewCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  LVCompare(ModulesListView, Item1, Item2, Compare);
end;

procedure TModulesDumpForm.FileProp1Update(Sender: TObject);
begin
  FileProp1.Enabled := Assigned(ModulesListView.Selected);
end;

procedure TModulesDumpForm.FileProp1Execute(Sender: TObject);
begin
  DisplayPropDialog(Application.Handle, SelectedFileName);
end;

procedure TModulesDumpForm.ModulesListViewInfoTip(Sender: TObject;
  Item: TListItem; var InfoTip: String);
begin
  InfoTip := InfoTipVersionString(Item.SubItems[2]);
end;

procedure TModulesDumpForm.DumpPe1Execute(Sender: TObject);
begin
  GlobalModule.ViewPE(ModulesListView.Selected.SubItems[2]);
end;

procedure TModulesDumpForm.DumpPe1Update(Sender: TObject);
begin
  DumpPe1.Enabled := GlobalModule.PeViewerRegistred and Assigned(ModulesListView.Selected)
end;

end.
