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
{ The Original Code is PeSearch.pas.                                                               }
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

unit PeSearch;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, JclPeImage, ExtCtrls;

type
  TPeSearchChild = class(TForm)
    FuncNameEdit: TEdit;
    ResultListView: TListView;
    StartBtn: TButton;
    ProcessLabel: TLabel;
    StopBtn: TButton;
    Bevel1: TBevel;
    PathEdit: TEdit;
    CountLabel: TLabel;
    SelectDirBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ExportCheckBox: TCheckBox;
    ImportCheckBox: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure StartBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SelectDirBtnClick(Sender: TObject);
    procedure FuncNameEditChange(Sender: TObject);
    procedure ResultListViewDblClick(Sender: TObject);
  private
    FSearchThread: TJclPeNameSearch;
    procedure SearchDone(Sender: TObject);
    procedure SearchFound(Sender: TObject; const FileName: TFileName;
      const FunctionName: string; Option: TJclPeNameSearchOption);
    procedure SearchProcessFile(Sender: TObject; PeImage: TJclPeImage; var Process: Boolean);
    procedure UpdateCounter;
    procedure UpdateButtons;
  public
    function ActiveLibName: string;
    procedure ClearResults;
    procedure StartSearch;
    procedure StopSearch;
  end;

var
  PeSearchChild: TPeSearchChild;

implementation

{$R *.DFM}

uses
  FileCtrl, JclSysInfo, PeViewerMain;

procedure TPeSearchChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FSearchThread) then
  begin
    FSearchThread.OnFound := nil;
    FSearchThread.OnProcessFile := nil;
    FSearchThread.OnTerminate := nil;
    FSearchThread.Terminate;
  end;
  Action := caFree;
end;

procedure TPeSearchChild.SearchDone(Sender: TObject);
begin
  FSearchThread := nil;
  UpdateButtons;
  ProcessLabel.Caption := '';
end;

procedure TPeSearchChild.SearchFound(Sender: TObject; const FileName: TFileName;
  const FunctionName: string; Option: TJclPeNameSearchOption);
begin
  with ResultListView.Items.Add do
  begin
    Caption := FunctionName;
    SubItems.Add(FileName);
    case Option of
      seImports: ImageIndex := icoImports;
      seDelayImports: ImageIndex := icoDelayImport;
      seBoundImports: ImageIndex := icoBoundImport;
      seExports: ImageIndex := icoExports;
    end;
  end;
  UpdateCounter;
end;

procedure TPeSearchChild.SearchProcessFile(Sender: TObject; PeImage: TJclPeImage; var Process: Boolean);
begin
  ProcessLabel.Caption := PeImage.FileName;
end;

procedure TPeSearchChild.StartSearch;
var
  Options: TJclPeNameSearchOptions;
begin
  Options := [];
  if ExportCheckBox.Checked then Include(Options, seExports);
  if ImportCheckBox.Checked then Options := Options + [seImports, seDelayImports, seBoundImports];
  FSearchThread := TJclPeNameSearch.Create(Trim(FuncNameEdit.Text),
    PathEdit.Text, Options);
  FSearchThread.OnTerminate := SearchDone;
  FSearchThread.OnFound := SearchFound;
  FSearchThread.OnProcessFile := SearchProcessFile;
  UpdateButtons;
  ClearResults;
  {$IFDEF RTL230_UP}
  FSearchThread.Start;
  {$ELSE ~RTL230_UP}
  FSearchThread.Resume;
  {$ENDIF ~RTL230_UP}
end;

procedure TPeSearchChild.StopSearch;
begin
  FSearchThread.Terminate;
end;

procedure TPeSearchChild.StartBtnClick(Sender: TObject);
begin
  StartSearch;
end;

procedure TPeSearchChild.StopBtnClick(Sender: TObject);
begin
  StopSearch;
end;

procedure TPeSearchChild.FormCreate(Sender: TObject);
begin
  ProcessLabel.Caption := '';
  PathEdit.Text := GetWindowsSystemFolder;
  UpdateButtons;
  UpdateCounter;
end;

procedure TPeSearchChild.SelectDirBtnClick(Sender: TObject);
var
  S: string;
begin
  if SelectDirectory('', '', S) then PathEdit.Text := S;
end;

procedure TPeSearchChild.ClearResults;
begin
  with ResultListView.Items do
  begin
    BeginUpdate;
    Clear;
    EndUpdate;
  end;
  UpdateCounter;
end;

procedure TPeSearchChild.UpdateCounter;
begin
  with ResultListView.Items do
    if Count = 0 then
      CountLabel.Caption := ''
    else
      CountLabel.Caption := Format('%d', [Count]);
end;

procedure TPeSearchChild.UpdateButtons;
begin
  StartBtn.Enabled := (FuncNameEdit.Text <> '') and (PathEdit.Text <> '') and
    (ImportCheckBox.Checked or ExportCheckBox.Checked) and
    not Assigned(FSearchThread);
  StopBtn.Enabled := Assigned(FSearchThread);
  FuncNameEdit.Enabled := not Assigned(FSearchThread);
  PathEdit.Enabled := not Assigned(FSearchThread);
  SelectDirBtn.Enabled := not Assigned(FSearchThread);
  ExportCheckBox.Enabled := not Assigned(FSearchThread);
  ImportCheckBox.Enabled := not Assigned(FSearchThread);
end;

procedure TPeSearchChild.FuncNameEditChange(Sender: TObject);
begin
  UpdateButtons;
end;

function TPeSearchChild.ActiveLibName: string;
begin
  if ResultListView.Selected <> nil then
    Result := ResultListView.Selected.SubItems[0]
  else
    Result := '';
end;

procedure TPeSearchChild.ResultListViewDblClick(Sender: TObject);
begin
  MainForm.OpenLibrary1.Execute;
end;

end.
