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
{ The Original Code is ViewTemplate.pas.                                                           }
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

unit ViewTemplate;

{$I JCL.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolWin, ComCtrls, ActnList, Menus;

const
  UM_BUILD = WM_USER + $100;

type
  TViewForm = class(TForm)
    CoolBar: TCoolBar;
    ToolBar: TToolBar;
    ActionList: TActionList;
    PopupMenu: TPopupMenu;
    TextLabels1: TAction;
    ToolBarPopupMenu: TPopupMenu;
    Textlabels2: TMenuItem;
    Copy1: TAction;
    SaveToFile1: TAction;
    Refresh1: TAction;
    SelectAll1: TAction;
    Find1: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure TextLabels1Execute(Sender: TObject);
    procedure SelectAll1Update(Sender: TObject);
    procedure SelectAll1Execute(Sender: TObject);
    procedure Copy1Update(Sender: TObject);
    procedure Copy1Execute(Sender: TObject);
    procedure SaveToFile1Execute(Sender: TObject);
    procedure Find1Update(Sender: TObject);
    procedure Find1Execute(Sender: TObject);
  private
    procedure UpdateTextLabels;
    procedure UMBuild(var Msg: TMessage); message UM_BUILD;
  public
    procedure BuildContent; dynamic; abstract;
    procedure PostBuildContentMessage;
  end;

var
  ViewForm: TViewForm;

implementation

uses Main, Global, ToolsUtils, About, FindDlg;

{$R *.DFM}

procedure TViewForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MainForm.DeleteFromViewsMenu(Self);
  Action := caFree;
end;

procedure TViewForm.FormShow(Sender: TObject);
begin
  MainForm.AddToViewsMenu(Self, Caption);
end;

procedure TViewForm.TextLabels1Execute(Sender: TObject);
begin
  with TextLabels1 do Checked := not Checked;
  UpdateTextLabels;
end;

procedure TViewForm.UpdateTextLabels;
begin
  ToolBar.ShowCaptions := TextLabels1.Checked;
  if not ToolBar.ShowCaptions then
  begin
    ToolBar.ButtonHeight := 0;
    ToolBar.ButtonWidth := 0;
  end;
end;

procedure TViewForm.Copy1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveControl is TListView;
end;

procedure TViewForm.SelectAll1Update(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (ActiveControl is TListView) and TListView(ActiveControl).MultiSelect;
end;

procedure TViewForm.SelectAll1Execute(Sender: TObject);
begin
  ListViewSelectAll(ActiveControl as TListView);
end;

procedure TViewForm.Copy1Execute(Sender: TObject);
begin
  GlobalModule.ListViewToClipboard(ActiveControl as TListView);
end;

procedure TViewForm.SaveToFile1Execute(Sender: TObject);
begin
  GlobalModule.ListViewToFile(ActiveControl as TListView, Caption);
end;

procedure TViewForm.UMBuild(var Msg: TMessage);
begin
  Update;
  BuildContent;
end;

procedure TViewForm.PostBuildContentMessage;
begin
  PostMessage(Handle, UM_BUILD, 0, 0);
end;

procedure TViewForm.Find1Update(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (ActiveControl is TListView) and not TListView(ActiveControl).HideSelection;
end;

procedure TViewForm.Find1Execute(Sender: TObject);
begin
  ShowFindDialog(ActiveControl as TListView);
end;

end.
