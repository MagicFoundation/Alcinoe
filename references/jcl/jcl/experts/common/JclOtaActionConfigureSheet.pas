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
{ The Original Code is JclOtaActionConfigureSheet.pas.                                             }
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

unit JclOtaActionConfigureSheet;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Dialogs, ComCtrls, StdCtrls;

type
  TJclOtaActionConfigureFrame = class(TFrame)
    ListViewActions: TListView;
    LabelActions: TLabel;
    HotKeyShortcut: THotKey;
    LabelShortcut: TLabel;
    ButtonRestore: TButton;
    procedure HotKeyShortcutExit(Sender: TObject);
    procedure ButtonRestoreClick(Sender: TObject);
    procedure ListViewActionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveChanges;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}

uses
  ActnList, Menus,
  ToolsApi,
  JclOtaConsts, JclOtaResources, JclOtaUtils, JclOtaActions;

{ TFrameActions }

procedure TJclOtaActionConfigureFrame.ButtonRestoreClick(Sender: TObject);
var
  AListItem: TListItem;
  AAction: TAction;
begin
  AListItem := ListViewActions.Selected;
  if Assigned(AListItem) then
  begin
    AAction := TJclOTAActionExpert.GetAction(AListItem.Index);
    AListItem.SubItems.Strings[0] := ShortcutToText(TShortcut(AAction.Tag));
    HotKeyShortcut.HotKey := TShortcut(AAction.Tag);
  end;
end;

constructor TJclOtaActionConfigureFrame.Create(AOwner: TComponent);
var
  Index: Integer;
  ANTAServices: INTAServices;
  AListItem: TListItem;
  AAction: TAction;
begin
  inherited Create(AOwner);

  ButtonRestore.Caption := LoadResString(@RsRestore);
  LabelActions.Caption := LoadResString(@RsActions);
  LabelShortcut.Caption := LoadResString(@RsShortcut);
  ListViewActions.Columns.Items[0].Caption := LoadResString(@RsCaption);
  ListViewActions.Columns.Items[1].Caption := LoadResString(@RsShortcut);

  Supports(BorlandIDEServices, INTAServices, ANTAServices);
  if not Assigned(ANTAServices) then
    raise EJclExpertException.CreateRes(@RsENoNTAServices);

  ListViewActions.SmallImages := ANTAServices.ImageList;

  for Index := 0 to TJclOTAActionExpert.GetActionCount - 1 do
  begin
    AListItem := ListViewActions.Items.Add;
    AAction := TJclOTAActionExpert.GetAction(Index);
    AListItem.ImageIndex := AAction.ImageIndex;
    AListItem.Caption := AAction.Caption;
    AListItem.Data := Pointer(AAction.ShortCut);
    AListItem.SubItems.Add(ShortcutToText(AAction.ShortCut));
  end;
end;

procedure TJclOtaActionConfigureFrame.HotKeyShortcutExit(Sender: TObject);
var
  AListItem: TListItem;
begin
  AListItem := ListViewActions.Selected;
  if Assigned(AListItem) then
  begin
    AListItem.Data := Pointer(HotKeyShortcut.HotKey);
    AListItem.SubItems.Strings[0] := ShortCutToText(HotKeyShortcut.HotKey);
  end;
end;

procedure TJclOtaActionConfigureFrame.ListViewActionsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    HotKeyShortcut.HotKey := TShortcut(Item.Data)
  else
    HotKeyShortcut.HotKey := scNone;
end;

procedure TJclOtaActionConfigureFrame.SaveChanges;
var
  Index: Integer;
begin
  { (ahuser) In Delphi 7 the ListViewActions.Items.Count is 0 if the page was
    not shown. Something must delete the items that were filled in the constructor. }
  if ListViewActions.Items.Count = TJclOTAActionExpert.GetActionCount then
  begin
    for Index := 0 to TJclOTAActionExpert.GetActionCount - 1 do
      TJclOTAActionExpert.GetAction(Index).ShortCut :=
        TShortcut(ListViewActions.Items.Item[Index].Data);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
