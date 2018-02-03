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
{ The Original Code is JclVersionCtrlCommonOptions.pas                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet.                          }
{                                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                        $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclVersionCtrlCommonOptions;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Dialogs, StdCtrls, ComCtrls, ActnList, Menus;

type
  TIconType = (itNone, itJCL);

  TJclVersionCtrlOptionsFrame = class(TFrame)
    CheckBoxHideActions: TCheckBox;
    LabelIcons: TLabel;
    ComboBoxIcons: TComboBox;
    TreeViewMenu: TTreeView;
    LabelMenuOrganization: TLabel;
    CheckBoxDisableActions: TCheckBox;
    ButtonNewSeparator: TButton;
    ButtonDelete: TButton;
    ButtonRename: TButton;
    ButtonMoveUp: TButton;
    ButtonMoveDown: TButton;
    ActionListVersionCtrl: TActionList;
    ActionNewSeparator: TAction;
    ActionDeleteItem: TAction;
    ActionRenameItem: TAction;
    ActionMoveItemUp: TAction;
    ActionMoveItemDown: TAction;
    CheckBoxSaveConfirmation: TCheckBox;
    PopupMenuActions: TPopupMenu;
    ActionNewAction: TAction;
    ButtonNewAction: TButton;
    ActionNewSubMenu: TAction;
    ButtonNewSubMenu: TButton;
    CheckBoxActOnTopSandbox: TCheckBox;
    procedure ActionActOnTopSandboxUpdate(Sender: TObject);
    procedure ActionNewActionExecute(Sender: TObject);
    procedure ActionNewActionUpdate(Sender: TObject);
    procedure ActionRenameItemExecute(Sender: TObject);
    procedure ActionNewSubMenuExecute(Sender: TObject);
    procedure ActionNewSubMenuUpdate(Sender: TObject);
    procedure ActionNewSeparatorExecute(Sender: TObject);
    procedure ActionMoveItemUpExecute(Sender: TObject);
    procedure ActionMoveItemDownExecute(Sender: TObject);
    procedure ActionDeleteItemExecute(Sender: TObject);
    procedure ActionSaveConfirmationUpdate(Sender: TObject);
    procedure ActionRenameItemUpdate(Sender: TObject);
    procedure ActionNewSeparatorUpdate(Sender: TObject);
    procedure ActionMoveItemUpUpdate(Sender: TObject);
    procedure ActionMoveItemDownUpdate(Sender: TObject);
    procedure ActionHideUnSupportedActionsUpdate(Sender: TObject);
    procedure ActionDisableActionsUpdate(Sender: TObject);
    procedure ActionDeleteItemUpdate(Sender: TObject);
    procedure TreeViewMenuEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure TreeViewMenuEdited(Sender: TObject; Node: TTreeNode; var S: string);
  private
    FMenuTree: TStrings;
    function GetActOnTopSandbox: Boolean;
    procedure SetActOnTopSandbox(const Value: Boolean);
    function GetSaveConfirmation: Boolean;
    procedure SetSaveConfirmation(const Value: Boolean);
    function GetDisableActions: Boolean;
    function GetHideActions: Boolean;
    function GetIconType: TIconType;
    function GetMenuTree: TStrings;
    procedure SetDisableActions(const Value: Boolean);
    procedure SetHideActions(const Value: Boolean);
    procedure SetIconType(const Value: TIconType);
    procedure SetMenuTree(const Value: TStrings);
    procedure MenuItemNewActionClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetActions(const Actions: array of TCustomAction);
    property ActOnTopSandbox: Boolean read GetActOnTopSandbox write SetActOnTopSandbox;
    property DisableActions: Boolean read GetDisableActions write SetDisableActions;
    property HideActions: Boolean read GetHideActions write SetHideActions;
    property IconType: TIconType read GetIconType write SetIconType;
    property MenuTree: TStrings read GetMenuTree write SetMenuTree;
    property SaveConfirmation: Boolean read GetSaveConfirmation write SetSaveConfirmation;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\versioncontrol';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}

uses
  TypInfo, ToolsAPI,
  JclStrings, JclVersionControl,
  JclOtaUtils, JclOtaResources, JclVersionControlImpl;

//=== TJclVersionCtrlOptionsFrame ============================================

procedure TJclVersionCtrlOptionsFrame.ActionActOnTopSandboxUpdate(
  Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TJclVersionCtrlOptionsFrame.ActionDeleteItemExecute(Sender: TObject);
var
  ATreeNode: TTreeNode;
begin
  ATreeNode := TreeViewMenu.Selected;
  if Assigned(ATreeNode) then
    ATreeNode.Delete;
end;

procedure TJclVersionCtrlOptionsFrame.ActionDeleteItemUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(TreeViewMenu.Selected);
end;

procedure TJclVersionCtrlOptionsFrame.ActionDisableActionsUpdate(
  Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TJclVersionCtrlOptionsFrame.ActionHideUnSupportedActionsUpdate(
  Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TJclVersionCtrlOptionsFrame.ActionMoveItemDownExecute(
  Sender: TObject);
var
  ATreeNode, BTreeNode: TTreeNode;
begin
  ATreeNode := TreeViewMenu.Selected;
  BTreeNode := ATreeNode.getNextSibling;
  if Assigned(BTreeNode) then
    BTreeNode.MoveTo(ATreeNode, naInsert);
end;

procedure TJclVersionCtrlOptionsFrame.ActionMoveItemDownUpdate(Sender: TObject);
var
  ATreeNode: TTreeNode;
begin
  ATreeNode := TreeViewMenu.Selected;
  if Assigned(ATreeNode) then
    ATreeNode := ATreeNode.getNextSibling;
  TAction(Sender).Enabled := Assigned(ATreeNode);
end;

procedure TJclVersionCtrlOptionsFrame.ActionMoveItemUpExecute(Sender: TObject);
var
  ATreeNode, BTreeNode: TTreeNode;
begin
  ATreeNode := TreeViewMenu.Selected;
  BTreeNode := ATreeNode.getPrevSibling;
  ATreeNode.MoveTo(BTreeNode, naInsert);
end;

procedure TJclVersionCtrlOptionsFrame.ActionMoveItemUpUpdate(Sender: TObject);
var
  ATreeNode: TTreeNode;
begin
  ATreeNode := TreeViewMenu.Selected;
  if Assigned(ATreeNode) then
    ATreeNode := ATreeNode.getPrevSibling;
  TAction(Sender).Enabled := Assigned(ATreeNode);
end;

procedure TJclVersionCtrlOptionsFrame.ActionNewActionExecute(Sender: TObject);
var
  APoint: TPoint;
begin
  APoint.X := 0;
  APoint.Y := ButtonNewAction.Height;
  APoint := ButtonNewAction.ClientToScreen(APoint);
  PopupMenuActions.Popup(APoint.X, APoint.Y);
end;

procedure TJclVersionCtrlOptionsFrame.ActionNewActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TJclVersionCtrlOptionsFrame.ActionNewSubMenuExecute(
  Sender: TObject);
var
  ATreeNode, NewTreeNode: TTreeNode;
begin
  ATreeNode := TreeViewMenu.Selected;

  if Assigned(ATreeNode) and Assigned(ATreeNode.Parent) then
    ATreeNode := ATreeNode.Parent;

  if Assigned(ATreeNode) and (ATreeNode.getNextSibling <> nil) then
    NewTreeNode := TreeViewMenu.Items.Insert(ATreeNode.getNextSibling, LoadResString(@RsNewItem))
  else
    NewTreeNode := TreeViewMenu.Items.Add(ATreeNode, LoadResString(@RsNewItem));

  NewTreeNode.ImageIndex := -1;
  NewTreeNode.SelectedIndex := -1;
  NewTreeNode.Data := nil;

  NewTreeNode.EditText;
end;

procedure TJclVersionCtrlOptionsFrame.ActionNewSubMenuUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TJclVersionCtrlOptionsFrame.ActionNewSeparatorExecute(Sender: TObject);
var
  ATreeNode, NewTreeNode: TTreeNode;
begin
  ATreeNode := TreeViewMenu.Selected;

  if Assigned(ATreeNode) and (ATreeNode.getNextSibling <> nil) then
    NewTreeNode := TreeViewMenu.Items.Insert(ATreeNode.getNextSibling, '-')
  else
    NewTreeNode := TreeViewMenu.Items.Add(ATreeNode, '-');

  NewTreeNode.ImageIndex := -1;
  NewTreeNode.SelectedIndex := -1;
  NewTreeNode.Data := nil;
end;

procedure TJclVersionCtrlOptionsFrame.ActionNewSeparatorUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(TreeViewMenu.Selected);
end;

procedure TJclVersionCtrlOptionsFrame.ActionRenameItemExecute(Sender: TObject);
var
  ATreeNode: TTreeNode;
begin
  ATreeNode := TreeViewMenu.Selected;
  if Assigned(ATreeNode) then
    ATreeNode.EditText;
end;

procedure TJclVersionCtrlOptionsFrame.ActionRenameItemUpdate(Sender: TObject);
var
  ATreeNode: TTreeNode;
begin
  ATreeNode := TreeViewMenu.Selected;
  TAction(Sender).Enabled := Assigned(ATreeNode) and (ATreeNode.Text <> '-')
    and not Assigned(ATreeNode.Data);
end;

procedure TJclVersionCtrlOptionsFrame.ActionSaveConfirmationUpdate(
  Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

constructor TJclVersionCtrlOptionsFrame.Create(AOwner: TComponent);
var
  NTAServices: INTAServices;
begin
  inherited Create(AOwner);
  FMenuTree := TStringList.Create;

  Supports(BorlandIDEServices, INTAServices, NTAServices);
  if not Assigned(NTAServices) then
    raise EJclExpertException.CreateRes(@RsENoNTAServices);
    
  TreeViewMenu.Images := NTAServices.ImageList;
  PopupMenuActions.Images := NTAServices.ImageList;

  CheckBoxActOnTopSandbox.Caption := LoadResString(@RsActOnTopSandBox);
  CheckBoxDisableActions.Caption := LoadResString(@RsDisableActions);
  CheckBoxHideActions.Caption := LoadResString(@RsHideUnsupportedActions);
  CheckBoxSaveConfirmation.Caption := LoadResString(@RsSaveConfirmation);
  ActionNewSubMenu.Caption := LoadResString(@RsNewSubMenu);
  ActionNewSeparator.Caption := LoadResString(@RsNewSeparator);
  ActionNewAction.Caption := LoadResString(@RsNewAction);
  ActionDeleteItem.Caption := LoadResString(@RsDeleteItem);
  ActionRenameItem.Caption := LoadResString(@RsRenameItem);
  ActionMoveItemUp.Caption := LoadResString(@RsMoveItemUp);
  ActionMoveItemDown.Caption := LoadResString(@RsMoveItemDown);
  LabelIcons.Caption := LoadResString(@RsIcons);
  LabelMenuOrganization.Caption := LoadResString(@RsMenuOrganization);
  ComboBoxIcons.Items.Strings[0] := LoadResString(@RsNoIcon);
  ComboBoxIcons.Items.Strings[1] := LoadResString(@RsJCLIcons);
end;

destructor TJclVersionCtrlOptionsFrame.Destroy;
begin
  FMenuTree.Free;
  inherited Destroy;
end;

function TJclVersionCtrlOptionsFrame.GetActOnTopSandbox: Boolean;
begin
  Result := CheckBoxActOnTopSandbox.Checked;
end;

function TJclVersionCtrlOptionsFrame.GetDisableActions: Boolean;
begin
  Result := CheckBoxDisableActions.Checked;
end;

function TJclVersionCtrlOptionsFrame.GetHideActions: Boolean;
begin
  Result := CheckBoxHideActions.Checked;
end;

function TJclVersionCtrlOptionsFrame.GetIconType: TIconType;
begin
  if ComboBoxIcons.ItemIndex = 1 then
    Result := itJCL
  else
    Result := itNone;
end;

function TJclVersionCtrlOptionsFrame.GetMenuTree: TStrings;
var
  ATreeNode, BTreeNode: TTreeNode;
  ItemName: string;
  AAction: TCustomAction;
  Index: Integer;
begin
  FMenuTree.Clear;
  ATreeNode := TreeViewMenu.Items.GetFirstNode;
  while Assigned(ATreeNode) do
  begin
    AAction := TCustomAction(ATreeNode.Data);
    ItemName := '';
    if Assigned(AAction) then
      for Index := 0 to PopupMenuActions.Items.Count - 1 do
        if TCustomAction(PopupMenuActions.Items.Items[Index].Tag) = AAction then
          ItemName := GetEnumName(TypeInfo(TJclVersionControlActionType), Index);

    if ItemName = '' then
      ItemName := ATreeNode.Text;

    FMenuTree.Add(Format('%d%s', [ATreeNode.Index, ItemName]));

    BTreeNode := ATreeNode.getFirstChild;
    while Assigned(BTreeNode) do
    begin
      AAction := TCustomAction(BTreeNode.Data);
      ItemName := '';
      if Assigned(AAction) then
        for Index := 0 to PopupMenuActions.Items.Count - 1 do
          if TCustomAction(PopupMenuActions.Items.Items[Index].Tag) = AAction then
            ItemName := GetEnumName(TypeInfo(TJclVersionControlActionType), Index);

      if ItemName = '' then
        ItemName := BTreeNode.Text;

      FMenuTree.Add(Format('%d%s%d', [ATreeNode.Index, ItemName, BTreeNode.Index]));

      BTreeNode := BTreeNode.getNextSibling;
    end;
    ATreeNode := ATreeNode.getNextSibling;
  end;
  Result := FMenuTree;
end;

function TJclVersionCtrlOptionsFrame.GetSaveConfirmation: Boolean;
begin
  Result := CheckBoxSaveConfirmation.Checked;
end;

procedure TJclVersionCtrlOptionsFrame.MenuItemNewActionClick(Sender: TObject);
var
  AAction: TCustomAction;
  ATreeNode, NewTreeNode: TTreeNode;
begin
  AAction := TCustomAction((Sender as TMenuItem).Tag);

  ATreeNode := TreeViewMenu.Selected;
  if Assigned(ATreeNode.Data) or (ATreeNode.Text = '-') then
  begin
    if Assigned(ATreeNode) and (ATreeNode.getNextSibling <> nil) then
      NewTreeNode := TreeViewMenu.Items.Insert(ATreeNode.getNextSibling, AAction.Caption)
    else
      NewTreeNode := TreeViewMenu.Items.Add(ATreeNode, AAction.Caption);
  end
  else
  begin
    NewTreeNode := TreeViewMenu.Items.AddChildFirst(ATreeNode, AAction.Caption);
    ATreeNode.Expand(False);
  end;

  NewTreeNode.Data := AAction;
  NewTreeNode.ImageIndex := AAction.ImageIndex;
  NewTreeNode.SelectedIndex := AAction.ImageIndex;
end;

procedure TJclVersionCtrlOptionsFrame.SetActions(
  const Actions: array of TCustomAction);
var
  Index: Integer;
  AMenuItem: TMenuItem;
begin
  for Index := Low(Actions) to High(Actions) do
  begin
    AMenuItem := TMenuItem.Create(Self);
    AMenuItem.Tag := Integer(Actions[Index]);
    AMenuItem.Caption := Actions[Index].Caption;
    AMenuItem.ImageIndex := Actions[Index].ImageIndex;
    AMenuItem.OnClick := MenuItemNewActionClick;
    PopupMenuActions.Items.Add(AMenuItem);
  end;
end;

procedure TJclVersionCtrlOptionsFrame.SetActOnTopSandbox(const Value: Boolean);
begin
  CheckBoxActOnTopSandbox.Checked := Value;
end;

procedure TJclVersionCtrlOptionsFrame.SetDisableActions(const Value: Boolean);
begin
  CheckBoxDisableActions.Checked := Value;
end;

procedure TJclVersionCtrlOptionsFrame.SetHideActions(const Value: Boolean);
begin
  CheckBoxHideActions.Checked := Value;
end;

procedure TJclVersionCtrlOptionsFrame.SetIconType(const Value: TIconType);
begin
  case Value of
    itNone:
      ComboBoxIcons.ItemIndex := 0;
    itJCL:
      ComboBoxIcons.ItemIndex := 1;
  end;
end;

procedure TJclVersionCtrlOptionsFrame.SetMenuTree(const Value: TStrings);
var
  ATreeNode, BTreeNode: TTreeNode;
  Index, IndexB: Integer;
  Item, ItemName: string;
  AAction: Integer;
  ControlAction: TCustomAction;
begin
  TreeViewMenu.Items.Clear;
  ATreeNode := nil;
  for Index := 0 to Value.Count - 1 do
  begin
    Item := Value.Strings[Index];
    IndexB := GetItemIndexB(Item);
    ItemName := GetItemName(Item);
    AAction := GetEnumValue(TypeInfo(TJclVersionControlActionType), ItemName);

    if IndexB = -1 then
    begin
      if (AAction = -1) or (ItemName = '-') then
      begin
        ATreeNode := TreeViewMenu.Items.Add(nil, ItemName);
        ATreeNode.ImageIndex := -1;
        ATreeNode.SelectedIndex := -1;
        ATreeNode.Data := nil;
      end
      else
      begin
        ControlAction := TCustomAction(PopupMenuActions.Items.Items[AAction].Tag);
        ATreeNode := TreeViewMenu.Items.Add(nil, StrRemoveChars(ControlAction.Caption, CharIsAmpersand));
        ATreeNode.Data := ControlAction;
        ATreeNode.ImageIndex := ControlAction.ImageIndex;
        ATreeNode.SelectedIndex := ControlAction.ImageIndex;
        ATreeNode := nil;
      end;
    end
    else
    begin
      if not Assigned(ATreeNode) then
        Abort;

      if (AAction = -1) or (ItemName = '-') then
      begin
        BTreeNode := TreeViewMenu.Items.AddChild(ATreeNode, ItemName);
        BTreeNode.ImageIndex := -1;
        BTreeNode.SelectedIndex := -1;
        BTreeNode.Data := nil;
      end
      else
      begin
        ControlAction := TCustomAction(PopupMenuActions.Items.Items[AAction].Tag);
        BTreeNode := TreeViewMenu.Items.AddChild(ATreeNode, StrRemoveChars(ControlAction.Caption, CharIsAmpersand));
        BTreeNode.ImageIndex := ControlAction.ImageIndex;
        BTreeNode.SelectedIndex := ControlAction.ImageIndex;
        BTreeNode.Data := ControlAction;
      end;
      ATreeNode.Expand(False);
    end;
  end;
end;

procedure TJclVersionCtrlOptionsFrame.SetSaveConfirmation(const Value: Boolean);
begin
  CheckBoxSaveConfirmation.Checked := Value;
end;

function CharIsInvalid(const C: Char): Boolean;
begin
  case C of
    '\', '_', '0'..'9':
      Result := True;
  else
    Result := False;
  end;
end;

procedure TJclVersionCtrlOptionsFrame.TreeViewMenuEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
begin
  if StrContainsChars(S, CharIsInvalid, True) then
  begin
    S := Node.Text;
    MessageDlg(LoadResString(@RsEInvalidMenuCaption), mtError, [mbAbort], 0);
  end;
end;

procedure TJclVersionCtrlOptionsFrame.TreeViewMenuEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  AllowEdit := Assigned(Node) and (Node.Text <> '-') and not Assigned(Node.Data);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
