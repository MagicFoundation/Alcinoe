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
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaConfigurationForm;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOtaUtils;

type
  TJclOtaOptionsForm = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    PanelName: TPanel;
    PanelTree: TPanel;
    PanelOptions: TPanel;
    SplitterSep: TSplitter;
    TreeViewCategories: TTreeView;
    LabelSelectPage: TLabel;
    LabelHomePage: TLabel;
    procedure LabelHomePageClick(Sender: TObject);
    procedure TreeViewCategoriesChange(Sender: TObject; Node: TTreeNode);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeViewCategoriesChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
  private
    FSettings: TJclOTASettings;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddPage(Expert: IJclOTAOptionsCallback);
    function Execute(PageName: string): Boolean;
    property Settings: TJclOTASettings read FSettings;
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
  ShellApi,
  JclOtaConsts, JclOtaResources;

type
  TItemDataRec = class
  public
    Frame: TCustomFrame;
    Expert: IJclOTAOptionsCallback;
  end;

//=== TJclOtaOptionsForm =====================================================

procedure TJclOtaOptionsForm.AddPage(Expert: IJclOTAOptionsCallback);
var
  ParentNode, ChildNode: TTreeNode;
  PageName, NodeName: string;
  PosSeparator, Index: Integer;
  AItemDataRec: TItemDataRec;
begin
  ParentNode := TreeViewCategories.Items.GetFirstNode;
  ChildNode := ParentNode;
  PageName := Expert.GetPageName;

  repeat
    PosSeparator := Pos('\', PageName);
    if PosSeparator > 0 then
    begin
      NodeName := Copy(PageName, 1, PosSeparator - 1);
      PageName := Copy(PageName, PosSeparator + 1, Length(PageName) - PosSeparator);
      while Assigned(ChildNode) and (CompareText(NodeName, ChildNode.Text) <> 0) do
        ChildNode := ChildNode.getNextSibling;
      if not Assigned(ChildNode) then
      begin
        ChildNode := TreeViewCategories.Items.AddChild(ParentNode, NodeName);
        if Assigned(ParentNode) then
          ParentNode.Expand(False);
      end;
      ParentNode := ChildNode;
    end
    else
    begin
      while Assigned(ParentNode) and (CompareText(NodeName, ParentNode.Text) <> 0) do
        ParentNode := ParentNode.getNextSibling;
    end;
  until PosSeparator = 0;

  ChildNode := nil;
  if Assigned(ParentNode) then
    for Index := 0 to ParentNode.Count - 1 do
      if CompareText(ParentNode.Item[Index].Text, PageName) = 0 then
        ChildNode := ParentNode.Item[Index];

  if not Assigned(ChildNode) then
  begin
    ChildNode := TreeViewCategories.Items.AddChild(ParentNode, PageName);
    if Assigned(ParentNode) then
      ParentNode.Expand(False);
  end;

  AItemDataRec := TItemDataRec.Create;
  AItemDataRec.Frame := nil;
  AItemDataRec.Expert := Expert;
  ChildNode.Data := Pointer(AItemDataRec);
end;

constructor TJclOtaOptionsForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSettings := TJclOTASettings.Create(JclConfigurationSettings);
end;

procedure TJclOtaOptionsForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  // Fixing the Window Ghosting "bug"
  Params.Style := params.Style or WS_POPUP;
  if Assigned(Screen.ActiveForm) then
    Params.WndParent := Screen.ActiveForm.Handle
  else if Assigned (Application.MainForm) then
    Params.WndParent := Application.MainForm.Handle
  else
    Params.WndParent := Application.Handle;
end;

destructor TJclOtaOptionsForm.Destroy;
begin
  FreeAndNil(FSettings);
  inherited Destroy;
end;

function TJclOtaOptionsForm.Execute(PageName: string): Boolean;
var
  ATreeNode: TTreeNode;
  NodeName: string;
  PosSeparator: Integer;
  AItemDataRec: TItemDataRec;
begin
  ATreeNode := TreeViewCategories.Items.GetFirstNode;

  repeat
    PosSeparator := Pos('\', PageName);
    if PosSeparator > 0 then
    begin
      NodeName := Copy(PageName, 1, PosSeparator - 1);
      PageName := Copy(PageName, PosSeparator + 1, Length(PageName) - PosSeparator);
      while Assigned(ATreeNode) and (CompareText(NodeName, ATreeNode.Text) <> 0) do
        ATreeNode := ATreeNode.getNextSibling;
      if Assigned(ATreeNode) then
        ATreeNode := ATreeNode.getFirstChild;
    end
    else
    begin
      while Assigned(ATreeNode) and (CompareText(PageName, ATreeNode.Text) <> 0) do
        ATreeNode := ATreeNode.getNextSibling;
    end;
  until PosSeparator = 0;

  if not Assigned(ATreeNode) then
    ATreeNode := TreeViewCategories.Items.GetFirstNode;
  if Assigned(ATreeNode) then
    TreeViewCategories.Selected := ATreeNode;

  Result := ShowModal = mrOk;

  ATreeNode := TreeViewCategories.Items.GetFirstNode;
  while Assigned(ATreeNode) do
  begin
    AItemDataRec := TItemDataRec(ATreeNode.Data);
    if Assigned(AItemDataRec) then
    begin
      AItemDataRec.Expert.DialogClosed(Result);
      AItemDataRec.Frame.Free;
      AItemDataRec.Free;
    end;
    ATreeNode := ATreeNode.GetNext;
  end;
end;

procedure TJclOtaOptionsForm.FormCreate(Sender: TObject);
begin
  Caption := LoadResString(@RsConfigurationCaption);
  ButtonOk.Caption := LoadResString(@RsOk);
  ButtonCancel.Caption := LoadResString(@RsCancel);
  LabelSelectPage.Caption := LoadResString(@RsSelectPage);
  LabelHomePage.Caption := LoadResString(@RsHomePage);

  SetBounds(Settings.LoadInteger(JclLeft, Left),
            Settings.LoadInteger(JclTop, Top),
            Settings.LoadInteger(JclWidth, Width),
            Settings.LoadInteger(JclHeight, Height));
  PanelTree.Width := Settings.LoadInteger(JclPanelTreeWidth, PanelTree.Width);
end;

procedure TJclOtaOptionsForm.FormDestroy(Sender: TObject);
begin
  Settings.SaveInteger(JclLeft, Left);
  Settings.SaveInteger(JclTop, Top);
  Settings.SaveInteger(JclWidth, Width);
  Settings.SaveInteger(JclHeight, Height);
  Settings.SaveInteger(JclPanelTreeWidth, PanelTree.Width);
end;

procedure TJclOtaOptionsForm.LabelHomePageClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(LoadResString(@RsHomePageURL)), '', '', SW_SHOW);
end;

procedure TJclOtaOptionsForm.TreeViewCategoriesChange(Sender: TObject;
  Node: TTreeNode);
var
  AItemDataRec: TItemDataRec;
  AFrame: TCustomFrame;
  AControl: TControl;
  Index: Integer;
begin
  AItemDataRec := TItemDataRec(Node.Data);
  if Assigned(AItemDataRec) then
  begin
    AFrame := AItemDataRec.Frame;
    if not Assigned(AFrame) then
    begin
      AFrame := AItemDataRec.Expert.GetFrameClass.Create(Self);
      AFrame.Parent := PanelOptions;
      AFrame.SetBounds(8, 8, PanelOptions.ClientWidth - 16, PanelOptions.ClientHeight - 16);
      AFrame.Visible := False;
      AItemDataRec.Expert.FrameCreated(AFrame);
      AItemDataRec.Frame := AFrame;
    end;
    AControl := AFrame;
  end
  else
    AControl := LabelSelectPage;
  for Index := 0 to PanelOptions.ControlCount - 1 do
    PanelOptions.Controls[Index].Visible := PanelOptions.Controls[Index] = AControl;
end;

procedure TJclOtaOptionsForm.TreeViewCategoriesChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
var
  AItemDataRec: TItemDataRec;
begin
  AItemDataRec := TItemDataRec(Node.Data);
  if Assigned(AItemDataRec) then
    AllowChange := AItemDataRec.Expert.ValidateContents
  else
    AllowChange := True;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
