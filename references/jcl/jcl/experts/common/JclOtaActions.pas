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
{ The Original Code is JclOtaUtils.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.                                  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaActions;

interface

{$I jcl.inc}
{$I crossplatform.inc}

uses
  SysUtils, Classes, Windows,
  Controls, ComCtrls, ActnList, Menus, Forms, Graphics,
  {$IFNDEF COMPILER8_UP}
  Idemenuaction, // dependency walker reports a class TPopupAction in
  // unit Idemenuaction in designide.bpl used by the IDE to display tool buttons
  // with a drop down menu, this class seems to have the same interface
  // as TControlAction defined in Controls.pas for newer versions of Delphi
  {$ENDIF COMPILER8_UP}
  JclBase,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  ToolsAPI,
  JclOTAUtils;

type
  // class of actions with a drop down menu on tool bars
  {$IFDEF COMPILER8_UP}
  TDropDownAction = TControlAction;
  {$ELSE COMPILER8_UP}
  TDropDownAction = TPopupAction;
  {$ENDIF COMPILER8_UP}

  TJclOTAActionExpert = class(TJclOTAExpert)
  private
    FConfigurationAction: TAction;
    FConfigurationMenuItem: TMenuItem;
    FActionConfigureSheet: TControl;
    procedure ConfigurationActionUpdate(Sender: TObject);
    procedure ConfigurationActionExecute(Sender: TObject);
  public
    class procedure CheckToolBarButton(AToolBar: TToolBar; AAction: TCustomAction);
    class procedure RegisterAction(Action: TCustomAction);
    class procedure UnregisterAction(Action: TCustomAction);
    class function GetActionCount: Integer;
    class function GetAction(Index: Integer): TAction;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure RegisterCommands; override;
    procedure UnregisterCommands; override;
  public
    function GetPageName: string; override;
    function GetFrameClass: TCustomFrameClass; override;
    procedure FrameCreated(AFrame: TCustomFrame); override;
    procedure DialogClosed(Accepted: Boolean); override;
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

uses
  JclOtaConsts, JclOtaResources,
  JclOtaActionConfigureSheet;

var
  GlobalActionList: TList = nil;
  GlobalActionExpert: TJclOTAActionExpert;

function FindActions(const Name: string): TComponent;
var
  Index: Integer;
  TestAction: TCustomAction;
  ActionList: TList;
begin
  ActionList := GlobalActionList;
  Result := nil;
  try
    if Assigned(ActionList) then
      for Index := 0 to ActionList.Count-1 do
      begin
        TestAction := TCustomAction(ActionList.Items[Index]);
        if (CompareText(Name,TestAction.Name) = 0) then
          Result := TestAction;
      end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

//=== { TJclOTAActionExpert } ================================================

constructor TJclOTAActionExpert.Create;
begin
  inherited Create(JclActionSettings);
  GlobalActionExpert := Self;
end;

destructor TJclOTAActionExpert.Destroy;
begin
  GlobalActionExpert := nil;
  inherited Destroy;
end;

procedure TJclOTAActionExpert.DialogClosed(Accepted: Boolean);
begin
  if Accepted then
    TJclOtaActionConfigureFrame(FActionConfigureSheet).SaveChanges;
  FActionConfigureSheet := nil;
end;

procedure TJclOTAActionExpert.FrameCreated(AFrame: TCustomFrame);
begin
  FActionConfigureSheet := AFrame as TJclOtaActionConfigureFrame;
end;

class function TJclOTAActionExpert.GetAction(Index: Integer): TAction;
begin
  if Assigned(GlobalActionList) then
    Result := TAction(GlobalActionList.Items[Index])
  else
    Result := nil;
end;

class function TJclOTAActionExpert.GetActionCount: Integer;
begin
  if Assigned(GlobalActionList) then
    Result := GlobalActionList.Count
  else
    Result := 0;
end;

function TJclOTAActionExpert.GetFrameClass: TCustomFrameClass;
begin
  Result := TJclOtaActionConfigureFrame;
end;

function TJclOTAActionExpert.GetPageName: string;
begin
  Result := LoadResString(@RsActionSheet);
end;

type
  TAccessToolButton = class(TToolButton);

class procedure TJclOTAActionExpert.CheckToolBarButton(AToolBar: TToolBar; AAction: TCustomAction);
var
  Index: Integer;
  AButton: TAccessToolButton;
begin
  if Assigned(AToolBar) then
    for Index := AToolBar.ButtonCount - 1 downto 0 do
    begin
      AButton := TAccessToolButton(AToolBar.Buttons[Index]);
      if AButton.Action = AAction then
      begin
        AButton.SetToolBar(nil);
        AButton.Free;
      end;
    end;
end;

procedure TJclOTAActionExpert.ConfigurationActionExecute(Sender: TObject);
begin
  try
    ConfigurationDialog('');
  except
    on EAbort do
      raise;
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclOTAActionExpert.ConfigurationActionUpdate(Sender: TObject);
begin
  try
    (Sender as TAction).Enabled := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

class procedure TJclOTAActionExpert.RegisterAction(Action: TCustomAction);
begin
  if Action.Name <> '' then
  begin
    Action.Tag := Action.ShortCut;  // to restore settings
    if Assigned(GlobalActionExpert) then
      Action.ShortCut := GlobalActionExpert.Settings.LoadInteger(Action.Name, Action.ShortCut);
  end;

  if not Assigned(GlobalActionList) then
    GlobalActionList := TList.Create;

  GlobalActionList.Add(Action);
end;

class procedure TJclOTAActionExpert.UnregisterAction(Action: TCustomAction);
var
  NTAServices: INTAServices;
begin
  if (Action.Name <> '') and Assigned(GlobalActionExpert) then
    GlobalActionExpert.Settings.SaveInteger(Action.Name, Action.ShortCut);

  if Assigned(GlobalActionList) then
  begin
    GlobalActionList.Remove(Action);
    if (GlobalActionList.Count = 0) then
      FreeAndNil(GlobalActionList);
  end;

  NTAServices := GetNTAServices;
  // remove action from toolbar to avoid crash when recompile package inside the IDE.
  CheckToolBarButton(NTAServices.ToolBar[sCustomToolBar], Action);
  CheckToolBarButton(NTAServices.ToolBar[sStandardToolBar], Action);
  CheckToolBarButton(NTAServices.ToolBar[sDebugToolBar], Action);
  CheckToolBarButton(NTAServices.ToolBar[sViewToolBar], Action);
  CheckToolBarButton(NTAServices.ToolBar[sDesktopToolBar], Action);
  {$IFDEF COMPILER7_UP}
  CheckToolBarButton(NTAServices.ToolBar[sInternetToolBar], Action);
  CheckToolBarButton(NTAServices.ToolBar[sCORBAToolBar], Action);
  {$ENDIF COMPILER7_UP}
end;

procedure TJclOTAActionExpert.RegisterCommands;
var
  JclIcon: TIcon;
  Category: string;
  Index: Integer;
  IDEMenuItem, ToolsMenuItem: TMenuItem;
  NTAServices: INTAServices;
begin
  NTAServices := GetNTAServices;

  Category := '';
  for Index := 0 to NTAServices.ActionList.ActionCount - 1 do
    if CompareText(NTAServices.ActionList.Actions[Index].Name, 'ToolsOptionsCommand') = 0 then
      Category := NTAServices.ActionList.Actions[Index].Category;

  FConfigurationAction := TAction.Create(nil);
  JclIcon := TIcon.Create;
  try
    // not ModuleHInstance because the resource is in JclBaseExpert.bpl
    JclIcon.Handle := LoadIcon(HInstance, 'JCLCONFIGURE');
    FConfigurationAction.ImageIndex := NTAServices.ImageList.AddIcon(JclIcon);
  finally
    JclIcon.Free;
  end;
  FConfigurationAction.Caption := LoadResString(@RsJCLOptions);
  FConfigurationAction.Name := JclConfigureActionName;
  FConfigurationAction.Category := Category;
  FConfigurationAction.Visible := True;
  FConfigurationAction.OnUpdate := ConfigurationActionUpdate;
  FConfigurationAction.OnExecute := ConfigurationActionExecute;

  FConfigurationAction.ActionList := NTAServices.ActionList;
  RegisterAction(FConfigurationAction);

  IDEMenuItem := NTAServices.MainMenu.Items;
  if not Assigned(IDEMenuItem) then
    raise EJclExpertException.CreateRes(@RsENoIDEMenu);

  ToolsMenuItem := nil;
  for Index := 0 to IDEMenuItem.Count - 1 do
    if CompareText(IDEMenuItem.Items[Index].Name, 'ToolsMenu') = 0 then
      ToolsMenuItem := IDEMenuItem.Items[Index];
  if not Assigned(ToolsMenuItem) then
    raise EJclExpertException.CreateRes(@RsENoToolsMenu);

  FConfigurationMenuItem := TMenuItem.Create(nil);
  FConfigurationMenuItem.Name := JclConfigureMenuName;
  FConfigurationMenuItem.Action := FConfigurationAction;

  ToolsMenuItem.Insert(0, FConfigurationMenuItem);
end;

procedure TJclOTAActionExpert.UnregisterCommands;
begin
  UnregisterAction(FConfigurationAction);
  FreeAndNil(FConfigurationAction);
  FreeAndNil(FConfigurationMenuItem);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  RegisterFindGlobalComponentProc(FindActions);
finalization
  UnRegisterFindGlobalComponentProc(FindActions);
  FreeAndNil(GlobalActionList);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
