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
{ The Original Code is VersionControlImpl.pas                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Elahn Ientile.                                     }
{ Portions created by Elahn Ientile are Copyright (C) of Elahn Ientile.                            }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{   Sandeep Chandra                                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclVersionControlImpl;

{$I jcl.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Menus, ActnList, Dialogs, Forms,
  ToolsAPI,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclVersionControl,
  JclOtaUtils, JclOtaActions,
  JclVersionCtrlCommonOptions;

type
  TJclVersionControlStandardAction = class(TCustomAction)
  private
    FControlAction: TJclVersionControlActionType;
  public
    property ControlAction: TJclVersionControlActionType read FControlAction write FControlAction;
  end;

  TJclVersionControlDropDownAction = class(TDropDownAction)
  private
    FControlAction: TJclVersionControlActionType;
  public
    property ControlAction: TJclVersionControlActionType read FControlAction write FControlAction;
  end;

  TJclVersionControlExpert = class (TJclOTAExpert)
  private
    FVersionCtrlMenu: TMenuItem;
    FActions: array [TJclVersionControlActionType] of TCustomAction;
    FIconIndexes: array [TJclVersionControlActionType] of Integer;
    FHideActions: Boolean;
    FIconType: TIconType;
    FActOnTopSandbox: Boolean;
    FSaveConfirmation: Boolean;
    FDisableActions: Boolean;
    FOptionsFrame: TJclVersionCtrlOptionsFrame;
    FMenuOrganization: TStringList;
    procedure SetIconType(const Value: TIconType);

    procedure ActionUpdate(Sender: TObject);
    procedure ActionExecute(Sender: TObject);
    procedure IDEActionMenuClick(Sender: TObject);
    procedure SubItemClick(Sender: TObject);
    procedure DropDownMenuPopup(Sender: TObject);
    procedure IDEVersionCtrlMenuClick(Sender: TObject);
    procedure RefreshIcons;
    procedure RefreshMenu;
    function GetCurrentCache: TJclVersionControlCache;
    function GetCurrentPlugin: TJclVersionControlPlugin;
    function GetCurrentFileName: string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure RegisterCommands; override;
    procedure UnregisterCommands; override;
    function SaveModules(const FileName: string;
      const IncludeSubDirectories: Boolean): Boolean;

    property ActOnTopSandbox: Boolean read FActOnTopSandbox write FActOnTopSandbox;
    property DisableActions: Boolean read FDisableActions write FDisableActions;
    property HideActions: Boolean read FHideActions write FHideActions;
    property SaveConfirmation: Boolean read FSaveConfirmation write FSaveConfirmation;
    property IconType: TIconType read FIconType write SetIconType;
    property CurrentCache: TJclVersionControlCache read GetCurrentCache;
    property CurrentPlugin: TJclVersionControlPlugin read GetCurrentPlugin;
    property CurrentFileName: string read GetCurrentFileName;
  public
    function GetPageName: string; override;
    function GetFrameClass: TCustomFrameClass; override;
    procedure FrameCreated(AFrame: TCustomFrame); override;
    procedure DialogClosed(Accepted: Boolean); override;
  end;

// design package entry point
procedure Register;

// expert DLL entry point
function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var TerminateProc: TWizardTerminateProc): Boolean; stdcall;

function GetItemIndexA(const Item: string): Integer;
function GetItemIndexB(const Item: string): Integer;
function GetItemName(const Item: string): string;

function CharIsAmpersand(const C: Char): Boolean;

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

uses
  Windows, TypInfo, ImgList,
  {$IFDEF BDS8_UP}
  JclOtaAddinOptions,
  {$ENDIF BDS8_UP}
  JclDebug, JclFileUtils, JclRegistry, JclShell, JclStrings,
  JclOtaConsts, JclOtaResources,
  JclVersionCtrlGITImpl,
  JclVersionCtrlSVNImpl,
  JclVersionCtrlCVSImpl;

{$R JclVersionCtrlIcons.RES}

const
  IconNames: array [TJclVersionControlActionType] of PChar =
    ( 'FILEADD',         // vcaAdd
      'SANDBOXADD',      // vcaAddSandbox
      'FILEBLAME',       // vcaBlame
      'FILEBRANCH',      // vcaBranch
      'SANDBOXBRANCH',   // vcaBranchSandbox
      'SANDBOXCHECKOUT', // vcaCheckOutSandbox
      'FILECOMMIT',      // vcaCommit
      'SANDBOXCOMMIT',   // vcaCommitSandbox
      'CONTEXTMENU',     // vcaContextMenu
      'FILEDIFF',        // vcaDiff
      'EXPLORE',         // vcaExplore
      'EXPLORE',         // vcaExploreSandbox
      'FILEGRAPH',       // vcaGraph
      'FILELOG',         // vcaLog
      'SANDBOXLOG',      // vcaLogSandbox
      'FILELOCK',        // vcaLock
      'SANDBOXLOCK',     // vcaLockSandbox
      'FILEMERGE',       // vcaMerge
      'SANDBOXMERGE',    // vcaMergeSandbox
      'PROPERTIES',      // vcaProperties
      'PROPERTIES',      // vcaPropertiesSandbox
      'FILERENAME',      // vcaRename
      'SANDBOXRENAME',   // vcaRenameSandbox
      'REPOBROWSER',     // vcaRepoBrowser
      'FILEREVERT',      // vcaRevert
      'SANDBOXREVERT',   // vcaRevertSandbox
      'STATUS',          // vcaStatus
      'STATUS',          // vcaStatusSandbox
      'FILETAG',         // vcaTag
      'SANDBOXTAG',      // vcaTagSandBox
      'FILEUPDATE',      // vcaUpdate
      'SANDBOXUPDATE',   // vcaUpdateSandbox
      'FILEUPDATE',      // vcaUpdateTo
      'SANDBOXUPDATE',   // vcaUpdateSandboxTo
      'FILEUNLOCK',      // vcaUnlock
      'SANDBOXUNLOCK');  // vcaUnlockSandbox


function CharIsAmpersand(const C: Char): Boolean;
begin
  Result := C = '&';
end;

procedure Register;
begin
  try
    RegisterPackageWizard(TJclVersionControlExpert.Create);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

var
  JCLWizardIndex: Integer = -1;

procedure JclWizardTerminate;
begin
  try
    if JCLWizardIndex <> -1 then
      TJclOTAExpertBase.GetOTAWizardServices.RemoveWizard(JCLWizardIndex);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
    RegisterProc: TWizardRegisterProc;
    var TerminateProc: TWizardTerminateProc): Boolean stdcall;
begin
  try
    TerminateProc := JclWizardTerminate;

    JCLWizardIndex := TJclOTAExpertBase.GetOTAWizardServices.AddWizard(TJclVersionControlExpert.Create);

    Result := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;

function GetItemIndexA(const Item: string): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 1 to Length(Item) do
    if not CharIsDigit(Item[Index]) then
  begin
    Result := StrToInt(Copy(Item, 1, Index - 1));
    Exit;
  end;
  Abort;
end;

function GetItemIndexB(const Item: string): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := Length(Item) downto 1 do
    if not CharIsDigit(Item[Index]) then
  begin
    if Index < Length(Item) then
      Result := StrToInt(Copy(Item, Index + 1, Length(Item) - Index));
    Exit;
  end;
end;

function GetItemName(const Item: string): string;
var
  Index1, Index2: Integer;
begin
  for Index1 := 1 to Length(Item) do
    if not CharIsDigit(Item[Index1]) then
  begin
    if Index1 = 1 then
      Abort;
    Break;
  end;

  for Index2 := Length(Item) downto 1 do
    if not CharIsDigit(Item[Index2]) then
      Break;

  Result := Copy(Item, Index1, Index2 - Index1 + 1);
end;

function MenuOrganizationSort(List: TStringList; Index1, Index2: Integer): Integer;
var
  Item1, Item2: string;
  Index1A, Index1B, Index2A, Index2B: Integer;
begin
  Item1 := List.Strings[Index1];
  Item2 := List.Strings[Index2];
  Index1A := GetItemIndexA(Item1);
  Index1B := GetItemIndexB(Item1);
  Index2A := GetItemIndexA(Item2);
  Index2B := GetItemIndexB(Item2);

  if Index1A < Index2A then
    Result := -1
  else
  if Index1A > Index2A then
    Result := 1
  else
  if Index1B < Index2B then
    Result := -1
  else
  if Index1B > Index2B then
    Result := 1
  else
    Result := 0;
end;

function ActionToControlAction(AAction: TCustomAction): TJclVersionControlActionType;
begin
  if AAction is TJclVersionControlDropDownAction then
    Result := TJclVersionControlDropDownAction(AAction).ControlAction
  else
  if AAction is TJclVersionControlStandardAction then
    Result := TJclVersionControlStandardAction(AAction).ControlAction
  else
    raise EJclExpertException.CreateRes(@RsEInvalidAction);
end;

//=== { TJclVersionControlExpert } ===================================================

procedure TJclVersionControlExpert.ActionExecute(Sender: TObject);
var
  Index: Integer;
  AAction: TCustomAction;
  ControlAction: TJclVersionControlActionType;
  ControlActionInfo: TJclVersionControlActionInfo;
  APlugin: TJclVersionControlPlugin;
  AFileName: string;
  AFileCache: TJclVersionControlCache;
  PluginList: TJclVersionControlPluginList;
begin
  try
    AAction := Sender as TCustomAction;
    ControlAction := ActionToControlAction(AAction);
    ControlActionInfo := VersionControlActionInfo(ControlAction);

    if ControlActionInfo.Sandbox then
    begin
      AFileCache := CurrentCache;
      if not Assigned(AFileCache) or ControlActionInfo.AllPlugins then
        Exit;
      if ActOnTopSandbox then
      begin
        for Index := AFileCache.SandboxCount - 1 downto 0 do
          if ControlAction in AFileCache.SandboxActions[Index] then
        begin
          if ControlActionInfo.SaveFile then
            SaveModules(AFileCache.SandBoxes[Index], True);
          AFileCache.Plugin.ExecuteAction(AFileCache.SandBoxes[Index], ControlAction);
          Exit;
        end;
      end
      else
      begin
        for Index := 0 to AFileCache.SandboxCount - 1 do
          if ControlAction in AFileCache.SandboxActions[Index] then
        begin
          if ControlActionInfo.SaveFile then
            SaveModules(AFileCache.SandBoxes[Index], True);
          AFileCache.Plugin.ExecuteAction(AFileCache.SandBoxes[Index], ControlAction);
          Exit;
        end;
      end;
    end
    else
    begin
      AFileName := CurrentFileName;
      if ControlActionInfo.SaveFile then
        SaveModules(AFileName, False);

      if ControlActionInfo.AllPlugins then
      begin
        PluginList := VersionControlPluginList;
        for Index := 0 to PluginList.Count - 1 do
        begin
          AFileCache := PluginList.GetFileCache(AFileName, PluginList.Plugins[Index]);

          if ControlAction in AFileCache.Actions then
          begin
            AFileCache.Plugin.ExecuteAction(AFileName, ControlAction);
            Exit;
          end;
        end;
      end
      else
      begin
        APlugin := CurrentPlugin;
        if Assigned(APlugin) then
          APlugin.ExecuteAction(AFileName, ControlAction);
      end;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclVersionControlExpert.ActionUpdate(Sender: TObject);
var
  IndexSandbox, IndexPlugin: Integer;
  AAction: TCustomAction;
  ControlAction: TJclVersionControlActionType;
  ControlActionInfo: TJclVersionControlActionInfo;
  AFileCache: TJclVersionControlCache;
  AFileName: string;
  PluginList: TJclVersionControlPluginList;
begin
  try
    AAction := Sender as TCustomAction;
    ControlAction := ActionToControlAction(AAction);
    ControlActionInfo := VersionControlActionInfo(ControlAction);
    AFileCache := CurrentCache;

    if HideActions and not ControlActionInfo.AllPlugins then
      AAction.Visible := Assigned(AFileCache) and Assigned(AFileCache.Plugin)
        and (ControlAction in AFileCache.Plugin.SupportedActionTypes)
    else
      AAction.Visible := True;

    if DisableActions then
    begin
      if ControlActionInfo.Sandbox then
      begin
        if ControlActionInfo.AllPlugins then
        begin
          PluginList := VersionControlPluginList;
          AFileName := CurrentFileName;
          for IndexPlugin := 0 to PluginList.Count - 1 do
          begin
            AFileCache := PluginList.GetFileCache(AFileName, PluginList.Plugins[IndexPlugin]);
            for IndexSandbox := 0 to AFileCache.SandBoxCount - 1 do
              if ControlAction in AFileCache.SandBoxActions[IndexSandbox] then
            begin
              AAction.Enabled := True;
              Exit;
            end;
            AAction.Enabled := False;
            Exit;
          end;
        end
        else // work for all plugin
        begin
          if Assigned(AFileCache) then
          begin
            for IndexSandbox := 0 to AFileCache.SandBoxCount - 1 do
              if ControlAction in AFileCache.SandBoxActions[IndexSandbox] then
            begin
              AAction.Enabled := True;
              Exit;
            end;
            AAction.Enabled := False;
            Exit;
          end
          else
            AAction.Enabled := False;
        end;
        Exit;
      end
      else // file
      begin
        if ControlActionInfo.AllPlugins then
        begin
          PluginList := VersionControlPluginList;
          AFileName := CurrentFileName;
          for IndexPlugin := 0 to PluginList.Count - 1 do
          begin
            AFileCache := PluginList.GetFileCache(AFileName, PluginList.Plugins[IndexPlugin]);
            if ControlAction in AFileCache.Actions then
            begin
              AAction.Enabled := True;
              Exit;
            end;
          end;
          AAction.Enabled := False;
          Exit;
        end
        else // only the current plugin
        begin
          AFileCache := CurrentCache;
          AAction.Enabled := Assigned(AFileCache) and (ControlAction in AFileCache.Actions);
        end;
      end;
    end
    else
      AAction.Enabled := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

constructor TJclVersionControlExpert.Create;
begin
  FMenuOrganization := TStringList.Create;

  inherited Create('JclVersionControlExpert');
end;

destructor TJclVersionControlExpert.Destroy;
begin
  inherited Destroy;
  FMenuOrganization.Free;
end;

procedure TJclVersionControlExpert.DialogClosed(Accepted: Boolean);
begin
  if Accepted then
  begin
    DisableActions := FOptionsFrame.DisableActions;
    HideActions := FOptionsFrame.HideActions;
    SaveConfirmation := FOptionsFrame.SaveConfirmation;
    ActOnTopSandbox := FOptionsFrame.ActOnTopSandbox;
    FMenuOrganization.Assign(FOptionsFrame.MenuTree);
    IconType := FOptionsFrame.IconType;
    RefreshMenu;
  end;
  FOptionsFrame := nil;
end;

procedure TJclVersionControlExpert.DropDownMenuPopup(Sender: TObject);
var
  APopupMenu: TPopupMenu;
  AMenuItem: TMenuItem;
  ControlAction: TJclVersionControlActionType;
  ControlActionInfo: TJclVersionControlActionInfo;
  AFileCache: TJclVersionControlCache;
  IndexPlugin, IndexSandbox: Integer;
  AFileName: string;
  PluginList: TJclVersionControlPluginList;
begin
  try
    APopupMenu := Sender as TPopupMenu;
    ControlAction := TJclVersionControlActionType(APopupMenu.Tag);
    ControlActionInfo := VersionControlActionInfo(ControlAction);

    APopupMenu.Items.Clear;

    if ControlActionInfo.AllPlugins then
    begin
      PluginList := VersionControlPluginList;
      AFileName := CurrentFileName;
      for IndexPlugin := 0 to PluginList.Count - 1 do
      begin
        AFileCache := PluginList.GetFileCache(AFileName, PluginList.Plugins[IndexPlugin]);
        for IndexSandbox := 0 to AFileCache.SandBoxCount - 1 do
          if ControlAction in AFileCache.SandBoxActions[IndexSandbox] then
        begin
          AMenuItem := TMenuItem.Create(APopupMenu.Items);
          AMenuItem.Caption := Format('%s | %s', [AFileCache.Plugin.Name, AFileCache.SandBoxes[IndexSandbox]]);
          AMenuItem.Tag := APopupMenu.Tag;
          AMenuItem.OnClick := SubItemClick;
          case IconType of
            itNone:
              AMenuItem.ImageIndex := -1;
            itJCL:
              AMenuItem.ImageIndex := FIconIndexes[ControlAction];
          end;
          APopupMenu.Items.Add(AMenuItem);
        end;
      end;
    end
    else
    begin
      AFileCache := CurrentCache;
      if Assigned(AFileCache) then
        for IndexSandbox := 0 to AFileCache.SandBoxCount - 1 do
          if ControlAction in AFileCache.SandBoxActions[IndexSandbox] then
      begin
        AMenuItem := TMenuItem.Create(APopupMenu.Items);
        AMenuItem.Caption := AFileCache.SandBoxes[IndexSandbox];
        AMenuItem.Tag := APopupMenu.Tag;
        AMenuItem.OnClick := SubItemClick;
        case IconType of
          itNone:
            AMenuItem.ImageIndex := -1;
          itJCL:
            AMenuItem.ImageIndex := FIconIndexes[ControlAction];
        end;
        APopupMenu.Items.Add(AMenuItem);
      end;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclVersionControlExpert.FrameCreated(AFrame: TCustomFrame);
begin
  FOptionsFrame := AFrame as TJclVersionCtrlOptionsFrame;

  FOptionsFrame.DisableActions := DisableActions;
  FOptionsFrame.HideActions := HideActions;
  FOptionsFrame.SaveConfirmation := SaveConfirmation;
  FOptionsFrame.ActOnTopSandbox := ActOnTopSandbox;
  FOptionsFrame.SetActions(FActions);
  // after SetActions
  FOptionsFrame.MenuTree := FMenuOrganization;
  FOptionsFrame.IconType := IconType;
end;

function TJclVersionControlExpert.GetCurrentCache: TJclVersionControlCache;
var
  Index: Integer;
  AFileName: string;
  PluginList: TJclVersionControlPluginList;
begin
  PluginList := VersionControlPluginList;
  AFileName := CurrentFileName;
  for Index := 0 to PluginList.Count - 1 do
  begin
    Result := PluginList.GetFileCache(AFileName, PluginList.Plugins[Index]);
    if Result.Supported then
      Exit;
  end;
  Result := nil;
end;

function TJclVersionControlExpert.GetCurrentFileName: string;
var
  AOTAModule: IOTAModule;
begin
  AOTAModule := GetOTAModuleServices.CurrentModule;
  //SC  20/03/2007
  if Assigned(AOTAModule) and Assigned(AOTAModule.CurrentEditor) then
  begin
    Result := AOTAModule.CurrentEditor.FileName;
    Exit;
  end
  //SC  20/03/2007
  else
  if Assigned(AOTAModule) and (AOTAModule.FileSystem = '') then
    Result := AOTAModule.FileName
  else
    Result := '';
end;

function TJclVersionControlExpert.GetCurrentPlugin: TJclVersionControlPlugin;
var
  Index: Integer;
  AFileCacheInfo: TJclVersionControlCache;
  AFileName: string;
  PluginList: TJclVersionControlPluginList;
begin
  PluginList := VersionControlPluginList;
  AFileName := CurrentFileName;
  for Index := 0 to PluginList.Count - 1 do
  begin
    Result := TJclVersionControlPlugin(PluginList.Plugins[Index]);
    AFileCacheInfo := PluginList.GetFileCache(AFileName, Result);
    if AFileCacheInfo.Supported then
      Exit;
  end;
  Result := nil;
end;

function TJclVersionControlExpert.GetFrameClass: TCustomFrameClass;
begin
  Result := TJclVersionCtrlOptionsFrame;
end;

function TJclVersionControlExpert.GetPageName: string;
begin
  Result := LoadResString(@RsVersionControlSheet);
end;

procedure TJclVersionControlExpert.IDEActionMenuClick(Sender: TObject);
var
  AMenuItem, SubMenuItem: TMenuItem;
  ControlAction: TJclVersionControlActionType;
  ControlActionInfo: TJclVersionControlActionInfo;
  IndexSandbox, IndexPlugin, IndexItem: Integer;
  AFileCache: TJclVersionControlCache;
  AFileName: string;
  PluginList: TJclVersionControlPluginList;
begin
  try
    AMenuItem := Sender as TMenuItem;
    // do not delete the dummy subitem
    for IndexItem := AMenuItem.Count - 1 downto 1 do
      AMenuItem.Items[IndexItem].Free;
    ControlAction := TJclVersionControlActionType(AMenuItem.Tag);
    ControlActionInfo := VersionControlActionInfo(ControlAction);

    if ControlActionInfo.AllPlugins then
    begin
      PluginList := VersionControlPluginList;
      for IndexPlugin := 0 to PluginList.Count - 1 do
      begin
        AFileName := CurrentFileName;
        AFileCache := PluginList.GetFileCache(AFileName, PluginList.Plugins[IndexPlugin]);
        for IndexSandbox := 0 to AFileCache.SandBoxCount - 1 do
          if ControlAction in AFileCache.SandBoxActions[IndexSandbox] then
        begin
          SubMenuItem := TMenuItem.Create(AMenuItem);
          SubMenuItem.Caption := Format('%s | %s', [AFileCache.Plugin.Name, AFileCache.SandBoxes[IndexSandbox]]);
          SubMenuItem.Tag := Integer(ControlAction);
          SubMenuItem.OnClick := SubItemClick;
          case IconType of
            itNone:
              SubMenuItem.ImageIndex := -1;
            itJCL:
              SubMenuItem.ImageIndex := FIconIndexes[ControlAction];
          end;
          AMenuItem.Add(SubMenuItem);
        end;
      end;
    end
    else
    begin
      AFileCache := CurrentCache;

      if Assigned(AFileCache) then
        for IndexSandbox := 0 to AFileCache.SandBoxCount - 1 do
          if ControlAction in AFileCache.SandBoxActions[IndexSandbox] then
      begin
        SubMenuItem := TMenuItem.Create(AMenuItem);
        SubMenuItem.Caption := AFileCache.SandBoxes[IndexSandbox];
        SubMenuItem.Tag := Integer(ControlAction);
        SubMenuItem.OnClick := SubItemClick;
        case IconType of
          itNone:
            SubMenuItem.ImageIndex := -1;
          itJCL:
            SubMenuItem.ImageIndex := FIconIndexes[ControlAction];
        end;
        AMenuItem.Add(SubMenuItem);
      end;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclVersionControlExpert.IDEVersionCtrlMenuClick(Sender: TObject);
  procedure UpdateMenuItem(const AMenuItem: TMenuItem);
  var
    BMenuItem: TMenuItem;
    IndexMenu, IndexSandbox: Integer;
    ControlAction: TJclVersionControlActionType;
    ControlActionInfo: TJclVersionControlActionInfo;
    AFileCache: TJclVersionControlCache;
    AEnabled: Boolean;
    IndexPlugin: Integer;
    AFileName: string;
    PluginList: TJclVersionControlPluginList;
  begin
    for IndexMenu := 0 to AMenuItem.Count - 1 do
    begin
      BMenuItem := AMenuItem.Items[IndexMenu];
      if BMenuItem.Tag = -1 then
        UpdateMenuItem(BMenuItem)
      else
      if BMenuItem.Tag >= 0 then
      begin
        ControlAction := TJclVersionControlActionType(BMenuItem.Tag);
        ControlActionInfo := VersionControlActionInfo(ControlAction);
        if ControlActionInfo.Sandbox then
        begin
          AFileCache := CurrentCache;

          case IconType of
            itNone:
              BMenuItem.ImageIndex := -1;
            itJCL:
              BMenuItem.ImageIndex := FIconIndexes[ControlAction];
          end;

          if HideActions and not ControlActionInfo.AllPlugins then
            BMenuItem.Visible := Assigned(AFileCache) and Assigned(AFileCache.Plugin)
              and (ControlAction in AFileCache.Plugin.SupportedActionTypes)
          else
            BMenuItem.Visible := True;

          if DisableActions then
          begin
            AEnabled := False;
            if ControlActionInfo.AllPlugins then
            begin
              PluginList := VersionControlPluginList;
              AFileName := CurrentFileName;
              for IndexPlugin := 0 to PluginList.Count - 1 do
              begin
                AFileCache := PluginList.GetFileCache(AFileName, PluginList.Plugins[IndexPlugin]);
                for IndexSandbox := 0 to AFileCache.SandBoxCount - 1 do
                  if ControlAction in AFileCache.SandBoxActions[IndexSandbox] then
                begin
                  AEnabled := True;
                  Break;
                end;

                if AEnabled then
                  Break;
              end;
            end
            else
            if Assigned(AFileCache) then
            begin
              for IndexSandbox := 0 to AFileCache.SandboxCount - 1 do
                if ControlAction in AFileCache.SandboxActions[IndexSandbox] then
              begin
                AEnabled := True;
                Break;
              end;
            end;
            BMenuItem.Enabled := AEnabled;
          end
          else
            BMenuItem.Enabled := True;
        end;
      end;
    end;
  end;
begin
  try
    UpdateMenuItem(FVersionCtrlMenu);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclVersionControlExpert.RefreshIcons;
var
  ControlAction: TJclVersionControlActionType;
begin
  for ControlAction := Low(TJclVersionControlActionType) to High(TJclVersionControlActionType) do
    if Assigned(FActions[ControlAction]) then
  begin
    case IconType of
      // No icon
      itNone :
        FActions[ControlAction].ImageIndex := -1;
      // JCL icons
      itJCL :
        FActions[ControlAction].ImageIndex := FIconIndexes[ControlAction];
    end;
  end;
end;

procedure TJclVersionControlExpert.RefreshMenu;
  procedure LoadDefaultMenu;
  var
    Action: TJclVersionControlActionType;
  begin
    FMenuOrganization.Clear;
    for Action := Low(TJclVersionControlActionType) to High(TJclVersionControlActionType) do
      FMenuOrganization.Add(Format('%d%s', [Integer(Action), GetEnumName(TypeInfo(TJclVersionControlActionType), Integer(Action))]));
  end;
var
  Index, IndexA, IndexB, ActionIndex: Integer;
  SubMenuItem, ActionMenuItem, DummyMenuItem: TMenuItem;
  Item, ItemName: string;
  AAction: TCustomAction;
begin
  FVersionCtrlMenu.Clear;

  if FMenuOrganization.Count > 0 then
  try
    FMenuOrganization.CustomSort(MenuOrganizationSort);
  except
    LoadDefaultMenu;
  end
  else
    LoadDefaultMenu;

  SubMenuItem := nil;
  for Index := 0 to FMenuOrganization.Count - 1 do
  begin
    Item := FMenuOrganization.Strings[Index];
    IndexA := GetItemIndexA(Item);
    IndexB := GetItemIndexB(Item);
    ItemName := GetItemName(Item);
    ActionIndex := GetEnumValue(TypeInfo(TJclVersionControlActionType), ItemName);

    if IndexB = -1 then
    begin
      if FVersionCtrlMenu.Count <> IndexA then
        Abort;

      if (ActionIndex = -1) or (ItemName = '-') then
      begin
        SubMenuItem := TMenuItem.Create(FVersionCtrlMenu);
        SubMenuItem.Caption := ItemName;
        SubMenuItem.Tag := -1;
        FVersionCtrlMenu.Add(SubMenuItem);
      end
      else
      begin
        ActionMenuItem := TMenuItem.Create(FVersionCtrlMenu);
        AAction := FActions[TJclVersionControlActionType(ActionIndex)];
        if VersionControlActionInfo(TJclVersionControlActionType(ActionIndex)).Sandbox then
        begin
          ActionMenuItem.Caption := AAction.Caption;
          ActionMenuItem.ShortCut := AAction.ShortCut;
          ActionMenuItem.ImageIndex := AAction.ImageIndex;
          ActionMenuItem.Tag := ActionIndex;
          ActionMenuItem.OnClick := IDEActionMenuClick;

          // to always have the arrow in the parent menu item
          DummyMenuItem := TMenuItem.Create(ActionMenuItem);
          DummyMenuItem.Visible := False;
          DummyMenuItem.Tag := -2;
          ActionMenuItem.Add(DummyMenuItem);
        end
        else
          ActionMenuItem.Action := AAction;
        FVersionCtrlMenu.Add(ActionMenuItem);
        SubMenuItem := nil;
      end;
    end
    else
    begin
      if (not Assigned(SubMenuItem)) or (SubMenuItem.Count <> IndexB) then
        Abort;
      if (ActionIndex = -1) or (ItemName = '-') then
      begin
        ActionMenuItem := TMenuItem.Create(FVersionCtrlMenu);
        ActionMenuItem.Caption := ItemName;
      end
      else
      begin
        ActionMenuItem := TMenuItem.Create(FVersionCtrlMenu);
        AAction := FActions[TJclVersionControlActionType(ActionIndex)];
        if VersionControlActionInfo(TJclVersionControlActionType(ActionIndex)).Sandbox then
        begin
          ActionMenuItem.Caption := AAction.Caption;
          ActionMenuItem.ShortCut := AAction.ShortCut;
          ActionMenuItem.ImageIndex := AAction.ImageIndex;
          ActionMenuItem.Tag := ActionIndex;
          ActionMenuItem.OnClick := IDEActionMenuClick;

          // to always have the arrow in the parent menu item
          DummyMenuItem := TMenuItem.Create(ActionMenuItem);
          DummyMenuItem.Visible := False;
          DummyMenuItem.Tag := -2;
          ActionMenuItem.Add(DummyMenuItem);
        end
        else
          ActionMenuItem.Action := AAction;
      end;
      SubMenuItem.Add(ActionMenuItem);
    end;
  end;
end;

procedure TJclVersionControlExpert.RegisterCommands;
var
  IDEMainMenu: TMainMenu;
  IDEToolsItem: TMenuItem;
  IDEImageList: TCustomImageList;
  IDEActionList: TCustomActionList;
  I: Integer;
  AStandardAction: TJclVersionControlStandardAction;
  ADropDownAction: TJclVersionControlDropDownAction;
  AAction: TCustomAction;
  IconTypeStr: string;
  ControlAction: TJclVersionControlActionType;
  ControlActionInfo: TJclVersionControlActionInfo;
  NTAServices: INTAServices;
  AIcon: TIcon;
begin
  inherited RegisterCommands;
  NTAServices := GetNTAServices;

  Settings.LoadStrings(JclVersionCtrlMenuOrganizationName, FMenuOrganization);
  SaveConfirmation := Settings.LoadBool(JclVersionCtrlSaveConfirmationName, True);
  DisableActions := Settings.LoadBool(JclVersionCtrlDisableActionsName, True);
  HideActions := Settings.LoadBool(JclVersionCtrlHideActionsName, False);
  IconTypeStr := Settings.LoadString(JclVersionCtrlIconTypeName, JclVersionCtrlIconTypeAutoValue);
  ActOnTopSandbox := Settings.LoadBool(JclVersionCtrlActOnTopSandboxName, False);

  FIconType := itJCL;
  if IconTypeStr = JclVersionCtrlIconTypeNoIconValue then
    FIconType := itNone
  else
  if IconTypeStr = JclVersionCtrlIconTypeJclIconValue then
    FIconType := itJCL;

  IDEImageList := NTAServices.ImageList;
  AIcon := TIcon.Create;
  try
    {$IFDEF COMPILER14_UP}
    IDEImageList.BeginUpdate;
    try
    {$ENDIF COMPILER14_UP}
      for ControlAction := Low(TJclVersionControlActionType) to High(TJclVersionControlActionType) do
      begin
        AIcon.Handle := LoadIcon(HInstance, IconNames[ControlAction]);
        FIconIndexes[ControlAction] := IDEImageList.AddIcon(AIcon);
      end;
    {$IFDEF COMPILER14_UP}
    finally
      IDEImageList.EndUpdate;
    end;
    {$ENDIF COMPILER14_UP}
  finally
    AIcon.Free;
  end;

  IDEMainMenu := NTAServices.MainMenu;
  IDEToolsItem := nil;
  for I := 0 to IDEMainMenu.Items.Count - 1 do
    if IDEMainMenu.Items[I].Name = 'ToolsMenu' then
    begin
      IDEToolsItem := IDEMainMenu.Items[I];
      Break;
    end;
  if not Assigned(IDEToolsItem) then
    raise EJclExpertException.CreateRes(@RsENoToolsMenuItem);

  IDEActionList := NTAServices.ActionList;

  FVersionCtrlMenu := TMenuItem.Create(nil);
  FVersionCtrlMenu.Caption := LoadResString(@RsVersionCtrlMenuCaption);
  FVersionCtrlMenu.Name := JclVersionCtrlMenuName;
  FVersionCtrlMenu.OnClick := IDEVersionCtrlMenuClick;
  IDEMainMenu.Items.Insert(IDEToolsItem.MenuIndex + 1, FVersionCtrlMenu);
  if not Assigned(FVersionCtrlMenu.Parent) then
    raise EJclExpertException.CreateResFmt(@RsSvnMenuItemNotInserted, [FVersionCtrlMenu.Caption]);

  for ControlAction := Low(TJclVersionControlActionType) to High(TJclVersionControlActionType) do
  begin
    ControlActionInfo := VersionControlActionInfo(ControlAction);

    if ControlActionInfo.Sandbox then
    begin
      ADropDownAction := TJclVersionControlDropDownAction.Create(nil);
      ADropDownAction.ControlAction := ControlAction;
      ADropDownAction.DropdownMenu := TPopupMenu.Create(nil);
      ADropDownAction.DropdownMenu.AutoPopup := True;
      ADropDownAction.DropdownMenu.AutoHotkeys := maManual;
      ADropDownAction.DropdownMenu.Tag := Integer(ControlAction);
      ADropDownAction.DropdownMenu.OnPopup := DropDownMenuPopup;
      AAction := ADropDownAction;
    end
    else
    begin
      AStandardAction := TJclVersionControlStandardAction.Create(nil);
      AStandardAction.ControlAction := ControlAction;
      AAction := AStandardAction;
    end;

    AAction.Caption := LoadResString(ControlActionInfo.Caption);
    AAction.Name := ControlActionInfo.ActionName;
    AAction.Visible := True;
    AAction.ActionList := IDEActionList;
    AAction.OnExecute := ActionExecute;
    AAction.OnUpdate := ActionUpdate;
    AAction.Category := LoadResString(@RsActionCategory);
    TJclOTAActionExpert.RegisterAction(AAction);
    FActions[ControlAction] := AAction;
  end;

  RefreshIcons;

  RefreshMenu;
end;

function TJclVersionControlExpert.SaveModules(const FileName: string;
  const IncludeSubDirectories: Boolean): Boolean;
var
  Module: IOTAModule;
  Index: Integer;
  Save: Boolean;
  OTAModuleServices: IOTAModuleServices;
begin
  Result := True;
  OTAModuleServices := GetOTAModuleServices;

  for Index := 0 to OTAModuleServices.ModuleCount - 1 do
  begin
    Module := OTAModuleServices.Modules[Index];

    if Module.FileSystem <> '' then
    begin
      if IncludeSubDirectories then
        Save := PathIsChild(Module.FileName, FileName)
      else
        Save := Module.FileName = FileName;

      if Save then
        Module.Save(False, True);
    end;
  end;
end;

procedure TJclVersionControlExpert.SetIconType(const Value: TIconType);
begin
  if Value <> FIconType then
  begin
    FIconType := Value;
    RefreshIcons;
  end;
end;

procedure TJclVersionControlExpert.SubItemClick(Sender: TObject);
var
  APlugin: TJclVersionControlPlugin;
  AMenuItem: TMenuItem;
  AAction: TCustomAction;
  Directory, PluginName: string;
  PosSeparator, IndexPlugin: Integer;
  ControlAction: TJclVersionControlActionType;
  ControlActionInfo: TJclVersionControlActionInfo;
  PluginList: TJclVersionControlPluginList;
begin
  try
    APlugin := CurrentPlugin;
    if Sender is TCustomAction then
    begin
      AAction := TCustomAction(Sender);
      ControlAction := TJclVersionControlActionType(AAction.Tag);
      Directory := AAction.Caption;
    end
    else
    if Sender is TMenuItem then
    begin
      AMenuItem := TMenuItem(Sender);
      ControlAction := TJclVersionControlActionType(AMenuItem.Tag);
      Directory := AMenuItem.Caption;
    end
    else
      Exit;

    ControlActionInfo := VersionControlActionInfo(ControlAction);
    Directory := StrRemoveChars(Directory, CharIsAmpersand);

    if ControlActionInfo.AllPlugins then
    begin
      PluginList := VersionControlPluginList;
      PosSeparator := Pos('|', Directory);
      PluginName := StrLeft(Directory, PosSeparator - 2);
      Directory := StrRight(Directory, Length(Directory) - PosSeparator - 1);
      for IndexPlugin := 0 to PluginList.Count - 1 do
      begin
        APlugin := TJclVersionControlPlugin(PluginList.Plugins[IndexPlugin]);
        if SameText(APlugin.Name, PluginName) then
          Break;
        APlugin := nil;
      end;

      if not Assigned(APlugin) then
        Exit;
    end;

    if ControlActionInfo.SaveFile then
      SaveModules(Directory, True);
    if Assigned(APlugin) then
      APlugin.ExecuteAction(Directory , ControlAction);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclVersionControlExpert.UnregisterCommands;
var
  ControlAction: TJclVersionControlActionType;
  ADropDownAction: TDropDownAction;
begin
  inherited UnregisterCommands;

  Settings.SaveStrings(JclVersionCtrlMenuOrganizationName, FMenuOrganization);
  Settings.SaveBool(JclVersionCtrlSaveConfirmationName, SaveConfirmation);
  Settings.SaveBool(JclVersionCtrlDisableActionsName, DisableActions);
  Settings.SaveBool(JclVersionCtrlHideActionsName, HideActions);
  Settings.SaveBool(JclVersionCtrlActOnTopSandboxName, ActOnTopSandbox);
  case FIconType of
    itNone:
      Settings.SaveString(JclVersionCtrlIconTypeName, JclVersionCtrlIconTypeNoIconValue);
    itJCL:
      Settings.SaveString(JclVersionCtrlIconTypeName, JclVersionCtrlIconTypeJclIconValue);
  end;

  for ControlAction := Low(TJclVersionControlActionType) to High(TJclVersionControlActionType) do
  begin
    TJclOTAActionExpert.UnregisterAction(FActions[ControlAction]);
    if FActions[ControlAction] is TDropDownAction then
    begin
      ADropDownAction := TDropDownAction(FActions[ControlAction]);
      if Assigned(ADropDownAction.DropDownMenu) then
      begin
        ADropDownAction.DropDownMenu.Items.Clear;
        ADropDownAction.DropDownMenu.Free;
        ADropDownAction.DropDownMenu := nil;
      end;
    end;
    FreeAndNil(FActions[ControlAction]);
  end;
  FVersionCtrlMenu.Clear;
  FreeAndNil(FVersionCtrlMenu);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
