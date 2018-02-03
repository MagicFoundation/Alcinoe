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
{ The Original Code is ProjAnalyzerImpl.pas.                                                       }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclProjectAnalyzerImpl;

{$I jcl.inc}

interface

uses
  Classes, Menus, ActnList, ToolsAPI, SysUtils, Graphics, Dialogs, Forms,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOtaUtils, JclOtaActions, JclProjectAnalyzerFrm;

type
  TJclProjectAnalyzerExpert = class(TJclOTAExpert)
  private
    FBuildMenuItem: TMenuItem;
    FBuildAction: TAction;
    {$IFDEF BDS4_UP}
    FProjectManagerNotifierIndex: Integer;
    {$ENDIF BDS4_UP}
    procedure ActionExecute(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure AnalyzeProject(const AProject: IOTAProject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure RegisterCommands; override;
    procedure UnregisterCommands; override;
  end;

  {$IFDEF BDS7_UP}
  // RAD Studio 2010 and newer
  TProjectManagerMultipleNotifier = class(TNotifierObject, IOTANotifier, IOTAProjectMenuItemCreatorNotifier)
  private
    FProjectAnalyser: TJclProjectAnalyzerExpert;
  public
    constructor Create(AProjectAnalyzer: TJclProjectAnalyzerExpert);
    procedure MenuExecute(const MenuContextList: IInterfaceList);
    { IOTAProjectMenuItemCreatorNotifier }
    procedure AddMenu(const Project: IOTAProject; const Ident: TStrings;
      const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
  end;
  {$ELSE ~BDS7_UP}
  {$IFDEF BDS4_UP}
  // BDS 2006, RAD Studio 2007 and RAD Studio 2009
  TProjectManagerSimpleNotifier = class(TNotifierObject, IOTANotifier, INTAProjectMenuCreatorNotifier)
  private
    FProjectAnalyser: TJclProjectAnalyzerExpert;
    FOTAProjectManager: IOTAProjectManager;
  public
    constructor Create(AProjectAnalyzer: TJclProjectAnalyzerExpert; const AOTAProjectManager: IOTAProjectManager);
    procedure AnalyzeProjectMenuClick(Sender: TObject);
    { INTAProjectMenuCreatorNotifier }
    function AddMenu(const Ident: string): TMenuItem;
    function CanHandle(const Ident: string): Boolean;
  end;
  {$ENDIF BDS4_UP}
  {$ENDIF ~BDS7_UP}

// design package entry point
procedure Register;

// expert DLL entry point
function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var TerminateProc: TWizardTerminateProc): Boolean; stdcall;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\projectanalyser';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$R JclProjectAnalyzerIcon.res}

uses
  {$IFDEF HAS_UNIT_SYSTEM_ACTIONS}
  System.Actions,
  {$ENDIF HAS_UNIT_SYSTEM_ACTIONS}
  Variants,
  JclDebug, JclFileUtils, JclOtaConsts, 
  JclOtaResources;

procedure Register;
begin
  try
    RegisterPackageWizard(TJclProjectAnalyzerExpert.Create);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

var
  JCLWizardIndex: Integer;

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

    JCLWizardIndex := TJclOTAExpertBase.GetOTAWizardServices.AddWizard(TJclProjectAnalyzerExpert.Create);

    Result := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;

//=== { TJclProjectAnalyzerExpert } ==========================================

constructor TJclProjectAnalyzerExpert.Create;
begin
  inherited Create(JclProjectAnalyzerExpertName);
end;

destructor TJclProjectAnalyzerExpert.Destroy;
begin
  FreeAndNil(ProjectAnalyzerForm);
  inherited Destroy;
end;

procedure TJclProjectAnalyzerExpert.ActionExecute(Sender: TObject);
var
  ActiveProject: IOTAProject;
begin
  try
    ActiveProject := GetActiveProject;
    if ActiveProject <> nil then
      AnalyzeProject(ActiveProject)
    else
      raise EJclExpertException.CreateRes(@RsENoActiveProject);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclProjectAnalyzerExpert.ActionUpdate(Sender: TObject);
var
  ActiveProject: IOTAProject;
  ProjectName: string;
begin
  try
    ActiveProject := GetActiveProject;
    if Assigned(ActiveProject) then
      ProjectName := ExtractFileName(ActiveProject.FileName)
    else
      ProjectName := '';
    FBuildAction.Enabled := Assigned(ActiveProject);
    if not FBuildAction.Enabled then
      ProjectName := LoadResString(@RsProjectNone);
    FBuildAction.Caption := Format(LoadResString(@RsAnalyzeActionCaption), [ProjectName]);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclProjectAnalyzerExpert.AnalyzeProject(const AProject: IOTAProject);
var
  BuildOK, Succ: Boolean;
  ProjOptions: IOTAProjectOptions;
  SaveMapFile, SaveDccMapFileType, SaveILinkMapFileType: Variant;
  ProjectName, OutputDirectory: string;
  ProjectFileName, MapFileName, ExecutableFileName: TFileName;
  {$IFDEF BDS6_UP}
  ProjOptionsConfigurations: IOTAProjectOptionsConfigurations;
  ActiveConfiguration: IOTABuildConfiguration;
  {$ENDIF BDS6_UP}
begin
  try
    JclDisablePostCompilationProcess := True;

    ProjectFileName := AProject.FileName;
    ProjectName := ExtractFileName(ProjectFileName);
    Succ := False;

    ProjOptions := AProject.ProjectOptions;
    if not Assigned(ProjOptions) then
      raise EJclExpertException.CreateRes(@RsENoProjectOptions);

    OutputDirectory := GetOutputDirectory(AProject);
    MapFileName := GetMapFileName(AProject);

    if ProjectAnalyzerForm = nil then
    begin
      ProjectAnalyzerForm := TProjectAnalyzerForm.Create(Application, Settings);
      ProjectAnalyzerForm.Show;
    end;
    ProjectAnalyzerForm.ClearContent;
    ProjectAnalyzerForm.StatusBarText := Format(LoadResString(@RsBuildingProject), [ProjectName]);

    {$IFDEF BDS6_UP}
    Supports(ProjOptions, IOTAProjectOptionsConfigurations, ProjOptionsConfigurations);
    if not Assigned(ProjOptionsConfigurations) then
      raise EJclExpertException.CreateRes(@RsENoProjectOptionsConfigurations);

    // get the current build configuration
    ActiveConfiguration := ProjOptionsConfigurations.ActiveConfiguration;

    // retrieve options from this build configuration
    SaveMapFile := ActiveConfiguration.GetValue(MapFileOptionName, True);
    SaveDccMapFileType := ActiveConfiguration.GetValue(DccMapFileOptionName, True);
    SaveILinkMapFileType := ActiveConfiguration.GetValue(ILinkMapFileTypeOptionName, True);
    ActiveConfiguration.SetValue(MapFileOptionName, IntToStr(MapFileOptionDetailed));
    ActiveConfiguration.SetValue(DccMapFileOptionName, IntToStr(MapFileOptionDetailed));
    ActiveConfiguration.SetValue(ILinkMapFileTypeOptionName, MapFileOptionDetailedSegments);
    {$ELSE ~BDS6_UP}
    SaveMapFile := ProjOptions.Values[MapFileOptionName];
    SaveDccMapFileType := ProjOptions.Values[DccMapFileOptionName];
    SaveILinkMapFileType := ProjOptions.Values[ILinkMapFileTypeOptionName];
    ProjOptions.Values[MapFileOptionName] := MapFileOptionDetailed;
    ProjOptions.Values[DccMapFileOptionName] := MapFileOptionDetailed;
    ProjOptions.Values[ILinkMapFileTypeOptionName] := MapFileOptionDetailedSegments;
    {$ENDIF ~BDS6_UP}
    // workaround for MsBuild, the project has to be saved (seems useless with Delphi 2007 update 1)
    ProjOptions.ModifiedState := True;
    //TempActiveProject.Save(False, True);

    BuildOK := AProject.ProjectBuilder.BuildProject(cmOTABuild, False);

    {$IFDEF BDS6_UP}
    ActiveConfiguration.SetValue(MapFileOptionName, SaveMapFile);
    ActiveConfiguration.SetValue(DccMapFileOptionName, SaveDccMapFileType);
    ActiveConfiguration.SetValue(ILinkMapFileTypeOptionName, SaveILinkMapFileType);
    {$ELSE ~BDS6_UP}
    ProjOptions.Values[MapFileOptionName] := SaveMapFile;
    ProjOptions.Values[DccMapFileOptionName] := SaveDccMapFileType;
    ProjOptions.Values[ILinkMapFileTypeOptionName] := SaveILinkMapFileType;
    {$ENDIF ~BDS6_UP}
    // workaround for MsBuild, the project has to be saved (seems useless with Delphi 2007 update 1)
    ProjOptions.ModifiedState := True;
    //TempActiveProject.Save(False, True);

    if BuildOK then
    begin // Build was successful, continue ...
      Succ := FileExists(MapFileName) and FindExecutableName(MapFileName, OutputDirectory, ExecutableFileName);
      if Succ then
      begin // MAP files was created
        ProjectAnalyzerForm.SetFileName(ExecutableFileName, MapFileName, ProjectName);
        ProjectAnalyzerForm.Show;
      end;
      if (not VarIsOrdinal(SaveMapFile)) or (Integer(SaveMapFile) <> MapFileOptionDetailed) then
      begin // delete MAP and DRC file
        DeleteFile(MapFileName);
        DeleteFile(ChangeFileExt(MapFileName, DrcFileExtension));
      end;
    end;
    if not Succ then
    begin
      ProjectAnalyzerForm.StatusBarText := '';
      if BuildOK then
        MessageDlg(Format(LoadResString(@RsCantFindMAPFile), [MapFileName, ProjectFileName]), mtError, [mbOk], 0);
    end;
  finally
    JclDisablePostCompilationProcess := False;
  end;
end;

procedure TJclProjectAnalyzerExpert.RegisterCommands;
var
  IDEMainMenu: TMainMenu;
  IDEProjectItem: TMenuItem;
  IDEActionList: TActionList;
  I: Integer;
  ImageBmp: TBitmap;
  NTAServices: INTAServices;
  {$IFDEF BDS4_UP}
  OTAProjectManager: IOTAProjectManager;
  {$ENDIF BDS4_UP}
begin
  inherited RegisterCommands;

  NTAServices := GetNTAServices;

  // create actions
  FBuildAction := TAction.Create(nil);
  FBuildAction.Caption := Format(LoadResString(@RsAnalyzeActionCaption), [LoadResString(@RsProjectNone)]);
  FBuildAction.Visible := True;
  FBuildAction.OnExecute := ActionExecute;
  FBuildAction.OnUpdate := ActionUpdate;
  FBuildAction.Name := JclProjectAnalyzeActionName;
  ImageBmp := TBitmap.Create;
  try
    ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'PROJANALYZER');
    FBuildAction.ImageIndex := NTAServices.AddMasked(ImageBmp, clOlive);
  finally
    ImageBmp.Free;
  end;

  // create project manager notifier
  {$IFDEF BDS7_UP}
  OTAProjectManager := GetOTAProjectManager;
  FProjectManagerNotifierIndex := OTAProjectManager.AddMenuItemCreatorNotifier(TProjectManagerMultipleNotifier.Create(Self));
  {$ELSE ~BDS7_UP}
  {$IFDEF BDS4_UP}
  OTAProjectManager := GetOTAProjectManager;
  FProjectManagerNotifierIndex := OTAProjectManager.AddMenuCreatorNotifier(TProjectManagerSimpleNotifier.Create(Self,
    OTAProjectManager));
  {$ENDIF BDS4_UP}
  {$ENDIF ~BDS7_UP}

  // create menu item
  IDEMainMenu := NTAServices.MainMenu;
  IDEProjectItem := nil;
  with IDEMainMenu do
    for I := 0 to Items.Count - 1 do
      if Items[I].Name = 'ProjectMenu' then
      begin
        IDEProjectItem := Items[I];
        Break;
      end;
  if not Assigned(IDEProjectItem) then
    raise EJclExpertException.CreateRes(@RsENoProjectMenuItem);

  with IDEProjectItem do
    for I := 0 to Count - 1 do
      if Items[I].Name = 'ProjectInformationItem' then
      begin
        IDEActionList := TActionList(NTAServices.ActionList);
        if Assigned(Items[I].Action) then
          FBuildAction.Category := TContainedAction(Items[I].Action).Category;
        FBuildAction.ActionList := IDEActionList;
        TJclOTAActionExpert.RegisterAction(FBuildAction);
        FBuildMenuItem := TMenuItem.Create(nil);
        FBuildMenuItem.Name := JclProjectAnalyzeMenuName;
        FBuildMenuItem.Action := FBuildAction;

        IDEProjectItem.Insert(I + 1, FBuildMenuItem);

        System.Break;
      end;
  if not Assigned(FBuildMenuItem.Parent) then
    raise EJclExpertException.CreateRes(@RsAnalyseMenuItemNotInserted);
end;

procedure TJclProjectAnalyzerExpert.UnregisterCommands;
begin
  inherited UnregisterCommands;
  // remove notifier
  {$IFDEF BDS7_UP}
  // RAD Studio 2010 and newer
  if FProjectManagerNotifierIndex <> -1 then
    GetOTAProjectManager.RemoveMenuItemCreatorNotifier(FProjectManagerNotifierIndex);
  {$ELSE ~BDS7_UP}
  {$IFDEF BDS4_UP}
  // BDS 2006, RAD Studio 2007 and RAD Studio 2009
  if FProjectManagerNotifierIndex <> -1 then
    GetOTAProjectManager.RemoveMenuCreatorNotifier(FProjectManagerNotifierIndex);
  {$ENDIF BDS4_UP}
  {$ENDIF ~BDS7_UP}

  TJclOTAActionExpert.UnregisterAction(FBuildAction);
  FreeAndNil(FBuildMenuItem);
  FreeAndNil(FBuildAction);
end;

{$IFDEF BDS7_UP}
// RAD Studio 2010 and newer

//=== { TProjectManagerMultipleNotifier } ====================================

constructor TProjectManagerMultipleNotifier.Create(AProjectAnalyzer: TJclProjectAnalyzerExpert);
begin
  inherited Create;
  FProjectAnalyser := AProjectAnalyzer;
end;

procedure TProjectManagerMultipleNotifier.AddMenu(const Project: IOTAProject; const Ident: TStrings;
  const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
var
  AMenu: TJclOTAProjectManagerMenu;
begin
  if (not IsMultiSelect) and Assigned(Project) and (Ident.IndexOf(sProjectContainer) <> -1) then
  begin
    AMenu := TJclOTAProjectManagerMenu.Create;
    AMenu.Enabled := True;
    AMenu.Caption := Format(LoadResString(@RsAnalyzeActionCaption), [ExtractFileName(Project.FileName)]);
    AMenu.IsMultiSelectable := True;
    AMenu.OnExecute := MenuExecute;
    AMenu.Position := pmmpUserBuild;
    ProjectManagerMenuList.Add(AMenu);
  end;
end;

procedure TProjectManagerMultipleNotifier.MenuExecute(const MenuContextList: IInterfaceList);
var
  Index: Integer;
  MenuContext: IOTAProjectMenuContext;
  Project: IOTAProject;
begin
  try
    for Index := 0 to MenuContextList.Count - 1 do
    begin
      MenuContext := MenuContextList.Items[Index] as IOTAProjectMenuContext;
      Project := MenuContext.Project;
      if Project <> nil then
        FProjectAnalyser.AnalyzeProject(Project)
      else
        raise EJclExpertException.CreateRes(@RsENoActiveProject);
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

{$ELSE ~BDS7_UP}
{$IFDEF BDS4_UP}
// BDS 2006, RAD Studio 2007 and RAD Studio 2009

//=== { TProjectManagerSimpleNotifier } ======================================

constructor TProjectManagerSimpleNotifier.Create(AProjectAnalyzer: TJclProjectAnalyzerExpert;
  const AOTAProjectManager: IOTAProjectManager);
begin
  inherited Create;
  FProjectAnalyser := AProjectAnalyzer;
  FOTAProjectManager := AOTAProjectManager;
end;

function TProjectManagerSimpleNotifier.AddMenu(const Ident: string): TMenuItem;
var
  SelectedIdent: string;
  AProject: IOTAProject;
begin
  try
    SelectedIdent := Ident;
    AProject := FOTAProjectManager.GetCurrentSelection(SelectedIdent);
    if AProject <> nil then
    begin
      // root item
      Result := TMenuItem.Create(nil);
      Result.Visible := True;
      Result.Caption := Format(LoadResString(@RsAnalyzeActionCaption), [ExtractFileName(AProject.FileName)]);
      Result.OnClick := AnalyzeProjectMenuClick;
    end
    else
      raise EJclExpertException.CreateRes(@RsENoActiveProject);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := nil;
    end;
  end;
end;

procedure TProjectManagerSimpleNotifier.AnalyzeProjectMenuClick(Sender: TObject);
var
  TempProject: IOTAProject;
  SelectedIdent: string;
begin
  try
    SelectedIdent := '';
    TempProject := FOTAProjectManager.GetCurrentSelection(SelectedIdent);
    if TempProject <> nil then
      FProjectAnalyser.AnalyzeProject(TempProject)
    else
      raise EJclExpertException.CreateRes(@RsENoActiveProject);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

function TProjectManagerSimpleNotifier.CanHandle(const Ident: string): Boolean;
begin
  Result := Ident = sProjectContainer;
end;

{$ENDIF BDS4_UP}
{$ENDIF ~BDS7_UP}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
