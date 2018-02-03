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
{ The Original Code is JclDebugIdeImpl.pas.                                                        }
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

unit JclDebugIdeImpl;

{$I jcl.inc}
                              
interface

uses
  Windows, Classes, Menus, ActnList, SysUtils, Graphics, Dialogs, Controls, Forms, ToolsAPI,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOtaUtils, JclOtaConsts,
  JclDebugIdeConfigFrame,
  JclOtaActions;

type
  TJclDebugDataInfo = record
    ProjectName: string;
    ExecutableFileName: TFileName;
    MapFileSize, JclDebugDataSize: Integer;
    LinkerBugUnit: string;
    LineNumberErrors: Integer;
    Success: Boolean;
  end;

  TDebugExpertAction = (deGenerateJdbg, deInsertJdbg, deDeleteMapFile);
  TDebugExpertActions = set of TDebugExpertAction;

  TJclDebugExtension = class(TJclOTAExpert)
  private
    FResultInfo: array of TJclDebugDataInfo;
    FStoreResults: Boolean;
    FBuildError: Boolean;
    FDebugExpertAction: TDropDownAction;
    FDebugExpertItem: TMenuItem;
    FGenerateJdbgAction: TDropDownAction;
    FGenerateJdbgItem: TMenuItem;
    FInsertJdbgAction: TDropDownAction;
    FInsertJdbgItem: TMenuItem;
    FDeleteMapFileAction: TDropDownAction;
    FDeleteMapFileItem: TMenuItem;
    FDebugImageIndex: Integer;
    FNoDebugImageIndex: Integer;
    FGenerateJdbgImageIndex: Integer;
    FNoGenerateJdbgImageIndex: Integer;
    FInsertJdbgImageIndex: Integer;
    FNoInsertJdbgImageIndex: Integer;
    FDeleteMapFileImageIndex: Integer;
    FNoDeleteMapFileImageIndex: Integer;
    FSaveBuildProjectAction: TCustomAction;
    FSaveBuildProjectActionExecute: TNotifyEvent;
    FSaveBuildAllProjectsAction: TCustomAction;
    FSaveBuildAllProjectsActionExecute: TNotifyEvent;
    FIDENotifierIndex: Integer;
    {$IFDEF BDS4_UP}
    FProjectManagerNotifierIndex: Integer;
    {$ENDIF BDS4_UP}
    FConfigFrame: TJclDebugIdeConfigFrame;
    FGlobalStates: array [TDebugExpertAction] of TDebugExpertState;
    FQuiet: Boolean;
    procedure DebugExpertActionExecute(Sender: TObject);
    procedure DebugExpertActionUpdate(Sender: TObject);
    procedure DebugExpertMenuClick(Sender: TObject);
    procedure DebugExpertMenuDropDown(Sender: TObject);
    procedure DebugExpertSubMenuClick(Sender: TObject);
    procedure GenerateJdbgActionExecute(Sender: TObject);
    procedure GenerateJdbgActionUpdate(Sender: TObject);
    procedure GenerateJdbgMenuClick(Sender: TObject);
    procedure GenerateJdbgMenuDropDown(Sender: TObject);
    procedure GenerateJdbgSubMenuClick(Sender: TObject);
    procedure InsertJdbgActionExecute(Sender: TObject);
    procedure InsertJdbgActionUpdate(Sender: TObject);
    procedure InsertJdbgMenuClick(Sender: TObject);
    procedure InsertJdbgMenuDropDown(Sender: TObject);
    procedure InsertJdbgSubMenuClick(Sender: TObject);
    procedure DeleteMapFileActionExecute(Sender: TObject);
    procedure DeleteMapFileActionUpdate(Sender: TObject);
    procedure DeleteMapFileMenuClick(Sender: TObject);
    procedure DeleteMapFileMenuDropDown(Sender: TObject);
    procedure DeleteMapFileSubMenuClick(Sender: TObject);
    procedure LoadExpertValues;
    procedure SaveExpertValues;
    procedure BuildAllProjects(Sender: TObject);
    procedure BuildProject(Sender: TObject);
    procedure BeginStoreResults;
    procedure DisplayResults;
    procedure EndStoreResults;
    function GetGlobalState(Index: TDebugExpertAction): TDebugExpertState;
    procedure SetGlobalState(Index: TDebugExpertAction; Value: TDebugExpertState);
    function GetProjectState(Index: TDebugExpertAction; const AProject: IOTAProject): TDebugExpertState;
    procedure SetProjectState(Index: TDebugExpertAction; const AProject: IOTAProject; Value: TDebugExpertState);
    function GetProjectActions(const AProject: IOTAProject): TDebugExpertActions;
    procedure UpdateMenuItems(const ActiveProject: IOTAProject; AMenuItem: TMenuItem; CheckTag: Integer);
    procedure UpdateMenuCheckState(Sender: TMenuItem; DebugExpertAction: TDebugExpertAction);
  public
    constructor Create; reintroduce;
    procedure AfterCompile(const Project: IOTAProject; Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure RegisterCommands; override;
    procedure UnregisterCommands; override;
    procedure DisableExpert(const AProject: IOTAProject);
    property GlobalStates[Index: TDebugExpertAction]: TDebugExpertState read GetGlobalState
      write SetGlobalState;
    property ProjectStates[Index: TDebugExpertAction; const AProject: IOTAProject]: TDebugExpertState
      read GetProjectState write SetProjectState;
    property ProjectActions[const AProject: IOTAProject]: TDebugExpertActions read GetProjectActions;
  public
    function GetPageName: string; override;
    function GetFrameClass: TCustomFrameClass; override;
    procedure FrameCreated(AFrame: TCustomFrame); override;
    procedure DialogClosed(Accepted: Boolean); override;
  end;

  TIdeNotifier = class(TNotifierObject, IOTANotifier, IOTAIDENotifier, IOTAIDENotifier50
                       {$IFDEF BDS7_UP}, IOTAIDENotifier80{$ENDIF})
  private
    FDebugExtension: TJclDebugExtension;
    {$IFNDEF BDS7_UP}
    FCurrentProject: IOTAProject;
    {$ENDIF ~BDS7_UP}
  public
    constructor Create(ADebugExtension: TJclDebugExtension);
    { IOTAIDENotifier }
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
    { IOTAIDENotifier50 }
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
    { IOTAIDENotifier80 }
    procedure AfterCompile(const Project: IOTAProject; Succeeded: Boolean; IsCodeInsight: Boolean); overload;
  end;

  {$IFDEF BDS7_UP}
  // RAD Studio 2010 and newer
  TProjectManagerMultipleNotifier = class(TNotifierObject, IOTANotifier, IOTAProjectMenuItemCreatorNotifier)
  private
    FDebugExtension: TJclDebugExtension;
  public
    constructor Create(ADebugExtension: TJclDebugExtension);
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
    FDebugExtension: TJclDebugExtension;
    FOTAProjectManager: IOTAProjectManager;
    FNTAServices: INTAServices;
  public
    constructor Create(ADebugExtension: TJclDebugExtension; const ANTAServices: INTAServices;
      const AOTAProjectManager: IOTAProjectManager);
    procedure GenerateJdbgSubMenuClick(Sender: TObject);
    procedure InsertJdbgSubMenuClick(Sender: TObject);
    procedure DeleteMapFileSubMenuClick(Sender: TObject);
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

const
  DebugActionNames: array [TDebugExpertAction] of AnsiString =
    ( JclDebugGenerateJdbgSetting, // deGenerateJdbg
      JclDebugInsertJdbgSetting,   // deInsertJdbg
      JclDebugDeleteMapfileSetting // deDeleteMapFile
    );
  DebugActionValues: array [False..True] of AnsiString =
    ( 'OFF', 'ON' );
  ProjectManagerSubMenuNames: array[TDebugExpertAction, TDebugExpertState] of string =
    ( // deGenerateJdbg
      ( JclGenerateJdbgProjMenuName + 'AD',   // deAlwaysDisabled
        JclGenerateJdbgProjMenuName + 'PD',   // deProjectDisabled
        JclGenerateJdbgProjMenuName + 'PE',   // deProjectEnabled
        JclGenerateJdbgProjMenuName + 'AE'),  // deAlwaysEnabled
      // deInsertJdbg
      ( JclInsertJdbgProjMenuName + 'AD',     // deAlwaysDisabled
        JclInsertJdbgProjMenuName + 'PD',     // deProjectDisabled
        JclInsertJdbgProjMenuName + 'PE',     // deProjectEnabled
        JclInsertJdbgProjMenuName + 'AE'),    // deAlwaysEnabled
      // deDeleteMapFile
      ( JclDeleteMapFileProjMenuName + 'AD',  // deAlwaysDisabled
        JclDeleteMapFileProjMenuName + 'PD',  // deProjectDisabled
        JclDeleteMapFileProjMenuName + 'PE',  // deProjectEnabled
        JclDeleteMapFileProjMenuName + 'AE')  // deAlwaysEnabled
    );

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\debug\converter';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$R JclDebugIdeIcon.res}

uses
  TypInfo,
  Variants,
  {$IFDEF HAS_UNIT_SYSTEM_ACTIONS}
  System.Actions,
  {$ENDIF HAS_UNIT_SYSTEM_ACTIONS}
  {$IFDEF BDS8_UP}
  JclOtaAddinOptions,
  {$ENDIF BDS8_UP}
  JclBase, JclIDEUtils, JclDebug, JclDebugIdeResult,
  JclOtaResources;

var
  JCLWizardIndex: Integer = -1;

procedure Register;
begin
  try
    RegisterPackageWizard(TJclDebugExtension.Create);
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure JclWizardTerminate;
begin
  try
    if JCLWizardIndex <> -1 then
      TJclOTAExpertBase.GetOTAWizardServices.RemoveWizard(JCLWizardIndex);
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc; var TerminateProc: TWizardTerminateProc): Boolean stdcall;
begin
  try
    TerminateProc := JclWizardTerminate;

    JCLWizardIndex := TJclOTAExpertBase.GetOTAWizardServices.AddWizard(TJclDebugExtension.Create);

    Result := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;

//=== { TJclDebugExtension } =================================================

constructor TJclDebugExtension.Create;
begin
  inherited Create(JclDebugExpertRegKey);
end;

procedure TJclDebugExtension.AfterCompile(const Project: IOTAProject; Succeeded: Boolean);
var
  ProjectFileName, MapFileName, DrcFileName, ExecutableFileName, JdbgFileName: TFileName;
  OutputDirectory, LinkerBugUnit, ErrorMsg: string;
  Succ: Boolean;
  MapFileSize, JclDebugDataSize, LineNumberErrors, C: Integer;
  EnabledActions: TDebugExpertActions;
  OTAMessageServices: IOTAMessageServices;

  procedure OutputToolMessage(const Msg: string);
  begin
    if Assigned(Project) then
      OTAMessageServices.AddToolMessage(Project.FileName, Msg, LoadResString(@RsJclDebugMessagePrefix), 1, 1)
    else
      OTAMessageServices.AddToolMessage('', Msg, LoadResString(@RsJclDebugMessagePrefix), 1, 1);
  end;

begin
  if JclDisablePostCompilationProcess or (Project = nil) then
    Exit;

  OTAMessageServices := GetOTAMessageServices;
  EnabledActions := GetProjectActions(Project);
  if EnabledActions <> [] then
  begin
    ProjectFileName := Project.FileName;
    OutputDirectory := GetOutputDirectory(Project);
    MapFileName := GetMapFileName(Project);
    DrcFileName := GetDrcFileName(Project);
    JdbgFileName := ChangeFileExt(MapFileName, JclDbgFileExtension);

    if Succeeded then
    begin
      Screen.Cursor := crHourGlass;
      try
        LinkerBugUnit := '';
        LineNumberErrors := 0;

        Succ := FileExists(MapFileName);
        if not Succ then
          OutputToolMessage(Format(LoadResString(@RsEMapFileNotFound), [MapFileName, ProjectFileName]));

        // creation of .jdbg
        if Succ and (deGenerateJdbg in EnabledActions) then
        begin
          Succ := ConvertMapFileToJdbgFile(MapFileName, LinkerBugUnit, LineNumberErrors,
            MapFileSize, JclDebugDataSize);
          if Succ then
          begin
            if not FQuiet then
              OutputToolMessage(Format(LoadResString(@RsConvertedMapToJdbg), [MapFileName, MapFileSize, JclDebugDataSize]));
          end
          else
            OutputToolMessage(Format(LoadResString(@RsEMapConversion), [MapFileName]));
        end;

        // insertion of JEDI Debug Information into the binary
        if Succ and (deInsertJdbg in EnabledActions) then
        begin
          Succ := FindExecutableName(MapFileName, OutputDirectory, ExecutableFileName);
          if Succ then
          begin
            Succ := InsertDebugDataIntoExecutableFile(ExecutableFileName, MapFileName,
              LinkerBugUnit, MapFileSize, JclDebugDataSize, LineNumberErrors);
            if Succ then
            begin
              if not FQuiet then
                OutputToolMessage(Format(LoadResString(@RsInsertedJdbg), [MapFileName, MapFileSize, JclDebugDataSize]));
            end
            else
              OutputToolMessage(Format(LoadResString(@RsEMapInsertion), [MapFileName]));
          end
          else
            OutputToolMessage(Format(LoadResString(@RsEExecutableNotFound), [ProjectFileName]));
        end;

        // deletion of MAP files
        if Succ and (deDeleteMapFile in EnabledActions) then
        begin
          Succ := DeleteFile(MapFileName);
          if Succ then
          begin
            if not FQuiet then
              OutputToolMessage(Format(LoadResString(@RsDeletedMapFile), ['MAP', MapFileName]));
          end
          else
            OutputToolMessage(Format(LoadResString(@RsEFailedToDeleteMapFile), ['MAP', MapFileName]));

          if not FileExists(DrcFileName) then // Mantis #6488
            DrcFileName := ChangeFileExt(MapFileName, CompilerExtensionDRC);

          if DeleteFile(DrcFileName) then
            OutputToolMessage(Format(LoadResString(@RsDeletedMapFile), ['DRC', DrcFileName]))
          else
          begin
            ErrorMsg := SysErrorMessage(GetLastError);
            OutputToolMessage(Format(LoadResString(@RsEFailedToDeleteMapFile), ['DRC', DrcFileName]));
            OutputToolMessage(ErrorMsg);
          end;
        end;

        Screen.Cursor := crDefault;
      except
        Screen.Cursor := crDefault;
        raise;
      end;

      if FStoreResults then
      begin
        C := Length(FResultInfo);
        SetLength(FResultInfo, C + 1);
        FResultInfo[C].ProjectName := ExtractFileName(ProjectFileName);
        FResultInfo[C].ExecutableFileName := ExecutableFileName;
        FResultInfo[C].MapFileSize := MapFileSize;
        FResultInfo[C].JclDebugDataSize := JclDebugDataSize;
        FResultInfo[C].LinkerBugUnit := LinkerBugUnit;
        FResultInfo[C].LineNumberErrors := LineNumberErrors;
        FResultInfo[C].Success := Succ;
      end;
    end
    else
      FBuildError := True;
  end
end;

procedure TJclDebugExtension.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
var
  ProjOptions: IOTAProjectOptions;
  EnabledActions: TDebugExpertActions;
  HasILinkMapFileTypeOption, HasDccMapFileOption, HasMapFileOption,
  ChangeILinkMapFileTypeOption, ChangeDccMapFileOption, ChangeMapFileOption: Boolean;
  {$IFDEF BDS6_UP}
  ProjOptionsConfigurations: IOTAProjectOptionsConfigurations;
  ActiveConfiguration: IOTABuildConfiguration;
  OptionValue: string;
  {$ELSE ~BDS6_UP}
  OptionValue: Variant;
  {$ENDIF ~BDS6_UP}
begin
  EnabledActions := GetProjectActions(Project);
  if EnabledActions <> [] then
  begin
    if (deInsertJdbg in EnabledActions) and IsInstalledPackage(Project) then
    begin
      if MessageDlg(Format(LoadResString(@RsCantInsertToInstalledPackage), [Project.FileName]), mtError, [mbYes, mbNo], 0) = mrYes then
      begin
        DisableExpert(Project);
        MessageDlg(LoadResString(@RsDisabledDebugExpert), mtInformation, [mbOK], 0);
      end
      else
      begin
        Cancel := True;
        MessageDlg(LoadResString(@RsCompilationAborted), mtError, [mbOK], 0);
      end;
    end
    else
    begin
      ProjOptions := Project.ProjectOptions;
      if not Assigned(ProjOptions) then
        raise EJclExpertException.CreateRes(@RsENoProjectOptions);

      {$IFDEF BDS6_UP}
      Supports(ProjOptions, IOTAProjectOptionsConfigurations, ProjOptionsConfigurations);
      if not Assigned(ProjOptionsConfigurations) then
        raise EJclExpertException.CreateRes(@RsENoProjectOptionsConfigurations);

      // get the current build configuration
      ActiveConfiguration := ProjOptionsConfigurations.ActiveConfiguration;

      // retrieve options from this build configuration
      OptionValue := ActiveConfiguration.GetValue(ILinkMapFileTypeOptionName, True);
      HasILinkMapFileTypeOption := OptionValue <> '';
      ChangeILinkMapFileTypeOption := HasILinkMapFileTypeOption and (OptionValue <> MapFileOptionDetailedSegments);
      OptionValue := ActiveConfiguration.GetValue(DccMapFileOptionName, True);
      HasDccMapFileOption := OptionValue <> '';
      ChangeDccMapFileOption := HasDccMapFileOption and (OptionValue <> IntToStr(MapFileOptionDetailed));
      OptionValue := ActiveConfiguration.GetValue(MapFileOptionName, True);
      HasMapFileOption := OptionValue <> '';
      ChangeMapFileOption := HasMapFileOption and (OptionValue <> IntToStr(MapFileOptionDetailed));
      {$ELSE ~BDS6_UP}
      {$IFDEF BDS5}
      OptionValue := ProjOptions.Values[ILinkMapFileTypeOptionName];
      HasILinkMapFileTypeOption := not VarIsEmpty(OptionValue);
      ChangeILinkMapFileTypeOption := HasILinkMapFileTypeOption and (VarToStr(OptionValue) <> MapFileOptionDetailedSegments);
      OptionValue := ProjOptions.Values[DccMapFileOptionName];
      HasDccMapFileOption := not VarIsEmpty(OptionValue);
      ChangeDccMapFileOption := HasDccMapFileOption and (VarToStr(OptionValue) <> IntToStr(MapFileOptionDetailed));
      {$ELSE ~BDS5}
      HasILinkMapFileTypeOption := False;
      ChangeILinkMapFileTypeOption := HasILinkMapFileTypeOption;
      HasDccMapFileOption := False;
      ChangeDccMapFileOption := HasDccMapFileOption;
      {$ENDIF ~BDS5}
      OptionValue := ProjOptions.Values[MapFileOptionName];
      HasMapFileOption := not VarIsEmpty(OptionValue);
      ChangeMapFileOption := HasMapFileOption and (VarToStr(OptionValue) <> IntToStr(MapFileOptionDetailed));
      {$ENDIF ~BDS6_UP}

      if ChangeILinkMapFileTypeOption or ChangeDccMapFileOption or ChangeMapFileOption then
      begin
        if FQuiet or (MessageDlg(Format(LoadResString(@RsChangeMapFileOption), [ExtractFileName(Project.FileName)]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
        begin
          {$IFDEF BDS6_UP}
          if ChangeILinkMapFileTypeOption then
            ActiveConfiguration.Value[ILinkMapFileTypeOptionName] := MapFileOptionDetailedSegments;
          if ChangeDccMapFileOption then
            ActiveConfiguration.Value[DccMapFileOptionName] := IntToStr(MapFileOptionDetailed);
          if ChangeMapFileOption then
            ActiveConfiguration.Value[MapFileOptionName] := IntToStr(MapFileOptionDetailed);
          {$ELSE ~BDS6_UP}
          if ChangeILinkMapFileTypeOption then
            ProjOptions.Values[ILinkMapFileTypeOptionName] := MapFileOptionDetailedSegments;
          if ChangeDccMapFileOption then
            ProjOptions.Values[DccMapFileOptionName] := MapFileOptionDetailed;
          if ChangeMapFileOption then
            ProjOptions.Values[MapFileOptionName] := MapFileOptionDetailed;
          {$ENDIF ~BDS6_UP}
          ProjOptions.ModifiedState := True;
        end
        else
        begin
          DisableExpert(Project);
          MessageDlg(LoadResString(@RsDisabledDebugExpert), mtInformation, [mbOK], 0);
        end;
      end;
    end;
  end;
end;

procedure TJclDebugExtension.BeginStoreResults;
begin
  FBuildError := False;
  FStoreResults := True;
  FResultInfo := nil;
end;

procedure TJclDebugExtension.BuildAllProjects(Sender: TObject);
begin
  BeginStoreResults;
  try
    try
      FSaveBuildAllProjectsActionExecute(Sender);
      DisplayResults;
    except
      on EAbort do
        raise;
      on EFOpenError do // when ".ridl" files are not found by IDE, reraise the exception
        raise;
      on ExceptionObj: TObject do
      begin
        JclExpertShowExceptionDialog(ExceptionObj);
      end;
    end;
  finally
    EndStoreResults;
  end;
end;

procedure TJclDebugExtension.BuildProject(Sender: TObject);
begin
  BeginStoreResults;
  try
    try
      FSaveBuildProjectActionExecute(Sender);
      DisplayResults;
    except
      on EAbort do
        raise;
      on EFOpenError do // when ".ridl" files are not found by IDE, reraise the exception
        raise;
      on ExceptionObj: TObject do
      begin
        JclExpertShowExceptionDialog(ExceptionObj);
      end;
    end;
  finally
    EndStoreResults;
  end;
end;

procedure TJclDebugExtension.DialogClosed(Accepted: Boolean);
begin
  if Accepted then
  begin
    GlobalStates[deGenerateJdbg] := FConfigFrame.GenerateJdbgState;
    GlobalStates[deInsertJdbg] := FConfigFrame.InsertJdbgState;
    GlobalStates[deDeleteMapFile] := FConfigFrame.DeleteMapFileState;
    FQuiet := FConfigFrame.Quiet;
  end;
  FConfigFrame := nil;
end;

procedure TJclDebugExtension.DisableExpert(const AProject: IOTAProject);
begin
  ProjectStates[deGenerateJdbg, AProject] := DisableDebugExpertState(ProjectStates[deGenerateJdbg, AProject]);
  ProjectStates[deInsertJdbg, AProject] := DisableDebugExpertState(ProjectStates[deInsertJdbg, AProject]);
  ProjectStates[deDeleteMapFile, AProject] := DisableDebugExpertState(ProjectStates[deDeleteMapFile, AProject]);
end;

procedure TJclDebugExtension.DisplayResults;
var
  I: Integer;
begin
  if FBuildError or (Length(FResultInfo) = 0) then
    Exit;
  with TJclDebugResultForm.Create(Application, Settings) do
  try
    for I := 0 to Length(FResultInfo) - 1 do
      with ResultListView.Items.Add, FResultInfo[I] do
      begin
        Caption := ProjectName;
        if Success then
        begin
          SubItems.Add(IntToStr(MapFileSize));
          SubItems.Add(IntToStr(JclDebugDataSize));
          SubItems.Add(Format('%3.1f', [JclDebugDataSize * 100 / MapFileSize]));
          SubItems.Add(ExecutableFileName);
          SubItems.Add(LinkerBugUnit);
          if LineNumberErrors > 0 then
            SubItems.Add(IntToStr(LineNumberErrors))
          else
            SubItems.Add('');
          ImageIndex := 0;
        end
        else
        begin
          SubItems.Add('');
          SubItems.Add('');
          SubItems.Add('');
          SubItems.Add(ExecutableFileName);
          SubItems.Add(LinkerBugUnit);
          SubItems.Add('');
          ImageIndex := 1;
        end;
      end;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TJclDebugExtension.EndStoreResults;
begin
  FStoreResults := False;
  FResultInfo := nil;
end;

procedure TJclDebugExtension.FrameCreated(AFrame: TCustomFrame);
begin
  FConfigFrame := AFrame as TJclDebugIdeConfigFrame;
  FConfigFrame.GenerateJdbgState := GlobalStates[deGenerateJdbg];
  FConfigFrame.InsertJdbgState := GlobalStates[deInsertJdbg];
  FConfigFrame.DeleteMapFileState := GlobalStates[deDeleteMapFile];
  FConfigFrame.Quiet := FQuiet;
end;

procedure TJclDebugExtension.UpdateMenuItems(const ActiveProject: IOTAProject; AMenuItem: TMenuItem; CheckTag: Integer);
var
  Index: Integer;
  BMenuItem: TMenuItem;
begin
  for Index := 0 to AMenuItem.Count - 1 do
  begin
    BMenuItem := AMenuItem.Items[Index];
    BMenuItem.Enabled := (ActiveProject <> nil) or (BMenuItem.Tag = DebugExpertStateToInt(deAlwaysDisabled))
      or (BMenuItem.Tag = DebugExpertStateToInt(deAlwaysEnabled));
    BMenuItem.Checked := BMenuItem.Tag = CheckTag;
  end;
end;

procedure TJclDebugExtension.UpdateMenuCheckState(Sender: TMenuItem; DebugExpertAction: TDebugExpertAction);
var
  CheckTag: Integer;
  ActiveProject: IOTAProject;
begin
  try
    ActiveProject := GetActiveProject;
    if ActiveProject <> nil then
      CheckTag := DebugExpertStateToInt(ProjectStates[DebugExpertAction, ActiveProject])
    else
      CheckTag := DebugExpertStateToInt(GlobalStates[DebugExpertAction]);
    UpdateMenuItems(ActiveProject, Sender, CheckTag);
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TJclDebugExtension.DebugExpertActionExecute(Sender: TObject);
var
  ActiveProject: IOTAProject;
begin
  try
    ActiveProject := GetActiveProject;
    if ActiveProject <> nil then
    begin
      if ProjectActions[ActiveProject] <> [] then
      begin
        // disable all actions
        ProjectStates[deGenerateJdbg, ActiveProject] := DisableDebugExpertState(ProjectStates[deGenerateJdbg, ActiveProject]);
        ProjectStates[deInsertJdbg, ActiveProject] := DisableDebugExpertState(ProjectStates[deInsertJdbg, ActiveProject]);
        ProjectStates[deDeleteMapFile, ActiveProject] := DisableDebugExpertState(ProjectStates[deDeleteMapFile, ActiveProject]);
      end
      else
      begin
        // enable all actions
        ProjectStates[deGenerateJdbg, ActiveProject] := EnableDebugExpertState(ProjectStates[deGenerateJdbg, ActiveProject]);
        ProjectStates[deInsertJdbg, ActiveProject] := EnableDebugExpertState(ProjectStates[deInsertJdbg, ActiveProject]);
        ProjectStates[deDeleteMapFile, ActiveProject] := EnableDebugExpertState(ProjectStates[deDeleteMapFile, ActiveProject]);
      end;
    end;
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TJclDebugExtension.DebugExpertActionUpdate(Sender: TObject);
var
  AAction: TCustomAction;
  AEnabled: Boolean;
  ActiveProject: IOTAProject;
begin
  try
    AAction := Sender as TCustomAction;
    ActiveProject := GetActiveProject;
    AEnabled := ActiveProject <> nil;
    AAction.Enabled := AEnabled;
    if AEnabled then
    begin
      AAction.Checked := ProjectActions[ActiveProject] <> [];
      AAction.ImageIndex := FDebugImageIndex;
    end
    else
      AAction.ImageIndex := FNoDebugImageIndex;
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TJclDebugExtension.DebugExpertMenuClick(Sender: TObject);
var
  EnabledActions: TDebugExpertActions;
  ActiveProject: IOTAProject;
begin
  try
    ActiveProject := GetActiveProject;
    if ActiveProject <> nil then
      EnabledActions := ProjectActions[ActiveProject]
    else
      EnabledActions := [];
    FGenerateJdbgItem.Checked := deGenerateJdbg in EnabledActions;
    FInsertJdbgItem.Checked := deInsertJdbg in EnabledActions;
    FDeleteMapFileItem.Checked := deDeleteMapFile in EnabledActions;
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TJclDebugExtension.DebugExpertMenuDropDown(Sender: TObject);
var
  CheckTag: Integer;
  ActiveProject: IOTAProject;
  TestState: TDebugExpertState;
  IndexAction: TDebugExpertAction;
begin
  try
    ActiveProject := GetActiveProject;
    if ActiveProject <> nil then
    begin
      TestState := ProjectStates[Low(TDebugExpertAction), ActiveProject];
      CheckTag := DebugExpertStateToInt(TestState);
      for IndexAction := Succ(Low(TDebugExpertAction)) to High(TDebugExpertAction) do
        if TestState <> ProjectStates[IndexAction, ActiveProject] then
      begin
        CheckTag := -1;
        Break;
      end;
    end
    else
    begin
      TestState := GlobalStates[Low(TDebugExpertAction)];
      CheckTag := DebugExpertStateToInt(TestState);
      for IndexAction := Succ(Low(TDebugExpertAction)) to High(TDebugExpertAction) do
        if TestState <> GlobalStates[IndexAction] then
      begin
        CheckTag := -1;
        Break;
      end;
    end;

    UpdateMenuItems(ActiveProject, (Sender as TPopupMenu).Items, CheckTag);
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TJclDebugExtension.DebugExpertSubMenuClick(Sender: TObject);
var
  AState: TDebugExpertState;
  ActiveProject: IOTAProject;
begin
  try
    AState := IntToDebugExpertState((Sender as TComponent).Tag);
    ActiveProject := GetActiveProject;
    if ActiveProject <> nil then
    begin
      ProjectStates[deGenerateJdbg, ActiveProject] := AState;
      ProjectStates[deInsertJdbg, ActiveProject] := AState;
      ProjectStates[deDeleteMapFile, ActiveProject] := AState;
    end
    else
    begin
      GlobalStates[deGenerateJdbg] := AState;
      GlobalStates[deInsertJdbg] := AState;
      GlobalStates[deDeleteMapFile] := AState;
    end;
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TJclDebugExtension.DeleteMapFileActionExecute(Sender: TObject);
var
  ActiveProject: IOTAProject;
begin
  try
    ActiveProject := GetActiveProject;
    if ActiveProject <> nil then
      ProjectStates[deDeleteMapFile, ActiveProject] := ToggleDebugExpertState(ProjectStates[deDeleteMapFile, ActiveProject])
    else
      GlobalStates[deDeleteMapFile] := ToggleDebugExpertState(GlobalStates[deDeleteMapFile]);
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TJclDebugExtension.DeleteMapFileActionUpdate(Sender: TObject);
var
  AAction: TCustomAction;
  AEnabled: Boolean;
  ActiveProject: IOTAProject;
begin
  try
    AAction := Sender as TCustomAction;
    ActiveProject := GetActiveProject;
    AEnabled := ActiveProject <> nil;
    AAction.Enabled := AEnabled;
    if AEnabled then
    begin
      AAction.Checked := ProjectStates[deDeleteMapFile, ActiveProject] in [deAlwaysEnabled, deProjectEnabled];
      AAction.ImageIndex := FDeleteMapFileImageIndex;
    end
    else
    begin
      AAction.Checked := False;
      AAction.ImageIndex := FNoDeleteMapFileImageIndex;
    end;
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TJclDebugExtension.DeleteMapFileMenuClick(Sender: TObject);
begin
  UpdateMenuCheckState(Sender as TMenuItem, deDeleteMapFile);
end;

procedure TJclDebugExtension.DeleteMapFileMenuDropDown(Sender: TObject);
begin
  UpdateMenuCheckState((Sender as TPopupMenu).Items, deDeleteMapFile);
end;

procedure TJclDebugExtension.DeleteMapFileSubMenuClick(Sender: TObject);
var
  AState: TDebugExpertState;
  ActiveProject: IOTAProject;
begin
  try
    AState := IntToDebugExpertState((Sender as TComponent).Tag);
    ActiveProject := GetActiveProject;
    if ActiveProject <> nil then
      ProjectStates[deDeleteMapFile, ActiveProject] := AState
    else
      GlobalStates[deDeleteMapFile] := AState;
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TJclDebugExtension.GenerateJdbgActionExecute(Sender: TObject);
var
  ActiveProject: IOTAProject;
begin
  try
    ActiveProject := GetActiveProject;
    if ActiveProject <> nil then
      ProjectStates[deGenerateJdbg, ActiveProject] := ToggleDebugExpertState(ProjectStates[deGenerateJdbg, ActiveProject])
    else
      GlobalStates[deGenerateJdbg] := ToggleDebugExpertState(GlobalStates[deGenerateJdbg]);
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TJclDebugExtension.GenerateJdbgActionUpdate(Sender: TObject);
var
  AAction: TCustomAction;
  AEnabled: Boolean;
  ActiveProject: IOTAProject;
begin
  try
    AAction := Sender as TCustomAction;
    ActiveProject := GetActiveProject;
    AEnabled := ActiveProject <> nil;
    AAction.Enabled := AEnabled;
    if AEnabled then
    begin
      AAction.Checked := ProjectStates[deGenerateJdbg, ActiveProject] in [deAlwaysEnabled, deProjectEnabled];
      AAction.ImageIndex := FGenerateJdbgImageIndex;
    end
    else
    begin
      AAction.Checked := False;
      AAction.ImageIndex := FNoGenerateJdbgImageIndex;
    end;
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TJclDebugExtension.GenerateJdbgMenuClick(Sender: TObject);
begin
  UpdateMenuCheckState(Sender as TMenuItem, deGenerateJdbg);
end;

procedure TJclDebugExtension.GenerateJdbgMenuDropDown(Sender: TObject);
begin
  UpdateMenuCheckState((Sender as TPopupMenu).Items, deGenerateJdbg);
end;

procedure TJclDebugExtension.GenerateJdbgSubMenuClick(Sender: TObject);
var
  AState: TDebugExpertState;
  ActiveProject: IOTAProject;
begin
  try
    AState := IntToDebugExpertState((Sender as TComponent).Tag);
    ActiveProject := GetActiveProject;
    if ActiveProject <> nil then
      ProjectStates[deGenerateJdbg, ActiveProject] := AState
    else
      GlobalStates[deGenerateJdbg] := AState;
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

function TJclDebugExtension.GetFrameClass: TCustomFrameClass;
begin
  Result := TJclDebugIdeConfigFrame;
end;

function TJclDebugExtension.GetGlobalState(Index: TDebugExpertAction): TDebugExpertState;
begin
  Result := FGlobalStates[Index];
end;

function TJclDebugExtension.GetPageName: string;
begin
  Result := LoadResString(@RsDebugConfigPageCaption);
end;

function TJclDebugExtension.GetProjectActions(const AProject: IOTAProject): TDebugExpertActions;
var
  PropIDs, PropValues: TDynAnsiStringArray;
  Index: TDebugExpertAction;
begin
  SetLength(PropIDs, Integer(High(TDebugExpertAction)) - Integer(Low(TDebugExpertAction)) + 1);
  for Index := Low(TDebugExpertAction) to High(TDebugExpertAction) do
    PropIDs[Integer(Index)] := DebugActionNames[Index];
  PropValues := GetProjectProperties(AProject, PropIDs);
  Result := [];
  for Index := Low(TDebugExpertAction) to High(TDebugExpertAction) do
    case FGlobalStates[Index] of
      deAlwaysEnabled:
        Include(Result, Index);
      deProjectEnabled:
        if PropValues[Integer(Index)] <> DebugActionValues[False] then
          Include(Result, Index);
      deProjectDisabled:
        if PropValues[Integer(Index)] = DebugActionValues[True] then
          Include(Result, Index);
    end;
end;

function TJclDebugExtension.GetProjectState(Index: TDebugExpertAction; const AProject: IOTAProject): TDebugExpertState;
var
  PropIDs: TDynAnsiStringArray;
begin
  case FGlobalStates[Index] of
    deAlwaysDisabled:
      Result := deAlwaysDisabled;
    deProjectDisabled:
      begin
        SetLength(PropIDs, 1);
        PropIDs[0] := DebugActionNames[Index];
        if GetProjectProperties(AProject, PropIDs)[0] = DebugActionValues[True] then
          Result := deProjectEnabled
        else
          Result := deProjectDisabled;
      end;
    deProjectEnabled:
      begin
        SetLength(PropIDs, 1);
        PropIDs[0] := DebugActionNames[Index];
        if GetProjectProperties(AProject, PropIDs)[0] <> DebugActionValues[False] then
          Result := deProjectEnabled
        else
          Result := deProjectDisabled;
      end;
    deAlwaysEnabled:
      Result := deAlwaysEnabled;
  else
    raise EJclExpertException.CreateResFmt(@RsEInvalidDebugExpertState, [Integer(FGlobalStates[Index])]);
  end;
end;

procedure TJclDebugExtension.InsertJdbgActionExecute(Sender: TObject);
var
  ActiveProject: IOTAProject;
begin
  try
    ActiveProject := GetActiveProject;
    if ActiveProject <> nil then
      ProjectStates[deInsertJdbg, ActiveProject] := ToggleDebugExpertState(ProjectStates[deInsertJdbg, ActiveProject])
    else
      GlobalStates[deInsertJdbg] := ToggleDebugExpertState(GlobalStates[deInsertJdbg]);
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TJclDebugExtension.InsertJdbgActionUpdate(Sender: TObject);
var
  AAction: TCustomAction;
  AEnabled: Boolean;
  ActiveProject: IOTAProject;
begin
  try
    AAction := Sender as TCustomAction;
    ActiveProject := GetActiveProject;
    AEnabled := ActiveProject <> nil;
    AAction.Enabled := AEnabled;
    if AEnabled then
    begin
      AAction.Checked := ProjectStates[deInsertJdbg, ActiveProject] in [deAlwaysEnabled, deProjectEnabled];
      AAction.ImageIndex := FInsertJdbgImageIndex
    end
    else
    begin
      AAction.Checked := False;
      AAction.ImageIndex := FNoInsertJdbgImageIndex;
    end;
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TJclDebugExtension.InsertJdbgMenuClick(Sender: TObject);
begin
  UpdateMenuCheckState(Sender as TMenuItem, deInsertJdbg);
end;

procedure TJclDebugExtension.InsertJdbgMenuDropDown(Sender: TObject);
begin
  UpdateMenuCheckState((Sender as TPopupMenu).Items, deInsertJdbg);
end;

procedure TJclDebugExtension.InsertJdbgSubMenuClick(Sender: TObject);
var
  AState: TDebugExpertState;
  ActiveProject: IOTAProject;
begin
  try
    AState := IntToDebugExpertState((Sender as TComponent).Tag);
    ActiveProject := GetActiveProject;
    if ActiveProject <> nil then
      ProjectStates[deInsertJdbg, ActiveProject] := AState
    else
      GlobalStates[deInsertJdbg] := AState;
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TJclDebugExtension.LoadExpertValues;
begin
  GlobalStates[deGenerateJdbg] := IntToDebugExpertState(Settings.LoadInteger(JclDebugGenerateJdbgRegValue, 0));
  GlobalStates[deInsertJdbg] := IntToDebugExpertState(Settings.LoadInteger(JclDebugInsertJdbgRegValue, 0));
  GlobalStates[deDeleteMapFile] := IntToDebugExpertState(Settings.LoadInteger(JclDebugDeleteMapFileRegValue, 0));
  FQuiet := Settings.LoadBool(JclDebugQuietSetting, False);
end;

procedure TJclDebugExtension.SaveExpertValues;
begin
  Settings.SaveInteger(JclDebugGenerateJdbgRegValue, DebugExpertStateToInt(GlobalStates[deGenerateJdbg]));
  Settings.SaveInteger(JclDebugInsertJdbgRegValue, DebugExpertStateToInt(GlobalStates[deInsertJdbg]));
  Settings.SaveInteger(JclDebugDeleteMapFileRegValue, DebugExpertStateToInt(GlobalStates[deDeleteMapFile]));
  Settings.SaveBool(JclDebugQuietSetting, FQuiet);
end;

procedure TJclDebugExtension.SetGlobalState(Index: TDebugExpertAction; Value: TDebugExpertState);
begin
  FGlobalStates[Index] := Value;
end;

procedure TJclDebugExtension.SetProjectState(Index: TDebugExpertAction; const AProject: IOTAProject;
  Value: TDebugExpertState);
var
  PropIDs, PropValues: TDynAnsiStringArray;
begin
  case Value of
    deAlwaysDisabled:
      FGlobalStates[Index] := deAlwaysDisabled;
    deProjectDisabled:
      begin
        if not (GlobalStates[Index] in [deProjectDisabled, deProjectEnabled]) then
          FGlobalStates[Index] := deProjectDisabled;
        SetLength(PropIDs, 1);
        PropIDs[0] := DebugActionNames[Index];
        SetLength(PropValues, 1);
        PropValues[0] := DebugActionValues[False];
        if SetProjectProperties(AProject, PropIDs, PropValues) <> 1 then
          MessageDlg(LoadResString(@RsEProjectPropertyFailed), mtError, [mbAbort],0);
      end;
    deProjectEnabled:
      begin
        if not (GlobalStates[Index] in [deProjectDisabled, deProjectEnabled]) then
          FGlobalStates[Index] := deProjectEnabled;
        SetLength(PropIDs, 1);
        PropIDs[0] := DebugActionNames[Index];
        SetLength(PropValues, 1);
        PropValues[0] := DebugActionValues[True];
        if SetProjectProperties(AProject, PropIDs, PropValues) <> 1 then
          MessageDlg(LoadResString(@RsEProjectPropertyFailed), mtError, [mbAbort],0);
      end;
    deAlwaysEnabled:
      FGlobalStates[Index] := deAlwaysEnabled;
  end;
end;

procedure TJclDebugExtension.RegisterCommands;

  procedure FillMenu(AMenuItem: TMenuItem; AEvent: TNotifyEvent);
  var
    BMenuItem: TMenuItem;
  begin
    BMenuItem := TMenuItem.Create(AMenuItem);
    BMenuItem.Caption := LoadResString(@RsAlwaysEnabled);
    BMenuItem.RadioItem := True;
    BMenuItem.Tag := DebugExpertStateToInt(deAlwaysEnabled);
    BMenuItem.OnClick := AEvent;
    AMenuItem.Add(BMenuItem);

    BMenuItem := TMenuItem.Create(AMenuItem);
    BMenuItem.Caption := LoadResString(@RsProjectEnabled);
    BMenuItem.RadioItem := True;
    BMenuItem.Tag := DebugExpertStateToInt(deProjectEnabled);
    BMenuItem.OnClick := AEvent;
    AMenuItem.Add(BMenuItem);

    BMenuItem := TMenuItem.Create(AMenuItem);
    BMenuItem.Caption := LoadResString(@RsProjectDisabled);
    BMenuItem.RadioItem := True;
    BMenuItem.Tag := DebugExpertStateToInt(deProjectDisabled);
    BMenuItem.OnClick := AEvent;
    AMenuItem.Add(BMenuItem);

    BMenuItem := TMenuItem.Create(AMenuItem);
    BMenuItem.Caption := LoadResString(@RsAlwaysDisabled);
    BMenuItem.RadioItem := True;
    BMenuItem.Tag := DebugExpertStateToInt(deAlwaysDisabled);
    BMenuItem.OnClick := AEvent;
    AMenuItem.Add(BMenuItem);
  end;

var
  IDEMainMenu: TMainMenu;
  IDEProjectItem: TMenuItem;
  IDEActionList: TActionList;
  I: Integer;
  ImageBmp: TBitmap;
  NTAServices: INTAServices;
  OTAServices: IOTAServices;
  {$IFDEF BDS4_UP}
  OTAProjectManager: IOTAProjectManager;
  {$ENDIF BDS4_UP}
begin
  inherited RegisterCommands;

  NTAServices := GetNTAServices;
  OTAServices := GetOTAServices;

  IDEActionList := TActionList(NTAServices.ActionList);
  IDEMainMenu := NTAServices.MainMenu;
  ImageBmp := TBitmap.Create;
  try
    // load images
    {$IFDEF COMPILER14_UP}
    NTAServices.ImageList.BeginUpdate;
    try
    {$ENDIF COMPILER14_UP}
      ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLDEBUG');
      FDebugImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
      ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLNODEBUG');
      FNoDebugImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
      ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLGENERATEJDBG');
      FGenerateJdbgImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
      ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLNOGENERATEJDBG');
      FNoGenerateJdbgImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
      ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLINSERTJDBG');
      FInsertJdbgImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
      ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLNOINSERTJDBG');
      FNoInsertJdbgImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
      ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLDELETEMAP');
      FDeleteMapFileImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
      ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLNODELETEMAP');
      FNoDeleteMapFileImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
    {$IFDEF COMPILER14_UP}
    finally
      NTAServices.ImageList.EndUpdate;
    end;
    {$ENDIF COMPILER14_UP}

    // create actions
    FDebugExpertAction := TDropDownAction.Create(nil);
    FDebugExpertAction.Caption := LoadResString(@RsDebugExpertCaption);
    FDebugExpertAction.Visible := True;
    FDebugExpertAction.ImageIndex := FDebugImageIndex;
    FDebugExpertAction.OnUpdate := DebugExpertActionUpdate;
    FDebugExpertAction.OnExecute := DebugExpertActionExecute;
    FDebugExpertAction.ActionList := IDEActionList;
    FDebugExpertAction.Name := JclDebugExpertActionName;
    FDebugExpertAction.DropdownMenu := TPopupMenu.Create(nil);
    FDebugExpertAction.DropdownMenu.OnPopup := DebugExpertMenuDropDown;
    FDebugExpertAction.DropdownMenu.AutoPopup := True;
    FillMenu(FDebugExpertAction.DropDownMenu.Items, DebugExpertSubMenuClick);
    TJclOTAActionExpert.RegisterAction(FDebugExpertAction);

    FGenerateJdbgAction := TDropDownAction.Create(nil);
    FGenerateJdbgAction.Caption := LoadResString(@RsDebugGenerateJdbg);
    FGenerateJdbgAction.Visible := True;
    FGenerateJdbgAction.ImageIndex := FGenerateJdbgImageIndex;
    FGenerateJdbgAction.OnUpdate := GenerateJdbgActionUpdate;
    FGenerateJdbgAction.OnExecute := GenerateJdbgActionExecute;
    FGenerateJdbgAction.ActionList := IDEActionList;
    FGenerateJdbgAction.Name := JclGenerateJdbgActionName;
    FGenerateJdbgAction.DropdownMenu := TPopupMenu.Create(nil);
    FGenerateJdbgAction.DropdownMenu.OnPopup := GenerateJdbgMenuDropDown;
    FGenerateJdbgAction.DropdownMenu.AutoPopup := True;
    FillMenu(FGenerateJdbgAction.DropDownMenu.Items, GenerateJdbgSubMenuClick);
    TJclOTAActionExpert.RegisterAction(FGenerateJdbgAction);

    FInsertJdbgAction := TDropDownAction.Create(nil);
    FInsertJdbgAction.Caption := LoadResString(@RsDebugInsertJdbg);
    FInsertJdbgAction.Visible := True;
    FInsertJdbgAction.ImageIndex := FInsertJdbgImageIndex;
    FInsertJdbgAction.OnUpdate := InsertJdbgActionUpdate;
    FInsertJdbgAction.OnExecute := InsertJdbgActionExecute;
    FInsertJdbgAction.ActionList := IDEActionList;
    FInsertJdbgAction.Name := JclInsertJdbgActionName;
    FInsertJdbgAction.DropdownMenu := TPopupMenu.Create(nil);
    FInsertJdbgAction.DropdownMenu.OnPopup := InsertJdbgMenuDropDown;
    FInsertJdbgAction.DropdownMenu.AutoPopup := True;
    FillMenu(FInsertJdbgAction.DropDownMenu.Items, InsertJdbgSubMenuClick);
    TJclOTAActionExpert.RegisterAction(FInsertJdbgAction);

    FDeleteMapFileAction := TDropDownAction.Create(nil);
    FDeleteMapFileAction.Caption := LoadResString(@RsDeleteMapFile);
    FDeleteMapFileAction.Visible := True;
    FDeleteMapFileAction.ImageIndex := FDeleteMapFileImageIndex;
    FDeleteMapFileAction.OnUpdate := DeleteMapFileActionUpdate;
    FDeleteMapFileAction.OnExecute := DeleteMapFileActionExecute;
    FDeleteMapFileAction.ActionList := IDEActionList;
    FDeleteMapFileAction.Name := JclDeleteMapFileActionName;
    FDeleteMapFileAction.DropdownMenu := TPopupMenu.Create(nil);
    FDeleteMapFileAction.DropdownMenu.OnPopup := DeleteMapFileMenuDropDown;
    FDeleteMapFileAction.DropdownMenu.AutoPopup := True;
    FillMenu(FDeleteMapFileAction.DropDownMenu.Items, DeleteMapFileSubMenuClick);
    TJclOTAActionExpert.RegisterAction(FDeleteMapFileAction);

    // create menu items
    FDebugExpertItem := TMenuItem.Create(nil);
    FDebugExpertItem.Name := JclDebugExpertMenuName;
    FDebugExpertItem.Caption := LoadResString(@RsDebugExpertCaption);
    FDebugExpertItem.OnClick := DebugExpertMenuClick;
    FDebugExpertItem.ImageIndex := FDebugImageIndex;

    FGenerateJdbgItem := TMenuItem.Create(nil);
    FGenerateJdbgItem.Name := JclGenerateJdbgMenuName;
    FGenerateJdbgItem.Caption := LoadResString(@RsDebugGenerateJdbg);
    FGenerateJdbgItem.OnClick := GenerateJdbgMenuClick;
    FGenerateJdbgItem.ImageIndex := FGenerateJdbgImageIndex;
    FillMenu(FGenerateJdbgItem, GenerateJdbgSubMenuClick);
    FDebugExpertItem.Add(FGenerateJdbgItem);

    FInsertJdbgItem := TMenuItem.Create(nil);
    FInsertJdbgItem.Name := JclInsertJdbgMenuName;
    FInsertJdbgItem.Caption := LoadResString(@RsDebugInsertJdbg);
    FInsertJdbgItem.OnClick := InsertJdbgMenuClick;
    FInsertJdbgItem.ImageIndex := FInsertJdbgImageIndex;
    FillMenu(FInsertJdbgItem, InsertJdbgSubMenuClick);
    FDebugExpertItem.Add(FInsertJdbgItem);

    FDeleteMapFileItem := TMenuItem.Create(nil);
    FDeleteMapFileItem.Name := JclDeleteMapFileMenuName;
    FDeleteMapFileItem.Caption := LoadResString(@RsDeleteMapFile);
    FDeleteMapFileItem.OnClick := DeleteMapFileMenuClick;
    FDeleteMapFileItem.ImageIndex := FDeleteMapFileImageIndex;
    FillMenu(FDeleteMapFileItem, DeleteMapFileSubMenuClick);
    FDebugExpertItem.Add(FDeleteMapFileItem);
  finally
    ImageBmp.Free;
  end;

  // register notifiers
  FIDENotifierIndex := OTAServices.AddNotifier(TIdeNotifier.Create(Self));
  {$IFDEF BDS7_UP}
  OTAProjectManager := GetOTAProjectManager;
  FProjectManagerNotifierIndex := OTAProjectManager.AddMenuItemCreatorNotifier(TProjectManagerMultipleNotifier.Create(Self));
  {$ELSE ~BDS7_UP}
  {$IFDEF BDS4_UP}
  OTAProjectManager := GetOTAProjectManager;
  FProjectManagerNotifierIndex := OTAProjectManager.AddMenuCreatorNotifier(TProjectManagerSimpleNotifier.Create(Self,
    NTAServices, OTAProjectManager));
  {$ENDIF BDS4_UP}
  {$ENDIF ~BDS7_UP}

  LoadExpertValues;

  // insert menus
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
      if Items[I].Name = 'ProjectOptionsItem' then
      begin
        if Assigned(Items[I].Action) then
        begin
          FDebugExpertAction.Category := TContainedAction(Items[I].Action).Category;
          FGenerateJdbgAction.Category := FDebugExpertAction.Category;
          FInsertJdbgAction.Category := FDebugExpertAction.Category;
          FDeleteMapFileAction.Category := FDebugExpertAction.Category;
        end;
        IDEProjectItem.Insert(I + 1, FDebugExpertItem);
        System.Break;
      end;
  if not Assigned(FDebugExpertItem.Parent) then
     raise EJclExpertException.CreateRes(@RsEInsertDataMenuItemNotInserted);

  // hook actions
  FSaveBuildProjectAction := nil;
  with IDEActionList do
    for I := 0 to ActionCount - 1 do
      if Actions[I].Name = 'ProjectBuildCommand' then
      begin
        FSaveBuildProjectAction := TCustomAction(Actions[I]);
        FSaveBuildProjectActionExecute := FSaveBuildProjectAction.OnExecute;
        FSaveBuildProjectAction.OnExecute := BuildProject;
        Break;
      end;
  if not Assigned(FSaveBuildProjectAction) then
     raise EJclExpertException.CreateRes(@RsENoBuildAction);

  FSaveBuildAllProjectsAction := nil;
  with IDEActionList do
    for I := 0 to ActionCount - 1 do
      if Actions[I].Name = 'ProjectBuildAllCommand' then
      begin
        FSaveBuildAllProjectsAction := TCustomAction(Actions[I]);
        FSaveBuildAllProjectsActionExecute := FSaveBuildAllProjectsAction.OnExecute;
        FSaveBuildAllProjectsAction.OnExecute := BuildAllProjects;
        Break;
      end;
  if not Assigned(FSaveBuildAllProjectsAction) then
     raise EJclExpertException.CreateRes(@RsENoBuildAllAction);
end;

procedure TJclDebugExtension.UnregisterCommands;
begin
  inherited UnregisterCommands;
  {$IFDEF BDS7_UP}
  if FProjectManagerNotifierIndex <> -1 then
    GetOTAProjectManager.RemoveMenuItemCreatorNotifier(FProjectManagerNotifierIndex);
  {$ELSE ~BDS7_UP}
  {$IFDEF BDS4_UP}
  if FProjectManagerNotifierIndex <> -1 then
    GetOTAProjectManager.RemoveMenuCreatorNotifier(FProjectManagerNotifierIndex);
  {$ENDIF BDS4_UP}
  {$ENDIF ~BDS7_UP}
  if FIDENotifierIndex <> -1 then
    GetOTAServices.RemoveNotifier(FIDENotifierIndex);
  // save settings
  SaveExpertValues;

  // unhook actions
  FSaveBuildProjectAction.OnExecute := FSaveBuildProjectActionExecute;
  FSaveBuildAllProjectsAction.OnExecute := FSaveBuildAllProjectsActionExecute;

  // remove menu items
  if FDebugExpertAction <> nil then
    FDebugExpertAction.DropdownMenu.Free;
  if FGenerateJdbgAction <> nil then
    FGenerateJdbgAction.DropdownMenu.Free;
  if FInsertJdbgAction <> nil then
    FInsertJdbgAction.DropdownMenu.Free;
  if FDeleteMapFileAction <> nil then
    FDeleteMapFileAction.DropdownMenu.Free;
  FGenerateJdbgItem.Free;
  FInsertJdbgItem.Free;
  FDeleteMapFileItem.Free;
  FDebugExpertItem.Free;

  // remove actions
  TJclOTAActionExpert.UnregisterAction(FDeleteMapFileAction);
  TJclOTAActionExpert.UnregisterAction(FInsertJdbgAction);
  TJclOTAActionExpert.UnregisterAction(FGenerateJdbgAction);
  TJclOTAActionExpert.UnregisterAction(FDebugExpertAction);
  FDeleteMapFileAction.Free;
  FInsertJdbgAction.Free;
  FGenerateJdbgAction.Free;
  FDebugExpertAction.Free;
end;

//=== { TIdeNotifier } =======================================================

constructor TIdeNotifier.Create(ADebugExtension: TJclDebugExtension);
begin
  inherited Create;
  FDebugExtension := ADebugExtension;
end;

procedure TIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TIdeNotifier.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin
  {$IFNDEF BDS7_UP}
  try
    try
      if not IsCodeInsight then
        FDebugExtension.AfterCompile(FCurrentProject, Succeeded);
    finally
      FCurrentProject := nil;
    end;
  except
    on EAbort do
      raise;
    on ExceptionObj: Exception do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
  {$ENDIF ~BDS7_UP}
end;

procedure TIdeNotifier.AfterCompile(const Project: IOTAProject; Succeeded: Boolean; IsCodeInsight: Boolean);
begin
  try
    if not IsCodeInsight then
      FDebugExtension.AfterCompile(Project, Succeeded);
  except
    on EAbort do
      raise;
    on ExceptionObj: Exception do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  try
    if not IsCodeInsight then
    begin
      {$IFNDEF BDS7_UP}
      FCurrentProject := Project;
      {$ENDIF ~BDS7_UP}
      FDebugExtension.BeforeCompile(Project, Cancel);
    end;
  except
    on EAbort do
      raise;
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
end;

procedure TIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
end;

{$IFDEF BDS7_UP}
// RAD Studio 2010 and newer

//=== { TProjectManagerMultipleNotifier } ====================================

constructor TProjectManagerMultipleNotifier.Create(ADebugExtension: TJclDebugExtension);
begin
  inherited Create;
  FDebugExtension := ADebugExtension;
end;

procedure TProjectManagerMultipleNotifier.AddMenu(const Project: IOTAProject; const Ident: TStrings;
  const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);

  procedure FillProjMenu(const ParentVerb: string; Action: TDebugExpertAction);
  var
    BMenu: TJclOTAProjectManagerMenu;
    State: TDebugExpertState;
  begin
    State := FDebugExtension.ProjectStates[Action, Project];

    BMenu := TJclOTAProjectManagerMenu.Create;
    BMenu.Enabled := True;
    BMenu.Checked := State = deAlwaysEnabled;
    BMenu.Caption := LoadResString(@RsAlwaysEnabled);
    BMenu.Verb := ProjectManagerSubMenuNames[Action, deAlwaysEnabled];
    BMenu.Parent := ParentVerb;
    BMenu.OnExecute := MenuExecute;
    BMenu.Position := pmmpUserOptions + 11;
    ProjectManagerMenuList.Add(BMenu);

    BMenu := TJclOTAProjectManagerMenu.Create;
    BMenu.Enabled := True;
    BMenu.Checked := State = deProjectEnabled;
    BMenu.Caption := LoadResString(@RsProjectEnabled);
    BMenu.Verb := ProjectManagerSubMenuNames[Action, deProjectEnabled];
    BMenu.Parent := ParentVerb;
    BMenu.OnExecute := MenuExecute;
    BMenu.Position := pmmpUserOptions + 12;
    ProjectManagerMenuList.Add(BMenu);

    BMenu := TJclOTAProjectManagerMenu.Create;
    BMenu.Enabled := True;
    BMenu.Checked := State = deProjectDisabled;
    BMenu.Caption := LoadResString(@RsProjectDisabled);
    BMenu.Verb := ProjectManagerSubMenuNames[Action, deProjectDisabled];
    BMenu.Parent := ParentVerb;
    BMenu.OnExecute := MenuExecute;
    BMenu.Position := pmmpUserOptions + 13;
    ProjectManagerMenuList.Add(BMenu);

    BMenu := TJclOTAProjectManagerMenu.Create;
    BMenu.Enabled := True;
    BMenu.Checked := State = deAlwaysDisabled;
    BMenu.Caption := LoadResString(@RsAlwaysDisabled);
    BMenu.Verb := ProjectManagerSubMenuNames[Action, deAlwaysDisabled];
    BMenu.Parent := ParentVerb;
    BMenu.OnExecute := MenuExecute;
    BMenu.Position := pmmpUserOptions + 14;
    ProjectManagerMenuList.Add(BMenu);
  end;

var
  AMenu: TJclOTAProjectManagerMenu;
  Actions: TDebugExpertActions;
begin
  try
    if (not IsMultiSelect) and Assigned(Project) and (Ident.IndexOf(sProjectContainer) <> -1) then
    begin
      Actions := FDebugExtension.ProjectActions[Project];

      AMenu := TJclOTAProjectManagerMenu.Create;
      AMenu.Enabled := True;
      AMenu.Checked := Actions <> [];
      AMenu.Caption := LoadResString(@RsDebugExpertCaption);
      AMenu.Verb := JclDebugExpertProjMenuName;
      AMenu.Position := pmmpUserOptions;
      ProjectManagerMenuList.Add(AMenu);

      AMenu := TJclOTAProjectManagerMenu.Create;
      AMenu.Enabled := True;
      AMenu.Caption := LoadResString(@RsDebugGenerateJdbg);
      AMenu.Parent := JclDebugExpertProjMenuName;
      AMenu.Verb := JclGenerateJdbgProjMenuName;
      AMenu.Position := pmmpUserOptions + 1;
      ProjectManagerMenuList.Add(AMenu);
      FillProjMenu(JclGenerateJdbgProjMenuName, deGenerateJdbg);

      AMenu := TJclOTAProjectManagerMenu.Create;
      AMenu.Enabled := True;
      AMenu.Caption := LoadResString(@RsDebugInsertJdbg);
      AMenu.Parent := JclDebugExpertProjMenuName;
      AMenu.Verb := JclInsertJdbgProjMenuName;
      AMenu.Position := pmmpUserOptions + 2;
      ProjectManagerMenuList.Add(AMenu);
      FillProjMenu(JclInsertJdbgProjMenuName, deInsertJdbg);

      AMenu := TJclOTAProjectManagerMenu.Create;
      AMenu.Enabled := True;
      AMenu.Caption := LoadResString(@RsDeleteMapFile);
      AMenu.IsMultiSelectable := True;
      AMenu.Parent := JclDebugExpertProjMenuName;
      AMenu.Verb := JclDeleteMapFileProjMenuName;
      AMenu.Position := pmmpUserOptions + 3;
      ProjectManagerMenuList.Add(AMenu);
      FillProjMenu(JclDeleteMapFileProjMenuName, deDeleteMapFile);
    end;
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TProjectManagerMultipleNotifier.MenuExecute(const MenuContextList: IInterfaceList);
var
  Index: Integer;
  MenuContext: IOTAProjectMenuContext;
  Verb: string;
  Project: IOTAProject;
  Action: TDebugExpertAction;
  State: TDebugExpertState;
begin
  try
    for Index := 0 to MenuContextList.Count - 1 do
    begin
      MenuContext := MenuContextList.Items[Index] as IOTAProjectMenuContext;
      Project := MenuContext.Project;
      Verb := MenuContext.Verb;
      if Project <> nil then
      begin
        for Action := Low(Action) to High(Action) do
          for State := Low(State) to High(State) do
            if ProjectManagerSubMenuNames[Action, State] = Verb then
              FDebugExtension.ProjectStates[Action, Project] := State;
      end
      else
        raise EJclExpertException.CreateRes(@RsENoActiveProject);
    end;
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

{$ELSE ~BDS7_UP}
{$IFDEF BDS4_UP}
// BDS 2006, RAD Studio 2007 and RAD Studio 2009

//=== { TProjectManagerSimpleNotifier } ======================================

constructor TProjectManagerSimpleNotifier.Create(ADebugExtension: TJclDebugExtension;
  const ANTAServices: INTAServices; const AOTAProjectManager: IOTAProjectManager);
begin
  inherited Create;
  FDebugExtension := ADebugExtension;
  FNTAServices := ANTAServices;
  FOTAProjectManager := AOTAProjectManager;
end;

function TProjectManagerSimpleNotifier.AddMenu(const Ident: string): TMenuItem;

  procedure FillSubMenu(AMenuItem: TMenuItem; const AOnClickEvent: TNotifyEvent; AState: TDebugExpertState);
  var
    SubMenuItem: TMenuItem;
  begin
    SubMenuItem := TMenuItem.Create(AMenuItem);
    SubMenuItem.Visible := True;
    SubMenuItem.Caption := LoadResString(@RsAlwaysEnabled);
    SubMenuItem.RadioItem := True;
    SubMenuItem.Checked := AState = deAlwaysEnabled;
    SubMenuItem.Tag := DebugExpertStateToInt(deAlwaysEnabled);
    SubMenuItem.OnClick := AOnClickEvent;
    AMenuItem.Add(SubMenuItem);

    SubMenuItem := TMenuItem.Create(AMenuItem);
    SubMenuItem.Visible := True;
    SubMenuItem.Caption := LoadResString(@RsProjectEnabled);
    SubMenuItem.RadioItem := True;
    SubMenuItem.Checked := AState = deProjectEnabled;
    SubMenuItem.Tag := DebugExpertStateToInt(deProjectEnabled);
    SubMenuItem.OnClick := AOnClickEvent;
    AMenuItem.Add(SubMenuItem);

    SubMenuItem := TMenuItem.Create(AMenuItem);
    SubMenuItem.Visible := True;
    SubMenuItem.Caption := LoadResString(@RsProjectDisabled);
    SubMenuItem.RadioItem := True;
    SubMenuItem.Checked := AState = deProjectDisabled;
    SubMenuItem.Tag := DebugExpertStateToInt(deProjectDisabled);
    SubMenuItem.OnClick := AOnClickEvent;
    AMenuItem.Add(SubMenuItem);

    SubMenuItem := TMenuItem.Create(AMenuItem);
    SubMenuItem.Visible := True;
    SubMenuItem.Caption := LoadResString(@RsAlwaysDisabled);
    SubMenuItem.RadioItem := True;
    SubMenuItem.Checked := AState = deAlwaysDisabled;
    SubMenuItem.Tag := DebugExpertStateToInt(deAlwaysDisabled);
    SubMenuItem.OnClick := AOnClickEvent;
    AMenuItem.Add(SubMenuItem);
  end;

var
  SelectedIdent: string;
  AProject: IOTAProject;
  ADeleteMapFileState, AGenerateJdbgState, AInsertJdbgState: TDebugExpertState;
  ActionMenuItem: TMenuItem;
begin
  try
    SelectedIdent := Ident;
    AProject := FOTAProjectManager.GetCurrentSelection(SelectedIdent);
    if AProject <> nil then
    begin
      ADeleteMapFileState := FDebugExtension.ProjectStates[deDeleteMapFile, AProject];
      AGenerateJdbgState := FDebugExtension.ProjectStates[deGenerateJdbg, AProject];
      AInsertJdbgState := FDebugExtension.ProjectStates[deInsertJdbg, AProject];

      // root item
      Result := TMenuItem.Create(nil);
      Result.Visible := True;
      Result.Caption := LoadResString(@RsDebugExpertCaption);
      if (ADeleteMapFileState in [deAlwaysEnabled, deProjectEnabled])
        or (AGenerateJdbgState in [deAlwaysEnabled, deProjectEnabled])
        or (AInsertJdbgState in [deAlwaysEnabled, deProjectEnabled]) then
      begin
        Result.Checked := True;
        Result.ImageIndex := FDebugExtension.FDebugImageIndex
      end
      else
        Result.ImageIndex := FDebugExtension.FNoDebugImageIndex;
      Result.SubMenuImages := FNTAServices.ImageList;

      // actions items
      ActionMenuItem := TMenuItem.Create(Result);
      ActionMenuItem.Visible := True;
      ActionMenuItem.Caption := LoadResString(@RsDebugGenerateJdbg);
      if AGenerateJdbgState in [deAlwaysEnabled, deProjectEnabled] then
      begin
        ActionMenuItem.Checked := True;
        ActionMenuItem.ImageIndex := FDebugExtension.FGenerateJdbgImageIndex;
      end
      else
        ActionMenuItem.ImageIndex := FDebugExtension.FNoGenerateJdbgImageIndex;
      FillSubMenu(ActionMenuItem, GenerateJdbgSubMenuClick, AGenerateJdbgState);
      Result.Add(ActionMenuItem);

      ActionMenuItem := TMenuItem.Create(Result);
      ActionMenuItem.Visible := True;
      ActionMenuItem.Caption := LoadResString(@RsDebugInsertJdbg);
      if AInsertJdbgState in [deAlwaysEnabled, deProjectEnabled] then
      begin
        ActionMenuItem.Checked := True;
        ActionMenuItem.ImageIndex := FDebugExtension.FInsertJdbgImageIndex;
      end
      else
        ActionMenuItem.ImageIndex := FDebugExtension.FNoInsertJdbgImageIndex;
      FillSubMenu(ActionMenuItem, InsertJdbgSubMenuClick, AInsertJdbgState);
      Result.Add(ActionMenuItem);

      ActionMenuItem := TMenuItem.Create(Result);
      ActionMenuItem.Visible := True;
      ActionMenuItem.Caption := LoadResString(@RsDeleteMapFile);
      if ADeleteMapFileState in [deAlwaysEnabled, deProjectEnabled] then
      begin
        ActionMenuItem.Checked := True;
        ActionMenuItem.ImageIndex := FDebugExtension.FDeleteMapFileImageIndex;
      end
      else
        ActionMenuItem.ImageIndex := FDebugExtension.FNoDeleteMapFileImageIndex;
      FillSubMenu(ActionMenuItem, DeleteMapFileSubMenuClick, ADeleteMapFileState);
      Result.Add(ActionMenuItem);
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

function TProjectManagerSimpleNotifier.CanHandle(const Ident: string): Boolean;
begin
  Result := Ident = sProjectContainer;
end;

procedure TProjectManagerSimpleNotifier.DeleteMapFileSubMenuClick(Sender: TObject);
var
  AProject: IOTAProject;
  Ident: string;
begin
  try
    Ident := '';
    AProject := FOTAProjectManager.GetCurrentSelection(Ident);
    if AProject <> nil then
      FDebugExtension.ProjectStates[deDeleteMapFile, AProject] := IntToDebugExpertState((Sender as TMenuItem).Tag)
    else
      raise EJclExpertException.CreateRes(@RsENoActiveProject);
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TProjectManagerSimpleNotifier.GenerateJdbgSubMenuClick(Sender: TObject);
var
  AProject: IOTAProject;
  Ident: string;
begin
  try
    Ident := '';
    AProject := FOTAProjectManager.GetCurrentSelection(Ident);
    if AProject <> nil then
      FDebugExtension.ProjectStates[deGenerateJdbg, AProject] := IntToDebugExpertState((Sender as TMenuItem).Tag)
    else
      raise EJclExpertException.CreateRes(@RsENoActiveProject);
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
end;

procedure TProjectManagerSimpleNotifier.InsertJdbgSubMenuClick(Sender: TObject);
var
  AProject: IOTAProject;
  Ident: string;
begin
  try
    Ident := '';
    AProject := FOTAProjectManager.GetCurrentSelection(Ident);
    if AProject <> nil then
      FDebugExtension.ProjectStates[deInsertJdbg, AProject] := IntToDebugExpertState((Sender as TMenuItem).Tag)
    else
      raise EJclExpertException.CreateRes(@RsENoActiveProject);
  except
    on ExceptionObj: TObject do
      JclExpertShowExceptionDialog(ExceptionObj);
  end;
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
