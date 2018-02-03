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
{ The Original Code is JclOtaRepositoryUtils.pas.                                                  }
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

unit JclOtaRepositoryUtils;

interface

{$I jcl.inc}

uses
  Windows,
  SysUtils,
  ToolsAPI,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclIDEUtils,
  JclOtaUtils;

type
  TJclRepositoryItemType = (ritForm, ritProject);

  // abstraction layer for all versions of Delphi from 5 to 2006
  TJclOTARepositoryExpert = class(TJclOTAExpert, IInterface, IOTAWizard, IOTARepositoryWizard60,
    {$IFDEF COMPILER8_UP} IOTARepositoryWizard80, {$ENDIF COMPILER8_UP}
    IOTARepositoryWizard,
    {$IFDEF COMPILER10_UP} IOTAProjectWizard100, {$ENDIF COMPILER10_UP}
    IOTAProjectWizard,
    {$IFDEF COMPILER10_UP} IOTAFormWizard100, {$ENDIF COMPILER10_UP}
    IOTAFormWizard)
  private
    FName: string;
    FDescription: string;
    FAuthor: string;
    FPage: string;
    FGalleryCategory: string;
    FGlyph: Cardinal;
    FItemType: TJclRepositoryItemType;
    FDesigner: string;
    FPersonality: string;
  public
    constructor Create(const AName, ADescription, AAuthor, APage, AGalleryCategory,
      ADesigner, APersonality: string; AGlyph: Cardinal;
      AItemType: TJclRepositoryItemType); reintroduce; overload; virtual;
    constructor Create; reintroduce; overload; virtual; abstract;
    destructor Destroy; override;

    // override to customize
    procedure DoExecute(const Personality: TJclBorPersonality); virtual;
    function IsVisible(const Personality: TJclBorPersonality): Boolean; virtual;
  public
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    { IOTAWizard }
    procedure Execute; override;
    function GetName: string; override;
    function GetState: TWizardState; override;
    { IOTARepositoryWizard }
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;

    { IOTARepositoryWizard60 }
    function GetDesigner: string;

    {$IFDEF COMPILER8_UP}
    { IOTARepositoryWizard80 }
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
    {$ENDIF COMPILER8_UP}

    { IOTAProjectWizard }

    {$IFDEF COMPILER10_UP}
    { IOTAProjectWizard100 }
    function IsProjectWizardVisible(Project: IOTAProject): Boolean;
    function IOTAProjectWizard100.IsVisible = IsProjectWizardVisible;
    {$ENDIF COMPILER10_UP}

    { IOTAFormWizard }

    {$IFDEF COMPILER10_UP}
    { IOTAFormWizard100 }
    function IsFormWizardVisible(Project: IOTAProject): Boolean;
    function IOTAFormWizard100.IsVisible = IsFormWizardVisible;
    {$ENDIF COMPILER10_UP}

    property Name: string read FName;
  public
    function CreateForm(const FormAncestor, FormName: string;
      const FormFileName: TFileName; const FormContent: string;
      const SourceFileName: TFileName; const SourceContent: string;
      const HeaderFileName: TFileName; const HeaderContent: string): IOTAModule;
  end;

  TJclOTARepositoryExpertClass = class of TJclOTARepositoryExpert;

  TJclOtaFormCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FFormFileName: TFileName;
    FFormContent: string;
    FSourceFileName: TFileName;
    FSourceContent: string;
    FHeaderFileName: TFileName;
    FHeaderContent: string;
    FFormAncestor: string;
    FFormName: string;
    FProjectModule: IOTAProject;
    procedure SaveFile(const FileName: TFileName; const FileContent: string);
  public
    constructor Create(const ProjectModule: IOTAProject;
      const FormAncestor, FormName: string;
      const FormFileName: TFileName; const FormContent: string;
      const SourceFileName: TFileName; const SourceContent: string;
      const HeaderFileName: TFileName; const HeaderContent: string); reintroduce;
    destructor Destroy; override;
    { IOTACreator }
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    { IOTAModuleCreator }
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  TJclOtaFile = class(TInterfacedObject, IOTAFile)
  private
    FFileName: string;
    FContent: string;
  public
    constructor Create(const AFileName, AContent: string); reintroduce;
    { IOTAFile }
    function GetSource: string;
    function GetAge: TDateTime;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\repository';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Classes, ActiveX,
  JclDateTime, JclFileUtils, JclOtaResources, JclPreProcessorTemplates;

//=== { TJclOTARepositoryExpertBase } ========================================

constructor TJclOTARepositoryExpert.Create(const AName, ADescription, AAuthor, APage,
  AGalleryCategory, ADesigner, APersonality: string; AGlyph: Cardinal;
  AItemType: TJclRepositoryItemType);
begin
  inherited Create(AName);
  FName := AName;
  FDescription := ADescription;
  FAuthor := AAuthor;
  FPage := APage;
  FGalleryCategory := AGalleryCategory;
  FGlyph := AGlyph;
  FItemType := AItemType;
  FDesigner := ADesigner;
  FPersonality := APersonality;
end;

function TJclOTARepositoryExpert.CreateForm(const FormAncestor, FormName: string;
  const FormFileName: TFileName; const FormContent: string;
  const SourceFileName: TFileName; const SourceContent: string;
  const HeaderFileName: TFileName; const HeaderContent: string): IOTAModule;
var
  AModuleCreator: IOTAModuleCreator;
begin
  AModuleCreator := TJclOtaFormCreator.Create(GetActiveProject, FormAncestor,
    FormName, FormFileName, FormContent, SourceFileName, SourceContent,
    HeaderFileName, HeaderContent);
  try
    Result := GetOTAModuleServices.CreateModule(AModuleCreator);
  finally
    AModuleCreator := nil;
  end;
end;

destructor TJclOTARepositoryExpert.Destroy;
begin
  inherited Destroy;
end;

procedure TJclOTARepositoryExpert.DoExecute(
  const Personality: TJclBorPersonality);
begin
  // inherit to customize
end;

procedure TJclOTARepositoryExpert.Execute;
var
  Personality: TJclBorPersonality;
begin
  try
    Personality := ActivePersonality;
    if Personality <> bpUnknown then
      DoExecute(Personality);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

//IOTARepositoryWizard.GetAuthor
function TJclOTARepositoryExpert.GetAuthor: string;
begin
  Result := FAuthor;
end;

//IOTARepositoryWizard.GetComment
function TJclOTARepositoryExpert.GetComment: string;
begin
  Result := FDescription;
end;

//IOTARepositoryWizard60.GetDesigner
function TJclOTARepositoryExpert.GetDesigner: string;
begin
  Result := FDesigner;
end;

{$IFDEF COMPILER8_UP}
// IOTARepositoryWizard80.GetGalleryCategory
function TJclOTARepositoryExpert.GetGalleryCategory: IOTAGalleryCategory;
begin
  try
    Result := GetOTAGalleryCategoryManager.FindCategory(FGalleryCategory);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;
{$ENDIF COMPILER8_UP}

//IOTARepositoryWizard.GetGlyph
function TJclOTARepositoryExpert.GetGlyph: Cardinal;
begin
  Result := FGlyph;
end;

function TJclOTARepositoryExpert.GetName: string;
begin
  Result := FName;
end;

//IOTARepositoryWizard.GetPage
function TJclOTARepositoryExpert.GetPage: string;
begin
  Result := FPage;
end;

function TJclOTARepositoryExpert.GetState: TWizardState;
begin
  try
    if IsVisible(ActivePersonality) then
      Result := [wsEnabled]
    else
      Result := [];
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

{$IFDEF COMPILER8_UP}
//IOTARepositoryWizard80.GetPage
function TJclOTARepositoryExpert.GetPersonality: string;
begin
  Result := FPersonality;
end;
{$ENDIF COMPILER8_UP}

{$IFDEF COMPILER10_UP}
//IOTAFormWizard100.IsVisible
function TJclOTARepositoryExpert.IsFormWizardVisible(
  Project: IOTAProject): Boolean;
begin
  try
    Result := (FItemType = ritForm) and Assigned(Project)
      and IsVisible(PersonalityTextToId(Project.Personality));
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;
{$ENDIF COMPILER10_UP}

{$IFDEF COMPILER10_UP}
//IOTAProjectWizard100.IsVisible
function TJclOTARepositoryExpert.IsProjectWizardVisible(
  Project: IOTAProject): Boolean;
begin
  try
    Result := (FItemType = ritProject) and Assigned(Project)
      and IsVisible(PersonalityTextToId(Project.Personality));
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;
{$ENDIF COMPILER10_UP}

function TJclOTARepositoryExpert.IsVisible(
  const Personality: TJclBorPersonality): Boolean;
begin
  // override to customize
  Result := Personality <> bpUnknown;
end;

function TJclOTARepositoryExpert.QueryInterface(const IID: TGUID;
  out Obj): HResult; stdcall;
begin
  if   (IsEqualGUID(IID, IOTAFormWizard) and (FItemType <> ritForm))
    {$IFDEF COMPILER10_UP}
    or (IsEqualGUID(IID, IOTAFormWizard100) and (FItemType <> ritForm))
    or (IsEqualGUID(IID, IOTAProjectWizard100) and (FItemType <> ritProject))
    {$ENDIF COMPILER10_UP}
    or (IsEqualGUID(IID, IOTAProjectWizard) and (FItemType <> ritProject)) then
  begin
    Result := E_NOINTERFACE;
    Pointer(Obj) := nil;
  end
  else
    Result := inherited QueryInterface(IID, Obj);
end;

//=== { TJclOtaModuleCreator } ===============================================

constructor TJclOtaFormCreator.Create(const ProjectModule: IOTAProject;
  const FormAncestor, FormName: string;
  const FormFileName: TFileName; const FormContent: string;
  const SourceFileName: TFileName; const SourceContent: string;
  const HeaderFileName: TFileName; const HeaderContent: string);
begin
  inherited Create;
  FProjectModule := ProjectModule;
  FFormAncestor := FormAncestor;
  FFormName := FormName;
  FFormFileName := FormFileName;
  FFormContent := FormContent;
  FSourceFileName := SourceFileName;
  FSourceContent := SourceContent;
  FHeaderFileName := HeaderFileName;
  FHeaderContent := HeaderContent;
end;

destructor TJclOtaFormCreator.Destroy;
begin
  FProjectModule := nil;
  inherited Destroy;
end;

procedure TJclOtaFormCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  // nothing
end;

function TJclOtaFormCreator.GetAncestorName: string;
begin
  Result := FFormAncestor;
end;

function TJclOtaFormCreator.GetCreatorType: string;
begin
  // form module
  Result := sForm;
end;

function TJclOtaFormCreator.GetExisting: Boolean;
begin
  // new module
  Result := (FSourceFileName <> '') and (FFormFileName <> '') and (FHeaderFileName <> '');
end;

function TJclOtaFormCreator.GetFileSystem: string;
begin
  // no file system
  Result := '';
end;

function TJclOtaFormCreator.GetFormName: string;
begin
  Result := FFormName;
end;

function TJclOtaFormCreator.GetImplFileName: string;
begin
  if (FFormContent <> '') and (FFormFileName <> '') then
    SaveFile(FFormFileName, GetFinalFormContent(FFormContent, FFormName, FFormAncestor));

  if (FSourceContent <> '') and (FSourceFileName <> '') then
    SaveFile(FSourceFileName, GetFinalSourceContent(FSourceContent, PathExtractFileNameNoExt(FSourceFileName), FFormName, FFormAncestor));

  Result := FSourceFileName;
end;

function TJclOtaFormCreator.GetIntfFileName: string;
begin
  if (FHeaderContent <> '') and (FHeaderFileName <> '') then
    SaveFile(FHeaderFileName, GetFinalHeaderContent(FHeaderContent, PathExtractFileNameNoExt(FSourceFileName), FFormName, FFormAncestor));
    
  Result := FHeaderFileName;
end;

function TJclOtaFormCreator.GetMainForm: Boolean;
begin
  // it is not the main form
  Result := False;
end;

function TJclOtaFormCreator.GetOwner: IOTAModule;
begin
  // the owner is the project
  Result := FProjectModule;
end;

function TJclOtaFormCreator.GetShowForm: Boolean;
begin
  // shows the form once created
  Result := False;
end;

function TJclOtaFormCreator.GetShowSource: Boolean;
begin
  // shows the source once created
  Result := True;
end;

function TJclOtaFormCreator.GetUnnamed: Boolean;
begin
  // the save-as dialog will be displayed
  Result :=   ((FFormFileName = '') and (FFormContent <> ''))
           or ((FSourceFileName = '') and (FSourceContent <> ''))
           or ((FHeaderFileName = '') and (FHeaderContent <> ''));
end;

function TJclOtaFormCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  if FFormContent <> '' then
    Result := TJclOtaFile.Create(FFormFileName, GetFinalFormContent(FFormContent, FormIdent, AncestorIdent))
  else
    Result := nil;
end;

function TJclOtaFormCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  if FSourceContent <> '' then
    Result := TJclOtaFile.Create(FSourceFileName, GetFinalSourceContent(FSourceContent, ModuleIdent, FormIdent, AncestorIdent))
  else
    Result := nil;
end;

function TJclOtaFormCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  if FHeaderContent <> '' then
    Result := TJclOtaFile.Create(FHeaderFileName, GetFinalHeaderContent(FHeaderContent, ModuleIdent, FormIdent, AncestorIdent))
  else
    Result := nil;
end;

procedure TJclOtaFormCreator.SaveFile(const FileName: TFileName; const FileContent: string);
var
  AFileStream: TFileStream;
  Buffer: AnsiString;
begin
  AFileStream := TFileStream.Create(FileName, fmCreate);
  try
    Buffer := AnsiString(FileContent);
    AFileStream.WriteBuffer(Buffer[1], Length(Buffer));
  finally
    AFileStream.Free;
  end;
end;

//=== { TJclOtaFile } ========================================================

constructor TJclOtaFile.Create(const AFileName, AContent: string);
begin
  inherited Create;
  FContent := AContent;
  FFileName := AFileName;
end;

function TJclOtaFile.GetAge: TDateTime;
var
  AFileTime: TFileTime;
  AFileStream: TFileStream;
begin
  // new file
  if FFileName <> '' then
  begin
    try
      AFileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
      try
        if GetFileTime(AFileStream.Handle, nil, nil, @AFileTime) then
          Result := FileTimeToDateTime(AFileTime)
        else
          Result := -1;
      finally
        AFileStream.Free;
      end;
    except
      Result := -1;
    end;
  end
  else
    Result := -1;
end;

function TJclOtaFile.GetSource: string;
begin
  // return the file content
  Result := FContent;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
