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
{ The Original Code is JclOtaRepositoryReg.pas.                                                    }
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

unit JclOtaExcDlgRepository;

{$I jcl.inc}

interface

{$IFDEF DELPHI6_UP}
{$DEFINE DELPHIEXCDLG}
{$ENDIF DELPHI6_UP}

{$IFDEF BCB6_UP}
{$DEFINE CBUILDEREXCDLG}
{$ENDIF BCB6_UP}

{$IFDEF COMPILER10_UP}
{$DEFINE DELPHIEXCDLG}
{$DEFINE CBUILDEREXCDLG}
{$ENDIF COMPILER10_UP}

uses
  SysUtils, Classes,
  ToolsAPI,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclIDEUtils,
  JclOtaUtils, JclOtaRepositoryUtils, JclPreProcessorExcDlgTemplates;

type
  TJclExcDlgExpert = class(TJclOtaRepositoryExpert)
  public
    procedure CreateExceptionDialog(const Params: TJclExcDlgParams);
  end;

  TJclExcDlgDelphiExpert = class(TJclExcDlgExpert)
  public
    constructor Create; override;

    procedure DoExecute(const Personality: TJclBorPersonality); override;
    function IsVisible(const Personality: TJclBorPersonality): Boolean; override;
  end;

  TJclExcDlgCBuilderExpert = class(TJclExcDlgExpert)
  public
    constructor Create; override;

    procedure DoExecute(const Personality: TJclBorPersonality); override;
    function IsVisible(const Personality: TJclBorPersonality): Boolean; override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\repository\ExceptionDialog';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Windows,
  JclStrings, JclFileUtils, JclRegistry,
  JclOtaResources, JclOtaConsts,
  JclPreProcessorTemplates, JclOtaRepositoryReg, JclOtaExcDlgWizard;

{$R JclOtaExcDlgIcons.res}

//=== { TJclExcDlgExpert } ===================================================

procedure TJclExcDlgExpert.CreateExceptionDialog(
  const Params: TJclExcDlgParams);
  function LoadTemplate(const FileName: string): string;
  var
    AFileStream: TFileStream;
    StreamLength: Int64;
    AnsiResult: AnsiString;
  begin
    AnsiResult := '';
    if FileExists(FileName) then
    begin
      AFileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      try
        StreamLength := AFileStream.Size;
        SetLength(AnsiResult, StreamLength);
        AFileStream.ReadBuffer(AnsiResult[1], StreamLength);
      finally
        AFileStream.Free;
      end;
    end;
    Result := string(AnsiResult);
  end;

  function PathGetAbsolutePath(const P: string): string;
  var
    ActiveEditBuffer: IOTAEditBuffer;
    ActiveProject: IOTAProject;
    CurrentDirectory: string;
  begin
    if not PathIsAbsolute(P) then
    begin
      CurrentDirectory := '';
      ActiveEditBuffer := GetActiveEditBuffer;
      if Assigned(ActiveEditBuffer) then
        CurrentDirectory := ExtractFileDir(ActiveEditBuffer.FileName);
      if CurrentDirectory = '' then
      begin
        ActiveProject := GetActiveProject;
        if Assigned(ActiveProject) then
          CurrentDirectory := ExtractFileDir(ActiveProject.FileName);
      end;
      if CurrentDirectory <> '' then
        Result := PathGetRelativePath(PathAddSeparator(CurrentDirectory), P)
      else
        Result := P;
    end
    else
      Result := P;
  end;
const
  TemplateSubDir = 'experts\repository\ExceptionDialog\Templates\';
  DelphiTemplate = 'ExceptDlg.Delphi32';
  BCBTemplate = 'ExceptDlg.CBuilder32';
var
  TemplatePath, FormExtension, FormTemplate, FormContent, FormFileName,
  HeaderExtension, HeaderTemplate, HeaderContent, HeaderFileName,
  SourceExtension, SourceTemplate, SourceContent, SourceFileName: string;
begin
  TemplatePath := PathAddSeparator(JCLRootDir) + TemplateSubDir;

  case Params.Language of
    bpDelphi32:
      begin
        FormExtension := JclBorDesignerFormExtension[Params.Designer];
        FormTemplate := DelphiTemplate + FormExtension;
        HeaderExtension := '';
        HeaderTemplate := '';
        SourceExtension := SourceExtensionPAS;
        SourceTemplate := DelphiTemplate + SourceExtension;
      end;
    bpBCBuilder32:
      begin
        FormExtension := JclBorDesignerFormExtension[Params.Designer];
        FormTemplate := BCBTemplate + FormExtension;
        HeaderExtension := SourceExtensionH;
        HeaderTemplate := BCBTemplate + HeaderExtension;
        SourceExtension := SourceExtensionCPP;
        SourceTemplate := BCBTemplate + SourceExtension;
      end;
  else
      begin
        FormExtension := '';
        FormTemplate := '';
        HeaderExtension := '';
        HeaderTemplate := '';
        SourceExtension := '';
        SourceTemplate := '';
      end;
  end;

  FormTemplate := LoadTemplate(TemplatePath + FormTemplate);
  HeaderTemplate := LoadTemplate(TemplatePath + HeaderTemplate);
  SourceTemplate := LoadTemplate(TemplatePath + SourceTemplate);

  FormContent := ApplyTemplate(FormTemplate, Params);
  HeaderContent := ApplyTemplate(HeaderTemplate, Params);
  SourceContent := ApplyTemplate(SourceTemplate, Params);

  if Params.FileName <> '' then
  begin
    FormFileName := PathGetAbsolutePath(ChangeFileExt(Params.FileName, FormExtension));
    HeaderFileName := PathGetAbsolutePath(ChangeFileExt(Params.FileName, HeaderExtension));
    SourceFileName := PathGetAbsolutePath(ChangeFileExt(Params.FileName, SourceExtension));
  end
  else
  begin
    FormFileName := '';
    HeaderFileName := '';
    SourceFileName := '';
  end;

  CreateForm(Params.FormAncestor, Params.FormName, FormFileName, FormContent, SourceFileName,
    SourceContent, HeaderFileName, HeaderContent);
end;

//=== { TJclRepositoryExpert } ===============================================

constructor TJclExcDlgDelphiExpert.Create;
begin
  inherited Create(LoadResString(@RsRepositoryExcDlgDelphiName), LoadResString(@RsRepositoryExcDlgDelphiDescription),
    LoadResString(@RsAboutDialogTitle), LoadResString(@RsRepositoryExcDlgPage), JclRepositoryCategoryDelphiFiles,
    JclDesignerVcl, JclDelphiPersonality, LoadIcon(FindResourceHInstance(HInstance), 'JclExcDlg'), ritForm);
end;

procedure TJclExcDlgDelphiExpert.DoExecute(const Personality: TJclBorPersonality);
var
  AParams: TJclExcDlgParams;
begin
  AParams := TJclExcDlgParams.Create;
  try
    AParams.Languages := [bpDelphi32];
    AParams.Language := bpDelphi32;
    AParams.ActivePersonality := bpDelphi32;
    if ExcDlgWizard(AParams) and (AParams.Language <> bpUnknown) then
      CreateExceptionDialog(AParams);
  finally
    AParams.Free;
  end;
end;

function TJclExcDlgDelphiExpert.IsVisible(
  const Personality: TJclBorPersonality): Boolean;
begin
  Result := Personality = bpDelphi32;
end;

//=== { TJclExcDlgCBuilderExpert } ===========================================

constructor TJclExcDlgCBuilderExpert.Create;
begin
  inherited Create(LoadResString(@RsRepositoryExcDlgCBuilderName), LoadResString(@RsRepositoryExcDlgCBuilderDescription),
    LoadResString(@RsAboutDialogTitle), LoadResString(@RsRepositoryExcDlgPage), JclRepositoryCategoryCBuilderFiles,
    JclDesignerVcl, JclCBuilderPersonality, LoadIcon(FindResourceHInstance(HInstance), 'JclExcDlgCPP'), ritForm);
end;

procedure TJclExcDlgCBuilderExpert.DoExecute(
  const Personality: TJclBorPersonality);
var
  AParams: TJclExcDlgParams;
begin
  AParams := TJclExcDlgParams.Create;
  try
    AParams.Languages := [bpDelphi32];
    AParams.Language := bpDelphi32;
    AParams.ActivePersonality := bpBCBuilder32;
    if ExcDlgWizard(AParams) and (AParams.Language <> bpUnknown) then
      CreateExceptionDialog(AParams);
  finally
    AParams.Free;
  end;
end;

function TJclExcDlgCBuilderExpert.IsVisible(
  const Personality: TJclBorPersonality): Boolean;
begin
  Result := Personality = bpBCBuilder32;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

  {$IFDEF DELPHIEXCDLG}
  RegisterJclOTARepositoryExpert(TJclExcDlgDelphiExpert, JclDelphiPersonality);
  {$ENDIF DELPHIEXCDLG}
  {$IFDEF CBUILDEREXCDLG}
  RegisterJclOTARepositoryExpert(TJclExcDlgCBuilderExpert, JclCBuilderPersonality);
  {$ENDIF CBUILDEREXCDLG}

finalization
  {$IFDEF DELPHIEXCDLG}
  //UnregisterJclOTARepositoryExpert(TJclExcDlgDelphiExpert);
  {$ENDIF DELPHIEXCDLG}
  {$IFDEF CBUILDEREXCDLG}
  //UnregisterJclOTARepositoryExpert(TJclExcDlgCBuilderExpert);
  {$ENDIF CBUILDEREXCDLG}
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
