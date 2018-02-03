{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JediInstallConfigIni.pas.                                                   }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet. Portions created by Florent Ouchet }
{ are Copyright (C) of Florent Ouchet. All Rights Reserved.                                        }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Storage facility into an ini file for the installer core                                         }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JediInstallConfigIni;

{$I jcl.inc}
{$I crossplatform.inc}

interface

uses
  JediInstall, IniFiles;

type
  TJediConfigIni = class(TInterfacedObject, IJediConfiguration)
  private
    FIniFile: TMemIniFile;
  public
    constructor Create;
    destructor Destroy; override;
    
    // IJediConfiguration
    function GetSections: TStringArray;
    function GetOptions(const Section: string): TOptionArray;
    function GetOptionAsBool(const Section: string; Id: Integer): Boolean;
    procedure SetOptionAsBool(const Section: string; Id: Integer; Value: Boolean);
    function GetOptionAsBoolByName(const Section: string; const Name: string): Boolean;
    procedure SetOptionAsBoolByName(const Section: string; const Name: string; Value: Boolean);
    function GetOptionAsString(const Section: string; Id: Integer): string;
    procedure SetOptionAsString(const Section: string; Id: Integer; const Value: string);
    function GetOptionAsStringByName(const Section: string; const Name: string): string;
    procedure SetOptionAsStringByName(const Section: string; const Name: string; const Value: string);

    procedure Clear;
    procedure DeleteSection(const Section: string);
    procedure DeleteOption(const Section: string; Id: Integer);
    function SectionExists(const Section: string): Boolean;
    function ValueExists(const Section: string; Id: Integer): Boolean; overload;
    function ValueExists(const Section: string; const Name: string): Boolean; overload;

    property Sections: TStringArray read GetSections;
    property Options[const Section: string]: TOptionArray read GetOptions;
    property OptionAsBool[const Section: string; Id: Integer]: Boolean read GetOptionAsBool
      write SetOptionAsBool;
    property OptionAsBoolByName[const Section: string; const Name: string]: Boolean
      read GetOptionAsBoolByName write SetOptionAsBoolByName;
    property OptionAsString[const Section: string; Id: Integer]: string read GetOptionAsString
      write SetOptionAsString;
    property OptionAsStringByName[const Section: string; const Name: string]: string
      read GetOptionAsStringByName write SetOptionAsStringByName;
  end;

function CreateConfigIni: IJediConfiguration;

implementation

uses
  SysUtils, Classes,
  JclSysInfo, JclFileUtils;

const
  DefaultIniFileName = 'JCL-install.ini';

function CreateConfigIni: IJediConfiguration;
begin
  Result := TJediConfigIni.Create;
end;

//=== { TJediConfigIni } =====================================================

procedure TJediConfigIni.Clear;
begin
  FIniFile.Clear;
end;

constructor TJediConfigIni.Create;
var
  AFileName: string;
begin
  inherited Create;
  
  AFileName := '';

  if not GetEnvironmentVar('JCL_INSTALL_INI', AFileName) then
    AFileName := '';

  if AFileName = '' then
    AFileName := DefaultIniFileName;

  if not PathIsAbsolute(AFileName) then
    AFileName := ExtractFilePath(ParamStr(0)) + AFileName;

  FIniFile := TMemIniFile.Create(AFileName);
end;

procedure TJediConfigIni.DeleteOption(const Section: string; Id: Integer);
begin
  FIniFile.DeleteKey(Section, InstallCore.InstallOptionName[Id]);
end;

procedure TJediConfigIni.DeleteSection(const Section: string);
begin
  FIniFile.EraseSection(Section);
end;

destructor TJediConfigIni.Destroy;
begin
  FIniFile.UpdateFile;
  FIniFile.Free;
  inherited Destroy;
end;

function TJediConfigIni.GetOptionAsBool(const Section: string;
  Id: Integer): Boolean;
begin
  Result := FIniFile.ReadBool(Section, InstallCore.InstallOptionName[Id], False);
end;

function TJediConfigIni.GetOptionAsBoolByName(const Section,
  Name: string): Boolean;
begin
  Result := FIniFile.ReadBool(Section, Name, False);
end;

function TJediConfigIni.GetOptionAsString(const Section: string;
  Id: Integer): string;
begin
  Result := FIniFile.ReadString(Section, InstallCore.InstallOptionName[Id], '');
end;

function TJediConfigIni.GetOptionAsStringByName(const Section,
  Name: string): string;
begin
  Result := FIniFile.ReadString(Section, Name, '');
end;

function TJediConfigIni.GetOptions(const Section: string): TOptionArray;
var
  Values: TStrings;
  Index: Integer;
  Name: string;
begin
  Values := TStringList.Create;
  try
    FIniFile.ReadSectionValues(Section, Values);
    SetLength(Result, Values.Count);
    for Index := 0 to Values.Count - 1 do
    begin
      Name := Values.Names[Index];
      Result[Index].Name := Name;
      Result[Index].Value := Values.Values[Name];
    end;
  finally
    Values.Free;
  end;
end;

function TJediConfigIni.GetSections: TStringArray;
var
  Sections: TStrings;
  Index: Integer;
begin
  Sections := TStringList.Create;
  try
    FIniFile.ReadSections(Sections);
    SetLength(Result, Sections.Count);
    for Index := 0 to Sections.Count - 1 do
      Result[Index] := Sections.Strings[Index];
  finally
    Sections.Free;
  end;
end;

function TJediConfigIni.SectionExists(const Section: string): Boolean;
begin
  Result := FIniFile.SectionExists(Section);
end;

procedure TJediConfigIni.SetOptionAsBool(const Section: string; Id: Integer;
  Value: Boolean);
begin
  FIniFile.WriteBool(Section, InstallCore.InstallOptionName[Id], Value);
end;

procedure TJediConfigIni.SetOptionAsBoolByName(const Section, Name: string;
  Value: Boolean);
begin
  FIniFile.WriteBool(Section, Name, Value);
end;

procedure TJediConfigIni.SetOptionAsString(const Section: string; Id: Integer;
  const Value: string);
begin
  FIniFile.WriteString(Section, InstallCore.InstallOptionName[Id], Value);
end;

procedure TJediConfigIni.SetOptionAsStringByName(const Section, Name,
  Value: string);
begin
  FIniFile.WriteString(Section, Name, Value);
end;

function TJediConfigIni.ValueExists(const Section: string;
  Id: Integer): Boolean;
begin
  Result := FIniFile.ValueExists(Section, InstallCore.InstallOptionName[Id]);
end;

function TJediConfigIni.ValueExists(const Section, Name: string): Boolean;
begin
  Result := FIniFile.ValueExists(Section, Name);
end;

initialization

InstallCore.ConfigurationCreator := CreateConfigIni;

end.
