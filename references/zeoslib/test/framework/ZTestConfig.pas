{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Configuration for Testing Framework           }
{                                                         }
{          Originally written by Sergey Seroukhov         }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZTestConfig;

interface

{$I ZTestFramework.inc}

{$IFDEF VER130}
  {$DEFINE USE_MEMCHECK}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE USE_MEMCHECK}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE USE_MEMCHECK}
{$ENDIF}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
{$IFDEF USE_MEMCHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, IniFiles, ZCompatibility;

const
  { Test configuration file }
  CONFIG_CMD_OPTION   = '-c';
  DEFAULT_CONFIG_DIR  = 'database';
  DEFAULT_LOG_DIR     = 'log';
  DEFAULT_CONFIG_FILE = 'test.properties';
{$IFDEF LINUX}
  PATH_DELIMITER      = '/';
{$ELSE}
  PATH_DELIMITER      = '\';
{$ENDIF}

const
  { Names of the main test groups }
  COMMON_GROUP           = 'common';
  CORE_TEST_GROUP        = 'core';
  PARSESQL_TEST_GROUP    = 'parsesql';
  PLAIN_TEST_GROUP       = 'plain';
  DBC_TEST_GROUP         = 'dbc';
  COMPONENT_TEST_GROUP   = 'component';
  BUGREPORT_TEST_GROUP   = 'bugreport';
  PERFORMANCE_TEST_GROUP = 'performance';

const
  { Names of the main configuration keys. }
  ENABLE_MEMCHECK_KEY      = 'enable.memcheck';
  MEMCHECK_LOGFILE_KEY     = 'memcheck.logfile';
  MEMCHECK_SHOWRESULT_KEY  = 'memcheck.showresult';
  DECIMAL_SEPARATOR_KEY    = 'decimal.separator';
  SUPPRESS_TEST_OUTPUT_KEY = 'suppress.output';
  ENABLE_KEY               = 'enable';
  SKIP_CLOSED_KEY          = 'skip.closed';
  ACTIVE_CONNECTIONS_KEY   = 'connections';

const
  { Names of the connection configuration keys. }
  DATABASE_ALIAS_KEY          = 'alias';
  DATABASE_PROTOCOL_KEY       = 'protocol';
  DATABASE_HOST_KEY           = 'host';
  DATABASE_PORT_KEY           = 'port';
  DATABASE_NAME_KEY           = 'database';
  DATABASE_USER_KEY           = 'user';
  DATABASE_PASSWORD_KEY       = 'password';
  DATABASE_REBUILD_KEY        = 'rebuild';
  DATABASE_DELIMITER_TYPE_KEY = 'delimiter.type';
  DATABASE_DELIMITER_KEY      = 'delimiter';
  DATABASE_CREATE_SCRIPTS_KEY = 'create.scripts';
  DATABASE_DROP_SCRIPTS_KEY   = 'drop.scripts';
  DATABASE_PROPERTIES_KEY     = 'properties';

const
  { SQL script delimiters }
  DEFAULT_DELIMITER    = 'default';
  SET_TERM_DELIMITER   = 'setterm';
  GO_DELIMITER         = 'go';
  EMPTY_LINE_DELIMITER = 'emptyline';

const
  { Well Known Values. }
  NONE_VALUE      = 'none';
  FALSE_VALUE     = 'false';
  TRUE_VALUE      = 'true';
  LIST_DELIMITERS = ' ,;';

const
  { Default Values. }
  DEFAULT_DECIMAL_SEPARATOR = ',';
  DEFAULT_PORT_VALUE        = 0;
  DEFAULT_HOST_VALUE        = 'localhost';

type

  {** Implements a wrapper for test configuration file. }
  TZTestConfiguration = class
  private
    FConfigFile: TIniFile;
    FConfigLoaded: Boolean;
    FConfigFileName: string;
    FEnableMemCheck: Boolean;
    FMemCheckLogFile: string;
    FMemCheckShowResult: Boolean;

    function GetConfigFileName: string;
    function GetConfigFilePath: string;
  public
    destructor Destroy; override;

    procedure LoadConfig;
    function ReadProperty(const Group, Key, Default: string): string;

    procedure ActivateMemCheck;
    procedure DeactivateMemCheck;

    property ConfigFile: TIniFile read FConfigFile;
    property ConfigFileName: string read FConfigFileName;
    property ConfigFilePath: string read GetConfigFilePath;
    property ConfigLoaded: Boolean read FConfigLoaded;

    property EnableMemCheck: Boolean read FEnableMemCheck;
    property MemCheckLogFile: string read FMemCheckLogFile;
    property MemCheckShowResult: Boolean read FMemCheckShowResult;
  end;


{**
  Splits string using the delimiter string.
  @param Str the source string
  @param Delimiters the delimiter string
  @return the result list where plased delimited string
}
function SplitStringToArray(Value: string; Delimiters: string): TStringDynArray;

var
  {** The default test configuration instance. }
  TestConfig: TZTestConfiguration;

  {** The active test group. }
  TestGroup: string;

implementation

uses ZSysUtils;

{**
  Splits string using the delimiter string.
  @param Str the source string
  @param Delimiters the delimiter string
  @return the result list where plased delimited string
}
function SplitStringToArray(Value: string; Delimiters: string): TStringDynArray;
var
  I: Integer;
  Temp: TStrings;
begin
  Temp := SplitString(Value, Delimiters);
  try
    SetLength(Result, Temp.Count);
    for I := 0 to Temp.Count - 1 do
      Result[I] := Temp[I];
  finally
    Temp.Free;
  end;
end;

{ TZTestConfiguration }

{**
  Destroys this test configuration and cleanups the memory.
}
destructor TZTestConfiguration.Destroy;
begin
  if Assigned(FConfigFile) then
    ConfigFile.Free;

  inherited Destroy;
end;

{**
  Gets a fully qualified name of the configuration file.
  @return a fully qualified name of the configuration file.
}
function TZTestConfiguration.GetConfigFileName: string;
var
  I: Integer;
  Path: string;
begin
  Result := '';

  { Searches for config file within command line parameters. }
  { Config file must be defined as '-c <file path>' }
  for I := 1 to ParamCount do
  begin
    if (ParamStr(I) = CONFIG_CMD_OPTION)
      and (I < ParamCount) then
    begin
      Result := ParamStr(I + 1);
      Break;
    end;
  end;

  { Searches for default configuration file. }
  if Result = '' then
  begin
    Path := '.' + PATH_DELIMITER + DEFAULT_CONFIG_DIR
      + PATH_DELIMITER + DEFAULT_CONFIG_FILE;
    for I := 1 to 4 do
    begin
      if FileExists(Path) then
      begin
        Result := Path;
        Break;
      end;
      Path := '.' + PATH_DELIMITER + '.' + Path;
    end;
  end;

  { If config file still is not defined set it by default. }
  if Result = '' then
    Result := ExtractFileDir(ParamStr(0)) + PATH_DELIMITER + Path;

  FConfigFileName := Result;
end;

{**
  Gets a fully qualified path of the configuration file.
  @return a fully qualified path of the configuration file.
}
function TZTestConfiguration.GetConfigFilePath: string;
begin
  if FConfigFileName = '' then
    FConfigFileName := GetConfigFileName;
  Result := ExtractFilePath(FConfigFileName);
end;

{**
  Loads a configuration from the configuration file.
}
procedure TZTestConfiguration.LoadConfig;
begin
  { Reads a configuration file from the disk. }
  if Assigned(FConfigFile) then
    FConfigFile.Free;
  FConfigFile := TIniFile.Create(GetConfigFileName);

  { Reads default properties. }
  FEnableMemCheck := StrToBoolEx(ReadProperty(COMMON_GROUP,
    ENABLE_MEMCHECK_KEY, FALSE_VALUE));
  FMemCheckLogFile := ReadProperty(COMMON_GROUP,
    MEMCHECK_LOGFILE_KEY, '');
  FMemCheckShowResult := StrToBoolEx(ReadProperty(COMMON_GROUP,
    MEMCHECK_SHOWRESULT_KEY, FALSE_VALUE));
end;

{**
  Reads a configuration property from loaded config file.
  @param Group a name of group of properties. Serves as a section name
    within the INI-file and format real key as <group>.<key>
  @param Key a property key.
  @param Default a default property value.
  @returns a read property value or default value if property
    was not found in the config file.
}
function TZTestConfiguration.ReadProperty(
  const Group, Key, Default: string): string;
begin
  if Assigned(FConfigFile) then
    Result := Trim(FConfigFile.ReadString(Group, Group + '.' + Key, Default))
  else Result := '';
end;

{**
  Activates checking for memory leaks using MemCheck.
}
procedure TZTestConfiguration.ActivateMemCheck;
begin
{$IFDEF USE_MEMCHECK}
  if FEnableMemCheck then
  begin
    MemCheck.ShowLogFileWhenUseful := FMemCheckShowResult;
    MemCheck.MemCheckLogFileName := FMemCheckLogFile;
    MemCheck.MemChk;
  end;
{$ENDIF}
end;

{**
  Deactivates checking for memory leaks.
}
procedure TZTestConfiguration.DeactivateMemCheck;
begin
{$IFDEF USE_MEMCHECK}
  MemCheck.UnMemChk;
{$ENDIF}
end;

initialization
  TestGroup := COMMON_GROUP;

  TestConfig := TZTestConfiguration.Create;
  TestConfig.LoadConfig;
  TestConfig.ActivateMemCheck;
finalization
  if Assigned(TestConfig) then
    TestConfig.Free;
end.

