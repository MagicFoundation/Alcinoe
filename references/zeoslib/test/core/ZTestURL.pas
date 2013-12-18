{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for URL Classes                    }
{                                                         }
{         Originally written by Fabiano Bonin             }
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

unit ZTestURL;

interface

{$I ZCore.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZTestDefinitions,ZURL;

type

  { TZURLTest }

  TZURLTest = class (TZCoreGenericTestCase)
  private
  published
    procedure TestAssignToUrl;
    procedure TestAssignToProperties;
    procedure TestAssignToProperties_DatabaseIsFile;
    procedure TestAssignToProperties_NoHostUserPwd;
    procedure TestAssignToProperties_NoUser;
    procedure TestAssignToProperties_Properties;
    procedure TestAssignToProperties_Properties2;
    procedure TestAssignToProperties_ProtocolOnly;
    procedure TestAssignToUrl_UID_PWD;
    procedure TestAssignToUrl_UID_PWD2;
    procedure TestEmpty;
    procedure TestAssignToUrl_NoHost;
    procedure TestAssignToProperties_Properties_NoHost;
  end;

implementation
uses ZCompatibility;

procedure TZURLTest.TestAssignToUrl;
var
  ZURL: TZURL;
begin
  // Test assignment to URL
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0://127.0.0.1:3050/model?username=sysdba;password=masterkey;rolename=public';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('127.0.0.1', ZURL.HostName);
    CheckEquals(3050, ZURL.Port);
    CheckEquals('model', ZURL.Database);
    CheckEquals('sysdba', ZURL.UserName);
    CheckEquals('masterkey', ZURL.Password);
    CheckEquals('rolename=public'+LineEnding, ZURL.Properties.Text);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties;
var
  ZURL: TZURL;
begin
  // Test assignement to properties
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'oracle';
    ZURL.HostName := '127.0.0.1';
    ZURL.Port := 3050;
    ZURL.Database := 'model';
    ZURL.UserName := 'root';
    ZURL.Password := 'passwd';
    ZURL.Properties.Text := 'rolename=public'+LineEnding;
    CheckEquals('zdbc:oracle://127.0.0.1:3050/model?username=root;password=passwd;rolename=public', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToUrl_UID_PWD;
var
  ZURL: TZURL;
begin
  // Test assignment to URL using UID and PWD
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:odbc://localhost/model?UID=admin;PWD=pw;rolename=public';
    CheckEquals('zdbc',ZURL.Prefix);
    CheckEquals('odbc', ZURL.Protocol);
    CheckEquals('localhost', ZURL.HostName);
    CheckEquals(0, ZURL.Port);
    CheckEquals('model', ZURL.Database);
    CheckEquals('admin', ZURL.UserName);
    CheckEquals('pw', ZURL.Password);
    CheckEquals('rolename=public'+LineEnding, ZURL.Properties.Text);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToUrl_UID_PWD2;
var
  ZURL: TZURL;
begin
  // Test assignment to URL using UID and PWD in lower case and out of order
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0://127.0.0.1:3050/model?rolename=public;pwd=masterkey;uid=sysdba';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('127.0.0.1', ZURL.HostName);
    CheckEquals(3050, ZURL.Port);
    CheckEquals('model', ZURL.Database);
    CheckEquals('sysdba', ZURL.UserName);
    CheckEquals('masterkey', ZURL.Password);
    CheckEquals('rolename=public'+LineEnding,ZURL.Properties.Text);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties_NoUser;
var
  ZURL: TZURL;
begin
  // Test assignment to properties without port, user, password and properties
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'firebird-2.0';
    ZURL.HostName := '127.0.0.1';
    ZURL.Database := 'model';
    CheckEquals('zdbc:firebird-2.0://127.0.0.1/model', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties_NoHostUserPwd;
var
  ZURL: TZURL;
begin
  // Test assignment to properties without hostname, port, user, password and properties
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'postgresql';
    ZURL.Database := 'model';
    CheckEquals('zdbc:postgresql:/model', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties_Properties;
var
  ZURL: TZURL;
begin
  // Test assignement to properties, setting user and password in properties
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'mysql';
    ZURL.HostName := '127.0.0.1';
    ZURL.Port := 3050;
    ZURL.Database := 'model';
    ZURL.Properties.Values['username'] := 'admin';
    ZURL.Properties.Values['password'] := 'admin';
    ZURL.Properties.Values['prop1'] := 'prop1';
    ZURL.Properties.Values['prop2'] := 'prop2';
    CheckEquals('zdbc:mysql://127.0.0.1:3050/model?username=admin;password=admin;prop1=prop1;prop2=prop2', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties_Properties2;
var
  ZURL: TZURL;
begin
  // Test assignement to properties, setting user and password in properties as UID and PWD
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'ado';
    ZURL.HostName := 'localhost';
    ZURL.Database := 'database';
    ZURL.Properties.Values['UID'] := 'admin';
    ZURL.Properties.Values['PWD'] := '123';
    ZURL.Properties.Values['role'] := 'rolename';
    CheckEquals('admin', ZURL.UserName);
    CheckEquals('123', ZURL.Password);
    CheckEquals('zdbc:ado://localhost/database?username=admin;password=123;role=rolename', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestEmpty;
var
  ZURL: TZURL;
begin
  try
    ZURL := TZURL.Create;
    CheckEquals('zdbc::', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties_ProtocolOnly;
var
  ZURL: TZURL;
begin
  try
    ZURL := TZURL.Create;
    ZURL.Protocol := 'protocol';
    CheckEquals('zdbc:protocol:', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties_DatabaseIsFile;
var
  ZURL: TZURL;
begin
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0://127.0.0.1/C:\database.fdb?username=sysdba;password=masterkey;rolename=public';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('127.0.0.1', ZURL.HostName);
    CheckEquals(0,ZURL.Port);
    CheckEquals('C:\database.fdb', ZURL.Database);
    CheckEquals('sysdba', ZURL.UserName);
    CheckEquals('masterkey', ZURL.Password);
    CheckEquals('rolename=public'+LineEnding, ZURL.Properties.Text,'11.8');
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToUrl_NoHost;
var
  ZURL: TZURL;
begin
  // Test assignement to URL without hostname
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0:/C:\database.fdb?username=sysdba;password=masterkey;rolename=public';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('', ZURL.HostName);
    CheckEquals(0, ZURL.Port);
    CheckEquals('C:\database.fdb', ZURL.Database);
    CheckEquals('sysdba', ZURL.UserName);
    CheckEquals('masterkey', ZURL.Password);
    CheckEquals('rolename=public'+LineEnding, ZURL.Properties.Text);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties_Properties_NoHost;
var
  ZURL: TZURL;
begin
  // Test assignement to properties without hostname
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'oracle';
    ZURL.HostName := '';
    ZURL.Port := 0;
    ZURL.Database := 'C:\model';
    ZURL.UserName := 'root';
    ZURL.Password := 'passwd';
    ZURL.Properties.Text := 'rolename=public'+LineEnding;
    CheckEquals('zdbc:oracle:/C:\model?username=root;password=passwd;rolename=public', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

initialization
  RegisterTest('core',TZURLTest.Suite);

end.
