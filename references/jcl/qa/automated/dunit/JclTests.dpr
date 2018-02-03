{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{ DUnit Test                                                                                       }
{                                                                                                  }
{ Last Update: $Date$                                }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{**************************************************************************************************}

program JclTests;

uses
  Forms,
  GUITestRunner,
  TestJcl8087 in 'units\TestJcl8087.pas',
  TestJclMath in 'units\TestJclMath.pas',
  TestJclStrings in 'units\TestJclStrings.pas',
  TestJclDateTime in 'units\TestJclDateTime.pas',
  TestJclContainer in 'units\TestJclContainer.pas',
  TestJclNotify in 'units\TestJclNotify.pas',
  TestJclDebug in 'units\TestJclDebug.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
  GUITestRunner.RunRegisteredTests;
end.