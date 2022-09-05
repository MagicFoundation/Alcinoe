{ $Id: TestModules.pas,v 1.6 2003/01/08 14:33:36 juanco Exp $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 1.6 $ 2001/03/08 uberto
}
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo A±ez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2003.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)
unit TestModules;

interface
uses
  Windows,
  SysUtils,
  TestFramework;


const
  rcs_id :string = '#(@)$Id: TestModules.pas,v 1.6 2003/01/08 14:33:36 juanco Exp $';

type
  TModuleRecord = record
    Handle :THandle;
    Test   :ITest;
  end;

  TGetTestFunc = function :ITest;


var
  __Modules   :array of TModuleRecord = nil;

function LoadModuleTests(LibName: string) :ITest;
procedure RegisterModuleTests(LibName: string);
procedure UnloadTestModules;
                                                    
implementation

function LoadModuleTests(LibName: string) :ITest;
var
  LibHandle :THandle;
  GetTest   :TGetTestFunc;
  U         :IUnknown;
begin
  Result := nil;
  if ExtractFileExt(LibName) = '' then
  begin
    LibName := ChangeFileExt(LibName, '.dll');
    if not FileExists(LibName) then
       LibName := ChangeFileExt(LibName, '.dtl');
  end;

  LibHandle := LoadLibrary(PChar(AnsiString(LibName)));
  if LibHandle = 0 then
    raise EDUnitException.Create(Format('Could not load module %s: %s', [LibName, SysErrorMessage(GetLastError)]))
  else
  begin
    GetTest := GetProcAddress(LibHandle, 'Test');
    if not Assigned(GetTest) then
      raise EDUnitException.Create(Format('Module "%s" does not export a "Test" function: %s', [LibName, SysErrorMessage(GetLastError)]))
    else
    begin
      U := GetTest;
      if U = nil then
        U := TestFramework.TestSuite(LibName, []);
      if 0 <> U.QueryInterface(ITest, Result) then
        raise EDUnitException.Create(Format('Module "%s.Test" did not return an ITest', [LibName]))
      else
      begin
        Assert(Result <> nil);

        SetLength(__Modules, 1+Length(__Modules));
        __Modules[High(__Modules)].Handle := LibHandle;
        __Modules[High(__Modules)].Test   := Result;
      end;
    end;
  end;
end;

procedure RegisterModuleTests(LibName: string);
begin
  RegisterTest(LoadModuleTests(LibName));
end;

procedure UnloadTestModules;
var
  i :Integer;
begin
  ClearRegistry;
  for i := Low(__Modules) to High(__Modules) do
    with __Modules[i] do
    begin
      Test := nil;
      FreeLibrary(Handle);
    end;
  __Modules := nil;
end;

initialization
finalization
  UnloadTestModules;
end.
