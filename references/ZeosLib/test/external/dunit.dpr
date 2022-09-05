{ $Id: dunit.dpr,v 1.10 2004/04/29 18:52:46 juanco Exp $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 1.10 $
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
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2003.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Uberto Barbini <uberto@usa.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)

program dunit;

uses
  ShareMem,
  Windows,
  SysUtils,
  Forms,
  Dialogs,
  GUITestRunner,
  TestFramework in 'TestFramework.pas',
  TextTestRunner in 'TextTestRunner.pas',
  DUnitMainForm in 'DUnitMainForm.pas',
  DunitAbout in 'DunitAbout.pas' {DunitAboutBox},
  TestModules in 'TestModules.pas',
  TestExtensions in 'TestExtensions.pas';

{$R *.RES}
{$R versioninfo.res }

const
  rcs_id :string = '#(@)$Id: dunit.dpr,v 1.10 2004/04/29 18:52:46 juanco Exp $';
  SwitchChars = ['-','/'];

procedure RunInConsoleMode;
var
  i :Integer;
begin
  try
    if not IsConsole then
      Windows.AllocConsole;
    for i := 1 to ParamCount do
    begin
      if not (ParamStr(i)[1] in SwitchChars) then
         RegisterModuleTests(ParamStr(i));
    end;
    TextTestRunner.RunRegisteredTests(rxbHaltOnFailures);
  except
    on e:Exception do
      Writeln(Format('%s: %s', [e.ClassName, e.Message]));
  end;
end;

begin
  if FindCmdLineSwitch('c', SwitchChars, true) then
    RunInConsoleMode
  else
  begin
    Application.Initialize;
    Application.Title := 'DUnit - An Extreme Testing Framework';
    if not SysUtils.FindCmdLineSwitch('nologo', ['/','-'], true) then
      DUnitAbout.Splash;
    Application.CreateForm(TDUnitForm, DUnitForm);
    try
      Application.Run;
    except
       on e:Exception do
         ShowMessage(e.Message);
    end;
  end;
end.
