{ $Id: DUnitMainForm.pas,v 1.6 2003/01/08 14:33:33 juanco Exp $ }
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
unit DUnitMainForm;

interface

uses
  Windows,
  Messages,
  Classes,
  SysUtils,

  TestFramework,
  TestModules,
  GUITestRunner,

  Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, ImgList, StdCtrls, ComCtrls, ToolWin,
  ExtCtrls;

const
  rcs_id :string = '#(@)$Id: DUnitMainForm.pas,v 1.6 2003/01/08 14:33:33 juanco Exp $';

type
  TDUnitForm = class(TGUITestRunner)
    DUnitActions: TActionList;
    LoadTestsAction: TAction;
    UnloadTestscAction: TAction;
    OpenTestsDialog: TOpenDialog;
    LoadTests1: TMenuItem;
    AboutAction: TAction;
    Help1: TMenuItem;
    AboutItem: TMenuItem;
    procedure LoadTestsActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AboutActionExecute(Sender: TObject);
  protected
    FRootSuite :ITestSuite;
  public
    property RootSuite :ITestSuite read FRootSuite;
  end;

var
  DUnitForm: TDUnitForm;

implementation

uses DunitAbout;

{$R *.DFM}

{ TDUnitForm }

procedure TDUnitForm.LoadTestsActionExecute(Sender: TObject);
var
  f  :Integer;
begin
  inherited;
  with OpenTestsDialog do
    if Execute then
    begin
      for f := 0 to Files.Count-1 do
      begin
        try
          RootSuite.AddTest(LoadModuleTests(Files[f]));
        except
          on e :Exception do
            MessageDlg(e.Message, mtError, [mbOK], 0);
        end;
      end;
      inherited Suite := RootSuite;
    end;
end;

procedure TDUnitForm.FormCreate(Sender: TObject);
var
  i  :Integer;
  Suite :ITest;
begin
  inherited;
  FRootSuite := TTestSuite.Create('All Tests');
  for i := 1 to ParamCount do
  begin
    if not (ParamStr(i)[1] in ['/','-']) then
    begin
      Suite := LoadModuleTests(ParamStr(i));
      if Suite <> nil then
        RootSuite.AddTest(Suite);
    end;
  end;
  inherited Suite := RootSuite;
end;

procedure TDUnitForm.FormDestroy(Sender: TObject);
begin
  inherited Suite := nil;
  FRootSuite := nil;
  inherited;
  UnloadTestModules;
end;

procedure TDUnitForm.AboutActionExecute(Sender: TObject);
begin
  inherited;
  TDunitAboutBox.Create(nil).ShowModal;
end;

end.
 
