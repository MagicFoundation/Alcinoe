{ $Id: DunitAbout.pas,v 1.3 2003/01/08 14:33:33 juanco Exp $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 1.3 $ 2001/03/08 uberto
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
unit DunitAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

const
  rcs_id :string = '#(@)$Id: DunitAbout.pas,v 1.3 2003/01/08 14:33:33 juanco Exp $';

type
  TDunitAboutBox = class(TForm)
    MainPanel: TPanel;
    NamePanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    IdentMemo: TMemo;
    Credits: TMemo;
    Timer: TTimer;
    LogoPanel: TPanel;
    LogoImage: TImage;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerTimer(Sender: TObject);
    procedure MainPanelClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure Splash;
  
implementation

{$R *.DFM}

const
{$include versioninfo.inc }

procedure TDunitAboutBox.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TDunitAboutBox.TimerTimer(Sender: TObject);
begin
  Close;
end;

procedure TDunitAboutBox.MainPanelClick(Sender: TObject);
begin
  Close;
end;

procedure TDunitAboutBox.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Close;
end;

procedure Splash;
begin
  with TDunitAboutBox.Create(nil) do
  begin
    FormStyle := fsStayOnTop;
    Show;
    Update;
    Timer.Enabled := True;
  end;
end;

procedure TDunitAboutBox.FormCreate(Sender: TObject);
begin
  IdentMemo.Lines[2] := Format('v %s', [ReleaseStr]);
end;

end.
