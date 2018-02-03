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
{ The Original Code is JclOtaWizardForm.pas.                                                       }
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
unit JclOtaWizardForm;

interface

{$I jcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ActnList,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOtaWizardFrame;

type
  TJclWizardForm = class(TForm)
    ButtonCancel: TButton;
    ButtonFinish: TButton;
    ButtonNext: TButton;
    ButtonPrevious: TButton;
    Bevel1: TBevel;
    PanelTitle: TPanel;
    ImageJcl: TImage;
    LabelJcl: TLabel;
    LabelProgression: TLabel;
    ActionListButtons: TActionList;
    ActionPrevious: TAction;
    ActionNext: TAction;
    ActionFinish: TAction;
    PanelPages: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ActionPreviousExecute(Sender: TObject);
    procedure ActionPreviousUpdate(Sender: TObject);
    procedure ActionNextExecute(Sender: TObject);
    procedure ActionNextUpdate(Sender: TObject);
    procedure ActionFinishExecute(Sender: TObject);
    procedure ActionFinishUpdate(Sender: TObject);
  private
    FDescription: string;
    FPageIndex: Integer;
    FExecuting: Boolean;
    function GetPageCount: Integer;
    function GetPageIndex: Integer;
    procedure SetPageIndex(const Value: Integer);
    function GetActivePage: TJclWizardFrame;
    function GetPage(Index: Integer): TJclWizardFrame;
  public
    function AddPage(const WizardFrame: TJclWizardFrame): Integer;
    function Execute: Boolean;

    property PageCount: Integer read GetPageCount;
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
    property Description: string read FDescription write FDescription;
    property Pages[Index: Integer]: TJclWizardFrame read GetPage;
    property ActivePage: TJclWizardFrame read GetActivePage;
    property Executing: Boolean read FExecuting;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}

uses
  JclOtaResources;

//=== { TJclWizardForm } =====================================================

procedure TJclWizardForm.ActionFinishExecute(Sender: TObject);
begin
  PageIndex := -1;
  ModalResult := mrOk;
end;

procedure TJclWizardForm.ActionFinishUpdate(Sender: TObject);
var
  CurrentPage: TJclWizardFrame;
begin
  CurrentPage := ActivePage;
  (Sender as TAction).Enabled := Assigned(CurrentPage) and CurrentPage.SupportsFinish;
end;

procedure TJclWizardForm.ActionNextExecute(Sender: TObject);
begin
  PageIndex := PageIndex + 1;
end;

procedure TJclWizardForm.ActionNextUpdate(Sender: TObject);
var
  CurrentPage: TJclWizardFrame;
begin
  CurrentPage := ActivePage;
  (Sender as TAction).Enabled := (PageIndex < (PageCount - 1))
    and Assigned(CurrentPage) and CurrentPage.SupportsNext;
end;

procedure TJclWizardForm.ActionPreviousExecute(Sender: TObject);
begin
  PageIndex := PageIndex - 1;
end;

procedure TJclWizardForm.ActionPreviousUpdate(Sender: TObject);
var
  CurrentPage: TJclWizardFrame;
begin
  CurrentPage := ActivePage;
  (Sender as TAction).Enabled := (PageIndex > 0)
    and Assigned(CurrentPage) and CurrentPage.SupportsPrevious;
end;

function TJclWizardForm.AddPage(const WizardFrame: TJclWizardFrame): Integer;
begin
  WizardFrame.Visible := False;
  WizardFrame.Parent := PanelPages;
  WizardFrame.Align := alClient;
  for Result := 0 to PanelPages.ControlCount - 1 do
    if PanelPages.Controls[Result] = WizardFrame then
      Exit;
  Result := -1;
end;

function TJclWizardForm.Execute: Boolean;
begin
  FExecuting := True;
  try
    if PageCount > 0 then
    begin
      FPageIndex := -1;
      PageIndex := 0;
      Result := ShowModal = mrOk;
    end
    else
      Result := False;
  finally
    FExecuting := False;
  end;
end;

procedure TJclWizardForm.FormCreate(Sender: TObject);
begin
  {$IFDEF COMPILER7_UP}
  PanelTitle.ParentBackground := False;
  {$ENDIF COMPILER7_UP}

  ActionPrevious.Caption := LoadResString(@RsPrevious);
  ActionNext.Caption := LoadResString(@RsNext);
  ActionFinish.Caption := LoadResString(@RsFinish);
  ButtonCancel.Caption := LoadResString(@RsCancel);
  LabelJcl.Caption := LoadResString(@RsAboutDialogTitle);
  try
    ImageJcl.Picture.Bitmap.TransparentMode := tmAuto;
    ImageJcl.Picture.Bitmap.Transparent := True;
    ImageJcl.Picture.Bitmap.LoadFromResourceName(FindResourceHInstance(HInstance), 'JCLSPLASH');
  except

  end;
end;

function TJclWizardForm.GetActivePage: TJclWizardFrame;
begin
  if Executing then
    Result := Pages[PageIndex]
  else
    Result := nil;
end;

function TJclWizardForm.GetPage(Index: Integer): TJclWizardFrame;
begin
  if (Index >= 0) and (Index < PanelPages.ControlCount) then
    Result := PanelPages.Controls[Index] as TJclWizardFrame
  else
    Result := nil;
end;

function TJclWizardForm.GetPageCount: Integer;
begin
  Result := PanelPages.ControlCount;
end;

function TJclWizardForm.GetPageIndex: Integer;
begin
  if Executing then
    Result := FPageIndex
  else
    Result := -1;
end;

procedure TJclWizardForm.SetPageIndex(const Value: Integer);
var
  Direction: TJclWizardDirection;
  AFrame: TJclWizardFrame;
begin
  if Value > FPageIndex then
    Direction := wdForward
  else
    Direction := wdBackward;

  AFrame := Pages[FPageIndex];
  if Assigned(AFrame) then
  begin
    AFrame.PageDesactivated(Direction);
    AFrame.Visible := False;
  end;

  FPageIndex := Value;

  AFrame := Pages[FPageIndex];
  if Assigned(AFrame) then
  begin
    AFrame.PageActivated(Direction);
    AFrame.Visible := True;
    LabelProgression.Caption := Format(LoadResString(@RsWizardProgression), [PageIndex+1 {one based}, PageCount, AFrame.Caption]);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
