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
{ The Original Code is JediGUIReadme.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet. Portions created by Florent Ouchet }
{ are Copyright (C) of Florent Ouchet. All Rights Reserved.                                        }
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

unit JediGUIText;

{$I jcl.inc}
{$I crossplatform.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  JediInstall, ExtCtrls;

type
  TTextFrame = class(TFrame, IJediTextPage, IJediPage)
    PanelOptions: TPanel;
    PanelText: TPanel;
    RichEditText: TRichEdit;
    procedure RichEditTextDblClick(Sender: TObject);
  private
    FTextFileName: string;
    FOptions: TList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // IJediPage
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetHintAtPos(ScreenX, ScreenY: Integer): string;
    procedure Show;
    // IJediTextPage
    procedure SetTextFileName(const Value: string);
    function GetTextFileName: string;
    function AddOption(const Caption: string): Integer;
    function GetOptionCount: Integer;
    function GetOption(Index: Integer): Boolean;
    procedure SetOption(Index: Integer; Value: Boolean);

    property TextFileName: string read GetTextFileName write SetTextFileName;
    property OptionCount: Integer read GetOptionCount;
    property Options[Index: Integer]: Boolean read GetOption write SetOption;
  end;

implementation

{$R *.dfm}

uses
  JclShell;

type
  TOptionRec = record
    CheckBox: TCheckBox;
  end;

  POptionRec = ^TOptionRec;

constructor TTextFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TList.Create;
end;

destructor TTextFrame.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FOptions.Count - 1 do
    Dispose(FOptions.Items[Index]);
  FOptions.Free;
  inherited Destroy;
end;

function TTextFrame.GetCaption: string;
begin
  Result := (Parent as TTabSheet).Caption;
end;

function TTextFrame.GetTextFileName: string;
begin
  Result := FTextFileName;
end;

procedure TTextFrame.RichEditTextDblClick(Sender: TObject);
begin
  { TODO: implement for Unix }
  ShellExecEx(TextFileName);
end;

procedure TTextFrame.SetCaption(const Value: string);
begin
  (Parent as TTabSheet).Caption := Value;
end;

function TTextFrame.GetHintAtPos(ScreenX, ScreenY: Integer): string;
begin
  Result := '';
end;

procedure TTextFrame.SetTextFileName(const Value: string);
begin
  FTextFileName := Value;
  if FileExists(Value) then
    RichEditText.Lines.LoadFromFile(Value);
end;

procedure TTextFrame.Show;
var
  ATabSheet: TTabSheet;
begin
  ATabSheet := Parent as TTabSheet;
  (ATabSheet.Parent as TPageControl).ActivePage := ATabSheet;
end;

function TTextFrame.AddOption(const Caption: string): Integer;
var
  AOptionRec: POptionRec;
  ControlTop: Integer;
begin
  if FOptions.Count > 0 then
  begin
    AOptionRec := FOptions.Items[FOptions.Count - 1];
    ControlTop := AOptionRec^.CheckBox.Top + AOptionRec^.CheckBox.Height + 10;
  end
  else
    ControlTop := 16;

  New(AOptionRec);
  AOptionRec^.CheckBox := TCheckBox.Create(Self);
  AOptionRec^.CheckBox.Parent := PanelOptions;
  AOptionRec^.CheckBox.Anchors := [akLeft, akTop, akRight];
  AOptionRec^.CheckBox.Caption := Caption;

  AOptionRec^.CheckBox.SetBounds(16, ControlTop, PanelOptions.ClientWidth - 32, AOptionRec^.CheckBox.Height);

  PanelOptions.ClientHeight := AOptionRec^.CheckBox.Top + AOptionRec^.CheckBox.Height + 16;

  Result := FOptions.Add(AOptionRec);
end;

procedure TTextFrame.SetOption(Index: Integer; Value: Boolean);
begin
  POptionRec(FOptions.Items[Index])^.CheckBox.Checked := Value;
end;

function TTextFrame.GetOption(Index: Integer): Boolean;
begin
  Result := POptionRec(FOptions.Items[Index])^.CheckBox.Checked;
end;

function TTextFrame.GetOptionCount: Integer;
begin
  Result := FOptions.Count;
end;

end.
