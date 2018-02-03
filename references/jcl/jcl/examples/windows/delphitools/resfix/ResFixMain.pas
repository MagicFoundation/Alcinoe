{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) - Delphi Tools                                                   }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is ResFixMain.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date$                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit ResFixMain;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ImgList, ActnList, Menus, JclPeImage, StdCtrls,
  ExtCtrls;

type
  TMainForm = class(TForm)
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    MainMenu1: TMainMenu;
    ActionList1: TActionList;
    ImageList1: TImageList;
    StatusBar: TStatusBar;
    Open1: TAction;
    Exit1: TAction;
    About1: TAction;
    File1: TMenuItem;
    Open2: TMenuItem;
    N1: TMenuItem;
    Exit2: TMenuItem;
    Help1: TMenuItem;
    About11: TMenuItem;
    Description1: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    Descriptionofbug1: TMenuItem;
    N2: TMenuItem;
    OpenFileDialog: TOpenDialog;
    ResListView: TListView;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MinResLabel: TLabel;
    MaxResLabel: TLabel;
    FactorLabel: TLabel;
    SendMail1: TAction;
    Support1: TMenuItem;
    procedure Exit1Execute(Sender: TObject);
    procedure Description1Execute(Sender: TObject);
    procedure About1Execute(Sender: TObject);
    procedure Open1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ResListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure SendMail1Execute(Sender: TObject);
    procedure CoolBar1Resize(Sender: TObject);
  private
    FPeImage: TJclPeImage;
    procedure OpenFile(const FileName: TFileName);
    procedure ProcessFile;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses About, ToolsUtils, JclLogic, JclShell, JclSysUtils;

{$R *.DFM}

resourcestring
  RsCheckApp = 'It is recommended to check the application. Would you like to start it ?';
  RsDescriptionURL = 'http://support.microsoft.com/support/kb/articles/Q182/8/19.asp';
  RsFixed = 'File was fixed';
  RsNoFixes = 'Not fixes needed';

type
  TJclPeImageHack = class(TJclPeImage);

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FPeImage := TJclPeImage.Create;
  TJclPeImageHack(FPeImage).ReadOnlyAccess := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPeImage);
end;

procedure TMainForm.Exit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Description1Execute(Sender: TObject);
begin
  Win32Check(ShellExecEx(RsDescriptionURL));
end;

procedure TMainForm.About1Execute(Sender: TObject);
begin
  ShowToolsAboutBox;
end;

procedure TMainForm.Open1Execute(Sender: TObject);
begin
  with OpenFileDialog do
  begin
    FileName := '';
    if Execute then OpenFile(FileName);
  end;
end;

procedure TMainForm.OpenFile(const FileName: TFileName);
begin
  FPeImage.FileName := FileName;
  StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := FileName;
  ProcessFile;
end;

procedure TMainForm.ProcessFile;
var
  MinResSize, MaxResSize: Integer;
  ScalingFactor: Integer;
  NeedFix, AnyFixes: Boolean;
  FileName: TFileName;

  procedure ScanResources(List: TJclPeResourceList);
  var
    I, Size: Integer;
    Item: TJclPeResourceItem;
  begin
    for I := 0 to List.Count - 1 do
    begin
      Item := List[I];
      if Item.IsDirectory then
        ScanResources(Item.List)
      else
      begin
        Size := Item.DataEntry^.Size;
        MinResSize := Min(MinResSize, Size);
        MaxResSize := Max(MaxResSize, Size);
        with ResListView.Items.Add do
        begin
          Caption := Item.ResourceTypeStr;
          Data := Item;
          SubItems.Add(Item.ParentItem.Name);
          SubItems.Add(Format('%u', [Size]));
          SubItems.Add('');
        end;  
      end;
    end;
  end;

  procedure FixResources(List: TJclPeResourceList);
  var
    I, Size: Integer;
    Item: TJclPeResourceItem;
  begin
    for I := 0 to List.Count - 1 do
    begin
      Item := List[I];
      if Item.IsDirectory then
        FixResources(Item.List)
      else
        if Item.ResourceType in [rtCursor, rtIcon, rtCursorEntry, rtIconEntry] then
        begin
          Size := Item.DataEntry^.Size;
          if (Size mod ScalingFactor <> 0) or (Size < ScalingFactor * 2) then
          begin
            Size := Max((Size div ScalingFactor + 1) * ScalingFactor, ScalingFactor * 2);
            Item.DataEntry^.Size := Size;
            AnyFixes := True;
            ResListView.FindData(0, Item, True, False).SubItems[2] := Format('%u', [Size]);
          end;
        end;
    end;
  end;

begin
  MinResSize := MaxInt;
  MaxResSize := 0;
  FileName := FPeImage.FileName;
  ResListView.Items.BeginUpdate;
  try
    ResListView.Items.Clear;
    ScanResources(FPeImage.ResourceList);

    ScalingFactor := ((MaxResSize div 65536) div 2 + 1) * 2;
    MinResLabel.Caption := Format('%d', [MinResSize]);
    MaxResLabel.Caption := Format('%d', [MaxResSize]);
    FactorLabel.Caption := Format('%d', [ScalingFactor]);

    NeedFix := (MaxResSize >= 65536) and (MinResSize mod ScalingFactor <> 0);
    AnyFixes := False;
    if NeedFix then FixResources(FPeImage.ResourceList);
    FPeImage.FileName := '';
    ListViewFocusFirstItem(ResListView);
  finally
    ResListView.Items.EndUpdate;
  end;
  with StatusBar.Panels[0] do
  if AnyFixes then
  begin
    Text := RsFixed;
    if MessBox(RsCheckApp, MB_YESNO or MB_ICONQUESTION) = ID_YES then
      ShellExecEx(FileName);
  end else
    Text := RsNoFixes;
end;

procedure TMainForm.ResListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Item.SubItems[2] <> '' then
    Sender.Canvas.Font.Color := clRed;
end;

procedure TMainForm.SendMail1Execute(Sender: TObject);
begin
  SendEmail;
end;

procedure TMainForm.CoolBar1Resize(Sender: TObject);
begin
  D4FixCoolBarResizePaint(Sender);
end;

end.
