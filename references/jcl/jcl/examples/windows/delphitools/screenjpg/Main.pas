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
{ The Original Code is Main.pas.                                                                   }
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

unit Main;

{$I JCL.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ActnList, ImgList, Menus, ExtCtrls, StdCtrls, Jpeg,
  ClipBrd, ExtDlgs;

type
  TMainForm = class(TForm)
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    ImageList1: TImageList;
    ActionList1: TActionList;
    OpenFile1: TAction;
    Exit1: TAction;
    File1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Exit2: TMenuItem;
    ToolButton1: TToolButton;
    ScrollBox: TScrollBox;
    Image1: TImage;
    RatioComboBox: TComboBox;
    SaveFile1: TAction;
    SaveAs1: TMenuItem;
    ToolButton3: TToolButton;
    SaveDialog1: TSaveDialog;
    ColorComboBox: TComboBox;
    Paste1: TAction;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    Edit1: TMenuItem;
    Paste11: TMenuItem;
    Help1: TMenuItem;
    OpenDialog1: TOpenPictureDialog;
    About1: TAction;
    About2: TMenuItem;
    procedure Exit1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenFile1Execute(Sender: TObject);
    procedure RatioComboBoxChange(Sender: TObject);
    procedure SaveFile1Execute(Sender: TObject);
    procedure Paste1Execute(Sender: TObject);
    procedure Paste1Update(Sender: TObject);
    procedure SaveFile1Update(Sender: TObject);
    procedure About1Execute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CoolBar1Resize(Sender: TObject);
  private
    FJpegImage: TJPEGImage;
    FFileName: TFileName;
    FModified: Boolean;
    FOriginalPicture: TPicture;
    procedure CompressPicture;
    procedure FillCombos;
    procedure EnableCombos;
  public
    function CheckSaved: Boolean;
    procedure OpenFile;
    function SaveFile: Boolean;
    procedure UpdatePicture;
    property Modified: Boolean read FModified;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses
  ToolsUtils, JclSysUtils;

resourcestring
  RsSaveImage = 'Save current image ?';
  RsJpegSize = 'JPEG Size: %.0n';

function TMainForm.CheckSaved: Boolean;
begin
  Result := not Modified;
  if not Result then
    case MessBox(RsSaveImage, MB_ICONEXCLAMATION or MB_YESNOCANCEL) of
      ID_YES: Result := SaveFile;
      ID_NO: Result := True;
    else
      Result := False;
    end;
end;

procedure TMainForm.CompressPicture;
var
  Ratio: Integer;
begin
  with RatioComboBox do Ratio := Integer(Items.Objects[ItemIndex]);
  FJpegImage.Grayscale := (ColorComboBox.ItemIndex = 0);
  FJpegImage.CompressionQuality := Ratio;
  FJpegImage.Assign(FOriginalPicture.Graphic);
end;

procedure TMainForm.EnableCombos;
begin
  RatioComboBox.Enabled := True;
  RatioComboBox.Color := clWindow;
  ColorComboBox.Enabled := True;
  ColorComboBox.Color := clWindow;
end;

procedure TMainForm.Exit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.OpenFile;
begin
  if CheckSaved then
  begin
    with OpenDialog1 do
    begin
      FileName := '';
      if Execute then
      begin
        FFileName := FileName;
        FOriginalPicture.LoadFromFile(FileName);
        UpdatePicture;
      end;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FJpegImage := TJPEGImage.Create;
  FOriginalPicture := TPicture.Create;
  Image1.Align := alNone;
  FillCombos;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FOriginalPicture);
  FreeAndNil(FJpegImage);
end;

procedure TMainForm.UpdatePicture;
var
  MemStream : TMemoryStream;
begin
  Screen.Cursor := crHourGlass;
  try
    EnableCombos;
    CompressPicture;
    MemStream := TMemoryStream.Create;
    try
      FJpegImage.SaveToStream(MemStream);
      StatusBar1.Panels[0].Text := Format(RsJpegSize, [IntToExtended(MemStream.Size)]);
      MemStream.Position := 0;
      FJpegImage.LoadFromStream(MemStream);
      Image1.Picture.Assign(FJpegImage);
      Image1.Update;
    finally
      MemStream.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.OpenFile1Execute(Sender: TObject);
begin
  OpenFile;
end;

procedure TMainForm.FillCombos;
const
  QualityTable: array [0..10] of TJPEGQualityRange =
    (10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100);
var
  I: Integer;
begin
  with RatioComboBox do
  begin
    for I := Low(QualityTable) to High(QualityTable) do
      Items.AddObject(Format('%d%%', [QualityTable[I]]), Pointer(QualityTable[I]));
    ItemIndex := 8;
  end;
  ColorComboBox.ItemIndex := 1;
end;

procedure TMainForm.RatioComboBoxChange(Sender: TObject);
begin
  UpdatePicture;
  FModified := True;
end;

procedure TMainForm.SaveFile1Execute(Sender: TObject);
begin
  SaveFile;
end;

procedure TMainForm.Paste1Execute(Sender: TObject);
begin
  if CheckSaved then
  begin
    FOriginalPicture.Assign(Clipboard);
    FFileName := '';
    UpdatePicture;
    FModified := True;
  end;  
end;

procedure TMainForm.Paste1Update(Sender: TObject);
begin
  Paste1.Enabled := Clipboard.HasFormat(CF_BITMAP);
end;

procedure TMainForm.SaveFile1Update(Sender: TObject);
begin
  SaveFile1.Enabled := Assigned(Image1.Picture.Graphic);
end;

function TMainForm.SaveFile: Boolean;
begin
  Result := False;
  with SaveDialog1 do
  begin
    FileName := ChangeFileExt(FFileName, '.jpeg');
    if Execute then
    begin
      FJpegImage.SaveToFile(FileName);
      Result := True;
      FModified := False;
    end;
  end;
end;

procedure TMainForm.About1Execute(Sender: TObject);
begin
  ShowToolsAboutBox;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckSaved;
end;

procedure TMainForm.CoolBar1Resize(Sender: TObject);
begin
  D4FixCoolBarResizePaint(Sender);
end;

end.
