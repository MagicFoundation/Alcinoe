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
{ The Original Code is MapToJdbgMain.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.                                  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Michael Chernyshev                                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit MapToJdbgMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ImgList, ActnList, Menus, ToolWin;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    MainMenu1: TMainMenu;
    ActionList1: TActionList;
    ImageList1: TImageList;
    StatusBar1: TStatusBar;
    Exit1: TAction;
    Open1: TAction;
    Convert1: TAction;
    File1: TMenuItem;
    Open2: TMenuItem;
    N1: TMenuItem;
    Exit2: TMenuItem;
    OpenDialog1: TOpenDialog;
    FilesListView: TListView;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    Run1: TMenuItem;
    Convert2: TMenuItem;
    procedure Exit1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Open1Execute(Sender: TObject);
    procedure Convert1Execute(Sender: TObject);
    procedure Convert1Update(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses
  JclCounter, JclDebug, JclFileUtils, JclUnitConv;

resourcestring
  RsConverting = 'Converting ...';
  RsConversionStatus = '%d file(s) converted, Conversion time: %5.3f sec.';
  RsLinkerBugs = 'Linker bugs: %d';

procedure TMainForm.FormCreate(Sender: TObject);
var
  MapFileName, ExeFileName: TFileName;
  LinkerBugUnit: string;
  MapFileSize, JclDebugDataSize, LineNumberErrors: Integer;
begin
  if ParamCount = 1 then
  begin
    MapFileName := ParamStr(1);
    if MapFileName <> '' then
    begin
      if not ConvertMapFileToJdbgFile(MapFileName) then
        ExitCode := 1;
      Application.ShowMainForm := False;
      Application.Terminate;
    end;
  end
  else
  if ParamCount = 2 then
  begin
    MapFileName := ParamStr(1);
    ExeFileName := ParamStr(2);
    if (MapFileName <> '') and (ExeFileName <> '') then
    begin
      if not InsertDebugDataIntoExecutableFile(ExeFileName, MapFileName, LinkerBugUnit, MapFileSize, JclDebugDataSize, LineNumberErrors) then
        ExitCode := 1;
      Application.ShowMainForm := False;
      Application.Terminate;
    end;
  end;
end;

procedure TMainForm.Exit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Open1Execute(Sender: TObject);
var
  I, FileSize: Integer;
begin
  with OpenDialog1 do
  begin
    FileName := '';
    if Execute then
    begin
      with FilesListView.Items do
      begin
        BeginUpdate;
        try
          Clear;
          for I := 0 to Files.Count - 1 do
            with Add do
            begin
              Caption := PathExtractFileNameNoExt(Files[I]);
              FileSize := FileGetSize(Files[I]);
              SubItems.AddObject(IntToStr(FileSize), Pointer(FileSize));
              SubItems.Add('');
              SubItems.Add('');
              SubItems.Add(Files[I]);
              SubItems.Add('');
              SubItems.Add('');
              ImageIndex := 1;
            end;
        finally
          EndUpdate;
        end;
      end;
      StatusBar1.Panels[0].Text := '';
    end;
  end;
end;

procedure TMainForm.Convert1Execute(Sender: TObject);
var
  I, JdbgFileSize, FilesConverted, LineNumberErrors, LinkerBugCnt: Integer;
  MapFileName, JdbgFileName: TFileName;
  Ratio: Extended;
  LinkerBugUnit: string;
  Cnt: TJclCounter;
begin
  Screen.Cursor := crHourGlass;
  try
    with FilesListView do
    begin
      StatusBar1.Panels[0].Text := RsConverting;
      StatusBar1.Panels[1].Text := '';
      StatusBar1.Update;
      Items.BeginUpdate;
      for I := 0 to Items.Count - 1 do
        with Items[I] do
        begin
          SubItems[1] := '';
          SubItems[2] := '';
          SubItems[4] := '';
          SubItems[5] := '';
          ImageIndex := 1;
        end;
      Items.EndUpdate;
      Update;
      FilesConverted := 0;
      LinkerBugCnt := 0;
      StartCount(Cnt);
      for I := 0 to Items.Count - 1 do
      begin
        with Items[I] do
        begin
          MapFileName := SubItems[3];
          JdbgFileName := ChangeFileExt(MapFileName, JclDbgFileExtension);
          if ConvertMapFileToJdbgFile(MapFileName, LinkerBugUnit, LineNumberErrors) then
          begin
            ImageIndex := 3;
            JdbgFileSize := FileGetSize(JdbgFileName);
            Ratio := JdbgFileSize * 100 / Integer(SubItems.Objects[0]);
            SubItems[1] := IntToStr(JdbgFileSize);
            SubItems[2] := Format('%3.1f %%', [Ratio]);
            SubItems[4] := LinkerBugUnit;
            if LinkerBugUnit <> '' then
              Inc(LinkerBugCnt);
            if LineNumberErrors > 0 then
              SubItems[5] := IntToStr(LineNumberErrors);
            Inc(FilesConverted);
          end
          else
          begin
            SubItems[0] := '';
            ImageIndex := 4;
          end;
        end;
        Update;
      end;
      StatusBar1.Panels[0].Text := Format(RsConversionStatus, [FilesConverted, StopCount(Cnt)]);
      StatusBar1.Panels[1].Text := Format(RsLinkerBugs, [LinkerBugCnt]);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.Convert1Update(Sender: TObject);
begin
  Convert1.Enabled := FilesListView.Items.Count > 0;
end;

end.
