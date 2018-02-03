unit TextReaderDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, JclFileUtils;

type
  TMainForm = class(TForm)
    TextListView: TListView;
    OpenDialog: TOpenDialog;
    OpenBtn: TButton;
    StatusBar: TStatusBar;
    ReadLnBtn: TButton;
    ReadLnLabel: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure TextListViewData(Sender: TObject; Item: TListItem);
    procedure OpenBtnClick(Sender: TObject);
    procedure ReadLnBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFileName: string;
    FTextReader: TJclAnsiMappedTextReader;
  public
    procedure ClearLabels;
    procedure OpenFile(const FileName: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  JclCounter, JclSysUtils;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ClearLabels;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTextReader);
end;

procedure TMainForm.ClearLabels;
begin
  ReadLnLabel.Caption := '';
end;

procedure TMainForm.OpenFile(const FileName: string);
var
  C: TJclCounter;
  LineCount: Integer;
  LineCountTime: Extended;
begin
  FreeAndNil(FTextReader);
  FFileName := '';
  TextListView.Items.Count := 0;
  StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := '';
  ClearLabels;
  FTextReader := TJclAnsiMappedTextReader.Create(FileName);
  FFileName := FileName;
  StartCount(C);
  LineCount := FTextReader.LineCount;
  LineCountTime := StopCount(C);
  TextListView.Items.Count := LineCount;
  TextListView.Invalidate;
  StatusBar.Panels[0].Text := ExtractFileName(FileName);
  StatusBar.Panels[1].Text := Format('Lines: %d, Counting time: %.2f ms', [LineCount, LineCountTime * 1000]);
end;

procedure TMainForm.TextListViewData(Sender: TObject; Item: TListItem);
begin
  Item.Caption := string(FTextReader.Lines[Item.Index]);
end;

procedure TMainForm.OpenBtnClick(Sender: TObject);
begin
  with OpenDialog do
  begin
    FileName := '';
    if Execute then
      OpenFile(FileName);
  end;
end;

procedure TMainForm.ReadLnBtnClick(Sender: TObject);
var
  C: TJclCounter;
  TotalTime, StringListTotalTime, AssignFileTotalTime: Extended;
  LineCount, I: Integer;
  S: string;
  Reader: TJclAnsiMappedTextReader;
  SL: TStringList;
  T: TextFile;
begin
  if FFileName = '' then
    Exit;
  Screen.Cursor := crHourGlass;
  try
    ClearLabels;
    // TJclAnsiMappedTextReader
    LineCount := 0;
    StartCount(C);
    Reader := TJclAnsiMappedTextReader.Create(FFileName);
    try
      Reader.GoBegin;
      while not Reader.Eof do
      begin
        S := string(Reader.ReadLn);
        Inc(LineCount);
      end;
      TotalTime := StopCount(C);
    finally
      Reader.Free;
    end;
    // TStringList
    SL := TStringList.Create;
    try
      StartCount(C);
      SL.LoadFromFile(FFileName);
      for I := 0 to SL.Count - 1 do
        S := SL[I];
      StringListTotalTime := StopCount(C);
    finally
      SL.Free;
    end;
    // AssignFile
    StartCount(C);
    AssignFile(T, FFileName);
    Reset(T);
    while not Eof(T) do
      ReadLn(T, S);
    AssignFileTotalTime := StopCount(C);
    CloseFile(T);

    ReadLnLabel.Caption := Format('Lines: %d, TJclAnsiMappedTextReader: %.2f ms,  TStringList: %.2f ms,  AssignFile: %.2f ms',
      [LineCount, TotalTime * 1000, StringListTotalTime * 1000, AssignFileTotalTime * 1000]);
  finally
    Screen.Cursor := crDefault;
  end;      
end;



end.
