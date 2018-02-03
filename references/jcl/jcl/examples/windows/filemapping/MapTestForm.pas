unit MapTestForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, JclFileUtils;

type
  TTestForm = class(TForm)
    mmData: TMemo;
    Label1: TLabel;
    edFile: TEdit;
    edMap: TEdit;
    odFile: TOpenDialog;
    Label3: TLabel;
    cmdFile: TButton;
    cmdGo: TButton;
    cmdWrite: TButton;
    cbTime: TComboBox;
    Timer1: TTimer;
    Label5: TLabel;
    Memo1: TMemo;
    procedure cmdFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdGoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure cbTimeChange(Sender: TObject);
    procedure cmdWriteClick(Sender: TObject);
  private
    FMap: TJclCustomFileMapping;
    FView: TJclFileMappingView;
  public
    { Public declarations }
  end;

var
  TestForm: TTestForm;

implementation

{$R *.DFM}

uses
  JclAnsiStrings,
  MapWriteForm;

procedure TTestForm.cmdFileClick(Sender: TObject);
begin
  if odFile.Execute then
    edFile.Text := odFile.Files[0];
end;

procedure TTestForm.FormCreate(Sender: TObject);
begin
  cbTime.ItemIndex := 0;
end;

procedure TTestForm.cmdGoClick(Sender: TObject);
var
  ViewIndex: Integer;
begin
  if edFile.Text <> '' then
  begin
    FMap := TJclFileMapping.Create(edFile.Text, fmOpenReadWrite, edMap.Text, PAGE_WRITECOPY, 0, nil);
    ViewIndex := FMap.Add(GENERIC_READ or GENERIC_WRITE, GetFileSize(TJclFileMapping(FMap).FileHandle, nil), 0);
    FView := FMap.Views[ViewIndex];
  end
  else
  begin
    FMap := TJclSwapFileMapping.Create(edMap.Text, PAGE_WRITECOPY, 4096, nil);
    ViewIndex := FMap.Add(GENERIC_READ or GENERIC_WRITE, 4096, 0);
    FView := FMap.Views[ViewIndex];
  end;
  cmdGo.Enabled := False;
  cmdWrite.Enabled := True;
  Timer1.Enabled := True;
  Timer1Timer(Self);
end;

procedure TTestForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FView.Free;
  FMap.Free;
end;

procedure TTestForm.Timer1Timer(Sender: TObject);
var
  S: AnsiString;
  L: Cardinal;
begin
  mmData.Clear;

  SetLength(S, 4096);
  FView.Position := 0;
  L := FView.Read(S[1], 4096);
  if L > 0 then
  begin
    StrResetLength(S);
    mmData.Lines.Text := string(S);
  end;
end;

procedure TTestForm.cbTimeChange(Sender: TObject);
begin
  Timer1.Interval := (cbTime.ItemIndex + 1) * 1000;
end;

procedure TTestForm.cmdWriteClick(Sender: TObject);
var
  S: AnsiString;
begin
  WriteForm := TWriteForm.Create(Self);
  WriteForm.ShowModal;
  S := AnsiString(WriteForm.mmWrite.Lines.Text);
  if Length(S) > 0 then
  begin
    FView.Position := 0;
    FView.Write(S[1], Length(S));
  end;
end;

end.
