unit SourceLocDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    CallerBtn: TButton;
    LevelSpinEdit: TSpinEdit;
    AddrBtn: TButton;
    AddrEdit: TEdit;
    StackBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    TraceLocBtn: TButton;
    ProcBtn: TButton;
    ModuleBtn: TButton;
    RawCheckBox: TCheckBox;
    procedure CallerBtnClick(Sender: TObject);
    procedure AddrBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StackBtnClick(Sender: TObject);
    procedure TraceLocBtnClick(Sender: TObject);
    procedure ProcBtnClick(Sender: TObject);
    procedure ModuleBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ReportLocation(Addr: Pointer);
    procedure ReportTime(T: Extended);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  JclCounter, JclDebug;

procedure TForm1.FormCreate(Sender: TObject);
var
  P: Pointer;
begin
  P := @TForm1.AddrBtnClick;
  AddrEdit.Text := IntToHex(Integer(P), 8);
end;

procedure TForm1.ReportLocation(Addr: Pointer);
var
  C: TJclCounter;
  S: string;
  T: Extended;
begin
  StartCount(C);
  S := GetLocationInfoStr(Addr, False, True, True);
  T := StopCount(C);
  Memo1.Lines.Add(S);
  ReportTime(T);
end;

procedure TForm1.ReportTime(T: Extended);
begin
  Memo1.Lines.Add(Format('Time: %4.3f ms'#13#10, [T * 1000]));
end;

procedure TForm1.CallerBtnClick(Sender: TObject);
begin
  ReportLocation(Caller(LevelSpinEdit.Value));
end;

procedure TForm1.AddrBtnClick(Sender: TObject);
var
  Addr: Pointer;
begin
  Addr := Pointer(StrToInt('$' + Trim(AddrEdit.Text)));
  ReportLocation(Addr);
end;

procedure TForm1.StackBtnClick(Sender: TObject);
var
  C: TJclCounter;
  T: Extended;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    StartCount(C);
    with TJclStackInfoList.Create(RawCheckBox.Checked, 0, nil) do
    try
      AddToStrings(SL, False, True, True);
      T := StopCount(C);
      Memo1.Lines.AddStrings(SL);
      ReportTime(T);
    finally
      Free;
    end;
  finally
    SL.Free;
  end;
end;

procedure TForm1.TraceLocBtnClick(Sender: TObject);
begin
  TraceLoc('text');
end;

procedure TForm1.ProcBtnClick(Sender: TObject);
begin
  ShowMessage(ProcByLevel);
end;

procedure TForm1.ModuleBtnClick(Sender: TObject);
begin
  ShowMessage(ModuleByLevel);
end;

end.
