unit mORMotVisualizerTesterMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  { mORMot }
  SynCommons;

procedure TMainForm.Button1Click(Sender: TObject);
var
  LTimeLog: TTimeLog;
  LTempTimeLog: TTimeLog;
begin
  PTimeLogBits(@LTimeLog)^.From(0, 0, 0, 0, 0, 0);
  LTempTimeLog := LTimeLog;

  PTimeLogBits(@LTimeLog)^.From(1991, 12, 19, 19, 19, 19);
  LTempTimeLog := LTimeLog;

  PTimeLogBits(@LTimeLog)^.From(1991, 12, 19, 0, 0, 0);
  LTempTimeLog := LTimeLog;

  PTimeLogBits(@LTimeLog)^.From(0, 0, 0, 19, 19, 19);
  LTempTimeLog := LTimeLog;
end;

end.
