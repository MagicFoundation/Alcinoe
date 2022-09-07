unit Unit1;

interface

uses
  System.SysUtils,
  FMX.Forms,
  FMX.StdCtrls,
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FKey: integer;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin

  FKey := 2;
  var LCompareKey: integer := 2;
  AtomicCmpExchange(FKey{target}, LCompareKey{NewValue}, LCompareKey{Comparand});
  if FKey <> LCompareKey then raise Exception.Create('Error 2');

  TThread.queue(nil,
    procedure
    begin
      if LCompareKey <> FKey
        then raise Exception.Create('Error 3');
    end);

end;

end.
