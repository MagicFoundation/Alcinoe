program ALFmxControls;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMain in 'UnitMain.pas' {Form1},
  UnitDemo in 'UnitDemo.pas' {DemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDemoForm, DemoForm);
  Application.Run;
end.
