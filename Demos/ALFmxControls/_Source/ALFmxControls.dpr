program ALFmxControls;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMain in 'UnitMain.pas' {Form1},
  UnitScrollBoxDemo in 'UnitScrollBoxDemo.pas' {ScrollBoxDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TScrollBoxDemoForm, ScrollBoxDemoForm);
  Application.Run;
end.
