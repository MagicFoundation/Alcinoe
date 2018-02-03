program WideStringExample;

uses
  Forms,
  WideStringDemoMain in 'WideStringDemoMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'TWideStringList Example (JclUnicode)';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
