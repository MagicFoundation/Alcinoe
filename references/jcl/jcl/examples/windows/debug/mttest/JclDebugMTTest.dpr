program JclDebugMTTest;

uses
  Forms,
  JclDebugMTTestMain in 'JclDebugMTTestMain.pas' {MTTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMTTestForm, MTTestForm);
  Application.Run;
end.
