program FileMapping;

uses
  Forms,
  MapTestForm in 'MapTestForm.pas' {TestForm},
  MapWriteForm in 'MapWriteForm.pas' {WriteForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTestForm, TestForm);
  Application.Run;
end.
