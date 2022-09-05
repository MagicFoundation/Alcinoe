program Security;

uses
  {$IFDEF LINUX} QForms,{$ELSE} Forms,{$ENDIF}
  main in 'main.pas' {MainForm},
  user in 'user.pas' {UserForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TUserForm, UserForm);
  Application.Run;
end.
