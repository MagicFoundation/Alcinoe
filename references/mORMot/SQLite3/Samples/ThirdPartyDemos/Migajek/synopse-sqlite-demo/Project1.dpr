// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE ON
// JCL_DEBUG_EXPERT_INSERTJDBG OFF

program Project1;

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

//   first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  Forms,
  Controls,
  Unit1 in 'Unit1.pas' {Form1},
  uCustomer in 'uCustomer.pas',
  uQueryHistory in 'uQueryHistory.pas',
  uLoginForm in 'uLoginForm.pas' {LoginForm};

const
  AppName = 'Synopse mORMot Demo';

{$R *.res}
begin
  Application.Initialize;
  // init the database client
  InitClient();
  LoginForm:= TLoginForm.Create(Application);

  // when no users defined, just let in ...
  if globalClient.TableRowCount(TSQLUser) <= 0 then
    begin
      TSQLUserRole.CreateStandardRoles(globalClient);
      LoginForm.LoginOk:= true
    end
  else
    begin
      LoginForm.Caption:= AppName + ' - ' + LoginForm.Caption;
      LoginForm.ShowModal;
    end;
    
  if LoginForm.LoginOk then
    begin
      Application.CreateForm(TForm1, Form1);
      Form1.Caption:= AppName;
      Form1.Label4.Caption:= AppName;
    end;
  Application.Run;
end.
