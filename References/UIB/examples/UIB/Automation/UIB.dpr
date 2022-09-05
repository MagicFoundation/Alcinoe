library UIB;

uses
  ComServ,
  UIB_TLB in 'UIB_TLB.pas',
  database in 'database.pas' {Database: CoClass},
  transaction in 'transaction.pas' {Transaction: CoClass},
  InternalItf in 'InternalItf.pas',
  query in 'query.pas' {Query: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
