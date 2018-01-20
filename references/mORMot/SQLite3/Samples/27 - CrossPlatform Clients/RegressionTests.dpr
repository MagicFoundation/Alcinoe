program RegressionTests;

{$i SynCrossPlatform.inc} // define e.g. HASINLINE

{$APPTYPE CONSOLE}

{$ifdef MSWINDOWS}
{$ifndef FPC}  // under FPC, please run program RegressionTestsServer.dpr
  {$define RUNSERVER}
{$endif}
{$endif}

{$ifdef ISDELPHI5OROLDER}
  {$undef RUNSERVER} // mORMot.pas not available prior to Delphi 6
{$endif}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SynCrossPlatformJSON,
  SynCrossPlatformSpecific,
  SynCrossPlatformREST,
  SynCrossPlatformCrypto,
  SynCrossPlatformTests,
  {$ifdef RUNSERVER}
  PeopleServer,
  {$endif}
  SysUtils;

var
  TotalFailed: cardinal = 0;

procedure TestWithAuth(aAuth: SynCrossPlatformREST.TSQLRestServerAuthenticationClass);
begin
  with TSynCrossPlatformClient.Create(aAuth) do
  try
    Ident := 'Cross Platform Client for mORMot';
    if aAuth=nil then
      Ident := Ident+' without authentication' else
      Ident := Ident+' using '+string(aAuth.ClassName);
{$ifdef RUNSERVER}
    try
      if aAuth=TSQLRestServerAuthenticationDefault then
        StartServer(psaDefault) else
      if aAuth=TSQLRestServerAuthenticationNone then
        StartServer(psaWeak) else
        StartServer(psaNone);
{$endif}
      Run(true);
      inc(TotalFailed,Failed);
{$ifdef RUNSERVER}
    finally
      StopServer;
    end;
{$endif}
  finally
    Free;
  end;
end;

begin
  with TSynCrossPlatformTests.Create('Cross Platform Units for mORMot') do
  try
    Run(true);
  finally
    Free;
  end;
  writeln;
  {$ifdef RUNSERVER} // only last one should be tested for server-less FPC
  TestWithAuth(nil);
  TestWithAuth(TSQLRestServerAuthenticationNone);
  {$endif}
  TestWithAuth(TSQLRestServerAuthenticationDefault);
  if TotalFailed>0 then
    writeln(#10'Some tests failed... please fix it ASAP!');
  write(#10'Press [Enter] to quit');
  readln;
end.

