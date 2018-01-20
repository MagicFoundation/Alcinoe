program WindowsAuth;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc}
  SysUtils,
  SynLZ in 'SynLZ.pas',
  SynCommons in 'SynCommons.pas',
  SynSSPIAuth in 'SynSSPIAuth.pas';

procedure Test;
var
  ClientSecContext: TSecContext;
  ServerSecContext: TSecContext;
  InData, OutData: RawByteString;
  UserName: RawUTF8;
begin
  try
    InvalidateSecContext(ClientSecContext, 0);
    InvalidateSecContext(ServerSecContext, 0);

    ClientSSPIAuth(ClientSecContext, InData, '', OutData);

    InData := OutData;

    ServerSSPIAuth(ServerSecContext, InData, OutData);

    InData := OutData;

    ClientSSPIAuth(ClientSecContext, InData, '', OutData);

    InData := OutData;

    ServerSSPIAuth(ServerSecContext, InData, OutData);
    ServerSSPIAuthUser(ServerSecContext, UserName);

    FreeSecContext(ClientSecContext);
    FreeSecContext(ServerSecContext);

    Writeln('Authentified as: "',UserName,'"');
    writeln('Press [Enter] to continue');
    readln;
  except
    on E: Exception do
      ConsoleShowFatalException(E);
  end;
end;

begin
  Test;
end.
