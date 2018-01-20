{{ in order to be able to use http.sys server for Project04Server.exe 
 under Vista or Seven, call first this program with Administrator rights
  - you can unregister it later with command line parameter /delete }
program Project04ServerRegister;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SynCrtSock,
  SysUtils;

const
  REGSTR: array[boolean] of string = (
    'Registration', 'Deletion');

{$R VistaAdm.res} // force elevation to Administrator under Vista/Seven

var delete: boolean;

procedure Call(const Root: SockString);
begin
  writeln(REGSTR[delete],' of http://+:8080/',root,'/ for http.sys');
  writeln(THttpApiServer.AddUrlAuthorize(root,'8080',false,'+',delete));
end;

begin
  // test if we have to un-register the url
  delete := (ParamCount=1) and SameText(ParamStr(1),'/DELETE');
  // perform url (un)registration for http.sys
  // (e.g. to be run as administrator under Windows Vista/Seven)
  Call('root');   // for the TSQLModel as defined in SampleData.pas
  Call('static'); // for Project04Static.dpr
  // we're done
  WriteLn('Done - Press ENTER to Exit');
  ReadLn;
end.
 