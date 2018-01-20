program ECC;

{$APPTYPE CONSOLE}

(*

  Synopse mORMot framework

  Sample 33 - ECC command line tool
    Manage certificate-based public-key cryptography using ECC-secp256r1,
    i.e. public/private key pairs, ECDSA digital signatures and ECIES encryption,
    as implemented in SynECC.


  Version 1.18
  - Initial Release


  first line of uses clause below must be {$I SynDprUses.inc} to enable FastMM4

*)

uses
  {$I SynDprUses.inc}
  SysUtils,
  SynCommons,
  SynCrypto,
  SynEcc,
  ECCProcess;

{$R *.res}

function ProcessCommandLine: TECCCommandError;
var cmd: RawUTF8;
    main: TECCCommand;
    sw: ICommandLine;
begin
  cmd := StringToUTF8(ParamStr(1));
  main := TECCCommand(GetEnumNameValueTrimmed(TypeInfo(TECCCommand),pointer(cmd),length(cmd)));
  if main=ecChain then
    sw := TCommandLine.CreateAsArray(2) else
    sw := TCommandLine.Create;
  result := ECCCommand(main,sw);
  if result=eccUnknownCommand then begin
    TextColor(ccLightGreen);
    writeln(#13#10'Synopse ECC certificate-based public-key cryptography'+
            #13#10'-----------------------------------------------------');
    TextColor(ccGreen);
    writeln('Using mORMot''s SynECC rev. ' + {$I SynopseCommit.inc} + #13#10);
    TextColor(ccLightGray);
    writeln(ExeVersion.ProgramName,
      ' help');
    writeln(ExeVersion.ProgramName,
      ' new -auth key.private -authpass authP@ssW0rd -authrounds 60000'#13#10+
      '      -issuer toto@toto.com -start 2016-10-30 -days 30'#13#10+
      '      -newpass P@ssw0RD@ -newrounds 60000'); // -splitfiles 1');
    writeln(ExeVersion.ProgramName,
      ' rekey -auth key.private -authpass P@ssW0rd -authrounds 60000'#13#10+
      '      -newpass newP@ssw0RD@ -newrounds 60000');
    writeln(ExeVersion.ProgramName,
      ' sign -file some.doc -auth key.private -pass P@ssW0rd -rounds 60000');
    writeln(ExeVersion.ProgramName,
      ' verify -file some.doc -auth key.public');
    writeln(ExeVersion.ProgramName,
      ' source -auth key.private -pass P@ssW0rd -rounds 60000'#13#10+
      '      -const MY_PRIVKEY -comment "My Private Key"');
    writeln(ExeVersion.ProgramName,
      ' infopriv -auth key.private -pass P@ssW0rd -rounds 60000 [-json key.json]');
    writeln(ExeVersion.ProgramName,
      ' chain file1.public file2.public file3.public ...');
    writeln(ExeVersion.ProgramName,
      ' chainall');
    writeln(ExeVersion.ProgramName,
      ' crypt -file some.doc -out some.doc.synecc -auth key.public'#13#10+
      '      -saltpass salt -saltrounds 60000 [-algo 0]');
    writeln(ExeVersion.ProgramName,
      ' decrypt -file some.doc.synecc -out some.doc -auth key.private'#13#10+
      '      -authpass P@ssW0rd -authrounds 60000 -saltpass salt -saltrounds 60000');
    writeln(ExeVersion.ProgramName,
      ' infocrypt -file some.doc.synecc [-rawfile some.raw][-json some.json]');
    writeln(ExeVersion.ProgramName,
      ' cheatinit -newpass MasterP@ssw0RD@ -newrounds 100000');
    writeln(ExeVersion.ProgramName,
      ' cheat -auth key.private -authpass MasterP@ssw0RD@ -authrounds 100000');
    writeln(#10'Note that you can add the -noprompt switch for no console interactivity.');
  end;
end;

begin
  ExitCode := ord(ProcessCommandLine);
  {$ifndef FPC}
  {$WARNINGS OFF}
  {$ifdef MSWINDOWS}
  if DebugHook<>0 then
    readln;
  {$endif}
  {$WARNINGS ON}
  {$endif}
end.
