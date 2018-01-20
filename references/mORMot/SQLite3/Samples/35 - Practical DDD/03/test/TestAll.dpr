program TestAll;

uses
  {$I SynDprUses.inc} // cross-platform, cross-compiler (includes FastMM4)
  SynCommons,         // shared types for the whole framework
  SynLog,             // logging
  SynTests,           // unitary tests
  mORMot,             // ORM + SOA
  SynSQLite3Static,   // statically linked SQLite3 engine
  mORMotSQLite3,      // use SQlite3 as ORM core
  // domain
  DomConferenceTypes in '..\dom\DomConferenceTypes.pas',
  DomConferenceInterfaces in '..\dom\DomConferenceInterfaces.pas',
  DomConferenceDepend in '..\dom\DomConferenceDepend.pas',
  DomConferenceServices in '..\dom\DomConferenceServices.pas',
  DomConferenceTest in '..\dom\DomConferenceTest.pas',
  // infrastructure
  InfraConferenceRepository in '..\infra\InfraConferenceRepository.pas',
  // servers
  ServBookMain in '..\serv\ServBookMain.pas',
  ServBookTest in '..\serv\ServBookTest.pas',
  // tests
  TestAllMain in 'TestAllMain.pas';

begin
  TSynLogTestLog := TSQLLog; // share the same log file with the whole mORMot
  TTestEKON.RunAsConsole('EKON Automated Tests', LOG_VERBOSE);
end.
