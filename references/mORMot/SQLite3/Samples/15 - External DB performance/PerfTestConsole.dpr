program PerfTestConsole;

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

uses
  {$I SynDprUses.inc}
  SynCommons,
  SynLog,
  mORMot,
  PerfTestCases in '.\PerfTestCases.pas';

begin
  TTestDatabaseBenchmark.RunAsConsole(
    'mORMot Framework Database Benchmark'{, LOG_VERBOSE});
end.
