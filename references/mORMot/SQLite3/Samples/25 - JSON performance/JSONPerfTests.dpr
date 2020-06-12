/// benchmarking of JSON process: mORMot vs SuperObject/dwsJSON/DBXJSON
program JSONPerfTests;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc}
  SynCommons,
  SynCrossPlatformJSON in '..\..\..\CrossPlatform\SynCrossPlatformJSON.pas',
  JSONPerfTestCases;

begin
  with TTestJSONBenchmarking.Create do
  try
    Run;
    readln;
  finally
    Free;
  end;
end.
