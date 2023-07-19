program Project1;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  system.Diagnostics,
  System.SysUtils;

begin
  try

    {$IF not defined(WIN64)}
    writeln('Please run in Win64 Release');
    writeln('');
    {$ENDIF}
    var SA1: ansiString := 'qsmldkqsdpoqsdkpqsokdqspodkjqsi';
    var SA2: ansiString := 'mlqskdqmslkdqsmldqsmldk';

    var LStopWatch := TstopWatch.StartNew;
    for var I := 1 to 1000000000 do
      if SA1 = SA2 then
        raise Exception.Create('Error FCE2EBFC-C72F-488A-A919-A6BB12C91DA1');
    LStopWatch.Stop;
    writeln('AnsiString string comparaison: ' + FloatToStr(LStopWatch.Elapsed.TotalMilliseconds) + ' ms');

    var SU1: String := 'qsmldkqsdpoqsdkpqsokdqspodkjqsi';
    var SU2: String := 'mlqskdqmslkdqsmldqsmldk';
    LStopWatch := TstopWatch.StartNew;
    for var I := 1 to 1000000000 do
      if SU1 = SU2 then
        raise Exception.Create('Error FCE2EBFC-C72F-488A-A919-A6BB12C91DA1');
    LStopWatch.Stop;
    writeln('UnicodeString string comparaison: ' + FloatToStr(LStopWatch.Elapsed.TotalMilliseconds) + ' ms');

    writeln('AnsiString or UnicodeString comparaison must be equal in speed');
    readln;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
