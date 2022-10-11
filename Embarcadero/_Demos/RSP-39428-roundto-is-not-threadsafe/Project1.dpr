program Project1;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Classes,
  System.Math,
  System.SysUtils;

begin
  try

    Writeln('This app must be compiled in WIN64 and must failed at startup.');
    Writeln('if not failed immediatly after startup, then stop it and launch it again');

    SetRoundMode(rmTruncate);
    var LformatSettings := TFormatSettings.Invariant;

    TThread.CreateAnonymousThread(
      procedure
      begin
        while true do begin
          Var F1 := StrToFloat('1.5988952565', LformatSettings);
          var F2 := StrToFloat('1.5988952565', LformatSettings);
          if not sameValue(F1, F2) then
            raise Exception.Create('Error');
        end;
      end).Start;

    for var I := 0 to 5 do
      TThread.CreateAnonymousThread(
        procedure
        begin
         While true do begin
           var LRound1 := RoundTo(-1.51966971252744830, 0);
           var LRound2 := RoundTo(-1.51966971252744830, 0);
           if not SameValue(LRound1, LRound2) then begin
             Writeln(Format('Failed : %0.18f %0.18f', [LRound1, LRound2]));
             raise Exception.Createfmt('Failed : %0.18f %0.18f', [LRound1, LRound2]);
           end;
         end;
       end).Start;

     sleep(maxint);

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
