program Project1;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  system.Diagnostics,
  System.SysUtils;


function _LStrEqual(const Left, Right: AnsiString): Integer;
label
  BothStringsNonNil, FoundMismatch;
var
  LPtr, RPtr: PByte;
  i: NativeInt;
begin
  LPtr := Pointer(Left);
  RPtr := Pointer(Right);
  if LPtr <> RPtr then
  begin
    if NativeUInt(LPtr) and NativeUInt(RPtr) <> 0 then
    begin
BothStringsNonNil:
      Result := PInteger(LPtr - 4)^;
      if Result <> PInteger(RPtr - 4)^ then
        goto FoundMismatch;
      i := Result * -1;
      LPtr := LPtr - i;
      RPtr := RPtr - i;
      Dec(i, (4 - (Result and 3)) and 3);
      repeat
        if PInteger(@LPtr[i])^ <> PInteger(@RPtr[i])^ then
          goto FoundMismatch;
        Inc(i, 4);
      until i >= 0;
    end
    else if (LPtr <> nil) and (RPtr <> nil) then
      goto BothStringsNonNil
    else
FoundMismatch:
      Exit(-1);
  end;
  Result := 0;
end;

{********************************************************************************************}
function  ALRandomStrA(const aLength: Longint; const aCharset: Array of ansiChar): AnsiString; overload;
var X: Longint;
    P: Pansichar;
begin
  if aLength <= 0 then exit('');
  SetLength(Result, aLength);
  P := pansiChar(Result);
  for X:=1 to aLength do begin
    P^ := aCharset[Random(length(aCharset))];
    inc(P);
  end;
end;

{********************************************************}
function ALRandomStrA(const aLength: Longint): AnsiString; overload;
begin
  Result := ALRandomStrA(aLength,['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']);
end;

{************************************************************************************}
function  ALRandomStrW(const aLength: Longint; const aCharset: Array of Char): String; overload;
var X: Longint;
    P: Pchar;
begin
  if aLength <= 0 then exit('');
  SetLength(Result, aLength);
  P := pchar(Result);
  for X:=1 to aLength do begin
    P^ := aCharset[Random(length(aCharset))];
    inc(P);
  end;
end;

{****************************************************}
function ALRandomStrW(const aLength: Longint): String; overload;
begin
  Result := ALRandomStrW(aLength,['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']);
end;


begin
  try

    var SA1: ansiString := 'mlqskdqmslkdqsmldqsmldz';
    var SA2: ansiString := 'mlqskdqmslkdqsmldqsmldk';
    var SU1: String := 'mlqskdqmslkdqsmldqsmldz';
    var SU2: String := 'mlqskdqmslkdqsmldqsmldk';

    {$IF not defined(WIN64)}
    writeln('Please run in Win64 Release');
    writeln('');
    {$ENDIF}
    var LStopWatch := TstopWatch.StartNew;
    for var I := 1 to 100_000_000 do begin
      if SA1 = SA2 then
        raise Exception.Create('Error 25C51F74-654B-4CA1-B365-07289C7B7A09');
    end;
    LStopWatch.Stop;
    writeln('AnsiString string comparaison via = operator: ' + FloatToStr(LStopWatch.Elapsed.TotalMilliseconds) + ' ms');


    LStopWatch := TstopWatch.StartNew;
    for var I := 1 to 100_000_000 do begin
      if _LStrEqual(SA1, SA2) = 0 then
        raise Exception.Create('Error 25C51F74-654B-4CA1-B365-07289C7B7A09');
      //SA1:= ALRandomStrA(Random(64));
      //SA2 := ALRandomStrA(Random(64));
      //if _LStrEqual(SA1, SA2) = 0 then begin
      //  if SA1 <> SA2 then
      //    Raise Exception.Create('Error 1');
      //end
      //else begin
      //  if SA1 = SA2 then
      //    Raise Exception.Create('Error 2');
      //end;
    end;
    LStopWatch.Stop;
    writeln('AnsiString string comparaison via patched _LStrEqual: ' + FloatToStr(LStopWatch.Elapsed.TotalMilliseconds) + ' ms');

    LStopWatch := TstopWatch.StartNew;
    for var I := 1 to 100_000_000 do begin
      if SU1 = SU2 then
        raise Exception.Create('Error 25C51F74-654B-4CA1-B365-07289C7B7A09');
    end;
    LStopWatch.Stop;
    writeln('UnicodeString string comparaison via = operator: ' + FloatToStr(LStopWatch.Elapsed.TotalMilliseconds) + ' ms');
    writeln('AnsiString or UnicodeString comparaison via = operator must be equal in speed');

    readln;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
