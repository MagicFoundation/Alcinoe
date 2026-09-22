program Test_002;

uses
  SysUtils,
  StringBuilderUnit;

const
  TestString = 'TestString';
  CountOfTestString = 1013;
  CountOfTests = 10000;

procedure TestWithConcat;
var
  time: TDateTime;
  testIndex, i: Cardinal;
  s: string;
begin
  WriteLN('Now testing concat...');
  time := Now;
  for testIndex := 1 to CountOfTests do
  begin
    s := '';
    for i := 1 to CountOfTestString do
      s := s + TestString;
  end;
  time := Now - time;
  WriteLN(FormatDateTime('hh:nn:ss.zzz', time));
end;

{ $Define EnableIntegrityCheck}

procedure TestWithBuilder;
var
  time: TDateTime;
  testIndex, i: Cardinal;
  resultValid, allValid: Boolean;
  builder: TStringBuilder;
  s: string;
begin
  WriteLN('Now testing concat...');
  time := Now;
  allValid := True;
  for testIndex := 1 to CountOfTests do
  begin
    builder := TStringBuilder.Create;
    for i := 1 to CountOfTestString do
      builder.Add(TestString);
    s := builder.ToString;
    builder.Free;
    // integrity check below:
    {$IfDef EnableIntegrityCheck}
    resultValid := True;
    for i := 1 to Length(s) do
      if s[i] <> TestString[(i - 1) mod Length(TestString) + 1] then
        resultValid := False;
    allValid := allValid and resultValid;
    {$EndIf}
  end;
  time := Now - time;
  {$IfDef EnableIntegrityCheck}
  WriteLN('All strings are valid: ', allValid);
  {$EndIf}
  WriteLN(FormatDateTime('hh:nn:ss.zzz', time));
end;

begin
  TestWithConcat;
  TestWithBuilder;
end.

