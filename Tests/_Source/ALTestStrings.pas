unit ALTestStrings;

interface

uses
  System.SysUtils,
  System.AnsiStrings,
  System.Diagnostics,
  Winapi.Windows,
  Alcinoe.Common,
  Alcinoe.Cipher,
  Alcinoe.StringUtils,
  DUnitX.TestFramework;

type

  [TestFixture]
  TALTestStrings = class
  strict private
    fFullAsciiCharsetA: TArray<AnsiChar>;
    fFullAsciiCharsetW: TArray<Char>;
    FValidLCID: TArray<LCID>;
    fStopWatchAlcinoe: TStopwatch;
    fStopWatchDELPHI: TStopwatch;
    procedure CheckExecutionTime(const ARatio: single = 1.2);
    function UnicodeRandomStr(const aLength: Longint): String;
  public
    constructor create;
    [Setup]
    procedure Setup;
    //[TearDown]
    //procedure TearDown;
    [Test]
    procedure TestALBase64EncodeStringA;
    [Test]
    procedure TestAlposIgnoreCaseA;
    [Test]
    procedure TestAlposIgnoreCaseW;
    [Test]
    procedure TestALTryStrToDateA;
    [Test]
    procedure TestALTryStrToTimeA;
    [Test]
    procedure TestALTryStrToDateTimeA;
    [Test]
    procedure TestALDateToStrA;
    [Test]
    procedure TestALTimeToStrA;
    [Test]
    procedure TestALDateTimeToStrA;
    [Test]
    procedure TestALFormatDateTimeA;
    [Test]
    procedure TestALIntToStrA;
    [Test]
    procedure TestALStrToIntA;
    [Test]
    procedure TestALStrToUIntA;
    [Test]
    procedure TestALStrToInt64A;
    [Test]
    procedure TestALStrToUInt64A;
    [Test]
    procedure TestALFloatToStrA;
    [Test]
    procedure TestALFloatToStrFA;
    [Test]
    procedure TestALStrToFloatA;
    [Test]
    procedure TestALCurrToStrA;
    [Test]
    procedure TestALStrToCurrA;
    [Test]
    procedure TestALFormatFloatA;
    [Test]
    procedure TestALFormatCurrA;
    [Test]
    procedure TestALFormatA;
    [Test]
    procedure TestALUTF8CharSize;
    [Test]
    procedure TestALUTF8CharToUtf16;
  end;

implementation

uses
  System.NetEncoding,
  System.Math,
  System.Classes,
  System.DateUtils,
  System.Character,
  System.StrUtils,
  Alcinoe.StringList;

{********************************}
constructor TALTestStrings.create;
begin
  Setlength(fFullAsciiCharsetA, 255);
  for var I := Low(fFullAsciiCharsetA) to High(fFullAsciiCharsetA) do
    fFullAsciiCharsetA[i] := AnsiChar(i); // will contain some invalid UTF-8 chars
  //--
  Setlength(fFullAsciiCharsetW, 65535);
  for var I := Low(fFullAsciiCharsetW) to High(fFullAsciiCharsetW) do
    fFullAsciiCharsetW[i] := Char(i); // will contain some invalid UTF-16 chars
  //--
  setlength(FValidLCID, 0);
  For var i := 0 to 1000000 do
    if IsValidLocale(I, LCID_INSTALLED) then begin
      setlength(FValidLCID, length(FValidLCID) + 1);
      FValidLCID[high(FValidLCID)] := i;
    end;
end;

{*****************************}
procedure TALTestStrings.Setup;
begin
  fStopWatchAlcinoe := TStopwatch.Create;
  fStopWatchDELPHI := TStopwatch.Create;
end;

{**********************************}
//procedure TALTestStrings.TearDown;
//begin
//end;

{**********************************************************************}
procedure TALTestStrings.CheckExecutionTime(const ARatio: single = 1.2);
begin
  {$IF defined(debug) or defined(Win32)}
  //In debug we have overflow checking and range checking so that mean
  //that the execution time will be much slower that the Delphi RTL so skip it
  //In Win32 we remove all ASM (fastcode heritage) than the delphi RTL have
  //so we will be othen much more slower than the Delphi RTL
  Writeln(ALFormatw('CheckExecutionTime Skipped - %0.0f ms for Alcinoe vs %0.0f ms for Delphi (%0.1fx faster)', [fStopWatchAlcinoe.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds / fStopWatchAlcinoe.Elapsed.TotalMilliseconds], ALDefaultFormatSettingsW));
  {$ELSE}
  if fStopWatchAlcinoe.Elapsed.TotalMilliseconds > fStopWatchDELPHI.Elapsed.TotalMilliseconds * ARatio then
    Assert.Fail(ALFormatW('Time too long (%0.0f ms for Alcinoe vs %0.0f ms for Delphi)', [fStopWatchAlcinoe.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds], ALDefaultFormatSettingsW))
  else
    //https://github.com/VSoftTechnologies/DUnitX/issues/319
    Writeln(ALFormatW('%0.0f ms for Alcinoe vs %0.0f ms for Delphi (%0.1fx faster)', [fStopWatchAlcinoe.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds / fStopWatchAlcinoe.Elapsed.TotalMilliseconds], ALDefaultFormatSettingsW));
  {$ENDIF}
end;

{***********************************************************************}
function TALTestStrings.UnicodeRandomStr(const aLength: Longint): String;
begin
  result := '';
  for var i := 0 to aLength - 1 do begin
   //In the version 6.0, Unicode has 1,114,112 code points (U+0000—U+10FFFF)
   //the last code point is U+10FFFF (1,114,111)
   var LUCS4Char := ALRandom32(1114111);
   while ((LUCS4Char >= UCS4Char(Char.MinHighSurrogate)) and (LUCS4Char <= UCS4Char(Char.MaxLowSurrogate))) do
     LUCS4Char := ALRandom32(1114111);
   Result := Result + char.ConvertFromUtf32(LUCS4Char);
  end;
end;

{*************************************************}
procedure TALTestStrings.TestALBase64EncodeStringA;
begin
  for var I := 0 to 10000 do begin
    var LStrIn := ALRandomStrA(ALRandom32(16384), fFullAsciiCharsetA);
    var LEncoding := TBase64Encoding.Create(0);
    Try
      fStopWatchDELPHI.Start; {-} var LDelphiStrOut := AnsiString(LEncoding.EncodeBytesToString(BytesOf(LStrIn))); {-} fStopWatchDELPHI.Stop;
      fStopWatchAlcinoe.Start; {-} var LAlcinoeStrOut := ALBase64EncodeString(LStrIn); {-} fStopWatchAlcinoe.Stop;
      if ALBase64DecodeString(LAlcinoeStrOut) <> LStrIn then
        Assert.Fail;
      if LAlcinoeStrOut <> LDelphiStrOut then
        Assert.Fail;
    finally
      ALFreeAndNil(LEncoding);
    End;
  end;
  //--
  CheckExecutionTime(0.4{ARatio});
end;

{********************************************}
procedure TALTestStrings.TestAlposIgnoreCaseA;
begin
  //full random substr
  for var I := 0 to 100000 do begin
    var LStr := ALRandomStrA(ALRandom32(1024), fFullAsciiCharsetA);
    var LSubStr := ALRandomStrA(ALRandom32(1024), fFullAsciiCharsetA);
    var LOffset := ALrandom32(1024);
    fStopWatchDELPHI.Start; {-} var LDelphiPos := Pos(Uppercase(LSubStr), Uppercase(LStr), LOffset); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoePos := AlposIgnoreCaseA(LSubStr, LStr, LOffset); {-} fStopWatchAlcinoe.Stop;
    if LAlcinoePos <> LDelphiPos then Assert.Fail;
  end;
  //substr inside Str
  for var I := 0 to 100000 do begin
    var LStr := ALRandomStrA(ALRandom32(1024), fFullAsciiCharsetA);
    var LSubStr := AlCopyStr(Lstr, ALRandom32(1024), ALRandom32(1024));
    case ALRandom32(2) of
      0: LSubStr := ALLowerCase(LSubStr);
      1: LSubStr := ALUpperCase(LSubStr);
    end;
    var LOffset := ALrandom32(1024);
    fStopWatchDELPHI.Start; {-} var LDelphiPos := Pos(Uppercase(LSubStr), Uppercase(LStr), LOffset); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoePos := AlposIgnoreCaseA(LSubStr, LStr, LOffset); {-} fStopWatchAlcinoe.Stop;
    if LAlcinoePos <> LDelphiPos then Assert.Fail;
  end;
  //--
  CheckExecutionTime(0.4{ARatio});
end;

{********************************************}
procedure TALTestStrings.TestAlposIgnoreCaseW;
begin
  //full random substr
  for var I := 0 to 100000 do begin
    var LStr := ALRandomStrW(ALRandom32(1024), fFullAsciiCharsetW);
    var LSubStr := ALRandomStrW(ALRandom32(1024), fFullAsciiCharsetW);
    var LOffset := ALrandom32(1024);
    fStopWatchDELPHI.Start; {-} var LDelphiPos := Pos(Uppercase(LSubStr), Uppercase(LStr), LOffset); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoePos := AlposIgnoreCaseW(LSubStr, LStr, LOffset); {-} fStopWatchAlcinoe.Stop;
    if LAlcinoePos <> LDelphiPos then Assert.Fail;
  end;
  //substr inside Str
  for var I := 0 to 100000 do begin
    var LStr := ALRandomStrW(ALRandom32(1024), fFullAsciiCharsetW);
    var LSubStr := AlCopyStr(Lstr, ALRandom32(1024), ALRandom32(1024));
    case ALRandom32(2) of
      0: LSubStr := ALLowerCase(LSubStr);
      1: LSubStr := ALUpperCase(LSubStr);
    end;
    var LOffset := ALrandom32(1024);
    fStopWatchDELPHI.Start; {-} var LDelphiPos := Pos(Uppercase(LSubStr), Uppercase(LStr), LOffset); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoePos := AlposIgnoreCaseW(LSubStr, LStr, LOffset); {-} fStopWatchAlcinoe.Stop;
    if LAlcinoePos <> LDelphiPos then Assert.Fail;
  end;
  //--
  CheckExecutionTime(0.4{ARatio});
end;

{*******************************************}
procedure TALTestStrings.TestALTryStrToDateA;
begin
  for var I := 0 to 100000 do begin

    var LDate: TdateTime;
    While not TryEncodeDateTime(
                Word(ALRandom32(9998) + 1), //const AYear,
                Word(ALrandom32(12) + 1), // AMonth,
                Word(ALRandom32(31) + 1), //ADay,
                Word(ALRandom32(23)), // AHour,
                Word(ALRandom32(60)), // AMinute,
                Word(ALRandom32(60)), // ASecond,
                Word(ALRandom32(999)), // AMilliSecond: Word
                LDate) do continue;

    Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
    {$WARN SYMBOL_PLATFORM OFF}
    Var LDelphiFormatSettings := TformatSettings.Create(LCID);
    Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
    {$WARN SYMBOL_PLATFORM ON}
    if ALRandom32(2) = 1 then begin
      LDelphiFormatSettings.ShortDateFormat := LDelphiFormatSettings.LongDateFormat;
      LDelphiFormatSettings.ShortTimeFormat := LDelphiFormatSettings.LongTimeFormat;
      LAlcinoeFormatSettings.ShortDateFormat := LAlcinoeFormatSettings.LongDateFormat;
      LAlcinoeFormatSettings.ShortTimeFormat := LAlcinoeFormatSettings.LongTimeFormat;
    end;
    var LDateStrU := DateToStr(LDate, LDelphiFormatSettings);
    var LDatestr := AnsiString(LDateStrU);
    var LAlcinoeDate: TdateTime;
    var LDelphiDate: TdateTime;

    fStopWatchDELPHI.Start; {-} var LDelphiResult := tryStrToDate(LDateStrU, LDelphiDate, LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoeResult := ALTryStrToDate(LDateStr, LAlcinoeDate, LAlcinoeFormatSettings); {-} fStopWatchAlcinoe.Stop;

    if LAlcinoeResult <> LDelphiResult then Assert.Fail;
    if LAlcinoeResult and (not SameDate(LAlcinoeDate, LDelphiDate)) then Assert.Fail;

  end;
  //--
  CheckExecutionTime;
end;

{*******************************************}
procedure TALTestStrings.TestALTryStrToTimeA;
begin
  for var I := 0 to 100000 do begin

    var LTime: TDateTime;
    While not TryEncodeDateTime(
                Word(ALRandom32(9998) + 1), //const AYear,
                Word(ALrandom32(12) + 1), // AMonth,
                Word(ALRandom32(31) + 1), //ADay,
                Word(ALRandom32(23)), // AHour,
                Word(ALRandom32(60)), // AMinute,
                Word(ALRandom32(60)), // ASecond,
                Word(ALRandom32(999)), // AMilliSecond: Word
                LTime) do continue;

    Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
    {$WARN SYMBOL_PLATFORM OFF}
    Var LDelphiFormatSettings := TformatSettings.Create(LCID);
    Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
    {$WARN SYMBOL_PLATFORM ON}
    if ALRandom32(2) = 1 then begin
      LDelphiFormatSettings.ShortDateFormat := LDelphiFormatSettings.LongDateFormat;
      LDelphiFormatSettings.ShortTimeFormat := LDelphiFormatSettings.LongTimeFormat;
      LAlcinoeFormatSettings.ShortDateFormat := LAlcinoeFormatSettings.LongDateFormat;
      LAlcinoeFormatSettings.ShortTimeFormat := LAlcinoeFormatSettings.LongTimeFormat;
    end;
    var LTimeStrU := TimeToStr(LTime, LDelphiFormatSettings);
    var LTimestr := AnsiString(LTimeStrU);
    var LAlcinoeTime: TDateTime;
    var LDelphiTime: TDateTime;

    fStopWatchDELPHI.Start; {-} var LDelphiResult := tryStrToTime(LTimeStrU, LDelphiTime, LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoeResult := ALTryStrToTime(LTimeStr, LAlcinoeTime, LAlcinoeFormatSettings); {-} fStopWatchAlcinoe.Stop;

    if LAlcinoeResult <> LDelphiResult then Assert.Fail;
    if LAlcinoeResult and (not SameTime(LAlcinoeTime, LDelphiTime)) then Assert.Fail;

  end;
  //--
  CheckExecutionTime(0.8{ARatio});
end;

{***********************************************}
procedure TALTestStrings.TestALTryStrToDateTimeA;
begin
  for var I := 0 to 100000 do begin

    var LDateTime: TdateTime;
    While not TryEncodeDateTime(
                Word(ALRandom32(9998) + 1), //const AYear,
                Word(ALrandom32(12) + 1), // AMonth,
                Word(ALRandom32(31) + 1), //ADay,
                Word(ALRandom32(23)), // AHour,
                Word(ALRandom32(60)), // AMinute,
                Word(ALRandom32(60)), // ASecond,
                Word(ALRandom32(999)), // AMilliSecond: Word
                LDateTime) do continue;

    Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
    {$WARN SYMBOL_PLATFORM OFF}
    Var LDelphiFormatSettings := TformatSettings.Create(LCID);
    Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
    {$WARN SYMBOL_PLATFORM ON}
    if ALRandom32(2) = 1 then begin
      LDelphiFormatSettings.ShortDateFormat := LDelphiFormatSettings.LongDateFormat;
      LDelphiFormatSettings.ShortTimeFormat := LDelphiFormatSettings.LongTimeFormat;
      LAlcinoeFormatSettings.ShortDateFormat := LAlcinoeFormatSettings.LongDateFormat;
      LAlcinoeFormatSettings.ShortTimeFormat := LAlcinoeFormatSettings.LongTimeFormat;
    end;
    var LDateTimeStrU := DateTimeToStr(LDateTime, LDelphiFormatSettings);
    var LDateTimestr := AnsiString(LDateTimeStrU);
    var LAlcinoeDatetime: TdateTime;
    var LDelphiDatetime: TdateTime;

    fStopWatchDELPHI.Start; {-} var LDelphiResult := tryStrToDateTime(LDateTimeStrU, LDelphiDatetime, LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoeResult := ALTryStrToDateTime(LDateTimeStr, LAlcinoeDatetime, LAlcinoeFormatSettings); {-} fStopWatchAlcinoe.Stop;

    if LAlcinoeResult <> LDelphiResult then Assert.Fail;
    if LAlcinoeResult and (not SameDateTime(LAlcinoeDatetime, LDelphiDatetime)) then Assert.Fail;

  end;
  //--
  CheckExecutionTime(0.8{ARatio});
end;

{****************************************}
procedure TALTestStrings.TestALDateToStrA;
begin
  for var I := 0 to 100000 do begin

    var LDate: TdateTime;
    While not TryEncodeDateTime(
                Word(ALRandom32(9998) + 1), //const AYear,
                Word(ALrandom32(12) + 1), // AMonth,
                Word(ALRandom32(31) + 1), //ADay,
                Word(ALRandom32(23)), // AHour,
                Word(ALRandom32(60)), // AMinute,
                Word(ALRandom32(60)), // ASecond,
                Word(ALRandom32(999)), // AMilliSecond: Word
                LDate) do continue;

    Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
    {$WARN SYMBOL_PLATFORM OFF}
    Var LDelphiFormatSettings := TformatSettings.Create(LCID);
    Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
    {$WARN SYMBOL_PLATFORM ON}
    if ALRandom32(2) = 1 then begin
      LDelphiFormatSettings.ShortDateFormat := LDelphiFormatSettings.LongDateFormat;
      LDelphiFormatSettings.ShortTimeFormat := LDelphiFormatSettings.LongTimeFormat;
      LAlcinoeFormatSettings.ShortDateFormat := LAlcinoeFormatSettings.LongDateFormat;
      LAlcinoeFormatSettings.ShortTimeFormat := LAlcinoeFormatSettings.LongTimeFormat;
    end;
    case ALRandom32(3) of
      0: begin
        LDelphiFormatSettings.ShortDateFormat := LDelphiFormatSettings.LongDateFormat.ToLower;
        LDelphiFormatSettings.ShortTimeFormat := LDelphiFormatSettings.LongTimeFormat.ToLower;
        LAlcinoeFormatSettings.ShortDateFormat := ansiString(LDelphiFormatSettings.ShortDateFormat);
        LAlcinoeFormatSettings.ShortTimeFormat := ansiString(LDelphiFormatSettings.ShortTimeFormat);
      end;
      1: begin
        LDelphiFormatSettings.ShortDateFormat := LDelphiFormatSettings.LongDateFormat.Toupper;
        LDelphiFormatSettings.ShortTimeFormat := LDelphiFormatSettings.LongTimeFormat.Toupper;
        LAlcinoeFormatSettings.ShortDateFormat := ansiString(LDelphiFormatSettings.ShortDateFormat);
        LAlcinoeFormatSettings.ShortTimeFormat := ansiString(LDelphiFormatSettings.ShortTimeFormat);
      end;
      2:;
      else raise Exception.Create('Error 96C64CFA-DCCE-463C-828B-077F21659C6B');
    end;

    var LDelphiDateStr: String;
    var LAlcinoeDatestr: ansiString;
    fStopWatchDELPHI.Start; {-} LDelphiDateStr := DateToStr(LDate, LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} LAlcinoeDatestr := alDateToStrA(LDate, LAlcinoeFormatSettings); {-} fStopWatchAlcinoe.Stop;

    if LAlcinoeDatestr <> AnsiString(LDelphiDateStr) then Assert.Fail;

  end;
  //--
  CheckExecutionTime;
end;

{****************************************}
procedure TALTestStrings.TestALTimeToStrA;
begin
  for var I := 0 to 100000 do begin

    var LTime: TDateTime;
    While not TryEncodeDateTime(
                Word(ALRandom32(9998) + 1), //const AYear,
                Word(ALrandom32(12) + 1), // AMonth,
                Word(ALRandom32(31) + 1), //ADay,
                Word(ALRandom32(23)), // AHour,
                Word(ALRandom32(60)), // AMinute,
                Word(ALRandom32(60)), // ASecond,
                Word(ALRandom32(999)), // AMilliSecond: Word
                LTime) do continue;

    Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
    {$WARN SYMBOL_PLATFORM OFF}
    Var LDelphiFormatSettings := TformatSettings.Create(LCID);
    Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
    {$WARN SYMBOL_PLATFORM ON}
    if ALRandom32(2) = 1 then begin
      LDelphiFormatSettings.ShortDateFormat := LDelphiFormatSettings.LongDateFormat;
      LDelphiFormatSettings.ShortTimeFormat := LDelphiFormatSettings.LongTimeFormat;
      LAlcinoeFormatSettings.ShortDateFormat := LAlcinoeFormatSettings.LongDateFormat;
      LAlcinoeFormatSettings.ShortTimeFormat := LAlcinoeFormatSettings.LongTimeFormat;
    end;
    case ALRandom32(3) of
      0: begin
        LDelphiFormatSettings.ShortDateFormat := LDelphiFormatSettings.LongDateFormat.ToLower;
        LDelphiFormatSettings.ShortTimeFormat := LDelphiFormatSettings.LongTimeFormat.ToLower;
        LAlcinoeFormatSettings.ShortDateFormat := ansiString(LDelphiFormatSettings.ShortDateFormat);
        LAlcinoeFormatSettings.ShortTimeFormat := ansiString(LDelphiFormatSettings.ShortTimeFormat);
      end;
      1: begin
        LDelphiFormatSettings.ShortDateFormat := LDelphiFormatSettings.LongDateFormat.Toupper;
        LDelphiFormatSettings.ShortTimeFormat := LDelphiFormatSettings.LongTimeFormat.Toupper;
        LAlcinoeFormatSettings.ShortDateFormat := ansiString(LDelphiFormatSettings.ShortDateFormat);
        LAlcinoeFormatSettings.ShortTimeFormat := ansiString(LDelphiFormatSettings.ShortTimeFormat);
      end;
      2:;
      else raise Exception.Create('Error 96C64CFA-DCCE-463C-828B-077F21659C6B');
    end;

    var LDelphiTimeStr: String;
    var LAlcinoeTimestr: ansiString;
    fStopWatchDELPHI.Start; {-} LDelphiTimeStr := TimeToStr(LTime, LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} LAlcinoeTimestr := alTimeToStrA(LTime, LAlcinoeFormatSettings); {-} fStopWatchAlcinoe.Stop;

    if LAlcinoeTimestr <> AnsiString(LDelphiTimeStr) then Assert.Fail;

  end;
  //--
  CheckExecutionTime;
end;

{********************************************}
procedure TALTestStrings.TestALDateTimeToStrA;
begin
  for var I := 0 to 100000 do begin

    var LDateTime: TDateTime;
    While not TryEncodeDateTime(
                Word(ALRandom32(9998) + 1), //const AYear,
                Word(ALrandom32(12) + 1), // AMonth,
                Word(ALRandom32(31) + 1), //ADay,
                Word(ALRandom32(23)), // AHour,
                Word(ALRandom32(60)), // AMinute,
                Word(ALRandom32(60)), // ASecond,
                Word(ALRandom32(999)), // AMilliSecond: Word
                LDateTime) do continue;

    Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
    {$WARN SYMBOL_PLATFORM OFF}
    Var LDelphiFormatSettings := TformatSettings.Create(LCID);
    Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
    {$WARN SYMBOL_PLATFORM ON}
    if ALRandom32(2) = 1 then begin
      LDelphiFormatSettings.ShortDateFormat := LDelphiFormatSettings.LongDateFormat;
      LDelphiFormatSettings.ShortTimeFormat := LDelphiFormatSettings.LongTimeFormat;
      LAlcinoeFormatSettings.ShortDateFormat := LAlcinoeFormatSettings.LongDateFormat;
      LAlcinoeFormatSettings.ShortTimeFormat := LAlcinoeFormatSettings.LongTimeFormat;
    end;
    case ALRandom32(3) of
      0: begin
        LDelphiFormatSettings.ShortDateFormat := LDelphiFormatSettings.LongDateFormat.ToLower;
        LDelphiFormatSettings.ShortTimeFormat := LDelphiFormatSettings.LongTimeFormat.ToLower;
        LAlcinoeFormatSettings.ShortDateFormat := ansiString(LDelphiFormatSettings.ShortDateFormat);
        LAlcinoeFormatSettings.ShortTimeFormat := ansiString(LDelphiFormatSettings.ShortTimeFormat);
      end;
      1: begin
        LDelphiFormatSettings.ShortDateFormat := LDelphiFormatSettings.LongDateFormat.Toupper;
        LDelphiFormatSettings.ShortTimeFormat := LDelphiFormatSettings.LongTimeFormat.Toupper;
        LAlcinoeFormatSettings.ShortDateFormat := ansiString(LDelphiFormatSettings.ShortDateFormat);
        LAlcinoeFormatSettings.ShortTimeFormat := ansiString(LDelphiFormatSettings.ShortTimeFormat);
      end;
      2:;
      else raise Exception.Create('Error 96C64CFA-DCCE-463C-828B-077F21659C6B');
    end;

    var LDelphiDateTimeStr: String;
    var LAlcinoeDateTimestr: ansiString;
    fStopWatchDELPHI.Start; {-} LDelphiDateTimeStr := DateTimeToStr(LDateTime, LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} LAlcinoeDateTimestr := alDateTimeToStrA(LDateTime, LAlcinoeFormatSettings); {-} fStopWatchAlcinoe.Stop;

    if LAlcinoeDateTimestr <> AnsiString(LDelphiDateTimeStr) then
      Assert.Fail;

  end;
  //--
  CheckExecutionTime;
end;

{*********************************************}
procedure TALTestStrings.TestALFormatDateTimeA;
begin

  Var LSpecifiers := TalStringListW.create;
  try

    LSpecifiers.add('c'); // Displays the date using the format given by the ShortDateFormat global variable, followed by the time using the format given by the LongTimeFormat global variable. The time is not displayed if the date-time value indicates midnight precisely.
    LSpecifiers.add('d'); // Displays the day as a number without a leading zero (1-31).
    LSpecifiers.add('dd'); // Displays the day as a number with a leading zero (01-31).
    LSpecifiers.add('ddd'); // Displays the day as an abbreviation (Sun-Sat) using the strings given by the ShortDayNames global variable.
    LSpecifiers.add('dddd'); // Displays the day as a full name (Sunday-Saturday) using the strings given by the LongDayNames global variable.
    LSpecifiers.add('ddddd'); // Displays the date using the format given by the ShortDateFormat global variable.
    LSpecifiers.add('dddddd'); // Displays the date using the format given by the LongDateFormat global variable.
    LSpecifiers.add('e'); // (Windows only) Displays the year in the current period/era as a number without a leading zero (Japanese, Korean, and Taiwanese locales only).
    LSpecifiers.add('ee'); // (Windows only) Displays the year in the current period/era as a number with a leading zero (Japanese, Korean, and Taiwanese locales only).
    LSpecifiers.add('g'); // (Windows only) Displays the period/era as an abbreviation (Japanese and Taiwanese locales only).
    LSpecifiers.add('gg'); // (Windows only) Displays the period/era as a full name (Japanese and Taiwanese locales only).
    LSpecifiers.add('m'); // Displays the month as a number without a leading zero (1-12). If the m specifier immediately follows an h or hh specifier, the minute rather than the month is displayed.
    LSpecifiers.add('mm'); // Displays the month as a number with a leading zero (01-12). If the mm specifier immediately follows an h or hh specifier, the minute rather than the month is displayed.
    LSpecifiers.add('mmm'); // Displays the month as an abbreviation (Jan-Dec) using the strings given by the ShortMonthNames global variable.
    LSpecifiers.add('mmmm'); // Displays the month as a full name (January-December) using the strings given by the LongMonthNames global variable.
    LSpecifiers.add('yy'); // Displays the year as a two-digit number (00-99).
    LSpecifiers.add('yyyy'); // Displays the year as a four-digit number (0000-9999).
    LSpecifiers.add('h'); // Displays the hour without a leading zero (0-23).
    LSpecifiers.add('hh'); // Displays the hour with a leading zero (00-23).
    LSpecifiers.add('n'); // Displays the minute without a leading zero (0-59).
    LSpecifiers.add('nn'); // Displays the minute with a leading zero (00-59).
    LSpecifiers.add('s'); // Displays the second without a leading zero (0-59).
    LSpecifiers.add('ss'); // Displays the second with a leading zero (00-59).
    LSpecifiers.add('z'); // Displays the millisecond without a leading zero (0-999).
    LSpecifiers.add('zzz'); // Displays the millisecond with a leading zero (000-999).
    LSpecifiers.add('t'); // Displays the time using the format given by the ShortTimeFormat global variable.
    LSpecifiers.add('tt'); // Displays the time using the format given by the LongTimeFormat global variable.
    LSpecifiers.add('am/pm'); // Uses the 12-hour clock for the preceding h or hh specifier, and displays 'am' for any hour before noon, and 'pm' for any hour after noon. The am/pm specifier can use lower, upper, or mixed case, and the result is displayed accordingly.
    LSpecifiers.add('a/p'); // Uses the 12-hour clock for the preceding h or hh specifier, and displays 'a' for any hour before noon, and 'p' for any hour after noon. The a/p specifier can use lower, upper, or mixed case, and the result is displayed accordingly.
    LSpecifiers.add('ampm'); // Uses the 12-hour clock for the preceding h or hh specifier, and displays the contents of the TimeAMString global variable for any hour before noon, and the contents of the TimePMString global variable for any hour after noon.
    LSpecifiers.add('/'); //Displays the date separator character given by the DateSeparator global variable.
    LSpecifiers.add(':'); //Displays the time separator character given by the TimeSeparator global variable.

    for var I := 0 to 10000 do begin

      var LDateTime: TDateTime;
      While not TryEncodeDateTime(
                  Word(ALRandom32(9998) + 1), //const AYear,
                  Word(ALrandom32(12) + 1), // AMonth,
                  Word(ALRandom32(31) + 1), //ADay,
                  Word(ALRandom32(23)), // AHour,
                  Word(ALRandom32(60)), // AMinute,
                  Word(ALRandom32(60)), // ASecond,
                  Word(ALRandom32(999)), // AMilliSecond: Word
                  LDateTime) do continue;

      Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
      {$WARN SYMBOL_PLATFORM OFF}
      Var LDelphiFormatSettings := TformatSettings.Create(LCID);
      Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
      {$WARN SYMBOL_PLATFORM ON}
      if ALRandom32(2) = 1 then begin
        LDelphiFormatSettings.ShortDateFormat := LDelphiFormatSettings.LongDateFormat;
        LDelphiFormatSettings.ShortTimeFormat := LDelphiFormatSettings.LongTimeFormat;
        LAlcinoeFormatSettings.ShortDateFormat := LAlcinoeFormatSettings.LongDateFormat;
        LAlcinoeFormatSettings.ShortTimeFormat := LAlcinoeFormatSettings.LongTimeFormat;
      end;

      Var LformatU: String := '';
      for var J := 0 to ALRandom32(32) do begin
        case ALRandom32(10) of
          0..8: begin
                  case alRandom32(3) of
                    0: LformatU := LformatU + LSpecifiers[ALRandom32(LSpecifiers.Count)].ToLower;
                    1: LformatU := LformatU + LSpecifiers[ALRandom32(LSpecifiers.Count)].ToUpper;
                    3: LformatU := LformatU + LSpecifiers[ALRandom32(LSpecifiers.Count)];
                  end;
                end;
          9: begin
               Var LRandomStr: String := UnicodeRandomStr(ALRandom32(32));
               if ALRandom32(2) = 1 then LformatU := LformatU + '"' + LRandomStr + '"'
               else LformatU := LformatU + '''' + LRandomStr + '''';
             end;
          else raise Exception.Create('Error 8B77C611-FED9-4563-8DB4-D20805738BA9');
        end;
      end;
      Var Lformat := AnsiString(LformatU);

      fStopWatchDELPHI.Start; {-} var LDelphiDateTimeStr := FormatDateTime(LformatU, LDateTime, LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
      fStopWatchAlcinoe.Start; {-} var LAlcinoeDateTimestr := ALFormatDateTimeA(Lformat, LDateTime, LAlcinoeFormatSettings); {-} fStopWatchAlcinoe.Stop;

      if string(LAlcinoeDateTimestr) <> LDelphiDateTimeStr then Assert.Fail;

    end;
  finally
    ALFreeAndNil(LSpecifiers);
  end;
  //--
  CheckExecutionTime;

end;

{***************************************}
procedure TALTestStrings.TestALIntToStrA;
begin
  //int32
  for var I := 0 to 1000000 do begin
    var LInt: int32 := integer(ALRandom32(Maxint));
    if ALRandom32(2) = 1 then Lint := Lint * -1;
    fStopWatchDELPHI.Start; {-} var LDELPHIStr := IntToStr(LInt); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoeStr := ALIntToStrA(LInt); {-} fStopWatchAlcinoe.Stop;
    if String(LAlcinoeStr) <> LDELPHIStr then Assert.Fail;
  end;
  //int64
  for var I := 0 to 1000000 do begin
    var LInt: int64 := int64(ALRandom64(ALMaxint64));
    if ALRandom32(2) = 1 then Lint := Lint * -1;
    fStopWatchDELPHI.Start; {-} var LDELPHIStr := IntToStr(LInt); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoeStr := ALIntToStrA(LInt); {-} fStopWatchAlcinoe.Stop;
    if String(LAlcinoeStr) <> LDELPHIStr then Assert.Fail;
  end;
  //--
  CheckExecutionTime;
end;

{***************************************}
procedure TALTestStrings.TestALStrToIntA;
begin
  for var I := 0 to 1000000 do begin
    Var LIntegerIn: Integer := ALRandom32(ALMaxInt);
    if ALRandom32(2) = 1 then LIntegerIn := LIntegerIn * -1;

    var LStrU := IntToStr(LIntegerIn);
    var LStr := AnsiString(LstrU);

    fStopWatchDELPHI.Start; {-} Var LDelphiIntegerOut := StrToInt(LStrU); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} Var LAlcinoeIntegerOut := ALStrToInt(LStr); {-} fStopWatchAlcinoe.Stop;

    if LIntegerIn <> LAlcinoeIntegerOut then Assert.Fail;
    if LDelphiIntegerOut <> LAlcinoeIntegerOut then Assert.Fail;
  end;
  //--
  CheckExecutionTime;
end;

{****************************************}
procedure TALTestStrings.TestALStrToUIntA;
begin
  for var I := 0 to 1000000 do begin
    Var LIntegerIn: cardinal := ALRandom32(ALMaxUInt);

    var LStrU := UIntToStr(LIntegerIn);
    var LStr := AnsiString(LstrU);

    fStopWatchDELPHI.Start; {-} Var LDelphiIntegerOut := StrToUInt(LStrU); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} Var LAlcinoeIntegerOut := ALStrToUInt(LStr); {-} fStopWatchAlcinoe.Stop;

    if LIntegerIn <> LAlcinoeIntegerOut then Assert.Fail;
    if LDelphiIntegerOut <> LAlcinoeIntegerOut then Assert.Fail;
  end;
  //--
  CheckExecutionTime;
end;

{*****************************************}
procedure TALTestStrings.TestALStrToInt64A;
begin
  for var I := 0 to 1000000 do begin
    Var LIntegerIn: int64 := ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LIntegerIn := LIntegerIn * -1;

    var LStrU := IntToStr(LIntegerIn);
    var LStr := AnsiString(LstrU);

    fStopWatchDELPHI.Start; {-} Var LDelphiIntegerOut := StrToInt64(LStrU); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} Var LAlcinoeIntegerOut := ALStrToInt64(LStr); {-} fStopWatchAlcinoe.Stop;

    if LIntegerIn <> LAlcinoeIntegerOut then Assert.Fail;
    if LDelphiIntegerOut <> LAlcinoeIntegerOut then Assert.Fail;
  end;
  //--
  CheckExecutionTime;
end;

{******************************************}
procedure TALTestStrings.TestALStrToUInt64A;
begin
  for var I := 0 to 1000000 do begin
    Var LIntegerIn: UInt64 := ALRandom64(ALMaxUInt64);

    var LStrU := UIntToStr(LIntegerIn);
    var LStr := AnsiString(LstrU);

    fStopWatchDELPHI.Start; {-} Var LDelphiIntegerOut := StrToUInt64(LStrU); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} Var LAlcinoeIntegerOut := ALStrToUInt64(LStr); {-} fStopWatchAlcinoe.Stop;

    if LIntegerIn <> LAlcinoeIntegerOut then Assert.Fail;
    if LDelphiIntegerOut <> LAlcinoeIntegerOut then Assert.Fail;
  end;
  //--
  CheckExecutionTime;
end;

{*****************************************}
procedure TALTestStrings.TestALFloatToStrA;
begin
  for var I := 0 to 100000 do begin
    Var LExtendedIn: Extended := ALRandom64(ALMaxInt64) / ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtendedIn := LExtendedIn + ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtendedIn := LExtendedIn * -1;

    Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
    {$WARN SYMBOL_PLATFORM OFF}
    Var LDelphiFormatSettings := TformatSettings.Create(LCID);
    Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
    {$WARN SYMBOL_PLATFORM ON}

    fStopWatchDELPHI.Start; {-} var LDELPHIStr := FloatToStr(LExtendedIn, LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoeStr := ALFloatToStrA(LExtendedIn, LAlcinoeFormatSettings); {-} fStopWatchAlcinoe.Stop;
    if String(LAlcinoeStr) <> LDELPHIStr then Assert.Fail;
  end;
  //--
  CheckExecutionTime;
end;

{******************************************}
procedure TALTestStrings.TestALFloatToStrFA;
begin
  for var I := 0 to 100000 do begin
    Var LExtendedIn: Extended := ALRandom64(ALMaxInt64) / ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtendedIn := LExtendedIn + ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtendedIn := LExtendedIn * -1;

    Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
    {$WARN SYMBOL_PLATFORM OFF}
    Var LDelphiFormatSettings := TformatSettings.Create(LCID);
    Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
    {$WARN SYMBOL_PLATFORM ON}

    var LFormat: TFloatFormat;
    case ALRandom32(5) of
      0: LFormat := ffGeneral;
      1: LFormat := ffExponent;
      2: LFormat := ffFixed;
      3: LFormat := ffNumber;
      4: LFormat := ffCurrency;
      else raise Exception.Create('Error B9DFD044-7BD9-4EB8-89F1-8099A73A7E8A');
    end;
    var LPrecision := ALRandom32(32);
    var LDigits := ALRandom32(32);

    fStopWatchDELPHI.Start; {-} var LDELPHIStr := FloatToStrF(LExtendedIn, LFormat, LPrecision, LDigits, LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoeStr := ALFloatToStrFA(LExtendedIn, LFormat, LPrecision, LDigits, LAlcinoeFormatSettings); {-} fStopWatchAlcinoe.Stop;
    if String(LAlcinoeStr) <> LDELPHIStr then Assert.Fail;
  end;
  //--
  CheckExecutionTime;
end;

{*****************************************}
procedure TALTestStrings.TestALStrToFloatA;
begin
  for var I := 0 to 100000 do begin
    Var LExtendedIn: Extended := ALRandom64(ALMaxInt64) / ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtendedIn := LExtendedIn + ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtendedIn := LExtendedIn * -1;

    Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
    {$WARN SYMBOL_PLATFORM OFF}
    Var LDelphiFormatSettings := TformatSettings.Create(LCID);
    Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
    {$WARN SYMBOL_PLATFORM ON}
    var LStrU := FloatToStr(LExtendedIn, LDelphiFormatSettings);
    var LStr := AnsiString(LstrU);

    fStopWatchDELPHI.Start; {-} Var LDelphiExtendedOut := StrToFloat(LStrU, LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} Var LAlcinoeExtendedOut := ALStrToFloat(LStr, LAlcinoeFormatSettings);fStopWatchAlcinoe.stop;

    {$IF not defined(WIN32)}
    if not sameValue(LExtendedIn, LAlcinoeExtendedOut) then Assert.Fail;
    {$ENDIF}
    if not sameValue(LDelphiExtendedOut, LAlcinoeExtendedOut) then Assert.Fail;
  end;
  //--
  CheckExecutionTime;
end;

{****************************************}
procedure TALTestStrings.TestALCurrToStrA;
begin
  for var I := 0 to 100000 do begin
    Var LCurrencyIn: Currency := ALRandom32(ALMaxInt) / ALRandom32(ALMaxInt);
    if ALRandom32(2) = 1 then LCurrencyIn := LCurrencyIn + ALRandom32(ALMaxInt);
    if ALRandom32(2) = 1 then LCurrencyIn := LCurrencyIn * -1;

    Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
    {$WARN SYMBOL_PLATFORM OFF}
    Var LDelphiFormatSettings := TformatSettings.Create(LCID);
    Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
    {$WARN SYMBOL_PLATFORM ON}

    fStopWatchDELPHI.Start; {-} var LDELPHIStr := CurrToStr(LCurrencyIn, LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoeStr := ALCurrToStrA(LCurrencyIn, LAlcinoeFormatSettings); {-} fStopWatchAlcinoe.Stop;
    if String(LAlcinoeStr) <> LDELPHIStr then Assert.Fail;
  end;
  //--
  CheckExecutionTime;
end;

{****************************************}
procedure TALTestStrings.TestALStrToCurrA;
begin
  for var I := 0 to 100000 do begin
    Var LCurrencyIn: Currency := ALRandom32(ALMaxInt) / ALRandom32(ALMaxInt);
    if ALRandom32(2) = 1 then LCurrencyIn := LCurrencyIn + ALRandom32(ALMaxInt);
    if ALRandom32(2) = 1 then LCurrencyIn := LCurrencyIn * -1;

    Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
    {$WARN SYMBOL_PLATFORM OFF}
    Var LDelphiFormatSettings := TformatSettings.Create(LCID);
    Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
    {$WARN SYMBOL_PLATFORM ON}
    var LStrU := FloatToStr(LCurrencyIn, LDelphiFormatSettings);
    var LStr := AnsiString(LstrU);

    fStopWatchDELPHI.Start; {-} Var LDelphiCurrencyOut := StrToCurr(LStrU, LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} Var LAlcinoeCurrencyOut := ALStrToCurr(LStr, LAlcinoeFormatSettings);fStopWatchAlcinoe.stop;

    {$IF not defined(WIN32)}
    if not sameValue(LCurrencyIn, LAlcinoeCurrencyOut) then Assert.Fail;
    {$ENDIF}
    if not sameValue(LDelphiCurrencyOut, LAlcinoeCurrencyOut) then Assert.Fail;
  end;
  //--
  CheckExecutionTime;
end;

{******************************************}
procedure TALTestStrings.TestALFormatFloatA;
begin
  //In Win32, delphi RTL use the fastcode ASM that we abandon in alcinoe so
  //the result given by FormatFloat is often <> from the one given by ALFormatFloatA
  {$IF defined(WIN64)}
  for var I := 0 to 100000 do begin
    Var LExtendedIn: Extended := ALRandom64(ALMaxInt64) / ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtendedIn := LExtendedIn + ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtendedIn := LExtendedIn * -1;

    Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
    {$WARN SYMBOL_PLATFORM OFF}
    Var LDelphiFormatSettings := TformatSettings.Create(LCID);
    Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
    {$WARN SYMBOL_PLATFORM ON}

    var LDelphiFormat: String;
    case ALRandom32(4) of
      0:  LDelphiFormat := '0';
      1:  LDelphiFormat := '#';
      2:  LDelphiFormat := '#.#';
      3:  LDelphiFormat := '0.#';
      4:  LDelphiFormat := '#.0';
      5:  LDelphiFormat := '0.0';
      6:  LDelphiFormat := 'E+';
      7:  LDelphiFormat := 'e+';
      8:  LDelphiFormat := 'E-';
      9:  LDelphiFormat := 'e-';
      10: LDelphiFormat := 'E+0';
      11: LDelphiFormat := 'e+0';
      12: LDelphiFormat := 'E-0';
      13: LDelphiFormat := 'e-0';
      else raise Exception.Create('Error EA4AB333-2A25-4F16-99F5-90EB657A0087');
    end;
    for var J := 0 to 3 do begin
      var LCh: Char;
      if J <= 1 then LCh := '#'
      else LCh := '0';
      var LStr: String;
      Setlength(LStr, ALRandom32(32) + 1);
      for var K := Low(LStr) to High(Lstr) do
        LStr[K] := Lch;
      LDelphiFormat := StringReplace(LDelphiFormat,Lch,Lstr,[]);
    end;
    Case ALRandom32(3) of
      0: ;
      1: LDelphiFormat := LDelphiFormat + ',';
      2: LDelphiFormat := ',' + LDelphiFormat;
      else raise Exception.Create('Error B9541973-2763-4D3C-937C-1A85B13B1D37');
    End;
    Case ALRandom32(5) of
      0: ;
      1: LDelphiFormat := LDelphiFormat + '"xyz''''xyz"';
      2: LDelphiFormat := '"xyz''''xyz"' + LDelphiFormat;
      3: LDelphiFormat := LDelphiFormat + '''xyz"xyz''';
      4: LDelphiFormat := '''xyz"xyz''' + LDelphiFormat;
      else raise Exception.Create('Error 42A2FAE7-83C5-4A0B-B515-9701530903D2');
    End;
    Case ALRandom32(2) of
      0:;
      1: begin
           LDelphiFormat := StringReplace(LDelphiFormat,'-','+',[]);
           Var LStr := StringReplace(LDelphiFormat,'+','-',[]);
           LDelphiFormat := LDelphiFormat + ';' + Lstr + ';' + LDelphiFormat;
         end;
    End;
    LDelphiFormat := Copy(LDelphiFormat,1,255-32);
    var LAlcinoeFormat := AnsiString(LDelphiFormat);

    fStopWatchDELPHI.Start; {-} var LDelphiStrOut := FormatFloat(LDelphiFormat, LExtendedIn, LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoeStrOut := ALFormatFloatA(LAlcinoeFormat, LExtendedIn, LAlcinoeFormatSettings); {-} fStopWatchAlcinoe.Stop;

    if LDelphiStrOut <> String(LAlcinoeStrOut) then
      Assert.Fail;

  end;
  //--
  CheckExecutionTime;
  {$ENDIF}
end;

{*****************************************}
procedure TALTestStrings.TestALFormatCurrA;
begin
  //In Win32, delphi RTL use the fastcode ASM that we abandon in alcinoe so
  //the result given by FormatFloat is often <> from the one given by ALFormatFloatA
  {$IF defined(WIN64)}
  for var I := 0 to 100000 do begin
    Var LCurrencyIn: Currency := ALRandom32(ALMaxInt) / ALRandom32(ALMaxInt);
    if ALRandom32(2) = 1 then LCurrencyIn := LCurrencyIn + ALRandom32(ALMaxInt);
    if ALRandom32(2) = 1 then LCurrencyIn := LCurrencyIn * -1;

    Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
    {$WARN SYMBOL_PLATFORM OFF}
    Var LDelphiFormatSettings := TformatSettings.Create(LCID);
    Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
    {$WARN SYMBOL_PLATFORM ON}

    var LDelphiFormat: String;
    case ALRandom32(4) of
      0:  LDelphiFormat := '0';
      1:  LDelphiFormat := '#';
      2:  LDelphiFormat := '#.#';
      3:  LDelphiFormat := '0.#';
      4:  LDelphiFormat := '#.0';
      5:  LDelphiFormat := '0.0';
      6:  LDelphiFormat := 'E+';
      7:  LDelphiFormat := 'e+';
      8:  LDelphiFormat := 'E-';
      9:  LDelphiFormat := 'e-';
      10: LDelphiFormat := 'E+0';
      11: LDelphiFormat := 'e+0';
      12: LDelphiFormat := 'E-0';
      13: LDelphiFormat := 'e-0';
      else raise Exception.Create('Error EA4AB333-2A25-4F16-99F5-90EB657A0087');
    end;
    for var J := 0 to 3 do begin
      var LCh: Char;
      if J <= 1 then LCh := '#'
      else LCh := '0';
      var LStr: String;
      Setlength(LStr, ALRandom32(32) + 1);
      for var K := Low(LStr) to High(Lstr) do
        LStr[K] := Lch;
      LDelphiFormat := StringReplace(LDelphiFormat,Lch,Lstr,[]);
    end;
    Case ALRandom32(3) of
      0: ;
      1: LDelphiFormat := LDelphiFormat + ',';
      2: LDelphiFormat := ',' + LDelphiFormat;
      else raise Exception.Create('Error B9541973-2763-4D3C-937C-1A85B13B1D37');
    End;
    Case ALRandom32(5) of
      0: ;
      1: LDelphiFormat := LDelphiFormat + '"xyz''''xyz"';
      2: LDelphiFormat := '"xyz''''xyz"' + LDelphiFormat;
      3: LDelphiFormat := LDelphiFormat + '''xyz"xyz''';
      4: LDelphiFormat := '''xyz"xyz''' + LDelphiFormat;
      else raise Exception.Create('Error 42A2FAE7-83C5-4A0B-B515-9701530903D2');
    End;
    Case ALRandom32(2) of
      0:;
      1: begin
           LDelphiFormat := StringReplace(LDelphiFormat,'-','+',[]);
           Var LStr := StringReplace(LDelphiFormat,'+','-',[]);
           LDelphiFormat := LDelphiFormat + ';' + Lstr + ';' + LDelphiFormat;
         end;
    End;
    LDelphiFormat := Copy(LDelphiFormat,1,255-32);
    var LAlcinoeFormat := AnsiString(LDelphiFormat);

    fStopWatchDELPHI.Start; {-} var LDelphiStrOut := FormatCurr(LDelphiFormat, LCurrencyIn, LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoeStrOut := ALFormatCurrA(LAlcinoeFormat, LCurrencyIn, LAlcinoeFormatSettings); {-} fStopWatchAlcinoe.Stop;

    if LDelphiStrOut <> String(LAlcinoeStrOut) then
      Assert.Fail;

  end;
  //--
  CheckExecutionTime;
  {$ENDIF}
end;

{*************************************}
procedure TALTestStrings.TestALFormatA;
begin
  for var I := 0 to 10000 do begin

    Var LCID := FValidLCID[ALrandom32(length(FValidLCID))];
    {$WARN SYMBOL_PLATFORM OFF}
    Var LDelphiFormatSettings := TformatSettings.Create(LCID);
    Var LAlcinoeFormatSettings := TALFormatSettingsA.Create(LCID);
    {$WARN SYMBOL_PLATFORM ON}

    //"%" [index ":"] ["-"] [width] ["." prec] type
    var LDelphiFormat: String := '';

    //d Decimal. The argument must be an integer value. The value is converted to a string
    //of decimal digits. If the format string contains a precision specifier, it indicates
    //that the resulting string must contain at least the specified number of digits; if
    //the value has less digits, the resulting string is left-padded with zeros.
    Var LInt1: int64 := ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LInt1 := LInt1 * -1;
    //
    LDelphiFormat := LDelphiFormat + '%';
    if ALRandom32(5)=0 then LDelphiFormat := LDelphiFormat + '-';
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + alinttostrW(ALRandom32(32));
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + '.' + alinttostrW(ALRandom32(32));
    LDelphiFormat := LDelphiFormat + 'd';
    if ALRandom32(2) = 1 then LDelphiFormat := LDelphiFormat + ALStringReplaceW(UnicodeRandomStr(ALRandom32(10)), '%', '%%',[rfReplaceALL]);
    //--
    var LInt2: int32 := integer(ALRandom32(Maxint));
    if ALRandom32(2) = 1 then Lint2 := Lint2 * -1;
    //
    LDelphiFormat := LDelphiFormat + '%';
    if ALRandom32(5)=0 then LDelphiFormat := LDelphiFormat + '-';
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + alinttostrW(ALRandom32(32));
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + '.' + alinttostrW(ALRandom32(32));
    LDelphiFormat := LDelphiFormat + 'd';
    if ALRandom32(2) = 1 then LDelphiFormat := LDelphiFormat + ALStringReplaceW(UnicodeRandomStr(ALRandom32(10)), '%', '%%',[rfReplaceALL]);

    //u Unsigned decimal. Similar to d, but no sign is output.
    Var LInt3: int64 := ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LInt3 := LInt3 * -1;
    //
    LDelphiFormat := LDelphiFormat + '%';
    if ALRandom32(5)=0 then LDelphiFormat := LDelphiFormat + '-';
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + alinttostrW(ALRandom32(32));
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + '.' + alinttostrW(ALRandom32(32));
    LDelphiFormat := LDelphiFormat + 'u';
    if ALRandom32(2) = 1 then LDelphiFormat := LDelphiFormat + ALStringReplaceW(UnicodeRandomStr(ALRandom32(10)), '%', '%%',[rfReplaceALL]);
    //--
    var LInt4: int32 := integer(ALRandom32(Maxint));
    if ALRandom32(2) = 1 then Lint4 := Lint4 * -1;
    //--
    LDelphiFormat := LDelphiFormat + '%';
    if ALRandom32(5)=0 then LDelphiFormat := LDelphiFormat + '-';
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + alinttostrW(ALRandom32(32));
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + '.' + alinttostrW(ALRandom32(32));
    LDelphiFormat := LDelphiFormat + 'u';
    if ALRandom32(2) = 1 then LDelphiFormat := LDelphiFormat + ALStringReplaceW(UnicodeRandomStr(ALRandom32(10)), '%', '%%',[rfReplaceALL]);

    //e Scientific. The argument must be a floating-point value. The value is converted to a
    //string of the form "-d.ddd...E+ddd". The resulting string starts with a minus sign if
    //the number is negative. One digit always precedes the decimal point. The total number
    //of digits in the resulting string (including the one before the decimal point) is given by
    //the precision specifier in the format string; a default precision of 15 is assumed if no
    //precision specifier is present. The "E" exponent character in the resulting string is
    //always followed by a plus or minus sign and at least three digits.
    Var LExtended1: Extended := ALRandom64(ALMaxInt64) / ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtended1 := LExtended1 + ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtended1 := LExtended1 * -1;
    //
    LDelphiFormat := LDelphiFormat + '%';
    if ALRandom32(5)=0 then LDelphiFormat := LDelphiFormat + '-';
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + alinttostrW(ALRandom32(32));
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + '.' + alinttostrW(ALRandom32(32));
    LDelphiFormat := LDelphiFormat + 'e';
    if ALRandom32(2) = 1 then LDelphiFormat := LDelphiFormat + ALStringReplaceW(UnicodeRandomStr(ALRandom32(10)), '%', '%%',[rfReplaceALL]);

    //f Fixed. The argument must be a floating-point value. The value is converted to a string
    //of the form "-ddd.ddd...". The resulting string starts with a minus sign if the number
    //is negative. The number of digits after the decimal point is given by the precision
    //specifier in the format string—a default of 2 decimal digits is assumed if no precision
    //specifier is present.
    Var LExtended2: Extended := ALRandom64(ALMaxInt64) / ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtended2 := LExtended2 + ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtended2 := LExtended2 * -1;
    //
    LDelphiFormat := LDelphiFormat + '%';
    if ALRandom32(5)=0 then LDelphiFormat := LDelphiFormat + '-';
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + alinttostrW(ALRandom32(32));
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + '.' + alinttostrW(ALRandom32(32));
    LDelphiFormat := LDelphiFormat + 'f';
    if ALRandom32(2) = 1 then LDelphiFormat := LDelphiFormat + ALStringReplaceW(UnicodeRandomStr(ALRandom32(10)), '%', '%%',[rfReplaceALL]);

    //g General. The argument must be a floating-point value. The value is converted to the
    //shortest possible decimal string using fixed or scientific format. The number of
    //significant digits in the resulting string is given by the precision specifier
    //in the format string; a default precision of 15 is assumed if no precision specifier
    //is present. Trailing zeros are removed from the resulting string, and a decimal point
    //appears only if necessary. The resulting string uses the fixed-point format if the number
    //of digits to the left of the decimal point in the value is less than or equal to the
    //specified precision, and if the value is greater than or equal to 0.00001. Otherwise
    //the resulting string uses scientific format.
    Var LExtended3: Extended := ALRandom64(ALMaxInt64) / ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtended3 := LExtended3 + ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtended3 := LExtended3 * -1;
    //
    LDelphiFormat := LDelphiFormat + '%';
    if ALRandom32(5)=0 then LDelphiFormat := LDelphiFormat + '-';
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + alinttostrW(ALRandom32(32));
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + '.' + alinttostrW(ALRandom32(32));
    LDelphiFormat := LDelphiFormat + 'g';
    if ALRandom32(2) = 1 then LDelphiFormat := LDelphiFormat + ALStringReplaceW(UnicodeRandomStr(ALRandom32(10)), '%', '%%',[rfReplaceALL]);

    //n Number. The argument must be a floating-point value. The value is converted to a
    //string of the form "-d,ddd,ddd.ddd...". The n format corresponds to the f format,
    //except that the resulting string contains thousand separators.
    Var LExtended4: Extended := ALRandom64(ALMaxInt64) / ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtended4 := LExtended4 + ALRandom64(ALMaxInt64);
    if ALRandom32(2) = 1 then LExtended4 := LExtended4 * -1;
    //
    LDelphiFormat := LDelphiFormat + '%';
    if ALRandom32(5)=0 then LDelphiFormat := LDelphiFormat + '-';
    if (ALRandom32(2)=0) and (length(LAlcinoeFormatSettings.ThousandSeparator) = length(LDelphiFormatSettings.ThousandSeparator)) then LDelphiFormat := LDelphiFormat + alinttostrW(ALRandom32(32));
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + '.' + alinttostrW(ALRandom32(32));
    LDelphiFormat := LDelphiFormat + 'n';
    if ALRandom32(2) = 1 then LDelphiFormat := LDelphiFormat + ALStringReplaceW(UnicodeRandomStr(ALRandom32(10)), '%', '%%',[rfReplaceALL]);

    //m Money. The argument must be a floating-point value. The value is converted to
    //a string that represents a currency amount. The conversion is controlled by
    //the CurrencyString, CurrencyFormat, NegCurrFormat, ThousandSeparator,
    //DecimalSeparator, and CurrencyDecimals global variables or their equivalent
    //in a TFormatSettings data structure. If the format string contains a precision
    //specifier, it overrides the value given by the CurrencyDecimals global variable
    //or its TFormatSettings equivalent.
    Var LCurrency1: Currency := ALRandom32(ALMaxInt) / ALRandom32(ALMaxInt);
    if ALRandom32(2) = 1 then LCurrency1 := LCurrency1 + ALRandom32(ALMaxInt);
    if ALRandom32(2) = 1 then LCurrency1 := LCurrency1 * -1;
    //
    LDelphiFormat := LDelphiFormat + '%';
    if ALRandom32(5)=0 then LDelphiFormat := LDelphiFormat + '-';
    if (ALRandom32(2)=0) and (length(LAlcinoeFormatSettings.ThousandSeparator) = length(LDelphiFormatSettings.ThousandSeparator)) and (length(LAlcinoeFormatSettings.CurrencyString) = length(LDelphiFormatSettings.CurrencyString)) then LDelphiFormat := LDelphiFormat + alinttostrW(ALRandom32(32));
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + '.' + alinttostrW(ALRandom32(32));
    LDelphiFormat := LDelphiFormat + 'm';
    if ALRandom32(2) = 1 then LDelphiFormat := LDelphiFormat + ALStringReplaceW(UnicodeRandomStr(ALRandom32(10)), '%', '%%',[rfReplaceALL]);

    //p Pointer. The argument must be a pointer value. The value is converted
    //to an 8-character string that represents the pointer's value in hexadecimal.
    LDelphiFormat := LDelphiFormat + '%';
    if ALRandom32(5)=0 then LDelphiFormat := LDelphiFormat + '-';
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + alinttostrW(ALRandom32(32));
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + '.' + alinttostrW(ALRandom32(32));
    LDelphiFormat := LDelphiFormat + 'p';
    if ALRandom32(2) = 1 then LDelphiFormat := LDelphiFormat + ALStringReplaceW(UnicodeRandomStr(ALRandom32(10)), '%', '%%',[rfReplaceALL]);

    //s String. The argument must be a character, a string, or a PChar value. The string or
    //character is inserted in place of the format specifier. The precision specifier,
    //if present in the format string, specifies the maximum length of the resulting string.
    //If the argument is a string that is longer than this maximum, the string is truncated.
    var LStrU1 := UnicodeRandomStr(ALRandom32(255));
    LStrU1 := ALStringReplaceW(LstrU1,'%','%%',[rfReplaceALL]);
    var Lstr1 := AnsiString(LStrU1);
    //--
    LDelphiFormat := LDelphiFormat + '%';
    if ALRandom32(5)=0 then LDelphiFormat := LDelphiFormat + '-';
    //if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + alinttostrW(ALRandom32(32));
    //if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + '.' + alinttostrW(ALRandom32(32));
    LDelphiFormat := LDelphiFormat + 's';
    if ALRandom32(2) = 1 then LDelphiFormat := LDelphiFormat + ALStringReplaceW(UnicodeRandomStr(ALRandom32(10)), '%', '%%',[rfReplaceALL]);

    //x Hexadecimal. The argument must be an integer value. The value is converted to a string
    //of hexadecimal digits. If the format string contains a precision specifier, it indicates
    //that the resulting string must contain at least the specified number of digits; if the
    //value has fewer digits, the resulting string is left-padded with zeros.
    var LInt5: int32 := integer(ALRandom32(Maxint));
    if ALRandom32(2) = 1 then Lint5 := Lint5 * -1;
    //--
    LDelphiFormat := LDelphiFormat + '%';
    if ALRandom32(5)=0 then LDelphiFormat := LDelphiFormat + '-';
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + alinttostrW(ALRandom32(32));
    if ALRandom32(2)=0 then LDelphiFormat := LDelphiFormat + '.' + alinttostrW(ALRandom32(32));
    LDelphiFormat := LDelphiFormat + 'x';
    if ALRandom32(2) = 1 then LDelphiFormat := LDelphiFormat + ALStringReplaceW(UnicodeRandomStr(ALRandom32(10)), '%', '%%',[rfReplaceALL]);

    var LAlcinoeFormat := AnsiString(LDelphiFormat);

    fStopWatchDELPHI.Start; {-} var LDelphiStrOut := Format(LDelphiFormat, [LInt1, LInt2, LInt3, LInt4, LExtended1, LExtended2, LExtended3, LExtended4, LCurrency1, pointer(self), LStrU1, LInt5], LDelphiFormatSettings); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoeStrOut := ALFormatA(LAlcinoeFormat, [LInt1, LInt2, LInt3, LInt4, LExtended1, LExtended2, LExtended3, LExtended4, LCurrency1, pointer(self), LStr1, LInt5], LAlcinoeFormatSettings); {-} fStopWatchAlcinoe.Stop;

    if LDelphiStrOut <> String(LAlcinoeStrOut) then
      Assert.Fail;

  end;
  //--
  CheckExecutionTime;
end;

{******************************************}
procedure TALTestStrings.TestALUTF8CharSize;
begin
  for var LUCS4Char := Low(UCS4Char) to High(UCS4Char) do begin
    //In the version 6.0, Unicode has 1,114,112 code points (U+0000—U+10FFFF)
    //the last code point is U+10FFFF (1,114,111)
    if (LUCS4Char > 1114111) then
      break;
    //In the UTF-16 encoding, code points less than 2^16 are encoded with a single 16-bit code unit
    //equal to the numerical value of the code point, as in the older UCS-2. The newer code points greater
    //than or equal to 2^16 are encoded by a compound value using two 16-bit code units. These two
    //16-bit code units are chosen from the UTF-16 surrogate range 0xD800–0xDFFF which had not previously
    //been assigned to characters. Values in this range are not used as characters, and UTF-16 provides
    //no legal way to code them as individual code points.
    if ((LUCS4Char >= UCS4Char(Char.MinHighSurrogate)) and (LUCS4Char <= UCS4Char(Char.MaxLowSurrogate))) then
      continue;
    var LStr := char.ConvertFromUtf32(LUCS4Char);
    var LAnsiChar := AnsiString(LStr);
    if length(LAnsiChar) = 0 then
      Assert.Fail;
    if length(LAnsiChar) <> ALUTF8CharSize(LAnsiChar[low(LAnsiChar)]) then
      Assert.Fail;
  end;
end;

{*********************************************}
procedure TALTestStrings.TestALUTF8CharToUtf16;
begin
  for var LUCS4Char := Low(UCS4Char) to High(UCS4Char) do begin
    //In the version 6.0, Unicode has 1,114,112 code points (U+0000—U+10FFFF)
    //the last code point is U+10FFFF (1,114,111)
    if (LUCS4Char > 1114111) then
      break;
    //In the UTF-16 encoding, code points less than 2^16 are encoded with a single 16-bit code unit
    //equal to the numerical value of the code point, as in the older UCS-2. The newer code points greater
    //than or equal to 2^16 are encoded by a compound value using two 16-bit code units. These two
    //16-bit code units are chosen from the UTF-16 surrogate range 0xD800–0xDFFF which had not previously
    //been assigned to characters. Values in this range are not used as characters, and UTF-16 provides
    //no legal way to code them as individual code points.
    if ((LUCS4Char >= UCS4Char(Char.MinHighSurrogate)) and (LUCS4Char <= UCS4Char(Char.MaxLowSurrogate))) then
      continue;
    var LStr := char.ConvertFromUtf32(LUCS4Char);
    var LAnsiChar := AnsiString(LStr);
    if length(LAnsiChar) = 0 then
      Assert.Fail;
    var LUTF8CharSize: integer;
    var LUTF16HighSurrogate: Char;
    var LUTF16lowSurrogate: Char;
    fStopWatchDELPHI.Start; {-} var LDelphiStrOut := String(LAnsiChar); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} var LAlcinoeResult := ALUTF8CharToUtf16(LAnsiChar, low(LAnsiChar), LUTF8CharSize, LUTF16HighSurrogate, LUTF16lowSurrogate); {-} fStopWatchAlcinoe.Stop;
    if not LAlcinoeResult then Assert.Fail;
    if length(LAnsiChar) <> LUTF8CharSize then Assert.Fail;
    if length(LDelphiStrOut) = 0 then Assert.Fail;
    if LDelphiStrOut[low(LDelphiStrOut)] <> LUTF16HighSurrogate then Assert.Fail;
    if (LUTF16lowSurrogate <> #0) then begin
      if (length(LDelphiStrOut) <> 2) then Assert.Fail;
      if LDelphiStrOut[low(LDelphiStrOut)+1] <> LUTF16LowSurrogate then Assert.Fail;
    end
    else if (length(LDelphiStrOut) <> 1) then Assert.Fail;
  end;
  //--
  for var I := 0 to 255 do begin
    Var LAnsiChar: AnsiString := ansichar(i);
    var LUTF8CharSize: integer;
    var LUTF16HighSurrogate: Char;
    var LUTF16lowSurrogate: Char;
    fStopWatchDELPHI.Start; {-} var LDelphiStrOut := String(LAnsiChar); {-} fStopWatchDELPHI.Stop;
    fStopWatchAlcinoe.Start; {-} ALUTF8CharToUtf16(LAnsiChar, low(LAnsiChar), LUTF8CharSize, LUTF16HighSurrogate, LUTF16lowSurrogate); {-} fStopWatchAlcinoe.Stop;
    if length(LDelphiStrOut) = 0 then Assert.Fail;
    if LDelphiStrOut[low(LDelphiStrOut)] <> LUTF16HighSurrogate then Assert.Fail;
    if (LUTF16lowSurrogate <> #0) then begin
      if (length(LDelphiStrOut) <> 2) then Assert.Fail;
      if LDelphiStrOut[low(LDelphiStrOut)+1] <> LUTF16LowSurrogate then Assert.Fail;
    end
    else if (length(LDelphiStrOut) <> 1) then Assert.Fail;
  end;
  //--
  CheckExecutionTime;
end;

initialization
  TDUnitX.RegisterTestFixture(TALTestStrings);

end.
