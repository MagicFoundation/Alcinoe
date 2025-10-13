unit ALDUnitXTestStringUtils;

interface

uses
  System.SysUtils,
  System.AnsiStrings,
  System.Diagnostics,
  Winapi.Windows,
  Alcinoe.Common,
  Alcinoe.Cipher,
  DUnitX.TestFramework;

type

  [TestFixture]
  TALDUnitXTestStringUtils = class
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
    [Test]
    procedure TestALStringBuilderA;
    [Test]
    procedure TestALStringBuilderW;
    [Test]
    procedure TestALPercentEncodeA;
    [Test]
    procedure TestALPercentEncodeW;
    [Test]
    procedure TestALPercentDecodeA;
    [Test]
    procedure TestALPercentDecodeW;
    [Test]
    procedure ALExtractHeaderFieldsA;
    [Test]
    procedure ALExtractHeaderFieldsW;
    [Test]
    procedure TestALMatchesMaskA;
  end;

implementation

uses
  System.NetEncoding,
  System.Math,
  System.Classes,
  System.DateUtils,
  System.Character,
  System.StrUtils,
  System.Masks,
  Alcinoe.StringUtils,
  Alcinoe.Localization,
  Alcinoe.StringList;

{******************************************}
constructor TALDUnitXTestStringUtils.create;
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

{***************************************}
procedure TALDUnitXTestStringUtils.Setup;
begin
  fStopWatchAlcinoe := TStopwatch.Create;
  fStopWatchDELPHI := TStopwatch.Create;
end;

{********************************************}
//procedure TALDUnitXTestStringUtils.TearDown;
//begin
//end;

{********************************************************************************}
procedure TALDUnitXTestStringUtils.CheckExecutionTime(const ARatio: single = 1.2);
begin
  {$IF defined(debug) or defined(Win32)}
  //In debug we have overflow checking and range checking so that mean
  //that the execution time will be much slower that the Delphi RTL so skip it
  //In Win32 we remove all ASM (fastcode heritage) than the delphi RTL have
  //so we will be othen much more slower than the Delphi RTL
  Writeln(ALFormatw('CheckExecutionTime Skipped - %0.0f ms for Alcinoe vs %0.0f ms for Delphi (%0.1fx faster)', [fStopWatchAlcinoe.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds / fStopWatchAlcinoe.Elapsed.TotalMilliseconds]));
  {$ELSE}
  if fStopWatchAlcinoe.Elapsed.TotalMilliseconds > fStopWatchDELPHI.Elapsed.TotalMilliseconds * ARatio then
    Assert.Fail(ALFormatW('Time too long (%0.0f ms for Alcinoe vs %0.0f ms for Delphi)', [fStopWatchAlcinoe.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds]))
  else
    //https://github.com/VSoftTechnologies/DUnitX/issues/319
    Writeln(ALFormatW('%0.0f ms for Alcinoe vs %0.0f ms for Delphi (%0.1fx faster)', [fStopWatchAlcinoe.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds / fStopWatchAlcinoe.Elapsed.TotalMilliseconds]));
  {$ENDIF}
end;

{*********************************************************************************}
function TALDUnitXTestStringUtils.UnicodeRandomStr(const aLength: Longint): String;
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

{***********************************************************}
procedure TALDUnitXTestStringUtils.TestALBase64EncodeStringA;
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

{******************************************************}
procedure TALDUnitXTestStringUtils.TestAlposIgnoreCaseA;
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

{******************************************************}
procedure TALDUnitXTestStringUtils.TestAlposIgnoreCaseW;
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

{*****************************************************}
procedure TALDUnitXTestStringUtils.TestALTryStrToDateA;
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

{*****************************************************}
procedure TALDUnitXTestStringUtils.TestALTryStrToTimeA;
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

{*********************************************************}
procedure TALDUnitXTestStringUtils.TestALTryStrToDateTimeA;
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
  CheckExecutionTime(0.9{ARatio});
end;

{**************************************************}
procedure TALDUnitXTestStringUtils.TestALDateToStrA;
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

{**************************************************}
procedure TALDUnitXTestStringUtils.TestALTimeToStrA;
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

{******************************************************}
procedure TALDUnitXTestStringUtils.TestALDateTimeToStrA;
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

{*******************************************************}
procedure TALDUnitXTestStringUtils.TestALFormatDateTimeA;
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

{*************************************************}
procedure TALDUnitXTestStringUtils.TestALIntToStrA;
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

{*************************************************}
procedure TALDUnitXTestStringUtils.TestALStrToIntA;
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

{**************************************************}
procedure TALDUnitXTestStringUtils.TestALStrToUIntA;
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

{***************************************************}
procedure TALDUnitXTestStringUtils.TestALStrToInt64A;
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

{****************************************************}
procedure TALDUnitXTestStringUtils.TestALStrToUInt64A;
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

{***************************************************}
procedure TALDUnitXTestStringUtils.TestALFloatToStrA;
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

{****************************************************}
procedure TALDUnitXTestStringUtils.TestALFloatToStrFA;
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

{***************************************************}
procedure TALDUnitXTestStringUtils.TestALStrToFloatA;
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

{**************************************************}
procedure TALDUnitXTestStringUtils.TestALCurrToStrA;
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

{**************************************************}
procedure TALDUnitXTestStringUtils.TestALStrToCurrA;
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

{****************************************************}
procedure TALDUnitXTestStringUtils.TestALFormatFloatA;
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

{***************************************************}
procedure TALDUnitXTestStringUtils.TestALFormatCurrA;
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

{***********************************************}
procedure TALDUnitXTestStringUtils.TestALFormatA;
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

{****************************************************}
procedure TALDUnitXTestStringUtils.TestALUTF8CharSize;
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

{*******************************************************}
procedure TALDUnitXTestStringUtils.TestALUTF8CharToUtf16;
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

{******************************************************}
procedure TALDUnitXTestStringUtils.TestALStringBuilderA;
begin
  // Test default constructor
  var SB := TALStringBuilderA.Create;
  try
    Assert.AreEqual(16, SB.Capacity, 'Default capacity should be 16');
    Assert.AreEqual(0, SB.Length, 'Initial length should be 0');
    Assert.AreEqual(MaxInt, SB.MaxCapacity, 'Max capacity should be MaxInt');
    Assert.AreEqual(ansiString(''), SB.ToString, 'Initial string should be empty');
  finally
    SB.Free;
  end;

  // Test constructor with capacity
  SB := TALStringBuilderA.Create(100);
  try
    Assert.AreEqual(100, SB.Capacity, 'Capacity should be 100');
    Assert.AreEqual(0, SB.Length, 'Initial length should be 0');
    Assert.AreEqual(MaxInt, SB.MaxCapacity, 'Max capacity should be MaxInt');
  finally
    SB.Free;
  end;

  // Test constructor with value
  SB := TALStringBuilderA.Create('Hello');
  try
    Assert.IsTrue(SB.Capacity >= 5, 'Capacity should be at least 5');
    Assert.AreEqual(5, SB.Length, 'Length should be 5');
    Assert.AreEqual(ansiString('Hello'), SB.ToString, 'String should be "Hello"');
  finally
    SB.Free;
  end;

  // Test constructor with capacity and max capacity
  SB := TALStringBuilderA.Create(50, 100);
  try
    Assert.AreEqual(50, SB.Capacity, 'Capacity should be 50');
    Assert.AreEqual(0, SB.Length, 'Initial length should be 0');
    Assert.AreEqual(100, SB.MaxCapacity, 'Max capacity should be 100');
  finally
    SB.Free;
  end;

  // Test constructor with value and capacity
  SB := TALStringBuilderA.Create('Test', 20);
  try
    Assert.AreEqual(20, SB.Capacity, 'Capacity should be 20');
    Assert.AreEqual(4, SB.Length, 'Length should be 4');
    Assert.AreEqual(ansiString('Test'), SB.ToString, 'String should be "Test"');
  finally
    SB.Free;
  end;

  // Test constructor with value, start index, length, and capacity
  SB := TALStringBuilderA.Create('HelloWorld', 1, 5, 10);
  try
    Assert.AreEqual(10, SB.Capacity, 'Capacity should be 10');
    Assert.AreEqual(5, SB.Length, 'Length should be 5');
    Assert.AreEqual(ansiString('Hello'), SB.ToString, 'String should be "Hello"');
  finally
    SB.Free;
  end;

  // Test Clear
  SB := TALStringBuilderA.Create('Test');
  try
    SB.Clear;
    Assert.AreEqual(16, SB.Capacity, 'Capacity should be reset to 16');
    Assert.AreEqual(0, SB.Length, 'Length should be 0');
    Assert.AreEqual(ansiString(''), SB.ToString, 'String should be empty');
  finally
    SB.Free;
  end;

  // Test EnsureCapacity
  SB := TALStringBuilderA.Create;
  try
    Assert.AreEqual(16, SB.EnsureCapacity(10), 'EnsureCapacity should return 16');
    Assert.AreEqual(16, SB.Capacity, 'Capacity should remain 16');
    Assert.AreEqual(50, SB.EnsureCapacity(50), 'EnsureCapacity should return 50');
    Assert.AreEqual(50, SB.Capacity, 'Capacity should be 50');
  finally
    SB.Free;
  end;

  // Test Append(Char)
  SB := TALStringBuilderA.Create;
  try
    SB.Append('A');
    Assert.AreEqual(1, SB.Length, 'Length should be 1');
    Assert.AreEqual(ansiString('A'), SB.ToString, 'String should be "A"');
  finally
    SB.Free;
  end;

  // Test Append(string)
  SB := TALStringBuilderA.Create;
  try
    SB.Append('Hello');
    Assert.AreEqual(5, SB.Length, 'Length should be 5');
    Assert.AreEqual(ansiString('Hello'), SB.ToString, 'String should be "Hello"');
    SB.Append('World');
    Assert.AreEqual(10, SB.Length, 'Length should be 10');
    Assert.AreEqual(ansiString('HelloWorld'), SB.ToString, 'String should be "HelloWorld"');
  finally
    SB.Free;
  end;

  // Test Append(Char, RepeatCount)
  SB := TALStringBuilderA.Create;
  try
    SB.Append('A', 3);
    Assert.AreEqual(3, SB.Length, 'Length should be 3');
    Assert.AreEqual(ansiString('AAA'), SB.ToString, 'String should be "AAA"');
  finally
    SB.Free;
  end;

  // Test Append(string, StartIndex, Count)
  SB := TALStringBuilderA.Create;
  try
    SB.Append('HelloWorld', 6, 5);
    Assert.AreEqual(5, SB.Length, 'Length should be 5');
    Assert.AreEqual(ansiString('World'), SB.ToString, 'String should be "World"');
  finally
    SB.Free;
  end;

  // Test AppendLine
  SB := TALStringBuilderA.Create;
  try
    SB.AppendLine;
    Assert.AreEqual(Length(sLineBreak), SB.Length, 'Length should match sLineBreak');
    Assert.AreEqual(sLineBreak, SB.ToString, 'String should be sLineBreak');
  finally
    SB.Free;
  end;

  // Test AppendLine(string)
  SB := TALStringBuilderA.Create;
  try
    SB.AppendLine('Hello');
    Assert.AreEqual(5 + Length(sLineBreak), SB.Length, 'Length should be 5 + sLineBreak');
    Assert.AreEqual(ansiString('Hello' + sLineBreak), SB.ToString, 'String should be "Hello" + sLineBreak');
  finally
    SB.Free;
  end;

  // Test ToString
  SB := TALStringBuilderA.Create('Test');
  try
    Assert.AreEqual(ansiString('Test'), SB.ToString, 'ToString should return "Test"');
    Assert.IsTrue(SB.Capacity >= 4, 'Capacity should remain unchanged');
  finally
    SB.Free;
  end;

  // Test ToString(UpdateCapacity)
  SB := TALStringBuilderA.Create(20);
  try
    SB.Append('Test');
    Assert.AreEqual(ansiString('Test'), SB.ToString(True), 'ToString should return "Test"');
    Assert.AreEqual(4, SB.Capacity, 'Capacity should be updated to 4');
  finally
    SB.Free;
  end;

  // Test Capacity
  SB := TALStringBuilderA.Create;
  try
    SB.Capacity := 100;
    Assert.AreEqual(100, SB.Capacity, 'Capacity should be 100');
  finally
    SB.Free;
  end;

  // Test Length
  SB := TALStringBuilderA.Create;
  try
    SB.Append('Test');
    SB.Length := 2;
    Assert.AreEqual(2, SB.Length, 'Length should be 2');
    Assert.AreEqual(ansiString('Te'), SB.ToString, 'String should be "Te"');
  finally
    SB.Free;
  end;

  // Test MaxCapacity
  SB := TALStringBuilderA.Create(50, 100);
  try
    Assert.AreEqual(100, SB.MaxCapacity, 'MaxCapacity should be 100');
  finally
    SB.Free;
  end;

  // Test ExpandCapacity when Length = Capacity
  SB := TALStringBuilderA.Create(10);
  try
    SB.Append('1234567890'); // Length = 10, Capacity = 10
    Assert.AreEqual(10, SB.Length, 'Length should be 10');
    Assert.AreEqual(10, SB.Capacity, 'Capacity should be 10');
    SB.Append('A'); // Triggers ExpandCapacity
    Assert.AreEqual(11, SB.Length, 'Length should be 11');
    Assert.AreEqual(15, SB.Capacity, 'Capacity should be 15 (10 * 3 / 2)');
    Assert.AreEqual(ansiString('1234567890A'), SB.ToString, 'String should be "1234567890A"');
  finally
    SB.Free;
  end;

  // Test ExpandCapacity when Length > Capacity
  SB := TALStringBuilderA.Create(5);
  try
    SB.Append('123456'); // Length = 6, Capacity = 5, triggers ExpandCapacity
    Assert.AreEqual(6, SB.Length, 'Length should be 6');
    Assert.AreEqual(7, SB.Capacity, 'Capacity should be 7 (5 * 3) div 2');
    Assert.AreEqual(ansiString('123456'), SB.ToString, 'String should be "123456"');
  finally
    SB.Free;
  end;

  // Test ExpandCapacity near MaxCapacity
  SB := TALStringBuilderA.Create(10, 15);
  try
    SB.Append('1234567890'); // Length = 10, Capacity = 10
    Assert.AreEqual(10, SB.Length, 'Length should be 10');
    Assert.AreEqual(10, SB.Capacity, 'Capacity should be 10');
    SB.Append('12345'); // Length = 15, triggers ExpandCapacity
    Assert.AreEqual(15, SB.Length, 'Length should be 15');
    Assert.AreEqual(15, SB.Capacity, 'Capacity should be 15 (limited by MaxCapacity)');
    Assert.AreEqual(ansiString('123456789012345'), SB.ToString, 'String should be "123456789012345"');
  finally
    SB.Free;
  end;
end;

{******************************************************}
procedure TALDUnitXTestStringUtils.TestALStringBuilderW;
begin
  // Test default constructor
  var SB := TALStringBuilderW.Create;
  try
    Assert.AreEqual(16, SB.Capacity, 'Default capacity should be 16');
    Assert.AreEqual(0, SB.Length, 'Initial length should be 0');
    Assert.AreEqual(MaxInt, SB.MaxCapacity, 'Max capacity should be MaxInt');
    Assert.AreEqual('', SB.ToString, 'Initial string should be empty');
  finally
    SB.Free;
  end;

  // Test constructor with capacity
  SB := TALStringBuilderW.Create(100);
  try
    Assert.AreEqual(100, SB.Capacity, 'Capacity should be 100');
    Assert.AreEqual(0, SB.Length, 'Initial length should be 0');
    Assert.AreEqual(MaxInt, SB.MaxCapacity, 'Max capacity should be MaxInt');
  finally
    SB.Free;
  end;

  // Test constructor with value
  SB := TALStringBuilderW.Create('Hello');
  try
    Assert.IsTrue(SB.Capacity >= 5, 'Capacity should be at least 5');
    Assert.AreEqual(5, SB.Length, 'Length should be 5');
    Assert.AreEqual('Hello', SB.ToString, 'String should be "Hello"');
  finally
    SB.Free;
  end;

  // Test constructor with capacity and max capacity
  SB := TALStringBuilderW.Create(50, 100);
  try
    Assert.AreEqual(50, SB.Capacity, 'Capacity should be 50');
    Assert.AreEqual(0, SB.Length, 'Initial length should be 0');
    Assert.AreEqual(100, SB.MaxCapacity, 'Max capacity should be 100');
  finally
    SB.Free;
  end;

  // Test constructor with value and capacity
  SB := TALStringBuilderW.Create('Test', 20);
  try
    Assert.AreEqual(20, SB.Capacity, 'Capacity should be 20');
    Assert.AreEqual(4, SB.Length, 'Length should be 4');
    Assert.AreEqual('Test', SB.ToString, 'String should be "Test"');
  finally
    SB.Free;
  end;

  // Test constructor with value, start index, length, and capacity
  SB := TALStringBuilderW.Create('HelloWorld', 1, 5, 10);
  try
    Assert.AreEqual(10, SB.Capacity, 'Capacity should be 10');
    Assert.AreEqual(5, SB.Length, 'Length should be 5');
    Assert.AreEqual('Hello', SB.ToString, 'String should be "Hello"');
  finally
    SB.Free;
  end;

  // Test Clear
  SB := TALStringBuilderW.Create('Test');
  try
    SB.Clear;
    Assert.AreEqual(16, SB.Capacity, 'Capacity should be reset to 16');
    Assert.AreEqual(0, SB.Length, 'Length should be 0');
    Assert.AreEqual('', SB.ToString, 'String should be empty');
  finally
    SB.Free;
  end;

  // Test EnsureCapacity
  SB := TALStringBuilderW.Create;
  try
    Assert.AreEqual(16, SB.EnsureCapacity(10), 'EnsureCapacity should return 16');
    Assert.AreEqual(16, SB.Capacity, 'Capacity should remain 16');
    Assert.AreEqual(50, SB.EnsureCapacity(50), 'EnsureCapacity should return 50');
    Assert.AreEqual(50, SB.Capacity, 'Capacity should be 50');
  finally
    SB.Free;
  end;

  // Test Append(Char)
  SB := TALStringBuilderW.Create;
  try
    SB.Append('A');
    Assert.AreEqual(1, SB.Length, 'Length should be 1');
    Assert.AreEqual('A', SB.ToString, 'String should be "A"');
  finally
    SB.Free;
  end;

  // Test Append(string)
  SB := TALStringBuilderW.Create;
  try
    SB.Append('Hello');
    Assert.AreEqual(5, SB.Length, 'Length should be 5');
    Assert.AreEqual('Hello', SB.ToString, 'String should be "Hello"');
    SB.Append('World');
    Assert.AreEqual(10, SB.Length, 'Length should be 10');
    Assert.AreEqual('HelloWorld', SB.ToString, 'String should be "HelloWorld"');
  finally
    SB.Free;
  end;

  // Test Append(Char, RepeatCount)
  SB := TALStringBuilderW.Create;
  try
    SB.Append('A', 3);
    Assert.AreEqual(3, SB.Length, 'Length should be 3');
    Assert.AreEqual('AAA', SB.ToString, 'String should be "AAA"');
  finally
    SB.Free;
  end;

  // Test Append(string, StartIndex, Count)
  SB := TALStringBuilderW.Create;
  try
    SB.Append('HelloWorld', 6, 5);
    Assert.AreEqual(5, SB.Length, 'Length should be 5');
    Assert.AreEqual('World', SB.ToString, 'String should be "World"');
  finally
    SB.Free;
  end;

  // Test AppendLine
  SB := TALStringBuilderW.Create;
  try
    SB.AppendLine;
    Assert.AreEqual(Length(sLineBreak), SB.Length, 'Length should match sLineBreak');
    Assert.AreEqual(sLineBreak, SB.ToString, 'String should be sLineBreak');
  finally
    SB.Free;
  end;

  // Test AppendLine(string)
  SB := TALStringBuilderW.Create;
  try
    SB.AppendLine('Hello');
    Assert.AreEqual(5 + Length(sLineBreak), SB.Length, 'Length should be 5 + sLineBreak');
    Assert.AreEqual('Hello' + sLineBreak, SB.ToString, 'String should be "Hello" + sLineBreak');
  finally
    SB.Free;
  end;

  // Test ToString
  SB := TALStringBuilderW.Create('Test');
  try
    Assert.AreEqual('Test', SB.ToString, 'ToString should return "Test"');
    Assert.IsTrue(SB.Capacity >= 4, 'Capacity should remain unchanged');
  finally
    SB.Free;
  end;

  // Test ToString(UpdateCapacity)
  SB := TALStringBuilderW.Create(20);
  try
    SB.Append('Test');
    Assert.AreEqual('Test', SB.ToString(True), 'ToString should return "Test"');
    Assert.AreEqual(4, SB.Capacity, 'Capacity should be updated to 4');
  finally
    SB.Free;
  end;

  // Test Capacity
  SB := TALStringBuilderW.Create;
  try
    SB.Capacity := 100;
    Assert.AreEqual(100, SB.Capacity, 'Capacity should be 100');
  finally
    SB.Free;
  end;

  // Test Length
  SB := TALStringBuilderW.Create;
  try
    SB.Append('Test');
    SB.Length := 2;
    Assert.AreEqual(2, SB.Length, 'Length should be 2');
    Assert.AreEqual('Te', SB.ToString, 'String should be "Te"');
  finally
    SB.Free;
  end;

  // Test MaxCapacity
  SB := TALStringBuilderW.Create(50, 100);
  try
    Assert.AreEqual(100, SB.MaxCapacity, 'MaxCapacity should be 100');
  finally
    SB.Free;
  end;

  // Test ExpandCapacity when Length = Capacity
  SB := TALStringBuilderW.Create(10);
  try
    SB.Append('1234567890'); // Length = 10, Capacity = 10
    Assert.AreEqual(10, SB.Length, 'Length should be 10');
    Assert.AreEqual(10, SB.Capacity, 'Capacity should be 10');
    SB.Append('A'); // Triggers ExpandCapacity
    Assert.AreEqual(11, SB.Length, 'Length should be 11');
    Assert.AreEqual(15, SB.Capacity, 'Capacity should be 15 (10 * 3 / 2)');
    Assert.AreEqual('1234567890A', SB.ToString, 'String should be "1234567890A"');
  finally
    SB.Free;
  end;

  // Test ExpandCapacity when Length > Capacity
  SB := TALStringBuilderW.Create(5);
  try
    SB.Append('123456'); // Length = 6, Capacity = 5, triggers ExpandCapacity
    Assert.AreEqual(6, SB.Length, 'Length should be 6');
    Assert.AreEqual(7, SB.Capacity, 'Capacity should be 7 (5 * 3) div 2');
    Assert.AreEqual('123456', SB.ToString, 'String should be "123456"');
  finally
    SB.Free;
  end;

  // Test ExpandCapacity near MaxCapacity
  SB := TALStringBuilderW.Create(10, 15);
  try
    SB.Append('1234567890'); // Length = 10, Capacity = 10
    Assert.AreEqual(10, SB.Length, 'Length should be 10');
    Assert.AreEqual(10, SB.Capacity, 'Capacity should be 10');
    SB.Append('12345'); // Length = 15, triggers ExpandCapacity
    Assert.AreEqual(15, SB.Length, 'Length should be 15');
    Assert.AreEqual(15, SB.Capacity, 'Capacity should be 15 (limited by MaxCapacity)');
    Assert.AreEqual('123456789012345', SB.ToString, 'String should be "123456789012345"');
  finally
    SB.Free;
  end;
end;

{******************************************************}
procedure TALDUnitXTestStringUtils.TestALPercentEncodeA;

  // Build a TSysCharSet from characters (AnsiString for convenience).
  function MakeSet(const S: AnsiString): TSysCharSet;
  var
    C: AnsiChar;
  begin
    Result := [];
    for C in S do
      Include(Result, C);
  end;

  // RFC 3986 unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
  function RFC3986_Unreserved: TSysCharSet;
  var
    C: AnsiChar;
  begin
    Result := [];
    for C := 'A' to 'Z' do Include(Result, C);
    for C := 'a' to 'z' do Include(Result, C);
    for C := '0' to '9' do Include(Result, C);
    Include(Result, '-'); Include(Result, '.'); Include(Result, '_'); Include(Result, '~');
  end;

var
  Safe: TSysCharSet;
  S, E: AnsiString;
begin
  // 1) Empty string
  Safe := RFC3986_Unreserved;
  Assert.AreEqual<AnsiString>(ansiString(''), ALPercentEncode(ansiString(''), Safe, False));

  // 2) All-safe short-circuit (unchanged)
  S := 'AZaz09-._~';
  Assert.AreEqual<AnsiString>(S, ALPercentEncode(S, Safe, False));

  // 3) Space handling (ASpacesAsPlus = False -> %20)
  Assert.AreEqual<AnsiString>(ansiString('hello%20world'), ALPercentEncode(ansiString('hello world'), Safe, False));

  // 4) Space handling (ASpacesAsPlus = True -> '+')
  Assert.AreEqual<AnsiString>(ansiString('hello+world'), ALPercentEncode(ansiString('hello world'), Safe, True));

  // 5) Literal plus with form-mode: '+' must be encoded to %2B (space still '+')
  Assert.AreEqual<AnsiString>(ansiString('a%2Bb+c'), ALPercentEncode(ansiString('a+b c'), Safe, True));

  // 6) Reserved characters (none are in the safe set -> all percent-encoded)
  S := ansiString(':/?#[]@!$&''()*+,;=');
  E := ansiString('%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D');
  Assert.AreEqual<AnsiString>(E, ALPercentEncode(S, Safe, False));

  // 7) Percent sign itself (should encode to %25); plus false so space -> %20
  Assert.AreEqual<AnsiString>(ansiString('100%25%20sure'), ALPercentEncode(ansiString('100% sure'), Safe, False));

  // 8) Non-ASCII byte (AnsiString): 'caf' + #$E9 (é in Latin-1) => caf%E9
  S := AnsiString('café');
  Assert.AreEqual<AnsiString>('caf%C3%A9', ALPercentEncode(S, Safe, False));

  // 9) Embedded #0 byte should be encoded to %00 and NOT terminate processing
  S := AnsiString('A') + AnsiChar(#0) + AnsiString('B');
  Assert.AreEqual<AnsiString>(ansiString('A%00B'), ALPercentEncode(S, Safe, False));

  // 10) Custom safe set including space and slash (e.g., path-ish tolerance).
  //     If ' ' is safe, ASpacesAsPlus must not apply; '/' also left as-is.
  Safe := RFC3986_Unreserved + MakeSet(' /');
  Assert.AreEqual<AnsiString>(ansiString('a b/c'), ALPercentEncode(ansiString('a b/c'), Safe, True));

  // 11) All-unsafe bytes (control chars) -> all percent-encoded
  S := AnsiChar(#1) + AnsiChar(#2);
  Assert.AreEqual<AnsiString>(ansiString('%01%02'), ALPercentEncode(S, RFC3986_Unreserved, False));

  // 12) Mixed sample covering multiple categories at once
  //     "Az-._~ 123%+/é" with form-mode:
  //       space -> '+'
  //       '%' -> %25
  //       '+' -> %2B
  //       '/' -> %2F
  //       'é' (#$E9) -> %E9
  S := AnsiString('Az-._~ 123%+/') + AnsiChar(#$E9);
  E := 'Az-._~+123%25%2B%2F%E9';
  Assert.AreEqual<AnsiString>(E, ALPercentEncode(S, RFC3986_Unreserved, True));
end;

{******************************************************}
procedure TALDUnitXTestStringUtils.TestALPercentEncodeW;

  // Build a TSysCharSet from characters (String for convenience).
  function MakeSet(const S: String): TSysCharSet;
  var
    C: AnsiChar;
  begin
    Result := [];
    for C in AnsiString(S) do
      Include(Result, C);
  end;

  // RFC 3986 unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
  function RFC3986_Unreserved: TSysCharSet;
  var
    C: AnsiChar;
  begin
    Result := [];
    for C := 'A' to 'Z' do Include(Result, C);
    for C := 'a' to 'z' do Include(Result, C);
    for C := '0' to '9' do Include(Result, C);
    Include(Result, '-'); Include(Result, '.'); Include(Result, '_'); Include(Result, '~');
  end;

var
  Safe: TSysCharSet;
  S, E: String;
begin
  // 1) Empty string
  Safe := RFC3986_Unreserved;
  Assert.AreEqual<String>(String(''), ALPercentEncode(String(''), Safe, False));

  // 2) All-safe short-circuit (unchanged)
  S := 'AZaz09-._~';
  Assert.AreEqual<String>(S, ALPercentEncode(S, Safe, False));

  // 3) Space handling (ASpacesAsPlus = False -> %20)
  Assert.AreEqual<String>(String('hello%20world'), ALPercentEncode(String('hello world'), Safe, False));

  // 4) Space handling (ASpacesAsPlus = True -> '+')
  Assert.AreEqual<String>(String('hello+world'), ALPercentEncode(String('hello world'), Safe, True));

  // 5) Literal plus with form-mode: '+' must be encoded to %2B (space still '+')
  Assert.AreEqual<String>(String('a%2Bb+c'), ALPercentEncode(String('a+b c'), Safe, True));

  // 6) Reserved characters (none are in the safe set -> all percent-encoded)
  S := String(':/?#[]@!$&''()*+,;=');
  E := String('%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D');
  Assert.AreEqual<String>(E, ALPercentEncode(S, Safe, False));

  // 7) Percent sign itself (should encode to %25); plus false so space -> %20
  Assert.AreEqual<String>(String('100%25%20sure'), ALPercentEncode(String('100% sure'), Safe, False));

  // 8) Non-ASCII byte (String): 'caf' + #$E9 (é in Latin-1) => caf%E9
  S := String('café');
  Assert.AreEqual<String>('caf%C3%A9', ALPercentEncode(S, Safe, False));

  // 9) Embedded #0 byte should be encoded to %00 and NOT terminate processing
  S := String('A') + AnsiChar(#0) + String('B');
  Assert.AreEqual<String>(String('A%00B'), ALPercentEncode(S, Safe, False));

  // 10) Custom safe set including space and slash (e.g., path-ish tolerance).
  //     If ' ' is safe, ASpacesAsPlus must not apply; '/' also left as-is.
  Safe := RFC3986_Unreserved + MakeSet(' /');
  Assert.AreEqual<String>(String('a b/c'), ALPercentEncode(String('a b/c'), Safe, True));

  // 11) All-unsafe bytes (control chars) -> all percent-encoded
  S := AnsiChar(#1) + AnsiChar(#2);
  Assert.AreEqual<String>(String('%01%02'), ALPercentEncode(S, RFC3986_Unreserved, False));

  // 12) Mixed sample covering multiple categories at once
  //     "Az-._~ 123%+/é" with form-mode:
  //       space -> '+'
  //       '%' -> %25
  //       '+' -> %2B
  //       '/' -> %2F
  //       'é' (#$E9) -> %E9
  S := String('Az-._~ 123%+/é');
  E := 'Az-._~+123%25%2B%2F%C3%A9';
  Assert.AreEqual<String>(E, ALPercentEncode(S, RFC3986_Unreserved, True));
end;

{******************************************************}
procedure TALDUnitXTestStringUtils.TestALPercentDecodeA;
begin
  // Test case 1: Basic percent-encoded ASCII characters
  Assert.AreEqual(ALPercentDecode(AnsiString('Hello%20World')), AnsiString('Hello World'), 'Basic percent decode (%20 to space) failed');
  Assert.AreEqual(ALPercentDecode(AnsiString('%7B%7D')), AnsiString('{}'), 'Percent decode of braces (%7B%7D) failed');

  // Test case 2: Non-ASCII percent-encoded characters (UTF-8)
  Assert.AreEqual(ALPercentDecode(AnsiString('caf%C3%A9')), AnsiString('café'), 'Percent decode of UTF-8 é (%C3%A9) failed');
  Assert.AreEqual(ALPercentDecode(AnsiString('%E2%82%AC')), AnsiString('€'), 'Percent decode of Euro symbol (%E2%82%AC) failed');

  // Test case 3: Plus sign handling with APlusAsSpaces = True
  Assert.AreEqual(ALPercentDecode(AnsiString('Hello+World'), True), AnsiString('Hello World'), 'Plus to space with APlusAsSpaces=True failed');
  Assert.AreEqual(ALPercentDecode(AnsiString('Hello+World'), False), AnsiString('Hello+World'), 'Plus as literal with APlusAsSpaces=False failed');

  // Test case 4: Mixed encoded and unencoded characters
  Assert.AreEqual(ALPercentDecode(AnsiString('Hello%20World%21%40caf%C3%A9')), AnsiString('Hello World!@café'), 'Mixed encoded/unencoded string failed');

  // Test case 5: Invalid percent encodings
  Assert.AreEqual(ALPercentDecode(AnsiString('Hello%GGWorld')), AnsiString('Hello%GGWorld'), 'Invalid percent encoding (%GG) should be unchanged');
  Assert.AreEqual(ALPercentDecode(AnsiString('Hello%1')), AnsiString('Hello%1'), 'Incomplete percent encoding (%1) should be unchanged');
  Assert.AreEqual(ALPercentDecode(AnsiString('Hello%')), AnsiString('Hello%'), 'Lone percent sign (%) should be unchanged');

  // Test case 6: Empty string
  Assert.AreEqual(ALPercentDecode(AnsiString('')), AnsiString(''), 'Empty string decode failed');

  // Test case 7: String with only percent-encoded characters
  Assert.AreEqual(ALPercentDecode(AnsiString('%25%26%27')), AnsiString('%&'''), 'Fully percent-encoded string failed');

  // Test case 8: Plus sign with percent-encoded characters
  Assert.AreEqual(ALPercentDecode(AnsiString('Hello+%2B+World'), True), AnsiString('Hello + World'), 'Plus and %2B with APlusAsSpaces=True failed');
  Assert.AreEqual(ALPercentDecode(AnsiString('Hello+%2B+World'), False), AnsiString('Hello+++World'), 'Plus and %2B with APlusAsSpaces=False failed');

  // Test case 9: Edge case - multiple consecutive percent encodings
  Assert.AreEqual(ALPercentDecode(AnsiString('%20%20%20')), AnsiString('   '), 'Multiple consecutive spaces (%20%20%20) failed');

  // Test case 10: Edge case - string with no percent encoding
  Assert.AreEqual(ALPercentDecode(AnsiString('HelloWorld')), AnsiString('HelloWorld'), 'String with no percent encoding failed');

  // Test case 11: Edge case - percent-encoded plus sign
  Assert.AreEqual(ALPercentDecode(AnsiString('%2B'), True), AnsiString('+'), 'Percent-encoded plus (%2B) with APlusAsSpaces=True failed');
  Assert.AreEqual(ALPercentDecode(AnsiString('%2B'), False), AnsiString('+'), 'Percent-encoded plus (%2B) with APlusAsSpaces=False failed');

  // Test case 12: Invalid UTF-8 percent-encoded sequences
  Assert.AreEqual(ALPercentDecode(AnsiString('abc%C3def')), AnsiString('abc')+AnsiString(#$C3)+AnsiString('def'), 'Incomplete UTF-8 sequence (%C3) in "abc%C3def" should be unchanged');
  Assert.AreEqual(ALPercentDecode(AnsiString('abc%C3%XYdef')), AnsiString('abc')+AnsiString(#$C3)+AnsiString('%XYdef'), 'Invalid UTF-8 continuation byte (%C3%XY) should be unchanged');
  Assert.AreEqual(ALPercentDecode(AnsiString('%FF')), AnsiString(#$FF), 'Invalid UTF-8 byte (%FF) should be unchanged');
  Assert.AreEqual(ALPercentDecode(AnsiString('caf%C3%A9%GGdef')), AnsiString('café%GGdef'), 'Mixed valid UTF-8 (%C3%A9) and invalid (%GG) failed');
end;

{******************************************************}
procedure TALDUnitXTestStringUtils.TestALPercentDecodeW;
begin
  // Test case 1: Basic percent-encoded ASCII characters
  Assert.AreEqual(ALPercentDecode(String('Hello%20World')), String('Hello World'), 'Basic percent decode (%20 to space) failed');
  Assert.AreEqual(ALPercentDecode(String('%7B%7D')), String('{}'), 'Percent decode of braces (%7B%7D) failed');

  // Test case 2: Non-ASCII percent-encoded characters (UTF-8)
  Assert.AreEqual(ALPercentDecode(String('caf%C3%A9')), String('café'), 'Percent decode of UTF-8 é (%C3%A9) failed');
  Assert.AreEqual(ALPercentDecode(String('%E2%82%AC')), String('€'), 'Percent decode of Euro symbol (%E2%82%AC) failed');

  // Test case 3: Plus sign handling with APlusAsSpaces = True
  Assert.AreEqual(ALPercentDecode(String('Hello+World'), True), String('Hello World'), 'Plus to space with APlusAsSpaces=True failed');
  Assert.AreEqual(ALPercentDecode(String('Hello+World'), False), String('Hello+World'), 'Plus as literal with APlusAsSpaces=False failed');

  // Test case 4: Mixed encoded and unencoded characters
  Assert.AreEqual(ALPercentDecode(String('Hello%20World%21%40caf%C3%A9')), String('Hello World!@café'), 'Mixed encoded/unencoded string failed');

  // Test case 5: Invalid percent encodings
  Assert.AreEqual(ALPercentDecode(String('Hello%GGWorld')), String('Hello%GGWorld'), 'Invalid percent encoding (%GG) should be unchanged');
  Assert.AreEqual(ALPercentDecode(String('Hello%1')), String('Hello%1'), 'Incomplete percent encoding (%1) should be unchanged');
  Assert.AreEqual(ALPercentDecode(String('Hello%')), String('Hello%'), 'Lone percent sign (%) should be unchanged');

  // Test case 6: Empty string
  Assert.AreEqual(ALPercentDecode(String('')), String(''), 'Empty string decode failed');

  // Test case 7: String with only percent-encoded characters
  Assert.AreEqual(ALPercentDecode(String('%25%26%27')), String('%&'''), 'Fully percent-encoded string failed');

  // Test case 8: Plus sign with percent-encoded characters
  Assert.AreEqual(ALPercentDecode(String('Hello+%2B+World'), True), String('Hello + World'), 'Plus and %2B with APlusAsSpaces=True failed');
  Assert.AreEqual(ALPercentDecode(String('Hello+%2B+World'), False), String('Hello+++World'), 'Plus and %2B with APlusAsSpaces=False failed');

  // Test case 9: Edge case - multiple consecutive percent encodings
  Assert.AreEqual(ALPercentDecode(String('%20%20%20')), String('   '), 'Multiple consecutive spaces (%20%20%20) failed');

  // Test case 10: Edge case - string with no percent encoding
  Assert.AreEqual(ALPercentDecode(String('HelloWorld')), String('HelloWorld'), 'String with no percent encoding failed');

  // Test case 11: Edge case - percent-encoded plus sign
  Assert.AreEqual(ALPercentDecode(String('%2B'), True), String('+'), 'Percent-encoded plus (%2B) with APlusAsSpaces=True failed');
  Assert.AreEqual(ALPercentDecode(String('%2B'), False), String('+'), 'Percent-encoded plus (%2B) with APlusAsSpaces=False failed');

  // Test case 12: Invalid UTF-8 percent-encoded sequences
  Assert.AreEqual(ALPercentDecode(String('abc%C3def')), String('abc')+String(#$C3)+String('def'), 'Incomplete UTF-8 sequence (%C3) in "abc%C3def" should be unchanged');
  Assert.AreEqual(ALPercentDecode(String('abc%C3%XYdef')), String('abc')+String(#$C3)+String('%XYdef'), 'Invalid UTF-8 continuation byte (%C3%XY) should be unchanged');
  Assert.AreEqual(ALPercentDecode(String('%FF')), String(#$FF), 'Invalid UTF-8 byte (%FF) should be unchanged');
  Assert.AreEqual(ALPercentDecode(String('caf%C3%A9%GGdef')), String('café%GGdef'), 'Mixed valid UTF-8 (%C3%A9) and invalid (%GG) failed');
end;

{********************************************************}
procedure TALDUnitXTestStringUtils.ALExtractHeaderFieldsA;
var
  Strings: TALNVStringListA;
  Content: AnsiString;
begin
  Strings := TALNVStringListA.Create;
  try
    // Test 1: Basic name=value without quotes, separator ;
    Content := 'name=value; another=val2';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 1 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 1 name0');
    Assert.AreEqual(AnsiString('value'), Strings.ValueFromIndex[0], 'Test 1 value0');
    Assert.AreEqual(AnsiString('another'), Strings.Names[1], 'Test 1 name1');
    Assert.AreEqual(AnsiString('val2'), Strings.ValueFromIndex[1], 'Test 1 value1');
    Strings.Clear;

    // Test 2: Empty content
    Content := '';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(0, Strings.Count, 'Test 2 count');
    Strings.Clear;

    // Test 3: Nil content
    ALExtractHeaderFields([';'], [' ', #9], [], nil, Strings, False);
    Assert.AreEqual(0, Strings.Count, 'Test 3 count');
    Strings.Clear;

    // Test 4: Single name without value
    Content := 'name';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(1, Strings.Count, 'Test 4 count');
    Assert.AreEqual(AnsiString('name'), Strings[0], 'Test 4 token');
    Strings.Clear;

    // Test 5: Multiple names without values
    Content := 'name1;name2;name3;';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(3, Strings.Count, 'Test 5 count');
    Assert.AreEqual(AnsiString('name1'), Strings[0], 'Test 5 tok0');
    Assert.AreEqual(AnsiString('name2'), Strings[1], 'Test 5 tok1');
    Assert.AreEqual(AnsiString('name3'), Strings[2], 'Test 5 tok2');
    Strings.Clear;

    // Test 6: Different field separator (comma)
    Content := 'name=value, another=val2';
    ALExtractHeaderFields([','], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 6 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 6 name0');
    Assert.AreEqual(AnsiString('value'), Strings.ValueFromIndex[0], 'Test 6 value0');
    Assert.AreEqual(AnsiString('another'), Strings.Names[1], 'Test 6 name1');
    Assert.AreEqual(AnsiString('val2'), Strings.ValueFromIndex[1], 'Test 6 value1');
    Strings.Clear;

    // Test 7: Multiple separators in set ; and ,
    Content := 'a=b; c=d, e=f';
    ALExtractHeaderFields([';', ','], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(3, Strings.Count, 'Test 7 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 7 name0');
    Assert.AreEqual(AnsiString('b'), Strings.ValueFromIndex[0], 'Test 7 value0');
    Assert.AreEqual(AnsiString('c'), Strings.Names[1], 'Test 7 name1');
    Assert.AreEqual(AnsiString('d'), Strings.ValueFromIndex[1], 'Test 7 value1');
    Assert.AreEqual(AnsiString('e'), Strings.Names[2], 'Test 7 name2');
    Assert.AreEqual(AnsiString('f'), Strings.ValueFromIndex[2], 'Test 7 value2');
    Strings.Clear;

    // Test 8: Whitespace trimming around '='
    Content := '  name  =  value  ;   another  =  val2  ';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 8 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 8 name0');
    Assert.AreEqual(AnsiString('value'), Strings.ValueFromIndex[0], 'Test 8 value0');
    Assert.AreEqual(AnsiString('another'), Strings.Names[1], 'Test 8 name1');
    Assert.AreEqual(AnsiString('val2'), Strings.ValueFromIndex[1], 'Test 8 value1');
    Strings.Clear;

    // Test 9: Tabs are whitespace
    Content := 'name'#9'='#9'value;'#9'another'#9'='#9'val2';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 9 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 9 name0');
    Assert.AreEqual(AnsiString('value'), Strings.ValueFromIndex[0], 'Test 9 value0');
    Assert.AreEqual(AnsiString('another'), Strings.Names[1], 'Test 9 name1');
    Assert.AreEqual(AnsiString('val2'), Strings.ValueFromIndex[1], 'Test 9 value1');
    Strings.Clear;

    // Test 10: Quoted values (strip OFF)
    Content := 'name="value"; another="val2"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 10 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 10 name0');
    Assert.AreEqual(AnsiString('"value"'), Strings.ValueFromIndex[0], 'Test 10 value0');
    Assert.AreEqual(AnsiString('another'), Strings.Names[1], 'Test 10 name1');
    Assert.AreEqual(AnsiString('"val2"'), Strings.ValueFromIndex[1], 'Test 10 value1');
    Strings.Clear;

    // Test 11: Quoted values (strip ON)
    Content := 'name="value"; another="val2"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 11 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 11 name0');
    Assert.AreEqual(AnsiString('value'), Strings.ValueFromIndex[0], 'Test 11 value0');
    Assert.AreEqual(AnsiString('another'), Strings.Names[1], 'Test 11 name1');
    Assert.AreEqual(AnsiString('val2'), Strings.ValueFromIndex[1], 'Test 11 value1');
    Strings.Clear;

    // Test 12: Single quotes supported
    Content := 'name=''value''; another=''val2''';
    ALExtractHeaderFields([';'], [' ', #9], [#39], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 12 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 12 name0');
    Assert.AreEqual(AnsiString('value'), Strings.ValueFromIndex[0], 'Test 12 value0');
    Assert.AreEqual(AnsiString('another'), Strings.Names[1], 'Test 12 name1');
    Assert.AreEqual(AnsiString('val2'), Strings.ValueFromIndex[1], 'Test 12 value1');
    Strings.Clear;

    // Test 13: Mixed quotes allowed
    Content := 'name="value"; another=''val2''';
    ALExtractHeaderFields([';'], [' ', #9], ['"', #39], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 13 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 13 name0');
    Assert.AreEqual(AnsiString('value'), Strings.ValueFromIndex[0], 'Test 13 value0');
    Assert.AreEqual(AnsiString('another'), Strings.Names[1], 'Test 13 name1');
    Assert.AreEqual(AnsiString('val2'), Strings.ValueFromIndex[1], 'Test 13 value1');
    Strings.Clear;

    // Test 14: Quote doubling ("" → ")
    Content := 'name="val""ue"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True, True);
    Assert.AreEqual(1, Strings.Count, 'Test 14 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 14 name0');
    Assert.AreEqual(AnsiString('val"ue'), Strings.ValueFromIndex[0], 'Test 14 value0');
    Strings.Clear;

    // Test 15: Escape char inside quotes (\)
    Content := 'name="val\"ue"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True, False, '\');
    Assert.AreEqual(1, Strings.Count, 'Test 15 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 15 name0');
    Assert.AreEqual(AnsiString('val"ue'), Strings.ValueFromIndex[0], 'Test 15 value0');
    Strings.Clear;

    // Test 16: Custom name/value separator ':'
    Content := 'name:value; another:val2';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False, False, #0, ':');
    Assert.AreEqual(2, Strings.Count, 'Test 16 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 16 name0');
    Assert.AreEqual(AnsiString('value'), Strings.ValueFromIndex[0], 'Test 16 value0');
    Assert.AreEqual(AnsiString('another'), Strings.Names[1], 'Test 16 name1');
    Assert.AreEqual(AnsiString('val2'), Strings.ValueFromIndex[1], 'Test 16 value1');
    Strings.Clear;

    // Test 17: Empty value
    Content := 'name=; another=val2';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 17 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 17 name0');
    Assert.AreEqual(AnsiString(''), Strings.ValueFromIndex[0], 'Test 17 value0');
    Assert.AreEqual(AnsiString('another'), Strings.Names[1], 'Test 17 name1');
    Assert.AreEqual(AnsiString('val2'), Strings.ValueFromIndex[1], 'Test 17 value1');
    Strings.Clear;

    // Test 18: Empty name
    Content := '=value; another=val2';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 18 count');
    Assert.AreEqual(AnsiString(''), Strings.Names[0], 'Test 18 name0');
    Assert.AreEqual(AnsiString('value'), Strings.ValueFromIndex[0], 'Test 18 value0');
    Assert.AreEqual(AnsiString('another'), Strings.Names[1], 'Test 18 name1');
    Assert.AreEqual(AnsiString('val2'), Strings.ValueFromIndex[1], 'Test 18 value1');
    Strings.Clear;

    // Test 19: Quoted empty value (strip ON)
    Content := 'name=""; another="val2"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 19 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 19 name0');
    Assert.AreEqual(AnsiString(''), Strings.ValueFromIndex[0], 'Test 19 value0');
    Assert.AreEqual(AnsiString('another'), Strings.Names[1], 'Test 19 name1');
    Assert.AreEqual(AnsiString('val2'), Strings.ValueFromIndex[1], 'Test 19 value1');
    Strings.Clear;

    // Test 20: Consecutive separators (empties ignored)
    Content := 'name=value;;;another=val2';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 20 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 20 name0');
    Assert.AreEqual(AnsiString('value'), Strings.ValueFromIndex[0], 'Test 20 value0');
    Assert.AreEqual(AnsiString('another'), Strings.Names[1], 'Test 20 name1');
    Assert.AreEqual(AnsiString('val2'), Strings.ValueFromIndex[1], 'Test 20 value1');
    Strings.Clear;

    // Test 21: Only separators
    Content := ';;;';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(0, Strings.Count, 'Test 21 count');
    Strings.Clear;

    // Test 22: Only whitespace
    Content := '   ';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(0, Strings.Count, 'Test 22 count');
    Strings.Clear;

    // Test 23: Embedded separator inside quotes (no split)
    Content := 'name="value;with;semi"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(1, Strings.Count, 'Test 23 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 23 name0');
    Assert.AreEqual(AnsiString('value;with;semi'), Strings.ValueFromIndex[0], 'Test 23 value0');
    Strings.Clear;

    // Test 24: No separators set -> entire string is one pair
    Content := 'name=value; another=val2';
    ALExtractHeaderFields([], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(1, Strings.Count, 'Test 24 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 24 name0');
    Assert.AreEqual(AnsiString('value; another=val2'), Strings.ValueFromIndex[0], 'Test 24 value0');
    Strings.Clear;

    // Test 25: No whitespace set (spaces preserved)
    Content := 'name  =  value  ;  another  =  val2';
    ALExtractHeaderFields([';'], [], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 25 count');
    Assert.AreEqual(AnsiString('name  '), Strings.Names[0], 'Test 25 name0');
    Assert.AreEqual(AnsiString('  value  '), Strings.ValueFromIndex[0], 'Test 25 value0');
    Assert.AreEqual(AnsiString('  another  '), Strings.Names[1], 'Test 25 name1');
    Assert.AreEqual(AnsiString('  val2'), Strings.ValueFromIndex[1], 'Test 25 value1');
    Strings.Clear;

    // Test 26: Quotes disabled -> quotes literal
    Content := 'a="b c"; x=y';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 26 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 26 name0');
    Assert.AreEqual(AnsiString('"b c"'), Strings.ValueFromIndex[0], 'Test 26 value0');
    Assert.AreEqual(AnsiString('x'), Strings.Names[1], 'Test 26 name1');
    Assert.AreEqual(AnsiString('y'), Strings.ValueFromIndex[1], 'Test 26 value1');
    Strings.Clear;

    // Test 27: Embedded '=' inside quoted value
    Content := 'a="b=c=d"; x=y';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 27 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 27 name0');
    Assert.AreEqual(AnsiString('b=c=d'), Strings.ValueFromIndex[0], 'Test 27 value0');
    Assert.AreEqual(AnsiString('x'), Strings.Names[1], 'Test 27 name1');
    Assert.AreEqual(AnsiString('y'), Strings.ValueFromIndex[1], 'Test 27 value1');
    Strings.Clear;

    // Test 28: Multiple '=' in unquoted value (split at first)
    Content := 'name=with=equals=value';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(1, Strings.Count, 'Test 28 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 28 name0');
    Assert.AreEqual(AnsiString('with=equals=value'), Strings.ValueFromIndex[0], 'Test 28 value0');
    Strings.Clear;

    // Test 29: CRLF and tabs as whitespace
    Content := #13#10'a=b;'#9'c="d e"'#13#10', f=g';
    ALExtractHeaderFields([';', ','], [' ', #9, #13, #10], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(3, Strings.Count, 'Test 29 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 29 name0');
    Assert.AreEqual(AnsiString('b'), Strings.ValueFromIndex[0], 'Test 29 value0');
    Assert.AreEqual(AnsiString('c'), Strings.Names[1], 'Test 29 name1');
    Assert.AreEqual(AnsiString('d e'), Strings.ValueFromIndex[1], 'Test 29 value1');
    Assert.AreEqual(AnsiString('f'), Strings.Names[2], 'Test 29 name2');
    Assert.AreEqual(AnsiString('g'), Strings.ValueFromIndex[2], 'Test 29 value2');
    Strings.Clear;

    // Test 30: Opposite quotes inside quoted value (no special handling)
    Content := 'a="he said ''yo''"';
    ALExtractHeaderFields([';'], [' ', #9], ['"', #39], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(1, Strings.Count, 'Test 30 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 30 name0');
    Assert.AreEqual(AnsiString('he said ''yo'''), Strings.ValueFromIndex[0], 'Test 30 value0');
    Strings.Clear;

    // Test 31: Unbalanced quoted value (kept as-is)
    Content := 'a="unterminated';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(1, Strings.Count, 'Test 31 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 31 name0');
    Assert.AreEqual(AnsiString('"unterminated'), Strings.ValueFromIndex[0], 'Test 31 value0');
    Strings.Clear;

    // Test 32: Unbalanced bare quoted token (token only)
    Content := '"token';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(1, Strings.Count, 'Test 32 count');
    Assert.AreEqual(AnsiString('"token'), Strings.Names[0], 'Test 32 name0');
    Assert.AreEqual(AnsiString(''), Strings.ValueFromIndex[0], 'Test 32 value0');
    Strings.Clear;

    // Test 33: Trailing separator (ignored)
    Content := 'a=b;';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(1, Strings.Count, 'Test 33 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 33 name0');
    Assert.AreEqual(AnsiString('b'), Strings.ValueFromIndex[0], 'Test 33 value0');
    Strings.Clear;

    // Test 34: Leading separator (ignored)
    Content := '; a=b; c=d';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 34 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 34 name0');
    Assert.AreEqual(AnsiString('b'), Strings.ValueFromIndex[0], 'Test 34 value0');
    Assert.AreEqual(AnsiString('c'), Strings.Names[1], 'Test 34 name1');
    Assert.AreEqual(AnsiString('d'), Strings.ValueFromIndex[1], 'Test 34 value1');
    Strings.Clear;

    // Test 35: Cookie-like flags + params
    Content := 'theme=light; sessionToken="abc123=="; Secure; HttpOnly; Path=/';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(5, Strings.Count, 'Test 35 count');
    Assert.AreEqual(AnsiString('theme'), Strings.Names[0], 'Test 35 name0');
    Assert.AreEqual(AnsiString('light'), Strings.ValueFromIndex[0], 'Test 35 value0');
    Assert.AreEqual(AnsiString('sessionToken'), Strings.Names[1], 'Test 35 name1');
    Assert.AreEqual(AnsiString('abc123=='), Strings.ValueFromIndex[1], 'Test 35 value1');
    Assert.AreEqual(AnsiString('Secure'), Strings.Names[2], 'Test 35 name2');
    Assert.AreEqual(AnsiString(''), Strings.ValueFromIndex[2], 'Test 35 value2');
    Assert.AreEqual(AnsiString('HttpOnly'), Strings.Names[3], 'Test 35 name3');
    Assert.AreEqual(AnsiString(''), Strings.ValueFromIndex[3], 'Test 35 value3');
    Assert.AreEqual(AnsiString('Path'), Strings.Names[4], 'Test 35 name4');
    Assert.AreEqual(AnsiString('/'), Strings.ValueFromIndex[4], 'Test 35 value4');
    Strings.Clear;

    // Test 36: Non-standard quote char `
    Content := '`name`=`value`';
    ALExtractHeaderFields([';'], [' ', #9], ['`'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(1, Strings.Count, 'Test 36 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 36 name0');
    Assert.AreEqual(AnsiString('value'), Strings.ValueFromIndex[0], 'Test 36 value0');
    Strings.Clear;

    // Test 37: Escaped separator inside quotes
    Content := 'name="value\;with\;semi"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True, False, '\');
    Assert.AreEqual(1, Strings.Count, 'Test 37 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 37 name0');
    Assert.AreEqual(AnsiString('value;with;semi'), Strings.ValueFromIndex[0], 'Test 37 value0');
    Strings.Clear;

    // Test 38: Escaped quotes and doubled quotes together
    Content := 'a="x\"y""z\""';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True, True, '\');
    Assert.AreEqual(1, Strings.Count, 'Test 38 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 38 name0');
    Assert.AreEqual(AnsiString('x"y"z"'), Strings.ValueFromIndex[0], 'Test 38 value0');
    Strings.Clear;

    // Test 39: Escaped backslashes inside quoted value
    Content := 'a="C:\\Temp\\File"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True, False, '\');
    Assert.AreEqual(1, Strings.Count, 'Test 39 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 39 name0');
    Assert.AreEqual(AnsiString('C:\Temp\File'), Strings.ValueFromIndex[0], 'Test 39 value0');
    Strings.Clear;

    // Test 40: Space as field separator (token list), quotes strip ON
    Content := '"name1" "name2"';
    ALExtractHeaderFields([' '], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 40 count');
    Assert.AreEqual(AnsiString('name1'), Strings[0], 'Test 40 tok0');
    Assert.AreEqual(AnsiString('name2'), Strings[1], 'Test 40 tok1');
    Strings.Clear;

    // Test 41: Bare tokens mixed with name/values
    Content := 'token1; a=b; "token 2"; c="d e"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(4, Strings.Count, 'Test 41 count');
    Assert.AreEqual(AnsiString('token1'), Strings.Names[0], 'Test 41 name0');
    Assert.AreEqual(AnsiString(''), Strings.ValueFromIndex[0], 'Test 41 value0');
    Assert.AreEqual(AnsiString('a'), Strings.Names[1], 'Test 41 name1');
    Assert.AreEqual(AnsiString('b'), Strings.ValueFromIndex[1], 'Test 41 value1');
    Assert.AreEqual(AnsiString('token 2'), Strings.Names[2], 'Test 41 name2');
    Assert.AreEqual(AnsiString(''), Strings.ValueFromIndex[2], 'Test 41 value2');
    Assert.AreEqual(AnsiString('c'), Strings.Names[3], 'Test 41 name3');
    Assert.AreEqual(AnsiString('d e'), Strings.ValueFromIndex[3], 'Test 41 value3');
    Strings.Clear;

    // Test 42: Name quoted (strip ON)
    Content := '"nm"=val';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(1, Strings.Count, 'Test 42 count');
    Assert.AreEqual(AnsiString('nm'), Strings.Names[0], 'Test 42 name0');
    Assert.AreEqual(AnsiString('val'), Strings.ValueFromIndex[0], 'Test 42 value0');
    Strings.Clear;

    // Test 43: Name quoted (strip OFF)
    Content := '"nm"=val';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(1, Strings.Count, 'Test 43 count');
    Assert.AreEqual(AnsiString('"nm"'), Strings.Names[0], 'Test 43 name0');
    Assert.AreEqual(AnsiString('val'), Strings.ValueFromIndex[0], 'Test 43 value0');
    Strings.Clear;

    // Test 44: Name contains separator char inside quotes
    Content := '"a;b"=x; y=z';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 44 count');
    Assert.AreEqual(AnsiString('a;b'), Strings.Names[0], 'Test 44 name0');
    Assert.AreEqual(AnsiString('x'), Strings.ValueFromIndex[0], 'Test 44 value0');
    Assert.AreEqual(AnsiString('y'), Strings.Names[1], 'Test 44 name1');
    Assert.AreEqual(AnsiString('z'), Strings.ValueFromIndex[1], 'Test 44 value1');
    Strings.Clear;

    // Test 45: Real-world header (Content-Type)
    Content := 'Content-Type= text/html; charset=utf-8; boundary="----=_NextPart_000_0000_01D9876A.ABCD1234"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(3, Strings.Count, 'Test 45 count');
    Assert.AreEqual(AnsiString('Content-Type'), Strings.Names[0], 'Test 45 name0');
    Assert.AreEqual(AnsiString('text/html'), Strings.ValueFromIndex[0], 'Test 45 value0');
    Assert.AreEqual(AnsiString('charset'), Strings.Names[1], 'Test 45 name1');
    Assert.AreEqual(AnsiString('utf-8'), Strings.ValueFromIndex[1], 'Test 45 value1');
    Assert.AreEqual(AnsiString('boundary'), Strings.Names[2], 'Test 45 name2');
    Assert.AreEqual(AnsiString('"----=_NextPart_000_0000_01D9876A.ABCD1234"'), Strings.ValueFromIndex[2], 'Test 45 value2');
    Strings.Clear;

    // Test 46: Very long strings
    Content := AnsiString(StringOfChar('a', 1000) + '=' + StringOfChar('b', 1000));
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(1, Strings.Count, 'Test 46 count');
    Assert.AreEqual(AnsiString(StringOfChar('a', 1000)), Strings.Names[0], 'Test 46 name0');
    Assert.AreEqual(AnsiString(StringOfChar('b', 1000)), Strings.ValueFromIndex[0], 'Test 46 value0');
    Strings.Clear;

    // Test 47: Unicode in AnsiString (if codepage allows)
    Content := 'name=valüe; another=val2';
    ALExtractHeaderFields([';'], [' ', #9], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 47 count');
    Assert.AreEqual(AnsiString('name'), Strings.Names[0], 'Test 47 name0');
    Assert.AreEqual(AnsiString('valüe'), Strings.ValueFromIndex[0], 'Test 47 value0');
    Assert.AreEqual(AnsiString('another'), Strings.Names[1], 'Test 47 name1');
    Assert.AreEqual(AnsiString('val2'), Strings.ValueFromIndex[1], 'Test 47 value1');
    Strings.Clear;

    // Test 48: Space-only quoted value preserved when stripped
    Content := 'a="   "';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(1, Strings.Count, 'Test 48 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 48 name0');
    Assert.AreEqual(AnsiString('   '), Strings.ValueFromIndex[0], 'Test 48 value0');
    Strings.Clear;

    // Test 49: Comma separator with quoted commas
    Content := 'a="1,2", b="3,4", c=5';
    ALExtractHeaderFields([','], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(3, Strings.Count, 'Test 49 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 49 name0');
    Assert.AreEqual(AnsiString('1,2'), Strings.ValueFromIndex[0], 'Test 49 value0');
    Assert.AreEqual(AnsiString('b'), Strings.Names[1], 'Test 49 name1');
    Assert.AreEqual(AnsiString('3,4'), Strings.ValueFromIndex[1], 'Test 49 value1');
    Assert.AreEqual(AnsiString('c'), Strings.Names[2], 'Test 49 name2');
    Assert.AreEqual(AnsiString('5'), Strings.ValueFromIndex[2], 'Test 49 value2');
    Strings.Clear;

    // Test 50: Name/value separator ':' with quoted colon inside value
    Content := 'a:"b:c:d"; e:f';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True, False, #0, ':');
    Assert.AreEqual(2, Strings.Count, 'Test 50 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 50 name0');
    Assert.AreEqual(AnsiString('b:c:d'), Strings.ValueFromIndex[0], 'Test 50 value0');
    Assert.AreEqual(AnsiString('e'), Strings.Names[1], 'Test 50 name1');
    Assert.AreEqual(AnsiString('f'), Strings.ValueFromIndex[1], 'Test 50 value1');
    Strings.Clear;

    // Test 51: Separator immediately after closing quote (semicolon)
    Content := 'a="x";b=y';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 51 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 51 name0');
    Assert.AreEqual(AnsiString('x'), Strings.ValueFromIndex[0], 'Test 51 value0');
    Assert.AreEqual(AnsiString('b'), Strings.Names[1], 'Test 51 name1');
    Assert.AreEqual(AnsiString('y'), Strings.ValueFromIndex[1], 'Test 51 value1');
    Strings.Clear;

    // Test 52: Separator immediately after closing quote (comma)
    Content := 'a="x",b=y';
    ALExtractHeaderFields([','], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 52 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 52 name0');
    Assert.AreEqual(AnsiString('x'), Strings.ValueFromIndex[0], 'Test 52 value0');
    Assert.AreEqual(AnsiString('b'), Strings.Names[1], 'Test 52 name1');
    Assert.AreEqual(AnsiString('y'), Strings.ValueFromIndex[1], 'Test 52 value1');
    Strings.Clear;

    // Test 53: No field separators at all: one pair
    Content := 'a=b c d';
    ALExtractHeaderFields([], [' ', #9], ['"'], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(1, Strings.Count, 'Test 53 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 53 name0');
    Assert.AreEqual(AnsiString('b c d'), Strings.ValueFromIndex[0], 'Test 53 value0');
    Strings.Clear;

    // Test 54: No whitespace set with ':' separator (spaces preserved)
    Content := 'a  :  b;  c:  d';
    ALExtractHeaderFields([';'], [], ['"'], PAnsiChar(Content), Strings, False, False, #0, ':');
    Assert.AreEqual(2, Strings.Count, 'Test 54 count');
    Assert.AreEqual(AnsiString('a  '), Strings.Names[0], 'Test 54 name0');
    Assert.AreEqual(AnsiString('  b'), Strings.ValueFromIndex[0], 'Test 54 value0');
    Assert.AreEqual(AnsiString('  c'), Strings.Names[1], 'Test 54 name1');
    Assert.AreEqual(AnsiString('  d'), Strings.ValueFromIndex[1], 'Test 54 value1');
    Strings.Clear;

    // Test 55: Escaped separator with comma list
    Content := 'a="x\,y", b=2';
    ALExtractHeaderFields([','], [' ', #9], ['"'], PAnsiChar(Content), Strings, True, False, '\');
    Assert.AreEqual(2, Strings.Count, 'Test 55 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 55 name0');
    Assert.AreEqual(AnsiString('x,y'), Strings.ValueFromIndex[0], 'Test 55 value0');
    Assert.AreEqual(AnsiString('b'), Strings.Names[1], 'Test 55 name1');
    Assert.AreEqual(AnsiString('2'), Strings.ValueFromIndex[1], 'Test 55 value1');
    Strings.Clear;

    // Test 56: Trailing escape within quoted value (kept as literal by parser)
    Content := 'a="xyz\"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True, False, '\');
    Assert.AreEqual(1, Strings.Count, 'Test 56 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 56 name0');
    Assert.AreEqual(AnsiString('"xyz\"'), Strings.ValueFromIndex[0], 'Test 56 value0');
    Strings.Clear;

    // Test 57: Alternate escape char ^
    Content := 'a="x^"y^"z"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True, False, '^');
    Assert.AreEqual(1, Strings.Count, 'Test 57 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 57 name0');
    Assert.AreEqual(AnsiString('x"y"z'), Strings.ValueFromIndex[0], 'Test 57 value0');
    Strings.Clear;

    // Test 58: Doubling ON for single quotes
    Content := 'a=''It''''s''';
    ALExtractHeaderFields([';'], [' ', #9], [#39], PAnsiChar(Content), Strings, True, True);
    Assert.AreEqual(1, Strings.Count, 'Test 58 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 58 name0');
    Assert.AreEqual(AnsiString('It''s'), Strings.ValueFromIndex[0], 'Test 58 value0');
    Strings.Clear;

    // Test 59: Name quoted with doubled quotes (strip ON)
    Content := '"na""me"=x';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True, True);
    Assert.AreEqual(1, Strings.Count, 'Test 59 count');
    Assert.AreEqual(AnsiString('na"me'), Strings.Names[0], 'Test 59 name0');
    Assert.AreEqual(AnsiString('x'), Strings.ValueFromIndex[0], 'Test 59 value0');
    Strings.Clear;

    // Test 60: Cookie-like Expires date with comma inside quotes
    Content := 'Expires="Wed, 21 Oct 2015 07:28:00 GMT"; Path=/; Secure; HttpOnly';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(4, Strings.Count, 'Test 60 count');
    Assert.AreEqual(AnsiString('Expires'), Strings.Names[0], 'Test 60 name0');
    Assert.AreEqual(AnsiString('Wed, 21 Oct 2015 07:28:00 GMT'), Strings.ValueFromIndex[0], 'Test 60 value0');
    Assert.AreEqual(AnsiString('Path'), Strings.Names[1], 'Test 60 name1');
    Assert.AreEqual(AnsiString('/'), Strings.ValueFromIndex[1], 'Test 60 value1');
    Assert.AreEqual(AnsiString('Secure'), Strings.Names[2], 'Test 60 name2');
    Assert.AreEqual(AnsiString(''), Strings.ValueFromIndex[2], 'Test 60 value2');
    Assert.AreEqual(AnsiString('HttpOnly'), Strings.Names[3], 'Test 60 name3');
    Assert.AreEqual(AnsiString(''), Strings.ValueFromIndex[3], 'Test 60 value3');
    Strings.Clear;

// Test 61: Tab as field separator (IMPORTANT: do NOT include #9 in whitespace)
    Content := 'a=b'#9'c=d'#9'e=f';
    ALExtractHeaderFields([#9], [' '], [], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(3, Strings.Count, 'Test 61 count');
    Assert.AreEqual(AnsiString('a'), Strings.Names[0], 'Test 61 name0');
    Assert.AreEqual(AnsiString('b'), Strings.ValueFromIndex[0], 'Test 61 value0');
    Assert.AreEqual(AnsiString('c'), Strings.Names[1], 'Test 61 name1');
    Assert.AreEqual(AnsiString('d'), Strings.ValueFromIndex[1], 'Test 61 value1');
    Assert.AreEqual(AnsiString('e'), Strings.Names[2], 'Test 61 name2');
    Assert.AreEqual(AnsiString('f'), Strings.ValueFromIndex[2], 'Test 61 value2');
    Strings.Clear;

    // Test 62: Token-only mixed list, strip OFF (keep quotes on quoted tokens)
    Content := '"alpha"; beta; "gamma delta"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(3, Strings.Count, 'Test 62 count');
    Assert.AreEqual(AnsiString('"alpha"'), Strings.Names[0], 'Test 62 tok0');
    Assert.AreEqual(AnsiString(''), Strings.ValueFromIndex[0], 'Test 62 val0');
    Assert.AreEqual(AnsiString('beta'), Strings.Names[1], 'Test 62 tok1');
    Assert.AreEqual(AnsiString(''), Strings.ValueFromIndex[1], 'Test 62 val1');
    Assert.AreEqual(AnsiString('"gamma delta"'), Strings.Names[2], 'Test 62 tok2');
    Assert.AreEqual(AnsiString(''), Strings.ValueFromIndex[2], 'Test 62 val2');
    Strings.Clear;

    // Test 63: Space-as-separator token list, strip OFF (quotes preserved)
    Content := '"name1" "name2" z';
    ALExtractHeaderFields([' '], [' ', #9], ['"'], PAnsiChar(Content), Strings, False);
    Assert.AreEqual(3, Strings.Count, 'Test 63 count');
    Assert.AreEqual(AnsiString('"name1"'), Strings[0], 'Test 63 tok0');
    Assert.AreEqual(AnsiString('"name2"'), Strings[1], 'Test 63 tok1');
    Assert.AreEqual(AnsiString('z'), Strings[2], 'Test 63 tok2');
    Strings.Clear;

    // Test 64: Tab-as-separator with quoted tokens, strip ON
    Content := '"a b"'#9'"c d"';
    ALExtractHeaderFields([#9], [' '], ['"'], PAnsiChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 64 count');
    Assert.AreEqual(AnsiString('a b'), Strings[0], 'Test 64 tok0');
    Assert.AreEqual(AnsiString('c d'), Strings[1], 'Test 64 tok1');
    Strings.Clear;

  finally
    Strings.Free;
  end;
end;

{********************************************************}
procedure TALDUnitXTestStringUtils.ALExtractHeaderFieldsW;
var
  Strings: TALNVStringListW;
  Content: String;
begin
  Strings := TALNVStringListW.Create;
  try
    // Test 1: Basic name=value without quotes, separator ;
    Content := 'name=value; another=val2';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 1 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 1 name0');
    Assert.AreEqual(String('value'), Strings.ValueFromIndex[0], 'Test 1 value0');
    Assert.AreEqual(String('another'), Strings.Names[1], 'Test 1 name1');
    Assert.AreEqual(String('val2'), Strings.ValueFromIndex[1], 'Test 1 value1');
    Strings.Clear;

    // Test 2: Empty content
    Content := '';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(0, Strings.Count, 'Test 2 count');
    Strings.Clear;

    // Test 3: Nil content
    ALExtractHeaderFields([';'], [' ', #9], [], nil, Strings, False);
    Assert.AreEqual(0, Strings.Count, 'Test 3 count');
    Strings.Clear;

    // Test 4: Single name without value
    Content := 'name';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(1, Strings.Count, 'Test 4 count');
    Assert.AreEqual(String('name'), Strings[0], 'Test 4 token');
    Strings.Clear;

    // Test 5: Multiple names without values
    Content := 'name1;name2;name3;';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(3, Strings.Count, 'Test 5 count');
    Assert.AreEqual(String('name1'), Strings[0], 'Test 5 tok0');
    Assert.AreEqual(String('name2'), Strings[1], 'Test 5 tok1');
    Assert.AreEqual(String('name3'), Strings[2], 'Test 5 tok2');
    Strings.Clear;

    // Test 6: Different field separator (comma)
    Content := 'name=value, another=val2';
    ALExtractHeaderFields([','], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 6 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 6 name0');
    Assert.AreEqual(String('value'), Strings.ValueFromIndex[0], 'Test 6 value0');
    Assert.AreEqual(String('another'), Strings.Names[1], 'Test 6 name1');
    Assert.AreEqual(String('val2'), Strings.ValueFromIndex[1], 'Test 6 value1');
    Strings.Clear;

    // Test 7: Multiple separators in set ; and ,
    Content := 'a=b; c=d, e=f';
    ALExtractHeaderFields([';', ','], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(3, Strings.Count, 'Test 7 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 7 name0');
    Assert.AreEqual(String('b'), Strings.ValueFromIndex[0], 'Test 7 value0');
    Assert.AreEqual(String('c'), Strings.Names[1], 'Test 7 name1');
    Assert.AreEqual(String('d'), Strings.ValueFromIndex[1], 'Test 7 value1');
    Assert.AreEqual(String('e'), Strings.Names[2], 'Test 7 name2');
    Assert.AreEqual(String('f'), Strings.ValueFromIndex[2], 'Test 7 value2');
    Strings.Clear;

    // Test 8: Whitespace trimming around '='
    Content := '  name  =  value  ;   another  =  val2  ';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 8 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 8 name0');
    Assert.AreEqual(String('value'), Strings.ValueFromIndex[0], 'Test 8 value0');
    Assert.AreEqual(String('another'), Strings.Names[1], 'Test 8 name1');
    Assert.AreEqual(String('val2'), Strings.ValueFromIndex[1], 'Test 8 value1');
    Strings.Clear;

    // Test 9: Tabs are whitespace
    Content := 'name'#9'='#9'value;'#9'another'#9'='#9'val2';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 9 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 9 name0');
    Assert.AreEqual(String('value'), Strings.ValueFromIndex[0], 'Test 9 value0');
    Assert.AreEqual(String('another'), Strings.Names[1], 'Test 9 name1');
    Assert.AreEqual(String('val2'), Strings.ValueFromIndex[1], 'Test 9 value1');
    Strings.Clear;

    // Test 10: Quoted values (strip OFF)
    Content := 'name="value"; another="val2"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 10 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 10 name0');
    Assert.AreEqual(String('"value"'), Strings.ValueFromIndex[0], 'Test 10 value0');
    Assert.AreEqual(String('another'), Strings.Names[1], 'Test 10 name1');
    Assert.AreEqual(String('"val2"'), Strings.ValueFromIndex[1], 'Test 10 value1');
    Strings.Clear;

    // Test 11: Quoted values (strip ON)
    Content := 'name="value"; another="val2"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 11 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 11 name0');
    Assert.AreEqual(String('value'), Strings.ValueFromIndex[0], 'Test 11 value0');
    Assert.AreEqual(String('another'), Strings.Names[1], 'Test 11 name1');
    Assert.AreEqual(String('val2'), Strings.ValueFromIndex[1], 'Test 11 value1');
    Strings.Clear;

    // Test 12: Single quotes supported
    Content := 'name=''value''; another=''val2''';
    ALExtractHeaderFields([';'], [' ', #9], [#39], PChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 12 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 12 name0');
    Assert.AreEqual(String('value'), Strings.ValueFromIndex[0], 'Test 12 value0');
    Assert.AreEqual(String('another'), Strings.Names[1], 'Test 12 name1');
    Assert.AreEqual(String('val2'), Strings.ValueFromIndex[1], 'Test 12 value1');
    Strings.Clear;

    // Test 13: Mixed quotes allowed
    Content := 'name="value"; another=''val2''';
    ALExtractHeaderFields([';'], [' ', #9], ['"', #39], PChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 13 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 13 name0');
    Assert.AreEqual(String('value'), Strings.ValueFromIndex[0], 'Test 13 value0');
    Assert.AreEqual(String('another'), Strings.Names[1], 'Test 13 name1');
    Assert.AreEqual(String('val2'), Strings.ValueFromIndex[1], 'Test 13 value1');
    Strings.Clear;

    // Test 14: Quote doubling ("" → ")
    Content := 'name="val""ue"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True, True);
    Assert.AreEqual(1, Strings.Count, 'Test 14 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 14 name0');
    Assert.AreEqual(String('val"ue'), Strings.ValueFromIndex[0], 'Test 14 value0');
    Strings.Clear;

    // Test 15: Escape char inside quotes (\)
    Content := 'name="val\"ue"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True, False, '\');
    Assert.AreEqual(1, Strings.Count, 'Test 15 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 15 name0');
    Assert.AreEqual(String('val"ue'), Strings.ValueFromIndex[0], 'Test 15 value0');
    Strings.Clear;

    // Test 16: Custom name/value separator ':'
    Content := 'name:value; another:val2';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False, False, #0, ':');
    Assert.AreEqual(2, Strings.Count, 'Test 16 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 16 name0');
    Assert.AreEqual(String('value'), Strings.ValueFromIndex[0], 'Test 16 value0');
    Assert.AreEqual(String('another'), Strings.Names[1], 'Test 16 name1');
    Assert.AreEqual(String('val2'), Strings.ValueFromIndex[1], 'Test 16 value1');
    Strings.Clear;

    // Test 17: Empty value
    Content := 'name=; another=val2';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 17 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 17 name0');
    Assert.AreEqual(String(''), Strings.ValueFromIndex[0], 'Test 17 value0');
    Assert.AreEqual(String('another'), Strings.Names[1], 'Test 17 name1');
    Assert.AreEqual(String('val2'), Strings.ValueFromIndex[1], 'Test 17 value1');
    Strings.Clear;

    // Test 18: Empty name
    Content := '=value; another=val2';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 18 count');
    Assert.AreEqual(String(''), Strings.Names[0], 'Test 18 name0');
    Assert.AreEqual(String('value'), Strings.ValueFromIndex[0], 'Test 18 value0');
    Assert.AreEqual(String('another'), Strings.Names[1], 'Test 18 name1');
    Assert.AreEqual(String('val2'), Strings.ValueFromIndex[1], 'Test 18 value1');
    Strings.Clear;

    // Test 19: Quoted empty value (strip ON)
    Content := 'name=""; another="val2"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 19 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 19 name0');
    Assert.AreEqual(String(''), Strings.ValueFromIndex[0], 'Test 19 value0');
    Assert.AreEqual(String('another'), Strings.Names[1], 'Test 19 name1');
    Assert.AreEqual(String('val2'), Strings.ValueFromIndex[1], 'Test 19 value1');
    Strings.Clear;

    // Test 20: Consecutive separators (empties ignored)
    Content := 'name=value;;;another=val2';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 20 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 20 name0');
    Assert.AreEqual(String('value'), Strings.ValueFromIndex[0], 'Test 20 value0');
    Assert.AreEqual(String('another'), Strings.Names[1], 'Test 20 name1');
    Assert.AreEqual(String('val2'), Strings.ValueFromIndex[1], 'Test 20 value1');
    Strings.Clear;

    // Test 21: Only separators
    Content := ';;;';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(0, Strings.Count, 'Test 21 count');
    Strings.Clear;

    // Test 22: Only whitespace
    Content := '   ';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(0, Strings.Count, 'Test 22 count');
    Strings.Clear;

    // Test 23: Embedded separator inside quotes (no split)
    Content := 'name="value;with;semi"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(1, Strings.Count, 'Test 23 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 23 name0');
    Assert.AreEqual(String('value;with;semi'), Strings.ValueFromIndex[0], 'Test 23 value0');
    Strings.Clear;

    // Test 24: No separators set -> entire string is one pair
    Content := 'name=value; another=val2';
    ALExtractHeaderFields([], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(1, Strings.Count, 'Test 24 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 24 name0');
    Assert.AreEqual(String('value; another=val2'), Strings.ValueFromIndex[0], 'Test 24 value0');
    Strings.Clear;

    // Test 25: No whitespace set (spaces preserved)
    Content := 'name  =  value  ;  another  =  val2';
    ALExtractHeaderFields([';'], [], [], PChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 25 count');
    Assert.AreEqual(String('name  '), Strings.Names[0], 'Test 25 name0');
    Assert.AreEqual(String('  value  '), Strings.ValueFromIndex[0], 'Test 25 value0');
    Assert.AreEqual(String('  another  '), Strings.Names[1], 'Test 25 name1');
    Assert.AreEqual(String('  val2'), Strings.ValueFromIndex[1], 'Test 25 value1');
    Strings.Clear;

    // Test 26: Quotes disabled -> quotes literal
    Content := 'a="b c"; x=y';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 26 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 26 name0');
    Assert.AreEqual(String('"b c"'), Strings.ValueFromIndex[0], 'Test 26 value0');
    Assert.AreEqual(String('x'), Strings.Names[1], 'Test 26 name1');
    Assert.AreEqual(String('y'), Strings.ValueFromIndex[1], 'Test 26 value1');
    Strings.Clear;

    // Test 27: Embedded '=' inside quoted value
    Content := 'a="b=c=d"; x=y';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 27 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 27 name0');
    Assert.AreEqual(String('b=c=d'), Strings.ValueFromIndex[0], 'Test 27 value0');
    Assert.AreEqual(String('x'), Strings.Names[1], 'Test 27 name1');
    Assert.AreEqual(String('y'), Strings.ValueFromIndex[1], 'Test 27 value1');
    Strings.Clear;

    // Test 28: Multiple '=' in unquoted value (split at first)
    Content := 'name=with=equals=value';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(1, Strings.Count, 'Test 28 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 28 name0');
    Assert.AreEqual(String('with=equals=value'), Strings.ValueFromIndex[0], 'Test 28 value0');
    Strings.Clear;

    // Test 29: CRLF and tabs as whitespace
    Content := #13#10'a=b;'#9'c="d e"'#13#10', f=g';
    ALExtractHeaderFields([';', ','], [' ', #9, #13, #10], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(3, Strings.Count, 'Test 29 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 29 name0');
    Assert.AreEqual(String('b'), Strings.ValueFromIndex[0], 'Test 29 value0');
    Assert.AreEqual(String('c'), Strings.Names[1], 'Test 29 name1');
    Assert.AreEqual(String('d e'), Strings.ValueFromIndex[1], 'Test 29 value1');
    Assert.AreEqual(String('f'), Strings.Names[2], 'Test 29 name2');
    Assert.AreEqual(String('g'), Strings.ValueFromIndex[2], 'Test 29 value2');
    Strings.Clear;

    // Test 30: Opposite quotes inside quoted value (no special handling)
    Content := 'a="he said ''yo''"';
    ALExtractHeaderFields([';'], [' ', #9], ['"', #39], PChar(Content), Strings, True);
    Assert.AreEqual(1, Strings.Count, 'Test 30 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 30 name0');
    Assert.AreEqual(String('he said ''yo'''), Strings.ValueFromIndex[0], 'Test 30 value0');
    Strings.Clear;

    // Test 31: Unbalanced quoted value (kept as-is)
    Content := 'a="unterminated';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(1, Strings.Count, 'Test 31 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 31 name0');
    Assert.AreEqual(String('"unterminated'), Strings.ValueFromIndex[0], 'Test 31 value0');
    Strings.Clear;

    // Test 32: Unbalanced bare quoted token (token only)
    Content := '"token';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(1, Strings.Count, 'Test 32 count');
    Assert.AreEqual(String('"token'), Strings.Names[0], 'Test 32 name0');
    Assert.AreEqual(String(''), Strings.ValueFromIndex[0], 'Test 32 value0');
    Strings.Clear;

    // Test 33: Trailing separator (ignored)
    Content := 'a=b;';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(1, Strings.Count, 'Test 33 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 33 name0');
    Assert.AreEqual(String('b'), Strings.ValueFromIndex[0], 'Test 33 value0');
    Strings.Clear;

    // Test 34: Leading separator (ignored)
    Content := '; a=b; c=d';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 34 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 34 name0');
    Assert.AreEqual(String('b'), Strings.ValueFromIndex[0], 'Test 34 value0');
    Assert.AreEqual(String('c'), Strings.Names[1], 'Test 34 name1');
    Assert.AreEqual(String('d'), Strings.ValueFromIndex[1], 'Test 34 value1');
    Strings.Clear;

    // Test 35: Cookie-like flags + params
    Content := 'theme=light; sessionToken="abc123=="; Secure; HttpOnly; Path=/';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(5, Strings.Count, 'Test 35 count');
    Assert.AreEqual(String('theme'), Strings.Names[0], 'Test 35 name0');
    Assert.AreEqual(String('light'), Strings.ValueFromIndex[0], 'Test 35 value0');
    Assert.AreEqual(String('sessionToken'), Strings.Names[1], 'Test 35 name1');
    Assert.AreEqual(String('abc123=='), Strings.ValueFromIndex[1], 'Test 35 value1');
    Assert.AreEqual(String('Secure'), Strings.Names[2], 'Test 35 name2');
    Assert.AreEqual(String(''), Strings.ValueFromIndex[2], 'Test 35 value2');
    Assert.AreEqual(String('HttpOnly'), Strings.Names[3], 'Test 35 name3');
    Assert.AreEqual(String(''), Strings.ValueFromIndex[3], 'Test 35 value3');
    Assert.AreEqual(String('Path'), Strings.Names[4], 'Test 35 name4');
    Assert.AreEqual(String('/'), Strings.ValueFromIndex[4], 'Test 35 value4');
    Strings.Clear;

    // Test 36: Non-standard quote char `
    Content := '`name`=`value`';
    ALExtractHeaderFields([';'], [' ', #9], ['`'], PChar(Content), Strings, True);
    Assert.AreEqual(1, Strings.Count, 'Test 36 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 36 name0');
    Assert.AreEqual(String('value'), Strings.ValueFromIndex[0], 'Test 36 value0');
    Strings.Clear;

    // Test 37: Escaped separator inside quotes
    Content := 'name="value\;with\;semi"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True, False, '\');
    Assert.AreEqual(1, Strings.Count, 'Test 37 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 37 name0');
    Assert.AreEqual(String('value;with;semi'), Strings.ValueFromIndex[0], 'Test 37 value0');
    Strings.Clear;

    // Test 38: Escaped quotes and doubled quotes together
    Content := 'a="x\"y""z\""';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True, True, '\');
    Assert.AreEqual(1, Strings.Count, 'Test 38 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 38 name0');
    Assert.AreEqual(String('x"y"z"'), Strings.ValueFromIndex[0], 'Test 38 value0');
    Strings.Clear;

    // Test 39: Escaped backslashes inside quoted value
    Content := 'a="C:\\Temp\\File"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True, False, '\');
    Assert.AreEqual(1, Strings.Count, 'Test 39 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 39 name0');
    Assert.AreEqual(String('C:\Temp\File'), Strings.ValueFromIndex[0], 'Test 39 value0');
    Strings.Clear;

    // Test 40: Space as field separator (token list), quotes strip ON
    Content := '"name1" "name2"';
    ALExtractHeaderFields([' '], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 40 count');
    Assert.AreEqual(String('name1'), Strings[0], 'Test 40 tok0');
    Assert.AreEqual(String('name2'), Strings[1], 'Test 40 tok1');
    Strings.Clear;

    // Test 41: Bare tokens mixed with name/values
    Content := 'token1; a=b; "token 2"; c="d e"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(4, Strings.Count, 'Test 41 count');
    Assert.AreEqual(String('token1'), Strings.Names[0], 'Test 41 name0');
    Assert.AreEqual(String(''), Strings.ValueFromIndex[0], 'Test 41 value0');
    Assert.AreEqual(String('a'), Strings.Names[1], 'Test 41 name1');
    Assert.AreEqual(String('b'), Strings.ValueFromIndex[1], 'Test 41 value1');
    Assert.AreEqual(String('token 2'), Strings.Names[2], 'Test 41 name2');
    Assert.AreEqual(String(''), Strings.ValueFromIndex[2], 'Test 41 value2');
    Assert.AreEqual(String('c'), Strings.Names[3], 'Test 41 name3');
    Assert.AreEqual(String('d e'), Strings.ValueFromIndex[3], 'Test 41 value3');
    Strings.Clear;

    // Test 42: Name quoted (strip ON)
    Content := '"nm"=val';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(1, Strings.Count, 'Test 42 count');
    Assert.AreEqual(String('nm'), Strings.Names[0], 'Test 42 name0');
    Assert.AreEqual(String('val'), Strings.ValueFromIndex[0], 'Test 42 value0');
    Strings.Clear;

    // Test 43: Name quoted (strip OFF)
    Content := '"nm"=val';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, False);
    Assert.AreEqual(1, Strings.Count, 'Test 43 count');
    Assert.AreEqual(String('"nm"'), Strings.Names[0], 'Test 43 name0');
    Assert.AreEqual(String('val'), Strings.ValueFromIndex[0], 'Test 43 value0');
    Strings.Clear;

    // Test 44: Name contains separator char inside quotes
    Content := '"a;b"=x; y=z';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 44 count');
    Assert.AreEqual(String('a;b'), Strings.Names[0], 'Test 44 name0');
    Assert.AreEqual(String('x'), Strings.ValueFromIndex[0], 'Test 44 value0');
    Assert.AreEqual(String('y'), Strings.Names[1], 'Test 44 name1');
    Assert.AreEqual(String('z'), Strings.ValueFromIndex[1], 'Test 44 value1');
    Strings.Clear;

    // Test 45: Real-world header (Content-Type)
    Content := 'Content-Type= text/html; charset=utf-8; boundary="----=_NextPart_000_0000_01D9876A.ABCD1234"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, False);
    Assert.AreEqual(3, Strings.Count, 'Test 45 count');
    Assert.AreEqual(String('Content-Type'), Strings.Names[0], 'Test 45 name0');
    Assert.AreEqual(String('text/html'), Strings.ValueFromIndex[0], 'Test 45 value0');
    Assert.AreEqual(String('charset'), Strings.Names[1], 'Test 45 name1');
    Assert.AreEqual(String('utf-8'), Strings.ValueFromIndex[1], 'Test 45 value1');
    Assert.AreEqual(String('boundary'), Strings.Names[2], 'Test 45 name2');
    Assert.AreEqual(String('"----=_NextPart_000_0000_01D9876A.ABCD1234"'), Strings.ValueFromIndex[2], 'Test 45 value2');
    Strings.Clear;

    // Test 46: Very long strings
    Content := String(StringOfChar('a', 1000) + '=' + StringOfChar('b', 1000));
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(1, Strings.Count, 'Test 46 count');
    Assert.AreEqual(String(StringOfChar('a', 1000)), Strings.Names[0], 'Test 46 name0');
    Assert.AreEqual(String(StringOfChar('b', 1000)), Strings.ValueFromIndex[0], 'Test 46 value0');
    Strings.Clear;

    // Test 47: Unicode in String (if codepage allows)
    Content := 'name=valüe; another=val2';
    ALExtractHeaderFields([';'], [' ', #9], [], PChar(Content), Strings, False);
    Assert.AreEqual(2, Strings.Count, 'Test 47 count');
    Assert.AreEqual(String('name'), Strings.Names[0], 'Test 47 name0');
    Assert.AreEqual(String('valüe'), Strings.ValueFromIndex[0], 'Test 47 value0');
    Assert.AreEqual(String('another'), Strings.Names[1], 'Test 47 name1');
    Assert.AreEqual(String('val2'), Strings.ValueFromIndex[1], 'Test 47 value1');
    Strings.Clear;

    // Test 48: Space-only quoted value preserved when stripped
    Content := 'a="   "';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(1, Strings.Count, 'Test 48 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 48 name0');
    Assert.AreEqual(String('   '), Strings.ValueFromIndex[0], 'Test 48 value0');
    Strings.Clear;

    // Test 49: Comma separator with quoted commas
    Content := 'a="1,2", b="3,4", c=5';
    ALExtractHeaderFields([','], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(3, Strings.Count, 'Test 49 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 49 name0');
    Assert.AreEqual(String('1,2'), Strings.ValueFromIndex[0], 'Test 49 value0');
    Assert.AreEqual(String('b'), Strings.Names[1], 'Test 49 name1');
    Assert.AreEqual(String('3,4'), Strings.ValueFromIndex[1], 'Test 49 value1');
    Assert.AreEqual(String('c'), Strings.Names[2], 'Test 49 name2');
    Assert.AreEqual(String('5'), Strings.ValueFromIndex[2], 'Test 49 value2');
    Strings.Clear;

    // Test 50: Name/value separator ':' with quoted colon inside value
    Content := 'a:"b:c:d"; e:f';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True, False, #0, ':');
    Assert.AreEqual(2, Strings.Count, 'Test 50 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 50 name0');
    Assert.AreEqual(String('b:c:d'), Strings.ValueFromIndex[0], 'Test 50 value0');
    Assert.AreEqual(String('e'), Strings.Names[1], 'Test 50 name1');
    Assert.AreEqual(String('f'), Strings.ValueFromIndex[1], 'Test 50 value1');
    Strings.Clear;

    // Test 51: Separator immediately after closing quote (semicolon)
    Content := 'a="x";b=y';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 51 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 51 name0');
    Assert.AreEqual(String('x'), Strings.ValueFromIndex[0], 'Test 51 value0');
    Assert.AreEqual(String('b'), Strings.Names[1], 'Test 51 name1');
    Assert.AreEqual(String('y'), Strings.ValueFromIndex[1], 'Test 51 value1');
    Strings.Clear;

    // Test 52: Separator immediately after closing quote (comma)
    Content := 'a="x",b=y';
    ALExtractHeaderFields([','], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 52 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 52 name0');
    Assert.AreEqual(String('x'), Strings.ValueFromIndex[0], 'Test 52 value0');
    Assert.AreEqual(String('b'), Strings.Names[1], 'Test 52 name1');
    Assert.AreEqual(String('y'), Strings.ValueFromIndex[1], 'Test 52 value1');
    Strings.Clear;

    // Test 53: No field separators at all: one pair
    Content := 'a=b c d';
    ALExtractHeaderFields([], [' ', #9], ['"'], PChar(Content), Strings, False);
    Assert.AreEqual(1, Strings.Count, 'Test 53 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 53 name0');
    Assert.AreEqual(String('b c d'), Strings.ValueFromIndex[0], 'Test 53 value0');
    Strings.Clear;

    // Test 54: No whitespace set with ':' separator (spaces preserved)
    Content := 'a  :  b;  c:  d';
    ALExtractHeaderFields([';'], [], ['"'], PChar(Content), Strings, False, False, #0, ':');
    Assert.AreEqual(2, Strings.Count, 'Test 54 count');
    Assert.AreEqual(String('a  '), Strings.Names[0], 'Test 54 name0');
    Assert.AreEqual(String('  b'), Strings.ValueFromIndex[0], 'Test 54 value0');
    Assert.AreEqual(String('  c'), Strings.Names[1], 'Test 54 name1');
    Assert.AreEqual(String('  d'), Strings.ValueFromIndex[1], 'Test 54 value1');
    Strings.Clear;

    // Test 55: Escaped separator with comma list
    Content := 'a="x\,y", b=2';
    ALExtractHeaderFields([','], [' ', #9], ['"'], PChar(Content), Strings, True, False, '\');
    Assert.AreEqual(2, Strings.Count, 'Test 55 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 55 name0');
    Assert.AreEqual(String('x,y'), Strings.ValueFromIndex[0], 'Test 55 value0');
    Assert.AreEqual(String('b'), Strings.Names[1], 'Test 55 name1');
    Assert.AreEqual(String('2'), Strings.ValueFromIndex[1], 'Test 55 value1');
    Strings.Clear;

    // Test 56: Trailing escape within quoted value (kept as literal by parser)
    Content := 'a="xyz\"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True, False, '\');
    Assert.AreEqual(1, Strings.Count, 'Test 56 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 56 name0');
    Assert.AreEqual(String('"xyz\"'), Strings.ValueFromIndex[0], 'Test 56 value0');
    Strings.Clear;

    // Test 57: Alternate escape char ^
    Content := 'a="x^"y^"z"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True, False, '^');
    Assert.AreEqual(1, Strings.Count, 'Test 57 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 57 name0');
    Assert.AreEqual(String('x"y"z'), Strings.ValueFromIndex[0], 'Test 57 value0');
    Strings.Clear;

    // Test 58: Doubling ON for single quotes
    Content := 'a=''It''''s''';
    ALExtractHeaderFields([';'], [' ', #9], [#39], PChar(Content), Strings, True, True);
    Assert.AreEqual(1, Strings.Count, 'Test 58 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 58 name0');
    Assert.AreEqual(String('It''s'), Strings.ValueFromIndex[0], 'Test 58 value0');
    Strings.Clear;

    // Test 59: Name quoted with doubled quotes (strip ON)
    Content := '"na""me"=x';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True, True);
    Assert.AreEqual(1, Strings.Count, 'Test 59 count');
    Assert.AreEqual(String('na"me'), Strings.Names[0], 'Test 59 name0');
    Assert.AreEqual(String('x'), Strings.ValueFromIndex[0], 'Test 59 value0');
    Strings.Clear;

    // Test 60: Cookie-like Expires date with comma inside quotes
    Content := 'Expires="Wed, 21 Oct 2015 07:28:00 GMT"; Path=/; Secure; HttpOnly';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(4, Strings.Count, 'Test 60 count');
    Assert.AreEqual(String('Expires'), Strings.Names[0], 'Test 60 name0');
    Assert.AreEqual(String('Wed, 21 Oct 2015 07:28:00 GMT'), Strings.ValueFromIndex[0], 'Test 60 value0');
    Assert.AreEqual(String('Path'), Strings.Names[1], 'Test 60 name1');
    Assert.AreEqual(String('/'), Strings.ValueFromIndex[1], 'Test 60 value1');
    Assert.AreEqual(String('Secure'), Strings.Names[2], 'Test 60 name2');
    Assert.AreEqual(String(''), Strings.ValueFromIndex[2], 'Test 60 value2');
    Assert.AreEqual(String('HttpOnly'), Strings.Names[3], 'Test 60 name3');
    Assert.AreEqual(String(''), Strings.ValueFromIndex[3], 'Test 60 value3');
    Strings.Clear;

// Test 61: Tab as field separator (IMPORTANT: do NOT include #9 in whitespace)
    Content := 'a=b'#9'c=d'#9'e=f';
    ALExtractHeaderFields([#9], [' '], [], PChar(Content), Strings, False);
    Assert.AreEqual(3, Strings.Count, 'Test 61 count');
    Assert.AreEqual(String('a'), Strings.Names[0], 'Test 61 name0');
    Assert.AreEqual(String('b'), Strings.ValueFromIndex[0], 'Test 61 value0');
    Assert.AreEqual(String('c'), Strings.Names[1], 'Test 61 name1');
    Assert.AreEqual(String('d'), Strings.ValueFromIndex[1], 'Test 61 value1');
    Assert.AreEqual(String('e'), Strings.Names[2], 'Test 61 name2');
    Assert.AreEqual(String('f'), Strings.ValueFromIndex[2], 'Test 61 value2');
    Strings.Clear;

    // Test 62: Token-only mixed list, strip OFF (keep quotes on quoted tokens)
    Content := '"alpha"; beta; "gamma delta"';
    ALExtractHeaderFields([';'], [' ', #9], ['"'], PChar(Content), Strings, False);
    Assert.AreEqual(3, Strings.Count, 'Test 62 count');
    Assert.AreEqual(String('"alpha"'), Strings.Names[0], 'Test 62 tok0');
    Assert.AreEqual(String(''), Strings.ValueFromIndex[0], 'Test 62 val0');
    Assert.AreEqual(String('beta'), Strings.Names[1], 'Test 62 tok1');
    Assert.AreEqual(String(''), Strings.ValueFromIndex[1], 'Test 62 val1');
    Assert.AreEqual(String('"gamma delta"'), Strings.Names[2], 'Test 62 tok2');
    Assert.AreEqual(String(''), Strings.ValueFromIndex[2], 'Test 62 val2');
    Strings.Clear;

    // Test 63: Space-as-separator token list, strip OFF (quotes preserved)
    Content := '"name1" "name2" z';
    ALExtractHeaderFields([' '], [' ', #9], ['"'], PChar(Content), Strings, False);
    Assert.AreEqual(3, Strings.Count, 'Test 63 count');
    Assert.AreEqual(String('"name1"'), Strings[0], 'Test 63 tok0');
    Assert.AreEqual(String('"name2"'), Strings[1], 'Test 63 tok1');
    Assert.AreEqual(String('z'), Strings[2], 'Test 63 tok2');
    Strings.Clear;

    // Test 64: Tab-as-separator with quoted tokens, strip ON
    Content := '"a b"'#9'"c d"';
    ALExtractHeaderFields([#9], [' '], ['"'], PChar(Content), Strings, True);
    Assert.AreEqual(2, Strings.Count, 'Test 64 count');
    Assert.AreEqual(String('a b'), Strings[0], 'Test 64 tok0');
    Assert.AreEqual(String('c d'), Strings[1], 'Test 64 tok1');
    Strings.Clear;

  finally
    Strings.Free;
  end;
end;

{****************************************************}
procedure TALDUnitXTestStringUtils.TestALMatchesMaskA;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure DoAssert(const Filename, Mask: string; CaseSensitive: Boolean; Expected: Boolean);
  var
    ActualA, ActualW: Boolean;
  begin
    ActualA := ALMatchesMaskA(AnsiString(Filename), AnsiString(Mask), CaseSensitive);
    ActualW := ALMatchesMaskW(Filename, Mask, CaseSensitive);
    Assert.AreEqual(ActualW, ActualA, Format('Mismatch for "%s" vs "%s" (CaseSens=%s): Expected %s, got %s', [Filename, Mask, BoolToStr(CaseSensitive, True), BoolToStr(Expected, True), BoolToStr(ActualA, True)]));
  end;

begin
  // Exact match, case insensitive (default)
  DoAssert('file.txt', 'file.txt', False, True);
  DoAssert('FILE.TXT', 'file.txt', False, True);
  DoAssert('File.Txt', 'file.txt', False, True);

  // Exact match, case sensitive
  DoAssert('file.txt', 'file.txt', True, True);
  DoAssert('FILE.TXT', 'file.txt', True, False);
  DoAssert('File.Txt', 'file.txt', True, False);

  // Wildcard *
  DoAssert('anything', '*', False, True);
  DoAssert('', '*', False, True);
  DoAssert('file.txt', '*.txt', False, True);
  DoAssert('file.doc', '*.txt', False, False);
  DoAssert('dir/file.txt', 'dir/*.txt', False, True);
  DoAssert('dir/file.doc', 'dir/*.txt', False, False);

  // Wildcard ?
  DoAssert('abc', 'a?c', False, True);
  DoAssert('abc', '???', False, True);
  DoAssert('ab', '???', False, False);
  DoAssert('abcd', '???', False, False);
  DoAssert('a1c', 'a?c', False, True);
  DoAssert('a1C', 'a?c', False, True); // case insensitive

  // Sets [a-z]
  DoAssert('abc', '[a-c]bc', False, True);
  DoAssert('Abc', '[a-c]bc', False, True);
  DoAssert('dbc', '[a-c]bc', False, False);
  DoAssert('ABC', '[a-c]bc', False, True); // case insensitive
  DoAssert('abc', '[a-c]bc', True, True);
  DoAssert('Abc', '[a-c]bc', True, False); // case sensitive

  // Ranges in sets
  DoAssert('a1c', '[a-z09]1[a-z]', False, True);
  DoAssert('a1C', '[a-z09]1[a-z]', False, True);
  DoAssert('a1!', '[a-z09]1[a-z]', False, False);

  // Negated sets [!a-z]
  DoAssert('1bc', '[!a-c]bc', False, True);
  DoAssert('abc', '[!a-c]bc', False, False);
  DoAssert('Abc', '[!a-c]bc', False, False); // A upcases to A, which is not a-c? Wait, assuming ASCII, A is before a, but UpCase('A')='A', and [a-c] is lowercase.
  // Note: In original, sets use UpCase for non-CS, so [a-c] matches A-C as well in non-CS mode.

  // Mixed wildcards and literals
  DoAssert('file123.txt', 'file*.txt', False, True);
  DoAssert('file123.doc', 'file*.txt', False, False);
  DoAssert('test.file', '*file', False, True);
  DoAssert('file.test', '*file', False, False);
  DoAssert('a*b', 'a*b', False, True);
  DoAssert('acb', 'a*b', False, True); // * matches 'c'
  DoAssert('ab', 'a*b', False, True); // * matches nothing
  DoAssert('abb', 'a*b', False, True); // * matches 'b'

  // Complex: multiple *, sets, ?
  DoAssert('dir1/subfile.txt', 'dir?/*.[t-z][x-z][s-t]', False, True);
  DoAssert('dir1/subfile.doc', 'dir?/*.[t-z][x-z][s-t]', False, False);

  // Edge cases
  DoAssert('', '', False, True);
  DoAssert('file', '', False, False); // empty mask shouldn't match non-empty
  DoAssert('', 'file', False, False); // empty file shouldn't match non-empty mask
  DoAssert('', '*', False, True);
  DoAssert(' ', '?', False, True);
  DoAssert('ab', 'a[b-?]', False, True); // ? in set
  DoAssert('a?', 'a?', False, True);
  DoAssert('a?', 'a?', True, True); // literal ? matches ?

  // Invalid masks - but since we trust ALMatchesMaskW, assume it raises EMaskException on invalid, and ALMatchesMaskA should too, but for equality, test valid ones only
  // To test exceptions, could add separate asserts, but keeping to boolean results

  // More set cases
  DoAssert('a1b', 'a[!0-9]b', False, False); // !0-9 should match non-digit, but 1 is digit, so false
  DoAssert('aAb', 'a[!0-9]b', False, True); // A is not digit
  DoAssert('a1b', '[a][!0-9][b]', False, False);

  // Case sensitive sets
  DoAssert('abc', '[A-C]bc', True, False); // [A-C] doesn't match 'a'
  DoAssert('Abc', '[A-C]bc', True, True);


  // Multiple consecutive stars
  DoAssert('abc', '**', False, True);
  DoAssert('abc', '***', False, True);
  DoAssert('abc', 'a**c', False, True);
  DoAssert('abbbc', 'a**c', False, True);
  DoAssert('ac', 'a**c', False, True);
  DoAssert('abbc', 'a****bc', False, True);
  DoAssert('a', '****a****', False, True);
  DoAssert('', '****', False, True);
  DoAssert('x', '**?**', False, True);
  DoAssert('xy', '**??**', False, True);

  // Mixed ? and * at edges
  DoAssert('a', '?*', False, True);
  DoAssert('ab', '?*', False, True);
  DoAssert('', '?*', False, False);
  DoAssert('a', '*?', False, True);
  DoAssert('ab', '*?', False, True);
  DoAssert('', '*?', False, False);
  DoAssert('abc', '?*?', False, True);
  DoAssert('ab', '?*?', False, True);
  DoAssert('a', '?*?', False, False);

  // Exact-length ? sequences
  DoAssert('abcd', '????', False, True);
  DoAssert('abc', '????', False, False);
  DoAssert('abcde', '????', False, False);
  DoAssert('', '', True, True);
  DoAssert('', '?', False, False);

  // Star absorbing everything
  DoAssert('anything.ext', '*.ext', False, True);
  DoAssert('anything.ext', '*.*', False, True);
  DoAssert('.hidden', '*.*', False, True);
  DoAssert('noext', '*.*', False, False);
  DoAssert('a.b.c', '*.*', False, True);
  DoAssert('a.b.c', '*.b.*', False, True);

  // Dots and extensions
  DoAssert('file.', 'file.', False, True);
  DoAssert('file.', 'file.?', False, False);
  DoAssert('file..', 'file..', False, True);
  DoAssert('file..txt', 'file..*', False, True);
  DoAssert('.gitignore', '.*', False, True);
  DoAssert('.config', '.*', False, True);
  DoAssert('..', '..', False, True);
  DoAssert('..', '.*', False, True);
  DoAssert('.', '.', False, True);

  // Sets: simple membership
  DoAssert('a', '[abc]', False, True);
  DoAssert('b', '[abc]', False, True);
  DoAssert('d', '[abc]', False, False);
  DoAssert('A', '[abc]', False, True);
  DoAssert('A', '[abc]', True, False);
  DoAssert('C', '[A-C]', True, True);

  // Sets: multiple segments/ranges
  DoAssert('m', '[a-fm-z]', False, True);
  DoAssert('g', '[a-fm-z]', False, False);
  DoAssert('5', '[0-3 5-9]', False, True); // space is literal member
  DoAssert(' ', '[0-3 5-9]', False, True);

  // Sets: as first char in name
  DoAssert('[', '[[]', False, True);
  DoAssert('[', '[[]', True, True);
  DoAssert('[[', '[[][[]', False, True);

  // Sets: negation
  DoAssert('z', '[!a-y]', False, True);
  DoAssert('a', '[!a-y]', False, False);
  DoAssert('A', '[!a-y]', False, False);
  DoAssert('9', '[!0-8]', False, True);
  DoAssert('8', '[!0-8]', False, False);

  // Sets: used within longer patterns
  DoAssert('cat', 'c[a-z]t', False, True);
  DoAssert('cAt', 'c[a-z]t', False, True);
  DoAssert('cAt', 'c[a-z]t', True, False);
  DoAssert('c.t', 'c[.-/]t', False, True);  // dot is literal in set
  DoAssert('c/t', 'c[.-/]t', False, True);
  DoAssert('c-t', 'c[.-/]t', False, True);
  DoAssert('cxt', 'c[.-/]t', False, False);

  // Mixed cards with sets
  DoAssert('prefixXsuffix', 'prefix*[A-Z]suffix', False, True);
  DoAssert('prefixxsuffix', 'prefix*[A-Z]suffix', False, False);
  DoAssert('pXmidYend', 'p*[A-Z]*[A-Z]end', False, True);
  DoAssert('pXmidyend', 'p*[A-Z]*[A-Z]end', False, False);

  // Case sensitivity toggles
  DoAssert('Readme.MD', '*.md', False, True);
  DoAssert('Readme.MD', '*.md', True, False);
  DoAssert('Δ.txt', '*.TXT', False, True); // non-ASCII; W may consider case-insensitive; we trust W vs A comparison
  DoAssert('Δ.txt', '*.TXT', True, False);

  // Empty + star interplay
  DoAssert('', '***', False, True);
  DoAssert('', '**?**', False, False);
  DoAssert('x', '**?**', False, True);

  // Star placement specifics
  DoAssert('abc', '*abc', False, True);
  DoAssert('xabc', '*abc', False, True);
  DoAssert('xab', '*abc', False, False);
  DoAssert('abc', 'abc*', False, True);
  DoAssert('abccccc', 'abc*', False, True);
  DoAssert('ab', 'abc*', False, False);

  // Deep backtracking
  DoAssert('aaaaab', 'a*a*a*a*b', False, True);
  DoAssert('aaaaac', 'a*a*a*a*b', False, False);
  DoAssert('axbxcxd', 'a*b*c*d', False, True);
  DoAssert('abcd', 'a*b*c*d', False, True);
  DoAssert('acbd', 'a*b*c*d', False, False);

  // Question marks at ends
  DoAssert('a', '?', False, True);
  DoAssert('ab', '?', False, False);
  DoAssert('ab', 'a?', False, True);
  DoAssert('ab', '?b', False, True);
  DoAssert('ab', '??', False, True);
  DoAssert('ab', '???', False, False);

  // Mixed directory-like separators (treated as literals)
  DoAssert('dir/file.txt', 'dir/*', False, True);
  DoAssert('dir/sub/file.txt', 'dir/*', False, False);
  DoAssert('dir\\file.txt', 'dir\\*.txt', False, True);
  DoAssert('dir\\sub\\file.txt', 'dir\\*.txt', False, False);
  DoAssert('dir/sub/file.txt', 'dir/*/*.txt', False, True);
  DoAssert('dir/sub/file.doc', 'dir/*/*.txt', False, False);

  // Literal question mark in name
  DoAssert('what?', 'what?', False, True);
  DoAssert('what?', 'what??', False, False);
  DoAssert('what??', 'what??', False, True);

  // Literal star in name (no escaping in this engine => '*' is wildcard; ensure mismatch when literal star present)
  DoAssert('a*b', 'a*b', False, True); // wildcard absorbs literal '*'
  DoAssert('a*b', 'a**b', False, True);
  DoAssert('a*b', 'a?b', False, False); // '?' cannot match two chars

  // Complex mixed cases
  DoAssert('log-2025-10-05.txt', 'log-????-??-??.txt', False, True);
  DoAssert('log-2025-10-5.txt', 'log-????-??-??.txt', False, False);
  DoAssert('img_0012.jpeg', 'img_*.[jJ][pP][eE][gG]', False, True);
  DoAssert('img_0012.jPeG', 'img_*.[jJ][pP][eE][gG]', True, True);
  DoAssert('img_0012.JPG', '*.jp*g', False, True);
  DoAssert('img_0012.JPG', '*.jp*g', True, False);

  // Boundary of sets with single-member ranges
  DoAssert('a', '[a-a]', False, True);
  DoAssert('b', '[a-a]', False, False);
  DoAssert('0', '[0-0]', False, True);
  DoAssert('1', '[0-0]', False, False);

  // Negation with single member
  DoAssert('x', '[!x]', False, True);
  DoAssert('x', '[!x]', True, False);

  // Spaces in name and mask
  DoAssert('my file.txt', 'my *.txt', False, True);
  DoAssert('myfile.txt', 'my *.txt', False, False);
  DoAssert(' spaced ', '* *', False, True);
  DoAssert('spaced', '* *', False, False);

  // Many cards with minimal content
  DoAssert('a', '*a*', False, True);
  DoAssert('b', '*a*', False, False);
  DoAssert('aba', '*a*', False, True);
  DoAssert('bbb', '*a*', False, False);

  // End anchoring via sentinel (no trailing star)
  DoAssert('abcd', 'abc', False, False);
  DoAssert('abc', 'abc', False, True);
  DoAssert('abc', 'ab?', False, True);
  DoAssert('abcd', 'ab?', False, False);

  // Bracket as literal outside sets using set-trick
  DoAssert('[x]', '[[]x]', False, True);
  DoAssert('[]', '[[]]', False, True);
  DoAssert('[', '[[]', False, True);
  DoAssert('x[', 'x[[]', False, True);

  // Mixed ranges; case variants
  DoAssert('G', '[a-g]', False, True);
  DoAssert('G', '[a-g]', True, False);
  DoAssert('g', '[A-G]', True, False);
  DoAssert('G', '[A-G]', True, True);

  // Star consuming nothing between literals
  DoAssert('ab', 'a*b', False, True);
  DoAssert('aXb', 'a*b', False, True);
  DoAssert('aXYZb', 'a*b', False, True);
  DoAssert('ab', 'a**b', False, True);

  // Multi-dot tricky
  DoAssert('a..b', 'a..b', False, True);
  DoAssert('a..b', 'a.*.b', False, True);
  DoAssert('a...b', 'a.*.b', False, True);
  DoAssert('a.b', 'a.*.b', False, True);
  DoAssert('a.b', 'a..b', False, False);

  // Numeric-looking names
  DoAssert('12345', '12*45', False, True);
  DoAssert('1245', '12*45', False, True);
  DoAssert('125', '12*45', False, False);
  DoAssert('12945', '12?45', False, True);
  DoAssert('12A45', '12?45', False, True);
  DoAssert('12AA45', '12?45', False, False);

  // Long names with scattered wildcards
  DoAssert('alpha_beta_gamma_delta', 'a*_*_*_delta', False, True);
  DoAssert('alpha_beta_gamma_theta', 'a*_*_*_delta', False, False);

  // Question marks crossing separators (still literal match)
  DoAssert('a/b', 'a/?', False, True);
  DoAssert('a/bc', 'a/?', False, False);
  DoAssert('a\\b', 'a\\?', False, True);

  // Star + set right after
  DoAssert('fooX', '*[A-Z]', False, True);
  DoAssert('foox', '*[A-Z]', False, False);
  DoAssert('foo9', '*[0-9]', False, True);
  DoAssert('foo/', '*[0-9]', False, False);

  // Star in the middle with minimal remainder
  DoAssert('abc', 'a*c', False, True);
  DoAssert('ac', 'a*c', False, True);
  DoAssert('abdc', 'a*c', False, True);
  DoAssert('abdc', 'a*d', False, False);

  // Very long star coverage
  DoAssert('this_is_a_very_long_filename.ext', 'this*ext', False, True);
  DoAssert('this_is_a_very_long_filename.ext', 'that*ext', False, False);
  DoAssert('prefix_middle_suffix', 'prefix*suffix', False, True);
  DoAssert('prefix_middle_suf', 'prefix*suffix', False, False);
end;

initialization
  TDUnitX.RegisterTestFixture(TALDUnitXTestStringUtils);

end.