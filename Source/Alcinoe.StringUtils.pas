(*******************************************************************************
There are 8-bit strings (AnsiString, RawByteString, UTF8String, etc.) and
16-bit strings (String, WideString, etc.). Both can be Unicode (UTF8 in 8-bit
or UTF16 in 16-bit). In server applications (such as web servers), it's
preferable to stay in 8-bit strings (but in Unicode with UTF8) as these apps
generally receive their input requests in UTF8 and output their responses in
UTF8. Moving to UTF16 in this case would mean converting the input (UTF8) to
UTF16, processing the data, converting back to UTF8 for output. In fact,
except for input/output to the "visual interface", most (if not all) of the
input/output of most applications will be done in 8-bit strings (e.g. file
storage, client/server protocol, HTTP, SMTP, TCP, XML, JSON, HTML, database,
etc.). Here's a very good article about why you should avoid using
UTF-16: http://www.utf8everywhere.org/.

Unfortunately, most of the useful and needed string functions in D2009+ are
not available in 8-bit strings, such as inttostr, strtoint, strtofloat,
Tstrings, TstringStream, etc. Of course, we can still do
ansistring(inttostr(x)), but this is very inefficient if you need to do it
often. So, I decided to create an "8-bit string framework" to fully support
8-bit strings (but mostly in Unicode UTF8, as I think local strings should
absolutely be avoided).

In Alcinoe, 8-bit string functions/classes will generally be prefixed with
ALxxxA (A for Ansi String), and 16-bit string functions/classes will be
prefixed with ALxxxW (W for Wide/Unicode String). This is inspired from
https://learn.microsoft.com/en-us/windows/win32/learnwin32/working-with-strings
Under D2009+, AnsiString now has a codepage, and some transliteration
(OldCodePage => UTF16 => NewCodePage) will occur when assigning one AnsiString
with a different codepage to another AnsiString with a different codepage. To
avoid this, it's important to always set the project option to the code page
you want (e.g. 65001 for UTF8) and also to call
SetMultiByteConversionCodePage(CP_UTF8) at the beginning of the program.
If you don't set the default codepage to be CP_UTF8 and instead use Local
AnsiString in your app, it's a bad design!

Note: In Win64, we lost all the FastCode heritage (which was mostly based on
Win32 ASM). This means that most of the functions will be around 2x to 10x
slower. You can try launching /demo/ALStringBenchMark/ in Win64 and Win32 to
see the difference in speed.
*******************************************************************************)
unit Alcinoe.StringUtils;

interface

{$I Alcinoe.inc}

{.$H+} {Long string}
{.$B-} {Boolean short-circuit evaluation}
{.$R-} {Range-Checking}

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.Contnrs,
  {$IFDEF MACOS}
  Macapi.CoreFoundation,
  {$ENDIF MACOS}
  Alcinoe.StringList,
  Alcinoe.Common;

resourcestring
  SALInvalidFormat = 'Format ''%s'' invalid or incompatible with argument';
  SALArgumentMissing = 'No argument for format ''%s''';

type

  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if System.SysUtils.TFormatSettings is still the same and adjust the IFDEF'}
  {$ENDIF}

  pALFormatSettingsA = ^TALFormatSettingsA;
  TALFormatSettingsA = record
  public
    type
      TEraInfo = record
        EraName: ansiString;
        EraOffset: Integer;
        EraStart: TDate;
        EraEnd: TDate;
      end;
  public
    // Important: Do not change the order of these declarations, they must
    // match the declaration order of the corresponding globals variables exactly!
    CurrencyString: AnsiString;
    CurrencyFormat: Byte;
    CurrencyDecimals: Byte;
    DateSeparator: AnsiChar;
    TimeSeparator: AnsiChar;
    ListSeparator: AnsiString;
    ShortDateFormat: AnsiString;
    LongDateFormat: AnsiString;
    TimeAMString: AnsiString;
    TimePMString: AnsiString;
    ShortTimeFormat: AnsiString;
    LongTimeFormat: AnsiString;
    ShortMonthNames: array[1..12] of AnsiString;
    LongMonthNames: array[1..12] of AnsiString;
    ShortDayNames: array[1..7] of AnsiString;
    LongDayNames: array[1..7] of AnsiString;
    EraInfo: array of TEraInfo;
    ThousandSeparator: AnsiString;
    DecimalSeparator: AnsiChar;
    TwoDigitYearCenturyWindow: Word;
    NegCurrFormat: Byte;
    // Creates a TALFormatSettingsA record with current default values provided
    // by the operating system.
    class function Create: TALFormatSettingsA; overload; static; inline;
    // Creates a TALFormatSettingsA record with values provided by the operating
    // system for the specified locale. The locale is an LCID on Windows
    // platforms, or a locale_t on Posix platforms.
    {$IF defined(MSWINDOWS)}
    class function Create(Locale: LCID): TALFormatSettingsA; overload; platform; static;
    {$ENDIF}
    // Creates a TALFormatSettingsA record with values provided by the operating
    // system for the specified locale name in the "Language-Country" format.
    // Example: 'en-US' for U.S. English settings or 'en-UK' for UK English settings.
    class function Create(const LocaleName: AnsiString): TALFormatSettingsA; overload; static;
    function GetEraYearOffset(const Name: ansistring): Integer;
  end;

  pALFormatSettingsW = ^TALFormatSettingsW;
  TALFormatSettingsW = TFormatSettings;

  function ALGetFormatSettingsID(const aFormatSettings: TALFormatSettingsA): AnsiString;
  {$IF defined(MSWINDOWS)}
  procedure ALGetLocaleFormatSettings(Locale: LCID; var AFormatSettings: TALFormatSettingsA); platform;
  {$ENDIF}

var
  ALDefaultFormatSettingsA: TALformatSettingsA;
  ALDefaultFormatSettingsW: TALformatSettingsW;

Const
  cAlUTF8Bom = ansiString(#$EF) + ansiString(#$BB) + ansiString(#$BF);
  cAlUTF16LittleEndianBom = ansiString(#$FF) + ansiString(#$FE);
  cAlUTF16bigEndianBom = ansiString(#$FE) + ansiString(#$FF);
  cAlUTF32LittleEndianBom = ansiString(#$FF) + ansiString(#$FE) + ansiString(#$00) + ansiString(#$00);
  cAlUTF32BigEndianBom = ansiString(#$00) + ansiString(#$00) + ansiString(#$FE) + ansiString(#$FF);

type

  TALStringStreamA = class(TStream)
  private
    FDataString: AnsiString;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const AString: AnsiString);
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadString(Count: Longint): AnsiString;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure WriteString(const AString: AnsiString);
    property DataString: AnsiString read FDataString;
  end;
  TALStringStreamW = class(TStringStream);

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if System.Masks.pas is still the same and adjust the IFDEF'}
  {$ENDIF}

  EALMaskException = class(EALException);

  TALMaskA = class
  private type
    // WideChar Reduced to ByteChar in set expressions.
    TMaskSet = set of ansiChar;
    PMaskSet = ^TMaskSet;
    TMaskStates = (msLiteral, msAny, msSet, msMBCSLiteral);
    TMaskState = record
      SkipTo: Boolean;
      case State: TMaskStates of
        TMaskStates.msLiteral: (Literal: ansiChar);
        TMaskStates.msAny: ();
        TMaskStates.msSet: (
          Negate: Boolean;
          CharSet: PMaskSet);
        TMaskStates.msMBCSLiteral: (LeadByte, TrailByte: ansiChar);
    end;
  private
    FMaskStates: array of TMaskState;
  protected
    function InitMaskStates(const Mask: ansistring): Integer;
    procedure DoneMaskStates;
    function MatchesMaskStates(const Filename: ansistring): Boolean;
  public
    constructor Create(const MaskValue: ansistring);
    destructor Destroy; override;
    function Matches(const Filename: ansistring): Boolean;
  end;

function  ALMatchesMaskA(const Filename, Mask: AnsiString): Boolean;
function  ALMatchesMaskW(const Filename, Mask: String): Boolean; inline;

Function  ALNewGUIDBytes: TBytes;
function  ALGUIDToByteString(const Guid: TGUID): Ansistring;
function  ALNewGUIDByteString: Ansistring;
function  ALGUIDToStringA(const Guid: TGUID; const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): Ansistring;
function  ALGUIDToStringW(const Guid: TGUID; const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): string;
Function  ALNewGUIDStringA(const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): AnsiString;
Function  ALNewGUIDStringW(const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): String;
function  ALFormatA(const Format: AnsiString; const Args: array of const): AnsiString; overload;
procedure ALFormatA(const Format: AnsiString; const Args: array of const; var Result: ansiString); overload;
function  ALFormatA(const Format: AnsiString; const Args: array of const; const AFormatSettings: TALFormatSettingsA): AnsiString; overload;
procedure ALFormatA(const Format: AnsiString; const Args: array of const; const AFormatSettings: TALFormatSettingsA; var Result: ansiString); overload;
function  ALFormatW(const Format: String; const Args: array of const): String; overload;
procedure ALFormatW(const Format: String; const Args: array of const; var Result: String); overload;
function  ALFormatW(const Format: String; const Args: array of const; const AFormatSettings: TALFormatSettingsW): String; overload;
procedure ALFormatW(const Format: String; const Args: array of const; const AFormatSettings: TALFormatSettingsW; var Result: String); overload;
function  ALTryStrToBool(const S: Ansistring; out Value: Boolean): Boolean; overload;
function  ALTryStrToBool(const S: String; out Value: Boolean): Boolean; overload;
Function  AlStrToBool(Value:AnsiString):Boolean; overload;
Function  AlStrToBool(Value:String):Boolean; overload;
function  ALBoolToStrA(B: Boolean; const trueStr: ansistring='1'; const falseStr: ansistring='0'): Ansistring; overload;
procedure ALBoolToStrA(var s: ansiString; B: Boolean; const trueStr: ansistring='1'; const falseStr: ansistring='0'); overload;
function  ALBoolToStrW(B: Boolean; const trueStr: String='1'; const falseStr: String='0'): String; overload;
procedure ALBoolToStrW(var s: String; B: Boolean; const trueStr: String='1'; const falseStr: String='0'); overload;
function  ALDateToStrA(const DateTime: TDateTime; const AFormatSettings: TALFormatSettingsA): AnsiString;
function  ALDateToStrW(const DateTime: TDateTime; const AFormatSettings: TALFormatSettingsW): string; inline;
function  ALTimeToStrA(const DateTime: TDateTime; const AFormatSettings: TALFormatSettingsA): AnsiString;
function  ALTimeToStrW(const DateTime: TDateTime; const AFormatSettings: TALFormatSettingsW): string; inline;
function  ALDateTimeToStrA(const DateTime: TDateTime; const AFormatSettings: TALFormatSettingsA): AnsiString; overload;
procedure ALDateTimeToStrA(const DateTime: TDateTime; var s: ansiString; const AFormatSettings: TALFormatSettingsA); overload;
function  ALDateTimeToStrW(const DateTime: TDateTime; const AFormatSettings: TALFormatSettingsW): String; overload; inline;
procedure ALDateTimeToStrW(const DateTime: TDateTime; var s: String; const AFormatSettings: TALFormatSettingsW); overload; inline;
function  ALFormatDateTimeA(const Format: AnsiString; DateTime: TDateTime; const AFormatSettings: TALFormatSettingsA): AnsiString;
function  ALFormatDateTimeW(const Format: string; DateTime: TDateTime; const AFormatSettings: TALFormatSettingsW): string; inline;
function  ALTryStrToDate(const S: AnsiString; out Value: TDateTime; const AFormatSettings: TALFormatSettingsA): Boolean; overload;
function  ALTryStrToDate(const S: string; out Value: TDateTime; const AFormatSettings: TALFormatSettingsW): Boolean; overload; inline;
function  ALStrToDate(const S: AnsiString; const AFormatSettings: TALFormatSettingsA): TDateTime; overload;
function  ALStrToDate(const S: string; const AFormatSettings: TALFormatSettingsW): TDateTime; overload; inline;
function  ALTryStrToTime(const S: AnsiString; out Value: TDateTime; const AFormatSettings: TALFormatSettingsA): Boolean; overload;
function  ALTryStrToTime(const S: string; out Value: TDateTime; const AFormatSettings: TALFormatSettingsW): Boolean; overload; inline;
function  ALStrToTime(const S: AnsiString; const AFormatSettings: TALFormatSettingsA): TDateTime; overload;
function  ALStrToTime(const S: string; const AFormatSettings: TALFormatSettingsW): TDateTime; overload; inline;
function  ALTryStrToDateTime(const S: AnsiString; out Value: TDateTime; const AFormatSettings: TALFormatSettingsA): Boolean; overload;
function  ALTryStrToDateTime(const S: string; out Value: TDateTime; const AFormatSettings: TALFormatSettingsW): Boolean; overload; inline;
function  ALStrToDateTime(const S: AnsiString; const AFormatSettings: TALFormatSettingsA): TDateTime; overload;
function  ALStrToDateTime(const S: string; const AFormatSettings: TALFormatSettingsW): TDateTime; overload; inline;
function  ALTryStrToInt(const S: AnsiString; out Value: Integer): Boolean; overload;
function  ALTryStrToInt(const S: string; out Value: Integer): Boolean; overload; inline;
function  ALStrToInt(const S: AnsiString): Integer; overload;
function  ALStrToInt(const S: string): Integer; overload; inline;
function  ALStrToIntDef(const S: AnsiString; Default: Integer): Integer; overload;
function  ALStrToIntDef(const S: string; Default: Integer): Integer; overload; inline;
function  ALTryStrToUInt(const S: AnsiString; out Value: Cardinal): Boolean; overload;
function  ALTryStrToUInt(const S: String; out Value: Cardinal): Boolean; overload; inline;
function  ALStrToUInt(const S: AnsiString): Cardinal; overload;
function  ALStrToUInt(const S: String): Cardinal; overload; inline;
function  ALStrToUIntDef(const S: Ansistring; Default: Cardinal): Cardinal; overload;
function  ALStrToUIntDef(const S: string; Default: Cardinal): Cardinal; overload; inline;
function  ALTryStrToInt64(const S: AnsiString; out Value: Int64): Boolean; overload;
function  ALTryStrToInt64(const S: string; out Value: Int64): Boolean; overload; inline;
function  ALStrToInt64(const S: AnsiString): Int64; overload;
function  ALStrToInt64(const S: string): Int64; overload; inline;
function  ALStrToInt64Def(const S: AnsiString; const Default: Int64): Int64; overload;
function  ALStrToInt64Def(const S: string; const Default: Int64): Int64; overload; inline;
function  ALTryStrToUInt64(const S: ansistring; out Value: UInt64): Boolean; overload;
function  ALTryStrToUInt64(const S: String; out Value: UInt64): Boolean; overload; inline;
function  ALStrToUInt64(const S: ansistring): UInt64; overload;
function  ALStrToUInt64(const S: String): UInt64; overload; inline;
function  ALStrToUInt64Def(const S: ansistring; const Default: UInt64): UInt64; overload;
function  ALStrToUInt64Def(const S: String; const Default: UInt64): UInt64; overload; inline;
function  ALIntToStrA(Value: Integer): AnsiString; overload;
procedure ALIntToStrA(Value: Integer; var s: ansiString); overload;
function  ALIntToStrA(Value: Int64): AnsiString; overload;
procedure ALIntToStrA(Value: Int64; var s: ansiString); overload;
function  ALIntToStrW(Value: Integer): String; overload; inline;
procedure ALIntToStrW(Value: Integer; var s: String); overload; inline;
function  ALIntToStrW(Value: Int64): String; overload; inline;
procedure ALIntToStrW(Value: Int64; var s: String); overload; inline;
function  ALUIntToStrA(Value: Cardinal): AnsiString; overload;
function  ALUIntToStrA(Value: UInt64): AnsiString; overload;
function  ALUIntToStrW(Value: Cardinal): String; overload; inline;
function  ALUIntToStrW(Value: UInt64): String; overload; inline;
function  ALIntToHexA(Value: Integer; Digits: Integer): AnsiString; overload;
function  ALIntToHexA(Value: Int64; Digits: Integer): AnsiString; overload;
function  ALIntToHexA(Value: UInt64; Digits: Integer): AnsiString; overload;
function  ALIntToHexW(Value: Integer; Digits: Integer): String; inline; overload;
function  ALIntToHexW(Value: Int64; Digits: Integer): String; inline; overload;
function  ALIntToHexW(Value: UInt64; Digits: Integer): String; inline; overload;
function  ALTryBinToHex(const aBin: AnsiString; out Value: AnsiString): boolean; overload;
function  ALTryBinToHex(const aBin; aBinSize : Cardinal; out Value: AnsiString): boolean; overload;
function  ALTryBinToHex(const aBin: Tbytes; out Value: String): boolean; overload;
function  ALTryBinToHex(const aBin; aBinSize : Cardinal; out Value: String): boolean; overload;
function  ALBinToHexA(const aBin: AnsiString): AnsiString; overload;
Function  ALBinToHexA(const aBin; aBinSize : Cardinal): AnsiString; overload;
Function  ALBinToHexW(const aBin: Tbytes): String; overload;
Function  ALBinToHexW(const aBin; aBinSize : Cardinal): String; overload;
Function  ALTryHexToBin(const aHex: AnsiString; out Value: AnsiString): boolean; overload;
Function  ALTryHexToBin(const aHex: String; out Value: Tbytes): boolean; overload;
Function  ALHexToBin(const aHex: AnsiString): AnsiString; overload;
Function  ALHexToBin(const aHex: String): Tbytes; overload;
function  ALIntToBitA(value: Integer; digits: integer): ansistring;
function  AlBitToInt(Value: ansiString): Integer;
function  AlInt2BaseN(NumIn: UInt64; const charset: array of ansiChar): ansistring;
function  AlBaseN2Int(const Str: ansiString; const charset: array of ansiChar): UInt64;
function  ALBase64EncodeString(const P: PansiChar; const ln: Integer): AnsiString; overload;
function  ALBase64EncodeString(const S: AnsiString): AnsiString; overload;
Function  ALBase64EncodeString(const S: String; const AEncoding: TEncoding = nil): String; overload;
function  ALBase64DecodeString(const P: PansiChar; const ln: Integer): AnsiString; overload;
function  ALBase64DecodeString(const S: AnsiString): AnsiString; overload;
Function  ALBase64DecodeString(const S: String; const AEncoding: TEncoding = nil): String; overload;
function  ALBase64EncodeStringMIME(const S: AnsiString): AnsiString;
function  ALBase64DecodeStringMIME(const S: AnsiString): AnsiString;
function  ALURLBase64EncodeString(const S: AnsiString; const aDoOnlyUrlEncode: boolean = false): AnsiString;
function  ALURLBase64DecodeString(const S: AnsiString; const aDoOnlyUrlDecode: boolean = false): AnsiString;
Function  ALBase64EncodeBytesW(const Bytes: Tbytes): String; overload;
Function  ALBase64EncodeBytesW(const Bytes: pointer; const Size: Integer): String; overload;
Function  ALBase64DecodeBytes(const S: String): Tbytes;
function  ALIsDecimal(const S: AnsiString; const RejectPlusMinusSign: boolean = False): boolean; overload;
function  ALIsDecimal(const S: String; const RejectPlusMinusSign: boolean = False): boolean; overload;
Function  ALIsInteger(const S: AnsiString): Boolean; overload;
Function  ALIsInteger(const S: String): Boolean; overload;
Function  ALIsInt64(const S: AnsiString): Boolean; overload;
Function  ALIsInt64(const S: String): Boolean; overload;
Function  ALIsSmallInt(const S: AnsiString): Boolean; overload;
Function  ALIsSmallInt(const S: String): Boolean; overload;
Function  ALIsFloat(const S: AnsiString; const AFormatSettings: TALFormatSettingsA): Boolean; overload;
Function  ALIsFloat(const S: String; const AFormatSettings: TALFormatSettingsW): Boolean; overload;
function  ALFloatToStrA(Value: Extended; const AFormatSettings: TALFormatSettingsA): AnsiString; overload;
procedure ALFloatToStrA(Value: Extended; var S: ansiString; const AFormatSettings: TALFormatSettingsA); overload;
function  ALFloatToStrW(Value: Extended; const AFormatSettings: TALFormatSettingsW): String; overload; inline;
procedure ALFloatToStrW(Value: Extended; var S: String; const AFormatSettings: TALFormatSettingsW); overload; inline;
function  ALFloatToStrFA(Value: Extended; Format: TFloatFormat; Precision, Digits: Integer; const AFormatSettings: TALFormatSettingsA): AnsiString;
function  ALFloatToStrFW(Value: Extended; Format: TFloatFormat; Precision, Digits: Integer; const AFormatSettings: TALFormatSettingsW): String; inline;
function  ALCurrToStrA(Value: Currency; const AFormatSettings: TALFormatSettingsA): AnsiString;
function  ALCurrToStrW(Value: Currency; const AFormatSettings: TALFormatSettingsW): string; inline;
function  ALFormatFloatA(const Format: AnsiString; Value: Extended; const AFormatSettings: TALFormatSettingsA): AnsiString;
function  ALFormatFloatW(const Format: string; Value: Extended; const AFormatSettings: TALFormatSettingsW): string; overload; inline;
function  ALFormatFloatW(const Format: string; Value: Extended): string; overload; inline;
function  ALFormatCurrA(const Format: AnsiString; Value: Currency; const AFormatSettings: TALFormatSettingsA): AnsiString;
function  ALFormatCurrW(const Format: string; Value: Currency; const AFormatSettings: TALFormatSettingsW): string; overload; inline;
function  ALFormatCurrW(const Format: string; Value: Currency): string; overload; inline;
function  ALStrToFloat(const S: AnsiString; const AFormatSettings: TALFormatSettingsA): Extended; overload;
function  ALStrToFloat(const S: string; const AFormatSettings: TALFormatSettingsW): Extended; overload; inline;
function  ALStrToFloatDef(const S: AnsiString; const Default: Extended; const AFormatSettings: TALFormatSettingsA): Extended; overload;
function  ALStrToFloatDef(const S: string; const Default: Extended; const AFormatSettings: TALFormatSettingsW): Extended; overload; inline;
function  ALTryStrToFloat(const S: AnsiString; out Value: Extended; const AFormatSettings: TALFormatSettingsA): Boolean; overload;
function  ALTryStrToFloat(const S: String; out Value: Extended; const AFormatSettings: TALFormatSettingsW): Boolean; overload; inline;
function  ALTryStrToFloat(const S: AnsiString; out Value: Double; const AFormatSettings: TALFormatSettingsA): Boolean; overload;
function  ALTryStrToFloat(const S: String; out Value: Double; const AFormatSettings: TALFormatSettingsW): Boolean; overload; inline;
function  ALTryStrToFloat(const S: AnsiString; out Value: Single; const AFormatSettings: TALFormatSettingsA): Boolean; overload;
function  ALTryStrToFloat(const S: String; out Value: Single; const AFormatSettings: TALFormatSettingsW): Boolean; overload; inline;
function  ALStrToCurr(const S: AnsiString; const AFormatSettings: TALFormatSettingsA): Currency; overload;
function  ALStrToCurr(const S: string; const AFormatSettings: TALFormatSettingsW): Currency; overload; inline;
function  ALStrToCurrDef(const S: AnsiString; const Default: Currency; const AFormatSettings: TALFormatSettingsA): Currency; overload;
function  ALStrToCurrDef(const S: string; const Default: Currency; const AFormatSettings: TALFormatSettingsW): Currency; overload; inline;
function  ALTryStrToCurr(const S: AnsiString; out Value: Currency; const AFormatSettings: TALFormatSettingsA): Boolean; overload;
function  ALTryStrToCurr(const S: string; out Value: Currency; const AFormatSettings: TALFormatSettingsW): Boolean; overload; inline;
function  ALPosA(const SubStr, Str: AnsiString; const Offset: Integer = 1): Integer; inline;
function  ALPosW(const SubStr, Str: String; const Offset: Integer = 1): Integer; inline;
function  ALPosIgnoreCaseA(const SubStr, S: Ansistring; const Offset: Integer = 1): Integer;
function  ALPosIgnoreCaseW(const SubStr, S: String; const Offset: Integer = 1): Integer;
function  ALCompareStrA(const S1, S2: AnsiString): Integer; inline;
function  ALCompareStrW(const S1, S2: string): Integer; inline;
function  ALSameStrA(const S1, S2: AnsiString): Boolean; inline;
function  ALSameStrW(const S1, S2: string): Boolean; inline;
function  ALCompareTextA(const S1, S2: AnsiString): Integer; inline;
function  ALCompareTextW(const S1, S2: string): Integer; inline;
function  ALSameTextA(const S1, S2: AnsiString): Boolean; inline;
function  ALSameTextW(const S1, S2: string): Boolean; inline;
function  ALMatchTextA(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
function  ALMatchTextW(const AText: String; const AValues: array of String): Boolean;
function  ALMatchStrA(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
function  ALMatchStrW(const AText: String; const AValues: array of String): Boolean;
function  ALTrim(const S: AnsiString): AnsiString; overload;
function  ALTrim(const S: string): string; overload; inline;
function  ALTrimLeft(const S: AnsiString): AnsiString; overload;
function  ALTrimLeft(const S: string): string; overload; inline;
function  ALTrimRight(const S: AnsiString): AnsiString; overload;
function  ALTrimRight(const S: string): string; overload; inline;
function  ALPadLeft(const S: AnsiString; Const Width: Integer): AnsiString; overload;
function  ALPadLeft(const S: String; Const Width: Integer): String; overload;
function  ALPadRight(const S: AnsiString; Const Width: Integer): AnsiString; overload;
function  ALPadRight(const S: String; Const Width: Integer): String; overload;
function  ALQuotedStr(const S: AnsiString; const Quote: AnsiChar = ''''): AnsiString; overload;
function  ALQuotedStr(const S: String; const Quote: Char = ''''): String; overload;
function  ALExtractQuotedStr(var Src: PAnsiChar; Quote: AnsiChar): AnsiString; overload;
function  ALExtractQuotedStr(var Src: PChar; Quote: Char): String; overload; inline;
function  ALDequotedStr(const S: AnsiString; AQuote: AnsiChar): AnsiString; overload;
function  ALDequotedStr(const S: string; AQuote: Char): string; overload; inline;
function  ALExtractFilePath(const FileName: AnsiString): AnsiString;overload; inline;
function  ALExtractFilePath(const FileName: String): String;overload; inline;
function  ALExtractFileDir(const FileName: AnsiString): AnsiString;overload; inline;
function  ALExtractFileDir(const FileName: String): String;overload; inline;
function  ALExtractFileDrive(const FileName: AnsiString): AnsiString;overload; inline;
function  ALExtractFileDrive(const FileName: String): String;overload; inline;
function  ALExtractFileName(const FileName: AnsiString; const RemoveFileExt: Boolean=false): AnsiString; overload;
function  ALExtractFileName(const FileName: String; const RemoveFileExt: Boolean=false): String; overload;
function  ALExtractFileExt(const FileName: AnsiString): AnsiString;overload; inline;
function  ALExtractFileExt(const FileName: String): String;overload; inline;
function  ALLastDelimiterA(const Delimiters, S: AnsiString): Integer;
function  ALLastDelimiterW(const Delimiters, S: string): Integer; inline;
function  ALIsPathDelimiter(const S: AnsiString; Index: Integer; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): Boolean; overload;
function  ALIsPathDelimiter(const S: String; Index: Integer; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): Boolean; overload;
function  ALIncludeTrailingPathDelimiterA(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString; overload;
function  ALIncludeTrailingPathDelimiterW(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String; overload;
function  ALExcludeTrailingPathDelimiterA(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString; overload;
function  ALExcludeTrailingPathDelimiterW(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String; overload;
function  ALIncludeLeadingPathDelimiterA(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString; overload;
function  ALIncludeLeadingPathDelimiterW(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String; overload;
function  ALExcludeLeadingPathDelimiterA(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString; overload;
function  ALExcludeLeadingPathDelimiterW(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String; overload;
procedure ALStrMove(const Source: PAnsiChar; var Dest: PAnsiChar; Count: NativeInt); overload; inline;
procedure ALStrMove(const Source: PChar; var Dest: PChar; Count: NativeInt); overload; inline;
function  ALCopyStr(const aSourceString: AnsiString; aStart, aLength: Integer): AnsiString; overload;
function  ALCopyStr(const aSourceString: String; aStart, aLength: Integer): String; overload;
procedure ALCopyStr(const aSourceString: AnsiString; var aDestString: ansiString; aStart, aLength: Integer); overload;
procedure ALCopyStr(const aSourceString: String; var aDestString: String; aStart, aLength: Integer); overload;
function  ALCopyStr(
            const aSourceString: AnsiString;
            const aStartStr: AnsiString;
            const aEndStr: AnsiString;
            const aOffset: integer = 1;
            const aRaiseExceptionIfNotFound: Boolean = True): AnsiString; overload;
function  ALCopyStr(
            const aSourceString: String;
            const aStartStr: String;
            const aEndStr: String;
            const aOffset: integer = 1;
            const aRaiseExceptionIfNotFound: Boolean = True): String; overload;
function  ALStringReplaceA(const Source, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString; inline;
function  ALStringReplaceW(const Source, OldPattern, NewPattern: string; Flags: TReplaceFlags): string; inline;
function  ALRandomStrA(const aLength: Longint; const aCharset: Array of ansiChar): AnsiString; overload;
function  ALRandomStrA(const aLength: Longint): AnsiString; overload;
function  ALRandomStrW(const aLength: Longint; const aCharset: Array of Char): String; overload;
function  ALRandomStrW(const aLength: Longint): String; overload;
function  ALNEVExtractName(const S: AnsiString): AnsiString;
function  ALNEVExtractValue(const s: AnsiString): AnsiString;
function  ALGetBytesFromStream(const aStream : TStream): Tbytes;
function  ALGetBytesFromFile(const filename: ansiString; const ShareMode: Word = fmShareDenyWrite): Tbytes; overload;
function  ALGetBytesFromFile(const filename: String; const ShareMode: Word = fmShareDenyWrite): Tbytes; overload;
function  ALGetStringFromBuffer(const buf : TBytes; const ADefaultEncoding: TEncoding): String;
function  ALGetStringFromStream(const aStream : TStream; const ADefaultEncoding: TEncoding) : String;
function  ALGetStringFromFile(const filename: AnsiString; const ShareMode: Word = fmShareDenyWrite): AnsiString; overload;
function  ALGetStringFromFile(const filename: String; const ShareMode: Word = fmShareDenyWrite): AnsiString; overload;
function  ALGetStringFromFile(const filename: String; const ADefaultEncoding: TEncoding; const ShareMode: Word = fmShareDenyWrite): String; overload;
function  ALGetStringFromFileWithoutUTF8BOM(const filename: AnsiString; const ShareMode: Word = fmShareDenyWrite): AnsiString; overload;
function  ALGetStringFromFileWithoutUTF8BOM(const filename: String; const ShareMode: Word = fmShareDenyWrite): AnsiString; overload;
procedure ALAppendStringToFile(const Str: AnsiString; const FileName: AnsiString); overload;
procedure ALAppendStringToFile(const Str: AnsiString; const FileName: String); overload;
procedure ALSaveStringtoFile(const Str: AnsiString; const filename: AnsiString); overload;
procedure ALSaveStringtoFile(const Str: AnsiString; const filename: String); overload;
procedure ALSaveStringtoFile(const Str: String; const filename: String; AEncoding: TEncoding; const WriteBOM: boolean = False); overload;
{$IF defined(MSWINDOWS)}
Function  ALNormalize(
            const S: AnsiString;
            const WordSeparator: ansiChar;
            const SymbolsToIgnore: array of AnsiChar): AnsiString; overload;
Function  ALNormalize(
            const S: AnsiString;
            const WordSeparator: ansiChar = '-'): AnsiString; overload;
Function  ALNormalize(
            const S: Widestring;
            const WordSeparator: WideChar;
            const SymbolsToIgnore: array of WideChar): Widestring; overload;
Function  ALNormalize(
            const S: Widestring;
            const WordSeparator: WideChar = '-'): Widestring; overload;
Function  ALRemoveDiacritic(const S: AnsiString): AnsiString; overload;
Function  ALRemoveDiacritic(const S: Widestring): Widestring; overload;
Function  ALExpandLigatures(const S: AnsiString): AnsiString; overload;
Function  ALExpandLigatures(const S: Widestring): Widestring; overload;
{$ENDIF}
function  AlUpperCase(const S: AnsiString): AnsiString; overload; inline;
function  AlUpperCase(const S: string): string; overload; inline;
function  AlLowerCase(const S: AnsiString): AnsiString; overload; inline;
function  AlLowerCase(const S: string): string; overload; inline;
function  AlUpCase(const Ch: AnsiChar): AnsiChar; overload;
function  AlUpCase(Ch: Char): Char; overload; inline;
function  AlLoCase(const Ch: AnsiChar): AnsiChar; overload;
function  AlLoCase(Ch: Char): Char; overload;
function  ALUnicodeUpperCase(const s: AnsiString): AnsiString; overload; inline;
function  ALUnicodeUpperCase(const s: String): String; overload; inline;
function  ALUnicodeLowerCase(const s: AnsiString): AnsiString; overload; inline;
function  ALUnicodeLowerCase(const s: String): String; overload; inline;
{$IF defined(MSWINDOWS)}
Function  ALUnicodeUpperCaseNoDiacritic(const S: AnsiString): AnsiString; overload;
Function  ALUnicodeUpperCaseNoDiacritic(const S: Widestring): Widestring; overload;
Function  ALUnicodeLowerCaseNoDiacritic(const S: AnsiString): AnsiString; overload;
Function  ALUnicodeLowerCaseNoDiacritic(const S: Widestring): Widestring; overload;
{$ENDIF}
Function  ALUnicodeUpperFirstChar(const s:AnsiString): AnsiString;
Function  ALTitleCase(const s:AnsiString): AnsiString; overload;
Function  ALTitleCase(const s: String): String; overload;
Function  ALSentenceCase(const s:AnsiString): AnsiString; overload;
Function  ALSentenceCase(const s: String): String; overload;
{$IF defined(MSWINDOWS)}
Function  ALStringToWideString(const S: RawByteString; const aCodePage: Word): WideString;
function  AlWideStringToString(const WS: WideString; const aCodePage: Word): AnsiString;
{$ENDIF}
Function  ALISO91995CyrillicToLatin(const aCyrillicText: AnsiString): AnsiString;
Function  ALBGNPCGN1947CyrillicToLatin(const aCyrillicText: AnsiString): AnsiString;
{$IF defined(MSWINDOWS)}
function  AlUTF8Check(const S: AnsiString): Boolean;
{$ENDIF}
function  AlUTF8DetectBOM(const P: PAnsiChar; const Size: Integer): Boolean;
function  AlUTF8removeBOM(const S: AnsiString): AnsiString;
function  ALUTF8CharSize(Lead: AnsiChar; out IsValid: Boolean): Integer; overload;
function  ALUTF8CharSize(Lead: AnsiChar): Integer; overload;
function  ALUTF8CharCount(const S: AnsiString): Integer;
Function  ALUTF8ByteTrunc(const s:AnsiString; const Count: Integer): AnsiString;
Function  ALUTF8CharTrunc(const s:AnsiString; const Count: Integer): AnsiString;
Function  ALUTF8CharToUtf16(
            const S: AnsiString;
            const AIndex: integer;
            out AUTF8CharSize: integer;
            out AUTF16HighSurrogate: Char;
            out AUTF16lowSurrogate: Char): boolean;
Function  ALUTF8Encode(const S: RawByteString; const aCodePage: Word): AnsiString;
Function  ALStringDecode(const S: AnsiString; const aCodePage: Word): AnsiString;
Function  ALGetCodePageFromCharSetName(Acharset:AnsiString): Word;
{$IF defined(MSWINDOWS)}
Function  ALGetCodePageFromLCID(const aLCID:Integer): Word;
{$ENDIF}
function  ALExtractExpressionA(
            const S: AnsiString;
            const OpenChar, CloseChar: AnsiChar; // ex: '(' and ')'
            Const QuoteChars: Array of ansiChar; // ex: ['''', '"']
            Const EscapeQuoteChar: ansiChar; // ex: '\' or #0 to ignore
            var StartPos: integer;
            var EndPos: integer): boolean;
function  ALHTTPEncode(const AStr: AnsiString): AnsiString; overload;
function  ALHTTPEncode(const AStr: String): String; overload;
function  ALHTTPDecode(const AStr: AnsiString): AnsiString; overload;
function  ALHTTPDecode(const AStr: String): String; overload;
procedure ALExtractHeaderFields(
            Separators,
            WhiteSpace,
            Quotes: TSysCharSet;
            Content: PAnsiChar;
            Strings: TALStringsA;
            HttpDecode: Boolean;
            StripQuotes: Boolean = False);
procedure ALExtractHeaderFieldsWithQuoteEscaped(
            Separators,
            WhiteSpace,
            Quotes: TSysCharSet;
            Content: PAnsiChar;
            Strings: TALStringsA;
            HttpDecode: Boolean;
            StripQuotes: Boolean = False); overload;
{$WARN SYMBOL_DEPRECATED OFF}
procedure ALExtractHeaderFieldsWithQuoteEscaped(
            Separators,
            WhiteSpace,
            Quotes: TSysCharSet;
            Content: PChar;
            Strings: TALStringsW;
            HttpDecode: Boolean;
            StripQuotes: Boolean = False); overload;
{$WARN SYMBOL_DEPRECATED ON}

type

  TALTagParamsClassA = class of TALStringsA;

  TALBasePrecompiledTagA = Class(Tobject)
  private
    fTagString: ansiString;
  protected
    function GetTagParams: TALStringsA; virtual; abstract;
  public
    property TagString: ansiString read fTagString write fTagString;
    property TagParams: TALStringsA read GetTagParams;
  End;

  TALPrecompiledTagA = Class(TALBasePrecompiledTagA)
  private
    fTagParams: TALStringsA;
  protected
    function GetTagParams: TALStringsA; override;
  public
    constructor Create;
    destructor Destroy; override;
  End;

  TALHandleTagfunctA = function(
                         const TagString: AnsiString;
                         TagParams: TALStringsA;
                         Context: pointer;
                         Var Handled: Boolean): AnsiString;

  TALHandleTagExtendedfunctA = function(
                                 const TagString: AnsiString;
                                 TagParams: TALStringsA;
                                 Context: pointer;
                                 Var Handled: Boolean;
                                 Const SourceString: AnsiString;
                                 Var TagPosition, TagLength: integer): AnsiString;

  TALHandleTagPrecompileFunctA = function(
                                   const TagString: AnsiString;
                                   TagParams: TALStringsA;
                                   Context: pointer;
                                   Const SourceString: AnsiString;
                                   Var TagPosition, TagLength: integer): TALBasePrecompiledTagA;

function ALFastTagReplacePrecompileA(
           Const SourceString, TagStart, TagEnd: AnsiString;
           PrecompileProc: TALHandleTagPrecompileFunctA;
           StripParamQuotes: Boolean;
           Context: Pointer;
           TagsContainer: TObjectList;
           Const flags: TReplaceFlags=[]): AnsiString; // rfreplaceall is ignored here, only rfIgnoreCase is matter
function ALFastTagReplaceA(
           Const SourceString, TagStart, TagEnd: AnsiString;
           ReplaceProc: TALHandleTagFunctA;
           ReplaceExtendedProc: TALHandleTagExtendedfunctA;
           StripParamQuotes: Boolean;
           Flags: TReplaceFlags;
           Context: Pointer;
           TagParamsClass: TALTagParamsClassA;
           const TagReplaceProcResult: Boolean = False): AnsiString; overload;
function  ALFastTagReplaceA(
            const SourceString, TagStart, TagEnd: AnsiString;
            ReplaceProc: TALHandleTagFunctA;
            StripParamQuotes: Boolean;
            Context: Pointer;
            Const flags: TReplaceFlags=[rfreplaceall];
            const TagReplaceProcResult: Boolean = False): AnsiString; overload;
function  ALFastTagReplaceA(
            const SourceString, TagStart, TagEnd: AnsiString;
            ReplaceExtendedProc: TALHandleTagExtendedfunctA;
            StripParamQuotes: Boolean;
            Context: Pointer;
            Const flags: TReplaceFlags=[rfreplaceall];
            const TagReplaceProcResult: Boolean = False): AnsiString; overload;
function  ALFastTagReplaceA(
            const SourceString, TagStart, TagEnd: AnsiString;
            const ReplaceWith: AnsiString;
            const Flags: TReplaceFlags=[rfreplaceall]): AnsiString; overload;
function  ALExtractTagParamsA(
            Const SourceString, TagStart, TagEnd: AnsiString;
            StripParamQuotes: Boolean;
            TagParams: TALStringsA;
            IgnoreCase: Boolean): Boolean;
Procedure ALSplitTextAndTagA(
            Const SourceString, TagStart, TagEnd: AnsiString;
            SplitTextAndTagLst: TALStringsA;
            IgnoreCase: Boolean);

implementation

uses
  System.SysConst,
  System.RTLConsts,
  System.StrUtils,
  System.Masks,
  System.IOUtils,
  system.netencoding,
  System.Ansistrings,
  System.Character,
  System.Math;

{*************************************************************}
constructor TALStringStreamA.Create(const AString: AnsiString);
begin
  inherited Create;
  FDataString := AString;
end;

{******************************************************************}
function TALStringStreamA.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Length(FDataString) - FPosition;
  if Result > Count then Result := Count;

  // a little modification from the original TStringStream
  // because in original we will have a call to uniqueString on FDataString :(
  // https://forums.embarcadero.com/thread.jspa?threadID=119103
  // ALMove(PAnsiChar(@FDataString[FPosition + SizeOf(AnsiChar)])^, Buffer, Result * SizeOf(AnsiChar));
  ALMove(Pbyte(FDataString)[FPosition], Buffer, Result * SizeOf(AnsiChar));

  Inc(FPosition, Result);
end;

{*********************************************************************}
function TALStringStreamA.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;

  // a little modification from the original TStringStream
  // because in original it's crazy we can not update part inside the datastring !!
  // SetLength(FDataString, (FPosition + Result));
  if FPosition + Result > length(FDataString) then SetLength(FDataString, (FPosition + Result));

  ALMove(Buffer, PAnsiChar(@FDataString[FPosition + SizeOf(AnsiChar)])^, Result * SizeOf(AnsiChar));
  Inc(FPosition, Result);
end;

{*********************************************************************}
function TALStringStreamA.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := Length(FDataString) - Offset;
  end;
  if FPosition > Length(FDataString) then
    FPosition := Length(FDataString)
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

{***************************************************************}
function TALStringStreamA.ReadString(Count: Longint): AnsiString;
var
  Len: Integer;
begin
  Len := Length(FDataString) - FPosition;
  if Len > Count then Len := Count;
  SetString(Result, PAnsiChar(@FDataString[FPosition + SizeOf(AnsiChar)]), Len);
  Inc(FPosition, Len);
end;

{****************************************************************}
procedure TALStringStreamA.WriteString(const AString: AnsiString);
begin
  Write(PAnsiChar(AString)^, Length(AString));
end;

{***************************************************}
procedure TALStringStreamA.SetSize(NewSize: Longint);
begin
  SetLength(FDataString, NewSize);
  if FPosition > NewSize then FPosition := NewSize;
end;

{**********************}
{$IF defined(MSWINDOWS)}
class function TALFormatSettingsA.Create(Locale: LCID): TALFormatSettingsA;
var LFormatSettings: TformatSettings;
    I: integer;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  LFormatSettings:= TformatSettings.Create(Locale);
  {$WARN SYMBOL_PLATFORM ON}
  with result do begin
    CurrencyString := AnsiString(LFormatSettings.CurrencyString);
    CurrencyFormat := LFormatSettings.CurrencyFormat;
    CurrencyDecimals := LFormatSettings.CurrencyDecimals;
    DateSeparator := AnsiChar(LFormatSettings.DateSeparator);
    TimeSeparator := AnsiChar(LFormatSettings.TimeSeparator);
    ListSeparator := AnsiString(LFormatSettings.ListSeparator);
    ShortDateFormat := AnsiString(LFormatSettings.ShortDateFormat);
    LongDateFormat := AnsiString(LFormatSettings.LongDateFormat);
    TimeAMString := AnsiString(LFormatSettings.TimeAMString);
    TimePMString := AnsiString(LFormatSettings.TimePMString);
    ShortTimeFormat := AnsiString(LFormatSettings.ShortTimeFormat);
    LongTimeFormat := AnsiString(LFormatSettings.LongTimeFormat);
    for I := Low(ShortMonthNames) to High(ShortMonthNames) do
      ShortMonthNames[i] := AnsiString(LFormatSettings.ShortMonthNames[i]);
    for I := Low(LongMonthNames) to High(LongMonthNames) do
      LongMonthNames[i] := AnsiString(LFormatSettings.LongMonthNames[i]);
    for I := Low(ShortDayNames) to High(ShortDayNames) do
      ShortDayNames[i] := AnsiString(LFormatSettings.ShortDayNames[i]);
    for I := Low(LongDayNames) to High(LongDayNames) do
      LongDayNames[i] := AnsiString(LFormatSettings.LongDayNames[i]);
    setlength(EraInfo, length(LFormatSettings.EraInfo));
    for I := Low(LFormatSettings.EraInfo) to High(LFormatSettings.EraInfo) do begin
      EraInfo[i].EraName := ansiString(LFormatSettings.EraInfo[i].EraName);
      EraInfo[i].EraOffset := LFormatSettings.EraInfo[i].EraOffset;
      EraInfo[i].EraStart := LFormatSettings.EraInfo[i].EraStart;
      EraInfo[i].EraEnd := LFormatSettings.EraInfo[i].EraEnd;
    end;
    ThousandSeparator := AnsiString(LFormatSettings.ThousandSeparator);
    DecimalSeparator := AnsiChar(LFormatSettings.DecimalSeparator);
    TwoDigitYearCenturyWindow := LFormatSettings.TwoDigitYearCenturyWindow;
    NegCurrFormat := LFormatSettings.NegCurrFormat;
  end;
end;
{$ENDIF}

{*****************************************************************************************}
class function TALFormatSettingsA.Create(const LocaleName: AnsiString): TALFormatSettingsA;
var LFormatSettings: TformatSettings;
    I: integer;
begin
  LFormatSettings:= TformatSettings.Create(String(LocaleName));
  with result do begin
    CurrencyString := AnsiString(LFormatSettings.CurrencyString);
    CurrencyFormat := LFormatSettings.CurrencyFormat;
    CurrencyDecimals := LFormatSettings.CurrencyDecimals;
    DateSeparator := AnsiChar(LFormatSettings.DateSeparator);
    TimeSeparator := AnsiChar(LFormatSettings.TimeSeparator);
    ListSeparator := AnsiString(LFormatSettings.ListSeparator);
    ShortDateFormat := AnsiString(LFormatSettings.ShortDateFormat);
    LongDateFormat := AnsiString(LFormatSettings.LongDateFormat);
    TimeAMString := AnsiString(LFormatSettings.TimeAMString);
    TimePMString := AnsiString(LFormatSettings.TimePMString);
    ShortTimeFormat := AnsiString(LFormatSettings.ShortTimeFormat);
    LongTimeFormat := AnsiString(LFormatSettings.LongTimeFormat);
    for I := Low(ShortMonthNames) to High(ShortMonthNames) do
      ShortMonthNames[i] := AnsiString(LFormatSettings.ShortMonthNames[i]);
    for I := Low(LongMonthNames) to High(LongMonthNames) do
      LongMonthNames[i] := AnsiString(LFormatSettings.LongMonthNames[i]);
    for I := Low(ShortDayNames) to High(ShortDayNames) do
      ShortDayNames[i] := AnsiString(LFormatSettings.ShortDayNames[i]);
    for I := Low(LongDayNames) to High(LongDayNames) do
      LongDayNames[i] := AnsiString(LFormatSettings.LongDayNames[i]);
    setlength(EraInfo, length(LFormatSettings.EraInfo));
    for I := Low(LFormatSettings.EraInfo) to High(LFormatSettings.EraInfo) do begin
      EraInfo[i].EraName := ansiString(LFormatSettings.EraInfo[i].EraName);
      EraInfo[i].EraOffset := LFormatSettings.EraInfo[i].EraOffset;
      EraInfo[i].EraStart := LFormatSettings.EraInfo[i].EraStart;
      EraInfo[i].EraEnd := LFormatSettings.EraInfo[i].EraEnd;
    end;
    ThousandSeparator := AnsiString(LFormatSettings.ThousandSeparator);
    DecimalSeparator := AnsiChar(LFormatSettings.DecimalSeparator);
    TwoDigitYearCenturyWindow := LFormatSettings.TwoDigitYearCenturyWindow;
    NegCurrFormat := LFormatSettings.NegCurrFormat;
  end;
end;

{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.SysUtils.TFormatSettings.GetEraYearOffset is still the same and adjust the IFDEF'}
{$ENDIF}
function TALFormatSettingsA.GetEraYearOffset(const Name: ansistring): Integer;
var
  I: Integer;
begin
  Result := -MaxInt;
  for I := High(EraInfo) downto Low(EraInfo) do
  begin
    if EraInfo[I].EraName = '' then Break;
    if ALPosA(EraInfo[I].EraName, Name) > 0 then
    begin
      Result := EraInfo[I].EraOffset - 1;
      Exit;
    end;
  end;
end;

{***********************************************************}
class function TALFormatSettingsA.Create: TALFormatSettingsA;
begin
  Result := TALFormatSettingsA.Create('');
end;

{************************************************************************************}
function ALGetFormatSettingsID(const aFormatSettings: TALFormatSettingsA): AnsiString;
begin
  With aFormatSettings do begin
    Result := ALIntToStrA(CurrencyFormat) + '#' +
              ALIntToStrA(CurrencyDecimals) + '#' +
              DateSeparator + '#' +
              TimeSeparator + '#' +
              ListSeparator + '#' +
              ShortDateFormat + '#' +
              LongDateFormat + '#' +
              ShortTimeFormat + '#' +
              LongTimeFormat + '#' +
              ThousandSeparator + '#' +
              DecimalSeparator + '#' +
              ALIntToStrA(TwoDigitYearCenturyWindow) + '#' +
              ALIntToStrA(NegCurrFormat);
  end;
end;

{**********************}
{$IF defined(MSWINDOWS)}
procedure ALGetLocaleFormatSettings(Locale: LCID; var AFormatSettings: TALFormatSettingsA);
begin
  AFormatSettings := TALFormatSettingsA.Create(Locale);
end;
{$ENDIF}

{*******************************}
Function  ALNewGUIDBytes: TBytes;
Var LGUID: TGUID;
Begin
  if CreateGUID(LGUID) <> S_OK then RaiseLastOSError;
  SetLength(Result, 16);
  ALMove(LGUID.D1, Result[0], 4); // D1: Cardinal;
  ALMove(LGUID.D2, Result[4], 2); // D2: Word;
  ALMove(LGUID.D3, Result[6], 2); // D3: Word;
  ALMove(LGUID.D4[0], Result[8], 8); // D4: array[0..7] of Byte;
End;

{**********************************************************}
function  ALGUIDToByteString(const Guid: TGUID): Ansistring;
var LByteArray: TBytes;
begin
  LByteArray := Guid.ToByteArray;
  SetString(result, PAnsiChar(@LByteArray[0]), length(LByteArray));
end;

{****************************************}
function  ALNewGUIDByteString: Ansistring;
var LGuid: TGUID;
begin
  if CreateGUID(LGuid) <> S_OK then RaiseLastOSError;
  result := ALGUIDToByteString(LGuid);
end;

{************************************************************************************************************************************}
function  ALGUIDToStringA(const Guid: TGUID; const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): Ansistring;
begin
  if WithoutBracket then begin
    if WithoutHyphen then begin
      SetLength(Result, 32);
      System.Ansistrings.StrLFmt(
        PAnsiChar(Result), 32,'%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end
    else begin
      SetLength(Result, 36);
      System.Ansistrings.StrLFmt(
        PAnsiChar(Result), 36,'%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end;
  end
  else begin
    if WithoutHyphen then begin
      SetLength(Result, 34);
      System.Ansistrings.StrLFmt(
        PAnsiChar(Result), 34,'{%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x}',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end
    else begin
      SetLength(Result, 38);
      System.Ansistrings.StrLFmt(
        PAnsiChar(Result), 38,'{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end;
  end;
end;

{********************************************************************************************************************************}
function  ALGUIDToStringW(const Guid: TGUID; const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): string;
begin
  if WithoutBracket then begin
    if WithoutHyphen then begin
      SetLength(Result, 32);
      StrLFmt(
        PChar(Result), 32,'%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end
    else begin
      SetLength(Result, 36);
      StrLFmt(
        PChar(Result), 36,'%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end;
  end
  else begin
    if WithoutHyphen then begin
      SetLength(Result, 34);
      StrLFmt(
        PChar(Result), 34,'{%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x}',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end
    else begin
      SetLength(Result, 38);
      StrLFmt(
        PChar(Result), 38,'{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end;
  end;
end;

{******************************************************************************************************************}
Function  ALNewGUIDStringA(const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): AnsiString;
Var LGUID: TGUID;
Begin
  if CreateGUID(LGUID) <> S_OK then RaiseLastOSError;
  Result := ALGUIDToStringA(LGUID, WithoutBracket, WithoutHyphen);
End;

{**************************************************************************************************************}
Function  ALNewGUIDStringW(const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): String;
Var LGUID: TGUID;
Begin
  if CreateGUID(LGUID) <> S_OK then RaiseLastOSError;
  Result := ALGUIDToStringW(LGUID, WithoutBracket, WithoutHyphen);
End;

{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.Masks.pas is still the same and adjust the IFDEF'}
{$ENDIF}

{***}
const
  _MaxCards = 30;

{****************************************************************}
function TALMaskA.InitMaskStates(const Mask: ansistring): Integer;
var
  I: Integer;
  SkipTo: Boolean;
  Literal: ansiChar;
  LeadByte, TrailByte: ansiChar;
  P: PansiChar;
  Negate: Boolean;
  CharSet: TMaskSet;
  Cards: Integer;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure InvalidMask;
  begin
    raise EALMaskException.CreateResFmt(
            @SInvalidMask,
            [Mask, P - PansiChar(Mask) + 1]);
  end;

  {~~~~~~~~~~~~~~}
  procedure Reset;
  begin
    SkipTo := False;
    Negate := False;
    CharSet := [];
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure WriteScan(MaskState: TMaskStates);
  begin
    if I <= High(FMaskStates) then
    begin
      if SkipTo then
      begin
        Inc(Cards);
        if Cards > _MaxCards then InvalidMask;
      end;
      FMaskStates[I].SkipTo := SkipTo;
      FMaskStates[I].State := MaskState;
      case MaskState of
        TMaskStates.msLiteral: FMaskStates[I].Literal := UpCase(Literal);
        TMaskStates.msSet:
          begin
            FMaskStates[I].Negate := Negate;
            New(FMaskStates[I].CharSet);
            FMaskStates[I].CharSet^ := CharSet;
          end;
        TMaskStates.msMBCSLiteral:
          begin
            FMaskStates[I].LeadByte := LeadByte;
            FMaskStates[I].TrailByte := TrailByte;
          end;
      end;
    end;
    Inc(I);
    Reset;
  end;

  {~~~~~~~~~~~~~~~~}
  procedure ScanSet;
  var
    LastChar: ansiChar;
    C: ansiChar;
  begin
    Inc(P);
    if P^ = '!' then
    begin
      Negate := True;
      Inc(P);
    end;
    LastChar := #0;
    while not (P^ in [#0, ']']) do
    begin
      // MBCS characters not supported in msSet!
      //if IsLeadChar(P^) then
      //   Inc(P)
      //else
      case P^ of
        '-':
          if LastChar = #0 then InvalidMask
          else
          begin
            Inc(P);
            for C := LastChar to UpCase(P^) do
              CharSet := CharSet + [C];
          end;
      else
        LastChar := UpCase(P^);
        CharSet := CharSet + [LastChar];
      end;
      Inc(P);
    end;
    if (P^ <> ']') or (CharSet = []) then InvalidMask;
    WriteScan(TMaskStates.msSet);
  end;

begin
  P := PansiChar(Mask);
  I := 0;
  Cards := 0;
  Reset;
  while P^ <> #0 do
  begin
    case P^ of
      '*': SkipTo := True;
      '?': if not SkipTo then WriteScan(TMaskStates.msAny);
      '[':  ScanSet;
    else
      //if IsLeadChar(P^) then
      //begin
      //  LeadByte := P^;
      //  Inc(P);
      //  TrailByte := P^;
      //  WriteScan(msMBCSLiteral);
      //end
      //else
      begin
        Literal := P^;
        WriteScan(TMaskStates.msLiteral);
      end;
    end;
    Inc(P);
  end;
  Literal := #0;
  WriteScan(TMaskStates.msLiteral);
  Result := I;
end;

{***********************************************************************}
function TALMaskA.MatchesMaskStates(const Filename: ansistring): Boolean;
type
  TStackRec = record
    sP: PansiChar;
    sI: Integer;
  end;
var
  T: Integer;
  S: array of TStackRec;
  I: Integer;
  P: PansiChar;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure Push(P: PansiChar; I: Integer);
  begin
    S[T].sP := P;
    S[T].sI := I;
    Inc(T);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function Pop(var P: PansiChar; var I: Integer): Boolean;
  begin
    if T = 0 then
      Result := False
    else
    begin
      Dec(T);
      P := S[T].sP;
      I := S[T].sI;
      Result := True;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function Matches(P: PansiChar; Start: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Start to High(FMaskStates) do
    begin
      if FMaskStates[I].SkipTo then
      begin
        case FMaskStates[I].State of
          TMaskStates.msLiteral:
            while (P^ <> #0) and (UpCase(P^) <> FMaskStates[I].Literal) do Inc(P);
          TMaskStates.msSet:
            while (P^ <> #0) and not (FMaskStates[I].Negate xor (UpCase(P^) in FMaskStates[I].CharSet^)) do Inc(P);
          TMaskStates.msMBCSLiteral:
            while (P^ <> #0) do
            begin
              if (P^ <> FMaskStates[I].LeadByte) then Inc(P, 2)
              else
              begin
                Inc(P);
                if (P^ = FMaskStates[I].TrailByte) then Break;
                Inc(P);
              end;
            end;
        end;
        if P^ <> #0 then
          Push(@P[1], I);
      end;
      case FMaskStates[I].State of
        TMaskStates.msLiteral: if UpCase(P^) <> FMaskStates[I].Literal then Exit;
        TMaskStates.msSet: if not (FMaskStates[I].Negate xor (UpCase(P^) in FMaskStates[I].CharSet^)) then Exit;
        TMaskStates.msMBCSLiteral:
          begin
            if P^ <> FMaskStates[I].LeadByte then Exit;
            Inc(P);
            if P^ <> FMaskStates[I].TrailByte then Exit;
          end;
        TMaskStates.msAny:
          if P^ = #0 then
          begin
            Result := False;
            Exit;
          end;
      end;
      Inc(P);
    end;
    Result := True;
  end;

begin
  SetLength(S, _MaxCards);
  Result := True;
  T := 0;
  P := PansiChar(Filename);
  I := Low(FMaskStates);
  repeat
    if Matches(P, I) then Exit;
  until not Pop(P, I);
  Result := False;
end;

{********************************}
procedure TALMaskA.DoneMaskStates;
var
  I: Integer;
begin
  for I := Low(FMaskStates) to High(FMaskStates) do
    if FMaskStates[I].State = TMaskStates.msSet then Dispose(FMaskStates[I].CharSet);
end;

{*******************************************************}
constructor TALMaskA.Create(const MaskValue: ansistring);
var
  Size: Integer;
begin
  inherited Create;
  SetLength(FMaskStates, 1);
  Size := InitMaskStates(MaskValue);
  DoneMaskStates;

  SetLength(FMaskStates, Size);
  InitMaskStates(MaskValue);
end;

{**************************}
destructor TALMaskA.Destroy;
begin
  DoneMaskStates;
  SetLength(FMaskStates, 0);
  inherited;
end;

{*************************************************************}
function TALMaskA.Matches(const Filename: ansistring): Boolean;
begin
  Result := MatchesMaskStates(Filename);
end;

{*****************************************************************}
function ALMatchesMaskA(const Filename, Mask: ansistring): Boolean;
var
  CMask: TALMaskA;
begin
  CMask := TALMaskA.Create(Mask);
  try
    Result := CMask.Matches(Filename);
  finally
    CMask.Free;
  end;
end;

{*************************************************************}
function ALMatchesMaskW(const Filename, Mask: String): Boolean;
begin
  result := System.Masks.MatchesMask(Filename, Mask);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.SysUtils.ConvertErrorFmt is still the same and adjust the IFDEF'}
{$ENDIF}
procedure ALConvertErrorFmt(ResString: PResStringRec; const Args: array of const);
begin
  raise EConvertError.CreateResFmt(ResString, Args) at ReturnAddress;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.SysUtils.ConvertError is still the same and adjust the IFDEF'}
{$ENDIF}
procedure ALConvertError(ResString: PResStringRec);
begin
  raise EConvertError.CreateRes(ResString) at ReturnAddress;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.SysUtils.FormatError is still the same and adjust the IFDEF'}
{$ENDIF}
procedure ALFormatError(ErrorCode: Integer; Format: PChar; FmtLen: Cardinal);
const
  FormatErrorStrs: array[0..1] of PResStringRec = (
    @SALInvalidFormat, @SALArgumentMissing);
var
  Buffer: array[0..31] of Char;
begin
  if FmtLen > 31 then FmtLen := 31;

  if StrByteType(Format, FmtLen-1) = mbLeadByte then Dec(FmtLen);
  StrMove(Buffer, Format, FmtLen);
  Buffer[FmtLen] := #0;
  ALConvertErrorFmt(FormatErrorStrs[ErrorCode], [PChar(@Buffer)]);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.SysUtils.AnsiFormatError is still the same and adjust the IFDEF'}
{$ENDIF}
procedure ALAnsiFormatError(ErrorCode: Integer; Format: PAnsiChar; FmtLen: Cardinal);
var
  FormatText: string;
begin
  FormatText := UTF8ToUnicodeString(Format);
  ALFormatError(ErrorCode, PChar(FormatText), FmtLen);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.SysUtils.InternalFloatToText is still the same and adjust the IFDEF'}
{$ENDIF}
{$R-} {Range-Checking}
function ALInternalFloatToText(
           ABuffer: PByte;
           //ABufferIsUnicode: Boolean;
           const AValue;
           AValueType: TFloatValue;
           AFormat: TFloatFormat;
           APrecision, ADigits: Integer;
           const AFormatSettings: TALFormatSettingsA): Integer;
const
  CMinExtPrecision = 2;
{$IFDEF EXTENDEDHAS10BYTES}
  CMaxExtPrecision = 18;
{$ELSE !EXTENDEDHAS10BYTES}
  CMaxExtPrecision = 17;
{$ENDIF !EXTENDEDHAS10BYTES}

  CCurrPrecision = 19;
  CGenExpDigits = 9999;

  CExpChar = 'E';          // DO NOT LOCALIZE
  CMinusSign: AnsiChar = '-';  // DO NOT LOCALIZE
  CPlusSign: AnsiChar = '+';   // DO NOT LOCALIZE
  CZero: AnsiChar = '0';       // DO NOT LOCALIZE
  CSpecial: array[0 .. 1] of ansistring = ('INF', 'NAN'); // DO NOT LOCALIZE
  CCurrencyFormats: array[0 .. 3] of ansistring = ('$*@@@', '*$@@@', '$ *@@', '* $@@'); // DO NOT LOCALIZE
  CNegCurrencyFormats: array[0 .. 15] of ansistring =
  (
    '($*)@', '-$*@@', '$-*@@', '$*-@@', '(*$)@', '-*$@@', // DO NOT LOCALIZE
    '*-$@@', '*$-@@', '-* $@', '-$ *@', '* $-@', // DO NOT LOCALIZE
    '$ *-@', '$ -*@', '*- $@', '($ *)', '(* $)' // DO NOT LOCALIZE
  );

var
  FloatRec: TFloatRec;

  LDigits: Integer;
  LExponent: Cardinal;
  LUseENotation: Boolean;

  LCurrentFormat: ansistring;
  //LCurrChar: Char;
  ICurrChar: Integer;
  LFloatRecDigit: Integer;
  LNextThousand: Integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure AppendChar(const AChar: AnsiChar);
  begin
    //if ABufferIsUnicode then
    //begin
    //  PChar(ABuffer)^ := AChar;
    //  Inc(ABuffer, SizeOf(Char));
    //end else
    //begin
    //  PByte(ABuffer)^ := Byte(AChar);
    //  Inc(ABuffer, SizeOf(Byte));
    //end;
    PAnsiChar(ABuffer)^ := AChar;
    Inc(ABuffer, SizeOf(AnsiChar));

    Inc(Result);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure AppendString(const AStr: AnsiString);
  var
    {I,} L: Integer;
  begin
    L := Length(AStr);

    if L > 0 then
    begin
      //if ABufferIsUnicode then
      //begin
      //  { Unicode -- move directly }
      //  MoveChars(AStr[Low(string)], ABuffer^, L);
      //  Inc(ABuffer, L * SizeOf(Char));
      //end else
      //begin
      //  { ANSI -- loop }
      //  for I := Low(string) to High(AStr) do
      //  begin
      //    PByte(ABuffer)^ := Byte(AStr[I]);
      //    Inc(ABuffer, SizeOf(Byte));
      //  end;
      //end;
      ALMove(pointer(AStr)^, ABuffer^, L);
      Inc(ABuffer, L * SizeOf(AnsiChar));

      Inc(Result, L);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~}
  function GetDigit: Byte;
  begin
    Result := FloatRec.Digits[LFloatRecDigit];

    if Result = Ord(#0) then
      Result := Ord('0')
    else
      Inc(LFloatRecDigit);
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure FormatNumber;
  var
    K: Integer;
  begin
    if ADigits > CMaxExtPrecision then
      LDigits := CMaxExtPrecision
    else
      LDigits := ADigits;

    K := FloatRec.Exponent;
    if K > 0 then
    begin
      { Find the position of the next thousand separator }
      LNextThousand := 0;
      if AFormat <> ffFixed then
        LNextThousand := ((K - 1) mod 3) + 1;

      repeat
        { Append the next digit }
        AppendChar(ansiChar(GetDigit));

        { Update loop counters }
        Dec(K);
        Dec(LNextThousand);

        { Try to append the thousands separator and reset the counter }
        if (LNextThousand = 0) and (K > 0) then
        begin
          LNextThousand := 3;

          if AFormatSettings.ThousandSeparator <> #0 then
            AppendString(AFormatSettings.ThousandSeparator);
        end;
      until (K = 0);

    end else
      AppendChar(CZero);

    { If there are ADigits left to fill }
    if LDigits <> 0 then
    begin
      { Put in the decimal separator if it was specified }
      if AFormatSettings.DecimalSeparator <> #0 then
        AppendChar(AFormatSettings.DecimalSeparator);

      { If there is  negative exponent }
      if K < 0 then
      begin
        { Fill with zeroes until the exponent or ADigits are exhausted}
        repeat
          AppendChar(CZero);

          Inc(K);
          Dec(LDigits);
        until (K = 0) or (LDigits = 0);
      end;

      if LDigits > 0 then
      begin
        { Exponent was filled, there are still ADigits left to fill }
        repeat
          AppendChar(ansiChar(GetDigit));
          Dec(LDigits);
        until (LDigits <= 0);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~}
  procedure FormatExponent;
  var
    LMinCnt, LExponent: Integer;
    LExpString: ansistring;
    LDigitCnt: Integer;
  begin
    { Adjust digit count }
    if ADigits > 4 then
      LMinCnt := 0
    else
      LMinCnt := ADigits;

    { Get exponent }
    LExponent := FloatRec.Exponent - 1;

    { Place the E character into position }
    AppendChar(CExpChar);

    if Byte(FloatRec.Digits[0]) <> Ord(#0) then
    begin
      if LExponent < 0 then
      begin
        LExponent := -LExponent;
        AppendChar(CMinusSign);
      end
      else
      begin
        if AFormat <> ffGeneral then
          AppendChar(CPlusSign);
      end;
    end else
    begin
      if AFormat <> ffGeneral then
        AppendChar(CPlusSign);
      LExponent := 0;
    end;

    LExpString := ALIntToStrA(LExponent);
    LDigitCnt := Length(LExpString);

    while LDigitCnt < LMinCnt do
    begin
      AppendChar(CZero);
      Inc(LDigitCnt);
    end;

    AppendString(LExpString);
  end;

begin
  LFloatRecDigit := 0;
  Result := 0;

  if AValueType = fvExtended then
  begin
    { Check min and max precisions for an Extended }
    if APrecision < CMinExtPrecision then
      APrecision := CMinExtPrecision
    else if APrecision > CMaxExtPrecision then
      APrecision := CMaxExtPrecision;
  end else
    APrecision := CCurrPrecision;

  { Check the number of ADigits to use }
  if AFormat in [ffGeneral, ffExponent] then
    LDigits := CGenExpDigits
  else
    LDigits := ADigits;

  { Decode the float }
  FloatToDecimal(FloatRec, AValue, AValueType, APrecision, LDigits);
{$IFDEF EXTENDEDHAS10BYTES}
  LExponent := UInt16(FloatRec.Exponent) - $7FFF;
{$ELSE !EXTENDEDHAS10BYTES}
  LExponent := UInt16(FloatRec.Exponent) - $7FF;
{$ENDIF !EXTENDEDHAS10BYTES}

  { Check for INF or NAN}
  if LExponent < 2 then
  begin
    { Append the sign to output buffer }
    if FloatRec.Negative then
      AppendChar(CMinusSign);

    AppendString(CSpecial[LExponent]);
    Exit;
  end;

  if (not (AFormat in [ffGeneral .. ffCurrency])) or
    ((FloatRec.Exponent > APrecision) and (AFormat <> ffExponent)) then
    AFormat := ffGeneral;

  case AFormat of
    ffGeneral:
    begin
      { Append the sign to output buffer }
      if FloatRec.Negative then
        AppendChar(CMinusSign);

      LUseENotation := False;

      { Obtain digit count and whether to use the E notation }
      LDigits := FloatRec.Exponent;
      if (LDigits > APrecision) or (LDigits < -3) then
      begin
        LDigits := 1;
        LUseENotation := True;
      end;

      if LDigits > 0 then
      begin
        { Append the ADigits that precede decimal separator }
        while LDigits > 0 do
        begin
          AppendChar(ansiChar(GetDigit));
          Dec(LDigits);
        end;

        { Append the decimal separator and the following digit }
        if FloatRec.Digits[LFloatRecDigit] <> Ord(#0) then
        begin
          AppendChar(AFormatSettings.DecimalSeparator);

          { Append the ADigits that come after the decimal separator }
          while FloatRec.Digits[LFloatRecDigit] <> Ord(#0) do
            AppendChar(ansiChar(GetDigit));
        end;

        if LUseENotation then
          FormatExponent();
      end else
      begin
        AppendChar(CZero);

        if FloatRec.Digits[0] <> Ord(#0) then
        begin
          AppendChar(AFormatSettings.DecimalSeparator);
          LDigits := -LDigits;

          { Append zeroes to fulfill the exponent }
          while LDigits > 0 do
          begin
            AppendChar(CZero);
            Dec(LDigits);
          end;

          { Attach all the other ADigits now }
          while FloatRec.Digits[LFloatRecDigit] <> Ord(#0) do
            AppendChar(ansiChar(GetDigit));
        end;
      end;
    end;

    ffExponent:
    begin
      { Append the sign to output buffer }
      if FloatRec.Negative then
        AppendChar(CMinusSign);

      { Append the first digit and the decimal separator }
      AppendChar(ansiChar(GetDigit));
      AppendChar(AFormatSettings.DecimalSeparator);

      { Append ADigits based on the APrecision requirements }
      Dec(APrecision);
      repeat
        AppendChar(ansiChar(GetDigit));
        Dec(APrecision);
      until (APrecision = 0);

      FormatExponent();
    end;

    ffNumber, ffFixed:
    begin
      { Append the sign to output buffer }
      if FloatRec.Negative then
        AppendChar(CMinusSign);

      FormatNumber();
    end;

    ffCurrency:
    begin
      { Select the appropriate currency AFormat}
      if FloatRec.Negative then
      begin
        {  negative AFormat is used, check for bounds and select }
        if AFormatSettings.NegCurrFormat > High(CNegCurrencyFormats) then
          LCurrentFormat := CNegCurrencyFormats[High(CNegCurrencyFormats)]
        else
          LCurrentFormat := CNegCurrencyFormats[AFormatSettings.NegCurrFormat];
      end else
      begin
        {  positive AFormat is used, check for bounds and select }
        if AFormatSettings.CurrencyFormat > High(CCurrencyFormats) then
          LCurrentFormat := CCurrencyFormats[High(CCurrencyFormats)]
        else
          LCurrentFormat := CCurrencyFormats[AFormatSettings.CurrencyFormat];
      end;

      { Iterate over each charater in the AFormat string }
//      for LCurrChar in LCurrentFormat do
//        case LCurrChar of
      for ICurrChar := Low(AnsiString) to High(LCurrentFormat) do
        case LCurrentFormat[ICurrChar] of
          '@': break;
          '$':
            if AFormatSettings.CurrencyString <> '' {EmptyStr} then
              AppendString(AFormatSettings.CurrencyString);
          '*': FormatNumber();
          else
             //AppendChar(LCurrChar);
             AppendChar(LCurrentFormat[ICurrChar]);
        end;
    end;
  end;
end;
{$IF defined(ALRangeCheckingON)}
  {$R+} {Range-Checking}
{$ENDIF}

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.AnsiStrings.FormatBuf is still the same and adjust the IFDEF'}
{$ENDIF}
function ALFormatBuf(
           var Buffer; BufLen: Cardinal; const Format;
           FmtLen: Cardinal; const Args: array of const;
           const AFormatSettings: TALFormatSettingsA): Cardinal;
var
  BufPtr: PAnsiChar;
  FormatPtr: PAnsiChar;
  FormatStartPtr: PAnsiChar;
  FormatEndPtr: PAnsiChar;
  ArgsIndex: Integer;
  ArgsLength: Integer;
  BufMaxLen: Cardinal;
  Overwrite: Boolean;
  FormatChar: AnsiChar;
  S: AnsiString;
  StrBuf: array[0..64] of AnsiChar; // if currencystring contain more than 64 chars then it's raise an error :(
  LeftJustification: Boolean;
  Width: Integer;
  Precision: Integer;
  Len: Integer;
  FirstNumber: Integer;
  CurrentArg: TVarRec;
  FloatVal: TFloatValue;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ApplyWidth(NumChar, Negitive: Integer): Boolean;
  var
    I: Integer;
    Max: Integer;
  begin
    Result := False;
    if (Precision > NumChar) and (FormatChar <> 'S') then
      Max := Precision
    else
      Max := NumChar;
    if (Width <> 0) and (Width > Max + Negitive) then
    begin
      for I := Max + 1  + Negitive to Width do
      begin
        if BufMaxLen = 0 then
        begin
          Result := True;
          Break;
        end;
        BufPtr^ := ' ';
        Inc(BufPtr);
        Dec(BufMaxLen, Sizeof(AnsiChar));
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function AddBuf(const AItem: PAnsiChar; ItemLen: Integer = -1; StringLen: Integer = -1): Boolean;
  var
    NumChar: Integer;
    Len: Integer;
    I: Integer;
    Item: PAnsiChar;
    Negitive: Integer;
    BytesToCopy: Cardinal;
  begin
    Item := AItem;
    if Assigned(AItem) then
      if StringLen = -1 then
        NumChar := System.AnsiStrings.StrLen(Item)
      else
        NumChar := StringLen
    else
      NumChar := 0;
    if (ItemLen > -1) and (NumChar > ItemLen) then
      NumChar := ItemLen;
    Len := NumChar * Sizeof(AnsiChar);
    if (Assigned(AItem)) and (Item^ = '-') and (FormatChar <> 'S') then
    begin
      Dec(Len, Sizeof(AnsiChar));
      Dec(NumChar);
      Negitive := 1;
    end
    else
      Negitive := 0;
    if not LeftJustification then
    begin
      Result := ApplyWidth(NumChar, Negitive);
      if Result then
        Exit;
    end;
    if Negitive = 1 then
    begin
      if BufMaxLen = 0 then
      begin
        Result := True;
        Exit;
      end;
      Inc(Item);
      BufPtr^ := '-';
      Inc(BufPtr);
      Dec(BufMaxLen, Sizeof(AnsiChar));
    end;
    if (Precision <> -1) and (Precision > NumChar) and (FormatChar <> 'S') then
      for I := NumChar + 1 to Precision do
      begin
        if BufMaxLen = 0 then
        begin
          Result := True;
          Exit;
        end;
        BufPtr^ := '0';
        Inc(BufPtr);
        Dec(BufMaxLen, Sizeof(AnsiChar));
      end;
    if Assigned(AItem) then
    begin
      Result := BufMaxLen < Cardinal(Len);
      if Result then
        BytesToCopy := BufMaxLen
      else
        BytesToCopy := Len;
      ALMove(Item^, BufPtr^, BytesToCopy);
      BufPtr := PAnsiChar(PByte(BufPtr) + BytesToCopy);
      Dec(BufMaxLen, BytesToCopy);
    end
    else
      Result := False;
    if LeftJustification then
      Result := ApplyWidth(NumChar, Negitive);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function VariantToAnsiString(V: TVarData): AnsiString;
  begin
    Result := '';
    if Assigned(System.VarToLStrProc) then
      System.VarToLStrProc(Result, V)
    else
      System.Error(reVarInvalidOp);
  end;

begin
  if (not Assigned(@Buffer)) or  (not Assigned(@Format)) then
  begin
    Result := 0;
    Exit;
  end;
  ArgsIndex := -1;
  ArgsLength := Length(Args);
  BufPtr := PAnsiChar(@Buffer);
  FormatPtr := PAnsiChar(@Format);
  if BufLen < $7FFFFFFF then
    BufMaxLen := BufLen * Sizeof(AnsiChar)
  else
    BufMaxLen := BufLen;
  FormatEndPtr := FormatPtr + FmtLen;
  while (FormatPtr < FormatEndPtr) do
    if FormatPtr^ = '%' then
    begin
      Inc(FormatPtr);
      if (FormatPtr >= FormatEndPtr) then
        Break;
      if FormatPtr^ = '%' then
      begin
        if BufMaxLen = 0 then
        begin
          Result := BufPtr - PAnsiChar(@Buffer);
          Exit;
        end;
        BufPtr^ := FormatPtr^;
        Inc(FormatPtr);
        Inc(BufPtr);
        Dec(BufMaxLen, Sizeof(AnsiChar));
        Continue;
      end;
      Width := 0;
      // Gather Index
      Inc(ArgsIndex);
      if Char(FormatPtr^).IsNumber then
      begin
        FormatStartPtr := FormatPtr;
        while (FormatPtr < FormatEndPtr) and (Char(FormatPtr^).IsNumber)  do
          Inc(FormatPtr);
        if FormatStartPtr <> FormatPtr then
        begin
          System.AnsiStrings.StrLCopy(StrBuf, FormatStartPtr, Integer(FormatPtr - FormatStartPtr));
          if not ALTryStrToInt(AnsiString(StrBuf), FirstNumber) then
            ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
          if FormatPtr^ = ':' then
          begin
            Inc(FormatPtr);
            ArgsIndex := FirstNumber;
          end
          else
            Width := FirstNumber;
        end;
      end
      else if FormatPtr^ = ':' then
      begin
        ArgsIndex := 0;
        Inc(FormatPtr);
      end;
      // Gather Justification
      if FormatPtr^ = '-' then
      begin
        LeftJustification := True;
        Inc(FormatPtr);
      end
      else
        LeftJustification := False;
      // Gather Width
      FormatStartPtr := FormatPtr;
      if FormatPtr^ = '*' then
      begin
        Width := -2;
        Inc(FormatPtr);
      end
      else if Char(FormatPtr^).IsNumber then
      begin
        while (FormatPtr < FormatEndPtr) and (Char(FormatPtr^).IsNumber) do
          Inc(FormatPtr);
        if FormatStartPtr <> FormatPtr then
        begin
          System.AnsiStrings.StrLCopy(StrBuf, FormatStartPtr, Integer(FormatPtr - FormatStartPtr));
          if not ALTryStrToInt(AnsiString(StrBuf), Width) then
            ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
        end
      end;
      // Gather Precision
      if FormatPtr^ = '.' then
      begin
        Inc(FormatPtr);
        if (FormatPtr >= FormatEndPtr) then
          Break;
        if FormatPtr^ = '*' then
        begin
          Precision := -2;
          Inc(FormatPtr);
        end
        else
        begin
          FormatStartPtr := FormatPtr;
          while (FormatPtr < FormatEndPtr) and (Char(FormatPtr^).IsNumber) do
            Inc(FormatPtr);
          System.AnsiStrings.StrLCopy(StrBuf, FormatStartPtr, Integer(FormatPtr - FormatStartPtr));
          if not ALTryStrToInt(AnsiString(StrBuf), Precision) then
            ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
        end;
      end
      else
        Precision := -1;

      // Gather Conversion Character
      if not Char(FormatPtr^).IsLetter then
        Break;
      case FormatPtr^ of
        'a'..'z':
          FormatChar := AnsiChar(Byte(FormatPtr^) xor $20);
      else
        FormatChar := FormatPtr^;
      end;
      Inc(FormatPtr);

      // Handle Args
      if Width = -2 then // If * width was found
      begin
        if ArgsIndex >= ArgsLength then
          ALAnsiFormatError(1, PAnsiChar(@Format), FmtLen);
        if Args[ArgsIndex].VType = vtInteger then
        begin
          if ArgsIndex >= ArgsLength then
            ALAnsiFormatError(1, PAnsiChar(@Format), FmtLen);
          Width := Args[ArgsIndex].VInteger;
          if Width < 0 then
          begin
            LeftJustification := not LeftJustification;
            Width := -Width;
          end;
          Inc(ArgsIndex);
        end
        else
          ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
      end;
      if Precision = -2 then
      begin
        if ArgsIndex >= ArgsLength then
          ALAnsiFormatError(1, PAnsiChar(@Format), FmtLen);
        if Args[ArgsIndex].VType = vtInteger then
        begin
          if ArgsIndex >= ArgsLength then
            ALAnsiFormatError(1, PAnsiChar(@Format), FmtLen);
          Precision := Args[ArgsIndex].VInteger;
          Inc(ArgsIndex);
        end
        else
          ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
      end;

      if ArgsIndex >= ArgsLength then
        ALAnsiFormatError(1, PAnsiChar(@Format), FmtLen);
      CurrentArg := Args[ArgsIndex];

      Overwrite := False;
      case CurrentArg.VType of
        vtBoolean,
        vtObject,
        vtClass,
        vtWideChar,
        vtPWideChar,
        vtWideString,
        vtInterface: ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
        vtInteger:
          begin
            //https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2001
            if Precision = -1 then
              Precision := 0;
            case FormatChar of
              'D': S := AnsiString(ALIntToStrA(CurrentArg.VInteger));
              'U': S := AnsiString(ALUIntToStrA(Cardinal(CurrentArg.VInteger)));
              'X': S := AnsiString(ALIntToHexA(CurrentArg.VInteger, 0));
            else
              ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
            end;
            Overwrite := AddBuf(PAnsiChar(S));
          end;
        vtChar:
          if FormatChar = 'S' then
          begin
            S := AnsiChar(CurrentArg.VChar);
            Overwrite := AddBuf(PAnsiChar(S), Precision);
          end
          else
            ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
        vtExtended, vtCurrency:
          begin
            if CurrentArg.VType = vtExtended then
              FloatVal := fvExtended
            else
              FloatVal := fvCurrency;
            Len := 0;
            if (FormatChar = 'G') or (FormatChar = 'E') then
            begin
              if Cardinal(Precision) > 18 then
                Precision := 15;
            end
            else if Cardinal(Precision) > 18 then
            begin
              Precision := 2;
              if FormatChar = 'M' then
                Precision := AFormatSettings.CurrencyDecimals;
            end;
            case FormatChar of
              'G': Len := ALInternalFloatToText(PByte(@StrBuf), CurrentArg.VExtended^, FloatVal, ffGeneral, Precision, 3, AFormatSettings);
              'E': Len := ALInternalFloatToText(PByte(@StrBuf), CurrentArg.VExtended^, FloatVal, ffExponent, Precision, 3, AFormatSettings);
              'F': Len := ALInternalFloatToText(PByte(@StrBuf), CurrentArg.VExtended^, FloatVal, ffFixed, 18, Precision, AFormatSettings);
              'N': Len := ALInternalFloatToText(PByte(@StrBuf), CurrentArg.VExtended^, FloatVal, ffNumber, 18, Precision, AFormatSettings);
              'M': Len := ALInternalFloatToText(PByte(@StrBuf), CurrentArg.VExtended^, FloatVal, ffCurrency, 18, Precision, AFormatSettings);
            else
              ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
            end;
            StrBuf[Len] := #0;
            Precision := 0;
            Overwrite := AddBuf(StrBuf);
          end;
        vtString:
          if FormatChar = 'S' then
            Overwrite := AddBuf(PAnsiChar(AnsiString(ShortString(PShortString(CurrentArg.VAnsiString)^))), Precision)
          else
            ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
        vtUnicodeString:
          if FormatChar = 'S' then
            Overwrite := AddBuf(PAnsiChar(AnsiString(CurrentArg.VPWideChar)), Precision)
          else
            ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
        vtVariant:
          if FormatChar = 'S' then
            Overwrite := AddBuf(PAnsiChar(VariantToAnsiString(TVarData(CurrentArg.VVariant^))), Precision)
          else
            ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
        vtPointer:
          if FormatChar = 'P' then
          begin
            S := AnsiString(ALIntToHexA(IntPtr(CurrentArg.VPointer), SizeOf(Pointer)*2));
            Overwrite := AddBuf(PAnsiChar(S));
          end
          else
            ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
        vtPChar:
          if FormatChar = 'S' then
            Overwrite := AddBuf(CurrentArg.VWideString, Precision)
          else
            ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
        vtAnsiString:
          if FormatChar = 'S' then
            Overwrite := AddBuf(CurrentArg.VAnsiString, Precision, Length(AnsiString(CurrentArg.VAnsiString)))
          else
            ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
        vtInt64:
          begin
            //https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2001
            if Precision = -1 then
              Precision := 0;
            case FormatChar of
              'D': S := AnsiString(ALIntToStrA(CurrentArg.VInt64^));
              'U': S := AnsiString(ALUIntToStrA(UInt64(CurrentArg.VInt64^)));
              'X': S := AnsiString(ALIntToHexA(CurrentArg.VInt64^, 0));
            else
              ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
            end;
            Overwrite := AddBuf(PAnsiChar(S));
          end;
      end;
      if Overwrite then
      begin
        Result := BufPtr - PAnsiChar(@Buffer);
        Exit;
      end;
    end
    else
    begin
      if BufMaxLen = 0 then
      begin
        Result := BufPtr - PAnsiChar(@Buffer);
        Exit;
      end;
      BufPtr^ := FormatPtr^;
      Inc(FormatPtr);
      Inc(BufPtr);
      Dec(BufMaxLen, Sizeof(AnsiChar));
    end;
  Result := BufPtr - PAnsiChar(@Buffer);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.AnsiStrings.FmtStr is still the same and adjust the IFDEF'}
{$ENDIF}
procedure ALFmtStr(
            var Result: AnsiString; const Format: AnsiString;
            const Args: array of const; const AFormatSettings: TALFormatSettingsA);
var
  Len, BufLen: Integer;
  Buffer: array[0..4095] of AnsiChar;
begin
  BufLen := SizeOf(Buffer);
  if Length(Format) < (sizeof(Buffer) - (sizeof(Buffer) div 4)) then
    Len := ALFormatBuf(
             Buffer, sizeof(Buffer) - 1, Pointer(Format)^, Length(Format),
             Args, AFormatSettings)
  else
  begin
    BufLen := Length(Format);
    Len := BufLen;
  end;
  if Len >= BufLen - 1 then
  begin
    while Len >= BufLen - 1 do
    begin
      Inc(BufLen, BufLen);
      Result := '';          // prevent copying of existing data, for speed
      SetLength(Result, BufLen);
      Len := ALFormatBuf(
               Pointer(Result)^, BufLen - 1, Pointer(Format)^,
               Length(Format), Args, AFormatSettings);
    end;
    SetLength(Result, Len);
  end
  else
    SetString(Result, Buffer, Len);
end;

{***********************************************************************************}
function ALFormatA(const Format: AnsiString; const Args: array of const): AnsiString;
begin
  Result := ALFormatA(Format, Args, ALDefaultFormatSettingsA);
end;

{************************************************************************************************}
procedure ALFormatA(const Format: AnsiString; const Args: array of const; var Result: ansiString);
begin
  Result := ALFormatA(Format, Args, ALDefaultFormatSettingsA);
end;

{******************************************************************************************************************************}
function ALFormatA(const Format: AnsiString; const Args: array of const; const AFormatSettings: TALFormatSettingsA): AnsiString;
begin
  ALFmtStr(Result, Format, Args, AFormatSettings);
end;

{*******************************************************************************************************************************************}
procedure ALFormatA(const Format: AnsiString; const Args: array of const; const AFormatSettings: TALFormatSettingsA; var Result: ansiString);
begin
  ALFmtStr(Result, Format, Args, AFormatSettings);
end;

{***************************************************************************}
function ALFormatW(const Format: String; const Args: array of const): String;
begin
  Result := ALFormatW(Format, Args, ALDefaultFormatSettingsW);
end;

{****************************************************************************************}
procedure ALFormatW(const Format: String; const Args: array of const; var Result: String);
begin
  Result := ALFormatW(Format, Args, ALDefaultFormatSettingsW);
end;

{**********************************************************************************************************************}
function ALFormatW(const Format: String; const Args: array of const; const AFormatSettings: TALFormatSettingsW): String;
begin
  Result := System.SysUtils.Format(Format, Args, AFormatSettings);
end;

{***********************************************************************************************************************************}
procedure ALFormatW(const Format: String; const Args: array of const; const AFormatSettings: TALFormatSettingsW; var Result: String);
begin
  Result := System.SysUtils.Format(Format, Args, AFormatSettings);
end;

{************************************************************************}
function ALTryStrToBool(const S: AnsiString; out Value: Boolean): Boolean;
var
  LResult: Integer;
begin
  Result := ALTryStrToInt(S, LResult);
  if Result then
    Value := LResult <> 0
  else
  begin
    Result := ALSametextA(S,'True');
    if Result then
      Value := True
    else
    begin
      Result := ALSametextA(S,'False');
      if Result then
        Value := False;
    end;
  end;
end;

{********************************************************************}
function ALTryStrToBool(const S: String; out Value: Boolean): Boolean;
var
  LResult: Integer;
begin
  Result := ALTryStrToInt(S, LResult);
  if Result then
    Value := LResult <> 0
  else
  begin
    Result := ALSametextW(S,'True');
    if Result then
      Value := True
    else
    begin
      Result := ALSametextW(S,'False');
      if Result then
        Value := False;
    end;
  end;
end;

{*********************************************}
Function AlStrToBool(Value:AnsiString):Boolean;
Begin
  Result := False;
  ALTryStrtoBool(Value,Result);
end;

{*****************************************}
Function AlStrToBool(Value:String):Boolean;
Begin
  Result := False;
  ALTryStrtoBool(Value,Result);
end;

{************************************************************************************************************}
function  ALBoolToStrA(B: Boolean; const trueStr: ansistring='1'; const falseStr: ansistring='0'): Ansistring;
begin
  if B then result := trueStr
  else result := falseStr;
end;

{*******************************************************************************************************************}
procedure ALBoolToStrA(var s: ansiString; B: Boolean; const trueStr: ansistring='1'; const falseStr: ansistring='0');
begin
  if B then s := trueStr
  else s := falseStr;
end;

{************************************************************************************************}
function  ALBoolToStrW(B: Boolean; const trueStr: String='1'; const falseStr: String='0'): String;
begin
  if B then result := trueStr
  else result := falseStr;
end;

{*******************************************************************************************************}
procedure ALBoolToStrW(var s: String; B: Boolean; const trueStr: String='1'; const falseStr: String='0');
begin
  if B then s := trueStr
  else s := falseStr;
end;

{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.TCFString is still the same and adjust the IFDEF'}
{$ENDIF}

{$IFDEF MACOS}
{**}
type
  TCFString = record
    Value: CFStringRef;

    constructor Create(const Val: string);
    function AsString(Release: Boolean = False): string;
    function AsAnsiString(Release: Boolean = False): ansistring;
    function AsChar(Release: Boolean = False): Char;
    class operator Implicit(const Ref: CFStringRef): TCFString;
    class operator Implicit(const Ref: CFMutableStringRef): TCFString;
  end;

{**********************************************}
constructor TCFString.Create(const Val: string);
begin
  Value := CFStringCreateWithCharacters(
             kCFAllocatorDefault,
             PChar(Val),
             Length(Val));
end;

{************************************************************}
function TCFString.AsString(Release: Boolean = False): string;
var
  Range: CFRange;
  Tmp: TCharArray;
begin
  if Value = nil then Exit('');
  try
    Range := CFRangeMake(0, CFStringGetLength(Value));
    if Range.Length > 0 then
    begin
      SetLength(Tmp, Range.Length);
      CFStringGetCharacters(Value, Range, MarshaledString(Tmp));
      Result := string.Create(Tmp);
    end
    else
      Result := EmptyStr;
  finally
    if Release then
      CFRelease(Value);
  end;
end;

{********************************************************************}
function TCFString.AsAnsiString(Release: Boolean = False): ansistring;
begin
  result := AnsiString(AsString(Release));
end;

{********************************************************}
function TCFString.AsChar(Release: Boolean = False): Char;
begin
  if Value = nil then Exit(#0);
  try
    if CFStringGetLength(Value) > 0 then
      Result := CFStringGetCharacterAtIndex(Value, 0)
  else
      Result := #0;
  finally
    if Release then
      CFRelease(Value);
  end;
end;

{*******************************************************************}
class operator TCFString.Implicit(const Ref: CFStringRef): TCFString;
begin
  Result.Value := Ref;
end;

{**************************************************************************}
class operator TCFString.Implicit(const Ref: CFMutableStringRef): TCFString;
begin
  Result.Value := CFStringRef(Ref);
end;
{$ENDIF MACOS}

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.DateTimeToString is still the same and adjust the IFDEF'}
{$ENDIF}
procedure ALDateTimeToString(
            var Result: AnsiString; const Format: AnsiString;
            DateTime: TDateTime; const AFormatSettings: TALFormatSettingsA);
var
  BufPos, AppendLevel: Integer;
  Buffer: array[0..255] of AnsiChar;
  DynBuffer: array of AnsiChar;
  Sb: TArray<ansiChar>;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure AppendChars(P: PAnsiChar; Count: Integer);
  var
    N, I: Integer;
    begin
    N := SizeOf(Buffer) div SizeOf(AnsiChar);
    N := N - BufPos;
    if Count > N then
      begin
      I := Length(DynBuffer);
      SetLength(DynBuffer, I + BufPos + Count);
      if BufPos > 0 then
      begin
        ALMove(Buffer[0], DynBuffer[I], BufPos * SizeOf(AnsiChar));
        Inc(I, BufPos);
      end;
      ALMove(P[0], DynBuffer[I], Count * SizeOf(AnsiChar));
      BufPos := 0;
    end
    else if Count > 0 then
    begin
      ALMove(P[0], Buffer[BufPos], Count * SizeOf(AnsiChar));
      Inc(BufPos, Count);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure AppendString(const S: AnsiString);
  begin
    AppendChars(Pointer(S), Length(S));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure AppendNumber(Number, Digits: Integer);
  const
    Format: array[0..3] of AnsiChar = '%.*d';
  var
    NumBuf: array[0..15] of AnsiChar;
  begin
    AppendChars(
      NumBuf,
      ALFormatBuf(
        NumBuf,
        Length(NumBuf),
        Format,
        Length(Format),
        [Digits, Number],
        AFormatSettings));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure AppendFormat(Format: PAnsiChar);
  var
    Starter, Token, LastToken: AnsiChar;
    DateDecoded, TimeDecoded, Use12HourClock,
    BetweenQuotes: Boolean;
    P: PAnsiChar;
    Count: Integer;
    Year, Month, Day, Hour, Min, Sec, MSec, H: Word;

    {~~~~~~~~~~~~~~~~~}
    procedure GetCount;
    var
      P: PAnsiChar;
    begin
      P := Format;
      while Format^ = Starter do Inc(Format);
      Count := Format - P + 1;
    end;

    {~~~~~~~~~~~~~~~~}
    procedure GetDate;
    begin
      if not DateDecoded then
      begin
        DecodeDate(DateTime, Year, Month, Day);
        DateDecoded := True;
      end;
    end;

    {~~~~~~~~~~~~~~~~}
    procedure GetTime;
    begin
      if not TimeDecoded then
      begin
        DecodeTime(DateTime, Hour, Min, Sec, MSec);
        TimeDecoded := True;
      end;
    end;

{$IFDEF MSWINDOWS}
    function ConvertEraString(const Count: Integer) : AnsiString;
    var
      FormatStr: AnsiString;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of AnsiChar;
      P: PAnsiChar;
    begin
      Result := '';
      SystemTime.wYear  := Year;
      SystemTime.wMonth := Month;
      SystemTime.wDay   := Day;

      FormatStr := 'gg';
      if GetDateFormatA(
           GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
           PAnsiChar(FormatStr), Buffer, SizeOf(Buffer)) <> 0 then
      begin
        Result := Buffer;
        if Count = 1 then
        begin
          case SysLocale.PriLangID of
            LANG_JAPANESE:
              Result := ALCopyStr(Result, 1, System.Ansistrings.CharToElementLen(Result, 1));
            LANG_CHINESE:
              if (SysLocale.SubLangID = SUBLANG_CHINESE_TRADITIONAL)
                and (System.Ansistrings.ElementToCharLen(Result, Length(Result)) = 4) then
              begin
                P := Buffer + System.Ansistrings.CharToElementIndex(Result, 3) - 1;
                SetString(Result, P, System.Ansistrings.CharToElementLen(P, 2));
              end;
          end;
        end;
      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    function ConvertYearString(const Count: Integer): AnsiString;
    var
      FormatStr: AnsiString;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of AnsiChar;
    begin
      Result := '';
      SystemTime.wYear  := Year;
      SystemTime.wMonth := Month;
      SystemTime.wDay   := Day;

      if Count <= 2 then
        FormatStr := 'yy' // avoid Win95 bug.
      else
        FormatStr := 'yyyy';

      if GetDateFormatA(
           GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
           PAnsiChar(FormatStr), Buffer, SizeOf(Buffer)) <> 0 then
      begin
        Result := Buffer;
        if (Count = 1) and (Result[Low(AnsiString)] = '0') then
          Result := ALCopyStr(Result, 2, Length(Result)-1);
      end;
    end;
{$ENDIF MSWINDOWS}

{$IFDEF POSIX}
    {$IFNDEF MACOS}
    function FindEra(Date: Integer): Integer;
    var
      I : Integer;
    begin
      Result := 0;
      for I := High(AFormatSettings.EraInfo) downto Low(AFormatSettings.EraInfo) do
      begin
        if (AFormatSettings.EraInfo[I].EraStart <= Date) then
          Exit(I);
      end;
    end;
    {$ENDIF !MACOS}

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    function ConvertEraString(const Count: Integer) : AnsiString;
    var
      {$IFDEF MACOS}
      Formatter: CFDateFormatterRef;
      LDate: CFGregorianDate;
      LYear, LMonth, LDay: Word;
      FormatString: TCFString;
      DefaultTZ: CFTimeZoneRef;
      Locale: CFLocaleRef;
      {$ELSE !MACOS}
      I : Integer;
      {$ENDIF MACOS}
    begin
      Result := '';
      {$IFDEF MACOS}
      Locale := nil;
      DefaultTZ := nil;
      Formatter := nil;
      FormatString.Value := nil;

      try
        Locale := CFLocaleCopyCurrent;
        DefaultTZ := CFTimeZoneCopyDefault;
        Formatter := CFDateFormatterCreate(
                       kCFAllocatorDefault, Locale,
                       kCFDateFormatterFullStyle, kCFDateFormatterNoStyle);
        FormatString := TCFString.Create('GG');
        CFDateFormatterSetFormat(Formatter, FormatString.Value);
        DecodeDate(DateTime, LYear, LMonth, LDay);
        LDate.year := LYear; LDate.month := ShortInt(LMonth); LDate.day := ShortInt(LDay);
        LDate.hour := 0; LDate.minute := 0; LDate.second := 0;
        Result := TCFString(
                    CFDateFormatterCreateStringWithAbsoluteTime(
                      kCFAllocatorDefault,
                      Formatter,
                      CFGregorianDateGetAbsoluteTime(LDate, DefaultTZ))).AsAnsiString(true);
      finally
        if FormatString.Value <> nil then
          CFRelease(FormatString.Value);
        if Formatter <> nil then
          CFRelease(Formatter);
        if DefaultTZ <> nil then
          CFRelease(DefaultTZ);
        if Locale <> nil then
          CFRelease(Locale);
      end;
      {$ELSE !MACOS}
      I := FindEra(Trunc(DateTime));
      if I > 0 then
        Result := AFormatSettings.EraInfo[I].EraName;
      {$ENDIF MACOS}
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    function ConvertYearString(const Count: Integer) : AnsiString;
    var
      S : AnsiString;
      function GetEraOffset: Integer;
      {$IFDEF MACOS}
      var
        StartEra, TargetDate, LengthEra: CFAbsoluteTime;
        LDate: CFGregorianDate;
        LYear, LMonth, LDay: Word;
        Calendar, CurrentCalendar: CFCalendarRef;
        TimeZone: CFTimeZoneRef;
      {$ENDIF MACOS}
      begin
        {$IFDEF MACOS}
        Result := 0;
        TimeZone := nil;
        CurrentCalendar := nil;
        Calendar := nil;
        try
          DecodeDate(DateTime, LYear, LMonth, LDay);
          LDate.year := LYear; LDate.month := ShortInt(LMonth); LDate.day := ShortInt(LDay);
          LDate.hour := 0; LDate.minute := 0; LDate.second := 0;
          TimeZone := CFTimeZoneCopyDefault;
          TargetDate := CFGregorianDateGetAbsoluteTime(LDate, TimeZone);
          CurrentCalendar := CFCalendarCopyCurrent;
          Calendar := CFCalendarCreateWithIdentifier(
                        kCFAllocatorDefault,
                        CFCalendarGetIdentifier(CurrentCalendar));
          if CFCalendarGetTimeRangeOfUnit(
               Calendar, kCFCalendarUnitEra,
               TargetDate, @StartEra, @LengthEra) then
          begin
            LDate := CFAbsoluteTimeGetGregorianDate(StartEra, TimeZone);
            Result := LDate.Year - 1;
          end;
        finally
          if CurrentCalendar <> nil then
            CFRelease(CurrentCalendar);
          if Calendar <> nil then
            CFRelease(Calendar);
          if TimeZone <> nil then
            CFRelease(TimeZone);
        end;
        {$ELSE !MACOS}
        Result := FindEra(Trunc(DateTime));
        if Result > 0 then
          Result := AFormatSettings.EraInfo[Result].EraOffset -1;
        {$ENDIF MACOS}
      end;
    begin
      S := ALIntToStrA(Year - GetEraOffset);
      while Length(S) < Count do
        S := '0' + S;
      if Length(S) > Count then
        S := ALCopyStr(S, Length(S) - (Count - 1), Count);
      Result := S;
    end;
{$ENDIF POSIX}

  begin
    if (Format <> nil) and (AppendLevel < 2) then
    begin
      Inc(AppendLevel);
      LastToken := ' ';
      DateDecoded := False;
      TimeDecoded := False;
      Use12HourClock := False;
      while Format^ <> #0 do
      begin
        Starter := Format^;
        //if IsLeadChar(Starter) then
        //begin
        //  AppendChars(Format, System.Ansistrings.StrCharLength(Format) div SizeOf(AnsiChar));
        //  Format := System.Ansistrings.StrNextChar(Format);
        //  LastToken := ' ';
        //  Continue;
        //end;
        Format := System.Ansistrings.StrNextChar(Format);
        Token := Starter;
        if Token in ['a'..'z'] then Dec(Token, 32);
        if Token in ['A'..'Z'] then
        begin
          if (Token = 'M') and (LastToken = 'H') then Token := 'N';
          LastToken := Token;
        end;
        case Token of
          'Y':
            begin
              GetCount;
              GetDate;
              if Count <= 2 then
                AppendNumber(Year mod 100, 2) else
                AppendNumber(Year, 4);
            end;
          'G':
            begin
              GetCount;
              GetDate;
              AppendString(ConvertEraString(Count));
            end;
          'E':
            begin
              GetCount;
              GetDate;
              AppendString(ConvertYearString(Count));
            end;
          'M':
            begin
              GetCount;
              GetDate;
              case Count of
                1, 2: AppendNumber(Month, Count);
                3: AppendString(AFormatSettings.ShortMonthNames[Month]);
              else
                AppendString(AFormatSettings.LongMonthNames[Month]);
              end;
            end;
          'D':
            begin
              GetCount;
              case Count of
                1, 2:
                  begin
                    GetDate;
                    AppendNumber(Day, Count);
                  end;
                3: AppendString(AFormatSettings.ShortDayNames[DayOfWeek(DateTime)]);
                4: AppendString(AFormatSettings.LongDayNames[DayOfWeek(DateTime)]);
                5: AppendFormat(Pointer(AFormatSettings.ShortDateFormat));
              else
                AppendFormat(Pointer(AFormatSettings.LongDateFormat));
              end;
            end;
          'H':
            begin
              GetCount;
              GetTime;
              BetweenQuotes := False;
              P := Format;
              while P^ <> #0 do
              begin
                //if IsLeadChar(P^) then
                //begin
                //  P := System.Ansistrings.StrNextChar(P);
                //  Continue;
                //end;
                case P^ of
                  'A', 'a':
                    if not BetweenQuotes then
                    begin
                      if ( (System.Ansistrings.StrLIComp(P, 'AM/PM', 5) = 0)
                        or (System.Ansistrings.StrLIComp(P, 'A/P',   3) = 0)
                        or (System.Ansistrings.StrLIComp(P, 'AMPM',  4) = 0) ) then
                        Use12HourClock := True;
                      Break;
                    end;
                  'H', 'h':
                    Break;
                  '''', '"': BetweenQuotes := not BetweenQuotes;
                end;
                Inc(P);
              end;
              H := Hour;
              if Use12HourClock then
                if H = 0 then H := 12 else if H > 12 then Dec(H, 12);
              if Count > 2 then Count := 2;
              AppendNumber(H, Count);
            end;
          'N':
            begin
              GetCount;
              GetTime;
              if Count > 2 then Count := 2;
              AppendNumber(Min, Count);
            end;
          'S':
            begin
              GetCount;
              GetTime;
              if Count > 2 then Count := 2;
              AppendNumber(Sec, Count);
            end;
          'T':
            begin
              GetCount;
              if Count = 1 then
                AppendFormat(Pointer(AFormatSettings.ShortTimeFormat)) else
                AppendFormat(Pointer(AFormatSettings.LongTimeFormat));
            end;
          'Z':
            begin
              GetCount;
              GetTime;
              if Count > 3 then Count := 3;
              AppendNumber(MSec, Count);
            end;
          'A':
            begin
              GetTime;
              P := Format - 1;
              if System.Ansistrings.StrLIComp(P, 'AM/PM', 5) = 0 then
              begin
                if Hour >= 12 then Inc(P, 3);
                AppendChars(P, 2);
                Inc(Format, 4);
                Use12HourClock := TRUE;
              end else
              if System.Ansistrings.StrLIComp(P, 'A/P', 3) = 0 then
              begin
                if Hour >= 12 then Inc(P, 2);
                AppendChars(P, 1);
                Inc(Format, 2);
                Use12HourClock := TRUE;
              end else
              if System.Ansistrings.StrLIComp(P, 'AMPM', 4) = 0 then
              begin
                if Hour < 12 then
                  AppendString(AFormatSettings.TimeAMString) else
                  AppendString(AFormatSettings.TimePMString);
                Inc(Format, 3);
                Use12HourClock := TRUE;
              end else
              if System.Ansistrings.StrLIComp(P, 'AAAA', 4) = 0 then
              begin
                GetDate;
                AppendString(AFormatSettings.LongDayNames[DayOfWeek(DateTime)]);
                Inc(Format, 3);
              end else
              if System.Ansistrings.StrLIComp(P, 'AAA', 3) = 0 then
              begin
                GetDate;
                AppendString(AFormatSettings.ShortDayNames[DayOfWeek(DateTime)]);
                Inc(Format, 2);
              end else
              AppendChars(@Starter, 1);
            end;
          'C':
            begin
              GetCount;
              AppendFormat(Pointer(AFormatSettings.ShortDateFormat));
              GetTime;
              if (Hour <> 0) or (Min <> 0) or (Sec <> 0) or (MSec <> 0) then
              begin
                AppendChars(' ', 1);
                AppendFormat(Pointer(AFormatSettings.LongTimeFormat));
              end;
            end;
          '/':
            if AFormatSettings.DateSeparator <> #0 then
              AppendChars(@AFormatSettings.DateSeparator, 1);
          ':':
            if AFormatSettings.TimeSeparator <> #0 then
              AppendChars(@AFormatSettings.TimeSeparator, 1);
          '''', '"':
            begin
              P := Format;
              while (Format^ <> #0) and (Format^ <> Starter) do
              begin
                //if IsLeadChar(Format^) then
                //  Format := System.Ansistrings.StrNextChar(Format)
                //else
                  Inc(Format);
              end;
              AppendChars(P, Format - P);
              if Format^ <> #0 then Inc(Format);
            end;
        else
          AppendChars(@Starter, 1);
        end;
      end;
      Dec(AppendLevel);
    end;
  end;

begin
  BufPos := 0;
  AppendLevel := 0;
  if Format <> '' then AppendFormat(Pointer(Format)) else AppendFormat('C');
  if Length(DynBuffer) > 0 then
  begin
    SetLength(Sb, Length(DynBuffer) + BufPos);
    ALMove(DynBuffer[0], Sb[0], Length(DynBuffer) * SizeOf(AnsiChar));
    if BufPos > 0 then
      ALMove(Buffer[0], Sb[Length(DynBuffer)], BufPos * SizeOf(AnsiChar));
    //Result := String.Create(Sb);
    SetLength(Result, Length(Sb));
    alMove(Sb[0], PansiChar(Result)^, Length(Sb));
  end
  else begin
    //Result := AnsiString.Create(Buffer, 0, BufPos);
    SetLength(Result, BufPos);
    alMove(Buffer[0], PansiChar(Result)^, BufPos);
  end;
end;

{********************}
function ALDateToStrA(
           const DateTime: TDateTime;
           const AFormatSettings: TALFormatSettingsA): AnsiString;
begin
  ALDateTimeToString(
    Result, AFormatSettings.ShortDateFormat, DateTime,
    AFormatSettings);
end;

{********************}
function ALDateToStrW(
           const DateTime: TDateTime;
           const AFormatSettings: TALFormatSettingsW): string;
begin
  result := system.sysutils.DateToStr(DateTime, AFormatSettings);
end;

{********************}
function ALTimeToStrA(
           const DateTime: TDateTime;
           const AFormatSettings: TALFormatSettingsA): AnsiString;
begin
  ALDateTimeToString(
    Result, AFormatSettings.LongTimeFormat, DateTime,
    AFormatSettings);
end;

{********************}
function ALTimeToStrW(
           const DateTime: TDateTime;
           const AFormatSettings: TALFormatSettingsW): string;
begin
  result := system.sysutils.TimeToStr(DateTime, AFormatSettings);
end;

{************************}
function ALDateTimeToStrA(
           const DateTime: TDateTime;
           const AFormatSettings: TALFormatSettingsA): AnsiString;
begin
  ALDateTimeToString(Result, '', DateTime, AFormatSettings);
end;

{******************************************************************************************************************}
procedure ALDateTimeToStrA(const DateTime: TDateTime; var s: ansiString; const AFormatSettings: TALFormatSettingsA);
begin
  ALDateTimeToString(s, '', DateTime, AFormatSettings);
end;

{************************}
function ALDateTimeToStrW(
           const DateTime: TDateTime;
           const AFormatSettings: TALFormatSettingsW): String;
begin
  Result := system.sysutils.DateTimeToStr(DateTime, AFormatSettings);
end;

{**************************************************************************************************************}
procedure ALDateTimeToStrW(const DateTime: TDateTime; var s: String; const AFormatSettings: TALFormatSettingsW);
begin
  s := system.sysutils.DateTimeToStr(DateTime, AFormatSettings);
end;

{*************************}
function ALFormatDateTimeA(
           const Format: AnsiString;
           DateTime: TDateTime;
           const AFormatSettings: TALFormatSettingsA): AnsiString;
begin
  ALDateTimeToString(Result, Format, DateTime, AFormatSettings);
end;

{*************************}
function ALFormatDateTimeW(
           const Format: string;
           DateTime: TDateTime;
           const AFormatSettings: TALFormatSettingsW): string;
begin
  result := system.sysutils.FormatDateTime(Format, DateTime, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.TDatePart/TDateItem/TDateSeq are still the same and adjust the IFDEF'}
{$ENDIF}
type
  TALDatePart = (dpNone, dpChar, dpQuote,
    dpDSep, dpYear, dpMonth, dpDay, dpYearCurEra, dpEraName,
    dpTSep, dpMSSep, dpHour, dpMin, dpSec, dpMSec, dpAmPm, dpAP, dpAmPmS,
    dpTF, dpCur);
  TALDateItem = record
    FPart: TALDatePart;
    FLen: Byte;
    FChar: AnsiChar;
  end;
  TALDateSeq = array [1 .. 64] of TALDateItem;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.ParseDateTimeFormat is still the same and adjust the IFDEF'}
{$ENDIF}
function ALParseDateTimeFormat(const Format: AnsiString; WithTime: Boolean): TALDateSeq;
var
  I: Integer;
  PrevChar, Ch: AnsiChar;
  CountChar: Integer;
  P: PAnsiChar;
  Part: TALDatePart;
  InQuote, InDoubleQuote, WasHour: Boolean;
begin
  I := Low(TALDateSeq);
  InQuote := False;
  InDoubleQuote := False;
  WasHour := False;
  CountChar := 0;
  PrevChar := #0;
  P := PAnsiChar(Format);
  while True do
  begin
    Ch := P^;
    if PrevChar <> Ch then
    begin
      case PrevChar of
      'Y', 'y': Part := TALDatePart.dpYear;
      'M', 'm': if WithTime and WasHour then Part := TALDatePart.dpMin else Part := TALDatePart.dpMonth;
      'D', 'd': Part := TALDatePart.dpDay;
      '/':      Part := TALDatePart.dpDSep;
      'G', 'g': Part := TALDatePart.dpEraName;
      'E', 'e': Part := TALDatePart.dpYearCurEra;
      #0:       Part := TALDatePart.dpNone;
      ' ':      if WithTime then Part := TALDatePart.dpChar else Part := TALDatePart.dpNone;
      '''':
        if InDoubleQuote then
          Part := TALDatePart.dpChar
        else if CountChar = 2 then
        begin
          Part := TALDatePart.dpChar;
          CountChar := 1;
        end
        else
        begin
          Part := TALDatePart.dpQuote;
          InQuote := not InQuote;
        end;
      '"':
        if InQuote then
          Part := TALDatePart.dpChar
        else if CountChar = 2 then
        begin
          Part := TALDatePart.dpChar;
          CountChar := 1;
        end
        else
        begin
          Part := TALDatePart.dpQuote;
          InDoubleQuote := not InDoubleQuote;
        end;
      ':':      if WithTime then Part := TALDatePart.dpTSep else Part := TALDatePart.dpChar;
      '.':      if WithTime then Part := TALDatePart.dpMSSep else Part := TALDatePart.dpChar;
      'H', 'h': if WithTime then Part := TALDatePart.dpHour else Part := TALDatePart.dpChar;
      'N', 'n': if WithTime then Part := TALDatePart.dpMin else Part := TALDatePart.dpChar;
      'S', 's': if WithTime then Part := TALDatePart.dpSec else Part := TALDatePart.dpChar;
      'Z', 'z': if WithTime then Part := TALDatePart.dpMSec else Part := TALDatePart.dpChar;
      'A', 'a':
        if WithTime and not (InQuote or InDoubleQuote) then
        begin
          if System.ansistrings.StrLIComp(P - 1, PAnsiChar('AM/PM'), 5) = 0 then
          begin
            Part := TALDatePart.dpAmPm;
            Inc(P, 3);
          end
          else if System.ansistrings.StrLIComp(P - 1, PAnsiChar('A/P'), 3) = 0 then
          begin
            Part := TALDatePart.dpAP;
            Inc(P, 1);
          end
          else if System.ansistrings.StrLIComp(P - 1, PAnsiChar('AMPM'), 4) = 0 then
          begin
            Part := TALDatePart.dpAmPmS;
            Inc(P, 2);
          end
          else
            Part := TALDatePart.dpChar;
        end
        else
          Part := TALDatePart.dpChar;
      'T', 't': if WithTime then Part := TALDatePart.dpTF else Part := TALDatePart.dpChar;
      'C', 'c': if WithTime then Part := TALDatePart.dpCur else Part := TALDatePart.dpChar;
      else
        Part := TALDatePart.dpChar;
      end;
      if (Part <> TALDatePart.dpQuote) and (InQuote or InDoubleQuote) then
        Part := TALDatePart.dpChar;
      if not (Part in [TALDatePart.dpNone, TALDatePart.dpQuote]) then
      begin
        if I = High(TALDateSeq) + 1 then
        begin
          I := Low(TALDateSeq);
          Break;
        end;
        if (CountChar = 1) and (Part in [TALDatePart.dpYear, TALDatePart.dpYearCurEra, TALDatePart.dpMonth, TALDatePart.dpDay]) then
          CountChar := 2
        else if WithTime then
          if (Part = TALDatePart.dpTF) and (CountChar > 2) then
            Part := TALDatePart.dpChar
          else if Part = TALDatePart.dpHour then
            WasHour := True
          else if (Part = TALDatePart.dpMSec) and (CountChar < 3) then
            CountChar := 3;
        Result[I].FPart := Part;
        Result[I].FLen := CountChar;
        Result[I].FChar := PrevChar;
        Inc(I);
      end;
      if Part <> TALDatePart.dpQuote then
        CountChar := 1;
      if Part in [TALDatePart.dpAmPm, TALDatePart.dpAP, TALDatePart.dpAmPmS] then
        PrevChar := #0
      else
        PrevChar := Ch;
    end
    else
      Inc(CountChar);
    if P^ = #0 then
      Break;
    Inc(P);
  end;
  Result[I].FPart := TALDatePart.dpNone;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.ScanBlanks is still the same and adjust the IFDEF'}
{$ENDIF}
function ALScanBlanks(const S: AnsiString; var Pos: Integer): Boolean;
begin
  Result := False;
  while (Pos <= High(S)) and (S[Pos] = ' ') do
  begin
    Inc(Pos);
    Result := True;
  end;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.ScanNumber is still the same and adjust the IFDEF'}
{$ENDIF}
function ALScanNumber(const S: AnsiString; var Pos: Integer; var Number: Word; MaxChars: Integer): Integer;
var
  I, E: Integer;
  N: Word;
begin
  Result := 0;
  ALScanBlanks(S, Pos);
  I := Pos;
  E := High(S);
  if (MaxChars >= 0) and (E - I + 1 > MaxChars) then
    E := I + MaxChars - 1;
  N := 0;
  while (I <= E) and (S[I] in ['0'..'9']) and (N < 1000) do
  begin
    N := N * 10 + (Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if I > Pos then
  begin
    Result := I - Pos;
    Pos := I;
    Number := N;
  end;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.ScanFractional is still the same and adjust the IFDEF'}
{$ENDIF}
function ALScanFractional(const S: AnsiString; var Pos: Integer; var Number: Word; BaseDigits, MaxChars: Integer): Integer;
var
  I, E: Integer;
  N: Word;
begin
  Result := 0;
  ALScanBlanks(S, Pos);
  I := Pos;
  E := High(S);
  if (MaxChars >= 0) and (E - I + 1 > MaxChars) then
    E := I + MaxChars - 1;
  N := 0;
  while (I <= E) and (S[I] in ['0'..'9']) do
  begin
    if I - Pos < BaseDigits then
      N := N * 10 + (Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if I > Pos then
  begin
    Result := I - Pos;
    while Result < BaseDigits do
    begin
      N := N * 10;
      Dec(BaseDigits);
    end;
    Pos := I;
    Number := N;
  end;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.ScanString is still the same and adjust the IFDEF'}
{$ENDIF}
function ALScanString(const S: AnsiString; var Pos: Integer; const Symbol: AnsiString{; AUseAnsi: Boolean}): Boolean;
var
  L: Integer;
begin
  Result := False;
  if Symbol <> '' then
  begin
    ALScanBlanks(S, Pos);
    L := Length(Symbol);
    if (Pos + L - Low(AnsiString) <= Length(S)) and
       ({AUseAnsi and (System.Ansistrings.AnsiStrLIComp(Pointer(Symbol), PAnsiChar(Pointer(S)) + Pos - Low(AnsiString), L) = 0) or
        not AUseAnsi and} (System.Ansistrings.StrLIComp(Pointer(Symbol), PAnsiChar(Pointer(S)) + Pos - Low(AnsiString), L) = 0)) then
    begin
      Inc(Pos, L);
      Result := True;
    end;
  end;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.ScanChar is still the same and adjust the IFDEF'}
{$ENDIF}
function ALScanChar(const S: AnsiString; var Pos: Integer; Ch: AnsiChar): Boolean;
var
  C: AnsiChar;
begin
  if (Ch = ' ') and ALScanBlanks(S, Pos) then
    Exit(True);
  Result := False;
  if Pos <= High(S) then
  begin
    C := S[Pos];
    if C = Ch then
      Result := True;
    //https://stackoverflow.com/questions/73698212/in-delphi-alexandria-rtl-is-scanchar-badly-written
    //else if (Ch >= 'a') and (Ch <= 'z') and (C >= 'a') and (C <= 'z') then
    //  Result := Char(Word(C) xor $0020) = Char(Word(Ch) xor $0020)
    //else if Ch.IsLetter and C.IsLetter then
    //  Result := ToUpper(C) = ToUpper(Ch);
    if Result then
      Inc(Pos);
  end;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.ScanToNumber is still the same and adjust the IFDEF'}
{$ENDIF}
procedure ALScanToNumber(const S: AnsiString; var Pos: Integer);
begin
  while (Pos <= High(S)) and not (S[Pos] in ['0'..'9']) do
  begin
    //if IsLeadChar(S[Pos]) then
    //  Pos := NextCharIndex(S, Pos)
    //else
      Inc(Pos);
  end;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.ScanName is still the same and adjust the IFDEF'}
{$ENDIF}
function ALScanName(const S: AnsiString; var Pos: Integer; var Name: AnsiString; AnAbbr: Boolean): Boolean;
var
  Start: Integer;
  CharSize: Integer;
  HighSurrogate: Char;
  lowSurrogate: Char;
begin
  Start := Pos;
  if StringCodePage(S)=CP_UTF8 then begin
    while (Pos <= High(S)) and
          (ALUTF8CharToUtf16(
             S, //const S: AnsiString;
             Pos, // const AIndex: integer;
             CharSize, // out AUTF8CharSize: integer;
             HighSurrogate, // out AUTF16HighSurrogate: Char;
             lowSurrogate)) and // out AUTF16lowSurrogate: Char): boolean;
          (HighSurrogate.IsLetter or AnAbbr and (HighSurrogate = '.')) do
    begin
      //if IsLeadChar(S[Pos]) then
      //  Pos := NextCharIndex(S, Pos)
      //else
        Inc(Pos, CharSize);
    end;
  end
  else begin
    while (Pos <= High(S)) and (char(S[Pos]).IsLetter or AnAbbr and (S[Pos] = '.')) do
    begin
      //if IsLeadChar(S[Pos]) then
      //  Pos := NextCharIndex(S, Pos)
      //else
        Inc(Pos);
    end;
  end;
  Name := ALCopyStr(S, Start {- 1 + 1}, Pos - Start);
  Result := Name <> ''; // not Name.IsEmpty;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.ScanDate is still the same and adjust the IFDEF'}
{$ENDIF}
function ALScanDate(
           const S: AnsiString; var Pos: Integer; var Date: TDateTime;
           const AFormatSettings: TALFormatSettingsA): Boolean; overload;
type
  TNamesArray = array[1..12] of AnsiString;
  PNamesArray =^TNamesArray;
  TSpecifiedParts = set of (spDay, spDayOfWeek, spMonth, spYear, spEra, spShortYear);
var
  DateSeq: TALDateSeq;
  I, J: Integer;
  Y, M, D, DW: Word;
  CenturyBase: Integer;
  Name: AnsiString;
  PNames: PNamesArray;
  EraYearOffset: Integer;
  Was: TSpecifiedParts;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function EraToYear(Year: Integer): Integer;
  begin
{$IFDEF MSWINDOWS}
    if SysLocale.PriLangID = LANG_KOREAN then
    begin
      if Year <= 99 then
        Inc(Year, (CurrentYear + Abs(EraYearOffset + 1)) div 100 * 100);
      if EraYearOffset > 0 then
        EraYearOffset := -EraYearOffset;
    end;
{$ENDIF MSWINDOWS}
    Result := Year + EraYearOffset;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function CanDiscard(): Boolean;
  var
    J: Integer;
  begin
    Result := True;
    for J := I to High(DateSeq) do
      case DateSeq[J].FPart of
      TALDatePart.dpNone:
        Break;
      TALDatePart.dpChar:
        ;
      TALDatePart.dpMonth:
        if not (spMonth in Was) then
          Exit(False);
      TALDatePart.dpDay:
        if not (spDay in Was) then
          Exit(False);
      TALDatePart.dpYear,
      TALDatePart.dpYearCurEra:
        if [spYear, spShortYear] * Was = []  then
          Exit(False);
      else
        Exit(False);
      end;
  end;

begin
  DateSeq := ALParseDateTimeFormat(AFormatSettings.ShortDateFormat, False);
  EraYearOffset := 0;
  Y := 0;
  M := 0;
  D := 0;
  DW := 0;
  Was := [];
  ALScanBlanks(S, Pos);
  for I := Low(DateSeq) to High(DateSeq) do
  begin
    if (AFormatSettings.DateSeparator <> ' ') and
       not ((DateSeq[I].FPart = TALDatePart.dpChar) and (DateSeq[I].FChar = ' ')) then
      ALScanBlanks(S, Pos);
    case DateSeq[I].FPart of
    TALDatePart.dpNone:
      Break;
    TALDatePart.dpEraName:
      begin
        if (I < High(DateSeq)) and (DateSeq[I].FPart <> TALDatePart.dpNone) then
        begin
          if not ALScanName(S, Pos, Name, True) then
            Exit(False);
        end
        else
        begin
          ALScanToNumber(S, Pos);
          Name := Trim(ALCopyStr(S, {0 +} 1, Pos - Low(AnsiString)));
        end;
        EraYearOffset := AFormatSettings.GetEraYearOffset(Name);
        if EraYearOffset = -MaxInt then
          Exit(False);
        Include(Was, spEra);
      end;
    TALDatePart.dpDSep:
      begin
        if AFormatSettings.DateSeparator = ' ' then
        begin
          if not ALScanBlanks(S, Pos) then
            Exit(False);
        end
        else if AFormatSettings.DateSeparator <> #0 then
        begin
          if not ALScanChar(S, Pos, AFormatSettings.DateSeparator) then
            // Year is optional
            if (Pos > Length(S)) or (S[Pos] = ' ') then
            begin
              if (DateSeq[I + 1].FPart = TALDatePart.dpYear) and (DateSeq[I + 2].FPart = TALDatePart.dpNone) then
                Break
              else if (DateSeq[I + 1].FPart in [TALDatePart.dpMonth, TALDatePart.dpDay]) and (DateSeq[I + 2].FPart = TALDatePart.dpNone) and (I = 4) then
              begin
                if (DateSeq[1].FPart = TALDatePart.dpYear) and (DateSeq[3].FPart = TALDatePart.dpMonth) and (DateSeq[5].FPart = TALDatePart.dpDay) and
                   (Was = [spYear, spShortYear, spMonth]) then
                begin
                  D := M;
                  M := Y;
                end
                else if (DateSeq[1].FPart = TALDatePart.dpYear) and (DateSeq[3].FPart = TALDatePart.dpDay) and (DateSeq[5].FPart = TALDatePart.dpMonth) and
                        (Was = [spYear, spShortYear, spDay]) then
                begin
                  M := D;
                  D := Y;
                end
                else
                  Exit(False);
                Was := [spDay, spMonth];
                Break;
              end
              else
                Exit(False);
            end
            else
              Exit(False);
        end;
      end;
    TALDatePart.dpMonth:
      begin
        if spMonth in Was then
          Exit(False);
        if DateSeq[I].FLen >= 3 then
        begin
          if not ALScanName(S, Pos, Name, (AFormatSettings.DateSeparator <> '.') and (DateSeq[I].FLen = 3)) then
            Exit(False);
          if DateSeq[I].FLen = 3 then
            PNames := @AFormatSettings.ShortMonthNames
          else
            PNames := @AFormatSettings.LongMonthNames;
          M := 0;
          for J := 1 to 12 do
            if ALSameTextA(PNames^[J], Name) then
            begin
              M := J;
              Break;
            end;
          if M = 0 then
            Exit(False);
        end
        else
        begin
          if ALScanNumber(S, Pos, M, DateSeq[I].FLen) = 0 then
            Exit(False);
        end;
        Include(Was, spMonth);
      end;
    TALDatePart.dpDay:
      if DateSeq[I].FLen >= 3 then
      begin
        if spDayOfWeek in Was then
          Exit(False);
        if not ALScanName(S, Pos, Name, (AFormatSettings.DateSeparator <> '.') and (DateSeq[I].FLen = 3)) then
        begin
          if CanDiscard() then
            Break
          else
            Exit(False);
        end
        else
        begin
          if DateSeq[I].FLen = 3 then
            PNames := @AFormatSettings.ShortDayNames
          else
            PNames := @AFormatSettings.LongDayNames;
          DW := 0;
          for J := 1 to 7 do
            if ALSameTextA(PNames^[J], Name) then
            begin
              DW := J;
              Break;
            end;
          if DW = 0 then
            Exit(False);
          Include(Was, spDayOfWeek);
        end;
      end
      else
      begin
        if spDay in Was then
          Exit(False);
        if ALScanNumber(S, Pos, D, DateSeq[I].FLen) = 0 then
          Exit(False);
        Include(Was, spDay);
      end;
    TALDatePart.dpYear,
    TALDatePart.dpYearCurEra:
      begin
        if spYear in Was then
          Exit(False);
        // Year is optional
        if ((Pos > Length(S)) or (S[Pos] = ' ')) and
           (DateSeq[I + 1].FPart = TALDatePart.dpNone) then
          Break;
        // Consider year in the last era, when the mask has 'ee' or 'e', and 'g' is not yet occured
        if DateSeq[I].FPart = TALDatePart.dpYearCurEra then
        begin
          if EraYearOffset = 0 then
          begin
            if High(AFormatSettings.EraInfo) >= 0 then
              EraYearOffset := AFormatSettings.EraInfo[High(AFormatSettings.EraInfo)].EraOffset - 1
            else
              Exit(False);
          end;
        end
        else
          EraYearOffset := 0;
        // Try read as maximum digits as it is possible
        if (DateSeq[I].FLen <= 2) and
           ((I = High(DateSeq)) or not (DateSeq[I + 1].FPart in [TALDatePart.dpMonth, TALDatePart.dpDay, TALDatePart.dpYear, TALDatePart.dpYearCurEra])) then
          J := 4
        else
          J := DateSeq[I].FLen;
        J := ALScanNumber(S, Pos, Y, J);
        if J = 0 then
          Exit(False);
        // Consider year as "short year", when the mask has 'y', 'yy', etc
        if (J <= 2) and (DateSeq[I].FPart = TALDatePart.dpYear) then
          Include(Was, spShortYear);
        Include(Was, spYear);
      end;
    TALDatePart.dpChar:
      for J := 1 to DateSeq[I].FLen do
        if not ALScanChar(S, Pos, DateSeq[I].FChar) then
        begin
          if CanDiscard() then
            Break
          else
            Exit(False);
        end;
    else
      Exit(False);
    end;
  end;
  if not (spYear in Was) then
    Y := CurrentYear
  else if EraYearOffset > 0 then
    Y := EraToYear(Y)
  else if [spYear, spShortYear] * Was = [spYear, spShortYear] then
  begin
    CenturyBase := CurrentYear - AFormatSettings.TwoDigitYearCenturyWindow;
    Inc(Y, CenturyBase div 100 * 100);
    if (AFormatSettings.TwoDigitYearCenturyWindow > 0) and (Y < CenturyBase) then
      Inc(Y, 100);
  end;
  if not (spDay in Was) then
    D := 1;
  if not (spMonth in Was) then
    Exit(False);
  Result := TryEncodeDate(Y, M, D, Date);
  if Result and (spDayOfWeek in Was) then
  begin
    if DayOfWeek(Date) <> DW then
      Exit(False);
  end;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.ScanTimeRegular is still the same and adjust the IFDEF'}
{$ENDIF}
function ALScanTimeRegular(
           const S: AnsiString; var Pos: Integer; var Time: TDateTime;
           const AFormatSettings: TALFormatSettingsA): Boolean;
var
  BaseHour: Integer;
  Hour, Min, Sec, MSec: Word;
begin
  Result := False;
  BaseHour := -1;
  if ALScanString(S, Pos, AFormatSettings.TimeAMString{, True}) or ALScanString(S, Pos, 'AM'{, False}) then
    BaseHour := 0
  else if ALScanString(S, Pos, AFormatSettings.TimePMString{, True}) or ALScanString(S, Pos, 'PM'{, False}) then
    BaseHour := 12;
  if BaseHour >= 0 then ALScanBlanks(S, Pos);
  if ALScanNumber(S, Pos, Hour, -1) = 0 then Exit;
  Min := 0;
  Sec := 0;
  MSec := 0;
  if ALScanChar(S, Pos, AFormatSettings.TimeSeparator) then
  begin
    if ALScanNumber(S, Pos, Min, -1) = 0 then Exit;
    if ALScanChar(S, Pos, AFormatSettings.TimeSeparator) then
    begin
      if ALScanNumber(S, Pos, Sec, -1) = 0 then Exit;
      if (AFormatSettings.DecimalSeparator <> #0) and ALScanChar(S, Pos, AFormatSettings.DecimalSeparator) or
         (AFormatSettings.DecimalSeparator <> '.') and ALScanChar(S, Pos, '.') or
         (AFormatSettings.DecimalSeparator <> ',') and ALScanChar(S, Pos, ',') then
        if ALScanNumber(S, Pos, MSec, -1) = 0 then Exit;
    end;
  end;
  if BaseHour < 0 then
    if ALScanString(S, Pos, AFormatSettings.TimeAMString{, True}) or ALScanString(S, Pos, 'AM'{, False}) then
      BaseHour := 0
    else
      if ALScanString(S, Pos, AFormatSettings.TimePMString{, True}) or ALScanString(S, Pos, 'PM'{, False}) then
        BaseHour := 12;
  if BaseHour >= 0 then
  begin
    if (Hour = 0) or (Hour > 12) then Exit;
    if Hour = 12 then Hour := 0;
    Inc(Hour, BaseHour);
  end;
  ALScanBlanks(S, Pos);
  Result := TryEncodeTime(Hour, Min, Sec, MSec, Time);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.ScanTimeUsingShortTimeFormat is still the same and adjust the IFDEF'}
{$ENDIF}
function ALScanTimeUsingShortTimeFormat(
           const S: AnsiString; var Pos: Integer; var Time: TDateTime;
           const AFormatSettings: TALFormatSettingsA): Boolean;
type
  TSpecifiedParts = set of (spHour, spMin, spSec, spMSec);
var
  DateSeq: TALDateSeq;
  I, J: Integer;
  BaseHour: Integer;
  Hour, Min, Sec, MSec: Word;
  Was: TSpecifiedParts;
begin
  BaseHour := -1;
  DateSeq := ALParseDateTimeFormat(AFormatSettings.ShortTimeFormat, True);
  Hour := 0;
  Min := 0;
  Sec := 0;
  MSec := 0;
  Was := [];
  ALScanBlanks(S, Pos);
  for I := Low(DateSeq) to High(DateSeq) do
  begin
    if (AFormatSettings.TimeSeparator <> ' ') and
       not ((DateSeq[I].FPart = TALDatePart.dpChar) and (DateSeq[I].FChar = ' ')) then
      ALScanBlanks(S, Pos);
    case DateSeq[I].FPart of
    TALDatePart.dpNone:
      Break;
    TALDatePart.dpTSep:
      begin
        if AFormatSettings.TimeSeparator = ' ' then
        begin
          if not ALScanBlanks(S, Pos) then
            Exit(False);
        end
        else if AFormatSettings.TimeSeparator <> #0 then
        begin
          if not ALScanChar(S, Pos, AFormatSettings.TimeSeparator) then
            Exit(False);
        end;
      end;
    TALDatePart.dpMSSep:
      if not (
          (AFormatSettings.DecimalSeparator <> #0) and ALScanChar(S, Pos, AFormatSettings.DecimalSeparator) or
          (AFormatSettings.DecimalSeparator <> '.') and ALScanChar(S, Pos, '.') or
          (AFormatSettings.DecimalSeparator <> ',') and ALScanChar(S, Pos, ',')) then
        Exit(False);
    TALDatePart.dpHour:
      begin
        if spHour in Was then
          Exit(False);
        if ALScanNumber(S, Pos, Hour, 2) = 0 then
          Exit(False);
        Include(Was, spHour);
      end;
    TALDatePart.dpMin:
      begin
        if spMin in Was then
          Exit(False);
        if ALScanNumber(S, Pos, Min, 2) = 0 then
          Exit(False);
        Include(Was, spMin);
      end;
    TALDatePart.dpSec:
      begin
        if spSec in Was then
          Exit(False);
        if ALScanNumber(S, Pos, Sec, 2) = 0 then
          Exit(False);
        Include(Was, spSec);
      end;
    TALDatePart.dpMSec:
      begin
        if spMSec in Was then
          Exit(False);
        if ALScanFractional(S, Pos, MSec, 3, DateSeq[I].FLen) = 0 then
          Exit(False);
        Include(Was, spMSec);
      end;
    TALDatePart.dpAmPm,
    TALDatePart.dpAP,
    TALDatePart.dpAmPmS:
      if ALScanString(S, Pos, AFormatSettings.TimeAMString{, True}) or ALScanString(S, Pos, 'AM'{, False}) then
        BaseHour := 0
      else if ALScanString(S, Pos, AFormatSettings.TimePMString{, True}) or ALScanString(S, Pos, 'PM'{, False}) then
        BaseHour := 12
      else if (AFormatSettings.TimeAMString <> '') and ALScanChar(S, Pos, AFormatSettings.TimeAMString[1]) or
              ALScanChar(S, Pos, 'A') then
        BaseHour := 0
      else if (AFormatSettings.TimePMString <> '') and ALScanChar(S, Pos, AFormatSettings.TimePMString[1]) or
              ALScanChar(S, Pos, 'P') then
        BaseHour := 12;
    TALDatePart.dpChar:
      for J := 1 to DateSeq[I].FLen do
        if not ALScanChar(S, Pos, DateSeq[I].FChar) then
          Exit(False);
    else
      Exit(False);
    end;
  end;
  if BaseHour >= 0 then
  begin
    if (Hour = 0) or (Hour > 12) then Exit(False);
    if Hour = 12 then Hour := 0;
    Inc(Hour, BaseHour);
  end;
  Result := TryEncodeTime(Hour, Min, Sec, MSec, Time);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.ScanTime is still the same and adjust the IFDEF'}
{$ENDIF}
function ALScanTime(
           const S: AnsiString; var Pos: Integer; var Time: TDateTime;
           const AFormatSettings: TALFormatSettingsA): Boolean; overload;
var
  OrigPos: Integer;
begin
  OrigPos := Pos;
  Result := ALScanTimeRegular(S, Pos, Time, AFormatSettings);
  if (not Result or (Pos <= High(S)) and (Pos > Low(S)) and (S[Pos - 1] <> ' ')) and
     (AFormatSettings.ShortTimeFormat <> '') then
  begin
    Pos := OrigPos;
    Result := ALScanTimeUsingShortTimeFormat(S, Pos, Time, AFormatSettings) and
      (Pos > High(S));
  end;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.TryStrToDate is still the same and adjust the IFDEF'}
{$ENDIF}
function ALTryStrToDate(
           const S: AnsiString;
           out Value: TDateTime;
           const AFormatSettings: TALFormatSettingsA): Boolean;
var
  Pos: Integer;
begin
  Pos := Low(ansiString);
  Result := ALScanDate(S, Pos, Value, AFormatSettings) and (Pos > High(S));
end;

{**********************}
function ALTryStrToDate(
           const S: string;
           out Value: TDateTime;
           const AFormatSettings: TALFormatSettingsW): Boolean;
begin
  result := system.sysutils.TryStrToDate(S, Value, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.StrToDate is still the same and adjust the IFDEF'}
{$ENDIF}
function ALStrToDate(
           const S: AnsiString;
           const AFormatSettings: TALFormatSettingsA): TDateTime;
begin
  if not ALTryStrToDate(S, Result, AFormatSettings) then
    ALConvertErrorFmt(@System.SysConst.SInvalidDate, [S]);
end;

{*******************}
function ALStrToDate(
           const S: string;
           const AFormatSettings: TALFormatSettingsW): TDateTime;
begin
  result := system.sysutils.StrToDate(S, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.TryStrToTime is still the same and adjust the IFDEF'}
{$ENDIF}
function ALTryStrToTime(
           const S: AnsiString;
           out Value: TDateTime;
           const AFormatSettings: TALFormatSettingsA): Boolean;
var
  Pos: Integer;
begin
  Pos := Low(ansiString);
  Result := ALScanTime(S, Pos, Value, AFormatSettings) and (Pos > High(S));
end;

{***********************}
function  ALTryStrToTime(
            const S: string;
            out Value: TDateTime;
            const AFormatSettings: TALFormatSettingsW): Boolean;
begin
  result := system.sysutils.TryStrToTime(S, Value, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.StrToTime is still the same and adjust the IFDEF'}
{$ENDIF}
function ALStrToTime(
           const S: AnsiString;
           const AFormatSettings: TALFormatSettingsA): TDateTime;
begin
  if not ALTryStrToTime(S, Result, AFormatSettings) then
    ALConvertErrorFmt(@System.SysConst.SInvalidTime, [S]);
end;

{*******************}
function ALStrToTime(
           const S: string;
           const AFormatSettings: TALFormatSettingsW): TDateTime;
begin
  result := system.sysutils.StrToTime(S, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.TryStrToDateTime is still the same and adjust the IFDEF'}
{$ENDIF}
{$R-} {Range-Checking}
function ALTryStrToDateTime(
           const S: AnsiString;
           out Value: TDateTime;
           const AFormatSettings: TALFormatSettingsA): Boolean;
var
  Pos: Integer;
  NumberPos: Integer;
  BlankPos, OrigBlankPos: Integer;
  LDate, LTime: TDateTime;
  Stop: Boolean;
begin
  Result := True;
  Pos := Low(ansistring);
  LTime := 0;

  // date data scanned; searched for the time data
  if ALScanDate(S, Pos, LDate, AFormatSettings) then
  begin
    // search for time data; search for the first number in the time data
    NumberPos := Pos;
    ALScanToNumber(S, NumberPos);

    // the first number of the time data was found
    if NumberPos < High(S) then
    begin
      // search between the end of date and the start of time for AM and PM
      // strings; if found, then ScanTime from this position where it is found
      BlankPos := Pos - 1;
      Stop := False;
      while (not Stop) and (BlankPos < NumberPos) do
      begin
        // blank was found; scan for AM/PM strings that may follow the blank
        if (BlankPos > 0) and (BlankPos < NumberPos) then
        begin
          Inc(BlankPos); // start after the blank
          OrigBlankPos := BlankPos; // keep BlankPos because ScanString modifies it
          Stop := ALScanString(S, BlankPos, AFormatSettings.TimeAMString{, True}) or
                  ALScanString(S, BlankPos, 'AM'{, False}) or
                  ALScanString(S, BlankPos, AFormatSettings.TimePMString{, True}) or
                  ALScanString(S, BlankPos, 'PM'{, False});

          // ScanString jumps over the AM/PM string; if found, then it is needed
          // by ScanTime to correctly scan the time
          BlankPos := OrigBlankPos;
        end
        // no more blanks found; end the loop
        else
          Stop := True;

        // search of the next blank if no AM/PM string has been found
        if not Stop then
        begin
          while (S[BlankPos] <> ' ') and (BlankPos <= High(S)) do
            Inc(BlankPos);
          if BlankPos > High(S) then
            BlankPos := 0;
        end;
      end;

      // loop was forcely stopped; check if AM/PM has been found
      if Stop then
        // AM/PM has been found; check if it is before or after the time data
        if BlankPos > 0 then
          if BlankPos < NumberPos then // AM/PM is before the time number
            Pos := BlankPos
          else
            Pos := NumberPos // AM/PM is after the time number
        else
          Pos := NumberPos
      // the blank found is after the the first number in time data
      else
        Pos := NumberPos;

      // get the time data
      Result := ALScanTime(S, Pos, LTime, AFormatSettings);

      // time data scanned with no errors
      if Result then
        if LDate >= 0 then
          Value := LDate + LTime
        else
          Value := LDate - LTime;
    end
    // no time data; return only date data
    else
      Value := LDate;
  end
  // could not scan date data; try to scan time data
  else
    Result := ALTryStrToTime(S, Value, AFormatSettings)
end;
{$IF defined(ALRangeCheckingON)}
  {$R+} {Range-Checking}
{$ENDIF}

{**************************}
function ALTryStrToDateTime(
           const S: string;
           out Value: TDateTime;
           const AFormatSettings: TALFormatSettingsW): Boolean;
begin
  result := system.sysutils.TryStrToDateTime(S, Value, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.StrToDateTime is still the same and adjust the IFDEF'}
{$ENDIF}
function ALStrToDateTime(
           const S: AnsiString;
           const AFormatSettings: TALFormatSettingsA): TDateTime;
begin
  if not ALTryStrToDateTime(S, Result, AFormatSettings) then
    ALConvertErrorFmt(@System.SysConst.SInvalidDateTime, [S]);
end;

{************************}
function  ALStrToDateTime(
            const S: string;
            const AFormatSettings: TALFormatSettingsW): TDateTime;
begin
  Result := system.sysutils.StrToDateTime(S, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system._ValLong is still the same and adjust the IFDEF'}
{$ENDIF}
// Hex : ( '$' | 'X' | 'x' | '0X' | '0x' ) [0-9A-Fa-f]*
// Dec : ( '+' | '-' )? [0-9]*
{$R-} {Range-Checking}
function _ALValLong(const S: ansiString; var Code: Integer): Integer;
const
  FirstIndex = Low(ansistring);
var
  I: Integer;
  Dig: Integer;
  Sign: Boolean;
  Empty: Boolean;
begin
  I := FirstIndex;
  Sign := False;
  Result := 0;
  Empty := True;

  if S = '' then
  begin
    Code := 1;
    Exit;
  end;
  while S[I] = ' ' do
    Inc(I);

  if S[I] = '-' then
  begin
    Sign := True;
    Inc(I);
  end
  else if S[I] = '+' then
    Inc(I);
  // Hex
  if ((S[I] = '0') and (I < High(S)) and ((S[I+1] = 'X') or (S[I+1] = 'x'))) or
      (S[I] = '$') or
      (S[I] = 'X') or
      (S[I] = 'x') then
  begin
    if S[I] = '0' then
      Inc(I);
    Inc(I);
    while True do
    begin
      case S[I] of
       '0'..'9': Dig := Ord(S[I]) - Ord('0');
       'A'..'F': Dig := Ord(S[I]) - Ord('A') + 10;
       'a'..'f': Dig := Ord(S[I]) - Ord('a') + 10;
      else
        Break;
      end;
      if (Result < 0) or (Result > (High(Integer) shr 3)) then
        Break;
      Result := Result shl 4 + Dig;
      Inc(I);
      Empty := False;
    end;

    if Sign then
      Result := - Result;
  end
  // Decimal
  else
  begin
    while True do
    begin
      case S[I] of
        '0'..'9': Dig := Ord(S[I]) - Ord('0');
      else
        Break;
      end;
      if (Result < 0) or (Result > (High(Integer) div 10)) then
        Break;
      Result := Result*10 + Dig;
      Inc(I);
      Empty := False;
    end;

    if Sign then
      Result := - Result;
    if (Result <> 0) and (Sign <> (Result < 0)) then
      Dec(I);
  end;

  if ((S[I] <> ansiChar(#0)) or Empty) then
    Code := I + 1 - FirstIndex
  else
    Code := 0;
end;
{$IF defined(ALRangeCheckingON)}
  {$R+} {Range-Checking}
{$ENDIF}

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system._ValInt64 is still the same and adjust the IFDEF'}
{$ENDIF}
{$R-} {Range-Checking}
function _ALValInt64(const S: AnsiString; var Code: Integer): Int64;
const
  FirstIndex = Low(ansistring);
var
  I: Integer;
  Dig: Integer;
  Sign: Boolean;
  Empty: Boolean;
begin
  I := FirstIndex;
  Sign := False;
  Result := 0;
  Empty := True;

  if S = '' then
  begin
    Code := 1;
    Exit;
  end;
  while S[I] = ' ' do
    Inc(I);

  if S[I] = '-' then
  begin
    Sign := True;
    Inc(I);
  end
  else if S[I] = '+' then
    Inc(I);
  // Hex
  if ((S[I] = '0') and (I < High(S)) and ((S[I+1] = 'X') or (S[I+1] = 'x'))) or
      (S[I] = '$') or
      (S[I] = 'X') or
      (S[I] = 'x') then
  begin
    if S[I] = '0' then
      Inc(I);
    Inc(I);
    while True do
    begin
      case S[I] of
       '0'..'9': Dig := Ord(S[I]) - Ord('0');
       'A'..'F': Dig := Ord(S[I]) - Ord('A') + 10;
       'a'..'f': Dig := Ord(S[I]) - Ord('a') + 10;
      else
        Break;
      end;
      if (Result < 0) or (Result > (High(Int64) shr 3)) then
        Break;
      Result := Result shl 4 + Dig;
      Inc(I);
      Empty := False;
    end;

    if Sign then
      Result := - Result;
  end
  // Decimal
  else
  begin
    while True do
    begin
      case S[I] of
        '0'..'9': Dig := Ord(S[I]) - Ord('0');
      else
        Break;
      end;
      if (Result < 0) or (Result > (High(Int64) div 10)) then
        Break;
      Result := Result*10 + Dig;
      Inc(I);
      Empty := False;
    end;

    if Sign then
      Result := - Result;
    if (Result <> 0) and (Sign <> (Result < 0)) then
      Dec(I);
  end;

  if ((S[I] <> ansiChar(#0)) or Empty) then
    Code := I + 1 - FirstIndex
  else
    Code := 0;
end;
{$IF defined(ALRangeCheckingON)}
  {$R+} {Range-Checking}
{$ENDIF}

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system._ValUInt64 is still the same and adjust the IFDEF'}
{$ENDIF}
{$R-} {Range-Checking}
function _ALValUInt64(const s: ansistring; var code: Integer): UInt64;
const
  FirstIndex = Low(ansistring);
var
  i: Integer;
  dig: Integer;
  sign: Boolean;
  empty: Boolean;
begin
  i := FirstIndex;
  Result := 0;
  if s = '' then
  begin
    code := 1;
    exit;
  end;
  while s[i] = ansiChar(' ') do
    Inc(i);
  sign := False;
  if s[i] =  ansiChar('-') then
  begin
    sign := True;
    Inc(i);
  end
  else if s[i] =  ansiChar('+') then
    Inc(i);
  empty := True;
  if (s[i] =  ansiChar('$')) or (Upcase(s[i]) =  ansiChar('X'))
    or ((s[i] =  ansiChar('0')) and (I < High(S)) and (Upcase(s[i+1]) =  ansiChar('X'))) then
  begin
    if s[i] =  ansiChar('0') then
      Inc(i);
    Inc(i);
    while True do
    begin
      case   ansiChar(s[i]) of
       ansiChar('0').. ansiChar('9'): dig := Ord(s[i]) -  Ord('0');
       ansiChar('A').. ansiChar('F'): dig := Ord(s[i]) - (Ord('A') - 10);
       ansiChar('a').. ansiChar('f'): dig := Ord(s[i]) - (Ord('a') - 10);
      else
        break;
      end;
      if Result > (High(UInt64) shr 4) then
        Break;
      if sign and (dig <> 0) then
        Break;
      Result := Result shl 4 + Cardinal(dig);
      Inc(i);
      empty := False;
    end;
  end
  else
  begin
    while True do
    begin
      case  ansiChar(s[i]) of
        ansiChar('0').. ansiChar('9'): dig := Ord(s[i]) - Ord('0');
      else
        break;
      end;
                // 18446744073709551615
      if Result >= 1844674407370955161 then
      begin
        if (Result > 1844674407370955161) or (High(UInt64) - Result*10 < dig) then
          Break
      end;
      if sign and (dig <> 0) then
        Break;
      Result := Result*10 + Cardinal(dig);
      Inc(i);
      empty := False;
    end;
  end;
  if (s[i] <> ansiChar(#0)) or empty then
    code := i + 1 - FirstIndex
  else
    code := 0;
end;
{$IF defined(ALRangeCheckingON)}
  {$R+} {Range-Checking}
{$ENDIF}

{***********************************************************************}
function ALTryStrToInt(const S: AnsiString; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Value := _ALValLong(S, E);
  Result := E = 0;
end;

{********************************************************************}
function  ALTryStrToInt(const S: string; out Value: Integer): Boolean;
begin
  result := system.sysutils.TryStrToInt(S, Value);
end;

{************************************************}
function ALStrToInt(const S: AnsiString): Integer;
var
  E: Integer;
begin
  Result := _ALValLong(S, E);
  if E <> 0 then raise EConvertError.CreateResFmt(@System.SysConst.SInvalidInteger, [S]);
end;

{********************************************}
function ALStrToInt(const S: string): Integer;
begin
  Result := system.sysutils.StrToInt(S);
end;

{*********************************************************************}
function ALStrToIntDef(const S: AnsiString; Default: Integer): Integer;
var
  E: Integer;
begin
  Result := _ALValLong(S, E);
  if E <> 0 then Result := Default;
end;

{*****************************************************************}
function ALStrToIntDef(const S: string; Default: Integer): Integer;
begin
  result := system.sysutils.StrToIntDef(S, Default);
end;

{**************************************************************************}
function  ALTryStrToUInt(const S: AnsiString; out Value: Cardinal): Boolean;
var
  I64: Int64;
  E: Integer;
begin
  I64 := _ALValInt64(S, E);
  Value := I64;
  Result := (E = 0) and (Cardinal.MinValue <= I64) and (I64 <= Cardinal.MaxValue);
end;

{**********************************************************************}
function  ALTryStrToUInt(const S: String; out Value: Cardinal): Boolean;
begin
  Result := System.SysUtils.TryStrToUInt(S, Value);
end;

{***************************************************}
function  ALStrToUInt(const S: AnsiString): Cardinal;
var
  I64: Int64;
  E: Integer;
begin
  I64 := _ALValInt64(S, E);
  if (E <> 0) or not((Cardinal.MinValue <= I64) and (I64 <= Cardinal.MaxValue)) then
    raise EConvertError.CreateResFmt(@System.SysConst.SInvalidInteger, [S]);
  Result := I64;
end;

{***********************************************}
function  ALStrToUInt(const S: String): Cardinal;
begin
  Result := System.SysUtils.StrToUInt(S);
end;

{*************************************************************************}
function  ALStrToUIntDef(const S: Ansistring; Default: Cardinal): Cardinal;
var
  I64: Int64;
  E: Integer;
begin
  I64 := _ALValInt64(S, E);
  if (E <> 0) or not((Cardinal.MinValue <= I64) and (I64 <= Cardinal.MaxValue)) then
    Result := Default
  else
    Result := I64;
end;

{*********************************************************************}
function  ALStrToUIntDef(const S: string; Default: Cardinal): Cardinal;
begin
  Result := System.SysUtils.StrToUIntDef(S, Default);
end;

{***********************************************************************}
function ALTryStrToInt64(const S: AnsiString; out Value: Int64): Boolean;
var
  E: Integer;
begin
  Value := _ALValInt64(S, E);
  Result := E = 0;
end;

{*******************************************************************}
function ALTryStrToInt64(const S: string; out Value: Int64): Boolean;
begin
  Result := system.sysutils.TryStrToInt64(S, Value);
end;

{************************************************}
function ALStrToInt64(const S: AnsiString): Int64;
var
  E: Integer;
begin
  Result := _ALValInt64(S, E);
  if E <> 0 then raise EConvertError.CreateResFmt(@System.SysConst.SInvalidInteger, [S]);
end;

{********************************************}
function ALStrToInt64(const S: string): Int64;
begin
  Result := system.sysutils.StrToInt64(S);
end;

{*************************************************************************}
function ALStrToInt64Def(const S: AnsiString; const Default: Int64): Int64;
var
  E: Integer;
begin
  Result := _ALValInt64(S, E);
  if E <> 0 then Result := Default;
end;

{*********************************************************************}
function ALStrToInt64Def(const S: string; const Default: Int64): Int64;
begin
  Result := system.sysutils.StrToInt64Def(S, Default);
end;

{*************************************************************************}
function ALTryStrToUInt64(const S: ansistring; out Value: UInt64): Boolean;
var
  E: Integer;
begin
  Value := _ALValUInt64(S, E);
  Result := E = 0;
end;

{*********************************************************************}
function ALTryStrToUInt64(const S: String; out Value: UInt64): Boolean;
begin
  Result := system.sysutils.TryStrToUInt64(S, Value);
end;

{**************************************************}
function ALStrToUInt64(const S: ansistring): UInt64;
var
  E: Integer;
begin
  Result := _ALValUInt64(S, E);
  if E <> 0 then raise EConvertError.CreateResFmt(@System.SysConst.SInvalidInteger, [S]);
end;

{**********************************************}
function ALStrToUInt64(const S: String): UInt64;
begin
  Result := system.sysutils.StrToUInt64(S);
end;

{****************************************************************************}
function ALStrToUInt64Def(const S: ansistring; const Default: UInt64): UInt64;
var
  E: Integer;
begin
  Result := _ALValUInt64(S, E);
  if E <> 0 then Result := Default;
end;

{************************************************************************}
function ALStrToUInt64Def(const S: String; const Default: UInt64): UInt64;
begin
  Result := system.sysutils.StrToUInt64Def(S, Default);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.TwoDigitLookup is still the same and adjust the IFDEF'}
{$ENDIF}
const
  ALTwoDigitLookup : packed array[0..99] of array[1..2] of AnsiChar =
    ('00','01','02','03','04','05','06','07','08','09',
     '10','11','12','13','14','15','16','17','18','19',
     '20','21','22','23','24','25','26','27','28','29',
     '30','31','32','33','34','35','36','37','38','39',
     '40','41','42','43','44','45','46','47','48','49',
     '50','51','52','53','54','55','56','57','58','59',
     '60','61','62','63','64','65','66','67','68','69',
     '70','71','72','73','74','75','76','77','78','79',
     '80','81','82','83','84','85','86','87','88','89',
     '90','91','92','93','94','95','96','97','98','99');

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils._IntToStr32 is still the same and adjust the IFDEF'}
{$ENDIF}
function _ALIntToStr32(Value: Cardinal; Negative: Boolean): AnsiString;
var
  I, K: Cardinal;
  Digits: Integer;
  P: PAnsiChar;
begin
  I := Value;
  if I >= 10000 then
    if I >= 1000000 then
      if I >= 100000000 then
        Digits := 9 + Byte(Ord(I >= 1000000000))
      else
        Digits := 7 + Byte(Ord(I >= 10000000))
    else
      Digits := 5 + Byte(Ord(I >= 100000))
  else
    if I >= 100 then
      Digits := 3 + Byte(Ord(I >= 1000))
    else
      Digits := 1 + Byte(Ord(I >= 10));
  SetLength(Result, Digits + Ord(Negative));
  P := Pointer(Result);
  P^ := '-';
  Inc(P, Ord(Negative));
  while Digits > 1 do
  begin
    K := I;
    I := I div 100;
    Dec(K, I * 100);
    Dec(Digits, 2);
    PWord(@P[Digits])^ := Word(ALTwoDigitLookup[K]);
  end;
  if Digits <> 0 then
    P^ := AnsiChar(I or Ord('0'));
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils._IntToStr64 is still the same and adjust the IFDEF'}
{$ENDIF}
function _ALIntToStr64(Value: UInt64; Negative: Boolean): AnsiString;
var
  I64, K64 : UInt64;
  I32, K32 : Cardinal;
  Digits   : Byte;
  P        : PAnsiChar;
begin
  {Within Integer Range - Use Faster Integer Version}
  if (Int64Rec(Value).Hi = 0) then
    Exit(_ALIntToStr32(Value, Negative));

  I64 := Value;
  if I64 >= 100000000000000 then
    if I64 >= 10000000000000000 then
      if I64 >= 1000000000000000000 then
        if I64 >= 10000000000000000000 then
          Digits := 20
        else
          Digits := 19
      else
        Digits := 17 + Byte(Ord(I64 >= 100000000000000000))
    else
      Digits := 15 + Byte(Ord(I64 >= 1000000000000000))
  else
    if I64 >= 1000000000000 then
      Digits := 13 + Byte(Ord(I64 >= 10000000000000))
    else
      if I64 >= 10000000000 then
        Digits := 11 + Byte(Ord(I64 >= 100000000000))
      else
        Digits := 10;
  SetLength(Result, Digits + Ord(Negative));
  P := Pointer(Result);
  P^ := '-';
  Inc(P, Ord(Negative));
  if Digits = 20 then
  begin
    P^ := '1';
    Inc(P);
    Dec(I64, 10000000000000000000);
    Dec(Digits);
  end;
  if Digits > 17 then
  begin {18 or 19 Digits}
    if Digits = 19 then
    begin
      P^ := '0';
      while I64 >= 1000000000000000000 do
      begin
        Dec(I64, 1000000000000000000);
        Inc(P^);
      end;
      Inc(P);
    end;
    P^ := '0';
    while I64 >= 100000000000000000 do
    begin
      Dec(I64, 100000000000000000);
      Inc(P^);
    end;
    Inc(P);
    Digits := 17;
  end;
  K64 := I64;
  I64 := I64 div 100000000;
  Dec(K64, I64 * 100000000); {Remainder = 0..99999999}
  K32 := K64;
  I32 := K32 div 100;
  Dec(K32, I32 * 100);
  PWord(P + Digits - 2)^ := Word(ALTwoDigitLookup[K32]);
  K32 := I32;
  I32 := I32 div 100;
  Dec(K32, I32 * 100);
  PWord(P + Digits - 4)^ := Word(ALTwoDigitLookup[K32]);
  K32 := I32;
  I32 := I32 div 100;
  Dec(K32, I32 * 100);
  PWord(P + Digits - 6)^ := Word(ALTwoDigitLookup[K32]);
  PWord(P + Digits - 8)^ := Word(ALTwoDigitLookup[I32]);
  Dec(Digits, 8);
  I32 := I64; {Dividend now Fits within Integer - Use Faster Version}
  while Digits > 1 do
  begin
    K32 := I32;
    I32 := I32 div 100;
    Dec(K32, I32 * 100);
    Dec(Digits, 2);
    PWord(@P[Digits])^ := Word(ALTwoDigitLookup[K32]);
  end;
  if Digits <> 0 then
    P^ := AnsiChar(I32 or Ord('0'));
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.IntToStr is still the same and adjust the IFDEF'}
{$ENDIF}
function ALIntToStrA(Value: Integer): AnsiString;
begin
  Result := _ALIntToStr32(Abs(Value), Value < 0);
end;

{*******************************************************}
procedure ALIntToStrA(Value: Integer; var s: ansiString);
begin
  s := _ALIntToStr32(Abs(Value), Value < 0);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.IntToStr is still the same and adjust the IFDEF'}
{$ENDIF}
function ALIntToStrA(Value: Int64): AnsiString;
begin
  Result := _ALIntToStr64(Abs(Value), Value < 0);
end;

{*****************************************************}
procedure ALIntToStrA(Value: Int64; var s: ansiString);
begin
  s := _ALIntToStr64(Abs(Value), Value < 0);
end;

{*******************************************}
function ALIntToStrW(Value: Integer): String;
begin
  result := System.SysUtils.IntToStr(Value);
end;

{***************************************************}
procedure ALIntToStrW(Value: Integer; var s: String);
begin
  s := System.SysUtils.IntToStr(Value);
end;

{*****************************************}
function ALIntToStrW(Value: Int64): String;
begin
  result := System.SysUtils.IntToStr(Value);
end;

{*************************************************}
procedure ALIntToStrW(Value: Int64; var s: String);
begin
  s := System.SysUtils.IntToStr(Value);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.UIntToStr is still the same and adjust the IFDEF'}
{$ENDIF}
function ALUIntToStrA(Value: Cardinal): AnsiString;
begin
  Result := _ALIntToStr32(Value, False);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.UIntToStr is still the same and adjust the IFDEF'}
{$ENDIF}
function ALUIntToStrA(Value: UInt64): AnsiString;
begin
  Result := _ALIntToStr64(Value, False);
end;

{*********************************************}
function ALUIntToStrW(Value: Cardinal): String;
begin
  Result := System.SysUtils.UIntToStr(Value);
end;

{*******************************************}
function ALUIntToStrW(Value: UInt64): String;
begin
  Result := System.SysUtils.UIntToStr(Value);
end;

{***}
const
  ALTwoHexLookup : packed array[0..255] of array[1..2] of AnsiChar =
  ('00','01','02','03','04','05','06','07','08','09','0A','0B','0C','0D','0E','0F',
   '10','11','12','13','14','15','16','17','18','19','1A','1B','1C','1D','1E','1F',
   '20','21','22','23','24','25','26','27','28','29','2A','2B','2C','2D','2E','2F',
   '30','31','32','33','34','35','36','37','38','39','3A','3B','3C','3D','3E','3F',
   '40','41','42','43','44','45','46','47','48','49','4A','4B','4C','4D','4E','4F',
   '50','51','52','53','54','55','56','57','58','59','5A','5B','5C','5D','5E','5F',
   '60','61','62','63','64','65','66','67','68','69','6A','6B','6C','6D','6E','6F',
   '70','71','72','73','74','75','76','77','78','79','7A','7B','7C','7D','7E','7F',
   '80','81','82','83','84','85','86','87','88','89','8A','8B','8C','8D','8E','8F',
   '90','91','92','93','94','95','96','97','98','99','9A','9B','9C','9D','9E','9F',
   'A0','A1','A2','A3','A4','A5','A6','A7','A8','A9','AA','AB','AC','AD','AE','AF',
   'B0','B1','B2','B3','B4','B5','B6','B7','B8','B9','BA','BB','BC','BD','BE','BF',
   'C0','C1','C2','C3','C4','C5','C6','C7','C8','C9','CA','CB','CC','CD','CE','CF',
   'D0','D1','D2','D3','D4','D5','D6','D7','D8','D9','DA','DB','DC','DD','DE','DF',
   'E0','E1','E2','E3','E4','E5','E6','E7','E8','E9','EA','EB','EC','ED','EE','EF',
   'F0','F1','F2','F3','F4','F5','F6','F7','F8','F9','FA','FB','FC','FD','FE','FF');

{***************************************************************}
function _ALIntToHex(Value: UInt64; Digits: Integer): AnsiString;
var
  I32    : Integer;
  I, J   : UInt64;
  P      : Integer;
  NewLen : Integer;
  Sb     : TArray<ansiChar>;
begin
  NewLen := 1;
  I := Value shr 4;
  while I > 0 do
  begin
    Inc(NewLen);
    I := I shr 4;
  end;
  if Digits > NewLen then
  begin
    SetLength(Sb, Digits);
    for I32 := 0 to (Digits - NewLen) - 1 do
      Sb[I32] := '0';
    P := Digits - NewLen;
  end
  else
  begin
    SetLength(Sb, NewLen);
    P := 0;
  end;
  I := Value;
  while NewLen > 2 do
  begin
    J := I and $FF;
    I := I shr 8;
    Dec(NewLen, 2);
    Sb[P + NewLen] := AlTwoHexLookup[J][1];
    Sb[P + NewLen + 1] := AlTwoHexLookup[J][2];

  end;
  if NewLen = 2 then
  begin
    Sb[P] := AlTwoHexLookup[I][1];
    Sb[P+1] := AlTwoHexLookup[I][2];
  end
  else
    Sb[P] := AlTwoHexLookup[I][2];

  //Result := String.Create(Sb);
  SetLength(Result, Length(Sb));
  alMove(Sb[0], PansiChar(Result)^, Length(Sb));
end;

{****************************************************************}
function ALIntToHexA(Value: Integer; Digits: Integer): AnsiString;
begin
  Result := _ALIntToHex(Cardinal(Value), Digits);
end;

{**************************************************************}
function ALIntToHexA(Value: Int64; Digits: Integer): AnsiString;
begin
  Result := _ALIntToHex(Value, digits);
end;

{***************************************************************}
function ALIntToHexA(Value: UInt64; Digits: Integer): AnsiString;
begin
  Result := _ALIntToHex(Value, digits);
end;

{*************************************************************}
function  ALIntToHexW(Value: Integer; Digits: Integer): String;
begin
  Result := IntToHex(Value, Digits);
end;

{***********************************************************}
function  ALIntToHexW(Value: Int64; Digits: Integer): String;
begin
  Result := IntToHex(Value, Digits);
end;

{************************************************************}
function  ALIntToHexW(Value: UInt64; Digits: Integer): String;
begin
  Result := IntToHex(Value, Digits);
end;

{******************************************************************}
procedure _ALBinToHex(Buffer: PByte; Text: PByte; BufSize: Integer);
const
  B2HConvert: array[0..15] of Byte = ($30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $61, $62, $63, $64, $65, $66);
var
  I: Integer;
begin
  for I := 0  to BufSize - 1 do
  begin
    Text[I * 2] := B2HConvert[Buffer[I] shr 4];
    Text[I * 2 + 1] := B2HConvert[Buffer[I] and $0F];
  end;
end;

{******************************************************************************}
Function  ALTryBinToHex(const aBin: AnsiString; out Value: AnsiString): boolean;
begin
  if aBin = '' then exit(false);
  setlength(Value,length(aBin) * 2);
  _ALBinToHex(
    PByte(@aBin[low(aBin)]),
    PByte(@Value[low(Value)]),
    length(aBin));
  result := true;
end;

{***************************************************************************************}
Function  ALTryBinToHex(const aBin; aBinSize : Cardinal; out Value: AnsiString): boolean;
begin
  if aBinSize = 0 then exit(false);
  setlength(Value, aBinSize * 2);
  _ALBinToHex(
    PByte(@aBin),
    PByte(@Value[low(Value)]),
    aBinSize);
  result := true;
end;

{**********************************************************************}
Function  ALTryBinToHex(const aBin: TBytes; out Value: String): boolean;
var bufOut: TBytes;
begin
  if length(aBin) = 0 then exit(false);
  setlength(bufOut,length(aBin) * 2);
  _ALBintoHex(
    Pbyte(@aBin[0]), // Buffer: TBytes
    Pbyte(@bufOut[0]), // Text: TBytes;
    length(aBin)); // Count: Integer
  Value := Tencoding.UTF8.GetString(bufOut); // UTF8 is good because bufOut must contain only low ascii chars
  result := true;
end;

{***********************************************************************************}
Function  ALTryBinToHex(const aBin; aBinSize : Cardinal; out Value: String): boolean;
var bufOut: TBytes;
begin
  if aBinSize = 0 then exit(false);
  setlength(bufOut,aBinSize * 2);
  _ALBintoHex(
    Pbyte(@aBin), // Buffer: TBytes
    Pbyte(@bufOut[0]), // Text: TBytes;
    aBinSize); // Count: Integer
  Value := Tencoding.UTF8.GetString(bufOut); // UTF8 is good because bufOut must contain only low ascii chars
  result := true;
end;

{********************************************************}
Function  ALBinToHexA(const aBin: AnsiString): AnsiString;
begin
  if not ALTryBinToHex(aBin, Result) then
    raise Exception.Create('Bad binary value');
end;

{*****************************************************************}
Function  ALBinToHexA(const aBin; aBinSize : Cardinal): AnsiString;
begin
  if not ALTryBinToHex(aBin, aBinSize, Result) then
    raise Exception.Create('Bad binary value');
end;

{************************************************}
Function  ALBinToHexW(const aBin: TBytes): String;
begin
  if not ALTryBinToHex(aBin, Result) then
    raise Exception.Create('Bad binary value');
end;

{*************************************************************}
Function  ALBinToHexW(const aBin; aBinSize : Cardinal): String;
begin
  if not ALTryBinToHex(aBin, aBinSize, Result) then
    raise Exception.Create('Bad binary value');
end;

{******************************************************************************}
Function  ALTryHexToBin(const aHex: AnsiString; out Value: AnsiString): boolean;
var l: integer;
begin
  l := length(aHex);
  if (l = 0) or (l mod 2 <> 0) then exit(False);
  setlength(Value,l div 2);
  result := HexToBin(PansiChar(aHex),pansiChar(Value),length(Value)) = l div 2;
end;

{**********************************************************************}
Function  ALTryHexToBin(const aHex: String; out Value: TBytes): boolean;
var l: integer;
begin
  l := length(aHex);
  if (l = 0) or (l mod 2 <> 0) then exit(False);
  setlength(Value,l div 2);
  result := HexToBin(
              PChar(aHex), // Text
              0, // TextOffset
              Value, //Buffer
              0, // BufOffset
              length(Value)) = l div 2;
end;

{*******************************************************}
Function  ALHexToBin(const aHex: AnsiString): AnsiString;
begin
  if not ALTryHexToBin(aHex, Result) then
    raise Exception.Create('Bad hex value');
end;

{***********************************************}
Function  ALHexToBin(const aHex: String): TBytes;
begin
  if not ALTryHexToBin(aHex, Result) then
    raise Exception.Create('Bad hex value');
end;

{****************************************************************}
function ALIntToBitA(value: integer; digits: integer): ansistring;
begin
  result := StringOfChar (ansiChar('0'), digits) ;
  while value > 0 do begin
    if (value and 1) = 1 then
      result[digits] := '1';
    dec(digits) ;
    value := value shr 1;
  end;
end;

{**********************************************}
function ALBitToInt(Value: ansiString): integer;
var i: Integer;
begin

  //init result
  Result:=0;

  //remove leading zeroes
  i := 1;
  while (i <= length(Value)) and (Value[i] = '0') do inc(i);
  if i > length(Value) then exit;
  Value := ALCopyStr(Value,I,Maxint);

  //do the conversion
  for i:=Length(Value) downto 1 do
   if Value[i]='1' then
    Result:=Result+(1 shl (Length(Value)-i));

end;

{********************************************************************************}
function AlInt2BaseN(NumIn: UInt64; const charset: array of ansiChar): ansistring;
var Remainder: UInt64;
    BaseOut: integer;
begin

  //ex convert 1544745455 to base26
  //                                               |
  //1544745455 / 26 = 59413286, reste  19 -> T     |      26 / 26 = 1, reste 0 -> A
  //59413286 / 26 = 2285126, reste 10 -> K         |      1 -> B
  //2285126 / 26 = 87889, reste 12 -> M            |
  //87889 / 26 = 3380, reste 9 -> J                |      26 = BA
  //3380 / 26 = 130, reste 0 -> A                  |
  //130 / 26 = 5, reste 0 -> A                     |
  //5 -> G                                         |
  //                                               |
  //1544745455 = GAAJMKT

  if length(charset) = 0 then raise Exception.Create('Charset must not be empty');

  result := '';
  BaseOut := length(charset);
  while NumIn >= BaseOut do begin
    DivMod(NumIn{Dividend}, BaseOut{Divisor}, NumIn{Result}, Remainder{Remainder});
    Result := charset[Remainder] + Result;
  end;
  Result := charset[NumIn] + Result;

end;

{************************************************************************************}
function AlBaseN2Int(const Str: ansiString; const charset: array of ansiChar): UInt64;
var BaseIn: Byte;
    Lst: TALStringListA;
    I,j: integer;
    P: UInt64;
begin

  //
  //ex convert ABCD to int
  //
  // ABCD = (26*26*26) * 0 + (26*26) * 1 + (26) * 2 + 3 = 731
  //                     A             B          C   D

  Lst := TALStringListA.Create;
  try

    Lst.CaseSensitive := True;
    Lst.Duplicates := DupError;
    for I := Low(charset) to High(charset) do
      Lst.addObject(charset[i],pointer(i));
    Lst.Sorted := True;

    result := 0;
    P := 1;
    BaseIn := length(charset);
    for I := length(Str) downto 1 do begin
      J := Lst.IndexOf(Str[I]);
      if J < 0 then raise EALException.CreateFmt('Character (%s) not found in charset', [Str[I]]);
      result := result + (Uint64(Lst.Objects[J]) * P);
      P := P * BaseIn;
    end;

  finally
    Lst.Free;
  end;

end;

////////////////////
////// Base64 //////
////////////////////

//
// Taken from https://github.com/synopse/mORMot.git
// https://synopse.info
// http://mormot.net
//

var
  _Base64Encoding: TBase64Encoding;

{****************************************}
function _GetBase64Encoding: TNetEncoding;
var LEncoding: TBase64Encoding;
begin
  if _Base64Encoding = nil then begin
    LEncoding := TBase64Encoding.Create(0); // this constructor to omits line breaks
    if AtomicCmpExchange(Pointer(_Base64Encoding), Pointer(LEncoding), nil) <> nil then ALFreeAndNil(LEncoding)
    {$IFDEF AUTOREFCOUNT}
    else _Base64Encoding.__ObjAddRef;
    {$ENDIF AUTOREFCOUNT}
  end;
  Result := _Base64Encoding;
end;

{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if https://github.com/synopse/mORMot.git SynCommons.pas was not updated from References\mORMot\SynCommons.pas and adjust the IFDEF'}
{$ENDIF}

type
  TBase64Enc = array[0..63] of AnsiChar;
  PBase64Enc = ^TBase64Enc;
  TBase64Dec = array[AnsiChar] of shortint;
  PBase64Dec = ^TBase64Dec;
const
  b64enc: TBase64Enc =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

var
  /// a conversion table from Base64 text into binary data
  // - used by Base64ToBin/IsBase64 functions
  // - contains -1 for invalid char, -2 for '=', 0..63 for b64enc[] chars
  ConvertBase64ToBin: TBase64Dec;

const
  sInvalidbase64String = 'Invalid base64 string';

{********************************************************************************************}
function Base64AnyDecode(const decode: TBase64Dec; sp,rp: PAnsiChar; len: NativeInt): boolean;
var c, ch: NativeInt;
begin
  result := false;
  while len>=4 do begin
    c := decode[sp[0]];
    if c<0 then
      exit;
    c := c shl 6;
    ch := decode[sp[1]];
    if ch<0 then
      exit;
    c := (c or ch) shl 6;
    ch := decode[sp[2]];
    if ch<0 then
      exit;
    c := (c or ch) shl 6;
    ch := decode[sp[3]];
    if ch<0 then
      exit;
    c := c or ch;
    rp[2] := AnsiChar(c);
    c := c shr 8;
    rp[1] := AnsiChar(c);
    c := c shr 8;
    rp[0] := AnsiChar(c);
    dec(len,4);
    inc(rp,3);
    inc(sp,4);
  end;
  if len>=2 then begin
    c := decode[sp[0]];
    if c<0 then
      exit;
    c := c shl 6;
    ch := decode[sp[1]];
    if ch<0 then
      exit;
    if len=2 then
      rp[0] := AnsiChar((c or ch) shr 4) else begin
      c := (c or ch) shl 6;
      ch := decode[sp[2]];
      if ch<0 then
        exit;
      c := (c or ch) shr 2;
      rp[1] := AnsiChar(c);
      rp[0] := AnsiChar(c shr 8);
    end;
  end;
  result := true;
end;

{*******************************************************************}
function Base64EncodeMain(rp, sp: PAnsiChar; len: cardinal): integer;
var c: cardinal;
    enc: PBase64Enc; // use local register
begin
  enc := @b64enc;
  len := len div 3;
  result := len;
  if len<>0 then
    repeat
      c := (ord(sp[0]) shl 16) or (ord(sp[1]) shl 8) or ord(sp[2]);
      rp[0] := enc[(c shr 18) and $3f];
      rp[1] := enc[(c shr 12) and $3f];
      rp[2] := enc[(c shr 6) and $3f];
      rp[3] := enc[c and $3f];
      inc(rp,4);
      inc(sp,3);
      dec(len);
    until len=0;
end;

{***********************************************************************}
procedure Base64EncodeTrailing(rp, sp: PAnsiChar; len: cardinal); inline;
var c: cardinal;
    enc: PBase64Enc; // use local register
begin
  enc := @b64enc;
  case len of
    1: begin
      c := ord(sp[0]) shl 4;
      rp[0] := enc[(c shr 6) and $3f];
      rp[1] := enc[c and $3f];
      PWord(rp+2)^ := ord('=')+ord('=') shl 8;
    end;
    2: begin
      c := (ord(sp[0]) shl 10) or (ord(sp[1]) shl 2);
      rp[0] := enc[(c shr 12) and $3f];
      rp[1] := enc[(c shr 6) and $3f];
      rp[2] := enc[c and $3f];
      rp[3] := '=';
    end;
  end;
end;

{*******************************************************}
procedure Base64Encode(rp, sp: PAnsiChar; len: cardinal);
var main: cardinal;
begin
  main := Base64EncodeMain(rp,sp,len);
  Base64EncodeTrailing(rp+main*4,sp+main*3,len-main*3);
end;

{******************************************************}
function BinToBase64Length(len: NativeUInt): NativeUInt;
begin
  result := ((len+2)div 3)*4;
end;

{*******************************************************************}
function Base64ToBinLength(sp: PAnsiChar; len: NativeInt): NativeInt;
var dec: PBase64Dec;
begin
  result := 0;
  if (len=0) or (len and 3<>0) then
    exit;
  dec := @ConvertBase64ToBin;
  if dec[sp[len-2]]>=0 then
    if dec[sp[len-1]]>=0 then
      result := 0 else
      result := 1 else
      result := 2;
  result := (len shr 2)*3-result;
end;

{*************************************************************************************}
function Base64ToBinSafe(sp: PAnsiChar; len: NativeInt; var data: AnsiString): boolean;
var resultLen: NativeInt;
begin
  resultLen := Base64ToBinLength(sp,len);
  if resultLen<>0 then begin
    SetString(data,nil,resultLen);
    if ConvertBase64ToBin[sp[len-2]]>=0 then
      if ConvertBase64ToBin[sp[len-1]]>=0 then else
        dec(len) else
        dec(len,2); // adjust for Base64AnyDecode() algorithm
    result := Base64AnyDecode(ConvertBase64ToBin,sp,pointer(data),len);
    if not result then
      data := '';
  end else begin
    result := false;
    data := '';
  end;
end;

{********************************************************************************}
function  ALBase64EncodeString(const P: PansiChar; const ln: Integer): AnsiString;
begin
  result := '';
  if ln=0 then
    exit;
  SetString(result,nil,BinToBase64Length(ln));
  Base64Encode(pointer(result),P,ln);
end;

{**************************************************************}
function  ALBase64EncodeString(const S: AnsiString): AnsiString;
var len: integer;
begin
  result := '';
  len := length(s);
  if len=0 then
    exit;
  SetString(result,nil,BinToBase64Length(len));
  Base64Encode(pointer(result),pointer(s),len);
end;

{****************************************************************************************}
Function  ALBase64EncodeString(const S: String; const AEncoding: TEncoding = nil): String;
var BufIn: TBytes;
begin
  if assigned(AEncoding) then BufIn := AEncoding.GetBytes(S)
  else BufIn := TEncoding.unicode.GetBytes(S);
  result := _GetBase64Encoding.EncodeBytesToString(BufIn);
end;

{********************************************************************************}
function  ALBase64DecodeString(const P: PansiChar; const ln: Integer): AnsiString;
begin
  if ln=0 then exit('');
  if not Base64ToBinSafe(P,ln,result) then
    raise Exception.Create(sInvalidbase64String);
end;

{**************************************************************}
function  ALBase64DecodeString(const S: AnsiString): AnsiString;
begin
  if S='' then exit('');
  if not Base64ToBinSafe(pointer(s),length(s),result) then
    raise Exception.Create(sInvalidbase64String);
end;

{****************************************************************************************}
Function  ALBase64DecodeString(const S: String; const AEncoding: TEncoding = nil): String;
var BufOut: TBytes;
begin
  BufOut := _GetBase64Encoding.DecodeStringToBytes(S);
  if assigned(AEncoding) then result := AEncoding.GetString(BufOut)
  else result := TEncoding.unicode.GetString(BufOut);
end;

{*********************}
{$ZEROBASEDSTRINGS OFF}
function  ALBase64EncodeStringMIME(const S: AnsiString): AnsiString;
var Ln: integer;
    CountOfCRLF: integer;
    CurrentPos: integer;
    TmpStr: AnsiString;
    i: integer;
const maximumLineLength = 76;
begin

  //https://en.wikipedia.org/wiki/Base64
  //MIME does not specify a fixed length for Base64-encoded lines, but it does specify a maximum line length of
  //76 characters. Additionally it specifies that any extra-alphabetic characters must be ignored by a
  //compliant decoder, although most implementations use a CR/LF newline pair to delimit encoded lines.

  result := ALBase64EncodeString(s);
  Ln := length(result);
  CountOfCRLF := (ln div maximumLineLength);
  if (ln mod maximumLineLength) = 0 then dec(CountOfCRLF);
  if CountOfCRLF > 0 then begin
    setlength(TmpStr, ln + (CountOfCRLF * 2));
    CurrentPos := 0;
    for I := 0 to CountOfCRLF - 1 do begin
      AlMove(pbyte(result)[CurrentPos], pbyte(TmpStr)[CurrentPos + (i * 2)], maximumLineLength); // pbyte(Result) to not jump inside uniqueString (Result is already unique thanks to previous SetLength))
      currentPos := currentPos + maximumLineLength;
      pbyte(TmpStr)[CurrentPos + (i * 2)] := 13;
      pbyte(TmpStr)[CurrentPos + (i * 2) +1] := 10;
    end;
    AlMove(pbyte(result)[CurrentPos], pbyte(TmpStr)[CurrentPos + (CountOfCRLF * 2)], ln-CurrentPos);
    result := TmpStr;
  end;

end;
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{******************************************************************}
function  ALBase64DecodeStringMIME(const S: AnsiString): AnsiString;
begin

  //https://en.wikipedia.org/wiki/Base64
  //MIME specifies that any extra-alphabetic characters must be ignored by a
  //compliant decoder, but here we just ignore the #13#10

  result := ALBase64DecodeString(AlStringReplaceA(s, #13#10, '', [rfReplaceAll]));

end;

{**********************************************************************************************************}
function  ALURLBase64EncodeString(const S: AnsiString; const aDoOnlyUrlEncode: boolean = false): AnsiString;
begin

  //It’s often more convenient to manage data in text format rather than binary data
  //(for example a string column in a database, or a string rendered into a HTTP response).
  //Common examples in security are digital signatures and encryption. Signing and encrypting
  //typically produce bytes of data and in a web application sometimes it’s just easier to
  //manage that data as text.
  //
  //Base64 is a useful tool for doing this encoding. The only problem is that base64
  //encoding uses characters that do not work well in URLs and sometimes HTTP headers
  //(e.g. the +, / and = characters are either reserved or have special meaning in URLs).
  //URL encoding is designed to address that problem, but it’s sometimes error prone (e.g.
  //double encoding) or the tooling just doesn’t do the right thing (IIS decodes %2F into
  //a / before it arrives into the application and thus confuses the ASP.NET routing framework).
  //It is very useful to put these sorts of values in a URL, but it’s also frustrating that
  //it’s problematic and that we have to work around these issues again and again.
  //
  //While reading the JWT specs they faced the same problem and they addressed it by using
  //base64url encoding (which is almost the same, yet different than base64 encoding).
  //Base64url encoding is basically base64 encoding except they use non-reserved URL
  //characters (e.g. – is used instead of + and _ is used instead of /) and they omit
  //the padding characters. I’ve been using this for some time now and am quite
  //happy with it as a replacement for base64 encoding.

  if aDoOnlyUrlEncode then result := S
  else result := ALBase64EncodeString(s);
  Result := ALStringReplaceA(Result, '+', '-', [rfReplaceALL]);
  Result := ALStringReplaceA(Result, '/', '_', [rfReplaceALL]);
  Result := ALStringReplaceA(Result, '=', '',  [rfReplaceALL]);

end;

{**********************************************************************************************************}
function  ALURLBase64DecodeString(const S: AnsiString; const aDoOnlyUrlDecode: boolean = false): AnsiString;
begin
  result := ALStringReplaceA(S,      '-', '+', [rfReplaceALL]);
  result := ALStringReplaceA(result, '_', '/', [rfReplaceALL]);
  while length(result) mod 4 <> 0 do
    result := result + '=';
  if aDoOnlyUrlDecode then exit;
  result := ALBase64DecodeString(result);
end;

{**********************************************************}
Function  ALBase64EncodeBytesW(const Bytes: Tbytes): String;
begin
  result := _GetBase64Encoding.EncodeBytesToString(Bytes);
end;

{********************************************************************************}
Function  ALBase64EncodeBytesW(const Bytes: pointer; const Size: Integer): String;
begin
  result := _GetBase64Encoding.EncodeBytesToString(Bytes, Size);
end;

{*****************************************************}
Function  ALBase64DecodeBytes(const S: String): Tbytes;
begin
  result := _GetBase64Encoding.DecodeStringToBytes(S);
end;

{*********************************************************************************************}
function ALIsDecimal(const S: AnsiString; const RejectPlusMinusSign: boolean = False): boolean;
var i: integer;
begin
  result := true;
  if S = '' then Exit(false);
  for i := low(s) to high(S) do begin
    if (not RejectPlusMinusSign) and (i=low(s)) then begin
      if not (S[i] in ['0'..'9','-','+']) then begin
        result := false;
        break;
      end;
    end
    else if not (S[i] in ['0'..'9']) then begin
      result := false;
      break;
    end;
  end;
end;

{***************************}
{$WARN SYMBOL_DEPRECATED OFF}
function ALIsDecimal(const S: String; const RejectPlusMinusSign: boolean = False): boolean;
var I: integer;
begin
  result := true;
  if S = '' then Exit(false);
  for i := low(s) to high(S) do begin
    if (not RejectPlusMinusSign) and (i=low(s)) then begin
      if not CharInSet(S[i], ['0'..'9','-','+']) then begin
        result := false;
        break;
      end;
    end
    else if not CharInSet(S[i], ['0'..'9']) then begin
      result := false;
      break;
    end;
  end;
end;
{$WARN SYMBOL_DEPRECATED ON}

{*************************************************}
Function ALIsInteger(const S: AnsiString): Boolean;
var i: integer;
Begin
  result := ALIsDecimal(S) and ALTryStrToInt(S, i);
End;

{*********************************************}
Function ALIsInteger(const S: String): Boolean;
var I: integer;
Begin
  result := ALIsDecimal(S) and ALTryStrToInt(S, i);
End;

{***********************************************}
Function ALIsInt64(const S: AnsiString): Boolean;
var i : int64;
Begin
  Result := ALIsDecimal(S) and ALTryStrToInt64(S, I);
End;

{*******************************************}
Function ALIsInt64(const S: String): Boolean;
var I : int64;
Begin
  Result := ALIsDecimal(S) and ALTryStrToInt64(S, I);
End;

{**************************************************}
Function ALIsSmallInt(const S: AnsiString): Boolean;
var i : Integer;
Begin
  Result := ALIsDecimal(S) and ALTryStrToInt(S, I) and (i <= 32767) and (I >= -32768);
End;

{**********************************************}
Function ALIsSmallInt(const S: String): Boolean;
var I : Integer;
Begin
  Result := ALIsDecimal(S) and ALTryStrToInt(S, I) and (i <= 32767) and (I >= -32768);
End;

{********************************************************************************************}
Function  ALIsFloat (const S: AnsiString; const AFormatSettings: TALFormatSettingsA): Boolean;
var i: integer;
    LDouble: Double;
begin
  if S = '' then Exit(false);
  for i := low(s) to high(s) do begin
    if not (S[i] in ['0'..'9','-',AFormatSettings.DecimalSeparator]) then begin
      result := false;
      exit;
    end;
  end;
  result := ALTryStrToFloat(s,LDouble,AFormatSettings);
end;

{***************************}
{$WARN SYMBOL_DEPRECATED OFF}
Function  ALIsFloat(const S: String; const AFormatSettings: TALFormatSettingsW): Boolean;
var I: integer;
    LDouble: Double;
begin
  if S = '' then Exit(false);
  for i := low(s) to high(s) do begin
    if not CharInSet(S[i], ['0'..'9','-',AFormatSettings.DecimalSeparator]) then begin
      result := false;
      exit;
    end;
  end;
  result := ALTryStrToFloat(s,LDouble,AFormatSettings);
end;
{$WARN SYMBOL_DEPRECATED ON}

{*********************************************************************************************}
function ALFloatToStrA(Value: Extended; const AFormatSettings: TALFormatSettingsA): AnsiString;
var
  Buffer: array[0..63] of AnsiChar;
begin
  SetString(
    Result,
    Buffer,
    ALInternalFloatToText(
      PByte(@Buffer),
      Value,
      fvExtended,
      ffGeneral,
      15,
      0,
      AFormatSettings));
end;

{*****************************************************************************************************}
procedure ALFloatToStrA(Value: Extended; var S: ansiString; const AFormatSettings: TALFormatSettingsA);
var
  Buffer: array[0..63] of AnsiChar;
begin
  SetString(
    S,
    Buffer,
    ALInternalFloatToText(
      PByte(@Buffer),
      Value,
      fvExtended,
      ffGeneral,
      15,
      0,
      AFormatSettings));
end;

{*****************************************************************************************}
function ALFloatToStrW(Value: Extended; const AFormatSettings: TALFormatSettingsW): String;
begin
  result := FloatToStr(Value, AFormatSettings);
end;

{*************************************************************************************************}
procedure ALFloatToStrW(Value: Extended; var S: String; const AFormatSettings: TALFormatSettingsW);
begin
  S := FloatToStr(Value, AFormatSettings);
end;

{**********************}
function ALFloatToStrFA(
           Value: Extended;
           Format: TFloatFormat;
           Precision, Digits: Integer;
           const AFormatSettings: TALFormatSettingsA): AnsiString;
var
  Buffer: array[0..63] of AnsiChar;
begin
  SetString(
    Result,
    Buffer,
    ALInternalFloatToText(
      PByte(@Buffer),
      Value,
      fvExtended,
      Format,
      Precision,
      Digits,
      AFormatSettings));
end;

{**********************}
function ALFloatToStrFW(
           Value: Extended;
           Format: TFloatFormat;
           Precision, Digits: Integer;
           const AFormatSettings: TALFormatSettingsW): String;
begin
  result := System.sysUtils.FloatToStrF(Value, Format, Precision, Digits, AFormatSettings);
end;

{*********************************************************************************************}
function  ALCurrToStrA(Value: Currency; const AFormatSettings: TALFormatSettingsA): AnsiString;
var
  Buffer: array[0..63] of AnsiChar;
begin
  SetString(
    Result,
    Buffer,
    ALInternalFloatToText(
      PByte(@Buffer),
      Value,
      fvCurrency,
      ffGeneral,
      0,
      0,
      AFormatSettings));
end;

{*****************************************************************************************}
function  ALCurrToStrW(Value: Currency; const AFormatSettings: TALFormatSettingsW): string;
begin
  result := system.sysutils.CurrToStr(Value, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if declaration below in system.Sysutils is still the same and adjust the IFDEF'}
{$ENDIF}
const
// 8087/SSE status word masks
  mIE = $0001;
  mDE = $0002;
  mZE = $0004;
  mOE = $0008;
  mUE = $0010;
  mPE = $0020;
{$IFDEF CPUX86}
  mC0 = $0100;
  mC1 = $0200;
  mC2 = $0400;
  mC3 = $4000;
{$ENDIF CPUX86}

{$IFDEF CPUX86}
const
// 8087 control word
// Infinity control  = 1 Affine
// Rounding Control  = 0 Round to nearest or even
// Precision Control = 3 64 bits
// All interrupts masked
  CWNear: Word = $133F;
{$ENDIF CPUX86}
{$IFDEF CPUX64}
const
//  MXCSR control word
// Rounding Control  = 0 Round to nearest or even
// All interrupts masked
  MXCSRNear: UInt32 = $1F80;
{$ENDIF CPUX64}

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.TestAndClearFPUExceptions is still the same and adjust the IFDEF'}
{$ENDIF}
{$IFDEF CPUX86}
function ALTestAndClearFPUExceptions(AExceptionMask: Word): Boolean;
asm
      PUSH    ECX
      MOV     CX, AX
      FSTSW   AX
      TEST    AX, CX
      JNE     @@bad
      XOR     EAX, EAX
      INC     EAX
      JMP     @@exit
@@bad:
      XOR     EAX, EAX
@@exit:
      POP     ECX
      FCLEX
      RET
end;
{$ENDIF CPUX86}

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.TestAndClearSSEExceptions is still the same and adjust the IFDEF'}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$IF Defined(CPUX64) and not Defined(EXTERNALLINKER)}
function ALTestAndClearSSEExceptions(AExceptionMask: UInt32): Boolean;
var
  MXCSR: UInt32;
begin
  MXCSR := GetMXCSR;
  Result := ((MXCSR and $003F) and AExceptionMask) = 0;
  ResetMXCSR;
end;
{$ENDIF CPUX64}
{$WARN SYMBOL_PLATFORM ON}


{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.GetSpecialValueAC is still the same and adjust the IFDEF'}
{$ENDIF}
function ALGetSpecialValueAC(Buffer: PAnsiChar; var Value; ValueType: TFloatValue): Boolean;
begin
  if (ValueType = fvExtended) and (Buffer^ in ['n', 'N', 'i', 'I', '+', '-']) then
  begin
    if system.Ansistrings.StrIComp(Buffer, 'NAN') = 0 then begin
      Extended(Value) := Extended.NaN;
      Result := True;
    end
    else if (system.Ansistrings.StrIComp(Buffer, 'INF') = 0) or (system.Ansistrings.StrIComp(Buffer, '+INF') = 0) then begin
      Extended(Value) := Extended.PositiveInfinity;
      Result := True;
    end
    else if system.Ansistrings.StrIComp(Buffer, '-INF') = 0 then begin
      Extended(Value) := Extended.NegativeInfinity;
      Result := True;
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.InternalTextToExtended is still the same and adjust the IFDEF'}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
//this function is not threadsafe because of Set8087CW / SetMXCSR
//https://quality.embarcadero.com/browse/RSP-39428
function ALInternalTextToExtended(
           ABuffer: PAnsiChar;
           var AValue: Extended;
           const AFormatSettings: TALFormatSettingsA): Boolean;
const
{$IFDEF EXTENDEDHAS10BYTES}
  CMaxExponent = 4999;
{$ELSE !EXTENDEDHAS10BYTES}
  CMaxExponent = 1024;
{$ENDIF !EXTENDEDHAS10BYTES}

  CExponent = 'E'; // DO NOT LOCALIZE;
  CPlus = '+';     // DO NOT LOCALIZE;
  CMinus = '-';    // DO NOT LOCALIZE;

var
{$IFDEF EXTERNALLINKER}
//  SavedRoundMode: Int32;
//  LSavedFlags: Word;
//  LDummyFlags: Word;
{$ELSE !EXTERNALLINKER}
{$IFDEF CPUX86}
  LSavedCtrlWord: Word;
{$ENDIF CPUX86}
{$IFDEF CPUX64}
  LSavedMXCSR: UInt32;
{$ENDIF CPUX64}
{$ENDIF}
  LPower: Integer;
  LSign: SmallInt;
  LResult: Extended;
  LCurrChar: AnsiChar;

  {~~~~~~~~~~~~~~~~~}
  procedure NextChar;
  begin
    LCurrChar := PAnsiChar(ABuffer)^;
    Inc(PAnsiChar(ABuffer));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure SkipWhitespace();
  begin
    { Skip white spaces }
    while LCurrChar = ' ' do
      NextChar;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ReadSign(): SmallInt;
  begin
    Result := 1;
    if LCurrChar = CPlus then
      NextChar()
    else if LCurrChar = CMinus then
    begin
      NextChar();
      Result := -1;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ReadNumber(var AOut: Extended): Integer;
  const
    CMax = UInt64.MaxValue div 10 - 9;
  var
    L: UInt64;
  begin
    Result := 0;
    if AOut = 0.0 then
    begin
      L := 0;
      while (LCurrChar >= '0') and (LCurrChar <= '9') and (L <= CMax) do
      begin
        L := L * 10;
        L := L + Ord(LCurrChar) - Ord('0');

        NextChar();
        Inc(Result);
      end;
      AOut := L;
    end;

    while (LCurrChar >= '0') and (LCurrChar <= '9') do
    begin
      AOut := AOut * 10;
      AOut := AOut + Ord(LCurrChar) - Ord('0');

      NextChar();
      Inc(Result);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ReadExponent(var ExponentNumber: Integer): Integer;
  var
    LSign: Integer;
  begin
    Result := 0;
    ExponentNumber := 0;
    LSign := ReadSign();
    while (LCurrChar >= '0') and (LCurrChar <= '9') do
    begin
      Inc(Result);
      ExponentNumber := ExponentNumber * 10;
      ExponentNumber := ExponentNumber + Ord(LCurrChar) - Ord('0');
      if ExponentNumber > CMaxExponent then
        Break;
      NextChar();
    end;

    if ExponentNumber > CMaxExponent then
      ExponentNumber := CMaxExponent;

    ExponentNumber := ExponentNumber * LSign;
  end;

var
  IntPart, FracPart: Integer;
  ExponentNumber: Integer;
  OrigBuff: PAnsiChar;
begin
  { Prepare }
  Result := False;
  NextChar();

{$IFDEF EXTERNALLINKER}

//  FEnvGetExceptFlag(LSavedFlags, fe_ALL_EXCEPT);
//  FEnvSetExceptFlag(LSavedFlags, fe_ALL_EXCEPT);
//  SavedRoundMode := FEnvGetRound;
//  FEnvSetRound(fe_TONEAREST);
{$ELSE  EXTERNALLINKER}
{$IFDEF CPUX86}
  { Prepare the FPU }
  LSavedCtrlWord := Get8087CW();
  ALTestAndClearFPUExceptions(0);
  Set8087CW(CWNear);
{$ENDIF CPUX86}
{$IF Defined(CPUX64)}
  { Prepare the FPU }
  LSavedMXCSR := GetMXCSR;
  ALTestAndClearSSEExceptions(0);
  SetMXCSR(MXCSRNear);
{$ENDIF Defined(CPUX64)}
{$ENDIF EXTERNALLINKER}
  try
    { Skip white spaces }
    SkipWhitespace();
    OrigBuff := ABuffer - 1;

    { Exit if nothing to do }
    if LCurrChar <> #0 then
    begin
      { Detect the sign of the number }
      LSign := ReadSign();
      if LCurrChar <> #0 then
      begin
        { De result }
        LResult := 0;

        { Read the integer and fractionary parts }
        IntPart := ReadNumber(LResult);
        FracPart := 0;

        if LCurrChar = AFormatSettings.DecimalSeparator then
        begin
          NextChar();
          FracPart := ReadNumber(LResult);
          LPower := -FracPart;
        end else
          LPower := 0;

        { Read the exponent and adjust the power }
        if Char(Word(LCurrChar) and $FFDF) = CExponent then
        begin
          NextChar();
          ExponentNumber := 0;
          if ReadExponent(ExponentNumber) = 0 then
            Exit(False);
          Inc(LPower, ExponentNumber);
        end;

        // Reject "E3" or ".E1" case.
        if (IntPart > 0) or (FracPart > 0) then
        begin
          { Skip white spaces }
          SkipWhitespace();

          { Continue only if the buffer is depleted }
          if LCurrChar = #0 then
          begin
            { Calculate the final number }
          {$IFDEF EXTERNALLINKER}
            try
              LResult := Power10(LResult, LPower) * LSign;
              AValue := LResult;
              Result := True;
            except
              Result := False;
            end;
          {$ELSE !EXTERNALLINKER}
            LResult := Power10(LResult, LPower) * LSign;
            AValue := LResult;
          {$ENDIF EXTERNALLINKER}

{$IFDEF EXTERNALLINKER}


//        Result := True;
{$ELSE !EXTERNALLINKER}
{$IFDEF CPUX86}
            { Final check that everything went OK }
            Result := ALTestAndClearFPUExceptions(mIE + mOE);
{$ENDIF CPUX86}
{$IFDEF CPUX64}
            { Final check that everything went OK }
            Result := ALTestAndClearSSEExceptions(mIE + mOE);
{$ENDIF CPUX64}
{$ENDIF EXTERNALLINKER}
          end;
        end;

        if not Result then
          Result := ALGetSpecialValueAC(OrigBuff, AValue, fvExtended);
      end;
    end;
  finally
    { Clear Math Exceptions }
{$IFDEF EXTERNALLINKER}

//  FEnvSetRound(SavedRoundMode);
//  FEnvSetExceptFlag(LDummyFlags, LSavedFlags);
{$ELSE  EXTERNALLINKER}
{$IFDEF CPUX86}
    Set8087CW(LSavedCtrlWord);
{$ENDIF CPUX86}
{$IFDEF CPUX64}
    SetMXCSR(LSavedMXCSR);
{$ENDIF CPUX64}
{$ENDIF EXTERNALLINKER}
  end;
end;
{$WARN SYMBOL_PLATFORM ON}

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.InternalTextToCurrency is still the same and adjust the IFDEF'}
{$ENDIF}
//this function is not threadsafe because of Set8087CW / SetMXCSR
//https://quality.embarcadero.com/browse/RSP-39428
function ALInternalTextToCurrency(
           ABuffer: PAnsiChar;
           var AValue: Currency;
           const AFormatSettings: TALFormatSettingsA): Boolean;
{$IF Defined(EXTENDEDHAS10BYTES) and not Defined(Linux64)}
const
  CMaxExponent = 4999;
  CExponent = 'E'; // DO NOT LOCALIZE;
  CPlus = '+';     // DO NOT LOCALIZE;
  CMinus = '-';    // DO NOT LOCALIZE;
var
{$IFDEF EXTERNALLINKER}

{$ELSE  EXTERNALLINKER}
{$IFDEF CPUX86}
{$IF defined(CPUX86) and not defined(EXTENDEDHAS10BYTES)}
  LSavedCtrlWord: Word;
{$ENDIF CPUX86 and !EXTENDEDHAS10BYTES}
{$ELSE}
  {$MESSAGE ERROR 'Unknown platform'}
{$ENDIF CPUX86}
{$ENDIF EXTERNALLINKER}
  LPower: Integer;
  LSign: SmallInt;
  LResult: Extended;
  LCurrChar: AnsiChar;

  {~~~~~~~~~~~~~~~~~}
  procedure NextChar;
  begin
    LCurrChar := ABuffer^;
    Inc(ABuffer);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure SkipWhitespace();
  begin
    { Skip white spaces }
    while LCurrChar = ' ' do
      NextChar;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ReadSign(): SmallInt;
  begin
    Result := 1;
    if LCurrChar = CPlus then
      NextChar()
    else if LCurrChar = CMinus then
    begin
      NextChar();
      Result := -1;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ReadNumber(var AOut: Extended): Integer;
  begin
    Result := 0;
    while (LCurrChar >= '0') and (LCurrChar <= '9') do
    begin
      AOut := AOut * 10;
      AOut := AOut + Ord(LCurrChar) - Ord('0');

      NextChar();
      Inc(Result);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ReadExponent: SmallInt;
  var
    LSign: SmallInt;
  begin
    LSign := ReadSign();
    Result := 0;
    while (LCurrChar >= '0') and (LCurrChar <= '9') do
    begin
      Result := Result * 10;
      Result := Result + Ord(LCurrChar) - Ord('0');
      NextChar();
    end;

    if Result > CMaxExponent then
      Result := CMaxExponent;

    Result := Result * LSign;
  end;

const
  One2p63 = 9223372036854775808.0; // 2^63
var
  IntPart, FracPart: Integer;
  OrigBuff: PAnsiChar;
begin
  { Prepare }
  Result := False;
  NextChar();

{$IFDEF EXTENDEDHAS10BYTES}

{$ELSE !EXTENDEDHAS10BYTES}
{$IFDEF CPUX86}
  { Prepare the FPU }
  LSavedCtrlWord := Get8087CW();
  ALTestAndClearFPUExceptions(0);
  Set8087CW(CWNear);
{$ELSE}
  {$MESSAGE ERROR 'Unknown platform'}
{$ENDIF CPUX86}
{$ENDIF EXTENDEDHAS10BYTES}
  try

    { Skip white spaces }
    SkipWhitespace();
    OrigBuff := ABuffer - 1;

    { Exit if nothing to do }
    if LCurrChar <> #0 then
    begin
      { Detect the sign of the number }
      LSign := ReadSign();
      if LCurrChar <> #0 then
      begin
        { De result }
        LResult := 0;

        { Read the integer and fractionary parts }
        IntPart := ReadNumber(LResult);
        FracPart := 0;

        if LCurrChar = AFormatSettings.DecimalSeparator then
        begin
          NextChar();
          FracPart := ReadNumber(LResult);
          LPower := -FracPart;
        end else
          LPower := 0;

        { Read the exponent and adjust the power }
        if Char(Word(LCurrChar) and $FFDF) = CExponent then
        begin
          NextChar();
          Inc(LPower, ReadExponent());
        end;

        // Reject "E3" or ".E1" case.
        if (IntPart <> 0) or (FracPart <> 0) then
        begin
          { Skip white spaces }
          SkipWhitespace();

          { Continue only if the buffer is depleted }
          if LCurrChar = #0 then
          begin
            { Calculate the final number }
            LResult := Power10(LResult, LPower+4) * LSign; // +4 for currency offset.
            // One2p63 = 9223372036854775808.0; // 2^63
            // This asymmetric comparison is due to the two's complement representation of ordinal number.
            if (LResult >= One2p63) or (LResult < -One2p63) then
              Exit(False);
            PInt64(@AValue)^ := Round(LResult);

{$IFDEF EXTENDEDHAS10BYTES}


            Result := true;
{$ELSE !EXTENDEDHAS10BYTES}
{$IFDEF CPUX86}
            { Final check that everything went OK }
            Result := ALTestAndClearFPUExceptions(mIE + mOE);
{$ELSE}
  {$MESSAGE ERROR 'Unknown platform'}
{$ENDIF CPUX86}
{$ENDIF EXTENDEDHAS10BYTES}
          end;
        end;

        if not Result then
          Result := ALGetSpecialValueAC(OrigBuff, AValue, fvCurrency);
      end;
    end;
  finally
    { Clear Math Exceptions }
{$IFDEF EXTENDEDHAS10BYTES}

{$ELSE !EXTENDEDHAS10BYTES}
{$IFDEF CPUX86}
    Set8087CW(LSavedCtrlWord);
{$ELSE}
  {$MESSAGE ERROR 'Unknown platform'}
{$ENDIF CPUX86}
{$ENDIF EXTENDEDHAS10BYTES}
  end;
end;
{$ELSE !EXTENDEDHAS10BYTES}
const
  CExponent = 'E'; // DO NOT LOCALIZE;
  CPlus = '+';     // DO NOT LOCALIZE;
  CMinus = '-';    // DO NOT LOCALIZE;
var
  LPower: Integer;
  LSign: SmallInt;
  BufIndex: Integer;

  {~~~~~~~~~~~~~~~~~~~~~~~}
  procedure SkipWhitespace;
  begin
    { Skip white spaces }
    while ABuffer[BufIndex] = ' ' do
      Inc(BufIndex);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ReadSign: SmallInt;
  begin
    Result := 1;
    if ABuffer[BufIndex] = CPlus then
      Inc(BufIndex)
    else if ABuffer[BufIndex] = CMinus then
    begin
      Inc(BufIndex);
      Result := -1;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ReadNumberPart: AnsiString;
  begin
    Result := '';
    while ABuffer[BufIndex] in ['0'..'9'] do
    begin
      Result := Result + ABuffer[BufIndex];
      Inc(BufIndex);
    end;
    // Skip remaining numbers.
    while ABuffer[BufIndex] in ['0'..'9'] do
      Inc(BufIndex);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ReadExponent: Integer;
  var
    LSign: Integer;
  begin
    LSign := ReadSign;
    Result := 0;
    while ABuffer[BufIndex] in ['0'..'9'] do
    begin
      Result := Result * 10;
      Result := Result + Ord(ABuffer[BufIndex]) - Ord('0');
      Inc(BufIndex);
    end;
    Result := Result * LSign;
  end;

var
  I: Integer;
  U64: UInt64;
  RoundUp: Boolean;
  IntPart, FracPart: AnsiString;
begin
  { Prepare }
  BufIndex := 0;
  Result := False;
  { Skip white spaces }
  SkipWhitespace;

  { Exit if nothing to do }
  if ABuffer[BufIndex] <> #0 then
  begin
    { Detect the sign of the number }
    LSign := ReadSign;
    if ABuffer[BufIndex] <> #0 then
    begin
      { Read the integer and fractionary parts }
      IntPart := ReadNumberPart;
      if ABuffer[BufIndex] = AFormatSettings.DecimalSeparator then
      begin
        Inc(BufIndex);
        FracPart := ReadNumberPart;
      end;

      LPower := 0;
      { Read the exponent and adjust the power }
      if Char(Word(ABuffer[BufIndex]) and $FFDF) = CExponent then
      begin
        Inc(BufIndex);
        LPower := ReadExponent;
      end;

      if (IntPart = '') and (FracPart = '') then
        Exit;

      { Skip white spaces }
      SkipWhitespace();

      { Continue only if the buffer is depleted }
      if ABuffer[BufIndex] = #0 then
      begin
        { Calculate the final number }
        LPower := LPower + 4; // Add Currency's offset digit

        if LPower > 0 then
        begin
          if Length(FracPart) < LPower then
            FracPart := FracPart + StringOfChar(AnsiChar('0'), LPower);
          IntPart := IntPart + ALCopyStr(FracPart, Low(AnsiString), LPower);
          FracPart := ALCopyStr(FracPart, Low(AnsiString) + LPower, MaxInt);
        end
        else if LPower < 0 then
        begin
          LPower := - LPower;
          if Length(IntPart) < LPower then
            IntPart := StringOfChar(AnsiChar('0'), LPower) + IntPart;
          FracPart := ALCopyStr(IntPart, Low(AnsiString) + (Length(IntPart) - LPower), LPower) + FracPart;
          IntPart := ALCopyStr(IntPart, Low(AnsiString), Length(IntPart) - LPower);
        end;

        if IntPart = '' then
          IntPart := '0';
        U64 := _ALValInt64(IntPart, I); // Val(IntPart, U64, I);
        if I <> 0 then
          Exit; // error
        if U64 > UInt64(Int64.MaxValue) + 1 then
          Exit; // overflow error

        if (FracPart <> '') and (FracPart[Low(FracPart)] >= '5') then
        begin
          RoundUp := True;
          // exact half -> False / more than half -> True
          if FracPart[Low(FracPart)] = '5' then
          begin
            RoundUp := False;
            for I := Low(FracPart) + 1 to High(FracPart) do
              if FracPart[I] <> '0' then
              begin
                RoundUp := True;
                Break;
              end;
            RoundUp := RoundUp or (IntPart[High(IntPart)] in ['1', '3', '5', '7', '9']);
          end;
          if RoundUp then
            Inc(U64); // U64 is UInt64. no overflow.
        end;

        if LSign < 0 then
        begin
          if U64 > UInt64(Int64.MaxValue) + 1 then
            Exit;
          U64 := (not U64) + 1; // negate
        end
        else
          if U64 > Int64.MaxValue then
            Exit;
        PUInt64(@AValue)^ := U64;
        Result := True;
      end;
    end;
  end;
end;
{$ENDIF !EXTENDEDHAS10BYTES}

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.TextToFloat is still the same and adjust the IFDEF'}
{$ENDIF}
function ALTextToFloat(
           Buffer: PAnsiChar; var Value;
           ValueType: TFloatValue; const AFormatSettings: TALFormatSettingsA): Boolean;
begin
  if ValueType = fvExtended then
    Result := ALInternalTextToExtended(Buffer, Extended(Value), AFormatSettings)
  else
    Result := ALInternalTextToCurrency(Buffer, Currency(Value), AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.SysUtils.InternalFloatToTextFmt is still the same and adjust the IFDEF'}
{$ENDIF}
function ALInternalFloatToTextFmt(
           Buf: PByte; const Value; ValueType: TFloatValue; Format: PByte;
           const AFormatSettings: TALFormatSettingsA{; const Unicode: Boolean}): Integer;
const
  CMinExtPrecision = 2;
{$IFDEF EXTENDEDHAS10BYTES}
  CMaxExtPrecision = 18;
{$ELSE !EXTENDEDHAS10BYTES}
  CMaxExtPrecision = 17;
{$ENDIF !EXTENDEDHAS10BYTES}

var
  AIndex: Integer;
  ThousandSep: Boolean;
  Section: ansiString;
  SectionIndex: Integer;
  FloatValue: TFloatRec;
  DecimalIndex: Integer;
  FirstDigit: Integer;
  LastDigit: Integer;
  DigitCount: Integer;
  Scientific: Boolean;
  Precision: Integer;
  Digits: Integer;
  DecimalSep: ansiChar;
  ThousandsSep: ansiString;
  FormatLength: Integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure AppendChar(const AChar: ansiChar);
  begin
    //if Unicode then
    //begin
    //  PWideChar(Buf)^ := char(AChar);
    //  Inc(Buf, SizeOf(Char));
    //end else
    //begin
      PByte(Buf)^ := Byte(AChar);
      Inc(Buf, SizeOf(Byte));
    //end;

    Inc(Result);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetLength(const ABuf: PByte): Integer;
  var
    //AWide: PChar;
    AAnsi: PByte;
  begin
    Result := 0;
    //if Unicode then
    //begin
    //  AWide := PChar(ABuf);
    //  while AWide^ <> #0 do
    //  begin
    //    Inc(AWide);
    //    Inc(Result);
    //  end;
    //end else
    //begin
      AAnsi := PByte(ABuf);
      while AAnsi^ <> Ord(#0) do
      begin
        Inc(AAnsi);
        Inc(Result);
      end;
    //end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetCharIndex(const ABuf: PByte; const Index: Integer): ansiChar;
  begin
    //if Unicode then
    //  Result := PWideChar(ABuf)[Index]
    //else
      Result := ansiChar(PByte(ABuf)[Index]);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure AppendString(const AStr: ansiString);
  var
    {I,} L: Integer;
  begin
    L := length(AStr);

    if L > 0 then
    begin
      //if Unicode then
      //begin
      //  { Unicode -- move directly }
      //  MoveChars(AStr[Low(string)], Buf^, L);
      //  Inc(Buf, L * SizeOf(Char));
      //end else
      //begin
      //  { ANSI -- loop }
      //  for I := Low(string) to High(AStr) do
      //  begin
      //    PByte(Buf)^ := Byte(AStr[I]);
      //    Inc(Buf, SizeOf(Byte));
      //  end;
      //end;
      ALMove(pointer(AStr)^, Buf^, L);
      Inc(Buf, L * SizeOf(AnsiChar));

      Inc(Result, L);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function FindSection(AIndex: Integer): Integer;
  var
    Section: Integer;
    C: Integer;
  begin
    Section := 0;
    C := 0;
    FormatLength := GetLength(Format);
    while (Section <> AIndex) and (C < FormatLength) do
    begin
      case GetCharIndex(Format, C) of
        ';': begin
          Inc(Section);
          Inc(C);
        end;

        '"': begin
          Inc(C);
          while (C < FormatLength) and (GetCharIndex(Format, C) <> '"') do
            Inc(C);
          if C < FormatLength then
            Inc(C);
        end;

        '''': begin
          Inc(C);
          while (C < FormatLength) and (GetCharIndex(Format, C) <> '''') do
            Inc(C);
          if C < FormatLength then
            Inc(C);
        end;

        else
          Inc(C);
      end;
    end;
    if (Section < AIndex) or (C = FormatLength) then
      Result := 0
    else
      Result := C;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ScanSection(APos: Integer): ansiString;
  var
    C: Integer;
    AChar: ansiChar;
    I: Integer;
  begin
    DecimalIndex := -1;
    Scientific := false;
    ThousandSep := false;
    C := APos;
    FirstDigit := 32767;
    DigitCount := 0;
    LastDigit := 0;
    while (C < FormatLength) and (GetCharIndex(Format, C) <> ';') do
    begin
      case GetCharIndex(Format, C) of
        ',': begin
          ThousandSep := true;
          Inc(C);
        end;

        '.': begin
          if DecimalIndex = -1 then
            DecimalIndex := DigitCount;
          Inc(C);
        end;

        '"': begin
          Inc(C);
          while (C < FormatLength) and (GetCharIndex(Format, C) <> '"') do
            Inc(C);
          if C < FormatLength then
            Inc(C);
        end;

        '''': begin
          Inc(C);
          while (C < FormatLength) and (GetCharIndex(Format, C) <> '''') do
            Inc(C);
          if C < FormatLength then
            Inc(C);
        end;

        'e', 'E': begin
          Inc(C);
          if C < FormatLength then
          begin
            AChar := GetCharIndex(Format, C);
            if (AChar = '-') or (AChar = '+') then
            begin
              Scientific := true;
              Inc(C);
              while (C < FormatLength) and (GetCharIndex(Format, C) = '0') do
                Inc(C);
            end;
          end;
        end;

        '#': begin
          Inc(DigitCount);
          Inc(C);
        end;

        '0': begin
          if DigitCount < FirstDigit then
            FirstDigit := DigitCount;

          Inc(DigitCount);
          LastDigit := DigitCount;
          Inc(C);
        end;

        else
          Inc(C);
      end;
    end;

    if DecimalIndex = -1 then
      DecimalIndex := DigitCount;
    LastDigit := DecimalIndex - LastDigit;
    if LastDigit > 0 then
      LastDigit := 0;

    FirstDigit := DecimalIndex - FirstDigit;
    if FirstDigit < 0 then
      FirstDigit := 0;
    Result := '';
    for I := APos to APos + (C - APos - 1) do
      Result := Result + GetCharIndex(Format, I);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function DigitsLength: Integer;
  var
    C: Integer;
  begin
    Result := 0;
    C := Low(FloatValue.Digits);
    while (C <= High(FloatValue.Digits)) and (FloatValue.Digits[C] <> Ord(#0)) do
    begin
      Inc(C);
      Inc(Result);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure ApplyFormat;
  var
    C: Integer;
    DigitDelta: Integer;
    DigitPlace: Integer;
    DigitsC: Integer;
    DigitsLimit: Integer;
    OldC: ansiChar;
    Sign: ansiChar;
    Zeros: Integer;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure WriteDigit(ADigit: ansiChar);
    begin
      if DigitPlace = 0 then
      begin
        AppendChar(DecimalSep);
        AppendChar(ADigit);
        Dec(DigitPlace);
      end
      else
      begin
        AppendChar(ADigit);
        Dec(DigitPlace);
        if ThousandSep and (DigitPlace > 1) and ((DigitPlace mod 3) = 0) then
          AppendString(ThousandsSep);
      end;
    end;

    {~~~~~~~~~~~~~~~~~}
    procedure AddDigit;
    var
      AChar: ansiChar;
    begin
      if DigitsC <= DigitsLimit then
      begin
        AChar := ansiChar(FloatValue.Digits[DigitsC]);
        Inc(DigitsC);
        WriteDigit(AChar);
      end
      else
      begin
        if DigitPlace <= LastDigit then
          Dec(DigitPlace)
        else
          WriteDigit('0');
      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~}
    procedure PutFmtDigit;
    begin
      if DigitDelta < 0 then
      begin
        Inc(DigitDelta);
        if DigitPlace <= FirstDigit then
          WriteDigit('0')
        else
          Dec(DigitPlace);
      end
      else
      begin
        if DigitDelta = 0 then
          AddDigit
        else
        begin  // DigitDelta > 0
          while DigitDelta > 0 do
          begin
            AddDigit;
            Dec(DigitDelta);
          end;
          AddDigit;
        end;
      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure PutExponent(EChar: ansiChar; Sign: ansiChar; Zeros: Integer; Exponent: Integer);
    var
      Exp: ansiString;
      WriteSign: ansiString;
    begin
      AppendChar(EChar);
      if (Sign = '+') and (Exponent >=0) then
        WriteSign := '+'
      else
        if Exponent < 0 then
          WriteSign := '-'
        else
          WriteSign := '';

      Exp := AlIntToStrA(Abs(Exponent));
      AppendString(WriteSign + StringOfChar(ansiChar('0'), Zeros - length(Exp)) + Exp);
    end;

  begin
    if (FloatValue.Negative) and (SectionIndex = 0) then
      AppendChar('-');

    if Scientific then
    begin
      DigitPlace := DecimalIndex;
      DigitDelta := 0;
    end
    else
    begin
      DigitDelta := FloatValue.Exponent - DecimalIndex;
      if DigitDelta >= 0 then
        DigitPlace := FloatValue.Exponent
      else
        DigitPlace := DecimalIndex;
    end;

    DigitsLimit := DigitsLength - 1;
    C := low(ansiString);
    DigitsC := 0;
    while C <= High(Section) do
    begin
      case Section[C] of
        '0', '#': begin
          PutFmtDigit;
          Inc(C);
        end;

        '.', ',': Inc(C);

        '"', '''': begin
          OldC := Section[C];
          Inc(C);
          while (C < High(Section)) and (Section[C] <> OldC) do
          begin
            AppendChar(Section[C]);
            Inc(C);
          end;
          Inc(C);
        end;

        'e', 'E': begin
          OldC := Section[C];
          Inc(C);
          if C <= High(Section) then
          begin
            Sign := Section[C];
            if (Sign <> '+') and (Sign <> '-') then
              AppendChar(OldC)
            else
            begin
              Zeros := 0;
              Inc(C);
              while (C <= High(Section)) and (Section[C] = '0') do
              begin
                Inc(C);
                if Zeros < 4 then Inc(Zeros);
              end;
              PutExponent(OldC, Sign, Zeros, FloatValue.Exponent - DecimalIndex);
            end;
          end
          else
            AppendChar(OldC);
        end;

        else
        begin
          AppendChar(Section[C]);
          Inc(C);
        end;
      end;
    end;
    if Result > 0 then
    begin
      AppendChar(#0);
      Dec(Result);
    end;
  end;

var
  Temp: Extended;

begin
  Result := 0;
  DecimalSep := AFormatSettings.DecimalSeparator;
  ThousandsSep := AFormatSettings.ThousandSeparator;

  if ValueType = fvCurrency then
    Temp := Currency(Value)
  else
    Temp := Extended(Value);

  if Extended(Temp) > 0 then
    AIndex := 0
  else
    if Extended(Temp) < 0 then
      AIndex := 1
    else
      AIndex := 2;

  SectionIndex := FindSection(AIndex);
  Section := ScanSection(SectionIndex);
  if Section = '' then
  begin
    SectionIndex := FindSection(0);
    Section := ScanSection(SectionIndex);
  end;

  if Scientific then
  begin
    Precision := DigitCount;
    Digits := 9999;
  end
  else begin
    Precision := CMaxExtPrecision;
    Digits := DigitCount - DecimalIndex;
  end;
  FloatToDecimal(FloatValue, Value, ValueType, Precision, Digits);

  if (FormatLength = 0) or (GetCharIndex(Format, 0) = ';') or
    ((FloatValue.Exponent >= 18) and (not Scientific)) or
    (FloatValue.Exponent = $7FF) or (FloatValue.Exponent = $800) then
//{$IFNDEF NEXTGEN}
//    if Unicode then
//      Result := FloatToText(PWideChar(Buf), Value, ValueType, ffGeneral, 15, 0, AFormatSettings)
//    else
      Result := ALInternalFloatToText(Buf, Value, ValueType, ffGeneral, 15, 0, AFormatSettings)
//{$ELSE NEXTGEN}
//    Result := FloatToText(PWideChar(Buf), Value, ValueType, ffGeneral, 15, 0, AFormatSettings)
//{$ENDIF !NEXTGEN}
  else
    ApplyFormat;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.sysUtils.FormatFloat is still the same and adjust the IFDEF'}
{$ENDIF}
function ALFormatFloatA(
           const Format: AnsiString;
           Value: Extended;
           const AFormatSettings: TALFormatSettingsA): AnsiString;
var
  Buffer: array[0..255] of AnsiChar;
begin
  if Length(Format) > Length(Buffer) - 32 then ALConvertError(@SFormatTooLong);
  SetString(
    Result,
    Buffer,
    ALInternalFloatToTextFmt(
      PByte(@Buffer),
      Value,
      fvExtended,
      PByte(Format),
      AFormatSettings
      {, False}));
end;

{***********************}
function  ALFormatFloatW(
            const Format: string;
            Value: Extended;
            const AFormatSettings: TALFormatSettingsW): string;
begin
  result := system.sysutils.FormatFloat(Format, Value, AFormatSettings);
end;

{***********************}
function  ALFormatFloatW(
            const Format: string;
            Value: Extended): string;
begin
  result := system.sysutils.FormatFloat(Format, Value);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.sysUtils.FormatCurr is still the same and adjust the IFDEF'}
{$ENDIF}
function ALFormatCurrA(
           const Format: AnsiString;
           Value: Currency;
           const AFormatSettings: TALFormatSettingsA): AnsiString;
var
  Buffer: array[0..255] of AnsiChar;
begin
  if Length(Format) > Length(Buffer) - 32 then ALConvertError(@SFormatTooLong);
  SetString(
    Result,
    Buffer,
    ALInternalFloatToTextFmt(
      PByte(@Buffer),
      Value,
      fvCurrency,
      PByte(Format),
      AFormatSettings
      {, False}));
end;

{**********************}
function  ALFormatCurrW(
            const Format: string;
            Value: Currency;
            const AFormatSettings: TALFormatSettingsW): string;
begin
  result := system.sysutils.FormatCurr(Format, Value, AFormatSettings);
end;

{**********************}
function  ALFormatCurrW(
            const Format: string;
            Value: Currency): string;
begin
  result := system.sysutils.FormatCurr(Format, Value);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.sysUtils.StrToFloat is still the same and adjust the IFDEF'}
{$ENDIF}
function  ALStrToFloat(const S: AnsiString; const AFormatSettings: TALFormatSettingsA): Extended;
begin
  if not ALTextToFloat(PAnsiChar(S), Result, fvExtended, AFormatSettings) then
    ALConvertErrorFmt(@SInvalidFloat, [S]);
end;

{*******************************************************************************************}
function  ALStrToFloat(const S: string; const AFormatSettings: TALFormatSettingsW): Extended;
begin
  result := system.sysutils.StrToFloat(S, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.sysUtils.StrToFloatDef is still the same and adjust the IFDEF'}
{$ENDIF}
function  ALStrToFloatDef(const S: AnsiString; const Default: Extended; const AFormatSettings: TALFormatSettingsA): Extended;
begin
  if not ALTextToFloat(PAnsiChar(S), Result, fvExtended, AFormatSettings) then
    Result := Default;
end;

{***********************************************************************************************************************}
function  ALStrToFloatDef(const S: string; const Default: Extended; const AFormatSettings: TALFormatSettingsW): Extended;
begin
  result := system.sysutils.StrToFloatDef(S, Default, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.sysUtils.TryStrToFloat is still the same and adjust the IFDEF'}
{$ENDIF}
function  ALTryStrToFloat(const S: AnsiString; out Value: Extended; const AFormatSettings: TALFormatSettingsA): Boolean;
begin
  Result := ALTextToFloat(PansiChar(S), Value, fvExtended, AFormatSettings);
end;

{******************************************************************************************************************}
function  ALTryStrToFloat(const S: String; out Value: Extended; const AFormatSettings: TALFormatSettingsW): Boolean;
begin
  Result := System.sysutils.TryStrToFloat(S, Value, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.sysUtils.TryStrToFloat is still the same and adjust the IFDEF'}
{$ENDIF}
function  ALTryStrToFloat(const S: AnsiString; out Value: Double; const AFormatSettings: TALFormatSettingsA): Boolean;
var
  LValue: Extended;
begin
  Result := ALTextToFloat(PAnsiChar(S), LValue, fvExtended, AFormatSettings);
  if Result then
    if not (LValue.SpecialType in [fsInf, fsNInf, fsNaN]) and
       ((LValue < Double.MinValue) or (LValue > Double.MaxValue)) then
      Result := False;
  if Result then
    Value := LValue;
end;

{****************************************************************************************************************}
function  ALTryStrToFloat(const S: String; out Value: Double; const AFormatSettings: TALFormatSettingsW): Boolean;
begin
  Result := System.sysutils.TryStrToFloat(S, Value, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.sysUtils.TryStrToFloat is still the same and adjust the IFDEF'}
{$ENDIF}
function  ALTryStrToFloat(const S: AnsiString; out Value: Single; const AFormatSettings: TALFormatSettingsA): Boolean;
var
  LValue: Extended;
begin
  Result := ALTextToFloat(PAnsiChar(S), LValue, fvExtended, AFormatSettings);
  if Result then
    if not (LValue.SpecialType in [fsInf, fsNInf, fsNaN]) and
       ((LValue < Single.MinValue) or (LValue > Single.MaxValue)) then
      Result := False;
  if Result then
    Value := LValue;
end;

{****************************************************************************************************************}
function  ALTryStrToFloat(const S: String; out Value: Single; const AFormatSettings: TALFormatSettingsW): Boolean;
begin
  Result := System.sysutils.TryStrToFloat(S, Value, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.sysUtils.StrToCurr is still the same and adjust the IFDEF'}
{$ENDIF}
function  ALStrToCurr(const S: AnsiString; const AFormatSettings: TALFormatSettingsA): Currency;
begin
  if not ALTextToFloat(PAnsiChar(S), Result, fvCurrency, AFormatSettings) then
    ALConvertErrorFmt(@SInvalidFloat, [S]);
end;

{******************************************************************************************}
function  ALStrToCurr(const S: string; const AFormatSettings: TALFormatSettingsW): Currency;
begin
  result := system.sysutils.StrToCurr(S, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.sysUtils.StrToCurrDef is still the same and adjust the IFDEF'}
{$ENDIF}
function  ALStrToCurrDef(const S: AnsiString; const Default: Currency; const AFormatSettings: TALFormatSettingsA): Currency;
begin
  if not ALTextToFloat(PAnsiChar(S), Result, fvCurrency, AFormatSettings) then
    Result := Default;
end;

{**********************************************************************************************************************}
function  ALStrToCurrDef(const S: string; const Default: Currency; const AFormatSettings: TALFormatSettingsW): Currency;
begin
  result := system.sysutils.StrToCurrDef(S, Default, AFormatSettings);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if system.sysUtils.TryStrToCurr is still the same and adjust the IFDEF'}
{$ENDIF}
function  ALTryStrToCurr(const S: AnsiString; out Value: Currency; const AFormatSettings: TALFormatSettingsA): Boolean;
begin
  Result := ALTextToFloat(PAnsiChar(S), Value, fvCurrency, AFormatSettings);
end;

{*****************************************************************************************************************}
function  ALTryStrToCurr(const S: string; out Value: Currency; const AFormatSettings: TALFormatSettingsW): Boolean;
begin
  result := system.sysutils.TryStrToCurr(S, Value, AFormatSettings);
end;

{**********************************************************************************}
function  ALPosA(const SubStr, Str: AnsiString; const Offset: Integer = 1): Integer;
begin
  Result := System.Pos(SubStr, Str, Offset);
end;

{******************************************************************************}
function  ALPosW(const SubStr, Str: String; const Offset: Integer = 1): Integer;
begin
  result := system.Pos(SubStr, Str, Offset);
end;

var
  ALPosIgnoreCaseLookupTable: packed array[AnsiChar] of AnsiChar; {Upcase Lookup Table}

{*********************************************}
procedure ALPosIgnoreCaseInitialiseLookupTable;
var Ch: AnsiChar;
begin
  for Ch := #0 to #255 do
    ALPosIgnoreCaseLookupTable[Ch] := AlUpCase(Ch);
end;

{*****************************************************************************************}
{from John O'Harrow (john@elmcrest.demon.co.uk) - original name: StringReplace_JOH_IA32_12}
function  ALPosIgnoreCaseA(const SubStr, S: Ansistring; const Offset: Integer = 1): Integer;
var
  I, LIterCnt, L, J: Integer;
  PSubStr, PS: PAnsiChar;
  C1, C2: AnsiChar;
begin
  {Exit if SubStr = ''}
  if SubStr = '' then exit(0);

  { Calculate the number of possible iterations. Not valid if Offset < 1. }
  LIterCnt := Length(S) - Offset - Length(SubStr) + 1;

  { Only continue if the number of iterations is positive or zero (there is space to check) }
  if (Offset > 0) and (LIterCnt >= 0) then
  begin
    L := Length(SubStr);
    PSubStr := PAnsiChar(SubStr);
    PS := PAnsiChar(S);
    Inc(PS, Offset - 1);

    for I := 0 to LIterCnt do
    begin
      J := 0;
      while (J >= 0) and (J < L) do
      begin
        C1 := (PS + I + J)^;
        C2 := (PSubStr + J)^;
        if (C1 = C2) or
           (((C1 >= AnsiChar('a')) and (C1 <= AnsiChar('z'))) and
            (C1 = AnsiChar(Byte(C2) + $20))) or
           (((C1 >= AnsiChar('A')) and (C1 <= AnsiChar('Z'))) and
            (C1 = AnsiChar(Byte(C2) - $20))) then
          Inc(J)
        else
          J := -1;
      end;
      if J >= L then
        Exit(I + Offset);
    end;
  end;

  Result := 0;
end;

{*****************************************************************************************}
{from John O'Harrow (john@elmcrest.demon.co.uk) - original name: StringReplace_JOH_IA32_12}
function  ALPosIgnoreCaseW(const SubStr, S: String; const Offset: Integer = 1): Integer;
var
  I, LIterCnt, L, J: Integer;
  PSubStr, PS: PChar;
  C1, C2: Char;
begin
  {Exit if SubStr = ''}
  if SubStr = '' then exit(0);

  { Calculate the number of possible iterations. Not valid if Offset < 1. }
  LIterCnt := Length(S) - Offset - Length(SubStr) + 1;

  { Only continue if the number of iterations is positive or zero (there is space to check) }
  if (Offset > 0) and (LIterCnt >= 0) then
  begin
    L := Length(SubStr);
    PSubStr := PChar(SubStr);
    PS := PChar(S);
    Inc(PS, Offset - 1);

    for I := 0 to LIterCnt do
    begin
      J := 0;
      while (J >= 0) and (J < L) do
      begin
        C1 := (PS + I + J)^;
        C2 := (PSubStr + J)^;
        if (C1 = C2) or
           (((C1 >= char('a')) and (C1 <= char('z'))) and
            (C1 = Char(word(C2) + $20))) or
           (((C1 >= char('A')) and (C1 <= char('Z'))) and
            (C1 = Char(word(C2) - $20))) then
          Inc(J)
        else
          J := -1;
      end;
      if J >= L then
        Exit(I + Offset);
    end;

  end;

  Result := 0;
end;

{*********************************************************}
function  ALCompareStrA(const S1, S2: AnsiString): Integer;
begin
  result := System.Ansistrings.CompareStr(S1, S2);
end;

{*****************************************************}
function  ALCompareStrW(const S1, S2: string): Integer;
begin
  result := system.sysutils.CompareStr(S1, S2);
end;

{******************************************************}
function  ALSameStrA(const S1, S2: AnsiString): Boolean;
begin
  result := System.Ansistrings.SameStr(S1, S2);
end;

{**************************************************}
function  ALSameStrW(const S1, S2: string): Boolean;
begin
  result := system.sysutils.SameStr(S1, S2);
end;

{**********************************************************}
function  ALCompareTextA(const S1, S2: AnsiString): Integer;
begin
  result := System.Ansistrings.CompareText(S1, S2);
end;

{******************************************************}
function  ALCompareTextW(const S1, S2: string): Integer;
begin
  result := system.sysutils.CompareText(S1, S2);
end;

{*******************************************************}
function  ALSameTextA(const S1, S2: AnsiString): Boolean;
begin
  result := System.Ansistrings.SameText(S1, S2);
end;

{***************************************************}
function  ALSameTextW(const S1, S2: string): Boolean;
begin
  result := system.sysutils.SameText(S1, S2);
end;

{*******************************************************************************************}
function  ALMatchTextA(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
begin
  result := System.Ansistrings.MatchText(AText, AValues);
end;

{***********************************************************************************}
function  ALMatchTextW(const AText: String; const AValues: array of String): Boolean;
begin
  result := System.StrUtils.MatchText(AText, AValues);
end;

{******************************************************************************************}
function  ALMatchStrA(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
begin
  result := System.Ansistrings.MatchStr(AText, AValues);
end;

{**********************************************************************************}
function  ALMatchStrW(const AText: String; const AValues: array of String): Boolean;
begin
  result := System.StrUtils.MatchStr(AText, AValues);
end;

{************************************************}
function  ALTrim(const S: AnsiString): AnsiString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  if (L > 0) and (S[I] > ' ') and (S[L] > ' ') then Exit(S);
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Exit('');
  while S[L] <= ' ' do Dec(L);
  Result := ALCopyStr(S, I, L - I + 1);
end;

{****************************************}
function  ALTrim(const S: string): string;
begin
  result := system.sysutils.Trim(S);
end;

{****************************************************}
function  ALTrimLeft(const S: AnsiString): AnsiString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I = 1 then Exit(S);
  Result := ALCopyStr(S, I, Maxint);
end;

{********************************************}
function  ALTrimLeft(const S: string): string;
begin
  result := system.sysutils.TrimLeft(S);
end;

{*****************************************************}
function  ALTrimRight(const S: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := Length(S);
  if (I > 0) and (S[I] > ' ') then Exit(S);
  while (I > 0) and (S[I] <= ' ') do Dec(I);
  Result := ALCopyStr(S, 1, I);
end;

{*********************************************}
function  ALTrimRight(const S: string): string;
begin
  result := system.sysutils.TrimRight(S);
end;

{*************************************************************************}
function  ALPadLeft(const S: AnsiString; Const Width: Integer): AnsiString;
begin
  result := ALFormatA('%'+ALIntToStrA(Width)+'s', [S]);
end;

{*****************************************************************}
function  ALPadLeft(const S: String; Const Width: Integer): String;
begin
  result := ALFormatW('%'+ALIntToStrW(Width)+'s', [S]);
end;

{**************************************************************************}
function  ALPadRight(const S: AnsiString; Const Width: Integer): AnsiString;
begin
  result := ALFormatA('%-'+ALIntToStrA(Width)+'s', [S]);
end;

{******************************************************************}
function  ALPadRight(const S: String; Const Width: Integer): String;
begin
  result := ALFormatW('%-'+ALIntToStrW(Width)+'s', [S]);
end;

{***********************************************************************************}
function  ALQuotedStr(const S: AnsiString; const Quote: AnsiChar = ''''): AnsiString;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if Result[I] = Quote then Insert(Quote, Result, I);
  Result := Quote + Result + Quote;
end;

{***********************************************************************}
function  ALQuotedStr(const S: String; const Quote: Char = ''''): String;
var
  I: Integer;
begin
  Result := S;
  for I := Result.Length - 1 downto 0 do
    if Result.Chars[I] = Quote then Result := Result.Insert(I, Quote);
  Result := Quote + Result + Quote;
end;

{***************************************************************************}
function ALExtractQuotedStr(var Src: PAnsiChar; Quote: AnsiChar): AnsiString;
var
  P, Dest: PAnsiChar;
  DropCount: Integer;
  EndSuffix: Integer;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then Exit;
  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := System.Ansistrings.StrScan(Src, Quote);
  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then Break;
    Inc(Src);
    Inc(DropCount);
    Src := System.Ansistrings.StrScan(Src, Quote);
  end;
  EndSuffix := Ord(Src = nil); // Has an ending quoatation mark?
  if Src = nil then Src := System.Ansistrings.StrEnd(P);
  if ((Src - P) <= 1 - EndSuffix) or ((Src - P - DropCount) = EndSuffix) then Exit;
  if DropCount = 1 then
    SetString(Result, P, Src - P - 1 + EndSuffix)
  else
  begin
    SetLength(Result, Src - P - DropCount + EndSuffix);
    Dest := PAnsiChar(Result);
    Src := System.Ansistrings.StrScan(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then Break;
      ALMove(P^, Dest^, Src - P);
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := System.Ansistrings.StrScan(Src, Quote);
    end;
    if Src = nil then Src := System.Ansistrings.StrEnd(P);
    ALMove(P^, Dest^, Src - P - 1 + EndSuffix);
  end;
end;

{***************************************************************}
function ALExtractQuotedStr(var Src: PChar; Quote: Char): String;
begin
  result := AnsiExtractQuotedStr(PWideChar(Src), Quote);
end;

{************************************************************************}
function ALDequotedStr(const S: AnsiString; AQuote: AnsiChar): AnsiString;
var
  LText: PAnsiChar;
begin
  LText := PAnsiChar(S);
  Result := ALExtractQuotedStr(LText, AQuote);
  if ((Result = '') or (LText^ = #0)) and
     (Length(S) > 0) and ((S[1] <> AQuote) or (S[Length(S)] <> AQuote)) then
    Result := S;
end;

{*************************************************************}
function  ALDequotedStr(const S: string; AQuote: Char): string;
begin
  result := system.sysutils.AnsiDequotedStr(S, AQuote);
end;

{******************************************************************}
function  ALExtractFilePath(const FileName: AnsiString): AnsiString;
begin
  Result := System.AnsiStrings.ExtractFilePath(FileName);
end;

{**********************************************************}
function  ALExtractFilePath(const FileName: String): String;
begin
  Result := ExtractFilePath(FileName);
end;

{*****************************************************************}
function  ALExtractFileDir(const FileName: AnsiString): AnsiString;
begin
  Result := ExtractFileDir(FileName);
end;

{*********************************************************}
function  ALExtractFileDir(const FileName: String): String;
begin
  Result := ExtractFileDir(FileName);
end;

{*******************************************************************}
function  ALExtractFileDrive(const FileName: AnsiString): AnsiString;
begin
  Result := ExtractFileDrive(FileName);
end;

{***********************************************************}
function  ALExtractFileDrive(const FileName: String): String;
begin
  Result := ExtractFileDrive(FileName);
end;

{******************************************************************************************************}
function  ALExtractFileName(const FileName: AnsiString; const RemoveFileExt: Boolean=false): AnsiString;
begin
  Result := ExtractFileName(FileName);
  if RemoveFileExt then result := ALCopyStr(result, 1, length(Result) - length(ALExtractFileExt(FileName)));
end;

{**********************************************************************************************}
function  ALExtractFileName(const FileName: String; const RemoveFileExt: Boolean=false): String;
begin
  Result := ExtractFileName(FileName);
  if RemoveFileExt then result := ALCopyStr(result, 1, length(Result) - length(ALExtractFileExt(FileName)));
end;

{*****************************************************************}
function  ALExtractFileExt(const FileName: AnsiString): AnsiString;
begin
  Result := System.AnsiStrings.ExtractFileExt(FileName);
end;

{*********************************************************}
function  ALExtractFileExt(const FileName: String): String;
begin
  Result := ExtractFileExt(FileName);
end;

{*******************************************************************}
function  ALLastDelimiterA(const Delimiters, S: AnsiString): Integer;
var
  P: PAnsiChar;
begin
  Result := Length(S);
  P := PAnsiChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (System.Ansistrings.StrScan(P, S[Result]) <> nil) then
      Exit;
    Dec(Result);
  end;
end;

{***************************************************************}
function  ALLastDelimiterW(const Delimiters, S: string): Integer;
begin
  result := system.sysutils.LastDelimiter(Delimiters, S);
end;

{******************************************************************************************************************************************************}
function ALIsPathDelimiter(const S: AnsiString; Index: Integer; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): Boolean;
begin
  Result := (Index >= low(s)) and (Index <= high(S)) and (S[Index] = PathDelimiter);
end;

{**********************************************************************************************************************************************}
function ALIsPathDelimiter(const S: String; Index: Integer; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): Boolean;
begin
  Result := (Index >= low(s)) and (Index <= high(S)) and (S[low(s) + Index - 1] = PathDelimiter);
end;

{*******************************************************************************************************************************************************}
function ALIncludeTrailingPathDelimiterA(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString;
begin
  Result := S;
  if not ALIsPathDelimiter(Result, Length(Result), PathDelimiter) then
    Result := Result + PathDelimiter;
end;

{*******************************************************************************************************************************************}
function ALIncludeTrailingPathDelimiterW(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String;
begin
  Result := S;
  if not ALIsPathDelimiter(Result, Length(Result), PathDelimiter) then
    Result := Result + PathDelimiter;
end;

{*******************************************************************************************************************************************************}
function ALExcludeTrailingPathDelimiterA(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString;
begin
  Result := S;
  while ALIsPathDelimiter(Result, Length(Result), PathDelimiter) do
    SetLength(Result, Length(Result)-1);
end;

{*******************************************************************************************************************************************}
function ALExcludeTrailingPathDelimiterW(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String;
begin
  Result := S;
  While ALIsPathDelimiter(Result, Length(Result), PathDelimiter) do
    SetLength(Result, Length(Result)-1);
end;

{******************************************************************************************************************************************************}
function ALIncludeLeadingPathDelimiterA(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString;
begin
  if not ALIsPathDelimiter(s, 1, PathDelimiter) then Result := PathDelimiter + s
  else Result := S;
end;

{******************************************************************************************************************************************}
function ALIncludeLeadingPathDelimiterW(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String;
begin
  if not ALIsPathDelimiter(s, 1, PathDelimiter) then Result := PathDelimiter + s
  else Result := S;
end;

{******************************************************************************************************************************************************}
function ALExcludeLeadingPathDelimiterA(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString;
begin
  if ALIsPathDelimiter(S, 1, PathDelimiter) then Result := ALcopyStr(S,2,maxint)
  else result := S;
end;

{******************************************************************************************************************************************}
function ALExcludeLeadingPathDelimiterW(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String;
begin
  if ALIsPathDelimiter(S, 1, PathDelimiter) then Result := ALcopyStr(S,2,maxint)
  else result := S;
end;

{***********************************************************************************************}
// warning ALStrMove inverse the order of the original StrMove (to keep the same order as ALMove)
procedure ALStrMove(const Source: PAnsiChar; var Dest: PAnsiChar; Count: NativeInt);
begin
  ALMove(Source^, Dest^, Count);
end;

{***********************************************************************************************}
// warning ALStrMove inverse the order of the original StrMove (to keep the same order as ALMove)
procedure ALStrMove(const Source: PChar; var Dest: PChar; Count: NativeInt);
begin
  ALMove(Source^, Dest^, Count * sizeOf(Char));
end;

{****************************************************************************************}
function ALCopyStr(const aSourceString: AnsiString; aStart, aLength: Integer): AnsiString;
var LSourceStringLn: Integer;
begin
  LSourceStringLn := Length(aSourceString);
  If (aStart < 1) then aStart := 1;

  if (LSourceStringLn=0) or
     (aLength < 1) or
     (aStart > LSourceStringLn) then Begin
    Result := '';
    Exit;
  end;

  if aLength > LSourceStringLn - (aStart - 1) then aLength := LSourceStringLn - (aStart-1);

  SetLength(Result,aLength); //  To guarantee that the string is unique, call the SetLength, SetString, or UniqueString procedures
  ALMove(Pbyte(aSourceString)[aStart-1], pointer(Result)^, aLength); // pointer(Result)^ to not jump inside uniqueString (aDestString is already unique thanks to previous SetLength))
end;

{********************************************************************************}
function ALCopyStr(const aSourceString: String; aStart, aLength: Integer): String;
var LSourceStringLn: Integer;
begin
  LSourceStringLn := Length(aSourceString);
  If (aStart < 1) then aStart := 1;

  if (LSourceStringLn=0) or
     (aLength < 1) or
     (aStart > LSourceStringLn) then Begin
    Result := '';
    Exit;
  end;

  if aLength > LSourceStringLn - (aStart - 1) then aLength := LSourceStringLn - (aStart-1);

  SetLength(Result,aLength); //  To guarantee that the string is unique, call the SetLength, SetString, or UniqueString procedures
  ALMove(PChar(aSourceString)[aStart-1], pointer(Result)^, aLength*SizeOf(Char)); // pointer(Result)^ to not jump inside uniqueString (aDestString is already unique thanks to previous SetLength))
end;

{**********************************************************************************************************}
procedure ALCopyStr(const aSourceString: AnsiString; var aDestString: ansiString; aStart, aLength: Integer);
var LSourceStringLn: Integer;
begin
  LSourceStringLn := Length(aSourceString);
  If (aStart < 1) then aStart := 1;

  if (LSourceStringLn=0) or
     (aLength < 1) or
     (aStart > LSourceStringLn) then Begin
    aDestString := '';
    Exit;
  end;

  if aLength > LSourceStringLn - (aStart - 1) then aLength := LSourceStringLn - (aStart-1);

  SetLength(aDestString,aLength); //  To guarantee that the string is unique, call the SetLength, SetString, or UniqueString procedures
  ALMove(Pbyte(aSourceString)[aStart-1], pointer(aDestString)^, aLength);  // pointer(aDestString)^ to not jump inside uniqueString (aDestString is already unique thanks to previous SetLength))
end;

{**************************************************************************************************}
procedure ALCopyStr(const aSourceString: String; var aDestString: String; aStart, aLength: Integer);
var LSourceStringLn: Integer;
begin
  LSourceStringLn := Length(aSourceString);
  If (aStart < 1) then aStart := 1;

  if (LSourceStringLn=0) or
     (aLength < 1) or
     (aStart > LSourceStringLn) then Begin
    aDestString := '';
    Exit;
  end;

  if aLength > LSourceStringLn - (aStart - 1) then aLength := LSourceStringLn - (aStart-1);

  SetLength(aDestString,aLength); //  To guarantee that the string is unique, call the SetLength, SetString, or UniqueString procedures
  ALMove(PChar(aSourceString)[aStart-1], pointer(aDestString)^, aLength*SizeOf(Char));  // pointer(aDestString)^ to not jump inside uniqueString (aDestString is already unique thanks to previous SetLength))
end;

{******************}
function  ALCopyStr(
            const aSourceString: AnsiString;
            const aStartStr: AnsiString;
            const aEndStr: AnsiString;
            const aOffset: integer = 1;
            const aRaiseExceptionIfNotFound: Boolean = True): AnsiString;
var P1, P2: integer;
begin
  P1 := ALPosIgnoreCaseA(aStartStr, aSourceString, aOffset);
  if P1 <= 0 then begin
    if aRaiseExceptionIfNotFound then Raise EALException.Createfmt('Can not find%s the text %s in %s', [ALIfThenA(aOffset > 1, AlFormatA(' after offset %d', [aOffset])), aStartStr, aSourceString])
    else begin
      result := '';
      exit;
    end;
  end;
  Inc(P1, Length(aStartStr));
  P2 := ALPosIgnoreCaseA(aEndStr, aSourceString, P1);
  if P2 <= 0 then begin
    if aRaiseExceptionIfNotFound then Raise EALException.Createfmt('Can not find%s the text %s in %s', [ALIfThenA(aOffset > 1, AlFormatA(' after offset %d', [aOffset])), aEndStr, aSourceString])
    else begin
      result := '';
      exit;
    end;
  end;
  result := ALCopyStr(aSourceString, P1, P2 - P1);
end;

{******************}
function  ALCopyStr(
            const aSourceString: String;
            const aStartStr: String;
            const aEndStr: String;
            const aOffset: integer = 1;
            const aRaiseExceptionIfNotFound: Boolean = True): String;
var P1, P2: integer;
begin
  P1 := ALPosIgnoreCasew(aStartStr, aSourceString, aOffset);
  if P1 <= 0 then begin
    if aRaiseExceptionIfNotFound then Raise EALException.Createfmt('Can not find%s the text %s in %s', [ALIfThenA(aOffset > 1, AlFormatA(' after offset %d', [aOffset])), aStartStr, aSourceString])
    else begin
      result := '';
      exit;
    end;
  end;
  Inc(P1, Length(aStartStr));
  P2 := ALPosIgnoreCaseW(aEndStr, aSourceString, P1);
  if P2 <= 0 then begin
    if aRaiseExceptionIfNotFound then Raise EALException.Createfmt('Can not find%s the text %s in %s', [ALIfThenA(aOffset > 1, AlFormatA(' after offset %d', [aOffset])), aEndStr, aSourceString])
    else begin
      result := '';
      exit;
    end;
  end;
  result := ALCopyStr(aSourceString, P1, P2 - P1);
end;

{*************************************************************************************************************}
function  ALStringReplaceA(const Source, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;
begin
  result := System.AnsiStrings.StringReplace(Source, OldPattern, NewPattern, Flags);
end;

{*****************************************************************************************************}
function  ALStringReplaceW(const Source, OldPattern, NewPattern: string; Flags: TReplaceFlags): string;
begin
  result := system.sysutils.StringReplace(Source, OldPattern, NewPattern, Flags);
end;

{********************************************************************************************}
function  ALRandomStrA(const aLength: Longint; const aCharset: Array of ansiChar): AnsiString;
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
function ALRandomStrA(const aLength: Longint): AnsiString;
begin
  Result := ALRandomStrA(aLength,['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']);
end;

{************************************************************************************}
function  ALRandomStrW(const aLength: Longint; const aCharset: Array of Char): String;
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
function ALRandomStrW(const aLength: Longint): String;
begin
  Result := ALRandomStrW(aLength,['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']);
end;

{*********************************************************}
function ALNEVExtractName(const S: AnsiString): AnsiString;
var P: Integer;
begin
  Result := S;
  P := alPosA('=', Result);
  if P <> 0 then SetLength(Result, P-1)
  else SetLength(Result, 0);
end;

{**********************************************************}
function ALNEVExtractValue(const s: AnsiString): AnsiString;
begin
  Result := AlCopyStr(s, Length(ALNEVExtractName(s)) + 2, MaxInt)
end;

{**************************************************************}
function  ALGetBytesFromStream(const aStream : TStream): Tbytes;
var l: Integer;
begin
   l:=aStream.Size-aStream.Position;
   SetLength(result, l);
   if L > 0 then
     aStream.ReadBuffer(result[0], l);
end;

{********************************************************************************************************}
function ALGetBytesFromFile(const filename: ansiString; const ShareMode: Word = fmShareDenyWrite): Tbytes;
Var LFileStream: TfileStream;
begin
  LFileStream := TFileStream.Create(string(filename),fmOpenRead or ShareMode);
  try
    Result := ALGetBytesFromStream(LFileStream);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{*****************************************************************************************************}
function  ALGetBytesFromFile(const filename: String; const ShareMode: Word = fmShareDenyWrite): Tbytes;
Var LFileStream: TfileStream;
begin
  LFileStream := TFileStream.Create(filename,fmOpenRead or ShareMode);
  try
    Result := ALGetBytesFromStream(LFileStream);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{*********************************************************************************************}
function  ALGetStringFromBuffer(const buf : TBytes; const ADefaultEncoding: TEncoding): String;
var encoding : TEncoding;
    n : Integer;
begin
  encoding:=nil;
  n:=TEncoding.GetBufferEncoding(buf, encoding, ADefaultEncoding);
  Result:=encoding.GetString(buf, n, Length(buf)-n);
end;

{**************************************************************************************************}
function  ALGetStringFromStream(const aStream : TStream; const ADefaultEncoding: TEncoding): String;
var buf: Tbytes;
begin
   Buf := ALGetBytesFromStream(aStream);
   Result:=ALGetStringFromBuffer(buf, ADefaultEncoding);
end;

{*************************************************************************************************************}
function ALGetStringFromFile(const filename: AnsiString; const ShareMode: Word = fmShareDenyWrite): AnsiString;
begin
  result := ALGetStringFromFile(String(filename), ShareMode);
end;

{*********************************************************************************************************}
function ALGetStringFromFile(const filename: String; const ShareMode: Word = fmShareDenyWrite): AnsiString;
Var LFileStream: TfileStream;
begin
  LFileStream := TFileStream.Create(filename,fmOpenRead or ShareMode);
  try

    If LFileStream.size > 0 then begin
      SetLength(Result, LFileStream.size);
      LFileStream.ReadBuffer(pointer(Result)^,LFileStream.Size)
    end
    else Result := '';

  finally
    LFileStream.Free;
  end;
end;

{*****************************************************************************************************************************************}
function  ALGetStringFromFile(const filename: String; const ADefaultEncoding: TEncoding; const ShareMode: Word = fmShareDenyWrite): String;
Var LFileStream: TfileStream;
begin
  LFileStream := TFileStream.Create(filename,fmOpenRead or ShareMode);
  try
    Result := ALGetStringFromStream(LFileStream, ADefaultEncoding);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{***************************************************************************************************************************}
function ALGetStringFromFileWithoutUTF8BOM(const filename: AnsiString; const ShareMode: Word = fmShareDenyWrite): AnsiString;
begin
  result := ALGetStringFromFileWithoutUTF8BOM(String(filename), ShareMode);
end;

{***********************************************************************************************************************}
function ALGetStringFromFileWithoutUTF8BOM(const filename: String; const ShareMode: Word = fmShareDenyWrite): AnsiString;
Var LFileStream: TfileStream;
    LBOMStr: AnsiString;
    LSize: Integer;
begin
  LFileStream := TFileStream.Create(filename,fmOpenRead or ShareMode);
  try

    LSize := LFileStream.size;
    If LSize > 0 then begin

      If LSize >= 3 then begin
        SetLength(LBOMStr,3);
        LFileStream.ReadBuffer(pointer(LBOMStr)^,3);
        If AlUTF8DetectBOM(PAnsiChar(LBOMStr), 3) then LSize := LSize - 3
        else LFileStream.Position := 0;
      end;

      If LSize > 0 then begin
        SetLength(Result, LSize);
        LFileStream.ReadBuffer(pointer(Result)^,LSize)
      end
      else Result := '';

    end
    else Result := '';

  finally
    LFileStream.Free;
  end;
end;

{********************************************************************************}
procedure ALAppendStringToFile(const Str: AnsiString; const FileName: AnsiString);
begin
  ALAppendStringToFile(Str, String(FileName));
end;

{****************************************************************************}
procedure ALAppendStringToFile(const Str: AnsiString; const FileName: String);
var LFileStream: TFileStream;
begin
  if FileExists(FileName) then LFileStream := TFileStream.Create(FileName, fmOpenReadWrite)
  else                         LFileStream := TFileStream.Create(FileName, fmCreate);
  try
    LFileStream.Position := LFileStream.Size;
    LFileStream.WriteBuffer(Pointer(Str)^, Length(Str));
  finally
    LFileStream.Free;
  end;
end;

{******************************************************************************}
procedure ALSaveStringtoFile(const Str: AnsiString; const filename: AnsiString);
begin
  ALSaveStringtoFile(Str, String(filename));
end;

{**************************************************************************}
procedure ALSaveStringtoFile(const Str: AnsiString; const filename: String);
Var LFileStream: TFileStream;
begin
  TDirectory.CreateDirectory(ExtractFilePath(filename));
  LFileStream := TFileStream.Create(filename, fmCreate);
  try
    LFileStream.WriteBuffer(Pointer(Str)^, Length(Str));
  finally
    LFileStream.Free;
  end;
end;

{*****************************************************************************************************************************}
procedure ALSaveStringtoFile(const Str: String; const filename: String; AEncoding: TEncoding; const WriteBOM: boolean = False);
var LfileStream: TfileStream;
    Buffer, Preamble: TBytes;
begin
  TDirectory.CreateDirectory(ExtractFilePath(filename));
  LfileStream := TfileStream.Create(filename,fmCreate);
  Try

    Buffer := aEncoding.GetBytes(Str);

    if WriteBOM then begin
      Preamble := aEncoding.GetPreamble;
      if Length(Preamble) > 0 then
        LfileStream.WriteBuffer(Preamble, Length(Preamble));
    end;

    LfileStream.WriteBuffer(Buffer, Length(Buffer));

  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{**********************}
{$IF defined(MSWINDOWS)}
// S is a AnsiString that contains UTF-8 encoded characters
// The result of the function is the corresponding UTF-8 encoded string
// "normalized":
// ie: l''été sur l''europe => l-ete-sur-l-europe
Function  ALNormalize(
            const S: AnsiString;
            const WordSeparator: ansiChar;
            const SymbolsToIgnore: array of AnsiChar): AnsiString;
var LWideSymbolsToIgnore: Array of WideChar;
    I: integer;
begin
  setlength(LWideSymbolsToIgnore, length(SymbolsToIgnore));
  for I := Low(SymbolsToIgnore) to High(SymbolsToIgnore) do
    LWideSymbolsToIgnore[i] := WideChar(SymbolsToIgnore[i]);
  Result := AnsiString(ALNormalize(WideString(S), WideChar(WordSeparator)));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
// S is a AnsiString that contains UTF-8 encoded characters
// The result of the function is the corresponding UTF-8 encoded string
// "normalized":
// ie: l''été sur l''europe => l-ete-sur-l-europe
Function ALNormalize(
           const S: AnsiString;
           const WordSeparator: ansiChar = '-'): AnsiString;
begin
  Result := AnsiString(ALNormalize(WideString(S), WideChar(WordSeparator), []));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
// Normalize a Widestring
// ie: l''été sur l''europe => l-ete-sur-l-europe
Function  ALNormalize(
            const S: Widestring;
            const WordSeparator: WideChar;
            const SymbolsToIgnore: array of WideChar): Widestring;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  {source: http://issues.apache.org/jira/browse/LUCENE-1343}
  Procedure _foldNonDiacriticChar(Var aStr: WideString);
  Var I, J : integer;
  Begin
    for I := 1 to length(aStr) do begin
      j := ord(aStr[i]);
      case j of
        $0181: aStr[i] := widechar($0042);    // Ɓ -> B | LATIN CAPITAL LETTER B WITH HOOK -> LATIN CAPITAL LETTER B
        $0182: aStr[i] := widechar($0042);    // Ƃ -> B | LATIN CAPITAL LETTER B WITH TOPBAR -> LATIN CAPITAL LETTER B
        $0187: aStr[i] := widechar($0043);    // Ƈ -> C | LATIN CAPITAL LETTER C WITH HOOK -> LATIN CAPITAL LETTER C
        $0110: aStr[i] := widechar($0044);    // Đ -> D | LATIN CAPITAL LETTER D WITH STROKE -> LATIN CAPITAL LETTER D
        $018A: aStr[i] := widechar($0044);    // Ɗ -> D | LATIN CAPITAL LETTER D WITH HOOK -> LATIN CAPITAL LETTER D
        $018B: aStr[i] := widechar($0044);    // Ƌ -> D | LATIN CAPITAL LETTER D WITH TOPBAR -> LATIN CAPITAL LETTER D
        $0191: aStr[i] := widechar($0046);    // Ƒ -> F | LATIN CAPITAL LETTER F WITH HOOK -> LATIN CAPITAL LETTER F
        $0193: aStr[i] := widechar($0047);    // Ɠ -> G | LATIN CAPITAL LETTER G WITH HOOK -> LATIN CAPITAL LETTER G
        $01E4: aStr[i] := widechar($0047);    // Ǥ -> G | LATIN CAPITAL LETTER G WITH STROKE -> LATIN CAPITAL LETTER G
        $0126: aStr[i] := widechar($0048);    // Ħ -> H | LATIN CAPITAL LETTER H WITH STROKE -> LATIN CAPITAL LETTER H
        $0197: aStr[i] := widechar($0049);    // Ɨ -> I | LATIN CAPITAL LETTER I WITH STROKE -> LATIN CAPITAL LETTER I
        $0198: aStr[i] := widechar($004B);    // Ƙ -> K | LATIN CAPITAL LETTER K WITH HOOK -> LATIN CAPITAL LETTER K
        $0141: aStr[i] := widechar($004C);    // Ł -> L | LATIN CAPITAL LETTER L WITH STROKE -> LATIN CAPITAL LETTER L
        $019D: aStr[i] := widechar($004E);    // Ɲ -> N | LATIN CAPITAL LETTER N WITH LEFT HOOK -> LATIN CAPITAL LETTER N
        $0220: aStr[i] := widechar($004E);    // Ƞ -> N | LATIN CAPITAL LETTER N WITH LONG RIGHT LEG -> LATIN CAPITAL LETTER N
        $00D8: aStr[i] := widechar($004F);    // Ø -> O | LATIN CAPITAL LETTER O WITH STROKE -> LATIN CAPITAL LETTER O
        $019F: aStr[i] := widechar($004F);    // Ɵ -> O | LATIN CAPITAL LETTER O WITH MIDDLE TILDE -> LATIN CAPITAL LETTER O
        $01FE: aStr[i] := widechar($004F);    // Ǿ -> O | LATIN CAPITAL LETTER O WITH STROKE AND ACUTE -> LATIN CAPITAL LETTER O
        $01A4: aStr[i] := widechar($0050);    // Ƥ -> P | LATIN CAPITAL LETTER P WITH HOOK -> LATIN CAPITAL LETTER P
        $0166: aStr[i] := widechar($0054);    // Ŧ -> T | LATIN CAPITAL LETTER T WITH STROKE -> LATIN CAPITAL LETTER T
        $01AC: aStr[i] := widechar($0054);    // Ƭ -> T | LATIN CAPITAL LETTER T WITH HOOK -> LATIN CAPITAL LETTER T
        $01AE: aStr[i] := widechar($0054);    // Ʈ -> T | LATIN CAPITAL LETTER T WITH RETROFLEX HOOK -> LATIN CAPITAL LETTER T
        $01B2: aStr[i] := widechar($0056);    // Ʋ -> V | LATIN CAPITAL LETTER V WITH HOOK -> LATIN CAPITAL LETTER V
        $01B3: aStr[i] := widechar($0059);    // Ƴ -> Y | LATIN CAPITAL LETTER Y WITH HOOK -> LATIN CAPITAL LETTER Y
        $01B5: aStr[i] := widechar($005A);    // Ƶ -> Z | LATIN CAPITAL LETTER Z WITH STROKE -> LATIN CAPITAL LETTER Z
        $0224: aStr[i] := widechar($005A);    // Ȥ -> Z | LATIN CAPITAL LETTER Z WITH HOOK -> LATIN CAPITAL LETTER Z
        $0180: aStr[i] := widechar($0062);    // ƀ -> b | LATIN SMALL LETTER B WITH STROKE -> LATIN SMALL LETTER B
        $0183: aStr[i] := widechar($0062);    // ƃ -> b | LATIN SMALL LETTER B WITH TOPBAR -> LATIN SMALL LETTER B
        $0253: aStr[i] := widechar($0062);    // ɓ -> b | LATIN SMALL LETTER B WITH HOOK -> LATIN SMALL LETTER B
        $0188: aStr[i] := widechar($0063);    // ƈ -> c | LATIN SMALL LETTER C WITH HOOK -> LATIN SMALL LETTER C
        $0255: aStr[i] := widechar($0063);    // ɕ -> c | LATIN SMALL LETTER C WITH CURL -> LATIN SMALL LETTER C
        $0111: aStr[i] := widechar($0064);    // đ -> d | LATIN SMALL LETTER D WITH STROKE -> LATIN SMALL LETTER D
        $018C: aStr[i] := widechar($0064);    // ƌ -> d | LATIN SMALL LETTER D WITH TOPBAR -> LATIN SMALL LETTER D
        $0221: aStr[i] := widechar($0064);    // ȡ -> d | LATIN SMALL LETTER D WITH CURL -> LATIN SMALL LETTER D
        $0256: aStr[i] := widechar($0064);    // ɖ -> d | LATIN SMALL LETTER D WITH TAIL -> LATIN SMALL LETTER D
        $0257: aStr[i] := widechar($0064);    // ɗ -> d | LATIN SMALL LETTER D WITH HOOK -> LATIN SMALL LETTER D
        $0192: aStr[i] := widechar($0066);    // ƒ -> f | LATIN SMALL LETTER F WITH HOOK -> LATIN SMALL LETTER F
        $01E5: aStr[i] := widechar($0067);    // ǥ -> g | LATIN SMALL LETTER G WITH STROKE -> LATIN SMALL LETTER G
        $0260: aStr[i] := widechar($0067);    // ɠ -> g | LATIN SMALL LETTER G WITH HOOK -> LATIN SMALL LETTER G
        $0127: aStr[i] := widechar($0068);    // ħ -> h | LATIN SMALL LETTER H WITH STROKE -> LATIN SMALL LETTER H
        $0266: aStr[i] := widechar($0068);    // ɦ -> h | LATIN SMALL LETTER H WITH HOOK -> LATIN SMALL LETTER H
        $0268: aStr[i] := widechar($0069);    // ɨ -> i | LATIN SMALL LETTER I WITH STROKE -> LATIN SMALL LETTER I
        $029D: aStr[i] := widechar($006A);    // ʝ -> j | LATIN SMALL LETTER J WITH CROSSED-TAIL -> LATIN SMALL LETTER J
        $0199: aStr[i] := widechar($006B);    // ƙ -> k | LATIN SMALL LETTER K WITH HOOK -> LATIN SMALL LETTER K
        $0142: aStr[i] := widechar($006C);    // ł -> l | LATIN SMALL LETTER L WITH STROKE -> LATIN SMALL LETTER L
        $019A: aStr[i] := widechar($006C);    // ƚ -> l | LATIN SMALL LETTER L WITH BAR -> LATIN SMALL LETTER L
        $0234: aStr[i] := widechar($006C);    // ȴ -> l | LATIN SMALL LETTER L WITH CURL -> LATIN SMALL LETTER L
        $026B: aStr[i] := widechar($006C);    // ɫ -> l | LATIN SMALL LETTER L WITH MIDDLE TILDE -> LATIN SMALL LETTER L
        $026C: aStr[i] := widechar($006C);    // ɬ -> l | LATIN SMALL LETTER L WITH BELT -> LATIN SMALL LETTER L
        $026D: aStr[i] := widechar($006C);    // ɭ -> l | LATIN SMALL LETTER L WITH RETROFLEX HOOK -> LATIN SMALL LETTER L
        $0271: aStr[i] := widechar($006D);    // ɱ -> m | LATIN SMALL LETTER M WITH HOOK -> LATIN SMALL LETTER M
        $019E: aStr[i] := widechar($006E);    // ƞ -> n | LATIN SMALL LETTER N WITH LONG RIGHT LEG -> LATIN SMALL LETTER N
        $0235: aStr[i] := widechar($006E);    // ȵ -> n | LATIN SMALL LETTER N WITH CURL -> LATIN SMALL LETTER N
        $0272: aStr[i] := widechar($006E);    // ɲ -> n | LATIN SMALL LETTER N WITH LEFT HOOK -> LATIN SMALL LETTER N
        $0273: aStr[i] := widechar($006E);    // ɳ -> n | LATIN SMALL LETTER N WITH RETROFLEX HOOK -> LATIN SMALL LETTER N
        $00F8: aStr[i] := widechar($006F);    // ø -> o | LATIN SMALL LETTER O WITH STROKE -> LATIN SMALL LETTER O
        $01FF: aStr[i] := widechar($006F);    // ǿ -> o | LATIN SMALL LETTER O WITH STROKE AND ACUTE -> LATIN SMALL LETTER O
        $01A5: aStr[i] := widechar($0070);    // ƥ -> p | LATIN SMALL LETTER P WITH HOOK -> LATIN SMALL LETTER P
        $02A0: aStr[i] := widechar($0071);    // ʠ -> q | LATIN SMALL LETTER Q WITH HOOK -> LATIN SMALL LETTER Q
        $027C: aStr[i] := widechar($0072);    // ɼ -> r | LATIN SMALL LETTER R WITH LONG LEG -> LATIN SMALL LETTER R
        $027D: aStr[i] := widechar($0072);    // ɽ -> r | LATIN SMALL LETTER R WITH TAIL -> LATIN SMALL LETTER R
        $0282: aStr[i] := widechar($0073);    // ʂ -> s | LATIN SMALL LETTER S WITH HOOK -> LATIN SMALL LETTER S
        $0167: aStr[i] := widechar($0074);    // ŧ -> t | LATIN SMALL LETTER T WITH STROKE -> LATIN SMALL LETTER T
        $01AB: aStr[i] := widechar($0074);    // ƫ -> t | LATIN SMALL LETTER T WITH PALATAL HOOK -> LATIN SMALL LETTER T
        $01AD: aStr[i] := widechar($0074);    // ƭ -> t | LATIN SMALL LETTER T WITH HOOK -> LATIN SMALL LETTER T
        $0236: aStr[i] := widechar($0074);    // ȶ -> t | LATIN SMALL LETTER T WITH CURL -> LATIN SMALL LETTER T
        $0288: aStr[i] := widechar($0074);    // ʈ -> t | LATIN SMALL LETTER T WITH RETROFLEX HOOK -> LATIN SMALL LETTER T
        $028B: aStr[i] := widechar($0076);    // ʋ -> v | LATIN SMALL LETTER V WITH HOOK -> LATIN SMALL LETTER V
        $01B4: aStr[i] := widechar($0079);    // ƴ -> y | LATIN SMALL LETTER Y WITH HOOK -> LATIN SMALL LETTER Y
        $01B6: aStr[i] := widechar($007A);    // ƶ -> z | LATIN SMALL LETTER Z WITH STROKE -> LATIN SMALL LETTER Z
        $0225: aStr[i] := widechar($007A);    // ȥ -> z | LATIN SMALL LETTER Z WITH HOOK -> LATIN SMALL LETTER Z
        $0290: aStr[i] := widechar($007A);    // ʐ -> z | LATIN SMALL LETTER Z WITH RETROFLEX HOOK -> LATIN SMALL LETTER Z
        $0291: aStr[i] := widechar($007A);    // ʑ -> z | LATIN SMALL LETTER Z WITH CURL -> LATIN SMALL LETTER Z
        $025A: aStr[i] := widechar($0259);    // ɚ -> ə | LATIN SMALL LETTER SCHWA WITH HOOK -> LATIN SMALL LETTER SCHWA
        $0286: aStr[i] := widechar($0283);    // ʆ -> ʃ | LATIN SMALL LETTER ESH WITH CURL -> LATIN SMALL LETTER ESH
        $01BA: aStr[i] := widechar($0292);    // ƺ -> ʒ | LATIN SMALL LETTER EZH WITH TAIL -> LATIN SMALL LETTER EZH
        $0293: aStr[i] := widechar($0292);    // ʓ -> ʒ | LATIN SMALL LETTER EZH WITH CURL -> LATIN SMALL LETTER EZH
        $0490: aStr[i] := widechar($0413);    // Ґ -> Г | CYRILLIC CAPITAL LETTER GHE WITH UPTURN -> CYRILLIC CAPITAL LETTER GHE
        $0492: aStr[i] := widechar($0413);    // Ғ -> Г | CYRILLIC CAPITAL LETTER GHE WITH STROKE -> CYRILLIC CAPITAL LETTER GHE
        $0494: aStr[i] := widechar($0413);    // Ҕ -> Г | CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK -> CYRILLIC CAPITAL LETTER GHE
        $0496: aStr[i] := widechar($0416);    // Җ -> Ж | CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER -> CYRILLIC CAPITAL LETTER ZHE
        $0498: aStr[i] := widechar($0417);    // Ҙ -> З | CYRILLIC CAPITAL LETTER ZE WITH DESCENDER -> CYRILLIC CAPITAL LETTER ZE
        $048A: aStr[i] := widechar($0419);    // Ҋ -> Й | CYRILLIC CAPITAL LETTER SHORT I WITH TAIL -> CYRILLIC CAPITAL LETTER SHORT I
        $049A: aStr[i] := widechar($041A);    // Қ -> К | CYRILLIC CAPITAL LETTER KA WITH DESCENDER -> CYRILLIC CAPITAL LETTER KA
        $049C: aStr[i] := widechar($041A);    // Ҝ -> К | CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE -> CYRILLIC CAPITAL LETTER KA
        $049E: aStr[i] := widechar($041A);    // Ҟ -> К | CYRILLIC CAPITAL LETTER KA WITH STROKE -> CYRILLIC CAPITAL LETTER KA
        $04C3: aStr[i] := widechar($041A);    // Ӄ -> К | CYRILLIC CAPITAL LETTER KA WITH HOOK -> CYRILLIC CAPITAL LETTER KA
        $04C5: aStr[i] := widechar($041B);    // Ӆ -> Л | CYRILLIC CAPITAL LETTER EL WITH TAIL -> CYRILLIC CAPITAL LETTER EL
        $04CD: aStr[i] := widechar($041C);    // Ӎ -> М | CYRILLIC CAPITAL LETTER EM WITH TAIL -> CYRILLIC CAPITAL LETTER EM
        $04A2: aStr[i] := widechar($041D);    // Ң -> Н | CYRILLIC CAPITAL LETTER EN WITH DESCENDER -> CYRILLIC CAPITAL LETTER EN
        $04C7: aStr[i] := widechar($041D);    // Ӈ -> Н | CYRILLIC CAPITAL LETTER EN WITH HOOK -> CYRILLIC CAPITAL LETTER EN
        $04C9: aStr[i] := widechar($041D);    // Ӊ -> Н | CYRILLIC CAPITAL LETTER EN WITH TAIL -> CYRILLIC CAPITAL LETTER EN
        $04A6: aStr[i] := widechar($041F);    // Ҧ -> П | CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK -> CYRILLIC CAPITAL LETTER PE
        $048E: aStr[i] := widechar($0420);    // Ҏ -> Р | CYRILLIC CAPITAL LETTER ER WITH TICK -> CYRILLIC CAPITAL LETTER ER
        $04AA: aStr[i] := widechar($0421);    // Ҫ -> С | CYRILLIC CAPITAL LETTER ES WITH DESCENDER -> CYRILLIC CAPITAL LETTER ES
        $04AC: aStr[i] := widechar($0422);    // Ҭ -> Т | CYRILLIC CAPITAL LETTER TE WITH DESCENDER -> CYRILLIC CAPITAL LETTER TE
        $04B2: aStr[i] := widechar($0425);    // Ҳ -> Х | CYRILLIC CAPITAL LETTER HA WITH DESCENDER -> CYRILLIC CAPITAL LETTER HA
        $04B3: aStr[i] := widechar($0425);    // ҳ -> Х | CYRILLIC SMALL LETTER HA WITH DESCENDER -> CYRILLIC CAPITAL LETTER HA
        $0491: aStr[i] := widechar($0433);    // ґ -> г | CYRILLIC SMALL LETTER GHE WITH UPTURN -> CYRILLIC SMALL LETTER GHE
        $0493: aStr[i] := widechar($0433);    // ғ -> г | CYRILLIC SMALL LETTER GHE WITH STROKE -> CYRILLIC SMALL LETTER GHE
        $0495: aStr[i] := widechar($0433);    // ҕ -> г | CYRILLIC SMALL LETTER GHE WITH MIDDLE HOOK -> CYRILLIC SMALL LETTER GHE
        $0497: aStr[i] := widechar($0436);    // җ -> ж | CYRILLIC SMALL LETTER ZHE WITH DESCENDER -> CYRILLIC SMALL LETTER ZHE
        $0499: aStr[i] := widechar($0437);    // ҙ -> з | CYRILLIC SMALL LETTER ZE WITH DESCENDER -> CYRILLIC SMALL LETTER ZE
        $048B: aStr[i] := widechar($0439);    // ҋ -> й | CYRILLIC SMALL LETTER SHORT I WITH TAIL -> CYRILLIC SMALL LETTER SHORT I
        $049B: aStr[i] := widechar($043A);    // қ -> к | CYRILLIC SMALL LETTER KA WITH DESCENDER -> CYRILLIC SMALL LETTER KA
        $049D: aStr[i] := widechar($043A);    // ҝ -> к | CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE -> CYRILLIC SMALL LETTER KA
        $049F: aStr[i] := widechar($043A);    // ҟ -> к | CYRILLIC SMALL LETTER KA WITH STROKE -> CYRILLIC SMALL LETTER KA
        $04C4: aStr[i] := widechar($043A);    // ӄ -> к | CYRILLIC SMALL LETTER KA WITH HOOK -> CYRILLIC SMALL LETTER KA
        $04C6: aStr[i] := widechar($043B);    // ӆ -> л | CYRILLIC SMALL LETTER EL WITH TAIL -> CYRILLIC SMALL LETTER EL
        $04CE: aStr[i] := widechar($043C);    // ӎ -> м | CYRILLIC SMALL LETTER EM WITH TAIL -> CYRILLIC SMALL LETTER EM
        $04A3: aStr[i] := widechar($043D);    // ң -> н | CYRILLIC SMALL LETTER EN WITH DESCENDER -> CYRILLIC SMALL LETTER EN
        $04C8: aStr[i] := widechar($043D);    // ӈ -> н | CYRILLIC SMALL LETTER EN WITH HOOK -> CYRILLIC SMALL LETTER EN
        $04CA: aStr[i] := widechar($043D);    // ӊ -> н | CYRILLIC SMALL LETTER EN WITH TAIL -> CYRILLIC SMALL LETTER EN
        $04A7: aStr[i] := widechar($043F);    // ҧ -> п | CYRILLIC SMALL LETTER PE WITH MIDDLE HOOK -> CYRILLIC SMALL LETTER PE
        $048F: aStr[i] := widechar($0440);    // ҏ -> р | CYRILLIC SMALL LETTER ER WITH TICK -> CYRILLIC SMALL LETTER ER
        $04AB: aStr[i] := widechar($0441);    // ҫ -> с | CYRILLIC SMALL LETTER ES WITH DESCENDER -> CYRILLIC SMALL LETTER ES
        $04AD: aStr[i] := widechar($0442);    // ҭ -> т | CYRILLIC SMALL LETTER TE WITH DESCENDER -> CYRILLIC SMALL LETTER TE
        $04B9: aStr[i] := widechar($0447);    // ҹ -> ч | CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE -> CYRILLIC SMALL LETTER CHE
        $047C: aStr[i] := widechar($0460);    // Ѽ -> Ѡ | CYRILLIC CAPITAL LETTER OMEGA WITH TITLO -> CYRILLIC CAPITAL LETTER OMEGA
        $047D: aStr[i] := widechar($0461);    // ѽ -> ѡ | CYRILLIC SMALL LETTER OMEGA WITH TITLO -> CYRILLIC SMALL LETTER OMEGA
        $04B0: aStr[i] := widechar($04AE);    // Ұ -> Ү | CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE -> CYRILLIC CAPITAL LETTER STRAIGHT U
        $04B1: aStr[i] := widechar($04AF);    // ұ -> ү | CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE -> CYRILLIC SMALL LETTER STRAIGHT U
        $04B6: aStr[i] := widechar($04BC);    // Ҷ -> Ҽ | CYRILLIC CAPITAL LETTER CHE WITH DESCENDER -> CYRILLIC CAPITAL LETTER ABKHASIAN CHE
        $04B7: aStr[i] := widechar($04BC);    // ҷ -> Ҽ | CYRILLIC SMALL LETTER CHE WITH DESCENDER -> CYRILLIC CAPITAL LETTER ABKHASIAN CHE
        $04B8: aStr[i] := widechar($04BC);    // Ҹ -> Ҽ | CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE -> CYRILLIC CAPITAL LETTER ABKHASIAN CHE
        $04BE: aStr[i] := widechar($04BC);    // Ҿ -> Ҽ | CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER -> CYRILLIC CAPITAL LETTER ABKHASIANCHE
        $04BF: aStr[i] := widechar($04BC);    // ҿ -> Ҽ | CYRILLIC SMALL LETTER ABKHASIAN CHE WITH DESCENDER -> CYRILLIC CAPITAL LETTER ABKHASIAN CHE
        $04CB: aStr[i] := widechar($04BC);    // Ӌ -> Ҽ | CYRILLIC CAPITAL LETTER KHAKASSIAN CHE -> CYRILLIC CAPITAL LETTER ABKHASIAN CHE
        $04CC: aStr[i] := widechar($04BC);    // ӌ -> Ҽ | CYRILLIC SMALL LETTER KHAKASSIAN CHE -> CYRILLIC CAPITAL LETTER ABKHASIAN CHE
      end;
    end;
  End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _IsIgnoredSymbol(aSymbol: WideChar): boolean;
  var I: integer;
  begin
    result := False;
    for I := Low(SymbolsToIgnore) to High(SymbolsToIgnore) do
      if SymbolsToIgnore[i] = aSymbol then begin
        result := true;
        exit;
      end;
  end;

Var
  TmpWideStr: WideString;
  I,J: integer;

Begin
  TmpWideStr := ALExpandLigatures(ALRemoveDiacritic(Widelowercase(s)));
  SetLength(Result,length(TmpWideStr));
  j := 0;
  For i := 1 to length(TmpWideStr) do begin
    if IsCharAlphaNumericW(TmpWideStr[i]) or
       _IsIgnoredSymbol(TmpWideStr[i]) then begin
      inc(j);
      result[j] := TmpWideStr[i];
    end
    else if ((j >= 1) and
             (result[j] <> WordSeparator)) then begin
      inc(j);
      result[j] := WordSeparator;
    end;
  end;
  While (J > 0) and (result[j] = WordSeparator) do dec(j);
  setlength(result,j);
  _foldNonDiacriticChar(result);
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
// Normalize a Widestring
// ie: l''été sur l''europe => l-ete-sur-l-europe
Function ALNormalize(
           const S: Widestring;
           const WordSeparator: WideChar = '-'): Widestring;
Begin
  result := ALNormalize(S, WordSeparator, []);
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
// S is a AnsiString that contains UTF-8 encoded characters
// The result of the function is the corresponding UTF-8 encoded string
// without any Diacritic.
Function ALRemoveDiacritic(const S: AnsiString): AnsiString;
begin
  Result := AnsiString(ALRemoveDiacritic(WideString(S)));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
{remove accented character}
Function ALRemoveDiacritic(const S: Widestring): Widestring;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function internalGetCompositeCharSize(aChar: WideString): integer;
  Begin
    //max(1,xxx) in case FoldString return on error mean result = 0
    //this can really happen ?
    Result := Max(1, FoldStringW(MAP_COMPOSITE, PwideChar(aChar), length(aChar), nil, 0));
  end;

var
  LenS, LenTmpWideStr: Integer;
  i,J: integer;
  TmpWideStr: WideString;

begin
  result := '';
  If s = '' then exit;
  LenS := length(S);
  LenTmpWideStr := FoldStringW(MAP_COMPOSITE, PwideChar(S), LenS, nil, 0);
  if LenTmpWideStr <= 0 then Exit;
  setlength(TmpWideStr,LenTmpWideStr);
  FoldStringW(MAP_COMPOSITE, PwideChar(S), LenS, PwideChar(TmpWideStr), LenTmpWideStr);
  i := 1;
  J := 1;
  SetLength(result,lenS);
  while J <= lenS do begin
    Result[j] := TmpWideStr[i];
    if S[j] <> TmpWideStr[i] then inc(i,internalGetCompositeCharSize(S[j])) //some Diacritic can have a length of 3 in composite (ie: U+1E69)
    else inc(i);
    inc(j);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
// S is a AnsiString that contains UTF-8 encoded characters
// The result of the function is the corresponding UTF-8 encoded string
// without any Ligatures.
Function ALExpandLigatures(const S: AnsiString): AnsiString;
begin
  Result := AnsiString(ALExpandLigatures(WideString(S)));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
// Expand all ligature characters so that they are represented by
// their two-character equivalent. For example, the ligature 'æ' expands to
// the two characters 'a' and 'e'.}
Function ALExpandLigatures(const S: Widestring): Widestring;
Const MAP_EXPAND_LIGATURES = $2000;
var LenS, LenResult: Integer;
begin
  result := '';
  If s = '' then exit;
  LenS := length(S);
  LenResult := FoldStringW(MAP_EXPAND_LIGATURES, PwideChar(S), LenS, nil, 0);
  setlength(Result,LenResult);
  FoldStringW(MAP_EXPAND_LIGATURES, PwideChar(S), LenS, PwideChar(Result), LenResult);
end;
{$ENDIF}

{*****************************************************}
function  AlUpperCase(const S: AnsiString): AnsiString;
begin
  result := System.Ansistrings.UpperCase(S);
end;

{*********************************************}
function  AlUpperCase(const S: string): string;
begin
  result := system.sysutils.UpperCase(S);
end;

{*****************************************************}
function  AlLowerCase(const S: AnsiString): AnsiString;
begin
  result := System.Ansistrings.LowerCase(S);
end;

{*********************************************}
function  AlLowerCase(const S: string): string;
begin
  result := system.sysutils.LowerCase(S);
end;

{***********************************************}
function  AlUpCase(const Ch: AnsiChar): AnsiChar;
begin
  Result := Ch;
  if Result in ['a'..'z'] then
    Dec(Result, Ord('a')-Ord('A'));
end;

{*********************************}
function  AlUpCase(Ch: Char): Char;
begin
  result := system.UpCase(Ch);
end;

{***********************************************}
function  AlLoCase(const Ch: AnsiChar): AnsiChar;
begin
  Result := Ch;
  if Result in ['A'..'Z'] then
    Inc(Result, Ord('a')-Ord('A'));
end;

{*********************************}
function  AlLoCase(Ch: Char): Char;
begin
  Result := Ch;
  case Ch of
    'A'..'Z':
      Inc(Result, Ord(char('a'))-Ord(char('A')));
  end;
end;

{**********************}
{$IF defined(MSWINDOWS)}
Function ALStringToWideString(const S: RawByteString; const aCodePage: Word): WideString;
var InputLength,
    OutputLength: Integer;
begin
  InputLength := Length(S);
  if InputLength = 0 then begin
    result := '';
    exit;
  end;
  OutputLength := MultiByteToWideChar(
                    aCodePage,     // UINT CodePage,
                    0,             // DWORD dwFlags
                    PAnsiChar(S),  // LPCSTR lpMultiByteStr
                    InputLength,   // int cbMultiByte
                    nil,           // LPWSTR lpWideCharStr
                    0);            // int cchWideChar
  if OutputLength = 0 then raiseLastOsError;
  SetLength(Result, OutputLength);
  if MultiByteToWideChar(
       aCodePage,
       0,
       PAnsiChar(S),
       InputLength,
       PWideChar(Result),
       OutputLength) = 0 then raiseLastOsError;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function AlWideStringToString(const WS: WideString; const aCodePage: Word): AnsiString;
var InputLength,
    OutputLength: Integer;
begin
  InputLength := Length(WS);
  if InputLength = 0 then begin
    result := '';
    exit;
  end;
  OutputLength := WideCharToMultiByte(
                    aCodePage,      // UINT CodePage
                    0,              // DWORD dwFlags,
                    PWideChar(WS),  // LPCWSTR lpWideCharStr,
                    InputLength,    // int cchWideChar
                    nil,            // LPSTR lpMultiByteStr,
                    0,              // int cbMultiByte
                    nil,            // LPCSTR lpDefaultChar (Pointer to the character to use if a character cannot be represented in the specified code page)
                    nil);           // LPBOOL lpUsedDefaultChar (Pointer to a flag that indicates if the function has used a default character in the conversion)
  if OutputLength = 0 then raiseLastOsError;
  SetLength(Result, OutputLength);
  if WideCharToMultiByte(
       aCodePage,
       0,
       PWideChar(WS),
       InputLength,
       PAnsiChar(Result),
       OutputLength,
       nil,
       nil) = 0 then raiseLastOsError;
end;
{$ENDIF}

{***********************************************************}
function ALUnicodeUpperCase(const s: AnsiString): AnsiString;
begin
  result := AnsiString(ALUnicodeUpperCase(String(s)));
end;

{*********************************}
// this function use CharUpperBuffW
// The only problem I know that makes Unicode uppercase/lowercase conversion
// locale-dependent is the case of dotless i (ı, $0131) and dotted I (İ, $0130).
// In most languages the upper of i ($69) is I ($49), but in turkish locale i ($69)
// maps to İ ($0130). Similarly in turkish the lower of I ($49) is ı ($0131).
// CharUpperBuff/CharLowerBuffW always maps lowercase I ("i") to uppercase I,
// even when the current language is Turkish or Azeri and this is why I prefer
// to use WideUppercase/WideLowerCase instead of str.toUpper/str.toLower string
// helper (that use internaly LCMapString who depend of the localID) because I
// want to stay consistant.
function ALUnicodeUpperCase(const s: String): String;
begin
  result := String(WideUppercase(WideString(s)));
end;

{***********************************************************}
function ALUnicodeLowerCase(const s: AnsiString): AnsiString;
begin
  result := AnsiString(ALUnicodeLowerCase(String(s)));
end;

{*********************************}
// this function use CharLowerBuffW
// The only problem I know that makes Unicode uppercase/lowercase conversion
// locale-dependent is the case of dotless i (ı, $0131) and dotted I (İ, $0130).
// In most languages the upper of i ($69) is I ($49), but in turkish locale i ($69)
// maps to İ ($0130). Similarly in turkish the lower of I ($49) is ı ($0131).
// CharUpperBuff/CharLowerBuffW always maps lowercase I ("i") to uppercase I,
// even when the current language is Turkish or Azeri and this is why I prefer
// to use WideUppercase/WideLowerCase instead of str.toUpper/str.toLower string
// helper (that use internaly LCMapString who depend of the localID) because I
// want to stay consistant.
function ALUnicodeLowerCase(const s: String): String;
begin
  result := String(WideLowerCase(WideString(s)));
end;

{**********************}
{$IF defined(MSWINDOWS)}
// S is a AnsiString that contains UTF-8 encoded characters
// The result of the function is the corresponding UTF-8 encoded string
// in UpperCase without any Diacritic.
Function ALUnicodeUpperCaseNoDiacritic(const S: AnsiString): AnsiString;
begin
  Result := AnsiString(ALUnicodeUpperCaseNoDiacritic(WideString(S)));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
Function ALUnicodeUpperCaseNoDiacritic(const S: Widestring): Widestring;
begin
  Result := ALRemoveDiacritic(WideUppercase(s));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
// S is a AnsiString that contains UTF-8 encoded characters
// The result of the function is the corresponding UTF-8 encoded string
// in LowerCase without any Diacritic.
Function ALUnicodeLowerCaseNoDiacritic(const S: AnsiString): AnsiString;
begin
  Result := AnsiString(ALUnicodeLowerCaseNoDiacritic(WideString(S)));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
Function ALUnicodeLowerCaseNoDiacritic(const S: Widestring): Widestring;
begin
  Result := ALRemoveDiacritic(Widelowercase(s));
end;
{$ENDIF}

{*****************************}
{Uppercase only the First char}
Function ALUnicodeUpperFirstChar(const s:AnsiString): AnsiString;
var tmpWideStr: WideString;
begin
  TmpWideStr := WideString(S);
  result := AnsiString(WideUpperCase(copy(TmpWideStr,1,1)) + copy(TmpWideStr,2,MaxInt));
end;

{*********************************************}
//the first letter of each word is capitalized,
//the rest are lower case
Function ALTitleCase(const s:AnsiString): AnsiString;
begin
  // in such function, use of the ansiString is
  // painfull, so convert to unicode and do the job
  result := ansiString(ALTitleCase(string(s)));
end;

{*********************************************}
//the first letter of each word is capitalized,
//the rest are lower case
Function ALTitleCase(const s: String): String;
var I: integer;
begin
  Result := ALSentenceCase(s);
  for i:= low(result)+1 to high(Result) do
    if (Result[i-1].IsInArray(['&', ' ', '-', ''''])) and
       (
        (i = high(Result)) or
        (
         ((Result[i+1] <> ' ') or (Result[i-1] = '&')) and // Agenge L&G Prestige - Maison à Vendre - A Prendre Ou a Laisser
         (Result[i+1] <> '''') // Avenue de l'Elysée
        )
       )
    then Result[i] := Result.Chars[i-low(result)].ToUpper;
end;

{****************************************************************}
// first letter of the sentence capitalized, all others lower case
Function ALSentenceCase(const s:AnsiString): AnsiString;
begin
  // in such function, use of the ansiString is
  // painfull, so convert to unicode and do the job
  result := ansiString(ALSentenceCase(string(s)));
end;

{***************************************************************}
//first letter of the sentence capitalized, all others lower case
Function ALSentenceCase(const s: String): String;
begin
  Result := S.ToLower;
  if length(Result) = 0 then exit;
  result[low(result)] := Result.Chars[0].ToUpper;
end;

{*******************************************************************************************
 ISO 9:1995 is the current transliteration standard from ISO. It is based on its predecessor
 ISO/R 9:1968, which it deprecates; for Russian they only differ in the treatment of five
 modern letters. It is the first language-independent, univocal system of one character
 for one character equivalents (by the use of diacritics), which faithfully represents
 the original and allows for reverse transliteration for Cyrillic text in any contemporary language.}
Function ALISO91995CyrillicToLatin(const aCyrillicText: AnsiString): AnsiString;

Var
  LCyrillicWideStr: WideString;
  LLatinWideStr: WideString;
  LLatinWideStrCurrentPos: Integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure InternalAddCharsToResult(aArrayOfChar: Array of Integer);
  var I: integer;
  begin
    for I := low(aArrayOfChar) to high(aArrayOfChar) do begin
      inc(LLatinWideStrCurrentPos);
      LLatinWideStr[LLatinWideStrCurrentPos] := WideChar(aArrayOfChar[i]);
    end;
  end;

Var
  I, j: Integer;

Begin
  Result := '';
  LCyrillicWideStr := WideString(aCyrillicText);
  setlength(LLatinWideStr,length(LCyrillicWideStr) * 2); //to Be on the safe way
  LLatinWideStrCurrentPos := 0;
  for I := 1 to length(LCyrillicWideStr) do begin
    J := ord(LCyrillicWideStr[i]);
    case J of
      $0410 {А} : InternalAddCharsToResult([$0041]); {A}
      $0430 {а} : InternalAddCharsToResult([$0061]); {a}
      $04D0 {Ӑ} : InternalAddCharsToResult([$0102]); {Ă}
      $04D1 {ӑ} : InternalAddCharsToResult([$0103]); {ă}
      $04D2 {Ӓ} : InternalAddCharsToResult([$00C4]); {Ä}
      $04D3 {ӓ} : InternalAddCharsToResult([$00E4]); {ä}
      $04D8 {Ә} : InternalAddCharsToResult([$0041,$030B]); {A̋}
      $04D9 {ә} : InternalAddCharsToResult([$0061,$030B]); {a̋}
      $0411 {Б} : InternalAddCharsToResult([$0042]); {B}
      $0431 {б} : InternalAddCharsToResult([$0062]); {b}
      $0412 {В} : InternalAddCharsToResult([$0056]); {V}
      $0432 {в} : InternalAddCharsToResult([$0076]); {v}
      $0413 {Г} : InternalAddCharsToResult([$0047]); {G}
      $0433 {г} : InternalAddCharsToResult([$0067]); {g}
      $0490 {Ґ} : InternalAddCharsToResult([$0047,$0300]); {G̀}
      $0491 {ґ} : InternalAddCharsToResult([$0067,$0300]); {g̀}
      $0494 {Ҕ} : InternalAddCharsToResult([$011E]); {Ğ}
      $0495 {ҕ} : InternalAddCharsToResult([$011F]); {ğ}
      $0492 {Ғ} : InternalAddCharsToResult([$0120]); {Ġ}
      $0493 {ғ} : InternalAddCharsToResult([$0121]); {ġ}
      $0414 {Д} : InternalAddCharsToResult([$0044]); {D}
      $0434 {д} : InternalAddCharsToResult([$0064]); {d}
      $0402 {Ђ} : InternalAddCharsToResult([$0110]); {Đ}
      $0452 {ђ} : InternalAddCharsToResult([$0111]); {đ}
      $0403 {Ѓ} : InternalAddCharsToResult([$01F4]); {Ǵ}
      $0453 {ѓ} : InternalAddCharsToResult([$01F5]); {ǵ}
      $0415 {Е} : InternalAddCharsToResult([$0045]); {E}
      $0435 {е} : InternalAddCharsToResult([$0065]); {e}
      $0401 {Ё} : InternalAddCharsToResult([$00CB]); {Ë}
      $0451 {ё} : InternalAddCharsToResult([$00EB]); {ë}
      $04D6 {Ӗ} : InternalAddCharsToResult([$0114]); {Ĕ}
      $04D7 {ӗ} : InternalAddCharsToResult([$0115]); {ĕ}
      $0404 {Є} : InternalAddCharsToResult([$00CA]); {Ê}
      $0454 {є} : InternalAddCharsToResult([$00EA]); {ê}
      $04BC {Ҽ} : InternalAddCharsToResult([$0043,$0306]); {C̆}
      $04BD {ҽ} : InternalAddCharsToResult([$0063,$0306]); {c̆}
      $04BE {Ҿ} : InternalAddCharsToResult([$00C7,$0306]); {Ç̆}
      $04BF {ҿ} : InternalAddCharsToResult([$00E7,$0306]); {ç̆}
      $0416 {Ж} : InternalAddCharsToResult([$017D]); {Ž}
      $0436 {ж} : InternalAddCharsToResult([$017E]); {ž}
      $04C1 {Ӂ} : InternalAddCharsToResult([$005A,$0306]); {Z̆}
      $04C2 {ӂ} : InternalAddCharsToResult([$007A,$0306]); {z̆}
      $04DC {Ӝ} : InternalAddCharsToResult([$005A,$0304]); {Z̄}
      $04DD {ӝ} : InternalAddCharsToResult([$007A,$0304]); {z̄}
      $0496 {Җ} : InternalAddCharsToResult([$017D,$0326]); {Ž̦}
      $0497 {җ} : InternalAddCharsToResult([$017E,$0327]); {ž̧}
      $0417 {З} : InternalAddCharsToResult([$005A]); {Z}
      $0437 {з} : InternalAddCharsToResult([$007A]); {z}
      $04DE {Ӟ} : InternalAddCharsToResult([$005A,$0308]); {Z̈}
      $04DF {ӟ} : InternalAddCharsToResult([$007A,$0308]); {z̈}
      $0405 {Ѕ} : InternalAddCharsToResult([$1E90]); {Ẑ}
      $0455 {ѕ} : InternalAddCharsToResult([$1E91]); {ẑ}
      $04E0 {Ӡ} : InternalAddCharsToResult([$0179]); {Ź}
      $04E1 {ӡ} : InternalAddCharsToResult([$017A]); {ź}
      $0418 {И} : InternalAddCharsToResult([$0049]); {I}
      $0438 {и} : InternalAddCharsToResult([$0069]); {i}
      $04E4 {Ӥ} : InternalAddCharsToResult([$00CE]); {Î}
      $04E5 {ӥ} : InternalAddCharsToResult([$00EE]); {î}
      $0406 {І} : InternalAddCharsToResult([$00CC]); {Ì}
      $0456 {і} : InternalAddCharsToResult([$00EC]); {ì}
      $0407 {Ї} : InternalAddCharsToResult([$00CF]); {Ï}
      $0457 {ї} : InternalAddCharsToResult([$00EF]); {ï}
      $0419 {Й} : InternalAddCharsToResult([$004A]); {J}
      $0439 {й} : InternalAddCharsToResult([$006A]); {j}
      $0408 {Ј} : InternalAddCharsToResult([$004A,$030C]); {J̌}
      $0458 {ј} : InternalAddCharsToResult([$01F0]); {ǰ}
      $041A {К} : InternalAddCharsToResult([$004B]); {K}
      $043A {к} : InternalAddCharsToResult([$006B]); {k}
      $049A {Қ} : InternalAddCharsToResult([$0136]); {Ķ}
      $049B {қ} : InternalAddCharsToResult([$0137]); {ķ}
      $049E {Ҟ} : InternalAddCharsToResult([$004B,$0304]); {K̄}
      $049F {ҟ} : InternalAddCharsToResult([$006B,$0304]); {k̄}
      $041B {Л} : InternalAddCharsToResult([$004C]); {L}
      $043B {л} : InternalAddCharsToResult([$006C]); {l}
      $0409 {Љ} : InternalAddCharsToResult([$004C,$0302]); {L̂}
      $0459 {љ} : InternalAddCharsToResult([$006C,$0302]); {l̂}
      $041C {М} : InternalAddCharsToResult([$004D]); {M}
      $043C {м} : InternalAddCharsToResult([$006D]); {m}
      $041D {Н} : InternalAddCharsToResult([$004E]); {N}
      $043D {н} : InternalAddCharsToResult([$006E]); {n}
      $040A {Њ} : InternalAddCharsToResult([$004E,$0302]); {N̂}
      $045A {њ} : InternalAddCharsToResult([$006E,$0302]); {n̂}
      $04A4 {Ҥ} : InternalAddCharsToResult([$1E44]); {Ṅ}
      $04A5 {ҥ} : InternalAddCharsToResult([$1E45]); {ṅ}
      $04A2 {Ң} : InternalAddCharsToResult([$1E46]); {Ṇ}
      $04A3 {ң} : InternalAddCharsToResult([$1E47]); {ṇ}
      $041E {О} : InternalAddCharsToResult([$004F]); {O}
      $043E {о} : InternalAddCharsToResult([$006F]); {o}
      $04E6 {Ӧ} : InternalAddCharsToResult([$00D6]); {Ö}
      $04E7 {ӧ} : InternalAddCharsToResult([$00F6]); {ö}
      $04E8 {Ө} : InternalAddCharsToResult([$00D4]); {Ô}
      $04E9 {ө} : InternalAddCharsToResult([$00F4]); {ô}
      $041F {П} : InternalAddCharsToResult([$0050]); {P}
      $043F {п} : InternalAddCharsToResult([$0070]); {p}
      $04A6 {Ҧ} : InternalAddCharsToResult([$1E54]); {Ṕ}
      $04A7 {ҧ} : InternalAddCharsToResult([$1E55]); {ṕ}
      $0420 {Р} : InternalAddCharsToResult([$0052]); {R}
      $0440 {р} : InternalAddCharsToResult([$0072]); {r}
      $0421 {С} : InternalAddCharsToResult([$0053]); {S}
      $0441 {с} : InternalAddCharsToResult([$0073]); {s}
      $04AA {Ҫ} : InternalAddCharsToResult([$00C7]); {Ç}
      $04AB {ҫ} : InternalAddCharsToResult([$00E7]); {ç}
      $0422 {Т} : InternalAddCharsToResult([$0054]); {T}
      $0442 {т} : InternalAddCharsToResult([$0074]); {t}
      $04AC {Ҭ} : InternalAddCharsToResult([$0162]); {Ţ}
      $04AD {ҭ} : InternalAddCharsToResult([$0163]); {ţ}
      $040B {Ћ} : InternalAddCharsToResult([$0106]); {Ć}
      $045B {ћ} : InternalAddCharsToResult([$0170]); {Ű}
      $040C {Ќ} : InternalAddCharsToResult([$1E30]); {Ḱ}
      $045C {ќ} : InternalAddCharsToResult([$1E31]); {ḱ}
      $0423 {У} : InternalAddCharsToResult([$0055]); {U}
      $0443 {у} : InternalAddCharsToResult([$0075]); {u}
      $EE19 {} : InternalAddCharsToResult([$00DA]); {Ú}
      $EE99 {} : InternalAddCharsToResult([$00FA]); {ú}
      $040E {Ў} : InternalAddCharsToResult([$016C]); {Ŭ}
      $045E {ў} : InternalAddCharsToResult([$016D]); {ŭ}
      $04F0 {Ӱ} : InternalAddCharsToResult([$00DC]); {Ü}
      $04F1 {ӱ} : InternalAddCharsToResult([$00FC]); {ü}
      $04F2 {Ӳ} : InternalAddCharsToResult([$0170]); {Ű}
      $04F3 {ӳ} : InternalAddCharsToResult([$0171]); {ű}
      $04AE {Ү} : InternalAddCharsToResult([$00D9]); {Ù}
      $04AF {ү} : InternalAddCharsToResult([$00F9]); {ù}
      $0424 {Ф} : InternalAddCharsToResult([$0046]); {F}
      $0444 {ф} : InternalAddCharsToResult([$0066]); {f}
      $0425 {Х} : InternalAddCharsToResult([$0048]); {H}
      $0445 {х} : InternalAddCharsToResult([$0068]); {h}
      $04B2 {Ҳ} : InternalAddCharsToResult([$1E28]); {Ḩ}
      $04B3 {ҳ} : InternalAddCharsToResult([$1E29]); {ḩ}
      $04BA {Һ} : InternalAddCharsToResult([$1E24]); {Ḥ}
      $04BB {һ} : InternalAddCharsToResult([$1E25]); {ḥ}
      $0426 {Ц} : InternalAddCharsToResult([$0043]); {C}
      $0446 {ц} : InternalAddCharsToResult([$0063]); {c}
      $04B4 {Ҵ} : InternalAddCharsToResult([$0043,$0304]); {C̄}
      $04B5 {ҵ} : InternalAddCharsToResult([$0063,$0304]); {c̄}
      $0427 {Ч} : InternalAddCharsToResult([$010C]); {Č}
      $0447 {ч} : InternalAddCharsToResult([$010D]); {č}
      $04F4 {Ӵ} : InternalAddCharsToResult([$0043,$0308]); {C̈}
      $04F5 {ӵ} : InternalAddCharsToResult([$0063,$0308]); {c̈}
      $04CB {Ӌ} : InternalAddCharsToResult([$00C7]); {Ç}
      $04CC {ӌ} : InternalAddCharsToResult([$00E7]); {ç}
      $040F {Џ} : InternalAddCharsToResult([$0044,$0302]); {D̂}
      $045F {џ} : InternalAddCharsToResult([$0064,$0302]); {d̂}
      $0428 {Ш} : InternalAddCharsToResult([$0160]); {Š}
      $0448 {ш} : InternalAddCharsToResult([$0161]); {š}
      $0429 {Щ} : InternalAddCharsToResult([$015C]); {Ŝ}
      $0449 {щ} : InternalAddCharsToResult([$015D]); {ŝ}
      $042A {Ъ} : InternalAddCharsToResult([$02BA]); {ʺ}
      {The capital hard sign is very seldom in use. It may be capital, if everything is written in upper case -
       therefore when transliteration is used reverse we can say that $02BA = $042A if everything is written in upper case}
      $044A {ъ} : InternalAddCharsToResult([$02BA]); {ʺ}
      $02BC {ʼ} : InternalAddCharsToResult([$2019]); {’}
      $042B {Ы} : InternalAddCharsToResult([$0059]); {Y}
      $044B {ы} : InternalAddCharsToResult([$0079]); {y}
      $04F8 {Ӹ} : InternalAddCharsToResult([$0178]); {Ÿ}
      $04F9 {ӹ} : InternalAddCharsToResult([$00FF]); {ÿ}
      $042C {Ь} : InternalAddCharsToResult([$02B9]); {ʹ}
      {The capital hard sign is very seldom in use. It may be capital, if everything is written in upper case -
       therefore when transliteration is used reverse we can say that $042C = $02B9 if everything is written in upper case}
      $044C {ь} : InternalAddCharsToResult([$02B9]); {ʹ}
      $042D {Э} : InternalAddCharsToResult([$00C8]); {È}
      $044D {э} : InternalAddCharsToResult([$00E8]); {è}
      $042E {Ю} : InternalAddCharsToResult([$00DB]); {Û}
      $044E {ю} : InternalAddCharsToResult([$00FB]); {û}
      $042F {Я} : InternalAddCharsToResult([$00C2]); {Â}
      $044F {я} : InternalAddCharsToResult([$00E2]); {â}
      $048C {Ҍ} : InternalAddCharsToResult([$011A]); {Ě}
      $048D {ҍ} : InternalAddCharsToResult([$011B]); {ě}
      $046A {Ѫ} : InternalAddCharsToResult([$01CD]); {Ǎ}
      $046B {ѫ} : InternalAddCharsToResult([$01CE]); {ǎ}
      $0472 {Ѳ} : InternalAddCharsToResult([$0046,$0300]); {F̀}
      $0473 {ѳ} : InternalAddCharsToResult([$0066,$0300]); {f̀}
      $0474 {Ѵ} : InternalAddCharsToResult([$1EF2]); {Ỳ}
      $0475 {ѵ} : InternalAddCharsToResult([$1EF3]); {ỳ}
      $04A8 {Ҩ} : InternalAddCharsToResult([$00D2]); {Ò}
      $04A9 {ҩ} : InternalAddCharsToResult([$00F2]); {ò}
      $04C0 {Ӏ} : InternalAddCharsToResult([$2021]); {‡}
      else InternalAddCharsToResult([j]);
    end;
  end;
  SetLength(LLatinWideStr,LLatinWideStrCurrentPos);
  Result := AnsiString(LLatinWideStr);
End;

{*******************************************************************************
 The BGN/PCGN system is relatively intuitive for anglophones to read and pronounce.
 In many publications a simplified form of the system is used to render English versions
 of Russian names, typically converting ë to yo, simplifying -iy and -yy endings to
 -y, and omitting apostrophes for ъ and ь. It can be rendered using only the basic
 letters and punctuation found on English-language keyboards: no diacritics or unusual
 letters are required, although the Interpunct character (·) can optionally be used to
 avoid some ambiguity.

 This particular standard is part of the BGN/PCGN romanization system which was developed
 by the United States Board on Geographic Names and by the Permanent Committee on
 Geographical Names for British Official Use. The portion of the system pertaining to
 the Russian language was adopted by BGN in 1944, and by PCGN in 1947}
Function ALBGNPCGN1947CyrillicToLatin(const aCyrillicText: AnsiString): AnsiString;

Var
  LCyrillicWideStr: WideString;
  LLatinWideStr: WideString;
  LLatinWideStrCurrentPos: Integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure InternalAddCharsToResult(aArrayOfChar: Array of Integer);
  var I: integer;
  begin
    for I := low(aArrayOfChar) to high(aArrayOfChar) do begin
      inc(LLatinWideStrCurrentPos);
      LLatinWideStr[LLatinWideStrCurrentPos] := WideChar(aArrayOfChar[i]);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function InternalCheckInRange(aChar: integer; aArrayOfChar: Array of Integer): boolean;
  var I: integer;
  begin
    Result := False;
    for I := low(aArrayOfChar) to high(aArrayOfChar) do
      if aChar = aArrayOfChar[i] then begin
        result := true;
        exit;
      end;
  end;

Var
  I, j: Integer;

Begin
  Result := '';
  LCyrillicWideStr := WideString(aCyrillicText);
  setlength(LLatinWideStr,length(LCyrillicWideStr) * 2); //to Be on the safe way
  LLatinWideStrCurrentPos := 0;
  for i := 1 to length(LCyrillicWideStr) do begin
    j := ord(LCyrillicWideStr[i]);
    case j of
      $0410 {А} : InternalAddCharsToResult([$0041]); {A}
      $0430 {а} : InternalAddCharsToResult([$0061]); {a}
      $0411 {Б} : InternalAddCharsToResult([$0042]); {B}
      $0431 {б} : InternalAddCharsToResult([$0062]); {b}
      $0412 {В} : InternalAddCharsToResult([$0056]); {V}
      $0432 {в} : InternalAddCharsToResult([$0076]); {v}
      $0413 {Г} : InternalAddCharsToResult([$0047]); {G}
      $0433 {г} : InternalAddCharsToResult([$0067]); {g}
      $0414 {Д} : InternalAddCharsToResult([$0044]); {D}
      $0434 {д} : InternalAddCharsToResult([$0064]); {d}
      $0415 {Е} : begin
                    {The character e should be romanized ye initially, after the vowel characters a, e, ё, и, о, у, ы, э, ю, and я,
                     and after й, ъ, and ь. In all other instances, it should be romanized e.}
                    if (i > 1) and InternalCheckInRange(
                                     ord(LCyrillicWideStr[i-1]),[$0410 {А}, $0430 {а},
                                     $0415 {Е}, $0435 {е},
                                     $0401 {Ё}, $0451 {ё},
                                     $0418 {И}, $0438 {и},
                                     $041E {О}, $043E {о},
                                     $0423 {У}, $0443 {у},
                                     $042B {Ы}, $044B {ы},
                                     $042D {Э}, $044D {э},
                                     $042E {Ю}, $044E {ю},
                                     $042F {Я}, $044F {я},
                                     $0419 {Й}, $0439 {й},
                                     $042A {Ъ}, $044A {ъ},
                                     $042C {Ь}, $044C {ь}]) then InternalAddCharsToResult([$0059, $0065]) {Ye}
                    else InternalAddCharsToResult([$0045]); {E}
                  end;
      $0435 {е} : begin
                    {The character e should be romanized ye initially, after the vowel characters a, e, ё, и, о, у, ы, э, ю, and я,
                     and after й, ъ, and ь. In all other instances, it should be romanized e.}
                    if (i > 1) and InternalCheckInRange(
                                     ord(LCyrillicWideStr[i-1]),[$0410 {А}, $0430 {а},
                                     $0415 {Е}, $0435 {е},
                                     $0401 {Ё}, $0451 {ё},
                                     $0418 {И}, $0438 {и},
                                     $041E {О}, $043E {о},
                                     $0423 {У}, $0443 {у},
                                     $042B {Ы}, $044B {ы},
                                     $042D {Э}, $044D {э},
                                     $042E {Ю}, $044E {ю},
                                     $042F {Я}, $044F {я},
                                     $0419 {Й}, $0439 {й},
                                     $042A {Ъ}, $044A {ъ},
                                     $042C {Ь}, $044C {ь}]) then InternalAddCharsToResult([$0079, $0065]) {ye}
                    else InternalAddCharsToResult([$0065]); {e}
                  end;
      $0401 {Ё} : begin
                    {The character ё is not considered a separate character of the Russian alphabet and the dieresis is generally not shown.
                    When the dieresis is shown, the character should be romanized yë initially, after the vowel characters a, e, ё, и, о, у, ы, э, ю, and я,
                    and after й, ъ, and ь. In all other instances, it should be romanized ё. When the dieresis is not shown, the character may still be
                    romanized in the preceding manner or, alternatively, in accordance with note 1.}
                    if (i > 1) and InternalCheckInRange(
                                     ord(LCyrillicWideStr[i-1]),[$0410 {А}, $0430 {а},
                                     $0415 {Е}, $0435 {е},
                                     $0401 {Ё}, $0451 {ё},
                                     $0418 {И}, $0438 {и},
                                     $041E {О}, $043E {о},
                                     $0423 {У}, $0443 {у},
                                     $042B {Ы}, $044B {ы},
                                     $042D {Э}, $044D {э},
                                     $042E {Ю}, $044E {ю},
                                     $042F {Я}, $044F {я},
                                     $0419 {Й}, $0439 {й},
                                     $042A {Ъ}, $044A {ъ},
                                     $042C {Ь}, $044C {ь}]) then InternalAddCharsToResult([$0059, $00EB]) {Yë}
                    else InternalAddCharsToResult([$00CB]); {Ë}
                  end;
      $0451 {ё} : begin
                    {The character ё is not considered a separate character of the Russian alphabet and the dieresis is generally not shown.
                    When the dieresis is shown, the character should be romanized yë initially, after the vowel characters a, e, ё, и, о, у, ы, э, ю, and я,
                    and after й, ъ, and ь. In all other instances, it should be romanized ё. When the dieresis is not shown, the character may still be
                    romanized in the preceding manner or, alternatively, in accordance with note 1.}
                    if (i > 1) and InternalCheckInRange(
                                     ord(LCyrillicWideStr[i-1]),[$0410 {А}, $0430 {а},
                                     $0415 {Е}, $0435 {е},
                                     $0401 {Ё}, $0451 {ё},
                                     $0418 {И}, $0438 {и},
                                     $041E {О}, $043E {о},
                                     $0423 {У}, $0443 {у},
                                     $042B {Ы}, $044B {ы},
                                     $042D {Э}, $044D {э},
                                     $042E {Ю}, $044E {ю},
                                     $042F {Я}, $044F {я},
                                     $0419 {Й}, $0439 {й},
                                     $042A {Ъ}, $044A {ъ},
                                     $042C {Ь}, $044C {ь}]) then InternalAddCharsToResult([$0079, $00EB]) {yë}
                    else InternalAddCharsToResult([$00EB]); {ë}
                  end;
      $0416 {Ж} : InternalAddCharsToResult([$005A,$0068]); {Zh}
      $0436 {ж} : InternalAddCharsToResult([$007A,$0068]); {zh}
      $0417 {З} : InternalAddCharsToResult([$005A]); {Z}
      $0437 {з} : InternalAddCharsToResult([$007A]); {z}
      $0418 {И} : InternalAddCharsToResult([$0049]); {I}
      $0438 {и} : InternalAddCharsToResult([$0069]); {i}
      $0419 {Й} : InternalAddCharsToResult([$0059]); {Y}
      $0439 {й} : InternalAddCharsToResult([$0079]); {y}
      $041A {К} : InternalAddCharsToResult([$004B]); {K}
      $043A {к} : InternalAddCharsToResult([$006B]); {k}
      $041B {Л} : InternalAddCharsToResult([$004C]); {L}
      $043B {л} : InternalAddCharsToResult([$006C]); {l}
      $041C {М} : InternalAddCharsToResult([$004D]); {M}
      $043C {м} : InternalAddCharsToResult([$006D]); {m}
      $041D {Н} : InternalAddCharsToResult([$004E]); {N}
      $043D {н} : InternalAddCharsToResult([$006E]); {n}
      $041E {О} : InternalAddCharsToResult([$004F]); {O}
      $043E {о} : InternalAddCharsToResult([$006F]); {o}
      $041F {П} : InternalAddCharsToResult([$0050]); {P}
      $043F {п} : InternalAddCharsToResult([$0070]); {p}
      $0420 {Р} : InternalAddCharsToResult([$0052]); {R}
      $0440 {р} : InternalAddCharsToResult([$0072]); {r}
      $0421 {С} : InternalAddCharsToResult([$0053]); {S}
      $0441 {с} : InternalAddCharsToResult([$0073]); {s}
      $0422 {Т} : InternalAddCharsToResult([$0054]); {T}
      $0442 {т} : InternalAddCharsToResult([$0074]); {t}
      $0423 {У} : InternalAddCharsToResult([$0055]); {U}
      $0443 {у} : InternalAddCharsToResult([$0075]); {u}
      $0424 {Ф} : InternalAddCharsToResult([$0046]); {F}
      $0444 {ф} : InternalAddCharsToResult([$0066]); {f}
      $0425 {Х} : InternalAddCharsToResult([$004B, $0068]); {Kh}
      $0445 {х} : InternalAddCharsToResult([$006B, $0068]); {kh}
      $0426 {Ц} : InternalAddCharsToResult([$0054, $0073]); {Ts}
      $0446 {ц} : InternalAddCharsToResult([$0074, $0073]); {ts}
      $0427 {Ч} : InternalAddCharsToResult([$0043, $0068]); {Ch}
      $0447 {ч} : InternalAddCharsToResult([$0063, $0068]); {ch}
      $0428 {Ш} : InternalAddCharsToResult([$0053, $0068]); {Sh}
      $0448 {ш} : InternalAddCharsToResult([$0073, $0068]); {sh}
      $0429 {Щ} : InternalAddCharsToResult([$0053, $0068, $0063, $0068]); {Shch}
      $0449 {щ} : InternalAddCharsToResult([$0073, $0068, $0063, $0068]); {shch}
      $042A {Ъ} : InternalAddCharsToResult([$0022]); {"}
      $044A {ъ} : InternalAddCharsToResult([$0022]); {"}
      $042B {Ы} : InternalAddCharsToResult([$0059]); {Y}
      $044B {ы} : InternalAddCharsToResult([$0079]); {y}
      $042C {Ь} : InternalAddCharsToResult([$0027]); {'}
      $044C {ь} : InternalAddCharsToResult([$0027]); {'}
      $042D {Э} : InternalAddCharsToResult([$0045]); {E}
      $044D {э} : InternalAddCharsToResult([$0065]); {e}
      $042E {Ю} : InternalAddCharsToResult([$0059, $0075]); {Yu}
      $044E {ю} : InternalAddCharsToResult([$0079, $0075]); {yu}
      $042F {Я} : InternalAddCharsToResult([$0059, $0061]); {Ya}
      $044F {я} : InternalAddCharsToResult([$0079, $0061]); {ya}
      else InternalAddCharsToResult([j]);
    end;
  end;
  SetLength(LLatinWideStr,LLatinWideStrCurrentPos);
  Result := AnsiString(LLatinWideStr);
End;

{**********************}
{$IF defined(MSWINDOWS)}
function AlUTF8Check(const S: AnsiString): Boolean;
begin
  if S = '' then exit(true);
  Result := MultiByteToWideChar(
              CP_UTF8, //UINT CodePage)
              8{8=MB_ERR_INVALID_CHARS that is not defined in d2007}, // DWORD dwFlags
              PAnsiChar(S), // LPCSTR lpMultiByteStr,
              length(S), // int cbMultiByte
              nil, // LPWSTR lpWideCharStr
              0) > 0; // int cchWideChar
end;
{$ENDIF}

{*************************************************************************}
function AlUTF8DetectBOM(const P: PansiChar; const Size: Integer): Boolean;
var Q: PansiChar;
begin
  Result := False;
  if Assigned(P) and (Size >= 3) and (P^ = #$EF) then begin
    Q := P;
    Inc(Q);
    if Q^ = #$BB then begin
      Inc(Q);
      if Q^ = #$BF then Result := True;
    end;
  end;
end;

{********************************************************}
function AlUTF8removeBOM(const S: AnsiString): AnsiString;
begin
  if AlUTF8DetectBOM(PAnsiChar(S), length(S)) then result := AlCopyStr(S,length(cAlUTF8BOM) + 1,Maxint)
  else Result := S;
end;

{*******************************************************}
// determine the number of bytes that follow a lead UTF-8
// character (including the lead byte). UTF8CharLength
// always returns 1, if the given character is not a valid
// UTF-8 lead byte.
function ALUTF8CharSize(Lead: AnsiChar; out IsValid: Boolean): Integer;
begin
  IsValid := True;
  case Lead of
    #$00..#$7F: Result := 1; //
    #$C2..#$DF: Result := 2; // 110x xxxx C0 - DF
    #$E0..#$EF: Result := 3; // 1110 xxxx E0 - EF
    #$F0..#$F7: Result := 4; // 1111 0xxx F0 - F7 // outside traditional UNICODE
    #$F8..#$FB: Result := 5; // 1111 10xx F8 - FB // outside UTF-16
    #$FC..#$FD: Result := 6; // 1111 110x FC - FD // outside UTF-16
    else begin
      IsValid := False;
      Result := 1; // Illegal leading character.
    end;
  end;
end;

{***********************************************}
function ALUTF8CharSize(Lead: AnsiChar): Integer;
var LIsValid: Boolean;
begin
  result := ALUTF8CharSize(Lead, LIsValid);
end;

{*****************************************}
// return how many char (not byte) are in S
function ALUTF8CharCount(const S: AnsiString): Integer;
var P, L : Integer;
begin
  Result := 0;
  L := length(s);
  P := 1;
  While P <= L do begin
    Inc(P, ALUTF8CharSize(S[P]));
    Inc(Result);
  end;
end;

{**************************************}
// Trunc a AnsiString to max count bytes
Function ALUTF8ByteTrunc(const s:AnsiString; const Count: Integer): AnsiString;
var L, P, C: Integer;
begin
  L := Length(S);
  If (L = 0) or (Count >= L) then Begin
    Result := S;
    Exit;
  end;

  P := 1;
  While P <= Count do begin
    C := ALUTF8CharSize(S[P]);
    if P + C - 1 > Count then break;
    inc(P,C);
  end;

  Result := ALCopyStr(S,1,P-1);
end;

{*****************************************}
// Trunc a AnsiString to count unicode char
Function ALUTF8CharTrunc(const s:AnsiString; const Count: Integer): AnsiString;
var L, P, C: Integer;
begin
  L := Length(S);
  If (L = 0) or (Count >= L) then Begin
    Result := S;
    Exit;
  end;

  P := 1;
  c := 0;
  While P <= L do begin
    Inc(P, ALUTF8CharSize(S[P]));
    Inc(c);
    if c >= count then break;
  end;

  Result := ALCopyStr(S,1,P-1);
end;

{*************************}
Function ALUTF8CharToUtf16(
           const S: AnsiString;
           const AIndex: integer;
           out AUTF8CharSize: integer;
           out AUTF16HighSurrogate: Char;
           out AUTF16lowSurrogate: Char): boolean;
begin
  var LIsValid: Boolean;
  AUTF8CharSize := ALUTF8CharSize(S[AIndex], LIsValid);
  if LIsValid and (AIndex + AUTF8CharSize - 1 <= high(s)) then begin
    case AUTF8CharSize of
      1: begin
           AUTF16HighSurrogate := Char(byte(S[AIndex]) and $7F);
           AUTF16lowSurrogate := #0;
           exit(true);
         end;
      2: begin
           AUTF16HighSurrogate := char(
                                    (UInt16(byte(S[AIndex]) and $1F) shl 6) or
                                    (UInt16(byte(S[AIndex + 1]) and $3F)));
           AUTF16lowSurrogate := #0;
           exit(true);
         end;
      3: begin
           AUTF16HighSurrogate := char(
                                    (UInt16(byte(S[AIndex]) and $0F) shl 12) or
                                    (UInt16(byte(S[AIndex + 1]) and $3F) shl 6) or
                                    (UInt16(byte(S[AIndex + 2]) and $3F)));
           AUTF16lowSurrogate := #0;
           exit(true);
         end;
      4: begin
           Var LCP := (UInt32(byte(S[AIndex]) and $07) shl 18) or
                      (UInt32(byte(S[AIndex + 1]) and $3F) shl 12) or
                      (UInt32(byte(S[AIndex + 2]) and $3F) shl 6) or
                      (UInt32(byte(S[AIndex + 3]) and $3F));
           LCP := LCP - $10000;
           AUTF16HighSurrogate := char($D800 + UInt16((LCP shr 10) and $3FF));
           AUTF16lowSurrogate := char($DC00 + UInt16(LCP and $3FF));
           exit(true);
         end;
    end;
  end;
  //47U+FFFD (decimal 65533) is the "replacement character". When a decoder
  //encounters an invalid sequence of bytes, it may (depending on its configuration)
  //substitute � for the corrupt sequence and continue.
  AUTF16HighSurrogate := Char($FFFD);
  AUTF16lowSurrogate := #0;
  result := false;
end;

{*******************************************************************************}
Function ALUTF8Encode(const S: RawByteString; const aCodePage: Word): AnsiString;
var TmpS: RawByteString;
begin
  TmpS := S;
  SetCodePage(TmpS, aCodePage, False);
  result := UTF8Encode(String(TmpS));
end;

{******************************************************************************}
Function ALStringDecode(const S: AnsiString; const aCodePage: Word): AnsiString;
begin
  result := S;
  SetCodePage(RawByteString(Result), aCodePage, true);
end;

{***************************************************************}
Function ALGetCodePageFromCharSetName(Acharset:AnsiString): Word;
Var LEncoding: Tencoding;
begin
  Try

    if Acharset = '' then begin
      result := 0; // Default ansi code page
      exit;
    end;

    LEncoding := Tencoding.GetEncoding(String(Acharset));
    Try
      Result := LEncoding.CodePage;
    Finally
      LEncoding.Free;
    end;

  Except
    Result := 0; // Default ansi code page
  end;
end;

{**********************}
{$IF defined(MSWINDOWS)}
Function ALGetCodePageFromLCID(const aLCID:Integer): Word;
var
  Buffer: array [0..6] of AnsiChar;
begin
  GetLocaleInfoA(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, Length(Buffer));
  Result:= ALStrToIntDef(Buffer, 0);
end;
{$ENDIF}

{****************************}
function ALExtractExpressionA(
           const S: AnsiString;
           const OpenChar, CloseChar: AnsiChar; // ex: '(' and ')'
           Const QuoteChars: Array of ansiChar; // ex: ['''', '"']
           Const EscapeQuoteChar: ansiChar; // ex: '\' or #0 to ignore
           var StartPos: integer;
           var EndPos: integer): boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _IsQuote(aChar: ansiChar): Boolean;
  var i: integer;
  begin
    result := False;
    for I := Low(QuoteChars) to High(QuoteChars) do
      if aChar = QuoteChars[i] then begin
        result := true;
        break;
      end;
    if (result) and
       (EscapeQuoteChar <> #0) and
       (S[EndPos - 1] = EscapeQuoteChar) then result := False;
  end;

var
  LCurInQuote: boolean;
  LCurQuoteChar: AnsiChar;
  LOpenCount: integer;

begin
  result := false;
  if StartPos <= 0 then StartPos := 1;
  while (StartPos <= length(S)) and
        (s[StartPos] <> OpenChar) do inc(StartPos);
  if StartPos > length(S) then exit;
  LOpenCount := 1;
  LCurInQuote := False;
  LCurQuoteChar := #0;
  EndPos := StartPos + 1;
  while (EndPos <= length(S)) and
        (LOpenCount > 0) do begin
    if _IsQuote(s[EndPos]) then begin
      if LCurInQuote then begin
        if (s[EndPos] = LCurQuoteChar) then LCurInQuote := False
      end
      else begin
        LCurInQuote := True;
        LCurQuoteChar := s[EndPos];
      end;
    end
    else if not LCurInQuote then begin
      if s[EndPos] = OpenChar then inc(LOpenCount)
      else if s[EndPos] = CloseChar then dec(LOpenCount);
    end;
    if LOpenCount <> 0 then inc(EndPos);
  end;
  result := EndPos <= length(S);
end;

{*********************************************************}
function  ALHTTPEncode(const AStr: AnsiString): AnsiString;
// The NoConversion set contains characters as specificed in RFC 1738 and
// should not be modified unless the standard changes.
const
  NoConversion = ['A'..'Z','a'..'z','*','@','.','_','-',
                  '0'..'9','$','!','''','(',')'];
var
  Sp, Rp: PAnsiChar;
begin
  SetLength(Result, Length(AStr) * 3);
  Sp := PAnsiChar(AStr);
  Rp := PAnsiChar(Result);
  while Sp^ <> #0 do
  begin
    if Sp^ in NoConversion then
      Rp^ := Sp^
    else
      if Sp^ = ' ' then
        Rp^ := '+'
      else
      begin
        System.AnsiStrings.FormatBuf(Rp^, 3, AnsiString('%%%.2x'), 6, [Ord(Sp^)]);
        Inc(Rp,2);
      end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PAnsiChar(Result));
end;

{*************************************************}
function  ALHTTPEncode(const AStr: String): String;
// The NoConversion set contains characters as specificed in RFC 1738 and
// should not be modified unless the standard changes.
const
  NoConversion = [Ord('A')..Ord('Z'),Ord('a')..Ord('z'),Ord('*'),Ord('@'),Ord('.'),Ord('_'),Ord('-'),
                  Ord('0')..Ord('9'),Ord('$'),Ord('!'),Ord(''''),Ord('('),Ord(')')];
var
  Sb: Tbytes;
  Rp: PChar;
  ln: integer;
  i: integer;
begin
  Sb := Tencoding.UTF8.GetBytes(aStr);
  ln := length(Sb);
  SetLength(Result, ln * 3);
  Rp := PChar(Result);
  i := 0;
  while i <= ln - 1 do
  begin
    if Sb[i] in NoConversion then
      Rp^ := Char(Sb[i])
    else
      if Sb[i] = Ord(' ') then
        Rp^ := '+'
      else
      begin
        FormatBuf(Rp, 3, String('%%%.2x'), 6, [Sb[i]]);
        Inc(Rp,2);
      end;
    Inc(Rp);
    Inc(i);
  end;
  SetLength(Result, Rp - PChar(Result));
end;

{************************************************************}
//the difference between this function and the delphi function
//HttpApp.HttpDecode is that this function will not raise any
//error (EConvertError) when the url will contain % that
//are not encoded
function ALHTTPDecode(const AStr: AnsiString): AnsiString;
var Sp, Rp, Cp, Tp: PAnsiChar;
    int: integer;
    S: AnsiString;
begin
  SetLength(Result, Length(AStr));
  Sp := PAnsiChar(AStr);
  Rp := PAnsiChar(Result);
  while Sp^ <> #0 do begin
    case Sp^ of
      '+': Rp^ := ' ';
      '%': begin
             Tp := Sp;
             Inc(Sp);

             //escaped % (%%)
             if Sp^ = '%' then Rp^ := '%'

             // %<hex> encoded character
             else begin
               Cp := Sp;
               Inc(Sp);
               if (Cp^ <> #0) and (Sp^ <> #0) then begin
                 S := AnsiChar('$') + AnsiChar(Cp^) + AnsiChar(Sp^);
                 if ALTryStrToInt(s,int) then Rp^ := ansiChar(int)
                 else begin
                   Rp^ := '%';
                   Sp := Tp;
                 end;
               end
               else begin
                 Rp^ := '%';
                 Sp := Tp;
               end;
             end;
           end;
      else Rp^ := Sp^;
    end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PAnsiChar(Result));
end;

{************************************************************}
//the difference between this function and the delphi function
//HttpApp.HttpDecode is that this function will not raise any
//error (EConvertError) when the url will contain % that
//are not encoded
function ALHTTPDecode(const AStr: String): String;
var Rb: Tbytes;
    Sp, Cp, Tp: PChar;
    int: integer;
    S: String;
    i: integer;
begin
  SetLength(Rb, Length(AStr));
  Sp := PChar(AStr);
  i := 0;
  while Sp^ <> #0 do begin
    case Sp^ of
      '+': Rb[i] := ord(' ');
      '%': begin
             Tp := Sp;
             Inc(Sp);

             //escaped % (%%)
             if Sp^ = '%' then Rb[i] := ord('%')

             // %<hex> encoded character
             else begin
               Cp := Sp;
               Inc(Sp);
               if (Cp^ <> #0) and (Sp^ <> #0) then begin
                 S := Char('$') + Char(Cp^) + Char(Sp^);
                 if ALTryStrToInt(s,int) then Rb[i] := int
                 else begin
                   Rb[i] := ord('%');
                   Sp := Tp;
                 end;
               end
               else begin
                 Rb[i] := ord('%');
                 Sp := Tp;
               end;
             end;
           end;
      else Rb[i] := ord(Sp^);
    end;
    Inc(i);
    Inc(Sp);
  end;
  result := Tencoding.Utf8.GetString(Rb, 0{ByteIndex}, i{ByteCount});
end;

{********************************************************}
{Parses a multi-valued string into its constituent fields.
 ExtractHeaderFields is a general utility to parse multi-valued HTTP header strings into separate substrings.
 * Separators is a set of characters that are used to separate individual values within the multi-valued string.
 * WhiteSpace is a set of characters that are to be ignored when parsing the string.
 * Content is the multi-valued string to be parsed.
 * Strings is the TStrings object that receives the individual values that are parsed from Content.
 * StripQuotes determines whether the surrounding quotes are removed from the resulting items. When StripQuotes is true, surrounding quotes are removed
   before substrings are added to Strings.
 Note:	Characters contained in Separators or WhiteSpace are treated as part of a value substring if the substring is surrounded by single or double quote
 marks. HTTP escape characters are converted using the ALHTTPDecode function.}
procedure ALExtractHeaderFields(
            Separators,
            WhiteSpace,
            Quotes: TSysCharSet;
            Content: PAnsiChar;
            Strings: TALStringsA;
            HttpDecode: Boolean;
            StripQuotes: Boolean = False);

var Head, Tail: PAnsiChar;
    EOS, InQuote: Boolean;
    QuoteChar: AnsiChar;
    ExtractedField: AnsiString;
    SeparatorsWithQuotesAndNulChar: TSysCharSet;
    QuotesWithNulChar: TSysCharSet;

  {-------------------------------------------------------}
  //as i don't want to add the parameter namevalueseparator
  //to the function, we will stripquote only if the string end
  //with the quote or start with the quote
  //ex: "name"="value"  =>  name=value
  //ex: "name"=value    =>  name=value
  //ex: name="value"    =>  name=value
  function DoStripQuotes(const S: AnsiString): AnsiString;
  var I: Integer;
      StripQuoteChar: AnsiChar;
      canStripQuotesOnLeftSide: boolean;
  begin
    Result := S;
    if StripQuotes then begin

      canStripQuotesOnLeftSide := True;
      if (length(result) > 0) and (result[length(result)] in quotes) then begin
        StripQuoteChar := result[length(result)];
        Delete(Result, length(result), 1);
        i := Length(Result);
        while i > 0 do begin
          if (Result[I] = StripQuoteChar) then begin
            Delete(Result, I, 1);
            canStripQuotesOnLeftSide := i > 1;
            break;
          end;
          dec(i);
        end;
      end;

      if (canStripQuotesOnLeftSide) and (length(result) > 0) and (result[1] in quotes) then begin
        StripQuoteChar := result[1];
        Delete(Result, 1, 1);
        i := 1;
        while i <= Length(Result) do begin
          if (Result[I] = StripQuoteChar) then begin
            Delete(Result, I, 1);
            break;
          end;
          inc(i);
        end;
      end;

    end;
  end;

Begin
  if (Content = nil) or (Content^ = #0) then Exit;
  SeparatorsWithQuotesAndNulChar := Separators + Quotes + [#0];
  QuotesWithNulChar := Quotes + [#0];
  Tail := Content;
  QuoteChar := #0;
  repeat
    while Tail^ in WhiteSpace do Inc(Tail);
    Head := Tail;
    InQuote := False;
    while True do begin
      while (InQuote and not (Tail^ in QuotesWithNulChar)) or not (Tail^ in SeparatorsWithQuotesAndNulChar) do Inc(Tail);
      if Tail^ in Quotes then begin
        if (QuoteChar <> #0) and (QuoteChar = Tail^) then QuoteChar := #0
        else If QuoteChar = #0 then QuoteChar := Tail^;
        InQuote := QuoteChar <> #0;
        Inc(Tail);
      end
      else Break;
    end;
    EOS := Tail^ = #0;
    if Head^ <> #0 then begin
      SetString(ExtractedField, Head, Tail-Head);
      if HttpDecode then Strings.Add(ALHTTPDecode(DoStripQuotes(ExtractedField)))
      else Strings.Add(DoStripQuotes(ExtractedField));
    end;
    Inc(Tail);
  until EOS;
end;

{**************************************************************************************}
{same as ALExtractHeaderFields except the it take care or escaped quote (like '' or "")}
procedure ALExtractHeaderFieldsWithQuoteEscaped(
            Separators,
            WhiteSpace,
            Quotes: TSysCharSet;
            Content: PAnsiChar;
            Strings: TALStringsA;
            HttpDecode: Boolean;
            StripQuotes: Boolean = False);

var Head, Tail, NextTail: PAnsiChar;
    EOS, InQuote: Boolean;
    QuoteChar: AnsiChar;
    ExtractedField: AnsiString;
    SeparatorsWithQuotesAndNulChar: TSysCharSet;
    QuotesWithNulChar: TSysCharSet;

  {-------------------------------------------------------}
  //as i don't want to add the parameter namevalueseparator
  //to the function, we will stripquote only if the string end
  //with the quote or start with the quote
  //ex: "name"="value"  =>  name=value
  //ex: "name"=value    =>  name=value
  //ex: name="value"    =>  name=value
  function DoStripQuotes(const S: AnsiString): AnsiString;
  var I: Integer;
      StripQuoteChar: AnsiChar;
      canStripQuotesOnLeftSide: boolean;
  begin
    Result := S;
    if StripQuotes then begin

      canStripQuotesOnLeftSide := True;
      if (length(result) > 0) and (result[length(result)] in quotes) then begin
        StripQuoteChar := result[length(result)];
        Delete(Result, length(result), 1);
        i := Length(Result);
        while i > 0 do begin
          if (Result[I] = StripQuoteChar) then begin
            Delete(Result, I, 1);
            if (i > 1) and (Result[I-1] = StripQuoteChar) then dec(i)
            else begin
              canStripQuotesOnLeftSide := i > 1;
              break;
            end;
          end;
          dec(i);
        end;
      end;

      if (canStripQuotesOnLeftSide) and (length(result) > 0) and (result[1] in quotes) then begin
        StripQuoteChar := result[1];
        Delete(Result, 1, 1);
        i := 1;
        while i <= Length(Result) do begin
          if (Result[I] = StripQuoteChar) then begin
            Delete(Result, I, 1);
            if (i < Length(Result)) and (Result[I+1] = StripQuoteChar) then inc(i)
            else break;
          end;
          inc(i);
        end;
      end;

    end;
  end;

Begin
  if (Content = nil) or (Content^ = #0) then Exit;
  SeparatorsWithQuotesAndNulChar := Separators + Quotes + [#0];
  QuotesWithNulChar := Quotes + [#0];
  Tail := Content;
  QuoteChar := #0;
  repeat
    while Tail^ in WhiteSpace do Inc(Tail);
    Head := Tail;
    InQuote := False;
    while True do begin
      while (InQuote and not (Tail^ in QuotesWithNulChar)) or not (Tail^ in SeparatorsWithQuotesAndNulChar) do Inc(Tail);
      if Tail^ in Quotes then begin
        if (QuoteChar <> #0) and (QuoteChar = Tail^) then begin
          NextTail := Tail + 1;
          if NextTail^ = Tail^ then inc(tail)
          else QuoteChar := #0;
        end
        else If QuoteChar = #0 then QuoteChar := Tail^;
        InQuote := QuoteChar <> #0;
        Inc(Tail);
      end
      else Break;
    end;
    EOS := Tail^ = #0;
    if Head^ <> #0 then begin
      SetString(ExtractedField, Head, Tail-Head);
      if HttpDecode then Strings.Add(ALHTTPDecode(DoStripQuotes(ExtractedField)))
      else Strings.Add(DoStripQuotes(ExtractedField));
    end;
    Inc(Tail);
  until EOS;
end;

{**************************************************************************************}
{same as ALExtractHeaderFields except the it take care or escaped quote (like '' or "")}
{$ZEROBASEDSTRINGS OFF}
{$WARN SYMBOL_DEPRECATED OFF}
procedure ALExtractHeaderFieldsWithQuoteEscaped(
            Separators,
            WhiteSpace,
            Quotes: TSysCharSet;
            Content: PChar;
            Strings: TALStringsW;
            HttpDecode: Boolean;
            StripQuotes: Boolean = False);

var Head, Tail, NextTail: PChar;
    EOS, InQuote: Boolean;
    QuoteChar: Char;
    ExtractedField: String;
    SeparatorsWithQuotesAndNulChar: TSysCharSet;
    QuotesWithNulChar: TSysCharSet;

  {-------------------------------------------------------}
  //as i don't want to add the parameter namevalueseparator
  //to the function, we will stripquote only if the string end
  //with the quote or start with the quote
  //ex: "name"="value"  =>  name=value
  //ex: "name"=value    =>  name=value
  //ex: name="value"    =>  name=value
  function DoStripQuotes(const S: String): String;
  var I: Integer;
      StripQuoteChar: Char;
      canStripQuotesOnLeftSide: boolean;
  begin
    Result := S;
    if StripQuotes then begin

      canStripQuotesOnLeftSide := True;
      if (length(result) > 0) and charInSet(result[length(result)], quotes) then begin
        StripQuoteChar := result[length(result)];
        Delete(Result, length(result), 1);
        i := Length(Result);
        while i > 0 do begin
          if (Result[I] = StripQuoteChar) then begin
            Delete(Result, I, 1);
            if (i > 1) and (Result[I-1] = StripQuoteChar) then dec(i)
            else begin
              canStripQuotesOnLeftSide := i > 1;
              break;
            end;
          end;
          dec(i);
        end;
      end;

      if (canStripQuotesOnLeftSide) and (length(result) > 0) and charInSet(result[1], quotes) then begin
        StripQuoteChar := result[1];
        Delete(Result, 1, 1);
        i := 1;
        while i <= Length(Result) do begin
          if (Result[I] = StripQuoteChar) then begin
            Delete(Result, I, 1);
            if (i < Length(Result)) and (Result[I+1] = StripQuoteChar) then inc(i)
            else break;
          end;
          inc(i);
        end;
      end;

    end;
  end;

Begin
  if (Content = nil) or (Content^ = #0) then Exit;
  SeparatorsWithQuotesAndNulChar := Separators + Quotes + [#0];
  QuotesWithNulChar := Quotes + [#0];
  Tail := Content;
  QuoteChar := #0;
  repeat
    while charInSet(Tail^, WhiteSpace) do Inc(Tail);
    Head := Tail;
    InQuote := False;
    while True do begin
      while (InQuote and not charInSet(Tail^, QuotesWithNulChar)) or not charInSet(Tail^, SeparatorsWithQuotesAndNulChar) do Inc(Tail);
      if charInSet(Tail^, Quotes) then begin
        if (QuoteChar <> #0) and (QuoteChar = Tail^) then begin
          NextTail := Tail + 1;
          if NextTail^ = Tail^ then inc(tail)
          else QuoteChar := #0;
        end
        else If QuoteChar = #0 then QuoteChar := Tail^;
        InQuote := QuoteChar <> #0;
        Inc(Tail);
      end
      else Break;
    end;
    EOS := Tail^ = #0;
    if Head^ <> #0 then begin
      SetString(ExtractedField, Head, Tail-Head);
      if HttpDecode then Strings.Add(ALHTTPDecode(DoStripQuotes(ExtractedField)))
      else Strings.Add(DoStripQuotes(ExtractedField));
    end;
    Inc(Tail);
  until EOS;
end;
{$WARN SYMBOL_DEPRECATED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{************************************}
constructor TALPrecompiledTagA.Create;
begin
  fTagString := '';
  fTagParams := TALStringListA.Create;
  TALStringListA(fTagParams).Duplicates := dupIgnore;
end;

{************************************}
destructor TALPrecompiledTagA.Destroy;
begin
  AlFreeAndNil(fTagParams);
  inherited;
end;

{****************************************************}
function TALPrecompiledTagA.GetTagParams: TALStringsA;
begin
  result := fTagParams;
end;

{***********************************}
function ALFastTagReplacePrecompileA(
           Const SourceString, TagStart, TagEnd: AnsiString;
           PrecompileProc: TALHandleTagPrecompileFunctA;
           StripParamQuotes: Boolean; // useless if PrecompileProc is provided
           Context: Pointer;
           TagsContainer: TObjectList; // just a container where all the PrecompiledTag will be store. must free all the PrecompiledTag at the end of the application
           Const flags: TReplaceFlags=[]): AnsiString; // rfreplaceall is ignored here, only rfIgnoreCase is matter

var ReplaceString: AnsiString;
    TagEndFirstChar, TagEndFirstCharLower, TagEndFirstCharUpper: AnsiChar;
    TokenStr, ParamStr: AnsiString;
    ParamList: TALStringListA;
    TagStartLength: integer;
    TagEndLength: integer;
    SourceStringLength: Integer;
    InDoubleQuote: Boolean;
    InsingleQuote: Boolean;
    SourceCurrentPos: integer;
    ResultCurrentPos: integer;
    ResultCurrentLength: integer;
    PrecompiledTag: TALBasePrecompiledTagA;
    IgnoreCase: Boolean;
    PosFunct: Function(const SubStr, S: AnsiString; const Offset: Integer = 1): Integer;
    T1,T2: Integer;
    I: integer;

Const ResultBuffSize: integer = 16384;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Function _ExtractTokenStr: AnsiString;
    var x: Integer;
    Begin
      X := 1;
      while (x <= length(ReplaceString)) and
            (not (ReplaceString[x] in [' ', #9, #13, #10])) do inc(x);
      if x > length(ReplaceString) then Result := ReplaceString
      else Result := AlcopyStr(ReplaceString,1,x-1);
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Function _ExtractParamsStr(const TokenStr: ansiString): AnsiString;
    Begin
      Result := ALTrim(AlcopyStr(ReplaceString,length(TokenStr) + 1, MaxInt));
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _MoveStr2Result(const aSourceString: AnsiString; aStart, aLength: Integer);
    var LSourceStringLn: Integer;
    begin
      LSourceStringLn := Length(aSourceString);
      If (aStart < 1) then aStart := 1;

      if (LSourceStringLn=0) or
         (aLength < 1) or
         (aStart > LSourceStringLn) then Exit;

      if aLength > LSourceStringLn - (aStart - 1) then aLength := LSourceStringLn - (aStart-1);

      If aLength + ResultCurrentPos - 1 > ResultCurrentLength then begin
        ResultCurrentLength := ResultCurrentLength + aLength + ResultBuffSize;
        SetLength(Result, ResultCurrentLength);
      end;
      AlMove(pbyte(aSourceString)[aStart-1], pbyte(Result)[ResultCurrentPos-1], aLength);
      ResultCurrentPos := ResultCurrentPos + aLength;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    function  _ObjAddressToStr(Const Obj: Tobject): AnsiString;
    begin
      result := ALIntToHexA(NativeInt(Obj), sizeof(pointer) * 2);
    end;

begin
  if (SourceString = '') or (TagStart = '') or (TagEnd = '') then begin
    Result := SourceString;
    Exit;
  end;

  IgnoreCase := rfIgnoreCase in flags;
  If IgnoreCase then PosFunct := ALPosIgnoreCaseA
  Else PosFunct := ALPosA;

  SourceCurrentPos := 1;
  T1 := PosFunct(TagStart,SourceString,SourceCurrentPos);
  if T1 <= 0 then begin
    result := SourceString;
    exit;
  end;

  SourceStringLength := length(SourceString);
  ResultCurrentLength := SourceStringLength;
  SetLength(Result,ResultCurrentLength);
  ResultCurrentPos := 1;
  TagStartLength := Length(TagStart);
  TagEndLength := Length(TagEnd);
  TagEndFirstChar := TagEnd[1];
  TagEndFirstCharLower := ALLoCase(TagEnd[1]);
  TagEndFirstCharUpper := ALUpCase(TagEnd[1]);

  T2 := T1 + TagStartLength;
  If (T1 > 0) and (T2 <= SourceStringLength) then begin
    InDoubleQuote := False;
    InsingleQuote := False;
    While (T2 <= SourceStringLength) and
          (InDoubleQuote or
           InSingleQuote or
           (IgnoreCase and (SourceString[T2] <> TagEndFirstCharLower) and (SourceString[T2] <> TagEndFirstCharUpper)) or
           ((not IgnoreCase) and (SourceString[T2] <> TagEndFirstChar)) or
           ((TagEndLength > 1) and (PosFunct(TagEnd,AlCopyStr(SourceString,T2,TagEndLength),1) <> 1))) do begin
      If SourceString[T2] = '"' then InDoubleQuote := (not InDoubleQuote) and (not InSingleQuote)
      else If SourceString[T2] = '''' then InSingleQuote := (not InSingleQuote) and (not InDoubleQuote);
      inc(T2);
    end;
    if (T2 > SourceStringLength) then T2 := 0;
  end;


  While (T1 > 0) and (T2 > T1) do begin
    ReplaceString := AlCopyStr(SourceString,T1 + TagStartLength,T2 - T1 - TagStartLength);
    T2 := T2 + TagEndLength;

    If assigned(PrecompileProc) then begin
      TokenStr := _ExtractTokenStr;
      ParamStr := _ExtractParamsStr(TokenStr);
      ParamList := TALStringListA.Create;
      try
        ParamList.Duplicates := dupIgnore;
        ALExtractHeaderFieldsWithQuoteEscaped(
          [' ', #9, #13, #10],
          [' ', #9, #13, #10],
          ['"', ''''],
          PAnsiChar(ParamStr),
          ParamList,
          False,
          StripParamQuotes);

        T2 := T2 - T1;
        PrecompiledTag := PrecompileProc(TokenStr, ParamList, Context, SourceString, T1, T2);
        T2 := T2 + T1;
        if assigned(PrecompiledTag) then begin
          TagsContainer.Add(PrecompiledTag);
          ReplaceString := TagStart + #2{start of text} + _ObjAddressToStr(PrecompiledTag) + #3{end of text} + TagEnd;
        end
        else ReplaceString := '';
      finally
        ParamList.Free;
      end;
    end
    else begin
      PrecompiledTag := TALPrecompiledTagA.Create;
      try
        PrecompiledTag.TagString := _ExtractTokenStr;
        ParamStr := _ExtractParamsStr(PrecompiledTag.TagString);
        ALExtractHeaderFieldsWithQuoteEscaped(
          [' ', #9, #13, #10],
          [' ', #9, #13, #10],
          ['"', ''''],
          PAnsiChar(ParamStr),
          PrecompiledTag.TagParams,
          False,
          StripParamQuotes);
        TagsContainer.Add(PrecompiledTag);
        ReplaceString := TagStart + #2{start of text} + _ObjAddressToStr(PrecompiledTag) + #3{end of text} + TagEnd;
      except
        AlFreeAndNil(PrecompiledTag);
        raise;
      end;
      for I := 0 to PrecompiledTag.TagParams.Count - 1 do
        PrecompiledTag.TagParams[i] := ALFastTagReplacePrecompileA(
                                         PrecompiledTag.TagParams[i], //Const SourceString,
                                         TagStart,
                                         TagEnd,
                                         PrecompileProc,
                                         StripParamQuotes,
                                         Context,
                                         TagsContainer,
                                         flags);
      PrecompiledTag.TagString := ALFastTagReplacePrecompileA(
                                    PrecompiledTag.TagString, //Const SourceString,
                                    TagStart,
                                    TagEnd,
                                    PrecompileProc,
                                    StripParamQuotes,
                                    Context,
                                    TagsContainer,
                                    flags);
    end;

    _MoveStr2Result(SourceString,SourceCurrentPos,T1 - SourceCurrentPos);
    _MoveStr2Result(ReplaceString,1,length(ReplaceString));
    SourceCurrentPos := T2;

    T1 := PosFunct(TagStart,SourceString,SourceCurrentPos);
    T2 := T1 + TagStartLength;
    If (T1 > 0) and (T2 <= SourceStringLength) then begin
      InDoubleQuote := False;
      InsingleQuote := False;
      While (T2 <= SourceStringLength) and
            (InDoubleQuote or
             InSingleQuote or
             (IgnoreCase and (SourceString[T2] <> TagEndFirstCharLower) and (SourceString[T2] <> TagEndFirstCharUpper)) or
             ((not IgnoreCase) and (SourceString[T2] <> TagEndFirstChar)) or
             ((TagEndLength > 1) and (PosFunct(TagEnd,AlCopyStr(SourceString,T2,TagEndLength),1) <> 1))) do begin
        If SourceString[T2] = '"' then InDoubleQuote := (not InDoubleQuote) and (not InSingleQuote)
        else If SourceString[T2] = '''' then InSingleQuote := (not InSingleQuote) and (not InDoubleQuote);
        inc(T2);
      end;
      if (T2 > SourceStringLength) then T2 := 0;
    end;
  end;

  _MoveStr2Result(SourceString,SourceCurrentPos,maxint);
  SetLength(Result,ResultCurrentPos-1);
end;

{*************************}
function ALFastTagReplaceA(
           Const SourceString, TagStart, TagEnd: AnsiString;
           ReplaceProc: TALHandleTagFunctA;
           ReplaceExtendedProc: TALHandleTagExtendedfunctA;
           StripParamQuotes: Boolean;
           Flags: TReplaceFlags;
           Context: Pointer;
           TagParamsClass: TALTagParamsClassA;
           const TagReplaceProcResult: Boolean = False): AnsiString; overload;

var ReplaceString: AnsiString;
    TagEndFirstChar, TagEndFirstCharLower, TagEndFirstCharUpper: AnsiChar;
    TokenStr, ParamStr: AnsiString;
    ParamList: TALStringsA;
    TagStartLength: integer;
    TagEndLength: integer;
    SourceStringLength: Integer;
    InDoubleQuote: Boolean;
    InsingleQuote: Boolean;
    TagHandled: Boolean;
    SourceCurrentPos: integer;
    ResultCurrentPos: integer;
    ResultCurrentLength: integer;
    PrecompiledTag: TALBasePrecompiledTagA;
    InPrecompiledTag: Boolean;
    IgnoreCase: Boolean;
    pSize: integer;
    PosFunct: Function(const SubStr, S: AnsiString; const Offset: Integer = 1): Integer;
    T1,T2: Integer;

Const ResultBuffSize: integer = 16384;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Function _ExtractTokenStr: AnsiString;
    var x: Integer;
    Begin
      X := 1;
      while (x <= length(ReplaceString)) and
            (not (ReplaceString[x] in [' ', #9, #13, #10])) do inc(x);
      if x > length(ReplaceString) then Result := ReplaceString
      else Result := AlcopyStr(ReplaceString,1,x-1);
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Function _ExtractParamsStr: AnsiString;
    Begin
      Result := ALTrim(AlcopyStr(ReplaceString,length(TokenStr) + 1, MaxInt));
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _MoveStr2Result(const aSourceString: AnsiString; aStart, aLength: Integer);
    var LSourceStringLn: Integer;
    begin
      LSourceStringLn := Length(aSourceString);
      If (aStart < 1) then aStart := 1;

      if (LSourceStringLn=0) or
         (aLength < 1) or
         (aStart > LSourceStringLn) then Exit;

      if aLength > LSourceStringLn - (aStart - 1) then aLength := LSourceStringLn - (aStart-1);

      If aLength + ResultCurrentPos - 1 > ResultCurrentLength then begin
        ResultCurrentLength := ResultCurrentLength + aLength + ResultBuffSize;
        SetLength(Result, ResultCurrentLength);
      end;
      AlMove(pbyte(aSourceString)[aStart-1], pbyte(Result)[ResultCurrentPos-1], aLength);
      ResultCurrentPos := ResultCurrentPos + aLength;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    function _HexToInt(const aSourceString: ansistring; Start, Length: Integer): integer;
    begin
      Result := 0;
      for Start := start to start + length - 1 do
        case aSourceString[Start] of
          '0'..'9': Result := Result * 16 + Ord(aSourceString[Start]) - Ord('0');
          'A'..'F': Result := Result * 16 + Ord(aSourceString[Start]) - Ord('A') + 10;
        end;
    end;

begin
  if (SourceString = '') or (TagStart = '') or (TagEnd = '') then begin
    Result := SourceString;
    Exit;
  end;

  IgnoreCase := rfIgnoreCase in flags;
  If IgnoreCase then PosFunct := ALPosIgnoreCaseA
  Else PosFunct := ALPosA;

  SourceCurrentPos := 1;
  T1 := PosFunct(TagStart,SourceString,SourceCurrentPos);
  if T1 <= 0 then begin
    result := SourceString;
    exit;
  end;

  SourceStringLength := length(SourceString);
  ResultCurrentLength := SourceStringLength;
  SetLength(Result,ResultCurrentLength);
  ResultCurrentPos := 1;
  TagStartLength := Length(TagStart);
  TagEndLength := Length(TagEnd);
  TagEndFirstChar := TagEnd[1];
  TagEndFirstCharLower := ALLoCase(TagEnd[1]);
  TagEndFirstCharUpper := ALUpCase(TagEnd[1]);
  pSize := sizeOf(pointer) * 2;
  InPrecompiledTag := False; // to remove warning

  T2 := T1 + TagStartLength;
  If (T1 > 0) and (T2 <= SourceStringLength) then begin

    //we are in precompiled tag
    if (SourceString[T2] = #2) and
       ((T2 + pSize + 1 + TagEndLength) <= SourceStringLength) and
       (SourceString[T2 + pSize + 1] = #3) and
       ((IgnoreCase and ((SourceString[T2 + pSize + 2] = TagEndFirstCharLower) or (SourceString[T2 + pSize + 2] = TagEndFirstCharUpper))) or
        ((not IgnoreCase) and (SourceString[T2 + pSize + 2] = TagEndFirstChar))) and
       ((TagEndLength <= 1) or (PosFunct(TagEnd,AlCopyStr(SourceString,T2 + pSize + 2,TagEndLength),1) = 1)) then begin
      InPrecompiledTag := True;
      T2 := T2 + pSize + 1 + TagEndLength;
    end

    //else not precompiled tag
    else begin
      InDoubleQuote := False;
      InsingleQuote := False;
      While (T2 <= SourceStringLength) and
            (InDoubleQuote or
             InSingleQuote or
             (IgnoreCase and (SourceString[T2] <> TagEndFirstCharLower) and (SourceString[T2] <> TagEndFirstCharUpper)) or
             ((not IgnoreCase) and (SourceString[T2] <> TagEndFirstChar)) or
             ((TagEndLength > 1) and (PosFunct(TagEnd,AlCopyStr(SourceString,T2,TagEndLength),1) <> 1))) do begin
        If SourceString[T2] = '"' then InDoubleQuote := (not InDoubleQuote) and (not InSingleQuote)
        else If SourceString[T2] = '''' then InSingleQuote := (not InSingleQuote) and (not InDoubleQuote);
        inc(T2);
      end;
      if (T2 > SourceStringLength) then T2 := 0;
    end;

  end;


  While (T1 > 0) and (T2 > T1) do begin

    //we are in precompiled tag
    if InPrecompiledTag then begin
      PrecompiledTag := Pointer(_HexToInt(SourceString, T1 + TagStartLength+1, pSize));
      T2 := T2 + TagEndLength;
      if assigned(ReplaceExtendedProc) then begin
        T2 := T2 - T1;
        ReplaceString := ReplaceExtendedProc(PrecompiledTag.TagString, PrecompiledTag.TagParams, Context, TagHandled, SourceString, T1, T2);
        T2 := T2 + T1;
      end
      else ReplaceString := ReplaceProc(PrecompiledTag.TagString, PrecompiledTag.TagParams, Context, TagHandled);
    end

    //else not precompiled tag
    else begin
      ReplaceString := AlCopyStr(SourceString,T1 + TagStartLength,T2 - T1 - TagStartLength);
      T2 := T2 + TagEndLength;

      TagHandled := True;
      TokenStr := _ExtractTokenStr;
      ParamStr := _ExtractParamsStr;
      ParamList := TagParamsClass.Create;
      try
        ALExtractHeaderFieldsWithQuoteEscaped(
          [' ', #9, #13, #10],
          [' ', #9, #13, #10],
          ['"', ''''],
          PAnsiChar(ParamStr),
          ParamList,
          False,
          StripParamQuotes);
        if assigned(ReplaceExtendedProc) then begin
          T2 := T2 - T1;
          ReplaceString := ReplaceExtendedProc(TokenStr, ParamList, Context, TagHandled, SourceString, T1, T2);
          T2 := T2 + T1;
        end
        else ReplaceString := ReplaceProc(TokenStr, ParamList, Context, TagHandled);
      finally
        AlFreeAndNil(ParamList);
      end;
    end;

    if (TagHandled) and
       (TagReplaceProcResult) and
       (rfreplaceAll in flags) then ReplaceString := ALFastTagReplaceA(
                                                       ReplaceString,
                                                       TagStart,
                                                       TagEnd,
                                                       ReplaceProc,
                                                       ReplaceExtendedProc,
                                                       StripParamQuotes,
                                                       Flags,
                                                       Context,
                                                       TagParamsClass,
                                                       TagReplaceProcResult);

    If tagHandled then begin
      _MoveStr2Result(SourceString,SourceCurrentPos,T1 - SourceCurrentPos);
      _MoveStr2Result(ReplaceString,1,length(ReplaceString))
    end
    else _MoveStr2Result(SourceString,SourceCurrentPos,T2 - SourceCurrentPos);
    SourceCurrentPos := T2;

    If TagHandled and (not (rfreplaceAll in flags)) then Break;

    InPrecompiledTag := False;
    T1 := PosFunct(TagStart,SourceString,SourceCurrentPos);
    T2 := T1 + TagStartLength;
    If (T1 > 0) and (T2 <= SourceStringLength) then begin

      //we are in precompiled tag
      if (SourceString[T2] = #2) and
         ((T2 + pSize + 1 + TagEndLength) <= SourceStringLength) and
         (SourceString[T2 + pSize + 1] = #3) and
         ((IgnoreCase and ((SourceString[T2 + pSize + 2] = TagEndFirstCharLower) or (SourceString[T2 + pSize + 2] = TagEndFirstCharUpper))) or
          ((not IgnoreCase) and (SourceString[T2 + pSize + 2] = TagEndFirstChar))) and
         ((TagEndLength <= 1) or (PosFunct(TagEnd,AlCopyStr(SourceString,T2 + pSize + 2,TagEndLength),1) = 1)) then begin
        InPrecompiledTag := True;
        T2 := T2 + pSize + 1 + TagEndLength;
      end

      //else not precompiled tag
      else begin
        InDoubleQuote := False;
        InsingleQuote := False;
        While (T2 <= SourceStringLength) and
              (InDoubleQuote or
               InSingleQuote or
               (IgnoreCase and (SourceString[T2] <> TagEndFirstCharLower) and (SourceString[T2] <> TagEndFirstCharUpper)) or
               ((not IgnoreCase) and (SourceString[T2] <> TagEndFirstChar)) or
               ((TagEndLength > 1) and (PosFunct(TagEnd,AlCopyStr(SourceString,T2,TagEndLength),1) <> 1))) do begin
          If SourceString[T2] = '"' then InDoubleQuote := (not InDoubleQuote) and (not InSingleQuote)
          else If SourceString[T2] = '''' then InSingleQuote := (not InSingleQuote) and (not InDoubleQuote);
          inc(T2);
        end;
        if (T2 > SourceStringLength) then T2 := 0;
      end;

    end;
  end;

  _MoveStr2Result(SourceString,SourceCurrentPos,maxint);
  SetLength(Result,ResultCurrentPos-1);
end;

{*************************}
function ALFastTagReplaceA(
           const SourceString, TagStart, TagEnd: AnsiString;
           ReplaceProc: TALHandleTagFunctA;
           StripParamQuotes: Boolean;
           Context: Pointer;
           Const flags: TReplaceFlags=[rfreplaceall];
           const TagReplaceProcResult: Boolean = False): AnsiString;
Begin
  result := ALFastTagReplaceA(
              SourceString,
              TagStart,
              TagEnd,
              ReplaceProc,
              nil,
              StripParamQuotes,
              flags,
              Context,
              TALStringListA,
              TagReplaceProcResult);
end;

{*************************}
function ALFastTagReplaceA(
           const SourceString, TagStart, TagEnd: AnsiString;
           ReplaceExtendedProc: TALHandleTagExtendedfunctA;
           StripParamQuotes: Boolean;
           Context: Pointer;
           Const flags: TReplaceFlags=[rfreplaceall];
           const TagReplaceProcResult: Boolean = False): AnsiString;
Begin
  result := ALFastTagReplaceA(
              SourceString,
              TagStart,
              TagEnd,
              nil,
              ReplaceExtendedProc,
              StripParamQuotes,
              flags,
              Context,
              TALStringListA,
              TagReplaceProcResult);
end;

{********************************}
function ALFastTagReplaceWithFunc(
           const TagString: AnsiString;
           TagParams: TALStringsA;
           Context: pointer;
           Var Handled: Boolean): AnsiString;
begin
  Handled := true;
  result := AnsiString(Context);
end;

{*************************}
function ALFastTagReplaceA(
           const SourceString, TagStart, TagEnd: AnsiString;
           const ReplaceWith: AnsiString;
           const Flags: TReplaceFlags=[rfreplaceall]): AnsiString;
Begin
  Result := ALFastTagReplaceA(
              SourceString,
              TagStart,
              TagEnd,
              ALFastTagReplaceWithFunc,
              nil,
              True,
              flags,
              PAnsiChar(ReplaceWith),
              TALStringListA,
              false);
end;

{**************************************************}
//the problem with this function is that if you have
//<#mytagwww params="xxx"> and
//<#mytag params="xxx">
//then the ALExtractTagParamsA(str, '<#mytag', '>' ... ) will not work like we expect
//because it's will extract the params of the <#mytagwww
function ALExtractTagParamsA(
           Const SourceString, TagStart, TagEnd: AnsiString;
           StripParamQuotes: Boolean;
           TagParams: TALStringsA;
           IgnoreCase: Boolean): Boolean;

var ReplaceString: AnsiString;
    TagEndFirstChar, TagEndFirstCharLower, TagEndFirstCharUpper: AnsiChar;
    TokenStr, ParamStr: AnsiString;
    TagStartLength: integer;
    TagEndLength: integer;
    SourceStringLength: Integer;
    InDoubleQuote: Boolean;
    InsingleQuote: Boolean;
    PosFunct: Function(const SubStr, S: AnsiString; const Offset: Integer = 1): Integer;
    T1,T2: Integer;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Function _ExtractTokenStr: AnsiString;
    var x: Integer;
    Begin
      X := 1;
      while (x <= length(ReplaceString)) and
            (not (ReplaceString[x] in [' ', #9, #13, #10])) do inc(x);
      if x > length(ReplaceString) then Result := ReplaceString
      else Result := AlcopyStr(ReplaceString,1,x-1);
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    Function _ExtractParamsStr: AnsiString;
    Begin
      Result := ALTrim( AlcopyStr(ReplaceString,length(TokenStr) + 1, MaxInt) );
    end;

begin
  Result := False;
  if (SourceString = '') or (TagStart = '') or (TagEnd = '') then Exit;

  If IgnoreCase then PosFunct := ALPosIgnoreCaseA
  Else PosFunct := ALPosA;

  SourceStringLength := length(SourceString);
  TagStartLength := Length(TagStart);
  TagEndLength := Length(TagEnd);
  TagEndFirstChar := TagEnd[1];
  TagEndFirstCharLower := ALLowerCase(TagEnd[1])[1];
  TagEndFirstCharUpper := ALUpperCase(TagEnd[1])[1];

  T1 := PosFunct(TagStart,SourceString,1);
  T2 := T1 + TagStartLength;
  If (T1 > 0) and (T2 <= SourceStringLength) then begin
    InDoubleQuote := False;
    InsingleQuote := False;
    While (T2 <= SourceStringLength) and
          (InDoubleQuote or
           InSingleQuote or
           (IgnoreCase and (not (SourceString[T2] in [TagEndFirstCharLower, TagEndFirstCharUpper]))) or
           ((not IgnoreCase) and (SourceString[T2] <> TagEndFirstChar)) or
           (PosFunct(TagEnd,AlCopyStr(SourceString,T2,TagEndLength),1) <> 1)) do begin
      If SourceString[T2] = '"' then InDoubleQuote := (not InDoubleQuote) and (not InSingleQuote)
      else If SourceString[T2] = '''' then InSingleQuote := (not InSingleQuote) and (not InDoubleQuote);
      inc(T2);
    end;
    if (T2 > SourceStringLength) then T2 := 0;
  end;

  If (T1 > 0) and (T2 > T1) Then begin
    ReplaceString := AlCopyStr(SourceString,T1 + TagStartLength,T2 - T1 - TagStartLength);
    TokenStr := _ExtractTokenStr;
    ParamStr := _ExtractParamsStr;
    ALExtractHeaderFieldsWithQuoteEscaped(
      [' ', #9, #13, #10],
      [' ', #9, #13, #10],
      ['"', ''''],
      PAnsiChar(ParamStr),
      TagParams,
      False,
      StripParamQuotes);
    Result := True
  end;
end;

{********************}
// split the text like
// blablabla<#tag param="xxx">whouwhouwhou
// in a list of
// blablabla
// <#tag param="xxx">
// whouwhouwhou
Procedure ALSplitTextAndTagA(
            Const SourceString, TagStart, TagEnd: AnsiString;
            SplitTextAndTagLst: TALStringsA;
            IgnoreCase: Boolean);

var TagEndFirstChar, TagEndFirstCharLower, TagEndFirstCharUpper: AnsiChar;
    TagStartLength: integer;
    TagEndLength: integer;
    SourceStringLength: Integer;
    SourceCurrentPos: integer;
    InDoubleQuote: Boolean;
    InsingleQuote: Boolean;
    PosFunct: Function(const SubStr, S: AnsiString; const Offset: Integer = 1): Integer;
    T1,T2: Integer;

begin

  if (SourceString = '') or (TagStart = '') or (TagEnd = '') then begin
    if SourceString <> '' then SplitTextAndTagLst.Add(SourceString);
    Exit;
  end;

  If IgnoreCase then PosFunct := ALPosIgnoreCaseA
  Else PosFunct := ALPosA;

  SourceStringLength := length(SourceString);
  TagStartLength := Length(TagStart);
  TagEndLength := Length(TagEnd);
  TagEndFirstChar := TagEnd[1];
  TagEndFirstCharLower := ALLowerCase(TagEnd[1])[1];
  TagEndFirstCharUpper := ALUpperCase(TagEnd[1])[1];
  SourceCurrentPos := 1;

  T1 := PosFunct(TagStart,SourceString,SourceCurrentPos);
  T2 := T1 + TagStartLength;
  If (T1 > 0) and (T2 <= SourceStringLength) then begin
    InDoubleQuote := False;
    InsingleQuote := False;
    While (T2 <= SourceStringLength) and
          (InDoubleQuote or
           InSingleQuote or
           (IgnoreCase and (not (SourceString[T2] in [TagEndFirstCharLower, TagEndFirstCharUpper]))) or
           ((not IgnoreCase) and (SourceString[T2] <> TagEndFirstChar)) or
           (PosFunct(TagEnd,AlCopyStr(SourceString,T2,TagEndLength),1) <> 1)) do begin
      If SourceString[T2] = '"' then InDoubleQuote := (not InDoubleQuote) and (not InSingleQuote)
      else If SourceString[T2] = '''' then InSingleQuote := (not InSingleQuote) and (not InDoubleQuote);
      inc(T2);
    end;
    if (T2 > SourceStringLength) then T2 := 0;
  end;

  While (T1 > 0) and (T2 > T1) do begin
    SplitTextAndTagLst.AddObject(AlcopyStr(SourceString,SourceCurrentPos,T1 - SourceCurrentPos), pointer(0));
    SplitTextAndTagLst.AddObject(AlCopyStr(SourceString,T1,T2 - T1 + TagEndLength), pointer(1));

    SourceCurrentPos := T2 + TagEndLength;

    T1 := PosFunct(TagStart,SourceString,SourceCurrentPos);
    T2 := T1 + TagStartLength;
    If (T1 > 0) and (T2 <= SourceStringLength) then begin
      InDoubleQuote := False;
      InsingleQuote := False;
      While (T2 <= SourceStringLength) and
            (InDoubleQuote or
             InSingleQuote or
             (IgnoreCase and (not (SourceString[T2] in [TagEndFirstCharLower, TagEndFirstCharUpper]))) or
             ((not IgnoreCase) and (SourceString[T2] <> TagEndFirstChar)) or
             (PosFunct(TagEnd,AlCopyStr(SourceString,T2,TagEndLength),1) <> 1)) do begin
        If SourceString[T2] = '"' then InDoubleQuote := (not InDoubleQuote) and (not InSingleQuote)
        else If SourceString[T2] = '''' then InSingleQuote := (not InSingleQuote) and (not InDoubleQuote);
        inc(T2);
      end;
      if (T2 > SourceStringLength) then T2 := 0;
    end;
  end;

  SplitTextAndTagLst.AddObject(AlcopyStr(SourceString,SourceCurrentPos,maxint), pointer(0));

end;

{********************************}
Procedure _ALStringInitialization;
var I: integer;
begin

  //
  // Taken from https://github.com/synopse/mORMot.git
  // https://synopse.info
  // http://mormot.net
  //

  _Base64Encoding := nil;

  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://github.com/synopse/mORMot.git SynCommons.pas was not updated from References\mORMot\SynCommons.pas and adjust the IFDEF'}
  {$ENDIF}

  Fillchar(ConvertBase64ToBin,256,255); // invalid value set to -1
  for i := 0 to high(b64enc) do
    ConvertBase64ToBin[b64enc[i]] := i;
  ConvertBase64ToBin['='] := -2; // special value for '='

  //https://stackoverflow.com/questions/50590627/tformatsettings-createen-us-returns-different-settings-on-different-platform
  ALPosIgnoreCaseInitialiseLookupTable;
  ALDefaultFormatSettingsA := TALFormatSettingsA.Create('en-US'); // 1033 {en-US}
  ALDefaultFormatSettingsA.CurrencyString := '$';
  ALDefaultFormatSettingsA.CurrencyFormat := 0;
  ALDefaultFormatSettingsA.CurrencyDecimals := 2;
  ALDefaultFormatSettingsA.DateSeparator := '/';
  ALDefaultFormatSettingsA.TimeSeparator := ':';
  ALDefaultFormatSettingsA.ListSeparator := ';';
  ALDefaultFormatSettingsA.ShortDateFormat := 'M/d/yyyy';
  ALDefaultFormatSettingsA.LongDateFormat := 'dddd, MMMM d, yyyy';
  ALDefaultFormatSettingsA.TimeAMString := 'AM';
  ALDefaultFormatSettingsA.TimePMString := 'PM';
  ALDefaultFormatSettingsA.ShortTimeFormat := 'h:mm AMPM';
  ALDefaultFormatSettingsA.LongTimeFormat := 'h:mm:ss AMPM';
  ALDefaultFormatSettingsA.ShortMonthNames[1] := 'Jan';
  ALDefaultFormatSettingsA.LongMonthNames [1] := 'January';
  ALDefaultFormatSettingsA.ShortMonthNames[2] := 'Feb';
  ALDefaultFormatSettingsA.LongMonthNames [2] := 'February';
  ALDefaultFormatSettingsA.ShortMonthNames[3] := 'Mar';
  ALDefaultFormatSettingsA.LongMonthNames [3] := 'March';
  ALDefaultFormatSettingsA.ShortMonthNames[4] := 'Apr';
  ALDefaultFormatSettingsA.LongMonthNames [4] := 'April';
  ALDefaultFormatSettingsA.ShortMonthNames[5] := 'May';
  ALDefaultFormatSettingsA.LongMonthNames [5] := 'May';
  ALDefaultFormatSettingsA.ShortMonthNames[6] := 'Jun';
  ALDefaultFormatSettingsA.LongMonthNames [6] := 'June';
  ALDefaultFormatSettingsA.ShortMonthNames[7] := 'Jul';
  ALDefaultFormatSettingsA.LongMonthNames [7] := 'July';
  ALDefaultFormatSettingsA.ShortMonthNames[8] := 'Aug';
  ALDefaultFormatSettingsA.LongMonthNames [8] := 'August';
  ALDefaultFormatSettingsA.ShortMonthNames[9] := 'Sep';
  ALDefaultFormatSettingsA.LongMonthNames [9] := 'September';
  ALDefaultFormatSettingsA.ShortMonthNames[10] := 'Oct';
  ALDefaultFormatSettingsA.LongMonthNames [10] := 'October';
  ALDefaultFormatSettingsA.ShortMonthNames[11] := 'Nov';
  ALDefaultFormatSettingsA.LongMonthNames [11] := 'November';
  ALDefaultFormatSettingsA.ShortMonthNames[12] := 'Dec';
  ALDefaultFormatSettingsA.LongMonthNames [12] := 'December';
  ALDefaultFormatSettingsA.ShortDayNames[1] := 'Sun';
  ALDefaultFormatSettingsA.LongDayNames [1] := 'Sunday';
  ALDefaultFormatSettingsA.ShortDayNames[2] := 'Mon';
  ALDefaultFormatSettingsA.LongDayNames [2] := 'Monday';
  ALDefaultFormatSettingsA.ShortDayNames[3] := 'Tue';
  ALDefaultFormatSettingsA.LongDayNames [3] := 'Tuesday';
  ALDefaultFormatSettingsA.ShortDayNames[4] := 'Wed';
  ALDefaultFormatSettingsA.LongDayNames [4] := 'Wednesday';
  ALDefaultFormatSettingsA.ShortDayNames[5] := 'Thu';
  ALDefaultFormatSettingsA.LongDayNames [5] := 'Thursday';
  ALDefaultFormatSettingsA.ShortDayNames[6] := 'Fri';
  ALDefaultFormatSettingsA.LongDayNames [6] := 'Friday';
  ALDefaultFormatSettingsA.ShortDayNames[7] := 'Sat';
  ALDefaultFormatSettingsA.LongDayNames [7] := 'Saturday';
  ALDefaultFormatSettingsA.ThousandSeparator := ',';
  ALDefaultFormatSettingsA.DecimalSeparator := '.';
  ALDefaultFormatSettingsA.TwoDigitYearCenturyWindow := 50;
  ALDefaultFormatSettingsA.NegCurrFormat := 0;

  ALDefaultFormatSettingsW := TALFormatSettingsW.Create('en-US'); // 1033 {en-US}
  ALDefaultFormatSettingsW.CurrencyString := '$';
  ALDefaultFormatSettingsW.CurrencyFormat := 0;
  ALDefaultFormatSettingsW.CurrencyDecimals := 2;
  ALDefaultFormatSettingsW.DateSeparator := '/';
  ALDefaultFormatSettingsW.TimeSeparator := ':';
  ALDefaultFormatSettingsW.ListSeparator := ';';
  ALDefaultFormatSettingsW.ShortDateFormat := 'M/d/yyyy';
  ALDefaultFormatSettingsW.LongDateFormat := 'dddd, MMMM d, yyyy';
  ALDefaultFormatSettingsW.TimeAMString := 'AM';
  ALDefaultFormatSettingsW.TimePMString := 'PM';
  ALDefaultFormatSettingsW.ShortTimeFormat := 'h:mm AMPM';
  ALDefaultFormatSettingsW.LongTimeFormat := 'h:mm:ss AMPM';
  ALDefaultFormatSettingsW.ShortMonthNames[1] := 'Jan';
  ALDefaultFormatSettingsW.LongMonthNames [1] := 'January';
  ALDefaultFormatSettingsW.ShortMonthNames[2] := 'Feb';
  ALDefaultFormatSettingsW.LongMonthNames [2] := 'February';
  ALDefaultFormatSettingsW.ShortMonthNames[3] := 'Mar';
  ALDefaultFormatSettingsW.LongMonthNames [3] := 'March';
  ALDefaultFormatSettingsW.ShortMonthNames[4] := 'Apr';
  ALDefaultFormatSettingsW.LongMonthNames [4] := 'April';
  ALDefaultFormatSettingsW.ShortMonthNames[5] := 'May';
  ALDefaultFormatSettingsW.LongMonthNames [5] := 'May';
  ALDefaultFormatSettingsW.ShortMonthNames[6] := 'Jun';
  ALDefaultFormatSettingsW.LongMonthNames [6] := 'June';
  ALDefaultFormatSettingsW.ShortMonthNames[7] := 'Jul';
  ALDefaultFormatSettingsW.LongMonthNames [7] := 'July';
  ALDefaultFormatSettingsW.ShortMonthNames[8] := 'Aug';
  ALDefaultFormatSettingsW.LongMonthNames [8] := 'August';
  ALDefaultFormatSettingsW.ShortMonthNames[9] := 'Sep';
  ALDefaultFormatSettingsW.LongMonthNames [9] := 'September';
  ALDefaultFormatSettingsW.ShortMonthNames[10] := 'Oct';
  ALDefaultFormatSettingsW.LongMonthNames [10] := 'October';
  ALDefaultFormatSettingsW.ShortMonthNames[11] := 'Nov';
  ALDefaultFormatSettingsW.LongMonthNames [11] := 'November';
  ALDefaultFormatSettingsW.ShortMonthNames[12] := 'Dec';
  ALDefaultFormatSettingsW.LongMonthNames [12] := 'December';
  ALDefaultFormatSettingsW.ShortDayNames[1] := 'Sun';
  ALDefaultFormatSettingsW.LongDayNames [1] := 'Sunday';
  ALDefaultFormatSettingsW.ShortDayNames[2] := 'Mon';
  ALDefaultFormatSettingsW.LongDayNames [2] := 'Monday';
  ALDefaultFormatSettingsW.ShortDayNames[3] := 'Tue';
  ALDefaultFormatSettingsW.LongDayNames [3] := 'Tuesday';
  ALDefaultFormatSettingsW.ShortDayNames[4] := 'Wed';
  ALDefaultFormatSettingsW.LongDayNames [4] := 'Wednesday';
  ALDefaultFormatSettingsW.ShortDayNames[5] := 'Thu';
  ALDefaultFormatSettingsW.LongDayNames [5] := 'Thursday';
  ALDefaultFormatSettingsW.ShortDayNames[6] := 'Fri';
  ALDefaultFormatSettingsW.LongDayNames [6] := 'Friday';
  ALDefaultFormatSettingsW.ShortDayNames[7] := 'Sat';
  ALDefaultFormatSettingsW.LongDayNames [7] := 'Saturday';
  ALDefaultFormatSettingsW.ThousandSeparator := ',';
  ALDefaultFormatSettingsW.DecimalSeparator := '.';
  ALDefaultFormatSettingsW.TwoDigitYearCenturyWindow := 50;
  ALDefaultFormatSettingsW.NegCurrFormat := 0;

end;

{******************************}
Procedure _ALStringFinalization;
begin
  AlFreeAndNil(_Base64Encoding);
end;

initialization
  _ALStringInitialization;

finalization
  _ALStringFinalization;

end.
