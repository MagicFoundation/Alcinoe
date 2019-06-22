{*************************************************************
Description:  Powerfull stringreplace, Pos, Move, comparetext,
              uppercase, lowercase function. Also a powerfull
              FastTagReplace function To replace in string tag
              like <#tagname params1="value1" params2="value2">
              by custom value
**************************************************************}

unit ALString;

interface

{$H+,B-,R-}

{$IF CompilerVersion < 29} {Delphi XE8}
  {$IF defined(CPUX64)} // The CPU supports the x86-64 instruction set, and is in a 64-bit environment. *New* in XE2/x64
    {$DEFINE CPU64BITS} // The CPU is in a 64-bit environment, such as DCC64.EXE. *New* in XE8
  {$ENDIF}
  {$IF defined(CPUX86)} // 	The CPU supports the x86-64 instruction set, and is in a 64-bit environment. *New* in XE2/x64
    {$DEFINE CPU32BITS} // The CPU is in a 32-bit environment, such as DCC32.EXE. *New* in XE8
  {$ENDIF}
{$ENDIF}

{$IF Low(string) = 0}
  {$DEFINE _ZEROBASEDSTRINGS_ON}
{$ENDIF}

// http://docwiki.embarcadero.com/RADStudio/en/Conditional_compilation_(Delphi)
// http://docwiki.embarcadero.com/RADStudio/en/Compiler_Versions
{$IFDEF CPUX86}
  {$DEFINE X86ASM}
{$ELSE !CPUX86}
  {$DEFINE PUREPASCAL}
  {$DEFINE PUREPASCAL_X64ONLY}
{$ENDIF !CPUX86}

{$IF SizeOf(Extended) = 10}
  {$DEFINE EXTENDEDHAS10BYTES}
{$ENDIF}

{$IFDEF ANDROID}
  {$DEFINE USE_LIBICU}
{$ENDIF}

{$DEFINE LEGACY_FORMAT} // Define this to enable the old ASM code for Win32.

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses {$IFDEF MSWINDOWS}
     Winapi.Windows,
     {$ENDIF}
     System.SysUtils,
     System.Classes,
     {$IFNDEF NEXTGEN}
     System.Contnrs,
     {$ENDIF}
     {$IFDEF MACOS}
     Macapi.CoreFoundation,
     {$ENDIF MACOS}
     ALInit,
     ALStringList;

resourcestring
  SALInvalidFormat = 'Format ''%s'' invalid or incompatible with argument';
  SALArgumentMissing = 'No argument for format ''%s''';

type

  {$IF CompilerVersion > 33} // rio
    {$MESSAGE WARN 'Check if System.SysUtils.TFormatSettings is still the same and adjust the IFDEF'}
  {$IFEND}

  {$IFNDEF NEXTGEN}
  pALFormatSettings = ^TALFormatSettings;
  TALFormatSettings = record
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
    ListSeparator: AnsiChar;
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
    ThousandSeparator: AnsiChar;
    DecimalSeparator: AnsiChar;
    TwoDigitYearCenturyWindow: Word;
    NegCurrFormat: Byte;
    // Creates a TALFormatSettings record with current default values provided
    // by the operating system.
    class function Create: TALFormatSettings; overload; static; inline;
    // Creates a TALFormatSettings record with values provided by the operating
    // system for the specified locale. The locale is an LCID on Windows
    // platforms, or a locale_t on Posix platforms.
    {$IF defined(MSWINDOWS)}
    class function Create(Locale: LCID): TALFormatSettings; overload; platform; static;
    {$IFEND}
    // Creates a TALFormatSettings record with values provided by the operating
    // system for the specified locale name in the "Language-Country" format.
    // Example: 'en-US' for U.S. English settings or 'en-UK' for UK English settings.
    class function Create(const LocaleName: AnsiString): TALFormatSettings; overload; static;
    function GetEraYearOffset(const Name: ansistring): Integer;
  end;
  {$ENDIF}

  pALFormatSettingsU = ^TALFormatSettingsU;
  TALFormatSettingsU = TFormatSettings;

  {$IFNDEF NEXTGEN}
  function ALGetFormatSettingsID(const aFormatSettings: TALFormatSettings): AnsiString;
  {$IF defined(MSWINDOWS)}
  procedure ALGetLocaleFormatSettings(Locale: LCID; var AFormatSettings: TALFormatSettings); platform;
  {$IFEND}
  {$ENDIF}

var
  {$IFNDEF NEXTGEN}
  ALDefaultFormatSettings: TALformatSettings;
  {$ENDIF}
  ALDefaultFormatSettingsU: TALformatSettingsU;

type

  {$IFNDEF NEXTGEN}
  EALException = class(Exception)
  public
    constructor Create(const Msg: AnsiString);
    constructor CreateFmt(const Msg: ansistring; const Args: array of const);
  end;
  {$ENDIF}
  EALExceptionU = class(Exception);

  {$IFNDEF NEXTGEN}
  TALStringStream = class(TStream)
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
  {$ENDIF}

  {$IFNDEF NEXTGEN}

  {************************}
  {$IF CompilerVersion > 33} // rio
    {$MESSAGE WARN 'Check if System.Masks is still the same and adjust the IFDEF'}
  {$IFEND}

  EALMaskException = class(Exception);

  TALMask = class
  private type
    // WideChar Reduced to ByteChar in set expressions.
    TALMaskSet = set of ansiChar;
    PALMaskSet = ^TALMaskSet;
    TALMaskStates = (msLiteral, msAny, msSet, msMBCSLiteral);
    TALMaskState = record
      SkipTo: Boolean;
      case State: TALMaskStates of
        msLiteral: (Literal: ansiChar);
        msAny: ();
        msSet: (
          Negate: Boolean;
          CharSet: PALMaskSet);
        msMBCSLiteral: (LeadByte, TrailByte: ansiChar);
    end;

  private
    FMaskStates: array of TALMaskState;

  protected
    function InitMaskStates(const Mask: ansistring): Integer;
    procedure DoneMaskStates;
    function MatchesMaskStates(const Filename: ansistring): Boolean;

  public
    constructor Create(const MaskValue: ansistring);
    destructor Destroy; override;
    function Matches(const Filename: ansistring): Boolean;
  end;

  function  ALMatchesMask(const Filename, Mask: AnsiString): Boolean;

  {$ENDIF}

type
  TALPerlRegExOptions = set of (
    preCaseLess,       // /i -> Case insensitive
    preMultiLine,      // /m -> ^ and $ also match before/after a newline, not just at the beginning and the end of the string
    preSingleLine,     // /s -> Dot matches any character, including \n (newline). Otherwise, it matches anything except \n
    preExtended,       // /x -> Allow regex to contain extra whitespace, newlines and Perl-style comments, all of which will be filtered out
    preAnchored,       // /A -> Successful match can only occur at the start of the subject or right after the previous match
    preUnGreedy,       // Repeat operators (+, *, ?) are not greedy by default (i.e. they try to match the minimum number of characters instead of the maximum)
    preNoAutoCapture   // (group) is a non-capturing group; only named groups capture
  );

// TALPerlRegEx is not supported anymore after Tokyo, you can use instead
// TPerlRegEx (unicode). TALPerlRegEx is hard to maintain and the win in
// performance (mostly to avoid to do a conversion from ansiString to Unicode)
// seam to be low (I didn't do any test, but i suppose)
{$IF (not defined(NEXTGEN)) and (CompilerVersion <= 32)}{Delphi Tokyo}

type
  TALPerlRegExState = set of (
    preNotBOL,         // Not Beginning Of Line: ^ does not match at the start of Subject
    preNotEOL,         // Not End Of Line: $ does not match at the end of Subject
    preNotEmpty        // Empty matches not allowed
  );

const
  // Maximum number of subexpressions (backreferences)
  // Subexpressions are created by placing round brackets in the regex, and are referenced by \1, \2, ...
  // In Perl, they are available as $1, $2, ... after the regex matched; with TALPerlRegEx, use the Subexpressions property
  // You can also insert \1, \2, ... in the replacement string; \0 is the complete matched expression
  cALPerlRegExMAXSUBEXPRESSIONS = 99;

// All implicit string casts have been verified to be correct
{ $WARN IMPLICIT_STRING_CAST OFF}

type
  TALPerlRegExReplaceEvent = procedure(Sender: TObject; var ReplaceWith: AnsiString) of object;

type
  TALPerlRegEx = class
  private    // *** Property storage, getters and setters
    FCompiled, FStudied: Boolean;
    FOptions: TALPerlRegExOptions;
    FState: TALPerlRegExState;
    FRegEx: AnsiString;
    FReplacement: AnsiString;
    FSubject: AnsiString;
    FStart, FStop: Integer;
    FOnMatch: TNotifyEvent;
    FOnReplace: TALPerlRegExReplaceEvent;
    function GetMatchedText: AnsiString;
    function GetMatchedLength: Integer;
    function GetMatchedOffset: Integer;
    procedure SetOptions(Value: TALPerlRegExOptions);
    procedure SetRegEx(const Value: AnsiString);
    function GetGroupCount: Integer;
    function GetGroups(Index: Integer): AnsiString;
    function GetGroupLengths(Index: Integer): Integer;
    function GetGroupOffsets(Index: Integer): Integer;
    procedure SetSubject(const Value: AnsiString);
    procedure SetStart(const Value: Integer);
    procedure SetStop(const Value: Integer);
    function GetFoundMatch: Boolean;
  private    // *** Variables used by PCRE
    Offsets: array[0..(cALPerlRegExMAXSUBEXPRESSIONS+1)*3] of Integer;
    OffsetCount: Integer;
    FPCREOptions: Integer;
    FPattern: Pointer;
    FHints: Pointer;
    FCharTable: Pointer;
    FSubjectPChar: PAnsiChar;
    FHasStoredGroups: Boolean;
    FStoredGroups: array of AnsiString;
    function GetSubjectLeft: AnsiString;
    function GetSubjectRight: AnsiString;
  protected
    procedure CleanUp;
        // Dispose off whatever we created, so we can start over. Called automatically when needed, so it is not made public
    procedure ClearStoredGroups;
  public
    constructor Create;
        // Come to life
    destructor Destroy; override;
        // Clean up after ourselves
    class function EscapeRegExChars(const S: AnsiString): AnsiString;
        // Escapes regex characters in S so that the regex engine can be used to match S as plain text
    function Compile(const RaiseException: boolean = True): boolean;
        // Compile the regex. Called automatically by Match
    procedure Study;
        // Study the regex. Studying takes time, but will make the execution of the regex a lot faster.
        // Call study if you will be using the same regex many times
    function Match: Boolean; overload;
        // Attempt to match the regex, starting the attempt from the beginning of Subject
    function Match(const aSubject: ansiString; aGroups: TalStrings): Boolean; overload;
        // Thread Safe version of Match
    function MatchAgain: Boolean;
        // Attempt to match the regex to the remainder of Subject after the previous match (as indicated by Start)
    function Replace: AnsiString;
        // Replace matched expression in Subject with ComputeReplacement.  Returns the actual replacement text from ComputeReplacement
    function ReplaceAll: Boolean;
        // Repeat MatchAgain and Replace until you drop.  Returns True if anything was replaced at all.
    function ComputeReplacement: AnsiString;
        // Returns Replacement with backreferences filled in
    procedure StoreGroups;
        // Stores duplicates of Groups[] so they and ComputeReplacement will still return the proper strings
        // even if FSubject is changed or cleared
    function NamedGroup(const Name: AnsiString): Integer;
        // Returns the index of the named group Name
    procedure Split(Strings: TALStrings; Limit: Integer);
        // Split Subject along regex matches.  Capturing groups are ignored.
    procedure SplitCapture(Strings: TALStrings; Limit: Integer); overload;
    procedure SplitCapture(Strings: TALStrings; Limit: Integer; Offset : Integer); overload;
        // Split Subject along regex matches.  Capturing groups are added to Strings as well.
    property Compiled: Boolean read FCompiled;
        // True if the RegEx has already been compiled.
    property FoundMatch: Boolean read GetFoundMatch;
        // Returns True when Matched* and Group* indicate a match
    property Studied: Boolean read FStudied;
        // True if the RegEx has already been studied
    property MatchedText: AnsiString read GetMatchedText;
        // The matched text
    property MatchedLength: Integer read GetMatchedLength;
        // Length of the matched text
    property MatchedOffset: Integer read GetMatchedOffset;
        // Character offset in the Subject string at which MatchedText starts
    property Start: Integer read FStart write SetStart;
        // Starting position in Subject from which MatchAgain begins
    property Stop: Integer read FStop write SetStop;
        // Last character in Subject that Match and MatchAgain search through
    property State: TALPerlRegExState read FState write FState;
        // State of Subject
    property GroupCount: Integer read GetGroupCount;
        // Number of matched capturing groups
    property Groups[Index: Integer]: AnsiString read GetGroups;
        // Text matched by capturing groups
    property GroupLengths[Index: Integer]: Integer read GetGroupLengths;
        // Lengths of the text matched by capturing groups
    property GroupOffsets[Index: Integer]: Integer read GetGroupOffsets;
        // Character offsets in Subject at which the capturing group matches start
    property Subject: AnsiString read FSubject write SetSubject;
        // The string on which Match() will try to match RegEx
    property SubjectLeft: AnsiString read GetSubjectLeft;
        // Part of the subject to the left of the match
    property SubjectRight: AnsiString read GetSubjectRight;
        // Part of the subject to the right of the match
  public
    property Options: TALPerlRegExOptions read FOptions write SetOptions;
        // Options
    property RegEx: AnsiString read FRegEx write SetRegEx;
        // The regular expression to be matched
    property Replacement: AnsiString read FReplacement write FReplacement;
        // Text to replace matched expression with. \number and $number backreferences will be substituted with Groups
        // TALPerlRegEx supports the "JGsoft" replacement text flavor as explained at http://www.regular-expressions.info/refreplace.html
    property OnMatch: TNotifyEvent read FOnMatch write FOnMatch;
        // Triggered by Match and MatchAgain after a successful match
    property OnReplace: TALPerlRegExReplaceEvent read FOnReplace write FOnReplace;
        // Triggered by Replace and ReplaceAll just before the replacement is done, allowing you to determine the new AnsiString
  end;

{
  You can add TALPerlRegEx instances to a TALPerlRegExList to match them all together on the same subject,
  as if they were one regex regex1|regex2|regex3|...
  TALPerlRegExList does not own the TALPerlRegEx components, just like a TList
  If a TALPerlRegEx has been added to a TALPerlRegExList, it should not be used in any other situation
  until it is removed from the list
}

type
  TALPerlRegExList = class
  private
    FList: TList;
    FSubject: AnsiString;
    FMatchedRegEx: TALPerlRegEx;
    FStart, FStop: Integer;
    function GetRegEx(Index: Integer): TALPerlRegEx;
    procedure SetRegEx(Index: Integer; Value: TALPerlRegEx);
    procedure SetSubject(const Value: AnsiString);
    procedure SetStart(const Value: Integer);
    procedure SetStop(const Value: Integer);
    function GetCount: Integer;
  protected
    procedure UpdateRegEx(ARegEx: TALPerlRegEx);
  public
    constructor Create;
    destructor Destroy; override;
  public
    function Add(ARegEx: TALPerlRegEx): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    function IndexOf(ARegEx: TALPerlRegEx): Integer;
    procedure Insert(Index: Integer; ARegEx: TALPerlRegEx);
  public
    function Match: Boolean;
    function MatchAgain: Boolean;
    property RegEx[Index: Integer]: TALPerlRegEx read GetRegEx write SetRegEx;
    property Count: Integer read GetCount;
    property Subject: AnsiString read FSubject write SetSubject;
    property Start: Integer read FStart write SetStart;
    property Stop: Integer read FStop write SetStop;
    property MatchedRegEx: TALPerlRegEx read FMatchedRegEx;
  end;

  ERegularExpressionError = class(Exception);
{$IFEND}

var ALMove: procedure (const Source; var Dest; Count: NativeInt);
function  ALIfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer): Integer; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64): Int64; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: UInt64; const AFalse: UInt64): UInt64; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: Single; const AFalse: Single): Single; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double): Double; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: Extended; const AFalse: Extended): Extended; overload; inline;

{$IFNDEF NEXTGEN}
function  ALGUIDToByteString(const Guid: TGUID): Ansistring;
function  ALNewGUIDByteString: Ansistring;
function  ALGUIDToString(const Guid: TGUID; const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): Ansistring;
Function  ALNewGUIDString(const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): AnsiString;
function  ALIfThen(AValue: Boolean; const ATrue: AnsiString; AFalse: AnsiString = ''): AnsiString; overload; inline;
function  ALFormat(const Format: AnsiString; const Args: array of const): AnsiString; overload;
procedure ALFormat(const Format: AnsiString; const Args: array of const; var Result: ansiString); overload;
function  ALFormat(const Format: AnsiString; const Args: array of const; const AFormatSettings: TALFormatSettings): AnsiString; overload;
procedure ALFormat(const Format: AnsiString; const Args: array of const; const AFormatSettings: TALFormatSettings; var Result: ansiString); overload;
function  ALTryStrToBool(const S: Ansistring; out Value: Boolean): Boolean;
Function  AlStrToBool(Value:AnsiString):Boolean;
function  ALBoolToStr(B: Boolean; const trueStr: ansistring='1'; const falseStr: ansistring='0'): Ansistring; overload;
procedure ALBoolToStr(var s: ansiString; B: Boolean; const trueStr: ansistring='1'; const falseStr: ansistring='0'); overload;
function  ALDateToStr(const DateTime: TDateTime; const AFormatSettings: TALFormatSettings): AnsiString;
function  ALTimeToStr(const DateTime: TDateTime; const AFormatSettings: TALFormatSettings): AnsiString;
function  ALDateTimeToStr(const DateTime: TDateTime; const AFormatSettings: TALFormatSettings): AnsiString; overload;
procedure ALDateTimeToStr(const DateTime: TDateTime; var s: ansiString; const AFormatSettings: TALFormatSettings); overload;
function  ALFormatDateTime(const Format: AnsiString; DateTime: TDateTime; const AFormatSettings: TALFormatSettings): AnsiString;
function  ALTryStrToDate(const S: AnsiString; out Value: TDateTime; const AFormatSettings: TALFormatSettings): Boolean;
function  ALStrToDate(const S: AnsiString; const AFormatSettings: TALFormatSettings): TDateTime;
function  ALTryStrToTime(const S: AnsiString; out Value: TDateTime; const AFormatSettings: TALFormatSettings): Boolean;
function  ALStrToTime(const S: AnsiString; const AFormatSettings: TALFormatSettings): TDateTime;
function  ALTryStrToDateTime(const S: AnsiString; out Value: TDateTime; const AFormatSettings: TALFormatSettings): Boolean;
function  ALStrToDateTime(const S: AnsiString; const AFormatSettings: TALFormatSettings): TDateTime;
function  ALTryStrToInt(const S: AnsiString; out Value: Integer): Boolean;
function  ALStrToInt(const S: AnsiString): Integer;
function  ALStrToIntDef(const S: AnsiString; Default: Integer): Integer;
function  ALTryStrToUInt(const S: AnsiString; out Value: Cardinal): Boolean;
function  ALStrToUInt(const S: AnsiString): Cardinal;
function  ALStrToUIntDef(const S: Ansistring; Default: Cardinal): Cardinal;
function  ALTryStrToInt64(const S: AnsiString; out Value: Int64): Boolean;
function  ALStrToInt64(const S: AnsiString): Int64;
function  ALStrToInt64Def(const S: AnsiString; const Default: Int64): Int64;
function  ALIntToStr(Value: Integer): AnsiString; overload;
procedure ALIntToStr(Value: Integer; var s: ansiString); overload;
function  ALIntToStr(Value: Int64): AnsiString; overload;
procedure ALIntToStr(Value: Int64; var s: ansiString); overload;
function  ALStrToUInt64(const S: ansistring): UInt64;
function  ALStrToUInt64Def(const S: ansistring; const Default: UInt64): UInt64;
function  ALTryStrToUInt64(const S: ansistring; out Value: UInt64): Boolean;
function  ALUIntToStr(Value: Cardinal): AnsiString; overload;
function  ALUIntToStr(Value: UInt64): AnsiString; overload;
function  ALIntToHex(Value: Integer; Digits: Integer): AnsiString; overload;
function  ALIntToHex(Value: Int64; Digits: Integer): AnsiString; overload;
function  ALIntToHex(Value: UInt64; Digits: Integer): AnsiString; overload;
Function  ALTryBinToHex(const aBin: AnsiString; out Value: AnsiString): boolean; overload;
Function  ALBinToHex(const aBin: AnsiString): AnsiString; overload;
Function  ALTryBinToHex(const aBin; aBinSize : Cardinal; out Value: AnsiString): boolean; overload;
Function  ALBinToHex(const aBin; aBinSize : Cardinal): AnsiString; overload;
Function  ALTryHexToBin(const aHex: AnsiString; out Value: AnsiString): boolean;
Function  ALHexToBin(const aHex: AnsiString): AnsiString;
function  ALIntToBit(value: Integer; digits: integer): ansistring;
function  AlBitToInt(Value: ansiString): Integer;
function  AlInt2BaseN(NumIn: UInt64; const charset: array of ansiChar): ansistring;
function  AlBaseN2Int(const Str: ansiString; const charset: array of ansiChar): UInt64;
function  ALBase64EncodeString(const P: PansiChar; const ln: Integer): AnsiString; overload;
function  ALBase64EncodeString(const S: AnsiString): AnsiString; overload;
function  ALBase64DecodeString(const P: PansiChar; const ln: Integer): AnsiString; overload;
function  ALBase64DecodeString(const S: AnsiString): AnsiString; overload;
function  ALBase64EncodeStringMIME(const S: AnsiString): AnsiString;
function  ALBase64DecodeStringMIME(const S: AnsiString): AnsiString;
function  ALIsDecimal(const S: AnsiString; const RejectPlusMinusSign: boolean = False): boolean;
Function  ALIsInt64 (const S: AnsiString): Boolean;
Function  ALIsInteger (const S: AnsiString): Boolean;
Function  ALIsSmallInt (const S: AnsiString): Boolean;
Function  ALIsFloat (const S: AnsiString; const AFormatSettings: TALFormatSettings): Boolean;
function  ALFloatToStr(Value: Extended; const AFormatSettings: TALFormatSettings): AnsiString; overload;
procedure ALFloatToStr(Value: Extended; var S: ansiString; const AFormatSettings: TALFormatSettings); overload;
function  ALFloatToStrF(Value: Extended; Format: TFloatFormat; Precision, Digits: Integer; const AFormatSettings: TALFormatSettings): AnsiString;
function  ALCurrToStr(Value: Currency; const AFormatSettings: TALFormatSettings): AnsiString;
function  ALFormatFloat(const Format: AnsiString; Value: Extended; const AFormatSettings: TALFormatSettings): AnsiString;
function  ALFormatCurr(const Format: AnsiString; Value: Currency; const AFormatSettings: TALFormatSettings): AnsiString;
function  ALStrToFloat(const S: AnsiString; const AFormatSettings: TALFormatSettings): Extended;
function  ALStrToFloatDef(const S: AnsiString; const Default: Extended; const AFormatSettings: TALFormatSettings): Extended;
function  ALTryStrToFloat(const S: AnsiString; out Value: Extended; const AFormatSettings: TALFormatSettings): Boolean; overload;
function  ALTryStrToFloat(const S: AnsiString; out Value: Double; const AFormatSettings: TALFormatSettings): Boolean; overload;
function  ALTryStrToFloat(const S: AnsiString; out Value: Single; const AFormatSettings: TALFormatSettings): Boolean; overload;
function  ALStrToCurr(const S: AnsiString; const AFormatSettings: TALFormatSettings): Currency;
function  ALStrToCurrDef(const S: AnsiString; const Default: Currency; const AFormatSettings: TALFormatSettings): Currency;
function  ALTryStrToCurr(const S: AnsiString; out Value: Currency; const AFormatSettings: TALFormatSettings): Boolean;
function  ALPos(const SubStr, Str: AnsiString; Offset: Integer = 1): Integer; inline;
var       ALPosEx: function(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;
function  ALPosExIgnoreCase(const SubStr, S: Ansistring; Offset: Integer = 1): Integer;
var       AlUpperCase: function(const S: AnsiString): AnsiString;
var       AlLowerCase: function(const S: AnsiString): AnsiString;
function  AlUpCase(const Ch: AnsiChar): AnsiChar;
function  AlLoCase(const Ch: AnsiChar): AnsiChar;
var       ALCompareStr: function(const S1, S2: AnsiString): Integer;
var       ALSameStr: function(const S1, S2: AnsiString): Boolean;
var       ALCompareText: function(const S1, S2: AnsiString): Integer;
var       ALSameText: function(const S1, S2: AnsiString): Boolean;
var       ALMatchText: function(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
var       ALMatchStr: function(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
function  ALTrim(const S: AnsiString): AnsiString;
function  ALTrimLeft(const S: AnsiString): AnsiString;
function  ALTrimRight(const S: AnsiString): AnsiString;
function  ALPadLeft(const S: AnsiString; Const Width: Integer): AnsiString;
function  ALPadRight(const S: AnsiString; Const Width: Integer): AnsiString;
function  ALQuotedStr(const S: AnsiString; const Quote: AnsiChar = ''''): AnsiString;
function  ALDequotedStr(const S: AnsiString; AQuote: AnsiChar): AnsiString;
function  ALExtractQuotedStr(var Src: PAnsiChar; Quote: AnsiChar): AnsiString;
function  ALExtractFilePath(const FileName: AnsiString): AnsiString;
function  ALExtractFileDir(const FileName: AnsiString): AnsiString;
function  ALExtractFileDrive(const FileName: AnsiString): AnsiString;
function  ALExtractFileName(const FileName: AnsiString): AnsiString;
function  ALExtractFileExt(const FileName: AnsiString): AnsiString;
function  ALLastDelimiter(const Delimiters, S: AnsiString): Integer;
function  ALIsPathDelimiter(const S: AnsiString; Index: Integer; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): Boolean;
function  ALIncludeTrailingPathDelimiter(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString;
function  ALExcludeTrailingPathDelimiter(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString;
function  ALIncludeLeadingPathDelimiter(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString;
function  ALExcludeLeadingPathDelimiter(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString;
procedure ALStrMove(const Source: PAnsiChar; var Dest: PAnsiChar; Count: NativeInt); inline;
function  ALCopyStr(const aSourceString: AnsiString; aStart, aLength: Integer): AnsiString; overload;
procedure ALCopyStr(const aSourceString: AnsiString; var aDestString: ansiString; aStart, aLength: Integer); overload;
function  ALCopyStr(const aSourceString: AnsiString;
                    const aStartStr: AnsiString;
                    const aEndStr: AnsiString;
                    const aOffset: integer = 1;
                    const aRaiseExceptionIfNotFound: Boolean = True): AnsiString; overload;
function  ALStringReplace(const S, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;
{$ENDIF}

Function  ALNewGUIDBytes: TBytes;
function  ALGUIDToStringU(const Guid: TGUID; const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): string;
Function  ALNewGUIDStringU(const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): String;
function  ALIfThenU(AValue: Boolean; const ATrue: String; AFalse: String = ''): String; overload; inline;
function  ALFormatU(const Format: String; const Args: array of const): String; overload;
procedure ALFormatU(const Format: String; const Args: array of const; var Result: String); overload;
function  ALFormatU(const Format: String; const Args: array of const; const AFormatSettings: TALFormatSettingsU): String; overload;
procedure ALFormatU(const Format: String; const Args: array of const; const AFormatSettings: TALFormatSettingsU; var Result: String); overload;
function  ALTryStrToBoolU(const S: String; out Value: Boolean): Boolean;
Function  AlStrToBoolU(Value:String):Boolean;
function  ALBoolToStrU(B: Boolean; const trueStr: String='1'; const falseStr: String='0'): String; overload;
procedure ALBoolToStrU(var s: String; B: Boolean; const trueStr: String='1'; const falseStr: String='0'); overload;
var       ALDateToStrU: function(const DateTime: TDateTime; const AFormatSettings: TALFormatSettingsU): string;
var       ALTimeToStrU: function(const DateTime: TDateTime; const AFormatSettings: TALFormatSettingsU): string;
function  ALDateTimeToStrU(const DateTime: TDateTime; const AFormatSettings: TALFormatSettingsU): String; overload; inline;
procedure ALDateTimeToStrU(const DateTime: TDateTime; var s: String; const AFormatSettings: TALFormatSettingsU); overload; inline;
var       ALFormatDateTimeU: function(const Format: string; DateTime: TDateTime; const AFormatSettings: TALFormatSettingsU): string;
var       ALTryStrToDateU: function(const S: string; out Value: TDateTime; const AFormatSettings: TALFormatSettingsU): Boolean;
var       ALStrToDateU: function(const S: string; const AFormatSettings: TALFormatSettingsU): TDateTime;
var       ALTryStrToTimeU: function(const S: string; out Value: TDateTime; const AFormatSettings: TALFormatSettingsU): Boolean;
var       ALStrToTimeU: function(const S: string; const AFormatSettings: TALFormatSettingsU): TDateTime;
var       ALTryStrToDateTimeU: function(const S: string; out Value: TDateTime; const AFormatSettings: TALFormatSettingsU): Boolean;
var       ALStrToDateTimeU: function(const S: string; const AFormatSettings: TALFormatSettingsU): TDateTime;
var       ALTryStrToIntU: function(const S: string; out Value: Integer): Boolean;
var       ALStrToIntU: function(const S: string): Integer;
var       ALStrToIntDefU: function(const S: string; Default: Integer): Integer;
var       ALTryStrToInt64U: function(const S: string; out Value: Int64): Boolean;
var       ALStrToInt64U: function(const S: string): Int64;
var       ALStrToInt64DefU: function(const S: string; const Default: Int64): Int64;
function  ALIntToStrU(Value: Integer): String; overload; inline;
procedure ALIntToStrU(Value: Integer; var s: String); overload; inline;
function  ALIntToStrU(Value: Int64): String; overload; inline;
procedure ALIntToStrU(Value: Int64; var s: String); overload; inline;
{$IF CompilerVersion >= 26}{Delphi XE5}
var       ALStrToUInt64U: function(const S: String): UInt64;
var       ALStrToUInt64DefU: function(const S: String; const Default: UInt64): UInt64;
var       ALTryStrToUInt64U: function(const S: String; out Value: UInt64): Boolean;
{$ifend}
function  ALUIntToStrU(Value: Cardinal): String; overload; inline;
function  ALUIntToStrU(Value: UInt64): String; overload; inline;
function  ALTryBinToHexU(const aBin: Tbytes; out Value: String): boolean; overload;
Function  ALBinToHexU(const aBin: Tbytes): String; overload;
Function  ALTryBinToHexU(const aBin; aBinSize : Cardinal; out Value: String): boolean; overload;
Function  ALBinToHexU(const aBin; aBinSize : Cardinal): String; overload;
Function  ALTryHexToBinU(const aHex: String; out Value: Tbytes): boolean;
Function  ALHexToBinU(const aHex: String): Tbytes;
Function  ALBase64EncodeStringU(const S: String; const AEncoding: TEncoding = nil): String;
Function  ALBase64DecodeStringU(const S: String; const AEncoding: TEncoding = nil): String;
Function  ALBase64EncodeBytesU(const Bytes: Tbytes): String; overload;
{$IF CompilerVersion >= 31} // berlin
Function  ALBase64EncodeBytesU(const Bytes: pointer; const Size: Integer): String; overload;
{$IFEND}
Function  ALBase64DecodeBytesU(const S: String): Tbytes;
function  ALIsDecimalU(const S: String; const RejectPlusMinusSign: boolean = False): boolean;
Function  ALIsInt64U(const S: String): Boolean;
Function  ALIsIntegerU(const S: String): Boolean;
Function  ALIsSmallIntU(const S: String): Boolean;
Function  ALIsFloatU(const S: String; const AFormatSettings: TALFormatSettingsU): Boolean;
function  ALFloatToStrU(Value: Extended; const AFormatSettings: TALFormatSettingsU): String; overload; inline;
procedure ALFloatToStrU(Value: Extended; var S: String; const AFormatSettings: TALFormatSettingsU); overload; inline;
var       ALCurrToStrU: function(Value: Currency; const AFormatSettings: TALFormatSettingsU): string;
var       ALFormatFloatU: function(const Format: string; Value: Extended; const AFormatSettings: TALFormatSettingsU): string;
var       ALFormatCurrU: function(const Format: string; Value: Currency; const AFormatSettings: TALFormatSettingsU): string;
var       ALStrToFloatU: function(const S: string; const AFormatSettings: TALFormatSettingsU): Extended;
var       ALStrToFloatDefU: function(const S: string; const Default: Extended; const AFormatSettings: TALFormatSettingsU): Extended;
function  ALTryStrToFloatU(const S: String; out Value: Extended; const AFormatSettings: TALFormatSettingsU): Boolean; overload; inline;
function  ALTryStrToFloatU(const S: String; out Value: Double; const AFormatSettings: TALFormatSettingsU): Boolean; overload; inline;
function  ALTryStrToFloatU(const S: String; out Value: Single; const AFormatSettings: TALFormatSettingsU): Boolean; overload; inline;
var       ALStrToCurrU: function(const S: string; const AFormatSettings: TALFormatSettingsU): Currency;
var       ALStrToCurrDefU: function(const S: string; const Default: Currency; const AFormatSettings: TALFormatSettingsU): Currency;
var       ALTryStrToCurrU: function(const S: string; out Value: Currency; const AFormatSettings: TALFormatSettingsU): Boolean;
var       ALPosU: function(const SubStr, Str: UnicodeString; Offset: Integer = 1): Integer;
var       ALPosExU: function(const SubStr, S: string; Offset: Integer = 1): Integer;
function  ALPosExIgnoreCaseU(const SubStr, S: String; Offset: Integer = 1): Integer;
var       AlUpperCaseU: function(const S: string): string;
var       AlLowerCaseU: function(const S: string): string;
var       AlUpCaseU: function(Ch: WideChar): WideChar;
function  AlLoCaseU(Ch: WideChar): WideChar;
var       ALCompareStrU: function(const S1, S2: string): Integer;
var       ALSameStrU: function(const S1, S2: string): Boolean;
var       ALCompareTextU: function(const S1, S2: string): Integer;
var       ALSameTextU: function(const S1, S2: string): Boolean;
var       ALTrimU: function(const S: string): string;
var       ALTrimLeftU: function(const S: string): string;
var       ALTrimRightU: function(const S: string): string;
function  ALQuotedStrU(const S: String; const Quote: Char = ''''): String;
var       ALDequotedStrU: function(const S: string; AQuote: Char): string;
function  ALExtractQuotedStrU(var Src: PChar; Quote: Char): String;
var       ALLastDelimiterU: function(const Delimiters, S: string): Integer;
function  ALIsPathDelimiterU(const S: String; Index: Integer; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): Boolean;
function  ALIncludeTrailingPathDelimiterU(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String;
function  ALExcludeTrailingPathDelimiterU(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String;
function  ALIncludeLeadingPathDelimiterU(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String;
function  ALExcludeLeadingPathDelimiterU(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String;
procedure ALStrMoveU(const Source: PChar; var Dest: PChar; Count: NativeInt); inline;
function  ALCopyStrU(const aSourceString: String; aStart, aLength: Integer): String; overload;
procedure ALCopyStrU(const aSourceString: String; var aDestString: String; aStart, aLength: Integer); overload;
function  ALCopyStrU(const aSourceString: String;
                     const aStartStr: String;
                     const aEndStr: String;
                     const aOffset: integer = 1;
                     const aRaiseExceptionIfNotFound: Boolean = True): String; overload;
var ALStringReplaceU: function(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string;

{$IFNDEF NEXTGEN}
type

  TALTagParamsClass = class of TalStrings;

  TALBasePrecompiledTag = Class(Tobject)
  private
    fTagString: ansiString;
  protected
    function GetTagParams: TALStrings; virtual; abstract;
  public
    property TagString: ansiString read fTagString write fTagString;
    property TagParams: TALStrings read GetTagParams;
  End;

  TALPrecompiledTag = Class(TALBasePrecompiledTag)
  private
    fTagParams: TALStrings;
  protected
    function GetTagParams: TALStrings; override;
  public
    constructor Create;
    destructor Destroy; override;
  End;

  TALHandleTagfunct = function(const TagString: AnsiString;
                               TagParams: TALStrings;
                               ExtData: pointer;
                               Var Handled: Boolean): AnsiString;

  TALHandleTagExtendedfunct = function(const TagString: AnsiString;
                                       TagParams: TALStrings;
                                       ExtData: pointer;
                                       Var Handled: Boolean;
                                       Const SourceString: AnsiString;
                                       Var TagPosition, TagLength: integer): AnsiString;

  TALHandleTagPrecompileFunct = function(const TagString: AnsiString;
                                         TagParams: TALStrings;
                                         ExtData: pointer;
                                         Const SourceString: AnsiString;
                                         Var TagPosition, TagLength: integer): TALBasePrecompiledTag;

function ALFastTagReplacePrecompile(Const SourceString, TagStart, TagEnd: AnsiString;
                                    PrecompileProc: TALHandleTagPrecompileFunct;
                                    StripParamQuotes: Boolean;
                                    ExtData: Pointer;
                                    TagsContainer: TObjectList;
                                    Const flags: TReplaceFlags=[]): AnsiString; // rfreplaceall is ignored here, only rfIgnoreCase is matter
function ALFastTagReplace(Const SourceString, TagStart, TagEnd: AnsiString;
                          ReplaceProc: TALHandleTagFunct;
                          ReplaceExtendedProc: TALHandleTagExtendedfunct;
                          StripParamQuotes: Boolean;
                          Flags: TReplaceFlags;
                          ExtData: Pointer;
                          TagParamsClass: TALTagParamsClass;
                          const TagReplaceProcResult: Boolean = False): AnsiString; overload;
function  ALFastTagReplace(const SourceString, TagStart, TagEnd: AnsiString;
                           ReplaceProc: TALHandleTagFunct;
                           StripParamQuotes: Boolean;
                           ExtData: Pointer;
                           Const flags: TReplaceFlags=[rfreplaceall];
                           const TagReplaceProcResult: Boolean = False): AnsiString; overload;
function  ALFastTagReplace(const SourceString, TagStart, TagEnd: AnsiString;
                           ReplaceExtendedProc: TALHandleTagExtendedfunct;
                           StripParamQuotes: Boolean;
                           ExtData: Pointer;
                           Const flags: TReplaceFlags=[rfreplaceall];
                           const TagReplaceProcResult: Boolean = False): AnsiString; overload;
function  ALFastTagReplace(const SourceString, TagStart, TagEnd: AnsiString;
                           const ReplaceWith: AnsiString;
                           const Flags: TReplaceFlags=[rfreplaceall]): AnsiString; overload;
function  ALExtractTagParams(Const SourceString, TagStart, TagEnd: AnsiString;
                             StripParamQuotes: Boolean;
                             TagParams: TALStrings;
                             IgnoreCase: Boolean): Boolean;
Procedure ALSplitTextAndTag(Const SourceString, TagStart, TagEnd: AnsiString;
                            SplitTextAndTagLst: TALStrings;
                            IgnoreCase: Boolean);
{$ENDIF}


{$IFNDEF NEXTGEN}
function  ALRandomStr(const aLength: Longint; const aCharset: Array of ansiChar): AnsiString; overload;
function  ALRandomStr(const aLength: Longint): AnsiString; overload;
function  ALNEVExtractName(const S: AnsiString): AnsiString;
function  ALNEVExtractValue(const s: AnsiString): AnsiString;
function  ALGetStringFromFile(const filename: AnsiString; const ShareMode: Word = fmShareDenyWrite): AnsiString;
function  ALGetStringFromFileWithoutUTF8BOM(const filename: AnsiString; const ShareMode: Word = fmShareDenyWrite): AnsiString;
procedure ALAppendStringToFile(const Str: AnsiString; const FileName: AnsiString);
procedure ALSaveStringtoFile(const Str: AnsiString; const filename: AnsiString);
{$IF defined(MSWINDOWS)}
Function  ALWideNormalize(const S: Widestring;
                          const WordSeparator: WideChar;
                          const SymbolsToIgnore: array of WideChar): Widestring; overload;
Function  ALWideNormalize(const S: Widestring;
                          const WordSeparator: WideChar = '-'): Widestring; overload;
Function  ALWideRemoveDiacritic(const S: Widestring): Widestring;
Function  ALWideExpandLigatures(const S: Widestring): Widestring;
Function  ALWideUpperCaseNoDiacritic(const S: Widestring): Widestring;
Function  ALWideLowerCaseNoDiacritic(const S: Widestring): Widestring;
Function  ALUTF8RemoveDiacritic(const S: AnsiString): AnsiString;
Function  ALUTF8ExpandLigatures(const S: AnsiString): AnsiString;
Function  ALUTF8UpperCaseNoDiacritic(const S: AnsiString): AnsiString;
Function  ALUTF8LowerCaseNoDiacritic(const S: AnsiString): AnsiString;
Function  ALUTF8Normalize(const S: AnsiString;
                          const WordSeparator: ansiChar;
                          const SymbolsToIgnore: array of AnsiChar): AnsiString; overload;
Function  ALUTF8Normalize(const S: AnsiString;
                          const WordSeparator: ansiChar = '-'): AnsiString; overload;
function  AlUTF8Check(const S: AnsiString): Boolean;
{$IFEND}
function  ALUTF8UpperCase(const s: AnsiString): AnsiString;
function  ALUTF8LowerCase(const s: AnsiString): AnsiString;
function  AlUTF8removeBOM(const S: AnsiString): AnsiString;
function  AlUTF8DetectBOM(const P: PAnsiChar; const Size: Integer): Boolean;
function  ALUTF8CharSize(Lead: AnsiChar): Integer;
function  ALUTF8CharCount(const S: AnsiString): Integer;
Function  ALUTF8ByteTrunc(const s:AnsiString; const Count: Integer): AnsiString;
Function  ALUTF8CharTrunc(const s:AnsiString; const Count: Integer): AnsiString;
Function  ALUTF8UpperFirstChar(const s:AnsiString): AnsiString;
Function  ALUTF8TitleCase(const s:AnsiString): AnsiString;
Function  ALUTF8SentenceCase(const s:AnsiString): AnsiString;
{$IF defined(MSWINDOWS)}
Function  ALStringToWideString(const S: RawByteString; const aCodePage: Word): WideString;
function  AlWideStringToString(const WS: WideString; const aCodePage: Word): AnsiString;
Function  ALGetCodePageFromLCID(const aLCID:Integer): Word;
{$IFEND}
Function  ALUTF8Encode(const S: RawByteString; const aCodePage: Word): AnsiString;
Function  ALUTF8decode(const S: UTF8String; const aCodePage: Word): AnsiString;
Function  ALGetCodePageFromCharSetName(Acharset:AnsiString): Word;
Function  ALUTF8ISO91995CyrillicToLatin(const aCyrillicText: AnsiString): AnsiString;
Function  ALUTF8BGNPCGN1947CyrillicToLatin(const aCyrillicText: AnsiString): AnsiString;
function  ALExtractExpression(const S: AnsiString;
                              const OpenChar, CloseChar: AnsiChar; // ex: '(' and ')'
                              Const QuoteChars: Array of ansiChar; // ex: ['''', '"']
                              Const EscapeQuoteChar: ansiChar; // ex: '\' or #0 to ignore
                              var StartPos: integer;
                              var EndPos: integer): boolean;
function  ALHTTPEncode(const AStr: AnsiString): AnsiString;
function  ALHTTPDecode(const AStr: AnsiString): AnsiString;
procedure ALExtractHeaderFields(Separators,
                                WhiteSpace,
                                Quotes: TSysCharSet;
                                Content: PAnsiChar;
                                Strings: TALStrings;
                                HttpDecode: Boolean;
                                StripQuotes: Boolean = False);
procedure ALExtractHeaderFieldsWithQuoteEscaped(Separators,
                                                WhiteSpace,
                                                Quotes: TSysCharSet;
                                                Content: PAnsiChar;
                                                Strings: TALStrings;
                                                HttpDecode: Boolean;
                                                StripQuotes: Boolean = False);
{$ENDIF}

function  ALGetBytesFromStream(const aStream : TStream): Tbytes;
function  ALGetBytesFromFileU(const filename: String; const ShareMode: Word = fmShareDenyWrite): Tbytes;
function  ALGetStringFromBufferU(const buf : TBytes; const ADefaultEncoding: TEncoding): String;
function  ALGetStringFromStreamU(const aStream : TStream; const ADefaultEncoding: TEncoding) : String;
function  ALGetStringFromFileU(const filename: String; const ADefaultEncoding: TEncoding; const ShareMode: Word = fmShareDenyWrite): String;
procedure ALSaveStringtoFileU(const Str: String; const filename: String; AEncoding: TEncoding; const WriteBOM: boolean = False);
function  ALRandomStrU(const aLength: Longint; const aCharset: Array of Char): String; overload;
function  ALRandomStrU(const aLength: Longint): String; overload;
function  ALHTTPEncodeU(const AStr: String): String;
function  ALHTTPDecodeU(const AStr: String): String;
{$WARN SYMBOL_DEPRECATED OFF}
procedure ALExtractHeaderFieldsWithQuoteEscapedU(Separators,
                                                 WhiteSpace,
                                                 Quotes: TSysCharSet;
                                                 Content: PChar;
                                                 Strings: TALStringsU;
                                                 HttpDecode: Boolean;
                                                 StripQuotes: Boolean = False);
{$WARN SYMBOL_DEPRECATED ON}

{$IFNDEF NEXTGEN}
Const cAlUTF8Bom = ansiString(#$EF) + ansiString(#$BB) + ansiString(#$BF);
      cAlUTF16LittleEndianBom = ansiString(#$FF) + ansiString(#$FE);
      cAlUTF16bigEndianBom = ansiString(#$FE) + ansiString(#$FF);
      cAlUTF32LittleEndianBom = ansiString(#$FF) + ansiString(#$FE) + ansiString(#$00) + ansiString(#$00);
      cAlUTF32BigEndianBom = ansiString(#$00) + ansiString(#$00) + ansiString(#$FE) + ansiString(#$FF);
{$ENDIF}

Procedure ALStringInitialization;
procedure ALStringFinalization;

implementation

uses System.SysConst,
     System.RTLConsts,
     System.StrUtils,
     {$IF (not defined(NEXTGEN)) and (CompilerVersion <= 32)}{Delphi Tokyo}
     System.RegularExpressionsAPI,
     System.RegularExpressionsConsts,
     {$IFEND}
     {$IF CompilerVersion >= 31} // berlin
     system.netencoding,
     {$IFEND}
     {$IFNDEF NEXTGEN}
     System.Ansistrings,
     {$ENDIF}
     System.Character,
     System.Math,
     ALcommon;

{$IFNDEF NEXTGEN}

{*****************************************************}
constructor EALException.Create(const Msg: AnsiString);
begin
  inherited create(String(Msg));
end;

{************************************************************************************}
constructor EALException.CreateFmt(const Msg: ansistring; const Args: array of const);
begin
  inherited CreateFmt(String(Msg), Args);
end;

{************************************************************}
constructor TALStringStream.Create(const AString: AnsiString);
begin
  inherited Create;
  FDataString := AString;
end;

{*****************************************************************}
function TALStringStream.Read(var Buffer; Count: Longint): Longint;
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

{********************************************************************}
function TALStringStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;

  // a little modification from the original TStringStream
  // because in original it's crazy we can not update part inside the datastring !!
  // SetLength(FDataString, (FPosition + Result));
  if FPosition + Result > length(FDataString) then SetLength(FDataString, (FPosition + Result));

  ALMove(Buffer, PAnsiChar(@FDataString[FPosition + SizeOf(AnsiChar)])^, Result * SizeOf(AnsiChar));
  Inc(FPosition, Result);
end;

{********************************************************************}
function TALStringStream.Seek(Offset: Longint; Origin: Word): Longint;
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

{**************************************************************}
function TALStringStream.ReadString(Count: Longint): AnsiString;
var
  Len: Integer;
begin
  Len := Length(FDataString) - FPosition;
  if Len > Count then Len := Count;
  SetString(Result, PAnsiChar(@FDataString[FPosition + SizeOf(AnsiChar)]), Len);
  Inc(FPosition, Len);
end;

{***************************************************************}
procedure TALStringStream.WriteString(const AString: AnsiString);
begin
  Write(PAnsiChar(AString)^, Length(AString));
end;

{**************************************************}
procedure TALStringStream.SetSize(NewSize: Longint);
begin
  SetLength(FDataString, NewSize);
  if FPosition > NewSize then FPosition := NewSize;
end;

{**********************}
{$IF defined(MSWINDOWS)}
class function TALFormatSettings.Create(Locale: LCID): TALFormatSettings;
var aFormatSettings: TformatSettings;
    i: integer;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  aFormatSettings:= TformatSettings.Create(Locale);
  {$WARN SYMBOL_PLATFORM ON}
  with result do begin
    CurrencyString := AnsiString(aFormatSettings.CurrencyString);
    CurrencyFormat := aFormatSettings.CurrencyFormat;
    CurrencyDecimals := aFormatSettings.CurrencyDecimals;
    DateSeparator := AnsiChar(aFormatSettings.DateSeparator);
    TimeSeparator := AnsiChar(aFormatSettings.TimeSeparator);
    ListSeparator := AnsiChar(aFormatSettings.ListSeparator);
    ShortDateFormat := AnsiString(aFormatSettings.ShortDateFormat);
    LongDateFormat := AnsiString(aFormatSettings.LongDateFormat);
    TimeAMString := AnsiString(aFormatSettings.TimeAMString);
    TimePMString := AnsiString(aFormatSettings.TimePMString);
    ShortTimeFormat := AnsiString(aFormatSettings.ShortTimeFormat);
    LongTimeFormat := AnsiString(aFormatSettings.LongTimeFormat);
    for I := Low(ShortMonthNames) to High(ShortMonthNames) do
      ShortMonthNames[i] := AnsiString(aFormatSettings.ShortMonthNames[i]);
    for I := Low(LongMonthNames) to High(LongMonthNames) do
      LongMonthNames[i] := AnsiString(aFormatSettings.LongMonthNames[i]);
    for I := Low(ShortDayNames) to High(ShortDayNames) do
      ShortDayNames[i] := AnsiString(aFormatSettings.ShortDayNames[i]);
    for I := Low(LongDayNames) to High(LongDayNames) do
      LongDayNames[i] := AnsiString(aFormatSettings.LongDayNames[i]);
    {$IF CompilerVersion >= 28} {Delphi XE7}
    setlength(EraInfo, length(aFormatSettings.EraInfo));
    for I := Low(aFormatSettings.EraInfo) to High(aFormatSettings.EraInfo) do begin
      EraInfo[i].EraName := ansiString(aFormatSettings.EraInfo[i].EraName);
      EraInfo[i].EraOffset := aFormatSettings.EraInfo[i].EraOffset;
      EraInfo[i].EraStart := aFormatSettings.EraInfo[i].EraStart;
      EraInfo[i].EraEnd := aFormatSettings.EraInfo[i].EraEnd;
    end;
    {$else}
    setlength(EraInfo, MaxEraCount);
    for I := 1 to MaxEraCount do begin
      EraInfo[i-1].EraName := ansiString(EraNames[i]);
      EraInfo[i-1].EraOffset := EraYearOffsets[i];
      {$IFDEF POSIX}
      EraInfo[i-1].EraStart := EraRanges[i].StartDate;
      EraInfo[i-1].EraEnd := EraRanges[i].EndDate;
      {$ENDIF POSIX}
    end;
    {$ifend}
    ThousandSeparator := AnsiChar(aFormatSettings.ThousandSeparator);
    DecimalSeparator := AnsiChar(aFormatSettings.DecimalSeparator);
    TwoDigitYearCenturyWindow := aFormatSettings.TwoDigitYearCenturyWindow;
    NegCurrFormat := aFormatSettings.NegCurrFormat;
  end;
end;
{$IFEND}

{***************************************************************************************}
class function TALFormatSettings.Create(const LocaleName: AnsiString): TALFormatSettings;
var aFormatSettings: TformatSettings;
    i: integer;
begin
  aFormatSettings:= TformatSettings.Create(String(LocaleName));
  with result do begin
    CurrencyString := AnsiString(aFormatSettings.CurrencyString);
    CurrencyFormat := aFormatSettings.CurrencyFormat;
    CurrencyDecimals := aFormatSettings.CurrencyDecimals;
    DateSeparator := AnsiChar(aFormatSettings.DateSeparator);
    TimeSeparator := AnsiChar(aFormatSettings.TimeSeparator);
    ListSeparator := AnsiChar(aFormatSettings.ListSeparator);
    ShortDateFormat := AnsiString(aFormatSettings.ShortDateFormat);
    LongDateFormat := AnsiString(aFormatSettings.LongDateFormat);
    TimeAMString := AnsiString(aFormatSettings.TimeAMString);
    TimePMString := AnsiString(aFormatSettings.TimePMString);
    ShortTimeFormat := AnsiString(aFormatSettings.ShortTimeFormat);
    LongTimeFormat := AnsiString(aFormatSettings.LongTimeFormat);
    for I := Low(ShortMonthNames) to High(ShortMonthNames) do
      ShortMonthNames[i] := AnsiString(aFormatSettings.ShortMonthNames[i]);
    for I := Low(LongMonthNames) to High(LongMonthNames) do
      LongMonthNames[i] := AnsiString(aFormatSettings.LongMonthNames[i]);
    for I := Low(ShortDayNames) to High(ShortDayNames) do
      ShortDayNames[i] := AnsiString(aFormatSettings.ShortDayNames[i]);
    for I := Low(LongDayNames) to High(LongDayNames) do
      LongDayNames[i] := AnsiString(aFormatSettings.LongDayNames[i]);
    {$IF CompilerVersion >= 28} {Delphi XE7}
    setlength(EraInfo, length(aFormatSettings.EraInfo));
    for I := Low(aFormatSettings.EraInfo) to High(aFormatSettings.EraInfo) do begin
      EraInfo[i].EraName := ansiString(aFormatSettings.EraInfo[i].EraName);
      EraInfo[i].EraOffset := aFormatSettings.EraInfo[i].EraOffset;
      EraInfo[i].EraStart := aFormatSettings.EraInfo[i].EraStart;
      EraInfo[i].EraEnd := aFormatSettings.EraInfo[i].EraEnd;
    end;
    {$else}
    setlength(EraInfo, MaxEraCount);
    for I := 1 to MaxEraCount do begin
      EraInfo[i-1].EraName := ansiString(EraNames[i]);
      EraInfo[i-1].EraOffset := EraYearOffsets[i];
      {$IFDEF POSIX}
      EraInfo[i-1].EraStart := EraRanges[i].StartDate;
      EraInfo[i-1].EraEnd := EraRanges[i].EndDate;
      {$ENDIF POSIX}
    end;
    {$ifend}
    ThousandSeparator := AnsiChar(aFormatSettings.ThousandSeparator);
    DecimalSeparator := AnsiChar(aFormatSettings.DecimalSeparator);
    TwoDigitYearCenturyWindow := aFormatSettings.TwoDigitYearCenturyWindow;
    NegCurrFormat := aFormatSettings.NegCurrFormat;
  end;
end;

{***************************************************************************}
function TALFormatSettings.GetEraYearOffset(const Name: ansistring): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(EraInfo) to High(EraInfo) do
  begin
    if EraInfo[I].EraName = '' then Break;
    if ALPos(EraInfo[I].EraName, Name) > 0 then
    begin
      Result := EraInfo[I].EraOffset;
      Exit;
    end;
  end;
end;

{*********************************************************}
class function TALFormatSettings.Create: TALFormatSettings;
begin
  Result := TALFormatSettings.Create('');
end;

{***********************************************************************************}
function ALGetFormatSettingsID(const aFormatSettings: TALFormatSettings): AnsiString;
begin
  With aFormatSettings do begin
    Result := ALIntToStr(CurrencyFormat) + '#' +
              ALIntToStr(CurrencyDecimals) + '#' +
              DateSeparator + '#' +
              TimeSeparator + '#' +
              ListSeparator + '#' +
              ShortDateFormat + '#' +
              LongDateFormat + '#' +
              ShortTimeFormat + '#' +
              LongTimeFormat + '#' +
              ThousandSeparator + '#' +
              DecimalSeparator + '#' +
              ALIntToStr(TwoDigitYearCenturyWindow) + '#' +
              ALIntToStr(NegCurrFormat);
  end;
end;

{**********************}
{$IF defined(MSWINDOWS)}
procedure ALGetLocaleFormatSettings(Locale: LCID; var AFormatSettings: TALFormatSettings);
begin
  AFormatSettings := TALFormatSettings.Create(Locale);
end;
{$IFEND}

{**********************************************************}
function  ALGUIDToByteString(const Guid: TGUID): Ansistring;
var aByteArray: TBytes;
begin
  aByteArray := Guid.ToByteArray;
  SetString(result, PAnsiChar(@aByteArray[0]), length(aByteArray));
end;

{****************************************}
function  ALNewGUIDByteString: Ansistring;
var aGuid: TGUID;
begin
  if CreateGUID(aGUID) <> S_OK then RaiseLastOSError;
  result := ALGUIDToByteString(aGuid);
end;

{***********************************************************************************************************************************}
function  ALGUIDToString(const Guid: TGUID; const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): Ansistring;
begin
  if WithoutBracket then begin
    if WithoutHyphen then begin
      SetLength(Result, 32);
      System.Ansistrings.StrLFmt(PAnsiChar(Result), 32,'%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end
    else begin
      SetLength(Result, 36);
      System.Ansistrings.StrLFmt(PAnsiChar(Result), 36,'%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end;
  end
  else begin
    if WithoutHyphen then begin
      SetLength(Result, 34);
      System.Ansistrings.StrLFmt(PAnsiChar(Result), 34,'{%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x}',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end
    else begin
      SetLength(Result, 38);
      System.Ansistrings.StrLFmt(PAnsiChar(Result), 38,'{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end;
  end;
end;

{*****************************************************************************************************************}
Function  ALNewGUIDString(const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): AnsiString;
Var aGUID: TGUID;
Begin
  if CreateGUID(aGUID) <> S_OK then RaiseLastOSError;
  Result := ALGUIDToString(aGUID, WithoutBracket, WithoutHyphen);
End;

{$ENDIF !NEXTGEN}

{*******************************}
Function  ALNewGUIDBytes: TBytes;
Var aGUID: TGUID;
Begin
  if CreateGUID(aGUID) <> S_OK then RaiseLastOSError;
  SetLength(Result, 16);
  ALMove(aGuid.D1, Result[0], 4); // D1: Cardinal;
  ALMove(aGuid.D2, Result[4], 2); // D2: Word;
  ALMove(aGuid.D3, Result[6], 2); // D3: Word;
  ALMove(aGuid.D4[0], Result[8], 8); // D4: array[0..7] of Byte;
End;

{********************************************************************************************************************************}
function  ALGUIDToStringU(const Guid: TGUID; const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): string;
begin
  if WithoutBracket then begin
    if WithoutHyphen then begin
      SetLength(Result, 32);
      StrLFmt(PChar(Result), 32,'%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end
    else begin
      SetLength(Result, 36);
      StrLFmt(PChar(Result), 36,'%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end;
  end
  else begin
    if WithoutHyphen then begin
      SetLength(Result, 34);
      StrLFmt(PChar(Result), 34,'{%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x}',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end
    else begin
      SetLength(Result, 38);
      StrLFmt(PChar(Result), 38,'{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',   // do not localize
        [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
        Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
    end;
  end;
end;

{**************************************************************************************************************}
Function  ALNewGUIDStringU(const WithoutBracket: boolean = false; const WithoutHyphen: boolean = false): String;
Var aGUID: TGUID;
Begin
  if CreateGUID(aGUID) <> S_OK then RaiseLastOSError;
  Result := ALGUIDToStringU(aGUID, WithoutBracket, WithoutHyphen);
End;

{$IFNDEF NEXTGEN}

//
//TALMask is taken from delphi seattle upd1
//

{***}
const
  MaxCards = 30;

{***************************************************************}
function TALMask.InitMaskStates(const Mask: ansistring): Integer;
var
  I: Integer;
  SkipTo: Boolean;
  Literal: ansiChar;
  LeadByte, TrailByte: ansiChar;
  P: PansiChar;
  Negate: Boolean;
  CharSet: TALMaskSet;
  Cards: Integer;

  procedure InvalidMask;
  begin
    raise EALMaskException.CreateResFmt(@SInvalidMask, [Mask,
      P - PansiChar(Mask) + 1]);
  end;

  procedure Reset;
  begin
    SkipTo := False;
    Negate := False;
    CharSet := [];
  end;

  procedure WriteScan(MaskState: TALMaskStates);
  begin
    if I <= High(FMaskStates) then
    begin
      if SkipTo then
      begin
        Inc(Cards);
        if Cards > MaxCards then InvalidMask;
      end;
      FMaskStates[I].SkipTo := SkipTo;
      FMaskStates[I].State := MaskState;
      case MaskState of
        msLiteral: FMaskStates[I].Literal := UpCase(Literal);
        msSet:
          begin
            FMaskStates[I].Negate := Negate;
            New(FMaskStates[I].CharSet);
            FMaskStates[I].CharSet^ := CharSet;
          end;
        msMBCSLiteral:
          begin
            FMaskStates[I].LeadByte := LeadByte;
            FMaskStates[I].TrailByte := TrailByte;
          end;
      end;
    end;
    Inc(I);
    Reset;
  end;

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
    WriteScan(msSet);
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
      '?': if not SkipTo then WriteScan(msAny);
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
        WriteScan(msLiteral);
      end;
    end;
    Inc(P);
  end;
  Literal := #0;
  WriteScan(msLiteral);
  Result := I;
end;

{**********************************************************************}
function TALMask.MatchesMaskStates(const Filename: ansistring): Boolean;
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

  procedure Push(P: PansiChar; I: Integer);
  begin
    S[T].sP := P;
    S[T].sI := I;
    Inc(T);
  end;

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
          msLiteral:
            while (P^ <> #0) and (UpCase(P^) <> FMaskStates[I].Literal) do Inc(P);
          msSet:
            while (P^ <> #0) and not (FMaskStates[I].Negate xor (UpCase(P^) in FMaskStates[I].CharSet^)) do Inc(P);
          msMBCSLiteral:
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
        msLiteral: if UpCase(P^) <> FMaskStates[I].Literal then Exit;
        msSet: if not (FMaskStates[I].Negate xor (UpCase(P^) in FMaskStates[I].CharSet^)) then Exit;
        msMBCSLiteral:
          begin
            if P^ <> FMaskStates[I].LeadByte then Exit;
            Inc(P);
            if P^ <> FMaskStates[I].TrailByte then Exit;
          end;
        msAny:
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
  SetLength(S, MaxCards);
  Result := True;
  T := 0;
  P := PansiChar(Filename);
  I := Low(FMaskStates);
  repeat
    if Matches(P, I) then Exit;
  until not Pop(P, I);
  Result := False;
end;

{*******************************}
procedure TALMask.DoneMaskStates;
var
  I: Integer;
begin
  for I := Low(FMaskStates) to High(FMaskStates) do
    if FMaskStates[I].State = msSet then Dispose(FMaskStates[I].CharSet);
end;

{****************************************************}
constructor TALMask.Create(const MaskValue: ansistring);
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

{*************************}
destructor TALMask.Destroy;
begin
  DoneMaskStates;
  SetLength(FMaskStates, 0);
  inherited;
end;

{************************************************************}
function TALMask.Matches(const Filename: ansistring): Boolean;
begin
  Result := MatchesMaskStates(Filename);
end;

{****************************************************************}
function ALMatchesMask(const Filename, Mask: ansistring): Boolean;
var
  CMask: TALMask;
begin
  CMask := TALMask.Create(Mask);
  try
    Result := CMask.Matches(Filename);
  finally
    CMask.Free;
  end;
end;

{$ENDIF !NEXTGEN}

{$IF (not defined(NEXTGEN)) and (CompilerVersion <= 32)}{Delphi Tokyo}

{****************************************************}
function ALPerlRegExFirstCap(const S: string): string;
begin
  if S = '' then
    Result := ''
  else
  begin
    Result := LowerCase(S);
    Result[1] := UpperCase(S[1])[1];
  end
end;

{*******************************************************}
function ALPerlRegExInitialCaps(const S: string): string;
var
  I: Integer;
  Up: Boolean;
begin
  Result := LowerCase(S);
  Up := True;
  for I := 1 to Length(Result) do
  begin
    case Result[I] of
      #0..'&', '(', '*', '+', ',', '-', '.', '?', '<', '[', '{', #$00B7:
        Up := True
      else
        if Up and (Result[I] <> '''') then
        begin
          Result[1] := UpperCase(S[1])[1];
          Up := False
        end
    end;
  end;
end;

{*****************************}
procedure TALPerlRegEx.CleanUp;
begin
  FCompiled := False;
  FStudied := False;
  pcre_dispose(FPattern, FHints, nil);
  FPattern := nil;
  FHints := nil;
  ClearStoredGroups;
  OffsetCount := 0;
end;

{***************************************}
procedure TALPerlRegEx.ClearStoredGroups;
begin
  FHasStoredGroups := False;
  FStoredGroups := nil;
end;

{***************************************************************************}
function TALPerlRegEx.Compile(const RaiseException: boolean = True): boolean;
var
  Error: PAnsiChar;
  ErrorOffset: Integer;
begin
  result := False;
  CleanUp;
  if FRegEx = '' then begin
    if RaiseException then raise ERegularExpressionError.CreateRes(@SRegExMissingExpression)
    else exit;
  end;
  FPattern := pcre_compile(PAnsiChar(FRegEx), FPCREOptions, @Error, @ErrorOffset, FCharTable);
  if FPattern = nil then begin
    if RaiseException then raise ERegularExpressionError.CreateResFmt(@SRegExExpressionError, [ErrorOffset, String(Error)])
    else exit;
  end;
  FCompiled := True;
  result := True;
end;

(* Backreference overview:

Assume there are 13 backreferences:

Text        TALPerlRegEx    .NET      Java       ECMAScript
$17         $1 + "7"        "$17"     $1 + "7"   $1 + "7"
$017        $1 + "7"        "$017"    $1 + "7"   $1 + "7"
$12         $12             $12       $12        $12
$012        $1 + "2"        $12       $12        $1 + "2"
${1}2       $1 + "2"        $1 + "2"  error      "${1}2"
$$          "$"             "$"       error      "$"
\$          "$"             "\$"      "$"        "\$"
*)

{***************************************************}
function TALPerlRegEx.ComputeReplacement: AnsiString;
var
  Mode: AnsiChar;
  S: AnsiString;
  I, J, N: Integer;

  procedure ReplaceBackreference(Number: Integer);
  var
    Backreference: AnsiString;
  begin
    Delete(S, I, J-I);
    if Number <= GroupCount then
    begin
      Backreference := Groups[Number];
      if Backreference <> '' then
      begin
        // Ignore warnings; converting to UTF-8 does not cause data loss
        case Mode of
          'L', 'l': Backreference := AnsiString(LowerCase(String(Backreference)));
          'U', 'u': Backreference := AnsiString(UpperCase(String(Backreference)));
          'F', 'f': Backreference := AnsiString(ALPerlRegExFirstCap(String(Backreference)));
          'I', 'i': Backreference := AnsiString(ALPerlRegExInitialCaps(String(Backreference)));
        end;
        if S <> '' then
        begin
          Insert(Backreference, S, I);
          I := I + Length(Backreference);
        end
        else
        begin
          S := Backreference;
          I := MaxInt;
        end
      end;
    end
  end;

  procedure ProcessBackreference(NumberOnly, Dollar: Boolean);
  var
    Number, Number2: Integer;
    Group: AnsiString;
  begin
    Number := -1;
    if (J <= Length(S)) and (S[J] in ['0'..'9']) then
    begin
      // Get the number of the backreference
      Number := Ord(S[J]) - Ord('0');
      Inc(J);
      if (J <= Length(S)) and (S[J] in ['0'..'9']) then
      begin
        // Expand it to two digits only if that would lead to a valid backreference
        Number2 := Number*10 + Ord(S[J]) - Ord('0');
        if Number2 <= GroupCount then
        begin
          Number := Number2;
          Inc(J)
        end;
      end;
    end
    else if not NumberOnly then
    begin
      if Dollar and (J < Length(S)) and (S[J] = '{') then
      begin
        // Number or name in curly braces
        Inc(J);
        case S[J] of
          '0'..'9':
            begin
              Number := Ord(S[J]) - Ord('0');
              Inc(J);
              while (J <= Length(S)) and (S[J] in ['0'..'9']) do
              begin
                Number := Number*10 + Ord(S[J]) - Ord('0');
                Inc(J)
              end;
            end;
          'A'..'Z', 'a'..'z', '_':
            begin
              Inc(J);
              while (J <= Length(S)) and (S[J] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
                Inc(J);
              if (J <= Length(S)) and (S[J] = '}') then
              begin
                Group := ALCopyStr(S, I+2, J-I-2);
                Number := NamedGroup(Group);
              end
            end;
        end;
        if (J > Length(S)) or (S[J] <> '}') then
          Number := -1
        else
          Inc(J);
      end
      else if Dollar and (S[J] = '_') then
      begin
        // $_ (whole subject)
        Delete(S, I, J+1-I);
        Insert(Subject, S, I);
        I := I + Length(Subject);
        Exit;
      end
      else
      case S[J] of
        '&':
          begin
            // \& or $& (whole regex match)
            Number := 0;
            Inc(J);
          end;
        '+':
          begin
            // \+ or $+ (highest-numbered participating group)
            Number := GroupCount;
            Inc(J);
          end;
        '`':
          begin
            // \` or $` (backtick; subject to the left of the match)
            Delete(S, I, J+1-I);
            Insert(SubjectLeft, S, I);
            I := I + Offsets[0] - 1;
            Exit;
          end;
        '''':
          begin
            // \' or $' (straight quote; subject to the right of the match)
            Delete(S, I, J+1-I);
            Insert(SubjectRight, S, I);
            I := I + Length(Subject) - Offsets[1];
            Exit;
          end
      end;
    end;
    if Number >= 0 then
      ReplaceBackreference(Number)
    else
      Inc(I)
  end;

begin
  S := FReplacement;
  I := 1;
  while I < Length(S) do
  begin
    case S[I] of
      '\':
        begin
          J := I + 1;
          // We let I stop one character before the end, so J cannot point
          // beyond the end of the AnsiString here
          if J > Length(S) then
            raise ERegularExpressionError.CreateResFmt(@SRegExIndexOutOfBounds, [J]);
          case S[J] of
            '$', '\':
              begin
                Delete(S, I, 1);
                Inc(I);
              end;
            'g':
              begin
                if (J < Length(S)-1) and (S[J+1] = '<') and (S[J+2] in ['A'..'Z', 'a'..'z', '_']) then
                begin
                  // Python-style named group reference \g<name>
                  J := J+3;
                  while (J <= Length(S)) and (S[J] in ['0'..'9', 'A'..'Z', 'a'..'z', '_']) do
                    Inc(J);
                  if (J <= Length(S)) and (S[J] = '>') then
                  begin
                    N := NamedGroup(ALCopyStr(S, I+3, J-I-3));
                    Inc(J);
                    Mode := #0;
                    if N > 0 then
                      ReplaceBackreference(N)
                    else
                      Delete(S, I, J-I);
                  end
                  else
                    I := J
                end
                else
                  I := I+2;
              end;
            'l', 'L', 'u', 'U', 'f', 'F', 'i', 'I':
              begin
                Mode := S[J];
                Inc(J);
                ProcessBackreference(True, False);
              end;
          else
            Mode := #0;
            ProcessBackreference(False, False);
          end;
        end;
      '$':
        begin
          J := I + 1;
          // We let I stop one character before the end, so J cannot point
          // beyond the end of the AnsiString here
          if J > Length(S) then
            raise ERegularExpressionError.CreateResFmt(@SRegExIndexOutOfBounds, [J]);
          if S[J] = '$' then
          begin
            Delete(S, J, 1);
            Inc(I);
          end
          else
          begin
            Mode := #0;
            ProcessBackreference(False, True);
          end
        end;
    else
      Inc(I);
    end
  end;
  Result := S
end;

{******************************}
constructor TALPerlRegEx.Create;
begin
  inherited Create;
  FState := [preNotEmpty];
  FCharTable := pcre_maketables;
  FPCREOptions := PCRE_UTF8 or PCRE_NEWLINE_ANY;
end;

{******************************}
destructor TALPerlRegEx.Destroy;
begin
  pcre_dispose(FPattern, FHints, FCharTable);
  inherited Destroy;
end;

{****************************************************************************}
class function TALPerlRegEx.EscapeRegExChars(const S: AnsiString): AnsiString;
var
  I: Integer;
begin
  Result := S;
  I := Length(Result);
  while I > 0 do
  begin
    case Result[I] of
      '.', '[', ']', '(', ')', '?', '*', '+', '{', '}', '^', '$', '|', '\', '/' {NOTE: '/' was added from the delphi original TPerlRegEx}:
        Insert('\', Result, I);
      #0:
        begin
          Result[I] := '0';
          Insert('\', Result, I);
        end;
    end;
    Dec(I);
  end;
end;

{*******************************************}
function TALPerlRegEx.GetFoundMatch: Boolean;
begin
  Result := OffsetCount > 0;
end;

{***********************************************}
function TALPerlRegEx.GetMatchedText: AnsiString;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);
  Result := GetGroups(0);
end;

{**********************************************}
function TALPerlRegEx.GetMatchedLength: Integer;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);
  Result := GetGroupLengths(0)
end;

{**********************************************}
function TALPerlRegEx.GetMatchedOffset: Integer;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);
  Result := GetGroupOffsets(0)
end;

{*******************************************}
function TALPerlRegEx.GetGroupCount: Integer;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);
  Result := OffsetCount-1
end;

{*************************************************************}
function TALPerlRegEx.GetGroupLengths(Index: Integer): Integer;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);

  if (Index >= 0) and (Index <= GroupCount) then
    Result := Offsets[Index*2+1]-Offsets[Index*2]
  else
    raise ERegularExpressionError.CreateResFmt(@SRegExIndexOutOfBounds, [Index]);
end;

{*************************************************************}
function TALPerlRegEx.GetGroupOffsets(Index: Integer): Integer;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);

  if (Index >= 0) and (Index <= GroupCount) then
    Result := Offsets[Index*2]
  else
    raise ERegularExpressionError.CreateResFmt(@SRegExIndexOutOfBounds, [Index]);
end;

{**********************************************************}
function TALPerlRegEx.GetGroups(Index: Integer): AnsiString;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);
  if Index > GroupCount then
    Result := ''
  else if FHasStoredGroups then
    Result := FStoredGroups[Index]
  else
    Result := ALCopyStr(FSubject, Offsets[Index*2], Offsets[Index*2+1]-Offsets[Index*2]);
end;

{***********************************************}
function TALPerlRegEx.GetSubjectLeft: AnsiString;
begin
  Result := ALCopyStr(Subject, 1, Offsets[0]-1);
end;

{************************************************}
function TALPerlRegEx.GetSubjectRight: AnsiString;
begin
  Result := ALCopyStr(Subject, Offsets[1], MaxInt);
end;

{***********************************}
function TALPerlRegEx.Match: Boolean;
var
  I, Opts: Integer;
begin
  ClearStoredGroups;
  if not Compiled then
    Compile;
  if preNotBOL in State then
    Opts := PCRE_NOTBOL
  else
    Opts := 0;
  if preNotEOL in State then
    Opts := Opts or PCRE_NOTEOL;
  if preNotEmpty in State then
    Opts := Opts or PCRE_NOTEMPTY;
  OffsetCount := pcre_exec(FPattern, FHints, FSubjectPChar, FStop, 0, Opts, @Offsets[0], High(Offsets));
  Result := OffsetCount > 0;
  // Convert offsets into AnsiString indices
  if Result then
  begin
    for I := 0 to OffsetCount*2-1 do
      Inc(Offsets[I]);
    FStart := Offsets[1];
    if Offsets[0] = Offsets[1] then
      Inc(FStart); // Make sure we don't get stuck at the same position
    if Assigned(OnMatch) then
      OnMatch(Self)
  end;
end;

{************************************************************************************}
function TALPerlRegEx.Match(const aSubject: ansiString; aGroups: TalStrings): Boolean;
var aOpts: Integer;
    aSubjectPChar: PAnsiChar;
    aStop: Integer;
    aOffsetCount: Integer;
    aOffsets: array[0..(cALPerlRegExMAXSUBEXPRESSIONS+1)*3] of Integer;
    i: integer;
begin
  aSubjectPChar := PAnsiChar(aSubject);
  aStop := Length(aSubject);
  if not Compiled then raise ERegularExpressionError.Create('You must compile first to call the thread safe version of Match');
  if preNotBOL in State then aOpts := PCRE_NOTBOL
  else aOpts := 0;
  if preNotEOL in State then aOpts := aOpts or PCRE_NOTEOL;
  if preNotEmpty in State then aOpts := aOpts or PCRE_NOTEMPTY;
  aOffsetCount := pcre_exec(FPattern, FHints, aSubjectPChar, aStop, 0, aOpts, @aOffsets[0], High(aOffsets));
  Result := aOffsetCount > 0;
  aGroups.Clear;
  if Result then begin
    for I := 0 to aOffsetCount*2-1 do Inc(aOffsets[I]);
    for I := 0 to aOffsetCount-1 do aGroups.Add(ALCopyStr(aSubject, aOffsets[I*2], aOffsets[I*2+1]-aOffsets[I*2]))
  end;
end;

{****************************************}
function TALPerlRegEx.MatchAgain: Boolean;
var
  I, Opts: Integer;
begin
  ClearStoredGroups;
  if not Compiled then
    Compile;
  if preNotBOL in State then
    Opts := PCRE_NOTBOL
  else
    Opts := 0;
  if preNotEOL in State then
    Opts := Opts or PCRE_NOTEOL;
  if preNotEmpty in State then
    Opts := Opts or PCRE_NOTEMPTY;
  if FStart-1 > FStop then
    OffsetCount := -1
  else
    OffsetCount := pcre_exec(FPattern, FHints, FSubjectPChar, FStop, FStart-1, Opts, @Offsets[0], High(Offsets));
  Result := OffsetCount > 0;
  // Convert offsets into AnsiString indices
  if Result then
  begin
    for I := 0 to OffsetCount*2-1 do
      Inc(Offsets[I]);
    FStart := Offsets[1];
    if Offsets[0] = Offsets[1] then
      Inc(FStart); // Make sure we don't get stuck at the same position
    if Assigned(OnMatch) then
      OnMatch(Self)
  end;
end;

{****************************************************************}
function TALPerlRegEx.NamedGroup(const Name: AnsiString): Integer;
begin
  Result := pcre_get_stringnumber(FPattern, PAnsiChar(Name));
end;

{****************************************}
function TALPerlRegEx.Replace: AnsiString;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);
  // Substitute backreferences
  Result := ComputeReplacement;
  // Allow for just-in-time substitution determination
  if Assigned(OnReplace) then
    OnReplace(Self, Result);
  // Perform substitution
  Delete(FSubject, MatchedOffset, MatchedLength);
  if Result <> '' then
    Insert(Result, FSubject, MatchedOffset);
  FSubjectPChar := PAnsiChar(FSubject);
  // Position to continue search
  FStart := FStart - MatchedLength + Length(Result);
  FStop := FStop - MatchedLength + Length(Result);
  // Replacement no longer matches regex, we assume
  ClearStoredGroups;
  OffsetCount := 0;
end;

{****************************************}
function TALPerlRegEx.ReplaceAll: Boolean;
begin
  if Match then
  begin
    Result := True;
    repeat
      Replace
    until not MatchAgain;
  end
  else
    Result := False;
end;

{************************************************************}
procedure TALPerlRegEx.SetOptions(Value: TALPerlRegExOptions);
begin
  if (FOptions <> Value) then
  begin
    FOptions := Value;
    FPCREOptions := PCRE_UTF8 or PCRE_NEWLINE_ANY;
    if (preCaseLess in Value) then
      FPCREOptions := FPCREOptions or PCRE_CASELESS;
    if (preMultiLine in Value) then
      FPCREOptions := FPCREOptions or PCRE_MULTILINE;
    if (preSingleLine in Value) then
      FPCREOptions := FPCREOptions or PCRE_DOTALL;
    if (preExtended in Value) then
      FPCREOptions := FPCREOptions or PCRE_EXTENDED;
    if (preAnchored in Value) then
      FPCREOptions := FPCREOptions or PCRE_ANCHORED;
    if (preUnGreedy in Value) then
      FPCREOptions := FPCREOptions or PCRE_UNGREEDY;
    if (preNoAutoCapture in Value) then
      FPCREOptions := FPCREOptions or PCRE_NO_AUTO_CAPTURE;
    CleanUp
  end
end;

{*******************************************************}
procedure TALPerlRegEx.SetRegEx(const Value: AnsiString);
begin
  if FRegEx <> Value then
  begin
    FRegEx := Value;
    CleanUp
  end
end;

{****************************************************}
procedure TALPerlRegEx.SetStart(const Value: Integer);
begin
  if Value < 1 then
    FStart := 1
  else
    FStart := Value;
  // If FStart > Length(Subject), MatchAgain() will simply return False
end;

{***************************************************}
procedure TALPerlRegEx.SetStop(const Value: Integer);
begin
  if Value > Length(Subject) then
    FStop := Length(Subject)
  else
    FStop := Value;
end;

{*********************************************************}
procedure TALPerlRegEx.SetSubject(const Value: AnsiString);
begin
  FSubject := Value;
  FSubjectPChar := PAnsiChar(Value);
  FStart := 1;
  FStop := Length(Subject);
  if not FHasStoredGroups then
    OffsetCount := 0;
end;

{****************************************************************}
procedure TALPerlRegEx.Split(Strings: TALStrings; Limit: Integer);
var
  Offset, Count: Integer;
begin
  if Strings = nil then
    raise ERegularExpressionError.CreateRes(@SRegExStringsRequired);

  if (Limit = 1) or not Match then
    Strings.Add(Subject)
  else
  begin
    Offset := 1;
    Count := 1;
    repeat
      Strings.Add(ALCopyStr(Subject, Offset, MatchedOffset - Offset));
      Inc(Count);
      Offset := MatchedOffset + MatchedLength;
    until ((Limit > 1) and (Count >= Limit)) or not MatchAgain;
    Strings.Add(ALCopyStr(Subject, Offset, MaxInt));
  end
end;

{*******************************************************************************}
procedure TALPerlRegEx.SplitCapture(Strings: TALStrings; Limit, Offset: Integer);
var
  Count: Integer;
  LUseOffset: Boolean;
  LOffset: Integer;
begin
  if Strings = nil then
    raise ERegularExpressionError.CreateRes(@SRegExStringsRequired);

  if (Limit = 1) or not Match then
    Strings.Add(Subject)
  else
  begin
    LUseOffset := Offset <> 1;
    if Offset <> 1 then
      Dec(Limit);
    LOffset := 1;
    Count := 1;
    repeat
      if LUseOffset then
      begin
        if MatchedOffset >= Offset then
        begin
          LUseOffset := False;
          Strings.Add(ALCopyStr(Subject, 1, MatchedOffset -1));
          if Self.GroupCount > 0 then
            Strings.Add(Self.Groups[Self.GroupCount]);
        end;
      end
      else
      begin
        Strings.Add(ALCopyStr(Subject, LOffset, MatchedOffset - LOffset));
        Inc(Count);
        if Self.GroupCount > 0 then
          Strings.Add(Self.Groups[Self.GroupCount]);
      end;
      LOffset := MatchedOffset + MatchedLength;
    until ((Limit > 1) and (Count >= Limit)) or not MatchAgain;
    Strings.Add(ALCopyStr(Subject, LOffset, MaxInt));
  end
end;

{***********************************************************************}
procedure TALPerlRegEx.SplitCapture(Strings: TALStrings; Limit: Integer);
begin
  SplitCapture(Strings,Limit,1);
end;

{*********************************}
procedure TALPerlRegEx.StoreGroups;
var
  I: Integer;
begin
  if OffsetCount > 0 then
  begin
    ClearStoredGroups;
    SetLength(FStoredGroups, GroupCount+1);
    for I := GroupCount downto 0 do
      FStoredGroups[I] := Groups[I];
    FHasStoredGroups := True;
  end
end;

{***************************}
procedure TALPerlRegEx.Study;
var
  Error: PAnsiChar;
begin
  if not FCompiled then
    Compile;
  FHints := pcre_study(FPattern, 0, @Error);
  if Error <> nil then
    raise ERegularExpressionError.CreateResFmt(@SRegExStudyError, [String(Error)]);
  FStudied := True
end;

{***********************************************************}
function TALPerlRegExList.Add(ARegEx: TALPerlRegEx): Integer;
begin
  Result := FList.Add(ARegEx);
  UpdateRegEx(ARegEx);
end;

{*******************************}
procedure TALPerlRegExList.Clear;
begin
  FList.Clear;
end;

{**********************************}
constructor TALPerlRegExList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

{************************************************}
procedure TALPerlRegExList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

{**********************************}
destructor TALPerlRegExList.Destroy;
begin
  FList.Free;
  inherited
end;

{******************************************}
function TALPerlRegExList.GetCount: Integer;
begin
  Result := FList.Count;
end;

{***************************************************************}
function TALPerlRegExList.GetRegEx(Index: Integer): TALPerlRegEx;
begin
  Result := TALPerlRegEx(Pointer(FList[Index]));
end;

{***************************************************************}
function TALPerlRegExList.IndexOf(ARegEx: TALPerlRegEx): Integer;
begin
  Result := FList.IndexOf(ARegEx);
end;

{**********************************************************************}
procedure TALPerlRegExList.Insert(Index: Integer; ARegEx: TALPerlRegEx);
begin
  FList.Insert(Index, ARegEx);
  UpdateRegEx(ARegEx);
end;

{***************************************}
function TALPerlRegExList.Match: Boolean;
begin
  SetStart(1);
  FMatchedRegEx := nil;
  Result := MatchAgain;
end;

{********************************************}
function TALPerlRegExList.MatchAgain: Boolean;
var
  I, MatchStart, MatchPos: Integer;
  ARegEx: TALPerlRegEx;
begin
  if FMatchedRegEx <> nil then
    MatchStart := FMatchedRegEx.MatchedOffset + FMatchedRegEx.MatchedLength
  else
    MatchStart := FStart;
  FMatchedRegEx := nil;
  MatchPos := MaxInt;
  for I := 0 to Count-1 do
  begin
    ARegEx := RegEx[I];
    if (not ARegEx.FoundMatch) or (ARegEx.MatchedOffset < MatchStart) then
    begin
      ARegEx.Start := MatchStart;
      ARegEx.MatchAgain;
    end;
    if ARegEx.FoundMatch and (ARegEx.MatchedOffset < MatchPos) then
    begin
      MatchPos := ARegEx.MatchedOffset;
      FMatchedRegEx := ARegEx;
    end;
    if MatchPos = MatchStart then Break;
  end;
  Result := MatchPos < MaxInt;
end;

{***********************************************************************}
procedure TALPerlRegExList.SetRegEx(Index: Integer; Value: TALPerlRegEx);
begin
  FList[Index] := Value;
  UpdateRegEx(Value);
end;

{********************************************************}
procedure TALPerlRegExList.SetStart(const Value: Integer);
var
  I: Integer;
begin
  if FStart <> Value then
  begin
    FStart := Value;
    for I := Count-1 downto 0 do
      RegEx[I].Start := Value;
    FMatchedRegEx := nil;
  end;
end;

{*******************************************************}
procedure TALPerlRegExList.SetStop(const Value: Integer);
var
  I: Integer;
begin
  if FStop <> Value then
  begin
    FStop := Value;
    for I := Count-1 downto 0 do
      RegEx[I].Stop := Value;
    FMatchedRegEx := nil;
  end;
end;

{*************************************************************}
procedure TALPerlRegExList.SetSubject(const Value: AnsiString);
var
  I: Integer;
begin
  if FSubject <> Value then
  begin
    FSubject := Value;
    for I := Count-1 downto 0 do
      RegEx[I].Subject := Value;
    FMatchedRegEx := nil;
  end;
end;

{***********************************************************}
procedure TALPerlRegExList.UpdateRegEx(ARegEx: TALPerlRegEx);
begin
  ARegEx.Subject := FSubject;
  ARegEx.Start := FStart;
end;

{$IFEND}

{$IFNDEF NEXTGEN}

{***********************************************************************************************}
function ALIfThen(AValue: Boolean; const ATrue: AnsiString; AFalse: AnsiString = ''): AnsiString;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{$ENDIF !NEXTGEN}

{************************************************************************************}
function ALIfThenU(AValue: Boolean; const ATrue: String; AFalse: String = ''): String;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{***************************************************************************************}
function ALIfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer): Integer;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{*********************************************************************************}
function ALIfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64): Int64;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{************************************************************************************}
function ALIfThen(AValue: Boolean; const ATrue: UInt64; const AFalse: UInt64): UInt64;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{************************************************************************************}
function ALIfThen(AValue: Boolean; const ATrue: Single; const AFalse: Single): Single;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{************************************************************************************}
function ALIfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double): Double;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{******************************************************************************************}
function ALIfThen(AValue: Boolean; const ATrue: Extended; const AFalse: Extended): Extended;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{$IFNDEF NEXTGEN}

{***************************************************************************************}
procedure ALConvertErrorFmt(ResString: PResStringRec; const Args: array of const); local;
begin
  raise EConvertError.CreateResFmt(ResString, Args);
end;

{********************************************************}
procedure ALConvertError(ResString: PResStringRec); local;
begin
  raise EConvertError.CreateRes(ResString);
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if System.SysUtils.FormatError is still the same and adjust the IFDEF'}
{$IFEND}
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

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if System.SysUtils.AnsiFormatError is still the same and adjust the IFDEF'}
{$IFEND}
procedure ALAnsiFormatError(ErrorCode: Integer; Format: PAnsiChar; FmtLen: Cardinal);
var
  FormatText: string;
begin
  FormatText := UTF8ToUnicodeString(Format);
  ALFormatError(ErrorCode, PChar(FormatText), FmtLen);
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if System.SysUtils.FormatVarToStr is still the same and adjust the IFDEF'}
{$IFEND}
procedure ALFormatVarToStr(var S: AnsiString; const V: TVarData);
begin
  if Assigned(System.VarToLStrProc) then
    System.VarToLStrProc(S, V)
  else
    System.Error(reVarInvalidOp);
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if System.AnsiStrings.FormatClearStr is still the same and adjust the IFDEF'}
{$IFEND}
procedure ALFormatClearStr(var S: AnsiString);
begin
  S := '';
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if System.SysUtils.GetGOT is still the same and adjust the IFDEF'}
{$IFEND}
{$IFDEF X86ASM}
{$IFDEF PIC}
{ Do not remove export or the begin block. }
function ALGetGOT: Pointer;
begin
  asm
        MOV     Result,EBX
  end;
end;
{$ENDIF}
{$ENDIF X86ASM}

{***}
const
  cALDCon10: Integer = 10;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if System.SysUtils.PutExponent is still the same and adjust the IFDEF'}
{$IFEND}
{$IFDEF X86ASM}
procedure ALPutExponent;
// Store exponent
// In   AL  = Exponent character ('E' or 'e')
//      AH  = Positive sign character ('+' or 0)
//      BL  = Zero indicator
//      BH  = Destination buffer type: 0=Ansi, 1=Unicode
//      ECX = Minimum number of digits (0..4)
//      EDX = Exponent
//      EDI = Destination buffer
asm //StackAlignSafe - internal method can be called unaligned
        PUSH    ESI
{$IFDEF PIC}
        PUSH    EAX
        PUSH    ECX
        CALL    ALGetGOT
        MOV     ESI,EAX
        POP     ECX
        POP     EAX
{$ELSE !PIC}
        XOR     ESI,ESI
{$ENDIF !PIC}
        STOSB
        CMP     BH,0
        JE      @@a
        XOR     AL,AL
        STOSB
@@a:    OR      BL,BL
        JNE     @@0
        XOR     EDX,EDX
        JMP     @@1
@@0:    OR      EDX,EDX
        JGE     @@1
        MOV     AL,'-'
        NEG     EDX
        JMP     @@2
@@1:    OR      AH,AH
        JE      @@3
        MOV     AL,AH
@@2:    STOSB
        CMP     BH,0
        JE      @@3
        XOR     AL,AL
        STOSB
@@3:    XCHG    EAX,EDX
        PUSH    EAX
        PUSH    EBX
        MOV     EBX,ESP
        SUB     EBX,8
        PUSH    EBX
@@4:    XOR     EDX,EDX
        DIV     [ESI].cALDCon10
        ADD     DL,'0'
        MOV     [EBX],DL
        INC     EBX
        DEC     ECX
        OR      EAX,EAX
        JNE     @@4
        OR      ECX,ECX
        JG      @@4
        POP     EDX
        POP     ECX
@@5:    DEC     EBX
        MOV     AL,[EBX]
        STOSB
        CMP     CH,0
        JE      @@6
        XOR     AL,AL
        STOSB
@@6:    CMP     EBX,EDX
        JNE     @@5
        POP     EAX
        POP     ESI
end;
{$ENDIF X86ASM}

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if System.SysUtils.InternalFloatToText is still the same and adjust the IFDEF'}
{$IFEND}
{$IFDEF PUREPASCAL}
function ALInternalFloatToText(
  ABuffer: PByte;
  ABufferIsUnicode: Boolean;
  const AValue;
  AValueType: TFloatValue;
  AFormat: TFloatFormat;
  APrecision, ADigits: Integer;
  const AFormatSettings: TALFormatSettings): Integer;
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
  ICurrChar: integer;
  LFloatRecDigit: Integer;
  LNextThousand: Integer;

  procedure AppendChar(const AChar: AnsiChar);
  begin
    //if ABufferIsUnicode then
    //begin
    //  PChar(ABuffer)^ := Char(AChar);
    //  Inc(ABuffer, SizeOf(Char));
    //end else
    //begin
      PAnsiChar(ABuffer)^ := AChar;
      Inc(ABuffer, SizeOf(AnsiChar));
    //end;

    Inc(Result);
  end;

  procedure AppendString(const AStr: AnsiString);
  var
    {I,} L: Integer;
  begin
    L := Length(AStr);

    if L > 0 then
    begin
      //if ABufferIsUnicode then
      //begin
      //  { Unicode -- loop }
      //  for I := Low(AStr) to High(AStr) do
      //  begin
      //    PChar(ABuffer)^ := Char(AStr[I]);
      //    Inc(ABuffer, SizeOf(Char));
      //  end;
      //end else
      //begin
        { ANSI -- move directly }
        ALMove(pointer(AStr)^, ABuffer^, L);
        Inc(ABuffer, L * SizeOf(AnsiChar));
      //end;

      Inc(Result, L);
    end;
  end;

  function GetDigit: Byte;
  begin
    Result := FloatRec.Digits[LFloatRecDigit];

    if Result = Ord(#0) then
      Result := Ord('0')
    else
      Inc(LFloatRecDigit);
  end;

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
            AppendChar(AFormatSettings.ThousandSeparator);
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

    LExpString := ALIntToStr(LExponent);
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
        if FloatRec.Digits[LFloatRecDigit] <> ord(#0) then
        begin
          AppendChar(AFormatSettings.DecimalSeparator);

          { Append the ADigits that come after the decimal separator }
          while FloatRec.Digits[LFloatRecDigit] <> ord(#0) do
            AppendChar(ansiChar(GetDigit));
        end;

        if LUseENotation then
          FormatExponent();
      end else
      begin
        AppendChar(CZero);

        if FloatRec.Digits[0] <> ord(#0) then
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
          while FloatRec.Digits[LFloatRecDigit] <> ord(#0) do
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
      for ICurrChar := Low(LCurrentFormat) to High(LCurrentFormat) do
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
{$ENDIF PUREPASCAL}

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if System.AnsiStrings.FloatToText is still the same and adjust the IFDEF'}
{$IFEND}
function ALFloatToText(BufferArg: PAnsiChar; const Value; ValueType: TFloatValue;
  Format: TFloatFormat; Precision, Digits: Integer;
  const AFormatSettings: TALFormatSettings): Integer;
{$IFDEF PUREPASCAL}
begin
  { Call internal helper. Specify that we're using an ANSI buffer }
  Result := ALInternalFloatToText(PByte(BufferArg), False, Value, ValueType, Format, Precision, Digits, AFormatSettings);
end;
{$ELSE !PUREPASCAL}
{$IFDEF X86ASM}
var
  Buffer: Pointer;
  FloatRec: TFloatRec;
  SaveGOT: Integer;
  DecimalSep: AnsiChar;
  ThousandSep: AnsiChar;
  CurrencyStr: Pointer;
  CurrFmt: Byte;
  NegCurrFmt: Byte;
  //AnsiCurrencyStr: AnsiString;
asm //StackAligned
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     Buffer,EAX
{$IFDEF PIC}
        PUSH    ECX
        CALL    ALGetGOT
        MOV     SaveGOT,EAX
        POP     ECX
{$ELSE !PIC}
        MOV     SaveGOT,0
{$ENDIF !PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP,4
{$ENDIF ALIGN_STACK}
        //PUSH    ECX
        //PUSH    EDX
{$IFDEF PIC}                                    // Double indirect using GOT
        //MOV     ECX, [EAX].DefaultSystemCodePage
        //MOV     ECX, [ECX]
{$ELSE !PIC}
        //MOV     ECX, DefaultSystemCodePage
{$ENDIF}
        //LEA     EAX,AnsiCurrencyStr
        //MOV     EDX,AFormatSettings
        //MOV     EDX,[EDX].TALFormatSettings.CurrencyString
        //CALL    System.@LStrFromUStr
        //MOV     EAX,AnsiCurrencyStr
        //MOV     CurrencyStr,EAX
        //POP     EDX
        //POP     ECX

        MOV     EAX,AFormatSettings
        MOV     EAX,[EAX].TALFormatSettings.CurrencyString
        MOV     CurrencyStr,EAX

        MOV     EAX,AFormatSettings
        MOV     AL,AnsiChar([EAX].TALFormatSettings.DecimalSeparator)
        MOV     DecimalSep,AL
        MOV     EAX,AFormatSettings
        MOV     AL,AnsiChar([EAX].TALFormatSettings.ThousandSeparator)
        MOV     ThousandSep,AL
        MOV     EAX,AFormatSettings
        MOV     AL,[EAX].TALFormatSettings.CurrencyFormat
        MOV     CurrFmt,AL
        MOV     EAX,AFormatSettings
        MOV     AL,[EAX].TALFormatSettings.NegCurrFormat
        MOV     NegCurrFmt,AL

        MOV     EAX,19
        CMP     CL,fvExtended
        JNE     @@2
        MOV     EAX,Precision
        CMP     EAX,2
        JGE     @@1
        MOV     EAX,2
@@1:    CMP     EAX,18
        JLE     @@2
        MOV     EAX,18
@@2:    MOV     Precision,EAX
        PUSH    EAX
        MOV     EAX,9999
        CMP     Format,ffFixed
        JB      @@3
        MOV     EAX,Digits
@@3:    PUSH    EAX
        LEA     EAX,FloatRec
        CALL    FloatToDecimal
        MOV     EDI,Buffer
        MOVZX   EAX,FloatRec.Exponent
        SUB     EAX,7FFFH
        CMP     EAX,2
        JAE     @@4
        MOV     ECX, EAX
        CALL    @@PutSign
        LEA     ESI,@@INFNAN[ECX+ECX*2]
        ADD     ESI,SaveGOT
        MOV     ECX,3
        REP     MOVSB
        JMP     @@7
@@4:    LEA     ESI,FloatRec.Digits
        MOVZX   EBX,Format
        CMP     BL,ffExponent
        JE      @@6
        CMP     BL,ffCurrency
        JA      @@5
        MOVSX   EAX,FloatRec.Exponent
        CMP     EAX,Precision
        JLE     @@6
@@5:    MOV     BL,ffGeneral
@@6:    LEA     EBX,@@FormatVector[EBX*4]
        ADD     EBX,SaveGOT
        MOV     EBX,[EBX]
        ADD     EBX,SaveGOT
        CALL    EBX
@@7:    MOV     EAX,EDI
        SUB     EAX,Buffer
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        POP     EBX
        POP     ESI
        POP     EDI
        JMP     @@Exit

@@FormatVector:
        DD      @@PutFGeneral
        DD      @@PutFExponent
        DD      @@PutFFixed
        DD      @@PutFNumber
        DD      @@PutFCurrency

@@INFNAN: DB 'INFNAN'

// Get digit or '0' if at end of digit string

@@GetDigit:

        LODSB
        OR      AL,AL
        JNE     @@a1
        MOV     AL,'0'
        DEC     ESI
@@a1:   RET

// Store '-' if number is negative

@@PutSign:

        CMP     FloatRec.Negative,0
        JE      @@b1
        MOV     AL,'-'
        STOSB
@@b1:   RET

// Convert number using ffGeneral format

@@PutFGeneral:

        CALL    @@PutSign
        MOVSX   ECX,FloatRec.Exponent
        XOR     EDX,EDX
        CMP     ECX,Precision
        JG      @@c1
        CMP     ECX,-3
        JL      @@c1
        OR      ECX,ECX
        JG      @@c2
        MOV     AL,'0'
        STOSB
        CMP     BYTE PTR [ESI],0
        JE      @@c6
        MOV     AL,DecimalSep
        STOSB
        NEG     ECX
        MOV     AL,'0'
        REP     STOSB
        JMP     @@c3
@@c1:   MOV     ECX,1
        INC     EDX
@@c2:   LODSB
        OR      AL,AL
        JE      @@c4
        STOSB
        LOOP    @@c2
        LODSB
        OR      AL,AL
        JE      @@c5
        MOV     AH,AL
        MOV     AL,DecimalSep
        STOSW
@@c3:   LODSB
        OR      AL,AL
        JE      @@c5
        STOSB
        JMP     @@c3
@@c4:   MOV     AL,'0'
        REP     STOSB
@@c5:   OR      EDX,EDX
        JE      @@c6
        XOR     EAX,EAX
        JMP     @@PutFloatExpWithDigits
@@c6:   RET

// Convert number using ffExponent format

@@PutFExponent:

        CALL    @@PutSign
        CALL    @@GetDigit
        MOV     AH,DecimalSep
        STOSW
        MOV     ECX,Precision
        DEC     ECX
@@d1:   CALL    @@GetDigit
        STOSB
        LOOP    @@d1
        MOV     AH,'+'

@@PutFloatExpWithDigits:

        MOV     ECX,Digits
        CMP     ECX,4
        JBE     @@PutFloatExp
        XOR     ECX,ECX

// Store exponent
// In   AH  = Positive sign character ('+' or 0)
//      ECX = Minimum number of digits (0..4)

@@PutFloatExp:

        MOV     AL,'E'
        MOV     BL, FloatRec.Digits.Byte
        XOR     BH,BH
        MOVSX   EDX,FloatRec.Exponent
        DEC     EDX
        CALL    ALPutExponent   {Safe to call unaligned}
        RET

// Convert number using ffFixed or ffNumber format

@@PutFFixed:
@@PutFNumber:

        CALL    @@PutSign

// Store number in fixed point format

@@PutNumber:

        MOV     EDX,Digits
        CMP     EDX,18
        JB      @@f1
        MOV     EDX,18
@@f1:   MOVSX   ECX,FloatRec.Exponent
        OR      ECX,ECX
        JG      @@f2
        MOV     AL,'0'
        STOSB
        JMP     @@f4
@@f2:   XOR     EBX,EBX
        CMP     Format,ffFixed
        JE      @@f3
        MOV     EAX,ECX
        DEC     EAX
        MOV     BL,3
        DIV     BL
        MOV     BL,AH
        INC     EBX
@@f3:   CALL    @@GetDigit
        STOSB
        DEC     ECX
        JE      @@f4
        DEC     EBX
        JNE     @@f3
        MOV     AL,ThousandSep
        TEST    AL,AL
        JZ      @@f3
        STOSB
        MOV     BL,3
        JMP     @@f3
@@f4:   OR      EDX,EDX
        JE      @@f7
        MOV     AL,DecimalSep
        TEST    AL,AL
        JZ      @@f4b
        STOSB
@@f4b:  JECXZ   @@f6
        MOV     AL,'0'
@@f5:   STOSB
        DEC     EDX
        JE      @@f7
        INC     ECX
        JNE     @@f5
@@f6:   CALL    @@GetDigit
        STOSB
        DEC     EDX
        JNE     @@f6
@@f7:   RET

// Convert number using ffCurrency format

@@PutFCurrency:

        XOR     EBX,EBX
        MOV     BL,CurrFmt.Byte
        MOV     ECX,0003H
        CMP     FloatRec.Negative,0
        JE      @@g1
        MOV     BL,NegCurrFmt.Byte
        MOV     ECX,040FH
@@g1:   CMP     BL,CL
        JBE     @@g2
        MOV     BL,CL
@@g2:   ADD     BL,CH
        LEA     EBX,@@MoneyFormats[EBX+EBX*4]
        ADD     EBX,SaveGOT
        MOV     ECX,5
@@g10:  MOV     AL,[EBX]
        CMP     AL,'@'
        JE      @@g14
        PUSH    ECX
        PUSH    EBX
        CMP     AL,'$'
        JE      @@g11
        CMP     AL,'*'
        JE      @@g12
        STOSB
        JMP     @@g13
@@g11:  CALL    @@PutCurSym
        JMP     @@g13
@@g12:  CALL    @@PutNumber
@@g13:  POP     EBX
        POP     ECX
        INC     EBX
        LOOP    @@g10
@@g14:  RET

// Store currency symbol string

@@PutCurSym:

        PUSH    ESI
        MOV     ESI,CurrencyStr
        TEST    ESI,ESI
        JE      @@h1
        MOV     ECX,[ESI-4]
        REP     MOVSB
@@h1:   POP     ESI
        RET

// Currency formatting templates

@@MoneyFormats:
        DB      '$*@@@'
        DB      '*$@@@'
        DB      '$ *@@'
        DB      '* $@@'
        DB      '($*)@'
        DB      '-$*@@'
        DB      '$-*@@'
        DB      '$*-@@'
        DB      '(*$)@'
        DB      '-*$@@'
        DB      '*-$@@'
        DB      '*$-@@'
        DB      '-* $@'
        DB      '-$ *@'
        DB      '* $-@'
        DB      '$ *-@'
        DB      '$ -*@'
        DB      '*- $@'
        DB      '($ *)'
        DB      '(* $)'

@@Exit:
{$IFDEF ALIGN_STACK}
        //SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        //PUSH    EAX
        //PUSH    EBX
        //LEA     EAX,AnsiCurrencyStr
        //MOV     EBX,SaveGOT
        //CALL    System.@LStrClr
        //POP     EBX
        //POP     EAX
{$IFDEF ALIGN_STACK}
        //ADD     ESP, 4
{$ENDIF ALIGN_STACK}
end;
{$ENDIF X86ASM}
{$ENDIF !PUREPASCAL}

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if System.SysUtils.CvtInt is still the same and adjust the IFDEF'}
{$IFEND}
{$IFDEF X86ASM}
procedure ALCvtInt;
{ IN:
    EAX:  The integer value to be converted to text
    ESI:  Ptr to the right-hand side of the output buffer:  LEA ESI, StrBuf[16]
    ECX:  Base for conversion: 0 for signed decimal, 10 or 16 for unsigned
    EDX:  Precision: zero padded minimum field width
  OUT:
    ESI:  Ptr to start of converted text (not start of buffer)
    ECX:  Length of converted text
}
asm // StackAlignSafe
        OR      CL,CL
        JNZ     @CvtLoop
@C1:    OR      EAX,EAX
        JNS     @C2
        NEG     EAX
        CALL    @C2
        MOV     AL,'-'
        INC     ECX
        DEC     ESI
        MOV     [ESI],AL
        RET
@C2:    MOV     ECX,10

@CvtLoop:
        PUSH    EDX
        PUSH    ESI
@D1:    XOR     EDX,EDX
        DIV     ECX
        DEC     ESI
        ADD     DL,'0'
        CMP     DL,'0'+10
        JB      @D2
        ADD     DL,('A'-'0')-10
@D2:    MOV     [ESI],DL
        OR      EAX,EAX
        JNE     @D1
        POP     ECX
        POP     EDX
        SUB     ECX,ESI
        SUB     EDX,ECX
        JBE     @D5
        ADD     ECX,EDX
        MOV     AL,'0'
        SUB     ESI,EDX
        JMP     @z
@zloop: MOV     [ESI+EDX],AL
@z:     DEC     EDX
        JNZ     @zloop
        MOV     [ESI],AL
@D5:
end;
{$ENDIF X86ASM}

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if System.SysUtils.CvtInt64 is still the same and adjust the IFDEF'}
{$IFEND}
{$IFDEF X86ASM}
procedure ALCvtInt64;
{ IN:
    EAX:  Address of the int64 value to be converted to text
    ESI:  Ptr to the right-hand side of the output buffer:  LEA ESI, StrBuf[32]
    ECX:  Base for conversion: 0 for signed decimal, or 10 or 16 for unsigned
    EDX:  Precision: zero padded minimum field width
  OUT:
    ESI:  Ptr to start of converted text (not start of buffer)
    ECX:  Byte length of converted text
}
asm //StackAlignSafe
        OR      CL, CL
        JNZ     @start             // CL = 0  => signed integer conversion
        MOV     ECX, 10
        TEST    [EAX + 4], $80000000
        JZ      @start
        PUSH    [EAX + 4]
        PUSH    [EAX]
        MOV     EAX, ESP
        NEG     [ESP]              // negate the value
        ADC     [ESP + 4],0
        NEG     [ESP + 4]
        CALL    @start             // perform unsigned conversion
        MOV     [ESI-1].Byte, '-'  // tack on the negative sign
        DEC     ESI
        INC     ECX
        ADD     ESP, 8
        RET

@start:   // perform unsigned conversion
        PUSH    ESI
        SUB     ESP, 4
        FNSTCW  [ESP+2].Word     // save
        FNSTCW  [ESP].Word       // scratch
        OR      [ESP].Word, $0F00  // trunc toward zero, full precision
        FLDCW   [ESP].Word

        MOV     [ESP].Word, CX
        FLD1
        TEST    [EAX + 4], $80000000 // test for negative
        JZ      @ld1                 // FPU doesn't understand unsigned ints
        PUSH    [EAX + 4]            // copy value before modifying
        PUSH    [EAX]
        AND     [ESP + 4], $7FFFFFFF // clear the sign bit
        PUSH    $7FFFFFFF
        PUSH    $FFFFFFFF
        FILD    [ESP + 8].QWord     // load value
        FILD    [ESP].QWord
        FADD    ST(0), ST(2)        // Add 1.  Produces unsigned $80000000 in ST(0)
        FADDP   ST(1), ST(0)        // Add $80000000 to value to replace the sign bit
        ADD     ESP, 16
        JMP     @ld2
@ld1:
        FILD    [EAX].QWord         // value
@ld2:
        FILD    [ESP].Word          // base
        FLD     ST(1)
@loop:
        DEC     ESI
        FPREM                       // accumulator mod base
        FISTP   [ESP].Word
        FDIV    ST(1), ST(0)        // accumulator := acumulator / base
        MOV     AL, [ESP].Byte      // overlap long FPU division op with int ops
        ADD     AL, '0'
        CMP     AL, '0'+10
        JB      @store
        ADD     AL, ('A'-'0')-10
@store:
        MOV     [ESI].Byte, AL
        FLD     ST(1)           // copy accumulator
        FCOM    ST(3)           // if accumulator >= 1.0 then loop
        FSTSW   AX
        SAHF
        JAE @loop

        FLDCW   [ESP+2].Word
        ADD     ESP,4

        FFREE   ST(3)
        FFREE   ST(2)
        FFREE   ST(1);
        FFREE   ST(0);

        POP     ECX             // original ESI
        SUB     ECX, ESI        // ECX = length of converted string
        SUB     EDX,ECX
        JBE     @done           // output longer than field width = no pad
        SUB     ESI,EDX
        MOV     AL,'0'
        ADD     ECX,EDX
        JMP     @z
@zloop: MOV     [ESI+EDX].Byte,AL
@z:     DEC     EDX
        JNZ     @zloop
        MOV     [ESI].Byte,AL
@done:
end;
{$ENDIF X86ASM}

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if System.AnsiStrings.FormatBuf is still the same and adjust the IFDEF'}
{$IFEND}
function ALFormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const;
  const AFormatSettings: TALFormatSettings): Cardinal; overload;
{$IF not defined(LEGACY_FORMAT) or defined(PUREPASCAL)}
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
          ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
        BufPtr^ := FormatPtr^;
        Inc(FormatPtr);
        Inc(BufPtr);
        Dec(BufMaxLen, Sizeof(AnsiChar));
        Continue;
      end;
      Width := 0;
      // Gather Index
      Inc(ArgsIndex);
      if {$IF CompilerVersion >= 25}{Delphi XE4} Char(FormatPtr^).IsNumber {$ELSE} TCharacter.IsNumber(Char(FormatPtr^)) {$IFEND} then
      begin
        FormatStartPtr := FormatPtr;
        while (FormatPtr < FormatEndPtr) and {$IF CompilerVersion >= 25}{Delphi XE4} (Char(FormatPtr^).IsNumber) {$ELSE} (TCharacter.IsNumber(Char(FormatPtr^))) {$IFEND}  do
          Inc(FormatPtr);
        if FormatStartPtr <> FormatPtr then
        begin
          System.Ansistrings.StrLCopy(StrBuf, FormatStartPtr, Integer(FormatPtr - FormatStartPtr));
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
      else if {$IF CompilerVersion >= 25}{Delphi XE4} Char(FormatPtr^).IsNumber {$ELSE} TCharacter.IsNumber(Char(FormatPtr^)) {$IFEND} then
      begin
        while (FormatPtr < FormatEndPtr) and {$IF CompilerVersion >= 25}{Delphi XE4} (Char(FormatPtr^).IsNumber) {$ELSE} (TCharacter.IsNumber(Char(FormatPtr^))) {$IFEND} do
          Inc(FormatPtr);
        if FormatStartPtr <> FormatPtr then
        begin
          System.Ansistrings.StrLCopy(StrBuf, FormatStartPtr, Integer(FormatPtr - FormatStartPtr));
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
          while (FormatPtr < FormatEndPtr) and {$IF CompilerVersion >= 25}{Delphi XE4} (Char(FormatPtr^).IsNumber) {$ELSE} (TCharacter.IsNumber(Char(FormatPtr^))) {$IFEND} do
            Inc(FormatPtr);
          System.Ansistrings.StrLCopy(StrBuf, FormatStartPtr, Integer(FormatPtr - FormatStartPtr));
          if not ALTryStrToInt(AnsiString(StrBuf), Precision) then
            ALAnsiFormatError(0, PAnsiChar(@Format), FmtLen);
        end;
      end
      else
        Precision := -1;

      // Gather Conversion Character
      if not {$IF CompilerVersion >= 25}{Delphi XE4} Char(FormatPtr^).IsLetter {$ELSE} TCharacter.IsLetter(Char(FormatPtr^)) {$IFEND} then
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
            if (Precision > 16) or (Precision = -1) then
              Precision := 0;
            case FormatChar of
              'D': S := AnsiString(ALIntToStr(CurrentArg.VInteger));
              'U': S := AnsiString(ALUIntToStr(Cardinal(CurrentArg.VInteger)));
              'X': S := AnsiString(ALIntToHex(CurrentArg.VInteger, 0));
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
              'G': Len := ALFloatToText(StrBuf, CurrentArg.VExtended^, FloatVal, ffGeneral, Precision, 3, AFormatSettings);
              'E': Len := ALFloatToText(StrBuf, CurrentArg.VExtended^, FloatVal, ffExponent, Precision, 3, AFormatSettings);
              'F': Len := ALFloatToText(StrBuf, CurrentArg.VExtended^, FloatVal, ffFixed, 18, Precision, AFormatSettings);
              'N': Len := ALFloatToText(StrBuf, CurrentArg.VExtended^, FloatVal, ffNumber, 18, Precision, AFormatSettings);
              'M': Len := ALFloatToText(StrBuf, CurrentArg.VExtended^, FloatVal, ffCurrency, 18, Precision, AFormatSettings);
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
            S := AnsiString(ALIntToHex(IntPtr(CurrentArg.VPointer), SizeOf(Pointer)*2));
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
            if (Precision > 32) or (Precision = -1)  then
              Precision := 0;
            case FormatChar of
              'D': S := AnsiString(ALIntToStr(CurrentArg.VInt64^));
              'U': S := AnsiString(ALUIntToStr(UInt64(CurrentArg.VInt64^)));
              'X': S := AnsiString(ALIntToHex(CurrentArg.VInt64^, 0));
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
{$ELSE LEGACY_FORMAT or !PUREPASCAL}
{$IFDEF X86ASM}
  function AnsiFloatToTextEx(BufferArg: PAnsiChar; const Value; ValueType: TFloatValue;
    Format: TFloatFormat; Precision, Digits: Integer;
    const AFormatSettings: TALFormatSettings): Integer;
  begin
    Result := ALFloatToText(BufferArg, Value, ValueType, Format, Precision, Digits,
      AFormatSettings);
  end;

var
  ArgIndex, Width, Prec: Integer;
  BufferOrg, FormatOrg, FormatPtr: PAnsiChar;
  JustFlag: Byte;
  StrBuf: array[0..64] of AnsiChar; // if currencystring contain more than 64 chars then it's raise an error :(
  TempAnsiStr: AnsiString;
  SaveGOT: Integer;
asm
        { -> eax     Buffer }
        {    edx     BufLen }
        {    ecx     Format }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EDI, EAX
        MOV     ESI, ECX
{$IFDEF PIC}
        PUSH    ECX
        CALL    ALGetGOT
        POP     ECX
{$ELSE !PIC}
        XOR     EAX, EAX
{$ENDIF !PIC}
        MOV     SaveGOT, EAX
        ADD     ECX, FmtLen
        MOV     BufferOrg, EDI
        XOR     EAX, EAX
        MOV     ArgIndex, EAX
        MOV     TempAnsiStr, EAX

@Loop:
        OR      EDX, EDX
        JE      @Done

@NextChar:
        CMP     ESI, ECX
        JE      @Done
        LODSB
        CMP     AL, '%'
        JE      @Format

@StoreChar:
        STOSB
        DEC     EDX
        JNE     @NextChar

@Done:
        MOV     EAX, EDI
        SUB     EAX, BufferOrg
        JMP     @Exit

@Format:
        CMP     ESI, ECX
        JE      @Done
        LODSB
        CMP     AL, '%'
        JE      @StoreChar
        LEA     EBX, [ESI-2]
        MOV     FormatOrg, EBX
@A0:    MOV     JustFlag, AL
        CMP     AL, '-'
        JNE     @A1
        CMP     ESI, ECX
        JE      @Done
        LODSB
@A1:    CALL    @Specifier
        CMP     AL, ':'
        JNE     @A2
        MOV     ArgIndex, EBX
        CMP     ESI, ECX
        JE      @Done
        LODSB
        JMP     @A0

@A2:    OR      EBX, EBX
        JNS     @A2_3
        NEG     EBX
        CMP     JustFlag, '-'
        JE      @A2_2
        MOV     JustFlag, '-'
        JMP     @A2_3

@A2_2:  MOV     JustFlag, '*'

@A2_3:  MOV     Width, EBX
        MOV     EBX, -1
        CMP     AL, '.'
        JNE     @A3
        CMP     ESI, ECX
        JE      @Done
        LODSB
        CALL    @Specifier
@A3:    MOV     Prec, EBX
        MOV     FormatPtr, ESI
        PUSH    ECX
        PUSH    EDX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF}
        CALL    @Convert
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF}

        POP     EDX
        MOV     EBX, Width
        SUB     EBX, ECX       // ECX <=> number of characters output
        JAE     @A4            //         jump -> output smaller than width
        XOR     EBX, EBX

@A4:    CMP     JustFlag, '-'
        JNE     @A6
        SUB     EDX, ECX
        JAE     @A5
        ADD     ECX, EDX
        XOR     EDX, EDX

@A5:    REP     MOVSB

@A6:    XCHG    EBX, ECX
        SUB     EDX, ECX
        JAE     @A7
        ADD     ECX, EDX
        XOR     EDX, EDX
@A7:    MOV     AL, ' '
        REP     STOSB
        XCHG    EBX, ECX
        SUB     EDX, ECX
        JAE     @A8
        ADD     ECX, EDX
        XOR     EDX, EDX
@A8:    REP     MOVSB
        CMP     TempAnsiStr, 0
        JE      @A9
        PUSH    EDX
        LEA     EAX, TempAnsiStr
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    ALFormatClearStr
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        POP     EDX
@A9:    POP     ECX
        MOV     ESI,FormatPtr
        JMP     @Loop

@Specifier:
        XOR     EBX, EBX
        CMP     AL, '*'
        JE      @B3
@B1:    CMP     AL, '0'
        JB      @B5
        CMP     AL, '9'
        JA      @B5
        IMUL    EBX, EBX, 10
        SUB     AL, '0'
        MOVZX   EAX, AL
        ADD     EBX, EAX
        CMP     ESI, ECX
        JE      @B2
        LODSB
        JMP     @B1
@B2:    POP     EAX
        JMP     @Done
@B3:    MOV     EAX, ArgIndex
        CMP     EAX, Args.Integer[-4]
        JG      @B4
        INC     ArgIndex
        MOV     EBX, Args
        CMP     [EBX+EAX*8].Byte[4], vtInteger
        MOV     EBX, [EBX+EAX*8]
        JE      @B4
        XOR     EBX, EBX
@B4:    CMP     ESI, ECX
        JE      @B2
        LODSB
@B5:    RET

@Convert:
        AND     AL, 0DFH
        MOV     CL, AL
        MOV     EAX, 1
        MOV     EBX, ArgIndex
        CMP     EBX, Args.Integer[-4]
        JG      @ErrorExit
        INC     ArgIndex
        MOV     ESI, Args
        LEA     ESI, [ESI+EBX*8]
        MOV     EAX, [ESI].Integer[0]       // TVarRec.data
        MOVZX   EDX, [ESI].Byte[4]          // TVarRec.VType
{$IFDEF PIC}
        MOV     EBX, SaveGOT
        ADD     EBX, offset @CvtVector
        MOV     EBX, [EBX+EDX*4]
        ADD     EBX, SaveGOT
        JMP     EBX
{$ELSE !PIC}
        JMP     @CvtVector.Pointer[EDX*4]
{$ENDIF !PIC}

@CvtVector:
        DD      @CvtInteger                // vtInteger
        DD      @CvtBoolean                // vtBoolean
        DD      @CvtChar                   // vtChar
        DD      @CvtExtended               // vtExtended
        DD      @CvtShortStr               // vtString
        DD      @CvtPointer                // vtPointer
        DD      @CvtPChar                  // vtPChar
        DD      @CvtObject                 // vtObject
        DD      @CvtClass                  // vtClass
        DD      @CvtWideChar               // vtWideChar
        DD      @CvtPWideChar              // vtPWideChar
        DD      @CvtAnsiStr                // vtAnsiString
        DD      @CvtCurrency               // vtCurrency
        DD      @CvtVariant                // vtVariant
        DD      @CvtInterface              // vtInterface
        DD      @CvtWideString             // vtWideString
        DD      @CvtInt64                  // vtInt64
        DD      @CvtUnicodeString          // vtUnicodeString

@CvtBoolean:
@CvtObject:
@CvtClass:
@CvtWideChar:
@CvtInterface:
@CvtError:
        XOR     EAX,EAX

@ErrorExit:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF}
        CALL    @ClearTmpAnsiStr
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF}
        MOV     EDX, FormatOrg
        MOV     ECX, FormatPtr
        SUB     ECX, EDX
        MOV     EBX, SaveGOT
{$IFDEF PC_MAPPED_EXCEPTIONS}
        //  Because of all the assembly code here, we can't call a routine
        //  that throws an exception if it looks like we're still on the
        //  stack.  The static disassembler cannot give sufficient unwind
        //  frame info to unwind the confusion that is generated from the
        //  assembly code above.  So before we throw the exception, we
        //  go to some lengths to excise ourselves from the stack chain.
        //  We were passed 12 bytes of parameters on the stack, and we have
        //  to make sure that we get rid of those, too.
{$IFDEF ALIGN_STACK}
        MOV     ESP, EBP        // Ditch everthing to the frame
        POP     EBP             // Ditch the rest of the frame
{$ELSE  !ALIGN_STACK}
        MOV     ESP, EBP        // Ditch everthing to the frame
        MOV     EBP, [ESP + 4]  // Get the return addr
        MOV     [ESP + 16], EBP // Move the ret addr up in the stack
        POP     EBP             // Ditch the rest of the frame
        ADD     ESP, 12         // Ditch the space that was taken by params
{$ENDIF !ALIGN_STACK}
        JMP     ALAnsiFormatError     // Off to FormatErr - corrected from FormatError from original Delphi xe2 source. it's must be AnsiFormatError
{$ELSE !PC_MAPPED_EXCEPTIONS}
        CALL    ALAnsiFormatError     // corrected from FormatError from original Delphi xe2 source. it's must be AnsiFormatError
{$ENDIF !PC_MAPPED_EXCEPTIONS}
        // The above call raises an exception and does not return

@CvtInt64:
        // CL  <= format character
        // EAX <= address of int64
        // EBX <= TVarRec.VType

        LEA     ESI, StrBuf[32]
        MOV     EDX, Prec
        CMP     EDX, 32
        JBE     @I64_1           // zero padded field width > buffer => no padding
        XOR     EDX, EDX
@I64_1: MOV     EBX, ECX
        SUB     CL, 'D'
        JZ      ALCvtInt64         // branch predict backward jump taken
        MOV     ECX, 16
        CMP     BL, 'X'
        JE      ALCvtInt64
        MOV     ECX, 10
        CMP     BL, 'U'
        JE      ALCvtInt64
        JMP     @CvtError

@CvtInteger:
        LEA     ESI, StrBuf[16]
        MOV     EDX, Prec
        MOV     EBX, ECX
        CMP     EDX, 16
        JBE     @C1             // zero padded field width > buffer => no padding
        XOR     EDX, EDX
@C1:    SUB     CL, 'D'
        JZ      ALCvtInt          // branch predict backward jump taken
        MOV     ECX, 16
        CMP     BL, 'X'
        JE      ALCvtInt
        MOV     ECX, 10
        CMP     BL, 'U'
        JE      ALCvtInt
        JMP     @CvtError

@CvtChar:
        CMP     CL, 'S'
        JNE     @CvtError
        MOV     ECX, 1
        JMP     @CvtStrLen

@CvtVariant:
        CMP     CL, 'S'
        JNE     @CvtError
        CMP     [EAX].TVarData.VType, varNull
        JBE     @CvtEmptyStr
        MOV     EDX, EAX
        LEA     EAX, TempAnsiStr
        CALL    ALFormatVarToStr
        MOV     ESI, TempAnsiStr
        JMP     @CvtStrRef

@CvtEmptyStr:
        XOR     ECX,ECX
        RET

@CvtShortStr:
        CMP     CL, 'S'
        JNE     @CvtError
        MOV     ESI, EAX
        LODSB
        MOVZX   ECX, AL
        JMP     @CvtStrLen

@CvtPWideChar:
        MOV    ESI, OFFSET System.@LStrFromPWChar
        JMP    @CvtWideThing

@CvtUnicodeString:
        MOV    ESI, OFFSET System.@LStrFromUStr
        JMP    @CvtWideThing

@CvtWideString:
        MOV    ESI, OFFSET System.@LStrFromWStr

@CvtWideThing:
{$IFDEF PIC}
        ADD    ESI, SaveGOT
{$ENDIF PIC}
        CMP    CL, 'S'
        JNE    @CvtError
        MOV    EDX, EAX
        LEA    EAX, TempAnsiStr
{$IFDEF ALIGN_STACK}
        SUB    ESP, 4
{$ENDIF}
        PUSH   EBX
        PUSH   ECX
        MOV    EBX, SaveGOT
{$IFDEF PIC}                                    // Double indirect using GOT
        MOV    ECX, [EBX].DefaultSystemCodePage
        MOV    ECX, [ECX]
{$ELSE !PIC}
        //MOV    ECX, DefaultSystemCodePage  // >> we must use CP_UTF8 instead of DefaultSystemCodePage because if not we receive the error Need imported data reference ($G) to access DefaultSystemCodePage when we compile the dpk.
        MOV    ECX, CP_UTF8
{$ENDIF}
        CALL   ESI
        POP    ECX
        POP    EBX
{$IFDEF ALIGN_STACK}
        ADD    ESP, 4
{$ENDIF}
        MOV    ESI, TempAnsiStr
        MOV    EAX, ESI
        JMP    @CvtStrRef

@CvtAnsiStr:
        CMP     CL, 'S'
        JNE     @CvtError
        MOV     ESI, EAX

@CvtStrRef:
        OR      ESI, ESI
        JE      @CvtEmptyStr
        MOV     ECX, [ESI-4]

@CvtStrLen:
        CMP     ECX, Prec
        JA      @E1
        RET
@E1:    MOV     ECX, Prec
        RET

@CvtPChar:
        CMP     CL, 'S'
        JNE     @CvtError
        MOV     ESI, EAX
        PUSH    EDI
        MOV     EDI, EAX
        XOR     AL, AL
        MOV     ECX, Prec
        JECXZ   @F1
        REPNE   SCASB
        JNE     @F1
        DEC     EDI
@F1:    MOV     ECX, EDI
        SUB     ECX, ESI
        POP     EDI
        RET

@CvtPointer:
        CMP     CL, 'P'
        JNE     @CvtError
        MOV     EDX, 8
        MOV     ECX, 16
        LEA     ESI, StrBuf[16]
        JMP     ALCvtInt

@CvtCurrency:
        MOV     BH, fvCurrency
        JMP     @CvtFloat

@CvtExtended:
        MOV     BH, fvExtended

@CvtFloat:
        MOV     ESI, EAX
        MOV     BL, ffGeneral
        CMP     CL, 'G'
        JE      @G2
        MOV     BL, ffExponent
        CMP     CL, 'E'
        JE      @G2
        MOV     BL, ffFixed
        CMP     CL, 'F'
        JE      @G1
        MOV     BL, ffNumber
        CMP     CL, 'N'
        JE      @G1
        CMP     CL, 'M'
        JNE     @CvtError
        MOV     BL, ffCurrency
@G1:    MOV     EAX, 18
        MOV     EDX, Prec
        CMP     EDX, EAX
        JBE     @G3
        MOV     EDX, 2
        CMP     CL, 'M'
        JNE     @G3
        MOV     EDX, AFormatSettings
        MOVZX   EDX, [EDX].TALFormatSettings.CurrencyDecimals
        JMP     @G3
@G2:    MOV     EAX, Prec
        MOV     EDX, 3
        CMP     EAX, 18
        JBE     @G3
        MOV     EAX, 15
@G3:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBX
        PUSH    EAX
        PUSH    EDX
        MOV     EDX, [AFormatSettings]
        PUSH    EDX
        LEA     EAX, StrBuf
        MOV     EDX, ESI
        MOVZX   ECX, BH
        MOV     EBX, SaveGOT
        CALL    AnsiFloatToTextEx
        MOV     ECX, EAX
        LEA     ESI, StrBuf
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        RET

@ClearTmpAnsiStr:
        PUSH    EBX
        PUSH    EAX
        LEA     EAX, TempAnsiStr
        MOV     EBX, SaveGOT
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF}
        CALL    System.@LStrClr
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF}
        POP     EAX
        POP     EBX
        RET

@Exit:
        CALL    @ClearTmpAnsiStr
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF X86ASM}
{$IFEND LEGACY_FORMAT or !PUREPASCAL}

{**************************************************************}
function ALFormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const): Cardinal; overload;
begin
  Result := ALFormatBuf(Buffer, BufLen, Format, FmtLen, Args, ALDefaultFormatSettings);
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if System.AnsiStrings.FmtStr is still the same and adjust the IFDEF'}
{$IFEND}
procedure ALFmtStr(var Result: AnsiString; const Format: AnsiString;
  const Args: array of const; const AFormatSettings: TALFormatSettings);
var
  Len, BufLen: Integer;
  Buffer: array[0..4095] of AnsiChar;
begin
  BufLen := SizeOf(Buffer);
  if Length(Format) < (sizeof(Buffer) - (sizeof(Buffer) div 4)) then
    Len := ALFormatBuf(Buffer, sizeof(Buffer) - 1, Pointer(Format)^, Length(Format),
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
      Len := ALFormatBuf(Pointer(Result)^, BufLen - 1, Pointer(Format)^,
        Length(Format), Args, AFormatSettings);
    end;
    SetLength(Result, Len);
  end
  else
    SetString(Result, Buffer, Len);
end;

{**********************************************************************************}
function ALFormat(const Format: AnsiString; const Args: array of const): AnsiString;
begin
  Result := ALFormat(Format, Args, ALDefaultFormatSettings);
end;

{***********************************************************************************************}
procedure ALFormat(const Format: AnsiString; const Args: array of const; var Result: ansiString);
begin
  Result := ALFormat(Format, Args, ALDefaultFormatSettings);
end;

{****************************************************************************************************************************}
function ALFormat(const Format: AnsiString; const Args: array of const; const AFormatSettings: TALFormatSettings): AnsiString;
begin
  ALFmtStr(Result, Format, Args, AFormatSettings);
end;

{*****************************************************************************************************************************************}
procedure ALFormat(const Format: AnsiString; const Args: array of const; const AFormatSettings: TALFormatSettings; var Result: ansiString);
begin
  ALFmtStr(Result, Format, Args, AFormatSettings);
end;

{$ENDIF !NEXTGEN}

{***************************************************************************}
function ALFormatU(const Format: String; const Args: array of const): String;
begin
  Result := System.SysUtils.Format(Format, Args, ALDefaultFormatSettingsU);
end;

{****************************************************************************************}
procedure ALFormatU(const Format: String; const Args: array of const; var Result: String);
begin
  Result := System.SysUtils.Format(Format, Args, ALDefaultFormatSettingsU);
end;

{*********************************************************************************************************************}
function ALFormatU(const Format: String; const Args: array of const; const AFormatSettings: TALFormatSettingsU): String;
begin
  Result := System.SysUtils.Format(Format, Args, AFormatSettings);
end;

{**********************************************************************************************************************************}
procedure ALFormatU(const Format: String; const Args: array of const; const AFormatSettings: TALFormatSettingsU; var Result: String);
begin
  Result := System.SysUtils.Format(Format, Args, AFormatSettings);
end;

{$IFNDEF NEXTGEN}

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
    Result := ALSametext(S,'True');
    if Result then
      Value := True
    else
    begin
      Result := ALSametext(S,'False');
      if Result then
        Value := False;
    end;
  end;
end;

{$ENDIF !NEXTGEN}

{*********************************************************************}
function ALTryStrToBoolU(const S: String; out Value: Boolean): Boolean;
var
  LResult: Integer;
begin
  Result := ALTryStrToIntU(S, LResult);
  if Result then
    Value := LResult <> 0
  else
  begin
    Result := ALSametextU(S,'True');
    if Result then
      Value := True
    else
    begin
      Result := ALSametextU(S,'False');
      if Result then
        Value := False;
    end;
  end;
end;

{$IFNDEF NEXTGEN}

{*********************************************}
Function AlStrToBool(Value:AnsiString):Boolean;
Begin
  Result := False;
  ALTryStrtoBool(Value,Result);
end;

{$ENDIF !NEXTGEN}

{******************************************}
Function AlStrToBoolU(Value:String):Boolean;
Begin
  Result := False;
  ALTryStrtoBoolU(Value,Result);
end;

{$IFNDEF NEXTGEN}

{***********************************************************************************************************}
function  ALBoolToStr(B: Boolean; const trueStr: ansistring='1'; const falseStr: ansistring='0'): Ansistring;
begin
  if B then result := trueStr
  else result := falseStr;
end;

{******************************************************************************************************************}
procedure ALBoolToStr(var s: ansiString; B: Boolean; const trueStr: ansistring='1'; const falseStr: ansistring='0');
begin
  if B then s := trueStr
  else s := falseStr;
end;

{$ENDIF !NEXTGEN}

{************************************************************************************************}
function  ALBoolToStrU(B: Boolean; const trueStr: String='1'; const falseStr: String='0'): String;
begin
  if B then result := trueStr
  else result := falseStr;
end;

{*******************************************************************************************************}
procedure ALBoolToStrU(var s: String; B: Boolean; const trueStr: String='1'; const falseStr: String='0');
begin
  if B then s := trueStr
  else s := falseStr;
end;

{$IFNDEF NEXTGEN}

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
  Value := CFStringCreateWithCharacters(kCFAllocatorDefault,
    PChar(Val), Length(Val));
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

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.DateTimeToString is still the same and adjust the IFDEF'}
{$IFEND}
procedure ALDateTimeToString(var Result: AnsiString; const Format: AnsiString;
  DateTime: TDateTime; const AFormatSettings: TALFormatSettings);
var
  BufPos, AppendLevel: Integer;
  Buffer: array[0..255] of AnsiChar;
  DynBuffer: array of AnsiChar;
  Sb: TArray<ansiChar>;

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

  procedure AppendString(const S: AnsiString);
  begin
    AppendChars(Pointer(S), Length(S));
  end;

  procedure AppendNumber(Number, Digits: Integer);
  const
    Format: array[0..3] of AnsiChar = '%.*d';
  var
    NumBuf: array[0..15] of AnsiChar;
  begin
    AppendChars(NumBuf, ALFormatBuf(NumBuf, Length(NumBuf), Format,
      Length(Format), [Digits, Number]));
  end;

  procedure AppendFormat(Format: PAnsiChar);
  var
    Starter, Token, LastToken: AnsiChar;
    DateDecoded, TimeDecoded, Use12HourClock,
    BetweenQuotes: Boolean;
    P: PAnsiChar;
    Count: Integer;
    Year, Month, Day, Hour, Min, Sec, MSec, H: Word;

    procedure GetCount;
    var
      P: PAnsiChar;
    begin
      P := Format;
      while Format^ = Starter do Inc(Format);
      Count := Format - P + 1;
    end;

    procedure GetDate;
    begin
      if not DateDecoded then
      begin
        DecodeDate(DateTime, Year, Month, Day);
        DateDecoded := True;
      end;
    end;

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
      if GetDateFormatA(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
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

      if GetDateFormatA(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
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
    function FindEra(Date: Integer): Byte;
    var
      I : Byte;
    begin
      Result := 0;
      for I := High(AFormatSettings.EraInfo) downto Low(AFormatSettings.EraInfo) do
      begin
        if (AFormatSettings.EraInfo[I].EraStart <= Date) then
          Exit(I);
      end;
    end;
    {$ENDIF !MACOS}

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
      I : Byte;
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
        Formatter := CFDateFormatterCreate(kCFAllocatorDefault, Locale,
                        kCFDateFormatterFullStyle, kCFDateFormatterNoStyle);
        FormatString := TCFString.Create('GG');
        CFDateFormatterSetFormat(Formatter, FormatString.Value);
        DecodeDate(DateTime, LYear, LMonth, LDay);
        LDate.year := LYear; LDate.month := ShortInt(LMonth); LDate.day := ShortInt(LDay);
        LDate.hour := 0; LDate.minute := 0; LDate.second := 0;
        Result := TCFString(CFDateFormatterCreateStringWithAbsoluteTime(
                              kCFAllocatorDefault, Formatter,
                              CFGregorianDateGetAbsoluteTime(LDate, DefaultTZ))
                           ).AsAnsiString(true);
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

    function ConvertYearString(const Count: Integer) : AnsiString;
    var
      S : AnsiString;
      function GetEraOffset: integer;
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
          Calendar := CFCalendarCreateWithIdentifier(kCFAllocatorDefault,
                                  CFCalendarGetIdentifier(CurrentCalendar));
          if CFCalendarGetTimeRangeOfUnit(Calendar, kCFCalendarUnitEra,
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
          Result := AFormatSettings.EraInfo[Result].EraOffset;
        {$ENDIF MACOS}
      end;
    begin
      S := ALIntToStr(Year - GetEraOffset);
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

{*********************************************}
function ALDateToStr(const DateTime: TDateTime;
  const AFormatSettings: TALFormatSettings): AnsiString;
begin
  ALDateTimeToString(Result, AFormatSettings.ShortDateFormat, DateTime,
    AFormatSettings);
end;

{*********************************************}
function ALTimeToStr(const DateTime: TDateTime;
  const AFormatSettings: TALFormatSettings): AnsiString;
begin
  ALDateTimeToString(Result, AFormatSettings.LongTimeFormat, DateTime,
    AFormatSettings);
end;

{*************************************************}
function ALDateTimeToStr(const DateTime: TDateTime;
  const AFormatSettings: TALFormatSettings): AnsiString;
begin
  ALDateTimeToString(Result, '', DateTime, AFormatSettings);
end;

{****************************************************************************************************************}
procedure ALDateTimeToStr(const DateTime: TDateTime; var s: ansiString; const AFormatSettings: TALFormatSettings);
begin
  ALDateTimeToString(s, '', DateTime, AFormatSettings);
end;

{$ENDIF !NEXTGEN}

{**************************************************}
function ALDateTimeToStrU(const DateTime: TDateTime;
  const AFormatSettings: TALFormatSettingsU): String;
begin
  Result := DateTimeToStr(DateTime, AFormatSettings);
end;

{*************************************************************************************************************}
procedure ALDateTimeToStrU(const DateTime: TDateTime; var s: String; const AFormatSettings: TALFormatSettingsU);
begin
  s := DateTimeToStr(DateTime, AFormatSettings);
end;

{$IFNDEF NEXTGEN}

{**********************************************************************}
function ALFormatDateTime(const Format: AnsiString; DateTime: TDateTime;
  const AFormatSettings: TALFormatSettings): AnsiString;
begin
  ALDateTimeToString(Result, Format, DateTime, AFormatSettings);
end;

{**}
type
  TALDateOrder = (doMDY, doDMY, doYMD);

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.ScanBlanks is still the same and adjust the IFDEF'}
{$IFEND}
procedure ALScanBlanks(const S: AnsiString; var Pos: Integer);
var
  I: Integer;
begin
  I := Pos;
  while (I <= High(S)) and (S[I] = ' ') do Inc(I);
  Pos := I;
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.ScanNumber is still the same and adjust the IFDEF'}
{$IFEND}
function ALScanNumber(const S: AnsiString; var Pos: Integer;
  var Number: Word; var CharCount: Byte): Boolean;
var
  I: Integer;
  N: Word;
begin
  Result := False;
  CharCount := 0;
  ALScanBlanks(S, Pos);
  I := Pos;
  N := 0;
  while (I <= High(S)) and (S[I] in ['0'..'9']) and (N < 1000) do
  begin
    N := N * 10 + (Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if I > Pos then
  begin
    CharCount := I - Pos;
    Pos := I;
    Number := N;
    Result := True;
  end;
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.ScanString is still the same and adjust the IFDEF'}
{$IFEND}
function ALScanString(const S: AnsiString; var Pos: Integer;
  const Symbol: AnsiString): Boolean;
begin
  Result := False;
  if Symbol <> '' then
  begin
    ALScanBlanks(S, Pos);
    //if AnsiCompareText(Symbol, S.SubString(Pos - Low(string), Symbol.Length)) = 0 then
    if ALCompareText(Symbol, ALCopyStr(S, Pos + alifThen(Low(ansiString)=0,1,0), Length(Symbol))) = 0 then
    begin
      Inc(Pos, Length(Symbol));
      Result := True;
    end;
  end;
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.ScanChar is still the same and adjust the IFDEF'}
{$IFEND}
function ALScanChar(const S: AnsiString; var Pos: Integer; Ch: AnsiChar): Boolean;
begin
  Result := False;
  ALScanBlanks(S, Pos);
  if (Pos <= High(S)) and (S[Pos] = Ch) then
  begin
    Inc(Pos);
    Result := True;
  end;
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.GetDateOrder is still the same and adjust the IFDEF'}
{$IFEND}
function ALGetDateOrder(const DateFormat: AnsiString): TALDateOrder;
var
  I: Integer;
begin
  Result := doMDY;
  I := low(ansiString);
  while I <= High(DateFormat) do
  begin
    case AnsiChar(Ord(DateFormat[I]) and $DF) of
      'E': Result := doYMD;
      'Y': Result := doYMD;
      'M': Result := doMDY;
      'D': Result := doDMY;
    else
      Inc(I);
      Continue;
    end;
    Exit;
  end;
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.ScanToNumber is still the same and adjust the IFDEF'}
{$IFEND}
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

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.ScanDate is still the same and adjust the IFDEF'}
{$IFEND}
function ALScanDate(const S: AnsiString; var Pos: Integer; var Date: TDateTime;
  const AFormatSettings: TALFormatSettings): Boolean; overload;
var
  DateOrder: TALDateOrder;
  N1, N2, N3, Y, M, D: Word;
  L1, L2, L3, YearLen: Byte;
  CenturyBase: Integer;
  EraName : AnsiString;
  EraYearOffset: Integer;

  function EraToYear(Year: Integer): Integer;
  begin
{$IFDEF MSWINDOWS}
    if SysLocale.PriLangID = LANG_KOREAN then
    begin
      if Year <= 99 then
        Inc(Year, (CurrentYear + Abs(EraYearOffset)) div 100 * 100);
      if EraYearOffset > 0 then
        EraYearOffset := -EraYearOffset;
    end
    else
      Dec(EraYearOffset);
{$ENDIF MSWINDOWS}
    Result := Year + EraYearOffset;
  end;

begin
  Result := False;
  DateOrder := ALGetDateOrder(AFormatSettings.ShortDateFormat);
  EraYearOffset := 0;
  if AFormatSettings.ShortDateFormat[Low(ansistring)] = 'g' then  // skip over prefix text
  begin
    ALScanToNumber(S, Pos);
    EraName := ALTrim(ALCopyStr(S, 1, Pos-low(ansiString)));
    EraYearOffset := AFormatSettings.GetEraYearOffset(EraName);
  end
  else
    // If we are with only two digits for year, we suppose we are in the last era.
    if (ALPos('e', AFormatSettings.ShortDateFormat) > 0) and
       (High(AFormatSettings.EraInfo)>=0) then
      EraYearOffset := AFormatSettings.EraInfo[High(AFormatSettings.EraInfo)].EraOffset;
  if not (ALScanNumber(S, Pos, N1, L1) and ALScanChar(S, Pos, AFormatSettings.DateSeparator) and
    ALScanNumber(S, Pos, N2, L2)) then Exit;
  if ALScanChar(S, Pos, AFormatSettings.DateSeparator) then
  begin
    if not ALScanNumber(S, Pos, N3, L3) then Exit;
    case DateOrder of
      doMDY: begin Y := N3; YearLen := L3; M := N1; D := N2; end;
      doDMY: begin Y := N3; YearLen := L3; M := N2; D := N1; end;
      else{doYMD:} begin Y := N1; YearLen := L1; M := N2; D := N3; end;
    end;
    if EraYearOffset > 0 then
      Y := EraToYear(Y)
    else
    if (YearLen <= 2) then
    begin
      CenturyBase := CurrentYear - AFormatSettings.TwoDigitYearCenturyWindow;
      Inc(Y, CenturyBase div 100 * 100);
      if (AFormatSettings.TwoDigitYearCenturyWindow > 0) and (Y < CenturyBase) then
        Inc(Y, 100);
    end;
  end else
  begin
    Y := CurrentYear;
    if DateOrder = doDMY then
    begin
      D := N1; M := N2;
    end else
    begin
      M := N1; D := N2;
    end;
  end;
  ALScanChar(S, Pos, AFormatSettings.DateSeparator);
  ALScanBlanks(S, Pos);
  if SysLocale.FarEast and (ALPos('dddd', AFormatSettings.ShortDateFormat) <> 0) then
  begin     // ignore trailing text
    if AFormatSettings.ShortTimeFormat[Low(ansistring)] in ['0'..'9'] then  // stop at time digit
      ALScanToNumber(S, Pos)
    else  // stop at time prefix
      repeat
        while (Pos <= High(S)) and (S[Pos] <> ' ') do Inc(Pos);
        ALScanBlanks(S, Pos);
      until (Pos > high(S)) or
        //(AnsiCompareText(AFormatSettings.TimeAMString, S.SubString(Pos - Low(string), AFormatSettings.TimeAMString.Length)) = 0) or
        //(AnsiCompareText(AFormatSettings.TimePMString, S.SubString(Pos - Low(string), AFormatSettings.TimePMString.Length)) = 0);
        (ALCompareText(AFormatSettings.TimeAMString, ALCopyStr(S, Pos + alifThen(Low(ansiString)=0,1,0), Length(AFormatSettings.TimeAMString))) = 0) or
        (ALCompareText(AFormatSettings.TimePMString, ALCopyStr(S, Pos + alifThen(Low(ansiString)=0,1,0), Length(AFormatSettings.TimePMString))) = 0);
  end;
  Result := TryEncodeDate(Y, M, D, Date);
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.ScanTime is still the same and adjust the IFDEF'}
{$IFEND}
function ALScanTime(const S: AnsiString; var Pos: Integer; var Time: TDateTime;
  const AFormatSettings: TALFormatSettings): Boolean; overload;
var
  BaseHour: Integer;
  Hour, Min, Sec, MSec: Word;
  Junk: Byte;
begin
  Result := False;
  BaseHour := -1;
  if ALScanString(S, Pos, AFormatSettings.TimeAMString) or ALScanString(S, Pos, 'AM') then
    BaseHour := 0
  else if ALScanString(S, Pos, AFormatSettings.TimePMString) or ALScanString(S, Pos, 'PM') then
    BaseHour := 12;
  if BaseHour >= 0 then ALScanBlanks(S, Pos);
  if not ALScanNumber(S, Pos, Hour, Junk) then Exit;
  Min := 0;
  Sec := 0;
  MSec := 0;
  if ALScanChar(S, Pos, AFormatSettings.TimeSeparator) then
  begin
    if not ALScanNumber(S, Pos, Min, Junk) then Exit;
    if ALScanChar(S, Pos, AFormatSettings.TimeSeparator) then
    begin
      if not ALScanNumber(S, Pos, Sec, Junk) then Exit;
      if ALScanChar(S, Pos, AFormatSettings.DecimalSeparator) then
        if not ALScanNumber(S, Pos, MSec, Junk) then Exit;
    end;
  end;
  if BaseHour < 0 then
    if ALScanString(S, Pos, AFormatSettings.TimeAMString) or ALScanString(S, Pos, 'AM') then
      BaseHour := 0
    else
      if ALScanString(S, Pos, AFormatSettings.TimePMString) or ALScanString(S, Pos, 'PM') then
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

{****************************************************************}
function ALTryStrToDate(const S: AnsiString; out Value: TDateTime;
  const AFormatSettings: TALFormatSettings): Boolean;
var
  Pos: Integer;
begin
  Pos := low(ansiString);
  Result := ALScanDate(S, Pos, Value, AFormatSettings) and (Pos > High(S));
end;

{***************************************}
function ALStrToDate(const S: AnsiString;
  const AFormatSettings: TALFormatSettings): TDateTime;
begin
  if not ALTryStrToDate(S, Result, AFormatSettings) then
    ALConvertErrorFmt(@System.SysConst.SInvalidDate, [S]);
end;

{****************************************************************}
function ALTryStrToTime(const S: AnsiString; out Value: TDateTime;
  const AFormatSettings: TALFormatSettings): Boolean;
var
  Pos: Integer;
begin
  Pos := low(ansiString);
  Result := ALScanTime(S, Pos, Value, AFormatSettings) and (Pos > High(S));
end;

{***************************************}
function ALStrToTime(const S: AnsiString;
  const AFormatSettings: TALFormatSettings): TDateTime;
begin
  if not ALTryStrToTime(S, Result, AFormatSettings) then
    ALConvertErrorFmt(@System.SysConst.SInvalidTime, [S]);
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.TryStrToDateTime is still the same and adjust the IFDEF'}
{$IFEND}
function ALTryStrToDateTime(const S: AnsiString; out Value: TDateTime;
  const AFormatSettings: TALFormatSettings): Boolean;
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

  // jump over all the non-numeric characters before the date data
  // if the format starts with era name, do not skip any character.
  if AFormatSettings.ShortDateFormat[Low(ansistring)] <> 'g' then
    ALScanToNumber(S, Pos);

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
          Stop := ALScanString(S, BlankPos, AFormatSettings.TimeAMString) or
                  ALScanString(S, BlankPos, 'AM') or
                  ALScanString(S, BlankPos, AFormatSettings.TimePMString) or
                  ALScanString(S, BlankPos, 'PM');

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

{*******************************************}
function ALStrToDateTime(const S: AnsiString;
  const AFormatSettings: TALFormatSettings): TDateTime;
begin
  if not ALTryStrToDateTime(S, Result, AFormatSettings) then
    ALConvertErrorFmt(@System.SysConst.SInvalidDateTime, [S]);
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system._ValLong is still the same and adjust the IFDEF'}
{$IFEND}
// Hex : ( '$' | 'X' | 'x' | '0X' | '0x' ) [0-9A-Fa-f]*
// Dec : ( '+' | '-' )? [0-9]*
function _ALValLong(const S: ansiString; var Code: Integer): Integer;
{$IFDEF PUREPASCAL}
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
  {$IF CompilerVersion <= 31} // berlin
    {$IF not (defined(CPUX64) and not defined(EXTERNALLINKER))}
    Dig := 0;
    {$IFEND}
  {$IFEND}
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
{$ELSE !PUREPASCAL}
asm
{       FUNCTION _ValLong( s: AnsiString; VAR code: Integer ) : Longint;        }
{     ->EAX     Pointer to string       }
{       EDX     Pointer to code result  }
{     <-EAX     Result                  }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        PUSH    EAX             { save for the error case       }

        TEST    EAX,EAX
        JE      @@empty

        XOR     EAX,EAX
        XOR     EBX,EBX
        MOV     EDI,07FFFFFFFH / 10     { limit }

@@blankLoop:
        MOV     BL,[ESI]
        INC     ESI
        CMP     BL,' '
        JE      @@blankLoop

@@endBlanks:
        MOV     CH,0
        CMP     BL,'-'
        JE      @@minus
        CMP     BL,'+'
        JE      @@plus

@@checkDollar:
        CMP     BL,'$'
        JE      @@dollar

        CMP     BL, 'x'
        JE      @@dollar
        CMP     BL, 'X'
        JE      @@dollar
        CMP     BL, '0'
        JNE     @@firstDigit
        MOV     BL, [ESI]
        INC     ESI
        CMP     BL, 'x'
        JE      @@dollar
        CMP     BL, 'X'
        JE      @@dollar
        TEST    BL, BL
        JE      @@endDigits
        JMP     @@digLoop

@@firstDigit:
        TEST    BL,BL
        JE      @@error

@@digLoop:
        SUB     BL,'0'
        CMP     BL,9
        JA      @@error
        CMP     EAX,EDI         { value > limit ?       }
        JA      @@overFlow
        LEA     EAX,[EAX+EAX*4]
        ADD     EAX,EAX
        ADD     EAX,EBX         { fortunately, we can't have a carry    }

        MOV     BL,[ESI]
        INC     ESI

        TEST    BL,BL
        JNE     @@digLoop

@@endDigits:
        DEC     CH
        JE      @@negate
        TEST    EAX,EAX
        JGE     @@successExit
        JMP     @@overFlow

@@empty:
        INC     ESI
        JMP     @@error

@@negate:
        NEG     EAX
        JLE     @@successExit
        JS      @@successExit           { to handle 2**31 correctly, where the negate overflows }

@@error:
@@overFlow:
        POP     EBX
        SUB     ESI,EBX
        JMP     @@exit

@@minus:
        INC     CH
@@plus:
        MOV     BL,[ESI]
        INC     ESI
        JMP     @@checkDollar

@@dollar:
        MOV     EDI,0FFFFFFFH

        MOV     BL,[ESI]
        INC     ESI
        TEST    BL,BL
        JZ      @@empty

@@hDigLoop:
        CMP     BL,'a'
        JB      @@upper
        SUB     BL,'a' - 'A'
@@upper:
        SUB     BL,'0'
        CMP     BL,9
        JBE     @@digOk
        SUB     BL,'A' - '0'
        CMP     BL,5
        JA      @@error
        ADD     BL,10
@@digOk:
        CMP     EAX,EDI
        JA      @@overFlow
        SHL     EAX,4
        ADD     EAX,EBX

        MOV     BL,[ESI]
        INC     ESI

        TEST    BL,BL
        JNE     @@hDigLoop

        DEC     CH
        JNE     @@successExit
        NEG     EAX

@@successExit:
        POP     ECX                     { saved copy of string pointer  }
        XOR     ESI,ESI         { signal no error to caller     }

@@exit:
        MOV     [EDX],ESI
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system._ValInt64 is still the same and adjust the IFDEF'}
{$IFEND}
function _ALValInt64(const s: AnsiString; var code: Integer): Int64;
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
  {$IF CompilerVersion <= 31} // berlin
    {$IF not (defined(CPUX64) and not defined(EXTERNALLINKER))}
    Dig := 0;
    {$IFEND}
  {$IFEND}
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

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system._ValUInt64 is still the same and adjust the IFDEF'}
{$IFEND}
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
  {$IF CompilerVersion <= 31} // berlin
    // avoid E1036: Variable 'dig' might not have been initialized
    {$IF not (defined(CPUX64) and not defined(EXTERNALLINKER))}
    dig := 0;
    {$IFEND}
  {$IFEND}
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

{***********************************************************************}
function ALTryStrToInt(const S: AnsiString; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Value := _ALValLong(S, E);
  Result := E = 0;
end;

{************************************************}
function ALStrToInt(const S: AnsiString): Integer;
var
  E: Integer;
begin
  Result := _ALValLong(S, E);
  if E <> 0 then raise EConvertError.CreateResFmt(@System.SysConst.SInvalidInteger, [S]);
end;

{*********************************************************************}
function ALStrToIntDef(const S: AnsiString; Default: Integer): Integer;
var
  E: Integer;
begin
  Result := _ALValLong(S, E);
  if E <> 0 then Result := Default;
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

{***********************************************************************}
function ALTryStrToInt64(const S: AnsiString; out Value: Int64): Boolean;
var
  E: Integer;
begin
  Value := _ALValInt64(S, E);
  Result := E = 0;
end;

{************************************************}
function ALStrToInt64(const S: AnsiString): Int64;
var
  E: Integer;
begin
  Result := _ALValInt64(S, E);
  if E <> 0 then raise EConvertError.CreateResFmt(@System.SysConst.SInvalidInteger, [S]);
end;

{*************************************************************************}
function ALStrToInt64Def(const S: AnsiString; const Default: Int64): Int64;
var
  E: Integer;
begin
  Result := _ALValInt64(S, E);
  if E <> 0 then Result := Default;
end;

{***}
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

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils._IntToStr32 is still the same and adjust the IFDEF'}
{$IFEND}
function _ALIntToStr32(Value: Cardinal; Negative: Boolean): AnsiString;
var
  I, J, K : Cardinal;
  Digits  : Integer;
  P       : PAnsiChar;
  NewLen  : Integer;
begin
  I := Value;
  if I >= 10000 then
    if I >= 1000000 then
      if I >= 100000000 then
        Digits := 9 + Ord(I >= 1000000000)
      else
        Digits := 7 + Ord(I >= 10000000)
    else
      Digits := 5 + Ord(I >= 100000)
  else
    if I >= 100 then
      Digits := 3 + Ord(I >= 1000)
    else
      Digits := 1 + Ord(I >= 10);
  NewLen  := Digits + Ord(Negative);
  SetLength(Result, NewLen);
  P := PAnsiChar(Result);
  P^ := AnsiChar('-');
  Inc(P, Ord(Negative));
  if Digits > 2 then
    repeat
      J  := I div 100;           {Dividend div 100}
      K  := J * 100;
      K  := I - K;               {Dividend mod 100}
      I  := J;                   {Next Dividend}
      Dec(Digits, 2);
      PWord(P + Digits)^ := Word(ALTwoDigitLookup[K]);
    until Digits <= 2;
  if Digits = 2 then
    PWord(P+ Digits-2)^ := Word(ALTwoDigitLookup[I])
  else
    PAnsiChar(P)^ := AnsiChar(I or ord(AnsiChar('0')));
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils._IntToStr64 is still the same and adjust the IFDEF'}
{$IFEND}
function _ALIntToStr64(Value: UInt64; Negative: Boolean): AnsiString;
var
  I64, J64, K64      : UInt64;
  I32, J32, K32, L32 : Cardinal;
  Digits             : Byte;
  P                  : PAnsiChar;
  NewLen             : Integer;
begin
  {Within Integer Range - Use Faster Integer Version}
  if (Negative and (Value <= High(Integer))) or
     (not Negative and (Value <= High(Cardinal))) then
    exit(_ALIntToStr32(Value, Negative));

  I64 := Value;
  if I64 >= 100000000000000 then
    if I64 >= 10000000000000000 then
      if I64 >= 1000000000000000000 then
        if I64 >= 10000000000000000000 then
          Digits := 20
        else
          Digits := 19
      else
        Digits := 17 + Ord(I64 >= 100000000000000000)
    else
      Digits := 15 + Ord(I64 >= 1000000000000000)
  else
    if I64 >= 1000000000000 then
      Digits := 13 + Ord(I64 >= 10000000000000)
    else
      if I64 >= 10000000000 then
        Digits := 11 + Ord(I64 >= 100000000000)
      else
        Digits := 10;
  NewLen  := Digits + Ord(Negative);
  SetLength(Result, NewLen);
  P := PAnsiChar(Result);
  P^ := AnsiChar('-');
  Inc(P, Ord(Negative));
  if Digits = 20 then
  begin
    P^ := AnsiChar('1');
    Inc(P);
    Dec(I64, 10000000000000000000);
    Dec(Digits);
  end;
  if Digits > 17 then
  begin {18 or 19 Digits}
    if Digits = 19 then
    begin
      P^ := AnsiChar('0');
      while I64 >= 1000000000000000000 do
      begin
        Dec(I64, 1000000000000000000);
        Inc(P^);
      end;
      Inc(P);
    end;
    P^ := AnsiChar('0');
    while I64 >= 100000000000000000 do
    begin
      Dec(I64, 100000000000000000);
      Inc(P^);
    end;
    Inc(P);
    Digits := 17;
  end;
  J64 := I64 div 100000000;
  K64 := I64 - (J64 * 100000000); {Remainder = 0..99999999}
  I32 := K64;
  J32 := I32 div 100;
  K32 := J32 * 100;
  K32 := I32 - K32;
  PWord(P + Digits - 2)^ := Word(ALTwoDigitLookup[K32]);
  I32 := J32 div 100;
  L32 := I32 * 100;
  L32 := J32 - L32;
  PWord(P + Digits - 4)^ := Word(ALTwoDigitLookup[L32]);
  J32 := I32 div 100;
  K32 := J32 * 100;
  K32 := I32 - K32;
  PWord(P + Digits - 6)^ := Word(ALTwoDigitLookup[K32]);
  PWord(P + Digits - 8)^ := Word(ALTwoDigitLookup[J32]);
  Dec(Digits, 8);
  I32 := J64; {Dividend now Fits within Integer - Use Faster Version}
  if Digits > 2 then
    repeat
      J32 := I32 div 100;
      K32 := J32 * 100;
      K32 := I32 - K32;
      I32 := J32;
      Dec(Digits, 2);
      PWord(P + Digits)^ := Word(ALTwoDigitLookup[K32]);
    until Digits <= 2;
  if Digits = 2 then
    PWord(P + Digits-2)^ := Word(ALTwoDigitLookup[I32])
  else
    P^ := AnsiChar(I32 or ord(AnsiChar('0')));
end;

{**********************************************}
function ALIntToStr(Value: Integer): AnsiString;
begin
  if Value < 0 then
    Result := _ALIntToStr32(-Value, True)
  else
    Result := _ALIntToStr32(Value, False);
end;

{******************************************************}
procedure ALIntToStr(Value: Integer; var s: ansiString);
begin
  if Value < 0 then
    s := _ALIntToStr32(-Value, True)
  else
    s := _ALIntToStr32(Value, False);
end;

{********************************************}
function ALIntToStr(Value: Int64): AnsiString;
begin
  if Value < 0 then
    Result := _ALIntToStr64(-Value, True)
  else
    Result := _ALIntToStr64(Value, False);
end;

{****************************************************}
procedure ALIntToStr(Value: Int64; var s: ansiString);
begin
  if Value < 0 then
    s := _ALIntToStr64(-Value, True)
  else
    s := _ALIntToStr64(Value, False);
end;

{$ENDIF !NEXTGEN}

{***************************************************}
procedure ALIntToStrU(Value: Integer; var s: String);
begin
  s := IntToStr(Value);
end;

{*****************************************}
function ALIntToStrU(Value: Int64): String;
begin
  result := IntToStr(Value);
end;

{*************************************************}
procedure ALIntToStrU(Value: Int64; var s: String);
begin
  s := IntToStr(Value);
end;

{*******************************************}
function ALIntToStrU(Value: Integer): String;
begin
  result := IntToStr(Value);
end;

{$IFNDEF NEXTGEN}

{**************************************************}
function ALStrToUInt64(const S: ansistring): UInt64;
var
  E: Integer;
begin
  Result := _ALValUInt64(S, E);
  if E <> 0 then raise EConvertError.CreateResFmt(@System.SysConst.SInvalidInteger, [S]);
end;

{****************************************************************************}
function ALStrToUInt64Def(const S: ansistring; const Default: UInt64): UInt64;
var
  E: Integer;
begin
  Result := _ALValUInt64(S, E);
  if E <> 0 then Result := Default;
end;

{*************************************************************************}
function ALTryStrToUInt64(const S: ansistring; out Value: UInt64): Boolean;
var
  E: Integer;
begin
  Value := _ALValUInt64(S, E);
  Result := E = 0;
end;

{************************************************}
function ALUIntToStr(Value: Cardinal): AnsiString;
begin
  Result := _ALIntToStr32(Value, False);
end;

{**********************************************}
function ALUIntToStr(Value: UInt64): AnsiString;
begin
  Result := _ALIntToStr64(Value, False);
end;

{$ENDIF !NEXTGEN}

{*********************************************}
function ALUIntToStrU(Value: Cardinal): String;
begin
  Result := UIntToStr(Value);
end;

{*******************************************}
function ALUIntToStrU(Value: UInt64): String;
begin
  Result := UIntToStr(Value);
end;

{$IFNDEF NEXTGEN}

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

{***************************************************************}
function ALIntToHex(Value: Integer; Digits: Integer): AnsiString;
begin
  Result := _ALIntToHex(Cardinal(Value), Digits);
end;

{*************************************************************}
function ALIntToHex(Value: Int64; Digits: Integer): AnsiString;
begin
  Result := _ALIntToHex(Value, digits);
end;

{**************************************************************}
function ALIntToHex(Value: UInt64; Digits: Integer): AnsiString;
begin
  Result := _ALIntToHex(Value, digits);
end;

{**************************************************************************}
procedure _ALBinToHex(Buffer: PAnsiChar; Text: PAnsiChar; BufSize: Integer); overload;
const
  Convert: array[0..15] of AnsiChar = AnsiString('0123456789abcdef');
var
  I: Integer;
begin
  for I := 0 to BufSize - 1 do
  begin
    Text[0] := Convert[Byte(Buffer[I]) shr 4];
    Text[1] := Convert[Byte(Buffer[I]) and $F];
    Inc(Text, 2);
  end;
end;

{******************************************************************************}
Function  ALTryBinToHex(const aBin: AnsiString; out Value: AnsiString): boolean;
begin
  if aBin = '' then exit(false);
  setlength(Value,length(aBin) * 2);
  _ALBinToHex(PansiChar(aBin),pansiChar(Value),length(aBin));
  result := true;
end;

{*******************************************************}
Function  ALBinToHex(const aBin: AnsiString): AnsiString;
begin
  if not ALTryBinToHex(aBin, Result) then
    raise Exception.Create('Bad binary value');
end;

{***************************************************************************************}
Function  ALTryBinToHex(const aBin; aBinSize : Cardinal; out Value: AnsiString): boolean;
begin
  if aBinSize = 0 then exit(false);
  setlength(Value, aBinSize * 2);
  _ALBinToHex(@aBin,pansiChar(Value),aBinSize);
  result := true;
end;

{****************************************************************}
Function  ALBinToHex(const aBin; aBinSize : Cardinal): AnsiString;
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

{*******************************************************}
Function  ALHexToBin(const aHex: AnsiString): AnsiString;
begin
  if not ALTryHexToBin(aHex, Result) then
    raise Exception.Create('Bad hex value');
end;

{$ENDIF !NEXTGEN}

{*********************************************************************************************************************}
procedure _ALBinToHex(const Buffer: TBytes; BufOffset: Integer; var Text: TBytes; TextOffset: Integer; Count: Integer); overload;
const
  B2HConvert: array[0..15] of Byte = ($30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $61, $62, $63, $64, $65, $66);
var
  I: Integer;
begin
  for I := 0  to Count - 1 do
  begin
    Text[TextOffset + I * 2] := B2HConvert[Buffer[BufOffset + I] shr 4];
    Text[TextOffset + I * 2 + 1] := B2HConvert[Buffer[BufOffset + I] and $0F];
  end;
end;

{***********************************************************************}
Function  ALTryBinToHexU(const aBin: TBytes; out Value: String): boolean;
var bufOut: TBytes;
begin
  if length(aBin) = 0 then exit(false);
  setlength(bufOut,length(aBin) * 2);
  _ALBintoHex(aBin, // Buffer: TBytes
              0, // BufOffset: Integer;
              bufOut, // Text: TBytes;
              0, // TextOffset: Integer;
              length(aBin)); // Count: Integer
  Value := Tencoding.UTF8.GetString(bufOut); // UTF8 is good because bufOut must contain only low ascii chars
  result := true;
end;

{************************************************}
Function  ALBinToHexU(const aBin: TBytes): String;
begin
  if not ALTryBinToHexU(aBin, Result) then
    raise Exception.Create('Bad binary value');
end;

{************************************************************************************}
Function  ALTryBinToHexU(const aBin; aBinSize : Cardinal; out Value: String): boolean;
var bufOut: TBytes;
begin
  if aBinSize = 0 then exit(false);
  setlength(bufOut,aBinSize * 2);
  _ALBintoHex(Tbytes(@aBin), // Buffer: TBytes
              0, // BufOffset: Integer;
              bufOut, // Text: TBytes;
              0, // TextOffset: Integer;
              aBinSize); // Count: Integer
  Value := Tencoding.UTF8.GetString(bufOut); // UTF8 is good because bufOut must contain only low ascii chars
  result := true;
end;

{*************************************************************}
Function  ALBinToHexU(const aBin; aBinSize : Cardinal): String;
begin
  if not ALTryBinToHexU(aBin, aBinSize, Result) then
    raise Exception.Create('Bad binary value');
end;

{***********************************************************************}
Function  ALTryHexToBinU(const aHex: String; out Value: TBytes): boolean;
var l: integer;
    {$IF CompilerVersion < 30}{Delphi seattle}
    aByteHex: Tbytes;
    {$IFEND}
begin
  {$IF CompilerVersion >= 30}{Delphi seattle}
  l := length(aHex);
  if (l = 0) or (l mod 2 <> 0) then exit(False);
  setlength(Value,l div 2);
  result := HexToBin(PChar(aHex), // Text
                     0, // TextOffset
                     Value, //Buffer
                     0, // BufOffset
                     length(Value)) = l div 2;
  {$ELSE}
  aByteHex := Tencoding.UTF8.GetBytes(aHex);
  l := length(aByteHex);
  if (l = 0) or (l mod 2 <> 0) then exit(False);
  setlength(Value,l div 2);
  result := HexToBin(aByteHex, // Text
                     0, // TextOffset
                     Value, //Buffer
                     0, // BufOffset
                     length(Value)) = l div 2;
  {$IFEND}
end;

{************************************************}
Function  ALHexToBinU(const aHex: String): TBytes;
begin
  if not ALTryHexToBinU(aHex, Result) then
    raise Exception.Create('Bad hex value');
end;

{$IFNDEF NEXTGEN}

{***************************************************************}
function ALIntToBit(value: integer; digits: integer): ansistring;
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
    Lst: TalStringList;
    I,j: integer;
    P: UInt64;
begin

  //
  //ex convert ABCD to int
  //
  // ABCD = (26*26*26) * 0 + (26*26) * 1 + (26) * 2 + 3 = 731
  //                     A             B          C   D

  Lst := TalStringList.Create;
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

/////////////////////////////////
////// Base64 (ansiString) //////
/////////////////////////////////

//
// Taken from https://github.com/synopse/mORMot.git
// https://synopse.info
// http://mormot.net
//
// NOTE: the original function was different from the Unicode in the way that it's
//       don't ignore Characters outside alphabet (and even don't raise any exception)
//       also the purepascal implementation seam to be 2x more faster than the
//       ASM implementation. So i decide to remove the ASM implementation
//       and i make that Base64decode crash with invalid chars
//       https://synopse.info/forum/viewtopic.php?pid=26173#p26173
//

{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if https://github.com/synopse/mORMot.git SynCommons.pas was not updated from references\mORMot\SynCommons.pas and adjust the IFDEF'}
{$IFEND}

type
  TBase64Enc = array[0..63] of AnsiChar;
  TBase64Dec = array[AnsiChar] of shortint;
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
var i: integer;
    c: cardinal;
begin
  result := len div 3;
  for i := 1 to result do begin
    c := ord(sp[0]) shl 16 + ord(sp[1]) shl 8 + ord(sp[2]);
    rp[0] := b64enc[(c shr 18) and $3f];
    rp[1] := b64enc[(c shr 12) and $3f];
    rp[2] := b64enc[(c shr 6) and $3f];
    rp[3] := b64enc[c and $3f];
    inc(rp,4);
    inc(sp,3);
  end;
end;

{*******************************************************}
procedure Base64Decode(sp,rp: PAnsiChar; len: NativeInt);
begin
  len := len shl 2; // len was the number of 4 chars chunks in sp
  if (len>0) and (ConvertBase64ToBin[sp[len-2]]>=0) then
    if ConvertBase64ToBin[sp[len-1]]>=0 then else
      dec(len) else
      dec(len,2); // adjust for Base64AnyDecode() algorithm
  if not Base64AnyDecode(ConvertBase64ToBin,sp,rp,len) then
    raise Exception.Create(sInvalidbase64String);
end;

{***********************************************************************}
procedure Base64EncodeTrailing(rp, sp: PAnsiChar; len: cardinal); inline;
var c: cardinal;
begin
  case len of
    1: begin
      c := ord(sp[0]) shl 4;
      rp[0] := b64enc[(c shr 6) and $3f];
      rp[1] := b64enc[c and $3f];
      rp[2] := '=';
      rp[3] := '=';
    end;
    2: begin
      c := ord(sp[0]) shl 10 + ord(sp[1]) shl 2;
      rp[0] := b64enc[(c shr 12) and $3f];
      rp[1] := b64enc[(c shr 6) and $3f];
      rp[2] := b64enc[c and $3f];
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
begin
  result := 0;
  if (len=0) then exit;
  if (len and 3<>0) then raise Exception.Create(sInvalidbase64String);
  if ConvertBase64ToBin[sp[len-2]]>=0 then
    if ConvertBase64ToBin[sp[len-1]]>=0 then
      result := 0 else
      result := 1 else
      result := 2;
  result := (len shr 2)*3-result;
end;

{***************************************************************************}
procedure Base64ToBin(sp: PAnsiChar; len: NativeInt; var result: AnsiString);
var resultLen: NativeInt;
begin
  resultLen := Base64ToBinLength(sp,len);
  if resultLen=0 then
    result := '' else begin
    SetString(result,nil,resultLen);
    Base64Decode(sp,pointer(result),len shr 2);
  end;
end;

{********************************************************************************}
function  ALBase64EncodeString(const P: PansiChar; const ln: Integer): AnsiString;
begin
  result := '';
  if ln=0 then exit;
  SetLength(result,BinToBase64Length(ln));
  Base64Encode(pointer(result),P,ln);
end;

{**************************************************************}
function  ALBase64EncodeString(const S: AnsiString): AnsiString;
var len: integer;
begin
  result := '';
  len := length(s);
  if len=0 then exit;
  SetLength(result,BinToBase64Length(len));
  Base64Encode(pointer(result),pointer(s),len);
end;

{********************************************************************************}
function  ALBase64DecodeString(const P: PansiChar; const ln: Integer): AnsiString;
begin
  Base64ToBin(P,ln,result);
end;

{**************************************************************}
function  ALBase64DecodeString(const S: AnsiString): AnsiString;
begin
  Base64ToBin(pointer(s),length(s),result);
end;

{*********************}
{$ZEROBASEDSTRINGS OFF} // << the guy who introduce zero base string in delphi is just a mix of a Monkey and a Donkey !
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
{$IF defined(_ZEROBASEDSTRINGS_ON)}
  {$ZEROBASEDSTRINGS ON}
{$IFEND}

{******************************************************************}
function  ALBase64DecodeStringMIME(const S: AnsiString): AnsiString;
begin

  //https://en.wikipedia.org/wiki/Base64
  //MIME specifies that any extra-alphabetic characters must be ignored by a
  //compliant decoder, but here we just ignore the #13#10

  result := ALBase64DecodeString(AlStringReplace(s, #13#10, '', [rfReplaceAll]));

end;

////////////////////////////////////
////// Base64 (UnicodeString) //////
////////////////////////////////////

{$ENDIF !NEXTGEN}
{$IF CompilerVersion >= 31} // berlin

{*}
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

{*****************************************************************************************}
Function  ALBase64EncodeStringU(const S: String; const AEncoding: TEncoding = nil): String;
var BufIn: TBytes;
begin
  if assigned(AEncoding) then BufIn := AEncoding.GetBytes(S)
  else BufIn := TEncoding.unicode.GetBytes(S);
  result := _GetBase64Encoding.EncodeBytesToString(BufIn);
end;

{*****************************************************************************************}
Function  ALBase64DecodeStringU(const S: String; const AEncoding: TEncoding = nil): String;
var BufOut: TBytes;
begin
  BufOut := _GetBase64Encoding.DecodeStringToBytes(S);
  if assigned(AEncoding) then result := AEncoding.GetString(BufOut)
  else result := TEncoding.unicode.GetString(BufOut);
end;

{**********************************************************}
Function  ALBase64EncodeBytesU(const Bytes: Tbytes): String;
begin
  result := _GetBase64Encoding.EncodeBytesToString(Bytes);
end;

{********************************************************************************}
Function  ALBase64EncodeBytesU(const Bytes: pointer; const Size: Integer): String;
begin
  result := _GetBase64Encoding.EncodeBytesToString(Bytes, Size);
end;

{******************************************************}
Function  ALBase64DecodeBytesU(const S: String): Tbytes;
begin
  result := _GetBase64Encoding.DecodeStringToBytes(S);
end;

{$IFEND CompilerVersion >= 31}

{$IFNDEF NEXTGEN}

{$IF CompilerVersion <= 30} // seattle

{*****************************************************************************************}
function ALBase64EncodeStringU(const S: String; const AEncoding: TEncoding = nil): String;
begin
  result := String(ALBase64EncodeString(AnsiString(S)));
end;

{*****************************************************************************************}
function ALBase64DecodeStringU(const S: String; const AEncoding: TEncoding = nil): String;
begin
  result := String(ALBase64DecodeString(AnsiString(S)));
end;

{**********************************************************}
function ALBase64EncodeBytesU(const Bytes: Tbytes): String;
begin
  result := String(ALBase64EncodeString(AnsiString(StringOf(Bytes))));
end;

{******************************************************}
function ALBase64DecodeBytesU(const S: String): Tbytes;
begin
  result := BytesOf(String(ALBase64DecodeString(AnsiString(S))));
end;

{$IFEND}

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

{*************************************************}
Function ALIsInteger(const S: AnsiString): Boolean;
var i: integer;
Begin
  result := ALIsDecimal(S) and ALTryStrToInt(S, i);
End;

{***********************************************}
Function ALIsInt64(const S: AnsiString): Boolean;
var i : int64;
Begin
  Result := ALIsDecimal(S) and ALTryStrToInt64(S, I);
End;

{**************************************************}
Function ALIsSmallInt(const S: AnsiString): Boolean;
var i : Integer;
Begin
  Result := ALIsDecimal(S) and ALTryStrToInt(S, I) and (i <= 32767) and (I >= -32768);
End;

{*******************************************************************************************}
Function  ALIsFloat (const S: AnsiString; const AFormatSettings: TALFormatSettings): Boolean;
var i: integer;
    aDouble: Double;
begin
  if S = '' then Exit(false);
  for i := low(s) to high(s) do begin
    if not (S[i] in ['0'..'9','-',AFormatSettings.DecimalSeparator]) then begin
      result := false;
      exit;
    end;
  end;
  result := ALTryStrToFloat(s,aDouble,AFormatSettings);
end;

{*******************************************************************************************}
function ALFloatToStr(Value: Extended; const AFormatSettings: TALFormatSettings): AnsiString;
var
  Buffer: array[0..63] of AnsiChar;
begin
  SetString(Result, Buffer, ALFloatToText(Buffer, Value, fvExtended,
    ffGeneral, 15, 0, AFormatSettings));
end;

{***************************************************************************************************}
procedure ALFloatToStr(Value: Extended; var S: ansiString; const AFormatSettings: TALFormatSettings);
var
  Buffer: array[0..63] of AnsiChar;
begin
  SetString(S, Buffer, ALFloatToText(Buffer, Value, fvExtended,
    ffGeneral, 15, 0, AFormatSettings));
end;

{***************************************************************************************************}
function ALFloatToStrF(Value: Extended; Format: TFloatFormat;
  Precision, Digits: Integer; const AFormatSettings: TALFormatSettings): AnsiString;
var
  Buffer: array[0..63] of AnsiChar;
begin
  SetString(Result, Buffer, ALFloatToText(Buffer, Value, fvExtended,
    Format, Precision, Digits, AFormatSettings));
end;

{$ENDIF !NEXTGEN}

{***************************}
{$WARN SYMBOL_DEPRECATED OFF}
function ALIsDecimalU(const S: String; const RejectPlusMinusSign: boolean = False): boolean;
var i: integer;
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

{**********************************************}
Function ALIsIntegerU(const S: String): Boolean;
var i: integer;
Begin
  result := ALIsDecimalU(S) and ALTryStrToIntU(S, i);
End;

{********************************************}
Function ALIsInt64U(const S: String): Boolean;
var i : int64;
Begin
  Result := ALIsDecimalU(S) and ALTryStrToInt64U(S, I);
End;

{***********************************************}
Function ALIsSmallIntU(const S: String): Boolean;
var i : Integer;
Begin
  Result := ALIsDecimalU(S) and ALTryStrToIntU(S, I) and (i <= 32767) and (I >= -32768);
End;

{***************************}
{$WARN SYMBOL_DEPRECATED OFF}
Function  ALIsFloatU(const S: String; const AFormatSettings: TALFormatSettingsU): Boolean;
var i: integer;
    aDouble: Double;
begin
  if S = '' then Exit(false);
  for i := low(s) to high(s) do begin
    if not CharInSet(S[i], ['0'..'9','-',AFormatSettings.DecimalSeparator]) then begin
      result := false;
      exit;
    end;
  end;
  result := ALTryStrToFloatU(s,aDouble,AFormatSettings);
end;
{$WARN SYMBOL_DEPRECATED ON}

{****************************************************************************************}
function ALFloatToStrU(Value: Extended; const AFormatSettings: TALFormatSettingsU): String;
begin
  result := FloatToStr(Value, AFormatSettings);
end;

{************************************************************************************************}
procedure ALFloatToStrU(Value: Extended; var S: String; const AFormatSettings: TALFormatSettingsU);
begin
  S := FloatToStr(Value, AFormatSettings);
end;

{$IFNDEF NEXTGEN}

{*******************************************************************************************}
function  ALCurrToStr(Value: Currency; const AFormatSettings: TALFormatSettings): AnsiString;
var
  Buffer: array[0..63] of AnsiChar;
begin
  SetString(Result, Buffer, ALFloatToText(Buffer, Value, fvCurrency,
    ffGeneral, 0, 0, AFormatSettings));
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if declaration below in system.Sysutils is still the same and adjust the IFDEF'}
{$IFEND}
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

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.TestAndClearFPUExceptions is still the same and adjust the IFDEF'}
{$IFEND}
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

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.TestAndClearSSEExceptions is still the same and adjust the IFDEF'}
{$IFEND}
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
{$IFEND CPUX64}
{$WARN SYMBOL_PLATFORM ON}

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.InternalTextToExtended is still the same and adjust the IFDEF'}
{$IFEND}
//this function is not threadsafe because of Set8087CW
//!! this is amazing !!
{$WARN SYMBOL_PLATFORM OFF}
function ALInternalTextToExtended(
  ABuffer: PansiChar;
  var AValue: Extended;
  const AFormatSettings: TALFormatSettings): Boolean;
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
  LCurrChar: ansiChar;

  procedure NextChar;
  begin
    LCurrChar := PansiChar(ABuffer)^;
    Inc(PansiChar(ABuffer));
  end;

  procedure SkipWhitespace();
  begin
    { Skip white spaces }
    while LCurrChar = ' ' do
      NextChar;
  end;

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

  function ReadNumber(var AOut: Extended): Integer;
  begin
    Result := 0;
    //while LCurrChar.IsDigit do
    while CharInSet(LCurrChar, ['0'..'9']) do
    begin
      AOut := AOut * 10;
      AOut := AOut + Ord(LCurrChar) - Ord('0');

      NextChar();
      Inc(Result);
    end;
  end;

  function ReadExponent: SmallInt;
  var
    LSign: SmallInt;
  begin
    LSign := ReadSign();
    Result := 0;
    //while LCurrChar.IsDigit do
    while CharInSet(LCurrChar, ['0'..'9']) do
    begin
      Result := Result * 10;
      Result := Result + Ord(LCurrChar) - Ord('0');
      NextChar();
    end;

    if Result > CMaxExponent then
      Result := CMaxExponent;

    Result := Result * LSign;
  end;

var
  IntPart, FracPart: Integer;
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
{$IFEND Defined(CPUX64)}
{$ENDIF EXTERNALLINKER}

  { Skip white spaces }
  SkipWhitespace();

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

      if (IntPart = 0) and (FracPart = 0) then
        exit; // Reject "E3" or ".E1" case.

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
  end;

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
{$WARN SYMBOL_PLATFORM ON}

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.InternalTextToCurrency is still the same and adjust the IFDEF'}
{$IFEND}
//this function is not threadsafe because of Set8087CW
//!! this is amazing !!
function ALInternalTextToCurrency(
  ABuffer: PansiChar;
  var AValue: Currency;
  const AFormatSettings: TALFormatSettings): Boolean;
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
{$IFEND CPUX86 and !EXTENDEDHAS10BYTES}
{$ELSE}
  {$MESSAGE ERROR 'Unknown platform'}
{$ENDIF CPUX86}
{$ENDIF EXTERNALLINKER}
  LPower: Integer;
  LSign: SmallInt;
  LResult: Extended;
  LCurrChar: ansiChar;

  procedure NextChar;
  begin
    LCurrChar := ABuffer^;
    Inc(ABuffer);
  end;

  procedure SkipWhitespace();
  begin
    { Skip white spaces }
    while LCurrChar = ' ' do
      NextChar;
  end;

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

  function ReadNumber(var AOut: Extended): Integer;
  begin
    Result := 0;
    while CharInSet(LCurrChar, ['0'..'9']) do
    begin
      AOut := AOut * 10;
      AOut := AOut + Ord(LCurrChar) - Ord('0');

      NextChar();
      Inc(Result);
    end;
  end;

  function ReadExponent: SmallInt;
  var
    LSign: SmallInt;
  begin
    LSign := ReadSign();
    Result := 0;
    while CharInSet(LCurrChar, ['0'..'9']) do
    begin
      Result := Result * 10;
      Result := Result + Ord(LCurrChar) - Ord('0');
      NextChar();
    end;

    if Result > CMaxExponent then
      Result := CMaxExponent;

    Result := Result * LSign;
  end;

var
  IntPart, FracPart: Integer;
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

  { Skip white spaces }
  SkipWhitespace();

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

      if (IntPart = 0) and (FracPart = 0) then
        exit; // Reject "E3" or ".E1" case.

      { Skip white spaces }
      SkipWhitespace();

      { Continue only if the buffer is depleted }
      if LCurrChar = #0 then
      begin
        { Calculate the final number }
        LResult := Power10(LResult, LPower) * LSign;

        Currency(AValue) := LResult;

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
  end;

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
{$ELSE !EXTENDEDHAS10BYTES}
const
  CExponent = 'E'; // DO NOT LOCALIZE;
  CPlus = '+';     // DO NOT LOCALIZE;
  CMinus = '-';    // DO NOT LOCALIZE;
var
  LPower: Integer;
  LSign: SmallInt;
  BufIndex: Integer;

  procedure SkipWhitespace;
  begin
    { Skip white spaces }
    while ABuffer[BufIndex] = ' ' do
      Inc(BufIndex);
  end;

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

  function ReadNumberPart: ansistring;
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
  IntPart, FracPart: ansistring;
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
          IntPart := IntPart + Copy(FracPart, Low(ansiString), LPower);
          FracPart := Copy(FracPart, Low(ansiString) + LPower);
        end
        else if LPower < 0 then
        begin
          LPower := - LPower;
          if Length(IntPart) < LPower then
            IntPart := StringOfChar(AnsiChar('0'), LPower) + IntPart;
          FracPart := Copy(IntPart, Low(ansiString) + (Length(IntPart) - LPower), LPower) + FracPart;
          IntPart := Copy(IntPart, Low(ansiString), Length(IntPart) - LPower);
        end;

        if IntPart = '' then
          IntPart := '0';
        U64 := _ALValInt64(IntPart, I);
        if I <> 0 then Exit; // error.
        if U64 > UInt64(Int64.MaxValue)+1 then Exit; // overflow error

        if (FracPart <> '') and (FracPart[low(FracPart)] >= '5') then
        begin
          RoundUp := True;
          if (FracPart[low(FracPart)] = '5') then
          begin
            RoundUp := False;
            for I := low(FracPart) + 1 to high(FracPart) do
              if IntPart[I] <> '0' then
              begin
                RoundUp := True;
                Break;
              end;
            // exact half -> False / not half -> True
            RoundUp := RoundUp or (IntPart[length(IntPart)] in ['1','3','5','7','9']);
          end;
          if RoundUp then
            Inc(U64); // U64 is UInt64. no overflow.
        end;

        if LSign < 0 then
        begin
          if U64 > UInt64(Int64.MaxValue)+1 then Exit;
          U64 := (not U64) + 1; // nagate;
        end
        else
          if U64 > Int64.MaxValue then Exit;
        PUInt64(@AValue)^ := U64;
        Result := True;
      end;
    end;
  end;
end;
{$IFEND !EXTENDEDHAS10BYTES}

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.TextToFloat is still the same and adjust the IFDEF'}
{$IFEND}
{$WARN SYMBOL_DEPRECATED OFF}
function ALTextToFloat(Buffer: PAnsiChar; var Value;
  ValueType: TFloatValue; const AFormatSettings: TALFormatSettings): Boolean;
{$IFDEF PUREPASCAL}
//var
//  S: string;
begin
//  S := string(AnsiString(Buffer));
//  Result := TextToFloat( PChar(s), Value, ValueType, AFormatSettings);
  if ValueType = fvExtended then
    Result := ALInternalTextToExtended(Buffer, Extended(Value), AFormatSettings)
  else
    Result := ALInternalTextToCurrency(Buffer, Currency(Value), AFormatSettings);
end;
{$ELSE !PUREPASCAL}
{$IFDEF X86ASM}
var
  Temp: Integer;
  CtrlWord: Word;
  DecimalSep: AnsiChar;
  SaveGOT: Integer;
asm //StackAligned
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
{$IFDEF PIC}
        PUSH    ECX
        CALL    ALGetGOT
        POP     EBX
        MOV     SaveGOT,EAX
{$ELSE !PIC}
        MOV     SaveGOT,0
        MOV     EBX,ECX
{$ENDIF !PIC}
        MOV     EAX,AFormatSettings
        MOV     AL,AnsiChar([EAX].TALFormatSettings.DecimalSeparator)
        MOV     DecimalSep,AL
        FSTCW   CtrlWord
        FCLEX
{$IFDEF PIC}
        MOV     EAX, SaveGOT
        FLDCW   [EAX].CWNear
{$ELSE !PIC}
        FLDCW   CWNear
{$ENDIF !PIC}
        FLDZ
        CALL    @@SkipBlanks
        MOV     BH, byte ptr [ESI]
        CMP     BH,'+'
        JE      @@1
        CMP     BH,'-'
        JNE     @@2
@@1:    INC     ESI
@@2:    MOV     ECX,ESI
        CALL    @@GetDigitStr
        XOR     EDX,EDX
        MOV     AL,[ESI]
        CMP     AL,DecimalSep
        JNE     @@3
        INC     ESI
        CALL    @@GetDigitStr
        NEG     EDX
@@3:    CMP     ECX,ESI
        JE      @@9
        MOV     AL, byte ptr [ESI]
        AND     AL,0DFH
        CMP     AL,'E'
        JNE     @@4
        INC     ESI
        PUSH    EDX
        CALL    @@GetExponent
        POP     EAX
        ADD     EDX,EAX
@@4:    CALL    @@SkipBlanks
        CMP     BYTE PTR [ESI],0
        JNE     @@9
        MOV     EAX,EDX
        CMP     BL,fvCurrency
        JNE     @@5
        ADD     EAX,4
@@5:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBX
        MOV     EBX,SaveGOT
        CALL    FPower10
        POP     EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        CMP     BH,'-'
        JNE     @@6
        FCHS
@@6:    CMP     BL,fvExtended
        JE      @@7
        FISTP   QWORD PTR [EDI]
        JMP     @@8
@@7:    FSTP    TBYTE PTR [EDI]
@@8:    FSTSW   AX
        TEST    AX,mIE+mOE
        JNE     @@10
        MOV     AL,1
        JMP     @@11
@@9:    FSTP    ST(0)
@@10:   XOR     EAX,EAX
@@11:   FCLEX
        FLDCW   CtrlWord
        FWAIT
        JMP     @@Exit

@@SkipBlanks:

@@21:   LODSB
        OR      AL,AL
        JE      @@22
        CMP     AL,' '
        JE      @@21
@@22:   DEC     ESI
        RET

// Process string of digits
// Out EDX = Digit count

@@GetDigitStr:

        XOR     EAX,EAX
        XOR     EDX,EDX
@@31:   LODSB
        SUB     AL,'0'+10
        ADD     AL,10
        JNC     @@32
{$IFDEF PIC}
        XCHG    SaveGOT,EBX
        FIMUL   [EBX].calDCon10
        XCHG    SaveGOT,EBX
{$ELSE !PIC}
        FIMUL   calDCon10
{$ENDIF !PIC}
        MOV     Temp,EAX
        FIADD   Temp
        INC     EDX
        JMP     @@31
@@32:   DEC     ESI
        RET

// Get exponent
// Out EDX = Exponent (-4999..4999)

@@GetExponent:

        XOR     EAX,EAX
        XOR     EDX,EDX
        MOV     CL, byte ptr [ESI]
        CMP     CL,'+'
        JE      @@41
        CMP     CL,'-'
        JNE     @@42
@@41:   INC     ESI
@@42:   MOV     AL, byte ptr [ESI]
        SUB     AL,'0'+10
        ADD     AL,10
        JNC     @@43
        INC     ESI
        IMUL    EDX,10
        ADD     EDX,EAX
        CMP     EDX,500
        JB      @@42
@@43:   CMP     CL,'-'
        JNE     @@44
        NEG     EDX
@@44:   RET

@@Exit:
        POP     EBX
        POP     ESI
        POP     EDI
end;
{$ENDIF X86ASM}
{$ENDIF !PUREPASCAL}
{$WARN SYMBOL_DEPRECATED ON}

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.SysUtils.InternalFloatToTextFmt is still the same and adjust the IFDEF'}
{$IFEND}
{$IFDEF PUREPASCAL}
function InternalFloatToTextFmt(Buf: PByte; const Value; ValueType: TFloatValue; Format: PByte;
  const AFormatSettings: TALFormatSettings; const Unicode: Boolean): Integer;
const
  CMinExtPrecision = 2;
{$IFDEF EXTENDEDHAS10BYTES}
  CMaxExtPrecision = 18;
{$ELSE !EXTENDEDHAS10BYTES}
  CMaxExtPrecision = 17;
{$ENDIF EXTENDEDHAS10BYTES}

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
  ThousandsSep: ansiChar;
  FormatLength: Integer;

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

  function GetCharIndex(const ABuf: PByte; const Index: Integer): ansiChar;
  begin
    //if Unicode then
    //  Result := PWideChar(ABuf)[Index]
    //else
      Result := ansiChar(PByte(ABuf)[Index]);
  end;

  procedure AppendString(const AStr: ansiString);
  var
    {I,} L: Integer;
  begin
    L := length(AStr);

    if L > 0 then
    begin
      //if Unicode then
      //begin
      //  { Unicode -- loop }
      //  for I := Low(AStr) to High(AStr) do
      //  begin
      //    PChar(Buf)^ := Char(AStr[I]);
      //    Inc(Buf, SizeOf(Char));
      //  end;
      //end else
      //begin
        { ANSI -- move directly }
        ALMove(pointer(AStr)^, Buf^, L);
        Inc(Buf, L * SizeOf(AnsiChar));
      //end;
      Inc(Result, L);
    end;
  end;

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
          AppendChar(ThousandsSep);
      end;
    end;

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

      Exp := alIntToStr(Abs(Exponent));
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
    //while C < length(Section) do
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
          //if C < length(Section) then
          if C <= High(Section) then
          begin
            Sign := Section[C];
            if (Sign <> '+') and (Sign <> '-') then
              AppendChar(OldC)
            else
            begin
              Zeros := 0;
              Inc(C);
              //while (C < length(Section)) and (Section[C] = '0') do
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
{$IFNDEF NEXTGEN}
    //if Unicode then
    //  Result := FloatToText(PWideChar(Buf), Value, ValueType, ffGeneral, 15, 0, AFormatSettings)
    //else
      Result := ALFloatToText(PAnsiChar(Buf), Value, ValueType, ffGeneral, 15, 0, AFormatSettings)
{$ELSE NEXTGEN}
    Result := FloatToText(PWideChar(Buf), Value, ValueType, ffGeneral, 15, 0, AFormatSettings)
{$ENDIF !NEXTGEN}
  else
    ApplyFormat;
end;
{$ENDIF PUREPASCAL}

{*************************************************************************************}
function ALAnsiFloatToTextEx(BufferArg: PAnsiChar; const Value; ValueType: TFloatValue;
  Format: TFloatFormat; Precision, Digits: Integer;
  const AFormatSettings: TALFormatSettings): Integer;
begin
  Result := ALFloatToText(BufferArg, Value, ValueType, Format, Precision, Digits,
    AFormatSettings);
end;

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if system.sysUtils.FloatToTextFmt is still the same and adjust the IFDEF'}
{$IFEND}
function ALFloatToTextFmt(Buf: PAnsiChar; const Value; ValueType: TFloatValue;
  Format: PAnsiChar; const AFormatSettings: TALFormatSettings): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := InternalFloatToTextFmt(PByte(Buf), Value, ValueType, PByte(Format), AFormatSettings, False);
end;
{$ELSE !PUREPASCAL}
{$IFDEF X86ASM}
var
  Buffer: Pointer;
  ThousandSep: Boolean;
  DecimalSep: AnsiChar;
  ThousandsSep: AnsiChar;
  Scientific: Boolean;
  Section: Integer;
  DigitCount: Integer;
  DecimalIndex: Integer;
  FirstDigit: Integer;
  LastDigit: Integer;
  DigitPlace: Integer;
  DigitDelta: Integer;
  FloatRec: TFloatRec;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     Buffer,EAX
        MOV     EDI,EDX
        MOV     EBX,ECX
        MOV     EAX,AFormatSettings
        MOV     AL,AnsiChar([EAX].TALFormatSettings.DecimalSeparator)
        MOV     DecimalSep,AL
        MOV     EAX,AFormatSettings
        MOV     AL,AnsiChar([EAX].TALFormatSettings.ThousandSeparator)
        MOV     ThousandsSep,AL
        MOV     ECX,2
        CMP     BL,fvExtended
        JE      @@1
        MOV     EAX,[EDI].Integer
        OR      EAX,[EDI].Integer[4]
        JE      @@2
        MOV     ECX,[EDI].Integer[4]
        SHR     ECX,31
        JMP     @@2
@@1:    MOVZX   EAX,[EDI].Word[8]
        OR      EAX,[EDI].Integer[0]
        OR      EAX,[EDI].Integer[4]
        JE      @@2
        MOVZX   ECX,[EDI].Word[8]
        SHR     ECX,15
@@2:    CALL    @@FindSection
        JE      @@5
        CALL    @@ScanSection
        MOV     EAX,DigitCount
        MOV     EDX,9999
        CMP     Scientific,0
        JNE     @@3
        SUB     EAX,DecimalIndex
        MOV     EDX,EAX
        MOV     EAX,18
@@3:    PUSH    EAX
        PUSH    EDX
        LEA     EAX,FloatRec
        MOV     EDX,EDI
        MOV     ECX,EBX
        CALL    FloatToDecimal                { Stack aligned - ESP(xxxxxxx0h) on call }
        MOV     AX,FloatRec.Exponent
        CMP     AX,8000H
        JE      @@5
        CMP     AX,7FFFH
        JE      @@5
        CMP     BL,fvExtended
        JNE     @@6
        CMP     AX,18
        JLE     @@6
        CMP     Scientific,0
        JNE     @@6
@@5:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    ffGeneral
        PUSH    15
        PUSH    0
        MOV     EAX,[AFormatSettings]
        PUSH    EAX
        MOV     EAX,Buffer
        MOV     EDX,EDI
        MOV     ECX,EBX
        CALL    ALAnsiFloatToTextEx
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        JMP     @@Exit
@@6:    CMP     FloatRec.Digits.Byte,0
        JNE     @@7
        MOV     ECX,2
        CALL    @@FindSection
        JE      @@5
        CMP     ESI,Section
        JE      @@7
        CALL    @@ScanSection
@@7:    CALL    @@ApplyFormat
        JMP     @@Exit

// Find format section
// In   ECX = Section index
// Out  ESI = Section offset
//      ZF  = 1 if section is empty

@@FindSection:
        MOV     ESI,Format
        JECXZ   @@fs2
@@fs1:  LODSB
        CMP     AL,"'"
        JE      @@fs4
        CMP     AL,'"'
        JE      @@fs4
        OR      AL,AL
        JE      @@fs2
        CMP     AL,';'
        JNE     @@fs1
        LOOP    @@fs1
        MOV     AL,byte ptr [ESI]
        OR      AL,AL
        JE      @@fs2
        CMP     AL,';'
        JNE     @@fs3
@@fs2:  MOV     ESI,Format
        MOV     AL,byte ptr [ESI]
        OR      AL,AL
        JE      @@fs3
        CMP     AL,';'
@@fs3:  RET
@@fs4:  MOV     AH,AL
@@fs5:  LODSB
        CMP     AL,AH
        JE      @@fs1
        OR      AL,AL
        JNE     @@fs5
        JMP     @@fs2

// Scan format section

@@ScanSection:
        PUSH    EBX
        MOV     Section,ESI
        MOV     EBX,32767
        XOR     ECX,ECX
        XOR     EDX,EDX
        MOV     DecimalIndex,-1
        MOV     ThousandSep,DL
        MOV     Scientific,DL
@@ss1:  LODSB
@@ss2:  CMP     AL,'#'
        JE      @@ss10
        CMP     AL,'0'
        JE      @@ss11
        CMP     AL,'.'
        JE      @@ss13
        CMP     AL,','
        JE      @@ss14
        CMP     AL,"'"
        JE      @@ss15
        CMP     AL,'"'
        JE      @@ss15
        CMP     AL,'E'
        JE      @@ss20
        CMP     AL,'e'
        JE      @@ss20
        CMP     AL,';'
        JE      @@ss30
        OR      AL,AL
        JNE     @@ss1
        JMP     @@ss30
@@ss10: INC     EDX
        JMP     @@ss1
@@ss11: CMP     EDX,EBX
        JGE     @@ss12
        MOV     EBX,EDX
@@ss12: INC     EDX
        MOV     ECX,EDX
        JMP     @@ss1
@@ss13: CMP     DecimalIndex,-1
        JNE     @@ss1
        MOV     DecimalIndex,EDX
        JMP     @@ss1
@@ss14: MOV     ThousandSep,1
        JMP     @@ss1
@@ss15: MOV     AH,AL
@@ss16: LODSB
        CMP     AL,AH
        JE      @@ss1
        OR      AL,AL
        JNE     @@ss16
        JMP     @@ss30
@@ss20: LODSB
        CMP     AL,'-'
        JE      @@ss21
        CMP     AL,'+'
        JNE     @@ss2
@@ss21: MOV     Scientific,1
@@ss22: LODSB
        CMP     AL,'0'
        JE      @@ss22
        JMP     @@ss2
@@ss30: MOV     DigitCount,EDX
        CMP     DecimalIndex,-1
        JNE     @@ss31
        MOV     DecimalIndex,EDX
@@ss31: MOV     EAX,DecimalIndex
        SUB     EAX,ECX
        JLE     @@ss32
        XOR     EAX,EAX
@@ss32: MOV     LastDigit,EAX
        MOV     EAX,DecimalIndex
        SUB     EAX,EBX
        JGE     @@ss33
        XOR     EAX,EAX
@@ss33: MOV     FirstDigit,EAX
        POP     EBX
        RET

// Apply format string

@@ApplyFormat:
        CMP     Scientific,0
        JE      @@af1
        MOV     EAX,DecimalIndex
        XOR     EDX,EDX
        JMP     @@af3
@@af1:  MOVSX   EAX,FloatRec.Exponent
        CMP     EAX,DecimalIndex
        JG      @@af2
        MOV     EAX,DecimalIndex
@@af2:  MOVSX   EDX,FloatRec.Exponent
        SUB     EDX,DecimalIndex
@@af3:  MOV     DigitPlace,EAX
        MOV     DigitDelta,EDX
        MOV     ESI,Section
        MOV     EDI,Buffer
        LEA     EBX,FloatRec.Digits
        CMP     FloatRec.Negative,0
        JE      @@af10
        CMP     ESI,Format
        JNE     @@af10
        MOV     AL,'-'
        STOSB
@@af10: LODSB
        CMP     AL,'#'
        JE      @@af20
        CMP     AL,'0'
        JE      @@af20
        CMP     AL,'.'
        JE      @@af10
        CMP     AL,','
        JE      @@af10
        CMP     AL,"'"
        JE      @@af25
        CMP     AL,'"'
        JE      @@af25
        CMP     AL,'E'
        JE      @@af30
        CMP     AL,'e'
        JE      @@af30
        CMP     AL,';'
        JE      @@af40
        OR      AL,AL
        JE      @@af40
@@af11: STOSB
        JMP     @@af10
@@af20: CALL    @@PutFmtDigit
        JMP     @@af10
@@af25: MOV     AH,AL
@@af26: LODSB
        CMP     AL,AH
        JE      @@af10
        OR      AL,AL
        JE      @@af40
        STOSB
        JMP     @@af26
@@af30: MOV     AH,[ESI]
        CMP     AH,'+'
        JE      @@af31
        CMP     AH,'-'
        JNE     @@af11
        XOR     AH,AH
@@af31: MOV     ECX,-1
@@af32: INC     ECX
        INC     ESI
        CMP     [ESI].Byte,'0'
        JE      @@af32
        CMP     ECX,4
        JB      @@af33
        MOV     ECX,4
@@af33: PUSH    EBX
        MOV     BL,FloatRec.Digits.Byte
        XOR     BH,BH
        MOVSX   EDX,FloatRec.Exponent
        SUB     EDX,DecimalIndex
        CALL    ALPutExponent   {Safe to call unaligned}
        POP     EBX
        JMP     @@af10
@@af40: MOV     EAX,EDI
        SUB     EAX,Buffer
        RET

// Store formatted digit

@@PutFmtDigit:
        CMP     DigitDelta,0
        JE      @@fd3
        JL      @@fd2
@@fd1:  CALL    @@fd3
        DEC     DigitDelta
        JNE     @@fd1
        JMP     @@fd3
@@fd2:  INC     DigitDelta
        MOV     EAX,DigitPlace
        CMP     EAX,FirstDigit
        JLE     @@fd4
        JMP     @@fd7
@@fd3:  MOV     AL,[EBX]
        INC     EBX
        OR      AL,AL
        JNE     @@fd5
        DEC     EBX
        MOV     EAX,DigitPlace
        CMP     EAX,LastDigit
        JLE     @@fd7
@@fd4:  MOV     AL,'0'
@@fd5:  CMP     DigitPlace,0
        JNE     @@fd6
        MOV     AH,AL
        MOV     AL,DecimalSep
        STOSW
        JMP     @@fd7
@@fd6:  STOSB
        CMP     ThousandSep,0
        JE      @@fd7
        MOV     EAX,DigitPlace
        CMP     EAX,1
        JLE     @@fd7
        MOV     DL,3
        DIV     DL
        CMP     AH,1
        JNE     @@fd7
        MOV     AL,ThousandsSep
        TEST    AL,AL
        JZ      @@fd7
        STOSB
@@fd7:  DEC     DigitPlace
        RET

@@exit:
        POP     EBX
        POP     ESI
        POP     EDI
end;
{$ENDIF X86ASM}
{$ENDIF !PUREPASCAL}

{***************************************************************}
function ALFormatFloat(const Format: AnsiString; Value: Extended;
  const AFormatSettings: TALFormatSettings): AnsiString;
var
  Buffer: array[0..255] of AnsiChar;
begin
  if Length(Format) > Length(Buffer) - 32 then ALConvertError(@SFormatTooLong);
  SetString(Result, Buffer, ALFloatToTextFmt(Buffer, Value, fvExtended,
    PAnsiChar(Format), AFormatSettings));
end;

{**************************************************************}
function ALFormatCurr(const Format: AnsiString; Value: Currency;
  const AFormatSettings: TALFormatSettings): AnsiString;
var
  Buffer: array[0..255] of AnsiChar;
begin
  if Length(Format) > Length(Buffer) - 32 then ALConvertError(@SFormatTooLong);
  SetString(Result, Buffer, ALFloatToTextFmt(Buffer, Value, fvCurrency,
    PAnsiChar(Format), AFormatSettings));
end;

{**********************************************************************************************}
function  ALStrToFloat(const S: AnsiString; const AFormatSettings: TALFormatSettings): Extended;
begin
  if not ALTextToFloat(PAnsiChar(S), Result, fvExtended, AFormatSettings) then
    ALConvertErrorFmt(@SInvalidFloat, [S]);
end;

{**************************************************************************************************************************}
function  ALStrToFloatDef(const S: AnsiString; const Default: Extended; const AFormatSettings: TALFormatSettings): Extended;
begin
  if not ALTextToFloat(PAnsiChar(S), Result, fvExtended, AFormatSettings) then
    Result := Default;
end;

{*********************************************************************************************************************}
function  ALTryStrToFloat(const S: AnsiString; out Value: Extended; const AFormatSettings: TALFormatSettings): Boolean;
begin
  Result := ALTextToFloat(PansiChar(S), Value, fvExtended, AFormatSettings);
end;

{*******************************************************************************************************************}
function  ALTryStrToFloat(const S: AnsiString; out Value: Double; const AFormatSettings: TALFormatSettings): Boolean;
var
  LValue: Extended;
begin
  Result := ALTextToFloat(PAnsiChar(S), LValue, fvExtended, AFormatSettings);
  if Result then
    if (LValue < -MaxDouble) or (LValue > MaxDouble) then
      Result := False;
  if Result then
    Value := LValue;
end;

{*******************************************************************************************************************}
function  ALTryStrToFloat(const S: AnsiString; out Value: Single; const AFormatSettings: TALFormatSettings): Boolean;
var
  LValue: Extended;
begin
  Result := ALTextToFloat(PAnsiChar(S), LValue, fvExtended, AFormatSettings);
  if Result then
    if (LValue < -MaxSingle) or (LValue > MaxSingle) then
      Result := False;
  if Result then
    Value := LValue;
end;

{$ENDIF !NEXTGEN}

{****************************************************************************************************************}
function  ALTryStrToFloatU(const S: String; out Value: Single; const AFormatSettings: TALFormatSettingsU): Boolean;
begin
  Result := TryStrToFloat(S, Value, AFormatSettings);
end;

{****************************************************************************************************************}
function  ALTryStrToFloatU(const S: String; out Value: Double; const AFormatSettings: TALFormatSettingsU): Boolean;
begin
  Result := TryStrToFloat(S, Value, AFormatSettings);
end;

{******************************************************************************************************************}
function  ALTryStrToFloatU(const S: String; out Value: Extended; const AFormatSettings: TALFormatSettingsU): Boolean;
begin
  Result := TryStrToFloat(S, Value, AFormatSettings);
end;

{$IFNDEF NEXTGEN}

{*********************************************************************************************}
function  ALStrToCurr(const S: AnsiString; const AFormatSettings: TALFormatSettings): Currency;
begin
  if not ALTextToFloat(PAnsiChar(S), Result, fvCurrency, AFormatSettings) then
    ALConvertErrorFmt(@SInvalidFloat, [S]);
end;

{*************************************************************************************************************************}
function  ALStrToCurrDef(const S: AnsiString; const Default: Currency; const AFormatSettings: TALFormatSettings): Currency;
begin
  if not ALTextToFloat(PAnsiChar(S), Result, fvCurrency, AFormatSettings) then
    Result := Default;
end;

{********************************************************************************************************************}
function  ALTryStrToCurr(const S: AnsiString; out Value: Currency; const AFormatSettings: TALFormatSettings): Boolean;
begin
  Result := ALTextToFloat(PAnsiChar(S), Value, fvCurrency, AFormatSettings);
end;

{***************************************************************************}
function  ALPos(const SubStr, Str: AnsiString; Offset: Integer = 1): Integer;
begin
  Result := System.Pos(SubStr, Str, Offset);
end;

var
  vALPosExIgnoreCaseLookupTable: packed array[AnsiChar] of AnsiChar; {Upcase Lookup Table}

{***********************************************}
procedure ALPosExIgnoreCaseInitialiseLookupTable;
var Ch: AnsiChar;
begin
  for Ch := #0 to #255 do
    vALPosExIgnoreCaseLookupTable[Ch] := AlUpCase(Ch);
end;

{*****************************************************************************************}
{from John O'Harrow (john@elmcrest.demon.co.uk) - original name: StringReplace_JOH_IA32_12}
function  ALPosExIgnoreCase(const SubStr, S: Ansistring; Offset: Integer = 1): Integer;
{$IFDEF PUREPASCAL}
var
  I, LIterCnt, L, J: Integer;
  PSubStr, PS: PAnsiChar;
  C1, C2: AnsiChar;
begin
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
           ((C1 in ['a' .. 'z']) and
            (C1 = AnsiChar(Byte(C2) + $20))) or
           ((C1 in ['A' .. 'Z']) and
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
{$ELSE !PUREPASCAL}
{$IFDEF X86ASM}
asm
  push    ebx
  push    esi
  push    edx              {@Str}
  test    eax, eax
  jz      @@NotFound       {Exit if SubStr = ''}
  test    edx, edx
  jz      @@NotFound       {Exit if Str = ''}
  mov     esi, ecx
  mov     ecx, [edx-4]     {Length(Str)}
  mov     ebx, [eax-4]     {Length(SubStr)}
  add     ecx, edx
  sub     ecx, ebx         {Max Start Pos for Full Match}
  lea     edx, [edx+esi-1] {Set Start Position}
  cmp     edx, ecx
  jg      @@NotFound       {StartPos > Max Start Pos}
  cmp     ebx, 1           {Length(SubStr)}
  jle     @@SingleChar     {Length(SubStr) <= 1}
  push    edi
  push    ebp
  lea     edi, [ebx-2]     {Length(SubStr) - 2}
  mov     esi, eax
  push    edi              {Save Remainder to Check = Length(SubStr) - 2}
  push    ecx              {Save Max Start Position}
  lea     edi, vALPosExIgnoreCaseLookupTable  {Uppercase Lookup Table}
  movzx   ebx, [eax]       {Search Character = 1st Char of SubStr}
  movzx   ebx, [edi+ebx]   {Convert to Uppercase}
@@Loop:                    {Loop Comparing 2 Characters per Loop}
  movzx   eax, [edx]       {Get Next Character}
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  jne     @@NotChar1
  mov     ebp, [esp+4]     {Remainder to Check}
@@Char1Loop:
  movzx   eax, [esi+ebp]
  movzx   ecx, [edx+ebp]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar1
  movzx   eax, [esi+ebp+1]
  movzx   ecx, [edx+ebp+1]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar1
  sub     ebp, 2
  jnc     @@Char1Loop
  pop     ecx
  pop     edi
  pop     ebp
  pop     edi
  jmp     @@SetResult
@@NotChar1:
  movzx   eax, [edx+1]     {Get Next Character}
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  cmp     bl, al
  jne     @@NotChar2
  mov     ebp, [esp+4]     {Remainder to Check}
@@Char2Loop:
  movzx   eax, [esi+ebp]
  movzx   ecx, [edx+ebp+1]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar2
  movzx   eax, [esi+ebp+1]
  movzx   ecx, [edx+ebp+2]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar2
  sub     ebp, 2
  jnc     @@Char2Loop
  pop     ecx
  pop     edi
  pop     ebp
  pop     edi
  jmp     @@CheckResult    {Check Match is within String Data}
@@NotChar2:
  add     edx, 2
  cmp     edx, [esp]       {Compate to Max Start Position}
  jle     @@Loop           {Loop until Start Position > Max Start Position}
  pop     ecx              {Dump Start Position}
  pop     edi              {Dump Remainder to Check}
  pop     ebp
  pop     edi
  jmp     @@NotFound
@@SingleChar:
  jl      @@NotFound       {Needed for Zero-Length Non-NIL Strings}
  lea     esi, vALPosExIgnoreCaseLookupTable
  movzx   ebx, [eax]       {Search Character = 1st Char of SubStr}
  movzx   ebx, [esi+ebx]   {Convert to Uppercase}
@@CharLoop:
  movzx   eax, [edx]
  movzx   eax, [esi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  je      @@SetResult
  movzx   eax, [edx+1]
  movzx   eax, [esi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  je      @@CheckResult
  add     edx, 2
  cmp     edx, ecx
  jle     @@CharLoop
@@NotFound:
  xor     eax, eax
  pop     edx
  pop     esi
  pop     ebx
  ret
@@CheckResult:             {Check Match is within String Data}
  cmp     edx, ecx
  jge     @@NotFound
  add     edx, 1           {OK - Adjust Result}
@@SetResult:               {Set Result Position}
  pop     ecx              {@Str}
  pop     esi
  pop     ebx
  neg     ecx
  lea     eax, [edx+ecx+1]
end; {AnsiPosExIC}
{$ENDIF X86ASM}
{$ENDIF !PUREPASCAL}

{$ENDIF !NEXTGEN}

{*****************************************************************************************}
{from John O'Harrow (john@elmcrest.demon.co.uk) - original name: StringReplace_JOH_IA32_12}
function  ALPosExIgnoreCaseU(const SubStr, S: String; Offset: Integer = 1): Integer;
var
  I, LIterCnt, L, J: Integer;
  PSubStr, PS: PChar;
  C1, C2: Char;
begin

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

{$IFNDEF NEXTGEN}

{***********************************************}
function  AlUpCase(const Ch: AnsiChar): AnsiChar;
begin
  Result := Ch;
  if Result in ['a'..'z'] then
    Dec(Result, Ord('a')-Ord('A'));
end;

{***********************************************}
function  AlLoCase(const Ch: AnsiChar): AnsiChar;
begin
  Result := Ch;
  if Result in ['A'..'Z'] then
    Inc(Result, Ord('a')-Ord('A'));
end;

{$ENDIF !NEXTGEN}

{******************************************}
function  AlLoCaseU(Ch: WideChar): WideChar;
begin
  Result := Ch;
  case Ch of
    'A'..'Z':
      Inc(Result, Ord(char('a'))-Ord(char('A')));
  end;
end;

{$IFNDEF NEXTGEN}

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

{*************************************************************************}
function  ALPadLeft(const S: AnsiString; Const Width: Integer): AnsiString;
begin
  result := ALFormat('%'+ALIntToStr(Width)+'s', [S]);
end;

{**************************************************************************}
function  ALPadRight(const S: AnsiString; Const Width: Integer): AnsiString;
begin
  result := ALFormat('%-'+ALIntToStr(Width)+'s', [S]);
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

{$ENDIF !NEXTGEN}

{************************************************************************}
function  ALQuotedStrU(const S: String; const Quote: Char = ''''): String;
var
  I: Integer;
begin
  Result := S;
  for I := Result.Length - 1 downto 0 do
    if Result.Chars[I] = Quote then Result := Result.Insert(I, Quote);
  Result := Quote + Result + Quote;
end;

{$IFNDEF NEXTGEN}

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

{$ENDIF !NEXTGEN}

{****************************************************************}
function ALExtractQuotedStrU(var Src: PChar; Quote: Char): String;
begin
  result := AnsiExtractQuotedStr(PWideChar(Src), Quote);
end;

{$IFNDEF NEXTGEN}

{******************************************************************}
function  ALExtractFilePath(const FileName: AnsiString): AnsiString;
begin
  Result := System.AnsiStrings.ExtractFilePath(FileName);
end;

{*****************************************************************}
function  ALExtractFileDir(const FileName: AnsiString): AnsiString;
begin
  Result := ExtractFileDir(FileName);
end;

{*******************************************************************}
function  ALExtractFileDrive(const FileName: AnsiString): AnsiString;
begin
  Result := ExtractFileDrive(FileName);
end;

{******************************************************************}
function  ALExtractFileName(const FileName: AnsiString): AnsiString;
begin
  Result := ExtractFileName(FileName);
end;

{*****************************************************************}
function  ALExtractFileExt(const FileName: AnsiString): AnsiString;
begin
  Result := System.AnsiStrings.ExtractFileExt(FileName);
end;

{******************************************************************}
function  ALLastDelimiter(const Delimiters, S: AnsiString): Integer;
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

{******************************************************************************************************************************************************}
function ALIsPathDelimiter(const S: AnsiString; Index: Integer; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): Boolean;
begin
  Result := (Index >= low(s)) and (Index <= high(S)) and (S[Index] = PathDelimiter);
end;

{******************************************************************************************************************************************************}
function ALIncludeTrailingPathDelimiter(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString;
begin
  Result := S;
  if not ALIsPathDelimiter(Result, Length(Result), PathDelimiter) then
    Result := Result + PathDelimiter;
end;

{******************************************************************************************************************************************************}
function ALExcludeTrailingPathDelimiter(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString;
begin
  Result := S;
  if ALIsPathDelimiter(Result, Length(Result), PathDelimiter) then
    SetLength(Result, Length(Result)-1);
end;

{*****************************************************************************************************************************************************}
function ALIncludeLeadingPathDelimiter(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString;
begin
  if not ALIsPathDelimiter(s, 1, PathDelimiter) then Result := PathDelimiter + s
  else Result := S;
end;

{*****************************************************************************************************************************************************}
function ALExcludeLeadingPathDelimiter(const S: AnsiString; const PathDelimiter: ansiString = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): AnsiString;
begin
  if ALIsPathDelimiter(S, 1, PathDelimiter) then Result := ALcopyStr(S,2,maxint)
  else result := S;
end;

{$ENDIF !NEXTGEN}

{***********************************************************************************************************************************************}
function ALIsPathDelimiterU(const S: String; Index: Integer; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): Boolean;
begin
  Result := (Index >= low(s)) and (Index <= high(S)) and (S[low(s) + Index - 1] = PathDelimiter);
end;

{*******************************************************************************************************************************************}
function ALIncludeTrailingPathDelimiterU(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String;
begin
  Result := S;
  if not ALIsPathDelimiterU(Result, Length(Result), PathDelimiter) then
    Result := Result + PathDelimiter;
end;

{*******************************************************************************************************************************************}
function ALExcludeTrailingPathDelimiterU(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String;
begin
  Result := S;
  if ALIsPathDelimiterU(Result, Length(Result), PathDelimiter) then
    SetLength(Result, Length(Result)-1);
end;

{******************************************************************************************************************************************}
function ALIncludeLeadingPathDelimiterU(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String;
begin
  if not ALIsPathDelimiterU(s, 1, PathDelimiter) then Result := PathDelimiter + s
  else Result := S;
end;

{******************************************************************************************************************************************}
function ALExcludeLeadingPathDelimiterU(const S: String; const PathDelimiter: String = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF}): String;
begin
  if ALIsPathDelimiterU(S, 1, PathDelimiter) then Result := ALcopyStrU(S,2,maxint)
  else result := S;
end;

{$IFNDEF NEXTGEN}

{*****************************************************************************************}
{from John O'Harrow (john@elmcrest.demon.co.uk) - original name: StringReplace_JOH_IA32_12}
function ALStringReplace(const S, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;
type TPosEx = function(const SubStr, S: AnsiString; Offset: Integer): Integer;
const StaticBufferSize = 16;
var
  SrcLen, OldLen, NewLen, Found, Count, Start, Match, Matches, BufSize,
  Remainder    : Integer;
  PosExFunction: TPosEx;
  StaticBuffer : array[0..StaticBufferSize-1] of Integer;
  Buffer       : PIntegerArray;
  P, PSrc, PRes: PAnsiChar;
  Ch           : AnsiChar;
begin
  SrcLen := Length(S);
  OldLen := Length(OldPattern);
  NewLen := Length(NewPattern);
  if (OldLen = 0) or (SrcLen < OldLen) then
    begin
      if SrcLen = 0 then
        Result := '' {Needed for Non-Nil Zero Length Strings}
      else
        Result := S
    end
  else
    begin
      if rfIgnoreCase in Flags then
        begin
          PosExFunction := ALPosExIgnoreCase;
        end
      else
        PosExFunction := ALPosEx;
      if rfReplaceAll in Flags then
        begin
          if (OldLen = 1) and (NewLen = 1) then
            begin {Single Character Replacement}
              Remainder := SrcLen;
              SetLength(Result, Remainder);
              P := Pointer(Result);
              ALMove(Pointer(S)^, P^, Remainder);
              if rfIgnoreCase in Flags then
                begin
                  Ch := vALPosExIgnoreCaseLookupTable[OldPattern[1]];
                  repeat
                    Dec(Remainder);
                    if vALPosExIgnoreCaseLookupTable[P[Remainder]] = Ch then
                      P[Remainder] := NewPattern[1];
                  until Remainder = 0;
                end
              else
                begin
                  repeat
                    Dec(Remainder);
                    if P[Remainder] = OldPattern[1] then
                      P[Remainder] := NewPattern[1];
                  until Remainder = 0;
                end;
              Exit;
            end;
          Found := PosExFunction(OldPattern, S, 1);
          if Found <> 0 then
            begin
              Buffer    := @StaticBuffer;
              BufSize   := StaticBufferSize;
              Matches   := 1;
              Buffer[0] := Found;
              repeat
                Inc(Found, OldLen);
                Found := PosExFunction(OldPattern, S, Found);
                if Found > 0 then
                  begin
                    if Matches = BufSize then
                      begin {Create or Expand Dynamic Buffer}
                        BufSize := BufSize + (BufSize shr 1); {Grow by 50%}
                        if Buffer = @StaticBuffer then
                          begin {Create Dynamic Buffer}
                            GetMem(Buffer, BufSize * SizeOf(Integer));
                            ALMove(StaticBuffer, Buffer^, SizeOf(StaticBuffer));
                          end
                        else {Expand Dynamic Buffer}
                          ReallocMem(Buffer, BufSize * SizeOf(Integer));
                      end;
                    Buffer[Matches] := Found;
                    Inc(Matches);
                  end
              until Found = 0;
              SetLength(Result, SrcLen + (Matches * (NewLen - OldLen)));
              PSrc := Pointer(S);
              PRes := Pointer(Result);
              Start := 1;
              Match := 0;
              repeat
                Found := Buffer[Match];
                Count := Found - Start;
                Start := Found + OldLen;
                if Count > 0 then
                  begin
                    ALMove(PSrc^, PRes^, Count);
                    Inc(PRes, Count);
                  end;
                Inc(PSrc, Count + OldLen);
                ALMove(Pointer(NewPattern)^, PRes^, NewLen);
                Inc(PRes, NewLen);
                Inc(Match);
              until Match = Matches;
              Remainder := SrcLen - Start;
              if Remainder >= 0 then
                ALMove(PSrc^, PRes^, Remainder + 1);
              if BufSize <> StaticBufferSize then
                FreeMem(Buffer); {Free Dynamic Buffer if Created}
            end
          else {No Matches Found}
            Result := S
        end {ReplaceAll}
      else
        begin {Replace First Occurance Only}
          Found := PosExFunction(OldPattern, S, 1);
          if Found <> 0 then
            begin {Match Found}
              SetLength(Result, SrcLen - OldLen + NewLen);
              Dec(Found);
              PSrc := Pointer(S);
              PRes := Pointer(Result);
              if NewLen = OldLen then
                begin
                  ALMove(PSrc^, PRes^, SrcLen);
                  Inc(PRes, Found);
                  ALMove(Pointer(NewPattern)^, PRes^, NewLen);
                end
              else
                begin
                  ALMove(PSrc^, PRes^, Found);
                  Inc(PRes, Found);
                  Inc(PSrc, Found + OldLen);
                  ALMove(Pointer(NewPattern)^, PRes^, NewLen);
                  Inc(PRes, NewLen);
                  ALMove(PSrc^, PRes^, SrcLen - Found - OldLen);
                end;
            end
          else {No Matches Found}
            Result := S
        end;
    end;
end;

{***********************************************************************************************}
// warning ALStrMove inverse the order of the original StrMove (to keep the same order as ALMove)
procedure ALStrMove(const Source: PAnsiChar; var Dest: PAnsiChar; Count: NativeInt);
begin
  ALMove(Source^, Dest^, Count);
end;

{$ENDIF !NEXTGEN}

{***********************************************************************************************}
// warning ALStrMove inverse the order of the original StrMove (to keep the same order as ALMove)
procedure ALStrMoveU(const Source: PChar; var Dest: PChar; Count: NativeInt);
begin
  ALMove(Source^, Dest^, Count * sizeOf(Char));
end;

{$IFNDEF NEXTGEN}

{****************************************************************************************}
function ALCopyStr(const aSourceString: AnsiString; aStart, aLength: Integer): AnsiString;
var aSourceStringLength: Integer;
begin
  aSourceStringLength := Length(aSourceString);
  If (aStart < 1) then aStart := 1;

  if (aSourceStringLength=0) or
     (aLength < 1) or
     (aStart > aSourceStringLength) then Begin
    Result := '';
    Exit;
  end;

  if aLength > aSourceStringLength - (aStart - 1) then aLength := aSourceStringLength - (aStart-1);

  SetLength(Result,aLength); //  To guarantee that the string is unique, call the SetLength, SetString, or UniqueString procedures
  ALMove(Pbyte(aSourceString)[aStart-1], pointer(Result)^, aLength); // pointer(Result)^ to not jump inside uniqueString (aDestString is already unique thanks to previous SetLength))
end;

{**********************************************************************************************************}
procedure ALCopyStr(const aSourceString: AnsiString; var aDestString: ansiString; aStart, aLength: Integer);
var aSourceStringLength: Integer;
begin
  aSourceStringLength := Length(aSourceString);
  If (aStart < 1) then aStart := 1;

  if (aSourceStringLength=0) or
     (aLength < 1) or
     (aStart > aSourceStringLength) then Begin
    aDestString := '';
    Exit;
  end;

  if aLength > aSourceStringLength - (aStart - 1) then aLength := aSourceStringLength - (aStart-1);

  SetLength(aDestString,aLength); //  To guarantee that the string is unique, call the SetLength, SetString, or UniqueString procedures
  ALMove(Pbyte(aSourceString)[aStart-1], pointer(aDestString)^, aLength);  // pointer(aDestString)^ to not jump inside uniqueString (aDestString is already unique thanks to previous SetLength))
end;

{**************************************************}
function  ALCopyStr(const aSourceString: AnsiString;
                    const aStartStr: AnsiString;
                    const aEndStr: AnsiString;
                    const aOffset: integer = 1;
                    const aRaiseExceptionIfNotFound: Boolean = True): AnsiString;
var P1, P2: integer;
begin
  P1 := AlPosExIgnoreCase(aStartStr, aSourceString, aOffset);
  if P1 <= 0 then begin
    if aRaiseExceptionIfNotFound then Raise EALException.Createfmt('Can not find%s the text %s in %s', [ALIfThen(aOffset > 1, AlFormat(' after offset %d', [aOffset])), aStartStr, aSourceString])
    else begin
      result := '';
      exit;
    end;
  end;
  Inc(P1, Length(aStartStr));
  P2 := AlPosExIgnoreCase(aEndStr, aSourceString, P1);
  if P2 <= 0 then begin
    if aRaiseExceptionIfNotFound then Raise EALException.Createfmt('Can not find%s the text %s in %s', [ALIfThen(aOffset > 1, AlFormat(' after offset %d', [aOffset])), aEndStr, aSourceString])
    else begin
      result := '';
      exit;
    end;
  end;
  result := ALCopyStr(aSourceString, P1, P2 - P1);
end;

{$ENDIF !NEXTGEN}

{*********************************************************************************}
function ALCopyStrU(const aSourceString: String; aStart, aLength: Integer): String;
var aSourceStringLength: Integer;
begin
  aSourceStringLength := Length(aSourceString);
  If (aStart < 1) then aStart := 1;

  if (aSourceStringLength=0) or
     (aLength < 1) or
     (aStart > aSourceStringLength) then Begin
    Result := '';
    Exit;
  end;

  if aLength > aSourceStringLength - (aStart - 1) then aLength := aSourceStringLength - (aStart-1);

  SetLength(Result,aLength); //  To guarantee that the string is unique, call the SetLength, SetString, or UniqueString procedures
  ALMove(PChar(aSourceString)[aStart-1], pointer(Result)^, aLength*SizeOf(Char)); // pointer(Result)^ to not jump inside uniqueString (aDestString is already unique thanks to previous SetLength))
end;

{***************************************************************************************************}
procedure ALCopyStrU(const aSourceString: String; var aDestString: String; aStart, aLength: Integer);
var aSourceStringLength: Integer;
begin
  aSourceStringLength := Length(aSourceString);
  If (aStart < 1) then aStart := 1;

  if (aSourceStringLength=0) or
     (aLength < 1) or
     (aStart > aSourceStringLength) then Begin
    aDestString := '';
    Exit;
  end;

  if aLength > aSourceStringLength - (aStart - 1) then aLength := aSourceStringLength - (aStart-1);

  SetLength(aDestString,aLength); //  To guarantee that the string is unique, call the SetLength, SetString, or UniqueString procedures
  ALMove(PChar(aSourceString)[aStart-1], pointer(aDestString)^, aLength*SizeOf(Char));  // pointer(aDestString)^ to not jump inside uniqueString (aDestString is already unique thanks to previous SetLength))
end;

{***********************************************}
function  ALCopyStrU(const aSourceString: String;
                     const aStartStr: String;
                     const aEndStr: String;
                     const aOffset: integer = 1;
                     const aRaiseExceptionIfNotFound: Boolean = True): String;
var P1, P2: integer;
begin
  P1 := AlPosExIgnoreCaseU(aStartStr, aSourceString, aOffset);
  if P1 <= 0 then begin
    if aRaiseExceptionIfNotFound then Raise EALExceptionU.Createfmt('Can not find%s the text %s in %s', [ALIfThenU(aOffset > 1, AlFormatU(' after offset %d', [aOffset])), aStartStr, aSourceString])
    else begin
      result := '';
      exit;
    end;
  end;
  Inc(P1, Length(aStartStr));
  P2 := AlPosExIgnoreCaseU(aEndStr, aSourceString, P1);
  if P2 <= 0 then begin
    if aRaiseExceptionIfNotFound then Raise EALExceptionU.Createfmt('Can not find%s the text %s in %s', [ALIfThenU(aOffset > 1, AlFormatU(' after offset %d', [aOffset])), aEndStr, aSourceString])
    else begin
      result := '';
      exit;
    end;
  end;
  result := ALCopyStrU(aSourceString, P1, P2 - P1);
end;

{$IFNDEF NEXTGEN}

{*******************************************************************************************}
function  ALRandomStr(const aLength: Longint; const aCharset: Array of ansiChar): AnsiString;
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

{*******************************************************}
function ALRandomStr(const aLength: Longint): AnsiString;
begin
  Result := ALRandomStr(aLength,['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']);
end;

{$ENDIF !NEXTGEN}

{************************************************************************************}
function  ALRandomStrU(const aLength: Longint; const aCharset: Array of Char): String;
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
function ALRandomStrU(const aLength: Longint): String;
begin
  Result := ALRandomStrU(aLength,['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']);
end;

{$IFNDEF NEXTGEN}

{*********************************************************}
function ALNEVExtractName(const S: AnsiString): AnsiString;
var P: Integer;
begin
  Result := S;
  P := alPos('=', Result);
  if P <> 0 then SetLength(Result, P-1)
  else SetLength(Result, 0);
end;

{**********************************************************}
function ALNEVExtractValue(const s: AnsiString): AnsiString;
begin
  Result := AlCopyStr(s, Length(ALNEVExtractName(s)) + 2, MaxInt)
end;

{***********************************}
constructor TALPrecompiledTag.Create;
begin
  fTagString := '';
  fTagParams := TALStringList.Create;
  TALStringList(fTagParams).Duplicates := dupIgnore;
end;

{***********************************}
destructor TALPrecompiledTag.Destroy;
begin
  fTagParams.Free;
  inherited;
end;

{**************************************************}
function TALPrecompiledTag.GetTagParams: TALStrings;
begin
  result := fTagParams;
end;

{***********************************************************************************}
function ALFastTagReplacePrecompile(Const SourceString, TagStart, TagEnd: AnsiString;
                                    PrecompileProc: TALHandleTagPrecompileFunct;
                                    StripParamQuotes: Boolean; // useless if PrecompileProc is provided
                                    ExtData: Pointer;
                                    TagsContainer: TObjectList; // just a container where all the PrecompiledTag will be store. must free all the PrecompiledTag at the end of the application
                                    Const flags: TReplaceFlags=[]): AnsiString; // rfreplaceall is ignored here, only rfIgnoreCase is matter

var ReplaceString: AnsiString;
    TagEndFirstChar, TagEndFirstCharLower, TagEndFirstCharUpper: AnsiChar;
    TokenStr, ParamStr: AnsiString;
    ParamList: TALStringList;
    TagStartLength: integer;
    TagEndLength: integer;
    SourceStringLength: Integer;
    InDoubleQuote: Boolean;
    InsingleQuote: Boolean;
    SourceCurrentPos: integer;
    ResultCurrentPos: integer;
    ResultCurrentLength: integer;
    PrecompiledTag: TALBasePrecompiledTag;
    IgnoreCase: Boolean;
    PosExFunct: Function(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;
    T1,T2: Integer;
    i: integer;

Const ResultBuffSize: integer = 16384;

    {------------------------------------}
    Function _ExtractTokenStr: AnsiString;
    var x: Integer;
    Begin
      X := 1;
      while (x <= length(ReplaceString)) and
            (not (ReplaceString[x] in [' ', #9, #13, #10])) do inc(x);
      if x > length(ReplaceString) then Result := ReplaceString
      else Result := AlcopyStr(ReplaceString,1,x-1);
    end;

    {-----------------------------------------------------------------}
    Function _ExtractParamsStr(const TokenStr: ansiString): AnsiString;
    Begin
      Result := ALTrim(AlcopyStr(ReplaceString,length(TokenStr) + 1, MaxInt));
    end;

    {-----------------------------------------------------------------------------------}
    procedure _MoveStr2Result(const aSourceString: AnsiString; aStart, aLength: Integer);
    var aSourceStringLength: Integer;
    begin
      aSourceStringLength := Length(aSourceString);
      If (aStart < 1) then aStart := 1;

      if (aSourceStringLength=0) or
         (aLength < 1) or
         (aStart > aSourceStringLength) then Exit;

      if aLength > aSourceStringLength - (aStart - 1) then aLength := aSourceStringLength - (aStart-1);

      If aLength + ResultCurrentPos - 1 > ResultCurrentLength then begin
        ResultCurrentLength := ResultCurrentLength + aLength + ResultBuffSize;
        SetLength(Result, ResultCurrentLength);
      end;
      AlMove(pbyte(aSourceString)[aStart-1], pbyte(Result)[ResultCurrentPos-1], aLength);
      ResultCurrentPos := ResultCurrentPos + aLength;
    end;

    {---------------------------------------------------------}
    function  _ObjAddressToStr(Const Obj: Tobject): AnsiString;
    begin
      result := ALIntToHex(NativeInt(Obj), sizeof(pointer) * 2);
    end;

begin
  if (SourceString = '') or (TagStart = '') or (TagEnd = '') then begin
    Result := SourceString;
    Exit;
  end;

  IgnoreCase := rfIgnoreCase in flags;
  If IgnoreCase then PosExFunct := ALPosExIgnoreCase
  Else PosExFunct := ALPosEx;

  SourceCurrentPos := 1;
  T1 := PosExFunct(TagStart,SourceString,SourceCurrentPos);
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
           ((TagEndLength > 1) and (PosExFunct(TagEnd,AlCopyStr(SourceString,T2,TagEndLength),1) <> 1))) do begin
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
      ParamList := TALStringList.Create;
      try
        ParamList.Duplicates := dupIgnore;
        ALExtractHeaderFieldsWithQuoteEscaped([' ', #9, #13, #10],
                                              [' ', #9, #13, #10],
                                              ['"', ''''],
                                              PAnsiChar(ParamStr),
                                              ParamList,
                                              False,
                                              StripParamQuotes);

        T2 := T2 - T1;
        PrecompiledTag := PrecompileProc(TokenStr, ParamList, ExtData, SourceString, T1, T2);
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
      PrecompiledTag := TALPrecompiledTag.Create;
      try
        PrecompiledTag.TagString := _ExtractTokenStr;
        ParamStr := _ExtractParamsStr(PrecompiledTag.TagString);
        ALExtractHeaderFieldsWithQuoteEscaped([' ', #9, #13, #10],
                                              [' ', #9, #13, #10],
                                              ['"', ''''],
                                              PAnsiChar(ParamStr),
                                              PrecompiledTag.TagParams,
                                              False,
                                              StripParamQuotes);
        TagsContainer.Add(PrecompiledTag);
        ReplaceString := TagStart + #2{start of text} + _ObjAddressToStr(PrecompiledTag) + #3{end of text} + TagEnd;
      except
        PrecompiledTag.Free;
        raise;
      end;
      for I := 0 to PrecompiledTag.TagParams.Count - 1 do
        PrecompiledTag.TagParams[i] := ALFastTagReplacePrecompile(PrecompiledTag.TagParams[i], //Const SourceString,
                                                                  TagStart,
                                                                  TagEnd,
                                                                  PrecompileProc,
                                                                  StripParamQuotes,
                                                                  ExtData,
                                                                  TagsContainer,
                                                                  flags);
      PrecompiledTag.TagString := ALFastTagReplacePrecompile(PrecompiledTag.TagString, //Const SourceString,
                                                             TagStart,
                                                             TagEnd,
                                                             PrecompileProc,
                                                             StripParamQuotes,
                                                             ExtData,
                                                             TagsContainer,
                                                             flags);
    end;

    _MoveStr2Result(SourceString,SourceCurrentPos,T1 - SourceCurrentPos);
    _MoveStr2Result(ReplaceString,1,length(ReplaceString));
    SourceCurrentPos := T2;

    T1 := PosExFunct(TagStart,SourceString,SourceCurrentPos);
    T2 := T1 + TagStartLength;
    If (T1 > 0) and (T2 <= SourceStringLength) then begin
      InDoubleQuote := False;
      InsingleQuote := False;
      While (T2 <= SourceStringLength) and
            (InDoubleQuote or
             InSingleQuote or
             (IgnoreCase and (SourceString[T2] <> TagEndFirstCharLower) and (SourceString[T2] <> TagEndFirstCharUpper)) or
             ((not IgnoreCase) and (SourceString[T2] <> TagEndFirstChar)) or
             ((TagEndLength > 1) and (PosExFunct(TagEnd,AlCopyStr(SourceString,T2,TagEndLength),1) <> 1))) do begin
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

{*************************************************************************}
function ALFastTagReplace(Const SourceString, TagStart, TagEnd: AnsiString;
                          ReplaceProc: TALHandleTagFunct;
                          ReplaceExtendedProc: TALHandleTagExtendedfunct;
                          StripParamQuotes: Boolean;
                          Flags: TReplaceFlags;
                          ExtData: Pointer;
                          TagParamsClass: TALTagParamsClass;
                          const TagReplaceProcResult: Boolean = False): AnsiString; overload;

var ReplaceString: AnsiString;
    TagEndFirstChar, TagEndFirstCharLower, TagEndFirstCharUpper: AnsiChar;
    TokenStr, ParamStr: AnsiString;
    ParamList: TAlStrings;
    TagStartLength: integer;
    TagEndLength: integer;
    SourceStringLength: Integer;
    InDoubleQuote: Boolean;
    InsingleQuote: Boolean;
    TagHandled: Boolean;
    SourceCurrentPos: integer;
    ResultCurrentPos: integer;
    ResultCurrentLength: integer;
    PrecompiledTag: TALBasePrecompiledTag;
    InPrecompiledTag: Boolean;
    IgnoreCase: Boolean;
    pSize: integer;
    PosExFunct: Function(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;
    T1,T2: Integer;

Const ResultBuffSize: integer = 16384;

    {------------------------------------}
    Function _ExtractTokenStr: AnsiString;
    var x: Integer;
    Begin
      X := 1;
      while (x <= length(ReplaceString)) and
            (not (ReplaceString[x] in [' ', #9, #13, #10])) do inc(x);
      if x > length(ReplaceString) then Result := ReplaceString
      else Result := AlcopyStr(ReplaceString,1,x-1);
    end;

    {-------------------------------------}
    Function _ExtractParamsStr: AnsiString;
    Begin
      Result := ALTrim(AlcopyStr(ReplaceString,length(TokenStr) + 1, MaxInt));
    end;

    {-----------------------------------------------------------------------------------}
    procedure _MoveStr2Result(const aSourceString: AnsiString; aStart, aLength: Integer);
    var aSourceStringLength: Integer;
    begin
      aSourceStringLength := Length(aSourceString);
      If (aStart < 1) then aStart := 1;

      if (aSourceStringLength=0) or
         (aLength < 1) or
         (aStart > aSourceStringLength) then Exit;

      if aLength > aSourceStringLength - (aStart - 1) then aLength := aSourceStringLength - (aStart-1);

      If aLength + ResultCurrentPos - 1 > ResultCurrentLength then begin
        ResultCurrentLength := ResultCurrentLength + aLength + ResultBuffSize;
        SetLength(Result, ResultCurrentLength);
      end;
      AlMove(pbyte(aSourceString)[aStart-1], pbyte(Result)[ResultCurrentPos-1], aLength);
      ResultCurrentPos := ResultCurrentPos + aLength;
    end;

    {-----------------------------------------------------------------------------------}
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
  If IgnoreCase then PosExFunct := ALPosExIgnoreCase
  Else PosExFunct := ALPosEx;

  SourceCurrentPos := 1;
  T1 := PosExFunct(TagStart,SourceString,SourceCurrentPos);
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
       ((TagEndLength <= 1) or (PosExFunct(TagEnd,AlCopyStr(SourceString,T2 + pSize + 2,TagEndLength),1) = 1)) then begin
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
             ((TagEndLength > 1) and (PosExFunct(TagEnd,AlCopyStr(SourceString,T2,TagEndLength),1) <> 1))) do begin
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
        ReplaceString := ReplaceExtendedProc(PrecompiledTag.TagString, PrecompiledTag.TagParams, ExtData, TagHandled, SourceString, T1, T2);
        T2 := T2 + T1;
      end
      else ReplaceString := ReplaceProc(PrecompiledTag.TagString, PrecompiledTag.TagParams, ExtData, TagHandled);
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
        ALExtractHeaderFieldsWithQuoteEscaped([' ', #9, #13, #10],
                                              [' ', #9, #13, #10],
                                              ['"', ''''],
                                              PAnsiChar(ParamStr),
                                              ParamList,
                                              False,
                                              StripParamQuotes);
        if assigned(ReplaceExtendedProc) then begin
          T2 := T2 - T1;
          ReplaceString := ReplaceExtendedProc(TokenStr, ParamList, ExtData, TagHandled, SourceString, T1, T2);
          T2 := T2 + T1;
        end
        else ReplaceString := ReplaceProc(TokenStr, ParamList, ExtData, TagHandled);
      finally
        ParamList.Free;
      end;
    end;

    if (TagHandled) and
       (TagReplaceProcResult) and
       (rfreplaceAll in flags) then ReplaceString := ALFastTagReplace(ReplaceString,
                                                                      TagStart,
                                                                      TagEnd,
                                                                      ReplaceProc,
                                                                      ReplaceExtendedProc,
                                                                      StripParamQuotes,
                                                                      Flags,
                                                                      ExtData,
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
    T1 := PosExFunct(TagStart,SourceString,SourceCurrentPos);
    T2 := T1 + TagStartLength;
    If (T1 > 0) and (T2 <= SourceStringLength) then begin

      //we are in precompiled tag
      if (SourceString[T2] = #2) and
         ((T2 + pSize + 1 + TagEndLength) <= SourceStringLength) and
         (SourceString[T2 + pSize + 1] = #3) and
         ((IgnoreCase and ((SourceString[T2 + pSize + 2] = TagEndFirstCharLower) or (SourceString[T2 + pSize + 2] = TagEndFirstCharUpper))) or
          ((not IgnoreCase) and (SourceString[T2 + pSize + 2] = TagEndFirstChar))) and
         ((TagEndLength <= 1) or (PosExFunct(TagEnd,AlCopyStr(SourceString,T2 + pSize + 2,TagEndLength),1) = 1)) then begin
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
               ((TagEndLength > 1) and (PosExFunct(TagEnd,AlCopyStr(SourceString,T2,TagEndLength),1) <> 1))) do begin
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

{*************************************************************************}
function ALFastTagReplace(const SourceString, TagStart, TagEnd: AnsiString;
                          ReplaceProc: TALHandleTagFunct;
                          StripParamQuotes: Boolean;
                          ExtData: Pointer;
                          Const flags: TReplaceFlags=[rfreplaceall];
                          const TagReplaceProcResult: Boolean = False): AnsiString;
Begin
  result := ALFastTagReplace(SourceString,
                             TagStart,
                             TagEnd,
                             ReplaceProc,
                             nil,
                             StripParamQuotes,
                             flags,
                             extdata,
                             TALStringList,
                             TagReplaceProcResult);
end;

{*************************************************************************}
function ALFastTagReplace(const SourceString, TagStart, TagEnd: AnsiString;
                          ReplaceExtendedProc: TALHandleTagExtendedfunct;
                          StripParamQuotes: Boolean;
                          ExtData: Pointer;
                          Const flags: TReplaceFlags=[rfreplaceall];
                          const TagReplaceProcResult: Boolean = False): AnsiString;
Begin
  result := ALFastTagReplace(SourceString,
                             TagStart,
                             TagEnd,
                             nil,
                             ReplaceExtendedProc,
                             StripParamQuotes,
                             flags,
                             extdata,
                             TALStringList,
                             TagReplaceProcResult);
end;

{************************************************************}
function ALFastTagReplaceWithFunc(const TagString: AnsiString;
                                  TagParams: TALStrings;
                                  ExtData: pointer;
                                  Var Handled: Boolean): AnsiString;
begin
  Handled := true;
  result := AnsiString(ExtData);
end;

{*************************************************************************}
function ALFastTagReplace(const SourceString, TagStart, TagEnd: AnsiString;
                          const ReplaceWith: AnsiString;
                          const Flags: TReplaceFlags=[rfreplaceall]): AnsiString;
Begin
  Result := ALFastTagReplace(SourceString,
                             TagStart,
                             TagEnd,
                             ALFastTagReplaceWithFunc,
                             nil,
                             True,
                             flags,
                             PAnsiChar(ReplaceWith),
                             TalStringList,
                             false);
end;

{**************************************************}
//the problem with this function is that if you have
//<#mytagwww params="xxx"> and
//<#mytag params="xxx">
//then the ALExtractTagParams(str, '<#mytag', '>' ... ) will not work like we expect
//because it's will extract the params of the <#mytagwww
function ALExtractTagParams(Const SourceString, TagStart, TagEnd: AnsiString;
                            StripParamQuotes: Boolean;
                            TagParams: TALStrings;
                            IgnoreCase: Boolean): Boolean;

var ReplaceString: AnsiString;
    TagEndFirstChar, TagEndFirstCharLower, TagEndFirstCharUpper: AnsiChar;
    TokenStr, ParamStr: AnsiString;
    TagStartLength: integer;
    TagEndLength: integer;
    SourceStringLength: Integer;
    InDoubleQuote: Boolean;
    InsingleQuote: Boolean;
    PosExFunct: Function(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;
    T1,T2: Integer;

    {------------------------------------}
    Function _ExtractTokenStr: AnsiString;
    var x: Integer;
    Begin
      X := 1;
      while (x <= length(ReplaceString)) and
            (not (ReplaceString[x] in [' ', #9, #13, #10])) do inc(x);
      if x > length(ReplaceString) then Result := ReplaceString
      else Result := AlcopyStr(ReplaceString,1,x-1);
    end;

    {-------------------------------------}
    Function _ExtractParamsStr: AnsiString;
    Begin
      Result := ALTrim( AlcopyStr(ReplaceString,length(TokenStr) + 1, MaxInt) );
    end;

begin
  Result := False;
  if (SourceString = '') or (TagStart = '') or (TagEnd = '') then Exit;

  If IgnoreCase then PosExFunct := ALPosExIgnoreCase
  Else PosExFunct := ALPosEx;

  SourceStringLength := length(SourceString);
  TagStartLength := Length(TagStart);
  TagEndLength := Length(TagEnd);
  TagEndFirstChar := TagEnd[1];
  TagEndFirstCharLower := ALLowerCase(TagEnd[1])[1];
  TagEndFirstCharUpper := ALUpperCase(TagEnd[1])[1];

  T1 := PosExFunct(TagStart,SourceString,1);
  T2 := T1 + TagStartLength;
  If (T1 > 0) and (T2 <= SourceStringLength) then begin
    InDoubleQuote := False;
    InsingleQuote := False;
    While (T2 <= SourceStringLength) and
          (InDoubleQuote or
           InSingleQuote or
           (IgnoreCase and (not (SourceString[T2] in [TagEndFirstCharLower, TagEndFirstCharUpper]))) or
           ((not IgnoreCase) and (SourceString[T2] <> TagEndFirstChar)) or
           (PosExFunct(TagEnd,AlCopyStr(SourceString,T2,TagEndLength),1) <> 1)) do begin
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
    ALExtractHeaderFieldsWithQuoteEscaped([' ', #9, #13, #10],
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
Procedure ALSplitTextAndTag(Const SourceString, TagStart, TagEnd: AnsiString;
                            SplitTextAndTagLst: TALStrings;
                            IgnoreCase: Boolean);

var TagEndFirstChar, TagEndFirstCharLower, TagEndFirstCharUpper: AnsiChar;
    TagStartLength: integer;
    TagEndLength: integer;
    SourceStringLength: Integer;
    SourceCurrentPos: integer;
    InDoubleQuote: Boolean;
    InsingleQuote: Boolean;
    PosExFunct: Function(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;
    T1,T2: Integer;

begin

  if (SourceString = '') or (TagStart = '') or (TagEnd = '') then begin
    if SourceString <> '' then SplitTextAndTagLst.Add(SourceString);
    Exit;
  end;

  If IgnoreCase then PosExFunct := ALPosExIgnoreCase
  Else PosExFunct := ALPosEx;

  SourceStringLength := length(SourceString);
  TagStartLength := Length(TagStart);
  TagEndLength := Length(TagEnd);
  TagEndFirstChar := TagEnd[1];
  TagEndFirstCharLower := ALLowerCase(TagEnd[1])[1];
  TagEndFirstCharUpper := ALUpperCase(TagEnd[1])[1];
  SourceCurrentPos := 1;

  T1 := PosExFunct(TagStart,SourceString,SourceCurrentPos);
  T2 := T1 + TagStartLength;
  If (T1 > 0) and (T2 <= SourceStringLength) then begin
    InDoubleQuote := False;
    InsingleQuote := False;
    While (T2 <= SourceStringLength) and
          (InDoubleQuote or
           InSingleQuote or
           (IgnoreCase and (not (SourceString[T2] in [TagEndFirstCharLower, TagEndFirstCharUpper]))) or
           ((not IgnoreCase) and (SourceString[T2] <> TagEndFirstChar)) or
           (PosExFunct(TagEnd,AlCopyStr(SourceString,T2,TagEndLength),1) <> 1)) do begin
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

    T1 := PosExFunct(TagStart,SourceString,SourceCurrentPos);
    T2 := T1 + TagStartLength;
    If (T1 > 0) and (T2 <= SourceStringLength) then begin
      InDoubleQuote := False;
      InsingleQuote := False;
      While (T2 <= SourceStringLength) and
            (InDoubleQuote or
             InSingleQuote or
             (IgnoreCase and (not (SourceString[T2] in [TagEndFirstCharLower, TagEndFirstCharUpper]))) or
             ((not IgnoreCase) and (SourceString[T2] <> TagEndFirstChar)) or
             (PosExFunct(TagEnd,AlCopyStr(SourceString,T2,TagEndLength),1) <> 1)) do begin
        If SourceString[T2] = '"' then InDoubleQuote := (not InDoubleQuote) and (not InSingleQuote)
        else If SourceString[T2] = '''' then InSingleQuote := (not InSingleQuote) and (not InDoubleQuote);
        inc(T2);
      end;
      if (T2 > SourceStringLength) then T2 := 0;
    end;
  end;

  SplitTextAndTagLst.AddObject(AlcopyStr(SourceString,SourceCurrentPos,maxint), pointer(0));

end;

{*************************************************************************************************************}
function ALGetStringFromFile(const filename: AnsiString; const ShareMode: Word = fmShareDenyWrite): AnsiString;
Var AFileStream: TfileStream;
begin
  AFileStream := TFileStream.Create(String(filename),fmOpenRead or ShareMode);
  try

    If AFileStream.size > 0 then begin
      SetLength(Result, AFileStream.size);
      AfileStream.ReadBuffer(pointer(Result)^,AfileStream.Size)
    end
    else Result := '';

  finally
    AfileStream.Free;
  end;
end;

{***************************************************************************************************************************}
function ALGetStringFromFileWithoutUTF8BOM(const filename: AnsiString; const ShareMode: Word = fmShareDenyWrite): AnsiString;
Var AFileStream: TfileStream;
    aBOMStr: AnsiString;
    aSize: Integer;
begin
  AFileStream := TFileStream.Create(String(filename),fmOpenRead or ShareMode);
  try

    aSize := AFileStream.size;
    If ASize > 0 then begin

      If Asize >= 3 then begin
        SetLength(aBOMStr,3);
        AfileStream.ReadBuffer(pointer(aBOMStr)^,3);
        If AlUTF8DetectBOM(PAnsiChar(aBOMStr), 3) then aSize := aSize - 3
        else AfileStream.Position := 0;
      end;

      If aSize > 0 then begin
        SetLength(Result, aSize);
        AfileStream.ReadBuffer(pointer(Result)^,ASize)
      end
      else Result := '';

    end
    else Result := '';

  finally
    AfileStream.Free;
  end;
end;

{********************************************************************************}
procedure ALAppendStringToFile(const Str: AnsiString; const FileName: AnsiString);
var aFileStream: TFileStream;
begin
  if FileExists(String(FileName)) then aFileStream := TFileStream.Create(String(FileName), fmOpenReadWrite)
  else                                 aFileStream := TFileStream.Create(String(FileName), fmCreate);
  try
    aFileStream.Position := aFileStream.Size;
    aFileStream.WriteBuffer(Pointer(Str)^, Length(Str));
  finally
    aFileStream.Free;
  end;
end;

{******************************************************************************}
procedure ALSaveStringtoFile(const Str: AnsiString; const filename: AnsiString);
Var AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(String(filename), fmCreate);
  try
    AFileStream.WriteBuffer(Pointer(Str)^, Length(Str));
  finally
    AFileStream.Free;
  end;
end;

{$ENDIF}

{**************************************************************}
function  ALGetBytesFromStream(const aStream : TStream): Tbytes;
var l: Integer;
begin
   l:=aStream.Size-aStream.Position;
   SetLength(result, l);
   aStream.ReadBuffer(result[0], l);
end;

{******************************************************************************************************}
function  ALGetBytesFromFileU(const filename: String; const ShareMode: Word = fmShareDenyWrite): Tbytes;
Var AFileStream: TfileStream;
begin
  AFileStream := TFileStream.Create(filename,fmOpenRead or ShareMode);
  try
    Result := ALGetBytesFromStream(AFileStream);
  finally
    ALFreeAndNil(AfileStream);
  end;
end;

{**********************************************************************************************}
function  ALGetStringFromBufferU(const buf : TBytes; const ADefaultEncoding: TEncoding): String;
var encoding : TEncoding;
    n : Integer;
begin
  encoding:=nil;
  n:=TEncoding.GetBufferEncoding(buf, encoding, ADefaultEncoding);
  Result:=encoding.GetString(buf, n, Length(buf)-n);
end;

{***************************************************************************************************}
function  ALGetStringFromStreamU(const aStream : TStream; const ADefaultEncoding: TEncoding): String;
var buf: Tbytes;
begin
   Buf := ALGetBytesFromStream(aStream);
   Result:=ALGetStringFromBufferU(buf, ADefaultEncoding);
end;

{******************************************************************************************************************************************}
function  ALGetStringFromFileU(const filename: String; const ADefaultEncoding: TEncoding; const ShareMode: Word = fmShareDenyWrite): String;
Var AFileStream: TfileStream;
begin
  AFileStream := TFileStream.Create(filename,fmOpenRead or ShareMode);
  try
    Result := ALGetStringFromStreamU(AFileStream, ADefaultEncoding);
  finally
    ALFreeAndNil(AfileStream);
  end;
end;

{******************************************************************************************************************************}
procedure ALSaveStringtoFileU(const Str: String; const filename: String; AEncoding: TEncoding; const WriteBOM: boolean = False);
var afileStream: TfileStream;
    Buffer, Preamble: TBytes;
begin
  aFileStream := TfileStream.Create(filename,fmCreate);
  Try

    Buffer := aEncoding.GetBytes(Str);

    if WriteBOM then begin
      Preamble := aEncoding.GetPreamble;
      if Length(Preamble) > 0 then
        afileStream.WriteBuffer(Preamble, Length(Preamble));
    end;

    afileStream.WriteBuffer(Buffer, Length(Buffer));

  finally
    ALFreeAndNil(aFileStream);
  end;
end;

{$IFNDEF NEXTGEN}

{**********************}
{$IF defined(MSWINDOWS)}
// Normalize a Widestring
// ie: l''été sur l''europe => l-ete-sur-l-europe
Function  ALWideNormalize(const S: Widestring;
                          const WordSeparator: WideChar;
                          const SymbolsToIgnore: array of WideChar): Widestring;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  {source: http://issues.apache.org/jira/browse/LUCENE-1343}
  Procedure _foldNonDiacriticChar(Var aStr: WideString);
  Var i, j : integer;
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
  var i: integer;
  begin
    result := False;
    for I := Low(SymbolsToIgnore) to High(SymbolsToIgnore) do
      if SymbolsToIgnore[i] = aSymbol then begin
        result := true;
        exit;
      end;
  end;

Var i,j: integer;
    TmpWideStr: WideString;

Begin
  TmpWideStr := ALWideExpandLigatures(ALWideRemoveDiacritic(Widelowercase(s)));
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
{$IFEND}

{**********************}
{$IF defined(MSWINDOWS)}
// Normalize a Widestring
// ie: l''été sur l''europe => l-ete-sur-l-europe
Function ALWideNormalize(const S: Widestring; const WordSeparator: WideChar = '-'): Widestring;
Begin
  result := ALWideNormalize(S, WordSeparator, []);
end;
{$IFEND}

{**********************}
{$IF defined(MSWINDOWS)}
{remove accented character}
Function ALWideRemoveDiacritic(const S: Widestring): Widestring;

  {----------------------------------------------------------------}
  Function internalGetCompositeCharSize(aChar: WideString): integer;
  Begin
    //max(1,xxx) in case FoldString return on error mean result = 0
    //this can really happen ?
    Result := Max(1, FoldStringW(MAP_COMPOSITE, PwideChar(aChar), length(aChar), nil, 0));
  end;

var LenS, LenTmpWideStr: Integer;
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
{$IFEND}

{**********************}
{$IF defined(MSWINDOWS)}
// Expand all ligature characters so that they are represented by
// their two-character equivalent. For example, the ligature 'æ' expands to
// the two characters 'a' and 'e'.}
Function ALWideExpandLigatures(const S: Widestring): Widestring;
Const aMAP_EXPAND_LIGATURES = $2000;
var LenS, LenResult: Integer;
begin
  result := '';
  If s = '' then exit;
  LenS := length(S);
  LenResult := FoldStringW(aMAP_EXPAND_LIGATURES, PwideChar(S), LenS, nil, 0);
  setlength(Result,LenResult);
  FoldStringW(aMAP_EXPAND_LIGATURES, PwideChar(S), LenS, PwideChar(Result), LenResult);
end;
{$IFEND}

{**********************}
{$IF defined(MSWINDOWS)}
Function ALWideUpperCaseNoDiacritic(const S: Widestring): Widestring;
begin
  Result := ALWideRemoveDiacritic(WideUppercase(s));
end;
{$IFEND}

{**********************}
{$IF defined(MSWINDOWS)}
Function ALWideLowerCaseNoDiacritic(const S: Widestring): Widestring;
begin
  Result := ALWideRemoveDiacritic(Widelowercase(s));
end;
{$IFEND}

{**********************}
{$IF defined(MSWINDOWS)}
// S is a AnsiString that contains UTF-8 encoded characters
// The result of the function is the corresponding UTF-8 encoded string
// without any Diacritic.
Function ALUTF8RemoveDiacritic(const S: AnsiString): AnsiString;
begin
  Result := utf8Encode(ALWideRemoveDiacritic(UTF8ToWideString(S)));
end;
{$IFEND}

{**********************}
{$IF defined(MSWINDOWS)}
// S is a AnsiString that contains UTF-8 encoded characters
// The result of the function is the corresponding UTF-8 encoded string
// without any Ligatures.
Function ALUTF8ExpandLigatures(const S: AnsiString): AnsiString;
begin
  Result := utf8Encode(ALWideExpandLigatures(UTF8ToWideString(S)));
end;
{$IFEND}

{**********************}
{$IF defined(MSWINDOWS)}
// S is a AnsiString that contains UTF-8 encoded characters
// The result of the function is the corresponding UTF-8 encoded string
// in UpperCase without any Diacritic.
Function ALUTF8UpperCaseNoDiacritic(const S: AnsiString): AnsiString;
begin
  Result := utf8Encode(ALWideUpperCaseNoDiacritic(UTF8ToWideString(S)));
end;
{$IFEND}

{**********************}
{$IF defined(MSWINDOWS)}
// S is a AnsiString that contains UTF-8 encoded characters
// The result of the function is the corresponding UTF-8 encoded string
// in LowerCase without any Diacritic.
Function ALUTF8LowerCaseNoDiacritic(const S: AnsiString): AnsiString;
begin
  Result := utf8Encode(ALWideLowerCaseNoDiacritic(UTF8ToWideString(S)));
end;
{$IFEND}

{**********************}
{$IF defined(MSWINDOWS)}
// S is a AnsiString that contains UTF-8 encoded characters
// The result of the function is the corresponding UTF-8 encoded string
// "normalized":
// ie: l''été sur l''europe => l-ete-sur-l-europe
Function  ALUTF8Normalize(const S: AnsiString;
                          const WordSeparator: ansiChar;
                          const SymbolsToIgnore: array of AnsiChar): AnsiString;
var aWideSymbolsToIgnore: Array of WideChar;
    i: integer;
begin
  setlength(aWideSymbolsToIgnore, length(SymbolsToIgnore));
  for I := Low(SymbolsToIgnore) to High(SymbolsToIgnore) do
    aWideSymbolsToIgnore[i] := WideChar(SymbolsToIgnore[i]);
  Result := utf8Encode(ALWideNormalize(UTF8ToWideString(S), WideChar(WordSeparator)));
end;
{$IFEND}

{**********************}
{$IF defined(MSWINDOWS)}
// S is a AnsiString that contains UTF-8 encoded characters
// The result of the function is the corresponding UTF-8 encoded string
// "normalized":
// ie: l''été sur l''europe => l-ete-sur-l-europe
Function ALUTF8Normalize(const S: AnsiString;
                         const WordSeparator: ansiChar = '-'): AnsiString;
begin
  Result := utf8Encode(ALWideNormalize(UTF8ToWideString(S), WideChar(WordSeparator), []));
end;
{$IFEND}

{*********************************************************}
// S is a AnsiString that contains UTF-8 encoded characters
// The result of the function is the corresponding UTF-8 encoded string
// in LowerCase. this function use CharLowerBuffW
// The only problem I know that makes Unicode uppercase/lowercase conversion
// locale-dependent is the case of dotless i (ı, $0131) and dotted I (İ, $0130).
// In most languages the upper of i ($69) is I ($49), but in turkish locale i ($69)
// maps to İ ($0130). Similarly in turkish the lower of I ($49) is ı ($0131).
// CharUpperBuff/CharLowerBuffW always maps lowercase I ("i") to uppercase I,
// even when the current language is Turkish or Azeri
function ALUTF8UpperCase(const s: AnsiString): AnsiString;
begin
  result := utf8encode(WideUppercase(UTF8ToWideString(s)));
end;

{*********************************************************}
// S is a AnsiString that contains UTF-8 encoded characters
// The result of the function is the corresponding UTF-8 encoded string
// in LowerCase. this function use CharLowerBuffW
// The only problem I know that makes Unicode uppercase/lowercase conversion
// locale-dependent is the case of dotless i (ı, $0131) and dotted I (İ, $0130).
// In most languages the upper of i ($69) is I ($49), but in turkish locale i ($69)
// maps to İ ($0130). Similarly in turkish the lower of I ($49) is ı ($0131).
// CharUpperBuff/CharLowerBuffW always maps lowercase I ("i") to uppercase I,
// even when the current language is Turkish or Azeri
function ALUTF8LowerCase(const s: AnsiString): AnsiString;
begin
  result := utf8encode(WideLowerCase(UTF8ToWideString(s)));
end;

{**********************}
{$IF defined(MSWINDOWS)}
function AlUTF8Check(const S: AnsiString): Boolean;
begin
  if S = '' then exit(true);
  Result := MultiByteToWideChar(CP_UTF8, //UINT CodePage)
                                8{8=MB_ERR_INVALID_CHARS that is not defined in d2007}, // DWORD dwFlags
                                PAnsiChar(S), // LPCSTR lpMultiByteStr,
                                length(S), // int cbMultiByte
                                nil, // LPWSTR lpWideCharStr
                                0) > 0; // int cchWideChar
end;
{$IFEND}

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
function ALUTF8CharSize(Lead: AnsiChar): Integer;
begin
  case Lead of
    #$00..#$7F: Result := 1; //
    #$C2..#$DF: Result := 2; // 110x xxxx C0 - DF
    #$E0..#$EF: Result := 3; // 1110 xxxx E0 - EF
    #$F0..#$F7: Result := 4; // 1111 0xxx F0 - F7 // outside traditional UNICODE
    #$F8..#$FB: Result := 5; // 1111 10xx F8 - FB // outside UTF-16
    #$FC..#$FD: Result := 6; // 1111 110x FC - FD // outside UTF-16
  else
    Result := 1; // Illegal leading character.
  end;
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

{*****************************}
{Uppercase only the First char}
Function ALUTF8UpperFirstChar(const s:AnsiString): AnsiString;
var tmpWideStr: WideString;
begin
  TmpWideStr := UTF8ToWideString(S);
  result := utf8encode(WideUpperCase(copy(TmpWideStr,1,1)) + copy(TmpWideStr,2,MaxInt));
end;

{*********************************************}
//the first letter of each word is capitalized,
//the rest are lower case
Function ALUTF8TitleCase(const s:AnsiString): AnsiString;
var tmpWideStr: WideString;
    i: integer;
begin
  TmpWideStr := UTF8ToWideString(S);
  if length(TmpWideStr) = 0 then begin
    result := '';
    exit;
  end;
  TmpWideStr := WideUpperCase(copy(TmpWideStr,1,1)) + WidelowerCase(copy(TmpWideStr,2,MaxInt));
  for i:= 2 to length(TmpWideStr) do
    if ((TmpWideStr[i-1] = WideChar('&')) or
        (TmpWideStr[i-1] = WideChar(' ')) or
        (TmpWideStr[i-1] = WideChar('-')) or
        (TmpWideStr[i-1] = WideChar('''')))
       and
       (
        (i = length(TmpWideStr)) or
        (
         ((TmpWideStr[i+1] <> ' ') or (TmpWideStr[i-1] = '&')) and // Agenge L&G Prestige - Maison à Vendre - A Prendre Ou a Laisser
         (TmpWideStr[i+1] <> '''') // Avenue de l'Elysée
        )
       )
    then TmpWideStr[i] := WideUpperCase(TmpWideStr[i])[1];
  result := utf8encode(TmpWideStr);
end;

{****************************************************************}
// first letter of the sentence capitalized, all others lower case
Function ALUTF8SentenceCase(const s:AnsiString): AnsiString;
begin
  Result := AlUtf8LowerCase(S);
  Result := ALUTF8UpperFirstChar(Result);
end;

{***************************************************************}
Function ALGetCodePageFromCharSetName(Acharset:AnsiString): Word;
{$IF CompilerVersion >= 23} {Delphi XE2}
Var aEncoding: Tencoding;
begin
  Try

    if Acharset = '' then begin
      result := 0; // Default ansi code page
      exit;
    end;

    aEncoding := Tencoding.GetEncoding(String(Acharset));
    Try
      Result := aEncoding.CodePage;
    Finally
      aEncoding.Free;
    end;

  Except
    Result := 0; // Default ansi code page
  end;
end;
{$ELSE}
begin
  Acharset := ALTrim(AlLowerCase(ACharset));
  if acharset='utf-8' then result := 65001 // unicode (utf-8)
  else if acharset='iso-8859-1' then result := 28591 // western european (iso)
  else if acharset='iso-8859-2' then result := 28592 // central european (iso)
  else if acharset='iso-8859-3' then result := 28593 // latin 3 (iso)
  else if acharset='iso-8859-4' then result := 28594 // baltic (iso)
  else if acharset='iso-8859-5' then result := 28595 // cyrillic (iso)
  else if acharset='iso-8859-6' then result := 28596 // arabic (iso)
  else if acharset='iso-8859-7' then result := 28597 // greek (iso)
  else if acharset='iso-8859-8' then result := 28598 // hebrew (iso-visual)
  else if acharset='iso-8859-9' then result := 28599 // turkish (iso)
  else if acharset='iso-8859-13' then result := 28603 // estonian (iso)
  else if acharset='iso-8859-15' then result := 28605 // latin 9 (iso)
  else if acharset='ibm037' then result := 37 // ibm ebcdic (us-canada)
  else if acharset='ibm437' then result := 437 // oem united states
  else if acharset='ibm500' then result := 500 // ibm ebcdic (international)
  else if acharset='asmo-708' then result := 708 // arabic (asmo 708)
  else if acharset='dos-720' then result := 720 // arabic (dos)
  else if acharset='ibm737' then result := 737 // greek (dos)
  else if acharset='ibm775' then result := 775 // baltic (dos)
  else if acharset='ibm850' then result := 850 // western european (dos)
  else if acharset='ibm852' then result := 852 // central european (dos)
  else if acharset='ibm855' then result := 855 // oem cyrillic
  else if acharset='ibm857' then result := 857 // turkish (dos)
  else if acharset='ibm00858' then result := 858 // oem multilingual latin i
  else if acharset='ibm860' then result := 860 // portuguese (dos)
  else if acharset='ibm861' then result := 861 // icelandic (dos)
  else if acharset='dos-862' then result := 862 // hebrew (dos)
  else if acharset='ibm863' then result := 863 // french canadian (dos)
  else if acharset='ibm864' then result := 864 // arabic (864)
  else if acharset='ibm865' then result := 865 // nordic (dos)
  else if acharset='cp866' then result := 866 // cyrillic (dos)
  else if acharset='ibm869' then result := 869 // greek, modern (dos)
  else if acharset='ibm870' then result := 870 // ibm ebcdic (multilingual latin-2)
  else if acharset='windows-874' then result := 874 // thai (windows)
  else if acharset='cp875' then result := 875 // ibm ebcdic (greek modern)
  else if acharset='shift_jis' then result := 932 // japanese (shift-jis)
  else if acharset='gb2312' then result := 936 // chinese simplified (gb2312)
  else if acharset='ks_c_5601-1987' then result := 949 // korean
  else if acharset='big5' then result := 950 // chinese traditional (big5)
  else if acharset='ibm1026' then result := 1026 // ibm ebcdic (turkish latin-5)
  else if acharset='ibm01047' then result := 1047 // ibm latin-1
  else if acharset='ibm01140' then result := 1140 // ibm ebcdic (us-canada-euro)
  else if acharset='ibm01141' then result := 1141 // ibm ebcdic (germany-euro)
  else if acharset='ibm01142' then result := 1142 // ibm ebcdic (denmark-norway-euro)
  else if acharset='ibm01143' then result := 1143 // ibm ebcdic (finland-sweden-euro)
  else if acharset='ibm01144' then result := 1144 // ibm ebcdic (italy-euro)
  else if acharset='ibm01145' then result := 1145 // ibm ebcdic (spain-euro)
  else if acharset='ibm01146' then result := 1146 // ibm ebcdic (uk-euro)
  else if acharset='ibm01147' then result := 1147 // ibm ebcdic (france-euro)
  else if acharset='ibm01148' then result := 1148 // ibm ebcdic (international-euro)
  else if acharset='ibm01149' then result := 1149 // ibm ebcdic (icelandic-euro)
  else if acharset='utf-16' then result := 1200 // unicode
  else if acharset='unicodefffe' then result := 1201 // unicode (big-endian)
  else if acharset='windows-1250' then result := 1250 // central european (windows)
  else if acharset='windows-1251' then result := 1251 // cyrillic (windows)
  else if acharset='windows-1252' then result := 1252 // western european (windows)
  else if acharset='windows-1253' then result := 1253 // greek (windows)
  else if acharset='windows-1254' then result := 1254 // turkish (windows)
  else if acharset='windows-1255' then result := 1255 // hebrew (windows)
  else if acharset='windows-1256' then result := 1256 // arabic (windows)
  else if acharset='windows-1257' then result := 1257 // baltic (windows)
  else if acharset='windows-1258' then result := 1258 // vietnamese (windows)
  else if acharset='johab' then result := 1361 // korean (johab)
  else if acharset='macintosh' then result := 10000 // western european (mac)
  else if acharset='x-mac-japanese' then result := 10001 // japanese (mac)
  else if acharset='x-mac-chinesetrad' then result := 10002 // chinese traditional (mac)
  else if acharset='x-mac-korean' then result := 10003 // korean (mac)
  else if acharset='x-mac-arabic' then result := 10004 // arabic (mac)
  else if acharset='x-mac-hebrew' then result := 10005 // hebrew (mac)
  else if acharset='x-mac-greek' then result := 10006 // greek (mac)
  else if acharset='x-mac-cyrillic' then result := 10007 // cyrillic (mac)
  else if acharset='x-mac-chinesesimp' then result := 10008 // chinese simplified (mac)
  else if acharset='x-mac-romanian' then result := 10010 // romanian (mac)
  else if acharset='x-mac-ukrainian' then result := 10017 // ukrainian (mac)
  else if acharset='x-mac-thai' then result := 10021 // thai (mac)
  else if acharset='x-mac-ce' then result := 10029 // central european (mac)
  else if acharset='x-mac-icelandic' then result := 10079 // icelandic (mac)
  else if acharset='x-mac-turkish' then result := 10081 // turkish (mac)
  else if acharset='x-mac-croatian' then result := 10082 // croatian (mac)
  else if acharset='x-chinese-cns' then result := 20000 // chinese traditional (cns)
  else if acharset='x-cp20001' then result := 20001 // tca taiwan
  else if acharset='x-chinese-eten' then result := 20002 // chinese traditional (eten)
  else if acharset='x-cp20003' then result := 20003 // ibm5550 taiwan
  else if acharset='x-cp20004' then result := 20004 // teletext taiwan
  else if acharset='x-cp20005' then result := 20005 // wang taiwan
  else if acharset='x-ia5' then result := 20105 // western european (ia5)
  else if acharset='x-ia5-german' then result := 20106 // german (ia5)
  else if acharset='x-ia5-swedish' then result := 20107 // swedish (ia5)
  else if acharset='x-ia5-norwegian' then result := 20108 // norwegian (ia5)
  else if acharset='us-ascii' then result := 20127 // us-ascii
  else if acharset='x-cp20261' then result := 20261 // t.61
  else if acharset='x-cp20269' then result := 20269 // iso-6937
  else if acharset='ibm273' then result := 20273 // ibm ebcdic (germany)
  else if acharset='ibm277' then result := 20277 // ibm ebcdic (denmark-norway)
  else if acharset='ibm278' then result := 20278 // ibm ebcdic (finland-sweden)
  else if acharset='ibm280' then result := 20280 // ibm ebcdic (italy)
  else if acharset='ibm284' then result := 20284 // ibm ebcdic (spain)
  else if acharset='ibm285' then result := 20285 // ibm ebcdic (uk)
  else if acharset='ibm290' then result := 20290 // ibm ebcdic (japanese katakana)
  else if acharset='ibm297' then result := 20297 // ibm ebcdic (france)
  else if acharset='ibm420' then result := 20420 // ibm ebcdic (arabic)
  else if acharset='ibm423' then result := 20423 // ibm ebcdic (greek)
  else if acharset='ibm424' then result := 20424 // ibm ebcdic (hebrew)
  else if acharset='x-ebcdic-koreanextended' then result := 20833 // ibm ebcdic (korean extended)
  else if acharset='ibm-thai' then result := 20838 // ibm ebcdic (thai)
  else if acharset='koi8-r' then result := 20866 // cyrillic (koi8-r)
  else if acharset='ibm871' then result := 20871 // ibm ebcdic (icelandic)
  else if acharset='ibm880' then result := 20880 // ibm ebcdic (cyrillic russian)
  else if acharset='ibm905' then result := 20905 // ibm ebcdic (turkish)
  else if acharset='ibm00924' then result := 20924 // ibm latin-1
  else if acharset='euc-jp' then result := 20932 // japanese (jis 0208-1990 and 0212-1990)
  else if acharset='x-cp20936' then result := 20936 // chinese simplified (gb2312-80)
  else if acharset='x-cp20949' then result := 20949 // korean wansung
  else if acharset='cp1025' then result := 21025 // ibm ebcdic (cyrillic serbian-bulgarian)
  else if acharset='koi8-u' then result := 21866 // cyrillic (koi8-u)
  else if acharset='x-europa' then result := 29001 // europa
  else if acharset='iso-8859-8-i' then result := 38598 // hebrew (iso-logical)
  else if acharset='iso-2022-jp' then result := 50220 // japanese (jis)
  else if acharset='csiso2022jp' then result := 50221 // japanese (jis-allow 1 byte kana)
  else if acharset='iso-2022-jp' then result := 50222 // japanese (jis-allow 1 byte kana - so/si)
  else if acharset='iso-2022-kr' then result := 50225 // korean (iso)
  else if acharset='x-cp50227' then result := 50227 // chinese simplified (iso-2022)
  else if acharset='euc-jp' then result := 51932 // japanese (euc)
  else if acharset='euc-cn' then result := 51936 // chinese simplified (euc)
  else if acharset='euc-kr' then result := 51949 // korean (euc)
  else if acharset='hz-gb-2312' then result := 52936 // chinese simplified (hz)
  else if acharset='gb18030' then result := 54936 // chinese simplified (gb18030)
  else if acharset='x-iscii-de' then result := 57002 // iscii devanagari
  else if acharset='x-iscii-be' then result := 57003 // iscii bengali
  else if acharset='x-iscii-ta' then result := 57004 // iscii tamil
  else if acharset='x-iscii-te' then result := 57005 // iscii telugu
  else if acharset='x-iscii-as' then result := 57006 // iscii assamese
  else if acharset='x-iscii-or' then result := 57007 // iscii oriya
  else if acharset='x-iscii-ka' then result := 57008 // iscii kannada
  else if acharset='x-iscii-ma' then result := 57009 // iscii malayalam
  else if acharset='x-iscii-gu' then result := 57010 // iscii gujarati
  else if acharset='x-iscii-pa' then result := 57011 // iscii punjabi
  else if acharset='utf-7' then result := 65000 // unicode (utf-7)
  else if acharset='utf-32' then result := 65005 // unicode (utf-32)
  else if acharset='utf-32be' then result := 65006 // unicode (utf-32 big-endian)
  else Result := 0; //Default ansi code page
end;
{$IFEND}

{**********************}
{$IF defined(MSWINDOWS)}
Function ALGetCodePageFromLCID(const aLCID:Integer): Word;
var
  Buffer: array [0..6] of AnsiChar;
begin
  GetLocaleInfoA(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, Length(Buffer));
  Result:= ALStrToIntDef(Buffer, 0);
end;
{$IFEND}

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
  OutputLength := MultiByteToWideChar(aCodePage,     // UINT CodePage,
                                      0,             // DWORD dwFlags
                                      PAnsiChar(S),  // LPCSTR lpMultiByteStr
                                      InputLength,   // int cbMultiByte
                                      nil,           // LPWSTR lpWideCharStr
                                      0);            // int cchWideChar
  if OutputLength = 0 then raiseLastOsError;
  SetLength(Result, OutputLength);
  if MultiByteToWideChar(aCodePage,
                         0,
                         PAnsiChar(S),
                         InputLength,
                         PWideChar(Result),
                         OutputLength) = 0 then raiseLastOsError;
end;
{$IFEND}

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
  OutputLength := WideCharToMultiByte(aCodePage,      // UINT CodePage
                                      0,              // DWORD dwFlags,
                                      PWideChar(WS),  // LPCWSTR lpWideCharStr,
                                      InputLength,    // int cchWideChar
                                      nil,            // LPSTR lpMultiByteStr,
                                      0,              // int cbMultiByte
                                      nil,            // LPCSTR lpDefaultChar (Pointer to the character to use if a character cannot be represented in the specified code page)
                                      nil);           // LPBOOL lpUsedDefaultChar (Pointer to a flag that indicates if the function has used a default character in the conversion)
  if OutputLength = 0 then raiseLastOsError;
  SetLength(Result, OutputLength);
  if WideCharToMultiByte(aCodePage,
                         0,
                         PWideChar(WS),
                         InputLength,
                         PAnsiChar(Result),
                         OutputLength,
                         nil,
                         nil) = 0 then raiseLastOsError;
end;
{$IFEND}

{*******************************************************************************}
Function ALUTF8Encode(const S: RawByteString; const aCodePage: Word): AnsiString;
var TmpS: RawByteString;
begin
  //Result := UTF8Encode(ALStringToWideString(S, aCodePage));
  //it's look like the code below is a little (15%) more faster then
  //the previous implementation, and it's compatible with OSX
  TmpS := S;
  SetCodePage(TmpS, aCodePage, False);
  result := UTF8Encode(String(TmpS));
end;

{****************************************************************************}
Function ALUTF8decode(const S: UTF8String; const aCodePage: Word): AnsiString;
begin
  //Result := ALWideStringToString(UTF8ToWideString(S), aCodePage);
  //it's look like the code below is a little (15%) more faster then
  //the previous implementation, and it's compatible with OSX
  result := ansiString(S);
  SetCodePage(RawByteString(Result), aCodePage, true);
end;

{*******************************************************************************************
 ISO 9:1995 is the current transliteration standard from ISO. It is based on its predecessor
 ISO/R 9:1968, which it deprecates; for Russian they only differ in the treatment of five
 modern letters. It is the first language-independent, univocal system of one character
 for one character equivalents (by the use of diacritics), which faithfully represents
 the original and allows for reverse transliteration for Cyrillic text in any contemporary language.}
Function ALUTF8ISO91995CyrillicToLatin(const aCyrillicText: AnsiString): AnsiString;

Var aCyrillicWideStr: WideString;
    aLatinWideStr: WideString;
    aLatinWideStrCurrentPos: Integer;

  {-----------------------------------------------------------------}
  Procedure InternalAddCharsToResult(aArrayOfChar: Array of Integer);
  var i: integer;
  begin
    for i := low(aArrayOfChar) to high(aArrayOfChar) do begin
      inc(aLatinWideStrCurrentPos);
      aLatinWideStr[aLatinWideStrCurrentPos] := WideChar(aArrayOfChar[i]);
    end;
  end;

Var I, j: Integer;

Begin
  Result := '';
  aCyrillicWideStr := UTF8ToWideString(aCyrillicText);
  setlength(ALatinWideStr,length(aCyrillicWideStr) * 2); //to Be on the safe way
  aLatinWideStrCurrentPos := 0;
  for i := 1 to length(aCyrillicWideStr) do begin
    j := ord(aCyrillicWideStr[i]);
    case j of
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
  SetLength(aLatinWideStr,aLatinWideStrCurrentPos);
  Result := UTF8Encode(aLatinWideStr);
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
Function ALUTF8BGNPCGN1947CyrillicToLatin(const aCyrillicText: AnsiString): AnsiString;

Var aCyrillicWideStr: WideString;
    aLatinWideStr: WideString;
    aLatinWideStrCurrentPos: Integer;

  {-----------------------------------------------------------------}
  Procedure InternalAddCharsToResult(aArrayOfChar: Array of Integer);
  var i: integer;
  begin
    for i := low(aArrayOfChar) to high(aArrayOfChar) do begin
      inc(aLatinWideStrCurrentPos);
      aLatinWideStr[aLatinWideStrCurrentPos] := WideChar(aArrayOfChar[i]);
    end;
  end;

  {-------------------------------------------------------------------------------------}
  function InternalCheckInRange(aChar: integer; aArrayOfChar: Array of Integer): boolean;
  var i: integer;
  begin
    Result := False;
    for i := low(aArrayOfChar) to high(aArrayOfChar) do
      if aChar = aArrayOfChar[i] then begin
        result := true;
        exit;
      end;
  end;

Var I, j: Integer;
Begin
  Result := '';
  aCyrillicWideStr := UTF8ToWideString(aCyrillicText);
  setlength(ALatinWideStr,length(aCyrillicWideStr) * 2); //to Be on the safe way
  aLatinWideStrCurrentPos := 0;
  for i := 1 to length(aCyrillicWideStr) do begin
    j := ord(aCyrillicWideStr[i]);
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
                    if (i > 1) and InternalCheckInRange(ord(aCyrillicWideStr[i-1]),[$0410 {А}, $0430 {а},
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
                    if (i > 1) and InternalCheckInRange(ord(aCyrillicWideStr[i-1]),[$0410 {А}, $0430 {а},
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
                    if (i > 1) and InternalCheckInRange(ord(aCyrillicWideStr[i-1]),[$0410 {А}, $0430 {а},
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
                    if (i > 1) and InternalCheckInRange(ord(aCyrillicWideStr[i-1]),[$0410 {А}, $0430 {а},
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
  SetLength(aLatinWideStr,aLatinWideStrCurrentPos);
  Result := UTF8Encode(aLatinWideStr);
End;

{***********************************************}
function ALExtractExpression(const S: AnsiString;
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

var aCurInQuote: boolean;
    aCurQuoteChar: AnsiChar;
    aOpenCount: integer;

begin
  result := false;
  if StartPos <= 0 then StartPos := 1;
  while (StartPos <= length(S)) and
        (s[StartPos] <> OpenChar) do inc(StartPos);
  if StartPos > length(S) then exit;
  aOpenCount := 1;
  aCurInQuote := False;
  aCurQuoteChar := #0;
  EndPos := StartPos + 1;
  while (EndPos <= length(S)) and
        (aOpenCount > 0) do begin
    if _IsQuote(s[EndPos]) then begin
      if aCurInQuote then begin
        if (s[EndPos] = aCurQuoteChar) then aCurInQuote := False
      end
      else begin
        aCurInQuote := True;
        aCurQuoteChar := s[EndPos];
      end;
    end
    else if not aCurInQuote then begin
      if s[EndPos] = OpenChar then inc(aOpenCount)
      else if s[EndPos] = CloseChar then dec(aOpenCount);
    end;
    if aOpenCount <> 0 then inc(EndPos);
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
procedure ALExtractHeaderFields(Separators,
                                WhiteSpace,
                                Quotes: TSysCharSet;
                                Content: PAnsiChar;
                                Strings: TALStrings;
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
procedure ALExtractHeaderFieldsWithQuoteEscaped(Separators,
                                                WhiteSpace,
                                                Quotes: TSysCharSet;
                                                Content: PAnsiChar;
                                                Strings: TALStrings;
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

{$ENDIF}

{**************************************************}
function  ALHTTPEncodeU(const AStr: String): String;
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
function ALHTTPDecodeU(const AStr: String): String;
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
                 if ALTryStrToIntU(s,int) then Rb[i] := int
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

{**************************************************************************************}
{same as ALExtractHeaderFields except the it take care or escaped quote (like '' or "")}
{$ZEROBASEDSTRINGS OFF} // << the guy who introduce zero base string in delphi is just a mix of a Monkey and a Donkey !
{$WARN SYMBOL_DEPRECATED OFF}
procedure ALExtractHeaderFieldsWithQuoteEscapedU(Separators,
                                                 WhiteSpace,
                                                 Quotes: TSysCharSet;
                                                 Content: PChar;
                                                 Strings: TALStringsU;
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
      if HttpDecode then Strings.Add(ALHTTPDecodeU(DoStripQuotes(ExtractedField)))
      else Strings.Add(DoStripQuotes(ExtractedField));
    end;
    Inc(Tail);
  until EOS;
end;
{$WARN SYMBOL_DEPRECATED ON}
{$IF defined(_ZEROBASEDSTRINGS_ON)}
  {$ZEROBASEDSTRINGS ON}
{$IFEND}

{*******************************}
Procedure ALStringInitialization;
{$IFNDEF NEXTGEN}
var i: integer;
{$ENDIF}
begin

  {$IFNDEF NEXTGEN}

  //
  // Taken from https://github.com/synopse/mORMot.git
  // https://synopse.info
  // http://mormot.net
  //

  {$IF CompilerVersion > 33} // rio
    {$MESSAGE WARN 'Check if https://github.com/synopse/mORMot.git SynCommons.pas was not updated from references\mORMot\SynCommons.pas and adjust the IFDEF'}
  {$IFEND}

  Fillchar(ConvertBase64ToBin,256,255); // invalid value set to -1
  for i := 0 to high(b64enc) do
    ConvertBase64ToBin[b64enc[i]] := i;
  ConvertBase64ToBin['='] := -2; // special value for '='

  {$ENDIF}

  {$IF CompilerVersion >= 31} // berlin
  _Base64Encoding := nil;
  {$IFEND}

  //https://stackoverflow.com/questions/50590627/tformatsettings-createen-us-returns-different-settings-on-different-platform
  {$IFNDEF NEXTGEN}
  ALPosExIgnoreCaseInitialiseLookupTable;
  ALDefaultFormatSettings := TALFormatSettings.Create('en-US'); // 1033 {en-US}
  ALDefaultFormatSettings.CurrencyString := '$';
  ALDefaultFormatSettings.CurrencyFormat := 0;
  ALDefaultFormatSettings.CurrencyDecimals := 2;
  ALDefaultFormatSettings.DateSeparator := '/';
  ALDefaultFormatSettings.TimeSeparator := ':';
  ALDefaultFormatSettings.ListSeparator := ';';
  ALDefaultFormatSettings.ShortDateFormat := 'M/d/yyyy';
  ALDefaultFormatSettings.LongDateFormat := 'dddd, MMMM d, yyyy';
  ALDefaultFormatSettings.TimeAMString := 'AM';
  ALDefaultFormatSettings.TimePMString := 'PM';
  ALDefaultFormatSettings.ShortTimeFormat := 'h:mm AMPM';
  ALDefaultFormatSettings.LongTimeFormat := 'h:mm:ss AMPM';
  ALDefaultFormatSettings.ShortMonthNames[1] := 'Jan';
  ALDefaultFormatSettings.LongMonthNames [1] := 'January';
  ALDefaultFormatSettings.ShortMonthNames[2] := 'Feb';
  ALDefaultFormatSettings.LongMonthNames [2] := 'February';
  ALDefaultFormatSettings.ShortMonthNames[3] := 'Mar';
  ALDefaultFormatSettings.LongMonthNames [3] := 'March';
  ALDefaultFormatSettings.ShortMonthNames[4] := 'Apr';
  ALDefaultFormatSettings.LongMonthNames [4] := 'April';
  ALDefaultFormatSettings.ShortMonthNames[5] := 'May';
  ALDefaultFormatSettings.LongMonthNames [5] := 'May';
  ALDefaultFormatSettings.ShortMonthNames[6] := 'Jun';
  ALDefaultFormatSettings.LongMonthNames [6] := 'June';
  ALDefaultFormatSettings.ShortMonthNames[7] := 'Jul';
  ALDefaultFormatSettings.LongMonthNames [7] := 'July';
  ALDefaultFormatSettings.ShortMonthNames[8] := 'Aug';
  ALDefaultFormatSettings.LongMonthNames [8] := 'August';
  ALDefaultFormatSettings.ShortMonthNames[9] := 'Sep';
  ALDefaultFormatSettings.LongMonthNames [9] := 'September';
  ALDefaultFormatSettings.ShortMonthNames[10] := 'Oct';
  ALDefaultFormatSettings.LongMonthNames [10] := 'October';
  ALDefaultFormatSettings.ShortMonthNames[11] := 'Nov';
  ALDefaultFormatSettings.LongMonthNames [11] := 'November';
  ALDefaultFormatSettings.ShortMonthNames[12] := 'Dec';
  ALDefaultFormatSettings.LongMonthNames [12] := 'December';
  ALDefaultFormatSettings.ShortDayNames[1] := 'Sun';
  ALDefaultFormatSettings.LongDayNames [1] := 'Sunday';
  ALDefaultFormatSettings.ShortDayNames[2] := 'Mon';
  ALDefaultFormatSettings.LongDayNames [2] := 'Monday';
  ALDefaultFormatSettings.ShortDayNames[3] := 'Tue';
  ALDefaultFormatSettings.LongDayNames [3] := 'Tuesday';
  ALDefaultFormatSettings.ShortDayNames[4] := 'Wed';
  ALDefaultFormatSettings.LongDayNames [4] := 'Wednesday';
  ALDefaultFormatSettings.ShortDayNames[5] := 'Thu';
  ALDefaultFormatSettings.LongDayNames [5] := 'Thursday';
  ALDefaultFormatSettings.ShortDayNames[6] := 'Fri';
  ALDefaultFormatSettings.LongDayNames [6] := 'Friday';
  ALDefaultFormatSettings.ShortDayNames[7] := 'Sat';
  ALDefaultFormatSettings.LongDayNames [7] := 'Saturday';
  ALDefaultFormatSettings.ThousandSeparator := ',';
  ALDefaultFormatSettings.DecimalSeparator := '.';
  ALDefaultFormatSettings.TwoDigitYearCenturyWindow := 50;
  ALDefaultFormatSettings.NegCurrFormat := 0;
  {$ENDIF}
  ALDefaultFormatSettingsU := TALFormatSettingsU.Create('en-US'); // 1033 {en-US}
  ALDefaultFormatSettingsU.CurrencyString := '$';
  ALDefaultFormatSettingsU.CurrencyFormat := 0;
  ALDefaultFormatSettingsU.CurrencyDecimals := 2;
  ALDefaultFormatSettingsU.DateSeparator := '/';
  ALDefaultFormatSettingsU.TimeSeparator := ':';
  ALDefaultFormatSettingsU.ListSeparator := ';';
  ALDefaultFormatSettingsU.ShortDateFormat := 'M/d/yyyy';
  ALDefaultFormatSettingsU.LongDateFormat := 'dddd, MMMM d, yyyy';
  ALDefaultFormatSettingsU.TimeAMString := 'AM';
  ALDefaultFormatSettingsU.TimePMString := 'PM';
  ALDefaultFormatSettingsU.ShortTimeFormat := 'h:mm AMPM';
  ALDefaultFormatSettingsU.LongTimeFormat := 'h:mm:ss AMPM';
  ALDefaultFormatSettingsU.ShortMonthNames[1] := 'Jan';
  ALDefaultFormatSettingsU.LongMonthNames [1] := 'January';
  ALDefaultFormatSettingsU.ShortMonthNames[2] := 'Feb';
  ALDefaultFormatSettingsU.LongMonthNames [2] := 'February';
  ALDefaultFormatSettingsU.ShortMonthNames[3] := 'Mar';
  ALDefaultFormatSettingsU.LongMonthNames [3] := 'March';
  ALDefaultFormatSettingsU.ShortMonthNames[4] := 'Apr';
  ALDefaultFormatSettingsU.LongMonthNames [4] := 'April';
  ALDefaultFormatSettingsU.ShortMonthNames[5] := 'May';
  ALDefaultFormatSettingsU.LongMonthNames [5] := 'May';
  ALDefaultFormatSettingsU.ShortMonthNames[6] := 'Jun';
  ALDefaultFormatSettingsU.LongMonthNames [6] := 'June';
  ALDefaultFormatSettingsU.ShortMonthNames[7] := 'Jul';
  ALDefaultFormatSettingsU.LongMonthNames [7] := 'July';
  ALDefaultFormatSettingsU.ShortMonthNames[8] := 'Aug';
  ALDefaultFormatSettingsU.LongMonthNames [8] := 'August';
  ALDefaultFormatSettingsU.ShortMonthNames[9] := 'Sep';
  ALDefaultFormatSettingsU.LongMonthNames [9] := 'September';
  ALDefaultFormatSettingsU.ShortMonthNames[10] := 'Oct';
  ALDefaultFormatSettingsU.LongMonthNames [10] := 'October';
  ALDefaultFormatSettingsU.ShortMonthNames[11] := 'Nov';
  ALDefaultFormatSettingsU.LongMonthNames [11] := 'November';
  ALDefaultFormatSettingsU.ShortMonthNames[12] := 'Dec';
  ALDefaultFormatSettingsU.LongMonthNames [12] := 'December';
  ALDefaultFormatSettingsU.ShortDayNames[1] := 'Sun';
  ALDefaultFormatSettingsU.LongDayNames [1] := 'Sunday';
  ALDefaultFormatSettingsU.ShortDayNames[2] := 'Mon';
  ALDefaultFormatSettingsU.LongDayNames [2] := 'Monday';
  ALDefaultFormatSettingsU.ShortDayNames[3] := 'Tue';
  ALDefaultFormatSettingsU.LongDayNames [3] := 'Tuesday';
  ALDefaultFormatSettingsU.ShortDayNames[4] := 'Wed';
  ALDefaultFormatSettingsU.LongDayNames [4] := 'Wednesday';
  ALDefaultFormatSettingsU.ShortDayNames[5] := 'Thu';
  ALDefaultFormatSettingsU.LongDayNames [5] := 'Thursday';
  ALDefaultFormatSettingsU.ShortDayNames[6] := 'Fri';
  ALDefaultFormatSettingsU.LongDayNames [6] := 'Friday';
  ALDefaultFormatSettingsU.ShortDayNames[7] := 'Sat';
  ALDefaultFormatSettingsU.LongDayNames [7] := 'Saturday';
  ALDefaultFormatSettingsU.ThousandSeparator := ',';
  ALDefaultFormatSettingsU.DecimalSeparator := '.';
  ALDefaultFormatSettingsU.TwoDigitYearCenturyWindow := 50;
  ALDefaultFormatSettingsU.NegCurrFormat := 0;

  ALMove := system.Move;
  {$IFNDEF NEXTGEN}
  ALPosEx := System.AnsiStrings.PosEx;
  AlUpperCase := system.AnsiStrings.UpperCase;
  AlLowerCase := system.AnsiStrings.LowerCase;
  ALCompareStr := system.AnsiStrings.CompareStr;
  ALSameStr := system.AnsiStrings.SameStr;
  ALCompareText := system.AnsiStrings.CompareText;
  ALSameText := system.AnsiStrings.SameText;
  ALMatchText := System.AnsiStrings.MatchText;
  ALMatchStr := System.AnsiStrings.MatchStr;
  {$ENDIF}

  ALDateToStrU := system.sysutils.DateToStr;
  ALTimeToStrU := system.sysutils.TimeToStr;
  ALFormatDateTimeU := system.sysutils.FormatDateTime;
  ALTryStrToDateU := system.sysutils.TryStrToDate;
  ALStrToDateU := system.sysutils.StrToDate;
  ALTryStrToTimeU := system.sysutils.TryStrToTime;
  ALStrToTimeU := system.sysutils.StrToTime;
  ALTryStrToDateTimeU := system.sysutils.TryStrToDateTime;
  ALStrToDateTimeU := system.sysutils.StrToDateTime;
  ALTryStrToIntU := system.sysutils.TryStrToInt;
  ALStrToIntU := system.sysutils.StrToInt;
  ALStrToIntDefU := system.sysutils.StrToIntDef;
  ALTryStrToInt64U := system.sysutils.TryStrToInt64;
  ALStrToInt64U := system.sysutils.StrToInt64;
  ALStrToInt64DefU := system.sysutils.StrToInt64Def;
  {$IF CompilerVersion >= 26}{Delphi XE5}
  ALStrToUInt64U := system.sysutils.StrToUInt64;
  ALStrToUInt64DefU := system.sysutils.StrToUInt64Def;
  ALTryStrToUInt64U := system.sysutils.TryStrToUInt64;
  {$ifend}
  ALCurrToStrU := system.sysutils.CurrToStr;
  ALFormatFloatU := system.sysutils.FormatFloat;
  ALFormatCurrU := system.sysutils.FormatCurr;
  ALStrToFloatU := system.sysutils.StrToFloat;
  ALStrToFloatDefU := system.sysutils.StrToFloatDef;
  ALStrToCurrU := system.sysutils.StrToCurr;
  ALStrToCurrDefU := system.sysutils.StrToCurrDef;
  ALTryStrToCurrU := system.sysutils.TryStrToCurr;
  ALPosU := system.Pos;
  ALPosExU := system.StrUtils.PosEx;
  AlUpperCaseU := system.sysutils.UpperCase;
  AlLowerCaseU := system.sysutils.LowerCase;
  AlUpCaseU := system.UpCase;
  ALCompareStrU := system.sysutils.CompareStr;
  ALSameStrU := system.sysutils.SameStr;
  ALCompareTextU := system.sysutils.CompareText;
  ALSameTextU := system.sysutils.SameText;
  ALTrimU := system.sysutils.Trim;
  ALTrimLeftU := system.sysutils.TrimLeft;
  ALTrimRightU := system.sysutils.TrimRight;
  ALDequotedStrU := system.sysutils.AnsiDequotedStr;
  ALLastDelimiterU := system.sysutils.LastDelimiter;
  ALStringReplaceU := system.sysutils.StringReplace;

end;

{*****************************}
Procedure ALStringFinalization;
begin

  {$IF CompilerVersion >= 31} // berlin
  AlFreeAndNil(_Base64Encoding);
  {$IFEND}

end;

end.
