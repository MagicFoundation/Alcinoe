unit Alcinoe.Localization;

interface

{$I Alcinoe.inc}

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils,
  System.Generics.Collections;

type

  {$IFNDEF ALCompilerVersionSupported123}
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

type

  /// <summary>
  ///   A Locale object represents a specific geographical, political, or
  ///   cultural region. An operation that requires a Locale to perform its task
  ///   is called locale-sensitive and uses the Locale to tailor information for
  ///   the user. For example, displaying a number is a locale-sensitive
  ///   operation— the number should be formatted according to the customs and
  ///   conventions of the user's native country, region, or culture.
  /// </summary>
  TALLocaleW = record
    /// <summary>
    ///   ISO 639 alpha-2 or alpha-3 language code, or registered language subtags
    ///   up to 8 alpha letters (for future enhancements). When a language has both
    ///   an alpha-2 code and an alpha-3 code, the alpha-2 code must be used. You
    ///   can find a full list of valid language codes in the IANA Language Subtag
    ///   Registry (search for "Type: language"). The language field is case
    ///   insensitive, but Locale always canonicalizes to lower case.
    ///   Well-formed language values have the form [a-zA-Z]{2,8}. Note that
    ///   this is not the the full BCP47 language production, since it excludes
    ///   extlang. They are not needed since modern three-letter language codes
    ///   replace them. Example: "en" (English), "ja" (Japanese), "kok" (Konkani)
    /// </summary>
    Language: String;
    /// <summary>
    ///   An internal identifier generated via ALLanguageCodeToID for fast lookups.
    ///   <b>Important:</b> this ID is runtime-only and must never be stored
    ///   in a database or persisted.
    /// </summary>
    LanguageID: Integer;
    /// <summary>
    ///   ISO 15924 alpha-4 script code. You can find a full list of valid script
    ///   codes in the IANA Language Subtag Registry (search for "Type: script").
    ///   The script field is case insensitive, but Locale always canonicalizes
    ///   to title case (the first letter is upper case and the rest of the
    ///   letters are lower case). Well-formed script values have the form [a-zA-Z]{4}
    ///   Example: "Latn" (Latin), "Cyrl" (Cyrillic)
    /// </summary>
    Script: String;
    /// <summary>
    ///   An internal identifier generated via ALScriptCodeToID for fast lookups.
    ///   <b>Important:</b> this ID is runtime-only and must never be stored
    ///   in a database or persisted.
    /// </summary>
    ScriptID: Integer;
    /// <summary>
    ///   ISO 3166 alpha-2 country code or UN M.49 numeric-3 area code. You can
    ///   find a full list of valid country and region codes in the IANA Language
    ///   Subtag Registry (search for "Type: region"). The country (region) field
    ///   is case insensitive, but Locale always canonicalizes to upper case.
    ///   Well-formed country/region values have the form [a-zA-Z]{2} | [0-9]{3}
    ///   Example: "US" (United States), "FR" (France), "029" (Caribbean)
    /// </summary>
    Region: String;
    /// <summary>
    ///   An internal identifier generated via ALRegionCodeToID for fast lookups.
    ///   <b>Important:</b> this ID is runtime-only and must never be stored
    ///   in a database or persisted.
    /// </summary>
    RegionID: Integer;
    /// <summary>
    ///   Any arbitrary value used to indicate a variation of a Locale. Where
    ///   there are two or more variant values each indicating its own semantics,
    ///   these values should be ordered by importance, with most important first.
    ///   The variant field is case insensitive, but Locale always canonicalizes
    ///   to lower case. Note: IETF BCP 47 places syntactic restrictions on
    ///   variant subtags. Also BCP 47 subtags are strictly used to indicate
    ///   additional variations that define a language or its dialects that are
    ///   not covered by any combinations of language, script and region subtags.
    ///   You can find a full list of valid variant codes in the IANA Language
    ///   Subtag Registry (search for "Type: variant"). However, the variant
    ///   field in Locale has historically been used for any kind of variation,
    ///   not just language variations. For example, some supported variants
    ///   available in Java SE Runtime Environments indicate alternative
    ///   cultural behaviors such as calendar type or number script. In BCP 47
    ///   this kind of information, which does not identify the language, is
    ///   supported by extension subtags or private use subtags. Well-formed
    ///   variant values have the form SUBTAG (('_'|'-') SUBTAG)* where
    ///   SUBTAG = [0-9][0-9a-zA-Z]{3} | [0-9a-zA-Z]{5,8}.
    ///   (Note: BCP 47 only uses hyphen ('-') as a delimiter, this is more
    ///   lenient). Example: "polyton" (Polytonic Greek), "POSIX"
    /// </summary>
    Variants: TArray<String>;
    /// <summary>
    ///   A map from single character keys to string values, indicating extensions
    ///   apart from language identification. The extensions in Locale implement
    ///   the semantics and syntax of BCP 47 extension subtags and private use
    ///   subtags. The extensions are case insensitive, but Locale canonicalizes
    ///   all extension keys and values to lower case. Note that extensions cannot
    ///   have empty values. Well-formed keys are single characters from the set
    ///   [0-9a-zA-Z]. Well-formed values have the form SUBTAG ('-' SUBTAG)* where
    ///   for the key 'x' SUBTAG = [0-9a-zA-Z]{1,8} and for other keys SUBTAG = [0-9a-zA-Z]{2,8}
    ///   (that is, 'x' allows single-character subtags).
    ///   Example: key="u"/value="ca-japanese" (Japanese Calendar), key="x"/value="java-1-7"
    /// </summary>
    Extensions: TArray<String>;
    /// <summary>
    ///   A private use subtag that allows for custom or application-specific extensions
    ///   to the language tag. Private use subtags are introduced with the singleton "x"
    ///   and can follow a complete or partial language tag. Their interpretation is not
    ///   standardized and is defined by private agreement between systems. This field is
    ///   case insensitive and canonicalized to lowercase.
    ///   Well-formed values have the form "x"-SUBTAG, where SUBTAG is one or more
    ///   non-empty components of up to 8 alphanumeric characters, separated by hyphens.
    ///   Example: "x-mylocale", "en-US-x-test"
    /// </summary>
    Privateuse: String;
    /// <summary>
    ///   Contains locale-specific formatting information such as decimal separator,
    ///   thousands separator, date and time formats, currency symbol, and more.
    ///   These settings are initialized according to the language, script, and region
    ///   components of the locale, and are used by locale-sensitive operations like
    ///   number, date, and currency formatting.
    /// </summary>
    FormatSettings: TALFormatSettingsW;
    // Creates a TALLocaleW record with current default values provided
    // by the operating system.
    class function Create: TALLocaleW; overload; static; inline;
    /// <summary>
    ///   Returns a locale for the specified IETF BCP 47 language tag string.
    /// </summary>
    class function Create(const ALanguageTag: String): TALLocaleW; overload; static;
  end;

  /// <summary>
  ///   A Locale object represents a specific geographical, political, or
  ///   cultural region. An operation that requires a Locale to perform its task
  ///   is called locale-sensitive and uses the Locale to tailor information for
  ///   the user. For example, displaying a number is a locale-sensitive
  ///   operation— the number should be formatted according to the customs and
  ///   conventions of the user's native country, region, or culture.
  /// </summary>
  TALLocaleA = record
    /// <summary>
    ///   ISO 639 alpha-2 or alpha-3 language code, or registered language subtags
    ///   up to 8 alpha letters (for future enhancements). When a language has both
    ///   an alpha-2 code and an alpha-3 code, the alpha-2 code must be used. You
    ///   can find a full list of valid language codes in the IANA Language Subtag
    ///   Registry (search for "Type: language"). The language field is case
    ///   insensitive, but Locale always canonicalizes to lower case.
    ///   Well-formed language values have the form [a-zA-Z]{2,8}. Note that
    ///   this is not the the full BCP47 language production, since it excludes
    ///   extlang. They are not needed since modern three-letter language codes
    ///   replace them. Example: "en" (English), "ja" (Japanese), "kok" (Konkani)
    /// </summary>
    Language: AnsiString;
    /// <summary>
    ///   An internal identifier generated via ALLanguageCodeToID for fast lookups.
    ///   <b>Important:</b> this ID is runtime-only and must never be stored
    ///   in a database or persisted.
    /// </summary>
    LanguageID: Integer;
    /// <summary>
    ///   ISO 15924 alpha-4 script code. You can find a full list of valid script
    ///   codes in the IANA Language Subtag Registry (search for "Type: script").
    ///   The script field is case insensitive, but Locale always canonicalizes
    ///   to title case (the first letter is upper case and the rest of the
    ///   letters are lower case). Well-formed script values have the form [a-zA-Z]{4}
    ///   Example: "Latn" (Latin), "Cyrl" (Cyrillic)
    /// </summary>
    Script: AnsiString;
    /// <summary>
    ///   An internal identifier generated via ALScriptCodeToID for fast lookups.
    ///   <b>Important:</b> this ID is runtime-only and must never be stored
    ///   in a database or persisted.
    /// </summary>
    ScriptID: Integer;
    /// <summary>
    ///   ISO 3166 alpha-2 country code or UN M.49 numeric-3 area code. You can
    ///   find a full list of valid country and region codes in the IANA Language
    ///   Subtag Registry (search for "Type: region"). The country (region) field
    ///   is case insensitive, but Locale always canonicalizes to upper case.
    ///   Well-formed country/region values have the form [a-zA-Z]{2} | [0-9]{3}
    ///   Example: "US" (United States), "FR" (France), "029" (Caribbean)
    /// </summary>
    Region: AnsiString;
    /// <summary>
    ///   An internal identifier generated via ALRegionCodeToID for fast lookups.
    ///   <b>Important:</b> this ID is runtime-only and must never be stored
    ///   in a database or persisted.
    /// </summary>
    RegionID: Integer;
    /// <summary>
    ///   Any arbitrary value used to indicate a variation of a Locale. Where
    ///   there are two or more variant values each indicating its own semantics,
    ///   these values should be ordered by importance, with most important first.
    ///   The variant field is case insensitive, but Locale always canonicalizes
    ///   to lower case. Note: IETF BCP 47 places syntactic restrictions on
    ///   variant subtags. Also BCP 47 subtags are strictly used to indicate
    ///   additional variations that define a language or its dialects that are
    ///   not covered by any combinations of language, script and region subtags.
    ///   You can find a full list of valid variant codes in the IANA Language
    ///   Subtag Registry (search for "Type: variant"). However, the variant
    ///   field in Locale has historically been used for any kind of variation,
    ///   not just language variations. For example, some supported variants
    ///   available in Java SE Runtime Environments indicate alternative
    ///   cultural behaviors such as calendar type or number script. In BCP 47
    ///   this kind of information, which does not identify the language, is
    ///   supported by extension subtags or private use subtags. Well-formed
    ///   variant values have the form SUBTAG (('_'|'-') SUBTAG)* where
    ///   SUBTAG = [0-9][0-9a-zA-Z]{3} | [0-9a-zA-Z]{5,8}.
    ///   (Note: BCP 47 only uses hyphen ('-') as a delimiter, this is more
    ///   lenient). Example: "polyton" (Polytonic Greek), "POSIX"
    /// </summary>
    Variants: TArray<AnsiString>;
    /// <summary>
    ///   A map from single character keys to string values, indicating extensions
    ///   apart from language identification. The extensions in Locale implement
    ///   the semantics and syntax of BCP 47 extension subtags and private use
    ///   subtags. The extensions are case insensitive, but Locale canonicalizes
    ///   all extension keys and values to lower case. Note that extensions cannot
    ///   have empty values. Well-formed keys are single characters from the set
    ///   [0-9a-zA-Z]. Well-formed values have the form SUBTAG ('-' SUBTAG)* where
    ///   for the key 'x' SUBTAG = [0-9a-zA-Z]{1,8} and for other keys SUBTAG = [0-9a-zA-Z]{2,8}
    ///   (that is, 'x' allows single-character subtags).
    ///   Example: key="u"/value="ca-japanese" (Japanese Calendar), key="x"/value="java-1-7"
    /// </summary>
    Extensions: TArray<AnsiString>;
    /// <summary>
    ///   A private use subtag that allows for custom or application-specific extensions
    ///   to the language tag. Private use subtags are introduced with the singleton "x"
    ///   and can follow a complete or partial language tag. Their interpretation is not
    ///   standardized and is defined by private agreement between systems. This field is
    ///   case insensitive and canonicalized to lowercase.
    ///   Well-formed values have the form "x"-SUBTAG, where SUBTAG is one or more
    ///   non-empty components of up to 8 alphanumeric characters, separated by hyphens.
    ///   Example: "x-mylocale", "en-US-x-test"
    /// </summary>
    Privateuse: AnsiString;
    /// <summary>
    ///   Contains locale-specific formatting information such as decimal separator,
    ///   thousands separator, date and time formats, currency symbol, and more.
    ///   These settings are initialized according to the language, script, and region
    ///   components of the locale, and are used by locale-sensitive operations like
    ///   number, date, and currency formatting.
    /// </summary>
    FormatSettings: TALFormatSettingsA;
    // Creates a TALLocaleW record with current default values provided
    // by the operating system.
    class function Create: TALLocaleA; overload; static; inline;
    /// <summary>
    ///   Returns a locale for the specified IETF BCP 47 language tag string.
    /// </summary>
    class function Create(const ALanguageTag: AnsiString): TALLocaleA; overload; static;
  end;

  {*****************************}
  TALPluralRules = Class(TObject)
  private
    class function CreateInstance: TALPluralRules;
    class function GetInstance: TALPluralRules; static;
  protected
    class var FInstance: TALPluralRules;
  public
    type
      TCreateInstanceFunc = function: TALPluralRules;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALPluralRules read GetInstance;
    class function HasInstance: Boolean; inline;
  private
  public
    type
      TPluralCategory = (Zero, One, Two, Few, Many, Other);
      TPluralType = (Cardinal, Ordinal);
  protected
    Type
      TGetPluralCategoryFunc = function (const n, i: Int64; v, f, t, e: Int32): TPluralCategory of object;
  protected
    FCardinalPluralCategoryFuncByLanguages: TDictionary<integer, TGetPluralCategoryFunc>;
    FOrdinalPluralCategoryFuncByLanguages: TDictionary<integer, TGetPluralCategoryFunc>;
    function select(
               const AGetPluralCategoryFunc: TGetPluralCategoryFunc;
               const ANumber: Double;
               const AType: TPluralType = TPluralType.Cardinal;
               const AMinFractionDigits: Integer = 0;
               const AExponent: Integer = 0): TPluralCategory; overload;
    function select(
               const AGetPluralCategoryFunc: TGetPluralCategoryFunc;
               const ANumber: Int64;
               const AType: TPluralType = TPluralType.Cardinal;
               const AExponent: Integer = 0): TPluralCategory; overload;
  protected

    {$REGION 'Auto-generated by <ALCINOE>\Tools\CodeBuilder (1)'}

    function GetCardinalPluralCategory_AF(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_AK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_AM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_AN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_AR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_ARS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_AS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_ASA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_AST(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_AZ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_BAL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_BE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_BEM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_BEZ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_BG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_BHO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_BLO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_BM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_BN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_BO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_BR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_BRX(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_BS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_CA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_CE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_CEB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_CGG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_CHR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_CKB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_CS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_CSW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_CY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_DA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_DE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_DOI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_DSB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_DV(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_DZ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_EE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_EL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_EN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_EO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_ES(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_ET(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_EU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_FA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_FF(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_FI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_FIL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_FO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_FR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_FUR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_FY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_GA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_GD(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_GL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_GSW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_GU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_GUW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_GV(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_HA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_HAW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_HE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_HI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_HNJ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_HR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_HSB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_HU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_HY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_IA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_ID(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_IG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_II(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_IO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_IS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_IT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_IU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_JA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_JBO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_JGO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_JMC(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_JV(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_JW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KAB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KAJ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KCG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KDE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KEA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KKJ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KSB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KSH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_KY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_LAG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_LB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_LG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_LIJ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_LKT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_LLD(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_LN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_LO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_LT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_LV(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_MAS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_MG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_MGO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_MK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_ML(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_MN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_MO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_MR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_MS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_MT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_MY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_NAH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_NAQ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_NB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_ND(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_NE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_NL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_NN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_NNH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_NO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_NQO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_NR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_NSO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_NY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_NYN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_OM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_OR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_OS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_OSA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_PA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_PAP(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_PCM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_PL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_PRG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_PS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_PT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_PT_PT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_RM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_RO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_ROF(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_RU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_RWK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SAH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SAQ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SAT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SC(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SCN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SD(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SDH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SEH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SES(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SHI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SMA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SMI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SMJ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SMN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SMS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SQ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SSY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_ST(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SV(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_SYR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_TA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_TE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_TEO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_TH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_TI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_TIG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_TK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_TL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_TN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_TO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_TPI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_TR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_TS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_TZM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_UG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_UK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_UND(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_UR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_UZ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_VE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_VEC(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_VI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_VO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_VUN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_WA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_WAE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_WO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_XH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_XOG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_YI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_YO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_YUE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_ZH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetCardinalPluralCategory_ZU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_AF(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_AM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_AN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_AR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_AS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_AST(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_AZ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_BAL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_BE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_BG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_BLO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_BN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_BS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_CA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_CE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_CS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_CY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_DA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_DE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_DSB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_EL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_EN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_ES(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_ET(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_EU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_FA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_FI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_FIL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_FR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_FY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_GA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_GD(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_GL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_GSW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_GU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_HE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_HI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_HR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_HSB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_HU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_HY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_IA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_ID(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_IS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_IT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_JA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_KA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_KK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_KM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_KN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_KO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_KW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_KY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_LIJ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_LLD(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_LO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_LT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_LV(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_MK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_ML(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_MN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_MO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_MR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_MS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_MY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_NB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_NE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_NL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_NO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_OR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_PA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_PL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_PRG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_PS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_PT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_RO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_RU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_SC(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_SCN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_SD(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_SH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_SI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_SK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_SL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_SQ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_SR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_SV(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_SW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_TA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_TE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_TH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_TK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_TL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_TPI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_TR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_UK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_UND(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_UR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_UZ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_VEC(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_VI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_YUE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_ZH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
    function GetOrdinalPluralCategory_ZU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;

    {$ENDREGION 'Auto-generated by <ALCINOE>\Tools\CodeBuilder (1)'}

  public
    Constructor Create; virtual;
    Destructor Destroy; override;
    /// <summary>
    ///   Determines the appropriate plural category for a given number and locale,
    ///   based on the Unicode CLDR plural rules.
    /// </summary>
    /// <param name="ALocale">
    ///   The locale used to determine the pluralization rules. This includes language
    ///   and region represented by a <c>TALLocaleW</c> record.
    /// </param>
    /// <param name="ANumber">
    ///   The numeric value to evaluate. This may be an integer or a decimal value.
    /// </param>
    /// <param name="AType">
    ///   Specifies whether to apply <c>Cardinal</c> rules (used for quantities like "1 file")
    ///   or <c>Ordinal</c> rules (used for ranks like "1st", "2nd"). Defaults to <c>Cardinal</c>.
    /// </param>
    /// <param name="AMinFractionDigits">
    ///   The minimum number of visible fraction digits to consider. This ensures that
    ///   numbers like 1, 1.0, and 1.00 can be treated differently where required by the
    ///   locale's plural rules. Defaults to <c>0</c>.
    /// </param>
    /// <param name="AExponent">
    ///   The exponent used in scientific notation (e.g., <c>1.2e5</c> has an exponent of 5).
    ///   This is typically <c>0</c> unless dealing with compact or scientific formats.
    /// </param>
    /// <returns>
    ///   A <c>TPluralCategory</c> value representing the plural form for the given input.
    ///   This may be one of the following categories:
    ///   <list type="bullet">
    ///     <item><c>Zero</c></item>
    ///     <item><c>One</c></item>
    ///     <item><c>Two</c></item>
    ///     <item><c>Few</c></item>
    ///     <item><c>Many</c></item>
    ///     <item><c>Other</c></item>
    ///   </list>
    /// </returns>
    /// <remarks>
    ///   This function implements locale-sensitive plural rule selection using data
    ///   derived from the Unicode CLDR. It supports both integer and decimal inputs.
    /// </remarks>
    function select(
               const Alocale: TALLocaleW;
               const ANumber: Double;
               const AType: TPluralType = TPluralType.Cardinal;
               const AMinFractionDigits: Integer = 0;
               const AExponent: Integer = 0): TPluralCategory; overload;
    function select(
               const Alocale: TALLocaleW;
               const ANumber: Int64;
               const AType: TPluralType = TPluralType.Cardinal;
               const AExponent: Integer = 0): TPluralCategory; overload;
    function select(
               const Alocale: TALLocaleA;
               const ANumber: Double;
               const AType: TPluralType = TPluralType.Cardinal;
               const AMinFractionDigits: Integer = 0;
               const AExponent: Integer = 0): TPluralCategory; overload;
    function select(
               const Alocale: TALLocaleA;
               const ANumber: Int64;
               const AType: TPluralType = TPluralType.Cardinal;
               const AExponent: Integer = 0): TPluralCategory; overload;
    class function PluralCategoryToStringW(const ACategory: TPluralCategory): String;
    class function PluralCategoryToStringA(const ACategory: TPluralCategory): AnsiString;
  end;

function ALGetPreferredLanguages: TArray<String>;
function ALLanguageCodeToID(const ALanguageCode: string): Integer; overload;
function ALLanguageCodeToID(const ALanguageCode: Ansistring): Integer; overload;
function ALScriptCodeToID(const AScriptCode: string): Integer; overload;
function ALScriptCodeToID(const AScriptCode: Ansistring): Integer; overload;
function ALRegionCodeToID(const ARegionCode: string): Integer; overload;
function ALRegionCodeToID(const ARegionCode: Ansistring): Integer; overload;

const
  ALLanguageID_Afar = 100;             ALLanguageID_AA = 100;   // Afar
  ALLanguageID_Abkhazian = 101;        ALLanguageID_AB = 101;   // Abkhazian
  ALLanguageID_Afrikaans = 105;        ALLanguageID_AF = 105;   // Afrikaans
  ALLanguageID_Akan = 110;             ALLanguageID_AK = 110;   // Akan
  ALLanguageID_Albanian = 584;         ALLanguageID_SQ = 584;   // Albanian
  ALLanguageID_Amharic = 112;          ALLanguageID_AM = 112;   // Amharic
  ALLanguageID_Arabic = 117;           ALLanguageID_AR = 117;   // Arabic
  ALLanguageID_Aragonese = 113;        ALLanguageID_AN = 113;   // Aragonese
  ALLanguageID_Armenian = 306;         ALLanguageID_HY = 306;   // Armenian
  ALLanguageID_Assamese = 118;         ALLanguageID_AS = 118;   // Assamese
  ALLanguageID_Avaric = 121;           ALLanguageID_AV = 121;   // Avaric
  ALLanguageID_Avestan = 104;          ALLanguageID_AE = 104;   // Avestan
  ALLanguageID_Aymara = 124;           ALLanguageID_AY = 124;   // Aymara
  ALLanguageID_Azerbaijani = 125;      ALLanguageID_AZ = 125;   // Azerbaijani
  ALLanguageID_Bashkir = 126;          ALLanguageID_BA = 126;   // Bashkir
  ALLanguageID_Bambara = 138;          ALLanguageID_BM = 138;   // Bambara
  ALLanguageID_Basque = 224;           ALLanguageID_EU = 224;   // Basque
  ALLanguageID_Belarusian = 130;       ALLanguageID_BE = 130;   // Belarusian
  ALLanguageID_Bengali = 139;          ALLanguageID_BN = 139;   // Bengali
  ALLanguageID_Bislama = 134;          ALLanguageID_BI = 134;   // Bislama
  ALLanguageID_Bosnian = 144;          ALLanguageID_BS = 144;   // Bosnian
  ALLanguageID_Breton = 143;           ALLanguageID_BR = 143;   // Breton
  ALLanguageID_Bulgarian = 132;        ALLanguageID_BG = 132;   // Bulgarian
  ALLanguageID_Burmese = 436;          ALLanguageID_MY = 436;   // Burmese
  ALLanguageID_Catalan = 152;          ALLanguageID_CA = 152;   // Catalan; Valencian
  ALLanguageID_Chamorro = 159;         ALLanguageID_CH = 159;   // Chamorro
  ALLanguageID_Chechen = 156;          ALLanguageID_CE = 156;   // Chechen
  ALLanguageID_Chinese = 757;          ALLanguageID_ZH = 757;   // Chinese
  ALLanguageID_ChurchSlavic = 172;     ALLanguageID_CU = 172;   // Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic
  ALLanguageID_Chuvash = 173;          ALLanguageID_CV = 173;   // Chuvash
  ALLanguageID_Cornish = 382;          ALLanguageID_KW = 382;   // Cornish
  ALLanguageID_Corsican = 166;         ALLanguageID_CO = 166;   // Corsican
  ALLanguageID_Cree = 169;             ALLanguageID_CR = 169;   // Cree
  ALLanguageID_Czech = 170;            ALLanguageID_CS = 170;   // Czech
  ALLanguageID_Danish = 178;           ALLanguageID_DA = 178;   // Danish
  ALLanguageID_Divehi = 199;           ALLanguageID_DV = 199;   // Divehi; Dhivehi; Maldivian
  ALLanguageID_Dutch = 449;            ALLanguageID_NL = 449;   // Dutch; Flemish
  ALLanguageID_Dzongkha = 203;         ALLanguageID_DZ = 203;   // Dzongkha
  ALLanguageID_English = 217;          ALLanguageID_EN = 217;   // English
  ALLanguageID_Esperanto = 218;        ALLanguageID_EO = 218;   // Esperanto
  ALLanguageID_Estonian = 223;         ALLanguageID_ET = 223;   // Estonian
  ALLanguageID_Ewe = 208;              ALLanguageID_EE = 208;   // Ewe
  ALLanguageID_Faroese = 244;          ALLanguageID_FO = 244;   // Faroese
  ALLanguageID_Fijian = 239;           ALLanguageID_FJ = 239;   // Fijian
  ALLanguageID_Finnish = 238;          ALLanguageID_FI = 238;   // Finnish
  ALLanguageID_French = 247;           ALLanguageID_FR = 247;   // French
  ALLanguageID_WesternFrisian = 254;   ALLanguageID_FY = 254;   // Western Frisian
  ALLanguageID_Fulah = 235;            ALLanguageID_FF = 235;   // Fulah
  ALLanguageID_Georgian = 360;         ALLanguageID_KA = 360;   // Georgian
  ALLanguageID_German = 182;           ALLanguageID_DE = 182;   // German
  ALLanguageID_Gaelic = 259;           ALLanguageID_GD = 259;   // Gaelic; Scottish Gaelic
  ALLanguageID_Irish = 256;            ALLanguageID_GA = 256;   // Irish
  ALLanguageID_Galician = 267;         ALLanguageID_GL = 267;   // Galician
  ALLanguageID_Manx = 277;             ALLanguageID_GV = 277;   // Manx
  ALLanguageID_Greek = 215;            ALLanguageID_EL = 215;   // Greek, Modern (1453-)
  ALLanguageID_Guarani = 269;          ALLanguageID_GN = 269;   // Guarani
  ALLanguageID_Gujarati = 276;         ALLanguageID_GU = 276;   // Gujarati
  ALLanguageID_Haitian = 301;          ALLanguageID_HT = 301;   // Haitian; Haitian Creole
  ALLanguageID_Hausa = 282;            ALLanguageID_HA = 282;   // Hausa
  ALLanguageID_Hebrew = 286;           ALLanguageID_HE = 286;   // Hebrew
  ALLanguageID_Herero = 307;           ALLanguageID_HZ = 307;   // Herero
  ALLanguageID_Hindi = 290;            ALLanguageID_HI = 290;   // Hindi
  ALLanguageID_HiriMotu = 296;         ALLanguageID_HO = 296;   // Hiri Motu
  ALLanguageID_Croatian = 299;         ALLanguageID_HR = 299;   // Croatian
  ALLanguageID_Hungarian = 302;        ALLanguageID_HU = 302;   // Hungarian
  ALLanguageID_Igbo = 314;             ALLanguageID_IG = 314;   // Igbo
  ALLanguageID_Icelandic = 326;        ALLanguageID_IS = 326;   // Icelandic
  ALLanguageID_Ido = 322;              ALLanguageID_IO = 322;   // Ido
  ALLanguageID_SichuanYi = 316;        ALLanguageID_II = 316;   // Sichuan Yi; Nuosu
  ALLanguageID_Inuktitut = 328;        ALLanguageID_IU = 328;   // Inuktitut
  ALLanguageID_Interlingue = 312;      ALLanguageID_IE = 312;   // Interlingue; Occidental
  ALLanguageID_Interlingua = 308;      ALLanguageID_IA = 308;   // Interlingua (International Auxiliary Language Association)
  ALLanguageID_Indonesian = 311;       ALLanguageID_ID = 311;   // Indonesian
  ALLanguageID_Inupiaq = 318;          ALLanguageID_IK = 318;   // Inupiaq
  ALLanguageID_Italian = 327;          ALLanguageID_IT = 327;   // Italian
  ALLanguageID_Javanese = 355;         ALLanguageID_JV = 355;   // Javanese
  ALLanguageID_Japanese = 334;         ALLanguageID_JA = 334;   // Japanese
  ALLanguageID_Kalaallisut = 371;      ALLanguageID_KL = 371;   // Kalaallisut; Greenlandic
  ALLanguageID_Kannada = 373;          ALLanguageID_KN = 373;   // Kannada
  ALLanguageID_Kashmiri = 378;         ALLanguageID_KS = 378;   // Kashmiri
  ALLanguageID_Kanuri = 377;           ALLanguageID_KR = 377;   // Kanuri
  ALLanguageID_Kazakh = 370;           ALLanguageID_KK = 370;   // Kazakh
  ALLanguageID_CentralKhmer = 372;     ALLanguageID_KM = 372;   // Central Khmer
  ALLanguageID_Kikuyu = 368;           ALLanguageID_KI = 368;   // Kikuyu; Gikuyu
  ALLanguageID_Kinyarwanda = 564;      ALLanguageID_RW = 564;   // Kinyarwanda
  ALLanguageID_Kirghiz = 384;          ALLanguageID_KY = 384;   // Kirghiz; Kyrgyz
  ALLanguageID_Komi = 381;             ALLanguageID_KV = 381;   // Komi
  ALLanguageID_Kongo = 366;            ALLanguageID_KG = 366;   // Kongo
  ALLanguageID_Korean = 374;           ALLanguageID_KO = 374;   // Korean
  ALLanguageID_Kuanyama = 369;         ALLanguageID_KJ = 369;   // Kuanyama; Kwanyama
  ALLanguageID_Kurdish = 380;          ALLanguageID_KU = 380;   // Kurdish
  ALLanguageID_Lao = 400;              ALLanguageID_LO = 400;   // Lao
  ALLanguageID_Latin = 386;            ALLanguageID_LA = 386;   // Latin
  ALLanguageID_Latvian = 407;          ALLanguageID_LV = 407;   // Latvian
  ALLanguageID_Limburgan = 394;        ALLanguageID_LI = 394;   // Limburgan; Limburger; Limburgish
  ALLanguageID_Lingala = 399;          ALLanguageID_LN = 399;   // Lingala
  ALLanguageID_Lithuanian = 405;       ALLanguageID_LT = 405;   // Lithuanian
  ALLanguageID_Luxembourgish = 387;    ALLanguageID_LB = 387;   // Luxembourgish; Letzeburgesch
  ALLanguageID_LubaKatanga = 406;      ALLanguageID_LU = 406;   // Luba-Katanga
  ALLanguageID_Ganda = 392;            ALLanguageID_LG = 392;   // Ganda
  ALLanguageID_Macedonian = 422;       ALLanguageID_MK = 422;   // Macedonian
  ALLanguageID_Marshallese = 419;      ALLanguageID_MH = 419;   // Marshallese
  ALLanguageID_Malayalam = 423;        ALLanguageID_ML = 423;   // Malayalam
  ALLanguageID_Maori = 420;            ALLanguageID_MI = 420;   // Maori
  ALLanguageID_Marathi = 429;          ALLanguageID_MR = 429;   // Marathi
  ALLanguageID_Malay = 430;            ALLanguageID_MS = 430;   // Malay
  ALLanguageID_Malagasy = 418;         ALLanguageID_MG = 418;   // Malagasy
  ALLanguageID_Maltese = 431;          ALLanguageID_MT = 431;   // Maltese
  ALLanguageID_Mongolian = 425;        ALLanguageID_MN = 425;   // Mongolian
  ALLanguageID_Nauru = 438;            ALLanguageID_NA = 438;   // Nauru
  ALLanguageID_Navajo = 459;           ALLanguageID_NV = 459;   // Navajo; Navaho
  ALLanguageID_SouthNdebele = 455;     ALLanguageID_NR = 455;   // Ndebele, South; South Ndebele
  ALLanguageID_NorthNdebele = 441;     ALLanguageID_ND = 441;   // Ndebele, North; North Ndebele
  ALLanguageID_Ndonga = 444;           ALLanguageID_NG = 444;   // Ndonga
  ALLanguageID_Nepali = 442;           ALLanguageID_NE = 442;   // Nepali
  ALLanguageID_NorwegianNynorsk = 451; ALLanguageID_NN = 451;   // Norwegian Nynorsk; Nynorsk, Norwegian
  ALLanguageID_NorwegianBokmal = 439;  ALLanguageID_NB = 439;   // Bokmål, Norwegian; Norwegian Bokmål
  ALLanguageID_Norwegian = 452;        ALLanguageID_NO = 452;   // Norwegian
  ALLanguageID_Chichewa = 462;         ALLanguageID_NY = 462;   // Chichewa; Chewa; Nyanja
  ALLanguageID_Occitan = 466;          ALLanguageID_OC = 466;   // Occitan (post 1500)
  ALLanguageID_Ojibwa = 473;           ALLanguageID_OJ = 473;   // Ojibwa
  ALLanguageID_Oriya = 481;            ALLanguageID_OR = 481;   // Oriya
  ALLanguageID_Oromo = 476;            ALLanguageID_OM = 476;   // Oromo
  ALLanguageID_Ossetian = 482;         ALLanguageID_OS = 482;   // Ossetian; Ossetic
  ALLanguageID_Panjabi = 490;          ALLanguageID_PA = 490;   // Panjabi; Punjabi
  ALLanguageID_Persian = 230;          ALLanguageID_FA = 230;   // Persian
  ALLanguageID_Pali = 498;             ALLanguageID_PI = 498;   // Pali
  ALLanguageID_Polish = 501;           ALLanguageID_PL = 501;   // Polish
  ALLanguageID_Portuguese = 509;       ALLanguageID_PT = 509;   // Portuguese
  ALLanguageID_Pushto = 508;           ALLanguageID_PS = 508;   // Pushto; Pashto
  ALLanguageID_Quechua = 536;          ALLanguageID_QU = 536;   // Quechua
  ALLanguageID_Romansh = 554;          ALLanguageID_RM = 554;   // Romansh
  ALLanguageID_Romanian = 556;         ALLanguageID_RO = 556;   // Romanian; Moldavian; Moldovan
  ALLanguageID_Rundi = 555;            ALLanguageID_RN = 555;   // Rundi
  ALLanguageID_Russian = 562;          ALLanguageID_RU = 562;   // Russian
  ALLanguageID_Sango = 574;            ALLanguageID_SG = 574;   // Sango
  ALLanguageID_Sanskrit = 568;         ALLanguageID_SA = 568;   // Sanskrit
  ALLanguageID_Sinhala = 576;          ALLanguageID_SI = 576;   // Sinhala; Sinhalese
  ALLanguageID_Slovak = 578;           ALLanguageID_SK = 578;   // Slovak
  ALLanguageID_Slovenian = 579;        ALLanguageID_SL = 579;   // Slovenian
  ALLanguageID_NorthernSami = 572;     ALLanguageID_SE = 572;   // Northern Sami
  ALLanguageID_Samoan = 580;           ALLanguageID_SM = 580;   // Samoan
  ALLanguageID_Shona = 581;            ALLanguageID_SN = 581;   // Shona
  ALLanguageID_Sindhi = 571;           ALLanguageID_SD = 571;   // Sindhi
  ALLanguageID_Somali = 582;           ALLanguageID_SO = 582;   // Somali
  ALLanguageID_SouthernSotho = 587;    ALLanguageID_ST = 587;   // Sotho, Southern
  ALLanguageID_Spanish = 222;          ALLanguageID_ES = 222;   // Spanish; Castilian
  ALLanguageID_Sardinian = 570;        ALLanguageID_SC = 570;   // Sardinian
  ALLanguageID_Serbian = 585;          ALLanguageID_SR = 585;   // Serbian
  ALLanguageID_Swati = 586;            ALLanguageID_SS = 586;   // Swati
  ALLanguageID_Sundanese = 588;        ALLanguageID_SU = 588;   // Sundanese
  ALLanguageID_Swahili = 590;          ALLanguageID_SW = 590;   // Swahili
  ALLanguageID_Swedish = 589;          ALLanguageID_SV = 589;   // Swedish
  ALLanguageID_Tahitian = 618;         ALLanguageID_TY = 618;   // Tahitian
  ALLanguageID_Tamil = 594;            ALLanguageID_TA = 594;   // Tamil
  ALLanguageID_Tatar = 613;            ALLanguageID_TT = 613;   // Tatar
  ALLanguageID_Telugu = 598;           ALLanguageID_TE = 598;   // Telugu
  ALLanguageID_Tajik = 600;            ALLanguageID_TG = 600;   // Tajik
  ALLanguageID_Tagalog = 605;          ALLanguageID_TL = 605;   // Tagalog
  ALLanguageID_Thai = 601;             ALLanguageID_TH = 601;   // Thai
  ALLanguageID_Tibetan = 140;          ALLanguageID_BO = 140;   // Tibetan
  ALLanguageID_Tigrinya = 602;         ALLanguageID_TI = 602;   // Tigrinya
  ALLanguageID_Tonga = 608;            ALLanguageID_TO = 608;   // Tonga (Tonga Islands)
  ALLanguageID_Tswana = 607;           ALLanguageID_TN = 607;   // Tswana
  ALLanguageID_Tsonga = 612;           ALLanguageID_TS = 612;   // Tsonga
  ALLanguageID_Turkmen = 604;          ALLanguageID_TK = 604;   // Turkmen
  ALLanguageID_Turkish = 611;          ALLanguageID_TR = 611;   // Turkish
  ALLanguageID_Twi = 616;              ALLanguageID_TW = 616;   // Twi
  ALLanguageID_Uighur = 626;           ALLanguageID_UG = 626;   // Uighur; Uyghur
  ALLanguageID_Ukrainian = 630;        ALLanguageID_UK = 630;   // Ukrainian
  ALLanguageID_Urdu = 637;             ALLanguageID_UR = 637;   // Urdu
  ALLanguageID_Uzbek = 645;            ALLanguageID_UZ = 645;   // Uzbek
  ALLanguageID_Venda = 650;            ALLanguageID_VE = 650;   // Venda
  ALLanguageID_Vietnamese = 654;       ALLanguageID_VI = 654;   // Vietnamese
  ALLanguageID_Volapuk = 660;          ALLanguageID_VO = 660;   // Volapük
  ALLanguageID_Welsh = 176;            ALLanguageID_CY = 176;   // Welsh
  ALLanguageID_Walloon = 672;          ALLanguageID_WA = 672;   // Walloon
  ALLanguageID_Wolof = 686;            ALLanguageID_WO = 686;   // Wolof
  ALLanguageID_Xhosa = 705;            ALLanguageID_XH = 705;   // Xhosa
  ALLanguageID_Yiddish = 732;          ALLanguageID_YI = 732;   // Yiddish
  ALLanguageID_Yoruba = 738;           ALLanguageID_YO = 738;   // Yoruba
  ALLanguageID_Zhuang = 750;           ALLanguageID_ZA = 750;   // Zhuang; Chuang
  ALLanguageID_Zulu = 770;             ALLanguageID_ZU = 770;   // Zulu

  ALCountryID_Andorra = 103;                     ALCountryID_AD = 103;   // Andorra
  ALCountryID_UnitedArabEmirates = 104;          ALCountryID_AE = 104;   // United Arab Emirates
  ALCountryID_Afghanistan = 105;                 ALCountryID_AF = 105;   // Afghanistan
  ALCountryID_AntiguaAndBarbuda = 106;           ALCountryID_AG = 106;   // Antigua and Barbuda
  ALCountryID_Anguilla = 108;                    ALCountryID_AI = 108;   // Anguilla
  ALCountryID_Albania = 111;                     ALCountryID_AL = 111;   // Albania
  ALCountryID_Armenia = 112;                     ALCountryID_AM = 112;   // Armenia
  ALCountryID_Angola = 114;                      ALCountryID_AO = 114;   // Angola
  ALCountryID_Antarctica = 116;                  ALCountryID_AQ = 116;   // Antarctica
  ALCountryID_Argentina = 117;                   ALCountryID_AR = 117;   // Argentina
  ALCountryID_AmericanSamoa = 118;               ALCountryID_AS = 118;   // American Samoa
  ALCountryID_Austria = 119;                     ALCountryID_AT = 119;   // Austria
  ALCountryID_Australia = 120;                   ALCountryID_AU = 120;   // Australia
  ALCountryID_Aruba = 122;                       ALCountryID_AW = 122;   // Aruba
  ALCountryID_AlandIslands = 123;                ALCountryID_AX = 123;   // Åland Islands
  ALCountryID_Azerbaijan = 125;                  ALCountryID_AZ = 125;   // Azerbaijan
  ALCountryID_BosniaAndHerzegovina = 126;        ALCountryID_BA = 126;   // Bosnia and Herzegovina
  ALCountryID_Barbados = 127;                    ALCountryID_BB = 127;   // Barbados
  ALCountryID_Bangladesh = 129;                  ALCountryID_BD = 129;   // Bangladesh
  ALCountryID_Belgium = 130;                     ALCountryID_BE = 130;   // Belgium
  ALCountryID_BurkinaFaso = 131;                 ALCountryID_BF = 131;   // Burkina Faso
  ALCountryID_Bulgaria = 132;                    ALCountryID_BG = 132;   // Bulgaria
  ALCountryID_Bahrain = 133;                     ALCountryID_BH = 133;   // Bahrain
  ALCountryID_Burundi = 134;                     ALCountryID_BI = 134;   // Burundi
  ALCountryID_Benin = 135;                       ALCountryID_BJ = 135;   // Benin
  ALCountryID_SaintBarthelemy = 137;             ALCountryID_BL = 137;   // Saint Barthélemy
  ALCountryID_Bermuda = 138;                     ALCountryID_BM = 138;   // Bermuda
  ALCountryID_BruneiDarussalam = 139;            ALCountryID_BN = 139;   // Brunei Darussalam
  ALCountryID_Bolivia = 140;                     ALCountryID_BO = 140;   // Bolivia, Plurinational State of
  ALCountryID_CaribbeanNetherlands = 142;        ALCountryID_BQ = 142;   // Bonaire, Sint Eustatius and Saba
  ALCountryID_Brazil = 143;                      ALCountryID_BR = 143;   // Brazil
  ALCountryID_Bahamas = 144;                     ALCountryID_BS = 144;   // Bahamas
  ALCountryID_Bhutan = 145;                      ALCountryID_BT = 145;   // Bhutan
  ALCountryID_BouvetIsland = 147;                ALCountryID_BV = 147;   // Bouvet Island
  ALCountryID_Botswana = 148;                    ALCountryID_BW = 148;   // Botswana
  ALCountryID_Belarus = 150;                     ALCountryID_BY = 150;   // Belarus
  ALCountryID_Belize = 151;                      ALCountryID_BZ = 151;   // Belize
  ALCountryID_Canada = 152;                      ALCountryID_CA = 152;   // Canada
  ALCountryID_CocosIslands = 154;                ALCountryID_CC = 154;   // Cocos (Keeling) Islands
  ALCountryID_DemocraticRepublicOfCongo = 155;   ALCountryID_CD = 155;   // Congo, Democratic Republic of the
  ALCountryID_CentralAfricanRepublic = 157;      ALCountryID_CF = 157;   // Central African Republic
  ALCountryID_Congo = 158;                       ALCountryID_CG = 158;   // Congo
  ALCountryID_Switzerland = 159;                 ALCountryID_CH = 159;   // Switzerland
  ALCountryID_CotedIvoire = 160;                 ALCountryID_CI = 160;   // Côte d'Ivoire
  ALCountryID_CookIslands = 162;                 ALCountryID_CK = 162;   // Cook Islands
  ALCountryID_Chile = 163;                       ALCountryID_CL = 163;   // Chile
  ALCountryID_Cameroon = 164;                    ALCountryID_CM = 164;   // Cameroon
  ALCountryID_China = 165;                       ALCountryID_CN = 165;   // China
  ALCountryID_Colombia = 166;                    ALCountryID_CO = 166;   // Colombia
  ALCountryID_CostaRica = 169;                   ALCountryID_CR = 169;   // Costa Rica
  ALCountryID_Cuba = 172;                        ALCountryID_CU = 172;   // Cuba
  ALCountryID_CaboVerde = 173;                   ALCountryID_CV = 173;   // Cabo Verde
  ALCountryID_Curacao = 174;                     ALCountryID_CW = 174;   // Curaçao
  ALCountryID_ChristmasIsland = 175;             ALCountryID_CX = 175;   // Christmas Island
  ALCountryID_Cyprus = 176;                      ALCountryID_CY = 176;   // Cyprus
  ALCountryID_Czechia = 177;                     ALCountryID_CZ = 177;   // Czechia
  ALCountryID_Germany = 182;                     ALCountryID_DE = 182;   // Germany
  ALCountryID_Djibouti = 187;                    ALCountryID_DJ = 187;   // Djibouti
  ALCountryID_Denmark = 188;                     ALCountryID_DK = 188;   // Denmark
  ALCountryID_Dominica = 190;                    ALCountryID_DM = 190;   // Dominica
  ALCountryID_DominicanRepublic = 192;           ALCountryID_DO = 192;   // Dominican Republic
  ALCountryID_Algeria = 203;                     ALCountryID_DZ = 203;   // Algeria
  ALCountryID_Ecuador = 206;                     ALCountryID_EC = 206;   // Ecuador
  ALCountryID_Estonia = 208;                     ALCountryID_EE = 208;   // Estonia
  ALCountryID_Egypt = 210;                       ALCountryID_EG = 210;   // Egypt
  ALCountryID_WesternSahara = 211;               ALCountryID_EH = 211;   // Western Sahara
  ALCountryID_Eritrea = 221;                     ALCountryID_ER = 221;   // Eritrea
  ALCountryID_Spain = 222;                       ALCountryID_ES = 222;   // Spain
  ALCountryID_Ethiopia = 223;                    ALCountryID_ET = 223;   // Ethiopia
  ALCountryID_Finland = 238;                     ALCountryID_FI = 238;   // Finland
  ALCountryID_Fiji = 239;                        ALCountryID_FJ = 239;   // Fiji
  ALCountryID_FalklandIslands = 240;             ALCountryID_FK = 240;   // Falkland Islands (Malvinas)
  ALCountryID_Micronesia = 242;                  ALCountryID_FM = 242;   // Micronesia, Federated States of
  ALCountryID_FaroeIslands = 244;                ALCountryID_FO = 244;   // Faroe Islands
  ALCountryID_France = 247;                      ALCountryID_FR = 247;   // France
  ALCountryID_Gabon = 256;                       ALCountryID_GA = 256;   // Gabon
  ALCountryID_UnitedKingdom = 257;               ALCountryID_GB = 257;   // United Kingdom of Great Britain and Northern Ireland
  ALCountryID_Grenada = 259;                     ALCountryID_GD = 259;   // Grenada
  ALCountryID_Georgia = 260;                     ALCountryID_GE = 260;   // Georgia
  ALCountryID_FrenchGuiana = 261;                ALCountryID_GF = 261;   // French Guiana
  ALCountryID_Guernsey = 262;                    ALCountryID_GG = 262;   // Guernsey
  ALCountryID_Ghana = 263;                       ALCountryID_GH = 263;   // Ghana
  ALCountryID_Gibraltar = 264;                   ALCountryID_GI = 264;   // Gibraltar
  ALCountryID_Greenland = 267;                   ALCountryID_GL = 267;   // Greenland
  ALCountryID_Gambia = 268;                      ALCountryID_GM = 268;   // Gambia
  ALCountryID_Guinea = 269;                      ALCountryID_GN = 269;   // Guinea
  ALCountryID_Guadeloupe = 271;                  ALCountryID_GP = 271;   // Guadeloupe
  ALCountryID_EquatorialGuinea = 272;            ALCountryID_GQ = 272;   // Equatorial Guinea
  ALCountryID_Greece = 273;                      ALCountryID_GR = 273;   // Greece
  ALCountryID_SouthGeorgia = 274;                ALCountryID_GS = 274;   // South Georgia and the South Sandwich Islands
  ALCountryID_Guatemala = 275;                   ALCountryID_GT = 275;   // Guatemala
  ALCountryID_Guam = 276;                        ALCountryID_GU = 276;   // Guam
  ALCountryID_GuineaBissau = 278;                ALCountryID_GW = 278;   // Guinea-Bissau
  ALCountryID_Guyana = 280;                      ALCountryID_GY = 280;   // Guyana
  ALCountryID_HongKong = 292;                    ALCountryID_HK = 292;   // Hong Kong
  ALCountryID_HeardIsland = 294;                 ALCountryID_HM = 294;   // Heard Island and McDonald Islands
  ALCountryID_Honduras = 295;                    ALCountryID_HN = 295;   // Honduras
  ALCountryID_Croatia = 299;                     ALCountryID_HR = 299;   // Croatia
  ALCountryID_Haiti = 301;                       ALCountryID_HT = 301;   // Haiti
  ALCountryID_Hungary = 302;                     ALCountryID_HU = 302;   // Hungary
  ALCountryID_Indonesia = 311;                   ALCountryID_ID = 311;   // Indonesia
  ALCountryID_Ireland = 312;                     ALCountryID_IE = 312;   // Ireland
  ALCountryID_Israel = 319;                      ALCountryID_IL = 319;   // Israel
  ALCountryID_IsleofMan = 320;                   ALCountryID_IM = 320;   // Isle of Man
  ALCountryID_India = 321;                       ALCountryID_IN = 321;   // India
  ALCountryID_BritishIndianOceanTerritory = 322; ALCountryID_IO = 322;   // British Indian Ocean Territory
  ALCountryID_Iraq = 324;                        ALCountryID_IQ = 324;   // Iraq
  ALCountryID_Iran = 325;                        ALCountryID_IR = 325;   // Iran, Islamic Republic of
  ALCountryID_Iceland = 326;                     ALCountryID_IS = 326;   // Iceland
  ALCountryID_Italy = 327;                       ALCountryID_IT = 327;   // Italy
  ALCountryID_Jersey = 338;                      ALCountryID_JE = 338;   // Jersey
  ALCountryID_Jamaica = 346;                     ALCountryID_JM = 346;   // Jamaica
  ALCountryID_Jordan = 348;                      ALCountryID_JO = 348;   // Jordan
  ALCountryID_Japan = 349;                       ALCountryID_JP = 349;   // Japan
  ALCountryID_Kenya = 364;                       ALCountryID_KE = 364;   // Kenya
  ALCountryID_Kyrgyzstan = 366;                  ALCountryID_KG = 366;   // Kyrgyzstan
  ALCountryID_Cambodia = 367;                    ALCountryID_KH = 367;   // Cambodia
  ALCountryID_Kiribati = 368;                    ALCountryID_KI = 368;   // Kiribati
  ALCountryID_Comoros = 372;                     ALCountryID_KM = 372;   // Comoros
  ALCountryID_SaintKittsAndNevis = 373;          ALCountryID_KN = 373;   // Saint Kitts and Nevis
  ALCountryID_NorthKorea = 375;                  ALCountryID_KP = 375;   // Korea, Democratic People's Republic of
  ALCountryID_SouthKorea = 377;                  ALCountryID_KR = 377;   // Korea, Republic of
  ALCountryID_Kuwait = 382;                      ALCountryID_KW = 382;   // Kuwait
  ALCountryID_CaymanIslands = 384;               ALCountryID_KY = 384;   // Cayman Islands
  ALCountryID_Kazakhstan = 385;                  ALCountryID_KZ = 385;   // Kazakhstan
  ALCountryID_Laos = 386;                        ALCountryID_LA = 386;   // Lao People's Democratic Republic
  ALCountryID_Lebanon = 387;                     ALCountryID_LB = 387;   // Lebanon
  ALCountryID_SaintLucia = 388;                  ALCountryID_LC = 388;   // Saint Lucia
  ALCountryID_Liechtenstein = 394;               ALCountryID_LI = 394;   // Liechtenstein
  ALCountryID_SriLanka = 396;                    ALCountryID_LK = 396;   // Sri Lanka
  ALCountryID_Liberia = 403;                     ALCountryID_LR = 403;   // Liberia
  ALCountryID_Lesotho = 404;                     ALCountryID_LS = 404;   // Lesotho
  ALCountryID_Lithuania = 405;                   ALCountryID_LT = 405;   // Lithuania
  ALCountryID_Luxembourg = 406;                  ALCountryID_LU = 406;   // Luxembourg
  ALCountryID_Latvia = 407;                      ALCountryID_LV = 407;   // Latvia
  ALCountryID_Libya = 410;                       ALCountryID_LY = 410;   // Libya
  ALCountryID_Morocco = 412;                     ALCountryID_MA = 412;   // Morocco
  ALCountryID_Monaco = 414;                      ALCountryID_MC = 414;   // Monaco
  ALCountryID_Moldova = 415;                     ALCountryID_MD = 415;   // Moldova, Republic of
  ALCountryID_Montenegro = 416;                  ALCountryID_ME = 416;   // Montenegro
  ALCountryID_SaintMartin = 417;                 ALCountryID_MF = 417;   // Saint Martin (French part)
  ALCountryID_Madagascar = 418;                  ALCountryID_MG = 418;   // Madagascar
  ALCountryID_MarshallIslands = 419;             ALCountryID_MH = 419;   // Marshall Islands
  ALCountryID_NorthMacedonia = 422;              ALCountryID_MK = 422;   // North Macedonia
  ALCountryID_Mali = 423;                        ALCountryID_ML = 423;   // Mali
  ALCountryID_Myanmar = 424;                     ALCountryID_MM = 424;   // Myanmar
  ALCountryID_Mongolia = 425;                    ALCountryID_MN = 425;   // Mongolia
  ALCountryID_Macao = 426;                       ALCountryID_MO = 426;   // Macao
  ALCountryID_NorthernMarianaIslands = 427;      ALCountryID_MP = 427;   // Northern Mariana Islands
  ALCountryID_Martinique = 428;                  ALCountryID_MQ = 428;   // Martinique
  ALCountryID_Mauritania = 429;                  ALCountryID_MR = 429;   // Mauritania
  ALCountryID_Montserrat = 430;                  ALCountryID_MS = 430;   // Montserrat
  ALCountryID_Malta = 431;                       ALCountryID_MT = 431;   // Malta
  ALCountryID_Mauritius = 432;                   ALCountryID_MU = 432;   // Mauritius
  ALCountryID_Maldives = 433;                    ALCountryID_MV = 433;   // Maldives
  ALCountryID_Malawi = 434;                      ALCountryID_MW = 434;   // Malawi
  ALCountryID_Mexico = 435;                      ALCountryID_MX = 435;   // Mexico
  ALCountryID_Malaysia = 436;                    ALCountryID_MY = 436;   // Malaysia
  ALCountryID_Mozambique = 437;                  ALCountryID_MZ = 437;   // Mozambique
  ALCountryID_Namibia = 438;                     ALCountryID_NA = 438;   // Namibia
  ALCountryID_NewCaledonia = 440;                ALCountryID_NC = 440;   // New Caledonia
  ALCountryID_Niger = 442;                       ALCountryID_NE = 442;   // Niger
  ALCountryID_NorfolkIsland = 443;               ALCountryID_NF = 443;   // Norfolk Island
  ALCountryID_Nigeria = 444;                     ALCountryID_NG = 444;   // Nigeria
  ALCountryID_Nicaragua = 446;                   ALCountryID_NI = 446;   // Nicaragua
  ALCountryID_Netherlands = 449;                 ALCountryID_NL = 449;   // Netherlands, Kingdom of the
  ALCountryID_Norway = 452;                      ALCountryID_NO = 452;   // Norway
  ALCountryID_Nepal = 453;                       ALCountryID_NP = 453;   // Nepal
  ALCountryID_Nauru = 455;                       ALCountryID_NR = 455;   // Nauru
  ALCountryID_Niue = 458;                        ALCountryID_NU = 458;   // Niue
  ALCountryID_NewZealand = 463;                  ALCountryID_NZ = 463;   // New Zealand
  ALCountryID_Oman = 476;                        ALCountryID_OM = 476;   // Oman
  ALCountryID_Panama = 490;                      ALCountryID_PA = 490;   // Panama
  ALCountryID_Peru = 494;                        ALCountryID_PE = 494;   // Peru
  ALCountryID_FrenchPolynesia = 495;             ALCountryID_PF = 495;   // French Polynesia
  ALCountryID_PapuaNewGuinea = 496;              ALCountryID_PG = 496;   // Papua New Guinea
  ALCountryID_Philippines = 497;                 ALCountryID_PH = 497;   // Philippines
  ALCountryID_Pakistan = 500;                    ALCountryID_PK = 500;   // Pakistan
  ALCountryID_Poland = 501;                      ALCountryID_PL = 501;   // Poland
  ALCountryID_SaintPierreandMiquelon = 502;      ALCountryID_PM = 502;   // Saint Pierre and Miquelon
  ALCountryID_Pitcairn = 503;                    ALCountryID_PN = 503;   // Pitcairn
  ALCountryID_PuertoRico = 507;                  ALCountryID_PR = 507;   // Puerto Rico
  ALCountryID_Palestine = 508;                   ALCountryID_PS = 508;   // Palestine, State of
  ALCountryID_Portugal = 509;                    ALCountryID_PT = 509;   // Portugal
  ALCountryID_Palau = 512;                       ALCountryID_PW = 512;   // Palau
  ALCountryID_Paraguay = 514;                    ALCountryID_PY = 514;   // Paraguay
  ALCountryID_Qatar = 516;                       ALCountryID_QA = 516;   // Qatar
  ALCountryID_Reunion = 546;                     ALCountryID_RE = 546;   // Réunion
  ALCountryID_Romania = 556;                     ALCountryID_RO = 556;   // Romania
  ALCountryID_Serbia = 560;                      ALCountryID_RS = 560;   // Serbia
  ALCountryID_Russia = 562;                      ALCountryID_RU = 562;   // Russian Federation
  ALCountryID_Rwanda = 564;                      ALCountryID_RW = 564;   // Rwanda
  ALCountryID_SaudiArabia = 568;                 ALCountryID_SA = 568;   // Saudi Arabia
  ALCountryID_SolomonIslands = 569;              ALCountryID_SB = 569;   // Solomon Islands
  ALCountryID_Seychelles = 570;                  ALCountryID_SC = 570;   // Seychelles
  ALCountryID_Sudan = 571;                       ALCountryID_SD = 571;   // Sudan
  ALCountryID_Sweden = 572;                      ALCountryID_SE = 572;   // Sweden
  ALCountryID_Singapore = 574;                   ALCountryID_SG = 574;   // Singapore
  ALCountryID_SaintHelena = 575;                 ALCountryID_SH = 575;   // Saint Helena, Ascension and Tristan da Cunha
  ALCountryID_Slovenia = 576;                    ALCountryID_SI = 576;   // Slovenia
  ALCountryID_SvalbardandJanMayen = 577;         ALCountryID_SJ = 577;   // Svalbard and Jan Mayen
  ALCountryID_Slovakia = 578;                    ALCountryID_SK = 578;   // Slovakia
  ALCountryID_SierraLeone = 579;                 ALCountryID_SL = 579;   // Sierra Leone
  ALCountryID_SanMarino = 580;                   ALCountryID_SM = 580;   // San Marino
  ALCountryID_Senegal = 581;                     ALCountryID_SN = 581;   // Senegal
  ALCountryID_Somalia = 582;                     ALCountryID_SO = 582;   // Somalia
  ALCountryID_Suriname = 585;                    ALCountryID_SR = 585;   // Suriname
  ALCountryID_SouthSudan = 586;                  ALCountryID_SS = 586;   // South Sudan
  ALCountryID_SaoTomeAndPrincipe = 587;          ALCountryID_ST = 587;   // Sao Tome and Principe
  ALCountryID_ElSalvador = 589;                  ALCountryID_SV = 589;   // El Salvador
  ALCountryID_SintMaarten = 591;                 ALCountryID_SX = 591;   // Sint Maarten (Dutch part)
  ALCountryID_Syria = 592;                       ALCountryID_SY = 592;   // Syrian Arab Republic
  ALCountryID_Eswatini = 593;                    ALCountryID_SZ = 593;   // Eswatini
  ALCountryID_TurksandCaicosIslands = 596;       ALCountryID_TC = 596;   // Turks and Caicos Islands
  ALCountryID_Chad = 597;                        ALCountryID_TD = 597;   // Chad
  ALCountryID_FrenchSouthernTerritories = 599;   ALCountryID_TF = 599;   // French Southern Territories
  ALCountryID_Togo = 600;                        ALCountryID_TG = 600;   // Togo
  ALCountryID_Thailand = 601;                    ALCountryID_TH = 601;   // Thailand
  ALCountryID_Tajikistan = 603;                  ALCountryID_TJ = 603;   // Tajikistan
  ALCountryID_Tokelau = 604;                     ALCountryID_TK = 604;   // Tokelau
  ALCountryID_TimorLeste = 605;                  ALCountryID_TL = 605;   // Timor-Leste
  ALCountryID_Turkmenistan = 606;                ALCountryID_TM = 606;   // Turkmenistan
  ALCountryID_Tunisia = 607;                     ALCountryID_TN = 607;   // Tunisia
  ALCountryID_Tonga = 608;                       ALCountryID_TO = 608;   // Tonga
  ALCountryID_Turkiye = 611;                     ALCountryID_TR = 611;   // Türkiye
  ALCountryID_TrinidadAndTobago = 613;           ALCountryID_TT = 613;   // Trinidad and Tobago
  ALCountryID_Tuvalu = 615;                      ALCountryID_TV = 615;   // Tuvalu
  ALCountryID_Taiwan = 616;                      ALCountryID_TW = 616;   // Taiwan, Province of China
  ALCountryID_Tanzania = 619;                    ALCountryID_TZ = 619;   // Tanzania, United Republic of
  ALCountryID_Ukraine = 620;                     ALCountryID_UA = 620;   // Ukraine
  ALCountryID_Uganda = 626;                      ALCountryID_UG = 626;   // Uganda
  ALCountryID_USMinorOutlyingIslands = 632;      ALCountryID_UM = 632;   // United States Minor Outlying Islands
  ALCountryID_UnitedStates = 638;                ALCountryID_US = 638;   // United States of America
  ALCountryID_Uruguay = 644;                     ALCountryID_UY = 644;   // Uruguay
  ALCountryID_Uzbekistan = 645;                  ALCountryID_UZ = 645;   // Uzbekistan
  ALCountryID_HolySee = 646;                     ALCountryID_VA = 646;   // Holy See
  ALCountryID_SaintVincent = 648;                ALCountryID_VC = 648;   // Saint Vincent and the Grenadines
  ALCountryID_Venezuela = 650;                   ALCountryID_VE = 650;   // Venezuela, Bolivarian Republic of
  ALCountryID_BritishVirginIslands = 652;        ALCountryID_VG = 652;   // Virgin Islands (British)
  ALCountryID_USVirginIslands = 654;             ALCountryID_VI = 654;   // Virgin Islands (U.S.)
  ALCountryID_VietNam = 659;                     ALCountryID_VN = 659;   // Viet Nam
  ALCountryID_Vanuatu = 666;                     ALCountryID_VU = 666;   // Vanuatu
  ALCountryID_WallisAndFutuna = 677;             ALCountryID_WF = 677;   // Wallis and Futuna
  ALCountryID_Samoa = 690;                       ALCountryID_WS = 690;   // Samoa
  ALCountryID_Yemen = 728;                       ALCountryID_YE = 728;   // Yemen
  ALCountryID_Mayotte = 743;                     ALCountryID_YT = 743;   // Mayotte
  ALCountryID_SouthAfrica = 750;                 ALCountryID_ZA = 750;   // South Africa
  ALCountryID_Zambia = 762;                      ALCountryID_ZM = 762;   // Zambia
  ALCountryID_Zimbabwe = 772;                    ALCountryID_ZW = 772;   // Zimbabwe

implementation

uses
  System.Math,
  {$IF defined(ANDROID)}
  Androidapi.Helpers,
  Androidapi.JNI.Os,
  Androidapi.JNI.JavaTypes,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.Helpers,
  iOSapi.Foundation,
  {$ENDIF}
  {$IF defined(ALMacOS)}
  Macapi.Helpers,
  Macapi.Foundation,
  {$ENDIF}
  System.AnsiStrings,
  Alcinoe.Common,
  Alcinoe.StringUtils;

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

{$IFNDEF ALCompilerVersionSupported123}
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

{*******************************************}
class function TALLocaleW.Create: TALLocaleW;
begin
  Result := TALLocaleW.Create('');
end;

{*********************}
// From sun.util.locale
// public static LanguageTag parse(String languageTag, ParseStatus sts)
class function TALLocaleW.Create(const ALanguageTag: String): TALLocaleW;

var
  P1, P2: Integer;
  LCurrent: String;
  LLanguageTag: String;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _IterateNext;
  begin
    P1 := P2 + 1;
    P2 := P1;
    While (P2 <= High(LLanguageTag)) and (not CharInSet(LLanguageTag[P2], ['-','_'])) do inc(P2);
    LCurrent := ALCopyStr(LLanguageTag, P1, P2-P1);
  end;

begin

  Result.Language := '';
  //Result.LanguageID
  Result.Script := '';
  //Result.ScriptID
  Result.Region := '';
  //Result.RegionID
  Setlength(Result.Variants, 0);
  Setlength(Result.Extensions, 0);
  Result.Privateuse := '';
  //Result.FormatSettings

  LLanguageTag := ALanguageTag;
  If LLanguageTag = '' then begin
    var LPreferredLanguages := ALGetPreferredLanguages;
    if length(LPreferredLanguages) > 0 then LLanguageTag := LPreferredLanguages[low(LPreferredLanguages)];
    if LLanguageTag = '' then LLanguageTag := 'en-US';
  end;

  // LLanguageTag = language
  //                ["-" script]
  //                ["-" region]
  //                *("-" variant)
  //                *("-" extension)
  //                ["-" privateuse]
  P1 := low(LLanguageTag);
  P2 := P1;
  While (P2 <= High(LLanguageTag)) and (not CharInSet(LLanguageTag[P2], ['-','_'])) do inc(P2);
  LCurrent := ALCopyStr(LLanguageTag, P1, P2-P1);

  // language      = 2*3ALPHA            ; shortest ISO 639 code
  //                 ["-" extlang]       ; sometimes followed by
  //                                     ; extended language subtags
  //               / 4ALPHA              ; or reserved for future use
  //               / 5*8ALPHA            ; or registered language subtag
  if (length(LCurrent) in [2..8]) and (ALisAlphaString(LCurrent)) then begin
    Result.Language := ALLowerCase(LCurrent);
    _IterateNext;
  end;

  // langtag must start with either language or privateuse
  if Result.Language <> '' then begin

    // extlang       = 3ALPHA              ; selected ISO 639 codes
    //                 *2("-" 3ALPHA)      ; permanently reserved
    for var LExtlang := 1 to 3 do begin // Maximum 3 extlangs
      if (length(LCurrent) = 3) and (ALIsAlphaString(LCurrent)) then _IterateNext
      else break;
    end;

    // script        = 4ALPHA              ; ISO 15924 code
    if (length(LCurrent) = 4) and (ALIsAlphaString(LCurrent)) then begin
      Result.Script := ALTitleCase(LCurrent);
      _IterateNext;
    end;

    // region        = 2ALPHA              ; ISO 3166-1 code
    //               / 3DIGIT              ; UN M.49 code
    if ((length(LCurrent) = 2) and (ALIsAlphaString(LCurrent))) or
       ((length(LCurrent) = 3) and (ALIsNumeric(LCurrent, true{RejectPlusMinusSign}))) then begin
      Result.Region := ALUppercase(LCurrent);
      _IterateNext;
    end;

    // variant       = 5*8alphanum         ; registered variants
    //               / (DIGIT 3alphanum)
    while ((length(LCurrent) in [5..8]) and (ALIsAlphaNumeric(LCurrent))) or
          ((length(LCurrent) = 4) and
           (CharInSet(LCurrent[low(LCurrent)], ['0'..'9'])) and
           (CharInSet(LCurrent[low(LCurrent)+1], ['A'..'Z']) or CharInSet(LCurrent[low(LCurrent)+1], ['a'..'z']) or CharInSet(LCurrent[low(LCurrent)+1], ['0'..'9'])) and
           (CharInSet(LCurrent[low(LCurrent)+2], ['A'..'Z']) or CharInSet(LCurrent[low(LCurrent)+2], ['a'..'z']) or CharInSet(LCurrent[low(LCurrent)+2], ['0'..'9'])) and
           (CharInSet(LCurrent[low(LCurrent)+3], ['A'..'Z']) or CharInSet(LCurrent[low(LCurrent)+3], ['a'..'z']) or CharInSet(LCurrent[low(LCurrent)+3], ['0'..'9']))) do begin
      Setlength(Result.Variants, length(Result.Variants)+1);
      Result.Variants[high(Result.Variants)] := ALLowercase(LCurrent);
      _IterateNext;
    end;

    // extension     = singleton 1*("-" (2*8alphanum))
    //
    //                                     ; Single alphanumerics
    //                                     ; "x" reserved for private use
    // singleton     = DIGIT               ; 0 - 9
    //               / %x41-57             ; A - W
    //               / %x59-5A             ; Y - Z
    //               / %x61-77             ; a - w
    //               / %x79-7A             ; y - z
    while (length(LCurrent) = 1) and (ALIsAlphaString(LCurrent)) and (not AlSameTextW(LCurrent, 'x')) do begin
      var Lextension := LCurrent;
      _IterateNext;
      while (length(LCurrent) in [2..8]) and (ALIsAlphaNumeric(LCurrent)) do begin
        Lextension := Lextension + '-' + LCurrent;
        _IterateNext;
      end;
      if length(Lextension) = 1 then
        Raise Exception.CreateFmt('Incomplete extension "%s"', [Lextension]);
      Setlength(Result.Extensions, length(Result.Extensions)+1);
      Result.Extensions[high(Result.Extensions)] := ALLowerCase(Lextension);
    end;
  end;

  // privateuse    = "x" 1*("-" (1*8alphanum))
  if (length(LCurrent) = 1) and (AlSameTextW(LCurrent, 'x')) then begin
    var LPrivateUse := LCurrent;
    _IterateNext;
    while (length(LCurrent) in [1..8]) and (ALIsAlphaNumeric(LCurrent)) do begin
      LPrivateUse := LPrivateUse + '-' + LCurrent;
      _IterateNext;
    end;
    if length(LPrivateUse) = 1 then
      Raise Exception.Create('Incomplete privateuse');
    Result.Privateuse := ALLowercase(LPrivateUse);
  end;

  // LCurrent must be empty
  if LCurrent <> '' then
    Raise Exception.CreateFmt('Invalid subtag', [LCurrent]);

  // FormatSettings
  Result.FormatSettings := TALFormatSettingsW.Create(LLanguageTag);

  // LanguageID
  Result.LanguageID := ALLanguageCodeToID(Result.Language);

  // ScriptID
  Result.ScriptID := ALScriptCodeToID(Result.Script);

  // RegionID
  Result.RegionID := ALRegionCodeToID(Result.Region);

end;

{*******************************************}
class function TALLocaleA.Create: TALLocaleA;
begin
  Result := TALLocaleA.Create('');
end;

{*********************}
// From sun.util.locale
// public static LanguageTag parse(String languageTag, ParseStatus sts)
class function TALLocaleA.Create(const ALanguageTag: AnsiString): TALLocaleA;

var
  P1, P2: Integer;
  LCurrent: AnsiString;
  LLanguageTag: AnsiString;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _IterateNext;
  begin
    P1 := P2 + 1;
    P2 := P1;
    While (P2 <= High(LLanguageTag)) and (not CharInSet(LLanguageTag[P2], ['-','_'])) do inc(P2);
    LCurrent := ALCopyStr(LLanguageTag, P1, P2-P1);
  end;

begin

  Result.Language := '';
  //Result.LanguageID
  Result.Script := '';
  //Result.ScriptID
  Result.Region := '';
  //Result.RegionID
  Setlength(Result.Variants, 0);
  Setlength(Result.Extensions, 0);
  Result.Privateuse := '';
  //Result.FormatSettings

  LLanguageTag := ALanguageTag;
  If LLanguageTag = '' then begin
    var LPreferredLanguages := ALGetPreferredLanguages;
    if length(LPreferredLanguages) > 0 then LLanguageTag := AnsiString(LPreferredLanguages[low(LPreferredLanguages)]);
    if LLanguageTag = '' then LLanguageTag := 'en-US';
  end;

  // LLanguageTag = language
  //                ["-" script]
  //                ["-" region]
  //                *("-" variant)
  //                *("-" extension)
  //                ["-" privateuse]
  P1 := low(LLanguageTag);
  P2 := P1;
  While (P2 <= High(LLanguageTag)) and (not CharInSet(LLanguageTag[P2], ['-','_'])) do inc(P2);
  LCurrent := ALCopyStr(LLanguageTag, P1, P2-P1);

  // language      = 2*3ALPHA            ; shortest ISO 639 code
  //                 ["-" extlang]       ; sometimes followed by
  //                                     ; extended language subtags
  //               / 4ALPHA              ; or reserved for future use
  //               / 5*8ALPHA            ; or registered language subtag
  if (length(LCurrent) in [2..8]) and (ALisAlphaString(LCurrent)) then begin
    Result.Language := ALLowerCase(LCurrent);
    _IterateNext;
  end;

  // langtag must start with either language or privateuse
  if Result.Language <> '' then begin

    // extlang       = 3ALPHA              ; selected ISO 639 codes
    //                 *2("-" 3ALPHA)      ; permanently reserved
    for var LExtlang := 1 to 3 do begin // Maximum 3 extlangs
      if (length(LCurrent) = 3) and (ALIsAlphaString(LCurrent)) then _IterateNext
      else break;
    end;

    // script        = 4ALPHA              ; ISO 15924 code
    if (length(LCurrent) = 4) and (ALIsAlphaString(LCurrent)) then begin
      Result.Script := ALTitleCase(LCurrent);
      _IterateNext;
    end;

    // region        = 2ALPHA              ; ISO 3166-1 code
    //               / 3DIGIT              ; UN M.49 code
    if ((length(LCurrent) = 2) and (ALIsAlphaString(LCurrent))) or
       ((length(LCurrent) = 3) and (ALIsNumeric(LCurrent, true{RejectPlusMinusSign}))) then begin
      Result.Region := ALUppercase(LCurrent);
      _IterateNext;
    end;

    // variant       = 5*8alphanum         ; registered variants
    //               / (DIGIT 3alphanum)
    while ((length(LCurrent) in [5..8]) and (ALIsAlphaNumeric(LCurrent))) or
          ((length(LCurrent) = 4) and
           (CharInSet(LCurrent[low(LCurrent)], ['0'..'9'])) and
           (CharInSet(LCurrent[low(LCurrent)+1], ['A'..'Z']) or CharInSet(LCurrent[low(LCurrent)+1], ['a'..'z']) or CharInSet(LCurrent[low(LCurrent)+1], ['0'..'9'])) and
           (CharInSet(LCurrent[low(LCurrent)+2], ['A'..'Z']) or CharInSet(LCurrent[low(LCurrent)+2], ['a'..'z']) or CharInSet(LCurrent[low(LCurrent)+2], ['0'..'9'])) and
           (CharInSet(LCurrent[low(LCurrent)+3], ['A'..'Z']) or CharInSet(LCurrent[low(LCurrent)+3], ['a'..'z']) or CharInSet(LCurrent[low(LCurrent)+3], ['0'..'9']))) do begin
      Setlength(Result.Variants, length(Result.Variants)+1);
      Result.Variants[high(Result.Variants)] := ALLowercase(LCurrent);
      _IterateNext;
    end;

    // extension     = singleton 1*("-" (2*8alphanum))
    //
    //                                     ; Single alphanumerics
    //                                     ; "x" reserved for private use
    // singleton     = DIGIT               ; 0 - 9
    //               / %x41-57             ; A - W
    //               / %x59-5A             ; Y - Z
    //               / %x61-77             ; a - w
    //               / %x79-7A             ; y - z
    while (length(LCurrent) = 1) and (ALIsAlphaString(LCurrent)) and (not AlSameTextA(LCurrent, 'x')) do begin
      var Lextension := LCurrent;
      _IterateNext;
      while (length(LCurrent) in [2..8]) and (ALIsAlphaNumeric(LCurrent)) do begin
        Lextension := Lextension + '-' + LCurrent;
        _IterateNext;
      end;
      if length(Lextension) = 1 then
        Raise Exception.CreateFmt('Incomplete extension "%s"', [Lextension]);
      Setlength(Result.Extensions, length(Result.Extensions)+1);
      Result.Extensions[high(Result.Extensions)] := ALLowerCase(Lextension);
    end;
  end;

  // privateuse    = "x" 1*("-" (1*8alphanum))
  if (length(LCurrent) = 1) and (AlSameTextA(LCurrent, 'x')) then begin
    var LPrivateUse := LCurrent;
    _IterateNext;
    while (length(LCurrent) in [1..8]) and (ALIsAlphaNumeric(LCurrent)) do begin
      LPrivateUse := LPrivateUse + '-' + LCurrent;
      _IterateNext;
    end;
    if length(LPrivateUse) = 1 then
      Raise Exception.Create('Incomplete privateuse');
    Result.Privateuse := ALLowercase(LPrivateUse);
  end;

  // LCurrent must be empty
  if LCurrent <> '' then
    Raise Exception.CreateFmt('Invalid subtag', [LCurrent]);

  // FormatSettings
  Result.FormatSettings := TALFormatSettingsA.Create(LLanguageTag);

  // LanguageID
  Result.LanguageID := ALLanguageCodeToID(Result.Language);

  // ScriptID
  Result.ScriptID := ALScriptCodeToID(Result.Script);

  // RegionID
  Result.RegionID := ALRegionCodeToID(Result.Region);

end;

{********************************}
Constructor TALPluralRules.Create;
begin

  inherited;

  FCardinalPluralCategoryFuncByLanguages := TDictionary<integer, TGetPluralCategoryFunc>.Create;
  FOrdinalPluralCategoryFuncByLanguages := TDictionary<integer, TGetPluralCategoryFunc>.Create;

  {$REGION 'Auto-generated by <ALCINOE>\Tools\CodeBuilder (2)'}

  FCardinalPluralCategoryFuncByLanguages.add(105,GetCardinalPluralCategory_AF);
  FCardinalPluralCategoryFuncByLanguages.add(110,GetCardinalPluralCategory_AK);
  FCardinalPluralCategoryFuncByLanguages.add(112,GetCardinalPluralCategory_AM);
  FCardinalPluralCategoryFuncByLanguages.add(113,GetCardinalPluralCategory_AN);
  FCardinalPluralCategoryFuncByLanguages.add(117,GetCardinalPluralCategory_AR);
  FCardinalPluralCategoryFuncByLanguages.add(1460,GetCardinalPluralCategory_ARS);
  FCardinalPluralCategoryFuncByLanguages.add(118,GetCardinalPluralCategory_AS);
  FCardinalPluralCategoryFuncByLanguages.add(1468,GetCardinalPluralCategory_ASA);
  FCardinalPluralCategoryFuncByLanguages.add(1487,GetCardinalPluralCategory_AST);
  FCardinalPluralCategoryFuncByLanguages.add(125,GetCardinalPluralCategory_AZ);
  FCardinalPluralCategoryFuncByLanguages.add(1687,GetCardinalPluralCategory_BAL);
  FCardinalPluralCategoryFuncByLanguages.add(130,GetCardinalPluralCategory_BE);
  FCardinalPluralCategoryFuncByLanguages.add(1792,GetCardinalPluralCategory_BEM);
  FCardinalPluralCategoryFuncByLanguages.add(1805,GetCardinalPluralCategory_BEZ);
  FCardinalPluralCategoryFuncByLanguages.add(132,GetCardinalPluralCategory_BG);
  FCardinalPluralCategoryFuncByLanguages.add(1872,GetCardinalPluralCategory_BHO);
  FCardinalPluralCategoryFuncByLanguages.add(1976,GetCardinalPluralCategory_BLO);
  FCardinalPluralCategoryFuncByLanguages.add(138,GetCardinalPluralCategory_BM);
  FCardinalPluralCategoryFuncByLanguages.add(139,GetCardinalPluralCategory_BN);
  FCardinalPluralCategoryFuncByLanguages.add(140,GetCardinalPluralCategory_BO);
  FCardinalPluralCategoryFuncByLanguages.add(143,GetCardinalPluralCategory_BR);
  FCardinalPluralCategoryFuncByLanguages.add(2141,GetCardinalPluralCategory_BRX);
  FCardinalPluralCategoryFuncByLanguages.add(144,GetCardinalPluralCategory_BS);
  FCardinalPluralCategoryFuncByLanguages.add(152,GetCardinalPluralCategory_CA);
  FCardinalPluralCategoryFuncByLanguages.add(156,GetCardinalPluralCategory_CE);
  FCardinalPluralCategoryFuncByLanguages.add(2457,GetCardinalPluralCategory_CEB);
  FCardinalPluralCategoryFuncByLanguages.add(2514,GetCardinalPluralCategory_CGG);
  FCardinalPluralCategoryFuncByLanguages.add(2551,GetCardinalPluralCategory_CHR);
  FCardinalPluralCategoryFuncByLanguages.add(2613,GetCardinalPluralCategory_CKB);
  FCardinalPluralCategoryFuncByLanguages.add(170,GetCardinalPluralCategory_CS);
  FCardinalPluralCategoryFuncByLanguages.add(2842,GetCardinalPluralCategory_CSW);
  FCardinalPluralCategoryFuncByLanguages.add(176,GetCardinalPluralCategory_CY);
  FCardinalPluralCategoryFuncByLanguages.add(178,GetCardinalPluralCategory_DA);
  FCardinalPluralCategoryFuncByLanguages.add(182,GetCardinalPluralCategory_DE);
  FCardinalPluralCategoryFuncByLanguages.add(3400,GetCardinalPluralCategory_DOI);
  FCardinalPluralCategoryFuncByLanguages.add(3497,GetCardinalPluralCategory_DSB);
  FCardinalPluralCategoryFuncByLanguages.add(199,GetCardinalPluralCategory_DV);
  FCardinalPluralCategoryFuncByLanguages.add(203,GetCardinalPluralCategory_DZ);
  FCardinalPluralCategoryFuncByLanguages.add(208,GetCardinalPluralCategory_EE);
  FCardinalPluralCategoryFuncByLanguages.add(215,GetCardinalPluralCategory_EL);
  FCardinalPluralCategoryFuncByLanguages.add(217,GetCardinalPluralCategory_EN);
  FCardinalPluralCategoryFuncByLanguages.add(218,GetCardinalPluralCategory_EO);
  FCardinalPluralCategoryFuncByLanguages.add(222,GetCardinalPluralCategory_ES);
  FCardinalPluralCategoryFuncByLanguages.add(223,GetCardinalPluralCategory_ET);
  FCardinalPluralCategoryFuncByLanguages.add(224,GetCardinalPluralCategory_EU);
  FCardinalPluralCategoryFuncByLanguages.add(230,GetCardinalPluralCategory_FA);
  FCardinalPluralCategoryFuncByLanguages.add(235,GetCardinalPluralCategory_FF);
  FCardinalPluralCategoryFuncByLanguages.add(238,GetCardinalPluralCategory_FI);
  FCardinalPluralCategoryFuncByLanguages.add(4599,GetCardinalPluralCategory_FIL);
  FCardinalPluralCategoryFuncByLanguages.add(244,GetCardinalPluralCategory_FO);
  FCardinalPluralCategoryFuncByLanguages.add(247,GetCardinalPluralCategory_FR);
  FCardinalPluralCategoryFuncByLanguages.add(4917,GetCardinalPluralCategory_FUR);
  FCardinalPluralCategoryFuncByLanguages.add(254,GetCardinalPluralCategory_FY);
  FCardinalPluralCategoryFuncByLanguages.add(256,GetCardinalPluralCategory_GA);
  FCardinalPluralCategoryFuncByLanguages.add(259,GetCardinalPluralCategory_GD);
  FCardinalPluralCategoryFuncByLanguages.add(267,GetCardinalPluralCategory_GL);
  FCardinalPluralCategoryFuncByLanguages.add(5546,GetCardinalPluralCategory_GSW);
  FCardinalPluralCategoryFuncByLanguages.add(276,GetCardinalPluralCategory_GU);
  FCardinalPluralCategoryFuncByLanguages.add(5598,GetCardinalPluralCategory_GUW);
  FCardinalPluralCategoryFuncByLanguages.add(277,GetCardinalPluralCategory_GV);
  FCardinalPluralCategoryFuncByLanguages.add(282,GetCardinalPluralCategory_HA);
  FCardinalPluralCategoryFuncByLanguages.add(5754,GetCardinalPluralCategory_HAW);
  FCardinalPluralCategoryFuncByLanguages.add(286,GetCardinalPluralCategory_HE);
  FCardinalPluralCategoryFuncByLanguages.add(290,GetCardinalPluralCategory_HI);
  FCardinalPluralCategoryFuncByLanguages.add(6079,GetCardinalPluralCategory_HNJ);
  FCardinalPluralCategoryFuncByLanguages.add(299,GetCardinalPluralCategory_HR);
  FCardinalPluralCategoryFuncByLanguages.add(6201,GetCardinalPluralCategory_HSB);
  FCardinalPluralCategoryFuncByLanguages.add(302,GetCardinalPluralCategory_HU);
  FCardinalPluralCategoryFuncByLanguages.add(306,GetCardinalPluralCategory_HY);
  FCardinalPluralCategoryFuncByLanguages.add(308,GetCardinalPluralCategory_IA);
  FCardinalPluralCategoryFuncByLanguages.add(311,GetCardinalPluralCategory_ID);
  FCardinalPluralCategoryFuncByLanguages.add(314,GetCardinalPluralCategory_IG);
  FCardinalPluralCategoryFuncByLanguages.add(316,GetCardinalPluralCategory_II);
  FCardinalPluralCategoryFuncByLanguages.add(322,GetCardinalPluralCategory_IO);
  FCardinalPluralCategoryFuncByLanguages.add(326,GetCardinalPluralCategory_IS);
  FCardinalPluralCategoryFuncByLanguages.add(327,GetCardinalPluralCategory_IT);
  FCardinalPluralCategoryFuncByLanguages.add(328,GetCardinalPluralCategory_IU);
  FCardinalPluralCategoryFuncByLanguages.add(334,GetCardinalPluralCategory_JA);
  FCardinalPluralCategoryFuncByLanguages.add(7124,GetCardinalPluralCategory_JBO);
  FCardinalPluralCategoryFuncByLanguages.add(7254,GetCardinalPluralCategory_JGO);
  FCardinalPluralCategoryFuncByLanguages.add(7398,GetCardinalPluralCategory_JMC);
  FCardinalPluralCategoryFuncByLanguages.add(355,GetCardinalPluralCategory_JV);
  FCardinalPluralCategoryFuncByLanguages.add(356,GetCardinalPluralCategory_JW);
  FCardinalPluralCategoryFuncByLanguages.add(360,GetCardinalPluralCategory_KA);
  FCardinalPluralCategoryFuncByLanguages.add(7761,GetCardinalPluralCategory_KAB);
  FCardinalPluralCategoryFuncByLanguages.add(7769,GetCardinalPluralCategory_KAJ);
  FCardinalPluralCategoryFuncByLanguages.add(7818,GetCardinalPluralCategory_KCG);
  FCardinalPluralCategoryFuncByLanguages.add(7842,GetCardinalPluralCategory_KDE);
  FCardinalPluralCategoryFuncByLanguages.add(7864,GetCardinalPluralCategory_KEA);
  FCardinalPluralCategoryFuncByLanguages.add(370,GetCardinalPluralCategory_KK);
  FCardinalPluralCategoryFuncByLanguages.add(8029,GetCardinalPluralCategory_KKJ);
  FCardinalPluralCategoryFuncByLanguages.add(371,GetCardinalPluralCategory_KL);
  FCardinalPluralCategoryFuncByLanguages.add(372,GetCardinalPluralCategory_KM);
  FCardinalPluralCategoryFuncByLanguages.add(373,GetCardinalPluralCategory_KN);
  FCardinalPluralCategoryFuncByLanguages.add(374,GetCardinalPluralCategory_KO);
  FCardinalPluralCategoryFuncByLanguages.add(378,GetCardinalPluralCategory_KS);
  FCardinalPluralCategoryFuncByLanguages.add(8229,GetCardinalPluralCategory_KSB);
  FCardinalPluralCategoryFuncByLanguages.add(8235,GetCardinalPluralCategory_KSH);
  FCardinalPluralCategoryFuncByLanguages.add(380,GetCardinalPluralCategory_KU);
  FCardinalPluralCategoryFuncByLanguages.add(382,GetCardinalPluralCategory_KW);
  FCardinalPluralCategoryFuncByLanguages.add(384,GetCardinalPluralCategory_KY);
  FCardinalPluralCategoryFuncByLanguages.add(8442,GetCardinalPluralCategory_LAG);
  FCardinalPluralCategoryFuncByLanguages.add(387,GetCardinalPluralCategory_LB);
  FCardinalPluralCategoryFuncByLanguages.add(392,GetCardinalPluralCategory_LG);
  FCardinalPluralCategoryFuncByLanguages.add(8653,GetCardinalPluralCategory_LIJ);
  FCardinalPluralCategoryFuncByLanguages.add(8715,GetCardinalPluralCategory_LKT);
  FCardinalPluralCategoryFuncByLanguages.add(8725,GetCardinalPluralCategory_LLD);
  FCardinalPluralCategoryFuncByLanguages.add(399,GetCardinalPluralCategory_LN);
  FCardinalPluralCategoryFuncByLanguages.add(400,GetCardinalPluralCategory_LO);
  FCardinalPluralCategoryFuncByLanguages.add(405,GetCardinalPluralCategory_LT);
  FCardinalPluralCategoryFuncByLanguages.add(407,GetCardinalPluralCategory_LV);
  FCardinalPluralCategoryFuncByLanguages.add(9130,GetCardinalPluralCategory_MAS);
  FCardinalPluralCategoryFuncByLanguages.add(418,GetCardinalPluralCategory_MG);
  FCardinalPluralCategoryFuncByLanguages.add(9282,GetCardinalPluralCategory_MGO);
  FCardinalPluralCategoryFuncByLanguages.add(422,GetCardinalPluralCategory_MK);
  FCardinalPluralCategoryFuncByLanguages.add(423,GetCardinalPluralCategory_ML);
  FCardinalPluralCategoryFuncByLanguages.add(425,GetCardinalPluralCategory_MN);
  FCardinalPluralCategoryFuncByLanguages.add(426,GetCardinalPluralCategory_MO);
  FCardinalPluralCategoryFuncByLanguages.add(429,GetCardinalPluralCategory_MR);
  FCardinalPluralCategoryFuncByLanguages.add(430,GetCardinalPluralCategory_MS);
  FCardinalPluralCategoryFuncByLanguages.add(431,GetCardinalPluralCategory_MT);
  FCardinalPluralCategoryFuncByLanguages.add(436,GetCardinalPluralCategory_MY);
  FCardinalPluralCategoryFuncByLanguages.add(9795,GetCardinalPluralCategory_NAH);
  FCardinalPluralCategoryFuncByLanguages.add(9804,GetCardinalPluralCategory_NAQ);
  FCardinalPluralCategoryFuncByLanguages.add(439,GetCardinalPluralCategory_NB);
  FCardinalPluralCategoryFuncByLanguages.add(441,GetCardinalPluralCategory_ND);
  FCardinalPluralCategoryFuncByLanguages.add(442,GetCardinalPluralCategory_NE);
  FCardinalPluralCategoryFuncByLanguages.add(449,GetCardinalPluralCategory_NL);
  FCardinalPluralCategoryFuncByLanguages.add(451,GetCardinalPluralCategory_NN);
  FCardinalPluralCategoryFuncByLanguages.add(10133,GetCardinalPluralCategory_NNH);
  FCardinalPluralCategoryFuncByLanguages.add(452,GetCardinalPluralCategory_NO);
  FCardinalPluralCategoryFuncByLanguages.add(10218,GetCardinalPluralCategory_NQO);
  FCardinalPluralCategoryFuncByLanguages.add(455,GetCardinalPluralCategory_NR);
  FCardinalPluralCategoryFuncByLanguages.add(10270,GetCardinalPluralCategory_NSO);
  FCardinalPluralCategoryFuncByLanguages.add(462,GetCardinalPluralCategory_NY);
  FCardinalPluralCategoryFuncByLanguages.add(10425,GetCardinalPluralCategory_NYN);
  FCardinalPluralCategoryFuncByLanguages.add(476,GetCardinalPluralCategory_OM);
  FCardinalPluralCategoryFuncByLanguages.add(481,GetCardinalPluralCategory_OR);
  FCardinalPluralCategoryFuncByLanguages.add(482,GetCardinalPluralCategory_OS);
  FCardinalPluralCategoryFuncByLanguages.add(10932,GetCardinalPluralCategory_OSA);
  FCardinalPluralCategoryFuncByLanguages.add(490,GetCardinalPluralCategory_PA);
  FCardinalPluralCategoryFuncByLanguages.add(11155,GetCardinalPluralCategory_PAP);
  FCardinalPluralCategoryFuncByLanguages.add(11204,GetCardinalPluralCategory_PCM);
  FCardinalPluralCategoryFuncByLanguages.add(501,GetCardinalPluralCategory_PL);
  FCardinalPluralCategoryFuncByLanguages.add(11588,GetCardinalPluralCategory_PRG);
  FCardinalPluralCategoryFuncByLanguages.add(508,GetCardinalPluralCategory_PS);
  FCardinalPluralCategoryFuncByLanguages.add(509,GetCardinalPluralCategory_PT);
  FCardinalPluralCategoryFuncByLanguages.add(554,GetCardinalPluralCategory_RM);
  FCardinalPluralCategoryFuncByLanguages.add(556,GetCardinalPluralCategory_RO);
  FCardinalPluralCategoryFuncByLanguages.add(12861,GetCardinalPluralCategory_ROF);
  FCardinalPluralCategoryFuncByLanguages.add(562,GetCardinalPluralCategory_RU);
  FCardinalPluralCategoryFuncByLanguages.add(13074,GetCardinalPluralCategory_RWK);
  FCardinalPluralCategoryFuncByLanguages.add(13175,GetCardinalPluralCategory_SAH);
  FCardinalPluralCategoryFuncByLanguages.add(13184,GetCardinalPluralCategory_SAQ);
  FCardinalPluralCategoryFuncByLanguages.add(13187,GetCardinalPluralCategory_SAT);
  FCardinalPluralCategoryFuncByLanguages.add(570,GetCardinalPluralCategory_SC);
  FCardinalPluralCategoryFuncByLanguages.add(13233,GetCardinalPluralCategory_SCN);
  FCardinalPluralCategoryFuncByLanguages.add(571,GetCardinalPluralCategory_SD);
  FCardinalPluralCategoryFuncByLanguages.add(13253,GetCardinalPluralCategory_SDH);
  FCardinalPluralCategoryFuncByLanguages.add(572,GetCardinalPluralCategory_SE);
  FCardinalPluralCategoryFuncByLanguages.add(13279,GetCardinalPluralCategory_SEH);
  FCardinalPluralCategoryFuncByLanguages.add(13290,GetCardinalPluralCategory_SES);
  FCardinalPluralCategoryFuncByLanguages.add(574,GetCardinalPluralCategory_SG);
  FCardinalPluralCategoryFuncByLanguages.add(575,GetCardinalPluralCategory_SH);
  FCardinalPluralCategoryFuncByLanguages.add(13358,GetCardinalPluralCategory_SHI);
  FCardinalPluralCategoryFuncByLanguages.add(576,GetCardinalPluralCategory_SI);
  FCardinalPluralCategoryFuncByLanguages.add(578,GetCardinalPluralCategory_SK);
  FCardinalPluralCategoryFuncByLanguages.add(579,GetCardinalPluralCategory_SL);
  FCardinalPluralCategoryFuncByLanguages.add(13480,GetCardinalPluralCategory_SMA);
  FCardinalPluralCategoryFuncByLanguages.add(13488,GetCardinalPluralCategory_SMI);
  FCardinalPluralCategoryFuncByLanguages.add(13489,GetCardinalPluralCategory_SMJ);
  FCardinalPluralCategoryFuncByLanguages.add(13493,GetCardinalPluralCategory_SMN);
  FCardinalPluralCategoryFuncByLanguages.add(13498,GetCardinalPluralCategory_SMS);
  FCardinalPluralCategoryFuncByLanguages.add(581,GetCardinalPluralCategory_SN);
  FCardinalPluralCategoryFuncByLanguages.add(582,GetCardinalPluralCategory_SO);
  FCardinalPluralCategoryFuncByLanguages.add(584,GetCardinalPluralCategory_SQ);
  FCardinalPluralCategoryFuncByLanguages.add(585,GetCardinalPluralCategory_SR);
  FCardinalPluralCategoryFuncByLanguages.add(586,GetCardinalPluralCategory_SS);
  FCardinalPluralCategoryFuncByLanguages.add(13660,GetCardinalPluralCategory_SSY);
  FCardinalPluralCategoryFuncByLanguages.add(587,GetCardinalPluralCategory_ST);
  FCardinalPluralCategoryFuncByLanguages.add(588,GetCardinalPluralCategory_SU);
  FCardinalPluralCategoryFuncByLanguages.add(589,GetCardinalPluralCategory_SV);
  FCardinalPluralCategoryFuncByLanguages.add(590,GetCardinalPluralCategory_SW);
  FCardinalPluralCategoryFuncByLanguages.add(13809,GetCardinalPluralCategory_SYR);
  FCardinalPluralCategoryFuncByLanguages.add(594,GetCardinalPluralCategory_TA);
  FCardinalPluralCategoryFuncByLanguages.add(598,GetCardinalPluralCategory_TE);
  FCardinalPluralCategoryFuncByLanguages.add(13962,GetCardinalPluralCategory_TEO);
  FCardinalPluralCategoryFuncByLanguages.add(601,GetCardinalPluralCategory_TH);
  FCardinalPluralCategoryFuncByLanguages.add(602,GetCardinalPluralCategory_TI);
  FCardinalPluralCategoryFuncByLanguages.add(14058,GetCardinalPluralCategory_TIG);
  FCardinalPluralCategoryFuncByLanguages.add(604,GetCardinalPluralCategory_TK);
  FCardinalPluralCategoryFuncByLanguages.add(605,GetCardinalPluralCategory_TL);
  FCardinalPluralCategoryFuncByLanguages.add(607,GetCardinalPluralCategory_TN);
  FCardinalPluralCategoryFuncByLanguages.add(608,GetCardinalPluralCategory_TO);
  FCardinalPluralCategoryFuncByLanguages.add(14242,GetCardinalPluralCategory_TPI);
  FCardinalPluralCategoryFuncByLanguages.add(611,GetCardinalPluralCategory_TR);
  FCardinalPluralCategoryFuncByLanguages.add(612,GetCardinalPluralCategory_TS);
  FCardinalPluralCategoryFuncByLanguages.add(14506,GetCardinalPluralCategory_TZM);
  FCardinalPluralCategoryFuncByLanguages.add(626,GetCardinalPluralCategory_UG);
  FCardinalPluralCategoryFuncByLanguages.add(630,GetCardinalPluralCategory_UK);
  FCardinalPluralCategoryFuncByLanguages.add(14861,GetCardinalPluralCategory_UND);
  FCardinalPluralCategoryFuncByLanguages.add(637,GetCardinalPluralCategory_UR);
  FCardinalPluralCategoryFuncByLanguages.add(645,GetCardinalPluralCategory_UZ);
  FCardinalPluralCategoryFuncByLanguages.add(650,GetCardinalPluralCategory_VE);
  FCardinalPluralCategoryFuncByLanguages.add(15302,GetCardinalPluralCategory_VEC);
  FCardinalPluralCategoryFuncByLanguages.add(654,GetCardinalPluralCategory_VI);
  FCardinalPluralCategoryFuncByLanguages.add(660,GetCardinalPluralCategory_VO);
  FCardinalPluralCategoryFuncByLanguages.add(15729,GetCardinalPluralCategory_VUN);
  FCardinalPluralCategoryFuncByLanguages.add(672,GetCardinalPluralCategory_WA);
  FCardinalPluralCategoryFuncByLanguages.add(15876,GetCardinalPluralCategory_WAE);
  FCardinalPluralCategoryFuncByLanguages.add(686,GetCardinalPluralCategory_WO);
  FCardinalPluralCategoryFuncByLanguages.add(705,GetCardinalPluralCategory_XH);
  FCardinalPluralCategoryFuncByLanguages.add(16918,GetCardinalPluralCategory_XOG);
  FCardinalPluralCategoryFuncByLanguages.add(732,GetCardinalPluralCategory_YI);
  FCardinalPluralCategoryFuncByLanguages.add(738,GetCardinalPluralCategory_YO);
  FCardinalPluralCategoryFuncByLanguages.add(17748,GetCardinalPluralCategory_YUE);
  FCardinalPluralCategoryFuncByLanguages.add(757,GetCardinalPluralCategory_ZH);
  FCardinalPluralCategoryFuncByLanguages.add(770,GetCardinalPluralCategory_ZU);
  FOrdinalPluralCategoryFuncByLanguages.add(105,GetOrdinalPluralCategory_AF);
  FOrdinalPluralCategoryFuncByLanguages.add(112,GetOrdinalPluralCategory_AM);
  FOrdinalPluralCategoryFuncByLanguages.add(113,GetOrdinalPluralCategory_AN);
  FOrdinalPluralCategoryFuncByLanguages.add(117,GetOrdinalPluralCategory_AR);
  FOrdinalPluralCategoryFuncByLanguages.add(118,GetOrdinalPluralCategory_AS);
  FOrdinalPluralCategoryFuncByLanguages.add(1487,GetOrdinalPluralCategory_AST);
  FOrdinalPluralCategoryFuncByLanguages.add(125,GetOrdinalPluralCategory_AZ);
  FOrdinalPluralCategoryFuncByLanguages.add(1687,GetOrdinalPluralCategory_BAL);
  FOrdinalPluralCategoryFuncByLanguages.add(130,GetOrdinalPluralCategory_BE);
  FOrdinalPluralCategoryFuncByLanguages.add(132,GetOrdinalPluralCategory_BG);
  FOrdinalPluralCategoryFuncByLanguages.add(1976,GetOrdinalPluralCategory_BLO);
  FOrdinalPluralCategoryFuncByLanguages.add(139,GetOrdinalPluralCategory_BN);
  FOrdinalPluralCategoryFuncByLanguages.add(144,GetOrdinalPluralCategory_BS);
  FOrdinalPluralCategoryFuncByLanguages.add(152,GetOrdinalPluralCategory_CA);
  FOrdinalPluralCategoryFuncByLanguages.add(156,GetOrdinalPluralCategory_CE);
  FOrdinalPluralCategoryFuncByLanguages.add(170,GetOrdinalPluralCategory_CS);
  FOrdinalPluralCategoryFuncByLanguages.add(176,GetOrdinalPluralCategory_CY);
  FOrdinalPluralCategoryFuncByLanguages.add(178,GetOrdinalPluralCategory_DA);
  FOrdinalPluralCategoryFuncByLanguages.add(182,GetOrdinalPluralCategory_DE);
  FOrdinalPluralCategoryFuncByLanguages.add(3497,GetOrdinalPluralCategory_DSB);
  FOrdinalPluralCategoryFuncByLanguages.add(215,GetOrdinalPluralCategory_EL);
  FOrdinalPluralCategoryFuncByLanguages.add(217,GetOrdinalPluralCategory_EN);
  FOrdinalPluralCategoryFuncByLanguages.add(222,GetOrdinalPluralCategory_ES);
  FOrdinalPluralCategoryFuncByLanguages.add(223,GetOrdinalPluralCategory_ET);
  FOrdinalPluralCategoryFuncByLanguages.add(224,GetOrdinalPluralCategory_EU);
  FOrdinalPluralCategoryFuncByLanguages.add(230,GetOrdinalPluralCategory_FA);
  FOrdinalPluralCategoryFuncByLanguages.add(238,GetOrdinalPluralCategory_FI);
  FOrdinalPluralCategoryFuncByLanguages.add(4599,GetOrdinalPluralCategory_FIL);
  FOrdinalPluralCategoryFuncByLanguages.add(247,GetOrdinalPluralCategory_FR);
  FOrdinalPluralCategoryFuncByLanguages.add(254,GetOrdinalPluralCategory_FY);
  FOrdinalPluralCategoryFuncByLanguages.add(256,GetOrdinalPluralCategory_GA);
  FOrdinalPluralCategoryFuncByLanguages.add(259,GetOrdinalPluralCategory_GD);
  FOrdinalPluralCategoryFuncByLanguages.add(267,GetOrdinalPluralCategory_GL);
  FOrdinalPluralCategoryFuncByLanguages.add(5546,GetOrdinalPluralCategory_GSW);
  FOrdinalPluralCategoryFuncByLanguages.add(276,GetOrdinalPluralCategory_GU);
  FOrdinalPluralCategoryFuncByLanguages.add(286,GetOrdinalPluralCategory_HE);
  FOrdinalPluralCategoryFuncByLanguages.add(290,GetOrdinalPluralCategory_HI);
  FOrdinalPluralCategoryFuncByLanguages.add(299,GetOrdinalPluralCategory_HR);
  FOrdinalPluralCategoryFuncByLanguages.add(6201,GetOrdinalPluralCategory_HSB);
  FOrdinalPluralCategoryFuncByLanguages.add(302,GetOrdinalPluralCategory_HU);
  FOrdinalPluralCategoryFuncByLanguages.add(306,GetOrdinalPluralCategory_HY);
  FOrdinalPluralCategoryFuncByLanguages.add(308,GetOrdinalPluralCategory_IA);
  FOrdinalPluralCategoryFuncByLanguages.add(311,GetOrdinalPluralCategory_ID);
  FOrdinalPluralCategoryFuncByLanguages.add(326,GetOrdinalPluralCategory_IS);
  FOrdinalPluralCategoryFuncByLanguages.add(327,GetOrdinalPluralCategory_IT);
  FOrdinalPluralCategoryFuncByLanguages.add(334,GetOrdinalPluralCategory_JA);
  FOrdinalPluralCategoryFuncByLanguages.add(360,GetOrdinalPluralCategory_KA);
  FOrdinalPluralCategoryFuncByLanguages.add(370,GetOrdinalPluralCategory_KK);
  FOrdinalPluralCategoryFuncByLanguages.add(372,GetOrdinalPluralCategory_KM);
  FOrdinalPluralCategoryFuncByLanguages.add(373,GetOrdinalPluralCategory_KN);
  FOrdinalPluralCategoryFuncByLanguages.add(374,GetOrdinalPluralCategory_KO);
  FOrdinalPluralCategoryFuncByLanguages.add(382,GetOrdinalPluralCategory_KW);
  FOrdinalPluralCategoryFuncByLanguages.add(384,GetOrdinalPluralCategory_KY);
  FOrdinalPluralCategoryFuncByLanguages.add(8653,GetOrdinalPluralCategory_LIJ);
  FOrdinalPluralCategoryFuncByLanguages.add(8725,GetOrdinalPluralCategory_LLD);
  FOrdinalPluralCategoryFuncByLanguages.add(400,GetOrdinalPluralCategory_LO);
  FOrdinalPluralCategoryFuncByLanguages.add(405,GetOrdinalPluralCategory_LT);
  FOrdinalPluralCategoryFuncByLanguages.add(407,GetOrdinalPluralCategory_LV);
  FOrdinalPluralCategoryFuncByLanguages.add(422,GetOrdinalPluralCategory_MK);
  FOrdinalPluralCategoryFuncByLanguages.add(423,GetOrdinalPluralCategory_ML);
  FOrdinalPluralCategoryFuncByLanguages.add(425,GetOrdinalPluralCategory_MN);
  FOrdinalPluralCategoryFuncByLanguages.add(426,GetOrdinalPluralCategory_MO);
  FOrdinalPluralCategoryFuncByLanguages.add(429,GetOrdinalPluralCategory_MR);
  FOrdinalPluralCategoryFuncByLanguages.add(430,GetOrdinalPluralCategory_MS);
  FOrdinalPluralCategoryFuncByLanguages.add(436,GetOrdinalPluralCategory_MY);
  FOrdinalPluralCategoryFuncByLanguages.add(439,GetOrdinalPluralCategory_NB);
  FOrdinalPluralCategoryFuncByLanguages.add(442,GetOrdinalPluralCategory_NE);
  FOrdinalPluralCategoryFuncByLanguages.add(449,GetOrdinalPluralCategory_NL);
  FOrdinalPluralCategoryFuncByLanguages.add(452,GetOrdinalPluralCategory_NO);
  FOrdinalPluralCategoryFuncByLanguages.add(481,GetOrdinalPluralCategory_OR);
  FOrdinalPluralCategoryFuncByLanguages.add(490,GetOrdinalPluralCategory_PA);
  FOrdinalPluralCategoryFuncByLanguages.add(501,GetOrdinalPluralCategory_PL);
  FOrdinalPluralCategoryFuncByLanguages.add(11588,GetOrdinalPluralCategory_PRG);
  FOrdinalPluralCategoryFuncByLanguages.add(508,GetOrdinalPluralCategory_PS);
  FOrdinalPluralCategoryFuncByLanguages.add(509,GetOrdinalPluralCategory_PT);
  FOrdinalPluralCategoryFuncByLanguages.add(556,GetOrdinalPluralCategory_RO);
  FOrdinalPluralCategoryFuncByLanguages.add(562,GetOrdinalPluralCategory_RU);
  FOrdinalPluralCategoryFuncByLanguages.add(570,GetOrdinalPluralCategory_SC);
  FOrdinalPluralCategoryFuncByLanguages.add(13233,GetOrdinalPluralCategory_SCN);
  FOrdinalPluralCategoryFuncByLanguages.add(571,GetOrdinalPluralCategory_SD);
  FOrdinalPluralCategoryFuncByLanguages.add(575,GetOrdinalPluralCategory_SH);
  FOrdinalPluralCategoryFuncByLanguages.add(576,GetOrdinalPluralCategory_SI);
  FOrdinalPluralCategoryFuncByLanguages.add(578,GetOrdinalPluralCategory_SK);
  FOrdinalPluralCategoryFuncByLanguages.add(579,GetOrdinalPluralCategory_SL);
  FOrdinalPluralCategoryFuncByLanguages.add(584,GetOrdinalPluralCategory_SQ);
  FOrdinalPluralCategoryFuncByLanguages.add(585,GetOrdinalPluralCategory_SR);
  FOrdinalPluralCategoryFuncByLanguages.add(589,GetOrdinalPluralCategory_SV);
  FOrdinalPluralCategoryFuncByLanguages.add(590,GetOrdinalPluralCategory_SW);
  FOrdinalPluralCategoryFuncByLanguages.add(594,GetOrdinalPluralCategory_TA);
  FOrdinalPluralCategoryFuncByLanguages.add(598,GetOrdinalPluralCategory_TE);
  FOrdinalPluralCategoryFuncByLanguages.add(601,GetOrdinalPluralCategory_TH);
  FOrdinalPluralCategoryFuncByLanguages.add(604,GetOrdinalPluralCategory_TK);
  FOrdinalPluralCategoryFuncByLanguages.add(605,GetOrdinalPluralCategory_TL);
  FOrdinalPluralCategoryFuncByLanguages.add(14242,GetOrdinalPluralCategory_TPI);
  FOrdinalPluralCategoryFuncByLanguages.add(611,GetOrdinalPluralCategory_TR);
  FOrdinalPluralCategoryFuncByLanguages.add(630,GetOrdinalPluralCategory_UK);
  FOrdinalPluralCategoryFuncByLanguages.add(14861,GetOrdinalPluralCategory_UND);
  FOrdinalPluralCategoryFuncByLanguages.add(637,GetOrdinalPluralCategory_UR);
  FOrdinalPluralCategoryFuncByLanguages.add(645,GetOrdinalPluralCategory_UZ);
  FOrdinalPluralCategoryFuncByLanguages.add(15302,GetOrdinalPluralCategory_VEC);
  FOrdinalPluralCategoryFuncByLanguages.add(654,GetOrdinalPluralCategory_VI);
  FOrdinalPluralCategoryFuncByLanguages.add(17748,GetOrdinalPluralCategory_YUE);
  FOrdinalPluralCategoryFuncByLanguages.add(757,GetOrdinalPluralCategory_ZH);
  FOrdinalPluralCategoryFuncByLanguages.add(770,GetOrdinalPluralCategory_ZU);

  {$ENDREGION 'Auto-generated by <ALCINOE>\Tools\CodeBuilder (2)'}

end;

{********************************}
Destructor TALPluralRules.Destroy;
begin
  inherited;
  ALFreeAndNil(FCardinalPluralCategoryFuncByLanguages);
  ALFreeAndNil(FOrdinalPluralCategoryFuncByLanguages);
end;

{*************}
//[MultiThread]
class function TALPluralRules.CreateInstance: TALPluralRules;
begin
  result := TALPluralRules.Create;
end;

{*************}
//[MultiThread]
class function TALPluralRules.GetInstance: TALPluralRules;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance);
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALPluralRules.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{*****************************}
function TALPluralRules.select(
           const AGetPluralCategoryFunc: TGetPluralCategoryFunc;
           const ANumber: Double;
           const AType: TPluralType = TPluralType.Cardinal;
           const AMinFractionDigits: Integer = 0;
           const AExponent: Integer = 0): TPluralCategory;
begin
  // Same params used by FloatToStr
  var LFloatRec: TFloatRec;
  FloatToDecimal(LFloatRec{Result}, ANumber{Value}, fvExtended{ValueType}, 15{Precision}, 9999{Decimals});

  // LFloatRec.Exponent = 2047 ($7FF) => INF
  // LFloatRec.Exponent = 2048 ($7FF + 1) => NAN
  if LFloatRec.Exponent > 15 then
    Exit(TPluralCategory.Other);

  // https://unicode.org/reports/tr35/tr35-numbers.html#table-plural-operand-meanings
  // n: the absolute value of ANumber
  // Ex: 1=1 ; 1.0=1 ; 1.00=1 ; 1.3=1.3 ; 1.30=1.3 ; 1.03=1.03 ; 1.230=1.23 ; 1200000=1200000 ; 1.2c6=1200000 ; 123c6=123000000 ; 123c5=12300000 ; 1200.50=1200.5 ; 1.20050c3=1200.5
  var n: Int64 := Round(ANumber);
  if not SameValue(n, ANumber) then n := -maxint
  else n := abs(n);

  // i: the integer digits of ANumber
  // Ex: 1=1 ; 1.0=1 ; 1.00=1 ; 1.3=1 ; 1.30=1 ; 1.03=1 ; 1.230=1 ; 1200000=1200000 ; 1.2c6=1200000 ; 123c6=123000000 ; 123c5=12300000 ; 1200.50=1200 ; 1.20050c3=1200
  var i: Int64 := Trunc(abs(ANumber));

  // v: the number of visible fraction digits in ANumber, with trailing zeros
  // Ex: 1=0 ; 1.0=1 ; 1.00=2 ; 1.3=1 ; 1.30=2 ; 1.03=2 ; 1.230=3 ; 1200000=0 ; 1.2c6=0 ; 123c6=0 ; 123c5=0 ; 1200.50=2 ; 1.20050c3=2
  var v: integer := 0;
  for var J := low(LFloatRec.Digits) to High(LFloatRec.Digits) do begin
    if (LFloatRec.Digits[J] = 0) then break
    else inc(v);
  end;
  v := max(0, v - LFloatRec.Exponent);

  // t: the visible fraction digits in N, without trailing zeros, expressed as an integer
  // Ex: 1=0 ; 1.0=0 ; 1.00=0 ; 1.3=3 ; 1.30=3 ; 1.03=3 ; 1.230=23 ; 1200000=0 ; 1.2c6=0 ; 123c6=0 ; 123c5=0 ; 1200.50=5 ; 1.20050c3=5
  var t: integer := 0;
  for var J := low(LFloatRec.Digits) + max(0, LFloatRec.Exponent) to High(LFloatRec.Digits) do begin
    if (LFloatRec.Digits[J] = 0) then break
    else t := (t * 10) + (ord(LFloatRec.Digits[J]) - 48{Ord('0')});
  end;

  // f: the visible fraction digits in ANumber, with trailing zeros, expressed as an integer
  // Ex: 1=0 ; 1.0=0 ; 1.00=0 ; 1.3=3 ; 1.30=30 ; 1.03=3 ; 1.230=230 ; 1200000=0 ; 1.2c6=0 ; 123c6=0 ; 123c5=0 ; 1200.50=50 ; 1.20050c3=50
  var f: integer := t;
  For var J := v to AMinFractionDigits - 1 do
    f := f * 10;
  v := max(AMinFractionDigits, v);

  // c: compact decimal exponent value: exponent of the power of 10 used in compact decimal formatting.
  // e: a deprecated synonym for ‘c’. Note: it may be redefined in the future.
  // Ex: 1=0 ; 1.0=0 ; 1.00=0 ; 1.3=0 ; 1.30=0 ; 1.03=0 ; 1.230=0 ; 1200000=0 ; 1.2c6=6 ; 123c6=6 ; 123c5=5 ; 1200.50=0 ; 1.20050c3=3
  //Var e: integer := AExponent;

  // Result
  Result := AGetPluralCategoryFunc(n, i, v, f, t, AExponent{e});
end;

{*****************************}
function TALPluralRules.select(
           const AGetPluralCategoryFunc: TGetPluralCategoryFunc;
           const ANumber: Int64;
           const AType: TPluralType = TPluralType.Cardinal;
           const AExponent: Integer = 0): TPluralCategory;
begin

  // https://unicode.org/reports/tr35/tr35-numbers.html#table-plural-operand-meanings
  // n: the absolute value of ANumber
  // Ex: 1=1 ; 1.0=1 ; 1.00=1 ; 1.3=1.3 ; 1.30=1.3 ; 1.03=1.03 ; 1.230=1.23 ; 1200000=1200000 ; 1.2c6=1200000 ; 123c6=123000000 ; 123c5=12300000 ; 1200.50=1200.5 ; 1.20050c3=1200.5
  var n: Int64 := abs(ANumber);

  // i: the integer digits of ANumber
  // Ex: 1=1 ; 1.0=1 ; 1.00=1 ; 1.3=1 ; 1.30=1 ; 1.03=1 ; 1.230=1 ; 1200000=1200000 ; 1.2c6=1200000 ; 123c6=123000000 ; 123c5=12300000 ; 1200.50=1200 ; 1.20050c3=1200
  //var i: Int64 := n;

  // v: the number of visible fraction digits in ANumber, with trailing zeros
  // Ex: 1=0 ; 1.0=1 ; 1.00=2 ; 1.3=1 ; 1.30=2 ; 1.03=2 ; 1.230=3 ; 1200000=0 ; 1.2c6=0 ; 123c6=0 ; 123c5=0 ; 1200.50=2 ; 1.20050c3=2
  //var v: integer := 0;

  // f: the visible fraction digits in ANumber, with trailing zeros, expressed as an integer
  // Ex: 1=0 ; 1.0=0 ; 1.00=0 ; 1.3=3 ; 1.30=30 ; 1.03=3 ; 1.230=230 ; 1200000=0 ; 1.2c6=0 ; 123c6=0 ; 123c5=0 ; 1200.50=50 ; 1.20050c3=50
  //var f: integer := 0;

  // t: the visible fraction digits in N, without trailing zeros, expressed as an integer
  // Ex: 1=0 ; 1.0=0 ; 1.00=0 ; 1.3=3 ; 1.30=3 ; 1.03=3 ; 1.230=23 ; 1200000=0 ; 1.2c6=0 ; 123c6=0 ; 123c5=0 ; 1200.50=5 ; 1.20050c3=5
  //var t: integer := 0;

  // c: compact decimal exponent value: exponent of the power of 10 used in compact decimal formatting.
  // e: a deprecated synonym for ‘c’. Note: it may be redefined in the future.
  // Ex: 1=0 ; 1.0=0 ; 1.00=0 ; 1.3=0 ; 1.30=0 ; 1.03=0 ; 1.230=0 ; 1200000=0 ; 1.2c6=6 ; 123c6=6 ; 123c5=5 ; 1200.50=0 ; 1.20050c3=3
  //Var e: integer := AExponent;

  Result := AGetPluralCategoryFunc(n{n}, n{i}, 0{v}, 0{f}, 0{t}, AExponent{e});

end;

{*****************************}
function TALPluralRules.select(
           const Alocale: TALLocaleW;
           const ANumber: Double;
           const AType: TPluralType = TPluralType.Cardinal;
           const AMinFractionDigits: Integer = 0;
           const AExponent: Integer = 0): TPluralCategory;
begin
  var LGetPluralCategoryFunc: TGetPluralCategoryFunc;
  case AType of
    TPluralType.Cardinal: begin
      If (Alocale.LanguageID = ALLanguageID_Portuguese) and
         (Alocale.RegionID = ALCountryID_Portugal) then
        LGetPluralCategoryFunc := GetCardinalPluralCategory_PT_PT
      else if not FCardinalPluralCategoryFuncByLanguages.TryGetValue(Alocale.LanguageID, LGetPluralCategoryFunc) then
        exit(TPluralCategory.Other);
    end;
    TPluralType.Ordinal: begin
      If not FOrdinalPluralCategoryFuncByLanguages.TryGetValue(Alocale.LanguageID, LGetPluralCategoryFunc) then
        exit(TPluralCategory.Other);
    end;
    else
      Raise Exception.Create('Error B4C7DF35-9DDA-4D93-88FD-8EB3D6ABF4D2')
  end;
  result := select(
              LGetPluralCategoryFunc, // const AGetPluralCategoryFunc: TGetPluralCategoryFunc;
              ANumber, // const ANumber: Double;
              AType, // const AType: TPluralType = TPluralType.Cardinal;
              AMinFractionDigits, // const AMinFractionDigits: Integer = 0;
              AExponent) // const AExponent: Integer = 0);
end;

{*****************************}
function TALPluralRules.select(
           const Alocale: TALLocaleW;
           const ANumber: Int64;
           const AType: TPluralType = TPluralType.Cardinal;
           const AExponent: Integer = 0): TPluralCategory;
begin
  var LGetPluralCategoryFunc: TGetPluralCategoryFunc;
  case AType of
    TPluralType.Cardinal: begin
      If (Alocale.LanguageID = ALLanguageID_Portuguese) and
         (Alocale.RegionID = ALCountryID_Portugal) then
        LGetPluralCategoryFunc := GetCardinalPluralCategory_PT_PT
      else if not FCardinalPluralCategoryFuncByLanguages.TryGetValue(Alocale.LanguageID, LGetPluralCategoryFunc) then
        exit(TPluralCategory.Other);
    end;
    TPluralType.Ordinal: begin
      If not FOrdinalPluralCategoryFuncByLanguages.TryGetValue(Alocale.LanguageID, LGetPluralCategoryFunc) then
        exit(TPluralCategory.Other);
    end;
    else
      Raise Exception.Create('Error 232ABAFE-73DE-4BF4-B9FF-B7363A604317')
  end;
  result := select(
              LGetPluralCategoryFunc, // const AGetPluralCategoryFunc: TGetPluralCategoryFunc;
              ANumber, // const ANumber: Integer;
              AType, // const AType: TPluralType = TPluralType.Cardinal;
              AExponent) // const AExponent: Integer = 0);
end;

{*****************************}
function TALPluralRules.select(
           const Alocale: TALLocaleA;
           const ANumber: Double;
           const AType: TPluralType = TPluralType.Cardinal;
           const AMinFractionDigits: Integer = 0;
           const AExponent: Integer = 0): TPluralCategory;
begin
  var LGetPluralCategoryFunc: TGetPluralCategoryFunc;
  case AType of
    TPluralType.Cardinal: begin
      If (Alocale.LanguageID = ALLanguageID_Portuguese) and
         (Alocale.RegionID = ALCountryID_Portugal) then
        LGetPluralCategoryFunc := GetCardinalPluralCategory_PT_PT
      else if not FCardinalPluralCategoryFuncByLanguages.TryGetValue(Alocale.LanguageID, LGetPluralCategoryFunc) then
        exit(TPluralCategory.Other);
    end;
    TPluralType.Ordinal: begin
      If not FOrdinalPluralCategoryFuncByLanguages.TryGetValue(Alocale.LanguageID, LGetPluralCategoryFunc) then
        exit(TPluralCategory.Other);
    end;
    else
      Raise Exception.Create('Error 94B3A8A3-9954-4920-88F4-B04EE1B14C2A')
  end;
  result := select(
              LGetPluralCategoryFunc, // const AGetPluralCategoryFunc: TGetPluralCategoryFunc;
              ANumber, // const ANumber: Double;
              AType, // const AType: TPluralType = TPluralType.Cardinal;
              AMinFractionDigits, // const AMinFractionDigits: Integer = 0;
              AExponent) // const AExponent: Integer = 0);
end;

{*****************************}
function TALPluralRules.select(
           const Alocale: TALLocaleA;
           const ANumber: Int64;
           const AType: TPluralType = TPluralType.Cardinal;
           const AExponent: Integer = 0): TPluralCategory;
begin
  var LGetPluralCategoryFunc: TGetPluralCategoryFunc;
  case AType of
    TPluralType.Cardinal: begin
      If (Alocale.LanguageID = ALLanguageID_Portuguese) and
         (Alocale.RegionID = ALCountryID_Portugal) then
        LGetPluralCategoryFunc := GetCardinalPluralCategory_PT_PT
      else if not FCardinalPluralCategoryFuncByLanguages.TryGetValue(Alocale.LanguageID, LGetPluralCategoryFunc) then
        exit(TPluralCategory.Other);
    end;
    TPluralType.Ordinal: begin
      If not FOrdinalPluralCategoryFuncByLanguages.TryGetValue(Alocale.LanguageID, LGetPluralCategoryFunc) then
        exit(TPluralCategory.Other);
    end;
    else
      Raise Exception.Create('Error B6721878-DA63-436E-94CC-6FC8D6F77E65')
  end;
  result := select(
              LGetPluralCategoryFunc, // const AGetPluralCategoryFunc: TGetPluralCategoryFunc;
              ANumber, // const ANumber: Integer;
              AType, // const AType: TPluralType = TPluralType.Cardinal;
              AExponent) // const AExponent: Integer = 0);
end;

{**********************************************************************************************}
class function TALPluralRules.PluralCategoryToStringW(const ACategory: TPluralCategory): String;
begin
  case ACategory of
    TPluralCategory.Zero: Result := 'Zero';
    TPluralCategory.One: Result := 'One';
    TPluralCategory.Two: Result := 'Two';
    TPluralCategory.Few: Result := 'Few';
    TPluralCategory.Many: Result := 'Many';
    TPluralCategory.Other: Result := 'Other';
    else Raise Exception.Create('Error 393D6BEC-BC6A-4C1E-BCF4-D104BC7A4C4E')
  end;
end;

{**************************************************************************************************}
class function TALPluralRules.PluralCategoryToStringA(const ACategory: TPluralCategory): AnsiString;
begin
  case ACategory of
    TPluralCategory.Zero: Result := 'Zero';
    TPluralCategory.One: Result := 'One';
    TPluralCategory.Two: Result := 'Two';
    TPluralCategory.Few: Result := 'Few';
    TPluralCategory.Many: Result := 'Many';
    TPluralCategory.Other: Result := 'Other';
    else Raise Exception.Create('Error 393D6BEC-BC6A-4C1E-BCF4-D104BC7A4C4E')
  end;
end;

{$REGION 'Auto-generated by <ALCINOE>\Tools\CodeBuilder (3)'}

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_AF(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_AK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [0..1]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_AM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 0) or (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_AN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_AR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 0) then Result := TPluralCategory.zero
  else if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else if (n mod 100 in [3..10]) then Result := TPluralCategory.few
  else if (n mod 100 in [11..99]) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_ARS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 0) then Result := TPluralCategory.zero
  else if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else if (n mod 100 in [3..10]) then Result := TPluralCategory.few
  else if (n mod 100 in [11..99]) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_AS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 0) or (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_ASA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_AST(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_AZ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_BAL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_BE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n mod 10 = 1) and (n mod 100 <> 11) then Result := TPluralCategory.one
  else if (n mod 10 in [2..4]) and (not (n mod 100 in [12..14])) then Result := TPluralCategory.few
  else if (n mod 10 = 0) or (n mod 10 in [5..9]) or (n mod 100 in [11..14]) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_BEM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_BEZ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_BG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_BHO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [0..1]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_BLO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 0) then Result := TPluralCategory.zero
  else if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_BM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_BN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 0) or (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_BO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_BR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n mod 10 = 1) and (not (n mod 100 in [11,71,91])) then Result := TPluralCategory.one
  else if (n mod 10 = 2) and (not (n mod 100 in [12,72,92])) then Result := TPluralCategory.two
  else if (n mod 10 in [3..4,9]) and (not (n mod 100 in [10..19,70..79,90..99])) then Result := TPluralCategory.few
  else if (n <> 0) and (n mod 1000000 = 0) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_BRX(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_BS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (v = 0) and (i mod 10 = 1) and (i mod 100 <> 11) or (f mod 10 = 1) and (f mod 100 <> 11) then Result := TPluralCategory.one
  else if (v = 0) and (i mod 10 in [2..4]) and (not (i mod 100 in [12..14])) or (f mod 10 in [2..4]) and (not (f mod 100 in [12..14])) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_CA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else if (e = 0) and (i <> 0) and (i mod 1000000 = 0) and (v = 0) or (not (e in [0..5])) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_CE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_CEB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (v = 0) and (i in [1,2,3]) or (v = 0) and (not (i mod 10 in [4,6,9])) or (v <> 0) and (not (f mod 10 in [4,6,9])) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_CGG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_CHR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_CKB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_CS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else if (i in [2..4]) and (v = 0) then Result := TPluralCategory.few
  else if (v <> 0) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_CSW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [0..1]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_CY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 0) then Result := TPluralCategory.zero
  else if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else if (n = 3) then Result := TPluralCategory.few
  else if (n = 6) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_DA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) or (t <> 0) and (i in [0,1]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_DE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_DOI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 0) or (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_DSB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (v = 0) and (i mod 100 = 1) or (f mod 100 = 1) then Result := TPluralCategory.one
  else if (v = 0) and (i mod 100 = 2) or (f mod 100 = 2) then Result := TPluralCategory.two
  else if (v = 0) and (i mod 100 in [3..4]) or (f mod 100 in [3..4]) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_DV(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_DZ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_EE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_EL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_EN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_EO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_ES(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (e = 0) and (i <> 0) and (i mod 1000000 = 0) and (v = 0) or (not (e in [0..5])) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_ET(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_EU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_FA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 0) or (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_FF(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i in [0,1]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_FI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_FIL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (v = 0) and (i in [1,2,3]) or (v = 0) and (not (i mod 10 in [4,6,9])) or (v <> 0) and (not (f mod 10 in [4,6,9])) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_FO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_FR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i in [0,1]) then Result := TPluralCategory.one
  else if (e = 0) and (i <> 0) and (i mod 1000000 = 0) and (v = 0) or (not (e in [0..5])) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_FUR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_FY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_GA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else if (n in [3..6]) then Result := TPluralCategory.few
  else if (n in [7..10]) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_GD(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [1,11]) then Result := TPluralCategory.one
  else if (n in [2,12]) then Result := TPluralCategory.two
  else if (n in [3..10,13..19]) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_GL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_GSW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_GU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 0) or (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_GUW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [0..1]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_GV(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (v = 0) and (i mod 10 = 1) then Result := TPluralCategory.one
  else if (v = 0) and (i mod 10 = 2) then Result := TPluralCategory.two
  else if (v = 0) and (i mod 100 in [0,20,40,60,80]) then Result := TPluralCategory.few
  else if (v <> 0) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_HA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_HAW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_HE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) or (i = 0) and (v <> 0) then Result := TPluralCategory.one
  else if (i = 2) and (v = 0) then Result := TPluralCategory.two
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_HI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 0) or (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_HNJ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_HR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (v = 0) and (i mod 10 = 1) and (i mod 100 <> 11) or (f mod 10 = 1) and (f mod 100 <> 11) then Result := TPluralCategory.one
  else if (v = 0) and (i mod 10 in [2..4]) and (not (i mod 100 in [12..14])) or (f mod 10 in [2..4]) and (not (f mod 100 in [12..14])) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_HSB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (v = 0) and (i mod 100 = 1) or (f mod 100 = 1) then Result := TPluralCategory.one
  else if (v = 0) and (i mod 100 = 2) or (f mod 100 = 2) then Result := TPluralCategory.two
  else if (v = 0) and (i mod 100 in [3..4]) or (f mod 100 in [3..4]) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_HU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_HY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i in [0,1]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_IA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_ID(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_IG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_II(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_IO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_IS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (t = 0) and (i mod 10 = 1) and (i mod 100 <> 11) or (t mod 10 = 1) and (t mod 100 <> 11) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_IT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else if (e = 0) and (i <> 0) and (i mod 1000000 = 0) and (v = 0) or (not (e in [0..5])) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_IU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_JA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_JBO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_JGO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_JMC(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_JV(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_JW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KAB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i in [0,1]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KAJ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KCG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KDE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KEA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KKJ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 0) or (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KSB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KSH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 0) then Result := TPluralCategory.zero
  else if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 0) then Result := TPluralCategory.zero
  else if (n = 1) then Result := TPluralCategory.one
  else if (n mod 100 in [2,22,42,62,82]) or (n mod 1000 = 0) and {(n mod 100000 in [1000..20000,40000,60000,80000])}(((n mod 100000 >= 1000) and (n mod 100000 <= 20000)) or (n mod 100000 = 40000) or (n mod 100000 = 60000) or (n mod 100000 = 80000)) or (n <> 0) and (n mod 1000000 = 100000) then Result := TPluralCategory.two
  else if (n mod 100 in [3,23,43,63,83]) then Result := TPluralCategory.few
  else if (n <> 1) and (n mod 100 in [1,21,41,61,81]) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_KY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_LAG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 0) then Result := TPluralCategory.zero
  else if (i in [0,1]) and (n <> 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_LB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_LG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_LIJ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_LKT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_LLD(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else if (e = 0) and (i <> 0) and (i mod 1000000 = 0) and (v = 0) or (not (e in [0..5])) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_LN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [0..1]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_LO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_LT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n mod 10 = 1) and (not (n mod 100 in [11..19])) then Result := TPluralCategory.one
  else if (n mod 10 in [2..9]) and (not (n mod 100 in [11..19])) then Result := TPluralCategory.few
  else if (f <> 0) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_LV(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n mod 10 = 0) or (n mod 100 in [11..19]) or (v = 2) and (f mod 100 in [11..19]) then Result := TPluralCategory.zero
  else if (n mod 10 = 1) and (n mod 100 <> 11) or (v = 2) and (f mod 10 = 1) and (f mod 100 <> 11) or (v <> 2) and (f mod 10 = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_MAS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_MG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [0..1]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_MGO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_MK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (v = 0) and (i mod 10 = 1) and (i mod 100 <> 11) or (f mod 10 = 1) and (f mod 100 <> 11) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_ML(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_MN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_MO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else if (v <> 0) or (n = 0) or (n <> 1) and (n mod 100 in [1..19]) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_MR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_MS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_MT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else if (n = 0) or (n mod 100 in [3..10]) then Result := TPluralCategory.few
  else if (n mod 100 in [11..19]) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_MY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_NAH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_NAQ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_NB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_ND(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_NE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_NL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_NN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_NNH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_NO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_NQO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_NR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_NSO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [0..1]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_NY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_NYN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_OM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_OR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_OS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_OSA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_PA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [0..1]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_PAP(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_PCM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 0) or (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_PL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else if (v = 0) and (i mod 10 in [2..4]) and (not (i mod 100 in [12..14])) then Result := TPluralCategory.few
  else if (v = 0) and (i <> 1) and (i mod 10 in [0..1]) or (v = 0) and (i mod 10 in [5..9]) or (v = 0) and (i mod 100 in [12..14]) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_PRG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n mod 10 = 0) or (n mod 100 in [11..19]) or (v = 2) and (f mod 100 in [11..19]) then Result := TPluralCategory.zero
  else if (n mod 10 = 1) and (n mod 100 <> 11) or (v = 2) and (f mod 10 = 1) and (f mod 100 <> 11) or (v <> 2) and (f mod 10 = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_PS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_PT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i in [0..1]) then Result := TPluralCategory.one
  else if (e = 0) and (i <> 0) and (i mod 1000000 = 0) and (v = 0) or (not (e in [0..5])) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*************************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_PT_PT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else if (e = 0) and (i <> 0) and (i mod 1000000 = 0) and (v = 0) or (not (e in [0..5])) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_RM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_RO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else if (v <> 0) or (n = 0) or (n <> 1) and (n mod 100 in [1..19]) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_ROF(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_RU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (v = 0) and (i mod 10 = 1) and (i mod 100 <> 11) then Result := TPluralCategory.one
  else if (v = 0) and (i mod 10 in [2..4]) and (not (i mod 100 in [12..14])) then Result := TPluralCategory.few
  else if (v = 0) and (i mod 10 = 0) or (v = 0) and (i mod 10 in [5..9]) or (v = 0) and (i mod 100 in [11..14]) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_RWK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SAH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SAQ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SAT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SC(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SCN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else if (e = 0) and (i <> 0) and (i mod 1000000 = 0) and (v = 0) or (not (e in [0..5])) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SD(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SDH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SEH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SES(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (v = 0) and (i mod 10 = 1) and (i mod 100 <> 11) or (f mod 10 = 1) and (f mod 100 <> 11) then Result := TPluralCategory.one
  else if (v = 0) and (i mod 10 in [2..4]) and (not (i mod 100 in [12..14])) or (f mod 10 in [2..4]) and (not (f mod 100 in [12..14])) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SHI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 0) or (n = 1) then Result := TPluralCategory.one
  else if (n in [2..10]) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [0,1]) or (i = 0) and (f = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else if (i in [2..4]) and (v = 0) then Result := TPluralCategory.few
  else if (v <> 0) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (v = 0) and (i mod 100 = 1) then Result := TPluralCategory.one
  else if (v = 0) and (i mod 100 = 2) then Result := TPluralCategory.two
  else if (v = 0) and (i mod 100 in [3..4]) or (v <> 0) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SMA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SMI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SMJ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SMN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SMS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SQ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (v = 0) and (i mod 10 = 1) and (i mod 100 <> 11) or (f mod 10 = 1) and (f mod 100 <> 11) then Result := TPluralCategory.one
  else if (v = 0) and (i mod 10 in [2..4]) and (not (i mod 100 in [12..14])) or (f mod 10 in [2..4]) and (not (f mod 100 in [12..14])) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SSY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_ST(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SV(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_SYR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_TA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_TE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_TEO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_TH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_TI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [0..1]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_TIG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_TK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_TL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (v = 0) and (i in [1,2,3]) or (v = 0) and (not (i mod 10 in [4,6,9])) or (v <> 0) and (not (f mod 10 in [4,6,9])) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_TN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_TO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_TPI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_TR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_TS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_TZM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [0..1]) or (n in [11..99]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_UG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_UK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (v = 0) and (i mod 10 = 1) and (i mod 100 <> 11) then Result := TPluralCategory.one
  else if (v = 0) and (i mod 10 in [2..4]) and (not (i mod 100 in [12..14])) then Result := TPluralCategory.few
  else if (v = 0) and (i mod 10 = 0) or (v = 0) and (i mod 10 in [5..9]) or (v = 0) and (i mod 100 in [11..14]) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_UND(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_UR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_UZ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_VE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_VEC(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else if (e = 0) and (i <> 0) and (i mod 1000000 = 0) and (v = 0) or (not (e in [0..5])) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_VI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_VO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_VUN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_WA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [0..1]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_WAE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_WO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_XH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_XOG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_YI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) and (v = 0) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_YO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{***********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_YUE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_ZH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetCardinalPluralCategory_ZU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 0) or (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_AF(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_AM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_AN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_AR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_AS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [1,5,7,8,9,10]) then Result := TPluralCategory.one
  else if (n in [2,3]) then Result := TPluralCategory.two
  else if (n = 4) then Result := TPluralCategory.few
  else if (n = 6) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_AST(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_AZ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i mod 10 in [1,2,5,7,8]) or (i mod 100 in [20,50,70,80]) then Result := TPluralCategory.one
  else if (i mod 10 in [3,4]) or {(i mod 1000 in [100,200,300,400,500,600,700,800,900])}((i mod 1000=100) or (i mod 1000=200) or (i mod 1000=300) or (i mod 1000=400) or (i mod 1000=500) or (i mod 1000=600) or (i mod 1000=700) or (i mod 1000=800) or (i mod 1000=900)) then Result := TPluralCategory.few
  else if (i = 0) or (i mod 10 = 6) or (i mod 100 in [40,60,90]) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_BAL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_BE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n mod 10 in [2,3]) and (not (n mod 100 in [12,13])) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_BG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_BLO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 0) then Result := TPluralCategory.zero
  else if (i = 1) then Result := TPluralCategory.one
  else if (i in [2,3,4,5,6]) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_BN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [1,5,7,8,9,10]) then Result := TPluralCategory.one
  else if (n in [2,3]) then Result := TPluralCategory.two
  else if (n = 4) then Result := TPluralCategory.few
  else if (n = 6) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_BS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_CA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [1,3]) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else if (n = 4) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_CE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_CS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_CY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [0,7,8,9]) then Result := TPluralCategory.zero
  else if (n = 1) then Result := TPluralCategory.one
  else if (n = 2) then Result := TPluralCategory.two
  else if (n in [3,4]) then Result := TPluralCategory.few
  else if (n in [5,6]) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_DA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_DE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_DSB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_EL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_EN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n mod 10 = 1) and (n mod 100 <> 11) then Result := TPluralCategory.one
  else if (n mod 10 = 2) and (n mod 100 <> 12) then Result := TPluralCategory.two
  else if (n mod 10 = 3) and (n mod 100 <> 13) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_ES(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_ET(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_EU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_FA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_FI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_FIL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_FR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_FY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_GA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_GD(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [1,11]) then Result := TPluralCategory.one
  else if (n in [2,12]) then Result := TPluralCategory.two
  else if (n in [3,13]) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_GL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_GSW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_GU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n in [2,3]) then Result := TPluralCategory.two
  else if (n = 4) then Result := TPluralCategory.few
  else if (n = 6) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_HE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_HI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n in [2,3]) then Result := TPluralCategory.two
  else if (n = 4) then Result := TPluralCategory.few
  else if (n = 6) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_HR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_HSB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_HU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [1,5]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_HY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_IA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_ID(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_IS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_IT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if {(n in [11,8,80,800])}((n=11) or (n=8) or (n=80) or (n=800)) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_JA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_KA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i = 1) then Result := TPluralCategory.one
  else if (i = 0) or (i mod 100 in [2..20,40,60,80]) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_KK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n mod 10 = 6) or (n mod 10 = 9) or (n mod 10 = 0) and (n <> 0) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_KM(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_KN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_KO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_KW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [1..4]) or (n mod 100 in [1..4,21..24,41..44,61..64,81..84]) then Result := TPluralCategory.one
  else if (n = 5) or (n mod 100 = 5) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_KY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_LIJ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if {(n in [11,8,80..89,800..899])}((n=11) or (n=8) or ((n >= 80) and (n <= 89)) or ((n >= 800) and (n <= 899))) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_LLD(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if {(n in [11,8,80,800])}((n=11) or (n=8) or (n=80) or (n=800)) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_LO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_LT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_LV(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_MK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (i mod 10 = 1) and (i mod 100 <> 11) then Result := TPluralCategory.one
  else if (i mod 10 = 2) and (i mod 100 <> 12) then Result := TPluralCategory.two
  else if (i mod 10 in [7,8]) and (not (i mod 100 in [17,18])) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_ML(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_MN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_MO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_MR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n in [2,3]) then Result := TPluralCategory.two
  else if (n = 4) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_MS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_MY(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_NB(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_NE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [1..4]) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_NL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_NO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_OR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n in [1,5,7..9]) then Result := TPluralCategory.one
  else if (n in [2,3]) then Result := TPluralCategory.two
  else if (n = 4) then Result := TPluralCategory.few
  else if (n = 6) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_PA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_PL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_PRG(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_PS(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_PT(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_RO(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_RU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_SC(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if {(n in [11,8,80,800])}((n=11) or (n=8) or (n=80) or (n=800)) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_SCN(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if {(n in [11,8,80,800])}((n=11) or (n=8) or (n=80) or (n=800)) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_SD(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_SH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_SI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_SK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_SL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_SQ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else if (n mod 10 = 4) and (n mod 100 <> 14) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_SR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_SV(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n mod 10 in [1,2]) and (not (n mod 100 in [11,12])) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_SW(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_TA(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_TE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_TH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_TK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n mod 10 in [6,9]) or (n = 10) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_TL(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_TPI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_TR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_UK(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n mod 10 = 3) and (n mod 100 <> 13) then Result := TPluralCategory.few
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_UND(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_UR(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_UZ(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_VEC(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if {(n in [11,8,80,800])}((n=11) or (n=8) or (n=80) or (n=800)) then Result := TPluralCategory.many
  else Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_VI(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  if (n = 1) then Result := TPluralCategory.one
  else Result := TPluralCategory.other;
end;

{**********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_YUE(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_ZH(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{*********************************************************************************************************}
function TALPluralRules.GetOrdinalPluralCategory_ZU(const n, i: Int64; v, f, t, e: Int32): TPluralCategory;
begin
  Result := TPluralCategory.other;
end;

{$ENDREGION 'Auto-generated by <ALCINOE>\Tools\CodeBuilder (3)'}

{***********************************************}
function ALGetPreferredLanguages: TArray<String>;
begin

  SetLength(Result, 0);

  {$IF defined(MSWindows)}
  var LLocaleName: array[0..LOCALE_NAME_MAX_LENGTH - 1] of WideChar;
  if GetUserDefaultLocaleName(LLocaleName, LOCALE_NAME_MAX_LENGTH) > 0 then begin
    SetLength(Result, 1);
    Result[0] := LLocaleName;
  end;
  {$ENDIF}

  {$IF defined(ANDROID)}
  if TOSVersion.Check(7, 0) {API level >= 24 (Android N)} then begin
    var LLocaleList := TJLocaleList.JavaClass.getDefault;
    SetLength(Result, LLocaleList.size);
    for var I := 0 to LLocaleList.size - 1 do
      Result[I] := JStringToString(LLocaleList.get(I).toLanguageTag);
  end
  else begin
    var LLanguageTag := JStringToString(TJLocale.JavaClass.getDefault.toLanguageTag);
    if LLanguageTag <> '' then begin
      SetLength(Result, 1);
      Result[0] := LLanguageTag;
    end;
  end;
  {$ENDIF}

  {$IF defined(IOS)}
  var LPreferredLanguages := TNSLocale.OCClass.preferredLanguages;
  if (LPreferredLanguages <> nil) and (LPreferredLanguages.count > 0) then begin
    SetLength(Result, LPreferredLanguages.count);
    For var I := 0 to LPreferredLanguages.count - 1 do
      Result[I] := NSStrToStr(TNSString.Wrap(LPreferredLanguages.objectAtIndex(I)));
  end;
  {$ENDIF}

  {$IF defined(ALMacOS)}
  var LPreferredLanguages := TNSLocale.OCClass.preferredLanguages;
  if (LPreferredLanguages <> nil) and (LPreferredLanguages.count > 0) then begin
    SetLength(Result, LPreferredLanguages.count);
    For var I := 0 to LPreferredLanguages.count - 1 do
      Result[I] := NSStrToStr(TNSString.Wrap(LPreferredLanguages.objectAtIndex(I)));
  end;
  {$ENDIF}

end;

{****************************************************************}
function ALLanguageCodeToID(const ALanguageCode: string): Integer;
begin
  case Length(ALanguageCode) of
    2: Result :=
         (((Ord(ALanguageCode[low(ALanguageCode)]) or $20) - $61{Ord('a')}) * 26) +
         ((Ord(ALanguageCode[low(ALanguageCode)+1]) or $20) - $61{Ord('a')}) +
         100; // Offset by +101 to disambiguate from 1-letter codes
    3: Result :=
         (((Ord(ALanguageCode[low(ALanguageCode)]) or $20) - $61{Ord('a')}) * 26 * 26) +
         (((Ord(ALanguageCode[low(ALanguageCode)+1]) or $20) - $61{Ord('a')}) * 26) +
         ((Ord(ALanguageCode[low(ALanguageCode)+2]) or $20) - $61{Ord('a')}) +
         1000; // Offset by +1001 to disambiguate from 2-letter codes
  else
    Result := 0;
  end;
end;

{******************************************************************************}
function ALLanguageCodeToID(const ALanguageCode: Ansistring): Integer; overload;
begin
  case Length(ALanguageCode) of
    2: Result :=
         (((Ord(ALanguageCode[low(ALanguageCode)]) or $20) - $61{Ord('a')}) * 26) +
         ((Ord(ALanguageCode[low(ALanguageCode)+1]) or $20) - $61{Ord('a')}) +
         100; // Offset by +101 to disambiguate from 1-letter codes
    3: Result :=
         (((Ord(ALanguageCode[low(ALanguageCode)]) or $20) - $61{Ord('a')}) * 26 * 26) +
         (((Ord(ALanguageCode[low(ALanguageCode)+1]) or $20) - $61{Ord('a')}) * 26) +
         ((Ord(ALanguageCode[low(ALanguageCode)+2]) or $20) - $61{Ord('a')}) +
         1000; // Offset by +1001 to disambiguate from 2-letter codes
  else
    Result := 0;
  end;
end;

{**********************************************************************}
function ALScriptCodeToID(const AScriptCode: string): Integer; overload;
begin
  if Length(AScriptCode) = 4 then
    Result :=
      (((Ord(AScriptCode[low(AScriptCode)]) or $20) - $61{Ord('a')}) * 26 * 26 * 26) +
      (((Ord(AScriptCode[low(AScriptCode)+1]) or $20) - $61{Ord('a')}) * 26 * 26) +
      (((Ord(AScriptCode[low(AScriptCode)+2]) or $20) - $61{Ord('a')}) * 26) +
      ((Ord(AScriptCode[low(AScriptCode)+3]) or $20) - $61{Ord('a')}) +
      20000 // Offset to avoid collision with 3-letter codes
  else
    Result := 0;
end;

{**************************************************************************}
function ALScriptCodeToID(const AScriptCode: Ansistring): Integer; overload;
begin
  if Length(AScriptCode) = 4 then
    Result :=
      (((Ord(AScriptCode[low(AScriptCode)]) or $20) - $61{Ord('a')}) * 26 * 26 * 26) +
      (((Ord(AScriptCode[low(AScriptCode)+1]) or $20) - $61{Ord('a')}) * 26 * 26) +
      (((Ord(AScriptCode[low(AScriptCode)+2]) or $20) - $61{Ord('a')}) * 26) +
      ((Ord(AScriptCode[low(AScriptCode)+3]) or $20) - $61{Ord('a')}) +
      20000 // Offset to avoid collision with 3-letter codes
  else
    Result := 0;
end;

{**********************************************************************}
function ALRegionCodeToID(const ARegionCode: string): Integer; overload;
begin
  case Length(ARegionCode) of
    2: Result :=
         (((Ord(ARegionCode[low(ARegionCode)]) or $20) - $61{Ord('a')}) * 26) +
         ((Ord(ARegionCode[low(ARegionCode)+1]) or $20) - $61{Ord('a')}) +
         100; // Offset by +101 to disambiguate from 1-letter codes
    3: Result :=
         ((Ord(ARegionCode[low(ARegionCode)]) - $30{Ord('0')}) * 10 * 10) +
         ((Ord(ARegionCode[low(ARegionCode)+1]) - $30{Ord('0')}) * 10) +
         (Ord(ARegionCode[low(ARegionCode)+2]) - $30{Ord('0')}) +
         1000; // Offset by +1001 to disambiguate from 2-letter codes
  else
    Result := 0;
  end;
end;

{**************************************************************************}
function ALRegionCodeToID(const ARegionCode: Ansistring): Integer; overload;
begin
  case Length(ARegionCode) of
    2: Result :=
         (((Ord(ARegionCode[low(ARegionCode)]) or $20) - $61{Ord('a')}) * 26) +
         ((Ord(ARegionCode[low(ARegionCode)+1]) or $20) - $61{Ord('a')}) +
         100; // Offset by +101 to disambiguate from 1-letter codes
    3: Result :=
         ((Ord(ARegionCode[low(ARegionCode)]) - $30{Ord('0')}) * 10 * 10) +
         ((Ord(ARegionCode[low(ARegionCode)+1]) - $30{Ord('0')}) * 10) +
         (Ord(ARegionCode[low(ARegionCode)+2]) - $30{Ord('0')}) +
         1000; // Offset by +1001 to disambiguate from 2-letter codes
  else
    Result := 0;
  end;
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.Localization','initialization');
  {$ENDIF}

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

  TALPluralRules.FInstance := nil;
  TALPluralRules.CreateInstanceFunc := @TALPluralRules.CreateInstance;

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.Localization','finalization');
  {$ENDIF}

  ALFreeAndNil(TALPluralRules.FInstance);

end.
