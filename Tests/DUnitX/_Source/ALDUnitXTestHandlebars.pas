unit ALDUnitXTestHandlebars;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.Types,
  System.Generics.Collections,
  Alcinoe.Common,
  Alcinoe.StringUtils,
  Alcinoe.JSONDoc,
  Alcinoe.Handlebars,
  DUnitX.TestFramework;

type

  [TestFixture]
  TALDUnitXTestHandlebars = class
  strict private
    FHandlebars: TALHandlebars;
    procedure CheckEq(const AExpected, AActual: AnsiString; const AMsg: String = '');
    function Render(const AFilename: AnsiString; const AJson: AnsiString): AnsiString;
    function HelperUpper(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function HelperLower(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function HelperWrap(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    (*$REGION 'Plain mustache {{ }}'*)
    [Test] procedure Test_Mustache_NoTags;
    [Test] procedure Test_Mustache_SimpleVar;
    [Test] procedure Test_Mustache_VarInText;
    [Test] procedure Test_Mustache_MultipleVars;
    [Test] procedure Test_Mustache_MissingVar;
    [Test] procedure Test_Mustache_DottedPath;
    [Test] procedure Test_Mustache_NumberValue;
    [Test] procedure Test_Mustache_BooleanValue;
    (*$ENDREGION*)

    {$REGION 'Helpers (registered)'}
    [Test] procedure Test_Helper_SingleArg_ContextVar;
    [Test] procedure Test_Helper_SingleArg_QuotedLiteral;
    [Test] procedure Test_Helper_Lower;
    [Test] procedure Test_Helper_Concat_TwoVars;
    [Test] procedure Test_Helper_Concat_MixedLiteralAndVar;
    [Test] procedure Test_Helper_Wrap_PositionalArgs;
    {$ENDREGION}

    {$REGION 'Subexpressions ( )'}
    [Test] procedure Test_Subexpr_Simple;
    [Test] procedure Test_Subexpr_WithContextVars;
    [Test] procedure Test_Subexpr_AsOnlyArg;
    [Test] procedure Test_Subexpr_Nested;
    [Test] procedure Test_Subexpr_DeepNested;
    [Test] procedure Test_Subexpr_MultipleSubexprArgs;
    [Test] procedure Test_Subexpr_MixedLiteralAndVar;
    [Test] procedure Test_Subexpr_TwoSubexprArgs;
    [Test] procedure Test_Subexpr_LiteralSpaceInside;
    [Test] procedure Test_Subexpr_LiteralWithEquals;
    {$ENDREGION}

    {$REGION 'Quotes / literals'}
    [Test] procedure Test_Quote_LiteralWithSpaces;
    [Test] procedure Test_Quote_SingleQuotes;
    [Test] procedure Test_Quote_ParensAreLiteral;
    [Test] procedure Test_Quote_ClosingBracesAreLiteral;
    [Test] procedure Test_Quote_SubexprInsideQuoteIsLiteral;
    {$ENDREGION}

    (*$REGION 'Blocks {{#x}}..{{/x}}'*)
    [Test] procedure Test_Block_If_True;
    [Test] procedure Test_Block_If_False;
    [Test] procedure Test_Block_If_NoElse_True;
    [Test] procedure Test_Block_If_NoElse_False;
    [Test] procedure Test_Block_Unless_True;
    [Test] procedure Test_Block_Each;
    [Test] procedure Test_Block_Each_Empty;
    [Test] procedure Test_Block_With;
    [Test] procedure Test_Block_WithMultiField;
    [Test] procedure Test_Block_Nested;
    [Test] procedure Test_Block_NestedElse;
    [Test] procedure Test_Block_HelperInsideBlock;
    (*$ENDREGION*)

    (*$REGION 'Partials {{> x}}'*)
    [Test] procedure Test_Partial_Plain;
    [Test] procedure Test_Partial_WithVar;
    [Test] procedure Test_Partial_WithHelper;
    (*$ENDREGION*)

    {$REGION 'Kitchen sink'}
    [Test] procedure Test_KitchenSink;
    {$ENDREGION}

    {$REGION 'Additional edge cases'}
    [Test] procedure Test_Path_SlashAndCurrentAliases;
    [Test] procedure Test_Truthy_NullFalse;
    [Test] procedure Test_Truthy_EmptyObjectTrue;
    [Test] procedure Test_Truthy_NonEmptyArrayTrue;
    [Test] procedure Test_Truthy_FloatZeroFalse;
    [Test] procedure Test_Block_If_IncludeZero;
    [Test] procedure Test_Block_Unless_IncludeZero;
    [Test] procedure Test_Block_If_NullLiteral;
    [Test] procedure Test_Block_With_ElseForMissing;
    [Test] procedure Test_Block_With_ElseForEmptyArray;
    [Test] procedure Test_Block_With_ScalarContext;
    [Test] procedure Test_Block_Each_ElseForMissing;
    [Test] procedure Test_Block_Each_ElseForScalar;
    [Test] procedure Test_Block_Each_LookupSubexpression;
    [Test] procedure Test_Lookup_MissingOutOfRangeAndInvalidTargets;
    [Test] procedure Test_Lookup_LiteralKeysWithDotsAndSlashes;
    [Test] procedure Test_Lookup_KeyDataVariableAsParam;
    [Test] procedure Test_Literals_NullNegativeAndInt64;
    [Test] procedure Test_Quote_EscapedQuoteChars;
    [Test] procedure Test_Partial_HashBorrowedValueAndMissingDeletesField;
    [Test] procedure Test_Partial_HashSubexpressions;
    [Test] procedure Test_Partial_NonObjectContextWithHash;
    {$ENDREGION}

    {$REGION 'Data-driven render tests'}
    [Test] procedure Test_cmp_gt_blk_001;
    [Test] procedure Test_cmp_gt_inl_001;
    [Test] procedure Test_cmp_gt_blk_002;
    [Test] procedure Test_cmp_gt_inl_002;
    [Test] procedure Test_cmp_gt_blk_003;
    [Test] procedure Test_cmp_gt_inl_003;
    [Test] procedure Test_cmp_gt_blk_004;
    [Test] procedure Test_cmp_gt_inl_004;
    [Test] procedure Test_cmp_gt_blk_005;
    [Test] procedure Test_cmp_gt_inl_005;
    [Test] procedure Test_cmp_gt_blk_006;
    [Test] procedure Test_cmp_gt_inl_006;
    [Test] procedure Test_cmp_gt_blk_007;
    [Test] procedure Test_cmp_gt_inl_007;
    [Test] procedure Test_cmp_gt_blk_008;
    [Test] procedure Test_cmp_gt_inl_008;
    [Test] procedure Test_cmp_gt_blk_009;
    [Test] procedure Test_cmp_gt_inl_009;
    [Test] procedure Test_cmp_gt_blk_010;
    [Test] procedure Test_cmp_gt_inl_010;
    [Test] procedure Test_cmp_gt_blk_011;
    [Test] procedure Test_cmp_gt_inl_011;
    [Test] procedure Test_cmp_gt_blk_012;
    [Test] procedure Test_cmp_gt_inl_012;
    [Test] procedure Test_cmp_gt_blk_013;
    [Test] procedure Test_cmp_gt_inl_013;
    [Test] procedure Test_cmp_gt_blk_014;
    [Test] procedure Test_cmp_gt_inl_014;
    [Test] procedure Test_cmp_gt_blk_015;
    [Test] procedure Test_cmp_gt_inl_015;
    [Test] procedure Test_cmp_gt_blk_016;
    [Test] procedure Test_cmp_gt_inl_016;
    [Test] procedure Test_cmp_gt_blk_017;
    [Test] procedure Test_cmp_gt_inl_017;
    [Test] procedure Test_cmp_gt_blk_018;
    [Test] procedure Test_cmp_gt_inl_018;
    [Test] procedure Test_cmp_gt_blk_019;
    [Test] procedure Test_cmp_gt_inl_019;
    [Test] procedure Test_cmp_gt_blk_020;
    [Test] procedure Test_cmp_gt_inl_020;
    [Test] procedure Test_cmp_gt_blk_021;
    [Test] procedure Test_cmp_gt_inl_021;
    [Test] procedure Test_cmp_gt_blk_022;
    [Test] procedure Test_cmp_gt_inl_022;
    [Test] procedure Test_cmp_gt_blk_023;
    [Test] procedure Test_cmp_gt_inl_023;
    [Test] procedure Test_cmp_gt_blk_024;
    [Test] procedure Test_cmp_gt_inl_024;
    [Test] procedure Test_cmp_gt_blk_025;
    [Test] procedure Test_cmp_gt_inl_025;
    [Test] procedure Test_cmp_gt_blk_026;
    [Test] procedure Test_cmp_gt_inl_026;
    [Test] procedure Test_cmp_gt_blk_027;
    [Test] procedure Test_cmp_gt_inl_027;
    [Test] procedure Test_cmp_gt_blk_028;
    [Test] procedure Test_cmp_gt_inl_028;
    [Test] procedure Test_cmp_gt_blk_029;
    [Test] procedure Test_cmp_gt_inl_029;
    [Test] procedure Test_cmp_gt_blk_030;
    [Test] procedure Test_cmp_gt_inl_030;
    [Test] procedure Test_cmp_gt_blk_031;
    [Test] procedure Test_cmp_gt_inl_031;
    [Test] procedure Test_cmp_gt_blk_032;
    [Test] procedure Test_cmp_gt_inl_032;
    [Test] procedure Test_cmp_gt_blk_033;
    [Test] procedure Test_cmp_gt_inl_033;
    [Test] procedure Test_cmp_gt_blk_034;
    [Test] procedure Test_cmp_gt_inl_034;
    [Test] procedure Test_cmp_gt_blk_035;
    [Test] procedure Test_cmp_gt_inl_035;
    [Test] procedure Test_cmp_gt_blk_036;
    [Test] procedure Test_cmp_gt_inl_036;
    [Test] procedure Test_cmp_gt_blk_037;
    [Test] procedure Test_cmp_gt_inl_037;
    [Test] procedure Test_cmp_gt_blk_038;
    [Test] procedure Test_cmp_gt_inl_038;
    [Test] procedure Test_cmp_gt_blk_039;
    [Test] procedure Test_cmp_gt_inl_039;
    [Test] procedure Test_cmp_gt_blk_040;
    [Test] procedure Test_cmp_gt_inl_040;
    [Test] procedure Test_cmp_gt_blk_041;
    [Test] procedure Test_cmp_gt_inl_041;
    [Test] procedure Test_cmp_gt_blk_042;
    [Test] procedure Test_cmp_gt_inl_042;
    [Test] procedure Test_cmp_gt_blk_043;
    [Test] procedure Test_cmp_gt_inl_043;
    [Test] procedure Test_cmp_gt_blk_044;
    [Test] procedure Test_cmp_gt_inl_044;
    [Test] procedure Test_cmp_gt_blk_045;
    [Test] procedure Test_cmp_gt_inl_045;
    [Test] procedure Test_cmp_gt_blk_046;
    [Test] procedure Test_cmp_gt_inl_046;
    [Test] procedure Test_cmp_gt_blk_047;
    [Test] procedure Test_cmp_gt_inl_047;
    [Test] procedure Test_cmp_gt_blk_048;
    [Test] procedure Test_cmp_gt_inl_048;
    [Test] procedure Test_cmp_gt_blk_049;
    [Test] procedure Test_cmp_gt_inl_049;
    [Test] procedure Test_cmp_gt_blk_050;
    [Test] procedure Test_cmp_gt_inl_050;
    [Test] procedure Test_cmp_gt_blk_051;
    [Test] procedure Test_cmp_gt_inl_051;
    [Test] procedure Test_cmp_gt_blk_052;
    [Test] procedure Test_cmp_gt_inl_052;
    [Test] procedure Test_cmp_gt_blk_053;
    [Test] procedure Test_cmp_gt_inl_053;
    [Test] procedure Test_cmp_gt_blk_054;
    [Test] procedure Test_cmp_gt_inl_054;
    [Test] procedure Test_cmp_gt_blk_055;
    [Test] procedure Test_cmp_gt_inl_055;
    [Test] procedure Test_cmp_gt_blk_056;
    [Test] procedure Test_cmp_gt_inl_056;
    [Test] procedure Test_cmp_gt_blk_057;
    [Test] procedure Test_cmp_gt_inl_057;
    [Test] procedure Test_cmp_gt_blk_058;
    [Test] procedure Test_cmp_gt_inl_058;
    [Test] procedure Test_cmp_gt_blk_059;
    [Test] procedure Test_cmp_gt_inl_059;
    [Test] procedure Test_cmp_gt_blk_060;
    [Test] procedure Test_cmp_gt_inl_060;
    [Test] procedure Test_cmp_gt_blk_061;
    [Test] procedure Test_cmp_gt_inl_061;
    [Test] procedure Test_cmp_gt_blk_062;
    [Test] procedure Test_cmp_gt_inl_062;
    [Test] procedure Test_cmp_gt_blk_063;
    [Test] procedure Test_cmp_gt_inl_063;
    [Test] procedure Test_cmp_gt_blk_064;
    [Test] procedure Test_cmp_gt_inl_064;
    [Test] procedure Test_cmp_lt_blk_065;
    [Test] procedure Test_cmp_lt_inl_065;
    [Test] procedure Test_cmp_lt_blk_066;
    [Test] procedure Test_cmp_lt_inl_066;
    [Test] procedure Test_cmp_lt_blk_067;
    [Test] procedure Test_cmp_lt_inl_067;
    [Test] procedure Test_cmp_lt_blk_068;
    [Test] procedure Test_cmp_lt_inl_068;
    [Test] procedure Test_cmp_lt_blk_069;
    [Test] procedure Test_cmp_lt_inl_069;
    [Test] procedure Test_cmp_lt_blk_070;
    [Test] procedure Test_cmp_lt_inl_070;
    [Test] procedure Test_cmp_lt_blk_071;
    [Test] procedure Test_cmp_lt_inl_071;
    [Test] procedure Test_cmp_lt_blk_072;
    [Test] procedure Test_cmp_lt_inl_072;
    [Test] procedure Test_cmp_lt_blk_073;
    [Test] procedure Test_cmp_lt_inl_073;
    [Test] procedure Test_cmp_lt_blk_074;
    [Test] procedure Test_cmp_lt_inl_074;
    [Test] procedure Test_cmp_lt_blk_075;
    [Test] procedure Test_cmp_lt_inl_075;
    [Test] procedure Test_cmp_lt_blk_076;
    [Test] procedure Test_cmp_lt_inl_076;
    [Test] procedure Test_cmp_lt_blk_077;
    [Test] procedure Test_cmp_lt_inl_077;
    [Test] procedure Test_cmp_lt_blk_078;
    [Test] procedure Test_cmp_lt_inl_078;
    [Test] procedure Test_cmp_lt_blk_079;
    [Test] procedure Test_cmp_lt_inl_079;
    [Test] procedure Test_cmp_lt_blk_080;
    [Test] procedure Test_cmp_lt_inl_080;
    [Test] procedure Test_cmp_lt_blk_081;
    [Test] procedure Test_cmp_lt_inl_081;
    [Test] procedure Test_cmp_lt_blk_082;
    [Test] procedure Test_cmp_lt_inl_082;
    [Test] procedure Test_cmp_lt_blk_083;
    [Test] procedure Test_cmp_lt_inl_083;
    [Test] procedure Test_cmp_lt_blk_084;
    [Test] procedure Test_cmp_lt_inl_084;
    [Test] procedure Test_cmp_lt_blk_085;
    [Test] procedure Test_cmp_lt_inl_085;
    [Test] procedure Test_cmp_lt_blk_086;
    [Test] procedure Test_cmp_lt_inl_086;
    [Test] procedure Test_cmp_lt_blk_087;
    [Test] procedure Test_cmp_lt_inl_087;
    [Test] procedure Test_cmp_lt_blk_088;
    [Test] procedure Test_cmp_lt_inl_088;
    [Test] procedure Test_cmp_lt_blk_089;
    [Test] procedure Test_cmp_lt_inl_089;
    [Test] procedure Test_cmp_lt_blk_090;
    [Test] procedure Test_cmp_lt_inl_090;
    [Test] procedure Test_cmp_lt_blk_091;
    [Test] procedure Test_cmp_lt_inl_091;
    [Test] procedure Test_cmp_lt_blk_092;
    [Test] procedure Test_cmp_lt_inl_092;
    [Test] procedure Test_cmp_lt_blk_093;
    [Test] procedure Test_cmp_lt_inl_093;
    [Test] procedure Test_cmp_lt_blk_094;
    [Test] procedure Test_cmp_lt_inl_094;
    [Test] procedure Test_cmp_lt_blk_095;
    [Test] procedure Test_cmp_lt_inl_095;
    [Test] procedure Test_cmp_lt_blk_096;
    [Test] procedure Test_cmp_lt_inl_096;
    [Test] procedure Test_cmp_lt_blk_097;
    [Test] procedure Test_cmp_lt_inl_097;
    [Test] procedure Test_cmp_lt_blk_098;
    [Test] procedure Test_cmp_lt_inl_098;
    [Test] procedure Test_cmp_lt_blk_099;
    [Test] procedure Test_cmp_lt_inl_099;
    [Test] procedure Test_cmp_lt_blk_100;
    [Test] procedure Test_cmp_lt_inl_100;
    [Test] procedure Test_cmp_lt_blk_101;
    [Test] procedure Test_cmp_lt_inl_101;
    [Test] procedure Test_cmp_lt_blk_102;
    [Test] procedure Test_cmp_lt_inl_102;
    [Test] procedure Test_cmp_lt_blk_103;
    [Test] procedure Test_cmp_lt_inl_103;
    [Test] procedure Test_cmp_lt_blk_104;
    [Test] procedure Test_cmp_lt_inl_104;
    [Test] procedure Test_cmp_lt_blk_105;
    [Test] procedure Test_cmp_lt_inl_105;
    [Test] procedure Test_cmp_lt_blk_106;
    [Test] procedure Test_cmp_lt_inl_106;
    [Test] procedure Test_cmp_lt_blk_107;
    [Test] procedure Test_cmp_lt_inl_107;
    [Test] procedure Test_cmp_lt_blk_108;
    [Test] procedure Test_cmp_lt_inl_108;
    [Test] procedure Test_cmp_lt_blk_109;
    [Test] procedure Test_cmp_lt_inl_109;
    [Test] procedure Test_cmp_lt_blk_110;
    [Test] procedure Test_cmp_lt_inl_110;
    [Test] procedure Test_cmp_lt_blk_111;
    [Test] procedure Test_cmp_lt_inl_111;
    [Test] procedure Test_cmp_lt_blk_112;
    [Test] procedure Test_cmp_lt_inl_112;
    [Test] procedure Test_cmp_lt_blk_113;
    [Test] procedure Test_cmp_lt_inl_113;
    [Test] procedure Test_cmp_lt_blk_114;
    [Test] procedure Test_cmp_lt_inl_114;
    [Test] procedure Test_cmp_lt_blk_115;
    [Test] procedure Test_cmp_lt_inl_115;
    [Test] procedure Test_cmp_lt_blk_116;
    [Test] procedure Test_cmp_lt_inl_116;
    [Test] procedure Test_cmp_lt_blk_117;
    [Test] procedure Test_cmp_lt_inl_117;
    [Test] procedure Test_cmp_lt_blk_118;
    [Test] procedure Test_cmp_lt_inl_118;
    [Test] procedure Test_cmp_lt_blk_119;
    [Test] procedure Test_cmp_lt_inl_119;
    [Test] procedure Test_cmp_lt_blk_120;
    [Test] procedure Test_cmp_lt_inl_120;
    [Test] procedure Test_cmp_lt_blk_121;
    [Test] procedure Test_cmp_lt_inl_121;
    [Test] procedure Test_cmp_lt_blk_122;
    [Test] procedure Test_cmp_lt_inl_122;
    [Test] procedure Test_cmp_lt_blk_123;
    [Test] procedure Test_cmp_lt_inl_123;
    [Test] procedure Test_cmp_lt_blk_124;
    [Test] procedure Test_cmp_lt_inl_124;
    [Test] procedure Test_cmp_lt_blk_125;
    [Test] procedure Test_cmp_lt_inl_125;
    [Test] procedure Test_cmp_lt_blk_126;
    [Test] procedure Test_cmp_lt_inl_126;
    [Test] procedure Test_cmp_lt_blk_127;
    [Test] procedure Test_cmp_lt_inl_127;
    [Test] procedure Test_cmp_lt_blk_128;
    [Test] procedure Test_cmp_lt_inl_128;
    [Test] procedure Test_cmp_gte_blk_129;
    [Test] procedure Test_cmp_gte_inl_129;
    [Test] procedure Test_cmp_gte_blk_130;
    [Test] procedure Test_cmp_gte_inl_130;
    [Test] procedure Test_cmp_gte_blk_131;
    [Test] procedure Test_cmp_gte_inl_131;
    [Test] procedure Test_cmp_gte_blk_132;
    [Test] procedure Test_cmp_gte_inl_132;
    [Test] procedure Test_cmp_gte_blk_133;
    [Test] procedure Test_cmp_gte_inl_133;
    [Test] procedure Test_cmp_gte_blk_134;
    [Test] procedure Test_cmp_gte_inl_134;
    [Test] procedure Test_cmp_gte_blk_135;
    [Test] procedure Test_cmp_gte_inl_135;
    [Test] procedure Test_cmp_gte_blk_136;
    [Test] procedure Test_cmp_gte_inl_136;
    [Test] procedure Test_cmp_gte_blk_137;
    [Test] procedure Test_cmp_gte_inl_137;
    [Test] procedure Test_cmp_gte_blk_138;
    [Test] procedure Test_cmp_gte_inl_138;
    [Test] procedure Test_cmp_gte_blk_139;
    [Test] procedure Test_cmp_gte_inl_139;
    [Test] procedure Test_cmp_gte_blk_140;
    [Test] procedure Test_cmp_gte_inl_140;
    [Test] procedure Test_cmp_gte_blk_141;
    [Test] procedure Test_cmp_gte_inl_141;
    [Test] procedure Test_cmp_gte_blk_142;
    [Test] procedure Test_cmp_gte_inl_142;
    [Test] procedure Test_cmp_gte_blk_143;
    [Test] procedure Test_cmp_gte_inl_143;
    [Test] procedure Test_cmp_gte_blk_144;
    [Test] procedure Test_cmp_gte_inl_144;
    [Test] procedure Test_cmp_gte_blk_145;
    [Test] procedure Test_cmp_gte_inl_145;
    [Test] procedure Test_cmp_gte_blk_146;
    [Test] procedure Test_cmp_gte_inl_146;
    [Test] procedure Test_cmp_gte_blk_147;
    [Test] procedure Test_cmp_gte_inl_147;
    [Test] procedure Test_cmp_gte_blk_148;
    [Test] procedure Test_cmp_gte_inl_148;
    [Test] procedure Test_cmp_gte_blk_149;
    [Test] procedure Test_cmp_gte_inl_149;
    [Test] procedure Test_cmp_gte_blk_150;
    [Test] procedure Test_cmp_gte_inl_150;
    [Test] procedure Test_cmp_gte_blk_151;
    [Test] procedure Test_cmp_gte_inl_151;
    [Test] procedure Test_cmp_gte_blk_152;
    [Test] procedure Test_cmp_gte_inl_152;
    [Test] procedure Test_cmp_gte_blk_153;
    [Test] procedure Test_cmp_gte_inl_153;
    [Test] procedure Test_cmp_gte_blk_154;
    [Test] procedure Test_cmp_gte_inl_154;
    [Test] procedure Test_cmp_gte_blk_155;
    [Test] procedure Test_cmp_gte_inl_155;
    [Test] procedure Test_cmp_gte_blk_156;
    [Test] procedure Test_cmp_gte_inl_156;
    [Test] procedure Test_cmp_gte_blk_157;
    [Test] procedure Test_cmp_gte_inl_157;
    [Test] procedure Test_cmp_gte_blk_158;
    [Test] procedure Test_cmp_gte_inl_158;
    [Test] procedure Test_cmp_gte_blk_159;
    [Test] procedure Test_cmp_gte_inl_159;
    [Test] procedure Test_cmp_gte_blk_160;
    [Test] procedure Test_cmp_gte_inl_160;
    [Test] procedure Test_cmp_gte_blk_161;
    [Test] procedure Test_cmp_gte_inl_161;
    [Test] procedure Test_cmp_gte_blk_162;
    [Test] procedure Test_cmp_gte_inl_162;
    [Test] procedure Test_cmp_gte_blk_163;
    [Test] procedure Test_cmp_gte_inl_163;
    [Test] procedure Test_cmp_gte_blk_164;
    [Test] procedure Test_cmp_gte_inl_164;
    [Test] procedure Test_cmp_gte_blk_165;
    [Test] procedure Test_cmp_gte_inl_165;
    [Test] procedure Test_cmp_gte_blk_166;
    [Test] procedure Test_cmp_gte_inl_166;
    [Test] procedure Test_cmp_gte_blk_167;
    [Test] procedure Test_cmp_gte_inl_167;
    [Test] procedure Test_cmp_gte_blk_168;
    [Test] procedure Test_cmp_gte_inl_168;
    [Test] procedure Test_cmp_gte_blk_169;
    [Test] procedure Test_cmp_gte_inl_169;
    [Test] procedure Test_cmp_gte_blk_170;
    [Test] procedure Test_cmp_gte_inl_170;
    [Test] procedure Test_cmp_gte_blk_171;
    [Test] procedure Test_cmp_gte_inl_171;
    [Test] procedure Test_cmp_gte_blk_172;
    [Test] procedure Test_cmp_gte_inl_172;
    [Test] procedure Test_cmp_gte_blk_173;
    [Test] procedure Test_cmp_gte_inl_173;
    [Test] procedure Test_cmp_gte_blk_174;
    [Test] procedure Test_cmp_gte_inl_174;
    [Test] procedure Test_cmp_gte_blk_175;
    [Test] procedure Test_cmp_gte_inl_175;
    [Test] procedure Test_cmp_gte_blk_176;
    [Test] procedure Test_cmp_gte_inl_176;
    [Test] procedure Test_cmp_gte_blk_177;
    [Test] procedure Test_cmp_gte_inl_177;
    [Test] procedure Test_cmp_gte_blk_178;
    [Test] procedure Test_cmp_gte_inl_178;
    [Test] procedure Test_cmp_gte_blk_179;
    [Test] procedure Test_cmp_gte_inl_179;
    [Test] procedure Test_cmp_gte_blk_180;
    [Test] procedure Test_cmp_gte_inl_180;
    [Test] procedure Test_cmp_gte_blk_181;
    [Test] procedure Test_cmp_gte_inl_181;
    [Test] procedure Test_cmp_gte_blk_182;
    [Test] procedure Test_cmp_gte_inl_182;
    [Test] procedure Test_cmp_gte_blk_183;
    [Test] procedure Test_cmp_gte_inl_183;
    [Test] procedure Test_cmp_gte_blk_184;
    [Test] procedure Test_cmp_gte_inl_184;
    [Test] procedure Test_cmp_gte_blk_185;
    [Test] procedure Test_cmp_gte_inl_185;
    [Test] procedure Test_cmp_gte_blk_186;
    [Test] procedure Test_cmp_gte_inl_186;
    [Test] procedure Test_cmp_gte_blk_187;
    [Test] procedure Test_cmp_gte_inl_187;
    [Test] procedure Test_cmp_gte_blk_188;
    [Test] procedure Test_cmp_gte_inl_188;
    [Test] procedure Test_cmp_gte_blk_189;
    [Test] procedure Test_cmp_gte_inl_189;
    [Test] procedure Test_cmp_gte_blk_190;
    [Test] procedure Test_cmp_gte_inl_190;
    [Test] procedure Test_cmp_gte_blk_191;
    [Test] procedure Test_cmp_gte_inl_191;
    [Test] procedure Test_cmp_gte_blk_192;
    [Test] procedure Test_cmp_gte_inl_192;
    [Test] procedure Test_cmp_lte_blk_193;
    [Test] procedure Test_cmp_lte_inl_193;
    [Test] procedure Test_cmp_lte_blk_194;
    [Test] procedure Test_cmp_lte_inl_194;
    [Test] procedure Test_cmp_lte_blk_195;
    [Test] procedure Test_cmp_lte_inl_195;
    [Test] procedure Test_cmp_lte_blk_196;
    [Test] procedure Test_cmp_lte_inl_196;
    [Test] procedure Test_cmp_lte_blk_197;
    [Test] procedure Test_cmp_lte_inl_197;
    [Test] procedure Test_cmp_lte_blk_198;
    [Test] procedure Test_cmp_lte_inl_198;
    [Test] procedure Test_cmp_lte_blk_199;
    [Test] procedure Test_cmp_lte_inl_199;
    [Test] procedure Test_cmp_lte_blk_200;
    [Test] procedure Test_cmp_lte_inl_200;
    [Test] procedure Test_cmp_lte_blk_201;
    [Test] procedure Test_cmp_lte_inl_201;
    [Test] procedure Test_cmp_lte_blk_202;
    [Test] procedure Test_cmp_lte_inl_202;
    [Test] procedure Test_cmp_lte_blk_203;
    [Test] procedure Test_cmp_lte_inl_203;
    [Test] procedure Test_cmp_lte_blk_204;
    [Test] procedure Test_cmp_lte_inl_204;
    [Test] procedure Test_cmp_lte_blk_205;
    [Test] procedure Test_cmp_lte_inl_205;
    [Test] procedure Test_cmp_lte_blk_206;
    [Test] procedure Test_cmp_lte_inl_206;
    [Test] procedure Test_cmp_lte_blk_207;
    [Test] procedure Test_cmp_lte_inl_207;
    [Test] procedure Test_cmp_lte_blk_208;
    [Test] procedure Test_cmp_lte_inl_208;
    [Test] procedure Test_cmp_lte_blk_209;
    [Test] procedure Test_cmp_lte_inl_209;
    [Test] procedure Test_cmp_lte_blk_210;
    [Test] procedure Test_cmp_lte_inl_210;
    [Test] procedure Test_cmp_lte_blk_211;
    [Test] procedure Test_cmp_lte_inl_211;
    [Test] procedure Test_cmp_lte_blk_212;
    [Test] procedure Test_cmp_lte_inl_212;
    [Test] procedure Test_cmp_lte_blk_213;
    [Test] procedure Test_cmp_lte_inl_213;
    [Test] procedure Test_cmp_lte_blk_214;
    [Test] procedure Test_cmp_lte_inl_214;
    [Test] procedure Test_cmp_lte_blk_215;
    [Test] procedure Test_cmp_lte_inl_215;
    [Test] procedure Test_cmp_lte_blk_216;
    [Test] procedure Test_cmp_lte_inl_216;
    [Test] procedure Test_cmp_lte_blk_217;
    [Test] procedure Test_cmp_lte_inl_217;
    [Test] procedure Test_cmp_lte_blk_218;
    [Test] procedure Test_cmp_lte_inl_218;
    [Test] procedure Test_cmp_lte_blk_219;
    [Test] procedure Test_cmp_lte_inl_219;
    [Test] procedure Test_cmp_lte_blk_220;
    [Test] procedure Test_cmp_lte_inl_220;
    [Test] procedure Test_cmp_lte_blk_221;
    [Test] procedure Test_cmp_lte_inl_221;
    [Test] procedure Test_cmp_lte_blk_222;
    [Test] procedure Test_cmp_lte_inl_222;
    [Test] procedure Test_cmp_lte_blk_223;
    [Test] procedure Test_cmp_lte_inl_223;
    [Test] procedure Test_cmp_lte_blk_224;
    [Test] procedure Test_cmp_lte_inl_224;
    [Test] procedure Test_cmp_lte_blk_225;
    [Test] procedure Test_cmp_lte_inl_225;
    [Test] procedure Test_cmp_lte_blk_226;
    [Test] procedure Test_cmp_lte_inl_226;
    [Test] procedure Test_cmp_lte_blk_227;
    [Test] procedure Test_cmp_lte_inl_227;
    [Test] procedure Test_cmp_lte_blk_228;
    [Test] procedure Test_cmp_lte_inl_228;
    [Test] procedure Test_cmp_lte_blk_229;
    [Test] procedure Test_cmp_lte_inl_229;
    [Test] procedure Test_cmp_lte_blk_230;
    [Test] procedure Test_cmp_lte_inl_230;
    [Test] procedure Test_cmp_lte_blk_231;
    [Test] procedure Test_cmp_lte_inl_231;
    [Test] procedure Test_cmp_lte_blk_232;
    [Test] procedure Test_cmp_lte_inl_232;
    [Test] procedure Test_cmp_lte_blk_233;
    [Test] procedure Test_cmp_lte_inl_233;
    [Test] procedure Test_cmp_lte_blk_234;
    [Test] procedure Test_cmp_lte_inl_234;
    [Test] procedure Test_cmp_lte_blk_235;
    [Test] procedure Test_cmp_lte_inl_235;
    [Test] procedure Test_cmp_lte_blk_236;
    [Test] procedure Test_cmp_lte_inl_236;
    [Test] procedure Test_cmp_lte_blk_237;
    [Test] procedure Test_cmp_lte_inl_237;
    [Test] procedure Test_cmp_lte_blk_238;
    [Test] procedure Test_cmp_lte_inl_238;
    [Test] procedure Test_cmp_lte_blk_239;
    [Test] procedure Test_cmp_lte_inl_239;
    [Test] procedure Test_cmp_lte_blk_240;
    [Test] procedure Test_cmp_lte_inl_240;
    [Test] procedure Test_cmp_lte_blk_241;
    [Test] procedure Test_cmp_lte_inl_241;
    [Test] procedure Test_cmp_lte_blk_242;
    [Test] procedure Test_cmp_lte_inl_242;
    [Test] procedure Test_cmp_lte_blk_243;
    [Test] procedure Test_cmp_lte_inl_243;
    [Test] procedure Test_cmp_lte_blk_244;
    [Test] procedure Test_cmp_lte_inl_244;
    [Test] procedure Test_cmp_lte_blk_245;
    [Test] procedure Test_cmp_lte_inl_245;
    [Test] procedure Test_cmp_lte_blk_246;
    [Test] procedure Test_cmp_lte_inl_246;
    [Test] procedure Test_cmp_lte_blk_247;
    [Test] procedure Test_cmp_lte_inl_247;
    [Test] procedure Test_cmp_lte_blk_248;
    [Test] procedure Test_cmp_lte_inl_248;
    [Test] procedure Test_cmp_lte_blk_249;
    [Test] procedure Test_cmp_lte_inl_249;
    [Test] procedure Test_cmp_lte_blk_250;
    [Test] procedure Test_cmp_lte_inl_250;
    [Test] procedure Test_cmp_lte_blk_251;
    [Test] procedure Test_cmp_lte_inl_251;
    [Test] procedure Test_cmp_lte_blk_252;
    [Test] procedure Test_cmp_lte_inl_252;
    [Test] procedure Test_cmp_lte_blk_253;
    [Test] procedure Test_cmp_lte_inl_253;
    [Test] procedure Test_cmp_lte_blk_254;
    [Test] procedure Test_cmp_lte_inl_254;
    [Test] procedure Test_cmp_lte_blk_255;
    [Test] procedure Test_cmp_lte_inl_255;
    [Test] procedure Test_cmp_lte_blk_256;
    [Test] procedure Test_cmp_lte_inl_256;
    [Test] procedure Test_cmp_eq_blk_257;
    [Test] procedure Test_cmp_eq_inl_257;
    [Test] procedure Test_cmp_eq_blk_258;
    [Test] procedure Test_cmp_eq_inl_258;
    [Test] procedure Test_cmp_eq_blk_259;
    [Test] procedure Test_cmp_eq_inl_259;
    [Test] procedure Test_cmp_eq_blk_260;
    [Test] procedure Test_cmp_eq_inl_260;
    [Test] procedure Test_cmp_eq_blk_261;
    [Test] procedure Test_cmp_eq_inl_261;
    [Test] procedure Test_cmp_eq_blk_262;
    [Test] procedure Test_cmp_eq_inl_262;
    [Test] procedure Test_cmp_eq_blk_263;
    [Test] procedure Test_cmp_eq_inl_263;
    [Test] procedure Test_cmp_eq_blk_264;
    [Test] procedure Test_cmp_eq_inl_264;
    [Test] procedure Test_cmp_eq_blk_265;
    [Test] procedure Test_cmp_eq_inl_265;
    [Test] procedure Test_cmp_eq_blk_266;
    [Test] procedure Test_cmp_eq_inl_266;
    [Test] procedure Test_cmp_eq_blk_267;
    [Test] procedure Test_cmp_eq_inl_267;
    [Test] procedure Test_cmp_eq_blk_268;
    [Test] procedure Test_cmp_eq_inl_268;
    [Test] procedure Test_cmp_eq_blk_269;
    [Test] procedure Test_cmp_eq_inl_269;
    [Test] procedure Test_cmp_eq_blk_270;
    [Test] procedure Test_cmp_eq_inl_270;
    [Test] procedure Test_cmp_eq_blk_271;
    [Test] procedure Test_cmp_eq_inl_271;
    [Test] procedure Test_cmp_eq_blk_272;
    [Test] procedure Test_cmp_eq_inl_272;
    [Test] procedure Test_cmp_eq_blk_273;
    [Test] procedure Test_cmp_eq_inl_273;
    [Test] procedure Test_cmp_eq_blk_274;
    [Test] procedure Test_cmp_eq_inl_274;
    [Test] procedure Test_cmp_eq_blk_275;
    [Test] procedure Test_cmp_eq_inl_275;
    [Test] procedure Test_cmp_eq_blk_276;
    [Test] procedure Test_cmp_eq_inl_276;
    [Test] procedure Test_cmp_eq_blk_277;
    [Test] procedure Test_cmp_eq_inl_277;
    [Test] procedure Test_cmp_eq_blk_278;
    [Test] procedure Test_cmp_eq_inl_278;
    [Test] procedure Test_cmp_eq_blk_279;
    [Test] procedure Test_cmp_eq_inl_279;
    [Test] procedure Test_cmp_eq_blk_280;
    [Test] procedure Test_cmp_eq_inl_280;
    [Test] procedure Test_cmp_eq_blk_281;
    [Test] procedure Test_cmp_eq_inl_281;
    [Test] procedure Test_cmp_eq_blk_282;
    [Test] procedure Test_cmp_eq_inl_282;
    [Test] procedure Test_cmp_eq_blk_283;
    [Test] procedure Test_cmp_eq_inl_283;
    [Test] procedure Test_cmp_eq_blk_284;
    [Test] procedure Test_cmp_eq_inl_284;
    [Test] procedure Test_cmp_eq_blk_285;
    [Test] procedure Test_cmp_eq_inl_285;
    [Test] procedure Test_cmp_eq_blk_286;
    [Test] procedure Test_cmp_eq_inl_286;
    [Test] procedure Test_cmp_eq_blk_287;
    [Test] procedure Test_cmp_eq_inl_287;
    [Test] procedure Test_cmp_eq_blk_288;
    [Test] procedure Test_cmp_eq_inl_288;
    [Test] procedure Test_cmp_eq_blk_289;
    [Test] procedure Test_cmp_eq_inl_289;
    [Test] procedure Test_cmp_eq_blk_290;
    [Test] procedure Test_cmp_eq_inl_290;
    [Test] procedure Test_cmp_eq_blk_291;
    [Test] procedure Test_cmp_eq_inl_291;
    [Test] procedure Test_cmp_eq_blk_292;
    [Test] procedure Test_cmp_eq_inl_292;
    [Test] procedure Test_cmp_eq_blk_293;
    [Test] procedure Test_cmp_eq_inl_293;
    [Test] procedure Test_cmp_eq_blk_294;
    [Test] procedure Test_cmp_eq_inl_294;
    [Test] procedure Test_cmp_eq_blk_295;
    [Test] procedure Test_cmp_eq_inl_295;
    [Test] procedure Test_cmp_eq_blk_296;
    [Test] procedure Test_cmp_eq_inl_296;
    [Test] procedure Test_cmp_eq_blk_297;
    [Test] procedure Test_cmp_eq_inl_297;
    [Test] procedure Test_cmp_eq_blk_298;
    [Test] procedure Test_cmp_eq_inl_298;
    [Test] procedure Test_cmp_eq_blk_299;
    [Test] procedure Test_cmp_eq_inl_299;
    [Test] procedure Test_cmp_eq_blk_300;
    [Test] procedure Test_cmp_eq_inl_300;
    [Test] procedure Test_cmp_eq_blk_301;
    [Test] procedure Test_cmp_eq_inl_301;
    [Test] procedure Test_cmp_eq_blk_302;
    [Test] procedure Test_cmp_eq_inl_302;
    [Test] procedure Test_cmp_eq_blk_303;
    [Test] procedure Test_cmp_eq_inl_303;
    [Test] procedure Test_cmp_eq_blk_304;
    [Test] procedure Test_cmp_eq_inl_304;
    [Test] procedure Test_cmp_eq_blk_305;
    [Test] procedure Test_cmp_eq_inl_305;
    [Test] procedure Test_cmp_eq_blk_306;
    [Test] procedure Test_cmp_eq_inl_306;
    [Test] procedure Test_cmp_eq_blk_307;
    [Test] procedure Test_cmp_eq_inl_307;
    [Test] procedure Test_cmp_eq_blk_308;
    [Test] procedure Test_cmp_eq_inl_308;
    [Test] procedure Test_cmp_eq_blk_309;
    [Test] procedure Test_cmp_eq_inl_309;
    [Test] procedure Test_cmp_eq_blk_310;
    [Test] procedure Test_cmp_eq_inl_310;
    [Test] procedure Test_cmp_eq_blk_311;
    [Test] procedure Test_cmp_eq_inl_311;
    [Test] procedure Test_cmp_eq_blk_312;
    [Test] procedure Test_cmp_eq_inl_312;
    [Test] procedure Test_cmp_eq_blk_313;
    [Test] procedure Test_cmp_eq_inl_313;
    [Test] procedure Test_cmp_eq_blk_314;
    [Test] procedure Test_cmp_eq_inl_314;
    [Test] procedure Test_cmp_eq_blk_315;
    [Test] procedure Test_cmp_eq_inl_315;
    [Test] procedure Test_cmp_eq_blk_316;
    [Test] procedure Test_cmp_eq_inl_316;
    [Test] procedure Test_cmp_eq_blk_317;
    [Test] procedure Test_cmp_eq_inl_317;
    [Test] procedure Test_cmp_eq_blk_318;
    [Test] procedure Test_cmp_eq_inl_318;
    [Test] procedure Test_cmp_eq_blk_319;
    [Test] procedure Test_cmp_eq_inl_319;
    [Test] procedure Test_cmp_eq_blk_320;
    [Test] procedure Test_cmp_eq_inl_320;
    [Test] procedure Test_cmp_ne_blk_321;
    [Test] procedure Test_cmp_ne_inl_321;
    [Test] procedure Test_cmp_ne_blk_322;
    [Test] procedure Test_cmp_ne_inl_322;
    [Test] procedure Test_cmp_ne_blk_323;
    [Test] procedure Test_cmp_ne_inl_323;
    [Test] procedure Test_cmp_ne_blk_324;
    [Test] procedure Test_cmp_ne_inl_324;
    [Test] procedure Test_cmp_ne_blk_325;
    [Test] procedure Test_cmp_ne_inl_325;
    [Test] procedure Test_cmp_ne_blk_326;
    [Test] procedure Test_cmp_ne_inl_326;
    [Test] procedure Test_cmp_ne_blk_327;
    [Test] procedure Test_cmp_ne_inl_327;
    [Test] procedure Test_cmp_ne_blk_328;
    [Test] procedure Test_cmp_ne_inl_328;
    [Test] procedure Test_cmp_ne_blk_329;
    [Test] procedure Test_cmp_ne_inl_329;
    [Test] procedure Test_cmp_ne_blk_330;
    [Test] procedure Test_cmp_ne_inl_330;
    [Test] procedure Test_cmp_ne_blk_331;
    [Test] procedure Test_cmp_ne_inl_331;
    [Test] procedure Test_cmp_ne_blk_332;
    [Test] procedure Test_cmp_ne_inl_332;
    [Test] procedure Test_cmp_ne_blk_333;
    [Test] procedure Test_cmp_ne_inl_333;
    [Test] procedure Test_cmp_ne_blk_334;
    [Test] procedure Test_cmp_ne_inl_334;
    [Test] procedure Test_cmp_ne_blk_335;
    [Test] procedure Test_cmp_ne_inl_335;
    [Test] procedure Test_cmp_ne_blk_336;
    [Test] procedure Test_cmp_ne_inl_336;
    [Test] procedure Test_cmp_ne_blk_337;
    [Test] procedure Test_cmp_ne_inl_337;
    [Test] procedure Test_cmp_ne_blk_338;
    [Test] procedure Test_cmp_ne_inl_338;
    [Test] procedure Test_cmp_ne_blk_339;
    [Test] procedure Test_cmp_ne_inl_339;
    [Test] procedure Test_cmp_ne_blk_340;
    [Test] procedure Test_cmp_ne_inl_340;
    [Test] procedure Test_cmp_ne_blk_341;
    [Test] procedure Test_cmp_ne_inl_341;
    [Test] procedure Test_cmp_ne_blk_342;
    [Test] procedure Test_cmp_ne_inl_342;
    [Test] procedure Test_cmp_ne_blk_343;
    [Test] procedure Test_cmp_ne_inl_343;
    [Test] procedure Test_cmp_ne_blk_344;
    [Test] procedure Test_cmp_ne_inl_344;
    [Test] procedure Test_cmp_ne_blk_345;
    [Test] procedure Test_cmp_ne_inl_345;
    [Test] procedure Test_cmp_ne_blk_346;
    [Test] procedure Test_cmp_ne_inl_346;
    [Test] procedure Test_cmp_ne_blk_347;
    [Test] procedure Test_cmp_ne_inl_347;
    [Test] procedure Test_cmp_ne_blk_348;
    [Test] procedure Test_cmp_ne_inl_348;
    [Test] procedure Test_cmp_ne_blk_349;
    [Test] procedure Test_cmp_ne_inl_349;
    [Test] procedure Test_cmp_ne_blk_350;
    [Test] procedure Test_cmp_ne_inl_350;
    [Test] procedure Test_cmp_ne_blk_351;
    [Test] procedure Test_cmp_ne_inl_351;
    [Test] procedure Test_cmp_ne_blk_352;
    [Test] procedure Test_cmp_ne_inl_352;
    [Test] procedure Test_cmp_ne_blk_353;
    [Test] procedure Test_cmp_ne_inl_353;
    [Test] procedure Test_cmp_ne_blk_354;
    [Test] procedure Test_cmp_ne_inl_354;
    [Test] procedure Test_cmp_ne_blk_355;
    [Test] procedure Test_cmp_ne_inl_355;
    [Test] procedure Test_cmp_ne_blk_356;
    [Test] procedure Test_cmp_ne_inl_356;
    [Test] procedure Test_cmp_ne_blk_357;
    [Test] procedure Test_cmp_ne_inl_357;
    [Test] procedure Test_cmp_ne_blk_358;
    [Test] procedure Test_cmp_ne_inl_358;
    [Test] procedure Test_cmp_ne_blk_359;
    [Test] procedure Test_cmp_ne_inl_359;
    [Test] procedure Test_cmp_ne_blk_360;
    [Test] procedure Test_cmp_ne_inl_360;
    [Test] procedure Test_cmp_ne_blk_361;
    [Test] procedure Test_cmp_ne_inl_361;
    [Test] procedure Test_cmp_ne_blk_362;
    [Test] procedure Test_cmp_ne_inl_362;
    [Test] procedure Test_cmp_ne_blk_363;
    [Test] procedure Test_cmp_ne_inl_363;
    [Test] procedure Test_cmp_ne_blk_364;
    [Test] procedure Test_cmp_ne_inl_364;
    [Test] procedure Test_cmp_ne_blk_365;
    [Test] procedure Test_cmp_ne_inl_365;
    [Test] procedure Test_cmp_ne_blk_366;
    [Test] procedure Test_cmp_ne_inl_366;
    [Test] procedure Test_cmp_ne_blk_367;
    [Test] procedure Test_cmp_ne_inl_367;
    [Test] procedure Test_cmp_ne_blk_368;
    [Test] procedure Test_cmp_ne_inl_368;
    [Test] procedure Test_cmp_ne_blk_369;
    [Test] procedure Test_cmp_ne_inl_369;
    [Test] procedure Test_cmp_ne_blk_370;
    [Test] procedure Test_cmp_ne_inl_370;
    [Test] procedure Test_cmp_ne_blk_371;
    [Test] procedure Test_cmp_ne_inl_371;
    [Test] procedure Test_cmp_ne_blk_372;
    [Test] procedure Test_cmp_ne_inl_372;
    [Test] procedure Test_cmp_ne_blk_373;
    [Test] procedure Test_cmp_ne_inl_373;
    [Test] procedure Test_cmp_ne_blk_374;
    [Test] procedure Test_cmp_ne_inl_374;
    [Test] procedure Test_cmp_ne_blk_375;
    [Test] procedure Test_cmp_ne_inl_375;
    [Test] procedure Test_cmp_ne_blk_376;
    [Test] procedure Test_cmp_ne_inl_376;
    [Test] procedure Test_cmp_ne_blk_377;
    [Test] procedure Test_cmp_ne_inl_377;
    [Test] procedure Test_cmp_ne_blk_378;
    [Test] procedure Test_cmp_ne_inl_378;
    [Test] procedure Test_cmp_ne_blk_379;
    [Test] procedure Test_cmp_ne_inl_379;
    [Test] procedure Test_cmp_ne_blk_380;
    [Test] procedure Test_cmp_ne_inl_380;
    [Test] procedure Test_cmp_ne_blk_381;
    [Test] procedure Test_cmp_ne_inl_381;
    [Test] procedure Test_cmp_ne_blk_382;
    [Test] procedure Test_cmp_ne_inl_382;
    [Test] procedure Test_cmp_ne_blk_383;
    [Test] procedure Test_cmp_ne_inl_383;
    [Test] procedure Test_cmp_ne_blk_384;
    [Test] procedure Test_cmp_ne_inl_384;
    [Test] procedure Test_logic_or_blk_001;
    [Test] procedure Test_logic_and_blk_001;
    [Test] procedure Test_logic_or_inl_001;
    [Test] procedure Test_logic_and_inl_001;
    [Test] procedure Test_logic_or_blk_002;
    [Test] procedure Test_logic_and_blk_002;
    [Test] procedure Test_logic_or_inl_002;
    [Test] procedure Test_logic_and_inl_002;
    [Test] procedure Test_logic_or_blk_003;
    [Test] procedure Test_logic_and_blk_003;
    [Test] procedure Test_logic_or_inl_003;
    [Test] procedure Test_logic_and_inl_003;
    [Test] procedure Test_logic_or_blk_004;
    [Test] procedure Test_logic_and_blk_004;
    [Test] procedure Test_logic_or_inl_004;
    [Test] procedure Test_logic_and_inl_004;
    [Test] procedure Test_logic_or_blk_005;
    [Test] procedure Test_logic_and_blk_005;
    [Test] procedure Test_logic_or_inl_005;
    [Test] procedure Test_logic_and_inl_005;
    [Test] procedure Test_logic_or_blk_006;
    [Test] procedure Test_logic_and_blk_006;
    [Test] procedure Test_logic_or_inl_006;
    [Test] procedure Test_logic_and_inl_006;
    [Test] procedure Test_logic_or_blk_007;
    [Test] procedure Test_logic_and_blk_007;
    [Test] procedure Test_logic_or_inl_007;
    [Test] procedure Test_logic_and_inl_007;
    [Test] procedure Test_logic_or_blk_008;
    [Test] procedure Test_logic_and_blk_008;
    [Test] procedure Test_logic_or_inl_008;
    [Test] procedure Test_logic_and_inl_008;
    [Test] procedure Test_logic_or_blk_009;
    [Test] procedure Test_logic_and_blk_009;
    [Test] procedure Test_logic_or_inl_009;
    [Test] procedure Test_logic_and_inl_009;
    [Test] procedure Test_logic_or_blk_010;
    [Test] procedure Test_logic_and_blk_010;
    [Test] procedure Test_logic_or_inl_010;
    [Test] procedure Test_logic_and_inl_010;
    [Test] procedure Test_logic_or_blk_011;
    [Test] procedure Test_logic_and_blk_011;
    [Test] procedure Test_logic_or_inl_011;
    [Test] procedure Test_logic_and_inl_011;
    [Test] procedure Test_logic_or_blk_012;
    [Test] procedure Test_logic_and_blk_012;
    [Test] procedure Test_logic_or_inl_012;
    [Test] procedure Test_logic_and_inl_012;
    [Test] procedure Test_logic_or_blk_013;
    [Test] procedure Test_logic_and_blk_013;
    [Test] procedure Test_logic_or_inl_013;
    [Test] procedure Test_logic_and_inl_013;
    [Test] procedure Test_logic_or_blk_014;
    [Test] procedure Test_logic_and_blk_014;
    [Test] procedure Test_logic_or_inl_014;
    [Test] procedure Test_logic_and_inl_014;
    [Test] procedure Test_logic_or_blk_015;
    [Test] procedure Test_logic_and_blk_015;
    [Test] procedure Test_logic_or_inl_015;
    [Test] procedure Test_logic_and_inl_015;
    [Test] procedure Test_logic_or_blk_016;
    [Test] procedure Test_logic_and_blk_016;
    [Test] procedure Test_logic_or_inl_016;
    [Test] procedure Test_logic_and_inl_016;
    [Test] procedure Test_logic_or_blk_017;
    [Test] procedure Test_logic_and_blk_017;
    [Test] procedure Test_logic_or_inl_017;
    [Test] procedure Test_logic_and_inl_017;
    [Test] procedure Test_logic_or_blk_018;
    [Test] procedure Test_logic_and_blk_018;
    [Test] procedure Test_logic_or_inl_018;
    [Test] procedure Test_logic_and_inl_018;
    [Test] procedure Test_logic_or_blk_019;
    [Test] procedure Test_logic_and_blk_019;
    [Test] procedure Test_logic_or_inl_019;
    [Test] procedure Test_logic_and_inl_019;
    [Test] procedure Test_logic_or_blk_020;
    [Test] procedure Test_logic_and_blk_020;
    [Test] procedure Test_logic_or_inl_020;
    [Test] procedure Test_logic_and_inl_020;
    [Test] procedure Test_logic_or_blk_021;
    [Test] procedure Test_logic_and_blk_021;
    [Test] procedure Test_logic_or_inl_021;
    [Test] procedure Test_logic_and_inl_021;
    [Test] procedure Test_logic_or_blk_022;
    [Test] procedure Test_logic_and_blk_022;
    [Test] procedure Test_logic_or_inl_022;
    [Test] procedure Test_logic_and_inl_022;
    [Test] procedure Test_logic_or_blk_023;
    [Test] procedure Test_logic_and_blk_023;
    [Test] procedure Test_logic_or_inl_023;
    [Test] procedure Test_logic_and_inl_023;
    [Test] procedure Test_logic_or_blk_024;
    [Test] procedure Test_logic_and_blk_024;
    [Test] procedure Test_logic_or_inl_024;
    [Test] procedure Test_logic_and_inl_024;
    [Test] procedure Test_logic_or_blk_025;
    [Test] procedure Test_logic_and_blk_025;
    [Test] procedure Test_logic_or_inl_025;
    [Test] procedure Test_logic_and_inl_025;
    [Test] procedure Test_logic_or_blk_026;
    [Test] procedure Test_logic_and_blk_026;
    [Test] procedure Test_logic_or_inl_026;
    [Test] procedure Test_logic_and_inl_026;
    [Test] procedure Test_logic_or_blk_027;
    [Test] procedure Test_logic_and_blk_027;
    [Test] procedure Test_logic_or_inl_027;
    [Test] procedure Test_logic_and_inl_027;
    [Test] procedure Test_logic_or_blk_028;
    [Test] procedure Test_logic_and_blk_028;
    [Test] procedure Test_logic_or_inl_028;
    [Test] procedure Test_logic_and_inl_028;
    [Test] procedure Test_logic_or_blk_029;
    [Test] procedure Test_logic_and_blk_029;
    [Test] procedure Test_logic_or_inl_029;
    [Test] procedure Test_logic_and_inl_029;
    [Test] procedure Test_logic_or_blk_030;
    [Test] procedure Test_logic_and_blk_030;
    [Test] procedure Test_logic_or_inl_030;
    [Test] procedure Test_logic_and_inl_030;
    [Test] procedure Test_logic_or_blk_031;
    [Test] procedure Test_logic_and_blk_031;
    [Test] procedure Test_logic_or_inl_031;
    [Test] procedure Test_logic_and_inl_031;
    [Test] procedure Test_logic_or_blk_032;
    [Test] procedure Test_logic_and_blk_032;
    [Test] procedure Test_logic_or_inl_032;
    [Test] procedure Test_logic_and_inl_032;
    [Test] procedure Test_logic_or_blk_033;
    [Test] procedure Test_logic_and_blk_033;
    [Test] procedure Test_logic_or_inl_033;
    [Test] procedure Test_logic_and_inl_033;
    [Test] procedure Test_logic_or_blk_034;
    [Test] procedure Test_logic_and_blk_034;
    [Test] procedure Test_logic_or_inl_034;
    [Test] procedure Test_logic_and_inl_034;
    [Test] procedure Test_logic_or_blk_035;
    [Test] procedure Test_logic_and_blk_035;
    [Test] procedure Test_logic_or_inl_035;
    [Test] procedure Test_logic_and_inl_035;
    [Test] procedure Test_logic_or_blk_036;
    [Test] procedure Test_logic_and_blk_036;
    [Test] procedure Test_logic_or_inl_036;
    [Test] procedure Test_logic_and_inl_036;
    [Test] procedure Test_logic_not_blk_037;
    [Test] procedure Test_logic_not_inl_037;
    [Test] procedure Test_logic_not_blk_038;
    [Test] procedure Test_logic_not_inl_038;
    [Test] procedure Test_logic_not_blk_039;
    [Test] procedure Test_logic_not_inl_039;
    [Test] procedure Test_logic_not_blk_040;
    [Test] procedure Test_logic_not_inl_040;
    [Test] procedure Test_logic_not_blk_041;
    [Test] procedure Test_logic_not_inl_041;
    [Test] procedure Test_logic_not_blk_042;
    [Test] procedure Test_logic_not_inl_042;
    [Test] procedure Test_logic_or3_a;
    [Test] procedure Test_logic_or3_b;
    [Test] procedure Test_logic_and3_a;
    [Test] procedure Test_logic_and3_b;
    [Test] procedure Test_cmp_lit_gt_a;
    [Test] procedure Test_cmp_lit_gt_b;
    [Test] procedure Test_cmp_lit_eq_a;
    [Test] procedure Test_cmp_lit_eq_b;
    [Test] procedure Test_cmp_lit_lte_a;
    [Test] procedure Test_cmp_lit_block;
    [Test] procedure Test_var_basic;
    [Test] procedure Test_var_in_text;
    [Test] procedure Test_var_missing;
    [Test] procedure Test_var_two;
    [Test] procedure Test_var_three_sep;
    [Test] procedure Test_var_number_int;
    [Test] procedure Test_var_number_neg;
    [Test] procedure Test_var_number_float;
    [Test] procedure Test_var_number_zero;
    [Test] procedure Test_var_bool_true;
    [Test] procedure Test_var_bool_false;
    [Test] procedure Test_var_empty_string;
    [Test] procedure Test_var_only_text;
    [Test] procedure Test_var_dotted_2;
    [Test] procedure Test_var_dotted_3;
    [Test] procedure Test_var_dotted_miss;
    [Test] procedure Test_esc_double_amp;
    [Test] procedure Test_esc_triple_amp;
    [Test] procedure Test_esc_amp_amp;
    [Test] procedure Test_esc_double_lt;
    [Test] procedure Test_esc_triple_lt;
    [Test] procedure Test_esc_amp_lt;
    [Test] procedure Test_esc_double_gt;
    [Test] procedure Test_esc_triple_gt;
    [Test] procedure Test_esc_amp_gt;
    [Test] procedure Test_esc_double_quot;
    [Test] procedure Test_esc_triple_quot;
    [Test] procedure Test_esc_amp_quot;
    [Test] procedure Test_esc_double_tag;
    [Test] procedure Test_esc_triple_tag;
    [Test] procedure Test_esc_amp_tag;
    [Test] procedure Test_esc_double_mix;
    [Test] procedure Test_esc_triple_mix;
    [Test] procedure Test_esc_amp_mix;
    [Test] procedure Test_esc_triple_in_text;
    [Test] procedure Test_esc_double_in_text;
    [Test] procedure Test_parent_each;
    [Test] procedure Test_parent_with;
    [Test] procedure Test_parent_double;
    [Test] procedure Test_root_each;
    [Test] procedure Test_root_nested;
    [Test] procedure Test_root_field;
    [Test] procedure Test_each_index;
    [Test] procedure Test_each_index_one;
    [Test] procedure Test_each_first;
    [Test] procedure Test_each_last;
    [Test] procedure Test_each_firstlast;
    [Test] procedure Test_each_key_obj;
    [Test] procedure Test_each_index_obj;
    [Test] procedure Test_each_this_index;
    [Test] procedure Test_each_nested_idx;
    [Test] procedure Test_lit_if_true;
    [Test] procedure Test_lit_if_false;
    [Test] procedure Test_lit_if_zero;
    [Test] procedure Test_lit_if_one;
    [Test] procedure Test_lit_unless_true;
    [Test] procedure Test_lit_unless_false;
    [Test] procedure Test_lit_concat_num;
    [Test] procedure Test_lit_concat_float;
    [Test] procedure Test_lit_upper_lit;
    [Test] procedure Test_lit_wrap;
    [Test] procedure Test_lookup_arr_0;
    [Test] procedure Test_lookup_arr_2;
    [Test] procedure Test_lookup_obj;
    [Test] procedure Test_lookup_index;
    [Test] procedure Test_lookup_atindex;
    [Test] procedure Test_cmt_short;
    [Test] procedure Test_cmt_long;
    [Test] procedure Test_cmt_with_braces;
    [Test] procedure Test_cmt_only;
    [Test] procedure Test_cmt_between_vars;
    [Test] procedure Test_ws_both;
    [Test] procedure Test_ws_left;
    [Test] procedure Test_ws_right;
    [Test] procedure Test_ws_keep;
    [Test] procedure Test_ws_newlines;
    [Test] procedure Test_sub_1;
    [Test] procedure Test_sub_2;
    [Test] procedure Test_sub_3;
    [Test] procedure Test_sub_4;
    [Test] procedure Test_sub_multi;
    [Test] procedure Test_sub_vars;
    [Test] procedure Test_sub_two;
    [Test] procedure Test_sub_5deep;
    [Test] procedure Test_sub_eqsub;
    [Test] procedure Test_blk_if_t;
    [Test] procedure Test_blk_if_f;
    [Test] procedure Test_blk_if_noelse_t;
    [Test] procedure Test_blk_if_noelse_f;
    [Test] procedure Test_blk_if_missing;
    [Test] procedure Test_blk_if_emptystr;
    [Test] procedure Test_blk_if_zero;
    [Test] procedure Test_blk_if_emptyarr;
    [Test] procedure Test_blk_unless_t;
    [Test] procedure Test_blk_unless_f;
    [Test] procedure Test_blk_unless_else;
    [Test] procedure Test_blk_with;
    [Test] procedure Test_blk_with_one;
    [Test] procedure Test_blk_each;
    [Test] procedure Test_blk_each_empty;
    [Test] procedure Test_blk_each_else;
    [Test] procedure Test_blk_nested_ie;
    [Test] procedure Test_blk_nested_else;
    [Test] procedure Test_blk_each_of_obj;
    [Test] procedure Test_blk_helper_in;
    [Test] procedure Test_blk_each_helper;
    [Test] procedure Test_blk_with_each;
    [Test] procedure Test_prt_plain;
    [Test] procedure Test_prt_var;
    [Test] procedure Test_prt_helper;
    [Test] procedure Test_prt_twice;
    [Test] procedure Test_prt_in_each;
    [Test] procedure Test_prt_dyn_plain;
    [Test] procedure Test_prt_dyn_helper;
    [Test] procedure Test_prt_dyn_var;
    [Test] procedure Test_prt_dyn_each;
    [Test] procedure Test_prt_arg_none;
    [Test] procedure Test_prt_arg_context;
    [Test] procedure Test_prt_arg_context_hash;
    [Test] procedure Test_prt_arg_hash;
    [Test] procedure Test_prt_arg_dynamic;
    [Test] procedure Test_prt_escape_chars;
    [Test] procedure Test_q_spaces;
    [Test] procedure Test_q_single;
    [Test] procedure Test_q_empty;
    [Test] procedure Test_q_parens_lit;
    [Test] procedure Test_q_braces_lit;
    [Test] procedure Test_q_sub_literal;
    [Test] procedure Test_q_mix;
    [Test] procedure Test_kitchen_1;
    [Test] procedure Test_kitchen_2;
    [Test] procedure Test_kitchen_3;
    [Test] procedure Test_kitchen_4;
    {$ENDREGION}

  end;

implementation

uses
  Alcinoe.FileUtils,
  Alcinoe.Localization,
  Winapi.Windows;

var
  GHandlebarsTemplatesExtracted: Boolean;
  GHandlebarsTemplatesModulePath: String;

{********************************************}
function DecodeHandlebarsTemplateResourceName(
           const AResourceName: String;
           const APrefix: String): String;
begin
  Result := '';
  var I := Length(APrefix) + 1;
  while I <= Length(AResourceName) do begin
    var LChar := AResourceName[I];
    if LChar = '_' then begin
      Inc(I);
      if I > Length(AResourceName) then
        raise Exception.CreateFmt('Invalid Handlebars template resource name "%s"', [AResourceName]);
      case AResourceName[I] of
        'D': Result := Result + '.';
        'P': Result := Result + PathDelim;
        'U': Result := Result + '_';
        else
          raise Exception.CreateFmt('Invalid Handlebars template resource name "%s"', [AResourceName]);
      end;
    end
    else
      Result := Result + LowerCase(LChar);
    Inc(I);
  end;
end;

{**********************************************************************************************************************}
procedure ExtractHandlebarsTemplateResource(const AResourceName: String; const ABaseDir: String; const APrefix: String);
begin
  var LRelativePath := DecodeHandlebarsTemplateResourceName(AResourceName, APrefix);
  var LOutputFileName := TPath.Combine(ABaseDir, LRelativePath);
  TDirectory.CreateDirectory(ExtractFilePath(LOutputFileName));
  var LFileStream := TFileStream.Create(LOutputFileName, fmCreate);
  try
    var LResourceStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
    try
      LFileStream.CopyFrom(LResourceStream, 0);
    finally
      ALFreeAndNil(LResourceStream);
    end;
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{*******************************************************************************************************************************}
function EnumHandlebarsTemplateResourceName(HModule: HMODULE; LpszType: PChar; LpszName: PChar; LParam: LONG_PTR): BOOL; stdcall;
begin
  Result := True;
  if NativeUInt(LpszName) <= High(Word) then
    Exit;
  var LResourceName := String(LpszName);
  if SameText(Copy(LResourceName, 1, Length('HB_VALID_')), 'HB_VALID_') then
    TStringList(Pointer(LParam)).Add(LResourceName);
end;

{******************************************************************************}
procedure ExtractHandlebarsTemplateResources(const AValidTemplatesPath: String);
begin
  var LResourceNames := TStringList.Create;
  try
    if not EnumResourceNames(HInstance, RT_RCDATA, @EnumHandlebarsTemplateResourceName, LONG_PTR(Pointer(LResourceNames))) then
      RaiseLastOSError;
    if LResourceNames.Count = 0 then
      raise Exception.Create('No Handlebars template resources found');
    LResourceNames.Sort;
    for var I := 0 to LResourceNames.Count - 1 do begin
      var LResourceName := LResourceNames[I];
      if SameText(Copy(LResourceName, 1, Length('HB_VALID_')), 'HB_VALID_') then
        ExtractHandlebarsTemplateResource(LResourceName, AValidTemplatesPath, 'HB_VALID_');
    end;
  finally
    ALFreeAndNil(LResourceNames);
  end;
end;

{*****************************************}
procedure PrepareHandlebarsTemplateFolders;
begin
  var LModulePath := ALGetModulePathW;
  if GHandlebarsTemplatesExtracted and SameText(GHandlebarsTemplatesModulePath, LModulePath) then
    Exit;

  var LValidTemplatesPath := TPath.Combine(LModulePath, 'Templates');

  if TDirectory.Exists(LValidTemplatesPath) then
    TDirectory.Delete(LValidTemplatesPath, True);

  TDirectory.CreateDirectory(LValidTemplatesPath);

  ExtractHandlebarsTemplateResources(LValidTemplatesPath);

  GHandlebarsTemplatesModulePath := LModulePath;
  GHandlebarsTemplatesExtracted := True;
end;

{**************************************}
procedure TALDUnitXTestHandlebars.Setup;
begin
  PrepareHandlebarsTemplateFolders;
  FHandlebars := TALHandlebars.Create(TPath.Combine(ALGetModulePathW, 'Templates'));
  FHandlebars.RegisterHelper('upper', HelperUpper);
  FHandlebars.RegisterHelper('lower', HelperLower);
  FHandlebars.RegisterHelper('wrap', HelperWrap);
end;

{*****************************************}
procedure TALDUnitXTestHandlebars.TearDown;
begin
  ALFreeAndNil(FHandlebars);
end;

{*******************************************************************************************************}
procedure TALDUnitXTestHandlebars.CheckEq(const AExpected, AActual: AnsiString; const AMsg: String = '');
begin
  Assert.AreEqual(String(AExpected), String(AActual), AMsg);
end;

{********************************************************************************************************}
function TALDUnitXTestHandlebars.Render(const AFilename: AnsiString; const AJson: AnsiString): AnsiString;
begin
  var LContext: TALJsonNodeA;
  if AJson = '' then LContext := TALJSONDocumentA.CreateFromJSONString('{}')
  else LContext := TALJSONDocumentA.CreateFromJSONString(AJson);
  try
    Result := FHandlebars.Render(AFilename, LContext);
  finally
    ALFreeAndNil(LContext);
  end;
end;

{***********************************************************}
// {{upper x}} -> uppercase of the (rendered) first argument.
function TALDUnitXTestHandlebars.HelperUpper(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin
  var LText: AnsiString;
  var LOwned: Boolean;
  var LJsonNode := FHandlebars.ResolveParamValue(AParams, 0, AContext, ADepths, LOwned);
  try
    if LJsonNode <> nil then LText := LJsonNode.Text
    else LText := '';
  finally
    if LOwned then
      ALFreeAndNil(LJsonNode);
  end;
  Result := TALJSONTextNodeA.Create('');
  Result.Text := ALUnicodeUpperCase(LText);
end;

{***********************************************************}
// {{lower x}} -> lowercase of the (rendered) first argument.
function TALDUnitXTestHandlebars.HelperLower(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin
  var LText: AnsiString;
  var LOwned: Boolean;
  var LJsonNode := FHandlebars.ResolveParamValue(AParams, 0, AContext, ADepths, LOwned);
  try
    if LJsonNode <> nil then LText := LJsonNode.Text
    else LText := '';
  finally
    if LOwned then
      ALFreeAndNil(LJsonNode);
  end;
  Result := TALJSONTextNodeA.Create('');
  Result.Text := ALUnicodeLowerCase(LText);
end;

{*********************************************************}
// {{wrap value prefix suffix}} -> prefix + value + suffix.
function TALDUnitXTestHandlebars.HelperWrap(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin
  var LText: AnsiString := '';
  var LOwned: Boolean;
  var LJsonNode := FHandlebars.ResolveParamValue(AParams, 1, AContext, ADepths, LOwned);
  try
    if LJsonNode <> nil then LText := LText + LJsonNode.Text;
  finally
    if LOwned then
      ALFreeAndNil(LJsonNode);
  end;

  LJsonNode := FHandlebars.ResolveParamValue(AParams, 0, AContext, ADepths, LOwned);
  try
    if LJsonNode <> nil then LText := LText + LJsonNode.Text;
  finally
    if LOwned then
      ALFreeAndNil(LJsonNode);
  end;

  LJsonNode := FHandlebars.ResolveParamValue(AParams, 2, AContext, ADepths, LOwned);
  try
    if LJsonNode <> nil then LText := LText + LJsonNode.Text;
  finally
    if LOwned then
      ALFreeAndNil(LJsonNode);
  end;

  Result := TALJSONTextNodeA.Create('');
  Result.Text := LText;
end;

(*$REGION 'Plain mustache {{ }}'*)

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_Mustache_NoTags;
begin
  // m_notags.hbs : just plain text
  CheckEq('just plain text', Render('m_notags.hbs', '{}'));
end;

{********************************************************}
procedure TALDUnitXTestHandlebars.Test_Mustache_SimpleVar;
begin
  // m_var.hbs : {{name}}
  CheckEq('World', Render('m_var.hbs', '{"name":"World"}'));
end;

{********************************************************}
procedure TALDUnitXTestHandlebars.Test_Mustache_VarInText;
begin
  // simple.hbs : Hello {{name}}!
  CheckEq('Hello World!', Render('simple.hbs', '{"name":"World"}'));
end;

{***********************************************************}
procedure TALDUnitXTestHandlebars.Test_Mustache_MultipleVars;
begin
  // m_multi.hbs : {{a}}-{{b}}-{{c}}
  CheckEq('1-2-3', Render('m_multi.hbs', '{"a":"1","b":"2","c":"3"}'));
end;


{*********************************************************}
procedure TALDUnitXTestHandlebars.Test_Mustache_MissingVar;
begin
  // m_missing.hbs : >{{missing}}<
  CheckEq('><', Render('m_missing.hbs', '{"name":"World"}'));
end;

{*********************************************************}
procedure TALDUnitXTestHandlebars.Test_Mustache_DottedPath;
begin
  // path.hbs : {{user.firstname}} {{user.lastname}}
  CheckEq('Bob Jones', Render('path.hbs', '{"user":{"firstname":"Bob","lastname":"Jones"}}'));
end;

{**********************************************************}
procedure TALDUnitXTestHandlebars.Test_Mustache_NumberValue;
begin
  // m_num.hbs : {{count}}
  CheckEq('42', Render('m_num.hbs', '{"count":42}'));
end;

{***********************************************************}
procedure TALDUnitXTestHandlebars.Test_Mustache_BooleanValue;
begin
  // m_bool.hbs : {{active}}
  CheckEq('true', Render('m_bool.hbs', '{"active":true}'));
end;

(*$ENDREGION*)

{$REGION 'Helpers (registered)'}

{*****************************************************************}
procedure TALDUnitXTestHandlebars.Test_Helper_SingleArg_ContextVar;
begin
  // helper.hbs : {{upper name}}
  CheckEq('WORLD', Render('helper.hbs', '{"name":"world"}'));
end;

{********************************************************************}
procedure TALDUnitXTestHandlebars.Test_Helper_SingleArg_QuotedLiteral;
begin
  // h_upper_lit.hbs : {{upper "abc"}}
  CheckEq('ABC', Render('h_upper_lit.hbs', '{}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_Helper_Lower;
begin
  // h_lower.hbs : {{lower name}}
  CheckEq('world', Render('h_lower.hbs', '{"name":"WORLD"}'));
end;

{***********************************************************}
procedure TALDUnitXTestHandlebars.Test_Helper_Concat_TwoVars;
begin
  // h_concat2.hbs : {{concat a b}}
  CheckEq('xy', Render('h_concat2.hbs', '{"a":"x","b":"y"}'));
end;

{**********************************************************************}
procedure TALDUnitXTestHandlebars.Test_Helper_Concat_MixedLiteralAndVar;
begin
  // quoted.hbs : {{concat "Hello, " name "!"}}
  CheckEq('Hello, World!', Render('quoted.hbs', '{"name":"World"}'));
end;

{****************************************************************}
procedure TALDUnitXTestHandlebars.Test_Helper_Wrap_PositionalArgs;
begin
  // h_wrap.hbs : {{wrap name "[" "]"}}
  CheckEq('[x]', Render('h_wrap.hbs', '{"name":"x"}'));
end;

{$ENDREGION}

{$REGION 'Subexpressions ( )'}

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_Subexpr_Simple;
begin
  // s_simple.hbs : {{upper (concat "a" "b")}}
  CheckEq('AB', Render('s_simple.hbs', '{}'));
end;

{*************************************************************}
procedure TALDUnitXTestHandlebars.Test_Subexpr_WithContextVars;
begin
  // s_vars.hbs : {{upper (concat greeting name)}}
  CheckEq('HI BOB', Render('s_vars.hbs', '{"greeting":"hi ","name":"bob"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_Subexpr_AsOnlyArg;
begin
  // s_onlyarg.hbs : {{lower (upper "MiXeD")}}
  CheckEq('mixed', Render('s_onlyarg.hbs', '{}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_Subexpr_Nested;
begin
  // s_nested.hbs : {{upper (concat "a" (concat "b" "c"))}} - subexpression inside a subexpression
  CheckEq('ABC', Render('s_nested.hbs', '{}'));
end;

{********************************************************}
procedure TALDUnitXTestHandlebars.Test_Subexpr_DeepNested;
begin
  // s_deep.hbs : {{concat (concat (concat "1" "2") "3") "4"}}
  CheckEq('1234', Render('s_deep.hbs', '{}'));
end;

{*****************************************************************}
procedure TALDUnitXTestHandlebars.Test_Subexpr_MultipleSubexprArgs;
begin
  // s_multi.hbs : {{concat (upper "ab") "-" (lower "CD")}}
  CheckEq('AB-cd', Render('s_multi.hbs', '{}'));
end;

{****************************************************************}
procedure TALDUnitXTestHandlebars.Test_Subexpr_MixedLiteralAndVar;
begin
  // s_mixed.hbs : {{upper (concat a " " (concat b " " c))}}
  CheckEq('A B C', Render('s_mixed.hbs', '{"a":"a","b":"b","c":"c"}'));
end;

{************************************************************}
procedure TALDUnitXTestHandlebars.Test_Subexpr_TwoSubexprArgs;
begin
  // nested_subexpr.hbs : {{concat (concat a b) (concat c (concat d e))}}
  CheckEq('12345', Render('nested_subexpr.hbs', '{"a":"1","b":"2","c":"3","d":"4","e":"5"}'));
end;

{****************************************************************}
procedure TALDUnitXTestHandlebars.Test_Subexpr_LiteralSpaceInside;
begin
  // subexpr.hbs : {{upper (concat greeting " " name)}}
  CheckEq('HI BOB', Render('subexpr.hbs', '{"greeting":"hi","name":"bob"}'));
end;

{***************************************************************}
procedure TALDUnitXTestHandlebars.Test_Subexpr_LiteralWithEquals;
begin
  // s_litequal.hbs : {{concat (concat "key=" val) "!"}}
  // The quoted literal "key=" sits inside a subexpression and contains the
  // name/value separator '='. Because it is quoted, the '=' must stay part of
  // the literal text and must NOT be parsed as a hash (name=value) argument -
  // otherwise concat would reject it as a named argument or truncate it.
  CheckEq('key=v1!', Render('s_litequal.hbs', '{"val":"v1"}'));
end;

{$ENDREGION}

{$REGION 'Quotes / literals'}

{*************************************************************}
procedure TALDUnitXTestHandlebars.Test_Quote_LiteralWithSpaces;
begin
  // q_spaces.hbs : {{concat "a b" "c"}}
  CheckEq('a bc', Render('q_spaces.hbs', '{}'));
end;

{********************************************************}
procedure TALDUnitXTestHandlebars.Test_Quote_SingleQuotes;
begin
  // q_single.hbs : {{concat 'a' 'b'}}
  CheckEq('ab', Render('q_single.hbs', '{}'));
end;

{************************************************************}
procedure TALDUnitXTestHandlebars.Test_Quote_ParensAreLiteral;
begin
  // q_parens.hbs : {{upper "(not a subexpr)"}} - parentheses inside a quote are literal.
  CheckEq('(NOT A SUBEXPR)', Render('q_parens.hbs', '{}'));
end;

{*******************************************************************}
procedure TALDUnitXTestHandlebars.Test_Quote_ClosingBracesAreLiteral;
begin
  // q_braces.hbs : {{concat "}}" "x"}} - the '}}' inside a quote must NOT close the tag.
  CheckEq('}}x', Render('q_braces.hbs', '{}'));
end;

{***********************************************************************}
procedure TALDUnitXTestHandlebars.Test_Quote_SubexprInsideQuoteIsLiteral;
begin
  // q_sub_literal.hbs : {{upper "(concat a b)"}} - a subexpression inside a quote stays literal.
  CheckEq('(CONCAT A B)', Render('q_sub_literal.hbs', '{"a":"x","b":"y"}'));
end;

{$ENDREGION}

(*$REGION 'Blocks {{#x}}..{{/x}}'*)

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_If_True;
begin
  // block_if.hbs : {{#if active}}ON{{else}}OFF{{/if}}
  CheckEq('ON', Render('block_if.hbs', '{"active":true}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_If_False;
begin
  CheckEq('OFF', Render('block_if.hbs', '{"active":false}'));
end;

{**********************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_If_NoElse_True;
begin
  // b_if_noelse.hbs : [{{#if active}}Y{{/if}}]
  CheckEq('[Y]', Render('b_if_noelse.hbs', '{"active":true}'));
end;

{***********************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_If_NoElse_False;
begin
  CheckEq('[]', Render('b_if_noelse.hbs', '{"active":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_Unless_True;
begin
  // b_unless.hbs : {{#unless active}}N{{/unless}}
  CheckEq('N', Render('b_unless.hbs', '{"active":false}'));
end;

{************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_Each;
begin
  // block_each.hbs : {{#each items}}[{{this}}]{{/each}}
  CheckEq('[a][b][c]', Render('block_each.hbs', '{"items":["a","b","c"]}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_Each_Empty;
begin
  // b_each_wrap.hbs : >{{#each items}}[{{this}}]{{/each}}<
  CheckEq('><', Render('b_each_wrap.hbs', '{"items":[]}'));
end;

{************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_With;
begin
  // b_with_first.hbs : {{#with user}}{{firstname}}{{/with}}
  CheckEq('Jo', Render('b_with_first.hbs', '{"user":{"firstname":"Jo"}}'));
end;

{**********************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_WithMultiField;
begin
  // block_with.hbs : {{#with user}}{{firstname}}-{{lastname}}{{/with}}
  CheckEq('J-D', Render('block_with.hbs', '{"user":{"firstname":"J","lastname":"D"}}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_Nested;
begin
  // block_nested.hbs : {{#if active}}{{#each items}}<{{this}}>{{/each}}{{else}}none{{/if}}
  CheckEq('<a><b>', Render('block_nested.hbs', '{"active":true,"items":["a","b"]}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_NestedElse;
begin
  CheckEq('none', Render('block_nested.hbs', '{"active":false,"items":["a","b"]}'));
end;

{*************************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_HelperInsideBlock;
begin
  // b_helper.hbs : {{#if active}}{{upper letters}}{{/if}}
  CheckEq('AB', Render('b_helper.hbs', '{"active":true,"letters":"ab"}'));
end;

(*$ENDREGION*)

(*$REGION 'Partials {{> x}}'*)

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_Partial_Plain;
begin
(*
  // partial_host.hbs : <{{> inner}}>   inner -> p_plain.hbs : PARTIAL
  CheckEq('<PARTIAL>', Render('partial_host.hbs', '{}', Inner('p_plain.hbs')));
*)
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_Partial_WithVar;
begin
(*
  // partial_host.hbs : <{{> inner}}>   inner -> p_var.hbs : [{{name}}]
  CheckEq('<[World]>', Render('partial_host.hbs', '{"name":"World"}', Inner('p_var.hbs')));
*)
end;

{********************************************************}
procedure TALDUnitXTestHandlebars.Test_Partial_WithHelper;
begin
(*
  // partial_host.hbs : <{{> inner}}>   inner -> p_helper.hbs : {{upper name}}
  CheckEq('<WORLD>', Render('partial_host.hbs', '{"name":"world"}', Inner('p_helper.hbs')));
*)
end;

(*$ENDREGION*)

{$REGION 'Kitchen sink'}

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_KitchenSink;
begin
  // kitchensink.hbs :
  //   Dear {{upper name}}, you have {{count}} item(s): {{#if active}}{{#each items}}[{{this}}]{{/each}}{{else}}(inactive){{/if}} -- {{concat "signed: " (upper author)}}
  CheckEq(
    'Dear BOB, you have 2 item(s): [x][y] -- signed: ALICE',
    Render('kitchensink.hbs', '{"name":"bob","count":2,"active":true,"items":["x","y"],"author":"alice"}'));
end;

{$ENDREGION}

{$REGION 'Additional edge cases'}

{*****************************************************************}
procedure TALDUnitXTestHandlebars.Test_Path_SlashAndCurrentAliases;
begin
  CheckEq(
    'Bob Jones Bob HQ HQ:Bob/Jones',
    Render('edge_path_aliases.hbs', '{"site":"HQ","user":{"firstname":"Bob","lastname":"Jones"}}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_Truthy_NullFalse;
begin
  CheckEq('F', Render('edge_truthiness.hbs', '{"value":null}'));
end;

{************************************************************}
procedure TALDUnitXTestHandlebars.Test_Truthy_EmptyObjectTrue;
begin
  CheckEq('T', Render('edge_truthiness.hbs', '{"value":{}}'));
end;

{**************************************************************}
procedure TALDUnitXTestHandlebars.Test_Truthy_NonEmptyArrayTrue;
begin
  CheckEq('T', Render('edge_truthiness.hbs', '{"value":[0]}'));
end;

{***********************************************************}
procedure TALDUnitXTestHandlebars.Test_Truthy_FloatZeroFalse;
begin
  CheckEq('F', Render('edge_truthiness.hbs', '{"value":0.0}'));
end;

{**********************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_If_IncludeZero;
begin
  CheckEq('T', Render('edge_if_include_zero.hbs', '{"value":0}'));
end;

{**************************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_Unless_IncludeZero;
begin
  CheckEq('F', Render('edge_unless_include_zero.hbs', '{"value":0}'));
end;

{**********************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_If_NullLiteral;
begin
  CheckEq('F', Render('edge_if_null_literal.hbs', '{}'));
end;

{***************************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_With_ElseForMissing;
begin
  CheckEq('missing', Render('edge_with_else.hbs', '{}'));
end;

{******************************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_With_ElseForEmptyArray;
begin
  CheckEq('missing', Render('edge_with_else.hbs', '{"user":[]}'));
end;

{**************************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_With_ScalarContext;
begin
  CheckEq('ok', Render('edge_with_scalar.hbs', '{"label":"ok"}'));
end;

{***************************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_Each_ElseForMissing;
begin
  CheckEq('empty', Render('edge_each_else.hbs', '{}'));
end;

{**************************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_Each_ElseForScalar;
begin
  CheckEq('empty', Render('edge_each_else.hbs', '{"items":"x"}'));
end;

{********************************************************************}
procedure TALDUnitXTestHandlebars.Test_Block_Each_LookupSubexpression;
begin
  CheckEq('0=a;1=b;', Render('edge_each_lookup.hbs', '{"items":["a","b"]}'));
end;

{*******************************************************************************}
procedure TALDUnitXTestHandlebars.Test_Lookup_MissingOutOfRangeAndInvalidTargets;
begin
  CheckEq(
    '><|><|><|><|><',
    Render('edge_lookup_miss.hbs', '{"arr":["a","b"],"obj":{"x":"y"},"value":"not-an-object"}'));
end;

{**************************************************************************}
procedure TALDUnitXTestHandlebars.Test_Lookup_LiteralKeysWithDotsAndSlashes;
begin
  CheckEq('dot|slash', Render('edge_lookup_literal_keys.hbs', '{"obj":{"a.b":"dot","a/b":"slash"}}'));
end;

{*******************************************************************}
procedure TALDUnitXTestHandlebars.Test_Lookup_KeyDataVariableAsParam;
begin
  CheckEq(
    'A=1;B=2;',
    Render('edge_lookup_key_param.hbs', '{"labels":{"a":"A","b":"B"},"obj":{"a":"1","b":"2"}}'));
end;

{*******************************************************************}
procedure TALDUnitXTestHandlebars.Test_Literals_NullNegativeAndInt64;
begin
  CheckEq('null|-7|2147483648', Render('edge_literals.hbs', '{}'));
end;

{*************************************************************}
procedure TALDUnitXTestHandlebars.Test_Quote_EscapedQuoteChars;
begin
  CheckEq('a&#34;b c&#39;d', Render('edge_quote_escape.hbs', '{}'));
end;

{*************************************************************************************}
procedure TALDUnitXTestHandlebars.Test_Partial_HashBorrowedValueAndMissingDeletesField;
begin
  CheckEq(
    'Guest User|A&#38;B|<>|off',
    Render('edge_partial_hash_values.hbs', '{"display":"A&B","name":"Old","email":"old@example","showEmail":true}'));
end;

{****************************************************************}
procedure TALDUnitXTestHandlebars.Test_Partial_HashSubexpressions;
begin
  CheckEq(
    'MEMBER|Ann Lee|<a&#38;b@example>|on',
    Render('edge_partial_hash_subexpr.hbs', '{"role":"member","first":"Ann","last":"Lee","email":"a&b@example","hidden":false}'));
end;

{**********************************************************************}
procedure TALDUnitXTestHandlebars.Test_Partial_NonObjectContextWithHash;
begin
  CheckEq('Scalar||<x@y>|on', Render('edge_partial_nonobject_context.hbs', '{"name":"Bob"}'));
end;

{$ENDREGION}

{$REGION 'Data-driven render tests'}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_001;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":0,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_001;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":0,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_002;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":0,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_002;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":0,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_003;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":0,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_003;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":0,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_004;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":0,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_004;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":0,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_005;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":0,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_005;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":0,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_006;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":0,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_006;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":0,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_007;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":0,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_007;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":0,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_008;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":0,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_008;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":0,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_009;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_009;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_010;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_010;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_011;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_011;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_012;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_012;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_013;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_013;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_014;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_014;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_015;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_015;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_016;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_016;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_017;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":2,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_017;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":2,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_018;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":2,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_018;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":2,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_019;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":2,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_019;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":2,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_020;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":2,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_020;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":2,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_021;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":2,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_021;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":2,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_022;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":2,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_022;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":2,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_023;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":2,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_023;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":2,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_024;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":2,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_024;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":2,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_025;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_025;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_026;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_026;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_027;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_027;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_028;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_028;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_029;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_029;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_030;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_030;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_031;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_031;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_032;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_032;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_033;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":-1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_033;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":-1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_034;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":-1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_034;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":-1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_035;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":-1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_035;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":-1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_036;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":-1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_036;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":-1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_037;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":-1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_037;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":-1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_038;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":-1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_038;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":-1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_039;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":-1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_039;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":-1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_040;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":-1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_040;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":-1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_041;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":2.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_041;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":2.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_042;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":2.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_042;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":2.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_043;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":2.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_043;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":2.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_044;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":2.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_044;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":2.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_045;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":2.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_045;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":2.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_046;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":2.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_046;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":2.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_047;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":2.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_047;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":2.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_048;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":2.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_048;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":2.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_049;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":12.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_049;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":12.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_050;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":12.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_050;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":12.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_051;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":12.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_051;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":12.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_052;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":12.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_052;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":12.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_053;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":12.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_053;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":12.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_054;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":12.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_054;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":12.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_055;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":12.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_055;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":12.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_056;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":12.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_056;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":12.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_057;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":100,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_057;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":100,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_058;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":100,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_058;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":100,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_059;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":100,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_059;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":100,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_060;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":100,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_060;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":100,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_061;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":100,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_061;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":100,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_062;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":100,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_062;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":100,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_063;
begin
  CheckEq('T', Render('g0001.hbs', '{"a":100,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_063;
begin
  CheckEq('true', Render('g0002.hbs', '{"a":100,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_blk_064;
begin
  CheckEq('F', Render('g0001.hbs', '{"a":100,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gt_inl_064;
begin
  CheckEq('false', Render('g0002.hbs', '{"a":100,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_065;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":0,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_065;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":0,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_066;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":0,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_066;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":0,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_067;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":0,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_067;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":0,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_068;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":0,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_068;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":0,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_069;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":0,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_069;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":0,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_070;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":0,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_070;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":0,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_071;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":0,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_071;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":0,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_072;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":0,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_072;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":0,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_073;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_073;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_074;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_074;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_075;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_075;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_076;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_076;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_077;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_077;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_078;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_078;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_079;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_079;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_080;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_080;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_081;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":2,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_081;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":2,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_082;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":2,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_082;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":2,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_083;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":2,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_083;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":2,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_084;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":2,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_084;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":2,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_085;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":2,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_085;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":2,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_086;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":2,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_086;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":2,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_087;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":2,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_087;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":2,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_088;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":2,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_088;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":2,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_089;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_089;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_090;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_090;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_091;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_091;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_092;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_092;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_093;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_093;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_094;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_094;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_095;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_095;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_096;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_096;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_097;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":-1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_097;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":-1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_098;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":-1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_098;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":-1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_099;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":-1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_099;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":-1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_100;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":-1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_100;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":-1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_101;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":-1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_101;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":-1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_102;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":-1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_102;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":-1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_103;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":-1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_103;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":-1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_104;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":-1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_104;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":-1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_105;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":2.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_105;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":2.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_106;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":2.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_106;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":2.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_107;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":2.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_107;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":2.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_108;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":2.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_108;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":2.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_109;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":2.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_109;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":2.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_110;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":2.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_110;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":2.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_111;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":2.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_111;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":2.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_112;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":2.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_112;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":2.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_113;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":12.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_113;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":12.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_114;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":12.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_114;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":12.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_115;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":12.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_115;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":12.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_116;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":12.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_116;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":12.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_117;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":12.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_117;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":12.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_118;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":12.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_118;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":12.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_119;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":12.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_119;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":12.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_120;
begin
  CheckEq('T', Render('g0003.hbs', '{"a":12.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_120;
begin
  CheckEq('true', Render('g0004.hbs', '{"a":12.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_121;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":100,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_121;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":100,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_122;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":100,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_122;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":100,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_123;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":100,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_123;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":100,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_124;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":100,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_124;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":100,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_125;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":100,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_125;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":100,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_126;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":100,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_126;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":100,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_127;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":100,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_127;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":100,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_blk_128;
begin
  CheckEq('F', Render('g0003.hbs', '{"a":100,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lt_inl_128;
begin
  CheckEq('false', Render('g0004.hbs', '{"a":100,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_129;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":0,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_129;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":0,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_130;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":0,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_130;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":0,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_131;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":0,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_131;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":0,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_132;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":0,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_132;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":0,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_133;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":0,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_133;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":0,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_134;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":0,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_134;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":0,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_135;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":0,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_135;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":0,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_136;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":0,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_136;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":0,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_137;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":1,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_137;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":1,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_138;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":1,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_138;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":1,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_139;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":1,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_139;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":1,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_140;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":1,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_140;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":1,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_141;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":1,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_141;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":1,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_142;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":1,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_142;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":1,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_143;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":1,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_143;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":1,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_144;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":1,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_144;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":1,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_145;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":2,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_145;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":2,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_146;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":2,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_146;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":2,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_147;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":2,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_147;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":2,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_148;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":2,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_148;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":2,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_149;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":2,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_149;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":2,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_150;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":2,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_150;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":2,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_151;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":2,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_151;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":2,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_152;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":2,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_152;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":2,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_153;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":5,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_153;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":5,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_154;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":5,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_154;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":5,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_155;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":5,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_155;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":5,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_156;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":5,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_156;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":5,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_157;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":5,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_157;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":5,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_158;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":5,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_158;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":5,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_159;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":5,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_159;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":5,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_160;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":5,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_160;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":5,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_161;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":-1,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_161;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":-1,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_162;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":-1,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_162;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":-1,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_163;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":-1,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_163;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":-1,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_164;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":-1,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_164;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":-1,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_165;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":-1,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_165;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":-1,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_166;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":-1,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_166;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":-1,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_167;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":-1,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_167;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":-1,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_168;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":-1,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_168;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":-1,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_169;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":2.5,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_169;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":2.5,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_170;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":2.5,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_170;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":2.5,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_171;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":2.5,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_171;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":2.5,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_172;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":2.5,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_172;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":2.5,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_173;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":2.5,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_173;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":2.5,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_174;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":2.5,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_174;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":2.5,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_175;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":2.5,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_175;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":2.5,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_176;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":2.5,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_176;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":2.5,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_177;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":12.5,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_177;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":12.5,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_178;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":12.5,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_178;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":12.5,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_179;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":12.5,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_179;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":12.5,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_180;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":12.5,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_180;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":12.5,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_181;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":12.5,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_181;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":12.5,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_182;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":12.5,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_182;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":12.5,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_183;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":12.5,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_183;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":12.5,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_184;
begin
  CheckEq('F', Render('g0005.hbs', '{"a":12.5,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_184;
begin
  CheckEq('false', Render('g0006.hbs', '{"a":12.5,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_185;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":100,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_185;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":100,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_186;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":100,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_186;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":100,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_187;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":100,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_187;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":100,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_188;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":100,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_188;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":100,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_189;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":100,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_189;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":100,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_190;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":100,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_190;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":100,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_191;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":100,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_191;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":100,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_blk_192;
begin
  CheckEq('T', Render('g0005.hbs', '{"a":100,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_gte_inl_192;
begin
  CheckEq('true', Render('g0006.hbs', '{"a":100,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_193;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":0,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_193;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":0,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_194;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":0,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_194;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":0,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_195;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":0,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_195;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":0,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_196;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":0,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_196;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":0,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_197;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":0,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_197;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":0,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_198;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":0,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_198;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":0,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_199;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":0,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_199;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":0,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_200;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":0,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_200;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":0,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_201;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":1,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_201;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":1,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_202;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":1,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_202;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":1,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_203;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":1,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_203;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":1,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_204;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":1,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_204;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":1,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_205;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":1,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_205;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":1,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_206;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":1,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_206;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":1,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_207;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":1,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_207;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":1,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_208;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":1,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_208;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":1,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_209;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":2,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_209;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":2,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_210;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":2,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_210;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":2,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_211;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":2,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_211;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":2,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_212;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":2,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_212;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":2,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_213;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":2,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_213;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":2,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_214;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":2,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_214;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":2,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_215;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":2,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_215;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":2,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_216;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":2,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_216;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":2,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_217;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":5,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_217;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":5,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_218;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":5,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_218;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":5,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_219;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":5,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_219;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":5,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_220;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":5,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_220;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":5,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_221;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":5,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_221;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":5,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_222;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":5,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_222;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":5,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_223;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":5,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_223;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":5,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_224;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":5,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_224;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":5,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_225;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":-1,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_225;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":-1,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_226;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":-1,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_226;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":-1,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_227;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":-1,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_227;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":-1,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_228;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":-1,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_228;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":-1,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_229;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":-1,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_229;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":-1,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_230;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":-1,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_230;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":-1,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_231;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":-1,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_231;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":-1,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_232;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":-1,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_232;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":-1,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_233;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":2.5,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_233;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":2.5,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_234;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":2.5,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_234;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":2.5,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_235;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":2.5,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_235;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":2.5,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_236;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":2.5,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_236;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":2.5,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_237;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":2.5,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_237;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":2.5,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_238;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":2.5,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_238;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":2.5,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_239;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":2.5,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_239;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":2.5,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_240;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":2.5,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_240;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":2.5,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_241;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":12.5,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_241;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":12.5,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_242;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":12.5,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_242;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":12.5,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_243;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":12.5,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_243;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":12.5,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_244;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":12.5,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_244;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":12.5,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_245;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":12.5,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_245;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":12.5,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_246;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":12.5,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_246;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":12.5,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_247;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":12.5,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_247;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":12.5,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_248;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":12.5,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_248;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":12.5,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_249;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":100,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_249;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":100,"b":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_250;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":100,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_250;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":100,"b":1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_251;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":100,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_251;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":100,"b":2}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_252;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":100,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_252;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":100,"b":5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_253;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":100,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_253;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":100,"b":-1}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_254;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":100,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_254;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":100,"b":2.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_255;
begin
  CheckEq('F', Render('g0007.hbs', '{"a":100,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_255;
begin
  CheckEq('false', Render('g0008.hbs', '{"a":100,"b":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_blk_256;
begin
  CheckEq('T', Render('g0007.hbs', '{"a":100,"b":100}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lte_inl_256;
begin
  CheckEq('true', Render('g0008.hbs', '{"a":100,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_257;
begin
  CheckEq('T', Render('g0009.hbs', '{"a":0,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_257;
begin
  CheckEq('true', Render('g0010.hbs', '{"a":0,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_258;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":0,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_258;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":0,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_259;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":0,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_259;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":0,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_260;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":0,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_260;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":0,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_261;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":0,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_261;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":0,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_262;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":0,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_262;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":0,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_263;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":0,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_263;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":0,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_264;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":0,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_264;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":0,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_265;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_265;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_266;
begin
  CheckEq('T', Render('g0009.hbs', '{"a":1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_266;
begin
  CheckEq('true', Render('g0010.hbs', '{"a":1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_267;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_267;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_268;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_268;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_269;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_269;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_270;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_270;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_271;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_271;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_272;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_272;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_273;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":2,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_273;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":2,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_274;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":2,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_274;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":2,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_275;
begin
  CheckEq('T', Render('g0009.hbs', '{"a":2,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_275;
begin
  CheckEq('true', Render('g0010.hbs', '{"a":2,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_276;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":2,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_276;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":2,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_277;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":2,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_277;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":2,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_278;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":2,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_278;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":2,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_279;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":2,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_279;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":2,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_280;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":2,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_280;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":2,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_281;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_281;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_282;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_282;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_283;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_283;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_284;
begin
  CheckEq('T', Render('g0009.hbs', '{"a":5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_284;
begin
  CheckEq('true', Render('g0010.hbs', '{"a":5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_285;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_285;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_286;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_286;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_287;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_287;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_288;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_288;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_289;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":-1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_289;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":-1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_290;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":-1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_290;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":-1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_291;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":-1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_291;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":-1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_292;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":-1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_292;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":-1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_293;
begin
  CheckEq('T', Render('g0009.hbs', '{"a":-1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_293;
begin
  CheckEq('true', Render('g0010.hbs', '{"a":-1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_294;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":-1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_294;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":-1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_295;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":-1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_295;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":-1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_296;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":-1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_296;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":-1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_297;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":2.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_297;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":2.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_298;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":2.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_298;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":2.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_299;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":2.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_299;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":2.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_300;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":2.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_300;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":2.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_301;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":2.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_301;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":2.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_302;
begin
  CheckEq('T', Render('g0009.hbs', '{"a":2.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_302;
begin
  CheckEq('true', Render('g0010.hbs', '{"a":2.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_303;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":2.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_303;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":2.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_304;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":2.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_304;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":2.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_305;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":12.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_305;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":12.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_306;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":12.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_306;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":12.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_307;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":12.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_307;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":12.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_308;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":12.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_308;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":12.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_309;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":12.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_309;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":12.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_310;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":12.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_310;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":12.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_311;
begin
  CheckEq('T', Render('g0009.hbs', '{"a":12.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_311;
begin
  CheckEq('true', Render('g0010.hbs', '{"a":12.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_312;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":12.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_312;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":12.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_313;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":100,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_313;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":100,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_314;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":100,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_314;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":100,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_315;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":100,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_315;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":100,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_316;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":100,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_316;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":100,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_317;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":100,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_317;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":100,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_318;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":100,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_318;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":100,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_319;
begin
  CheckEq('F', Render('g0009.hbs', '{"a":100,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_319;
begin
  CheckEq('false', Render('g0010.hbs', '{"a":100,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_blk_320;
begin
  CheckEq('T', Render('g0009.hbs', '{"a":100,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_eq_inl_320;
begin
  CheckEq('true', Render('g0010.hbs', '{"a":100,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_321;
begin
  CheckEq('F', Render('g0011.hbs', '{"a":0,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_321;
begin
  CheckEq('false', Render('g0012.hbs', '{"a":0,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_322;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":0,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_322;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":0,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_323;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":0,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_323;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":0,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_324;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":0,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_324;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":0,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_325;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":0,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_325;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":0,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_326;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":0,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_326;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":0,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_327;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":0,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_327;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":0,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_328;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":0,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_328;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":0,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_329;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_329;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_330;
begin
  CheckEq('F', Render('g0011.hbs', '{"a":1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_330;
begin
  CheckEq('false', Render('g0012.hbs', '{"a":1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_331;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_331;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_332;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_332;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_333;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_333;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_334;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_334;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_335;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_335;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_336;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_336;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_337;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":2,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_337;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":2,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_338;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":2,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_338;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":2,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_339;
begin
  CheckEq('F', Render('g0011.hbs', '{"a":2,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_339;
begin
  CheckEq('false', Render('g0012.hbs', '{"a":2,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_340;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":2,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_340;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":2,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_341;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":2,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_341;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":2,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_342;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":2,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_342;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":2,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_343;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":2,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_343;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":2,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_344;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":2,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_344;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":2,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_345;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_345;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_346;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_346;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_347;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_347;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_348;
begin
  CheckEq('F', Render('g0011.hbs', '{"a":5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_348;
begin
  CheckEq('false', Render('g0012.hbs', '{"a":5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_349;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_349;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_350;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_350;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_351;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_351;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_352;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_352;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_353;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":-1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_353;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":-1,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_354;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":-1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_354;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":-1,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_355;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":-1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_355;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":-1,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_356;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":-1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_356;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":-1,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_357;
begin
  CheckEq('F', Render('g0011.hbs', '{"a":-1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_357;
begin
  CheckEq('false', Render('g0012.hbs', '{"a":-1,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_358;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":-1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_358;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":-1,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_359;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":-1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_359;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":-1,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_360;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":-1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_360;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":-1,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_361;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":2.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_361;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":2.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_362;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":2.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_362;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":2.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_363;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":2.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_363;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":2.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_364;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":2.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_364;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":2.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_365;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":2.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_365;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":2.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_366;
begin
  CheckEq('F', Render('g0011.hbs', '{"a":2.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_366;
begin
  CheckEq('false', Render('g0012.hbs', '{"a":2.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_367;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":2.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_367;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":2.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_368;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":2.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_368;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":2.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_369;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":12.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_369;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":12.5,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_370;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":12.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_370;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":12.5,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_371;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":12.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_371;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":12.5,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_372;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":12.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_372;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":12.5,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_373;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":12.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_373;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":12.5,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_374;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":12.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_374;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":12.5,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_375;
begin
  CheckEq('F', Render('g0011.hbs', '{"a":12.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_375;
begin
  CheckEq('false', Render('g0012.hbs', '{"a":12.5,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_376;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":12.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_376;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":12.5,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_377;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":100,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_377;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":100,"b":0}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_378;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":100,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_378;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":100,"b":1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_379;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":100,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_379;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":100,"b":2}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_380;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":100,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_380;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":100,"b":5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_381;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":100,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_381;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":100,"b":-1}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_382;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":100,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_382;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":100,"b":2.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_383;
begin
  CheckEq('T', Render('g0011.hbs', '{"a":100,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_383;
begin
  CheckEq('true', Render('g0012.hbs', '{"a":100,"b":12.5}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_blk_384;
begin
  CheckEq('F', Render('g0011.hbs', '{"a":100,"b":100}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_ne_inl_384;
begin
  CheckEq('false', Render('g0012.hbs', '{"a":100,"b":100}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_001;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":true,"b":true}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_001;
begin
  CheckEq('T', Render('g0014.hbs', '{"a":true,"b":true}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_001;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":true,"b":true}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_001;
begin
  CheckEq('true', Render('g0016.hbs', '{"a":true,"b":true}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_002;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":true,"b":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_002;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":true,"b":false}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_002;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":true,"b":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_002;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":true,"b":false}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_003;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":true,"b":1}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_003;
begin
  CheckEq('T', Render('g0014.hbs', '{"a":true,"b":1}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_003;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":true,"b":1}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_003;
begin
  CheckEq('true', Render('g0016.hbs', '{"a":true,"b":1}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_004;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":true,"b":0}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_004;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":true,"b":0}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_004;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":true,"b":0}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_004;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":true,"b":0}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_005;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":true,"b":"x"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_005;
begin
  CheckEq('T', Render('g0014.hbs', '{"a":true,"b":"x"}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_005;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":true,"b":"x"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_005;
begin
  CheckEq('true', Render('g0016.hbs', '{"a":true,"b":"x"}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_006;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":true,"b":""}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_006;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":true,"b":""}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_006;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":true,"b":""}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_006;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":true,"b":""}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_007;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":false,"b":true}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_007;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":false,"b":true}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_007;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":false,"b":true}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_007;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":false,"b":true}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_008;
begin
  CheckEq('F', Render('g0013.hbs', '{"a":false,"b":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_008;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":false,"b":false}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_008;
begin
  CheckEq('false', Render('g0015.hbs', '{"a":false,"b":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_008;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":false,"b":false}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_009;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":false,"b":1}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_009;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":false,"b":1}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_009;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":false,"b":1}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_009;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":false,"b":1}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_010;
begin
  CheckEq('F', Render('g0013.hbs', '{"a":false,"b":0}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_010;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":false,"b":0}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_010;
begin
  CheckEq('false', Render('g0015.hbs', '{"a":false,"b":0}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_010;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":false,"b":0}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_011;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":false,"b":"x"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_011;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":false,"b":"x"}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_011;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":false,"b":"x"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_011;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":false,"b":"x"}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_012;
begin
  CheckEq('F', Render('g0013.hbs', '{"a":false,"b":""}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_012;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":false,"b":""}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_012;
begin
  CheckEq('false', Render('g0015.hbs', '{"a":false,"b":""}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_012;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":false,"b":""}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_013;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":1,"b":true}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_013;
begin
  CheckEq('T', Render('g0014.hbs', '{"a":1,"b":true}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_013;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":1,"b":true}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_013;
begin
  CheckEq('true', Render('g0016.hbs', '{"a":1,"b":true}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_014;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":1,"b":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_014;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":1,"b":false}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_014;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":1,"b":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_014;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":1,"b":false}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_015;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":1,"b":1}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_015;
begin
  CheckEq('T', Render('g0014.hbs', '{"a":1,"b":1}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_015;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":1,"b":1}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_015;
begin
  CheckEq('true', Render('g0016.hbs', '{"a":1,"b":1}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_016;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":1,"b":0}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_016;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":1,"b":0}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_016;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":1,"b":0}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_016;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":1,"b":0}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_017;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":1,"b":"x"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_017;
begin
  CheckEq('T', Render('g0014.hbs', '{"a":1,"b":"x"}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_017;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":1,"b":"x"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_017;
begin
  CheckEq('true', Render('g0016.hbs', '{"a":1,"b":"x"}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_018;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":1,"b":""}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_018;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":1,"b":""}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_018;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":1,"b":""}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_018;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":1,"b":""}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_019;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":0,"b":true}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_019;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":0,"b":true}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_019;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":0,"b":true}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_019;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":0,"b":true}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_020;
begin
  CheckEq('F', Render('g0013.hbs', '{"a":0,"b":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_020;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":0,"b":false}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_020;
begin
  CheckEq('false', Render('g0015.hbs', '{"a":0,"b":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_020;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":0,"b":false}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_021;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":0,"b":1}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_021;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":0,"b":1}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_021;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":0,"b":1}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_021;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":0,"b":1}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_022;
begin
  CheckEq('F', Render('g0013.hbs', '{"a":0,"b":0}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_022;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":0,"b":0}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_022;
begin
  CheckEq('false', Render('g0015.hbs', '{"a":0,"b":0}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_022;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":0,"b":0}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_023;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":0,"b":"x"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_023;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":0,"b":"x"}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_023;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":0,"b":"x"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_023;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":0,"b":"x"}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_024;
begin
  CheckEq('F', Render('g0013.hbs', '{"a":0,"b":""}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_024;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":0,"b":""}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_024;
begin
  CheckEq('false', Render('g0015.hbs', '{"a":0,"b":""}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_024;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":0,"b":""}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_025;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":"x","b":true}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_025;
begin
  CheckEq('T', Render('g0014.hbs', '{"a":"x","b":true}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_025;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":"x","b":true}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_025;
begin
  CheckEq('true', Render('g0016.hbs', '{"a":"x","b":true}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_026;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":"x","b":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_026;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":"x","b":false}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_026;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":"x","b":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_026;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":"x","b":false}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_027;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":"x","b":1}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_027;
begin
  CheckEq('T', Render('g0014.hbs', '{"a":"x","b":1}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_027;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":"x","b":1}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_027;
begin
  CheckEq('true', Render('g0016.hbs', '{"a":"x","b":1}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_028;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":"x","b":0}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_028;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":"x","b":0}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_028;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":"x","b":0}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_028;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":"x","b":0}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_029;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":"x","b":"x"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_029;
begin
  CheckEq('T', Render('g0014.hbs', '{"a":"x","b":"x"}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_029;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":"x","b":"x"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_029;
begin
  CheckEq('true', Render('g0016.hbs', '{"a":"x","b":"x"}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_030;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":"x","b":""}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_030;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":"x","b":""}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_030;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":"x","b":""}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_030;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":"x","b":""}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_031;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":"","b":true}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_031;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":"","b":true}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_031;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":"","b":true}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_031;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":"","b":true}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_032;
begin
  CheckEq('F', Render('g0013.hbs', '{"a":"","b":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_032;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":"","b":false}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_032;
begin
  CheckEq('false', Render('g0015.hbs', '{"a":"","b":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_032;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":"","b":false}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_033;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":"","b":1}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_033;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":"","b":1}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_033;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":"","b":1}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_033;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":"","b":1}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_034;
begin
  CheckEq('F', Render('g0013.hbs', '{"a":"","b":0}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_034;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":"","b":0}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_034;
begin
  CheckEq('false', Render('g0015.hbs', '{"a":"","b":0}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_034;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":"","b":0}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_035;
begin
  CheckEq('T', Render('g0013.hbs', '{"a":"","b":"x"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_035;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":"","b":"x"}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_035;
begin
  CheckEq('true', Render('g0015.hbs', '{"a":"","b":"x"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_035;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":"","b":"x"}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_blk_036;
begin
  CheckEq('F', Render('g0013.hbs', '{"a":"","b":""}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_blk_036;
begin
  CheckEq('F', Render('g0014.hbs', '{"a":"","b":""}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or_inl_036;
begin
  CheckEq('false', Render('g0015.hbs', '{"a":"","b":""}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and_inl_036;
begin
  CheckEq('false', Render('g0016.hbs', '{"a":"","b":""}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_not_blk_037;
begin
  CheckEq('F', Render('g0017.hbs', '{"a":true}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_not_inl_037;
begin
  CheckEq('false', Render('g0018.hbs', '{"a":true}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_not_blk_038;
begin
  CheckEq('T', Render('g0017.hbs', '{"a":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_not_inl_038;
begin
  CheckEq('true', Render('g0018.hbs', '{"a":false}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_not_blk_039;
begin
  CheckEq('F', Render('g0017.hbs', '{"a":1}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_not_inl_039;
begin
  CheckEq('false', Render('g0018.hbs', '{"a":1}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_not_blk_040;
begin
  CheckEq('T', Render('g0017.hbs', '{"a":0}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_not_inl_040;
begin
  CheckEq('true', Render('g0018.hbs', '{"a":0}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_not_blk_041;
begin
  CheckEq('F', Render('g0017.hbs', '{"a":"x"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_not_inl_041;
begin
  CheckEq('false', Render('g0018.hbs', '{"a":"x"}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_not_blk_042;
begin
  CheckEq('T', Render('g0017.hbs', '{"a":""}'));
end;

{*******************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_not_inl_042;
begin
  CheckEq('true', Render('g0018.hbs', '{"a":""}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or3_a;
begin
  CheckEq('T', Render('g0019.hbs', '{"a":false,"b":false,"c":true}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_or3_b;
begin
  CheckEq('F', Render('g0019.hbs', '{"a":false,"b":false,"c":false}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and3_a;
begin
  CheckEq('T', Render('g0020.hbs', '{"a":true,"b":true,"c":true}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_logic_and3_b;
begin
  CheckEq('F', Render('g0020.hbs', '{"a":true,"b":false,"c":true}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lit_gt_a;
begin
  CheckEq('true', Render('g0021.hbs', '{}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lit_gt_b;
begin
  CheckEq('false', Render('g0022.hbs', '{}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lit_eq_a;
begin
  CheckEq('true', Render('g0023.hbs', '{}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lit_eq_b;
begin
  CheckEq('false', Render('g0024.hbs', '{}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lit_lte_a;
begin
  CheckEq('true', Render('g0025.hbs', '{}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_cmp_lit_block;
begin
  CheckEq('big', Render('g0026.hbs', '{"count":42}'));
end;

{***********************************************}
procedure TALDUnitXTestHandlebars.Test_var_basic;
begin
  CheckEq('hi', Render('g0027.hbs', '{"x":"hi"}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_var_in_text;
begin
  CheckEq('a-b', Render('g0028.hbs', '{"x":"-"}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_var_missing;
begin
  CheckEq('><', Render('g0029.hbs', '{"x":"hi"}'));
end;

{*********************************************}
procedure TALDUnitXTestHandlebars.Test_var_two;
begin
  CheckEq('12', Render('g0030.hbs', '{"a":"1","b":"2"}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_var_three_sep;
begin
  CheckEq('1/2/3', Render('g0031.hbs', '{"a":"1","b":"2","c":"3"}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_var_number_int;
begin
  CheckEq('42', Render('g0032.hbs', '{"n":42}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_var_number_neg;
begin
  CheckEq('-7', Render('g0032.hbs', '{"n":-7}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_var_number_float;
begin
  CheckEq('12.5', Render('g0032.hbs', '{"n":12.5}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_var_number_zero;
begin
  CheckEq('0', Render('g0032.hbs', '{"n":0}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_var_bool_true;
begin
  CheckEq('true', Render('g0033.hbs', '{"b":true}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_var_bool_false;
begin
  CheckEq('false', Render('g0033.hbs', '{"b":false}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_var_empty_string;
begin
  CheckEq('><', Render('g0034.hbs', '{"s":""}'));
end;


{***************************************************}
procedure TALDUnitXTestHandlebars.Test_var_only_text;
begin
  CheckEq('no tags here', Render('g0036.hbs', '{}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_var_dotted_2;
begin
  CheckEq('v', Render('g0037.hbs', '{"a":{"b":"v"}}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_var_dotted_3;
begin
  CheckEq('deep', Render('g0038.hbs', '{"a":{"b":{"c":"deep"}}}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_var_dotted_miss;
begin
  CheckEq('><', Render('g0039.hbs', '{"a":{"b":{"c":"deep"}}}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_double_amp;
begin
  CheckEq('a&#38;b', Render('g0042.hbs', '{"h":"a&b"}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_triple_amp;
begin
  CheckEq('a&b', Render('g0043.hbs', '{"h":"a&b"}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_amp_amp;
begin
  CheckEq('a&b', Render('g0044.hbs', '{"h":"a&b"}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_double_lt;
begin
  CheckEq('a&#60;b', Render('g0042.hbs', '{"h":"a<b"}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_triple_lt;
begin
  CheckEq('a<b', Render('g0043.hbs', '{"h":"a<b"}'));
end;

{************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_amp_lt;
begin
  CheckEq('a<b', Render('g0044.hbs', '{"h":"a<b"}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_double_gt;
begin
  CheckEq('a&#62;b', Render('g0042.hbs', '{"h":"a>b"}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_triple_gt;
begin
  CheckEq('a>b', Render('g0043.hbs', '{"h":"a>b"}'));
end;

{************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_amp_gt;
begin
  CheckEq('a>b', Render('g0044.hbs', '{"h":"a>b"}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_double_quot;
begin
  CheckEq('a&#34;b', Render('g0042.hbs', '{"h":"a\"b"}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_triple_quot;
begin
  CheckEq('a"b', Render('g0043.hbs', '{"h":"a\"b"}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_amp_quot;
begin
  CheckEq('a"b', Render('g0044.hbs', '{"h":"a\"b"}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_double_tag;
begin
  CheckEq('&#60;b&#62;x&#60;/b&#62;', Render('g0042.hbs', '{"h":"<b>x</b>"}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_triple_tag;
begin
  CheckEq('<b>x</b>', Render('g0043.hbs', '{"h":"<b>x</b>"}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_amp_tag;
begin
  CheckEq('<b>x</b>', Render('g0044.hbs', '{"h":"<b>x</b>"}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_double_mix;
begin
  CheckEq('&#60;a href=&#34;?x=1&#38;y=2&#34;&#62;', Render('g0042.hbs', '{"h":"<a href=\"?x=1&y=2\">"}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_triple_mix;
begin
  CheckEq('<a href="?x=1&y=2">', Render('g0043.hbs', '{"h":"<a href=\"?x=1&y=2\">"}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_amp_mix;
begin
  CheckEq('<a href="?x=1&y=2">', Render('g0044.hbs', '{"h":"<a href=\"?x=1&y=2\">"}'));
end;

{********************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_triple_in_text;
begin
  CheckEq('x=<i>!', Render('g0045.hbs', '{"h":"<i>"}'));
end;

{********************************************************}
procedure TALDUnitXTestHandlebars.Test_esc_double_in_text;
begin
  CheckEq('x=&#60;i&#62;!', Render('g0046.hbs', '{"h":"<i>"}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_parent_each;
begin
  CheckEq('T:a T:b ', Render('g0047.hbs', '{"title":"T","items":["a","b"]}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_parent_with;
begin
  CheckEq('bob@S', Render('g0048.hbs', '{"site":"S","user":{"name":"bob"}}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_parent_double;
begin
  CheckEq('#a #b ', Render('g0049.hbs', '{"tag":"#","outer":[{"inner":["a"]},{"inner":["b"]}]}'));
end;

{***********************************************}
procedure TALDUnitXTestHandlebars.Test_root_each;
begin
  CheckEq('R-x R-y ', Render('g0050.hbs', '{"title":"R","items":["x","y"]}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_root_nested;
begin
  CheckEq('ROOT', Render('g0051.hbs', '{"top":"ROOT","a":{"b":{}}}'));
end;

{************************************************}
procedure TALDUnitXTestHandlebars.Test_root_field;
begin
  CheckEq('L1 L2 ', Render('g0052.hbs', '{"cfg":{"label":"L"},"items":["1","2"]}'));
end;

{************************************************}
procedure TALDUnitXTestHandlebars.Test_each_index;
begin
  CheckEq('0:a 1:b 2:c ', Render('g0053.hbs', '{"items":["a","b","c"]}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_each_index_one;
begin
  CheckEq('[0]', Render('g0054.hbs', '{"items":["x"]}'));
end;

{************************************************}
procedure TALDUnitXTestHandlebars.Test_each_first;
begin
  CheckEq('*a b c ', Render('g0055.hbs', '{"items":["a","b","c"]}'));
end;

{***********************************************}
procedure TALDUnitXTestHandlebars.Test_each_last;
begin
  CheckEq('a b c. ', Render('g0056.hbs', '{"items":["a","b","c"]}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_each_firstlast;
begin
  CheckEq('[abc]', Render('g0057.hbs', '{"items":["a","b","c"]}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_each_key_obj;
begin
  CheckEq('a=1 b=2 ', Render('g0058.hbs', '{"o":{"a":"1","b":"2"}}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_each_index_obj;
begin
  CheckEq('01', Render('g0059.hbs', '{"o":{"a":"1","b":"2"}}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_each_this_index;
begin
  CheckEq('0123', Render('g0060.hbs', '{"items":["q","w","e","r"]}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_each_nested_idx;
begin
  CheckEq('01|012|', Render('g0061.hbs', '{"rows":[["a","b"],["c","d","e"]]}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_lit_if_true;
begin
  CheckEq('Y', Render('g0062.hbs', '{}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_lit_if_false;
begin
  CheckEq('N', Render('g0063.hbs', '{}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_lit_if_zero;
begin
  CheckEq('N', Render('g0064.hbs', '{}'));
end;

{************************************************}
procedure TALDUnitXTestHandlebars.Test_lit_if_one;
begin
  CheckEq('Y', Render('g0065.hbs', '{}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_lit_unless_true;
begin
  CheckEq('N', Render('g0066.hbs', '{}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_lit_unless_false;
begin
  CheckEq('Y', Render('g0067.hbs', '{}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_lit_concat_num;
begin
  CheckEq('n=5', Render('g0068.hbs', '{}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_lit_concat_float;
begin
  CheckEq('f=2.5', Render('g0069.hbs', '{}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_lit_upper_lit;
begin
  CheckEq('ABC', Render('g0070.hbs', '{}'));
end;

{**********************************************}
procedure TALDUnitXTestHandlebars.Test_lit_wrap;
begin
  CheckEq('[x]', Render('g0071.hbs', '{"name":"x"}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_lookup_arr_0;
begin
  CheckEq('a', Render('g0072.hbs', '{"arr":["a","b","c"]}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_lookup_arr_2;
begin
  CheckEq('c', Render('g0073.hbs', '{"arr":["a","b","c"]}'));
end;

{************************************************}
procedure TALDUnitXTestHandlebars.Test_lookup_obj;
begin
  CheckEq('v', Render('g0074.hbs', '{"o":{"k":"v"}}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_lookup_index;
begin
  CheckEq('z x y ', Render('g0075.hbs', '{"vals":["x","y","z"],"idx":[2,0,1]}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_lookup_atindex;
begin
  CheckEq('A B ', Render('g0076.hbs', '{"labels":["A","B"],"items":["a","b"]}'));
end;

{***********************************************}
procedure TALDUnitXTestHandlebars.Test_cmt_short;
begin
  CheckEq('ab', Render('g0077.hbs', '{}'));
end;

{**********************************************}
procedure TALDUnitXTestHandlebars.Test_cmt_long;
begin
  CheckEq('ab', Render('g0078.hbs', '{}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_cmt_with_braces;
begin
  CheckEq('ab', Render('g0079.hbs', '{}'));
end;

{**********************************************}
procedure TALDUnitXTestHandlebars.Test_cmt_only;
begin
  CheckEq('', Render('g0081.hbs', '{}'));
end;

{******************************************************}
procedure TALDUnitXTestHandlebars.Test_cmt_between_vars;
begin
  CheckEq('12', Render('g0082.hbs', '{"a":"1","b":"2"}'));
end;

{*********************************************}
procedure TALDUnitXTestHandlebars.Test_ws_both;
begin
  CheckEq('a-b', Render('g0083.hbs', '{"x":"-"}'));
end;

{*********************************************}
procedure TALDUnitXTestHandlebars.Test_ws_left;
begin
  CheckEq('a-b', Render('g0084.hbs', '{"x":"-"}'));
end;

{**********************************************}
procedure TALDUnitXTestHandlebars.Test_ws_right;
begin
  CheckEq('a-b', Render('g0085.hbs', '{"x":"-"}'));
end;

{*********************************************}
procedure TALDUnitXTestHandlebars.Test_ws_keep;
begin
  CheckEq('a - b', Render('g0087.hbs', '{"x":"-"}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_ws_newlines;
begin
  CheckEq('a-b', Render('g0088.hbs', '{"x":"-"}'));
end;

{*******************************************}
procedure TALDUnitXTestHandlebars.Test_sub_1;
begin
  CheckEq('AB', Render('g0089.hbs', '{}'));
end;

{*******************************************}
procedure TALDUnitXTestHandlebars.Test_sub_2;
begin
  CheckEq('ABC', Render('g0090.hbs', '{}'));
end;

{*******************************************}
procedure TALDUnitXTestHandlebars.Test_sub_3;
begin
  CheckEq('1234', Render('g0091.hbs', '{}'));
end;

{*******************************************}
procedure TALDUnitXTestHandlebars.Test_sub_4;
begin
  CheckEq('mixed', Render('g0092.hbs', '{}'));
end;

{***********************************************}
procedure TALDUnitXTestHandlebars.Test_sub_multi;
begin
  CheckEq('AB-cd', Render('g0093.hbs', '{}'));
end;

{**********************************************}
procedure TALDUnitXTestHandlebars.Test_sub_vars;
begin
  CheckEq('HI BOB', Render('g0094.hbs', '{"g":"hi","n":"bob"}'));
end;

{*********************************************}
procedure TALDUnitXTestHandlebars.Test_sub_two;
begin
  CheckEq('12345', Render('g0095.hbs', '{"a":"1","b":"2","c":"3","d":"4","e":"5"}'));
end;

{***********************************************}
procedure TALDUnitXTestHandlebars.Test_sub_5deep;
begin
  CheckEq('123456', Render('g0097.hbs', '{}'));
end;

{***********************************************}
procedure TALDUnitXTestHandlebars.Test_sub_eqsub;
begin
  CheckEq('same', Render('g0098.hbs', '{"a":"hi","b":"HI"}'));
end;

{**********************************************}
procedure TALDUnitXTestHandlebars.Test_blk_if_t;
begin
  CheckEq('Y', Render('g0099.hbs', '{"a":true}'));
end;

{**********************************************}
procedure TALDUnitXTestHandlebars.Test_blk_if_f;
begin
  CheckEq('N', Render('g0099.hbs', '{"a":false}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_if_noelse_t;
begin
  CheckEq('[Y]', Render('g0100.hbs', '{"a":true}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_if_noelse_f;
begin
  CheckEq('[]', Render('g0100.hbs', '{"a":false}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_if_missing;
begin
  CheckEq('N', Render('g0099.hbs', '{}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_if_emptystr;
begin
  CheckEq('N', Render('g0099.hbs', '{"a":""}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_if_zero;
begin
  CheckEq('N', Render('g0099.hbs', '{"a":0}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_if_emptyarr;
begin
  CheckEq('N', Render('g0099.hbs', '{"a":[]}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_unless_t;
begin
  CheckEq('N', Render('g0101.hbs', '{"a":false}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_unless_f;
begin
  CheckEq('', Render('g0101.hbs', '{"a":true}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_unless_else;
begin
  CheckEq('Y', Render('g0102.hbs', '{"a":true}'));
end;

{**********************************************}
procedure TALDUnitXTestHandlebars.Test_blk_with;
begin
  CheckEq('J-D', Render('g0103.hbs', '{"u":{"first":"J","last":"D"}}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_with_one;
begin
  CheckEq('Jo', Render('g0104.hbs', '{"u":{"name":"Jo"}}'));
end;

{**********************************************}
procedure TALDUnitXTestHandlebars.Test_blk_each;
begin
  CheckEq('[a][b][c]', Render('g0105.hbs', '{"items":["a","b","c"]}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_each_empty;
begin
  CheckEq('><', Render('g0106.hbs', '{"items":[]}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_each_else;
begin
  CheckEq('EMPTY', Render('g0107.hbs', '{"items":[]}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_nested_ie;
begin
  CheckEq('<x><y>', Render('g0108.hbs', '{"a":true,"items":["x","y"]}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_nested_else;
begin
  CheckEq('none', Render('g0108.hbs', '{"a":false,"items":["x","y"]}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_each_of_obj;
begin
  CheckEq('1;2;3;', Render('g0109.hbs', '{"o":{"a":"1","b":"2","c":"3"}}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_helper_in;
begin
  CheckEq('AB', Render('g0110.hbs', '{"a":true,"s":"ab"}'));
end;

{*****************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_each_helper;
begin
  CheckEq('AB', Render('g0111.hbs', '{"items":["a","b"]}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_blk_with_each;
begin
  CheckEq('x.y.', Render('g0112.hbs', '{"u":{"tags":["x","y"]}}'));
end;

{***********************************************}
procedure TALDUnitXTestHandlebars.Test_prt_plain;
begin
  CheckEq('<PARTIAL>', Render('g0113.hbs', '{}'));
end;

{*********************************************}
procedure TALDUnitXTestHandlebars.Test_prt_var;
begin
  CheckEq('<[World]>', Render('g0114.hbs', '{"name":"World"}'));
end;

{************************************************}
procedure TALDUnitXTestHandlebars.Test_prt_helper;
begin
  CheckEq('<WORLD>', Render('g0115.hbs', '{"name":"world"}'));
end;

{***********************************************}
procedure TALDUnitXTestHandlebars.Test_prt_twice;
begin
  CheckEq('PARTIAL/PARTIAL', Render('g0116.hbs', '{}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_prt_in_each;
begin
  CheckEq('[a][b]', Render('g0117.hbs', '{"items":[{"name":"a"},{"name":"b"}]}'));
end;

// Dynamic partials: the partial name is computed by a subexpression.
// {{> (lookup . "partial")}} resolves the partial whose name is the value of
// the "partial" field on the current context.
procedure TALDUnitXTestHandlebars.Test_prt_dyn_plain;
begin
  CheckEq('<PARTIAL>', Render('g0129.hbs', '{"partial":"p_plain.hbs"}'));
end;

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_prt_dyn_helper;
begin
  CheckEq('<WORLD>', Render('g0129.hbs', '{"partial":"p_helper.hbs","name":"world"}'));
end;

{*************************************************}
procedure TALDUnitXTestHandlebars.Test_prt_dyn_var;
begin
  CheckEq('[World]', Render('g0130.hbs', '{"tpl":"p_var.hbs","name":"World"}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_prt_dyn_each;
begin
  CheckEq('PARTIAL[x]', Render('g0131.hbs', '{"items":[{"view":"p_plain.hbs"},{"view":"p_var.hbs","name":"x"}]}'));
end;

// Partial-call forms. The shared partial pcard.hbs is:
//   {{title}}|{{name}}|<{{email}}>|{{#if showEmail}}on{{else}}off{{/if}}
// Literal '<' and '>' around {{email}} stay verbatim (static text is not
// escaped) while the {{name}}/{{email}} values are HTML-escaped per spec.

// {{> partial}} - no argument, partial uses the current context.
procedure TALDUnitXTestHandlebars.Test_prt_arg_none;
begin
  CheckEq(
    'User|Bob &#38; Sons|<a&#60;b&#62;@x>|on',
    Render('hpart_plain.hbs', '{"title":"User","name":"Bob & Sons","email":"a<b>@x","showEmail":true}'));
end;

// {{> partial context}} - partial is rendered with "user" as its context.
procedure TALDUnitXTestHandlebars.Test_prt_arg_context;
begin
  CheckEq(
    'Member|A&#38;B|<x&#39;y>|off',
    Render('hpart_ctx.hbs', '{"user":{"title":"Member","name":"A&B","email":"x''y","showEmail":false}}'));
end;

// {{> partial context showEmail=true title="Administrator"}} - context plus
// hash arguments; the hash overrides the matching fields of the context.
procedure TALDUnitXTestHandlebars.Test_prt_arg_context_hash;
begin
  CheckEq(
    'Administrator|A&#38;B|<x&#39;y>|on',
    Render('hpart_ctx_hash.hbs', '{"user":{"title":"Member","name":"A&B","email":"x''y","showEmail":false}}'));
end;

// {{> partial showEmail=true title="Administrator"}} - hash arguments only;
// they extend/override the current context.
procedure TALDUnitXTestHandlebars.Test_prt_arg_hash;
begin
  CheckEq(
    'Administrator|Bob &#38; Sons|<a&#60;b&#62;@x>|on',
    Render('hpart_hash.hbs', '{"title":"User","name":"Bob & Sons","email":"a<b>@x","showEmail":false}'));
end;

// {{> (lookup . "partial")}} - dynamic partial whose name comes from context.
procedure TALDUnitXTestHandlebars.Test_prt_arg_dynamic;
begin
  CheckEq(
    'User|Bob &#38; Sons|<a&#60;b&#62;@x>|on',
    Render('hpart_dyn.hbs', '{"partial":"pcard.hbs","title":"User","name":"Bob & Sons","email":"a<b>@x","showEmail":true}'));
end;

// Escaping inside a partial. praw.hbs is:
//   lit:<a> & ' " >|esc:{{x}}|raw:{{{x}}}
// Literal special chars pass through; {{x}} is escaped; {{{x}}} is raw.
procedure TALDUnitXTestHandlebars.Test_prt_escape_chars;
begin
  CheckEq(
    'lit:<a> & '' " >|esc:&#60;&#38;&#62;|raw:<&>',
    Render('hpart_raw.hbs', '{"x":"<&>"}'));
end;

{**********************************************}
procedure TALDUnitXTestHandlebars.Test_q_spaces;
begin
  CheckEq('a bc', Render('g0118.hbs', '{}'));
end;

{**********************************************}
procedure TALDUnitXTestHandlebars.Test_q_single;
begin
  CheckEq('ab', Render('g0119.hbs', '{}'));
end;

{*********************************************}
procedure TALDUnitXTestHandlebars.Test_q_empty;
begin
  CheckEq('z', Render('g0120.hbs', '{"name":"z"}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_q_parens_lit;
begin
  CheckEq('(NOT A SUBEXPR)', Render('g0121.hbs', '{}'));
end;

{**************************************************}
procedure TALDUnitXTestHandlebars.Test_q_braces_lit;
begin
  CheckEq('}}x', Render('g0122.hbs', '{}'));
end;

{***************************************************}
procedure TALDUnitXTestHandlebars.Test_q_sub_literal;
begin
  CheckEq('(CONCAT A B)', Render('g0123.hbs', '{"a":"x","b":"y"}'));
end;

{*******************************************}
procedure TALDUnitXTestHandlebars.Test_q_mix;
begin
  CheckEq('Hello, World!', Render('g0124.hbs', '{"name":"World"}'));
end;

{***********************************************}
procedure TALDUnitXTestHandlebars.Test_kitchen_1;
begin
  CheckEq('Dear BOB, you have 2 item(s): [x][y] -- signed: ALICE', Render('g0125.hbs', '{"name":"bob","count":2,"active":true,"items":["x","y"],"author":"alice"}'));
end;

{***********************************************}
procedure TALDUnitXTestHandlebars.Test_kitchen_2;
begin
  CheckEq('0. A, 1. B!', Render('g0126.hbs', '{"users":[{"name":"a"},{"name":"b"}]}'));
end;

{***********************************************}
procedure TALDUnitXTestHandlebars.Test_kitchen_3;
begin
  CheckEq('[T]0:x;1:y;', Render('g0127.hbs', '{"cfg":{"title":"T"},"rows":["x","y"]}'));
end;

{***********************************************}
procedure TALDUnitXTestHandlebars.Test_kitchen_4;
begin
  CheckEq('HI', Render('g0128.hbs', '{"n":10,"label":"Hi"}'));
end;
{$ENDREGION}

initialization
  TDUnitX.RegisterTestFixture(TALDUnitXTestHandlebars);

end.