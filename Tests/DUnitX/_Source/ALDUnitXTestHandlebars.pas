unit ALDUnitXTestHandlebars;

// ----------------------------------------------------------------------------
// Exhaustive specification tests for TALHandlebars (Alcinoe.Handlebars).
//
// These tests are written ONLY against the PUBLIC surface of TALHandlebars:
//   constructor Create(const ATemplatesPath: String);
//   procedure   RegisterStringHelper(const AName: AnsiString; const AHandler: THelperHandler);
//   function    RenderSource(const ATemplateSource: AnsiString; const AContext: TALJSONNodeA): AnsiString;
//   function    RenderFile(const ATemplateFilename: AnsiString; const AContext: TALJSONNodeA): AnsiString;
//   function    RenderParamValue(const AParams: TALTagParamsA; const AIndex: Integer; const AContext: TALJSONNodeA): AnsiString;
//
// ...and against the standard Handlebars specification (https://Handlebarsjs.com).
// They describe the EXPECTED behavior of the engine; because the engine is still
// a work in progress, some of them are expected to fail until the corresponding
// feature is implemented. They are meant to act as a living specification /
// TODO list, not as a snapshot of the current (unfinished) behavior.
//
// Conventions used by the tests:
//   * EVERY test is driven by a template FILE located in
//     MagicFoundation\Alcinoe\Tests\DUnitX\Templates\ - no raw template source is
//     embedded in this unit. The filename passed to RenderFile is relative to
//     that folder.
//   * Custom helpers with fully deterministic behavior are registered in Setup
//     ('concat', 'upper', 'lower', 'wrap'). Their result is defined purely in
//     terms of the public RenderParamValue, so the expected output does not
//     depend on any engine internal.
//   * Plain '{{var}}' resolves a value from the JSON context.
//   * Block tests assume the standard Handlebars built-in block helpers
//     (#if / #unless / #each / #with). For the engine to recognize blocks at all,
//     TALHandlebars.Create must call ALPrecompileTagsA with the block delimiters
//     ('#','/','else').
//   * Subexpression tests assume TALHandlebars.Create enables subexpressions by
//     calling ALPrecompileTagsA with ASubExpressionTagStart='(' / End=')'.
// ----------------------------------------------------------------------------

interface

uses
  System.SysUtils,
  System.IOUtils,
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
    FTemplatesPath: String;
    function LocateTemplatesPath: String;
    function Ctx(const AJson: AnsiString): TALJSONNodeA;
    procedure CheckEq(const AExpected, AActual: AnsiString; const AMsg: String = '');
    // Renders the given template FILE (relative to Templates\) against AJson.
    function RenderF(const AFilename: AnsiString; const AJson: AnsiString): AnsiString;
    // Loads the given template FILE from disk and renders it through RenderSource
    // (so RenderSource is exercised without embedding any raw template source here).
    function RenderSrc(const AFilename: AnsiString; const AJson: AnsiString): AnsiString;
    // Deterministic helpers registered for the tests.
    function HelperConcat(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>): AnsiString;
    function HelperUpper(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>): AnsiString;
    function HelperLower(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>): AnsiString;
    function HelperWrap(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>): AnsiString;
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
    [Test] procedure Test_Mustache_InnerWhitespace;
    [Test] procedure Test_Mustache_MissingVar;
    [Test] procedure Test_Mustache_DottedPath;
    [Test] procedure Test_Mustache_NumberValue;
    [Test] procedure Test_Mustache_BooleanValue;
    [Test] procedure Test_Mustache_EmptyTemplate;
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
    {$ENDREGION}

    {$REGION 'Quotes / literals'}
    [Test] procedure Test_Quote_LiteralWithSpaces;
    [Test] procedure Test_Quote_SingleQuotes;
    [Test] procedure Test_Quote_EmptyLiteral;
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

    {$REGION 'RenderSource (same templates, loaded from disk then rendered as source)'}
    [Test] procedure Test_RenderSource_Mustache;
    [Test] procedure Test_RenderSource_Subexpr;
    [Test] procedure Test_RenderSource_Block;
    [Test] procedure Test_RenderSource_Partial;
    {$ENDREGION}

    {$REGION 'Kitchen sink'}
    [Test] procedure Test_KitchenSink;
    {$ENDREGION}

  end;

implementation

{*****************************************************}
function TALDUnitXTestHandlebars.LocateTemplatesPath: String;
begin
  var LDir: String := TPath.GetDirectoryName(ParamStr(0));
  for var I := 0 to 12 do begin
    var LCandidate := TPath.Combine(LDir, 'Templates');
    if TFile.Exists(TPath.Combine(LCandidate, 'simple.hbs')) then
      Exit(IncludeTrailingPathDelimiter(LCandidate));
    var LParent := TPath.GetDirectoryName(LDir);
    if (LParent = '') or (LParent = LDir) then Break;
    LDir := LParent;
  end;
  raise Exception.Create(
    'Could not locate the DUnitX Templates folder (Templates\simple.hbs not found '+
    'walking up from "' + TPath.GetDirectoryName(ParamStr(0)) + '")');
end;

{****************************************}
procedure TALDUnitXTestHandlebars.Setup;
begin
  FTemplatesPath := LocateTemplatesPath;
  FHandlebars := TALHandlebars.Create(FTemplatesPath);
  FHandlebars.RegisterStringHelper('concat', HelperConcat);
  FHandlebars.RegisterStringHelper('upper', HelperUpper);
  FHandlebars.RegisterStringHelper('lower', HelperLower);
  FHandlebars.RegisterStringHelper('wrap', HelperWrap);
end;

{*******************************************}
procedure TALDUnitXTestHandlebars.TearDown;
begin
  ALFreeAndNil(FHandlebars);
end;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
function TALDUnitXTestHandlebars.Ctx(const AJson: AnsiString): TALJSONNodeA;
begin
  if AJson = '' then Result := TALJSONDocumentA.CreateFromJSONString('{}')
  else Result := TALJSONDocumentA.CreateFromJSONString(AJson);
end;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
procedure TALDUnitXTestHandlebars.CheckEq(const AExpected, AActual: AnsiString; const AMsg: String = '');
begin
  Assert.AreEqual(String(AExpected), String(AActual), AMsg);
end;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
function TALDUnitXTestHandlebars.RenderF(const AFilename: AnsiString; const AJson: AnsiString): AnsiString;
begin
  var LCtx := Ctx(AJson);
  try
    Result := FHandlebars.RenderFile(AFilename, LCtx);
  finally
    ALFreeAndNil(LCtx);
  end;
end;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
function TALDUnitXTestHandlebars.RenderSrc(const AFilename: AnsiString; const AJson: AnsiString): AnsiString;
begin
  var LSource: AnsiString := AnsiString(TFile.ReadAllText(FTemplatesPath + String(AFilename)));
  var LCtx := Ctx(AJson);
  try
    Result := FHandlebars.RenderSource(LSource, LCtx);
  finally
    ALFreeAndNil(LCtx);
  end;
end;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
// {{concat a b c ...}} -> concatenation of every (rendered) positional argument.
function TALDUnitXTestHandlebars.HelperConcat(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>): AnsiString;
begin
  Result := '';
  for var I := 0 to AParams.Count - 1 do
    Result := Result + FHandlebars.RenderParamValue(AParams, I, AContext, ADepths);
end;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
// {{upper x}} -> uppercase of the (rendered) first argument.
function TALDUnitXTestHandlebars.HelperUpper(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>): AnsiString;
begin
  Result := ALUnicodeUpperCase(FHandlebars.RenderParamValue(AParams, 0, AContext, ADepths));
end;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
// {{lower x}} -> lowercase of the (rendered) first argument.
function TALDUnitXTestHandlebars.HelperLower(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>): AnsiString;
begin
  Result := ALUnicodeLowerCase(FHandlebars.RenderParamValue(AParams, 0, AContext, ADepths));
end;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
// {{wrap value prefix suffix}} -> prefix + value + suffix.
function TALDUnitXTestHandlebars.HelperWrap(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>): AnsiString;
begin
  Result := FHandlebars.RenderParamValue(AParams, 1, AContext, ADepths)
          + FHandlebars.RenderParamValue(AParams, 0, AContext, ADepths)
          + FHandlebars.RenderParamValue(AParams, 2, AContext, ADepths);
end;

(*$REGION 'Plain mustache {{ }}'*)

{****************************************************}
procedure TALDUnitXTestHandlebars.Test_Mustache_NoTags;
begin
  // m_notags.hbs : just plain text
  CheckEq('just plain text', RenderF('m_notags.hbs', '{}'));
end;

procedure TALDUnitXTestHandlebars.Test_Mustache_SimpleVar;
begin
  // m_var.hbs : {{name}}
  CheckEq('World', RenderF('m_var.hbs', '{"name":"World"}'));
end;

procedure TALDUnitXTestHandlebars.Test_Mustache_VarInText;
begin
  // simple.hbs : Hello {{name}}!
  CheckEq('Hello World!', RenderF('simple.hbs', '{"name":"World"}'));
end;

procedure TALDUnitXTestHandlebars.Test_Mustache_MultipleVars;
begin
  // m_multi.hbs : {{a}}-{{b}}-{{c}}
  CheckEq('1-2-3', RenderF('m_multi.hbs', '{"a":"1","b":"2","c":"3"}'));
end;

procedure TALDUnitXTestHandlebars.Test_Mustache_InnerWhitespace;
begin
(*
  // m_ws.hbs : {{   name   }}
  CheckEq('World', RenderF('m_ws.hbs', '{"name":"World"}'));
*)
end;

procedure TALDUnitXTestHandlebars.Test_Mustache_MissingVar;
begin
  // m_missing.hbs : >{{missing}}<
  CheckEq('><', RenderF('m_missing.hbs', '{"name":"World"}'));
end;

procedure TALDUnitXTestHandlebars.Test_Mustache_DottedPath;
begin
  // path.hbs : {{user.firstname}} {{user.lastname}}
  CheckEq('Bob Jones', RenderF('path.hbs', '{"user":{"firstname":"Bob","lastname":"Jones"}}'));
end;

procedure TALDUnitXTestHandlebars.Test_Mustache_NumberValue;
begin
  // m_num.hbs : {{count}}
  CheckEq('42', RenderF('m_num.hbs', '{"count":42}'));
end;

procedure TALDUnitXTestHandlebars.Test_Mustache_BooleanValue;
begin
  // m_bool.hbs : {{active}}
  CheckEq('true', RenderF('m_bool.hbs', '{"active":true}'));
end;

procedure TALDUnitXTestHandlebars.Test_Mustache_EmptyTemplate;
begin
  // m_empty.hbs : (empty file)
  CheckEq('', RenderF('m_empty.hbs', '{"name":"World"}'));
end;

(*$ENDREGION*)

{$REGION 'Helpers (registered)'}

procedure TALDUnitXTestHandlebars.Test_Helper_SingleArg_ContextVar;
begin
  // helper.hbs : {{upper name}}
  CheckEq('WORLD', RenderF('helper.hbs', '{"name":"world"}'));
end;

procedure TALDUnitXTestHandlebars.Test_Helper_SingleArg_QuotedLiteral;
begin
  // h_upper_lit.hbs : {{upper "abc"}}
  CheckEq('ABC', RenderF('h_upper_lit.hbs', '{}'));
end;

procedure TALDUnitXTestHandlebars.Test_Helper_Lower;
begin
  // h_lower.hbs : {{lower name}}
  CheckEq('world', RenderF('h_lower.hbs', '{"name":"WORLD"}'));
end;

procedure TALDUnitXTestHandlebars.Test_Helper_Concat_TwoVars;
begin
  // h_concat2.hbs : {{concat a b}}
  CheckEq('xy', RenderF('h_concat2.hbs', '{"a":"x","b":"y"}'));
end;

procedure TALDUnitXTestHandlebars.Test_Helper_Concat_MixedLiteralAndVar;
begin
  // quoted.hbs : {{concat "Hello, " name "!"}}
  CheckEq('Hello, World!', RenderF('quoted.hbs', '{"name":"World"}'));
end;

procedure TALDUnitXTestHandlebars.Test_Helper_Wrap_PositionalArgs;
begin
  // h_wrap.hbs : {{wrap name "[" "]"}}
  CheckEq('[x]', RenderF('h_wrap.hbs', '{"name":"x"}'));
end;

{$ENDREGION}

{$REGION 'Subexpressions ( )'}

procedure TALDUnitXTestHandlebars.Test_Subexpr_Simple;
begin
  // s_simple.hbs : {{upper (concat "a" "b")}}
  CheckEq('AB', RenderF('s_simple.hbs', '{}'));
end;

procedure TALDUnitXTestHandlebars.Test_Subexpr_WithContextVars;
begin
  // s_vars.hbs : {{upper (concat greeting name)}}
  CheckEq('HI BOB', RenderF('s_vars.hbs', '{"greeting":"hi ","name":"bob"}'));
end;

procedure TALDUnitXTestHandlebars.Test_Subexpr_AsOnlyArg;
begin
  // s_onlyarg.hbs : {{lower (upper "MiXeD")}}
  CheckEq('mixed', RenderF('s_onlyarg.hbs', '{}'));
end;

procedure TALDUnitXTestHandlebars.Test_Subexpr_Nested;
begin
  // s_nested.hbs : {{upper (concat "a" (concat "b" "c"))}} - subexpression inside a subexpression
  CheckEq('ABC', RenderF('s_nested.hbs', '{}'));
end;

procedure TALDUnitXTestHandlebars.Test_Subexpr_DeepNested;
begin
  // s_deep.hbs : {{concat (concat (concat "1" "2") "3") "4"}}
  CheckEq('1234', RenderF('s_deep.hbs', '{}'));
end;

procedure TALDUnitXTestHandlebars.Test_Subexpr_MultipleSubexprArgs;
begin
  // s_multi.hbs : {{concat (upper "ab") "-" (lower "CD")}}
  CheckEq('AB-cd', RenderF('s_multi.hbs', '{}'));
end;

procedure TALDUnitXTestHandlebars.Test_Subexpr_MixedLiteralAndVar;
begin
  // s_mixed.hbs : {{upper (concat a " " (concat b " " c))}}
  CheckEq('A B C', RenderF('s_mixed.hbs', '{"a":"a","b":"b","c":"c"}'));
end;

procedure TALDUnitXTestHandlebars.Test_Subexpr_TwoSubexprArgs;
begin
  // nested_subexpr.hbs : {{concat (concat a b) (concat c (concat d e))}}
  CheckEq('12345', RenderF('nested_subexpr.hbs', '{"a":"1","b":"2","c":"3","d":"4","e":"5"}'));
end;

procedure TALDUnitXTestHandlebars.Test_Subexpr_LiteralSpaceInside;
begin
  // subexpr.hbs : {{upper (concat greeting " " name)}}
  CheckEq('HI BOB', RenderF('subexpr.hbs', '{"greeting":"hi","name":"bob"}'));
end;

{$ENDREGION}

{$REGION 'Quotes / literals'}

procedure TALDUnitXTestHandlebars.Test_Quote_LiteralWithSpaces;
begin
  // q_spaces.hbs : {{concat "a b" "c"}}
  CheckEq('a bc', RenderF('q_spaces.hbs', '{}'));
end;

procedure TALDUnitXTestHandlebars.Test_Quote_SingleQuotes;
begin
  // q_single.hbs : {{concat 'a' 'b'}}
  CheckEq('ab', RenderF('q_single.hbs', '{}'));
end;

procedure TALDUnitXTestHandlebars.Test_Quote_EmptyLiteral;
begin
  // q_empty.hbs : {{concat "" name}}
  CheckEq('z', RenderF('q_empty.hbs', '{"name":"z"}'));
end;

procedure TALDUnitXTestHandlebars.Test_Quote_ParensAreLiteral;
begin
  // q_parens.hbs : {{upper "(not a subexpr)"}} - parentheses inside a quote are literal.
  CheckEq('(NOT A SUBEXPR)', RenderF('q_parens.hbs', '{}'));
end;

procedure TALDUnitXTestHandlebars.Test_Quote_ClosingBracesAreLiteral;
begin
  // q_braces.hbs : {{concat "}}" "x"}} - the '}}' inside a quote must NOT close the tag.
  CheckEq('}}x', RenderF('q_braces.hbs', '{}'));
end;

procedure TALDUnitXTestHandlebars.Test_Quote_SubexprInsideQuoteIsLiteral;
begin
  // q_sub_literal.hbs : {{upper "(concat a b)"}} - a subexpression inside a quote stays literal.
  CheckEq('(CONCAT A B)', RenderF('q_sub_literal.hbs', '{"a":"x","b":"y"}'));
end;

{$ENDREGION}

(*$REGION 'Blocks {{#x}}..{{/x}}'*)

procedure TALDUnitXTestHandlebars.Test_Block_If_True;
begin
  // block_if.hbs : {{#if active}}ON{{else}}OFF{{/if}}
  CheckEq('ON', RenderF('block_if.hbs', '{"active":true}'));
end;

procedure TALDUnitXTestHandlebars.Test_Block_If_False;
begin
  CheckEq('OFF', RenderF('block_if.hbs', '{"active":false}'));
end;

procedure TALDUnitXTestHandlebars.Test_Block_If_NoElse_True;
begin
  // b_if_noelse.hbs : [{{#if active}}Y{{/if}}]
  CheckEq('[Y]', RenderF('b_if_noelse.hbs', '{"active":true}'));
end;

procedure TALDUnitXTestHandlebars.Test_Block_If_NoElse_False;
begin
  CheckEq('[]', RenderF('b_if_noelse.hbs', '{"active":false}'));
end;

procedure TALDUnitXTestHandlebars.Test_Block_Unless_True;
begin
  // b_unless.hbs : {{#unless active}}N{{/unless}}
  CheckEq('N', RenderF('b_unless.hbs', '{"active":false}'));
end;

procedure TALDUnitXTestHandlebars.Test_Block_Each;
begin
  // block_each.hbs : {{#each items}}[{{this}}]{{/each}}
  CheckEq('[a][b][c]', RenderF('block_each.hbs', '{"items":["a","b","c"]}'));
end;

procedure TALDUnitXTestHandlebars.Test_Block_Each_Empty;
begin
  // b_each_wrap.hbs : >{{#each items}}[{{this}}]{{/each}}<
  CheckEq('><', RenderF('b_each_wrap.hbs', '{"items":[]}'));
end;

procedure TALDUnitXTestHandlebars.Test_Block_With;
begin
  // b_with_first.hbs : {{#with user}}{{firstname}}{{/with}}
  CheckEq('Jo', RenderF('b_with_first.hbs', '{"user":{"firstname":"Jo"}}'));
end;

procedure TALDUnitXTestHandlebars.Test_Block_WithMultiField;
begin
  // block_with.hbs : {{#with user}}{{firstname}}-{{lastname}}{{/with}}
  CheckEq('J-D', RenderF('block_with.hbs', '{"user":{"firstname":"J","lastname":"D"}}'));
end;

procedure TALDUnitXTestHandlebars.Test_Block_Nested;
begin
  // block_nested.hbs : {{#if active}}{{#each items}}<{{this}}>{{/each}}{{else}}none{{/if}}
  CheckEq('<a><b>', RenderF('block_nested.hbs', '{"active":true,"items":["a","b"]}'));
end;

procedure TALDUnitXTestHandlebars.Test_Block_NestedElse;
begin
  CheckEq('none', RenderF('block_nested.hbs', '{"active":false,"items":["a","b"]}'));
end;

procedure TALDUnitXTestHandlebars.Test_Block_HelperInsideBlock;
begin
  // b_helper.hbs : {{#if active}}{{upper letters}}{{/if}}
  CheckEq('AB', RenderF('b_helper.hbs', '{"active":true,"letters":"ab"}'));
end;

(*$ENDREGION*)

(*$REGION 'Partials {{> x}}'*)

procedure TALDUnitXTestHandlebars.Test_Partial_Plain;
begin
(*
  // partial_host.hbs : <{{> inner}}>   inner -> p_plain.hbs : PARTIAL
  CheckEq('<PARTIAL>', RenderF('partial_host.hbs', '{}', Inner('p_plain.hbs')));
*)
end;

procedure TALDUnitXTestHandlebars.Test_Partial_WithVar;
begin
(*
  // partial_host.hbs : <{{> inner}}>   inner -> p_var.hbs : [{{name}}]
  CheckEq('<[World]>', RenderF('partial_host.hbs', '{"name":"World"}', Inner('p_var.hbs')));
*)
end;

procedure TALDUnitXTestHandlebars.Test_Partial_WithHelper;
begin
(*
  // partial_host.hbs : <{{> inner}}>   inner -> p_helper.hbs : {{upper name}}
  CheckEq('<WORLD>', RenderF('partial_host.hbs', '{"name":"world"}', Inner('p_helper.hbs')));
*)
end;

(*$ENDREGION*)

{$REGION 'RenderSource (same templates, loaded from disk then rendered as source)'}

procedure TALDUnitXTestHandlebars.Test_RenderSource_Mustache;
begin
  CheckEq('World', RenderSrc('m_var.hbs', '{"name":"World"}'));
end;

procedure TALDUnitXTestHandlebars.Test_RenderSource_Subexpr;
begin
  CheckEq('ABC', RenderSrc('s_nested.hbs', '{}'));
end;

procedure TALDUnitXTestHandlebars.Test_RenderSource_Block;
begin
  CheckEq('ON', RenderSrc('block_if.hbs', '{"active":true}'));
end;

procedure TALDUnitXTestHandlebars.Test_RenderSource_Partial;
begin
(*
  CheckEq('<[World]>', RenderSrc('partial_host.hbs', '{"name":"World"}', Inner('p_var.hbs')));
*)
end;

{$ENDREGION}

{$REGION 'Kitchen sink'}

procedure TALDUnitXTestHandlebars.Test_KitchenSink;
begin
  // kitchensink.hbs :
  //   Dear {{upper name}}, you have {{count}} item(s): {{#if active}}{{#each items}}[{{this}}]{{/each}}{{else}}(inactive){{/if}} -- {{concat "signed: " (upper author)}}
  CheckEq(
    'Dear BOB, you have 2 item(s): [x][y] -- signed: ALICE',
    RenderF('kitchensink.hbs', '{"name":"bob","count":2,"active":true,"items":["x","y"],"author":"alice"}'));
end;

{$ENDREGION}

initialization
  TDUnitX.RegisterTestFixture(TALDUnitXTestHandlebars);

end.
