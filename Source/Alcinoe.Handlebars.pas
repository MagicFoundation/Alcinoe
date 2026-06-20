unit Alcinoe.Handlebars;

interface

{$I Alcinoe.inc}

uses
  System.Generics.Collections,
  System.Contnrs,
  System.Types,
  Alcinoe.StringUtils,
  Alcinoe.JSONDoc;

type

  {*******************}
  TALHandlebars = class
  public
    const
      MainBlockParamName = '__main';
      InverseBlockParamName = '__inverse';
      UnescapedParamName    = '__unescaped';
  public
    type
      // --------------
      // THelperHandler
      THelperHandler = function(
        const AParams: TALTagParamsA;
        const AContext: TALJsonNodeA;
        const ADepths: TList<TALJsonNodeA>;
        var AOwned: Boolean): TALJsonNodeA of object;
  private
    type
      // -------------------------
      // TTagReplaceHandlerContext
      TTagReplaceHandlerContext = record
        Json: TALJsonNodeA;
        Depths: TList<TALJsonNodeA>;
      end;
  private
    FTemplates: TDictionary<AnsiString{name}, AnsiString{content}>;
    FHelpers: TDictionary<AnsiString{name}, THelperHandler>;
    FTagsContainer: TObjectList;
    function CompareJsonNode(const AJsonNode1, AJsonNode2: TALJsonNodeA): TValueRelationship;
    function TagReplaceHandler(
               const ATagName: AnsiString;
               const ATagParams: TALTagParamsA;
               const AContext: pointer): AnsiString;
  protected
    /// <summary>
    ///   Resolves a tag as a registered helper or as a value from the current JSON context.
    ///   AOwned is set to True when the returned node was created by this method and must
    ///   be freed by the caller; otherwise, the returned node is borrowed and must not be freed.
    /// </summary>
    function ResolveTag(
               const ATagName: AnsiString;
               const ATagParams: TALTagParamsA;
               const AContext: TALJsonNodeA;
               const ADepths: TList<TALJsonNodeA>;
               out AOwned: Boolean): TALJsonNodeA; virtual;
    function ResolvePath(
               const APath: AnsiString;
               const AContext: TALJsonNodeA;
               const ADepths: TList<TALJsonNodeA>): TALJsonNodeA; virtual;
    function ResolveTemplate(
               const ATemplateSource: AnsiString;
               const AContext: TTagReplaceHandlerContext): AnsiString; overload; virtual;
    function ResolveTemplate(
               const ATemplateSource: AnsiString;
               const AContext: TALJsonNodeA): AnsiString; overload; virtual;
  protected

    {$REGION 'Default Helpers'}
    function partial(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function &if(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function unless(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function each(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function index(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function key(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function first(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function last(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function &with(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function lookup(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function concat(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function eq(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function ne(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function gt(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function lt(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function gte(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function lte(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function &and(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function &or(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function &not(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    {$ENDREGION}

  public
    constructor Create(const ATemplatesPath: String); virtual;
    destructor Destroy; override;
    procedure RegisterHelper(
                const AName: AnsiString;
                const AHandler: THelperHandler); virtual;
    function Render(
               const ATemplateName: AnsiString;
               const AContext: TALJsonNodeA): AnsiString; virtual;
    /// <summary>
    ///   Evaluates the truthiness of a JSON node. Nil, null, empty values, and numeric
    ///   zero are considered false. When AIncludeZero is True, numeric zero is
    ///   considered true.
    /// </summary>
    function IsTruthy(
               const AJsonNode: TALJsonNodeA;
               const AIncludeZero: Boolean = False): Boolean;
    /// <summary>
    ///   Resolves a helper parameter as a literal value, rendered block, precompiled
    ///   subexpression, or node from the current JSON context. AOwned is set to True
    ///   when a new node is returned and must be freed by the caller. Borrowed context
    ///   nodes and unresolved parameters set AOwned to False.
    /// </summary>
    function ResolveParamValue(
               const AParams: TALTagParamsA;
               const AIndex: Integer;
               const AContext: TALJsonNodeA;
               const ADepths: TList<TALJsonNodeA>;
               out AOwned: Boolean): TALJsonNodeA; virtual;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils,
  System.AnsiStrings,
  System.Math,
  System.IOUtils,
  Alcinoe.Common,
  Alcinoe.Localization,
  Alcinoe.XMLDoc;

{*************************************************************}
constructor TALHandlebars.Create(const ATemplatesPath: String);
begin

  inherited create;

  FTemplates := TDictionary<AnsiString{name}, AnsiString{content}>.Create;
  FHelpers := TDictionary<AnsiString{name}, THelperHandler>.Create;
  FTagsContainer := TObjectList.Create(true{AOwnsObjects});

  var LTemplatesPath := ALIncludeTrailingPathDelimiterW(ATemplatesPath);
  Var LTemplateFilePaths := TDirectory.GetFiles(LTemplatesPath, '*', TSearchOption.soAllDirectories);
  for var LTemplateFilePath in LTemplateFilePaths do begin
    var LTemplateFileName := AlStringReplaceW(LTemplateFilePath, LTemplatesPath, '');
    var LTemplateSrc := ALGetStringFromFile(LTemplateFilePath);
    // Remove whitespace before {{~ and replace {{~ with {{
    var P := ALPosA('{{~', LTemplateSrc);
    while P > 0 do begin
      var LStart := P;
      while (LStart > 1) and (LTemplateSrc[LStart - 1] <= ' ') do
        Dec(LStart);
      Delete(LTemplateSrc, LStart, P - LStart);
      Delete(LTemplateSrc, LStart + 2, 1);
      P := ALPosA('{{~', LTemplateSrc, LStart + 2);
    end;
    // Replace ~}} with }} and remove whitespace after it
    P := ALPosA('~}}', LTemplateSrc);
    while P > 0 do begin
      Delete(LTemplateSrc, P, 1);
      var LEnd := P + 2;
      while (LEnd <= Length(LTemplateSrc)) and (LTemplateSrc[LEnd] <= ' ') do
        Delete(LTemplateSrc, LEnd, 1);
      P := ALPosA('~}}', LTemplateSrc, P + 2);
    end;
    // Remove whitespace immediately after {{
    P := ALPosA('{{', LTemplateSrc);
    while P > 0 do begin
      var LStart := P + 2;
      var LEnd := LStart;
      while (LEnd <= Length(LTemplateSrc)) and
            (LTemplateSrc[LEnd] <= ' ') do
        Inc(LEnd);
      if LEnd > LStart then
        Delete(LTemplateSrc, LStart, LEnd - LStart);
      P := ALPosA('{{', LTemplateSrc, P + 2);
    end;
    // Remove whitespace immediately before }}
    P := ALPosA('}}', LTemplateSrc);
    while P > 0 do begin
      var LStart := P - 1;
      while (LStart >= 1) and
            (LTemplateSrc[LStart] <= ' ') do
        Dec(LStart);
      Inc(LStart);
      if LStart < P then begin
        Delete(LTemplateSrc, LStart, P - LStart);
        P := LStart;
      end;
      P := ALPosA('}}', LTemplateSrc, P + 2);
    end;
    // Remove {{!-- .... --}} comments
    LTemplateSrc := ALReplaceTagsA(
                      LTemplateSrc, // const ASourceString: AnsiString;
                      '{{!--', // const ATagStartMarker,
                      '--}}', // ATagEndMarker: AnsiString;
                      ''); // const AReplaceWith: AnsiString;
    // Remove {{! .... }} comments
    LTemplateSrc := ALReplaceTagsA(
                      LTemplateSrc, // const ASourceString: AnsiString;
                      '{{!', // const ATagStartMarker,
                      '}}', // ATagEndMarker: AnsiString;
                      ''); // const AReplaceWith: AnsiString;
    // Handle unescaped {{{ .... }}} blocks
    LTemplateSrc := ALStringReplaceA(LTemplateSrc, '{{{', '{{', [RfReplaceAll]);
    LTemplateSrc := ALStringReplaceA(LTemplateSrc, '}}}', ' ' + UnescapedParamName + '=true}}', [RfReplaceAll]);
    // Handle normal {{ .... }} blocks
    LTemplateSrc := ALPrecompileTagsA(
                      LTemplateSrc, // const ASourceString: AnsiString;
                      '{{', // const ATagStartMarker,
                      '}}', // ATagEndMarker: AnsiString;
                      FTagsContainer, // const ATagsContainer: TObjectList;
                      false, // const AIgnoreCase: Boolean = false;
                      false, // const AQuoteDoublingEscape: Boolean = False;
                      '\', // const AEscapeChar: AnsiChar = #0; // ex: '\' like in {{uppercase "abc\"def}}
                      '=', // const ANameValueSeparator: AnsiChar = '='; // ex: '=' like in {{link title url="/home" target="_blank"}}
                      '#', // const ABlockStartMarker: AnsiChar = #0; // ex: '#' like in {{#if ...}}
                      '/', // const ABlockEndMarker: AnsiChar = #0; // ex: '/' like in {{/if}}
                      'else', // const ABlockElseTag: AnsiString = ''; // ex: 'else' like in {{else}}
                      '(', // const ASubExpressionTagStartMarker: AnsiString = ''; // ex: '(' like in {{uppercase (concat a b)}}
                      ')', // const ASubExpressionTagEndMarker: AnsiString = ''; // ex: ')' like in {{uppercase (concat a b)}}
                      False, // const AHandleTagsInQuote: Boolean = True; // False = quoted params are kept literal
                      MainBlockParamName, // const AMainBlockParamName: AnsiString = '__main'; // name of the supplemental block param holding the main section
                      InverseBlockParamName, // const AInverseBlockParamName: AnsiString = '__inverse'; // name of the supplemental block param holding the inverse section
                      ALDefaultPrecompiledTagStartMarker, // const APrecompiledTagStartMarker: AnsiChar = ALDefaultPrecompiledTagStartMarker; // opens the precompiled-tag marker (ex: STX)
                      ALDefaultPrecompiledTagEndMarker); // const APrecompiledTagEndMarker: AnsiChar = ALDefaultPrecompiledTagEndMarker): AnsiString; // closes the precompiled-tag marker (ex: ETX)
    FTemplates.Add(AnsiString(LTemplateFileName){name}, LTemplateSrc{content});
  end;

  RegisterHelper('>', partial);
  RegisterHelper('if', &if);
  RegisterHelper('unless', unless);
  RegisterHelper('each', each);
  RegisterHelper('@index', index);
  RegisterHelper('@key', key);
  RegisterHelper('@first', first);
  RegisterHelper('@last', last);
  RegisterHelper('with', &with);
  RegisterHelper('lookup', lookup);
  RegisterHelper('concat', concat);
  RegisterHelper('eq', eq);
  RegisterHelper('ne', ne);
  RegisterHelper('gt', gt);
  RegisterHelper('lt', lt);
  RegisterHelper('gte', gte);
  RegisterHelper('lte', lte);
  RegisterHelper('and', &and);
  RegisterHelper('or', &or);
  RegisterHelper('not', &not);

end;

{*******************************}
destructor TALHandlebars.Destroy;
begin
  ALFreeAndNil(FTemplates);
  ALFreeAndNil(FHelpers);
  ALFreeAndNil(FTagsContainer);
  inherited;
end;

{****************************************************************************************************************************************************************}
function TALHandlebars.partial(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{> partial}}
  // {{> partial context}}
  // {{> partial context showEmail=true title="Administrator"}}
  // {{> partial showEmail=true title="Administrator"}}
  // {{> (lookup . "partial")}}
  //

  // A partial requires at least its name in the first positional parameter.
  if AParams.Count <= 0 then
    raise Exception.Create('The partial helper requires at least one argument: the partial name');

  // Resolve the partial name.
  var LPartialName := AParams[0];
  if (Length(LPartialName) = 2 + SizeOf(Pointer)){#2<address>#3} and
     (LPartialName[Low(LPartialName)] = ALDefaultPrecompiledTagStartMarker) and
     (LPartialName[High(LPartialName)] = ALDefaultPrecompiledTagEndMarker) then begin
    var LOwned: Boolean;
    var LJsonNode := ResolveParamValue(
                       AParams, // const AParams: TALTagParamsA;
                       0, // const AIndex: Integer;
                       AContext, // const AContext: TALJsonNodeA;
                       ADepths, // const ADepths: TList<TALJsonNodeA>;
                       LOwned); // out AOwned: Boolean): TALJsonNodeA;
    try
      if LJsonNode <> nil then LPartialName := LJsonNode.Text
      else LPartialName := '';
    finally
      if LOwned then ALFreeAndNil(LJsonNode);
    end;
  end;

  // Retrieve the precompiled template registered under this name.
  var LPartialSource: AnsiString;
  if not FTemplates.TryGetValue(LPartialName, LPartialSource) then
    raise Exception.CreateFmt('Partial "%s" not found', [LPartialName]);

  // When no explicit context or hash arguments are provided,
  // render the partial directly against the current context.
  if AParams.Count <= 1 then begin
    var LContext: TTagReplaceHandlerContext;
    LContext.Json := AContext;
    LContext.Depths := ADepths;
    var LText := ResolveTemplate(
                   LPartialSource, // const ATemplateSource: AnsiString;
                   LContext); // const AContext: TTagReplaceHandlerContext;
    AOwned := True;
    Result := TALJSONTextNodeA.Create('');
    Result.Text := LText;
  end

  // When an explicit context or hash arguments are provided,
  // build a temporary context before rendering the partial.
  else begin

    // The first positional parameter after the name (if present) overrides
    // the current context. It is itself evaluated in the caller's context.
    var LBaseContext := AContext;
    var LBaseOwned := False;
    if not AParams.ItemHasNameValue(1) then
      LBaseContext := ResolveParamValue(
                        AParams, // const AParams: TALTagParamsA;
                        1, // const AIndex: Integer;
                        AContext, // const AContext: TALJsonNodeA;
                        ADepths, // const ADepths: TList<TALJsonNodeA>;
                        LBaseOwned); // out AOwned: Boolean): TALJsonNodeA;
    try

      // Clone an object context so that hash arguments can be added
      // without modifying the original node. For any other context type,
      // start with an empty object.
      var LMergedContext: TALJsonNodeA;
      if (LBaseContext <> nil) and (LBaseContext.NodeType = TALJSONNodeType.ntObject) then LMergedContext := LBaseContext.Clone
      else LMergedContext := TALJSONObjectNodeA.Create('');
      try

        // Merge hash (named) parameters into the context.
        for var I := 1 to AParams.Count - 1 do begin
          if not AParams.ItemHasNameValue(I) then Continue;
          var LParamName := AParams.Names[I];
          //--
          LMergedContext.DeleteChild(LParamName);
          //--
          var LHashOwned: Boolean;
          var LHashNode := ResolveParamValue(
                             AParams, // const AParams: TALTagParamsA;
                             I, // const AIndex: Integer;
                             AContext, // const AContext: TALJsonNodeA;
                             ADepths, // const ADepths: TList<TALJsonNodeA>;
                             LHashOwned); // out AOwned: Boolean): TALJsonNodeA;
          try
            if LHashNode <> nil then begin
              if LHashOwned then begin
                LHashNode.NodeName := LParamName;
                LMergedContext.ChildNodes.Add(LHashNode);
                LHashNode := nil;
              end
              else begin
                var LJsonNode := LHashNode.Clone;
                LJsonNode.NodeName := LParamName;
                LMergedContext.ChildNodes.Add(LJsonNode);
              end;
            end;
          finally
            if LHashOwned then
              ALFreeAndNil(LHashNode);
          end;
        end;

        // Render the partial against the resolved context
        var LContext: TTagReplaceHandlerContext;
        LContext.Json := LMergedContext;
        LContext.Depths := ADepths;
        var LText := ResolveTemplate(
                       LPartialSource, // const ATemplateSource: AnsiString;
                       LContext); // const AContext: TTagReplaceHandlerContext;
        AOwned := True;
        Result := TALJSONTextNodeA.Create('');
        Result.Text := LText;

      finally
        if LMergedContext <> nil then
          ALFreeAndNil(LMergedContext);
      end;

    finally
      if LBaseOwned then
        ALFreeAndNil(LBaseContext);
    end;

  end;

end;

{*************************************************************************************************}
function TALHandlebars.&if(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{#if "value"}}...(main)...{{else}}...(inverse)...{{/if}}
  // {{#if value includeZero=true}}...(main)...{{else}}...(inverse)...{{/if}}
  // {{#if (eq value1 value2)}}...(main)...{{else}}...(inverse)...{{/if}}
  //

  var LOwned: Boolean;
  var LJSonNode := ResolveParamValue(
                     AParams, // const AParams: TALTagParamsA;
                     0, // const AIndex: Integer;
                     AContext, // const AContext: TALJsonNodeA;
                     ADepths, // const ADepths: TList<TALJsonNodeA>;
                     LOwned); // out AOwned: Boolean): TALJsonNodeA;
  try
    var LParamIndex: Integer;
    if IsTruthy(LJSonNode, ALStrToBool(AParams.Values['includeZero'])) then LParamIndex := AParams.IndexOfName(MainBlockParamName)
    else LParamIndex := AParams.IndexOfName(InverseBlockParamName);
    Result := ResolveParamValue(
                AParams, // const AParams: TALTagParamsA;
                LParamIndex, // const AIndex: Integer;
                AContext, // const AContext: TALJsonNodeA;
                ADepths, // const ADepths: TList<TALJsonNodeA>;
                AOwned); // out AOwned: Boolean): TALJsonNodeA;
  finally
    if LOwned then
      ALFreeAndNil(LJSonNode);
  end;

end;

{***************************************************************************************************}
function TALHandlebars.unless(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{#unless "value"}}...(main)...{{else}}...(inverse)...{{/if}}
  // {{#unless value includeZero=true}}...(main)...{{else}}...(inverse)...{{/if}}
  // {{#unless (eq value1 value2)}}...(main)...{{else}}...(inverse)...{{/if}}
  //

  var LOwned: Boolean;
  var LJSonNode := ResolveParamValue(
                     AParams, // const AParams: TALTagParamsA;
                     0, // const AIndex: Integer;
                     AContext, // const AContext: TALJsonNodeA;
                     ADepths, // const ADepths: TList<TALJsonNodeA>;
                     LOwned); // out AOwned: Boolean): TALJsonNodeA;
  try
    var LParamIndex: Integer;
    if IsTruthy(LJSonNode, ALStrToBool(AParams.Values['includeZero'])) then LParamIndex := AParams.IndexOfName(InverseBlockParamName)
    else LParamIndex := AParams.IndexOfName(MainBlockParamName);
    Result := ResolveParamValue(
                AParams, // const AParams: TALTagParamsA;
                LParamIndex, // const AIndex: Integer;
                AContext, // const AContext: TALJsonNodeA;
                ADepths, // const ADepths: TList<TALJsonNodeA>;
                AOwned); // out AOwned: Boolean): TALJsonNodeA;
  finally
    if LOwned then
      ALFreeAndNil(LJSonNode);
  end;

end;

{**************************************************************************************************}
function TALHandlebars.each(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{#each "items"}} ... {{/each}}
  // {{#each items}} ... {{/each}}
  // {{#each (filter items "active")}} ... {{/each}}
  //

  var LParamIndex: Integer;
  var LJSonArrayOwned: Boolean;
  var LJSonArray := ResolveParamValue(
                      AParams, // const AParams: TALTagParamsA;
                      0, // const AIndex: Integer;
                      AContext, // const AContext: TALJsonNodeA;
                      ADepths, // const ADepths: TList<TALJsonNodeA>;
                      LJSonArrayOwned); // out AOwned: Boolean): TALJsonNodeA;
  try
    if (LJSonArray <> nil) and
       (LJSonArray.NodeType in [TALJSONNodeType.ntObject, TALJSONNodeType.ntArray]) and
       (LJSonArray.ChildNodes.Count > 0) then begin
      ADepths.Add(LJSonArray);
      try
        LParamIndex := AParams.IndexOfName(MainBlockParamName);
        var LText: AnsiString := '';
        for var I := 0 to LJSonArray.ChildNodes.count - 1 do begin
          var LContext := LJSonArray.ChildNodes[I];
          var LJsonBlockOwned: Boolean;
          var LJsonBlock := ResolveParamValue(
                              AParams, // const AParams: TALTagParamsA;
                              LParamIndex, // const AIndex: Integer;
                              LContext, // const AContext: TALJsonNodeA;
                              ADepths, // const ADepths: TList<TALJsonNodeA>;
                              LJsonBlockOwned); // out AOwned: Boolean): TALJsonNodeA;
          try
            if LJsonBlock <> nil then
              LText := LText + LJsonBlock.Text;
          finally
            if LJsonBlockOwned then
              ALFreeAndNil(LJsonBlock);
          end;
        end;
        AOwned := True;
        Result := TALJSONTextNodeA.Create('');
        Result.Text := LText;
      finally
        ADepths.Delete(ADepths.Count-1);
      end;
    end
    else begin
      Result := ResolveParamValue(
                  AParams, // const AParams: TALTagParamsA;
                  AParams.IndexOfName(InverseBlockParamName), // const AIndex: Integer;
                  AContext, // const AContext: TALJsonNodeA;
                  ADepths, // const ADepths: TList<TALJsonNodeA>;
                  AOwned); // out AOwned: Boolean): TALJsonNodeA;
    end;
  finally
    if LJSonArrayOwned then
      ALFreeAndNil(LJSonArray);
  end;

end;

{**************************************************************************************************************************************************************}
function TALHandlebars.index(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin
  If (ADepths.Count < 1) or (AContext = nil) then raise Exception.Create('The "@index" variable can only be used inside an "each" block');
  var LParentContext := ADepths[ADepths.Count - 1];
  for var I := 0 to LParentContext.ChildNodes.Count - 1 do
    if LParentContext.ChildNodes[I] = AContext then begin
      AOwned := True;
      Result := TALJsonTextNodeA.Create('');
      Result.Int32 := I;
      exit;
    end;
  Raise Exception.Create('Unable to resolve "@index": the current context was not found in its parent context');
end;

{**************************************************************************************************************************************************************}
function TALHandlebars.key(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin
  AOwned := True;
  Result := TALJsonTextNodeA.Create('');
  if AContext <> nil then
    Result.Text := AContext.NodeName;
end;

{**************************************************************************************************************************************************************}
function TALHandlebars.first(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin
  If (ADepths.Count < 1) or (AContext = nil) then raise Exception.Create('The "@first" variable can only be used inside an "each" block');
  var LParentContext := ADepths[ADepths.Count - 1];
  AOwned := True;
  Result := TALJsonTextNodeA.Create('');
  Result.Bool := (LParentContext.ChildNodes.Count > 0) and (LParentContext.ChildNodes[0] = AContext);
  exit;
end;

{**************************************************************************************************************************************************************}
function TALHandlebars.last(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin
  If (ADepths.Count < 1) or (AContext = nil) then raise Exception.Create('The "@last" variable can only be used inside an "each" block');
  var LParentContext := ADepths[ADepths.Count - 1];
  AOwned := True;
  Result := TALJsonTextNodeA.Create('');
  Result.Bool := (LParentContext.ChildNodes.Count > 0) and (LParentContext.ChildNodes[LParentContext.ChildNodes.Count - 1] = AContext);
  exit;
end;

{**************************************************************************************************************************************************************}
function TALHandlebars.&with(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{#with user}}{{firstname}}-{{lastname}}{{/with}}
  //

  var LParamIndex: Integer;
  var LOwned: Boolean;
  var LJSonNode := ResolveParamValue(
                      AParams, // const AParams: TALTagParamsA;
                      0, // const AIndex: Integer;
                      AContext, // const AContext: TALJsonNodeA;
                      ADepths, // const ADepths: TList<TALJsonNodeA>;
                      LOwned); // out AOwned: Boolean): TALJsonNodeA;
  try
    if (LJSonNode <> nil) and (IsTruthy(LJSonNode)) then begin
      LParamIndex := AParams.IndexOfName(MainBlockParamName);
      ADepths.Add(LJSonNode);
      try
        Result := ResolveParamValue(
                    AParams, // const AParams: TALTagParamsA;
                    LParamIndex, // const AIndex: Integer;
                    LJSonNode, // const AContext: TALJsonNodeA;
                    ADepths, // const ADepths: TList<TALJsonNodeA>;
                    AOwned); // out AOwned: Boolean): TALJsonNodeA;
      finally
        ADepths.Delete(ADepths.Count-1);
      end;
    end
    else begin
      Result := ResolveParamValue(
                  AParams, // const AParams: TALTagParamsA;
                  AParams.IndexOfName(InverseBlockParamName), // const AIndex: Integer;
                  AContext, // const AContext: TALJsonNodeA;
                  ADepths, // const ADepths: TList<TALJsonNodeA>;
                  AOwned); // out AOwned: Boolean): TALJsonNodeA;
    end;
  finally
    if LOwned then
      ALFreeAndNil(LJSonNode);
  end;

end;

{**************************************************************************************************************************************************************}
function TALHandlebars.lookup(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{lookup object "property"}}
  // {{lookup object property}}
  // {{lookup array 0}}
  // {{lookup array @index}}
  //

  // The lookup helper requires exactly two positional arguments:
  // a target object/array and a property name or array index.
  if AParams.Count < 2 then
    raise Exception.Create('The "lookup" helper requires two arguments: a target and a property name or array index');
  if AParams.ItemHasNameValue(0) or
     AParams.ItemHasNameValue(1) then
   raise Exception.Create('The first two arguments of the "lookup" helper must be positional arguments');

  // Resolve the object or array to inspect.
  var LTargetOwned: Boolean;
  var LTarget := ResolveParamValue(
                   AParams, // const AParams: TALTagParamsA;
                   0, // const AIndex: Integer;
                   AContext, // const AContext: TALJsonNodeA;
                   ADepths, // const ADepths: TList<TALJsonNodeA>;
                   LTargetOwned); // out AOwned: Boolean): TALJsonNodeA;
  try
    if (LTarget = nil) or (LTarget.NodeType not in [ntObject, ntArray]) then begin
      AOwned := False;
      Exit(nil);
    end;

    // Resolve the property name or array index.
    var LKeyOwned: Boolean;
    var LKey := ResolveParamValue(
                  AParams, // const AParams: TALTagParamsA;
                  1, // const AIndex: Integer;
                  AContext, // const AContext: TALJsonNodeA;
                  ADepths, // const ADepths: TList<TALJsonNodeA>;
                  LKeyOwned); // out AOwned: Boolean): TALJsonNodeA;
    try
      if LKey = nil then begin
        AOwned := False;
        Exit(nil);
      end;

      var LJsonNode: TALJsonNodeA := nil;
      case LTarget.NodeType of

        // The key is treated as a literal property name. Dots and slashes
        // inside the key are not interpreted as path separators.
        TALJSONNodeType.ntObject: LJsonNode := LTarget.GetChildNode(LKey.Text);

        // Arrays are accessed using a zero-based numeric index.
        TALJSONNodeType.ntArray: begin
          var LIndex: Int32;
          if ALTryStrToInt(LKey.Text, LIndex) and
             (LIndex >= 0) and
             (LIndex < LTarget.ChildNodes.Count) then
            LJsonNode := LTarget.ChildNodes[LIndex];
        end;

      end;

      if LJsonNode = nil then begin
        AOwned := False;
        Exit(nil);
      end;

      // A child node cannot be returned as borrowed when its parent target
      // is owned and will be destroyed when leaving this function.
      if LTargetOwned then begin
        AOwned := True;
        Result := LJsonNode.Clone;
      end
      else begin
        AOwned := False;
        Result := LJsonNode;
      end;

    finally
      if LKeyOwned then
        ALFreeAndNil(LKey);
    end;

  finally
    if LTargetOwned then
      ALFreeAndNil(LTarget);
  end;

end;

{*************************************************************************************************************************************************************}
function TALHandlebars.concat(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{concat value1 " text " value2}}
  //

  if AParams.Count < 1 then
    raise Exception.Create('The "concat" helper requires at least one positional arguments');

  var LTextResult: AnsiString := '';
  for var I := 0 to AParams.Count - 1 do begin
    if AParams.ItemHasNameValue(I) then raise Exception.Create('The "concat" helper only accepts positional arguments');
    var LOwned: Boolean;
    var LJsonNode := ResolveParamValue(
                       AParams, // const AParams: TALTagParamsA;
                       I, // const AIndex: Integer;
                       AContext, // const AContext: TALJsonNodeA;
                       ADepths, // const ADepths: TList<TALJsonNodeA>;
                       LOwned); // out AOwned: Boolean): TALJsonNodeA;
    try
      if (LJsonNode <> nil) then
        LTextResult := LTextResult + LJsonNode.Text;
    finally
      if LOwned then
        ALFreeAndNil(LJsonNode);
    end;
  end;

  AOwned := True;
  Result := TALJSONTextNodeA.Create('');
  Result.Text := LTextResult;

end;

{*****************************************************************************}
function TALHandlebars.eq(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{eq a b}}
  // {{eq a b c}}
  //

  if AParams.Count < 2 then
    raise Exception.Create('The "eq" helper requires at least two positional arguments');
  if AParams.ItemHasNameValue(0) then
    raise Exception.Create('The "eq" helper only accepts positional arguments');

  var LFirstOwned: Boolean;
  var LFirstJsonNode := ResolveParamValue(
                          AParams, // const AParams: TALTagParamsA;
                          0, // const AIndex: Integer;
                          AContext, // const AContext: TALJsonNodeA;
                          ADepths, // const ADepths: TList<TALJsonNodeA>;
                          LFirstOwned); // out AOwned: Boolean): TALJsonNodeA;
  try

    var LBoolResult := True;
    for var I := 1 to AParams.Count - 1 do begin
      if AParams.ItemHasNameValue(I) then raise Exception.Create('The "eq" helper only accepts positional arguments');
      var LOwned: Boolean;
      var LJsonNode := ResolveParamValue(
                         AParams, // const AParams: TALTagParamsA;
                         I, // const AIndex: Integer;
                         AContext, // const AContext: TALJsonNodeA;
                         ADepths, // const ADepths: TList<TALJsonNodeA>;
                         LOwned); // out AOwned: Boolean): TALJsonNodeA;
      try
        LBoolResult := (LFirstJsonNode <> nil) and
                       (LJsonNode <> nil);
        if not LBoolResult then break;
        if (LFirstJsonNode.NodesubType in [nstInt32, nstInt64, nstFloat]) and
           (LJsonNode.NodesubType in [nstInt32, nstInt64, nstFloat]) then
          LBoolResult := CompareJsonNode(LFirstJsonNode, LJsonNode) = 0
        else if (LFirstJsonNode.NodeType = TALJSONNodeType.ntText) and
                (LJsonNode.NodeType = TALJSONNodeType.ntText) and
                (LFirstJsonNode.NodeSubType = LJsonNode.NodeSubType) then
          LBoolResult := ((LFirstJsonNode.NodeSubType = nstText) and (LFirstJsonNode.text = LJsonNode.text)) or
                         ((LFirstJsonNode.NodeSubType = nstboolean) and (LFirstJsonNode.Bool = LJsonNode.Bool)) or
                         ((LFirstJsonNode.NodeSubType = nstDateTime) and (SameDatetime(LFirstJsonNode.DateTime, LJsonNode.DateTime))) or
                         ((LFirstJsonNode.NodeSubType = nstnull))
        else
          LBoolResult := False;
        if not LBoolResult then break;
      finally
        if LOwned then
          ALFreeAndNil(LJsonNode);
      end;
    end;

    AOwned := True;
    Result := TALJSONTextNodeA.Create('');
    Result.Bool := LBoolResult;

  finally
    if LFirstOwned then
      ALFreeAndNil(LFirstJsonNode);
  end;

end;

{*****************************************************************************}
function TALHandlebars.ne(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin
  result := eq(AParams, AContext, ADepths, AOwned);
  if (Result = nil) or (not AOwned) then raise Exception.Create('Error 8F7EA796-D14A-40F1-BBA9-BF7C2106ACA8');
  result.Bool := not result.Bool;
end;

{*****************************************************************************}
function TALHandlebars.gt(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{gt a b}}
  //

  if AParams.Count <> 2 then
    raise Exception.Create('The "gt" helper requires exactly two positional arguments');
  if AParams.ItemHasNameValue(0) or
     AParams.ItemHasNameValue(1) then
    raise Exception.Create('The "gt" helper only accepts positional arguments');

  var LFirstOwned: Boolean;
  var LFirstJsonNode := ResolveParamValue(
                          AParams, // const AParams: TALTagParamsA;
                          0, // const AIndex: Integer;
                          AContext, // const AContext: TALJsonNodeA;
                          ADepths, // const ADepths: TList<TALJsonNodeA>;
                          LFirstOwned); // out AOwned: Boolean): TALJsonNodeA;
  try

    var LSecondOwned: Boolean;
    var LSecondJsonNode := ResolveParamValue(
                             AParams, // const AParams: TALTagParamsA;
                             1, // const AIndex: Integer;
                             AContext, // const AContext: TALJsonNodeA;
                             ADepths, // const ADepths: TList<TALJsonNodeA>;
                             LSecondOwned); // out AOwned: Boolean): TALJsonNodeA;
    try

      var LBoolResult := (LFirstJsonNode <> nil) and
                         (LSecondJsonNode <> nil) and
                         (CompareJsonNode(LFirstJsonNode, LSecondJsonNode) > 0);

      AOwned := True;
      Result := TALJSONTextNodeA.Create('');
      Result.Bool := LBoolResult;

    finally
      if LSecondOwned then
        ALFreeAndNil(LSecondJsonNode);
    end;

  finally
    if LFirstOwned then
      ALFreeAndNil(LFirstJsonNode);
  end;

end;

{*****************************************************************************}
function TALHandlebars.lt(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{lt a b}}
  //

  if AParams.Count <> 2 then
    raise Exception.Create('The "lt" helper requires exactly two positional arguments');
  if AParams.ItemHasNameValue(0) or
     AParams.ItemHasNameValue(1) then
    raise Exception.Create('The "lt" helper only accepts positional arguments');

  var LFirstOwned: Boolean;
  var LFirstJsonNode := ResolveParamValue(
                          AParams, // const AParams: TALTagParamsA;
                          0, // const AIndex: Integer;
                          AContext, // const AContext: TALJsonNodeA;
                          ADepths, // const ADepths: TList<TALJsonNodeA>;
                          LFirstOwned); // out AOwned: Boolean): TALJsonNodeA;
  try

    var LSecondOwned: Boolean;
    var LSecondJsonNode := ResolveParamValue(
                             AParams, // const AParams: TALTagParamsA;
                             1, // const AIndex: Integer;
                             AContext, // const AContext: TALJsonNodeA;
                             ADepths, // const ADepths: TList<TALJsonNodeA>;
                             LSecondOwned); // out AOwned: Boolean): TALJsonNodeA;
    try

      var LBoolResult := (LFirstJsonNode <> nil) and
                         (LSecondJsonNode <> nil) and
                         (CompareJsonNode(LFirstJsonNode, LSecondJsonNode) < 0);

      AOwned := True;
      Result := TALJSONTextNodeA.Create('');
      Result.Bool := LBoolResult;

    finally
      if LSecondOwned then
        ALFreeAndNil(LSecondJsonNode);
    end;

  finally
    if LFirstOwned then
      ALFreeAndNil(LFirstJsonNode);
  end;

end;

{*****************************************************************************}
function TALHandlebars.gte(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{gte a b}}
  //

  if AParams.Count <> 2 then
    raise Exception.Create('The "gte" helper requires exactly two positional arguments');
  if AParams.ItemHasNameValue(0) or
     AParams.ItemHasNameValue(1) then
    raise Exception.Create('The "gte" helper only accepts positional arguments');

  var LFirstOwned: Boolean;
  var LFirstJsonNode := ResolveParamValue(
                          AParams, // const AParams: TALTagParamsA;
                          0, // const AIndex: Integer;
                          AContext, // const AContext: TALJsonNodeA;
                          ADepths, // const ADepths: TList<TALJsonNodeA>;
                          LFirstOwned); // out AOwned: Boolean): TALJsonNodeA;
  try

    var LSecondOwned: Boolean;
    var LSecondJsonNode := ResolveParamValue(
                             AParams, // const AParams: TALTagParamsA;
                             1, // const AIndex: Integer;
                             AContext, // const AContext: TALJsonNodeA;
                             ADepths, // const ADepths: TList<TALJsonNodeA>;
                             LSecondOwned); // out AOwned: Boolean): TALJsonNodeA;
    try

      var LBoolResult := (LFirstJsonNode <> nil) and
                         (LSecondJsonNode <> nil) and
                         (CompareJsonNode(LFirstJsonNode, LSecondJsonNode) >= 0);

      AOwned := True;
      Result := TALJSONTextNodeA.Create('');
      Result.Bool := LBoolResult;

    finally
      if LSecondOwned then
        ALFreeAndNil(LSecondJsonNode);
    end;

  finally
    if LFirstOwned then
      ALFreeAndNil(LFirstJsonNode);
  end;

end;

{*****************************************************************************}
function TALHandlebars.lte(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{lte a b}}
  //

  if AParams.Count <> 2 then
    raise Exception.Create('The "lte" helper requires exactly two positional arguments');
  if AParams.ItemHasNameValue(0) or
     AParams.ItemHasNameValue(1) then
    raise Exception.Create('The "lte" helper only accepts positional arguments');

  var LFirstOwned: Boolean;
  var LFirstJsonNode := ResolveParamValue(
                          AParams, // const AParams: TALTagParamsA;
                          0, // const AIndex: Integer;
                          AContext, // const AContext: TALJsonNodeA;
                          ADepths, // const ADepths: TList<TALJsonNodeA>;
                          LFirstOwned); // out AOwned: Boolean): TALJsonNodeA;
  try

    var LSecondOwned: Boolean;
    var LSecondJsonNode := ResolveParamValue(
                             AParams, // const AParams: TALTagParamsA;
                             1, // const AIndex: Integer;
                             AContext, // const AContext: TALJsonNodeA;
                             ADepths, // const ADepths: TList<TALJsonNodeA>;
                             LSecondOwned); // out AOwned: Boolean): TALJsonNodeA;
    try

      var LBoolResult := (LFirstJsonNode <> nil) and
                         (LSecondJsonNode <> nil) and
                         (CompareJsonNode(LFirstJsonNode, LSecondJsonNode) <= 0);

      AOwned := True;
      Result := TALJSONTextNodeA.Create('');
      Result.Bool := LBoolResult;

    finally
      if LSecondOwned then
        ALFreeAndNil(LSecondJsonNode);
    end;

  finally
    if LFirstOwned then
      ALFreeAndNil(LFirstJsonNode);
  end;

end;

{*****************************************************************************}
function TALHandlebars.&and(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{and a b}}
  // {{and a b c}}
  //

  if AParams.Count < 2 then
    raise Exception.Create('The "and" helper requires at least two positional arguments');

  var LBoolResult := True;
  for var I := 0 to AParams.Count - 1 do begin
    if AParams.ItemHasNameValue(I) then raise Exception.Create('The "and" helper only accepts positional arguments');
    var LOwned: Boolean;
    var LJsonNode := ResolveParamValue(
                       AParams, // const AParams: TALTagParamsA;
                       I, // const AIndex: Integer;
                       AContext, // const AContext: TALJsonNodeA;
                       ADepths, // const ADepths: TList<TALJsonNodeA>;
                       LOwned); // out AOwned: Boolean): TALJsonNodeA;
    try
      LBoolResult := LBoolResult and IsTruthy(LJsonNode);
      if not LBoolResult then break;
    finally
      if LOwned then
        ALFreeAndNil(LJsonNode);
    end;
  end;

  AOwned := True;
  Result := TALJSONTextNodeA.Create('');
  Result.Bool := LBoolResult;

end;

{*****************************************************************************}
function TALHandlebars.&or(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{or a b}}
  // {{or a b c}}
  //

  if AParams.Count < 2 then
    raise Exception.Create('The "or" helper requires at least two positional arguments');

  var LBoolResult := False;
  for var I := 0 to AParams.Count - 1 do begin
    if AParams.ItemHasNameValue(I) then raise Exception.Create('The "or" helper only accepts positional arguments');
    var LOwned: Boolean;
    var LJsonNode := ResolveParamValue(
                       AParams, // const AParams: TALTagParamsA;
                       I, // const AIndex: Integer;
                       AContext, // const AContext: TALJsonNodeA;
                       ADepths, // const ADepths: TList<TALJsonNodeA>;
                       LOwned); // out AOwned: Boolean): TALJsonNodeA;
    try
      LBoolResult := LBoolResult or IsTruthy(LJsonNode);
      if LBoolResult then break;
    finally
      if LOwned then
        ALFreeAndNil(LJsonNode);
    end;
  end;

  AOwned := True;
  Result := TALJSONTextNodeA.Create('');
  Result.Bool := LBoolResult;

end;

{*****************************************************************************}
function TALHandlebars.&not(const AParams: TALTagParamsA; const AContext: TALJSONNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{not a}}
  //

  if AParams.Count <> 1 then
    raise Exception.Create('The "not" helper requires exactly one positional argument');

  if AParams.ItemHasNameValue(0) then raise Exception.Create('The "not" helper only accepts positional arguments');
  var LOwned: Boolean;
  var LJsonNode := ResolveParamValue(
                     AParams, // const AParams: TALTagParamsA;
                     0, // const AIndex: Integer;
                     AContext, // const AContext: TALJsonNodeA;
                     ADepths, // const ADepths: TList<TALJsonNodeA>;
                     LOwned); // out AOwned: Boolean): TALJsonNodeA;
  try
    AOwned := True;
    Result := TALJSONTextNodeA.Create('');
    Result.Bool := not IsTruthy(LJsonNode);
  finally
    if LOwned then
      ALFreeAndNil(LJsonNode);
  end;

end;

{********************************}
function TALHandlebars.ResolveTag(
           const ATagName: AnsiString;
           const ATagParams: TALTagParamsA;
           const AContext: TALJsonNodeA;
           const ADepths: TList<TALJsonNodeA>;
           out AOwned: Boolean): TALJsonNodeA;
begin

  // Resolve the tag as a registered helper.
  var LHelperHandler: THelperHandler;
  if FHelpers.TryGetValue(ATagName, LHelperHandler) then begin
    AOwned := True;
    Result := LHelperHandler(
                ATagParams, // const AParams: TALTagParamsA;
                AContext, // const AContext: TALJsonNodeA;
                ADepths, // const ADepths: TList<TALJsonNodeA>;
                AOwned); // var AOwned: Boolean): TALJsonNodeA
    exit;
  end;

  // Return nil when no context is available.
  if AContext = nil then begin
    AOwned := False;
    Exit(nil);
  end;

  // Resolve a borrowed node from the current JSON context.
  AOwned := False;
  Result := ResolvePath(
              ATagName, // const APath: AnsiString;
              AContext, // const AContext: TALJsonNodeA;
              ADepths); // const ADepths: TList<TALJsonNodeA>

end;

{*********************************}
function TALHandlebars.ResolvePath(
           const APath: AnsiString;
           const AContext: TALJsonNodeA;
           const ADepths: TList<TALJsonNodeA>): TALJsonNodeA;
begin

  var LPath := APath;
  var LContext := AContext;
  if (ADepths = nil) or (ADepths.Count <= 0) then
    raise Exception.Create('Error 0016053E-7261-41EE-961E-97714CCB2567');
  var LDepthIndex: Integer := ADepths.Count - 1;

  // Resolve @root and @root.xxx.
  if (ALPosA('@root', LPath) = 1) and
     ((Length(LPath) = 5) or
      ((Length(LPath) > 5) and
       (LPath[6] in ['.', '/']))) then begin
    LContext := ADepths[0];
    if LPath = '@root' then Exit(LContext);
    Delete(LPath, 1, 6{length('@root.') / length('@root/')});
  end

  // Resolve one or more parent-context prefixes.
  else begin

    // Resolve "../"
    while ALPosA('../', LPath) = 1 do begin
      if LDepthIndex <= 0 then Exit(nil);
      Dec(LDepthIndex);
      LContext := ADepths[LDepthIndex];
      Delete(LPath, 1, 3{length('../')});
    end;

    // Resolve ".."
    if LPath = '..' then begin
      if LDepthIndex <= 0 then Exit(nil);
      Exit(ADepths[LDepthIndex - 1]);
    end;

  end;

  // Resolve current-context aliases.
  if (LPath = '') or
     (LPath = '.') or
     (LPath = 'this') then
    Exit(LContext);

  // Remove "./".
  while ALPosA('./', LPath) = 1 do
    Delete(LPath, 1, 2{length('./')});

  // Remove "this." or "this/".
  While (ALPosA('this', LPath) = 1) and
        (Length(LPath) > 4) and
        (LPath[5] in ['.', '/']) do
    Delete(LPath, 1, 5{length('this.') or length('this/')});

  if LPath = '' then
    Exit(LContext);

  // A remaining property path requires a context.
  if LContext = nil then
    Exit(nil);

  // Handlebars accepts both "." and "/" as path separators.
  Result := LContext.GetChildNode(ALSplitA(LPath, ['.', '/']));

end;

{***************************************}
function TALHandlebars.TagReplaceHandler(
           const ATagName: AnsiString;
           const ATagParams: TALTagParamsA;
           const AContext: pointer): AnsiString;
begin
  var LContext: TTagReplaceHandlerContext := TTagReplaceHandlerContext(AContext^);
  var LOwned: Boolean;
  var LJsonNode := ResolveTag(
                     ATagName, // const ATagName: AnsiString;
                     ATagParams, // const ATagParams: TALTagParamsA;
                     LContext.Json, // const AContext: TALJsonNodeA;
                     LContext.Depths, // const ADepths: TList<TALJsonNodeA>;
                     LOwned); // out AOwned: Boolean): TALJsonNodeA;
  try
    if LJsonNode <> nil then Result := LJsonNode.Text
    else Result := '';
  finally
    if LOwned then
      ALFreeAndNil(LJsonNode);
  end;

  // The presence of MainBlockParamName indicates a block helper.
  // Its result already contains rendered template content and must not be escaped again.
  if (Result <> '') and
     (ATagName <> '>') and
     (ATagParams.IndexOfName(MainBlockParamName) < 0) then begin
    LJsonNode := ResolveParamValue(
                   ATagParams, // const AParams: TALTagParamsA;
                   ATagParams.IndexOfName(UnescapedParamName), // const AIndex: Integer;
                   LContext.Json, // const AContext: TALJsonNodeA;
                   LContext.Depths, // const ADepths: TList<TALJsonNodeA>;
                   LOwned); // out AOwned: Boolean): TALJsonNodeA;
    try
      if not IsTruthy(LJsonNode) then
        Result := ALXMLAttributeEncode(Result);
    finally
      if LOwned then
        ALFreeAndNil(LJsonNode);
    end;
  end;
end;

{*****************************************************************************************************}
function TALHandlebars.CompareJsonNode(const AJsonNode1, AJsonNode2: TALJsonNodeA): TValueRelationship;
begin
  case AJsonNode1.NodeSubType of
    nstInt32: begin
      case AJsonNode2.NodeSubType of
        nstInt32: Result := CompareValue(AJsonNode1.Int32, AJsonNode2.Int32);
        nstInt64: Result := CompareValue(AJsonNode1.Int32, AJsonNode2.Int64);
        nstFloat: Result := CompareValue(AJsonNode1.Int32, AJsonNode2.Float);
        nstDateTime: Raise Exception.Create('An integer JSON value cannot be compared with a date-time JSON value');
        else Raise Exception.Create('An integer JSON value cannot be compared with this JSON value type');
      end;
    end;
    nstInt64: begin
      case AJsonNode2.NodeSubType of
        nstInt32: Result := CompareValue(AJsonNode1.Int64, AJsonNode2.Int32);
        nstInt64: Result := CompareValue(AJsonNode1.Int64, AJsonNode2.Int64);
        nstFloat: Result := CompareValue(AJsonNode1.Int64, AJsonNode2.Float);
        nstDateTime: Raise Exception.Create('An integer JSON value cannot be compared with a date-time JSON value');
        else Raise Exception.Create('An integer JSON value cannot be compared with this JSON value type');
      end;
    end;
    nstFloat: begin
      case AJsonNode2.NodeSubType of
        nstInt32: Result := CompareValue(AJsonNode1.Float, AJsonNode2.Int32);
        nstInt64: Result := CompareValue(AJsonNode1.Float, AJsonNode2.Int64);
        nstFloat: Result := CompareValue(AJsonNode1.Float, AJsonNode2.Float);
        nstDateTime: Raise Exception.Create('A floating-point JSON value cannot be compared with a date-time JSON value');
        else Raise Exception.Create('A floating-point JSON value cannot be compared with this JSON value type');
      end;
    end;
    nstDateTime: begin
      case AJsonNode2.NodeSubType of
        nstInt32: Raise Exception.Create('A date-time JSON value cannot be compared with a numeric JSON value');
        nstInt64: Raise Exception.Create('A date-time JSON value cannot be compared with a numeric JSON value');
        nstFloat: Raise Exception.Create('A date-time JSON value cannot be compared with a numeric JSON value');
        nstDateTime: Result := CompareDateTime(AJsonNode1.DateTime, AJsonNode2.DateTime);
        else Raise Exception.Create('A date-time JSON value cannot be compared with this JSON value type');
      end;
    end;
    else
      Raise Exception.Create('The first JSON value type does not support comparison');
  end;
end;

{*************************************}
procedure TALHandlebars.RegisterHelper(
            const AName: AnsiString;
            const AHandler: THelperHandler);
begin
  FHelpers.AddOrSetValue(AName, AHandler);
end;

{**********************************}
function TALHandlebars.ResolveTemplate(
           const ATemplateSource: AnsiString;
           const AContext: TTagReplaceHandlerContext): AnsiString;
begin
  Result := ALReplacePrecompiledTagsA(
              ATemplateSource, // const ASourceString: AnsiString;
              ALDefaultPrecompiledTagStartMarker, // const ATagStartMarker,
              ALDefaultPrecompiledTagEndMarker, // ATagEndMarker: AnsiChar;
              TagReplaceHandler, // const AReplaceFunc: TALTagReplaceFuncA;
              @AContext, // const AContext: Pointer;
              False); // const AReplaceTagsInResult: Boolean = False): AnsiString; overload;
end;

{**********************************}
function TALHandlebars.ResolveTemplate(
           const ATemplateSource: AnsiString;
           const AContext: TALJsonNodeA): AnsiString;
begin
  var LContext: TTagReplaceHandlerContext;
  LContext.Json := AContext;
  LContext.Depths := TList<TALJsonNodeA>.Create;
  Try
    LContext.Depths.Add(AContext);
    Result := ResolveTemplate(ATemplateSource, LContext);
  finally
    ALFreeAndNil(LContext.Depths);
  End;
end;

{****************************}
function TALHandlebars.Render(
           const ATemplateName: AnsiString;
           const AContext: TALJsonNodeA): AnsiString;
begin
  var LTemplaceSource: AnsiString;
  If not FTemplates.TryGetValue(ATemplateName, LTemplaceSource) then
    raise Exception.CreateFmt('Template "%s" not found', [ATemplateName]);
  Result := ResolveTemplate(
              LTemplaceSource, // const ATemplateSource: AnsiString;
              AContext); // const AContext: TALJsonNodeA;
end;

{******************************}
function TALHandlebars.IsTruthy(
           const AJsonNode: TALJsonNodeA;
           const AIncludeZero: Boolean): Boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function IsTextTruthy: Boolean;
  begin
    Result := AJsonNode.Text <> '';
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function IsBinaryBytesTruthy: Boolean;
  begin
    Result := Length(AJsonNode.BinaryAsBytes) > 0;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function IsBinaryStreamTruthy: Boolean;
  begin
    Result := AJsonNode.BinaryAsStream.Size > 0;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function IsBinaryTruthy: Boolean;
  begin
    case AJsonNode.StorageKind of
      TALJSONStorageKind.skBytes: Result := IsBinaryBytesTruthy;
      TALJSONStorageKind.skOwnedStream,
      TALJSONStorageKind.skBorrowedStream: Result := IsBinaryStreamTruthy;
      else raise Exception.Create('Error 223F2DC0-F6F5-4093-8185-0544EF7F3388');
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function IsObjectIDTruthy: Boolean;
  begin
    Result := Length(AJsonNode.ObjectID) > 0;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function IsRegExTruthy: Boolean;
  begin
    Result := AJsonNode.RegEx <> '';
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function IsJavascriptTruthy: Boolean;
  begin
    Result := AJsonNode.Javascript <> '';
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function IsTimestampTruthy: Boolean;
  begin
    Result := AIncludeZero or (AJsonNode.Timestamp.I64 <> 0);
  end;

begin
  if AJsonNode = nil then Exit(False);
  case AJsonNode.NodeSubType of
    TALJsonNodeSubType.nstFloat: Result := AIncludeZero or (not SameValue(AJsonNode.Float, 0));
    TALJsonNodeSubType.nstText: Result := IsTextTruthy;
    TALJsonNodeSubType.nstObject: Result := true;
    TALJsonNodeSubType.nstArray: Result := AJsonNode.ChildNodes.Count > 0;
    TALJsonNodeSubType.nstBinary: Result := IsBinaryTruthy;
    TALJsonNodeSubType.nstObjectID: Result := IsObjectIDTruthy;
    TALJsonNodeSubType.nstBoolean: Result := AJsonNode.Bool;
    TALJsonNodeSubType.nstDateTime: Result := AIncludeZero or (not SameDateTime(AJsonNode.DateTime, ALNullDate));
    TALJsonNodeSubType.nstNull: Result := False;
    TALJsonNodeSubType.nstRegEx: Result := IsRegExTruthy;
    TALJsonNodeSubType.nstJavascript: Result := IsJavascriptTruthy;
    TALJsonNodeSubType.nstInt32: Result := AIncludeZero or (AJsonNode.Int32 <> 0);
    TALJsonNodeSubType.nstTimestamp: Result := IsTimestampTruthy;
    TALJsonNodeSubType.nstInt64: Result := AIncludeZero or (AJsonNode.Int64 <> 0);
    else raise Exception.Create('Error 13D6352A-AA12-43F0-9EB6-7E89E9CFEF3B');
  end;
end;

{***************************************}
function TALHandlebars.ResolveParamValue(
           const AParams: TALTagParamsA;
           const AIndex: Integer;
           const AContext: TALJsonNodeA;
           const ADepths: TList<TALJsonNodeA>;
           out AOwned: Boolean): TALJsonNodeA;
begin

  // Return nil when the requested parameter does not exist.
  if (AIndex < 0) or (AIndex >= AParams.Count) then begin
    AOwned := False;
    Exit(nil);
  end;

  // Extract the parameter value, taking named parameters into account.
  var LParamValue: AnsiString;
  var LItemHasNameValue := AParams.ItemHasNameValue(AIndex);
  if LItemHasNameValue then LParamValue := AParams.ValueFromIndex[AIndex]
  else LParamValue := AParams[AIndex];

  // Treat quoted and empty parameters as literal string values.
  if (LParamValue = '') or (AParams.WasQuoted[AIndex]) then begin
    AOwned := True;
    Result := TALJSONTextNodeA.Create('');
    Result.Text := LParamValue;
    Exit;
  end;

  // Retrieve the parameter name when this is a named parameter.
  var LName: AnsiString;
  if LItemHasNameValue then LName := AParams.Names[AIndex]
  else LName := '';

  // Render the main or inverse block using the current context.
  if (LName = MainBlockParamName) or
     (LName = InverseBlockParamName) then begin
    var LContext: TTagReplaceHandlerContext;
    LContext.Json := AContext;
    LContext.Depths := ADepths;
    var LText := ResolveTemplate(
                   LParamValue, // const ATemplateSource: AnsiString;
                   LContext); // const AContext: TTagReplaceHandlerContext;
    AOwned := True;
    Result := TALJSONTextNodeA.Create('');
    Result.Text := LText;
    Exit;
  end;

  // Resolve a precompiled subexpression such as {{helper (subhelper value)}}.
  if (length(LParamValue) = 2 + sizeOf(Pointer)) {length(#2<address>#3} and
     (LParamValue[low(LParamValue)] = ALDefaultPrecompiledTagStartMarker) and
     (LParamValue[high(LParamValue)] = ALDefaultPrecompiledTagEndMarker) then begin
    var LPrecompiledTag: TALPrecompiledTagA := PPointer(@LParamValue[low(LParamValue)+1])^;
    Result := ResolveTag(
                LPrecompiledTag.TagName, // const ATagName: AnsiString;
                LPrecompiledTag.TagParams, // const ATagParams: TALTagParamsA;
                AContext, // const AContext: TALJsonNodeA;
                ADepths, // const ADepths: TList<TALJsonNodeA>;
                AOwned); // out AOwned: Boolean): TALJsonNodeA;
    Exit;
  end;

  // Resolve unquoted literal values.
  // true, false, null, 123, -123, 12.8, -12.8, 1.5e3
  if LParamValue = 'true' then begin
    AOwned := True;
    Result := TALJSONTextNodeA.Create('');
    Result.Bool := True;
    Exit;
  end;

  if LParamValue = 'false' then begin
    AOwned := True;
    Result := TALJSONTextNodeA.Create('');
    Result.Bool := False;
    Exit;
  end;

  if LParamValue = 'null' then begin
    AOwned := True;
    Result := TALJSONTextNodeA.Create('');
    Result.Null := True;
    Exit;
  end;

  var LInt64: Int64;
  if ALTryStrToInt64(LParamValue, LInt64) then begin
    AOwned := True;
    Result := TALJSONTextNodeA.Create('');
    if (LInt64 >= Low(Int32)) and
       (LInt64 <= High(Int32)) then
      Result.Int32 := Int32(LInt64)
    else
      Result.Int64 := LInt64;
    Exit;
  end;

  var LFloat: Double;
  if ALTryStrToFloat(LParamValue, LFloat) then begin
    AOwned := True;
    Result := TALJSONTextNodeA.Create('');
    Result.Float := LFloat;
    Exit;
  end;

  // Resolve built-in data variables when they are used as parameters,
  // for example: {{#if @first}}.
  if (LParamValue = '@index') or
     (LParamValue = '@key') or
     (LParamValue = '@first') or
     (LParamValue = '@last') then begin
    Result := ResolveTag(
                LParamValue, // const ATagName: AnsiString;
                AParams, // const ATagParams: TALTagParamsA;
                AContext, // const AContext: TALJsonNodeA;
                ADepths, // const ADepths: TList<TALJsonNodeA>;
                AOwned); // out AOwned: Boolean): TALJsonNodeA;
    Exit;
  end;

  // Return nil when no context is available.
  if AContext = nil then begin
    AOwned := False;
    Exit(nil);
  end;

  // Resolve a borrowed node from the current JSON context.
  AOwned := False;
  Result := ResolvePath(
              LParamValue, // const APath: AnsiString;
              AContext, // const AContext: TALJsonNodeA;
              ADepths); // const ADepths: TList<TALJsonNodeA>

end;

end.
