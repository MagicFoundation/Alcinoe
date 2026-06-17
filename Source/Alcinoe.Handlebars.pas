unit Alcinoe.Handlebars;

interface

{$I Alcinoe.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Contnrs,
  Alcinoe.Common,
  Alcinoe.StringUtils,
  Alcinoe.StringList,
  Alcinoe.JSONDoc;

type

  {*******************}
  TALHandlebars = class
  public
    const
      MainBlockParamName = '__main';
      InverseBlockParamName = '__inverse';
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
    function TagReplaceHandler(
               const ATagName: AnsiString;
               const ATagParams: TALTagParamsA;
               const AContext: pointer): AnsiString; virtual;
    function RenderSource(
               const ATemplateSource: AnsiString;
               const AContext: TTagReplaceHandlerContext): AnsiString; overload; virtual;
  protected

    {$REGION 'Default Helpers'}
    function &if(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function unless(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function each(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function this(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    function &with(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
    {$ENDREGION}

  public
    constructor Create(const ATemplatesPath: String); virtual;
    destructor Destroy; override;
    procedure RegisterHelper(
                const AName: AnsiString;
                const AHandler: THelperHandler); virtual;
    function RenderFile(
               const ATemplateFilename: AnsiString;
               const AContext: TALJsonNodeA): AnsiString; virtual;
    function RenderSource(
               const ATemplateSource: AnsiString;
               const AContext: TALJsonNodeA): AnsiString; overload; virtual;
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
  System.DateUtils,
  System.AnsiStrings,
  System.Math,
  System.IOUtils,
  System.StrUtils;

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
    var LTemplateSrc := ALPrecompileTagsA(
                          ALGetStringFromFile(LTemplateFilePath), // const ASourceString: AnsiString;
                          '{{', // const ATagStart: AnsiString;
                          '}}', // const ATagEnd: AnsiString;
                          FTagsContainer, // const ATagsContainer: TObjectList;
                          [rfreplaceall], // const AFlags: TReplaceFlags=[rfreplaceall];
                          false, // const AQuoteDoublingEscape: Boolean = False;
                          '\', // const AEscapeChar: AnsiChar = #0; // ex: '\' like in {{uppercase "abc\"def}}
                          '=', // const ANameValueSeparator: AnsiChar = '='; // ex: '=' like in {{link title url="/home" target="_blank"}}
                          '#', // const ABlockStartChar: AnsiChar = #0; // ex: '#' like in {{#if ...}}
                          '/', // const ABlockEndChar: AnsiChar = #0; // ex: '/' like in {{/if}}
                          'else', // const ABlockElseTag: AnsiString = ''; // ex: 'else' like in {{else}}
                          '(', // const ASubExpressionTagStart: AnsiString = ''; // ex: '(' like in {{uppercase (concat a b)}}
                          ')', // const ASubExpressionTagEnd: AnsiString = ''; // ex: ')' like in {{uppercase (concat a b)}}
                          False); // const AHandleTagsInQuote: Boolean = True): AnsiString; // False = quoted params are kept literal
    FTemplates.Add(AnsiString(LTemplateFileName){name}, LTemplateSrc{content});
  end;

  RegisterHelper('if', &if);
  RegisterHelper('unless', unless);
  RegisterHelper('each', each);
  RegisterHelper('this', this);
  RegisterHelper('with', &with);

end;

{*******************************}
destructor TALHandlebars.Destroy;
begin
  ALFreeAndNil(FTemplates);
  ALFreeAndNil(FHelpers);
  ALFreeAndNil(FTagsContainer);
  inherited;
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
      LParamIndex := AParams.IndexOfName(MainBlockParamName);
      var LText: AnsiString := '';
      for var I := 0 to LJSonArray.ChildNodes.count - 1 do begin
        var LContext := LJSonArray.ChildNodes[I];
        ADepths.Add(LContext);
        Try
          var LJsonBlockOwned: Boolean;
          var LJsonBlock := ResolveParamValue(
                              AParams, // const AParams: TALTagParamsA;
                              LParamIndex, // const AIndex: Integer;
                              LContext, // const AContext: TALJsonNodeA;
                              ADepths, // const ADepths: TList<TALJsonNodeA>;
                              LJsonBlockOwned); // out AOwned: Boolean): TALJsonNodeA;
          try
            if LJsonBlock <> nil then LText := LText + LJsonBlock.Text;
          finally
            if LJsonBlockOwned then
              ALFreeAndNil(LJsonBlock);
          end;
        Finally
          ADepths.Delete(ADepths.Count-1);
        End;
      end;
      AOwned := True;
      Result := TALJSONTextNodeA.Create('');
      Result.Text := LText;
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

{*************************************************************************************************************************************************************}
function TALHandlebars.this(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>; var AOwned: Boolean): TALJsonNodeA;
begin

  //
  // {{this}}
  //

  AOwned := False;
  Result := AContext;

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
  var LPath := ALSplitA(ATagName, ['.']);
  AOwned := False;
  Result := AContext.GetChildNode(LPath);

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
end;

{*************************************}
procedure TALHandlebars.RegisterHelper(
            const AName: AnsiString;
            const AHandler: THelperHandler);
begin
  FHelpers.Add(AName, AHandler);
end;

{**********************************}
function TALHandlebars.RenderSource(
           const ATemplateSource: AnsiString;
           const AContext: TTagReplaceHandlerContext): AnsiString;
begin
  Result := ALReplaceTagsA(
              ATemplateSource, // Const SourceString,
              '{{', // TagStart,
              '}}', // TagEnd: AnsiString;
              TagReplaceHandler, // ReplaceProc: TALHandleTagFunctA;
              @AContext, // Context: Pointer;
              [rfreplaceall], // const AFlags: TReplaceFlags=[rfreplaceall];
              False, // const AReplaceTagsInResult: Boolean = False;
              False, // const AQuoteDoublingEscape: Boolean = False;
              '\', // const AEscapeChar: AnsiChar = #0;
              '='); // const ANameValueSeparator: AnsiChar = '=')
end;

{**********************************}
function TALHandlebars.RenderSource(
           const ATemplateSource: AnsiString;
           const AContext: TALJsonNodeA): AnsiString;
begin
  var LContext: TTagReplaceHandlerContext;
  LContext.Json := AContext;
  LContext.Depths := TList<TALJsonNodeA>.Create;
  Try
    LContext.Depths.Add(AContext);
    Result := RenderSource(ATemplateSource, LContext);
  finally
    ALFreeAndNil(LContext.Depths);
  End;
end;

{********************************}
function TALHandlebars.RenderFile(
           const ATemplateFilename: AnsiString;
           const AContext: TALJsonNodeA): AnsiString;
begin
  var LTemplaceSource: AnsiString;
  If not FTemplates.TryGetValue(ATemplateFilename, LTemplaceSource) then
    raise Exception.CreateFmt('Template "%s" not found', [ATemplateFilename]);
  Result := RenderSource(
              LTemplaceSource, // const ATemplateSource: AnsiString;
              AContext); // const AContext: TALJsonNodeA;
end;

{******************************}
function TALHandlebars.IsTruthy(
           const AJsonNode: TALJsonNodeA;
           const AIncludeZero: Boolean = False): Boolean;
begin
  if AJsonNode = nil then exit(False);
  case AJsonNode.NodeSubType of
    TALJsonNodeSubType.nstFloat: begin
      if (not SameValue(AJsonNode.Float, 0)) or (AIncludeZero) then Result := True
      else Result := False;
    end;
    TALJsonNodeSubType.nstText: begin
      if AJsonNode.text <> '' then Result := True
      else Result := False;
    end;
    TALJsonNodeSubType.nstObject: begin
      if AJsonNode.ChildNodes.Count > 0 then Result := True
      else Result := False;
    end;
    TALJsonNodeSubType.nstArray: begin
      if AJsonNode.ChildNodes.Count > 0 then Result := True
      else Result := False;
    end;
    TALJsonNodeSubType.nstBinary: begin
      case AJsonNode.StorageKind of
        TALJSONStorageKind.skBytes: begin
          if length(AJsonNode.BinaryAsBytes) > 0 then Result := True
          else Result := False;
        end;
        TALJSONStorageKind.skOwnedStream,
        TALJSONStorageKind.skBorrowedStream: begin
          if AJsonNode.BinaryAsStream.Size > 0 then Result := True
          else Result := False;
        end
        else raise Exception.Create('Error 223F2DC0-F6F5-4093-8185-0544EF7F3388');
      end;
    end;
    TALJsonNodeSubType.nstObjectID: begin
      if length(AJsonNode.ObjectID) > 0 then Result := True
      else Result := False;
    end;
    TALJsonNodeSubType.nstBoolean: begin
      if AJsonNode.Bool then Result := True
      else Result := False;
    end;
    TALJsonNodeSubType.nstDateTime: begin
      if (Not SameDateTime(AJsonNode.DateTime, ALNullDate)) or (AIncludeZero) then Result := True
      else Result := False;
    end;
    TALJsonNodeSubType.nstNull: begin
      Result := False;
    end;
    TALJsonNodeSubType.nstRegEx: begin
      if AJsonNode.RegEx <> '' then Result := True
      else Result := False;
    end;
    TALJsonNodeSubType.nstJavascript: begin
      if AJsonNode.Javascript <> '' then Result := True
      else Result := False;
    end;
    TALJsonNodeSubType.nstInt32: begin
      if (AJsonNode.Int32 <> 0) or (AIncludeZero) then Result := True
      else Result := False;
    end;
    TALJsonNodeSubType.nstTimestamp: begin
      if (AJsonNode.Timestamp.I64 <> 0) or (AIncludeZero) then Result := True
      else Result := False;
    end;
    TALJsonNodeSubType.nstInt64: begin
      if (AJsonNode.Int64 <> 0) or (AIncludeZero) then Result := True
      else Result := False;
    end
    else
      raise Exception.Create('Error 13D6352A-AA12-43F0-9EB6-7E89E9CFEF3B');
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
    var LText := RenderSource(
                   LParamValue, // const ATemplateSource: AnsiString;
                   LContext); // const AContext: TTagReplaceHandlerContext;
    AOwned := True;
    Result := TALJSONTextNodeA.Create('');
    Result.Text := LText;
    Exit;
  end;

  // Resolve a precompiled subexpression such as {{helper (subhelper value)}}.
  if (length(LParamValue) = 6 + sizeOf(Pointer)) (*length('{{#2<raw_address>#3}}'*) and
     (LParamValue[low(LParamValue)] = '{') and
     (LParamValue[low(LParamValue)+1] = '{') and
     (LParamValue[low(LParamValue)+2] = #2) and
     (LParamValue[high(LParamValue)-2] = #3) and
     (LParamValue[high(LParamValue)-1] = '}') and
     (LParamValue[high(LParamValue)] = '}') then begin
    var LPrecompiledTag: TALPrecompiledTagA := PPointer(@LParamValue[low(LParamValue)+3])^;
    Result := ResolveTag(
                LPrecompiledTag.TagName, // const ATagName: AnsiString;
                LPrecompiledTag.TagParams, // const ATagParams: TALTagParamsA;
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
  var LPath := ALSplitA(LParamValue, ['.']);
  AOwned := False;
  Result := AContext.GetChildNode(LPath);

end;

end.













I’ll compare the implementation against official Handlebars behavior, then separate semantic gaps from concrete Delphi bugs.

The core design is sound, but I found several behavior differences that would produce incorrect output—not merely optional Handlebars features.

Thought for 2m 4s

Yes. The implementation is already a good Handlebars-like engine, but several important behaviors are either incorrect or still missing. The most important issues are below.

1. each does not execute {{else}} for an empty collection

Currently, any object or array enters the main branch, even when ChildNodes.Count = 0:

if (LJSonArray <> nil) and
   (LJSonArray.NodeType in [TALJSONNodeType.ntObject,
                            TALJSONNodeType.ntArray]) then begin

Consequently:

{{#each items}}
  item
{{else}}
  empty
{{/each}}

with:

{ "items": [] }

returns nothing instead of empty.

Official Handlebars invokes the inverse block when no iteration occurred.

Use:

if (LJSonArray <> nil) and
   (LJSonArray.NodeType in [
      TALJSONNodeType.ntObject,
      TALJSONNodeType.ntArray]) and
   (LJSonArray.ChildNodes.Count > 0) then begin
2. with must test emptiness, not only nil

Your implementation does:

if LJSonNode <> nil then begin

That incorrectly renders the main block for:

JSON null
false
""
[]

Official with executes its inverse block for empty values, but notably considers numeric 0 and an empty object {} non-empty.

You need a separate IsEmpty function because it is not exactly the inverse of your IsTruthy:

function TALHandlebars.IsEmpty(
  const AJsonNode: TALJsonNodeA): Boolean;
begin
  if AJsonNode = nil then
    Exit(True);

  case AJsonNode.NodeSubType of
    TALJSONNodeSubType.nstNull:
      Result := True;

    TALJSONNodeSubType.nstBoolean:
      Result := not AJsonNode.Bool;

    TALJSONNodeSubType.nstText:
      Result := AJsonNode.Text = '';

    TALJSONNodeSubType.nstArray:
      Result := AJsonNode.ChildNodes.Count = 0;

    // Numbers, including zero, and objects are not empty.
    else
      Result := False;
  end;
end;

Then:

if not IsEmpty(LJsonNode) then begin
3. An empty object is truthy in Handlebars

Your IsTruthy returns False for an empty object:

TALJsonNodeSubType.nstObject: begin
  Result := AJsonNode.ChildNodes.Count > 0;
end;

Official Handlebars treats {} as truthy. Only empty arrays are treated as empty collections by the standard isEmpty implementation.

Change this to:

TALJsonNodeSubType.nstObject:
  Result := True;

Also, includeZero should normally affect only numeric zero. Applying it to DateTime, BSON timestamps, or other custom types is an Alcinoe extension rather than standard Handlebars behavior.

4. ADepths is populated but never used for path resolution

You correctly push contexts in each and with, but neither ResolveTag nor ResolveParamValue uses ADepths.

Therefore these currently cannot work:

{{../title}}
{{../../name}}
{{this.name}}
{{./name}}
{{.}}
{{@root.title}}

Handlebars explicitly supports parent-context paths, root access and current-context aliases.

I would extract a common resolver:

function ResolvePath(
  const APath: AnsiString;
  const AContext: TALJsonNodeA;
  const ADepths: TList<TALJsonNodeA>): TALJsonNodeA;

It should handle:

this and . → current context
this.name and ./name → current context property
../name → parent context
../../name → grandparent context
@root.name → ADepths[0]

Registering this as a helper works for {{this}}, but not for {{this.name}}. It would be better to remove the this helper and handle it in ResolvePath.

5. RenderSource and RenderFile do not behave equivalently

File templates are precompiled in the constructor:

LTemplateSrc := ALPrecompileTagsA(...);

But the public overload:

RenderSource(const ATemplateSource; const AContext)

passes the raw source directly to ALReplaceTagsA.

That means blocks and nested subexpressions in direct source rendering may not have the same behavior as templates loaded from files.

A clean solution is:

function TALHandlebars.PrecompileSource(
  const ATemplateSource: AnsiString;
  const ATagsContainer: TObjectList): AnsiString;
begin
  Result := ALPrecompileTagsA(
              ATemplateSource,
              '{{',
              '}}',
              ATagsContainer,
              [rfReplaceAll],
              False,
              '\',
              '=',
              '#',
              '/',
              'else',
              '(',
              ')',
              False);
end;

The public RenderSource can use a temporary tag container that remains alive until rendering completes.

6. Unquoted literal values are not parsed

Currently these are interpreted as paths:

{{helper true}}
{{helper false}}
{{helper null}}
{{helper 123}}
{{helper -12.5}}

For example, true becomes a lookup for a property named "true".

Handlebars supports literal boolean, number, string, null and undefined values.

Before resolving a value as a path, recognize:

true
false
null
undefined
integer
floating-point number

This also affects hash parameters:

{{helper enabled=true count=12}}

Additionally, this:

ALStrToBool(AParams.Values['includeZero'])

reads the raw parameter text. It does not evaluate expressions such as:

{{#if value includeZero=allowZero}}

Hash values should be passed through ResolveParamValue.

7. HTML escaping and triple braces are missing

Your output handler returns:

Result := LJsonNode.Text;

Standard Handlebars HTML-escapes {{value}}, while {{{value}}} emits raw output. It also supports safe-string helper results.

Without escaping, user-controlled values such as:

{
  "name": "<script>alert(1)</script>"
}

are inserted directly into HTML. This is an XSS risk.

Your parser or precompiled tag metadata therefore needs to indicate whether the tag was:

{{value}}       escaped
{{{value}}}     unescaped

You will probably also need a result flag such as IsSafeHtml for helpers that intentionally return HTML.

8. Missing-helper handling is incorrect

Given:

{{unknownHelper value}}

your implementation ignores value and attempts to resolve unknownHelper as a normal context path.

Normally:

{{unknownName}} may resolve as a path and become empty if missing.
{{unknownHelper value}} is a helper invocation and should report a missing helper.
A block with no helper may require blockHelperMissing semantics if full compatibility is desired.

At minimum:

if (ATagParams.Count > 0) and
   not FHelpers.TryGetValue(ATagName, LHelperHandler) then
  raise Exception.CreateFmt(
    'Helper "%s" is not registered',
    [ATagName]);

Be careful to distinguish compiler-added __main and __inverse parameters from actual template arguments.

9. Argument validation is missing

Official if, unless, and with require exactly one argument; each requires an iterator argument.

Your helpers currently silently accept:

{{#if}}{{/if}}
{{#if a b}}{{/if}}
{{#with}}{{/with}}
{{#each}}{{/each}}

Adding errors makes malformed templates much easier to diagnose:

raise Exception.Create(
  'The "if" helper requires exactly one argument');

You need to count only positional arguments, excluding hash parameters and the internal block parameters.

10. each data variables are missing

Standard each provides:

{{@index}}
{{@key}}
{{@first}}
{{@last}}
{{@../index}}

These require a separate data stack. They should not be placed in the JSON context itself.

Your context record could become:

TTagReplaceHandlerContext = record
  Json: TALJsonNodeA;
  Depths: TList<TALJsonNodeA>;
  DataFrames: TList<THandlebarsDataFrame>;
end;
11. Partials are not implemented

Although FTemplates contains templates, ResolveTag has no handling for:

{{> header}}
{{> user user}}
{{> user name="John"}}
{{> (lookup . partialName)}}

Handlebars partials support current/custom contexts, hash parameters, dynamic names and partial blocks.

A basic implementation should at least recognize > and render the named template with the current context.

12. Helper replacement currently fails

This:

FHelpers.Add(AName, AHandler);

raises an exception when trying to override a default helper.

Use:

FHelpers.AddOrSetValue(AName, AHandler);

That permits applications to replace if, each, or another registered helper.

An UnregisterHelper method would also be useful.

13. Raw pointers embedded inside AnsiString are fragile

This code:

LPrecompiledTag :=
  PPointer(@LParamValue[Low(LParamValue) + 3])^;

has several risks:

Potential unaligned pointer access, especially outside x86/x64.
A malformed or untrusted template could provide an arbitrary pointer.
The precompiled source cannot safely survive across processes or object lifetimes.
There is no validation that the pointer belongs to FTagsContainer.

At least use Move:

var LPointer: Pointer;
Move(
  LParamValue[Low(LParamValue) + 3],
  LPointer,
  SizeOf(LPointer));

var LPrecompiledTag := TALPrecompiledTagA(LPointer);

A safer design would embed an integer tag ID rather than a memory address.

14. Internal block parameters can collide with user parameters

Encoding block functions as:

__main
__inverse

inside the normal parameter list means a user can explicitly provide:

{{#helper value __main="something"}}

It is cleaner to pass block contents through a dedicated options record:

THandlebarsHelperOptions = record
  MainBlock: AnsiString;
  InverseBlock: AnsiString;
  Hash: TALTagParamsA;
  Depths: TList<TALJsonNodeA>;
end;

That structure would also provide room for:

root context
data variables
escaped/unescaped output state
helper name
block parameters
Recommended priority

Fix these first:

Empty each inverse.
Correct with emptiness.
Empty-object truthiness.
Parent/current/root path resolution.
Literal parsing.
HTML escaping and triple braces.
Make direct RenderSource precompile its input.

After those changes, the implementation will be a reliable Handlebars-compatible core rather than only a similar template syntax.




























Looking at this against standard Handlebars semantics, a few things are missing or broken. Sorting by how much they'll bite you:
Correctness gaps (these will produce wrong output today)
ADepths is built but never consumed. You push/pop the depth stack everywhere (each, with, root RenderSource), but neither ResolveTag nor ResolveParamValue ever reads it. Both resolve purely against AContext:
pascalvar LPath := ALSplitA(LParamValue, ['.']);
Result := AContext.GetChildNode(LPath);
So {{../foo}} (parent reference) and @root are non-functional — the infrastructure is there, the lookup logic isn't. ALSplitA on ../foo just yields ['', '', 'foo'] and GetChildNode fails. This is the biggest one because it looks supported but silently isn't.
{{#each}} over an empty collection skips the {{else}} block. Your guard is:
pascalif (LJSonArray <> nil) and (LJSonArray.NodeType in [ntObject, ntArray]) then begin
  // loops 0 times, returns empty text node
end
else begin
  // inverse block
end;
An empty array/object is still ntArray/ntObject, so you take the main branch, iterate zero times, and return '' instead of rendering the inverse. Standard Handlebars renders {{else}} on an empty list. You need a ChildNodes.Count = 0 check that falls through to the inverse branch.
{{#with}} only does a nil-check, not a falsy-check. if (LJSonNode <> nil) means {{#with}} on a null/empty/zero node still renders the main block. Handlebars renders the inverse when the value is falsy. If you want parity, route it through IsTruthy instead of <> nil.
Missing features
HTML escaping / triple-stache. RenderSource returns LJsonNode.Text raw — there's no {{value}} → entity-escaping, and {{{value}}} can't even parse given {{/}} delimiters. For server-side HTML this is an XSS footgun. If this engine is deliberately not for HTML (your AnsiString/UTF-8 server convention might mean it's general-purpose templating), then fine — but it's worth a decision rather than an omission, maybe an escaped-by-default helper or a flag.
Partials ({{> name}}). You already load every template into FTemplates, but there's no partial helper, so a template can't include another. That's most of the work done with the last mile missing — a > block handler that looks up FTemplates and renders into the current context would do it.
@index / @key / @first / @last in each. You add the child to depths but expose none of the loop metadata. For object iteration @key is especially missed since the child node's Name is right there.
eq isn't registered. Your own doc comments in &if/unless show {{#if (eq value1 value2)}}, but only if/unless/each/this/with get registered. Either that example is aspirational or you meant to ship eq/ne/lookup.
Smaller / precompiler-dependent

Comments ({{! }}, {{!-- --}}) — depends on what ALPrecompileTagsA does with a ! lead char; likely unhandled.
Standalone-line whitespace stripping. Block-only lines leave blank-line artifacts unless the precompiler trims them. Given your recent whitespace work in ALPrecompileTagsA, this is probably where it'd live.
Unregistered block helpers fail silently — ResolveTag falls through to context lookup, so {{#unknownBlock}}…{{/unknownBlock}} drops its body with no error. A helperMissing-style path would catch typos.
{{.}} and this.foo — this works as a helper, but this.foo becomes a ['this','foo'] path lookup rather than current-context resolution.
Block params (as |item idx|) and ~ whitespace control — advanced, probably out of scope.

If I had to pick three to fix first: the dead ADepths stack, the empty-each-else bug, and a decision on HTML escaping. Want me to sketch the ResolveParamValue/ResolveTag changes to make ../ and @root actually walk the depths list?
