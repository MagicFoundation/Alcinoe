unit Alcinoe.Handlebars;

interface

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
  private
    type
      // -------------------------
      // TTagReplaceHandlerContext
      TTagReplaceHandlerContext = record
        Json: TALJsonNodeA;
        Depths: TList<TALJsonNodeA>;
      end;
  public
    type
      // --------------------
      // TStringHelperHandler
      TStringHelperHandler = function(
        const AParams: TALTagParamsA;
        const AContext: TALJsonNodeA;
        const ADepths: TList<TALJsonNodeA>): AnsiString of object;
      // ---------------------
      // TBooleanHelperHandler
      TBooleanHelperHandler = function(
        const AParams: TALTagParamsA;
        const AContext: TALJsonNodeA;
        const ADepths: TList<TALJsonNodeA>): Boolean of object;
      // -------------------
      // TFloatHelperHandler
      TFloatHelperHandler = function(
        const AParams: TALTagParamsA;
        const AContext: TALJsonNodeA;
        const ADepths: TList<TALJsonNodeA>): Double of object;
      // -------------------
      // TInt32HelperHandler
      TInt32HelperHandler = function(
        const AParams: TALTagParamsA;
        const AContext: TALJsonNodeA;
        const ADepths: TList<TALJsonNodeA>): Int32 of object;
      // -------------------
      // TInt64HelperHandler
      TInt64HelperHandler = function(
        const AParams: TALTagParamsA;
        const AContext: TALJsonNodeA;
        const ADepths: TList<TALJsonNodeA>): Int64 of object;
      // ------------------
      // TJsonHelperHandler
      TJsonHelperHandler = function(
        const AParams: TALTagParamsA;
        const AContext: TALJsonNodeA;
        const ADepths: TList<TALJsonNodeA>): TALJsonNodeA of object;
  private
    FTemplates: TDictionary<AnsiString{name}, AnsiString{content}>;
    FStringHelpers: TDictionary<AnsiString{name}, TStringHelperHandler>;
    FBooleanHelpers: TDictionary<AnsiString{name}, TBooleanHelperHandler>;
    FFloatHelpers: TDictionary<AnsiString{name}, TFloatHelperHandler>;
    FInt32Helpers: TDictionary<AnsiString{name}, TInt32HelperHandler>;
    FInt64Helpers: TDictionary<AnsiString{name}, TInt64HelperHandler>;
    FJsonHelpers: TDictionary<AnsiString{name}, TJsonHelperHandler>;
    FTagsContainer: TObjectList;
  protected
    function ResolveContextValue(
               const AName: AnsiString;
               const AContext: TALJsonNodeA): AnsiString; virtual;
    function TagReplaceHandler(
               const ATagName: AnsiString;
               ATagParams: TALTagParamsA;
               AContext: pointer): AnsiString; virtual;
    function RenderSource(
               const ATemplateSource: AnsiString;
               const AContext: TTagReplaceHandlerContext): AnsiString; overload; virtual;
  protected
    {$REGION 'Default Helpers'}
    function &if(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>): AnsiString;
    function each(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>): AnsiString;
    function this(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>): AnsiString;
    {$ENDREGION}
  public
    constructor Create(const ATemplatesPath: String); virtual;
    destructor Destroy; override;
    procedure RegisterStringHelper(
                const AName: AnsiString;
                const AHandler: TStringHelperHandler); virtual;
    procedure RegisterBooleanHelper(
                const AName: AnsiString;
                const AHandler: TBooleanHelperHandler); virtual;
    procedure RegisterFloatHelper(
                const AName: AnsiString;
                const AHandler: TFloatHelperHandler); virtual;
    procedure RegisterInt32Helper(
                const AName: AnsiString;
                const AHandler: TInt32HelperHandler); virtual;
    procedure RegisterInt64Helper(
                const AName: AnsiString;
                const AHandler: TInt64HelperHandler); virtual;
    procedure RegisterJsonHelper(
                const AName: AnsiString;
                const AHandler: TJsonHelperHandler); virtual;
    function RenderSource(
               const ATemplateSource: AnsiString;
               const AContext: TALJsonNodeA): AnsiString; overload; virtual;
    function RenderFile(
               const ATemplateFilename: AnsiString;
               const AContext: TALJsonNodeA): AnsiString; virtual;
    function ResolveParamValue(
               const AParams: TALTagParamsA;
               const AIndex: Integer;
               const AContext: TALJsonNodeA;
               const ADepths: TList<TALJsonNodeA>): AnsiString; virtual;
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
  FStringHelpers := TDictionary<AnsiString{name}, TStringHelperHandler>.Create;
  FBooleanHelpers := TDictionary<AnsiString{name}, TBooleanHelperHandler>.Create;
  FFloatHelpers := TDictionary<AnsiString{name}, TFloatHelperHandler>.Create;
  FInt32Helpers := TDictionary<AnsiString{name}, TInt32HelperHandler>.Create;
  FInt64Helpers := TDictionary<AnsiString{name}, TInt64HelperHandler>.Create;
  FJsonHelpers := TDictionary<AnsiString{name}, TJsonHelperHandler>.Create;
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

  RegisterStringHelper('if', &if);
  RegisterStringHelper('each', each);
  RegisterStringHelper('this', this);

end;

{*******************************}
destructor TALHandlebars.Destroy;
begin
  ALFreeAndNil(FTemplates);
  ALFreeAndNil(FStringHelpers);
  ALFreeAndNil(FBooleanHelpers);
  ALFreeAndNil(FFloatHelpers);
  ALFreeAndNil(FInt32Helpers);
  ALFreeAndNil(FInt64Helpers);
  ALFreeAndNil(FJsonHelpers);
  ALFreeAndNil(FTagsContainer);
  inherited;
end;

{*************************************************************************************************}
function TALHandlebars.&if(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>): AnsiString;
begin

  // {{#if "value"}}...(main)...{{else}}...(inverse)...{{/if}}
  // {{#if value includeZero=true}}...(main)...{{else}}...(inverse)...{{/if}}
  // {{#if (eq value1 value2)}}...(main)...{{else}}...(inverse)...{{/if}}

  if AParams.Count <= 0 then
    raise Exception.Create('The "if" helper requires at least one argument');

  var LParamIndex: Integer;
  var LParam0 := AParams[0];

  (*$REGION '{{#if "value"}}...(main)...{{else}}...(inverse)...{{/if}}'*)
  if AParams.WasQuoted[0] then begin
    if LParam0 <> '' then LParamIndex := AParams.IndexOfName('__main')
    else LParamIndex := AParams.IndexOfName('__inverse');
  end
  (*$ENDREGION*)

  (*$REGION '{{#if value includeZero=true}}...(main)...{{else}}...(inverse)...{{/if}}'*)
  else if (LParam0 <> '') and (LParam0[low(LParam0)] <> '{') then begin
    if AContext = nil then LParamIndex := AParams.IndexOfName('__inverse')
    else begin
      var LPath := ALSplitA(LParam0, ['.']);
      var LJsonNode := AContext.GetChildNode(LPath);
      if LJsonNode = nil then LParamIndex := AParams.IndexOfName('__inverse')
      else begin
        case LJsonNode.NodeSubType of
          TALJSONNodeSubType.nstFloat: begin
            if (not SameValue(LJsonNode.Float, 0)) or (ALStrToBool(AParams.Values['includeZero'])) then LParamIndex := AParams.IndexOfName('__main')
            else LParamIndex := AParams.IndexOfName('__inverse');
          end;
          TALJSONNodeSubType.nstText: begin
            if LJsonNode.text <> '' then LParamIndex := AParams.IndexOfName('__main')
            else LParamIndex := AParams.IndexOfName('__inverse');
          end;
          TALJSONNodeSubType.nstObject: begin
            if LJsonNode.ChildNodes.Count > 0 then LParamIndex := AParams.IndexOfName('__main')
            else LParamIndex := AParams.IndexOfName('__inverse');
          end;
          TALJSONNodeSubType.nstArray: begin
            if LJsonNode.ChildNodes.Count > 0 then LParamIndex := AParams.IndexOfName('__main')
            else LParamIndex := AParams.IndexOfName('__inverse');
          end;
          TALJSONNodeSubType.nstBinary: begin
            case LJsonNode.StorageKind of
              TALJSONStorageKind.skBytes: begin
                if length(LJsonNode.BinaryAsBytes) > 0 then LParamIndex := AParams.IndexOfName('__main')
                else LParamIndex := AParams.IndexOfName('__inverse');
              end;
              TALJSONStorageKind.skOwnedStream,
              TALJSONStorageKind.skBorrowedStream: begin
                if LJsonNode.BinaryAsStream.Size > 0 then LParamIndex := AParams.IndexOfName('__main')
                else LParamIndex := AParams.IndexOfName('__inverse');
              end
              else raise Exception.Create('Error 223F2DC0-F6F5-4093-8185-0544EF7F3388');
            end;
          end;
          TALJSONNodeSubType.nstObjectID: begin
            if length(LJsonNode.ObjectID) > 0 then LParamIndex := AParams.IndexOfName('__main')
            else LParamIndex := AParams.IndexOfName('__inverse');
          end;
          TALJSONNodeSubType.nstBoolean: begin
            if LJsonNode.Bool then LParamIndex := AParams.IndexOfName('__main')
            else LParamIndex := AParams.IndexOfName('__inverse');
          end;
          TALJSONNodeSubType.nstDateTime: begin
            if (Not SameDateTime(LJsonNode.DateTime, ALNullDate)) or (ALStrToBool(AParams.Values['includeZero'])) then LParamIndex := AParams.IndexOfName('__main')
            else LParamIndex := AParams.IndexOfName('__inverse');
          end;
          TALJSONNodeSubType.nstNull: begin
            LParamIndex := AParams.IndexOfName('__inverse');
          end;
          TALJSONNodeSubType.nstRegEx: begin
            if LJsonNode.RegEx <> '' then LParamIndex := AParams.IndexOfName('__main')
            else LParamIndex := AParams.IndexOfName('__inverse');
          end;
          TALJSONNodeSubType.nstJavascript: begin
            if LJsonNode.Javascript <> '' then LParamIndex := AParams.IndexOfName('__main')
            else LParamIndex := AParams.IndexOfName('__inverse');
          end;
          TALJSONNodeSubType.nstInt32: begin
            if (LJsonNode.Int32 <> 0) or (ALStrToBool(AParams.Values['includeZero'])) then LParamIndex := AParams.IndexOfName('__main')
            else LParamIndex := AParams.IndexOfName('__inverse');
          end;
          TALJSONNodeSubType.nstTimestamp: begin
            if (LJsonNode.Timestamp.I64 <> 0) or (ALStrToBool(AParams.Values['includeZero'])) then LParamIndex := AParams.IndexOfName('__main')
            else LParamIndex := AParams.IndexOfName('__inverse');
          end;
          TALJSONNodeSubType.nstInt64: begin
            if (LJsonNode.Int64 <> 0) or (ALStrToBool(AParams.Values['includeZero'])) then LParamIndex := AParams.IndexOfName('__main')
            else LParamIndex := AParams.IndexOfName('__inverse');
          end
          else raise Exception.Create('Error 13D6352A-AA12-43F0-9EB6-7E89E9CFEF3B');
        end;
      end;
    end;
  end
  (*$ENDREGION*)

  (*$REGION '{{#if (eq value1 value2)}}...(main)...{{else}}...(inverse)...{{/if}}'*)
  else begin
    var LPrecompiledTag: TALPrecompiledTagA := PPointer(@LParam0[low(LParam0) + 3{length('{{'#2)}])^;
    var LStringHelperHandler: TStringHelperHandler;
    If FStringHelpers.TryGetValue(LPrecompiledTag.TagName, LStringHelperHandler) then begin
      var LValue: AnsiString := LStringHelperHandler(
                                  LPrecompiledTag.TagParams, // const AParams: TALStringsA;
                                  AContext, // const AContext: TALJsonNodeA): AnsiString of object;
                                  ADepths); // const ADepths: TList<TALJsonNodeA>;
      if LValue <> '' then LParamIndex := AParams.IndexOfName('__main')
      else LParamIndex := AParams.IndexOfName('__inverse');
    end
    else begin
      var LBooleanHelperHandler: TBooleanHelperHandler;
      If FBooleanHelpers.TryGetValue(LPrecompiledTag.TagName, LBooleanHelperHandler) then begin
        var LValue: Boolean := LBooleanHelperHandler(
                                 LPrecompiledTag.TagParams, // const AParams: TALStringsA;
                                 AContext, // const AContext: TALJsonNodeA): AnsiString of object;
                                 ADepths); // const ADepths: TList<TALJsonNodeA>;
        if LValue then LParamIndex := AParams.IndexOfName('__main')
        else LParamIndex := AParams.IndexOfName('__inverse');
      end
      else begin
        var LFloatHelperHandler: TBooleanHelperHandler;
        If FBooleanHelpers.TryGetValue(LPrecompiledTag.TagName, LBooleanHelperHandler) then begin
          var LValue: Boolean := LBooleanHelperHandler(
                                   LPrecompiledTag.TagParams, // const AParams: TALStringsA;
                                   AContext, // const AContext: TALJsonNodeA): AnsiString of object;
                                   ADepths); // const ADepths: TList<TALJsonNodeA>;
          if LValue then LParamIndex := AParams.IndexOfName('__main')
          else LParamIndex := AParams.IndexOfName('__inverse');
        end
        else begin

        end;
      end;
    end;



    FFloatHelpers: TDictionary<AnsiString{name}, TFloatHelperHandler>;
    FInt32Helpers: TDictionary<AnsiString{name}, TInt32HelperHandler>;
    FInt64Helpers: TDictionary<AnsiString{name}, TInt64HelperHandler>;
    FJsonHelpers: TDictionary<AnsiString{name}, TJsonHelperHandler>;



    if ResolveParamValue(
         AParams, // const AParams: TALTagParamsA;
         0, // const AIndex: Integer;
         AContext, // const AContext: TALJsonNodeA;
         ADepths) <> '' then // const ADepths: TList<TALJsonNodeA>;
      LParamIndex := AParams.IndexOfName('__main')
    else
      LParamIndex := AParams.IndexOfName('__inverse');
  end;
  (*$ENDREGION*)

  Result := ResolveParamValue(
              AParams, // const AParams: TALTagParamsA;
              LParamIndex, // const AIndex: Integer;
              AContext, // const AContext: TALJsonNodeA;
              ADepths); // const ADepths: TList<TALJsonNodeA>;

end;

{**************************************************************************************************}
function TALHandlebars.each(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>): AnsiString;
begin

  //
  // {{#each "items"}} ... {{/each}}
  // {{#each items}} ... {{/each}}
  // {{#each (filter items "active")}} ... {{/each}}
  //

  if AParams.Count <= 0 then
    raise Exception.Create('The "if" helper requires at least one argument');

  var LJsonArray: TALJsonNodeA;
  var LParamIndex: Integer;
  var LParam0 := AParams[0];
  if (LParam0 = '') and (not AParams.WasQuoted[0]) then
    raise Exception.Create('Error DF7BC190-CFFC-406A-AA9F-F1D415657B7C');

  (*$REGION '{{#each "items"}} ... {{/each}}'*)
  if AParams.WasQuoted[0] then begin
    LJsonArray := nil;
    LParamIndex := AParams.IndexOfName('__inverse');
  end
  (*$ENDREGION*)

  (*$REGION '{{#each items}} ... {{/each}}'*)
  else if LParam0[low(LParam0)] <> '{' then begin
    if AContext = nil then begin
      LJsonArray := nil;
      LParamIndex := AParams.IndexOfName('__inverse');
    end
    else begin
      var LPath := ALSplitA(LParam0, ['.']);
      LJsonArray := AContext.GetChildNode(LPath);
      if (LJsonArray <> nil) and
         (LJsonArray.NodeType in [TALJSONNodeType.ntArray, TALJSONNodeType.ntObject]) and
         (LJsonArray.childnodes.count > 0) then begin
        LParamIndex := AParams.IndexOfName('__main')
      end
      else begin
        LJsonArray := nil;
        LParamIndex := AParams.IndexOfName('__inverse')
      end;
    end;
  end
  (*$ENDREGION*)

  (*$REGION '{{#each (filter items "active")}} ... {{/each}}'*)
  else begin
    (*
    if ResolveParamValue(
         AParams, // const AParams: TALTagParamsA;
         0, // const AIndex: Integer;
         AContext, // const AContext: TALJsonNodeA;
         APartials) <> '' then // const APartials: TALNameValueArrayA): AnsiString;
      LParamIndex := AParams.IndexOfName('__main')
    else
      LParamIndex := AParams.IndexOfName('__inverse');
    *)
  end;
  (*$ENDREGION*)

  if LJsonArray <> nil then begin
    Result := '';
    for var I := 0 to LJsonArray.ChildNodes.count - 1 do begin
      var LContext := LJsonArray.ChildNodes[I];
      ADepths.Add(LContext);
      Result := Result +
                ResolveParamValue(
                  AParams, // const AParams: TALTagParamsA;
                  LParamIndex, // const AIndex: Integer;
                  LContext, // const AContext: TALJsonNodeA;
                  ADepths); // const ADepths: TList<TALJsonNodeA>;
      ADepths.Delete(ADepths.Count-1);
    end;
  end
  else begin
    Result := ResolveParamValue(
                AParams, // const AParams: TALTagParamsA;
                LParamIndex, // const AIndex: Integer;
                AContext, // const AContext: TALJsonNodeA;
                ADepths); // const ADepths: TList<TALJsonNodeA>;
  end;

end;

{*****************************************}
function TALHandlebars.this(const AParams: TALTagParamsA; const AContext: TALJsonNodeA; const ADepths: TList<TALJsonNodeA>): AnsiString;
begin
  if AContext = nil then result := ''
  else begin
    case AContext.NodeType of
      TALJSONNodeType.ntObject: Result := AContext.JSON;
      TALJSONNodeType.ntArray: Result := AContext.JSON;
      TALJSONNodeType.ntText: Result := AContext.Text;
      else raise Exception.Create('Error E68415CB-31A8-4EE7-B6AF-A410B6DE5698');
    end;
  end;
end;

{*****************************************}
function TALHandlebars.ResolveContextValue(
           const AName: AnsiString;
           const AContext: TALJsonNodeA): AnsiString;
begin
  if AContext = nil then Exit('');
  var LPath := ALSplitA(AName, ['.']);
  Result := AContext.GetChildValueText(LPath, '');
end;

{***************************************}
function TALHandlebars.TagReplaceHandler(
           const ATagName: AnsiString;
           ATagParams: TALTagParamsA;
           AContext: pointer): AnsiString;
begin

  var LContext: TTagReplaceHandlerContext := TTagReplaceHandlerContext(AContext^);

  //////////////////
  // {{>partial}} //
  //////////////////

  if (ATagName <> '') and (ATagName[low(ATagName)] = '>') then begin
    var LTemplate := ALCopyStr(ATagName, 2, Maxint);
    If not FTemplates.TryGetValue(LTemplate, Result) then
      raise Exception.CreateFmt('Template "%s" not found', [LTemplate]);
    Result := RenderSource(
                Result, // const ATemplate: AnsiString;
                LContext); // const AContext: TTagReplaceHandlerContext
    Exit;
  end;

  //////////////////////////////
  // {{helper value1 value2}} //
  //////////////////////////////

  var LHelperHandler: TStringHelperHandler;
  If FStringHelpers.TryGetValue(ATagName, LHelperHandler) then begin
    Result := LHelperHandler(
                ATagParams, // const AParams: TALStringsA;
                LContext.Json, // const AContext: TALJsonNodeA): AnsiString of object;
                LContext.Depths); // const ADepths: TList<TALJsonNodeA>;
    Exit;
  end

  //////////////////
  // {{variable}} //
  //////////////////

  else
    result := ResolveContextValue(ATagName, LContext.Json);

end;

{*************************************}
procedure TALHandlebars.RegisterStringHelper(
            const AName: AnsiString;
            const AHandler: TStringHelperHandler);
begin
  FStringHelpers.Add(ALLowerCase(AName), AHandler);
end;

{**********************************}
procedure TALHandlebars.RegisterBooleanHelper(
            const AName: AnsiString;
            const AHandler: TBooleanHelperHandler);
begin
  FBooleanHelpers.Add(ALLowerCase(AName), AHandler);
end;

{**********************************}
procedure TALHandlebars.RegisterFloatHelper(
            const AName: AnsiString;
            const AHandler: TFloatHelperHandler);
begin
  FFloatHelpers.Add(ALLowerCase(AName), AHandler);
end;

{**********************************}
procedure TALHandlebars.RegisterInt32Helper(
            const AName: AnsiString;
            const AHandler: TInt32HelperHandler);
begin
  FInt32Helpers.Add(ALLowerCase(AName), AHandler);
end;

{**********************************}
procedure TALHandlebars.RegisterInt64Helper(
            const AName: AnsiString;
            const AHandler: TInt64HelperHandler);
begin
  FInt64Helpers.Add(ALLowerCase(AName), AHandler);
end;

{**********************************}
procedure TALHandlebars.RegisterJsonHelper(
            const AName: AnsiString;
            const AHandler: TJsonHelperHandler);
begin
  FJsonHelpers.Add(ALLowerCase(AName), AHandler);
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

{**************************************}
function TALHandlebars.ResolveParamValue(
           const AParams: TALTagParamsA;
           const AIndex: Integer;
           const AContext: TALJsonNodeA;
           const ADepths: TList<TALJsonNodeA>): AnsiString;
begin
  if (AIndex < 0) or (AIndex >= AParams.Count) then Exit('');
  var LItemHasNameValue := AParams.ItemHasNameValue(AIndex);
  if LItemHasNameValue then Result := AParams.ValueFromIndex[AIndex]
  else Result := AParams[AIndex];
  if not AParams.WasQuoted[AIndex] then begin
    var LName: AnsiString;
    if LItemHasNameValue then LName := AParams.Names[AIndex]
    else LName := '';
    if (Result <> '') and
       (Result[low(Result)] <> '{') and
       (LName <> '__main') and
       (LName <> '__inverse') then begin
      // {{upper name}}
      Result := ResolveContextValue(Result, AContext)
    end
    else begin
      // {{upper (lower name)}}
      var LContext: TTagReplaceHandlerContext;
      LContext.Json := AContext;
      LContext.Depths := ADepths;
      Result := RenderSource(
                  Result, // const ATemplateSource: AnsiString;
                  LContext); // const AContext: TALJsonNodeA;
    end;
  end;
end;

end.
