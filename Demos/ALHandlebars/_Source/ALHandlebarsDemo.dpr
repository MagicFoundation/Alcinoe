program ALHandlebarsDemo;

{$APPTYPE CONSOLE}

{$R *.res}

{$I Alcinoe.inc}

//
// This demo shows how to use TALHandlebars (Alcinoe.Handlebars) together with a
// kernel-mode http.sys server (TALHttpSysServer) to render HTML pages on the
// server side from a JSON context.
//
// The Handlebars templates live in the "Templates" sub directory of the demo
// (Demos\ALHandlebars\Templates). TALHandlebars loads every file found there,
// using the file name (relative to the Templates directory) as the template
// name. So the files are:
//
//   Templates\index    -> rendered with Handlebars.Render('index', ...)
//   Templates\header   -> partial,  included with {{> header}}
//   Templates\footer   -> partial,  included with {{> footer}}
//
// For every request to "/" the demo builds a JSON context and renders "index".
// That template exercises most of the engine features:
//   - value resolution and nested paths      {{title}} / {{user.address.city}}
//   - the {{#with}} block                     {{#with user}} ... {{/with}}
//   - the {{#if}} / {{else}} block            {{#if admin}} ... {{else}} ... {{/if}}
//   - the {{#each}} / {{else}} block          {{#each roles}} ... {{else}} ... {{/each}}
//   - data variables                          {{@index}} / {{@first}} / {{@last}}
//   - subexpressions                          {{#if (gt stock 0)}} / {{#if (eq ...)}}
//   - the built-in "concat" helper            {{concat user.firstname " " user.lastname}}
//   - a user registered custom helper         {{shout user.firstname}}
//   - HTML escaping vs raw output             {{bio}} (escaped) / {{{bio}}} (raw)
//

uses
  {$IFDEF DEBUG}
  FastMM5,
  {$ENDIF}
  System.SysUtils,
  System.IOUtils,
  System.AnsiStrings,
  System.Generics.Collections,
  Alcinoe.HTTP,
  Alcinoe.HTTP.Server,
  Alcinoe.HTTP.Server.HttpSys,
  Alcinoe.StringUtils,
  Alcinoe.FileUtils,
  Alcinoe.JSONDoc,
  Alcinoe.Handlebars,
  Alcinoe.Common;

var

  {*******************************}
  MainHttpServer: TALHttpSysServer;

  {************************}
  Handlebars: TALHandlebars;

Type

  {****************************************************************************}
  // Holds the custom helpers registered on the TALHandlebars instance. A helper
  // is a method (THelperHandler is declared "of object"), so it lives on an
  // object that also keeps a reference to the engine in order to resolve its
  // parameter values through Handlebars.ResolveParamValue.
  THandlebarsDemoHelpers = class
  public
    class function Shout(
                     const AParams: TALTagParamsA;
                     const AContext: TALJsonNodeA;
                     const ADepths: TList<TALJsonNodeA>;
                     var AOwned: Boolean): TALJsonNodeA;
  end;

  {*************************}
  THttpRequestHandler = class
  public
    class procedure OnRequest(Const ARequest: TALHttpServerRequestA; Const AResponse: TALHttpServerResponseA);
  end;

{**************************************************************************}
// {{shout value}} -> resolves "value" in the current context and returns it
// uppercased. This is a minimal example of a custom helper.
class function THandlebarsDemoHelpers.Shout(
                 const AParams: TALTagParamsA;
                 const AContext: TALJsonNodeA;
                 const ADepths: TList<TALJsonNodeA>;
                 var AOwned: Boolean): TALJsonNodeA;
begin
  if AParams.Count < 1 then
    raise Exception.Create('The "shout" helper requires one positional argument');
  var LOwned: Boolean;
  var LJsonNode := Handlebars.ResolveParamValue(
                     AParams, // const AParams: TALTagParamsA;
                     0, // const AIndex: Integer;
                     AContext, // const AContext: TALJsonNodeA;
                     ADepths, // const ADepths: TList<TALJsonNodeA>;
                     LOwned); // out AOwned: Boolean): TALJsonNodeA;
  try
    AOwned := True;
    Result := TALJSONTextNodeA.Create('');
    if LJsonNode <> nil then Result.Text := AlUpperCase(LJsonNode.Text)
    else Result.Text := '';
  finally
    if LOwned then
      ALFreeAndNil(LJsonNode);
  end;
end;

{****************************************************************************************************************************}
class procedure THttpRequestHandler.OnRequest(const ARequest: TALHttpServerRequestA; const AResponse: TALHttpServerResponseA);
begin

  {$REGION '/'}
  If ALSameTextA(ARequest.CookedUrl.Path, '/') or
     ALSameTextA(ARequest.CookedUrl.Path, '/index') then begin
    var LContext := TALJSONDocumentA.CreateFromJSONString(
                      '''
                      {
                        "title": "Alcinoe Handlebars Demo",
                        "tagline": "Native Delphi Handlebars templating served over http.sys",
                        "year": 2026,
                        "user": {
                          "firstname": "John",
                          "lastname": "Doe",
                          "admin": true,
                          "age": 32,
                          "bio": "<i>Delphi</i> developer & Alcinoe contributor",
                          "address": { "city": "Brussels", "country": "Belgium" },
                          "roles": ["Administrator", "Developer", "Tester"]
                        },
                        "products": [
                          { "name": "Keyboard", "price": 49,  "stock": 12 },
                          { "name": "Mouse",    "price": 25,  "stock": 0  },
                          { "name": "Monitor",  "price": 199, "stock": 5  }
                        ]
                      }
                      ''');
    try
      AResponse.BodyString := Handlebars.Render('index', LContext);
      AResponse.Headers.ContentType := 'text/html; charset=utf-8';
    finally
      ALFreeAndNil(LContext);
    end;
  end
  {$ENDREGION}

  {$REGION '404'}
  else
    Aresponse.StatusCode := 404;
  {$ENDREGION}

end;

begin

  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

  try

    // The Templates folder is located at Demos\ALHandlebars\Templates, i.e. two
    // levels above the executable directory (Demos\ALHandlebars\{Platform}\{Config}).
    var LTemplatesPath := TPath.GetFullPath(ALGetModulePathW + '..\..\Templates');

    Handlebars := TALHandlebars.Create(LTemplatesPath);
    try

      Handlebars.RegisterHelper('shout', THandlebarsDemoHelpers.Shout);

      MainHttpServer := TALHttpSysServer.Create;
      try
        MainHttpServer.OnRequest := THttpRequestHandler.OnRequest;
        MainHttpServer.UrlPrefixes.Add('http://+:23456/');

        Writeln('===============================================================');
        Writeln('  ALHandlebarsDemo');
        Writeln('  Engine   : http.sys (kernel-mode)  |  IOCP');
        Writeln('  Renderer : TALHandlebars (Alcinoe.Handlebars)');
        Writeln('  Templates: ' + LTemplatesPath);
        Writeln('  Open http://localhost:23456/ in a browser to see the rendered page.');
        Writeln('  Press Ctrl+C or hit <Enter> to stop');
        Writeln('===============================================================');

        MainHttpServer.Start;
        readln;
        MainHttpServer.Stop;
      finally
        ALFreeAndNil(MainHttpServer);
      end;

    finally
      ALFreeAndNil(Handlebars);
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.