unit Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  Alcinoe.Common,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.NativeControl,
  Alcinoe.FMX.WebBrowser;

type

  TMainForm = class(TForm)
    HeaderLayout: TALLayout;
    RunJSButton: TALButton;
    StatusText: TALText;
    WebBrowser: TALWebBrowser;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormSafeAreaChanged(Sender: TObject; const AInsets: TRectF);
    procedure RunJSButtonClick(Sender: TObject);
    procedure WebBrowserShouldStartLoadUrl(ASender: TObject; const AURL: string; var AAllow: Boolean);
    procedure WebBrowserWebMessage(Sender: TObject; const AMessage: string);
  private
    FLog: TArray<String>;
    procedure Log(const AText: String);
    procedure SendToPage(const AType: String; const AText: String; const AID: Integer = 0);
    procedure HandlePageMessage(const AMessage: String; const AChannel: String);
  end;

var
  MainForm: TMainForm;

implementation

uses
  Alcinoe.FMX.Styles,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.ErrorReporting,
  Alcinoe.JSONDoc,
  Alcinoe.Url;

{$R *.fmx}

const
  // Only these hosts may be loaded.
  CAllowedHosts: array[0..2] of String = ('alcinoe.demo', 'example.com', 'www.example.com');

  // The demo page. alcinoe.post() sends a JSON message to Delphi:
  //  * Windows (WebView2): window.chrome.webview.postMessage(string) -> OnWebMessage
  //  * Android: window.alcinoeNativeBridge.postMessage(string) -> OnWebMessage
  //  * iOS/macOS (WKWebView): window.webkit.messageHandlers.alcinoeNativeBridge.postMessage(string) -> OnWebMessage
  //  * Otherwise: a navigation to alcinoe://message?data=... that
  //    OnShouldStartLoadUrl intercepts and cancels. This works on every
  //    platform, even without a native message bridge.
  // In all cases Delphi receives the message as JSON text.
  // Delphi talks to the page with EvaluateJavaScript.
  CPage = '''
    <!DOCTYPE html>
    <html>
    <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>WebBrowser demo</title>
    <style>
      :root { color-scheme: dark; }
      body { margin: 0; padding: 16px; background: #141218; color: #E6E0E9;
             font: 15px/1.45 -apple-system, "Segoe UI", Roboto, sans-serif; }
      h2 { font-size: 17px; margin: 20px 0 6px; }
      code { background: #2B2930; padding: 1px 5px; border-radius: 6px; }
      .card { background: #211F26; border-radius: 16px; padding: 12px 16px; }
      .big { font-size: 40px; font-weight: 700; }
      button { font: inherit; border: 0; border-radius: 20px; padding: 8px 16px;
               margin: 8px 6px 0 0; background: #D0BCFF; color: #381E72; }
      a { color: #A8C7FA; }
      li { margin-bottom: 8px; }
      #log { font: 12px/1.5 ui-monospace, Menlo, Consolas, monospace; color: #CAC4D0;
             white-space: pre-wrap; }
    </style>
    <script>
      function log(text) {
        var el = document.getElementById('log');
        var lines = el.textContent ? el.textContent.split('\n') : [];
        lines.push(text);
        el.textContent = lines.slice(-6).join('\n');
      }
      var queue = [];
      function flush() {
        // One URL at a time: a second navigation could cancel the first one.
        if (queue.length === 0) return;
        window.location.href = 'alcinoe://message?data=' + encodeURIComponent(queue[0]);
        setTimeout(function () { queue.shift(); flush(); }, 80);
      }
      window.alcinoe = {
        post: function (msg) {
          if (window.chrome && window.chrome.webview) window.chrome.webview.postMessage(JSON.stringify(msg));
          else if (window.alcinoeNativeBridge) window.alcinoeNativeBridge.postMessage(JSON.stringify(msg));
          else if (window.webkit && window.webkit.messageHandlers && window.webkit.messageHandlers.alcinoeNativeBridge) window.webkit.messageHandlers.alcinoeNativeBridge.postMessage(JSON.stringify(msg));
          else { queue.push(JSON.stringify(msg)); if (queue.length === 1) flush(); }
        },
        // Called by Delphi through EvaluateJavaScript.
        receive: function (msg) {
          if (msg.type === 'pong') log('Pong: ' + (performance.now() - pingStart).toFixed(1) + ' ms');
        }
      };
      var counter = 0;
      // Called by Delphi through EvaluateJavaScript. The return value is
      // passed back to the Delphi callback.
      function bump() {
        counter++;
        document.getElementById('counter').textContent = counter;
        return counter;
      }
      var pingStart = 0;
      function ping() {
        pingStart = performance.now();
        alcinoe.post({type: 'ping', id: 1});
      }
    </script>
    </head>
    <body>
      <div class="card">
        <div class="big" id="counter">0</div>
      </div>

      <h2>Web messages</h2>
      <div>These buttons post JSON to Delphi. Delphi answers with <code>alcinoe.receive()</code>.</div>
      <button onclick="alcinoe.post({type: 'text', text: 'Hello Delphi'})">Say hello</button>
      <button onclick="ping()">Ping</button>

      <h2>ShouldStartLoadUrl</h2>
      <div>Delphi allows or blocks every link.</div>
      <ul>
        <li><a href="https://example.com/">example.com</a> (allowed)</li>
        <li><a href="https://en.wikipedia.org/wiki/WebView">wikipedia.org</a> (blocked: not in the allow-list)</li>
        <li><a href="alcinoe://toast?text=Hello%20from%20a%20link">alcinoe://toast</a> (handled by Delphi, never loaded)</li>
        <li><a href="tel:+33100000000">tel:</a> (cancelled)</li>
      </ul>

      <div id="log"></div>
      <script>alcinoe.post({type: 'ready'});</script>
    </body>
    </html>
    ''';

{***********************************************************************}
function UrlQueryParam(const AURL: String; const AName: String): String;
begin
  Result := '';
  var P := Pos('?', AURL);
  if P = 0 then Exit;
  for var LPair in Copy(AURL, P + 1, MaxInt).Split(['&']) do
    if LPair.StartsWith(AName + '=') then
      Exit(ALUrlDecode(Copy(LPair, Length(AName) + 2, MaxInt), False));
end;

{*********************************************************************}
// Return the host of a URL like scheme://host:port/path?query
function UrlHost(const AURL: String): String;
begin
  Result := '';
  var P := Pos('://', AURL);
  if P = 0 then Exit;
  Result := Copy(AURL, P + 3, MaxInt);
  for var LDelimiter in '/?#:' do begin
    P := Pos(LDelimiter, Result);
    if P > 0 then Result := Copy(Result, 1, P - 1);
  end;
  Result := LowerCase(Result);
end;

{**********************************************}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  TALErrorReporter.Instance;

  TALStyleManager.Instance.DarkModeBehavior := TALStyleManager.TDarkModeBehavior.AlwaysDark;
  TALStyleManager.Instance.ApplyColorScheme(Self, 'Material3.Color.Surface'{AFormFillColorKey}, 'Material3.Color.Surface'{ASystemStatusBarBackgroundColorKey});
  ALSetSystemBarsColor(
    TALStyleManager.Instance.GetColor('Material3.Color.Surface'), // const AStatusBarColor: TAlphaColor
    TALStyleManager.Instance.GetColor('Material3.Color.Surface'), // const ANavigationBarColor: TAlphaColor
    TALStyleManager.Instance.IsDarkMode, // const AStatusBarUseLightIcons: TAlphaColor
    TALStyleManager.Instance.IsDarkMode); // const ANavigationBarUseLightIcons: TAlphaColor

  WebBrowser.LoadData(
    'https://alcinoe.demo/', // const ABaseUrl: String;
    CPage, // const AData: string;
    'text/html', // const AMimeType: String = '';
    'utf-8'); // const AEncoding: String = '';
end;

{***********************************************}
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Drop the queued events that still reference this form.
  WebBrowser.OnShouldStartLoadUrl := nil;
  WebBrowser.OnWebMessage := nil;
end;

{******************************************************************************}
procedure TMainForm.FormSafeAreaChanged(Sender: TObject; const AInsets: TRectF);
begin
  Padding.Top := AInsets.Top;
  Padding.Bottom := AInsets.Bottom;
end;


{*******************************************}
procedure TMainForm.Log(const AText: String);
begin
  ALLog('WebBrowserDemo', AText);
  FLog := FLog + [AText];
  if Length(FLog) > 3 then FLog := Copy(FLog, Length(FLog) - 3, 3);
  StatusText.Text := String.Join('<br>', FLog);
end;

{******************************************************************************************}
procedure TMainForm.SendToPage(const AType: String; const AText: String; const AID: Integer);
begin
  // A JSON object is also a valid JavaScript literal.
  var LJSON := TALJsonDocumentW.Create;
  try
    LJSON.SetChildValueText('type', AType);
    LJSON.SetChildValueText('text', AText);
    LJSON.SetChildValueInt32('id', AID);
    WebBrowser.EvaluateJavaScript('alcinoe.receive(' + LJSON.SaveToJSONString([soStandardJson]) + ');');
  finally
    ALFreeAndNil(LJSON);
  end;
end;

{***********************************************************************************}
procedure TMainForm.HandlePageMessage(const AMessage: String; const AChannel: String);
begin
  var LJSON: TALJSONNodeW;
  try
    LJSON := TALJsonDocumentW.CreateFromJSONString(AMessage);
  except
    Log('Invalid message: ' + AMessage);
    Exit;
  end;
  try
    var LType := LJSON.GetChildValueText('type', '');
    if LType = 'ready' then
      Log('Page ready (messages via ' + AChannel + ')')
    else if LType = 'text' then begin
      Log('Page says: ' + LJSON.GetChildValueText('text', ''));
      SendToPage('notice', 'Hello page, Delphi got your message');
    end
    else if LType = 'ping' then begin
      Log('Ping received, sending pong');
      SendToPage('pong', '', LJSON.GetChildValueInt32('id', 0));
    end
    else
      Log('Unknown message: ' + AMessage);
  finally
    ALFreeAndNil(LJSON);
  end;
end;

{*************************************************************************************************************}
procedure TMainForm.WebBrowserShouldStartLoadUrl(ASender: TObject; const AURL: string; var AAllow: Boolean);
begin
  // This runs inside the native navigation callback: decide right away,
  // and queue the UI work to run after the callback has returned.
  var LURL := AURL;
  var LScheme := LowerCase(Copy(AURL, 1, Pos(':', AURL) - 1));

  if LScheme = 'alcinoe' then begin
    // Our own scheme: handled in Delphi, never loaded.
    AAllow := False;
    TThread.ForceQueue(nil,
      procedure
      begin
        if UrlHost(LURL) = 'message' then
          HandlePageMessage(UrlQueryParam(LURL, 'data'), 'url scheme')
        else begin
          var LText := UrlQueryParam(LURL, 'text');
          Log('Handled ' + Copy(LURL, 1, Pos('?', LURL) - 1) + ': ' + LText);
          SendToPage('notice', 'Delphi handled the link: ' + LText);
        end;
      end);
    Exit;
  end;

  // LoadData navigates to about:blank or a data: URL on some platforms.
  if (LScheme = 'about') or (LScheme = 'data') then begin
    AAllow := True;
    Exit;
  end;

  if (LScheme = 'http') or (LScheme = 'https') then begin
    AAllow := False;
    var LHost := UrlHost(AURL);
    for var LAllowedHost in CAllowedHosts do
      if LHost = LAllowedHost then AAllow := True;
  end
  else
    // tel:, mailto:... A real app would hand these to the OS.
    AAllow := False;

  var LAllow := AAllow;
  TThread.ForceQueue(nil,
    procedure
    begin
      if LAllow then Log('ALLOW ' + LURL)
      else Log('BLOCK ' + LURL);
    end);
end;

{*********************************************************************************}
procedure TMainForm.WebBrowserWebMessage(Sender: TObject; const AMessage: string);
begin
  HandlePageMessage(AMessage, 'postMessage');
end;

{****************************************************}
procedure TMainForm.RunJSButtonClick(Sender: TObject);
begin
  WebBrowser.EvaluateJavaScript(
    'bump()',
    procedure(const AResult: string)
    begin
      Log('Evaluate JavaScript returned: ' + AResult);
    end);
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
