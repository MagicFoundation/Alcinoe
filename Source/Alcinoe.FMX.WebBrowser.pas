unit Alcinoe.FMX.WebBrowser;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported131}
  {$MESSAGE WARN 'Check if FMX.WebBrowser.Delegate.iOS.pas was not updated and adjust the IFDEF'}
{$ENDIF}

uses
  system.Classes,
  {$IF defined(android)}
  Androidapi.JNI.Net,
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Webkit,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Alcinoe.AndroidApi.WebKit,
  Alcinoe.FMX.NativeView.Android,
  {$ELSEIF defined(IOS)}
  System.TypInfo,
  iOSapi.Foundation,
  iOSapi.UIKit,
  iOSapi.WebKit,
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  FMX.DialogService.Async,
  Alcinoe.FMX.NativeView.iOS,
  {$ELSEIF defined(ALMacOS)}
  System.TypInfo,
  Macapi.Foundation,
  Macapi.WebKit,
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  FMX.DialogService.Async,
  Alcinoe.FMX.NativeView.Mac,
  {$ELSEIF defined(MSWindows)}
  Winapi.WebView2,
  FMX.Controls.Win,
  Alcinoe.FMX.NativeView.Win,
  Alcinoe.GuardianThread,
  {$ENDIF}
  Fmx.controls,
  Alcinoe.FMX.NativeControl;

Type

  {********************}
  {$IF defined(android)}
  TALAndroidWebBrowserView = class;
  {$endif}
  {$IF defined(ios)}
  TALIosWebBrowserView = class;
  {$endif}
  {$IF defined(ALMacOS)}
  TALMacWebBrowserView = class;
  {$endif}
  {$IF defined(MSWINDOWS)}
  TALWinWebBrowserView = class;
  {$endif}

  {************************************************************************************}
  TALBaseWebBrowser = class(TALNativeControl, IControlTypeSupportable, IALNativeControl)
  public
    type
      // -------------------
      // TShouldStartLoadUrl
      TShouldStartLoadUrl = procedure(ASender: TObject; const AURL: string; var AAllow: Boolean) of object;
      // -------------------------
      // TJavaScriptResultCallback
      TJavaScriptResultCallback = reference to procedure(const AResult: string);
      // ----------------
      // TWebMessageEvent
      TWebMessageEvent = procedure(Sender: TObject; const AMessage: string) of object;
  private
    FOnWebMessage: TWebMessageEvent;
    FOnShouldStartLoadUrl: TShouldStartLoadUrl;
    {$IF defined(android)}
    function GetNativeView: TALAndroidWebBrowserView;
    {$ELSEIF defined(IOS)}
    function GetNativeView: TALIosWebBrowserView;
    {$ELSEIF defined(ALMacOS)}
    function GetNativeView: TALMacWebBrowserView;
    {$ELSEIF defined(MSWindows)}
    function GetNativeView: TALWinWebBrowserView;
    {$ENDIF}
  protected
    {$IF defined(android)}
    Function CreateNativeView: TALAndroidNativeView; override;
    {$ELSEIF defined(IOS)}
    Function CreateNativeView: TALIosNativeView; override;
    {$ELSEIF defined(ALMacOS)}
    Function CreateNativeView: TALMacNativeView; override;
    {$ELSEIF defined(MSWindows)}
    Function CreateNativeView: TALWinNativeView; override;
    {$ENDIF}
    property OnWebMessage: TWebMessageEvent read FOnWebMessage write FOnWebMessage;
    function ShouldStartLoadUrl(const AURL: string): Boolean; virtual;
    property OnShouldStartLoadUrl: TShouldStartLoadUrl read FOnShouldStartLoadUrl write FOnShouldStartLoadUrl;
    procedure DoWebMessage(const AMessage: string); virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    {$IF defined(android)}
    property NativeView: TALAndroidWebBrowserView read GetNativeView;
    {$ELSEIF defined(IOS)}
    property NativeView: TALIosWebBrowserView read GetNativeView;
    {$ELSEIF defined(ALMacOS)}
    property NativeView: TALMacWebBrowserView read GetNativeView;
    {$ELSEIF defined(MSWindows)}
    property NativeView: TALWinWebBrowserView read GetNativeView;
    {$ENDIF}
    /// <summary>
    ///   Loads raw HTML or other content into the web view.
    /// </summary>
    /// <param name="ABaseUrl">
    ///   The URL to use as the page's base URL. If empty defaults to 'about:blank'.
    /// </param>
    /// <param name="AData">
    ///   A String of data in the given encoding This value cannot be empty.
    /// </param>
    /// <param name="AMimeType">
    ///   The MIME type of the data, e.g. 'text/html'. This value may be empty.
    /// </param>
    /// <param name="AEncoding">
    ///   The encoding of the data, e.g., 'utf-8', 'base64'. This value may be empty.
    /// </param>
    /// <param name="AHistoryUrl">
    ///   The URL to use as the history entry. If empty defaults to 'about:blank'.
    ///   If non-empty, this must be a valid URL.
    /// </param>
    procedure LoadData(
                const ABaseUrl: String;
                const AData: string;
                const AMimeType: String = '';
                const AEncoding: String = '';
                const AHistoryUrl: String = ''); virtual;
    procedure LoadUrl(const AURL: string); virtual;
    procedure EvaluateJavaScript(const ACode: string); overload;
    procedure EvaluateJavaScript(const ACode: string; const AValueCallback: TJavaScriptResultCallback); overload;
    property CanFocus default True;
  end;

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALWebBrowser = class(TALBaseWebBrowser)
  published
    //property Action;
    property Align;
    property Anchors;
    //property AutoSize;
    property CanFocus;
    //property CanParentFocus;
    //property ClickSound;
    //property ClipChildren;
    //property ClipParent;
    //property Cursor;
    //property DisableFocusEffect;
    //property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    //property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    //property HitTest;
    //property Locked;
    property Margins;
    //property Opacity;
    //property Padding;
    //property PopupMenu;
    property Position;
    //property ReadOnly;
    //property RotationAngle;
    //property RotationCenter;
    //property Pivot;
    //property Scale;
    property Size;
    //property TabOrder;
    //property TabStop;
    //property TouchTargetExpansion;
    property Visible;
    property Width;
    //property OnCanFocus;
    //property OnChange;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    //property OnMouseEnter;
    //property OnMouseLeave;
    //property OnMouseDown;
    //property OnMouseUp;
    //property OnMouseMove;
    //property OnMouseWheel;
    //property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    //property OnPainting;
    //property OnPaint;
    //property OnResize;
    property OnResized;
    property OnShouldStartLoadUrl;
  end;

{$REGION ' ANDROID'}
{$IF defined(android)}

  {****************************************************}
  TALAndroidWebBrowserView = class(TALAndroidNativeView)
  private
    type
      // ----------------
      // TWebViewListener
      TWebViewListener = class(TJavaLocal, JALWebViewListener)
      private
        FWebBrowserView: TALAndroidWebBrowserView;
      public
        constructor Create(const AWebBrowserView: TALAndroidWebBrowserView);
        procedure doUpdateVisitedHistory(view: JWebView; url: JString; isReload: Boolean); cdecl;
        procedure onFormResubmission(view: JWebView; dontResend: JMessage; resend: JMessage); cdecl;
        procedure onLoadResource(view: JWebView; url: JString); cdecl;
        procedure onPageFinished(view: JWebView; url: JString); cdecl;
        procedure onPageStarted(view: JWebView; url: JString; favicon: JBitmap); cdecl;
        procedure onReceivedError(view: JWebView; errorCode: Integer; description: JString; failingUrl: JString); cdecl;
        procedure onReceivedHttpAuthRequest(view: JWebView; handler: JHttpAuthHandler; host: JString; realm: JString); cdecl;
        procedure onReceivedSslError(view: JWebView; handler: JSslErrorHandler; error: JSslError); cdecl;
        procedure onScaleChanged(view: JWebView; oldScale: Single; newScale: Single); cdecl;
        procedure onUnhandledKeyEvent(view: JWebView; event: JKeyEvent); cdecl;
        function shouldOverrideKeyEvent(view: JWebView; event: JKeyEvent): Boolean; cdecl;
        function shouldOverrideUrlLoading(view: JWebView; url: JString): Boolean; cdecl;
      end;
  private
    FWebViewListener: TWebViewListener;
    function GetView: JALWebView;
    function GetControl: TALBaseWebBrowser;
  protected
    function CreateView: JView; override;
    procedure DoWebMessage(const AMessage: string); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    property View: JALWebView read GetView;
    property Control: TALBaseWebBrowser read GetControl;
    procedure LoadData(
                const ABaseUrl: String;
                const AData: string;
                const AMimeType: String = '';
                const AEncoding: String = '';
                const AHistoryUrl: String = ''); virtual;
    procedure LoadUrl(const AURL: string); virtual;
    procedure EvaluateJavaScript(const ACode: string); overload;
    procedure EvaluateJavaScript(const ACode: string; const AValueCallback: TALBaseWebBrowser.TJavaScriptResultCallback); overload;
  end;

{$endif}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}

  {*****************************************}
  IALIosWebBrowserView = interface(WKWebView)
    ['{F6E522FD-2CB4-4718-870A-4E1A29540D39}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
  end;

  {********************************************}
  TALIosWebBrowserView = class(TALIosNativeView)
  private
    class var IsWKWebViewClassRegistered: Boolean;
  private
    type
      // -------------------
      // TNavigationDelegate
      TNavigationDelegate = class(TOCLocal, WKNavigationDelegate)
      private
        FWebBrowserView: TALIosWebBrowserView;
      public
        constructor Create(const AWebBrowserView: TALIosWebBrowserView);
        procedure webView(webView: WKWebView; didFailProvisionalNavigation: WKNavigation; withError: NSError); overload; cdecl;
        procedure webView(webView: WKWebView; navigationResponse: WKNavigationResponse; didBecomeDownload: WKDownload); overload; cdecl;
        procedure webView(webView: WKWebView; didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge; completionHandler: Pointer); overload; cdecl;
        procedure webView(webView: WKWebView; navigationAction: WKNavigationAction; didBecomeDownload: WKDownload); overload; cdecl;
        procedure webView(webView: WKWebView; shouldGoToBackForwardListItem: WKBackForwardListItem; willUseInstantBack: Boolean; completionHandler: Pointer); overload; cdecl;
        procedure webView(webView: WKWebView; decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer); overload; cdecl;
        procedure webView(webView: WKWebView; decidePolicyForNavigationAction: WKNavigationAction; preferences: WKWebpagePreferences; decisionHandler: Pointer); overload; cdecl;
        procedure webView(webView: WKWebView; decidePolicyForNavigationResponse: WKNavigationResponse; decisionHandler: Pointer); overload; cdecl;
        procedure webView(webView: WKWebView; didStartProvisionalNavigation: WKNavigation); overload; cdecl;
        [MethodName('webView:authenticationChallenge:shouldAllowDeprecatedTLS:')]
        procedure webViewAuthenticationChallenge(webView: WKWebView; authenticationChallenge: NSURLAuthenticationChallenge; shouldAllowDeprecatedTLS: Pointer); cdecl;
        [MethodName('webView:didCommitNavigation:')]
        procedure webViewDidCommitNavigation(webView: WKWebView; didCommitNavigation: WKNavigation); cdecl;
        [MethodName('webView:didFailNavigation:withError:')]
        procedure webViewDidFailNavigation(webView: WKWebView; didFailNavigation: WKNavigation; withError: NSError); cdecl;
        [MethodName('webView:didFinishNavigation:')]
        procedure webViewDidFinishNavigation(webView: WKWebView; didFinishNavigation: WKNavigation); cdecl;
        [MethodName('webView:didReceiveServerRedirectForProvisionalNavigation:')]
        procedure webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView; didReceiveServerRedirectForProvisionalNavigation: WKNavigation); cdecl;
        procedure webViewWebContentProcessDidTerminate(webView: WKWebView); cdecl;
      end;
  private
    FNavigationDelegate: TNavigationDelegate;
    function GetView: WKWebView;
    function GetControl: TALBaseWebBrowser;
  protected
    procedure InitView; override;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure DoWebMessage(const AMessage: string); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    property View: WKWebView read GetView;
    property Control: TALBaseWebBrowser read GetControl;
    procedure LoadData(
                const ABaseUrl: String;
                const AData: string;
                const AMimeType: String = '';
                const AEncoding: String = '';
                const AHistoryUrl: String = ''); virtual;
    procedure LoadUrl(const AURL: string); virtual;
    procedure EvaluateJavaScript(const ACode: string); overload;
    procedure EvaluateJavaScript(const ACode: string; const AValueCallback: TALBaseWebBrowser.TJavaScriptResultCallback); overload;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}

  {*****************************************}
  IALMacWebBrowserView = interface(WKWebView)
    ['{22B2CD52-0DF9-4645-8359-4DDAE64A5CA1}']
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
  end;

  {********************************************}
  TALMacWebBrowserView = class(TALMacNativeView)
  private
    class var IsWKWebViewClassRegistered: Boolean;
  private
    type
      // -------------------
      // TNavigationDelegate
      TNavigationDelegate = class(TOCLocal, WKNavigationDelegate)
      private
        FWebBrowserView: TALMacWebBrowserView;
      public
        constructor Create(const AWebBrowserView: TALMacWebBrowserView);
        [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
        procedure webViewDecidePolicyForNavigationActionDecisionHandler(webView: WKWebView; decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer); cdecl;
        [MethodName('webView:decidePolicyForNavigationResponse:decisionHandler:')]
        procedure webViewDecidePolicyForNavigationResponseDecisionHandler(webView: WKWebView; decidePolicyForNavigationResponse: WKNavigationResponse; decisionHandler: Pointer); cdecl;
        [MethodName('webView:didStartProvisionalNavigation:')]
        procedure webViewDidStartProvisionalNavigation(webView: WKWebView; didStartProvisionalNavigation: WKNavigation); cdecl;
        [MethodName('webView:didReceiveServerRedirectForProvisionalNavigation:')]
        procedure webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView; didReceiveServerRedirectForProvisionalNavigation: WKNavigation); cdecl;
        [MethodName('webView:didFailProvisionalNavigation:withError:')]
        procedure webViewDidFailProvisionalNavigationWithError(webView: WKWebView; didFailProvisionalNavigation: WKNavigation; withError: NSError); cdecl;
        [MethodName('webView:didCommitNavigation:')]
        procedure webViewDidCommitNavigation(webView: WKWebView; didCommitNavigation: WKNavigation); cdecl;
        [MethodName('webView:didFinishNavigation:')]
        procedure webViewDidFinishNavigation(webView: WKWebView; didFinishNavigation: WKNavigation); cdecl;
        [MethodName('webView:didFailNavigation:withError:')]
        procedure webViewDidFailNavigationWithError(webView: WKWebView; didFailNavigation: WKNavigation; withError: NSError); cdecl;
        [MethodName('webView:didReceiveAuthenticationChallenge:completionHandler:')]
        procedure webViewDidReceiveAuthenticationChallengeCompletionHandler(webView: WKWebView; didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge; completionHandler: Pointer); cdecl;
      end;
  private
    FNavigationDelegate: TNavigationDelegate;
    function GetView: WKWebView;
    function GetControl: TALBaseWebBrowser;
  protected
    procedure InitView; override;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure DoWebMessage(const AMessage: string); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    property View: WKWebView read GetView;
    property Control: TALBaseWebBrowser read GetControl;
    procedure LoadData(
                const ABaseUrl: String;
                const AData: string;
                const AMimeType: String = '';
                const AEncoding: String = '';
                const AHistoryUrl: String = ''); virtual;
    procedure LoadUrl(const AURL: string); virtual;
    procedure EvaluateJavaScript(const ACode: string); overload;
    procedure EvaluateJavaScript(const ACode: string; const AValueCallback: TALBaseWebBrowser.TJavaScriptResultCallback); overload;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

  {********************************************}
  TALWinWebBrowserView = class(TALWinNativeView)
  private
    type
      // --------------------
      // TEnvironmentCallback
      TEnvironmentCallback = reference to procedure(AResult: HRESULT; const AEnvironment: ICoreWebView2Environment);
      // -------------------
      // TControllerCallback
      TControllerCallback = reference to procedure(AResult: HRESULT; const AController: ICoreWebView2Controller);
      // ----------------------------------------------
      // TCreateCoreWebView2EnvironmentCompletedHandler
      TCreateCoreWebView2EnvironmentCompletedHandler = class(TInterfacedObject, ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler)
      private
        FProc: TEnvironmentCallback;
      public
        constructor Create(const AProc: TEnvironmentCallback);
        function Invoke(errorCode: HResult; const createdEnvironment: ICoreWebView2Environment): HResult; stdcall;
      end;
      // ---------------------------------------------
      // TCreateCoreWebView2ControllerCompletedHandler
      TCreateCoreWebView2ControllerCompletedHandler = class(TInterfacedObject, ICoreWebView2CreateCoreWebView2ControllerCompletedHandler)
      private
        FProc: TControllerCallback;
      public
        constructor Create(const AProc: TControllerCallback);
        function Invoke(errorCode: HResult; const createdController: ICoreWebView2Controller): HResult; stdcall;
      end;
      // -------------------------------
      // TNavigationStartingEventHandler
      TNavigationStartingEventHandler = class(TInterfacedObject, ICoreWebView2NavigationStartingEventHandler)
      private
        FWebBrowserView: TALWinWebBrowserView;
      public
        constructor Create(const AWebBrowserView: TALWinWebBrowserView);
        function Invoke(const sender: ICoreWebView2; const args: ICoreWebView2NavigationStartingEventArgs): HResult; stdcall;
      end;
      // ---------------------------------
      // TWebResourceRequestedEventHandler
      TWebResourceRequestedEventHandler = class(TInterfacedObject, ICoreWebView2WebResourceRequestedEventHandler)
      private
        FWebBrowserView: TALWinWebBrowserView;
      public
        constructor Create(const AWebBrowserView: TALWinWebBrowserView);
        function Invoke(const sender: ICoreWebView2; const args: ICoreWebView2WebResourceRequestedEventArgs): HResult; stdcall;
      end;
      // -------------------------------
      // TWebMessageReceivedEventHandler
      TWebMessageReceivedEventHandler = class(TInterfacedObject, ICoreWebView2WebMessageReceivedEventHandler)
      private
        FWebBrowserView: TALWinWebBrowserView;
      public
        constructor Create(const AWebBrowserView: TALWinWebBrowserView);
        function Invoke(const sender: ICoreWebView2; const args: ICoreWebView2WebMessageReceivedEventArgs): HRESULT; stdcall;
      end;
  private
    FWebView2Lifetime: TALRefCountObject;
    FPendingUrl: String;
    FPendingHtml: String;
    FReferrer: String;
    FWebView2Controller: ICoreWebView2Controller;
    FWebView2: ICoreWebView2;
    FNavigationStartingToken: EventRegistrationToken;
    FWebResourceRequestedToken: EventRegistrationToken;
    FWebMessageReceivedToken: EventRegistrationToken;
    function GetControl: TALBaseWebBrowser;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function IsWebView2Ready: Boolean;
    procedure DoWebMessage(const AMessage: string); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Control: TALBaseWebBrowser read GetControl;
    procedure LoadData(
                const ABaseUrl: String;
                const AData: string;
                const AMimeType: String = '';
                const AEncoding: String = '';
                const AHistoryUrl: String = ''); virtual;
    procedure LoadUrl(const AURL: string); virtual;
    procedure EvaluateJavaScript(const ACode: string); overload;
    procedure EvaluateJavaScript(const ACode: string; const AValueCallback: TALBaseWebBrowser.TJavaScriptResultCallback); overload;
  end;

{$endif}
{$ENDREGION}

procedure Register;

implementation

uses
  System.SysUtils,
  FMX.types,
  {$IF defined(android)}
  Androidapi.Helpers,
  Androidapi.JNI.App,
  {$ELSEIF defined(IOS)}
  System.UITypes,
  System.Net.URLClient,
  System.RTLConsts,
  Macapi.Helpers,
  iOSapi.CoreGraphics,
  FMX.Helpers.iOS,
  FMX.Consts,
  Alcinoe.StringUtils,
  {$ELSEIF defined(ALMacOS)}
  System.UITypes,
  System.Net.URLClient,
  System.RTLConsts,
  Macapi.Helpers,
  Macapi.CoreGraphics,
  FMX.Consts,
  Alcinoe.StringUtils,
  {$ELSEIF defined(MSWindows)}
  Winapi.Ole2,
  Winapi.EdgeUtils,
  Alcinoe.WinApi.Windows,
  {$endif}
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  Alcinoe.Common;

{***************************************************}
constructor TALBaseWebBrowser.Create(AOwner: TComponent);
begin
  inherited;
  CanFocus := True;
  FOnWebMessage := nil;
  FOnShouldStartLoadUrl := Nil;
end;

{****************************}
procedure TALBaseWebBrowser.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{********************}
{$IF defined(android)}
Function TALBaseWebBrowser.CreateNativeView: TALAndroidNativeView;
begin
  result := TALAndroidWebBrowserView.create(self);
end;
{$ENDIF}

{********************}
{$IF defined(android)}
function TALBaseWebBrowser.GetNativeView: TALAndroidWebBrowserView;
begin
  result := TALAndroidWebBrowserView(inherited NativeView);
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
Function TALBaseWebBrowser.CreateNativeView: TALIosNativeView;
begin

  // We must create a WKWebView instance explicitly here to ensure that the underlying
  // Objective-C class (WKWebView) is properly loaded and registered by the Delphi runtime.
  // Without this, calling `TALIosWebBrowserView(inherited GetNativeView)` will raise the following error:
  //   Unhandled Exception | Item not found
  //   At address: $0000000100365670
  //   (Generics.Collections.TDictionary<TTypeInfo*, TRegisteredDelphiClass*>.GetItem)
  //
  // Attempting to register the class manually like this:
  //   RegisterObjectiveCClass(TWKWebView, TypeInfo(WKWebView));
  // also fails with:
  //   Unhandled Exception | Method function allowsLinkPreview: Boolean of class TWKWebView not found
  //   At address: $0000000102A2011C
  //   (Macapi.Objectivec.TRegisteredDelphiClass.RegisterClass)
  //
  // Attempting to register our own wrapper class:
  //   RegisterObjectiveCClass(TALIosWebBrowserView, TypeInfo(IALIosWebBrowserView));
  // fails as well, with:
  //   Unhandled Exception | Objective-C class WKWebView could not be found
  //   At address: $00000001046CA014
  //   (Macapi.Objectivec.ObjectiveCClassNotFound)
  //
  // The only reliable workaround is to instantiate the WKWebView explicitly to force
  // the WebKit framework to be loaded and the class to be registered:

  if not TALIosWebBrowserView.IsWKWebViewClassRegistered then begin
    LoadFramework(libWebKit); // force load framework
    var LConfiguration := TWKWebViewConfiguration.Create;
    try
      var LWKWebView := TWKWebView.Wrap(TWKWebView.Alloc.initWithFrame(CGRectMake(0, 0, 0, 0), LConfiguration));
      LWKWebView.release;
    finally
      LConfiguration.release;
    end;
    TALIosWebBrowserView.IsWKWebViewClassRegistered := True;
  end;

  result := TALIosWebBrowserView.create(self);

end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALBaseWebBrowser.GetNativeView: TALIosWebBrowserView;
begin
  result := TALIosWebBrowserView(inherited NativeView);
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
Function TALBaseWebBrowser.CreateNativeView: TALMacNativeView;
begin

  // We must create a WKWebView instance explicitly here to ensure that the underlying
  // Objective-C class (WKWebView) is properly loaded and registered by the Delphi runtime.
  // Without this, calling `TALMacWebBrowserView(inherited GetNativeView)` will raise the following error:
  //   Unhandled Exception | Item not found
  //   At address: $0000000100365670
  //   (Generics.Collections.TDictionary<TTypeInfo*, TRegisteredDelphiClass*>.GetItem)
  //
  // Attempting to register the class manually like this:
  //   RegisterObjectiveCClass(TWKWebView, TypeInfo(WKWebView));
  // also fails with:
  //   Unhandled Exception | Method function allowsLinkPreview: Boolean of class TWKWebView not found
  //   At address: $0000000102A2011C
  //   (Macapi.Objectivec.TRegisteredDelphiClass.RegisterClass)
  //
  // Attempting to register our own wrapper class:
  //   RegisterObjectiveCClass(TALMacWebBrowserView, TypeInfo(IALMacWebBrowserView));
  // fails as well, with:
  //   Unhandled Exception | Objective-C class WKWebView could not be found
  //   At address: $00000001046CA014
  //   (Macapi.Objectivec.ObjectiveCClassNotFound)
  //
  // The only reliable workaround is to instantiate the WKWebView explicitly to force
  // the WebKit framework to be loaded and the class to be registered:

  if not TALMacWebBrowserView.IsWKWebViewClassRegistered then begin
    LoadFramework(libWebKit); // force load framework
    var LConfiguration := TWKWebViewConfiguration.Create;
    try
      var LWKWebView := TWKWebView.Wrap(TWKWebView.Alloc.initWithFrame(CGRectMake(0, 0, 0, 0), LConfiguration));
      LWKWebView.release;
    finally
      LConfiguration.release;
    end;
    TALMacWebBrowserView.IsWKWebViewClassRegistered := True;
  end;

  result := TALMacWebBrowserView.create(self);

end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
function TALBaseWebBrowser.GetNativeView: TALMacWebBrowserView;
begin
  result := TALMacWebBrowserView(inherited NativeView);
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
Function TALBaseWebBrowser.CreateNativeView: TALWinNativeView;
begin
  {$IF defined(ALDPK)}
  Result := nil;
  {$ELSE}
  result := TALWinWebBrowserView.create(self);
  {$ENDIF}
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
function TALBaseWebBrowser.GetNativeView: TALWinWebBrowserView;
begin
  result := TALWinWebBrowserView(inherited NativeView);
end;
{$ENDIF}

{***************************************************************}
procedure TALBaseWebBrowser.DoWebMessage(const AMessage: string);
begin
  if Assigned(FOnWebMessage) then
    FOnWebMessage(Self, AMessage);
end;

{*********************************************************************}
function TALBaseWebBrowser.ShouldStartLoadUrl(const AURL: string): Boolean;
begin
  if assigned(fOnShouldStartLoadUrl) then
    fOnShouldStartLoadUrl(Self, AURL, Result)
  else
    Result := True;
end;

{*******************************}
procedure TALBaseWebBrowser.LoadData(
            const ABaseUrl: String;
            const AData: string;
            const AMimeType: String = '';
            const AEncoding: String = '';
            const AHistoryUrl: String = '');
begin
  if NativeView <> nil then
    NativeView.LoadData(
      ABaseUrl, // const ABaseUrl: String;
      AData, // const AData: string;
      AMimeType, // const AMimeType: String = '';
      AEncoding, // const AEncoding: String = '';
      AHistoryUrl); // const AHistoryUrl: String = ''
end;

{**************************************************}
procedure TALBaseWebBrowser.LoadUrl(const AURL: string);
begin
  if NativeView <> nil then
    NativeView.LoadUrl(AURL);
end;

{**************************************************}
procedure TALBaseWebBrowser.EvaluateJavaScript(const ACode: string);
begin
  if FNativeView <> nil then
    NativeView.EvaluateJavaScript(ACode);
end;

{**************************************************}
procedure TALBaseWebBrowser.EvaluateJavaScript(const ACode: string; const AValueCallback: TJavaScriptResultCallback);
begin
  if FNativeView <> nil then
    NativeView.EvaluateJavaScript(ACode, AValueCallback);
end;

{$REGION ' ANDROID'}
{$IF defined(android)}

{************************************************************************************************************}
constructor TALAndroidWebBrowserView.TWebViewListener.Create(const AWebBrowserView: TALAndroidWebBrowserView);
begin
  inherited Create;
  FWebBrowserView := AWebBrowserView;
end;

{**************************************************************************************************************************}
procedure TALAndroidWebBrowserView.TWebViewListener.doUpdateVisitedHistory(view: JWebView; url: JString; isReload: Boolean);
begin
  // Nothing
end;

{*****************************************************************************************************************************}
procedure TALAndroidWebBrowserView.TWebViewListener.onFormResubmission(view: JWebView; dontResend: JMessage; resend: JMessage);
begin
  // Nothing
end;

{***********************************************************************************************}
procedure TALAndroidWebBrowserView.TWebViewListener.onLoadResource(view: JWebView; url: JString);
begin
  // Nothing
end;

{***********************************************************************************************}
procedure TALAndroidWebBrowserView.TWebViewListener.onPageFinished(view: JWebView; url: JString);
begin
  // Nothing
end;

{****************************************************************************************************************}
procedure TALAndroidWebBrowserView.TWebViewListener.onPageStarted(view: JWebView; url: JString; favicon: JBitmap);
begin
  // Nothing
end;

{*************************************************************************************************************************************************}
procedure TALAndroidWebBrowserView.TWebViewListener.onReceivedError(view: JWebView; errorCode: Integer; description: JString; failingUrl: JString);
begin
  // Nothing
end;

{******************************************************************************************************************************************************}
procedure TALAndroidWebBrowserView.TWebViewListener.onReceivedHttpAuthRequest(view: JWebView; handler: JHttpAuthHandler; host: JString; realm: JString);
begin
  // Nothing
end;

{**********************************************************************************************************************************}
procedure TALAndroidWebBrowserView.TWebViewListener.onReceivedSslError(view: JWebView; handler: JSslErrorHandler; error: JSslError);
begin
  // Nothing
end;

{*********************************************************************************************************************}
procedure TALAndroidWebBrowserView.TWebViewListener.onScaleChanged(view: JWebView; oldScale: Single; newScale: Single);
begin
  // Nothing
end;

{********************************************************************************************************}
procedure TALAndroidWebBrowserView.TWebViewListener.onUnhandledKeyEvent(view: JWebView; event: JKeyEvent);
begin
  // Nothing
end;

{*******************************************************************************************************************}
function TALAndroidWebBrowserView.TWebViewListener.shouldOverrideKeyEvent(view: JWebView; event: JKeyEvent): Boolean;
begin
  Result := False;
end;

{*****************************************************************************************************************}
function TALAndroidWebBrowserView.TWebViewListener.shouldOverrideUrlLoading(view: JWebView; url: JString): Boolean;
begin
  Result := not FWebBrowserView.Control.ShouldStartLoadUrl(JStringToString(url));
end;

{******************************************}
constructor TALAndroidWebBrowserView.Create;
begin
  inherited; // This will call CreateView
  FWebViewListener := TWebViewListener.Create(self);
  view.setBackgroundColor(TJColor.JavaClass.TRANSPARENT);
  view.setBackground(nil);
  view.setListener(FWebViewListener);
  view.getSettings.setJavaScriptEnabled(true);
  // This method was deprecated in API level 35.
  // WebSQL is deprecated and this method will become a no-op on all Android
  // versions once support is removed in Chromium.
  // See https://developer.chrome.com/blog/deprecating-web-sql for more information.
  view.getSettings.setDatabaseEnabled(True);
  view.getSettings.setDomStorageEnabled(True);
  view.getSettings.setBuiltInZoomControls(True);
  view.getSettings.setDisplayZoomControls(False);
  view.getSettings.setAllowFileAccess(True);
end;

{******************************************}
destructor TALAndroidWebBrowserView.Destroy;
begin
  View.setListener(nil);
  alfreeandNil(FWebViewListener);
  inherited;
end;

{**************************************************}
function TALAndroidWebBrowserView.CreateView: JView;
begin
  Result := TJALWebView.JavaClass.init(TAndroidHelper.Activity)
end;

{****************************************************}
function TALAndroidWebBrowserView.GetView: JALWebView;
begin
  Result := inherited GetView<JALWebView>;
end;

{**********************************************************}
function TALAndroidWebBrowserView.GetControl: TALBaseWebBrowser;
begin
  Result := TALBaseWebBrowser(inherited Control)
end;

{******************************************}
procedure TALAndroidWebBrowserView.LoadData(
            const ABaseUrl: String;
            const AData: string;
            const AMimeType: String = '';
            const AEncoding: String = '';
            const AHistoryUrl: String = '');
begin
  View.loadDataWithBaseURL(
    StringToJString(ABaseUrl), // baseUrl: JString;
    StringToJString(AData), // data: JString;
    StringToJString(AMimeType), // mimeType: JString;
    StringToJString(AEncoding), // encoding: JString;
    StringToJString(AHistoryUrl)); // historyUrl: JString
end;

{*************************************************************}
procedure TALAndroidWebBrowserView.LoadUrl(const AURL: string);
begin
  View.loadUrl(StringToJString(AUrl));
end;

{$endif}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}

{**************************************}
constructor TALIosWebBrowserView.Create;
begin
  inherited; // This will call InitView
  FNavigationDelegate := TNavigationDelegate.Create(Self);
  View.setNavigationDelegate(FNavigationDelegate.GetObjectID);
  View.setOpaque(False);
  View.setbackgroundColor(TUIColor.OCClass.clearColor);
end;

{**************************************}
destructor TALIosWebBrowserView.Destroy;
begin
  View.setNavigationDelegate(nil);
  ALFreeAndNil(FNavigationDelegate);
  inherited Destroy;
end;

{**************************************}
procedure TALIosWebBrowserView.InitView;
begin
  var LConfiguration := TWKWebViewConfiguration.Create;
  try
    LConfiguration.setAllowsInlineMediaPlayback(True);
    var V: Pointer := WKWebView(Super).initWithFrame(CGRectFromRect(Control.GetNativeViewAbsoluteRect), LConfiguration);
    if GetObjectID <> V then
      UpdateObjectID(V);
  finally
    LConfiguration.release;
  end;
end;

{**********************************************************}
function TALIosWebBrowserView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALIosWebBrowserView);
end;

{***********************************************}
function TALIosWebBrowserView.GetView: WKWebView;
begin
  Result := inherited GetView<WKWebView>;
end;

{******************************************************}
function TALIosWebBrowserView.GetControl: TALBaseWebBrowser;
begin
  Result := TALBaseWebBrowser(inherited Control);
end;

{*******************************************************************************************************}
constructor TALIosWebBrowserView.TNavigationDelegate.Create(const AWebBrowserView: TALIosWebBrowserView);
begin
  inherited Create;
  FWebBrowserView := AWebBrowserView;
end;

{*********************************************************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webView(webView: WKWebView; didFailProvisionalNavigation: WKNavigation; withError: NSError);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webView:didFailProvisionalNavigation:withError');
  {$ENDIF}
end;

{******************************************************************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webView(webView: WKWebView; navigationResponse: WKNavigationResponse; didBecomeDownload: WKDownload);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webView:navigationResponse:didBecomeDownload');
  {$ENDIF}
end;

{**************************************************************************************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webView(webView: WKWebView; didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge; completionHandler: Pointer);

type
  TAuthenticationResponseProc = reference to procedure(const ACredential: NSURLCredential);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IFNDEF ALCompilerVersionSupported131}
    {$MESSAGE WARN 'Check if FMX.WebBrowser.Delegate.Cocoa.TBaseWebViewDelegate.AuthenticateForHost was not updated and adjust the IFDEF'}
  {$ENDIF}
  procedure AuthenticateForHost(const AHost: NSString; const AResponseProc: TAuthenticationResponseProc);
  var
    LTitle: string;
    LPrompts, LValues: TArray<string>;
    LUserName, LPassword: NSString;
    LCredential: NSURLCredential;
  begin
    SetLength(LValues, 2);
    SetLength(LPrompts, 2);
    {$IF Defined(IOS)}
    LPrompts[0] := ''; // Workaround for https://quality.embarcadero.com/browse/RSP-27777
    {$ELSE}
    LPrompts[0] := SUsername;
    {$ENDIF IOS}
    LPrompts[1] := #1 + SPassword;
    LTitle := ALFormatW(SHostRequiresAuthentication, [NSStrToStr(AHost)]);
    TDialogServiceAsync.InputQuery(
      LTitle,
      LPrompts,
      LValues,
      procedure(const AResult: TModalResult; const AValues: array of string)
      begin
        if AResult = mrOK then begin
          LUserName := StrToNSStr(AValues[0]);
          LPassword := StrToNSStr(AValues[1]);
          LCredential := TNSURLCredential.OCClass.credentialWithUser(LUserName, LPassword, NSURLCredentialPersistenceNone);
          AResponseProc(LCredential)
        end
        else
          AResponseProc(nil);
      end);
  end;

var
  LCompletionHandlerBlock: procedure(disposition: NSURLSessionAuthChallengeDisposition; ignored: Pointer; credential: Pointer); cdecl;
  LAuthMethod: NSString;
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webView:didReceiveAuthenticationChallenge:completionHandler');
  {$ENDIF}
  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
  LAuthMethod := didReceiveAuthenticationChallenge.protectionSpace.authenticationMethod;
  if LAuthMethod.isEqualToString(NSURLAuthenticationMethodDefault) or LAuthMethod.isEqualToString(NSURLAuthenticationMethodHTTPBasic) or
    LAuthMethod.isEqualToString(NSURLAuthenticationMethodHTTPDigest) then begin
    AuthenticateForHost(
      webView.URL.host,
      procedure(const ACredential: NSURLCredential)
      begin
        LCompletionHandlerBlock(NSURLSessionAuthChallengeUseCredential, nil, NSObjectToID(ACredential));
        imp_removeBlock(@LCompletionHandlerBlock);
      end);
  end
  else if LAuthMethod.isEqualToString(NSURLAuthenticationMethodServerTrust) then begin
    LCompletionHandlerBlock(NSURLSessionAuthChallengePerformDefaultHandling, nil, nil);
    imp_removeBlock(@LCompletionHandlerBlock);
  end
  else begin
    LCompletionHandlerBlock(NSURLSessionAuthChallengeCancelAuthenticationChallenge, nil, nil);
    imp_removeBlock(@LCompletionHandlerBlock);
  end;
end;

{**************************************************************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webView(webView: WKWebView; navigationAction: WKNavigationAction; didBecomeDownload: WKDownload);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webView:navigationAction:didBecomeDownload');
  {$ENDIF}
end;

{********************************************************************************************************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webView(webView: WKWebView; shouldGoToBackForwardListItem: WKBackForwardListItem; willUseInstantBack: Boolean; completionHandler: Pointer);
var
  // completionHandler:(void (^)(BOOL shouldGoToItem))completionHandler
  LBlockImp: procedure(shouldGoToItem: Boolean); cdecl;
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webView:shouldGoToBackForwardListItem:willUseInstantBack:completionHandler');
  {$ENDIF}
  @LBlockImp := imp_implementationWithBlock(completionHandler);
  LBlockImp(true{shouldGoToItem});
  imp_removeBlock(@LBlockImp);
end;

{************************************************************************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webView(webView: WKWebView; decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer);
var
  LBlockImp: procedure(policy: WKNavigationActionPolicy); cdecl;
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webView:decidePolicyForNavigationAction:decisionHandler');
  {$ENDIF}
  var LShouldStartLoadUrl := FWebBrowserView.Control.ShouldStartLoadUrl(NSStrToStr(decidePolicyForNavigationAction.request.URL.absoluteString));
  @LBlockImp := imp_implementationWithBlock(decisionHandler);
  if LShouldStartLoadUrl then LBlockImp(WKNavigationActionPolicyAllow)
  else LBlockImp(WKNavigationActionPolicyCancel);
  imp_removeBlock(@LBlockImp);
end;

{***********************************************************************************************************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webView(webView: WKWebView; decidePolicyForNavigationAction: WKNavigationAction; preferences: WKWebpagePreferences; decisionHandler: Pointer);
var
  // No idea why ignored parameter is needed, but removing it makes everything crash.
  // https://stackoverflow.com/questions/79827070/how-to-correctly-call-an-ios-completionhandler-block-from-delphi-without-crashin?noredirect=1#comment140867316_79827070
  // (WK_SWIFT_UI_ACTOR void (^)(WKNavigationActionPolicy, WKWebpagePreferences *))decisionHandler
  LBlockImp: procedure(policy: WKNavigationActionPolicy; ignored: pointer; preferences: Pointer{WKWebpagePreferences}); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webView:decidePolicyForNavigationAction:preferences:decisionHandler');
  {$ENDIF}
  var LShouldStartLoadUrl := FWebBrowserView.Control.ShouldStartLoadUrl(NSStrToStr(decidePolicyForNavigationAction.request.URL.absoluteString));
  @LBlockImp := imp_implementationWithBlock(decisionHandler);
  if LShouldStartLoadUrl then LBlockImp(WKNavigationActionPolicyAllow, nil, NSObjectToID(preferences))
  else LBlockImp(WKNavigationActionPolicyCancel, nil, NSObjectToID(preferences));
  imp_removeBlock(@LBlockImp);
end;

{****************************************************************************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webView(webView: WKWebView; decidePolicyForNavigationResponse: WKNavigationResponse; decisionHandler: Pointer);
var
  LDecisionHandlerBlock: procedure(policy: WKNavigationResponsePolicy); cdecl;
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webView:decidePolicyForNavigationResponse:decisionHandler');
  {$ENDIF}
  @LDecisionHandlerBlock := imp_implementationWithBlock(decisionHandler);
  LDecisionHandlerBlock(WKNavigationResponsePolicyAllow);
  imp_removeBlock(@LDecisionHandlerBlock);
end;

{**************************************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webView(webView: WKWebView; didStartProvisionalNavigation: WKNavigation);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webView:didStartProvisionalNavigation');
  {$ENDIF}
end;

{**********************************************************************************************************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webViewAuthenticationChallenge(webView: WKWebView; authenticationChallenge: NSURLAuthenticationChallenge; shouldAllowDeprecatedTLS: Pointer);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewAuthenticationChallenge');
  {$ENDIF}
end;

{***********************************************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webViewDidCommitNavigation(webView: WKWebView; didCommitNavigation: WKNavigation);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewDidCommitNavigation');
  {$ENDIF}
end;

{***************************************************************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webViewDidFailNavigation(webView: WKWebView; didFailNavigation: WKNavigation; withError: NSError);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewDidFailNavigation');
  {$ENDIF}
end;

{***********************************************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webViewDidFinishNavigation(webView: WKWebView; didFinishNavigation: WKNavigation);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewDidFinishNavigation');
  {$ENDIF}
end;

{*********************************************************************************************************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView; didReceiveServerRedirectForProvisionalNavigation: WKNavigation);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewDidReceiveServerRedirectForProvisionalNavigation');
  {$ENDIF}
end;

{**********************************************************************************************************}
procedure TALIosWebBrowserView.TNavigationDelegate.webViewWebContentProcessDidTerminate(webView: WKWebView);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewWebContentProcessDidTerminate');
  {$ENDIF}
end;

{**************************************}
procedure TALIosWebBrowserView.LoadData(
            const ABaseUrl: String;
            const AData: string;
            const AMimeType: String = '';
            const AEncoding: String = '';
            const AHistoryUrl: String = '');
begin
  var LData := StrToNSStr(AData);
  var LBaseUrl := StrToNSStr(ABaseUrl);
  var LURL := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(LBaseUrl));
  View.loadHTMLString(LData, LURL);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported131}
  {$MESSAGE WARN 'Check if FMX.WebBrowser.Cocoa.TCommonWebBrowserService.DoNavigate.pas was not updated and adjust the IFDEF'}
{$ENDIF}
procedure TALIosWebBrowserView.LoadUrl(const AURL: string);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function TryGetFileNSUrl(const AURI: TURI; var AUrl: NSUrl): Boolean;
  begin
    var LDir := ExtractFileDir(AURI.Path);
    var LFileName := ExtractFileName(AURI.Path);
    // URI can have the followed value  "file:///hello.html" . It can refers to file in two locations:
    // 1. In application bundle resources
    // 2. In root of disk.
    // For this case we start looking for this file in application bundle resources and after in global file system.
    var LBundle := TNSBundle.OCClass.mainBundle;
    var LPath := LBundle.pathForResource(StrToNSStr(LFileName), nil);
    if LPath = nil then begin
      LBundle := TNSBundle.Wrap(TNSBundle.OCClass.bundleWithPath(StrToNSStr(LDir)));
      LPath := LBundle.pathForResource(StrToNSStr(LFileName), nil);
    end;
    if LPath <> nil then
      AUrl := TNSURL.OCClass.fileURLWithPath(LPath)
    else if FileExists(AURI.Path) then
      AUrl := TNSUrl.OCClass.fileURLWithPath(StrToNSStr(AURI.Path))
    else
      AUrl := nil;
    Result := AUrl <> nil;
  end;

begin
  var LNSNewURL: NSURL := nil;
  var LNormalizedUri: string;
  if SameText('about:blank', AURL) then
    LNormalizedUri := AURL
  else
  begin
    LNormalizedUri := TURI.FixupForREST(AURL);
    if LNormalizedUri.IsEmpty then
      Exit;

    var LUri := TURI.Create(LNormalizedUri);
    if (LUri.Scheme = 'file') and not TryGetFileNSUrl(LUri, LNSNewURL) then
      raise EFileNotFoundException.CreateRes(@SSpecifiedFileNotFound);

    LNormalizedUri := LUri.ToString;
  end;
  if LNSNewURL = nil then
    LNSNewURL := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(StrToNSStr(LNormalizedUri)));

  var LRequest := TNSURLRequest.Wrap(TNSURLRequest.OCClass.requestWithURL(LNSNewURL, NSURLRequestReloadRevalidatingCacheData, 0{timeoutInterval}));
  View.loadRequest(LRequest);
end;

{$endif}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}

{**************************************}
constructor TALMacWebBrowserView.Create;
begin
  inherited; // This will call InitView
  FNavigationDelegate := TNavigationDelegate.Create(Self);
  View.setNavigationDelegate(FNavigationDelegate.GetObjectID);
end;

{**************************************}
destructor TALMacWebBrowserView.Destroy;
begin
  View.setNavigationDelegate(nil);
  ALFreeAndNil(FNavigationDelegate);
  inherited Destroy;
end;

{**************************************}
procedure TALMacWebBrowserView.InitView;
begin
  var LConfiguration := TWKWebViewConfiguration.Create;
  try
    var V: Pointer := WKWebView(Super).initWithFrame(CGRectFromRect(Control.GetNativeViewAbsoluteRect), LConfiguration);
    if GetObjectID <> V then
      UpdateObjectID(V);
  finally
    LConfiguration.release;
  end;
end;

{**********************************************************}
function TALMacWebBrowserView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALMacWebBrowserView);
end;

{***********************************************}
function TALMacWebBrowserView.GetView: WKWebView;
begin
  Result := inherited GetView<WKWebView>;
end;

{******************************************************}
function TALMacWebBrowserView.GetControl: TALBaseWebBrowser;
begin
  Result := TALBaseWebBrowser(inherited Control);
end;

{*******************************************************************************************************}
constructor TALMacWebBrowserView.TNavigationDelegate.Create(const AWebBrowserView: TALMacWebBrowserView);
begin
  inherited Create;
  FWebBrowserView := AWebBrowserView;
end;

{*****************************************************************************************************************************************************************************************************************}
procedure TALMacWebBrowserView.TNavigationDelegate.webViewDecidePolicyForNavigationActionDecisionHandler(webView: WKWebView; decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer); cdecl;
var
  LDecisionHandlerBlock: procedure(policy: WKNavigationResponsePolicy); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDecidePolicyForNavigationActionDecisionHandler');
  {$ENDIF}
  var LShouldStartLoadUrl := FWebBrowserView.Control.ShouldStartLoadUrl(NSStrToStr(decidePolicyForNavigationAction.request.URL.absoluteString));
  @LDecisionHandlerBlock := imp_implementationWithBlock(decisionHandler);
  if LShouldStartLoadUrl then LDecisionHandlerBlock(WKNavigationActionPolicyAllow)
  else LDecisionHandlerBlock(WKNavigationActionPolicyCancel);
  imp_removeBlock(@LDecisionHandlerBlock);
end;

{***********************************************************************************************************************************************************************************************************************}
procedure TALMacWebBrowserView.TNavigationDelegate.webViewDecidePolicyForNavigationResponseDecisionHandler(webView: WKWebView; decidePolicyForNavigationResponse: WKNavigationResponse; decisionHandler: Pointer); cdecl;
var
  LDecisionHandlerBlock: procedure(policy: WKNavigationResponsePolicy); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDecidePolicyForNavigationResponseDecisionHandler');
  {$ENDIF}
  @LDecisionHandlerBlock := imp_implementationWithBlock(decisionHandler);
  LDecisionHandlerBlock(WKNavigationResponsePolicyAllow);
  imp_removeBlock(@LDecisionHandlerBlock);
end;

{**************************************************************************************************************************************************************}
procedure TALMacWebBrowserView.TNavigationDelegate.webViewDidStartProvisionalNavigation(webView: WKWebView; didStartProvisionalNavigation: WKNavigation); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDidStartProvisionalNavigation');
  {$ENDIF}
end;

{****************************************************************************************************************************************************************************************************}
procedure TALMacWebBrowserView.TNavigationDelegate.webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView; didReceiveServerRedirectForProvisionalNavigation: WKNavigation); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDidReceiveServerRedirectForProvisionalNavigation');
  {$ENDIF}
end;

{*****************************************************************************************************************************************************************************************}
procedure TALMacWebBrowserView.TNavigationDelegate.webViewDidFailProvisionalNavigationWithError(webView: WKWebView; didFailProvisionalNavigation: WKNavigation; withError: NSError); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDidFailProvisionalNavigationWithError');
  {$ENDIF}
end;

{******************************************************************************************************************************************}
procedure TALMacWebBrowserView.TNavigationDelegate.webViewDidCommitNavigation(webView: WKWebView; didCommitNavigation: WKNavigation); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDidCommitNavigation');
  {$ENDIF}
end;

{******************************************************************************************************************************************}
procedure TALMacWebBrowserView.TNavigationDelegate.webViewDidFinishNavigation(webView: WKWebView; didFinishNavigation: WKNavigation); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDidFinishNavigation');
  {$ENDIF}
end;

{*******************************************************************************************************************************************************************}
procedure TALMacWebBrowserView.TNavigationDelegate.webViewDidFailNavigationWithError(webView: WKWebView; didFailNavigation: WKNavigation; withError: NSError); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDidFailNavigationWithError');
  {$ENDIF}
end;

{***********************************************************************************************************************************************************************************************************************************}
procedure TALMacWebBrowserView.TNavigationDelegate.webViewDidReceiveAuthenticationChallengeCompletionHandler(webView: WKWebView; didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge; completionHandler: Pointer); cdecl;

type
  TAuthenticationResponseProc = reference to procedure(const ACredential: NSURLCredential);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IFNDEF ALCompilerVersionSupported131}
    {$MESSAGE WARN 'Check if FMX.WebBrowser.Delegate.Cocoa.TBaseWebViewDelegate.AuthenticateForHost was not updated and adjust the IFDEF'}
  {$ENDIF}
  procedure AuthenticateForHost(const AHost: NSString; const AResponseProc: TAuthenticationResponseProc);
  var
    LTitle: string;
    LPrompts, LValues: TArray<string>;
    LUserName, LPassword: NSString;
    LCredential: NSURLCredential;
  begin
    SetLength(LValues, 2);
    SetLength(LPrompts, 2);
    {$IF Defined(IOS)}
    LPrompts[0] := ''; // Workaround for https://quality.embarcadero.com/browse/RSP-27777
    {$ELSE}
    LPrompts[0] := SUsername;
    {$ENDIF IOS}
    LPrompts[1] := #1 + SPassword;
    LTitle := ALFormatW(SHostRequiresAuthentication, [NSStrToStr(AHost)]);
    TDialogServiceAsync.InputQuery(
      LTitle,
      LPrompts,
      LValues,
      procedure(const AResult: TModalResult; const AValues: array of string)
      begin
        if AResult = mrOK then begin
          LUserName := StrToNSStr(AValues[0]);
          LPassword := StrToNSStr(AValues[1]);
          LCredential := TNSURLCredential.Wrap(TNSURLCredential.OCClass.credentialWithUser(LUserName, LPassword, NSURLCredentialPersistenceNone));
          AResponseProc(LCredential)
        end
        else
          AResponseProc(nil);
      end);
  end;

var
  LCompletionHandlerBlock: procedure(disposition: NSURLSessionAuthChallengeDisposition; ignored: Pointer; credential: Pointer); cdecl;
  LAuthMethod: NSString;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDidReceiveAuthenticationChallengeCompletionHandler');
  {$ENDIF}
  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
  LAuthMethod := didReceiveAuthenticationChallenge.protectionSpace.authenticationMethod;
  if LAuthMethod.isEqualToString(NSURLAuthenticationMethodDefault) or LAuthMethod.isEqualToString(NSURLAuthenticationMethodHTTPBasic) or
    LAuthMethod.isEqualToString(NSURLAuthenticationMethodHTTPDigest) then begin
    AuthenticateForHost(
      webView.URL.host,
      procedure(const ACredential: NSURLCredential)
      begin
        LCompletionHandlerBlock(NSURLSessionAuthChallengeUseCredential, nil, NSObjectToID(ACredential));
        imp_removeBlock(@LCompletionHandlerBlock);
      end);
  end
  else if LAuthMethod.isEqualToString(NSURLAuthenticationMethodServerTrust) then begin
    LCompletionHandlerBlock(NSURLSessionAuthChallengePerformDefaultHandling, nil, nil);
    imp_removeBlock(@LCompletionHandlerBlock);
  end
  else begin
    LCompletionHandlerBlock(NSURLSessionAuthChallengeCancelAuthenticationChallenge, nil, nil);
    imp_removeBlock(@LCompletionHandlerBlock);
  end;
end;

{**************************************}
procedure TALMacWebBrowserView.LoadData(
            const ABaseUrl: String;
            const AData: string;
            const AMimeType: String = '';
            const AEncoding: String = '';
            const AHistoryUrl: String = '');
begin
  var LData := StrToNSStr(AData);
  var LBaseUrl := StrToNSStr(ABaseUrl);
  var LURL := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(LBaseUrl));
  View.loadHTMLString(LData, LURL);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported131}
  {$MESSAGE WARN 'Check if FMX.WebBrowser.Cocoa.TCommonWebBrowserService.DoNavigate.pas was not updated and adjust the IFDEF'}
{$ENDIF}
procedure TALMacWebBrowserView.LoadUrl(const AURL: string);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function TryGetFileNSUrl(const AURI: TURI; var AUrl: NSUrl): Boolean;
  begin
    var LDir := ExtractFileDir(AURI.Path);
    var LFileName := ExtractFileName(AURI.Path);
    // URI can have the followed value  "file:///hello.html" . It can refers to file in two locations:
    // 1. In application bundle resources
    // 2. In root of disk.
    // For this case we start looking for this file in application bundle resources and after in global file system.
    var LBundle: Pointer := TNSBundle.OCClass.mainBundle;
    var LPath := TNSBundle.Wrap(LBundle).pathForResource(StrToNSStr(LFileName), nil);
    if LPath = nil then begin
      LBundle := TNSBundle.OCClass.bundleWithPath(StrToNSStr(LDir));
      LPath := TNSBundle.Wrap(LBundle).pathForResource(StrToNSStr(LFileName), nil);
    end;
    {$IFNDEF ALCompilerVersionSupported131}
      {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-4402 is corrected, if yes remove the TNSURL.Wrap(), and adjust the IFDEF'}
    {$ENDIF}
    if LPath <> nil then
      AUrl := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(LPath))
    else if FileExists(AURI.Path) then
      AUrl := TNSUrl.Wrap(TNSUrl.OCClass.fileURLWithPath(StrToNSStr(AURI.Path)))
    else
      AUrl := nil;
    Result := AUrl <> nil;
  end;

begin
  var LNSNewURL: NSURL := nil;
  var LNormalizedUri: string;
  if SameText('about:blank', AURL) then
    LNormalizedUri := AURL
  else
  begin
    LNormalizedUri := TURI.FixupForREST(AURL);
    if LNormalizedUri.IsEmpty then
      Exit;

    var LUri := TURI.Create(LNormalizedUri);
    if (LUri.Scheme = 'file') and not TryGetFileNSUrl(LUri, LNSNewURL) then
      raise EFileNotFoundException.CreateRes(@SSpecifiedFileNotFound);

    LNormalizedUri := LUri.ToString;
  end;
  if LNSNewURL = nil then
    LNSNewURL := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(StrToNSStr(LNormalizedUri)));

  var LRequest := TNSURLRequest.Wrap(TNSURLRequest.OCClass.requestWithURL(LNSNewURL, NSURLRequestReloadRevalidatingCacheData, 0{timeoutInterval}));
  View.loadRequest(LRequest);
end;

{$endif}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

{************************************************************************************************************************}
constructor TALWinWebBrowserView.TCreateCoreWebView2EnvironmentCompletedHandler.Create(const AProc: TEnvironmentCallback);
begin
  inherited Create;
  FProc := AProc;
end;

{****************************************************************************************************************************************************************************}
function TALWinWebBrowserView.TCreateCoreWebView2EnvironmentCompletedHandler.Invoke(errorCode: HResult; const createdEnvironment: ICoreWebView2Environment): HResult; stdcall;
begin
  FProc(errorCode, createdEnvironment);
  Result := S_OK;
end;

{**********************************************************************************************************************}
constructor TALWinWebBrowserView.TCreateCoreWebView2ControllerCompletedHandler.Create(const AProc: TControllerCallback);
begin
  inherited Create;
  FProc := AProc;
end;

{****************************************************************************************************************************************************************}
function TALWinWebBrowserView.TCreateCoreWebView2ControllerCompletedHandler.Invoke(errorCode: HResult; const createdController: ICoreWebView2Controller): HResult;
begin
  FProc(errorCode, createdController);
  Result := S_OK;
end;

{*******************************************************************************************************************}
constructor TALWinWebBrowserView.TNavigationStartingEventHandler.Create(const AWebBrowserView: TALWinWebBrowserView);
begin
  inherited Create;
  FWebBrowserView := AWebBrowserView;
end;

{***************************************************************************************************************************************************************}
function TALWinWebBrowserView.TNavigationStartingEventHandler.Invoke(const sender: ICoreWebView2; const args: ICoreWebView2NavigationStartingEventArgs): HResult;
begin
  Result := S_OK;
  var LUri: PWideChar := nil;
  try
    if Succeeded(args.Get_Uri(LUri)) and (LUri <> nil) then begin
      if not FWebBrowserView.Control.ShouldStartLoadUrl(string(LUri)) then
        args.Set_Cancel(1);
    end;
  finally
    CoTaskMemFree(LUri);
  end;
end;

{*********************************************************************************************************************}
constructor TALWinWebBrowserView.TWebResourceRequestedEventHandler.Create(const AWebBrowserView: TALWinWebBrowserView);
begin
  inherited Create;
  FWebBrowserView := AWebBrowserView;
end;

{*******************************************************************************************************************************************************************}
function TALWinWebBrowserView.TWebResourceRequestedEventHandler.Invoke(const sender: ICoreWebView2; const args: ICoreWebView2WebResourceRequestedEventArgs): HResult;
var
  Request: ICoreWebView2WebResourceRequest;
  Headers: ICoreWebView2HttpRequestHeaders;
begin
  if (FWebBrowserView.FReferrer <> '') and
     Succeeded(args.Get_Request(Request)) and
     Succeeded(Request.Get_Headers(Headers)) then
  begin
    Headers.RemoveHeader('Referer');
    Headers.SetHeader(PWideChar('Referer'), PWideChar(FWebBrowserView.FReferrer));
  end;
  Result := S_OK;
end;

{*********************************************************************************************************************}
constructor TALWinWebBrowserView.TWebMessageReceivedEventHandler.Create(const AWebBrowserView: TALWinWebBrowserView);
begin
  inherited Create;
  FWebBrowserView := AWebBrowserView;
end;

{*****************************************************************************************************************************************************************}
function TALWinWebBrowserView.TWebMessageReceivedEventHandler.Invoke(const sender: ICoreWebView2; const args: ICoreWebView2WebMessageReceivedEventArgs): HRESULT;
begin
  var LMessage: PWideChar := nil;
  if Succeeded(args.Get_webMessageAsJson(LMessage)) and (LMessage <> nil) then
    FWebBrowserView.DoWebMessage(string(LMessage));
  CoTaskMemFree(LMessage);
  Result := S_OK;
end;

{**************************************}
constructor TALWinWebBrowserView.Create;
begin
  inherited;

  FPendingUrl := '';
  FPendingHtml := '';
  FReferrer := '';

  FWebView2Lifetime := TALRefCountObject.Create;
  var LWebView2Lifetime := FWebView2Lifetime.IncreaseRefCount;

  var LEnvCompletedHandler := TCreateCoreWebView2EnvironmentCompletedHandler.Create(
                                procedure(Result: HRESULT; const Env: ICoreWebView2Environment)
                                begin
                                  if LWebView2Lifetime.GetRefCount > 1 then begin
                                    Try
                                      ALCheckWinApiHResult(
                                        Env.CreateCoreWebView2Controller(
                                          Handle,
                                          TCreateCoreWebView2ControllerCompletedHandler.Create(
                                            procedure(Result: HRESULT; const Controller: ICoreWebView2Controller)
                                            begin
                                              if LWebView2Lifetime.GetRefCount > 1 then begin
                                                Try
                                                  FWebView2Controller := Controller;
                                                  ALCheckWinApiHResult(Controller.get_CoreWebView2(FWebView2));
                                                  ALCheckWinApiHResult(Controller.Set_IsVisible(1));
                                                  var Bounds: tagRECT;
                                                  Bounds.Left := round(Control.GetNativeViewPosition.X);
                                                  Bounds.Top := round(Control.GetNativeViewPosition.Y);
                                                  Bounds.Right := round(Control.GetNativeViewWidth);
                                                  Bounds.Bottom := round(Control.GetNativeViewHeight);
                                                  ALCheckWinApiHResult(Controller.Set_Bounds(Bounds));
                                                  ALCheckWinApiHResult(FWebView2.add_NavigationStarting(TNavigationStartingEventHandler.Create(Self), FNavigationStartingToken));
                                                  ALCheckWinApiHResult(FWebView2.AddWebResourceRequestedFilter('*', COREWEBVIEW2_WEB_RESOURCE_CONTEXT_ALL));
                                                  ALCheckWinApiHResult(FWebView2.add_WebResourceRequested(TWebResourceRequestedEventHandler.Create(Self), FWebResourceRequestedToken));
                                                  ALCheckWinApiHResult(FWebView2.add_WebMessageReceived(TWebMessageReceivedEventHandler.Create(Self), FWebMessageReceivedToken));
                                                  if FPendingUrl <> '' then ALCheckWinApiHResult(FWebView2.Navigate(PWideChar(FPendingUrl)))
                                                  else if FPendingHtml <> '' then ALCheckWinApiHResult(FWebView2.NavigateToString(PWideChar(FPendingHtml)));
                                                Except
                                                  LWebView2Lifetime.DecreaseRefCount;
                                                  Raise;
                                                End;
                                              end;
                                              LWebView2Lifetime.DecreaseRefCount;
                                            end)));
                                    except
                                      LWebView2Lifetime.DecreaseRefCount;
                                      Raise;
                                    end;
                                  end
                                  else LWebView2Lifetime.DecreaseRefCount;
                                end);

  ALCheckWinApiHResult(CreateCoreWebView2EnvironmentWithOptions(nil, nil, nil, LEnvCompletedHandler));
end;

{*********************************************************************}
procedure TALWinWebBrowserView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

{**************************************}
destructor TALWinWebBrowserView.Destroy;
begin
  if FWebView2 <> nil then begin
    FWebView2.remove_NavigationStarting(FNavigationStartingToken);
    FWebView2.remove_WebResourceRequested(FWebResourceRequestedToken);
    FWebView2.remove_WebMessageReceived(FWebMessageReceivedToken);
  end;

  if FWebView2Controller <> nil then begin
    FWebView2Controller.Close;
    FWebView2Controller := nil;
  end;

  FWebView2 := nil;
  FWebView2Lifetime.DecreaseRefCount;

  inherited Destroy;
end;

{******************************************************}
function TALWinWebBrowserView.GetControl: TALBaseWebBrowser;
begin
  Result := TALBaseWebBrowser(Inherited Control);
end;

{****************************************************}
function TALWinWebBrowserView.IsWebView2Ready: Boolean;
begin
  Result := FWebView2 <> nil;
end;

{******************************************************************}
procedure TALWinWebBrowserView.DoWebMessage(const AMessage: string);
begin
  Control.DoWebMessage(AMessage);
end;

{**************************************}
procedure TALWinWebBrowserView.LoadData(
            const ABaseUrl: String;
            const AData: string;
            const AMimeType: String = '';
            const AEncoding: String = '';
            const AHistoryUrl: String = '');
begin
  FReferrer := ABaseUrl;
  if IsWebView2Ready then FWebView2.NavigateToString(PWideChar(AData))
  else FPendingHtml := AData;
end;

{*********************************************************}
procedure TALWinWebBrowserView.LoadUrl(const AURL: string);
begin
  FReferrer := '';
  if IsWebView2Ready then FWebView2.Navigate(PWideChar(AURL))
  else FPendingUrl := AURL;
end;

{*********************************************************}
procedure TALWinWebBrowserView.EvaluateJavaScript(const ACode: string);
begin
  if IsWebView2Ready then
    FWebView2.ExecuteScript(PWideChar(ACode), nil);
end;

{*********************************************************}
procedure TALWinWebBrowserView.EvaluateJavaScript(const ACode: string; const AValueCallback: TALBaseWebBrowser.TJavaScriptResultCallback);
begin
  if IsWebView2Ready then
    FWebView2.ExecuteScript(
      PWideChar(ACode),
      Callback<HResult, PWideChar>.CreateAs<ICoreWebView2ExecuteScriptCompletedHandler>(
        function(ErrorCode: HResult; ResultObjectAsJson: PWideChar): HResult stdcall
        begin
          Result := S_OK;
          if Assigned(AValueCallback) then
            AValueCallback(string(ResultObjectAsJson));
        end));
end;

{$endif}
{$ENDREGION}

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALWebBrowser]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALWebBrowser, 'Size');
  UnlistPublishedProperty(TALWebBrowser, 'StyleName');
  UnlistPublishedProperty(TALWebBrowser, 'OnTap');
  {$ENDIF}
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.WebBrowser','initialization');
  {$ENDIF}
  {$IF defined(IOS)}
  TALIosWebBrowserView.IsWKWebViewClassRegistered := False;
  {$ENDIF}
  {$IF defined(ALMacOS)}
  TALMacWebBrowserView.IsWKWebViewClassRegistered := False;
  {$ENDIF}
  RegisterFmxClasses([TALWebBrowser]);

end.