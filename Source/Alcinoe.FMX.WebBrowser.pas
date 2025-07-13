unit Alcinoe.FMX.WebBrowser;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.WebBrowser.Delegate.iOS.pas was not updated and adjust the IFDEF'}
{$ENDIF}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.WebBrowser.Cocoa.pas was not updated and adjust the IFDEF'}
{$ENDIF}

uses
  system.Classes,
  system.SysUtils,
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
  Macapi.AppKit,
  Macapi.WebKit,
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  Macapi.CocoaTypes,
  FMX.DialogService.Async,
  Alcinoe.Macapi.AppKit,
  Alcinoe.FMX.NativeView.Mac,
  {$ELSEIF defined(MSWindows)}
  Winapi.WebView2,
  FMX.Controls.Win,
  Alcinoe.FMX.NativeView.Win,
  {$ENDIF}
  Fmx.controls,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.NativeControl,
  Alcinoe.FMX.Common;

Type

  {*****************************************************************************************************}
  TALShouldStartLoadUrl = procedure(ASender: TObject; const AURL: string; var AAllow: Boolean) of object;

  {************************************************}
  TALBaseWebBrowserControl = class(TALNativeControl)
  strict private
    FOnShouldStartLoadUrl: TALShouldStartLoadUrl;
  protected
    function ShouldStartLoadUrl(const AURL: string): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadUrl(const AURL: string); virtual; abstract;
    procedure LoadData(
                const ABaseUrl: String;
                const AData: string;
                const AMimeType: String = '';
                const AEncoding: String = '';
                const AHistoryUrl: String = ''); virtual; abstract;
    property OnShouldStartLoadUrl: TALShouldStartLoadUrl read FOnShouldStartLoadUrl write FOnShouldStartLoadUrl;
  end;

{$REGION ' ANDROID'}
{$IF defined(android)}
type

  {**********************************}
  TALAndroidWebBrowserControl = class;

  {**********************************************}
  TALAndroidWebView = class(TALAndroidNativeView)
  private
    type
      // ----------------
      // TWebViewListener
      TWebViewListener = class(TJavaLocal, JALWebViewListener)
      private
        FWebView: TALAndroidWebView;
      public
        constructor Create(const AWebView: TALAndroidWebView);
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
    function GetControl: TALAndroidWebBrowserControl;
  protected
    function CreateView: JView; override;
    procedure InitView; override;
  public
    constructor Create(const AControl: TControl); override;
    destructor Destroy; override;
    property View: JALWebView read GetView;
    property Control: TALAndroidWebBrowserControl read GetControl;
  end;

  {***********************************************}
  TALAndroidWebBrowserControl = class(TALBaseWebBrowserControl)
  private
    function GetNativeView: TALAndroidWebView;
  public
    property NativeView: TALAndroidWebView read GetNativeView;
    procedure LoadUrl(const AURL: string); override;
    procedure LoadData(
                const ABaseUrl: String;
                const AData: string;
                const AMimeType: String = '';
                const AEncoding: String = '';
                const AHistoryUrl: String = ''); override;
  end;

{$endif}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}
type

  {******************************}
  TALIosWebBrowserControl = class;

  {**********************************}
  IALIosWebView = interface(WKWebView)
    ['{F6E522FD-2CB4-4718-870A-4E1A29540D39}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
  end;

  {*******************************************}
  TALIosWebView = class(TALIosNativeView)
  private
    type
      // -------------------
      // TNavigationDelegate
      TNavigationDelegate = class(TOCLocal, WKNavigationDelegate)
      private
        FWebView: TALIosWebView;
      public
        constructor Create(const AWebView: TALIosWebView);
        [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
        procedure webViewDecidePolicyForNavigationAction(webView: WKWebView; navigationAction: WKNavigationAction; decisionHandler: Pointer); overload; cdecl; // TWKNavigationDelegateBlockMethod1
        [MethodName('webView:decidePolicyForNavigationResponse:decisionHandler:')]
        procedure webViewDecidePolicyForNavigationResponse(webView: WKWebView; navigationResponse: WKNavigationResponse; decisionHandler: Pointer); cdecl; // TWKNavigationDelegateBlockMethod3
        [MethodName('webView:didCommitNavigation:')]
        procedure webViewDidCommitNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
        [MethodName('webView:didFailNavigation:withError:')]
        procedure webViewDidFailNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError); cdecl;
        [MethodName('webView:didFailProvisionalNavigation:withError:')]
        procedure webViewDidFailProvisionalNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError); cdecl;
        [MethodName('webView:didFinishNavigation:')]
        procedure webViewDidFinishNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
        [MethodName('webView:didReceiveAuthenticationChallenge:completionHandler:')]
        procedure webViewDidReceiveAuthenticationChallenge(webView: WKWebView; challenge: NSURLAuthenticationChallenge; completionHandler: Pointer); cdecl; // TWKNavigationDelegateBlockMethod4
        [MethodName('webView:didReceiveServerRedirectForProvisionalNavigation:')]
        procedure webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
        [MethodName('webView:didStartProvisionalNavigation:')]
        procedure webViewDidStartProvisionalNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
        procedure webViewWebContentProcessDidTerminate(webView: WKWebView); cdecl;
      end;
  private
    FNavigationDelegate: TNavigationDelegate;
    function GetView: WKWebView;
    function GetControl: TALIosWebBrowserControl;
  protected
    procedure InitView; override;
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create(const AControl: TControl); override;
    destructor Destroy; override;
    property View: WKWebView read GetView;
    property Control: TALIosWebBrowserControl read GetControl;
  end;

  {*******************************************}
  TALIosWebBrowserControl = class(TALBaseWebBrowserControl)
  private
    class var IsWKWebViewClassRegistered: Boolean;
    function GetNativeView: TALIosWebView;
  public
    property NativeView: TALIosWebView read GetNativeView;
    procedure LoadUrl(const AURL: string); override;
    procedure LoadData(
                const ABaseUrl: String;
                const AData: string;
                const AMimeType: String = '';
                const AEncoding: String = '';
                const AHistoryUrl: String = ''); override;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}
type

  {******************************}
  TALMacWebBrowserControl = class;

  {**********************************}
  IALMacWebView = interface(WKWebView)
    ['{22B2CD52-0DF9-4645-8359-4DDAE64A5CA1}']
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
  end;

  {*******************************************}
  TALMacWebView = class(TALMacNativeView)
  private
    type
      // -------------------
      // TNavigationDelegate
      TNavigationDelegate = class(TOCLocal, WKNavigationDelegate)
      private
        FWebView: TALMacWebView;
      public
        constructor Create(const AWebView: TALMacWebView);
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
    function GetControl: TALMacWebBrowserControl;
  protected
    procedure InitView; override;
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create(const AControl: TControl); override;
    destructor Destroy; override;
    property View: WKWebView read GetView;
    property Control: TALMacWebBrowserControl read GetControl;
  end;

  {*******************************************}
  TALMacWebBrowserControl = class(TALBaseWebBrowserControl)
  private
    class var IsWKWebViewClassRegistered: Boolean;
    function GetNativeView: TALMacWebView;
  public
    property NativeView: TALMacWebView read GetNativeView;
    procedure LoadUrl(const AURL: string); override;
    procedure LoadData(
                const ABaseUrl: String;
                const AData: string;
                const AMimeType: String = '';
                const AEncoding: String = '';
                const AHistoryUrl: String = ''); override;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}
type

  {************************}
  TALWinWebBrowserControl = class;

  {**************************************}
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
        FWebBrowserControl: TALWinWebBrowserControl;
      public
        constructor Create(const AControl: TALWinWebBrowserControl);
        function Invoke(const sender: ICoreWebView2; const args: ICoreWebView2NavigationStartingEventArgs): HResult; stdcall;
      end;
      // ---------------------------------
      // TWebResourceRequestedEventHandler
      TWebResourceRequestedEventHandler = class(TInterfacedObject, ICoreWebView2WebResourceRequestedEventHandler)
      private
        FWebBrowserControl: TALWinWebBrowserControl;
      public
        constructor Create(const AControl: TALWinWebBrowserControl);
        function Invoke(const sender: ICoreWebView2; const args: ICoreWebView2WebResourceRequestedEventArgs): HResult; stdcall;
      end;
  private
    FPendingUrl: String;
    FPendingHtml: String;
    FReferrer: String;
    FWebView2Controller: ICoreWebView2Controller;
    FWebView2: ICoreWebView2;
    function GetControl: TALWinWebBrowserControl;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(const AControl: TControl); override;
    destructor Destroy; override;
    property Control: TALWinWebBrowserControl read GetControl;
  end;

  {*******************************************}
  TALWinWebBrowserControl = class(TALBaseWebBrowserControl)
  private
    function GetNativeView: TALWinWebBrowserView;
  public
    constructor Create(AOwner: TComponent); override;
    property NativeView: TALWinWebBrowserView read GetNativeView;
    procedure LoadUrl(const AURL: string); override;
    procedure LoadData(
                const ABaseUrl: String;
                const AData: string;
                const AMimeType: String = '';
                const AEncoding: String = '';
                const AHistoryUrl: String = ''); override;
  end;

{$endif}
{$ENDREGION}

type

  {*******************************************}
  [ComponentPlatforms($FFFF)]
  TALWebBrowser = class(TALNativeControl, IControlTypeSupportable, IALNativeControl)
  private
    FWebBrowserControl: TALBaseWebBrowserControl;
    FOnShouldStartLoadUrl: TALShouldStartLoadUrl;
    procedure OnShouldStartLoadUrlImpl(ASender: TObject; const AURL: string; var AAllow: Boolean);
    {$IF defined(android)}
    function GetNativeView: TALAndroidWebBrowserView;
    {$ELSEIF defined(IOS)}
    function GetNativeView: TALIosWebBrowserView;
    {$ELSEIF defined(ALMacOS)}
    function GetNativeView: TALMacWebBrowserView;
    {$ELSEIF defined(MSWindows)}
    function GetNativeView: TALWinWebBrowserView;
    {$ENDIF}
    { IControlTypeSupportable }
    function GetControlType: TControlType;
    procedure SetControlType(const Value: TControlType);
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
    function CreateWebBrowserControl: TALBaseWebBrowserControl; virtual;
    function GetWebBrowserControl: TALBaseWebBrowserControl; virtual;
    property WebBrowserControl: TALBaseWebBrowserControl read GetWebBrowserControl;
    procedure InitWebBrowserControl; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Loaded; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF defined(android)}
    property NativeView: TALAndroidWebBrowserView read GetNativeView;
    {$ELSEIF defined(IOS)}
    property NativeView: TALIosWebBrowserView read GetNativeView;
    {$ELSEIF defined(ALMacOS)}
    property NativeView: TALMacWebBrowserView read GetNativeView;
    {$ELSEIF defined(MSWindows)}
    property NativeView: TALWinWebBrowserView read GetNativeView;
    {$ENDIF}
    function HasNativeView: boolean;
    Procedure AddNativeView;
    Procedure RemoveNativeView;
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
  published
    //property Action;
    property Align;
    property Anchors;
    //property AutoSize;
    property CanFocus default True;
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
    property OnShouldStartLoadUrl: TALShouldStartLoadUrl read FOnShouldStartLoadUrl write FOnShouldStartLoadUrl;
  end;

procedure Register;

implementation

uses
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
  iOSapi.CoreText,
  FMX.Helpers.iOS,
  FMX.Consts,
  {$ELSEIF defined(ALMacOS)}
  System.UITypes,
  System.Net.URLClient,
  System.RTLConsts,
  Macapi.CoreFoundation,
  Macapi.Helpers,
  Macapi.CoreGraphics,
  FMX.Helpers.Mac,
  FMX.Consts,
  Alcinoe.Macapi.Foundation,
  {$ELSEIF defined(MSWindows)}
  Winapi.Ole2,
  Winapi.CommCtrl,
  Winapi.EdgeUtils,
  {$endif}
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  Alcinoe.Common;

{********************************************************}
constructor TALBaseWebBrowserControl.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FOnShouldStartLoadUrl := nil;
end;

{***************************************}
function TALBaseWebBrowserControl.ShouldStartLoadUrl(const AURL: string): Boolean;
begin
  if assigned(fOnShouldStartLoadUrl) then
    fOnShouldStartLoadUrl(Self, AURL, Result)
  else
    Result := True;
end;

{$REGION ' ANDROID'}
{$IF defined(android)}

{*************************************************************************************}
constructor TALAndroidWebView.TWebViewListener.Create(const AWebView: TALAndroidWebView);
begin
  inherited Create;
  FWebView := AWebView;
end;

{*************************************************************************************}
procedure TALAndroidWebView.TWebViewListener.doUpdateVisitedHistory(view: JWebView; url: JString; isReload: Boolean);
begin
  // Nothing
end;

{*************************************************************************************}
procedure TALAndroidWebView.TWebViewListener.onFormResubmission(view: JWebView; dontResend: JMessage; resend: JMessage);
begin
  // Nothing
end;

{*************************************************************************************}
procedure TALAndroidWebView.TWebViewListener.onLoadResource(view: JWebView; url: JString);
begin
  // Nothing
end;

{*************************************************************************************}
procedure TALAndroidWebView.TWebViewListener.onPageFinished(view: JWebView; url: JString);
begin
  // Nothing
end;

{*************************************************************************************}
procedure TALAndroidWebView.TWebViewListener.onPageStarted(view: JWebView; url: JString; favicon: JBitmap);
begin
  // Nothing
end;

{*************************************************************************************}
procedure TALAndroidWebView.TWebViewListener.onReceivedError(view: JWebView; errorCode: Integer; description: JString; failingUrl: JString);
begin
  // Nothing
end;

{*************************************************************************************}
procedure TALAndroidWebView.TWebViewListener.onReceivedHttpAuthRequest(view: JWebView; handler: JHttpAuthHandler; host: JString; realm: JString);
begin
  // Nothing
end;

{*************************************************************************************}
procedure TALAndroidWebView.TWebViewListener.onReceivedSslError(view: JWebView; handler: JSslErrorHandler; error: JSslError);
begin
  // Nothing
end;

{*************************************************************************************}
procedure TALAndroidWebView.TWebViewListener.onScaleChanged(view: JWebView; oldScale: Single; newScale: Single);
begin
  // Nothing
end;

{*************************************************************************************}
procedure TALAndroidWebView.TWebViewListener.onUnhandledKeyEvent(view: JWebView; event: JKeyEvent);
begin
  // Nothing
end;

{*************************************************************************************}
function TALAndroidWebView.TWebViewListener.shouldOverrideKeyEvent(view: JWebView; event: JKeyEvent): Boolean;
begin
  Result := False;
end;

{*************************************************************************************}
function TALAndroidWebView.TWebViewListener.shouldOverrideUrlLoading(view: JWebView; url: JString): Boolean;
begin
  Result := not FWebView.Control.ShouldStartLoadUrl(JStringToString(url));
end;

{*************************************************************}
constructor TALAndroidWebView.Create(const AControl: TControl);
begin
  FWebViewListener := TWebViewListener.Create(self);
  inherited create(AControl);  // This will call InitView
end;

{************************************}
destructor TALAndroidWebView.Destroy;
begin
  View.setVisibility(TJView.JavaClass.INVISIBLE);
  View.setListener(nil);
  alfreeandNil(FWebViewListener);
  inherited;
end;

{********************************************}
function TALAndroidWebView.CreateView: JView;
begin
  Result := TJALWebView.JavaClass.init(TAndroidHelper.Activity)
end;

{************************************}
procedure TALAndroidWebView.InitView;
begin
  inherited;
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

{***********************************************}
function TALAndroidWebView.GetView: JALWebView;
begin
  Result := inherited GetView<JALWebView>;
end;

{***********************************************}
function TALAndroidWebView.GetControl: TALAndroidWebBrowserControl;
begin
  Result := TALAndroidWebBrowserControl(inherited Control)
end;

{***************************************************************}
function TALAndroidWebBrowserControl.GetNativeView: TALAndroidWebView;
begin
  result := TALAndroidWebView(inherited NativeView);
end;

{******************************************************************}
procedure TALAndroidWebBrowserControl.LoadUrl(const AURL: string);
begin
  NativeView.View.loadUrl(StringToJString(AUrl));
end;

{**********************}
procedure TALAndroidWebBrowserControl.LoadData(
            const ABaseUrl: String;
            const AData: string;
            const AMimeType: String = '';
            const AEncoding: String = '';
            const AHistoryUrl: String = '');
begin
  NativeView.View.loadDataWithBaseURL(
    StringToJString(ABaseUrl), // baseUrl: JString;
    StringToJString(AData), // data: JString;
    StringToJString(AMimeType), // mimeType: JString;
    StringToJString(AEncoding), // encoding: JString;
    StringToJString(AHistoryUrl)); // historyUrl: JString
end;

{$endif}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}

{*******************************************************}
constructor TALIosWebView.Create(const AControl: TControl);
begin
  FNavigationDelegate := TNavigationDelegate.Create(Self);
  inherited create(AControl);
end;

{***********************************}
destructor TALIosWebView.Destroy;
begin
  View.setNavigationDelegate(nil);
  ALFreeAndNil(FNavigationDelegate);
  inherited Destroy;
end;

{*********************************************************}
procedure TALIosWebView.InitView;
begin
  var LConfiguration := TWKWebViewConfiguration.Create;
  try
    LConfiguration.setAllowsInlineMediaPlayback(True);
    var V: Pointer := WKWebView(Super).initWithFrame(CGRectFromRect(Control.AbsoluteRect), LConfiguration);
    if GetObjectID <> V then
      UpdateObjectID(V);
  finally
    LConfiguration.release;
  end;
  View.setNavigationDelegate(FNavigationDelegate.GetObjectID);
end;

{*********************************************************}
function TALIosWebView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALIosWebView);
end;

{************************************************}
function TALIosWebView.GetView: WKWebView;
begin
  Result := inherited GetView<WKWebView>;
end;

{************************************************}
function TALIosWebView.GetControl: TALIosWebBrowserControl;
begin
  Result := TALIosWebBrowserControl(inherited Control);
end;

{************************************************************************************}
constructor TALIosWebView.TNavigationDelegate.Create(const AWebView: TALIosWebView);
begin
  inherited Create;
  FWebView := AWebView;
end;

{*******************************************************}
procedure TALIosWebView.TNavigationDelegate.webViewDecidePolicyForNavigationAction(webView: WKWebView; navigationAction: WKNavigationAction; decisionHandler: Pointer); // TWKNavigationDelegateBlockMethod1
var
  LBlockImp: procedure(policy: WKNavigationActionPolicy); cdecl;
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewDecidePolicyForNavigationAction');
  {$ENDIF}
  var LShouldStartLoadUrl := FWebView.Control.ShouldStartLoadUrl(NSStrToStr(navigationAction.request.URL.absoluteString));
  @LBlockImp := imp_implementationWithBlock(decisionHandler);
  if LShouldStartLoadUrl then LBlockImp(WKNavigationActionPolicyAllow)
  else LBlockImp(WKNavigationActionPolicyCancel);
  imp_removeBlock(@LBlockImp);
end;

{*******************************************************}
procedure TALIosWebView.TNavigationDelegate.webViewDecidePolicyForNavigationResponse(webView: WKWebView; navigationResponse: WKNavigationResponse; decisionHandler: Pointer); // TWKNavigationDelegateBlockMethod3
var
  LDecisionHandlerBlock: procedure(policy: WKNavigationResponsePolicy); cdecl;
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewDecidePolicyForNavigationResponse');
  {$ENDIF}
  @LDecisionHandlerBlock := imp_implementationWithBlock(decisionHandler);
  LDecisionHandlerBlock(WKNavigationResponsePolicyAllow);
  imp_removeBlock(@LDecisionHandlerBlock);
end;

{*******************************************************}
procedure TALIosWebView.TNavigationDelegate.webViewDidCommitNavigation(webView: WKWebView; navigation: WKNavigation);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewDidCommitNavigation');
  {$ENDIF}
end;

{*******************************************************}
procedure TALIosWebView.TNavigationDelegate.webViewDidFailNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewDidFailNavigation');
  {$ENDIF}
end;

{*******************************************************}
procedure TALIosWebView.TNavigationDelegate.webViewDidFailProvisionalNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewDidFailProvisionalNavigation');
  {$ENDIF}
end;

{*******************************************************}
procedure TALIosWebView.TNavigationDelegate.webViewDidFinishNavigation(webView: WKWebView; navigation: WKNavigation);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewDidFinishNavigation');
  {$ENDIF}
end;

{*******************************************************}
procedure TALIosWebView.TNavigationDelegate.webViewDidReceiveAuthenticationChallenge(webView: WKWebView; challenge: NSURLAuthenticationChallenge; completionHandler: Pointer); // TWKNavigationDelegateBlockMethod4

type
  TAuthenticationResponseProc = reference to procedure(const ACredential: NSURLCredential);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
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
    LTitle := Format(SHostRequiresAuthentication, [NSStrToStr(AHost)]);
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
  //ALLog('TNavigationDelegate.webViewDidReceiveAuthenticationChallenge');
  {$ENDIF}
  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
  LAuthMethod := challenge.protectionSpace.authenticationMethod;
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

{*******************************************************}
procedure TALIosWebView.TNavigationDelegate.webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView; navigation: WKNavigation);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewDidReceiveServerRedirectForProvisionalNavigation');
  {$ENDIF}
end;

{*******************************************************}
procedure TALIosWebView.TNavigationDelegate.webViewDidStartProvisionalNavigation(webView: WKWebView; navigation: WKNavigation);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewDidStartProvisionalNavigation');
  {$ENDIF}
end;

{*******************************************************}
procedure TALIosWebView.TNavigationDelegate.webViewWebContentProcessDidTerminate(webView: WKWebView);
begin
  {$IF defined(DEBUG)}
  //ALLog('TNavigationDelegate.webViewWebContentProcessDidTerminate');
  {$ENDIF}
end;

{************************************************************}
function TALIosWebBrowserControl.GetNativeView: TALIosWebView;
begin
  result := TALIosWebView(inherited NativeView);
end;

{************************************************************}
procedure TALIosWebBrowserControl.LoadUrl(const AURL: string);

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
  NativeView.View.loadRequest(LRequest);
end;

{******************************************}
procedure TALIosWebBrowserControl.LoadData(
            const ABaseUrl: String;
            const AData: string;
            const AMimeType: String = '';
            const AEncoding: String = '';
            const AHistoryUrl: String = '');
begin
  var LData := StrToNSStr(AData);
  var LBaseUrl := StrToNSStr(ABaseUrl);
  var LURL := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(LBaseUrl));
  NativeView.View.loadHTMLString(LData, LURL);
end;

{$endif}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}

{*******************************************************}
constructor TALMacWebView.Create(const AControl: TControl);
begin
  FNavigationDelegate := TNavigationDelegate.Create(Self);
  inherited create(AControl);
end;

{***********************************}
destructor TALMacWebView.Destroy;
begin
  View.setNavigationDelegate(nil);
  ALFreeAndNil(FNavigationDelegate);
  inherited Destroy;
end;

{*********************************************************}
procedure TALMacWebView.InitView;
begin
  var LConfiguration := TWKWebViewConfiguration.Create;
  try
    var V: Pointer := WKWebView(Super).initWithFrame(CGRectFromRect(Control.AbsoluteRect), LConfiguration);
    if GetObjectID <> V then
      UpdateObjectID(V);
  finally
    LConfiguration.release;
  end;
  View.setNavigationDelegate(FNavigationDelegate.GetObjectID);
end;

{*********************************************************}
function TALMacWebView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALMacWebView);
end;

{************************************************}
function TALMacWebView.GetView: WKWebView;
begin
  Result := inherited GetView<WKWebView>;
end;

{************************************************}
function TALMacWebView.GetControl: TALMacWebBrowserControl;
begin
  Result := TALMacWebBrowserControl(inherited Control);
end;

{************************************************************************************}
constructor TALMacWebView.TNavigationDelegate.Create(const AWebView: TALMacWebView);
begin
  inherited Create;
  FWebView := AWebView;
end;

{*******************************************************}
procedure TALMacWebView.TNavigationDelegate.webViewDecidePolicyForNavigationActionDecisionHandler(webView: WKWebView; decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer); cdecl;
var
  LDecisionHandlerBlock: procedure(policy: WKNavigationResponsePolicy); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDecidePolicyForNavigationActionDecisionHandler');
  {$ENDIF}
  var LShouldStartLoadUrl := FWebView.Control.ShouldStartLoadUrl(NSStrToStr(decidePolicyForNavigationAction.request.URL.absoluteString));
  @LDecisionHandlerBlock := imp_implementationWithBlock(decisionHandler);
  if LShouldStartLoadUrl then LDecisionHandlerBlock(WKNavigationActionPolicyAllow)
  else LDecisionHandlerBlock(WKNavigationActionPolicyCancel);
  imp_removeBlock(@LDecisionHandlerBlock);
end;

{*******************************************************}
procedure TALMacWebView.TNavigationDelegate.webViewDecidePolicyForNavigationResponseDecisionHandler(webView: WKWebView; decidePolicyForNavigationResponse: WKNavigationResponse; decisionHandler: Pointer); cdecl;
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

{*******************************************************}
procedure TALMacWebView.TNavigationDelegate.webViewDidStartProvisionalNavigation(webView: WKWebView; didStartProvisionalNavigation: WKNavigation); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDidStartProvisionalNavigation');
  {$ENDIF}
end;

{*******************************************************}
procedure TALMacWebView.TNavigationDelegate.webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView; didReceiveServerRedirectForProvisionalNavigation: WKNavigation); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDidReceiveServerRedirectForProvisionalNavigation');
  {$ENDIF}
end;

{*******************************************************}
procedure TALMacWebView.TNavigationDelegate.webViewDidFailProvisionalNavigationWithError(webView: WKWebView; didFailProvisionalNavigation: WKNavigation; withError: NSError); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDidFailProvisionalNavigationWithError');
  {$ENDIF}
end;

{*******************************************************}
procedure TALMacWebView.TNavigationDelegate.webViewDidCommitNavigation(webView: WKWebView; didCommitNavigation: WKNavigation); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDidCommitNavigation');
  {$ENDIF}
end;

{*******************************************************}
procedure TALMacWebView.TNavigationDelegate.webViewDidFinishNavigation(webView: WKWebView; didFinishNavigation: WKNavigation); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDidFinishNavigation');
  {$ENDIF}
end;

{*******************************************************}
procedure TALMacWebView.TNavigationDelegate.webViewDidFailNavigationWithError(webView: WKWebView; didFailNavigation: WKNavigation; withError: NSError); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TNavigationDelegate.webViewDidFailNavigationWithError');
  {$ENDIF}
end;

{*******************************************************}
procedure TALMacWebView.TNavigationDelegate.webViewDidReceiveAuthenticationChallengeCompletionHandler(webView: WKWebView; didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge; completionHandler: Pointer); cdecl;

type
  TAuthenticationResponseProc = reference to procedure(const ACredential: NSURLCredential);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
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
    LTitle := Format(SHostRequiresAuthentication, [NSStrToStr(AHost)]);
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
    AuthenticateForHost(webView.URL.host,
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

{************************************************************}
function TALMacWebBrowserControl.GetNativeView: TALMacWebView;
begin
  result := TALMacWebView(inherited NativeView);
end;

{************************************************************}
procedure TALMacWebBrowserControl.LoadUrl(const AURL: string);

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
  NativeView.View.loadRequest(LRequest);
end;

{******************************************}
procedure TALMacWebBrowserControl.LoadData(
            const ABaseUrl: String;
            const AData: string;
            const AMimeType: String = '';
            const AEncoding: String = '';
            const AHistoryUrl: String = '');
begin
  var LData := StrToNSStr(AData);
  var LBaseUrl := StrToNSStr(ABaseUrl);
  var LURL := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(LBaseUrl));
  NativeView.View.loadHTMLString(LData, LURL);
end;

{$endif}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

{***********************************************}
function InitCommonControl(CC: Integer): Boolean;
var
  ICC: TInitCommonControlsEx;
begin
  ICC.dwSize := SizeOf(TInitCommonControlsEx);
  ICC.dwICC := CC;
  Result := InitCommonControlsEx(ICC);
  if not Result then InitCommonControls;
end;

{****************************************}
procedure CheckCommonControl(CC: Integer);
begin
  if not InitCommonControl(CC) then
    raise EComponentError.Create('This control requires version 4.70 or greater of COMCTL32.DLL');
end;

{**********************************************************}
constructor TALWinWebBrowserView.TCreateCoreWebView2EnvironmentCompletedHandler.Create(const AProc: TEnvironmentCallback);
begin
  inherited Create;
  FProc := AProc;
end;

{**********************************************************}
function TALWinWebBrowserView.TCreateCoreWebView2EnvironmentCompletedHandler.Invoke(errorCode: HResult; const createdEnvironment: ICoreWebView2Environment): HResult; stdcall;
begin
  FProc(errorCode, createdEnvironment);
  Result := S_OK;
end;

{**********************************************************}
constructor TALWinWebBrowserView.TCreateCoreWebView2ControllerCompletedHandler.Create(const AProc: TControllerCallback);
begin
  inherited Create;
  FProc := AProc;
end;

{**********************************************************}
function TALWinWebBrowserView.TCreateCoreWebView2ControllerCompletedHandler.Invoke(errorCode: HResult; const createdController: ICoreWebView2Controller): HResult;
begin
  FProc(errorCode, createdController);
  Result := S_OK;
end;

{***********************************************}
constructor TALWinWebBrowserView.TNavigationStartingEventHandler.Create(const AControl: TALWinWebBrowserControl);
begin
  inherited Create;
  FWebBrowserControl := AControl;
end;

{**********************************************************}
function TALWinWebBrowserView.TNavigationStartingEventHandler.Invoke(const sender: ICoreWebView2; const args: ICoreWebView2NavigationStartingEventArgs): HResult;
var
  uri: PWideChar;
  url: string;
begin
  args.Get_Uri(uri);
  url := uri;
  var LShouldStartLoadUrl := FWebBrowserControl.ShouldStartLoadUrl(url);
  if not LShouldStartLoadUrl then
    args.Set_Cancel(1);
  Result := S_OK;
end;

{**********************************************************}
constructor TALWinWebBrowserView.TWebResourceRequestedEventHandler.Create(const AControl: TALWinWebBrowserControl);
begin
  inherited Create;
  FWebBrowserControl := AControl;
end;

{**********************************************************}
function TALWinWebBrowserView.TWebResourceRequestedEventHandler.Invoke(const sender: ICoreWebView2; const args: ICoreWebView2WebResourceRequestedEventArgs): HResult;
var
  Request: ICoreWebView2WebResourceRequest;
  Headers: ICoreWebView2HttpRequestHeaders;
begin
  if (FWebBrowserControl.NativeView.FReferrer <> '') and
     Succeeded(args.Get_Request(Request)) and
     Succeeded(Request.Get_Headers(Headers)) then
  begin
    Headers.RemoveHeader('Referer');
    Headers.SetHeader(PWideChar('Referer'), PWideChar(FWebBrowserControl.NativeView.FReferrer));
  end;
  Result := S_OK;
end;

{**********************************************************}
constructor TALWinWebBrowserView.Create(const AControl: TControl);
var
  EnvCompletedHandler: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler;
begin
  inherited Create(AControl);
  FPendingUrl := '';
  FPendingHtml := '';
  FReferrer := '';

  // Initialize COM environment
  CoInitialize(nil);

  EnvCompletedHandler := TCreateCoreWebView2EnvironmentCompletedHandler.Create(
    procedure(Result: HRESULT; const Env: ICoreWebView2Environment)
    begin
      Env.CreateCoreWebView2Controller(Handle,
        TCreateCoreWebView2ControllerCompletedHandler.Create(
          procedure(Result: HRESULT; const Controller: ICoreWebView2Controller)
          begin
            FWebView2Controller := Controller;
            Controller.get_CoreWebView2(FWebView2);
            Controller.Set_IsVisible(1);
            var Bounds: tagRECT;
            Bounds.Left := 0;
            Bounds.Top := 0;
            Bounds.Right := round(Control.Width);
            Bounds.Bottom := round(Control.Height);
            Controller.Set_Bounds(Bounds);
            var Token: EventRegistrationToken;
            FWebView2.add_NavigationStarting(TNavigationStartingEventHandler.Create(Control), Token);
            FWebView2.AddWebResourceRequestedFilter('*', COREWEBVIEW2_WEB_RESOURCE_CONTEXT_ALL);
            FWebView2.add_WebResourceRequested(TWebResourceRequestedEventHandler.Create(Control), Token);
            if FPendingUrl <> '' then FWebView2.Navigate(PWideChar(FPendingUrl))
            else if FPendingHtml <> '' then FWebView2.NavigateToString(PWideChar(FPendingHtml));
          end));
    end);

  CreateCoreWebView2EnvironmentWithOptions(nil, nil, nil, EnvCompletedHandler);
end;

{***************************************************************}
procedure TALWinWebBrowserView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

{********************************}
destructor TALWinWebBrowserView.Destroy;
begin
  inherited Destroy;
end;

{********************************}
function TALWinWebBrowserView.GetControl: TALWinWebBrowserControl;
begin
  Result := TALWinWebBrowserControl(Inherited Control);
end;

{*******************************************************}
constructor TALWinWebBrowserControl.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
end;

{*******************************************************}
function TALWinWebBrowserControl.GetNativeView: TALWinWebBrowserView;
begin
  result := TALWinWebBrowserView(inherited NativeView);
end;

{*************************************************}
procedure TALWinWebBrowserControl.LoadUrl(const AURL: string);
begin
  NativeView.FReferrer := '';
  if NativeView.FWebView2 <> nil then NativeView.FWebView2.Navigate(PWideChar(AURL))
  else NativeView.FPendingUrl := AURL;
end;

{*****************************************}
procedure TALWinWebBrowserControl.LoadData(
            const ABaseUrl: String;
            const AData: string;
            const AMimeType: String = '';
            const AEncoding: String = '';
            const AHistoryUrl: String = '');
begin
  NativeView.FReferrer := ABaseUrl;
  if NativeView.FWebView2 <> nil then NativeView.FWebView2.NavigateToString(PWideChar(AData))
  else NativeView.FPendingHtml := AData;
end;

{$endif}
{$ENDREGION}

{*************************************************}
constructor TALWebBrowser.Create(AOwner: TComponent);
begin
  inherited;
  CanFocus := True;
  FOnShouldStartLoadUrl := Nil;
  {$IF defined(DEBUG)}
  if FWebBrowserControl <> nil then
    raise Exception.Create('Error CE2932F6-E44A-4A4B-AAE3-71FE4077FCF2');
  {$ENDIF}
  FWebBrowserControl := CreateWebBrowserControl;
  InitWebBrowserControl;
end;

{*****************************}
destructor TALWebBrowser.Destroy;
begin
  ALFreeAndNil(FWebBrowserControl);
  inherited Destroy;
end;

{*********************************************************}
function TALWebBrowser.CreateWebBrowserControl: TALBaseWebBrowserControl;
begin
  {$IF defined(android)}
  Result := TALAndroidWebBrowserControl.Create(self);
  {$ELSEIF defined(ios)}
  Result := TALIosWebBrowserControl.Create(self);
  {$ELSEIF defined(ALMacOS)}
  Result := TALMacWebBrowserControl.Create(self);
  {$ELSEIF defined(MSWindows)}
  Result := TALWinWebBrowserControl.Create(self);
  {$ELSE}
    Not implemented
  {$ENDIF}
end;

{************************************}
procedure TALWebBrowser.InitWebBrowserControl;
begin
  FWebBrowserControl.Parent := self;
  FWebBrowserControl.Stored := False;
  FWebBrowserControl.SetSubComponent(True);
  FWebBrowserControl.Locked := True;
  FWebBrowserControl.Align := TALAlignLayout.Client;
  FWebBrowserControl.CanFocus := False;
  FWebBrowserControl.CanParentFocus := True;
  FWebBrowserControl.HitTest := False;
  FWebBrowserControl.OnShouldStartLoadUrl := OnShouldStartLoadUrlImpl;
end;

{***************************}
procedure TALWebBrowser.Loaded;
begin
  inherited;
  {$IF not defined(ALDPK)}
  if (NativeView <> nil) and (NativeView.visible) then begin
    // Because AncestorParentChanged is not called during loading,
    // we must call NativeView.SetVisible(true) in TALBaseWebBrowser.Loaded
    // to hide the NativeView in case a parent control is hidden.
    NativeView.SetVisible(true);
  end;
  {$ENDIF}
end;

{****************************}
procedure TALWebBrowser.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{********************}
{$IF defined(android)}
function TALWebBrowser.GetWebBrowserControl: TALBaseWebBrowserControl;
begin
  if FWebBrowserControl = nil then begin
    FWebBrowserControl := CreateWebBrowserControl;
    InitWebBrowserControl;
  end;
  Result := FWebBrowserControl;
end;
{$ENDIF}

{********************}
{$IF defined(android)}
Function TALWebBrowser.CreateNativeView: TALAndroidNativeView;
begin
  result := TALAndroidWebView.create(self);
end;
{$ENDIF}

{********************}
{$IF defined(android)}
function TALWebBrowser.GetNativeView: TALAndroidWebBrowserView;
begin
  result := TALAndroidWebBrowserView(inherited NativeView);
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALWebBrowser.GetWebBrowserControl: TALBaseWebBrowserControl;
begin
  if FWebBrowserControl = nil then begin
    FWebBrowserControl := CreateWebBrowserControl;
    InitWebBrowserControl;
  end;
  Result := FWebBrowserControl;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
Function TALWebBrowser.CreateNativeView: TALIosNativeView;
begin

  // We must create a WKWebView instance explicitly here to ensure that the underlying
  // Objective-C class (WKWebView) is properly loaded and registered by the Delphi runtime.
  // Without this, calling `TALIosWebView(inherited GetNativeView)` will raise the following error:
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
  //   RegisterObjectiveCClass(TALIosWebView, TypeInfo(IALIosWebView));
  // fails as well, with:
  //   Unhandled Exception | Objective-C class WKWebView could not be found
  //   At address: $00000001046CA014
  //   (Macapi.Objectivec.ObjectiveCClassNotFound)
  //
  // The only reliable workaround is to instantiate the WKWebView explicitly to force
  // the WebKit framework to be loaded and the class to be registered:

  if not IsWKWebViewClassRegistered then begin
    LoadFramework(libWebKit); // force load framework
    var LConfiguration := TWKWebViewConfiguration.Create;
    try
      var LWKWebView := TWKWebView.Wrap(TWKWebView.Alloc.initWithFrame(CGRectMake(0, 0, 0, 0), LConfiguration));
      LWKWebView.release;
    finally
      LConfiguration.release;
    end;
    IsWKWebViewClassRegistered := True;
  end;

  result := TALIosWebView.create(self);

end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALWebBrowser.GetNativeView: TALIosWebBrowserView;
begin
  result := TALIosWebBrowserView(inherited NativeView);
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
function TALWebBrowser.GetWebBrowserControl: TALBaseWebBrowserControl;
begin
  if FWebBrowserControl = nil then begin
    FWebBrowserControl := CreateWebBrowserControl;
    InitWebBrowserControl;
  end;
  Result := FWebBrowserControl;
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
Function TALWebBrowser.CreateNativeView: TALMacNativeView;
begin

  // We must create a WKWebView instance explicitly here to ensure that the underlying
  // Objective-C class (WKWebView) is properly loaded and registered by the Delphi runtime.
  // Without this, calling `TALMacWebView(inherited GetNativeView)` will raise the following error:
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
  //   RegisterObjectiveCClass(TALMacWebView, TypeInfo(IALMacWebView));
  // fails as well, with:
  //   Unhandled Exception | Objective-C class WKWebView could not be found
  //   At address: $00000001046CA014
  //   (Macapi.Objectivec.ObjectiveCClassNotFound)
  //
  // The only reliable workaround is to instantiate the WKWebView explicitly to force
  // the WebKit framework to be loaded and the class to be registered:

  if not IsWKWebViewClassRegistered then begin
    LoadFramework(libWebKit); // force load framework
    var LConfiguration := TWKWebViewConfiguration.Create;
    try
      var LWKWebView := TWKWebView.Wrap(TWKWebView.Alloc.initWithFrame(CGRectMake(0, 0, 0, 0), LConfiguration));
      LWKWebView.release;
    finally
      LConfiguration.release;
    end;
    IsWKWebViewClassRegistered := True;
  end;

  result := TALMacWebView.create(self);

end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
function TALWebBrowser.GetNativeView: TALMacWebBrowserView;
begin
  result := TALMacWebBrowserView(inherited NativeView);
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
function TALWebBrowser.GetWebBrowserControl: TALBaseWebBrowserControl;
begin
  if FWebBrowserControl = nil then begin
    FWebBrowserControl := CreateWebBrowserControl;
    InitWebBrowserControl;
  end;
  Result := FWebBrowserControl;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
Function TALWebBrowser.CreateNativeView: TALWinNativeView;
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
function TALWebBrowser.GetNativeView: TALWinWebBrowserView;
begin
  result := TALWinWebBrowserView(inherited NativeView);
end;
{$ENDIF}

{****************************}
procedure TALWebBrowser.DoEnter;
begin
  {$IF defined(DEBUG)}
  ALLog(classname+'.DoEnter', 'control.name: ' + Name);
  {$ENDIF}
  inherited DoEnter;
  {$IF not defined(ALDPK)}
  if HasNativeView then
    NativeView.SetFocus;
  {$ENDIF}
end;

{***************************}
procedure TALWebBrowser.DoExit;
begin
  {$IF defined(DEBUG)}
  ALLog(classname+'.DoExit', 'control.name: ' + Name);
  {$ENDIF}
  inherited DoExit;
  {$IF not defined(ALDPK)}
  if HasNativeView then
    NativeView.ResetFocus;
  {$ENDIF}
end;

{************************************************}
function TALWebBrowser.GetControlType: TControlType;
begin
  // We need ControlType because in function TFMXViewBase.canBecomeFirstResponder: Boolean;
  // we use it in IsNativeControl to determine if it's a native control or not
  Result := TControlType.Platform;
end;

{**************************************************************}
procedure TALWebBrowser.SetControlType(const Value: TControlType);
begin
  // The ControlType cannot be changed
end;

{******************************************}
function TALWebBrowser.HasNativeView: Boolean;
begin
  result := WebBrowserControl.HasNativeView;
end;

{**********************************}
Procedure TALWebBrowser.AddNativeView;
begin
  WebBrowserControl.AddNativeView;
end;

{*************************************}
Procedure TALWebBrowser.RemoveNativeView;
begin
  ResetFocus;
  WebBrowserControl.RemoveNativeView;
end;

{*********************************************}
procedure TALWebBrowser.LoadUrl(const AURL: string);
begin
  WebBrowserControl.LoadUrl(AURL);
end;

{*********************************************}
procedure TALWebBrowser.LoadData(
            const ABaseUrl: String;
            const AData: string;
            const AMimeType: String = '';
            const AEncoding: String = '';
            const AHistoryUrl: String = '');
begin
  WebBrowserControl.LoadData(
    ABaseUrl, // const ABaseUrl: String;
    AData, // const AData: string;
    AMimeType, // const AMimeType: String = '';
    AEncoding, // const AEncoding: String = '';
    AHistoryUrl); // const AHistoryUrl: String = ''
end;

{*********************************************************************}
procedure TALWebBrowser.OnShouldStartLoadUrlImpl(ASender: TObject; const AURL: string; var AAllow: Boolean);
begin
  if assigned(FOnShouldStartLoadUrl) then
    FOnShouldStartLoadUrl(self, AURL, AAllow)
  else
    AAllow := True;
end;

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
  TALIosWebBrowserControl.IsWKWebViewClassRegistered := False;
  {$ENDIF}
  {$IF defined(ALMacOS)}
  TALIosWebBrowserControl.IsWKWebViewClassRegistered := False;
  {$ENDIF}
  RegisterFmxClasses([TALWebBrowser]);

end.
