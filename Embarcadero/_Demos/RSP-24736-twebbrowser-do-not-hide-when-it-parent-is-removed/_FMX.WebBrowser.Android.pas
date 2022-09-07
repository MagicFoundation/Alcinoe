{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2021 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.WebBrowser.Android;

interface

{$SCOPEDENUMS ON}

procedure RegisterWebBrowserService;
procedure UnRegisterWebBrowserService;

implementation

uses
  System.Classes, System.Types, System.StrUtils, System.SysUtils,  System.RTLConsts, System.Net.URLClient,
  Androidapi.JNI.Webkit, AndroidApi.JNI.App, Androidapi.JNI.Embarcadero, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes, Androidapi.JNIBridge, Androidapi.JNI.Os, Androidapi.JNI.Net, Androidapi.Helpers,
  Androidapi.JNI.Widget, FMX.Forms, FMX.Helpers.Android, FMX.Graphics, FMX.Surfaces, FMX.ZOrder.Android, FMX.Platform,
  FMX.Platform.Android, FMX.WebBrowser, FMX.Types;

type
  TAndroidWBService = class(TWBFactoryService)
  protected
    function DoCreateWebBrowser: ICustomBrowser; override;
  end;

{ TAndroidWebBrowserService }

  TAndroidWebBrowserService = class(TInterfacedObject, ICustomBrowser)
  private
    type
      TWebBrowserListener = class(TJavaLocal,JOnWebViewListener)
      private
        [Weak] FWBService: TAndroidWebBrowserService;
      public
        constructor Create(const AWBService: TAndroidWebBrowserService);
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

      TFocusChangeListener = class(TJavaLocal, JView_OnFocusChangeListener)
      private
        [Weak] FService: TAndroidWebBrowserService;
      public
        constructor Create(const Service: TAndroidWebBrowserService);
        procedure onFocusChange(view: JView; hasFocus: Boolean); cdecl;
      end;

  private
    FListener: TWebBrowserListener;
    FFocusChangeListener: TFocusChangeListener;
    FWebView: JWebBrowser;
    FWebViewContainer: JViewGroup;
    FChildrenContainer: JViewGroup;
    FURL: string;
    [Weak] FWebControl: TCustomWebBrowser;
    [Weak] FForm: TCommonCustomForm; // https://quality.embarcadero.com/browse/RSP-24736
    function GetZOrderManager: TAndroidZOrderManager;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function GetParent: TFmxObject;
    function GetVisible: Boolean;
    procedure Show;
    procedure Hide;
    procedure UpdateContentFromControl;
    procedure RootChanged(const aRoot: IRoot); // https://quality.embarcadero.com/browse/RSP-24736
    procedure DoNavigate(const AURL: string);
    procedure DoReload;
    { IFMXWebBrowserService }
    function GetURL: string;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    procedure SetURL(const AValue: string);
    function GetEnableCaching: Boolean;
    procedure SetEnableCaching(const AValue : Boolean);
    procedure SetWebBrowserControl(const AValue: TCustomWebBrowser);
    procedure Navigate;
    procedure LoadFromStrings(const AContent: string; const ABaseUrl: string);
    procedure Reload;
    procedure Stop;
    procedure EvaluateJavaScript(const AJavaScript: string);
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;

    procedure StartLoading;
    procedure FinishLoading;
    procedure FailLoadingWithError;
    procedure ShouldStartLoading(const AURL: string);
  public
    constructor Create;
    destructor Destroy; override;
    function CaptureBitmap: TBitmap;

    property EnableCaching: Boolean read GetEnableCaching write SetEnableCaching;
    property URL: string read GetURL write SetURL;
    property CanGoBack: Boolean read GetCanGoBack;
    property CanGoForward: Boolean read GetCanGoForward;
    property ZOrderManager: TAndroidZOrderManager read GetZOrderManager;
  end;

var
  WBService: TAndroidWBService;

procedure RegisterWebBrowserService;
begin
  WBService := TAndroidWBService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXWBService, WBService);
end;

procedure UnregisterWebBrowserService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXWBService);
end;

function TAndroidWebBrowserService.GetCanGoBack: Boolean;
begin
  Result := FWebView.canGoBack;
end;

function TAndroidWebBrowserService.GetCanGoForward: Boolean;
begin
  Result := FWebView.canGoForward;
end;

function TAndroidWebBrowserService.GetEnableCaching: Boolean;
begin
  Result := FWebView.getSettings.getCacheMode = TJWebSettings.JavaClass.LOAD_CACHE_ELSE_NETWORK;
end;

function TAndroidWebBrowserService.GetParent: TFmxObject;
begin
  Result := FWebControl.Parent;
end;

function TAndroidWebBrowserService.GetURL: string;
begin
  Result := FURL;
end;

function TAndroidWebBrowserService.GetVisible: Boolean;
begin
  Result := FWebControl.Visible;
end;

//https://quality.embarcadero.com/browse/RSP-24736
function TAndroidWebBrowserService.GetZOrderManager: TAndroidZOrderManager;
begin
  if (fForm <> nil) and (fForm.Handle <> nil) then
  begin
    Result := WindowHandleToPlatform(fForm.Handle).ZOrderManager
  end
  else
    Result := nil;
end;

procedure TAndroidWebBrowserService.GoBack;
begin
  FWebView.goBack;
end;

procedure TAndroidWebBrowserService.GoForward;
begin
  FWebView.goForward;
end;

procedure TAndroidWebBrowserService.GoHome;
begin

end;

procedure TAndroidWebBrowserService.Hide;
begin
  FWebViewContainer.setVisibility(TJView.JavaClass.INVISIBLE);
end;

procedure TAndroidWebBrowserService.LoadFromStrings(const AContent: string; const ABaseUrl: string);
begin
  FWebView.loadDataWithBaseURL(StringToJString(ABaseUrl), StringToJString(AContent), nil, nil, nil);
  UpdateContentFromControl;
end;

procedure TAndroidWebBrowserService.EvaluateJavaScript(const AJavaScript: string);
begin
  FWebView.loadUrl(StringToJString('javascript:' + AJavaScript));
  UpdateContentFromControl;
end;

procedure TAndroidWebBrowserService.Navigate;
begin
  DoNavigate(URL);
end;

function TAndroidWebBrowserService.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (Result <> S_OK) and (FWebView <> nil) then
    Result := FWebView.QueryInterface(IID, Obj);
end;

procedure TAndroidWebBrowserService.Reload;
begin
  DoReload;
end;

procedure TAndroidWebBrowserService.SetEnableCaching(const AValue: Boolean);
var
  CacheMode: Integer;
begin
  if AValue then
    CacheMode := TJWebSettings.JavaClass.LOAD_CACHE_ELSE_NETWORK
  else
    CacheMode := TJWebSettings.JavaClass.LOAD_NO_CACHE;
  FWebView.getSettings.setCacheMode(CacheMode);
end;

procedure TAndroidWebBrowserService.SetURL(const AValue: string);
begin
  FURL := AValue;
end;

procedure TAndroidWebBrowserService.SetWebBrowserControl(const AValue: TCustomWebBrowser);
begin
  FWebControl := AValue;
  FWebView.setFocusable(FWebControl.CanFocus);
  UpdateContentFromControl;
end;

procedure TAndroidWebBrowserService.ShouldStartLoading(const AURL: string);
begin
  FWebControl.ShouldStartLoading(AURL);
end;

procedure TAndroidWebBrowserService.Show;
begin
  FWebViewContainer.setVisibility(TJView.JavaClass.VISIBLE);
end;

procedure TAndroidWebBrowserService.StartLoading;
begin
  FWebControl.StartLoading;
end;

procedure TAndroidWebBrowserService.Stop;
begin
  FWebView.stopLoading;
end;

function TAndroidWebBrowserService.CaptureBitmap: TBitmap;
var
  Surface: TBitmapSurface;
begin
  Result := nil;
  Surface := TBitmapSurface.Create;
  try
    if NativeViewToSurface(FWebView, Surface) then
    begin
      Result := TBitmap.Create;
      Result.Assign(Surface);
    end;
  finally
    Surface.Free;
  end;
end;

constructor TAndroidWebBrowserService.Create;
var
  LayoutParams: JRelativeLayout_LayoutParams;
begin
  FWebView := TJWebBrowser.JavaClass.init(TAndroidHelper.Activity);
  FWebView.getSettings.setJavaScriptEnabled(True);
  FListener := TWebBrowserListener.Create(Self);
  FWebView.SetWebViewListener(FListener);
  FFocusChangeListener := TFocusChangeListener.Create(Self);
  FWebView.setOnFocusChangeListener(FFocusChangeListener);
  FWebView.getSettings.setGeolocationEnabled(True);
  FWebView.getSettings.setAppCacheEnabled(True);
  FWebView.getSettings.setDatabaseEnabled(True);
  FWebView.getSettings.setDomStorageEnabled(True);
  FWebView.getSettings.setBuiltInZoomControls(True);
  FWebView.getSettings.setDisplayZoomControls(False);
  FWebView.getSettings.setAllowFileAccessFromFileURLs(True);
  FWebView.getSettings.setAllowUniversalAccessFromFileURLs(True);
  FWebView.getSettings.setMediaPlaybackRequiresUserGesture(False);

  FWebViewContainer := TJRelativeLayout.JavaClass.init(TAndroidHelper.Context);
  FChildrenContainer := TJRelativeLayout.JavaClass.init(TAndroidHelper.Context);
  LayoutParams := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT, TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
  FWebViewContainer.addView(FWebView, LayoutParams);
  LayoutParams := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT, TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
  FWebViewContainer.addView(FChildrenContainer, LayoutParams);

  SetEnableCaching(True);
end;

procedure TAndroidWebBrowserService.DoNavigate(const AURL: string);
var
  Uri: TURI;
  NormalizedUri: string;
begin
  NormalizedUri := TURI.FixupForREST(AUrl);
  if NormalizedUri.IsEmpty then
    Exit;

  Uri := TURI.Create(NormalizedUri);
  if (Uri.Scheme = 'file') and not FileExists(Uri.Path) then
    raise EFileNotFoundException.CreateRes(@SSpecifiedFileNotFound);
  FWebView.loadUrl(StringToJString(Uri.ToString));
  ShouldStartLoading(Uri.ToString);
  UpdateContentFromControl;
end;

procedure TAndroidWebBrowserService.DoReload;
begin
  FWebView.reload;
end;

procedure TAndroidWebBrowserService.FailLoadingWithError;
begin
  FWebControl.FailLoadingWithError;
end;

procedure TAndroidWebBrowserService.FinishLoading;
begin
  FWebControl.FinishLoading;
end;

destructor TAndroidWebBrowserService.Destroy;
begin
  if (ZOrderManager <> nil) and (FWebControl <> nil) then // https://quality.embarcadero.com/browse/RSP-24736
    ZOrderManager.RemoveLink(FWebControl);

  FWebView.SetWebViewListener(nil);
  inherited;
end;

procedure TAndroidWebBrowserService.UpdateContentFromControl;
begin
  if (FWebControl <> nil) and (ZOrderManager <> nil) then
  begin
    // https://quality.embarcadero.com/browse/RSP-24736
    // ZOrderManager.AddOrSetLink(FWebControl, FWebViewContainer, FChildrenContainer);
    ZOrderManager.UpdateOrderAndBounds(FWebControl);
  end
  else
    Hide;
end;

// https://quality.embarcadero.com/browse/RSP-24736
procedure TAndroidWebBrowserService.RootChanged(const aRoot: IRoot);
begin
  // Changing root for native control means changing ZOrderManager, because one form owns ZOrderManager.
  // So we need to remove itself from old one and add to new one.
  if (ZOrderManager <> nil) and (FWebControl <> nil) then ZOrderManager.RemoveLink(FWebControl);

  if aRoot is TCommonCustomForm then FForm := TCommonCustomForm(aRoot)
  else FForm := nil;

  if (ZOrderManager <> nil) and (FWebControl <> nil) then ZOrderManager.AddOrSetLink(FWebControl, FWebViewContainer, FChildrenContainer);
  UpdateContentFromControl;
end;

{ TAndroidWBService }

function TAndroidWBService.DoCreateWebBrowser: ICustomBrowser;
begin
  Result := TAndroidWebBrowserService.Create;
end;

{ TAndroidWebBrowserService.TWebBrowserListener }

constructor TAndroidWebBrowserService.TWebBrowserListener.Create(const AWBService: TAndroidWebBrowserService);
begin
  inherited Create;
  FWBService := AWBService;
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.doUpdateVisitedHistory(view: JWebView; url: JString;
  isReload: Boolean);
begin
  // Nothing
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onFormResubmission(view: JWebView; dontResend, resend: JMessage);
begin
  // Nothing
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onLoadResource(view: JWebView; url: JString);
begin
  // Nothing
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onPageFinished(view: JWebView; url: JString);
begin
  FWBService.FURL := JStringToString(url);
  FWBService.FinishLoading;
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onPageStarted(view: JWebView; url: JString; favicon: JBitmap);
begin
  FWBService.FURL := JStringToString(url);
  FWBService.StartLoading;
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onReceivedError(view: JWebView; errorCode: Integer; description,
  failingUrl: JString);
begin
  FWBService.FailLoadingWithError;
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onReceivedHttpAuthRequest(view: JWebView; handler: JHttpAuthHandler;
  host, realm: JString);
begin
  // Nothing
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onReceivedSslError(view: JWebView; handler: JSslErrorHandler;
  error: JSslError);
begin
  // Ignores expired SSL vertificate.
  handler.proceed;
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onScaleChanged(view: JWebView; oldScale, newScale: Single);
begin
  // Nothing
end;

procedure TAndroidWebBrowserService.TWebBrowserListener.onUnhandledKeyEvent(view: JWebView; event: JKeyEvent);
begin
  // Nothing
end;

function TAndroidWebBrowserService.TWebBrowserListener.shouldOverrideKeyEvent(view: JWebView; event: JKeyEvent): Boolean;
begin
  Result := False;
end;

function TAndroidWebBrowserService.TWebBrowserListener.shouldOverrideUrlLoading(view: JWebView; url: JString): Boolean;
begin
  FWBService.ShouldStartLoading(JStringToString(url));
  Result := False;
end;

{ TAndroidWebBrowserService.TFocusChangeListener }

constructor TAndroidWebBrowserService.TFocusChangeListener.Create(const Service: TAndroidWebBrowserService);
begin
  inherited Create;
  FService := Service;
end;

procedure TAndroidWebBrowserService.TFocusChangeListener.onFocusChange(view: JView; hasFocus: Boolean);
begin
  if hasFocus and FService.FWebControl.CanFocus then
    FService.FWebControl.SetFocus
  else
    FService.FWebControl.ResetFocus;
end;

end.

