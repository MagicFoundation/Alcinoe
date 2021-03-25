{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2021 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.WebBrowser.Cocoa;

interface

{$SCOPEDENUMS ON}
/// <summary>
/// Registration service for support native browser
/// </summary>
procedure RegisterWebBrowserService;
/// <summary>
/// Unregistering service
/// </summary>
procedure UnRegisterWebBrowserService;

implementation

uses
{$IFDEF IOS}
  iOSapi.CoreGraphics, FMX.Helpers.iOS, FMX.Platform.iOS, iOSapi.Foundation, iOSapi.WebKit, iOSapi.UIKit,
  FMX.WebBrowser.Delegate.iOS, FMX.ZOrder.iOS,
{$ELSE}
  System.Types, System.Math, Macapi.Foundation, Macapi.CoreGraphics, Macapi.WebKit,
  FMX.Platform.Mac, FMX.WebBrowser.Delegate.Mac, FMX.Helpers.Mac,
{$ENDIF IOS}
  System.Classes, System.SysUtils, System.RTLConsts, System.Net.URLClient,
  Macapi.ObjectiveC, Macapi.Helpers, FMX.Platform, FMX.WebBrowser, FMX.Types, FMX.Forms, FMX.Graphics, FMX.Surfaces;

type
  TCommonWebBrowserService = class;

  TCommonWBService = class(TWBFactoryService)
  protected
    function DoCreateWebBrowser: ICustomBrowser; override;
  end;

{ TCommonWebBrowserService }

  TCommonWebBrowserService = class(TInterfacedObject, ICustomBrowser)
  private
    [Weak] FForm: TCommonCustomForm; // https://quality.embarcadero.com/browse/RSP-24736
    FWebView: INativeWebView;
    FURL: string;
    FWebControl: TCustomWebBrowser;
    FNSCachePolicy: NativeCachePolicy;
    FNavigationDelegate: {$IFDEF IOS}WebNavigationDelegateiOS{$ELSE}WebNavigationDelegate{$ENDIF IOS};
    FUIDelegate: {$IFDEF IOS}WebUIDelegateiOS{$ELSE}WebUIDelegate{$ENDIF IOS};
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function GetParent: TFmxObject;
    function GetVisible: Boolean;
    procedure Show;
    procedure Hide;
    procedure UpdateContentFromControl;
    procedure RootChanged(const aRoot: IRoot); // https://quality.embarcadero.com/browse/RSP-24736
    procedure DoNavigate(const URL: string);
    procedure DoGoBack;
    procedure DoGoForward;
    procedure DoReload;
    procedure DoStop;
    { IFMXWebBrowserService }
    function GetURL: string;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    function GetEnableCaching: Boolean;
    procedure SetEnableCaching(const Value: Boolean);
    procedure SetURL(const AValue: string);
    procedure SetWebBrowserControl(const AValue: TCustomWebBrowser);
    procedure Navigate;
    procedure Reload;
    procedure Stop;
    procedure LoadFromStrings(const Content: string; const BaseUrl: string);
    procedure EvaluateJavaScript(const JavaScript: string);
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
  public
    constructor Create;
    destructor Destroy; override;
    property EnableCaching: Boolean read GetEnableCaching write SetEnableCaching default True;
    property URL: string read GetURL write SetURL;
    property CanGoBack: Boolean read GetCanGoBack;
    property CanGoForward: Boolean read GetCanGoForward;
    function CaptureBitmap: TBitmap;
  end;

var
  WBService: TCommonWBService;

procedure RegisterWebBrowserService;
begin
  WBService := TCommonWBService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXWBService, WBService);
end;

procedure UnregisterWebBrowserService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXWBService);
end;

function TCommonWebBrowserService.GetCanGoBack: Boolean;
begin
  Result := FWebView.canGoBack;
end;

function TCommonWebBrowserService.GetCanGoForward: Boolean;
begin
  Result := FWebView.canGoForward;
end;

function TCommonWebBrowserService.GetEnableCaching: Boolean;
begin
  Result := FNSCachePolicy = NSURLRequestReloadRevalidatingCacheData;
end;

function TCommonWebBrowserService.CaptureBitmap: TBitmap;
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

function TCommonWebBrowserService.GetParent: TFmxObject;
begin
  Result := FWebControl.Parent;
end;

function TCommonWebBrowserService.GetURL: string;
begin
  Result := FURL;
end;

function TCommonWebBrowserService.GetVisible: Boolean;
begin
  Result := False;
  if FWebControl <> nil then
    Result := FWebControl.Visible;
end;

procedure TCommonWebBrowserService.GoBack;
begin
  DoGoBack;
end;

procedure TCommonWebBrowserService.GoForward;
begin
  DoGoForward;
end;

procedure TCommonWebBrowserService.GoHome;
begin

end;

procedure TCommonWebBrowserService.Hide;
begin
  if not FWebView.isHidden then
    FWebView.setHidden(True);
end;

procedure TCommonWebBrowserService.LoadFromStrings(const Content: string; const BaseUrl: string);
var
  LContent: NSString;
  LURL: NSURL;
  LBase: NSString;
begin
  LContent := StrToNSStr(Content);
  LBase := StrToNSStr(baseUrl);
  LURL := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(LBase));
  TNativeWebViewHelper.MainFrame(FWebView).loadHTMLString(LContent, LURL);
  FWebView.setFrame(CGRectMake(0, 0, 1, 1));
  UpdateContentFromControl;
end;

procedure TCommonWebBrowserService.EvaluateJavaScript(const JavaScript: string);
var
  LJavaScript: NSString;
begin
  LJavaScript := StrToNSStr(JavaScript);
  FWebView.evaluateJavaScript(LJavaScript, nil);
end;

procedure TCommonWebBrowserService.Navigate;
begin
  DoNavigate(URL);
end;

function TCommonWebBrowserService.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if Result <> S_OK then
    Result := FWebView.QueryInterface(IID, Obj);
end;

procedure TCommonWebBrowserService.Reload;
begin
  DoReload;
end;

procedure TCommonWebBrowserService.SetEnableCaching(const Value: Boolean);
begin
  if Value then
    FNSCachePolicy := NSURLRequestReloadRevalidatingCacheData
  else
    FNSCachePolicy := NSURLRequestReloadIgnoringLocalCacheData;
end;

procedure TCommonWebBrowserService.SetURL(const AValue: string);
begin
  if FURL <> AValue then
    FURL:= AValue;
end;

procedure TCommonWebBrowserService.SetWebBrowserControl(const AValue: TCustomWebBrowser);
var
  LDelegate: {$IFDEF IOS}TWebViewDelegateiOS{$ELSE}TWebViewDelegate{$ENDIF IOS};
begin
  FWebControl := AValue;
  LDelegate := {$IFDEF IOS}TWebViewDelegateiOS{$ELSE}TWebViewDelegate{$ENDIF IOS}.Create;
  LDelegate.SetWebBrowser(FWebControl, Self);
  FUIDelegate := LDelegate;
  FNavigationDelegate := LDelegate;
  FWebView.setNavigationDelegate((FNavigationDelegate as ILocalObject).GetObjectID);
  FWebView.setUIDelegate((FUIDelegate as ILocalObject).GetObjectID);
end;

procedure TCommonWebBrowserService.Show;
begin
  if FWebView.isHidden then
    FWebView.setHidden(False);
end;

procedure TCommonWebBrowserService.Stop;
begin
  DoStop;
end;

constructor TCommonWebBrowserService.Create;
begin
  FWebView := TNativeWebViewHelper.CreateAndInitWebView;
  FNSCachePolicy := NSURLRequestReloadRevalidatingCacheData;
end;

destructor TCommonWebBrowserService.Destroy;
begin
{$IFDEF IOS}
  if (fForm <> nil) and (fForm.Handle <> nil) and (FWebControl <> nil) then //https://quality.embarcadero.com/browse/RSP-24736
    WindowHandleToPlatform(fForm.Handle).ZOrderManager.RemoveLink(FWebControl); //https://quality.embarcadero.com/browse/RSP-24736
  if FWebView.isLoading then
    FWebView.stopLoading;
{$ENDIF}
  FWebView.setUIDelegate(nil);
  FWebView.setNavigationDelegate(nil);
  FWebView.removeFromSuperview;
  FWebView := nil;
  FUIDelegate := nil;
  FNavigationDelegate := nil;
  inherited;
end;

procedure TCommonWebBrowserService.DoGoBack;
begin
  inherited;
  if FWebView.canGoBack then
  begin
    FWebView.GoBack;
    UpdateContentFromControl;
  end;
end;

procedure TCommonWebBrowserService.DoGoForward;
begin
  inherited;
  if FWebView.canGoForward then
  begin
    FWebView.GoForward;
    UpdateContentFromControl;
  end;
end;

procedure TCommonWebBrowserService.DoNavigate(const URL: string);
const
  cTheTimeoutIntervalForNewRequest = 0;

  function TryGetFileNSUrl(const AURI: TURI; var AUrl: NSUrl): Boolean;
  var
    Dir: string;
    FileName: string;
    Bundle: Pointer;
    Path: NSString;
  begin
    Dir := ExtractFileDir(AURI.Path);
    FileName := ExtractFileName(AURI.Path);
    // URI can have the followed value  "file:///hello.html" . It can refers to file in two locations:
    // 1. In application bundle resources
    // 2. In root of disk.
    // For this case we start looking for this file in application bundle resources and after in global file system.
    Bundle := TNSBundle.OCClass.mainBundle;
    Path := TNSBundle.Wrap(Bundle).pathForResource(StrToNSStr(FileName), nil);
    if Path = nil then
    begin
      Bundle := TNSBundle.OCClass.bundleWithPath(StrToNSStr(Dir));
      Path := TNSBundle.Wrap(Bundle).pathForResource(StrToNSStr(FileName), nil);
    end;
    if Path <> nil then
      AUrl := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(Path))
    else if FileExists(AURI.Path) then
      AUrl := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(StrToNSStr(AURI.Path)))
    else
      AUrl := nil;
    Result := AUrl <> nil;
  end;

var
  NSNewURL: NSURL;
  Request: NSURLRequest;
  Uri: TURI;
  NormalizedUri: string;
begin
  NormalizedUri := TURI.FixupForREST(URL);
  if NormalizedUri.IsEmpty then
    Exit;

  Uri := TURI.Create(NormalizedUri);
  if (Uri.Scheme = 'file') and not TryGetFileNSUrl(Uri, NSNewURL) then
    raise EFileNotFoundException.CreateRes(@SSpecifiedFileNotFound);

  if NSNewURL = nil then
    NSNewURL := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(StrToNSStr(Uri.ToString)));

  Request:= TNSURLRequest.Wrap(TNSURLRequest.OCClass.requestWithURL(NSNewURL, FNSCachePolicy, cTheTimeoutIntervalForNewRequest));
  TNativeWebViewHelper.MainFrame(FWebView).loadRequest(Request);
  FWebView.setFrame(CGRectMake(0, 0, 1, 1));
  UpdateContentFromControl;
end;

procedure TCommonWebBrowserService.DoReload;
begin
  TNativeWebViewHelper.MainFrame(FWebView).reload;
end;

procedure TCommonWebBrowserService.DoStop;
begin
  if FWebView.isLoading then
    TNativeWebViewHelper.MainFrame(FWebView).stopLoading;
end;

procedure TCommonWebBrowserService.UpdateContentFromControl;

{$IFDEF IOS}
  function IsFirstResponder(const AView: INativeView): Boolean;
  var
    I: Integer;
  begin
    if AView.IsFirstResponder then
      Exit(True);

    for I := 0 to AView.subviews.count - 1 do
      if IsFirstResponder(TUIView.Wrap(AView.subviews.objectAtIndex(I))) then
        Exit(True);

    Result := False;
  end;
{$ENDIF}

var
{$IFDEF IOS}
  ZOrderManager: TiOSZOrderManager;
{$ELSE}
  View: INativeView;
  Bounds: TRectF;
{$ENDIF}
begin
  if (FWebControl <> nil) and not (csDesigning in FWebControl.ComponentState) and
     (fForm <> nil) and (fForm.Handle <> nil) then //https://quality.embarcadero.com/browse/RSP-24736
  begin
  {$IFDEF IOS}
    ZOrderManager := WindowHandleToPlatform(fForm.Handle).ZOrderManager; //https://quality.embarcadero.com/browse/RSP-24736
    // ZOrderManager.AddOrSetLink(FWebControl, FWebView, nil); //https://quality.embarcadero.com/browse/RSP-24736
    ZOrderManager.UpdateOrderAndBounds(FWebControl);
    // WKWebView has a bug, It doesn't hide virtual keyboard itself, if we hide view from screen.
    // So we have to do it manually. If WebView is not visible and still have focus, we have to reset it.
    // WKWebView doesn't have directly focus, instead it contains internal focused view, so we have to search focused
    // nested view.
    if not FWebControl.ParentedVisible and IsFirstResponder(FWebView) then
    begin
      // This is the only way to reset the focus. The usage of resignFirstResponder doesn't really reset focus.
      // So we unlink webview from hierarchy and after add it back.
      FWebView.removeFromSuperview;
      ZOrderManager.UpdateOrderAndBounds(FWebControl);
    end;
  {$ELSE}
    Bounds := TRectF.Create(0,0,FWebControl.Width,FWebControl.Height);
    Bounds.Fit(FWebControl.AbsoluteRect);
    View := WindowHandleToPlatform(Form.Handle).View;
    View.addSubview(FWebView);
    if SameValue(Bounds.Width, 0) or SameValue(Bounds.Height, 0) then
      FWebView.setHidden(True)
    else
    begin
      TNativeWebViewHelper.SetBounds(FWebView, Bounds, View.bounds.size.height);
      FWebView.setHidden(not FWebControl.ParentedVisible);
    end;
  {$ENDIF}
  end
  else
    FWebView.setHidden(True);
end;

// https://quality.embarcadero.com/browse/RSP-24736
procedure TCommonWebBrowserService.RootChanged(const aRoot: IRoot);
begin
  {$IFDEF IOS}
  // Changing root for native control means changing ZOrderManager, because one form owns ZOrderManager.
  // So we need to remove itself from old one and add to new one.
  if (fForm <> nil) and (fForm.Handle <> nil) and (FWebControl <> nil) then WindowHandleToPlatform(fForm.Handle).ZOrderManager.RemoveLink(FWebControl);
  {$ENDIF}

  if aRoot is TCommonCustomForm then FForm := TCommonCustomForm(aRoot)
  else FForm := nil;

  {$IFDEF IOS}
  if (fForm <> nil) and (fForm.Handle <> nil) and (FWebControl <> nil) then WindowHandleToPlatform(fForm.Handle).ZOrderManager.AddOrSetLink(FWebControl, FWebView, nil);
  {$ENDIF}
  UpdateContentFromControl;
end;

{ TCommonWBService }

function TCommonWBService.DoCreateWebBrowser: ICustomBrowser;
begin
  Result := TCommonWebBrowserService.Create;
end;

end.
