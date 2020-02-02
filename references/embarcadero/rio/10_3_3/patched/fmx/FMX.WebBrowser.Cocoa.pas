{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2019 Embarcadero Technologies, Inc. }
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
  iOSapi.UIKit, iOSapi.CoreGraphics, FMX.Helpers.iOS, FMX.Platform.iOS, iOSapi.Foundation, FMX.WebBrowser.Delegate.iOS,
  FMX.ZOrder.iOS,
{$ELSE}
  System.UITypes, Macapi.WebKit, Macapi.Foundation, Macapi.CoreGraphics, Macapi.CocoaTypes, Macapi.AppKit,
  FMX.Platform.Mac, FMX.WebBrowser.Delegate.Mac, FMX.Helpers.Mac, FMX.DialogService.Sync, FMX.Dialogs,
{$ENDIF IOS}
  System.Classes, System.Types, System.StrUtils, System.SysUtils, System.IOUtils, System.RTLConsts, System.Math,
  Macapi.ObjectiveC, Macapi.Helpers, FMX.Platform, FMX.WebBrowser, FMX.Types, FMX.Forms, FMX.Graphics, FMX.Surfaces;

type
  TCommonWebBrowserService = class;

  TCommonWBService = class(TWBFactoryService)
  protected
    function DoCreateWebBrowser: ICustomBrowser; override;
  end;

{$IF Defined(MACOS) and not Defined(IOS)}
  // Redeclaring WebPolicyDelegate here so that it has the correct parameter types
  WebPolicyDelegate = interface(IObjectiveC)
    ['{39C410C0-FF04-4A6E-8C69-B73D34A92794}']
    [MethodName('webView:decidePolicyForNavigationAction:request:frame:decisionListener:')]
    procedure webViewDecidePolicyForNavigationActionRequestFrameDecisionListener(WebView: WebView;
      decidePolicyForNavigationAction: NSDictionary; request: NSURLRequest; frame: WebFrame;
      decisionListener: WebPolicyDecisionListener); cdecl;
    [MethodName('webView:decidePolicyForNewWindowAction:request:newFrameName:decisionListener:')]
    procedure webViewDecidePolicyForNewWindowActionRequestNewFrameNameDecisionListener(WebView: WebView;
      decidePolicyForNewWindowAction: NSDictionary; request: NSURLRequest; newFrameName: NSString;
      decisionListener: WebPolicyDecisionListener); cdecl;
    [MethodName('webView:decidePolicyForMIMEType:request:frame:decisionListener:')]
    procedure webViewDecidePolicyForMIMETypeRequestFrameDecisionListener(WebView: WebView;
      decidePolicyForMIMEType: NSString; request: NSURLRequest; frame: WebFrame; decisionListener: WebPolicyDecisionListener); cdecl;
    [MethodName('webView:unableToImplementPolicyWithError:frame:')]
    procedure webViewUnableToImplementPolicyWithErrorFrame(WebView: WebView; unableToImplementPolicyWithError: NSError;
      frame: WebFrame); cdecl;
  end;

  // Redeclaring WebUIDelegate here with just the Javascript methods
  WebUIDelegate = interface(IObjectiveC)
    ['{76F28114-F7E7-4D1C-A77E-F41D116852D7}']
    [MethodName('webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:')]
    procedure webViewRunJavaScriptAlertPanelWithMessageInitiatedByFrame(sender: WebView; runJavaScriptAlertPanelWithMessage: NSString;
      initiatedByFrame: WebFrame); cdecl;
    [MethodName('webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:')]
    function webViewRunJavaScriptConfirmPanelWithMessageInitiatedByFrame(sender: WebView; runJavaScriptConfirmPanelWithMessage: NSString;
      initiatedByFrame: WebFrame): Boolean; cdecl;
    [MethodName('webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:')]
    function webViewRunJavaScriptTextInputPanelWithPromptDefaultTextInitiatedByFrame(sender: WebView;
      runJavaScriptTextInputPanelWithPrompt: NSString; defaultText: NSString; initiatedByFrame: WebFrame): NSString; cdecl;
    [MethodName('webView:runJavaScriptAlertPanelWithMessage:')]
    procedure webViewRunJavaScriptAlertPanelWithMessage(sender: WebView; runJavaScriptAlertPanelWithMessage: NSString); cdecl;
    [MethodName('webView:runJavaScriptConfirmPanelWithMessage:')]
    function webViewRunJavaScriptConfirmPanelWithMessage(sender: WebView; runJavaScriptConfirmPanelWithMessage: NSString): Boolean; cdecl;
    [MethodName('webView:runJavaScriptTextInputPanelWithPrompt:defaultText:')]
    function webViewRunJavaScriptTextInputPanelWithPromptDefaultText(sender: WebView; runJavaScriptTextInputPanelWithPrompt: NSString;
      defaultText: NSString): NSString; cdecl;
  end;

  TWebViewPolicyUIDelegate = class(TOCLocal, WebPolicyDelegate, WebUIDelegate)
  private
    FWebBrowser: TCustomWebBrowser;
    function JavaScriptConfirmationMessage(const AMessage: NSString): Boolean;
    procedure JavaScriptInformationMessage(const AMessage: NSString);
    function JavaScriptInputQuery(const AMessage, ADefaultText: NSString): NSString;
  public
    { WebPolicyDelegate }
    [MethodName('webView:decidePolicyForNavigationAction:request:frame:decisionListener:')]
    procedure webViewDecidePolicyForNavigationActionRequestFrameDecisionListener(WebView: WebView;
      decidePolicyForNavigationAction: NSDictionary; request: NSURLRequest; frame: WebFrame;
      decisionListener: WebPolicyDecisionListener); cdecl;
    [MethodName('webView:decidePolicyForNewWindowAction:request:newFrameName:decisionListener:')]
    procedure webViewDecidePolicyForNewWindowActionRequestNewFrameNameDecisionListener(WebView: WebView;
      decidePolicyForNewWindowAction: NSDictionary; request: NSURLRequest; newFrameName: NSString;
      decisionListener: WebPolicyDecisionListener); cdecl;
    [MethodName('webView:decidePolicyForMIMEType:request:frame:decisionListener:')]
    procedure webViewDecidePolicyForMIMETypeRequestFrameDecisionListener(WebView: WebView;
      decidePolicyForMIMEType: NSString; request: NSURLRequest; frame: WebFrame; decisionListener: WebPolicyDecisionListener); cdecl;
    [MethodName('webView:unableToImplementPolicyWithError:frame:')]
    procedure webViewUnableToImplementPolicyWithErrorFrame(WebView: WebView; unableToImplementPolicyWithError: NSError;
      frame: WebFrame); cdecl;
    { WebUIDelegate }
    [MethodName('webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:')]
    procedure webViewRunJavaScriptAlertPanelWithMessageInitiatedByFrame(sender: WebView; runJavaScriptAlertPanelWithMessage: NSString;
      initiatedByFrame: WebFrame); cdecl;
    [MethodName('webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:')]
    function webViewRunJavaScriptConfirmPanelWithMessageInitiatedByFrame(sender: WebView; runJavaScriptConfirmPanelWithMessage: NSString;
      initiatedByFrame: WebFrame): Boolean; cdecl;
    [MethodName('webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:')]
    function webViewRunJavaScriptTextInputPanelWithPromptDefaultTextInitiatedByFrame(sender: WebView;
      runJavaScriptTextInputPanelWithPrompt: NSString; defaultText: NSString; initiatedByFrame: WebFrame): NSString; cdecl;
    [MethodName('webView:runJavaScriptAlertPanelWithMessage:')]
    procedure webViewRunJavaScriptAlertPanelWithMessage(sender: WebView; runJavaScriptAlertPanelWithMessage: NSString); cdecl;
    [MethodName('webView:runJavaScriptConfirmPanelWithMessage:')]
    function webViewRunJavaScriptConfirmPanelWithMessage(sender: WebView; runJavaScriptConfirmPanelWithMessage: NSString): Boolean; cdecl;
    [MethodName('webView:runJavaScriptTextInputPanelWithPrompt:defaultText:')]
    function webViewRunJavaScriptTextInputPanelWithPromptDefaultText(sender: WebView; runJavaScriptTextInputPanelWithPrompt: NSString;
      defaultText: NSString): NSString; cdecl;
  public
    constructor Create(const ABrowser: TCustomWebBrowser);
  end;
{$ENDIF}

{ TCommonWebBrowserService }

  TCommonWebBrowserService = class(TInterfacedObject, ICustomBrowser)
  private
    [Weak] FForm: TCommonCustomForm; // https://quality.embarcadero.com/browse/RSP-24736
    FWebView: INativeWebView;
    FURL: string;
    FWebControl: TCustomWebBrowser;
    FNSCachePolicy: NativeCachePolicy;
    FDelegate: WebDelegate;
    {$IF Defined(MACOS) and not Defined(IOS)}
    FPolicyDelegate: WebPolicyDelegate;
    FUIDelegate: WebUIDelegate;
    {$ENDIF}
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function GetParent: TFmxObject;
    function GetVisible: Boolean;
    procedure Show;
    procedure Hide;
    procedure PrepareForDestruction;
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
  Result := False;
  if FWebView <> nil then
    Result := FWebView.canGoBack;
end;

function TCommonWebBrowserService.GetCanGoForward: Boolean;
begin
  Result := False;
  if FWebView <> nil then
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
  if (FWebView <> nil) and (not FWebView.isHidden) then
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
  FWebView.stringByEvaluatingJavaScriptFromString(LJavaScript);
end;

procedure TCommonWebBrowserService.Navigate;
begin
  DoNavigate(URL);
end;

procedure TCommonWebBrowserService.PrepareForDestruction;
begin

end;

function TCommonWebBrowserService.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (Result <> S_OK) and (FWebView <> nil) then
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
  LDelegate: TWebViewDelegate;
  {$IF Defined(MACOS) and not Defined(IOS)}
  LPolicyUIDelegate: TWebViewPolicyUIDelegate;
  {$ENDIF}
begin
  FWebControl := AValue;
  LDelegate := TWebViewDelegate.Create;
  LDelegate.SetWebBrowser(FWebControl, Self);
  FDelegate := LDelegate;
  TNativeWebViewHelper.SetDelegate(FWebView, (FDelegate as ILocalObject).GetObjectID);
  {$IF Defined(MACOS) and not Defined(IOS)}
  LPolicyUIDelegate := TWebViewPolicyUIDelegate.Create(FWebControl);
  FPolicyDelegate := LPolicyUIDelegate;
  FUIDelegate := LPolicyUIDelegate;
  FWebView.setPolicyDelegate((FPolicyDelegate as ILocalObject).GetObjectID);
  FWebView.setUIDelegate((FUIDelegate as ILocalObject).GetObjectID);
  {$ENDIF}
end;

procedure TCommonWebBrowserService.Show;
begin
  if (FWebView <> nil) and (FWebView.isHidden) then
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
    FWebView.setDelegate(nil);
  {$ELSE}
    FWebView.setDownloadDelegate(nil);
  {$ENDIF}
  FWebView.removeFromSuperview;
  FWebView := nil;
  FDelegate := nil;
  {$IF Defined(MACOS) and not Defined(IOS)}
  FPolicyDelegate := nil;
  FUIDelegate := nil;
  {$ENDIF}
  inherited;
end;

procedure TCommonWebBrowserService.DoGoBack;
begin
  inherited;
  if (FWebView <> nil) and (FWebView.canGoBack = True)then
  begin
    FWebView.GoBack;
    UpdateContentFromControl;
  end;
end;

procedure TCommonWebBrowserService.DoGoForward;
begin
  inherited;
  if (FWebView <> nil) and (FWebView.canGoForward = True)then
  begin
    FWebView.GoForward;
    UpdateContentFromControl;
  end;
end;

procedure TCommonWebBrowserService.DoNavigate(const URL: string);
const
  cTheTimeoutIntervalForNewRequest = 0;
var
  NewURL: string;
  NSNewURL: NSURL;
  NSNewURLRequest: NSURLRequest;
  LPath: NSString;
  LFilePath: string;
  LDir, LFileName: string;
  Bundle: Pointer;
begin
  NewURL := URL;
  if Pos(TWebBrowser.FilesPref, URL) <> 0 then
  begin
    //extract the file name, the string after file://
    NewURL := Copy(URL, length(TWebBrowser.FilesPref) + 1, length(URL));
    LDir := ExtractFileDir(NewURL);
    LFileName := ExtractFileName(NewURL);
    if LDir.IsEmpty then
      Bundle := TNSBundle.OCClass.mainBundle
    else
      Bundle := TNSBundle.OCClass.bundleWithPath(StrToNSStr(LDir));
    LPath := TNSBundle.Wrap(Bundle).pathForResource(StrToNSStr(LFileName), nil);
    if LPath <> nil then
    begin
      LFilePath := NSStrToStr(LPath);
      NSNewURL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(LPath));
    end
    else
      raise EFileNotFoundException.Create(SSpecifiedFileNotFound);
  end
  else
  begin
    if Pos('http', URL) = 0 then
      Insert('http://', NewURL, 0);
    NSNewURL := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(StrToNSStr(NewURL)));
  end;
  NSNewURLRequest:= TNSURLRequest.Wrap(TNSURLRequest.OCClass.requestWithURL(NSNewURL, FNSCachePolicy,
    cTheTimeoutIntervalForNewRequest));
  TNativeWebViewHelper.MainFrame(FWebView).loadRequest(NSNewURLRequest);
  FWebView.setFrame(CGRectMake(0, 0, 1, 1));
  UpdateContentFromControl;
end;

procedure TCommonWebBrowserService.DoReload;
begin
  if FWebView <> nil then
    TNativeWebViewHelper.MainFrame(FWebView).reload;
end;

procedure TCommonWebBrowserService.DoStop;
begin
  if (FWebView <> nil) and FWebView.isLoading then
    TNativeWebViewHelper.MainFrame(FWebView).stopLoading;
end;

procedure TCommonWebBrowserService.UpdateContentFromControl;
var
  {$IFDEF IOS}
  ZOrderManager: TiOSZOrderManager;
  {$ELSE}
  View: INativeView;
  Bounds: TRectF;
  {$ENDIF}
begin
  if FWebView <> nil then
  begin
    if (FWebControl <> nil) and not (csDesigning in FWebControl.ComponentState) and
       (fForm <> nil) and (fForm.Handle <> nil) then //https://quality.embarcadero.com/browse/RSP-24736
    begin
      {$IFDEF IOS}
      ZOrderManager := WindowHandleToPlatform(fForm.Handle).ZOrderManager; //https://quality.embarcadero.com/browse/RSP-24736
      ZOrderManager.UpdateOrderAndBounds(FWebControl);
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

{$IF Defined(MACOS) and not Defined(IOS)}
{ TWebViewPolicyUIDelegate }

constructor TWebViewPolicyUIDelegate.Create(const ABrowser: TCustomWebBrowser);
begin
  inherited Create;
  FWebBrowser := ABrowser;
end;

procedure TWebViewPolicyUIDelegate.JavaScriptInformationMessage(const AMessage: NSString);
begin
  TDialogServiceSync.MessageDialog(NSStrToStr(AMessage), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
end;

function TWebViewPolicyUIDelegate.JavaScriptConfirmationMessage(const AMessage: NSString): Boolean;
begin
  Result := TDialogServiceSync.MessageDialog(NSStrToStr(AMessage), TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0) = mrYes;
end;

function TWebViewPolicyUIDelegate.JavaScriptInputQuery(const AMessage, ADefaultText: NSString): NSString;
var
  LValues: TArray<string>;
begin
  SetLength(LValues, 1);
  LValues[0] := NSStrToStr(ADefaultText);
  if TDialogServiceSync.InputQuery(string.Empty, [NSStrToStr(AMessage)], LValues) then
    Result := StrToNSStr(LValues[0])
  else
    Result := StrToNSStr(string.Empty);
end;

procedure TWebViewPolicyUIDelegate.webViewDecidePolicyForMIMETypeRequestFrameDecisionListener(WebView: WebView; decidePolicyForMIMEType: NSString;
  request: NSURLRequest; frame: WebFrame; decisionListener: WebPolicyDecisionListener);
begin
  decisionListener.use;
end;

procedure TWebViewPolicyUIDelegate.webViewDecidePolicyForNavigationActionRequestFrameDecisionListener(WebView: WebView;
  decidePolicyForNavigationAction: NSDictionary; request: NSURLRequest; frame: WebFrame; decisionListener: WebPolicyDecisionListener);
begin
  FWebBrowser.ShouldStartLoading(NSUrlToStr(request.URL));
  decisionListener.use;
end;

procedure TWebViewPolicyUIDelegate.webViewDecidePolicyForNewWindowActionRequestNewFrameNameDecisionListener(WebView: WebView;
  decidePolicyForNewWindowAction: NSDictionary; request: NSURLRequest; newFrameName: NSString; decisionListener: WebPolicyDecisionListener);
begin
  decisionListener.use;
end;

procedure TWebViewPolicyUIDelegate.webViewUnableToImplementPolicyWithErrorFrame(WebView: WebView; unableToImplementPolicyWithError: NSError;
  frame: WebFrame);
begin
  // Not implemented
end;

procedure TWebViewPolicyUIDelegate.webViewRunJavaScriptAlertPanelWithMessage(sender: WebView; runJavaScriptAlertPanelWithMessage: NSString);
begin
  JavaScriptInformationMessage(runJavaScriptAlertPanelWithMessage);
end;

procedure TWebViewPolicyUIDelegate.webViewRunJavaScriptAlertPanelWithMessageInitiatedByFrame(sender: WebView; runJavaScriptAlertPanelWithMessage: NSString;
  initiatedByFrame: WebFrame);
begin
  JavaScriptInformationMessage(runJavaScriptAlertPanelWithMessage);
end;

function TWebViewPolicyUIDelegate.webViewRunJavaScriptConfirmPanelWithMessage(sender: WebView; runJavaScriptConfirmPanelWithMessage: NSString): Boolean;
begin
  Result := JavaScriptConfirmationMessage(runJavaScriptConfirmPanelWithMessage);
end;

function TWebViewPolicyUIDelegate.webViewRunJavaScriptConfirmPanelWithMessageInitiatedByFrame(sender: WebView; runJavaScriptConfirmPanelWithMessage: NSString;
  initiatedByFrame: WebFrame): Boolean;
begin
  Result := JavaScriptConfirmationMessage(runJavaScriptConfirmPanelWithMessage);
end;

function TWebViewPolicyUIDelegate.webViewRunJavaScriptTextInputPanelWithPromptDefaultText(sender: WebView; runJavaScriptTextInputPanelWithPrompt,
  defaultText: NSString): NSString;
begin
  Result := JavaScriptInputQuery(runJavaScriptTextInputPanelWithPrompt, defaultText);
end;

function TWebViewPolicyUIDelegate.webViewRunJavaScriptTextInputPanelWithPromptDefaultTextInitiatedByFrame(sender: WebView;
  runJavaScriptTextInputPanelWithPrompt, defaultText: NSString; initiatedByFrame: WebFrame): NSString;
begin
  Result := JavaScriptInputQuery(runJavaScriptTextInputPanelWithPrompt, defaultText);
end;
{$ENDIF}

end.
