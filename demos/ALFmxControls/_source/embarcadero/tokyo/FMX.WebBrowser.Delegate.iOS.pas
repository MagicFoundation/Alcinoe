{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.WebBrowser.Delegate.iOS;

interface

{$SCOPEDENUMS ON}

uses
  Macapi.ObjectiveC, FMX.WebBrowser, iOSapi.UIKit, iOSApi.Foundation, System.Types;

type
  /// <summary>Wrapper for UIWebView</summary>
  INativeWebView = UIWebView;
  /// <summary>Wrapper for UIView</summary>
  INativeView = UIView;
  /// <summary>Wrapper for UIWebView</summary>
  IMainFrame = UIWebView;
  /// <summary>Wrapper for NSURLRequestCachePolicy</summary>
  NativeCachePolicy = NSURLRequestCachePolicy;
  /// <summary>Wrapper for UIWebViewDelegate</summary>
  WebDelegate = UIWebViewDelegate;

{ TWebViewDelegate }
  /// <summary> Class delegate for reciving call backs from native UIWebView</summary>
  TWebViewDelegate = class(TOCLocal, UIWebViewDelegate)
  private
    [weak] FWebBrowser: TCustomWebBrowser;
    [weak] FURLSetter: ICustomBrowser;
  public
    /// <summary>Setter for callback's receiver</summary>
    procedure SetWebBrowser(const AWebBrowser: TCustomWebBrowser; const AURLSetter: ICustomBrowser);
    /// <summary>Return a pointer to a native delegate</summary>
    constructor Create;
    procedure webView(webView: UIWebView; didFailLoadWithError: NSError); overload; cdecl;
    function webView(webView: UIWebView; shouldStartLoadWithRequest: NSURLRequest; navigationType: UIWebViewNavigationType): Boolean; overload; cdecl;
    procedure webViewDidFinishLoad(webView: UIWebView); cdecl;
    procedure webViewDidStartLoad(webView: UIWebView); cdecl;
  end;

  /// <summary>The class for the automation of work with native UIWebView</summary>
  TNativeWebViewHelper = class
  public
    /// <summary>Class function for init UIWebView with predefined params</summary>
    class function CreateAndInitWebView: UIWebView;
    /// <summary>Class function for getting working frame</summary>
    class function MainFrame(const ANativeWebView: INativeWebView): IMainFrame; inline;
    /// <summary> Setter of native delegate</summary>
    class procedure SetDelegate(const ANativeWebView: INativeWebView; const ADelegate: Pointer);
    /// <summary> Class procedure for setting Bounds/// </summary>
    class procedure SetBounds(const ANativeWebView: INativeWebView; const ABounds: TRectF; const AHeight: Single = 0);
  end;

implementation

uses
  FMX.Surfaces, Macapi.Helpers;

{ TWebViewDelegate }

procedure TWebViewDelegate.webView(webView: UIWebView; didFailLoadWithError: NSError);
begin
  if FWebBrowser <> nil then
    FWebBrowser.FailLoadingWithError;
end;

constructor TWebViewDelegate.Create;
begin
  inherited Create;
end;

procedure TWebViewDelegate.SetWebBrowser(const AWebBrowser: TCustomWebBrowser; const AURLSetter: ICustomBrowser);
begin
  FWebBrowser := AWebBrowser;
  FURLSetter := AURLSetter;
end;

function TWebViewDelegate.webView(webView: UIWebView; shouldStartLoadWithRequest: NSURLRequest;
  navigationType: UIWebViewNavigationType): Boolean;
begin
  if FWebBrowser <> nil then
  begin
    FURLSetter.SetURL(NSStrToStr(shouldStartLoadWithRequest.URL.absoluteString));
    FWebBrowser.ShouldStartLoading(FWebBrowser.URL);
  end;
  Result := True;
end;

procedure TWebViewDelegate.webViewDidFinishLoad(webView: UIWebView);
var
  LRequest : NSURLRequest;
begin
  if webView <> nil then
  begin
    LRequest := webView.request;
    if LRequest <> nil then
      FURLSetter.SetURL(NSStrToStr(LRequest.mainDocumentURL.absoluteString));
  end;
  if FWebBrowser <> nil then
    FWebBrowser.FinishLoading;
end;

procedure TWebViewDelegate.webViewDidStartLoad(webView: UIWebView);
begin
  if FWebBrowser <> nil then
    FWebBrowser.StartLoading;
end;

{ TNativeWVHelper }

class function TNativeWebViewHelper.CreateAndInitWebView: UIWebView;
begin
  Result := TUIWebView.Create;
  Result.setScalesPageToFit(True);
end;

class function TNativeWebViewHelper.MainFrame(const ANativeWebView: INativeWebView): IMainFrame;
begin
  Result := ANativeWebView;
end;

class procedure TNativeWebViewHelper.SetBounds(const ANativeWebView: INativeWebView; const ABounds: TRectF;
  const AHeight: Single);
begin
  ANativeWebView.setFrame(CGRectFromRect(TRectF.Create(ABounds.Left, ABounds.Bottom, ABounds.Right, ABounds.Top)));
end;

class procedure TNativeWebViewHelper.SetDelegate(const ANativeWebView: INativeWebView; const ADelegate: Pointer);
begin
  ANativeWebView.setDelegate(ADelegate);
end;

end.
