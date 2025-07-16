unit Alcinoe.AndroidApi.WebKit;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Webkit,
  Androidapi.JNI.Os,
  Androidapi.JNI.Net;

type

  {*****************************}
  JALWebViewListener = interface;
  JALWebView = interface;

  {*********************************************}
  JALWebViewListenerClass = interface(IJavaClass)
    ['{FDC3C9EF-E372-454A-843A-976D51257DF1}']
  end;
  [JavaSignature('io/magicfoundation/alcinoe/webkit/ALWebViewListener')]
  JALWebViewListener = interface(IJavaInstance)
    ['{D6B5E7C2-6C2F-4CCE-A6F2-0D8142850905}']
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
  TJALWebViewListener = class(TJavaGenericImport<JALWebViewListenerClass, JALWebViewListener>) end;

  {****************************************}
  JALWebViewClass = interface(JWebViewClass)
    ['{CB7206B2-5DF9-4270-8963-B4730A1A8ED9}']
    {class} function init(context: JContext): JALWebView; cdecl;
  end;
  [JavaSignature('io/magicfoundation/alcinoe/webkit/ALWebView')]
  JALWebView = interface(JWebView)
    ['{86076845-ABA4-4A86-90DD-D2F51357D6A0}']
    procedure setListener(listener: JALWebViewListener); cdecl;
  end;
  TJALWebView = class(TJavaGenericImport<JALWebViewClass, JALWebView>) end;

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.WebKit.JALWebViewListener', TypeInfo(Alcinoe.AndroidApi.WebKit.JALWebViewListener));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.WebKit.JALWebView', TypeInfo(Alcinoe.AndroidApi.WebKit.JALWebView));
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.AndroidApi.WebKit','initialization');
  {$ENDIF}
  RegisterTypes;

end.
