{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2012-2018 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.WebBrowser;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Messaging, FMX.Types, FMX.Controls, FMX.Graphics;

type
  EWebManagerException = class(Exception);

  TCustomWebBrowser = class;

  TWebBrowserDidStartLoad = procedure(ASender: TObject) of object;
  TWebBrowserDidFinishLoad = procedure(ASender: TObject) of object;
  TWebBrowserDidFailLoadWithError = procedure(ASender: TObject) of object;
  TWebBrowserShouldStartLoadWithRequest = procedure(ASender: TObject; const URL: string) of object;

{ IFMXWebBrowserService }

  ICustomBrowser = interface(IInterface)
  ['{A5BB2E8C-6D53-4FF3-BC38-2299285F07BD}']
    function GetURL: string;
    function CaptureBitmap: TBitmap;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    procedure SetURL(const AValue: string);
    function GetEnableCaching: Boolean;
    procedure SetEnableCaching(const Value : Boolean);
    procedure SetWebBrowserControl(const AValue: TCustomWebBrowser);
    function GetParent: TFmxObject;
    function GetVisible : Boolean;
    procedure UpdateContentFromControl;
    procedure RootChanged(const aRoot: IRoot); // https://quality.embarcadero.com/browse/RSP-24736
    procedure Navigate;
    procedure Reload;
    procedure Stop;
    procedure EvaluateJavaScript(const JavaScript: string);
    procedure LoadFromStrings(const Content: string; const BaseUrl: string);
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
    procedure Show;
    procedure Hide;
    /// <summary>
    ///   Method should be called before destruction of native control
    /// </summary>
    procedure PrepareForDestruction;                                           
    property URL: string read GetURL write SetURL;
    property EnableCaching: Boolean read GetEnableCaching write SetEnableCaching;
    property CanGoBack: Boolean read GetCanGoBack;
    property CanGoForward: Boolean read GetCanGoForward;
  end;

{ IFMXWBService }

  IFMXWBService = interface (IInterface)
    ['{61BC1956-BAA0-4B07-971D-EF40064FBD07}']
    function CreateWebBrowser: ICustomBrowser;
    procedure RealignBrowsers;
    procedure DestroyWebBrowser(const AWebBrowser: ICustomBrowser);
  end;

  { TCustomWebBrowser }

  TCustomWebBrowser = class(TControl)
  private
    procedure FormHandleCreated(const Sender: TObject; const Msg: TMessage);
  private
    FWeb: ICustomBrowser;
    FURL: string;
    FEnableCaching: Boolean;
    FOnDidStartLoad: TWebBrowserDidStartLoad;
    FOnDidFinishLoad: TWebBRowserDidFinishLoad;
    FOnDidFailLoadWithError: TWebBrowserdidFailLoadWithError;
    FOnShouldStartLoadWithRequest: TWebBrowsershouldStartLoadWithRequest;
    FSavedVisible: Boolean;
    function GetURL: string;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    procedure SetURL(const Value: string);
    procedure UpdateContent;
    function GetEnableCaching: Boolean;
    procedure SetEnableCaching(const Value: Boolean);
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure ParentChanged; override;
    procedure DoAbsoluteChanged; override;
    procedure DoRootChanged; override; // https://quality.embarcadero.com/browse/RSP-24736
    procedure Move; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure Show; override;
    procedure Hide; override;
    procedure ChangeOrder; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // getters and setters for web browser events
    function GetOnDidStartLoad: TWebBrowserDidStartLoad;
    procedure SetOnDidStartLoad(const AEvent: TWebBrowserDidStartLoad);
    function GetOnDidFinishLoad: TWebBrowserDidFinishLoad;
    procedure SetOnDidFinishLoad(const AEvent: TWebBrowserDidFinishLoad);
    function GetOnShouldStartLoadWithRequest: TWebBrowserShouldStartLoadWithRequest;
    procedure SetOnShouldStartLoadWithRequest(const AEvent: TWebBrowserShouldStartLoadWithRequest);
    function GetOnDidFailLoadWithError: TWebBrowserDidFailLoadWithError;
    procedure SetOnDidFailLoadWithError(const AEvent: TWebBrowserDidFailLoadWithError);
    procedure SetVisible(const Value: Boolean); override;
  public
    const FilesPref = 'file://';
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartLoading;
    procedure FinishLoading;
    procedure FailLoadingWithError;
    procedure ShouldStartLoading(const URL: string);
    function CaptureBitmap: TBitmap;
    procedure GoBack;
    procedure GoForward;
    procedure Navigate; overload;
    procedure Navigate(const AURL: string); overload;
    procedure Reload; overload;
    procedure Stop;
    procedure LoadFromStrings(const Content: string; const BaseUrl: string);
    procedure EvaluateJavaScript(const JavaScript: string);
    property EnableCaching: Boolean read GetEnableCaching write SetEnableCaching default True;
    property URL: string read GetURL write SetURL;
    property CanGoBack: Boolean read GetCanGoBack default False;
    property CanGoForward: Boolean read GetCanGoForward default False;

    property OnDidStartLoad: TWebBrowserDidStartLoad read GetOnDidStartLoad write SetOnDidStartLoad;
    property OnDidFinishLoad: TWebBrowserDidFinishLoad read GetOnDidFinishLoad write SetOnDidFinishLoad;
    property OnShouldStartLoadWithRequest: TWebBrowserShouldStartLoadWithRequest read GetOnShouldStartLoadWithRequest
      write SetOnShouldStartLoadWithRequest;
    property OnDidFailLoadWithError: TWebBrowserDidFailLoadWithError read GetOnDidFailLoadWithError
      write SetOnDidFailLoadWithError;
  end;

{ TWBFactoryService }

  TWBFactoryService = class abstract (TInterfacedObject, IFMXWBService)
  private
    procedure FormActivateHandler(const Sender: TObject; const M: TMessage);
  protected
    FWebBrowsers: TList<ICustomBrowser>;
    function DoCreateWebBrowser: ICustomBrowser; virtual; abstract;
    procedure DoRemoveWebBrowser(const AWebBrowser: ICustomBrowser);
    procedure RealignBrowsers;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateWebBrowser: ICustomBrowser;
    procedure DestroyWebBrowser(const AWebBrowser: ICustomBrowser);
  end;

{ TWebBrowser }
  TWebBrowser = class(TCustomWebBrowser)
  published
    property EnableCaching default True;
    property Align;
    property Anchors;
    property Height;
    property Size;
    property Margins;
    property Position;
    property URL;
    property Visible default True;
    property CanFocus default True;
    property Width;
    property OnDidStartLoad;
    property OnDidFinishLoad;
    property OnDidFailLoadWithError;
    property OnShouldStartLoadWithRequest;
  end;

implementation

uses
  System.Types,
{$IFDEF ANDROID}
  FMX.WebBrowser.Android,
{$ENDIF ANDROID}
{$IF DEFINED(IOS) OR DEFINED(MACOS)}
  FMX.WebBrowser.Cocoa,
{$ENDIF}
{$IFDEF MSWINDOWS}
  FMX.WebBrowser.Win,
{$ENDIF MSWINDOWS}
  FMX.Platform, FMX.Forms;

{ TCustomWebBrowser }

procedure TCustomWebBrowser.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  if FWeb <> nil then
    FWeb.UpdateContentFromControl;
end;

function TCustomWebBrowser.CaptureBitmap: TBitmap;
begin
  if FWeb <> nil then
    Result := FWeb.CaptureBitmap
  else
    Result := nil;
end;

constructor TCustomWebBrowser.Create(AOwner: TComponent);
var
  WBService : IFMXWBService;
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, FormHandleCreated);
  CanFocus := True;
  FSavedVisible := True;
  EnableCaching := True;
  if not (csDesigning in ComponentState) and TPlatformServices.Current.SupportsPlatformService(IFMXWBService, WBService) then
  begin
    FWeb := WBService.CreateWebBrowser;
    FWeb.SetWebBrowserControl(Self);
  end;
end;

destructor TCustomWebBrowser.Destroy;
var
  WBService : IFMXWBService;
begin
  if not (csDesigning in ComponentState) and TPlatformServices.Current.SupportsPlatformService(IFMXWBService, WBService) then
    WBService.DestroyWebBrowser(FWeb);
  FWeb := nil;

  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, FormHandleCreated);
  inherited;
end;

procedure TCustomWebBrowser.DoAbsoluteChanged;
begin
  inherited;
  if FWeb <> nil then
    FWeb.UpdateContentFromControl;
end;

//https://quality.embarcadero.com/browse/RSP-24736
procedure TCustomWebBrowser.DoRootChanged;
begin
  inherited;
  if FWeb <> nil then
    FWeb.RootChanged(root);
end;

procedure TCustomWebBrowser.FailLoadingWithError;
begin
  if Assigned(FOnDidFailLoadWithError) then
    FOnDidFailLoadWithError(Self);
end;

procedure TCustomWebBrowser.FinishLoading;
begin
  if Assigned(FOnDidFinishLoad) then
    FOnDidFinishLoad(Self);
end;

procedure TCustomWebBrowser.FormHandleCreated(const Sender: TObject; const Msg: TMessage);

  function GetParentForm(Control: TFmxObject): TCommonCustomForm;
  begin
    if (Control.Root <> nil) and (Control.Root.GetObject is TCommonCustomForm) then
      Result := TCommonCustomForm(Control.Root.GetObject)
    else
      Result := nil;
  end;

var
  WBService : IFMXWBService;
begin
  if not (csDesigning in ComponentState) and ((FWeb = nil) or (Sender = GetParentForm(self as TFmxObject))) and
    TPlatformServices.Current.SupportsPlatformService(IFMXWBService, WBService) then
  begin
    if FWeb <> nil then
      WBService.DestroyWebBrowser(FWeb);
    FWeb := WBService.CreateWebBrowser;
    FWeb.SetWebBrowserControl(Self);
    FWeb.UpdateContentFromControl;
    FWeb.URL := FURL;
    FWeb.Navigate;
  end;
end;

function TCustomWebBrowser.GetCanGoBack: Boolean;
begin
  Result:= False;
  if FWeb <> nil then
    Result:= FWeb.CanGoBack;
end;

function TCustomWebBrowser.GetCanGoForward: Boolean;
begin
  Result:= False;
  if FWeb <> nil then
    Result:= FWeb.CanGoForward;
end;

function TCustomWebBrowser.GetEnableCaching: Boolean;
begin
  Result := FEnableCaching;
  if FWeb <> nil then
    Result := FWeb.EnableCaching;
end;

function TCustomWebBrowser.GetOnDidFailLoadWithError: TWebBrowserdidFailLoadWithError;
begin
  Result := FOnDidFailLoadWithError;
end;

function TCustomWebBrowser.GetOnDidFinishLoad: TWebBrowserDidFinishLoad;
begin
  Result := FOnDidFinishLoad;
end;

function TCustomWebBrowser.GetOnDidStartLoad: TWebBrowserDidStartLoad;
begin
  Result := FOnDidStartLoad;
end;

function TCustomWebBrowser.GetOnShouldStartLoadWithRequest: TWebBrowserShouldStartLoadWithRequest;
begin
  Result := FOnShouldStartLoadWithRequest;
end;

function TCustomWebBrowser.GetURL: string;
begin
  if (csDesigning in ComponentState) or (FWeb = nil) then
    Result := FURL
  else
    Result := FWeb.URL;
end;

procedure TCustomWebBrowser.GoBack;
begin
  if (FWeb <> nil) and FWeb.CanGoBack then
    FWeb.GoBack;
end;

procedure TCustomWebBrowser.GoForward;
begin
  if (FWeb <> nil) and FWeb.CanGoForward then
    FWeb.GoForward;
end;

procedure TCustomWebBrowser.ChangeOrder;
begin
  inherited;
  UpdateContent;
end;

procedure TCustomWebBrowser.Hide;
begin
  inherited;
  UpdateContent;
end;

procedure TCustomWebBrowser.LoadFromStrings(const Content: string; const BaseUrl: string);
begin
  if FWeb <> nil then
    FWeb.LoadFromStrings(Content, BaseUrl);
end;

procedure TCustomWebBrowser.EvaluateJavaScript(const JavaScript: string);
begin
  if FWeb <> nil then
    FWeb.EvaluateJavaScript(JavaScript);
end;

procedure TCustomWebBrowser.Move;
begin
  inherited;
  UpdateContent;
end;

procedure TCustomWebBrowser.Navigate(const AURL: string);
begin
  if FWeb <> nil then
    SetUrl(AURL);
end;

procedure TCustomWebBrowser.Navigate;
begin
  if FWeb <> nil then
    FWeb.Navigate;
end;

procedure TCustomWebBrowser.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

procedure TCustomWebBrowser.Paint;
var
  R, DstR: TRectF;
  Bitmap: TBitmap;
begin
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
  if not (csDesigning in ComponentState) and FInPaintTo then
  begin
    Bitmap := CaptureBitmap;
    if Bitmap <> nil then
    begin
      R := TRectF.Create(0, 0, Bitmap.Width, Bitmap.Height);
      DstR := TRectF.Create(0, 0, Size.Width, Size.Height);
      Canvas.DrawBitmap(Bitmap, R, DstR, 1);
    end;
  end;
end;

procedure TCustomWebBrowser.ParentChanged;
begin
  inherited;
  if ([csLoading, csDesigning] * ComponentState) = [] then
  begin
    if HasParent then
      Visible := FSavedVisible
    else
    begin
      FSavedVisible := Visible;
      Visible := False;
    end;
    UpdateContent;
  end;
end;

function TCustomWebBrowser.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (FWeb <> nil) and (Result <> S_OK) then
    Result := FWeb.QueryInterface(IID, Obj);
end;

procedure TCustomWebBrowser.Reload;
begin
  inherited;
  if FWeb <> nil then
    FWeb.Reload;
end;

procedure TCustomWebBrowser.Resize;
begin
  inherited;
  UpdateContent;
end;

procedure TCustomWebBrowser.SetEnableCaching(const Value: Boolean);
begin
  if FEnableCaching <> Value then
  begin
    FEnableCaching := Value;
    if not(csDesigning in ComponentState) and (FWeb <> nil) and (FWeb.EnableCaching <> Value) then
      FWeb.EnableCaching := FEnableCaching;
  end;
end;

procedure TCustomWebBrowser.SetOnDidFailLoadWithError(
  const AEvent: TWebBrowserdidFailLoadWithError);
begin
  FOnDidFailLoadWithError := AEvent;
end;

procedure TCustomWebBrowser.SetOnDidFinishLoad(const AEvent: TWebBrowserDidFinishLoad);
begin
  FOnDidFinishLoad := AEvent;
end;

procedure TCustomWebBrowser.SetOnDidStartLoad(const AEvent: TWebBrowserDidStartLoad);
begin
  FOnDidStartLoad := AEvent;
end;

procedure TCustomWebBrowser.SetOnShouldStartLoadWithRequest(const AEvent: TWebBrowserShouldStartLoadWithRequest);
begin
  FOnShouldStartLoadWithRequest := AEvent;
end;

procedure TCustomWebBrowser.SetURL(const Value: string);
begin
  if FURL <> Value then
    FURL:= Value;
  if not(csDesigning in ComponentState) then
  begin
    if FWeb <> nil then
      FWeb.URL := Value;
    Navigate;
  end;
end;

procedure TCustomWebBrowser.SetVisible(const Value: Boolean);
begin
  inherited;
end;

procedure TCustomWebBrowser.ShouldStartLoading(const URL: string);
begin
  if Assigned(FOnShouldStartLoadWithRequest) then
    FOnShouldStartLoadWithRequest(Self, URL);
end;

procedure TCustomWebBrowser.Show;
begin
  inherited;
  UpdateContent;
end;

procedure TCustomWebBrowser.StartLoading;
begin
  if Assigned(FOnDidStartLoad) then
    FOnDidStartLoad(Self);
end;

procedure TCustomWebBrowser.Stop;
begin
  if FWeb <> nil then
    FWeb.Stop;
end;

procedure TCustomWebBrowser.UpdateContent;
begin
  if FWeb <> nil then
    FWeb.UpdateContentFromControl;
end;

{ TWBFactoryService }

constructor TWBFactoryService.Create;
begin
  inherited Create;
  FWebBrowsers := TList<ICustomBrowser>.Create;
  TMessageManager.DefaultManager.SubscribeToMessage(TFormActivateMessage, FormActivateHandler);
end;

function TWBFactoryService.CreateWebBrowser: ICustomBrowser;
begin
  Result := DoCreateWebBrowser;
  FWebBrowsers.Add(Result);
end;

destructor TWBFactoryService.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TFormActivateMessage, FormActivateHandler);
  FWebBrowsers.Free;
  inherited Destroy;
end;

procedure TWBFactoryService.DestroyWebBrowser(const AWebBrowser: ICustomBrowser);
begin
  DoRemoveWebBrowser(AWebBrowser);
end;

procedure TWBFactoryService.DoRemoveWebBrowser(const AWebBrowser: ICustomBrowser);
begin
  if (FWebBrowsers <> nil) and (AWebBrowser <> nil) then
  begin
    AWebBrowser.PrepareForDestruction;
    FWebBrowsers.Remove(AWebBrowser);
  end;
end;

procedure TWBFactoryService.FormActivateHandler(const Sender: TObject; const M: TMessage);
begin
  RealignBrowsers;
end;

procedure TWBFactoryService.RealignBrowsers;
var
  WebBrowser : ICustomBrowser;
begin
  for WebBrowser in FWebBrowsers do
    WebBrowser.UpdateContentFromControl;
end;

initialization
  RegisterWebBrowserService;
  RegisterFmxClasses([TWebBrowser]);

end.
