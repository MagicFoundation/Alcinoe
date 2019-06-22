{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2018 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.WebBrowser.Win;

interface

{$SCOPEDENUMS ON}

procedure RegisterWebBrowserService;
procedure UnRegisterWebBrowserService;

implementation

uses
  Winapi.Windows, System.Classes, System.Win.InternetExplorer, System.UITypes, System.Types, System.Win.IEInterfaces,
  System.SysUtils, System.RTLConsts, Winapi.Messages, Winapi.ActiveX, FMX.Platform, FMX.WebBrowser, FMX.Graphics,
  FMX.Types, FMX.Platform.Win, FMX.Forms, FMX.Controls.Ole, FMX.Surfaces, FMX.Controls.Win, FMX.ZOrder.Win, FMX.Utils;

type
  TWindowsWBService = class(TWBFactoryService)
  protected
    function DoCreateWebBrowser: ICustomBrowser; override;
  end;

  { TWindowsWebBrowserService }

  TWindowsWebBrowserService = class;

  TWinWBMediator = class(TInterfacedObject, ICustomBrowser)
  private
    FBrowser: TWindowsWebBrowserService;
  public
    constructor Create;
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    property WB: TWindowsWebBrowserService read FBrowser implements ICustomBrowser;
  end;

  TWindowsWebBrowserService = class(TOLEFrameworkDelegate, ICustomBrowser)
  private
    FCache: Boolean;
    FCanGoBack: Boolean;
    FCanGoForward: Boolean;
    FInstance: TOleWebBrowser;
    FUrl: string;
    FWebControl: TCustomWebBrowser;
    procedure InitBrowser;
    function CaptureBitmap: TBitmap;
    procedure WBCommandStateChange(Sender: TObject; Command: Integer; Enable: WordBool);
    procedure WebBrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL, Flags, TargetFrameName,
      PostData, Headers: OleVariant; var Cancel: WordBool);
    procedure WebBrowserNavigateError(ASender: TObject; const pDisp: IDispatch; const URL, Frame,
      StatusCode: OleVariant; var Cancel: WordBool);
    procedure WebBrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
  public
    function GetURL: string;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    procedure SetURL(const AValue: string);
    function GetEnableCaching: Boolean;
    procedure SetEnableCaching(const Value: Boolean);
    procedure SetWebBrowserControl(const AValue: TCustomWebBrowser);
    function GetVisible: Boolean;
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
    procedure Show; override;
    procedure Hide; override;
    procedure PrepareForDestruction;
    property URL: string read GetURL write SetURL;
    property EnableCaching: Boolean read GetEnableCaching write SetEnableCaching;
    property CanGoBack: Boolean read GetCanGoBack;
    property CanGoForward: Boolean read GetCanGoForward;
    procedure ReleaseInstance;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    procedure SetVisible(const Value: Boolean); override;
    procedure DoSetFocus; override;
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultHandler(var Message); override;
  end;

  TVCLObjectController = class(TInterfacedObject, IVCLComObject)
  private
    FComponent: TComponent;
  protected
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    procedure FreeOnRelease;
  public
    constructor Create(Component: TComponent);
    destructor Destroy; override;
  end;

type
  TWinBitmap = record
    BitmapInfo: TBitmapInfo;
    DC: HDC;
    Bitmap: HBITMAP;
    Bits: PAlphaColorRecArray;
    Width, Height: Integer;
  end;

function CreateWinBitmap(const Width, Height: Integer): TWinBitmap;
begin
  Result.Width := Width;
  Result.Height := Height;
  Result.DC := CreateCompatibleDC(0);
  ZeroMemory(@(Result.BitmapInfo.bmiHeader), SizeOf(TBitmapInfoHeader));
  Result.BitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
  Result.BitmapInfo.bmiHeader.biWidth := Width;
  Result.BitmapInfo.bmiHeader.biHeight := -Height;
  Result.BitmapInfo.bmiHeader.biPlanes := 1;
  Result.BitmapInfo.bmiHeader.biCompression := BI_RGB;
  Result.BitmapInfo.bmiHeader.biBitCount := 32;
  Result.Bitmap := CreateDIBSection(Result.DC, Result.BitmapInfo, DIB_RGB_COLORS, Pointer(Result.Bits), 0, 0);
  SetMapMode(Result.DC, MM_TEXT);
  SelectObject(Result.DC, Result.Bitmap);
end;

procedure DestroyWinBitmap(var Bitmap: TWinBitmap);
begin
  Bitmap.Bits := nil;
  DeleteObject(Bitmap.Bitmap);
  DeleteDC(Bitmap.DC);
end;

var
  WBService: TWindowsWBService;

procedure RegisterWebBrowserService;
begin
  WBService := TWindowsWBService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXWBService, WBService);
end;

procedure UnRegisterWebBrowserService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXWBService);
end;

{ TWindowsWebBrowserService }

function TWindowsWebBrowserService.CaptureBitmap: TBitmap;

  function CreateBitmapFromWinBitmap(const AWBitMap: TWinBitmap): TBitmap;
  var
    BSurface: TBitmapSurface;
    I, J: Integer;
  begin
    Result := TBitmap.Create;
    BSurface := TBitmapSurface.Create;
    try
      BSurface.SetSize(AWBitMap.Width, AWBitMap.Height);
      for I := 0 to AWBitMap.Height - 1 do
        for J := 0 to AWBitMap.Width - 1 do
          BSurface.Pixels[J, I] := AWBitMap.Bits^[I * AWBitMap.Width + J].Color;
      Result.Assign(BSurface);
    finally
      BSurface.Free;
    end;
  end;

var
  ViewObject: IViewObject;
  SourceDrawRect: TRect;
  WBitmap: TWinBitmap;
begin
  Result := nil;
  if FInstance.Document <> nil then
  begin
    FInstance.Document.QueryInterface(IViewObject, ViewObject);
    if ViewObject <> nil then
    begin
      SourceDrawRect := Rect(0, 0, Trunc(FWebControl.Size.Width), Trunc(FWebControl.Size.Height));
      WBitmap := CreateWinBitmap(SourceDrawRect.Width, SourceDrawRect.Height);
      try
        ViewObject.Draw(DVASPECT_CONTENT, 1, nil, nil, Self.Handle, WBitmap.DC, @SourceDrawRect, nil, nil, 0);
        Result := CreateBitmapFromWinBitmap(WBitmap);
      finally
        DestroyWinBitmap(WBitmap);
      end;
    end;
  end;
end;

constructor TWindowsWebBrowserService.Create(AOwner: TComponent);
begin
  inherited;
  TVCLObjectController.Create(Self);
  InitBrowser;
end;

procedure TWindowsWebBrowserService.CreateHandle;
var
  Form: TCommonCustomForm;
  ZOrderManager: TWinZOrderManager;
begin
  inherited;
  Form := TCommonCustomForm(FWebControl.Root);
  if Form <> nil then
  begin
    ZOrderManager := WindowHandleToPlatform(Form.Handle).ZOrderManager;
    ZOrderManager.AddOrSetLink(FWebControl, WindowHandle, 0);
  end;
end;

procedure TWindowsWebBrowserService.EvaluateJavaScript(const JavaScript: string);
var
  LDoc: IHTMLDocument2;
  LHTMLWindow: IHTMLWindow2;
begin
  LDoc := FInstance.Document as IHTMLDocument2;
  if not Assigned(LDoc) then
    Exit;
  LHTMLWindow := LDoc.parentWindow;
  if not Assigned(LHTMLWindow) then
    Exit;
  LHTMLWindow.execScript(JavaScript, 'JavaScript');
end;

procedure TWindowsWebBrowserService.DefaultHandler(var Message);
var
  LMessage: TMessage;
begin
  if HandleAllocated and (FInstance.MiscStatus and OLEMISC_SIMPLEFRAME = 0) then
  begin
    LMessage := TMessage(Message);
    LMessage.Result := CallWindowProc(DefWndProc, Handle, LMessage.Msg, LMessage.WParam, LMessage.LParam);
  end
  else
    inherited DefaultHandler(Message);
end;

destructor TWindowsWebBrowserService.Destroy;
begin
  if (FWebControl <> nil) and (FWebControl.Root is TCommonCustomForm) then
    WindowHandleToPlatform(TCommonCustomForm(FWebControl.Root).Handle).ZOrderManager.RemoveLink(FWebControl);
  inherited;
end;

procedure TWindowsWebBrowserService.DestroyHandle;
var
  Form: TCommonCustomForm;
  ZOrderManager: TWinZOrderManager;
begin
  Form := TCommonCustomForm(FWebControl.Root);
  if Form <> nil then
  begin
    ZOrderManager := WindowHandleToPlatform(Form.Handle).ZOrderManager;
    ZOrderManager.RemoveLink(FWebControl);
  end;
  inherited;
end;

procedure TWindowsWebBrowserService.DoSetFocus;
begin
  inherited;
  FWebControl.SetFocus;
end;

function TWindowsWebBrowserService.GetCanGoBack: Boolean;
begin
  Result := False;
  if FInstance <> nil then
    Result := FCanGoBack;
end;

function TWindowsWebBrowserService.GetCanGoForward: Boolean;
begin
  Result := False;
  if FInstance <> nil then
    Result := FCanGoForward;
end;

function TWindowsWebBrowserService.GetEnableCaching: Boolean;
begin
  Result := FCache;
end;

function TWindowsWebBrowserService.GetURL: string;
begin
  Result := FUrl;
end;

function TWindowsWebBrowserService.GetVisible: Boolean;
begin
  Result := False;
  if FInstance <> nil then
    Result := FInstance.Visible;
end;

procedure TWindowsWebBrowserService.GoBack;
begin
  if FInstance <> nil then
    FInstance.GoBack;
end;

procedure TWindowsWebBrowserService.GoForward;
begin
  if FInstance <> nil then
    FInstance.GoForward;
end;

procedure TWindowsWebBrowserService.GoHome;
begin
  if FInstance <> nil then
    FInstance.GoHome;
end;

procedure TWindowsWebBrowserService.Hide;
begin
  if FInstance <> nil then
    FInstance.Visible := False;
end;

procedure TWindowsWebBrowserService.InitBrowser;
begin
  if FInstance = nil then
  begin
    FInstance := TOleWebBrowser.Create(Self);
    FInstance.OnCommandStateChange := WBCommandStateChange;
    FInstance.OnBeforeNavigate2 := WebBrowserBeforeNavigate2;
    FInstance.OnNavigateError := WebBrowserNavigateError;
    FInstance.OnNavigateComplete2 := WebBrowserNavigateComplete2;
    FInstance.Silent := True;
  end;
end;

procedure TWindowsWebBrowserService.LoadFromStrings(const Content, BaseUrl: string);

  procedure LoadDocumentFromStream(const Stream: TStream);
  const
    CBlankPage: string= 'about:blank'; // do not localize
  var
    PersistStreamInit: IPersistStreamInit;
    StreamAdapter: IStream;
  begin
    if FInstance.Document = nil then
      FInstance.Navigate(CBlankPage);
    if FInstance.Document.QueryInterface(IPersistStreamInit, PersistStreamInit) = S_OK then
    begin
      if PersistStreamInit.InitNew = S_OK then
      begin
        StreamAdapter:= TStreamAdapter.Create(Stream);
        PersistStreamInit.Load(StreamAdapter);
      end;
    end;
  end;

var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(Content);
  try
    LoadDocumentFromStream(StringStream);
  finally
    StringStream.Free;
  end;
end;

procedure TWindowsWebBrowserService.Navigate;
var
  Flags: OleVariant;
  NewURL: string;
begin
  Flags := 0;
  if not FCache then
    Flags := Flags or navNoReadFromCache or navNoWriteToCache;

  if Pos(TWebBrowser.FilesPref, URL) <> 0 then
  begin
    NewURL := Copy(URL, length(TWebBrowser.FilesPref) + 1, length(URL));
    if not FileExists(NewURL) then
      raise EFileNotFoundException.Create(SSpecifiedFileNotFound);
  end
  else
    NewURL := URL;
  FInstance.Navigate(NewURL, Flags);
end;

procedure TWindowsWebBrowserService.PrepareForDestruction;
begin

end;

function TWindowsWebBrowserService.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (FInstance <> nil) and (Result <> S_OK) then
    Result := FInstance.ControlInterface.QueryInterface(IID, Obj);
end;

procedure TWindowsWebBrowserService.ReleaseInstance;
begin
  if (FWebControl <> nil) and (FWebControl.Root is TCommonCustomForm) then
    WindowHandleToPlatform(TCommonCustomForm(FWebControl.Root).Handle).ZOrderManager.RemoveLink(FWebControl);
  FInstance.Free;
end;

procedure TWindowsWebBrowserService.Reload;
begin
  if FInstance <> nil then
    FInstance.Refresh;
  Self.Parent := FWebControl.Parent;
end;

procedure TWindowsWebBrowserService.SetEnableCaching(const Value: Boolean);
begin
  FCache := Value;
end;

procedure TWindowsWebBrowserService.SetURL(const AValue: string);
begin
  if FUrl <> AValue then
    FUrl := AValue;
end;

procedure TWindowsWebBrowserService.SetVisible(const Value: Boolean);
begin
  inherited;
  FInstance.Visible := Self.Visible;
end;

procedure TWindowsWebBrowserService.SetWebBrowserControl(const AValue: TCustomWebBrowser);
begin
  FWebControl := AValue;
  InitBrowser;
end;

procedure TWindowsWebBrowserService.Show;
begin
  if FInstance <> nil then
    FInstance.Visible := True;
end;

procedure TWindowsWebBrowserService.Stop;
begin
  if FInstance <> nil then
    FInstance.Stop;
end;

procedure TWindowsWebBrowserService.UpdateContentFromControl;
var
  ZOrderManager: TWinZOrderManager;
  Form: TCommonCustomForm;
begin
  if FWebControl.Parent <> nil then
    Self.Parent := FWebControl.Parent;
  if (FWebControl.GetParentComponent <> nil) then
  begin
    if FInstance = nil then
      InitBrowser;
    FInstance.Visible := FWebControl.Visible and FWebControl.ParentedVisible;

    Form := TCommonCustomForm(FWebControl.Root);
    if Form <> nil then
    begin
      ZOrderManager := WindowHandleToPlatform(Form.Handle).ZOrderManager;
      ZOrderManager.UpdateOrderAndBounds(FWebControl);
    end;
  end;
end;

// https://quality.embarcadero.com/browse/RSP-24736
procedure TWindowsWebBrowserService.RootChanged(const aRoot: IRoot);
begin
end;

procedure TWindowsWebBrowserService.WBCommandStateChange(Sender: TObject; Command: Integer; Enable: WordBool);
begin
  case Command of
    CSC_NAVIGATEBACK: FCanGoBack := Enable;
    CSC_NAVIGATEFORWARD: FCanGoForward := Enable;
  end;
end;

procedure TWindowsWebBrowserService.WebBrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL,
  Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
begin
  FUrl := URL;
  FWebControl.StartLoading;
end;

procedure TWindowsWebBrowserService.WebBrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch;
  const URL: OleVariant);
begin
  FUrl := URL;
  FWebControl.FinishLoading;
end;

procedure TWindowsWebBrowserService.WebBrowserNavigateError(ASender: TObject; const pDisp: IDispatch; const URL, Frame,
  StatusCode: OleVariant; var Cancel: WordBool);
begin
  FWebControl.FailLoadingWithError;
end;

{ TWindowsWBService }

function TWindowsWBService.DoCreateWebBrowser: ICustomBrowser;
begin
  Result := TWinWBMediator.Create;
end;

{ TVCLObjectController }

constructor TVCLObjectController.Create(Component: TComponent);
begin
  inherited Create;
  FComponent := Component;
  FComponent.VCLComObject := Pointer(IVCLComObject(Self));
end;

destructor TVCLObjectController.Destroy;
begin
  if Assigned(FComponent) then
    FComponent.Free;
  inherited;
end;

procedure TVCLObjectController.FreeOnRelease;
begin

end;

function TVCLObjectController.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TVCLObjectController.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TVCLObjectController.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TVCLObjectController.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

{ TWinWBMediator }

constructor TWinWBMediator.Create;
begin
  FBrowser := TWindowsWebBrowserService.Create(nil);
end;

destructor TWinWBMediator.Destroy;
begin
  FBrowser.ReleaseInstance;
  inherited;
end;

function TWinWBMediator.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (FBrowser <> nil) and (Result <> S_OK) then
    Result := FBrowser.QueryInterface(IID, Obj);
end;

end.
