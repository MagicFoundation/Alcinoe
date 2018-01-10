{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Screen.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.TypInfo, System.Generics.Collections, Macapi.ObjectiveC, iOSapi.UIKit, iOSapi.Foundation,
  FMX.Platform, FMX.Types;

type

  TCocoaTouchScreenServices = class;

  IFMXInterfaceOrientationChanged = interface(NSObject)
  ['{AAF2DF0E-C4BF-4057-8C5F-1B8A832BC39E}']
    procedure DeviceOrientationChanged; cdecl;
  end;

  TInterfaceOrientationChangedListener = class(TOCLocal)
  private
    [Weak] FScreenService: TCocoaTouchScreenServices;
  protected
    { TOCLocal }
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create(const AService: TCocoaTouchScreenServices);
    destructor Destroy; override;
    { IFMXInterfaceOrientationChanged }
    procedure DeviceOrientationChanged; cdecl;
  end;

  /// <summary>Implementations of screen's services: <c>IFMXMultiDisplayService</c>, <c>IFMXScreenService</c>,
  ///  <c>IFMXDeviceMetricsService</c></summary>
  TCocoaTouchScreenServices = class(TInterfacedObject, IFMXMultiDisplayService, IFMXScreenService,
    IFMXDeviceMetricsService)
  private
    FMainScreen: UIScreen;
    FDisplayCount: Integer;
    FWorkAreaRect: TRect;
    FDesktopRect: TRect;
    FDisplayList: TList<TDisplay>;
    FOrientationListener: TInterfaceOrientationChangedListener;
    function CGRectToRect(const ACGRect: NSRect): TRect;
    procedure UpdateDisplays;
    function FindDisplay(const screen: UIScreen): TDisplay;
  protected
    /// <summary>Registers screen's services in the platform</summary>
    procedure RegisterServices; virtual;
    /// <summary>Unregisters screen's services</summary>
    procedure UnregisterServices; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXMultiDisplayService }
    /// <summary>Refreshes current information about all displays in system</summary>
    procedure UpdateDisplayInformation;
    /// <summary>Returns count of displays in system</summary>
    function GetDisplayCount: Integer;
    /// <summary>Declares the method that tries to return a rectangle having the specified Size and positioned
    /// in the center of the virtual screen.</summary>
    function GetDesktopCenterRect(const Size: TSize): TRect;
    /// <summary>Returns region on main screen allocated for showing application</summary>
    function GetWorkAreaRect: TRect;
    /// <summary>Returns union of all displays</summary>
    function GetDesktopRect: TRect;
    /// <summary>Returns display information by specified display index</summary>
    function GetDisplay(const AIndex: Integer): TDisplay;
    /// <summary>Returns display, which is related with specified form <c>AHandle</c></summary>
    function DisplayFromWindow(const AHandle: TWindowHandle): TDisplay;
    /// <summary>Returns display, which is related with specified form <c>AHandle</c> and <c>APoint</c></summary>
    /// <remarks>Form can be placed on two displays at the same time (on a border of two displays), but not on iOS.
    /// So <c>APoint</c> parameter is not supported on iOS. It works like a <c>DisplayFromWindow</c></remarks>
    function DisplayFromPoint(const AHandle: TWindowHandle; const APoint: TPoint): TDisplay;
    { IFMXScreenService }
    /// <summary>Returns logical size of main screen</summary>
    function GetScreenSize: TPointF;
    /// <summary>Returns scale of main screen</summary>
    function GetScreenScale: Single;
    /// <summary>Returns screen orientation</summary>
    function GetScreenOrientation: TScreenOrientation;
    /// <summary>Allows to set prefered screen orientation</summary>
    /// <remarks>Not needed for iOS</remarks>
    procedure SetScreenOrientation(AOrientations: TScreenOrientations);
    { IFMXDeviceMetricsService }
    /// <summary>Returns metrics of current display (physical and logical screen size, scale, aspect ration,
    /// DPI and etc)</summary>
    function GetDisplayMetrics: TDeviceDisplayMetrics;
  public
    /// <summary>Returns <c>UIScreen.mainScreen</c></summary>
    property MainScreen: UIScreen read FMainScreen;
  end;

implementation

uses
  System.Classes, System.SysUtils, System.Math, System.RTLConsts, Macapi.Helpers, Macapi.ObjCRuntime, FMX.Platform.iOS,
  FMX.Forms, FMX.Helpers.iOS, FMX.Consts;

{ TCocoaScreenServices }

procedure TCocoaTouchScreenServices.UnregisterServices;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXMultiDisplayService);
  TPlatformServices.Current.RemovePlatformService(IFMXScreenService);
  TPlatformServices.Current.RemovePlatformService(IFMXDeviceMetricsService);
end;

procedure TCocoaTouchScreenServices.UpdateDisplayInformation;
begin
  FDisplayCount := 0;
  FWorkAreaRect := TRect.Empty;
  FDesktopRect := TRect.Empty;
  FreeAndNil(FDisplayList);
end;

function TCocoaTouchScreenServices.GetDisplayCount: Integer;
begin
  if FDisplayCount = 0 then
    FDisplayCount := TUIScreen.OCClass.screens.count;
  Result := FDisplayCount;
end;

function TCocoaTouchScreenServices.GetDisplayMetrics: TDeviceDisplayMetrics;
const
  IOSBasePPI = 163;
var
  ScreenSize: TPointF;
  ScreenScale: Single;
begin
  ScreenSize := GetScreenSize;
  ScreenScale := GetScreenScale;
  Result.PhysicalScreenSize := TSize.Create(Round(ScreenSize.X * ScreenScale), Round(ScreenSize.Y * ScreenScale));
  Result.RawScreenSize := Result.PhysicalScreenSize;
  Result.LogicalScreenSize := TSize.Create(Round(ScreenSize.X), Round(ScreenSize.Y));
  if Abs(ScreenSize.X) > 0 then
    Result.AspectRatio := ScreenSize.Y / ScreenSize.X
  else
    Result.AspectRatio := 1;
  Result.PixelsPerInch := Round(IOSBasePPI * ScreenScale);
  Result.ScreenScale := ScreenScale;
  Result.FontScale := ScreenScale;
end;

function TCocoaTouchScreenServices.GetDesktopCenterRect(const Size: TSize): TRect;
var
  DesktopCenter: TPoint;
begin
  DesktopCenter := GetWorkAreaRect.CenterPoint;
  Result := TRect.Create(TPoint.Create(DesktopCenter.X - Size.cx div 2, DesktopCenter.Y - Size.cy div 2), Size.cx,
    Size.cy);
end;

function TCocoaTouchScreenServices.GetDesktopRect: TRect;
var
  I: Integer;
begin
  if FDesktopRect.IsEmpty then
  begin
    FDesktopRect := TRect.Empty;
    for I := 0 to GetDisplayCount - 1 do
      FDesktopRect.Union(GetDisplay(I).BoundsRect);
  end;
  Result := FDesktopRect;
end;

function TCocoaTouchScreenServices.CGRectToRect(const ACGRect: NSRect): TRect;
var
  LSize: NSSize;
begin
  if TOSVersion.Check(8) then
    Result := TRect.Create(TPoint.Create(Round(ACGRect.origin.x), Round(ACGRect.origin.y)), Round(ACGRect.size.width),
      Round(ACGRect.size.height))
  else
    case GetScreenOrientation of
      TScreenOrientation.Portrait:
      begin
        Result := TRect.Create(TPoint.Create(Round(ACGRect.origin.x), Round(ACGRect.origin.y)), Round(ACGRect.size.width),
          Round(ACGRect.size.height));
      end;
      TScreenOrientation.Landscape:
      begin
        Result := TRect.Create(TPoint.Create(Round(ACGRect.origin.y),
          Round(ACGRect.origin.x)), Round(ACGRect.size.height), Round(ACGRect.size.width));
      end;
      TScreenOrientation.InvertedPortrait:
      begin
        LSize := MainScreen.bounds.size;
        Result := TRect.Create(TPoint.Create(Round(ACGRect.origin.x),
          Round(LSize.height - ACGRect.origin.y - ACGRect.size.height)), Round(ACGRect.size.width),
          Round(ACGRect.size.height));
      end;
      TScreenOrientation.InvertedLandscape:
      begin
        LSize := MainScreen.bounds.size;
        Result := TRect.Create(TPoint.Create(Round(ACGRect.origin.y),
          Round(LSize.width - ACGRect.origin.x - ACGRect.size.width)), Round(ACGRect.size.height),
          Round(ACGRect.size.width));
      end;
    end;
end;

procedure TCocoaTouchScreenServices.UpdateDisplays;
  procedure AddInfo(const I: Integer);
  var
    LUIScreen: UIScreen;
  begin
    LUIScreen := TUIScreen.Wrap(TUIScreen.OCClass.screens.objectAtIndex(I));
    FDisplayList.Add(TDisplay.Create(I, I = 0, CGRectToRect(LUIScreen.bounds),
      CGRectToRect(LUIScreen.applicationFrame)));
  end;
var
  I: Integer;
begin
  UpdateDisplayInformation;
  FDisplayCount := TUIScreen.OCClass.screens.count;
  if FDisplayList = nil then
    FDisplayList := TList<TDisplay>.Create
  else
    FDisplayList.Clear;
  for I := 0 to FDisplayCount - 1 do
    AddInfo(I);
end;

function TCocoaTouchScreenServices.FindDisplay(const screen: UIScreen): TDisplay;
  function DoFind(const R: TRect): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    if FDisplayList <> nil then
      for I := 0 to FDisplayList.Count - 1 do
        if R = FDisplayList[I].BoundsRect then
          Exit(I);
  end;
var
  Index: Integer;
  R: TRect;
begin
  if screen = nil then
    raise EInvalidFmxHandle.Create(sArgumentInvalid);
  R := CGRectToRect(screen.bounds);
  Index := DoFind(R);
  if Index = -1 then
  begin
    UpdateDisplays;
    Index := DoFind(R);
  end;
  if Index = -1 then
    raise EInvalidArgument.Create(sArgumentInvalid)
  else
    Result := FDisplayList[Index];
end;

function TCocoaTouchScreenServices.DisplayFromWindow(const AHandle: TWindowHandle): TDisplay;

  function IsPopupForm(const Form: TCommonCustomForm): Boolean;
  begin
    Result := (Form <> nil) and ((Form.FormStyle = TFormStyle.Popup) or (Form is TCustomPopupForm));
  end;

var
  Wnd: TiOSWindowHandle;
  ParentForm: TCommonCustomForm;
begin
  if AHandle = nil then
    raise EArgumentNilException.Create(SArgumentNil);
  Wnd := WindowHandleToPlatform(AHandle);
  if IsPopupForm(Wnd.Form) then
  begin
    ParentForm := Wnd.Form.ParentForm;
    while IsPopupForm(ParentForm) do
      ParentForm := ParentForm.ParentForm;
    if ParentForm <> nil then
      Wnd := WindowHandleToPlatform(ParentForm.Handle);
  end;
  if (Wnd = nil) or (Wnd.Wnd = nil) then
    raise EArgumentException.Create(sArgumentInvalid);
  Result := FindDisplay(Wnd.Wnd.screen);
end;

constructor TCocoaTouchScreenServices.Create;
begin
  inherited;
  FMainScreen := TUIScreen.Wrap(TUIScreen.OCClass.mainScreen);
  FOrientationListener := TInterfaceOrientationChangedListener.Create(Self);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(FOrientationListener.GetObjectID,
    sel_getUid('DeviceOrientationChanged'), StringToID('UIDeviceOrientationDidChangeNotification'), nil);
  RegisterServices;
end;

destructor TCocoaTouchScreenServices.Destroy;
begin
  UnregisterServices;
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).removeObserver(FOrientationListener.GetObjectID);
  FOrientationListener.Free;
  FMainScreen := nil;
  inherited;
end;

function TCocoaTouchScreenServices.DisplayFromPoint(const AHandle: TWindowHandle; const APoint: TPoint): TDisplay;
begin
  Result := DisplayFromWindow(AHandle);
end;

function TCocoaTouchScreenServices.GetDisplay(const AIndex: Integer): TDisplay;
begin
  if AIndex < 0 then
    raise EListError.CreateFMT(SListIndexError, [AIndex]);
  if (FDisplayList = nil) or (FDisplayList.Count <> GetDisplayCount) then
    UpdateDisplays;
  if AIndex >= GetDisplayCount then
    raise EListError.CreateFMT(SListIndexError, [AIndex]);
  Result := FDisplayList[AIndex];
end;

function TCocoaTouchScreenServices.GetWorkAreaRect: TRect;
begin
  if FWorkAreaRect.IsEmpty then
    FWorkAreaRect := CGRectToRect(MainScreen.applicationFrame);
  Result := FWorkAreaRect;
end;

procedure TCocoaTouchScreenServices.RegisterServices;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXMultiDisplayService) then
    TPlatformServices.Current.AddPlatformService(IFMXMultiDisplayService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXScreenService) then
    TPlatformServices.Current.AddPlatformService(IFMXScreenService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXDeviceMetricsService) then
    TPlatformServices.Current.AddPlatformService(IFMXDeviceMetricsService, Self);
end;

function TCocoaTouchScreenServices.GetScreenSize: TPointF;
var
  ScreenSize: TPointF;
  LNSSize: NSSize;
begin
  LNSSize := MainScreen.bounds.size;
  ScreenSize := TPointF.Create(LNSSize.width, LNSSize.height);
  if not TOSVersion.Check(8) and (GetScreenOrientation in [TScreenOrientation.Landscape, TScreenOrientation.InvertedLandscape]) then
    Result := TPointF.Create(ScreenSize.Y, ScreenSize.X)
  else
    Result := ScreenSize;
end;

procedure TCocoaTouchScreenServices.SetScreenOrientation(AOrientations: TScreenOrientations);
begin
  // Not needed for iOS
end;

function TCocoaTouchScreenServices.GetScreenScale: Single;
begin
  if FMainScreen <> nil then
    Result := FMainScreen.scale
  else
    Result := 1.0;
end;

function TCocoaTouchScreenServices.GetScreenOrientation: TScreenOrientation;
var
  InterfaceOrientation: UIInterfaceOrientation;
begin
  InterfaceOrientation := SharedApplication.keyWindow.rootViewController.interfaceOrientation;
  case InterfaceOrientation of
    UIInterfaceOrientationLandscapeLeft:
      Result := TScreenOrientation.Landscape;
    UIInterfaceOrientationLandscapeRight:
      Result := TScreenOrientation.InvertedLandscape;
    UIInterfaceOrientationPortrait:
      Result := TScreenOrientation.Portrait;
    UIInterfaceOrientationPortraitUpsideDown:
      Result := TScreenOrientation.InvertedPortrait;
  else
    Result := TScreenOrientation.Portrait
  end;
end;

{ TInterfaceOrientationChangedListener }

constructor TInterfaceOrientationChangedListener.Create(const AService: TCocoaTouchScreenServices);
begin
  inherited Create;
  FScreenService := AService;
end;

destructor TInterfaceOrientationChangedListener.Destroy;
begin
  FScreenService := nil;
  inherited;
end;

procedure TInterfaceOrientationChangedListener.DeviceOrientationChanged;
begin
  FScreenService.UpdateDisplayInformation;
end;

function TInterfaceOrientationChangedListener.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IFMXInterfaceOrientationChanged);
end;

end.
