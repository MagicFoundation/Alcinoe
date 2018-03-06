{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Metrics.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.Rtti, FMX.Pickers, FMX.Platform, FMX.Graphics;

type

  /// <summary>This class represents all interfaces for getting metrics</summary>
  TCocoaTouchMetricsServices = class(TInterfacedObject, IFMXDefaultMetricsService, IFMXDefaultPropertyValueService,
    IFMXSystemInformationService, IFMXTextEditingService, IFMXSystemFontService, IFMXLocaleService, IFMXListingService)
  public const
    /// <summary>Default size of system font</summary>
    DefaultiOSFontSize = 14;
  private
    FDefaultiOSFontName: string;
    function IFMXListingService.GetHeaderBehaviors = GetListingHeaderBehaviors;
    function IFMXListingService.GetSearchFeatures = GetListingSearchFeatures;
    function IFMXListingService.GetTransitionFeatures = GetListingTransitionFeatures;
    function IFMXListingService.GetEditModeFeatures = GetListingEditModeFeatures;
  protected
    /// <summary>Registers all metrics services in platform</summary>
    procedure RegisterServices; virtual;
    /// <summary>Unregisters all metrics service</summary>
    procedure UnregisterServices; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXDefaultMetricsService }
    /// <summary>Does specified component kind support default size on current platform?</summary>
    function SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
    /// <summary>Returns default size for specified component kind.</summary>
    /// <remarks>If <c>AComponent</c> doesn't support default size, it will return (80, 22)</remarks>
    function GetDefaultSize(const AComponent: TComponentKind): TSize;
    { IFMXDefaultPropertyValueService }
    /// <summary>Returns stored value of property <c>PropertyName</c> for specified class name <c>AClassName</c></summary>
    function GetDefaultPropertyValue(const AClassName, APropertyName: string): TValue;
    { IFMXSystemInformationService }
    /// <summary>Returns set of options, which describe scrolling behavior</summary>
    function GetScrollingBehaviour: TScrollingBehaviours;
    /// <summary>Returns minimum thumb scroll bar size</summary>
    function GetMinScrollThumbSize: Single;
    /// <summary>Returns caret width</summary>
    function GetCaretWidth: Integer;
    /// <summary>Returns in msec delay of showing menu</summary>
    /// <remarks>iOS doesn't support context menu, so it always returns 0.</remarks>
    function GetMenuShowDelay: Integer;
    { IFMXTextEditingService }
    /// <summary>Returns set of options, which describe caret behavior</summary>
    function GetCaretBehaviors: TCaretBehaviors;
    { IFMXSystemFontService }
    /// <summary>Returns default font family name in system</summary>
    function GetDefaultFontFamilyName: string;
    /// <summary>Returns default font size in system</summary>
    function GetDefaultFontSize: Single;
    { IFMXLocaleService }
    /// <summary>Returns ID of current language</summary>
    function GetCurrentLangID: string;
    /// <summary>Returns first day of week in current locale</summary>
    /// <remarks>1 - Monday</remarks>
    function GetLocaleFirstDayOfWeek: string;
    /// <summary>Returns first day of week in current locale</summary>
    /// <remarks>1 - Monday</remarks>
    function GetFirstWeekday: Byte;
    { IFMXListingService }
    /// <summary>Returns set of options, which describe <c>TListView</c> headers behavior</summary>
    function GetListingHeaderBehaviors: TListingHeaderBehaviors;
    /// <summary>Returns set of options, which describe <c>TListView</c> search behavior</summary>
    function GetListingSearchFeatures: TListingSearchFeatures;
    /// <summary>Returns set of options, which describe <c>TListView</c> transition behavior</summary>
    function GetListingTransitionFeatures: TListingTransitionFeatures;
    /// <summary>Returns set of options, which describe <c>TListView</c> edit mode behavior</summary>
    function GetListingEditModeFeatures: TListingEditModeFeatures;
  end;

implementation

uses
  System.SysUtils, Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Helpers, iOSapi.CocoaTypes, iOSapi.UIKit, 
  iOSapi.Foundation;

function TCocoaTouchMetricsServices.SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
begin
  case AComponent of
    TComponentKind.Button: Result := True;
    TComponentKind.Label: Result := True;
    TComponentKind.Edit: Result := True;
    TComponentKind.ScrollBar: Result := True;
    TComponentKind.ListBoxItem: Result := True;
    TComponentKind.Calendar: Result := True;
  else
    Result := False;
  end;
end;

procedure TCocoaTouchMetricsServices.UnregisterServices;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXDefaultMetricsService);
  TPlatformServices.Current.RemovePlatformService(IFMXDefaultPropertyValueService);
  TPlatformServices.Current.RemovePlatformService(IFMXSystemInformationService);
  TPlatformServices.Current.RemovePlatformService(IFMXTextEditingService);
  TPlatformServices.Current.RemovePlatformService(IFMXSystemFontService);
  TPlatformServices.Current.RemovePlatformService(IFMXLocaleService);
  TPlatformServices.Current.RemovePlatformService(IFMXListingService);
end;

function TCocoaTouchMetricsServices.GetDefaultSize(const AComponent: TComponentKind): TSize;
begin
  case AComponent of
    TComponentKind.Button: Result := TSize.Create(73, 44);
    TComponentKind.Label: Result := TSize.Create(82, 21);
    TComponentKind.Edit: Result := TSize.Create(97, 30);
    TComponentKind.ScrollBar: Result := TSize.Create(7, 7);
    TComponentKind.ListBoxItem: Result := TSize.Create(44, 44);
    TComponentKind.Calendar: Result := TSize.Create(320, 280);
  else
    Result := TSize.Create(80, 22);
  end;
end;

function TCocoaTouchMetricsServices.GetFirstWeekday: Byte;
const
  MondayOffset = 1;
var
  Calendar: NSCalendar;
begin
  Calendar := TNSCalendar.Wrap(TNSCalendar.OCClass.currentCalendar);
  // On the iOS Zero index corresponds Sunday, so we need to add offset. Because in RTL DayMonday = 1
  Result := Calendar.firstWeekday - MondayOffset;
end;

function TCocoaTouchMetricsServices.GetLocaleFirstDayOfWeek: string;
var
  Calendar: NSCalendar;
  FirstDay: NSUInteger;
begin
  Calendar := TNSCalendar.Wrap(TNSCalendar.OCClass.currentCalendar);
  FirstDay := Calendar.firstWeekday;
  Result := IntToStr(FirstDay);
end;

function TCocoaTouchMetricsServices.GetDefaultFontFamilyName: string;
begin
  if FDefaultiOSFontName.IsEmpty then
    FDefaultiOSFontName := NSStrToStr(TUIFont.Wrap(TUIFont.OCClass.systemFontOfSize(DefaultiOSFontSize)).fontName);
  Result := FDefaultiOSFontName;
end;

function TCocoaTouchMetricsServices.GetDefaultFontSize: Single;
begin
  Result := DefaultiOSFontSize;
end;

function TCocoaTouchMetricsServices.GetDefaultPropertyValue(const AClassName, APropertyName: string): TValue;

  function GetSpinBoxPropertyDefaultValue: TValue;
  begin
    Result := TValue.Empty;
    if string.Compare(APropertyName, 'CanFocusOnPlusMinus', True) = 0 then
      Result := False;
  end;

begin
  Result := TValue.Empty;

  if string.Compare(AClassName, 'tcolorcombobox', True) = 0 then
    Result := TValue.From<TDropDownKind>(TDropDownKind.Native)
  else if string.Compare(AClassName, 'tspinbox', True) = 0 then
    Result := GetSpinBoxPropertyDefaultValue
  else
    Result := False;
end;

function TCocoaTouchMetricsServices.GetScrollingBehaviour: TScrollingBehaviours;
begin
  Result := [TScrollingBehaviour.BoundsAnimation, TScrollingBehaviour.Animation, TScrollingBehaviour.AutoShowing, TScrollingBehaviour.TouchTracking];
end;

procedure TCocoaTouchMetricsServices.RegisterServices;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXDefaultMetricsService) then
    TPlatformServices.Current.AddPlatformService(IFMXDefaultMetricsService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXDefaultPropertyValueService) then
    TPlatformServices.Current.AddPlatformService(IFMXDefaultPropertyValueService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXSystemInformationService) then
    TPlatformServices.Current.AddPlatformService(IFMXSystemInformationService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXTextEditingService) then
    TPlatformServices.Current.AddPlatformService(IFMXTextEditingService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXSystemFontService) then
    TPlatformServices.Current.AddPlatformService(IFMXSystemFontService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService) then
    TPlatformServices.Current.AddPlatformService(IFMXLocaleService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXListingService) then
    TPlatformServices.Current.AddPlatformService(IFMXListingService, Self);
end;

function TCocoaTouchMetricsServices.GetMinScrollThumbSize: Single;
begin
  Result := 30;
end;

constructor TCocoaTouchMetricsServices.Create;
begin
  inherited;
  RegisterServices;
end;

destructor TCocoaTouchMetricsServices.Destroy;
begin
  UnregisterServices;
  inherited;
end;

function TCocoaTouchMetricsServices.GetCaretBehaviors: TCaretBehaviors;
begin
  Result := [TCaretBehavior.DisableCaretInsideWords];
end;

function TCocoaTouchMetricsServices.GetCaretWidth: Integer;
begin
  Result := 3;
end;

function TCocoaTouchMetricsServices.GetCurrentLangID: string;
var
  CurrentLocale: NSLocale;
  LanguageISO: NSString;
begin
  CurrentLocale := TNSLocale.Wrap(TNSLocale.OCClass.currentLocale);
  LanguageISO := TNSString.Wrap(CurrentLocale.objectForKey((NSLocaleLanguageCode as ILocalObject).GetObjectID));
  Result := UTF8ToString(LanguageISO.UTF8String);
  if Length(Result) > 2 then
    Delete(Result, 3, MaxInt);
end;

function TCocoaTouchMetricsServices.GetMenuShowDelay: Integer;
begin
  Result := 0;
end;

function TCocoaTouchMetricsServices.GetListingHeaderBehaviors: TListingHeaderBehaviors;
begin
  Result := [TListingHeaderBehavior.Sticky];
end;

function TCocoaTouchMetricsServices.GetListingSearchFeatures: TListingSearchFeatures;
begin
  Result := [TListingSearchFeature.StayOnTop, TListingSearchFeature.AsFirstItem];
end;

function TCocoaTouchMetricsServices.GetListingTransitionFeatures: TListingTransitionFeatures;
begin
  Result := [TListingTransitionFeature.EditMode, TListingTransitionFeature.DeleteButtonSlide,
    TListingTransitionFeature.PullToRefresh];
end;

function TCocoaTouchMetricsServices.GetListingEditModeFeatures: TListingEditModeFeatures;
begin
  Result := [TListingEditModeFeature.Delete];
end;

end.
