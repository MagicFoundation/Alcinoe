{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016 Embarcadero Technologies, Inc.      }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Pickers.iOS;

interface

{$SCOPEDENUMS ON}

procedure RegisterPickersService;
procedure UnregisterPickersService;

implementation

uses
  System.Classes, System.SysUtils, System.TypInfo, System.Types, System.DateUtils, System.Math, System.Math.Vectors,
  System.Generics.Collections, System.Messaging, Macapi.ObjCRuntime, Macapi.Helpers, Macapi.ObjectiveC, iOSapi.Foundation,
  iOSapi.CocoaTypes, iOSapi.UIKit, iOSapi.CoreGraphics, FMX.Platform.iOS, FMX.Pickers, FMX.Platform, FMX.Types,
  FMX.Consts, FMX.Controls, FMX.Forms, FMX.Helpers.iOS, FMX.VirtualKeyboard;

const
  AnimationDuration = 0.3;
  DefaultPickerWidth = 320;
  DefaultPickerHeight = 216;
  ToolBarHeight = 44;

type

  IDialogActions = interface(NSObject)
  ['{59D129A3-8EA1-4B48-A09F-4DF2ACA7E26F}']
    procedure Cancel; cdecl;
    procedure Done; cdecl;
    procedure Hidden; cdecl;
    procedure DeviceOrientationChanged; cdecl;
  end;

  TPickerState = (Hidden, Shown);

  { Only for iPad }
  TPopoverDelegate = class;

  { Native animated popup with 2 buttons (Done, Close) }
  TPopupDialog = class(TOCLocal)
  strict private
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
  private
    [Weak] FParentControl: TControl;
    FPickerState: TPickerState;
  protected
    // For iPad
    FAbsoluteTargetRect: TRectF;
    FUIPopoverController: UIPopoverController;
    FUIPopoverContent: UIViewController;
    FUIPopoverDelegate: TPopoverDelegate;
    // Common controls for all devices: iPhone and iPad idioms
    FUIContainerView: UIView;
    FUIToolBar: UIToolBar;
    FUICloseButton: UIBarButtonItem;
    FUIFlexibleSepararator: UIBarButtonItem;
    FUIDoneButton: UIBarButtonItem;
    procedure DoShow; virtual;
    procedure DoDone; virtual;
    procedure DoCancel; virtual;
    procedure DoHide; virtual;
    procedure DoPopoverDismiss;
    { Size of Popup }
    function GetWidth: Single;
    function GetHeight: Single;
    function GetToolBarHeight: Single;
    function GetContentHeight: Single; virtual;
    function GetPopupFrame: NSRect; virtual;
    function GetToolbarFrame: NSRect;
    function GetContentFrame: NSRect;
    { TOCLocal }
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
    { IDialogActions }
    procedure Cancel; cdecl;
    procedure Done; cdecl;
    procedure Hidden; cdecl;
    procedure DeviceOrientationChanged; virtual; cdecl;
    property AbsoluteTargetRect: TRectF read FAbsoluteTargetRect write FAbsoluteTargetRect;
    property ParentControl: TControl read FParentControl write FParentControl;
    property PickerState: TPickerState read FPickerState;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
  end;

  TPopoverDelegate = class(TOCLocal, UIPopoverControllerDelegate)
  strict private
    [Weak] FPopupDialog: TPopupDialog;
  public
    constructor Create(const APopupDialog: TPopupDialog);
    { UIPopoverControllerDelegate }
    procedure popoverControllerDidDismissPopover(popoverController: UIPopoverController); cdecl;
    function popoverControllerShouldDismissPopover(popoverController: UIPopoverController): Boolean; cdecl;
  end;

  { To inherit the IDialogActions interface it is impossible
    Because the child interface can be not registered in Objective C
    at the moment of creation of a copy of a class. It registers
    at the moment of creation of a copy of a class. }
  IDateTimeDialogActions = interface(NSObject)
  ['{47CDC1C0-8FF6-4815-94A0-79893BDFD722}']
    procedure Cancel; cdecl;
    procedure Done; cdecl;
    procedure Hidden; cdecl;
    procedure DeviceOrientationChanged; cdecl;
  end;

  { Native animated popup with 2 buttons (Done, Close) and Date picker }
  TDateTimePopupDialog = class(TPopupDialog)
  private const
    MinDateIndex = 1;
    MaxDateIndex = 2;
  private
    FDateTimePicker: UIDatePicker;
    FOnDateChanged: TOnDateChanged;
    function GetDateTime: TDateTime;
    procedure SetDateTime(const Value: TDateTime);
    procedure SetShowMode(const Value: TDatePickerShowMode);
    procedure SetDateTimeConstraints(const Index: Integer; const Value: TDate);
  protected
    procedure DoDateChanged;
    procedure DoDone; override;
    { TOCLocal }
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Kind: TDatePickerShowMode write SetShowMode;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property MinDate: TDate index MinDateIndex write SetDateTimeConstraints;
    property MaxDate: TDate index MaxDateIndex write SetDateTimeConstraints;
    property OnDateChanged: TOnDateChanged read FOnDateChanged write FOnDateChanged;
  end;

  IListBoxDialogActions = interface(NSObject)
  ['{0BA0DD09-C216-44A6-99AA-5F139F936557}']
    procedure Cancel; cdecl;
    procedure Done; cdecl;
    procedure Hidden; cdecl;
    procedure DeviceOrientationChanged; cdecl;
  end;

  TPickerDelegate = class;
  TPickerDataSource = class;

  { Native animated popup with 2 buttons (Done, Close) and Custom picker }
  TListBoxPopupDialog = class(TPopupDialog)
  private
    FValues: TList<NSString>;
    FListBoxPicker: UIPickerView;
    FDelegate: TPickerDelegate;
    FDataSource: TPickerDataSource;
    FOnValueChanged: TOnValueChanged;
    procedure SetValues(const AValues: TStrings);
    procedure DoValueChanged;
    procedure SetItemIndex(const Value: Integer);
  protected
    procedure DoDone; override;
    procedure ClearValues;
    { TOCLocal }
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Values: TStrings write SetValues;
    property ItemIndex: Integer write SetItemIndex;
    property OnValueChanged: TOnValueChanged read FOnValueChanged write FOnValueChanged;
  end;

  {$REGION 'Helpers for event handling of native components'}

  { Class implement delegate and protocol interfaces for work with
    UIPickerView in |TPickerServiceiOS| }
  TPickerDataSource = class(TOCLocal, UIPickerViewDataSource)
  strict private
    [Weak] FValues: TList<NSString>;
  public
    constructor Create(const AValues: TList<NSString>);
    destructor Destroy; override;
    { UIPickerViewDataSource }
    function numberOfComponentsInPickerView(pickerView: UIPickerView): NSInteger; cdecl;
    function pickerView(pickerView: UIPickerView; numberOfRowsInComponent: NSInteger): NSInteger; cdecl;
  end;

  TPickerDelegate = class(TOCLocal, UIPickerViewDelegate)
  strict private
    [Weak] FValues: TList<NSString>;
  public
    constructor Create(const AValues: TList<NSString>);
    destructor Destroy; override;
    { UIPickerViewDelegate }
    function pickerView(pickerView: UIPickerView; titleForRow: NSInteger; forComponent: NSInteger): NSString; cdecl;
  end;

{$ENDREGION}

{ TCocoaDateTimePicker }

  TCocoaDateTimePicker = class(TCustomDateTimePicker)
  strict private
    FPopupDateTimePicker: TDateTimePopupDialog;
  public
    constructor Create(const APickerService: TPickerFactoryService); override;
    destructor Destroy; override;
    procedure Show; override;
    procedure Hide; override;
    function IsShown: Boolean; override;
  end;

{ TCocoaListPicker }

  TCocoaListPicker = class(TCustomListPicker)
  strict private
    FPopupListPicker: TListBoxPopupDialog;
  public
    constructor Create(const APickerService: TPickerFactoryService); override;
    destructor Destroy; override;
    procedure Show; override;
    procedure Hide; override;
    function IsShown: Boolean; override;
  end;

{ Picker Service }

  TCocoaPickerService = class(TPickerFactoryService)
  private
    procedure FormShownHandler(const Sender: TObject; const M: TMessage);
  protected
    function DoCreateDateTimePicker: TCustomDateTimePicker; override;
    function DoCreateListPicker: TCustomListPicker; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  PickerService: TPickerFactoryService;

procedure RegisterPickersService;
begin
  PickerService := TCocoaPickerService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXPickerService, PickerService);
end;

procedure UnregisterPickersService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXPickerService);
  PickerService := nil;
end;

{ TCocoaPickerSerivce }

constructor TCocoaPickerService.Create;
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TFormBeforeShownMessage, FormShownHandler);
end;

destructor TCocoaPickerService.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TFormBeforeShownMessage, FormShownHandler);
  inherited;
end;

function TCocoaPickerService.DoCreateDateTimePicker: TCustomDateTimePicker;
begin
  Result := TCocoaDateTimePicker.Create(Self);
end;

function TCocoaPickerService.DoCreateListPicker: TCustomListPicker;
begin
  Result := TCocoaListPicker.Create(Self);
end;

procedure TCocoaPickerService.FormShownHandler(const Sender: TObject; const M: TMessage);
begin
  CloseAllPickers;
end;

{ TCocoaDateTimePicker }

constructor TCocoaDateTimePicker.Create(const APickerService: TPickerFactoryService);
begin
  inherited Create(APickerService);
  FPopupDateTimePicker := TDateTimePopupDialog.Create;
end;

destructor TCocoaDateTimePicker.Destroy;
begin
  FreeAndNil(FPopupDateTimePicker);
  inherited Destroy;
end;

procedure TCocoaDateTimePicker.Hide;
begin
  if IsShown then
    FPopupDateTimePicker.Hide;
end;

function TCocoaDateTimePicker.IsShown: Boolean;
begin
  Result := FPopupDateTimePicker.PickerState = TPickerState.Shown;
end;

procedure TCocoaDateTimePicker.Show;
begin
  FPopupDateTimePicker.ParentControl := Parent;
  FPopupDateTimePicker.AbsoluteTargetRect := AbsoluteTargetRect;
  FPopupDateTimePicker.Kind := ShowMode;
  FPopupDateTimePicker.MinDate := MinDate;
  FPopupDateTimePicker.MaxDate := MaxDate;
  // Reset event handler, because when we set date to TCalendar by Date, it calls OnDateChanged event.
  FPopupDateTimePicker.OnDateChanged := nil;
  try
    FPopupDateTimePicker.DateTime := Date;
  finally
    FPopupDateTimePicker.OnDateChanged := OnDateChanged;
  end;
  FPopupDateTimePicker.OnHide := OnHide;
  FPopupDateTimePicker.OnShow := OnShow;

  FPopupDateTimePicker.Show;
end;

{ TPopupDialog }

procedure TPopupDialog.Cancel;
begin
  DoCancel;
  Hide;
end;

constructor TPopupDialog.Create;

  procedure CreateToolbarConstraint;
  var
    Constraint: NSLayoutConstraint;
  begin
    FUIToolBar.setTranslatesAutoresizingMaskIntoConstraints(False);
    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIToolBar), NSLayoutAttributeLeft,
      NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeLeft, 1, 0));
    Constraint.setActive(True);

    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIToolBar), NSLayoutAttributeRight,
      NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeRight, 1, 0));
    Constraint.setActive(True);

    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIToolBar), NSLayoutAttributeTop,
      NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeTop, 1, 0));
    Constraint.setActive(True);

    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIToolBar), NSLayoutAttributeHeight,
      NSLayoutRelationEqual, nil, NSLayoutAttributeNotAnAttribute, 1, ToolBarHeight));
    Constraint.setActive(True);
  end;

var
  PickerFrame: NSRect;
  Buttons: NSMutableArray;
begin
  inherited Create;

  { Subscribing to change orientation events }
  DefaultNotificationCenter.addObserver(GetObjectID, sel_getUid('DeviceOrientationChanged'),
    StringToID(FMXViewControllerFrameChanged), nil);

  FAbsoluteTargetRect := TRectF.Empty;
  { Creating Root view container for picker }
  FUIContainerView := TUIView.Create;
  FUIContainerView.setBackgroundColor(TUIColor.Wrap(TUIColor.OCClass.whiteColor));
  FUIContainerView.setAutoresizingMask(UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleLeftMargin or UIViewAutoresizingFlexibleRightMargin);
  PickerFrame := GetPopupFrame;
  FUIContainerView.setFrame(PickerFrame);

  if IsPad then
  begin
    FUIPopoverContent := TUIViewController.Create;
    FUIPopoverContent.setView(FUIContainerView);
    FUIPopoverContent.setContentSizeForViewInPopover(PickerFrame.size);
    FUIPopoverController := TUIPopoverController.Alloc;
    FUIPopoverController := TUIPopoverController.Wrap(FUIPopoverController.initWithContentViewController(FUIPopoverContent));
    FUIPopoverDelegate := TPopoverDelegate.Create(Self);
    FUIPopoverController.setPopoverContentSize(PickerFrame.size);
    FUIPopoverController.setDelegate(FUIPopoverDelegate.GetObjectID);
  end;

  { Creating Toolbar }
  FUIToolBar := TUIToolbar.Create;
  FUIToolBar.setAlpha(0.8);
  FUIContainerView.addSubview(FUIToolBar);
  CreateToolbarConstraint;

  Buttons := TNSMutableArray.Create;
  try
    { Adding Close Button }
    FUICloseButton := TUIBarButtonItem.Create;
    FUICloseButton.setTitle(StrToNSStr(SPickerCancel));
    FUICloseButton.setStyle(UIBarButtonItemStyleBordered);
    FUICloseButton.setTarget(Self.GetObjectID);
    FUICloseButton.setAction(sel_getUid('Cancel'));
    Buttons.addObject(NSObjectToID(FUICloseButton));

    { Adding Flexible Separator }
    FUIFlexibleSepararator := TUIBarButtonItem.Create;
    FUIFlexibleSepararator.initWithBarButtonSystemItem(UIBarButtonSystemItemFlexibleSpace, nil, nil);
    Buttons.addObject(NSObjectToID(FUIFlexibleSepararator));

    { Adding Done Button }
    FUIDoneButton := TUIBarButtonItem.Create;
    FUIDoneButton.setTitle(StrToNSStr(SPickerDone));
    FUIDoneButton.setStyle(UIBarButtonItemStyleDone);
    FUIDoneButton.setTarget(Self.GetObjectID);
    FUIDoneButton.setAction(sel_getUid('Done'));
    Buttons.addObject(NSObjectToID(FUIDoneButton));

    { Adding button to Toolbar }
    FUIToolBar.setItems(Buttons);
  finally
    Buttons.release;
  end;
end;

function TPopupDialog.GetPopupFrame: NSRect;
var
  RootViewRect: NSRect;
  KeyWin: UIWindow;
begin
  Result := CGRect.Create(0, 0, GetWidth, GetHeight);
  if not IsPad then
  begin
    KeyWin := SharedApplication.keyWindow;
    if (KeyWin <> nil) and (KeyWin.rootViewController <> nil) and (KeyWin.rootViewController.view <> nil) then
    begin
      RootViewRect := SharedApplication.keyWindow.rootViewController.view.frame;
      RootViewRect := SharedApplication.keyWindow.rootViewController.view.bounds;
      Result.origin.y := RootViewRect.size.height - GetHeight;
    end;
  end;
end;

destructor TPopupDialog.Destroy;
begin
  DefaultNotificationCenter.removeObserver(GetObjectID);
  FUIToolBar.setItems(nil);
  FUIDoneButton.release;
  FUIFlexibleSepararator.release;
  FUICloseButton.release;
  FUIToolBar.release;
  FUIContainerView.removeFromSuperview;
  FUIContainerView.release;
  if IsPad then
  begin
    FUIPopoverContent.release;
    FUIPopoverController.setDelegate(nil);
    FUIPopoverController.release;
    FUIPopoverDelegate := nil;
  end;
  inherited Destroy;
end;

procedure TPopupDialog.DoCancel;
begin
  // Nothing
end;

procedure TPopupDialog.DoDone;
begin
  // Nothing
end;

procedure TPopupDialog.DoHide;
begin
  FPickerState := TPickerState.Hidden;
  if Assigned(FOnHide) then
    FOnHide(FParentControl);
  if Screen.ActiveForm <> nil then
    Screen.ActiveForm.Focused := nil;
end;

procedure TPopupDialog.Done;
begin
  DoDone;
  Hide;
end;

procedure TPopupDialog.DoPopoverDismiss;
begin
  DoHide;
end;

procedure TPopupDialog.DoShow;
begin
  FPickerState := TPickerState.Shown;
  if Assigned(FOnShow) then
    FOnShow(FParentControl);
end;

function TPopupDialog.GetToolbarFrame: NSRect;
begin
  Result := CGRect.Create(0, 0, GetWidth, GetToolBarHeight);
end;

function TPopupDialog.GetToolBarHeight: Single;
begin
  Result := ToolBarHeight;
end;

function TPopupDialog.GetWidth: Single;
begin
  if IsPad then
    Result := DefaultPickerWidth
  else
    Result := Screen.Size.Width;
end;

function TPopupDialog.GetContentFrame: NSRect;
begin
  Result.origin.x := 0;
  Result.origin.y := GetToolBarHeight;
  Result.size.width := GetWidth;
  Result.size.height := GetContentHeight;
end;

function TPopupDialog.GetContentHeight: Single;
begin
  Result := DefaultPickerHeight;
end;

function TPopupDialog.GetHeight: Single;
begin
  Result := GetToolBarHeight + GetContentHeight;
end;

function TPopupDialog.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IDialogActions);
end;

procedure TPopupDialog.Hidden;
begin
  if not IsPad and (FPickerState = TPickerState.Hidden) then
    FUIContainerView.removeFromSuperview;
end;

procedure TPopupDialog.Hide;
var
  EndFrame: NSRect;
begin
  if FUIContainerView.superview = nil then
    Exit;

  if IsPad then
    FUIPopoverController.dismissPopoverAnimated(True)
  else
  begin
    EndFrame := GetPopupFrame;
    EndFrame.origin.y := EndFrame.origin.y + EndFrame.size.height;

    { Start Slide down Animation }
    TUIView.OCClass.beginAnimations(nil, nil);
    try
      TUIView.OCClass.setAnimationDuration(AnimationDuration);
      FUIContainerView.setFrame(EndFrame);
      TUIView.OCClass.setAnimationDelegate(Self.GetObjectID);
      TUIView.OCClass.setAnimationDidStopSelector(sel_getUid('Hidden'))
    finally
      TUIView.OCClass.commitAnimations;
    end;
  end;
  DoHide;
end;

procedure TPopupDialog.Show;

  function DefineTargetRect: CGRect;
  var
    ScreenPos: TPointF;
    AbsolutePos: TPointF;
    ScreenRect: TRectF;
  begin
    if AbsoluteTargetRect.IsEmpty and (ParentControl <> nil) then
    begin
      AbsolutePos := FParentControl.LocalToAbsolute(TPointF.Zero);
      ScreenPos := FParentControl.Scene.LocalToScreen(AbsolutePos);
      Result := CGRect.Create(ScreenPos.X, ScreenPos.Y, FParentControl.Width, FParentControl.Height);
    end
    else if not FAbsoluteTargetRect.IsEmpty then
    begin
      ScreenRect := AbsoluteTargetRect;
      if Screen.ActiveForm <> nil then
        ScreenRect.Location := Screen.ActiveForm.ClientToScreen(AbsoluteTargetRect.TopLeft);
      Result := CGRect.Create(ScreenRect);
    end
    else
      Result := CGRect.Create(TRect.Empty);
  end;

var
  StartFrame: CGRect;
  EndFrame: NSRect;
  VirtualKeyboard: IFMXVirtualKeyboardService;
begin
  if IsPad then
    FUIPopoverController.presentPopoverFromRect(DefineTargetRect, SharedApplication.keyWindow.rootViewController.view,
     UIPopoverArrowDirectionAny, True)
  else
  begin
    SharedApplication.keyWindow.rootViewController.view.addSubview(FUIContainerView);
    if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, VirtualKeyboard) then
      VirtualKeyboard.HideVirtualKeyboard;

    { Setting animation }
    StartFrame := GetPopupFrame;
    EndFrame := StartFrame;
    StartFrame.origin.y := StartFrame.origin.y + StartFrame.size.height;
    FUIContainerView.setFrame(StartFrame);

    { Start Animation }
    TUIView.OCClass.beginAnimations(nil, nil);
    try
      TUIView.OCClass.setAnimationDuration(AnimationDuration);
      FUIContainerView.setFrame(EndFrame);
    finally
      TUIView.OCClass.commitAnimations;
    end;
  end;
  DoShow;
end;

procedure TPopupDialog.DeviceOrientationChanged;
begin
  FUIContainerView.setFrame(GetPopupFrame);
end;

{ TCocoaListPicker }

constructor TCocoaListPicker.Create(const APickerService: TPickerFactoryService);
begin
  inherited Create(APickerService);
  FPopupListPicker := TListBoxPopupDialog.Create;
end;

destructor TCocoaListPicker.Destroy;
begin
  FPopupListPicker.DisposeOf;
  FPopupListPicker := nil;
  inherited Destroy;
end;

procedure TCocoaListPicker.Hide;
begin
  if IsShown then
    FPopupListPicker.Hide;
end;

function TCocoaListPicker.IsShown: Boolean;
begin
  Result := FPopupListPicker.PickerState = TPickerState.Shown;
end;

procedure TCocoaListPicker.Show;
begin
  if Values.Count = 0 then
    Exit;

  FPopupListPicker.ParentControl := Parent;
  FPopupListPicker.AbsoluteTargetRect := AbsoluteTargetRect;
  FPopupListPicker.Values := Values;
  FPopupListPicker.ItemIndex := ItemIndex;
  FPopupListPicker.OnValueChanged := OnValueChanged;
  FPopupListPicker.OnHide := OnHide;
  FPopupListPicker.OnShow := OnShow;
  FPopupListPicker.Show;
end;

{ TDateTimePopupDialog }

constructor TDateTimePopupDialog.Create;

  procedure CreateCalendarConstraint;
  var
    Constraint: NSLayoutConstraint;
  begin
    FDateTimePicker.setTranslatesAutoresizingMaskIntoConstraints(False);
    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FDateTimePicker), NSLayoutAttributeLeft,
      NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeLeft, 1, 0));
    Constraint.setActive(True);

    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FDateTimePicker), NSLayoutAttributeRight,
      NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeRight, 1, 0));
    Constraint.setActive(True);

    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FDateTimePicker), NSLayoutAttributeTop,
      NSLayoutRelationEqual, NSObjectToID(FUIToolBar), NSLayoutAttributeBottom, 1, 0));
    Constraint.setActive(True);

    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FDateTimePicker), NSLayoutAttributeHeight,
      NSLayoutRelationEqual, nil, NSLayoutAttributeNotAnAttribute, 1, DefaultPickerHeight));
    Constraint.setActive(True);
  end;

begin
  // We should Create instance of UIDatePicker before creating main container.
  // Because We use UIViewPicker for determinate finally size of picker container
  // (Toolbar + Picker)
  FDateTimePicker := TUIDatePicker.Create;
  FDateTimePicker.setTimeZone(TNSTimeZone.Wrap(TNSTimeZone.OCClass.timeZoneForSecondsFromGMT(0)));

  inherited Create;

  FUIContainerView.addSubview(FDateTimePicker);
  CreateCalendarConstraint;
end;

destructor TDateTimePopupDialog.Destroy;
begin
  FDateTimePicker.removeFromSuperview;
  FDateTimePicker.release;
  FDateTimePicker := nil;
  inherited Destroy;
end;

procedure TDateTimePopupDialog.DoDateChanged;
begin
  if Assigned(FOnDateChanged) then
    FOnDateChanged(FParentControl, DateTime);
end;

procedure TDateTimePopupDialog.DoDone;
begin
  DoDateChanged;
end;

function TDateTimePopupDialog.GetDateTime: TDateTime;

  function TrimSeconds(const ADateTime: TDateTime): TDateTime;
  var
    Hour: Word;
    Min: Word;
    Sec: Word;
    MSec: Word;
  begin
    DecodeTime(ADateTime, Hour, Min, Sec, MSec);
    Result := DateOf(ADateTime) +  EncodeTime(Hour, Min, 0, 0);
  end;

var
  DateTimeWithoutSec: TDateTime;
begin
  // Native ios UIDatePicker doesn't allow users to set second.
  // We must to trim second from datetime
  DateTimeWithoutSec := TrimSeconds(NSDateToDateTime(FDateTimePicker.date));
  Result := TrimSeconds(DateTimeWithoutSec);
end;

function TDateTimePopupDialog.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IDateTimeDialogActions);
end;

procedure TDateTimePopupDialog.SetDateTime(const Value: TDateTime);
begin
  // Bug in using NSDate in UIDatePicker. When we set time 10:00:00,
  // UIDatePicker shows 9:59:59, but UIDatePicker.date.description shows 10:00:00
  // So we need to add 0.1 sec for it
  FDateTimePicker.setDate(DateTimeToNSDate(Value + 0.1 / SecsPerDay));
end;

procedure TDateTimePopupDialog.SetDateTimeConstraints(const Index: Integer; const Value: TDate);
begin
  case Index of
    MinDateIndex:
      if not SameValue(Value, 0.0) then
        FDateTimePicker.setMinimumDate(DateTimeToNSDate(Value))
      else
        FDateTimePicker.setMinimumDate(nil);
    MaxDateIndex:
      if not SameValue(Value, 0.0) then
        FDateTimePicker.setMaximumDate(DateTimeToNSDate(Value))
      else
        FDateTimePicker.setMaximumDate(nil);
  end;
end;

procedure TDateTimePopupDialog.SetShowMode(const Value: TDatePickerShowMode);
var
  DatePickerMode: UIDatePickerMode;
begin
  case Value of
    TDatePickerShowMode.Date:
      DatePickerMode := UIDatePickerModeDate;
    TDatePickerShowMode.Time:
      DatePickerMode := UIDatePickerModeTime;
    TDatePickerShowMode.DateTime:
      DatePickerMode := UIDatePickerModeDateAndTime;
  else
    DatePickerMode := UIDatePickerModeDateAndTime;
  end;
  FDateTimePicker.setDatePickerMode(DatePickerMode);
end;

{ TListBoxPopupDialog }

procedure TListBoxPopupDialog.ClearValues;
var
  Value: NSString;
begin
  for Value in FValues do
    Value.release;
  FValues.Clear;
end;

constructor TListBoxPopupDialog.Create;

  procedure CreateListBoxConstraint;
  var
    Constraint: NSLayoutConstraint;
  begin
    FListBoxPicker.setTranslatesAutoresizingMaskIntoConstraints(False);
    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FListBoxPicker), NSLayoutAttributeLeft,
      NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeLeft, 1, 0));
    Constraint.setActive(True);

    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FListBoxPicker), NSLayoutAttributeRight,
      NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeRight, 1, 0));
    Constraint.setActive(True);

    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FListBoxPicker), NSLayoutAttributeTop,
      NSLayoutRelationEqual, NSObjectToID(FUIToolBar), NSLayoutAttributeBottom, 1, 0));
    Constraint.setActive(True);

    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FListBoxPicker), NSLayoutAttributeHeight,
      NSLayoutRelationEqual, nil, NSLayoutAttributeNotAnAttribute, 1, DefaultPickerHeight));
    Constraint.setActive(True);
  end;

begin
  FValues := TList<NSString>.Create;

  // We should Create instance of UIViewPicker before creating main container.
  // Because We use UIViewPicker for determinate finally size of picker container
  // (Toolbar + Picker)
  FListBoxPicker := TUIPickerView.Create;
  FListBoxPicker.setFrame(GetContentFrame);
  FListBoxPicker.setShowsSelectionIndicator(True);
  FDelegate := TPickerDelegate.Create(FValues);
  FDataSource := TPickerDataSource.Create(FValues);
  FListBoxPicker.setDataSource(FDataSource.GetObjectID);
  FListBoxPicker.setDelegate(FDelegate.GetObjectID);

  inherited Create;

  FUIContainerView.addSubview(FListBoxPicker);
  FUIContainerView.setNeedsLayout;

  CreateListBoxConstraint
end;

destructor TListBoxPopupDialog.Destroy;
begin
  FListBoxPicker.setDataSource(nil);
  FListBoxPicker.setDelegate(nil);
  ClearValues;
  FValues.Free;
  FDataSource.Free;
  FDelegate.Free;
  FListBoxPicker.removeFromSuperview;
  FListBoxPicker.release;
  FListBoxPicker := nil;
  inherited Destroy;
end;

procedure TListBoxPopupDialog.DoDone;
begin
  DoValueChanged;
end;

procedure TListBoxPopupDialog.DoValueChanged;
begin
  if Assigned(FOnValueChanged) and (FValues.count > 0) then
    FOnValueChanged(FParentControl, FListBoxPicker.selectedRowInComponent(0));
end;

function TListBoxPopupDialog.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IListBoxDialogActions);
end;

procedure TListBoxPopupDialog.SetItemIndex(const Value: Integer);
begin
  FListBoxPicker.selectRow(Value, 0, False);
end;

procedure TListBoxPopupDialog.SetValues(const AValues: TStrings);
var
  I: Integer;
  OCValue: NSString;
begin
  ClearValues;
  for I := 0 to AValues.Count - 1 do
  begin
    if AValues[I].IsEmpty then
      OCValue := TNSString.Create
    else
      OCValue := StrToNSStr(AValues[I]);
    OCValue.retain;
    FValues.Add(OCValue);
  end;
  FListBoxPicker.reloadAllComponents;
end;

{ TPickerDataSource }

constructor TPickerDataSource.Create(const AValues: TList<NSString>);
begin
  Assert(AValues <> nil);
  inherited Create;
  FValues := AValues;
end;

destructor TPickerDataSource.Destroy;
begin
  FValues := nil;
  inherited Destroy;
end;

function TPickerDataSource.numberOfComponentsInPickerView(pickerView: UIPickerView): NSInteger;
begin
  Result := 1;
end;

function TPickerDataSource.pickerView(pickerView: UIPickerView; numberOfRowsInComponent: NSInteger): NSInteger;
begin
  Result := FValues.count;
end;

{ TPickerDelegate }

constructor TPickerDelegate.Create(const AValues: TList<NSString>);
begin
  Assert(AValues <> nil);
  inherited Create;
  FValues := AValues;
end;

destructor TPickerDelegate.Destroy;
begin
  FValues := nil;
  inherited Destroy;
end;

function TPickerDelegate.pickerView(pickerView: UIPickerView; titleForRow, forComponent: NSInteger): NSString;
begin
  Result := FValues[titleForRow];
end;

{ TPopoverDelegate }

constructor TPopoverDelegate.Create(const APopupDialog: TPopupDialog);
begin
  inherited Create;
  FPopupDialog := APopupDialog;
end;

procedure TPopoverDelegate.popoverControllerDidDismissPopover(popoverController: UIPopoverController);
begin
  FPopupDialog.DoPopoverDismiss;
end;

function TPopoverDelegate.popoverControllerShouldDismissPopover(popoverController: UIPopoverController): Boolean;
begin
  Result := True;
end;

end.
