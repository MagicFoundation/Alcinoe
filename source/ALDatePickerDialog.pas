unit ALDatePickerDialog;

interface

uses {$IF defined(android)}
     Androidapi.JNIBridge,
     ALAndroidApi,
     {$ENDIF}
     {$IF defined(ios)}
     System.TypInfo,
     Macapi.ObjectiveC,
     iOSapi.uikit,
     iOSapi.Foundation,
     {$ENDIF}
     System.UITypes;

const
  mrClear = 103;

type

  {****************}
  {$IF defined(IOS)}
  IALDatePickerDialogActions = interface(NSObject)
  ['{2378009A-43FC-4FD7-A328-BB36C6C1E467}']
    procedure Cancel; cdecl;
    procedure Done; cdecl;
    procedure Clear; cdecl;
    procedure Hidden; cdecl;
    procedure DeviceOrientationChanged; cdecl;
    procedure HandleTap; cdecl;
  end;
  {$ENDIF}

  {*******************************************************************************************************************************************}
  TALDatePickerDialogCloseEvent = procedure(Sender: Tobject; const AResult: TModalResult; const ayear, amonth, adayOfMonth: integer) of object;

  {****************}
  {$IF defined(IOS)}
  TALDatePickerDialog = Class(TOCLocal)
  {$ELSE}
  TALDatePickerDialog = Class(Tobject)
  {$ENDIF}
  private

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TDatePickerDialogListener = class(TJavaLocal, JALDatePickerDialogListener)
      private
        [Weak] fDatePickerDialog: TALDatePickerDialog;
      public
        constructor Create(const aDatePickerDialog: TALDatePickerDialog);
        procedure onBtnClick(which: integer; year: integer; month: integer; dayOfMonth: integer); cdecl;
      end;

    {$ENDIF}
    {$ENDREGION}

  private

    fOnClose: TALDatePickerDialogCloseEvent;

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    fDatePickerDialogListener: TDatePickerDialogListener;
    fDatePickerDialog: JALDatePickerDialog;
    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(ios)}
    FUIOverlayView: UIView;
    FUIContainerView: UIView;
    FUIDatePicker: UIDatePicker;
    FUIToolBar: UIToolBar;
    FUICancelButton: UIBarButtonItem;
    FUIDoneButton: UIBarButtonItem;
    FUIClearButton: UIBarButtonItem;
    FUIFlexibleSepararator1: UIBarButtonItem;
    FUIFlexibleSepararator2: UIBarButtonItem;
    FUIFlexibleSepararator3: UIBarButtonItem;
    FUIFlexibleSepararator4: UIBarButtonItem;
    function GetWidth: Single;
    function GetHeight: Single;
    function GetToolBarHeight: Single;
    function GetContentHeight: Single;
    function GetPopupFrame: NSRect;
    function GetDateTime: TDateTime;
    procedure Hide;
    {$ENDIF}
    {$ENDREGION}

  protected

    {$REGION ' IOS'}
    {$IF defined(ios)}
    function GetObjectiveCClass: PTypeInfo; override;
    {$ENDIF}
    {$ENDREGION}

  public

    {$REGION ' IOS'}
    {$IF defined(ios)}
    procedure Cancel; cdecl;
    procedure Clear; cdecl;
    procedure Done; cdecl;
    procedure Hidden; cdecl;
    procedure DeviceOrientationChanged; cdecl;
    procedure HandleTap; cdecl;
    {$ENDIF}
    {$ENDREGION}

  public
    constructor create(const aBtnOKCaption: string;
                       const aBtnCancelCaption: string;
                       const aBtnClearCaption: string);
    destructor Destroy; override;
    procedure show(const aYear: integer;
                   const aMonth: integer;
                   const aDayOfMonth: integer);
    property OnClose: TALDatePickerDialogCloseEvent read fOnClose write fOnClose;
  End;

implementation

uses system.Classes,
     system.SysUtils,
     {$IF defined(android)}
     Androidapi.JNI.JavaTypes,
     Androidapi.JNI.GraphicsContentViewText,
     FMX.Helpers.Android,
     Androidapi.Helpers,
     {$ENDIF}
     {$IF defined(ios)}
     system.Math,
     system.DateUtils,
     Macapi.Helpers,
     iOSapi.CoreGraphics,
     Macapi.ObjCRuntime,
     Fmx.platForm.iOS,
     FMX.Helpers.iOS,
     FMX.Forms,
     {$ENDIF}
     alcommon;

{$REGION ' ANDROID'}
{$IF defined(android)}

{*************************************************************************************************************}
constructor TALDatePickerDialog.TDatePickerDialogListener.Create(const aDatePickerDialog: TALDatePickerDialog);
begin
  inherited Create;
  fDatePickerDialog := aDatePickerDialog;
end;

{*************************************************************************************************************************************}
procedure TALDatePickerDialog.TDatePickerDialogListener.onBtnClick(which: integer; year: integer; month: integer; dayOfMonth: integer);
begin

  {$IFDEF DEBUG}
  //i absolutly not understand why on android 6 i receive an error BUS ERROR 10 when i call allog ! this is big misterry
  //allog('TALDatePickerDialog.TDatePickerDialogListener.onClick','which: ' + alinttostrU(which) +
  //                                                              ' - year: ' + alinttostrU(year) +
  //                                                              ' - month: ' + alinttostrU(month) +
  //                                                              ' - dayOfMonth: ' + alinttostrU(dayOfMonth) +
  //                                                              ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  {$IF CompilerVersion > 31} // berlin
    {$MESSAGE WARN 'remove TThread.synchronize because maybe not anymore needed in tokyo (look if now TThread.Current.ThreadID=MainThreadID)'}
  {$ENDIF}
  TThread.queue(nil,
    procedure
    var AResult: TModalResult;
    begin
      if assigned(fDatePickerDialog) and
         assigned(fDatePickerDialog.fOnClose) then begin

        if which = TJDialogInterface.javaclass.BUTTON_POSITIVE then AResult := mrOk
        else if which = TJDialogInterface.javaclass.BUTTON_NEGATIVE then AResult := mrCancel
        else if which = TJDialogInterface.javaclass.BUTTON_NEUTRAL then AResult := mrClear
        else AResult := mrClose; // << not possible normally

        // Months are indexed starting at 0, so August is month 8, or index 7
        fDatePickerDialog.fOnClose(fDatePickerDialog, AResult, year, month+1, dayOfMonth);

      end;
    end);

end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}
const
  _AnimationDuration = 0.3;
  _DefaultPickerWidth = 320;
  _DefaultPickerHeight = 216;
  _ToolBarHeight = 44;
{$ENDIF}
{$ENDREGION}

{*****************************************************************}
constructor TALDatePickerDialog.create(const aBtnOKCaption: string;
                                       const aBtnCancelCaption: string;
                                       const aBtnClearCaption: string);

  {$REGION ' IOS'}
  {$IF defined(ios)}

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure CreateToolbarConstraint;
  var Constraint: NSLayoutConstraint;
  begin
    FUIToolBar.setTranslatesAutoresizingMaskIntoConstraints(False);
    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIToolBar), NSLayoutAttributeLeft,   NSLayoutRelationEqual, NSObjectToID(FUIContainerView),    NSLayoutAttributeLeft,           1, 0));
    Constraint.setActive(True);
    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIToolBar), NSLayoutAttributeRight,  NSLayoutRelationEqual, NSObjectToID(FUIContainerView),    NSLayoutAttributeRight,          1, 0));
    Constraint.setActive(True);
    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIToolBar), NSLayoutAttributeBottom, NSLayoutRelationEqual, NSObjectToID(FUIContainerView),    NSLayoutAttributeBottom,         1, 0));
    Constraint.setActive(True);
    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIToolBar), NSLayoutAttributeHeight, NSLayoutRelationEqual, nil,                               NSLayoutAttributeNotAnAttribute, 1, _ToolBarHeight));
    Constraint.setActive(True);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure CreateCalendarConstraint;
  var Constraint: NSLayoutConstraint;
  begin
    FUIDatePicker.setTranslatesAutoresizingMaskIntoConstraints(False);
    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIDatePicker), NSLayoutAttributeLeft,   NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeLeft,           1, 0));
    Constraint.setActive(True);
    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIDatePicker), NSLayoutAttributeRight,  NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeRight,          1, 0));
    Constraint.setActive(True);
    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIDatePicker), NSLayoutAttributebottom, NSLayoutRelationEqual, NSObjectToID(FUIToolBar),       NSLayoutAttributetop,            1, 0));
    Constraint.setActive(True);
    Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIDatePicker), NSLayoutAttributeHeight, NSLayoutRelationEqual, nil,                            NSLayoutAttributeNotAnAttribute, 1, _DefaultPickerHeight));
    Constraint.setActive(True);
  end;

  var aButtons: NSMutableArray;
      aUIColor: UIColor;
      aSingleTapGestureRecognizer: UITapGestureRecognizer;

  {$ENDIF}
  {$ENDREGION}

begin

  inherited create;
  fOnClose := nil;

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  fDatePickerDialogListener := TDatePickerDialogListener.Create(Self);
  TUIThreadCaller.Call<TDatePickerDialogListener>(
    procedure (aDatePickerDialogListener: TDatePickerDialogListener)
    begin
      fDatePickerDialog := TJALDatePickerDialog.JavaClass.init(TAndroidHelper.Context,
                                                               StrToJCharSequence(aBtnOKCaption),
                                                               StrToJCharSequence(aBtnCancelCaption),
                                                               StrToJCharSequence(aBtnClearCaption));
      fDatePickerDialog.setListener(aDatePickerDialogListener);
    end, fDatePickerDialogListener);
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

  // We should Create instance of UIDatePicker before creating main container.
  // Because We use UIViewPicker for determinate finally size of picker container
  // (Toolbar + Picker)
  FUIDatePicker := TUIDatePicker.Create;
  FUIDatePicker.setTimeZone(TNSTimeZone.Wrap(TNSTimeZone.OCClass.timeZoneForSecondsFromGMT(0)));
  FUIDatePicker.setDatePickerMode(UIDatePickerModeDate);

  { Subscribing to change orientation events }
  DefaultNotificationCenter.addObserver(GetObjectID, sel_getUid('DeviceOrientationChanged'), StringToID(FMXViewControllerFrameChanged), nil);

  { Creating Root view container for picker }
  FUIOverlayView := TUIView.Create;
  aUIColor := AlphaColorToUIColor($32000000);
  FUIOverlayView.setBackgroundColor(aUIColor);
  FUIOverlayView.setAutoresizingMask(UIViewAutoresizingFlexibleWidth or
                                     UIViewAutoresizingFlexibleHeight or
                                     UIViewAutoresizingFlexibleLeftMargin or
                                     UIViewAutoresizingFlexibleRightMargin or
                                     UIViewAutoresizingFlexibleTopMargin or
                                     UIViewAutoresizingFlexibleBottomMargin);
  FUIOverlayView.setFrame(CGRect.Create(0, 0, screen.Size.Width, screen.Size.Height));
  aSingleTapGestureRecognizer := TUITapGestureRecognizer.Wrap(TUITapGestureRecognizer.Alloc.initWithTarget(GetObjectID, sel_getUid('HandleTap')));
  aSingleTapGestureRecognizer.setDelegate(GetObjectID);
  aSingleTapGestureRecognizer.setNumberOfTapsRequired(1);
  try
    FUIOverlayView.addGestureRecognizer(aSingleTapGestureRecognizer);
  finally
    aSingleTapGestureRecognizer.release;
  end;

  { Creating Root view container for picker }
  FUIContainerView := TUIView.Create;
  FUIContainerView.setBackgroundColor(TUIColor.Wrap(TUIColor.OCClass.whiteColor));
  FUIContainerView.setAutoresizingMask(UIViewAutoresizingFlexibleWidth or
                                       UIViewAutoresizingFlexibleLeftMargin or
                                       UIViewAutoresizingFlexibleRightMargin);
  FUIContainerView.setFrame(GetPopupFrame);
  FUIContainerView.layer.setMasksToBounds(true);
  FUIContainerView.layer.setCornerRadius(12);

  { Creating Toolbar }
  FUIToolBar := TUIToolbar.Create;
  FUIToolBar.setAlpha(0.8);
  FUIContainerView.addSubview(FUIToolBar);
  CreateToolbarConstraint;

  { Creating Toolbar buttons }
  aButtons := TNSMutableArray.Create;
  try

    { Adding Flexible Separator }
    FUIFlexibleSepararator1 := TUIBarButtonItem.Create;
    FUIFlexibleSepararator1.initWithBarButtonSystemItem(UIBarButtonSystemItemFixedSpace, nil, nil);
    FUIFlexibleSepararator1.setWidth(5);
    aButtons.addObject(NSObjectToID(FUIFlexibleSepararator1));

    { Adding clear Button }
    if aBtnClearCaption <> '' then begin
      FUIClearButton := TUIBarButtonItem.Create;
      FUIClearButton.setTitle(StrToNSStr(aBtnClearCaption));
      FUIClearButton.setStyle(UIBarButtonItemStyleBordered);
      FUIClearButton.setTarget(Self.GetObjectID);
      FUIClearButton.setAction(sel_getUid('Clear'));
      aButtons.addObject(NSObjectToID(FUIClearButton));
    end
    else FUIClearButton := nil;

    { Adding Flexible Separator }
    FUIFlexibleSepararator2 := TUIBarButtonItem.Create;
    FUIFlexibleSepararator2.initWithBarButtonSystemItem(UIBarButtonSystemItemFlexibleSpace, nil, nil);
    aButtons.addObject(NSObjectToID(FUIFlexibleSepararator2));

    { Adding Close Button }
    if aBtnCancelCaption <> '' then begin
      FUICancelButton := TUIBarButtonItem.Create;
      FUICancelButton.setTitle(StrToNSStr(aBtnCancelCaption));
      FUICancelButton.setStyle(UIBarButtonItemStyleBordered);
      FUICancelButton.setTarget(Self.GetObjectID);
      FUICancelButton.setAction(sel_getUid('Cancel'));
      aButtons.addObject(NSObjectToID(FUICancelButton));
    end
    else FUICancelButton := nil;

    { Adding Flexible Separator }
    FUIFlexibleSepararator3 := TUIBarButtonItem.Create;
    FUIFlexibleSepararator3.initWithBarButtonSystemItem(UIBarButtonSystemItemFixedSpace, nil, nil);
    FUIFlexibleSepararator3.setWidth(28);
    aButtons.addObject(NSObjectToID(FUIFlexibleSepararator3));

    { Adding Done Button }
    FUIDoneButton := TUIBarButtonItem.Create;
    FUIDoneButton.setTitle(StrToNSStr(aBtnOKCaption));
    FUIDoneButton.setStyle(UIBarButtonItemStyleDone);
    FUIDoneButton.setTarget(Self.GetObjectID);
    FUIDoneButton.setAction(sel_getUid('Done'));
    aButtons.addObject(NSObjectToID(FUIDoneButton));

    { Adding Flexible Separator }
    FUIFlexibleSepararator4 := TUIBarButtonItem.Create;
    FUIFlexibleSepararator4.initWithBarButtonSystemItem(UIBarButtonSystemItemFixedSpace, nil, nil);
    if aBtnClearCaption <> '' then FUIFlexibleSepararator4.setWidth(5)
    else FUIFlexibleSepararator4.setWidth(15);
    aButtons.addObject(NSObjectToID(FUIFlexibleSepararator4));

    { Adding button to Toolbar }
    FUIToolBar.setItems(aButtons);

  finally
    aButtons.release;
  end;

  { Adding DatePicker }
  FUIContainerView.addSubview(FUIDatePicker);
  CreateCalendarConstraint;
  FUIOverlayView.addSubview(FUIContainerView);

  {$ENDIF}
  {$ENDREGION}

end;

{*************************************}
destructor TALDatePickerDialog.Destroy;
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  fDatePickerDialogListener.fDatePickerDialog := nil;
  TUIThreadCaller.Call<JALDatePickerDialog, TDatePickerDialogListener>(
    procedure (aDatePickerDialog: JALDatePickerDialog; aDatePickerDialogListener: TDatePickerDialogListener)
    begin
      aDatePickerDialog.setListener(nil);
      aDatePickerDialog := nil;
      TThread.Queue(nil,
        procedure
        begin
          AlFreeAndNil(aDatePickerDialogListener);
        end);
    end, fDatePickerDialog, fDatePickerDialogListener);
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}
  DefaultNotificationCenter.removeObserver(GetObjectID);
  FUIToolBar.setItems(nil);
  FUIFlexibleSepararator1.release;
  FUIDoneButton.release;
  FUIFlexibleSepararator2.release;
  if FUIClearButton <> nil then FUIClearButton.release;
  FUIFlexibleSepararator3.release;
  if FUICancelButton <> nil then FUICancelButton.release;
  FUIFlexibleSepararator4.release;
  FUIToolBar.release;
  FUIDatePicker.removeFromSuperview;
  FUIDatePicker.release;
  FUIContainerView.removeFromSuperview;
  FUIContainerView.release;
  FUIOverlayView.removeFromSuperview;
  FUIOverlayView.release;
  {$ENDIF}
  {$ENDREGION}

  inherited;

end;

{******************************************************}
procedure TALDatePickerDialog.show(const aYear: integer;
                                   const aMonth: integer;
                                   const aDayOfMonth: integer);

  {$REGION ' IOS'}
  {$IF defined(ios)}
  var aStartFrame: CGRect;
      aEndFrame: NSRect;
      aRootViewRect: NSRect;
      aKeyWin: UIWindow;
  {$ENDIF}
  {$ENDREGION}

begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  CallinUiThread(
    procedure
    begin
      // Months are indexed starting at 0, so August is month 8, or index 7
      fDatePickerDialog.show(aYear, aMonth - 1, aDayOfMonth);
    end);
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

  // Bug in using NSDate in UIDatePicker. When we set time 10:00:00,
  // UIDatePicker shows 9:59:59, but UIDatePicker.date.description shows 10:00:00
  // So we need to add 0.1 sec for it
  FUIDatePicker.setDate(DateTimeToNSDate(EncodeDate(aYear, amonth, aDayOfMonth) + 0.1 / SecsPerDay));
  SharedApplication.keyWindow.rootViewController.view.addSubview(FUIOverlayView);

  { Setting animation }
  aStartFrame := GetPopupFrame;
  aEndFrame := aStartFrame;
  aKeyWin := SharedApplication.keyWindow;
  if (aKeyWin <> nil) and (aKeyWin.rootViewController <> nil) and (aKeyWin.rootViewController.view <> nil) then begin
    aRootViewRect := SharedApplication.keyWindow.rootViewController.view.frame;
    aRootViewRect := SharedApplication.keyWindow.rootViewController.view.bounds;
    aStartFrame.origin.y := aRootViewRect.size.height;
  end;
  FUIContainerView.setFrame(aStartFrame);

  { Start Animation }
  TUIView.OCClass.beginAnimations(nil, nil);
  try
    TUIView.OCClass.setAnimationDuration(_AnimationDuration);
    FUIContainerView.setFrame(aEndFrame);
  finally
    TUIView.OCClass.commitAnimations;
  end;

  {$ENDIF}
  {$ENDREGION}

end;

{$REGION ' IOS'}
{$IF defined(ios)}

{*********************************************************}
function TALDatePickerDialog.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALDatePickerDialogActions);
end;

{****************************************************}
function TALDatePickerDialog.GetToolBarHeight: Single;
begin
  Result := _ToolBarHeight;
end;

{********************************************}
function TALDatePickerDialog.GetWidth: Single;
begin
  Result := min((Screen.Size.Width / 100) * 90, _DefaultPickerWidth);
end;

{****************************************************}
function TALDatePickerDialog.GetContentHeight: Single;
begin
  Result := _DefaultPickerHeight;
end;

{*********************************************}
function TALDatePickerDialog.GetHeight: Single;
begin
  Result := GetToolBarHeight + GetContentHeight;
end;

{*************************************************}
function TALDatePickerDialog.GetPopupFrame: NSRect;
var aRootViewRect: NSRect;
    aKeyWin: UIWindow;
begin
  Result := CGRect.Create(0, 0, GetWidth, GetHeight);
  aKeyWin := SharedApplication.keyWindow;
  if (aKeyWin <> nil) and (aKeyWin.rootViewController <> nil) and (aKeyWin.rootViewController.view <> nil) then begin
    aRootViewRect := SharedApplication.keyWindow.rootViewController.view.frame;
    aRootViewRect := SharedApplication.keyWindow.rootViewController.view.bounds;
    Result.origin.y := (aRootViewRect.size.height - GetHeight) / 2;
    Result.origin.x := (aRootViewRect.size.width - GetWidth) / 2;
  end;
end;

{**************************************************}
function TALDatePickerDialog.GetDateTime: TDateTime;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _TrimSeconds(const ADateTime: TDateTime): TDateTime;
  var Hour: Word;
      Min: Word;
      Sec: Word;
      MSec: Word;
  begin
    DecodeTime(ADateTime, Hour, Min, Sec, MSec);
    Result := DateOf(ADateTime) +  EncodeTime(Hour, Min, 0, 0);
  end;

var aDateTimeWithoutSec: TDateTime;
begin
  // Native ios UIDatePicker doesn't allow users to set second.
  // We must to trim second from datetime
  aDateTimeWithoutSec := _TrimSeconds(NSDateToDateTime(FUIDatePicker.date));
  Result := _TrimSeconds(aDateTimeWithoutSec);
end;

{***********************************}
procedure TALDatePickerDialog.Cancel;
var aDt: TdateTime;
begin
  Hide;
  aDt := GetDateTime;
  if assigned(FonClose) then fOnClose(self, mrCancel, yearOf(aDt), monthOf(aDt), DayOfTheMonth(aDt));
end;

{**********************************}
procedure TALDatePickerDialog.Clear;
var aDt: TdateTime;
begin
  Hide;
  aDt := GetDateTime;
  if assigned(FonClose) then fOnClose(self, mrClear, yearOf(aDt), monthOf(aDt), DayOfTheMonth(aDt));
end;

{*********************************}
procedure TALDatePickerDialog.Done;
var aDt: TdateTime;
begin
  Hide;
  aDt := GetDateTime;
  if assigned(FonClose) then fOnClose(self, mrOK, yearOf(aDt), monthOf(aDt), DayOfTheMonth(aDt));
end;

{*********************************}
procedure TALDatePickerDialog.Hide;
var aEndFrame: NSRect;
    aRootViewRect: NSRect;
    aKeyWin: UIWindow;
begin

  if FUIOverlayView.superview = nil then Exit;

  aEndFrame := GetPopupFrame;
  aKeyWin := SharedApplication.keyWindow;
  if (aKeyWin <> nil) and (aKeyWin.rootViewController <> nil) and (aKeyWin.rootViewController.view <> nil) then begin
    aRootViewRect := SharedApplication.keyWindow.rootViewController.view.frame;
    aRootViewRect := SharedApplication.keyWindow.rootViewController.view.bounds;
    aEndFrame.origin.y := aRootViewRect.size.height;
  end;

  { Start Slide down Animation }
  TUIView.OCClass.beginAnimations(nil, nil);
  try
    TUIView.OCClass.setAnimationDuration(_AnimationDuration);
    FUIContainerView.setFrame(aEndFrame);
    TUIView.OCClass.setAnimationDelegate(Self.GetObjectID);
    TUIView.OCClass.setAnimationDidStopSelector(sel_getUid('Hidden'))
  finally
    TUIView.OCClass.commitAnimations;
  end;

end;

{***********************************}
procedure TALDatePickerDialog.Hidden;
begin
  FUIOverlayView.removeFromSuperview;
end;

{*****************************************************}
procedure TALDatePickerDialog.DeviceOrientationChanged;
begin
  FUIContainerView.setFrame(GetPopupFrame);
end;

{**************************************}
procedure TALDatePickerDialog.HandleTap;
begin
  Cancel;
end;

{$ENDIF}
{$ENDREGION}

end.
