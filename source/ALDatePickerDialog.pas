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
    FUITitle: UILabel;
    FUITitleSeparatorLeft: UIView;
    FUITitleSeparatorRight: UIView;
    FUIToolBar: UIToolBar;
    FUICancelButton: UIBarButtonItem;
    FUIDoneButton: UIBarButtonItem;
    FUIClearButton: UIBarButtonItem;
    FUIToolbarButtonSepararator1: UIBarButtonItem;
    FUIToolbarButtonSepararator2: UIBarButtonItem;
    FUIToolbarButtonSepararator3: UIBarButtonItem;
    FUIToolbarButtonSepararator4: UIBarButtonItem;
    function GetWidth: Single;
    function GetHeight: Single;
    function GetToolBarHeight: Single;
    function GetContentHeight: Single;
    function GetTitleHeight: Single;
    function GetTitlePaddingTop: Single;
    function GetTitlePaddingBottom: Single;
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
                       const aBtnCancelCaption: string = '';
                       const aBtnClearCaption: string = '';
                       const aTitle: String = '');
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
     System.Types,
     system.Math,
     system.DateUtils,
     Macapi.Helpers,
     iOSapi.CoreGraphics,
     Macapi.ObjCRuntime,
     Fmx.platForm.iOS,
     FMX.Helpers.iOS,
     FMX.Forms,
     {$ENDIF}
     alstring,
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
var AResult: TModalResult;
begin

  {$IFDEF DEBUG}
  //i absolutly not understand why on berlin and android 6 i receive an error BUS ERROR 10 when i call allog ! this is big misterry
  //allog('TALDatePickerDialog.TDatePickerDialogListener.onClick','which: ' + alinttostrU(which) +
  //                                                              ' - year: ' + alinttostrU(year) +
  //                                                              ' - month: ' + alinttostrU(month) +
  //                                                              ' - dayOfMonth: ' + alinttostrU(dayOfMonth) +
  //                                                              ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  if assigned(fDatePickerDialog.fOnClose) then begin

    if which = TJDialogInterface.javaclass.BUTTON_POSITIVE then AResult := mrOk
    else if which = TJDialogInterface.javaclass.BUTTON_NEGATIVE then AResult := mrCancel
    else if which = TJDialogInterface.javaclass.BUTTON_NEUTRAL then AResult := mrClear
    else AResult := mrClose;

    // Months are indexed starting at 0, so August is month 8, or index 7
    fDatePickerDialog.fOnClose(fDatePickerDialog, AResult, year, month+1, dayOfMonth);

  end;

end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}
const
  _AnimationDuration = 0.3;
  _TitlePaddingLeft = 16;
  _TitlePaddingRight = 16;
  _TitlePaddingBottom = 0;
  _TitlePaddingTop = 16;

{$ENDIF}
{$ENDREGION}

{*****************************************************************}
constructor TALDatePickerDialog.create(const aBtnOKCaption: string;
                                       const aBtnCancelCaption: string = '';
                                       const aBtnClearCaption: string = '';
                                       const aTitle: String = '');

  {$REGION ' IOS'}
  {$IF defined(ios)}

  var aButtons: NSMutableArray;
      aUIColor: UIColor;
      aSingleTapGestureRecognizer: UITapGestureRecognizer;
      aConstraint: NSLayoutConstraint;

  {$ENDIF}
  {$ENDREGION}

begin

  inherited create;
  fOnClose := nil;

  {$REGION ' ANDROID'}
  {$IF defined(android)}

  fDatePickerDialogListener := TDatePickerDialogListener.Create(Self);
  fDatePickerDialog := TJALDatePickerDialog.JavaClass.init(TAndroidHelper.Context,
                                                           StrToJCharSequence(aBtnOKCaption),
                                                           StrToJCharSequence(aBtnCancelCaption),
                                                           StrToJCharSequence(aBtnClearCaption),
                                                           StrToJCharSequence(aTitle));
  fDatePickerDialog.setListener(fDatePickerDialogListener);

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
  try
    aSingleTapGestureRecognizer.setDelegate(GetObjectID);
    aSingleTapGestureRecognizer.setNumberOfTapsRequired(1);
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
  FUIContainerView.layer.setMasksToBounds(true);
  FUIContainerView.layer.setCornerRadius(12);

  { Creating Toolbar }
  FUIToolBar := TUIToolbar.Create;
  FUIToolBar.setAlpha(0.8);
  FUIContainerView.addSubview(FUIToolBar);
  FUIToolBar.setTranslatesAutoresizingMaskIntoConstraints(False);
  aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIToolBar), NSLayoutAttributeLeft,   NSLayoutRelationEqual, NSObjectToID(FUIContainerView),    NSLayoutAttributeLeft,           1, 0));
  aConstraint.setActive(True);
  aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIToolBar), NSLayoutAttributeRight,  NSLayoutRelationEqual, NSObjectToID(FUIContainerView),    NSLayoutAttributeRight,          1, 0));
  aConstraint.setActive(True);
  aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIToolBar), NSLayoutAttributeBottom, NSLayoutRelationEqual, NSObjectToID(FUIContainerView),    NSLayoutAttributeBottom,         1, 0));
  aConstraint.setActive(True);

  { Creating Toolbar buttons }
  aButtons := TNSMutableArray.Create;
  try

    { Adding Flexible Separator }
    FUIToolbarButtonSepararator1 := TUIBarButtonItem.Create;
    FUIToolbarButtonSepararator1.initWithBarButtonSystemItem(UIBarButtonSystemItemFixedSpace, nil, nil);
    FUIToolbarButtonSepararator1.setWidth(5);
    aButtons.addObject(NSObjectToID(FUIToolbarButtonSepararator1));

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
    FUIToolbarButtonSepararator2 := TUIBarButtonItem.Create;
    FUIToolbarButtonSepararator2.initWithBarButtonSystemItem(UIBarButtonSystemItemFlexibleSpace, nil, nil);
    aButtons.addObject(NSObjectToID(FUIToolbarButtonSepararator2));

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
    FUIToolbarButtonSepararator3 := TUIBarButtonItem.Create;
    FUIToolbarButtonSepararator3.initWithBarButtonSystemItem(UIBarButtonSystemItemFixedSpace, nil, nil);
    FUIToolbarButtonSepararator3.setWidth(28);
    aButtons.addObject(NSObjectToID(FUIToolbarButtonSepararator3));

    { Adding Done Button }
    FUIDoneButton := TUIBarButtonItem.Create;
    FUIDoneButton.setTitle(StrToNSStr(aBtnOKCaption));
    FUIDoneButton.setStyle(UIBarButtonItemStyleDone);
    FUIDoneButton.setTarget(Self.GetObjectID);
    FUIDoneButton.setAction(sel_getUid('Done'));
    aButtons.addObject(NSObjectToID(FUIDoneButton));

    { Adding Flexible Separator }
    FUIToolbarButtonSepararator4 := TUIBarButtonItem.Create;
    FUIToolbarButtonSepararator4.initWithBarButtonSystemItem(UIBarButtonSystemItemFixedSpace, nil, nil);
    if aBtnClearCaption <> '' then FUIToolbarButtonSepararator4.setWidth(5)
    else FUIToolbarButtonSepararator4.setWidth(15);
    aButtons.addObject(NSObjectToID(FUIToolbarButtonSepararator4));

    { Adding button to Toolbar }
    FUIToolBar.setItems(aButtons);

  finally
    aButtons.release;
  end;

  { Adding DatePicker }
  FUIContainerView.addSubview(FUIDatePicker);
  FUIDatePicker.setTranslatesAutoresizingMaskIntoConstraints(False);
  aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIDatePicker), NSLayoutAttributeLeft,   NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeLeft,           1, 0));
  aConstraint.setActive(True);
  aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIDatePicker), NSLayoutAttributeRight,  NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeRight,          1, 0));
  aConstraint.setActive(True);
  aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIDatePicker), NSLayoutAttributebottom, NSLayoutRelationEqual, NSObjectToID(FUIToolBar),       NSLayoutAttributetop,            1, 0));
  aConstraint.setActive(True);

  { Creating the title }
  if aTitle <> '' then begin

    FUITitle := TUILabel.Wrap(TUILabel.Alloc.initWithFrame(RectToNSRect(TRect.Create(0, 0, floor(getWidth) - _TitlePaddingleft - _TitlePaddingRight, 10000))));
    FUITitle.setTextAlignment(UITextAlignmentCenter);
    FUITitle.setLineBreakMode(UILineBreakModeWordWrap);
    FUITitle.setFont(FUITitle.font.fontWithSize(TUIFont.OCClass.labelFontSize + 5));
    FUITitle.setNumberOfLines(0);
    FUITitle.setText(StrToNsStr(aTitle));

    FUITitleSeparatorLeft := TUIView.Create;
    FUIContainerView.addSubview(FUITitleSeparatorLeft);
    FUITitleSeparatorLeft.setTranslatesAutoresizingMaskIntoConstraints(False);
    aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorLeft), NSLayoutAttributeLeft,   NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeLeft,           1, 0));
    aConstraint.setActive(True);
    aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorLeft), NSLayoutAttributeTop,    NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeTop,            1, 0));
    aConstraint.setActive(True);
    aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorLeft), NSLayoutAttributebottom, NSLayoutRelationEqual, NSObjectToID(FUIDatePicker),    NSLayoutAttributeBottom,         1, 0));
    aConstraint.setActive(True);
    aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorLeft), NSLayoutAttributeWidth,  NSLayoutRelationEqual, nil,                            NSLayoutAttributeNotAnAttribute, 1, _TitlePaddingleft));
    aConstraint.setActive(True);

    FUITitleSeparatorRight := TUIView.Create;
    FUIContainerView.addSubview(FUITitleSeparatorRight);
    FUITitleSeparatorRight.setTranslatesAutoresizingMaskIntoConstraints(False);
    aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorRight), NSLayoutAttributeRight,   NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeRight,          1, 0));
    aConstraint.setActive(True);
    aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorRight), NSLayoutAttributeTop,     NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeTop,            1, 0));
    aConstraint.setActive(True);
    aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorRight), NSLayoutAttributebottom,  NSLayoutRelationEqual, NSObjectToID(FUIDatePicker),    NSLayoutAttributeBottom,         1, 0));
    aConstraint.setActive(True);
    aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorRight), NSLayoutAttributeWidth,   NSLayoutRelationEqual, nil,                            NSLayoutAttributeNotAnAttribute, 1, _TitlePaddingRight));
    aConstraint.setActive(True);

    FUIContainerView.addSubview(FUITitle);
    FUITitle.setTranslatesAutoresizingMaskIntoConstraints(False);
    aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitle), NSLayoutAttributeLeft,   NSLayoutRelationEqual, NSObjectToID(FUITitleSeparatorLeft),    NSLayoutAttributeRight,  1, 0));
    aConstraint.setActive(True);
    aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitle), NSLayoutAttributeRight,  NSLayoutRelationEqual, NSObjectToID(FUITitleSeparatorRight),   NSLayoutAttributeLeft,   1, 0));
    aConstraint.setActive(True);
    aConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitle), NSLayoutAttributebottom, NSLayoutRelationEqual, NSObjectToID(FUIDatePicker),            NSLayoutAttributetop,    1, 0));
    aConstraint.setActive(True);

  end
  else begin

    FUITitle := nil;
    FUITitleSeparatorLeft := nil;
    FUITitleSeparatorRight := nil;

  end;

  { finishing the layout }
  FUIOverlayView.addSubview(FUIContainerView);

  {$ENDIF}
  {$ENDREGION}

end;

{*************************************}
destructor TALDatePickerDialog.Destroy;
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

  fDatePickerDialog.setListener(nil);
  fDatePickerDialog := nil;
  AlFreeAndNil(fDatePickerDialogListener);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}
  DefaultNotificationCenter.removeObserver(GetObjectID);
  FUIToolBar.setItems(nil);
  FUIToolbarButtonSepararator1.release;
  FUIDoneButton.release;
  FUIToolbarButtonSepararator2.release;
  if FUIClearButton <> nil then FUIClearButton.release;
  FUIToolbarButtonSepararator3.release;
  if FUICancelButton <> nil then FUICancelButton.release;
  FUIToolbarButtonSepararator4.release;
  FUIToolBar.release;
  FUIDatePicker.removeFromSuperview;
  FUIDatePicker.release;
  if FUITitle <> nil then begin
    FUITitle.removeFromSuperview;
    FUITitle.release;
  end;
  if FUITitleSeparatorLeft <> nil then begin
    FUITitleSeparatorLeft.removeFromSuperview;
    FUITitleSeparatorLeft.release;
  end;
  if FUITitleSeparatorRight <> nil then begin
    FUITitleSeparatorRight.removeFromSuperview;
    FUITitleSeparatorRight.release;
  end;
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

  // Months are indexed starting at 0, so August is month 8, or index 7
  fDatePickerDialog.show(aYear, aMonth - 1, aDayOfMonth);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

  // Bug in using NSDate in UIDatePicker. When we set time 10:00:00,
  // UIDatePicker shows 9:59:59, but UIDatePicker.date.description shows 10:00:00
  // So we need to add 0.1 sec for it
  FUIDatePicker.setDate(DateTimeToNSDate(EncodeDate(aYear, amonth, aDayOfMonth) + 0.1 / SecsPerDay));
  SharedApplication.keyWindow.rootViewController.view.addSubview(FUIOverlayView);

  // size everything
  FUIToolBar.sizeToFit;
  FUIDatePicker.sizeToFit;
  if FUITitle <> nil then FUITitle.sizeToFit;
  FUIContainerView.sizeToFit;

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
  result := FUIToolBar.frame.size.height;
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.GetToolBarHeight','result: ' + alFloatToStrU(result, ALDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{****************************************************}
function TALDatePickerDialog.GetContentHeight: Single;
begin
  Result := FUIDatePicker.frame.size.height;
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.GetContentHeight','result: ' + alFloatToStrU(result, ALDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{**************************************************}
function TALDatePickerDialog.GetTitleHeight: Single;
begin
  if assigned(FUITitle) then result := FUITitle.frame.size.height
  else Result := 0;
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.GetTitleHeight','result: ' + alFloatToStrU(result, ALDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{******************************************************}
function TALDatePickerDialog.GetTitlePaddingTop: Single;
begin
  if assigned(FUITitle) then result := _TitlePaddingTop
  else Result := 0;
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.GetTitlePaddingTop','result: ' + alFloatToStrU(result, ALDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{*********************************************************}
function TALDatePickerDialog.GetTitlePaddingBottom: Single;
begin
  if assigned(FUITitle) then result := _TitlePaddingBottom
  else Result := 0;
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.GetTitlePaddingBottom','result: ' + alFloatToStrU(result, ALDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{********************************************}
function TALDatePickerDialog.GetWidth: Single;
begin
  result := FUIDatePicker.frame.size.width;
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.GetWidth','result: ' + alFloatToStrU(result, ALDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
end;

{*********************************************}
function TALDatePickerDialog.GetHeight: Single;
begin
  Result := GetTitlePaddingTop +
              GetTitleHeight +
                GetTitlePaddingBottom +
                  GetToolBarHeight +
                    GetContentHeight;
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.GetHeight','result: ' + alFloatToStrU(result, ALDefaultFormatSettingsU), TalLogType.VERBOSE);
  {$ENDIF}
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
  FUIToolBar.sizeToFit;
  FUIDatePicker.sizeToFit;
  if FUITitle <> nil then FUITitle.sizeToFit;
  FUIContainerView.sizeToFit;
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
