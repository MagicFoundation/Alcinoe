unit Alcinoe.FMX.DatePickerDialog;

interface

{$I Alcinoe.inc}

uses
  {$IF defined(android)}
  Androidapi.JNIBridge,
  Alcinoe.AndroidApi.Widget,
  {$ENDIF}
  {$IF defined(ios)}
  System.TypInfo,
  Macapi.ObjectiveC,
  iOSapi.uikit,
  iOSapi.Foundation,
  {$ENDIF}
  System.UITypes,
  Alcinoe.fmx.Controls,
  Alcinoe.fmx.Dialogs;

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

  {****************}
  {$IF defined(IOS)}
  TALDatePickerDialog = Class(TOCLocal)
  {$ELSE}
  TALDatePickerDialog = Class(Tobject)
  {$ENDIF}
  public
    type
      // --------
      // TBuilder
      TButtonKind = (
        Done,
        Cancel,
        Clear);
      // ----------------
      // TOnActionRefProc
      TOnActionRefProc = reference to procedure(Const ADialog: TALDatePickerDialog; const AButtonKind: TButtonKind; const ADate: TDate);
      // ----------------
      // TOnActionObjProc
      TOnActionObjProc = procedure(Const ADialog: TALDatePickerDialog; const AButtonKind: TButtonKind; const ADate: TDate) of object;
      // --------
      // TBuilder
      TBuilder = Class(TObject)
      private
        FTitle: String;
        FMinDate: TDate;
        FMaxDate: TDate;
        FDate: TDate;
        FDoneButtonCaption: string;
        FCancelButtonCaption: string;
        FClearButtonCaption: string;
        FOnActionRefProc: TOnActionRefProc;
        FOnActionObjProc: TOnActionObjProc;
      public
        constructor Create;
        function SetTitle(const AValue: String): TBuilder;
        function SetMinDate(const AValue: TDate): TBuilder;
        function SetMaxDate(const AValue: TDate): TBuilder;
        function SetDate(const AValue: TDate): TBuilder;
        function AddButton(const ACaption: String; const AKind: TButtonKind): TBuilder;
        function SetOnActionCallback(const AValue: TOnActionRefProc): TBuilder; overload;
        function SetOnActionCallback(const AValue: TOnActionObjProc): TBuilder; overload;
        /// <summary>
        ///   The Builder instance will be released during this operation.
        ///   If a dialog is already being shown and <c>AForceImmediateShow</c> is <c>false</c>,
        ///   the new dialog will be queued and displayed after the current one is closed.
        ///   If <c>AForceImmediateShow</c> is <c>true</c>, the current dialog will be closed
        ///   immediately and replaced by the new one.
        ///   Note: Showing a dialog on top of another is considered a poor UI practice.
        /// </summary>
        procedure Show(const AForceImmediateShow: Boolean = True);
      end;
  public
    /// <summary>
    ///   Creates a builder object. The builder will automatically be
    ///   released when calling the Show method.
    ///   If you do not call Show, you are responsible for releasing the builder manually.
    /// </summary>
    class function Builder: TBuilder;
  private

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TDatePickerDialogListener = class(TJavaLocal, JALDatePickerDialogListener)
      private
        [Weak] FDatePickerDialog: TALDatePickerDialog;
      public
        constructor Create(const ADatePickerDialog: TALDatePickerDialog);
        procedure onBtnClick(which: integer; year: integer; month: integer; dayOfMonth: integer); cdecl;
      end;

    {$ENDIF}
    {$ENDREGION}

  private

    FMustNotifyCustomDialogFinished: Boolean;
    FMinDate: TDate;
    FMaxDate: TDate;
    FOnActionRefProc: TOnActionRefProc;
    FOnActionObjProc: TOnActionObjProc;

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    FDatePickerDialogListener: TDatePickerDialogListener;
    FDatePickerDialog: JALDatePickerDialog;
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
    constructor create(
                  const ATitle: String;
                  const ADoneButtonCaption: string;
                  const ACancelButtonCaption: string = '';
                  const AClearButtonCaption: string = '');
    destructor Destroy; override;
    procedure Show(const ADate: TDate);
    property MinDate: TDate read FMinDate write FMinDate;
    property MaxDate: TDate read FMaxDate write FMaxDate;
    property OnAction: TOnActionObjProc read FOnActionObjProc write FOnActionObjProc;
  End;

implementation

uses
  system.Classes,
  system.SysUtils,
  System.DateUtils,
  {$IF defined(android)}
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android,
  Androidapi.Helpers,
  {$ENDIF}
  {$IF defined(ios)}
  System.Types,
  system.Math,
  Macapi.Helpers,
  iOSapi.CoreGraphics,
  Macapi.ObjCRuntime,
  Fmx.platForm.iOS,
  FMX.Helpers.iOS,
  FMX.Forms,
  {$ENDIF}
  Alcinoe.Localization,
  Alcinoe.StringUtils,
  Alcinoe.Common;

{$REGION ' ANDROID'}
{$IF defined(android)}

{*************************************************************************************************************}
constructor TALDatePickerDialog.TDatePickerDialogListener.Create(const ADatePickerDialog: TALDatePickerDialog);
begin
  inherited Create;
  FDatePickerDialog := ADatePickerDialog;
end;

{*************************************************************************************************************************************}
procedure TALDatePickerDialog.TDatePickerDialogListener.onBtnClick(which: integer; year: integer; month: integer; dayOfMonth: integer);
begin

  if FDatePickerDialog.FMustNotifyCustomDialogFinished then TALDialogManager.Instance.CustomDialogFinished;

  var LButtonKind: TButtonKind;
  if which = TJDialogInterface.javaclass.BUTTON_POSITIVE then LButtonKind := TButtonKind.Done
  else if which = TJDialogInterface.javaclass.BUTTON_NEGATIVE then LButtonKind := TButtonKind.Cancel
  else if which = TJDialogInterface.javaclass.BUTTON_NEUTRAL then LButtonKind := TButtonKind.Clear
  else LButtonKind := TButtonKind.Cancel;

  // Months are indexed starting at 0
  if assigned(FDatePickerDialog.FOnActionRefProc) then
    FDatePickerDialog.FOnActionRefProc(FDatePickerDialog, LButtonKind, EncodeDate(year, month+1, dayOfMonth))
  else if assigned(FDatePickerDialog.FOnActionObjProc) then
    FDatePickerDialog.FOnActionObjProc(FDatePickerDialog, LButtonKind, EncodeDate(year, month+1, dayOfMonth));

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

{**********************************************}
constructor TALDatePickerDialog.TBuilder.Create;
begin
  Inherited create;
  FTitle := '';
  FMinDate := ALNullDate;
  FMaxDate := ALNullDate;
  FDate := DateOf(ALUtcNow);
  FDoneButtonCaption := '';
  FCancelButtonCaption := '';
  FClearButtonCaption := '';
  FOnActionRefProc := nil;
  FOnActionObjProc := nil;
end;

{*****************************************************************************}
function TALDatePickerDialog.TBuilder.SetTitle(const AValue: String): TBuilder;
begin
  FTitle := AValue;
  Result := Self;
end;

{******************************************************************************}
function TALDatePickerDialog.TBuilder.SetMinDate(const AValue: TDate): TBuilder;
begin
  FMinDate := AValue;
  Result := Self;
end;

{******************************************************************************}
function TALDatePickerDialog.TBuilder.SetMaxDate(const AValue: TDate): TBuilder;
begin
  FMaxDate := AValue;
  Result := Self;
end;

{***************************************************************************}
function TALDatePickerDialog.TBuilder.SetDate(const AValue: TDate): TBuilder;
begin
  FDate := AValue;
  Result := Self;
end;

{**********************************************************************************************************}
function TALDatePickerDialog.TBuilder.AddButton(const ACaption: String; const AKind: TButtonKind): TBuilder;
begin
  case AKind of
    TButtonKind.Done: FDoneButtonCaption := ACaption;
    TButtonKind.Cancel: FCancelButtonCaption := ACaption;
    TButtonKind.Clear: FClearButtonCaption := ACaption;
    else raise Exception.Create('Error 1935FDF7-4823-4850-8F62-A4DBACE3DD41');
  end;
  Result := Self;
end;

{**************************************************************************************************}
function TALDatePickerDialog.TBuilder.SetOnActionCallback(const AValue: TOnActionRefProc): TBuilder;
begin
  FOnActionRefProc := AValue;
  Result := Self;
end;

{**************************************************************************************************}
function TALDatePickerDialog.TBuilder.SetOnActionCallback(const AValue: TOnActionObjProc): TBuilder;
begin
  FOnActionObjProc := AValue;
  Result := Self;
end;

{*************************************************************************************}
procedure TALDatePickerDialog.TBuilder.Show(const AForceImmediateShow: Boolean = True);
begin
  if (not assigned(FOnActionRefProc)) and
     (not assigned(FOnActionObjProc)) then raise Exception.Create('The date picker dialog requires an action callback before it can be shown');

  {$IF defined(IOS) or defined(ANDROID)}

  var LDatePickerDialog := TALDatePickerDialog.create(
                             FTitle, // const ATitle: String;
                             FDoneButtonCaption, // const ADoneButtonCaption: string;
                             FCancelButtonCaption, // const ACancelButtonCaption: string = '';
                             FClearButtonCaption); // const AClearButtonCaption: string = '');
  LDatePickerDialog.FMustNotifyCustomDialogFinished := True;
  LDatePickerDialog.FMinDate := FMinDate;
  LDatePickerDialog.FMaxDate := FMaxDate;
  LDatePickerDialog.FOnActionRefProc := FOnActionRefProc;
  LDatePickerDialog.FOnActionObjProc := FOnActionObjProc;
  var LDate := FDate;

  TALDialog.Builder
    .SetCustomDialogProc(
       procedure
       begin
         LDatePickerDialog.show(LDate);
       end)
    .SetOnClosedCallback(
       procedure(Const ADialog: TALDialog)
       begin
         // In iOS LDatePickerDialog is destroyed in it's "Hidden" procedure
         {$IF not defined(IOS)}
         TThread.ForceQueue(nil,
           procedure
           begin
             ALFreeAndNil(LDatePickerDialog);
           end);
         {$ENDIF}
       end)
    .Show(AForceImmediateShow);

  {$ENDIF}

  Free;
end;

{***************************************************}
class function TALDatePickerDialog.Builder: TBuilder;
begin
  Result := TBuilder.Create;
end;

{*************************************}
constructor TALDatePickerDialog.create(
              const ATitle: String;
              const ADoneButtonCaption: string;
              const ACancelButtonCaption: string = '';
              const AClearButtonCaption: string = '');
begin

  inherited create;
  FMustNotifyCustomDialogFinished := False;
  FMinDate := ALNullDate;
  FMaxDate := ALNullDate;
  FOnActionRefProc := nil;
  FOnActionObjProc := nil;

  {$REGION ' ANDROID'}
  {$IF defined(android)}

  FDatePickerDialogListener := TDatePickerDialogListener.Create(Self);
  FDatePickerDialog := TJALDatePickerDialog.JavaClass.init(
                         TAndroidHelper.Context,
                         StrToJCharSequence(ADoneButtonCaption),
                         StrToJCharSequence(ACancelButtonCaption),
                         StrToJCharSequence(AClearButtonCaption),
                         StrToJCharSequence(ATitle));
  FDatePickerDialog.setListener(FDatePickerDialogListener);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

  // We should Create instance of UIDatePicker before creating main container.
  // Because We use UIViewPicker for determinate finally size of picker container
  // (Toolbar + Picker)
  FUIDatePicker := TUIDatePicker.Create;
  FUIDatePicker.setTimeZone(TNSTimeZone.Wrap(TNSTimeZone.OCClass.timeZoneForSecondsFromGMT(0)));
  if TOSVersion.Check(13, 4) then
    FUIDatePicker.setPreferredDatePickerStyle(UIDatePickerStyleWheels);
  FUIDatePicker.setDatePickerMode(UIDatePickerModeDate);

  { Subscribing to change orientation events }
  DefaultNotificationCenter.addObserver(GetObjectID, sel_getUid('DeviceOrientationChanged'), StrToNSStr(FMXViewControllerFrameChanged), nil);

  { Creating Root view container for picker }
  FUIOverlayView := TUIView.Create;
  FUIOverlayView.setAutoresizingMask(
    UIViewAutoresizingFlexibleWidth or
    UIViewAutoresizingFlexibleHeight or
    UIViewAutoresizingFlexibleLeftMargin or
    UIViewAutoresizingFlexibleRightMargin or
    UIViewAutoresizingFlexibleTopMargin or
    UIViewAutoresizingFlexibleBottomMargin);
  FUIOverlayView.setFrame(CGRect.Create(0, 0, screen.Size.Width, screen.Size.Height));
  var LSingleTapGestureRecognizer := TUITapGestureRecognizer.Wrap(TUITapGestureRecognizer.Alloc.initWithTarget(GetObjectID, sel_getUid('HandleTap')));
  try
    LSingleTapGestureRecognizer.setDelegate(GetObjectID);
    LSingleTapGestureRecognizer.setNumberOfTapsRequired(1);
    FUIOverlayView.addGestureRecognizer(LSingleTapGestureRecognizer);
  finally
    LSingleTapGestureRecognizer.release;
  end;

  { Creating Root view container for picker }
  FUIContainerView := TUIView.Create;
  FUIContainerView.setBackgroundColor(TUIColor.OCClass.systemBackgroundColor);
  FUIContainerView.setAutoresizingMask(
    UIViewAutoresizingFlexibleWidth or
    UIViewAutoresizingFlexibleLeftMargin or
    UIViewAutoresizingFlexibleRightMargin);
  FUIContainerView.layer.setMasksToBounds(true);
  FUIContainerView.layer.setCornerRadius(12);

  { Creating Toolbar }
  FUIToolBar := TUIToolbar.Create;
  FUIToolBar.setAlpha(0.8);
  FUIContainerView.addSubview(FUIToolBar);
  FUIToolBar.setTranslatesAutoresizingMaskIntoConstraints(False);
  var LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIToolBar), NSLayoutAttributeLeft,   NSLayoutRelationEqual, NSObjectToID(FUIContainerView),    NSLayoutAttributeLeft,           1, 0));
  LConstraint.setActive(True);
  LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIToolBar), NSLayoutAttributeRight,  NSLayoutRelationEqual, NSObjectToID(FUIContainerView),    NSLayoutAttributeRight,          1, 0));
  LConstraint.setActive(True);
  LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIToolBar), NSLayoutAttributeBottom, NSLayoutRelationEqual, NSObjectToID(FUIContainerView),    NSLayoutAttributeBottom,         1, 0));
  LConstraint.setActive(True);

  { Creating Toolbar buttons }
  var LButtons := TNSMutableArray.Create;
  try

    { Adding Flexible Separator }
    FUIToolbarButtonSepararator1 := TUIBarButtonItem.Create;
    FUIToolbarButtonSepararator1.initWithBarButtonSystemItem(UIBarButtonSystemItemFixedSpace, nil, nil);
    FUIToolbarButtonSepararator1.setWidth(5);
    LButtons.addObject(NSObjectToID(FUIToolbarButtonSepararator1));

    { Adding clear Button }
    if AClearButtonCaption <> '' then begin
      FUIClearButton := TUIBarButtonItem.Create;
      FUIClearButton.setTitle(StrToNSStr(AClearButtonCaption));
      FUIClearButton.setStyle(UIBarButtonItemStyleBordered);
      FUIClearButton.setTarget(Self.GetObjectID);
      FUIClearButton.setAction(sel_getUid('Clear'));
      LButtons.addObject(NSObjectToID(FUIClearButton));
    end
    else FUIClearButton := nil;

    { Adding Flexible Separator }
    FUIToolbarButtonSepararator2 := TUIBarButtonItem.Create;
    FUIToolbarButtonSepararator2.initWithBarButtonSystemItem(UIBarButtonSystemItemFlexibleSpace, nil, nil);
    LButtons.addObject(NSObjectToID(FUIToolbarButtonSepararator2));

    { Adding Close Button }
    if ACancelButtonCaption <> '' then begin
      FUICancelButton := TUIBarButtonItem.Create;
      FUICancelButton.setTitle(StrToNSStr(ACancelButtonCaption));
      FUICancelButton.setStyle(UIBarButtonItemStyleBordered);
      FUICancelButton.setTarget(Self.GetObjectID);
      FUICancelButton.setAction(sel_getUid('Cancel'));
      LButtons.addObject(NSObjectToID(FUICancelButton));
    end
    else FUICancelButton := nil;

    { Adding Flexible Separator }
    FUIToolbarButtonSepararator3 := TUIBarButtonItem.Create;
    FUIToolbarButtonSepararator3.initWithBarButtonSystemItem(UIBarButtonSystemItemFixedSpace, nil, nil);
    FUIToolbarButtonSepararator3.setWidth(28);
    LButtons.addObject(NSObjectToID(FUIToolbarButtonSepararator3));

    { Adding Done Button }
    FUIDoneButton := TUIBarButtonItem.Create;
    FUIDoneButton.setTitle(StrToNSStr(ADoneButtonCaption));
    FUIDoneButton.setStyle(UIBarButtonItemStyleDone);
    FUIDoneButton.setTarget(Self.GetObjectID);
    FUIDoneButton.setAction(sel_getUid('Done'));
    LButtons.addObject(NSObjectToID(FUIDoneButton));

    { Adding Flexible Separator }
    FUIToolbarButtonSepararator4 := TUIBarButtonItem.Create;
    FUIToolbarButtonSepararator4.initWithBarButtonSystemItem(UIBarButtonSystemItemFixedSpace, nil, nil);
    if AClearButtonCaption <> '' then FUIToolbarButtonSepararator4.setWidth(5)
    else FUIToolbarButtonSepararator4.setWidth(15);
    LButtons.addObject(NSObjectToID(FUIToolbarButtonSepararator4));

    { Adding button to Toolbar }
    FUIToolBar.setItems(LButtons);

  finally
    LButtons.release;
  end;

  { Adding DatePicker }
  FUIContainerView.addSubview(FUIDatePicker);
  FUIDatePicker.setTranslatesAutoresizingMaskIntoConstraints(False);
  LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIDatePicker), NSLayoutAttributeLeft,   NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeLeft,           1, 0));
  LConstraint.setActive(True);
  LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIDatePicker), NSLayoutAttributeRight,  NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeRight,          1, 0));
  LConstraint.setActive(True);
  LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUIDatePicker), NSLayoutAttributebottom, NSLayoutRelationEqual, NSObjectToID(FUIToolBar),       NSLayoutAttributetop,            1, 0));
  LConstraint.setActive(True);

  { Creating the title }
  if ATitle <> '' then begin

    FUITitle := TUILabel.Wrap(TUILabel.Alloc.initWithFrame(RectToNSRect(TRect.Create(0, 0, floor(getWidth) - _TitlePaddingleft - _TitlePaddingRight, 10000))));
    FUITitle.setTextAlignment(NSTextAlignmentCenter);
    FUITitle.setLineBreakMode(NSLineBreakByWordWrapping);
    FUITitle.setFont(FUITitle.font.fontWithSize(TUIFont.OCClass.labelFontSize + 5));
    FUITitle.setNumberOfLines(0);
    FUITitle.setText(StrToNsStr(ATitle));

    FUITitleSeparatorLeft := TUIView.Create;
    FUIContainerView.addSubview(FUITitleSeparatorLeft);
    FUITitleSeparatorLeft.setTranslatesAutoresizingMaskIntoConstraints(False);
    LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorLeft), NSLayoutAttributeLeft,   NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeLeft,           1, 0));
    LConstraint.setActive(True);
    LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorLeft), NSLayoutAttributeTop,    NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeTop,            1, 0));
    LConstraint.setActive(True);
    LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorLeft), NSLayoutAttributebottom, NSLayoutRelationEqual, NSObjectToID(FUIDatePicker),    NSLayoutAttributeBottom,         1, 0));
    LConstraint.setActive(True);
    LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorLeft), NSLayoutAttributeWidth,  NSLayoutRelationEqual, nil,                            NSLayoutAttributeNotAnAttribute, 1, _TitlePaddingleft));
    LConstraint.setActive(True);

    FUITitleSeparatorRight := TUIView.Create;
    FUIContainerView.addSubview(FUITitleSeparatorRight);
    FUITitleSeparatorRight.setTranslatesAutoresizingMaskIntoConstraints(False);
    LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorRight), NSLayoutAttributeRight,   NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeRight,          1, 0));
    LConstraint.setActive(True);
    LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorRight), NSLayoutAttributeTop,     NSLayoutRelationEqual, NSObjectToID(FUIContainerView), NSLayoutAttributeTop,            1, 0));
    LConstraint.setActive(True);
    LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorRight), NSLayoutAttributebottom,  NSLayoutRelationEqual, NSObjectToID(FUIDatePicker),    NSLayoutAttributeBottom,         1, 0));
    LConstraint.setActive(True);
    LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitleSeparatorRight), NSLayoutAttributeWidth,   NSLayoutRelationEqual, nil,                            NSLayoutAttributeNotAnAttribute, 1, _TitlePaddingRight));
    LConstraint.setActive(True);

    FUIContainerView.addSubview(FUITitle);
    FUITitle.setTranslatesAutoresizingMaskIntoConstraints(False);
    LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitle), NSLayoutAttributeLeft,   NSLayoutRelationEqual, NSObjectToID(FUITitleSeparatorLeft),    NSLayoutAttributeRight,  1, 0));
    LConstraint.setActive(True);
    LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitle), NSLayoutAttributeRight,  NSLayoutRelationEqual, NSObjectToID(FUITitleSeparatorRight),   NSLayoutAttributeLeft,   1, 0));
    LConstraint.setActive(True);
    LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FUITitle), NSLayoutAttributebottom, NSLayoutRelationEqual, NSObjectToID(FUIDatePicker),            NSLayoutAttributetop,    1, 0));
    LConstraint.setActive(True);

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

  FDatePickerDialog.setListener(nil);
  FDatePickerDialog := nil;
  AlFreeAndNil(FDatePickerDialogListener);

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

{*****************************************************}
procedure TALDatePickerDialog.Show(const ADate: TDate);
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

  // Months are indexed starting at 0
  if Not SameDate(FMinDate, ALNullDate) then FDatePickerDialog.setMinDate(YearOf(FMinDate), MonthOf(FMinDate) - 1, DayOf(FMinDate));
  if Not SameDate(FMaxDate, ALNullDate) then FDatePickerDialog.setMaxDate(YearOf(FMaxDate), MonthOf(FMaxDate) - 1, DayOf(FMaxDate));
  FDatePickerDialog.show(YearOf(ADate), MonthOf(ADate) - 1, DayOf(ADate));

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

  if Not SameDate(FMinDate, ALNullDate) then FUIDatePicker.setMinimumDate(DateTimeToNSDate(DateOf(FMinDate)))
  else FUIDatePicker.setMinimumDate(nil);
  if Not SameDate(FMaxDate, ALNullDate) then FUIDatePicker.setMaximumDate(DateTimeToNSDate(DateOf(FMaxDate)))
  else FUIDatePicker.setMaximumDate(nil);

  // Bug in using NSDate in UIDatePicker. When we set time 10:00:00,
  // UIDatePicker shows 9:59:59, but UIDatePicker.date.description shows 10:00:00
  // So we need to add 0.1 sec for it
  FUIDatePicker.setDate(DateTimeToNSDate(ADate + (0.1 / SecsPerDay)));
  SharedApplication.keyWindow.rootViewController.view.addSubview(FUIOverlayView);

  // size everything
  FUIToolBar.sizeToFit;
  FUIDatePicker.sizeToFit;
  if FUITitle <> nil then FUITitle.sizeToFit;
  FUIContainerView.sizeToFit;

  { Setting animation }
  var LStartFrame: CGRect := GetPopupFrame;
  var LEndFrame: NSRect := LStartFrame;
  var LKeyWin: UIWindow := SharedApplication.keyWindow;
  if (LKeyWin <> nil) and (LKeyWin.rootViewController <> nil) and (LKeyWin.rootViewController.view <> nil) then begin
    var LRootViewRect: NSRect := SharedApplication.keyWindow.rootViewController.view.frame;
    LRootViewRect := SharedApplication.keyWindow.rootViewController.view.bounds;
    LStartFrame.origin.y := LRootViewRect.size.height;
  end;
  FUIContainerView.setFrame(LStartFrame);

  { Start Animation }
  TUIView.OCClass.beginAnimations(nil, nil);
  try
    TUIView.OCClass.setAnimationDuration(_AnimationDuration);
    FUIContainerView.setFrame(LEndFrame);
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
  allog('TALDatePickerDialog.GetToolBarHeight','result: ' + ALFloatToStrW(result));
  {$ENDIF}
end;

{****************************************************}
function TALDatePickerDialog.GetContentHeight: Single;
begin
  Result := FUIDatePicker.frame.size.height;
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.GetContentHeight','result: ' + ALFloatToStrW(result));
  {$ENDIF}
end;

{**************************************************}
function TALDatePickerDialog.GetTitleHeight: Single;
begin
  if assigned(FUITitle) then result := FUITitle.frame.size.height
  else Result := 0;
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.GetTitleHeight','result: ' + ALFloatToStrW(result));
  {$ENDIF}
end;

{******************************************************}
function TALDatePickerDialog.GetTitlePaddingTop: Single;
begin
  if assigned(FUITitle) then result := _TitlePaddingTop
  else Result := 0;
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.GetTitlePaddingTop','result: ' + ALFloatToStrW(result));
  {$ENDIF}
end;

{*********************************************************}
function TALDatePickerDialog.GetTitlePaddingBottom: Single;
begin
  if assigned(FUITitle) then result := _TitlePaddingBottom
  else Result := 0;
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.GetTitlePaddingBottom','result: ' + ALFloatToStrW(result));
  {$ENDIF}
end;

{********************************************}
function TALDatePickerDialog.GetWidth: Single;
begin
  result := FUIDatePicker.frame.size.width;
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.GetWidth','result: ' + ALFloatToStrW(result));
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
  allog('TALDatePickerDialog.GetHeight','result: ' + ALFloatToStrW(result));
  {$ENDIF}
end;

{*************************************************}
function TALDatePickerDialog.GetPopupFrame: NSRect;
begin
  Result := CGRect.Create(0, 0, GetWidth, GetHeight);
  var LKeyWin: UIWindow := SharedApplication.keyWindow;
  if (LKeyWin <> nil) and (LKeyWin.rootViewController <> nil) and (LKeyWin.rootViewController.view <> nil) then begin
    var LRootViewRect: NSRect := SharedApplication.keyWindow.rootViewController.view.frame;
    LRootViewRect := SharedApplication.keyWindow.rootViewController.view.bounds;
    Result.origin.y := (LRootViewRect.size.height - GetHeight) / 2;
    Result.origin.x := (LRootViewRect.size.width - GetWidth) / 2;
  end;
end;

{**************************************************}
function TALDatePickerDialog.GetDateTime: TDateTime;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _TrimSeconds(const ADateTime: TDateTime): TDateTime;
  begin
    var LHour: Word;
    var LMin: Word;
    var LSec: Word;
    var LMSec: Word;
    DecodeTime(ADateTime, LHour, LMin, LSec, LMSec);
    Result := DateOf(ADateTime) +  EncodeTime(LHour, LMin, 0, 0);
  end;

begin
  // Native ios UIDatePicker doesn't allow users to set second.
  // We must to trim second from datetime
  var LDateTimeWithoutSec: TDateTime := _TrimSeconds(NSDateToDateTime(FUIDatePicker.date));
  Result := _TrimSeconds(LDateTimeWithoutSec);
end;

{***********************************}
procedure TALDatePickerDialog.Cancel;
begin
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.Cancel');
  {$ENDIF}
  Hide;
  if FMustNotifyCustomDialogFinished then TALDialogManager.Instance.CustomDialogFinished;
  if assigned(FOnActionRefProc) then
    FOnActionRefProc(Self, TButtonKind.Cancel, GetDateTime)
  else if assigned(FOnActionObjProc) then
    FOnActionObjProc(Self, TButtonKind.Cancel, GetDateTime);
end;

{**********************************}
procedure TALDatePickerDialog.Clear;
begin
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.Clear');
  {$ENDIF}
  Hide;
  if FMustNotifyCustomDialogFinished then TALDialogManager.Instance.CustomDialogFinished;
  if assigned(FOnActionRefProc) then
    FOnActionRefProc(Self, TButtonKind.Clear, GetDateTime)
  else if assigned(FOnActionObjProc) then
    FOnActionObjProc(Self, TButtonKind.Clear, GetDateTime);
end;

{*********************************}
procedure TALDatePickerDialog.Done;
begin
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.Done');
  {$ENDIF}
  Hide;
  if FMustNotifyCustomDialogFinished then TALDialogManager.Instance.CustomDialogFinished;
  if assigned(FOnActionRefProc) then
    FOnActionRefProc(Self, TButtonKind.Done, GetDateTime)
  else if assigned(FOnActionObjProc) then
    FOnActionObjProc(Self, TButtonKind.Done, GetDateTime);
end;

{*********************************}
procedure TALDatePickerDialog.Hide;
begin

  if FUIOverlayView.superview = nil then Exit;

  var LEndFrame: NSRect := GetPopupFrame;
  var LKeyWin: UIWindow := SharedApplication.keyWindow;
  if (LKeyWin <> nil) and (LKeyWin.rootViewController <> nil) and (LKeyWin.rootViewController.view <> nil) then begin
    var LRootViewRect: NSRect := SharedApplication.keyWindow.rootViewController.view.frame;
    LRootViewRect := SharedApplication.keyWindow.rootViewController.view.bounds;
    LEndFrame.origin.y := LRootViewRect.size.height;
  end;

  { Start Slide down Animation }
  TUIView.OCClass.beginAnimations(nil, nil);
  try
    TUIView.OCClass.setAnimationDuration(_AnimationDuration);
    FUIContainerView.setFrame(LEndFrame);
    TUIView.OCClass.setAnimationDelegate(Self.GetObjectID);
    TUIView.OCClass.setAnimationDidStopSelector(sel_getUid('Hidden'))
  finally
    TUIView.OCClass.commitAnimations;
  end;

end;

{***********************************}
procedure TALDatePickerDialog.Hidden;
begin
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.Hidden');
  {$ENDIF}
  FUIOverlayView.removeFromSuperview;
  TThread.ForceQueue(nil,
    procedure
    begin
      Free;
    end);
end;

{*****************************************************}
procedure TALDatePickerDialog.DeviceOrientationChanged;
begin
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.DeviceOrientationChanged');
  {$ENDIF}
  FUIToolBar.sizeToFit;
  FUIDatePicker.sizeToFit;
  if FUITitle <> nil then FUITitle.sizeToFit;
  FUIContainerView.sizeToFit;
  FUIContainerView.setFrame(GetPopupFrame);
end;

{**************************************}
procedure TALDatePickerDialog.HandleTap;
begin
  {$IFDEF DEBUG}
  allog('TALDatePickerDialog.HandleTap');
  {$ENDIF}
  Cancel;
end;

{$ENDIF}
{$ENDREGION}

end.