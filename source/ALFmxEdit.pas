unit ALFmxEdit;

interface

{$IF defined(MACOS) and not defined(IOS)}
  {$DEFINE _MACOS}
{$ENDIF}

uses System.Types,
     system.Classes,
     System.UITypes,
     {$IF defined(android)}
     System.Messaging,
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNIBridge,
     Androidapi.JNI.Widget,
     Androidapi.JNI.JavaTypes,
     ALAndroidApi,
     {$ELSEIF defined(IOS)}
     System.TypInfo,
     iOSapi.Foundation,
     iOSapi.UIKit,
     Macapi.ObjectiveC,
     Macapi.ObjCRuntime,
     ALIosNativeControl,
     {$ELSE}
     FMX.Edit,
     {$ENDIF}
     FMX.types,
     Fmx.Graphics,
     Fmx.controls,
     ALFmxObjects;

{$IF defined(android)}
type

  {*********************}
  TALAndroidEdit = class;

  {****************************************************************}
  // the design of the androidText can be done in the res/styles.xml
  // please see the example of the demos\AlFmxcontrols.dproj
  //
  // know BUG:
  //   * actiomode copy/paste popup is under my EditText - http://stackoverflow.com/questions/39561133/why-the-actiomode-copy-paste-popup-is-under-my-edittext
  //   * actionmode translate-contextual-menu not work - http://stackoverflow.com/questions/39540693/how-to-enable-the-translate-contextual-menu-in-my-edittext
  //   * no actionbar on lollipop: http://stackoverflow.com/questions/39506977/is-it-possible-to-show-the-android-actionbar-in-delphi-firemonkey-app
  //                               http://stackoverflow.com/questions/39501339/how-to-replace-the-actionbar-by-a-popup-menu
  //                               http://stackoverflow.com/questions/39396662/edittext-how-to-activate-copy-paste-popup-without-any-actionbar
  //
  // I didn't find yet how to set the background color of the floating actionbar under
  // marshmallow: http://stackoverflow.com/questions/39570788/how-to-customize-the-background-of-the-floating-actionbar
  TALAndroidEdit = class(TControl)
  private

    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALTextWatcher = class(TJavaLocal, JTextWatcher)
      private
        [Weak] FEditControl: TALAndroidEdit;
      public
        constructor Create(const aEditcontrol: TALAndroidEdit);
        procedure afterTextChanged(s: JEditable); cdecl;
        procedure beforeTextChanged(s: JCharSequence; start: Integer; count: Integer; after: Integer); cdecl;
        procedure onTextChanged(s: JCharSequence; start: Integer; before: Integer; count: Integer); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALEditorActionListener = class(TJavaLocal, JTextView_OnEditorActionListener)
      private
        [Weak] FEditControl: TALAndroidEdit;
      public
        constructor Create(const aEditcontrol: TALAndroidEdit);
        function onEditorAction(v: JTextView; actionId: Integer; event: JKeyEvent): Boolean; cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALFocusChangeListener = class(TJavaLocal, JView_OnFocusChangeListener)
      private
        [Weak] FEditControl: TALAndroidEdit;
      public
        constructor Create(const aEditcontrol: TALAndroidEdit);
        procedure onFocusChange(v: JView; hasFocus: Boolean); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALSoftInputListener = class(TJavaLocal, JALSoftInputListener)
      private
        [Weak] FEditControl: TALAndroidEdit;
        procedure ObtainKeyboardRect(var aBounds: TRect);
      public
        constructor Create(const aEditcontrol: TALAndroidEdit);
        procedure onSoftInputShown; cdecl;
        procedure onSoftInputHidden; cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALKeyPreImeListener = class(TJavaLocal, JALKeyPreImeListener)
      private
        [Weak] FEditControl: TALAndroidEdit;
      public
        constructor Create(const aEditcontrol: TALAndroidEdit);
        function onKeyPreIme(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
      end;

  private
    fOnChangeTracking: TNotifyEvent;
    FScreenScale: single;
    FLayout: JALControlHostLayout;
    FTextWatcher: TALTextWatcher;
    FEditorActionListener: TALEditorActionListener;
    FSoftInputListener: TALSoftInputListener;
    FKeyPreImeListener: TALKeyPreImeListener;
    FFocusChangeListener: TALFocusChangeListener;
    fApplicationEventMessageID: integer;
    fReturnKeyType: TReturnKeyType;
    fKeyboardType: TVirtualKeyboardType;
    fPassword: boolean;
    FTextSettings: TTextSettings;
    procedure DoSetInputType(const aKeyboardType: TVirtualKeyboardType; const aPassword: Boolean);
    procedure setKeyboardType(const Value: TVirtualKeyboardType);
    function GetKeyboardType: TVirtualKeyboardType;
    procedure setPassword(const Value: Boolean);
    function GetPassword: Boolean;
    procedure setCheckSpelling(const Value: Boolean);
    function GetCheckSpelling: Boolean;
    procedure DoSetReturnKeyType(const Value: TReturnKeyType);
    procedure setReturnKeyType(const Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
    function GetTextPrompt: String;
    procedure setTextPrompt(const Value: String);
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
    function getText: String;
    procedure SetText(const Value: String);
    procedure OnFontChanged(Sender: TObject);
    procedure ApplicationEventHandler(const Sender : TObject; const M : TMessage);
  protected
    FEditText: JALEditText;
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure ParentChanged; override;
    procedure DoAbsoluteChanged; override;
    procedure Resize; override;
    procedure VisibleChanged; override;
    procedure ChangeOrder; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure realignContent; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnChangeTracking: TNotifyEvent read fOnChangeTracking write fOnChangeTracking;
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType default TReturnKeyType.Default;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType default TVirtualKeyboardType.Default;
    property Password: Boolean read GetPassword write SetPassword default False;
    property TextPrompt: String read GetTextPrompt write setTextPrompt;
    property Text: String read getText write SetText;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property CheckSpelling: Boolean read GetCheckSpelling write SetCheckSpelling default true;
  end;
{$endif}

{$IF defined(ios)}
type

  {***********************************}
  IALTextField = interface(UITextField)
    ['{EF9E2F3E-4C60-4094-97EF-F7885B88E2C8}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure ControlEventEditingChanged; cdecl;
    procedure ControlEventEditingDidEnd; cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function canPerformAction(action: SEL; withSender: Pointer): Boolean; cdecl;
  end;

  {******************************}
  TALIosTextFieldDelegate = class;
  TALIosEdit = class;

  {******************************************}
  TALIosTextField = class(TALIosNativeControl)
  private
    [Weak] FEditControl: TALIosEdit;
    FTextFieldDelegate: TALIosTextFieldDelegate;
    function GetView: UITextField;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create; overload; override;
    constructor Create(const AControl: TControl); overload; override;
    destructor Destroy; override;
    procedure ControlEventEditingChanged; cdecl;
    procedure ControlEventEditingDidEnd; cdecl;
    function canPerformAction(action: SEL; withSender: Pointer): Boolean; cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    property View: UITextField read GetView;
  end;

  {************************************************************}
  TALIosTextFieldDelegate = class(TOCLocal, UITextFieldDelegate)
  private
    [Weak] FTextField: TALIosTextField;
  public
    constructor Create(const ATextField: TALIosTextField);
    function textField(textField: UITextField; shouldChangeCharactersInRange: NSRange; replacementString: NSString): Boolean; cdecl;
    procedure textFieldDidBeginEditing(textField: UITextField); cdecl;
    procedure textFieldDidEndEditing(textField: UITextField); cdecl;
    function textFieldShouldBeginEditing(textField: UITextField): Boolean; cdecl;
    function textFieldShouldClear(textField: UITextField): Boolean; cdecl;
    function textFieldShouldEndEditing(textField: UITextField): Boolean; cdecl;
    function textFieldShouldReturn(textField: UITextField): Boolean; cdecl;
  end;

  {**************************}
  TALIosEdit = class(TControl)
  private
    fOnChangeTracking: TNotifyEvent;
    FAttributedString: NSMutableAttributedString;
    FTextSettings: TTextSettings;
    procedure setKeyboardType(const Value: TVirtualKeyboardType);
    function GetKeyboardType: TVirtualKeyboardType;
    procedure setPassword(const Value: Boolean);
    function GetPassword: Boolean;
    procedure setCheckSpelling(const Value: Boolean);
    function GetCheckSpelling: Boolean;
    procedure setReturnKeyType(const Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
    function GetTextPrompt: String;
    procedure setTextPrompt(const Value: String);
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
    function getText: String;
    procedure SetText(const Value: String);
    procedure DoFontChanged(const aText: String);
    procedure OnFontChanged(Sender: TObject);
  protected
    FTextField: TALIosTextField;
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure AncestorParentChanged; override;
    procedure ParentChanged; override;
    procedure ClipChildrenChanged; override;
    procedure DoAbsoluteChanged; override;
    procedure DoRootChanged; override;
    procedure Resize; override;
    procedure VisibleChanged; override;
    procedure ChangeOrder; override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RecalcOpacity; override;
    procedure RecalcEnabled; override;
    function PointInObjectLocal(X: Single; Y: Single): Boolean; override;
    property OnChangeTracking: TNotifyEvent read fOnChangeTracking write fOnChangeTracking;
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType default TReturnKeyType.Default;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType default TVirtualKeyboardType.Default;
    property Password: Boolean read GetPassword write SetPassword default False;
    property TextPrompt: String read GetTextPrompt write setTextPrompt;
    property Text: String read getText write SetText;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property CheckSpelling: Boolean read GetCheckSpelling write SetCheckSpelling default true;
  end;

{$endif}

type

  {***************************}
  TALEdit = class(TALRectangle)
  private
    FAutoTranslate: Boolean;
    FAutoConvertFontFamily: boolean;
    fOnChangeTracking: TNotifyEvent;
    FTextSettings: TTextSettings;
    {$IF defined(android)}
    fEditControl: TALAndroidEdit;
    function GetAndroidEditText: JALEditText;
    {$ELSEIF defined(IOS)}
    fEditControl: TALIosEdit;
    function GetIosTextField: TALIosTextField;
    {$ELSE}
    fEditControl: TEdit;
    {$ENDIF}
    function GetTextPrompt: String;
    procedure setTextPrompt(const Value: String);
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
    procedure OnFontChanged(Sender: TObject);
    function getText: String;
    procedure SetText(const Value: String);
    procedure DoChangeTracking(Sender: TObject);
    procedure SetKeyboardType(Value: TVirtualKeyboardType);
    function GetKeyboardType: TVirtualKeyboardType;
    procedure SetPassword(const Value: Boolean);
    function GetPassword: Boolean;
    procedure SetCheckSpelling(const Value: Boolean);
    function GetCheckSpelling: Boolean;
    procedure SetReturnKeyType(const Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
  protected
    function GetDefaultSize: TSizeF; override;
    procedure Loaded; override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure SetSides(const Value: TSides); override;
    function GetCanFocus: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF defined(android)}
    property AndroidEditText: JALEditText read GetAndroidEditText;
    {$ELSEIF defined(IOS)}
    property IosTextField: TALIosTextField read GetIosTextField;
    {$ENDIF}
  published
    property TabOrder;
    property TabStop;
    property Cursor default crIBeam;
    property CanFocus default True;
    //property CanParentFocus;
    property DisableFocusEffect;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType default TVirtualKeyboardType.Default;
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType default TReturnKeyType.Default;
    property Password: Boolean read GetPassword write SetPassword default False;
    //property ReadOnly;
    //property MaxLength;
    //property FilterChar;
    property Text: String read getText write SetText;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property Hint;
    property TextPrompt: String read GetTextPrompt write setTextPrompt;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate default true; // << just the TextPrompt
    property AutoConvertFontFamily: Boolean read FAutoConvertFontFamily write fAutoConvertFontFamily default true;
    property TouchTargetExpansion;
    //property Caret;
    //property KillFocusByReturn; => always true
    property CheckSpelling: Boolean read GetCheckSpelling write SetCheckSpelling default true;
    property ParentShowHint;
    property ShowHint;
    //property OnChange;
    property OnChangeTracking: TNotifyEvent read fOnChangeTracking write fOnChangeTracking;
    //property OnTyping;
    //property OnValidating;
    //property OnValidate;
    property OnKeyDown; // << not work under android - it's like this with their @{[^# virtual keyboard :(
    property OnKeyUp; // << not work under android - it's like this with their @{[^# virtual keyboard :(
    property OnCanFocus;
    property OnEnter;
    property OnExit;
  end;

procedure Register;

implementation

uses {$IF defined(android)}
     System.SysUtils,
     Androidapi.Helpers,
     Androidapi.Input,
     Androidapi.KeyCodes,
     Androidapi.JNI.App,
     FMX.Platform,
     FMX.Platform.Android,
     FMX.Helpers.Android,
     FMX.Forms,
     {$ELSEIF defined(IOS)}
     System.SysUtils,
     Macapi.CoreFoundation,
     Macapi.Helpers,
     iOSapi.CoreText,
     FMX.Helpers.iOS,
     FMX.Consts,
     {$endif}
     ALCommon,
     AlFmxCommon;

{**}
type
  TALEditTextSettings = class(TTextSettings)
  public
    constructor Create(const AOwner: TPersistent); override;
  published
    property Font;
    property FontColor;
    property HorzAlign default TTextAlign.Leading;
    property VertAlign default TTextAlign.Center;
  end;

{****************************************************************}
constructor TALEditTextSettings.Create(const AOwner: TPersistent);
begin
  inherited;
  HorzAlign := TTextAlign.Leading;
  VertAlign := TTextAlign.Center;
end;

{$IF defined(android)}

{****************************************************}
constructor TALAndroidEdit.Create(AOwner: TComponent);
var aScreenSrv: IFMXScreenService;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.Create', 'start', TalLogType.VERBOSE);
  {$ENDIF}
  inherited create(AOwner);
  //-----
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  //-----
  CanFocus := True;
  fApplicationEventMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventHandler);
  fOnChangeTracking := nil;
  FTextSettings := TALEditTextSettings.Create(Self);
  FTextSettings.OnChanged := OnFontChanged;
  //-----
  CallInUIThreadAndWaitFinishing(
    procedure
    var aLayoutParams: JViewGroup_LayoutParams;
    begin
      FLayout := TJALControlHostLayout.JavaClass.init(TAndroidHelper.Activity);
      FLayout.setfocusable(true);               // << this is important to remove the focus from the FeditText
      FLayout.setfocusableInTouchMode(true);    //    else fedittext will always receive back the focus after we remove it
      //-----
      {$IF CompilerVersion > 31}
        {$MESSAGE WARN 'Check if viewStack.java in classes.dex is still the same as the version in delphi berlin 10.1 and adjust the IFDEF'}
      {$ENDIF}
      MainActivity.getViewStack.addview(FLayout); // << this will add the view
                                                  //
                                                  //    public class ViewStack {
                                                  //      public void addView(ViewGroup view) {
                                                  //        if (view == null || this.mViews.contains(view)) {
                                                  //          ....
                                                  //        }
                                                  //        this.mWindowManager.addView(view, getLayoutParams(view));
                                                  //        this.mViews.add(view);
                                                  //      }
                                                  //    }
                                                  //
      if not FLayout.disableMoveAnimations then begin
        {$IF defined(DEBUG)}
        ALLog('JALControlHostLayout.disableMoveAnimations', 'failed', TalLogType.warn);
        {$ENDIF}
      end;
      //-----
      FEditText := TJALEditText.JavaClass.init(TAndroidHelper.Activity);
      FLayout.addview(FEditText);
      //-----
      FFocusChangeListener := TALFocusChangeListener.Create(Self);
      FEditText.setOnFocusChangeListener(FFocusChangeListener);
      //-----
      FTextWatcher := TALTextWatcher.Create(Self);
      FEditText.addTextChangedListener(FTextWatcher);
      //-----
      FEditorActionListener := TALEditorActionListener.Create(Self);
      FEditText.setOnEditorActionListener(FEditorActionListener);
      //-----
      FSoftInputListener := TALSoftInputListener.Create(Self);
      FEditText.SetSoftInputListener(FSoftInputListener);
      //-----
      FKeyPreImeListener := TALKeyPreImeListener.Create(Self);
      FEditText.SetKeyPreImeListener(FKeyPreImeListener);
      //-----
      aLayoutParams := FEditText.getLayoutParams;
      aLayoutParams.width := TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT;
      aLayoutParams.height := TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT;
      FEditText.setLayoutParams(aLayoutParams);
      //-----
      FEditText.setShowSoftInputOnFocus(false);
      fEditText.setSingleLine(True);
      DoSetReturnKeyType(tReturnKeyType.Default); // << init fReturnKeyType to tReturnKeyType.Default
      DoSetInputType(TVirtualKeyboardType.default, false {aPassword}); // << init fKeyboardType to TVirtualKeyboardType.default and fPassword to false
      //-----
      MainActivity.getViewStack.addView(nil); // << this will "disable" all the view but i m not sure is we really need to do this
                                              //    but as emb team don't write any comments it's hard to know what really mean
                                              //    NOT_FOCUSABLE_FLAGS and FOCUSABLE_FLAGS. anyway it's cost nothing to do it to be safe
                                              //
                                              //    public class ViewStack {
                                              //      public void addView(ViewGroup view) {
                                              //        if (view == null || this.mViews.contains(view)) {
                                              //          LayoutParams lp;
                                              //          Iterator i$ = this.mViews.iterator();
                                              //          while (i$.hasNext()) {
                                              //            View v = (View) i$.next();
                                              //            lp = getLayoutParams(v);
                                              //            lp.flags = NOT_FOCUSABLE_FLAGS;
                                              //            this.mWindowManager.updateViewLayout(v, lp);
                                              //          }
                                              //          if (view != null) {
                                              //            lp = getLayoutParams(view);
                                              //            lp.flags = FOCUSABLE_FLAGS;
                                              //            this.mWindowManager.updateViewLayout(view, lp);
                                              //            return;
                                              //          }
                                              //          return;
                                              //        }
                                              //        ...
                                              //      }
                                              //    }
                                              //
    end);
  //-----
  RealignContent;
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.Create', 'end', TalLogType.VERBOSE);
  {$ENDIF}
end;

{********************************}
destructor TALAndroidEdit.Destroy;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.Destroy', 'start', TalLogType.VERBOSE);
  {$ENDIF}

  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, fApplicationEventMessageID);
  TUIThreadCaller.ForceRunnablesCollection;
  TUIThreadCaller.Call<JALEditText, JALControlHostLayout>(
    procedure (aEditText: JALEditText; aControlHostLayout: JALControlHostLayout)
    begin

      aEditText.setVisibility(TJView.JavaClass.INVISIBLE);
      aEditText.setOnFocusChangeListener(nil);
      aEditText.RemoveTextChangedListener(FTextWatcher);
      aEditText.setOnEditorActionListener(nil);
      aEditText.SetSoftInputListener(nil);
      aEditText.SetKeyPreImeListener(nil);


      aControlHostLayout.removeAllViews();
      MainActivity.getViewStack.removeView(aControlHostLayout);

    end, FEditText, FLayout);

  //unfortunatly i can't make the previous instruction with wait
  //because when the app close, then the wait will never finish :(
  //but doesn't matter we don't need to free the object below, it's
  //we be done automatiquely
  //freeandNil(FFocusChangeListener);
  //freeandNil(FTextWatcher);
  //freeandNil(fEditorActionListener);
  //freeandNil(FSoftInputListener);
  //freeandNil(FKeyPreImeListener);
  //FEditText := nil;
  //FLayout := nil;

  ALFreeAndNil(FTextSettings);
  inherited;

  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.Destroy', 'end', TalLogType.VERBOSE);
  {$ENDIF}
end;

{**************************************}
procedure TALAndroidEdit.realignContent;

var aPos: TPointF;
    aBounds: TRect;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _GetRealBounds: TRect;
  var aNativeWin: JWindow;
      aContentRect: JRect;
  begin
    aNativeWin := TAndroidHelper.Activity.getWindow;
    if aNativeWin <> nil then begin
      aContentRect := TJRect.Create;
      aNativeWin.getDecorView.getDrawingRect(aContentRect);
      result := Rect(aContentRect.left, aContentRect.top, aContentRect.right, aContentRect.bottom);
    end
    else result := TRect.Empty;
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _showContent;
  begin
    TUIThreadCaller.Call<TRect>(
      procedure (R: TRect)
      begin
        MainActivity.getViewStack.setPosition(FLayout, R.Left, R.Top);
        MainActivity.getViewStack.setSize(FLayout, R.Right, R.Bottom);
      end, aBounds);
    CallInUIThread(
      procedure
      begin
        if FEditText.getVisibility <> TJView.JavaClass.VISIBLE then
          FEditText.setVisibility(TJView.JavaClass.VISIBLE);
      end);
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _HideContent;
  begin
    TUIThreadCaller.Call<TRect>(
      procedure (R: TRect)
      begin
        if FEditText.getVisibility <> TJView.JavaClass.INVISIBLE then begin
          FEditText.setVisibility(TJView.JavaClass.INVISIBLE);
          MainActivity.getViewStack.setPosition(FLayout, R.Right * 2 , R.Height * 2);
        end;
      end, _GetRealBounds);
  end;

begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.realignContent', 'realignContent', TalLogType.VERBOSE);
  {$ENDIF}
  if csdestroying in componentState then exit;
  if FLayout <> nil then begin
    if (not (csDesigning in ComponentState)) and
       (Root <> nil) and
       (Root.GetObject is TCommonCustomForm) then begin
      aPos := LocalToAbsolute(TPointF.Zero) * FScreenScale;
      aBounds := Rect(Round(aPos.X), Round(aPos.Y), Round(Width * FScreenScale), Round(Height * FScreenScale));
      if Visible and
         ParentedVisible and
        (TCommonCustomForm(Root.GetObject)).Visible and
        (TCommonCustomForm(Root.GetObject)).Active then _showContent
      else _HideContent;
    end
    else _HideContent;
  end;
end;

{**********************************************************************}
procedure TALAndroidEdit.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  RealignContent;
end;

{*****************************************}
procedure TALAndroidEdit.DoAbsoluteChanged;
begin
  inherited;
  RealignContent;
end;

{***********************************}
procedure TALAndroidEdit.ChangeOrder;
begin
  inherited;
  RealignContent;
end;

{**************************************}
procedure TALAndroidEdit.VisibleChanged;
begin
  inherited;
  RealignContent;
end;

{*************************************}
procedure TALAndroidEdit.ParentChanged;
begin
  inherited;
  RealignContent;
end;

{******************************}
procedure TALAndroidEdit.Resize;
begin
  inherited;
  RealignContent;
end;

{*******************************}
procedure TALAndroidEdit.DoEnter;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.DoEnter', 'start', TalLogType.VERBOSE);
  {$ENDIF}

  inherited;

  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      MainActivity.getViewStack.addview(FLayout); // << this will "activate" the view - important to enable the virtual keyboard
                                                  //
                                                  //    public class ViewStack {
                                                  //      public void addView(ViewGroup view) {
                                                  //        if (view == null || this.mViews.contains(view)) {
                                                  //          LayoutParams lp;
                                                  //          Iterator i$ = this.mViews.iterator();
                                                  //          while (i$.hasNext()) {
                                                  //            View v = (View) i$.next();
                                                  //            lp = getLayoutParams(v);
                                                  //            lp.flags = NOT_FOCUSABLE_FLAGS;
                                                  //            this.mWindowManager.updateViewLayout(v, lp);
                                                  //          }
                                                  //          if (view != null) {
                                                  //            lp = getLayoutParams(view);
                                                  //            lp.flags = FOCUSABLE_FLAGS;
                                                  //            this.mWindowManager.updateViewLayout(view, lp);
                                                  //            return;
                                                  //          }
                                                  //          return;
                                                  //        }
                                                  //        ...
                                                  //      }
                                                  //    }
                                                  //
    end);

  sleep(250); // << it's look like we need to wait before the view is fully activated - else the soft keyboard will not work

  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      if FEditText.requestFocus then
        FEditText.showSoftInput;
    end);

  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.DoEnter', 'end', TalLogType.VERBOSE);
  {$ENDIF}
end;

{******************************}
procedure TALAndroidEdit.DoExit;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.DoExit', 'start', TalLogType.VERBOSE);
  {$ENDIF}
  inherited;
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      FEditText.HideSoftInput;
      FEditText.clearfocus; // << this will gave the focus to the fLayout in fact and in this way remove it from the FeditText
      MainActivity.getViewStack.addView(nil); // << this will "disable" all the view - important to fully close the virtual keyboard
                                              //
                                              //    public class ViewStack {
                                              //      public void addView(ViewGroup view) {
                                              //        if (view == null || this.mViews.contains(view)) {
                                              //          LayoutParams lp;
                                              //          Iterator i$ = this.mViews.iterator();
                                              //          while (i$.hasNext()) {
                                              //            View v = (View) i$.next();
                                              //            lp = getLayoutParams(v);
                                              //            lp.flags = NOT_FOCUSABLE_FLAGS;
                                              //            this.mWindowManager.updateViewLayout(v, lp);
                                              //          }
                                              //          if (view != null) {
                                              //            lp = getLayoutParams(view);
                                              //            lp.flags = FOCUSABLE_FLAGS;
                                              //            this.mWindowManager.updateViewLayout(view, lp);
                                              //            return;
                                              //          }
                                              //          return;
                                              //        }
                                              //        ...
                                              //      }
                                              //    }
                                              //
    end);
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.DoExit', 'end', TalLogType.VERBOSE);
  {$ENDIF}
end;

{***********************************************************************************************************}
procedure TALAndroidEdit.DoSetInputType(const aKeyboardType: TVirtualKeyboardType; const aPassword: Boolean);
var aInputType: integer;
begin

  // TYPE_CLASS_DATETIME: Class for dates and times.
  // TYPE_CLASS_NUMBER: Class for numeric text.
  // TYPE_CLASS_PHONE: Class for a phone number.
  // TYPE_CLASS_TEXT: Class for normal text.
  // TYPE_DATETIME_VARIATION_DATE: Default variation of TYPE_CLASS_DATETIME: allows entering only a date.
  // TYPE_DATETIME_VARIATION_NORMAL: Default variation of TYPE_CLASS_DATETIME: allows entering both a date and time.
  // TYPE_DATETIME_VARIATION_TIME: Default variation of TYPE_CLASS_DATETIME: allows entering only a time.
  // TYPE_MASK_CLASS: Mask of bits that determine the overall class of text being given.
  // TYPE_MASK_FLAGS: Mask of bits that provide addition bit flags of options.
  // TYPE_MASK_VARIATION: Mask of bits that determine the variation of the base content class.
  // TYPE_NULL: Special content type for when no explicit type has been specified.
  // TYPE_NUMBER_FLAG_DECIMAL: Flag of TYPE_CLASS_NUMBER: the number is decimal, allowing a decimal point to provide fractional values.
  // TYPE_NUMBER_FLAG_SIGNED: Flag of TYPE_CLASS_NUMBER: the number is signed, allowing a positive or negative sign at the start.
  // TYPE_NUMBER_VARIATION_NORMAL: Default variation of TYPE_CLASS_NUMBER: plain normal numeric text.
  // TYPE_NUMBER_VARIATION_PASSWORD: Variation of TYPE_CLASS_NUMBER: entering a numeric password.
  // TYPE_TEXT_FLAG_AUTO_COMPLETE: Flag for TYPE_CLASS_TEXT: the text editor (which means the application) is performing auto-completion of the text being entered based on its own semantics, which it will present to the user as they type.
  // TYPE_TEXT_FLAG_AUTO_CORRECT: Flag for TYPE_CLASS_TEXT: the user is entering free-form text that should have auto-correction applied to it.
  // TYPE_TEXT_FLAG_CAP_CHARACTERS: Flag for TYPE_CLASS_TEXT: capitalize all characters.
  // TYPE_TEXT_FLAG_CAP_SENTENCES: Flag for TYPE_CLASS_TEXT: capitalize the first character of each sentence.
  // TYPE_TEXT_FLAG_CAP_WORDS: Flag for TYPE_CLASS_TEXT: capitalize the first character of every word.
  // TYPE_TEXT_FLAG_IME_MULTI_LINE: Flag for TYPE_CLASS_TEXT: the regular text view associated with this should not be multi-line, but when a fullscreen input method is providing text it should use multiple lines if it can.
  // TYPE_TEXT_FLAG_MULTI_LINE: Flag for TYPE_CLASS_TEXT: multiple lines of text can be entered into the field.
  // TYPE_TEXT_FLAG_NO_SUGGESTIONS: Flag for TYPE_CLASS_TEXT: the input method does not need to display any dictionary-based candidates.
  // TYPE_TEXT_VARIATION_EMAIL_ADDRESS: Variation of TYPE_CLASS_TEXT: entering an e-mail address.
  // TYPE_TEXT_VARIATION_EMAIL_SUBJECT: Variation of TYPE_CLASS_TEXT: entering the subject line of an e-mail.
  // TYPE_TEXT_VARIATION_FILTER: Variation of TYPE_CLASS_TEXT: entering text to filter contents of a list etc.
  // TYPE_TEXT_VARIATION_LONG_MESSAGE: Variation of TYPE_CLASS_TEXT: entering the content of a long, possibly formal message such as the body of an e-mail.
  // TYPE_TEXT_VARIATION_NORMAL: Default variation of TYPE_CLASS_TEXT: plain old normal text.
  // TYPE_TEXT_VARIATION_PASSWORD: Variation of TYPE_CLASS_TEXT: entering a password.
  // TYPE_TEXT_VARIATION_PERSON_NAME: Variation of TYPE_CLASS_TEXT: entering the name of a person.
  // TYPE_TEXT_VARIATION_PHONETIC: Variation of TYPE_CLASS_TEXT: entering text for phonetic pronunciation, such as a phonetic name field in contacts.
  // TYPE_TEXT_VARIATION_POSTAL_ADDRESS: Variation of TYPE_CLASS_TEXT: entering a postal mailing address.
  // TYPE_TEXT_VARIATION_SHORT_MESSAGE: Variation of TYPE_CLASS_TEXT: entering a short, possibly informal message such as an instant message or a text message.
  // TYPE_TEXT_VARIATION_URI: Variation of TYPE_CLASS_TEXT: entering a URI.
  // TYPE_TEXT_VARIATION_VISIBLE_PASSWORD: Variation of TYPE_CLASS_TEXT: entering a password, which should be visible to the user.
  // TYPE_TEXT_VARIATION_WEB_EDIT_TEXT: Variation of TYPE_CLASS_TEXT: entering text inside of a web form.
  // TYPE_TEXT_VARIATION_WEB_EMAIL_ADDRESS: Variation of TYPE_CLASS_TEXT: entering e-mail address inside of a web form.
  // TYPE_TEXT_VARIATION_WEB_PASSWORD: Variation of TYPE_CLASS_TEXT: entering password inside of a web form.

  fKeyboardType := aKeyboardType;
  fPassword := aPassword;

  case fKeyboardType of
    TVirtualKeyboardType.Alphabet:              aInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT or TJInputType.JavaClass.TYPE_TEXT_FLAG_NO_SUGGESTIONS;
    TVirtualKeyboardType.URL:                   aInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT or TJInputType.JavaClass.TYPE_TEXT_VARIATION_URI;
    TVirtualKeyboardType.NamePhonePad:          aInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT;
    TVirtualKeyboardType.EmailAddress:          aInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT or TJInputType.JavaClass.TYPE_TEXT_VARIATION_EMAIL_ADDRESS;
    TVirtualKeyboardType.NumbersAndPunctuation: aInputType := TJInputType.JavaClass.TYPE_CLASS_NUMBER or TJInputType.JavaClass.TYPE_NUMBER_FLAG_DECIMAL;
    TVirtualKeyboardType.NumberPad:             aInputType := TJInputType.JavaClass.TYPE_CLASS_NUMBER;
    TVirtualKeyboardType.PhonePad:              aInputType := TJInputType.JavaClass.TYPE_CLASS_PHONE;
    else {TVirtualKeyboardType.Default}         aInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT;
  end;

  if fPassword then begin
    case fKeyboardType of
      TVirtualKeyboardType.NumbersAndPunctuation: aInputType := aInputType or TJInputType.JavaClass.TYPE_NUMBER_VARIATION_PASSWORD;
      TVirtualKeyboardType.NumberPad:             aInputType := aInputType or TJInputType.JavaClass.TYPE_NUMBER_VARIATION_PASSWORD;
      TVirtualKeyboardType.PhonePad:;
      TVirtualKeyboardType.Alphabet:              aInputType := aInputType or TJInputType.JavaClass.TYPE_TEXT_VARIATION_PASSWORD;
      TVirtualKeyboardType.URL:                   aInputType := aInputType or TJInputType.JavaClass.TYPE_TEXT_VARIATION_PASSWORD;
      TVirtualKeyboardType.NamePhonePad:          aInputType := aInputType or TJInputType.JavaClass.TYPE_TEXT_VARIATION_PASSWORD;
      TVirtualKeyboardType.EmailAddress:          aInputType := aInputType or TJInputType.JavaClass.TYPE_TEXT_VARIATION_PASSWORD;
      else {TVirtualKeyboardType.Default}         aInputType := aInputType or TJInputType.JavaClass.TYPE_TEXT_VARIATION_PASSWORD;
    end;
  end;

  FEditText.setInputType(aInputType);

end;

{**************************************************************************}
procedure TALAndroidEdit.setKeyboardType(const Value: TVirtualKeyboardType);
begin
  if (value <> fKeyboardType) then begin
    CallInUIThreadAndWaitFinishing(
      procedure
      begin
        DoSetInputType(Value, fPassword);
      end);
  end;
end;

{************************************************************}
function TALAndroidEdit.GetKeyboardType: TVirtualKeyboardType;
begin
  result := fKeyboardType;
end;

{*********************************************************}
procedure TALAndroidEdit.setPassword(const Value: Boolean);
begin
  if (value <> fPassword) then begin
    CallInUIThreadAndWaitFinishing(
      procedure
      begin
        DoSetInputType(fKeyboardType, Value);
      end);
  end;
end;

{*******************************************}
function TALAndroidEdit.GetPassword: Boolean;
begin
  result := fPassword;
end;

{**************************************************************}
procedure TALAndroidEdit.setCheckSpelling(const Value: Boolean);
begin
  // do nothing as far as i know their is no much such option but on marshmallow it's activated by default
end;

{************************************************}
function TALAndroidEdit.GetCheckSpelling: Boolean;
begin
  result := false;
end;

{***********************************************************************}
procedure TALAndroidEdit.DoSetReturnKeyType(const Value: TReturnKeyType);
var aimeOptions: integer;
begin
  fReturnKeyType := Value;
  case fReturnKeyType of
    TReturnKeyType.Done:          aimeOptions := TJEditorInfo.JavaClass.IME_ACTION_DONE;
    TReturnKeyType.Go:            aimeOptions := TJEditorInfo.JavaClass.IME_ACTION_GO;
    TReturnKeyType.Next:          aimeOptions := TJEditorInfo.JavaClass.IME_ACTION_NEXT;
    TReturnKeyType.Search:        aimeOptions := TJEditorInfo.JavaClass.IME_ACTION_SEARCH;
    TReturnKeyType.Send:          aimeOptions := TJEditorInfo.JavaClass.IME_ACTION_SEND;
    else {TReturnKeyType.Default} aimeOptions := TJEditorInfo.JavaClass.IME_ACTION_NONE;
  end;
  FEditText.setImeOptions(aimeOptions);
end;

{*********************************************************************}
procedure TALAndroidEdit.setReturnKeyType(const Value: TReturnKeyType);
begin
  if (value <> fReturnKeyType) then begin
    CallInUIThreadAndWaitFinishing(
      procedure
      begin
        DoSetReturnKeyType(Value);
      end);
  end;
end;

{*******************************************************}
function TALAndroidEdit.GetReturnKeyType: TReturnKeyType;
begin
  result := fReturnKeyType;
end;

{********************************************}
function TALAndroidEdit.GetTextPrompt: String;
var aHint: JCharSequence;
begin
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      aHint := FEditText.getHint;
    end);
  result := JCharSequenceToStr(aHint);
end;

{**********************************************************}
procedure TALAndroidEdit.setTextPrompt(const Value: String);
begin
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      FEditText.setHint(StrToJCharSequence(Value));
    end);
end;

{**************************************}
function TALAndroidEdit.getText: String;
var aText: JCharSequence;
begin
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      aText := FEditText.gettext;
    end);
  result := JCharSequenceToStr(aText);
end;

{****************************************************}
procedure TALAndroidEdit.SetText(const Value: String);
begin
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      FEditText.setText(StrToJCharSequence(Value), TJTextView_BufferType.javaClass.EDITABLE);
    end);
end;

{******************************************************}
procedure TALAndroidEdit.OnFontChanged(Sender: TObject);
var aTypeface: JTypeface;
    aStyle: integer;
    aGravity: integer;
begin
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      //-----
      feditText.setTextColor(ftextsettings.fontcolor); // << Sets the text color for all the states (normal, selected, focused) to be this color.
      feditText.setTextSize(ftextsettings.font.size); // << Set the default text size to the given value, interpreted as "scaled pixel" units.
                                                      //    This size is adjusted based on the current density and user font size preference.
      //-----
      if (TFontStyle.fsBold in ftextsettings.font.style) and
         (TFontStyle.fsItalic in ftextsettings.font.style) then aStyle := TJTypeface.JavaClass.BOLD_ITALIC
      else if (TFontStyle.fsBold in ftextsettings.font.style) then aStyle := TJTypeface.JavaClass.BOLD
      else if (TFontStyle.fsItalic in ftextsettings.font.style) then aStyle := TJTypeface.JavaClass.ITALIC
      else aStyle := TJTypeface.JavaClass.NORMAL;
      aTypeface := TJTypeface.JavaClass.create(StringToJString(ftextsettings.font.Family), aStyle);
      feditText.setTypeface(aTypeface); // << Sets the typeface and style in which the text should be displayed. Note that not all
                                        //    Typeface families actually have bold and italic variants, so you may need to use setTypeface(Typeface, int)
                                        //     to get the appearance that you actually want.
      aTypeface := nil;
      //-----
      //top	              0x30	     	Push object to the top of its container, not changing its size.
      //bottom	          0x50	     	Push object to the bottom of its container, not changing its size.
      //left	            0x03	     	Push object to the left of its container, not changing its size.
      //right            	0x05      	Push object to the right of its container, not changing its size.
      //center_vertical	  0x10      	Place object in the vertical center of its container, not changing its size.
      //fill_vertical	    0x70      	Grow the vertical size of the object if needed so it completely fills its container.
      //center_horizontal	0x01	     	Place object in the horizontal center of its container, not changing its size.
      //fill_horizontal	  0x07      	Grow the horizontal size of the object if needed so it completely fills its container.
      //center	          0x11	     	Place the object in the center of its container in both the vertical and horizontal axis, not changing its size.
      //fill	            0x77	     	Grow the horizontal and vertical size of the object if needed so it completely fills its container.
      //clip_vertical	    0x80	     	Additional option that can be set to have the top and/or bottom edges of the child clipped to its container's bounds. The clip will be based on the vertical gravity: a top gravity will clip the bottom edge, a bottom gravity will clip the top edge, and neither will clip both edges.
      //clip_horizontal	  0x08       	Additional option that can be set to have the left and/or right edges of the child clipped to its container's bounds. The clip will be based on the horizontal gravity: a left gravity will clip the right edge, a right gravity will clip the left edge, and neither will clip both edges.
      //start	            0x00800003	Push object to the beginning of its container, not changing its size.
      //end	              0x00800005	Push object to the end of its container, not changing its size.
      case ftextsettings.HorzAlign of
        TTextAlign.Center: aGravity := $01; // center_horizontal 0x01
        TTextAlign.Leading: aGravity := $03; // left 0x03
        TTextAlign.Trailing: aGravity := $05; // right 0x05
      end;
      case ftextsettings.VertAlign of
        TTextAlign.Center: aGravity := aGravity or $10; // center_vertical 0x10
        TTextAlign.Leading: aGravity := aGravity or $30; // top 0x30
        TTextAlign.Trailing: aGravity := aGravity or $50; // bottom 0x50
      end;
      feditText.setgravity(aGravity);
    end);
end;

{*****************************************************}
function TALAndroidEdit.GetTextSettings: TTextSettings;
begin
  Result := FTextSettings;
end;

{*******************************************************************}
procedure TALAndroidEdit.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{*****************************************************************************************}
procedure TALAndroidEdit.ApplicationEventHandler(const Sender: TObject; const M: TMessage);
begin
  {$IF defined(DEBUG)}
  if isfocused and
     (M is TApplicationEventMessage) then begin
     case (M as TApplicationEventMessage).Value.Event of
       TApplicationEvent.FinishedLaunching: ALLog('TALAndroidEdit.ApplicationEventHandler', 'Event: TApplicationEvent.FinishedLaunching', TalLogType.VERBOSE);
       TApplicationEvent.BecameActive: ALLog('TALAndroidEdit.ApplicationEventHandler', 'Event: TApplicationEvent.BecameActive', TalLogType.VERBOSE);
       TApplicationEvent.WillBecomeInactive: ALLog('TALAndroidEdit.ApplicationEventHandler', 'Event: TApplicationEvent.WillBecomeInactive', TalLogType.VERBOSE);
       TApplicationEvent.EnteredBackground: ALLog('TALAndroidEdit.ApplicationEventHandler', 'Event: TApplicationEvent.EnteredBackground', TalLogType.VERBOSE);
       TApplicationEvent.WillBecomeForeground: ALLog('TALAndroidEdit.ApplicationEventHandler', 'Event: TApplicationEvent.WillBecomeForeground', TalLogType.VERBOSE);
       TApplicationEvent.WillTerminate: ALLog('TALAndroidEdit.ApplicationEventHandler', 'Event: TApplicationEvent.WillTerminate', TalLogType.VERBOSE);
       TApplicationEvent.LowMemory: ALLog('TALAndroidEdit.ApplicationEventHandler', 'Event: TApplicationEvent.LowMemory', TalLogType.VERBOSE);
       TApplicationEvent.TimeChange: ALLog('TALAndroidEdit.ApplicationEventHandler', 'Event: TApplicationEvent.TimeChange', TalLogType.VERBOSE);
       TApplicationEvent.OpenURL: ALLog('TALAndroidEdit.ApplicationEventHandler', 'Event: TApplicationEvent.OpenURL', TalLogType.VERBOSE);
     end;
   end;
  {$ENDIF}
  //problem is that as we play with view, the WillBecomeInactive - BecameActive will be call everytime we toggle the
  //view under the MainActivity.getViewStack. so we can't use these event to know that the application resume from
  //background. most easy is to close the virtual keyboard when the application entere in background (EnteredBackground
  //event is call ONLY when application entered in the background so everything is fine
  if isfocused and
     (M is TApplicationEventMessage) and
     ((M as TApplicationEventMessage).Value.Event = TApplicationEvent.EnteredBackground) then resetfocus;
end;

{*******************************************************************************************}
constructor TALAndroidEdit.TALFocusChangeListener.Create(const aEditcontrol: TALAndroidEdit);
begin
  inherited Create;
  FEditcontrol := aEditcontrol;
end;

{*****************************************************************************************}
procedure TALAndroidEdit.TALFocusChangeListener.onFocusChange(v: JView; hasFocus: Boolean);
begin
  {$IF defined(DEBUG)}
  if hasFocus then ALLog('TALAndroidEdit.onFocusChange', 'hasFocus: true', TalLogType.VERBOSE)
  else ALLog('TALAndroidEdit.onFocusChange', 'hasFocus: false', TalLogType.VERBOSE);
  {$ENDIF}
  if hasFocus then begin
    TThread.Queue(nil,
      procedure
      begin
        if not FEditcontrol.isfocused then
          FEditcontrol.SetFocus;
      end);
  end
  else begin
    TThread.Queue(nil,
      procedure
      begin
        if FEditcontrol.isfocused then
          FEditcontrol.resetfocus;
      end);
  end
end;

{*****************************************************************************************}
constructor TALAndroidEdit.TALSoftInputListener.Create(const aEditcontrol: TALAndroidEdit);
begin
  inherited Create;
  FEditControl := aEditcontrol;
end;

{*************************************************************}
procedure TALAndroidEdit.TALSoftInputListener.onSoftInputShown;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.onSoftInputShown', 'Shown', TalLogType.VERBOSE);
  {$ENDIF}
  TThread.Queue(nil,
    procedure
    var aBounds: TRect;
    begin
      ObtainKeyboardRect(aBounds);
      ALLog('TALAndroidEdit.onSoftInputShown', 'Keyboard Rect bounds:('+inttostr(aBounds.Left)+','+inttostr(aBounds.top)+','+inttostr(aBounds.right)+','+inttostr(aBounds.bottom)+') - bounds-height:'+inttostr(aBounds.height)+' - bounds-width:'+inttostr(aBounds.width), TalLogType.VERBOSE);
      TMessageManager.DefaultManager.SendMessage(Self, TVKStateChangeMessage.Create(true, aBounds), True);
    end);
end;

{**************************************************************}
procedure TALAndroidEdit.TALSoftInputListener.onSoftInputHidden;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.onSoftInputHidden', 'Hidden', TalLogType.VERBOSE);
  {$ENDIF}
  TThread.Queue(nil,
    procedure
    begin
      TMessageManager.DefaultManager.SendMessage(Self, TVKStateChangeMessage.Create(false, Trect.create(0,0,0,0)), True);
    end);
end;

{***********************************************************************************}
procedure TALAndroidEdit.TALSoftInputListener.ObtainKeyboardRect(var aBounds: TRect);
var aContentRect, aTotalRect: JRect;
begin
  aContentRect := TJRect.Create;
  aTotalRect := TJRect.Create;
  MainActivity.getWindow.getDecorView.getWindowVisibleDisplayFrame(aContentRect);
  MainActivity.getWindow.getDecorView.getDrawingRect(aTotalRect);
  aBounds := TRectF.Create(ConvertPixelToPoint(TPointF.Create(aTotalRect.left, aContentRect.bottom)), // topleft
                           ConvertPixelToPoint(TPointF.Create(aTotalRect.right, aTotalRect.bottom))).Truncate;  //bottomRight
end;

{*****************************************************************************************}
constructor TALAndroidEdit.TALKeyPreImeListener.Create(const aEditcontrol: TALAndroidEdit);
begin
  inherited Create;
  FEditControl := aEditcontrol;
end;

{****************************************************************************************************}
function TALAndroidEdit.TALKeyPreImeListener.onKeyPreIme(keyCode: Integer; event: JKeyEvent): Boolean;
begin
  {$IF defined(DEBUG)}
  if event <> nil then ALLog('TALAndroidEdit.onKeyPreIme', 'keyCode: ' + inttostr(keyCode) + ' - event: ' + JstringToString(event.toString), TalLogType.VERBOSE)
  else ALLog('TALAndroidEdit.onKeyPreIme', 'keyCode: ' + inttostr(keyCode), TalLogType.VERBOSE);
  {$ENDIF}
  if ((event = nil) or (event.getAction = AKEY_EVENT_ACTION_DOWN)) and
     (keyCode = AKEYCODE_BACK) then begin

    result := true;

    TThread.Queue(nil,
     procedure
     begin
       sleep(250); // << the problem is we leave the finger on the back button, then the keyevent is send again and again
                   //    so leave 250 ms to remove the finger
       FEditcontrol.resetfocus;
     end);

  end
  else result := false;
end;

{***********************************************************************************}
constructor TALAndroidEdit.TALTextWatcher.Create(const aEditcontrol: TALAndroidEdit);
begin
  inherited Create;
  FEditControl := aEditcontrol;
end;

{*********************************************************************}
procedure TALAndroidEdit.TALTextWatcher.afterTextChanged(s: JEditable);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.afterTextChanged', '(Text changed)', TalLogType.VERBOSE);
  {$ENDIF}
  TThread.Queue(nil,
    procedure
    begin
      if assigned(fEditControl.fOnChangeTracking) then
        fEditControl.fOnChangeTracking(fEditControl);
    end);
end;

{**************************************************************************************************************************}
procedure TALAndroidEdit.TALTextWatcher.beforeTextChanged(s: JCharSequence; start: Integer; count: Integer; after: Integer);
begin
//nothing to do
end;

{***********************************************************************************************************************}
procedure TALAndroidEdit.TALTextWatcher.onTextChanged(s: JCharSequence; start: Integer; before: Integer; count: Integer);
begin
//nothing to do
end;

{********************************************************************************************}
constructor TALAndroidEdit.TALEditorActionListener.Create(const aEditcontrol: TALAndroidEdit);
begin
  inherited Create;
  FEditControl := aEditcontrol;
end;

{*************************************************************************************************************************}
function TALAndroidEdit.TALEditorActionListener.onEditorAction(v: JTextView; actionId: Integer; event: JKeyEvent): Boolean;
begin
  {$IF defined(DEBUG)}
   if event <> nil then ALLog('TALAndroidEdit.onEditorAction', 'actionId: ' + inttostr(actionId) + ' - event: ' + JstringToString(event.toString), TalLogType.VERBOSE)
   else ALLog('TALAndroidEdit.onEditorAction', 'actionId: ' + inttostr(actionId), TalLogType.VERBOSE);
  {$ENDIF}
  //IME_ACTION_DONE: the action key performs a "done" operation, typically meaning there is nothing more to input and the IME will be closed.
  //IME_ACTION_GO: the action key performs a "go" operation to take the user to the target of the text they typed. Typically used, for example, when entering a URL.
  //IME_ACTION_NEXT: the action key performs a "next" operation, taking the user to the next field that will accept text.
  //IME_ACTION_NONE: there is no available action.
  //IME_ACTION_PREVIOUS: like IME_ACTION_NEXT, but for moving to the previous field. This will normally not be used to specify an action (since it precludes IME_ACTION_NEXT), but can be returned to the app if it sets IME_FLAG_NAVIGATE_PREVIOUS.
  //IME_ACTION_SEARCH: the action key performs a "search" operation, taking the user to the results of searching for the text they have typed (in whatever context is appropriate).
  //IME_ACTION_SEND: the action key performs a "send" operation, delivering the text to its target. This is typically used when composing a message in IM or SMS where sending is immediate.
  //IME_ACTION_UNSPECIFIED: no specific action has been associated with this editor, let the editor come up with its own if it can.
  if (actionId = TJEditorInfo.javaClass.IME_ACTION_UNSPECIFIED) or // IME_ACTION_UNSPECIFIED = the return key
     (actionId = TJEditorInfo.javaClass.IME_ACTION_DONE) or
     (actionId = TJEditorInfo.javaClass.IME_ACTION_GO) or
     (actionId = TJEditorInfo.javaClass.IME_ACTION_NEXT) or
     (actionId = TJEditorInfo.javaClass.IME_ACTION_SEARCH) or
     (actionId = TJEditorInfo.javaClass.IME_ACTION_SEND) then begin

    result := true;

    TThread.Queue(nil,
     procedure
     begin
       FEditcontrol.resetfocus;
     end);

  end
  else result := false;
end;

{$endif}

{$IF defined(ios)}

{*********************************}
constructor TALIosTextField.Create;
begin
  inherited;
  FTextFieldDelegate := TALIosTextFieldDelegate.Create(Self);
  View.setExclusiveTouch(True);
  View.setBorderStyle(UITextBorderStyleNone);
  View.setDelegate((FTextFieldDelegate as ILocalObject).GetObjectID);
  RegisterNativeEventHandler('ControlEventEditingChanged', UIControlEventEditingChanged);
  RegisterNativeEventHandler('ControlEventEditingDidEnd', UIControlEventEditingDidEnd);
end;

{***********************************************************}
constructor TALIosTextField.Create(const AControl: TControl);
begin
  fEditControl := TalIosEdit(AControl);
  inherited;
end;

{*********************************}
destructor TALIosTextField.Destroy;
begin
  UnRegisterNativeEventHandler('ControlEventEditingChanged', UIControlEventEditingChanged);
  UnRegisterNativeEventHandler('ControlEventEditingDidEnd', UIControlEventEditingDidEnd);
  View.setDelegate(nil);
  ALFreeAndNil(FTextFieldDelegate);
  inherited;
end;

{********************************************************}
function TALIosTextField.canBecomeFirstResponder: Boolean;
begin
  Result := Control.CanFocus and Control.HitTest;
end;

{***********************************************************************************}
function TALIosTextField.canPerformAction(action: SEL; withSender: Pointer): Boolean;
begin
  Result := UIView(Super).canPerformAction(action, withSender);
end;

{***************************************************}
procedure TALIosTextField.ControlEventEditingChanged;
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosTextField.ControlEventEditingChanged', '', TalLogType.VERBOSE);
  {$ENDIF}
  if assigned(fEditControl.fOnChangeTracking) then
    fEditControl.fOnChangeTracking(fEditControl);
end;

{**************************************************}
procedure TALIosTextField.ControlEventEditingDidEnd;
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosTextField.ControlEventEditingDidEnd', '', TalLogType.VERBOSE);
  {$ENDIF}
  if assigned(fEditControl.fOnChangeTracking) then
    fEditControl.fOnChangeTracking(fEditControl);  // << when we change the word via the sugestion (clicking on the selection) then ControlEventEditingChanged is not fired
                                                   //    imediatly, only on the next key press ... but if we don't press any key but instead close
                                                   //    the keyboad then the event will be never fired ! so we catch here this case
end;

{*****************************************************}
function TALIosTextField.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALTextField);
end;

{********************************************}
function TALIosTextField.GetView: UITextField;
begin
  Result := inherited GetView<UITextField>;
end;

{****************************************************************************}
constructor TALIosTextFieldDelegate.Create(const ATextField: TALIosTextField);
begin
  inherited Create;
  FTextField := ATextField;
  if FTextField = nil then
    raise EArgumentNilException.Create(Format(SWrongParameter, ['ATextField']));
end;

{***********************************************************************************************************************************************}
function TALIosTextFieldDelegate.textField(textField: UITextField; shouldChangeCharactersInRange: NSRange; replacementString: NSString): Boolean;
begin
  Result := True;
end;

{*********************************************************************************}
procedure TALIosTextFieldDelegate.textFieldDidBeginEditing(textField: UITextField);
begin
  if not FTextField.Control.IsFocused then
    FTextField.Control.SetFocus;
end;

{*******************************************************************************}
procedure TALIosTextFieldDelegate.textFieldDidEndEditing(textField: UITextField);
begin
end;

{********************************************************************************************}
function TALIosTextFieldDelegate.textFieldShouldBeginEditing(textField: UITextField): Boolean;
begin
  Result := True;
end;

{*************************************************************************************}
function TALIosTextFieldDelegate.textFieldShouldClear(textField: UITextField): Boolean;
begin
  Result := true;
end;

{******************************************************************************************}
function TALIosTextFieldDelegate.textFieldShouldEndEditing(textField: UITextField): Boolean;
begin
  Result := True;
end;

{**************************************************************************************}
function TALIosTextFieldDelegate.textFieldShouldReturn(textField: UITextField): Boolean;
begin
  FTextField.ControlEventEditingDidEnd;
  FTextField.ResetFocus;
  Result := true;
end;

{************************************************}
constructor TalIosEdit.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  CanFocus := True;
  fOnChangeTracking := nil;
  FTextSettings := TALEditTextSettings.Create(Self);
  FTextSettings.OnChanged := OnFontChanged;
  FAttributedString := nil;
  FTextField := TalIosTextField.create(self);
  SetReturnKeyType(tReturnKeyType.Default);
  SetKeyboardType(TVirtualKeyboardType.default);
  SetPassword(false);
  SetCheckSpelling(True);
end;

{****************************}
destructor TalIosEdit.Destroy;
begin
  ALfreeandNil(FTextField);
  if FAttributedString <> nil then FAttributedString.release;
  ALFreeAndNil(FTextSettings);
  inherited Destroy;
end;

{**********************************************************************}
procedure TalIosEdit.SetKeyboardType(const Value: TVirtualKeyboardType);
var aUIKeyboardType: UIKeyboardType;
begin
  case Value of
    TVirtualKeyboardType.NumbersAndPunctuation: aUIKeyboardType := UIKeyboardTypeNumbersAndPunctuation;
    TVirtualKeyboardType.NumberPad:             aUIKeyboardType := UIKeyboardTypeNumberPad;
    TVirtualKeyboardType.PhonePad:              aUIKeyboardType := UIKeyboardTypePhonePad;
    TVirtualKeyboardType.Alphabet:              aUIKeyboardType := UIKeyboardTypeDefault;
    TVirtualKeyboardType.URL:                   aUIKeyboardType := UIKeyboardTypeURL;
    TVirtualKeyboardType.NamePhonePad:          aUIKeyboardType := UIKeyboardTypeNamePhonePad;
    TVirtualKeyboardType.EmailAddress:          aUIKeyboardType := UIKeyboardTypeEmailAddress;
    else {TVirtualKeyboardType.Default}         aUIKeyboardType := UIKeyboardTypeDefault;
  end;
  FTextField.View.setKeyboardType(aUIKeyboardType);
end;

{********************************************************}
function TalIosEdit.GetKeyboardType: TVirtualKeyboardType;
var aUIKeyboardType: UIKeyboardType;
begin
  aUIKeyboardType := FTextField.View.KeyboardType;
  case aUIKeyboardType of
    UIKeyboardTypeNumbersAndPunctuation: result := TVirtualKeyboardType.NumbersAndPunctuation;
    UIKeyboardTypeNumberPad:             result := TVirtualKeyboardType.NumberPad;
    UIKeyboardTypePhonePad:              result := TVirtualKeyboardType.PhonePad;
    UIKeyboardTypeURL:                   result := TVirtualKeyboardType.URL;
    UIKeyboardTypeNamePhonePad:          result := TVirtualKeyboardType.NamePhonePad;
    UIKeyboardTypeEmailAddress:          result := TVirtualKeyboardType.EmailAddress;
    else                                 result := TVirtualKeyboardType.Default;
  end;
end;

{*****************************************************}
procedure TalIosEdit.SetPassword(const Value: Boolean);
begin
  FTextField.View.setSecureTextEntry(Value);
end;

{***************************************}
function TalIosEdit.GetPassword: Boolean;
begin
  result := FTextField.View.isSecureTextEntry;
end;

{**********************************************************}
procedure TalIosEdit.SetCheckSpelling(const Value: Boolean);
begin
  if Value then FTextField.View.setSpellCheckingType(UITextSpellCheckingTypeYes)
  else FTextField.View.setSpellCheckingType(UITextSpellCheckingTypeNo)
end;

{********************************************}
function TalIosEdit.GetCheckSpelling: Boolean;
begin
  result := FTextField.View.SpellCheckingType = UITextSpellCheckingTypeYes;
end;

{*****************************************************************}
procedure TalIosEdit.setReturnKeyType(const Value: TReturnKeyType);
var aUIReturnKeyType: UIReturnKeyType;
begin
  case Value of
    TReturnKeyType.Done:           aUIReturnKeyType := UIReturnKeyDone;
    TReturnKeyType.Go:             aUIReturnKeyType := UIReturnKeyGo;
    TReturnKeyType.Next:           aUIReturnKeyType := UIReturnKeyNext;
    TReturnKeyType.Search:         aUIReturnKeyType := UIReturnKeySearch;
    TReturnKeyType.Send:           aUIReturnKeyType := UIReturnKeySend;
    else {TReturnKeyType.Default}  aUIReturnKeyType := UIReturnKeyDefault;
  end;
  FTextField.View.setReturnKeyType(aUIReturnKeyType);
end;

{***************************************************}
function TalIosEdit.GetReturnKeyType: TReturnKeyType;
var aUIReturnKeyType: UIReturnKeyType;
begin
  aUIReturnKeyType := FTextField.View.ReturnKeyType;
  case aUIReturnKeyType of
    UIReturnKeyDone:    result := TReturnKeyType.Done;
    UIReturnKeyGo:      result := TReturnKeyType.Go;
    UIReturnKeyNext:    result := TReturnKeyType.Next;
    UIReturnKeySearch:  result := TReturnKeyType.Search;
    UIReturnKeySend:    result := TReturnKeyType.Send;
    else                result := TReturnKeyType.Default;
  end;
end;

{****************************************}
function TalIosEdit.GetTextPrompt: String;
begin
  Result := NSStrToStr(FTextField.View.placeholder);
end;

{******************************************************}
procedure TalIosEdit.setTextPrompt(const Value: String);
begin
  FTextField.View.setPlaceholder(StrToNSStr(Value));
end;

{**********************************}
function TalIosEdit.getText: String;
begin
  result := NSStrToStr(TNSString.Wrap(FTextField.View.text));
end;

{************************************************}
procedure TalIosEdit.SetText(const Value: String);
begin
  if (FAttributedString = nil) or (FAttributedString.length = 0) then DoFontChanged(Value)
  else begin
    FAttributedString.replaceCharactersInRange(NSMakeRange(0, FAttributedString.length), StrToNSStr(Value));
    FTextField.View.setAttributedText(FAttributedString);
  end;
end;

{******************************************************}
procedure TalIosEdit.DoFontChanged(const aText: String);
var aTextRange: NSRange;
    aFontRef: CTFontRef;
    aUnderline: CFNumberRef;
    aValue: Cardinal;
begin

  if FAttributedString <> nil then begin
    FAttributedString.release;
    FAttributedString := nil;
  end;
  FAttributedString := TNSMutableAttributedString.Alloc;
  FAttributedString := TNSMutableAttributedString.Wrap(FAttributedString.initWithString(StrToNSStr(aText)));

  FAttributedString.beginEditing;
  try
    aTextRange := NSMakeRange(0, aText.Length);

    //Font
    aFontRef := ALGetCTFontRef(fTextSettings.Font.Family, fTextSettings.Font.Size, fTextSettings.Font.Style);
    if aFontRef <> nil then
      try
        FAttributedString.addAttribute(TNSString.Wrap(kCTFontAttributeName), aFontRef, aTextRange);
      finally
        CFRelease(aFontRef);
      end;

    //Font style
    if TFontStyle.fsUnderline in fTextSettings.Font.Style then begin
      aValue := kCTUnderlineStyleSingle;
      aUnderline := CFNumberCreate(nil, kCFNumberSInt32Type, @aValue);
      try
        FAttributedString.addAttribute(TNSString.Wrap(kCTUnderlineStyleAttributeName), aUnderline, aTextRange);
      finally
        CFRelease(aUnderline);
      end;
    end;

  finally
    FAttributedString.endEditing;
  end;

  FTextField.View.setAttributedText(FAttributedString);
  FTextField.View.setTextAlignment(TextAlignToUITextAlignment(fTextSettings.HorzAlign));
  FTextField.View.setTextColor(AlphaColorToUIColor(fTextSettings.FontColor));
end;

{**************************************************}
procedure TalIosEdit.OnFontChanged(Sender: TObject);
begin
  DoFontChanged(text);
end;

{*************************************************}
function TalIosEdit.GetTextSettings: TTextSettings;
begin
  Result := FTextSettings;
end;

{***************************************************************}
procedure TalIosEdit.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{*********************************}
procedure TalIosEdit.DoRootChanged;
begin
  inherited;
  FTextField.RootChanged(Root);
end;

{**************************}
procedure TalIosEdit.Resize;
begin
  inherited;
  FTextField.size := Size.size;
end;

{***************************************}
procedure TalIosEdit.ClipChildrenChanged;
begin
  inherited;
  FTextField.SetClipChildren(ClipChildren);
end;

{*************************************}
procedure TalIosEdit.DoAbsoluteChanged;
begin
  inherited;
  if not (csLoading in ComponentState) then
    FTextField.UpdateFrame;
end;

{**********************************}
procedure TalIosEdit.VisibleChanged;
begin
  inherited;
  FTextField.SetVisible(Visible);
end;

{*******************************}
procedure TalIosEdit.ChangeOrder;
begin
  inherited;
  FTextField.ChangeOrder;
end;

{*********************************}
procedure TalIosEdit.RecalcOpacity;
begin
  inherited;
  FTextField.setAlpha(AbsoluteOpacity);
end;

{*********************************}
procedure TalIosEdit.RecalcEnabled;
begin
  inherited;
  FTextField.SetAbsoluteEnabled(AbsoluteEnabled);
end;

{******************************************************************}
procedure TalIosEdit.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  if FTextField <> nil then FTextField.AncestorVisibleChanged;  // << this proc is called during the ondestroy also when FTextField is already destroyed
end;

{*****************************************}
procedure TalIosEdit.AncestorParentChanged;
begin
  inherited;
  if FTextField <> nil then FTextField.RefreshNativeParent;  // << this proc is called during the ondestroy also when FTextField is already destroyed
end;

{*********************************}
procedure TalIosEdit.ParentChanged;
begin
  inherited;
  if FTextField <> nil then FTextField.RefreshNativeParent; // << this proc is called during the ondestroy also when FTextField is already destroyed
end;

{********************************************************************}
function TalIosEdit.PointInObjectLocal(X: Single; Y: Single): Boolean;
begin
  result := FTextField.PointInObjectLocal(X, Y);
end;

{***************************}
procedure TalIosEdit.DoEnter;
begin
  {$IF defined(DEBUG)}
  ALLog('TalIosEdit.DoEnter', '', TalLogType.VERBOSE);
  {$ENDIF}
  inherited DoEnter;
  FTextField.SetFocus;
end;

{**************************}
procedure TalIosEdit.DoExit;
begin
  {$IF defined(DEBUG)}
  ALLog('TalIosEdit.DoExit', '', TalLogType.VERBOSE);
  {$ENDIF}
  inherited DoExit;
  FTextField.ResetFocus;
end;

{$endif}

{*********************************************}
constructor TALEdit.Create(AOwner: TComponent);
begin
  inherited;
  FAutoTranslate := true;
  FAutoConvertFontFamily := True;
  fOnChangeTracking := nil;
  Cursor := crIBeam;
  CanFocus := True;
  CanParentFocus := False; // else you must rewrite the GetCanFocus
  {$IF defined(android)}
  fEditControl := TALAndroidEdit.Create(self);
  fEditControl.Parent := self;
  FeditControl.Stored := False;
  FeditControl.SetSubComponent(True);
  FeditControl.Locked := True;
  {$ELSEIF defined(ios)}
  fEditControl := TALIosEdit.Create(self);
  fEditControl.Parent := self;
  FeditControl.Stored := False;
  FeditControl.SetSubComponent(True);
  FeditControl.Locked := True;
  {$ELSE}
  fEditControl := TEdit.Create(self);
  fEditControl.Parent := self;
  FeditControl.Stored := False;
  FeditControl.SetSubComponent(True);
  FeditControl.Locked := True;
  fEditControl.ControlType := TcontrolType.Styled; // << on windows platform is not good as Styled
  FeditControl.StyleLookup := 'transparentedit';
  FeditControl.StyledSettings := []; // Family, Size, Style, FontColor, Other
  fEditControl.KillFocusByReturn := True;
  {$ENDIF}
  FTextSettings := TALEditTextSettings.Create(Self);
  FTextSettings.OnChanged := OnFontChanged;
  fEditControl.Align := TAlignLayout.Client;
  FeditControl.OnChangeTracking := DoChangeTracking;
  fEditControl.Password := false; // noops operation
  fEditControl.ReturnKeyType := tReturnKeyType.Default;  // noops operation
  fEditControl.KeyboardType := TVirtualKeyboardType.Default; // noops operation
  fEditControl.CheckSpelling := True;
  //-----
  fill.DefaultColor := $ffffffff;
  fill.Color := $ffffffff;
  stroke.DefaultKind := TBrushKind.none;
  stroke.kind := TBrushKind.none;
end;

{*************************}
destructor TALEdit.Destroy;
begin
  ALFreeAndNil(FTextSettings);
  ALFreeAndNil(fEditControl, false{adelayed}, false{aRefCountWarn}); // << will call disposeOF under ARC so it's ok
  inherited;
end;

{***********************}
procedure TALEdit.Loaded;
begin
  if (AutoConvertFontFamily) and
     (TextSettings.Font.Family <> '') and
     (not (csDesigning in ComponentState)) then
      TextSettings.Font.Family := ALConvertFontFamily(TextSettings.Font.Family, TextSettings.Font.Style);
  //-----
  inherited;
  //-----
  if (AutoTranslate) and
     (TextPrompt <> '') and
     (not (csDesigning in ComponentState)) then
      TextPrompt := ALTranslate(TextPrompt);
  //-----
  OnFontChanged(nil);
end;

{**************************************}
function TALEdit.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(100, 22);
end;

{********************}
{$IF defined(android)}
function TALEdit.GetAndroidEditText: JALEditText;
begin
  result := fEditControl.FEditText;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALEdit.GetIosTextField: TALIosTextField;
begin
  result := fEditControl.FTextField;
end;
{$ENDIF}

{**********************************************}
function TALEdit.GetTextSettings: TTextSettings;
begin
  Result := FTextSettings;
end;

{************************************************************}
procedure TALEdit.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{***********************************************}
procedure TALEdit.OnFontChanged(Sender: TObject);
begin
  if csLoading in componentState then exit;
  FEditControl.TextSettings.Assign(ftextsettings);
end;

{*********************************************}
procedure TALEdit.SetText(const Value: String);
begin
  FeditControl.Text := Value;
end;

{*******************************}
function TALEdit.getText: String;
begin
  result := FeditControl.Text;
end;

{*************************************}
function TALEdit.GetTextPrompt: String;
begin
  result := FeditControl.TextPrompt;
end;

{***************************************************}
procedure TALEdit.setTextPrompt(const Value: String);
begin
  FeditControl.TextPrompt := Value;
end;

{*************************************************************}
procedure TALEdit.SetKeyboardType(Value: TVirtualKeyboardType);
begin
  FeditControl.KeyboardType := Value;
end;

{*****************************************************}
function TALEdit.GetKeyboardType: TVirtualKeyboardType;
begin
  result := FeditControl.KeyboardType;
end;

{**************************************************}
procedure TALEdit.SetPassword(const Value: Boolean);
begin
  FeditControl.Password := Value;
end;

{************************************}
function TALEdit.GetPassword: Boolean;
begin
  result := FeditControl.Password;
end;

{*******************************************************}
procedure TALEdit.SetCheckSpelling(const Value: Boolean);
begin
  FeditControl.CheckSpelling := Value;
end;

{*****************************************}
function TALEdit.GetCheckSpelling: Boolean;
begin
  result := FeditControl.CheckSpelling;
end;

{**************************************************************}
procedure TALEdit.SetReturnKeyType(const Value: TReturnKeyType);
begin
  FeditControl.ReturnKeyType := Value;
end;

{************************************************}
function TALEdit.GetReturnKeyType: TReturnKeyType;
begin
  result := FeditControl.ReturnKeyType;
end;

{**************************************************}
procedure TALEdit.DoChangeTracking(Sender: TObject);
begin
  if assigned(fOnChangeTracking) and (not (csLoading in componentState)) then
    fOnChangeTracking(self); // << yes need to send self instead of the fEditControl
end;

{***********************************************}
procedure TALEdit.StrokeChanged(Sender: TObject);
var aRect: TrectF;
begin
  inherited;
  if Stroke.Kind = TbrushKind.None then fEditControl.Margins.Rect := TrectF.Create(0,0,0,0)
  else begin
    aRect := TrectF.Create(0,0,0,0);
    if (TSide.Top in Sides) then aRect.Top := Stroke.Thickness;
    if (TSide.bottom in Sides) then aRect.bottom := Stroke.Thickness;
    if (TSide.right in Sides) then aRect.right := Stroke.Thickness;
    if (TSide.left in Sides) then aRect.left := Stroke.Thickness;
    fEditControl.Margins.Rect := arect;
  end;
end;

{**********************************************}
procedure TALEdit.SetSides(const Value: TSides);
begin
  inherited;
  StrokeChanged(nil);
end;

{************************************}
function TALEdit.GetCanFocus: Boolean;
begin
  {$IF defined(DEBUG)}
  ALLog('TALEdit.GetCanFocus', 'GetCanFocus', TalLogType.VERBOSE);
  {$ENDIF}
  result := inherited GetCanFocus;
  if result then begin
    fEditControl.SetFocus;
    exit(false);   // << the canparentfocus is also set to false, so the TCommonCustomForm.NewFocusedControl(const Value: IControl)
                   //    will do nothing !
  end;
end;

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALEdit]);
end;

initialization
  RegisterFmxClasses([TALEdit]);

end.
