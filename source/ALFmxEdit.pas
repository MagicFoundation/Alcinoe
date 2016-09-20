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
    FLayout: JLinearLayout;
    FTextWatcher: TALTextWatcher;
    FEditorActionListener: TALEditorActionListener;
    FSoftInputListener: TALSoftInputListener;
    FKeyPreImeListener: TALKeyPreImeListener;
    FFocusChangeListener: TALFocusChangeListener;
    fApplicationEventMessageID: integer;
    procedure ApplicationEventHandler(const Sender : TObject; const M : TMessage);
  protected
    FEditText: JALEditText;
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure ParentChanged; override;
    procedure DoAbsoluteChanged; override;
    procedure Move; override;
    procedure Resize; override;
    procedure Show; override;
    procedure Hide; override;
    procedure ChangeOrder; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure setInputType(const aKeyboardType: TVirtualKeyboardType; const aPassword: Boolean);
    procedure setReturnKeyType(const aReturnKeyType: TReturnKeyType);
    property OnChangeTracking: TNotifyEvent read fOnChangeTracking write fOnChangeTracking;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure realignContent; virtual;
  end;
{$endif}

{$IF defined(ios)}
type

  {**************************}
  TALIosEdit = class(TControl)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
{$endif}

type

  {***************************}
  TALEdit = class(TALRectangle)
  private
    fOnChangeTracking: TNotifyEvent;
    FTextSettings: TTextSettings;
    {$IF defined(android)}
    fEditControl: TALAndroidEdit;
    fKeyboardType: TVirtualKeyboardType;
    fPassword: boolean;
    fReturnKeyType: TReturnKeyType;
    function GetAndroidEditText: JALEditText;
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
    procedure SetReturnKeyType(Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
  protected
    function GetDefaultSize: TSizeF; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF defined(android)}
    property AndroidEditText: JALEditText read GetAndroidEditText;
    {$ENDIF}
  published
    property TabOrder;
    property TabStop;
    property Cursor default crIBeam;
    property CanFocus default True;
    property CanParentFocus;
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
    property TouchTargetExpansion;
    //property Caret;
    //property KillFocusByReturn; => always true
    //property CheckSpelling;
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

 {$IF defined(android)}
uses System.SysUtils,
     Androidapi.Helpers,
     Androidapi.Input,
     Androidapi.KeyCodes,
     Androidapi.JNI.App,
     FMX.Platform,
     FMX.Platform.Android,
     FMX.Helpers.Android,
     FMX.Forms,
     AlFmxCommon;
{$endif}

{$IF defined(android)}

{****************************************************}
constructor TALAndroidEdit.Create(AOwner: TComponent);
var aScreenSrv: IFMXScreenService;
begin
  inherited create(AOwner);
  //-----
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  //-----
  CanFocus := True;
  fApplicationEventMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventHandler);
  fOnChangeTracking := nil;
  //-----
  CallInUIThreadAndWaitFinishing(
    procedure
    var aLayoutParams: JViewGroup_LayoutParams;
    begin
      FLayout := TJLinearLayout.JavaClass.init(TAndroidHelper.Activity);
      FLayout.setfocusable(true);               // << this is important to remove the focus from the FeditText
      FLayout.setfocusableInTouchMode(true);    //    else fedittext will always receive back the focus after we remove it
      //-----
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
end;

{********************************}
destructor TALAndroidEdit.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, fApplicationEventMessageID);
  CallInUIThreadAndWaitFinishing(
    procedure
    begin

      FEditText.setVisibility(TJView.JavaClass.INVISIBLE);
      FEditText.setOnFocusChangeListener(nil);
      FEditText.RemoveTextChangedListener(FTextWatcher);
      FEditText.setOnEditorActionListener(nil);
      FEditText.SetSoftInputListener(nil);
      FEditText.SetKeyPreImeListener(nil);

      freeandNil(FFocusChangeListener);
      freeandNil(FTextWatcher);
      freeandNil(fEditorActionListener);
      freeandNil(FSoftInputListener);
      freeandNil(FKeyPreImeListener);

      FEditText := nil;
      FLayout.removeAllViews();
      MainActivity.getViewStack.removeView(FLayout);
      FLayout := nil;

    end);
  inherited;
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

{****************************}
procedure TALAndroidEdit.Hide;
begin
  inherited;
  RealignContent;
end;

{****************************}
procedure TALAndroidEdit.Move;
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

{****************************}
procedure TALAndroidEdit.Show;
begin
  inherited;
  RealignContent;
end;

{*******************************}
procedure TALAndroidEdit.DoEnter;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.DoEnter.START', 'START', TalLogType.VERBOSE);
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
  ALLog('TALAndroidEdit.DoEnter.END', 'END', TalLogType.VERBOSE);
  {$ENDIF}
end;

{******************************}
procedure TALAndroidEdit.DoExit;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.DoExit', 'DoExit', TalLogType.VERBOSE);
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
end;

{*********************************************************************************************************}
procedure TALAndroidEdit.setInputType(const aKeyboardType: TVirtualKeyboardType; const aPassword: Boolean);
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

  aInputType := 0;
  case aKeyboardType of
    TVirtualKeyboardType.Alphabet:              aInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT or TJInputType.JavaClass.TYPE_TEXT_FLAG_NO_SUGGESTIONS;
    TVirtualKeyboardType.URL:                   aInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT or TJInputType.JavaClass.TYPE_TEXT_VARIATION_URI;
    TVirtualKeyboardType.NamePhonePad:          aInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT;
    TVirtualKeyboardType.EmailAddress:          aInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT or TJInputType.JavaClass.TYPE_TEXT_VARIATION_EMAIL_ADDRESS;
    TVirtualKeyboardType.NumbersAndPunctuation: aInputType := TJInputType.JavaClass.TYPE_CLASS_NUMBER or TJInputType.JavaClass.TYPE_NUMBER_FLAG_DECIMAL;
    TVirtualKeyboardType.NumberPad:             aInputType := TJInputType.JavaClass.TYPE_CLASS_NUMBER;
    TVirtualKeyboardType.PhonePad:              aInputType := TJInputType.JavaClass.TYPE_CLASS_PHONE;
    else {TVirtualKeyboardType.Default}         aInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT;
  end;

  if aPassword then begin
    case aKeyboardType of
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

  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      FEditText.setInputType(aInputType);
    end);

end;

{******************************************************************************}
procedure TALAndroidEdit.setReturnKeyType(const aReturnKeyType: TReturnKeyType);
var aimeOptions: integer;
begin
  case aReturnKeyType of
    TReturnKeyType.Done:          aimeOptions := TJEditorInfo.JavaClass.IME_ACTION_DONE;
    TReturnKeyType.Go:            aimeOptions := TJEditorInfo.JavaClass.IME_ACTION_GO;
    TReturnKeyType.Next:          aimeOptions := TJEditorInfo.JavaClass.IME_ACTION_NEXT;
    TReturnKeyType.Search:        aimeOptions := TJEditorInfo.JavaClass.IME_ACTION_SEARCH;
    TReturnKeyType.Send:          aimeOptions := TJEditorInfo.JavaClass.IME_ACTION_SEND;
    else {TReturnKeyType.Default} aimeOptions := TJEditorInfo.JavaClass.IME_ACTION_NONE;
  end;

  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      FEditText.setImeOptions(aimeOptions);
    end);

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
  if assigned(fEditControl.fOnChangeTracking) then begin
    TThread.Queue(nil,
      procedure
      begin
        fEditControl.fOnChangeTracking(fEditControl);
      end);
  end;
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

{************************************************}
constructor TALIosEdit.Create(AOwner: TComponent);
begin
end;

{****************************}
destructor TALIosEdit.Destroy;
begin
end;

{$endif}

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

{*********************************************}
constructor TALEdit.Create(AOwner: TComponent);
begin
  inherited;
  fOnChangeTracking := nil;
  Cursor := crIBeam;
  CanFocus := True;
  fill.DefaultColor := $ffffffff;
  fill.Color := $ffffffff;
  stroke.DefaultKind := TBrushKind.none;
  stroke.kind := TBrushKind.none;
  {$IF defined(android)}
  fEditControl := TALAndroidEdit.Create(self);
  fEditControl.Parent := self;
  FeditControl.OnChangeTracking := DoChangeTracking;
  fKeyboardType := TVirtualKeyboardType.Default;
  fPassword:= false;
  fReturnKeyType := tReturnKeyType.Default;
  fEditControl.setInputType(fKeyboardType, fPassword);
  fEditControl.setReturnKeyType(fReturnKeyType);
  {$ELSE}
  fEditControl := TEdit.Create(self);
  fEditControl.Parent := self;
  FeditControl.Stored := False;
  FeditControl.SetSubComponent(True);
  FeditControl.Locked := True;
  fEditControl.ControlType := TcontrolType.platform; // << on windows platform is not good as Styled
  FeditControl.StyleLookup := 'transparentedit';
  FeditControl.StyledSettings := []; // Family, Size, Style, FontColor, Other
  FeditControl.OnChangeTracking := DoChangeTracking;
  fEditControl.KillFocusByReturn := True;
  fEditControl.KeyboardType := TVirtualKeyboardType.Default; // noops operation
  fEditControl.Password := false; // noops operation
  fEditControl.ReturnKeyType := tReturnKeyType.Default;  // noops operation
  {$ENDIF}
  FTextSettings := TALEditTextSettings.Create(Self);
  FTextSettings.OnChanged := OnFontChanged;
  fEditControl.Align := TAlignLayout.Client;
end;

{*************************}
destructor TALEdit.Destroy;
begin
  FTextSettings.Free;
  fEditControl.DisposeOf;
  inherited;
end;

{***********************}
procedure TALEdit.Loaded;
begin
  inherited;
  OnFontChanged(nil);
end;

{**************************************}
function TALEdit.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(100, 22);
end;

{*************************************}
function TALEdit.GetTextPrompt: String;
{$IF defined(android)}
var aHint: JCharSequence;
{$ENDIF}
begin
  {$IF defined(android)}
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      aHint := fEditControl.FEditText.getHint;
    end);
  result := JCharSequenceToStr(aHint);
  {$ELSE}
  result := FeditControl.TextPrompt;
  {$ENDIF}
end;

{***************************************************}
procedure TALEdit.setTextPrompt(const Value: String);
begin
  {$IF defined(android)}
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      fEditControl.FEditText.setHint(StrToJCharSequence(Value));
    end);
  {$ELSE}
  FeditControl.TextPrompt := Value;
  {$ENDIF}
end;

{********************}
{$IF defined(android)}
function TALEdit.GetAndroidEditText: JALEditText;
begin
  result := fEditControl.FEditText;
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
{$IF defined(android)}
var aTypeface: JTypeface;
    aStyle: integer;
    aGravity: integer;
{$ENDIF}
begin
  if csLoading in componentState then exit;
  {$IF defined(android)}
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      //-----
      FEditControl.feditText.setTextColor(ftextsettings.fontcolor); // << Sets the text color for all the states (normal, selected, focused) to be this color.
      FEditControl.feditText.setTextSize(ftextsettings.font.size); // << Set the default text size to the given value, interpreted as "scaled pixel" units.
                                                                   //    This size is adjusted based on the current density and user font size preference.
      //-----
      if (TFontStyle.fsBold in ftextsettings.font.style) and
         (TFontStyle.fsItalic in ftextsettings.font.style) then aStyle := TJTypeface.JavaClass.BOLD_ITALIC
      else if (TFontStyle.fsBold in ftextsettings.font.style) then aStyle := TJTypeface.JavaClass.BOLD
      else if (TFontStyle.fsItalic in ftextsettings.font.style) then aStyle := TJTypeface.JavaClass.ITALIC
      else aStyle := TJTypeface.JavaClass.NORMAL;
      aTypeface := TJTypeface.JavaClass.create(StringToJString(ftextsettings.font.Family), aStyle);
      FEditControl.feditText.setTypeface(aTypeface); // << Sets the typeface and style in which the text should be displayed. Note that not all
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
      FEditControl.feditText.setgravity(aGravity);
    end);
  //-----
  {$else}
  FEditControl.TextSettings.Assign(ftextsettings);
  {$ENDIF}
end;

{*********************************************}
procedure TALEdit.SetText(const Value: String);
begin
  {$IF defined(android)}
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      fEditControl.FEditText.setText(StrToJCharSequence(Value), TJTextView_BufferType.javaClass.EDITABLE);
    end);
  {$ELSE}
  FeditControl.Text := Value;
  {$ENDIF}
end;

{*******************************}
function TALEdit.getText: String;
{$IF defined(android)}
var aText: JCharSequence;
{$ENDIF}
begin
  {$IF defined(android)}
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      aText := fEditControl.FEditText.gettext;
    end);
  result := JCharSequenceToStr(aText);
  {$ELSE}
  result := FeditControl.Text;
  {$ENDIF}
end;

{*************************************************************}
procedure TALEdit.SetKeyboardType(Value: TVirtualKeyboardType);
begin
  {$IF defined(android)}
  if (value <> fKeyboardType) then begin
    fKeyboardType := Value;
    fEditControl.setInputType(fKeyboardType, fPassword);
  end;
  {$ELSE}
  FeditControl.KeyboardType := Value;
  {$ENDIF}
end;

{*****************************************************}
function TALEdit.GetKeyboardType: TVirtualKeyboardType;
begin
  {$IF defined(android)}
  result := fKeyboardType;
  {$ELSE}
  result := FeditControl.KeyboardType;
  {$ENDIF}
end;

{**************************************************}
procedure TALEdit.SetPassword(const Value: Boolean);
begin
  {$IF defined(android)}
  if (value <> fPassword) then begin
    fPassword := Value;
    fEditControl.setInputType(fKeyboardType, fPassword);
  end;
  {$ELSE}
  FeditControl.Password := Value;
  {$ENDIF}
end;

{************************************}
function TALEdit.GetPassword: Boolean;
begin
  {$IF defined(android)}
  result := fPassword;
  {$ELSE}
  result := FeditControl.Password;
  {$ENDIF}
end;

{********************************************************}
procedure TALEdit.SetReturnKeyType(Value: TReturnKeyType);
begin
  {$IF defined(android)}
  if (value <> fReturnKeyType) then begin
    fReturnKeyType := Value;
    fEditControl.setReturnKeyType(fReturnKeyType);
  end;
  {$ELSE}
  FeditControl.ReturnKeyType := Value;
  {$ENDIF}
end;

{************************************************}
function TALEdit.GetReturnKeyType: TReturnKeyType;
begin
  {$IF defined(android)}
  result := fReturnKeyType;
  {$ELSE}
  result := FeditControl.ReturnKeyType;
  {$ENDIF}
end;

{**************************************************}
procedure TALEdit.DoChangeTracking(Sender: TObject);
begin
  if assigned(fOnChangeTracking) then
    fOnChangeTracking(self); // << yes need to send self instead of the fEditControl
end;


procedure Register;
begin
  RegisterComponents('Alcinoe', [TALEdit]);
end;

initialization
  RegisterFmxClasses([TALEdit]);

end.
