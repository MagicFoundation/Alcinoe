unit ALFmxMemo;

interface

{$IF defined(MACOS) and not defined(IOS)}
  {$DEFINE _MACOS}
{$ENDIF}

uses System.Types,
     system.Classes,
     System.UITypes,
     {$IF defined(ANDROID)}
     ALANdroidApi,
     {$ELSEIF defined(IOS)}
     System.TypInfo,
     iOSapi.Foundation,
     iOSapi.UIKit,
     Macapi.ObjectiveC,
     Macapi.ObjCRuntime,
     ALIosNativeView,
     ALIosScrollBox,
     {$ELSE}
     FMX.StdCtrls,
     FMX.Memo.style,
     FMX.Memo,
     {$ENDIF}
     FMX.types,
     Fmx.Graphics,
     Fmx.controls,
     AlFMXEdit,
     ALFmxObjects;

{$REGION ' IOS'}
{$IF defined(ios)}
type

  {***********************************}
  IALUITextView = interface(UITextView)
    ['{3F2E6377-8394-4CD8-B9E8-CF86673B6B2C}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure HandlePan(gestureRecognizer: UIPanGestureRecognizer); cdecl;
    function gestureRecognizerShouldBegin(gestureRecognizer: UIGestureRecognizer): Boolean; cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function canPerformAction(action: SEL; withSender: Pointer): Boolean; cdecl;
  end;

  {******************************}
  TALIosTextViewDelegate = class;
  TALIosMemo = class;

  {*************************************}
  TALIosTextView = class(TALIosScrollBox)
  private
    [Weak] FMemoControl: TALIosMemo;
    function GetView: UITextView;
  protected
    function CreateDelegate: TALIosScrollBoxDelegate; override;
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create; overload; override;
    constructor Create(const AControl: TControl); overload; override;
    destructor Destroy; override;
    function canBecomeFirstResponder: Boolean; cdecl;
    function canPerformAction(action: SEL; withSender: Pointer): Boolean; cdecl;
    property View: UITextView read GetView;
  end;

  {*************************************************************************}
  TALIosTextViewDelegate = class(TALIosScrollBoxDelegate, UITextViewDelegate)
  private
    [Weak] FTextView: TALIosTextView;
  public
    constructor Create(const ATextView: TALIosTextView);
    procedure textViewDidBeginEditing(textView: UITextView); cdecl;
    procedure textViewDidChange(textView: UITextView); cdecl;
    procedure textViewDidChangeSelection(textView: UITextView); cdecl;
    procedure textViewDidEndEditing(textView: UITextView); cdecl;
    function textViewShouldBeginEditing(textView: UITextView): Boolean; cdecl;
    function textViewShouldEndEditing(textView: UITextView): Boolean; cdecl;
    [MethodName('textView:shouldChangeTextInRange:replacementText:')]
    function textViewShouldChangeTextInRangeReplacementText(textView: UITextView; shouldChangeTextInRange: NSRange; replacementText: NSString): Boolean; cdecl;
    [MethodName('textView:shouldInteractWithURL:inRange:')]
    function textViewShouldInteractWithURLInRange(textView: UITextView; shouldInteractWithURL: NSURL; inRange: NSRange): Boolean; cdecl;
    [MethodName('textView:shouldInteractWithTextAttachment:inRange:')]
    function textViewShouldInteractWithTextAttachmentInRange(textView: UITextView; shouldInteractWithTextAttachment: NSTextAttachment; inRange: NSRange): Boolean; cdecl;
  end;

  {**************************}
  TALIosMemo = class(TControl)
  private
    FPadding: TBounds;
    FTextView: TALIosTextView;
    fTextPrompt: String;
    fTextPromptColor: TalphaColor;
    fTextPromptVisible: Boolean;
    fLineSpacingMultiplier: single;
    fLineSpacingExtra: single;
    fOnChangeTracking: TNotifyEvent;
    FTextSettings: TTextSettings;
    fMaxLength: integer;
    procedure setKeyboardType(const Value: TVirtualKeyboardType);
    function GetKeyboardType: TVirtualKeyboardType;
    procedure setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
    function GetAutoCapitalizationType: TALAutoCapitalizationType;
    procedure setPassword(const Value: Boolean);
    function GetPassword: Boolean;
    procedure setCheckSpelling(const Value: Boolean);
    function GetCheckSpelling: Boolean;
    procedure setReturnKeyType(const Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
    function GetTextPrompt: String;
    procedure setTextPrompt(const Value: String);
    function GetTextPromptColor: TAlphaColor;
    procedure setTextPromptColor(const Value: TAlphaColor);
    function GetTintColor: TAlphaColor;
    procedure setTintColor(const Value: TAlphaColor);
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
    function getText: String;
    procedure SetText(const Value: String);
    function GetLineSpacingMultiplier: single;
    procedure SetLineSpacingMultiplier(const Value: single);
    function GetLineSpacingExtra: single;
    procedure SetLineSpacingExtra(const Value: single);
    procedure DoFontChanged;
    procedure OnFontChanged(Sender: TObject);
    procedure SetPadding(const Value: TBounds);
    function GetPadding: TBounds;
  protected
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
    procedure DoEndUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function getLineHeight: Single; // << it's include the line spacing
    function getLineCount: integer;
    procedure RecalcOpacity; override;
    procedure RecalcEnabled; override;
    Procedure AddNativeView;
    Procedure RemoveNativeView;
    function PointInObjectLocal(X: Single; Y: Single): Boolean; override;
    property OnChangeTracking: TNotifyEvent read fOnChangeTracking write fOnChangeTracking;
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType;
    property AutoCapitalizationType: TALAutoCapitalizationType read GetAutoCapitalizationType write SetAutoCapitalizationType;
    property Password: Boolean read GetPassword write SetPassword default False;
    property TextPrompt: String read GetTextPrompt write setTextPrompt;
    property TextPromptColor: TAlphaColor read GetTextPromptColor write setTextPromptColor default TalphaColorRec.null; // << null mean use the default color
    property TintColor: TAlphaColor read GetTintColor write setTintColor; // << null mean use the default color
    property MaxLength: integer read fMaxLength write fMaxLength default 0;
    property Text: String read getText write SetText;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property CheckSpelling: Boolean read GetCheckSpelling write SetCheckSpelling default true;
    property LineSpacingMultiplier: single read GetLineSpacingMultiplier write SetLineSpacingMultiplier; // <<  Each line will have its height multiplied by LineSpacingMultiplier
    property LineSpacingExtra: single read GetLineSpacingExtra write SetLineSpacingExtra; // <<  Each line will have its height added by LineSpacingExtra
    property TextView: TALIosTextView read FTextView;
    property Padding: TBounds read GetPadding write SetPadding;
  end;

{$endif}
{$ENDREGION}

{$REGION ' WINDOWS / MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
type

  {**************************}
  TALStyledMemo = class(TMemo)
  private
    fReturnKeyType: TReturnKeyType;
    [Weak] fStyledMemo: TStyledMemo;
    FPadding: TBounds;
    [Weak] FVertScrollBar: TscrollBar;
    fTextPromptControl: TalText;
    FOnChangeTracking: TNotifyEvent;
    FTextSettings: TTextSettings;
    procedure OnApplyStyleLookupImpl(sender: Tobject);
    procedure SetPadding(const Value: TBounds);
    function GetPadding: TBounds;
    function GetTextPrompt: String;
    procedure setTextPrompt(const Value: String);
    function GetTextPromptColor: TAlphaColor;
    procedure setTextPromptColor(const Value: TAlphaColor);
    procedure OnChangeTrackingImpl(sender: Tobject);
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
    procedure OnFontChanged(Sender: TObject);
  protected
    procedure DoEnter; override;
    procedure realignScrollBars; virtual;
    procedure Resize; override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function getLineHeight: Single;
    function getLineCount: integer;
    property TextPrompt: String read GetTextPrompt write setTextPrompt;
    property TextPromptColor: TAlphaColor read GetTextPromptColor write setTextPromptColor default TalphaColorRec.null; // << null mean use the default color
    property Padding: TBounds read GetPadding write SetPadding;
    property OnChangeTracking: TNotifyEvent read FOnChangeTracking write FOnChangeTracking;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property ReturnKeyType: TReturnKeyType read fReturnKeyType write fReturnKeyType default TReturnKeyType.Default; // << we don't use it
  end;

{$endif}
{$ENDREGION}

type

  {***************************}
  TALMemo = class(TALRectangle)
  private
    fPadding: TBounds;
    fDefStyleAttr: String;
    FAutoTranslate: Boolean;
    FAutoConvertFontFamily: boolean;
    fOnChangeTracking: TNotifyEvent;
    fOnEnter: TNotifyEvent;
    fOnExit: TNotifyEvent;
    FTextSettings: TTextSettings;
    {$IF defined(android)}
    fTintColor: TalphaColor;
    fAutoCapitalizationType: TALAutoCapitalizationType;
    fMemoControl: TALAndroidEdit;
    function GetAndroidEditText: TALAndroidEditText;
    {$ELSEIF defined(IOS)}
    fMemoControl: TALIosMemo;
    function GetIosTextView: TALIosTextView;
    {$ELSE}
    fTintColor: TalphaColor;
    fAutoCapitalizationType: TALAutoCapitalizationType;
    fLineSpacingMultiplier: Single;
    fLineSpacingExtra: Single;
    fMemoControl: TALStyledMemo;
    {$ENDIF}
    function GetTextPrompt: String;
    procedure setTextPrompt(const Value: String);
    function GetTextPromptColor: TAlphaColor;
    procedure setTextPromptColor(const Value: TAlphaColor);
    function GetTintColor: TAlphaColor;
    procedure setTintColor(const Value: TAlphaColor);
    function GetLineSpacingMultiplier: single;
    procedure SetLineSpacingMultiplier(const Value: single);
    function LineSpacingMultiplierStored: boolean;
    function GetLineSpacingExtra: single;
    procedure SetLineSpacingExtra(const Value: single);
    function LineSpacingExtraStored: boolean;
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
    procedure OnFontChanged(Sender: TObject);
    function getText: String;
    procedure SetText(const Value: String);
    procedure OnChangeTrackingImpl(Sender: TObject);
    procedure OnEnterImpl(Sender: TObject);
    procedure OnExitImpl(Sender: TObject);
    procedure SetKeyboardType(Value: TVirtualKeyboardType);
    function GetKeyboardType: TVirtualKeyboardType;
    procedure setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
    function GetAutoCapitalizationType: TALAutoCapitalizationType;
    procedure SetCheckSpelling(const Value: Boolean);
    function GetCheckSpelling: Boolean;
    procedure SetReturnKeyType(const Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
    procedure SetDefStyleAttr(const Value: String);
    procedure SetPadding(const Value: TBounds);
    function GetPadding: TBounds;
    procedure CreateMemoControl;
    procedure PaddingChangedHandler(Sender: TObject);
    function GetContainFocus: Boolean;
    procedure SetMaxLength(const Value: integer);
    function GetMaxLength: integer;
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
    property AndroidEditText: TALAndroidEditText read GetAndroidEditText;
    {$ELSEIF defined(IOS)}
    property IosTextView: TALIosTextView read GetIosTextView;
    {$ENDIF}
    Procedure AddNativeView;
    Procedure RemoveNativeView;
    Procedure setSelection(const aStart: integer; const aStop: Integer); overload;
    Procedure setSelection(const aindex: integer); overload;
    function getLineHeight: Single; // << it's include the line spacing
    function getLineCount: Integer;
    property ContainFocus: Boolean read GetContainFocus;
  published
    property DefStyleAttr: String read fDefStyleAttr write SetDefStyleAttr; // << android only - the name of An attribute in the current theme that contains a
                                                                            // << reference to a style resource that supplies defaults values for the StyledAttributes.
                                                                            // << exemple of use: https://stackoverflow.com/questions/5051753/how-do-i-apply-a-style-programmatically
                                                                            // << NOTE: !!IMPORTANT!! This properties must be defined the very first because the stream system
                                                                            // <<       must load it the very first
    property TabOrder;
    property TabStop;
    property Cursor default crIBeam;
    property CanFocus default True;
    //property CanParentFocus;
    property DisableFocusEffect;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType default TVirtualKeyboardType.Default;
    property AutoCapitalizationType: TALAutoCapitalizationType read GetAutoCapitalizationType write SetAutoCapitalizationType default TALAutoCapitalizationType.acNone;
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType default TReturnKeyType.Default;
    //property ReadOnly;
    property MaxLength: integer read GetMaxLength write SetMaxLength default 0;
    //property FilterChar;
    property Text: String read getText write SetText;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property Hint;
    property TextPrompt: String read GetTextPrompt write setTextPrompt;
    property TextPromptColor: TAlphaColor read GetTextPromptColor write setTextPromptColor default TalphaColorRec.null; // << null mean use the default TextPromptColor
    property LineSpacingMultiplier: single read GetLineSpacingMultiplier write SetLineSpacingMultiplier stored LineSpacingMultiplierStored; // <<  Each line will have its height multiplied by LineSpacingMultiplier
    property LineSpacingExtra: single read GetLineSpacingExtra write SetLineSpacingExtra stored LineSpacingExtraStored; // <<  Each line will have its height added LineSpacingExtra
    property TintColor: TAlphaColor read GetTintColor write setTintColor default TalphaColorRec.null; // << IOS only - the color of the cursor caret and the text selection handles. null mean use the default TintColor
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate default true; // << just the TextPrompt
    property AutoConvertFontFamily: Boolean read FAutoConvertFontFamily write fAutoConvertFontFamily default true;
    property TouchTargetExpansion;
    //property Caret;
    property CheckSpelling: Boolean read GetCheckSpelling write SetCheckSpelling default true;
    property ParentShowHint;
    property ShowHint;
    //property OnChange;
    property OnChangeTracking: TNotifyEvent read fOnChangeTracking write fOnChangeTracking;
    //property OnTyping;
    //property OnValidating;
    //property OnValidate;
    //property OnKeyDown; // << not work under android - it's like this with their @{[^# virtual keyboard :(
    //property OnKeyUp; // << not work under android - it's like this with their @{[^# virtual keyboard :(
    property OnEnter: TnotifyEvent Read fOnEnter Write fOnEnter;
    property OnExit: TnotifyEvent Read fOnExit Write fOnExit;
    property Padding: TBounds read GetPadding write SetPadding;
  end;

procedure Register;

implementation

uses {$IF defined(ANDROID)}
     {$ELSEIF defined(IOS)}
     System.SysUtils,
     Macapi.CoreFoundation,
     iOSapi.CocoaTypes,
     Macapi.Helpers,
     iOSapi.CoreText,
     FMX.Helpers.iOS,
     ALString,
     {$ELSE}
     FMX.Styles.Objects,
     FMX.BehaviorManager,
     FMX.Layouts,
     {$ENDIF}
     system.Math,
     system.Math.Vectors,
     fmx.consts,
     ALCommon,
     AlFmxCommon;

{**}
type
  TALMemoTextSettings = class(TTextSettings)
  public
    constructor Create(const AOwner: TPersistent); override;
  published
    property Font;
    property FontColor;
    property HorzAlign default TTextAlign.Leading;
    property VertAlign default TTextAlign.Leading;
    property WordWrap default True;
  end;

{****************************************************************}
constructor TALMemoTextSettings.Create(const AOwner: TPersistent);
begin
  inherited;
  HorzAlign := TTextAlign.Leading;
  VertAlign := TTextAlign.Leading;
  WordWrap := True;
end;

{$REGION ' IOS'}
{$IF defined(ios)}

{*********************************}
constructor TALIosTextView.Create;
var aUIColor: UIColor;
begin
  inherited;
  View.setExclusiveTouch(True);
  aUIColor := AlphaColorToUIColor(TalphaColorRec.Null);
  View.setbackgroundColor(aUIColor);
  //NOTE: if i try to release the aUIColor i have an exception
  //      so it's seam something acquire it
end;

{***********************************************************}
constructor TALIosTextView.Create(const AControl: TControl);
begin
  fMemoControl := TALIosMemo(AControl);
  inherited;
end;

{*********************************}
destructor TALIosTextView.Destroy;
begin
  inherited;
end;

{**************************************************************}
function TALIosTextView.CreateDelegate: TALIosScrollBoxDelegate;
begin
  Result := TALIosTextViewDelegate.Create(Self);
end;

{********************************************************}
function TALIosTextView.canBecomeFirstResponder: Boolean;
begin
  Result := Control.CanFocus and Control.HitTest;
end;

{**********************************************************************************}
function TALIosTextView.canPerformAction(action: SEL; withSender: Pointer): Boolean;
begin
  if FMemoControl.FTextPromptVisible and
     (action <> sel_getUid('paste:')) then result := false
  else result := UIScrollView(Super).canPerformAction(action, withSender);
end;

{*****************************************************}
function TALIosTextView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALUITextView);
end;

{******************************************}
function TALIosTextView.GetView: UITextView;
begin
  Result := inherited GetView<UITextView>;
end;

{*************************************************************************}
constructor TALIosTextViewDelegate.Create(const ATextView: TALIosTextView);
begin
  inherited Create;
  FTextView := ATextView;
  if FTextView = nil then
    raise EArgumentNilException.Create(Format(SWrongParameter, ['ATextView']));
end;

{*****************************************************************************}
procedure TALIosTextViewDelegate.TextViewDidBeginEditing(TextView: UITextView);
begin
  if not FTextView.Control.IsFocused then
    FTextView.Control.SetFocus;
end;

{***********************************************************************}
procedure TALIosTextViewDelegate.textViewDidChange(textView: UITextView);
begin
  {$IF defined(DEBUG)}
  //ALLog('TALIosTextViewDelegate.textViewDidChange', '', TalLogType.VERBOSE);
  {$ENDIF}
  if assigned(FTextView.fMemoControl.fOnChangeTracking) then
    FTextView.fMemoControl.fOnChangeTracking(FTextView.fMemoControl);
end;

{********************************************************************************}
procedure TALIosTextViewDelegate.textViewDidChangeSelection(textView: UITextView);
var aSelectedTextRange: NSRange;
begin
  {$IF defined(DEBUG)}
  //ALLog('TALIosTextViewDelegate.textViewDidChangeSelection', '', TalLogType.VERBOSE);
  {$ENDIF}
  if FTextView.FMemoControl.fTextPromptVisible then begin
    aSelectedTextRange := textView.selectedRange;
    if (aSelectedTextRange.location <> 0) or (aSelectedTextRange.length <> 0) then begin // << else i have a infinite loop when i cut a big text
      aSelectedTextRange.location := 0;
      aSelectedTextRange.length := 0;
      textView.setSelectedRange(aSelectedTextRange);
    end;
  end;
end;

{***************************************************************************}
procedure TALIosTextViewDelegate.textViewDidEndEditing(textView: UITextView);
begin
end;

{****************************************************************************************}
function TALIosTextViewDelegate.textViewShouldBeginEditing(textView: UITextView): Boolean;
begin
  Result := True;
end;

{*************************************************************************************************************************************************************************}
function TALIosTextViewDelegate.textViewShouldChangeTextInRangeReplacementText(textView: UITextView; shouldChangeTextInRange: NSRange; replacementText: NSString): Boolean;
begin

  {$IF defined(DEBUG)}
  //ALLog('TALIosTextViewDelegate.textViewShouldChangeTextInRangeReplacementText', '', TalLogType.VERBOSE);
  {$ENDIF}

  if FTextView.FMemoControl.maxLength > 0 then begin

    //https://stackoverflow.com/questions/433337/set-the-maximum-character-length-of-a-uitextfield
    if shouldChangeTextInRange.length + shouldChangeTextInRange.location > textView.text.length then exit(false);
    if textView.text.length + replacementText.length - shouldChangeTextInRange.length > NSUInteger(FTextView.FMemoControl.maxLength) then exit(False);

  end;

  if (replacementText.length > 0) then begin
    if FTextView.FMemoControl.FtextPromptVisible then begin
      textView.setText(StrToNsStr(''));
      FTextView.FMemoControl.FtextPromptVisible := False;
      FTextView.FMemoControl.DoFontChanged;
    end;
  end
  else if (shouldChangeTextInRange.location = 0) and
          (shouldChangeTextInRange.length = textView.text.length) and
          (not FTextView.FMemoControl.FtextPromptVisible) then begin

    FTextView.FMemoControl.FtextPromptVisible := True;
    FTextView.FMemoControl.DoFontChanged;

    //the problem is that if the memo is empty, if you press the return key
    //then then textViewDidChange will be fired that will in turn fire fOnChangeTracking
    //but if after you press the delete key (removing the break line character) then
    //the textViewDidChange will be not fired (maybe because of textViewDidChangeSelection)
    //so i fire manually the fOnChangeTracking here
    if assigned(FTextView.fMemoControl.fOnChangeTracking) then
      FTextView.fMemoControl.fOnChangeTracking(FTextView.fMemoControl);

  end;

  Result := True;

end;

{**************************************************************************************}
function TALIosTextViewDelegate.textViewShouldEndEditing(textView: UITextView): Boolean;
begin
  Result := True;
end;

{***********************************************************************************************************************************************************************************}
function TALIosTextViewDelegate.textViewShouldInteractWithTextAttachmentInRange(textView: UITextView; shouldInteractWithTextAttachment: NSTextAttachment; inRange: NSRange): Boolean;
begin
  Result := True;
end;

{**************************************************************************************************************************************************}
function TALIosTextViewDelegate.textViewShouldInteractWithURLInRange(textView: UITextView; shouldInteractWithURL: NSURL; inRange: NSRange): Boolean;
begin
  Result := True;
end;

{************************************************}
constructor TALIosMemo.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  fMaxLength := 0;
  CanFocus := True;
  FPadding := TBounds.Create(TRectF.Empty);
  fOnChangeTracking := nil;
  fTextPrompt := '';
  fTextPromptColor := TalphaColorRec.Null;
  fTextPromptVisible := True;
  fLineSpacingMultiplier := 1;
  fLineSpacingExtra := 0;
  FTextSettings := TALMemoTextSettings.Create(Self);
  FTextSettings.OnChanged := OnFontChanged;
  FTextView := TalIosTextView.create(self);
  SetReturnKeyType(tReturnKeyType.Default);
  SetKeyboardType(TVirtualKeyboardType.default);
  setAutoCapitalizationType(TALAutoCapitalizationType.acNone);
  SetPassword(false);
  SetCheckSpelling(True);
end;

{****************************}
destructor TALIosMemo.Destroy;
begin
  ALfreeandNil(FTextView);
  ALFreeAndNil(FTextSettings);
  ALFreeAndNil(FPadding);
  inherited Destroy;
end;

{****************************************}
function TALIosMemo.getLineHeight: Single;
begin
  result := (FTextView.View.font.lineHeight * fLineSpacingMultiplier) + fLineSpacingExtra;
end;

{****************************************}
function TALIosMemo.getLineCount: integer;
var aLineHeight: Single;
begin
  aLineHeight := getLineHeight;
  if compareValue(aLineHeight, 0, Tepsilon.Position) > 0 then result := round(FTextView.View.contentSize.height / aLineHeight)
  else result := 0;
  {$IF defined(DEBUG)}
  ALLog('TALIosMemo.getLineCount', 'ContentBounds.Height: ' + floattostr(FTextView.View.contentSize.height) +
                                   ' - LineHeight: ' + floattostr(aLineHeight) +
                                   ' - LineCount: ' + inttostr(result) +
                                   ' - Width: ' + floattostr(Width) +
                                   ' - Height: ' + floattostr(Height), TalLogType.VERBOSE);
  {$ENDIF}
end;

{**********************************************************************}
procedure TALIosMemo.SetKeyboardType(const Value: TVirtualKeyboardType);
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
  FTextView.View.setKeyboardType(aUIKeyboardType);
end;

{********************************************************}
function TALIosMemo.GetKeyboardType: TVirtualKeyboardType;
var aUIKeyboardType: UIKeyboardType;
begin
  aUIKeyboardType := FTextView.View.KeyboardType;
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

{*************************************************************************************}
procedure TALIosMemo.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
var aUITextAutoCapitalizationType: UITextAutoCapitalizationType;
begin
  case Value of
    TALAutoCapitalizationType.acWords:          aUITextAutoCapitalizationType := UITextAutoCapitalizationTypeWords;
    TALAutoCapitalizationType.acSentences:      aUITextAutoCapitalizationType := UITextAutoCapitalizationTypeSentences;
    TALAutoCapitalizationType.acAllCharacters:  aUITextAutoCapitalizationType := UITextAutoCapitalizationTypeAllCharacters;
    else {TALAutoCapitalizationType.acNone}     aUITextAutoCapitalizationType := UITextAutoCapitalizationTypeNone;
  end;
  FTextView.View.setAutoCapitalizationType(aUITextAutoCapitalizationType);
end;

{***********************************************************************}
function TALIosMemo.GetAutoCapitalizationType: TALAutoCapitalizationType;
var aUITextAutoCapitalizationType: UITextAutoCapitalizationType;
begin
  aUITextAutoCapitalizationType := FTextView.View.AutoCapitalizationType;
  case aUITextAutoCapitalizationType of
    UITextAutoCapitalizationTypeWords:         result := TALAutoCapitalizationType.acWords;
    UITextAutoCapitalizationTypeSentences:     result := TALAutoCapitalizationType.acSentences;
    UITextAutoCapitalizationTypeAllCharacters: result := TALAutoCapitalizationType.acAllCharacters;
    else                                       result := TALAutoCapitalizationType.acNone;
  end;
end;

{*****************************************************}
procedure TALIosMemo.SetPassword(const Value: Boolean);
begin
  FTextView.View.setSecureTextEntry(Value);
end;

{***************************************}
function TALIosMemo.GetPassword: Boolean;
begin
  result := FTextView.View.isSecureTextEntry;
end;

{**********************************************************}
procedure TALIosMemo.SetCheckSpelling(const Value: Boolean);
begin
  if Value then begin
    FTextView.View.setSpellCheckingType(UITextSpellCheckingTypeYes);
    FTextView.View.setAutocorrectionType(UITextAutocorrectionTypeDefault);
  end
  else begin
    FTextView.View.setSpellCheckingType(UITextSpellCheckingTypeNo);
    FTextView.View.setAutocorrectionType(UITextAutocorrectionTypeNo);
  end;
end;

{********************************************}
function TALIosMemo.GetCheckSpelling: Boolean;
begin
  result := FTextView.View.SpellCheckingType = UITextSpellCheckingTypeYes;
end;

{*****************************************************************}
procedure TALIosMemo.setReturnKeyType(const Value: TReturnKeyType);
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
  FTextView.View.setReturnKeyType(aUIReturnKeyType);
end;

{***************************************************}
function TALIosMemo.GetReturnKeyType: TReturnKeyType;
var aUIReturnKeyType: UIReturnKeyType;
begin
  aUIReturnKeyType := FTextView.View.ReturnKeyType;
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
function TALIosMemo.GetTextPrompt: String;
begin
  if fTextPrompt <> '' then Result := fTextPrompt.Substring(0, fTextPrompt.length-1) // skip the ' ' that we add in settextprompt
  else Result := fTextPrompt;
end;

{******************************************************}
procedure TALIosMemo.setTextPrompt(const Value: String);
begin
  if Value <> fTextPrompt then begin
    fTextPrompt := Value;
    if fTextPrompt <> '' then fTextPrompt := fTextPrompt + ' '; // this not fully disable the spellchecking popup but it's mitigate it a little
                                                                // now the spell checking popup will appear only if you click on the last word and
                                                                // if the last word contain mistake in the current keyboard language
    if fTextPromptVisible then DoFontChanged;
  end;
end;

{**************************************************}
function TALIosMemo.GetTextPromptColor: TAlphaColor;
begin
  result := fTextPromptColor;
end;

{****************************************************************}
procedure TALIosMemo.setTextPromptColor(const Value: TAlphaColor);
begin
  if Value <> fTextPromptColor then begin
    fTextPromptColor := Value;
    if fTextPromptVisible then DoFontChanged;
  end;
end;

{********************************************}
function TALIosMemo.GetTintColor: TAlphaColor;
var red: CGFloat;
    green: CGFloat;
    blue: CGFloat;
    alpha: CGFloat;
begin
  if not FTextView.View.tintColor.getRed(@red, @green, @blue, @alpha) then result := TalphaColorRec.Null
  else result := TAlphaColorF.Create(red, green, blue, alpha).ToAlphaColor;
end;

{**********************************************************}
procedure TALIosMemo.setTintColor(const Value: TAlphaColor);
begin
  if Value <> TalphaColorRec.Null then
    FTextView.View.setTintColor(AlphaColorToUIColor(Value));
end;

{***************************************************}
function TALIosMemo.GetLineSpacingMultiplier: single;
begin
  result := fLineSpacingMultiplier;
end;

{*****************************************************************}
procedure TALIosMemo.SetLineSpacingMultiplier(const Value: single);
begin
  if not samevalue(Value, fLineSpacingMultiplier, Tepsilon.FontSize) then begin
    fLineSpacingMultiplier := Value;
    DoFontChanged;
  end;
end;

{**********************************************}
function TALIosMemo.GetLineSpacingExtra: single;
begin
  result := fLineSpacingExtra;
end;

{************************************************************}
procedure TALIosMemo.SetLineSpacingExtra(const Value: single);
begin
  if not samevalue(Value, fLineSpacingExtra, Tepsilon.FontSize) then begin
    fLineSpacingExtra := Value;
    DoFontChanged;
  end;
end;

{**********************************}
function TALIosMemo.getText: String;
begin
  if fTextPromptVisible then result := ''
  else result := NSStrToStr(FTextView.View.text);
end;

{************************************************}
procedure TALIosMemo.SetText(const Value: String);
begin

  FTextView.View.setText(StrToNSStr(Value));

  if fTextPromptVisible and (Value <> '') then begin
    fTextPromptVisible := False;
    DoFontChanged;
  end
  else if (Value = '') then begin
    fTextPromptVisible := true;
    DoFontChanged;
  end;

  //textViewDidChange
  //The text view calls this method in response to user-initiated
  //changes to the text. This method is not called in response to
  //programmatically initiated changes.
  if assigned(fOnChangeTracking) then
    fOnChangeTracking(self);

end;

{*********************************}
procedure TALIosMemo.DoFontChanged;
var aTextAttr: NSMutableAttributedString;
    aTextRange: NSRange;
    aUIColor: UIColor;
    aFontRef: CTFontRef;
    aParagraphStyle: NSMutableParagraphStyle;
    aStr: NSString;
    aNeedResetText: Boolean;
begin

  {$IF defined(DEBUG)}
  ALLog('TALIosMemo.DoFontChanged', 'TextPromptVisible: ' + BoolToStr(fTextPromptVisible, true) +
                                    ' - TextPrompt: ' + fTextPrompt +
                                    ' - TextPromptColor: ' + inttostr(fTextPromptColor), TalLogType.VERBOSE);
  {$ENDIF}

  if fTextPromptVisible then aStr := StrToNsStr(fTextPrompt)
  else aStr := FTextView.View.text;
  if aStr.length = 0 then begin
    aStr := StrToNsStr(' ');
    aNeedResetText := True;
  end
  else aNeedResetText := False;

  aTextAttr := TNSMutableAttributedString.Wrap(TNSMutableAttributedString.Alloc.initWithString(aStr));
  try

    aTextAttr.beginEditing;
    try

      aTextRange := NSMakeRange(0, aStr.Length);

      aFontRef := ALGetCTFontRef(fTextSettings.Font.Family, fTextSettings.Font.Size, fTextSettings.Font.Style);
      if aFontRef <> nil then begin
        try
          aTextAttr.addAttribute(TNSString.Wrap(kCTFontAttributeName), aFontRef, aTextRange);
        finally
          CFRelease(aFontRef);
        end;
      end;

      if fTextPromptVisible then begin
        if (fTextPromptColor = TalphaColorRec.Null) then aUIColor := AlphaColorToUIColor(TalphaColorRec.Lightgray)
        else aUIColor := AlphaColorToUIColor(fTextPromptColor);
      end
      else aUIColor := AlphaColorToUIColor(fTextSettings.FontColor);
      aTextAttr.addAttribute(NSForegroundColorAttributeName, (aUIColor as ILocalObject).GetObjectID, aTextRange);
      //NOTE: if i try to release the aUIColor i have an exception
      //      so it's seam something acquire it

      aParagraphStyle := TNSMutableParagraphStyle.Alloc;
      try
        aParagraphStyle.init;
        aParagraphStyle.setlineHeightMultiple(fLineSpacingMultiplier);
        aParagraphStyle.setLineSpacing(fLineSpacingExtra);
        aParagraphStyle.setAlignment(TextAlignToUITextAlignment(fTextSettings.HorzAlign));
        aTextAttr.addAttribute(NSParagraphStyleAttributeName, (aParagraphStyle as ILocalObject).GetObjectID, aTextRange);
      finally
        aParagraphStyle.release;
      end;

    finally
      aTextAttr.endEditing;
    end;

    FTextView.View.setAttributedText(aTextAttr);
    if aNeedResetText then FTextView.View.setText(StrToNSStr('')); // << the only way i found to force the setAttributedText else with empty String setAttributedText not work :(

  finally
    aTextAttr.release;
  end;

end;

{**************************************************}
procedure TALIosMemo.OnFontChanged(Sender: TObject);
begin
  DoFontChanged;
end;

{****************************************************}
procedure TALIosMemo.SetPadding(const Value: TBounds);
var aUIEdgeInsets: UIEdgeInsets;
    aPaddingLeft, aPaddingRight: Single;
begin

  //unfortunatly padding bottom is not very well
  //handled under IOS, when you add a new line on a already fully filled memo, the new line
  //start completely at botton without taking care of the padding
  //(you must scroll yourself the memo to be able to see the padding)
  //so i prefer to use instead margins

  FPadding.Assign(Value);

  aPaddingLeft := FPadding.left;
  aPaddingRight := FPadding.right;
  FPadding.left := 0;
  FPadding.right := 0;
  try
    margins := FPadding;
  finally
    FPadding.left := aPaddingleft;
    FPadding.right := aPaddingright;
  end;

  aUIEdgeInsets.top := 0;
  aUIEdgeInsets.left := Value.left;
  aUIEdgeInsets.bottom := 0;
  aUIEdgeInsets.right := Value.right;
  fTextView.View.setTextContainerInset(aUIEdgeInsets);

end;

{**************************************}
function TALIosMemo.GetPadding: TBounds;
begin
  result := FPadding;
end;

{*************************************************}
function TALIosMemo.GetTextSettings: TTextSettings;
begin
  Result := FTextSettings;
end;

{***************************************************************}
procedure TALIosMemo.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{*********************************}
procedure TALIosMemo.DoRootChanged;
begin
  inherited;
  FTextView.RootChanged(Root);
end;

{**************************}
procedure TALIosMemo.Resize;
var aContentOffset: NSPoint;
begin
  inherited;
  FTextView.size := Size.size;
  if compareValue(FTextView.View.contentSize.height, Size.size.cy, Tepsilon.Position) <= 0 then begin
    aContentOffset.x := 0;
    aContentOffset.y := 0;
    FTextView.View.setContentOffset(aContentOffset, false{animated}); // << Scroll to top to avoid wrong contentOffset" artefact when line count changes
  end;
end;

{***************************************}
procedure TALIosMemo.ClipChildrenChanged;
begin
  inherited;
  FTextView.SetClipChildren(ClipChildren);
end;

{*************************************}
procedure TALIosMemo.DoAbsoluteChanged;
begin
  inherited;
  if not (csLoading in ComponentState) then
    FTextView.UpdateFrame;
end;

{**********************************}
procedure TALIosMemo.VisibleChanged;
begin
  inherited;
  FTextView.SetVisible(Visible);
end;

{*******************************}
procedure TALIosMemo.ChangeOrder;
begin
  inherited;
  FTextView.ChangeOrder;
end;

{*********************************}
procedure TALIosMemo.RecalcOpacity;
begin
  inherited;
  FTextView.setAlpha(AbsoluteOpacity);
end;

{*********************************}
procedure TALIosMemo.RecalcEnabled;
begin
  inherited;
  FTextView.SetAbsoluteEnabled(AbsoluteEnabled);
end;

{*********************************}
Procedure TALIosMemo.AddNativeView;
begin
  visible := true;
end;

{************************************}
Procedure TALIosMemo.RemoveNativeView;
begin
  visible := False;
end;

{******************************************************************}
procedure TALIosMemo.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  if FTextView <> nil then FTextView.AncestorVisibleChanged;  // << this proc is called during the ondestroy also when FTextView is already destroyed
end;

{*****************************************}
procedure TALIosMemo.AncestorParentChanged;
begin
  inherited;
  if FTextView <> nil then FTextView.RefreshNativeParent;  // << this proc is called during the ondestroy also when FTextView is already destroyed
end;

{*********************************}
procedure TALIosMemo.ParentChanged;
begin
  inherited;
  if FTextView <> nil then FTextView.RefreshNativeParent; // << this proc is called during the ondestroy also when FTextView is already destroyed
end;

{********************************************************************}
function TALIosMemo.PointInObjectLocal(X: Single; Y: Single): Boolean;
begin
  result := FTextView.PointInObjectLocal(X, Y);
end;

{***************************}
procedure TALIosMemo.DoEnter;
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosMemo.DoEnter', 'control.name: ' + parent.Name +
                              ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}
  inherited DoEnter;
  FTextView.SetFocus;
end;

{**************************}
procedure TALIosMemo.DoExit;
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosMemo.DoExit', 'control.name: ' + parent.Name +
                             ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}
  inherited DoExit;
  FTextView.ResetFocus;
end;

{*******************************}
procedure TALIosMemo.DoEndUpdate;
begin
  inherited;
  if FTextView <> nil then FTextView.UpdateFrame; // << without this, in some case when we are doing beginupdate to the TEdit
                                                  // << (because in android for exemple we would like to not refresh the position of the control during calculation)
                                                  // << then when we do endupdate the control is not paint or lost somewhere
end;

{$endif}
{$ENDREGION}

{$REGION ' WINDOWS / MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}

{***************************************************}
constructor TALStyledMemo.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  fStyledMemo := Nil;
  DisableDisappear := true;
  OnApplyStyleLookup := OnApplyStyleLookupImpl;
  FVertScrollBar := nil;
  FPadding := TBounds.Create(TRectF.Empty);
  FTextSettings := TALMemoTextSettings.Create(Self);
  FTextSettings.OnChanged := OnFontChanged;
  fTextPromptControl := TalText.Create(self);
  fTextPromptControl.Parent := Self;
  fTextPromptControl.Stored := False;
  fTextPromptControl.SetSubComponent(True);
  fTextPromptControl.Locked := True;
  fTextPromptControl.HitTest := False;
  fTextPromptControl.Visible := True;
  fTextPromptControl.Position.Point := TpointF.Create(0,0);
  fTextPromptControl.Width := Width;
  fTextPromptControl.Height := Height;
  fTextPromptControl.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom];
  fTextPromptControl.TextSettings.Assign(FTextSettings);
  fTextPromptControl.color := TalphaColorRec.Null;
  fTextPromptControl.doubleBuffered := True;
  FOnChangeTracking := nil;
  fReturnKeyType := TReturnKeyType.Default;
  inherited onchangeTracking := OnChangeTrackingImpl;
end;

{*******************************}
destructor TALStyledMemo.Destroy;
begin
  ALFreeAndNil(FPadding);
  alFreeAndNil(FTextSettings);
  ALFreeAndNil(fTextPromptControl);
  inherited;
end;

{**}
type
  _TOpenControl = class (TControl);
  _TStyledMemoProtectedAccess = class(TStyledMemo);

{******************************}
procedure TALStyledMemo.DoEnter;
begin
  _TOpenControl(content).FRecalcUpdateRect := True; // << without this the caret is not show when you first click on the Memo
                                                    // << #{\#@#{^ emb team they really don't test anything they do :(
                                                    // << mistake you can found in 1 min it's not normal they are in production
  inherited;
end;

{************************************************************}
procedure TALStyledMemo.OnChangeTrackingImpl(sender: Tobject);
begin
  fTextPromptControl.Visible := text = '';
  if assigned(fOnChangeTracking) and (not (csLoading in componentState)) then
    fOnChangeTracking(self);
end;

{**************************************************************}
procedure TALStyledMemo.OnApplyStyleLookupImpl(sender: Tobject);
Var aScrollBar: TScrollBar;
    I, j, k, l: integer;
begin

  // TALStyledMemo
  //   TStyledMemo
  //      TLayout
  //         TActiveStyleObject
  //            TLayout
  //            TScrollBar
  //            TScrollBar
  //            TLayout
  //               TSmallScrollBar
  //               TSmallScrollBar
  //   TScrollContent
  for I := 0 to controls.Count - 1 do begin
    if (controls[i] is TStyledMemo) then begin // << TStyledMemo
      fStyledMemo := TStyledMemo(controls[i]);
      for j := 0 to controls[i].controls.Count - 1 do begin
        if (controls[i].Controls[j] is TLayout) then begin // << TLayout
          for k := 0 to controls[i].Controls[j].controls.Count - 1 do begin
             if (controls[i].Controls[j].Controls[k] is TActiveStyleObject) then begin // << TActiveStyleObject
              with (controls[i].Controls[j].Controls[k] as TActiveStyleObject) do begin
                ActiveLink.Clear;
                SourceLink.Clear;
              end;
              for l := 0 to controls[i].Controls[j].Controls[k].controls.Count - 1 do begin
                if (controls[i].Controls[j].Controls[k].Controls[l] is TScrollBar) then begin // << TScrollBar
                  aScrollBar := controls[i].Controls[j].Controls[k].Controls[l] as TScrollBar;
                  with aScrollBar do begin
                    if Align = TalignLayout.Right then begin
                      Align := TalignLayout.None;
                      FVertScrollBar := aScrollBar;
                      SetPadding(FPadding);
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  //align the scrollbars
  realignScrollBars;

end;

{****************************************}
procedure TALStyledMemo.realignScrollBars;
begin
  if FVertScrollBar = nil then exit;
  FVertScrollBar.Height := ParentControl.Height;
  FVertScrollBar.position.y := -margins.Top;
  FVertScrollBar.position.x := Width - FVertScrollBar.Width + margins.Right;
end;

{*****************************}
procedure TALStyledMemo.Resize;
var aText: String;
begin
  inherited;
  aText := Lines.Text;   // => need to do this bullsheet because sometime the lines are not corectly aligned and it's the
  Lines.Text := '';      // => only way i found to force this function to run: TStyledMemo.TLines.RenderLayouts;
  Lines.Text := aText;   // => i m lazzy to open a bug report (and to make the bug demo program) for emb, i do not work for them after all ...
  realignScrollBars;
end;

{*******************************************************}
procedure TALStyledMemo.SetPadding(const Value: TBounds);
var aRect: Trectf;
begin
  aRect := Value.Rect;
  if fVertScrollBar <> nil then aRect.Right := aRect.Right + fVertScrollBar.Width;
  margins.Rect := aRect;
  FPadding.Assign(Value);
end;

{*****************************************}
function TALStyledMemo.GetPadding: TBounds;
begin
  result := FPadding;
end;

{**********************************************************************}
// to correct this bug: https://quality.embarcadero.com/browse/RSP-19119
procedure TALStyledMemo.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var aTmpKey: Word;
    aTmpKeyChar: System.WideChar;
begin
  aTmpKey := Key;
  inherited;
  if aTmpKey = vkBack then begin
    aTmpKeyChar := ' ';
    aTmpKey := ord(aTmpKeyChar);
    inherited KeyDown(aTmpKey, aTmpKeyChar, []);
    aTmpKeyChar := #0;
    aTmpKey := vkBack;
    inherited KeyDown(aTmpKey, aTmpKeyChar, []);
  end;
end;

{*******************************************}
function TALStyledMemo.GetTextPrompt: String;
begin
  Result := fTextPromptControl.Text;
end;

{*********************************************************}
procedure TALStyledMemo.setTextPrompt(const Value: String);
begin
  fTextPromptControl.Text := Value;
end;

{*****************************************************}
function TALStyledMemo.GetTextPromptColor: TAlphaColor;
begin
  result := fTextPromptControl.Color;
end;

{*******************************************************************}
procedure TALStyledMemo.setTextPromptColor(const Value: TAlphaColor);
begin
  fTextPromptControl.Color := Value;
  fTextPromptControl.clearBufBitmap;
end;

{******************************************************************}
procedure TALStyledMemo.SetTextSettings(const Value: TTextSettings);
begin
  fTextSettings.Assign(Value);
  inherited TextSettings := Value;
end;

{****************************************************}
function TALStyledMemo.GetTextSettings: TTextSettings;
begin
  Result := fTextSettings;
end;

{*****************************************************}
procedure TALStyledMemo.OnFontChanged(Sender: TObject);
var aPreviousColor: TalphaColor;
begin
  fTextPromptControl.TextSettings.BeginUpdate;
  try
    aPreviousColor := fTextPromptControl.Color;
    fTextPromptControl.TextSettings.Assign(FTextSettings);
    fTextPromptControl.color := aPreviousColor;
  finally
    fTextPromptControl.TextSettings.EndUpdate;
  end;
  inherited TextSettings := fTextSettings;
end;

{*******************************************}
function TALStyledMemo.getLineHeight: Single;
begin
  if fStyledMemo <> nil then result := _TStyledMemoProtectedAccess(fStyledMemo).GetLineHeight
  else result := 0;
end;

{*******************************************}
function TALStyledMemo.getLineCount: integer;
var aLineHeight: Single;
begin
  aLineHeight := getLineHeight;
  if compareValue(aLineHeight, 0, Tepsilon.Position) > 0 then result := round(ContentBounds.Height / aLineHeight)
  else result := 0;
end;

{$endif}
{$ENDREGION}

{*********************************************}
constructor TALMemo.Create(AOwner: TComponent);
begin
  inherited;
  fPadding := TBounds.Create(TrectF.Empty);
  fPadding.OnChange := PaddingChangedHandler;
  fDefStyleAttr := '';
  FAutoTranslate := true;
  FAutoConvertFontFamily := True;
  fOnChangeTracking := nil;
  fOnEnter := nil;
  fOnExit := nil;
  Cursor := crIBeam;
  CanFocus := True;
  CanParentFocus := False; // else you must rewrite the GetCanFocus
  //-----
  fMemoControl := nil;
  {$IF defined(android)}
  //i use this way to know that the compoment
  //will load it's properties from the dfm
  if (aOwner = nil) or
     (not (csloading in aOwner.ComponentState)) then CreateMemoControl; // because we must first know the value of DefStyleAttr to create the FMemoControl
  {$ELSE}
  CreateMemoControl;
  {$ENDIF}
  //-----
  {$IF (not defined(android)) and (not defined(IOS))}
  fLineSpacingMultiplier := 1;
  fLineSpacingExtra := 0;
  {$ENDIF}
  {$IF (not defined(IOS))}
  fTintColor := TalphaColorRec.Null;
  fAutoCapitalizationType := TALAutoCapitalizationType.acNone;
  {$ENDIF}
  //-----
  FTextSettings := TALMemoTextSettings.Create(Self);
  FTextSettings.OnChanged := OnFontChanged;
  //-----
  fill.DefaultColor := $ffffffff;
  fill.Color := $ffffffff;
  stroke.OnChanged := Nil;
  stroke.DefaultKind := TBrushKind.none;
  stroke.kind := TBrushKind.none;
  stroke.OnChanged := StrokeChanged;
end;

{*************************}
destructor TALMemo.Destroy;
begin
  ALFreeAndNil(fPadding);
  ALFreeAndNil(FTextSettings);
  ALFreeAndNil(fMemoControl, false{adelayed}, false{aRefCountWarn}); // << will call disposeOF under ARC so it's ok
  inherited;
end;

{**********************************}
procedure TALMemo.CreateMemoControl;
begin
  if fMemoControl <> nil then exit;
  {$IF defined(android)}
  FMemoControl := TALAndroidEdit.Create(self, true, fDefStyleAttr);
  FMemoControl.Parent := self;
  FMemoControl.Stored := False;
  FMemoControl.SetSubComponent(True);
  FMemoControl.Locked := True;
  FMemoControl.ReturnKeyType := tReturnKeyType.Default;  // noops operation
  FMemoControl.OnReturnKey := nil; // noops operation
  {$ELSEIF defined(ios)}
  FMemoControl := TALIosMemo.Create(self);
  FMemoControl.Parent := self;
  FMemoControl.Stored := False;
  FMemoControl.SetSubComponent(True);
  FMemoControl.Locked := True;
  FMemoControl.ReturnKeyType := tReturnKeyType.Default;  // noops operation
  FMemoControl.AutoCapitalizationType := TALAutoCapitalizationType.acNone; // noops operation
  {$ELSE}
  fMemoControl := TALStyledMemo.Create(self);
  fMemoControl.Parent := self;
  FMemoControl.Stored := False;
  FMemoControl.SetSubComponent(True);
  FMemoControl.Locked := True;
  FMemoControl.ControlType := TcontrolType.Styled; // << on windows platform is not good as Styled
  FMemoControl.StyledSettings := []; // Family, Size, Style, FontColor, Other
  FMemoControl.Bounces := TBehaviorBoolean.False;
  FMemoControl.AutoHide := TBehaviorBoolean.False;
  {$ENDIF}
  FMemoControl.Align := TAlignLayout.Client;
  FMemoControl.OnChangeTracking := OnChangeTrackingImpl;
  FMemoControl.OnEnter := OnEnterImpl;
  FMemoControl.OnExit := OnExitImpl;
  FMemoControl.ReturnKeyType := tReturnKeyType.Default;  // noops operation
  FMemoControl.KeyboardType := TVirtualKeyboardType.Default; // noops operation
  FMemoControl.CheckSpelling := True;
  FMemoControl.MaxLength := 0; // noops operation
end;

{***********************}
procedure TALMemo.Loaded;
begin
  if FMemoControl = nil then CreateMemoControl;
  //-----
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
  StrokeChanged(stroke);
  OnFontChanged(nil);
  PaddingChangedHandler(fPadding);
end;

{*****************************************************}
procedure TALMemo.SetDefStyleAttr(const Value: String);
begin
  if Value <> fDefStyleAttr then begin
    fDefStyleAttr := Value;
    {$IFDEF ANDROID}
    ALFreeAndNil(FMemoControl, false{adelayed}, false{aRefCountWarn}); // << will call disposeOF under ARC so it's ok
    CreateMemoControl;
    {$ENDIF}
  end;
end;

{**************************************}
function TALMemo.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(100, 88);
end;

{********************}
{$IF defined(android)}
function TALMemo.GetAndroidEditText: TALAndroidEditText;
begin
  if FMemoControl = nil then CreateMemoControl;
  result := FMemoControl.EditText;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALMemo.GetIosTextView: TALIosTextView;
begin
  if FMemoControl = nil then CreateMemoControl;
  result := FMemoControl.TextView;
end;
{$ENDIF}

{**********************************************}
function TALMemo.GetTextSettings: TTextSettings;
begin
  Result := FTextSettings;
end;

{************************************************************}
procedure TALMemo.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{***********************************************}
procedure TALMemo.OnFontChanged(Sender: TObject);
begin
  if csLoading in componentState then exit;
  if FMemoControl = nil then CreateMemoControl;
  FMemoControl.TextSettings.BeginUpdate;
  try
    FMemoControl.TextSettings.IsChanged := True;
    FMemoControl.TextSettings.Assign(ftextsettings);
  finally
    FMemoControl.TextSettings.EndUpdate;
  end;
end;

{*********************************************}
procedure TALMemo.SetText(const Value: String);
begin
  if FMemoControl = nil then CreateMemoControl;
  FMemoControl.Text := Value;
end;

{*******************************}
function TALMemo.getText: String;
begin
  if FMemoControl = nil then CreateMemoControl;
  result := FMemoControl.Text;
end;

{*************************************}
function TALMemo.GetTextPrompt: String;
begin
  if FMemoControl = nil then CreateMemoControl;
  result := FMemoControl.TextPrompt;
end;

{***************************************************}
procedure TALMemo.setTextPrompt(const Value: String);
begin
  if FMemoControl = nil then CreateMemoControl;
  FMemoControl.TextPrompt := Value;
end;

{***********************************************}
function TALMemo.GetTextPromptColor: TAlphaColor;
begin
  if FMemoControl = nil then CreateMemoControl;
  result := FMemoControl.TextPromptColor;
end;

{*************************************************************}
procedure TALMemo.setTextPromptColor(const Value: TAlphaColor);
begin
  if FMemoControl = nil then CreateMemoControl;
  FMemoControl.TextPromptColor := Value;
end;

{*****************************************}
function TALMemo.GetTintColor: TAlphaColor;
begin
  if FMemoControl = nil then CreateMemoControl;
  {$IF defined(ios)}
  result := FMemoControl.TintColor;
  {$ELSE}
  result := fTintColor;
  {$ENDIF}
end;

{*******************************************************}
procedure TALMemo.setTintColor(const Value: TAlphaColor);
begin
  if FMemoControl = nil then CreateMemoControl;
  {$IF defined(ios)}
  FMemoControl.TintColor := Value;
  {$ELSE}
  fTintColor := Value;
  {$ENDIF}
end;

{************************************************}
function TALMemo.GetLineSpacingMultiplier: single;
begin
   if FMemoControl = nil then CreateMemoControl;
  {$IF defined(ios) OR defined(android)}
  result := FMemoControl.LineSpacingMultiplier;
  {$ELSE}
  result := fLineSpacingMultiplier;
  {$ENDIF}
end;

{**************************************************************}
procedure TALMemo.SetLineSpacingMultiplier(const Value: single);
begin
  if FMemoControl = nil then CreateMemoControl;
  {$IF defined(ios) OR defined(android)}
  FMemoControl.LineSpacingMultiplier := Value;
  {$ELSE}
  fLineSpacingMultiplier := Value;
  {$ENDIF}
end;

{****************************************************}
function TALMemo.LineSpacingMultiplierStored: boolean;
begin
  {$IF defined(ios) OR defined(android)}
  result := True; // << we don't care in fact
  {$ELSE}
  result := not SameValue(fLineSpacingMultiplier, 1, Tepsilon.FontSize);
  {$ENDIF}
end;

{*******************************************}
function TALMemo.GetLineSpacingExtra: single;
begin
   if FMemoControl = nil then CreateMemoControl;
  {$IF defined(ios) OR defined(android)}
  result := FMemoControl.LineSpacingExtra;
  {$ELSE}
  result := fLineSpacingExtra;
  {$ENDIF}
end;

{*********************************************************}
procedure TALMemo.SetLineSpacingExtra(const Value: single);
begin
  if FMemoControl = nil then CreateMemoControl;
  {$IF defined(ios) OR defined(android)}
  FMemoControl.LineSpacingExtra := Value;
  {$ELSE}
  fLineSpacingExtra := Value;
  {$ENDIF}
end;

{***********************************************}
function TALMemo.LineSpacingExtraStored: boolean;
begin
  {$IF defined(ios) OR defined(android)}
  result := True; // << we don't care in fact
  {$ELSE}
  result := not SameValue(fLineSpacingExtra, 0, Tepsilon.FontSize);
  {$ENDIF}
end;

{*************************************************************}
procedure TALMemo.SetKeyboardType(Value: TVirtualKeyboardType);
begin
  if FMemoControl = nil then CreateMemoControl;
  FMemoControl.KeyboardType := Value;
end;

{*****************************************************}
function TALMemo.GetKeyboardType: TVirtualKeyboardType;
begin
  if FMemoControl = nil then CreateMemoControl;
  result := FMemoControl.KeyboardType;
end;

{********************************************************************}
function TALMemo.GetAutoCapitalizationType: TALAutoCapitalizationType;
begin
  if FMemoControl = nil then CreateMemoControl;
  {$IF defined(ios)}
  result := FMemoControl.AutoCapitalizationType;
  {$ELSE}
  result := fAutoCapitalizationType;
  {$ENDIF}
end;

{**********************************************************************************}
procedure TALMemo.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  if FMemoControl = nil then CreateMemoControl;
  {$IF defined(ios)}
  FMemoControl.AutoCapitalizationType := Value;
  {$ELSE}
  fAutoCapitalizationType := Value;
  {$ENDIF}
end;

{*******************************************************}
procedure TALMemo.SetCheckSpelling(const Value: Boolean);
begin
  if FMemoControl = nil then CreateMemoControl;
  FMemoControl.CheckSpelling := Value;
end;

{*****************************************}
function TALMemo.GetCheckSpelling: Boolean;
begin
  if FMemoControl = nil then CreateMemoControl;
  result := FMemoControl.CheckSpelling;
end;

{**************************************************************}
procedure TALMemo.SetReturnKeyType(const Value: TReturnKeyType);
begin
  if FMemoControl = nil then CreateMemoControl;
  FMemoControl.ReturnKeyType := Value;
end;

{************************************************}
function TALMemo.GetReturnKeyType: TReturnKeyType;
begin
  if FMemoControl = nil then CreateMemoControl;
  result := FMemoControl.ReturnKeyType;
end;

{***************************************************}
procedure TALMemo.SetMaxLength(const Value: integer);
begin
  if FMemoControl = nil then CreateMemoControl;
  FMemoControl.MaxLength := Value;
end;

{*************************************}
function TALMemo.GetMaxLength: integer;
begin
  if FMemoControl = nil then CreateMemoControl;
  result := FMemoControl.MaxLength;
end;

{*******************************************************}
procedure TALMemo.PaddingChangedHandler(Sender: TObject);
begin
  if (csLoading in componentState) then exit;
  if FMemoControl = nil then CreateMemoControl;
  FMemoControl.Padding := padding;
end;

{*************************************************}
procedure TALMemo.SetPadding(const Value: TBounds);
begin
  fPadding.Assign(Value);
end;

{***********************************}
function TALMemo.GetPadding: TBounds;
begin
  result := fPadding;
end;

{******************************************************}
procedure TALMemo.OnChangeTrackingImpl(Sender: TObject);
begin
  if assigned(fOnChangeTracking) and (not (csLoading in componentState)) then
    fOnChangeTracking(self); // << yes need to send self instead of the fEditControl
end;

{*********************************************}
procedure TALMemo.OnEnterImpl(Sender: TObject);
begin
  if assigned(fOnEnter) and (not (csLoading in componentState)) then
    fOnEnter(self); // << yes need to send self instead of the fEditControl
end;

{********************************************}
procedure TALMemo.OnExitImpl(Sender: TObject);
begin
  if assigned(fOnExit) and (not (csLoading in componentState)) then
    fOnExit(self); // << yes need to send self instead of the fEditControl
end;

{***********************************************}
procedure TALMemo.StrokeChanged(Sender: TObject);
var aRect: TrectF;
begin
  inherited StrokeChanged(Sender);
  if csLoading in componentState then exit;
  if FMemoControl = nil then CreateMemoControl;
  if Stroke.Kind = TbrushKind.None then FMemoControl.Margins.Rect := TrectF.Create(0,0,0,0)
  else begin
    aRect := TrectF.Create(0,0,0,0);
    if (TSide.Top in Sides) then aRect.Top := Stroke.Thickness;
    if (TSide.bottom in Sides) then aRect.bottom := Stroke.Thickness;
    if (TSide.right in Sides) then aRect.right := Stroke.Thickness;
    if (TSide.left in Sides) then aRect.left := Stroke.Thickness;
    FMemoControl.Margins.Rect := arect;
  end;
end;

{**********************************************}
procedure TALMemo.SetSides(const Value: TSides);
begin
  inherited SetSides(Value);
  StrokeChanged(nil);
end;

{************************************}
function TALMemo.GetCanFocus: Boolean;
begin
  {$IF defined(DEBUG)}
  ALLog('TALEdit.GetCanFocus', 'name: ' + Name, TalLogType.VERBOSE);
  {$ENDIF}
  if FMemoControl = nil then CreateMemoControl;
  result := inherited GetCanFocus;
  if result then begin
    {$IF defined(IOS)}
    FMemoControl.FTextView.SetFocus; // << instead of fEditControl.SetFocus because when I do setFocus
                                     // << of another TalEdit when one is already currently focused then the event
                                     // << formkeyboadHidden and formKeyboardShow will be raised (and it's slow)
    {$ELSE}
    FMemoControl.SetFocus;
    {$ENDIF}
    exit(false);   // << the canparentfocus is also set to false, so the TCommonCustomForm.NewFocusedControl(const Value: IControl)
                   //    will do nothing !
  end;
end;

{****************************************}
function TALMemo.GetContainFocus: Boolean;
begin
  if FMemoControl = nil then CreateMemoControl;
  result := isFocused or FMemoControl.IsFocused;
end;

{******************************}
Procedure TALMemo.AddNativeView;
begin
  if FMemoControl = nil then CreateMemoControl;
  {$IF defined(android) or defined(ios)}
  FMemoControl.AddNativeView;
  {$ENDIF}
end;

{*********************************}
Procedure TALMemo.RemoveNativeView;
begin
  if FMemoControl = nil then CreateMemoControl;
  {$IF defined(android) or defined(ios)}
  FMemoControl.RemoveNativeView;
  {$ENDIF}
end;

{**************************************************************************}
Procedure TALMemo.setSelection(const aStart: integer; const aStop: Integer);
begin
  if FMemoControl = nil then CreateMemoControl;
  {$IF defined(MSWINDOWS) or defined(_MACOS)}
  FMemoControl.SelStart := aStart;
  FMemoControl.SelLength := aStop - aStart;
  {$ELSEIF defined(android)}
  FMemoControl.setSelection(aStart, aStop);
  {$ENDIF}
end;

{****************************************************}
Procedure TALMemo.setSelection(const aindex: integer);
begin
  if FMemoControl = nil then CreateMemoControl;
  {$IF defined(MSWINDOWS) or defined(_MACOS)}
  FMemoControl.SelStart := aindex;
  {$ELSEIF defined(android)}
  FMemoControl.setSelection(aindex);
  {$ENDIF}
end;

{*************************************}
function TALMemo.getLineHeight: single;
begin
  result := FMemoControl.getLineHeight;
end;

{*************************************}
function TALMemo.getLineCount: Integer;
begin
  result := FMemoControl.getLineCount;
end;

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALMemo]);
end;

initialization
  RegisterFmxClasses([TALMemo]);

end.
