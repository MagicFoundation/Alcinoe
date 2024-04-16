unit Alcinoe.FMX.Edit;

interface

{$I Alcinoe.inc}

uses
  System.Types,
  system.Classes,
  System.UITypes,
  {$IF defined(android)}
  System.Messaging,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.Widget,
  Androidapi.JNI.JavaTypes,
  Alcinoe.AndroidApi.Common,
  Alcinoe.FMX.NativeView.Android,
  {$ELSEIF defined(IOS)}
  System.TypInfo,
  iOSapi.Foundation,
  iOSapi.UIKit,
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  Alcinoe.FMX.NativeView.iOS,
  {$ELSEIF defined(ALMacOS)}
  System.TypInfo,
  Macapi.Foundation,
  Macapi.AppKit,
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  Macapi.CocoaTypes,
  Alcinoe.FMX.NativeView.Mac,
  {$ELSEIF defined(MSWINDOWS)}
  Winapi.Messages,
  Winapi.Windows,
  FMX.Controls.Win,
  FMX.Helpers.Win,
  Alcinoe.FMX.NativeView.Win,
  {$ELSE}
  FMX.Edit,
  {$ENDIF}
  FMX.types,
  Fmx.controls,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.Objects;

Type

  {**********************************************}
  TALEditTextSettings = class(TALBaseTextSettings)
  published
    property Font;
    property HorzAlign;
    property VertAlign;
  end;

  {***************************}
  TALAutoCapitalizationType = (
    acNone, // Specifies that there is no automatic text capitalization.
    acWords, // Specifies automatic capitalization of the first letter of each word.
    acSentences, // Specifies automatic capitalization of the first letter of each sentence.
    acAllCharacters); // Specifies automatic capitalization of all characters, such as for entry of two-character state abbreviations for the United States.

{$REGION ' ANDROID'}
{$IF defined(android)}
type

  {*********************}
  TALAndroidEdit = class;

  {**********************************************}
  TALAndroidEditText = class(TALAndroidNativeView)
  private

    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALTextWatcher = class(TJavaLocal, JTextWatcher)
      private
        FEditText: TALAndroidEditText;
      public
        constructor Create(const aEditText: TALAndroidEditText);
        procedure afterTextChanged(s: JEditable); cdecl;
        procedure beforeTextChanged(s: JCharSequence; start: Integer; count: Integer; after: Integer); cdecl;
        procedure onTextChanged(s: JCharSequence; start: Integer; before: Integer; count: Integer); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALEditorActionListener = class(TJavaLocal, JTextView_OnEditorActionListener)
      private
        fIsMultiLineEditText: Boolean;
        FEditText: TALAndroidEditText;
      public
        constructor Create(const aEditText: TALAndroidEditText; const aIsMultiLineEditText: Boolean = false);
        function onEditorAction(v: JTextView; actionId: Integer; event: JKeyEvent): Boolean; cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALKeyPreImeListener = class(TJavaLocal, JALKeyPreImeListener)
      private
        FEditText: TALAndroidEditText;
      public
        constructor Create(const aEditText: TALAndroidEditText);
        function onKeyPreIme(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
      end;

  private
    fIsMultiline: boolean;
    fDefStyleAttr: String;
    fDefStyleRes: String;
    FTextWatcher: TALTextWatcher;
    FEditorActionListener: TALEditorActionListener;
    FKeyPreImeListener: TALKeyPreImeListener;
    FEditControl: TALAndroidEdit;
    function GetView: JALEditText;
  protected
    function CreateView: JView; override;
    procedure InitView; override;
  public
    constructor Create(const AControl: TalAndroidEdit; Const aIsMultiline: Boolean = False; const aDefStyleAttr: String = ''; const aDefStyleRes: String = ''); reintroduce;
    destructor Destroy; override;
    property View: JALEditText read GetView;
  end;

  {****************************************************************}
  // the design of the androidText can be done in the res/styles.xml
  // please see the example of the Demos\ALFmxControls.dproj
  TALAndroidEdit = class(TControl, IControlTypeSupportable)
  private
    FPadding: TBounds;
    fOnChangeTracking: TNotifyEvent;
    fOnReturnKey: TNotifyEvent;
    FFillColor: TAlphaColor;
    FTextSettings: TALEditTextSettings;
    fMaxLength: integer;
    fApplicationEventMessageID: integer;
    fReturnKeyType: TReturnKeyType;
    fKeyboardType: TVirtualKeyboardType;
    fAutoCapitalizationType: TALAutoCapitalizationType;
    fPassword: boolean;
    fCheckSpelling: boolean;
    fIsMultiline: Boolean;
    fTintColor: TalphaColor;
    FEditText: TALAndroidEditText;
    procedure DoSetInputType(
                const aKeyboardType: TVirtualKeyboardType;
                const aAutoCapitalizationType: TALAutoCapitalizationType;
                const aPassword: Boolean;
                const aCheckSpelling: Boolean;
                const aIsMultiline: Boolean);
    procedure setKeyboardType(const Value: TVirtualKeyboardType);
    procedure setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
    procedure setPassword(const Value: Boolean);
    procedure setCheckSpelling(const Value: Boolean);
    procedure DoSetReturnKeyType(const aReturnKeyType: TReturnKeyType);
    procedure setReturnKeyType(const Value: TReturnKeyType);
    function GetTextPrompt: String;
    procedure setTextPrompt(const Value: String);
    function GetTextPromptColor: TAlphaColor;
    procedure setTextPromptColor(const Value: TAlphaColor);
    procedure SetTextSettings(const Value: TALEditTextSettings);
    procedure TextSettingsChanged(Sender: TObject);
    function getText: String;
    procedure SetText(const Value: String);
    function GetLineSpacingMultiplier: single;
    procedure SetLineSpacingMultiplier(const Value: single);
    function GetLineSpacingExtra: single;
    procedure SetLineSpacingExtra(const Value: single);
    procedure SetPadding(const Value: TBounds);
    procedure SetMaxLength(const Value: integer);
    procedure ApplicationEventHandler(const Sender: TObject; const M : TMessage);
    { IControlTypeSupportable }
    function GetControlType: TControlType;
    procedure SetControlType(const Value: TControlType);
  protected
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure AncestorParentChanged; override;
    procedure ParentChanged; override;
    procedure DoAbsoluteChanged; override;
    procedure DoRootChanged; override;
    procedure Resize; override;
    procedure VisibleChanged; override;
    procedure ChangeOrder; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoEndUpdate; override;
  public
    constructor Create(const AOwner: TComponent; Const aIsMultiline: Boolean = False; const aDefStyleAttr: String = ''; const aDefStyleRes: String = ''); reintroduce; virtual;
    destructor Destroy; override;
    procedure RecalcOpacity; override;
    procedure RecalcEnabled; override;
    function HasNativeView: boolean;
    Procedure AddNativeView;
    Procedure RemoveNativeView;
    function getLineHeight: Single; // It's include the line spacing
    function getLineCount: integer;
    property OnChangeTracking: TNotifyEvent read fOnChangeTracking write fOnChangeTracking;
    property OnReturnKey: TNotifyEvent read fOnReturnKey write fOnReturnKey;
    property ReturnKeyType: TReturnKeyType read FReturnKeyType write SetReturnKeyType;
    property KeyboardType: TVirtualKeyboardType read FKeyboardType write SetKeyboardType;
    property AutoCapitalizationType: TALAutoCapitalizationType read FAutoCapitalizationType write SetAutoCapitalizationType;
    property Password: Boolean read FPassword write SetPassword;
    property TextPrompt: String read GetTextPrompt write setTextPrompt;
    property TextPromptColor: TAlphaColor read GetTextPromptColor write setTextPromptColor; // Null mean use the default color
    property TintColor: TalphaColor read fTintColor write fTintColor; // Not used
    property FillColor: TAlphaColor read FFillColor write FFillColor; // Not used
    property MaxLength: integer read FMaxLength write SetMaxLength;
    property Text: String read getText write SetText;
    property TextSettings: TALEditTextSettings read FTextSettings write SetTextSettings;
    property CheckSpelling: Boolean read FCheckSpelling write SetCheckSpelling;
    property EditText: TALAndroidEditText read FEditText;
    property LineSpacingMultiplier: single read GetLineSpacingMultiplier write SetLineSpacingMultiplier; // Each line will have its height multiplied by LineSpacingMultiplier
    property LineSpacingExtra: single read GetLineSpacingExtra write SetLineSpacingExtra; // Each line will have its height added by LineSpacingExtra
    property Padding: TBounds read FPadding write SetPadding;
  end;

{$endif}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}
type

  {*************************************}
  IALUITextField = interface(UITextField)
    ['{E4E67240-F15A-444F-8CE1-A3A830C023E8}']
    procedure ControlEventEditingChanged; cdecl;
    procedure ControlEventEditingDidEnd; cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
  end;

  {******************************}
  TALIosTextFieldDelegate = class;
  TALIosEdit = class;

  {***************************************}
  TALIosTextField = class(TALIosNativeView)
  private
    FTextFieldDelegate: TALIosTextFieldDelegate;
    FEditControl: TALIosEdit;
    function GetView: UITextField;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create; overload; override;
    constructor Create(const AControl: TControl); overload; override;
    destructor Destroy; override;
    procedure ControlEventEditingChanged; cdecl;
    procedure ControlEventEditingDidEnd; cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
    property View: UITextField read GetView;
  end;

  {************************************************************}
  TALIosTextFieldDelegate = class(TOCLocal, UITextFieldDelegate)
  private
    FTextField: TALIosTextField;
  public
    constructor Create(const ATextField: TALIosTextField);
    // Better name would be textFieldShouldChangeCharactersInRange
    function textField(textField: UITextField; shouldChangeCharactersInRange: NSRange; replacementString: NSString): Boolean; cdecl;
    procedure textFieldDidBeginEditing(textField: UITextField); cdecl;
    procedure textFieldDidEndEditing(textField: UITextField); cdecl;
    function textFieldShouldBeginEditing(textField: UITextField): Boolean; cdecl;
    function textFieldShouldClear(textField: UITextField): Boolean; cdecl;
    function textFieldShouldEndEditing(textField: UITextField): Boolean; cdecl;
    function textFieldShouldReturn(textField: UITextField): Boolean; cdecl;
  end;

  {***************************************************}
  TALIosEdit = class(TControl, IControlTypeSupportable)
  private
    fOnChangeTracking: TNotifyEvent;
    fOnReturnKey: TNotifyEvent;
    FFillColor: TAlphaColor;
    FTextSettings: TALEditTextSettings;
    fMaxLength: integer;
    fTextPromptColor: TalphaColor;
    FTextField: TALIosTextField;
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
    procedure setTextPromptColor(const Value: TAlphaColor);
    procedure applyTextPromptWithColor(const aStr: String; const aColor: TAlphaColor);
    function GetTintColor: TAlphaColor;
    procedure setTintColor(const Value: TAlphaColor);
    procedure SetTextSettings(const Value: TALEditTextSettings);
    procedure TextSettingsChanged(Sender: TObject);
    function getText: String;
    procedure SetText(const Value: String);
    { IControlTypeSupportable }
    function GetControlType: TControlType;
    procedure SetControlType(const Value: TControlType);
  protected
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure AncestorParentChanged; override;
    procedure ParentChanged; override;
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
    procedure RecalcOpacity; override;
    procedure RecalcEnabled; override;
    function HasNativeView: boolean;
    Procedure AddNativeView;
    Procedure RemoveNativeView;
    property OnChangeTracking: TNotifyEvent read fOnChangeTracking write fOnChangeTracking;
    property OnReturnKey: TNotifyEvent read fOnReturnKey write fOnReturnKey;
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType;
    property AutoCapitalizationType: TALAutoCapitalizationType read GetAutoCapitalizationType write SetAutoCapitalizationType;
    property Password: Boolean read GetPassword write SetPassword;
    property TextPrompt: String read GetTextPrompt write setTextPrompt;
    property TextPromptColor: TAlphaColor read FTextPromptColor write setTextPromptColor; // Null mean use the default color
    property TintColor: TAlphaColor read GetTintColor write setTintColor; // Null mean use the default color
    property FillColor: TAlphaColor read FFillColor write FFillColor; // Not used
    property MaxLength: integer read fMaxLength write fMaxLength;
    property Text: String read getText write SetText;
    property TextSettings: TALEditTextSettings read FTextSettings write SetTextSettings;
    property CheckSpelling: Boolean read GetCheckSpelling write SetCheckSpelling;
    property TextField: TALIosTextField read FTextField;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}
type

  {*************************************}
  IALNSTextField = interface(NSTextField)
    ['{E4E67240-F15A-444F-8CE1-A3A830C023E8}']
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
  end;

  {******************************}
  TALMacTextFieldDelegate = class;
  TALMacEdit = class;

  {***************************************}
  TALMacTextField = class(TALMacNativeView)
  private
    FTextFieldDelegate: TALMacTextFieldDelegate;
    FEditControl: TALMacEdit;
    function GetView: NSTextField;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create; overload; override;
    constructor Create(const AControl: TControl); overload; override;
    destructor Destroy; override;
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
    property View: NSTextField read GetView;
  end;

  {*********************************************************************}
  TALMacTextFieldDelegate = class(TOCLocal, NSControlTextEditingDelegate)
  private
    FTextField: TALMacTextField;
  public
    constructor Create(const ATextField: TALMacTextField);
    procedure controlTextDidBeginEditing(obj: NSNotification); cdecl;
    procedure controlTextDidEndEditing(obj: NSNotification); cdecl;
    procedure controlTextDidChange(obj: NSNotification); cdecl;
    [MethodName('control:textShouldBeginEditing:')]
    function controlTextShouldBeginEditing(control: NSControl; textShouldBeginEditing: NSText): Boolean; cdecl;
    [MethodName('control:textShouldEndEditing:')]
    function controlTextShouldEndEditing(control: NSControl; textShouldEndEditing: NSText): Boolean; cdecl;
    [MethodName('control:textView:doCommandBySelector:')]
    function controlTextViewDoCommandBySelector(control: NSControl; textView: NSTextView; doCommandBySelector: SEL): Boolean; cdecl;
  end;

  {***************************************************}
  TALMacEdit = class(TControl, IControlTypeSupportable)
  private
    fOnChangeTracking: TNotifyEvent;
    fOnReturnKey: TNotifyEvent;
    FFillColor: TAlphaColor;
    FTextSettings: TALEditTextSettings;
    fMaxLength: integer;
    fReturnKeyType: TReturnKeyType;
    fKeyboardType: TVirtualKeyboardType;
    fAutoCapitalizationType: TALAutoCapitalizationType;
    fPassword: boolean;
    fCheckSpelling: boolean;
    fTextPromptColor: TalphaColor;
    fTintColor: TalphaColor;
    FTextField: TALMacTextField;
    function GetTextPrompt: String;
    procedure setTextPrompt(const Value: String);
    procedure setTextPromptColor(const Value: TAlphaColor);
    procedure applyTextPromptWithColor(const aStr: String; const aColor: TAlphaColor);
    procedure SetTextSettings(const Value: TALEditTextSettings);
    procedure TextSettingsChanged(Sender: TObject);
    function getText: String;
    procedure SetText(const Value: String);
    { IControlTypeSupportable }
    function GetControlType: TControlType;
    procedure SetControlType(const Value: TControlType);
  protected
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure AncestorParentChanged; override;
    procedure ParentChanged; override;
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
    procedure RecalcOpacity; override;
    procedure RecalcEnabled; override;
    function HasNativeView: boolean;
    Procedure AddNativeView;
    Procedure RemoveNativeView;
    property OnChangeTracking: TNotifyEvent read fOnChangeTracking write fOnChangeTracking;
    property OnReturnKey: TNotifyEvent read fOnReturnKey write fOnReturnKey;
    property ReturnKeyType: TReturnKeyType read FReturnKeyType write FReturnKeyType; // Not used
    property KeyboardType: TVirtualKeyboardType read FKeyboardType write FKeyboardType; // Not used
    property AutoCapitalizationType: TALAutoCapitalizationType read FAutoCapitalizationType write FAutoCapitalizationType; // Not used
    property Password: Boolean read FPassword write FPassword; // Not used
    property TextPrompt: String read GetTextPrompt write setTextPrompt;
    property TextPromptColor: TAlphaColor read FTextPromptColor write setTextPromptColor; // Null mean use the default color
    property TintColor: TAlphaColor read FTintColor write FTintColor; // Not used
    property FillColor: TAlphaColor read FFillColor write FFillColor; // Not used
    property MaxLength: integer read fMaxLength write fMaxLength;
    property Text: String read getText write SetText;
    property TextSettings: TALEditTextSettings read FTextSettings write SetTextSettings;
    property CheckSpelling: Boolean read FCheckSpelling write FCheckSpelling; // Not used
    property TextField: TALMacTextField read FTextField;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}
type

  {*****************}
  TALWinEdit = class;

  {**************************************}
  TALWinEditView = class(TALWinNativeView)
  private
    FFontHandle: HFONT;
    FBackgroundBrush: HBRUSH;
    FEditControl: TALWinEdit;
    procedure UpdateFontHandle;
    procedure UpdateBackgroundBrush;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMChar(var Message: TWMKey); message WM_CHAR;
    procedure WMTextColor(var Message: WinApi.Messages.TMessage); message CN_CTLCOLOREDIT;
    procedure WMPaint(var Message: WinApi.Messages.TMessage); message WM_PAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(const AControl: TControl); override;
    destructor Destroy; override;
  end;

  {***************************************************}
  TALWinEdit = class(TControl, IControlTypeSupportable)
  private
    fOnChangeTracking: TNotifyEvent;
    fOnReturnKey: TNotifyEvent;
    FFillColor: TAlphaColor;
    FTextSettings: TALEditTextSettings;
    FTextPrompt: String;
    FTextPromptColor: TalphaColor;
    fReturnKeyType: TReturnKeyType;
    fKeyboardType: TVirtualKeyboardType;
    fAutoCapitalizationType: TALAutoCapitalizationType;
    fCheckSpelling: boolean;
    fTintColor: TalphaColor;
    FEditView: TALWinEditView;
    procedure setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
    procedure setPassword(const Value: Boolean);
    function GetPassword: Boolean;
    procedure setTextPrompt(const Value: String);
    procedure setTextPromptColor(const Value: TAlphaColor);
    procedure SetTextSettings(const Value: TALEditTextSettings);
    procedure TextSettingsChanged(Sender: TObject);
    function getText: String;
    procedure SetText(const Value: String);
    procedure SetFillColor(const Value: TAlphaColor);
    procedure SetMaxLength(const Value: integer);
    function GetMaxLength: integer;
    { IControlTypeSupportable }
    function GetControlType: TControlType;
    procedure SetControlType(const Value: TControlType);
  protected
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure AncestorParentChanged; override;
    procedure ParentChanged; override;
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
    procedure RecalcOpacity; override;
    procedure RecalcEnabled; override;
    function HasNativeView: boolean;
    Procedure AddNativeView;
    Procedure RemoveNativeView;
    property OnChangeTracking: TNotifyEvent read fOnChangeTracking write fOnChangeTracking;
    property OnReturnKey: TNotifyEvent read fOnReturnKey write fOnReturnKey;
    property ReturnKeyType: TReturnKeyType read FReturnKeyType write FReturnKeyType; // Not used
    property KeyboardType: TVirtualKeyboardType read FKeyboardType write FKeyboardType; // Not used
    property AutoCapitalizationType: TALAutoCapitalizationType read FAutoCapitalizationType write SetAutoCapitalizationType;
    property Password: Boolean read GetPassword write SetPassword;
    property TextPrompt: String read FTextPrompt write setTextPrompt;
    property TextPromptColor: TAlphaColor read FTextPromptColor write setTextPromptColor; // Null mean use the default color
    property TintColor: TalphaColor read fTintColor write fTintColor; // Not used
    property FillColor: TAlphaColor read FFillColor write SetFillColor;
    property MaxLength: integer read GetMaxLength write SetMaxLength;
    property Text: String read getText write SetText;
    property TextSettings: TALEditTextSettings read FTextSettings write SetTextSettings;
    property CheckSpelling: Boolean read FCheckSpelling write FCheckSpelling; // Not used
    property EditView: TALWinEditView read FEditView;
  end;

{$endif}
{$ENDREGION}

type

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALEdit = class(TALRectangle)
  private
    fDefStyleAttr: String;
    fDefStyleRes: String;
    FAutoTranslate: Boolean;
    fOnChangeTracking: TNotifyEvent;
    fOnReturnKey: TNotifyEvent;
    fOnEnter: TNotifyEvent;
    fOnExit: TNotifyEvent;
    FTextSettings: TALEditTextSettings;
    {$IF defined(android)}
    fEditControl: TALAndroidEdit;
    function GetEditControl: TALAndroidEdit;
    function GetAndroidEditText: TALAndroidEditText;
    property EditControl: TALAndroidEdit read GetEditControl;
    {$ELSEIF defined(IOS)}
    fEditControl: TALIosEdit;
    function GetEditControl: TALIosEdit;
    function GetIosTextField: TALIosTextField;
    property EditControl: TALIosEdit read GetEditControl;
    {$ELSEIF defined(ALMacOS)}
    fEditControl: TALMacEdit;
    function GetEditControl: TALMacEdit;
    function GetMacTextField: TALMacTextField;
    property EditControl: TALMacEdit read GetEditControl;
    {$ELSEIF defined(MSWindows)}
    fEditControl: TALWinEdit;
    function GetEditControl: TALWinEdit;
    function GetWinEditView: TALWinEditView;
    property EditControl: TALWinEdit read GetEditControl;
    {$ENDIF}
    function GetTextPrompt: String;
    procedure setTextPrompt(const Value: String);
    function GetTextPromptColor: TAlphaColor;
    procedure setTextPromptColor(const Value: TAlphaColor);
    function GetTintColor: TAlphaColor;
    procedure setTintColor(const Value: TAlphaColor);
    procedure SetTextSettings(const Value: TALEditTextSettings);
    procedure TextSettingsChanged(Sender: TObject);
    function getText: String;
    procedure SetText(const Value: String);
    procedure OnChangeTrackingImpl(Sender: TObject);
    procedure OnReturnKeyImpl(Sender: TObject);
    procedure SetOnReturnKey(const Value: TNotifyEvent);
    procedure OnEnterImpl(Sender: TObject);
    procedure OnExitImpl(Sender: TObject);
    procedure SetKeyboardType(Value: TVirtualKeyboardType);
    function GetKeyboardType: TVirtualKeyboardType;
    procedure setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
    function GetAutoCapitalizationType: TALAutoCapitalizationType;
    procedure SetPassword(const Value: Boolean);
    function GetPassword: Boolean;
    procedure SetCheckSpelling(const Value: Boolean);
    function GetCheckSpelling: Boolean;
    procedure SetReturnKeyType(const Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
    procedure SetDefStyleAttr(const Value: String);
    procedure SetDefStyleRes(const Value: String);
    procedure CreateEditControl;
    function GetContainFocus: Boolean;
    procedure SetMaxLength(const Value: integer);
    function GetMaxLength: integer;
  protected
    function GetDefaultSize: TSizeF; override;
    procedure Loaded; override;
    procedure PaddingChanged; override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure SetSides(const Value: TSides); override;
    function GetCanFocus: Boolean; override;
    procedure FillChanged(Sender: TObject); override;
    procedure DoResized; override;
    procedure AdjustSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF defined(android)}
    property AndroidEditText: TALAndroidEditText read GetAndroidEditText;
    {$ELSEIF defined(IOS)}
    property IosTextField: TALIosTextField read GetIosTextField;
    {$ELSEIF defined(ALMacOS)}
    property MacTextField: TALMacTextField read GetMacTextField;
    {$ELSEIF defined(MSWindows)}
    property WinEditView: TALWinEditView read GetWinEditView;
    {$ENDIF}
    function HasNativeView: boolean;
    Procedure AddNativeView;
    Procedure RemoveNativeView;
    property ContainFocus: Boolean read GetContainFocus;
  published
    // Android only - the name of an attribute in the current theme that contains a reference to
    // a style resource that supplies defaults style values
    // Exemple of use: https://stackoverflow.com/questions/5051753/how-do-i-apply-a-style-programmatically
    // NOTE: !!IMPORTANT!! This properties must be defined the very first because the stream system must load it the very first
    property DefStyleAttr: String read fDefStyleAttr write SetDefStyleAttr;
    // Android only - the name of a style resource that supplies default style values
    // NOTE: !!IMPORTANT!! This properties must be defined the very first because the stream system must load it the very first
    property DefStyleRes: String read fDefStyleRes write SetDefStyleRes;
    property TabOrder;
    property TabStop;
    property Cursor default crIBeam;
    property CanFocus default True;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType default TVirtualKeyboardType.Default;
    property AutoCapitalizationType: TALAutoCapitalizationType read GetAutoCapitalizationType write SetAutoCapitalizationType default TALAutoCapitalizationType.acNone;
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType default TReturnKeyType.Default;
    property Password: Boolean read GetPassword write SetPassword default False;
    //property ReadOnly;
    property MaxLength: integer read GetMaxLength write SetMaxLength default 0;
    //property FilterChar;
    property Text: String read getText write SetText;
    property TextSettings: TALEditTextSettings read FTextSettings write SetTextSettings;
    //property Hint;
    property TextPrompt: String read GetTextPrompt write setTextPrompt;
    property TextPromptColor: TAlphaColor read GetTextPromptColor write setTextPromptColor default TalphaColors.null; // Null mean use the default TextPromptColor
    property TintColor: TAlphaColor read GetTintColor write setTintColor default TalphaColors.null; // IOS only - the color of the cursor caret and the text selection handles. null mean use the default TintColor
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate default true; // Just the TextPrompt
    property TouchTargetExpansion;
    //property Caret;
    //property KillFocusByReturn; => always true
    property CheckSpelling: Boolean read GetCheckSpelling write SetCheckSpelling default true;
    //property ParentShowHint;
    //property ShowHint;
    //property OnChange;
    property OnChangeTracking: TNotifyEvent read fOnChangeTracking write fOnChangeTracking;
    property OnReturnKey: TNotifyEvent read fOnReturnKey write SetOnReturnKey;
    //property OnTyping;
    //property OnValidating;
    //property OnValidate;
    //property OnKeyDown; // Not work under android - it's like this with their @{[^# virtual keyboard :(
    //property OnKeyUp; // Not work under android - it's like this with their @{[^# virtual keyboard :(
    property OnEnter: TnotifyEvent Read fOnEnter Write fOnEnter;
    property OnExit: TnotifyEvent Read fOnExit Write fOnExit;
  end;

procedure Register;

implementation

uses
  System.SysUtils,
  System.Math,
  Fmx.Graphics,
  {$IF defined(android)}
  Androidapi.Helpers,
  Androidapi.Input,
  Androidapi.KeyCodes,
  Androidapi.JNI.App,
  Androidapi.JNI.Util,
  Androidapi.JNI.Os,
  FMX.VirtualKeyboard,
  FMX.Platform,
  FMX.Platform.Android,
  FMX.Helpers.Android,
  FMX.Forms,
  {$ELSEIF defined(IOS)}
  Macapi.CoreFoundation,
  iOSapi.CocoaTypes,
  Macapi.Helpers,
  iOSapi.CoreText,
  FMX.Helpers.iOS,
  FMX.Consts,
  {$ELSEIF defined(ALMacOS)}
  Macapi.CoreFoundation,
  Macapi.Helpers,
  Macapi.CoreText,
  FMX.Helpers.Mac,
  FMX.Consts,
  Alcinoe.StringUtils,
  {$ELSEIF defined(MSWINDOWS)}
  Winapi.CommCtrl,
  Alcinoe.FMX.Graphics,
  {$endif}
  Alcinoe.Common;

{$REGION ' ANDROID'}
{$IF defined(android)}

{**********************************************************************************************}
constructor TALAndroidEditText.TALKeyPreImeListener.Create(const aEditText: TALAndroidEditText);
begin
  inherited Create;
  FEditText := aEditText;
end;

{********************************************************************************************************}
function TALAndroidEditText.TALKeyPreImeListener.onKeyPreIme(keyCode: Integer; event: JKeyEvent): Boolean;
begin
  {$IF defined(DEBUG)}
  if event <> nil then
    ALLog(
      'TALAndroidEditText.TALKeyPreImeListener.onKeyPreIme',
      'control.name: ' + FEditText.FEditControl.parent.Name + ' | ' +
      'keyCode: ' + inttostr(keyCode) + ' | ' +
      'event: ' + JstringToString(event.toString),
      TalLogType.VERBOSE)
  else
    ALLog(
      'TALAndroidEditText.TALKeyPreImeListener.onKeyPreIme',
      'control.name: ' + FEditText.FEditControl.parent.Name + ' | ' +
      'keyCode: ' + inttostr(keyCode),
      TalLogType.VERBOSE);
  {$ENDIF}
  if ((event = nil) or (event.getAction = AKEY_EVENT_ACTION_UP)) and
     (keyCode = AKEYCODE_BACK) then begin

    result := true;
    FEditText.FEditcontrol.resetfocus;

  end
  else result := false;
end;

{****************************************************************************************}
constructor TALAndroidEditText.TALTextWatcher.Create(const aEditText: TALAndroidEditText);
begin
  inherited Create;
  FEditText := aEditText;
end;

{*************************************************************************}
procedure TALAndroidEditText.TALTextWatcher.afterTextChanged(s: JEditable);
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEditText.TALTextWatcher.afterTextChanged', 'control.name: ' + FEditText.FEditControl.parent.Name, TalLogType.VERBOSE);
  {$ENDIF}
  if assigned(FEditText.fEditControl.fOnChangeTracking) then
    FEditText.fEditControl.fOnChangeTracking(fEditText.fEditControl);
end;

{******************************************************************************************************************************}
procedure TALAndroidEditText.TALTextWatcher.beforeTextChanged(s: JCharSequence; start: Integer; count: Integer; after: Integer);
begin
  // Nothing to do
end;

{***************************************************************************************************************************}
procedure TALAndroidEditText.TALTextWatcher.onTextChanged(s: JCharSequence; start: Integer; before: Integer; count: Integer);
begin
  // Nothing to do
end;

{**********************************************************************************************************************************************}
constructor TALAndroidEditText.TALEditorActionListener.Create(const aEditText: TALAndroidEditText; const aIsMultiLineEditText: Boolean = false);
begin
  inherited Create;
  fIsMultiLineEditText := aIsMultiLineEditText;
  FEditText := aEditText;
end;

{*****************************************************************************************************************************}
function TALAndroidEditText.TALEditorActionListener.onEditorAction(v: JTextView; actionId: Integer; event: JKeyEvent): Boolean;
begin
  {$IF defined(DEBUG)}
   if event <> nil then
     ALLog(
       'TALAndroidEditText.TALEditorActionListener.onEditorAction',
       'control.name: ' + FEditText.FEditControl.parent.Name + ' | ' +
       'actionId: ' + inttostr(actionId) + ' | ' +
       'event: ' + JstringToString(event.toString),
       TalLogType.VERBOSE)
   else
     ALLog(
       'TALAndroidEditText.TALEditorActionListener.onEditorAction',
       'control.name: ' + FEditText.FEditControl.parent.Name + ' | ' +
       'actionId: ' + inttostr(actionId),
       TalLogType.VERBOSE);
  {$ENDIF}
  //IME_ACTION_DONE: the action key performs a "done" operation, typically meaning there is nothing more to input and the IME will be closed.
  //IME_ACTION_GO: the action key performs a "go" operation to take the user to the target of the text they typed. Typically used, for example, when entering a URL.
  //IME_ACTION_NEXT: the action key performs a "next" operation, taking the user to the next field that will accept text.
  //IME_ACTION_NONE: there is no available action.
  //IME_ACTION_PREVIOUS: like IME_ACTION_NEXT, but for moving to the previous field. This will normally not be used to specify an action (since it precludes IME_ACTION_NEXT), but can be returned to the app if it sets IME_FLAG_NAVIGATE_PREVIOUS.
  //IME_ACTION_SEARCH: the action key performs a "search" operation, taking the user to the results of searching for the text they have typed (in whatever context is appropriate).
  //IME_ACTION_SEND: the action key performs a "send" operation, delivering the text to its target. This is typically used when composing a message in IM or SMS where sending is immediate.
  //IME_ACTION_UNSPECIFIED: no specific action has been associated with this editor, let the editor come up with its own if it can.
  if (assigned(FEditText.FEditControl.fOnReturnKey)) and
     (((actionId = TJEditorInfo.javaClass.IME_ACTION_UNSPECIFIED) and // IME_ACTION_UNSPECIFIED = the return key
       (not fIsMultiLineEditText)) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_DONE) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_GO) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_NEXT) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_SEARCH) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_SEND)) then begin

    result := true;
    FEditText.FEditControl.fOnReturnKey(fEditText.fEditControl);

  end
  else result := false;
end;

{****************************************************************************************************************************************************************************}
constructor TALAndroidEditText.Create(const AControl: TalAndroidEdit; Const aIsMultiline: Boolean = False; const aDefStyleAttr: String = ''; const aDefStyleRes: String = '');
begin
  fIsMultiline := aIsMultiline;
  fDefStyleAttr := aDefStyleAttr;
  fDefStyleRes := aDefStyleRes;
  FTextWatcher := TALTextWatcher.Create(self);
  FEditorActionListener := TALEditorActionListener.Create(self, aIsMultiline);
  FKeyPreImeListener := TALKeyPreImeListener.Create(self);
  fEditControl := TalAndroidEdit(AControl);
  inherited create(AControl);  // This will call InitView
end;

{************************************}
destructor TALAndroidEditText.Destroy;
begin

  View.setVisibility(TJView.JavaClass.INVISIBLE);
  View.RemoveTextChangedListener(FTextWatcher);
  View.setOnEditorActionListener(nil);
  View.SetKeyPreImeListener(nil);

  alfreeandNil(FTextWatcher);
  alfreeandNil(fEditorActionListener);
  alfreeandNil(FKeyPreImeListener);

  inherited;

end;

{********************************************}
function TALAndroidEditText.CreateView: JView;
begin
  var LSDKVersion := TJBuild_VERSION.JavaClass.SDK_INT;
  //-----
  if (fDefStyleAttr = '') and
     ((LSDKVersion < 22{lollipop}) or (fDefStyleRes='')) then
    Result := TJALEditText.JavaClass.init(TAndroidHelper.Activity)
  //-----
  else if (LSDKVersion < 22{lollipop}) or (fDefStyleRes='')  then
    Result := TJALEditText.JavaClass.init(
                TAndroidHelper.Activity, // context: JContext;
                nil, // attrs: JAttributeSet;
                TAndroidHelper. // defStyleAttr: Integer
                  Context.
                    getResources().
                      getIdentifier(
                        StringToJstring(fDefStyleAttr), // name	String: The name of the desired resource.
                        StringToJstring('attr'), // String: Optional default resource type to find, if "type/" is not included in the name. Can be null to require an explicit type.
                        TAndroidHelper.Context.getPackageName())) // String: Optional default package to find, if "package:" is not included in the name. Can be null to require an explicit package.
  //-----
  else if (fDefStyleAttr = '') then
    Result := TJALEditText.JavaClass.init(
                TAndroidHelper.Activity, // context: JContext;
                nil, // attrs: JAttributeSet;
                0, // defStyleAttr: Integer
                TAndroidHelper. // defStyleRes: Integer
                  Context.
                    getResources().
                      getIdentifier(
                        StringToJstring(fDefStyleRes), // name	String: The name of the desired resource.
                        StringToJstring('style'), // String: Optional default resource type to find, if "type/" is not included in the name. Can be null to require an explicit type.
                        TAndroidHelper.Context.getPackageName())) // String: Optional default package to find, if "package:" is not included in the name. Can be null to require an explicit package.
  //-----
  else
    Result := TJALEditText.JavaClass.init(
                TAndroidHelper.Activity, // context: JContext;
                nil, // attrs: JAttributeSet;
                TAndroidHelper. // defStyleAttr: Integer
                  Context.
                    getResources().
                      getIdentifier(
                        StringToJstring(fDefStyleAttr), // name	String: The name of the desired resource.
                        StringToJstring('attr'), // String: Optional default resource type to find, if "type/" is not included in the name. Can be null to require an explicit type.
                        TAndroidHelper.Context.getPackageName()), // String: Optional default package to find, if "package:" is not included in the name. Can be null to require an explicit package.
                TAndroidHelper. // defStyleRes: Integer
                  Context.
                    getResources().
                      getIdentifier(
                        StringToJstring(fDefStyleRes), // name	String: The name of the desired resource.
                        StringToJstring('style'), // String: Optional default resource type to find, if "type/" is not included in the name. Can be null to require an explicit type.
                        TAndroidHelper.Context.getPackageName())) // String: Optional default package to find, if "package:" is not included in the name. Can be null to require an explicit package.
end;

{************************************}
procedure TALAndroidEditText.InitView;
begin
  inherited;
  //-----
  if fIsMultiline then View.setSingleLine(False)
  else View.setSingleLine(True);
  View.setClickable(True);
  View.setFocusable(True);
  View.setFocusableInTouchMode(True);
  //-----
  View.addTextChangedListener(fTextWatcher);
  View.setOnEditorActionListener(fEditorActionListener);
  View.SetKeyPreImeListener(fKeyPreImeListener);
end;

{***********************************************}
function TALAndroidEditText.GetView: JALEditText;
begin
  Result := inherited GetView<JALEditText>;
end;

{******************************************************************************************************************************************************************}
constructor TalAndroidEdit.Create(const AOwner: TComponent; Const aIsMultiline: Boolean = False; const aDefStyleAttr: String = ''; const aDefStyleRes: String = '');
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.Create', 'start', TalLogType.VERBOSE);
  {$ENDIF}
  //-----
  inherited create(AOwner);
  //-----
  CanFocus := True;
  FPadding := TBounds.Create(TRectF.Empty);
  fOnChangeTracking := nil;
  FOnReturnKey := nil;
  FFillColor := $ffffffff;
  FTextSettings := TALEditTextSettings.Create;
  FTextSettings.OnChanged := TextSettingsChanged;
  fMaxLength := 0;
  fApplicationEventMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventHandler);
  fReturnKeyType := tReturnKeyType.Default;
  fKeyboardType := TVirtualKeyboardType.default;
  fAutoCapitalizationType := TALAutoCapitalizationType.acNone;
  fPassword := false;
  fCheckSpelling := true;
  fIsMultiline := aIsMultiline;
  FTintColor := TalphaColors.Null;
  FEditText := TALAndroidEditText.create(self, aIsMultiline, aDefStyleAttr, aDefStyleRes);
  DoSetReturnKeyType(fReturnKeyType);
  DoSetInputType(fKeyboardType, fAutoCapitalizationType, fPassword, fCheckSpelling, fIsMultiline);
  //-----
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.Create', 'end', TalLogType.VERBOSE);
  {$ENDIF}
end;


{********************************}
destructor TalAndroidEdit.Destroy;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.Destroy', 'start', TalLogType.VERBOSE);
  {$ENDIF}

  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, fApplicationEventMessageID);
  ALfreeandNil(FEditText);
  ALFreeAndNil(FTextSettings);
  ALFreeAndNil(FPadding);

  inherited Destroy;

  {$IF defined(DEBUG)}
  ALLog('TALAndroidEdit.Destroy', 'end', TalLogType.VERBOSE);
  {$ENDIF}
end;

{**************************************}
procedure TALAndroidEdit.DoSetInputType(
            const aKeyboardType: TVirtualKeyboardType;
            const aAutoCapitalizationType: TALAutoCapitalizationType;
            const aPassword: Boolean;
            const aCheckSpelling: Boolean;
            const aIsMultiline: Boolean);
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

  var LInputType: integer;
  case aKeyboardType of
    TVirtualKeyboardType.Alphabet:              LInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT or TJInputType.JavaClass.TYPE_TEXT_FLAG_NO_SUGGESTIONS;
    TVirtualKeyboardType.URL:                   LInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT or TJInputType.JavaClass.TYPE_TEXT_VARIATION_URI;
    TVirtualKeyboardType.NamePhonePad:          LInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT;
    TVirtualKeyboardType.EmailAddress:          LInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT or TJInputType.JavaClass.TYPE_TEXT_VARIATION_EMAIL_ADDRESS;
    TVirtualKeyboardType.NumbersAndPunctuation: LInputType := TJInputType.JavaClass.TYPE_CLASS_NUMBER or TJInputType.JavaClass.TYPE_NUMBER_FLAG_DECIMAL;
    TVirtualKeyboardType.NumberPad:             LInputType := TJInputType.JavaClass.TYPE_CLASS_NUMBER;
    TVirtualKeyboardType.PhonePad:              LInputType := TJInputType.JavaClass.TYPE_CLASS_PHONE;
    else {TVirtualKeyboardType.Default}         LInputType := TJInputType.JavaClass.TYPE_CLASS_TEXT;
  end;

  case aAutoCapitalizationType of
    TALAutoCapitalizationType.acNone:;
    TALAutoCapitalizationType.acWords:         LInputType := LInputType or TJInputType.JavaClass.TYPE_TEXT_FLAG_CAP_WORDS;
    TALAutoCapitalizationType.acSentences:     LInputType := LInputType or TJInputType.JavaClass.TYPE_TEXT_FLAG_CAP_SENTENCES;
    TALAutoCapitalizationType.acAllCharacters: LInputType := LInputType or TJInputType.JavaClass.TYPE_TEXT_FLAG_CAP_CHARACTERS;
  end;

  if aPassword then begin
    case aKeyboardType of
      TVirtualKeyboardType.NumbersAndPunctuation: LInputType := LInputType or TJInputType.JavaClass.TYPE_NUMBER_VARIATION_PASSWORD;
      TVirtualKeyboardType.NumberPad:             LInputType := LInputType or TJInputType.JavaClass.TYPE_NUMBER_VARIATION_PASSWORD;
      TVirtualKeyboardType.PhonePad:;
      TVirtualKeyboardType.Alphabet:              LInputType := LInputType or TJInputType.JavaClass.TYPE_TEXT_VARIATION_PASSWORD;
      TVirtualKeyboardType.URL:                   LInputType := LInputType or TJInputType.JavaClass.TYPE_TEXT_VARIATION_PASSWORD;
      TVirtualKeyboardType.NamePhonePad:          LInputType := LInputType or TJInputType.JavaClass.TYPE_TEXT_VARIATION_PASSWORD;
      TVirtualKeyboardType.EmailAddress:          LInputType := LInputType or TJInputType.JavaClass.TYPE_TEXT_VARIATION_PASSWORD;
      else {TVirtualKeyboardType.Default}         LInputType := LInputType or TJInputType.JavaClass.TYPE_TEXT_VARIATION_PASSWORD;
    end;
  end
  else if not aCheckSpelling then begin
    // https://stackoverflow.com/questions/61704644/why-type-text-flag-no-suggestions-is-not-working-to-disable-auto-corrections
    case aKeyboardType of
      TVirtualKeyboardType.Alphabet:              LInputType := LInputType or TJInputType.JavaClass.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD;
      TVirtualKeyboardType.URL:;
      TVirtualKeyboardType.NamePhonePad:          LInputType := LInputType or TJInputType.JavaClass.TYPE_TEXT_FLAG_NO_SUGGESTIONS or TJInputType.JavaClass.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD;
      TVirtualKeyboardType.EmailAddress:;
      TVirtualKeyboardType.NumbersAndPunctuation:;
      TVirtualKeyboardType.NumberPad:;
      TVirtualKeyboardType.PhonePad:;
      else {TVirtualKeyboardType.Default}         LInputType := LInputType or TJInputType.JavaClass.TYPE_TEXT_FLAG_NO_SUGGESTIONS or TJInputType.JavaClass.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD;
    end;
  end;

  if aIsMultiline then LInputType := LInputType or TJInputType.JavaClass.TYPE_TEXT_FLAG_MULTI_LINE;

  fEditText.view.setInputType(LInputType);

end;

{**************************************************************************}
procedure TALAndroidEdit.setKeyboardType(const Value: TVirtualKeyboardType);
begin
  if (value <> fKeyboardType) then begin
    fKeyboardType := Value;
    DoSetInputType(Value, fAutoCapitalizationType, fPassword, fCheckSpelling, fIsMultiLine);
  end;
end;

{*****************************************************************************************}
procedure TALAndroidEdit.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  if (value <> fAutoCapitalizationType) then begin
    fAutoCapitalizationType := Value;
    DoSetInputType(fKeyboardType, Value, fPassword, fCheckSpelling, fIsMultiLine);
  end;
end;

{*********************************************************}
procedure TALAndroidEdit.setPassword(const Value: Boolean);
begin
  if (value <> fPassword) then begin
    fPassword := Value;
    DoSetInputType(fKeyboardType, fAutoCapitalizationType, Value, fCheckSpelling, fIsMultiLine);
  end;
end;

{**************************************************************}
procedure TalAndroidEdit.SetCheckSpelling(const Value: Boolean);
begin
  if (value <> fCheckSpelling) then begin
    fCheckSpelling := Value;
    DoSetInputType(fKeyboardType, fAutoCapitalizationType, fPassword, Value, fIsMultiLine);
  end;
end;

{********************************************************************************}
procedure TALAndroidEdit.DoSetReturnKeyType(const aReturnKeyType: TReturnKeyType);
begin
  var LImeOptions: integer;
  case aReturnKeyType of
    TReturnKeyType.Done:          LImeOptions := TJEditorInfo.JavaClass.IME_ACTION_DONE;
    TReturnKeyType.Go:            LImeOptions := TJEditorInfo.JavaClass.IME_ACTION_NONE; // TJEditorInfo.JavaClass.IME_ACTION_GO; => https://stackoverflow.com/questions/44708338/setimeactionlabel-or-setimeoptions-not-work-i-always-have-caption-done-on-the
    TReturnKeyType.Next:          LImeOptions := TJEditorInfo.JavaClass.IME_ACTION_NONE; // TJEditorInfo.JavaClass.IME_ACTION_NEXT; => https://stackoverflow.com/questions/44708338/setimeactionlabel-or-setimeoptions-not-work-i-always-have-caption-done-on-the
    TReturnKeyType.Search:        LImeOptions := TJEditorInfo.JavaClass.IME_ACTION_NONE; // TJEditorInfo.JavaClass.IME_ACTION_SEARCH; => https://stackoverflow.com/questions/44708338/setimeactionlabel-or-setimeoptions-not-work-i-always-have-caption-done-on-the
    TReturnKeyType.Send:          LImeOptions := TJEditorInfo.JavaClass.IME_ACTION_NONE; // TJEditorInfo.JavaClass.IME_ACTION_SEND; => https://stackoverflow.com/questions/44708338/setimeactionlabel-or-setimeoptions-not-work-i-always-have-caption-done-on-the
    else {TReturnKeyType.Default} LImeOptions := TJEditorInfo.JavaClass.IME_ACTION_NONE;
  end;
  fEditText.view.setImeOptions(LImeOptions);
end;

{*********************************************************************}
procedure TALAndroidEdit.setReturnKeyType(const Value: TReturnKeyType);
begin
  if (value <> fReturnKeyType) then begin
    fReturnKeyType := Value;
    DoSetReturnKeyType(Value);
  end;
end;

{*******************************************************}
function TALAndroidEdit.GetLineSpacingMultiplier: single;
begin
  result := FEditText.view.getLineSpacingMultiplier;
end;

{*********************************************************************}
procedure TALAndroidEdit.SetLineSpacingMultiplier(const Value: single);
begin
  fEditText.view.setLineSpacing(fEditText.view.getLineSpacingExtra, Value);
end;

{**************************************************}
function TALAndroidEdit.GetLineSpacingExtra: single;
begin
  result := FEditText.view.getLineSpacingExtra;
end;

{****************************************************************}
procedure TALAndroidEdit.SetLineSpacingExtra(const Value: single);
begin
  fEditText.view.setLineSpacing(Value, fEditText.view.getLineSpacingMultiplier);
end;

{********************************************************}
procedure TALAndroidEdit.SetPadding(const Value: TBounds);
begin
  FPadding.Assign(Value);
  fEditText.view.setPadding(round(Value.Left * ALGetScreenScale), round(Value.Top * ALGetScreenScale), round(Value.Right * AlGetScreenScale), round(Value.Bottom * ALGetScreenScale));
end;

{**********************************************************}
procedure TALAndroidEdit.SetMaxLength(const Value: integer);
begin
  if value <> FMaxLength then begin
    FMaxLength := value;
    fEditText.View.setMaxLength(Value);
  end;
end;

{********************************************}
function TalAndroidEdit.GetTextPrompt: String;
begin
  result := JCharSequenceToStr(fEditText.View.getHint);
end;

{**********************************************************}
procedure TalAndroidEdit.setTextPrompt(const Value: String);
begin
  fEditText.View.setHint(StrToJCharSequence(Value));
end;

{******************************************************}
function TalAndroidEdit.GetTextPromptColor: TAlphaColor;
begin
  result := TAlphaColor(FEditText.View.getCurrentHintTextColor);
end;

{********************************************************************}
procedure TalAndroidEdit.setTextPromptColor(const Value: TAlphaColor);
begin
  if Value <> TalphaColors.null then
    fEditText.View.setHintTextColor(integer(Value));
end;

{**************************************}
function TalAndroidEdit.getText: String;
begin
  result := JCharSequenceToStr(FEditText.View.gettext);
end;

{****************************************************}
procedure TalAndroidEdit.SetText(const Value: String);
begin
  fEditText.View.setText(StrToJCharSequence(Value), TJTextView_BufferType.javaClass.EDITABLE);
end;

{************************************************************}
procedure TALAndroidEdit.TextSettingsChanged(Sender: TObject);
begin

  var LFontColor := integer(ftextsettings.font.color);
  var LFontSize: single := ftextsettings.font.size;

  var LFontStyles: TFontStyles := [];
  if ftextsettings.font.Weight in [TFontWeight.Bold,
                                   TFontWeight.UltraBold,
                                   TFontWeight.Black,
                                   TFontWeight.UltraBlack] then LFontStyles := LFontStyles + [TFontStyle.fsBold];
  if ftextsettings.font.Slant in [TFontSlant.Italic, TFontSlant.Oblique] then LFontStyles := LFontStyles + [TFontStyle.fsItalic];
  var LFontFamily := ALExtractPrimaryFontFamily(ftextsettings.font.Family);
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
  var LGravity: integer;
  case ftextsettings.HorzAlign of
    TALTextHorzAlign.Center: LGravity := $01; // center_horizontal 0x01
    TALTextHorzAlign.Leading: LGravity := $03; // left 0x03
    TALTextHorzAlign.Trailing: LGravity := $05; // right 0x05
    else LGravity := $03; // left 0x03
  end;
  case ftextsettings.VertAlign of
    TALTextVertAlign.Center: LGravity := LGravity or $10; // center_vertical 0x10
    TALTextVertAlign.Leading: LGravity := LGravity or $30; // top 0x30
    TALTextVertAlign.Trailing: LGravity := LGravity or $50; // bottom 0x50
    else LGravity := LGravity or $10; // center_vertical 0x10
  end;

  //-----
  fEditText.view.setTextColor(LFontColor); // Sets the text color for all the states (normal, selected, focused) to be this color.
  fEditText.view.setTextSize(TJTypedValue.javaclass.COMPLEX_UNIT_DIP, LFontSize); // Set the default text size to a given unit and value.
  //-----
  var LTypeface := TJTypeface.JavaClass.create(StringToJString(LFontFamily), ALfontStyleToAndroidStyle(LFontStyles));
  // Note that not all Typeface families actually have bold and italic variants, so you may
  // need to use setTypeface(Typeface, int) to get the appearance that you actually want.
  fEditText.view.setTypeface(LTypeface);
  LTypeface := nil;
  fEditText.view.setgravity(LGravity);

end;

{*******************************************************************}
procedure TALAndroidEdit.SetTextSettings(const Value: TALEditTextSettings);
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

{***************************************************}
function TALAndroidEdit.GetControlType: TControlType;
begin
  // We need GetControlType in TALIosEdit but not really in TALAndroidEdit
  // but I prefer to keep the same logic of TALIosEdit in TALAndroidEdit
  Result := TControlType.Platform;
end;

{*****************************************************************}
procedure TALAndroidEdit.SetControlType(const Value: TControlType);
begin
  // The ControlType cannot be changed
end;

{*************************************}
procedure TalAndroidEdit.DoRootChanged;
begin
  inherited;
  FEditText.RootChanged(Root);
end;

{******************************}
procedure TalAndroidEdit.Resize;
begin
  inherited;
  FEditText.UpdateFrame;
end;

{*****************************************}
procedure TalAndroidEdit.DoAbsoluteChanged;
begin
  inherited;
  if not (csLoading in ComponentState) then
    FEditText.UpdateFrame;
end;

{**************************************}
procedure TalAndroidEdit.VisibleChanged;
begin
  inherited;
  FEditText.SetVisible(Visible);
end;

{***********************************}
procedure TalAndroidEdit.ChangeOrder;
begin
  inherited;
  FEditText.ChangeOrder;
end;

{*************************************}
procedure TalAndroidEdit.RecalcOpacity;
begin
  inherited;
  FEditText.setAlpha(AbsoluteOpacity);
end;

{*************************************}
procedure TalAndroidEdit.RecalcEnabled;
begin
  inherited;
  FEditText.SetAbsoluteEnabled(AbsoluteEnabled);
end;

{*********************************************}
function TalAndroidEdit.HasNativeView: boolean;
begin
  result := visible;
end;

{*************************************}
Procedure TalAndroidEdit.AddNativeView;
begin
  visible := true;
end;

{****************************************}
Procedure TalAndroidEdit.RemoveNativeView;
begin
  visible := False;
end;

{********************************************}
function TALAndroidEdit.getLineHeight: Single;
begin
  result := FEditText.view.getLineHeight / ALGetScreenScale;
end;

{********************************************}
function TALAndroidEdit.getLineCount: integer;
begin
  result := FEditText.view.getLineCount;
end;

{**********************************************************************}
procedure TalAndroidEdit.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  // This proc is called during the ondestroy also when FEditView is already destroyed
  if FEditText <> nil then FEditText.AncestorVisibleChanged;
end;

{*********************************************}
procedure TalAndroidEdit.AncestorParentChanged;
begin
  inherited;
  // This proc is called during the ondestroy also when FEditView is already destroyed
  if FEditText <> nil then FEditText.UpdateFrame;
end;

{*************************************}
procedure TalAndroidEdit.ParentChanged;
begin
  inherited;
  // This proc is called during the ondestroy also when FEditView is already destroyed
  if FEditText <> nil then FEditText.UpdateFrame;
end;

{*******************************}
procedure TalAndroidEdit.DoEnter;
begin
  {$IF defined(DEBUG)}
  ALLog('TalAndroidEdit.DoEnter', 'control.name: ' + parent.Name, TalLogType.VERBOSE);
  {$ENDIF}
  inherited DoEnter;
  FEditText.SetFocus;
  if IsFocused then begin
    ALVirtualKeyboardVisible := True;
    {$IF defined(DEBUG)}
    ALLog('TalAndroidEdit.showVirtualKeyboard', 'control.name: ' + parent.Name, TalLogType.VERBOSE);
    {$ENDIF}
    MainActivity.getVirtualKeyboard.showFor(FEditText.View);
  end;
end;

{******************************}
procedure TalAndroidEdit.DoExit;
begin
  {$IF defined(DEBUG)}
  ALLog('TalAndroidEdit.DoExit', 'control.name: ' + parent.Name, TalLogType.VERBOSE);
  {$ENDIF}
  inherited DoExit;
  FEditText.ResetFocus;
  ALVirtualKeyboardVisible := False;
  TThread.ForceQueue(nil,
    procedure
    begin
      If not ALVirtualKeyboardVisible then begin
        {$IF defined(DEBUG)}
        ALLog('TalAndroidEdit.hideVirtualKeyboard', TalLogType.VERBOSE);
        {$ENDIF}
        MainActivity.getVirtualKeyboard.hide;
      end;
    end);
end;

{***********************************}
procedure TalAndroidEdit.DoEndUpdate;
begin
  inherited;
  // Without this, in some case when we are doing beginupdate to the TEdit
  // (because in android for exemple we would like to not refresh the position of the control during calculation)
  // then when we do endupdate the control is not paint or lost somewhere
  if FEditText <> nil then FEditText.UpdateFrame;
end;

{$endif}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}

{*********************************}
constructor TALIosTextField.Create;
begin
  inherited;
  FTextFieldDelegate := TALIosTextFieldDelegate.Create(Self);
  View.setExclusiveTouch(True);
  View.setBorderStyle(UITextBorderStyleNone);
  View.setDelegate(FTextFieldDelegate.GetObjectID);
  View.addTarget(GetObjectID, sel_getUid(MarshaledAString(TMarshal.AsAnsi('ControlEventEditingChanged'))), UIControlEventEditingChanged);
  View.addTarget(GetObjectID, sel_getUid(MarshaledAString(TMarshal.AsAnsi('ControlEventEditingDidEnd'))), UIControlEventEditingDidEnd);
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
  View.removeTarget(GetObjectID, sel_getUid(MarshaledAString(TMarshal.AsAnsi('ControlEventEditingChanged'))), UIControlEventEditingChanged);
  View.removeTarget(GetObjectID, sel_getUid(MarshaledAString(TMarshal.AsAnsi('ControlEventEditingDidEnd'))), UIControlEventEditingDidEnd);
  View.setDelegate(nil);
  ALFreeAndNil(FTextFieldDelegate);
  inherited;
end;

{********************************************************}
function TALIosTextField.canBecomeFirstResponder: Boolean;
begin
  Result := Control.CanFocus and Control.HitTest;
end;

{*****************************************************}
function TALIosTextField.becomeFirstResponder: Boolean;
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosTextField.becomeFirstResponder', 'control.name: ' + fEditControl.parent.Name, TalLogType.VERBOSE);
  {$ENDIF}
  Result := UITextField(Super).becomeFirstResponder;
  if Result and (not fEditControl.IsFocused) then
    fEditControl.SetFocus;
end;

{***************************************************}
procedure TALIosTextField.ControlEventEditingChanged;
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosTextField.ControlEventEditingChanged', 'control.name: ' + fEditControl.parent.Name, TalLogType.VERBOSE);
  {$ENDIF}
  if assigned(fEditControl.fOnChangeTracking) then
    fEditControl.fOnChangeTracking(fEditControl);
end;

{**************************************************}
procedure TALIosTextField.ControlEventEditingDidEnd;
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosTextField.ControlEventEditingDidEnd', 'control.name: ' + fEditControl.parent.Name, TalLogType.VERBOSE);
  {$ENDIF}
  if assigned(fEditControl.fOnChangeTracking) then
    // When we change the word via the sugestion (clicking on the selection) then ControlEventEditingChanged is not fired
    // imediatly, only on the next key press ... but if we don't press any key but instead close
    // the keyboad then the event will be never fired ! so we catch here this case
    fEditControl.fOnChangeTracking(fEditControl);
end;

{*****************************************************}
function TALIosTextField.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALUITextField);
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
  {$IF defined(DEBUG)}
  ALLog(
    'TALIosTextFieldDelegate.textField',
    'control.name: ' + FTextField.fEditControl.parent.Name + ' | ' +
    'replacementString: ' + NSStrToStr(replacementString),
    TalLogType.VERBOSE);
  {$ENDIF}
  if FTextField.FEditControl.maxLength > 0 then begin
    var LText: NSString := TNSString.Wrap(textField.text);
    if shouldChangeCharactersInRange.length + shouldChangeCharactersInRange.location > LText.length then exit(false);
    result := LText.length + replacementString.length - shouldChangeCharactersInRange.length <= NSUInteger(FTextField.FEditControl.maxLength);
  end
  else Result := True;
end;

{*********************************************************************************}
procedure TALIosTextFieldDelegate.textFieldDidBeginEditing(textField: UITextField);
begin
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
  {$IF defined(DEBUG)}
  ALLog('TALIosTextFieldDelegate.textFieldShouldReturn', 'control.name: ' + FTextField.FEditControl.parent.Name, TalLogType.VERBOSE);
  {$ENDIF}
  if assigned(FTextField.fEditControl.fOnReturnKey) then begin
    FTextField.fEditControl.fOnReturnKey(FTextField.fEditControl);
    result := false;
  end
  else Result := true; // return YES if the text field should implement its default behavior for the return button; otherwise, NO.
end;

{************************************************}
constructor TalIosEdit.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  CanFocus := True;
  fOnChangeTracking := nil;
  FOnReturnKey := nil;
  FFillColor := $ffffffff;
  FTextSettings := TALEditTextSettings.Create;
  FTextSettings.OnChanged := TextSettingsChanged;
  fMaxLength := 0;
  fTextPromptColor := TalphaColors.Null;
  FTextField := TalIosTextField.create(self);
  SetReturnKeyType(tReturnKeyType.Default);
  SetKeyboardType(TVirtualKeyboardType.default);
  setAutoCapitalizationType(TALAutoCapitalizationType.acNone);
  SetPassword(false);
  SetCheckSpelling(True);
end;

{****************************}
destructor TalIosEdit.Destroy;
begin
  ALfreeandNil(FTextField);
  ALFreeAndNil(FTextSettings);
  inherited Destroy;
end;

{**********************************************************************}
procedure TalIosEdit.SetKeyboardType(const Value: TVirtualKeyboardType);
begin
  var LUIKeyboardType: UIKeyboardType;
  case Value of
    TVirtualKeyboardType.NumbersAndPunctuation: LUIKeyboardType := UIKeyboardTypeNumbersAndPunctuation;
    TVirtualKeyboardType.NumberPad:             LUIKeyboardType := UIKeyboardTypeNumberPad;
    TVirtualKeyboardType.PhonePad:              LUIKeyboardType := UIKeyboardTypePhonePad;
    TVirtualKeyboardType.Alphabet:              LUIKeyboardType := UIKeyboardTypeDefault;
    TVirtualKeyboardType.URL:                   LUIKeyboardType := UIKeyboardTypeURL;
    TVirtualKeyboardType.NamePhonePad:          LUIKeyboardType := UIKeyboardTypeNamePhonePad;
    TVirtualKeyboardType.EmailAddress:          LUIKeyboardType := UIKeyboardTypeEmailAddress;
    else {TVirtualKeyboardType.Default}         LUIKeyboardType := UIKeyboardTypeDefault;
  end;
  FTextField.View.setKeyboardType(LUIKeyboardType);
end;

{********************************************************}
function TalIosEdit.GetKeyboardType: TVirtualKeyboardType;
begin
  var LUIKeyboardType := FTextField.View.KeyboardType;
  case LUIKeyboardType of
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
procedure TalIosEdit.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  var LUITextAutoCapitalizationType: UITextAutoCapitalizationType;
  case Value of
    TALAutoCapitalizationType.acWords:          LUITextAutoCapitalizationType := UITextAutoCapitalizationTypeWords;
    TALAutoCapitalizationType.acSentences:      LUITextAutoCapitalizationType := UITextAutoCapitalizationTypeSentences;
    TALAutoCapitalizationType.acAllCharacters:  LUITextAutoCapitalizationType := UITextAutoCapitalizationTypeAllCharacters;
    else {TALAutoCapitalizationType.acNone}     LUITextAutoCapitalizationType := UITextAutoCapitalizationTypeNone;
  end;
  FTextField.View.setAutoCapitalizationType(LUITextAutoCapitalizationType);
end;

{***********************************************************************}
function TalIosEdit.GetAutoCapitalizationType: TALAutoCapitalizationType;
begin
  var LUITextAutoCapitalizationType := FTextField.View.AutoCapitalizationType;
  case LUITextAutoCapitalizationType of
    UITextAutoCapitalizationTypeWords:         result := TALAutoCapitalizationType.acWords;
    UITextAutoCapitalizationTypeSentences:     result := TALAutoCapitalizationType.acSentences;
    UITextAutoCapitalizationTypeAllCharacters: result := TALAutoCapitalizationType.acAllCharacters;
    else                                       result := TALAutoCapitalizationType.acNone;
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
  if Value then begin
    FTextField.View.setSpellCheckingType(UITextSpellCheckingTypeYes);
    FTextField.View.setAutocorrectionType(UITextAutocorrectionTypeDefault);
  end
  else begin
    FTextField.View.setSpellCheckingType(UITextSpellCheckingTypeNo);
    FTextField.View.setAutocorrectionType(UITextAutocorrectionTypeNo);
  end;
end;

{********************************************}
function TalIosEdit.GetCheckSpelling: Boolean;
begin
  result := FTextField.View.SpellCheckingType = UITextSpellCheckingTypeYes;
end;

{*****************************************************************}
procedure TalIosEdit.setReturnKeyType(const Value: TReturnKeyType);
begin
  var LUIReturnKeyType: UIReturnKeyType;
  case Value of
    TReturnKeyType.Done:           LUIReturnKeyType := UIReturnKeyDone;
    TReturnKeyType.Go:             LUIReturnKeyType := UIReturnKeyGo;
    TReturnKeyType.Next:           LUIReturnKeyType := UIReturnKeyNext;
    TReturnKeyType.Search:         LUIReturnKeyType := UIReturnKeySearch;
    TReturnKeyType.Send:           LUIReturnKeyType := UIReturnKeySend;
    else {TReturnKeyType.Default}  LUIReturnKeyType := UIReturnKeyDefault;
  end;
  FTextField.View.setReturnKeyType(LUIReturnKeyType);
end;

{***************************************************}
function TalIosEdit.GetReturnKeyType: TReturnKeyType;
begin
  var LUIReturnKeyType := FTextField.View.ReturnKeyType;
  case LUIReturnKeyType of
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
  var LAttributedString := FTextField.View.AttributedPlaceholder;
  if LAttributedString = nil then Result := NSStrToStr(FTextField.View.placeholder)
  else result := NSStrToStr(LAttributedString.&String);
end;

{******************************************************}
procedure TalIosEdit.setTextPrompt(const Value: String);
begin
  applyTextPromptWithColor(Value, fTextPromptColor);
end;

{****************************************************************}
procedure TalIosEdit.setTextPromptColor(const Value: TAlphaColor);
begin
  if Value <> fTextPromptColor then begin
    fTextPromptColor := Value;
    applyTextPromptWithColor(GetTextPrompt, fTextPromptColor);
  end;
end;

{*******************************************************************************************}
procedure TalIosEdit.applyTextPromptWithColor(const aStr: String; const aColor: TAlphaColor);
begin
  if (aColor = tAlphaColors.Null) or aStr.IsEmpty then FTextField.View.setPlaceholder(StrToNSStr(aStr))
  else begin

    var LTextPromptAttr: NSMutableAttributedString := TNSMutableAttributedString.Wrap(TNSMutableAttributedString.Alloc.initWithString(StrToNSStr(aStr)));
    try

      LTextPromptAttr.beginEditing;
      try

        var LTextRange := NSMakeRange(0, aStr.Length);
        var LUIColor := AlphaColorToUIColor(aColor);
        LTextPromptAttr.addAttribute(NSForegroundColorAttributeName, NSObjectToID(LUIColor), LTextRange);
        //NOTE: If i try to release the LUIColor I have an exception

      finally
        LTextPromptAttr.endEditing;
      end;

      FTextField.View.setAttributedPlaceholder(LTextPromptAttr);

    finally
      LTextPromptAttr.release;
    end;

  end;
end;

{********************************************}
function TalIosEdit.GetTintColor: TAlphaColor;
begin
  var red: CGFloat;
  var green: CGFloat;
  var blue: CGFloat;
  var alpha: CGFloat;
  if not FTextField.View.tintColor.getRed(@red, @green, @blue, @alpha) then result := TalphaColors.Null
  else result := TAlphaColorF.Create(red, green, blue, alpha).ToAlphaColor;
end;

{**********************************************************}
procedure TalIosEdit.setTintColor(const Value: TAlphaColor);
begin
  if Value <> TalphaColors.Null then
    FTextField.View.setTintColor(AlphaColorToUIColor(Value));
end;

{**********************************}
function TalIosEdit.getText: String;
begin
  result := NSStrToStr(TNSString.Wrap(FTextField.View.text));
end;

{************************************************}
procedure TalIosEdit.SetText(const Value: String);
begin
  FTextField.View.setText(StrToNSStr(Value));
end;

{********************************************************}
procedure TalIosEdit.TextSettingsChanged(Sender: TObject);
begin
  // Font
  var LFontRef := ALCreateCTFontRef(fTextSettings.Font.Family, fTextSettings.Font.Size, fTextSettings.Font.Weight, fTextSettings.Font.Slant);
  if LFontRef <> nil then begin
    try

      var LDictionary := TNSDictionary.Wrap(
                           TNSDictionary.OCClass.dictionaryWithObject(
                             LFontRef,
                             NSObjectToID(TNSString.Wrap(kCTFontAttributeName))));
      // Setting this property applies the specified attributes to the entire
      // text of the text field. Unset attributes maintain their default values.
      // note: seam that I can't later call aDictionary.release or i have an error
      FTextField.View.setdefaultTextAttributes(LDictionary);

    finally
      CFRelease(LFontRef);
    end;
  end;

  // TextAlignment and TextColor
  FTextField.View.setTextAlignment(ALTextHorzAlignToUITextAlignment(fTextSettings.HorzAlign));
  FTextField.View.setTextColor(AlphaColorToUIColor(fTextSettings.Font.Color));
end;

{***********************************************}
function TalIosEdit.GetControlType: TControlType;
begin
  //we need ControlType because in function TFMXViewBase.canBecomeFirstResponder: Boolean;
  //we use it in IsNativeControl to determine if it's a native control or not
  Result := TControlType.Platform;
end;

{*************************************************************}
procedure TalIosEdit.SetControlType(const Value: TControlType);
begin
  // The ControlType cannot be changed
end;

{***************************************************************}
procedure TalIosEdit.SetTextSettings(const Value: TALEditTextSettings);
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
  FTextField.UpdateFrame;
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
  FTextField.View.SetEnabled(AbsoluteEnabled);
end;

{*****************************************}
function TalIosEdit.HasNativeView: boolean;
begin
  result := visible;
end;

{*********************************}
Procedure TalIosEdit.AddNativeView;
begin
  visible := true;
end;

{************************************}
Procedure TalIosEdit.RemoveNativeView;
begin
  visible := False;
end;

{******************************************************************}
procedure TalIosEdit.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  // This proc is called during the ondestroy also when FEditView is already destroyed
  if FTextField <> nil then FTextField.AncestorVisibleChanged;
end;

{*****************************************}
procedure TalIosEdit.AncestorParentChanged;
begin
  inherited;
  // This proc is called during the ondestroy also when FEditView is already destroyed
  if FTextField <> nil then FTextField.UpdateFrame;
end;

{*********************************}
procedure TalIosEdit.ParentChanged;
begin
  inherited;
  // This proc is called during the ondestroy also when FEditView is already destroyed
  if FTextField <> nil then FTextField.UpdateFrame;
end;

{***************************}
procedure TalIosEdit.DoEnter;
begin
  {$IF defined(DEBUG)}
  ALLog('TalIosEdit.DoEnter', 'control.name: ' + parent.Name, TalLogType.VERBOSE);
  {$ENDIF}
  inherited DoEnter;
  FTextField.SetFocus;
end;

{**************************}
procedure TalIosEdit.DoExit;
begin
  {$IF defined(DEBUG)}
  ALLog('TalIosEdit.DoExit', 'control.name: ' + parent.Name, TalLogType.VERBOSE);
  {$ENDIF}
  inherited DoExit;
  FTextField.ResetFocus;
end;

{*******************************}
procedure TalIosEdit.DoEndUpdate;
begin
  inherited;
  // Without this, in some case when we are doing beginupdate to the TEdit
  // (because in android for exemple we would like to not refresh the position of the control during calculation)
  // then when we do endupdate the control is not paint or lost somewhere
  if FTextField <> nil then FTextField.UpdateFrame;
end;

{$endif}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}

{*********************************}
constructor TALMacTextField.Create;
begin
  inherited;
  FTextFieldDelegate := TALMacTextFieldDelegate.Create(Self);
  View.SetBezeled(False);
  View.setBordered(false);
  View.setLineBreakMode(NSLineBreakByClipping);
  View.setDrawsBackground(false);
  View.setFocusRingType(NSFocusRingTypeNone);
  View.setDelegate(FTextFieldDelegate.GetObjectID);
end;

{***********************************************************}
constructor TALMacTextField.Create(const AControl: TControl);
begin
  fEditControl := TalMacEdit(AControl);
  inherited;
end;

{*********************************}
destructor TALMacTextField.Destroy;
begin
  View.setDelegate(nil);
  ALFreeAndNil(FTextFieldDelegate);
  inherited;
end;

{******************************************************}
function TALMacTextField.acceptsFirstResponder: Boolean;
begin
  Result := Control.CanFocus and Control.HitTest;
end;

{*****************************************************}
function TALMacTextField.becomeFirstResponder: Boolean;
begin
  {$IF defined(DEBUG)}
  ALLog('TALMacTextField.becomeFirstResponder', 'control.name: ' + fEditControl.parent.Name, TalLogType.VERBOSE);
  {$ENDIF}
  Result := NSTextField(Super).becomeFirstResponder;
  if Result and (not fEditControl.IsFocused) then
    fEditControl.SetFocus;
end;

{*****************************************************}
function TALMacTextField.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALNSTextField);
end;

{********************************************}
function TALMacTextField.GetView: NSTextField;
begin
  Result := inherited GetView<NSTextField>;
end;

{****************************************************************************}
constructor TALMacTextFieldDelegate.Create(const ATextField: TALMacTextField);
begin
  inherited Create;
  FTextField := ATextField;
  if FTextField = nil then
    raise EArgumentNilException.Create(Format(SWrongParameter, ['ATextField']));
end;

{********************************************************************************}
procedure TALMacTextFieldDelegate.controlTextDidBeginEditing(obj: NSNotification);
begin
end;

{******************************************************************************}
procedure TALMacTextFieldDelegate.controlTextDidEndEditing(obj: NSNotification);
begin
end;

{**************************************************************************}
procedure TALMacTextFieldDelegate.controlTextDidChange(obj: NSNotification);
begin
  if FTextField.FEditControl.maxLength > 0 then begin
    var LText := NSStrToStr(FtextField.View.stringValue);
    if LText.length > FTextField.FEditControl.maxLength then begin
      FtextField.View.SetStringValue(StrToNSStr(ALCopyStr(LText,1,FTextField.FEditControl.maxLength)));
      exit;
    end;
  end;
  if assigned(FtextField.fEditControl.fOnChangeTracking) then
    FtextField.fEditControl.fOnChangeTracking(FtextField.fEditControl);
end;

{**************************************************************************************************************************}
function TALMacTextFieldDelegate.controlTextShouldBeginEditing(control: NSControl; textShouldBeginEditing: NSText): Boolean;
begin
  Result := True;
end;

{**********************************************************************************************************************}
function TALMacTextFieldDelegate.controlTextShouldEndEditing(control: NSControl; textShouldEndEditing: NSText): Boolean;
begin
  Result := True;
end;

{***********************************************************************************************************************************************}
function TALMacTextFieldDelegate.controlTextViewDoCommandBySelector(control: NSControl; textView: NSTextView; doCommandBySelector: SEL): Boolean;
begin
  if assigned(FtextField.fEditControl.fOnReturnKey) and (sel_getName(doCommandBySelector) = 'insertNewline:') then begin
    FtextField.fEditControl.fOnReturnKey(FtextField.fEditControl);
    Result := True;
  end
  else
    result := False;
end;

{************************************************}
constructor TalMacEdit.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  CanFocus := True;
  fOnChangeTracking := nil;
  FOnReturnKey := nil;
  FFillColor := $ffffffff;
  FTextSettings := TALEditTextSettings.Create;
  FTextSettings.OnChanged := TextSettingsChanged;
  fMaxLength := 0;
  fReturnKeyType := tReturnKeyType.Default;
  fKeyboardType := TVirtualKeyboardType.default;
  fAutoCapitalizationType := TALAutoCapitalizationType.acNone;
  fPassword := false;
  fCheckSpelling := true;
  fTextPromptColor := TalphaColors.Null;
  FTintColor := TalphaColors.Null;
  FTextField := TalMacTextField.create(self);
end;

{****************************}
destructor TalMacEdit.Destroy;
begin
  ALfreeandNil(FTextField);
  ALFreeAndNil(FTextSettings);
  inherited Destroy;
end;

{****************************************}
function TalMacEdit.GetTextPrompt: String;
begin
  var LAttributedString := FTextField.View.placeholderAttributedString;
  if LAttributedString = nil then Result := NSStrToStr(FTextField.View.PlaceholderString)
  else result := NSStrToStr(LAttributedString.&String);
end;

{******************************************************}
procedure TalMacEdit.setTextPrompt(const Value: String);
begin
  applyTextPromptWithColor(Value, fTextPromptColor);
end;

{****************************************************************}
procedure TalMacEdit.setTextPromptColor(const Value: TAlphaColor);
begin
  if Value <> fTextPromptColor then begin
    fTextPromptColor := Value;
    applyTextPromptWithColor(GetTextPrompt, fTextPromptColor);
  end;
end;

{*******************************************************************************************}
procedure TalMacEdit.applyTextPromptWithColor(const aStr: String; const aColor: TAlphaColor);
begin
  if (aColor = tAlphaColors.Null) or aStr.IsEmpty then FTextField.View.SetPlaceholderString(StrToNSStr(aStr))
  else begin

    var LTextPromptAttr: NSMutableAttributedString := TNSMutableAttributedString.Wrap(TNSMutableAttributedString.Alloc.initWithString(StrToNSStr(aStr)));
    try

      LTextPromptAttr.beginEditing;
      try

        var LTextRange := NSMakeRange(0, aStr.Length);
        var LUIColor := AlphaColorToNSColor(aColor);
        LTextPromptAttr.addAttribute(NSForegroundColorAttributeName, NSObjectToID(LUIColor), LTextRange);
        //NOTE: If U try to release the LUIColor I have an exception

        // No need to do this in iOS, only in MacOS
        var LFontRef := ALCreateCTFontRef(fTextSettings.Font.Family, fTextSettings.Font.Size, fTextSettings.Font.Weight, fTextSettings.Font.Slant);
        if LFontRef <> nil then begin
          try
            LTextPromptAttr.addAttribute(NSFontAttributeName, NSObjectToID(TNSFont.Wrap(LFontRef)), LTextRange);
          finally
            CFRelease(LFontRef);
          end;
        end;

      finally
        LTextPromptAttr.endEditing;
      end;

      FTextField.View.setPlaceholderAttributedString(LTextPromptAttr);

    finally
      LTextPromptAttr.release;
    end;

  end;
end;

{**********************************}
function TalMacEdit.getText: String;
begin
  result := NSStrToStr(TNSString.Wrap(FTextField.View.StringValue));
end;

{************************************************}
procedure TalMacEdit.SetText(const Value: String);
begin
  FTextField.View.setStringValue(StrToNSStr(Value));
end;

{********************************************************}
procedure TalMacEdit.TextSettingsChanged(Sender: TObject);
begin
  // Font
  var LFontRef := ALCreateCTFontRef(fTextSettings.Font.Family, fTextSettings.Font.Size, fTextSettings.Font.Weight, fTextSettings.Font.Slant);
  if LFontRef <> nil then begin
    try
      FTextField.View.setFont(TNSFont.Wrap(LFontRef));
    finally
      CFRelease(LFontRef);
    end;
  end;

  // TextAlignment and TextColor
  FTextField.View.setAlignment(ALTextHorzAlignToNSTextAlignment(fTextSettings.HorzAlign));
  FTextField.View.setTextColor(AlphaColorToNSColor(fTextSettings.Font.Color));

  // Update the TextPrompt with the new font settings. This is only necessary in macOS.
  // In iOS, this step is not required.
  applyTextPromptWithColor(TextPrompt, TextPromptColor);
end;

{***********************************************}
function TalMacEdit.GetControlType: TControlType;
begin
  //we need ControlType because in function TFMXViewBase.canBecomeFirstResponder: Boolean;
  //we use it in IsNativeControl to determine if it's a native control or not
  Result := TControlType.Platform;
end;

{*************************************************************}
procedure TalMacEdit.SetControlType(const Value: TControlType);
begin
  // The ControlType cannot be changed
end;

{***************************************************************}
procedure TalMacEdit.SetTextSettings(const Value: TALEditTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{*********************************}
procedure TalMacEdit.DoRootChanged;
begin
  inherited;
  FTextField.RootChanged(Root);
end;

{**************************}
procedure TalMacEdit.Resize;
begin
  inherited;
  FTextField.UpdateFrame;
end;

{*************************************}
procedure TalMacEdit.DoAbsoluteChanged;
begin
  inherited;
  if not (csLoading in ComponentState) then
    FTextField.UpdateFrame;
end;

{**********************************}
procedure TalMacEdit.VisibleChanged;
begin
  inherited;
  FTextField.SetVisible(Visible);
end;

{*******************************}
procedure TalMacEdit.ChangeOrder;
begin
  inherited;
  FTextField.ChangeOrder;
end;

{*********************************}
procedure TalMacEdit.RecalcOpacity;
begin
  inherited;
  FTextField.setAlpha(AbsoluteOpacity);
end;

{*********************************}
procedure TalMacEdit.RecalcEnabled;
begin
  inherited;
  FTextField.SetAbsoluteEnabled(AbsoluteEnabled);
  FTextField.View.SetEnabled(AbsoluteEnabled);
end;

{*****************************************}
function TalMacEdit.HasNativeView: boolean;
begin
  result := visible;
end;

{*********************************}
Procedure TalMacEdit.AddNativeView;
begin
  visible := true;
end;

{************************************}
Procedure TalMacEdit.RemoveNativeView;
begin
  visible := False;
end;

{******************************************************************}
procedure TalMacEdit.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  // This proc is called during the ondestroy also when FEditView is already destroyed
  if FTextField <> nil then FTextField.AncestorVisibleChanged;
end;

{*****************************************}
procedure TalMacEdit.AncestorParentChanged;
begin
  inherited;
  // This proc is called during the ondestroy also when FEditView is already destroyed
  if FTextField <> nil then FTextField.UpdateFrame;
end;

{*********************************}
procedure TalMacEdit.ParentChanged;
begin
  inherited;
  // This proc is called during the ondestroy also when FEditView is already destroyed
  if FTextField <> nil then FTextField.UpdateFrame;
end;

{***************************}
procedure TalMacEdit.DoEnter;
begin
  {$IF defined(DEBUG)}
  ALLog('TalMacEdit.DoEnter', 'control.name: ' + parent.Name, TalLogType.VERBOSE);
  {$ENDIF}
  inherited DoEnter;
  FTextField.SetFocus;
end;

{**************************}
procedure TalMacEdit.DoExit;
begin
  {$IF defined(DEBUG)}
  ALLog('TalMacEdit.DoExit', 'control.name: ' + parent.Name, TalLogType.VERBOSE);
  {$ENDIF}
  inherited DoExit;
  FTextField.ResetFocus;
end;

{*******************************}
procedure TalMacEdit.DoEndUpdate;
begin
  inherited;
  // Without this, in some case when we are doing beginupdate to the TEdit
  // (because in android for exemple we would like to not refresh the position of the control during calculation)
  // then when we do endupdate the control is not paint or lost somewhere
  if FTextField <> nil then FTextField.UpdateFrame;
end;

{$endif}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

{***********************************************}
function InitCommonControl(CC: Integer): Boolean;
var
  ICC: TInitCommonControlsEx;
begin
  ICC.dwSize := SizeOf(TInitCommonControlsEx);
  ICC.dwICC := CC;
  Result := InitCommonControlsEx(ICC);
  if not Result then InitCommonControls;
end;

{****************************************}
procedure CheckCommonControl(CC: Integer);
begin
  if not InitCommonControl(CC) then
    raise EComponentError.Create('This control requires version 4.70 or greater of COMCTL32.DLL');
end;

{**********************************************************}
constructor TALWinEditView.Create(const AControl: TControl);
begin
  CheckCommonControl(ICC_STANDARD_CLASSES);
  FFontHandle := 0;
  FBackgroundBrush := 0;
  fEditControl := TALWinEdit(AControl);
  inherited Create(AControl);
end;

{***************************************************************}
procedure TALWinEditView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'EDIT');
  Params.Style := Params.Style or ES_AUTOHSCROLL;
end;

{********************************}
destructor TALWinEditView.Destroy;
begin
  if (FFontHandle <> 0) and (not DeleteObject(FFontHandle)) then
    raise Exception.Create('Error 5E76956E-B13B-4694-8D83-CA6BEBC06CCE');
  if (FBackgroundBrush <> 0) and (not DeleteObject(FBackgroundBrush)) then
    RaiseLastOsError;
  inherited Destroy;
end;

{****************************************}
procedure TALWinEditView.UpdateFontHandle;
begin
  SendMessage(Handle, WM_SETFONT, 0, 0);

  if FFontHandle <> 0 then begin
    if not DeleteObject(FFontHandle) then RaiseLastOsError;
    FFontHandle := 0;
  end;
  FFontHandle := CreateFont(
                   -Round(FEditControl.TextSettings.Font.Size * ALGetScreenScale), // nHeight
                   0, // nWidth
                   0, // nEscapement
                   0, // nOrientaion
                   FontWeightToWinapi(FEditControl.TextSettings.Font.Weight), // fnWeight
                   DWORD(not FEditControl.TextSettings.Font.Slant.IsRegular), // fdwItalic
                   DWORD(TALTextDecorationKind.Underline in FEditControl.TextSettings.Decoration.Kinds), // fdwUnderline
                   DWORD(TALTextDecorationKind.LineThrough in FEditControl.TextSettings.Decoration.Kinds), // fdwStrikeOut
                   0, // fdwCharSet
                   0, // fdwOutputPrecision
                   0, // fdwClipPrecision
                   0, // fdwQuality
                   0, // fdwPitchAndFamily
                   PChar(ALExtractPrimaryFontFamily(FEditControl.TextSettings.Font.Family))); // lpszFace

  SendMessage(Handle, WM_SETFONT, FFontHandle, 1);
end;

{*********************************************}
procedure TALWinEditView.UpdateBackgroundBrush;
begin
  if (fBackgroundBrush <> 0) and (not DeleteObject(fBackgroundBrush)) then
    RaiseLastOsError;
  if FEditControl.FillColor <> TAlphaColors.Null then begin
    fBackgroundBrush := CreateSolidBrush(TAlphaColors.ColorToRGB(FEditControl.FillColor));
    if fBackgroundBrush = 0 then RaiseLastOsError;
  end
  else
    fBackgroundBrush := 0;
end;

{**********************************************************}
procedure TALWinEditView.WMKeyDown(var Message: TWMKeyDown);
begin
  if Message.CharCode = VK_TAB then begin
    // Without this adjustment, pressing the Tab key results in a beep sound.
    // This occurs because the Windows API attempts to move focus to the next
    // control, but tab navigation is somehow restricted in the current context),
    // it signals this inability or failure by playing a system sound.
    var LMsg: TMsg;
    PeekMessage(LMsg, Handle, 0, 0, PM_REMOVE);
  end;
  inherited;
  if Message.CharCode = VK_RETURN then begin
    if assigned(fEditControl.fOnReturnKey) then
      fEditControl.fOnReturnKey(fEditControl);
  end
  else if Message.CharCode = VK_DELETE then begin
    if assigned(fEditControl.fOnChangeTracking) then
      fEditControl.fOnChangeTracking(fEditControl);
  end;
end;

{***************************************************}
procedure TALWinEditView.WMChar(var Message: TWMKey);
begin
  inherited;
  if assigned(fEditControl.fOnChangeTracking) then
    fEditControl.fOnChangeTracking(fEditControl);
end;

{**************************************************************************}
procedure TALWinEditView.WMTextColor(var Message: WinApi.Messages.TMessage);
begin
  inherited;
  if SetTextColor(Message.wParam, TAlphaColors.ColorToRGB(FeditControl.TextSettings.Font.Color)) = CLR_INVALID then RaiseLastOSError;
  if fBackgroundBrush <> 0 then begin
    if SetBkColor(Message.wParam, TAlphaColors.ColorToRGB(FEditControl.FillColor)) = CLR_INVALID then RaiseLastOSError;
    Message.Result := FBackgroundBrush;
  end
  else
    Message.Result := GetSysColorBrush(COLOR_WINDOW);
end;

{**********************************************************************}
procedure TALWinEditView.WMPaint(var Message: WinApi.Messages.TMessage);
begin
  if (FEditControl.TextPrompt <> '') and
     (not FEditControl.IsFocused) and
     (GetWindowTextLength(Handle) = 0) then begin
    var LPS: PAINTSTRUCT;
    var LDC := BeginPaint(Handle, LPS);
    if LDC = 0 then raise Exception.Create('Error 3B053C24-4A18-497C-82B1-C540EF7C2A4B');
    Try
      var LTextPromptColor := FeditControl.TextPromptColor;
      if LTextPromptColor = TAlphaColors.Null then
        LTextPromptColor := ALBlendColorWithOpacity(FEditControl.fillColor, FeditControl.TextSettings.Font.Color, 0.3);
      if SetTextColor(LDC, TAlphaColors.ColorToRGB(LTextPromptColor)) = CLR_INVALID then RaiseLastOSError;
      if SetBkColor(LDC, TAlphaColors.ColorToRGB(FeditControl.fillColor)) = CLR_INVALID then RaiseLastOSError;
      if FBackgroundBrush <> 0 then begin
        if FillRect(LDC, LPS.rcPaint, FBackgroundBrush) = 0 then raiseLastOsError;
      end
      else begin
        if FillRect(LDC, LPS.rcPaint, GetSysColorBrush(COLOR_WINDOW)) = 0 then raiseLastOsError;
      end;
      if SetBkMode(LDC, TRANSPARENT) = 0 then RaiseLastOSError;
      If (FFontHandle <> 0) and (SelectObject(LDC, FFontHandle) = 0) then raiseLastOsError;
      var LMargins := SendMessage(Handle, EM_GETMARGINS, 0, 0);
      //LOWORD(LMargins) = Left Margin
      //HIWORD(LMargins) = Right Margin
      TextOut(LDC, round(LOWORD(LMargins) * ALGetScreenScale), 0, Pchar(FEditControl.TextPrompt), Length(FEditControl.TextPrompt));
    finally
      EndPaint(Handle, LPS);
    end;
  end
  else
    inherited;
end;

{************************************************}
constructor TALWinEdit.Create(AOwner: TComponent);
begin
  {$IF defined(DEBUG)}
  ALLog('TALWinEdit.Create', 'start', TalLogType.VERBOSE);
  {$ENDIF}
  //-----
  inherited create(AOwner);
  //-----
  CanFocus := True;
  fOnChangeTracking := nil;
  FOnReturnKey := nil;
  FFillColor := $ffffffff;
  FTextSettings := TALEditTextSettings.Create;
  FTextSettings.OnChanged := TextSettingsChanged;
  FTextPrompt := '';
  FTextPromptColor := TalphaColors.Null;
  fReturnKeyType := tReturnKeyType.Default;
  fKeyboardType := TVirtualKeyboardType.default;
  fAutoCapitalizationType := TALAutoCapitalizationType.acNone;
  fCheckSpelling := true;
  FTintColor := TalphaColors.Null;
  FEditView := TALWinEditView.create(self);
  SetPassword(false);
  //-----
  {$IF defined(DEBUG)}
  ALLog('TALWinEdit.Create', 'end', TalLogType.VERBOSE);
  {$ENDIF}
end;

{********************************}
destructor TALWinEdit.Destroy;
begin
  {$IF defined(DEBUG)}
  ALLog('TALWinEdit.Destroy', 'start', TalLogType.VERBOSE);
  {$ENDIF}

  ALfreeandNil(FEditView);
  ALFreeAndNil(FTextSettings);

  inherited Destroy;

  {$IF defined(DEBUG)}
  ALLog('TALWinEdit.Destroy', 'end', TalLogType.VERBOSE);
  {$ENDIF}
end;

{*************************************************************************************}
procedure TALWinEdit.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  if (value <> fAutoCapitalizationType) then begin
    fAutoCapitalizationType := Value;
    var LStyle := GetWindowLong(FEditView.Handle, GWL_STYLE);
    if LStyle = 0 then RaiseLastOsError;
    case fAutoCapitalizationType of
      TALAutoCapitalizationType.acNone:          LStyle := LStyle and not ES_UPPERCASE;
      TALAutoCapitalizationType.acWords:         LStyle := LStyle and not ES_UPPERCASE;
      TALAutoCapitalizationType.acSentences:     LStyle := LStyle and not ES_UPPERCASE;
      TALAutoCapitalizationType.acAllCharacters: LStyle := LStyle or ES_UPPERCASE;
      else raise Exception.Create('Error 21CC0DF5-9030-4F6C-9830-112E17E1A392');
    end;
    if SetWindowLong(FEditView.Handle, GWL_STYLE, LStyle) = 0 then RaiseLastOsError;
  end;
end;

{*****************************************************}
procedure TALWinEdit.setPassword(const Value: Boolean);
begin
  if (value <> Password) then begin
    if Value then SendMessage(FEditView.Handle, EM_SETPASSWORDCHAR, Ord('*'), 0)
    else SendMessage(FEditView.Handle, EM_SETPASSWORDCHAR, 0, 0);
    FEditView.Invalidate;
  end;
end;

{**************************************}
function TALWinEdit.GetPassword: Boolean;
begin
  result := SendMessage(FEditView.Handle, EM_GETPASSWORDCHAR, 0, 0) <> 0;
end;

{**********************************************************}
procedure TALWinEdit.SetMaxLength(const Value: integer);
begin
  SendMessage(FEditView.Handle, EM_LIMITTEXT, Value, 0);
end;

{****************************************}
function TALWinEdit.GetMaxLength: integer;
begin
  Result := SendMessage(FEditView.Handle, EM_GETLIMITTEXT, 0, 0);
  if Result = $7FFFFFFE {2147483646} then Result := 0;
end;

{**********************************************************}
procedure TALWinEdit.setTextPrompt(const Value: String);
begin
  if FTextPrompt <> Value then begin
    FTextPrompt := Value;
    FEditView.Invalidate;
  end;
end;

{********************************************************************}
procedure TALWinEdit.setTextPromptColor(const Value: TAlphaColor);
begin
  if Value <> FTextPromptColor then begin
    FTextPromptColor := Value;
    FEditView.Invalidate;
  end;
end;

{**********************************************************}
procedure TALWinEdit.SetFillColor(const Value: TAlphaColor);
begin
  if FFillColor <> Value then begin
    FFillColor := Value;
    FEditView.UpdateBackgroundBrush;
    FEditView.Invalidate;
  end;
end;

{**************************************}
function TALWinEdit.getText: String;
begin
  Result := GetHWNDText(FEditView.Handle);
end;

{************************************************}
procedure TALWinEdit.SetText(const Value: String);
begin
  SetWindowText(fEditView.Handle, PChar(Value));
end;

{******************************************************}
procedure TALWinEdit.TextSettingsChanged(Sender: TObject);
begin
  fEditView.UpdateFontHandle;
  //--
  var LStyle := GetWindowLong(fEditView.Handle, GWL_STYLE);
  if LStyle = 0 then RaiseLastOsError;
  case TextSettings.HorzAlign of
      TALTextHorzAlign.Center:   LStyle := LStyle or ES_CENTER;
      TALTextHorzAlign.Leading:  LStyle := LStyle or ES_LEFT;
      TALTextHorzAlign.Trailing: LStyle := LStyle or ES_RIGHT;
      TALTextHorzAlign.Justify:  LStyle := LStyle or ES_LEFT;
    else raise Exception.Create('Error 21CC0DF5-9030-4F6C-9830-112E17E1A392');
  end;
  if SetWindowLong(fEditView.Handle, GWL_STYLE, LStyle) = 0 then RaiseLastOsError;
  //--
  FEditView.Invalidate;
end;

{*******************************************************************}
procedure TALWinEdit.SetTextSettings(const Value: TALEditTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{***************************************************}
function TALWinEdit.GetControlType: TControlType;
begin
  // We need GetControlType in TALIosEdit but not really in TALWinEdit
  // but I prefer to keep the same logic of TALIosEdit in TALWinEdit
  Result := TControlType.Platform;
end;

{*****************************************************************}
procedure TALWinEdit.SetControlType(const Value: TControlType);
begin
  // The ControlType cannot be changed
end;

{*************************************}
procedure TALWinEdit.DoRootChanged;
begin
  inherited;
  FEditView.RootChanged(Root);
end;

{******************************}
procedure TALWinEdit.Resize;
begin
  inherited;
  FEditView.updateFrame;
end;

{*****************************************}
procedure TALWinEdit.DoAbsoluteChanged;
begin
  inherited;
  if not (csLoading in ComponentState) then
    FEditView.UpdateFrame;
end;

{**************************************}
procedure TALWinEdit.VisibleChanged;
begin
  inherited;
  FEditView.SetVisible(Visible);
end;

{***********************************}
procedure TALWinEdit.ChangeOrder;
begin
  inherited;
  FEditView.ChangeOrder;
end;

{*************************************}
procedure TALWinEdit.RecalcOpacity;
begin
  inherited;
end;

{*************************************}
procedure TALWinEdit.RecalcEnabled;
begin
  inherited;
  FEditView.SetAbsoluteEnabled(AbsoluteEnabled);
end;

{*********************************************}
function TALWinEdit.HasNativeView: boolean;
begin
  result := visible;
end;

{*************************************}
Procedure TALWinEdit.AddNativeView;
begin
  visible := true;
end;

{****************************************}
Procedure TALWinEdit.RemoveNativeView;
begin
  visible := False;
end;

{**********************************************************************}
procedure TALWinEdit.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  // This proc is called during the ondestroy also when FEditView is already destroyed
  if FEditView <> nil then FEditView.AncestorVisibleChanged;
end;

{*********************************************}
procedure TALWinEdit.AncestorParentChanged;
begin
  inherited;
  // This proc is called during the ondestroy also when FEditView is already destroyed
  if FEditView <> nil then FEditView.UpdateFrame;
end;

{*************************************}
procedure TALWinEdit.ParentChanged;
begin
  inherited;
  // This proc is called during the ondestroy also when FEditView is already destroyed
  if FEditView <> nil then FEditView.UpdateFrame;
end;

{*******************************}
procedure TALWinEdit.DoEnter;
begin
  {$IF defined(DEBUG)}
  ALLog('TALWinEdit.DoEnter', 'control.name: ' + parent.Name, TalLogType.VERBOSE);
  {$ENDIF}
  inherited DoEnter;
  FEditView.SetFocus;
end;

{******************************}
procedure TALWinEdit.DoExit;
begin
  {$IF defined(DEBUG)}
  ALLog('TALWinEdit.DoExit', 'control.name: ' + parent.Name, TalLogType.VERBOSE);
  {$ENDIF}
  inherited DoExit;
  FEditView.ResetFocus;
end;

{***********************************}
procedure TALWinEdit.DoEndUpdate;
begin
  inherited;
  // Without this, in some case when we are doing beginupdate to the TEdit
  // (because in android for exemple we would like to not refresh the position of the control during calculation)
  // then when we do endupdate the control is not paint or lost somewhere
  if FEditView <> nil then FEditView.UpdateFrame;
end;

{$endif}
{$ENDREGION}

{*********************************************}
constructor TALEdit.Create(AOwner: TComponent);
begin
  inherited;
  fDefStyleAttr := '';
  fDefStyleRes := '';
  FAutoTranslate := true;
  fOnChangeTracking := nil;
  FOnReturnKey := nil;
  fOnEnter := nil;
  fOnExit := nil;
  FTextSettings := TALEditTextSettings.Create;
  FTextSettings.OnChanged := TextSettingsChanged;
  Cursor := crIBeam;
  CanFocus := True;
  CanParentFocus := False; // else you must rewrite the GetCanFocus
  fill.OnChanged := nil;
  fill.DefaultColor := $ffffffff;
  fill.Color := $ffffffff;
  fill.OnChanged := FillChanged;
  stroke.OnChanged := Nil;
  stroke.DefaultKind := TBrushKind.none;
  stroke.kind := TBrushKind.none;
  stroke.OnChanged := StrokeChanged;
  //-----
  fEditControl := nil;
  {$IF defined(android)}
  // In Android we must first know the value of DefStyleAttr/DefStyleRes
  // before to create the fEditControl. I use this way to know that the compoment
  // will load it's properties from the dfm
  if (aOwner = nil) or
     (not (csloading in aOwner.ComponentState)) then CreateEditControl;
  {$ELSE}
  CreateEditControl;
  {$ENDIF}
end;

{*************************}
destructor TALEdit.Destroy;
begin
  ALFreeAndNil(FTextSettings);
  ALFreeAndNil(fEditControl);
  inherited;
end;

{**********************************}
procedure TALEdit.CreateEditControl;
begin
  if fEditControl <> nil then exit;
  {$IF defined(android)}
  fEditControl := TALAndroidEdit.Create(self, false, fDefStyleAttr, fDefStyleRes);
  {$ELSEIF defined(ios)}
  fEditControl := TALIosEdit.Create(self);
  {$ELSEIF defined(ALMacOS)}
  fEditControl := TALMacEdit.Create(self);
  {$ELSEIF defined(MSWindows)}
  fEditControl := TALWinEdit.Create(self);
  {$ELSE}
  raise Exception.Create('Not Implemented');
  {$ENDIF}
  fEditControl.Parent := self;
  FeditControl.Stored := False;
  FeditControl.SetSubComponent(True);
  FeditControl.Locked := True;
  FeditControl.OnReturnKey := nil; // noops operation
  fEditControl.Align := TAlignLayout.Client;
  FeditControl.OnChangeTracking := OnChangeTrackingImpl;
  FeditControl.OnEnter := OnEnterImpl;
  FeditControl.OnExit := OnExitImpl;
  fEditControl.Password := false; // noops operation
  fEditControl.ReturnKeyType := tReturnKeyType.Default;  // noops operation
  fEditControl.KeyboardType := TVirtualKeyboardType.Default; // noops operation
  fEditControl.AutoCapitalizationType := TALAutoCapitalizationType.acNone; // noops operation
  fEditControl.CheckSpelling := True;
  fEditControl.MaxLength := 0; // noops operation
  fEditControl.TextPromptColor := TALphaColors.Null; // noops operation
  fEditControl.TintColor := TALphaColors.Null; // noops operation
  fEditControl.FillColor := $ffffffff; // noops operation
end;

{***********************}
procedure TALEdit.Loaded;
begin
  CreateEditControl;
  //--
  // csLoading is in ComponentState
  if (TextSettings.Font.AutoConvert) and
     (TextSettings.Font.Family <> '') and
     (not (csDesigning in ComponentState)) then
    TextSettings.Font.Family := ALConvertFontFamily(TextSettings.Font.Family);
  //--
  // remove csLoading from ComponentState
  inherited;
  //--
  if (AutoTranslate) and
     (TextPrompt <> '') and
     (not (csDesigning in ComponentState)) then
    TextPrompt := ALTranslate(TextPrompt);
  //--
  TextSettingsChanged(TextSettings);
  //AdjustSize; => Already called in TextSettingsChanged
end;

{*****************************************************}
procedure TALEdit.SetDefStyleAttr(const Value: String);
begin
  if Value <> fDefStyleAttr then begin
    fDefStyleAttr := Value;
    {$IFDEF ANDROID}
    if not (csLoading in componentState) then begin
      ALFreeAndNil(fEditControl);
      CreateEditControl;
    end;
    {$ENDIF}
  end;
end;

{****************************************************}
procedure TALEdit.SetDefStyleRes(const Value: String);
begin
  if Value <> fDefStyleRes then begin
    fDefStyleRes := Value;
    {$IFDEF ANDROID}
    if not (csLoading in componentState) then begin
      ALFreeAndNil(fEditControl);
      CreateEditControl;
    end;
    {$ENDIF}
  end;
end;

{**************************************}
function TALEdit.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(100, 22);
end;

{********************}
{$IF defined(android)}
function TALEdit.GetEditControl: TALAndroidEdit;
begin
  if FEditControl = nil then CreateEditControl;
  Result := FEditControl;
end;
{$ENDIF}

{********************}
{$IF defined(android)}
function TALEdit.GetAndroidEditText: TALAndroidEditText;
begin
  result := EditControl.EditText;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALEdit.GetEditControl: TALIosEdit;
begin
  if FEditControl = nil then CreateEditControl;
  Result := FEditControl;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALEdit.GetIosTextField: TALIosTextField;
begin
  result := EditControl.TextField;
end;
{$ENDIF}

{******************}
{$IF defined(ALMacOS)}
function TALEdit.GetEditControl: TALMacEdit;
begin
  if FEditControl = nil then CreateEditControl;
  Result := FEditControl;
end;
{$ENDIF}

{******************}
{$IF defined(ALMacOS)}
function TALEdit.GetMacTextField: TALMacTextField;
begin
  result := EditControl.TextField;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
function TALEdit.GetEditControl: TALWinEdit;
begin
  if FEditControl = nil then CreateEditControl;
  Result := FEditControl;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
function TALEdit.GetWinEditView: TALWinEditView;
begin
  result := EditControl.EditView;
end;
{$ENDIF}

{************************************************************}
procedure TALEdit.SetTextSettings(const Value: TALEditTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{*****************************************************}
procedure TALEdit.TextSettingsChanged(Sender: TObject);
begin
  if csLoading in componentState then exit;
  EditControl.TextSettings.Assign(ftextsettings);
  AdjustSize;
end;

{*********************************************}
procedure TALEdit.SetText(const Value: String);
begin
  EditControl.Text := Value;
end;

{*******************************}
function TALEdit.getText: String;
begin
  result := EditControl.Text;
end;

{*************************************}
function TALEdit.GetTextPrompt: String;
begin
  result := EditControl.TextPrompt;
end;

{***************************************************}
procedure TALEdit.setTextPrompt(const Value: String);
begin
  EditControl.TextPrompt := Value;
end;

{***********************************************}
function TALEdit.GetTextPromptColor: TAlphaColor;
begin
  result := EditControl.TextPromptColor;
end;

{*************************************************************}
procedure TALEdit.setTextPromptColor(const Value: TAlphaColor);
begin
  EditControl.TextPromptColor := Value;
end;

{*****************************************}
function TALEdit.GetTintColor: TAlphaColor;
begin
  result := EditControl.TintColor;
end;

{*******************************************************}
procedure TALEdit.setTintColor(const Value: TAlphaColor);
begin
  EditControl.TintColor := Value;
end;

{*************************************************************}
procedure TALEdit.SetKeyboardType(Value: TVirtualKeyboardType);
begin
  EditControl.KeyboardType := Value;
end;

{*****************************************************}
function TALEdit.GetKeyboardType: TVirtualKeyboardType;
begin
  result := EditControl.KeyboardType;
end;

{********************************************************************}
function TALEdit.GetAutoCapitalizationType: TALAutoCapitalizationType;
begin
  result := EditControl.AutoCapitalizationType;
end;

{**********************************************************************************}
procedure TALEdit.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  EditControl.AutoCapitalizationType := Value;
end;

{**************************************************}
procedure TALEdit.SetPassword(const Value: Boolean);
begin
  EditControl.Password := Value;
end;

{************************************}
function TALEdit.GetPassword: Boolean;
begin
  result := EditControl.Password;
end;

{*******************************************************}
procedure TALEdit.SetCheckSpelling(const Value: Boolean);
begin
  EditControl.CheckSpelling := Value;
end;

{*****************************************}
function TALEdit.GetCheckSpelling: Boolean;
begin
  result := EditControl.CheckSpelling;
end;

{**************************************************************}
procedure TALEdit.SetReturnKeyType(const Value: TReturnKeyType);
begin
  EditControl.ReturnKeyType := Value;
end;

{************************************************}
function TALEdit.GetReturnKeyType: TReturnKeyType;
begin
  result := EditControl.ReturnKeyType;
end;

{***************************************************}
procedure TALEdit.SetMaxLength(const Value: integer);
begin
  EditControl.MaxLength := Value;
end;

{*************************************}
function TALEdit.GetMaxLength: integer;
begin
  result := EditControl.MaxLength;
end;

{******************************************************}
procedure TALEdit.OnChangeTrackingImpl(Sender: TObject);
begin
  {$IF defined(DEBUG)}
  if (csLoading in componentState) then raise Exception.Create('Error 923C6603-5C3D-45C3-BA6E-0D48843AACC3');
  {$ENDIF}
  if assigned(fOnChangeTracking) then
    fOnChangeTracking(self);
end;

{*************************************************}
procedure TALEdit.OnReturnKeyImpl(Sender: TObject);
begin
  {$IF defined(DEBUG)}
  if (csLoading in componentState) then raise Exception.Create('Error B1A3CE09-A6C4-44F5-92D1-E32112A7AAE2');
  {$ENDIF}
  if assigned(fOnReturnKey) then
    fOnReturnKey(self);
end;

{**********************************************************}
procedure TALEdit.SetOnReturnKey(const Value: TNotifyEvent);
begin
  fOnReturnKey := Value;
  if assigned(fOnReturnKey) then EditControl.onReturnKey := OnReturnKeyImpl
  else EditControl.onReturnKey := nil;
end;

{*********************************************}
procedure TALEdit.OnEnterImpl(Sender: TObject);
begin
  {$IF defined(DEBUG)}
  if (csLoading in componentState) then raise Exception.Create('Error 7438D305-07BC-4CD9-80E3-4E0FCAA8D446');
  {$ENDIF}
  if assigned(fOnEnter) then
    fOnEnter(self);
end;

{********************************************}
procedure TALEdit.OnExitImpl(Sender: TObject);
begin
  {$IF defined(DEBUG)}
  if (csLoading in componentState) then raise Exception.Create('Error B36DA0AF-D894-466B-85ED-BD1D7A85627D');
  {$ENDIF}
  if assigned(fOnExit) then
    fOnExit(self);
end;

{*******************************}
procedure TALEdit.PaddingChanged;
begin
  Inherited PaddingChanged;
  AdjustSize;
end;

{***********************************************}
procedure TALEdit.StrokeChanged(Sender: TObject);
begin
  inherited StrokeChanged(Sender);
  AdjustSize;
end;

{**********************************************}
procedure TALEdit.SetSides(const Value: TSides);
begin
  inherited SetSides(Value);
  AdjustSize;
end;

{************************************}
function TALEdit.GetCanFocus: Boolean;
begin
  {$IF defined(DEBUG)}
  ALLog('TALEdit.GetCanFocus', 'name: ' + Name, TalLogType.VERBOSE);
  {$ENDIF}
  result := inherited GetCanFocus;
  if result then begin
    {$IF defined(IOS)}
    // Instead of EditControl.SetFocus because when I do setFocus
    // of another TalEdit when one is already currently focused then the event
    // formkeyboadHidden and formKeyboardShow will be raised (and it's slow)
    EditControl.FTextField.SetFocus;
    {$ELSE}
    EditControl.SetFocus;
    {$ENDIF}
    // The canparentfocus is also set to false, so the TCommonCustomForm.NewFocusedControl(const Value: IControl)
    // will do nothing !
    exit(false);
  end;
end;

{****************************************}
function TALEdit.GetContainFocus: Boolean;
begin
  result := isFocused or EditControl.IsFocused;
end;

{*********************************************}
procedure TALEdit.FillChanged(Sender: TObject);
begin
  inherited FillChanged(Sender);
  if Fill.Kind = TBrushKind.Solid then
    EditControl.FillColor := Fill.Color
  else
    EditControl.FillColor := TalphaColors.Null;
end;

{**************************}
procedure TALEdit.DoResized;
begin
  inherited DoResized;
  AdjustSize;
end;

{***************************}
procedure TALEdit.AdjustSize;
begin
  if (not (csLoading in ComponentState)) and // loaded will call again AdjustSize
     (not (csDestroying in ComponentState)) then begin // if csDestroying do not do autosize

    var LStrokeSize := TRectF.Empty;
    if Stroke.Kind <> TbrushKind.None then begin
      if (TSide.Top in Sides) then    LStrokeSize.Top :=    max(Stroke.Thickness - Padding.top,    0);
      if (TSide.bottom in Sides) then LStrokeSize.bottom := max(Stroke.Thickness - Padding.bottom, 0);
      if (TSide.right in Sides) then  LStrokeSize.right :=  max(Stroke.Thickness - Padding.right,  0);
      if (TSide.left in Sides) then   LStrokeSize.left :=   max(Stroke.Thickness - Padding.left,   0);
    end;

    if (AutoSize) and // if AutoSize is false nothing to adjust
       (not (Align in [TAlignLayout.Client,
                       TAlignLayout.Contents,
                       TAlignLayout.Left,
                       TAlignLayout.Right,
                       TAlignLayout.MostLeft,
                       TAlignLayout.MostRight,
                       TAlignLayout.Vertical,
                       TAlignLayout.HorzCenter])) then begin // If aligned vertically nothing to adjust

      var LfontMetrics := ALGetFontMetrics(
                            TextSettings.Font.Family, // const AFontFamily: String;
                            TextSettings.Font.Size * ALGetScreenScale, // const AFontSize: single;
                            TextSettings.Font.Weight, // const AFontWeight: TFontWeight;
                            TextSettings.Font.Slant, // const AFontSlant: TFontSlant;
                            TextSettings.Font.Color, // const AFontColor: TalphaColor;
                            TextSettings.Decoration.Kinds); // const ADecorationKinds: TALTextDecorationKinds;

      SetBounds(
        Position.X,
        Position.Y,
        Width,
        -LfontMetrics.Ascent + LfontMetrics.Descent + LStrokeSize.Top + LStrokeSize.bottom + padding.Top + padding.Bottom);
    end;

    var LMarginRect := TRectF.Empty;

    {$IF defined(MSWINDOWS) or defined(ALMacOS)}
    // In Windows and MacOS, there is no way to align text vertically,
    // so we must center the EditControl.
    var LfontMetrics := ALGetFontMetrics(
                          TextSettings.Font.Family, // const AFontFamily: String;
                          TextSettings.Font.Size * ALGetScreenScale, // const AFontSize: single;
                          TextSettings.Font.Weight, // const AFontWeight: TFontWeight;
                          TextSettings.Font.Slant, // const AFontSlant: TFontSlant;
                          TextSettings.Font.Color, // const AFontColor: TalphaColor;
                          TextSettings.Decoration.Kinds); // const ADecorationKinds: TALTextDecorationKinds;

    Var LAvailableHeight := Height - LStrokeSize.Top - LStrokeSize.bottom - Padding.top - Padding.Bottom;
    var LEditControlHeight := -LfontMetrics.Ascent + LfontMetrics.Descent;
    {$IF defined(ALMacOS)}
    // For an obscure reason, when FocusRingType is set to NSFocusRingTypeNone and the font size
    // is not 16, focusing on the EditField causes it to shift up by 2 pixels.
    LEditControlHeight := LEditControlHeight + 2;
    {$ENDIF}

    LMarginRect.top := (LAvailableHeight - LEditControlHeight) / 2;
    LMarginRect.bottom := (LAvailableHeight - LEditControlHeight) / 2;
    {$ENDIF}

    LMarginRect.left :=   Max(LMarginRect.left   + LStrokeSize.left,   0);
    LMarginRect.Top :=    Max(LMarginRect.Top    + LStrokeSize.Top,    0);
    LMarginRect.Right :=  Max(LMarginRect.Right  + LStrokeSize.Right,  0);
    LMarginRect.Bottom := Max(LMarginRect.Bottom + LStrokeSize.Bottom, 0);

    EditControl.Margins.Rect := LMarginRect;

  end;
end;

{**************************************}
function TALEdit.HasNativeView: Boolean;
begin
  result := EditControl.HasNativeView;
end;

{******************************}
Procedure TALEdit.AddNativeView;
begin
  EditControl.AddNativeView;
end;

{*********************************}
Procedure TALEdit.RemoveNativeView;
begin
  EditControl.RemoveNativeView;
end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALEdit]);
end;

initialization
  RegisterFmxClasses([TALEdit]);

end.
