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
  Alcinoe.iOSapi.UIKit,
  Alcinoe.FMX.NativeView.iOS,
  {$ELSEIF defined(ALMacOS)}
  System.TypInfo,
  Macapi.Foundation,
  Macapi.AppKit,
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  Macapi.CocoaTypes,
  Alcinoe.Macapi.AppKit,
  Alcinoe.FMX.NativeView.Mac,
  {$ELSEIF defined(MSWINDOWS)}
  Winapi.Messages,
  Winapi.Windows,
  FMX.Controls.Win,
  FMX.Helpers.Win,
  Alcinoe.FMX.NativeView.Win,
  {$ENDIF}
  FMX.types,
  Fmx.controls,
  fmx.Graphics,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.Graphics;

Type

  {***************************}
  TALAutoCapitalizationType = (
    acNone, // Specifies that there is no automatic text capitalization.
    acWords, // Specifies automatic capitalization of the first letter of each word.
    acSentences, // Specifies automatic capitalization of the first letter of each sentence.
    acAllCharacters); // Specifies automatic capitalization of all characters, such as for entry of two-character state abbreviations for the United States.

  {************************************}
  TALBaseEditControl = class(TALControl)
  strict private
    fOnChange: TNotifyEvent;
    fOnReturnKey: TNotifyEvent;
    FTextSettings: TALBaseTextSettings;
  protected
    {$IF defined(android)}
    FNativeView: TALAndroidNativeView;
    Function CreateNativeView: TALAndroidNativeView; virtual; abstract;
    function GetNativeView: TALAndroidNativeView; virtual;
    {$ELSEIF defined(IOS)}
    FNativeView: TALIosNativeView;
    Function CreateNativeView: TALIosNativeView; virtual; abstract;
    function GetNativeView: TALIosNativeView; virtual;
    {$ELSEIF defined(ALMacOS)}
    FNativeView: TALMacNativeView;
    Function CreateNativeView: TALMacNativeView; virtual; abstract;
    function GetNativeView: TALMacNativeView; virtual;
    {$ELSEIF defined(MSWindows)}
    FNativeView: TALWinNativeView;
    Function CreateNativeView: TALWinNativeView; virtual; abstract;
    function GetNativeView: TALWinNativeView; virtual;
    {$ENDIF}
    function GetKeyboardType: TVirtualKeyboardType; virtual; abstract;
    procedure setKeyboardType(const Value: TVirtualKeyboardType); virtual; abstract;
    function GetAutoCapitalizationType: TALAutoCapitalizationType; virtual; abstract;
    procedure setAutoCapitalizationType(const Value: TALAutoCapitalizationType); virtual; abstract;
    function GetPassword: Boolean; virtual; abstract;
    procedure setPassword(const Value: Boolean); virtual; abstract;
    function GetCheckSpelling: Boolean; virtual; abstract;
    procedure setCheckSpelling(const Value: Boolean); virtual; abstract;
    function GetReturnKeyType: TReturnKeyType; virtual; abstract;
    procedure setReturnKeyType(const Value: TReturnKeyType); virtual; abstract;
    function GetPromptText: String; virtual; abstract;
    procedure setPromptText(const Value: String); virtual; abstract;
    function GetPromptTextColor: TAlphaColor; virtual; abstract;
    procedure setPromptTextColor(const Value: TAlphaColor); virtual; abstract;
    function GetTintColor: TAlphaColor; virtual; abstract;
    procedure setTintColor(const Value: TAlphaColor); virtual; abstract;
    function GetFillColor: TAlphaColor; virtual; abstract;
    procedure SetFillColor(const Value: TAlphaColor); virtual; abstract;
    procedure SetTextSettings(const Value: TALBaseTextSettings); virtual;
    procedure TextSettingsChanged(Sender: TObject); virtual; abstract;
    function getText: String; virtual; abstract;
    procedure SetText(const Value: String); virtual; abstract;
    function GetMaxLength: integer; virtual; abstract;
    procedure SetMaxLength(const Value: integer); virtual; abstract;
    procedure DoChange; virtual;
    procedure DoReturnKey; virtual;
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure AncestorParentChanged; override;
    procedure ParentChanged; override;
    procedure DoAbsoluteChanged; override;
    procedure DoRootChanged; override;
    procedure Resize; override;
    procedure VisibleChanged; override;
    procedure ChangeOrder; override;
    procedure DoEndUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RecalcOpacity; override;
    procedure RecalcEnabled; override;
    function getLineCount: integer; virtual;
    function getLineHeight: Single; virtual; abstract; // It includes the line spacing
    function HasNativeView: boolean; virtual;
    Procedure AddNativeView; virtual;
    Procedure RemoveNativeView; virtual;
    {$IF defined(android)}
    property NativeView: TALAndroidNativeView read GetNativeView;
    {$ELSEIF defined(IOS)}
    property NativeView: TALIosNativeView read GetNativeView;
    {$ELSEIF defined(ALMacOS)}
    property NativeView: TALMacNativeView read GetNativeView;
    {$ELSEIF defined(MSWindows)}
    property NativeView: TALWinNativeView read GetNativeView;
    {$ENDIF}
    Procedure SetSelection(const AStart: integer; const AStop: Integer); overload; virtual; abstract;
    Procedure SetSelection(const AIndex: integer); overload; virtual; abstract;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnReturnKey: TNotifyEvent read fOnReturnKey write fOnReturnKey;
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType;
    property AutoCapitalizationType: TALAutoCapitalizationType read GetAutoCapitalizationType write SetAutoCapitalizationType;
    property Password: Boolean read GetPassword write SetPassword;
    property PromptText: String read GetPromptText write setPromptText;
    property PromptTextColor: TAlphaColor read GetPromptTextColor write setPromptTextColor; // Null mean use the default color
    property TintColor: TalphaColor read GetTintColor write SetTintColor;
    property FillColor: TAlphaColor read GetFillColor write SetFillColor;
    property MaxLength: integer read GetMaxLength write SetMaxLength;
    property Text: String read getText write SetText;
    property TextSettings: TALBaseTextSettings read FTextSettings write SetTextSettings;
    property CheckSpelling: Boolean read GetCheckSpelling write SetCheckSpelling;
  end;

{$REGION ' ANDROID'}
{$IF defined(android)}
type

  {****************************}
  TALAndroidEditControl = class;

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

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TALTouchListener = class(TJavaLocal, JView_OnTouchListener)
      private
        FEditText: TALAndroidEditText;
      public
        constructor Create(const aEditText: TALAndroidEditText);
        function onTouch(v: JView; event: JMotionEvent): Boolean; cdecl;
      end;

  private
    fIsMultiline: boolean;
    fDefStyleAttr: String;
    fDefStyleRes: String;
    FTextWatcher: TALTextWatcher;
    FEditorActionListener: TALEditorActionListener;
    FKeyPreImeListener: TALKeyPreImeListener;
    FTouchListener: TALTouchListener;
    FEditControl: TALAndroidEditControl;
    function GetView: JALEditText;
  protected
    function CreateView: JView; override;
    procedure InitView; override;
  public
    constructor Create(const AControl: TALAndroidEditControl; Const aIsMultiline: Boolean = False; const aDefStyleAttr: String = ''; const aDefStyleRes: String = ''); reintroduce;
    destructor Destroy; override;
    property View: JALEditText read GetView;
  end;

  {***********************************************}
  TALAndroidEditControl = class(TALBaseEditControl)
  private
    FFillColor: TAlphaColor;
    fMaxLength: integer;
    fReturnKeyType: TReturnKeyType;
    fKeyboardType: TVirtualKeyboardType;
    fAutoCapitalizationType: TALAutoCapitalizationType;
    fPassword: boolean;
    fCheckSpelling: boolean;
    FDefStyleAttr: String;
    FDefStyleRes: String;
    fIsMultiline: Boolean;
    fTintColor: TalphaColor;
    procedure ApplicationEventHandler(const Sender: TObject; const M : TMessage);
  protected
    procedure DoSetInputType(
                const aKeyboardType: TVirtualKeyboardType;
                const aAutoCapitalizationType: TALAutoCapitalizationType;
                const aPassword: Boolean;
                const aCheckSpelling: Boolean;
                const aIsMultiline: Boolean); virtual;
    procedure DoSetReturnKeyType(const aReturnKeyType: TReturnKeyType); virtual;
    Function CreateNativeView: TALAndroidNativeView; override;
    function GetNativeView: TALAndroidEditText; reintroduce; virtual;
    function GetKeyboardType: TVirtualKeyboardType; override;
    procedure setKeyboardType(const Value: TVirtualKeyboardType); override;
    function GetAutoCapitalizationType: TALAutoCapitalizationType; override;
    procedure setAutoCapitalizationType(const Value: TALAutoCapitalizationType); override;
    function GetPassword: Boolean; override;
    procedure setPassword(const Value: Boolean); override;
    function GetCheckSpelling: Boolean; override;
    procedure setCheckSpelling(const Value: Boolean); override;
    function GetReturnKeyType: TReturnKeyType; override;
    procedure setReturnKeyType(const Value: TReturnKeyType); override;
    function GetPromptText: String; override;
    procedure setPromptText(const Value: String); override;
    function GetPromptTextColor: TAlphaColor; override;
    procedure setPromptTextColor(const Value: TAlphaColor); override;
    function GetTintColor: TAlphaColor; override;
    procedure setTintColor(const Value: TAlphaColor); override;
    function GetFillColor: TAlphaColor; override;
    procedure SetFillColor(const Value: TAlphaColor); override;
    procedure TextSettingsChanged(Sender: TObject); override;
    function getText: String; override;
    procedure SetText(const Value: String); override;
    function GetMaxLength: integer; override;
    procedure SetMaxLength(const Value: integer); override;
  public
    constructor Create(const AOwner: TComponent; Const AIsMultiline: Boolean = False; const ADefStyleAttr: String = ''; const ADefStyleRes: String = ''); reintroduce; virtual;
    destructor Destroy; override;
    function getLineHeight: Single; override; // It includes the line spacing
    property NativeView: TALAndroidEditText read GetNativeView;
    Procedure SetSelection(const AStart: integer; const AStop: Integer); overload; override;
    Procedure SetSelection(const AIndex: integer); overload; override;
  end;

{$endif}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}
type

  {************************}
  TALIosEditControl = class;

  {******************************************}
  IALIosEditTextField = interface(UITextField)
    ['{E4E67240-F15A-444F-8CE1-A3A830C023E8}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure ControlEventEditingChanged; cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
  end;

  {*******************************************}
  TALIosEditTextField = class(TALIosNativeView)
  private
    FEditControl: TALIosEditControl;
    function GetView: UITextField;
    function ExtractFirstTouchPoint(touches: NSSet): TPointF;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure SetEnabled(const value: Boolean); override;
  public
    constructor Create; overload; override;
    constructor Create(const AControl: TControl); overload; override;
    destructor Destroy; override;
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure ControlEventEditingChanged; cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
    property View: UITextField read GetView;
  end;

  {****************************************************************}
  TALIosEditTextFieldDelegate = class(TOCLocal, UITextFieldDelegate)
  private
    FEditControl: TALIosEditControl;
  public
    constructor Create(const AEditControl: TALIosEditControl);
    // Better name would be textFieldShouldChangeCharactersInRange
    function textField(textField: UITextField; shouldChangeCharactersInRange: NSRange; replacementString: NSString): Boolean; cdecl;
    procedure textFieldDidBeginEditing(textField: UITextField); cdecl;
    procedure textFieldDidEndEditing(textField: UITextField); cdecl;
    function textFieldShouldBeginEditing(textField: UITextField): Boolean; cdecl;
    function textFieldShouldClear(textField: UITextField): Boolean; cdecl;
    function textFieldShouldEndEditing(textField: UITextField): Boolean; cdecl;
    function textFieldShouldReturn(textField: UITextField): Boolean; cdecl;
  end;

  {*******************************************}
  TALIosEditControl = class(TALBaseEditControl)
  private
    FTextFieldDelegate: TALIosEditTextFieldDelegate;
    FFillColor: TAlphaColor;
    fMaxLength: integer;
    fPromptTextColor: TalphaColor;
  protected
    procedure applyPromptTextWithColor(const aStr: String; const aColor: TAlphaColor); virtual;
    Function CreateNativeView: TALIosNativeView; override;
    function GetNativeView: TALIosEditTextField; reintroduce; virtual;
    function GetKeyboardType: TVirtualKeyboardType; override;
    procedure setKeyboardType(const Value: TVirtualKeyboardType); override;
    function GetAutoCapitalizationType: TALAutoCapitalizationType; override;
    procedure setAutoCapitalizationType(const Value: TALAutoCapitalizationType); override;
    function GetPassword: Boolean; override;
    procedure setPassword(const Value: Boolean); override;
    function GetCheckSpelling: Boolean; override;
    procedure setCheckSpelling(const Value: Boolean); override;
    function GetReturnKeyType: TReturnKeyType; override;
    procedure setReturnKeyType(const Value: TReturnKeyType); override;
    function GetPromptText: String; override;
    procedure setPromptText(const Value: String); override;
    function GetPromptTextColor: TAlphaColor; override;
    procedure setPromptTextColor(const Value: TAlphaColor); override;
    function GetTintColor: TAlphaColor; override;
    procedure setTintColor(const Value: TAlphaColor); override;
    function GetFillColor: TAlphaColor; override;
    procedure SetFillColor(const Value: TAlphaColor); override;
    procedure TextSettingsChanged(Sender: TObject); override;
    function getText: String; override;
    procedure SetText(const Value: String); override;
    function GetMaxLength: integer; override;
    procedure SetMaxLength(const Value: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function getLineHeight: Single; override; // It includes the line spacing
    property NativeView: TALIosEditTextField read GetNativeView;
    Procedure SetSelection(const AStart: integer; const AStop: Integer); overload; override;
    Procedure SetSelection(const AIndex: integer); overload; override;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}
type

  {************************}
  TALMacEditControl = class;

  {******************************************}
  IALMacEditTextField = interface(NSTextField)
    ['{8FA87FC0-77FB-4451-8F92-52EE1AFDD94C}']
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
  end;

  {*******************************************}
  TALMacEditTextField = class(TALMacNativeView)
  private
    FEditControl: TALMacEditControl;
    function GetView: NSTextField;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure SetEnabled(const value: Boolean); override;
  public
    constructor Create; overload; override;
    constructor Create(const AControl: TControl); overload; override;
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
    property View: NSTextField read GetView;
  end;

  {***********************************************************************************************}
  TALMacEditTextFieldDelegate = class(TOCLocal, Alcinoe.Macapi.AppKit.NSControlTextEditingDelegate)
  private
    FEditControl: TALMacEditControl;
  public
    constructor Create(const AEditControl: TALMacEditControl);
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

  {*******************************************}
  TALMacEditControl = class(TALBaseEditControl)
  private
    FTextFieldDelegate: TALMacEditTextFieldDelegate;
    FFillColor: TAlphaColor;
    fMaxLength: integer;
    fReturnKeyType: TReturnKeyType;
    fKeyboardType: TVirtualKeyboardType;
    fAutoCapitalizationType: TALAutoCapitalizationType;
    fPassword: boolean;
    fCheckSpelling: boolean;
    fPromptTextColor: TalphaColor;
    fTintColor: TalphaColor;
  protected
    procedure applyPromptTextWithColor(const aStr: String; const aColor: TAlphaColor); virtual;
    Function CreateNativeView: TALMacNativeView; override;
    function GetNativeView: TALMacEditTextField; reintroduce; virtual;
    function GetKeyboardType: TVirtualKeyboardType; override;
    procedure setKeyboardType(const Value: TVirtualKeyboardType); override;
    function GetAutoCapitalizationType: TALAutoCapitalizationType; override;
    procedure setAutoCapitalizationType(const Value: TALAutoCapitalizationType); override;
    function GetPassword: Boolean; override;
    procedure setPassword(const Value: Boolean); override;
    function GetCheckSpelling: Boolean; override;
    procedure setCheckSpelling(const Value: Boolean); override;
    function GetReturnKeyType: TReturnKeyType; override;
    procedure setReturnKeyType(const Value: TReturnKeyType); override;
    function GetPromptText: String; override;
    procedure setPromptText(const Value: String); override;
    function GetPromptTextColor: TAlphaColor; override;
    procedure setPromptTextColor(const Value: TAlphaColor); override;
    function GetTintColor: TAlphaColor; override;
    procedure setTintColor(const Value: TAlphaColor); override;
    function GetFillColor: TAlphaColor; override;
    procedure SetFillColor(const Value: TAlphaColor); override;
    procedure TextSettingsChanged(Sender: TObject); override;
    function getText: String; override;
    procedure SetText(const Value: String); override;
    function GetMaxLength: integer; override;
    procedure SetMaxLength(const Value: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function getLineHeight: Single; override; // It includes the line spacing
    property NativeView: TALMacEditTextField read GetNativeView;
    Procedure SetSelection(const AStart: integer; const AStop: Integer); overload; override;
    Procedure SetSelection(const AIndex: integer); overload; override;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}
type

  {************************}
  TALWinEditControl = class;

  {**************************************}
  TALWinEditView = class(TALWinNativeView)
  private
    FFontHandle: HFONT;
    FBackgroundBrush: HBRUSH;
    FEditControl: TALWinEditControl;
    {$IF not defined(ALDPK)}
    procedure UpdateFontHandle;
    procedure UpdateBackgroundBrush;
    {$ENDIF}
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMSetText(var Message: TWMSetText); message WM_SETTEXT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMClear(var Message: TWMClear); message WM_CLEAR;
    procedure WMUndo(var Message: TWMUndo); message WM_UNDO;
    procedure WMTextColor(var Message: WinApi.Messages.TMessage); message CN_CTLCOLOREDIT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(const AControl: TControl); override;
    destructor Destroy; override;
    property EditControl: TALWinEditControl read FEditControl;
  end;

  {*******************************************}
  TALWinEditControl = class(TALBaseEditControl)
  private
    FFillColor: TAlphaColor;
    FPromptText: String;
    FPromptTextColor: TalphaColor;
    fReturnKeyType: TReturnKeyType;
    fKeyboardType: TVirtualKeyboardType;
    fAutoCapitalizationType: TALAutoCapitalizationType;
    fCheckSpelling: boolean;
    fTintColor: TalphaColor;
    {$IF defined(ALDPK)}
    FPassword: Boolean;
    FMaxLength: Integer;
    FText: String;
    {$ENDIF}
  protected
    Function CreateNativeView: TALWinNativeView; override;
    function GetNativeView: TALWinEditView; reintroduce; virtual;
    function GetKeyboardType: TVirtualKeyboardType; override;
    procedure setKeyboardType(const Value: TVirtualKeyboardType); override;
    function GetAutoCapitalizationType: TALAutoCapitalizationType; override;
    procedure setAutoCapitalizationType(const Value: TALAutoCapitalizationType); override;
    function GetPassword: Boolean; override;
    procedure setPassword(const Value: Boolean); override;
    function GetCheckSpelling: Boolean; override;
    procedure setCheckSpelling(const Value: Boolean); override;
    function GetReturnKeyType: TReturnKeyType; override;
    procedure setReturnKeyType(const Value: TReturnKeyType); override;
    function GetPromptText: String; override;
    procedure setPromptText(const Value: String); override;
    function GetPromptTextColor: TAlphaColor; override;
    procedure setPromptTextColor(const Value: TAlphaColor); override;
    function GetTintColor: TAlphaColor; override;
    procedure setTintColor(const Value: TAlphaColor); override;
    function GetFillColor: TAlphaColor; override;
    procedure SetFillColor(const Value: TAlphaColor); override;
    procedure TextSettingsChanged(Sender: TObject); override;
    function getText: String; override;
    procedure SetText(const Value: String); override;
    function GetMaxLength: integer; override;
    procedure SetMaxLength(const Value: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    function getLineHeight: Single; override; // It includes the line spacing
    property NativeView: TALWinEditView read GetNativeView;
    Procedure SetSelection(const AStart: integer; const AStop: Integer); overload; override;
    Procedure SetSelection(const AIndex: integer); overload; override;
  end;

{$endif}
{$ENDREGION}

type

  {*******************************************************************************************************}
  TALBaseEdit = class(TALBaseRectangle, IVirtualKeyboardControl, IControlTypeSupportable, IALNativeControl)
  public
    type
      // -------
      // TStroke
      TStroke = class(TALStrokeBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      // -------------
      // TTextSettings
      TTextSettings = class(TALBaseTextSettings)
      public
        Type
          TFont = Class(TALFont)
          protected
            function GetDefaultSize: Single; override;
          End;
      protected
        function CreateFont: TALFont; override;
      published
        property Font;
        property HorzAlign;
      end;
      // ----------------
      // TLabelTextLayout
      TLabelTextLayout = (Floating, &Inline);
      // ----------------
      // TLabelTextAnimation
      TLabelTextAnimation = (Translation, Opacity);
      // ------------------
      // TLabelTextSettings
      TLabelTextSettings = class(TALBaseTextSettings)
      public
        Type
          TMargins = class(TALBounds)
          protected
            function GetDefaultValue: TRectF; override;
          end;
          TFont = Class(TALFont)
          protected
            function GetDefaultSize: Single; override;
          End;
      private
        FMargins: TALBounds;
        FLayout: TLabelTextLayout;
        FAnimation: TLabelTextAnimation;
        procedure SetMargins(const Value: TALBounds);
        procedure SetLayout(const Value: TLabelTextLayout);
        procedure SetAnimation(const Value: TLabelTextAnimation);
        procedure MarginsChanged(Sender: TObject);
      protected
        function CreateMargins: TALBounds; virtual;
        function CreateFont: TALFont; override;
      public
        constructor Create; override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
      published
        property Font;
        property Ellipsis;
        property MaxLines;
        property IsHtml;
        property Trimming;
        property LineHeightMultiplier;
        property LetterSpacing;
        property Margins: TALBounds read FMargins write SetMargins;
        property Layout: TLabelTextLayout read FLayout write SetLayout default TLabelTextLayout.Floating;
        property Animation: TLabelTextAnimation read FAnimation write SetAnimation default TLabelTextAnimation.Translation;
      end;
      // ---------------------
      // TSupportingTextLayout
      TSupportingTextLayout = (Floating, &Inline);
      // -----------------------
      // TSupportingTextSettings
      TSupportingTextSettings = class(TALBaseTextSettings)
      public
        Type
          TMargins = class(TALBounds)
          protected
            function GetDefaultValue: TRectF; override;
          end;
          TFont = Class(TALFont)
          protected
            function GetDefaultSize: Single; override;
          End;
      private
        FMargins: TALBounds;
        FLayout: TSupportingTextLayout;
        procedure SetMargins(const Value: TALBounds);
        procedure SetLayout(const Value: TSupportingTextLayout);
        procedure MarginsChanged(Sender: TObject);
      protected
        function CreateMargins: TALBounds; virtual;
        function CreateFont: TALFont; override;
      public
        constructor Create; override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
      published
        property Font;
        property Ellipsis;
        property MaxLines;
        property IsHtml;
        property Trimming;
        property LineHeightMultiplier;
        property LetterSpacing;
        property Margins: TALBounds read FMargins write SetMargins;
        property Layout: TSupportingTextLayout read FLayout write SetLayout default TSupportingTextLayout.Floating;
      end;
      // ---------------
      // TBaseStateStyle
      TBaseStateStyle = class(TALBaseStateStyle)
      public
        type
          TStroke = class(TALInheritStrokeBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          TTextSettings = class(TALInheritBaseTextSettings)
          public
            type
              TFont = Class(TALFont)
              protected
                function GetDefaultSize: Single; override;
              End;
          protected
            function CreateFont: TALFont; override;
          published
            property Font;
          end;
          TLabelTextSettings = class(TALInheritBaseTextSettings)
          public
            type
              TFont = Class(TALFont)
              protected
                function GetDefaultSize: Single; override;
              End;
          protected
            function CreateFont: TALFont; override;
          published
            property Font;
          end;
          TSupportingTextSettings = class(TALInheritBaseTextSettings)
          public
            type
              TFont = Class(TALFont)
              protected
                function GetDefaultSize: Single; override;
              End;
          protected
            function CreateFont: TALFont; override;
          published
            property Font;
          end;
      private
        FPromptTextColor: TalphaColor;
        FTintColor: TalphaColor;
        FTextSettings: TBaseStateStyle.TTextSettings;
        FLabelTextSettings: TBaseStateStyle.TLabelTextSettings;
        FSupportingTextSettings: TBaseStateStyle.TSupportingTextSettings;
        FPriorSupersedePromptTextColor: TalphaColor;
        FPriorSupersedeTintColor: TalphaColor;
        function GetStateStyleParent: TBaseStateStyle;
        function GetControlParent: TALBaseEdit;
        procedure SetPromptTextColor(const AValue: TAlphaColor);
        procedure SetTintColor(const AValue: TAlphaColor);
        procedure SetTextSettings(const AValue: TBaseStateStyle.TTextSettings);
        procedure SetLabelTextSettings(const AValue: TBaseStateStyle.TLabelTextSettings);
        procedure SetSupportingTextSettings(const AValue: TBaseStateStyle.TSupportingTextSettings);
        procedure TextSettingsChanged(ASender: TObject);
        procedure LabelTextSettingsChanged(ASender: TObject);
        procedure SupportingTextSettingsChanged(ASender: TObject);
        function IsPromptTextColorStored: Boolean;
        function IsTintColorStored: Boolean;
      protected
        function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
        function CreateTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TTextSettings; virtual;
        function CreateLabelTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TLabelTextSettings; virtual;
        function CreateSupportingTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TSupportingTextSettings; virtual;
        function GetDefaultPromptTextColor: TalphaColor; virtual;
        function GetDefaultTintColor: TalphaColor; virtual;
        function GetInherit: Boolean; override;
        procedure DoSupersede; override;
      public
        BufPromptTextDrawable: TALDrawable;
        BufPromptTextDrawableRect: TRectF;
        BufLabelTextDrawable: TALDrawable;
        BufLabelTextDrawableRect: TRectF;
        BufSupportingTextDrawable: TALDrawable;
        BufSupportingTextDrawableRect: TRectF;
        procedure ClearBufDrawable; override;
        procedure ClearBufPromptTextDrawable;
        procedure ClearBufLabelTextDrawable;
        procedure ClearBufSupportingTextDrawable;
      public
        constructor Create(const AParent: TObject); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single); override;
        property StateStyleParent: TBaseStateStyle read GetStateStyleParent;
        property ControlParent: TALBaseEdit read GetControlParent;
        property DefaultPromptTextColor: TalphaColor read GetDefaultPromptTextColor;
        property DefaultTintColor: TalphaColor read GetDefaultTintColor;
      published
        property LabelTextSettings: TBaseStateStyle.TLabelTextSettings read fLabelTextSettings write SetLabelTextSettings;
        property PromptTextColor: TAlphaColor read FPromptTextColor write SetPromptTextColor stored IsPromptTextColorStored;
        property SupportingTextSettings: TBaseStateStyle.TSupportingTextSettings read fSupportingTextSettings write SetSupportingTextSettings;
        property TextSettings: TBaseStateStyle.TTextSettings read fTextSettings write SetTextSettings;
        property TintColor: TAlphaColor read FTintColor write SetTintColor stored IsTintColorStored;
      end;
      // -------------------
      // TDisabledStateStyle
      TDisabledStateStyle = class(TBaseStateStyle)
      private
        FOpacity: Single;
        procedure SetOpacity(const Value: Single);
        function IsOpacityStored: Boolean;
      protected
        function GetInherit: Boolean; override;
      public
        constructor Create(const AParent: TObject); override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
      published
        property Fill;
        property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
        property Shadow;
        property Stroke;
      end;
      // ------------------
      // THoveredStateStyle
      THoveredStateStyle = class(TBaseStateStyle)
      published
        property Fill;
        property Shadow;
        property StateLayer;
        property Stroke;
      end;
      // ------------------
      // TFocusedStateStyle
      TFocusedStateStyle = class(TBaseStateStyle)
      published
        property Fill;
        property Shadow;
        property StateLayer;
        property Stroke;
      end;
      // ------------
      // TStateStyles
      TStateStyles = class(TALBaseStateStyles)
      private
        FDisabled: TDisabledStateStyle;
        FHovered: THoveredStateStyle;
        FFocused: TFocusedStateStyle;
        function GetParent: TALBaseEdit;
        procedure SetDisabled(const AValue: TDisabledStateStyle);
        procedure SetHovered(const AValue: THoveredStateStyle);
        procedure SetFocused(const AValue: TFocusedStateStyle);
        procedure DisabledChanged(ASender: TObject);
        procedure HoveredChanged(ASender: TObject);
        procedure FocusedChanged(ASender: TObject);
      protected
        function CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle; virtual;
        function CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle; virtual;
        function CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle; virtual;
        procedure StartTransition; override;
        procedure TransitionAnimationProcess(Sender: TObject); override;
        procedure TransitionAnimationFinish(Sender: TObject); override;
      public
        constructor Create(const AParent: TALControl); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure ClearBufDrawable; override;
        procedure ClearBufPromptTextDrawable;
        procedure ClearBufLabelTextDrawable;
        procedure ClearBufSupportingTextDrawable;
        function GetCurrentRawStyle: TALBaseStateStyle; override;
        Property Parent: TALBaseEdit read GetParent;
      published
        property Disabled: TDisabledStateStyle read FDisabled write SetDisabled;
        property Hovered: THoveredStateStyle read FHovered write SetHovered;
        property Focused: TFocusedStateStyle read FFocused write SetFocused;
        property Transition;
      end;
  private
    fDefStyleAttr: String;
    fDefStyleRes: String;
    FAutoTranslate: Boolean;
    fOnChange: TNotifyEvent;
    fOnReturnKey: TNotifyEvent;
    FTextSettings: TTextSettings;
    FPromptText: String;
    FPromptTextColor: TAlphaColor;
    FTintcolor: TAlphaColor;
    FLabelText: String;
    FLabelTextSettings: TLabelTextSettings;
    FlabelTextAnimation: TALFloatAnimation;
    FSupportingText: String;
    FSupportingTextSettings: TSupportingTextSettings;
    FSupportingTextMarginBottomUpdated: Boolean;
    {$IF defined(ALDPK)}
    FPrevStateStyles: TStateStyles;
    {$ENDIF}
    FStateStyles: TStateStyles;
    FIsTextEmpty: Boolean;
    FNativeViewRemoved: Boolean;
    fEditControl: TALBaseEditControl;
    //--
    fBufPromptTextDrawable: TALDrawable;
    fBufPromptTextDrawableRect: TRectF;
    fBufLabelTextDrawable: TALDrawable;
    fBufLabelTextDrawableRect: TRectF;
    fBufSupportingTextDrawable: TALDrawable;
    fBufSupportingTextDrawableRect: TRectF;
    //--
    procedure UpdateEditControlPromptText;
    procedure UpdateNativeViewVisibility;
    function GetPromptText: String;
    procedure setPromptText(const Value: String);
    function GetPromptTextColor: TAlphaColor;
    procedure setPromptTextColor(const Value: TAlphaColor);
    procedure setLabelText(const Value: String);
    procedure setSupportingText(const Value: String);
    function GetTintColor: TAlphaColor;
    procedure setTintColor(const Value: TAlphaColor);
    procedure SetTextSettings(const Value: TTextSettings);
    procedure SetLabelTextSettings(const Value: TLabelTextSettings);
    procedure SetSupportingTextSettings(const Value: TSupportingTextSettings);
    procedure SetStateStyles(const AValue: TStateStyles);
    function getText: String;
    procedure SetText(const Value: String);
    function GetIsTextEmpty: Boolean; inline;
    procedure OnChangeImpl(Sender: TObject);
    procedure OnReturnKeyImpl(Sender: TObject);
    procedure SetOnReturnKey(const Value: TNotifyEvent);
    procedure setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
    function GetAutoCapitalizationType: TALAutoCapitalizationType;
    procedure SetPassword(const Value: Boolean);
    function GetPassword: Boolean;
    procedure SetCheckSpelling(const Value: Boolean);
    function GetCheckSpelling: Boolean;
    procedure SetDefStyleAttr(const Value: String);
    procedure SetDefStyleRes(const Value: String);
    procedure SetMaxLength(const Value: integer);
    function GetMaxLength: integer;
    procedure LabelTextAnimationProcess(Sender: TObject);
    procedure LabelTextAnimationFinish(Sender: TObject);
    function HasOpacityLabelTextAnimation: Boolean;
    function HasTranslationLabelTextAnimation: Boolean;
    procedure UpdateEditControlStyle;
    { IVirtualKeyboardControl }
    procedure SetKeyboardType(Value: TVirtualKeyboardType);
    function GetKeyboardType: TVirtualKeyboardType;
    procedure SetReturnKeyType(Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
    function IVirtualKeyboardControl.IsPassword = GetPassword;
    { IControlTypeSupportable }
    function GetControlType: TControlType;
    procedure SetControlType(const Value: TControlType);
  protected
    function CreateStroke: TALStrokeBrush; override;
    function CreateTextSettings: TTextSettings; virtual;
    function CreateLabelTextSettings: TLabelTextSettings; virtual;
    function CreateSupportingTextSettings: TSupportingTextSettings; virtual;
    function CreateStateStyles: TStateStyles; virtual;
    function CreateEditControl: TALBaseEditControl; virtual;
    function GetEditControl: TALBaseEditControl; virtual;
    property EditControl: TALBaseEditControl read GetEditControl;
    {$IF defined(android)}
    function GetNativeView: TALAndroidNativeView; virtual;
    {$ELSEIF defined(IOS)}
    function GetNativeView: TALIosNativeView; virtual;
    {$ELSEIF defined(ALMacOS)}
    function GetNativeView: TALMacNativeView; virtual;
    {$ELSEIF defined(MSWindows)}
    function GetNativeView: TALWinNativeView; virtual;
    {$ENDIF}
    procedure InitEditControl; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure IsMouseOverChanged; override;
    function GetDefaultSize: TSizeF; override;
    procedure Loaded; override;
    procedure SetXRadius(const Value: Single); override;
    procedure SetYRadius(const Value: Single); override;
    procedure TextSettingsChanged(Sender: TObject); virtual;
    procedure LabelTextSettingsChanged(Sender: TObject); virtual;
    procedure SupportingTextSettingsChanged(Sender: TObject); virtual;
    procedure StateStylesChanged(Sender: TObject); virtual;
    procedure EnabledChanged; override;
    procedure PaddingChanged; override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure SetSides(const Value: TSides); override;
    procedure FillChanged(Sender: TObject); override;
    {$IF NOT DEFINED(ALSkiaCanvas)}
    function GetRenderTargetRect(const ARect: TrectF): TRectF; override;
    {$ENDIF}
    procedure Paint; override;
    Procedure CreateBufPromptTextDrawable(
                var ABufDrawable: TALDrawable;
                var ABufDrawableRect: TRectF;
                const AText: String;
                const AFont: TALFont);
    Procedure CreateBufLabelTextDrawable(
                var ABufDrawable: TALDrawable;
                var ABufDrawableRect: TRectF;
                const AText: String;
                const AFont: TALFont);
    Procedure CreateBufSupportingTextDrawable(
                var ABufDrawable: TALDrawable;
                var ABufDrawableRect: TRectF;
                const AText: String;
                const AFont: TALFont);
    property BufPromptTextDrawable: TALDrawable read fBufPromptTextDrawable;
    property BufPromptTextDrawableRect: TRectF read fBufPromptTextDrawableRect;
    property BufLabelTextDrawable: TALDrawable read fBufLabelTextDrawable;
    property BufLabelTextDrawableRect: TRectF read fBufLabelTextDrawableRect;
    property BufSupportingTextDrawable: TALDrawable read fBufSupportingTextDrawable;
    property BufSupportingTextDrawableRect: TRectF read fBufSupportingTextDrawableRect;
  {$IF defined(ALBackwardCompatible)}
  private
    procedure ReadtextPrompt(Reader: TReader);
    procedure ReadtextPromptColor(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AlignToPixel; override;
    {$IF defined(android)}
    property NativeView: TALAndroidNativeView read GetNativeView;
    {$ELSEIF defined(IOS)}
    property NativeView: TALIosNativeView read GetNativeView;
    {$ELSEIF defined(ALMacOS)}
    property NativeView: TALMacNativeView read GetNativeView;
    {$ELSEIF defined(MSWindows)}
    property NativeView: TALWinNativeView read GetNativeView;
    {$ENDIF}
    function HasNativeView: boolean;
    Procedure AddNativeView;
    Procedure RemoveNativeView;
    Procedure SetSelection(const AStart: integer; const AStop: Integer); overload; virtual;
    Procedure SetSelection(const AIndex: integer); overload; virtual;
    function getLineCount: integer;
    function getLineHeight: Single; // It includes the line spacing
    procedure MakeBufDrawable; override;
    procedure MakeBufPromptTextDrawable; virtual;
    procedure MakeBufLabelTextDrawable; virtual;
    procedure MakeBufSupportingTextDrawable; virtual;
    procedure ClearBufDrawable; override;
    procedure ClearBufPromptTextDrawable; virtual;
    procedure ClearBufLabelTextDrawable; virtual;
    procedure ClearBufSupportingTextDrawable; virtual;
    property Password: Boolean read GetPassword write SetPassword default False;
  published
    // Android only - the name of an attribute in the current theme that contains a reference to
    // a style resource that supplies defaults style values
    // Exemple of use: https://stackoverflow.com/questions/5051753/how-do-i-apply-a-style-programmatically
    // NOTE: !!IMPORTANT!! This properties must be defined the very first because the stream system must load it the very first
    property DefStyleAttr: String read fDefStyleAttr write SetDefStyleAttr;
    // Android only - the name of a style resource that supplies default style values
    // NOTE: !!IMPORTANT!! This properties must be defined the very first because the stream system must load it the very first
    property DefStyleRes: String read fDefStyleRes write SetDefStyleRes;
    //property Action;
    property Align;
    property Anchors;
    property AutoCapitalizationType: TALAutoCapitalizationType read GetAutoCapitalizationType write SetAutoCapitalizationType default TALAutoCapitalizationType.acNone;
    //property AutoSize;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate default true; // Just the PromptText
    property CanFocus default True;
    //property CanParentFocus;
    //property Caret;
    property CheckSpelling: Boolean read GetCheckSpelling write SetCheckSpelling default true;
    property ClipChildren;
    //property ClipParent;
    property Corners;
    property Cursor default crIBeam;
    //property DisableFocusEffect;
    //property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Fill;
    //property FilterChar;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType default TVirtualKeyboardType.Default;
    //property KillFocusByReturn; => always true
    property LabelText: String read FLabelText write SetLabelText;
    property LabelTextSettings: TLabelTextSettings read FLabelTextSettings write SetLabelTextSettings;
    property Locked;
    property Margins;
    property MaxLength: integer read GetMaxLength write SetMaxLength default 0;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property PromptText: String read GetPromptText write setPromptText;
    property PromptTextColor: TAlphaColor read GetPromptTextColor write setPromptTextColor default TalphaColors.null; // Null mean use the default PromptTextColor
    //property ReadOnly;
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType default TReturnKeyType.Default;
    //property RotationAngle;
    //property RotationCenter;
    //property Pivot;
    //property Scale;
    property Shadow;
    property Sides;
    property Size;
    property StateStyles: TStateStyles read FStateStyles write SetStateStyles;
    property Stroke;
    property SupportingText: String read FSupportingText write SetSupportingText;
    property SupportingTextSettings: TSupportingTextSettings read FSupportingTextSettings write SetSupportingTextSettings;
    property TabOrder;
    property TabStop;
    property Text: String read getText write SetText;
    property TextSettings: TTextSettings read FTextSettings write SetTextSettings;
    property TintColor: TAlphaColor read GetTintColor write setTintColor default TalphaColors.null; // IOS only - the color of the cursor caret and the text selection handles. null mean use the default TintColor
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property XRadius;
    property YRadius;
    //property OnCanFocus;
    //property OnChange;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnEnter;
    property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown; // Not work under android - it's like this with their @{[^# virtual keyboard :(
    //property OnKeyUp; // Not work under android - it's like this with their @{[^# virtual keyboard :(
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
    property OnReturnKey: TNotifyEvent read fOnReturnKey write SetOnReturnKey;
    //property OnTyping;
    //property OnValidate;
    //property OnValidating;
  end;

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALEdit = class(TALBaseEdit)
  protected
    procedure AdjustSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    function HasUnconstrainedAutosizeX: Boolean; override;
  published
    property AutoSize default True;
    property Password;
  end;

procedure Register;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  System.UIConsts,
  FMX.Types3D,
  FMX.Utils,
  {$IF defined(android)}
  Androidapi.Helpers,
  Androidapi.Input,
  Androidapi.KeyCodes,
  Androidapi.JNI.App,
  Androidapi.JNI.Util,
  Androidapi.JNI.Os,
  FMX.Platform,
  FMX.Platform.Android,
  FMX.Platform.UI.Android,
  {$ELSEIF defined(IOS)}
  Macapi.CoreFoundation,
  iOSapi.CocoaTypes,
  Macapi.Helpers,
  iOSapi.CoreText,
  FMX.Platform.iOS,
  FMX.Helpers.iOS,
  FMX.Consts,
  Alcinoe.iOSapi.Foundation,
  {$ELSEIF defined(ALMacOS)}
  Macapi.CoreFoundation,
  Macapi.Helpers,
  FMX.Helpers.Mac,
  FMX.Consts,
  Alcinoe.Macapi.Foundation,
  {$ELSEIF defined(MSWINDOWS)}
  Winapi.CommCtrl,
  Fmx.Forms,
  {$endif}
  {$IF defined(ALSkiaCanvas)}
  System.Skia.API,
  FMX.Skia.Canvas,
  {$endif}
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  Alcinoe.FMX.Memo,
  Alcinoe.FMX.BreakText,
  Alcinoe.StringUtils,
  Alcinoe.Common;

{**}
Type
  _TALBaseStateStyleAccessProtected = class(TALBaseStateStyle);

{********************************************************}
constructor TALBaseEditControl.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  CanFocus := True;
  fOnChange := nil;
  FOnReturnKey := nil;
  FTextSettings := TALBaseEdit.TTextSettings.Create;
  FTextSettings.OnChanged := TextSettingsChanged;
  FNativeView := CreateNativeView;
end;

{************************************}
destructor TALBaseEditControl.Destroy;
begin
  ALFreeAndNil(FTextSettings);
  ALFreeAndNil(FNativeView);
  inherited Destroy;
end;

{********************}
{$IF defined(android)}
function TALBaseEditControl.GetNativeView: TALAndroidNativeView;
begin
  Result := FNativeView;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALBaseEditControl.GetNativeView: TALIosNativeView;
begin
  Result := FNativeView;
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
function TALBaseEditControl.GetNativeView: TALMacNativeView;
begin
  Result := FNativeView;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
function TALBaseEditControl.GetNativeView: TALWinNativeView;
begin
  Result := FNativeView;
end;
{$ENDIF}

{*****************************************************************************}
procedure TALBaseEditControl.SetTextSettings(const Value: TALBaseTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{************************************}
procedure TALBaseEditControl.DoChange;
begin
  if assigned(fOnChange) then
    fOnChange(self);
end;

{***************************************}
procedure TALBaseEditControl.DoReturnKey;
begin
  if assigned(fOnReturnKey) then
    fOnReturnKey(self);
end;

{*****************************************}
procedure TALBaseEditControl.DoRootChanged;
begin
  inherited;
  if csDestroying in ComponentState then exit;
  {$IF not defined(ALDPK)}
  NativeView.RootChanged(Root);
  {$ENDIF}
end;

{**********************************}
procedure TALBaseEditControl.Resize;
begin
  inherited;
  {$IF not defined(ALDPK)}
  NativeView.UpdateFrame;
  {$ENDIF}
end;

{*********************************************}
procedure TALBaseEditControl.DoAbsoluteChanged;
begin
  inherited;
  {$IF not defined(ALDPK)}
  if not (csLoading in ComponentState) then
    NativeView.UpdateFrame;
  {$ENDIF}
end;

{******************************************}
procedure TALBaseEditControl.VisibleChanged;
begin
  inherited;
  {$IF not defined(ALDPK)}
  NativeView.SetVisible(Visible);
  {$ENDIF}
end;

{***************************************}
procedure TALBaseEditControl.ChangeOrder;
begin
  inherited;
  {$IF not defined(ALDPK)}
  NativeView.ChangeOrder;
  {$ENDIF}
end;

{*****************************************}
procedure TALBaseEditControl.RecalcOpacity;
begin
  inherited;
  {$IF not defined(ALDPK)}
  NativeView.setAlpha(AbsoluteOpacity);
  {$ENDIF}
end;

{*****************************************}
procedure TALBaseEditControl.RecalcEnabled;
begin
  inherited;
  {$IF not defined(ALDPK)}
  NativeView.SetEnabled(AbsoluteEnabled);
  {$ENDIF}
end;

{*************************************************}
function TALBaseEditControl.HasNativeView: boolean;
begin
  {$IF not defined(ALDPK)}
  Result := NativeView.Visible;
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

{*****************************************}
Procedure TALBaseEditControl.AddNativeView;
begin
  {$IF not defined(ALDPK)}
  if NativeView.visible then exit;
  NativeView.SetVisible(true);
  if Parentcontrol.IsFocused then begin
    NativeView.SetFocus;
    {$IF defined(android)}
    ALVirtualKeyboardVisible := True;
    {$IF defined(DEBUG)}
    ALLog('TALBaseEditControl.showVirtualKeyboard', 'control.name: ' + Name);
    {$ENDIF}
    MainActivity.getVirtualKeyboard.showFor(NativeView.View);
    {$ENDIF}
  end;
  {$ENDIF}
end;

{********************************************}
Procedure TALBaseEditControl.RemoveNativeView;
begin
  {$IF not defined(ALDPK)}
  if not NativeView.visible then exit;
  NativeView.ResetFocus;
  {$IF defined(android)}
  ALVirtualKeyboardVisible := False;
  TThread.ForceQueue(nil,
    procedure
    begin
      If not ALVirtualKeyboardVisible then begin
        {$IF defined(DEBUG)}
        ALLog('TALBaseEditControl.hideVirtualKeyboard');
        {$ENDIF}
        MainActivity.getVirtualKeyboard.hide;
      end;
    end);
  {$ENDIF}
  NativeView.SetVisible(False);
  {$ENDIF}
end;

{**************************************************************************}
procedure TALBaseEditControl.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  {$IF not defined(ALDPK)}
  NativeView.AncestorVisibleChanged;
  {$ENDIF}
end;

{*************************************************}
procedure TALBaseEditControl.AncestorParentChanged;
begin
  inherited;
  if csDestroying in ComponentState then exit;
  {$IF not defined(ALDPK)}
  NativeView.UpdateFrame;
  {$ENDIF}
end;

{*****************************************}
procedure TALBaseEditControl.ParentChanged;
begin
  inherited;
  if csDestroying in ComponentState then exit;
  {$IF not defined(ALDPK)}
  NativeView.UpdateFrame;
  {$ENDIF}
end;

{***************************************}
procedure TALBaseEditControl.DoEndUpdate;
begin
  inherited;
  if csDestroying in ComponentState then exit;
  // Without this, in some case when we are doing beginupdate to the TEdit
  // (because in android for exemple we would like to not refresh the position of the control during calculation)
  // then when we do endupdate the control is not paint or lost somewhere
  {$IF not defined(ALDPK)}
  NativeView.UpdateFrame;
  {$ENDIF}
end;

{************************************************}
function TALBaseEditControl.getLineCount: integer;
begin
  Result := 1;
end;

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
      'event: ' + JstringToString(event.toString))
  else
    ALLog(
      'TALAndroidEditText.TALKeyPreImeListener.onKeyPreIme',
      'control.name: ' + FEditText.FEditControl.parent.Name + ' | ' +
      'keyCode: ' + inttostr(keyCode));
  {$ENDIF}
  if ((event = nil) or (event.getAction = AKEY_EVENT_ACTION_UP)) and
     (keyCode = AKEYCODE_BACK) then begin

    result := true;
    FEditText.FEditcontrol.resetfocus;

  end
  else result := false;
end;

{******************************************************************************************}
constructor TALAndroidEditText.TALTouchListener.Create(const aEditText: TALAndroidEditText);
begin
  inherited Create;
  FEditText := aEditText;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.Presentation.Android.TAndroidNativeView.ProcessTouch was not updated and adjust the IFDEF'}
{$ENDIF}
function TALAndroidEditText.TALTouchListener.onTouch(v: JView; event: JMotionEvent): Boolean;
begin
  if (FEditText.Form <> nil) and
     (not FeditText.view.hasFocus) and
     ((not FeditText.fIsMultiline) or
      (FeditText.FEditControl.getLineCount < FeditText.FeditControl.Height / FeditText.FeditControl.getLineHeight)) then begin

    var LHandle: TAndroidWindowHandle;
    if FEditText.Form.IsHandleAllocated then
      LHandle := WindowHandleToPlatform(FEditText.Form.Handle)
    else
      LHandle := nil;

    if LHandle <> nil then
      LHandle.CurrentMotionEvent := event;

    var LTouchPoint := TPointF.Create(event.getRawX / ALGetScreenScale, event.getRawY / ALGetScreenScale);
    LTouchPoint := FEditText.Form.ScreenToClient(LTouchPoint);
    var LEventAction := event.getAction;
    if LEventAction = TJMotionEvent.JavaClass.ACTION_DOWN then begin
      FEditText.Form.MouseMove([ssTouch], LTouchPoint.X, LTouchPoint.Y);
      FEditText.Form.MouseMove([], LTouchPoint.X, LTouchPoint.Y); // Require for correct IsMouseOver handle
      FEditText.Form.MouseDown(TMouseButton.mbLeft, [ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y);
    end
    else if LEventAction = TJMotionEvent.JavaClass.ACTION_MOVE then begin
      FEditText.Form.MouseMove([ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y)
    end
    else if LEventAction = TJMotionEvent.JavaClass.ACTION_CANCEL then begin
      FEditText.Form.MouseUp(TMouseButton.mbLeft, [ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y, False);
      FEditText.Form.MouseLeave;
    end
    else if LEventAction = TJMotionEvent.JavaClass.ACTION_UP then
    begin
      FEditText.Form.MouseUp(TMouseButton.mbLeft, [ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y);
      FEditText.Form.MouseLeave;
    end;
    Result := True;

    if LHandle <> nil then
      LHandle.CurrentMotionEvent := nil;

  end
  else begin
    // In Android, within a single-line EditText, it's possible for the text line to shift slightly vertically
    // (as if the text is taller than the line height). To prevent this minor scrolling, we consume the touch event.
    if (not FeditText.fIsMultiline) and (FeditText.view.hasFocus) and (event.getAction() = TJMotionEvent.JavaClass.ACTION_MOVE) then result := true
    else result := false;
  end
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
  ALLog('TALAndroidEditText.TALTextWatcher.afterTextChanged', 'control.name: ' + FEditText.FEditControl.parent.Name);
  {$ENDIF}
  FEditText.fEditControl.DoChange;
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
       'event: ' + JstringToString(event.toString))
   else
     ALLog(
       'TALAndroidEditText.TALEditorActionListener.onEditorAction',
       'control.name: ' + FEditText.FEditControl.parent.Name + ' | ' +
       'actionId: ' + inttostr(actionId));
  {$ENDIF}
  //IME_ACTION_DONE: the action key performs a "done" operation, typically meaning there is nothing more to input and the IME will be closed.
  //IME_ACTION_GO: the action key performs a "go" operation to take the user to the target of the text they typed. Typically used, for example, when entering a URL.
  //IME_ACTION_NEXT: the action key performs a "next" operation, taking the user to the next field that will accept text.
  //IME_ACTION_NONE: there is no available action.
  //IME_ACTION_PREVIOUS: like IME_ACTION_NEXT, but for moving to the previous field. This will normally not be used to specify an action (since it precludes IME_ACTION_NEXT), but can be returned to the app if it sets IME_FLAG_NAVIGATE_PREVIOUS.
  //IME_ACTION_SEARCH: the action key performs a "search" operation, taking the user to the results of searching for the text they have typed (in whatever context is appropriate).
  //IME_ACTION_SEND: the action key performs a "send" operation, delivering the text to its target. This is typically used when composing a message in IM or SMS where sending is immediate.
  //IME_ACTION_UNSPECIFIED: no specific action has been associated with this editor, let the editor come up with its own if it can.
  if (assigned(FEditText.FEditControl.OnReturnKey)) and
     (((actionId = TJEditorInfo.javaClass.IME_ACTION_UNSPECIFIED) and // IME_ACTION_UNSPECIFIED = the return key
       (not fIsMultiLineEditText)) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_DONE) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_GO) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_NEXT) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_SEARCH) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_SEND)) then begin

    result := true;
    FEditText.FEditControl.DoReturnKey;

  end
  else result := false;
end;

{***********************************************************************************************************************************************************************************}
constructor TALAndroidEditText.Create(const AControl: TALAndroidEditControl; Const aIsMultiline: Boolean = False; const aDefStyleAttr: String = ''; const aDefStyleRes: String = '');
begin
  fIsMultiline := aIsMultiline;
  fDefStyleAttr := aDefStyleAttr;
  fDefStyleRes := aDefStyleRes;
  FTextWatcher := TALTextWatcher.Create(self);
  FEditorActionListener := TALEditorActionListener.Create(self, aIsMultiline);
  FKeyPreImeListener := TALKeyPreImeListener.Create(self);
  FTouchListener := TALTouchListener.Create(self);
  fEditControl := TALAndroidEditControl(AControl);
  inherited create(AControl);  // This will call InitView
end;

{************************************}
destructor TALAndroidEditText.Destroy;
begin

  View.setVisibility(TJView.JavaClass.INVISIBLE);
  View.RemoveTextChangedListener(FTextWatcher);
  View.setOnEditorActionListener(nil);
  View.SetKeyPreImeListener(nil);
  View.SetOnTouchListener(nil);

  alfreeandNil(FTextWatcher);
  alfreeandNil(fEditorActionListener);
  alfreeandNil(FKeyPreImeListener);
  alfreeandNil(FTouchListener);

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
  View.SetOnTouchListener(fTouchListener);
end;

{***********************************************}
function TALAndroidEditText.GetView: JALEditText;
begin
  Result := inherited GetView<JALEditText>;
end;

{*************************************************************************************************************************************************************************}
constructor TALAndroidEditControl.Create(const AOwner: TComponent; Const AIsMultiline: Boolean = False; const ADefStyleAttr: String = ''; const ADefStyleRes: String = '');
begin
  FDefStyleAttr := ADefStyleAttr;
  FDefStyleRes := ADefStyleRes;
  fIsMultiline := aIsMultiline;
  inherited create(AOwner);
  FFillColor := $ffffffff;
  fMaxLength := 0;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventHandler);
  fReturnKeyType := tReturnKeyType.Default;
  fKeyboardType := TVirtualKeyboardType.default;
  fAutoCapitalizationType := TALAutoCapitalizationType.acNone;
  fPassword := false;
  fCheckSpelling := true;
  FTintColor := TalphaColors.Null;
  DoSetReturnKeyType(fReturnKeyType);
  DoSetInputType(fKeyboardType, fAutoCapitalizationType, fPassword, fCheckSpelling, fIsMultiline);
end;

{***************************************}
destructor TALAndroidEditControl.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventHandler);
  inherited Destroy;
end;

{********************************************************************}
Function TALAndroidEditControl.CreateNativeView: TALAndroidNativeView;
begin
  result := TALAndroidEditText.create(self, FIsMultiline, FDefStyleAttr, FDefStyleRes);
  result.view.setBackgroundColor(TJColor.JavaClass.TRANSPARENT);
  result.view.setBackground(nil);
  result.view.setPadding(0, 0, 0, 0);
end;

{***************************************************************}
function TALAndroidEditControl.GetNativeView: TALAndroidEditText;
begin
  result := TALAndroidEditText(inherited GetNativeView);
end;

{*********************************************}
procedure TALAndroidEditControl.DoSetInputType(
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

  NativeView.view.setInputType(LInputType);

end;

{*******************************************************************}
function TALAndroidEditControl.GetKeyboardType: TVirtualKeyboardType;
begin
  Result := fKeyboardType;
end;

{*********************************************************************************}
procedure TALAndroidEditControl.setKeyboardType(const Value: TVirtualKeyboardType);
begin
  if (value <> fKeyboardType) then begin
    fKeyboardType := Value;
    DoSetInputType(Value, fAutoCapitalizationType, fPassword, fCheckSpelling, fIsMultiLine);
  end;
end;

{**********************************************************************************}
function TALAndroidEditControl.GetAutoCapitalizationType: TALAutoCapitalizationType;
begin
  result := fAutoCapitalizationType;
end;

{************************************************************************************************}
procedure TALAndroidEditControl.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  if (value <> fAutoCapitalizationType) then begin
    fAutoCapitalizationType := Value;
    DoSetInputType(fKeyboardType, Value, fPassword, fCheckSpelling, fIsMultiLine);
  end;
end;

{**************************************************}
function TALAndroidEditControl.GetPassword: Boolean;
begin
  Result := fPassword;
end;

{****************************************************************}
procedure TALAndroidEditControl.setPassword(const Value: Boolean);
begin
  if (value <> fPassword) then begin
    fPassword := Value;
    DoSetInputType(fKeyboardType, fAutoCapitalizationType, Value, fCheckSpelling, fIsMultiLine);
  end;
end;

{*******************************************************}
function TALAndroidEditControl.GetCheckSpelling: Boolean;
begin
  result := fCheckSpelling;
end;

{*********************************************************************}
procedure TALAndroidEditControl.SetCheckSpelling(const Value: Boolean);
begin
  if (value <> fCheckSpelling) then begin
    fCheckSpelling := Value;
    DoSetInputType(fKeyboardType, fAutoCapitalizationType, fPassword, Value, fIsMultiLine);
  end;
end;

{***************************************************************************************}
procedure TALAndroidEditControl.DoSetReturnKeyType(const aReturnKeyType: TReturnKeyType);
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
  NativeView.view.setImeOptions(LImeOptions);
end;

{**************************************************************}
function TALAndroidEditControl.GetReturnKeyType: TReturnKeyType;
begin
  Result := fReturnKeyType;
end;

{****************************************************************************}
procedure TALAndroidEditControl.setReturnKeyType(const Value: TReturnKeyType);
begin
  if (value <> fReturnKeyType) then begin
    fReturnKeyType := Value;
    DoSetReturnKeyType(Value);
  end;
end;

{***************************************************}
function TALAndroidEditControl.GetMaxLength: integer;
begin
  Result := FMaxLength;
end;

{*****************************************************************}
procedure TALAndroidEditControl.SetMaxLength(const Value: integer);
begin
  if value <> FMaxLength then begin
    FMaxLength := value;
    NativeView.View.setMaxLength(Value);
  end;
end;

{***************************************************}
function TALAndroidEditControl.GetPromptText: String;
begin
  result := JCharSequenceToStr(NativeView.View.getHint);
end;

{*****************************************************************}
procedure TALAndroidEditControl.setPromptText(const Value: String);
begin
  NativeView.View.setHint(StrToJCharSequence(Value));
end;

{*************************************************************}
function TALAndroidEditControl.GetPromptTextColor: TAlphaColor;
begin
  result := TAlphaColor(NativeView.View.getCurrentHintTextColor);
end;

{***************************************************************************}
procedure TALAndroidEditControl.setPromptTextColor(const Value: TAlphaColor);
begin
  if Value <> TalphaColors.null then
    NativeView.View.setHintTextColor(integer(Value));
end;

{*********************************************}
function TALAndroidEditControl.getText: String;
begin
  result := JCharSequenceToStr(NativeView.View.gettext);
end;

{***********************************************************}
procedure TALAndroidEditControl.SetText(const Value: String);
begin
  NativeView.View.setText(StrToJCharSequence(Value), TJTextView_BufferType.javaClass.EDITABLE);
end;

{*******************************************************************}
procedure TALAndroidEditControl.TextSettingsChanged(Sender: TObject);
begin

  var LFontColor := integer(textsettings.font.color);
  var LFontSize: single := textsettings.font.size;

  var LFontStyles: TFontStyles := [];
  if textsettings.font.Weight in [TFontWeight.Bold,
                                  TFontWeight.UltraBold,
                                  TFontWeight.Black,
                                  TFontWeight.UltraBlack] then LFontStyles := LFontStyles + [TFontStyle.fsBold];
  if textsettings.font.Slant in [TFontSlant.Italic, TFontSlant.Oblique] then LFontStyles := LFontStyles + [TFontStyle.fsItalic];
  var LFontFamily := ALExtractPrimaryFontFamily(textsettings.font.Family);
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
  case textsettings.HorzAlign of
    TALTextHorzAlign.Center: LGravity := $01; // center_horizontal 0x01
    TALTextHorzAlign.Leading: LGravity := $03; // left 0x03
    TALTextHorzAlign.Trailing: LGravity := $05; // right 0x05
    else LGravity := $03; // left 0x03
  end;
  case textsettings.VertAlign of
    TALTextVertAlign.Center: LGravity := LGravity or $10; // center_vertical 0x10
    TALTextVertAlign.Leading: LGravity := LGravity or $30; // top 0x30
    TALTextVertAlign.Trailing: LGravity := LGravity or $50; // bottom 0x50
    else LGravity := LGravity or $10; // center_vertical 0x10
  end;

  //-----
  NativeView.view.setTextColor(LFontColor); // Sets the text color for all the states (normal, selected, focused) to be this color.
  NativeView.view.setTextSize(TJTypedValue.javaclass.COMPLEX_UNIT_DIP, LFontSize); // Set the default text size to a given unit and value.
  //-----
  var LTypeface := TJTypeface.JavaClass.create(StringToJString(LFontFamily), ALfontStyleToAndroidStyle(LFontStyles));
  // Note that not all Typeface families actually have bold and italic variants, so you may
  // need to use setTypeface(Typeface, int) to get the appearance that you actually want.
  NativeView.view.setTypeface(LTypeface);
  LTypeface := nil;
  NativeView.view.setgravity(LGravity);
  //-----
  if not SameValue(textsettings.LineHeightMultiplier, 0, TEpsilon.Scale) then
    NativeView.view.setLineSpacing(0{add}, textsettings.LineHeightMultiplier{mult});

end;

{*******************************************************}
function TALAndroidEditControl.GetTintColor: TAlphaColor;
begin
  Result := FTintColor;
end;

{*********************************************************************}
procedure TALAndroidEditControl.setTintColor(const Value: TAlphaColor);
begin
  FTintColor := Value;
end;

{*******************************************************}
function TALAndroidEditControl.GetFillColor: TAlphaColor;
begin
  Result := FFillColor;
end;

{*********************************************************************}
procedure TALAndroidEditControl.SetFillColor(const Value: TAlphaColor);
begin
  FFillColor := Value;
end;

{************************************************************************************************}
procedure TALAndroidEditControl.ApplicationEventHandler(const Sender: TObject; const M: TMessage);
begin
  {$IF defined(DEBUG)}
  if isfocused and
     (M is TApplicationEventMessage) then begin
    case (M as TApplicationEventMessage).Value.Event of
      TApplicationEvent.FinishedLaunching: ALLog('TALAndroidEditControl.ApplicationEventHandler', 'Event: TApplicationEvent.FinishedLaunching');
      TApplicationEvent.BecameActive: ALLog('TALAndroidEditControl.ApplicationEventHandler', 'Event: TApplicationEvent.BecameActive');
      TApplicationEvent.WillBecomeInactive: ALLog('TALAndroidEditControl.ApplicationEventHandler', 'Event: TApplicationEvent.WillBecomeInactive');
      TApplicationEvent.EnteredBackground: ALLog('TALAndroidEditControl.ApplicationEventHandler', 'Event: TApplicationEvent.EnteredBackground');
      TApplicationEvent.WillBecomeForeground: ALLog('TALAndroidEditControl.ApplicationEventHandler', 'Event: TApplicationEvent.WillBecomeForeground');
      TApplicationEvent.WillTerminate: ALLog('TALAndroidEditControl.ApplicationEventHandler', 'Event: TApplicationEvent.WillTerminate');
      TApplicationEvent.LowMemory: ALLog('TALAndroidEditControl.ApplicationEventHandler', 'Event: TApplicationEvent.LowMemory');
      TApplicationEvent.TimeChange: ALLog('TALAndroidEditControl.ApplicationEventHandler', 'Event: TApplicationEvent.TimeChange');
      TApplicationEvent.OpenURL: ALLog('TALAndroidEditControl.ApplicationEventHandler', 'Event: TApplicationEvent.OpenURL');
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
function TALAndroidEditControl.getLineHeight: Single;
begin
  result := NativeView.view.getLineHeight / ALGetScreenScale;
end;

{****************************************************************************************}
Procedure TALAndroidEditControl.setSelection(const AStart: integer; const AStop: Integer);
begin
  NativeView.View.setSelection(aStart, aStop);
end;

{******************************************************************}
Procedure TALAndroidEditControl.setSelection(const AIndex: integer);
begin
  NativeView.view.setSelection(aindex);
end;

{$endif}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}

{*************************************}
constructor TALIosEditTextField.Create;
begin
  inherited;
  View.setExclusiveTouch(True);
  View.setBorderStyle(UITextBorderStyleNone);
  View.addTarget(GetObjectID, sel_getUid(MarshaledAString(TMarshal.AsAnsi('ControlEventEditingChanged'))), UIControlEventEditingChanged);
end;

{***************************************************************}
constructor TALIosEditTextField.Create(const AControl: TControl);
begin
  fEditControl := TALIosEditControl(AControl);
  inherited;
end;

{*************************************}
destructor TALIosEditTextField.Destroy;
begin
  View.removeTarget(GetObjectID, sel_getUid(MarshaledAString(TMarshal.AsAnsi('ControlEventEditingChanged'))), UIControlEventEditingChanged);
  inherited;
end;

{*************************************************************}
procedure TALIosEditTextField.SetEnabled(const value: Boolean);
begin
  inherited;
  View.SetEnabled(value);
end;

{***************************************************************************}
function TALIosEditTextField.ExtractFirstTouchPoint(touches: NSSet): TPointF;
begin
  var LPointer := touches.anyObject;
  if LPointer=nil then
    raise Exception.Create('Error 46450539-F150-45FC-BA01-0397F2A98B0C');
  var LLocalTouch := TUITouch.Wrap(LPointer);
  if Form=nil then
    raise Exception.Create('Error 5F61EC6E-0C13-46CD-A0C0-8417AA32B62A');
  var LTouchPoint := LLocalTouch.locationInView(GetFormView(Form));
  Result := TPointF.Create(LTouchPoint.X, LTouchPoint.Y);
end;

{*****************************************************************************}
procedure TALIosEditTextField.touchesBegan(touches: NSSet; withEvent: UIEvent);
begin
  if (Form <> nil) and
     (not view.isFirstResponder) and
     (touches.count > 0) then begin

    var LHandle: TiOSWindowHandle;
    if Form.IsHandleAllocated then
      LHandle := WindowHandleToPlatform(Form.Handle)
    else
      LHandle := nil;

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := withEvent;

    var LTouchPoint := ExtractFirstTouchPoint(touches);
    Form.MouseMove([ssTouch], LTouchPoint.X, LTouchPoint.Y);
    Form.MouseMove([], LTouchPoint.X, LTouchPoint.Y); // Require for correct IsMouseOver handle
    Form.MouseDown(TMouseButton.mbLeft, [ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y);

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := nil;

  end;
end;

{*********************************************************************************}
procedure TALIosEditTextField.touchesCancelled(touches: NSSet; withEvent: UIEvent);
begin
  if (Form <> nil) and
     (not view.isFirstResponder) and
     (touches.count > 0) then begin

    var LHandle: TiOSWindowHandle;
    if Form.IsHandleAllocated then
      LHandle := WindowHandleToPlatform(Form.Handle)
    else
      LHandle := nil;

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := withEvent;

    var LTouchPoint := ExtractFirstTouchPoint(touches);
    Form.MouseUp(TMouseButton.mbLeft, [ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y);
    Form.MouseLeave;

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := nil;

  end;
end;

{*****************************************************************************}
procedure TALIosEditTextField.touchesEnded(touches: NSSet; withEvent: UIEvent);
begin
  if (Form <> nil) and
     (not view.isFirstResponder) and
     (touches.count > 0) then begin

    var LHandle: TiOSWindowHandle;
    if Form.IsHandleAllocated then
      LHandle := WindowHandleToPlatform(Form.Handle)
    else
      LHandle := nil;

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := withEvent;

    var LTouchPoint := ExtractFirstTouchPoint(touches);
    Form.MouseUp(TMouseButton.mbLeft, [ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y);
    Form.MouseLeave;

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := nil;

  end;
end;

{*****************************************************************************}
procedure TALIosEditTextField.touchesMoved(touches: NSSet; withEvent: UIEvent);
begin
  if (Form <> nil) and
     (not view.isFirstResponder) and
     (touches.count > 0) then begin

    var LHandle: TiOSWindowHandle;
    if Form.IsHandleAllocated then
      LHandle := WindowHandleToPlatform(Form.Handle)
    else
      LHandle := nil;

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := withEvent;

    var LTouchPoint := ExtractFirstTouchPoint(touches);
    Form.MouseMove([ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y);

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := nil;

  end;
end;

{************************************************************}
function TALIosEditTextField.canBecomeFirstResponder: Boolean;
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosEditTextField.canBecomeFirstResponder', 'control.name: ' + fEditControl.parent.Name);
  {$ENDIF}
  Result := UITextField(Super).canBecomeFirstResponder and TControl(fEditControl.Owner).canFocus;
end;

{*********************************************************}
function TALIosEditTextField.becomeFirstResponder: Boolean;
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosEditTextField.becomeFirstResponder', 'control.name: ' + fEditControl.parent.Name);
  {$ENDIF}
  Result := UITextField(Super).becomeFirstResponder;
  if (not TControl(fEditControl.Owner).IsFocused) then
    TControl(fEditControl.Owner).SetFocus;
end;

{*******************************************************}
procedure TALIosEditTextField.ControlEventEditingChanged;
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosEditTextField.ControlEventEditingChanged', 'control.name: ' + fEditControl.parent.Name);
  {$ENDIF}
  fEditControl.DoChange;
end;

{*********************************************************}
function TALIosEditTextField.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALIosEditTextField);
end;

{************************************************}
function TALIosEditTextField.GetView: UITextField;
begin
  Result := inherited GetView<UITextField>;
end;

{************************************************************************************}
constructor TALIosEditTextFieldDelegate.Create(const AEditControl: TALIosEditControl);
begin
  inherited Create;
  FEditControl := AEditControl;
  if FEditControl = nil then
    raise EArgumentNilException.Create(Format(SWrongParameter, ['AEditControl']));
end;

{***************************************************************************************************************************************************}
function TALIosEditTextFieldDelegate.textField(textField: UITextField; shouldChangeCharactersInRange: NSRange; replacementString: NSString): Boolean;
begin
  {$IF defined(DEBUG)}
  ALLog(
    'TALIosEditTextFieldDelegate.textField',
    'control.name: ' + FEditControl.parent.Name + ' | ' +
    'replacementString: ' + NSStrToStr(replacementString));
  {$ENDIF}
  if FEditControl.maxLength > 0 then begin
    var LText: NSString := TNSString.Wrap(textField.text);
    if shouldChangeCharactersInRange.length + shouldChangeCharactersInRange.location > LText.length then exit(false);
    result := LText.length + replacementString.length - shouldChangeCharactersInRange.length <= NSUInteger(FEditControl.maxLength);
  end
  else Result := True;
end;

{*************************************************************************************}
procedure TALIosEditTextFieldDelegate.textFieldDidBeginEditing(textField: UITextField);
begin
end;

{***********************************************************************************}
procedure TALIosEditTextFieldDelegate.textFieldDidEndEditing(textField: UITextField);
begin
  TControl(FEditControl.Owner).ResetFocus;
end;

{************************************************************************************************}
function TALIosEditTextFieldDelegate.textFieldShouldBeginEditing(textField: UITextField): Boolean;
begin
  Result := True;
end;

{*****************************************************************************************}
function TALIosEditTextFieldDelegate.textFieldShouldClear(textField: UITextField): Boolean;
begin
  Result := true;
end;

{**********************************************************************************************}
function TALIosEditTextFieldDelegate.textFieldShouldEndEditing(textField: UITextField): Boolean;
begin
  Result := True;
end;

{******************************************************************************************}
function TALIosEditTextFieldDelegate.textFieldShouldReturn(textField: UITextField): Boolean;
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosEditTextFieldDelegate.textFieldShouldReturn', 'control.name: ' + FEditControl.parent.Name);
  {$ENDIF}
  if assigned(fEditControl.OnReturnKey) then begin
    fEditControl.DoReturnKey;
    result := false;
  end
  else Result := true; // return YES if the text field should implement its default behavior for the return button; otherwise, NO.
end;

{*******************************************************}
constructor TALIosEditControl.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FTextFieldDelegate := TALIosEditTextFieldDelegate.Create(Self);
  NativeView.View.setDelegate(FTextFieldDelegate.GetObjectID);
  FFillColor := $ffffffff;
  fMaxLength := 0;
  fPromptTextColor := TalphaColors.Null;
  SetReturnKeyType(tReturnKeyType.Default);
  SetKeyboardType(TVirtualKeyboardType.default);
  setAutoCapitalizationType(TALAutoCapitalizationType.acNone);
  SetPassword(false);
  SetCheckSpelling(True);
end;

{***********************************}
destructor TALIosEditControl.Destroy;
begin
  NativeView.View.setDelegate(nil);
  ALFreeAndNil(FTextFieldDelegate);
  inherited Destroy;
end;

{************************************************************}
Function TALIosEditControl.CreateNativeView: TALIosNativeView;
begin
  result := TALIosEditTextField.create(self);
end;

{************************************************************}
function TALIosEditControl.GetNativeView: TALIosEditTextField;
begin
  result := TALIosEditTextField(inherited GetNativeView);
end;

{*****************************************************************************}
procedure TALIosEditControl.SetKeyboardType(const Value: TVirtualKeyboardType);
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
  NativeView.View.setKeyboardType(LUIKeyboardType);
end;

{***************************************************************}
function TALIosEditControl.GetKeyboardType: TVirtualKeyboardType;
begin
  var LUIKeyboardType := NativeView.View.KeyboardType;
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

{********************************************************************************************}
procedure TALIosEditControl.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  var LUITextAutoCapitalizationType: UITextAutoCapitalizationType;
  case Value of
    TALAutoCapitalizationType.acWords:          LUITextAutoCapitalizationType := UITextAutoCapitalizationTypeWords;
    TALAutoCapitalizationType.acSentences:      LUITextAutoCapitalizationType := UITextAutoCapitalizationTypeSentences;
    TALAutoCapitalizationType.acAllCharacters:  LUITextAutoCapitalizationType := UITextAutoCapitalizationTypeAllCharacters;
    else {TALAutoCapitalizationType.acNone}     LUITextAutoCapitalizationType := UITextAutoCapitalizationTypeNone;
  end;
  NativeView.View.setAutoCapitalizationType(LUITextAutoCapitalizationType);
end;

{******************************************************************************}
function TALIosEditControl.GetAutoCapitalizationType: TALAutoCapitalizationType;
begin
  var LUITextAutoCapitalizationType := NativeView.View.AutoCapitalizationType;
  case LUITextAutoCapitalizationType of
    UITextAutoCapitalizationTypeWords:         result := TALAutoCapitalizationType.acWords;
    UITextAutoCapitalizationTypeSentences:     result := TALAutoCapitalizationType.acSentences;
    UITextAutoCapitalizationTypeAllCharacters: result := TALAutoCapitalizationType.acAllCharacters;
    else                                       result := TALAutoCapitalizationType.acNone;
  end;
end;

{************************************************************}
procedure TALIosEditControl.SetPassword(const Value: Boolean);
begin
  NativeView.View.setSecureTextEntry(Value);
end;

{**********************************************}
function TALIosEditControl.GetPassword: Boolean;
begin
  result := NativeView.View.isSecureTextEntry;
end;

{*****************************************************************}
procedure TALIosEditControl.SetCheckSpelling(const Value: Boolean);
begin
  if Value then begin
    NativeView.View.setSpellCheckingType(UITextSpellCheckingTypeYes);
    NativeView.View.setAutocorrectionType(UITextAutocorrectionTypeDefault);
  end
  else begin
    NativeView.View.setSpellCheckingType(UITextSpellCheckingTypeNo);
    NativeView.View.setAutocorrectionType(UITextAutocorrectionTypeNo);
  end;
end;

{***************************************************}
function TALIosEditControl.GetCheckSpelling: Boolean;
begin
  result := NativeView.View.SpellCheckingType = UITextSpellCheckingTypeYes;
end;

{************************************************************************}
procedure TALIosEditControl.setReturnKeyType(const Value: TReturnKeyType);
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
  NativeView.View.setReturnKeyType(LUIReturnKeyType);
end;

{**********************************************************}
function TALIosEditControl.GetReturnKeyType: TReturnKeyType;
begin
  var LUIReturnKeyType := NativeView.View.ReturnKeyType;
  case LUIReturnKeyType of
    UIReturnKeyDone:    result := TReturnKeyType.Done;
    UIReturnKeyGo:      result := TReturnKeyType.Go;
    UIReturnKeyNext:    result := TReturnKeyType.Next;
    UIReturnKeySearch:  result := TReturnKeyType.Search;
    UIReturnKeySend:    result := TReturnKeyType.Send;
    else                result := TReturnKeyType.Default;
  end;
end;

{***********************************************}
function TALIosEditControl.GetPromptText: String;
begin
  var LAttributedString := TALNSAttributedString.wrap(NSObjectToID(TUITextField.Wrap(NSObjectToID(NativeView.View)).AttributedPlaceholder));
  if LAttributedString = nil then Result := NSStrToStr(NativeView.View.placeholder)
  else result := NSStrToStr(LAttributedString.&String);
end;

{*************************************************************}
procedure TALIosEditControl.setPromptText(const Value: String);
begin
  applyPromptTextWithColor(Value, fPromptTextColor);
end;

{*********************************************************}
function TALIosEditControl.GetPromptTextColor: TAlphaColor;
begin
  Result := fPromptTextColor;
end;

{***********************************************************************}
procedure TALIosEditControl.setPromptTextColor(const Value: TAlphaColor);
begin
  if Value <> fPromptTextColor then begin
    fPromptTextColor := Value;
    applyPromptTextWithColor(GetPromptText, fPromptTextColor);
  end;
end;

{**************************************************************************************************}
procedure TALIosEditControl.applyPromptTextWithColor(const aStr: String; const aColor: TAlphaColor);
begin
  //Using `setPlaceholderString` is not suitable here because it does not allow customizing the placeholder text color,
  //which depends on the OS's light or dark mode. However, this application requires a placeholder color that is
  //independent of the system theme, as the background color does not adapt to the dark or light mode settings.
  //if (aColor = tAlphaColors.Null) or aStr.IsEmpty then FTextField.View.setPlaceholder(StrToNSStr(aStr))
  //else begin

  var LPromptTextAttr: NSMutableAttributedString := TNSMutableAttributedString.Wrap(TNSMutableAttributedString.Alloc.initWithString(StrToNSStr(aStr)));
  try

    if not aStr.IsEmpty then begin
      LPromptTextAttr.beginEditing;
      try
        var LTextRange := NSMakeRange(0, aStr.Length);
        var LUIColor: UIColor;
        if aColor <> tAlphaColors.Null then LUIColor := AlphaColorToUIColor(aColor)
        else LUIColor := AlphaColorToUIColor(
                           TAlphaColorF.Create(
                             ALSetColorAlpha(TextSettings.Font.Color, 0.5)).
                               PremultipliedAlpha.
                               ToAlphaColor);
        LPromptTextAttr.addAttribute(NSForegroundColorAttributeName, NSObjectToID(LUIColor), LTextRange);
        //NOTE: If I try to release the LUIColor I have an exception
      finally
        LPromptTextAttr.endEditing;
      end;
    end;

    NativeView.View.setAttributedPlaceholder(LPromptTextAttr);

  finally
    LPromptTextAttr.release;
  end;
end;

{***************************************************}
function TALIosEditControl.GetTintColor: TAlphaColor;
begin
  var red: CGFloat;
  var green: CGFloat;
  var blue: CGFloat;
  var alpha: CGFloat;
  if not NativeView.View.tintColor.getRed(@red, @green, @blue, @alpha) then result := TalphaColors.Null
  else result := TAlphaColorF.Create(red, green, blue, alpha).ToAlphaColor;
end;

{*****************************************************************}
procedure TALIosEditControl.setTintColor(const Value: TAlphaColor);
begin
  if Value <> TalphaColors.Null then
    NativeView.View.setTintColor(AlphaColorToUIColor(Value));
end;

{*****************************************}
function TALIosEditControl.getText: String;
begin
  result := NSStrToStr(TNSString.Wrap(NativeView.View.text));
end;

{*******************************************************}
procedure TALIosEditControl.SetText(const Value: String);
begin
  NativeView.View.setText(StrToNSStr(Value));
end;

{***********************************************}
function TALIosEditControl.GetMaxLength: integer;
begin
  Result := FMaxLength;
end;

{*************************************************************}
procedure TALIosEditControl.SetMaxLength(const Value: integer);
begin
  FMaxLength := Value;
end;

{***************************************************************}
procedure TALIosEditControl.TextSettingsChanged(Sender: TObject);
begin
  // Font
  var LFontRef := ALCreateCTFontRef(TextSettings.Font.Family, TextSettings.Font.Size, TextSettings.Font.Weight, TextSettings.Font.Slant);
  try
    var LDictionary := TNSDictionary.Wrap(
                         TNSDictionary.OCClass.dictionaryWithObject(
                           LFontRef,
                           NSObjectToID(TNSString.Wrap(kCTFontAttributeName))));
    // Setting this property applies the specified attributes to the entire
    // text of the text field. Unset attributes maintain their default values.
    // Note: I can't later call LDictionary.release or I have an error
    NativeView.View.setdefaultTextAttributes(LDictionary);
  finally
    CFRelease(LFontRef);
  end;

  // TextAlignment
  NativeView.View.setTextAlignment(ALTextHorzAlignToUITextAlignment(TextSettings.HorzAlign));

  // TextColor
  NativeView.View.setTextColor(AlphaColorToUIColor(TextSettings.Font.Color));
end;

{***************************************************}
function TALIosEditControl.GetFillColor: TAlphaColor;
begin
  Result := FFillColor;
end;

{*****************************************************************}
procedure TALIosEditControl.SetFillColor(const Value: TAlphaColor);
begin
  FFillColor := Value;
end;

{***********************************************}
function TALIosEditControl.getLineHeight: Single;
begin
  if NativeView.View.font = nil then
    TextSettingsChanged(nil);
  result := NativeView.View.font.lineHeight;
  if not SameValue(textsettings.LineHeightMultiplier, 0, TEpsilon.Scale) then
    result := result * textsettings.LineHeightMultiplier;
end;

{************************************************************************************}
Procedure TALIosEditControl.setSelection(const AStart: integer; const AStop: Integer);
begin
  var LStartPosition: UITextPosition := NativeView.View.positionFromPosition(NativeView.View.beginningOfDocument, AStart);
  var LEndPosition: UITextPosition := NativeView.View.positionFromPosition(LStartPosition, AStop - AStart);
  NativeView.View.setSelectedTextRange(NativeView.View.textRangeFromPosition(LStartPosition, LEndPosition));
end;

{**************************************************************}
Procedure TALIosEditControl.setSelection(const AIndex: integer);
begin
  var LStartPosition := NativeView.View.positionFromPosition(NativeView.View.beginningOfDocument, AIndex);
  var LEndPosition := NativeView.View.positionFromPosition(LStartPosition, 0);
  NativeView.View.setSelectedTextRange(NativeView.View.textRangeFromPosition(LStartPosition, LEndPosition));
end;

{$endif}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}

{*************************************}
constructor TALMacEditTextField.Create;
begin
  inherited;
  View.SetBezeled(False);
  View.setBordered(false);
  TALNSControl.Wrap(NSObjectToID(View)).setLineBreakMode(NSLineBreakByClipping);
  View.setDrawsBackground(false);
  View.setFocusRingType(NSFocusRingTypeNone);
end;

{***************************************************************}
constructor TALMacEditTextField.Create(const AControl: TControl);
begin
  fEditControl := TalMacEditControl(AControl);
  inherited;
end;

{*************************************************************}
procedure TALMacEditTextField.SetEnabled(const value: Boolean);
begin
  inherited;
  View.SetEnabled(value);
end;

{**********************************************************}
function TALMacEditTextField.acceptsFirstResponder: Boolean;
begin
  {$IF defined(DEBUG)}
  ALLog('TALMacEditTextField.acceptsFirstResponder', 'control.name: ' + fEditControl.parent.Name);
  {$ENDIF}
  Result := NSTextField(Super).acceptsFirstResponder and TControl(fEditControl.Owner).canFocus;
end;

{*********************************************************}
function TALMacEditTextField.becomeFirstResponder: Boolean;
begin
  {$IF defined(DEBUG)}
  ALLog('TALMacEditTextField.becomeFirstResponder', 'control.name: ' + fEditControl.parent.Name);
  {$ENDIF}
  Result := NSTextField(Super).becomeFirstResponder;
  if (not TControl(fEditControl.Owner).IsFocused) then
    TControl(fEditControl.Owner).SetFocus;
end;

{*********************************************************}
function TALMacEditTextField.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALMacEditTextField);
end;

{************************************************}
function TALMacEditTextField.GetView: NSTextField;
begin
  Result := inherited GetView<NSTextField>;
end;

{************************************************************************************}
constructor TALMacEditTextFieldDelegate.Create(const AEditControl: TALMacEditControl);
begin
  inherited Create;
  FEditControl := AEditControl;
  if FEditControl = nil then
    raise EArgumentNilException.Create(Format(SWrongParameter, ['AEditControl']));
end;

{************************************************************************************}
procedure TALMacEditTextFieldDelegate.controlTextDidBeginEditing(obj: NSNotification);
begin
end;

{**********************************************************************************}
procedure TALMacEditTextFieldDelegate.controlTextDidEndEditing(obj: NSNotification);
begin
  TControl(FEditControl.Owner).ResetFocus;
end;

{******************************************************************************}
procedure TALMacEditTextFieldDelegate.controlTextDidChange(obj: NSNotification);
begin
  if FEditControl.maxLength > 0 then begin
    var LText := NSStrToStr(FEditControl.NativeView.View.stringValue);
    if LText.length > FEditControl.maxLength then begin
      FEditControl.NativeView.View.SetStringValue(StrToNSStr(ALCopyStr(LText,1,FEditControl.maxLength)));
      exit;
    end;
  end;
  fEditControl.DoChange;
end;

{******************************************************************************************************************************}
function TALMacEditTextFieldDelegate.controlTextShouldBeginEditing(control: NSControl; textShouldBeginEditing: NSText): Boolean;
begin
  Result := True;
end;

{**************************************************************************************************************************}
function TALMacEditTextFieldDelegate.controlTextShouldEndEditing(control: NSControl; textShouldEndEditing: NSText): Boolean;
begin
  Result := True;
end;

{***************************************************************************************************************************************************}
function TALMacEditTextFieldDelegate.controlTextViewDoCommandBySelector(control: NSControl; textView: NSTextView; doCommandBySelector: SEL): Boolean;
begin
  if assigned(fEditControl.OnReturnKey) and (sel_getName(doCommandBySelector) = 'insertNewline:') then begin
    fEditControl.DoReturnKey;
    Result := True;
  end
  else
    result := False;
end;

{*******************************************************}
constructor TALMacEditControl.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FTextFieldDelegate := TALMacEditTextFieldDelegate.Create(Self);
  TALNSTextField.wrap(NSObjectToID(NativeView.View)).setDelegate(FTextFieldDelegate.GetObjectID);
  FFillColor := $ffffffff;
  fMaxLength := 0;
  fReturnKeyType := tReturnKeyType.Default;
  fKeyboardType := TVirtualKeyboardType.default;
  fAutoCapitalizationType := TALAutoCapitalizationType.acNone;
  fPassword := false;
  fCheckSpelling := true;
  fPromptTextColor := TalphaColors.Null;
  FTintColor := TalphaColors.Null;
end;

{***********************************}
destructor TALMacEditControl.Destroy;
begin
  NativeView.View.setDelegate(nil);
  ALFreeAndNil(FTextFieldDelegate);
  inherited Destroy;
end;

{************************************************************}
Function TALMacEditControl.CreateNativeView: TALMacNativeView;
begin
  result := TALMacEditTextField.create(self);
end;

{************************************************************}
function TALMacEditControl.GetNativeView: TALMacEditTextField;
begin
  result := TALMacEditTextField(inherited GetNativeView);
end;

{***************************************************************}
function TALMacEditControl.GetKeyboardType: TVirtualKeyboardType;
begin
  Result := FKeyboardType;
end;

{*****************************************************************************}
procedure TALMacEditControl.setKeyboardType(const Value: TVirtualKeyboardType);
begin
  FKeyboardType := Value;
end;

{******************************************************************************}
function TALMacEditControl.GetAutoCapitalizationType: TALAutoCapitalizationType;
begin
  Result := FAutoCapitalizationType;
end;

{********************************************************************************************}
procedure TALMacEditControl.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  FAutoCapitalizationType := Value;
end;

{**********************************************}
function TALMacEditControl.GetPassword: Boolean;
begin
  Result := FPassword;
end;

{************************************************************}
procedure TALMacEditControl.setPassword(const Value: Boolean);
begin
  FPassword := Value;
end;

{***************************************************}
function TALMacEditControl.GetCheckSpelling: Boolean;
begin
  Result := FCheckSpelling;
end;

{*****************************************************************}
procedure TALMacEditControl.setCheckSpelling(const Value: Boolean);
begin
  FCheckSpelling := Value;
end;

{**********************************************************}
function TALMacEditControl.GetReturnKeyType: TReturnKeyType;
begin
  Result := FReturnKeyType;
end;

{************************************************************************}
procedure TALMacEditControl.setReturnKeyType(const Value: TReturnKeyType);
begin
  FReturnKeyType := Value;
end;

{***********************************************}
function TALMacEditControl.GetPromptText: String;
begin
  var LAttributedString := TALNSTextField.Wrap(NSObjectToID(NativeView.View)).placeholderAttributedString;
  if LAttributedString = nil then Result := NSStrToStr(TALNSTextField.Wrap(NSObjectToID(NativeView.View)).PlaceholderString)
  else result := NSStrToStr(TALNSAttributedString.wrap(NSObjectToID(LAttributedString)).&String);
end;

{*************************************************************}
procedure TALMacEditControl.setPromptText(const Value: String);
begin
  applyPromptTextWithColor(Value, fPromptTextColor);
end;

{*********************************************************}
function TALMacEditControl.GetPromptTextColor: TAlphaColor;
begin
  result := fPromptTextColor;
end;

{***********************************************************************}
procedure TALMacEditControl.setPromptTextColor(const Value: TAlphaColor);
begin
  if Value <> fPromptTextColor then begin
    fPromptTextColor := Value;
    applyPromptTextWithColor(GetPromptText, fPromptTextColor);
  end;
end;

{**************************************************************************************************}
procedure TALMacEditControl.applyPromptTextWithColor(const aStr: String; const aColor: TAlphaColor);
begin
  //Using `setPlaceholderString` is not suitable here because it does not allow customizing the placeholder text color,
  //which depends on the OS's light or dark mode. However, this application requires a placeholder color that is
  //independent of the system theme, as the background color does not adapt to the dark or light mode settings.
  //if (aColor = tAlphaColors.Null) or aStr.IsEmpty then FTextField.View.SetPlaceholderString(StrToNSStr(aStr))
  //else begin

  var LPromptTextAttr: NSMutableAttributedString := TNSMutableAttributedString.Wrap(TNSMutableAttributedString.Alloc.initWithString(StrToNSStr(aStr)));
  try

    if not aStr.IsEmpty then begin
      LPromptTextAttr.beginEditing;
      try
        var LTextRange := NSMakeRange(0, aStr.Length);
        var LNSColor: NSColor;
        if aColor <> tAlphaColors.Null then LNSColor := AlphaColorToNSColor(aColor)
        else LNSColor := AlphaColorToNSColor(
                           TAlphaColorF.Create(
                             ALSetColorAlpha(TextSettings.Font.Color, 0.5)).
                               PremultipliedAlpha.
                               ToAlphaColor);
        LPromptTextAttr.addAttribute(NSForegroundColorAttributeName, NSObjectToID(LNSColor), LTextRange);
        //NOTE: If I try to release the LNSColor I have an exception

        // No need to do this in iOS, only in MacOS
        var LParagraphStyle := TNSMutableParagraphStyle.Alloc;
        try
          LParagraphStyle.init;
          LParagraphStyle.setAlignment(ALTextHorzAlignToNSTextAlignment(TextSettings.HorzAlign));
          LPromptTextAttr.addAttribute(NSParagraphStyleAttributeName, NSObjectToID(LParagraphStyle), LTextRange);
        finally
          LParagraphStyle.release;
        end;

        // No need to do this in iOS, only in MacOS
        var LFontRef := ALCreateCTFontRef(TextSettings.Font.Family, TextSettings.Font.Size, TextSettings.Font.Weight, TextSettings.Font.Slant);
        try
          LPromptTextAttr.addAttribute(NSFontAttributeName, NSObjectToID(TNSFont.Wrap(LFontRef)), LTextRange);
        finally
          CFRelease(LFontRef);
        end;
      finally
        LPromptTextAttr.endEditing;
      end;
    end;

    TALNSTextField.Wrap(NSObjectToID(NativeView.View)).setPlaceholderAttributedString(LPromptTextAttr);

  finally
    LPromptTextAttr.release;
  end;
end;

{***************************************************}
function TALMacEditControl.GetTintColor: TAlphaColor;
begin
  Result := FTintColor;
end;

{*****************************************************************}
procedure TALMacEditControl.setTintColor(const Value: TAlphaColor);
begin
  FTintColor := Value;
end;

{***************************************************}
function TALMacEditControl.GetFillColor: TAlphaColor;
begin
  Result := FFillColor;
end;

{*****************************************************************}
procedure TALMacEditControl.SetFillColor(const Value: TAlphaColor);
begin
  FFillColor := Value;
end;

{*****************************************}
function TALMacEditControl.getText: String;
begin
  result := NSStrToStr(NativeView.View.StringValue);
end;

{*******************************************************}
procedure TALMacEditControl.SetText(const Value: String);
begin
  NativeView.View.setStringValue(StrToNSStr(Value));
end;

{***************************************************************}
procedure TALMacEditControl.TextSettingsChanged(Sender: TObject);
begin
  // Font
  var LFontRef := ALCreateCTFontRef(TextSettings.Font.Family, TextSettings.Font.Size, TextSettings.Font.Weight, TextSettings.Font.Slant);
  try
    NativeView.View.setFont(TNSFont.Wrap(LFontRef));
  finally
    CFRelease(LFontRef);
  end;

  // TextAlignment
  NativeView.View.setAlignment(ALTextHorzAlignToNSTextAlignment(TextSettings.HorzAlign));

  // TextColor
  NativeView.View.setTextColor(AlphaColorToNSColor(TextSettings.Font.Color));

  // Update the PromptText with the new font settings. This is only necessary in macOS.
  // In iOS, this step is not required.
  applyPromptTextWithColor(PromptText, PromptTextColor);
end;

{***********************************************}
function TALMacEditControl.GetMaxLength: integer;
begin
  Result := FMaxLength;
end;

{*************************************************************}
procedure TALMacEditControl.SetMaxLength(const Value: integer);
begin
  FMaxLength := Value;
end;

{***********************************************}
function TALMacEditControl.getLineHeight: Single;
begin
  var LfontMetrics := ALGetFontMetrics(
                        TextSettings.Font.Family, // const AFontFamily: String;
                        TextSettings.Font.Size, // const AFontSize: single;
                        TextSettings.Font.Weight, // const AFontWeight: TFontWeight;
                        TextSettings.Font.Slant); // const AFontSlant: TFontSlant;
  result := -LfontMetrics.Ascent + LfontMetrics.Descent + LfontMetrics.Leading;
  if not SameValue(textsettings.LineHeightMultiplier, 0, TEpsilon.Scale) then
    result := result * textsettings.LineHeightMultiplier;
end;

{************************************************************************************}
Procedure TALMacEditControl.setSelection(const AStart: integer; const AStop: Integer);
begin
  NativeView.View.currentEditor.setSelectedRange(NSMakeRange(AStart, AStop-AStart));
end;

{**************************************************************}
Procedure TALMacEditControl.setSelection(const AIndex: integer);
begin
  NativeView.View.currentEditor.setSelectedRange(NSMakeRange(AIndex, 0));
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
  fEditControl := TALWinEditControl(AControl);
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

{**********************}
{$IF not defined(ALDPK)}
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
{$ENDIF}

{**********************}
{$IF not defined(ALDPK)}
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
{$ENDIF}

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
  if Message.CharCode = VK_RETURN then fEditControl.DoReturnKey
  else if Message.CharCode = VK_DELETE then fEditControl.DoChange;
end;

{******************************************************************}
procedure TALWinEditView.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  invalidate;
end;

{**************************************************************}
procedure TALWinEditView.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  invalidate;
end;

{**************************************************************}
procedure TALWinEditView.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;
  invalidate;
end;

{****************************************************}
procedure TALWinEditView.WMChar(var Message: TWMChar);
begin
  inherited;
  invalidate;
  fEditControl.DoChange;
end;

{**********************************************************}
procedure TALWinEditView.WMSetText(var Message: TWMSetText);
begin
  inherited;
  invalidate;
  fEditControl.DoChange;
end;

{******************************************************}
procedure TALWinEditView.WMPaste(var Message: TWMPaste);
begin
  inherited;
  invalidate;
  fEditControl.DoChange;
end;

{**************************************************}
procedure TALWinEditView.WMCut(var Message: TWMCut);
begin
  inherited;
  invalidate;
  fEditControl.DoChange;
end;

{******************************************************}
procedure TALWinEditView.WMClear(var Message: TWMClear);
begin
  inherited;
  invalidate;
  fEditControl.DoChange;
end;

{****************************************************}
procedure TALWinEditView.WMUndo(var Message: TWMUndo);
begin
  inherited;
  invalidate;
  fEditControl.DoChange;
end;

{**************************************************************************}
procedure TALWinEditView.WMTextColor(var Message: WinApi.Messages.TMessage);
begin
  inherited;
  if SetTextColor(Message.wParam, TAlphaColors.ColorToRGB(FeditControl.TextSettings.Font.Color)) = CLR_INVALID then RaiseLastOSError;
  if fBackgroundBrush <> 0 then begin
    if SetBkColor(Message.wParam, TAlphaColors.ColorToRGB(FEditControl.FillColor)) = CLR_INVALID then RaiseLastOSError;
    Message.Result := LRESULT(FBackgroundBrush);
  end
  else
    Message.Result := GetSysColorBrush(COLOR_WINDOW);
end;

{******************************************************}
procedure TALWinEditView.WMPaint(var Message: TWMPaint);
begin
  if (FEditControl.PromptText <> '') and
     (GetWindowTextLength(Handle) = 0) then begin
    var LPS: PAINTSTRUCT;
    var LDC := BeginPaint(Handle, LPS);
    if LDC = 0 then raise Exception.Create('Error 3B053C24-4A18-497C-82B1-C540EF7C2A4B');
    Try
      var LPromptTextColor := FeditControl.PromptTextColor;
      if LPromptTextColor = TAlphaColors.Null then
        LPromptTextColor := ALBlendColor(FEditControl.fillColor, FeditControl.TextSettings.Font.Color, 0.3);
      if SetTextColor(LDC, TAlphaColors.ColorToRGB(LPromptTextColor)) = CLR_INVALID then RaiseLastOSError;
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
      case FeditControl.TextSettings.HorzAlign of
        TALTextHorzAlign.Center: begin
          var LTextSize: TSize;
          GetTextExtentPoint32(LDC, Pchar(FEditControl.PromptText), Length(FEditControl.PromptText), LTextSize);
          TextOut(LDC, round((FeditControl.Width - LtextSize.cx) / 2), 0, Pchar(FEditControl.PromptText), Length(FEditControl.PromptText));
        end;
        TALTextHorzAlign.Leading,
        TALTextHorzAlign.Justify:  begin
          TextOut(LDC, round(LOWORD(LMargins) * ALGetScreenScale), 0, Pchar(FEditControl.PromptText), Length(FEditControl.PromptText));
        end;
        TALTextHorzAlign.Trailing: begin
          var LTextSize: TSize;
          GetTextExtentPoint32(LDC, Pchar(FEditControl.PromptText), Length(FEditControl.PromptText), LTextSize);
          TextOut(LDC, round(FeditControl.Width - LtextSize.cx - HIWORD(LMargins)), 0, Pchar(FEditControl.PromptText), Length(FEditControl.PromptText));
        end;
        else
          raise Exception.Create('Error 21CC0DF5-9030-4F6C-9830-112E17E1A392');
      end;

    finally
      EndPaint(Handle, LPS);
    end;
  end
  else
    inherited;
end;

{*******************************************************}
constructor TALWinEditControl.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FFillColor := $ffffffff;
  FPromptText := '';
  FPromptTextColor := TalphaColors.Null;
  fReturnKeyType := tReturnKeyType.Default;
  fKeyboardType := TVirtualKeyboardType.default;
  fAutoCapitalizationType := TALAutoCapitalizationType.acNone;
  fCheckSpelling := true;
  FTintColor := TalphaColors.Null;
  {$IF defined(ALDPK)}
  FPassword := False;
  FMaxLength := 0;
  FText := '';
  {$ENDIF}
  SetPassword(false);
end;

{************************************************************}
Function TALWinEditControl.CreateNativeView: TALWinNativeView;
begin
  {$IF defined(ALDPK)}
  Result := nil;
  {$ELSE}
  result := TALWinEditView.create(self);
  {$ENDIF}
end;

{*******************************************************}
function TALWinEditControl.GetNativeView: TALWinEditView;
begin
  result := TALWinEditView(inherited GetNativeView);
end;

{***************************************************************}
function TALWinEditControl.GetKeyboardType: TVirtualKeyboardType;
begin
  Result := FKeyboardType;
end;

{*****************************************************************************}
procedure TALWinEditControl.setKeyboardType(const Value: TVirtualKeyboardType);
begin
  FKeyboardType := Value;
end;

{******************************************************************************}
function TALWinEditControl.GetAutoCapitalizationType: TALAutoCapitalizationType;
begin
  result := fAutoCapitalizationType;
end;

{********************************************************************************************}
procedure TALWinEditControl.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  if (value <> fAutoCapitalizationType) then begin
    fAutoCapitalizationType := Value;
    {$IF not defined(ALDPK)}
    var LStyle := GetWindowLong(NativeView.Handle, GWL_STYLE);
    if LStyle = 0 then RaiseLastOsError;
    case fAutoCapitalizationType of
      TALAutoCapitalizationType.acNone:          LStyle := LStyle and not ES_UPPERCASE;
      TALAutoCapitalizationType.acWords:         LStyle := LStyle and not ES_UPPERCASE;
      TALAutoCapitalizationType.acSentences:     LStyle := LStyle and not ES_UPPERCASE;
      TALAutoCapitalizationType.acAllCharacters: LStyle := LStyle or ES_UPPERCASE;
      else raise Exception.Create('Error 21CC0DF5-9030-4F6C-9830-112E17E1A392');
    end;
    if SetWindowLong(NativeView.Handle, GWL_STYLE, LStyle) = 0 then RaiseLastOsError;
    {$ENDIF}
  end;
end;

{************************************************************}
procedure TALWinEditControl.setPassword(const Value: Boolean);
begin
  {$IF defined(ALDPK)}
  FPassword := Value;
  {$ELSE}
  if (value <> Password) then begin
    if Value then SendMessage(NativeView.Handle, EM_SETPASSWORDCHAR, Ord('*'), 0)
    else SendMessage(NativeView.Handle, EM_SETPASSWORDCHAR, 0, 0);
    NativeView.Invalidate;
  end;
  {$ENDIF}
end;

{**********************************************}
function TALWinEditControl.GetPassword: Boolean;
begin
  {$IF defined(ALDPK)}
  Result := FPassword;
  {$ELSE}
  Result := SendMessage(NativeView.Handle, EM_GETPASSWORDCHAR, 0, 0) <> 0;
  {$ENDIF}
end;

{*************************************************************}
procedure TALWinEditControl.SetMaxLength(const Value: integer);
begin
  {$IF defined(ALDPK)}
  FMaxLength := Value;
  {$ELSE}
  SendMessage(NativeView.Handle, EM_LIMITTEXT, Value, 0);
  {$ENDIF}
end;

{***********************************************}
function TALWinEditControl.GetMaxLength: integer;
begin
  {$IF defined(ALDPK)}
  Result := FMaxLength;
  {$ELSE}
  Result := SendMessage(NativeView.Handle, EM_GETLIMITTEXT, 0, 0);
  if Result = $7FFFFFFE {2147483646} then Result := 0;
  {$ENDIF}
end;

{***************************************************}
function TALWinEditControl.GetCheckSpelling: Boolean;
begin
  result := FCheckSpelling
end;

{*****************************************************************}
procedure TALWinEditControl.setCheckSpelling(const Value: Boolean);
begin
  FCheckSpelling := Value;
end;

{**********************************************************}
function TALWinEditControl.GetReturnKeyType: TReturnKeyType;
begin
  Result := FReturnKeyType;
end;

{************************************************************************}
procedure TALWinEditControl.setReturnKeyType(const Value: TReturnKeyType);
begin
  FReturnKeyType := Value;
end;

{***********************************************}
function TALWinEditControl.GetPromptText: String;
begin
  Result := FPromptText;
end;

{*************************************************************}
procedure TALWinEditControl.setPromptText(const Value: String);
begin
  if FPromptText <> Value then begin
    FPromptText := Value;
    {$IF not defined(ALDPK)}
    NativeView.Invalidate;
    {$ENDIF}
  end;
end;

{*********************************************************}
function TALWinEditControl.GetPromptTextColor: TAlphaColor;
begin
  Result := FPromptTextColor;
end;

{***********************************************************************}
procedure TALWinEditControl.setPromptTextColor(const Value: TAlphaColor);
begin
  if Value <> FPromptTextColor then begin
    FPromptTextColor := Value;
    {$IF not defined(ALDPK)}
    NativeView.Invalidate;
    {$ENDIF}
  end;
end;

{***************************************************}
function TALWinEditControl.GetTintColor: TAlphaColor;
begin
  Result := FTintColor;
end;

{*****************************************************************}
procedure TALWinEditControl.setTintColor(const Value: TAlphaColor);
begin
  FTintColor := Value;
end;

{***************************************************}
function TALWinEditControl.GetFillColor: TAlphaColor;
begin
  Result := FFillColor;
end;

{*****************************************************************}
procedure TALWinEditControl.SetFillColor(const Value: TAlphaColor);
begin
  if FFillColor <> Value then begin
    FFillColor := Value;
    {$IF not defined(ALDPK)}
    NativeView.UpdateBackgroundBrush;
    NativeView.Invalidate;
    {$ENDIF}
  end;
end;

{*****************************************}
function TALWinEditControl.getText: String;
begin
  {$IF defined(ALDPK)}
  Result := FText;
  {$ELSE}
  Result := GetHWNDText(NativeView.Handle);
  {$ENDIF}
end;

{*******************************************************}
procedure TALWinEditControl.SetText(const Value: String);
begin
  {$IF defined(ALDPK)}
  FText := Value;
  {$ELSE}
  SetWindowText(NativeView.Handle, PChar(Value));
  {$ENDIF}
end;

{***************************************************************}
procedure TALWinEditControl.TextSettingsChanged(Sender: TObject);
begin
  {$IF not defined(ALDPK)}
  NativeView.UpdateFontHandle;
  //--
  var LStyle := GetWindowLong(NativeView.Handle, GWL_STYLE);
  if LStyle = 0 then RaiseLastOsError;
  case TextSettings.HorzAlign of
      TALTextHorzAlign.Center:   LStyle := LStyle or ES_CENTER;
      TALTextHorzAlign.Leading:  LStyle := LStyle or ES_LEFT;
      TALTextHorzAlign.Trailing: LStyle := LStyle or ES_RIGHT;
      TALTextHorzAlign.Justify:  LStyle := LStyle or ES_LEFT;
    else raise Exception.Create('Error 21CC0DF5-9030-4F6C-9830-112E17E1A392');
  end;
  if SetWindowLong(NativeView.Handle, GWL_STYLE, LStyle) = 0 then RaiseLastOsError;
  SendMessage(NativeView.Handle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, MakeLParam(0, 0));
  //--
  NativeView.Invalidate;
  {$ENDIF}
end;

{***********************************************}
function TALWinEditControl.getLineHeight: Single;
begin
  var LfontMetrics := ALGetFontMetrics(
                        TextSettings.Font.Family, // const AFontFamily: String;
                        TextSettings.Font.Size, // const AFontSize: single;
                        TextSettings.Font.Weight, // const AFontWeight: TFontWeight;
                        TextSettings.Font.Slant); // const AFontSlant: TFontSlant;
  result := -LfontMetrics.Ascent + LfontMetrics.Descent + LfontMetrics.Leading;
  // The classic Edit control doesn't natively support line spacing adjustments
  // if not SameValue(textsettings.LineHeightMultiplier, 0, TEpsilon.Scale) then
  //   result := result * textsettings.LineHeightMultiplier;
end;

{************************************************************************************}
Procedure TALWinEditControl.SetSelection(const AStart: integer; const AStop: Integer);
begin
  SendMessage(NativeView.Handle, EM_SETSEL, AStart, AStop);
end;

{**************************************************************}
Procedure TALWinEditControl.SetSelection(const AIndex: integer);
begin
  SendMessage(NativeView.Handle, EM_SETSEL, AIndex, AIndex);
end;

{$endif}
{$ENDREGION}

{********************************************************}
function TALBaseEdit.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := $FF7a7a7a;
end;

{**************************************************************}
function TALBaseEdit.TTextSettings.TFont.GetDefaultSize: Single;
begin
  Result := 16;
end;

{*****************************************************}
function TALBaseEdit.TTextSettings.CreateFont: TALFont;
begin
  Result := TFont.create;
end;

{***********************************************************************}
function TALBaseEdit.TLabelTextSettings.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(0,0,0,-4);
end;

{*******************************************************************}
function TALBaseEdit.TLabelTextSettings.TFont.GetDefaultSize: Single;
begin
  Result := 12;
end;

{************************************************}
constructor TALBaseEdit.TLabelTextSettings.Create;
begin
  inherited create;
  FMargins := CreateMargins;
  FMargins.OnChange := MarginsChanged;
  FLayout := TLabelTextLayout.Floating;
  FAnimation := TLabelTextAnimation.Translation;
end;

{************************************************}
destructor TALBaseEdit.TLabelTextSettings.Destroy;
begin
  ALFreeAndNil(FMargins);
  Inherited Destroy;
end;

{***************************************************************}
function TALBaseEdit.TLabelTextSettings.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{**********************************************************}
function TALBaseEdit.TLabelTextSettings.CreateFont: TALFont;
begin
  Result := TFont.create;
end;

{*******************************************************************}
procedure TALBaseEdit.TLabelTextSettings.Assign(Source: TPersistent);
begin
  if Source is TLabelTextSettings then begin
    BeginUpdate;
    Try
      Margins.Assign(TLabelTextSettings(Source).margins);
      Layout := TLabelTextSettings(Source).Layout;
      Animation := TLabelTextSettings(Source).Animation;
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{*********************************************}
procedure TALBaseEdit.TLabelTextSettings.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Margins.Rect := Margins.DefaultValue;
    Layout := TLabelTextLayout.Floating;
    Animation := TLabelTextAnimation.Translation;
  finally
    EndUpdate;
  end;
end;

{****************************************************}
procedure TALBaseEdit.TLabelTextSettings.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    Margins.Rect := ALAlignEdgesToPixelRound(Margins.Rect, ALGetScreenScale, TEpsilon.Position);
  finally
    EndUpdate;
  end;
end;

{**************************************************************************}
procedure TALBaseEdit.TLabelTextSettings.SetMargins(const Value: TALBounds);
begin
  FMargins.Assign(Value);
end;

{********************************************************************************}
procedure TALBaseEdit.TLabelTextSettings.SetLayout(const Value: TLabelTextLayout);
begin
  if FLayout <> Value then begin
    FLayout := Value;
    Change;
  end;
end;

{**************************************************************************************}
procedure TALBaseEdit.TLabelTextSettings.SetAnimation(const Value: TLabelTextAnimation);
begin
  if FAnimation <> Value then begin
    FAnimation := Value;
    Change;
  end;
end;

{***********************************************************************}
procedure TALBaseEdit.TLabelTextSettings.MarginsChanged(Sender: TObject);
begin
  Change;
end;

{****************************************************************************}
function TALBaseEdit.TSupportingTextSettings.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(0,4,0,0);
end;

{************************************************************************}
function TALBaseEdit.TSupportingTextSettings.TFont.GetDefaultSize: Single;
begin
  Result := 12;
end;

{*****************************************************}
constructor TALBaseEdit.TSupportingTextSettings.Create;
begin
  inherited create;
  FMargins := CreateMargins;
  FMargins.OnChange := MarginsChanged;
  FLayout := TSupportingTextLayout.Floating;
end;

{*****************************************************}
destructor TALBaseEdit.TSupportingTextSettings.Destroy;
begin
  ALFreeAndNil(FMargins);
  Inherited Destroy;
end;

{********************************************************************}
function TALBaseEdit.TSupportingTextSettings.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{***************************************************************}
function TALBaseEdit.TSupportingTextSettings.CreateFont: TALFont;
begin
  Result := TFont.create;
end;

{************************************************************************}
procedure TALBaseEdit.TSupportingTextSettings.Assign(Source: TPersistent);
begin
  if Source is TSupportingTextSettings then begin
    BeginUpdate;
    Try
      Margins.Assign(TSupportingTextSettings(Source).margins);
      Layout := TSupportingTextSettings(Source).Layout;
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**************************************************}
procedure TALBaseEdit.TSupportingTextSettings.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Margins.Rect := Margins.DefaultValue;
    Layout := TSupportingTextLayout.Floating;
  finally
    EndUpdate;
  end;
end;

{*********************************************************}
procedure TALBaseEdit.TSupportingTextSettings.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    Margins.Rect := ALAlignEdgesToPixelRound(Margins.Rect, ALGetScreenScale, TEpsilon.Position);
  finally
    EndUpdate;
  end;
end;

{*******************************************************************************}
procedure TALBaseEdit.TSupportingTextSettings.SetMargins(const Value: TALBounds);
begin
  FMargins.Assign(Value);
end;

{******************************************************************************************}
procedure TALBaseEdit.TSupportingTextSettings.SetLayout(const Value: TSupportingTextLayout);
begin
  if FLayout <> Value then begin
    FLayout := Value;
    Change;
  end;
end;

{****************************************************************************}
procedure TALBaseEdit.TSupportingTextSettings.MarginsChanged(Sender: TObject);
begin
  Change;
end;

{************************************************************************}
function TALBaseEdit.TBaseStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := $FF7a7a7a;
end;

{******************************************************************************}
function TALBaseEdit.TBaseStateStyle.TTextSettings.TFont.GetDefaultSize: Single;
begin
  Result := 16;
end;

{*********************************************************************}
function TALBaseEdit.TBaseStateStyle.TTextSettings.CreateFont: TALFont;
begin
  Result := TFont.create;
end;

{***********************************************************************************}
function TALBaseEdit.TBaseStateStyle.TLabelTextSettings.TFont.GetDefaultSize: Single;
begin
  Result := 12;
end;

{**************************************************************************}
function TALBaseEdit.TBaseStateStyle.TLabelTextSettings.CreateFont: TALFont;
begin
  Result := TFont.create;
end;

{****************************************************************************************}
function TALBaseEdit.TBaseStateStyle.TSupportingTextSettings.TFont.GetDefaultSize: Single;
begin
  Result := 12;
end;

{*******************************************************************************}
function TALBaseEdit.TBaseStateStyle.TSupportingTextSettings.CreateFont: TALFont;
begin
  Result := TFont.create;
end;

{*********************************************************************}
constructor TALBaseEdit.TBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  //--
  FPromptTextColor := DefaultPromptTextColor;
  FTintColor := DefaultTintColor;
  //--
  if StateStyleParent <> nil then begin
    FTextSettings := CreateTextSettings(StateStyleParent.TextSettings);
    FLabelTextSettings := CreateLabelTextSettings(StateStyleParent.LabelTextSettings);
    FSupportingTextSettings := CreateSupportingTextSettings(StateStyleParent.SupportingTextSettings);
  end
  else if ControlParent <> nil then begin
    FTextSettings := CreateTextSettings(ControlParent.TextSettings);
    FLabelTextSettings := CreateLabelTextSettings(ControlParent.LabelTextSettings);
    FSupportingTextSettings := CreateSupportingTextSettings(ControlParent.SupportingTextSettings);
  end
  else begin
    FTextSettings := CreateTextSettings(nil);
    FLabelTextSettings := CreateLabelTextSettings(nil);
    FSupportingTextSettings := CreateSupportingTextSettings(nil);
  end;
  //--
  FTextSettings.OnChanged := TextSettingsChanged;
  FLabelTextSettings.OnChanged := LabelTextSettingsChanged;
  FSupportingTextSettings.OnChanged := SupportingTextSettingsChanged;
  //--
  BufPromptTextDrawable := ALNullDrawable;
  //BufPromptTextDrawableRect: TRectF;
  BufLabelTextDrawable := ALNullDrawable;
  //BufLabelTextDrawableRect: TRectF;
  BufSupportingTextDrawable := ALNullDrawable;
  //BufSupportingTextDrawableRect: TRectF;
  //--
  //FPriorSupersedePromptTextColor
  //FPriorSupersedeTintColor
end;

{*********************************************}
destructor TALBaseEdit.TBaseStateStyle.Destroy;
begin
  ALFreeAndNil(FTextSettings);
  ALFreeAndNil(FLabelTextSettings);
  ALFreeAndNil(FSupportingTextSettings);
  inherited Destroy;
end;

{******************************************************************************************************}
function TALBaseEdit.TBaseStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{*************************************************************************************************************************}
function TALBaseEdit.TBaseStateStyle.CreateTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TTextSettings;
begin
  result := TTextSettings.Create(AParent);
end;

{***********************************************************************************************************************************}
function TALBaseEdit.TBaseStateStyle.CreateLabelTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TLabelTextSettings;
begin
  result := TLabelTextSettings.Create(AParent);
end;

{*********************************************************************************************************************************************}
function TALBaseEdit.TBaseStateStyle.CreateSupportingTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TSupportingTextSettings;
begin
  result := TSupportingTextSettings.Create(AParent);
end;

{****************************************************************}
procedure TALBaseEdit.TBaseStateStyle.Assign(Source: TPersistent);
begin
  if Source is TBaseStateStyle then begin
    BeginUpdate;
    Try
      PromptTextColor := TBaseStateStyle(Source).PromptTextColor;
      TintColor := TBaseStateStyle(Source).TintColor;
      TextSettings.Assign(TBaseStateStyle(Source).TextSettings);
      LabelTextSettings.Assign(TBaseStateStyle(Source).LabelTextSettings);
      SupportingTextSettings.Assign(TBaseStateStyle(Source).SupportingTextSettings);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;


{******************************************}
procedure TALBaseEdit.TBaseStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    PromptTextColor := DefaultPromptTextColor;
    TintColor := DefaultTintColor;
    TextSettings.Reset;
    LabelTextSettings.Reset;
    SupportingTextSettings.Reset;
  finally
    EndUpdate;
  end;
end;

{*************************************************}
procedure TALBaseEdit.TBaseStateStyle.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    TextSettings.AlignToPixel;
    LabelTextSettings.AlignToPixel;
    SupportingTextSettings.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{*****************************************************}
procedure TALBaseEdit.TBaseStateStyle.ClearBufDrawable;
begin
  inherited;
  ClearBufPromptTextDrawable;
  ClearBufLabelTextDrawable;
  ClearBufSupportingTextDrawable
end;

{***************************************************************}
procedure TALBaseEdit.TBaseStateStyle.ClearBufPromptTextDrawable;
begin
  ALFreeAndNilDrawable(BufPromptTextDrawable);
end;

{**************************************************************}
procedure TALBaseEdit.TBaseStateStyle.ClearBufLabelTextDrawable;
begin
  ALFreeAndNilDrawable(BufLabelTextDrawable);
end;

{*******************************************************************}
procedure TALBaseEdit.TBaseStateStyle.ClearBufSupportingTextDrawable;
begin
  ALFreeAndNilDrawable(BufSupportingTextDrawable);
end;

{*************************************************************************************************************}
procedure TALBaseEdit.TBaseStateStyle.Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single);
begin
  {$IF defined(debug)}
  if (ATo <> nil) and (not (ATo is TBaseStateStyle)) then
    Raise Exception.Create('Error 80F4A6FB-B9A3-4D69-B304-1E1DC2047851');
  {$ENDIF}
  BeginUpdate;
  Try
    inherited Interpolate(ATo, ANormalizedTime);
    if ATo <> nil then begin
      PromptTextColor := ALInterpolateColor(PromptTextColor{Start}, TBaseStateStyle(ATo).PromptTextColor{Stop}, ANormalizedTime);
      TextSettings.Interpolate(TBaseStateStyle(ATo).TextSettings, ANormalizedTime);
      TintColor := ALInterpolateColor(TintColor{Start}, TBaseStateStyle(ATo).TintColor{Stop}, ANormalizedTime);
    end
    {$IF defined(debug)}
    else if StateStyleParent <> nil then Raise Exception.Create('Error FE80E1DC-6F9F-42BF-825B-C72A12A704B8')
    {$ENDIF}
    else if ControlParent <> nil then begin
      PromptTextColor := ALInterpolateColor(PromptTextColor{Start}, ControlParent.PromptTextColor{Stop}, ANormalizedTime);
      TextSettings.Interpolate(ControlParent.TextSettings, ANormalizedTime);
      TintColor := ALInterpolateColor(TintColor{Start}, ControlParent.TintColor{Stop}, ANormalizedTime);
    end
    else begin
      PromptTextColor := ALInterpolateColor(PromptTextColor{Start}, DefaultPromptTextColor{Stop}, ANormalizedTime);
      TextSettings.Interpolate(nil, ANormalizedTime);
      TintColor := ALInterpolateColor(TintColor{Start}, DefaultTintColor{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{************************************************}
procedure TALBaseEdit.TBaseStateStyle.DoSupersede;
begin
  inherited;
  //--
  FPriorSupersedePromptTextColor := FPromptTextColor;
  FPriorSupersedeTintColor := FTintColor;
  //--
  if StateStyleParent <> nil then begin
    if PromptTextColor = TAlphaColors.Null then PromptTextColor := StateStyleParent.PromptTextColor;
    if TintColor = TAlphaColors.Null then TintColor := StateStyleParent.TintColor;
  end
  else if ControlParent <> nil then begin
    if PromptTextColor = TAlphaColors.Null then PromptTextColor := ControlParent.PromptTextColor;
    if TintColor = TAlphaColors.Null then TintColor := ControlParent.TintColor;
  end;
  TextSettings.Supersede;
  LabelTextSettings.Supersede;
  SupportingTextSettings.Supersede;
end;

{************************************************************************}
function TALBaseEdit.TBaseStateStyle.GetStateStyleParent: TBaseStateStyle;
begin
  {$IF defined(debug)}
  if (inherited StateStyleParent <> nil) and
     (not (inherited StateStyleParent is TBaseStateStyle)) then
    raise Exception.Create('StateStyleParent must be of type TBaseStateStyle');
  {$ENDIF}
  result := TBaseStateStyle(inherited StateStyleParent);
end;

{*****************************************************************}
function TALBaseEdit.TBaseStateStyle.GetControlParent: TALBaseEdit;
begin
  {$IF defined(debug)}
  if (inherited ControlParent <> nil) and
     (not (inherited ControlParent is TALBaseEdit)) then
    raise Exception.Create('ControlParent must be of type TALBaseEdit');
  {$ENDIF}
  result := TALBaseEdit(inherited ControlParent);
end;

{**********************************************************************************}
procedure TALBaseEdit.TBaseStateStyle.SetPromptTextColor(const AValue: TAlphaColor);
begin
  if FPromptTextColor <> AValue then begin
    FPromptTextColor := AValue;
    Change;
  end;
end;

{****************************************************************************}
procedure TALBaseEdit.TBaseStateStyle.SetTintColor(const AValue: TAlphaColor);
begin
  if FTintColor <> AValue then begin
    FTintColor := AValue;
    Change;
  end;
end;

{*************************************************************************************************}
procedure TALBaseEdit.TBaseStateStyle.SetTextSettings(const AValue: TBaseStateStyle.TTextSettings);
begin
  FTextSettings.Assign(AValue);
end;

{***********************************************************************************************************}
procedure TALBaseEdit.TBaseStateStyle.SetLabelTextSettings(const AValue: TBaseStateStyle.TLabelTextSettings);
begin
  FLabelTextSettings.Assign(AValue);
end;

{*********************************************************************************************************************}
procedure TALBaseEdit.TBaseStateStyle.SetSupportingTextSettings(const AValue: TBaseStateStyle.TSupportingTextSettings);
begin
  FSupportingTextSettings.Assign(AValue);
end;

{**************************************************************************}
function TALBaseEdit.TBaseStateStyle.GetDefaultPromptTextColor: TalphaColor;
begin
  Result := TAlphaColors.Null;
end;

{********************************************************************}
function TALBaseEdit.TBaseStateStyle.GetDefaultTintColor: TalphaColor;
begin
  Result := TAlphaColors.Null;
end;

{*******************************************************}
function TALBaseEdit.TBaseStateStyle.GetInherit: Boolean;
begin
  Result := inherited GetInherit and
            (PromptTextColor <> TalphaColors.Null) and
            (TintColor <> TalphaColors.Null) and
            TextSettings.Inherit and
            LabelTextSettings.Inherit and
            SupportingTextSettings.Inherit;
end;

{**************************************************************************}
procedure TALBaseEdit.TBaseStateStyle.TextSettingsChanged(ASender: TObject);
begin
  Change;
end;

{*******************************************************************************}
procedure TALBaseEdit.TBaseStateStyle.LabelTextSettingsChanged(ASender: TObject);
begin
  Change;
end;

{************************************************************************************}
procedure TALBaseEdit.TBaseStateStyle.SupportingTextSettingsChanged(ASender: TObject);
begin
  Change;
end;

{********************************************************************}
function TALBaseEdit.TBaseStateStyle.IsPromptTextColorStored: Boolean;
begin
  result := FPromptTextColor <> DefaultPromptTextColor;
end;

{**************************************************************}
function TALBaseEdit.TBaseStateStyle.IsTintColorStored: Boolean;
begin
  result := FTintColor <> DefaultTintColor;
end;

{****************************************************************}
function TALBaseEdit.TDisabledStateStyle.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{************************************************************************}
procedure TALBaseEdit.TDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{*************************************************************************}
constructor TALBaseEdit.TDisabledStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{********************************************************************}
procedure TALBaseEdit.TDisabledStateStyle.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TDisabledStateStyle then
      Opacity := TDisabledStateStyle(Source).Opacity
    else
      Opacity := TControl.DefaultDisabledOpacity;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{**********************************************}
procedure TALBaseEdit.TDisabledStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Opacity := TControl.DefaultDisabledOpacity;
  finally
    EndUpdate;
  end;
end;

{***********************************************************}
function TALBaseEdit.TDisabledStateStyle.GetInherit: Boolean;
begin
  // Opacity is not part of the GetInherit function because it updates the
  // disabledOpacity of the base control immediately every time it changes.
  // Essentially, it acts merely as a link to the disabledOpacity of the base control.
  Result := inherited GetInherit;
end;

{*********************************************************************}
constructor TALBaseEdit.TStateStyles.Create(const AParent: TALControl);
begin
  inherited Create(AParent);
  //--
  FDisabled := CreateDisabledStateStyle(AParent);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := CreateHoveredStateStyle(AParent);
  FHovered.OnChanged := HoveredChanged;
  //--
  FFocused := CreateFocusedStateStyle(AParent);
  FFocused.OnChanged := FocusedChanged;
end;

{******************************************}
destructor TALBaseEdit.TStateStyles.Destroy;
begin
  ALFreeAndNil(FDisabled);
  ALFreeAndNil(FHovered);
  ALFreeAndNil(FFocused);
  inherited Destroy;
end;

{******************************************************************************************************}
function TALBaseEdit.TStateStyles.CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle;
begin
  result := TDisabledStateStyle.Create(AParent);
end;

{****************************************************************************************************}
function TALBaseEdit.TStateStyles.CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle;
begin
  result := THoveredStateStyle.Create(AParent);
end;

{****************************************************************************************************}
function TALBaseEdit.TStateStyles.CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{*************************************************}
procedure TALBaseEdit.TStateStyles.StartTransition;
begin
  inherited;
  Parent.UpdateEditControlStyle;
end;

{*****************************************************************************}
procedure TALBaseEdit.TStateStyles.TransitionAnimationProcess(Sender: TObject);
begin
  inherited;
  Parent.UpdateEditControlStyle;
end;

{****************************************************************************}
procedure TALBaseEdit.TStateStyles.TransitionAnimationFinish(Sender: TObject);
begin
  inherited;
  Parent.UpdateEditControlStyle;
end;

{*************************************************************}
procedure TALBaseEdit.TStateStyles.Assign(Source: TPersistent);
begin
  if Source is TStateStyles then begin
    BeginUpdate;
    Try
      Disabled.Assign(TStateStyles(Source).Disabled);
      Hovered.Assign(TStateStyles(Source).Hovered);
      Focused.Assign(TStateStyles(Source).Focused);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{***************************************}
procedure TALBaseEdit.TStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.reset;
    Hovered.reset;
    Focused.reset;
  finally
    EndUpdate;
  end;
end;

{**********************************************}
procedure TALBaseEdit.TStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.AlignToPixel;
    Hovered.AlignToPixel;
    Focused.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{**************************************************}
procedure TALBaseEdit.TStateStyles.ClearBufDrawable;
begin
  inherited;
  Disabled.ClearBufDrawable;
  Hovered.ClearBufDrawable;
  Focused.ClearBufDrawable;
end;

{************************************************************}
procedure TALBaseEdit.TStateStyles.ClearBufPromptTextDrawable;
begin
  Disabled.ClearBufPromptTextDrawable;
  Hovered.ClearBufPromptTextDrawable;
  Focused.ClearBufPromptTextDrawable;
end;

{***********************************************************}
procedure TALBaseEdit.TStateStyles.ClearBufLabelTextDrawable;
begin
  Disabled.ClearBufLabelTextDrawable;
  Hovered.ClearBufLabelTextDrawable;
  Focused.ClearBufLabelTextDrawable;
end;

{****************************************************************}
procedure TALBaseEdit.TStateStyles.ClearBufSupportingTextDrawable;
begin
  Disabled.ClearBufSupportingTextDrawable;
  Hovered.ClearBufSupportingTextDrawable;
  Focused.ClearBufSupportingTextDrawable;
end;

{**********************************************************************}
function TALBaseEdit.TStateStyles.GetCurrentRawStyle: TALBaseStateStyle;
begin
  if Not Parent.Enabled then Result := Disabled
  else if Parent.IsFocused then Result := Focused
  else if Parent.IsMouseOver then Result := Hovered
  else result := nil;
end;

{*******************************************************}
function TALBaseEdit.TStateStyles.GetParent: TALBaseEdit;
begin
  Result := TALBaseEdit(inherited Parent);
end;

{********************************************************************************}
procedure TALBaseEdit.TStateStyles.SetDisabled(const AValue: TDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{******************************************************************************}
procedure TALBaseEdit.TStateStyles.SetHovered(const AValue: THoveredStateStyle);
begin
  FHovered.Assign(AValue);
end;

{******************************************************************************}
procedure TALBaseEdit.TStateStyles.SetFocused(const AValue: TFocusedStateStyle);
begin
  FFocused.Assign(AValue);
end;

{*******************************************************************}
procedure TALBaseEdit.TStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************}
procedure TALBaseEdit.TStateStyles.HoveredChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************}
procedure TALBaseEdit.TStateStyles.FocusedChanged(ASender: TObject);
begin
  Change;
end;

{*************************************************}
constructor TALBaseEdit.Create(AOwner: TComponent);
begin
  inherited;
  //--
  fDefStyleAttr := '';
  fDefStyleRes := '';
  FAutoTranslate := true;
  fOnChange := nil;
  FOnReturnKey := nil;
  //--
  FTextSettings := CreateTextSettings;
  FTextSettings.OnChanged := TextSettingsChanged;
  //--
  FPromptText := '';
  FPromptTextColor := TAlphaColors.null;
  //--
  FTintcolor := TAlphaColors.null;
  //--
  FLabelText := '';
  FLabelTextSettings := CreateLabelTextSettings;
  FLabelTextSettings.OnChanged := LabelTextSettingsChanged;
  FlabelTextAnimation := TALFloatAnimation.Create;
  FlabelTextAnimation.StartValue := 0;
  FlabelTextAnimation.StopValue := 1;
  FlabelTextAnimation.AnimationType := TanimationType.out;
  FlabelTextAnimation.Interpolation := TALInterpolationType.linear;
  FlabelTextAnimation.OnProcess := labelTextAnimationProcess;
  FlabelTextAnimation.OnFinish := labelTextAnimationFinish;
  //--
  FSupportingText := '';
  FSupportingTextSettings := CreateSupportingTextSettings;
  FSupportingTextSettings.OnChanged := SupportingTextSettingsChanged;
  FSupportingTextMarginBottomUpdated := False;
  //--
  FIsTextEmpty := True;
  FNativeViewRemoved := False;
  //--
  fBufPromptTextDrawable := ALNullDrawable;
  //fBufPromptTextDrawableRect
  fBufLabelTextDrawable := ALNullDrawable;
  //fBufLabelTextDrawableRect
  fBufSupportingTextDrawable := ALNullDrawable;
  //fBufSupportingTextDrawableRect
  //--
  {$IF defined(MSWindows) or defined(ALMacOS)}
  FocusOnMouseDown := True;
  {$ELSE}
  FocusOnMouseUp := True;
  {$ENDIF}
  Cursor := crIBeam;
  CanFocus := True;
  //--
  var LPaddingChange: TNotifyEvent := Padding.OnChange;
  Padding.OnChange := nil;
  Padding.DefaultValue := TRectF.create(8{Left}, 8{Top}, 8{Right}, 8{Bottom});
  Padding.Rect := Padding.DefaultValue;
  padding.OnChange := LPaddingChange;
  //--
  {$IF defined(ALDPK)}
  FPrevStateStyles := TStateStyles.Create(nil);
  {$ENDIF}
  FStateStyles := CreateStateStyles;
  FStateStyles.OnChanged := StateStylesChanged;
  //--
  {$IF defined(DEBUG)}
  if fEditControl <> nil then
    raise Exception.Create('Error CE2932F6-E44A-4A4B-AAE3-71FE4077FCF2');
  {$ENDIF}
  {$IF defined(android)}
  // In Android we must first know the value of DefStyleAttr/DefStyleRes
  // before to create the fEditControl. I use this way to know that the compoment
  // will load it's properties from the dfm
  if not IsOwnerLoading then begin
    fEditControl := CreateEditControl;
    InitEditControl;
  end
  else fEditControl := nil;
  {$ELSE}
  fEditControl := CreateEditControl;
  InitEditControl;
  {$ENDIF}
end;

{*****************************}
destructor TALBaseEdit.Destroy;
begin
  ALFreeAndNil(FTextSettings);
  ALFreeAndNil(FLabelTextSettings);
  ALFreeAndNil(FlabelTextAnimation);
  ALFreeAndNil(FSupportingTextSettings);
  {$IF defined(ALDPK)}
  ALFreeAndNil(FPrevStateStyles);
  {$ENDIF}
  ALFreeAndNil(FStateStyles);
  ALFreeAndNil(fEditControl);
  inherited Destroy;
end;

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALBaseEdit.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TextPrompt', ReadTextPrompt{ReadData}, nil{WriteData}, false{hasdata});
  Filer.DefineProperty('TextPromptColor', ReadTextPromptColor{ReadData}, nil{WriteData}, false{hasdata});
end;
{$ENDIF}

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALBaseEdit.ReadTextPrompt(Reader: TReader);
begin
  PromptText := Reader.ReadString;
end;
{$ENDIF}

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALBaseEdit.ReadTextPromptColor(Reader: TReader);
begin
  Var LColor: Integer;
  if IdentToAlphaColor(Reader.ReadIdent, Lcolor) then PromptTextColor := LColor;
end;
{$ENDIF}

{*********************************}
procedure TALBaseEdit.AlignToPixel;
begin
  beginUpdate;
  try
    inherited;
    TextSettings.AlignToPixel;
    LabelTextSettings.AlignToPixel;
    SupportingTextSettings.AlignToPixel;
    StateStyles.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{************************************************}
function TALBaseEdit.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{*****************************************************}
function TALBaseEdit.CreateTextSettings: TTextSettings;
begin
  result := TTextSettings.Create;
end;

{***************************************************************}
function TALBaseEdit.CreateLabelTextSettings: TLabelTextSettings;
begin
  result := TLabelTextSettings.Create;
end;

{*************************************************************************}
function TALBaseEdit.CreateSupportingTextSettings: TSupportingTextSettings;
begin
  result := TSupportingTextSettings.Create;
end;

{***************************************************}
function TALBaseEdit.CreateStateStyles: TStateStyles;
begin
  Result := TStateStyles.Create(Self);
end;

{*********************************************************}
function TALBaseEdit.CreateEditControl: TALBaseEditControl;
begin
  {$IF defined(android)}
  Result := TALAndroidEditControl.Create(self, false{aIsMultiline}, DefStyleAttr, DefStyleRes);
  {$ELSEIF defined(ios)}
  Result := TALIosEditControl.Create(self);
  {$ELSEIF defined(ALMacOS)}
  Result := TALMacEditControl.Create(self);
  {$ELSEIF defined(MSWindows)}
  Result := TALWinEditControl.Create(self);
  {$ELSE}
    Not implemented
  {$ENDIF}
end;

{************************************}
procedure TALBaseEdit.InitEditControl;
begin
  fEditControl.Parent := self;
  FeditControl.Stored := False;
  FeditControl.SetSubComponent(True);
  FeditControl.Locked := True;
  FeditControl.OnReturnKey := nil; // noops operation
  fEditControl.Align := TALAlignLayout.Client;
  FeditControl.OnChange := OnChangeImpl;
  fEditControl.Password := false; // noops operation
  fEditControl.ReturnKeyType := tReturnKeyType.Default;  // noops operation
  fEditControl.KeyboardType := TVirtualKeyboardType.Default; // noops operation
  fEditControl.AutoCapitalizationType := TALAutoCapitalizationType.acNone; // noops operation
  fEditControl.CheckSpelling := True;
  fEditControl.MaxLength := 0; // noops operation
  fEditControl.PromptTextColor := TALphaColors.Null; // noops operation
  fEditControl.TintColor := TALphaColors.Null; // noops operation
  fEditControl.FillColor := $ffffffff; // noops operation
  fEditControl.CanFocus := False;
  fEditControl.CanParentFocus := True;
  fEditControl.HitTest := False;
end;

{***************************}
procedure TALBaseEdit.Loaded;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _ConvertFontFamily(const AStateStyle: TBaseStateStyle);
  begin
    if (AStateStyle.TextSettings.Font.Family <> '') and
       (not (csDesigning in ComponentState)) then
      AStateStyle.TextSettings.Font.Family := ALConvertFontFamily(AStateStyle.TextSettings.Font.Family);
  end;

begin
  if FEditControl = nil then begin
    FEditControl := CreateEditControl;
    InitEditControl;
  end;
  //--
  // csLoading is in ComponentState
  if (TextSettings.Font.Family <> '') and
     (not (csDesigning in ComponentState)) then
    TextSettings.Font.Family := ALConvertFontFamily(TextSettings.Font.Family);
  //--
  if (LabelTextSettings.Font.Family <> '') and
     (not (csDesigning in ComponentState)) then
    LabelTextSettings.Font.Family := ALConvertFontFamily(LabelTextSettings.Font.Family);
  //--
  _ConvertFontFamily(StateStyles.Disabled);
  _ConvertFontFamily(StateStyles.Hovered);
  _ConvertFontFamily(StateStyles.Focused);
  //--
  // AdjustSize will be called in the following call to TextSettingsChanged(TextSettings)
  // therefore, we must deactivate it in the inherited method to avoid calling it twice.
  {$IF defined(debug)}
  if FIsAdjustingSize then
    Raise Exception.Create('Error D9247DBE-D093-415C-9537-11D7275EF141');
  {$ENDIF}
  FIsAdjustingSize := true;
  Try
    // remove csLoading from ComponentState
    inherited;
  Finally
    FIsAdjustingSize := False;
  End;
  //--
  if (AutoTranslate) and
     (PromptText <> '') and
     (not (csDesigning in ComponentState)) then
    PromptText := ALTranslate(PromptText);
  //--
  if (AutoTranslate) and
     (LabelText <> '') and
     (not (csDesigning in ComponentState)) then
    LabelText := ALTranslate(LabelText);
  //--
  if (AutoTranslate) and
     (SupportingText <> '') and
     (not (csDesigning in ComponentState)) then
    SupportingText := ALTranslate(SupportingText);
  //--
  FIsTextEmpty := GetText = '';
  //--
  TextSettingsChanged(TextSettings);
  //--
  //UpdateEditControlStyle => Already called in TextSettingsChanged
  //--
  UpdateEditControlPromptText;
  UpdateNativeViewVisibility;
  {$IF not defined(ALDPK)}
  if NativeView.visible then begin
    // Because AncestorParentChanged is not called during loading,
    // we must call NativeView.SetVisible(true) in TALBaseEdit.Loaded
    // to hide the NativeView in case a parent control is hidden.
    NativeView.SetVisible(true);
  end;
  {$ENDIF}
end;

{*********************************************************}
procedure TALBaseEdit.SetDefStyleAttr(const Value: String);
begin
  if Value <> fDefStyleAttr then begin
    fDefStyleAttr := Value;
    {$IFDEF ANDROID}
    if not (csLoading in componentState) then begin
      ALFreeAndNil(fEditControl);
      FEditControl := CreateEditControl;
      InitEditControl;
    end;
    {$ENDIF}
  end;
end;

{********************************************************}
procedure TALBaseEdit.SetDefStyleRes(const Value: String);
begin
  if Value <> fDefStyleRes then begin
    fDefStyleRes := Value;
    {$IFDEF ANDROID}
    if not (csLoading in componentState) then begin
      ALFreeAndNil(fEditControl);
      FEditControl := CreateEditControl;
      InitEditControl;
    end;
    {$ENDIF}
  end;
end;

{******************************************}
function TALBaseEdit.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(200, 50);
end;

{********************}
{$IF defined(android)}
function TALBaseEdit.GetEditControl: TALBaseEditControl;
begin
  if FEditControl = nil then begin
    FEditControl := CreateEditControl;
    InitEditControl;
  end;
  Result := FEditControl;
end;
{$ENDIF}

{********************}
{$IF defined(android)}
function TALBaseEdit.GetNativeView: TALAndroidNativeView;
begin
  result := EditControl.NativeView;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALBaseEdit.GetEditControl: TALBaseEditControl;
begin
  if FEditControl = nil then begin
    FEditControl := CreateEditControl;
    InitEditControl;
  end;
  Result := FEditControl;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALBaseEdit.GetNativeView: TALIosNativeView;
begin
  result := EditControl.NativeView;
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
function TALBaseEdit.GetEditControl: TALBaseEditControl;
begin
  if FEditControl = nil then begin
    FEditControl := CreateEditControl;
    InitEditControl;
  end;
  Result := FEditControl;
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
function TALBaseEdit.GetNativeView: TALMacNativeView;
begin
  result := EditControl.NativeView;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
function TALBaseEdit.GetEditControl: TALBaseEditControl;
begin
  if FEditControl = nil then begin
    FEditControl := CreateEditControl;
    InitEditControl;
  end;
  Result := FEditControl;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
function TALBaseEdit.GetNativeView: TALWinNativeView;
begin
  result := EditControl.NativeView;
end;
{$ENDIF}

{****************************}
procedure TALBaseEdit.DoEnter;
begin
  {$IF defined(DEBUG)}
  ALLog('TALBaseEdit.DoEnter', 'control.name: ' + Name);
  {$ENDIF}
  inherited DoEnter;
  StateStyles.startTransition;
  //--
  if (GetIsTextEmpty) and
     (HasTranslationLabelTextAnimation) then begin
    FlabelTextAnimation.StopAtCurrent;
    FlabelTextAnimation.Inverse := False;
    FlabelTextAnimation.Duration := 0.15;
    FLabelTextAnimation.Start;
  end;
  //--
  {$IF not defined(ALDPK)}
  if HasNativeView then
    NativeView.SetFocus;
  {$ENDIF}
  {$IF defined(android)}
  if IsFocused then begin
    ALVirtualKeyboardVisible := True;
    {$IF defined(DEBUG)}
    ALLog('TALBaseEdit.showVirtualKeyboard', 'control.name: ' + Name);
    {$ENDIF}
    MainActivity.getVirtualKeyboard.showFor(NativeView.View);
  end;
  {$ENDIF}
end;

{***************************}
procedure TALBaseEdit.DoExit;
begin
  {$IF defined(DEBUG)}
  ALLog('TALBaseEdit.DoExit', 'control.name: ' + Name);
  {$ENDIF}
  inherited DoExit;
  StateStyles.startTransition;
  UpdateNativeViewVisibility;
  //--
  if (GetIsTextEmpty) and
     (HasTranslationLabelTextAnimation) then begin
    FlabelTextAnimation.StopAtCurrent;
    FlabelTextAnimation.Inverse := True;
    FlabelTextAnimation.Duration := 0.10;
    FLabelTextAnimation.Start;
  end;
  //--
  {$IF not defined(ALDPK)}
  if HasNativeView then
    NativeView.ResetFocus;
  {$ENDIF}
  {$IF defined(android)}
  ALVirtualKeyboardVisible := False;
  TThread.ForceQueue(nil,
    procedure
    begin
      If not ALVirtualKeyboardVisible then begin
        {$IF defined(DEBUG)}
        ALLog('TALBaseEdit.hideVirtualKeyboard');
        {$ENDIF}
        MainActivity.getVirtualKeyboard.hide;
      end;
    end);
  {$ENDIF}
end;

{*******************************************}
procedure TALBaseEdit.UpdateEditControlStyle;
begin

  if (csLoading in ComponentState) then exit;

  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);

  // FillColor
  if LStateStyle <> nil then begin
    var LStateLayerColor: TAlphaColor;
    var LStateLayerOpacity: Single;
    if LStateStyle.StateLayer.UseContentColor then begin
      LStateLayerColor := LStateStyle.TextSettings.Font.Color;
      LStateLayerOpacity := LStateStyle.StateLayer.Opacity;
    end
    else begin
      LStateLayerColor := LStateStyle.StateLayer.Color;
      LStateLayerOpacity := LStateStyle.StateLayer.Opacity;
    end;
    EditControl.FillColor := ALBlendColor(LStateStyle.Fill.Color, LStateLayerColor, LStateLayerOpacity);
  end
  else EditControl.FillColor := Fill.Color;
  // PromptTextColor
  if LStateStyle <> nil then EditControl.PromptTextColor := LStateStyle.PromptTextColor
  else EditControl.PromptTextColor := PromptTextColor;
  // TintColor
  if LStateStyle <> nil then EditControl.TintColor := LStateStyle.TintColor
  else EditControl.TintColor := TintColor;
  // TextSettings
  if LStateStyle <> nil then EditControl.TextSettings.Assign(LStateStyle.TextSettings)
  else EditControl.TextSettings.Assign(TextSettings);

  repaint;

end;

{************************************************}
function TALBaseEdit.GetControlType: TControlType;
begin
  // We need ControlType because in function TFMXViewBase.canBecomeFirstResponder: Boolean;
  // we use it in IsNativeControl to determine if it's a native control or not
  Result := TControlType.Platform;
end;

{**************************************************************}
procedure TALBaseEdit.SetControlType(const Value: TControlType);
begin
  // The ControlType cannot be changed
end;

{***************************************************************}
procedure TALBaseEdit.LabelTextAnimationProcess(Sender: TObject);
begin
  Repaint;
end;

{**************************************************************}
procedure TALBaseEdit.LabelTextAnimationFinish(Sender: TObject);
begin
  FLabelTextAnimation.Enabled := False;
  UpdateNativeViewVisibility;
  Repaint;
end;

{*********************************************************}
function TALBaseEdit.HasOpacityLabelTextAnimation: Boolean;
begin
  result := (LabelTextSettings.Layout = TLabelTextLayout.Floating) and
            (LabelTextSettings.Animation = TLabelTextAnimation.Opacity) and
            (LabelText <> '') and
            ((prompttext = '') or (prompttext = labeltext));
end;

{*************************************************************}
function TALBaseEdit.HasTranslationLabelTextAnimation: Boolean;
begin
  result := ((LabelTextSettings.Layout = TLabelTextLayout.Inline) or
             ((LabelTextSettings.Layout = TLabelTextLayout.Floating) and
              (LabelTextSettings.Animation = TLabelTextAnimation.Translation))) and
            (LabelText <> '') and
            ((PromptText = '') or (PromptText = LabelText));
end;

{***************************************}
procedure TALBaseEdit.IsMouseOverChanged;
begin
  inherited;
  if not IsMouseOver then begin
    {$IF defined(MSWindows)}
    IF Form <> nil then begin
      var LMousePos := AbsoluteToLocal(Form.ScreenToClient(Screen.MousePos));
      if LocalRect.Contains(LMousePos) then
        FIsMouseOver := true;
    end;
    {$ENDIF}
  end;
  StateStyles.startTransition;
end;

{****************************************************************}
procedure TALBaseEdit.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{*********************************************************}
procedure TALBaseEdit.TextSettingsChanged(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin

    if (not (csLoading in ComponentState)) and
       (not AToStateStyle.TextSettings.inherit) then begin

      if APrevStateStyle.TextSettings.font.Family = AToStateStyle.TextSettings.font.Family then AToStateStyle.TextSettings.font.Family := TextSettings.font.Family;
      if SameValue(APrevStateStyle.TextSettings.font.Size, AToStateStyle.TextSettings.font.Size, TEpsilon.fontSize) then AToStateStyle.TextSettings.font.Size := TextSettings.font.Size;
      if APrevStateStyle.TextSettings.font.Weight = AToStateStyle.TextSettings.font.Weight then AToStateStyle.TextSettings.font.Weight := TextSettings.font.Weight;
      if APrevStateStyle.TextSettings.font.Slant = AToStateStyle.TextSettings.font.Slant then AToStateStyle.TextSettings.font.Slant := TextSettings.font.Slant;
      if APrevStateStyle.TextSettings.font.Stretch = AToStateStyle.TextSettings.font.Stretch then AToStateStyle.TextSettings.font.Stretch := TextSettings.font.Stretch;
      if APrevStateStyle.TextSettings.font.Color = AToStateStyle.TextSettings.font.Color then AToStateStyle.TextSettings.font.Color := TextSettings.font.Color;

    end;

    APrevStateStyle.TextSettings.font.Family := TextSettings.font.Family;
    APrevStateStyle.TextSettings.font.Size := TextSettings.font.Size;
    APrevStateStyle.TextSettings.font.Weight := TextSettings.font.Weight;
    APrevStateStyle.TextSettings.font.Slant := TextSettings.font.Slant;
    APrevStateStyle.TextSettings.font.Stretch := TextSettings.font.Stretch;
    APrevStateStyle.TextSettings.font.Color := TextSettings.font.Color;

  end;
  {$ENDIF}

begin
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
  if csLoading in componentState then exit;
  ClearBufPromptTextDrawable;
  UpdateEditControlStyle;
  AdjustSize;
end;

{****************************************************}
procedure TALBaseEdit.SetXRadius(const Value: Single);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    if (not (csLoading in ComponentState)) and
       (not AToStateStyle.StateLayer.HasFill) then begin
      if (SameValue(APrevStateStyle.StateLayer.XRadius, AToStateStyle.StateLayer.XRadius, TEpsilon.Vector)) then AToStateStyle.StateLayer.XRadius := XRadius;
    end;
    APrevStateStyle.StateLayer.XRadius := XRadius;
  end;
  {$ENDIF}

begin
  inherited;
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
end;

{****************************************************}
procedure TALBaseEdit.SetYRadius(const Value: Single);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    if (not (csLoading in ComponentState)) and
       (not AToStateStyle.StateLayer.HasFill) then begin
      if (SameValue(APrevStateStyle.StateLayer.YRadius, AToStateStyle.StateLayer.YRadius, TEpsilon.Vector)) then AToStateStyle.StateLayer.YRadius := YRadius;
    end;
    APrevStateStyle.StateLayer.YRadius := YRadius;
  end;
  {$ENDIF}

begin
  inherited;
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
end;

{**************************************************************************}
procedure TALBaseEdit.SetLabelTextSettings(const Value: TLabelTextSettings);
begin
  FLabelTextSettings.Assign(Value);
end;

{**************************************************************}
procedure TALBaseEdit.LabelTextSettingsChanged(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin

    if (not (csLoading in ComponentState)) and
       (not AToStateStyle.LabelTextSettings.inherit) then begin

      if APrevStateStyle.LabelTextSettings.font.Family = AToStateStyle.LabelTextSettings.font.Family then AToStateStyle.LabelTextSettings.font.Family := LabelTextSettings.font.Family;
      if SameValue(APrevStateStyle.LabelTextSettings.font.Size, AToStateStyle.LabelTextSettings.font.Size, TEpsilon.fontSize) then AToStateStyle.LabelTextSettings.font.Size := LabelTextSettings.font.Size;
      if APrevStateStyle.LabelTextSettings.font.Weight = AToStateStyle.LabelTextSettings.font.Weight then AToStateStyle.LabelTextSettings.font.Weight := LabelTextSettings.font.Weight;
      if APrevStateStyle.LabelTextSettings.font.Slant = AToStateStyle.LabelTextSettings.font.Slant then AToStateStyle.LabelTextSettings.font.Slant := LabelTextSettings.font.Slant;
      if APrevStateStyle.LabelTextSettings.font.Stretch = AToStateStyle.LabelTextSettings.font.Stretch then AToStateStyle.LabelTextSettings.font.Stretch := LabelTextSettings.font.Stretch;
      if APrevStateStyle.LabelTextSettings.font.Color = AToStateStyle.LabelTextSettings.font.Color then AToStateStyle.LabelTextSettings.font.Color := LabelTextSettings.font.Color;

    end;

    APrevStateStyle.LabelTextSettings.font.Family := LabelTextSettings.font.Family;
    APrevStateStyle.LabelTextSettings.font.Size := LabelTextSettings.font.Size;
    APrevStateStyle.LabelTextSettings.font.Weight := LabelTextSettings.font.Weight;
    APrevStateStyle.LabelTextSettings.font.Slant := LabelTextSettings.font.Slant;
    APrevStateStyle.LabelTextSettings.font.Stretch := LabelTextSettings.font.Stretch;
    APrevStateStyle.LabelTextSettings.font.Color := LabelTextSettings.font.Color;

  end;
  {$ENDIF}

begin
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
  if csLoading in componentState then exit;
  ClearBufLabelTextDrawable;
  UpdateEditControlPromptText;
  UpdateNativeViewVisibility;
  AdjustSize;
  repaint;
end;

{************************************************************************************}
procedure TALBaseEdit.SetSupportingTextSettings(const Value: TSupportingTextSettings);
begin
  FSupportingTextSettings.Assign(Value);
end;

{*******************************************************************}
procedure TALBaseEdit.SupportingTextSettingsChanged(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin

    if (not (csLoading in ComponentState)) and
       (not AToStateStyle.SupportingTextSettings.inherit) then begin

      if APrevStateStyle.SupportingTextSettings.font.Family = AToStateStyle.SupportingTextSettings.font.Family then AToStateStyle.SupportingTextSettings.font.Family := SupportingTextSettings.font.Family;
      if SameValue(APrevStateStyle.SupportingTextSettings.font.Size, AToStateStyle.SupportingTextSettings.font.Size, TEpsilon.fontSize) then AToStateStyle.SupportingTextSettings.font.Size := SupportingTextSettings.font.Size;
      if APrevStateStyle.SupportingTextSettings.font.Weight = AToStateStyle.SupportingTextSettings.font.Weight then AToStateStyle.SupportingTextSettings.font.Weight := SupportingTextSettings.font.Weight;
      if APrevStateStyle.SupportingTextSettings.font.Slant = AToStateStyle.SupportingTextSettings.font.Slant then AToStateStyle.SupportingTextSettings.font.Slant := SupportingTextSettings.font.Slant;
      if APrevStateStyle.SupportingTextSettings.font.Stretch = AToStateStyle.SupportingTextSettings.font.Stretch then AToStateStyle.SupportingTextSettings.font.Stretch := SupportingTextSettings.font.Stretch;
      if APrevStateStyle.SupportingTextSettings.font.Color = AToStateStyle.SupportingTextSettings.font.Color then AToStateStyle.SupportingTextSettings.font.Color := SupportingTextSettings.font.Color;

    end;

    APrevStateStyle.SupportingTextSettings.font.Family := SupportingTextSettings.font.Family;
    APrevStateStyle.SupportingTextSettings.font.Size := SupportingTextSettings.font.Size;
    APrevStateStyle.SupportingTextSettings.font.Weight := SupportingTextSettings.font.Weight;
    APrevStateStyle.SupportingTextSettings.font.Slant := SupportingTextSettings.font.Slant;
    APrevStateStyle.SupportingTextSettings.font.Stretch := SupportingTextSettings.font.Stretch;
    APrevStateStyle.SupportingTextSettings.font.Color := SupportingTextSettings.font.Color;

  end;
  {$ENDIF}

begin
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
  if csLoading in componentState then exit;
  ClearBufSupportingTextDrawable;
  repaint;
end;

{***************************************************************}
procedure TALBaseEdit.SetStateStyles(const AValue: TStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{********************************************************}
procedure TALBaseEdit.StateStylesChanged(Sender: TObject);
begin
  clearBufDrawable;
  DisabledOpacity := StateStyles.Disabled.opacity;
  UpdateEditControlStyle;
  Repaint;
end;

{*************************************************}
procedure TALBaseEdit.SetText(const Value: String);
begin
  EditControl.Text := Value;
  {$IF defined(ALDPK)}
  ClearBufPromptTextDrawable;
  ClearBufLabelTextDrawable;
  repaint;
  {$ENDIF}
end;

{***********************************}
function TALBaseEdit.getText: String;
begin
  result := EditControl.Text;
end;

{***********************************************}
procedure TALBaseEdit.UpdateNativeViewVisibility;
begin
  if {$IF defined(ALDPK)}true or{$ENDIF}
     (not Enabled) or
     ((GetIsTextEmpty) and (HasTranslationLabelTextAnimation) and (not IsFocused)) then
    EditControl.RemoveNativeView
  else if not FNativeViewRemoved then
    EditControl.AddNativeView;
end;

{************************************************}
procedure TALBaseEdit.UpdateEditControlPromptText;
begin
  if HasTranslationLabelTextAnimation then EditControl.PromptText := ''
  else if FPromptText<>'' then EditControl.PromptText := FPromptText
  else EditControl.PromptText := FLabelText;
end;

{*****************************************}
function TALBaseEdit.GetPromptText: String;
begin
  result := FPromptText;
end;

{*******************************************************}
procedure TALBaseEdit.setPromptText(const Value: String);
begin
  if FPromptText <> Value then begin
    ClearBufPromptTextDrawable;
    FPromptText := Value;
    UpdateEditControlPromptText;
    UpdateNativeViewVisibility;
    repaint;
  end;
end;

{***************************************************}
function TALBaseEdit.GetPromptTextColor: TAlphaColor;
begin
  result := FPromptTextColor;
end;

{*****************************************************************}
procedure TALBaseEdit.setPromptTextColor(const Value: TAlphaColor);
begin
  if FPromptTextColor <> Value then begin
    ClearBufPromptTextDrawable;
    FPromptTextColor := Value;
    UpdateEditControlStyle;
  end;
end;

{******************************************************}
procedure TALBaseEdit.setLabelText(const Value: String);
begin
  if FLabelText <> Value then begin
    ClearBufPromptTextDrawable;
    ClearBufLabelTextDrawable;
    FLabelText := Value;
    UpdateEditControlPromptText;
    UpdateNativeViewVisibility;
    if FLabelTextSettings.Layout = TLabelTextLayout.Inline then
      AdjustSize;
    Repaint;
  end;
end;

{***********************************************************}
procedure TALBaseEdit.setSupportingText(const Value: String);
begin
  if FSupportingText <> Value then begin
    ClearBufSupportingTextDrawable;
    FSupportingText := Value;
    Repaint;
  end;
end;

{*********************************************}
function TALBaseEdit.GetTintColor: TAlphaColor;
begin
  result := FTintColor;
end;

{***********************************************************}
procedure TALBaseEdit.setTintColor(const Value: TAlphaColor);
begin
  if FTintColor <> Value then begin
    FTintColor := Value;
    UpdateEditControlStyle;
  end;
end;

{*****************************************************************}
procedure TALBaseEdit.SetKeyboardType(Value: TVirtualKeyboardType);
begin
  EditControl.KeyboardType := Value;
end;

{*********************************************************}
function TALBaseEdit.GetKeyboardType: TVirtualKeyboardType;
begin
  result := EditControl.KeyboardType;
end;

{************************************************************************}
function TALBaseEdit.GetAutoCapitalizationType: TALAutoCapitalizationType;
begin
  result := EditControl.AutoCapitalizationType;
end;

{**************************************************************************************}
procedure TALBaseEdit.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  EditControl.AutoCapitalizationType := Value;
end;

{******************************************************}
procedure TALBaseEdit.SetPassword(const Value: Boolean);
begin
  EditControl.Password := Value;
  clearBufPromptTextDrawable;
end;

{****************************************}
function TALBaseEdit.GetPassword: Boolean;
begin
  result := EditControl.Password;
end;

{***********************************************************}
procedure TALBaseEdit.SetCheckSpelling(const Value: Boolean);
begin
  EditControl.CheckSpelling := Value;
end;

{*********************************************}
function TALBaseEdit.GetCheckSpelling: Boolean;
begin
  result := EditControl.CheckSpelling;
end;

{************************************************************}
procedure TALBaseEdit.SetReturnKeyType(Value: TReturnKeyType);
begin
  EditControl.ReturnKeyType := Value;
end;

{****************************************************}
function TALBaseEdit.GetReturnKeyType: TReturnKeyType;
begin
  result := EditControl.ReturnKeyType;
end;

{*******************************************************}
procedure TALBaseEdit.SetMaxLength(const Value: integer);
begin
  EditControl.MaxLength := Value;
end;

{*****************************************}
function TALBaseEdit.GetMaxLength: integer;
begin
  result := EditControl.MaxLength;
end;

{*******************************************}
function TALBaseEdit.GetIsTextEmpty: Boolean;
begin
  // FIsTextEmpty is used here because I believe that
  // GetText could be a little slow, and it's called
  // in every paint cycle.
  {$IF defined(ALDPK)}
  Result := GetText = '';
  {$ELSE}
  Result := FIsTextEmpty;
  {$ENDIF}
end;

{**************************************************}
procedure TALBaseEdit.OnChangeImpl(Sender: TObject);
begin
  if (csLoading in componentState) then exit;
  var LIsTextEmpty := GetText = '';
  if (LIsTextEmpty <> FIsTextEmpty) and
     (HasOpacityLabelTextAnimation) then begin
    FIsTextEmpty := LIsTextEmpty;
    FlabelTextAnimation.StopAtCurrent;
    FlabelTextAnimation.Inverse := FIsTextEmpty;
    if FIsTextEmpty then FlabelTextAnimation.Duration := 0.1
    else FlabelTextAnimation.Duration := 0.2;
    FLabelTextAnimation.Start;
    repaint;
  end
  else
    FIsTextEmpty := LIsTextEmpty;
  if assigned(fOnChange) then
    fOnChange(self);
end;

{*****************************************************}
procedure TALBaseEdit.OnReturnKeyImpl(Sender: TObject);
begin
  {$IF defined(DEBUG)}
  if (csLoading in componentState) then raise Exception.Create('Error B1A3CE09-A6C4-44F5-92D1-E32112A7AAE2');
  {$ENDIF}
  if assigned(fOnReturnKey) then
    fOnReturnKey(self);
end;

{**************************************************************}
procedure TALBaseEdit.SetOnReturnKey(const Value: TNotifyEvent);
begin
  fOnReturnKey := Value;
  if assigned(fOnReturnKey) then EditControl.onReturnKey := OnReturnKeyImpl
  else EditControl.onReturnKey := nil;
end;

{***********************************}
procedure TALBaseEdit.EnabledChanged;
begin
  Inherited;
  if (csLoading in componentState) then exit;
  UpdateEditControlStyle;
  UpdateNativeViewVisibility;
end;

{***********************************}
procedure TALBaseEdit.PaddingChanged;
begin
  clearBufDrawable;
  Inherited;
end;

{***************************************************}
procedure TALBaseEdit.StrokeChanged(Sender: TObject);
begin
  inherited;
  AdjustSize;
end;

{**************************************************}
procedure TALBaseEdit.SetSides(const Value: TSides);
begin
  inherited;
  AdjustSize;
end;

{*************************************************}
procedure TALBaseEdit.FillChanged(Sender: TObject);
begin
  inherited;
  UpdateEditControlStyle;
end;

{************************************************}
Procedure TALBaseEdit.CreateBufPromptTextDrawable(
            var ABufDrawable: TALDrawable;
            var ABufDrawableRect: TRectF;
            const AText: String;
            const AFont: TALFont);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  var LMaxSize := TSizeF.Create(Width-padding.Left-padding.Right, 65535);

  //init ABufDrawableRect
  ABufDrawableRect := TRectF.Create(0, 0, LMaxSize.cX, LMaxSize.cY);

  //create LOptions
  var LOptions := TALMultiLineTextOptions.Create;
  Try

    LOptions.Scale := ALGetScreenScale;
    LOptions.AlignToPixel := IsPixelAlignmentEnabled;
    //LOptions.Opacity: Single; // Default = 1
    //--
    LOptions.FontFamily := Afont.Family;
    LOptions.FontSize := Afont.Size;
    LOptions.FontWeight := Afont.Weight;
    LOptions.FontSlant := Afont.Slant;
    LOptions.FontStretch := Afont.Stretch;
    LOptions.FontColor := AFont.Color;
    //--
    //LOptions.DecorationKinds := ADecoration.Kinds;
    //LOptions.DecorationStyle := ADecoration.Style;
    //LOptions.DecorationThicknessMultiplier := ADecoration.ThicknessMultiplier;
    //LOptions.DecorationColor := ADecoration.Color;
    //--
    //LOptions.EllipsisText := TextSettings.Ellipsis;
    //LOptions.EllipsisInheritSettings := TextSettings.EllipsisSettings.inherit;
    //--
    //LOptions.EllipsisFontFamily := AEllipsisfont.Family;
    //LOptions.EllipsisFontSize := AEllipsisfont.Size;
    //LOptions.EllipsisFontWeight := AEllipsisfont.Weight;
    //LOptions.EllipsisFontSlant := AEllipsisfont.Slant;
    //LOptions.EllipsisFontStretch := AEllipsisfont.Stretch;
    //LOptions.EllipsisFontColor := AEllipsisFont.Color;
    //--
    //LOptions.EllipsisDecorationKinds := AEllipsisDecoration.Kinds;
    //LOptions.EllipsisDecorationStyle := AEllipsisDecoration.Style;
    //LOptions.EllipsisDecorationThicknessMultiplier := AEllipsisDecoration.ThicknessMultiplier;
    //LOptions.EllipsisDecorationColor := AEllipsisDecoration.Color;
    //--
    LOptions.AutoSize := True;
    //--
    LOptions.MaxLines := 1;
    //LOptions.LineHeightMultiplier := TextSettings.LineHeightMultiplier;
    //LOptions.LetterSpacing := TextSettings.LetterSpacing;
    //LOptions.Trimming := TextSettings.Trimming;
    //LOptions.FailIfTextBroken: boolean; // default = false
    //--
    if TFillTextFlag.RightToLeft in FillTextFlags then LOptions.Direction := TALTextDirection.RightToLeft
    else LOptions.Direction := TALTextDirection.LeftToRight;
    //LOptions.HTextAlign := TextSettings.HorzAlign;
    //LOptions.VTextAlign := TextSettings.VertAlign;
    //--
    //LOptions.FillColor: TAlphaColor; // default = TAlphaColors.null
    //LOptions.FillGradientStyle: TGradientStyle; // Default = TGradientStyle.Linear;
    //LOptions.FillGradientAngle: Single; // Default = 180;
    //LOptions.FillGradientColors: TArray<TAlphaColor>; // Default = [];
    //LOptions.FillGradientOffsets: TArray<Single>; // Default = [];
    //LOptions.FillResourceName: String; // default = ''
    //LOptions.FillMaskResourceName: String; // default = ''
    //LOptions.FillMaskBitmap: TALBitmap; // default = ALNullBitmap
    //LOptions.FillBackgroundMargins: TRectF; // default = TRectF.Empty
    //LOptions.FillImageMargins: TRectF; // default = TRectF.Empty
    //LOptions.FillImageNoRadius: Boolean; // default = False
    //LOptions.FillWrapMode: TALImageWrapMode; // default = TALImageWrapMode.Fit
    //LOptions.FillCropCenter: TpointF; // default = TPointF.create(-50,-50)
    //LOptions.FillBlurRadius: single; // default = 0
    //--
    //LOptions.StateLayerOpacity: Single; // Default = 0
    //LOptions.StateLayerColor: TAlphaColor; // Default = TAlphaColors.null
    //LOptions.StateLayerMargins: TRectF; // default = TRectF.Empty
    //LOptions.StateLayerXRadius: Single; // default = 0
    //LOptions.StateLayerYRadius: Single; // default = 0
    //--
    //LOptions.StrokeColor: TalphaColor; // default = TAlphaColors.null
    //LOptions.StrokeThickness: Single; // default = 1
    //--
    //LOptions.ShadowColor: TAlphaColor; // default = TAlphaColors.null
    //LOptions.ShadowBlur: Single; // default = 12
    //LOptions.ShadowOffsetX: Single; // default = 0
    //LOptions.ShadowOffsetY: Single; // default = 0
    //--
    //LOptions.Sides := Sides;
    //LOptions.XRadius := XRadius;
    //LOptions.YRadius := YRadius;
    //LOptions.Corners := Corners;
    //LOptions.Padding := padding.Rect;
    //--
    //LOptions.TextIsHtml := TextSettings.IsHtml;
    //--
    //LOptions.OnAdjustRect: TALMultiLineTextAdjustRectProc; // default = nil
    //LOptions.OnBeforeDrawBackground: TALMultiLineTextBeforeDrawBackgroundProc; // default = nil
    //LOptions.OnBeforeDrawParagraph: TALMultiLineTextBeforeDrawParagraphProc; // default = nil

    //build ABufDrawable
    var LTextBroken: Boolean;
    var LAllTextDrawn: Boolean;
    var LElements: TALTextElements;
    ABufDrawable := ALCreateMultiLineTextDrawable(
                      AText,
                      ABufDrawableRect,
                      LTextBroken,
                      LAllTextDrawn,
                      LElements,
                      LOptions);

    //Special case where it's impossible to draw at least one char.
    //To avoid to call again ALDrawMultiLineText on each paint
    //return a "blank" drawable.
    if (ALIsDrawableNull(ABufDrawable)) then begin
      ABufDrawable := ALCreateEmptyDrawable1x1;
      ABufDrawableRect := TRectF.Create(0,0,1/ALGetScreenScale,1/ALGetScreenScale);
    end;

  finally
    ALFreeAndNil(LOptions);
  end;

end;

{***********************************************}
Procedure TALBaseEdit.CreateBufLabelTextDrawable(
            var ABufDrawable: TALDrawable;
            var ABufDrawableRect: TRectF;
            const AText: String;
            const AFont: TALFont);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  var LMaxSize := TSizeF.Create(Width-padding.Left-padding.right-LabelTextSettings.Margins.Left-LabelTextSettings.Margins.right, 65535);

  //init ABufDrawableRect
  ABufDrawableRect := TRectF.Create(0, 0, LMaxSize.cX, LMaxSize.cY);

  //create LOptions
  var LOptions := TALMultiLineTextOptions.Create;
  Try

    LOptions.Scale := ALGetScreenScale;
    LOptions.AlignToPixel := IsPixelAlignmentEnabled;
    //LOptions.Opacity: Single; // Default = 1
    //--
    LOptions.FontFamily := Afont.Family;
    LOptions.FontSize := Afont.Size;
    LOptions.FontWeight := Afont.Weight;
    LOptions.FontSlant := Afont.Slant;
    LOptions.FontStretch := Afont.Stretch;
    LOptions.FontColor := AFont.Color;
    //--
    //LOptions.DecorationKinds := ADecoration.Kinds;
    //LOptions.DecorationStyle := ADecoration.Style;
    //LOptions.DecorationThicknessMultiplier := ADecoration.ThicknessMultiplier;
    //LOptions.DecorationColor := ADecoration.Color;
    //--
    LOptions.EllipsisText := LabelTextSettings.Ellipsis;
    //LOptions.EllipsisInheritSettings := LabelTextSettings.EllipsisSettings.inherit;
    //--
    //LOptions.EllipsisFontFamily := AEllipsisfont.Family;
    //LOptions.EllipsisFontSize := AEllipsisfont.Size;
    //LOptions.EllipsisFontWeight := AEllipsisfont.Weight;
    //LOptions.EllipsisFontSlant := AEllipsisfont.Slant;
    //LOptions.EllipsisFontStretch := AEllipsisfont.Stretch;
    //LOptions.EllipsisFontColor := AEllipsisFont.Color;
    //--
    //LOptions.EllipsisDecorationKinds := AEllipsisDecoration.Kinds;
    //LOptions.EllipsisDecorationStyle := AEllipsisDecoration.Style;
    //LOptions.EllipsisDecorationThicknessMultiplier := AEllipsisDecoration.ThicknessMultiplier;
    //LOptions.EllipsisDecorationColor := AEllipsisDecoration.Color;
    //--
    LOptions.AutoSize := True;
    //--
    LOptions.MaxLines := LabelTextSettings.MaxLines;
    LOptions.LineHeightMultiplier := LabelTextSettings.LineHeightMultiplier;
    LOptions.LetterSpacing := LabelTextSettings.LetterSpacing;
    LOptions.Trimming := LabelTextSettings.Trimming;
    //LOptions.FailIfTextBroken: boolean; // default = false
    //--
    if TFillTextFlag.RightToLeft in FillTextFlags then LOptions.Direction := TALTextDirection.RightToLeft
    else LOptions.Direction := TALTextDirection.LeftToRight;
    //LOptions.HTextAlign := LabelTextSettings.HorzAlign;
    //LOptions.VTextAlign := LabelTextSettings.VertAlign;
    //--
    //LOptions.FillColor: TAlphaColor; // default = TAlphaColors.null
    //LOptions.FillGradientStyle: TGradientStyle; // Default = TGradientStyle.Linear;
    //LOptions.FillGradientAngle: Single; // Default = 180;
    //LOptions.FillGradientColors: TArray<TAlphaColor>; // Default = [];
    //LOptions.FillGradientOffsets: TArray<Single>; // Default = [];
    //LOptions.FillResourceName: String; // default = ''
    //LOptions.FillMaskResourceName: String; // default = ''
    //LOptions.FillMaskBitmap: TALBitmap; // default = ALNullBitmap
    //LOptions.FillBackgroundMargins: TRectF; // default = TRectF.Empty
    //LOptions.FillImageMargins: TRectF; // default = TRectF.Empty
    //LOptions.FillImageNoRadius: Boolean; // default = False
    //LOptions.FillWrapMode: TALImageWrapMode; // default = TALImageWrapMode.Fit
    //LOptions.FillCropCenter: TpointF; // default = TPointF.create(-50,-50)
    //LOptions.FillBlurRadius: single; // default = 0
    //--
    //LOptions.StateLayerOpacity: Single; // Default = 0
    //LOptions.StateLayerColor: TAlphaColor; // Default = TAlphaColors.null
    //LOptions.StateLayerMargins: TRectF; // default = TRectF.Empty
    //LOptions.StateLayerXRadius: Single; // default = 0
    //LOptions.StateLayerYRadius: Single; // default = 0
    //--
    //LOptions.StrokeColor: TalphaColor; // default = TAlphaColors.null
    //LOptions.StrokeThickness: Single; // default = 1
    //--
    //LOptions.ShadowColor: TAlphaColor; // default = TAlphaColors.null
    //LOptions.ShadowBlur: Single; // default = 12
    //LOptions.ShadowOffsetX: Single; // default = 0
    //LOptions.ShadowOffsetY: Single; // default = 0
    //--
    //LOptions.Sides := Sides;
    //LOptions.XRadius := XRadius;
    //LOptions.YRadius := YRadius;
    //LOptions.Corners := Corners;
    //LOptions.Padding := padding.Rect;
    //--
    LOptions.TextIsHtml := LabelTextSettings.IsHtml;
    //--
    //LOptions.OnAdjustRect: TALMultiLineTextAdjustRectProc; // default = nil
    //LOptions.OnBeforeDrawBackground: TALMultiLineTextBeforeDrawBackgroundProc; // default = nil
    //LOptions.OnBeforeDrawParagraph: TALMultiLineTextBeforeDrawParagraphProc; // default = nil

    //build ABufDrawable
    var LTextBroken: Boolean;
    var LAllTextDrawn: Boolean;
    var LElements: TALTextElements;
    ABufDrawable := ALCreateMultiLineTextDrawable(
                      AText,
                      ABufDrawableRect,
                      LTextBroken,
                      LAllTextDrawn,
                      LElements,
                      LOptions);

    //Special case where it's impossible to draw at least one char.
    //To avoid to call again ALDrawMultiLineText on each paint
    //return a "blank" drawable.
    if (ALIsDrawableNull(ABufDrawable)) then begin
      ABufDrawable := ALCreateEmptyDrawable1x1;
      ABufDrawableRect := TRectF.Create(0,0,1/ALGetScreenScale,1/ALGetScreenScale);
    end;

  finally
    ALFreeAndNil(LOptions);
  end;

end;

{****************************************************}
Procedure TALBaseEdit.CreateBufSupportingTextDrawable(
            var ABufDrawable: TALDrawable;
            var ABufDrawableRect: TRectF;
            const AText: String;
            const AFont: TALFont);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  var LMaxSize := TSizeF.Create(Width-padding.Left-padding.right-SupportingTextSettings.Margins.Left-SupportingTextSettings.Margins.right, 65535);

  //init ABufDrawableRect
  ABufDrawableRect := TRectF.Create(0, 0, LMaxSize.cX, LMaxSize.cY);

  //create LOptions
  var LOptions := TALMultiLineTextOptions.Create;
  Try

    LOptions.Scale := ALGetScreenScale;
    LOptions.AlignToPixel := IsPixelAlignmentEnabled;
    //LOptions.Opacity: Single; // Default = 1
    //--
    LOptions.FontFamily := Afont.Family;
    LOptions.FontSize := Afont.Size;
    LOptions.FontWeight := Afont.Weight;
    LOptions.FontSlant := Afont.Slant;
    LOptions.FontStretch := Afont.Stretch;
    LOptions.FontColor := AFont.Color;
    //--
    //LOptions.DecorationKinds := ADecoration.Kinds;
    //LOptions.DecorationStyle := ADecoration.Style;
    //LOptions.DecorationThicknessMultiplier := ADecoration.ThicknessMultiplier;
    //LOptions.DecorationColor := ADecoration.Color;
    //--
    LOptions.EllipsisText := SupportingTextSettings.Ellipsis;
    //LOptions.EllipsisInheritSettings := SupportingTextSettings.EllipsisSettings.inherit;
    //--
    //LOptions.EllipsisFontFamily := AEllipsisfont.Family;
    //LOptions.EllipsisFontSize := AEllipsisfont.Size;
    //LOptions.EllipsisFontWeight := AEllipsisfont.Weight;
    //LOptions.EllipsisFontSlant := AEllipsisfont.Slant;
    //LOptions.EllipsisFontStretch := AEllipsisfont.Stretch;
    //LOptions.EllipsisFontColor := AEllipsisFont.Color;
    //--
    //LOptions.EllipsisDecorationKinds := AEllipsisDecoration.Kinds;
    //LOptions.EllipsisDecorationStyle := AEllipsisDecoration.Style;
    //LOptions.EllipsisDecorationThicknessMultiplier := AEllipsisDecoration.ThicknessMultiplier;
    //LOptions.EllipsisDecorationColor := AEllipsisDecoration.Color;
    //--
    LOptions.AutoSize := True;
    //--
    LOptions.MaxLines := SupportingTextSettings.MaxLines;
    LOptions.LineHeightMultiplier := SupportingTextSettings.LineHeightMultiplier;
    LOptions.LetterSpacing := SupportingTextSettings.LetterSpacing;
    LOptions.Trimming := SupportingTextSettings.Trimming;
    //LOptions.FailIfTextBroken: boolean; // default = false
    //--
    if TFillTextFlag.RightToLeft in FillTextFlags then LOptions.Direction := TALTextDirection.RightToLeft
    else LOptions.Direction := TALTextDirection.LeftToRight;
    //LOptions.HTextAlign := SupportingTextSettings.HorzAlign;
    //LOptions.VTextAlign := SupportingTextSettings.VertAlign;
    //--
    //LOptions.FillColor: TAlphaColor; // default = TAlphaColors.null
    //LOptions.FillGradientStyle: TGradientStyle; // Default = TGradientStyle.Linear;
    //LOptions.FillGradientAngle: Single; // Default = 180;
    //LOptions.FillGradientColors: TArray<TAlphaColor>; // Default = [];
    //LOptions.FillGradientOffsets: TArray<Single>; // Default = [];
    //LOptions.FillResourceName: String; // default = ''
    //LOptions.FillMaskResourceName: String; // default = ''
    //LOptions.FillMaskBitmap: TALBitmap; // default = ALNullBitmap
    //LOptions.FillBackgroundMargins: TRectF; // default = TRectF.Empty
    //LOptions.FillImageMargins: TRectF; // default = TRectF.Empty
    //LOptions.FillImageNoRadius: Boolean; // default = False
    //LOptions.FillWrapMode: TALImageWrapMode; // default = TALImageWrapMode.Fit
    //LOptions.FillCropCenter: TpointF; // default = TPointF.create(-50,-50)
    //LOptions.FillBlurRadius: single; // default = 0
    //--
    //LOptions.StateLayerOpacity: Single; // Default = 0
    //LOptions.StateLayerColor: TAlphaColor; // Default = TAlphaColors.null
    //LOptions.StateLayerMargins: TRectF; // default = TRectF.Empty
    //LOptions.StateLayerXRadius: Single; // default = 0
    //LOptions.StateLayerYRadius: Single; // default = 0
    //--
    //LOptions.StrokeColor: TalphaColor; // default = TAlphaColors.null
    //LOptions.StrokeThickness: Single; // default = 1
    //--
    //LOptions.ShadowColor: TAlphaColor; // default = TAlphaColors.null
    //LOptions.ShadowBlur: Single; // default = 12
    //LOptions.ShadowOffsetX: Single; // default = 0
    //LOptions.ShadowOffsetY: Single; // default = 0
    //--
    //LOptions.Sides := Sides;
    //LOptions.XRadius := XRadius;
    //LOptions.YRadius := YRadius;
    //LOptions.Corners := Corners;
    //LOptions.Padding := padding.Rect;
    //--
    LOptions.TextIsHtml := SupportingTextSettings.IsHtml;
    //--
    //LOptions.OnAdjustRect: TALMultiLineTextAdjustRectProc; // default = nil
    //LOptions.OnBeforeDrawBackground: TALMultiLineTextBeforeDrawBackgroundProc; // default = nil
    //LOptions.OnBeforeDrawParagraph: TALMultiLineTextBeforeDrawParagraphProc; // default = nil

    //build ABufDrawable
    var LTextBroken: Boolean;
    var LAllTextDrawn: Boolean;
    var LElements: TALTextElements;
    ABufDrawable := ALCreateMultiLineTextDrawable(
                      AText,
                      ABufDrawableRect,
                      LTextBroken,
                      LAllTextDrawn,
                      LElements,
                      LOptions);

    //Special case where it's impossible to draw at least one char.
    //To avoid to call again ALDrawMultiLineText on each paint
    //return a "blank" drawable.
    if (ALIsDrawableNull(ABufDrawable)) then begin
      ABufDrawable := ALCreateEmptyDrawable1x1;
      ABufDrawableRect := TRectF.Create(0,0,1/ALGetScreenScale,1/ALGetScreenScale);
    end;

  finally
    ALFreeAndNil(LOptions);
  end;

end;

{************************************}
procedure TALBaseEdit.MakeBufDrawable;
begin
  //--- Do not create BufDrawable if the size is 0
  if (Size.Size.IsZero) then begin
    clearBufDrawable;
    exit;
  end;
  //--
  inherited MakeBufDrawable;
  MakeBufPromptTextDrawable;
  MakeBufLabelTextDrawable;
  MakeBufSupportingTextDrawable;
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Fill.Inherit and
     (not LStateStyle.StateLayer.HasFill) and
     LStateStyle.Stroke.Inherit and
     LStateStyle.Shadow.Inherit then exit;
  if (not ALIsDrawableNull(LStateStyle.FBufDrawable)) then exit;
  LStateStyle.SupersedeNoChanges(true{ASaveState});
  try
    CreateBufDrawable(
      LStateStyle.FBufDrawable, // var ABufDrawable: TALDrawable;
      LStateStyle.FBufDrawableRect, // var ABufDrawableRect: TRectF;
      ALGetScreenScale, // const AScale: Single;
      LStateStyle.Fill, // const AFill: TALBrush;
      LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
      LStateStyle.TextSettings.Font.Color, // const AStateLayerContentColor: TAlphaColor;
      True, // const ADrawStateLayerOnTop: Boolean;
      LStateStyle.Stroke, // const AStroke: TALStrokeBrush;
      LStateStyle.Shadow); // const AShadow: TALShadow);
  finally
    LStateStyle.RestorestateNoChanges;
  end;
end;

{**********************************************}
procedure TALBaseEdit.MakeBufPromptTextDrawable;
begin
  Var LPromptText: String;
  var LUsePromptTextColor: Boolean;
  if (not Enabled) or
     (GetIsTextEmpty and HasTranslationLabelTextAnimation)
     {$IF defined(ALDPK)}or true{$ENDIF} then begin
    LPromptText := GetText;
    LUsePromptTextColor := False;
    if LPromptText = '' then begin
      LUsePromptTextColor := True;
      LPromptText := PromptText;
      if LPromptText = '' then LPromptText := LabelText;
    end
    else if Password then
      LPromptText := StringOfChar('*', Length(LPromptText));
  end
  else begin
    LPromptText := '';
    LUsePromptTextColor := True;
  end;
  //--
  if //--- Do not create BufPromptTextDrawable if LPromptText is empty
     (LPromptText = '') or
     //--- Do not create BufPromptTextDrawable if the size is 0
     (Size.Size.IsZero) then begin
    clearBufPromptTextDrawable;
    exit;
  end;
  //--
  if ALIsDrawableNull(FBufPromptTextDrawable) then begin
    var LFont := TextSettings.Font;
    var LPrevFontColor := LFont.Color;
    var LPrevFontOnchanged: TNotifyEvent := LFont.OnChanged;
    LFont.OnChanged := nil;
    if LUsePromptTextColor then begin
      if PromptTextColor <> TalphaColors.Null then LFont.Color := PromptTextColor
      else LFont.Color := TAlphaColorF.Create(
                            ALSetColorAlpha(TextSettings.Font.Color, 0.5)).
                              PremultipliedAlpha.
                              ToAlphaColor;
    end;
    try
      CreateBufPromptTextDrawable(
        FBufPromptTextDrawable, // var ABufPromptTextDrawable: TALDrawable;
        FBufPromptTextDrawableRect, // var ABufPromptTextDrawableRect: TRectF;
        LPromptText, // const AText: String;
        Lfont); // const AFont: TALFont;
    finally
      Lfont.Color := LPrevFontColor;
      LFont.OnChanged := LPrevFontOnchanged;
    end;
  end;
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if (LStateStyle.TextSettings.Inherit) and
     ((not LUsePromptTextColor) or (LStateStyle.PromptTextColor = TalphaColors.Null)) then exit;
  if (not ALIsDrawableNull(LStateStyle.BufPromptTextDrawable)) then exit;
  LStateStyle.SupersedeNoChanges(true{ASaveState});
  try
    var LPrevFontColor := LStateStyle.TextSettings.font.Color;
    var LPrevFontOnchanged: TNotifyEvent;
    LPrevFontOnchanged := LStateStyle.TextSettings.OnChanged;
    LStateStyle.TextSettings.OnChanged := nil;
    try
      if LUsePromptTextColor then begin
        if LStateStyle.PromptTextColor <> TAlphaColors.Null then LStateStyle.TextSettings.Font.Color := LStateStyle.PromptTextColor
        else if PromptTextColor <> TalphaColors.Null then LStateStyle.TextSettings.Font.Color := PromptTextColor
        else LStateStyle.TextSettings.Font.Color := TAlphaColorF.Create(
                                                      ALSetColorAlpha(TextSettings.Font.Color, 0.5)).
                                                        PremultipliedAlpha.
                                                        ToAlphaColor;
      end;
      CreateBufPromptTextDrawable(
        LStateStyle.BufPromptTextDrawable, // var ABufPromptTextDrawable: TALDrawable;
        LStateStyle.BufPromptTextDrawableRect, // var ABufPromptTextDrawableRect: TRectF;
        LPromptText, // const AText: String;
        LStateStyle.TextSettings.font); // const AFont: TALFont;
    finally
      LStateStyle.TextSettings.font.Color := LPrevFontColor;
      LStateStyle.TextSettings.OnChanged := LPrevFontOnchanged;
    end;
  finally
    LStateStyle.RestorestateNoChanges;
  end;
end;

{*********************************************}
procedure TALBaseEdit.MakeBufLabelTextDrawable;
begin
  if //--- Do not create BufLabelTextDrawable if LPromptText is empty
     (FLabelText = '') or
     //--- Do not create BufLabelTextDrawable if the size is 0
     (Size.Size.IsZero) then begin
    clearBufLabelTextDrawable;
    exit;
  end;
  //--
  if ALIsDrawableNull(FBufLabelTextDrawable) then begin
    CreateBufLabelTextDrawable(
      FBufLabelTextDrawable, // var ABufLabelTextDrawable: TALDrawable;
      FBufLabelTextDrawableRect, // var ABufLabelTextDrawableRect: TRectF;
      FLabelText, // const AText: String;
      LabelTextSettings.font); // const AFont: TALFont;
  end;
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.LabelTextSettings.Inherit then exit;
  if (not ALIsDrawableNull(LStateStyle.BufLabelTextDrawable)) then exit;
  LStateStyle.LabelTextSettings.SupersedeNoChanges(true{ASaveState});
  try
    CreateBufLabelTextDrawable(
      LStateStyle.BufLabelTextDrawable, // var ABufLabelTextDrawable: TALDrawable;
      LStateStyle.BufLabelTextDrawableRect, // var ABufLabelTextDrawableRect: TRectF;
      FLabelText, // const AText: String;
      LStateStyle.LabelTextSettings.font); // const AFont: TALFont;
  finally
    LStateStyle.LabelTextSettings.RestorestateNoChanges;
  end;
end;

{**************************************************}
procedure TALBaseEdit.MakeBufSupportingTextDrawable;
begin
  if //--- Do not create BufSupportingTextDrawable if LPromptText is empty
     (FSupportingText = '') or
     //--- Do not create BufSupportingTextDrawable if the size is 0
     (Size.Size.IsZero) then begin
    clearBufSupportingTextDrawable;
    exit;
  end;
  //--
  if ALIsDrawableNull(FBufSupportingTextDrawable) then begin
    CreateBufSupportingTextDrawable(
      FBufSupportingTextDrawable, // var ABufSupportingTextDrawable: TALDrawable;
      FBufSupportingTextDrawableRect, // var ABufSupportingTextDrawableRect: TRectF;
      FSupportingText, // const AText: String;
      SupportingTextSettings.font); // const AFont: TALFont;
    FBufSupportingTextDrawableRect.SetLocation(
      padding.Left + SupportingTextSettings.Margins.Left,
      Height+SupportingTextSettings.Margins.Top);
    if (SupportingTextSettings.Layout = TSupportingTextLayout.Inline) then begin
      {$IF defined(debug)}
      if FSupportingTextMarginBottomUpdated then
        Raise exception.Create('Error #7F39617E-617D-4A1E-A383-22E34B42AE1E');
      {$ENDIF}
      FSupportingTextMarginBottomUpdated := True;
      Margins.Bottom := FBufSupportingTextDrawableRect.Height + SupportingTextSettings.Margins.Top + SupportingTextSettings.Margins.bottom;
    end;
  end;
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.SupportingTextSettings.Inherit then exit;
  if (not ALIsDrawableNull(LStateStyle.BufSupportingTextDrawable)) then exit;
  LStateStyle.SupportingTextSettings.SupersedeNoChanges(true{ASaveState});
  try
    CreateBufSupportingTextDrawable(
      LStateStyle.BufSupportingTextDrawable, // var ABufSupportingTextDrawable: TALDrawable;
      LStateStyle.BufSupportingTextDrawableRect, // var ABufSupportingTextDrawableRect: TRectF;
      FSupportingText, // const AText: String;
      LStateStyle.SupportingTextSettings.font); // const AFont: TALFont;
    LStateStyle.BufSupportingTextDrawableRect.SetLocation(
      padding.Left + SupportingTextSettings.Margins.Left,
      Height+SupportingTextSettings.Margins.Top);
  finally
    LStateStyle.SupportingTextSettings.RestorestateNoChanges;
  end;
end;

{*************************************}
procedure TALBaseEdit.clearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (ALIsDrawableNull(FBufDrawable)) and // warn will be raise in inherited
     ((not ALIsDrawableNull(FStateStyles.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Focused.FBufDrawable))) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
  inherited clearBufDrawable;
  clearBufPromptTextDrawable;
  clearBufLabelTextDrawable;
  clearBufSupportingTextDrawable;
end;

{***********************************************}
procedure TALBaseEdit.ClearBufPromptTextDrawable;
begin
  ALFreeAndNilDrawable(fBufPromptTextDrawable);
  if FStateStyles <> nil then
    FStateStyles.clearBufPromptTextDrawable;
end;

{**********************************************}
procedure TALBaseEdit.ClearBufLabelTextDrawable;
begin
  ALFreeAndNilDrawable(fBufLabelTextDrawable);
  if FStateStyles <> nil then
    FStateStyles.clearBufLabelTextDrawable;
end;

{***************************************************}
procedure TALBaseEdit.ClearBufSupportingTextDrawable;
begin
  if (FSupportingTextMarginBottomUpdated) and
     (not ALIsDrawableNull(fBufSupportingTextDrawable)) and
     (not (csDestroying in ComponentState)) then begin
    FSupportingTextMarginBottomUpdated := False;
    Margins.Bottom := FSupportingTextSettings.Margins.Bottom;
  end;
  ALFreeAndNilDrawable(fBufSupportingTextDrawable);
  if FStateStyles <> nil then
    FStateStyles.clearBufSupportingTextDrawable;
end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALBaseEdit.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  if StateStyles.IsTransitionAnimationRunning then begin
    Result := ARect;
    if StateStyles.TransitionFrom <> nil then begin
      var LFromSurfaceRect := ALGetShapeSurfaceRect(
                                ARect, // const ARect: TRectF;
                                _TALBaseStateStyleAccessProtected(StateStyles.TransitionFrom).Fill, // const AFill: TALBrush;
                                nil, // const AFillResourceStream: TStream;
                                _TALBaseStateStyleAccessProtected(StateStyles.TransitionFrom).StateLayer, // const AStateLayer: TALStateLayer;
                                _TALBaseStateStyleAccessProtected(StateStyles.TransitionFrom).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LFromSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
    if StateStyles.TransitionTo <> nil then begin
      var LToSurfaceRect := ALGetShapeSurfaceRect(
                              ARect, // const ARect: TRectF;
                              _TALBaseStateStyleAccessProtected(StateStyles.TransitionTo).Fill, // const AFill: TALBrush;
                              nil, // const AFillResourceStream: TStream;
                              _TALBaseStateStyleAccessProtected(StateStyles.TransitionTo).StateLayer, // const AStateLayer: TALStateLayer;
                              _TALBaseStateStyleAccessProtected(StateStyles.TransitionTo).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LToSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  LStateStyle.Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
                  LStateStyle.Shadow); // const AShadow: TALShadow): TRectF;
    end
    else begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  nil, // const AStateLayer: TALStateLayer;
                  Shadow); // const AShadow: TALShadow): TRectF;
    end;
  end;
end;
{$ENDIF}

{**************************}
procedure TALBaseEdit.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;
  MakeBufDrawable;
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);

  {$REGION 'Background'}
  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  if StateStyles.IsTransitionAnimationRunning then begin
    LDrawable := ALNullDrawable;
    LDrawableRect := TRectF.Empty;
  end
  else begin
    if LStateStyle <> nil then begin
      LDrawable := LStateStyle.FBufDrawable;
      LDrawableRect := LStateStyle.FBufDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        LDrawable := FBufDrawable;
        LDrawableRect := FBufDrawableRect;
      end;
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;
  //--
  if ALIsDrawableNull(LDrawable) then begin

    var LCurrentAdjustedStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);
    if LCurrentAdjustedStateStyle = nil then inherited Paint
    else begin

      {$IF DEFINED(ALSkiaCanvas)}

      // Using a matrix on the canvas results in smoother animations compared to using
      // Ascale with DrawMultilineText. This is because changes in scale affect the font size,
      // leading to rounding issues (I spent many hours looking for a way to avoid this).
      // If there is an animation, it appears jerky because the text position
      // shifts up or down with scale changes due to pixel alignment.
      var LCanvasSaveState: TCanvasSaveState := ALScaleAndCenterCanvas(
                                                  Canvas, // Const ACanvas: TCanvas;
                                                  AbsoluteRect, // Const AAbsoluteRect: TRectF;
                                                  LCurrentAdjustedStateStyle.Scale, // Const AScale: Single;
                                                  true); // Const ASaveState: Boolean);
      try

        TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
          .SetAlignToPixel(IsPixelAlignmentEnabled)
          .SetDstRect(LocalRect)
          .SetOpacity(AbsoluteOpacity)
          .SetFill(LCurrentAdjustedStateStyle.Fill)
          .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, LCurrentAdjustedStateStyle.TextSettings.Font.Color)
          .SetStroke(LCurrentAdjustedStateStyle.Stroke)
          .SetShadow(LCurrentAdjustedStateStyle.Shadow)
          .SetSides(Sides)
          .SetCorners(Corners)
          .SetXRadius(XRadius)
          .SetYRadius(YRadius)
          .Draw;

      finally
        if LCanvasSaveState <> nil then
          Canvas.RestoreState(LCanvasSaveState);
      end;

      {$ELSE}

      var LRect := LocalRect;
      InitRenderTargets(LRect);
      if ALCanvasBeginScene(RenderTargetCanvas) then
      try

        ALClearCanvas(RenderTargetCanvas, TAlphaColors.Null);

        TALDrawRectangleHelper.Create(RenderTargetCanvas)
          .SetScale(ALGetScreenScale)
          .SetAlignToPixel(IsPixelAlignmentEnabled)
          .SetDstRect(LocalRect)
          .SetFill(LCurrentAdjustedStateStyle.Fill)
          .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, LCurrentAdjustedStateStyle.TextSettings.Font.Color)
          .SetStroke(LCurrentAdjustedStateStyle.Stroke)
          .SetShadow(LCurrentAdjustedStateStyle.Shadow)
          .SetSides(Sides)
          .SetCorners(Corners)
          .SetXRadius(XRadius)
          .SetYRadius(YRadius)
          .Draw;

      finally
        ALCanvasEndScene(RenderTargetCanvas)
      end;

      ALUpdateDrawableFromSurface(RenderTargetSurface, RenderTargetDrawable);

      // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
      // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
      LRect.Offset(-2*LRect.Left, -2*LRect.Top);

      // LRect must include the LScale
      LRect.Top := LRect.Top * LCurrentAdjustedStateStyle.Scale;
      LRect.right := LRect.right * LCurrentAdjustedStateStyle.Scale;
      LRect.left := LRect.left * LCurrentAdjustedStateStyle.Scale;
      LRect.bottom := LRect.bottom * LCurrentAdjustedStateStyle.Scale;

      // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect
      // (due to autosizing with different font sizes), we must center LStateStyle.FBufDrawableRect
      // within the main BufDrawableRect to ensure that all changes are visually centered.
      var LMainDrawableRect: TRectF;
      If AlIsDrawableNull(FBufDrawable) then LMainDrawableRect := LocalRect
      else LMainDrawableRect := FBufDrawableRect;
      LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
      var LCenteredRect := LRect.CenterAt(LMainDrawableRect);
      LRect.Offset(LCenteredRect.Left, LCenteredRect.top);

      // We cannot use the matrix because, if we do, ALAlignToPixelRound in ALDrawDrawable
      // will be ineffective since the matrix will no longer be a simple translation matrix.
      // In such a case, TCustomCanvasGpu(ACanvas).DrawTexture may produce border artifacts
      // if the texture is not perfectly pixel-aligned.
      var LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(RenderTargetDrawable), ALGetDrawableHeight(RenderTargetDrawable));
      LDstRect.Width := (LDstRect.Width / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
      LDstRect.height := (LDstRect.height / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
      LDstRect.SetLocation(
        LRect.Left,
        LRect.Top);
      ALDrawDrawable(
        Canvas, // const ACanvas: Tcanvas;
        RenderTargetDrawable, // const ADrawable: TALDrawable;
        LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
        AbsoluteOpacity); // const AOpacity: Single)

      {$ENDIF}

    end;

  end
  else
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      LDrawable, // const ADrawable: TALDrawable;
      LDrawableRect.TopLeft, // const ATopLeft: TpointF;
      AbsoluteOpacity); // const AOpacity: Single);
  {$ENDREGION}

  {$REGION 'LabelText'}
  var LLabelTextDrawable: TALDrawable;
  var LLabelTextDrawableRect: TRectF;
  if LStateStyle <> nil then begin
    LLabelTextDrawable := LStateStyle.BufLabelTextDrawable;
    LLabelTextDrawableRect := LStateStyle.BufLabelTextDrawableRect;
    if ALIsDrawableNull(LLabelTextDrawable) then begin
      LLabelTextDrawable := BufLabelTextDrawable;
      LLabelTextDrawableRect := BufLabelTextDrawableRect;
    end;
  end
  else begin
    LLabelTextDrawable := BufLabelTextDrawable;
    LLabelTextDrawableRect := BufLabelTextDrawableRect;
  end;
  //--
  if LabelTextSettings.Layout = TLabelTextLayout.Inline then
    LLabelTextDrawableRect.SetLocation(
      EditControl.Left + LabelTextSettings.Margins.Left,
      Padding.top+FeditControl.Margins.top-LLabelTextDrawableRect.Height-LabelTextSettings.Margins.Bottom)
  else
    LLabelTextDrawableRect.SetLocation(
      padding.Left + LabelTextSettings.Margins.Left,
      0-LLabelTextDrawableRect.Height-LabelTextSettings.Margins.Bottom);
  //--
  if (GetIsTextEmpty and HasTranslationLabelTextAnimation and (hasNativeView)) or
     ((Labeltext <> '') and (prompttext <> '') and (prompttext <> labeltext)) or
     (not GetIsTextEmpty) or
     (GetIsTextEmpty and HasOpacityLabelTextAnimation and FLabelTextAnimation.Running) then begin
    if (not ALisDrawableNull(LLabelTextDrawable)) and SameValue(AbsoluteOpacity, 1, TEpsilon.Scale) then begin
      var LRect := LLabelTextDrawableRect;
      LRect.Inflate(4{DL}, 4{DT}, 4{DR}, 4{DB});
      LRect.Intersect(LocalRect);
      if not LRect.IsEmpty then begin
        LRect := ALAlignToPixelRound(LRect, Canvas.Matrix, Canvas.Scale, TEpsilon.position);
        Canvas.Fill.Kind := TBrushKind.Solid;
        Canvas.Fill.Color := EditControl.FillColor;
        Canvas.FillRect(LRect, ALIfThen(FLabelTextAnimation.Running, Min(AbsoluteOpacity,FlabelTextAnimation.CurrentValue*AbsoluteOpacity*2), AbsoluteOpacity));
      end;
    end;
    //--
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      LLabelTextDrawable, // const ADrawable: TALDrawable;
      LLabelTextDrawableRect.TopLeft, // const ATopLeft: TpointF;
      ALIfThen(FLabelTextAnimation.Running, FLabelTextAnimation.CurrentValue*AbsoluteOpacity, AbsoluteOpacity)); // const AOpacity: Single);
  end;
  {$ENDREGION}

  {$REGION 'PromptText'}
  if (not Enabled) or
     (GetIsTextEmpty and HasTranslationLabelTextAnimation and (not hasNativeView))
     {$IF defined(ALDPK)}or true{$ENDIF} then begin
    var LPromptTextDrawable: TALDrawable;
    var LPromptTextDrawableRect: TRectF;
    if LStateStyle <> nil then begin
      LPromptTextDrawable := LStateStyle.BufPromptTextDrawable;
      LPromptTextDrawableRect := LStateStyle.BufPromptTextDrawableRect;
      if ALIsDrawableNull(LPromptTextDrawable) then begin
        LPromptTextDrawable := BufPromptTextDrawable;
        LPromptTextDrawableRect := BufPromptTextDrawableRect;
      end;
    end
    else begin
      LPromptTextDrawable := BufPromptTextDrawable;
      LPromptTextDrawableRect := BufPromptTextDrawableRect;
    end;
    //--
    Var LPos: TpointF;
    case TextSettings.HorzAlign of
      TALTextHorzAlign.Center: LPos.X := EditControl.Position.X + ((EditControl.width - LPromptTextDrawableRect.Width) / 2);
      TALTextHorzAlign.Leading: LPos.X := EditControl.Position.X;
      TALTextHorzAlign.Trailing: LPos.X := EditControl.Position.X + EditControl.Width - LPromptTextDrawableRect.Width;
      TALTextHorzAlign.Justify: LPos.X := EditControl.Position.X;
      else
        raise Exception.Create('Error EC344A62-E381-4126-A0EB-12BFC91E3664');
    end;
    if GetIsTextEmpty and (HasTranslationLabelTextAnimation) then begin
      if LabelTextSettings.Layout = TLabelTextLayout.Inline then begin
        case TextSettings.VertAlign of
          TALTextVertAlign.Center: LPos.y := LLabelTextDrawableRect.Top + ((EditControl.Height + LLabelTextDrawableRect.Height + LabelTextSettings.Margins.Bottom - LPromptTextDrawableRect.Height) / 2);
          TALTextVertAlign.Leading: LPos.y := EditControl.Position.y;
          TALTextVertAlign.Trailing: LPos.y := EditControl.Position.y + EditControl.Height - LPromptTextDrawableRect.Height;
          Else
            Raise Exception.Create('Error 28B2F8BC-B4C5-4F22-8E21-681AA1CA3C23')
        end;
      end
      else if LabelTextSettings.Layout = TLabelTextLayout.floating then begin
        case TextSettings.VertAlign of
          TALTextVertAlign.Center: LPos.y := EditControl.Position.y + ((EditControl.Height - LPromptTextDrawableRect.Height) / 2);
          TALTextVertAlign.Leading: LPos.y := EditControl.Position.y;
          TALTextVertAlign.Trailing: LPos.y := EditControl.Position.y + EditControl.Height - LPromptTextDrawableRect.Height;
          Else
            Raise Exception.Create('Error 28B2F8BC-B4C5-4F22-8E21-681AA1CA3C23')
        end;
      end
      else
        raise Exception.Create('Error 42442487-01E1-4810-A030-FC4904834EC2');
      LPromptTextDrawableRect.SetLocation(LPos.X, LPos.y);
      if (FLabelTextAnimation.Running) then begin
        LPromptTextDrawableRect.SetLocation(
          LPromptTextDrawableRect.Left - ((LPromptTextDrawableRect.Left - LLabelTextDrawableRect.Left) * FLabelTextAnimation.CurrentValue),
          LPromptTextDrawableRect.Top - ((LPromptTextDrawableRect.top - LLabelTextDrawableRect.top) * FLabelTextAnimation.CurrentValue));
        LPromptTextDrawableRect.Width := LPromptTextDrawableRect.width - ((LPromptTextDrawableRect.width - LLabelTextDrawableRect.Width) * FLabelTextAnimation.CurrentValue);
        LPromptTextDrawableRect.Height := LPromptTextDrawableRect.Height - ((LPromptTextDrawableRect.Height - LLabelTextDrawableRect.Height) * FLabelTextAnimation.CurrentValue);
        if (not ALisDrawableNull(LPromptTextDrawable)) then begin
          var LRect := LPromptTextDrawableRect;
          LRect.Inflate(4{DL}, 4{DT}, 4{DR}, 4{DB});
          LRect.Intersect(LocalRect);
          if not LRect.IsEmpty then begin
            LRect := ALAlignToPixelRound(LRect, Canvas.Matrix, Canvas.Scale, TEpsilon.position);
            Canvas.Fill.Kind := TBrushKind.Solid;
            Canvas.Fill.Color := EditControl.FillColor;
            Canvas.FillRect(LRect, 1);
          end;
        end;
        ALDrawDrawable(
          Canvas, // const ACanvas: Tcanvas;
          LPromptTextDrawable, // const ADrawable: TALDrawable;
          LPromptTextDrawableRect, // const ADstRect: TrectF;
          AbsoluteOpacity); // const AOpacity: Single);
      end
      else
        ALDrawDrawable(
          Canvas, // const ACanvas: Tcanvas;
          LPromptTextDrawable, // const ADrawable: TALDrawable;
          LPromptTextDrawableRect.TopLeft, // const ATopLeft: TpointF;
          AbsoluteOpacity); // const AOpacity: Single);
    end
    else begin
      case TextSettings.VertAlign of
        TALTextVertAlign.Center: LPos.y := EditControl.Position.y + ((EditControl.Height - LPromptTextDrawableRect.Height) / 2);
        TALTextVertAlign.Leading: LPos.y := EditControl.Position.y;
        TALTextVertAlign.Trailing: LPos.y := EditControl.Position.y + EditControl.Height - LPromptTextDrawableRect.Height;
        Else
          Raise Exception.Create('Error 84D5B492-1F70-43A0-AB68-D30C70D260BC')
      end;
      LPromptTextDrawableRect.SetLocation(LPos.X, LPos.y);
      ALDrawDrawable(
        Canvas, // const ACanvas: Tcanvas;
        LPromptTextDrawable, // const ADrawable: TALDrawable;
        LPromptTextDrawableRect.TopLeft, // const ATopLeft: TpointF;
        AbsoluteOpacity); // const AOpacity: Single);
    end;
  end;
  {$ENDREGION}

  {$REGION 'SupportingText'}
  var LSupportingTextDrawable: TALDrawable;
  var LSupportingTextDrawableRect: TRectF;
  if LStateStyle <> nil then begin
    LSupportingTextDrawable := LStateStyle.BufSupportingTextDrawable;
    LSupportingTextDrawableRect := LStateStyle.BufSupportingTextDrawableRect;
    if ALIsDrawableNull(LSupportingTextDrawable) then begin
      LSupportingTextDrawable := BufSupportingTextDrawable;
      LSupportingTextDrawableRect := BufSupportingTextDrawableRect;
    end;
  end
  else begin
    LSupportingTextDrawable := BufSupportingTextDrawable;
    LSupportingTextDrawableRect := BufSupportingTextDrawableRect;
  end;
  //--
  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LSupportingTextDrawable, // const ADrawable: TALDrawable;
    LSupportingTextDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);
  {$ENDREGION}

end;

{******************************************}
function TALBaseEdit.HasNativeView: Boolean;
begin
  result := EditControl.HasNativeView;
end;

{**********************************}
Procedure TALBaseEdit.AddNativeView;
begin
  FNativeViewRemoved := False;
  UpdateNativeViewVisibility;
end;

{*************************************}
Procedure TALBaseEdit.RemoveNativeView;
begin
  FNativeViewRemoved := True;
  ResetFocus;
  EditControl.RemoveNativeView;
end;

{******************************************************************************}
Procedure TALBaseEdit.SetSelection(const AStart: integer; const AStop: Integer);
begin
  EditControl.SetSelection(AStart, AStop);
end;

{********************************************************}
Procedure TALBaseEdit.SetSelection(const AIndex: integer);
begin
  EditControl.SetSelection(AIndex);
end;

{*****************************************}
function TALBaseEdit.getLineCount: integer;
begin
  Result := EditControl.getLineCount;
end;

{*****************************************}
function TALBaseEdit.getLineHeight: single;
begin
  result := EditControl.getLineHeight;
end;

{*********************************************}
constructor TALEdit.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSize := True;
end;

{**************************************************}
function TALEdit.HasUnconstrainedAutosizeX: Boolean;
begin
  result := False;
end;

{***************************}
procedure TALEdit.AdjustSize;
begin
  if (not (csLoading in ComponentState)) and // loaded will call again AdjustSize
     (not (csDestroying in ComponentState)) and // if csDestroying do not do autosize
     (scene <> nil) and // SetNewScene will call again AdjustSize
     (TNonReentrantHelper.EnterSection(FIsAdjustingSize)) then begin // non-reantrant
    try

      if isupdating then begin
        FAdjustSizeOnEndUpdate := True;
        Exit;
      end
      else
        FAdjustSizeOnEndUpdate := False;

      {$IF defined(debug)}
      //ALLog(ClassName + '.AdjustSize', 'Name: ' + Name + ' | HasUnconstrainedAutosize(X/Y) : '+ALBoolToStrW(HasUnconstrainedAutosizeX)+'/'+ALBoolToStrW(HasUnconstrainedAutosizeY));
      {$ENDIF}

      Var LInlinedLabelText := (LabelText <> '') and (LabelTextSettings.Layout = TLabelTextLayout.Inline);
      if LInlinedLabelText then MakeBufLabelTextDrawable;

      var LStrokeSize := TRectF.Empty;
      if Stroke.HasStroke then begin
        if (TSide.Top in Sides) then    LStrokeSize.Top :=    max(Stroke.Thickness - Padding.top,    0);
        if (TSide.bottom in Sides) then LStrokeSize.bottom := max(Stroke.Thickness - Padding.bottom, 0);
        if (TSide.right in Sides) then  LStrokeSize.right :=  max(Stroke.Thickness - Padding.right,  0);
        if (TSide.left in Sides) then   LStrokeSize.left :=   max(Stroke.Thickness - Padding.left,   0);
      end;

      if HasUnconstrainedAutosizeY then begin

        var LLineHeight: Single := GetLineHeight;
        if IsPixelAlignmentEnabled then LLineHeight := ALAlignDimensionToPixelRound(LLineHeight, ALGetScreenScale, TEpsilon.Position);

        If LInlinedLabelText then begin
          SetFixedSizeBounds(
            Position.X,
            Position.Y,
            Width,
            LLineHeight + LStrokeSize.Top + LStrokeSize.bottom + padding.Top + padding.Bottom + BufLabelTextDrawableRect.Height + LabelTextSettings.Margins.Top + LabelTextSettings.Margins.bottom);
        end
        else begin
          SetFixedSizeBounds(
            Position.X,
            Position.Y,
            Width,
            LLineHeight + LStrokeSize.Top + LStrokeSize.bottom + padding.Top + padding.Bottom);
        end;

      end;

      var LMarginRect := TRectF.Empty;

      {$IF defined(MSWINDOWS) or defined(ALMacOS)}
      // In Windows and MacOS, there is no way to align text vertically,
      // so we must center the EditControl.
      If not LInlinedLabelText then begin
        Var LAvailableHeight := Height - LStrokeSize.Top - LStrokeSize.bottom - Padding.top - Padding.Bottom;
        var LEditControlHeight := GetLineHeight;
        {$IF defined(ALMacOS)}
        // For an obscure reason, when FocusRingType is set to NSFocusRingTypeNone and the font size
        // is not 16, focusing on the EditField causes it to shift up by 2 pixels.
        LEditControlHeight := LEditControlHeight + 2;
        {$ENDIF}

        LMarginRect.top := (LAvailableHeight - LEditControlHeight) / 2;
        LMarginRect.bottom := (LAvailableHeight - LEditControlHeight) / 2;
      end;
      {$ENDIF}

      if LInlinedLabelText then begin
        Var LAvailableHeight := Height - LStrokeSize.Top - LStrokeSize.bottom - Padding.top - Padding.Bottom;
        var LEditControlHeight := GetLineHeight;
        {$IF defined(ALMacOS)}
        // For an obscure reason, when FocusRingType is set to NSFocusRingTypeNone and the font size
        // is not 16, focusing on the EditField causes it to shift up by 2 pixels.
        LEditControlHeight := LEditControlHeight + 2;
        {$ENDIF}

        LMarginRect.top := (LAvailableHeight - LEditControlHeight - BufLabelTextDrawableRect.Height - LabelTextSettings.Margins.Top - LabelTextSettings.Margins.bottom) / 2;
        LMarginRect.top := LMarginRect.top + BufLabelTextDrawableRect.Height + LabelTextSettings.Margins.Top + LabelTextSettings.Margins.bottom;
        LMarginRect.bottom := (LAvailableHeight - LEditControlHeight - BufLabelTextDrawableRect.Height - LabelTextSettings.Margins.Top - LabelTextSettings.Margins.bottom) / 2;
      end;

      LMarginRect.left :=   Max(LMarginRect.left   + LStrokeSize.left,   0);
      LMarginRect.Top :=    Max(LMarginRect.Top    + LStrokeSize.Top,    0);
      LMarginRect.Right :=  Max(LMarginRect.Right  + LStrokeSize.Right,  0);
      LMarginRect.Bottom := Max(LMarginRect.Bottom + LStrokeSize.Bottom, 0);

      if IsPixelAlignmentEnabled then
        LMarginRect := ALAlignEdgesToPixelRound(LMarginRect, ALGetScreenScale, TEpsilon.Position);

      EditControl.Margins.Rect := LMarginRect;

    finally
      TNonReentrantHelper.LeaveSection(FIsAdjustingSize)
    end;
  end;
end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALEdit]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALEdit, 'Size');
  UnlistPublishedProperty(TALEdit, 'StyleName');
  UnlistPublishedProperty(TALEdit, 'OnTap');
  {$ENDIF}
end;

initialization
  RegisterFmxClasses([TALEdit]);

end.
