unit Alcinoe.FMX.Edit;

interface

{$I Alcinoe.inc}

uses
  System.Types,
  system.Classes,
  System.UITypes,
  {$IF defined(android)}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.Widget,
  Androidapi.JNI.JavaTypes,
  Alcinoe.AndroidApi.Widget,
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
  fmx.Graphics,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.NativeControl,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Common;

Type

  {******************}
  TALBaseEdit = class;

  {***************************}
  TALAutoCapitalizationType = (
    acNone, // Specifies that there is no automatic text capitalization.
    acWords, // Specifies automatic capitalization of the first letter of each word.
    acSentences, // Specifies automatic capitalization of the first letter of each sentence.
    acAllCharacters); // Specifies automatic capitalization of all characters, such as for entry of two-character state abbreviations for the United States.

{$REGION ' ANDROID'}
{$IF defined(android)}

  {**********************************************}
  TALAndroidEditView = class(TALAndroidNativeView)
  private
    type
      // ------------
      // TTextWatcher
      TTextWatcher = class(TJavaLocal, JTextWatcher)
      private
        FEditView: TALAndroidEditView;
      public
        constructor Create(const aEditView: TALAndroidEditView);
        procedure afterTextChanged(s: JEditable); cdecl;
        procedure beforeTextChanged(s: JCharSequence; start: Integer; count: Integer; after: Integer); cdecl;
        procedure onTextChanged(s: JCharSequence; start: Integer; before: Integer; count: Integer); cdecl;
      end;
      // ---------------------
      // TEditorActionListener
      TEditorActionListener = class(TJavaLocal, JTextView_OnEditorActionListener)
      private
        fIsMultiLineEditText: Boolean;
        FEditView: TALAndroidEditView;
      public
        constructor Create(const aEditView: TALAndroidEditView; const aIsMultiLineEditText: Boolean = false);
        function onEditorAction(v: JTextView; actionId: Integer; event: JKeyEvent): Boolean; cdecl;
      end;
      // ------------------
      // TKeyPreImeListener
      TKeyPreImeListener = class(TJavaLocal, JALKeyPreImeListener)
      private
        FEditView: TALAndroidEditView;
      public
        constructor Create(const aEditView: TALAndroidEditView);
        function onKeyPreIme(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
      end;
      // --------------
      // TTouchListener
      TTouchListener = class(TJavaLocal, JView_OnTouchListener)
      private
        FEditView: TALAndroidEditView;
      public
        constructor Create(const aEditView: TALAndroidEditView);
        function onTouch(v: JView; event: JMotionEvent): Boolean; cdecl;
      end;
  private
    FTextSettings: TALBaseTextSettings;
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
    FTextWatcher: TTextWatcher;
    FEditorActionListener: TEditorActionListener;
    FKeyPreImeListener: TKeyPreImeListener;
    FTouchListener: TTouchListener;
    function GetView: JALEditText;
    function GetControl: TALBaseEdit;
    procedure DoSetInputType(
                const aKeyboardType: TVirtualKeyboardType;
                const aAutoCapitalizationType: TALAutoCapitalizationType;
                const aPassword: Boolean;
                const aCheckSpelling: Boolean;
                const aIsMultiline: Boolean);
    procedure DoSetReturnKeyType(const aReturnKeyType: TReturnKeyType);
    function GetKeyboardType: TVirtualKeyboardType;
    procedure setKeyboardType(const Value: TVirtualKeyboardType);
    function GetAutoCapitalizationType: TALAutoCapitalizationType;
    procedure setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
    function GetPassword: Boolean;
    procedure setPassword(const Value: Boolean);
    function GetCheckSpelling: Boolean;
    procedure setCheckSpelling(const Value: Boolean);
    function GetReturnKeyType: TReturnKeyType;
    procedure setReturnKeyType(const Value: TReturnKeyType);
    function GetPromptText: String;
    procedure setPromptText(const Value: String);
    function GetPromptTextColor: TAlphaColor;
    procedure setPromptTextColor(const Value: TAlphaColor);
    function GetTintColor: TAlphaColor;
    procedure setTintColor(const Value: TAlphaColor);
    function GetFillColor: TAlphaColor;
    procedure SetFillColor(const Value: TAlphaColor);
    procedure SetTextSettings(const Value: TALBaseTextSettings);
    procedure TextSettingsChanged(Sender: TObject);
    function getText: String;
    procedure SetText(const Value: String);
    function GetMaxLength: integer;
    procedure SetMaxLength(const Value: integer);
  protected
    function CreateView: JView; override;
  public
    constructor Create(const AControl: TALControl; Const aIsMultiline: Boolean = False; const aDefStyleAttr: String = ''; const aDefStyleRes: String = ''); reintroduce;
    destructor Destroy; override;
    property View: JALEditText read GetView;
    property Control: TALBaseEdit read GetControl;
    function getLineCount: integer; virtual;
    function getLineHeight: Single; virtual; // It includes the line spacing
    Procedure SetSelection(const AStart: integer; const AStop: Integer); overload; virtual;
    Procedure SetSelection(const AIndex: integer); overload; virtual;
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

{$endif}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}

  {******************************************}
  TALIosBaseEditView = class(TALIosNativeView)
  protected
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
    function GetTextSettings: TALBaseTextSettings; virtual; abstract;
    procedure SetTextSettings(const Value: TALBaseTextSettings); virtual; abstract;
    function getText: String; virtual; abstract;
    procedure SetText(const Value: String); virtual; abstract;
    function GetMaxLength: integer; virtual; abstract;
    procedure SetMaxLength(const Value: integer); virtual; abstract;
  public
    function getLineCount: integer; virtual; abstract;
    function getLineHeight: Single; virtual; abstract; // It includes the line spacing
    Procedure SetSelection(const AStart: integer; const AStop: Integer); overload; virtual; abstract;
    Procedure SetSelection(const AIndex: integer); overload; virtual; abstract;
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
    property TextSettings: TALBaseTextSettings read GetTextSettings write SetTextSettings;
    property CheckSpelling: Boolean read GetCheckSpelling write SetCheckSpelling;
  end;

  {*************************************}
  IALIosEditView = interface(UITextField)
    ['{E4E67240-F15A-444F-8CE1-A3A830C023E8}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure ControlEventEditingChanged; cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
  end;

  {****************************************}
  TALIosEditView = class(TALIosBaseEditView)
  private
    type
      // ------------------
      // TTextFieldDelegate
      TTextFieldDelegate = class(TOCLocal, UITextFieldDelegate)
      private
        FEditView: TALIosEditView;
      public
        constructor Create(const AEditView: TALIosEditView);
        // Better name would be textFieldShouldChangeCharactersInRange
        function textField(textField: UITextField; shouldChangeCharactersInRange: NSRange; replacementString: NSString): Boolean; cdecl;
        procedure textFieldDidBeginEditing(textField: UITextField); cdecl;
        procedure textFieldDidEndEditing(textField: UITextField); cdecl;
        function textFieldShouldBeginEditing(textField: UITextField): Boolean; cdecl;
        function textFieldShouldClear(textField: UITextField): Boolean; cdecl;
        function textFieldShouldEndEditing(textField: UITextField): Boolean; cdecl;
        function textFieldShouldReturn(textField: UITextField): Boolean; cdecl;
      end;
  private
    FTextFieldDelegate: TTextFieldDelegate;
    FTextSettings: TALBaseTextSettings;
    FFillColor: TAlphaColor;
    fMaxLength: integer;
    fPromptTextColor: TalphaColor;
    function GetView: UITextField;
    function GetControl: TALBaseEdit;
    procedure applyPromptTextWithColor(const aStr: String; const aColor: TAlphaColor);
    procedure TextSettingsChanged(Sender: TObject);
    procedure DoChange;
  protected
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
    function GetTextSettings: TALBaseTextSettings; override;
    procedure SetTextSettings(const Value: TALBaseTextSettings); override;
    function getText: String; override;
    procedure SetText(const Value: String); override;
    function GetMaxLength: integer; override;
    procedure SetMaxLength(const Value: integer); override;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure ControlEventEditingChanged; cdecl;
    property View: UITextField read GetView;
    property Control: TALBaseEdit read GetControl;
    procedure SetEnabled(const value: Boolean); override;
    function getLineCount: integer; override;
    function getLineHeight: Single; override; // It includes the line spacing
    Procedure SetSelection(const AStart: integer; const AStop: Integer); overload; override;
    Procedure SetSelection(const AIndex: integer); overload; override;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}

  {******************************************}
  TALMacBaseEditView = class(TALMacNativeView)
  protected
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
    function GetTextSettings: TALBaseTextSettings; virtual; abstract;
    procedure SetTextSettings(const Value: TALBaseTextSettings); virtual; abstract;
    function getText: String; virtual; abstract;
    procedure SetText(const Value: String); virtual; abstract;
    function GetMaxLength: integer; virtual; abstract;
    procedure SetMaxLength(const Value: integer); virtual; abstract;
  public
    function getLineCount: integer; virtual; abstract;
    function getLineHeight: Single; virtual; abstract; // It includes the line spacing
    Procedure SetSelection(const AStart: integer; const AStop: Integer); overload; virtual; abstract;
    Procedure SetSelection(const AIndex: integer); overload; virtual; abstract;
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
    property TextSettings: TALBaseTextSettings read GetTextSettings write SetTextSettings;
    property CheckSpelling: Boolean read GetCheckSpelling write SetCheckSpelling;
  end;

  {*************************************}
  IALMacEditView = interface(NSTextField)
    ['{8FA87FC0-77FB-4451-8F92-52EE1AFDD94C}']
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
  end;

  {****************************************}
  TALMacEditView = class(TALMacBaseEditView)
  private
    type
      // --------------------
      // TTextEditingDelegate
      TTextEditingDelegate = class(TOCLocal, Alcinoe.Macapi.AppKit.NSControlTextEditingDelegate)
      private
        FEditView: TALMacEditView;
      public
        constructor Create(const AEditView: TALMacEditView);
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
  private
    FTextFieldDelegate: TTextEditingDelegate;
    FTextSettings: TALBaseTextSettings;
    FFillColor: TAlphaColor;
    fMaxLength: integer;
    fReturnKeyType: TReturnKeyType;
    fKeyboardType: TVirtualKeyboardType;
    fAutoCapitalizationType: TALAutoCapitalizationType;
    fPassword: boolean;
    fCheckSpelling: boolean;
    fPromptTextColor: TalphaColor;
    fTintColor: TalphaColor;
    function GetView: NSTextField;
    function GetControl: TALBaseEdit;
    procedure applyPromptTextWithColor(const aStr: String; const aColor: TAlphaColor);
    procedure TextSettingsChanged(Sender: TObject);
    procedure DoChange;
  protected
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
    function GetTextSettings: TALBaseTextSettings; override;
    procedure SetTextSettings(const Value: TALBaseTextSettings); override;
    function getText: String; override;
    procedure SetText(const Value: String); override;
    function GetMaxLength: integer; override;
    procedure SetMaxLength(const Value: integer); override;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    property View: NSTextField read GetView;
    property Control: TALBaseEdit read GetControl;
    procedure SetEnabled(const value: Boolean); override;
    function getLineCount: integer; override;
    function getLineHeight: Single; override; // It includes the line spacing
    Procedure SetSelection(const AStart: integer; const AStop: Integer); overload; override;
    Procedure SetSelection(const AIndex: integer); overload; override;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

  {**************************************}
  TALWinEditView = class(TALWinNativeView)
  private
    FTextSettings: TALBaseTextSettings;
    FFillColor: TAlphaColor;
    FPromptText: String;
    FPromptTextColor: TalphaColor;
    fReturnKeyType: TReturnKeyType;
    fKeyboardType: TVirtualKeyboardType;
    fAutoCapitalizationType: TALAutoCapitalizationType;
    fCheckSpelling: boolean;
    fTintColor: TalphaColor;
    FFontHandle: HFONT;
    FBackgroundBrush: HBRUSH;
    function GetControl: TALBaseEdit;
    procedure UpdateFontHandle;
    procedure UpdateBackgroundBrush;
    function GetKeyboardType: TVirtualKeyboardType;
    procedure setKeyboardType(const Value: TVirtualKeyboardType);
    function GetAutoCapitalizationType: TALAutoCapitalizationType;
    procedure setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
    function GetPassword: Boolean;
    procedure setPassword(const Value: Boolean);
    function GetCheckSpelling: Boolean;
    procedure setCheckSpelling(const Value: Boolean);
    function GetReturnKeyType: TReturnKeyType;
    procedure setReturnKeyType(const Value: TReturnKeyType);
    function GetPromptText: String;
    procedure setPromptText(const Value: String);
    function GetPromptTextColor: TAlphaColor;
    procedure setPromptTextColor(const Value: TAlphaColor);
    function GetTintColor: TAlphaColor;
    procedure setTintColor(const Value: TAlphaColor);
    function GetFillColor: TAlphaColor;
    procedure SetFillColor(const Value: TAlphaColor);
    procedure SetTextSettings(const Value: TALBaseTextSettings);
    procedure TextSettingsChanged(Sender: TObject);
    function getText: String;
    procedure SetText(const Value: String);
    function GetMaxLength: integer;
    procedure SetMaxLength(const Value: integer);
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
    constructor Create; override;
    destructor Destroy; override;
    property Control: TALBaseEdit read GetControl;
    function getLineCount: integer; virtual;
    function getLineHeight: Single; virtual; // It includes the line spacing
    Procedure SetSelection(const AStart: integer; const AStop: Integer); overload; virtual;
    Procedure SetSelection(const AIndex: integer); overload; virtual;
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

{$endif}
{$ENDREGION}

  {************************************************************}
  TALBaseEdit = class(TALNativeControl, IVirtualKeyboardControl)
  public
    type
      // -----
      // TFill
      TFill = class(TALBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
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
        FPromptTextColorKey: String;
        FTintColor: TalphaColor;
        FTintColorKey: String;
        FTextSettings: TBaseStateStyle.TTextSettings;
        FLabelTextSettings: TBaseStateStyle.TLabelTextSettings;
        FSupportingTextSettings: TBaseStateStyle.TSupportingTextSettings;
        function GetStateStyleParent: TBaseStateStyle;
        function GetControlParent: TALBaseEdit;
        procedure SetPromptTextColor(const AValue: TAlphaColor);
        procedure SetPromptTextColorKey(const AValue: String);
        procedure SetTintColor(const AValue: TAlphaColor);
        procedure SetTintColorKey(const AValue: String);
        procedure SetTextSettings(const AValue: TBaseStateStyle.TTextSettings);
        procedure SetLabelTextSettings(const AValue: TBaseStateStyle.TLabelTextSettings);
        procedure SetSupportingTextSettings(const AValue: TBaseStateStyle.TSupportingTextSettings);
        procedure TextSettingsChanged(ASender: TObject);
        procedure LabelTextSettingsChanged(ASender: TObject);
        procedure SupportingTextSettingsChanged(ASender: TObject);
        function IsPromptTextColorStored: Boolean;
        function IsPromptTextColorKeyStored: Boolean;
        function IsTintColorStored: Boolean;
        function IsTintColorKeyStored: Boolean;
      protected
        function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
        function CreateTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TTextSettings; virtual;
        function CreateLabelTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TLabelTextSettings; virtual;
        function CreateSupportingTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TSupportingTextSettings; virtual;
        procedure ApplyPromptTextColorScheme; virtual;
        procedure ApplyTintColorScheme; virtual;
        function GetDefaultPromptTextColor: TalphaColor; virtual;
        function GetDefaultPromptTextColorKey: String; virtual;
        function GetDefaultTintColor: TalphaColor; virtual;
        function GetDefaultTintColorKey: String; virtual;
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
        procedure ApplyColorScheme; override;
        procedure Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single; const AReverse: Boolean); override;
        property StateStyleParent: TBaseStateStyle read GetStateStyleParent;
        property ControlParent: TALBaseEdit read GetControlParent;
        property DefaultPromptTextColor: TalphaColor read GetDefaultPromptTextColor;
        property DefaultPromptTextColorKey: String read GetDefaultPromptTextColorKey;
        property DefaultTintColor: TalphaColor read GetDefaultTintColor;
        property DefaultTintColorKey: String read GetDefaultTintColorKey;
      published
        property LabelTextSettings: TBaseStateStyle.TLabelTextSettings read fLabelTextSettings write SetLabelTextSettings;
        property PromptTextColor: TAlphaColor read FPromptTextColor write SetPromptTextColor stored IsPromptTextColorStored;
        property PromptTextColorKey: String read FPromptTextColorKey write SetPromptTextColorKey stored IsPromptTextColorKeyStored;
        property SupportingTextSettings: TBaseStateStyle.TSupportingTextSettings read fSupportingTextSettings write SetSupportingTextSettings;
        property TextSettings: TBaseStateStyle.TTextSettings read fTextSettings write SetTextSettings;
        property TintColor: TAlphaColor read FTintColor write SetTintColor stored IsTintColorStored;
        property TintColorKey: String read FTintColorKey write SetTintColorKey stored IsTintColorKeyStored;
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
      public
        type
          // -----------
          // TTransition
          TTransition = class(TALBaseStateStyles.TTransition)
          public
            procedure Start; override;
          protected
            procedure DoProcess; override;
            procedure DoFinish; override;
          end;
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
        function CreateTransition: TALBaseStateStyles.TTransition; override;
        function CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle; virtual;
        function CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle; virtual;
        function CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle; virtual;
      public
        constructor Create(const AParent: TALControl); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure ApplyColorScheme; override;
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
    FAutoTranslate: Boolean;
    fOnChange: TNotifyEvent;
    fOnReturnKey: TNotifyEvent;
    FTextSettings: TTextSettings;
    FPromptText: String;
    FPromptTextColor: TAlphaColor;
    FPromptTextColorKey: String;
    FTintcolor: TAlphaColor;
    FTintcolorKey: String;
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
    //--
    FDummyFillColor: TAlphaColor;
    fDummyReturnKeyType: TReturnKeyType;
    fDummyKeyboardType: TVirtualKeyboardType;
    fDummyAutoCapitalizationType: TALAutoCapitalizationType;
    fDummyCheckSpelling: boolean;
    FDummyPassword: Boolean;
    FDummyMaxLength: Integer;
    FDummyText: String;
    //--
    fBufPromptTextDrawable: TALDrawable;
    fBufPromptTextDrawableRect: TRectF;
    fBufLabelTextDrawable: TALDrawable;
    fBufLabelTextDrawableRect: TRectF;
    fBufSupportingTextDrawable: TALDrawable;
    fBufSupportingTextDrawableRect: TRectF;
    //--
    procedure UpdateEditControlPromptText;
    function ShouldHideNativeView: Boolean;
    procedure UpdateNativeViewVisibility;
    function GetPromptText: String;
    procedure setPromptText(const Value: String);
    function GetPromptTextColor: TAlphaColor;
    function GetPromptTextColorKey: String;
    procedure setPromptTextColor(const Value: TAlphaColor);
    procedure setPromptTextColorKey(const Value: String);
    procedure setLabelText(const Value: String);
    procedure setSupportingText(const Value: String);
    function GetTintColor: TAlphaColor;
    function GetTintColorKey: String;
    procedure setTintColor(const Value: TAlphaColor);
    procedure setTintColorKey(const Value: String);
    procedure SetTextSettings(const Value: TTextSettings);
    procedure SetLabelTextSettings(const Value: TLabelTextSettings);
    procedure SetSupportingTextSettings(const Value: TSupportingTextSettings);
    procedure SetStateStyles(const AValue: TStateStyles);
    function getText: String;
    procedure SetText(const Value: String);
    function GetIsTextEmpty: Boolean; inline;
    procedure setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
    function GetAutoCapitalizationType: TALAutoCapitalizationType;
    procedure SetPassword(const Value: Boolean);
    function GetPassword: Boolean;
    procedure SetCheckSpelling(const Value: Boolean);
    function GetCheckSpelling: Boolean;
    procedure SetMaxLength(const Value: integer);
    function GetMaxLength: integer;
    procedure LabelTextAnimationProcess(Sender: TObject);
    procedure LabelTextAnimationFinish(Sender: TObject);
    function HasOpacityLabelTextAnimation: Boolean;
    function HasTranslationLabelTextAnimation: Boolean;
    procedure UpdateNativeViewStyle;
    {$IF defined(android)}
    function GetNativeView: TALAndroidEditView;
    {$ELSEIF defined(IOS)}
    function GetNativeView: TALIosBaseEditView;
    {$ELSEIF defined(ALMacOS)}
    function GetNativeView: TALMacBaseEditView;
    {$ELSEIF defined(MSWindows)}
    function GetNativeView: TALWinEditView;
    {$ENDIF}
    { IVirtualKeyboardControl }
    procedure SetKeyboardType(Value: TVirtualKeyboardType);
    function GetKeyboardType: TVirtualKeyboardType;
    procedure SetReturnKeyType(Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
    function IVirtualKeyboardControl.IsPassword = GetPassword;
  protected
    {$IF defined(android)}
    Function CreateNativeView: TALAndroidNativeView; override;
    {$ELSEIF defined(IOS)}
    Function CreateNativeView: TALIosNativeView; override;
    {$ELSEIF defined(ALMacOS)}
    Function CreateNativeView: TALMacNativeView; override;
    {$ELSEIF defined(MSWindows)}
    Function CreateNativeView: TALWinNativeView; override;
    {$ENDIF}
    Procedure ShowNativeView; override;
    Procedure HideNativeView; override;
    function CreateFill: TALBrush; override;
    function CreateStroke: TALStrokeBrush; override;
    function CreateTextSettings: TTextSettings; virtual;
    function CreateLabelTextSettings: TLabelTextSettings; virtual;
    function CreateSupportingTextSettings: TSupportingTextSettings; virtual;
    function CreateStateStyles: TStateStyles; virtual;
    procedure ApplyPromptTextColorScheme; virtual;
    procedure ApplyTintColorScheme; virtual;
    procedure InitNativeView; override;
    procedure RecreateNativeView; override;
    procedure DoChange; virtual;
    procedure DoReturnKey; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure IsMouseOverChanged; override;
    function GetDefaultSize: TSizeF; override;
    procedure Loaded; override;
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
    procedure Assign(Source: TPersistent); override;
    procedure AlignToPixel; override;
    procedure ApplyColorScheme; override;
    {$IF defined(android)}
    property NativeView: TALAndroidEditView read GetNativeView;
    {$ELSEIF defined(IOS)}
    property NativeView: TALIosBaseEditView read GetNativeView;
    {$ELSEIF defined(ALMacOS)}
    property NativeView: TALMacBaseEditView read GetNativeView;
    {$ELSEIF defined(MSWindows)}
    property NativeView: TALWinEditView read GetNativeView;
    {$ENDIF}
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
    property DefStyleAttr;
    // Android only - the name of a style resource that supplies default style values
    // NOTE: !!IMPORTANT!! This properties must be defined the very first because the stream system must load it the very first
    property DefStyleRes;
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
    property ClickSound;
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
    /// <summary>
    ///   Null mean use the default PromptTextColor
    /// </summary>
    property PromptTextColor: TAlphaColor read GetPromptTextColor write setPromptTextColor default TalphaColors.null;
    property PromptTextColorKey: String read GetPromptTextColorKey write setPromptTextColorKey;
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
    /// <summary>
    ///   IOS only - the color of the cursor caret and the text
    ///   selection handles. null mean use the default TintColor
    /// </summary>
    property TintColor: TAlphaColor read GetTintColor write setTintColor default TalphaColors.null;
    property TintColorKey: String read GetTintColorKey write setTintColorKey;
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
    property OnDblClick;
    //property OnKeyDown; // Not work under android - it's like this with their @{[^# virtual keyboard :(
    //property OnKeyUp; // Not work under android - it's like this with their @{[^# virtual keyboard :(
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
    property OnReturnKey: TNotifyEvent read fOnReturnKey write fOnReturnKey;
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
    function HasUnconstrainedAutosizeWidth: Boolean; override;
  published
    property AutoSize default TALAutoSizeMode.Both;
    property Password;
  end;

  {***************************}
  TALDummyEdit = class(TALEdit)
  protected
    {$IF defined(android)}
    Function CreateNativeView: TALAndroidNativeView; override;
    {$ELSEIF defined(IOS)}
    Function CreateNativeView: TALIosNativeView; override;
    {$ELSEIF defined(ALMacOS)}
    Function CreateNativeView: TALMacNativeView; override;
    {$ELSEIF defined(MSWindows)}
    Function CreateNativeView: TALWinNativeView; override;
    {$ENDIF}
  end;

procedure Register;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  FMX.Utils,
  FMX.Types3D,
  Fmx.controls,
  {$IF defined(ALBackwardCompatible)}
  System.UIConsts,
  {$endif}
  {$IF defined(android)}
  Androidapi.Helpers,
  Androidapi.Input,
  Androidapi.KeyCodes,
  Androidapi.JNI.App,
  Androidapi.JNI.Util,
  Androidapi.JNI.Os,
  FMX.Platform.Android,
  FMX.Platform.UI.Android,
  {$ELSEIF defined(IOS)}
  Macapi.CoreFoundation,
  iOSapi.CocoaTypes,
  Macapi.Helpers,
  iOSapi.CoreText,
  FMX.Helpers.iOS,
  Alcinoe.iOSapi.Foundation,
  {$ELSEIF defined(ALMacOS)}
  Macapi.ObjCRuntime,
  Macapi.CoreFoundation,
  Macapi.Helpers,
  FMX.Helpers.Mac,
  Alcinoe.Macapi.Foundation,
  Alcinoe.StringUtils,
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
  Alcinoe.FMX.BreakText,
  Alcinoe.FMX.Styles,
  Alcinoe.FMX.Graphics,
  Alcinoe.Common;

{**}
Type
  _TALBaseStateStyleProtectedAccess = class(TALBaseStateStyle);

{$REGION ' ANDROID'}
{$IF defined(android)}

{********************************************************************************************}
constructor TALAndroidEditView.TKeyPreImeListener.Create(const aEditView: TALAndroidEditView);
begin
  inherited Create;
  FEditView := aEditView;
end;

{******************************************************************************************************}
function TALAndroidEditView.TKeyPreImeListener.onKeyPreIme(keyCode: Integer; event: JKeyEvent): Boolean;
begin
  {$IF defined(DEBUG)}
  //if event <> nil then
  //  ALLog(
  //    Classname + '.onKeyPreIme',
  //    'control.name: ' + FEditText.Control.Name + ' | ' +
  //    'keyCode: ' + inttostr(keyCode) + ' | ' +
  //    'event: ' + JstringToString(event.toString))
  //else
  //  ALLog(
  //    Classname + '.onKeyPreIme',
  //    'control.name: ' + FEditText.Control.Name + ' | ' +
  //    'keyCode: ' + inttostr(keyCode));
  {$ENDIF}
  if ((event = nil) or (event.getAction = AKEY_EVENT_ACTION_UP)) and
     (keyCode = AKEYCODE_BACK) then begin

    result := true;
    FEditView.control.resetfocus;

  end
  else result := false;
end;

{****************************************************************************************}
constructor TALAndroidEditView.TTouchListener.Create(const aEditView: TALAndroidEditView);
begin
  inherited Create;
  FEditView := aEditView;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.Presentation.Android.TAndroidNativeView.ProcessTouch was not updated and adjust the IFDEF'}
{$ENDIF}
function TALAndroidEditView.TTouchListener.onTouch(v: JView; event: JMotionEvent): Boolean;
begin
  if (FEditView.Form <> nil) and
     (not FEditView.view.hasFocus) and
     ((not FEditView.fIsMultiline) or
      (FEditView.getLineCount < FEditView.Control.GetNativeViewHeight / FEditView.getLineHeight)) then begin

    var LHandle: TAndroidWindowHandle;
    if FEditView.Form.IsHandleAllocated then
      LHandle := WindowHandleToPlatform(FEditView.Form.Handle)
    else
      LHandle := nil;

    if LHandle <> nil then
      LHandle.CurrentMotionEvent := event;

    var LTouchPoint := TPointF.Create(event.getRawX / ALGetScreenScale, event.getRawY / ALGetScreenScale);
    LTouchPoint := FEditView.Form.ScreenToClient(LTouchPoint);
    var LEventAction := event.getAction;
    if LEventAction = TJMotionEvent.JavaClass.ACTION_DOWN then begin
      FEditView.Form.MouseMove([ssTouch], LTouchPoint.X, LTouchPoint.Y);
      FEditView.Form.MouseMove([], LTouchPoint.X, LTouchPoint.Y); // Require for correct IsMouseOver handle
      FEditView.Form.MouseDown(TMouseButton.mbLeft, [ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y);
    end
    else if LEventAction = TJMotionEvent.JavaClass.ACTION_MOVE then begin
      FEditView.Form.MouseMove([ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y)
    end
    else if LEventAction = TJMotionEvent.JavaClass.ACTION_CANCEL then begin
      FEditView.Form.MouseUp(TMouseButton.mbLeft, [ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y, False);
      FEditView.Form.MouseLeave;
    end
    else if LEventAction = TJMotionEvent.JavaClass.ACTION_UP then
    begin
      FEditView.Form.MouseUp(TMouseButton.mbLeft, [ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y);
      FEditView.Form.MouseLeave;
    end;
    Result := True;

    if LHandle <> nil then
      LHandle.CurrentMotionEvent := nil;

  end
  else begin
    // In Android, within a single-line EditText, it's possible for the text line to shift slightly vertically
    // (as if the text is taller than the line height). To prevent this minor scrolling, we consume the touch event.
    if (not FEditView.fIsMultiline) and (FEditView.view.hasFocus) and (event.getAction() = TJMotionEvent.JavaClass.ACTION_MOVE) then result := true
    else result := false;
  end
end;

{**************************************************************************************}
constructor TALAndroidEditView.TTextWatcher.Create(const aEditView: TALAndroidEditView);
begin
  inherited Create;
  FEditView := aEditView;
end;

{***********************************************************************}
procedure TALAndroidEditView.TTextWatcher.afterTextChanged(s: JEditable);
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.afterTextChanged', 'control.name: ' + FEditText.Control.Name);
  {$ENDIF}
  FEditView.Control.DoChange;
end;

{****************************************************************************************************************************}
procedure TALAndroidEditView.TTextWatcher.beforeTextChanged(s: JCharSequence; start: Integer; count: Integer; after: Integer);
begin
  // Nothing to do
end;

{*************************************************************************************************************************}
procedure TALAndroidEditView.TTextWatcher.onTextChanged(s: JCharSequence; start: Integer; before: Integer; count: Integer);
begin
  // Nothing to do
end;

{********************************************************************************************************************************************}
constructor TALAndroidEditView.TEditorActionListener.Create(const aEditView: TALAndroidEditView; const aIsMultiLineEditText: Boolean = false);
begin
  inherited Create;
  fIsMultiLineEditText := aIsMultiLineEditText;
  FEditView := aEditView;
end;

{***************************************************************************************************************************}
function TALAndroidEditView.TEditorActionListener.onEditorAction(v: JTextView; actionId: Integer; event: JKeyEvent): Boolean;
begin
  {$IF defined(DEBUG)}
  //if event <> nil then
  //  ALLog(
  //    Classname + '.onEditorAction',
  //    'control.name: ' + FEditText.Control.Name + ' | ' +
  //    'actionId: ' + inttostr(actionId) + ' | ' +
  //    'event: ' + JstringToString(event.toString))
  //else
  //  ALLog(
  //    Classname + '.onEditorAction',
  //    'control.name: ' + FEditText.Control.Name + ' | ' +
  //    'actionId: ' + inttostr(actionId));
  {$ENDIF}
  //IME_ACTION_DONE: the action key performs a "done" operation, typically meaning there is nothing more to input and the IME will be closed.
  //IME_ACTION_GO: the action key performs a "go" operation to take the user to the target of the text they typed. Typically used, for example, when entering a URL.
  //IME_ACTION_NEXT: the action key performs a "next" operation, taking the user to the next field that will accept text.
  //IME_ACTION_NONE: there is no available action.
  //IME_ACTION_PREVIOUS: like IME_ACTION_NEXT, but for moving to the previous field. This will normally not be used to specify an action (since it precludes IME_ACTION_NEXT), but can be returned to the app if it sets IME_FLAG_NAVIGATE_PREVIOUS.
  //IME_ACTION_SEARCH: the action key performs a "search" operation, taking the user to the results of searching for the text they have typed (in whatever context is appropriate).
  //IME_ACTION_SEND: the action key performs a "send" operation, delivering the text to its target. This is typically used when composing a message in IM or SMS where sending is immediate.
  //IME_ACTION_UNSPECIFIED: no specific action has been associated with this editor, let the editor come up with its own if it can.
  if (assigned(FEditView.Control.OnReturnKey)) and
     (((actionId = TJEditorInfo.javaClass.IME_ACTION_UNSPECIFIED) and // IME_ACTION_UNSPECIFIED = the return key
       (not fIsMultiLineEditText)) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_DONE) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_GO) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_NEXT) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_SEARCH) or
      (actionId = TJEditorInfo.javaClass.IME_ACTION_SEND)) then begin

    result := true;
    FEditView.Control.DoReturnKey;

  end
  else result := false;
end;

{************************************************************************************************************************************************************************}
constructor TALAndroidEditView.Create(const AControl: TALControl; Const aIsMultiline: Boolean = False; const aDefStyleAttr: String = ''; const aDefStyleRes: String = '');
begin
  fDefStyleAttr := aDefStyleAttr;
  fDefStyleRes := aDefStyleRes;
  inherited create(AControl); // This will call CreateView
  fIsMultiline := aIsMultiline;
  FTextSettings := TALBaseEdit.TTextSettings.Create;
  FTextSettings.OnChanged := TextSettingsChanged;
  FFillColor := $ffffffff;
  fMaxLength := 0;
  fReturnKeyType := tReturnKeyType.Default;
  fKeyboardType := TVirtualKeyboardType.default;
  fAutoCapitalizationType := TALAutoCapitalizationType.acNone;
  fPassword := false;
  fCheckSpelling := true;
  FTintColor := TalphaColors.Null;
  //--
  // We must call setSingleLine before doing View.addTextChangedListener
  // else it's will fire TALAndroidEditView.TTextWatcher.afterTextChanged
  if fIsMultiline then View.setSingleLine(False)
  else View.setSingleLine(True);
  //--
  view.setBackgroundColor(TJColor.JavaClass.TRANSPARENT);
  view.setBackground(nil);
  view.setPadding(0, 0, 0, 0);
  //--
  DoSetReturnKeyType(fReturnKeyType);
  DoSetInputType(fKeyboardType, fAutoCapitalizationType, fPassword, fCheckSpelling, fIsMultiline);
  //--
  FTextWatcher := TTextWatcher.Create(self);
  View.addTextChangedListener(fTextWatcher);
  FEditorActionListener := TEditorActionListener.Create(self, aIsMultiline);
  View.setOnEditorActionListener(fEditorActionListener);
  FKeyPreImeListener := TKeyPreImeListener.Create(self);
  View.SetKeyPreImeListener(fKeyPreImeListener);
  FTouchListener := TTouchListener.Create(self);
  View.SetOnTouchListener(fTouchListener);
end;

{************************************}
destructor TALAndroidEditView.Destroy;
begin

  View.RemoveTextChangedListener(FTextWatcher);
  View.setOnEditorActionListener(nil);
  View.SetKeyPreImeListener(nil);
  View.SetOnTouchListener(nil);

  alfreeandNil(FTextWatcher);
  alfreeandNil(fEditorActionListener);
  alfreeandNil(FKeyPreImeListener);
  alfreeandNil(FTouchListener);

  ALFreeAndNil(FTextSettings);
  inherited;

end;

{********************************************}
function TALAndroidEditView.CreateView: JView;
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

{***********************************************}
function TALAndroidEditView.GetView: JALEditText;
begin
  Result := inherited GetView<JALEditText>;
end;

{**************************************************}
function TALAndroidEditView.GetControl: TALBaseEdit;
begin
  Result := TALBaseEdit(inherited Control);
end;

{************************************************}
function TALAndroidEditView.getLineCount: integer;
begin
  if FIsMultiline then result := view.getLineCount
  else Result := 1;
end;

{******************************************}
procedure TALAndroidEditView.DoSetInputType(
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

  view.setInputType(LInputType);

end;

{****************************************************************}
function TALAndroidEditView.GetKeyboardType: TVirtualKeyboardType;
begin
  Result := fKeyboardType;
end;

{******************************************************************************}
procedure TALAndroidEditView.setKeyboardType(const Value: TVirtualKeyboardType);
begin
  if (value <> fKeyboardType) then begin
    fKeyboardType := Value;
    DoSetInputType(Value, fAutoCapitalizationType, fPassword, fCheckSpelling, fIsMultiLine);
  end;
end;

{*******************************************************************************}
function TALAndroidEditView.GetAutoCapitalizationType: TALAutoCapitalizationType;
begin
  result := fAutoCapitalizationType;
end;

{*********************************************************************************************}
procedure TALAndroidEditView.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  if (value <> fAutoCapitalizationType) then begin
    fAutoCapitalizationType := Value;
    DoSetInputType(fKeyboardType, Value, fPassword, fCheckSpelling, fIsMultiLine);
  end;
end;

{***********************************************}
function TALAndroidEditView.GetPassword: Boolean;
begin
  Result := fPassword;
end;

{*************************************************************}
procedure TALAndroidEditView.setPassword(const Value: Boolean);
begin
  if (value <> fPassword) then begin
    fPassword := Value;
    DoSetInputType(fKeyboardType, fAutoCapitalizationType, Value, fCheckSpelling, fIsMultiLine);
  end;
end;

{****************************************************}
function TALAndroidEditView.GetCheckSpelling: Boolean;
begin
  result := fCheckSpelling;
end;

{******************************************************************}
procedure TALAndroidEditView.SetCheckSpelling(const Value: Boolean);
begin
  if (value <> fCheckSpelling) then begin
    fCheckSpelling := Value;
    DoSetInputType(fKeyboardType, fAutoCapitalizationType, fPassword, Value, fIsMultiLine);
  end;
end;

{************************************************************************************}
procedure TALAndroidEditView.DoSetReturnKeyType(const aReturnKeyType: TReturnKeyType);
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
  view.setImeOptions(LImeOptions);
end;

{***********************************************************}
function TALAndroidEditView.GetReturnKeyType: TReturnKeyType;
begin
  Result := fReturnKeyType;
end;

{*************************************************************************}
procedure TALAndroidEditView.setReturnKeyType(const Value: TReturnKeyType);
begin
  if (value <> fReturnKeyType) then begin
    fReturnKeyType := Value;
    DoSetReturnKeyType(Value);
  end;
end;

{************************************************}
function TALAndroidEditView.GetMaxLength: integer;
begin
  Result := FMaxLength;
end;

{**************************************************************}
procedure TALAndroidEditView.SetMaxLength(const Value: integer);
begin
  if value <> FMaxLength then begin
    FMaxLength := value;
    View.setMaxLength(Value);
  end;
end;

{************************************************}
function TALAndroidEditView.GetPromptText: String;
begin
  result := JCharSequenceToStr(View.getHint);
end;

{**************************************************************}
procedure TALAndroidEditView.setPromptText(const Value: String);
begin
  View.setHint(StrToJCharSequence(Value));
end;

{**********************************************************}
function TALAndroidEditView.GetPromptTextColor: TAlphaColor;
begin
  result := TAlphaColor(View.getCurrentHintTextColor);
end;

{************************************************************************}
procedure TALAndroidEditView.setPromptTextColor(const Value: TAlphaColor);
begin
  if Value <> TalphaColors.null then
    View.setHintTextColor(integer(Value));
end;

{******************************************}
function TALAndroidEditView.getText: String;
begin
  result := JCharSequenceToStr(View.gettext);
end;

{********************************************************}
procedure TALAndroidEditView.SetText(const Value: String);
begin
  View.setText(StrToJCharSequence(Value), TJTextView_BufferType.javaClass.EDITABLE);
  if Value <> '' then setSelection(length(Value));
end;

{*****************************************************************************}
procedure TALAndroidEditView.SetTextSettings(const Value: TALBaseTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{****************************************************************}
procedure TALAndroidEditView.TextSettingsChanged(Sender: TObject);
begin

  var LFontColor := integer(textsettings.font.color);
  var LFontSize: single := textsettings.font.size;

  var LFontStyles: TFontStyles := [];
  if textsettings.font.Weight in [TFontWeight.Bold,
                                  TFontWeight.UltraBold,
                                  TFontWeight.Black,
                                  TFontWeight.UltraBlack] then LFontStyles := LFontStyles + [TFontStyle.fsBold];
  if textsettings.font.Slant in [TFontSlant.Italic, TFontSlant.Oblique] then LFontStyles := LFontStyles + [TFontStyle.fsItalic];
  var LFontFamily := ALResolveFontFamily(ALExtractPrimaryFontFamily(textsettings.font.Family));
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
  view.setTextColor(LFontColor); // Sets the text color for all the states (normal, selected, focused) to be this color.
  view.setTextSize(TJTypedValue.javaclass.COMPLEX_UNIT_DIP, LFontSize); // Set the default text size to a given unit and value.
  //-----
  var LTypeface := TJTypeface.JavaClass.create(StringToJString(LFontFamily), ALfontStyleToAndroidStyle(LFontStyles));
  // Note that not all Typeface families actually have bold and italic variants, so you may
  // need to use setTypeface(Typeface, int) to get the appearance that you actually want.
  view.setTypeface(LTypeface);
  LTypeface := nil;
  view.setgravity(LGravity);
  //-----
  if not SameValue(textsettings.LineHeightMultiplier, 0, TEpsilon.Scale) then
    view.setLineSpacing(0{add}, textsettings.LineHeightMultiplier{mult});

end;

{****************************************************}
function TALAndroidEditView.GetTintColor: TAlphaColor;
begin
  Result := FTintColor;
end;

{******************************************************************}
procedure TALAndroidEditView.setTintColor(const Value: TAlphaColor);
begin
  FTintColor := Value;
end;

{****************************************************}
function TALAndroidEditView.GetFillColor: TAlphaColor;
begin
  Result := FFillColor;
end;

{******************************************************************}
procedure TALAndroidEditView.SetFillColor(const Value: TAlphaColor);
begin
  FFillColor := Value;
end;

{************************************************}
function TALAndroidEditView.getLineHeight: Single;
begin
  result := view.getLineHeight / ALGetScreenScale;
end;

{*************************************************************************************}
Procedure TALAndroidEditView.setSelection(const AStart: integer; const AStop: Integer);
begin
  View.setSelection(aStart, aStop);
end;

{***************************************************************}
Procedure TALAndroidEditView.setSelection(const AIndex: integer);
begin
  view.setSelection(aindex);
end;

{$endif}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}

{************************************************************************************}
constructor TALIosEditView.TTextFieldDelegate.Create(const AEditView: TALIosEditView);
begin
  inherited Create;
  FEditView := AEditView;
end;

{*********************************************************************************************************************************************************}
function TALIosEditView.TTextFieldDelegate.textField(textField: UITextField; shouldChangeCharactersInRange: NSRange; replacementString: NSString): Boolean;
begin
  {$IF defined(DEBUG)}
  //ALLog(
  //  Classname + '.textField',
  //  'control.name: ' + FTextField.Control.Name + ' | ' +
  //  'replacementString: ' + NSStrToStr(replacementString));
  {$ENDIF}
  if FEditView.maxLength > 0 then begin
    var LText: NSString := TNSString.Wrap(textField.text);
    if shouldChangeCharactersInRange.length + shouldChangeCharactersInRange.location > LText.length then exit(false);
    result := LText.length + replacementString.length - shouldChangeCharactersInRange.length <= NSUInteger(FEditView.maxLength);
  end
  else Result := True;
end;

{*******************************************************************************************}
procedure TALIosEditView.TTextFieldDelegate.textFieldDidBeginEditing(textField: UITextField);
begin
end;

{*****************************************************************************************}
procedure TALIosEditView.TTextFieldDelegate.textFieldDidEndEditing(textField: UITextField);
begin
  FEditView.Control.ResetFocus;
end;

{******************************************************************************************************}
function TALIosEditView.TTextFieldDelegate.textFieldShouldBeginEditing(textField: UITextField): Boolean;
begin
  Result := True;
end;

{***********************************************************************************************}
function TALIosEditView.TTextFieldDelegate.textFieldShouldClear(textField: UITextField): Boolean;
begin
  Result := true;
end;

{****************************************************************************************************}
function TALIosEditView.TTextFieldDelegate.textFieldShouldEndEditing(textField: UITextField): Boolean;
begin
  Result := True;
end;

{************************************************************************************************}
function TALIosEditView.TTextFieldDelegate.textFieldShouldReturn(textField: UITextField): Boolean;
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.textFieldShouldReturn', 'control.name: ' + FTextField.Control.Name);
  {$ENDIF}
  if assigned(FEditView.Control.OnReturnKey) then begin
    FEditView.Control.DoReturnKey;
    result := false;
  end
  else Result := true; // return YES if the text field should implement its default behavior for the return button; otherwise, NO.
end;

{********************************}
constructor TALIosEditView.Create;
begin
  inherited; // This will call InitView
  FTextFieldDelegate := TTextFieldDelegate.Create(Self);
  View.setDelegate(FTextFieldDelegate.GetObjectID);
  FTextSettings := TALBaseEdit.TTextSettings.Create;
  FTextSettings.OnChanged := TextSettingsChanged;
  FFillColor := $ffffffff;
  fMaxLength := 0;
  fPromptTextColor := TalphaColors.Null;
  SetReturnKeyType(tReturnKeyType.Default);
  SetKeyboardType(TVirtualKeyboardType.default);
  setAutoCapitalizationType(TALAutoCapitalizationType.acNone);
  SetPassword(false);
  SetCheckSpelling(True);
  View.setBorderStyle(UITextBorderStyleNone);
  View.addTarget(GetObjectID, sel_getUid(MarshaledAString(TMarshal.AsAnsi('ControlEventEditingChanged'))), UIControlEventEditingChanged);
end;

{********************************}
destructor TALIosEditView.Destroy;
begin
  View.removeTarget(GetObjectID, sel_getUid(MarshaledAString(TMarshal.AsAnsi('ControlEventEditingChanged'))), UIControlEventEditingChanged);
  View.setDelegate(nil);
  ALFreeAndNil(FTextFieldDelegate);
  ALFreeAndNil(FTextSettings);
  inherited;
end;


{********************************************************}
procedure TALIosEditView.SetEnabled(const value: Boolean);
begin
  inherited;
  View.SetEnabled(value);
end;

{**************************************************}
procedure TALIosEditView.ControlEventEditingChanged;
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.ControlEventEditingChanged', 'control.name: ' + Control.Name);
  {$ENDIF}
  DoChange;
end;

{****************************************************}
function TALIosEditView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALIosEditView);
end;

{*******************************************}
function TALIosEditView.GetView: UITextField;
begin
  Result := inherited GetView<UITextField>;
end;

{**********************************************}
function TALIosEditView.GetControl: TALBaseEdit;
begin
  Result := TALBaseEdit(inherited Control);
end;

{********************************************}
function TALIosEditView.getLineCount: integer;
begin
  Result := 1;
end;

{**************************************************************************}
procedure TALIosEditView.SetKeyboardType(const Value: TVirtualKeyboardType);
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
  View.setKeyboardType(LUIKeyboardType);
end;

{************************************************************}
function TALIosEditView.GetKeyboardType: TVirtualKeyboardType;
begin
  var LUIKeyboardType := View.KeyboardType;
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

{*****************************************************************************************}
procedure TALIosEditView.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  var LUITextAutoCapitalizationType: UITextAutoCapitalizationType;
  case Value of
    TALAutoCapitalizationType.acWords:          LUITextAutoCapitalizationType := UITextAutoCapitalizationTypeWords;
    TALAutoCapitalizationType.acSentences:      LUITextAutoCapitalizationType := UITextAutoCapitalizationTypeSentences;
    TALAutoCapitalizationType.acAllCharacters:  LUITextAutoCapitalizationType := UITextAutoCapitalizationTypeAllCharacters;
    else {TALAutoCapitalizationType.acNone}     LUITextAutoCapitalizationType := UITextAutoCapitalizationTypeNone;
  end;
  View.setAutoCapitalizationType(LUITextAutoCapitalizationType);
end;

{***************************************************************************}
function TALIosEditView.GetAutoCapitalizationType: TALAutoCapitalizationType;
begin
  var LUITextAutoCapitalizationType := View.AutoCapitalizationType;
  case LUITextAutoCapitalizationType of
    UITextAutoCapitalizationTypeWords:         result := TALAutoCapitalizationType.acWords;
    UITextAutoCapitalizationTypeSentences:     result := TALAutoCapitalizationType.acSentences;
    UITextAutoCapitalizationTypeAllCharacters: result := TALAutoCapitalizationType.acAllCharacters;
    else                                       result := TALAutoCapitalizationType.acNone;
  end;
end;

{*********************************************************}
procedure TALIosEditView.SetPassword(const Value: Boolean);
begin
  View.setSecureTextEntry(Value);
end;

{*******************************************}
function TALIosEditView.GetPassword: Boolean;
begin
  result := View.isSecureTextEntry;
end;

{**************************************************************}
procedure TALIosEditView.SetCheckSpelling(const Value: Boolean);
begin
  if Value then begin
    View.setSpellCheckingType(UITextSpellCheckingTypeYes);
    View.setAutocorrectionType(UITextAutocorrectionTypeDefault);
  end
  else begin
    View.setSpellCheckingType(UITextSpellCheckingTypeNo);
    View.setAutocorrectionType(UITextAutocorrectionTypeNo);
  end;
end;

{************************************************}
function TALIosEditView.GetCheckSpelling: Boolean;
begin
  result := View.SpellCheckingType = UITextSpellCheckingTypeYes;
end;

{*********************************************************************}
procedure TALIosEditView.setReturnKeyType(const Value: TReturnKeyType);
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
  View.setReturnKeyType(LUIReturnKeyType);
end;

{*******************************************************}
function TALIosEditView.GetReturnKeyType: TReturnKeyType;
begin
  var LUIReturnKeyType := View.ReturnKeyType;
  case LUIReturnKeyType of
    UIReturnKeyDone:    result := TReturnKeyType.Done;
    UIReturnKeyGo:      result := TReturnKeyType.Go;
    UIReturnKeyNext:    result := TReturnKeyType.Next;
    UIReturnKeySearch:  result := TReturnKeyType.Search;
    UIReturnKeySend:    result := TReturnKeyType.Send;
    else                result := TReturnKeyType.Default;
  end;
end;

{********************************************}
function TALIosEditView.GetPromptText: String;
begin
  var LAttributedString := TALNSAttributedString.wrap(NSObjectToID(TUITextField.Wrap(NSObjectToID(View)).AttributedPlaceholder));
  if LAttributedString = nil then Result := NSStrToStr(View.placeholder)
  else result := NSStrToStr(LAttributedString.&String);
end;

{**********************************************************}
procedure TALIosEditView.setPromptText(const Value: String);
begin
  applyPromptTextWithColor(Value, fPromptTextColor);
end;

{******************************************************}
function TALIosEditView.GetPromptTextColor: TAlphaColor;
begin
  Result := fPromptTextColor;
end;

{********************************************************************}
procedure TALIosEditView.setPromptTextColor(const Value: TAlphaColor);
begin
  if Value <> fPromptTextColor then begin
    fPromptTextColor := Value;
    applyPromptTextWithColor(GetPromptText, fPromptTextColor);
  end;
end;

{***********************************************************************************************}
procedure TALIosEditView.applyPromptTextWithColor(const aStr: String; const aColor: TAlphaColor);
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

    View.setAttributedPlaceholder(LPromptTextAttr);

  finally
    LPromptTextAttr.release;
  end;
end;

{************************************************}
function TALIosEditView.GetTintColor: TAlphaColor;
begin
  var red: CGFloat;
  var green: CGFloat;
  var blue: CGFloat;
  var alpha: CGFloat;
  if not View.tintColor.getRed(@red, @green, @blue, @alpha) then result := TalphaColors.Null
  else result := TAlphaColorF.Create(red, green, blue, alpha).ToAlphaColor;
end;

{**************************************************************}
procedure TALIosEditView.setTintColor(const Value: TAlphaColor);
begin
  if Value <> TalphaColors.Null then
    View.setTintColor(AlphaColorToUIColor(Value));
end;

{**************************************}
function TALIosEditView.getText: String;
begin
  result := NSStrToStr(TNSString.Wrap(View.text));
end;

{****************************************************}
procedure TALIosEditView.SetText(const Value: String);
begin
  if Value <> getText then begin
    View.setText(StrToNSStr(Value));
    DoChange;
  end;
end;

{********************************************}
function TALIosEditView.GetMaxLength: integer;
begin
  Result := FMaxLength;
end;

{**********************************************************}
procedure TALIosEditView.SetMaxLength(const Value: integer);
begin
  FMaxLength := Value;
end;

{***********************************************************}
function TALIosEditView.GetTextSettings: TALBaseTextSettings;
begin
  result := FTextSettings;
end;

{*************************************************************************}
procedure TALIosEditView.SetTextSettings(const Value: TALBaseTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{************************************************************}
procedure TALIosEditView.TextSettingsChanged(Sender: TObject);
begin
  // Font
  var LFontRef := ALCreateCTFontRef(ALResolveFontFamily(TextSettings.Font.Family), TextSettings.Font.Size, TextSettings.Font.Weight, TextSettings.Font.Slant);
  try
    var LDictionary := TNSDictionary.Wrap(
                         TNSDictionary.OCClass.dictionaryWithObject(
                           LFontRef,
                           NSObjectToID(TNSString.Wrap(kCTFontAttributeName))));
    // Setting this property applies the specified attributes to the entire
    // text of the text field. Unset attributes maintain their default values.
    // Note: I can't later call LDictionary.release or I have an error
    View.setdefaultTextAttributes(LDictionary);
  finally
    CFRelease(LFontRef);
  end;

  // TextAlignment
  View.setTextAlignment(ALTextHorzAlignToUITextAlignment(TextSettings.HorzAlign));

  // TextColor
  View.setTextColor(AlphaColorToUIColor(TextSettings.Font.Color));
end;

{********************************}
procedure TALIosEditView.DoChange;
begin
  Control.DoChange;
end;

{************************************************}
function TALIosEditView.GetFillColor: TAlphaColor;
begin
  Result := FFillColor;
end;

{**************************************************************}
procedure TALIosEditView.SetFillColor(const Value: TAlphaColor);
begin
  FFillColor := Value;
end;

{********************************************}
function TALIosEditView.getLineHeight: Single;
begin
  if View.font = nil then
    TextSettingsChanged(nil);
  result := View.font.lineHeight;
  if not SameValue(textsettings.LineHeightMultiplier, 0, TEpsilon.Scale) then
    result := result * textsettings.LineHeightMultiplier;
end;

{*********************************************************************************}
Procedure TALIosEditView.setSelection(const AStart: integer; const AStop: Integer);
begin
  var LStartPosition: UITextPosition := View.positionFromPosition(View.beginningOfDocument, AStart);
  var LEndPosition: UITextPosition := View.positionFromPosition(LStartPosition, AStop - AStart);
  View.setSelectedTextRange(View.textRangeFromPosition(LStartPosition, LEndPosition));
end;

{***********************************************************}
Procedure TALIosEditView.setSelection(const AIndex: integer);
begin
  var LStartPosition := View.positionFromPosition(View.beginningOfDocument, AIndex);
  var LEndPosition := View.positionFromPosition(LStartPosition, 0);
  View.setSelectedTextRange(View.textRangeFromPosition(LStartPosition, LEndPosition));
end;

{$endif}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}

{**************************************************************************************}
constructor TALMacEditView.TTextEditingDelegate.Create(const AEditView: TALMacEditView);
begin
  inherited Create;
  FEditView := AEditView;
end;

{********************************************************************************************}
procedure TALMacEditView.TTextEditingDelegate.controlTextDidBeginEditing(obj: NSNotification);
begin
end;

{******************************************************************************************}
procedure TALMacEditView.TTextEditingDelegate.controlTextDidEndEditing(obj: NSNotification);
begin
  FEditView.Control.ResetFocus;
end;

{**************************************************************************************}
procedure TALMacEditView.TTextEditingDelegate.controlTextDidChange(obj: NSNotification);
begin
  if FEditView.maxLength > 0 then begin
    var LText := NSStrToStr(FEditView.View.stringValue);
    if LText.length > FEditView.maxLength then begin
      FEditView.View.SetStringValue(StrToNSStr(ALCopyStr(LText,1,FEditView.maxLength)));
      exit;
    end;
  end;
  fEditView.DoChange;
end;

{**************************************************************************************************************************************}
function TALMacEditView.TTextEditingDelegate.controlTextShouldBeginEditing(control: NSControl; textShouldBeginEditing: NSText): Boolean;
begin
  Result := True;
end;

{**********************************************************************************************************************************}
function TALMacEditView.TTextEditingDelegate.controlTextShouldEndEditing(control: NSControl; textShouldEndEditing: NSText): Boolean;
begin
  Result := True;
end;

{***********************************************************************************************************************************************************}
function TALMacEditView.TTextEditingDelegate.controlTextViewDoCommandBySelector(control: NSControl; textView: NSTextView; doCommandBySelector: SEL): Boolean;
begin
  if assigned(fEditView.Control.OnReturnKey) and (sel_getName(doCommandBySelector) = 'insertNewline:') then begin
    fEditView.Control.DoReturnKey;
    Result := True;
  end
  else
    result := False;
end;

{********************************}
constructor TALMacEditView.Create;
begin
  inherited; // This will call InitView
  View.SetBezeled(False);
  View.setBordered(false);
  TALNSControl.Wrap(NSObjectToID(View)).setLineBreakMode(NSLineBreakByClipping);
  View.setDrawsBackground(false);
  View.setFocusRingType(NSFocusRingTypeNone);
  FTextFieldDelegate := TTextEditingDelegate.Create(Self);
  TALNSTextField.wrap(NSObjectToID(View)).setDelegate(FTextFieldDelegate.GetObjectID);
  FTextSettings := TALBaseEdit.TTextSettings.Create;
  FTextSettings.OnChanged := TextSettingsChanged;
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

{********************************}
destructor TALMacEditView.Destroy;
begin
  View.setDelegate(nil);
  ALFreeAndNil(FTextFieldDelegate);
  ALFreeAndNil(FTextSettings);
  inherited Destroy;
end;

{********************************************************}
procedure TALMacEditView.SetEnabled(const value: Boolean);
begin
  inherited;
  View.SetEnabled(value);
end;

{****************************************************}
function TALMacEditView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALMacEditView);
end;

{*******************************************}
function TALMacEditView.GetView: NSTextField;
begin
  Result := inherited GetView<NSTextField>;
end;

{**********************************************}
function TALMacEditView.GetControl: TALBaseEdit;
begin
  Result := TALBaseEdit(inherited Control);
end;

{********************************************}
function TALMacEditView.getLineCount: integer;
begin
  Result := 1;
end;

{************************************************************}
function TALMacEditView.GetKeyboardType: TVirtualKeyboardType;
begin
  Result := FKeyboardType;
end;

{**************************************************************************}
procedure TALMacEditView.setKeyboardType(const Value: TVirtualKeyboardType);
begin
  FKeyboardType := Value;
end;

{***************************************************************************}
function TALMacEditView.GetAutoCapitalizationType: TALAutoCapitalizationType;
begin
  Result := FAutoCapitalizationType;
end;

{*****************************************************************************************}
procedure TALMacEditView.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  FAutoCapitalizationType := Value;
end;

{*******************************************}
function TALMacEditView.GetPassword: Boolean;
begin
  Result := FPassword;
end;

{*********************************************************}
procedure TALMacEditView.setPassword(const Value: Boolean);
begin
  FPassword := Value;
end;

{************************************************}
function TALMacEditView.GetCheckSpelling: Boolean;
begin
  Result := FCheckSpelling;
end;

{**************************************************************}
procedure TALMacEditView.setCheckSpelling(const Value: Boolean);
begin
  FCheckSpelling := Value;
end;

{*******************************************************}
function TALMacEditView.GetReturnKeyType: TReturnKeyType;
begin
  Result := FReturnKeyType;
end;

{*********************************************************************}
procedure TALMacEditView.setReturnKeyType(const Value: TReturnKeyType);
begin
  FReturnKeyType := Value;
end;

{********************************************}
function TALMacEditView.GetPromptText: String;
begin
  var LAttributedString := TALNSTextField.Wrap(NSObjectToID(View)).placeholderAttributedString;
  if LAttributedString = nil then Result := NSStrToStr(TALNSTextField.Wrap(NSObjectToID(View)).PlaceholderString)
  else result := NSStrToStr(TALNSAttributedString.wrap(NSObjectToID(LAttributedString)).&String);
end;

{**********************************************************}
procedure TALMacEditView.setPromptText(const Value: String);
begin
  applyPromptTextWithColor(Value, fPromptTextColor);
end;

{******************************************************}
function TALMacEditView.GetPromptTextColor: TAlphaColor;
begin
  result := fPromptTextColor;
end;

{********************************************************************}
procedure TALMacEditView.setPromptTextColor(const Value: TAlphaColor);
begin
  if Value <> fPromptTextColor then begin
    fPromptTextColor := Value;
    applyPromptTextWithColor(GetPromptText, fPromptTextColor);
  end;
end;

{***********************************************************************************************}
procedure TALMacEditView.applyPromptTextWithColor(const aStr: String; const aColor: TAlphaColor);
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
        var LFontRef := ALCreateCTFontRef(ALResolveFontFamily(TextSettings.Font.Family), TextSettings.Font.Size, TextSettings.Font.Weight, TextSettings.Font.Slant);
        try
          LPromptTextAttr.addAttribute(NSFontAttributeName, NSObjectToID(TNSFont.Wrap(LFontRef)), LTextRange);
        finally
          CFRelease(LFontRef);
        end;
      finally
        LPromptTextAttr.endEditing;
      end;
    end;

    TALNSTextField.Wrap(NSObjectToID(View)).setPlaceholderAttributedString(LPromptTextAttr);

  finally
    LPromptTextAttr.release;
  end;
end;

{************************************************}
function TALMacEditView.GetTintColor: TAlphaColor;
begin
  Result := FTintColor;
end;

{**************************************************************}
procedure TALMacEditView.setTintColor(const Value: TAlphaColor);
begin
  FTintColor := Value;
end;

{************************************************}
function TALMacEditView.GetFillColor: TAlphaColor;
begin
  Result := FFillColor;
end;

{**************************************************************}
procedure TALMacEditView.SetFillColor(const Value: TAlphaColor);
begin
  FFillColor := Value;
end;

{**************************************}
function TALMacEditView.getText: String;
begin
  result := NSStrToStr(View.StringValue);
end;

{****************************************************}
procedure TALMacEditView.SetText(const Value: String);
begin
  if Value <> getText then begin
    View.setStringValue(StrToNSStr(Value));
    DoChange;
  end;
end;

{***********************************************************}
function TALMacEditView.GetTextSettings: TALBaseTextSettings;
begin
  result := FTextSettings;
end;

{*************************************************************************}
procedure TALMacEditView.SetTextSettings(const Value: TALBaseTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{************************************************************}
procedure TALMacEditView.TextSettingsChanged(Sender: TObject);
begin
  // Font
  var LFontRef := ALCreateCTFontRef(ALResolveFontFamily(TextSettings.Font.Family), TextSettings.Font.Size, TextSettings.Font.Weight, TextSettings.Font.Slant);
  try
    View.setFont(TNSFont.Wrap(LFontRef));
  finally
    CFRelease(LFontRef);
  end;

  // TextAlignment
  View.setAlignment(ALTextHorzAlignToNSTextAlignment(TextSettings.HorzAlign));

  // TextColor
  View.setTextColor(AlphaColorToNSColor(TextSettings.Font.Color));

  // Update the PromptText with the new font settings. This is only necessary in macOS.
  // In iOS, this step is not required.
  applyPromptTextWithColor(PromptText, PromptTextColor);
end;

{********************************}
procedure TALMacEditView.DoChange;
begin
  Control.DoChange;
end;

{********************************************}
function TALMacEditView.GetMaxLength: integer;
begin
  Result := FMaxLength;
end;

{**********************************************************}
procedure TALMacEditView.SetMaxLength(const Value: integer);
begin
  FMaxLength := Value;
end;

{********************************************}
function TALMacEditView.getLineHeight: Single;
begin
  var LfontMetrics := ALGetFontMetrics(
                        ALResolveFontFamily(TextSettings.Font.Family), // const AFontFamily: String;
                        TextSettings.Font.Size, // const AFontSize: single;
                        TextSettings.Font.Weight, // const AFontWeight: TFontWeight;
                        TextSettings.Font.Slant); // const AFontSlant: TFontSlant;
  result := -LfontMetrics.Ascent + LfontMetrics.Descent + LfontMetrics.Leading;
  if not SameValue(textsettings.LineHeightMultiplier, 0, TEpsilon.Scale) then
    result := result * textsettings.LineHeightMultiplier;
end;

{*********************************************************************************}
Procedure TALMacEditView.setSelection(const AStart: integer; const AStop: Integer);
begin
  View.currentEditor.setSelectedRange(NSMakeRange(AStart, AStop-AStart));
end;

{***********************************************************}
Procedure TALMacEditView.setSelection(const AIndex: integer);
begin
  View.currentEditor.setSelectedRange(NSMakeRange(AIndex, 0));
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

{********************************}
constructor TALWinEditView.Create;
begin
  CheckCommonControl(ICC_STANDARD_CLASSES);
  inherited;
  FFontHandle := 0;
  FBackgroundBrush := 0;
  FTextSettings := TALBaseEdit.TTextSettings.Create;
  FTextSettings.OnChanged := TextSettingsChanged;
  FFillColor := $ffffffff;
  FPromptText := '';
  FPromptTextColor := TalphaColors.Null;
  fReturnKeyType := tReturnKeyType.Default;
  fKeyboardType := TVirtualKeyboardType.default;
  fAutoCapitalizationType := TALAutoCapitalizationType.acNone;
  fCheckSpelling := true;
  FTintColor := TalphaColors.Null;
  SetPassword(false);
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
  ALFreeAndNil(FTextSettings);
  inherited Destroy;
end;

{**********************************************}
function TALWinEditView.GetControl: TALBaseEdit;
begin
  Result := TALBaseEdit(inherited Control);
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
                   -Round(TextSettings.Font.Size * ALGetScreenScale), // nHeight
                   0, // nWidth
                   0, // nEscapement
                   0, // nOrientaion
                   FontWeightToWinapi(TextSettings.Font.Weight), // fnWeight
                   DWORD(not TextSettings.Font.Slant.IsRegular), // fdwItalic
                   DWORD(TALTextDecorationKind.Underline in TextSettings.Decoration.Kinds), // fdwUnderline
                   DWORD(TALTextDecorationKind.LineThrough in TextSettings.Decoration.Kinds), // fdwStrikeOut
                   0, // fdwCharSet
                   0, // fdwOutputPrecision
                   0, // fdwClipPrecision
                   0, // fdwQuality
                   0, // fdwPitchAndFamily
                   PChar(ALResolveFontFamily(ALExtractPrimaryFontFamily(TextSettings.Font.Family)))); // lpszFace

  SendMessage(Handle, WM_SETFONT, FFontHandle, 1);
end;

{*********************************************}
procedure TALWinEditView.UpdateBackgroundBrush;
begin
  if (fBackgroundBrush <> 0) and (not DeleteObject(fBackgroundBrush)) then
    RaiseLastOsError;
  if FillColor <> TAlphaColors.Null then begin
    fBackgroundBrush := CreateSolidBrush(TAlphaColors.ColorToRGB(FillColor));
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
  if Message.CharCode = VK_RETURN then Control.DoReturnKey
  else if Message.CharCode = VK_DELETE then Control.DoChange;
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
  Control.DoChange;
end;

{**********************************************************}
procedure TALWinEditView.WMSetText(var Message: TWMSetText);
begin
  inherited;
  invalidate;
  Control.DoChange;
end;

{******************************************************}
procedure TALWinEditView.WMPaste(var Message: TWMPaste);
begin
  inherited;
  invalidate;
  Control.DoChange;
end;

{**************************************************}
procedure TALWinEditView.WMCut(var Message: TWMCut);
begin
  inherited;
  invalidate;
  Control.DoChange;
end;

{******************************************************}
procedure TALWinEditView.WMClear(var Message: TWMClear);
begin
  inherited;
  invalidate;
  Control.DoChange;
end;

{****************************************************}
procedure TALWinEditView.WMUndo(var Message: TWMUndo);
begin
  inherited;
  invalidate;
  Control.DoChange;
end;

{**************************************************************************}
procedure TALWinEditView.WMTextColor(var Message: WinApi.Messages.TMessage);
begin
  inherited;
  if SetTextColor(Message.wParam, TAlphaColors.ColorToRGB(TextSettings.Font.Color)) = CLR_INVALID then RaiseLastOSError;
  if fBackgroundBrush <> 0 then begin
    if SetBkColor(Message.wParam, TAlphaColors.ColorToRGB(FillColor)) = CLR_INVALID then RaiseLastOSError;
    Message.Result := LRESULT(FBackgroundBrush);
  end
  else
    Message.Result := GetSysColorBrush(COLOR_WINDOW);
end;

{******************************************************}
procedure TALWinEditView.WMPaint(var Message: TWMPaint);
begin
  if (PromptText <> '') and
     (GetWindowTextLength(Handle) = 0) then begin
    var LPS: PAINTSTRUCT;
    var LDC := BeginPaint(Handle, LPS);
    if LDC = 0 then raise Exception.Create('Error 3B053C24-4A18-497C-82B1-C540EF7C2A4B');
    Try
      var LPromptTextColor := PromptTextColor;
      if LPromptTextColor = TAlphaColors.Null then
        LPromptTextColor := ALBlendColor(fillColor, TextSettings.Font.Color, 0.3);
      if SetTextColor(LDC, TAlphaColors.ColorToRGB(LPromptTextColor)) = CLR_INVALID then RaiseLastOSError;
      if SetBkColor(LDC, TAlphaColors.ColorToRGB(fillColor)) = CLR_INVALID then RaiseLastOSError;
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
      case TextSettings.HorzAlign of
        TALTextHorzAlign.Center: begin
          var LTextSize: TSize;
          GetTextExtentPoint32(LDC, Pchar(PromptText), Length(PromptText), LTextSize);
          TextOut(LDC, round((Control.GetNativeViewWidth - LtextSize.cx) / 2), 0, Pchar(PromptText), Length(PromptText));
        end;
        TALTextHorzAlign.Leading,
        TALTextHorzAlign.Justify:  begin
          TextOut(LDC, round(LOWORD(LMargins) * ALGetScreenScale), 0, Pchar(PromptText), Length(PromptText));
        end;
        TALTextHorzAlign.Trailing: begin
          var LTextSize: TSize;
          GetTextExtentPoint32(LDC, Pchar(PromptText), Length(PromptText), LTextSize);
          TextOut(LDC, round(Control.GetNativeViewWidth - LtextSize.cx - HIWORD(LMargins)), 0, Pchar(PromptText), Length(PromptText));
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

{********************************************}
function TALWinEditView.getLineCount: integer;
begin
  Result := 1;
end;

{************************************************************}
function TALWinEditView.GetKeyboardType: TVirtualKeyboardType;
begin
  Result := FKeyboardType;
end;

{**************************************************************************}
procedure TALWinEditView.setKeyboardType(const Value: TVirtualKeyboardType);
begin
  FKeyboardType := Value;
end;

{***************************************************************************}
function TALWinEditView.GetAutoCapitalizationType: TALAutoCapitalizationType;
begin
  result := fAutoCapitalizationType;
end;

{*****************************************************************************************}
procedure TALWinEditView.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  if (value <> fAutoCapitalizationType) then begin
    fAutoCapitalizationType := Value;
    var LStyle := GetWindowLong(Handle, GWL_STYLE);
    if LStyle = 0 then RaiseLastOsError;
    case fAutoCapitalizationType of
      TALAutoCapitalizationType.acNone:          LStyle := LStyle and not ES_UPPERCASE;
      TALAutoCapitalizationType.acWords:         LStyle := LStyle and not ES_UPPERCASE;
      TALAutoCapitalizationType.acSentences:     LStyle := LStyle and not ES_UPPERCASE;
      TALAutoCapitalizationType.acAllCharacters: LStyle := LStyle or ES_UPPERCASE;
      else raise Exception.Create('Error 21CC0DF5-9030-4F6C-9830-112E17E1A392');
    end;
    if SetWindowLong(Handle, GWL_STYLE, LStyle) = 0 then RaiseLastOsError;
  end;
end;

{*********************************************************}
procedure TALWinEditView.setPassword(const Value: Boolean);
begin
  if (value <> Password) then begin
    if Value then SendMessage(Handle, EM_SETPASSWORDCHAR, Ord('*'), 0)
    else SendMessage(Handle, EM_SETPASSWORDCHAR, 0, 0);
    Invalidate;
  end;
end;

{*******************************************}
function TALWinEditView.GetPassword: Boolean;
begin
  Result := SendMessage(Handle, EM_GETPASSWORDCHAR, 0, 0) <> 0;
end;

{**********************************************************}
procedure TALWinEditView.SetMaxLength(const Value: integer);
begin
  SendMessage(Handle, EM_LIMITTEXT, Value, 0);
end;

{********************************************}
function TALWinEditView.GetMaxLength: integer;
begin
  Result := SendMessage(Handle, EM_GETLIMITTEXT, 0, 0);
  if Result = $7FFFFFFE {2147483646} then Result := 0;
end;

{************************************************}
function TALWinEditView.GetCheckSpelling: Boolean;
begin
  result := FCheckSpelling
end;

{**************************************************************}
procedure TALWinEditView.setCheckSpelling(const Value: Boolean);
begin
  FCheckSpelling := Value;
end;

{*******************************************************}
function TALWinEditView.GetReturnKeyType: TReturnKeyType;
begin
  Result := FReturnKeyType;
end;

{*********************************************************************}
procedure TALWinEditView.setReturnKeyType(const Value: TReturnKeyType);
begin
  FReturnKeyType := Value;
end;

{********************************************}
function TALWinEditView.GetPromptText: String;
begin
  Result := FPromptText;
end;

{**********************************************************}
procedure TALWinEditView.setPromptText(const Value: String);
begin
  if FPromptText <> Value then begin
    FPromptText := Value;
    Invalidate;
  end;
end;

{******************************************************}
function TALWinEditView.GetPromptTextColor: TAlphaColor;
begin
  Result := FPromptTextColor;
end;

{********************************************************************}
procedure TALWinEditView.setPromptTextColor(const Value: TAlphaColor);
begin
  if Value <> FPromptTextColor then begin
    FPromptTextColor := Value;
    Invalidate;
  end;
end;

{************************************************}
function TALWinEditView.GetTintColor: TAlphaColor;
begin
  Result := FTintColor;
end;

{**************************************************************}
procedure TALWinEditView.setTintColor(const Value: TAlphaColor);
begin
  FTintColor := Value;
end;

{************************************************}
function TALWinEditView.GetFillColor: TAlphaColor;
begin
  Result := FFillColor;
end;

{**************************************************************}
procedure TALWinEditView.SetFillColor(const Value: TAlphaColor);
begin
  if FFillColor <> Value then begin
    FFillColor := Value;
    UpdateBackgroundBrush;
    Invalidate;
  end;
end;

{**************************************}
function TALWinEditView.getText: String;
begin
  Result := GetHWNDText(Handle);
end;

{****************************************************}
procedure TALWinEditView.SetText(const Value: String);
begin
  SetWindowText(Handle, PChar(Value));
end;

{*************************************************************************}
procedure TALWinEditView.SetTextSettings(const Value: TALBaseTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{************************************************************}
procedure TALWinEditView.TextSettingsChanged(Sender: TObject);
begin
  UpdateFontHandle;
  //--
  var LStyle := GetWindowLong(Handle, GWL_STYLE);
  if LStyle = 0 then RaiseLastOsError;
  case TextSettings.HorzAlign of
      TALTextHorzAlign.Center:   LStyle := LStyle or ES_CENTER;
      TALTextHorzAlign.Leading:  LStyle := LStyle or ES_LEFT;
      TALTextHorzAlign.Trailing: LStyle := LStyle or ES_RIGHT;
      TALTextHorzAlign.Justify:  LStyle := LStyle or ES_LEFT;
    else raise Exception.Create('Error 21CC0DF5-9030-4F6C-9830-112E17E1A392');
  end;
  if SetWindowLong(Handle, GWL_STYLE, LStyle) = 0 then RaiseLastOsError;
  SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, MakeLParam(0, 0));
  //--
  Invalidate;
end;

{********************************************}
function TALWinEditView.getLineHeight: Single;
begin
  var LfontMetrics := ALGetFontMetrics(
                        ALResolveFontFamily(TextSettings.Font.Family), // const AFontFamily: String;
                        TextSettings.Font.Size, // const AFontSize: single;
                        TextSettings.Font.Weight, // const AFontWeight: TFontWeight;
                        TextSettings.Font.Slant); // const AFontSlant: TFontSlant;
  result := -LfontMetrics.Ascent + LfontMetrics.Descent + LfontMetrics.Leading;
  // The classic Edit control doesn't natively support line spacing adjustments
  // if not SameValue(textsettings.LineHeightMultiplier, 0, TEpsilon.Scale) then
  //   result := result * textsettings.LineHeightMultiplier;
end;

{*********************************************************************************}
Procedure TALWinEditView.SetSelection(const AStart: integer; const AStop: Integer);
begin
  SendMessage(Handle, EM_SETSEL, AStart, AStop);
end;

{***********************************************************}
Procedure TALWinEditView.SetSelection(const AIndex: integer);
begin
  SendMessage(Handle, EM_SETSEL, AIndex, AIndex);
end;

{$endif}
{$ENDREGION}

{******************************************************}
function TALBaseEdit.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.white;
end;

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
  FMargins.OnChanged := MarginsChanged;
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
  FMargins.OnChanged := MarginsChanged;
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
  FPromptTextColorKey := DefaultPromptTextColorKey;
  FTintColor := DefaultTintColor;
  FTintColorKey := DefaultTintColorKey;
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
      PromptTextColorKey := TBaseStateStyle(Source).PromptTextColorKey;
      TintColor := TBaseStateStyle(Source).TintColor;
      TintColorKey := TBaseStateStyle(Source).TintColorKey;
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
    PromptTextColorKey := DefaultPromptTextColorKey;
    TintColor := DefaultTintColor;
    TintColorKey := DefaultTintColorKey;
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

{***************************************************************}
procedure TALBaseEdit.TBaseStateStyle.ApplyPromptTextColorScheme;
begin
  if FPromptTextColorKey <> '' then begin
    var LPromptTextColor := TALStyleManager.Instance.GetColor(FPromptTextColorKey);
    if FPromptTextColor <> LPromptTextColor then begin
      FPromptTextColor := LPromptTextColor;
      Change;
    end;
  end;
end;

{*********************************************************}
procedure TALBaseEdit.TBaseStateStyle.ApplyTintColorScheme;
begin
  if FTintColorKey <> '' then begin
    var LTintColor := TALStyleManager.Instance.GetColor(FTintColorKey);
    if FTintColor <> LTintColor then begin
      FTintColor := LTintColor;
      Change;
    end;
  end;
end;

{*****************************************************}
procedure TALBaseEdit.TBaseStateStyle.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    Inherited;
    TextSettings.ApplyColorScheme;
    LabelTextSettings.ApplyColorScheme;
    SupportingTextSettings.ApplyColorScheme;
    ApplyPromptTextColorScheme;
    ApplyTintColorScheme;
  finally
    EndUpdate;
  End;
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

{**************************************************************************************************************************************}
procedure TALBaseEdit.TBaseStateStyle.Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single; const AReverse: Boolean);
begin
  {$IF defined(debug)}
  if (ATo <> nil) and (not (ATo is TBaseStateStyle)) then
    Raise Exception.Create('Error 80F4A6FB-B9A3-4D69-B304-1E1DC2047851');
  {$ENDIF}
  BeginUpdate;
  Try
    var LPrevPromptTextColorKey := FPromptTextColorKey;
    var LPrevTintColorKey := FTintColorKey;

    inherited Interpolate(ATo, ANormalizedTime, AReverse);

    if ATo <> nil then begin
      PromptTextColor := ALInterpolateColor(PromptTextColor{Start}, TBaseStateStyle(ATo).PromptTextColor{Stop}, ANormalizedTime);
      TextSettings.Interpolate(TBaseStateStyle(ATo).TextSettings, ANormalizedTime, AReverse);
      TintColor := ALInterpolateColor(TintColor{Start}, TBaseStateStyle(ATo).TintColor{Stop}, ANormalizedTime);
    end
    {$IF defined(debug)}
    else if StateStyleParent <> nil then Raise Exception.Create('Error FE80E1DC-6F9F-42BF-825B-C72A12A704B8')
    {$ENDIF}
    else if ControlParent <> nil then begin
      PromptTextColor := ALInterpolateColor(PromptTextColor{Start}, ControlParent.PromptTextColor{Stop}, ANormalizedTime);
      TextSettings.Interpolate(ControlParent.TextSettings, ANormalizedTime, AReverse);
      TintColor := ALInterpolateColor(TintColor{Start}, ControlParent.TintColor{Stop}, ANormalizedTime);
    end
    else begin
      PromptTextColor := ALInterpolateColor(PromptTextColor{Start}, DefaultPromptTextColor{Stop}, ANormalizedTime);
      TextSettings.Interpolate(nil, ANormalizedTime, AReverse);
      TintColor := ALInterpolateColor(TintColor{Start}, DefaultTintColor{Stop}, ANormalizedTime);
    end;

    var FPromptTextColorKey := LPrevPromptTextColorKey;
    var FTintColorKey := LPrevTintColorKey;
  finally
    EndUpdate;
  end;
end;

{************************************************}
procedure TALBaseEdit.TBaseStateStyle.DoSupersede;
begin
  inherited;
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
    FPromptTextColorKey := '';
    Change;
  end;
end;

{********************************************************************************}
procedure TALBaseEdit.TBaseStateStyle.SetPromptTextColorKey(const AValue: String);
begin
  if FPromptTextColorKey <> AValue then begin
    FPromptTextColorKey := AValue;
    ApplyPromptTextColorScheme;
  end;
end;

{****************************************************************************}
procedure TALBaseEdit.TBaseStateStyle.SetTintColor(const AValue: TAlphaColor);
begin
  if FTintColor <> AValue then begin
    FTintColor := AValue;
    FTintColorKey := '';
    Change;
  end;
end;

{**************************************************************************}
procedure TALBaseEdit.TBaseStateStyle.SetTintColorKey(const AValue: String);
begin
  if FTintColorKey <> AValue then begin
    FTintColorKey := AValue;
    ApplyTintColorScheme;
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

{************************************************************************}
function TALBaseEdit.TBaseStateStyle.GetDefaultPromptTextColorKey: String;
begin
  Result := '';
end;

{********************************************************************}
function TALBaseEdit.TBaseStateStyle.GetDefaultTintColor: TalphaColor;
begin
  Result := TAlphaColors.Null;
end;

{******************************************************************}
function TALBaseEdit.TBaseStateStyle.GetDefaultTintColorKey: String;
begin
  Result := '';
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

{***********************************************************************}
function TALBaseEdit.TBaseStateStyle.IsPromptTextColorKeyStored: Boolean;
begin
  result := FPromptTextColorKey <> DefaultPromptTextColorKey;
end;

{**************************************************************}
function TALBaseEdit.TBaseStateStyle.IsTintColorStored: Boolean;
begin
  result := FTintColor <> DefaultTintColor;
end;

{*****************************************************************}
function TALBaseEdit.TBaseStateStyle.IsTintColorKeyStored: Boolean;
begin
  result := FTintColorKey <> DefaultTintColorKey;
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

{***************************************************}
procedure TALBaseEdit.TStateStyles.TTransition.Start;
begin
  inherited;
  TStateStyles(Owner).Parent.UpdateNativeViewStyle;
end;

{*******************************************************}
procedure TALBaseEdit.TStateStyles.TTransition.DoProcess;
begin
  inherited;
  if Enabled then
    TStateStyles(Owner).Parent.UpdateNativeViewStyle;
end;

{******************************************************}
procedure TALBaseEdit.TStateStyles.TTransition.DoFinish;
begin
  inherited;
  if Enabled then
    TStateStyles(Owner).Parent.UpdateNativeViewStyle;
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

{*********************************************************************************}
function TALBaseEdit.TStateStyles.CreateTransition: TALBaseStateStyles.TTransition;
begin
  result := TTransition.Create(Self);
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
procedure TALBaseEdit.TStateStyles.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.ApplyColorScheme;
    Hovered.ApplyColorScheme;
    Focused.ApplyColorScheme;
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
  FAutoTranslate := true;
  fOnChange := nil;
  FOnReturnKey := nil;
  //--
  FTextSettings := CreateTextSettings;
  FTextSettings.OnChanged := TextSettingsChanged;
  //--
  FPromptText := '';
  FPromptTextColor := TAlphaColors.null;
  FPromptTextColorKey := '';
  //--
  FTintcolor := TAlphaColors.null;
  FTintcolorKey := '';
  //--
  FLabelText := '';
  FLabelTextSettings := CreateLabelTextSettings;
  FLabelTextSettings.OnChanged := LabelTextSettingsChanged;
  FlabelTextAnimation := TALFloatAnimation.Create;
  FlabelTextAnimation.StartValue := 0;
  FlabelTextAnimation.StopValue := 1;
  FlabelTextAnimation.InterpolationType := TALInterpolationType.linear;
  FlabelTextAnimation.InterpolationMode := TALInterpolationMode.out;
  FlabelTextAnimation.OnProcess := labelTextAnimationProcess;
  FlabelTextAnimation.OnFinish := labelTextAnimationFinish;
  //--
  FSupportingText := '';
  FSupportingTextSettings := CreateSupportingTextSettings;
  FSupportingTextSettings.OnChanged := SupportingTextSettingsChanged;
  FSupportingTextMarginBottomUpdated := False;
  //--
  FIsTextEmpty := True;
  //--
  FDummyFillColor := $ffffffff;
  fDummyReturnKeyType := tReturnKeyType.Default;
  fDummyKeyboardType := TVirtualKeyboardType.default;
  fDummyAutoCapitalizationType := TALAutoCapitalizationType.acNone;
  fDummyCheckSpelling := true;
  FDummyPassword := False;
  FDummyMaxLength := 0;
  FDummyText := '';
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
  inherited Destroy;
end;

{************************************************}
procedure TALBaseEdit.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TALBaseEdit then begin
      DefStyleAttr := TALBaseEdit(Source).DefStyleAttr;
      DefStyleRes := TALBaseEdit(Source).DefStyleRes;
      AutoTranslate := TALBaseEdit(Source).AutoTranslate;
      OnChange := TALBaseEdit(Source).OnChange;
      OnReturnKey := TALBaseEdit(Source).OnReturnKey;
      TextSettings.Assign(TALBaseEdit(Source).TextSettings);
      PromptText := TALBaseEdit(Source).PromptText;
      PromptTextColor := TALBaseEdit(Source).PromptTextColor;
      PromptTextColorKey := TALBaseEdit(Source).PromptTextColorKey;
      Tintcolor := TALBaseEdit(Source).Tintcolor;
      TintcolorKey := TALBaseEdit(Source).TintcolorKey;
      LabelText := TALBaseEdit(Source).LabelText;
      LabelTextSettings.Assign(TALBaseEdit(Source).LabelTextSettings);
      SupportingText := TALBaseEdit(Source).SupportingText;
      SupportingTextSettings.Assign(TALBaseEdit(Source).SupportingTextSettings);
      StateStyles.Assign(TALBaseEdit(Source).StateStyles);
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
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

{***********************************************}
procedure TALBaseEdit.ApplyPromptTextColorScheme;
begin
  if FPromptTextColorKey <> '' then begin
    var LPromptTextColor := TALStyleManager.Instance.GetColor(FPromptTextColorKey);
    if FPromptTextColor <> LPromptTextColor then begin
      FPromptTextColor := LPromptTextColor;
      ClearBufPromptTextDrawable;
      UpdateNativeViewStyle;
    end;
  end;
end;

{*****************************************}
procedure TALBaseEdit.ApplyTintColorScheme;
begin
  if FTintColorKey <> '' then begin
    var LTintColor := TALStyleManager.Instance.GetColor(FTintColorKey);
    if FTintColor <> LTintColor then begin
      FTintColor := LTintColor;
      UpdateNativeViewStyle;
    end;
  end;
end;

{*************************************}
procedure TALBaseEdit.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    inherited;
    TextSettings.ApplyColorScheme;
    LabelTextSettings.ApplyColorScheme;
    SupportingTextSettings.ApplyColorScheme;
    StateStyles.ApplyColorScheme;
    ApplyPromptTextColorScheme;
    ApplyTintColorScheme;
    {$IFDEF ANDROID}
    RecreateNativeView;
    {$ENDIF}
  finally
    EndUpdate;
  end;
end;

{****************************************}
function TALBaseEdit.CreateFill: TALBrush;
begin
  Result := TFill.Create;
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

{***********************************}
procedure TALBaseEdit.InitNativeView;
begin
  if FNativeView = nil then exit;
  NativeView.Password := false; // noops operation
  NativeView.ReturnKeyType := tReturnKeyType.Default;  // noops operation
  NativeView.KeyboardType := TVirtualKeyboardType.Default; // noops operation
  NativeView.AutoCapitalizationType := TALAutoCapitalizationType.acNone; // noops operation
  NativeView.CheckSpelling := True;
  NativeView.MaxLength := 0; // noops operation
  NativeView.PromptTextColor := TALphaColors.Null; // noops operation
  NativeView.TintColor := TALphaColors.Null; // noops operation
  NativeView.FillColor := $ffffffff; // noops operation
end;

{***************************************}
procedure TALBaseEdit.RecreateNativeView;
begin
  if FNativeView = nil then exit;
  var LPassword := NativeView.Password;
  var LReturnKeyType := NativeView.ReturnKeyType;
  var LKeyboardType := NativeView.KeyboardType;
  var LAutoCapitalizationType := NativeView.AutoCapitalizationType;
  var LCheckSpelling := NativeView.CheckSpelling;
  var LMaxLength := NativeView.MaxLength;
  var LText := NativeView.Text;
  var LOnChange := fOnChange;
  fOnChange := nil;
  inherited;
  UpdateNativeViewStyle;
  UpdateEditControlPromptText;
  NativeView.Password := LPassword;
  NativeView.ReturnKeyType := LReturnKeyType;
  NativeView.KeyboardType := LKeyboardType;
  NativeView.AutoCapitalizationType := LAutoCapitalizationType;
  NativeView.CheckSpelling := LCheckSpelling;
  NativeView.MaxLength := LMaxLength;
  NativeView.Text := LText;
  UpdateNativeViewVisibility;
  fOnChange := LOnChange;
end;

{***************************}
procedure TALBaseEdit.Loaded;
begin
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
  //UpdateNativeViewStyle => Already called in TextSettingsChanged
  //--
  UpdateEditControlPromptText;
  UpdateNativeViewVisibility;
end;

{******************************************}
function TALBaseEdit.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(200, 50);
end;

{********************}
{$IF defined(android)}
Function TALBaseEdit.CreateNativeView: TALAndroidNativeView;
begin
  result := TALAndroidEditView.create(self, false{FIsMultiline}, DefStyleAttr, DefStyleRes);
end;
{$ENDIF}

{********************}
{$IF defined(android)}
function TALBaseEdit.GetNativeView: TALAndroidEditView;
begin
  result := TALAndroidEditView(inherited NativeView);
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
Function TALBaseEdit.CreateNativeView: TALIosNativeView;
begin

  // [NOTE] This workaround is no longer necessary, as the delphi framework (e.g., the virtual
  // keyboard service) already instantiates a UITextField internally during app startup,
  // which ensures that the Objective-C class is properly loaded and registered by the Delphi runtime.
  //
  // Originally, we had to create a UITextField instance explicitly to force the Objective-C class
  // (UITextField) to be registered. Without this, calling `TALIosWebView(inherited GetNativeView)`
  // could raise the following error:
  //   Unhandled Exception | Item not found
  //   At address: $0000000100365670
  //   (Generics.Collections.TDictionary<TTypeInfo*, TRegisteredDelphiClass*>.GetItem)
  //
  // Attempting to register the class manually like this:
  //   RegisterObjectiveCClass(TUITextField, TypeInfo(UITextField));
  // also fails with:
  //   Unhandled Exception | Method function someUITextFieldMethod of class TUITextField not found
  //   At address: $00000001XXXXXXX
  //   (Macapi.Objectivec.TRegisteredDelphiClass.RegisterClass)
  //
  // Attempting to register our own wrapper class:
  //   RegisterObjectiveCClass(TALIosWebView, TypeInfo(IALIosWebView));
  // fails as well, with:
  //   Unhandled Exception | Objective-C class UITextField could not be found
  //   At address: $00000001046CA014
  //   (Macapi.Objectivec.ObjectiveCClassNotFound)
  //
  // The only reliable workaround is to instantiate a UITextField explicitly to force
  // the UIKit framework to be loaded and the class to be registered:
  //
  //if not IsUITextFieldClassRegistered then begin
  //  var LUITextField := TUITextField.Wrap(TUITextField.Alloc.initWithFrame(CGRectMake(0, 0, 0, 0)));
  //  LUITextField.release;
  //  IsUITextFieldClassRegistered := True;
  //end;

  result := TALIosEditView.create(self);

end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALBaseEdit.GetNativeView: TALIosBaseEditView;
begin
  result := TALIosBaseEditView(inherited NativeView);
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
Function TALBaseEdit.CreateNativeView: TALMacNativeView;
begin
  result := TALMacEditView.create(self);
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
function TALBaseEdit.GetNativeView: TALMacBaseEditView;
begin
  result := TALMacBaseEditView(inherited NativeView);
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
Function TALBaseEdit.CreateNativeView: TALWinNativeView;
begin
  {$IF defined(ALDPK)}
  Result := nil;
  {$ELSE}
  result := TALWinEditView.create(self);
  {$ENDIF}
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
function TALBaseEdit.GetNativeView: TALWinEditView;
begin
  result := TALWinEditView(inherited NativeView);
end;
{$ENDIF}

{*****************************}
procedure TALBaseEdit.DoChange;
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

{********************************}
procedure TALBaseEdit.DoReturnKey;
begin
  if assigned(fOnReturnKey) then
    fOnReturnKey(self);
end;

{****************************}
procedure TALBaseEdit.DoEnter;
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.DoEnter', 'control.name: ' + Name);
  {$ENDIF}
  inherited DoEnter;
  StateStyles.Transition.start;
  //--
  if (GetIsTextEmpty) and
     (HasTranslationLabelTextAnimation) then begin
    FlabelTextAnimation.StopAtCurrent;
    FlabelTextAnimation.Inverse := False;
    FlabelTextAnimation.Duration := 0.15;
    FLabelTextAnimation.Start;
  end;
  //--
  {$IF defined(android)}
  if (NativeView <> nil) and (IsFocused) then begin
    ALVirtualKeyboardVisible := True;
    {$IF defined(DEBUG)}
    //ALLog(Classname + '.showVirtualKeyboard', 'control.name: ' + Name);
    {$ENDIF}
    MainActivity.getVirtualKeyboard.showFor(NativeView.View);
  end;
  {$ENDIF}
end;

{***************************}
procedure TALBaseEdit.DoExit;
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.DoExit', 'control.name: ' + Name);
  {$ENDIF}
  inherited DoExit;
  StateStyles.Transition.start;
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
  {$IF defined(android)}
  if NativeView <> nil then begin
    ALVirtualKeyboardVisible := False;
    TThread.ForceQueue(nil,
      procedure
      begin
        If not ALVirtualKeyboardVisible then begin
          {$IF defined(DEBUG)}
          //ALLog(Classname + '.hideVirtualKeyboard');
          {$ENDIF}
          MainActivity.getVirtualKeyboard.hide;
        end;
      end);
  end;
  {$ENDIF}
end;

{******************************************}
procedure TALBaseEdit.UpdateNativeViewStyle;
begin

  if (csLoading in ComponentState) then exit;

  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);

  if NativeView <> nil then begin

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
      NativeView.FillColor := ALBlendColor(LStateStyle.Fill.Color, LStateLayerColor, LStateLayerOpacity);
    end
    else NativeView.FillColor := Fill.Color;
    // PromptTextColor
    if LStateStyle <> nil then NativeView.PromptTextColor := LStateStyle.PromptTextColor
    else NativeView.PromptTextColor := PromptTextColor;
    // TintColor
    if LStateStyle <> nil then NativeView.TintColor := LStateStyle.TintColor
    else NativeView.TintColor := TintColor;
    // TextSettings
    if LStateStyle <> nil then NativeView.TextSettings.Assign(LStateStyle.TextSettings)
    else NativeView.TextSettings.Assign(TextSettings);

  end
  else begin

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
      FDummyFillColor := ALBlendColor(LStateStyle.Fill.Color, LStateLayerColor, LStateLayerOpacity);
    end
    else FDummyFillColor := Fill.Color;

  end;
  repaint;

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
  StateStyles.Transition.start;
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
      if (APrevStateStyle.TextSettings.font.Color = AToStateStyle.TextSettings.font.Color) and
         (APrevStateStyle.TextSettings.font.ColorKey = AToStateStyle.TextSettings.font.ColorKey) then begin
        AToStateStyle.TextSettings.font.Color := TextSettings.font.Color;
        AToStateStyle.TextSettings.font.ColorKey := TextSettings.font.ColorKey;
      end;

    end;

    APrevStateStyle.TextSettings.font.Family := TextSettings.font.Family;
    APrevStateStyle.TextSettings.font.Size := TextSettings.font.Size;
    APrevStateStyle.TextSettings.font.Weight := TextSettings.font.Weight;
    APrevStateStyle.TextSettings.font.Slant := TextSettings.font.Slant;
    APrevStateStyle.TextSettings.font.Stretch := TextSettings.font.Stretch;
    APrevStateStyle.TextSettings.font.Color := TextSettings.font.Color;
    APrevStateStyle.TextSettings.font.ColorKey := TextSettings.font.ColorKey;

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
  UpdateNativeViewStyle;
  AdjustSize;
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
      if (APrevStateStyle.LabelTextSettings.font.Color = AToStateStyle.LabelTextSettings.font.Color) and
         (APrevStateStyle.LabelTextSettings.font.ColorKey = AToStateStyle.LabelTextSettings.font.ColorKey) then begin
        AToStateStyle.LabelTextSettings.font.Color := LabelTextSettings.font.Color;
        AToStateStyle.LabelTextSettings.font.ColorKey := LabelTextSettings.font.ColorKey;
      end;

    end;

    APrevStateStyle.LabelTextSettings.font.Family := LabelTextSettings.font.Family;
    APrevStateStyle.LabelTextSettings.font.Size := LabelTextSettings.font.Size;
    APrevStateStyle.LabelTextSettings.font.Weight := LabelTextSettings.font.Weight;
    APrevStateStyle.LabelTextSettings.font.Slant := LabelTextSettings.font.Slant;
    APrevStateStyle.LabelTextSettings.font.Stretch := LabelTextSettings.font.Stretch;
    APrevStateStyle.LabelTextSettings.font.Color := LabelTextSettings.font.Color;
    APrevStateStyle.LabelTextSettings.font.ColorKey := LabelTextSettings.font.ColorKey;

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
      if (APrevStateStyle.SupportingTextSettings.font.Color = AToStateStyle.SupportingTextSettings.font.Color) and
         (APrevStateStyle.SupportingTextSettings.font.ColorKey = AToStateStyle.SupportingTextSettings.font.ColorKey) then begin
        AToStateStyle.SupportingTextSettings.font.Color := SupportingTextSettings.font.Color;
        AToStateStyle.SupportingTextSettings.font.ColorKey := SupportingTextSettings.font.ColorKey;
      end;

    end;

    APrevStateStyle.SupportingTextSettings.font.Family := SupportingTextSettings.font.Family;
    APrevStateStyle.SupportingTextSettings.font.Size := SupportingTextSettings.font.Size;
    APrevStateStyle.SupportingTextSettings.font.Weight := SupportingTextSettings.font.Weight;
    APrevStateStyle.SupportingTextSettings.font.Slant := SupportingTextSettings.font.Slant;
    APrevStateStyle.SupportingTextSettings.font.Stretch := SupportingTextSettings.font.Stretch;
    APrevStateStyle.SupportingTextSettings.font.Color := SupportingTextSettings.font.Color;
    APrevStateStyle.SupportingTextSettings.font.ColorKey := SupportingTextSettings.font.ColorKey;

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
  UpdateNativeViewStyle;
  Repaint;
end;

{*************************************************}
procedure TALBaseEdit.SetText(const Value: String);
begin
  if NativeView <> nil then begin
    NativeView.Text := Value;
    UpdateNativeViewVisibility;
  end
  else begin
    FDummyText := Value;
    ClearBufPromptTextDrawable;
    ClearBufLabelTextDrawable;
    repaint;
  end;
end;

{***********************************}
function TALBaseEdit.getText: String;
begin
  if NativeView <> nil then
    result := NativeView.Text
  else
    result := FDummyText;
end;

{*************************************************}
function TALBaseEdit.ShouldHideNativeView: Boolean;
begin
  result := {$IF defined(ALDPK)}true or{$ENDIF}
            (not Enabled) or
            ((GetIsTextEmpty) and (HasTranslationLabelTextAnimation) and (not IsFocused))
end;

{***********************************************}
procedure TALBaseEdit.UpdateNativeViewVisibility;
begin
  if ShouldHideNativeView then HideNativeView
  else ShowNativeView;
end;

{************************************************}
procedure TALBaseEdit.UpdateEditControlPromptText;
begin
  if NativeView = nil then exit;
  if HasTranslationLabelTextAnimation then NativeView.PromptText := ''
  else if FPromptText<>'' then NativeView.PromptText := FPromptText
  else NativeView.PromptText := FLabelText;
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
    FPromptText := Value;
    ClearBufPromptTextDrawable;
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

{*************************************************}
function TALBaseEdit.GetPromptTextColorKey: String;
begin
  result := FPromptTextColorKey;
end;

{*****************************************************************}
procedure TALBaseEdit.setPromptTextColor(const Value: TAlphaColor);
begin
  if FPromptTextColor <> Value then begin
    FPromptTextColor := Value;
    FPromptTextColorKey := '';
    ClearBufPromptTextDrawable;
    UpdateNativeViewStyle;
  end;
end;

{***************************************************************}
procedure TALBaseEdit.setPromptTextColorKey(const Value: String);
begin
  if FPromptTextColorKey <> Value then begin
    FPromptTextColorKey := Value;
    ApplyPromptTextColorScheme;
  end;
end;

{******************************************************}
procedure TALBaseEdit.setLabelText(const Value: String);
begin
  if FLabelText <> Value then begin
    FLabelText := Value;
    ClearBufPromptTextDrawable;
    ClearBufLabelTextDrawable;
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
    FSupportingText := Value;
    ClearBufSupportingTextDrawable;
    Repaint;
  end;
end;

{*********************************************}
function TALBaseEdit.GetTintColor: TAlphaColor;
begin
  result := FTintColor;
end;

{*******************************************}
function TALBaseEdit.GetTintColorKey: String;
begin
  result := FTintColorKey;
end;

{***********************************************************}
procedure TALBaseEdit.setTintColor(const Value: TAlphaColor);
begin
  if FTintColor <> Value then begin
    FTintColor := Value;
    FTintColorKey := '';
    UpdateNativeViewStyle;
  end;
end;

{*********************************************************}
procedure TALBaseEdit.setTintColorKey(const Value: String);
begin
  if FTintColorKey <> Value then begin
    FTintColorKey := Value;
    ApplyTintColorScheme;
  end;
end;

{*****************************************************************}
procedure TALBaseEdit.SetKeyboardType(Value: TVirtualKeyboardType);
begin
  if NativeView <> nil then
    NativeView.KeyboardType := Value
  else
    FDummyKeyboardType := Value
end;

{*********************************************************}
function TALBaseEdit.GetKeyboardType: TVirtualKeyboardType;
begin
  if NativeView <> nil then
    result := NativeView.KeyboardType
  else
    result := FDummyKeyboardType
end;

{************************************************************************}
function TALBaseEdit.GetAutoCapitalizationType: TALAutoCapitalizationType;
begin
  if NativeView <> nil then
    result := NativeView.AutoCapitalizationType
  else
    result := FDummyAutoCapitalizationType;
end;

{**************************************************************************************}
procedure TALBaseEdit.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  if NativeView <> nil then
    NativeView.AutoCapitalizationType := Value
  else
    FDummyAutoCapitalizationType := Value
end;

{******************************************************}
procedure TALBaseEdit.SetPassword(const Value: Boolean);
begin
  if NativeView <> nil then
    NativeView.Password := Value
  else
    FDummyPassword := Value;
  var LText := Text;
  if LText <> '' then
    setSelection(length(LText));
  clearBufPromptTextDrawable;
end;

{****************************************}
function TALBaseEdit.GetPassword: Boolean;
begin
  if NativeView <> nil then
    result := NativeView.Password
  else
    result := FDummyPassword
end;

{***********************************************************}
procedure TALBaseEdit.SetCheckSpelling(const Value: Boolean);
begin
  if NativeView <> nil then
    NativeView.CheckSpelling := Value
  else
    FDummyCheckSpelling := Value
end;

{*********************************************}
function TALBaseEdit.GetCheckSpelling: Boolean;
begin
  if NativeView <> nil then
    result := NativeView.CheckSpelling
  else
    result := FDummyCheckSpelling
end;

{************************************************************}
procedure TALBaseEdit.SetReturnKeyType(Value: TReturnKeyType);
begin
  if NativeView <> nil then
    NativeView.ReturnKeyType := Value
  else
    FDummyReturnKeyType := Value
end;

{****************************************************}
function TALBaseEdit.GetReturnKeyType: TReturnKeyType;
begin
  if NativeView <> nil then
    result := NativeView.ReturnKeyType
  else
    result := FDummyReturnKeyType;
end;

{*******************************************************}
procedure TALBaseEdit.SetMaxLength(const Value: integer);
begin
  if NativeView <> nil then
    NativeView.MaxLength := Value
  else
    FDummyMaxLength := Value
end;

{*****************************************}
function TALBaseEdit.GetMaxLength: integer;
begin
  if NativeView <> nil then
    result := NativeView.MaxLength
  else
    result := FDummyMaxLength;
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

{***********************************}
procedure TALBaseEdit.EnabledChanged;
begin
  Inherited;
  if (csLoading in componentState) then exit;
  UpdateNativeViewStyle;
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
  UpdateNativeViewStyle;
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
    LOptions.AlignToPixel := AutoAlignToPixel;
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
    LOptions.AutoSize := TALAutoSizeMode.Both;
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
    //LOptions.FillBackgroundMargins: TRectF; // default = TRectF.Empty
    //LOptions.FillImageMargins: TRectF; // default = TRectF.Empty
    //LOptions.FillImageNoRadius: Boolean; // default = False
    //LOptions.FillWrapMode: TALImageWrapMode; // default = TALImageWrapMode.Fit
    //LOptions.FillCropCenter: TpointF; // default = TPointF.create(0.5,0.5)
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
    LOptions.AlignToPixel := AutoAlignToPixel;
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
    LOptions.AutoSize := TALAutoSizeMode.Both;
    //--
    LOptions.MaxLines := LabelTextSettings.MaxLines;
    LOptions.LineHeightMultiplier := LabelTextSettings.LineHeightMultiplier;
    LOptions.LetterSpacing := LabelTextSettings.LetterSpacing;
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
    //LOptions.FillBackgroundMargins: TRectF; // default = TRectF.Empty
    //LOptions.FillImageMargins: TRectF; // default = TRectF.Empty
    //LOptions.FillImageNoRadius: Boolean; // default = False
    //LOptions.FillWrapMode: TALImageWrapMode; // default = TALImageWrapMode.Fit
    //LOptions.FillCropCenter: TpointF; // default = TPointF.create(0.5,0.5)
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
    LOptions.AlignToPixel := AutoAlignToPixel;
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
    LOptions.AutoSize := TALAutoSizeMode.Both;
    //--
    LOptions.MaxLines := SupportingTextSettings.MaxLines;
    LOptions.LineHeightMultiplier := SupportingTextSettings.LineHeightMultiplier;
    LOptions.LetterSpacing := SupportingTextSettings.LetterSpacing;
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
    //LOptions.FillBackgroundMargins: TRectF; // default = TRectF.Empty
    //LOptions.FillImageMargins: TRectF; // default = TRectF.Empty
    //LOptions.FillImageNoRadius: Boolean; // default = False
    //LOptions.FillWrapMode: TALImageWrapMode; // default = TALImageWrapMode.Fit
    //LOptions.FillCropCenter: TpointF; // default = TPointF.create(0.5,0.5)
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
    var LPrevFontColorKey := LFont.ColorKey;
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
      Lfont.ColorKey := LPrevFontColorKey;
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
    var LPrevFontColorKey := LStateStyle.TextSettings.font.ColorKey;
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
      LStateStyle.TextSettings.font.ColorKey := LPrevFontColorKey;
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
  if StateStyles.Transition.Running then begin
    Result := ARect;
    if StateStyles.Transition.FromStateStyle <> nil then begin
      var LFromSurfaceRect := ALGetShapeSurfaceRect(
                                ARect, // const ARect: TRectF;
                                AutoAlignToPixel, // const AAlignToPixel: Boolean;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).Fill, // const AFill: TALBrush;
                                nil, // const AFillResourceStream: TStream;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).StateLayer, // const AStateLayer: TALStateLayer;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LFromSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
    if StateStyles.Transition.ToStateStyle <> nil then begin
      var LToSurfaceRect := ALGetShapeSurfaceRect(
                              ARect, // const ARect: TRectF;
                              AutoAlignToPixel, // const AAlignToPixel: Boolean;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).Fill, // const AFill: TALBrush;
                              nil, // const AFillResourceStream: TStream;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).StateLayer, // const AStateLayer: TALStateLayer;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LToSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  AutoAlignToPixel, // const AAlignToPixel: Boolean;
                  LStateStyle.Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
                  LStateStyle.Shadow); // const AShadow: TALShadow): TRectF;
    end
    else begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  AutoAlignToPixel, // const AAlignToPixel: Boolean;
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
  if StateStyles.Transition.Running then begin
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
          .SetAlignToPixel(AutoAlignToPixel)
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
          .SetAlignToPixel(AutoAlignToPixel)
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
  var LNativeViewBoundsRect := GetNativeViewBoundsRect;
  //--
  if LabelTextSettings.Layout = TLabelTextLayout.Inline then
    LLabelTextDrawableRect.SetLocation(
      LNativeViewBoundsRect.Left + LabelTextSettings.Margins.Left,
      Padding.top+NativeViewMargins.top-LLabelTextDrawableRect.Height-LabelTextSettings.Margins.Bottom)
  else
    LLabelTextDrawableRect.SetLocation(
      padding.Left + LabelTextSettings.Margins.Left,
      0-LLabelTextDrawableRect.Height-LabelTextSettings.Margins.Bottom);
  //--
  if (GetIsTextEmpty and HasTranslationLabelTextAnimation and (IsNativeViewVisible)) or
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
        if NativeView <> nil then
          Canvas.Fill.Color := NativeView.FillColor
        else
          Canvas.Fill.Color := FDummyFillColor;
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
     (GetIsTextEmpty and HasTranslationLabelTextAnimation and (not IsNativeViewVisible))
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
      TALTextHorzAlign.Center: LPos.X := LNativeViewBoundsRect.Left + ((LNativeViewBoundsRect.Width - LPromptTextDrawableRect.Width) / 2);
      TALTextHorzAlign.Leading: LPos.X := LNativeViewBoundsRect.Left;
      TALTextHorzAlign.Trailing: LPos.X := LNativeViewBoundsRect.Left + LNativeViewBoundsRect.Width - LPromptTextDrawableRect.Width;
      TALTextHorzAlign.Justify: LPos.X := LNativeViewBoundsRect.Left;
      else
        raise Exception.Create('Error EC344A62-E381-4126-A0EB-12BFC91E3664');
    end;
    if GetIsTextEmpty and (HasTranslationLabelTextAnimation) then begin
      if LabelTextSettings.Layout = TLabelTextLayout.Inline then begin
        case TextSettings.VertAlign of
          TALTextVertAlign.Center: LPos.y := LLabelTextDrawableRect.Top + ((LNativeViewBoundsRect.Height + LLabelTextDrawableRect.Height + LabelTextSettings.Margins.Bottom - LPromptTextDrawableRect.Height) / 2);
          TALTextVertAlign.Leading: LPos.y := LNativeViewBoundsRect.Top;
          TALTextVertAlign.Trailing: LPos.y := LNativeViewBoundsRect.Top + LNativeViewBoundsRect.Height - LPromptTextDrawableRect.Height;
          Else
            Raise Exception.Create('Error 28B2F8BC-B4C5-4F22-8E21-681AA1CA3C23')
        end;
      end
      else if LabelTextSettings.Layout = TLabelTextLayout.floating then begin
        case TextSettings.VertAlign of
          TALTextVertAlign.Center: LPos.y := LNativeViewBoundsRect.Top + ((LNativeViewBoundsRect.Height - LPromptTextDrawableRect.Height) / 2);
          TALTextVertAlign.Leading: LPos.y := LNativeViewBoundsRect.Top;
          TALTextVertAlign.Trailing: LPos.y := LNativeViewBoundsRect.Top + LNativeViewBoundsRect.Height - LPromptTextDrawableRect.Height;
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
            if NativeView <> nil then
              Canvas.Fill.Color := NativeView.FillColor
            else
              Canvas.Fill.Color := FDummyFillColor;
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
        TALTextVertAlign.Center: LPos.y := LNativeViewBoundsRect.Top + ((LNativeViewBoundsRect.Height - LPromptTextDrawableRect.Height) / 2);
        TALTextVertAlign.Leading: LPos.y := LNativeViewBoundsRect.Top;
        TALTextVertAlign.Trailing: LPos.y := LNativeViewBoundsRect.Top + LNativeViewBoundsRect.Height - LPromptTextDrawableRect.Height;
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

{***********************************}
Procedure TALBaseEdit.ShowNativeView;
begin
  if NativeView = nil then exit;
  if NativeView.visible then exit;
  if IsNativeViewFrozen then exit;
  if ShouldHideNativeView then Exit;
  inherited;
  {$IF defined(android)}
  if IsFocused then begin
    ALVirtualKeyboardVisible := True;
    {$IF defined(DEBUG)}
    ALLog(Classname + '.showVirtualKeyboard', 'control.name: ' + Name);
    {$ENDIF}
    MainActivity.getVirtualKeyboard.showFor(NativeView.View);
  end;
  {$ENDIF}
end;

{***********************************}
Procedure TALBaseEdit.HideNativeView;
begin
  if NativeView = nil then exit;
  if not NativeView.visible then exit;
  {$IF defined(android)}
  if IsFocused then begin
    ALVirtualKeyboardVisible := False;
    TThread.ForceQueue(nil,
      procedure
      begin
        If not ALVirtualKeyboardVisible then begin
          {$IF defined(DEBUG)}
          ALLog(Classname + '.hideVirtualKeyboard');
          {$ENDIF}
          MainActivity.getVirtualKeyboard.hide;
        end;
      end);
  end;
  {$ENDIF}
  inherited;
end;

{******************************************************************************}
Procedure TALBaseEdit.SetSelection(const AStart: integer; const AStop: Integer);
begin
  if NativeView <> nil then
    NativeView.SetSelection(AStart, AStop);
end;

{********************************************************}
Procedure TALBaseEdit.SetSelection(const AIndex: integer);
begin
  if NativeView <> nil then
    NativeView.SetSelection(AIndex);
end;

{*****************************************}
function TALBaseEdit.getLineCount: integer;
begin
  if NativeView <> nil then
    Result := NativeView.getLineCount
  else
    Result := 1;
end;

{*****************************************}
function TALBaseEdit.getLineHeight: single;
begin
  if NativeView <> nil then
    result := NativeView.getLineHeight
  else begin
    var LfontMetrics := ALGetFontMetrics(
                          ALResolveFontFamily(TextSettings.Font.Family), // const AFontFamily: String;
                          TextSettings.Font.Size, // const AFontSize: single;
                          TextSettings.Font.Weight, // const AFontWeight: TFontWeight;
                          TextSettings.Font.Slant); // const AFontSlant: TFontSlant;
    result := -LfontMetrics.Ascent + LfontMetrics.Descent + LfontMetrics.Leading;
    // The classic Edit control doesn't natively support line spacing adjustments
    // if not SameValue(textsettings.LineHeightMultiplier, 0, TEpsilon.Scale) then
    //   result := result * textsettings.LineHeightMultiplier;
  end;
end;

{*********************************************}
constructor TALEdit.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSize := TALAutoSizeMode.Both;
end;

{******************************************************}
function TALEdit.HasUnconstrainedAutosizeWidth: Boolean;
begin
  result := False;
end;

{***************************}
procedure TALEdit.AdjustSize;
begin
  if (not (csLoading in ComponentState)) and // loaded will call again AdjustSize
     (not (csDestroying in ComponentState)) and // if csDestroying do not do autosize
     (TNonReentrantHelper.EnterSection(FIsAdjustingSize)) then begin // non-reantrant
    try

      if isupdating then begin
        FAdjustSizeOnEndUpdate := True;
        Exit;
      end
      else
        FAdjustSizeOnEndUpdate := False;

      {$IF defined(debug)}
      //ALLog(ClassName + '.AdjustSize', 'Name: ' + Name + ' | HasUnconstrainedAutosize(X/Y) : '+ALBoolToStrW(HasUnconstrainedAutosizeWidth)+'/'+ALBoolToStrW(HasUnconstrainedAutosizeHeight));
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

      if HasUnconstrainedAutosizeHeight then begin

        var LLineHeight: Single := GetLineHeight;
        if AutoAlignToPixel then LLineHeight := ALAlignDimensionToPixelRound(LLineHeight, ALGetScreenScale, TEpsilon.Position);

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

      if AutoAlignToPixel then
        LMarginRect := ALAlignEdgesToPixelRound(LMarginRect, ALGetScreenScale, TEpsilon.Position);

      NativeViewMargins.Rect := LMarginRect;

    finally
      TNonReentrantHelper.LeaveSection(FIsAdjustingSize)
    end;
  end;
end;

{********************}
{$IF defined(android)}
Function TALDummyEdit.CreateNativeView: TALAndroidNativeView;
begin
  result := nil;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
Function TALDummyEdit.CreateNativeView: TALIosNativeView;
begin
  result := nil;
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
Function TALDummyEdit.CreateNativeView: TALMacNativeView;
begin
  result := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
Function TALDummyEdit.CreateNativeView: TALWinNativeView;
begin
  Result := nil;
end;
{$ENDIF}

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
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Edit','initialization');
  {$ENDIF}
  RegisterFmxClasses([TALEdit]);

end.
