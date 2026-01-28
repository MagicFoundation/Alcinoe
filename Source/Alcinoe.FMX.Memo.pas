unit Alcinoe.FMX.Memo;

interface

{$I Alcinoe.inc}

uses
  System.Types,
  system.Classes,
  System.UITypes,
  {$IF defined(android)}
  Alcinoe.FMX.NativeView.Android,
  {$ELSEIF defined(IOS)}
  System.TypInfo,
  iOSapi.Foundation,
  iOSapi.UIKit,
  Macapi.ObjectiveC,
  Alcinoe.FMX.NativeView.iOS,
  Alcinoe.FMX.BreakText,
  {$ELSEIF defined(ALMacOS)}
  System.TypInfo,
  Macapi.Foundation,
  Macapi.AppKit,
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  Macapi.CocoaTypes,
  Alcinoe.Macapi.AppKit,
  Alcinoe.FMX.NativeView.Mac,
  Alcinoe.FMX.BreakText,
  {$ELSEIF defined(MSWINDOWS)}
  Winapi.Messages,
  FMX.Controls.Win,
  Alcinoe.FMX.NativeView.Win,
  {$ENDIF}
  Fmx.controls,
  FMX.types,
  fmx.Graphics,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Edit,
  Alcinoe.FMX.Common;

Type

  {**************}
  TALMemo = class;

{$REGION ' IOS'}
{$IF defined(ios)}

  {************************************}
  IALIosMemoView = interface(UITextView)
    ['{EEF3FCE4-9755-48D7-B896-2C662EFDE9FC}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
  end;

  {****************************************}
  TALIosMemoView = class(TALIosBaseEditView)
  private
    type
      // -----------------
      // TTextViewDelegate
      TTextViewDelegate = class(TOCLocal, UITextViewDelegate)
      private
        FMemoView: TALIosMemoView;
      public
        constructor Create(const AMemoView: TALIosMemoView);
        function textView(textView: UITextView; shouldInteractWithURL: NSURL; inRange: NSRange; interaction: UITextItemInteraction): Boolean; overload; cdecl;
        function textView(textView: UITextView; writingToolsIgnoredRangesInEnclosingRange: NSRange): NSArray; overload; cdecl;
        procedure textView(textView: UITextView; textItemMenuWillDisplayForTextItem: UITextItem; animator: Pointer); overload; cdecl;
        function textView(textView: UITextView; menuConfigurationForTextItem: UITextItem; defaultMenu: UIMenu): UITextItemMenuConfiguration; overload; cdecl;
        function textView(textView: UITextView; primaryActionForTextItem: UITextItem; defaultAction: UIAction): UIAction; overload; cdecl;
        procedure textView(textView: UITextView; insertInputSuggestion: UIInputSuggestion); overload; cdecl;
        procedure textView(textView: UITextView; willBeginFormattingWithViewController: UITextFormattingViewController); overload; cdecl;
        function textView(textView: UITextView; shouldInteractWithTextAttachment: NSTextAttachment; inRange: NSRange): Boolean; overload; cdecl;
        function textView(textView: UITextView; shouldInteractWithURL: NSURL; inRange: NSRange): Boolean; overload; cdecl;
        function textView(textView: UITextView; shouldInteractWithTextAttachment: NSTextAttachment; inRange: NSRange; interaction: UITextItemInteraction): Boolean; overload; cdecl;
        function textView(textView: UITextView; shouldChangeTextInRange: NSRange; replacementText: NSString): Boolean; overload; cdecl;
        function textView(textView: UITextView; editMenuForTextInRange: NSRange; suggestedActions: NSArray): UIMenu; overload; cdecl;
        procedure textView(textView: UITextView; willPresentEditMenuWithAnimator: Pointer); overload; cdecl;
        procedure textViewDidBeginEditing(textView: UITextView); cdecl;
        [MethodName('textView:didBeginFormattingWithViewController:')]
        procedure textViewDidBeginFormattingWithViewController(textView: UITextView; didBeginFormattingWithViewController: UITextFormattingViewController); cdecl;
        procedure textViewDidChange(textView: UITextView); cdecl;
        procedure textViewDidChangeSelection(textView: UITextView); cdecl;
        procedure textViewDidEndEditing(textView: UITextView); cdecl;
        [MethodName('textView:didEndFormattingWithViewController:')]
        procedure textViewDidEndFormattingWithViewController(textView: UITextView; didEndFormattingWithViewController: UITextFormattingViewController); cdecl;
        function textViewShouldBeginEditing(textView: UITextView): Boolean; cdecl;
        function textViewShouldEndEditing(textView: UITextView): Boolean; cdecl;
        [MethodName('textView:textItemMenuWillEndForTextItem:animator:')]
        procedure textViewTextItemMenuWillEndForTextItem(textView: UITextView; textItemMenuWillEndForTextItem: UITextItem; animator: Pointer); cdecl;
        [MethodName('textView:willDismissEditMenuWithAnimator:')]
        procedure textViewWillDismissEditMenuWithAnimator(textView: UITextView; willDismissEditMenuWithAnimator: Pointer); cdecl;
        [MethodName('textView:willEndFormattingWithViewController:')]
        procedure textViewWillEndFormattingWithViewController(textView: UITextView; willEndFormattingWithViewController: UITextFormattingViewController); cdecl;
        procedure textViewWritingToolsDidEnd(textView: UITextView); cdecl;
        procedure textViewWritingToolsWillBegin(textView: UITextView); cdecl;
      end;
  private
    FTextViewDelegate: TTextViewDelegate;
    FPlaceholderLabel: UILabel;
    FTextSettings: TALBaseTextSettings;
    FFillColor: TAlphaColor;
    fMaxLength: integer;
    fPromptTextColor: TalphaColor;
    function GetView: UITextView;
    function GetControl: TALMemo;
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
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); override; cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); override; cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); override; cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); override; cdecl;
    property View: UITextView read GetView;
    property Control: TALMemo read GetControl;
    procedure SetEnabled(const value: Boolean); override;
    function getLineCount: integer; override;
    function getLineHeight: Single; override; // It includes the line spacing
    Procedure SetSelection(const AStart: integer; const AStop: Integer); overload; override;
    Procedure SetSelection(const AIndex: integer); overload; override;
    function getSelectionStart: Integer; override;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}

  {**************************************}
  IALMacMemoView = interface(NSScrollView)
    ['{00A5F139-8A09-43CA-AF31-05B3480DD657}']
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
  end;

  {****************************************}
  TALMacMemoView = class(TALMacBaseEditView)
  private
    type
      // ---------
      // ITextView
      ITextView = interface(NSTextView)
        ['{25DE9B6D-7F5F-462B-A785-145102C9D168}']
        function acceptsFirstResponder: Boolean; cdecl;
        function becomeFirstResponder: Boolean; cdecl;
      end;
      // ---------
      // TTextView
      TTextView = class(TOCLocal)
      private
        FMemoView: TALMacMemoView;
        function GetView: NSTextView;
      protected
        function GetObjectiveCClass: PTypeInfo; override;
      public
        constructor Create(const AMemoView: TALMacMemoView); Virtual;
        function acceptsFirstResponder: Boolean; cdecl;
        function becomeFirstResponder: Boolean; cdecl;
        property View: NSTextView read GetView;
      end;
      // -----------------
      // TTextViewDelegate
      TTextViewDelegate = class(TOCLocal, NSTextViewDelegate)
      private
        FMemoView: TALMacMemoView;
      public
        constructor Create(const AMemoView: TALMacMemoView);
        function textShouldBeginEditing(textObject: NSText): Boolean; cdecl;
        function textShouldEndEditing(textObject: NSText): Boolean; cdecl;
        procedure textDidBeginEditing(notification: NSNotification); cdecl;
        procedure textDidEndEditing(notification: NSNotification); cdecl;
        procedure textDidChange(notification: NSNotification); cdecl;
        [MethodName('textView:shouldChangeTextInRange:replacementString:')]
        function textViewShouldChangeTextInRangeReplacementString(textView: NSTextView; shouldChangeTextInRange: NSRange; replacementString: NSString): boolean; cdecl;
      end;
      // ------------
      // IPlaceHolder
      IPlaceHolder = interface(NSTextField)
        ['{72231FBB-4D10-4463-8DF3-90BFC7E55AA6}']
        function acceptsFirstResponder: Boolean; cdecl;
      end;
      // ------------
      // TPlaceHolder
      TPlaceHolder = class(TOCLocal)
      private
        FMemoView: TALMacMemoView;
        function GetView: NSTextField;
      protected
        function GetObjectiveCClass: PTypeInfo; override;
      public
        constructor Create(const AMemoView: TALMacMemoView); Virtual;
        function acceptsFirstResponder: Boolean; cdecl;
        property View: NSTextField read GetView;
      end;
  private
    FTextView: TTextView;
    FTextViewDelegate: TTextViewDelegate;
    FPlaceholderLabel: TPlaceHolder;
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
    function GetView: NSScrollView;
    function GetControl: TALMemo;
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
    property View: NSScrollView read GetView;
    property Control: TALMemo read GetControl;
    procedure SetEnabled(const value: Boolean); override;
    function getLineCount: integer; override;
    function getLineHeight: Single; override; // It includes the line spacing
    Procedure SetSelection(const AStart: integer; const AStop: Integer); overload; override;
    Procedure SetSelection(const AIndex: integer); overload; override;
    function getSelectionStart: Integer; override;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

  {************************************}
  TALWinMemoView = class(TALWinEditView)
  private
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    function getLineCount: integer; override;
  end;

{$endif}
{$ENDREGION}

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALMemo = class(TALBaseEdit)
  public
    type
      // -------------
      // TTextSettings
      TTextSettings = class(TALBaseEdit.TTextSettings)
      protected
        function GetDefaultVertAlign: TALTextVertAlign; override;
      published
        property LineHeightMultiplier;
      end;
      // -------------------
      // TDisabledStateStyle
      TDisabledStateStyle = class(TALBaseEdit.TDisabledStateStyle)
      public
        type
          TTextSettings = class(TALBaseEdit.TDisabledStateStyle.TTextSettings)
          protected
            function GetDefaultVertAlign: TALTextVertAlign; override;
          end;
      protected
        function CreateTextSettings(const AParent: TALBaseTextSettings): TALBaseEdit.TBaseStateStyle.TTextSettings; override;
      end;
      // ------------------
      // THoveredStateStyle
      THoveredStateStyle = class(TALBaseEdit.THoveredStateStyle)
      public
        type
          TTextSettings = class(TALBaseEdit.THoveredStateStyle.TTextSettings)
          protected
            function GetDefaultVertAlign: TALTextVertAlign; override;
          end;
      protected
        function CreateTextSettings(const AParent: TALBaseTextSettings): TALBaseEdit.TBaseStateStyle.TTextSettings; override;
      end;
      // ------------------
      // TFocusedStateStyle
      TFocusedStateStyle = class(TALBaseEdit.TFocusedStateStyle)
      public
        type
          TTextSettings = class(TALBaseEdit.TFocusedStateStyle.TTextSettings)
          protected
            function GetDefaultVertAlign: TALTextVertAlign; override;
          end;
      protected
        function CreateTextSettings(const AParent: TALBaseTextSettings): TALBaseEdit.TBaseStateStyle.TTextSettings; override;
      end;
      // ------------
      // TStateStyles
      TStateStyles = class(TALBaseEdit.TStateStyles)
      protected
        function CreateDisabledStateStyle(const AParent: TObject): TALBaseEdit.TDisabledStateStyle; override;
        function CreateHoveredStateStyle(const AParent: TObject): TALBaseEdit.THoveredStateStyle; override;
        function CreateFocusedStateStyle(const AParent: TObject): TALBaseEdit.TFocusedStateStyle; override;
      end;
  private
    FAutosizeLineCount: Integer;
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
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
    function GetDefaultSize: TSizeF; override;
    function GetAutoSize: TALAutoSizeMode; override;
    procedure SetAutosizeLineCount(const Value: Integer); virtual;
    function CreateStateStyles: TALBaseEdit.TStateStyles; override;
    function CreateTextSettings: TALBaseEdit.TTextSettings; override;
    procedure AdjustSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    function HasUnconstrainedAutosizeWidth: Boolean; override;
  published
    property AutoSizeLineCount: Integer read FAutosizeLineCount write SetAutosizeLineCount Default 0;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
  end;

  {***************************}
  TALDummyMemo = class(TALMemo)
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
  {$IF defined(IOS)}
  Macapi.CoreFoundation,
  iOSapi.CocoaTypes,
  Macapi.Helpers,
  FMX.Platform.iOS,
  FMX.Helpers.iOS,
  FMX.Consts,
  {$ELSEIF defined(ALMacOS)}
  system.Character,
  Macapi.CoreFoundation,
  Macapi.Helpers,
  Macapi.CoreText,
  FMX.Helpers.Mac,
  FMX.Consts,
  {$ELSEIF defined(MSWINDOWS)}
  Winapi.Windows,
  {$endif}
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  Alcinoe.StringUtils,
  Alcinoe.FMX.Graphics,
  Alcinoe.Common;

{$REGION ' IOS'}
{$IF defined(ios)}

{***********************************************************************************}
constructor TALIosMemoView.TTextViewDelegate.Create(const AMemoView: TALIosMemoView);
begin
  inherited Create;
  FMemoView := AMemoView;
end;

{********************************************************************************************************************************************************************}
function TALIosMemoView.TTextViewDelegate.textView(textView: UITextView; shouldInteractWithURL: NSURL; inRange: NSRange; interaction: UITextItemInteraction): Boolean;
begin
  Result := True;
end;

{************************************************************************************************************************************}
function TALIosMemoView.TTextViewDelegate.textView(textView: UITextView; writingToolsIgnoredRangesInEnclosingRange: NSRange): NSArray;
begin
  Result := TNSArray.Wrap(TNSArray.OCClass.&array);
end;

{*******************************************************************************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textView(textView: UITextView; textItemMenuWillDisplayForTextItem: UITextItem; animator: Pointer);
begin
end;

{*******************************************************************************************************************************************************************}
function TALIosMemoView.TTextViewDelegate.textView(textView: UITextView; menuConfigurationForTextItem: UITextItem; defaultMenu: UIMenu): UITextItemMenuConfiguration;
begin
  result := nil;
end;

{************************************************************************************************************************************************}
function TALIosMemoView.TTextViewDelegate.textView(textView: UITextView; primaryActionForTextItem: UITextItem; defaultAction: UIAction): UIAction;
begin
  result := nil;
end;

{******************************************************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textView(textView: UITextView; insertInputSuggestion: UIInputSuggestion);
begin
end;

{***********************************************************************************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textView(textView: UITextView; willBeginFormattingWithViewController: UITextFormattingViewController);
begin
end;

{******************************************************************************************************************************************************}
function TALIosMemoView.TTextViewDelegate.textView(textView: UITextView; shouldInteractWithTextAttachment: NSTextAttachment; inRange: NSRange): Boolean;
begin
  Result := True;
end;

{********************************************************************************************************************************}
function TALIosMemoView.TTextViewDelegate.textView(textView: UITextView; shouldInteractWithURL: NSURL; inRange: NSRange): Boolean;
begin
  Result := True;
end;

{******************************************************************************************************************************************************************************************}
function TALIosMemoView.TTextViewDelegate.textView(textView: UITextView; shouldInteractWithTextAttachment: NSTextAttachment; inRange: NSRange; interaction: UITextItemInteraction): Boolean;
begin
  Result := True;
end;

{*********************************************************************************************************************************************}
function TALIosMemoView.TTextViewDelegate.textView(textView: UITextView; shouldChangeTextInRange: NSRange; replacementText: NSString): Boolean;
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.textView:shouldChangeTextInRange:replacementText');
  {$ENDIF}
  if FMemoView.maxLength > 0 then begin
    var LText: NSString := textView.text;
    if shouldChangeTextInRange.length + shouldChangeTextInRange.location > LText.length then exit(false);
    result := LText.length + replacementText.length - shouldChangeTextInRange.length <= NSUInteger(FMemoView.maxLength);
  end
  else Result := True;
end;

{*******************************************************************************************************************************************}
function TALIosMemoView.TTextViewDelegate.textView(textView: UITextView; editMenuForTextInRange: NSRange; suggestedActions: NSArray): UIMenu;
begin
  // Return nil to present the default system menu.
  Result := nil;
end;

{******************************************************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textView(textView: UITextView; willPresentEditMenuWithAnimator: Pointer);
begin
end;

{***************************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textViewDidBeginEditing(textView: UITextView);
begin
end;

{**********************************************************************************************************************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textViewDidBeginFormattingWithViewController(textView: UITextView; didBeginFormattingWithViewController: UITextFormattingViewController);
begin
end;

{*********************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textViewDidChange(textView: UITextView);
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.textViewDidChange');
  {$ENDIF}
  FMemoView.DoChange;
end;

{******************************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textViewDidChangeSelection(textView: UITextView);
begin
end;

{*************************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textViewDidEndEditing(textView: UITextView);
begin
  FMemoView.Control.ResetFocus;
end;

{******************************************************************************************************************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textViewDidEndFormattingWithViewController(textView: UITextView; didEndFormattingWithViewController: UITextFormattingViewController);
begin
end;

{**************************************************************************************************}
function TALIosMemoView.TTextViewDelegate.textViewShouldBeginEditing(textView: UITextView): Boolean;
begin
  Result := True;
end;

{************************************************************************************************}
function TALIosMemoView.TTextViewDelegate.textViewShouldEndEditing(textView: UITextView): Boolean;
begin
  Result := True;
end;

{*********************************************************************************************************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textViewTextItemMenuWillEndForTextItem(textView: UITextView; textItemMenuWillEndForTextItem: UITextItem; animator: Pointer);
begin
end;

{*************************************************************************************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textViewWillDismissEditMenuWithAnimator(textView: UITextView; willDismissEditMenuWithAnimator: Pointer);
begin
end;

{********************************************************************************************************************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textViewWillEndFormattingWithViewController(textView: UITextView; willEndFormattingWithViewController: UITextFormattingViewController);
begin
end;

{******************************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textViewWritingToolsDidEnd(textView: UITextView);
begin
end;

{*********************************************************************************************}
procedure TALIosMemoView.TTextViewDelegate.textViewWritingToolsWillBegin(textView: UITextView);
begin
end;

{********************************}
constructor TALIosMemoView.Create;
begin
  inherited; // This will call InitView
  View.setbackgroundColor(TUIColor.OCClass.clearColor);
  var LUIEdgeInsets: UIEdgeInsets;
  LUIEdgeInsets.top := 0;
  LUIEdgeInsets.left := 0;
  LUIEdgeInsets.bottom := 0;
  LUIEdgeInsets.right := 0;
  View.setTextContainerInset(LUIEdgeInsets);
  View.textContainer.setLineFragmentPadding(0);
  FPlaceholderLabel := TUILabel.Wrap(TUILabel.Alloc.initWithFrame(RectToNSRect(TRect.Create(0,0,0,0))));
  FPlaceholderLabel.setHidden(True);
  View.addSubview(FPlaceholderLabel);
  FTextViewDelegate := TTextViewDelegate.Create(Self);
  View.setDelegate(FTextViewDelegate.GetObjectID);
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
end;

{********************************}
destructor TALIosMemoView.Destroy;
begin
  View.setDelegate(nil);
  ALFreeAndNil(FTextViewDelegate);
  FPlaceholderLabel.removeFromSuperview;
  FPlaceholderLabel.release;
  ALFreeAndNil(FTextSettings);
  inherited Destroy;
end;

{********************************************************}
procedure TALIosMemoView.SetEnabled(const value: Boolean);
begin
  inherited;
  View.setEditable(value);
  View.setSelectable(value);
end;

{************************************************************************}
procedure TALIosMemoView.touchesBegan(touches: NSSet; withEvent: UIEvent);
begin
  if (getLineCount < Control.GetNativeViewHeight / getLineHeight) then
    inherited;
end;

{****************************************************************************}
procedure TALIosMemoView.touchesCancelled(touches: NSSet; withEvent: UIEvent);
begin
  if (getLineCount < Control.GetNativeViewHeight / getLineHeight) then
    inherited;
end;

{************************************************************************}
procedure TALIosMemoView.touchesEnded(touches: NSSet; withEvent: UIEvent);
begin
  if (getLineCount < Control.GetNativeViewHeight / getLineHeight) then
    inherited;
end;

{************************************************************************}
procedure TALIosMemoView.touchesMoved(touches: NSSet; withEvent: UIEvent);
begin
  if (getLineCount < Control.GetNativeViewHeight / getLineHeight) then
    inherited;
end;

{****************************************************}
function TALIosMemoView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALIosMemoView);
end;

{******************************************}
function TALIosMemoView.GetView: UITextView;
begin
  Result := inherited GetView<UITextView>;
end;

{******************************************}
function TALIosMemoView.GetControl: TALMemo;
begin
  Result := TALMemo(inherited Control);
end;

{**************************************************************************}
procedure TALIosMemoView.SetKeyboardType(const Value: TVirtualKeyboardType);
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
function TALIosMemoView.GetKeyboardType: TVirtualKeyboardType;
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
procedure TALIosMemoView.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
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
function TALIosMemoView.GetAutoCapitalizationType: TALAutoCapitalizationType;
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
procedure TALIosMemoView.SetPassword(const Value: Boolean);
begin
  View.setSecureTextEntry(Value);
end;

{*******************************************}
function TALIosMemoView.GetPassword: Boolean;
begin
  result := View.isSecureTextEntry;
end;

{**************************************************************}
procedure TALIosMemoView.SetCheckSpelling(const Value: Boolean);
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
function TALIosMemoView.GetCheckSpelling: Boolean;
begin
  result := View.SpellCheckingType = UITextSpellCheckingTypeYes;
end;

{*********************************************************************}
procedure TALIosMemoView.setReturnKeyType(const Value: TReturnKeyType);
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
function TALIosMemoView.GetReturnKeyType: TReturnKeyType;
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
function TALIosMemoView.GetPromptText: String;
begin
  Result := NSStrToStr(FPlaceholderLabel.text);
end;

{**********************************************************}
procedure TALIosMemoView.setPromptText(const Value: String);
begin
  FPlaceholderLabel.setText(StrToNSStr(Value));
  FPlaceholderLabel.setHidden((not Text.IsEmpty) or (Value.IsEmpty));
  TextSettingsChanged(nil);
end;

{******************************************************}
function TALIosMemoView.GetPromptTextColor: TAlphaColor;
begin
  Result := fPromptTextColor;
end;

{********************************************************************}
procedure TALIosMemoView.setPromptTextColor(const Value: TAlphaColor);
begin
  if Value <> fPromptTextColor then begin
    fPromptTextColor := Value;
    TextSettingsChanged(nil);
  end;
end;

{************************************************}
function TALIosMemoView.GetTintColor: TAlphaColor;
begin
  var red: CGFloat;
  var green: CGFloat;
  var blue: CGFloat;
  var alpha: CGFloat;
  if not View.tintColor.getRed(@red, @green, @blue, @alpha) then result := TalphaColors.Null
  else result := TAlphaColorF.Create(red, green, blue, alpha).ToAlphaColor;
end;

{**************************************************************}
procedure TALIosMemoView.setTintColor(const Value: TAlphaColor);
begin
  if Value <> TalphaColors.Null then
    View.setTintColor(AlphaColorToUIColor(Value));
end;

{**************************************}
function TALIosMemoView.getText: String;
begin
  result := NSStrToStr(View.text);
end;

{****************************************************}
procedure TALIosMemoView.SetText(const Value: String);
begin
  if Value <> getText then begin
    View.setText(StrToNSStr(Value));
    DoChange;
  end;
end;

{********************************************}
function TALIosMemoView.GetMaxLength: integer;
begin
  Result := FMaxLength;
end;

{**********************************************************}
procedure TALIosMemoView.SetMaxLength(const Value: integer);
begin
  FMaxLength := Value;
end;

{***********************************************************}
function TALIosMemoView.GetTextSettings: TALBaseTextSettings;
begin
  result := FTextSettings;
end;

{*************************************************************************}
procedure TALIosMemoView.SetTextSettings(const Value: TALBaseTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{************************************************************}
procedure TALIosMemoView.TextSettingsChanged(Sender: TObject);
begin
  var LFontRef := ALCreateCTFontRef(ALResolveFontFamily(TextSettings.Font.Family), TextSettings.Font.Size, TextSettings.Font.Weight, TextSettings.Font.Slant);
  try

    // LineHeightMultiplier
    var LTmpLineHeightMultiplier: Single := ALResolveLineHeightMultiplier(textsettings.Font.Size, textsettings.LineHeightMultiplier);
    if CompareValue(LTmpLineHeightMultiplier, 0, TEpsilon.Scale) > 0 then begin
      var LParagraphStyle: NSMutableParagraphStyle := TNSMutableParagraphStyle.Alloc;
      try
        LParagraphStyle.init;
        // To mirror Skia’s behavior, the line height is calculated as: lineHeight = fontSize * LineHeightMultiplier
        LParagraphStyle.setMinimumLineHeight(textsettings.Font.Size * LTmpLineHeightMultiplier);
        LParagraphStyle.setMaximumLineHeight(textsettings.Font.Size * LTmpLineHeightMultiplier);

        var LObjects := TNSMutableArray.Create;
        var LForKeys := TNSMutableArray.Create;
        try
          LObjects.addObject(NSObjectToID(LParagraphStyle));
          LForKeys.addObject(NSObjectToID(NSParagraphStyleAttributeName));

          // If we call NativeView.View.setTypingAttributes before NativeView.View.setFont,
          // the font specified in NativeView.View.setFont will take precedence.
          // LObjects.addObject(LFontRef);
          // LForKeys.addObject(NSObjectToID(NSFontAttributeName));

          var LDictionary := TNSDictionary.Wrap(TNSDictionary.OCClass.dictionaryWithObjects(LObjects, LForKeys));
          // https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/MemoryMgmt/Articles/mmRules.html
          // No release required for LDictionary because it wasn’t created via a method whose name starts with “alloc”, “new”, “copy”, or “mutableCopy”.
          View.setTypingAttributes(LDictionary);
        finally
          LObjects.release;
          LForKeys.release;
        end;
      finally
        LParagraphStyle.release;
      end;
    end
    else
      View.setTypingAttributes(nil);

    // Font
    View.setFont(TUIFont.Wrap(LFontRef));

    // TextAlignment
    View.setTextAlignment(ALTextHorzAlignToNSTextAlignment(TextSettings.HorzAlign));

    // TextColor
    View.setTextColor(AlphaColorToUIColor(TextSettings.Font.Color));

    // PlaceHolder Text Color
    var LUIColor: UIColor;
    if PromptTextColor <> tAlphaColors.Null then LUIColor := AlphaColorToUIColor(PromptTextColor)
    else LUIColor := AlphaColorToUIColor(
                       TAlphaColorF.Create(
                         ALSetColorAlpha(TextSettings.Font.Color, 0.5)).
                           PremultipliedAlpha.
                           ToAlphaColor);
    FPlaceholderLabel.setTextColor(LUIColor);

    // PlaceHolder Font
    FPlaceholderLabel.setfont(TUIFont.Wrap(LFontRef));
    FPlaceholderLabel.sizeToFit;

  finally
    CFRelease(LFontRef);
  end;
end;

{********************************}
procedure TALIosMemoView.DoChange;
begin
  FPlaceholderLabel.setHidden((not Text.IsEmpty) or (PromptText.IsEmpty));
  Control.DoChange;
end;

{************************************************}
function TALIosMemoView.GetFillColor: TAlphaColor;
begin
  Result := FFillColor;
end;

{**************************************************************}
procedure TALIosMemoView.SetFillColor(const Value: TAlphaColor);
begin
  FFillColor := Value;
end;

{********************************************}
function TALIosMemoView.getLineCount: integer;
begin
  Result := 0;
  var LlayoutManager := view.layoutManager;
  var LnumberOfGlyphs := LlayoutManager.numberOfGlyphs;
  var LlineRange: NSRange;
  var LIndex: Integer := 0;
  while Lindex < LNumberOfGlyphs do begin
    // Get the range of the current line in the text view.
    LlayoutManager.lineFragmentRectForGlyphAtIndex(Lindex, @LlineRange);
    LIndex := NSMaxRange(LlineRange);
    inc(Result); // Increment the line count for each line fragment
  end;
  var LText := GetText;
  if (LText <> '') and (LText[high(LText)] = #10) then inc(result);
  if Result = 0 then result := 1;
end;

{********************************************}
function TALIosMemoView.getLineHeight: Single;
begin
  var LTmpLineHeightMultiplier: Single := ALResolveLineHeightMultiplier(textsettings.font.size, textsettings.LineHeightMultiplier);
  if CompareValue(LTmpLineHeightMultiplier, 0, TEpsilon.Scale) > 0 then result := textsettings.Font.Size * LTmpLineHeightMultiplier
  else begin
    if View.font = nil then TextSettingsChanged(nil);
    result := View.font.lineHeight;
  end;
end;

{*********************************************************************************}
Procedure TALIosMemoView.setSelection(const AStart: integer; const AStop: Integer);
begin
  View.setSelectedRange(NSMakeRange(AStart, AStop-AStart));
end;

{***********************************************************}
Procedure TALIosMemoView.setSelection(const AIndex: integer);
begin
  View.setSelectedRange(NSMakeRange(AIndex, 0));
end;

{*************************************************}
function TALIosMemoView.getSelectionStart: Integer;
begin
  Result := Integer(View.selectedRange.location);
end;

{$endif}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}

{***************************************************************************}
constructor TALMacMemoView.TTextView.Create(const AMemoView: TALMacMemoView);
begin
  inherited Create;
  FMemoView := AMemoView;
end;

{****************************************************}
function TALMacMemoView.TTextView.GetView: NSTextView;
begin
  Result := NSTextView(Super);
end;

{**************************************************************}
function TALMacMemoView.TTextView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(ITextView);
end;

{***************************************************************}
function TALMacMemoView.TTextView.acceptsFirstResponder: Boolean;
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.acceptsFirstResponder', 'control.name: ' + FScrollView.Control.Name);
  {$ENDIF}
  Result := NSTextView(Super).acceptsFirstResponder and FMemoView.Control.canFocus;
end;

{**************************************************************}
function TALMacMemoView.TTextView.becomeFirstResponder: Boolean;
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.becomeFirstResponder', 'control.name: ' + FScrollView.Control.Name);
  {$ENDIF}
  Result := NSTextView(Super).becomeFirstResponder;
  if (not FMemoView.Control.IsFocused) then
    FMemoView.Control.SetFocus;
end;

{***********************************************************************************}
constructor TALMacMemoView.TTextViewDelegate.Create(const AMemoView: TALMacMemoView);
begin
  inherited Create;
  FMemoView := AMemoView;
end;

{*******************************************************************************************}
procedure TALMacMemoView.TTextViewDelegate.textDidBeginEditing(notification: NSNotification);
begin
end;

{*************************************************************************************}
procedure TALMacMemoView.TTextViewDelegate.textDidChange(notification: NSNotification);
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.textDidChange');
  {$ENDIF}
  FMemoView.DoChange;
end;

{*****************************************************************************************}
procedure TALMacMemoView.TTextViewDelegate.textDidEndEditing(notification: NSNotification);
begin
  FMemoView.Control.ResetFocus;
end;

{********************************************************************************************}
function TALMacMemoView.TTextViewDelegate.textShouldBeginEditing(textObject: NSText): Boolean;
begin
  Result := True;
end;

{***************************************************************************************************************************************************************************************}
function TALMacMemoView.TTextViewDelegate.textViewShouldChangeTextInRangeReplacementString(textView: NSTextView; shouldChangeTextInRange: NSRange; replacementString: NSString): boolean;
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.textViewShouldChangeTextInRangeReplacementString');
  {$ENDIF}
  if FMemoView.maxLength > 0 then begin
    var LText: NSString := textView.&String;
    if shouldChangeTextInRange.length + shouldChangeTextInRange.location > LText.length then exit(false);
    result := LText.length + replacementString.length - shouldChangeTextInRange.length <= NSUInteger(FMemoView.maxLength);
  end
  else Result := True;
end;

{******************************************************************************************}
function TALMacMemoView.TTextViewDelegate.textShouldEndEditing(textObject: NSText): Boolean;
begin
  Result := True;
end;

{******************************************************************************}
constructor TALMacMemoView.TPlaceHolder.Create(const AMemoView: TALMacMemoView);
begin
  inherited Create;
  FMemoView := AMemoView;
end;

{********************************************************}
function TALMacMemoView.TPlaceHolder.GetView: NSTextField;
begin
  Result := NSTextField(Super);
end;

{*****************************************************************}
function TALMacMemoView.TPlaceHolder.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IPlaceHolder);
end;

{******************************************************************}
function TALMacMemoView.TPlaceHolder.acceptsFirstResponder: Boolean;
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.acceptsFirstResponder', 'control.name: ' + FScrollView.Control.Name);
  {$ENDIF}
  Result := False;
  FMemoView.View.becomeFirstResponder;
end;

{********************************}
constructor TALMacMemoView.Create;
begin
  inherited; // This will call InitView
  //--
  View.setDrawsBackground(false);
  View.setFocusRingType(NSFocusRingTypeNone);
  //--
  var LContentSize := View.contentSize;
  FTextView := TTextView.Create(Self);
  FTextView.View.initWithFrame(RectToNSRect(TRectF.Create(0,0,LContentSize.width,LContentSize.height).round));
  FTextView.View.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
  FTextView.View.setDrawsBackground(false);
  FTextView.View.setFocusRingType(NSFocusRingTypeNone);
  var LNSContainerInset: NSSize;
  LNSContainerInset.Width := 0;
  LNSContainerInset.Height := 0;
  FTextView.View.setTextContainerInset(LNSContainerInset);
  FTextView.View.textContainer.setLineFragmentPadding(0);
  //--
  FTextViewDelegate := TTextViewDelegate.Create(Self);
  FTextView.View.setDelegate(FTextViewDelegate.GetObjectID);
  //--
  FPlaceholderLabel := TPlaceHolder.Create(Self);
  FPlaceholderLabel.view.initWithFrame(RectToNSRect(TRect.Create(0,0,0,0)));
  FPlaceholderLabel.view.setEditable(False);
  FPlaceholderLabel.view.setSelectable(False);
  FPlaceholderLabel.view.setBordered(False);
  FPlaceholderLabel.view.setDrawsBackground(false);
  FPlaceholderLabel.view.setHidden(True);
  //--
  FTextView.View.addSubview(FPlaceholderLabel.view);
  View.addSubview(FTextView.View);
  view.setdocumentView(FTextView.View);
  //--
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
destructor TALMacMemoView.Destroy;
begin
  FPlaceholderLabel.View.removeFromSuperview;
  ALFreeAndNil(FPlaceholderLabel);
  FTextView.View.setDelegate(nil);
  ALFreeAndNil(FTextViewDelegate);
  FTextView.View.removeFromSuperview;
  ALFreeAndNil(FTextView);
  ALFreeAndNil(FTextSettings);
  inherited;
end;

{********************************************************}
procedure TALMacMemoView.SetEnabled(const value: Boolean);
begin
  inherited;
  FTextView.View.setEditable(value);
  FTextView.View.setSelectable(value);
end;

{****************************************************}
function TALMacMemoView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALMacMemoView);
end;

{********************************************}
function TALMacMemoView.GetView: NSScrollView;
begin
  Result := inherited GetView<NSScrollView>;
end;

{******************************************}
function TALMacMemoView.GetControl: TALMemo;
begin
  Result := TALMemo(inherited Control);
end;

{**************************************************************************}
procedure TALMacMemoView.SetKeyboardType(const Value: TVirtualKeyboardType);
begin
  FKeyboardType := Value;
end;

{************************************************************}
function TALMacMemoView.GetKeyboardType: TVirtualKeyboardType;
begin
  Result := FKeyboardType;
end;

{*****************************************************************************************}
procedure TALMacMemoView.setAutoCapitalizationType(const Value: TALAutoCapitalizationType);
begin
  FAutoCapitalizationType := Value;
end;

{***************************************************************************}
function TALMacMemoView.GetAutoCapitalizationType: TALAutoCapitalizationType;
begin
  Result := FAutoCapitalizationType;
end;

{*********************************************************}
procedure TALMacMemoView.SetPassword(const Value: Boolean);
begin
  FPassword := Value;
end;

{*******************************************}
function TALMacMemoView.GetPassword: Boolean;
begin
  Result := FPassword;
end;

{**************************************************************}
procedure TALMacMemoView.SetCheckSpelling(const Value: Boolean);
begin
  FCheckSpelling := Value;
end;

{************************************************}
function TALMacMemoView.GetCheckSpelling: Boolean;
begin
  Result := FCheckSpelling;
end;

{*********************************************************************}
procedure TALMacMemoView.setReturnKeyType(const Value: TReturnKeyType);
begin
  FReturnKeyType := Value;
end;

{*******************************************************}
function TALMacMemoView.GetReturnKeyType: TReturnKeyType;
begin
  Result := FReturnKeyType;
end;

{********************************************}
function TALMacMemoView.GetPromptText: String;
begin
  Result := NSStrToStr(FPlaceholderLabel.view.stringValue);
end;

{**********************************************************}
procedure TALMacMemoView.setPromptText(const Value: String);
begin
  FPlaceholderLabel.view.setStringValue(StrToNSStr(Value));
  FPlaceholderLabel.view.setHidden((not Text.IsEmpty) or (Value.IsEmpty));
  TextSettingsChanged(nil);
end;

{******************************************************}
function TALMacMemoView.GetPromptTextColor: TAlphaColor;
begin
  Result := fPromptTextColor;
end;

{********************************************************************}
procedure TALMacMemoView.setPromptTextColor(const Value: TAlphaColor);
begin
  if Value <> fPromptTextColor then begin
    fPromptTextColor := Value;
    TextSettingsChanged(nil);
  end;
end;

{************************************************}
function TALMacMemoView.GetTintColor: TAlphaColor;
begin
  Result := FTintColor;
end;

{**************************************************************}
procedure TALMacMemoView.setTintColor(const Value: TAlphaColor);
begin
  FTintColor := Value;
end;

{**************************************}
function TALMacMemoView.getText: String;
begin
  result := NSStrToStr(FtextView.View.&String);
end;

{****************************************************}
procedure TALMacMemoView.SetText(const Value: String);
begin
  if Value <> getText then begin
    FtextView.View.setString(StrToNSStr(Value));
    DoChange;
  end;
end;

{********************************************}
function TALMacMemoView.GetMaxLength: integer;
begin
  Result := FMaxLength;
end;

{**********************************************************}
procedure TALMacMemoView.SetMaxLength(const Value: integer);
begin
  FMaxLength := Value;
end;

{***********************************************************}
function TALMacMemoView.GetTextSettings: TALBaseTextSettings;
begin
  result := FTextSettings;
end;

{*************************************************************************}
procedure TALMacMemoView.SetTextSettings(const Value: TALBaseTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{************************************************************}
procedure TALMacMemoView.TextSettingsChanged(Sender: TObject);
begin
  var LFontRef := ALCreateCTFontRef(ALResolveFontFamily(TextSettings.Font.Family), TextSettings.Font.Size, TextSettings.Font.Weight, TextSettings.Font.Slant);
  try

    // LineHeightMultiplier
    var LTmpLineHeightMultiplier: Single := ALResolveLineHeightMultiplier(TextSettings.Font.Size, textsettings.LineHeightMultiplier);
    if CompareValue(LTmpLineHeightMultiplier, 0, TEpsilon.Scale) > 0 then begin
      var LParagraphStyle: NSMutableParagraphStyle := TNSMutableParagraphStyle.Alloc;
      try
        LParagraphStyle.init;
        // To mirror Skia’s behavior, the line height is calculated as: lineHeight = fontSize * LineHeightMultiplier
        LParagraphStyle.setMinimumLineHeight(textsettings.Font.Size * LTmpLineHeightMultiplier);
        LParagraphStyle.setMaximumLineHeight(textsettings.Font.Size * LTmpLineHeightMultiplier);

        var LObjects := TNSMutableArray.Create;
        var LForKeys := TNSMutableArray.Create;
        try
          LObjects.addObject(NSObjectToID(LParagraphStyle));
          LForKeys.addObject(NSObjectToID(NSParagraphStyleAttributeName));

          // If we call NativeView.View.setTypingAttributes before NativeView.View.setFont,
          // the font specified in NativeView.View.setFont will take precedence.
          // LObjects.addObject(LFontRef);
          // LForKeys.addObject(NSObjectToID(NSFontAttributeName));

          var LDictionary := TNSDictionary.Wrap(TNSDictionary.OCClass.dictionaryWithObjects(LObjects, LForKeys));
          // https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/MemoryMgmt/Articles/mmRules.html
          // No release required for LDictionary because it wasn’t created via a method whose name starts with “alloc”, “new”, “copy”, or “mutableCopy”.
          FTextView.View.setTypingAttributes(LDictionary);
        finally
          LObjects.release;
          LForKeys.release;
        end;
      finally
        LParagraphStyle.release;
      end;
    end
    else
      FTextView.View.setTypingAttributes(nil);

    // Font
    FTextView.View.setFont(TNSFont.Wrap(LFontRef));

    // TextAlignment
    var LRange: NSRange;
    LRange.location := 0;
    LRange.length := GetText.Length;
    FTextView.View.setAlignment(ALTextHorzAlignToNSTextAlignment(TextSettings.HorzAlign), LRange);

    // TextColor
    FTextView.View.setTextColor(AlphaColorToNSColor(TextSettings.Font.Color));

    // PlaceHolder Text Color
    var LNSColor: NSColor;
    if PromptTextColor <> tAlphaColors.Null then LNSColor := AlphaColorToNSColor(PromptTextColor)
    else LNSColor := AlphaColorToNSColor(
                       TAlphaColorF.Create(
                         ALSetColorAlpha(TextSettings.Font.Color, 0.5)).
                           PremultipliedAlpha.
                           ToAlphaColor);
    FPlaceholderLabel.view.setTextColor(LNSColor);

    // PlaceHolder Font
    FPlaceholderLabel.view.setfont(TNSFont.Wrap(LFontRef));
    FPlaceholderLabel.view.sizeToFit;

  finally
    CFRelease(LFontRef);
  end;
end;

{********************************}
procedure TALMacMemoView.DoChange;
begin
  FPlaceholderLabel.view.setHidden((not Text.IsEmpty) or (PromptText.IsEmpty));
  Control.DoChange;
end;

{************************************************}
function TALMacMemoView.GetFillColor: TAlphaColor;
begin
  Result := FFillColor;
end;

{**************************************************************}
procedure TALMacMemoView.SetFillColor(const Value: TAlphaColor);
begin
  FFillColor := Value;
end;

{********************************************}
function TALMacMemoView.getLineCount: integer;
begin
  Result := 0;
  var LlayoutManager := FtextView.View.layoutManager;
  var LnumberOfGlyphs := LlayoutManager.numberOfGlyphs;
  var LlineRange: NSRange;
  var LIndex: Integer := 0;
  while Lindex < LNumberOfGlyphs do begin
    // Get the range of the current line in the text view.
    LlayoutManager.lineFragmentRectForGlyphAtIndex(Lindex, @LlineRange);
    LIndex := NSMaxRange(LlineRange);
    inc(Result); // Increment the line count for each line fragment
  end;
  var LText := GetText;
  if (LText <> '') and (LText[high(LText)] = #10) then inc(result);
  if Result = 0 then result := 1;
end;

{********************************************}
function TALMacMemoView.getLineHeight: Single;
begin
  var LTmpLineHeightMultiplier: Single := ALResolveLineHeightMultiplier(textsettings.font.size, textsettings.LineHeightMultiplier);
  if CompareValue(LTmpLineHeightMultiplier, 0, TEpsilon.Scale) > 0 then result := textsettings.Font.Size * LTmpLineHeightMultiplier
  else begin
    var LfontMetrics := ALGetFontMetrics(
                          ALResolveFontFamily(TextSettings.Font.Family), // const AFontFamily: String;
                          TextSettings.Font.Size, // const AFontSize: single;
                          TextSettings.Font.Weight, // const AFontWeight: TFontWeight;
                          TextSettings.Font.Slant); // const AFontSlant: TFontSlant;
    result := -LfontMetrics.Ascent + LfontMetrics.Descent + LfontMetrics.Leading;
  end;
end;

{*********************************************************************************}
Procedure TALMacMemoView.setSelection(const AStart: integer; const AStop: Integer);
begin
  FTextView.View.setSelectedRange(NSMakeRange(AStart, AStop-AStart));
end;

{***********************************************************}
Procedure TALMacMemoView.setSelection(const AIndex: integer);
begin
  FTextView.View.setSelectedRange(NSMakeRange(AIndex, 0));
end;

{*************************************************}
function TALMacMemoView.getSelectionStart: Integer;
begin
  Result := Integer(FTextView.View.selectedRange.location);
end;

{$endif}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

{***************************************************************}
procedure TALWinMemoView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not ES_AUTOHSCROLL;
  Params.Style := Params.Style or ES_AUTOVSCROLL or ES_MULTILINE;
end;

{****************************************************************}
procedure TALWinMemoView.WMMouseWheel(var Message: TWMMouseWheel);
begin
  if (not Control.IsFocused) or
     (getLineCount < Control.GetNativeViewHeight / getLineHeight)  then inherited
  else begin
    if (Message.WheelDelta > 0) then
      SendMessage(Handle, EM_LINESCROLL, 0, -1)
    else if (Message.WheelDelta < 0) then
      SendMessage(Handle, EM_LINESCROLL, 0, 1);
  end;
end;

{********************************************}
function TALWinMemoView.getLineCount: integer;
begin
  Result := SendMessage(Handle, EM_GETLINECOUNT, 0, 0);
end;

{$endif}
{$ENDREGION}

{***************************************************************************************}
function TALMemo.TDisabledStateStyle.TTextSettings.GetDefaultVertAlign: TALTextVertAlign;
begin
  Result := TALTextVertAlign.Leading;
end;

{*************************************************************************************************************************************}
function TALMemo.TDisabledStateStyle.CreateTextSettings(const AParent: TALBaseTextSettings): TALBaseEdit.TBaseStateStyle.TTextSettings;
begin
  Result := TTextSettings.Create(AParent);
end;

{**************************************************************************************}
function TALMemo.THoveredStateStyle.TTextSettings.GetDefaultVertAlign: TALTextVertAlign;
begin
  Result := TALTextVertAlign.Leading;
end;

{************************************************************************************************************************************}
function TALMemo.THoveredStateStyle.CreateTextSettings(const AParent: TALBaseTextSettings): TALBaseEdit.TBaseStateStyle.TTextSettings;
begin
  Result := TTextSettings.Create(AParent);
end;

{**************************************************************************************}
function TALMemo.TFocusedStateStyle.TTextSettings.GetDefaultVertAlign: TALTextVertAlign;
begin
  Result := TALTextVertAlign.Leading;
end;

{************************************************************************************************************************************}
function TALMemo.TFocusedStateStyle.CreateTextSettings(const AParent: TALBaseTextSettings): TALBaseEdit.TBaseStateStyle.TTextSettings;
begin
  Result := TTextSettings.Create(AParent);
end;

{**************************************************************************************************************}
function TALMemo.TStateStyles.CreateDisabledStateStyle(const AParent: TObject): TALBaseEdit.TDisabledStateStyle;
begin
  result := TDisabledStateStyle.Create(AParent);
end;

{************************************************************************************************************}
function TALMemo.TStateStyles.CreateHoveredStateStyle(const AParent: TObject): TALBaseEdit.THoveredStateStyle;
begin
  result := THoveredStateStyle.Create(AParent);
end;

{************************************************************************************************************}
function TALMemo.TStateStyles.CreateFocusedStateStyle(const AParent: TObject): TALBaseEdit.TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{*******************************************************************}
function TALMemo.TTextSettings.GetDefaultVertAlign: TALTextVertAlign;
begin
  result := TALTextVertAlign.Leading;
end;

{*********************************************}
constructor TALMemo.Create(AOwner: TComponent);
begin
  inherited;
  FAutosizeLineCount := 0;
end;

{********************************************}
procedure TALMemo.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TALMemo then begin
      AutosizeLineCount := TALMemo(Source).AutoSizeLineCount;
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{********************}
{$IF defined(android)}
Function TALMemo.CreateNativeView: TALAndroidNativeView;
begin
  result := TALAndroidEditView.create(self, True{FIsMultiline}, DefStyleAttr, DefStyleRes);
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
Function TALMemo.CreateNativeView: TALIosNativeView;
begin

  // [NOTE] This workaround is no longer necessary, as the delphi framework (e.g., the virtual
  // keyboard service) already instantiates a UITextView internally during app startup,
  // which ensures that the Objective-C class is properly loaded and registered by the Delphi runtime.
  //
  // Originally, we had to create a UITextView instance explicitly to force the Objective-C class
  // (UITextView) to be registered. Without this, calling `TALIosWebView(inherited GetNativeView)`
  // could raise the following error:
  //   Unhandled Exception | Item not found
  //   At address: $0000000100365670
  //   (Generics.Collections.TDictionary<TTypeInfo*, TRegisteredDelphiClass*>.GetItem)
  //
  // Attempting to register the class manually like this:
  //   RegisterObjectiveCClass(TUITextView, TypeInfo(UITextView));
  // also fails with:
  //   Unhandled Exception | Method function someUITextViewMethod of class TUITextView not found
  //   At address: $00000001XXXXXXX
  //   (Macapi.Objectivec.TRegisteredDelphiClass.RegisterClass)
  //
  // Attempting to register our own wrapper class:
  //   RegisterObjectiveCClass(TALIosWebView, TypeInfo(IALIosWebView));
  // fails as well, with:
  //   Unhandled Exception | Objective-C class UITextView could not be found
  //   At address: $00000001046CA014
  //   (Macapi.Objectivec.ObjectiveCClassNotFound)
  //
  // The only reliable workaround is to instantiate a UITextView explicitly to force
  // the UIKit framework to be loaded and the class to be registered:
  //
  //if not IsUITextViewClassRegistered then begin
  //  var LUITextView := TUITextView.Wrap(TUITextView.Alloc.initWithFrame(CGRectMake(0, 0, 0, 0)));
  //  LUITextView.release;
  //  IsUITextViewClassRegistered := True;
  //end;

  result := TALIosMemoView.create(self);

end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
Function TALMemo.CreateNativeView: TALMacNativeView;
begin
  result := TALMacMemoView.create(self);
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
Function TALMemo.CreateNativeView: TALWinNativeView;
begin
  {$IF defined(ALDPK)}
  Result := nil;
  {$ELSE}
  Result := TALWinMemoView.create(self);
  {$ENDIF}
end;
{$ENDIF}

{***********************************************************}
function TALMemo.CreateStateStyles: TALBaseEdit.TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{*************************************************************}
function TALMemo.CreateTextSettings: TALBaseEdit.TTextSettings;
begin
  result := TTextSettings.Create;
end;

{**********************************************}
function TALMemo.GetTextSettings: TTextSettings;
begin
  result := TTextSettings(inherited TextSettings);
end;

{************************************************************}
procedure TALMemo.SetTextSettings(const Value: TTextSettings);
begin
  inherited TextSettings := Value;
end;

{***************************}
procedure TALMemo.AdjustSize;
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

        var LAdjustement: Single := ((LLineHeight / 100) * 25);
        if AutoAlignToPixel then LAdjustement := ALAlignDimensionToPixelRound(LAdjustement, ALGetScreenScale, TEpsilon.Position);

        If LInlinedLabelText then begin
          SetFixedSizeBounds(
            Position.X,
            Position.Y,
            Width,
            (LLineHeight * AutoSizeLineCount) + LAdjustement + LStrokeSize.Top + LStrokeSize.bottom + padding.Top + padding.Bottom + BufLabelTextDrawableRect.Height + LabelTextSettings.Margins.Top + LabelTextSettings.Margins.bottom);
        end
        else begin
          SetFixedSizeBounds(
            Position.X,
            Position.Y,
            Width,
            (LLineHeight * AutoSizeLineCount) + LAdjustement + LStrokeSize.Top + LStrokeSize.bottom + padding.Top + padding.Bottom);
        end;

      end;

      var LMarginRect := TRectF.Empty;

      if LInlinedLabelText then
        LMarginRect.top := BufLabelTextDrawableRect.Height + LabelTextSettings.Margins.Top + LabelTextSettings.Margins.bottom;

      LMarginRect.left :=   Max(LMarginRect.left   + LStrokeSize.left,   0);
      LMarginRect.Top :=    Max(LMarginRect.Top    + LStrokeSize.Top,    0);
      LMarginRect.Right :=  Max(LMarginRect.Right  + LStrokeSize.Right,  0);
      LMarginRect.Bottom := Max(LMarginRect.Bottom + LStrokeSize.Bottom, 0);

      NativeViewMargins.Rect := LMarginRect;

    finally
      TNonReentrantHelper.LeaveSection(FIsAdjustingSize)
    end;
  end;
end;

{******************************************************}
function TALMemo.HasUnconstrainedAutosizeWidth: Boolean;
begin
  result := False;
end;

{**************************************}
function TALMemo.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(200, 75);
end;

{********************************************}
function TALMemo.GetAutoSize: TALAutoSizeMode;
begin
  if FAutoSizeLineCount > 0 then Result := TALAutoSizeMode.Height
  else Result := TALAutoSizeMode.None;
end;

{***********************************************************}
procedure TALMemo.SetAutosizeLineCount(const Value: Integer);
begin
  if FAutoSizeLineCount <> Value then begin
    FAutoSizeLineCount := Max(0, Value);
    AdjustSize;
  end;
end;

{********************}
{$IF defined(android)}
Function TALDummyMemo.CreateNativeView: TALAndroidNativeView;
begin
  result := nil;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
Function TALDummyMemo.CreateNativeView: TALIosNativeView;
begin
  result := nil;
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
Function TALDummyMemo.CreateNativeView: TALMacNativeView;
begin
  result := nil;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
Function TALDummyMemo.CreateNativeView: TALWinNativeView;
begin
  Result := nil;
end;
{$ENDIF}

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALMemo]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALMemo, 'Size');
  UnlistPublishedProperty(TALMemo, 'StyleName');
  UnlistPublishedProperty(TALMemo, 'OnTap');
  {$ENDIF}
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Memo','initialization');
  {$ENDIF}
  RegisterFmxClasses([TALMemo]);

end.