{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2016 Embarcadero Technologies, Inc.      }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Edit.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.TypInfo, System.Classes, System.Types, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CocoaTypes,
  Macapi.ObjectiveC, Macapi.ObjCRuntime, FMX.Controls.Presentation, FMX.Presentation.iOS, FMX.Presentation.Messages,
  FMX.Edit, FMX.Types, FMX.Controls.Model;

type

{ TiOSNativeEdit }

  IFMXTextField = interface(UITextField)
  ['{E0EA93B8-E2B2-4B02-A832-FAAC42AEA154}']
    { Touches }
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;    
    { Changing }
    procedure ControlEventEditingChanged; cdecl;
    procedure ControlEventEditingDidEnd; cdecl;
    { Focus }
    function canBecomeFirstResponder: Boolean; cdecl;
    { Context Menu }
    function canPerformAction(action: SEL; withSender: Pointer): Boolean; cdecl;
  end;

  TiOSTextFieldDelegate = class;

  TiOSNativeEdit = class(TiOSNativeControl)
  private
    FTextFieldDelegate: TiOSTextFieldDelegate;
    FSavedButtomsContentVisible: Boolean;
    FPreviousSelection: NSRange;
    FAttributedString: NSMutableAttributedString;
    function GetEdit: TCustomEdit;
    function GetView: UITextField;
    function GetModel: TCustomEditModel;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
    /// <summary>Updates text settings of native UITextField based on Model.TextSettings</summary>
    procedure UpdateTextSettings;
    /// <summary>Updates caret and selection color of native UITextField based on Model.Caret</summary>
    procedure UpdateCaretColor;
    /// <summary>Updates selection of native UITextField based on Model.SelStart and Model.SelLength</summary>
    procedure UpdateSelection;
    /// <summary>Updates visible of clear bytton in native UITextField</summary>
    procedure UpdateVisibleOfClearButton;
    procedure UpdateSpellChecking;
    function DefineModelClass: TDataModelClass; override;
    /// <summary>Updates text in native UITextField</summary>
    procedure UpdateTextInField;
  protected
    { Messages from Model }
    procedure MMCheckSpellingChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message MM_EDIT_CHECKSPELLING_CHANGED;
    procedure MMTextPromptChanged(var AMessage: TDispatchMessageWithValue<string>); message MM_EDIT_PROMPTTEXT_CHANGED;
    procedure MMIsPasswordChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message MM_EDIT_ISPASSWORD_CHANGED;
    procedure MMKeyboardTypeChanged(var AMessage: TDispatchMessageWithValue<TVirtualkeyboardType>); message MM_EDIT_KEYBOARDTYPE_CHANGED;
    procedure MMReturnKeyTypeChanged(var AMessage: TDispatchMessageWithValue<TReturnKeyType>); message MM_EDIT_RETURNKEYTYPE_CHANGED;
    procedure MMTextSettingsChanged(var AMessage: TDispatchMessage); message MM_EDIT_TEXT_SETTINGS_CHANGED;
    procedure MMEditButtonsChanged(var AMessage: TDispatchMessage); message MM_EDIT_EDITBUTTONS_CHANGED;
    procedure MMTextChanged(var AMessage: TDispatchMessage); message MM_EDIT_TEXT_CHANGED;
    procedure MMCaretPositionChanged(var AMessage: TDispatchMessageWithValue<Integer>); message MM_EDIT_CARETPOSITION_CHANGED;
    procedure MMCaretChanged(var AMessage: TDispatchMessage); message MM_EDIT_CARETCHANGED;
    procedure MMSelStartChanged(var AMessage: TDispatchMessageWithValue<Integer>); message MM_EDIT_SELSTART_CHANGED;
    procedure MMSelLenghtChanged(var AMessage: TDispatchMessageWithValue<Integer>); message MM_EDIT_SELLENGTH_CHANGED;
    { Messages from PresentationProxy }
    procedure PMInit(var AMessage: TDispatchMessage); message PM_INIT;
    procedure PMUnload(var AMessage: TDispatchMessage); message PM_UNLOAD;
    procedure PMAbsoluteChanged(var AMessage: TDispatchMessage); message PM_ABSOLUTE_CHANGED;
    procedure PMSetStyleLookup(var AMessage: TDispatchMessageWithValue<string>); message PM_SET_STYLE_LOOKUP;
    procedure PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_RECOMMEND_SIZE;
  public
    constructor Create; override;
    destructor Destroy; override;
    { Editing }
    procedure ControlEventEditingChanged; cdecl;
    procedure ControlEventEditingDidEnd; cdecl;
    { Selection }
    /// <summary>Notifies about changes of Selection</summary>
    procedure SelectionChanged; virtual;
    { Context Menu }
    function canPerformAction(action: SEL; withSender: Pointer): Boolean; cdecl;
    { Focus }
    /// <summary>Overriding UIResponder.canBecomeFirstResponder method to retranslate TCustomMemo.CanFocus value</summary>
    function canBecomeFirstResponder: Boolean; cdecl;
    /// <summary>Returns selection range</summary>
    function SelectedRange: NSRange;
  public
    property Edit: TCustomEdit read GetEdit;
    property Model: TCustomEditModel read GetModel;
    property View: UITextField read GetView;
  end;

  TiOSTextFieldDelegate = class(TOCLocal, UITextFieldDelegate)
  private
    [Weak] FNativeEdit: TiOSNativeEdit;
  public
    constructor Create(const ANativeEdit: TiOSNativeEdit);
    { UITextFieldDelegate }
    function textField(textField: UITextField; shouldChangeCharactersInRange: NSRange; replacementString: NSString): Boolean; cdecl;
    procedure textFieldDidBeginEditing(textField: UITextField); cdecl;
    procedure textFieldDidEndEditing(textField: UITextField); cdecl;
    function textFieldShouldBeginEditing(textField: UITextField): Boolean; cdecl;
    function textFieldShouldClear(textField: UITextField): Boolean; cdecl;
    function textFieldShouldEndEditing(textField: UITextField): Boolean; cdecl;
    function textFieldShouldReturn(textField: UITextField): Boolean; cdecl;
  end;

implementation

uses
  Macapi.CoreFoundation, Macapi.Helpers, System.UITypes, iOSapi.CoreImage, iOSapi.CoreText, FMX.Controls,
  FMX.Presentation.Factory, FMX.Helpers.iOS, FMX.Graphics, FMX.Consts;

{ TiOSNativeEdit }

function TiOSNativeEdit.canBecomeFirstResponder: Boolean;
begin
  Result := Edit.CanFocus and Edit.HitTest;
end;

function TiOSNativeEdit.canPerformAction(action: SEL; withSender: Pointer): Boolean;
var
  NewSelectedRange: NSRange;
begin
  Result := UIView(Super).canPerformAction(action, withSender);

  // UITextField doesn't provide a way to intercept the moment when user changes selection. 
  // This is a workaround: every time the selection is changed, iOS shows context menu. 
  // Before doing that, it asks UITextField which menu items should be included in the menu.
  NewSelectedRange := SelectedRange;
  if not NSEqualRanges(NewSelectedRange, FPreviousSelection) then
  begin
    FPreviousSelection := NewSelectedRange;
    SelectionChanged;
  end;
end;

procedure TiOSNativeEdit.ControlEventEditingChanged;
var
  NewText: string;
begin
  NewText := NSStrToStr(TNSString.Wrap(View.text));
  Model.DisableNotify;
  try
    Model.Text := NewText;
  finally
    Model.EnableNotify;
  end;
  // There are can be a situation, when Model.Text will truncate or filter text and truncated text can be the same
  // as a old value of Model.Text. In this case Model will not send notification about changing text. So we need
  // to manually check it and update text in UITextField
  if Model.Text <> NewText then
    UpdateTextInField;
end;

procedure TiOSNativeEdit.ControlEventEditingDidEnd;
begin
  Model.Change;
  inherited;
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    TLinkObservers.EditLinkUpdate(Observers);
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueUpdate(Observers);
end;

constructor TiOSNativeEdit.Create;
begin
  inherited;
  FTextFieldDelegate := TiOSTextFieldDelegate.Create(Self);
  View.setExclusiveTouch(True);
  View.setBorderStyle(UITextBorderStyleRoundedRect);
  View.setDelegate((FTextFieldDelegate as ILocalObject).GetObjectID);
  FPreviousSelection := NSMakeRange(NSNotFound, NSNotFound);

  RegisterNativeEventHandler('ControlEventEditingChanged', UIControlEventEditingChanged);
  RegisterNativeEventHandler('ControlEventEditingDidEnd', UIControlEventEditingDidEnd);
end;

function TiOSNativeEdit.DefineModelClass: TDataModelClass;
begin
  Result := TCustomEditModel;
end;

destructor TiOSNativeEdit.Destroy;
begin
  UnRegisterNativeEventHandler('ControlEventEditingChanged', UIControlEventEditingChanged);
  UnRegisterNativeEventHandler('ControlEventEditingDidEnd', UIControlEventEditingDidEnd);

  View.setDelegate(nil);
  FTextFieldDelegate.Free;
  FAttributedString.release;
  inherited;
end;

function TiOSNativeEdit.GetEdit: TCustomEdit;
begin
  Result := Control as TCustomEdit
end;

function TiOSNativeEdit.GetModel: TCustomEditModel;
begin
  Result := inherited GetModel<TCustomEditModel>;
end;

function TiOSNativeEdit.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IFMXTextField);
end;

function TiOSNativeEdit.GetView: UITextField;
begin
  Result := inherited GetView<UITextField>;
end;

procedure TiOSNativeEdit.MMCaretChanged(var AMessage: TDispatchMessage);
begin
  UpdateCaretColor;
end;

procedure TiOSNativeEdit.MMCaretPositionChanged(var AMessage: TDispatchMessageWithValue<Integer>);
begin
  UpdateSelection;
end;

procedure TiOSNativeEdit.MMCheckSpellingChanged(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  UpdateSpellChecking;
end;

procedure TiOSNativeEdit.MMEditButtonsChanged(var AMessage: TDispatchMessage);
begin
  UpdateVisibleOfClearButton;
end;

procedure TiOSNativeEdit.MMIsPasswordChanged(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  View.setSecureTextEntry(Model.Password);
end;

procedure TiOSNativeEdit.MMKeyboardTypeChanged(var AMessage: TDispatchMessageWithValue<TVirtualkeyboardType>);
begin
  View.setKeyboardType(VirtualKeyboardTypeToUIKeyboardType(Model.KeyboardType));
end;

procedure TiOSNativeEdit.MMReturnKeyTypeChanged(var AMessage: TDispatchMessageWithValue<TReturnKeyType>);
begin
  View.setReturnKeyType(ReturnKeyTypeToUIReturnKeyType(Model.ReturnKeyType));
end;

procedure TiOSNativeEdit.MMSelLenghtChanged(var AMessage: TDispatchMessageWithValue<Integer>);
begin
  FPreviousSelection.length := Model.SelLength;
  UpdateSelection;
end;

procedure TiOSNativeEdit.MMSelStartChanged(var AMessage: TDispatchMessageWithValue<Integer>);
begin
  FPreviousSelection.location := Model.SelStart;
  UpdateSelection;
end;

procedure TiOSNativeEdit.MMTextChanged(var AMessage: TDispatchMessage);
begin
  UpdateTextInField;
end;

procedure TiOSNativeEdit.MMTextPromptChanged(var AMessage: TDispatchMessageWithValue<string>);
begin
  View.setPlaceholder(StrToNSStr(Model.TextPrompt));
end;

procedure TiOSNativeEdit.MMTextSettingsChanged(var AMessage: TDispatchMessage);
begin
  UpdateTextSettings;
end;

procedure TiOSNativeEdit.PMAbsoluteChanged(var AMessage: TDispatchMessage);
begin
  inherited;
  TUIMenuController.Wrap(TUIMenuController.OCClass.sharedMenuController).setMenuVisible(False);
end;

procedure TiOSNativeEdit.PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
end;

procedure TiOSNativeEdit.PMInit(var AMessage: TDispatchMessage);
begin
  UpdateSpellChecking;
  View.setSecureTextEntry(Model.Password);
  View.setPlaceholder(StrToNSStr(Model.TextPrompt));
  View.setReturnKeyType(ReturnKeyTypeToUIReturnKeyType(Model.ReturnKeyType));
  View.setKeyboardType(VirtualKeyboardTypeToUIKeyboardType(Model.KeyboardType));
  UpdateTextSettings;
  UpdateSelection;
  UpdateVisibleOfClearButton;

  // Hides buttons content, because native TEdit has own buttons
  FSavedButtomsContentVisible := Edit.ButtonsContent.Visible;
  Edit.ButtonsContent.Visible := False;
end;

procedure TiOSNativeEdit.PMSetStyleLookup(var AMessage: TDispatchMessageWithValue<string>);
const
  TransparentStyleName = 'transparentedit'; // do not localize
begin
  if AMessage.Value.ToLower = TransparentStyleName then
    View.setBorderStyle(UITextBorderStyleNone)
  else
    View.setBorderStyle(UITextBorderStyleRoundedRect);
end;

procedure TiOSNativeEdit.PMUnload(var AMessage: TDispatchMessage);
begin
  if (Edit <> nil) and (Edit.ButtonsContent <> nil) then
    Edit.ButtonsContent.Visible := FSavedButtomsContentVisible;
end;

function TiOSNativeEdit.SelectedRange: NSRange;
var
  Beginning: UITextPosition;
  SelectedRange: UITextRange;
  SelectionStart: UITextPosition;
  SelectionEnd: UITextPosition;
  SelStart: NSInteger;
  SelLength: NSInteger;
begin
  Beginning := View.beginningOfDocument;
  
  SelectedRange := View.selectedTextRange;
  SelectionStart := selectedRange.start;
  SelectionEnd := selectedRange.&end;
  
  SelStart := View.offsetFromPosition(Beginning, SelectionStart);
  SelLength := View.offsetFromPosition(SelectionStart, SelectionEnd);
  Result := NSMakeRange(SelStart, SelLength);
end;

procedure TiOSNativeEdit.SelectionChanged;
var
  LSelectedRange: NSRange;
begin
  Model.DisableNotify;
  try
    LSelectedRange := SelectedRange;
    Model.SelStart := LSelectedRange.location;
    Model.SelLength := LSelectedRange.length;
  finally
    Model.EnableNotify;
  end;
end;

procedure TiOSNativeEdit.UpdateVisibleOfClearButton;

  function EditHasClearButtons: Boolean;
  var
    I: Integer;
  begin
    for I := 0 to Edit.ButtonsContent.ChildrenCount - 1 do
      if Edit.ButtonsContent.Children[I] is TClearEditButton then
        Exit(True);
    Result := False;    
  end;

begin
  if EditHasClearButtons then
    View.setClearButtonMode(UITextFieldViewModeAlways)
  else
    View.setClearButtonMode(UITextFieldViewModeNever);
end;

procedure TiOSNativeEdit.UpdateSpellChecking;
begin
  if Model.CheckSpelling then
    View.setSpellCheckingType(UITextSpellCheckingTypeYes)
  else
    View.setSpellCheckingType(UITextSpellCheckingTypeNo)
end;

procedure TiOSNativeEdit.UpdateCaretColor;
begin
  if TOSVersion.Check(7) and (Model.Caret.Color <> TAlphaColorRec.Null) then
    View.setTintColor(AlphaColorToUIColor(Model.Caret.Color));
end;

procedure TiOSNativeEdit.UpdateSelection;
var
  StartPosition: UITextPosition;
  EndPosition: UITextPosition;
begin
  StartPosition := View.positionFromPosition(View.beginningOfDocument, Model.SelStart);
  EndPosition := View.positionFromPosition(StartPosition, Model.SelLength);
  View.setSelectedTextRange(View.textRangeFromPosition(StartPosition, EndPosition));
end;

procedure TiOSNativeEdit.UpdateTextInField;
begin
  if (FAttributedString = nil) or (FAttributedString.length = 0) then
    UpdateTextSettings
  else
  begin
    FAttributedString.replaceCharactersInRange(NSMakeRange(0, FAttributedString.length), StrToNSStr(Model.Text));
    View.setAttributedText(FAttributedString);
  end;
end;

procedure TiOSNativeEdit.UpdateTextSettings;
var
  TextSettings: TTextSettings;
  TextRange: NSRange;
  FontRef: CTFontRef;
  Underline: CFNumberRef;
  LValue: Cardinal;
begin
  TextSettings := Model.TextSettingsInfo.ResultingTextSettings;

  if FAttributedString <> nil then
    FAttributedString.release;

  FAttributedString := TNSMutableAttributedString.Alloc;
  FAttributedString := TNSMutableAttributedString.Wrap(FAttributedString.initWithString(StrToNSStr(Model.Text)));

  FAttributedString.beginEditing;
  try
    TextRange := NSMakeRange(0, Model.Text.Length);
    //Font
    FontRef := FontToCTFontRef(TextSettings.Font);
    if FontRef <> nil then
    try
      FAttributedString.addAttribute(TNSString.Wrap(kCTFontAttributeName), FontRef, TextRange);
    finally
      CFRelease(FontRef);
    end;
    //Font style
    if TFontStyle.fsUnderline in TextSettings.Font.Style then
    begin
      LValue := kCTUnderlineStyleSingle;
      Underline := CFNumberCreate(nil, kCFNumberSInt32Type, @LValue);
      try
        FAttributedString.addAttribute(TNSString.Wrap(kCTUnderlineStyleAttributeName), Underline, TextRange);
      finally
        CFRelease(Underline);
      end;
    end;
  finally
    FAttributedString.endEditing;
  end;

  View.setAttributedText(FAttributedString);

  View.setTextAlignment(TextAlignToUITextAlignment(TextSettings.HorzAlign));
  View.setTextColor(AlphaColorToUIColor(TextSettings.FontColor));
  View.setFont(FontToUIFont(TextSettings.Font));
  UpdateCaretColor;
end;

{ TiOSTextFieldDelegate }

constructor TiOSTextFieldDelegate.Create(const ANativeEdit: TiOSNativeEdit);
begin
  inherited Create;
  FNativeEdit := ANativeEdit;
  if ANativeEdit = nil then
    raise EArgumentNilException.Create(Format(SWrongParameter, ['ANativeEdit']));
end;

function TiOSTextFieldDelegate.textField(textField: UITextField; shouldChangeCharactersInRange: NSRange;
  replacementString: NSString): Boolean;
begin
  Result := True;
end;

procedure TiOSTextFieldDelegate.textFieldDidBeginEditing(textField: UITextField);
begin
  if not FNativeEdit.Control.IsFocused then
    FNativeEdit.Control.SetFocus;
end;

procedure TiOSTextFieldDelegate.textFieldDidEndEditing(textField: UITextField);
begin
end;

function TiOSTextFieldDelegate.textFieldShouldBeginEditing(textField: UITextField): Boolean;
begin
  Result := True;
  if FNativeEdit.Observers.IsObserving(TObserverMapping.EditLinkID) then
    if TLinkObservers.EditLinkEdit(FNativeEdit.Observers) then
      TLinkObservers.EditLinkModified(FNativeEdit.Observers)
    else
    begin
      TLinkObservers.EditLinkReset(FNativeEdit.Observers);
      Result := False;
    end;
  if FNativeEdit.Observers.IsObserving(TObserverMapping.ControlValueID) and Result then
    TLinkObservers.ControlValueModified(FNativeEdit.Observers);
  Result := Result and not FNativeEdit.Model.ReadOnly;
end;

function TiOSTextFieldDelegate.textFieldShouldClear(textField: UITextField): Boolean;
begin
  Result := not FNativeEdit.Model.ReadOnly;
end;

function TiOSTextFieldDelegate.textFieldShouldEndEditing(textField: UITextField): Boolean;
begin
  Result := True;
end;

function TiOSTextFieldDelegate.textFieldShouldReturn(textField: UITextField): Boolean;
begin
  FNativeEdit.ControlEventEditingDidEnd;
  Result := FNativeEdit.Model.KillFocusByReturn;
  if Result then
    FNativeEdit.ResetFocus;
end;

initialization
  TPresentationProxyFactory.Current.Register(TEdit, TControlType.Platform, TiOSPresentationProxy<TiOSNativeEdit>);
finalization
  TPresentationProxyFactory.Current.Unregister(TEdit, TControlType.Platform, TiOSPresentationProxy<TiOSNativeEdit>);
end.
