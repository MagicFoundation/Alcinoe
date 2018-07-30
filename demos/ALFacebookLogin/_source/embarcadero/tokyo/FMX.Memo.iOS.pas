{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2014-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Memo.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.SysUtils, System.TypInfo, Macapi.ObjectiveC, iOSapi.UIKit, iOSapi.Foundation, iOSapi.CoreGraphics,
  FMX.Memo, FMX.Presentation.Messages, FMX.Controls.Presentation, FMX.Presentation.iOS, FMX.Controls, FMX.Types,
  FMX.ScrollBox.iOS, FMX.Memo.Types, FMX.Controls.Model;

type

{ TiOSNativeMemo }

  TiOSTextViewDelegate = class;

  IFMXUITextView = interface(UITextView)
  ['{F24D110A-BDC7-4653-94E1-4FF6305FCAC0}']
    { Touches }
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    { Gestures }
    procedure HandlePan(gestureRecognizer: UIPanGestureRecognizer); cdecl;
    function gestureRecognizerShouldBegin(gestureRecognizer: UIGestureRecognizer): Boolean; cdecl;
    { Focus }
    function canBecomeFirstResponder: Boolean; cdecl;
  end;

  TiOSNativeMemo = class(TiOSScrollBox)
  private
    FDefaultBackgroundColor: UIColor;
    FAttributedString: NSMutableAttributedString;
    function GetModel: TCustomMemoModel;
    function GetMemo: TCustomMemo;
    function GetView: UITextView;
  protected
    procedure UpdateTextSettings;
    procedure UpdateTextSelection;
    function CreateDelegate: TiOSScrollBoxDelegate; override;
    function GetObjectiveCClass: PTypeInfo; override;
    function DefineModelClass: TDataModelClass; override;
  protected
    { Messages from Model }
    procedure MMCaretChanged(var AMessage: TDispatchMessageWithValue<TCaret>); message MM_MEMO_CARETCHANGED;
    procedure MMCheckSpellingChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message MM_MEMO_CHECKSPELLING_CHANGED;
    procedure MMDataDetectoTypes(var AMessage: TDispatchMessageWithValue<TDataDetectorTypes>); message MM_MEMO_DATADETECTORTYPES_CHANGED;
    procedure MMKeyboardTypeChanged(var AMessage: TDispatchMessageWithValue<TVirtualkeyboardType>); message MM_MEMO_KEYBOARDTYPE_CHANGED;
    procedure MMLinesChanged(var AMessage: TDispatchMessage); message MM_MEMO_LINES_CHANGED;
    procedure MMReadOnlyChanged(var AMessage: TDispatchMessage); message MM_MEMO_READONLY_CHANGED;
    procedure MMSelStartChanged(var AMessage: TDispatchMessage); message MM_MEMO_SELSTART_CHANGED;
    procedure MMSelLengthChanged(var AMessage: TDispatchMessage); message MM_MEMO_SELLENGTH_CHANGED;
    procedure MMTextSettingsChanged(var AMessage: TDispatchMessage); message MM_MEMO_TEXT_SETTINGS_CHANGED;
    procedure MMCaretPositionChanged(var AMessage: TDispatchMessageWithValue<TCaretPosition>); message MM_MEMO_SET_CARET_POSITION;
    procedure MMLinesClear(var AMessage: TDispatchMessage); message MM_MEMO_LINES_CLEAR;
    { Messages from PresentationProxy }
    procedure PMInit(var AMessage: TDispatchMessage); message PM_INIT;
    procedure PMAbsoluteChanged(var AMessage: TDispatchMessage); message PM_ABSOLUTE_CHANGED;
    procedure PMDoEnter(var AMessage: TDispatchMessage); message PM_DO_ENTER;
    procedure PMSetStyleLookup(var AMessage: TDispatchMessageWithValue<string>); message PM_SET_STYLE_LOOKUP;
    procedure PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_RECOMMEND_SIZE;
    procedure PMSetClipChildren(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_CLIP_CHILDREN;
    procedure PMGoToTextBegin(var AMessage: TDispatchMessage); message PM_MEMO_GOTO_TEXT_BEGIN;
    procedure PMGoToTextEnd(var AMessage: TDispatchMessage); message PM_MEMO_GOTO_TEXT_END;
    procedure PMGoToLineBegin(var AMessage: TDispatchMessage); message PM_MEMO_GOTO_LINE_BEGIN;
    procedure PMGoToLineEnd(var AMessage: TDispatchMessage); message PM_MEMO_GOTO_LINE_END;
    procedure PMSelectText(var AMessage: TDispatchMessage); message PM_MEMO_SELECT_TEXT;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    { Focus }
    ///<summary>Overriding UIResponder.canBecomeFirstResponder method to retranslate TCustomMemo.CanFocus value</summary>
    function canBecomeFirstResponder: Boolean; cdecl;
  public
    property Model: TCustomMemoModel read GetModel;
    property Memo: TCustomMemo read GetMemo;
    property View: UITextView read GetView;
  end;

  TiOSTextViewDelegate = class(TiOSScrollBoxDelegate, UITextViewDelegate)
  private
    [Weak] FNativeMemo: TiOSNativeMemo;
    FSkipEditing: Boolean;
  public
    constructor Create(const ANativeMemo: TiOSNativeMemo);
    { UITextViewDelegate }
    procedure textViewDidBeginEditing(textView: UITextView); cdecl;
    procedure textViewDidChange(textView: UITextView); cdecl;
    procedure textViewDidChangeSelection(textView: UITextView); cdecl;
    procedure textViewDidEndEditing(textView: UITextView); cdecl;
    function textViewShouldBeginEditing(textView: UITextView): Boolean; cdecl;
    function textViewShouldEndEditing(textView: UITextView): Boolean; cdecl;
    [MethodName('textView:shouldChangeTextInRange:replacementText:')]
    function textViewShouldChangeTextInRangeReplacementText(textView: UITextView; shouldChangeTextInRange: NSRange;
      replacementText: NSString): Boolean; cdecl;
    [MethodName('textView:shouldInteractWithURL:inRange:')]
    function textViewShouldInteractWithURLInRange(textView: UITextView; shouldInteractWithURL: NSURL; inRange: NSRange)
      : Boolean; cdecl;
    [MethodName('textView:shouldInteractWithTextAttachment:inRange:')]
    function textViewShouldInteractWithTextAttachmentInRange(textView: UITextView;
      shouldInteractWithTextAttachment: NSTextAttachment; inRange: NSRange): Boolean; cdecl;
  end;

implementation

uses
  System.UITypes, System.Classes, System.Math, MacApi.Helpers, {$IFDEF MACOS}Macapi.CoreFoundation,{$ENDIF}
  FMX.Presentation.Factory, FMX.Consts, FMX.Graphics, FMX.Helpers.iOS,
  iOSapi.CoreText;

function DataDetectorTypesToUIDataDetectorTypes(const ASource: TDataDetectorTypes): UIDataDetectorTypes;
begin
  Result := UIDataDetectorTypeNone;
  if TDataDetectorType.PhoneNumber in ASource then
    Result := Result or UIDataDetectorTypePhoneNumber;
  if TDataDetectorType.Link in ASource then
    Result := Result or UIDataDetectorTypeLink;
  if TDataDetectorType.Address in ASource then
    Result := Result or UIDataDetectorTypeAddress;
  if TDataDetectorType.CalendarEvent in ASource then
    Result := Result or UIDataDetectorTypeCalendarEvent;
end;

{ TiOSNativeMemo }

function TiOSNativeMemo.canBecomeFirstResponder: Boolean;
begin
  Result := Memo.CanFocus and Memo.HitTest;
end;

procedure TiOSNativeMemo.Clear;
begin
  if FAttributedString <> nil then
  begin
    FAttributedString.deleteCharactersInRange(NSMakeRange(0, FAttributedString.length));
    View.setAttributedText(FAttributedString);
  end;
end;

constructor TiOSNativeMemo.Create;
begin
  inherited;
  Delegate.Model := Model;
  FDefaultBackgroundColor := TUIColor.Wrap(View.layer.backgroundColor);
end;

function TiOSNativeMemo.CreateDelegate: TiOSScrollBoxDelegate;
begin
  Result := TiOSTextViewDelegate.Create(Self);
end;

function TiOSNativeMemo.DefineModelClass: TDataModelClass;
begin
  Result := TCustomMemoModel;
end;

destructor TiOSNativeMemo.Destroy;
begin
  FAttributedString.release;
  inherited;
end;

function TiOSNativeMemo.GetMemo: TCustomMemo;
begin
  Result := Control as TCustomMemo;
end;

function TiOSNativeMemo.GetModel: TCustomMemoModel;
begin
  Result := inherited GetModel<TCustomMemoModel>;
end;

function TiOSNativeMemo.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IFMXUITextView);
end;

function TiOSNativeMemo.GetView: UITextView;
begin
  Result := inherited GetView<UITextView>;
end;

procedure TiOSNativeMemo.MMCaretChanged(var AMessage: TDispatchMessageWithValue<TCaret>);
begin
  if TOSVersion.Check(7) and (Model.Caret.Color <> TAlphaColorRec.Null) then
    View.setTintColor(AlphaColorToUIColor(Model.Caret.Color));
end;

procedure TiOSNativeMemo.MMCaretPositionChanged(var AMessage: TDispatchMessageWithValue<TCaretPosition>);
begin
  View.setSelectedRange(NSMakeRange(Model.PosToTextPos(AMessage.Value), 0));
end;

procedure TiOSNativeMemo.MMCheckSpellingChanged(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  if Model.CheckSpelling then
    View.setSpellCheckingType(UITextSpellCheckingTypeYes)
  else
    View.setSpellCheckingType(UITextSpellCheckingTypeNo);
end;

procedure TiOSNativeMemo.MMDataDetectoTypes(var AMessage: TDispatchMessageWithValue<TDataDetectorTypes>);
begin
  View.setDataDetectorTypes(DataDetectorTypesToUIDataDetectorTypes(AMessage.Value));
end;

procedure TiOSNativeMemo.MMKeyboardTypeChanged(var AMessage: TDispatchMessageWithValue<TVirtualkeyboardType>);
begin
  View.setKeyboardType(VirtualKeyboardTypeToUIKeyboardType(AMessage.Value));
end;

procedure TiOSNativeMemo.MMLinesChanged(var AMessage: TDispatchMessage);
begin
  if (FAttributedString = nil) or (FAttributedString.length = 0) then
    UpdateTextSettings
  else
  begin
    FAttributedString.replaceCharactersInRange(NSMakeRange(0, FAttributedString.length), StrToNSStr(Model.Lines.Text));
    View.setAttributedText(FAttributedString);
  end;
end;

procedure TiOSNativeMemo.MMLinesClear(var AMessage: TDispatchMessage);
begin
  Clear;
end;

procedure TiOSNativeMemo.MMReadOnlyChanged(var AMessage: TDispatchMessage);
begin
  View.setEditable(not Model.ReadOnly);
end;

procedure TiOSNativeMemo.MMSelLengthChanged(var AMessage: TDispatchMessage);
begin
  UpdateTextSelection;
end;

procedure TiOSNativeMemo.MMSelStartChanged(var AMessage: TDispatchMessage);
begin
  UpdateTextSelection;
end;

procedure TiOSNativeMemo.MMTextSettingsChanged(var AMessage: TDispatchMessage);
begin
  UpdateTextSettings;
end;

procedure TiOSNativeMemo.PMAbsoluteChanged(var AMessage: TDispatchMessage);
begin
  inherited;
  TUIMenuController.Wrap(TUIMenuController.OCClass.sharedMenuController).setMenuVisible(False);
end;

procedure TiOSNativeMemo.PMDoEnter(var AMessage: TDispatchMessage);
var
  Range: NSRange;
begin
  inherited;
  if Model.AutoSelect then
  begin
    View.setSelectedRange(NSMakeRange(0, Model.Lines.Text.Length));
    Range := view.selectedRange;
  end;
end;

procedure TiOSNativeMemo.PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
  // This control doesn't support recommend size, so we don't have to change required size in AMessage
end;

procedure TiOSNativeMemo.PMGoToLineBegin(var AMessage: TDispatchMessage);
var
  LineBeginPos: TCaretPosition;
begin
  LineBeginPos := Model.CaretPosition;
  LineBeginPos.Pos := 0;
  View.setSelectedRange(NSMakeRange(Model.PosToTextPos(LineBeginPos), 0));
end;

procedure TiOSNativeMemo.PMGoToLineEnd(var AMessage: TDispatchMessage);
var
  LineEndPos: TCaretPosition;
begin
  LineEndPos := Model.CaretPosition;
  if InRange(LineEndPos.Line, 0, Model.Lines.Count - 1) then
  begin
    LineEndPos.Pos := Model.Lines[LineEndPos.Line].Length;
    View.setSelectedRange(NSMakeRange(Model.PosToTextPos(LineEndPos), 0));
  end;
end;

procedure TiOSNativeMemo.PMGoToTextBegin(var AMessage: TDispatchMessage);
begin
  View.setSelectedRange(NSMakeRange(0, 0));
end;

procedure TiOSNativeMemo.PMGoToTextEnd(var AMessage: TDispatchMessage);
begin
  View.setSelectedRange(NSMakeRange(Model.Lines.Text.Length, 0));
end;

procedure TiOSNativeMemo.PMInit(var AMessage: TDispatchMessage);
begin
  if Model.CheckSpelling then
    View.setSpellCheckingType(UITextSpellCheckingTypeYes)
  else
    View.setSpellCheckingType(UITextSpellCheckingTypeNo);
  View.setKeyboardType(VirtualKeyboardTypeToUIKeyboardType(Model.KeyboardType));
  View.setEditable(not Model.ReadOnly);
  View.setDataDetectorTypes(DataDetectorTypesToUIDataDetectorTypes(Model.DataDetectorTypes));
  UpdateTextSettings;
end;

procedure TiOSNativeMemo.PMSelectText(var AMessage: TDispatchMessage);
begin
  UpdateTextSelection;
end;

procedure TiOSNativeMemo.PMSetClipChildren(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  View.setClipsToBounds(True);
end;

procedure TiOSNativeMemo.PMSetStyleLookup(var AMessage: TDispatchMessageWithValue<string>);
const
  TransparentStyleName = 'transparentmemo'; // do not localize
begin
  if AMessage.Value.ToLower = TransparentStyleName then
    View.layer.setBackgroundColor(TUIColor.OCClass.clearColor)
  else
    View.layer.setBackgroundColor((FDefaultBackgroundColor as ILocalObject).GetObjectID);
end;

procedure TiOSNativeMemo.UpdateTextSelection;
begin
  View.setSelectedRange(NSMakeRange(Model.SelStart, Model.SelLength));
end;

procedure TiOSNativeMemo.UpdateTextSettings;
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
  FAttributedString := TNSMutableAttributedString.Wrap(FAttributedString.initWithString(StrToNSStr(Model.Lines.Text)));

  FAttributedString.beginEditing;
  try
    TextRange := NSMakeRange(0, Model.Lines.Text.Length);
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
  if TOSVersion.Check(7) and (Model.Caret.Color <> TAlphaColorRec.Null) then
    View.setTintColor(AlphaColorToUIColor(Model.Caret.Color));
end;

{ TiOSTextViewDelegate }

constructor TiOSTextViewDelegate.Create(const ANativeMemo: TiOSNativeMemo);
begin
  if ANativeMemo = nil then
    raise Exception.CreateFmt(SWrongParameter, ['ANativeMemo']); // do not localize
  inherited Create;
  FNativeMemo := ANativeMemo;
end;

procedure TiOSTextViewDelegate.textViewDidBeginEditing(textView: UITextView);
begin
  if not FNativeMemo.Control.IsFocused then
    FNativeMemo.Control.SetFocus;
end;

procedure TiOSTextViewDelegate.textViewDidChange(textView: UITextView);
var
  LinesCount: Integer;
begin
  FNativeMemo.Model.DisableNotify;
  try
    LinesCount := FNativeMemo.Model.Lines.Count;
    FNativeMemo.Model.Lines.Text := NSStrToStr(FNativeMemo.View.text);
  finally
    FNativeMemo.Model.EnableNotify;
  end;
  if (FNativeMemo.Model.Lines.Count <> LinesCount) and (FNativeMemo.Model.Lines.Count > 1) then
    FNativeMemo.Model.Change;
end;

procedure TiOSTextViewDelegate.textViewDidChangeSelection(textView: UITextView);
var
  SelectionRange: NSRange;
begin
  SelectionRange := FNativeMemo.View.selectedRange;
  FNativeMemo.Model.DisableNotify;
  try
    FNativeMemo.Model.SelStart := SelectionRange.location;
    FNativeMemo.Model.SelLength := SelectionRange.length;
  finally
    FNativeMemo.Model.EnableNotify;
  end;
end;

procedure TiOSTextViewDelegate.textViewDidEndEditing(textView: UITextView);
begin
end;

function TiOSTextViewDelegate.textViewShouldBeginEditing(textView: UITextView): Boolean;
begin
  Result := True;
  if FNativeMemo.Observers.IsObserving(TObserverMapping.EditLinkID) and (FNativeMemo <> nil) then
    if TLinkObservers.EditLinkEdit(FNativeMemo.Observers) then
      TLinkObservers.EditLinkModified(FNativeMemo.Observers)
    else
    begin
      TLinkObservers.EditLinkReset(FNativeMemo.Observers);
      Result := False;
    end;
  if FNativeMemo.Observers.IsObserving(TObserverMapping.ControlValueID) and Result then
    TLinkObservers.ControlValueModified(FNativeMemo.Observers);
  Result := Result and (FNativeMemo <> nil) and not FNativeMemo.Model.ReadOnly;
end;

function TiOSTextViewDelegate.textViewShouldChangeTextInRangeReplacementText(textView: UITextView;
  shouldChangeTextInRange: NSRange; replacementText: NSString): Boolean;
var
  Shift: TShiftState;
begin
  if FNativeMemo.Model.MaxLength > 0 then
    Result := (FNativeMemo.View.Text.Length - shouldChangeTextInRange.length + replacementText.length) <=
      FNativeMemo.Model.MaxLength
  else
    Result := True;

  if Result then
  begin
    if FSkipEditing then
      FSkipEditing := False
    else
    begin
      if (replacementText.length = 0) and (shouldChangeTextInRange.length > 0) then
        Result := FNativeMemo.PressKey(vkBack, #0, [])
      else if (replacementText.length = 1) and (shouldChangeTextInRange.length = 0) then
      begin
        if replacementText.characterAtIndex(0) = #10 then
          //ENTER key press
          Result := FNativeMemo.PressKey(vkReturn, #0, [])
        else
        begin
          Shift := [];
          if replacementText.characterAtIndex(0) <> LowerCase(replacementText.characterAtIndex(0)) then
            Shift := [ssShift];
          Result := FNativeMemo.PressKey(0, replacementText.characterAtIndex(0), Shift);
        end;
      end
      else if replacementText.length > 1 then
        //User have selected some word from the list of suggestions. Skipping next callback call.
        //If after inputing some characters user selects some word from the list of suggestions then this results in a
        //sequence "word" plus "space". If the user puts caret on some word and selects other word from the list of
        //suggestions then this results in a sequence "word" plus "empty string".
        FSkipEditing := True;
    end;
  end;
end;

function TiOSTextViewDelegate.textViewShouldEndEditing(textView: UITextView): Boolean;
begin
  FNativeMemo.Model.Change;
  Result := True;
end;

function TiOSTextViewDelegate.textViewShouldInteractWithTextAttachmentInRange(textView: UITextView;
  shouldInteractWithTextAttachment: NSTextAttachment; inRange: NSRange): Boolean;
begin
  Result := True;
end;

function TiOSTextViewDelegate.textViewShouldInteractWithURLInRange(textView: UITextView; shouldInteractWithURL: NSURL;
  inRange: NSRange): Boolean;
begin
  Result := True;
end;

initialization
  TPresentationProxyFactory.Current.Register(TMemo, TControlType.Platform, TiOSPresentationProxy<TiOSNativeMemo>);
finalization
  TPresentationProxyFactory.Current.Unregister(TMemo, TControlType.Platform, TiOSPresentationProxy<TiOSNativeMemo>);
end.
