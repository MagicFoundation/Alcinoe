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
     ALIosNativeControl,
     {$ELSE}
     FMX.Memo,
     {$ENDIF}
     FMX.types,
     Fmx.Graphics,
     Fmx.controls,
     AlFMXEdit,
     ALFmxObjects;

{$REGION ' WINDOWS / MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
type

  {**************************}
  TALStyledMemo = class(TMemo)
  private
    fWasEntered: Boolean;
    procedure OnApplyStyleLookupImpl(sender: Tobject);
  protected
    procedure DoEnter; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{$endif}
{$ENDREGION}

type

  {***************************}
  TALMemo = class(TALRectangle)
  private
    fDefStyleAttr: String;
    FAutoTranslate: Boolean;
    FAutoConvertFontFamily: boolean;
    fOnChangeTracking: TNotifyEvent;
    FTextSettings: TTextSettings;
    {$IF defined(android)}
    fTintColor: TalphaColor;
    fMemoControl: TALAndroidEdit;
    function GetAndroidEditText: JALEditText;
    {$ELSEIF defined(IOS)}
    fMemoControl: TALIosEdit;
    function GetIosTextField: TALIosTextField;
    {$ELSE}
    fReturnKeyType: TReturnKeyType;
    fTintColor: TalphaColor;
    fLineSpacingMultiplier: Single;
    fTextPrompt: String;
    fTextPromptColor: TalphaColor;
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
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
    procedure OnFontChanged(Sender: TObject);
    function getText: String;
    procedure SetText(const Value: String);
    procedure DoChangeTracking(Sender: TObject);
    procedure SetKeyboardType(Value: TVirtualKeyboardType);
    function GetKeyboardType: TVirtualKeyboardType;
    procedure SetCheckSpelling(const Value: Boolean);
    function GetCheckSpelling: Boolean;
    procedure SetReturnKeyType(const Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
    procedure SetDefStyleAttr(const Value: String);
    Function GetOnEnter: TNotifyEvent;
    Procedure SetOnEnter(AValue: TNotifyEvent);
    Function GetOnExit: TNotifyEvent;
    Procedure SetOnExit(AValue: TNotifyEvent);
    procedure CreateMemoControl;
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
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType default TReturnKeyType.Default;
    //property ReadOnly;
    //property MaxLength;
    //property FilterChar;
    property Text: String read getText write SetText;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property Hint;
    property TextPrompt: String read GetTextPrompt write setTextPrompt;
    property TextPromptColor: TAlphaColor read GetTextPromptColor write setTextPromptColor default TalphaColorRec.null; // << null mean use the default TextPromptColor
    property LineSpacingMultiplier: single read GetLineSpacingMultiplier write SetLineSpacingMultiplier stored LineSpacingMultiplierStored; // <<  Each line will have its height multiplied by LineSpacingMultiplier
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
    property OnKeyDown; // << not work under android - it's like this with their @{[^# virtual keyboard :(
    property OnKeyUp; // << not work under android - it's like this with their @{[^# virtual keyboard :(
    property OnEnter: TnotifyEvent Read GetOnEnter Write SetOnEnter;
    property OnExit: TnotifyEvent Read GetOnExit Write SetOnExit;
  end;

procedure Register;

implementation

uses system.Math,
     system.Math.Vectors,
     fmx.consts,
     {$IF defined(ANDROID)}
     {$ELSEIF defined(IOS)}
     System.SysUtils,
     Macapi.CoreFoundation,
     iOSapi.CoreGraphics,
     iOSapi.CocoaTypes,
     Macapi.Helpers,
     iOSapi.CoreText,
     FMX.Helpers.iOS,
     FMX.Consts,
     {$ELSE}
     FMX.Styles.Objects,
     FMX.BehaviorManager,
     FMX.StdCtrls,
     FMX.Memo.style,
     FMX.Layouts,
     {$ENDIF}
     ALCommon,
     ALString,
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

{$REGION ' WINDOWS / MACOS'}
{$IF defined(MSWINDOWS) or defined(_MACOS)}

{***************************************************}
constructor TALStyledMemo.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  fWasEntered := False;
  OnApplyStyleLookup := OnApplyStyleLookupImpl;
end;

{**}
type
  _TOpenControl = class (TControl);

{******************************}
procedure TALStyledMemo.DoEnter;
begin
  if not fWasEntered then begin
    _TOpenControl(content).FRecalcUpdateRect := True; // << without this the caret is not show when you first click on the Memo
                                                      // << #{\#@#{^ emb team they really don't test anything they do :(
                                                      // << mistake you can found in 1 min it's not normal they are in production
    fWasEntered := True;
  end;
  inherited;
end;

{**************************************************************}
procedure TALStyledMemo.OnApplyStyleLookupImpl(sender: Tobject);
Var aPaddingTop: Single;
    aPaddingRight: single;
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
      for j := 0 to controls[i].controls.Count - 1 do begin
        if (controls[i].Controls[j] is TLayout) then begin // << TLayout
          for k := 0 to controls[i].Controls[j].controls.Count - 1 do begin
             if (controls[i].Controls[j].Controls[k] is TActiveStyleObject) then begin // << TActiveStyleObject
              with (controls[i].Controls[j].Controls[k] as TActiveStyleObject) do begin
                ActiveLink.Clear;
                SourceLink.Clear;
                aPaddingTop := Padding.Top;
                aPaddingRight := Padding.right;
              end;
              for l := 0 to controls[i].Controls[j].Controls[k].controls.Count - 1 do begin
                if (controls[i].Controls[j].Controls[k].Controls[l] is TScrollBar) then begin // << TScrollBar
                  with (controls[i].Controls[j].Controls[k].Controls[l] as TScrollBar) do begin
                    if Align = TalignLayout.Right then begin
                      Align := TalignLayout.None;
                      Height := Tcontrol(self.Parent).Height;
                      anchors := [TAnchorKind.akright,TAnchorKind.akTop,TAnchorKind.akBottom];
                      Tcontrol(self.Parent).Padding.Right := Tcontrol(self.Parent).Padding.Right + width;
                      position.y := position.y - Tcontrol(self.Parent).Padding.Top - aPaddingTop;
                      position.x := position.x + Tcontrol(self.Parent).Padding.right + aPaddingRight;
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

end;

{$endif}
{$ENDREGION}

{*********************************************}
constructor TALMemo.Create(AOwner: TComponent);
begin
  inherited;
  fDefStyleAttr := '';
  FAutoTranslate := true;
  FAutoConvertFontFamily := True;
  fOnChangeTracking := nil;
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
  fReturnKeyType := TReturnKeyType.Default;
  fTextPrompt := '';
  fTextPromptColor := TalphaColorRec.Null;
  fLineSpacingMultiplier := 1;
  {$ENDIF}
  {$IF (not defined(IOS))}
  fTintColor := TalphaColorRec.Null;
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
  {$ELSEIF defined(ios)}
  FMemoControl := TALIosEdit.Create(self);
  FMemoControl.Parent := self;
  FMemoControl.Stored := False;
  FMemoControl.SetSubComponent(True);
  FMemoControl.Locked := True;
  FMemoControl.ReturnKeyType := tReturnKeyType.Default;  // noops operation
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
  FMemoControl.OnChangeTracking := DoChangeTracking;
  FMemoControl.KeyboardType := TVirtualKeyboardType.Default; // noops operation
  FMemoControl.CheckSpelling := True;
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
  Result := TSizeF.Create(100, 22);
end;

{********************}
{$IF defined(android)}
function TALMemo.GetAndroidEditText: JALEditText;
begin
  if FMemoControl = nil then CreateMemoControl;
  result := FMemoControl.EditText;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALMemo.GetIosTextField: TALIosTextField;
begin
  if FMemoControl = nil then CreateMemoControl;
  result := FMemoControl.TextField;
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
  {$IF defined(android) or defined(ios)}
  result := FMemoControl.TextPrompt;
  {$ELSE}
  result := fTextPrompt;
  {$ENDIF}
end;

{***************************************************}
procedure TALMemo.setTextPrompt(const Value: String);
begin
  if FMemoControl = nil then CreateMemoControl;
  {$IF defined(android) or defined(ios)}
  FMemoControl.TextPrompt := Value;
  {$ELSE}
  fTextPrompt := Value;
  {$ENDIF}
end;

{***********************************************}
function TALMemo.GetTextPromptColor: TAlphaColor;
begin
  if FMemoControl = nil then CreateMemoControl;
  {$IF defined(android) or defined(ios)}
  result := FMemoControl.TextPromptColor;
  {$ELSE}
  result := fTextPromptColor;
  {$ENDIF}
end;

{*************************************************************}
procedure TALMemo.setTextPromptColor(const Value: TAlphaColor);
begin
  if FMemoControl = nil then CreateMemoControl;
  {$IF defined(android) or defined(ios)}
  FMemoControl.TextPromptColor := Value;
  {$ELSE}
  fTextPromptColor := Value;
  {$ENDIF}
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
  {$IF defined(android) OR defined(android)}
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
  {$IF defined(ios) OR defined(android)}
  FMemoControl.ReturnKeyType := Value;
  {$ELSE}
  fReturnKeyType := Value;
  {$ENDIF}
end;

{************************************************}
function TALMemo.GetReturnKeyType: TReturnKeyType;
begin
  if FMemoControl = nil then CreateMemoControl;
  {$IF defined(android) OR defined(android)}
  result := FMemoControl.ReturnKeyType;
  {$ELSE}
  result := fReturnKeyType;
  {$ENDIF}
end;

{****************************************}
function TALMemo.GetOnEnter: TNotifyEvent;
begin
  if FMemoControl = nil then CreateMemoControl;
  Result := FMemoControl.OnEnter;
end;

{*************************************************}
procedure TALMemo.SetOnEnter(AValue: TNotifyEvent);
begin
  if FMemoControl = nil then CreateMemoControl;
  FMemoControl.OnEnter := Avalue;
end;

{***************************************}
function TALMemo.GetOnExit: TNotifyEvent;
begin
  if FMemoControl = nil then CreateMemoControl;
  Result := FMemoControl.OnExit;
end;

{************************************************}
procedure TALMemo.SetOnExit(AValue: TNotifyEvent);
begin
  if FMemoControl = nil then CreateMemoControl;
  FMemoControl.OnExit := Avalue;
end;

{**************************************************}
procedure TALMemo.DoChangeTracking(Sender: TObject);
begin
  if assigned(fOnChangeTracking) and (not (csLoading in componentState)) then
    fOnChangeTracking(self); // << yes need to send self instead of the FMemoControl
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
  ALLog('TALEdit.GetCanFocus', 'GetCanFocus', TalLogType.VERBOSE);
  {$ENDIF}
  if FMemoControl = nil then CreateMemoControl;
  result := inherited GetCanFocus;
  if result then begin
    FMemoControl.SetFocus;
    exit(false);   // << the canparentfocus is also set to false, so the TCommonCustomForm.NewFocusedControl(const Value: IControl)
                   //    will do nothing !
  end;
end;

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALMemo]);
end;

initialization
  RegisterFmxClasses([TALMemo]);

end.
