unit Alcinoe.FMX.Themes;

interface

{$I Alcinoe.inc}

uses
  System.Generics.Collections,
  Alcinoe.FMX.Edit,
  Alcinoe.FMX.StdCtrls;

Type
  TALApplyEditThemeInfo = record
  public
    Type
      TApplyThemeProc = Procedure(const AEdit: TALBaseEdit; const AFontSize: Single);
  public
    ApplyThemeProc: TApplyThemeProc;
    DefaultFontSize: Single;
    constructor create(const AApplyThemeProc: TApplyThemeProc; const ADefaultFontSize: Single);
  end;
  //--
  TALApplyButtonThemeInfo = record
  public
    Type
      TApplyThemeProc = Procedure(const AButton: TALButton; const AFontSize: Single);
  public
    ApplyThemeProc: TApplyThemeProc;
    DefaultFontSize: Single;
    constructor create(const AApplyThemeProc: TApplyThemeProc; const ADefaultFontSize: Single);
  end;
  //--
  TALApplyCheckBoxThemeInfo = record
  public
    Type
      TApplyThemeProc = Procedure(const ACheckBox: TALCheckBox; const AHeight: Single);
  public
    ApplyThemeProc: TApplyThemeProc;
    DefaultHeight: Single;
    constructor create(const AApplyThemeProc: TApplyThemeProc; const ADefaultHeight: Single);
  end;
  //--
  TALApplyRadioButtonThemeInfo = record
  public
    Type
      TApplyThemeProc = Procedure(const ARadioButton: TALRadioButton; const AHeight: Single);
  public
    ApplyThemeProc: TApplyThemeProc;
    DefaultHeight: Single;
    constructor create(const AApplyThemeProc: TApplyThemeProc; const ADefaultHeight: Single);
  end;
  //--
  TALApplySwitchThemeInfo = record
  public
    Type
      TApplyThemeProc = Procedure(const ASwitch: TALSwitch; const AHeight: Single);
  public
    ApplyThemeProc: TApplyThemeProc;
    DefaultHeight: Single;
    constructor create(const AApplyThemeProc: TApplyThemeProc; const ADefaultHeight: Single);
  end;
  //--
  TALApplyTrackBarThemeInfo = record
  public
    Type
      TApplyThemeProc = Procedure(const ATrackBar: TALCustomTrack; const AHeight: Single);
  public
    ApplyThemeProc: TApplyThemeProc;
    DefaultHeight: Single;
    constructor create(const AApplyThemeProc: TApplyThemeProc; const ADefaultHeight: Single);
  end;

var
  ALEditThemes: TDictionary<String, TALApplyEditThemeInfo>;
  ALMemoThemes: TDictionary<String, TALApplyEditThemeInfo>;
  ALButtonThemes: TDictionary<String, TALApplyButtonThemeInfo>;
  ALCheckBoxThemes: TDictionary<String, TALApplyCheckBoxThemeInfo>;
  ALRadioButtonThemes: TDictionary<String, TALApplyRadioButtonThemeInfo>;
  ALSwitchThemes: TDictionary<String, TALApplySwitchThemeInfo>;
  ALTrackBarThemes: TDictionary<String, TALApplyTrackBarThemeInfo>;
  ALRangeTrackBarThemes: TDictionary<String, TALApplyTrackBarThemeInfo>;

procedure ALApplyEditTheme(const ATheme: String; const AEdit: TALBaseEdit);
procedure ALApplyMemoTheme(const ATheme: String; const AMemo: TALBaseEdit);
procedure ALApplyButtonTheme(const ATheme: String; const AButton: TALButton);
procedure ALApplyCheckBoxTheme(const ATheme: String; const ACheckBox: TALCheckBox);
procedure ALApplyRadioButtonTheme(const ATheme: String; const ARadioButton: TALRadioButton);
procedure ALApplySwitchTheme(const ATheme: String; const ASwitch: TALSwitch);
procedure ALApplyTrackBarTheme(const ATheme: String; const ATrackBar: TALCustomTrack);
procedure ALApplyRangeTrackBarTheme(const ATheme: String; const ARangeTrackBar: TALCustomTrack);

implementation

uses
  System.SysUtils,
  System.Types,
  System.uitypes,
  System.Math,
  FMX.types,
  {$IF defined(ALDPK)}
  Vcl.Dialogs,
  {$ENDIF}
  Alcinoe.Common,
  Alcinoe.StringUtils,
  Alcinoe.FMX.Memo,
  Alcinoe.FMX.Graphics;

{****************************************************************************************}
function ALGetThemeSizeRatio(const ACaption: String; const ADefaultValue: Single): Single;
begin
  var LValueF: Single := ADefaultValue;
  {$IF defined(ALDPK)}
  While True do begin
    var LValueStr := InputBox(ACaption, '', ALFloatToStrW(ADefaultValue, ALDefaultFormatSettingsW));
    if ALTryStrToFloat(LValueStr,LValueF,ALDefaultFormatSettingsW) then break;
  end;
  {$ENDIF}
  Result := LValueF / ADefaultValue;
end;

{***************************************************************************************************************}
constructor TALApplyEditThemeInfo.create(const AApplyThemeProc: TApplyThemeProc; const ADefaultFontSize: Single);
begin
  ApplyThemeProc := AApplyThemeProc;
  DefaultFontSize := ADefaultFontSize;
end;

{*****************************************************************************************************************}
constructor TALApplyButtonThemeInfo.create(const AApplyThemeProc: TApplyThemeProc; const ADefaultFontSize: Single);
begin
  ApplyThemeProc := AApplyThemeProc;
  DefaultFontSize := ADefaultFontSize;
end;

{*****************************************************************************************************************}
constructor TALApplyCheckBoxThemeInfo.create(const AApplyThemeProc: TApplyThemeProc; const ADefaultHeight: Single);
begin
  ApplyThemeProc := AApplyThemeProc;
  DefaultHeight := ADefaultHeight;
end;

{********************************************************************************************************************}
constructor TALApplyRadioButtonThemeInfo.create(const AApplyThemeProc: TApplyThemeProc; const ADefaultHeight: Single);
begin
  ApplyThemeProc := AApplyThemeProc;
  DefaultHeight := ADefaultHeight;
end;

{***************************************************************************************************************}
constructor TALApplySwitchThemeInfo.create(const AApplyThemeProc: TApplyThemeProc; const ADefaultHeight: Single);
begin
  ApplyThemeProc := AApplyThemeProc;
  DefaultHeight := ADefaultHeight;
end;

{*****************************************************************************************************************}
constructor TALApplyTrackbarThemeInfo.create(const AApplyThemeProc: TApplyThemeProc; const ADefaultHeight: Single);
begin
  ApplyThemeProc := AApplyThemeProc;
  DefaultHeight := ADefaultHeight;
end;

//////////
// EDIT //
//////////

{*********************************************************************************}
procedure ALResetEditTheme(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  With AEdit do begin
    BeginUpdate;
    Try
      var LRatio: Single := AFontSize / TextSettings.Font.DefaultSize;
      if AEdit is TALEdit then TALEdit(AEdit).AutoSize := True
      else if AEdit is TALMemo then TALMemo(AEdit).AutoSizeLineCount := 0;
      //Margins.Rect := ALScaleRect(Margins.DefaultValue, LRatio).RoundTo(-2);
      Padding.Rect := ALScaleRect(Padding.DefaultValue, LRatio).RoundTo(-2);
      TouchTargetExpansion.Rect := ALScaleRect(TouchTargetExpansion.DefaultValue, LRatio).RoundTo(-2);
      Corners := AllCorners;
      Sides := AllSides;
      XRadius := 0;
      YRadius := 0;
      TintColor := TalphaColors.null;
      PromptTextcolor := TAlphaColors.null;
      DefStyleAttr := '';
      DefStyleRes := '';
      Fill.Reset;
      Stroke.Reset;
      Stroke.Thickness := RoundTo(Stroke.DefaultThickness * LRatio, -2);
      Shadow.Reset;
      //--
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.Font.Size := RoundTo(TextSettings.Font.DefaultSize * LRatio, -2);
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.LetterSpacing := RoundTo(TextSettings.DefaultLetterSpacing * LRatio, -2);
      //--
      LPrevIsHtml := LabelTextSettings.IsHtml;
      LabelTextSettings.Reset;
      LabelTextSettings.Font.Size := RoundTo(LabelTextSettings.Font.DefaultSize * LRatio, -2);
      LabelTextSettings.IsHtml := LPrevIsHtml;
      LabelTextSettings.LetterSpacing := RoundTo(LabelTextSettings.DefaultLetterSpacing * LRatio, -2);
      LabelTextSettings.Margins.Rect := ALScaleRect(LabelTextSettings.Margins.DefaultValue, LRatio).RoundTo(-2);
      //--
      LPrevIsHtml := SupportingTextSettings.IsHtml;
      SupportingTextSettings.Reset;
      SupportingTextSettings.Font.Size := RoundTo(SupportingTextSettings.Font.DefaultSize * LRatio, -2);
      SupportingTextSettings.IsHtml := LPrevIsHtml;
      SupportingTextSettings.LetterSpacing := RoundTo(SupportingTextSettings.DefaultLetterSpacing * LRatio, -2);
      //SupportingTextSettings.Margins.Rect := ALScaleRect(SupportingTextSettings.Margins.DefaultValue, LRatio).RoundTo(-2);
      //--
      StateStyles.Reset;
    Finally
      EndUpdate;
    End;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterial3LightFilledEditTheme(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditTheme(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 12*LRatio{Top}, 16*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      Corners := [TCorner.TopLeft, Tcorner.TopRight];
      Sides := [TSide.Bottom];
      XRadius := RoundTo(4 * LRatio, -2);
      YRadius := RoundTo(4 * LRatio, -2);
      DefStyleAttr := 'Material3LightFilledEditTextStyle';
      TintColor := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      Fill.Color := $FFE6E0E9; // md.sys.color.surface-container-highest / md.ref.palette.neutral90
      Stroke.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      TextSettings.Font.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
      LabelTextSettings.Font.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      LabelTextSettings.Margins.Rect := TRectF.Create(0,-4*LRatio,0,4*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextcolor := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.04); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.PromptTextcolor := StateStyles.Disabled.LabelTextSettings.Font.Color;
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * LRatio, -2);
      StateStyles.Focused.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Focused.LabelTextSettings.Inherit := False;
      StateStyles.Focused.LabelTextSettings.Font.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Focused.PromptTextcolor := StateStyles.Focused.LabelTextSettings.Font.Color;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#e4964192-72ad-414f-85b4-4b4357abb83c
procedure ALApplyMaterial3LightOutlinedEditTheme(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditTheme(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 16*LRatio{Top}, 16*LRatio{Right}, 16*LRatio{Bottom}).RoundTo(-2);
      XRadius := RoundTo(4 * LRatio, -2);
      YRadius := RoundTo(4 * LRatio, -2);
      DefStyleAttr := 'Material3LightOutlinedEditTextStyle';
      TintColor := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      Fill.Color := $FFFFFFFF;
      Stroke.Color := $FF79747E; // md.sys.color.outline / md.ref.palette.neutral-variant50
      TextSettings.Font.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.floating;
      LabelTextSettings.Font.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      LabelTextSettings.Margins.Rect := TRectF.Create(0,0,0,-6*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextcolor := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.PromptTextcolor := StateStyles.Disabled.LabelTextSettings.Font.Color;
      //--Hovered--
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Hovered.PromptTextcolor := StateStyles.Hovered.LabelTextSettings.Font.Color;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * LRatio, -2);
      StateStyles.Focused.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Focused.LabelTextSettings.Inherit := False;
      StateStyles.Focused.LabelTextSettings.Font.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Focused.PromptTextcolor := StateStyles.Focused.LabelTextSettings.Font.Color;

    finally
      EndUpdate;
    end;
  end;
end;

{**************************************}
//https://llama.meta.com/llama-downloads
procedure ALApplyFacebookOutlinedEditTheme(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditTheme(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 12*LRatio{Top}, 16*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := RoundTo(8 * LRatio, -2);
      YRadius := RoundTo(8 * LRatio, -2);
      TintColor := $FF1c2b33;
      Fill.Color := $FFFFFFFF;
      Stroke.Color := $FFdee3e9;
      TextSettings.Font.Color := $FF1c2b33;
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
      LabelTextSettings.Font.Color := $FF465a69;
      LabelTextSettings.Margins.Rect := TRectF.Create(0,-4*LRatio,0,4*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.Color := $FF465a69;
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextcolor := $FF465a69;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALBlendColor($FFFFFFFF, $FF1c2b33, 0.12);
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1c2b33, 0.38);
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1c2b33, 0.38);
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1c2b33, 0.38);
      StateStyles.Disabled.PromptTextcolor := StateStyles.Disabled.LabelTextSettings.Font.Color;
      //--Hovered--
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Color := $FF1d65c1;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterial3DarkFilledEditTheme(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditTheme(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 12*LRatio{Top}, 16*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      Corners := [TCorner.TopLeft, Tcorner.TopRight];
      Sides := [TSide.Bottom];
      XRadius := RoundTo(4 * LRatio, -2);
      YRadius := RoundTo(4 * LRatio, -2);
      DefStyleAttr := 'Material3DarkFilledEditTextStyle';
      TintColor := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      Fill.Color := $FF36343B; // md.sys.color.surface-container-highest / md.ref.palette.neutral22
      Stroke.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      TextSettings.Font.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
      LabelTextSettings.Font.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      LabelTextSettings.Margins.Rect := TRectF.Create(0,-4*LRatio,0,4*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextcolor := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.04); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.PromptTextcolor := StateStyles.Disabled.LabelTextSettings.Font.Color;
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * LRatio, -2);
      StateStyles.Focused.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Focused.LabelTextSettings.Inherit := False;
      StateStyles.Focused.LabelTextSettings.Font.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Focused.PromptTextcolor := StateStyles.Focused.LabelTextSettings.Font.Color;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#e4964192-72ad-414f-85b4-4b4357abb83c
procedure ALApplyMaterial3DarkOutlinedEditTheme(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditTheme(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 16*LRatio{Top}, 16*LRatio{Right}, 16*LRatio{Bottom}).RoundTo(-2);
      XRadius := RoundTo(4 * LRatio, -2);
      YRadius := RoundTo(4 * LRatio, -2);
      DefStyleAttr := 'Material3DarkOutlinedEditTextStyle';
      TintColor := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      Fill.Color := $FF000000;
      Stroke.Color := $FF938F99; // md.sys.color.outline / md.ref.palette.neutral-variant60
      TextSettings.Font.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.floating;
      LabelTextSettings.Font.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      LabelTextSettings.Margins.Rect := TRectF.Create(0,0,0,-6*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextcolor := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.PromptTextcolor := StateStyles.Disabled.LabelTextSettings.Font.Color;
      //--Hovered--
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Hovered.PromptTextcolor := StateStyles.Hovered.LabelTextSettings.Font.Color;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * LRatio, -2);
      StateStyles.Focused.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Focused.LabelTextSettings.Inherit := False;
      StateStyles.Focused.LabelTextSettings.Font.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Focused.PromptTextcolor := StateStyles.Focused.LabelTextSettings.Font.Color;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterial3LightFilledErrorEditTheme(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditTheme(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 12*LRatio{Top}, 16*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      Corners := [TCorner.TopLeft, Tcorner.TopRight];
      Sides := [TSide.Bottom];
      XRadius := RoundTo(4 * LRatio, -2);
      YRadius := RoundTo(4 * LRatio, -2);
      DefStyleAttr := 'Material3LightFilledErrorEditTextStyle';
      TintColor := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      Fill.Color := $FFE6E0E9; // md.sys.color.surface-container-highest / md.ref.palette.neutral90
      Stroke.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      TextSettings.Font.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
      LabelTextSettings.Font.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      LabelTextSettings.Margins.Rect := TRectF.Create(0,-4*LRatio,0,4*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextcolor := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.04); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.PromptTextcolor := StateStyles.Disabled.LabelTextSettings.Font.Color;
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.Color := $FF410E0B; // md.sys.color.on-error-container / md.ref.palette.error10
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.Color := $FF410E0B; // md.sys.color.on-error-container / md.ref.palette.error10
      StateStyles.Hovered.PromptTextcolor := StateStyles.Hovered.LabelTextSettings.Font.Color;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * LRatio, -2);
      StateStyles.Focused.PromptTextcolor := LabelTextSettings.Font.Color;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#e4964192-72ad-414f-85b4-4b4357abb83c
procedure ALApplyMaterial3LightOutlinedErrorEditTheme(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditTheme(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 16*LRatio{Top}, 16*LRatio{Right}, 16*LRatio{Bottom}).RoundTo(-2);
      XRadius := RoundTo(4 * LRatio, -2);
      YRadius := RoundTo(4 * LRatio, -2);
      DefStyleAttr := 'Material3LightOutlinedErrorEditTextStyle';
      TintColor := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      Fill.Color := $FFFFFFFF;
      Stroke.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      TextSettings.Font.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.floating;
      LabelTextSettings.Font.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      LabelTextSettings.Margins.Rect := TRectF.Create(0,0,0,-6*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextcolor := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.PromptTextcolor := StateStyles.Disabled.LabelTextSettings.Font.Color;
      //--Hovered--
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.Color := $FF410E0B; // md.sys.color.on-error-container / md.ref.palette.error10
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.Color := $FF410E0B; // md.sys.color.on-error-container / md.ref.palette.error10
      StateStyles.Hovered.PromptTextcolor := StateStyles.Hovered.LabelTextSettings.Font.Color;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * LRatio, -2);
      StateStyles.Focused.PromptTextcolor := LabelTextSettings.Font.Color;

    finally
      EndUpdate;
    end;
  end;
end;

{**************************************}
//https://llama.meta.com/llama-downloads
procedure ALApplyFacebookOutlinedErrorEditTheme(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditTheme(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 12*LRatio{Top}, 16*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := RoundTo(8 * LRatio, -2);
      YRadius := RoundTo(8 * LRatio, -2);
      DefStyleAttr := '';
      TintColor := $FF1c2b33;
      Fill.Color := $FFFFFFFF;
      Stroke.Color := $FFc80a28;
      TextSettings.Font.Color := $FF1c2b33;
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
      LabelTextSettings.Font.Color := $FF465a69;
      LabelTextSettings.Margins.Rect := TRectF.Create(0,-4*LRatio,0,4*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.Color := $FFc80a28;
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextcolor := $FF465a69;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALBlendColor($FFFFFFFF, $FF1c2b33, 0.12);
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1c2b33, 0.38);
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1c2b33, 0.38);
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.Color := ALBlendColor($FFFFFFFF, $FF1c2b33, 0.38);
      StateStyles.Disabled.PromptTextcolor := StateStyles.Disabled.LabelTextSettings.Font.Color;
      //--Hovered--
      //--Focused--

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterial3DarkFilledErrorEditTheme(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditTheme(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 12*LRatio{Top}, 16*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      Corners := [TCorner.TopLeft, Tcorner.TopRight];
      Sides := [TSide.Bottom];
      XRadius := RoundTo(4 * LRatio, -2);
      YRadius := RoundTo(4 * LRatio, -2);
      DefStyleAttr := 'Material3DarkFilledErrorEditTextStyle';
      TintColor := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      Fill.Color := $FF36343B; // md.sys.color.surface-container-highest / md.ref.palette.neutral22
      Stroke.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      TextSettings.Font.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
      LabelTextSettings.Font.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      LabelTextSettings.Margins.Rect := TRectF.Create(0,-4*LRatio,0,4*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextcolor := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.04); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.PromptTextcolor := StateStyles.Disabled.LabelTextSettings.Font.Color;
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.Color := $FFF9DEDC; // md.sys.color.on-error-container / md.ref.palette.error90
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.Color := $FFF9DEDC; // md.sys.color.on-error-container / md.ref.palette.error90
      StateStyles.Hovered.PromptTextcolor := StateStyles.Hovered.LabelTextSettings.Font.Color;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * LRatio, -2);
      StateStyles.Focused.PromptTextcolor := LabelTextSettings.Font.Color;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#e4964192-72ad-414f-85b4-4b4357abb83c
procedure ALApplyMaterial3DarkOutlinedErrorEditTheme(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditTheme(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 16*LRatio{Top}, 16*LRatio{Right}, 16*LRatio{Bottom}).RoundTo(-2);
      XRadius := RoundTo(4 * LRatio, -2);
      YRadius := RoundTo(4 * LRatio, -2);
      DefStyleAttr := 'Material3DarkOutlinedErrorEditTextStyle';
      TintColor := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      Fill.Color := $FF000000;
      Stroke.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      TextSettings.Font.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.floating;
      LabelTextSettings.Font.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      LabelTextSettings.Margins.Rect := TRectF.Create(0,0,0,-6*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextcolor := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.Color := ALBlendColor($FF000000, $FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.PromptTextcolor := StateStyles.Disabled.LabelTextSettings.Font.Color;
      //--Hovered--
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.Color := $FFF9DEDC; // md.sys.color.on-error-container / md.ref.palette.error90
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.Color := $FFF9DEDC; // md.sys.color.on-error-container / md.ref.palette.error90
      StateStyles.Hovered.PromptTextcolor := StateStyles.Hovered.LabelTextSettings.Font.Color;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * LRatio, -2);
      StateStyles.Focused.PromptTextcolor := LabelTextSettings.Font.Color;

    finally
      EndUpdate;
    end;
  end;
end;

{*************************************************************************}
procedure ALApplyEditTheme(const ATheme: String; const AEdit: TALBaseEdit);
begin
  Var LApplyEditThemeInfo: TALApplyEditThemeInfo;
  If not ALEditThemes.TryGetValue(Atheme,LApplyEditThemeInfo) then
    Raise Exception.Createfmt('The theme "%s" could not be found', [ATheme]);
  AEdit.BeginUpdate;
  try
    LApplyEditThemeInfo.ApplyThemeProc(AEdit, LApplyEditThemeInfo.DefaultFontSize);
  finally
    AEdit.EndUpdate;
  end;
end;

//////////
// MEMO //
//////////

{*************************************************************************}
procedure ALApplyMemoTheme(const ATheme: String; const AMemo: TALBaseEdit);
begin
  Var LApplyEditThemeInfo: TALApplyEditThemeInfo;
  If not ALMemoThemes.TryGetValue(Atheme,LApplyEditThemeInfo) then
    Raise Exception.Createfmt('The theme "%s" could not be found', [ATheme]);
  AMemo.BeginUpdate;
  try
    LApplyEditThemeInfo.ApplyThemeProc(AMemo, LApplyEditThemeInfo.DefaultFontSize);
  finally
    AMemo.EndUpdate;
  end;
end;


////////////
// BUTTON //
////////////

{***********************************************************************************}
procedure ALResetButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  With AButton do begin
    BeginUpdate;
    Try
      var LRatio: Single := AFontSize / TextSettings.Font.DefaultSize;
      AutoSize := True;
      //Margins.Rect := ALScaleRect(Margins.DefaultValue, LRatio).RoundTo(-2);
      Padding.Rect := ALScaleRect(Padding.DefaultValue, LRatio).RoundTo(-2);
      TouchTargetExpansion.Rect := ALScaleRect(TouchTargetExpansion.DefaultValue, LRatio).RoundTo(-2);
      Corners := AllCorners;
      Sides := AllSides;
      XRadius := 0;
      YRadius := 0;
      Fill.Reset;
      Stroke.Reset;
      Stroke.Thickness := RoundTo(Stroke.DefaultThickness * LRatio, -2);
      Shadow.Reset;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.Font.Size := RoundTo(TextSettings.Font.DefaultSize * LRatio, -2);
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.LetterSpacing := RoundTo(TextSettings.DefaultLetterSpacing * LRatio, -2);
      StateStyles.Reset;
    Finally
      EndUpdate;
    End;
  end;
end;

{******************************************************************************************}
procedure ALApplyWindowsButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonTheme(AButton, AFontSize * LRatio);
      //--Disabled--
      //--Hovered--
      StateStyles.Hovered.Fill.assign(Fill);
      StateStyles.Hovered.Fill.Inherit := False;
      StateStyles.Hovered.Fill.Color := $FFe5f1fb;
      StateStyles.Hovered.Stroke.Assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.Color := $FF0078d7;
      //--Pressed--
      StateStyles.Pressed.Fill.assign(Fill);
      StateStyles.Pressed.Fill.Inherit := False;
      StateStyles.Pressed.Fill.Color := $FFcce4f7;
      StateStyles.Pressed.Stroke.Assign(Stroke);
      StateStyles.Pressed.Stroke.Inherit := False;
      StateStyles.Pressed.Stroke.Color := $FF005499;
      //--Focused--
      StateStyles.Focused.Stroke.Assign(Stroke);
      StateStyles.focused.Stroke.Inherit := False;
      StateStyles.focused.Stroke.Color := $FF0078d7;
      StateStyles.focused.Stroke.Thickness := RoundTo(2 * LRatio, -2);

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#cbfd91a6-d688-4be7-9a69-672549de3ea9
procedure ALApplyMaterial3LightFilledButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonTheme(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(24*LRatio{Left}, 12*LRatio{Top}, 24*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      Stroke.Color := Talphacolors.Null;
      TextSettings.Font.Color := $FFFFFFFF; // md.sys.color.on-primary // md.ref.palette.primary100
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorAlpha($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.Color := ALSetColorAlpha($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      StateStyles.Hovered.Shadow.blur := RoundTo(2 * LRatio, -2);
      StateStyles.Hovered.Shadow.OffsetY := RoundTo(1 * LRatio, -2);
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#4a0c06da-0b2f-47de-a583-97e0ae80b5a5
procedure ALApplyMaterial3LightOutlinedButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonTheme(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(24*LRatio{Left}, 12*LRatio{Top}, 24*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.Color := Talphacolors.Null;
      Stroke.Color := $FF79747E; // md.sys.color.outline / md.ref.palette.neutral-variant50
      TextSettings.Font.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.Assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALSetColorAlpha($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.inherit := False;
      StateStyles.Focused.Stroke.Color := $FF6750A4;  // md.sys.color.primary / md.ref.palette.primary40

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#398d84eb-fc8a-4c8a-bfb4-82d2e85dee4d
procedure ALApplyMaterial3LightTextButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonTheme(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(12*LRatio{Left}, 12*LRatio{Top}, 12*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.Color := Talphacolors.Null;
      Stroke.Color := Talphacolors.Null;
      TextSettings.Font.Color := $FF6750A4; // md.sys.color.primary // md.ref.palette.primary40
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#c75be779-5a59-4748-98d4-e47fc888d0b1
procedure ALApplyMaterial3LightElevatedButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonTheme(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(24*LRatio{Left}, 12*LRatio{Top}, 24*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.Color := $FFF7F2FA; // md.sys.color.surface-container-low / md.ref.palette.neutral96
      Stroke.Color := Talphacolors.Null;
      TextSettings.Font.Color := $FF6750A4; // md.sys.color.primary // md.ref.palette.primary40
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      Shadow.Color := ALSetColorAlpha($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      Shadow.blur := RoundTo(2 * LRatio, -2);
      Shadow.OffsetY := RoundTo(1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorAlpha($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.Shadow.inherit := False;
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.Color := ALSetColorAlpha($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      StateStyles.Hovered.Shadow.blur := RoundTo(3 * LRatio, -2);
      StateStyles.Hovered.Shadow.OffsetY := RoundTo(1 * LRatio, -2);
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#6ce8b926-87c4-4600-9bec-5deb4aaa65d8
procedure ALApplyMaterial3LightTonalButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonTheme(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(24*LRatio{Left}, 12*LRatio{Top}, 24*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.Color := $FFE8DEF8; // md.sys.color.secondary-container / md.ref.palette.secondary90
      Stroke.Color := Talphacolors.Null;
      TextSettings.Font.Color := $FF1D192B; // md.sys.color.on-secondary-container // md.ref.palette.secondary10
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorAlpha($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.Color := ALSetColorAlpha($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      StateStyles.Hovered.Shadow.blur := RoundTo(2 * LRatio, -2);
      StateStyles.Hovered.Shadow.OffsetY := RoundTo(1 * LRatio, -2);
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#5309610a-4515-44f9-830d-880e2a2240a2
procedure ALApplyMaterial3LightFilledIconButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the button', 40);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonTheme(AButton);
      Width := RoundTo(40 * LRatio, -2);
      Height := RoundTo(40 * LRatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*LRatio,8*LRatio,8*LRatio,8*LRatio).RoundTo(-2);
      Fill.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      Fill.ResourceName := LPrevResourceName;
      Stroke.Color := TALphaColors.Null;
      StateStyles.Transition.Duration := 0.2;
      //Filled icon button icon color: $FFFFFFFF
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorAlpha($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      //Filled icon button disabled icon color: $FF1D1B20 with 0.38 opacity
      //--Hovered--
      StateStyles.Hovered.StateLayer.Color := $FFFFFFFF; // md.sys.color.on-primary / md.ref.palette.primary100
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //Filled icon button hover icon color: $FFFFFFFF
      //--Pressed--
      StateStyles.Pressed.StateLayer.Color := $FFFFFFFF; // md.sys.color.on-primary / md.ref.palette.primary100
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Filled icon button pressed icon color: $FFFFFFFF
      //--Focused--
      StateStyles.Focused.StateLayer.Color := $FFFFFFFF; // md.sys.color.on-primary / md.ref.palette.primary100
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Filled icon button focus icon color: $FFFFFFFF

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#ba97cf8a-2112-47dc-af87-2e32aabccdde
procedure ALApplyMaterial3LightTonalIconButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the button', 40);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonTheme(AButton);
      Width := RoundTo(40 * LRatio, -2);
      Height := RoundTo(40 * LRatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*LRatio,8*LRatio,8*LRatio,8*LRatio).RoundTo(-2);
      Fill.Color := $FFE8DEF8; // md.sys.color.secondary-container / md.ref.palette.secondary90
      Fill.ResourceName := LPrevResourceName;
      Stroke.Color := TALphaColors.Null;
      StateStyles.Transition.Duration := 0.2;
      //Filled tonal icon button icon color: $FF1D192B
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorAlpha($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      //Filled tonal icon button disabled icon color: $FF1D1B20 with 0.38 opacity
      //--Hovered--
      StateStyles.Hovered.StateLayer.Color := $FF1D192B; // md.sys.color.on-secondary-container / md.ref.palette.secondary10
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //Filled tonal icon button hover icon color: $FF1D192B
      //--Pressed--
      StateStyles.Pressed.StateLayer.Color := $FF1D192B; // md.sys.color.on-secondary-container / md.ref.palette.secondary10
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Filled tonal icon button pressed icon color: $FF1D192B
      //--Focused--
      StateStyles.Focused.StateLayer.Color := $FF1D192B; // md.sys.color.on-secondary-container / md.ref.palette.secondary10
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Filled tonal icon button focus icon color: $FF1D192B

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#05e02b7f-ebf2-4f02-9709-8230db3702b4
procedure ALApplyMaterial3LightOutlinedIconButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the button', 40);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonTheme(AButton);
      Width := RoundTo(40 * LRatio, -2);
      Height := RoundTo(40 * LRatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*LRatio,8*LRatio,8*LRatio,8*LRatio).RoundTo(-2);
      Fill.Color := TalphaColorRec.Null;
      Fill.ResourceName := LPrevResourceName;
      Stroke.Color := $FF79747E; // md.sys.color.outline / md.ref.palette.neutral-variant50
      StateStyles.Transition.Duration := 0.2;
      //Outlined icon button unselected icon color: $FF49454F
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorAlpha($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      StateStyles.Disabled.Stroke.Assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALSetColorAlpha($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      //Outlined icon button disabled icon color: $FF1D1B20 with 0.38 opacity
      //--Hovered--
      StateStyles.Hovered.StateLayer.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //Outlined icon button unselected hover icon color: $FF49454F
      //--Pressed--
      StateStyles.Pressed.StateLayer.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Outlined icon button unselected pressed icon color: $FF49454F
      //--Focused--
      StateStyles.Focused.StateLayer.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Outlined icon button focus icon color: $FF49454F

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#e63a9b45-a20c-402c-8cc5-2c67ad8aae25
procedure ALApplyMaterial3LightStandardIconButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the button', 40);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonTheme(AButton);
      Width := RoundTo(40 * LRatio, -2);
      Height := RoundTo(40 * LRatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*LRatio,8*LRatio,8*LRatio,8*LRatio).RoundTo(-2);
      Fill.ResourceName := LPrevResourceName;
      Fill.Color := TalphaColors.Null;
      Stroke.Color := TALphaColors.Null;
      StateStyles.Transition.Duration := 0.2;
      //Icon button unselected icon color: $FF49454F
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      if LPrevDisabledResourceName <> '' then begin
        StateStyles.Disabled.Fill.Assign(Fill);
        StateStyles.Disabled.Fill.Inherit := False;
        StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      end;
      //Icon button disabled icon color: $FF1D1B20 with 0.38 opacity
      //--Hovered--
      StateStyles.Hovered.StateLayer.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //Icon button unselected hover icon color: $FF49454F
      //--Pressed--
      StateStyles.Pressed.StateLayer.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Icon button unselected pressed icon color: $FF49454F
      //--Focused--
      StateStyles.Focused.StateLayer.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Icon button unselected focus icon color: $FF49454F

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#cbfd91a6-d688-4be7-9a69-672549de3ea9
procedure ALApplyMaterial3DarkFilledButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonTheme(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(24*LRatio{Left}, 12*LRatio{Top}, 24*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      Stroke.Color := Talphacolors.Null;
      TextSettings.Font.Color := $FF381E72; // md.sys.color.on-primary / md.ref.palette.primary20
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorAlpha($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.Color := ALSetColorAlpha($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      StateStyles.Hovered.Shadow.blur := RoundTo(2 * LRatio, -2);
      StateStyles.Hovered.Shadow.OffsetY := RoundTo(1 * LRatio, -2);
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#4a0c06da-0b2f-47de-a583-97e0ae80b5a5
procedure ALApplyMaterial3DarkOutlinedButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonTheme(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(24*LRatio{Left}, 12*LRatio{Top}, 24*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.Color := Talphacolors.Null;
      Stroke.Color := $FF938F99; // md.sys.color.outline / md.ref.palette.neutral-variant60
      TextSettings.Font.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.Assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALSetColorAlpha($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.inherit := False;
      StateStyles.Focused.Stroke.Color := $FFD0BCFF;  // md.sys.color.primary / md.ref.palette.primary80

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#398d84eb-fc8a-4c8a-bfb4-82d2e85dee4d
procedure ALApplyMaterial3DarkTextButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonTheme(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(12*LRatio{Left}, 12*LRatio{Top}, 12*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.Color := Talphacolors.Null;
      Stroke.Color := Talphacolors.Null;
      TextSettings.Font.Color := $FFD0BCFF; // md.sys.color.primary // md.ref.palette.primary80
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#c75be779-5a59-4748-98d4-e47fc888d0b1
procedure ALApplyMaterial3DarkElevatedButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonTheme(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(24*LRatio{Left}, 12*LRatio{Top}, 24*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.Color := $FF1D1B20; // md.sys.color.surface-container-low / md.ref.palette.neutral10
      Stroke.Color := Talphacolors.Null;
      TextSettings.Font.Color := $FFD0BCFF; // md.sys.color.primary // md.ref.palette.primary80
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      Shadow.Color := ALSetColorAlpha($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      Shadow.blur := RoundTo(2 * LRatio, -2);
      Shadow.OffsetY := RoundTo(1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorAlpha($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.Shadow.inherit := False;
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.Color := ALSetColorAlpha($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      StateStyles.Hovered.Shadow.blur := RoundTo(3 * LRatio, -2);
      StateStyles.Hovered.Shadow.OffsetY := RoundTo(1 * LRatio, -2);
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#6ce8b926-87c4-4600-9bec-5deb4aaa65d8
procedure ALApplyMaterial3DarkTonalButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonTheme(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(24*LRatio{Left}, 12*LRatio{Top}, 24*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.Color := $FF4A4458; // md.sys.color.secondary-container / md.ref.palette.secondary30
      Stroke.Color := Talphacolors.Null;
      TextSettings.Font.Color := $FFE8DEF8; // md.sys.color.on-secondary-container // md.ref.palette.secondary90
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorAlpha($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.Color := ALSetColorAlpha($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      StateStyles.Hovered.Shadow.blur := RoundTo(2 * LRatio, -2);
      StateStyles.Hovered.Shadow.OffsetY := RoundTo(1 * LRatio, -2);
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#5309610a-4515-44f9-830d-880e2a2240a2
procedure ALApplyMaterial3DarkFilledIconButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the button', 40);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonTheme(AButton);
      Width := RoundTo(40 * LRatio, -2);
      Height := RoundTo(40 * LRatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*LRatio,8*LRatio,8*LRatio,8*LRatio).RoundTo(-2);
      Fill.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      Fill.ResourceName := LPrevResourceName;
      Stroke.Color := TALphaColors.Null;
      StateStyles.Transition.Duration := 0.2;
      //Filled icon button icon color: $FF381E72
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorAlpha($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      //Filled icon button disabled icon color: $FFE6E0E9 with 0.38 opacity
      //--Hovered--
      StateStyles.Hovered.StateLayer.Color := $FF381E72; // md.sys.color.on-primary / md.ref.palette.primary20
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //Filled icon button hover icon color: $FF381E72
      //--Pressed--
      StateStyles.Pressed.StateLayer.Color := $FF381E72; // md.sys.color.on-primary / md.ref.palette.primary20
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Filled icon button pressed icon color: $FF381E72
      //--Focused--
      StateStyles.Focused.StateLayer.Color := $FF381E72; // md.sys.color.on-primary / md.ref.palette.primary20
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Filled icon button focus icon color: $FF381E72

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#ba97cf8a-2112-47dc-af87-2e32aabccdde
procedure ALApplyMaterial3DarkTonalIconButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the button', 40);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonTheme(AButton);
      Width := RoundTo(40 * LRatio, -2);
      Height := RoundTo(40 * LRatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*LRatio,8*LRatio,8*LRatio,8*LRatio).RoundTo(-2);
      Fill.Color := $FF4A4458; // md.sys.color.secondary-container / md.ref.palette.secondary30
      Fill.ResourceName := LPrevResourceName;
      Stroke.Color := TALphaColors.Null;
      StateStyles.Transition.Duration := 0.2;
      //Filled tonal icon button icon color: $FFE8DEF8
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorAlpha($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      //Filled tonal icon button disabled icon color: $FFE6E0E9 with 0.38 opacity
      //--Hovered--
      StateStyles.Hovered.StateLayer.Color := $FFE8DEF8; // md.sys.color.on-secondary-container / md.ref.palette.secondary90
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //Filled tonal icon button hover icon color: $FFE8DEF8
      //--Pressed--
      StateStyles.Pressed.StateLayer.Color := $FFE8DEF8; // md.sys.color.on-secondary-container / md.ref.palette.secondary90
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Filled tonal icon button pressed icon color: $FFE8DEF8
      //--Focused--
      StateStyles.Focused.StateLayer.Color := $FFE8DEF8; // md.sys.color.on-secondary-container / md.ref.palette.secondary90
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Filled tonal icon button focus icon color: $FFE8DEF8

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#05e02b7f-ebf2-4f02-9709-8230db3702b4
procedure ALApplyMaterial3DarkOutlinedIconButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the button', 40);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonTheme(AButton);
      Width := RoundTo(40 * LRatio, -2);
      Height := RoundTo(40 * LRatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*LRatio,8*LRatio,8*LRatio,8*LRatio).RoundTo(-2);
      Fill.Color := TalphaColorRec.Null;
      Fill.ResourceName := LPrevResourceName;
      Stroke.Color := $FF938F99; // md.sys.color.outline / md.ref.palette.neutral-variant60
      StateStyles.Transition.Duration := 0.2;
      //Outlined icon button unselected icon color: $FFCAC4D0
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorAlpha($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      StateStyles.Disabled.Stroke.Assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALSetColorAlpha($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      //Outlined icon button disabled icon color: $FFE6E0E9 with 0.38 opacity
      //--Hovered--
      StateStyles.Hovered.StateLayer.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //Outlined icon button unselected hover icon color: $FFCAC4D0
      //--Pressed--
      StateStyles.Pressed.StateLayer.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Outlined icon button unselected pressed icon color: $FFCAC4D0
      //--Focused--
      StateStyles.Focused.StateLayer.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Outlined icon button focus icon color: $FFCAC4D0

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#e63a9b45-a20c-402c-8cc5-2c67ad8aae25
procedure ALApplyMaterial3DarkStandardIconButtonTheme(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the button', 40);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonTheme(AButton);
      Width := RoundTo(40 * LRatio, -2);
      Height := RoundTo(40 * LRatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*LRatio,8*LRatio,8*LRatio,8*LRatio).RoundTo(-2);
      Fill.ResourceName := LPrevResourceName;
      Fill.Color := TalphaColors.Null;
      Stroke.Color := TALphaColors.Null;
      StateStyles.Transition.Duration := 0.2;
      //Icon button unselected icon color: $FFCAC4D0
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      if LPrevDisabledResourceName <> '' then begin
        StateStyles.Disabled.Fill.Assign(Fill);
        StateStyles.Disabled.Fill.Inherit := False;
        StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      end;
      //Icon button disabled icon color: $FFE6E0E9 with 0.38 opacity
      //--Hovered--
      StateStyles.Hovered.StateLayer.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //Icon button unselected hover icon color: $FFCAC4D0
      //--Pressed--
      StateStyles.Pressed.StateLayer.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Icon button unselected pressed icon color: $FFCAC4D0
      //--Focused--
      StateStyles.Focused.StateLayer.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Icon button unselected focus icon color: $FFCAC4D0

    finally
      EndUpdate;
    end;
  end;
end;

{***************************************************************************}
procedure ALApplyButtonTheme(const ATheme: String; const AButton: TALButton);
begin
  Var LApplyButtonThemeInfo: TALApplybuttonThemeInfo;
  If not ALButtonThemes.TryGetValue(Atheme,LApplyButtonThemeInfo) then
    Raise Exception.Createfmt('The theme "%s" could not be found', [ATheme]);
  AButton.BeginUpdate;
  try
    LApplyButtonThemeInfo.ApplyThemeProc(AButton, LApplyButtonThemeInfo.DefaultFontSize);
  finally
    AButton.EndUpdate;
  end;
end;

//////////////
// CHECKBOX //
//////////////

{***************************************************************************************}
procedure ALResetCheckBoxTheme(const ACheckBox: TALCheckBox; const AHeight: Single = 18);
begin
  With ACheckBox do begin
    BeginUpdate;
    Try
      var LSize := DefaultSize;
      var LRatio: Single := AHeight / LSize.Height;
      LSize.Height := RoundTo(LSize.Height * LRatio, -2);
      LSize.Width := RoundTo(LSize.Width * LRatio, -2);
      Size.Size := LSize;
      //Margins.Rect := ALScaleRect(Margins.DefaultValue, LRatio).RoundTo(-2);
      Padding.Rect := ALScaleRect(Padding.DefaultValue, LRatio).RoundTo(-2);
      TouchTargetExpansion.Rect := ALScaleRect(TouchTargetExpansion.DefaultValue, LRatio).RoundTo(-2);
      XRadius := DefaultXRadius;
      YRadius := DefaultYRadius;
      if XRadius > 0 then XRadius := RoundTo(XRadius * LRatio, -2);
      if YRadius > 0 then YRadius := RoundTo(YRadius * LRatio, -2);
      Checkmark.Reset;
      Checkmark.Margins.Rect := ALScaleRect(Checkmark.Margins.DefaultValue, LRatio).RoundTo(-2);
      CheckMark.Thickness := RoundTo(CheckMark.DefaultThickness * LRatio, -2);
      Fill.Reset;
      Stroke.Reset;
      Stroke.Thickness := RoundTo(Stroke.DefaultThickness * LRatio, -2);
      Shadow.Reset;
      StateStyles.Reset;
      StateStyles.Checked.Hovered.statelayer.margins.rect := ALScaleRect(StateStyles.Checked.Hovered.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      StateStyles.Checked.Pressed.statelayer.margins.rect := ALScaleRect(StateStyles.Checked.Pressed.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      StateStyles.Checked.Focused.statelayer.margins.rect := ALScaleRect(StateStyles.Checked.Focused.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      StateStyles.UnChecked.Hovered.statelayer.margins.rect := ALScaleRect(StateStyles.UnChecked.Hovered.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      StateStyles.UnChecked.Pressed.statelayer.margins.rect := ALScaleRect(StateStyles.UnChecked.Pressed.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      StateStyles.UnChecked.Focused.statelayer.margins.rect := ALScaleRect(StateStyles.UnChecked.Focused.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
    Finally
      EndUpdate;
    End;
  end;
end;

{*************************************************************************************}
//https://m3.material.io/components/checkbox/specs#fd29f662-6e61-4c1f-9b97-1145c3b33075
procedure ALApplyMaterial3LightCheckBoxTheme(const ACheckBox: TALCheckBox; const AHeight: Single = 18);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the checkbox', AHeight);
  With ACheckBox do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetCheckBoxTheme(ACheckBox, AHeight * LRatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := RoundTo(2 * LRatio, -2);
      YRadius := RoundTo(2 * LRatio, -2);
      Fill.Color := TalphaColors.Null;
      Stroke.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      Stroke.Thickness := RoundTo(2 * LRatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FF1D1B20 / md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.Fill.Assign(Fill);
      StateStyles.Checked.Default.Fill.Inherit := False;
      StateStyles.Checked.Default.Fill.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Checked.Default.Stroke.Assign(Stroke);
      StateStyles.Checked.Default.Stroke.Inherit := False;
      StateStyles.Checked.Default.Stroke.Thickness := 0;
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.Color := $FFFFFFFF; // md.sys.color.on-primary / md.ref.palette.primary100

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Fill.Assign(Fill);
      StateStyles.Checked.Disabled.Fill.Inherit := False;
      StateStyles.Checked.Disabled.Fill.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.Color := $FFFEF7FF; // md.sys.color.surface / md.ref.palette.neutral98

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

{*************************************************************************************}
//https://m3.material.io/components/checkbox/specs#fd29f662-6e61-4c1f-9b97-1145c3b33075
procedure ALApplyMaterial3LightErrorCheckBoxTheme(const ACheckBox: TALCheckBox; const AHeight: Single = 18);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the checkbox', AHeight);
  With ACheckBox do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetCheckBoxTheme(ACheckBox, AHeight * LRatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := RoundTo(2 * LRatio, -2);
      YRadius := RoundTo(2 * LRatio, -2);
      Fill.Color := TalphaColors.Null;
      Stroke.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      Stroke.Thickness := RoundTo(2 * LRatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FFB3261E / md.sys.color.error / md.ref.palette.error40
      StateStyles.Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.Fill.Assign(Fill);
      StateStyles.Checked.Default.Fill.Inherit := False;
      StateStyles.Checked.Default.Fill.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      StateStyles.Checked.Default.Stroke.Assign(Stroke);
      StateStyles.Checked.Default.Stroke.Inherit := False;
      StateStyles.Checked.Default.Stroke.Thickness := 0;
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.Color := $FFFFFFFF; // md.sys.color.on-error / md.ref.palette.error100

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Fill.Assign(Fill);
      StateStyles.Checked.Disabled.Fill.Inherit := False;
      StateStyles.Checked.Disabled.Fill.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.Color := $FFFEF7FF; // md.sys.color.surface / md.ref.palette.neutral98

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

{*************************************************************************************}
//https://m3.material.io/components/checkbox/specs#fd29f662-6e61-4c1f-9b97-1145c3b33075
procedure ALApplyMaterial3DarkCheckBoxTheme(const ACheckBox: TALCheckBox; const AHeight: Single = 18);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the checkbox', AHeight);
  With ACheckBox do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetCheckBoxTheme(ACheckBox, AHeight * LRatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := RoundTo(2 * LRatio, -2);
      YRadius := RoundTo(2 * LRatio, -2);
      Fill.Color := TalphaColors.Null;
      Stroke.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      Stroke.Thickness := RoundTo(2 * LRatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FFE6E0E9 / md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.Fill.Assign(Fill);
      StateStyles.Checked.Default.Fill.Inherit := False;
      StateStyles.Checked.Default.Fill.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Checked.Default.Stroke.Assign(Stroke);
      StateStyles.Checked.Default.Stroke.Inherit := False;
      StateStyles.Checked.Default.Stroke.Thickness := 0;
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.Color := $FF381E72; // md.sys.color.on-primary / md.ref.palette.primary20

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.primary / md.ref.palette.primary80
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Fill.Assign(Fill);
      StateStyles.Checked.Disabled.Fill.Inherit := False;
      StateStyles.Checked.Disabled.Fill.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.Color := $FF141218; // md.sys.color.surface / md.ref.palette.neutral6

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

{*************************************************************************************}
//https://m3.material.io/components/checkbox/specs#fd29f662-6e61-4c1f-9b97-1145c3b33075
procedure ALApplyMaterial3DarkErrorCheckBoxTheme(const ACheckBox: TALCheckBox; const AHeight: Single = 18);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the checkbox', AHeight);
  With ACheckBox do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetCheckBoxTheme(ACheckBox, AHeight * LRatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := RoundTo(2 * LRatio, -2);
      YRadius := RoundTo(2 * LRatio, -2);
      Fill.Color := TalphaColors.Null;
      Stroke.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      Stroke.Thickness := RoundTo(2 * LRatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FFF2B8B5 / md.sys.color.error / md.ref.palette.error80
      StateStyles.Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.Fill.Assign(Fill);
      StateStyles.Checked.Default.Fill.Inherit := False;
      StateStyles.Checked.Default.Fill.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      StateStyles.Checked.Default.Stroke.Assign(Stroke);
      StateStyles.Checked.Default.Stroke.Inherit := False;
      StateStyles.Checked.Default.Stroke.Thickness := 0;
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.Color := $FF601410; // md.sys.color.on-error / md.ref.palette.error20

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.primary / md.ref.palette.primary80
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Fill.Assign(Fill);
      StateStyles.Checked.Disabled.Fill.Inherit := False;
      StateStyles.Checked.Disabled.Fill.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.Color := $FF141218; // md.sys.color.surface / md.ref.palette.neutral6

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

{*********************************************************************************}
procedure ALApplyCheckBoxTheme(const ATheme: String; const ACheckBox: TALCheckBox);
begin
  Var LApplyCheckBoxThemeInfo: TALApplyCheckBoxThemeInfo;
  If not ALCheckBoxThemes.TryGetValue(Atheme,LApplyCheckBoxThemeInfo) then
    Raise Exception.Createfmt('The theme "%s" could not be found', [ATheme]);
  ACheckBox.BeginUpdate;
  try
    LApplyCheckBoxThemeInfo.ApplyThemeProc(ACheckBox, LApplyCheckBoxThemeInfo.DefaultHeight);
  finally
    ACheckBox.EndUpdate;
  end;
end;

/////////////////
// RADIOBUTTON //
/////////////////

{************************************************************************************************}
procedure ALResetRadioButtonTheme(const ARadioButton: TALRadioButton; const AHeight: Single = 20);
begin
  With ARadioButton do begin
    BeginUpdate;
    Try
      var LSize := DefaultSize;
      var LRatio: Single := AHeight / LSize.Height;
      LSize.Height := RoundTo(LSize.Height * LRatio, -2);
      LSize.Width := RoundTo(LSize.Width * LRatio, -2);
      Size.Size := LSize;
      //Margins.Rect := ALScaleRect(Margins.DefaultValue, LRatio).RoundTo(-2);
      Padding.Rect := ALScaleRect(Padding.DefaultValue, LRatio).RoundTo(-2);
      TouchTargetExpansion.Rect := ALScaleRect(TouchTargetExpansion.DefaultValue, LRatio).RoundTo(-2);
      XRadius := DefaultXRadius;
      YRadius := DefaultYRadius;
      if XRadius > 0 then XRadius := RoundTo(XRadius * LRatio, -2);
      if YRadius > 0 then YRadius := RoundTo(YRadius * LRatio, -2);
      Checkmark.Reset;
      Checkmark.Margins.Rect := ALScaleRect(Checkmark.Margins.DefaultValue, LRatio).RoundTo(-2);
      CheckMark.Thickness := RoundTo(CheckMark.DefaultThickness * LRatio, -2);
      Fill.Reset;
      Stroke.Reset;
      Stroke.Thickness := RoundTo(Stroke.DefaultThickness * LRatio, -2);
      Shadow.Reset;
      StateStyles.Reset;
      StateStyles.Checked.Hovered.statelayer.margins.rect := ALScaleRect(StateStyles.Checked.Hovered.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      StateStyles.Checked.Pressed.statelayer.margins.rect := ALScaleRect(StateStyles.Checked.Pressed.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      StateStyles.Checked.Focused.statelayer.margins.rect := ALScaleRect(StateStyles.Checked.Focused.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      StateStyles.UnChecked.Hovered.statelayer.margins.rect := ALScaleRect(StateStyles.UnChecked.Hovered.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      StateStyles.UnChecked.Pressed.statelayer.margins.rect := ALScaleRect(StateStyles.UnChecked.Pressed.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      StateStyles.UnChecked.Focused.statelayer.margins.rect := ALScaleRect(StateStyles.UnChecked.Focused.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
    Finally
      EndUpdate;
    End;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/radio-button/specs#4eca59b9-dfb5-4ca4-9c76-8e664fb02137
procedure ALApplyMaterial3LightRadioButtonTheme(const ARadioButton: TALRadioButton; const AHeight: Single = 20);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the radio button', AHeight);
  With ARadioButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetRadioButtonTheme(ARadioButton, AHeight * LRatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      Fill.Color := TalphaColors.Null;
      Stroke.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      Stroke.Thickness := RoundTo(2 * LRatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FF1D1B20 / md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.Stroke.Assign(Stroke);
      StateStyles.Checked.Default.Stroke.Inherit := False;
      StateStyles.Checked.Default.Stroke.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Stroke.assign(Stroke);
      StateStyles.Checked.Disabled.Stroke.Inherit := False;
      StateStyles.Checked.Disabled.Stroke.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/radio-button/specs#4eca59b9-dfb5-4ca4-9c76-8e664fb02137
procedure ALApplyMaterial3LightErrorRadioButtonTheme(const ARadioButton: TALRadioButton; const AHeight: Single = 20);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the radio button', AHeight);
  With ARadioButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetRadioButtonTheme(ARadioButton, AHeight * LRatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      Fill.Color := TalphaColors.Null;
      Stroke.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      Stroke.Thickness := RoundTo(2 * LRatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FFB3261E / md.sys.color.error / md.ref.palette.error40
      StateStyles.Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Stroke.assign(Stroke);
      StateStyles.Checked.Disabled.Stroke.Inherit := False;
      StateStyles.Checked.Disabled.Stroke.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/radio-button/specs#4eca59b9-dfb5-4ca4-9c76-8e664fb02137
procedure ALApplyMaterial3DarkRadioButtonTheme(const ARadioButton: TALRadioButton; const AHeight: Single = 20);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the radio button', AHeight);
  With ARadioButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetRadioButtonTheme(ARadioButton, AHeight * LRatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      Fill.Color := TalphaColors.Null;
      Stroke.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      Stroke.Thickness := RoundTo(2 * LRatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FFE6E0E9 / md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.Stroke.Assign(Stroke);
      StateStyles.Checked.Default.Stroke.Inherit := False;
      StateStyles.Checked.Default.Stroke.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.primary / md.ref.palette.primary80
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Stroke.assign(Stroke);
      StateStyles.Checked.Disabled.Stroke.Inherit := False;
      StateStyles.Checked.Disabled.Stroke.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.primary / md.ref.palette.primary80

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/radio-button/specs#4eca59b9-dfb5-4ca4-9c76-8e664fb02137
procedure ALApplyMaterial3DarkErrorRadioButtonTheme(const ARadioButton: TALRadioButton; const AHeight: Single = 20);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the radio button', AHeight);
  With ARadioButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetRadioButtonTheme(ARadioButton, AHeight * LRatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      Fill.Color := TalphaColors.Null;
      Stroke.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      Stroke.Thickness := RoundTo(2 * LRatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FFF2B8B5 / md.sys.color.error / md.ref.palette.error80
      StateStyles.Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.primary / md.ref.palette.primary80
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Stroke.assign(Stroke);
      StateStyles.Checked.Disabled.Stroke.Inherit := False;
      StateStyles.Checked.Disabled.Stroke.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.primary / md.ref.palette.primary80

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

{******************************************************************************************}
procedure ALApplyRadioButtonTheme(const ATheme: String; const ARadioButton: TALRadioButton);
begin
  Var LApplyRadioButtonThemeInfo: TALApplyRadioButtonThemeInfo;
  If not ALRadioButtonThemes.TryGetValue(Atheme,LApplyRadioButtonThemeInfo) then
    Raise Exception.Createfmt('The theme "%s" could not be found', [ATheme]);
  ARadioButton.BeginUpdate;
  try
    LApplyRadioButtonThemeInfo.ApplyThemeProc(ARadioButton, LApplyRadioButtonThemeInfo.DefaultHeight);
  finally
    ARadioButton.EndUpdate;
  end;
end;

////////////
// SWITCH //
////////////

{*********************************************************************************}
procedure ALResetSwitchTheme(const ASwitch: TALSwitch; const AHeight: Single = 32);
begin
  With ASwitch do begin
    BeginUpdate;
    Try
      var LSize := DefaultSize;
      var LRatio: Single := AHeight / LSize.Height;
      LSize.Height := RoundTo(LSize.Height * LRatio, -2);
      LSize.Width := RoundTo(LSize.Width * LRatio, -2);
      Size.Size := LSize;
      //Margins.Rect := ALScaleRect(Margins.DefaultValue, LRatio).RoundTo(-2);
      Padding.Rect := ALScaleRect(Padding.DefaultValue, LRatio).RoundTo(-2);
      TouchTargetExpansion.Rect := ALScaleRect(TouchTargetExpansion.DefaultValue, LRatio).RoundTo(-2);
      //--
      Track.Margins.Rect := ALScaleRect(Track.Margins.DefaultValue, LRatio).RoundTo(-2);
      Track.Padding.Rect := ALScaleRect(Track.Padding.DefaultValue, LRatio).RoundTo(-2);
      Track.TouchTargetExpansion.Rect := ALScaleRect(Track.TouchTargetExpansion.DefaultValue, LRatio).RoundTo(-2);
      Track.XRadius := Track.DefaultXRadius;
      Track.YRadius := Track.DefaultYRadius;
      if Track.XRadius > 0 then Track.XRadius := RoundTo(Track.XRadius * LRatio, -2);
      if Track.YRadius > 0 then Track.YRadius := RoundTo(Track.YRadius * LRatio, -2);
      Track.Fill.Reset;
      Track.Stroke.Reset;
      Track.Stroke.Thickness := RoundTo(Track.Stroke.DefaultThickness * LRatio, -2);
      Track.Shadow.Reset;
      Track.StateStyles.Reset;
      Track.StateStyles.Checked.Hovered.statelayer.margins.rect := ALScaleRect(Track.StateStyles.Checked.Hovered.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      Track.StateStyles.Checked.Pressed.statelayer.margins.rect := ALScaleRect(Track.StateStyles.Checked.Pressed.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      Track.StateStyles.Checked.Focused.statelayer.margins.rect := ALScaleRect(Track.StateStyles.Checked.Focused.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      Track.StateStyles.UnChecked.Hovered.statelayer.margins.rect := ALScaleRect(Track.StateStyles.UnChecked.Hovered.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      Track.StateStyles.UnChecked.Pressed.statelayer.margins.rect := ALScaleRect(Track.StateStyles.UnChecked.Pressed.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      Track.StateStyles.UnChecked.Focused.statelayer.margins.rect := ALScaleRect(Track.StateStyles.UnChecked.Focused.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      //--
      Thumb.Margins.Rect := ALScaleRect(Thumb.Margins.DefaultValue, LRatio).RoundTo(-2);
      Thumb.Width := Height - Thumb.Margins.Top - Thumb.Margins.bottom;
      Thumb.Padding.Rect := ALScaleRect(Thumb.Padding.DefaultValue, LRatio).RoundTo(-2);
      Thumb.TouchTargetExpansion.Rect := ALScaleRect(Thumb.TouchTargetExpansion.DefaultValue, LRatio).RoundTo(-2);
      Thumb.XRadius := Thumb.DefaultXRadius;
      Thumb.YRadius := Thumb.DefaultYRadius;
      if Thumb.XRadius > 0 then Thumb.XRadius := RoundTo(Thumb.XRadius * LRatio, -2);
      if Thumb.YRadius > 0 then Thumb.YRadius := RoundTo(Thumb.YRadius * LRatio, -2);
      Thumb.Checkmark.Reset;
      Thumb.Checkmark.Margins.Rect := ALScaleRect(Thumb.Checkmark.Margins.DefaultValue, LRatio).RoundTo(-2);
      Thumb.CheckMark.Thickness := RoundTo(Thumb.CheckMark.DefaultThickness * LRatio, -2);
      Thumb.Fill.Reset;
      Thumb.Stroke.Reset;
      Thumb.Stroke.Thickness := RoundTo(Thumb.Stroke.DefaultThickness * LRatio, -2);
      Thumb.Shadow.Reset;
      Thumb.StateStyles.Reset;
      Thumb.StateStyles.Checked.Hovered.statelayer.margins.rect := ALScaleRect(Thumb.StateStyles.Checked.Hovered.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Pressed.statelayer.margins.rect := ALScaleRect(Thumb.StateStyles.Checked.Pressed.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Focused.statelayer.margins.rect := ALScaleRect(Thumb.StateStyles.Checked.Focused.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Hovered.statelayer.margins.rect := ALScaleRect(Thumb.StateStyles.UnChecked.Hovered.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Pressed.statelayer.margins.rect := ALScaleRect(Thumb.StateStyles.UnChecked.Pressed.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Focused.statelayer.margins.rect := ALScaleRect(Thumb.StateStyles.UnChecked.Focused.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
    Finally
      EndUpdate;
    End;
  end;
end;

{***********************************************************************************}
//https://m3.material.io/components/switch/specs#e27a8630-f5e0-481a-ad24-0e8ebb8a8619
procedure ALApplyMaterial3LightSwitchTheme(const ASwitch: TALSwitch; const AHeight: Single = 32);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the switch', AHeight);
  With ASwitch do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetSwitchTheme(ASwitch, AHeight * LRatio);

      //--Default (UnChecked)--
      Track.StateStyles.UnChecked.Default.Stroke.Assign(Track.Stroke);
      Track.StateStyles.UnChecked.Default.Stroke.Inherit := False;
      Track.StateStyles.UnChecked.Default.Stroke.Color := $FF79747E; // md.sys.color.outline / md.ref.palette.neutral-variant50
      Track.StateStyles.UnChecked.Default.Stroke.Thickness := RoundTo(2 * LRatio, -2);
      Track.StateStyles.UnChecked.Default.fill.Assign(Track.fill);
      Track.StateStyles.UnChecked.Default.fill.Inherit := False;
      Track.StateStyles.UnChecked.Default.fill.Color := $FFE6E0E9; // md.sys.color.surface-container-highest / md.ref.palette.neutral90
      //--
      Thumb.StateStyles.UnChecked.Default.fill.Assign(Thumb.fill);
      Thumb.StateStyles.UnChecked.Default.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Default.fill.Color := $FF79747E; // md.sys.color.outline / md.ref.palette.neutral-variant50
      Thumb.StateStyles.UnChecked.Default.Fill.BackgroundMargins.Rect := TRectF.Create(4*LRatio,4*LRatio,4*LRatio,4*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Default.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.UnChecked.Default.CheckMark.Inherit := False;
      Thumb.StateStyles.UnChecked.Default.CheckMark.Color := TAlphacolors.Null; // $FFE6E0E9; // md.sys.color.surface-container-highest / md.ref.palette.neutral90
      //--Default (Checked)--
      Track.StateStyles.Checked.Default.fill.Assign(Track.fill);
      Track.StateStyles.Checked.Default.fill.Inherit := False;
      Track.StateStyles.Checked.Default.fill.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      //--
      Thumb.StateStyles.Checked.Default.fill.Assign(Thumb.fill);
      Thumb.StateStyles.Checked.Default.fill.Inherit := False;
      Thumb.StateStyles.Checked.Default.fill.Color := $FFFFFFFF; // md.sys.color.on-primary / md.ref.palette.primary100
      Thumb.StateStyles.Checked.Default.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.Checked.Default.CheckMark.Inherit := False;
      Thumb.StateStyles.Checked.Default.CheckMark.Color := $FF4F378B; // md.sys.color.on-primary-container / md.ref.palette.primary30

      //--Disabled (UnChecked)--
      Track.StateStyles.UnChecked.Disabled.Opacity := 1;
      Track.StateStyles.UnChecked.Disabled.Stroke.Assign(Track.Stroke);
      Track.StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      Track.StateStyles.UnChecked.Disabled.Stroke.Color := ALSetColorAlpha($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      Track.StateStyles.UnChecked.Disabled.Stroke.Thickness := RoundTo(2 * LRatio, -2);
      Track.StateStyles.UnChecked.Disabled.fill.Assign(Track.fill);
      Track.StateStyles.UnChecked.Disabled.fill.Inherit := False;
      Track.StateStyles.UnChecked.Disabled.fill.Color := ALSetColorAlpha($FFE6E0E9, 0.12); // md.sys.color.surface-container-highest / md.ref.palette.neutral90
      //--
      Thumb.StateStyles.UnChecked.Disabled.Opacity := 1;
      Thumb.StateStyles.UnChecked.Disabled.fill.Assign(Thumb.fill);
      Thumb.StateStyles.UnChecked.Disabled.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Disabled.fill.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      Thumb.StateStyles.UnChecked.Disabled.Fill.BackgroundMargins.Rect := TRectF.Create(4*LRatio,4*LRatio,4*LRatio,4*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Disabled.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      Thumb.StateStyles.UnChecked.Disabled.CheckMark.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.surface-container-highest / md.ref.palette.neutral90
      //--Disabled (Checked)--
      Track.StateStyles.Checked.Disabled.Opacity := 1;
      Track.StateStyles.Checked.Disabled.fill.Assign(Track.fill);
      Track.StateStyles.Checked.Disabled.fill.Inherit := False;
      Track.StateStyles.Checked.Disabled.fill.Color := ALSetColorAlpha($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      //--
      Thumb.StateStyles.Checked.Disabled.Opacity := 1;
      Thumb.StateStyles.Checked.Disabled.fill.Assign(Thumb.fill);
      Thumb.StateStyles.Checked.Disabled.fill.Inherit := False;
      Thumb.StateStyles.Checked.Disabled.fill.Color := $FFFEF7FF; // md.sys.color.surface / md.ref.palette.neutral98
      Thumb.StateStyles.Checked.Disabled.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      Thumb.StateStyles.Checked.Disabled.CheckMark.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10

      //--Hovered (UnChecked)--
      Thumb.StateStyles.UnChecked.Hovered.fill.Assign(Thumb.StateStyles.UnChecked.Default.fill);
      Thumb.StateStyles.UnChecked.Hovered.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Hovered.fill.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      Thumb.StateStyles.UnChecked.Hovered.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Hovered.StateLayer.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
      Thumb.StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      Thumb.StateStyles.Checked.Hovered.fill.Assign(Thumb.StateStyles.Checked.Default.fill);
      Thumb.StateStyles.Checked.Hovered.fill.Inherit := False;
      Thumb.StateStyles.Checked.Hovered.fill.Color := $FFEADDFF; // md.sys.color.primary-container / md.ref.palette.primary90
      Thumb.StateStyles.Checked.Hovered.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Hovered.StateLayer.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      Thumb.StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      Thumb.StateStyles.UnChecked.Pressed.Fill.Assign(Thumb.StateStyles.UnChecked.Default.fill);
      Thumb.StateStyles.UnChecked.Pressed.Fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Pressed.fill.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      Thumb.StateStyles.UnChecked.Pressed.Fill.BackgroundMargins.Rect := TRectF.Create(-2*LRatio,-2*LRatio,-2*LRatio,-2*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Pressed.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Pressed.StateLayer.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
      Thumb.StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Pressed (Checked)--
      Thumb.StateStyles.Checked.Pressed.Fill.Assign(Thumb.StateStyles.Checked.Default.fill);
      Thumb.StateStyles.Checked.Pressed.Fill.Inherit := False;
      Thumb.StateStyles.Checked.Pressed.fill.Color := $FFEADDFF; // md.sys.color.primary-container / md.ref.palette.primary90
      Thumb.StateStyles.Checked.Pressed.Fill.BackgroundMargins.Rect := TRectF.Create(-2*LRatio,-2*LRatio,-2*LRatio,-2*LRatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Pressed.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Pressed.StateLayer.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      Thumb.StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

      //--Focused (UnChecked)--
      Thumb.StateStyles.UnChecked.Focused.fill.Assign(Thumb.StateStyles.UnChecked.Default.fill);
      Thumb.StateStyles.UnChecked.Focused.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Focused.fill.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
      Thumb.StateStyles.UnChecked.Focused.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Focused.StateLayer.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
      Thumb.StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused (Checked)--
      Thumb.StateStyles.Checked.Focused.fill.Assign(Thumb.StateStyles.Checked.Default.fill);
      Thumb.StateStyles.Checked.Focused.fill.Inherit := False;
      Thumb.StateStyles.Checked.Focused.fill.Color := $FFEADDFF; // md.sys.color.primary-container / md.ref.palette.primary90
      Thumb.StateStyles.Checked.Focused.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Focused.StateLayer.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      Thumb.StateStyles.Checked.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

    finally
      EndUpdate;
    end;
  end;
end;

{***********************************************************************************}
//https://m3.material.io/components/switch/specs#e27a8630-f5e0-481a-ad24-0e8ebb8a8619
procedure ALApplyMaterial3DarkSwitchTheme(const ASwitch: TALSwitch; const AHeight: Single = 32);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the switch', AHeight);
  With ASwitch do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetSwitchTheme(ASwitch, AHeight * LRatio);

      //--Default (UnChecked)--
      Track.StateStyles.UnChecked.Default.Stroke.Assign(Track.Stroke);
      Track.StateStyles.UnChecked.Default.Stroke.Inherit := False;
      Track.StateStyles.UnChecked.Default.Stroke.Color := $FF938F99; // md.sys.color.outline / md.ref.palette.neutral-variant60
      Track.StateStyles.UnChecked.Default.Stroke.Thickness := RoundTo(2 * LRatio, -2);
      Track.StateStyles.UnChecked.Default.fill.Assign(Track.fill);
      Track.StateStyles.UnChecked.Default.fill.Inherit := False;
      Track.StateStyles.UnChecked.Default.fill.Color := $FF36343B; // md.sys.color.surface-container-highest / md.ref.palette.neutral22
      //--
      Thumb.StateStyles.UnChecked.Default.fill.Assign(Thumb.fill);
      Thumb.StateStyles.UnChecked.Default.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Default.fill.Color := $FF938F99; // md.sys.color.outline / md.ref.palette.neutral-variant60
      Thumb.StateStyles.UnChecked.Default.Fill.BackgroundMargins.Rect := TRectF.Create(4*LRatio,4*LRatio,4*LRatio,4*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Default.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.UnChecked.Default.CheckMark.Inherit := False;
      Thumb.StateStyles.UnChecked.Default.CheckMark.Color := TAlphacolors.Null; // $FF36343B; // md.sys.color.surface-container-highest / md.ref.palette.neutral22
      //--Default (Checked)--
      Track.StateStyles.Checked.Default.fill.Assign(Track.fill);
      Track.StateStyles.Checked.Default.fill.Inherit := False;
      Track.StateStyles.Checked.Default.fill.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      //--
      Thumb.StateStyles.Checked.Default.fill.Assign(Thumb.fill);
      Thumb.StateStyles.Checked.Default.fill.Inherit := False;
      Thumb.StateStyles.Checked.Default.fill.Color := $FF381E72; // md.sys.color.on-primary / md.ref.palette.primary20
      Thumb.StateStyles.Checked.Default.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.Checked.Default.CheckMark.Inherit := False;
      Thumb.StateStyles.Checked.Default.CheckMark.Color := $FFEADDFF; // md.sys.color.on-primary-container / md.ref.palette.primary90

      //--Disabled (UnChecked)--
      Track.StateStyles.UnChecked.Disabled.Opacity := 1;
      Track.StateStyles.UnChecked.Disabled.Stroke.Assign(Track.Stroke);
      Track.StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      Track.StateStyles.UnChecked.Disabled.Stroke.Color := ALSetColorAlpha($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      Track.StateStyles.UnChecked.Disabled.Stroke.Thickness := RoundTo(2 * LRatio, -2);
      Track.StateStyles.UnChecked.Disabled.fill.Assign(Track.fill);
      Track.StateStyles.UnChecked.Disabled.fill.Inherit := False;
      Track.StateStyles.UnChecked.Disabled.fill.Color := ALSetColorAlpha($FF36343B, 0.12); // md.sys.color.surface-container-highest / md.ref.palette.neutral22
      //--
      Thumb.StateStyles.UnChecked.Disabled.Opacity := 1;
      Thumb.StateStyles.UnChecked.Disabled.fill.Assign(Thumb.fill);
      Thumb.StateStyles.UnChecked.Disabled.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Disabled.fill.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      Thumb.StateStyles.UnChecked.Disabled.Fill.BackgroundMargins.Rect := TRectF.Create(4*LRatio,4*LRatio,4*LRatio,4*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Disabled.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      Thumb.StateStyles.UnChecked.Disabled.CheckMark.Color := ALSetColorAlpha($FF36343B, 0.38); // md.sys.color.surface-container-highest / md.ref.palette.neutral22
      //--Disabled (Checked)--
      Track.StateStyles.Checked.Disabled.Opacity := 1;
      Track.StateStyles.Checked.Disabled.fill.Assign(Track.fill);
      Track.StateStyles.Checked.Disabled.fill.Inherit := False;
      Track.StateStyles.Checked.Disabled.fill.Color := ALSetColorAlpha($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      //--
      Thumb.StateStyles.Checked.Disabled.Opacity := 1;
      Thumb.StateStyles.Checked.Disabled.fill.Assign(Thumb.fill);
      Thumb.StateStyles.Checked.Disabled.fill.Inherit := False;
      Thumb.StateStyles.Checked.Disabled.fill.Color := $FF141218; // md.sys.color.surface / md.ref.palette.neutral6
      Thumb.StateStyles.Checked.Disabled.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      Thumb.StateStyles.Checked.Disabled.CheckMark.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90

      //--Hovered (UnChecked)--
      Thumb.StateStyles.UnChecked.Hovered.fill.Assign(Thumb.StateStyles.UnChecked.Default.fill);
      Thumb.StateStyles.UnChecked.Hovered.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Hovered.fill.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      Thumb.StateStyles.UnChecked.Hovered.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Hovered.StateLayer.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
      Thumb.StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      Thumb.StateStyles.Checked.Hovered.fill.Assign(Thumb.StateStyles.Checked.Default.fill);
      Thumb.StateStyles.Checked.Hovered.fill.Inherit := False;
      Thumb.StateStyles.Checked.Hovered.fill.Color := $FF4F378B; // md.sys.color.primary-container / md.ref.palette.primary30
      Thumb.StateStyles.Checked.Hovered.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Hovered.StateLayer.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      Thumb.StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      Thumb.StateStyles.UnChecked.Pressed.Fill.Assign(Thumb.StateStyles.UnChecked.Default.fill);
      Thumb.StateStyles.UnChecked.Pressed.Fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Pressed.fill.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      Thumb.StateStyles.UnChecked.Pressed.Fill.BackgroundMargins.Rect := TRectF.Create(-2*LRatio,-2*LRatio,-2*LRatio,-2*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Pressed.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Pressed.StateLayer.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
      Thumb.StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Pressed (Checked)--
      Thumb.StateStyles.Checked.Pressed.Fill.Assign(Thumb.StateStyles.Checked.Default.fill);
      Thumb.StateStyles.Checked.Pressed.Fill.Inherit := False;
      Thumb.StateStyles.Checked.Pressed.fill.Color := $FF4F378B; // md.sys.color.primary-container / md.ref.palette.primary30
      Thumb.StateStyles.Checked.Pressed.Fill.BackgroundMargins.Rect := TRectF.Create(-2*LRatio,-2*LRatio,-2*LRatio,-2*LRatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Pressed.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Pressed.StateLayer.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      Thumb.StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

      //--Focused (UnChecked)--
      Thumb.StateStyles.UnChecked.Focused.fill.Assign(Thumb.StateStyles.UnChecked.Default.fill);
      Thumb.StateStyles.UnChecked.Focused.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Focused.fill.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
      Thumb.StateStyles.UnChecked.Focused.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Focused.StateLayer.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
      Thumb.StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused (Checked)--
      Thumb.StateStyles.Checked.Focused.fill.Assign(Thumb.StateStyles.Checked.Default.fill);
      Thumb.StateStyles.Checked.Focused.fill.Inherit := False;
      Thumb.StateStyles.Checked.Focused.fill.Color := $FF4F378B; // md.sys.color.primary-container / md.ref.palette.primary30
      Thumb.StateStyles.Checked.Focused.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Focused.StateLayer.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      Thumb.StateStyles.Checked.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

    finally
      EndUpdate;
    end;
  end;
end;

{***************************************************************************}
procedure ALApplySwitchTheme(const ATheme: String; const ASwitch: TALSwitch);
begin
  Var LApplySwitchThemeInfo: TALApplySwitchThemeInfo;
  If not ALSwitchThemes.TryGetValue(Atheme,LApplySwitchThemeInfo) then
    Raise Exception.Createfmt('The theme "%s" could not be found', [ATheme]);
  ASwitch.BeginUpdate;
  try
    LApplySwitchThemeInfo.ApplyThemeProc(ASwitch, LApplySwitchThemeInfo.DefaultHeight);
  finally
    ASwitch.EndUpdate;
  end;
end;

//////////////
// TRACKBAR //
//////////////

type
  _TALCustomTrackProtectedAccess = class(TALCustomTrack);

{******************************************************************************************}
procedure ALResetTrackBarTheme(const ATrackBar: TALCustomTrack; const AHeight: Single = 32);
begin
  With _TALCustomTrackProtectedAccess(ATrackBar) do begin
    BeginUpdate;
    Try
      var LSize := DefaultSize;
      var LRatio: Single := AHeight / LSize.Height;
      LSize.Height := RoundTo(LSize.Height * LRatio, -2);
      LSize.Width := RoundTo(LSize.Width * LRatio, -2);
      Size.Size := LSize;
      //Margins.Rect := ALScaleRect(Margins.DefaultValue, LRatio).RoundTo(-2);
      Padding.Rect := ALScaleRect(Padding.DefaultValue, LRatio).RoundTo(-2);
      TouchTargetExpansion.Rect := ALScaleRect(TouchTargetExpansion.DefaultValue, LRatio).RoundTo(-2);
      //--
      InactiveTrack.Margins.Rect := ALScaleRect(InactiveTrack.Margins.DefaultValue, LRatio).RoundTo(-2);
      InactiveTrack.Padding.Rect := ALScaleRect(InactiveTrack.Padding.DefaultValue, LRatio).RoundTo(-2);
      InactiveTrack.TouchTargetExpansion.Rect := ALScaleRect(InactiveTrack.TouchTargetExpansion.DefaultValue, LRatio).RoundTo(-2);
      InactiveTrack.XRadius := InactiveTrack.DefaultXRadius;
      InactiveTrack.YRadius := InactiveTrack.DefaultYRadius;
      if InactiveTrack.XRadius > 0 then InactiveTrack.XRadius := RoundTo(InactiveTrack.XRadius * LRatio, -2);
      if InactiveTrack.YRadius > 0 then InactiveTrack.YRadius := RoundTo(InactiveTrack.YRadius * LRatio, -2);
      InactiveTrack.Corners := AllCorners;
      InactiveTrack.Opacity := 1;
      InactiveTrack.Fill.Reset;
      InactiveTrack.Stroke.Reset;
      InactiveTrack.Stroke.Thickness := RoundTo(InactiveTrack.Stroke.DefaultThickness * LRatio, -2);
      InactiveTrack.Shadow.Reset;
      InactiveTrack.stopIndicator.Reset;
      InactiveTrack.stopIndicator.Size := RoundTo(InactiveTrack.stopIndicator.DefaultSize * LRatio, -2);
      InactiveTrack.StateStyles.Reset;
      //--
      ActiveTrack.Margins.Rect := ALScaleRect(ActiveTrack.Margins.DefaultValue, LRatio).RoundTo(-2);
      ActiveTrack.Padding.Rect := ALScaleRect(ActiveTrack.Padding.DefaultValue, LRatio).RoundTo(-2);
      ActiveTrack.TouchTargetExpansion.Rect := ALScaleRect(ActiveTrack.TouchTargetExpansion.DefaultValue, LRatio).RoundTo(-2);
      ActiveTrack.XRadius := ActiveTrack.DefaultXRadius;
      ActiveTrack.YRadius := ActiveTrack.DefaultYRadius;
      if ActiveTrack.XRadius > 0 then ActiveTrack.XRadius := RoundTo(ActiveTrack.XRadius * LRatio, -2);
      if ActiveTrack.YRadius > 0 then ActiveTrack.YRadius := RoundTo(ActiveTrack.YRadius * LRatio, -2);
      ActiveTrack.Corners := AllCorners;
      ActiveTrack.Opacity := 1;
      ActiveTrack.Fill.Reset;
      ActiveTrack.Stroke.Reset;
      ActiveTrack.Stroke.Thickness := RoundTo(ActiveTrack.Stroke.DefaultThickness * LRatio, -2);
      ActiveTrack.Shadow.Reset;
      ActiveTrack.stopIndicator.Reset;
      ActiveTrack.stopIndicator.Size := RoundTo(ActiveTrack.stopIndicator.DefaultSize * LRatio, -2);
      ActiveTrack.StateStyles.Reset;
      //--
      Thumb.Margins.Rect := ALScaleRect(Thumb.Margins.DefaultValue, LRatio).RoundTo(-2);
      Thumb.Padding.Rect := ALScaleRect(Thumb.Padding.DefaultValue, LRatio).RoundTo(-2);
      Thumb.TouchTargetExpansion.Rect := ALScaleRect(Thumb.TouchTargetExpansion.DefaultValue, LRatio).RoundTo(-2);
      Thumb.XRadius := Thumb.DefaultXRadius;
      Thumb.YRadius := Thumb.DefaultYRadius;
      if Thumb.XRadius > 0 then Thumb.XRadius := RoundTo(Thumb.XRadius * LRatio, -2);
      if Thumb.YRadius > 0 then Thumb.YRadius := RoundTo(Thumb.YRadius * LRatio, -2);
      Thumb.Corners := AllCorners;
      Thumb.Opacity := 1;
      Thumb.Fill.Reset;
      Thumb.Stroke.Reset;
      Thumb.Stroke.Thickness := RoundTo(Thumb.Stroke.DefaultThickness * LRatio, -2);
      Thumb.Shadow.Reset;
      Thumb.StateStyles.Reset;
      Thumb.StateStyles.Hovered.statelayer.margins.rect := ALScaleRect(Thumb.StateStyles.Hovered.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      Thumb.StateStyles.Pressed.statelayer.margins.rect := ALScaleRect(Thumb.StateStyles.Pressed.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      Thumb.StateStyles.Focused.statelayer.margins.rect := ALScaleRect(Thumb.StateStyles.Focused.statelayer.margins.DefaultValue, LRatio).RoundTo(-2);
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then
        Thumb.Width := ATrackBar.Height - ATrackBar.Padding.Top - ATrackBar.Padding.Bottom - Thumb.Margins.Top - Thumb.Margins.Bottom
      else
        Thumb.Height := ATrackBar.Width - ATrackBar.Padding.left - ATrackBar.Padding.right - Thumb.Margins.left - Thumb.Margins.right;
      //--
      //ValueIndicator.Margins.Rect := ALScaleRect(ValueIndicator.Margins.DefaultValue, LRatio).RoundTo(-2);
      ValueIndicator.Padding.Rect := ALScaleRect(ValueIndicator.Padding.DefaultValue, LRatio).RoundTo(-2);
      ValueIndicator.TouchTargetExpansion.Rect := ALScaleRect(ValueIndicator.TouchTargetExpansion.DefaultValue, LRatio).RoundTo(-2);
      ValueIndicator.XRadius := ValueIndicator.DefaultXRadius;
      ValueIndicator.YRadius := ValueIndicator.DefaultYRadius;
      if ValueIndicator.XRadius > 0 then ValueIndicator.XRadius := RoundTo(ValueIndicator.XRadius * LRatio, -2);
      if ValueIndicator.YRadius > 0 then ValueIndicator.YRadius := RoundTo(ValueIndicator.YRadius * LRatio, -2);
      ValueIndicator.Animation := TValueIndicator.TAnimation.ScaleInOut;
      ValueIndicator.AutoSize := true;
      ValueIndicator.Corners := AllCorners;
      ValueIndicator.Sides := AllSides;
      ValueIndicator.Opacity := 1;
      ValueIndicator.Fill.Reset;
      ValueIndicator.Stroke.Reset;
      ValueIndicator.Stroke.Thickness := RoundTo(ValueIndicator.Stroke.DefaultThickness * LRatio, -2);
      ValueIndicator.Shadow.Reset;
      ValueIndicator.TextSettings.Reset;
    Finally
      EndUpdate;
    End;
  end;
end;

{*************************************************************************************}
//https://m3.material.io/components/TrackBar/specs#e27a8630-f5e0-481a-ad24-0e8ebb8a8619
procedure ALApplyMaterial3LightTrackBarTheme(const ATrackBar: TALCustomTrack; const AHeight: Single = 44);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the TrackBar', AHeight);
  With _TALCustomTrackProtectedAccess(ATrackBar) do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetTrackBarTheme(ATrackBar, AHeight * LRatio);
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then begin
        InactiveTrack.Margins.Rect := TRectF.Create(0,14*LRatio,0,14*LRatio).RoundTo(-2);
        ActiveTrack.Margins.Rect := TRectF.Create(0,14*LRatio,0,14*LRatio).RoundTo(-2);
        InactiveTrack.Padding.Rect := TRectF.Create(6*LRatio,0,6*LRatio,0).RoundTo(-2);
        ActiveTrack.Padding.Rect := TRectF.Create(6*LRatio,0,6*LRatio,0).RoundTo(-2);
      end
      else begin
        InactiveTrack.Margins.Rect := TRectF.Create(14*LRatio,0,14*LRatio,0).RoundTo(-2);
        ActiveTrack.Margins.Rect := TRectF.Create(14*LRatio,0,14*LRatio,0).RoundTo(-2);
        InactiveTrack.Padding.Rect := TRectF.Create(0,6*LRatio,0,6*LRatio).RoundTo(-2);
        ActiveTrack.Padding.Rect := TRectF.Create(0,6*LRatio,0,6*LRatio).RoundTo(-2);
      end;
      InactiveTrack.XRadius := -50;
      InactiveTrack.YRadius := -50;
      ActiveTrack.XRadius := -50;
      ActiveTrack.YRadius := -50;
      InactiveTrack.Fill.Color := $FFE8DEF8; // md.sys.color.secondary-container / md.ref.palette.secondary90
      ActiveTrack.Fill.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      InactiveTrack.StopIndicator.Size := RoundTo(4 * LRatio, -2);
      ActiveTrack.StopIndicator.Size := RoundTo(4 * LRatio, -2);
      InactiveTrack.StopIndicator.Color := $FF4A4458; // md.sys.color.on-secondary-container / md.ref.palette.secondary30
      ActiveTrack.StopIndicator.Color := $FFFFFFFF; // md.sys.color.on-primary / md.ref.palette.primary100
      ValueIndicator.TextSettings.Font.Color := $FFF5EFF7; // md.sys.color.inverse-on-surface / md.ref.palette.neutral95
      ValueIndicator.TextSettings.Font.Size := RoundTo(14 * LRatio, -2);
      ValueIndicator.padding.Rect := TRectF.create(16 * LRatio{Left}, 12 * LRatio{Top}, 16 * LRatio{Right}, 12 * LRatio{Bottom}).RoundTo(-2);
      ValueIndicator.Fill.Color := $FF322F35; // md.sys.color.inverse-surface / md.ref.palette.neutral20
      Thumb.Width := RoundTo(4 * LRatio, -2);
      Thumb.Fill.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      Thumb.Stroke.Color := TalphaColors.Null;
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then begin
        Thumb.Margins.Rect := TRectF.Create(6*LRatio,0,6*LRatio,0).RoundTo(-2);
        Thumb.TouchTargetExpansion.Rect := TRectF.Create(22*LRatio,2*LRatio,22*LRatio,2*LRatio).RoundTo(-2);
      end
      else begin
        Thumb.Margins.Rect := TRectF.Create(0,6*LRatio,0,6*LRatio).RoundTo(-2);
        Thumb.TouchTargetExpansion.Rect := TRectF.Create(2*LRatio,22*LRatio,2*LRatio,22*LRatio).RoundTo(-2);
      end;

      //--Disabled--
      InactiveTrack.StateStyles.Disabled.Opacity := 1;
      ActiveTrack.StateStyles.Disabled.Opacity := 1;
      //--
      InactiveTrack.StateStyles.Disabled.fill.Assign(InactiveTrack.Fill);
      InactiveTrack.StateStyles.Disabled.fill.Inherit := False;
      InactiveTrack.StateStyles.Disabled.fill.Color := ALSetColorAlpha($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      //--
      ActiveTrack.StateStyles.Disabled.fill.Assign(ActiveTrack.Fill);
      ActiveTrack.StateStyles.Disabled.fill.Inherit := False;
      ActiveTrack.StateStyles.Disabled.fill.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      //--
      InactiveTrack.StateStyles.Disabled.StopIndicator.Assign(InactiveTrack.StopIndicator);
      InactiveTrack.StateStyles.Disabled.StopIndicator.Inherit := False;
      InactiveTrack.StateStyles.Disabled.StopIndicator.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      //--
      ActiveTrack.StateStyles.Disabled.StopIndicator.Assign(ActiveTrack.StopIndicator);
      ActiveTrack.StateStyles.Disabled.StopIndicator.Inherit := False;
      ActiveTrack.StateStyles.Disabled.StopIndicator.Color := ALSetColorAlpha($FFF5EFF7, 0.66); // md.sys.color.inverse-on-surface / md.ref.palette.neutral95
      //--
      Thumb.StateStyles.Disabled.Opacity := 1;
      Thumb.StateStyles.Disabled.fill.Assign(Thumb.Fill);
      Thumb.StateStyles.Disabled.fill.Inherit := False;
      Thumb.StateStyles.Disabled.fill.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10

      //--Hovered--

      //--Pressed--
      Thumb.StateStyles.Pressed.fill.Assign(Thumb.Fill);
      Thumb.StateStyles.Pressed.fill.Inherit := False;
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then
        Thumb.StateStyles.Pressed.fill.BackgroundMargins.Rect := TRectF.Create(1*LRatio,0,1*LRatio,0).RoundTo(-2)
      else
        Thumb.StateStyles.Pressed.fill.BackgroundMargins.Rect := TRectF.Create(0,1*LRatio,0,1*LRatio).RoundTo(-2);

      //--Focused--
      Thumb.StateStyles.Pressed.fill.Assign(Thumb.Fill);
      Thumb.StateStyles.Pressed.fill.Inherit := False;
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then
        Thumb.StateStyles.Pressed.fill.BackgroundMargins.Rect := TRectF.Create(1*LRatio,0,1*LRatio,0).RoundTo(-2)
      else
        Thumb.StateStyles.Pressed.fill.BackgroundMargins.Rect := TRectF.Create(0,1*LRatio,0,1*LRatio).RoundTo(-2);

    finally
      EndUpdate;
    end;
  end;
end;

{*************************************************************************************}
//https://m3.material.io/components/TrackBar/specs#e27a8630-f5e0-481a-ad24-0e8ebb8a8619
procedure ALApplyMaterial3DarkTrackBarTheme(const ATrackBar: TALCustomTrack; const AHeight: Single = 44);
begin
  var LRatio: Single := ALGetThemeSizeRatio('Please enter the desired height for the TrackBar', AHeight);
  With _TALCustomTrackProtectedAccess(ATrackBar) do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetTrackBarTheme(ATrackBar, AHeight * LRatio);
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then begin
        InactiveTrack.Margins.Rect := TRectF.Create(0,14*LRatio,0,14*LRatio).RoundTo(-2);
        ActiveTrack.Margins.Rect := TRectF.Create(0,14*LRatio,0,14*LRatio).RoundTo(-2);
        InactiveTrack.Padding.Rect := TRectF.Create(6*LRatio,0,6*LRatio,0).RoundTo(-2);
        ActiveTrack.Padding.Rect := TRectF.Create(6*LRatio,0,6*LRatio,0).RoundTo(-2);
      end
      else begin
        InactiveTrack.Margins.Rect := TRectF.Create(14*LRatio,0,14*LRatio,0).RoundTo(-2);
        ActiveTrack.Margins.Rect := TRectF.Create(14*LRatio,0,14*LRatio,0).RoundTo(-2);
        InactiveTrack.Padding.Rect := TRectF.Create(0,6*LRatio,0,6*LRatio).RoundTo(-2);
        ActiveTrack.Padding.Rect := TRectF.Create(0,6*LRatio,0,6*LRatio).RoundTo(-2);
      end;
      InactiveTrack.XRadius := -50;
      InactiveTrack.YRadius := -50;
      ActiveTrack.XRadius := -50;
      ActiveTrack.YRadius := -50;
      InactiveTrack.Fill.Color := $FF4A4458; // md.sys.color.secondary-container / md.ref.palette.secondary30
      ActiveTrack.Fill.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      InactiveTrack.StopIndicator.Size := RoundTo(4 * LRatio, -2);
      ActiveTrack.StopIndicator.Size := RoundTo(4 * LRatio, -2);
      InactiveTrack.StopIndicator.Color := $FFE8DEF8; // md.sys.color.on-secondary-container / md.ref.palette.secondary90
      ActiveTrack.StopIndicator.Color := $FF381E72; // md.sys.color.on-primary / md.ref.palette.primary20
      ValueIndicator.TextSettings.Font.Color := $FF322F35; // md.sys.color.inverse-on-surface / md.ref.palette.neutral20
      ValueIndicator.TextSettings.Font.Size := RoundTo(14 * LRatio, -2);
      ValueIndicator.padding.Rect := TRectF.create(16 * LRatio{Left}, 12 * LRatio{Top}, 16 * LRatio{Right}, 12 * LRatio{Bottom}).RoundTo(-2);
      ValueIndicator.Fill.Color := $FFE6E0E9; // md.sys.color.inverse-surface / md.ref.palette.neutral90
      Thumb.Width := RoundTo(4 * LRatio, -2);
      Thumb.Fill.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      Thumb.Stroke.Color := TalphaColors.Null;
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then begin
        Thumb.Margins.Rect := TRectF.Create(6*LRatio,0,6*LRatio,0).RoundTo(-2);
        Thumb.TouchTargetExpansion.Rect := TRectF.Create(22*LRatio,2*LRatio,22*LRatio,2*LRatio).RoundTo(-2);
      end
      else begin
        Thumb.Margins.Rect := TRectF.Create(0,6*LRatio,0,6*LRatio).RoundTo(-2);
        Thumb.TouchTargetExpansion.Rect := TRectF.Create(2*LRatio,22*LRatio,2*LRatio,22*LRatio).RoundTo(-2);
      end;

      //--Disabled--
      InactiveTrack.StateStyles.Disabled.Opacity := 1;
      ActiveTrack.StateStyles.Disabled.Opacity := 1;
      //--
      InactiveTrack.StateStyles.Disabled.fill.Assign(InactiveTrack.Fill);
      InactiveTrack.StateStyles.Disabled.fill.Inherit := False;
      InactiveTrack.StateStyles.Disabled.fill.Color := ALSetColorAlpha($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      //--
      ActiveTrack.StateStyles.Disabled.fill.Assign(ActiveTrack.Fill);
      ActiveTrack.StateStyles.Disabled.fill.Inherit := False;
      ActiveTrack.StateStyles.Disabled.fill.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      //--
      InactiveTrack.StateStyles.Disabled.StopIndicator.Assign(InactiveTrack.StopIndicator);
      InactiveTrack.StateStyles.Disabled.StopIndicator.Inherit := False;
      InactiveTrack.StateStyles.Disabled.StopIndicator.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      //--
      ActiveTrack.StateStyles.Disabled.StopIndicator.Assign(ActiveTrack.StopIndicator);
      ActiveTrack.StateStyles.Disabled.StopIndicator.Inherit := False;
      ActiveTrack.StateStyles.Disabled.StopIndicator.Color := ALSetColorAlpha($FF322F35, 0.66); // md.sys.color.inverse-on-surface / md.ref.palette.neutral20
      //--
      Thumb.StateStyles.Disabled.Opacity := 1;
      Thumb.StateStyles.Disabled.fill.Assign(Thumb.Fill);
      Thumb.StateStyles.Disabled.fill.Inherit := False;
      Thumb.StateStyles.Disabled.fill.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90

      //--Hovered--

      //--Pressed--
      Thumb.StateStyles.Pressed.fill.Assign(Thumb.Fill);
      Thumb.StateStyles.Pressed.fill.Inherit := False;
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then
        Thumb.StateStyles.Pressed.fill.BackgroundMargins.Rect := TRectF.Create(1*LRatio,0,1*LRatio,0).RoundTo(-2)
      else
        Thumb.StateStyles.Pressed.fill.BackgroundMargins.Rect := TRectF.Create(0,1*LRatio,0,1*LRatio).RoundTo(-2);

      //--Focused--
      Thumb.StateStyles.Pressed.fill.Assign(Thumb.Fill);
      Thumb.StateStyles.Pressed.fill.Inherit := False;
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then
        Thumb.StateStyles.Pressed.fill.BackgroundMargins.Rect := TRectF.Create(1*LRatio,0,1*LRatio,0).RoundTo(-2)
      else
        Thumb.StateStyles.Pressed.fill.BackgroundMargins.Rect := TRectF.Create(0,1*LRatio,0,1*LRatio).RoundTo(-2);

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
procedure ALApplyTrackBarTheme(const ATheme: String; const ATrackBar: TALCustomTrack);
begin
  Var LApplyTrackBarThemeInfo: TALApplyTrackBarThemeInfo;
  If not ALTrackBarThemes.TryGetValue(Atheme,LApplyTrackBarThemeInfo) then
    Raise Exception.Createfmt('The theme "%s" could not be found', [ATheme]);
  ATrackBar.BeginUpdate;
  try
    LApplyTrackBarThemeInfo.ApplyThemeProc(ATrackBar, LApplyTrackBarThemeInfo.DefaultHeight);
  finally
    ATrackBar.EndUpdate;
  end;
end;

///////////////////
// RANGETRACKBAR //
///////////////////

{**********************************************************************************************}
procedure ALApplyRangeTrackBarTheme(const ATheme: String; const ARangeTrackBar: TALCustomTrack);
begin
  Var LApplyTrackBarThemeInfo: TALApplyTrackBarThemeInfo;
  If not ALTrackBarThemes.TryGetValue(Atheme,LApplyTrackBarThemeInfo) then
    Raise Exception.Createfmt('The theme "%s" could not be found', [ATheme]);
  ARangeTrackBar.BeginUpdate;
  try
    LApplyTrackBarThemeInfo.ApplyThemeProc(ARangeTrackBar, LApplyTrackBarThemeInfo.DefaultHeight);
  finally
    ARangeTrackBar.EndUpdate;
  end;
end;

initialization
  ALEditThemes := TDictionary<String, TALApplyEditThemeInfo>.Create;
  ALMemoThemes := TDictionary<String, TALApplyEditThemeInfo>.Create;
  ALButtonThemes := TDictionary<String, TALApplyButtonThemeInfo>.Create;
  ALCheckBoxThemes := TDictionary<String, TALApplyCheckBoxThemeInfo>.Create;
  ALRadioButtonThemes := TDictionary<String, TALApplyRadioButtonThemeInfo>.Create;
  ALSwitchThemes := TDictionary<String, TALApplySwitchThemeInfo>.Create;
  ALTrackBarThemes := TDictionary<String, TALApplyTrackBarThemeInfo>.Create;
  ALRangeTrackBarThemes := TDictionary<String, TALApplyTrackBarThemeInfo>.Create;

  ALEditThemes.Add('Default', TALApplyEditThemeInfo.create(ALResetEditTheme, 16{ADefaultFontSize}));
  ALEditThemes.Add('Material3.Light.Filled', TALApplyEditThemeInfo.create(ALApplyMaterial3LightFilledEditTheme, 16{ADefaultFontSize}));
  ALEditThemes.Add('Material3.Light.Filled.Error', TALApplyEditThemeInfo.create(ALApplyMaterial3LightFilledErrorEditTheme, 16{ADefaultFontSize}));
  ALEditThemes.Add('Material3.Light.Outlined', TALApplyEditThemeInfo.create(ALApplyMaterial3LightOutlinedEditTheme, 16{ADefaultFontSize}));
  ALEditThemes.Add('Material3.Light.Outlined.Error', TALApplyEditThemeInfo.create(ALApplyMaterial3LightOutlinedErrorEditTheme, 16{ADefaultFontSize}));
  ALEditThemes.Add('Material3.Dark.Filled', TALApplyEditThemeInfo.create(ALApplyMaterial3DarkFilledEditTheme, 16{ADefaultFontSize}));
  ALEditThemes.Add('Material3.Dark.Filled.Error', TALApplyEditThemeInfo.create(ALApplyMaterial3DarkFilledErrorEditTheme, 16{ADefaultFontSize}));
  ALEditThemes.Add('Material3.Dark.Outlined', TALApplyEditThemeInfo.create(ALApplyMaterial3DarkOutlinedEditTheme, 16{ADefaultFontSize}));
  ALEditThemes.Add('Material3.Dark.Outlined.Error', TALApplyEditThemeInfo.create(ALApplyMaterial3DarkOutlinedErrorEditTheme, 16{ADefaultFontSize}));
  ALEditThemes.Add('Lama.Outlined', TALApplyEditThemeInfo.create(ALApplyFacebookOutlinedEditTheme, 16{ADefaultFontSize}));
  ALEditThemes.Add('Lama.Outlined.Error', TALApplyEditThemeInfo.create(ALApplyFacebookOutlinedErrorEditTheme, 16{ADefaultFontSize}));

  ALMemoThemes.Add('Default', TALApplyEditThemeInfo.create(ALResetEditTheme, 16{ADefaultFontSize}));
  ALMemoThemes.Add('Material3.Light.Filled', TALApplyEditThemeInfo.create(ALApplyMaterial3LightFilledEditTheme, 16{ADefaultFontSize}));
  ALMemoThemes.Add('Material3.Light.Filled.Error', TALApplyEditThemeInfo.create(ALApplyMaterial3LightFilledErrorEditTheme, 16{ADefaultFontSize}));
  ALMemoThemes.Add('Material3.Light.Outlined', TALApplyEditThemeInfo.create(ALApplyMaterial3LightOutlinedEditTheme, 16{ADefaultFontSize}));
  ALMemoThemes.Add('Material3.Light.Outlined.Error', TALApplyEditThemeInfo.create(ALApplyMaterial3LightOutlinedErrorEditTheme, 16{ADefaultFontSize}));
  ALMemoThemes.Add('Material3.Dark.Filled', TALApplyEditThemeInfo.create(ALApplyMaterial3DarkFilledEditTheme, 16{ADefaultFontSize}));
  ALMemoThemes.Add('Material3.Dark.Filled.Error', TALApplyEditThemeInfo.create(ALApplyMaterial3DarkFilledErrorEditTheme, 16{ADefaultFontSize}));
  ALMemoThemes.Add('Material3.Dark.Outlined', TALApplyEditThemeInfo.create(ALApplyMaterial3DarkOutlinedEditTheme, 16{ADefaultFontSize}));
  ALMemoThemes.Add('Material3.Dark.Outlined.Error', TALApplyEditThemeInfo.create(ALApplyMaterial3DarkOutlinedErrorEditTheme, 16{ADefaultFontSize}));
  ALMemoThemes.Add('Lama.Outlined', TALApplyEditThemeInfo.create(ALApplyFacebookOutlinedEditTheme, 16{ADefaultFontSize}));
  ALMemoThemes.Add('Lama.Outlined.Error', TALApplyEditThemeInfo.create(ALApplyFacebookOutlinedErrorEditTheme, 16{ADefaultFontSize}));

  ALButtonThemes.Add('Default', TALApplyButtonThemeInfo.create(ALResetButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Windows', TALApplyButtonThemeInfo.create(ALApplyWindowsButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Light.Filled', TALApplyButtonThemeInfo.create(ALApplyMaterial3LightFilledButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Light.Outlined', TALApplyButtonThemeInfo.create(ALApplyMaterial3LightOutlinedButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Light.Text', TALApplyButtonThemeInfo.create(ALApplyMaterial3LightTextButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Light.Elevated', TALApplyButtonThemeInfo.create(ALApplyMaterial3LightElevatedButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Light.Tonal', TALApplyButtonThemeInfo.create(ALApplyMaterial3LightTonalButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Light.Icon.Filled', TALApplyButtonThemeInfo.create(ALApplyMaterial3LightFilledIconButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Light.Icon.Outlined', TALApplyButtonThemeInfo.create(ALApplyMaterial3LightOutlinedIconButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Light.Icon.Standard', TALApplyButtonThemeInfo.create(ALApplyMaterial3LightStandardIconButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Light.Icon.Tonal', TALApplyButtonThemeInfo.create(ALApplyMaterial3LightTonalIconButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Dark.Filled', TALApplyButtonThemeInfo.create(ALApplyMaterial3DarkFilledButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Dark.Outlined', TALApplyButtonThemeInfo.create(ALApplyMaterial3DarkOutlinedButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Dark.Text', TALApplyButtonThemeInfo.create(ALApplyMaterial3DarkTextButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Dark.Elevated', TALApplyButtonThemeInfo.create(ALApplyMaterial3DarkElevatedButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Dark.Tonal', TALApplyButtonThemeInfo.create(ALApplyMaterial3DarkTonalButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Dark.Icon.Filled', TALApplyButtonThemeInfo.create(ALApplyMaterial3DarkFilledIconButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Dark.Icon.Outlined', TALApplyButtonThemeInfo.create(ALApplyMaterial3DarkOutlinedIconButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Dark.Icon.Standard', TALApplyButtonThemeInfo.create(ALApplyMaterial3DarkStandardIconButtonTheme, 14{ADefaultFontSize}));
  ALButtonThemes.Add('Material3.Dark.Icon.Tonal', TALApplyButtonThemeInfo.create(ALApplyMaterial3DarkTonalIconButtonTheme, 14{ADefaultFontSize}));

  ALCheckBoxThemes.Add('Default', TALApplyCheckBoxThemeInfo.create(ALResetCheckBoxTheme, 18{ADefaultHeight}));
  ALCheckBoxThemes.Add('Material3.Light', TALApplyCheckBoxThemeInfo.create(ALApplyMaterial3LightCheckBoxTheme, 18{ADefaultHeight}));
  ALCheckBoxThemes.Add('Material3.Light.Error', TALApplyCheckBoxThemeInfo.create(ALApplyMaterial3LightErrorCheckBoxTheme, 18{ADefaultHeight}));
  ALCheckBoxThemes.Add('Material3.Dark', TALApplyCheckBoxThemeInfo.create(ALApplyMaterial3DarkCheckBoxTheme, 18{ADefaultHeight}));
  ALCheckBoxThemes.Add('Material3.Dark.Error', TALApplyCheckBoxThemeInfo.create(ALApplyMaterial3DarkErrorCheckBoxTheme, 18{ADefaultHeight}));

  ALRadioButtonThemes.Add('Default', TALApplyRadioButtonThemeInfo.create(ALResetRadioButtonTheme, 20{ADefaultHeight}));
  ALRadioButtonThemes.Add('Material3.Light', TALApplyRadioButtonThemeInfo.create(ALApplyMaterial3LightRadioButtonTheme, 20{ADefaultHeight}));
  ALRadioButtonThemes.Add('Material3.Light.Error', TALApplyRadioButtonThemeInfo.create(ALApplyMaterial3LightErrorRadioButtonTheme, 20{ADefaultHeight}));
  ALRadioButtonThemes.Add('Material3.Dark', TALApplyRadioButtonThemeInfo.create(ALApplyMaterial3DarkRadioButtonTheme, 20{ADefaultHeight}));
  ALRadioButtonThemes.Add('Material3.Dark.Error', TALApplyRadioButtonThemeInfo.create(ALApplyMaterial3DarkErrorRadioButtonTheme, 20{ADefaultHeight}));

  ALSwitchThemes.Add('Default', TALApplySwitchThemeInfo.create(ALResetSwitchTheme, 32{ADefaultHeight}));
  ALSwitchThemes.Add('Material3.Light', TALApplySwitchThemeInfo.create(ALApplyMaterial3LightSwitchTheme, 32{ADefaultHeight}));
  ALSwitchThemes.Add('Material3.Dark', TALApplySwitchThemeInfo.create(ALApplyMaterial3DarkSwitchTheme, 32{ADefaultHeight}));

  ALTrackBarThemes.Add('Default', TALApplyTrackBarThemeInfo.create(ALResetTrackBarTheme, 32{ADefaultHeight}));
  ALTrackBarThemes.Add('Material3.Light', TALApplyTrackBarThemeInfo.create(ALApplyMaterial3LightTrackBarTheme, 44{ADefaultHeight}));
  ALTrackBarThemes.Add('Material3.Dark', TALApplyTrackBarThemeInfo.create(ALApplyMaterial3DarkTrackBarTheme, 44{ADefaultHeight}));

  ALRangeTrackBarThemes.Add('Default', TALApplyTrackBarThemeInfo.create(ALResetTrackBarTheme, 32{ADefaultHeight}));
  ALRangeTrackBarThemes.Add('Material3.Light', TALApplyTrackBarThemeInfo.create(ALApplyMaterial3LightTrackBarTheme, 44{ADefaultHeight}));
  ALRangeTrackBarThemes.Add('Material3.Dark', TALApplyTrackBarThemeInfo.create(ALApplyMaterial3DarkTrackBarTheme, 44{ADefaultHeight}));

finalization
  ALFreeAndNil(ALEditThemes);
  ALFreeAndNil(ALMemoThemes);
  ALFreeAndNil(ALButtonThemes);
  ALFreeAndNil(ALCheckBoxThemes);
  ALFreeAndNil(ALRadioButtonThemes);
  ALFreeAndNil(ALSwitchThemes);
  ALFreeAndNil(ALTrackBarThemes);
  ALFreeAndNil(ALRangeTrackBarThemes);

end.
