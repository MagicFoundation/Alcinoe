unit Alcinoe.FMX.Themes;

interface

{$I Alcinoe.inc}

uses
  System.Generics.Collections,
  Alcinoe.FMX.Edit,
  Alcinoe.FMX.StdCtrls;

Type
  TALApplyEditThemeProc = Procedure(const AEdit: TALBaseEdit);
  TALApplyButtonThemeProc = Procedure(const AButton: TALButton);
  TALApplyCheckBoxThemeProc = Procedure(const ACheckBox: TALCheckBox);

var
  ALEditThemes: TDictionary<String, TALApplyEditThemeProc>;
  ALMemoThemes: TDictionary<String, TALApplyEditThemeProc>;
  ALButtonThemes: TDictionary<String, TALApplyButtonThemeProc>;
  ALCheckBoxThemes: TDictionary<String, TALApplyCheckBoxThemeProc>;
  ALRadioButtonThemes: TDictionary<String, TALApplyCheckBoxThemeProc>;

procedure ALApplyEditTheme(const ATheme: String; const AEdit: TALBaseEdit);
procedure ALApplyMemoTheme(const ATheme: String; const AMemo: TALBaseEdit);
procedure ALApplyButtonTheme(const ATheme: String; const AButton: TALButton);
procedure ALApplyCheckBoxTheme(const ATheme: String; const ACheckBox: TALCheckBox);
procedure ALApplyRadioButtonTheme(const ATheme: String; const ARadioButton: TALCheckBox);

implementation

uses
  System.SysUtils,
  System.Types,
  System.uitypes,
  FMX.types,
  FMX.Controls,
  FMX.Graphics,
  Alcinoe.Common,
  Alcinoe.StringUtils,
  Alcinoe.FMX.Memo,
  Alcinoe.FMX.Graphics;

//////////
// EDIT //
//////////

{***************************************************}
procedure ALResetEditTheme(const AEdit: TALBaseEdit);
begin
  With AEdit do begin
    //--Enabled (default)--
    if AEdit is TALEdit then TALEdit(AEdit).AutoSize := True
    else if AEdit is TALMemo then TALMemo(AEdit).AutoSizeLineCount := 3;
    padding.Rect := padding.DefaultValue;
    Corners := AllCorners;
    Sides := AllSides;
    XRadius := 0;
    YRadius := 0;
    TintColor := TalphaColors.null;
    Fill.Color := fill.DefaultColor;
    Stroke.Color := Stroke.DefaultColor;
    Stroke.Thickness := Stroke.DefaultThickness;
    var LPrevIsHtml := TextSettings.IsHtml;
    TextSettings.Reset;
    TextSettings.IsHtml := LPrevIsHtml;
    LPrevIsHtml := LabelTextSettings.IsHtml;
    LabelTextSettings.Reset;
    LabelTextSettings.IsHtml := LPrevIsHtml;
    LPrevIsHtml := SupportingTextSettings.IsHtml;
    SupportingTextSettings.Reset;
    SupportingTextSettings.IsHtml := LPrevIsHtml;
    Shadow.Reset;
    PromptTextcolor := TAlphaColors.null;
    //--Disabled--
    StateStyles.Disabled.TintColor := TAlphaColors.Null;
    StateStyles.Disabled.Opacity := TControl.DefaultDisabledOpacity;
    StateStyles.Disabled.Fill.Reset;
    StateStyles.Disabled.Stroke.Reset;
    StateStyles.Disabled.TextSettings.Reset;
    StateStyles.Disabled.LabelTextSettings.Reset;
    StateStyles.Disabled.SupportingTextSettings.Reset;
    StateStyles.Disabled.Shadow.Reset;
    StateStyles.Disabled.PromptTextcolor := TAlphaColors.Null;
    //--Hovered--
    StateStyles.Hovered.TintColor := TAlphaColors.Null;
    StateStyles.Hovered.Fill.Reset;
    StateStyles.Hovered.Stroke.Reset;
    StateStyles.Hovered.TextSettings.Reset;
    StateStyles.Hovered.LabelTextSettings.Reset;
    StateStyles.Hovered.SupportingTextSettings.Reset;
    StateStyles.Hovered.Shadow.Reset;
    StateStyles.Hovered.PromptTextcolor := TAlphaColors.Null;
    //--Focused--
    StateStyles.Focused.TintColor := TAlphaColors.Null;
    StateStyles.Focused.Fill.Reset;
    StateStyles.Focused.Stroke.Reset;
    StateStyles.Focused.TextSettings.Reset;
    StateStyles.Focused.LabelTextSettings.Reset;
    StateStyles.Focused.SupportingTextSettings.Reset;
    StateStyles.Focused.Shadow.Reset;
    StateStyles.Focused.PromptTextcolor := TAlphaColors.Null;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterial3LightFilledEditTheme(const AEdit: TALBaseEdit);
begin
  ALResetEditTheme(AEdit);
  With AEdit do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(16{Left}, 12{Top}, 16{Right}, 12{Bottom});
    Corners := [TCorner.TopLeft, Tcorner.TopRight];
    Sides := [TSide.Bottom];
    XRadius := 4;
    YRadius := 4;
    DefStyleAttr := 'Material3LightFilledEditTextStyle';
    DefStyleRes := '';
    TintColor := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
    Fill.Color := $FFE6E0E9; // md.sys.color.surface-container-highest / md.ref.palette.neutral90
    Stroke.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
    TextSettings.Font.Size := 16;
    TextSettings.Font.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
    LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
    LabelTextSettings.Font.Size := 12;
    LabelTextSettings.Font.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
    LabelTextSettings.Margins.Rect := TRectF.Create(0,-4,0,4);
    SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
    SupportingTextSettings.Font.Size := 12;
    SupportingTextSettings.Font.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
    SupportingTextSettings.Margins.Rect := TRectF.Create(0,4,0,0);
    PromptTextcolor := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
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
    StateStyles.Focused.Stroke.Thickness := 3;
    StateStyles.Focused.LabelTextSettings.Assign(LabelTextSettings);
    StateStyles.Focused.LabelTextSettings.Inherit := False;
    StateStyles.Focused.LabelTextSettings.Font.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
    StateStyles.Focused.PromptTextcolor := StateStyles.Focused.LabelTextSettings.Font.Color;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#e4964192-72ad-414f-85b4-4b4357abb83c
procedure ALApplyMaterial3LightOutlinedEditTheme(const AEdit: TALBaseEdit);
begin
  ALResetEditTheme(AEdit);
  With AEdit do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(16{Left}, 16{Top}, 16{Right}, 16{Bottom});
    XRadius := 4;
    YRadius := 4;
    DefStyleAttr := 'Material3LightOutlinedEditTextStyle';
    DefStyleRes := '';
    TintColor := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
    Fill.Color := $FFFFFFFF;
    Stroke.Color := $FF79747E; // md.sys.color.outline / md.ref.palette.neutral-variant50
    TextSettings.Font.Size := 16;
    TextSettings.Font.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
    LabelTextSettings.Layout := TALEdit.TLabelTextLayout.floating;
    LabelTextSettings.Font.Size := 12;
    LabelTextSettings.Font.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
    LabelTextSettings.Margins.Rect := TRectF.Create(0,0,0,-6);
    SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
    SupportingTextSettings.Font.Size := 12;
    SupportingTextSettings.Font.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
    SupportingTextSettings.Margins.Rect := TRectF.Create(0,4,0,0);
    PromptTextcolor := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
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
    StateStyles.Focused.Stroke.Thickness := 3;
    StateStyles.Focused.LabelTextSettings.Assign(LabelTextSettings);
    StateStyles.Focused.LabelTextSettings.Inherit := False;
    StateStyles.Focused.LabelTextSettings.Font.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
    StateStyles.Focused.PromptTextcolor := StateStyles.Focused.LabelTextSettings.Font.Color;
  end;
end;

{**************************************}
//https://llama.meta.com/llama-downloads
procedure ALApplyFacebookOutlinedEditTheme(const AEdit: TALBaseEdit);
begin
  ALResetEditTheme(AEdit);
  With AEdit do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(16{Left}, 12{Top}, 16{Right}, 12{Bottom});
    XRadius := 8;
    YRadius := 8;
    DefStyleAttr := '';
    DefStyleRes := '';
    TintColor := $FF1c2b33;
    Fill.Color := $FFFFFFFF;
    Stroke.Color := $FFdee3e9;
    TextSettings.Font.Size := 16;
    TextSettings.Font.Color := $FF1c2b33;
    LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
    LabelTextSettings.Font.Size := 12;
    LabelTextSettings.Font.Color := $FF465a69;
    LabelTextSettings.Margins.Rect := TRectF.Create(0,-4,0,4);
    SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
    SupportingTextSettings.Font.Size := 12;
    SupportingTextSettings.Font.Color := $FF465a69;
    SupportingTextSettings.Margins.Rect := TRectF.Create(0,4,0,0);
    PromptTextcolor := $FF465a69;
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
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterial3DarkFilledEditTheme(const AEdit: TALBaseEdit);
begin
  ALResetEditTheme(AEdit);
  With AEdit do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(16{Left}, 12{Top}, 16{Right}, 12{Bottom});
    Corners := [TCorner.TopLeft, Tcorner.TopRight];
    Sides := [TSide.Bottom];
    XRadius := 4;
    YRadius := 4;
    DefStyleAttr := 'Material3DarkFilledEditTextStyle';
    DefStyleRes := '';
    TintColor := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
    Fill.Color := $FF36343B; // md.sys.color.surface-container-highest / md.ref.palette.neutral22
    Stroke.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
    TextSettings.Font.Size := 16;
    TextSettings.Font.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
    LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
    LabelTextSettings.Font.Size := 12;
    LabelTextSettings.Font.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
    LabelTextSettings.Margins.Rect := TRectF.Create(0,-4,0,4);
    SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
    SupportingTextSettings.Font.Size := 12;
    SupportingTextSettings.Font.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
    SupportingTextSettings.Margins.Rect := TRectF.Create(0,4,0,0);
    PromptTextcolor := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
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
    StateStyles.Focused.Stroke.Thickness := 3;
    StateStyles.Focused.LabelTextSettings.Assign(LabelTextSettings);
    StateStyles.Focused.LabelTextSettings.Inherit := False;
    StateStyles.Focused.LabelTextSettings.Font.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
    StateStyles.Focused.PromptTextcolor := StateStyles.Focused.LabelTextSettings.Font.Color;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#e4964192-72ad-414f-85b4-4b4357abb83c
procedure ALApplyMaterial3DarkOutlinedEditTheme(const AEdit: TALBaseEdit);
begin
  ALResetEditTheme(AEdit);
  With AEdit do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(16{Left}, 16{Top}, 16{Right}, 16{Bottom});
    XRadius := 4;
    YRadius := 4;
    DefStyleAttr := 'Material3DarkOutlinedEditTextStyle';
    DefStyleRes := '';
    TintColor := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
    Fill.Color := $FF000000;
    Stroke.Color := $FF938F99; // md.sys.color.outline / md.ref.palette.neutral-variant60
    TextSettings.Font.Size := 16;
    TextSettings.Font.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
    LabelTextSettings.Layout := TALEdit.TLabelTextLayout.floating;
    LabelTextSettings.Font.Size := 12;
    LabelTextSettings.Font.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
    LabelTextSettings.Margins.Rect := TRectF.Create(0,0,0,-6);
    SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
    SupportingTextSettings.Font.Size := 12;
    SupportingTextSettings.Font.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
    SupportingTextSettings.Margins.Rect := TRectF.Create(0,4,0,0);
    PromptTextcolor := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
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
    StateStyles.Focused.Stroke.Thickness := 3;
    StateStyles.Focused.LabelTextSettings.Assign(LabelTextSettings);
    StateStyles.Focused.LabelTextSettings.Inherit := False;
    StateStyles.Focused.LabelTextSettings.Font.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
    StateStyles.Focused.PromptTextcolor := StateStyles.Focused.LabelTextSettings.Font.Color;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterial3LightFilledErrorEditTheme(const AEdit: TALBaseEdit);
begin
  ALResetEditTheme(AEdit);
  With AEdit do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(16{Left}, 12{Top}, 16{Right}, 12{Bottom});
    Corners := [TCorner.TopLeft, Tcorner.TopRight];
    Sides := [TSide.Bottom];
    XRadius := 4;
    YRadius := 4;
    DefStyleAttr := 'Material3LightFilledErrorEditTextStyle';
    DefStyleRes := '';
    TintColor := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
    Fill.Color := $FFE6E0E9; // md.sys.color.surface-container-highest / md.ref.palette.neutral90
    Stroke.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
    TextSettings.Font.Size := 16;
    TextSettings.Font.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
    LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
    LabelTextSettings.Font.Size := 12;
    LabelTextSettings.Font.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
    LabelTextSettings.Margins.Rect := TRectF.Create(0,-4,0,4);
    SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
    SupportingTextSettings.Font.Size := 12;
    SupportingTextSettings.Font.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
    SupportingTextSettings.Margins.Rect := TRectF.Create(0,4,0,0);
    PromptTextcolor := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
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
    StateStyles.Focused.Stroke.Thickness := 3;
    StateStyles.Focused.PromptTextcolor := LabelTextSettings.Font.Color;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#e4964192-72ad-414f-85b4-4b4357abb83c
procedure ALApplyMaterial3LightOutlinedErrorEditTheme(const AEdit: TALBaseEdit);
begin
  ALResetEditTheme(AEdit);
  With AEdit do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(16{Left}, 16{Top}, 16{Right}, 16{Bottom});
    XRadius := 4;
    YRadius := 4;
    DefStyleAttr := 'Material3LightOutlinedErrorEditTextStyle';
    DefStyleRes := '';
    TintColor := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
    Fill.Color := $FFFFFFFF;
    Stroke.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
    TextSettings.Font.Size := 16;
    TextSettings.Font.Color := $FF1D1B20; // md.sys.color.on-surface / md.ref.palette.neutral10
    LabelTextSettings.Layout := TALEdit.TLabelTextLayout.floating;
    LabelTextSettings.Font.Size := 12;
    LabelTextSettings.Font.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
    LabelTextSettings.Margins.Rect := TRectF.Create(0,0,0,-6);
    SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
    SupportingTextSettings.Font.Size := 12;
    SupportingTextSettings.Font.Color := $FFB3261E; // md.sys.color.error / md.ref.palette.error40
    SupportingTextSettings.Margins.Rect := TRectF.Create(0,4,0,0);
    PromptTextcolor := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
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
    StateStyles.Focused.Stroke.Thickness := 3;
    StateStyles.Focused.PromptTextcolor := LabelTextSettings.Font.Color;
  end;
end;

{**************************************}
//https://llama.meta.com/llama-downloads
procedure ALApplyFacebookOutlinedErrorEditTheme(const AEdit: TALBaseEdit);
begin
  ALResetEditTheme(AEdit);
  With AEdit do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(16{Left}, 12{Top}, 16{Right}, 12{Bottom});
    XRadius := 8;
    YRadius := 8;
    DefStyleAttr := '';
    DefStyleRes := '';
    TintColor := $FF1c2b33;
    Fill.Color := $FFFFFFFF;
    Stroke.Color := $FFc80a28;
    TextSettings.Font.Size := 16;
    TextSettings.Font.Color := $FF1c2b33;
    LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
    LabelTextSettings.Font.Size := 12;
    LabelTextSettings.Font.Color := $FF465a69;
    LabelTextSettings.Margins.Rect := TRectF.Create(0,-4,0,4);
    SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
    SupportingTextSettings.Font.Size := 12;
    SupportingTextSettings.Font.Color := $FFc80a28;
    SupportingTextSettings.Margins.Rect := TRectF.Create(0,4,0,0);
    PromptTextcolor := $FF465a69;
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
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterial3DarkFilledErrorEditTheme(const AEdit: TALBaseEdit);
begin
  ALResetEditTheme(AEdit);
  With AEdit do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(16{Left}, 12{Top}, 16{Right}, 12{Bottom});
    Corners := [TCorner.TopLeft, Tcorner.TopRight];
    Sides := [TSide.Bottom];
    XRadius := 4;
    YRadius := 4;
    DefStyleAttr := 'Material3DarkFilledErrorEditTextStyle';
    DefStyleRes := '';
    TintColor := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
    Fill.Color := $FF36343B; // md.sys.color.surface-container-highest / md.ref.palette.neutral22
    Stroke.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
    TextSettings.Font.Size := 16;
    TextSettings.Font.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
    LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
    LabelTextSettings.Font.Size := 12;
    LabelTextSettings.Font.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
    LabelTextSettings.Margins.Rect := TRectF.Create(0,-4,0,4);
    SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
    SupportingTextSettings.Font.Size := 12;
    SupportingTextSettings.Font.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
    SupportingTextSettings.Margins.Rect := TRectF.Create(0,4,0,0);
    PromptTextcolor := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
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
    StateStyles.Focused.Stroke.Thickness := 3;
    StateStyles.Focused.PromptTextcolor := LabelTextSettings.Font.Color;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#e4964192-72ad-414f-85b4-4b4357abb83c
procedure ALApplyMaterial3DarkOutlinedErrorEditTheme(const AEdit: TALBaseEdit);
begin
  ALResetEditTheme(AEdit);
  With AEdit do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(16{Left}, 16{Top}, 16{Right}, 16{Bottom});
    XRadius := 4;
    YRadius := 4;
    DefStyleAttr := 'Material3DarkOutlinedErrorEditTextStyle';
    DefStyleRes := '';
    TintColor := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
    Fill.Color := $FF000000;
    Stroke.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
    TextSettings.Font.Size := 16;
    TextSettings.Font.Color := $FFE6E0E9; // md.sys.color.on-surface / md.ref.palette.neutral90
    LabelTextSettings.Layout := TALEdit.TLabelTextLayout.floating;
    LabelTextSettings.Font.Size := 12;
    LabelTextSettings.Font.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
    LabelTextSettings.Margins.Rect := TRectF.Create(0,0,0,-6);
    SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
    SupportingTextSettings.Font.Size := 12;
    SupportingTextSettings.Font.Color := $FFF2B8B5; // md.sys.color.error / md.ref.palette.error80
    SupportingTextSettings.Margins.Rect := TRectF.Create(0,4,0,0);
    PromptTextcolor := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
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
    StateStyles.Focused.Stroke.Thickness := 3;
    StateStyles.Focused.PromptTextcolor := LabelTextSettings.Font.Color;
  end;
end;

{*************************************************************************}
procedure ALApplyEditTheme(const ATheme: String; const AEdit: TALBaseEdit);
begin
  Var LApplyEditThemeProc: TALApplyEditThemeProc;
  If not ALEditThemes.TryGetValue(Atheme,LApplyEditThemeProc) then
    Raise Exception.Createfmt('The theme "%s" could not be found', [ATheme]);
  AEdit.BeginUpdate;
  try
    LApplyEditThemeProc(AEdit);
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
  Var LApplyEditThemeProc: TALApplyEditThemeProc;
  If not ALMemoThemes.TryGetValue(Atheme,LApplyEditThemeProc) then
    Raise Exception.Createfmt('The theme "%s" could not be found', [ATheme]);
  AMemo.BeginUpdate;
  try
    LApplyEditThemeProc(AMemo);
  finally
    AMemo.EndUpdate;
  end;
end;


////////////
// BUTTON //
////////////

{*****************************************************}
procedure ALResetButtonTheme(const AButton: TALButton);
begin
  With AButton do begin
    //--Enabled (default)--
    TouchTargetExpansion.Rect := TRectF.Empty;
    AutoSize := True;
    Padding.Rect := Padding.DefaultValue;
    Corners := AllCorners;
    Sides := AllSides;
    XRadius := 0;
    YRadius := 0;
    Fill.Reset;
    Stroke.Reset;
    var LPrevIsHtml := TextSettings.IsHtml;
    TextSettings.Reset;
    TextSettings.IsHtml := LPrevIsHtml;
    Shadow.Reset;
    //--Disabled--
    StateStyles.Disabled.Reset;
    //--Hovered--
    StateStyles.Hovered.Reset;
    //--Pressed--
    StateStyles.Pressed.Reset;
    //--Focused--
    StateStyles.Focused.Reset;
  end;
end;

{************************************************************}
procedure ALApplyWindowsButtonTheme(const AButton: TALButton);
begin
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
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
    StateStyles.focused.Stroke.Thickness := 2;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#cbfd91a6-d688-4be7-9a69-672549de3ea9
procedure ALApplyMaterial3LightFilledButtonTheme(const AButton: TALButton);
begin
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
    XRadius := -50;
    YRadius := -50;
    Fill.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
    Stroke.Color := Talphacolors.Null;
    TextSettings.Font.Color := $FFFFFFFF; // md.sys.color.on-primary // md.ref.palette.primary100
    TextSettings.LetterSpacing := 0.1;
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
    StateStyles.Hovered.Shadow.blur := 2;
    StateStyles.Hovered.Shadow.OffsetY := 1;
    StateStyles.Hovered.Transition.Duration := 0.2;
    //--Pressed--
    StateStyles.Pressed.StateLayer.UseContentColor := True;
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //--Focused--
    StateStyles.Focused.StateLayer.UseContentColor := True;
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#4a0c06da-0b2f-47de-a583-97e0ae80b5a5
procedure ALApplyMaterial3LightOutlinedButtonTheme(const AButton: TALButton);
begin
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
    XRadius := -50;
    YRadius := -50;
    Fill.Color := Talphacolors.Null;
    Stroke.Color := $FF79747E; // md.sys.color.outline / md.ref.palette.neutral-variant50
    TextSettings.Font.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
    TextSettings.LetterSpacing := 0.1;
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
    StateStyles.Hovered.Transition.Duration := 0.2;
    //--Pressed--
    StateStyles.Pressed.StateLayer.UseContentColor := True;
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //--Focused--
    StateStyles.Focused.StateLayer.UseContentColor := True;
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Stroke.assign(Stroke);
    StateStyles.Focused.Stroke.inherit := False;
    StateStyles.Focused.Stroke.Color := $FF6750A4;  // md.sys.color.primary / md.ref.palette.primary40
    StateStyles.Focused.Transition.Duration := 0.2;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#398d84eb-fc8a-4c8a-bfb4-82d2e85dee4d
procedure ALApplyMaterial3LightTextButtonTheme(const AButton: TALButton);
begin
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(12{Left}, 12{Top}, 12{Right}, 12{Bottom});
    XRadius := -50;
    YRadius := -50;
    Fill.Color := Talphacolors.Null;
    Stroke.Color := Talphacolors.Null;
    TextSettings.Font.Color := $FF6750A4; // md.sys.color.primary // md.ref.palette.primary40
    TextSettings.LetterSpacing := 0.1;
    //--Disabled--
    StateStyles.Disabled.Opacity := 1;
    StateStyles.Disabled.TextSettings.Assign(TextSettings);
    StateStyles.Disabled.TextSettings.Inherit := False;
    StateStyles.Disabled.TextSettings.Font.Color := ALSetColorAlpha($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
    //--Hovered--
    StateStyles.Hovered.StateLayer.UseContentColor := True;
    StateStyles.Hovered.StateLayer.Opacity := 0.08;
    StateStyles.Hovered.Transition.Duration := 0.2;
    //--Pressed--
    StateStyles.Pressed.StateLayer.UseContentColor := True;
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //--Focused--
    StateStyles.Focused.StateLayer.UseContentColor := True;
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#c75be779-5a59-4748-98d4-e47fc888d0b1
procedure ALApplyMaterial3LightElevatedButtonTheme(const AButton: TALButton);
begin
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
    XRadius := -50;
    YRadius := -50;
    Fill.Color := $FFF7F2FA; // md.sys.color.surface-container-low / md.ref.palette.neutral96
    Stroke.Color := Talphacolors.Null;
    TextSettings.Font.Color := $FF6750A4; // md.sys.color.primary // md.ref.palette.primary40
    TextSettings.LetterSpacing := 0.1;
    Shadow.Color := ALSetColorAlpha($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
    Shadow.blur := 2;
    Shadow.OffsetY := 1;
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
    StateStyles.Hovered.Shadow.blur := 3;
    StateStyles.Hovered.Shadow.OffsetY := 1;
    StateStyles.Hovered.Transition.Duration := 0.2;
    //--Pressed--
    StateStyles.Pressed.StateLayer.UseContentColor := True;
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //--Focused--
    StateStyles.Focused.StateLayer.UseContentColor := True;
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#6ce8b926-87c4-4600-9bec-5deb4aaa65d8
procedure ALApplyMaterial3LightTonalButtonTheme(const AButton: TALButton);
begin
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
    XRadius := -50;
    YRadius := -50;
    Fill.Color := $FFE8DEF8; // md.sys.color.secondary-container / md.ref.palette.secondary90
    Stroke.Color := Talphacolors.Null;
    TextSettings.Font.Color := $FF1D192B; // md.sys.color.on-secondary-container // md.ref.palette.secondary10
    TextSettings.LetterSpacing := 0.1;
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
    StateStyles.Hovered.Shadow.blur := 2;
    StateStyles.Hovered.Shadow.OffsetY := 1;
    StateStyles.Hovered.Transition.Duration := 0.2;
    //--Pressed--
    StateStyles.Pressed.StateLayer.UseContentColor := True;
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //--Focused--
    StateStyles.Focused.StateLayer.UseContentColor := True;
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#5309610a-4515-44f9-830d-880e2a2240a2
procedure ALApplyMaterial3LightFilledIconButtonTheme(const AButton: TALButton);
begin
  var LPrevResourceName := Abutton.Fill.ResourceName;
  var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    Width := 40;
    Height := 40;
    TouchTargetExpansion.Rect := TRectf.Create(4,4,4,4);
    XRadius := -50;
    YRadius := -50;
    Fill.ImageMargins.Rect := TRectF.Create(8,8,8,8);
    Fill.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
    Fill.ResourceName := LPrevResourceName;
    Stroke.Color := TALphaColors.Null;
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
    StateStyles.Hovered.Transition.Duration := 0.2;
    //Filled icon button hover icon color: $FFFFFFFF
    //--Pressed--
    StateStyles.Pressed.StateLayer.Color := $FFFFFFFF; // md.sys.color.on-primary / md.ref.palette.primary100
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //Filled icon button pressed icon color: $FFFFFFFF
    //--Focused--
    StateStyles.Focused.StateLayer.Color := $FFFFFFFF; // md.sys.color.on-primary / md.ref.palette.primary100
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
    //Filled icon button focus icon color: $FFFFFFFF
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#ba97cf8a-2112-47dc-af87-2e32aabccdde
procedure ALApplyMaterial3LightTonalIconButtonTheme(const AButton: TALButton);
begin
  var LPrevResourceName := Abutton.Fill.ResourceName;
  var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    Width := 40;
    Height := 40;
    TouchTargetExpansion.Rect := TRectf.Create(4,4,4,4);
    XRadius := -50;
    YRadius := -50;
    Fill.ImageMargins.Rect := TRectF.Create(8,8,8,8);
    Fill.Color := $FFE8DEF8; // md.sys.color.secondary-container / md.ref.palette.secondary90
    Fill.ResourceName := LPrevResourceName;
    Stroke.Color := TALphaColors.Null;
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
    StateStyles.Hovered.Transition.Duration := 0.2;
    //Filled tonal icon button hover icon color: $FF1D192B
    //--Pressed--
    StateStyles.Pressed.StateLayer.Color := $FF1D192B; // md.sys.color.on-secondary-container / md.ref.palette.secondary10
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //Filled tonal icon button pressed icon color: $FF1D192B
    //--Focused--
    StateStyles.Focused.StateLayer.Color := $FF1D192B; // md.sys.color.on-secondary-container / md.ref.palette.secondary10
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
    //Filled tonal icon button focus icon color: $FF1D192B
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#05e02b7f-ebf2-4f02-9709-8230db3702b4
procedure ALApplyMaterial3LightOutlinedIconButtonTheme(const AButton: TALButton);
begin
  var LPrevResourceName := Abutton.Fill.ResourceName;
  var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    Width := 40;
    Height := 40;
    TouchTargetExpansion.Rect := TRectf.Create(4,4,4,4);
    XRadius := -50;
    YRadius := -50;
    Fill.ImageMargins.Rect := TRectF.Create(8,8,8,8);
    Fill.Color := TalphaColorRec.Null;
    Fill.ResourceName := LPrevResourceName;
    Stroke.Color := $FF79747E; // md.sys.color.outline / md.ref.palette.neutral-variant50
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
    StateStyles.Hovered.Transition.Duration := 0.2;
    //Outlined icon button unselected hover icon color: $FF49454F
    //--Pressed--
    StateStyles.Pressed.StateLayer.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //Outlined icon button unselected pressed icon color: $FF49454F
    //--Focused--
    StateStyles.Focused.StateLayer.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
    //Outlined icon button focus icon color: $FF49454F
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#e63a9b45-a20c-402c-8cc5-2c67ad8aae25
procedure ALApplyMaterial3LightStandardIconButtonTheme(const AButton: TALButton);
begin
  var LPrevResourceName := Abutton.Fill.ResourceName;
  var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    Width := 40;
    Height := 40;
    TouchTargetExpansion.Rect := TRectf.Create(4,4,4,4);
    XRadius := -50;
    YRadius := -50;
    Fill.ImageMargins.Rect := TRectF.Create(8,8,8,8);
    Fill.ResourceName := LPrevResourceName;
    Fill.Color := TalphaColors.Null;
    Stroke.Color := TALphaColors.Null;
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
    StateStyles.Hovered.Transition.Duration := 0.2;
    //Icon button unselected hover icon color: $FF49454F
    //--Pressed--
    StateStyles.Pressed.StateLayer.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //Icon button unselected pressed icon color: $FF49454F
    //--Focused--
    StateStyles.Focused.StateLayer.Color := $FF49454F; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
    //Icon button unselected focus icon color: $FF49454F
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#cbfd91a6-d688-4be7-9a69-672549de3ea9
procedure ALApplyMaterial3DarkFilledButtonTheme(const AButton: TALButton);
begin
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
    XRadius := -50;
    YRadius := -50;
    Fill.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
    Stroke.Color := Talphacolors.Null;
    TextSettings.Font.Color := $FF381E72; // md.sys.color.on-primary / md.ref.palette.primary20
    TextSettings.LetterSpacing := 0.1;
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
    StateStyles.Hovered.Shadow.blur := 2;
    StateStyles.Hovered.Shadow.OffsetY := 1;
    StateStyles.Hovered.Transition.Duration := 0.2;
    //--Pressed--
    StateStyles.Pressed.StateLayer.UseContentColor := True;
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //--Focused--
    StateStyles.Focused.StateLayer.UseContentColor := True;
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#4a0c06da-0b2f-47de-a583-97e0ae80b5a5
procedure ALApplyMaterial3DarkOutlinedButtonTheme(const AButton: TALButton);
begin
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
    XRadius := -50;
    YRadius := -50;
    Fill.Color := Talphacolors.Null;
    Stroke.Color := $FF938F99; // md.sys.color.outline / md.ref.palette.neutral-variant60
    TextSettings.Font.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
    TextSettings.LetterSpacing := 0.1;
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
    StateStyles.Hovered.Transition.Duration := 0.2;
    //--Pressed--
    StateStyles.Pressed.StateLayer.UseContentColor := True;
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //--Focused--
    StateStyles.Focused.StateLayer.UseContentColor := True;
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Stroke.assign(Stroke);
    StateStyles.Focused.Stroke.inherit := False;
    StateStyles.Focused.Stroke.Color := $FFD0BCFF;  // md.sys.color.primary / md.ref.palette.primary80
    StateStyles.Focused.Transition.Duration := 0.2;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#398d84eb-fc8a-4c8a-bfb4-82d2e85dee4d
procedure ALApplyMaterial3DarkTextButtonTheme(const AButton: TALButton);
begin
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(12{Left}, 12{Top}, 12{Right}, 12{Bottom});
    XRadius := -50;
    YRadius := -50;
    Fill.Color := Talphacolors.Null;
    Stroke.Color := Talphacolors.Null;
    TextSettings.Font.Color := $FFD0BCFF; // md.sys.color.primary // md.ref.palette.primary80
    TextSettings.LetterSpacing := 0.1;
    //--Disabled--
    StateStyles.Disabled.Opacity := 1;
    StateStyles.Disabled.TextSettings.Assign(TextSettings);
    StateStyles.Disabled.TextSettings.Inherit := False;
    StateStyles.Disabled.TextSettings.Font.Color := ALSetColorAlpha($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
    //--Hovered--
    StateStyles.Hovered.StateLayer.UseContentColor := True;
    StateStyles.Hovered.StateLayer.Opacity := 0.08;
    StateStyles.Hovered.Transition.Duration := 0.2;
    //--Pressed--
    StateStyles.Pressed.StateLayer.UseContentColor := True;
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //--Focused--
    StateStyles.Focused.StateLayer.UseContentColor := True;
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#c75be779-5a59-4748-98d4-e47fc888d0b1
procedure ALApplyMaterial3DarkElevatedButtonTheme(const AButton: TALButton);
begin
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
    XRadius := -50;
    YRadius := -50;
    Fill.Color := $FF1D1B20; // md.sys.color.surface-container-low / md.ref.palette.neutral10
    Stroke.Color := Talphacolors.Null;
    TextSettings.Font.Color := $FFD0BCFF; // md.sys.color.primary // md.ref.palette.primary80
    TextSettings.LetterSpacing := 0.1;
    Shadow.Color := ALSetColorAlpha($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
    Shadow.blur := 2;
    Shadow.OffsetY := 1;
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
    StateStyles.Hovered.Shadow.blur := 3;
    StateStyles.Hovered.Shadow.OffsetY := 1;
    StateStyles.Hovered.Transition.Duration := 0.2;
    //--Pressed--
    StateStyles.Pressed.StateLayer.UseContentColor := True;
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //--Focused--
    StateStyles.Focused.StateLayer.UseContentColor := True;
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#6ce8b926-87c4-4600-9bec-5deb4aaa65d8
procedure ALApplyMaterial3DarkTonalButtonTheme(const AButton: TALButton);
begin
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
    XRadius := -50;
    YRadius := -50;
    Fill.Color := $FF4A4458; // md.sys.color.secondary-container / md.ref.palette.secondary30
    Stroke.Color := Talphacolors.Null;
    TextSettings.Font.Color := $FFE8DEF8; // md.sys.color.on-secondary-container // md.ref.palette.secondary90
    TextSettings.LetterSpacing := 0.1;
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
    StateStyles.Hovered.Shadow.blur := 2;
    StateStyles.Hovered.Shadow.OffsetY := 1;
    StateStyles.Hovered.Transition.Duration := 0.2;
    //--Pressed--
    StateStyles.Pressed.StateLayer.UseContentColor := True;
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //--Focused--
    StateStyles.Focused.StateLayer.UseContentColor := True;
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#5309610a-4515-44f9-830d-880e2a2240a2
procedure ALApplyMaterial3DarkFilledIconButtonTheme(const AButton: TALButton);
begin
  var LPrevResourceName := Abutton.Fill.ResourceName;
  var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    Width := 40;
    Height := 40;
    TouchTargetExpansion.Rect := TRectf.Create(4,4,4,4);
    XRadius := -50;
    YRadius := -50;
    Fill.ImageMargins.Rect := TRectF.Create(8,8,8,8);
    Fill.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
    Fill.ResourceName := LPrevResourceName;
    Stroke.Color := TALphaColors.Null;
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
    StateStyles.Hovered.Transition.Duration := 0.2;
    //Filled icon button hover icon color: $FF381E72
    //--Pressed--
    StateStyles.Pressed.StateLayer.Color := $FF381E72; // md.sys.color.on-primary / md.ref.palette.primary20
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //Filled icon button pressed icon color: $FF381E72
    //--Focused--
    StateStyles.Focused.StateLayer.Color := $FF381E72; // md.sys.color.on-primary / md.ref.palette.primary20
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
    //Filled icon button focus icon color: $FF381E72
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#ba97cf8a-2112-47dc-af87-2e32aabccdde
procedure ALApplyMaterial3DarkTonalIconButtonTheme(const AButton: TALButton);
begin
  var LPrevResourceName := Abutton.Fill.ResourceName;
  var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    Width := 40;
    Height := 40;
    TouchTargetExpansion.Rect := TRectf.Create(4,4,4,4);
    XRadius := -50;
    YRadius := -50;
    Fill.ImageMargins.Rect := TRectF.Create(8,8,8,8);
    Fill.Color := $FF4A4458; // md.sys.color.secondary-container / md.ref.palette.secondary30
    Fill.ResourceName := LPrevResourceName;
    Stroke.Color := TALphaColors.Null;
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
    StateStyles.Hovered.Transition.Duration := 0.2;
    //Filled tonal icon button hover icon color: $FFE8DEF8
    //--Pressed--
    StateStyles.Pressed.StateLayer.Color := $FFE8DEF8; // md.sys.color.on-secondary-container / md.ref.palette.secondary90
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //Filled tonal icon button pressed icon color: $FFE8DEF8
    //--Focused--
    StateStyles.Focused.StateLayer.Color := $FFE8DEF8; // md.sys.color.on-secondary-container / md.ref.palette.secondary90
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
    //Filled tonal icon button focus icon color: $FFE8DEF8
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#05e02b7f-ebf2-4f02-9709-8230db3702b4
procedure ALApplyMaterial3DarkOutlinedIconButtonTheme(const AButton: TALButton);
begin
  var LPrevResourceName := Abutton.Fill.ResourceName;
  var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    Width := 40;
    Height := 40;
    TouchTargetExpansion.Rect := TRectf.Create(4,4,4,4);
    XRadius := -50;
    YRadius := -50;
    Fill.ImageMargins.Rect := TRectF.Create(8,8,8,8);
    Fill.Color := TalphaColorRec.Null;
    Fill.ResourceName := LPrevResourceName;
    Stroke.Color := $FF938F99; // md.sys.color.outline / md.ref.palette.neutral-variant60
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
    StateStyles.Hovered.Transition.Duration := 0.2;
    //Outlined icon button unselected hover icon color: $FFCAC4D0
    //--Pressed--
    StateStyles.Pressed.StateLayer.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //Outlined icon button unselected pressed icon color: $FFCAC4D0
    //--Focused--
    StateStyles.Focused.StateLayer.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
    //Outlined icon button focus icon color: $FFCAC4D0
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#e63a9b45-a20c-402c-8cc5-2c67ad8aae25
procedure ALApplyMaterial3DarkStandardIconButtonTheme(const AButton: TALButton);
begin
  var LPrevResourceName := Abutton.Fill.ResourceName;
  var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
  ALResetButtonTheme(AButton);
  With AButton do begin
    //--Enabled (default)--
    Width := 40;
    Height := 40;
    TouchTargetExpansion.Rect := TRectf.Create(4,4,4,4);
    XRadius := -50;
    YRadius := -50;
    Fill.ImageMargins.Rect := TRectF.Create(8,8,8,8);
    Fill.ResourceName := LPrevResourceName;
    Fill.Color := TalphaColors.Null;
    Stroke.Color := TALphaColors.Null;
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
    StateStyles.Hovered.Transition.Duration := 0.2;
    //Icon button unselected hover icon color: $FFCAC4D0
    //--Pressed--
    StateStyles.Pressed.StateLayer.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
    StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Pressed.Transition.Duration := 0.2;
    //Icon button unselected pressed icon color: $FFCAC4D0
    //--Focused--
    StateStyles.Focused.StateLayer.Color := $FFCAC4D0; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant80
    StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    StateStyles.Focused.Transition.Duration := 0.2;
    //Icon button unselected focus icon color: $FFCAC4D0
  end;
end;

{***************************************************************************}
procedure ALApplyButtonTheme(const ATheme: String; const AButton: TALButton);
begin
  Var LApplyButtonThemeProc: TALApplybuttonThemeProc;
  If not ALButtonThemes.TryGetValue(Atheme,LApplyButtonThemeProc) then
    Raise Exception.Createfmt('The theme "%s" could not be found', [ATheme]);
  AButton.BeginUpdate;
  try
    LApplyButtonThemeProc(AButton);
  finally
    AButton.EndUpdate;
  end;
end;

//////////////
// CHECKBOX //
//////////////

{***********************************************************}
procedure ALResetCheckBoxTheme(const ACheckBox: TALCheckBox);
begin

end;

{***************************************************************************}
procedure ALApplyCheckBoxTheme(const ATheme: String; const ACheckBox: TALCheckBox);
begin
end;

/////////////////
// RADIOBUTTON //
/////////////////

{*****************************************************************}
procedure ALResetRadioButtonTheme(const ARadioButton: TALCheckBox);
begin

end;

{***************************************************************************************}
procedure ALApplyRadioButtonTheme(const ATheme: String; const ARadioButton: TALCheckBox);
begin
end;

initialization
  ALEditThemes := TDictionary<String, TALApplyEditThemeProc>.Create;
  ALMemoThemes := TDictionary<String, TALApplyEditThemeProc>.Create;
  ALButtonThemes := TDictionary<String, TALApplyButtonThemeProc>.Create;
  ALCheckBoxThemes := TDictionary<String, TALApplyCheckBoxThemeProc>.Create;
  ALRadioButtonThemes := TDictionary<String, TALApplyCheckBoxThemeProc>.Create;

  ALEditThemes.Add('Default', ALResetEditTheme);
  ALEditThemes.Add('Material3.Light.Filled', ALApplyMaterial3LightFilledEditTheme);
  ALEditThemes.Add('Material3.Light.Filled.Error', ALApplyMaterial3LightFilledErrorEditTheme);
  ALEditThemes.Add('Material3.Light.Outlined', ALApplyMaterial3LightOutlinedEditTheme);
  ALEditThemes.Add('Material3.Light.Outlined.Error', ALApplyMaterial3LightOutlinedErrorEditTheme);
  ALEditThemes.Add('Material3.Dark.Filled', ALApplyMaterial3DarkFilledEditTheme);
  ALEditThemes.Add('Material3.Dark.Filled.Error', ALApplyMaterial3DarkFilledErrorEditTheme);
  ALEditThemes.Add('Material3.Dark.Outlined', ALApplyMaterial3DarkOutlinedEditTheme);
  ALEditThemes.Add('Material3.Dark.Outlined.Error', ALApplyMaterial3DarkOutlinedErrorEditTheme);
  ALEditThemes.Add('Facebook.Outlined', ALApplyFacebookOutlinedEditTheme);
  ALEditThemes.Add('Facebook.Outlined.Error', ALApplyFacebookOutlinedErrorEditTheme);

  ALMemoThemes.Add('Default', ALResetEditTheme);
  ALMemoThemes.Add('Material3.Light.Filled', ALApplyMaterial3LightFilledEditTheme);
  ALMemoThemes.Add('Material3.Light.Filled.Error', ALApplyMaterial3LightFilledErrorEditTheme);
  ALMemoThemes.Add('Material3.Light.Outlined', ALApplyMaterial3LightOutlinedEditTheme);
  ALMemoThemes.Add('Material3.Light.Outlined.Error', ALApplyMaterial3LightOutlinedErrorEditTheme);
  ALMemoThemes.Add('Material3.Dark.Filled', ALApplyMaterial3DarkFilledEditTheme);
  ALMemoThemes.Add('Material3.Dark.Filled.Error', ALApplyMaterial3DarkFilledErrorEditTheme);
  ALMemoThemes.Add('Material3.Dark.Outlined', ALApplyMaterial3DarkOutlinedEditTheme);
  ALMemoThemes.Add('Material3.Dark.Outlined.Error', ALApplyMaterial3DarkOutlinedErrorEditTheme);
  ALMemoThemes.Add('Facebook.Outlined', ALApplyFacebookOutlinedEditTheme);
  ALMemoThemes.Add('Facebook.Outlined.Error', ALApplyFacebookOutlinedErrorEditTheme);

  ALButtonThemes.Add('Default', ALResetButtonTheme);
  ALButtonThemes.Add('Windows', ALApplyWindowsButtonTheme);
  ALButtonThemes.Add('Material3.Light.Filled', ALApplyMaterial3LightFilledButtonTheme);
  ALButtonThemes.Add('Material3.Light.Outlined', ALApplyMaterial3LightOutlinedButtonTheme);
  ALButtonThemes.Add('Material3.Light.Text', ALApplyMaterial3LightTextButtonTheme);
  ALButtonThemes.Add('Material3.Light.Elevated', ALApplyMaterial3LightElevatedButtonTheme);
  ALButtonThemes.Add('Material3.Light.Tonal', ALApplyMaterial3LightTonalButtonTheme);
  ALButtonThemes.Add('Material3.Light.Icon.Filled', ALApplyMaterial3LightFilledIconButtonTheme);
  ALButtonThemes.Add('Material3.Light.Icon.Outlined', ALApplyMaterial3LightOutlinedIconButtonTheme);
  ALButtonThemes.Add('Material3.Light.Icon.Standard', ALApplyMaterial3LightStandardIconButtonTheme);
  ALButtonThemes.Add('Material3.Light.Icon.Tonal', ALApplyMaterial3LightTonalIconButtonTheme);
  ALButtonThemes.Add('Material3.Dark.Filled', ALApplyMaterial3DarkFilledButtonTheme);
  ALButtonThemes.Add('Material3.Dark.Outlined', ALApplyMaterial3DarkOutlinedButtonTheme);
  ALButtonThemes.Add('Material3.Dark.Text', ALApplyMaterial3DarkTextButtonTheme);
  ALButtonThemes.Add('Material3.Dark.Elevated', ALApplyMaterial3DarkElevatedButtonTheme);
  ALButtonThemes.Add('Material3.Dark.Tonal', ALApplyMaterial3DarkTonalButtonTheme);
  ALButtonThemes.Add('Material3.Dark.Icon.Filled', ALApplyMaterial3DarkFilledIconButtonTheme);
  ALButtonThemes.Add('Material3.Dark.Icon.Outlined', ALApplyMaterial3DarkOutlinedIconButtonTheme);
  ALButtonThemes.Add('Material3.Dark.Icon.Standard', ALApplyMaterial3DarkStandardIconButtonTheme);
  ALButtonThemes.Add('Material3.Dark.Icon.Tonal', ALApplyMaterial3DarkTonalIconButtonTheme);

finalization
  ALFreeAndNil(ALEditThemes);
  ALFreeAndNil(ALMemoThemes);
  ALFreeAndNil(ALButtonThemes);
  ALFreeAndNil(ALCheckBoxThemes);
  ALFreeAndNil(ALRadioButtonThemes);

end.
