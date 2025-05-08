unit Alcinoe.FMX.Styles;

interface

{$I Alcinoe.inc}

uses
  System.Generics.Collections,
  System.UITypes,
  FMX.Controls,
  Fmx.forms,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.Edit,
  Alcinoe.FMX.StdCtrls;

Type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALStyleManager = class(Tobject)
  private
    class function CreateInstance: TALStyleManager;
    class function GetInstance: TALStyleManager; static;
  protected
    class var FInstance: TALStyleManager;
  public
    type
      TCreateInstanceFunc = function: TALStyleManager;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALStyleManager read GetInstance;
    class function HasInstance: Boolean; inline;
  private
    class var FSortOrderCounter: integer;
  protected
    class function GetNextSortOrder: integer;
  public
    type
      // -----------------
      // TDarkModeBehavior
      TDarkModeBehavior = (FollowSystem, AlwaysDark, AlwaysLight);
      // -------------------
      // TTextStyleInfo
      TTextApplyStyleProc = Procedure(const AText: TALText; const AFontSize: Single);
      TTextStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TTextApplyStyleProc;
        DefaultFontSize: Single;
        constructor create(const AApplyStyleProc: TTextApplyStyleProc; const ADefaultFontSize: Single);
      end;
      // -------------------
      // TEditStyleInfo
      TEditApplyStyleProc = Procedure(const AEdit: TALBaseEdit; const AFontSize: Single);
      TEditStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TEditApplyStyleProc;
        DefaultFontSize: Single;
        constructor create(const AApplyStyleProc: TEditApplyStyleProc; const ADefaultFontSize: Single);
      end;
      // ---------------------
      // TButtonStyleInfo
      TButtonApplyStyleProc = Procedure(const AButton: TALButton; const AFontSize: Single);
      TButtonStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TButtonApplyStyleProc;
        DefaultFontSize: Single;
        constructor create(const AApplyStyleProc: TButtonApplyStyleProc; const ADefaultFontSize: Single);
      end;
      // -----------------------
      // TCheckBoxStyleInfo
      TCheckBoxApplyStyleProc = Procedure(const ACheckBox: TALCheckBox; const AHeight: Single);
      TCheckBoxStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TCheckBoxApplyStyleProc;
        DefaultHeight: Single;
        constructor create(const AApplyStyleProc: TCheckBoxApplyStyleProc; const ADefaultHeight: Single);
      end;
      // --------------------------
      // TRadioButtonStyleInfo
      TRadioButtonApplyStyleProc = Procedure(const ARadioButton: TALRadioButton; const AHeight: Single);
      TRadioButtonStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TRadioButtonApplyStyleProc;
        DefaultHeight: Single;
        constructor create(const AApplyStyleProc: TRadioButtonApplyStyleProc; const ADefaultHeight: Single);
      end;
      // ---------------------
      // TSwitchStyleInfo
      TSwitchApplyStyleProc = Procedure(const ASwitch: TALSwitch; const AHeight: Single);
      TSwitchStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TSwitchApplyStyleProc;
        DefaultHeight: Single;
        constructor create(const AApplyStyleProc: TSwitchApplyStyleProc; const ADefaultHeight: Single);
      end;
      // -----------------------
      // TTrackBarStyleInfo
      TTrackBarApplyStyleProc = Procedure(const ATrackBar: TALCustomTrack; const AHeight: Single);
      TTrackBarStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TTrackBarApplyStyleProc;
        DefaultHeight: Single;
        constructor create(const AApplyStyleProc: TTrackBarApplyStyleProc; const ADefaultHeight: Single);
      end;
  private
    FLightColors: TDictionary<string, TPair<TAlphaColor, integer{SortOrder}>>;
    FDarkColors: TDictionary<string, TPair<TAlphaColor, integer{SortOrder}>>;
    FColors: TDictionary<string, TAlphaColor>;
    FTextStyles: TDictionary<String, TTextStyleInfo>;
    FEditStyles: TDictionary<String, TEditStyleInfo>;
    FMemoStyles: TDictionary<String, TEditStyleInfo>;
    FButtonStyles: TDictionary<String, TButtonStyleInfo>;
    FCheckBoxStyles: TDictionary<String, TCheckBoxStyleInfo>;
    FRadioButtonStyles: TDictionary<String, TRadioButtonStyleInfo>;
    FSwitchStyles: TDictionary<String, TSwitchStyleInfo>;
    FTrackBarStyles: TDictionary<String, TTrackBarStyleInfo>;
    FRangeTrackBarStyles: TDictionary<String, TTrackBarStyleInfo>;
    FIsDarkMode: Boolean;
    function GetSystemIsDarkMode: Boolean;
    function GetDarkModeBehavior: TDarkModeBehavior;
    procedure SetDarkModeBehavior(const AValue: TDarkModeBehavior);
  protected
    procedure InitStyles; virtual;
    procedure InitColors; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property DarkModeBehavior: TDarkModeBehavior read GetDarkModeBehavior write SetDarkModeBehavior;
    property IsDarkMode: Boolean read FIsDarkMode;
    procedure ApplyColorScheme(const AForm: TCustomForm; const AFormFillColorName: String); virtual;
    //--
    procedure AddOrSetColor(const AName: String; Const AValue: TAlphaColor; Const AIsForDarkMode: Boolean);
    procedure AddOrSetTextStyle(const AName: String; const AApplyStyleProc: TTextApplyStyleProc; const ADefaultFontSize: Single);
    procedure AddOrSetEditStyle(const AName: String; const AApplyStyleProc: TEditApplyStyleProc; const ADefaultFontSize: Single);
    procedure AddOrSetMemoStyle(const AName: String; const AApplyStyleProc: TEditApplyStyleProc; const ADefaultFontSize: Single);
    procedure AddOrSetButtonStyle(const AName: String; const AApplyStyleProc: TButtonApplyStyleProc; const ADefaultFontSize: Single);
    procedure AddOrSetCheckBoxStyle(const AName: String; const AApplyStyleProc: TCheckBoxApplyStyleProc; const ADefaultHeight: Single);
    procedure AddOrSetRadioButtonStyle(const AName: String; const AApplyStyleProc: TRadioButtonApplyStyleProc; const ADefaultHeight: Single);
    procedure AddOrSetSwitchStyle(const AName: String; const AApplyStyleProc: TSwitchApplyStyleProc; const ADefaultHeight: Single);
    procedure AddOrSetTrackBarStyle(const AName: String; const AApplyStyleProc: TTrackBarApplyStyleProc; const ADefaultHeight: Single);
    procedure AddOrSetRangeTrackBarStyle(const AName: String; const AApplyStyleProc: TTrackBarApplyStyleProc; const ADefaultHeight: Single);
    //--
    function GetColor(const AName: String): TAlphaColor;
    procedure ApplyTextStyle(const AName: String; const AText: TALText);
    procedure ApplyEditStyle(const AName: String; const AEdit: TALBaseEdit);
    procedure ApplyMemoStyle(const AName: String; const AMemo: TALBaseEdit);
    procedure ApplyButtonStyle(const AName: String; const AButton: TALButton);
    procedure ApplyCheckBoxStyle(const AName: String; const ACheckBox: TALCheckBox);
    procedure ApplyRadioButtonStyle(const AName: String; const ARadioButton: TALRadioButton);
    procedure ApplySwitchStyle(const AName: String; const ASwitch: TALSwitch);
    procedure ApplyTrackBarStyle(const AName: String; const ATrackBar: TALCustomTrack);
    procedure ApplyRangeTrackBarStyle(const AName: String; const ARangeTrackBar: TALCustomTrack);
    //--
    function GetColorNames: TArray<String>;
    function GetTextStyleNames: TArray<String>;
    function GetEditStyleNames: TArray<String>;
    function GetMemoStyleNames: TArray<String>;
    function GetButtonStyleNames: TArray<String>;
    function GetCheckBoxStyleNames: TArray<String>;
    function GetRadioButtonStyleNames: TArray<String>;
    function GetSwitchStyleNames: TArray<String>;
    function GetTrackBarStyleNames: TArray<String>;
    function GetRangeTrackBarStyleNames: TArray<String>;
  end;

implementation

uses
  System.Generics.Defaults,
  System.SysUtils,
  System.Types,
  System.Math,
  FMX.Platform,
  FMX.types,
  {$IF defined(ANDROID)}
  Androidapi.Helpers,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Alcinoe.AndroidApi.App,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.Helpers,
  iOSapi.Helpers,
  iOSapi.UIKit,
  Alcinoe.iOSapi.UIKit,
  {$ENDIF}
  {$IF defined(ALDPK)}
  Vcl.Dialogs,
  {$ENDIF}
  Alcinoe.Common,
  Alcinoe.StringUtils,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Memo,
  Alcinoe.FMX.UserPreferences,
  Alcinoe.FMX.Graphics;

type
  TALControlAccessProtected = Class(TALControl);

{****************************************************************************************}
function ALGetStyleSizeRatio(const ACaption: String; const ADefaultValue: Single): Single;
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

//////////
// TEXT //
//////////

{*****************************************************************************}
procedure ALResetTextStyle(const AText: TALText; const AFontSize: Single = 16);
begin
  With AText do begin
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
    Finally
      EndUpdate;
    End;
  end;
end;

//////////
// EDIT //
//////////

{*********************************************************************************}
procedure ALResetEditStyle(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
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
      TintColorKey := '';
      PromptTextcolor := TAlphaColors.null;
      PromptTextcolorKey := '';
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
procedure ALApplyMaterial3EditFilledStyle(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditStyle(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 12*LRatio{Top}, 16*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      Corners := [TCorner.TopLeft, Tcorner.TopRight];
      Sides := [TSide.Bottom];
      XRadius := RoundTo(4 * LRatio, -2);
      YRadius := RoundTo(4 * LRatio, -2);
      DefStyleAttr := 'material3EditFilled';
      TintColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Fill.ColorKey := 'Material3.Color.SurfaceContainerHighest'; // md.sys.color.surface-container-highest / md.ref.palette.neutral90 / #E6E0E9
      Stroke.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      TextSettings.Font.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
      LabelTextSettings.Font.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      LabelTextSettings.Margins.Rect := TRectF.Create(0,-4*LRatio,0,4*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material3.Color.OnSurface.Alpha04'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.PromptTextcolorKey := StateStyles.Disabled.LabelTextSettings.Font.ColorKey;
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * LRatio, -2);
      StateStyles.Focused.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Focused.LabelTextSettings.Inherit := False;
      StateStyles.Focused.LabelTextSettings.Font.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Focused.PromptTextcolorKey := StateStyles.Focused.LabelTextSettings.Font.ColorKey;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#e4964192-72ad-414f-85b4-4b4357abb83c
procedure ALApplyMaterial3EditOutlinedStyle(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditStyle(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 16*LRatio{Top}, 16*LRatio{Right}, 16*LRatio{Bottom}).RoundTo(-2);
      XRadius := RoundTo(4 * LRatio, -2);
      YRadius := RoundTo(4 * LRatio, -2);
      DefStyleAttr := 'material3EditOutlined';
      TintColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Fill.ColorKey := 'Material3.Color.Surface'; // md.sys.color.surface / md.ref.palette.neutral98 / #FEF7FF
      Stroke.ColorKey := 'Material3.Color.Outline'; // md.sys.color.outline / md.ref.palette.neutral-variant50 / #79747E
      TextSettings.Font.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.floating;
      LabelTextSettings.Font.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      LabelTextSettings.Margins.Rect := TRectF.Create(0,0,0,-6*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.PromptTextcolorKey := StateStyles.Disabled.LabelTextSettings.Font.ColorKey;
      //--Hovered--
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Hovered.PromptTextcolorKey := StateStyles.Hovered.LabelTextSettings.Font.ColorKey;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * LRatio, -2);
      StateStyles.Focused.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Focused.LabelTextSettings.Inherit := False;
      StateStyles.Focused.LabelTextSettings.Font.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Focused.PromptTextcolorKey := StateStyles.Focused.LabelTextSettings.Font.ColorKey;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterial3EditHybridStyle(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditStyle(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 16*LRatio{Top}, 16*LRatio{Right}, 16*LRatio{Bottom}).RoundTo(-2);
      XRadius := RoundTo(4 * LRatio, -2);
      YRadius := RoundTo(4 * LRatio, -2);
      DefStyleAttr := 'material3EditOutlined';
      TintColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Fill.ColorKey := 'Material3.Color.Surface'; // md.sys.color.surface / md.ref.palette.neutral98 / #FEF7FF
      Stroke.ColorKey := 'Material3.Color.Outline'; // md.sys.color.outline / md.ref.palette.neutral-variant50 / #79747E
      TextSettings.Font.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
      LabelTextSettings.Font.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      LabelTextSettings.Margins.Rect := TRectF.Create(0,-4*LRatio,0,4*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.PromptTextcolorKey := StateStyles.Disabled.LabelTextSettings.Font.ColorKey;
      //--Hovered--
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Hovered.PromptTextcolorKey := StateStyles.Hovered.LabelTextSettings.Font.ColorKey;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * LRatio, -2);
      StateStyles.Focused.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Focused.LabelTextSettings.Inherit := False;
      StateStyles.Focused.LabelTextSettings.Font.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Focused.PromptTextcolorKey := StateStyles.Focused.LabelTextSettings.Font.ColorKey;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterial3EditFilledErrorStyle(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditStyle(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 12*LRatio{Top}, 16*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      Corners := [TCorner.TopLeft, Tcorner.TopRight];
      Sides := [TSide.Bottom];
      XRadius := RoundTo(4 * LRatio, -2);
      YRadius := RoundTo(4 * LRatio, -2);
      DefStyleAttr := 'material3EditFilledError';
      TintColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      Fill.ColorKey := 'Material3.Color.SurfaceContainerHighest'; // md.sys.color.surface-container-highest / md.ref.palette.neutral90 / #E6E0E9
      Stroke.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      TextSettings.Font.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
      LabelTextSettings.Font.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      LabelTextSettings.Margins.Rect := TRectF.Create(0,-4*LRatio,0,4*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material3.Color.OnSurface.Alpha04'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.PromptTextcolorKey := StateStyles.Disabled.LabelTextSettings.Font.ColorKey;
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.ColorKey := 'Material3.Color.OnErrorContainer'; // md.sys.color.on-error-container / md.ref.palette.error30 / #8C1D18
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.ColorKey := 'Material3.Color.OnErrorContainer'; // md.sys.color.on-error-container / md.ref.palette.error30 / #8C1D18
      StateStyles.Hovered.PromptTextcolorKey := StateStyles.Hovered.LabelTextSettings.Font.ColorKey;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * LRatio, -2);
      StateStyles.Focused.PromptTextcolorKey := LabelTextSettings.Font.ColorKey;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#e4964192-72ad-414f-85b4-4b4357abb83c
procedure ALApplyMaterial3EditOutlinedErrorStyle(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditStyle(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 16*LRatio{Top}, 16*LRatio{Right}, 16*LRatio{Bottom}).RoundTo(-2);
      XRadius := RoundTo(4 * LRatio, -2);
      YRadius := RoundTo(4 * LRatio, -2);
      DefStyleAttr := 'material3EditOutlinedError';
      TintColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      Fill.ColorKey := 'Material3.Color.Surface'; // md.sys.color.surface / md.ref.palette.neutral98 / #FEF7FF
      Stroke.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      TextSettings.Font.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.floating;
      LabelTextSettings.Font.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      LabelTextSettings.Margins.Rect := TRectF.Create(0,0,0,-6*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.PromptTextcolorKey := StateStyles.Disabled.LabelTextSettings.Font.ColorKey;
      //--Hovered--
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.ColorKey := 'Material3.Color.OnErrorContainer'; // md.sys.color.on-error-container / md.ref.palette.error30 / #8C1D18
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.ColorKey := 'Material3.Color.OnErrorContainer'; // md.sys.color.on-error-container / md.ref.palette.error30 / #8C1D18
      StateStyles.Hovered.PromptTextcolorKey := StateStyles.Hovered.LabelTextSettings.Font.ColorKey;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * LRatio, -2);
      StateStyles.Focused.PromptTextcolorKey := LabelTextSettings.Font.ColorKey;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterial3EditHybridErrorStyle(const AEdit: TALBaseEdit; const AFontSize: Single = 16);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired font size for the edit', AFontSize);
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditStyle(AEdit, AFontSize * LRatio);
      padding.Rect := TRectF.Create(16*LRatio{Left}, 16*LRatio{Top}, 16*LRatio{Right}, 16*LRatio{Bottom}).RoundTo(-2);
      XRadius := RoundTo(4 * LRatio, -2);
      YRadius := RoundTo(4 * LRatio, -2);
      DefStyleAttr := 'material3EditOutlinedError';
      TintColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      Fill.ColorKey := 'Material3.Color.Surface'; // md.sys.color.surface / md.ref.palette.neutral98 / #FEF7FF
      Stroke.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      TextSettings.Font.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
      LabelTextSettings.Font.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      LabelTextSettings.Margins.Rect := TRectF.Create(0,-4*LRatio,0,4*LRatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*LRatio,0,0).RoundTo(-2);
      PromptTextColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.PromptTextcolorKey := StateStyles.Disabled.LabelTextSettings.Font.ColorKey;
      //--Hovered--
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.ColorKey := 'Material3.Color.OnErrorContainer'; // md.sys.color.on-error-container / md.ref.palette.error30 / #8C1D18
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.ColorKey := 'Material3.Color.OnErrorContainer'; // md.sys.color.on-error-container / md.ref.palette.error30 / #8C1D18
      StateStyles.Hovered.PromptTextcolorKey := StateStyles.Hovered.LabelTextSettings.Font.ColorKey;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * LRatio, -2);
      StateStyles.Focused.PromptTextcolorKey := LabelTextSettings.Font.ColorKey;

    finally
      EndUpdate;
    end;
  end;
end;

////////////
// BUTTON //
////////////

{***********************************************************************************}
procedure ALResetButtonStyle(const AButton: TALButton; const AFontSize: Single = 14);
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

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#cbfd91a6-d688-4be7-9a69-672549de3ea9
procedure ALApplyMaterial3ButtonFilledStyle(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonStyle(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(24*LRatio{Left}, 12*LRatio{Top}, 24*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Stroke.Color := Talphacolors.Null;
      Stroke.ColorKey := '';
      TextSettings.Font.ColorKey := 'Material3.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.ColorKey := 'Material3.Color.Shadow.Alpha50'; // md.sys.color.shadow / md.ref.palette.neutral0 / #000000
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
procedure ALApplyMaterial3ButtonOutlinedStyle(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonStyle(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(24*LRatio{Left}, 12*LRatio{Top}, 24*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.Color := Talphacolors.Null;
      Fill.ColorKey := '';
      Stroke.ColorKey := 'Material3.Color.Outline'; // md.sys.color.outline / md.ref.palette.neutral-variant50 / #79747E
      TextSettings.Font.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.Assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
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
      StateStyles.Focused.Stroke.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#398d84eb-fc8a-4c8a-bfb4-82d2e85dee4d
procedure ALApplyMaterial3ButtonTextStyle(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonStyle(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(12*LRatio{Left}, 12*LRatio{Top}, 12*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.Color := Talphacolors.Null;
      Fill.ColorKey := '';
      Stroke.Color := Talphacolors.Null;
      Stroke.ColorKey := '';
      TextSettings.Font.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
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
procedure ALApplyMaterial3ButtonElevatedStyle(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonStyle(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(24*LRatio{Left}, 12*LRatio{Top}, 24*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.ColorKey := 'Material3.Color.SurfaceContainerLow'; // md.sys.color.surface-container-low / md.ref.palette.neutral96 / #F7F2FA
      Stroke.Color := Talphacolors.Null;
      Stroke.ColorKey := '';
      TextSettings.Font.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      Shadow.ColorKey := 'Material3.Color.Shadow.Alpha50'; // md.sys.color.shadow / md.ref.palette.neutral0 / #000000
      Shadow.blur := RoundTo(2 * LRatio, -2);
      Shadow.OffsetY := RoundTo(1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Shadow.inherit := False;
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.ColorKey := 'Material3.Color.Shadow.Alpha50'; // md.sys.color.shadow / md.ref.palette.neutral0 / #000000
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
procedure ALApplyMaterial3ButtonTonalStyle(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired font size for the button', AFontSize);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonStyle(AButton, AFontSize * LRatio);
      padding.Rect := TRectF.Create(24*LRatio{Left}, 12*LRatio{Top}, 24*LRatio{Right}, 12*LRatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.ColorKey := 'Material3.Color.SecondaryContainer'; // md.sys.color.secondary-container / md.ref.palette.secondary90 / #E8DEF8
      Stroke.Color := Talphacolors.Null;
      Stroke.ColorKey := '';
      TextSettings.Font.ColorKey := 'Material3.Color.OnSecondaryContainer'; // md.sys.color.on-secondary-container / md.ref.palette.secondary30 / #4A4458
      TextSettings.LetterSpacing := RoundTo(0.1 * LRatio, -2);
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.ColorKey := 'Material3.Color.Shadow.Alpha50'; // md.sys.color.shadow / md.ref.palette.neutral0 / #000000
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
procedure ALApplyMaterial3ButtonIconFilledStyle(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired height for the button', 40);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonStyle(AButton);
      Width := RoundTo(40 * LRatio, -2);
      Height := RoundTo(40 * LRatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*LRatio,8*LRatio,8*LRatio,8*LRatio).RoundTo(-2);
      Fill.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Fill.ResourceName := LPrevResourceName;
      Stroke.Color := TALphaColors.Null;
      Stroke.ColorKey := '';
      StateStyles.Transition.Duration := 0.2;
      //Filled icon button icon color: $FFFFFFFF
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      //Filled icon button disabled icon color: $FF1D1B20 with 0.38 opacity
      //--Hovered--
      StateStyles.Hovered.StateLayer.ColorKey := 'Material3.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //Filled icon button hover icon color: $FFFFFFFF
      //--Pressed--
      StateStyles.Pressed.StateLayer.ColorKey := 'Material3.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Filled icon button pressed icon color: $FFFFFFFF
      //--Focused--
      StateStyles.Focused.StateLayer.ColorKey := 'Material3.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Filled icon button focus icon color: $FFFFFFFF

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#ba97cf8a-2112-47dc-af87-2e32aabccdde
procedure ALApplyMaterial3ButtonIconTonalStyle(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired height for the button', 40);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonStyle(AButton);
      Width := RoundTo(40 * LRatio, -2);
      Height := RoundTo(40 * LRatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*LRatio,8*LRatio,8*LRatio,8*LRatio).RoundTo(-2);
      Fill.ColorKey := 'Material3.Color.SecondaryContainer'; // md.sys.color.secondary-container / md.ref.palette.secondary90 / #E8DEF8
      Fill.ResourceName := LPrevResourceName;
      Stroke.Color := TALphaColors.Null;
      Stroke.ColorKey := '';
      StateStyles.Transition.Duration := 0.2;
      //Filled tonal icon button icon color: $FF1D192B
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      //Filled tonal icon button disabled icon color: $FF1D1B20 with 0.38 opacity
      //--Hovered--
      StateStyles.Hovered.StateLayer.ColorKey := 'Material3.Color.OnSecondaryContainer'; // md.sys.color.on-secondary-container / md.ref.palette.secondary30 / #4A4458
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //Filled tonal icon button hover icon color: $FF1D192B
      //--Pressed--
      StateStyles.Pressed.StateLayer.ColorKey := 'Material3.Color.OnSecondaryContainer'; // md.sys.color.on-secondary-container / md.ref.palette.secondary30 / #4A4458
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Filled tonal icon button pressed icon color: $FF1D192B
      //--Focused--
      StateStyles.Focused.StateLayer.ColorKey := 'Material3.Color.OnSecondaryContainer'; // md.sys.color.on-secondary-container / md.ref.palette.secondary30 / #4A4458
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Filled tonal icon button focus icon color: $FF1D192B

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#05e02b7f-ebf2-4f02-9709-8230db3702b4
procedure ALApplyMaterial3ButtonIconOutlinedStyle(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired height for the button', 40);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonStyle(AButton);
      Width := RoundTo(40 * LRatio, -2);
      Height := RoundTo(40 * LRatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*LRatio,8*LRatio,8*LRatio,8*LRatio).RoundTo(-2);
      Fill.Color := TalphaColorRec.Null;
      Fill.ColorKey := '';
      Fill.ResourceName := LPrevResourceName;
      Stroke.ColorKey := 'Material3.Color.Outline'; // md.sys.color.outline / md.ref.palette.neutral-variant50 / #79747E
      StateStyles.Transition.Duration := 0.2;
      //Outlined icon button unselected icon color: $FF49454F
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      StateStyles.Disabled.Stroke.Assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //Outlined icon button disabled icon color: $FF1D1B20 with 0.38 opacity
      //--Hovered--
      StateStyles.Hovered.StateLayer.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //Outlined icon button unselected hover icon color: $FF49454F
      //--Pressed--
      StateStyles.Pressed.StateLayer.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Outlined icon button unselected pressed icon color: $FF49454F
      //--Focused--
      StateStyles.Focused.StateLayer.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Outlined icon button focus icon color: $FF49454F

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#e63a9b45-a20c-402c-8cc5-2c67ad8aae25
procedure ALApplyMaterial3ButtonIconStandardStyle(const AButton: TALButton; const AFontSize: Single = 14);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired height for the button', 40);
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonStyle(AButton);
      Width := RoundTo(40 * LRatio, -2);
      Height := RoundTo(40 * LRatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*LRatio,8*LRatio,8*LRatio,8*LRatio).RoundTo(-2);
      Fill.ResourceName := LPrevResourceName;
      Fill.Color := TalphaColors.Null;
      Fill.ColorKey := '';
      Stroke.Color := TALphaColors.Null;
      Stroke.ColorKey := '';
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
      StateStyles.Hovered.StateLayer.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //Icon button unselected hover icon color: $FF49454F
      //--Pressed--
      StateStyles.Pressed.StateLayer.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Icon button unselected pressed icon color: $FF49454F
      //--Focused--
      StateStyles.Focused.StateLayer.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //Icon button unselected focus icon color: $FF49454F

    finally
      EndUpdate;
    end;
  end;
end;

//////////////
// CHECKBOX //
//////////////

{***************************************************************************************}
procedure ALResetCheckBoxStyle(const ACheckBox: TALCheckBox; const AHeight: Single = 18);
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
procedure ALApplyMaterial3CheckBoxStyle(const ACheckBox: TALCheckBox; const AHeight: Single = 18);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired height for the checkbox', AHeight);
  With ACheckBox do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetCheckBoxStyle(ACheckBox, AHeight * LRatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := RoundTo(2 * LRatio, -2);
      YRadius := RoundTo(2 * LRatio, -2);
      Fill.Color := TalphaColors.Null;
      Fill.ColorKey := '';
      Stroke.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      Stroke.Thickness := RoundTo(2 * LRatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FF1D1B20 / md.sys.color.on-surface / md.ref.palette.neutral10
      CheckMark.ColorKey := ''; // $FF1D1B20 / md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.Fill.Assign(Fill);
      StateStyles.Checked.Default.Fill.Inherit := False;
      StateStyles.Checked.Default.Fill.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Default.Stroke.Assign(Stroke);
      StateStyles.Checked.Default.Stroke.Inherit := False;
      StateStyles.Checked.Default.Stroke.Thickness := 0;
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.ColorKey := 'Material3.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Fill.Assign(Fill);
      StateStyles.Checked.Disabled.Fill.Inherit := False;
      StateStyles.Checked.Disabled.Fill.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.ColorKey := 'Material3.Color.Surface'; // md.sys.color.surface / md.ref.palette.neutral98 / #FEF7FF

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

{*************************************************************************************}
//https://m3.material.io/components/checkbox/specs#fd29f662-6e61-4c1f-9b97-1145c3b33075
procedure ALApplyMaterial3CheckBoxErrorStyle(const ACheckBox: TALCheckBox; const AHeight: Single = 18);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired height for the checkbox', AHeight);
  With ACheckBox do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetCheckBoxStyle(ACheckBox, AHeight * LRatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := RoundTo(2 * LRatio, -2);
      YRadius := RoundTo(2 * LRatio, -2);
      Fill.Color := TalphaColors.Null;
      Fill.ColorKey := '';
      Stroke.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      Stroke.Thickness := RoundTo(2 * LRatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FFB3261E / md.sys.color.error / md.ref.palette.error40
      CheckMark.ColorKey := ''; // $FFB3261E / md.sys.color.error / md.ref.palette.error40
      StateStyles.Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.Fill.Assign(Fill);
      StateStyles.Checked.Default.Fill.Inherit := False;
      StateStyles.Checked.Default.Fill.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.Checked.Default.Stroke.Assign(Stroke);
      StateStyles.Checked.Default.Stroke.Inherit := False;
      StateStyles.Checked.Default.Stroke.Thickness := 0;
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.ColorKey := 'Material3.Color.OnError'; // md.sys.color.on-error / md.ref.palette.error100 / #FFFFFF

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Fill.Assign(Fill);
      StateStyles.Checked.Disabled.Fill.Inherit := False;
      StateStyles.Checked.Disabled.Fill.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.ColorKey := 'Material3.Color.Surface'; // md.sys.color.surface / md.ref.palette.neutral98 / #FEF7FF

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

/////////////////
// RADIOBUTTON //
/////////////////

{************************************************************************************************}
procedure ALResetRadioButtonStyle(const ARadioButton: TALRadioButton; const AHeight: Single = 20);
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
procedure ALApplyMaterial3RadioButtonStyle(const ARadioButton: TALRadioButton; const AHeight: Single = 20);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired height for the radio button', AHeight);
  With ARadioButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetRadioButtonStyle(ARadioButton, AHeight * LRatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      Fill.Color := TalphaColors.Null;
      Fill.ColorKey := '';
      Stroke.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      Stroke.Thickness := RoundTo(2 * LRatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FF1D1B20 / md.sys.color.on-surface / md.ref.palette.neutral10
      CheckMark.ColorKey := ''; // $FF1D1B20 / md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.Stroke.Assign(Stroke);
      StateStyles.Checked.Default.Stroke.Inherit := False;
      StateStyles.Checked.Default.Stroke.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Stroke.assign(Stroke);
      StateStyles.Checked.Disabled.Stroke.Inherit := False;
      StateStyles.Checked.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/radio-button/specs#4eca59b9-dfb5-4ca4-9c76-8e664fb02137
procedure ALApplyMaterial3RadioButtonErrorStyle(const ARadioButton: TALRadioButton; const AHeight: Single = 20);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired height for the radio button', AHeight);
  With ARadioButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetRadioButtonStyle(ARadioButton, AHeight * LRatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * LRatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      Fill.Color := TalphaColors.Null;
      Fill.ColorKey := '';
      Stroke.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      Stroke.Thickness := RoundTo(2 * LRatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FFB3261E / md.sys.color.error / md.ref.palette.error40
      CheckMark.ColorKey := ''; // $FFB3261E / md.sys.color.error / md.ref.palette.error40
      StateStyles.Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Stroke.assign(Stroke);
      StateStyles.Checked.Disabled.Stroke.Inherit := False;
      StateStyles.Checked.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.ColorKey := 'Material3.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

////////////
// SWITCH //
////////////

{*********************************************************************************}
procedure ALResetSwitchStyle(const ASwitch: TALSwitch; const AHeight: Single = 32);
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
procedure ALApplyMaterial3SwitchStyle(const ASwitch: TALSwitch; const AHeight: Single = 32);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired height for the switch', AHeight);
  With ASwitch do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetSwitchStyle(ASwitch, AHeight * LRatio);

      //--Default (UnChecked)--
      Track.StateStyles.UnChecked.Default.Stroke.Assign(Track.Stroke);
      Track.StateStyles.UnChecked.Default.Stroke.Inherit := False;
      Track.StateStyles.UnChecked.Default.Stroke.ColorKey := 'Material3.Color.Outline'; // md.sys.color.outline / md.ref.palette.neutral-variant50 / #79747E
      Track.StateStyles.UnChecked.Default.Stroke.Thickness := RoundTo(2 * LRatio, -2);
      Track.StateStyles.UnChecked.Default.fill.Assign(Track.fill);
      Track.StateStyles.UnChecked.Default.fill.Inherit := False;
      Track.StateStyles.UnChecked.Default.fill.ColorKey := 'Material3.Color.SurfaceContainerHighest'; // md.sys.color.surface-container-highest / md.ref.palette.neutral90 / #E6E0E9
      //--
      Thumb.StateStyles.UnChecked.Default.fill.Assign(Thumb.fill);
      Thumb.StateStyles.UnChecked.Default.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Default.fill.ColorKey := 'Material3.Color.Outline'; // md.sys.color.outline / md.ref.palette.neutral-variant50 / #79747E
      Thumb.StateStyles.UnChecked.Default.Fill.BackgroundMargins.Rect := TRectF.Create(4*LRatio,4*LRatio,4*LRatio,4*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Default.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.UnChecked.Default.CheckMark.Inherit := False;
      Thumb.StateStyles.UnChecked.Default.CheckMark.Color := TAlphacolors.Null; // TALStyleManager.Instance.GetColor('Material3.Color.SurfaceContainerHighest'; // md.sys.color.surface-container-highest / md.ref.palette.neutral90 / #E6E0E9
      Thumb.StateStyles.UnChecked.Default.CheckMark.ColorKey := ''; // TALStyleManager.Instance.GetColor('Material3.Color.SurfaceContainerHighest'; // md.sys.color.surface-container-highest / md.ref.palette.neutral90 / #E6E0E9
      //--Default (Checked)--
      Track.StateStyles.Checked.Default.fill.Assign(Track.fill);
      Track.StateStyles.Checked.Default.fill.Inherit := False;
      Track.StateStyles.Checked.Default.fill.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      //--
      Thumb.StateStyles.Checked.Default.fill.Assign(Thumb.fill);
      Thumb.StateStyles.Checked.Default.fill.Inherit := False;
      Thumb.StateStyles.Checked.Default.fill.ColorKey := 'Material3.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF
      Thumb.StateStyles.Checked.Default.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.Checked.Default.CheckMark.Inherit := False;
      Thumb.StateStyles.Checked.Default.CheckMark.ColorKey := 'Material3.Color.OnPrimaryContainer'; // md.sys.color.on-primary-container / md.ref.palette.primary30 / #4F378B

      //--Disabled (UnChecked)--
      Track.StateStyles.UnChecked.Disabled.Opacity := 1;
      Track.StateStyles.UnChecked.Disabled.Stroke.Assign(Track.Stroke);
      Track.StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      Track.StateStyles.UnChecked.Disabled.Stroke.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      Track.StateStyles.UnChecked.Disabled.Stroke.Thickness := RoundTo(2 * LRatio, -2);
      Track.StateStyles.UnChecked.Disabled.fill.Assign(Track.fill);
      Track.StateStyles.UnChecked.Disabled.fill.Inherit := False;
      Track.StateStyles.UnChecked.Disabled.fill.ColorKey := 'Material3.Color.SurfaceContainerHighest.Alpha12'; // md.sys.color.surface-container-highest / md.ref.palette.neutral90 / #E6E0E9
      //--
      Thumb.StateStyles.UnChecked.Disabled.Opacity := 1;
      Thumb.StateStyles.UnChecked.Disabled.fill.Assign(Thumb.fill);
      Thumb.StateStyles.UnChecked.Disabled.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Disabled.fill.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      Thumb.StateStyles.UnChecked.Disabled.Fill.BackgroundMargins.Rect := TRectF.Create(4*LRatio,4*LRatio,4*LRatio,4*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Disabled.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      Thumb.StateStyles.UnChecked.Disabled.CheckMark.ColorKey := 'Material3.Color.SurfaceContainerHighest.Alpha38'; // md.sys.color.surface-container-highest / md.ref.palette.neutral90 / #E6E0E9
      //--Disabled (Checked)--
      Track.StateStyles.Checked.Disabled.Opacity := 1;
      Track.StateStyles.Checked.Disabled.fill.Assign(Track.fill);
      Track.StateStyles.Checked.Disabled.fill.Inherit := False;
      Track.StateStyles.Checked.Disabled.fill.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--
      Thumb.StateStyles.Checked.Disabled.Opacity := 1;
      Thumb.StateStyles.Checked.Disabled.fill.Assign(Thumb.fill);
      Thumb.StateStyles.Checked.Disabled.fill.Inherit := False;
      Thumb.StateStyles.Checked.Disabled.fill.ColorKey := 'Material3.Color.Surface'; // md.sys.color.surface / md.ref.palette.neutral98 / #FEF7FF
      Thumb.StateStyles.Checked.Disabled.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      Thumb.StateStyles.Checked.Disabled.CheckMark.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20

      //--Hovered (UnChecked)--
      Thumb.StateStyles.UnChecked.Hovered.fill.Assign(Thumb.StateStyles.UnChecked.Default.fill);
      Thumb.StateStyles.UnChecked.Hovered.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Hovered.fill.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      Thumb.StateStyles.UnChecked.Hovered.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Hovered.StateLayer.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      Thumb.StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      Thumb.StateStyles.Checked.Hovered.fill.Assign(Thumb.StateStyles.Checked.Default.fill);
      Thumb.StateStyles.Checked.Hovered.fill.Inherit := False;
      Thumb.StateStyles.Checked.Hovered.fill.ColorKey := 'Material3.Color.PrimaryContainer'; // md.sys.color.primary-container / md.ref.palette.primary90 / #EADDFF
      Thumb.StateStyles.Checked.Hovered.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Hovered.StateLayer.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Thumb.StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      Thumb.StateStyles.UnChecked.Pressed.Fill.Assign(Thumb.StateStyles.UnChecked.Default.fill);
      Thumb.StateStyles.UnChecked.Pressed.Fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Pressed.fill.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      Thumb.StateStyles.UnChecked.Pressed.Fill.BackgroundMargins.Rect := TRectF.Create(-2*LRatio,-2*LRatio,-2*LRatio,-2*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Pressed.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Pressed.StateLayer.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      Thumb.StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Pressed (Checked)--
      Thumb.StateStyles.Checked.Pressed.Fill.Assign(Thumb.StateStyles.Checked.Default.fill);
      Thumb.StateStyles.Checked.Pressed.Fill.Inherit := False;
      Thumb.StateStyles.Checked.Pressed.fill.ColorKey := 'Material3.Color.PrimaryContainer'; // md.sys.color.primary-container / md.ref.palette.primary90 / #EADDFF
      Thumb.StateStyles.Checked.Pressed.Fill.BackgroundMargins.Rect := TRectF.Create(-2*LRatio,-2*LRatio,-2*LRatio,-2*LRatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Pressed.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Pressed.StateLayer.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Thumb.StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

      //--Focused (UnChecked)--
      Thumb.StateStyles.UnChecked.Focused.fill.Assign(Thumb.StateStyles.UnChecked.Default.fill);
      Thumb.StateStyles.UnChecked.Focused.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Focused.fill.ColorKey := 'Material3.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      Thumb.StateStyles.UnChecked.Focused.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Focused.StateLayer.ColorKey := 'Material3.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      Thumb.StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused (Checked)--
      Thumb.StateStyles.Checked.Focused.fill.Assign(Thumb.StateStyles.Checked.Default.fill);
      Thumb.StateStyles.Checked.Focused.fill.Inherit := False;
      Thumb.StateStyles.Checked.Focused.fill.ColorKey := 'Material3.Color.PrimaryContainer'; // md.sys.color.primary-container / md.ref.palette.primary90 / #EADDFF
      Thumb.StateStyles.Checked.Focused.StateLayer.Margins.Rect := TRectF.Create(-8*LRatio,-8*LRatio,-8*LRatio,-8*LRatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Focused.StateLayer.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Thumb.StateStyles.Checked.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

    finally
      EndUpdate;
    end;
  end;
end;

//////////////
// TRACKBAR //
//////////////

type
  _TALCustomTrackProtectedAccess = class(TALCustomTrack);

{******************************************************************************************}
procedure ALResetTrackBarStyle(const ATrackBar: TALCustomTrack; const AHeight: Single = 32);
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
procedure ALApplyMaterial3TrackBarStyle(const ATrackBar: TALCustomTrack; const AHeight: Single = 44);
begin
  var LRatio: Single := ALGetStyleSizeRatio('Please enter the desired height for the TrackBar', AHeight);
  With _TALCustomTrackProtectedAccess(ATrackBar) do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetTrackBarStyle(ATrackBar, AHeight * LRatio);
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
      InactiveTrack.Fill.ColorKey := 'Material3.Color.SecondaryContainer'; // md.sys.color.secondary-container / md.ref.palette.secondary90 / #E8DEF8
      ActiveTrack.Fill.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      InactiveTrack.StopIndicator.Size := RoundTo(4 * LRatio, -2);
      ActiveTrack.StopIndicator.Size := RoundTo(4 * LRatio, -2);
      InactiveTrack.StopIndicator.ColorKey := 'Material3.Color.OnSecondaryContainer'; // md.sys.color.on-secondary-container / md.ref.palette.secondary30 / #4A4458
      ActiveTrack.StopIndicator.ColorKey := 'Material3.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF
      ValueIndicator.TextSettings.Font.ColorKey := 'Material3.Color.InverseOnSurface'; // md.sys.color.inverse-on-surface / md.ref.palette.neutral95 / #F5EFF7
      ValueIndicator.TextSettings.Font.Size := RoundTo(14 * LRatio, -2);
      ValueIndicator.padding.Rect := TRectF.create(16 * LRatio{Left}, 12 * LRatio{Top}, 16 * LRatio{Right}, 12 * LRatio{Bottom}).RoundTo(-2);
      ValueIndicator.Fill.ColorKey := 'Material3.Color.InverseSurface'; // md.sys.color.inverse-surface / md.ref.palette.neutral20 / #322F35
      Thumb.Width := RoundTo(4 * LRatio, -2);
      Thumb.Fill.ColorKey := 'Material3.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Thumb.Stroke.Color := TalphaColors.Null;
      Thumb.Stroke.ColorKey := '';
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
      InactiveTrack.StateStyles.Disabled.fill.ColorKey := 'Material3.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--
      ActiveTrack.StateStyles.Disabled.fill.Assign(ActiveTrack.Fill);
      ActiveTrack.StateStyles.Disabled.fill.Inherit := False;
      ActiveTrack.StateStyles.Disabled.fill.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--
      InactiveTrack.StateStyles.Disabled.StopIndicator.Assign(InactiveTrack.StopIndicator);
      InactiveTrack.StateStyles.Disabled.StopIndicator.Inherit := False;
      InactiveTrack.StateStyles.Disabled.StopIndicator.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--
      ActiveTrack.StateStyles.Disabled.StopIndicator.Assign(ActiveTrack.StopIndicator);
      ActiveTrack.StateStyles.Disabled.StopIndicator.Inherit := False;
      ActiveTrack.StateStyles.Disabled.StopIndicator.ColorKey := 'Material3.Color.InverseOnSurface.Alpha66'; // md.sys.color.inverse-on-surface / md.ref.palette.neutral95 / #F5EFF7
      //--
      Thumb.StateStyles.Disabled.Opacity := 1;
      Thumb.StateStyles.Disabled.fill.Assign(Thumb.Fill);
      Thumb.StateStyles.Disabled.fill.Inherit := False;
      Thumb.StateStyles.Disabled.fill.ColorKey := 'Material3.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20

      //--Hovered--

      //--Pressed--
      Thumb.StateStyles.Pressed.fill.Assign(Thumb.Fill);
      Thumb.StateStyles.Pressed.fill.Inherit := False;
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then
        Thumb.StateStyles.Pressed.fill.BackgroundMargins.Rect := TRectF.Create(1*LRatio,0,1*LRatio,0).RoundTo(-2)
      else
        Thumb.StateStyles.Pressed.fill.BackgroundMargins.Rect := TRectF.Create(0,1*LRatio,0,1*LRatio).RoundTo(-2);

      //--Focused--
      Thumb.StateStyles.Focused.fill.Assign(Thumb.Fill);
      Thumb.StateStyles.Focused.fill.Inherit := False;
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then
        Thumb.StateStyles.Focused.fill.BackgroundMargins.Rect := TRectF.Create(1*LRatio,0,1*LRatio,0).RoundTo(-2)
      else
        Thumb.StateStyles.Focused.fill.BackgroundMargins.Rect := TRectF.Create(0,1*LRatio,0,1*LRatio).RoundTo(-2);

    finally
      EndUpdate;
    end;
  end;
end;

/////////////////////
// TALStyleManager //
/////////////////////

{*******************************************************}
class function TALStyleManager.GetNextSortOrder: integer;
begin
  Result := FSortOrderCounter;
  inc(FSortOrderCounter);
end;

{****************************************************************************************************************************}
constructor TALStyleManager.TTextStyleInfo.create(const AApplyStyleProc: TTextApplyStyleProc; const ADefaultFontSize: Single);
begin
  SortOrder := TALStyleManager.GetNextSortOrder;
  ApplyStyleProc := AApplyStyleProc;
  DefaultFontSize := ADefaultFontSize;
end;

{****************************************************************************************************************************}
constructor TALStyleManager.TEditStyleInfo.create(const AApplyStyleProc: TEditApplyStyleProc; const ADefaultFontSize: Single);
begin
  SortOrder := TALStyleManager.GetNextSortOrder;
  ApplyStyleProc := AApplyStyleProc;
  DefaultFontSize := ADefaultFontSize;
end;

{********************************************************************************************************************************}
constructor TALStyleManager.TButtonStyleInfo.create(const AApplyStyleProc: TButtonApplyStyleProc; const ADefaultFontSize: Single);
begin
  SortOrder := TALStyleManager.GetNextSortOrder;
  ApplyStyleProc := AApplyStyleProc;
  DefaultFontSize := ADefaultFontSize;
end;

{**********************************************************************************************************************************}
constructor TALStyleManager.TCheckBoxStyleInfo.create(const AApplyStyleProc: TCheckBoxApplyStyleProc; const ADefaultHeight: Single);
begin
  SortOrder := TALStyleManager.GetNextSortOrder;
  ApplyStyleProc := AApplyStyleProc;
  DefaultHeight := ADefaultHeight;
end;

{****************************************************************************************************************************************}
constructor TALStyleManager.TRadioButtonStyleInfo.create(const AApplyStyleProc: TRadioButtonApplyStyleProc; const ADefaultHeight: Single);
begin
  SortOrder := TALStyleManager.GetNextSortOrder;
  ApplyStyleProc := AApplyStyleProc;
  DefaultHeight := ADefaultHeight;
end;

{******************************************************************************************************************************}
constructor TALStyleManager.TSwitchStyleInfo.create(const AApplyStyleProc: TSwitchApplyStyleProc; const ADefaultHeight: Single);
begin
  SortOrder := TALStyleManager.GetNextSortOrder;
  ApplyStyleProc := AApplyStyleProc;
  DefaultHeight := ADefaultHeight;
end;

{**********************************************************************************************************************************}
constructor TALStyleManager.TTrackbarStyleInfo.create(const AApplyStyleProc: TTrackBarApplyStyleProc; const ADefaultHeight: Single);
begin
  SortOrder := TALStyleManager.GetNextSortOrder;
  ApplyStyleProc := AApplyStyleProc;
  DefaultHeight := ADefaultHeight;
end;

{*********************************}
constructor TALStyleManager.Create;
begin
  FLightColors := TDictionary<string, TPair<TAlphaColor, integer{SortOrder}>>.Create;
  FDarkColors := TDictionary<string, TPair<TAlphaColor, integer{SortOrder}>>.Create;
  fColors := TDictionary<string, TAlphaColor>.create;
  FTextStyles := TDictionary<String, TTextStyleInfo>.create;
  FEditStyles := TDictionary<String, TEditStyleInfo>.create;
  FMemoStyles := TDictionary<String, TEditStyleInfo>.create;
  FButtonStyles := TDictionary<String, TButtonStyleInfo>.create;
  FCheckBoxStyles := TDictionary<String, TCheckBoxStyleInfo>.create;
  FRadioButtonStyles := TDictionary<String, TRadioButtonStyleInfo>.create;
  FSwitchStyles := TDictionary<String, TSwitchStyleInfo>.create;
  FTrackBarStyles := TDictionary<String, TTrackBarStyleInfo>.create;
  FRangeTrackBarStyles := TDictionary<String, TTrackBarStyleInfo>.create;
  {$IF defined(ALDPK)}
  FIsDarkMode := False;
  {$ELSEIF defined(Android)}
  FIsDarkMode := GetSystemIsDarkMode;
  {$ELSEIF defined(IOS)}
  var LSystemIsDarkMode := GetSystemIsDarkMode;
  case DarkModeBehavior of
    TDarkModeBehavior.FollowSystem: FIsDarkMode := LSystemIsDarkMode;
    TDarkModeBehavior.AlwaysDark: FIsDarkMode := True;
    TDarkModeBehavior.AlwaysLight: FIsDarkMode := False;
    else raise Exception.Create('Error 888B969C-A83E-4717-838E-6A830DB0B7D6');
  end;
  if LSystemIsDarkMode <> FIsDarkMode then begin
    var LWindow := TALUIView.Wrap(NSObjectToID(TiOSHelper.SharedApplication.keyWindow));
    if Assigned(LWindow) then begin
      if not FIsDarkMode then LWindow.setOverrideUserInterfaceStyle(UIUserInterfaceStyleLight)
      else LWindow.setOverrideUserInterfaceStyle(UIUserInterfaceStyleDark);
    end
  end;
  {$ELSEIF defined(MSWindows) or defined(ALMacOS)}
  case DarkModeBehavior of
    TDarkModeBehavior.FollowSystem: FIsDarkMode := GetSystemIsDarkMode;
    TDarkModeBehavior.AlwaysDark: FIsDarkMode := True;
    TDarkModeBehavior.AlwaysLight: FIsDarkMode := False;
    else raise Exception.Create('Error 06933A1F-8EA6-46BE-9204-F2D1D27566DD');
  end;
  {$ELSE}
  FIsDarkMode := False;
  {$ENDIF}
  InitStyles;
  InitColors;
  {$IF defined(DEBUG)}
  ALLog('TALStyleManager', 'DarkMode: ' + ALBoolToStrW(FIsDarkMode, 'True', 'False'));
  {$ENDIF}
  inherited;
end;

{*********************************}
destructor TALStyleManager.Destroy;
begin
  ALFreeAndNil(FLightColors);
  ALFreeAndNil(FDarkColors);
  AlFreeAndNil(fColors);
  AlFreeAndNil(FTextStyles);
  AlFreeAndNil(FEditStyles);
  AlFreeAndNil(FMemoStyles);
  AlFreeAndNil(FButtonStyles);
  AlFreeAndNil(FCheckBoxStyles);
  AlFreeAndNil(FRadioButtonStyles);
  AlFreeAndNil(FSwitchStyles);
  AlFreeAndNil(FTrackBarStyles);
  AlFreeAndNil(FRangeTrackBarStyles);
  inherited;
end;

{***********************************}
procedure TALStyleManager.InitStyles;
begin
  // Note: To generate a new Material 3 color palette, you can use the following tool:
  // https://www.figma.com/community/plugin/1034969338659738588/material-theme-builder
  // Select Export > Material Theme (JSON)

  AddOrSetColor('Material3.Color.Primary', $FF6750A4, False); // md.ref.palette.primary40 | Main color used across screens and components
  AddOrSetColor('Material3.Color.OnPrimary', $FFFFFFFF, False); // md.ref.palette.primary100 | Text and icons shown against the primary color
  AddOrSetColor('Material3.Color.PrimaryContainer', $FFEADDFF, False); // md.ref.palette.primary90 | Standout container color for key components
  AddOrSetColor('Material3.Color.OnPrimaryContainer', $FF4F378B, False); // md.ref.palette.primary30 | Contrast-passing color shown against the primary container
  AddOrSetColor('Material3.Color.Secondary', $FF625B71, False); // md.ref.palette.secondary40 | Accent color used across screens and components
  AddOrSetColor('Material3.Color.OnSecondary', $FFFFFFFF, False); // md.ref.palette.secondary100 | Text and icons shown against the secondary color
  AddOrSetColor('Material3.Color.SecondaryContainer', $FFE8DEF8, False); // md.ref.palette.secondary90 | Less prominent container color, for components like tonal buttons
  AddOrSetColor('Material3.Color.OnSecondaryContainer', $FF4A4458, False); // md.ref.palette.secondary30 | Contrast-passing color shown against the secondary container
  AddOrSetColor('Material3.Color.Tertiary', $FF7D5260, False); // md.ref.palette.tertiary40
  AddOrSetColor('Material3.Color.OnTertiary', $FFFFFFFF, False); // md.ref.palette.tertiary100
  AddOrSetColor('Material3.Color.TertiaryContainer', $FFFFD8E4, False); // md.ref.palette.tertiary90 | Contrasting container color, for components like input fields
  AddOrSetColor('Material3.Color.OnTertiaryContainer', $FF633B48, False); // md.ref.palette.tertiary30 | Contrast-passing color shown against the tertiary container
  AddOrSetColor('Material3.Color.Error', $FFB3261E, False); // md.ref.palette.error40 | Indicates errors, such as invalid input in a date picker
  AddOrSetColor('Material3.Color.OnError', $FFFFFFFF, False); // md.ref.palette.error100 | Used for text and icons on the error color
  AddOrSetColor('Material3.Color.ErrorContainer', $FFF9DEDC, False); // md.ref.palette.error90
  AddOrSetColor('Material3.Color.OnErrorContainer', $FF8C1D18, False); // md.ref.palette.error30
  AddOrSetColor('Material3.Color.Surface', $FFFEF7FF, False); // md.ref.palette.neutral98 | Surface color for components like cards, sheets, and menus
  AddOrSetColor('Material3.Color.OnSurface', $FF1D1B20, False); // md.ref.palette.neutral10 | Text and icons shown against the surface color
  AddOrSetColor('Material3.Color.SurfaceVariant', $FFE7E0EC, False); // md.ref.palette.neutral-variant90 | Alternate surface color, can be used for active states
  AddOrSetColor('Material3.Color.OnSurfaceVariant', $FF49454F, False); // md.ref.palette.neutral-variant30 | For text and icons to indicate active or inactive component state
  AddOrSetColor('Material3.Color.SurfaceContainerHighest', $FFE6E0E9, False); // md.ref.palette.neutral90
  AddOrSetColor('Material3.Color.SurfaceContainerHigh', $FFECE6F0, False); // md.ref.palette.neutral92
  AddOrSetColor('Material3.Color.SurfaceContainer', $FFF3EDF7, False); // md.ref.palette.neutral94
  AddOrSetColor('Material3.Color.SurfaceContainerLow', $FFF7F2FA, False); // md.ref.palette.neutral96
  AddOrSetColor('Material3.Color.SurfaceContainerLowest', $FFFFFFFF, False); // md.ref.palette.neutral100
  AddOrSetColor('Material3.Color.InverseSurface', $FF322F35, False); // md.ref.palette.neutral20 | Displays opposite color of the surrounding UI
  AddOrSetColor('Material3.Color.InverseOnSurface', $FFF5EFF7, False); // md.ref.palette.neutral95 | Used for text and icons shown against the inverse surface color
  AddOrSetColor('Material3.Color.SurfaceTint', $FF6750A4, False); // md.ref.palette.primary40
  AddOrSetColor('Material3.Color.Outline', $FF79747E, False); // md.ref.palette.neutral-variant50 | Subtle color used for boundaries
  AddOrSetColor('Material3.Color.OutlineVariant', $FFCAC4D0, False); // md.ref.palette.neutral-variant80 | Outline-variant is used to define the border of a component where 3:1 contrast ratio isn’t required, a container, or a divider.
  AddOrSetColor('Material3.Color.PrimaryFixed', $FFEADDFF, False); // md.ref.palette.primary90
  AddOrSetColor('Material3.Color.OnPrimaryFixed', $FF21005D, False); // md.ref.palette.primary10
  AddOrSetColor('Material3.Color.PrimaryFixedDim', $FFD0BCFF, False); // md.ref.palette.primary80
  AddOrSetColor('Material3.Color.OnPrimaryFixedVariant', $FF4F378B, False); // md.ref.palette.primary30
  AddOrSetColor('Material3.Color.InversePrimary', $FFD0BCFF, False); // md.ref.palette.primary80 | Displays opposite of the primary color
  AddOrSetColor('Material3.Color.SecondaryFixed', $FFE8DEF8, False); // md.ref.palette.secondary90
  AddOrSetColor('Material3.Color.OnSecondaryFixed', $FF1D192B, False); // md.ref.palette.secondary10
  AddOrSetColor('Material3.Color.SecondaryFixedDim', $FFCCC2DC, False); // md.ref.palette.secondary80
  AddOrSetColor('Material3.Color.OnSecondaryFixedVariant', $FF4A4458, False); // md.ref.palette.secondary30
  AddOrSetColor('Material3.Color.TertiaryFixed', $FFFFD8E4, False); // md.ref.palette.tertiary90
  AddOrSetColor('Material3.Color.OnTertiaryFixed', $FF31111D, False); // md.ref.palette.tertiary10
  AddOrSetColor('Material3.Color.TertiaryFixedDim', $FFEFB8C8, False); // md.ref.palette.tertiary80
  AddOrSetColor('Material3.Color.OnTertiaryFixedVariant', $FF633B48, False); // md.ref.palette.tertiary30
  //AddOrSetColor('Material3.Color.Background', $FFFEF7FF, False); // md.ref.palette.neutral98 | Note: Background is a legacy color role. It is recommended to use Surface instead of Background.
  //AddOrSetColor('Material3.Color.OnBackground', $FF1D1B20, False); // md.ref.palette.neutral10 | Used for text and icons shown against the background color
  AddOrSetColor('Material3.Color.SurfaceBright', $FFFEF7FF, False); // md.ref.palette.neutral98
  AddOrSetColor('Material3.Color.SurfaceDim', $FFDED8E1, False); // md.ref.palette.neutral87
  AddOrSetColor('Material3.Color.Scrim', $FF000000, False); // md.ref.palette.neutral0 | Used for scrims which help separate floating components from the background.
  AddOrSetColor('Material3.Color.Shadow', $FF000000, False); // md.ref.palette.neutral0 | For shadows applied to elevated components

  AddOrSetColor('Material3.Color.Primary', $FFD0BCFF, True); // md.ref.palette.primary80 | Main color used across screens and components
  AddOrSetColor('Material3.Color.OnPrimary', $FF381E72, True); // md.ref.palette.primary20 | Text and icons shown against the primary color
  AddOrSetColor('Material3.Color.PrimaryContainer', $FF4F378B, True); // md.ref.palette.primary30 | Standout container color for key components
  AddOrSetColor('Material3.Color.OnPrimaryContainer', $FFEADDFF, True); // md.ref.palette.primary90 | Contrast-passing color shown against the primary container
  AddOrSetColor('Material3.Color.Secondary', $FFCCC2DC, True); // md.ref.palette.secondary80 | Accent color used across screens and components
  AddOrSetColor('Material3.Color.OnSecondary', $FF332D41, True); // md.ref.palette.secondary20 | Text and icons shown against the secondary color
  AddOrSetColor('Material3.Color.SecondaryContainer', $FF4A4458, True); // md.ref.palette.secondary30 | Less prominent container color, for components like tonal buttons
  AddOrSetColor('Material3.Color.OnSecondaryContainer', $FFE8DEF8, True); // md.ref.palette.secondary90 | Contrast-passing color shown against the secondary container
  AddOrSetColor('Material3.Color.Tertiary', $FFEFB8C8, True); // md.ref.palette.tertiary80
  AddOrSetColor('Material3.Color.OnTertiary', $FF492532, True); // md.ref.palette.tertiary20
  AddOrSetColor('Material3.Color.TertiaryContainer', $FF633B48, True); // md.ref.palette.tertiary30 | Contrasting container color, for components like input fields
  AddOrSetColor('Material3.Color.OnTertiaryContainer', $FFFFD8E4, True); // md.ref.palette.tertiary90 | Contrast-passing color shown against the tertiary container
  AddOrSetColor('Material3.Color.Error', $FFF2B8B5, True); // md.ref.palette.error80 | Indicates errors, such as invalid input in a date picker
  AddOrSetColor('Material3.Color.OnError', $FF601410, True); // md.ref.palette.error20 | Used for text and icons on the error color
  AddOrSetColor('Material3.Color.ErrorContainer', $FF8C1D18, True); // md.ref.palette.error30
  AddOrSetColor('Material3.Color.OnErrorContainer', $FFF9DEDC, True); // md.ref.palette.error90
  AddOrSetColor('Material3.Color.Surface', $FF141218, True); // md.ref.palette.neutral6 | Surface color for components like cards, sheets, and menus
  AddOrSetColor('Material3.Color.OnSurface', $FFE6E0E9, True); // md.ref.palette.neutral90 | Text and icons shown against the surface color
  AddOrSetColor('Material3.Color.SurfaceVariant', $FF49454F, True); // md.ref.palette.neutral-variant30 | Alternate surface color, can be used for active states
  AddOrSetColor('Material3.Color.OnSurfaceVariant', $FFCAC4D0, True); // md.ref.palette.neutral-variant80 | For text and icons to indicate active or inactive component state
  AddOrSetColor('Material3.Color.SurfaceContainerHighest', $FF36343B, True); // md.ref.palette.neutral22
  AddOrSetColor('Material3.Color.SurfaceContainerHigh', $FF2B2930, True); // md.ref.palette.neutral17
  AddOrSetColor('Material3.Color.SurfaceContainer', $FF211F26, True); // md.ref.palette.neutral12
  AddOrSetColor('Material3.Color.SurfaceContainerLow', $FF1D1B20, True); // md.ref.palette.neutral10
  AddOrSetColor('Material3.Color.SurfaceContainerLowest', $FF0F0D13, True); // md.ref.palette.neutral4
  AddOrSetColor('Material3.Color.InverseSurface', $FFE6E0E9, True); // md.ref.palette.neutral90 | Displays opposite color of the surrounding UI
  AddOrSetColor('Material3.Color.InverseOnSurface', $FF322F35, True); // md.ref.palette.neutral20 | Used for text and icons shown against the inverse surface color
  AddOrSetColor('Material3.Color.SurfaceTint', $FFD0BCFF, True); // md.ref.palette.primary80
  AddOrSetColor('Material3.Color.Outline', $FF938F99, True); // md.ref.palette.neutral-variant60 | Subtle color used for boundaries
  AddOrSetColor('Material3.Color.OutlineVariant', $FF49454F, True); // md.ref.palette.neutral-variant30 | Outline-variant is used to define the border of a component where 3:1 contrast ratio isn’t required, a container, or a divider.
  AddOrSetColor('Material3.Color.PrimaryFixed', $FFEADDFF, True); // md.ref.palette.primary90
  AddOrSetColor('Material3.Color.OnPrimaryFixed', $FF21005D, True); // md.ref.palette.primary10
  AddOrSetColor('Material3.Color.PrimaryFixedDim', $FFD0BCFF, True); // md.ref.palette.primary80
  AddOrSetColor('Material3.Color.OnPrimaryFixedVariant', $FF4F378B, True); // md.ref.palette.primary30
  AddOrSetColor('Material3.Color.InversePrimary', $FF6750A4, True); // md.ref.palette.primary40 | Displays opposite of the primary color
  AddOrSetColor('Material3.Color.SecondaryFixed', $FFE8DEF8, True); // md.ref.palette.secondary90
  AddOrSetColor('Material3.Color.OnSecondaryFixed', $FF1D192B, True); // md.ref.palette.secondary10
  AddOrSetColor('Material3.Color.SecondaryFixedDim', $FFCCC2DC, True); // md.ref.palette.secondary80
  AddOrSetColor('Material3.Color.OnSecondaryFixedVariant', $FF4A4458, True); // md.ref.palette.secondary30
  AddOrSetColor('Material3.Color.TertiaryFixed', $FFFFD8E4, True); // md.ref.palette.tertiary90
  AddOrSetColor('Material3.Color.OnTertiaryFixed', $FF31111D, True); // md.ref.palette.tertiary10
  AddOrSetColor('Material3.Color.TertiaryFixedDim', $FFEFB8C8, True); // md.ref.palette.tertiary80
  AddOrSetColor('Material3.Color.OnTertiaryFixedVariant', $FF633B48, True); // md.ref.palette.tertiary30
  //AddOrSetColor('Material3.Color.Background', $FF141218, True); // md.ref.palette.neutral6 | Note: Background is a legacy color role. It is recommended to use Surface instead of Background.
  //AddOrSetColor('Material3.Color.OnBackground', $FFE6E0E9, True); // md.ref.palette.neutral90 | Used for text and icons shown against the background color
  AddOrSetColor('Material3.Color.SurfaceBright', $FF3B383E, True); // md.ref.palette.neutral24
  AddOrSetColor('Material3.Color.SurfaceDim', $FF141218, True); // md.ref.palette.neutral6
  AddOrSetColor('Material3.Color.Scrim', $FF000000, True); // md.ref.palette.neutral0 | Used for scrims which help separate floating components from the background.
  AddOrSetColor('Material3.Color.Shadow', $FF000000, True); // md.ref.palette.neutral0 | For shadows applied to elevated components

  AddOrSetTextStyle('Default', ALResetTextStyle, 16{ADefaultFontSize});

  AddOrSetEditStyle('Default', ALResetEditStyle, 16{ADefaultFontSize});
  AddOrSetEditStyle('Material3.Edit.Filled', ALApplyMaterial3EditFilledStyle, 16{ADefaultFontSize});
  AddOrSetEditStyle('Material3.Edit.Filled.Error', ALApplyMaterial3EditFilledErrorStyle, 16{ADefaultFontSize});
  AddOrSetEditStyle('Material3.Edit.Outlined', ALApplyMaterial3EditOutlinedStyle, 16{ADefaultFontSize});
  AddOrSetEditStyle('Material3.Edit.Outlined.Error', ALApplyMaterial3EditOutlinedErrorStyle, 16{ADefaultFontSize});
  AddOrSetEditStyle('Material3.Edit.Hybrid', ALApplyMaterial3EditHybridStyle, 16{ADefaultFontSize});
  AddOrSetEditStyle('Material3.Edit.Hybrid.Error', ALApplyMaterial3EditHybridErrorStyle, 16{ADefaultFontSize});

  AddOrSetMemoStyle('Default', ALResetEditStyle, 16{ADefaultFontSize});
  AddOrSetMemoStyle('Material3.Memo.Filled', ALApplyMaterial3EditFilledStyle, 16{ADefaultFontSize});
  AddOrSetMemoStyle('Material3.Memo.Filled.Error', ALApplyMaterial3EditFilledErrorStyle, 16{ADefaultFontSize});
  AddOrSetMemoStyle('Material3.Memo.Outlined', ALApplyMaterial3EditOutlinedStyle, 16{ADefaultFontSize});
  AddOrSetMemoStyle('Material3.Memo.Outlined.Error', ALApplyMaterial3EditOutlinedErrorStyle, 16{ADefaultFontSize});

  AddOrSetButtonStyle('Default', ALResetButtonStyle, 14{ADefaultFontSize});
  AddOrSetButtonStyle('Material3.Button.Filled', ALApplyMaterial3ButtonFilledStyle, 14{ADefaultFontSize});
  AddOrSetButtonStyle('Material3.Button.Outlined', ALApplyMaterial3ButtonOutlinedStyle, 14{ADefaultFontSize});
  AddOrSetButtonStyle('Material3.Button.Text', ALApplyMaterial3ButtonTextStyle, 14{ADefaultFontSize});
  AddOrSetButtonStyle('Material3.Button.Elevated', ALApplyMaterial3ButtonElevatedStyle, 14{ADefaultFontSize});
  AddOrSetButtonStyle('Material3.Button.Tonal', ALApplyMaterial3ButtonTonalStyle, 14{ADefaultFontSize});
  AddOrSetButtonStyle('Material3.Button.Icon.Filled', ALApplyMaterial3ButtonIconFilledStyle, 14{ADefaultFontSize});
  AddOrSetButtonStyle('Material3.Button.Icon.Outlined', ALApplyMaterial3ButtonIconOutlinedStyle, 14{ADefaultFontSize});
  AddOrSetButtonStyle('Material3.Button.Icon.Standard', ALApplyMaterial3ButtonIconStandardStyle, 14{ADefaultFontSize});
  AddOrSetButtonStyle('Material3.Button.Icon.Tonal', ALApplyMaterial3ButtonIconTonalStyle, 14{ADefaultFontSize});

  AddOrSetCheckBoxStyle('Default', ALResetCheckBoxStyle, 18{ADefaultHeight});
  AddOrSetCheckBoxStyle('Material3.CheckBox', ALApplyMaterial3CheckBoxStyle, 18{ADefaultHeight});
  AddOrSetCheckBoxStyle('Material3.CheckBox.Error', ALApplyMaterial3CheckBoxErrorStyle, 18{ADefaultHeight});

  AddOrSetRadioButtonStyle('Default', ALResetRadioButtonStyle, 20{ADefaultHeight});
  AddOrSetRadioButtonStyle('Material3.RadioButton', ALApplyMaterial3RadioButtonStyle, 20{ADefaultHeight});
  AddOrSetRadioButtonStyle('Material3.RadioButton.Error', ALApplyMaterial3RadioButtonErrorStyle, 20{ADefaultHeight});

  AddOrSetSwitchStyle('Default', ALResetSwitchStyle, 32{ADefaultHeight});
  AddOrSetSwitchStyle('Material3.Switch', ALApplyMaterial3SwitchStyle, 32{ADefaultHeight});

  AddOrSetTrackBarStyle('Default', ALResetTrackBarStyle, 32{ADefaultHeight});
  AddOrSetTrackBarStyle('Material3.TrackBar', ALApplyMaterial3TrackBarStyle, 44{ADefaultHeight});

  AddOrSetRangeTrackBarStyle('Default', ALResetTrackBarStyle, 32{ADefaultHeight});
  AddOrSetRangeTrackBarStyle('Material3.RangeTrackBar', ALApplyMaterial3TrackBarStyle, 44{ADefaultHeight});
end;

{***********************************}
procedure TALStyleManager.InitColors;
begin
  FColors.Clear;
  if not IsDarkMode then begin
    var LArray := FLightColors.ToArray;
    for var I := low(LArray) to High(LArray) do
      FColors.AddOrSetValue(LArray[I].Key, LArray[I].Value.Key);
  end
  else begin
    var LArray := FDarkColors.ToArray;
    for var I := low(LArray) to High(LArray) do
      FColors.AddOrSetValue(LArray[I].Key, LArray[I].Value.Key);
  end;
end;

{*************}
//[MultiThread]
class function TALStyleManager.CreateInstance: TALStyleManager;
begin
  result := TALStyleManager.Create;
end;

{*************}
//[MultiThread]
class function TALStyleManager.GetInstance: TALStyleManager;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance);
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALStyleManager.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{****************************************************}
function TALStyleManager.GetSystemIsDarkMode: boolean;
begin
  // Under iOS: IFMXSystemAppearanceService always returns the system appearance setting,
  // ignoring any value assigned via setOverrideUserInterfaceStyle.
  // Its result is also influenced by the UIUserInterfaceStyle value defined in Info.plist.
  // Under Android: IFMXSystemAppearanceService returns the system appearance setting,
  // or the value previously set via setApplicationNightMode, which persists across app restarts.
  var LSystemAppearance: IFMXSystemAppearanceService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXSystemAppearanceService, LSystemAppearance) then
    Result := LSystemAppearance.ThemeKind = TSystemThemeKind.Dark
  else
    Result := False;
end;

{**************************************************************}
function TALStyleManager.GetDarkModeBehavior: TDarkModeBehavior;
begin
  Result := TDarkModeBehavior(TALUserPreferences.Instance.getInt32('Alcinoe.DarkModeBehavior', integer(TDarkModeBehavior.FollowSystem)));
end;

{*****************************************************************************}
procedure TALStyleManager.SetDarkModeBehavior(const AValue: TDarkModeBehavior);
begin
  var FPrevIsDarkMode := FIsDarkMode;

  {$IF defined(ANDROID)}
  // We use UiModeManager because it persists across app restarts.
  // AppCompatDelegate.setDefaultNightMode is not persistent and requires
  // calling activity.recreate(), which in our case would terminate the app.
  if TOSVersion.Check(12, 0) {API level >= 31 (Android S)} then begin
    var LUIModeService := TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.UI_MODE_SERVICE);
    if LUIModeService <> nil then begin
      var LUiModeManager := TJUiModeManager.Wrap(TAndroidHelper.JObjectToID(LUIModeService));
      case AValue of
        TDarkModeBehavior.FollowSystem: begin
          LUiModeManager.setApplicationNightMode(TJUiModeManager.Javaclass.MODE_NIGHT_AUTO);
          TALUserPreferences.Instance.SetInt32('Alcinoe.DarkModeBehavior', integer(TDarkModeBehavior.FollowSystem));
          FIsDarkMode := LUiModeManager.getNightMode = TJUiModeManager.Javaclass.MODE_NIGHT_YES;
        end;
        TDarkModeBehavior.AlwaysDark: begin
          LUiModeManager.setApplicationNightMode(TJUiModeManager.Javaclass.MODE_NIGHT_YES);
          TALUserPreferences.Instance.SetInt32('Alcinoe.DarkModeBehavior', integer(TDarkModeBehavior.AlwaysDark));
          FIsDarkMode := True;
        end;
        TDarkModeBehavior.AlwaysLight: begin
          LUiModeManager.setApplicationNightMode(TJUiModeManager.Javaclass.MODE_NIGHT_NO);
          TALUserPreferences.Instance.SetInt32('Alcinoe.DarkModeBehavior', integer(TDarkModeBehavior.AlwaysLight));
          FIsDarkMode := False;
        end;
        else Raise Exception.Create('Error C03E31E0-61CA-4C67-8A50-6765BED18013')
      end;
    end;
  end;
  {$ELSEIF defined(IOS)}
  var LWindow := TALUIView.Wrap(NSObjectToID(TiOSHelper.SharedApplication.keyWindow));
  case AValue of
    TDarkModeBehavior.FollowSystem: begin
      if Assigned(LWindow) then LWindow.setOverrideUserInterfaceStyle(UIUserInterfaceStyleUnspecified);
      TALUserPreferences.Instance.SetInt32('Alcinoe.DarkModeBehavior', integer(TDarkModeBehavior.FollowSystem));
      FIsDarkMode := GetSystemIsDarkMode;
    end;
    TDarkModeBehavior.AlwaysDark: begin
      if Assigned(LWindow) then LWindow.setOverrideUserInterfaceStyle(UIUserInterfaceStyleDark);
      TALUserPreferences.Instance.SetInt32('Alcinoe.DarkModeBehavior', integer(TDarkModeBehavior.AlwaysDark));
      FIsDarkMode := True;
    end;
    TDarkModeBehavior.AlwaysLight: begin
      if Assigned(LWindow) then LWindow.setOverrideUserInterfaceStyle(UIUserInterfaceStyleLight);
      TALUserPreferences.Instance.SetInt32('Alcinoe.DarkModeBehavior', integer(TDarkModeBehavior.AlwaysLight));
      FIsDarkMode := False;
    end;
    else Raise Exception.Create('Error 45A23EDB-9B44-4885-B506-0B529E6D5AEA')
  end;
  {$ELSEIF defined(MSWindows) or defined(ALMacOS)}
  case AValue of
    TDarkModeBehavior.FollowSystem: begin
      TALUserPreferences.Instance.SetInt32('Alcinoe.DarkModeBehavior', integer(TDarkModeBehavior.FollowSystem));
      FIsDarkMode := GetSystemIsDarkMode;
    end;
    TDarkModeBehavior.AlwaysDark: begin
      TALUserPreferences.Instance.SetInt32('Alcinoe.DarkModeBehavior', integer(TDarkModeBehavior.AlwaysDark));
      FIsDarkMode := True;
    end;
    TDarkModeBehavior.AlwaysLight: begin
      TALUserPreferences.Instance.SetInt32('Alcinoe.DarkModeBehavior', integer(TDarkModeBehavior.AlwaysLight));
      FIsDarkMode := False;
    end;
    else Raise Exception.Create('Error 45A23EDB-9B44-4885-B506-0B529E6D5AEA')
  end;
  {$ENDIF}

  if FPrevIsDarkMode <> FIsDarkMode then
    InitColors;
end;

{*****************************************************************************************************}
procedure TALStyleManager.ApplyColorScheme(const AForm: TCustomForm; const AFormFillColorName: String);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure ApplyColorSchemeRecursive(const AControl: TControl);
  begin
    for var I := 0 to AControl.ControlsCount - 1 do
      if AControl.Controls[i] is TALControl then TALControl(AControl.Controls[i]).ApplyColorScheme
      else ApplyColorSchemeRecursive(AControl.Controls[i]);
  end;

begin
  if AFormFillColorName <> '' then
    AForm.Fill.Color := GetColor(AFormFillColorName);
  For var I := 0 to AForm.ChildrenCount - 1 do begin
    if (AForm.Children[i] is TALControl) then TALControl(AForm.Children[i]).ApplyColorScheme
    else if (AForm.Children[i] is TControl) then ApplyColorSchemeRecursive(TControl(AForm.Children[i]));
  end;
end;

{*********************************************************************************************************************}
procedure TALStyleManager.AddOrSetColor(const AName: String; Const AValue: TAlphaColor; Const AIsForDarkMode: Boolean);
begin
  var LPair: TPair<TAlphaColor, integer{SortOrder}>;
  if AIsForDarkMode then begin
    if not fDarkColors.TryGetValue(AName, LPair) then begin
      fDarkColors.Add(
        AName,
        TPair<TAlphaColor, integer{SortOrder}>.Create(
                                                 AValue,
                                                 TALStyleManager.GetNextSortOrder))
    end
    else begin
      LPair.Key := AValue;
      fDarkColors.AddOrSetValue(AName, LPair);
    end;
  end
  else begin
    if not fLightColors.TryGetValue(AName, LPair) then begin
      fLightColors.Add(
        AName,
        TPair<TAlphaColor, integer{SortOrder}>.Create(
                                                 AValue,
                                                 TALStyleManager.GetNextSortOrder))
    end
    else begin
      LPair.Key := AValue;
      fLightColors.AddOrSetValue(AName, LPair);
    end;
  end;
end;

{*******************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetTextStyle(const AName: String; const AApplyStyleProc: TTextApplyStyleProc; const ADefaultFontSize: Single);
begin
  FTextStyles.AddOrSetValue(AName, TTextStyleInfo.create(AApplyStyleProc, ADefaultFontSize));
end;

{*******************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetEditStyle(const AName: String; const AApplyStyleProc: TEditApplyStyleProc; const ADefaultFontSize: Single);
begin
  FEditStyles.AddOrSetValue(AName, TEditStyleInfo.create(AApplyStyleProc, ADefaultFontSize));
end;

{*******************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetMemoStyle(const AName: String; const AApplyStyleProc: TEditApplyStyleProc; const ADefaultFontSize: Single);
begin
  FMemoStyles.AddOrSetValue(AName, TEditStyleInfo.create(AApplyStyleProc, ADefaultFontSize));
end;

{***********************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetButtonStyle(const AName: String; const AApplyStyleProc: TButtonApplyStyleProc; const ADefaultFontSize: Single);
begin
  FButtonStyles.AddOrSetValue(AName, TButtonStyleInfo.create(AApplyStyleProc, ADefaultFontSize));
end;

{*************************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetCheckBoxStyle(const AName: String; const AApplyStyleProc: TCheckBoxApplyStyleProc; const ADefaultHeight: Single);
begin
  FCheckBoxStyles.AddOrSetValue(AName, TCheckBoxStyleInfo.create(AApplyStyleProc, ADefaultHeight));
end;

{*******************************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetRadioButtonStyle(const AName: String; const AApplyStyleProc: TRadioButtonApplyStyleProc; const ADefaultHeight: Single);
begin
  FRadioButtonStyles.AddOrSetValue(AName, TRadioButtonStyleInfo.create(AApplyStyleProc, ADefaultHeight));
end;

{*********************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetSwitchStyle(const AName: String; const AApplyStyleProc: TSwitchApplyStyleProc; const ADefaultHeight: Single);
begin
  FSwitchStyles.AddOrSetValue(AName, TSwitchStyleInfo.create(AApplyStyleProc, ADefaultHeight));
end;

{*************************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetTrackBarStyle(const AName: String; const AApplyStyleProc: TTrackBarApplyStyleProc; const ADefaultHeight: Single);
begin
  FTrackBarStyles.AddOrSetValue(AName, TTrackBarStyleInfo.create(AApplyStyleProc, ADefaultHeight));
end;

{******************************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetRangeTrackBarStyle(const AName: String; const AApplyStyleProc: TTrackBarApplyStyleProc; const ADefaultHeight: Single);
begin
  FRangeTrackBarStyles.AddOrSetValue(AName, TTrackBarStyleInfo.create(AApplyStyleProc, ADefaultHeight));
end;

{******************************************************************}
function TALStyleManager.GetColor(const AName: String): TAlphaColor;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _TryGetValueWithAlpha(out AValue: TAlphacolor): Boolean;
  begin
    // Ex: Material3.Color.Primary.Alpha08
    var Ln := length(AName);
    Result := (Ln > 8) and
              (AName[Ln-7] = '.') and
              (charInSet(AName[Ln-6], ['a','A'])) and
              (charInSet(AName[Ln-5], ['l','L'])) and
              (charInSet(AName[Ln-4], ['p','P'])) and
              (charInSet(AName[Ln-3], ['h','H'])) and
              (charInSet(AName[Ln-2], ['a','A'])) and
              (charInSet(AName[Ln-1], ['0'..'9'])) and
              (charInSet(AName[Ln], ['0'..'9']));
    if result then begin
      var LAlpha: Single := (((ord(AName[Ln-1]) - 48) * 10) + ((ord(AName[Ln]) - 48))) / 100;
      if fColors.TryGetValue(AlCopyStr(AName, 1, Ln-8), Avalue) then Avalue := ALSetColorAlpha(Avalue, LAlpha)
      else begin
        AValue := TAlphaColors.Null;
        result := False;
      end;
    end;

  end;

begin
  if (not fColors.TryGetValue(AName, Result)) and
     (not _TryGetValueWithAlpha(Result)) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Color "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit(TAlphaColors.Null);
  end;
end;

{**********************************************************************************}
procedure TALStyleManager.ApplyTextStyle(const AName: String; const AText: TALText);
begin
  Var LApplyTextStyleInfo: TTextStyleInfo;
  if not fTextStyles.TryGetValue(AName, LApplyTextStyleInfo) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Color "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit;
  end;

  AText.BeginUpdate;
  try
    {$IF defined(ALDPK)}
    LApplyTextStyleInfo.ApplyStyleProc(AText, LApplyTextStyleInfo.DefaultFontSize);
    {$ELSE}
    LApplyTextStyleInfo.ApplyStyleProc(AText, AText.TextSettings.Font.Size);
    {$ENDIF}
  finally
    AText.EndUpdate;
  end;
end;

{**************************************************************************************}
procedure TALStyleManager.ApplyEditStyle(const AName: String; const AEdit: TALBaseEdit);
begin
  Var LApplyEditStyleInfo: TEditStyleInfo;
  if not fEditStyles.TryGetValue(AName, LApplyEditStyleInfo) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Style "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit;
  end;

  AEdit.BeginUpdate;
  try
    {$IF defined(ALDPK)}
    LApplyEditStyleInfo.ApplyStyleProc(AEdit, LApplyEditStyleInfo.DefaultFontSize);
    {$ELSE}
    LApplyEditStyleInfo.ApplyStyleProc(AEdit, AEdit.TextSettings.Font.Size);
    {$ENDIF}
  finally
    AEdit.EndUpdate;
  end;
end;

{**************************************************************************************}
procedure TALStyleManager.ApplyMemoStyle(const AName: String; const AMemo: TALBaseEdit);
begin
  Var LApplyMemoStyleInfo: TEditStyleInfo;
  if not fMemoStyles.TryGetValue(AName, LApplyMemoStyleInfo) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Style "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit;
  end;

  AMemo.BeginUpdate;
  try
    {$IF defined(ALDPK)}
    LApplyMemoStyleInfo.ApplyStyleProc(AMemo, LApplyMemoStyleInfo.DefaultFontSize);
    {$ELSE}
    LApplyMemoStyleInfo.ApplyStyleProc(AMemo, AMemo.TextSettings.Font.Size);
    {$ENDIF}
  finally
    AMemo.EndUpdate;
  end;
end;

{****************************************************************************************}
procedure TALStyleManager.ApplyButtonStyle(const AName: String; const AButton: TALButton);
begin
  Var LApplyButtonStyleInfo: TButtonStyleInfo;
  if not fButtonStyles.TryGetValue(AName, LApplyButtonStyleInfo) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Style "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit;
  end;

  AButton.BeginUpdate;
  try
    {$IF defined(ALDPK)}
    LApplyButtonStyleInfo.ApplyStyleProc(AButton, LApplyButtonStyleInfo.DefaultFontSize);
    {$ELSE}
    LApplyButtonStyleInfo.ApplyStyleProc(AButton, AButton.TextSettings.Font.Size);
    {$ENDIF}
  finally
    AButton.EndUpdate;
  end;
end;

{**********************************************************************************************}
procedure TALStyleManager.ApplyCheckBoxStyle(const AName: String; const ACheckBox: TALCheckBox);
begin
  Var LApplyCheckBoxStyleInfo: TCheckBoxStyleInfo;
  if not fCheckBoxStyles.TryGetValue(AName, LApplyCheckBoxStyleInfo) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Style "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit;
  end;

  ACheckBox.BeginUpdate;
  try
    {$IF defined(ALDPK)}
    LApplyCheckBoxStyleInfo.ApplyStyleProc(ACheckBox, LApplyCheckBoxStyleInfo.DefaultHeight);
    {$ELSE}
    LApplyCheckBoxStyleInfo.ApplyStyleProc(ACheckBox, ACheckBox.Height);
    {$ENDIF}
  finally
    ACheckBox.EndUpdate;
  end;
end;

{*******************************************************************************************************}
procedure TALStyleManager.ApplyRadioButtonStyle(const AName: String; const ARadioButton: TALRadioButton);
begin
  Var LApplyRadioButtonStyleInfo: TRadioButtonStyleInfo;
  if not fRadioButtonStyles.TryGetValue(AName, LApplyRadioButtonStyleInfo) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Style "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit;
  end;

  ARadioButton.BeginUpdate;
  try
    {$IF defined(ALDPK)}
    LApplyRadioButtonStyleInfo.ApplyStyleProc(ARadioButton, LApplyRadioButtonStyleInfo.DefaultHeight);
    {$ELSE}
    LApplyRadioButtonStyleInfo.ApplyStyleProc(ARadioButton, ARadioButton.Height);
    {$ENDIF}
  finally
    ARadioButton.EndUpdate;
  end;
end;

{****************************************************************************************}
procedure TALStyleManager.ApplySwitchStyle(const AName: String; const ASwitch: TALSwitch);
begin
  Var LApplySwitchStyleInfo: TSwitchStyleInfo;
  if not fSwitchStyles.TryGetValue(AName, LApplySwitchStyleInfo) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Style "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit;
  end;

  ASwitch.BeginUpdate;
  try
    {$IF defined(ALDPK)}
    LApplySwitchStyleInfo.ApplyStyleProc(ASwitch, LApplySwitchStyleInfo.DefaultHeight);
    {$ELSE}
    LApplySwitchStyleInfo.ApplyStyleProc(ASwitch, ASwitch.Height);
    {$ENDIF}
  finally
    ASwitch.EndUpdate;
  end;
end;

{*************************************************************************************************}
procedure TALStyleManager.ApplyTrackBarStyle(const AName: String; const ATrackBar: TALCustomTrack);
begin
  Var LApplyTrackBarStyleInfo: TTrackBarStyleInfo;
  if not fTrackBarStyles.TryGetValue(AName, LApplyTrackBarStyleInfo) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Style "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit;
  end;

  ATrackBar.BeginUpdate;
  try
    {$IF defined(ALDPK)}
    LApplyTrackBarStyleInfo.ApplyStyleProc(ATrackBar, LApplyTrackBarStyleInfo.DefaultHeight);
    {$ELSE}
    LApplyTrackBarStyleInfo.ApplyStyleProc(ATrackBar, ATrackBar.Height);
    {$ENDIF}
  finally
    ATrackBar.EndUpdate;
  end;
end;

{***********************************************************************************************************}
procedure TALStyleManager.ApplyRangeTrackBarStyle(const AName: String; const ARangeTrackBar: TALCustomTrack);
begin
  Var LApplyRangeTrackBarStyleInfo: TTrackBarStyleInfo;
  if not fRangeTrackBarStyles.TryGetValue(AName, LApplyRangeTrackBarStyleInfo) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Style "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit;
  end;

  ARangeTrackBar.BeginUpdate;
  try
    {$IF defined(ALDPK)}
    LApplyRangeTrackBarStyleInfo.ApplyStyleProc(ARangeTrackBar, LApplyRangeTrackBarStyleInfo.DefaultHeight);
    {$ELSE}
    LApplyRangeTrackBarStyleInfo.ApplyStyleProc(ARangeTrackBar, ARangeTrackBar.Height);
    {$ENDIF}
  finally
    ARangeTrackBar.EndUpdate;
  end;
end;

{*****************************************************}
function TALStyleManager.GetColorNames: TArray<String>;
begin
  var LArray := FLightColors.ToArray;
  TArray.Sort<TPair<String, TPair<TAlphaColor, integer{SortOrder}>>>(LArray,
    TComparer<TPair<String, TPair<TAlphaColor, integer{SortOrder}>>>.Construct(
                                                                       function(const Left, Right: TPair<String, TPair<TAlphaColor, integer{SortOrder}>>): Integer
                                                                       begin
                                                                         Result := Left.value.Value - Right.value.value;
                                                                       end));
  SetLength(Result, Length(LArray));
  for var I := low(LArray) to High(LArray) do
    Result[I] := LArray[I].Key;
end;

{*********************************************************}
function TALStyleManager.GetTextStyleNames: TArray<String>;
begin
  var LArray := FTextStyles.ToArray;
  TArray.Sort<TPair<String, TTextStyleInfo>>(LArray,
    TComparer<TPair<String, TTextStyleInfo>>.Construct(
      function(const Left, Right: TPair<String, TTextStyleInfo>): Integer
      begin
        Result := Left.value.SortOrder - Right.value.SortOrder;
      end));
  SetLength(Result, Length(LArray));
  for var I := low(LArray) to High(LArray) do
    Result[I] := LArray[I].Key;
end;

{*********************************************************}
function TALStyleManager.GetEditStyleNames: TArray<String>;
begin
  var LArray := FEditStyles.ToArray;
  TArray.Sort<TPair<String, TEditStyleInfo>>(LArray,
    TComparer<TPair<String, TEditStyleInfo>>.Construct(
      function(const Left, Right: TPair<String, TEditStyleInfo>): Integer
      begin
        Result := Left.value.SortOrder - Right.value.SortOrder;
      end));
  SetLength(Result, Length(LArray));
  for var I := low(LArray) to High(LArray) do
    Result[I] := LArray[I].Key;
end;

{*********************************************************}
function TALStyleManager.GetMemoStyleNames: TArray<String>;
begin
  var LArray := FMemoStyles.ToArray;
  TArray.Sort<TPair<String, TEditStyleInfo>>(LArray,
    TComparer<TPair<String, TEditStyleInfo>>.Construct(
      function(const Left, Right: TPair<String, TEditStyleInfo>): Integer
      begin
        Result := Left.value.SortOrder - Right.value.SortOrder;
      end));
  SetLength(Result, Length(LArray));
  for var I := low(LArray) to High(LArray) do
    Result[I] := LArray[I].Key;
end;

{***********************************************************}
function TALStyleManager.GetButtonStyleNames: TArray<String>;
begin
  var LArray := FButtonStyles.ToArray;
  TArray.Sort<TPair<String, TButtonStyleInfo>>(LArray,
    TComparer<TPair<String, TButtonStyleInfo>>.Construct(
      function(const Left, Right: TPair<String, TButtonStyleInfo>): Integer
      begin
        Result := Left.value.SortOrder - Right.value.SortOrder;
      end));
  SetLength(Result, Length(LArray));
  for var I := low(LArray) to High(LArray) do
    Result[I] := LArray[I].Key;
end;

{*************************************************************}
function TALStyleManager.GetCheckBoxStyleNames: TArray<String>;
begin
  var LArray := FCheckBoxStyles.ToArray;
  TArray.Sort<TPair<String, TCheckBoxStyleInfo>>(LArray,
    TComparer<TPair<String, TCheckBoxStyleInfo>>.Construct(
      function(const Left, Right: TPair<String, TCheckBoxStyleInfo>): Integer
      begin
        Result := Left.value.SortOrder - Right.value.SortOrder;
      end));
  SetLength(Result, Length(LArray));
  for var I := low(LArray) to High(LArray) do
    Result[I] := LArray[I].Key;
end;

{****************************************************************}
function TALStyleManager.GetRadioButtonStyleNames: TArray<String>;
begin
  var LArray := FRadioButtonStyles.ToArray;
  TArray.Sort<TPair<String, TRadioButtonStyleInfo>>(LArray,
    TComparer<TPair<String, TRadioButtonStyleInfo>>.Construct(
      function(const Left, Right: TPair<String, TRadioButtonStyleInfo>): Integer
      begin
        Result := Left.value.SortOrder - Right.value.SortOrder;
      end));
  SetLength(Result, Length(LArray));
  for var I := low(LArray) to High(LArray) do
    Result[I] := LArray[I].Key;
end;

{***********************************************************}
function TALStyleManager.GetSwitchStyleNames: TArray<String>;
begin
  var LArray := FSwitchStyles.ToArray;
  TArray.Sort<TPair<String, TSwitchStyleInfo>>(LArray,
    TComparer<TPair<String, TSwitchStyleInfo>>.Construct(
      function(const Left, Right: TPair<String, TSwitchStyleInfo>): Integer
      begin
        Result := Left.value.SortOrder - Right.value.SortOrder;
      end));
  SetLength(Result, Length(LArray));
  for var I := low(LArray) to High(LArray) do
    Result[I] := LArray[I].Key;
end;

{*************************************************************}
function TALStyleManager.GetTrackBarStyleNames: TArray<String>;
begin
  var LArray := FTrackBarStyles.ToArray;
  TArray.Sort<TPair<String, TTrackBarStyleInfo>>(LArray,
    TComparer<TPair<String, TTrackBarStyleInfo>>.Construct(
      function(const Left, Right: TPair<String, TTrackBarStyleInfo>): Integer
      begin
        Result := Left.value.SortOrder - Right.value.SortOrder;
      end));
  SetLength(Result, Length(LArray));
  for var I := low(LArray) to High(LArray) do
    Result[I] := LArray[I].Key;
end;

{******************************************************************}
function TALStyleManager.GetRangeTrackBarStyleNames: TArray<String>;
begin
  var LArray := FRangeTrackBarStyles.ToArray;
  TArray.Sort<TPair<String, TTrackBarStyleInfo>>(LArray,
    TComparer<TPair<String, TTrackBarStyleInfo>>.Construct(
      function(const Left, Right: TPair<String, TTrackBarStyleInfo>): Integer
      begin
        Result := Left.value.SortOrder - Right.value.SortOrder;
      end));
  SetLength(Result, Length(LArray));
  for var I := low(LArray) to High(LArray) do
    Result[I] := LArray[I].Key;
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Styles','initialization');
  {$ENDIF}
  TALStyleManager.FInstance := nil;
  TALStyleManager.CreateInstanceFunc := @TALStyleManager.CreateInstance;
  TALStyleManager.FSortOrderCounter := 0;

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Styles','finalization');
  {$ENDIF}
  ALFreeAndNil(TALStyleManager.FInstance);

end.
