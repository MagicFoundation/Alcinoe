unit Alcinoe.FMX.Styles;

interface

{$I Alcinoe.inc}

uses
  System.Generics.Collections,
  System.UITypes,
  FMX.Controls,
  Fmx.forms,
  Alcinoe.FMX.Layouts,
  Alcinoe.fmx.Dialogs,
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
      // --------------
      // TTextStyleInfo
      TTextApplyStyleProc = Procedure(const AText: TALBaseText; const ARatio: Single = 1);
      TTextStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TTextApplyStyleProc;
        DefaultFontSize: Single;
        constructor create(const AApplyStyleProc: TTextApplyStyleProc; const ADefaultFontSize: Single);
      end;
      // --------------
      // TEditStyleInfo
      TEditApplyStyleProc = Procedure(const AEdit: TALBaseEdit; const ARatio: Single = 1);
      TEditStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TEditApplyStyleProc;
        DefaultFontSize: Single;
        constructor create(const AApplyStyleProc: TEditApplyStyleProc; const ADefaultFontSize: Single);
      end;
      // ----------------
      // TButtonStyleInfo
      TButtonApplyStyleProc = Procedure(const AButton: TALButton; const ARatio: Single = 1);
      TButtonStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TButtonApplyStyleProc;
        DefaultFontSize: Single;
        DefaultHeight: Single;
        constructor create(const AApplyStyleProc: TButtonApplyStyleProc; const ADefaultFontSize: Single; const ADefaultHeight: Single);
      end;
      // ------------------
      // TCheckBoxStyleInfo
      TCheckBoxApplyStyleProc = Procedure(const ACheckBox: TALBaseCheckBox; const ARatio: Single = 1);
      TCheckBoxStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TCheckBoxApplyStyleProc;
        DefaultHeight: Single;
        constructor create(const AApplyStyleProc: TCheckBoxApplyStyleProc; const ADefaultHeight: Single);
      end;
      // ---------------------
      // TRadioButtonStyleInfo
      TRadioButtonApplyStyleProc = Procedure(const ARadioButton: TALRadioButton; const ARatio: Single = 1);
      TRadioButtonStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TRadioButtonApplyStyleProc;
        DefaultHeight: Single;
        constructor create(const AApplyStyleProc: TRadioButtonApplyStyleProc; const ADefaultHeight: Single);
      end;
      // ----------------
      // TSwitchStyleInfo
      TSwitchApplyStyleProc = Procedure(const ASwitch: TALSwitch; const ARatio: Single = 1);
      TSwitchStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TSwitchApplyStyleProc;
        DefaultHeight: Single;
        constructor create(const AApplyStyleProc: TSwitchApplyStyleProc; const ADefaultHeight: Single);
      end;
      // ------------------
      // TTrackBarStyleInfo
      TTrackBarApplyStyleProc = Procedure(const ATrackBar: TALCustomTrack; const ARatio: Single = 1);
      TTrackBarStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TTrackBarApplyStyleProc;
        DefaultSize: Single;
        constructor create(const AApplyStyleProc: TTrackBarApplyStyleProc; const ADefaultSize: Single);
      end;
      // ------------------
      // TScrollBarStyleInfo
      TScrollBarApplyStyleProc = Procedure(const AScrollBar: TALCustomScrollBar; const ARatio: Single = 1);
      TScrollBarStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TScrollBarApplyStyleProc;
        DefaultSize: Single;
        constructor create(const AApplyStyleProc: TScrollBarApplyStyleProc; const ADefaultSize: Single);
      end;
      // ------------------
      // TScrollBoxStyleInfo
      TScrollBoxApplyStyleProc = Procedure(const AScrollBox: TALCustomScrollBox);
      TScrollBoxStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TScrollBoxApplyStyleProc;
        constructor create(const AApplyStyleProc: TScrollBoxApplyStyleProc);
      end;
      // -----------------------
      // TDialogManagerStyleInfo
      TDialogManagerApplyStyleProc = Procedure(const ADialogManager: TALDialogManager; const ARatio: Single = 1);
      TDialogManagerStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TDialogManagerApplyStyleProc;
        DefaultFontSize: Single;
        constructor create(const AApplyStyleProc: TDialogManagerApplyStyleProc; const ADefaultFontSize: Single);
      end;
      // ----------------
      // TDialogStyleInfo
      TDialogApplyStyleProc = Procedure(const ADialog: TALDialog; const ARatio: Single = 1);
      TDialogStyleInfo = record
      public
        SortOrder: Integer;
        ApplyStyleProc: TDialogApplyStyleProc;
        DefaultFontSize: Single;
        constructor create(const AApplyStyleProc: TDialogApplyStyleProc; const ADefaultFontSize: Single);
      end;
  private
    FLightColors: TDictionary<string, TPair<TAlphaColor, integer{SortOrder}>>;
    FDarkColors: TDictionary<string, TPair<TAlphaColor, integer{SortOrder}>>;
    FColors: TDictionary<string, TAlphaColor>;
    FFontFamilies: TDictionary<string, string>;
    FTextStyles: TDictionary<String, TTextStyleInfo>;
    FEditStyles: TDictionary<String, TEditStyleInfo>;
    FMemoStyles: TDictionary<String, TEditStyleInfo>;
    FButtonStyles: TDictionary<String, TButtonStyleInfo>;
    FCheckBoxStyles: TDictionary<String, TCheckBoxStyleInfo>;
    FRadioButtonStyles: TDictionary<String, TRadioButtonStyleInfo>;
    FSwitchStyles: TDictionary<String, TSwitchStyleInfo>;
    FTrackBarStyles: TDictionary<String, TTrackBarStyleInfo>;
    FRangeTrackBarStyles: TDictionary<String, TTrackBarStyleInfo>;
    FScrollBarStyles: TDictionary<String, TScrollBarStyleInfo>;
    FScrollBoxStyles: TDictionary<String, TScrollBoxStyleInfo>;
    FDialogManagerStyles: TDictionary<String, TDialogManagerStyleInfo>;
    FDialogStyles: TDictionary<String, TDialogStyleInfo>;
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
    procedure AddOrSetFontFamily(const AName: String; Const AValue: String);
    procedure AddOrSetTextStyle(const AName: String; const AApplyStyleProc: TTextApplyStyleProc; const ADefaultFontSize: Single);
    procedure AddOrSetEditStyle(const AName: String; const AApplyStyleProc: TEditApplyStyleProc; const ADefaultFontSize: Single);
    procedure AddOrSetMemoStyle(const AName: String; const AApplyStyleProc: TEditApplyStyleProc; const ADefaultFontSize: Single);
    procedure AddOrSetButtonStyle(const AName: String; const AApplyStyleProc: TButtonApplyStyleProc; const ADefaultFontSize: Single);
    procedure AddOrSetButtonIconStyle(const AName: String; const AApplyStyleProc: TButtonApplyStyleProc; const ADefaultHeight: Single);
    procedure AddOrSetCheckBoxStyle(const AName: String; const AApplyStyleProc: TCheckBoxApplyStyleProc; const ADefaultHeight: Single);
    procedure AddOrSetRadioButtonStyle(const AName: String; const AApplyStyleProc: TRadioButtonApplyStyleProc; const ADefaultHeight: Single);
    procedure AddOrSetSwitchStyle(const AName: String; const AApplyStyleProc: TSwitchApplyStyleProc; const ADefaultHeight: Single);
    procedure AddOrSetTrackBarStyle(const AName: String; const AApplyStyleProc: TTrackBarApplyStyleProc; const ADefaultSize: Single);
    procedure AddOrSetRangeTrackBarStyle(const AName: String; const AApplyStyleProc: TTrackBarApplyStyleProc; const ADefaultSize: Single);
    procedure AddOrSetScrollBarStyle(const AName: String; const AApplyStyleProc: TScrollBarApplyStyleProc; const ADefaultSize: Single);
    procedure AddOrSetScrollBoxStyle(const AName: String; const AApplyStyleProc: TScrollBoxApplyStyleProc);
    procedure AddOrSetDialogManagerStyle(const AName: String; const AApplyStyleProc: TDialogManagerApplyStyleProc; const ADefaultFontSize: Single);
    procedure AddOrSetDialogStyle(const AName: String; const AApplyStyleProc: TDialogApplyStyleProc; const ADefaultFontSize: Single);
    //--
    function GetColor(const AName: String): TAlphaColor;
    function GetFontFamily(const AName: String): String;
    procedure ApplyTextStyle(const AName: String; const AText: TALText; const AFontSize: Single); overload;
    procedure ApplyTextStyle(const AName: String; const AText: TALText); overload;
    procedure ApplyEditStyle(const AName: String; const AEdit: TALBaseEdit; const AFontSize: Single); overload;
    procedure ApplyEditStyle(const AName: String; const AEdit: TALBaseEdit); overload;
    procedure ApplyMemoStyle(const AName: String; const AMemo: TALBaseEdit; const AFontSize: Single); overload;
    procedure ApplyMemoStyle(const AName: String; const AMemo: TALBaseEdit); overload;
    procedure ApplyButtonStyle(const AName: String; const AButton: TALButton; const AFontSize: Single); overload;
    procedure ApplyButtonIconStyle(const AName: String; const AButton: TALButton; const AHeight: Single);
    procedure ApplyButtonStyle(const AName: String; const AButton: TALButton); overload;
    procedure ApplyCheckBoxStyle(const AName: String; const ACheckBox: TALCheckBox; const AHeight: Single); overload;
    procedure ApplyCheckBoxStyle(const AName: String; const ACheckBox: TALCheckBox); overload;
    procedure ApplyRadioButtonStyle(const AName: String; const ARadioButton: TALRadioButton; const AHeight: Single); overload;
    procedure ApplyRadioButtonStyle(const AName: String; const ARadioButton: TALRadioButton); overload;
    procedure ApplySwitchStyle(const AName: String; const ASwitch: TALSwitch; const AHeight: Single); overload;
    procedure ApplySwitchStyle(const AName: String; const ASwitch: TALSwitch); overload;
    procedure ApplyTrackBarStyle(const AName: String; const ATrackBar: TALCustomTrack; const ASize: Single); overload;
    procedure ApplyTrackBarStyle(const AName: String; const ATrackBar: TALCustomTrack); overload;
    procedure ApplyRangeTrackBarStyle(const AName: String; const ARangeTrackBar: TALCustomTrack; const ASize: Single); overload;
    procedure ApplyRangeTrackBarStyle(const AName: String; const ARangeTrackBar: TALCustomTrack); overload;
    procedure ApplyScrollBarStyle(const AName: String; const AScrollBar: TALCustomScrollBar; const ASize: Single); overload;
    procedure ApplyScrollBarStyle(const AName: String; const AScrollBar: TALCustomScrollBar); overload;
    procedure ApplyScrollBoxStyle(const AName: String; const AScrollBox: TALCustomScrollBox);
    procedure ApplyDialogManagerStyle(const AName: String; const ADialogManager: TALDialogManager; const AFontSize: Single); overload;
    procedure ApplyDialogManagerStyle(const AName: String; const ADialogManager: TALDialogManager); overload;
    procedure ApplyDialogStyle(const AName: String; const ADialog: TALDialog; const AFontSize: Single); overload;
    procedure ApplyDialogStyle(const AName: String; const ADialog: TALDialog); overload;
    //--
    function GetColorNames: TArray<String>;
    function GetFontFamilyNames: TArray<String>;
    function GetTextStyleNames: TArray<String>;
    function GetEditStyleNames: TArray<String>;
    function GetMemoStyleNames: TArray<String>;
    function GetButtonStyleNames: TArray<String>;
    function GetCheckBoxStyleNames: TArray<String>;
    function GetRadioButtonStyleNames: TArray<String>;
    function GetSwitchStyleNames: TArray<String>;
    function GetTrackBarStyleNames: TArray<String>;
    function GetRangeTrackBarStyleNames: TArray<String>;
    function GetScrollBarStyleNames: TArray<String>;
    function GetScrollBoxStyleNames: TArray<String>;
    function GetDialogManagerStyleNames: TArray<String>;
    function GetDialogStyleNames: TArray<String>;
  end;

implementation

uses
  System.Generics.Defaults,
  System.SysUtils,
  System.Types,
  System.Math,
  System.math.Vectors,
  FMX.Platform,
  FMX.types,
  FMX.Graphics,
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
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.Helpers,
  iOSapi.UIKit,
  Alcinoe.iOSapi.UIKit,
  {$ENDIF}
  {$IF defined(ALDPK)}
  Vcl.Dialogs,
  {$ENDIF}
  Alcinoe.Common,
  Alcinoe.StringUtils,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Memo,
  Alcinoe.FMX.UserPreferences,
  Alcinoe.FMX.Graphics;

{**********************************************************************************************************}
function ALGetStyleRatio(const ACaption: String; const AValue: Single; const ADefaultValue: Single): Single;
begin
  if ADefaultValue = 0 then exit(1);
  var LValueF: Single := AValue;
  {$IF defined(ALDPK)}
  While True do begin
    var LValueStr := InputBox(ACaption, '', ALFloatToStrW(ADefaultValue, ALDefaultFormatSettingsW));
    if ALTryStrToFloat(LValueStr,LValueF,ALDefaultFormatSettingsW) then break;
  end;
  {$ENDIF}
  Result := LValueF / ADefaultValue;
end;

{***********************************************************************}
function ALEstimateLineHeightMultiplier(Const AFontSize: Single): Single;
begin
  // Excellent reference:
  // https://pimpmytype.com/line-length-line-height
  // There is no universal rule for ideal line height — it depends on many factors
  // such as line length, typeface, font size, device, and context.
  // This function provides an approximate estimation only.
  case round(AFontSize) of
    10: Result := 1.33;
    11: Result := 1.33; // 16/12 - https://m3.material.io/styles/typography/type-scale-tokens#a734c6ed-634c-4abb-adb2-35daf0aed06a
    12: Result := 1.33; // 16/12 - https://m3.material.io/styles/typography/type-scale-tokens#a734c6ed-634c-4abb-adb2-35daf0aed06a
    13: Result := 1.50;
    14: Result := 1.43; // 20/14 - https://m3.material.io/styles/typography/type-scale-tokens#a734c6ed-634c-4abb-adb2-35daf0aed06a
    15: Result := 1.48;
    16: Result := 1.50; // 24/16 - https://m3.material.io/styles/typography/type-scale-tokens#a734c6ed-634c-4abb-adb2-35daf0aed06a
    17: Result := 1.45;
    18: result := 1.42;
    19: Result := 1.40;
    20: Result := 1.37;
    21: Result := 1.32;
    22: result := 1.27; // 28/22 - https://m3.material.io/styles/typography/type-scale-tokens#a734c6ed-634c-4abb-adb2-35daf0aed06a
    23: Result := 1.33;
    24: result := 1.33; // 32/24 - https://m3.material.io/styles/typography/type-scale-tokens#a734c6ed-634c-4abb-adb2-35daf0aed06a
    else
      Result := 0;
  end;
end;

/////////////
// CONTROL //
/////////////

{**********************************************************************************}
procedure ALResetControlStyle(const AControl: TALControl; const ARatio: Single = 1);
begin
  //With AControl do begin
    //BeginUpdate;
    //Try
      // --TALControl
      //Align
      //AutoAlignToPixel
      //AutoSize
      //DoubleBuffered
      //Pivot
      //Scale
      // --TControl
      //Anchors
      //CanFocus
      //CanParentFocus
      //ClipChildren
      //ClipParent
      //Cursor
      //DisabledOpacity
      //DragMode
      //EnableDragHighlight
      //Enabled
      //Hint
      //HitTest
      //Locked
      //Margins
      //Opacity
      //Padding
      //ParentShowHint
      //Position
      //RotationAngle
      //ShowHint
      //Size
      //StyleName
      //TabOrder
      //TabStop
      //Tag
      //TagFloat
      //TagObject
      //TagString
      //TouchTargetExpansion
      //Visible
    //Finally
      //EndUpdate;
    //End;
  //end;
end;

///////////
// SHAPE //
///////////

{****************************************************************************}
procedure ALResetShapeStyle(const AShape: TALShape; const ARatio: Single = 1);
begin
  With AShape do begin
    BeginUpdate;
    Try
      ALResetControlStyle(AShape, ARatio);
      Fill.Reset;
      Stroke.Reset;
      Stroke.Thickness := RoundTo(Stroke.DefaultThickness * ARatio, -2);
      Shadow.Reset;
    Finally
      EndUpdate;
    End;
  end;
end;

///////////////
// RECTANGLE //
///////////////

{********************************************************************************************}
procedure ALResetRectangleStyle(const ARectangle: TALBaseRectangle; const ARatio: Single = 1);
begin
  With ARectangle do begin
    BeginUpdate;
    Try
      ALResetShapeStyle(ARectangle, ARatio);
      //DoubleBuffered
      XRadius := ARectangle.DefaultXRadius;
      YRadius := ARectangle.DefaultYRadius;
      if XRadius > 0 then XRadius := RoundTo(XRadius * ARatio, -2);
      if YRadius > 0 then YRadius := RoundTo(YRadius * ARatio, -2);
      Corners := ARectangle.DefaultCorners;
      Sides := ARectangle.DefaultSides;
      //CacheIndex
      //CacheEngine
    Finally
      EndUpdate;
    End;
  end;
end;

////////////
// LAYOUT //
////////////

{*******************************************************************************}
procedure ALResetLayoutStyle(const ALayout: TALLayout; const ARatio: Single = 1);
begin
  //With ALayout do begin
    //BeginUpdate;
    //Try
      //ALResetControlStyle(AShape, ARatio);
    //Finally
      //EndUpdate;
    //End;
  //end;
end;

///////////
// IMAGE //
///////////

{****************************************************************************}
procedure ALResetImageStyle(const AImage: TALImage; const ARatio: Single = 1);
begin
  With AImage do begin
    BeginUpdate;
    Try
      ALResetControlStyle(AImage, ARatio);
      BackgroundColor := DefaultBackgroundColor;
      BackgroundColorKey := DefaultBackgroundColorKey;
      LoadingColor := DefaultLoadingColor;
      LoadingColorKey := DefaultLoadingColorKey;
      //ResourceName
      //MaskResourceName
      //MaskBitmap
      //ReadyAfterResourcesLoaded
      //WrapMode
      //ExifOrientationInfo
      //RotateAccordingToExifOrientation
      Corners := DefaultCorners;
      Sides := DefaultSides;
      XRadius := DefaultXRadius;
      YRadius := DefaultYRadius;
      if XRadius > 0 then XRadius := RoundTo(XRadius * ARatio, -2);
      if YRadius > 0 then YRadius := RoundTo(YRadius * ARatio, -2);
      BlurRadius := DefaultBlurRadius;
      //CacheIndex
      //LoadingCacheIndex
      //CacheEngine
      //CropCenter
      Stroke.Reset;
      Stroke.Thickness := RoundTo(Stroke.DefaultThickness * ARatio, -2);
      Shadow.Reset;
      //FadeInDuration
    Finally
      EndUpdate;
    End;
  end;
end;

//////////
// TEXT //
//////////

Type
  _TALBaseTextProtectedAccess = Class(TALBaseText);

{*****************************************************************************}
procedure ALResetTextStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
  With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      ALResetShapeStyle(AText, ARatio);
      //DoubleBuffered
      //CacheIndex
      //CacheEngine
      //AutoTranslate
      //Text
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Size := RoundTo(TextSettings.Font.DefaultSize * ARatio, -2);
      TextSettings.LetterSpacing := RoundTo(TextSettings.DefaultLetterSpacing * ARatio, -2);
      //MaxWidth
      //MaxHeight
      XRadius := DefaultXRadius;
      YRadius := DefaultYRadius;
      if XRadius > 0 then XRadius := RoundTo(XRadius * ARatio, -2);
      if YRadius > 0 then YRadius := RoundTo(YRadius * ARatio, -2);
      Corners := AllCorners;
      Sides := AllSides;
    Finally
      EndUpdate;
    End;
  end;
end;

{*************************************************************************************************}
procedure ALApplyMaterialTextDisplayLargeStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 57;
      TextSettings.Font.Weight := TFontWeight.Regular;
      TextSettings.LineHeightMultiplier := 64/TextSettings.font.Size;
      TextSettings.LetterSpacing := -0.25;
    finally
      EndUpdate;
    end;
  end;
end;

{**************************************************************************************************}
procedure ALApplyMaterialTextDisplayMediumStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 45;
      TextSettings.Font.Weight := TFontWeight.Regular;
      TextSettings.LineHeightMultiplier := 52/TextSettings.font.Size;
      TextSettings.LetterSpacing := 0;
    finally
      EndUpdate;
    end;
  end;
end;

{*************************************************************************************************}
procedure ALApplyMaterialTextDisplaySmallStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 36;
      TextSettings.Font.Weight := TFontWeight.Regular;
      TextSettings.LineHeightMultiplier := 44/TextSettings.font.Size;
      TextSettings.LetterSpacing := 0;
    finally
      EndUpdate;
    end;
  end;
end;

{**************************************************************************************************}
procedure ALApplyMaterialTextHeadlineLargeStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 32;
      TextSettings.Font.Weight := TFontWeight.Regular;
      TextSettings.LineHeightMultiplier := 40/TextSettings.font.Size;
      TextSettings.LetterSpacing := 0;
    finally
      EndUpdate;
    end;
  end;
end;

{***************************************************************************************************}
procedure ALApplyMaterialTextHeadlineMediumStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 28;
      TextSettings.Font.Weight := TFontWeight.Regular;
      TextSettings.LineHeightMultiplier := 36/TextSettings.font.Size;
      TextSettings.LetterSpacing := 0;
    finally
      EndUpdate;
    end;
  end;
end;

{**************************************************************************************************}
procedure ALApplyMaterialTextHeadlineSmallStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 24;
      TextSettings.Font.Weight := TFontWeight.Regular;
      TextSettings.LineHeightMultiplier := 32/TextSettings.font.Size;
      TextSettings.LetterSpacing := 0;
    finally
      EndUpdate;
    end;
  end;
end;

{***********************************************************************************************}
procedure ALApplyMaterialTextTitleLargeStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 22;
      TextSettings.Font.Weight := TFontWeight.Regular;
      TextSettings.LineHeightMultiplier := 28/TextSettings.font.Size;
      TextSettings.LetterSpacing := 0;
    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************************}
procedure ALApplyMaterialTextTitleMediumStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 16;
      TextSettings.Font.Weight := TFontWeight.Medium;
      TextSettings.LineHeightMultiplier := 24/TextSettings.font.Size;
      TextSettings.LetterSpacing := 0.15;
    finally
      EndUpdate;
    end;
  end;
end;

{***********************************************************************************************}
procedure ALApplyMaterialTextTitleSmallStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 14;
      TextSettings.Font.Weight := TFontWeight.Medium;
      TextSettings.LineHeightMultiplier := 20/TextSettings.font.Size;
      TextSettings.LetterSpacing := 0.1;
    finally
      EndUpdate;
    end;
  end;
end;

{**********************************************************************************************}
procedure ALApplyMaterialTextBodyLargeStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 16;
      TextSettings.Font.Weight := TFontWeight.Regular;
      TextSettings.LineHeightMultiplier := 24/TextSettings.font.Size;
      TextSettings.LetterSpacing := 0.5;
    finally
      EndUpdate;
    end;
  end;
end;

{***********************************************************************************************}
procedure ALApplyMaterialTextBodyMediumStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 14;
      TextSettings.Font.Weight := TFontWeight.Regular;
      TextSettings.LineHeightMultiplier := 20/TextSettings.font.Size;
      TextSettings.LetterSpacing := 0.25;
    finally
      EndUpdate;
    end;
  end;
end;

{**********************************************************************************************}
procedure ALApplyMaterialTextBodySmallStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 12;
      TextSettings.Font.Weight := TFontWeight.Regular;
      TextSettings.LineHeightMultiplier := 16/TextSettings.font.Size;
      TextSettings.LetterSpacing := 0.4;
    finally
      EndUpdate;
    end;
  end;
end;

{***********************************************************************************************}
procedure ALApplyMaterialTextLabelLargeStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 14;
      TextSettings.Font.Weight := TFontWeight.Medium;
      TextSettings.LineHeightMultiplier := 20/TextSettings.font.Size;
      TextSettings.LetterSpacing := 0.1;
    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************************}
procedure ALApplyMaterialTextLabelMediumStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 12;
      TextSettings.Font.Weight := TFontWeight.Medium;
      TextSettings.LineHeightMultiplier := 16/TextSettings.font.Size;
      TextSettings.LetterSpacing := 0.5;
    finally
      EndUpdate;
    end;
  end;
end;

{***********************************************************************************************}
procedure ALApplyMaterialTextLabelSmallStyle(const AText: TALBaseText; const ARatio: Single = 1);
begin
   With _TALBaseTextProtectedAccess(AText) do begin
    BeginUpdate;
    Try
      var LPrevFontColor := TextSettings.Font.Color;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Color := LPrevFontColor;
      TextSettings.Font.Size := 11;
      TextSettings.Font.Weight := TFontWeight.Medium;
      TextSettings.LineHeightMultiplier := 16/TextSettings.font.Size;
      TextSettings.LetterSpacing := 0.5;
    finally
      EndUpdate;
    end;
  end;
end;

//////////
// EDIT //
//////////

{*****************************************************************************}
procedure ALResetEditStyle(const AEdit: TALBaseEdit; const ARatio: Single = 1);
begin
  With AEdit do begin
    BeginUpdate;
    Try
      ALResetRectangleStyle(AEdit, ARatio);
      if AEdit is TALEdit then TALEdit(AEdit).AutoSize := True
      else if AEdit is TALMemo then TALMemo(AEdit).AutoSizeLineCount := 0;
      //Margins.Rect := ALScaleRect(Margins.DefaultValue, ARatio).RoundTo(-2);
      Padding.Rect := ALScaleRect(Padding.DefaultValue, ARatio).RoundTo(-2);
      TouchTargetExpansion.Rect := ALScaleRect(TouchTargetExpansion.DefaultValue, ARatio).RoundTo(-2);
      TintColor := TalphaColors.null;
      TintColorKey := '';
      PromptTextcolor := TAlphaColors.null;
      PromptTextcolorKey := '';
      DefStyleAttr := '';
      DefStyleRes := '';
      //--
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Size := RoundTo(TextSettings.Font.DefaultSize * ARatio, -2);
      TextSettings.LetterSpacing := RoundTo(TextSettings.DefaultLetterSpacing * ARatio, -2);
      //--
      LPrevIsHtml := LabelTextSettings.IsHtml;
      LabelTextSettings.Reset;
      LabelTextSettings.IsHtml := LPrevIsHtml;
      LabelTextSettings.Font.Size := RoundTo(LabelTextSettings.Font.DefaultSize * ARatio, -2);
      LabelTextSettings.LetterSpacing := RoundTo(LabelTextSettings.DefaultLetterSpacing * ARatio, -2);
      LabelTextSettings.Margins.Rect := ALScaleRect(LabelTextSettings.Margins.DefaultValue, ARatio).RoundTo(-2);
      //--
      LPrevIsHtml := SupportingTextSettings.IsHtml;
      SupportingTextSettings.Reset;
      SupportingTextSettings.IsHtml := LPrevIsHtml;
      SupportingTextSettings.Font.Size := RoundTo(SupportingTextSettings.Font.DefaultSize * ARatio, -2);
      SupportingTextSettings.LetterSpacing := RoundTo(SupportingTextSettings.DefaultLetterSpacing * ARatio, -2);
      //SupportingTextSettings.Margins.Rect := ALScaleRect(SupportingTextSettings.Margins.DefaultValue, ARatio).RoundTo(-2);
      //--
      StateStyles.Reset;
    Finally
      EndUpdate;
    End;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterialEditFilledStyle(const AEdit: TALBaseEdit; const ARatio: Single = 1);
begin
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditStyle(AEdit, ARatio);
      padding.Rect := TRectF.Create(16*ARatio{Left}, 12*ARatio{Top}, 16*ARatio{Right}, 12*ARatio{Bottom}).RoundTo(-2);
      Corners := [TCorner.TopLeft, Tcorner.TopRight];
      Sides := [TSide.Bottom];
      XRadius := RoundTo(4 * ARatio, -2);
      YRadius := RoundTo(4 * ARatio, -2);
      DefStyleAttr := 'MaterialEditFilled';
      TintColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Fill.ColorKey := 'Material.Color.SurfaceContainerHighest'; // md.sys.color.surface-container-highest / md.ref.palette.neutral90 / #E6E0E9
      Stroke.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      TextSettings.Font.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
      LabelTextSettings.Font.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      LabelTextSettings.Margins.Rect := TRectF.Create(0,-4*ARatio,0,4*ARatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*ARatio,0,0).RoundTo(-2);
      PromptTextColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material.Color.OnSurface.Alpha04'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.PromptTextcolorKey := StateStyles.Disabled.LabelTextSettings.Font.ColorKey;
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * ARatio, -2);
      StateStyles.Focused.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Focused.LabelTextSettings.Inherit := False;
      StateStyles.Focused.LabelTextSettings.Font.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Focused.PromptTextcolorKey := StateStyles.Focused.LabelTextSettings.Font.ColorKey;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#e4964192-72ad-414f-85b4-4b4357abb83c
procedure ALApplyMaterialEditOutlinedStyle(const AEdit: TALBaseEdit; const ARatio: Single = 1);
begin
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditStyle(AEdit, ARatio);
      padding.Rect := TRectF.Create(16*ARatio{Left}, 16*ARatio{Top}, 16*ARatio{Right}, 16*ARatio{Bottom}).RoundTo(-2);
      XRadius := RoundTo(4 * ARatio, -2);
      YRadius := RoundTo(4 * ARatio, -2);
      DefStyleAttr := 'MaterialEditOutlined';
      TintColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Fill.ColorKey := 'Material.Color.Surface'; // md.sys.color.surface / md.ref.palette.neutral98 / #FEF7FF
      Stroke.ColorKey := 'Material.Color.Outline'; // md.sys.color.outline / md.ref.palette.neutral-variant50 / #79747E
      TextSettings.Font.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.floating;
      LabelTextSettings.Font.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      LabelTextSettings.Margins.Rect := TRectF.Create(0,0,0,-6*ARatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*ARatio,0,0).RoundTo(-2);
      PromptTextColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.PromptTextcolorKey := StateStyles.Disabled.LabelTextSettings.Font.ColorKey;
      //--Hovered--
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Hovered.PromptTextcolorKey := StateStyles.Hovered.LabelTextSettings.Font.ColorKey;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * ARatio, -2);
      StateStyles.Focused.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Focused.LabelTextSettings.Inherit := False;
      StateStyles.Focused.LabelTextSettings.Font.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Focused.PromptTextcolorKey := StateStyles.Focused.LabelTextSettings.Font.ColorKey;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterialEditHybridStyle(const AEdit: TALBaseEdit; const ARatio: Single = 1);
begin
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditStyle(AEdit, ARatio);
      padding.Rect := TRectF.Create(16*ARatio{Left}, 16*ARatio{Top}, 16*ARatio{Right}, 16*ARatio{Bottom}).RoundTo(-2);
      XRadius := RoundTo(4 * ARatio, -2);
      YRadius := RoundTo(4 * ARatio, -2);
      DefStyleAttr := 'MaterialEditOutlined';
      TintColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Fill.ColorKey := 'Material.Color.Surface'; // md.sys.color.surface / md.ref.palette.neutral98 / #FEF7FF
      Stroke.ColorKey := 'Material.Color.Outline'; // md.sys.color.outline / md.ref.palette.neutral-variant50 / #79747E
      TextSettings.Font.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
      LabelTextSettings.Font.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      LabelTextSettings.Margins.Rect := TRectF.Create(0,-4*ARatio,0,4*ARatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*ARatio,0,0).RoundTo(-2);
      PromptTextColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.PromptTextcolorKey := StateStyles.Disabled.LabelTextSettings.Font.ColorKey;
      //--Hovered--
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Hovered.PromptTextcolorKey := StateStyles.Hovered.LabelTextSettings.Font.ColorKey;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * ARatio, -2);
      StateStyles.Focused.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Focused.LabelTextSettings.Inherit := False;
      StateStyles.Focused.LabelTextSettings.Font.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Focused.PromptTextcolorKey := StateStyles.Focused.LabelTextSettings.Font.ColorKey;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterialEditFilledErrorStyle(const AEdit: TALBaseEdit; const ARatio: Single = 1);
begin
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditStyle(AEdit, ARatio);
      padding.Rect := TRectF.Create(16*ARatio{Left}, 12*ARatio{Top}, 16*ARatio{Right}, 12*ARatio{Bottom}).RoundTo(-2);
      Corners := [TCorner.TopLeft, Tcorner.TopRight];
      Sides := [TSide.Bottom];
      XRadius := RoundTo(4 * ARatio, -2);
      YRadius := RoundTo(4 * ARatio, -2);
      DefStyleAttr := 'MaterialEditFilledError';
      TintColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      Fill.ColorKey := 'Material.Color.SurfaceContainerHighest'; // md.sys.color.surface-container-highest / md.ref.palette.neutral90 / #E6E0E9
      Stroke.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      TextSettings.Font.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
      LabelTextSettings.Font.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      LabelTextSettings.Margins.Rect := TRectF.Create(0,-4*ARatio,0,4*ARatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*ARatio,0,0).RoundTo(-2);
      PromptTextColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material.Color.OnSurface.Alpha04'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.PromptTextcolorKey := StateStyles.Disabled.LabelTextSettings.Font.ColorKey;
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.ColorKey := 'Material.Color.OnErrorContainer'; // md.sys.color.on-error-container / md.ref.palette.error30 / #8C1D18
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.ColorKey := 'Material.Color.OnErrorContainer'; // md.sys.color.on-error-container / md.ref.palette.error30 / #8C1D18
      StateStyles.Hovered.PromptTextcolorKey := StateStyles.Hovered.LabelTextSettings.Font.ColorKey;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * ARatio, -2);
      StateStyles.Focused.PromptTextcolorKey := LabelTextSettings.Font.ColorKey;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#e4964192-72ad-414f-85b4-4b4357abb83c
procedure ALApplyMaterialEditOutlinedErrorStyle(const AEdit: TALBaseEdit; const ARatio: Single = 1);
begin
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditStyle(AEdit, ARatio);
      padding.Rect := TRectF.Create(16*ARatio{Left}, 16*ARatio{Top}, 16*ARatio{Right}, 16*ARatio{Bottom}).RoundTo(-2);
      XRadius := RoundTo(4 * ARatio, -2);
      YRadius := RoundTo(4 * ARatio, -2);
      DefStyleAttr := 'MaterialEditOutlinedError';
      TintColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      Fill.ColorKey := 'Material.Color.Surface'; // md.sys.color.surface / md.ref.palette.neutral98 / #FEF7FF
      Stroke.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      TextSettings.Font.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.floating;
      LabelTextSettings.Font.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      LabelTextSettings.Margins.Rect := TRectF.Create(0,0,0,-6*ARatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*ARatio,0,0).RoundTo(-2);
      PromptTextColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.PromptTextcolorKey := StateStyles.Disabled.LabelTextSettings.Font.ColorKey;
      //--Hovered--
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.ColorKey := 'Material.Color.OnErrorContainer'; // md.sys.color.on-error-container / md.ref.palette.error30 / #8C1D18
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.ColorKey := 'Material.Color.OnErrorContainer'; // md.sys.color.on-error-container / md.ref.palette.error30 / #8C1D18
      StateStyles.Hovered.PromptTextcolorKey := StateStyles.Hovered.LabelTextSettings.Font.ColorKey;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * ARatio, -2);
      StateStyles.Focused.PromptTextcolorKey := LabelTextSettings.Font.ColorKey;

    finally
      EndUpdate;
    end;
  end;
end;

{****************************************************************************************}
//https://m3.material.io/components/text-fields/specs#f967d3f6-0139-43f7-8336-510022684fd1
procedure ALApplyMaterialEditHybridErrorStyle(const AEdit: TALBaseEdit; const ARatio: Single = 1);
begin
  With AEdit do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetEditStyle(AEdit, ARatio);
      padding.Rect := TRectF.Create(16*ARatio{Left}, 16*ARatio{Top}, 16*ARatio{Right}, 16*ARatio{Bottom}).RoundTo(-2);
      XRadius := RoundTo(4 * ARatio, -2);
      YRadius := RoundTo(4 * ARatio, -2);
      DefStyleAttr := 'MaterialEditOutlinedError';
      TintColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      Fill.ColorKey := 'Material.Color.Surface'; // md.sys.color.surface / md.ref.palette.neutral98 / #FEF7FF
      Stroke.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      TextSettings.Font.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      LabelTextSettings.Layout := TALEdit.TLabelTextLayout.Inline;
      LabelTextSettings.Font.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      LabelTextSettings.Margins.Rect := TRectF.Create(0,-4*ARatio,0,4*ARatio).RoundTo(-2);
      SupportingTextSettings.Layout := TALEdit.TSupportingTextLayout.Inline;
      SupportingTextSettings.Font.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      SupportingTextSettings.Margins.Rect := TRectF.Create(0,4*ARatio,0,0).RoundTo(-2);
      PromptTextColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Disabled.LabelTextSettings.Inherit := False;
      StateStyles.Disabled.LabelTextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.SupportingTextSettings.Assign(SupportingTextSettings);
      StateStyles.Disabled.SupportingTextSettings.Inherit := False;
      StateStyles.Disabled.SupportingTextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.PromptTextcolorKey := StateStyles.Disabled.LabelTextSettings.Font.ColorKey;
      //--Hovered--
      StateStyles.Hovered.Stroke.assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.ColorKey := 'Material.Color.OnErrorContainer'; // md.sys.color.on-error-container / md.ref.palette.error30 / #8C1D18
      StateStyles.Hovered.LabelTextSettings.Assign(LabelTextSettings);
      StateStyles.Hovered.LabelTextSettings.Inherit := False;
      StateStyles.Hovered.LabelTextSettings.Font.ColorKey := 'Material.Color.OnErrorContainer'; // md.sys.color.on-error-container / md.ref.palette.error30 / #8C1D18
      StateStyles.Hovered.PromptTextcolorKey := StateStyles.Hovered.LabelTextSettings.Font.ColorKey;
      //--Focused--
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.Inherit := False;
      StateStyles.Focused.Stroke.Thickness := RoundTo(3 * ARatio, -2);
      StateStyles.Focused.PromptTextcolorKey := LabelTextSettings.Font.ColorKey;

    finally
      EndUpdate;
    end;
  end;
end;

////////////
// BUTTON //
////////////

{*******************************************************************************}
procedure ALResetButtonStyle(const AButton: TALButton; const ARatio: Single = 1);
begin
  With AButton do begin
    BeginUpdate;
    Try
      ALResetTextStyle(AButton, ARatio);
      AutoSize := True;
      //Margins.Rect := ALScaleRect(Margins.DefaultValue, ARatio).RoundTo(-2);
      Padding.Rect := ALScaleRect(Padding.DefaultValue, ARatio).RoundTo(-2);
      TouchTargetExpansion.Rect := ALScaleRect(TouchTargetExpansion.DefaultValue, ARatio).RoundTo(-2);
      StateStyles.Reset;
    Finally
      EndUpdate;
    End;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#cbfd91a6-d688-4be7-9a69-672549de3ea9
procedure ALApplyMaterialButtonFilledStyle(const AButton: TALButton; const ARatio: Single = 1);
begin
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonStyle(AButton, ARatio);
      padding.Rect := TRectF.Create(24*ARatio{Left}, 12*ARatio{Top}, 24*ARatio{Right}, 12*ARatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Stroke.Color := Talphacolors.Null;
      Stroke.ColorKey := '';
      TextSettings.Font.ColorKey := 'Material.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF
      TextSettings.LetterSpacing := RoundTo(0.1 * ARatio, -2);
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.StateLayer.XRadius := -50;
      StateStyles.Hovered.StateLayer.YRadius := -50;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.ColorKey := 'Material.Color.Shadow.Alpha50'; // md.sys.color.shadow / md.ref.palette.neutral0 / #000000
      StateStyles.Hovered.Shadow.blur := RoundTo(2 * ARatio, -2);
      StateStyles.Hovered.Shadow.OffsetY := RoundTo(1 * ARatio, -2);
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Pressed.StateLayer.XRadius := -50;
      StateStyles.Pressed.StateLayer.YRadius := -50;
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Focused.StateLayer.XRadius := -50;
      StateStyles.Focused.StateLayer.YRadius := -50;

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#4a0c06da-0b2f-47de-a583-97e0ae80b5a5
procedure ALApplyMaterialButtonOutlinedStyle(const AButton: TALButton; const ARatio: Single = 1);
begin
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonStyle(AButton, ARatio);
      padding.Rect := TRectF.Create(24*ARatio{Left}, 12*ARatio{Top}, 24*ARatio{Right}, 12*ARatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.Color := Talphacolors.Null;
      Fill.ColorKey := '';
      Stroke.ColorKey := 'Material.Color.Outline'; // md.sys.color.outline / md.ref.palette.neutral-variant50 / #79747E
      TextSettings.Font.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      TextSettings.LetterSpacing := RoundTo(0.1 * ARatio, -2);
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Stroke.Assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.StateLayer.XRadius := -50;
      StateStyles.Hovered.StateLayer.YRadius := -50;
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Pressed.StateLayer.XRadius := -50;
      StateStyles.Pressed.StateLayer.YRadius := -50;
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Focused.StateLayer.XRadius := -50;
      StateStyles.Focused.StateLayer.YRadius := -50;
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.inherit := False;
      StateStyles.Focused.Stroke.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#398d84eb-fc8a-4c8a-bfb4-82d2e85dee4d
procedure ALApplyMaterialButtonTextStyle(const AButton: TALButton; const ARatio: Single = 1);
begin
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonStyle(AButton, ARatio);
      padding.Rect := TRectF.Create(12*ARatio{Left}, 12*ARatio{Top}, 12*ARatio{Right}, 12*ARatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.Color := Talphacolors.Null;
      Fill.ColorKey := '';
      Stroke.Color := Talphacolors.Null;
      Stroke.ColorKey := '';
      TextSettings.Font.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      TextSettings.LetterSpacing := RoundTo(0.1 * ARatio, -2);
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.StateLayer.XRadius := -50;
      StateStyles.Hovered.StateLayer.YRadius := -50;
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Pressed.StateLayer.XRadius := -50;
      StateStyles.Pressed.StateLayer.YRadius := -50;
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Focused.StateLayer.XRadius := -50;
      StateStyles.Focused.StateLayer.YRadius := -50;

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#c75be779-5a59-4748-98d4-e47fc888d0b1
procedure ALApplyMaterialButtonElevatedStyle(const AButton: TALButton; const ARatio: Single = 1);
begin
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonStyle(AButton, ARatio);
      padding.Rect := TRectF.Create(24*ARatio{Left}, 12*ARatio{Top}, 24*ARatio{Right}, 12*ARatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.ColorKey := 'Material.Color.SurfaceContainerLow'; // md.sys.color.surface-container-low / md.ref.palette.neutral96 / #F7F2FA
      Stroke.Color := Talphacolors.Null;
      Stroke.ColorKey := '';
      TextSettings.Font.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      TextSettings.LetterSpacing := RoundTo(0.1 * ARatio, -2);
      Shadow.ColorKey := 'Material.Color.Shadow.Alpha50'; // md.sys.color.shadow / md.ref.palette.neutral0 / #000000
      Shadow.blur := RoundTo(2 * ARatio, -2);
      Shadow.OffsetY := RoundTo(1 * ARatio, -2);
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Shadow.inherit := False;
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.StateLayer.XRadius := -50;
      StateStyles.Hovered.StateLayer.YRadius := -50;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.ColorKey := 'Material.Color.Shadow.Alpha50'; // md.sys.color.shadow / md.ref.palette.neutral0 / #000000
      StateStyles.Hovered.Shadow.blur := RoundTo(3 * ARatio, -2);
      StateStyles.Hovered.Shadow.OffsetY := RoundTo(1 * ARatio, -2);
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Pressed.StateLayer.XRadius := -50;
      StateStyles.Pressed.StateLayer.YRadius := -50;
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Focused.StateLayer.XRadius := -50;
      StateStyles.Focused.StateLayer.YRadius := -50;

    finally
      EndUpdate;
    end;
  end;
end;

{************************************************************************************}
//https://m3.material.io/components/buttons/specs#6ce8b926-87c4-4600-9bec-5deb4aaa65d8
procedure ALApplyMaterialButtonTonalStyle(const AButton: TALButton; const ARatio: Single = 1);
begin
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetButtonStyle(AButton, ARatio);
      padding.Rect := TRectF.Create(24*ARatio{Left}, 12*ARatio{Top}, 24*ARatio{Right}, 12*ARatio{Bottom}).RoundTo(-2);
      XRadius := -50;
      YRadius := -50;
      Fill.ColorKey := 'Material.Color.SecondaryContainer'; // md.sys.color.secondary-container / md.ref.palette.secondary90 / #E8DEF8
      Stroke.Color := Talphacolors.Null;
      Stroke.ColorKey := '';
      TextSettings.Font.ColorKey := 'Material.Color.OnSecondaryContainer'; // md.sys.color.on-secondary-container / md.ref.palette.secondary30 / #4A4458
      TextSettings.LetterSpacing := RoundTo(0.1 * ARatio, -2);
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Hovered--
      StateStyles.Hovered.StateLayer.UseContentColor := True;
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.StateLayer.XRadius := -50;
      StateStyles.Hovered.StateLayer.YRadius := -50;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.ColorKey := 'Material.Color.Shadow.Alpha50'; // md.sys.color.shadow / md.ref.palette.neutral0 / #000000
      StateStyles.Hovered.Shadow.blur := RoundTo(2 * ARatio, -2);
      StateStyles.Hovered.Shadow.OffsetY := RoundTo(1 * ARatio, -2);
      //--Pressed--
      StateStyles.Pressed.StateLayer.UseContentColor := True;
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Pressed.StateLayer.XRadius := -50;
      StateStyles.Pressed.StateLayer.YRadius := -50;
      //--Focused--
      StateStyles.Focused.StateLayer.UseContentColor := True;
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Focused.StateLayer.XRadius := -50;
      StateStyles.Focused.StateLayer.YRadius := -50;

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#5309610a-4515-44f9-830d-880e2a2240a2
procedure ALApplyMaterialButtonIconFilledStyle(const AButton: TALButton; const ARatio: Single = 1);
begin
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonStyle(AButton);
      Width := RoundTo(40 * ARatio, -2);
      Height := RoundTo(40 * ARatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * ARatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*ARatio,8*ARatio,8*ARatio,8*ARatio).RoundTo(-2);
      Fill.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Fill.ImageTintColorKey := 'Material.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF
      Fill.ResourceName := LPrevResourceName;
      Stroke.Color := TALphaColors.Null;
      Stroke.ColorKey := '';
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Fill.ImageTintColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      //--Hovered--
      StateStyles.Hovered.StateLayer.ColorKey := 'Material.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.StateLayer.XRadius := -50;
      StateStyles.Hovered.StateLayer.YRadius := -50;
      //--Pressed--
      StateStyles.Pressed.StateLayer.ColorKey := 'Material.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Pressed.StateLayer.XRadius := -50;
      StateStyles.Pressed.StateLayer.YRadius := -50;
      //--Focused--
      StateStyles.Focused.StateLayer.ColorKey := 'Material.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Focused.StateLayer.XRadius := -50;
      StateStyles.Focused.StateLayer.YRadius := -50;

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#ba97cf8a-2112-47dc-af87-2e32aabccdde
procedure ALApplyMaterialButtonIconTonalStyle(const AButton: TALButton; const ARatio: Single = 1);
begin
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonStyle(AButton);
      Width := RoundTo(40 * ARatio, -2);
      Height := RoundTo(40 * ARatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * ARatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*ARatio,8*ARatio,8*ARatio,8*ARatio).RoundTo(-2);
      Fill.ColorKey := 'Material.Color.SecondaryContainer'; // md.sys.color.secondary-container / md.ref.palette.secondary90 / #E8DEF8
      Fill.ImageTintColorKey := 'Material.Color.OnSecondaryContainer'; // md.sys.color.on-secondary-container / md.ref.palette.secondary30 / #4A4458
      Fill.ResourceName := LPrevResourceName;
      Stroke.Color := TALphaColors.Null;
      Stroke.ColorKey := '';
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Fill.ImageTintColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      //--Hovered--
      StateStyles.Hovered.StateLayer.ColorKey := 'Material.Color.OnSecondaryContainer'; // md.sys.color.on-secondary-container / md.ref.palette.secondary30 / #4A4458
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.StateLayer.XRadius := -50;
      StateStyles.Hovered.StateLayer.YRadius := -50;
      //--Pressed--
      StateStyles.Pressed.StateLayer.ColorKey := 'Material.Color.OnSecondaryContainer'; // md.sys.color.on-secondary-container / md.ref.palette.secondary30 / #4A4458
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Pressed.StateLayer.XRadius := -50;
      StateStyles.Pressed.StateLayer.YRadius := -50;
      //--Focused--
      StateStyles.Focused.StateLayer.ColorKey := 'Material.Color.OnSecondaryContainer'; // md.sys.color.on-secondary-container / md.ref.palette.secondary30 / #4A4458
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Focused.StateLayer.XRadius := -50;
      StateStyles.Focused.StateLayer.YRadius := -50;

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#05e02b7f-ebf2-4f02-9709-8230db3702b4
procedure ALApplyMaterialButtonIconOutlinedStyle(const AButton: TALButton; const ARatio: Single = 1);
begin
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonStyle(AButton);
      Width := RoundTo(40 * ARatio, -2);
      Height := RoundTo(40 * ARatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * ARatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*ARatio,8*ARatio,8*ARatio,8*ARatio).RoundTo(-2);
      Fill.Color := TalphaColorRec.Null;
      Fill.ColorKey := '';
      Fill.ImageTintColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      Fill.ResourceName := LPrevResourceName;
      Stroke.ColorKey := 'Material.Color.Outline'; // md.sys.color.outline / md.ref.palette.neutral-variant50 / #79747E
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Fill.ImageTintColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      StateStyles.Disabled.Stroke.Assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Hovered--
      StateStyles.Hovered.StateLayer.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.StateLayer.XRadius := -50;
      StateStyles.Hovered.StateLayer.YRadius := -50;
      //--Pressed--
      StateStyles.Pressed.StateLayer.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Pressed.StateLayer.XRadius := -50;
      StateStyles.Pressed.StateLayer.YRadius := -50;
      //--Focused--
      StateStyles.Focused.StateLayer.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Focused.StateLayer.XRadius := -50;
      StateStyles.Focused.StateLayer.YRadius := -50;

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/icon-buttons/specs#e63a9b45-a20c-402c-8cc5-2c67ad8aae25
procedure ALApplyMaterialButtonIconStandardStyle(const AButton: TALButton; const ARatio: Single = 1);
begin
  With AButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      var LPrevResourceName := Abutton.Fill.ResourceName;
      var LPrevDisabledResourceName := Abutton.StateStyles.Disabled.Fill.ResourceName;
      ALResetButtonStyle(AButton);
      Width := RoundTo(40 * ARatio, -2);
      Height := RoundTo(40 * ARatio, -2);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * ARatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := -50;
      YRadius := -50;
      Fill.ImageMargins.Rect := TRectF.Create(8*ARatio,8*ARatio,8*ARatio,8*ARatio).RoundTo(-2);
      Fill.ResourceName := LPrevResourceName;
      Fill.Color := TalphaColors.Null;
      Fill.ColorKey := '';
      Fill.ImageTintColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      Stroke.Color := TALphaColors.Null;
      Stroke.ColorKey := '';
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;
      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.ImageTintColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Disabled.Fill.ResourceName := LPrevDisabledResourceName;
      //--Hovered--
      StateStyles.Hovered.StateLayer.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Hovered.StateLayer.Opacity := 0.08;
      StateStyles.Hovered.StateLayer.XRadius := -50;
      StateStyles.Hovered.StateLayer.YRadius := -50;
      //--Pressed--
      StateStyles.Pressed.StateLayer.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Pressed.StateLayer.XRadius := -50;
      StateStyles.Pressed.StateLayer.YRadius := -50;
      //--Focused--
      StateStyles.Focused.StateLayer.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      StateStyles.Focused.StateLayer.XRadius := -50;
      StateStyles.Focused.StateLayer.YRadius := -50;

    finally
      EndUpdate;
    end;
  end;
end;

//////////////
// CHECKBOX //
//////////////

type
  TALBaseCheckBoxStateStylesProtectedAccess = class(TALBaseCheckBox.TStateStyles);

{*****************************************************************************************}
procedure ALResetCheckBoxStyle(const ACheckBox: TALBaseCheckBox; const ARatio: Single = 1);
begin
  With ACheckBox do begin
    BeginUpdate;
    Try
      ALResetShapeStyle(ACheckBox, ARatio);
      var LSize := DefaultSize;
      LSize.Height := RoundTo(LSize.Height * ARatio, -2);
      LSize.Width := RoundTo(LSize.Width * ARatio, -2);
      Size.Size := LSize;
      //Margins.Rect := ALScaleRect(Margins.DefaultValue, ARatio).RoundTo(-2);
      Padding.Rect := ALScaleRect(Padding.DefaultValue, ARatio).RoundTo(-2);
      TouchTargetExpansion.Rect := ALScaleRect(TouchTargetExpansion.DefaultValue, ARatio).RoundTo(-2);
      XRadius := DefaultXRadius;
      YRadius := DefaultYRadius;
      if XRadius > 0 then XRadius := RoundTo(XRadius * ARatio, -2);
      if YRadius > 0 then YRadius := RoundTo(YRadius * ARatio, -2);
      Checkmark.Reset;
      Checkmark.Margins.Rect := ALScaleRect(Checkmark.Margins.DefaultValue, ARatio).RoundTo(-2);
      CheckMark.Thickness := RoundTo(CheckMark.DefaultThickness * ARatio, -2);
      StateStyles.Reset;
      StateStyles.Checked.Hovered.statelayer.margins.rect := ALScaleRect(StateStyles.Checked.Hovered.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
      StateStyles.Checked.Pressed.statelayer.margins.rect := ALScaleRect(StateStyles.Checked.Pressed.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
      StateStyles.Checked.Focused.statelayer.margins.rect := ALScaleRect(StateStyles.Checked.Focused.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
      StateStyles.UnChecked.Hovered.statelayer.margins.rect := ALScaleRect(StateStyles.UnChecked.Hovered.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
      StateStyles.UnChecked.Pressed.statelayer.margins.rect := ALScaleRect(StateStyles.UnChecked.Pressed.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
      StateStyles.UnChecked.Focused.statelayer.margins.rect := ALScaleRect(StateStyles.UnChecked.Focused.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
    Finally
      EndUpdate;
    End;
  end;
end;

{*************************************************************************************}
//https://m3.material.io/components/checkbox/specs#fd29f662-6e61-4c1f-9b97-1145c3b33075
procedure ALApplyMaterialCheckBoxStyle(const ACheckBox: TALBaseCheckBox; const ARatio: Single = 1);
begin
  With ACheckBox do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetCheckBoxStyle(ACheckBox, ARatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * ARatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := RoundTo(2 * ARatio, -2);
      YRadius := RoundTo(2 * ARatio, -2);
      Fill.Color := TalphaColors.Null;
      Fill.ColorKey := '';
      Stroke.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      Stroke.Thickness := RoundTo(2 * ARatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FF1D1B20 / md.sys.color.on-surface / md.ref.palette.neutral10
      CheckMark.ColorKey := ''; // $FF1D1B20 / md.sys.color.on-surface / md.ref.palette.neutral10
      TALBaseCheckBoxStateStylesProtectedAccess(StateStyles).Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      TALBaseCheckBoxStateStylesProtectedAccess(StateStyles).Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.Fill.Assign(Fill);
      StateStyles.Checked.Default.Fill.Inherit := False;
      StateStyles.Checked.Default.Fill.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Default.Stroke.Assign(Stroke);
      StateStyles.Checked.Default.Stroke.Inherit := False;
      StateStyles.Checked.Default.Stroke.Thickness := 0;
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.ColorKey := 'Material.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Fill.Assign(Fill);
      StateStyles.Checked.Disabled.Fill.Inherit := False;
      StateStyles.Checked.Disabled.Fill.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.ColorKey := 'Material.Color.Surface'; // md.sys.color.surface / md.ref.palette.neutral98 / #FEF7FF

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

{*************************************************************************************}
//https://m3.material.io/components/checkbox/specs#fd29f662-6e61-4c1f-9b97-1145c3b33075
procedure ALApplyMaterialCheckBoxErrorStyle(const ACheckBox: TALBaseCheckBox; const ARatio: Single = 1);
begin
  With ACheckBox do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetCheckBoxStyle(ACheckBox, ARatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * ARatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      XRadius := RoundTo(2 * ARatio, -2);
      YRadius := RoundTo(2 * ARatio, -2);
      Fill.Color := TalphaColors.Null;
      Fill.ColorKey := '';
      Stroke.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      Stroke.Thickness := RoundTo(2 * ARatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FFB3261E / md.sys.color.error / md.ref.palette.error40
      CheckMark.ColorKey := ''; // $FFB3261E / md.sys.color.error / md.ref.palette.error40
      TALBaseCheckBoxStateStylesProtectedAccess(StateStyles).Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      TALBaseCheckBoxStateStylesProtectedAccess(StateStyles).Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.Fill.Assign(Fill);
      StateStyles.Checked.Default.Fill.Inherit := False;
      StateStyles.Checked.Default.Fill.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.Checked.Default.Stroke.Assign(Stroke);
      StateStyles.Checked.Default.Stroke.Inherit := False;
      StateStyles.Checked.Default.Stroke.Thickness := 0;
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.ColorKey := 'Material.Color.OnError'; // md.sys.color.on-error / md.ref.palette.error100 / #FFFFFF

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Fill.Assign(Fill);
      StateStyles.Checked.Disabled.Fill.Inherit := False;
      StateStyles.Checked.Disabled.Fill.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.ColorKey := 'Material.Color.Surface'; // md.sys.color.surface / md.ref.palette.neutral98 / #FEF7FF

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

/////////////////
// RADIOBUTTON //
/////////////////

{**********************************************************************************************}
procedure ALResetRadioButtonStyle(const ARadioButton: TALRadioButton; const ARatio: Single = 1);
begin
  ALResetCheckBoxStyle(ARadioButton, ARatio);
end;

{*****************************************************************************************}
//https://m3.material.io/components/radio-button/specs#4eca59b9-dfb5-4ca4-9c76-8e664fb02137
procedure ALApplyMaterialRadioButtonStyle(const ARadioButton: TALRadioButton; const ARatio: Single = 1);
begin
  With ARadioButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetRadioButtonStyle(ARadioButton, ARatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * ARatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      Fill.Color := TalphaColors.Null;
      Fill.ColorKey := '';
      Stroke.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      Stroke.Thickness := RoundTo(2 * ARatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FF1D1B20 / md.sys.color.on-surface / md.ref.palette.neutral10
      CheckMark.ColorKey := ''; // $FF1D1B20 / md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.Stroke.Assign(Stroke);
      StateStyles.Checked.Default.Stroke.Inherit := False;
      StateStyles.Checked.Default.Stroke.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Stroke.assign(Stroke);
      StateStyles.Checked.Disabled.Stroke.Inherit := False;
      StateStyles.Checked.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

{*****************************************************************************************}
//https://m3.material.io/components/radio-button/specs#4eca59b9-dfb5-4ca4-9c76-8e664fb02137
procedure ALApplyMaterialRadioButtonErrorStyle(const ARadioButton: TALRadioButton; const ARatio: Single = 1);
begin
  With ARadioButton do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetRadioButtonStyle(ARadioButton, ARatio);
      var LDelta := RoundTo(max(0, (48 - Height) / 2) * ARatio, -2);
      TouchTargetExpansion.Rect := TRectf.Create(LDelta,LDelta,LDelta,LDelta);
      Fill.Color := TalphaColors.Null;
      Fill.ColorKey := '';
      Stroke.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      Stroke.Thickness := RoundTo(2 * ARatio, -2);
      CheckMark.Color := TAlphaColors.Null; // $FFB3261E / md.sys.color.error / md.ref.palette.error40
      CheckMark.ColorKey := ''; // $FFB3261E / md.sys.color.error / md.ref.palette.error40
      StateStyles.Transition.Interpolation := TALInterpolationType.MaterialStandardDefaultEffects;
      StateStyles.Transition.Duration := 0.2;

      //--Default (UnChecked)--
      //--Default (Checked)--
      StateStyles.Checked.Default.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Default.CheckMark.Inherit := False;
      StateStyles.Checked.Default.CheckMark.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E

      //--Disabled (UnChecked)--
      StateStyles.UnChecked.Disabled.Opacity := 1;
      StateStyles.UnChecked.Disabled.Stroke.assign(Stroke);
      StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      StateStyles.UnChecked.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.UnChecked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      StateStyles.UnChecked.Disabled.CheckMark.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--Disabled (Checked)--
      StateStyles.Checked.Disabled.Opacity := 1;
      StateStyles.Checked.Disabled.Stroke.assign(Stroke);
      StateStyles.Checked.Disabled.Stroke.Inherit := False;
      StateStyles.Checked.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      StateStyles.Checked.Disabled.CheckMark.Assign(CheckMark);
      StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      StateStyles.Checked.Disabled.CheckMark.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20

      //--Hovered (UnChecked)--
      StateStyles.UnChecked.Hovered.StateLayer.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      StateStyles.Checked.Hovered.StateLayer.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      StateStyles.UnChecked.Pressed.StateLayer.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12;
      //--Pressed (Checked)--
      StateStyles.Checked.Pressed.StateLayer.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12;

      //--Focused (UnChecked)--
      StateStyles.UnChecked.Focused.StateLayer.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12;
      //--Focused (Checked)--
      StateStyles.Checked.Focused.StateLayer.ColorKey := 'Material.Color.Error'; // md.sys.color.error / md.ref.palette.error40 / #B3261E
      StateStyles.Checked.Focused.StateLayer.Opacity := 0.12;

    finally
      EndUpdate;
    end;
  end;
end;

////////////
// SWITCH //
////////////

{*******************************************************************************}
procedure ALResetSwitchStyle(const ASwitch: TALSwitch; const ARatio: Single = 1);
begin
  With ASwitch do begin
    BeginUpdate;
    Try
      var LSize := DefaultSize;
      LSize.Height := RoundTo(LSize.Height * ARatio, -2);
      LSize.Width := RoundTo(LSize.Width * ARatio, -2);
      Size.Size := LSize;
      //Margins.Rect := ALScaleRect(Margins.DefaultValue, ARatio).RoundTo(-2);
      Padding.Rect := ALScaleRect(Padding.DefaultValue, ARatio).RoundTo(-2);
      TouchTargetExpansion.Rect := ALScaleRect(TouchTargetExpansion.DefaultValue, ARatio).RoundTo(-2);
      //--
      ALResetShapeStyle(Track, ARatio);
      Track.Margins.Rect := ALScaleRect(Track.Margins.DefaultValue, ARatio).RoundTo(-2);
      Track.Padding.Rect := ALScaleRect(Track.Padding.DefaultValue, ARatio).RoundTo(-2);
      Track.TouchTargetExpansion.Rect := ALScaleRect(Track.TouchTargetExpansion.DefaultValue, ARatio).RoundTo(-2);
      Track.XRadius := Track.DefaultXRadius;
      Track.YRadius := Track.DefaultYRadius;
      if Track.XRadius > 0 then Track.XRadius := RoundTo(Track.XRadius * ARatio, -2);
      if Track.YRadius > 0 then Track.YRadius := RoundTo(Track.YRadius * ARatio, -2);
      Track.StateStyles.Reset;
      Track.StateStyles.Checked.Hovered.statelayer.margins.rect := ALScaleRect(Track.StateStyles.Checked.Hovered.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
      Track.StateStyles.Checked.Pressed.statelayer.margins.rect := ALScaleRect(Track.StateStyles.Checked.Pressed.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
      Track.StateStyles.Checked.Focused.statelayer.margins.rect := ALScaleRect(Track.StateStyles.Checked.Focused.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
      Track.StateStyles.UnChecked.Hovered.statelayer.margins.rect := ALScaleRect(Track.StateStyles.UnChecked.Hovered.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
      Track.StateStyles.UnChecked.Pressed.statelayer.margins.rect := ALScaleRect(Track.StateStyles.UnChecked.Pressed.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
      Track.StateStyles.UnChecked.Focused.statelayer.margins.rect := ALScaleRect(Track.StateStyles.UnChecked.Focused.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
      //--
      ALResetCheckBoxStyle(Thumb, ARatio);
      Thumb.Margins.Rect := ALScaleRect(Thumb.Margins.DefaultValue, ARatio).RoundTo(-2);
      Thumb.Width := Height - Thumb.Margins.Top - Thumb.Margins.bottom;
    Finally
      EndUpdate;
    End;
  end;
end;

{***********************************************************************************}
//https://m3.material.io/components/switch/specs#e27a8630-f5e0-481a-ad24-0e8ebb8a8619
procedure ALApplyMaterialSwitchStyle(const ASwitch: TALSwitch; const ARatio: Single = 1);
begin
  With ASwitch do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetSwitchStyle(ASwitch, ARatio);

      //--Default (UnChecked)--
      Track.StateStyles.UnChecked.Default.Stroke.Assign(Track.Stroke);
      Track.StateStyles.UnChecked.Default.Stroke.Inherit := False;
      Track.StateStyles.UnChecked.Default.Stroke.ColorKey := 'Material.Color.Outline'; // md.sys.color.outline / md.ref.palette.neutral-variant50 / #79747E
      Track.StateStyles.UnChecked.Default.Stroke.Thickness := RoundTo(2 * ARatio, -2);
      Track.StateStyles.UnChecked.Default.fill.Assign(Track.fill);
      Track.StateStyles.UnChecked.Default.fill.Inherit := False;
      Track.StateStyles.UnChecked.Default.fill.ColorKey := 'Material.Color.SurfaceContainerHighest'; // md.sys.color.surface-container-highest / md.ref.palette.neutral90 / #E6E0E9
      //--
      Thumb.StateStyles.UnChecked.Default.fill.Assign(Thumb.fill);
      Thumb.StateStyles.UnChecked.Default.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Default.fill.ColorKey := 'Material.Color.Outline'; // md.sys.color.outline / md.ref.palette.neutral-variant50 / #79747E
      Thumb.StateStyles.UnChecked.Default.Fill.BackgroundMargins.Rect := TRectF.Create(4*ARatio,4*ARatio,4*ARatio,4*ARatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Default.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.UnChecked.Default.CheckMark.Inherit := False;
      Thumb.StateStyles.UnChecked.Default.CheckMark.Color := TAlphacolors.Null; // TALStyleManager.Instance.GetColor('Material.Color.SurfaceContainerHighest'; // md.sys.color.surface-container-highest / md.ref.palette.neutral90 / #E6E0E9
      Thumb.StateStyles.UnChecked.Default.CheckMark.ColorKey := ''; // TALStyleManager.Instance.GetColor('Material.Color.SurfaceContainerHighest'; // md.sys.color.surface-container-highest / md.ref.palette.neutral90 / #E6E0E9
      //--Default (Checked)--
      Track.StateStyles.Checked.Default.fill.Assign(Track.fill);
      Track.StateStyles.Checked.Default.fill.Inherit := False;
      Track.StateStyles.Checked.Default.fill.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      //--
      Thumb.StateStyles.Checked.Default.fill.Assign(Thumb.fill);
      Thumb.StateStyles.Checked.Default.fill.Inherit := False;
      Thumb.StateStyles.Checked.Default.fill.ColorKey := 'Material.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF
      Thumb.StateStyles.Checked.Default.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.Checked.Default.CheckMark.Inherit := False;
      Thumb.StateStyles.Checked.Default.CheckMark.ColorKey := 'Material.Color.OnPrimaryContainer'; // md.sys.color.on-primary-container / md.ref.palette.primary30 / #4F378B

      //--Disabled (UnChecked)--
      Track.StateStyles.UnChecked.Disabled.Opacity := 1;
      Track.StateStyles.UnChecked.Disabled.Stroke.Assign(Track.Stroke);
      Track.StateStyles.UnChecked.Disabled.Stroke.Inherit := False;
      Track.StateStyles.UnChecked.Disabled.Stroke.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      Track.StateStyles.UnChecked.Disabled.Stroke.Thickness := RoundTo(2 * ARatio, -2);
      Track.StateStyles.UnChecked.Disabled.fill.Assign(Track.fill);
      Track.StateStyles.UnChecked.Disabled.fill.Inherit := False;
      Track.StateStyles.UnChecked.Disabled.fill.ColorKey := 'Material.Color.SurfaceContainerHighest.Alpha12'; // md.sys.color.surface-container-highest / md.ref.palette.neutral90 / #E6E0E9
      //--
      Thumb.StateStyles.UnChecked.Disabled.Opacity := 1;
      Thumb.StateStyles.UnChecked.Disabled.fill.Assign(Thumb.fill);
      Thumb.StateStyles.UnChecked.Disabled.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Disabled.fill.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      Thumb.StateStyles.UnChecked.Disabled.Fill.BackgroundMargins.Rect := TRectF.Create(4*ARatio,4*ARatio,4*ARatio,4*ARatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Disabled.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.UnChecked.Disabled.CheckMark.Inherit := False;
      Thumb.StateStyles.UnChecked.Disabled.CheckMark.ColorKey := 'Material.Color.SurfaceContainerHighest.Alpha38'; // md.sys.color.surface-container-highest / md.ref.palette.neutral90 / #E6E0E9
      //--Disabled (Checked)--
      Track.StateStyles.Checked.Disabled.Opacity := 1;
      Track.StateStyles.Checked.Disabled.fill.Assign(Track.fill);
      Track.StateStyles.Checked.Disabled.fill.Inherit := False;
      Track.StateStyles.Checked.Disabled.fill.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--
      Thumb.StateStyles.Checked.Disabled.Opacity := 1;
      Thumb.StateStyles.Checked.Disabled.fill.Assign(Thumb.fill);
      Thumb.StateStyles.Checked.Disabled.fill.Inherit := False;
      Thumb.StateStyles.Checked.Disabled.fill.ColorKey := 'Material.Color.Surface'; // md.sys.color.surface / md.ref.palette.neutral98 / #FEF7FF
      Thumb.StateStyles.Checked.Disabled.CheckMark.Assign(Thumb.CheckMark);
      Thumb.StateStyles.Checked.Disabled.CheckMark.Inherit := False;
      Thumb.StateStyles.Checked.Disabled.CheckMark.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20

      //--Hovered (UnChecked)--
      Thumb.StateStyles.UnChecked.Hovered.fill.Assign(Thumb.StateStyles.UnChecked.Default.fill);
      Thumb.StateStyles.UnChecked.Hovered.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Hovered.fill.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      Thumb.StateStyles.UnChecked.Hovered.StateLayer.Margins.Rect := TRectF.Create(-8*ARatio,-8*ARatio,-8*ARatio,-8*ARatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Hovered.StateLayer.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      Thumb.StateStyles.UnChecked.Hovered.StateLayer.Opacity := 0.08;
      //--Hovered (Checked)--
      Thumb.StateStyles.Checked.Hovered.fill.Assign(Thumb.StateStyles.Checked.Default.fill);
      Thumb.StateStyles.Checked.Hovered.fill.Inherit := False;
      Thumb.StateStyles.Checked.Hovered.fill.ColorKey := 'Material.Color.PrimaryContainer'; // md.sys.color.primary-container / md.ref.palette.primary90 / #EADDFF
      Thumb.StateStyles.Checked.Hovered.StateLayer.Margins.Rect := TRectF.Create(-8*ARatio,-8*ARatio,-8*ARatio,-8*ARatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Hovered.StateLayer.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Thumb.StateStyles.Checked.Hovered.StateLayer.Opacity := 0.08;

      //--Pressed (UnChecked)--
      Thumb.StateStyles.UnChecked.Pressed.Fill.Assign(Thumb.StateStyles.UnChecked.Default.fill);
      Thumb.StateStyles.UnChecked.Pressed.Fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Pressed.fill.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      Thumb.StateStyles.UnChecked.Pressed.Fill.BackgroundMargins.Rect := TRectF.Create(-2*ARatio,-2*ARatio,-2*ARatio,-2*ARatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Pressed.StateLayer.Margins.Rect := TRectF.Create(-8*ARatio,-8*ARatio,-8*ARatio,-8*ARatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Pressed.StateLayer.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      Thumb.StateStyles.UnChecked.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Pressed (Checked)--
      Thumb.StateStyles.Checked.Pressed.Fill.Assign(Thumb.StateStyles.Checked.Default.fill);
      Thumb.StateStyles.Checked.Pressed.Fill.Inherit := False;
      Thumb.StateStyles.Checked.Pressed.fill.ColorKey := 'Material.Color.PrimaryContainer'; // md.sys.color.primary-container / md.ref.palette.primary90 / #EADDFF
      Thumb.StateStyles.Checked.Pressed.Fill.BackgroundMargins.Rect := TRectF.Create(-2*ARatio,-2*ARatio,-2*ARatio,-2*ARatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Pressed.StateLayer.Margins.Rect := TRectF.Create(-8*ARatio,-8*ARatio,-8*ARatio,-8*ARatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Pressed.StateLayer.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Thumb.StateStyles.Checked.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast

      //--Focused (UnChecked)--
      Thumb.StateStyles.UnChecked.Focused.fill.Assign(Thumb.StateStyles.UnChecked.Default.fill);
      Thumb.StateStyles.UnChecked.Focused.fill.Inherit := False;
      Thumb.StateStyles.UnChecked.Focused.fill.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
      Thumb.StateStyles.UnChecked.Focused.StateLayer.Margins.Rect := TRectF.Create(-8*ARatio,-8*ARatio,-8*ARatio,-8*ARatio).RoundTo(-2);
      Thumb.StateStyles.UnChecked.Focused.StateLayer.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      Thumb.StateStyles.UnChecked.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused (Checked)--
      Thumb.StateStyles.Checked.Focused.fill.Assign(Thumb.StateStyles.Checked.Default.fill);
      Thumb.StateStyles.Checked.Focused.fill.Inherit := False;
      Thumb.StateStyles.Checked.Focused.fill.ColorKey := 'Material.Color.PrimaryContainer'; // md.sys.color.primary-container / md.ref.palette.primary90 / #EADDFF
      Thumb.StateStyles.Checked.Focused.StateLayer.Margins.Rect := TRectF.Create(-8*ARatio,-8*ARatio,-8*ARatio,-8*ARatio).RoundTo(-2);
      Thumb.StateStyles.Checked.Focused.StateLayer.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
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

{****************************************************************************************}
procedure ALResetTrackBarStyle(const ATrackBar: TALCustomTrack; const ARatio: Single = 1);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function SwapTopBottomWithLeftRight(Const ARect: TrectF): TRectF;
  Begin
    Result.Left := ARect.Top;
    Result.Top := ARect.Left;
    Result.Right := ARect.Bottom;
    Result.Bottom := ARect.Right;
  End;

begin
  With _TALCustomTrackProtectedAccess(ATrackBar) do begin
    BeginUpdate;
    Try
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then begin
        Height := RoundTo(DefaultSize.Height * ARatio, -2);
        //Margins.Rect := ALScaleRect(Margins.DefaultValue, ARatio).RoundTo(-2);
        Padding.Rect := ALScaleRect(Padding.DefaultValue, ARatio).RoundTo(-2);
        TouchTargetExpansion.Rect := ALScaleRect(TouchTargetExpansion.DefaultValue, ARatio).RoundTo(-2);
      end
      else begin
        Width := RoundTo(DefaultSize.Height * ARatio, -2);
        //Margins.Rect := SwapTopBottomWithLeftRight(ALScaleRect(Margins.DefaultValue, ARatio).RoundTo(-2));
        Padding.Rect := SwapTopBottomWithLeftRight(ALScaleRect(Padding.DefaultValue, ARatio).RoundTo(-2));
        TouchTargetExpansion.Rect := SwapTopBottomWithLeftRight(ALScaleRect(TouchTargetExpansion.DefaultValue, ARatio).RoundTo(-2));
      end;
      //--
      if InactiveTrack <> nil then begin
        If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then begin
          InactiveTrack.Margins.Rect := ALScaleRect(InactiveTrack.Margins.DefaultValue, ARatio).RoundTo(-2);
          InactiveTrack.Padding.Rect := ALScaleRect(InactiveTrack.Padding.DefaultValue, ARatio).RoundTo(-2);
          InactiveTrack.TouchTargetExpansion.Rect := ALScaleRect(InactiveTrack.TouchTargetExpansion.DefaultValue, ARatio).RoundTo(-2);
        end
        else begin
          InactiveTrack.Margins.Rect := SwapTopBottomWithLeftRight(ALScaleRect(InactiveTrack.Margins.DefaultValue, ARatio).RoundTo(-2));
          InactiveTrack.Padding.Rect := SwapTopBottomWithLeftRight(ALScaleRect(InactiveTrack.Padding.DefaultValue, ARatio).RoundTo(-2));
          InactiveTrack.TouchTargetExpansion.Rect := SwapTopBottomWithLeftRight(ALScaleRect(InactiveTrack.TouchTargetExpansion.DefaultValue, ARatio).RoundTo(-2));
        end;
        InactiveTrack.XRadius := InactiveTrack.DefaultXRadius;
        InactiveTrack.YRadius := InactiveTrack.DefaultYRadius;
        if InactiveTrack.XRadius > 0 then InactiveTrack.XRadius := RoundTo(InactiveTrack.XRadius * ARatio, -2);
        if InactiveTrack.YRadius > 0 then InactiveTrack.YRadius := RoundTo(InactiveTrack.YRadius * ARatio, -2);
        InactiveTrack.Corners := AllCorners;
        InactiveTrack.Opacity := 1;
        InactiveTrack.Fill.Reset;
        InactiveTrack.Stroke.Reset;
        InactiveTrack.Stroke.Thickness := RoundTo(InactiveTrack.Stroke.DefaultThickness * ARatio, -2);
        InactiveTrack.Shadow.Reset;
        InactiveTrack.stopIndicator.Reset;
        InactiveTrack.stopIndicator.Size := RoundTo(InactiveTrack.stopIndicator.DefaultSize * ARatio, -2);
        InactiveTrack.StateStyles.Reset;
      end;
      //--
      if ActiveTrack <> nil then begin
        If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then begin
          ActiveTrack.Margins.Rect := ALScaleRect(ActiveTrack.Margins.DefaultValue, ARatio).RoundTo(-2);
          ActiveTrack.Padding.Rect := ALScaleRect(ActiveTrack.Padding.DefaultValue, ARatio).RoundTo(-2);
          ActiveTrack.TouchTargetExpansion.Rect := ALScaleRect(ActiveTrack.TouchTargetExpansion.DefaultValue, ARatio).RoundTo(-2);
        end
        else begin
          ActiveTrack.Margins.Rect := SwapTopBottomWithLeftRight(ALScaleRect(ActiveTrack.Margins.DefaultValue, ARatio).RoundTo(-2));
          ActiveTrack.Padding.Rect := SwapTopBottomWithLeftRight(ALScaleRect(ActiveTrack.Padding.DefaultValue, ARatio).RoundTo(-2));
          ActiveTrack.TouchTargetExpansion.Rect := SwapTopBottomWithLeftRight(ALScaleRect(ActiveTrack.TouchTargetExpansion.DefaultValue, ARatio).RoundTo(-2));
        end;
        ActiveTrack.XRadius := ActiveTrack.DefaultXRadius;
        ActiveTrack.YRadius := ActiveTrack.DefaultYRadius;
        if ActiveTrack.XRadius > 0 then ActiveTrack.XRadius := RoundTo(ActiveTrack.XRadius * ARatio, -2);
        if ActiveTrack.YRadius > 0 then ActiveTrack.YRadius := RoundTo(ActiveTrack.YRadius * ARatio, -2);
        ActiveTrack.Corners := AllCorners;
        ActiveTrack.Opacity := 1;
        ActiveTrack.Fill.Reset;
        ActiveTrack.Stroke.Reset;
        ActiveTrack.Stroke.Thickness := RoundTo(ActiveTrack.Stroke.DefaultThickness * ARatio, -2);
        ActiveTrack.Shadow.Reset;
        ActiveTrack.stopIndicator.Reset;
        ActiveTrack.stopIndicator.Size := RoundTo(ActiveTrack.stopIndicator.DefaultSize * ARatio, -2);
        ActiveTrack.StateStyles.Reset;
      end;
      //--
      if Thumb <> nil then begin
        If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then begin
          Thumb.Margins.Rect := ALScaleRect(Thumb.Margins.DefaultValue, ARatio).RoundTo(-2);
          Thumb.Padding.Rect := ALScaleRect(Thumb.Padding.DefaultValue, ARatio).RoundTo(-2);
          Thumb.TouchTargetExpansion.Rect := ALScaleRect(Thumb.TouchTargetExpansion.DefaultValue, ARatio).RoundTo(-2);
          Thumb.Width := ATrackBar.Height - ATrackBar.Padding.Top - ATrackBar.Padding.Bottom - Thumb.Margins.Top - Thumb.Margins.Bottom
        end
        else begin
          Thumb.Margins.Rect := SwapTopBottomWithLeftRight(ALScaleRect(Thumb.Margins.DefaultValue, ARatio).RoundTo(-2));
          Thumb.Padding.Rect := SwapTopBottomWithLeftRight(ALScaleRect(Thumb.Padding.DefaultValue, ARatio).RoundTo(-2));
          Thumb.TouchTargetExpansion.Rect := SwapTopBottomWithLeftRight(ALScaleRect(Thumb.TouchTargetExpansion.DefaultValue, ARatio).RoundTo(-2));
          Thumb.Height := ATrackBar.Width - ATrackBar.Padding.left - ATrackBar.Padding.right - Thumb.Margins.left - Thumb.Margins.right;
        end;
        Thumb.XRadius := Thumb.DefaultXRadius;
        Thumb.YRadius := Thumb.DefaultYRadius;
        if Thumb.XRadius > 0 then Thumb.XRadius := RoundTo(Thumb.XRadius * ARatio, -2);
        if Thumb.YRadius > 0 then Thumb.YRadius := RoundTo(Thumb.YRadius * ARatio, -2);
        Thumb.Corners := AllCorners;
        Thumb.Opacity := 1;
        Thumb.Fill.Reset;
        Thumb.Stroke.Reset;
        Thumb.Stroke.Thickness := RoundTo(Thumb.Stroke.DefaultThickness * ARatio, -2);
        Thumb.Shadow.Reset;
        Thumb.StateStyles.Reset;
        Thumb.StateStyles.Hovered.statelayer.margins.rect := ALScaleRect(Thumb.StateStyles.Hovered.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
        Thumb.StateStyles.Pressed.statelayer.margins.rect := ALScaleRect(Thumb.StateStyles.Pressed.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
        Thumb.StateStyles.Focused.statelayer.margins.rect := ALScaleRect(Thumb.StateStyles.Focused.statelayer.margins.DefaultValue, ARatio).RoundTo(-2);
      end;
      //--
      if ValueIndicator <> nil then begin
        //ValueIndicator.Margins.Rect := ALScaleRect(ValueIndicator.Margins.DefaultValue, ARatio).RoundTo(-2);
        ValueIndicator.Padding.Rect := ALScaleRect(ValueIndicator.Padding.DefaultValue, ARatio).RoundTo(-2);
        ValueIndicator.TouchTargetExpansion.Rect := ALScaleRect(ValueIndicator.TouchTargetExpansion.DefaultValue, ARatio).RoundTo(-2);
        ValueIndicator.XRadius := ValueIndicator.DefaultXRadius;
        ValueIndicator.YRadius := ValueIndicator.DefaultYRadius;
        if ValueIndicator.XRadius > 0 then ValueIndicator.XRadius := RoundTo(ValueIndicator.XRadius * ARatio, -2);
        if ValueIndicator.YRadius > 0 then ValueIndicator.YRadius := RoundTo(ValueIndicator.YRadius * ARatio, -2);
        ValueIndicator.Animation := TValueIndicator.TAnimation.ScaleInOut;
        ValueIndicator.AutoSize := true;
        ValueIndicator.Corners := AllCorners;
        ValueIndicator.Sides := AllSides;
        ValueIndicator.Opacity := 1;
        ValueIndicator.Fill.Reset;
        ValueIndicator.Stroke.Reset;
        ValueIndicator.Stroke.Thickness := RoundTo(ValueIndicator.Stroke.DefaultThickness * ARatio, -2);
        ValueIndicator.Shadow.Reset;
        ValueIndicator.TextSettings.Reset;
      end;
    Finally
      EndUpdate;
    End;
  end;
end;

{*************************************************************************************}
//https://m3.material.io/components/TrackBar/specs#e27a8630-f5e0-481a-ad24-0e8ebb8a8619
procedure ALApplyMaterialTrackBarStyle(const ATrackBar: TALCustomTrack; const ARatio: Single = 1);
begin
  With _TALCustomTrackProtectedAccess(ATrackBar) do begin
    BeginUpdate;
    Try

      //--Enabled (default)--
      ALResetTrackBarStyle(ATrackBar, ARatio);
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then begin
        Height := RoundTo(44 * ARatio, -2);
        InactiveTrack.Margins.Rect := TRectF.Create(0,14*ARatio,0,14*ARatio).RoundTo(-2);
        ActiveTrack.Margins.Rect := TRectF.Create(0,14*ARatio,0,14*ARatio).RoundTo(-2);
        InactiveTrack.Padding.Rect := TRectF.Create(6*ARatio,0,6*ARatio,0).RoundTo(-2);
        ActiveTrack.Padding.Rect := TRectF.Create(6*ARatio,0,6*ARatio,0).RoundTo(-2);
      end
      else begin
        Width := RoundTo(44 * ARatio, -2);
        InactiveTrack.Margins.Rect := TRectF.Create(14*ARatio,0,14*ARatio,0).RoundTo(-2);
        ActiveTrack.Margins.Rect := TRectF.Create(14*ARatio,0,14*ARatio,0).RoundTo(-2);
        InactiveTrack.Padding.Rect := TRectF.Create(0,6*ARatio,0,6*ARatio).RoundTo(-2);
        ActiveTrack.Padding.Rect := TRectF.Create(0,6*ARatio,0,6*ARatio).RoundTo(-2);
      end;
      InactiveTrack.XRadius := -50;
      InactiveTrack.YRadius := -50;
      ActiveTrack.XRadius := -50;
      ActiveTrack.YRadius := -50;
      InactiveTrack.Fill.ColorKey := 'Material.Color.SecondaryContainer'; // md.sys.color.secondary-container / md.ref.palette.secondary90 / #E8DEF8
      ActiveTrack.Fill.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      InactiveTrack.StopIndicator.Size := RoundTo(4 * ARatio, -2);
      ActiveTrack.StopIndicator.Size := RoundTo(4 * ARatio, -2);
      InactiveTrack.StopIndicator.ColorKey := 'Material.Color.OnSecondaryContainer'; // md.sys.color.on-secondary-container / md.ref.palette.secondary30 / #4A4458
      ActiveTrack.StopIndicator.ColorKey := 'Material.Color.OnPrimary'; // md.sys.color.on-primary / md.ref.palette.primary100 / #FFFFFF
      ValueIndicator.TextSettings.Font.ColorKey := 'Material.Color.InverseOnSurface'; // md.sys.color.inverse-on-surface / md.ref.palette.neutral95 / #F5EFF7
      ValueIndicator.TextSettings.Font.Size := RoundTo(14 * ARatio, -2);
      ValueIndicator.padding.Rect := TRectF.create(16 * ARatio{Left}, 12 * ARatio{Top}, 16 * ARatio{Right}, 12 * ARatio{Bottom}).RoundTo(-2);
      ValueIndicator.Fill.ColorKey := 'Material.Color.InverseSurface'; // md.sys.color.inverse-surface / md.ref.palette.neutral20 / #322F35
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then begin
        Thumb.Margins.Rect := TRectF.Create(6*ARatio,0,6*ARatio,0).RoundTo(-2);
        Thumb.TouchTargetExpansion.Rect := TRectF.Create(22*ARatio,2*ARatio,22*ARatio,2*ARatio).RoundTo(-2);
        Thumb.Width := RoundTo(4 * ARatio, -2)
      end
      else begin
        Thumb.Margins.Rect := TRectF.Create(0,6*ARatio,0,6*ARatio).RoundTo(-2);
        Thumb.TouchTargetExpansion.Rect := TRectF.Create(2*ARatio,22*ARatio,2*ARatio,22*ARatio).RoundTo(-2);
        Thumb.Height := RoundTo(4 * ARatio, -2);
      end;
      Thumb.Fill.ColorKey := 'Material.Color.Primary'; // md.sys.color.primary / md.ref.palette.primary40 / #6750A4
      Thumb.Stroke.Color := TalphaColors.Null;
      Thumb.Stroke.ColorKey := '';

      //--Disabled--
      InactiveTrack.StateStyles.Disabled.Opacity := 1;
      ActiveTrack.StateStyles.Disabled.Opacity := 1;
      //--
      InactiveTrack.StateStyles.Disabled.fill.Assign(InactiveTrack.Fill);
      InactiveTrack.StateStyles.Disabled.fill.Inherit := False;
      InactiveTrack.StateStyles.Disabled.fill.ColorKey := 'Material.Color.OnSurface.Alpha12'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--
      ActiveTrack.StateStyles.Disabled.fill.Assign(ActiveTrack.Fill);
      ActiveTrack.StateStyles.Disabled.fill.Inherit := False;
      ActiveTrack.StateStyles.Disabled.fill.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--
      InactiveTrack.StateStyles.Disabled.StopIndicator.Assign(InactiveTrack.StopIndicator);
      InactiveTrack.StateStyles.Disabled.StopIndicator.Inherit := False;
      InactiveTrack.StateStyles.Disabled.StopIndicator.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
      //--
      ActiveTrack.StateStyles.Disabled.StopIndicator.Assign(ActiveTrack.StopIndicator);
      ActiveTrack.StateStyles.Disabled.StopIndicator.Inherit := False;
      ActiveTrack.StateStyles.Disabled.StopIndicator.ColorKey := 'Material.Color.InverseOnSurface.Alpha66'; // md.sys.color.inverse-on-surface / md.ref.palette.neutral95 / #F5EFF7
      //--
      Thumb.StateStyles.Disabled.Opacity := 1;
      Thumb.StateStyles.Disabled.fill.Assign(Thumb.Fill);
      Thumb.StateStyles.Disabled.fill.Inherit := False;
      Thumb.StateStyles.Disabled.fill.ColorKey := 'Material.Color.OnSurface.Alpha38'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20

      //--Hovered--

      //--Pressed--
      Thumb.StateStyles.Pressed.fill.Assign(Thumb.Fill);
      Thumb.StateStyles.Pressed.fill.Inherit := False;
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then
        Thumb.StateStyles.Pressed.fill.BackgroundMargins.Rect := TRectF.Create(1*ARatio,0,1*ARatio,0).RoundTo(-2)
      else
        Thumb.StateStyles.Pressed.fill.BackgroundMargins.Rect := TRectF.Create(0,1*ARatio,0,1*ARatio).RoundTo(-2);

      //--Focused--
      Thumb.StateStyles.Focused.fill.Assign(Thumb.Fill);
      Thumb.StateStyles.Focused.fill.Inherit := False;
      If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then
        Thumb.StateStyles.Focused.fill.BackgroundMargins.Rect := TRectF.Create(1*ARatio,0,1*ARatio,0).RoundTo(-2)
      else
        Thumb.StateStyles.Focused.fill.BackgroundMargins.Rect := TRectF.Create(0,1*ARatio,0,1*ARatio).RoundTo(-2);

    finally
      EndUpdate;
    end;
  end;
end;

///////////////
// SCROLLBAR //
///////////////

{**********************************************************************************************}
procedure ALResetScrollBarStyle(const AScrollBar: TALCustomScrollBar; const ARatio: Single = 1);
begin
  ALResetTrackBarStyle(AScrollBar, ARatio);
end;

{**************************************************************************************}
//https://m3.material.io/components/ScrollBar/specs#e27a8630-f5e0-481a-ad24-0e8ebb8a8619
procedure ALApplyMaterialScrollBarStyle(const AScrollBar: TALCustomScrollBar; const ARatio: Single = 1);
begin
  With _TALCustomTrackProtectedAccess(AScrollBar) do begin
    BeginUpdate;
    Try
      ALResetScrollBarStyle(AScrollBar, ARatio);
      Thumb.Fill.ColorKey := 'Material.Color.OnSurface.alpha30';
      //--Disabled--
      //--Hovered--
      Thumb.StateStyles.Hovered.StateLayer.ColorKey := 'Material.Color.OnSurface';
      Thumb.StateStyles.Hovered.StateLayer.Opacity := 0.08;
      //--Pressed--
      Thumb.StateStyles.Pressed.StateLayer.ColorKey := 'Material.Color.OnSurface';
      Thumb.StateStyles.Pressed.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
      //--Focused--
      Thumb.StateStyles.Focused.StateLayer.ColorKey := 'Material.Color.OnSurface';
      Thumb.StateStyles.Focused.StateLayer.Opacity := 0.12; // Instead of 0.10, use a higher value for better contrast
    finally
      EndUpdate;
    end;
  end;
end;

///////////////
// SCROLLBOX //
///////////////

type
  _TALCustomScrollBoxProtectedAccess = class(TALCustomScrollBox);

{********************************************************************}
procedure ALResetScrollBoxStyle(const AScrollBox: TALCustomScrollBox);
begin
  With _TALCustomScrollBoxProtectedAccess(AScrollBox) do begin
    BeginUpdate;
    Try
      ALResetRectangleStyle(AScrollBox);
      if HScrollBar <> nil then ALResetScrollBarStyle(HScrollBar);
      if VScrollBar <> nil then ALResetScrollBarStyle(VScrollBar);
    Finally
      EndUpdate;
    End;
  end;
end;

{****************************************************************************}
procedure ALApplyMaterialScrollBoxStyle(const AScrollBox: TALCustomScrollBox);
begin
  With _TALCustomScrollBoxProtectedAccess(AScrollBox) do begin
    BeginUpdate;
    Try
      ALResetRectangleStyle(AScrollBox);
      if HScrollBar <> nil then ALApplyMaterialScrollBarStyle(HScrollBar);
      if VScrollBar <> nil then ALApplyMaterialScrollBarStyle(VScrollBar);
    finally
      EndUpdate;
    end;
  end;
end;

///////////////////
// DIALOGMANAGER //
///////////////////

{****************************************************************************************************}
procedure ALResetDialogManagerStyle(const ADialogManager: TALDialogManager; const ARatio: Single = 1);
begin
  With ADialogManager do begin

    // DefaultScrim
    ALResetRectangleStyle(DefaultScrim, ARatio);
    DefaultScrim.Align := TALAlignLayout.Contents;
    DefaultScrim.Fill.Color := $52000000; {Alpha = 32%}
    DefaultScrim.Stroke.Color := TAlphaColors.Null;
    DefaultScrim.Stroke.ColorKey := '';

    // DefaultContainer
    ALResetRectangleStyle(DefaultContainer);
    DefaultContainer.Padding.Rect := TRectF.Create(24*ARatio{Left}, 24*ARatio{Top}, 24*ARatio{Right}, 24*ARatio{Bottom}).RoundTo(-2);
    DefaultContainer.AutoSize := True;
    DefaultContainer.Align := TALAlignLayout.Center;
    DefaultContainer.Fill.Color := $FFFFFFFF;
    DefaultContainer.Fill.ColorKey := '';
    DefaultContainer.Stroke.Color := TAlphaColors.Null;
    DefaultContainer.Stroke.ColorKey := '';
    DefaultContainer.XRadius := RoundTo(28 * ARatio, -2);
    DefaultContainer.YRadius := RoundTo(28 * ARatio, -2);

    // DefaultIcon
    ALResetImageStyle(DefaultIcon);
    DefaultIcon.Width := RoundTo(24 * ARatio, -2);
    DefaultIcon.Height := RoundTo(24 * ARatio, -2);
    DefaultIcon.Align := TALAlignLayout.TopCenter;

    // DefaultHeadline
    var LRatio: Single := 24 / DefaultHeadline.Textsettings.Font.DefaultSize;
    ALResetTextStyle(DefaultHeadline, LRatio);
    if CompareValue(ARatio, 1, TEpsilon.scale) > 0 then DefaultHeadline.TextSettings.Font.Weight := TFontWeight.Medium;
    DefaultHeadline.Margins.Top := RoundTo(16 * ARatio, -2);
    DefaultHeadline.AutoSize := True;
    DefaultHeadline.Align := TALAlignLayout.TopLeft;

    // DefaultContent
    DefaultContent.Align := TALAlignLayout.TopLeft;
    DefaultContent.Margins.Rect := TRectF.Create(-24*ARatio{Left}, 16*ARatio{Top}, -24*ARatio{Right}, 0{Bottom}).RoundTo(-2);

    // DefaultMessage
    LRatio := 14 / DefaultMessage.Textsettings.Font.DefaultSize;
    ALResetTextStyle(DefaultMessage, LRatio * ARatio);
    DefaultMessage.Margins.Rect := TRectF.Create(24*ARatio{Left}, 0{Top}, 24*ARatio{Right}, 0{Bottom}).RoundTo(-2);
    DefaultMessage.AutoSize := True;
    DefaultMessage.Align := TALAlignLayout.TopLeft;

    // DefaultOptionLayout
    ALResetLayoutStyle(DefaultOptionLayout);
    DefaultOptionLayout.Margins.Rect := TRectF.Create(24*ARatio{Left}, 16*ARatio{Top}, 24*ARatio{Right}, 0{Bottom}).RoundTo(-2);
    DefaultOptionLayout.TagFloat := 12 * LRatio; // The padding top of the first item (except message) and padding bottom of the last item (except message)
    DefaultOptionLayout.AutoSize := True;
    DefaultOptionLayout.Align := TALAlignLayout.TopLeft;

    // DefaultRadioButton
    LRatio := (20 + ((14 * ARatio) - 14) / 2) / DefaultRadioButton.DefaultSize.Height;
    ALResetRadioButtonStyle(DefaultRadioButton, LRatio);
    DefaultRadioButton.Align := TALAlignLayout.MostLeftCenter;

    // DefaultCheckBox
    LRatio := (18 + ((14 * ARatio) - 14) / 2) / DefaultCheckBox.DefaultSize.Height;
    ALResetCheckBoxStyle(DefaultCheckBox, LRatio);
    DefaultCheckBox.Align := TALAlignLayout.MostLeftCenter;

    // DefaultInlineButton
    LRatio := 14 / DefaultInlineButton.Textsettings.Font.DefaultSize;
    ALResetButtonStyle(DefaultInlineButton, LRatio * ARatio);
    DefaultInlineButton.Margins.Rect := TRectF.Create(
                                          24*ARatio{Left},
                                          12*ARatio{Top},
                                          24*ARatio{Right},
                                          0{Bottom}).RoundTo(-2);
    DefaultInlineButton.TagFloat := 0; // The padding top of the first item (except message) and padding bottom of the last item (except message)
    DefaultInlineButton.AutoSize := True;
    DefaultInlineButton.Align := TALAlignLayout.TopLeft;

    // DefaultEdit
    LRatio := 16 / DefaultEdit.Textsettings.Font.DefaultSize;
    ALResetEditStyle(DefaultEdit, LRatio * ARatio);
    DefaultEdit.Margins.Rect := TRectF.Create(0{Left}, 16*ARatio{Top}, 0{Right}, 0{Bottom}).RoundTo(-2);
    DefaultEdit.AutoSize := True;
    DefaultEdit.Align := TALAlignLayout.TopLeft;

    // DefaultMemo
    LRatio := 16 / DefaultMemo.Textsettings.Font.DefaultSize;
    ALResetEditStyle(DefaultMemo, LRatio * ARatio);
    DefaultMemo.Margins.Rect := TRectF.Create(0{Left}, 16*ARatio{Top}, 0{Right}, 0{Bottom}).RoundTo(-2);
    DefaultMemo.AutoSizeLineCount := 3;
    DefaultMemo.Align := TALAlignLayout.TopLeft;

    // DefaultLabel
    LRatio := 14 / DefaultLabel.Textsettings.Font.DefaultSize;
    ALResetTextStyle(DefaultLabel, LRatio * ARatio);
    DefaultLabel.Margins.Left := RoundTo(12 * ARatio, -2);
    DefaultLabel.AutoSize := True;
    DefaultLabel.Align := TALAlignLayout.LeftCenter;

    // DefaultFooterBar
    ALResetRectangleStyle(DefaultFooterBar, ARatio);
    DefaultFooterBar.Margins.Top := RoundTo(24{* ARatio}, -2);
    DefaultFooterBar.AutoSize := True;
    DefaultFooterBar.Align := TALAlignLayout.TopRight;
    DefaultFooterBar.Fill.Color := TalphaColors.Null;
    DefaultFooterBar.Fill.ColorKey := '';
    DefaultFooterBar.Stroke.Color := TAlphaColors.Null;
    DefaultFooterBar.Stroke.ColorKey := '';

    // DefaultFooterButton
    LRatio := 14 / DefaultFooterButton.Textsettings.Font.DefaultSize;
    ALResetButtonStyle(DefaultFooterButton, LRatio);
    DefaultFooterButton.Margins.Left := RoundTo(8 * ARatio, -2);
    DefaultFooterButton.Align := TALAlignLayout.RightCenter;

  end;
end;

{************************************************************************************}
//https://m3.material.io/components/dialogs/specs#8e0c5daf-d82a-4963-8759-94769997de9f
procedure ALApplyMaterialDialogManagerStyle(const ADialogManager: TALDialogManager; const ARatio: Single = 1);
begin
  With ADialogManager do begin

    // Default
    ALResetDialogManagerStyle(ADialogManager, ARatio);

    // DefaultScrim
    // https://m3.material.io/styles/elevation/applying-elevation#eb0451aa-61b5-4c35-8d5f-f5b434f63654
    DefaultScrim.Fill.ColorKey := 'Material.Color.Scrim.Alpha32'; // Scrims use the scrim color role at an opacity of 32%.

    // DefaultContainer
    DefaultContainer.Fill.ColorKey := 'Material.Color.SurfaceContainerHigh'; // md.sys.color.surface-container-high / md.ref.palette.neutral92 / #ECE6F0
    DefaultContainer.Shadow.ColorKey := 'Material.Color.Shadow.Alpha50'; // md.sys.color.shadow / md.ref.palette.neutral0 / #000000
    DefaultContainer.Shadow.blur := RoundTo(6 * ARatio, -2);
    DefaultContainer.Shadow.OffsetY := RoundTo(2 * ARatio, -2);

    // DefaultIcon
    DefaultIcon.TintColorKey := 'Material.Color.Secondary'; // md.sys.color.secondary / md.ref.palette.secondary40 / #625B71

    // DefaultHeadline
    DefaultHeadline.TextSettings.Font.ColorKey := 'Material.Color.OnSurface'; // md.sys.color.on-surface / md.ref.palette.neutral10 / #1D1B20
    DefaultHeadline.TextSettings.LineHeightMultiplier := 32/24;

    // DefaultContent
    ALApplyMaterialScrollBoxStyle(DefaultContent);

    // DefaultMessage
    DefaultMessage.TextSettings.Font.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
    if CompareValue(ARatio, 1, TEpsilon.scale) = 0 then begin
      DefaultMessage.TextSettings.LineHeightMultiplier := 20/14;
      DefaultMessage.TextSettings.LetterSpacing := RoundTo(0.25 * ARatio, -2);
    end
    else
      DefaultMessage.TextSettings.LineHeightMultiplier := ALEstimateLineHeightMultiplier(DefaultMessage.TextSettings.Font.Size);

    // DefaultOptionLayout

    // DefaultRadioButton
    var LRatio := (20 + ((14 * ARatio) - 14) / 2) / DefaultRadioButton.DefaultSize.Height;
    ALApplyMaterialRadioButtonStyle(DefaultRadioButton, LRatio);

    // DefaultCheckBox
    LRatio := (18 + ((14 * ARatio) - 14) / 2) / DefaultCheckBox.DefaultSize.Height;
    ALApplyMaterialCheckBoxStyle(DefaultCheckBox, LRatio);

    // DefaultInlineButton
    LRatio := 14 / DefaultInlineButton.Textsettings.Font.DefaultSize;
    ALApplyMaterialButtonTextStyle(DefaultInlineButton, LRatio * ARatio);
    DefaultInlineButton.Margins.Rect := TRectF.Create(
                                          (24*ARatio) - (DefaultInlineButton.Padding.Left){Left},
                                          12*ARatio{Top},
                                          (24*ARatio) - (DefaultInlineButton.Padding.Right){Right},
                                          -12*ARatio{Bottom}).RoundTo(-2);
    DefaultInlineButton.TextSettings.Font.ColorKey := DefaultMessage.TextSettings.Font.ColorKey;
    DefaultInlineButton.TextSettings.Font.Weight := TFontWeight.Regular;

    // DefaultEdit
    LRatio := 14 / DefaultEdit.Textsettings.Font.DefaultSize;
    ALApplyMaterialEditHybridStyle(DefaultEdit, LRatio * ARatio);

    // DefaultMemo
    LRatio := 14 / DefaultMemo.Textsettings.Font.DefaultSize;
    ALApplyMaterialEditHybridStyle(DefaultMemo, LRatio * ARatio);

    // DefaultLabel
    DefaultLabel.TextSettings.Font.ColorKey := 'Material.Color.OnSurfaceVariant'; // md.sys.color.on-surface-variant / md.ref.palette.neutral-variant30 / #49454F
    if CompareValue(ARatio, 1, TEpsilon.scale) = 0 then begin
      DefaultLabel.TextSettings.LineHeightMultiplier := 20 / 14;
      DefaultLabel.TextSettings.LetterSpacing := RoundTo(0.25 * ARatio, -2);
    end
    else
      DefaultLabel.TextSettings.LineHeightMultiplier := ALEstimateLineHeightMultiplier(DefaultLabel.TextSettings.Font.Size);

    // DefaultFooterBar

    // DefaultFooterButton
    LRatio := 14 / DefaultFooterButton.Textsettings.Font.DefaultSize;
    ALApplyMaterialButtonTextStyle(DefaultFooterButton, LRatio * ARatio);

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

{**************************************************************************************************************************************************************}
constructor TALStyleManager.TButtonStyleInfo.create(const AApplyStyleProc: TButtonApplyStyleProc; const ADefaultFontSize: Single; const ADefaultHeight: Single);
begin
  SortOrder := TALStyleManager.GetNextSortOrder;
  ApplyStyleProc := AApplyStyleProc;
  DefaultFontSize := ADefaultFontSize;
  DefaultHeight := ADefaultHeight;
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

{********************************************************************************************************************************}
constructor TALStyleManager.TTrackbarStyleInfo.create(const AApplyStyleProc: TTrackBarApplyStyleProc; const ADefaultSize: Single);
begin
  SortOrder := TALStyleManager.GetNextSortOrder;
  ApplyStyleProc := AApplyStyleProc;
  DefaultSize := ADefaultSize;
end;

{**********************************************************************************************************************************}
constructor TALStyleManager.TScrollbarStyleInfo.create(const AApplyStyleProc: TScrollBarApplyStyleProc; const ADefaultSize: Single);
begin
  SortOrder := TALStyleManager.GetNextSortOrder;
  ApplyStyleProc := AApplyStyleProc;
  DefaultSize := ADefaultSize;
end;

{******************************************************************************************************}
constructor TALStyleManager.TScrollboxStyleInfo.create(const AApplyStyleProc: TScrollBoxApplyStyleProc);
begin
  SortOrder := TALStyleManager.GetNextSortOrder;
  ApplyStyleProc := AApplyStyleProc;
end;

{**********************************************************************************************************************************************}
constructor TALStyleManager.TDialogManagerStyleInfo.create(const AApplyStyleProc: TDialogManagerApplyStyleProc; const ADefaultFontSize: Single);
begin
  SortOrder := TALStyleManager.GetNextSortOrder;
  ApplyStyleProc := AApplyStyleProc;
  DefaultFontSize := ADefaultFontSize;
end;

{********************************************************************************************************************************}
constructor TALStyleManager.TDialogStyleInfo.create(const AApplyStyleProc: TDialogApplyStyleProc; const ADefaultFontSize: Single);
begin
  SortOrder := TALStyleManager.GetNextSortOrder;
  ApplyStyleProc := AApplyStyleProc;
  DefaultFontSize := ADefaultFontSize;
end;

{*********************************}
constructor TALStyleManager.Create;
begin
  FLightColors := TDictionary<string, TPair<TAlphaColor, integer{SortOrder}>>.Create;
  FDarkColors := TDictionary<string, TPair<TAlphaColor, integer{SortOrder}>>.Create;
  fColors := TDictionary<string, TAlphaColor>.create;
  FFontFamilies := TDictionary<string, string>.create;
  FTextStyles := TDictionary<String, TTextStyleInfo>.create;
  FEditStyles := TDictionary<String, TEditStyleInfo>.create;
  FMemoStyles := TDictionary<String, TEditStyleInfo>.create;
  FButtonStyles := TDictionary<String, TButtonStyleInfo>.create;
  FCheckBoxStyles := TDictionary<String, TCheckBoxStyleInfo>.create;
  FRadioButtonStyles := TDictionary<String, TRadioButtonStyleInfo>.create;
  FSwitchStyles := TDictionary<String, TSwitchStyleInfo>.create;
  FTrackBarStyles := TDictionary<String, TTrackBarStyleInfo>.create;
  FRangeTrackBarStyles := TDictionary<String, TTrackBarStyleInfo>.create;
  FScrollBarStyles := TDictionary<String, TScrollBarStyleInfo>.create;
  FScrollBoxStyles := TDictionary<String, TScrollBoxStyleInfo>.create;
  FDialogManagerStyles := TDictionary<String, TDialogManagerStyleInfo>.create;
  FDialogStyles := TDictionary<String, TDialogStyleInfo>.create;
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
  AlFreeAndNil(FFontFamilies);
  AlFreeAndNil(FTextStyles);
  AlFreeAndNil(FEditStyles);
  AlFreeAndNil(FMemoStyles);
  AlFreeAndNil(FButtonStyles);
  AlFreeAndNil(FCheckBoxStyles);
  AlFreeAndNil(FRadioButtonStyles);
  AlFreeAndNil(FSwitchStyles);
  AlFreeAndNil(FTrackBarStyles);
  AlFreeAndNil(FRangeTrackBarStyles);
  AlFreeAndNil(FScrollBarStyles);
  AlFreeAndNil(FScrollBoxStyles);
  AlFreeAndNil(FDialogManagerStyles);
  AlFreeAndNil(FDialogStyles);
  inherited;
end;

{***********************************}
procedure TALStyleManager.InitStyles;
begin
  // Note: To generate a new Material 3 color palette, you can use the following tool:
  // https://www.figma.com/community/plugin/1034969338659738588/material-theme-builder
  // Select Export > Material Theme (JSON)

  AddOrSetColor('Material.Color.Primary', $FF6750A4, False); // md.ref.palette.primary40 | Main color used across screens and components
  AddOrSetColor('Material.Color.OnPrimary', $FFFFFFFF, False); // md.ref.palette.primary100 | Text and icons shown against the primary color
  AddOrSetColor('Material.Color.PrimaryContainer', $FFEADDFF, False); // md.ref.palette.primary90 | Standout container color for key components
  AddOrSetColor('Material.Color.OnPrimaryContainer', $FF4F378B, False); // md.ref.palette.primary30 | Contrast-passing color shown against the primary container
  AddOrSetColor('Material.Color.Secondary', $FF625B71, False); // md.ref.palette.secondary40 | Accent color used across screens and components
  AddOrSetColor('Material.Color.OnSecondary', $FFFFFFFF, False); // md.ref.palette.secondary100 | Text and icons shown against the secondary color
  AddOrSetColor('Material.Color.SecondaryContainer', $FFE8DEF8, False); // md.ref.palette.secondary90 | Less prominent container color, for components like tonal buttons
  AddOrSetColor('Material.Color.OnSecondaryContainer', $FF4A4458, False); // md.ref.palette.secondary30 | Contrast-passing color shown against the secondary container
  AddOrSetColor('Material.Color.Tertiary', $FF7D5260, False); // md.ref.palette.tertiary40
  AddOrSetColor('Material.Color.OnTertiary', $FFFFFFFF, False); // md.ref.palette.tertiary100
  AddOrSetColor('Material.Color.TertiaryContainer', $FFFFD8E4, False); // md.ref.palette.tertiary90 | Contrasting container color, for components like input fields
  AddOrSetColor('Material.Color.OnTertiaryContainer', $FF633B48, False); // md.ref.palette.tertiary30 | Contrast-passing color shown against the tertiary container
  AddOrSetColor('Material.Color.Error', $FFB3261E, False); // md.ref.palette.error40 | Indicates errors, such as invalid input in a date picker
  AddOrSetColor('Material.Color.OnError', $FFFFFFFF, False); // md.ref.palette.error100 | Used for text and icons on the error color
  AddOrSetColor('Material.Color.ErrorContainer', $FFF9DEDC, False); // md.ref.palette.error90
  AddOrSetColor('Material.Color.OnErrorContainer', $FF8C1D18, False); // md.ref.palette.error30
  AddOrSetColor('Material.Color.Surface', $FFFEF7FF, False); // md.ref.palette.neutral98 | Surface color for components like cards, sheets, and menus
  AddOrSetColor('Material.Color.OnSurface', $FF1D1B20, False); // md.ref.palette.neutral10 | Text and icons shown against the surface color
  AddOrSetColor('Material.Color.SurfaceVariant', $FFE7E0EC, False); // md.ref.palette.neutral-variant90 | Alternate surface color, can be used for active states
  AddOrSetColor('Material.Color.OnSurfaceVariant', $FF49454F, False); // md.ref.palette.neutral-variant30 | For text and icons to indicate active or inactive component state
  AddOrSetColor('Material.Color.SurfaceContainerHighest', $FFE6E0E9, False); // md.ref.palette.neutral90
  AddOrSetColor('Material.Color.SurfaceContainerHigh', $FFECE6F0, False); // md.ref.palette.neutral92
  AddOrSetColor('Material.Color.SurfaceContainer', $FFF3EDF7, False); // md.ref.palette.neutral94
  AddOrSetColor('Material.Color.SurfaceContainerLow', $FFF7F2FA, False); // md.ref.palette.neutral96
  AddOrSetColor('Material.Color.SurfaceContainerLowest', $FFFFFFFF, False); // md.ref.palette.neutral100
  AddOrSetColor('Material.Color.InverseSurface', $FF322F35, False); // md.ref.palette.neutral20 | Displays opposite color of the surrounding UI
  AddOrSetColor('Material.Color.InverseOnSurface', $FFF5EFF7, False); // md.ref.palette.neutral95 | Used for text and icons shown against the inverse surface color
  AddOrSetColor('Material.Color.SurfaceTint', $FF6750A4, False); // md.ref.palette.primary40
  AddOrSetColor('Material.Color.Outline', $FF79747E, False); // md.ref.palette.neutral-variant50 | Subtle color used for boundaries
  AddOrSetColor('Material.Color.OutlineVariant', $FFCAC4D0, False); // md.ref.palette.neutral-variant80 | Outline-variant is used to define the border of a component where 3:1 contrast ratio isn’t required, a container, or a divider.
  AddOrSetColor('Material.Color.PrimaryFixed', $FFEADDFF, False); // md.ref.palette.primary90
  AddOrSetColor('Material.Color.OnPrimaryFixed', $FF21005D, False); // md.ref.palette.primary10
  AddOrSetColor('Material.Color.PrimaryFixedDim', $FFD0BCFF, False); // md.ref.palette.primary80
  AddOrSetColor('Material.Color.OnPrimaryFixedVariant', $FF4F378B, False); // md.ref.palette.primary30
  AddOrSetColor('Material.Color.InversePrimary', $FFD0BCFF, False); // md.ref.palette.primary80 | Displays opposite of the primary color
  AddOrSetColor('Material.Color.SecondaryFixed', $FFE8DEF8, False); // md.ref.palette.secondary90
  AddOrSetColor('Material.Color.OnSecondaryFixed', $FF1D192B, False); // md.ref.palette.secondary10
  AddOrSetColor('Material.Color.SecondaryFixedDim', $FFCCC2DC, False); // md.ref.palette.secondary80
  AddOrSetColor('Material.Color.OnSecondaryFixedVariant', $FF4A4458, False); // md.ref.palette.secondary30
  AddOrSetColor('Material.Color.TertiaryFixed', $FFFFD8E4, False); // md.ref.palette.tertiary90
  AddOrSetColor('Material.Color.OnTertiaryFixed', $FF31111D, False); // md.ref.palette.tertiary10
  AddOrSetColor('Material.Color.TertiaryFixedDim', $FFEFB8C8, False); // md.ref.palette.tertiary80
  AddOrSetColor('Material.Color.OnTertiaryFixedVariant', $FF633B48, False); // md.ref.palette.tertiary30
  //AddOrSetColor('Material.Color.Background', $FFFEF7FF, False); // md.ref.palette.neutral98 | Note: Background is a legacy color role. It is recommended to use Surface instead of Background.
  //AddOrSetColor('Material.Color.OnBackground', $FF1D1B20, False); // md.ref.palette.neutral10 | Used for text and icons shown against the background color
  AddOrSetColor('Material.Color.SurfaceBright', $FFFEF7FF, False); // md.ref.palette.neutral98
  AddOrSetColor('Material.Color.SurfaceDim', $FFDED8E1, False); // md.ref.palette.neutral87
  AddOrSetColor('Material.Color.Scrim', $FF000000, False); // md.ref.palette.neutral0 | Used for scrims which help separate floating components from the background.
  AddOrSetColor('Material.Color.Shadow', $FF000000, False); // md.ref.palette.neutral0 | For shadows applied to elevated components

  AddOrSetColor('Material.Color.Primary', $FFD0BCFF, True); // md.ref.palette.primary80 | Main color used across screens and components
  AddOrSetColor('Material.Color.OnPrimary', $FF381E72, True); // md.ref.palette.primary20 | Text and icons shown against the primary color
  AddOrSetColor('Material.Color.PrimaryContainer', $FF4F378B, True); // md.ref.palette.primary30 | Standout container color for key components
  AddOrSetColor('Material.Color.OnPrimaryContainer', $FFEADDFF, True); // md.ref.palette.primary90 | Contrast-passing color shown against the primary container
  AddOrSetColor('Material.Color.Secondary', $FFCCC2DC, True); // md.ref.palette.secondary80 | Accent color used across screens and components
  AddOrSetColor('Material.Color.OnSecondary', $FF332D41, True); // md.ref.palette.secondary20 | Text and icons shown against the secondary color
  AddOrSetColor('Material.Color.SecondaryContainer', $FF4A4458, True); // md.ref.palette.secondary30 | Less prominent container color, for components like tonal buttons
  AddOrSetColor('Material.Color.OnSecondaryContainer', $FFE8DEF8, True); // md.ref.palette.secondary90 | Contrast-passing color shown against the secondary container
  AddOrSetColor('Material.Color.Tertiary', $FFEFB8C8, True); // md.ref.palette.tertiary80
  AddOrSetColor('Material.Color.OnTertiary', $FF492532, True); // md.ref.palette.tertiary20
  AddOrSetColor('Material.Color.TertiaryContainer', $FF633B48, True); // md.ref.palette.tertiary30 | Contrasting container color, for components like input fields
  AddOrSetColor('Material.Color.OnTertiaryContainer', $FFFFD8E4, True); // md.ref.palette.tertiary90 | Contrast-passing color shown against the tertiary container
  AddOrSetColor('Material.Color.Error', $FFF2B8B5, True); // md.ref.palette.error80 | Indicates errors, such as invalid input in a date picker
  AddOrSetColor('Material.Color.OnError', $FF601410, True); // md.ref.palette.error20 | Used for text and icons on the error color
  AddOrSetColor('Material.Color.ErrorContainer', $FF8C1D18, True); // md.ref.palette.error30
  AddOrSetColor('Material.Color.OnErrorContainer', $FFF9DEDC, True); // md.ref.palette.error90
  AddOrSetColor('Material.Color.Surface', $FF141218, True); // md.ref.palette.neutral6 | Surface color for components like cards, sheets, and menus
  AddOrSetColor('Material.Color.OnSurface', $FFE6E0E9, True); // md.ref.palette.neutral90 | Text and icons shown against the surface color
  AddOrSetColor('Material.Color.SurfaceVariant', $FF49454F, True); // md.ref.palette.neutral-variant30 | Alternate surface color, can be used for active states
  AddOrSetColor('Material.Color.OnSurfaceVariant', $FFCAC4D0, True); // md.ref.palette.neutral-variant80 | For text and icons to indicate active or inactive component state
  AddOrSetColor('Material.Color.SurfaceContainerHighest', $FF36343B, True); // md.ref.palette.neutral22
  AddOrSetColor('Material.Color.SurfaceContainerHigh', $FF2B2930, True); // md.ref.palette.neutral17
  AddOrSetColor('Material.Color.SurfaceContainer', $FF211F26, True); // md.ref.palette.neutral12
  AddOrSetColor('Material.Color.SurfaceContainerLow', $FF1D1B20, True); // md.ref.palette.neutral10
  AddOrSetColor('Material.Color.SurfaceContainerLowest', $FF0F0D13, True); // md.ref.palette.neutral4
  AddOrSetColor('Material.Color.InverseSurface', $FFE6E0E9, True); // md.ref.palette.neutral90 | Displays opposite color of the surrounding UI
  AddOrSetColor('Material.Color.InverseOnSurface', $FF322F35, True); // md.ref.palette.neutral20 | Used for text and icons shown against the inverse surface color
  AddOrSetColor('Material.Color.SurfaceTint', $FFD0BCFF, True); // md.ref.palette.primary80
  AddOrSetColor('Material.Color.Outline', $FF938F99, True); // md.ref.palette.neutral-variant60 | Subtle color used for boundaries
  AddOrSetColor('Material.Color.OutlineVariant', $FF49454F, True); // md.ref.palette.neutral-variant30 | Outline-variant is used to define the border of a component where 3:1 contrast ratio isn’t required, a container, or a divider.
  AddOrSetColor('Material.Color.PrimaryFixed', $FFEADDFF, True); // md.ref.palette.primary90
  AddOrSetColor('Material.Color.OnPrimaryFixed', $FF21005D, True); // md.ref.palette.primary10
  AddOrSetColor('Material.Color.PrimaryFixedDim', $FFD0BCFF, True); // md.ref.palette.primary80
  AddOrSetColor('Material.Color.OnPrimaryFixedVariant', $FF4F378B, True); // md.ref.palette.primary30
  AddOrSetColor('Material.Color.InversePrimary', $FF6750A4, True); // md.ref.palette.primary40 | Displays opposite of the primary color
  AddOrSetColor('Material.Color.SecondaryFixed', $FFE8DEF8, True); // md.ref.palette.secondary90
  AddOrSetColor('Material.Color.OnSecondaryFixed', $FF1D192B, True); // md.ref.palette.secondary10
  AddOrSetColor('Material.Color.SecondaryFixedDim', $FFCCC2DC, True); // md.ref.palette.secondary80
  AddOrSetColor('Material.Color.OnSecondaryFixedVariant', $FF4A4458, True); // md.ref.palette.secondary30
  AddOrSetColor('Material.Color.TertiaryFixed', $FFFFD8E4, True); // md.ref.palette.tertiary90
  AddOrSetColor('Material.Color.OnTertiaryFixed', $FF31111D, True); // md.ref.palette.tertiary10
  AddOrSetColor('Material.Color.TertiaryFixedDim', $FFEFB8C8, True); // md.ref.palette.tertiary80
  AddOrSetColor('Material.Color.OnTertiaryFixedVariant', $FF633B48, True); // md.ref.palette.tertiary30
  //AddOrSetColor('Material.Color.Background', $FF141218, True); // md.ref.palette.neutral6 | Note: Background is a legacy color role. It is recommended to use Surface instead of Background.
  //AddOrSetColor('Material.Color.OnBackground', $FFE6E0E9, True); // md.ref.palette.neutral90 | Used for text and icons shown against the background color
  AddOrSetColor('Material.Color.SurfaceBright', $FF3B383E, True); // md.ref.palette.neutral24
  AddOrSetColor('Material.Color.SurfaceDim', $FF141218, True); // md.ref.palette.neutral6
  AddOrSetColor('Material.Color.Scrim', $FF000000, True); // md.ref.palette.neutral0 | Used for scrims which help separate floating components from the background.
  AddOrSetColor('Material.Color.Shadow', $FF000000, True); // md.ref.palette.neutral0 | For shadows applied to elevated components

  var LDefaultSystemFontFamily: String;
  {$if defined(ANDROID)}
  // In Android, when you want to use the default system font, you should
  // specify "sans-serif" as the font name. Roboto has been the default font
  // for Android since Android 4.0 (Ice Cream Sandwich), and specifying
  // "sans-serif" in your Java/Kotlin code will use Roboto or whichever font
  // is the system default on the user's device. This approach ensures that
  // your application uses the default system font, which provides a consistent
  // user experience across different devices and versions of Android.
  //   sans-serif
  //   sans-serif-thin
  //   sans-serif-light
  //   sans-serif-medium
  //   sans-serif-black
  //   sans-serif-condensed
  //   sans-serif-smallcaps
  LDefaultSystemFontFamily := 'sans-serif';
  {$ELSEif defined(ALAppleOS)}
  // https://developer.apple.com/fonts/system-fonts/
  //   Helvetica Neue
  //   Helvetica Neue Thin
  //   Helvetica Neue Light
  //   Helvetica Neue Medium
  //   Helvetica Neue Bold
  LDefaultSystemFontFamily := 'Helvetica Neue';
  {$ELSEIF defined(MSWINDOWS)}
  // Segoe UI Light
  // Segoe UI Light
  // Segoe UI Semibold
  // Segoe UI Black
  LDefaultSystemFontFamily := 'Segoe UI';
  {$ELSE}
  LDefaultSystemFontFamily := 'sans-serif';
  {$endif}
  AddOrSetFontFamily('sans-serif', LDefaultSystemFontFamily);
  // Material 3 typography tokens live at:
  // https://m3.material.io/styles/typography/type-scale-tokens
  // I’m not using them here because they conflate font family with size—
  // in our app we often change sizes without swapping families—
  // and, in practice, they all resolve to the same face (“Roboto”).
  AddOrSetFontFamily('Material.Font.Display.Large', LDefaultSystemFontFamily); // md.sys.typescale.display-large.font / md.ref.typeface.brand / Roboto
  AddOrSetFontFamily('Material.Font.Display.Medium', LDefaultSystemFontFamily); // md.sys.typescale.display-medium.font / md.ref.typeface.brand / Roboto
  AddOrSetFontFamily('Material.Font.Display.Small', LDefaultSystemFontFamily); // md.sys.typescale.display-small.font / md.ref.typeface.brand / Roboto
  AddOrSetFontFamily('Material.Font.Headline.Large', LDefaultSystemFontFamily); // md.sys.typescale.headline-large.font / md.ref.typeface.brand / Roboto
  AddOrSetFontFamily('Material.Font.Headline.Medium', LDefaultSystemFontFamily); // md.sys.typescale.headline-medium.font / md.ref.typeface.brand / Roboto
  AddOrSetFontFamily('Material.Font.Headline.Small', LDefaultSystemFontFamily); // md.sys.typescale.headline-small.font / md.ref.typeface.brand / Roboto
  AddOrSetFontFamily('Material.Font.Title.Large', LDefaultSystemFontFamily); // md.sys.typescale.title-large.font / md.ref.typeface.brand / Roboto
  AddOrSetFontFamily('Material.Font.Title.Medium', LDefaultSystemFontFamily); // md.sys.typescale.title-medium.font / md.ref.typeface.plain / Roboto
  AddOrSetFontFamily('Material.Font.Title.Small', LDefaultSystemFontFamily); // md.sys.typescale.title-small.font / md.ref.typeface.plain / Roboto
  AddOrSetFontFamily('Material.Font.Body.Large', LDefaultSystemFontFamily); // md.sys.typescale.body-large.font / md.ref.typeface.plain / Roboto
  AddOrSetFontFamily('Material.Font.Body.Medium', LDefaultSystemFontFamily); // md.sys.typescale.body-medium.font / md.ref.typeface.plain / Roboto
  AddOrSetFontFamily('Material.Font.Body.Small', LDefaultSystemFontFamily); // md.sys.typescale.body-small.font / md.ref.typeface.plain / Roboto
  AddOrSetFontFamily('Material.Font.Label.Large', LDefaultSystemFontFamily); // md.sys.typescale.label-large.font / md.ref.typeface.plain / Roboto
  AddOrSetFontFamily('Material.Font.Label.Medium', LDefaultSystemFontFamily); // md.sys.typescale.label-medium.font / md.ref.typeface.plain / Roboto
  AddOrSetFontFamily('Material.Font.Label.Small', LDefaultSystemFontFamily); // md.sys.typescale.label-small.font / md.ref.typeface.plain / Roboto

  AddOrSetTextStyle('Default', ALResetTextStyle, 14{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Display.Large', ALApplyMaterialTextDisplayLargeStyle, 0{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Display.Medium', ALApplyMaterialTextDisplayMediumStyle, 0{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Display.Small', ALApplyMaterialTextDisplaySmallStyle, 0{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Headline.Large', ALApplyMaterialTextHeadlineLargeStyle, 0{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Headline.Medium', ALApplyMaterialTextHeadlineMediumStyle, 0{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Headline.Small', ALApplyMaterialTextHeadlineSmallStyle, 0{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Title.Large', ALApplyMaterialTextTitleLargeStyle, 0{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Title.Medium', ALApplyMaterialTextTitleMediumStyle, 0{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Title.Small', ALApplyMaterialTextTitleSmallStyle, 0{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Body.Large', ALApplyMaterialTextBodyLargeStyle, 0{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Body.Medium', ALApplyMaterialTextBodyMediumStyle, 0{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Body.Small', ALApplyMaterialTextBodySmallStyle, 0{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Label.Large', ALApplyMaterialTextLabelLargeStyle, 0{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Label.Medium', ALApplyMaterialTextLabelMediumStyle, 0{ADefaultFontSize});
  AddOrSetTextStyle('Material.Text.Label.Small', ALApplyMaterialTextLabelSmallStyle, 0{ADefaultFontSize});

  AddOrSetEditStyle('Default', ALResetEditStyle, 16{ADefaultFontSize});
  AddOrSetEditStyle('Material.Edit.Filled', ALApplyMaterialEditFilledStyle, 16{ADefaultFontSize});
  AddOrSetEditStyle('Material.Edit.Filled.Error', ALApplyMaterialEditFilledErrorStyle, 16{ADefaultFontSize});
  AddOrSetEditStyle('Material.Edit.Outlined', ALApplyMaterialEditOutlinedStyle, 16{ADefaultFontSize});
  AddOrSetEditStyle('Material.Edit.Outlined.Error', ALApplyMaterialEditOutlinedErrorStyle, 16{ADefaultFontSize});
  AddOrSetEditStyle('Material.Edit.Hybrid', ALApplyMaterialEditHybridStyle, 16{ADefaultFontSize});
  AddOrSetEditStyle('Material.Edit.Hybrid.Error', ALApplyMaterialEditHybridErrorStyle, 16{ADefaultFontSize});

  AddOrSetMemoStyle('Default', ALResetEditStyle, 16{ADefaultFontSize});
  AddOrSetMemoStyle('Material.Memo.Filled', ALApplyMaterialEditFilledStyle, 16{ADefaultFontSize});
  AddOrSetMemoStyle('Material.Memo.Filled.Error', ALApplyMaterialEditFilledErrorStyle, 16{ADefaultFontSize});
  AddOrSetMemoStyle('Material.Memo.Outlined', ALApplyMaterialEditOutlinedStyle, 16{ADefaultFontSize});
  AddOrSetMemoStyle('Material.Memo.Outlined.Error', ALApplyMaterialEditOutlinedErrorStyle, 16{ADefaultFontSize});
  AddOrSetMemoStyle('Material.Memo.Hybrid', ALApplyMaterialEditHybridStyle, 16{ADefaultFontSize});
  AddOrSetMemoStyle('Material.Memo.Hybrid.Error', ALApplyMaterialEditHybridErrorStyle, 16{ADefaultFontSize});

  AddOrSetButtonStyle('Default', ALResetButtonStyle, 14{ADefaultFontSize});
  AddOrSetButtonStyle('Material.Button.Filled', ALApplyMaterialButtonFilledStyle, 14{ADefaultFontSize});
  AddOrSetButtonStyle('Material.Button.Outlined', ALApplyMaterialButtonOutlinedStyle, 14{ADefaultFontSize});
  AddOrSetButtonStyle('Material.Button.Text', ALApplyMaterialButtonTextStyle, 14{ADefaultFontSize});
  AddOrSetButtonStyle('Material.Button.Elevated', ALApplyMaterialButtonElevatedStyle, 14{ADefaultFontSize});
  AddOrSetButtonStyle('Material.Button.Tonal', ALApplyMaterialButtonTonalStyle, 14{ADefaultFontSize});
  AddOrSetButtonIconStyle('Material.Button.Icon.Filled', ALApplyMaterialButtonIconFilledStyle, 40{ADefaultHeight});
  AddOrSetButtonIconStyle('Material.Button.Icon.Outlined', ALApplyMaterialButtonIconOutlinedStyle, 40{ADefaultHeight});
  AddOrSetButtonIconStyle('Material.Button.Icon.Standard', ALApplyMaterialButtonIconStandardStyle, 40{ADefaultHeight});
  AddOrSetButtonIconStyle('Material.Button.Icon.Tonal', ALApplyMaterialButtonIconTonalStyle, 40{ADefaultHeight});

  AddOrSetCheckBoxStyle('Default', ALResetCheckBoxStyle, 18{ADefaultHeight});
  AddOrSetCheckBoxStyle('Material.CheckBox', ALApplyMaterialCheckBoxStyle, 18{ADefaultHeight});
  AddOrSetCheckBoxStyle('Material.CheckBox.Error', ALApplyMaterialCheckBoxErrorStyle, 18{ADefaultHeight});

  AddOrSetRadioButtonStyle('Default', ALResetRadioButtonStyle, 20{ADefaultHeight});
  AddOrSetRadioButtonStyle('Material.RadioButton', ALApplyMaterialRadioButtonStyle, 20{ADefaultHeight});
  AddOrSetRadioButtonStyle('Material.RadioButton.Error', ALApplyMaterialRadioButtonErrorStyle, 20{ADefaultHeight});

  AddOrSetSwitchStyle('Default', ALResetSwitchStyle, 32{ADefaultHeight});
  AddOrSetSwitchStyle('Material.Switch', ALApplyMaterialSwitchStyle, 32{ADefaultHeight});

  AddOrSetTrackBarStyle('Default', ALResetTrackBarStyle, 32{ADefaultSize});
  AddOrSetTrackBarStyle('Material.TrackBar', ALApplyMaterialTrackBarStyle, 44{ADefaultSize});

  AddOrSetRangeTrackBarStyle('Default', ALResetTrackBarStyle, 32{ADefaultSize});
  AddOrSetRangeTrackBarStyle('Material.RangeTrackBar', ALApplyMaterialTrackBarStyle, 44{ADefaultSize});

  AddOrSetScrollBarStyle('Default', ALResetScrollBarStyle, 18{ADefaultSize});
  AddOrSetScrollBarStyle('Material.ScrollBar', ALApplyMaterialScrollBarStyle, 18{ADefaultSize});

  AddOrSetScrollBoxStyle('Default', ALResetScrollBoxStyle);
  AddOrSetScrollBoxStyle('Material.ScrollBox', ALApplyMaterialScrollBoxStyle);

  AddOrSetDialogManagerStyle('Default', ALResetDialogManagerStyle, 14{ADefaultFontSize});
  AddOrSetDialogManagerStyle('Material.DialogManager', ALApplyMaterialDialogManagerStyle, 14{ADefaultFontSize});
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

{**************************************************************************************}
procedure TALStyleManager.AddOrSetFontFamily(const AName: String; Const AValue: String);
begin
  FFontFamilies.AddOrSetValue(AName, AValue);
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
  FButtonStyles.AddOrSetValue(AName, TButtonStyleInfo.create(AApplyStyleProc, ADefaultFontSize, 0{ADefaultHeight}));
end;

{*************************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetButtonIconStyle(const AName: String; const AApplyStyleProc: TButtonApplyStyleProc; const ADefaultHeight: Single);
begin
  FButtonStyles.AddOrSetValue(AName, TButtonStyleInfo.create(AApplyStyleProc, 0{ADefaultFontSize}, ADefaultHeight));
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

{***********************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetTrackBarStyle(const AName: String; const AApplyStyleProc: TTrackBarApplyStyleProc; const ADefaultSize: Single);
begin
  FTrackBarStyles.AddOrSetValue(AName, TTrackBarStyleInfo.create(AApplyStyleProc, ADefaultSize));
end;

{****************************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetRangeTrackBarStyle(const AName: String; const AApplyStyleProc: TTrackBarApplyStyleProc; const ADefaultSize: Single);
begin
  FRangeTrackBarStyles.AddOrSetValue(AName, TTrackBarStyleInfo.create(AApplyStyleProc, ADefaultSize));
end;

{*************************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetScrollBarStyle(const AName: String; const AApplyStyleProc: TScrollBarApplyStyleProc; const ADefaultSize: Single);
begin
  FScrollBarStyles.AddOrSetValue(AName, TScrollBarStyleInfo.create(AApplyStyleProc, ADefaultSize));
end;

{*********************************************************************************************************************}
procedure TALStyleManager.AddOrSetScrollBoxStyle(const AName: String; const AApplyStyleProc: TScrollBoxApplyStyleProc);
begin
  FScrollBoxStyles.AddOrSetValue(AName, TScrollBoxStyleInfo.create(AApplyStyleProc));
end;

{*************************************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetDialogManagerStyle(const AName: String; const AApplyStyleProc: TDialogManagerApplyStyleProc; const ADefaultFontSize: Single);
begin
  FDialogManagerStyles.AddOrSetValue(AName, TDialogManagerStyleInfo.create(AApplyStyleProc, ADefaultFontSize));
end;

{***********************************************************************************************************************************************}
procedure TALStyleManager.AddOrSetDialogStyle(const AName: String; const AApplyStyleProc: TDialogApplyStyleProc; const ADefaultFontSize: Single);
begin
  FDialogStyles.AddOrSetValue(AName, TDialogStyleInfo.create(AApplyStyleProc, ADefaultFontSize));
end;

{******************************************************************}
function TALStyleManager.GetColor(const AName: String): TAlphaColor;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _TryGetValueWithAlpha(out AValue: TAlphacolor): Boolean;
  begin
    // Ex: Material.Color.Primary.Alpha08
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

{******************************************************************}
function TALStyleManager.GetFontFamily(const AName: String): String;
begin
  if (not FFontFamilies.TryGetValue(AName, Result)) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Font "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit('');
  end;
end;

{***********************************************************************************************************}
procedure TALStyleManager.ApplyTextStyle(const AName: String; const AText: TALText; const AFontSize: Single);
begin
  Var LApplyTextStyleInfo: TTextStyleInfo;
  if not fTextStyles.TryGetValue(AName, LApplyTextStyleInfo) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Style "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit;
  end;

  AText.BeginUpdate;
  try
    LApplyTextStyleInfo.ApplyStyleProc(AText, AFontSize / LApplyTextStyleInfo.DefaultFontSize);
    {$IF not defined(ALDPK)}
    if AText.AutoAlignToPixel then AText.AlignToPixel;
    {$ENDIF}
  finally
    AText.EndUpdate;
  end;
end;

{**********************************************************************************}
procedure TALStyleManager.ApplyTextStyle(const AName: String; const AText: TALText);
begin
  {$IF defined(ALDPK)}
  Var LApplyTextStyleInfo: TTextStyleInfo;
  if not fTextStyles.TryGetValue(AName, LApplyTextStyleInfo) then Exit;
  var LRatio: Single := ALGetStyleRatio('Please enter the desired font size', LApplyTextStyleInfo.DefaultFontSize, LApplyTextStyleInfo.DefaultFontSize);
  ApplyTextStyle(AName, AText, LApplyTextStyleInfo.DefaultFontSize * LRatio);
  {$ELSE}
  ApplyTextStyle(AName, AText, AText.TextSettings.Font.Size);
  {$ENDIF}
end;

{***************************************************************************************************************}
procedure TALStyleManager.ApplyEditStyle(const AName: String; const AEdit: TALBaseEdit; const AFontSize: Single);
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
    LApplyEditStyleInfo.ApplyStyleProc(AEdit, AFontSize / LApplyEditStyleInfo.DefaultFontSize);
    {$IF not defined(ALDPK)}
    if AEdit.AutoAlignToPixel then AEdit.AlignToPixel;
    {$ENDIF}
  finally
    AEdit.EndUpdate;
  end;
end;

{**************************************************************************************}
procedure TALStyleManager.ApplyEditStyle(const AName: String; const AEdit: TALBaseEdit);
begin
  {$IF defined(ALDPK)}
  Var LApplyEditStyleInfo: TEditStyleInfo;
  if not fEditStyles.TryGetValue(AName, LApplyEditStyleInfo) then Exit;
  var LRatio: Single := ALGetStyleRatio('Please enter the desired font size', LApplyEditStyleInfo.DefaultFontSize, LApplyEditStyleInfo.DefaultFontSize);
  ApplyEditStyle(AName, AEdit, LApplyEditStyleInfo.DefaultFontSize * LRatio);
  {$ELSE}
  ApplyEditStyle(AName, AEdit, AEdit.TextSettings.Font.Size);
  {$ENDIF}
end;

{***************************************************************************************************************}
procedure TALStyleManager.ApplyMemoStyle(const AName: String; const AMemo: TALBaseEdit; const AFontSize: Single);
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
    LApplyMemoStyleInfo.ApplyStyleProc(AMemo, AFontSize / LApplyMemoStyleInfo.DefaultFontSize);
    {$IF not defined(ALDPK)}
    if AMemo.AutoAlignToPixel then AMemo.AlignToPixel;
    {$ENDIF}
  finally
    AMemo.EndUpdate;
  end;
end;

{**************************************************************************************}
procedure TALStyleManager.ApplyMemoStyle(const AName: String; const AMemo: TALBaseEdit);
begin
  {$IF defined(ALDPK)}
  Var LApplyMemoStyleInfo: TEditStyleInfo;
  if not fMemoStyles.TryGetValue(AName, LApplyMemoStyleInfo) then Exit;
  var LRatio: Single := ALGetStyleRatio('Please enter the desired font size', LApplyMemoStyleInfo.DefaultFontSize, LApplyMemoStyleInfo.DefaultFontSize);
  ApplyMemoStyle(AName, AMemo, LApplyMemoStyleInfo.DefaultFontSize * LRatio);
  {$ELSE}
  ApplyMemoStyle(AName, AMemo, AMemo.TextSettings.Font.Size);
  {$ENDIF}
end;

{*****************************************************************************************************************}
procedure TALStyleManager.ApplyButtonStyle(const AName: String; const AButton: TALButton; const AFontSize: Single);
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
    LApplyButtonStyleInfo.ApplyStyleProc(AButton, AFontSize / LApplyButtonStyleInfo.DefaultFontSize);
    {$IF not defined(ALDPK)}
    if AButton.AutoAlignToPixel then AButton.AlignToPixel;
    {$ENDIF}
  finally
    AButton.EndUpdate;
  end;
end;

{*******************************************************************************************************************}
procedure TALStyleManager.ApplyButtonIconStyle(const AName: String; const AButton: TALButton; const AHeight: Single);
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
    LApplyButtonStyleInfo.ApplyStyleProc(AButton, AHeight / LApplyButtonStyleInfo.DefaultHeight);
    {$IF not defined(ALDPK)}
    if AButton.AutoAlignToPixel then AButton.AlignToPixel;
    {$ENDIF}
  finally
    AButton.EndUpdate;
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
  if LApplyButtonStyleInfo.DefaultFontSize > 0 then begin
    {$IF defined(ALDPK)}
    var LRatio: Single := ALGetStyleRatio('Please enter the desired font size', LApplyButtonStyleInfo.DefaultFontSize, LApplyButtonStyleInfo.DefaultFontSize);
    ApplyButtonStyle(AName, AButton, LApplyButtonStyleInfo.DefaultFontSize * LRatio);
    {$ELSE}
    ApplyButtonStyle(AName, AButton, AButton.TextSettings.Font.Size);
    {$ENDIF}
  end
  else begin
    {$IF defined(ALDPK)}
    var LRatio: Single := ALGetStyleRatio('Please enter the desired height', LApplyButtonStyleInfo.DefaultHeight, LApplyButtonStyleInfo.DefaultHeight);
    ApplyButtonIconStyle(AName, AButton, LApplyButtonStyleInfo.DefaultHeight * LRatio);
    {$ELSE}
    ApplyButtonIconStyle(AName, AButton, AButton.Height);
    {$ENDIF}
  end;
end;

{*********************************************************************************************************************}
procedure TALStyleManager.ApplyCheckBoxStyle(const AName: String; const ACheckBox: TALCheckBox; const AHeight: Single);
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
    LApplyCheckBoxStyleInfo.ApplyStyleProc(ACheckBox, AHeight / LApplyCheckBoxStyleInfo.DefaultHeight);
    {$IF not defined(ALDPK)}
    if ACheckBox.AutoAlignToPixel then ACheckBox.AlignToPixel;
    {$ENDIF}
  finally
    ACheckBox.EndUpdate;
  end;
end;

{**********************************************************************************************}
procedure TALStyleManager.ApplyCheckBoxStyle(const AName: String; const ACheckBox: TALCheckBox);
begin
  {$IF defined(ALDPK)}
  Var LApplyCheckBoxStyleInfo: TCheckBoxStyleInfo;
  if not fCheckBoxStyles.TryGetValue(AName, LApplyCheckBoxStyleInfo) then Exit;
  var LRatio: Single := ALGetStyleRatio('Please enter the desired height', LApplyCheckBoxStyleInfo.DefaultHeight, LApplyCheckBoxStyleInfo.DefaultHeight);
  ApplyCheckBoxStyle(AName, ACheckBox, LApplyCheckBoxStyleInfo.DefaultHeight * LRatio);
  {$ELSE}
  ApplyCheckBoxStyle(AName, ACheckBox, ACheckBox.Height);
  {$ENDIF}
end;

{******************************************************************************************************************************}
procedure TALStyleManager.ApplyRadioButtonStyle(const AName: String; const ARadioButton: TALRadioButton; const AHeight: Single);
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
    LApplyRadioButtonStyleInfo.ApplyStyleProc(ARadioButton, AHeight / LApplyRadioButtonStyleInfo.DefaultHeight);
    {$IF not defined(ALDPK)}
    if ARadioButton.AutoAlignToPixel then ARadioButton.AlignToPixel;
    {$ENDIF}
  finally
    ARadioButton.EndUpdate;
  end;
end;

{*******************************************************************************************************}
procedure TALStyleManager.ApplyRadioButtonStyle(const AName: String; const ARadioButton: TALRadioButton);
begin
  {$IF defined(ALDPK)}
  Var LApplyRadioButtonStyleInfo: TRadioButtonStyleInfo;
  if not fRadioButtonStyles.TryGetValue(AName, LApplyRadioButtonStyleInfo) then Exit;
  var LRatio: Single := ALGetStyleRatio('Please enter the desired height', LApplyRadioButtonStyleInfo.DefaultHeight, LApplyRadioButtonStyleInfo.DefaultHeight);
  ApplyRadioButtonStyle(AName, ARadioButton, LApplyRadioButtonStyleInfo.DefaultHeight * LRatio);
  {$ELSE}
  ApplyRadioButtonStyle(AName, ARadioButton, ARadioButton.Height);
  {$ENDIF}
end;

{***************************************************************************************************************}
procedure TALStyleManager.ApplySwitchStyle(const AName: String; const ASwitch: TALSwitch; const AHeight: Single);
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
    LApplySwitchStyleInfo.ApplyStyleProc(ASwitch, AHeight / LApplySwitchStyleInfo.DefaultHeight);
    {$IF not defined(ALDPK)}
    if ASwitch.AutoAlignToPixel then ASwitch.AlignToPixel;
    {$ENDIF}
  finally
    ASwitch.EndUpdate;
  end;
end;

{****************************************************************************************}
procedure TALStyleManager.ApplySwitchStyle(const AName: String; const ASwitch: TALSwitch);
begin
  {$IF defined(ALDPK)}
  Var LApplySwitchStyleInfo: TSwitchStyleInfo;
  if not fSwitchStyles.TryGetValue(AName, LApplySwitchStyleInfo) then Exit;
  var LRatio: Single := ALGetStyleRatio('Please enter the desired height', LApplySwitchStyleInfo.DefaultHeight, LApplySwitchStyleInfo.DefaultHeight);
  ApplySwitchStyle(AName, ASwitch, LApplySwitchStyleInfo.DefaultHeight * LRatio);
  {$ELSE}
  ApplySwitchStyle(AName, ASwitch, ASwitch.Height);
  {$ENDIF}
end;

{**********************************************************************************************************************}
procedure TALStyleManager.ApplyTrackBarStyle(const AName: String; const ATrackBar: TALCustomTrack; const ASize: Single);
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
    LApplyTrackBarStyleInfo.ApplyStyleProc(ATrackBar, ASize / LApplyTrackBarStyleInfo.DefaultSize);
    {$IF not defined(ALDPK)}
    if ATrackBar.AutoAlignToPixel then ATrackBar.AlignToPixel;
    {$ENDIF}
  finally
    ATrackBar.EndUpdate;
  end;
end;

{*************************************************************************************************}
procedure TALStyleManager.ApplyTrackBarStyle(const AName: String; const ATrackBar: TALCustomTrack);
begin
  {$IF defined(ALDPK)}
  Var LApplyTrackBarStyleInfo: TTrackBarStyleInfo;
  if not fTrackBarStyles.TryGetValue(AName, LApplyTrackBarStyleInfo) then Exit;
  var LRatio: Single;
  If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then
    LRatio := ALGetStyleRatio('Please enter the desired height', LApplyTrackBarStyleInfo.DefaultSize, LApplyTrackBarStyleInfo.DefaultSize)
  else
    LRatio := ALGetStyleRatio('Please enter the desired width', LApplyTrackBarStyleInfo.DefaultSize, LApplyTrackBarStyleInfo.DefaultSize);
  ApplyTrackBarStyle(AName, ATrackBar, LApplyTrackBarStyleInfo.DefaultSize * LRatio);
  {$ELSE}
  If ATrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then
    ApplyTrackBarStyle(AName, ATrackBar, ATrackBar.Height)
  else
    ApplyTrackBarStyle(AName, ATrackBar, ATrackBar.Width);
  {$ENDIF}
end;

{********************************************************************************************************************************}
procedure TALStyleManager.ApplyRangeTrackBarStyle(const AName: String; const ARangeTrackBar: TALCustomTrack; const ASize: Single);
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
    LApplyRangeTrackBarStyleInfo.ApplyStyleProc(ARangeTrackBar, ASize / LApplyRangeTrackBarStyleInfo.DefaultSize);
    {$IF not defined(ALDPK)}
    if ARangeTrackBar.AutoAlignToPixel then ARangeTrackBar.AlignToPixel;
    {$ENDIF}
  finally
    ARangeTrackBar.EndUpdate;
  end;
end;

{***********************************************************************************************************}
procedure TALStyleManager.ApplyRangeTrackBarStyle(const AName: String; const ARangeTrackBar: TALCustomTrack);
begin
  {$IF defined(ALDPK)}
  Var LApplyRangeTrackBarStyleInfo: TTrackBarStyleInfo;
  if not fRangeTrackBarStyles.TryGetValue(AName, LApplyRangeTrackBarStyleInfo) then Exit;
  var LRatio: Single;
  If ARangeTrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then
    LRatio := ALGetStyleRatio('Please enter the desired height', LApplyRangeTrackBarStyleInfo.DefaultSize, LApplyRangeTrackBarStyleInfo.DefaultSize)
  else
    LRatio := ALGetStyleRatio('Please enter the desired width', LApplyRangeTrackBarStyleInfo.DefaultSize, LApplyRangeTrackBarStyleInfo.DefaultSize);
  ApplyRangeTrackBarStyle(AName, ARangeTrackBar, LApplyRangeTrackBarStyleInfo.DefaultSize * LRatio);
  {$ELSE}
  If ARangeTrackBar.Orientation = FMX.Controls.TOrientation.Horizontal then
    ApplyRangeTrackBarStyle(AName, ARangeTrackBar, ARangeTrackBar.Height)
  else
    ApplyRangeTrackBarStyle(AName, ARangeTrackBar, ARangeTrackBar.Width);
  {$ENDIF}
end;

{****************************************************************************************************************************}
procedure TALStyleManager.ApplyScrollBarStyle(const AName: String; const AScrollBar: TALCustomScrollBar; const ASize: Single);
begin
  Var LApplyScrollBarStyleInfo: TScrollBarStyleInfo;
  if not fScrollBarStyles.TryGetValue(AName, LApplyScrollBarStyleInfo) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Style "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit;
  end;

  AScrollBar.BeginUpdate;
  try
    LApplyScrollBarStyleInfo.ApplyStyleProc(AScrollBar, ASize / LApplyScrollBarStyleInfo.DefaultSize);
    {$IF not defined(ALDPK)}
    if AScrollBar.AutoAlignToPixel then AScrollBar.AlignToPixel;
    {$ENDIF}
  finally
    AScrollBar.EndUpdate;
  end;
end;

{*******************************************************************************************************}
procedure TALStyleManager.ApplyScrollBarStyle(const AName: String; const AScrollBar: TALCustomScrollBar);
begin
  {$IF defined(ALDPK)}
  Var LApplyScrollBarStyleInfo: TScrollBarStyleInfo;
  if not fScrollBarStyles.TryGetValue(AName, LApplyScrollBarStyleInfo) then Exit;
  var LRatio: Single;
  If AScrollBar.Orientation = FMX.Controls.TOrientation.Horizontal then
    LRatio := ALGetStyleRatio('Please enter the desired height', LApplyScrollBarStyleInfo.DefaultSize, LApplyScrollBarStyleInfo.DefaultSize)
  else
    LRatio := ALGetStyleRatio('Please enter the desired width', LApplyScrollBarStyleInfo.DefaultSize, LApplyScrollBarStyleInfo.DefaultSize);
  ApplyScrollBarStyle(AName, AScrollBar, LApplyScrollBarStyleInfo.DefaultSize * LRatio);
  {$ELSE}
  If AScrollBar.Orientation = FMX.Controls.TOrientation.Horizontal then
    ApplyScrollBarStyle(AName, AScrollBar, AScrollBar.Height)
  else
    ApplyScrollBarStyle(AName, AScrollBar, AScrollBar.Width);
  {$ENDIF}
end;

{*******************************************************************************************************}
procedure TALStyleManager.ApplyScrollBoxStyle(const AName: String; const AScrollBox: TALCustomScrollBox);
begin
  Var LApplyScrollBoxStyleInfo: TScrollBoxStyleInfo;
  if not fScrollBoxStyles.TryGetValue(AName, LApplyScrollBoxStyleInfo) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Style "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit;
  end;

  AScrollBox.BeginUpdate;
  try
    LApplyScrollBoxStyleInfo.ApplyStyleProc(AScrollBox);
    {$IF not defined(ALDPK)}
    if AScrollBox.AutoAlignToPixel then AScrollBox.AlignToPixel;
    {$ENDIF}
  finally
    AScrollBox.EndUpdate;
  end;
end;

{**************************************************************************************************************************************}
procedure TALStyleManager.ApplyDialogManagerStyle(const AName: String; const ADialogManager: TALDialogManager; const AFontSize: Single);
begin
  Var LApplyDialogManagerStyleInfo: TDialogManagerStyleInfo;
  if not fDialogManagerStyles.TryGetValue(AName, LApplyDialogManagerStyleInfo) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Style "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit;
  end;

  LApplyDialogManagerStyleInfo.ApplyStyleProc(ADialogManager, AFontSize / LApplyDialogManagerStyleInfo.DefaultFontSize);
  {$IF not defined(ALDPK)}
  if ADialogManager.DefaultScrim.AutoAlignToPixel then ADialogManager.DefaultScrim.AlignToPixel;
  if ADialogManager.DefaultContainer.AutoAlignToPixel then ADialogManager.DefaultContainer.AlignToPixel;
  if ADialogManager.DefaultIcon.AutoAlignToPixel then ADialogManager.DefaultIcon.AlignToPixel;
  if ADialogManager.DefaultHeadline.AutoAlignToPixel then ADialogManager.DefaultHeadline.AlignToPixel;
  if ADialogManager.DefaultContent.AutoAlignToPixel then ADialogManager.DefaultContent.AlignToPixel;
  if ADialogManager.DefaultMessage.AutoAlignToPixel then ADialogManager.DefaultMessage.AlignToPixel;
  if ADialogManager.DefaultOptionLayout.AutoAlignToPixel then ADialogManager.DefaultOptionLayout.AlignToPixel;
  if ADialogManager.DefaultRadioButton.AutoAlignToPixel then ADialogManager.DefaultRadioButton.AlignToPixel;
  if ADialogManager.DefaultCheckBox.AutoAlignToPixel then ADialogManager.DefaultCheckBox.AlignToPixel;
  if ADialogManager.DefaultInlineButton.AutoAlignToPixel then ADialogManager.DefaultInlineButton.AlignToPixel;
  if ADialogManager.DefaultEdit.AutoAlignToPixel then ADialogManager.DefaultEdit.AlignToPixel;
  if ADialogManager.DefaultMemo.AutoAlignToPixel then ADialogManager.DefaultMemo.AlignToPixel;
  if ADialogManager.DefaultLabel.AutoAlignToPixel then ADialogManager.DefaultLabel.AlignToPixel;
  if ADialogManager.DefaultFooterBar.AutoAlignToPixel then ADialogManager.DefaultFooterBar.AlignToPixel;
  if ADialogManager.DefaultFooterButton.AutoAlignToPixel then ADialogManager.DefaultFooterButton.AlignToPixel;
  {$ENDIF}
end;

{*************************************************************************************************************}
procedure TALStyleManager.ApplyDialogManagerStyle(const AName: String; const ADialogManager: TALDialogManager);
begin
  {$IF defined(ALDPK)}
  Var LApplyDialogManagerStyleInfo: TDialogManagerStyleInfo;
  if not fDialogManagerStyles.TryGetValue(AName, LApplyDialogManagerStyleInfo) then Exit;
  var LRatio: Single := ALGetStyleRatio('Please enter the desired font size', LApplyDialogManagerStyleInfo.DefaultFontSize, LApplyDialogManagerStyleInfo.DefaultFontSize);
  ApplyDialogManagerStyle(AName, ADialogManager, LApplyDialogManagerStyleInfo.DefaultFontSize * LRatio);
  {$ELSE}
  ApplyDialogManagerStyle(AName, ADialogManager, ADialogManager.DefaultMessage.TextSettings.Font.Size);
  {$ENDIF}
end;

{*****************************************************************************************************************}
procedure TALStyleManager.ApplyDialogStyle(const AName: String; const ADialog: TALDialog; const AFontSize: Single);
begin
  Var LApplyDialogStyleInfo: TDialogStyleInfo;
  if not fDialogStyles.TryGetValue(AName, LApplyDialogStyleInfo) then begin
    {$IF not defined(ALDPK)}
    ALLog('TALStyleManager', 'Style "%s" not found in style manager', [AName], TALLogType.ERROR);
    {$ENDIF}
    Exit;
  end;

  ADialog.BeginUpdate;
  try
    LApplyDialogStyleInfo.ApplyStyleProc(ADialog, AFontSize / LApplyDialogStyleInfo.DefaultFontSize);
    {$IF not defined(ALDPK)}
    if ADialog.AutoAlignToPixel then ADialog.AlignToPixel;
    {$ENDIF}
  finally
    ADialog.EndUpdate;
  end;
end;

{****************************************************************************************}
procedure TALStyleManager.ApplyDialogStyle(const AName: String; const ADialog: TALDialog);
begin
  {$IF defined(ALDPK)}
  Var LApplyDialogStyleInfo: TDialogStyleInfo;
  if not fDialogStyles.TryGetValue(AName, LApplyDialogStyleInfo) then Exit;
  var LRatio: Single := ALGetStyleRatio('Please enter the desired font size', LApplyDialogStyleInfo.DefaultFontSize, LApplyDialogStyleInfo.DefaultFontSize);
  ApplyDialogStyle(AName, ADialog, LApplyDialogStyleInfo.DefaultFontSize * LRatio);
  {$ELSE}
  ApplyDialogStyle(AName, ADialog, ADialog.Message.TextSettings.Font.Size);
  {$ENDIF}
end;

{*****************************************************}
function TALStyleManager.GetColorNames: TArray<String>;
begin
  var LArray := FLightColors.ToArray;
  TArray.Sort<TPair<string, TPair<TAlphaColor, integer{SortOrder}>>>(LArray,
    TComparer<TPair<string, TPair<TAlphaColor, integer{SortOrder}>>>.Construct(
                                                                       function(const Left, Right: TPair<string, TPair<TAlphaColor, integer{SortOrder}>>): Integer
                                                                       begin
                                                                         Result := Left.value.value - Right.value.value;
                                                                       end));
  SetLength(Result, Length(LArray));
  for var I := low(LArray) to High(LArray) do
    Result[I] := LArray[I].Key;
end;

{**********************************************************}
function TALStyleManager.GetFontFamilyNames: TArray<String>;
begin
  Result := FFontFamilies.Keys.ToArray;
  TArray.Sort<String>(Result,
    TComparer<String>.Construct(
      function(const Left, Right: String): Integer
      begin
        Result := ALCompareStrW(Left, Right);
      end));
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

{**************************************************************}
function TALStyleManager.GetScrollBarStyleNames: TArray<String>;
begin
  var LArray := FScrollBarStyles.ToArray;
  TArray.Sort<TPair<String, TScrollBarStyleInfo>>(LArray,
    TComparer<TPair<String, TScrollBarStyleInfo>>.Construct(
      function(const Left, Right: TPair<String, TScrollBarStyleInfo>): Integer
      begin
        Result := Left.value.SortOrder - Right.value.SortOrder;
      end));
  SetLength(Result, Length(LArray));
  for var I := low(LArray) to High(LArray) do
    Result[I] := LArray[I].Key;
end;

{**************************************************************}
function TALStyleManager.GetScrollBoxStyleNames: TArray<String>;
begin
  var LArray := FScrollBoxStyles.ToArray;
  TArray.Sort<TPair<String, TScrollBoxStyleInfo>>(LArray,
    TComparer<TPair<String, TScrollBoxStyleInfo>>.Construct(
      function(const Left, Right: TPair<String, TScrollBoxStyleInfo>): Integer
      begin
        Result := Left.value.SortOrder - Right.value.SortOrder;
      end));
  SetLength(Result, Length(LArray));
  for var I := low(LArray) to High(LArray) do
    Result[I] := LArray[I].Key;
end;

{******************************************************************}
function TALStyleManager.GetDialogManagerStyleNames: TArray<String>;
begin
  var LArray := FDialogManagerStyles.ToArray;
  TArray.Sort<TPair<String, TDialogManagerStyleInfo>>(LArray,
    TComparer<TPair<String, TDialogManagerStyleInfo>>.Construct(
      function(const Left, Right: TPair<String, TDialogManagerStyleInfo>): Integer
      begin
        Result := Left.value.SortOrder - Right.value.SortOrder;
      end));
  SetLength(Result, Length(LArray));
  for var I := low(LArray) to High(LArray) do
    Result[I] := LArray[I].Key;
end;

{***********************************************************}
function TALStyleManager.GetDialogStyleNames: TArray<String>;
begin
  var LArray := FDialogStyles.ToArray;
  TArray.Sort<TPair<String, TDialogStyleInfo>>(LArray,
    TComparer<TPair<String, TDialogStyleInfo>>.Construct(
      function(const Left, Right: TPair<String, TDialogStyleInfo>): Integer
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
