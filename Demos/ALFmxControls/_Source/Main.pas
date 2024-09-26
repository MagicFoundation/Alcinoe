﻿unit Main;

{$I Alcinoe.inc}

interface

uses
  System.Messaging,
  System.Generics.Collections,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.ImageList,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Objects,
  FMX.Layouts,
  FMX.types3D,
  FMX.ImgList,
  FMX.ScrollBox,
  FMX.Edit,
  FMX.Effects,
  FMX.Filter.Effects,
  Alcinoe.FMX.Themes,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.TabControl,
  Alcinoe.FMX.Edit,
  Alcinoe.FMX.VideoPlayer,
  Alcinoe.FMX.DatePickerDialog,
  Alcinoe.FMX.ErrorReporting,
  Alcinoe.FMX.BreakText,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Memo,
  Alcinoe.FMX.Controls;

type

  {*****************************************}
  TALRectangleStopWatch = Class(TALRectangle)
  protected
     PaintMs: double;
     MakeBufDrawableMs: double;
     procedure Paint; override;
  public
    procedure MakeBufDrawable; override;
  End;

  {*************************************}
  TRectangleStopWatch = Class(TRectangle)
  public
     PaintMs: double;
     procedure Paint; override;
  End;

  {***********************************}
  TALCircleStopWatch = Class(TALCircle)
  protected
     PaintMs: double;
     MakeBufDrawableMs: double;
     procedure Paint; override;
  public
     procedure MakeBufDrawable; override;
  End;

  {*******************************}
  TCircleStopWatch = Class(TCircle)
  public
     PaintMs: double;
     procedure Paint; override;
  End;

  {*******************************}
  TALTextStopWatch = Class(TalText)
  protected
     PaintMs: double;
     MakeBufDrawableMs: double;
     procedure Paint; override;
  public
     procedure MakeBufDrawable; override;
  End;

  {***************************}
  TTextStopWatch = Class(TText)
  public
     PaintMs: double;
     procedure Paint; override;
  End;

  {***************************}
  TLineStopWatch = Class(TLine)
  public
     PaintMs: double;
     procedure Paint; override;
  End;

  {*******************************}
  TALLineStopWatch = Class(TALLine)
  protected
     PaintMs: double;
     MakeBufDrawableMs: double;
  public
     procedure MakeBufDrawable; override;
     procedure Paint; override;
  End;

  {**********************}
  TMainForm = class(TForm)
    ALVertScrollBox1: TALVertScrollBox;
    ButtonLaunchScrollBoxDemoDelphi: TALButton;
    ButtonLaunchScrollBoxDemoAlcinoe: TALButton;
    ButtonBenchTALText: TALButton;
    ButtonBenchTRectangle: TALButton;
    Text1: TALText;
    ButtonBenchTALRectangle: TALButton;
    ButtonBenchTText: TALButton;
    Text4: TALText;
    ButtonBenchTLine: TALButton;
    Text10: TALText;
    ButtonBenchTALLine: TALButton;
    Text7: TALText;
    Text9: TALText;
    ButtonBenchTCircle: TALButton;
    ButtonBenchTALCircle: TALButton;
    ALRectangle9: TALRectangle;
    ALTabControl1: TALTabControl;
    ALTabItem1: TALTabItem;
    ALText3: TALText;
    ALText4: TALText;
    alImage1: TALImage;
    ALTabItem2: TALTabItem;
    ALText1: TALText;
    ALText2: TALText;
    Image2: TALImage;
    ALTabItem3: TALTabItem;
    ALText5: TALText;
    ALText6: TALText;
    Image3: TALImage;
    ALTabItem4: TALTabItem;
    ALText7: TALText;
    ALText8: TALText;
    Image4: TALImage;
    ALRangeTrackBar1: TALRangeTrackBar;
    ALTrackBar1: TALTrackBar;
    Text11: TALText;
    Text8: TALText;
    ALVideoPlayerSurface1: TALVideoPlayerSurface;
    MonochromeEffect1: TMonochromeEffect;
    BandedSwirlEffect1: TBandedSwirlEffect;
    MagnifyEffect1: TMagnifyEffect;
    RippleEffect1: TRippleEffect;
    SmoothMagnifyEffect1: TSmoothMagnifyEffect;
    WaveEffect1: TWaveEffect;
    PaperSketchEffect1: TPaperSketchEffect;
    PencilStrokeEffect1: TPencilStrokeEffect;
    PixelateEffect1: TPixelateEffect;
    ContrastEffect1: TContrastEffect;
    TilerEffect1: TTilerEffect;
    MaskToAlphaEffect1: TMaskToAlphaEffect;
    EmbossEffect1: TEmbossEffect;
    PinchEffect1: TPinchEffect;
    ALText10: TALText;
    ALRectangle1: TALRectangle;
    ButtonShowTALDatePickerDialog: TALButton;
    ButtonApplyEffectToVideo: TALButton;
    ALLayout7: TALLayout;
    ALLayout8: TALLayout;
    ALButton6: TALButton;
    ALLayout9: TALLayout;
    ALButton7: TALButton;
    ALLayout14: TALLayout;
    ALLayout15: TALLayout;
    ALButton9: TALButton;
    ALLayout16: TALLayout;
    ALButton10: TALButton;
    ALLayout17: TALLayout;
    ALLayout18: TALLayout;
    ALButton11: TALButton;
    ALLayout19: TALLayout;
    ALButton12: TALButton;
    ALLayout20: TALLayout;
    ALLayout21: TALLayout;
    ALButton13: TALButton;
    ALLayout22: TALLayout;
    ALButton14: TALButton;
    ALLayout26: TALLayout;
    ALLayout27: TALLayout;
    ALButton17: TALButton;
    ALLayout28: TALLayout;
    ALButton18: TALButton;
    ALRectangle2: TALRectangle;
    ALLayout2: TALLayout;
    ALLayout3: TALLayout;
    ALButton1: TALButton;
    ALLayout4: TALLayout;
    ALButton2: TALButton;
    ALLayout5: TALLayout;
    ALLayout6: TALLayout;
    ALButton3: TALButton;
    ALLayout10: TALLayout;
    ALButton4: TALButton;
    ALLayout11: TALLayout;
    ALLayout12: TALLayout;
    ALButton5: TALButton;
    ALLayout13: TALLayout;
    ALButton8: TALButton;
    ALLayout23: TALLayout;
    ALLayout24: TALLayout;
    ALButton15: TALButton;
    ALLayout25: TALLayout;
    ALButton16: TALButton;
    ALLayout29: TALLayout;
    ALLayout30: TALLayout;
    ALButton19: TALButton;
    ALLayout31: TALLayout;
    ALButton20: TALButton;
    ALText29: TALText;
    ALLayout41: TALLayout;
    ALLayout42: TALLayout;
    ALButton23: TALButton;
    ALLayout43: TALLayout;
    ALButton24: TALButton;
    ALText17: TALText;
    MainTitle: TALText;
    SubTitle: TALText;
    ALText18: TALText;
    ALText21: TALText;
    ALText23: TALText;
    ALText19: TALText;
    ALTextDemo: TALText;
    ALText9: TALText;
    ALText24: TALText;
    ALText11: TALText;
    ALText15: TALText;
    ALLayout32: TALLayout;
    ALLayout33: TALLayout;
    ALLayout34: TALLayout;
    ALText25: TALText;
    ALLayout35: TALLayout;
    ALAniIndicator1: TALAniIndicator;
    ALText26: TALText;
    ALText28: TALText;
    ALText16: TALText;
    ALText12: TALText;
    ALText13: TALText;
    ALText14: TALText;
    ALText30: TALText;
    ALText31: TALText;
    ALText32: TALText;
    ALLayout1: TALLayout;
    ALLayout36: TALLayout;
    ALLayout37: TALLayout;
    ALText33: TALText;
    ALText34: TALText;
    ALText35: TALText;
    ALText36: TALText;
    ALText37: TALText;
    ALEdit1: TALEdit;
    ALLayout38: TALLayout;
    ALLayout39: TALLayout;
    ALButton21: TALButton;
    ALLayout40: TALLayout;
    ALButton22: TALButton;
    ALMemo1: TALMemo;
    ALLayout46: TALLayout;
    ALLayout47: TALLayout;
    ALLayout48: TALLayout;
    ALEdit6: TALEdit;
    ALEdit7: TALEdit;
    ALImage2: TALImage;
    ALImage3: TALImage;
    ALEdit2: TALEdit;
    ALImage4: TALImage;
    ALLayout45: TALLayout;
    ALLayout49: TALLayout;
    ALEdit3: TALEdit;
    ALImage5: TALImage;
    ALLayout50: TALLayout;
    ALEdit4: TALEdit;
    ALMemo2: TALMemo;
    ALRectangle3: TALRectangle;
    ALText20: TALText;
    ALEdit5: TALEdit;
    ALImage6: TALImage;
    ALLayout51: TALLayout;
    ALLayout52: TALLayout;
    ALEdit8: TALEdit;
    ALImage7: TALImage;
    ALLayout53: TALLayout;
    ALEdit9: TALEdit;
    ALMemo3: TALMemo;
    ALEdit10: TALEdit;
    ALImage8: TALImage;
    ALLayout54: TALLayout;
    ALLayout55: TALLayout;
    ALEdit11: TALEdit;
    ALImage9: TALImage;
    ALLayout56: TALLayout;
    ALEdit12: TALEdit;
    ALMemo4: TALMemo;
    ALText38: TALText;
    ALText39: TALText;
    ALEdit14: TALEdit;
    ALImage10: TALImage;
    ALEdit15: TALEdit;
    ALImage11: TALImage;
    ALText42: TALText;
    ALText43: TALText;
    ALLayout57: TALLayout;
    ALLayout58: TALLayout;
    ALText45: TALText;
    ALLayout59: TALLayout;
    ALImage12: TALImage;
    ALRectangle4: TALRectangle;
    ALText51: TALText;
    ALText52: TALText;
    ALRectangle5: TALRectangle;
    ALRectangle6: TALRectangle;
    ALText54: TALText;
    ALText41: TALText;
    ALLayout67: TALLayout;
    ALAnimatedImage1: TALAnimatedImage;
    ALAnimatedImage3: TALAnimatedImage;
    ALText56: TALText;
    ALText57: TALText;
    ALLayout68: TALLayout;
    ALAnimatedImage2: TALAnimatedImage;
    ALText59: TALText;
    ALLayout69: TALLayout;
    ALText58: TALText;
    ALImage13: TALImage;
    ALText60: TALText;
    ALText61: TALText;
    ALText62: TALText;
    ALLayout72: TALLayout;
    ALLayout73: TALLayout;
    ALText63: TALText;
    ALLayout74: TALLayout;
    Image1: TImage;
    ALLayout75: TALLayout;
    ALLayout76: TALLayout;
    ALLayout77: TALLayout;
    ALLayout70: TALLayout;
    ALLayout71: TALLayout;
    ALLayout78: TALLayout;
    Text2: TALText;
    ALText64: TALText;
    ALImage14: TALImage;
    ALImage15: TALImage;
    ALText65: TALText;
    ALText66: TALText;
    ALLayout79: TALLayout;
    ALSwitchAnimatedImage: TALAnimatedImage;
    ALLayout80: TALLayout;
    ALButton25: TALButton;
    ALButton26: TALButton;
    ALButton27: TALButton;
    ALButton28: TALButton;
    ALLayout81: TALLayout;
    ALButton29: TALButton;
    ALButton30: TALButton;
    ALButton31: TALButton;
    ALButton32: TALButton;
    ALText67: TALText;
    ALLayout82: TALLayout;
    ALLayout83: TALLayout;
    ALButton33: TALButton;
    ALLayout84: TALLayout;
    ALLayout85: TALLayout;
    ALCheckBox3: TALCheckBox;
    ALText40: TALText;
    ALLayout86: TALLayout;
    ALLayout87: TALLayout;
    ALCheckBox4: TALCheckBox;
    ALText50: TALText;
    ALRectangle7: TALRectangle;
    ALText68: TALText;
    ALLayout94: TALLayout;
    ALLayout95: TALLayout;
    ALCheckBox5: TALCheckBox;
    ALText69: TALText;
    ALLayout96: TALLayout;
    ALLayout97: TALLayout;
    ALCheckBox6: TALCheckBox;
    ALText70: TALText;
    ALLayout88: TALLayout;
    ALLayout89: TALLayout;
    ALText71: TALText;
    ALLayout44: TALLayout;
    ALLayout60: TALLayout;
    ALText47: TALText;
    ALLayout90: TALLayout;
    ALLayout91: TALLayout;
    ALText48: TALText;
    ALRadioButton1: TALRadioButton;
    ALRadioButton2: TALRadioButton;
    ALRectangle8: TALRectangle;
    ALText72: TALText;
    ALLayout92: TALLayout;
    ALLayout93: TALLayout;
    ALText73: TALText;
    ALLayout98: TALLayout;
    ALLayout99: TALLayout;
    ALText74: TALText;
    ALRadioButton3: TALRadioButton;
    ALRadioButton4: TALRadioButton;
    RadioButton1: TRadioButton;
    ALRectangle10: TALRectangle;
    ALText75: TALText;
    ALLayout100: TALLayout;
    ALLayout101: TALLayout;
    ALText76: TALText;
    ALSwitch2: TALSwitch;
    ALLayout104: TALLayout;
    ALLayout105: TALLayout;
    ALSwitch1: TALSwitch;
    ALText78: TALText;
    ALLayout102: TALLayout;
    ALLayout103: TALLayout;
    ALSwitch3: TALSwitch;
    ALText77: TALText;
    ALLayout106: TALLayout;
    ALLayout107: TALLayout;
    ALSwitch4: TALSwitch;
    ALText79: TALText;
    ALLayout108: TALLayout;
    ALLayout109: TALLayout;
    ALSwitch5: TALSwitch;
    ALText80: TALText;
    ALLayout110: TALLayout;
    ALLayout111: TALLayout;
    ALText81: TALText;
    Switch1: TSwitch;
    ALTrackBar2: TALTrackBar;
    TrackBar1: TTrackBar;
    ALTrackBar3: TALTrackBar;
    ALText82: TALText;
    ALText83: TALText;
    ALText84: TALText;
    ALText85: TALText;
    ALTrackBar4: TALTrackBar;
    ALText86: TALText;
    ALRectangle11: TALRectangle;
    ALText87: TALText;
    ALTrackBar5: TALTrackBar;
    ALText88: TALText;
    ALTrackBar6: TALTrackBar;
    ALText89: TALText;
    ALText90: TALText;
    ALRangeTrackBar2: TALRangeTrackBar;
    ALText91: TALText;
    ALRangeTrackBar3: TALRangeTrackBar;
    ALText92: TALText;
    ALEdit13: TALEdit;
    ALImage16: TALImage;
    procedure ButtonLaunchScrollBoxDemoAlcinoeClick(Sender: TObject);
    procedure ButtonBenchTALRectangleClick(Sender: TObject);
    procedure ButtonBenchTRectangleClick(Sender: TObject);
    procedure ButtonBenchTALTextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonBenchTTextClick(Sender: TObject);
    procedure ButtonLaunchScrollBoxDemoDelphiClick(Sender: TObject);
    procedure ButtonBenchTLineClick(Sender: TObject);
    procedure ButtonBenchTALLineClick(Sender: TObject);
    procedure ButtonBenchTALCircleClick(Sender: TObject);
    procedure ButtonBenchTCircleClick(Sender: TObject);
    procedure ALTabControl1ViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF);
    procedure ALTabControl1AniTransitionInit(
                const sender: TObject;
                const ATransition: TALTabTransition;
                const aVelocity: Single;
                const aAnimation: TALFloatPropertyAnimation);
    procedure ALTabControl1Resized(Sender: TObject);
    procedure FormVirtualKeyboardHidden(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardShown(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure ALVertScrollBox1Click(Sender: TObject);
    procedure ButtonApplyEffectToVideoClick(Sender: TObject);
    procedure ALVertScrollBox1ViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF);
    procedure FormResize(Sender: TObject);
    procedure ALVideoPlayerSurface1VideoSizeChanged(
                const Sender: TObject;
                const width, height: Integer);
    procedure ButtonShowTALDatePickerDialogClick(Sender: TObject);
    procedure ALTextDemoElementClick(Sender: TObject; const Element: TALTextElement);
    procedure ALTextDemoElementMouseEnter(Sender: TObject; const Element: TALTextElement);
    procedure ALTextDemoElementMouseLeave(Sender: TObject; const Element: TALTextElement);
    procedure ALTextDemoElementMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single; const Element: TALTextElement);
    procedure ALTextDemoPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure ALTextEllipsisElementClick(Sender: TObject; const Element: TALTextElement);
    procedure ALTextEllipsisElementMouseEnter(Sender: TObject; const Element: TALTextElement);
    procedure ALTextEllipsisElementMouseLeave(Sender: TObject; const Element: TALTextElement);
    procedure ALSwitchAnimatedImageClick(Sender: TObject);
  private
    FDatePickerDialog: TALDatePickerDialog;
    fLine: TLineStopWatch;
    fALLine: TALLineStopWatch;
    fText: TTextStopWatch;
    fALText: TALTextStopWatch;
    fALRectangle: TALRectangleStopWatch;
    fRectangle: TRectangleStopWatch;
    fALCircle: TALCircleStopWatch;
    fCircle: TCircleStopWatch;
    FVirtualKeyboardOpen: boolean;
    FCurrentTextElements: TDictionary<TObject, TALTextElement>;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  system.Diagnostics,
  system.threading,
  system.Math,
  system.DateUtils,
  fmx.DialogService,
  Alcinoe.FMX.ScrollEngine,
  Alcinoe.Common,
  ScrollBox;

{$R *.fmx}

{***********************************************}
constructor TMainForm.Create(AOwner: TComponent);
begin
  TALFontManager.RegisterTypefaceFromResource('GoodDogPlain', 'GoodDog Plain');
  TALFontManager.RegisterTypefaceFromResource('MaShanZhengRegular', 'Ma Shan Zheng');
  inherited;
end;

{**********************************************}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  TALErrorReporting.Instance;
  //ALVertScrollBox1.ScrollEngine.TouchTracking := [ttVertical];
  //special case for windows
  if not ALVertScrollBox1.HasTouchScreen then begin
    ALVertScrollBox1.VScrollBar.Width := 8;
    ALVertScrollBox1.VScrollBar.Margins.Right := 3;
    ALVertScrollBox1.VScrollBar.Thumb.XRadius := -50;
    ALVertScrollBox1.VScrollBar.Thumb.yRadius := -50;
  end;
  FCurrentTextElements := TDictionary<TObject, TALTextElement>.Create;
  fDatePickerDialog := nil;
  FVirtualKeyboardOpen := False;
  beginupdate;
  try
    var LTitle: String := MainForm.Canvas.ClassName;
    {$IF defined(Android) and defined(SKIA)}
    if GlobalUseVulkan then LTitle := LTitle + ' - Vulkan'
    else LTitle := LTitle + ' - OpenGL';
    {$ELSEIF defined(Android)}
    LTitle := LTitle + ' - OpenGL';
    {$ELSEIF defined(AlAppleOS)}
    if GlobalUseMetal then LTitle := LTitle + ' - Metal'
    else LTitle := LTitle + ' - OpenGL';
    {$ENDIF}
    MainTitle.Text := LTitle;

    {$IF defined(ALSkiaCanvas)}
    SubTitle.Text := 'sk_surface_t (ALSkiaCanvas)';
    {$ELSEIF defined(ALSkiaEngine)}
    SubTitle.Text := 'sk_surface_t (ALSkiaEngine)';
    {$ELSEIF defined(Android)}
    SubTitle.Text := 'Jbitmap';
    {$ELSEIF defined(AlAppleOS)}
    SubTitle.Text := 'CGContextRef';
    {$ELSE}
    SubTitle.Text := 'Tbitmap';
    {$ENDIF}
    //-----
    ALVideoPlayerSurface1.Height := (width / 1920) * 1080;
    //-----
    fALRectangle := TALRectangleStopWatch.Create(self);
    fALRectangle.Parent := ALVertScrollBox1;
    //fALRectangle.DoubleBuffered := False;
    fALRectangle.Fill.Color := $FFf1ecff;
    fALRectangle.Stroke.Color := $FFbea7fb;
    fALRectangle.Align := TalignLayout.Top;
    fALRectangle.Margins.Left := 16;
    fALRectangle.Margins.Top := 8;
    fALRectangle.Margins.Right := 16;
    fALRectangle.Position.Y := ButtonBenchTALRectangle.Position.Y - ButtonBenchTALRectangle.Margins.Top;
    fALRectangle.Size.Height := 50;
    fALRectangle.XRadius := 12;
    fALRectangle.YRadius := 12;
    fALRectangle.HitTest := False;
    var LText := TalText.Create(fALRectangle);
    LText.Parent := fALRectangle;
    LText.AutoSize := true;
    LText.Align := TAlignLayout.Center;
    LText.TextSettings.Font.Size := 16;
    LText.Text := 'TALRectangle';
    LText.HitTest := false;
    //-----
    fRectangle := TRectangleStopWatch.Create(self);
    fRectangle.Parent := ALVertScrollBox1;
    fRectangle.Align := TalignLayout.Top;
    fRectangle.Fill.Color := $FFf1ecff;
    fRectangle.Stroke.Color := $FFbea7fb;
    fRectangle.Margins.Left := 16;
    fRectangle.Margins.Top := 12;
    fRectangle.Margins.Right := 16;
    fRectangle.Margins.Bottom := 8;
    fRectangle.Position.Y := ButtonBenchTALRectangle.Position.Y - ButtonBenchTALRectangle.Margins.Top;
    fRectangle.Size.Height := 50;
    fRectangle.XRadius := 12;
    fRectangle.YRadius := 12;
    fRectangle.HitTest := false;
    LText := TalText.Create(fRectangle);
    LText.Parent := fRectangle;
    LText.AutoSize := true;
    LText.Align := TAlignLayout.Center;
    LText.TextSettings.Font.Size := 16;
    LText.Text := 'TRectangle';
    LText.HitTest := false;
    //-----
    fALCircle := TALCircleStopWatch.Create(ALLayout33);
    fALCircle.Parent := ALLayout33;
    //fALCircle.DoubleBuffered := False;
    fALCircle.Fill.Color := $FFf1ecff;
    fALCircle.Stroke.Color := $FFbea7fb;
    fALCircle.Margins.Left := 15;
    fALCircle.Margins.Right := 15;
    fALCircle.Size.Height := 100;
    fALCircle.Size.Width := 100;
    fALCircle.HitTest := False;
    LText := TalText.Create(fALCircle);
    LText.Parent := fALCircle;
    LText.AutoSize := true;
    LText.Align := TAlignLayout.Center;
    LText.TextSettings.Font.Size := 16;
    LText.Text := 'TALCircle';
    LText.HitTest := False;
    //-----
    fCircle := TCircleStopWatch.Create(ALLayout34);
    fCircle.Parent := ALLayout34;
    fCircle.Fill.Color := $FFf1ecff;
    fCircle.Stroke.Color := $FFbea7fb;
    fCircle.Margins.Left := 15;
    fCircle.Margins.Right := 15;
    fCircle.Size.Height := 100;
    fCircle.Size.Width := 100;
    fCircle.HitTest := False;
    LText := TalText.Create(fCircle);
    LText.Parent := fCircle;
    LText.AutoSize := true;
    LText.Align := TAlignLayout.Center;
    LText.TextSettings.Font.Size := 16;
    LText.Text := 'TCircle';
    LText.HitTest := False;
    //-----
    fALText := TALTextStopWatch.Create(self);
    fALText.Parent := ALVertScrollBox1;
    //fALText.DoubleBuffered := False;
    fALText.TextSettings.HorzAlign := TALTextHorzAlign.Center;
    fALText.TextSettings.Font.Size := 18;
    fALText.TextSettings.Font.Family := ALConvertFontFamily('sans-serif');
    fALText.Align := TalignLayout.Top;
    fALText.Margins.Top := 12;
    fALText.Margins.left := 16;
    fALText.Margins.Right := 16;
    fALText.Position.Y := ButtonBenchTALText.Position.Y - ButtonBenchTALText.Margins.Top;
    fALText.Size.Height := 80;
    fALText.Text := 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.';
    fALText.HitTest := False;
    //-----
    fText := TTextStopWatch.Create(self);
    fText.Parent := ALVertScrollBox1;
    fText.TextSettings.WordWrap := True;
    fText.TextSettings.HorzAlign := TTextAlign.Center;
    fText.TextSettings.Font.Size := 18;
    fText.TextSettings.Font.Family := ALConvertFontFamily('sans-serif');
    fText.Align := TalignLayout.Top;
    fText.Margins.Top := 8;
    fText.Margins.left := 16;
    fText.Margins.Right := 16;
    fText.Position.Y := ButtonBenchTALText.Position.Y - ButtonBenchTALText.Margins.Top;
    fText.Size.Height := 80;
    fText.Text := 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.';
    FText.HitTest := False;
    //-----
    fALline := TALLineStopWatch.Create(ALLayout36);
    fALline.Parent := ALLayout36;
    //fALline.DoubleBuffered := False;
    fALline.Margins.right := 15;
    fALline.Margins.left := 15;
    fALline.Size.Height := 100;
    fALline.Size.Width := 100;
    fALline.Stroke.Thickness := 3;
    fALline.LineType := TALLineType.TopLeftToBottomRight;
    fALline.HitTest := False;
    //-----
    fline := TLineStopWatch.Create(ALLayout37);
    fline.Parent := ALLayout37;
    fline.Margins.right := 15;
    fline.Margins.left := 15;
    fline.Size.Height := 100;
    fline.Size.Width := 100;
    fline.Stroke.Thickness := 3;
    fline.LineType := TLineType.Diagonal;
    fline.HitTest := False;
    //-----
    ALMakeBufDrawables(ALVertScrollBox1, False{AEnsureDoubleBuffered});
  finally
    endupdate;
  end;
  ALTabControl1Resized(nil);
end;

{***********************************************}
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ALFreeAndNil(FCurrentTextElements);
  ALLog('FormDestroy');
end;

{**************************************************************}
procedure TMainForm.ALSwitchAnimatedImageClick(Sender: TObject);
begin
  with TALAnimatedImage(Sender) do begin
    Animation.Stop;
    if Tag = integer(true) then begin
      Tag := integer(false);
      Animation.StartProgress := 0.5;
      Animation.StopProgress := 1;
      Animation.Start;
    end
    else begin
      Tag := integer(true);
      Animation.StartProgress := 0;
      Animation.StopProgress := 0.5;
      Animation.Start;
    end;
  end;
end;

{*********************************************************}
procedure TMainForm.ALVertScrollBox1Click(Sender: TObject);
begin
  SetFocused(nil);
end;

{*************************************************}
procedure TMainForm.ALTabControl1AniTransitionInit(
            const sender: TObject;
            const ATransition: TALTabTransition;
            const aVelocity: Single;
            const aAnimation: TALFloatPropertyAnimation);
begin
  // aVelocity = pixels per seconds given by the anicalculations
  // ALTabControl1.Width - abs(ALTabControl1.activeTab.Position.X) = the number of pixel we need to scroll
  // 6 = factor i choose to compensate the deceleration made by the quartic Interpolation
  if comparevalue(aVelocity, 0) <> 0 then aAnimation.Duration := abs((ALTabControl1.Width - abs(ALTabControl1.activeTab.Position.X)) / aVelocity) * 6
  else aAnimation.Duration := 0.8;
  if aAnimation.Duration > 0.8 then aAnimation.Duration := 0.8
  else if aAnimation.Duration < 0.1 then aAnimation.Duration := 0.1;
  aAnimation.AnimationType := TAnimationType.out;
  aAnimation.Interpolation := TALInterpolationType.circular;
end;

{********************************************************}
procedure TMainForm.ALTabControl1Resized(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _updateLabels(const aTab: TalTabItem);
  begin
    var LText1: TalText := nil;
    var LText2: TalText := nil;
    for var LControl1 in aTab.Controls do begin
      if (LControl1 is TalText) and (LControl1.Tag = 1) then LText1 := TalText(LControl1)
      else if (LControl1 is TalText) and (LControl1.Tag = 2) then LText2 := TalText(LControl1);
    end;
    if LText1 <> nil then
      LText1.Position.X := ((aTab.Width - LText1.Width) / 2) + (aTab.Position.X / 5);
    if LText2 <> nil then
      LText2.Position.X := ((aTab.Width - LText2.Width) / 2) + (aTab.Position.X);
  end;

begin
  if ALTabControl1.TabIndex > 0 then _updateLabels(ALTabControl1.tabs[ALTabControl1.TabIndex - 1]);
  _updateLabels(ALTabControl1.tabs[ALTabControl1.TabIndex]);
  if ALTabControl1.TabIndex < ALTabControl1.Tabcount - 1 then _updateLabels(ALTabControl1.tabs[ALTabControl1.TabIndex + 1]);
end;

{********************************************************************************************************************************}
procedure TMainForm.ALTabControl1ViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF);
begin
  ALTabControl1Resized(nil);
end;

{*********************************************************************************************}
procedure TMainForm.ALTextEllipsisElementClick(Sender: TObject; const Element: TALTextElement);
begin
  If Element.Id = 'ellipsis' then begin
    TALText(Sender).TextSettings.MaxLines := 65535;
    TALText(Sender).MaxHeight := 65535;
    Invalidate;
  end;
end;

{**************************************************************************************************}
procedure TMainForm.ALTextEllipsisElementMouseEnter(Sender: TObject; const Element: TALTextElement);
begin
  If Element.Id = 'ellipsis' then
    TALText(Sender).Cursor := crHandPoint;
end;

{**************************************************************************************************}
procedure TMainForm.ALTextEllipsisElementMouseLeave(Sender: TObject; const Element: TALTextElement);
begin
  If Element.Id = 'ellipsis' then
    TALText(Sender).Cursor := crDefault;
end;

{*****************************************************************************************}
procedure TMainForm.ALTextDemoElementClick(Sender: TObject; const Element: TALTextElement);
begin
  TDialogService.ShowMessage('You clicked on the span with ID: ' + Element.Id);
end;

{**********************************************************************************************}
procedure TMainForm.ALTextDemoElementMouseEnter(Sender: TObject; const Element: TALTextElement);
begin
  ALTextDemo.Cursor := crHandPoint;
  FCurrentTextElements.AddOrSetValue(Sender, Element);
  Invalidate;
end;

{**********************************************************************************************}
procedure TMainForm.ALTextDemoElementMouseLeave(Sender: TObject; const Element: TALTextElement);
begin
  ALTextDemo.Cursor := crDefault;
  FCurrentTextElements.Remove(Sender);
  Invalidate;
end;

{*******************************************************************************************************************************}
procedure TMainForm.ALTextDemoElementMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single; const Element: TALTextElement);
begin
  ALTextDemo.Cursor := crHandPoint;
  FCurrentTextElements.AddOrSetValue(Sender, Element);
  Invalidate;
end;

{*****************************************************************************************}
procedure TMainForm.ALTextDemoPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  Var LTextElement: TalTextElement;
  If FCurrentTextElements.TryGetValue(Sender, LTextElement) then begin
    var LtextElements := ALGetTextElementsByID(TALText(Sender).elements, LTextElement.id);
    For var I := low(LtextElements) to high (LtextElements) do begin
      Canvas.Fill.Color := TalphaColors.Red;
      Canvas.FillRect(LtextElements[i].Rect, 0.3);
    end;
  end;
end;

{**********************************************************************************************************************************}
procedure TMainForm.ALVertScrollBox1ViewportPositionChange(Sender: TObject;const OldViewportPosition, NewViewportPosition: TPointF);
begin

  if (ALVideoPlayerSurface1.Position.y + ALVideoPlayerSurface1.Height >= NewViewportPosition.Y) and
     (ALVideoPlayerSurface1.Position.y < NewViewportPosition.Y + height) then begin
    if ALVideoPlayerSurface1.VideoPlayer.state in [vpsIdle] then begin
      ALVideoPlayerSurface1.VideoPlayer.setLooping(true);

      //ALVideoPlayerSurface1.VideoPlayer.prepare('https://bitdash-a.akamaihd.net/content/sintel/hls/playlist.m3u8', True{AutoStartWhenPrepared});
      (*
      #EXTM3U
      #EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID="stereo",LANGUAGE="en",NAME="English",DEFAULT=YES,AUTOSELECT=YES,URI="audio/stereo/en/128kbit.m3u8"
      #EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID="stereo",LANGUAGE="dubbing",NAME="Dubbing",DEFAULT=NO,AUTOSELECT=YES,URI="audio/stereo/none/128kbit.m3u8"
      #EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID="surround",LANGUAGE="en",NAME="English",DEFAULT=YES,AUTOSELECT=YES,URI="audio/surround/en/320kbit.m3u8"
      #EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID="surround",LANGUAGE="dubbing",NAME="Dubbing",DEFAULT=NO,AUTOSELECT=YES,URI="audio/stereo/none/128kbit.m3u8"
      #EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID="subs",NAME="Deutsch",DEFAULT=NO,AUTOSELECT=YES,FORCED=NO,LANGUAGE="de",URI="subtitles_de.m3u8"
      #EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID="subs",NAME="English",DEFAULT=YES,AUTOSELECT=YES,FORCED=NO,LANGUAGE="en",URI="subtitles_en.m3u8"
      #EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID="subs",NAME="Espanol",DEFAULT=NO,AUTOSELECT=YES,FORCED=NO,LANGUAGE="es",URI="subtitles_es.m3u8"
      #EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID="subs",NAME="Français",DEFAULT=NO,AUTOSELECT=YES,FORCED=NO,LANGUAGE="fr",URI="subtitles_fr.m3u8"
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=258157,CODECS="avc1.4d400d,mp4a.40.2",AUDIO="stereo",RESOLUTION=422x180,SUBTITLES="subs"
      video/250kbit.m3u8
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=520929,CODECS="avc1.4d4015,mp4a.40.2",AUDIO="stereo",RESOLUTION=638x272,SUBTITLES="subs"
      video/500kbit.m3u8
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=831270,CODECS="avc1.4d4015,mp4a.40.2",AUDIO="stereo",RESOLUTION=638x272,SUBTITLES="subs"
      video/800kbit.m3u8
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=1144430,CODECS="avc1.4d401f,mp4a.40.2",AUDIO="surround",RESOLUTION=958x408,SUBTITLES="subs"
      video/1100kbit.m3u8
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=1558322,CODECS="avc1.4d401f,mp4a.40.2",AUDIO="surround",RESOLUTION=1277x554,SUBTITLES="subs"
      video/1500kbit.m3u8
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=4149264,CODECS="avc1.4d4028,mp4a.40.2",AUDIO="surround",RESOLUTION=1921x818,SUBTITLES="subs"
      video/4000kbit.m3u8
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=6214307,CODECS="avc1.4d4028,mp4a.40.2",AUDIO="surround",RESOLUTION=1921x818,SUBTITLES="subs"
      video/6000kbit.m3u8
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=10285391,CODECS="avc1.4d4033,mp4a.40.2",AUDIO="surround",RESOLUTION=4096x1744,SUBTITLES="subs"
      video/10000kbit.m3u8
      *)

      //ALVideoPlayerSurface1.VideoPlayer.prepare('http://cdn-fms.rbs.com.br/vod/hls_sample1_manifest.m3u8', True{AutoStartWhenPrepared});
      (*
      #EXTM3U
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=150000
      /hls-vod/sample1_150kbps.f4v.m3u8
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=500000
      /hls-vod/sample1_500kbps.f4v.m3u8
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=700000
      /hls-vod/sample1_700kbps.f4v.m3u8
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=1000000
      /hls-vod/sample1_1000kbps.f4v.m3u8
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=1500000
      /hls-vod/sample1_1500kbps.f4v.m3u8
      *)

      //ALVideoPlayerSurface1.VideoPlayer.prepare('http://content.jwplatform.com/manifests/vM7nH0Kl.m3u8', True{AutoStartWhenPrepared});
      (*
      #EXTM3U
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=670000,RESOLUTION=640x286,CODECS="mp4a.40.2,avc1.77.30",CLOSED-CAPTIONS=NONE
      http://videos-f.jwpsrv.com/content/conversions/zWLy8Jer/videos/21ETjILN-1753142.mp4.m3u8?token=0_5a709f9d_0xf5b3d43b44268c58a2712ac88abc3d45e032eb4a
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=3400000,RESOLUTION=1920x858,CODECS="mp4a.40.2,avc1.77.30",CLOSED-CAPTIONS=NONE
      http://videos-f.jwpsrv.com/content/conversions/zWLy8Jer/videos/21ETjILN-1703854.mp4.m3u8?token=0_5a709f9d_0xa8762db0a663f6b81a81b5033ce89bc4578e4a33
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=1710000,RESOLUTION=1280x572,CODECS="mp4a.40.2,avc1.77.30",CLOSED-CAPTIONS=NONE
      http://videos-f.jwpsrv.com/content/conversions/zWLy8Jer/videos/21ETjILN-364768.mp4.m3u8?token=0_5a709f9d_0xd4799dbb4504aac710409af0598840625a5415dd
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=380000,RESOLUTION=320x142,CODECS="mp4a.40.2,avc1.77.30",CLOSED-CAPTIONS=NONE
      http://videos-f.jwpsrv.com/content/conversions/zWLy8Jer/videos/21ETjILN-364765.mp4.m3u8?token=0_5a709f9d_0x32cc62bc74320eee50114bc88f70d5f307033c44
      #EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=120000,CODECS="mp4a.40.2"
      http://videos-f.jwpsrv.com/content/conversions/zWLy8Jer/videos/21ETjILN-588477.m4a.m3u8?token=0_5a709f9d_0x160466a864635c5481a5f70c20b15d4bfa2ada9b
      *)

      ALVideoPlayerSurface1.VideoPlayer.prepare('https://mirror.clarkson.edu/blender/demo/movies/BBB/bbb_sunflower_1080p_30fps_normal.mp4', True{AutoStartWhenPrepared});
      //ALVideoPlayerSurface1.VideoPlayer.prepare('https://mirror.clarkson.edu/blender/demo/movies/BBB/bbb_sunflower_1080p_60fps_normal.mp4', True{AutoStartWhenPrepared});
      //ALVideoPlayerSurface1.VideoPlayer.prepare('https://mirror.clarkson.edu/blender/demo/movies/BBB/bbb_sunflower_2160p_60fps_normal.mp4', True{AutoStartWhenPrepared});
      //ALVideoPlayerSurface1.VideoPlayer.prepare('http://techslides.com/demos/samples/sample.mp4', True{AutoStartWhenPrepared}); // << this have sound on ios

    end
    else if ALVideoPlayerSurface1.VideoPlayer.state in [vpsPrepared, vpsPaused] then ALVideoPlayerSurface1.VideoPlayer.Start
    else ALVideoPlayerSurface1.VideoPlayer.AutoStartWhenPrepared := true;
  end
  else begin
    if ALVideoPlayerSurface1.VideoPlayer.state in [vpsStarted] then ALVideoPlayerSurface1.VideoPlayer.Pause
    else ALVideoPlayerSurface1.VideoPlayer.AutoStartWhenPrepared := False;
  end;
end;

{*****************************************************************}
procedure TMainForm.ButtonApplyEffectToVideoClick(Sender: TObject);
begin
  BandedSwirlEffect1.Enabled := false;
  ContrastEffect1.Enabled := false;
  MagnifyEffect1.Enabled := false;
  MonochromeEffect1.Enabled := false;
  RippleEffect1.Enabled := false;
  SmoothMagnifyEffect1.Enabled := false;
  WaveEffect1.Enabled := false;
  PencilStrokeEffect1.Enabled := false;
  PaperSketchEffect1.Enabled := false;
  PixelateEffect1.Enabled := false;
  TilerEffect1.Enabled := false;
  MaskToAlphaEffect1.Enabled := false;
  EmbossEffect1.Enabled := false;
  PinchEffect1.Enabled := false;

  ALVideoPlayerSurface1.Tag := ALVideoPlayerSurface1.Tag + 1;
  if ALVideoPlayerSurface1.Tag > 15 then ALVideoPlayerSurface1.Tag := 0;

  case ALVideoPlayerSurface1.Tag of
    1: BandedSwirlEffect1.Enabled := true;
    2: ContrastEffect1.Enabled := true;
    3: MagnifyEffect1.Enabled := true;
    4: MonochromeEffect1.Enabled := true;
    5: RippleEffect1.Enabled := true;
    6: SmoothMagnifyEffect1.Enabled := true;
    7: WaveEffect1.Enabled := true;
    8: PencilStrokeEffect1.Enabled := true;
    9: PaperSketchEffect1.Enabled := true;
    10: PixelateEffect1.Enabled := true;
    11: TilerEffect1.Enabled := true;
    12: MaskToAlphaEffect1.Enabled := true;
    13: EmbossEffect1.Enabled := true;
    14: PinchEffect1.Enabled := true;
  end;
end;

{*************************************************************************************************************}
procedure TMainForm.ALVideoPlayerSurface1VideoSizeChanged(const Sender: TObject; const width, height: Integer);
begin
  ALVideoPlayerSurface1.Height := height * (ClientWidth/Width)
end;

{*************************************************************************}
procedure TMainForm.ButtonLaunchScrollBoxDemoAlcinoeClick(Sender: TObject);
begin
  var LScrollBoxForm := TScrollBoxForm.Create(nil);
  var LVertScrollBox := TalVertScrollBox.Create(LScrollBoxForm);
  LVertScrollBox.Parent := LScrollBoxForm;
  LVertScrollBox.BeginUpdate;
  LVertScrollBox.Align := TalignLayout.Client;
  LVertScrollBox.ScrollEngine.MinEdgeSpringbackEnabled := False;
  LVertScrollBox.ScrollEngine.MaxEdgeSpringbackEnabled := False;
  LScrollBoxForm.fScrollEngine := LVertScrollBox.ScrollEngine;
  LScrollBoxForm.fvertScrollBox := LVertScrollBox;
  for var I := 1 to 150 do begin
    var LRectangle := TALRectangle.Create(LVertScrollBox);
    LRectangle.Parent := LVertScrollBox;
    LRectangle.Align := TalignLayout.Top;
    Lrectangle.Fill.Color := $FFf7f2fa;
    LRectangle.Stroke.Color := $FFe4e1e7;
    LRectangle.Margins.Left := 15;
    LRectangle.Margins.Top := 5;
    LRectangle.Margins.Right := 15;
    LRectangle.Margins.Bottom := 5;
    LRectangle.Position.Y := 0;
    LRectangle.Size.Height := 50;
    LRectangle.XRadius := 12;
    LRectangle.YRadius := 12;
    //-----
    var LText := TALText.Create(self);
    LText.Parent := LRectangle;
    LText.Align := TalignLayout.left;
    LText.Text := 'Alcinoe';
    LText.Margins.Left := 15;
    LText.TextSettings.MaxLines := 1;
    LText.autosize := True;
    //-----
    var LChildRectangle := TALRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TalignLayout.right;
    LChildRectangle.Margins.Left := 0;
    LChildRectangle.Margins.Top := 10;
    LChildRectangle.Margins.Right := random(15);
    LChildRectangle.Margins.Bottom := 10;
    LChildRectangle.Position.Y := 0;
    LChildRectangle.Size.width := 25;
    LChildRectangle.XRadius := 5;
    LChildRectangle.YRadius := 5;
    LChildRectangle.Fill.Color := $FF6750a4;
    //-----
    LChildRectangle := TALRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TalignLayout.right;
    LChildRectangle.Margins.Left := 0;
    LChildRectangle.Margins.Top := 10;
    LChildRectangle.Margins.Right := random(15);
    LChildRectangle.Margins.Bottom := 10;
    LChildRectangle.Position.Y := 0;
    LChildRectangle.Size.width := 25;
    LChildRectangle.XRadius := 5;
    LChildRectangle.YRadius := 5;
    LChildRectangle.Fill.Color := $FF625b71;
    //-----
    LChildRectangle := TALRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TalignLayout.right;
    LChildRectangle.Margins.Left := 0;
    LChildRectangle.Margins.Top := 10;
    LChildRectangle.Margins.Right := random(15);
    LChildRectangle.Margins.Bottom := 10;
    LChildRectangle.Position.Y := 0;
    LChildRectangle.Size.width := 25;
    LChildRectangle.XRadius := 5;
    LChildRectangle.YRadius := 5;
    LChildRectangle.Fill.Color := $FF7d5260;
    //-----
    LChildRectangle := TALRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TalignLayout.right;
    LChildRectangle.Margins.Left := 0;
    LChildRectangle.Margins.Top := 10;
    LChildRectangle.Margins.Right := random(15);
    LChildRectangle.Margins.Bottom := 10;
    LChildRectangle.Position.Y := 0;
    LChildRectangle.Size.width := 25;
    LChildRectangle.XRadius := 5;
    LChildRectangle.YRadius := 5;
    LChildRectangle.Fill.Color := $FFb3261e;
    //-----
    LChildRectangle := TALRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TalignLayout.right;
    LChildRectangle.Margins.Left := 0;
    LChildRectangle.Margins.Top := 10;
    LChildRectangle.Margins.Right := random(15);
    LChildRectangle.Margins.Bottom := 10;
    LChildRectangle.Position.Y := 0;
    LChildRectangle.Size.width := 25;
    LChildRectangle.XRadius := 5;
    LChildRectangle.YRadius := 5;
    LChildRectangle.Fill.Color := $FFeaddff;
    //-----
    LChildRectangle := TALRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TalignLayout.right;
    LChildRectangle.Margins.Left := 0;
    LChildRectangle.Margins.Top := 10;
    LChildRectangle.Margins.Right := random(15);
    LChildRectangle.Margins.Bottom := 10;
    LChildRectangle.Position.Y := 0;
    LChildRectangle.Size.width := 25;
    LChildRectangle.XRadius := 5;
    LChildRectangle.YRadius := 5;
    LChildRectangle.Fill.Color := $FFe8def8;
    //-----
    LChildRectangle := TALRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TalignLayout.right;
    LChildRectangle.Margins.Left := 0;
    LChildRectangle.Margins.Top := 10;
    LChildRectangle.Margins.Right := random(15);
    LChildRectangle.Margins.Bottom := 10;
    LChildRectangle.Position.Y := 0;
    LChildRectangle.Size.width := 25;
    LChildRectangle.XRadius := 5;
    LChildRectangle.YRadius := 5;
    LChildRectangle.Fill.Color := $FFffd8e4;
  end;
  LVertScrollBox.endUpdate;
  LScrollBoxForm.Show;
  ALMakeBufDrawables(LVertScrollBox);
end;

{************************************************************************}
procedure TMainForm.ButtonLaunchScrollBoxDemoDelphiClick(Sender: TObject);
begin
  var LScrollBoxForm := TScrollBoxForm.Create(nil);
  var LVertScrollBox := TVertScrollBox.Create(LScrollBoxForm);
  LVertScrollBox.Parent := LScrollBoxForm;
  LVertScrollBox.BeginUpdate;
  LVertScrollBox.Align := TalignLayout.Client;
  LVertScrollBox.AniCalculations.BoundsAnimation := False;
  LScrollBoxForm.fAniCalculations := LVertScrollBox.AniCalculations;
  LScrollBoxForm.fvertScrollBox := LVertScrollBox;
  for var I := 1 to 150 do begin
    var LRectangle := TRectangle.Create(LVertScrollBox);
    LRectangle.Parent := LVertScrollBox;
    LRectangle.Align := TalignLayout.Top;
    Lrectangle.Fill.Color := $FFf7f2fa;
    LRectangle.Stroke.Color := $FFe4e1e7;
    LRectangle.Margins.Left := 15;
    LRectangle.Margins.Top := 5;
    LRectangle.Margins.Right := 15;
    LRectangle.Margins.Bottom := 5;
    LRectangle.Position.Y := 0;
    LRectangle.Size.Height := 50;
    LRectangle.XRadius := 12;
    LRectangle.YRadius := 12;
    //-----
    var LText := TText.Create(self);
    LText.Parent := LRectangle;
    LText.Align := TalignLayout.left;
    LText.Text := 'Alcinoe';
    LText.Margins.Left := 15;
    LText.WordWrap := False;
    LText.autosize := True;
    //-----
    var LChildRectangle := TRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TalignLayout.right;
    LChildRectangle.Margins.Left := 0;
    LChildRectangle.Margins.Top := 10;
    LChildRectangle.Margins.Right := random(15);
    LChildRectangle.Margins.Bottom := 10;
    LChildRectangle.Position.Y := 0;
    LChildRectangle.Size.width := 25;
    LChildRectangle.XRadius := 5;
    LChildRectangle.YRadius := 5;
    LChildRectangle.Fill.Color := $FF6750a4;
    //-----
    LChildRectangle := TRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TalignLayout.right;
    LChildRectangle.Margins.Left := 0;
    LChildRectangle.Margins.Top := 10;
    LChildRectangle.Margins.Right := random(15);
    LChildRectangle.Margins.Bottom := 10;
    LChildRectangle.Position.Y := 0;
    LChildRectangle.Size.width := 25;
    LChildRectangle.XRadius := 5;
    LChildRectangle.YRadius := 5;
    LChildRectangle.Fill.Color := $FF625b71;
    //-----
    LChildRectangle := TRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TalignLayout.right;
    LChildRectangle.Margins.Left := 0;
    LChildRectangle.Margins.Top := 10;
    LChildRectangle.Margins.Right := random(15);
    LChildRectangle.Margins.Bottom := 10;
    LChildRectangle.Position.Y := 0;
    LChildRectangle.Size.width := 25;
    LChildRectangle.XRadius := 5;
    LChildRectangle.YRadius := 5;
    LChildRectangle.Fill.Color := $FF7d5260;
    //-----
    LChildRectangle := TRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TalignLayout.right;
    LChildRectangle.Margins.Left := 0;
    LChildRectangle.Margins.Top := 10;
    LChildRectangle.Margins.Right := random(15);
    LChildRectangle.Margins.Bottom := 10;
    LChildRectangle.Position.Y := 0;
    LChildRectangle.Size.width := 25;
    LChildRectangle.XRadius := 5;
    LChildRectangle.YRadius := 5;
    LChildRectangle.Fill.Color := $FFb3261e;
    //-----
    LChildRectangle := TRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TalignLayout.right;
    LChildRectangle.Margins.Left := 0;
    LChildRectangle.Margins.Top := 10;
    LChildRectangle.Margins.Right := random(15);
    LChildRectangle.Margins.Bottom := 10;
    LChildRectangle.Position.Y := 0;
    LChildRectangle.Size.width := 25;
    LChildRectangle.XRadius := 5;
    LChildRectangle.YRadius := 5;
    LChildRectangle.Fill.Color := $FFeaddff;
    //-----
    LChildRectangle := TRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TalignLayout.right;
    LChildRectangle.Margins.Left := 0;
    LChildRectangle.Margins.Top := 10;
    LChildRectangle.Margins.Right := random(15);
    LChildRectangle.Margins.Bottom := 10;
    LChildRectangle.Position.Y := 0;
    LChildRectangle.Size.width := 25;
    LChildRectangle.XRadius := 5;
    LChildRectangle.YRadius := 5;
    LChildRectangle.Fill.Color := $FFe8def8;
    //-----
    LChildRectangle := TRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TalignLayout.right;
    LChildRectangle.Margins.Left := 0;
    LChildRectangle.Margins.Top := 10;
    LChildRectangle.Margins.Right := random(15);
    LChildRectangle.Margins.Bottom := 10;
    LChildRectangle.Position.Y := 0;
    LChildRectangle.Size.width := 25;
    LChildRectangle.XRadius := 5;
    LChildRectangle.YRadius := 5;
    LChildRectangle.Fill.Color := $FFffd8e4;
  end;
  LVertScrollBox.endUpdate;
  LScrollBoxForm.Show;
end;

{**********************************************************************}
procedure TMainForm.ButtonShowTALDatePickerDialogClick(Sender: TObject);
begin
  ALFreeAndNil(fDatePickerDialog);
  fDatePickerDialog := TALDatePickerDialog.create(
                         'OK', // const aBtnOKCaption: string;
                         'Cancel', // const aBtnCancelCaption: string;
                         '', // const aBtnClearCaption: string
                         'Title');// const aTitle: String = ''
  fDatePickerDialog.show(
    YearOf(now), // const aYear: integer;
    MonthOf(now), // const aMonth: integer;
    DayOfTheMonth(now)); // const aDayOfMonth: integer);

end;

{**********************************************}
procedure TMainForm.FormResize(Sender: TObject);
begin
  ALLog('FormResize', 'width: ' + FloatToStr(width) + ' | ' + FloatToStr(height));
  ALVideoPlayerSurface1.Height := (width / 1920) * 1080;

  {$IF Defined(ANDROID)}
  //handle the action bar under lollipop
  if FVirtualKeyboardOpen then
    AlVertScrollBox1.ScrollEngine.ViewportPosition := AlVertScrollBox1.ScrollEngine.MaxScrollLimit;
  {$ENDIF}
end;

{********************************************}
procedure TMainForm.FormVirtualKeyboardHidden(
            Sender: TObject;
            KeyboardVisible: Boolean; const Bounds: TRect);
begin
  ALLog('FormVirtualKeyboardHidden');
  FVirtualKeyboardOpen := False;
  AlVertScrollBox1.margins.Bottom := 0;
end;

{*******************************************}
procedure TMainForm.FormVirtualKeyboardShown(
            Sender: TObject;
            KeyboardVisible: Boolean; const Bounds: TRect);
begin
  ALLog('FormVirtualKeyboardShown');
  FVirtualKeyboardOpen := True;
  AlVertScrollBox1.margins.Bottom := Bounds.height;
  if (Self.Focused <> nil) and
     (AlVertScrollBox1.ScreenToLocal(Self.Focused.LocalToScreen(TPointF.Create(0,0))).y > AlVertScrollBox1.Height - Tcontrol(Self.Focused.GetObject).Height - 16) then
    AlVertScrollBox1.VScrollBar.Value := AlVertScrollBox1.Content.ScreenToLocal(Self.Focused.LocalToScreen(TPointF.Create(0,0))).y - AlVertScrollBox1.Height + Tcontrol(Self.Focused.GetObject).Height + Tcontrol(Self.Focused.GetObject).Margins.Bottom + 16;
end;

{*********************************************************}
procedure TMainForm.ButtonBenchTLineClick(Sender: TObject);
begin
  fline.Repaint;
  Text10.Text := 'Paint: ' + FormatFloat('0.#####',fline.PaintMs) + ' ms';
end;

{***********************************************************}
procedure TMainForm.ButtonBenchTALTextClick(Sender: TObject);
begin
  fALText.clearBufDrawable;
  fALText.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text4.Text := 'buffer creation: ' + FormatFloat('0.#####',fALText.MakeBufDrawableMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALText.PaintMs) + ' ms';
        end);
    end).Start;
end;

{***********************************************************}
procedure TMainForm.ButtonBenchTALLineClick(Sender: TObject);
begin
  fALLine.clearBufDrawable;
  fALLine.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text10.Text := 'buffer creation: ' + FormatFloat('0.#####',fALLine.MakeBufDrawableMs) + ' ms - ' +
                         'Paint: ' + FormatFloat('0.#####',fALLine.PaintMs) + ' ms';
        end);
    end).Start;
end;

{*********************************************************}
procedure TMainForm.ButtonBenchTTextClick(Sender: TObject);
begin
  fText.Repaint;
  var LStopWatch := TstopWatch.StartNew;
  fText.Layout.WordWrap := not fText.Layout.WordWrap;
  fText.Layout.WordWrap := not fText.Layout.WordWrap;
  LStopWatch.stop;
  Text4.Text := 'RenderLayout: '+ FormatFloat('0.#####',LStopWatch.Elapsed.TotalMilliseconds) + ' ms - Paint: ' + FormatFloat('0.#####',fText.PaintMs) + ' ms'+#13#10+
                #13#10+
                'Note: Actually, at least under Skia, the TText also uses a "buffer" (a sk_paragraph_t object) that '+
                'consumes more memory than the picture snapshot. The RenderLayout represents the time needed to create this object.'
end;

{**************************************************************}
procedure TMainForm.ButtonBenchTRectangleClick(Sender: TObject);
begin
  fRectangle.Repaint;
  Text1.Text := 'Paint: ' + FormatFloat('0.#####',fRectangle.PaintMs) + ' ms';
end;

{***********************************************************}
procedure TMainForm.ButtonBenchTCircleClick(Sender: TObject);
begin
  fCircle.Repaint;
  Text9.Text := 'Paint: ' + FormatFloat('0.#####',fCircle.PaintMs) + ' ms';
end;

{*************************************************************}
procedure TMainForm.ButtonBenchTALCircleClick(Sender: TObject);
begin
  fALCircle.clearBufDrawable;
  fALCircle.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text9.Text := 'buffer creation: ' + FormatFloat('0.#####',fALCircle.MakeBufDrawableMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALCircle.PaintMs) + ' ms';
        end);
    end).Start;
end;

{****************************************************************}
procedure TMainForm.ButtonBenchTALRectangleClick(Sender: TObject);
begin
  fALRectangle.clearBufDrawable;
  fALRectangle.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text1.Text := 'buffer creation: ' + FormatFloat('0.#####',fALRectangle.MakeBufDrawableMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALRectangle.PaintMs) + ' ms';
        end);
    end).Start;
end;

{*****************************************}
procedure TALTextStopWatch.MakeBufDrawable;
begin
  if ALIsDrawableNull(BufDrawable) then begin
    var LStopWatch := TstopWatch.StartNew;
    inherited MakeBufDrawable;
    LStopWatch.stop;
    MakeBufDrawableMs := LStopWatch.Elapsed.TotalMilliseconds;
  end
  else inherited MakeBufDrawable;
end;

{*******************************}
procedure TALTextStopWatch.Paint;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  var LRemoveMakeBufDrawableMs := ALIsDrawableNull(BufDrawable);
  var LStopWatch := TstopWatch.StartNew;
  inherited paint;
  LStopWatch.stop;
  PaintMs := LStopWatch.Elapsed.TotalMilliseconds;
  if LRemoveMakeBufDrawableMs then PaintMs := PaintMs - MakeBufDrawableMs;
end;

{*****************************}
procedure TTextStopWatch.Paint;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  var LStopWatch := TstopWatch.StartNew;
  inherited paint;
  LStopWatch.stop;
  PaintMs := LStopWatch.Elapsed.TotalMilliseconds;
end;

{**********************************}
procedure TRectangleStopWatch.Paint;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  var LStopWatch := TstopWatch.StartNew;
  inherited paint;
  LStopWatch.stop;
  PaintMs := LStopWatch.Elapsed.TotalMilliseconds;
end;

{**********************************************}
procedure TALRectangleStopWatch.MakeBufDrawable;
begin
  if ALIsDrawableNull(BufDrawable) then begin
    var LStopWatch := TstopWatch.StartNew;
    inherited MakeBufDrawable;
    LStopWatch.stop;
    MakeBufDrawableMs := LStopWatch.Elapsed.TotalMilliseconds;
  end
  else inherited MakeBufDrawable;
end;

{************************************}
procedure TALRectangleStopWatch.Paint;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  var LRemoveMakeBufDrawableMs := ALIsDrawableNull(BufDrawable);
  var LStopWatch := TstopWatch.StartNew;
  inherited paint;
  LStopWatch.stop;
  PaintMs := LStopWatch.Elapsed.TotalMilliseconds;
  if LRemoveMakeBufDrawableMs then PaintMs := PaintMs - MakeBufDrawableMs;
end;

{*****************************}
procedure TLineStopWatch.Paint;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  var LStopWatch := TstopWatch.StartNew;
  inherited paint;
  LStopWatch.stop;
  PaintMs := LStopWatch.Elapsed.TotalMilliseconds;
end;

{*****************************************}
procedure TALLineStopWatch.MakeBufDrawable;
begin
  if ALIsDrawableNull(BufDrawable) then begin
    var LStopWatch := TstopWatch.StartNew;
    inherited MakeBufDrawable;
    LStopWatch.stop;
    MakeBufDrawableMs := LStopWatch.Elapsed.TotalMilliseconds;
  end
  else inherited MakeBufDrawable;
end;

{*******************************}
procedure TALLineStopWatch.Paint;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  var LRemoveMakeBufDrawableMs := ALIsDrawableNull(BufDrawable);
  var LStopWatch := TstopWatch.StartNew;
  inherited paint;
  LStopWatch.stop;
  PaintMs := LStopWatch.Elapsed.TotalMilliseconds;
  if LRemoveMakeBufDrawableMs then PaintMs := PaintMs - MakeBufDrawableMs;
end;

{*******************************************}
procedure TALCircleStopWatch.MakeBufDrawable;
begin
  if ALIsDrawableNull(BufDrawable) then begin
    var LStopWatch := TstopWatch.StartNew;
    inherited MakeBufDrawable;
    LStopWatch.stop;
    MakeBufDrawableMs := LStopWatch.Elapsed.TotalMilliseconds;
  end
  else inherited MakeBufDrawable;
end;

{*********************************}
procedure TALCircleStopWatch.Paint;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  var LRemoveMakeBufDrawableMs := ALIsDrawableNull(BufDrawable);
  var LStopWatch := TstopWatch.StartNew;
  inherited paint;
  LStopWatch.stop;
  PaintMs := LStopWatch.Elapsed.TotalMilliseconds;
  if LRemoveMakeBufDrawableMs then PaintMs := PaintMs - MakeBufDrawableMs;
end;

{*******************************}
procedure TCircleStopWatch.Paint;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  var LStopWatch := TstopWatch.StartNew;
  inherited paint;
  LStopWatch.stop;
  PaintMs := LStopWatch.Elapsed.TotalMilliseconds;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
