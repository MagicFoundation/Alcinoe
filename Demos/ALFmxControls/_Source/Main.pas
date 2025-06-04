unit Main;

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
  FMX.Platform,
  Alcinoe.Common,
  Alcinoe.FMX.Materials.Canvas,
  Alcinoe.FMX.Styles,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.PageController,
  Alcinoe.FMX.Edit,
  Alcinoe.FMX.VideoPlayer,
  Alcinoe.FMX.DatePickerDialog,
  Alcinoe.FMX.ErrorReporting,
  Alcinoe.FMX.BreakText,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Memo,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.FilterEffects;

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
    ALRangeTrackBar1: TALRangeTrackBar;
    ALTrackBar1: TALTrackBar;
    Text11: TALText;
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
    ALButton6: TALButton;
    ALButton7: TALButton;
    ALLayout14: TALLayout;
    ALButton9: TALButton;
    ALButton10: TALButton;
    ALLayout17: TALLayout;
    ALButton11: TALButton;
    ALButton12: TALButton;
    ALLayout20: TALLayout;
    ALButton13: TALButton;
    ALButton14: TALButton;
    ALLayout26: TALLayout;
    ALButton17: TALButton;
    ALButton18: TALButton;
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
    ALButton21: TALButton;
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
    ALText38: TALText;
    ALText39: TALText;
    ALEdit14: TALEdit;
    ALImage10: TALImage;
    ALText42: TALText;
    ALText43: TALText;
    ALAutoSizeLayout1: TALLayout;
    ALAutoSizeLayoutText: TALText;
    ALAutoSizeLayoutImage: TALImage;
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
    ALAnimatedImage2: TALAnimatedImage;
    ALText59: TALText;
    ALText58: TALText;
    ALImage13: TALImage;
    ALText60: TALText;
    ALText61: TALText;
    ALText62: TALText;
    ALLayout72: TALLayout;
    ALText63: TALText;
    ALLayout74: TALLayout;
    Image1: TImage;
    ALLayout75: TALLayout;
    ALLayout70: TALLayout;
    Text2: TALText;
    ALText64: TALText;
    ALImage14: TALImage;
    ALImage15: TALImage;
    ALText65: TALText;
    ALText66: TALText;
    ALSwitchAnimatedImage: TALAnimatedImage;
    ALLayout80: TALLayout;
    ALButton25: TALButton;
    ALButton26: TALButton;
    ALButton27: TALButton;
    ALButton28: TALButton;
    ALText67: TALText;
    ALButton33: TALButton;
    ALLayout84: TALLayout;
    ALCheckBox3: TALCheckBox;
    ALText40: TALText;
    ALLayout86: TALLayout;
    ALCheckBox4: TALCheckBox;
    ALText50: TALText;
    ALLayout88: TALLayout;
    ALText71: TALText;
    ALLayout44: TALLayout;
    ALText47: TALText;
    ALRadioButton1: TALRadioButton;
    ALRadioButton2: TALRadioButton;
    ALLayout104: TALLayout;
    ALSwitch1: TALSwitch;
    ALText78: TALText;
    ALLayout102: TALLayout;
    ALSwitch3: TALSwitch;
    ALText77: TALText;
    ALLayout108: TALLayout;
    ALText80: TALText;
    ALTrackBar2: TALTrackBar;
    ALTrackBarCustomStyle: TALTrackBar;
    ALText82: TALText;
    ALText83: TALText;
    ALText84: TALText;
    ALTrackBar4: TALTrackBar;
    ALText86: TALText;
    ALText90: TALText;
    ALRangeTrackBar2: TALRangeTrackBar;
    ALText91: TALText;
    ALText92: TALText;
    ALText45: TALText;
    ALImage12: TALImage;
    ALText48: TALText;
    ALImage17: TALImage;
    ALText55: TALText;
    ALPageController3: TALPageController;
    ALPageView3_1: TALPageView;
    ALPageView3_2: TALPageView;
    ALPageView3_6: TALPageView;
    ALImageGallery3_1: TALImage;
    ALImageGallery3_2: TALImage;
    ALImageGallery3_6: TALImage;
    ALPageView3_5: TALPageView;
    ALPageView3_4: TALPageView;
    ALPageView3_3: TALPageView;
    ALImageGallery3_5: TALImage;
    ALImageGallery3_3: TALImage;
    ALImageGallery3_4: TALImage;
    ALPageController2: TALPageController;
    ALPageView2_2: TALPageView;
    ALImageGallery2_2: TALImage;
    ALPageView2_3: TALPageView;
    ALImageGallery2_3: TALImage;
    ALPageView2_4: TALPageView;
    ALImageGallery2_4: TALImage;
    ALPageView2_5: TALPageView;
    ALImageGallery2_5: TALImage;
    ALPageView2_6: TALPageView;
    ALImageGallery2_6: TALImage;
    ALPageView2_1: TALPageView;
    ALImageGallery2_1: TALImage;
    ALPageIndicator2: TALPageIndicator;
    ALPageIndicator3: TALPageIndicator;
    ALText1: TALText;
    ALText2: TALText;
    ButtonShowSimpleDialog: TALButton;
    ButtonShowMultipleChoiceDialog: TALButton;
    ButtonShowSingleChoiceDialog: TALButton;
    ButtonShowEditDialog: TALButton;
    ALText3: TALText;
    ALText4: TALText;
    ALLayout3: TALLayout;
    ALText5: TALText;
    ALRadioButtonLightTheme: TALRadioButton;
    ALLayout4: TALLayout;
    ALText6: TALText;
    ALRadioButtonDarkTheme: TALRadioButton;
    ALLayout6: TALLayout;
    ALText7: TALText;
    ALRadioButtonAutoTheme: TALRadioButton;
    ALLayout2: TALLayout;
    ALSwitch5: TALSwitch;
    ALRangeTrackBarCustomStyle: TALRangeTrackBar;
    ALEdit5: TALEdit;
    ALImage1: TALImage;
    ButtonShowMemoDialog: TALButton;
    ButtonShowActionDialog: TALButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonLaunchScrollBoxDemoAlcinoeClick(Sender: TObject);
    procedure ButtonBenchTALRectangleClick(Sender: TObject);
    procedure ButtonBenchTRectangleClick(Sender: TObject);
    procedure ButtonBenchTALTextClick(Sender: TObject);
    procedure ButtonBenchTTextClick(Sender: TObject);
    procedure ButtonLaunchScrollBoxDemoDelphiClick(Sender: TObject);
    procedure ButtonBenchTLineClick(Sender: TObject);
    procedure ButtonBenchTALLineClick(Sender: TObject);
    procedure ButtonBenchTALCircleClick(Sender: TObject);
    procedure ButtonBenchTCircleClick(Sender: TObject);
    procedure FormVirtualKeyboardHidden(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardShown(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure ALVertScrollBox1Click(Sender: TObject);
    procedure ButtonApplyEffectToVideoClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
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
    procedure ALVertScrollBox1Resized(Sender: TObject);
    procedure ALPageController2Resized(Sender: TObject);
    procedure ALPageController2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ALPageController2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ALPageController3Resized(Sender: TObject);
    procedure ALPageController3ViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TALPointD);
    procedure ButtonShowSimpleDialogClick(Sender: TObject);
    procedure ALRadioButtonThemeClick(Sender: TObject);
    procedure ButtonShowSingleChoiceDialogClick(Sender: TObject);
    procedure ButtonShowMultipleChoiceDialogClick(Sender: TObject);
    procedure ButtonShowEditDialogClick(Sender: TObject);
    procedure ButtonShowMemoDialogClick(Sender: TObject);
    procedure ButtonShowActionDialogClick(Sender: TObject);
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
    FCurrentTextElements: TDictionary<TObject, TALTextElement>;
    FPageViewAnimation: TALFloatAnimation;
    FCanvasColorAdjustTextureMaterialGallery2: TALCanvasColorAdjustTextureMaterial;
    procedure PageViewAnimationProcess(Sender: TObject);
    {$IF defined(android)}
    procedure UpdateSystemBarsAppearance;
    function ApplicationEventHandler(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
    {$ENDIF}
  public
    procedure InitializeNewForm; override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  system.Diagnostics,
  system.threading,
  system.Math,
  system.DateUtils,
  system.Math.Vectors,
  {$IF defined(ANDROID)}
  Androidapi.Helpers,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.App,
  {$ENDIF}
  fmx.DialogService,
  Alcinoe.FMX.Dialogs,
  Alcinoe.FMX.ScrollEngine,
  Alcinoe.Stringutils,
  ScrollBoxDemo;

{$R *.fmx}

type
  TALImageProtectedAccess = class(TALImage);

{************************************}
procedure TMainForm.InitializeNewForm;
begin
  ALLog('TMainForm.InitializeNewForm', 'begin');
  {$IF defined(DEBUG)}
  ALInitHasTouchScreen;
  ALHasTouchScreen := true;
  {$ENDIF}
  TALStyleManager.Instance.ApplyDialogManagerStyle('Material.DialogManager', TALDialogManager.Instance, 18{AFontSize});
  // We can not call RegisterTypefaceFromResource in FormCreate because FormCreate is
  // called in TCommonCustomForm.AfterConstruction once all child components are
  // already fully loaded.
  TALFontManager.RegisterTypefaceFromResource('GoodDogPlain', 'GoodDog Plain');
  TALFontManager.RegisterTypefaceFromResource('MaShanZhengRegular', 'Ma Shan Zheng');

  {$IF defined(android)}

  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-3207 has been resolved. If resolved, remove the code below.'}
  {$ENDIF}
  var LApplicationEventService: IFMXApplicationEventService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(LApplicationEventService)) then
    LApplicationEventService.SetApplicationEventHandler(ApplicationEventHandler);
  UpdateSystemBarsAppearance;

  {$ENDIF}

  inherited;
  BeginUpdate;
  ALLog('TMainForm.InitializeNewForm', 'end | Form.size: ' + FloatToStr(width) + 'x' + FloatToStr(height));
end;

{************************************************************}
// FormCreate is called in TCommonCustomForm.AfterConstruction
// once all child components are fully loaded.
procedure TMainForm.FormCreate(Sender: TObject);
begin
  ALLog('TMainForm.FormCreate', 'begin');

  TALErrorReporting.Instance;

  FCanvasColorAdjustTextureMaterialGallery2 := TALCanvasColorAdjustTextureMaterial.Create;

  if not ALGetHasTouchScreen then begin
    ALVertScrollBox1.VScrollBar.Width := 8;
    ALVertScrollBox1.VScrollBar.Margins.Right := 3;
    ALVertScrollBox1.VScrollBar.Thumb.XRadius := -50;
    ALVertScrollBox1.VScrollBar.Thumb.yRadius := -50;
  end;

  FCurrentTextElements := TDictionary<TObject, TALTextElement>.Create;
  fDatePickerDialog := nil;

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
  SubTitle.Text := 'sk_surface_t (Skia Canvas)';
  {$ELSEIF defined(ALSkiaEngine)}
  SubTitle.Text := 'sk_surface_t (Skia Engine)';
  {$ELSEIF defined(Android)}
  SubTitle.Text := 'Jbitmap (OS Engine)';
  {$ELSEIF defined(AlAppleOS)}
  SubTitle.Text := 'CGContextRef (OS Engine)';
  {$ELSE}
  SubTitle.Text := 'Tbitmap (Delphi Engine)';
  {$ENDIF}
  //-----
  ALVideoPlayerSurface1.Height := (ALVertScrollBox1.Content.width / 1920) * 1080;
  //-----
  fALRectangle := TALRectangleStopWatch.Create(self);
  fALRectangle.Parent := ALVertScrollBox1;
  //fALRectangle.DoubleBuffered := False;
  fALRectangle.Fill.ColorKey := 'Material.Color.SurfaceContainerHighest';
  fALRectangle.Stroke.ColorKey := 'Material.Color.OutlineVariant';
  fALRectangle.Align := TALalignLayout.Top;
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
  LText.Align := TALAlignLayout.Center;
  LText.TextSettings.Font.Size := 16;
  LText.TextSettings.Font.ColorKey := 'Material.Color.OnSurface';
  LText.Text := 'TALRectangle';
  LText.HitTest := false;
  //-----
  fRectangle := TRectangleStopWatch.Create(self);
  fRectangle.Parent := ALVertScrollBox1;
  fRectangle.Align := TalignLayout.Top;
  fRectangle.Fill.Color := TALStyleManager.Instance.GetColor('Material.Color.SurfaceContainerHighest');
  fRectangle.Stroke.Color := TALStyleManager.Instance.GetColor('Material.Color.OutlineVariant');
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
  LText.Align := TALAlignLayout.Center;
  LText.TextSettings.Font.Size := 16;
  LText.TextSettings.Font.ColorKey := 'Material.Color.OnSurface';
  LText.Text := 'TRectangle';
  LText.HitTest := false;
  //-----
  fALCircle := TALCircleStopWatch.Create(ALLayout33);
  fALCircle.Parent := ALLayout33;
  //fALCircle.DoubleBuffered := False;
  fALCircle.Fill.ColorKey := 'Material.Color.SurfaceContainerHighest';
  fALCircle.Stroke.ColorKey := 'Material.Color.OutlineVariant';
  fALCircle.Margins.Left := 15;
  fALCircle.Margins.Right := 15;
  fALCircle.Size.Height := 100;
  fALCircle.Size.Width := 100;
  fALCircle.HitTest := False;
  LText := TalText.Create(fALCircle);
  LText.Parent := fALCircle;
  LText.AutoSize := true;
  LText.Align := TALAlignLayout.Center;
  LText.TextSettings.Font.Size := 16;
  LText.TextSettings.Font.ColorKey := 'Material.Color.OnSurface';
  LText.Text := 'TALCircle';
  LText.HitTest := False;
  //-----
  fCircle := TCircleStopWatch.Create(ALLayout34);
  fCircle.Parent := ALLayout34;
  fCircle.Fill.Color := TALStyleManager.Instance.GetColor('Material.Color.SurfaceContainerHighest');
  fCircle.Stroke.Color := TALStyleManager.Instance.GetColor('Material.Color.OutlineVariant');
  fCircle.Margins.Left := 15;
  fCircle.Margins.Right := 15;
  fCircle.Size.Height := 100;
  fCircle.Size.Width := 100;
  fCircle.HitTest := False;
  LText := TalText.Create(fCircle);
  LText.Parent := fCircle;
  LText.AutoSize := true;
  LText.Align := TALAlignLayout.Center;
  LText.TextSettings.Font.Size := 16;
  LText.TextSettings.Font.ColorKey := 'Material.Color.OnSurface';
  LText.Text := 'TCircle';
  LText.HitTest := False;
  //-----
  fALText := TALTextStopWatch.Create(self);
  fALText.Parent := ALVertScrollBox1;
  //fALText.DoubleBuffered := False;
  fALText.TextSettings.HorzAlign := TALTextHorzAlign.Center;
  fALText.TextSettings.Font.Size := 18;
  fALText.TextSettings.Font.Family := 'sans-serif';
  fALText.TextSettings.Font.ColorKey := 'Material.Color.OnSurface';
  fALText.Align := TALalignLayout.Top;
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
  fText.TextSettings.Font.Family := TALStyleManager.Instance.GetFontFamily('sans-serif');
  fText.TextSettings.FontColor := TALStyleManager.Instance.GetColor('Material.Color.OnSurface');
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
  fALline.Stroke.ColorKey := 'Material.Color.Outline';
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
  fline.Stroke.Color := TALStyleManager.Instance.GetColor('Material.Color.Outline');
  fline.LineType := TLineType.Diagonal;
  fline.HitTest := False;

  FPageViewAnimation := TALFloatAnimation.Create;
  FPageViewAnimation.OnProcess := PageViewAnimationProcess;

  case TALStyleManager.Instance.DarkModeBehavior of
    TALStyleManager.TDarkModeBehavior.AlwaysLight: ALRadioButtonLightTheme.Checked := True;
    TALStyleManager.TDarkModeBehavior.AlwaysDark: ALRadioButtonDarkTheme.Checked := True;
    TALStyleManager.TDarkModeBehavior.FollowSystem: ALRadioButtonAutoTheme.Checked := True;
  end;
  Fill.Color := TALStyleManager.Instance.GetColor('Material.Color.Surface');
  SystemStatusBar.BackgroundColor := TALStyleManager.Instance.GetColor('Material.Color.Surface');

  if TALStyleManager.Instance.IsDarkMode then begin
    ALTrackBarCustomStyle.Thumb.Fill.Color := $FFe5e0e8;
    ALTrackBarCustomStyle.Thumb.Stroke.Color := TalphaColors.Null;
    ALRangeTrackBarCustomStyle.Thumb.Fill.Color := $FFe5e0e8;
    ALRangeTrackBarCustomStyle.Thumb.Stroke.Color := TalphaColors.Null;
  end;

  EndUpdate;

  ALPageController2Resized(nil);
  ALPageController3Resized(nil);
  ALVertScrollBox1Resized(nil);
  ALLog('TMainForm.FormCreate', 'end | Form.size: ' + FloatToStr(width) + 'x' + FloatToStr(height));
end;

{***********************************************}
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ALFreeAndNil(FCanvasColorAdjustTextureMaterialGallery2);
  ALFreeAndNil(FPageViewAnimation);
  ALFreeAndNil(FCurrentTextElements);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-3207 has been resolved. If resolved, remove the code below.'}
{$ENDIF}
{$IF defined(android)}
procedure TMainForm.UpdateSystemBarsAppearance;
begin
  if TOSVersion.Check(11{API level 30}) then begin
    var LWindow := TAndroidHelper.Activity.getWindow;
    LWindow.setNavigationBarColor(integer(TALStyleManager.Instance.GetColor('Material.Color.Surface')));
    LWindow.setStatusBarColor(integer(TALStyleManager.Instance.GetColor('Material.Color.Surface')));
    var LInsetsController: JWindowInsetsController := LWindow.getInsetsController;
    if LInsetsController <> nil then begin
      if TALStyleManager.Instance.IsDarkMode then begin
        LInsetsController.setSystemBarsAppearance(
          0,
          TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_STATUS_BARS or
          TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_NAVIGATION_BARS);
      end
      else begin
        LInsetsController.setSystemBarsAppearance(
          TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_STATUS_BARS or
          TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_NAVIGATION_BARS,
          TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_STATUS_BARS or
          TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_NAVIGATION_BARS);
      end;
    end;
  end
  else begin
    TThread.ForceQueue(nil,
      procedure
      begin
        var LWindow := TAndroidHelper.Activity.getWindow;
        LWindow.setNavigationBarColor(integer(TALStyleManager.Instance.GetColor('Material.Color.Surface')));
        LWindow.setStatusBarColor(integer(TALStyleManager.Instance.GetColor('Material.Color.Surface')));
        if TALStyleManager.Instance.IsDarkMode then begin
          LWindow.getDecorView.setSystemUiVisibility(0);
        end
        else begin
          LWindow.getDecorView.setSystemUiVisibility(
            TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR or
            TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_NAVIGATION_BAR);
        end;
      end, 500);
  end;
end;
{$ENDIF}

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-3207 has been resolved. If resolved, remove the code below.'}
{$ENDIF}
{$IF defined(android)}
function TMainForm.ApplicationEventHandler(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  if AAppEvent = TApplicationEvent.BecameActive then UpdateSystemBarsAppearance;
  Result := True;
end;
{$ENDIF}

{***********************************************************}
procedure TMainForm.ALRadioButtonThemeClick(Sender: TObject);
begin
  if ALRadioButtonDarkTheme.Checked then
    TALStyleManager.Instance.DarkModeBehavior := TALStyleManager.TDarkModeBehavior.AlwaysDark
  else if ALRadioButtonLightTheme.Checked then
    TALStyleManager.Instance.DarkModeBehavior := TALStyleManager.TDarkModeBehavior.AlwaysLight
  else
    TALStyleManager.Instance.DarkModeBehavior := TALStyleManager.TDarkModeBehavior.FollowSystem;

  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          {$IF defined(android)}
          UpdateSystemBarsAppearance;
          {$ENDIF}
          TALStyleManager.Instance.ApplyColorScheme(Self, 'Material.Color.Surface'{AFormFillColorKey}, 'Material.Color.Surface'{ASystemStatusBarBackgroundColorKey});
          fRectangle.Fill.Color := TALStyleManager.Instance.GetColor('Material.Color.SurfaceContainerHighest');
          fRectangle.Stroke.Color := TALStyleManager.Instance.GetColor('Material.Color.OutlineVariant');
          fCircle.Fill.Color := TALStyleManager.Instance.GetColor('Material.Color.SurfaceContainerHighest');
          fCircle.Stroke.Color := TALStyleManager.Instance.GetColor('Material.Color.OutlineVariant');
          fText.TextSettings.FontColor := TALStyleManager.Instance.GetColor('Material.Color.OnSurface');
          fline.Stroke.Color := TALStyleManager.Instance.GetColor('Material.Color.Outline');
          if TALStyleManager.Instance.IsDarkMode then begin
            ALTrackBarCustomStyle.Thumb.Fill.Color := $FFe5e0e8;
            ALTrackBarCustomStyle.Thumb.Stroke.Color := TalphaColors.Null;
            ALRangeTrackBarCustomStyle.Thumb.Fill.Color := $FFe5e0e8;
            ALRangeTrackBarCustomStyle.Thumb.Stroke.Color := TalphaColors.Null;
          end
          else begin
            ALTrackBarCustomStyle.Thumb.Fill.Color := $FFFFFFFF;
            ALTrackBarCustomStyle.Thumb.Stroke.ColorKey := 'Material.Color.OutlineVariant';
            ALRangeTrackBarCustomStyle.Thumb.Fill.Color := $FFFFFFFF;
            ALRangeTrackBarCustomStyle.Thumb.Stroke.ColorKey := 'Material.Color.OutlineVariant';
          end;
          ALMakeBufDrawables(ALVertScrollBox1, False{AEnsureDoubleBuffered});
        end)
    end).Start;
end;

{**********************************************}
procedure TMainForm.FormResize(Sender: TObject);
begin
  ALLog('TMainForm.FormResize', 'Form.size: ' + FloatToStr(width) + 'x' + FloatToStr(height));
  ALVideoPlayerSurface1.Height := (ALVertScrollBox1.Content.width / 1920) * 1080;
end;

{***********************************************************}
procedure TMainForm.ALVertScrollBox1Resized(Sender: TObject);
begin
  ALLog('TMainForm.ALVertScrollBox1Resized', 'ALVertScrollBox1.size: ' + FloatToStr(ALVertScrollBox1.width) + 'x' + FloatToStr(ALVertScrollBox1.height));
  if FUpdating <= 0 then
    ALMakeBufDrawables(ALVertScrollBox1, False{AEnsureDoubleBuffered});
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
  TALDialog.Builder
    .SetMessageText('You clicked on the span with ID: <b>' + Element.Id + '</b>')
    .addButton('OK', 1)
    .Show;
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
      Canvas.Fill.Color := TALStyleManager.Instance.GetColor('Material.Color.OnTertiaryContainer');
      Canvas.FillRect(LtextElements[i].Rect, 0.2);
    end;
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

{*************************************************************************}
procedure TMainForm.ButtonLaunchScrollBoxDemoAlcinoeClick(Sender: TObject);
begin
  var LScrollBoxDemoForm := TScrollBoxDemoForm.Create(nil);
  var LVertScrollBox := TalVertScrollBox.Create(LScrollBoxDemoForm);
  LVertScrollBox.Parent := LScrollBoxDemoForm;
  LVertScrollBox.BeginUpdate;
  LVertScrollBox.Align := TALalignLayout.Client;
  LVertScrollBox.ScrollEngine.MinEdgeSpringbackEnabled := False;
  LVertScrollBox.ScrollEngine.MaxEdgeSpringbackEnabled := False;
  LScrollBoxDemoForm.fScrollEngine := LVertScrollBox.ScrollEngine;
  LScrollBoxDemoForm.fvertScrollBox := LVertScrollBox;
  for var I := 1 to 150 do begin
    var LRectangle := TALRectangle.Create(LVertScrollBox);
    LRectangle.Parent := LVertScrollBox;
    LRectangle.Align := TALalignLayout.Top;
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
    LText.Align := TALalignLayout.left;
    LText.Text := 'Alcinoe';
    LText.Margins.Left := 15;
    LText.TextSettings.MaxLines := 1;
    LText.autosize := True;
    //-----
    var LChildRectangle := TALRectangle.Create(LRectangle);
    LChildRectangle.Parent := LRectangle;
    LChildRectangle.Align := TALalignLayout.right;
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
    LChildRectangle.Align := TALalignLayout.right;
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
    LChildRectangle.Align := TALalignLayout.right;
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
    LChildRectangle.Align := TALalignLayout.right;
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
    LChildRectangle.Align := TALalignLayout.right;
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
    LChildRectangle.Align := TALalignLayout.right;
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
    LChildRectangle.Align := TALalignLayout.right;
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
  LScrollBoxDemoForm.Show;
  ALMakeBufDrawables(LVertScrollBox);
end;

{************************************************************************}
procedure TMainForm.ButtonLaunchScrollBoxDemoDelphiClick(Sender: TObject);
begin
  var LScrollBoxDemoForm := TScrollBoxDemoForm.Create(nil);
  var LVertScrollBox := TVertScrollBox.Create(LScrollBoxDemoForm);
  LVertScrollBox.Parent := LScrollBoxDemoForm;
  LVertScrollBox.BeginUpdate;
  LVertScrollBox.Align := TalignLayout.Client;
  LVertScrollBox.AniCalculations.BoundsAnimation := False;
  LScrollBoxDemoForm.fAniCalculations := LVertScrollBox.AniCalculations;
  LScrollBoxDemoForm.fvertScrollBox := LVertScrollBox;
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
  LScrollBoxDemoForm.Show;
end;

{**********************************************************************}
procedure TMainForm.ButtonShowTALDatePickerDialogClick(Sender: TObject);
begin
  ALFreeAndNil(fDatePickerDialog);
  fDatePickerDialog := TALDatePickerDialog.create(
                         'OK', // const aBtnOKCaption: string;
                         'Cancel', // const aBtnCancelCaption: string;
                         '', // const aBtnClearCaption: string
                         'TALDatePickerDialog (Spinner)');// const aTitle: String = ''
  fDatePickerDialog.show(
    YearOf(now), // const aYear: integer;
    MonthOf(now), // const aMonth: integer;
    DayOfTheMonth(now)); // const aDayOfMonth: integer);

end;

{***************************************************************}
procedure TMainForm.ButtonShowSimpleDialogClick(Sender: TObject);
begin
  TALDialog.Builder
    .SetIconResourceName('checkbox_checked')
    .SetHeadlineText('Dialog')
    .SetMessageText('A dialog is a type of modal window that appears in front of app content to provide critical information, or ask for a decision.')
    .addButton('Accept', 1)
    .addButton('Cancel', 2)
    .SetOnActionCallback(
       procedure(Const ADialog: TALDialog; const AAction: Integer; var ACanClose: Boolean)
       begin
         // Handle the action here
       end)
    .Show;
end;

{*********************************************************************}
procedure TMainForm.ButtonShowSingleChoiceDialogClick(Sender: TObject);
begin
  TALDialog.Builder
    .SetHeadlineText('Select one country')
    .addRadioButton('Argentina', 1{ATag}, False{AChecked})
    .addRadioButton('Australia', 2{ATag}, False{AChecked})
    .addRadioButton('Brazil', 3{ATag}, False{AChecked})
    .addRadioButton('Canada', 4{ATag}, False{AChecked})
    .addRadioButton('China', 5{ATag}, False{AChecked})
    .addRadioButton('Egypt', 6{ATag}, False{AChecked})
    .addRadioButton('France', 7{ATag}, False{AChecked})
    .addRadioButton('Germany', 8{ATag}, False{AChecked})
    .addRadioButton('India', 9{ATag}, False{AChecked})
    .addRadioButton('Indonesia', 10{ATag}, False{AChecked})
    .addRadioButton('Italy', 10{ATag}, False{AChecked})
    .addRadioButton('Japan', 12{ATag}, False{AChecked})
    .addRadioButton('Mexico', 13{ATag}, False{AChecked})
    .addRadioButton('Morocco', 14{ATag}, False{AChecked})
    .addRadioButton('Nigeria', 15{ATag}, False{AChecked})
    .addRadioButton('Russia', 16{ATag}, False{AChecked})
    .addRadioButton('South Africa', 17{ATag}, False{AChecked})
    .addRadioButton('South Korea', 18{ATag}, False{AChecked})
    .addRadioButton('Spain', 19{ATag}, False{AChecked})
    .addRadioButton('United States', 20{ATag}, False{AChecked})
    .addButton('Done', 1)
    .addButton('Cancel', 2)
    .SetOnActionCallback(
       procedure(Const ADialog: TALDialog; const AAction: Integer; var ACanClose: Boolean)
       begin
         ACanClose := True;
         if AAction = 1 then begin
           var LCheckedRadioButton := ADialog.GetCheckedRadioButton;
           if LCheckedRadioButton = nil then begin
             ACanClose := False;
             var LRadioButtons := ADialog.GetRadioButtons;
             For var I := low(LRadioButtons) to high(LRadioButtons) do
               TALStyleManager.Instance.ApplyRadioButtonStyle('Material.RadioButton.Error', LRadioButtons[i]);
           end;
         end;
       end)
    .Show;
end;

{***********************************************************************}
procedure TMainForm.ButtonShowMultipleChoiceDialogClick(Sender: TObject);
begin
  TALDialog.Builder
    .SetHeadlineText('Select countries')
    .SetMessageText('Because the fate of your coffee depends on it.')
    .addCheckBox('Argentina', 1{ATag}, False{AChecked})
    .addCheckBox('Australia', 2{ATag}, False{AChecked})
    .addCheckBox('Brazil', 3{ATag}, False{AChecked})
    .addCheckBox('Canada', 4{ATag}, False{AChecked})
    .addButton('Done', 1)
    .addButton('Cancel', 2)
    .SetOnActionCallback(
       procedure(Const ADialog: TALDialog; const AAction: Integer; var ACanClose: Boolean)
       begin
         // Handle the action here
       end)
    .Show;
end;

{***************************************************************}
procedure TMainForm.ButtonShowActionDialogClick(Sender: TObject);
begin
  TALDialog.Builder
    .SetHeadlineText('You are missing rewards!')
    .SetMessageText('Every time you search is an opportunity to earn points.')
    .addButton('<img src="login" width="25" height="25" color="inherit">&nbsp; Log In', 1{ATag}, False{AIsFooterButton})
    .addButton('<img src="signin" width="24" height="25" color="inherit">&nbsp; Create an Account', 2{ATag}, False{AIsFooterButton})
    .addButton('Skip', 3{ATag})
    .SetOnActionCallback(
       procedure(Const ADialog: TALDialog; const AAction: Integer; var ACanClose: Boolean)
       begin
         // Handle the action here
       end)
    .Show;
end;

{*************************************************************}
procedure TMainForm.ButtonShowEditDialogClick(Sender: TObject);
begin
  TALDialog.Builder
    .SetHeadlineText('Login')
    .SetHeadlineAlign(TALAlignLayout.TopCenter)
    .AddEdit('User name'{APromptText}, 'User name'{ALabelText}, ''{ASupportingText}, 1{ATag})
    .AddEdit('Password'{APromptText}, 'Password'{ALabelText}, ''{ASupportingText}, 2{ATag})
    .addButton('Login', 1)
    .addButton('Cancel', 2)
    .SetOnActionCallback(
       procedure(Const ADialog: TALDialog; const AAction: Integer; var ACanClose: Boolean)
       begin
         ACanClose := True;
         SetFocused(nil);
         if AAction = 1{Login} then begin
           Var LLoginEdit := ADialog.GetEdit(1{ATag});
           if LLoginEdit.Text = '' then begin
             ACanClose := False;
             TALStyleManager.Instance.ApplyEditStyle('Material.Edit.Hybrid.Error', LLoginEdit);
           end;
           Var LPasswordEdit := ADialog.GetEdit(2{ATag});
           if LPasswordEdit.Text = '' then begin
             ACanClose := False;
             TALStyleManager.Instance.ApplyEditStyle('Material.Edit.Hybrid.Error', LPasswordEdit);
           end;
         end;
       end)
    .Show;
end;

{*************************************************************}
procedure TMainForm.ButtonShowMemoDialogClick(Sender: TObject);
begin
  TALDialog.Builder
    .SetHeadlineText('Add your note')
    .AddMemo('Enter your note'{APromptText}, ''{ALabelText}, ''{ASupportingText}, 1{ATag})
    .addButton('OK', 1)
    .addButton('Cancel', 2)
    .SetOnActionCallback(
       procedure(Const ADialog: TALDialog; const AAction: Integer; var ACanClose: Boolean)
       begin
         // Handle the action here
       end)
    .Show;
end;

{********************************************}
procedure TMainForm.FormVirtualKeyboardHidden(
            Sender: TObject;
            KeyboardVisible: Boolean; const Bounds: TRect);
begin
  ALLog('FormVirtualKeyboardHidden');
  AlVertScrollBox1.margins.Bottom := 0;
end;

{*******************************************}
procedure TMainForm.FormVirtualKeyboardShown(
            Sender: TObject;
            KeyboardVisible: Boolean; const Bounds: TRect);
begin
  ALLog('FormVirtualKeyboardShown');
  if TALDialogManager.Instance.IsShowingDialog then exit;
  AlVertScrollBox1.margins.Bottom := Bounds.height;
  if (Focused <> nil) and
     (AlVertScrollBox1.ScreenToLocal(Focused.LocalToScreen(TPointF.Create(0,0))).y > AlVertScrollBox1.Height - Tcontrol(Focused.GetObject).Height - 16) then
    AlVertScrollBox1.VScrollBar.Value := AlVertScrollBox1.Content.ScreenToLocal(Focused.LocalToScreen(TPointF.Create(0,0))).y - AlVertScrollBox1.Height + Tcontrol(Focused.GetObject).Height + Tcontrol(Focused.GetObject).Margins.Bottom + 16;
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

{************************************************************}
procedure TMainForm.PageViewAnimationProcess(Sender: TObject);
begin
  ALImageGallery2_1.Scale.Point := TPointF.Create(FPageViewAnimation.CurrentValue, FPageViewAnimation.CurrentValue);
  ALImageGallery2_2.Scale.Point := TPointF.Create(FPageViewAnimation.CurrentValue, FPageViewAnimation.CurrentValue);
  ALImageGallery2_3.Scale.Point := TPointF.Create(FPageViewAnimation.CurrentValue, FPageViewAnimation.CurrentValue);
  ALImageGallery2_4.Scale.Point := TPointF.Create(FPageViewAnimation.CurrentValue, FPageViewAnimation.CurrentValue);
  ALImageGallery2_5.Scale.Point := TPointF.Create(FPageViewAnimation.CurrentValue, FPageViewAnimation.CurrentValue);
  ALImageGallery2_6.Scale.Point := TPointF.Create(FPageViewAnimation.CurrentValue, FPageViewAnimation.CurrentValue);
  FCanvasColorAdjustTextureMaterialGallery2.ShaderVariables.saturation := 4 * (FPageViewAnimation.CurrentValue - 1);
end;

{*********************************************************************************************************************}
procedure TMainForm.ALPageController2MouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FPageViewAnimation.StopAtCurrent;
  FPageViewAnimation.StartValue := ALImageGallery2_1.Scale.X;
  FPageViewAnimation.StopValue := 0.75;
  FPageViewAnimation.Duration := 0.2;
  FPageViewAnimation.Start;
  {$IF defined(ALGpuCanvas)}
  if TALImageProtectedAccess(ALImageGallery2_1).fBufDrawable <> nil then TALImageProtectedAccess(ALImageGallery2_1).fBufDrawable.Material := FCanvasColorAdjustTextureMaterialGallery2;
  if TALImageProtectedAccess(ALImageGallery2_2).fBufDrawable <> nil then TALImageProtectedAccess(ALImageGallery2_2).fBufDrawable.Material := FCanvasColorAdjustTextureMaterialGallery2;
  if TALImageProtectedAccess(ALImageGallery2_3).fBufDrawable <> nil then TALImageProtectedAccess(ALImageGallery2_3).fBufDrawable.Material := FCanvasColorAdjustTextureMaterialGallery2;
  if TALImageProtectedAccess(ALImageGallery2_4).fBufDrawable <> nil then TALImageProtectedAccess(ALImageGallery2_4).fBufDrawable.Material := FCanvasColorAdjustTextureMaterialGallery2;
  if TALImageProtectedAccess(ALImageGallery2_5).fBufDrawable <> nil then TALImageProtectedAccess(ALImageGallery2_5).fBufDrawable.Material := FCanvasColorAdjustTextureMaterialGallery2;
  if TALImageProtectedAccess(ALImageGallery2_6).fBufDrawable <> nil then TALImageProtectedAccess(ALImageGallery2_6).fBufDrawable.Material := FCanvasColorAdjustTextureMaterialGallery2;
  {$ENDIF}
end;

{********************************************************************************************************************}
procedure TMainForm.ALPageController2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FPageViewAnimation.StopAtCurrent;
  FPageViewAnimation.StartValue := ALImageGallery2_1.Scale.X;
  FPageViewAnimation.StopValue := 1;
  FPageViewAnimation.Duration := 0.4;
  FPageViewAnimation.Start;
end;

{************************************************************}
procedure TMainForm.ALPageController2Resized(Sender: TObject);
begin
  ALPageController2.Height := (ALPageController2.Width - 50) * (1350 / 1080);
end;

{************************************************************}
procedure TMainForm.ALPageController3Resized(Sender: TObject);
begin
  ALPageController3.Height := (ALPageController3.Width * 0.6) * (1349 / 1080);
  ALPageController3ViewportPositionChange(sender, ALPageController3.ScrollEngine.ViewportPosition, ALPageController3.ScrollEngine.ViewportPosition);
end;

{**************************************************************************************************************************************}
procedure TMainForm.ALPageController3ViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TALPointD);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure UpdateImageScale(const AImage: TALImage);
  begin
    var LRootCenterLine := ALPageController3.Width / 2;
    var LImageCenterLine := AImage.LocalToAbsolute(AImage.LocalRect).Left + (AImage.Width / 2);
    var LScale := 1 + (abs(LImageCenterLine - LRootCenterLine) - 1) * ((0.8 - 1) / ((AImage.Width) - 1));
    AImage.Scale.Point := TPointF.Create(LScale,LScale);
  end;

begin
  UpdateImageScale(ALImageGallery3_1);
  UpdateImageScale(ALImageGallery3_2);
  UpdateImageScale(ALImageGallery3_3);
  UpdateImageScale(ALImageGallery3_4);
  UpdateImageScale(ALImageGallery3_5);
  UpdateImageScale(ALImageGallery3_6);
end;

{*****************************************}
procedure TALTextStopWatch.MakeBufDrawable;
begin
  if ALIsDrawableNull(FBufDrawable) then begin
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
  var LRemoveMakeBufDrawableMs := ALIsDrawableNull(FBufDrawable);
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
  if ALIsDrawableNull(FBufDrawable) then begin
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
  var LRemoveMakeBufDrawableMs := ALIsDrawableNull(FBufDrawable);
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
  if ALIsDrawableNull(FBufDrawable) then begin
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
  var LRemoveMakeBufDrawableMs := ALIsDrawableNull(FBufDrawable);
  var LStopWatch := TstopWatch.StartNew;
  inherited paint;
  LStopWatch.stop;
  PaintMs := LStopWatch.Elapsed.TotalMilliseconds;
  if LRemoveMakeBufDrawableMs then PaintMs := PaintMs - MakeBufDrawableMs;
end;

{*******************************************}
procedure TALCircleStopWatch.MakeBufDrawable;
begin
  if ALIsDrawableNull(FBufDrawable) then begin
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
  var LRemoveMakeBufDrawableMs := ALIsDrawableNull(FBufDrawable);
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
