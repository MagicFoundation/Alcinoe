unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, ALFmxObjects, FMX.Layouts,
  ALFmxLayouts, fmx.types3D, ALFmxCommon, System.ImageList,
  FMX.ImgList, ALFmxStdCtrls, ALFmxTabControl,
  FMX.ScrollBox, FMX.Edit, ALFmxEdit, ALVideoPlayer, ALDatePickerDialog, FMX.Effects,
  {$IF Defined(IOS) or Defined(ANDROID)}
  Grijjy.ErrorReporting,
  {$ENDIF}
  FMX.Filter.Effects, system.Messaging, ALFMXAni, alFmxMemo;

type

  TALRectangleStopWatch = Class(TALRectangle)
  protected
     bufPaintMs: double;
     bufCreatePaintMs: double;
     procedure Paint; override;
  public
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     function MakeBufBitmap: TTexture; override;
    {$ELSE}
     function MakeBufBitmap: Tbitmap; override;
    {$ENDIF}
  End;
  TRectangleStopWatch = Class(TRectangle)
  public
     PaintMs: double;
     procedure Paint; override;
  End;

  TALCircleStopWatch = Class(TALCircle)
  protected
     bufPaintMs: double;
     bufCreatePaintMs: double;
     procedure Paint; override;
  public
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     function MakeBufBitmap: TTexture; override;
    {$ELSE}
     function MakeBufBitmap: Tbitmap; override;
    {$ENDIF}
  End;
  TCircleStopWatch = Class(TCircle)
  public
     PaintMs: double;
     procedure Paint; override;
  End;

  TALTextStopWatch = Class(TalText)
  protected
     bufPaintMs: double;
     bufCreatePaintMs: double;
     procedure Paint; override;
  public
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     function MakeBufBitmap: TTexture; override;
    {$ELSE}
     function MakeBufBitmap: Tbitmap; override;
    {$ENDIF}
  End;
  TTextStopWatch = Class(TText)
  public
     PaintMs: double;
     procedure Paint; override;
  End;

  TALCheckBoxStopWatch = Class(TALCheckBox)
  protected
     bufPaintMs: double;
     bufCreatePaintMs: double;
     procedure Paint; override;
  public
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     function MakeBufBitmap: TTexture; override;
    {$ELSE}
     function MakeBufBitmap: Tbitmap; override;
    {$ENDIF}
  End;
  TCheckBoxStopWatch = Class(TCheckBox)
  public
     PaintMs: double;
     procedure PaintChildren; override;
  End;

  TLineStopWatch = Class(TLine)
  public
     PaintMs: double;
     procedure Paint; override;
  End;
  TALLineStopWatch = Class(TALLine)
  protected
     bufPaintMs: double;
     bufCreatePaintMs: double;
  public
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     function MakeBufBitmap: TTexture; override;
    {$ELSE}
     function MakeBufBitmap: Tbitmap; override;
    {$ENDIF}
     procedure Paint; override;
  End;

  TALRangeTrackBarStopWatch = Class(TALRangeTrackBar)
  public
     PaintMs: double;
     procedure PaintChildren; override;
  End;

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button15: TButton;
    ALVertScrollBox1: TALVertScrollBox;
    Button4: TButton;
    Text1: TText;
    Button5: TButton;
    Button3: TButton;
    Text4: TText;
    Button6: TButton;
    Button7: TButton;
    Text2: TText;
    Button8: TButton;
    Button9: TButton;
    Text5: TText;
    Button13: TButton;
    Text10: TText;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Text3: TText;
    Button20: TButton;
    Text7: TText;
    Text9: TText;
    Button21: TButton;
    Button22: TButton;
    ALTabControl1: TALTabControl;
    ALTabItem1: TALTabItem;
    ALTabItem2: TALTabItem;
    Image2: TALImage;
    ALText1: TALText;
    ALText2: TALText;
    ALTabItem3: TALTabItem;
    Image3: TALImage;
    ALTabItem4: TALTabItem;
    image4: TalImage;
    ALText3: TALText;
    ALText4: TALText;
    ALText5: TALText;
    ALText6: TALText;
    ALText7: TALText;
    ALText8: TALText;
    ALRectangle9: TALRectangle;
    ALRangeTrackBar1: TALRangeTrackBar;
    ALTrackBar1: TALTrackBar;
    Button14: TButton;
    Layout3: TLayout;
    Layout4: TLayout;
    Text11: TALText;
    Button12: TButton;
    ALText9: TALText;
    Text8: TText;
    ALVideoPlayerSurface1: TALVideoPlayerSurface;
    Text12: TText;
    MonochromeEffect1: TMonochromeEffect;
    RippleEffect1: TRippleEffect;
    MagnifyEffect1: TMagnifyEffect;
    SmoothMagnifyEffect1: TSmoothMagnifyEffect;
    WaveEffect1: TWaveEffect;
    BandedSwirlEffect1: TBandedSwirlEffect;
    PencilStrokeEffect1: TPencilStrokeEffect;
    PaperSketchEffect1: TPaperSketchEffect;
    PixelateEffect1: TPixelateEffect;
    ALText10: TALText;
    ContrastEffect1: TContrastEffect;
    TilerEffect1: TTilerEffect;
    MaskToAlphaEffect1: TMaskToAlphaEffect;
    EmbossEffect1: TEmbossEffect;
    PinchEffect1: TPinchEffect;
    alImage1: TalImage;
    ALLayout1: TALLayout;
    ALAniIndicator1: TALAniIndicator;
    Button19: TButton;
    ALRectangle1: TALRectangle;
    Button10: TButton;
    Text6: TText;
    ALRectangle3: TALRectangle;
    ALEdit1: TALEdit;
    Layout1: TLayout;
    Image1: TImage;
    Layout2: TLayout;
    Image5: TImage;
    ALMemo1: TALMemo;
    Button11: TButton;
    ALSwitch1: TALSwitch;
    Layout5: TLayout;
    Button23: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button255Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure ALTabControl1ViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF);
    procedure ALTabControl1AniTransitionInit(const sender: TObject;
                                             const ATransition: TALTabTransition;
                                             const aVelocity: Double;
                                             const aAnimation: TALFloatPropertyAnimation);
    procedure ALTabControl1Resize(Sender: TObject);
    procedure ALVertScrollBox1ScrollBarInit(const sender: TObject; const aScrollBar: TALScrollBoxBar);
    procedure VScrollBarThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure VScrollBarThumbMouseLeave(Sender: TObject);
    procedure VScrollBarThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure VScrollBarThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormVirtualKeyboardHidden(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardShown(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure ALVertScrollBox1Click(Sender: TObject);
    procedure ALVideoPlayerSurface1DblClick(Sender: TObject);
    procedure ALVertScrollBox1ViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF);
    procedure FormResize(Sender: TObject);
    procedure ALEditEnter(Sender: TObject);
    procedure ALEditExit(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ALMemo1Exit(Sender: TObject);
    procedure ALVideoPlayerSurface1VideoSizeChanged(const Sender: TObject;
      const width, height: Integer);
    procedure Button11Click(Sender: TObject);
    procedure ALSwitch1Change(Sender: TObject);
    procedure Button23Click(Sender: TObject);
  private
    FDatePickerDialog: TALDatePickerDialog;
    fALcheckbox2: TALcheckboxStopWatch;
    fcheckbox2: TcheckboxStopWatch;
    fLine: TLineStopWatch;
    fALLine: TALLineStopWatch;
    fText: TTextStopWatch;
    fALText: TALTextStopWatch;
    fALRectangle1: TALRectangleStopWatch;
    fRectangle1: TRectangleStopWatch;
    fALRectangle2: TALRectangleStopWatch;
    fRectangle2: TRectangleStopWatch;
    fALRectangle3: TALRectangleStopWatch;
    fRectangle3: TRectangleStopWatch;
    fALCircle1: TALCircleStopWatch;
    fCircle1: TCircleStopWatch;
    FVKKeyboardOpen: boolean;
    fLastClickDT: TdateTime;
    {$IF Defined(IOS) or Defined(ANDROID)}
    procedure ApplicationExceptionHandler(const Sender: TObject; const M: TMessage);
    {$ENDIF}
    {$IF Defined(MSWINDOWS) or Defined(_MACOS)}
    procedure ApplicationExceptionHandler(Sender: TObject; E: Exception);
    {$ENDIF}
  public
  end;

var
  Form1: TForm1;

implementation

uses system.Diagnostics,
     system.threading,
     system.Math,
     UnitDemo,
     system.DateUtils,
     ALFmxInertialMovement,
     ALCommon;

{$R *.fmx}

procedure TForm1.ALEditEnter(Sender: TObject);
begin
  ALLog('ALEditEnter', 'ALEditEnter');
end;

procedure TForm1.ALEditExit(Sender: TObject);
begin
  ALLog('ALEditExit', 'ALEditExit');
end;

procedure TForm1.ALMemo1Exit(Sender: TObject);
begin
 //
end;

procedure TForm1.ALSwitch1Change(Sender: TObject);
begin
  if ALSwitch1.IsChecked then begin
    ALSwitch1.BackGround.Fill.Color := $fffbd7d7;
    ALSwitch1.Thumb.Stroke.Color := $ffec6262;
    ALSwitch1.Thumb.Fill.Color := $ffec6262;
  end
  else begin
    ALSwitch1.BackGround.Fill.Color := $ffc5c5c5;
    ALSwitch1.Thumb.Stroke.Color := $ffd5d5d5;
    ALSwitch1.Thumb.Fill.Color := $ffffffff;
  end;

end;

procedure TForm1.ALVertScrollBox1Click(Sender: TObject);
begin
  SetFocused(nil);
end;

procedure TForm1.ALTabControl1AniTransitionInit(const sender: TObject;
                                                const ATransition: TALTabTransition;
                                                const aVelocity: Double;
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
  aAnimation.Interpolation := TInterpolationType.circular;
end;

procedure TForm1.ALTabControl1Resize(Sender: TObject);

  procedure _updateLabels(const aTab: TalTabItem);
  var aText1, aText2: TalText;
      aControl1: Tcontrol;
  begin
    aText1 := nil;
    aText2 := nil;
    for aControl1 in aTab.Controls do begin
      if (aControl1 is TalText) and (aControl1.Tag = 1) then aText1 := TalText(aControl1)
      else if (aControl1 is TalText) and (aControl1.Tag = 2) then aText2 := TalText(aControl1);
    end;
    if aText1 <> nil then
      aText1.Position.X := ((aTab.Width - aText1.Width) / 2) + (aTab.Position.X / 5);
    if aText2 <> nil then
      aText2.Position.X := ((aTab.Width - aText2.Width) / 2) + (aTab.Position.X);
  end;

begin
  if ALTabControl1.TabIndex > 0 then _updateLabels(ALTabControl1.tabs[ALTabControl1.TabIndex - 1]);
  _updateLabels(ALTabControl1.tabs[ALTabControl1.TabIndex]);
  if ALTabControl1.TabIndex < ALTabControl1.Tabcount - 1 then _updateLabels(ALTabControl1.tabs[ALTabControl1.TabIndex + 1]);
end;

procedure TForm1.ALTabControl1ViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF);
begin
  ALTabControl1Resize(nil);
end;

procedure TForm1.ALVertScrollBox1ScrollBarInit(const sender: TObject; const aScrollBar: TALScrollBoxBar);
begin
  //special case for windows
  if not ALVertScrollBox1.HasTouchScreen then begin
    if aScrollBar.Orientation = Torientation.Vertical then begin
      aScrollBar.Width := 8;
      aScrollBar.Margins.Right := 3;
      aScrollBar.Thumb.XRadius := 4;
      aScrollBar.Thumb.yRadius := 4;
    end;
  end;
end;

procedure TForm1.ALVertScrollBox1ViewportPositionChange(Sender: TObject;const OldViewportPosition, NewViewportPosition: TPointF);
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

      ALVideoPlayerSurface1.VideoPlayer.prepare('http://distribution.bbb3d.renderfarming.net/video/mp4/bbb_sunflower_1080p_30fps_normal.mp4', True{AutoStartWhenPrepared}); // << no sound on ios, don't know why :(
      //ALVideoPlayerSurface1.VideoPlayer.prepare('http://distribution.bbb3d.renderfarming.net/video/mp4/bbb_sunflower_1080p_60fps_normal.mp4', True{AutoStartWhenPrepared});
      //ALVideoPlayerSurface1.VideoPlayer.prepare('http://distribution.bbb3d.renderfarming.net/video/mp4/bbb_sunflower_2160p_60fps_normal.mp4', True{AutoStartWhenPrepared});
      //ALVideoPlayerSurface1.VideoPlayer.prepare('http://techslides.com/demos/samples/sample.mp4', True{AutoStartWhenPrepared}); // << this have sound on ios

    end
    else if ALVideoPlayerSurface1.VideoPlayer.state in [vpsPrepared, vpsPaused] then ALVideoPlayerSurface1.VideoPlayer.Start
    else ALVideoPlayerSurface1.VideoPlayer.AutoStartWhenPrepared := true;
  end
  else begin
    if ALVideoPlayerSurface1.VideoPlayer.state in [vpsStarted] then ALVideoPlayerSurface1.VideoPlayer.Pause
    else ALVideoPlayerSurface1.VideoPlayer.AutoStartWhenPrepared := False;
  end;


  if (ALLayout1.Position.y + ALLayout1.Height >= NewViewportPosition.Y) and
     (ALLayout1.Position.y < NewViewportPosition.Y + height) then begin
    ALAniIndicator1.enabled := true;
  end
  else begin
    ALAniIndicator1.enabled := False;
  end;

  if ALMemo1.Position.Y - NewViewportPosition.Y > clientHeight then ALMemo1.RemoveNativeView
  else ALMemo1.AddNativeView;

  if ALRectangle3.Position.Y - NewViewportPosition.Y > clientHeight then ALEdit1.RemoveNativeView
  else ALEdit1.AddNativeView;

end;

procedure TForm1.ALVideoPlayerSurface1DblClick(Sender: TObject);
begin
  if milliSecondsBetween(fLastClickDT, NOW) > 300 then begin
    fLastClickDT := NOW;
    exit;
  end;
  fLastClickDT := Now;

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
  Text12.Text := 'Z-order work !';

  ALVideoPlayerSurface1.Tag := ALVideoPlayerSurface1.Tag + 1;
  if ALVideoPlayerSurface1.Tag > 15 then ALVideoPlayerSurface1.Tag := 0;

  case ALVideoPlayerSurface1.Tag of
    1: begin BandedSwirlEffect1.Enabled := true; Text12.Text := 'BandedSwirlEffect'; end;
    2: begin ContrastEffect1.Enabled := true; Text12.Text := 'ContrastEffect'; end;
    3: begin MagnifyEffect1.Enabled := true; Text12.Text := 'MagnifyEffect'; end;
    4: begin MonochromeEffect1.Enabled := true; Text12.Text := 'MonochromeEffect'; end;
    5: begin RippleEffect1.Enabled := true; Text12.Text := 'RippleEffect'; end;
    6: begin SmoothMagnifyEffect1.Enabled := true; Text12.Text := 'SmoothMagnifyEffect'; end;
    7: begin WaveEffect1.Enabled := true; Text12.Text := 'WaveEffect'; end;
    8: begin PencilStrokeEffect1.Enabled := true; Text12.Text := 'PencilStrokeEffect'; end;
    9: begin PaperSketchEffect1.Enabled := true; Text12.Text := 'PaperSketchEffect'; end;
    10: begin PixelateEffect1.Enabled := true; Text12.Text := 'PixelateEffect'; end;
    11: begin TilerEffect1.Enabled := true; Text12.Text := 'TilerEffect'; end;
    12: begin MaskToAlphaEffect1.Enabled := true; Text12.Text := 'MaskToAlphaEffect'; end;
    13: begin EmbossEffect1.Enabled := true; Text12.Text := 'EmbossEffect'; end;
    14: begin PinchEffect1.Enabled := true; Text12.Text := 'PinchEffect'; end;
  end;
end;

procedure TForm1.ALVideoPlayerSurface1VideoSizeChanged(const Sender: TObject; const width, height: Integer);
begin
  ALVideoPlayerSurface1.Height := height * (ClientWidth/Width)
end;

procedure TForm1.VScrollBarThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if not ALVertScrollBox1.HasTouchScreen then begin
    with (sender as TALTrackThumb) do begin
      if tag = 0 then begin // 0 = out and not down
        tag := 1; // 1 = in and not down
        Fill.Color := $64000000;
        (sender as TALTrackThumb).InvalidateRect(localrect);
      end
      else if tag = 10 then  // 10 = out and down
        tag := 11; // 11 = in and down
    end;
  end;
end;

procedure TForm1.VScrollBarThumbMouseLeave(Sender: TObject);
begin
  if not ALVertScrollBox1.HasTouchScreen then begin
    with (sender as TALTrackThumb) do begin
      if tag = 1 then begin // 1 = in and not down
        tag := 0; // 0 = out and not down
        Fill.Color := $47000000;
        (sender as TALTrackThumb).InvalidateRect(localrect);
      end
      else if tag = 11 then // 11 = in and down
        tag := 10; // 10 = out and down
    end;
  end;
end;

procedure TForm1.VScrollBarThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not ALVertScrollBox1.HasTouchScreen then begin
    with (sender as TALTrackThumb) do begin
      if tag = 1 then // 1 = in and not down
        tag := 11 // 11 = in and down
      else
        tag := 10; // 10 = out and down
      Fill.Color := $96000000;
      (sender as TALTrackThumb).InvalidateRect(localrect);
    end;
  end;
end;

procedure TForm1.VScrollBarThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not ALVertScrollBox1.HasTouchScreen then begin
    with (sender as TALTrackThumb) do begin
      if tag = 11 then begin // 11 = in and down
        tag := 1; // 1 = in and not down
        Fill.Color := $64000000;
      end
      else if tag = 10 then begin // 10 = out and down
        tag := 0; // 0 = out and not down
        Fill.Color := $47000000;
      end;
      (sender as TALTrackThumb).InvalidateRect(localrect);
    end;
  end;
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  ALSwitch1.IsChecked := not ALSwitch1.IsChecked;
end;

procedure TForm1.Button13Click(Sender: TObject);
begin
  fline.Repaint;
  Text10.Text := 'Paint: ' + FormatFloat('0.#####',fline.PaintMs) + ' ms';
end;

procedure TForm1.Button15Click(Sender: TObject);
begin
  fALText.clearBufBitmap;
  fALText.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text4.Text := 'buffer creation: ' + FormatFloat('0.#####',fALText.bufCreatePaintMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALText.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.Button16Click(Sender: TObject);
begin
  fALLine.clearBufBitmap;
  fALLine.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text10.Text := 'buffer creation: ' + FormatFloat('0.#####',fALLine.bufCreatePaintMs) + ' ms - ' +
                         'Paint: ' + FormatFloat('0.#####',fALLine.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.Button17Click(Sender: TObject);
begin
  fALcheckbox2.clearBufBitmap;
  fALcheckbox2.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text3.Text := 'buffer creation: ' + FormatFloat('0.#####',fALcheckbox2.bufCreatePaintMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALcheckbox2.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.Button18Click(Sender: TObject);
begin
  fcheckbox2.Repaint;
  Text3.Text := 'Paint: ' + FormatFloat('0.#####',fcheckbox2.PaintMs) + ' ms';
end;


procedure TForm1.Button2Click(Sender: TObject);
Var aDemoForm: TDemoForm;
    aRectangle: TALRectangle;
    aChildRectangle: TalRectangle;
    //aChildRectangle2: TalRectangle;
    aVertScrollBox: TalVertScrollBox;
    aText: TALText;
    i: integer;
begin
  aDemoForm := TDemoForm.Create(nil);
  aVertScrollBox := TalVertScrollBox.Create(aDemoForm);
  aVertScrollBox.Parent := aDemoForm;
  aVertScrollBox.BeginUpdate;
  aVertScrollBox.Align := TalignLayout.Client;
  aVertScrollBox.AniCalculations.VelocityFactor := 2;
  aVertScrollBox.AniCalculations.BoundsAnimation := False;
  aDemoForm.fALAniCalculations := aVertScrollBox.AniCalculations;
  aDemoForm.fvertScrollBox := aVertScrollBox;
  for I := 1 to 50 do begin
    aRectangle := TALRectangle.Create(aVertScrollBox);
    aRectangle.Parent := aVertScrollBox;
    aRectangle.doubleBuffered := True;
    aRectangle.Align := TalignLayout.Top;
    aRectangle.Margins.Left := 15;
    aRectangle.Margins.Top := 10;
    aRectangle.Margins.Right := 15;
    aRectangle.Margins.Bottom := 10;
    aRectangle.Position.Y := 0;
    aRectangle.Size.Height := 50;
    aRectangle.XRadius := 12;
    aRectangle.YRadius := 12;
    //-----
    aText := TALText.Create(self);
    aText.Parent := aRectangle;
    aText.doubleBuffered := True;
    aText.Align := TalignLayout.left;
    aText.Text := 'Alcinoe';
    aText.Margins.Left := 15;
    aText.WordWrap := False;
    aText.autosize := True;
    //-----
    aChildRectangle := TALRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.red;
      //-----
      (*
      aChildRectangle2 := TALRectangle.Create(aRectangle);
      aChildRectangle2.Parent := aChildRectangle;
      aChildRectangle2.doubleBuffered := True;
      aChildRectangle2.Align := TalignLayout.client;
      aChildRectangle2.Margins.Left := 5;
      aChildRectangle2.Margins.Top := 5;
      aChildRectangle2.Margins.Right := 5;
      aChildRectangle2.Margins.Bottom := 5;
      aChildRectangle2.XRadius := 5;
      aChildRectangle2.YRadius := 5;
      aChildRectangle2.Fill.Color := TAlphaColorRec.Darkblue;
      *)
    //-----
    aChildRectangle := TALRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.green;
      //-----
      (*
      aChildRectangle2 := TALRectangle.Create(aRectangle);
      aChildRectangle2.Parent := aChildRectangle;
      aChildRectangle2.doubleBuffered := True;
      aChildRectangle2.Align := TalignLayout.client;
      aChildRectangle2.Margins.Left := 5;
      aChildRectangle2.Margins.Top := 5;
      aChildRectangle2.Margins.Right := 5;
      aChildRectangle2.Margins.Bottom := 5;
      aChildRectangle2.XRadius := 5;
      aChildRectangle2.YRadius := 5;
      aChildRectangle2.Fill.Color := TAlphaColorRec.Coral;
      *)
    //-----
    aChildRectangle := TALRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.blue;
      //-----
      (*
      aChildRectangle2 := TALRectangle.Create(aRectangle);
      aChildRectangle2.Parent := aChildRectangle;
      aChildRectangle2.doubleBuffered := True;
      aChildRectangle2.Align := TalignLayout.client;
      aChildRectangle2.Margins.Left := 5;
      aChildRectangle2.Margins.Top := 5;
      aChildRectangle2.Margins.Right := 5;
      aChildRectangle2.Margins.Bottom := 5;
      aChildRectangle2.XRadius := 5;
      aChildRectangle2.YRadius := 5;
      aChildRectangle2.Fill.Color := TAlphaColorRec.Indigo;
      *)
    //-----
    aChildRectangle := TALRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.yellow;
      //-----
      (*
      aChildRectangle2 := TALRectangle.Create(aRectangle);
      aChildRectangle2.Parent := aChildRectangle;
      aChildRectangle2.doubleBuffered := True;
      aChildRectangle2.Align := TalignLayout.client;
      aChildRectangle2.Margins.Left := 5;
      aChildRectangle2.Margins.Top := 5;
      aChildRectangle2.Margins.Right := 5;
      aChildRectangle2.Margins.Bottom := 5;
      aChildRectangle2.XRadius := 5;
      aChildRectangle2.YRadius := 5;
      aChildRectangle2.Fill.Color := TAlphaColorRec.Deeppink;
      *)
    //-----
    aChildRectangle := TALRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.Orange;
      //-----
      (*
      aChildRectangle2 := TALRectangle.Create(aRectangle);
      aChildRectangle2.Parent := aChildRectangle;
      aChildRectangle2.doubleBuffered := True;
      aChildRectangle2.Align := TalignLayout.client;
      aChildRectangle2.Margins.Left := 5;
      aChildRectangle2.Margins.Top := 5;
      aChildRectangle2.Margins.Right := 5;
      aChildRectangle2.Margins.Bottom := 5;
      aChildRectangle2.XRadius := 5;
      aChildRectangle2.YRadius := 5;
      aChildRectangle2.Fill.Color := TAlphaColorRec.Lavender;
      *)
    //-----
    aChildRectangle := TALRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.Aliceblue;
      //-----
      (*
      aChildRectangle2 := TALRectangle.Create(aRectangle);
      aChildRectangle2.Parent := aChildRectangle;
      aChildRectangle2.doubleBuffered := True;
      aChildRectangle2.Align := TalignLayout.client;
      aChildRectangle2.Margins.Left := 5;
      aChildRectangle2.Margins.Top := 5;
      aChildRectangle2.Margins.Right := 5;
      aChildRectangle2.Margins.Bottom := 5;
      aChildRectangle2.XRadius := 5;
      aChildRectangle2.YRadius := 5;
      aChildRectangle2.Fill.Color := TAlphaColorRec.Oldlace;
      *)
    //-----
    aChildRectangle := TALRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.Darkgoldenrod;
      //-----
      (*
      aChildRectangle2 := TALRectangle.Create(aRectangle);
      aChildRectangle2.Parent := aChildRectangle;
      aChildRectangle2.doubleBuffered := True;
      aChildRectangle2.Align := TalignLayout.client;
      aChildRectangle2.Margins.Left := 5;
      aChildRectangle2.Margins.Top := 5;
      aChildRectangle2.Margins.Right := 5;
      aChildRectangle2.Margins.Bottom := 5;
      aChildRectangle2.XRadius := 5;
      aChildRectangle2.YRadius := 5;
      aChildRectangle2.Fill.Color := TAlphaColorRec.Silver;
      *)
  end;
  aVertScrollBox.endUpdate;
  aDemoForm.Show;
  ALFmxMakeBufBitmaps(aVertScrollBox);
end;

procedure TForm1.Button1Click(Sender: TObject);
Var aDemoForm: TDemoForm;
    aRectangle: TRectangle;
    aChildRectangle: TRectangle;
    //aChildRectangle2: TRectangle;
    aVertScrollBox: TVertScrollBox;
    aText: TText;
    i: integer;
begin
  aDemoForm := TDemoForm.Create(nil);
  aVertScrollBox := TVertScrollBox.Create(aDemoForm);
  aVertScrollBox.Parent := aDemoForm;
  aVertScrollBox.BeginUpdate;
  aVertScrollBox.Align := TalignLayout.Client;
  //aVertScrollBox.AniCalculations.VelocityFactor := 2;
  aVertScrollBox.AniCalculations.BoundsAnimation := False;
  aDemoForm.fAniCalculations := aVertScrollBox.AniCalculations;
  aDemoForm.fvertScrollBox := aVertScrollBox;
  for I := 1 to 50 do begin
    aRectangle := TRectangle.Create(aVertScrollBox);
    aRectangle.Parent := aVertScrollBox;
    //aRectangle.doubleBuffered := True;
    aRectangle.Align := TalignLayout.Top;
    aRectangle.Margins.Left := 15;
    aRectangle.Margins.Top := 10;
    aRectangle.Margins.Right := 15;
    aRectangle.Margins.Bottom := 10;
    aRectangle.Position.Y := 0;
    aRectangle.Size.Height := 50;
    aRectangle.XRadius := 12;
    aRectangle.YRadius := 12;
    //-----
    aText := TText.Create(self);
    aText.Parent := aRectangle;
    //aText.doubleBuffered := True;
    aText.Align := TalignLayout.left;
    aText.Text := 'Alcinoe';
    aText.Margins.Left := 15;
    aText.WordWrap := False;
    aText.autosize := True;
    //-----
    aChildRectangle := TRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    //aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.red;
      //-----
      (*
      aChildRectangle2 := TRectangle.Create(aRectangle);
      aChildRectangle2.Parent := aChildRectangle;
      //aChildRectangle2.doubleBuffered := True;
      aChildRectangle2.Align := TalignLayout.client;
      aChildRectangle2.Margins.Left := 5;
      aChildRectangle2.Margins.Top := 5;
      aChildRectangle2.Margins.Right := 5;
      aChildRectangle2.Margins.Bottom := 5;
      aChildRectangle2.XRadius := 5;
      aChildRectangle2.YRadius := 5;
      aChildRectangle2.Fill.Color := TAlphaColorRec.Darkblue;
      *)
    //-----
    aChildRectangle := TRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    //aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.green;
      //-----
      (*
      aChildRectangle2 := TRectangle.Create(aRectangle);
      aChildRectangle2.Parent := aChildRectangle;
      //aChildRectangle2.doubleBuffered := True;
      aChildRectangle2.Align := TalignLayout.client;
      aChildRectangle2.Margins.Left := 5;
      aChildRectangle2.Margins.Top := 5;
      aChildRectangle2.Margins.Right := 5;
      aChildRectangle2.Margins.Bottom := 5;
      aChildRectangle2.XRadius := 5;
      aChildRectangle2.YRadius := 5;
      aChildRectangle2.Fill.Color := TAlphaColorRec.Coral;
      *)
    //-----
    aChildRectangle := TRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    //aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.blue;
      //-----
      (*
      aChildRectangle2 := TRectangle.Create(aRectangle);
      aChildRectangle2.Parent := aChildRectangle;
      //aChildRectangle2.doubleBuffered := True;
      aChildRectangle2.Align := TalignLayout.client;
      aChildRectangle2.Margins.Left := 5;
      aChildRectangle2.Margins.Top := 5;
      aChildRectangle2.Margins.Right := 5;
      aChildRectangle2.Margins.Bottom := 5;
      aChildRectangle2.XRadius := 5;
      aChildRectangle2.YRadius := 5;
      aChildRectangle2.Fill.Color := TAlphaColorRec.Indigo;
      *)
    //-----
    aChildRectangle := TRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    //aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.yellow;
      //-----
      (*
      aChildRectangle2 := TRectangle.Create(aRectangle);
      aChildRectangle2.Parent := aChildRectangle;
      //aChildRectangle2.doubleBuffered := True;
      aChildRectangle2.Align := TalignLayout.client;
      aChildRectangle2.Margins.Left := 5;
      aChildRectangle2.Margins.Top := 5;
      aChildRectangle2.Margins.Right := 5;
      aChildRectangle2.Margins.Bottom := 5;
      aChildRectangle2.XRadius := 5;
      aChildRectangle2.YRadius := 5;
      aChildRectangle2.Fill.Color := TAlphaColorRec.Deeppink;
      *)
    //-----
    aChildRectangle := TRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    //aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.Orange;
      //-----
      (*
      aChildRectangle2 := TRectangle.Create(aRectangle);
      aChildRectangle2.Parent := aChildRectangle;
      //aChildRectangle2.doubleBuffered := True;
      aChildRectangle2.Align := TalignLayout.client;
      aChildRectangle2.Margins.Left := 5;
      aChildRectangle2.Margins.Top := 5;
      aChildRectangle2.Margins.Right := 5;
      aChildRectangle2.Margins.Bottom := 5;
      aChildRectangle2.XRadius := 5;
      aChildRectangle2.YRadius := 5;
      aChildRectangle2.Fill.Color := TAlphaColorRec.Lavender;
      *)
    //-----
    aChildRectangle := TRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    //aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.Aliceblue;
      //-----
      (*
      aChildRectangle2 := TRectangle.Create(aRectangle);
      aChildRectangle2.Parent := aChildRectangle;
      //aChildRectangle2.doubleBuffered := True;
      aChildRectangle2.Align := TalignLayout.client;
      aChildRectangle2.Margins.Left := 5;
      aChildRectangle2.Margins.Top := 5;
      aChildRectangle2.Margins.Right := 5;
      aChildRectangle2.Margins.Bottom := 5;
      aChildRectangle2.XRadius := 5;
      aChildRectangle2.YRadius := 5;
      aChildRectangle2.Fill.Color := TAlphaColorRec.Oldlace;
      *)
    //-----
    aChildRectangle := TRectangle.Create(aRectangle);
    aChildRectangle.Parent := aRectangle;
    //aChildRectangle.doubleBuffered := True;
    aChildRectangle.Align := TalignLayout.right;
    aChildRectangle.Margins.Left := 0;
    aChildRectangle.Margins.Top := 10;
    aChildRectangle.Margins.Right := random(15);
    aChildRectangle.Margins.Bottom := 10;
    aChildRectangle.Position.Y := 0;
    aChildRectangle.Size.width := 25;
    aChildRectangle.XRadius := 5;
    aChildRectangle.YRadius := 5;
    aChildRectangle.Fill.Color := TAlphaColorRec.Darkgoldenrod;
      //-----
      (*
      aChildRectangle2 := TRectangle.Create(aRectangle);
      aChildRectangle2.Parent := aChildRectangle;
      //aChildRectangle2.doubleBuffered := True;
      aChildRectangle2.Align := TalignLayout.client;
      aChildRectangle2.Margins.Left := 5;
      aChildRectangle2.Margins.Top := 5;
      aChildRectangle2.Margins.Right := 5;
      aChildRectangle2.Margins.Bottom := 5;
      aChildRectangle2.XRadius := 5;
      aChildRectangle2.YRadius := 5;
      aChildRectangle2.Fill.Color := TAlphaColorRec.Silver;
      *)
  end;
  aVertScrollBox.endUpdate;
  aDemoForm.Show;
  //ALFmxMakeBufBitmaps(aVertScrollBox);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  fText.Repaint;
  Text4.Text := 'Paint: ' + FormatFloat('0.#####',fText.PaintMs) + ' ms';
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  fRectangle1.Repaint;
  Text1.Text := 'Paint: ' + FormatFloat('0.#####',fRectangle1.PaintMs) + ' ms';
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  fRectangle2.Repaint;
  Text2.Text := 'Paint: ' + FormatFloat('0.#####',fRectangle2.PaintMs) + ' ms';
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  fRectangle3.Repaint;
  Text5.Text := 'Paint: ' + FormatFloat('0.#####',fRectangle3.PaintMs) + ' ms';
end;

procedure TForm1.Button21Click(Sender: TObject);
begin
  fCircle1.Repaint;
  Text9.Text := 'Paint: ' + FormatFloat('0.#####',fCircle1.PaintMs) + ' ms';
end;

procedure TForm1.Button22Click(Sender: TObject);
begin
  fALCircle1.clearBufBitmap;
  fALCircle1.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text9.Text := 'buffer creation: ' + FormatFloat('0.#####',fALCircle1.bufCreatePaintMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALCircle1.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.Button23Click(Sender: TObject);
begin
  ALFreeAndNil(fDatePickerDialog);
  fDatePickerDialog := TALDatePickerDialog.create('OK', // const aBtnOKCaption: string;
                                                  'Cancel', // const aBtnCancelCaption: string;
                                                  '', // const aBtnClearCaption: string
                                                  'this is a title');// const aTitle: String = ''
  fDatePickerDialog.show(YearOf(now), // const aYear: integer;
                         MonthOf(now), // const aMonth: integer;
                         DayOfTheMonth(now)); // const aDayOfMonth: integer);

end;

procedure TForm1.Button255Click(Sender: TObject);
begin
  fALRectangle1.clearBufBitmap;
  fALRectangle1.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text1.Text := 'buffer creation: ' + FormatFloat('0.#####',fALRectangle1.bufCreatePaintMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALRectangle1.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  fALRectangle2.clearBufBitmap;
  fALRectangle2.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text2.Text := 'buffer creation: ' + FormatFloat('0.#####',fALRectangle2.bufCreatePaintMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALRectangle2.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  fALRectangle3.clearBufBitmap;
  fALRectangle3.repaint;
  TTask.Create (procedure ()
    begin
      sleep(250);
      TThread.Synchronize(nil,
        procedure
        begin
          Text5.Text := 'buffer creation: ' + FormatFloat('0.#####',fALRectangle3.bufCreatePaintMs) + ' ms - ' +
                        'Paint: ' + FormatFloat('0.#####',fALRectangle3.bufPaintMs) + ' ms';
        end);
    end).Start;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  {$IF Defined(IOS) or Defined(ANDROID)}
  Application.OnException := TgoExceptionReporter.ExceptionHandler;
  TMessageManager.DefaultManager.SubscribeToMessage(TgoExceptionReportMessage, ApplicationExceptionHandler);
  {$ELSE}
  Application.OnException := ApplicationExceptionHandler;
  {$ENDIF}

  fDatePickerDialog := nil;

  FVKKeyboardOpen := False;
  beginupdate;

  //-----
  ALVideoPlayerSurface1.Height := (width / 1920) * 1080;
  //-----
  fALRectangle1 := TALRectangleStopWatch.Create(self);
  fALRectangle1.Parent := ALVertScrollBox1;
  fALRectangle1.doubleBuffered := True;
  fALRectangle1.Align := TalignLayout.Top;
  fALRectangle1.Margins.Left := 15;
  fALRectangle1.Margins.Top := 15;
  fALRectangle1.Margins.Right := 15;
  fALRectangle1.Margins.Bottom := 15;
  fALRectangle1.Position.Y := Button5.Position.Y - Button5.Margins.Top;
  fALRectangle1.Size.Height := 50;
  fALRectangle1.XRadius := 12;
  fALRectangle1.YRadius := 12;
  //-----
  fRectangle1 := TRectangleStopWatch.Create(self);
  fRectangle1.Parent := ALVertScrollBox1;
  fRectangle1.Align := TalignLayout.Top;
  fRectangle1.Margins.Left := 15;
  fRectangle1.Margins.Top := 15;
  fRectangle1.Margins.Right := 15;
  fRectangle1.Margins.Bottom := 15;
  fRectangle1.Position.Y := Button5.Position.Y - Button5.Margins.Top;
  fRectangle1.Size.Height := 50;
  fRectangle1.XRadius := 12;
  fRectangle1.YRadius := 12;

  //-----
  fALRectangle2 := TALRectangleStopWatch.Create(self);
  fALRectangle2.Parent := ALVertScrollBox1;
  fALRectangle2.doubleBuffered := True;
  fALRectangle2.Align := TalignLayout.Top;
  fALRectangle2.Margins.Left := 15;
  fALRectangle2.Margins.Top := 15;
  fALRectangle2.Margins.Right := 15;
  fALRectangle2.Margins.Bottom := 15;
  fALRectangle2.Position.Y := button6.Position.Y- Button6.Margins.Top;
  fALRectangle2.Size.Height := 50;
  fALRectangle2.XRadius := 0;
  fALRectangle2.YRadius := 0;
  //-----
  fRectangle2 := TRectangleStopWatch.Create(self);
  fRectangle2.Parent := ALVertScrollBox1;
  fRectangle2.Align := TalignLayout.Top;
  fRectangle2.Margins.Left := 15;
  fRectangle2.Margins.Top := 15;
  fRectangle2.Margins.Right := 15;
  fRectangle2.Margins.Bottom := 15;
  fRectangle2.Position.Y := button6.Position.Y- Button6.Margins.Top;
  fRectangle2.Size.Height := 50;
  fRectangle2.XRadius := 0;
  fRectangle2.YRadius := 0;

  //-----
  fALRectangle3 := TALRectangleStopWatch.Create(self);
  fALRectangle3.Parent := ALVertScrollBox1;
  fALRectangle3.doubleBuffered := True;
  fALRectangle3.Align := TalignLayout.Top;
  fALRectangle3.Margins.Left := 15;
  fALRectangle3.Margins.Top := 15;
  fALRectangle3.Margins.Right := 15;
  fALRectangle3.Margins.Bottom := 15;
  fALRectangle3.Position.Y := button8.Position.Y- Button8.Margins.Top;
  fALRectangle3.Size.Height := 50;
  fALRectangle3.Stroke.Kind := TbrushKind.None;
  fALRectangle3.XRadius := 0;
  fALRectangle3.YRadius := 0;
  //-----
  fRectangle3 := TRectangleStopWatch.Create(self);
  fRectangle3.Parent := ALVertScrollBox1;
  fRectangle3.Align := TalignLayout.Top;
  fRectangle3.Margins.Left := 15;
  fRectangle3.Margins.Top := 15;
  fRectangle3.Margins.Right := 15;
  fRectangle3.Margins.Bottom := 15;
  fRectangle3.Position.Y := button8.Position.Y- Button8.Margins.Top;
  fRectangle3.Size.Height := 50;
  fRectangle3.Stroke.Kind := TbrushKind.None;
  fRectangle3.XRadius := 0;
  fRectangle3.YRadius := 0;

  //-----
  fALCircle1 := TALCircleStopWatch.Create(self);
  fALCircle1.Parent := ALVertScrollBox1;
  fALCircle1.doubleBuffered := True;
  fALCircle1.Align := TalignLayout.Top;
  fALCircle1.Margins.Left := 15;
  fALCircle1.Margins.Top := 15;
  fALCircle1.Margins.Right := 15;
  fALCircle1.Margins.Bottom := 15;
  fALCircle1.Position.Y := button22.Position.Y- Button22.Margins.Top;
  fALCircle1.Size.Height := 50;
  //-----
  fCircle1 := TCircleStopWatch.Create(self);
  fCircle1.Parent := ALVertScrollBox1;
  fCircle1.Align := TalignLayout.Top;
  fCircle1.Margins.Left := 15;
  fCircle1.Margins.Top := 15;
  fCircle1.Margins.Right := 15;
  fCircle1.Margins.Bottom := 15;
  fCircle1.Position.Y := button22.Position.Y- Button22.Margins.Top;
  fCircle1.Size.Height := 50;

  //-----
  fALText := TALTextStopWatch.Create(self);
  fALText.Parent := ALVertScrollBox1;
  fALText.TextSettings.WordWrap := True;
  fALText.TextSettings.HorzAlign := TTextAlign.Center;
  fALText.doubleBuffered := True;
  fALText.Align := TalignLayout.Top;
  fALText.Margins.Top := 8;
  fALText.Position.Y := button15.Position.Y - button15.Margins.Top;
  fALText.Size.Height := 80;
  fALText.Text := 'TALText Random 😂 😉 😐 🙉 🙇 💑 👨‍👨‍👧‍👦 💪 💥 🐇 🌼 🍡 🌋 🗽 🚚 🎁 🎶 📫 azert yuio p qs dfg jhk lm wxvcn bkn ,;/'#167'  123 098 4756 '#168#163' * AZE' +
                  ' RUTY IOP LK QJSH DFU AZZE F WBX CN';
  //-----
  fText := TTextStopWatch.Create(self);
  fText.Parent := ALVertScrollBox1;
  fText.TextSettings.WordWrap := True;
  fText.TextSettings.HorzAlign := TTextAlign.Center;
  fText.Align := TalignLayout.Top;
  fText.Margins.Top := 8;
  fText.Position.Y := button15.Position.Y - button15.Margins.Top;
  fText.Size.Height := 80;
  fText.Text := 'TText Random 😂 😉 😐 🙉 🙇 💑 👨‍👨‍👧‍👦 💪 💥 🐇 🌼 🍡 🌋 🗽 🚚 🎁 🎶 📫 azert yuio p qs dfg jhk lm wxvcn bkn ,;/'#167'  123 098 4756 '#168#163' * AZE' +
                ' RUTY IOP LK QJSH DFU AZZE F WBX CN';

  //-----
  fALcheckbox2 := TALcheckboxStopWatch.Create(layout3);
  fALcheckbox2.Parent := layout3;
  fALcheckbox2.Height := 22;
  fALcheckbox2.width := 22;
  fALcheckbox2.Position.Point := TpointF.Create(0,0);
  //-----
  fcheckbox2 := TcheckboxStopWatch.Create(layout4);
  fcheckbox2.Parent := layout4;
  fcheckbox2.Text := '';
  fcheckbox2.Height := 22;
  fcheckbox2.width := 22;
  fcheckbox2.Position.Point := TpointF.Create(0,0);

  //-----
  fALline := TALLineStopWatch.Create(self);
  fALline.Parent := ALVertScrollBox1;
  fALline.doubleBuffered := true;
  fALline.Align := TalignLayout.Top;
  fALline.Margins.Top := 8;
  fALline.Margins.right := 24;
  fALline.Margins.left := 24;
  fALline.Height := 1;
  fALline.Position.Y := button16.Position.Y - button16.Margins.Top;
  fALline.LineType := TLineType.Top;
  //-----
  fline := TLineStopWatch.Create(self);
  fline.Parent := ALVertScrollBox1;
  fline.Align := TalignLayout.Top;
  fline.Margins.Top := 8;
  fline.Margins.right := 24;
  fline.Margins.left := 24;
  fline.Height := 1;
  fline.Position.Y := button16.Position.Y - button16.Margins.Top;
  fline.LineType := TLineType.Top;
  //-----
  ALFmxMakeBufBitmaps(ALVertScrollBox1);
  endupdate;
  ALTabControl1Resize(nil);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin

  {$IF Defined(IOS) or Defined(ANDROID)}
  TMessageManager.DefaultManager.Unsubscribe(TgoExceptionReportMessage, ApplicationExceptionHandler);
  {$ENDIF}

  ALLog('FormDestroy', 'FormDestroy');
end;

{$IF Defined(IOS) or Defined(ANDROID)}
procedure TForm1.ApplicationExceptionHandler(const Sender: TObject; const M: TMessage);
var aReport: IgoExceptionReport;
begin
  aReport := TgoExceptionReportMessage(M).Report;
  allog('ERROR', aReport.Report, TalLogType.error);

  {$IF Defined(IOS)}
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        Halt(1); // << This is the only way i found to crash the app :(
      end);
    end).Start;
  {$ELSE}
  Application.Terminate;
  {$ENDIF}

end;
{$ENDIF}

{$IF Defined(MSWINDOWS) or Defined(_MACOS)}
procedure TForm1.ApplicationExceptionHandler(Sender: TObject; E: Exception);
begin
  Application.Terminate;
end;
{$ENDIF}

procedure TForm1.FormResize(Sender: TObject);
begin
  ALLog('FormResize', 'width: ' + FloatToStr(width) + ' - ' + FloatToStr(height));
  ALVideoPlayerSurface1.Height := (width / 1920) * 1080;

  {$IF Defined(ANDROID)}
  //handle the action bar under lollipop
  if FVKKeyboardOpen then
    AlVertScrollBox1.AniCalculations.ViewportPosition := AlVertScrollBox1.AniCalculations.MaxTarget.Point;
  {$ENDIF}

end;

procedure TForm1.FormVirtualKeyboardHidden(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  ALLog('FormVirtualKeyboardHidden', 'FormVirtualKeyboardHidden');
  FVKKeyboardOpen := False;
  AlVertScrollBox1.margins.Bottom := 0;
  AlVertScrollBox1.AniCalculations.TouchTracking := [ttVertical];
end;

procedure TForm1.FormVirtualKeyboardShown(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  ALLog('FormVirtualKeyboardShown', 'FormVirtualKeyboardShown');
  FVKKeyboardOpen := True;
  AlVertScrollBox1.margins.Bottom := Bounds.height;
  AlVertScrollBox1.VScrollBar.Value := AlVertScrollBox1.VScrollBar.Max;
  AlVertScrollBox1.AniCalculations.TouchTracking := [];
end;

{ TALTextStopWatch }

{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALTextStopWatch.MakeBufBitmap: TTexture;
{$ELSE}
function TALTextStopWatch.MakeBufBitmap: Tbitmap;
{$ENDIF}
var aStopWatch: TstopWatch;
begin
  if (BufBitmap = nil) then begin
    aStopWatch := TstopWatch.StartNew;
    result := inherited MakeBufBitmap;
    aStopWatch.stop;
    bufCreatePaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  end
  else result := inherited MakeBufBitmap;
end;

procedure TALTextStopWatch.Paint;
var aStopWatch: TstopWatch;
    aRemovebufCreatePaintMs: boolean;
begin
  clearbufBitmap;
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aRemovebufCreatePaintMs := (BufBitmap = nil);
  aStopWatch := TstopWatch.StartNew;
  MakeBufBitmap;
  inherited paint;
  aStopWatch.stop;
  bufPaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  if aRemovebufCreatePaintMs then bufPaintMs := bufPaintMs - bufCreatePaintMs;
end;

{ TTextStopWatch }

procedure TTextStopWatch.Paint;
var aStopWatch: TstopWatch;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  PaintMs := aStopWatch.Elapsed.TotalMilliseconds;
end;

{ TRectangleStopWatch }

procedure TRectangleStopWatch.Paint;
var aStopWatch: TstopWatch;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  PaintMs := aStopWatch.Elapsed.TotalMilliseconds;
end;

{ TALRectangleStopWatch }

{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALRectangleStopWatch.MakeBufBitmap: TTexture;
{$ELSE}
function TALRectangleStopWatch.MakeBufBitmap: Tbitmap;
{$ENDIF}
var aStopWatch: TstopWatch;
begin
  if (BufBitmap = nil) then begin
    aStopWatch := TstopWatch.StartNew;
    result := inherited MakeBufBitmap;
    aStopWatch.stop;
    bufCreatePaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  end
  else result := inherited MakeBufBitmap;
end;

procedure TALRectangleStopWatch.Paint;
var aStopWatch: TstopWatch;
    aRemovebufCreatePaintMs: boolean;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aRemovebufCreatePaintMs := (BufBitmap = nil);
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  bufPaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  if aRemovebufCreatePaintMs then bufPaintMs := bufPaintMs - bufCreatePaintMs;
end;

{ TCheckBoxStopWatch }

procedure TCheckBoxStopWatch.PaintChildren;
var aStopWatch: TstopWatch;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aStopWatch := TstopWatch.StartNew;
  inherited PaintChildren;
  aStopWatch.stop;
  PaintMs := aStopWatch.Elapsed.TotalMilliseconds;
end;

{ TALCheckBoxStopWatch }

{$IF DEFINED(IOS) or DEFINED(ANDROID)}
 function TALCheckBoxStopWatch.MakeBufBitmap: TTexture;
{$ELSE}
 function TALCheckBoxStopWatch.MakeBufBitmap: Tbitmap;
{$ENDIF}
var aStopWatch: TstopWatch;
begin
  if (BufBitmap = nil) then begin
    aStopWatch := TstopWatch.StartNew;
    result := inherited MakeBufBitmap;
    aStopWatch.stop;
    bufCreatePaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  end
  else result := inherited MakeBufBitmap;
end;

procedure TALCheckBoxStopWatch.Paint;
var aStopWatch: TstopWatch;
    aRemovebufCreatePaintMs: boolean;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aRemovebufCreatePaintMs := (BufBitmap = nil);
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  bufPaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  if aRemovebufCreatePaintMs then bufPaintMs := bufPaintMs - bufCreatePaintMs;
end;

{ TLineStopWatch }

procedure TLineStopWatch.Paint;
var aStopWatch: TstopWatch;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  PaintMs := aStopWatch.Elapsed.TotalMilliseconds;
end;

{ TALLineStopWatch }

{$IF DEFINED(IOS) or DEFINED(ANDROID)}
 function TALLineStopWatch.MakeBufBitmap: TTexture;
{$ELSE}
 function TALLineStopWatch.MakeBufBitmap: Tbitmap;
{$ENDIF}
var aStopWatch: TstopWatch;
begin
  if (BufBitmap = nil) then begin
    aStopWatch := TstopWatch.StartNew;
    result := inherited MakeBufBitmap;
    aStopWatch.stop;
    bufCreatePaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  end
  else result := inherited MakeBufBitmap;
end;

procedure TALLineStopWatch.Paint;
var aStopWatch: TstopWatch;
    aRemovebufCreatePaintMs: boolean;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aRemovebufCreatePaintMs := (BufBitmap = nil);
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  bufPaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  if aRemovebufCreatePaintMs then bufPaintMs := bufPaintMs - bufCreatePaintMs;
end;

{ TALRangeTrackBarStopWatch }

procedure TALRangeTrackBarStopWatch.PaintChildren;
var aStopWatch: TstopWatch;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aStopWatch := TstopWatch.StartNew;
  inherited PaintChildren;
  aStopWatch.stop;
  PaintMs := aStopWatch.Elapsed.TotalMilliseconds;
end;

{ TALCircleStopWatch }

{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALCircleStopWatch.MakeBufBitmap: TTexture;
{$ELSE}
function TALCircleStopWatch.MakeBufBitmap: Tbitmap;
{$ENDIF}
var aStopWatch: TstopWatch;
begin
  if (BufBitmap = nil) then begin
    aStopWatch := TstopWatch.StartNew;
    result := inherited MakeBufBitmap;
    aStopWatch.stop;
    bufCreatePaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  end
  else result := inherited MakeBufBitmap;
end;

procedure TALCircleStopWatch.Paint;
var aStopWatch: TstopWatch;
    aRemovebufCreatePaintMs: boolean;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aRemovebufCreatePaintMs := (BufBitmap = nil);
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  bufPaintMs := aStopWatch.Elapsed.TotalMilliseconds;
  if aRemovebufCreatePaintMs then bufPaintMs := bufPaintMs - bufCreatePaintMs;
end;

{ TCircleStopWatch }

procedure TCircleStopWatch.Paint;
var aStopWatch: TstopWatch;
begin
  canvas.ClearRect(TrectF.Create(0,0,0,0)); // it's just to flush what is inside the canvas
  aStopWatch := TstopWatch.StartNew;
  inherited paint;
  aStopWatch.stop;
  PaintMs := aStopWatch.Elapsed.TotalMilliseconds;
end;

end.
