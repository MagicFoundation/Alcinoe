unit Main;

{$I Alcinoe.inc}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  system.Diagnostics,
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Objects,
  FMX.Layouts,
  Alcinoe.FMX.BreakText,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.Confetti,
  Alcinoe.FMX.VideoPlayer,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.StdCtrls;

type
  TMainForm = class(TForm)
    MainScrollBox: TALVertScrollBox;
    MainTitle: TALText;
    ButtonRecreateBuffers: TALButton;
    ButtonTestFPS: TALButton;
    ButtonCheckForMemoryLeaks: TALButton;
    ButtonTestMultilineText: TALButton;
    SubTitle: TALText;
    StatusLabel3: TALText;
    StatusLabel1: TALText;
    StatusLabel2: TALText;
    ButtonStop: TALButton;
    ALText61: TALText;
    ALLayout72: TALLayout;
    ALLayout73: TALLayout;
    ALText63: TALText;
    ALLayout74: TALLayout;
    Image1: TImage;
    ALLayout75: TALLayout;
    ALLayout76: TALLayout;
    ALText64: TALText;
    ALLayout77: TALLayout;
    ALImage14: TALImage;
    ALLayout70: TALLayout;
    ALLayout71: TALLayout;
    ALText1: TALText;
    ALLayout78: TALLayout;
    ALImage15: TALImage;
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure FormCreate(Sender: TObject);
    procedure LoadImagePaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure ButtonRecreateBuffersClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonTestFPSClick(Sender: TObject);
    procedure ButtonCheckForMemoryLeaksClick(Sender: TObject);
    procedure TextEllipsisElementClick(Sender: TObject; const Element: TALTextElement);
    procedure TextEllipsisElementMouseEnter(Sender: TObject; const Element: TALTextElement);
    procedure TextEllipsisElementMouseLeave(Sender: TObject; const Element: TALTextElement);
    procedure ButtonTestMultilineTextClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    FScrollBoxDrawables: TArray<TAlDrawable>;
    FTestFPSDrawables: TArray<TAlDrawable>;
    fDebugFpsStarted: Boolean;
    fDebugFpsCount: integer;
    fDebugFpsStopWatch: TstopWatch;
    fDebugAverageFpsCount: integer;
    fDebugAverageFps: double;
    FCurrentTextElements: TDictionary<TObject, TALTextElement>;
    procedure CreateLoadImagePaintBox(
                const aIdx: integer;
                const aTitle: String);
    procedure InitScrollBoxDrawables;
    function InitTestFPSDrawables(const ACount: Integer; const APrevCount: integer): double;
    procedure HideStatusLabels;
    procedure OrderStatusLabels;
    procedure DisableButtons(Const AExcept: TALButton);
    procedure EnableButtons;
  end;

var
  MainForm: TMainForm;

implementation

uses
  {$IF defined(ALMacOS)}
  macapi.Foundation,
  Macapi.CoreGraphics,
  {$ENDIF}
  {$IF defined(IOS)}
  iosApi.Foundation,
  iOSapi.CoreGraphics,
  {$ENDIF}
  {$IF defined(ALSkiaEngine)}
  System.Skia.API,
  FMX.Skia,
  {$ENDIF}
  system.DateUtils,
  system.Math,
  FMX.Types3D,
  FMX.DialogService,
  FMX.Platform,
  Alcinoe.Cipher,
  Alcinoe.FMX.ErrorReporting,
  Alcinoe.Common,
  Alcinoe.FMX.Common,
  Alcinoe.StringUtils;

{$R *.fmx}

{**********************************************}
procedure TMainForm.FormCreate(Sender: TObject);
begin

  TALErrorReporting.Instance;

  HideStatusLabels;

  FCurrentTextElements := TDictionary<TObject, TALTextElement>.Create;

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

  BeginUpdate;
  try
    CreateLoadImagePaintBox(00, 'Load and FitInto (Big image)');
    CreateLoadImagePaintBox(01, 'Load and FitInto (Small image)');
    CreateLoadImagePaintBox(02, 'Load and FitInto and Crop (Big image)');
    CreateLoadImagePaintBox(03, 'Load and FitInto and Crop (Small image)');
    CreateLoadImagePaintBox(04, 'Load and FitInto and Crop to RoundRect (Big image)');
    CreateLoadImagePaintBox(05, 'Load and FitInto and Crop to RoundRect (Small image)');
    CreateLoadImagePaintBox(06, 'Load and FitInto and Crop to Circle (Big image)');
    CreateLoadImagePaintBox(07, 'Load and FitInto and Crop to Circle (Small image)');
    CreateLoadImagePaintBox(08, 'Load and FitInto and Crop and Blur (Big image)');
    CreateLoadImagePaintBox(09, 'Load and FitInto and Crop and Blur (Small image)');
    CreateLoadImagePaintBox(10, 'Load and FitInto and Crop and Blur to Circle (Big image)');
    CreateLoadImagePaintBox(11, 'Load and FitInto and Crop and Blur to Circle (Small image)');
    CreateLoadImagePaintBox(12, 'Load and FitInto and Crop and Mask (Big image)');
    CreateLoadImagePaintBox(13, 'Load and FitInto and Crop and Mask (Small image)');
    CreateLoadImagePaintBox(14, 'Load and FitInto and Crop and Mask and Blur (Big image)');
    CreateLoadImagePaintBox(15, 'Load and FitInto and Crop and Mask and Blur (Small image)');
    CreateLoadImagePaintBox(16, 'Load and PlaceInto (Big image)');
    CreateLoadImagePaintBox(17, 'Load and PlaceInto (Small image)');
    CreateLoadImagePaintBox(18, 'Load and PlaceInto and Blur (Big image)');
    CreateLoadImagePaintBox(19, 'Load and PlaceInto and Blur (Small image)');
    CreateLoadImagePaintBox(20, 'Load and Stretch (Big image)');
    CreateLoadImagePaintBox(21, 'Load and Stretch (Small image)');
    CreateLoadImagePaintBox(22, 'Load and Normalize Orientation');
  finally
    EndUpdate;
  end;

  InitScrollBoxDrawables;
end;

{***********************************************}
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ALFreeAndNil(FCurrentTextElements);
  For var I := low(FScrollBoxDrawables) to high(FScrollBoxDrawables) do
    ALFreeAndNilDrawable(FScrollBoxDrawables[i]);
end;

{******************************************}
procedure TMainForm.CreateLoadImagePaintBox(
            const aIdx: integer;
            const aTitle: String);
begin
  setlength(FScrollBoxDrawables, length(FScrollBoxDrawables) + 1);
  FScrollBoxDrawables[high(FScrollBoxDrawables)] := ALNullDrawable;
  var LLayout1 := TALLayout.Create(MainScrollBox);
  LLayout1.parent := MainScrollBox;
  LLayout1.Align := TALignLayout.Top;
  LLayout1.Position.Y := aIdx;
  LLayout1.Margins.Top := 24;
  LLayout1.Size.Height := 150;
  var LPaintBox := TPaintBox.Create(LLayout1);
  LPaintBox.parent := LLayout1;
  LPaintBox.Align := TALignLayout.Left;
  LPaintBox.Margins.Left := 12;
  LPaintBox.Margins.Right := 12;
  LPaintBox.Size.Width := 150;
  LPaintBox.OnPaint := LoadImagePaintBoxPaint;
  LPaintBox.Tag := aIdx;
  var LLayout2 := TALLayout.create(LLayout1);
  LLayout2.parent := LLayout1;
  LPaintBox.TagObject := LLayout2;
  LLayout2.Align := TALignLayout.Client;
  LLayout2.Margins.Right := 12;
  var LText1 := TALText.create(LLayout2);
  LText1.parent := LLayout2;
  LText1.Align := TALignLayout.Top;
  Ltext1.Margins.Bottom := 8;
  LText1.AutoSize := True;
  LText1.TextSettings.Font.Family := ALConvertFontFamily('sans-serif');
  LText1.TextSettings.IsHtml := true;
  LText1.TextSettings.Font.Size := 1;
  LText1.TextSettings.Font.Weight := TfontWeight.Bold;
  LText1.TextSettings.HorzAlign := TALTextHorzAlign.Leading;
  LText1.Text := '<span font-weight="700" font-size="20px">'+aTitle+'</span><br>'+
                 '<span font-size="10px"> </span><br>'+
                 '<span font-size="17"></span>';
  LText1.Position.Y := 0;
end;

{********************************}
procedure TMainForm.EnableButtons;
begin
  ButtonRecreateBuffers.Enabled := True;
  ButtonTestFPS.Enabled := True;
  ButtonTestMultilineText.Enabled := True;
  ButtonCheckForMemoryLeaks.Enabled := True;
  ButtonStop.Enabled := True;
end;

{***********************************************************}
procedure TMainForm.DisableButtons(const AExcept: TALButton);
begin
  if AExcept <> ButtonStop then
    ButtonStop.Enabled := False;
  if AExcept <> ButtonRecreateBuffers then
    ButtonRecreateBuffers.Enabled := False;
  if AExcept <> ButtonTestFPS then
    ButtonTestFPS.Enabled := False;
  if AExcept <> ButtonTestMultilineText then
    ButtonTestMultilineText.Enabled := False;
  if AExcept <> ButtonCheckForMemoryLeaks then
    ButtonCheckForMemoryLeaks.Enabled := False;
end;

{***********************************************************************************}
procedure TMainForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin

  if (ButtonTestFPS.Tag <> 2) then exit;

  For var I := low(FTestFPSDrawables) to high(FTestFPSDrawables) do begin

    var LDrawable := FTestFPSDrawables[i];
    if not ALIsDrawableNull(LDrawable) then begin
      var LDestRect := Trectf.Create(0,0,ALGetDrawableWidth(LDrawable) / ALGetScreenScale, ALGetDrawableHeight(LDrawable) / ALGetScreenScale);
      var LDeltaX := ClientWidth - LDestRect.Width;
      var LDeltaY := ClientHeight - ButtonStop.Position.y - ButtonStop.Height - 10 - StatusLabel1.Height - StatusLabel2.Height  - 10 - LDestRect.Height;
      LDestRect.Location := TpointF.Create(
                              Random(round(LDeltaX)),
                              Random(round(LDeltaY)) + ButtonStop.Position.y + ButtonStop.Height + 10);
      ALDrawDrawable(
        Canvas, // const ACanvas: Tcanvas;
        LDrawable, // const ADrawable: TALDrawable;
        LDestRect.TopLeft, // const ATopLeft: TpointF;
        1); // const AOpacity: Single);
    end;

  end;

  // In order to compute the frame rate, we need
  // to constantly refresh the form
  TThread.ForceQueue(nil,
    procedure
    begin
      invalidate;
    end);

  if not fDebugFpsStarted then begin
    fDebugFpsStarted := true;
    fDebugFpsCount := 0;
    fDebugFpsStopWatch := TstopWatch.StartNew;
  end
  else begin
    inc(fDebugFpsCount);
    if fDebugFpsCount >= 300 then begin
      fDebugFpsStopWatch.stop;
      fDebugAverageFps := ((fDebugAverageFps * fDebugAverageFpsCount) + ((fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000)) / (fDebugAverageFpsCount + 1);
      inc(fDebugAverageFpsCount);
      ALLog(
        'FramePaint.fps',
        ALFormatFloatW('0.##', (fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000, ALDefaultFormatSettingsW) + ' fps' + ' | ' +
        'average: ' + ALFormatFloatW('0.##', fDebugAverageFps, ALDefaultFormatSettingsW) + ' fps',
        TalLogType.verbose);
      StatusLabel1.Text := ALFormatFloatW('0.##', (fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000, ALDefaultFormatSettingsW) + ' fps' +
                           ' (average: ' + ALFormatFloatW('0.##', fDebugAverageFps, ALDefaultFormatSettingsW) + ' fps)';
      StatusLabel1.Visible := True;
      OrderStatusLabels;
      fDebugFpsCount := 0;
      fDebugFpsStopWatch := TstopWatch.StartNew;
    end;
  end;

end;

{***********************************}
procedure TMainForm.HideStatusLabels;
begin
  StatusLabel1.Visible := False;
  StatusLabel1.Text := '';
  StatusLabel2.Visible := False;
  StatusLabel2.Text := '';
  StatusLabel3.Visible := False;
  StatusLabel3.Text := '';
end;

{************************************}
procedure TMainForm.OrderStatusLabels;
begin
  StatusLabel1.Position.Y := Height - StatusLabel1.Height;
  StatusLabel2.Position.Y := StatusLabel1.Position.Y - StatusLabel2.Height;
  StatusLabel3.Position.Y := StatusLabel2.Position.Y - StatusLabel3.Height;
end;

{*****************************************}
procedure TMainForm.InitScrollBoxDrawables;
begin
  HideStatusLabels;
  For var I := low(FScrollBoxDrawables) to high(FScrollBoxDrawables) do
    ALFreeAndNilDrawable(FScrollBoxDrawables[i]);

  var LStopWatch := TStopWatch.StartNew;
  For var I := low(FScrollBoxDrawables) to high(FScrollBoxDrawables) do begin
    var W := 150 * ALGetScreenScale;
    var H := 150 * ALGetScreenScale;
    case i of
      00: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoToDrawable('family', W, H);
      01: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoToDrawable('familylittle', W, H);
      02: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoAndCropToDrawable('family', W, H, -65, -48);
      03: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoAndCropToDrawable('familylittle', W, H, -65, -48);
      04: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoAndCropToRoundRectDrawable('family', W, H, 25*ALGetScreenScale, 25*ALGetScreenScale, -65, -48);
      05: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoAndCropToRoundRectDrawable('familylittle', W, H, 25*ALGetScreenScale, 25*ALGetScreenScale, -65, -48);
      06: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoAndCropToCircleDrawable('family', W, H, -65, -48);
      07: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoAndCropToCircleDrawable('familylittle', W, H, -65, -48);
      08: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndBlurToDrawable('family', W, H, 10*ALGetScreenScale, -65, -48);
      09: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndBlurToDrawable('familylittle', W, H, 10*ALGetScreenScale, -65, -48);
      10: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleDrawable('family', W, H, 10*ALGetScreenScale, -65, -48);
      11: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleDrawable('familylittle', W, H, 10*ALGetScreenScale, -65, -48);
      12, 13, 14, 15: begin
        {$IF defined(ALSkiaEngine)}
        var LMask := ALLoadFromResourceAndFitIntoToSKImage('mask', W, H);
        {$ELSEIF defined(ANDROID)}
        var LMask := ALLoadFromResourceAndFitIntoToJbitmap('mask', W, H);
        {$ELSEIF defined(ALAppleOS)}
        var LMask := ALLoadFromResourceAndFitIntoToCGImageRef('mask', W, H);
        {$ELSE}
        var LMask := ALLoadFromResourceAndFitIntoToBitmap('mask', W, H);
        {$ENDIF}
        try
          case I of
            12: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndMaskToDrawable('family', LMask, -65, -48);
            13: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndMaskToDrawable('familylittle', LMask, -65, -48);
            14: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToDrawable('family', LMask, 10*ALGetScreenScale, -65, -48);
            15: FScrollBoxDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToDrawable('familylittle', LMask, 10*ALGetScreenScale, -65, -48);
            else FScrollBoxDrawables[i] := ALNullDrawable;
          end;
        finally
          {$IF defined(ALSkiaEngine)}
          sk4d_refcnt_unref(LMask);
          {$ELSEIF defined(ANDROID)}
          LMask.recycle;
          LMask := nil;
          {$ELSEIF defined(ALAppleOS)}
          CGImageRelease(LMask);
          {$ELSE}
          ALFreeAndNil(LMask);
          {$ENDIF}
        end;
      end;
      16: FScrollBoxDrawables[i] := ALLoadFromResourceAndPlaceIntoToDrawable('family', W, H);
      17: FScrollBoxDrawables[i] := ALLoadFromResourceAndPlaceIntoToDrawable('familylittle', W, H);
      18: FScrollBoxDrawables[i] := ALLoadFromResourceAndPlaceIntoAndBlurToDrawable('family', W, H, 10*ALGetScreenScale);
      19: FScrollBoxDrawables[i] := ALLoadFromResourceAndPlaceIntoAndBlurToDrawable('familylittle', W, H, 10*ALGetScreenScale);
      20: FScrollBoxDrawables[i] := ALLoadFromResourceAndStretchToDrawable('family', W, H);
      21: FScrollBoxDrawables[i] := ALLoadFromResourceAndStretchToDrawable('familylittle', W, H);
      22: FScrollBoxDrawables[i] := ALLoadFromResourceAndNormalizeOrientationToDrawable('familyrotate180', TalExifOrientationInfo.ROTATE_180);
      else FScrollBoxDrawables[i] := ALNullDrawable;
    end;
  end;
  LStopWatch.Stop;
  StatusLabel1.Text := 'All images created in: ' + ALFormatFloatW('0.00', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW) + ' ms';
  StatusLabel1.Visible := True;
  OrderStatusLabels;
end;

{************************************************************************************************}
function TMainForm.InitTestFPSDrawables(const ACount: Integer; const APrevCount: integer): double;
begin
  For var I := low(FTestFPSDrawables) to high(FTestFPSDrawables) do
    ALFreeAndNilDrawable(FTestFPSDrawables[i]);

  setlength(FTestFPSDrawables, ACount);

  RandSeed := 0;
  var LStopWatch := TStopWatch.StartNew;
  For var I := low(FTestFPSDrawables) to high(FTestFPSDrawables) do begin
    {$IF defined(ios)}
    //If you write a loop that creates many temporary objects.
    //You may use an autorelease pool block inside the loop to
    //dispose of those objects before the next iteration. Using an
    //autorelease pool block in the loop helps to reduce the maximum
    //memory footprint of the application.
    var LAutoReleasePool := TNSAutoreleasePool.Create;
    try
    {$ENDIF}
      var W := 10 + random(150) * ALGetScreenScale;
      var H := 10 + random(150) * ALGetScreenScale;
      case i mod 22 of
        00: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoToDrawable('family', W, H);
        01: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoToDrawable('familylittle', W, H);
        02: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoAndCropToDrawable('family', W, H, -65, -48);
        03: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoAndCropToDrawable('familylittle', W, H, -65, -48);
        04: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoAndCropToRoundRectDrawable('family', W, H, 25*ALGetScreenScale, 25*ALGetScreenScale, -65, -48);
        05: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoAndCropToRoundRectDrawable('familylittle', W, H, 25*ALGetScreenScale, 25*ALGetScreenScale, -65, -48);
        06: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoAndCropToCircleDrawable('family', W, H, -65, -48);
        07: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoAndCropToCircleDrawable('familylittle', W, H, -65, -48);
        08: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndBlurToDrawable('family', W, H, 10*ALGetScreenScale, -65, -48);
        09: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndBlurToDrawable('familylittle', W, H, 10*ALGetScreenScale, -65, -48);
        10: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleDrawable('family', W, H, 10*ALGetScreenScale, -65, -48);
        11: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndBlurToCircleDrawable('familylittle', W, H, 10*ALGetScreenScale, -65, -48);
        12, 13, 14, 15: begin
              {$IF defined(ALSkiaEngine)}
              var LMask := ALLoadFromResourceAndFitIntoToSKImage('mask', W, H);
              {$ELSEIF defined(ANDROID)}
              var LMask := ALLoadFromResourceAndFitIntoToJbitmap('mask', W, H);
              {$ELSEIF defined(ALAppleOS)}
              var LMask := ALLoadFromResourceAndFitIntoToCGImageRef('mask', W, H);
              {$ELSE}
              var LMask := ALLoadFromResourceAndFitIntoToBitmap('mask', W, H);
              {$ENDIF}
              try
                case i of
                  12: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndMaskToDrawable('family', LMask, -65, -48);
                  13: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndMaskToDrawable('familylittle', LMask, -65, -48);
                  14: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToDrawable('family', LMask, 10*ALGetScreenScale, -65, -48);
                  15: FTestFPSDrawables[i] := ALLoadFromResourceAndFitIntoAndCropAndMaskAndBlurToDrawable('familylittle', LMask, 10*ALGetScreenScale, -65, -48);
                  else FTestFPSDrawables[i] := ALNullDrawable;
                end;
              finally
                {$IF defined(ALSkiaEngine)}
                sk4d_refcnt_unref(LMask);
                {$ELSEIF defined(ANDROID)}
                LMask.recycle;
                LMask := nil;
                {$ELSEIF defined(ALAppleOS)}
                CGImageRelease(LMask);
                {$ELSE}
                ALFreeAndNil(LMask);
                {$ENDIF}
              end;
            end;
        16: FTestFPSDrawables[i] := ALLoadFromResourceAndPlaceIntoToDrawable('family', W, H);
        17: FTestFPSDrawables[i] := ALLoadFromResourceAndPlaceIntoToDrawable('familylittle', W, H);
        18: FTestFPSDrawables[i] := ALLoadFromResourceAndPlaceIntoAndBlurToDrawable('family', W, H, 10*ALGetScreenScale);
        19: FTestFPSDrawables[i] := ALLoadFromResourceAndPlaceIntoAndBlurToDrawable('familylittle', W, H, 10*ALGetScreenScale);
        20: FTestFPSDrawables[i] := ALLoadFromResourceAndStretchToDrawable('family', W, H);
        21: FTestFPSDrawables[i] := ALLoadFromResourceAndStretchToDrawable('familylittle', W, H);
        22: FTestFPSDrawables[i] := ALLoadFromResourceAndNormalizeOrientationToDrawable('familyrotate180', TalExifOrientationInfo.ROTATE_180);
        else FTestFPSDrawables[i] := ALNullDrawable;
      end;
    {$IF defined(ios)}
    finally
      LAutoReleasePool.release;
    end;
    {$ENDIF}
    if (I mod 100 = 0) then begin
      var J := I;
      TThread.Queue(nil,
        procedure
        begin
          StatusLabel1.Text := ALIntToStrW(APrevCount + J)+' images created';
          StatusLabel1.Visible := True;
          OrderStatusLabels;
        end);
    end;
  end;
  LStopWatch.Stop;
  Result := LStopWatch.Elapsed.TotalMilliseconds;

end;


{******************************************************************}
procedure TMainForm.ButtonCheckForMemoryLeaksClick(Sender: TObject);
begin
  IF ButtonCheckForMemoryLeaks.Tag = 1 then begin
    EnableButtons;
    ButtonCheckForMemoryLeaks.Tag := 0;
    ButtonCheckForMemoryLeaks.Text := 'Check for Memory Leaks';
  end
  Else begin
    TDialogService.ShowMessage('''
      We will now initiate a loop to create a large number of images. It is
      necessary to keep the application running for a sufficient duration to
      observe if there's an increase in memory usage or if the application
      eventually crashes due to the unavailability of memory.
      ''');
    ButtonCheckForMemoryLeaks.Tag := 1;
    ButtonCheckForMemoryLeaks.Text := 'Stop Checking for Memory Leaks';
    DisableButtons(ButtonCheckForMemoryLeaks);
    HideStatusLabels;
    TALGraphicThreadPool.Instance.ExecuteProc(
      procedure(var AExtData: Tobject)
      Begin
        var I: integer := 0;
        while ButtonCheckForMemoryLeaks.Tag = 1 do begin
          InitTestFPSDrawables(100, I);
          inc(I, 100);
        end;
      End);
  end;
end;

{**************************************************************}
procedure TMainForm.ButtonRecreateBuffersClick(Sender: TObject);
begin
  InitScrollBoxDrawables;
end;

{***************************************************}
procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  ButtonTestFPSClick(Sender);
end;

{****************************************************************}
procedure TMainForm.ButtonTestMultilineTextClick(Sender: TObject);
begin
  IF ButtonTestMultilineText.Tag = 1 then exit
  Else begin
    ButtonTestMultilineText.Tag := 1;
    ButtonTestMultilineText.Text := 'Creating images... Please wait.';
    HideStatusLabels;
    //--
    TDialogService.MessageDialog(
      'We will now generate 1000 images containing multiline text and measure the time required to complete this task.',
      TMsgDlgType.mtInformation,
      [TMsgDlgBtn.mbCancel, TMsgDlgBtn.mbOK],
      TMsgDlgBtn.mbOK,
      0,
      procedure(const AResult: TModalResult)
      begin
        if AResult <> mrOk then begin
          ButtonTestMultilineText.Tag := 0;
          ButtonTestMultilineText.Text := 'Test Multiline Text';
          exit;
        end;
        DisableButtons(nil);
        TALGraphicThreadPool.Instance.ExecuteProc(
          procedure(var AExtData: Tobject)
          Begin
            RandSeed := 0;
            var LRawTextStopWatch := TStopWatch.Create;
            For var i := 1 to 500 do begin
              {$IF defined(AlAppleOS)}
              //If you write a loop that creates many temporary objects.
              //You may use an autorelease pool block inside the loop to
              //dispose of those objects before the next iteration. Using an
              //autorelease pool block in the loop helps to reduce the maximum
              //memory footprint of the application.
              var LAutoReleasePool := TNSAutoreleasePool.Create;
              try
              {$ENDIF}
                var LOptions := TALMultiLineTextOptions.Create;
                Try
                  LOptions.Scale := 1;
                  //--
                  LOptions.FontFamily := ALConvertFontFamily('sans-serif');
                  LOptions.FontSize := 10 + Random(30);
                  LOptions.FontWeight := TfontWeight(Random(11)); // Thin, UltraLight, Light, SemiLight, Regular, Medium, Semibold, Bold, UltraBold, Black, UltraBlack
                  LOptions.FontSlant := TFontSlant(Random(2)); // Regular, Italic
                  //LOptions.FontStretch := TFontStretch(Random(9));
                  LOptions.FontColor := Random(Maxint);
                  //--
                  LOptions.DecorationKinds := [];
                  if random(5) = 0 then LOptions.DecorationKinds := LOptions.DecorationKinds + [TALTextDecorationKind.Underline];
                  if random(10) = 0 then LOptions.DecorationKinds := LOptions.DecorationKinds + [TALTextDecorationKind.LineThrough];
                  //if random(20) = 0 then LOptions.DecorationKinds := LOptions.DecorationKinds + [TALTextDecorationKind.Overline];
                  //LOptions.DecorationStyle := TALTextDecorationStyle(Random(5)); // Solid, Double, Dotted, Dashed, Wavy
                  //LOptions.DecorationThicknessMultiplier := Random(2) + random;
                  LOptions.DecorationColor := Random(Maxint);
                  //--
                  if Random(5) = 0 then LOptions.EllipsisText := '… more[+]'
                  else LOptions.EllipsisText := '…';
                  If random(5) = 0 then begin
                    LOptions.EllipsisFontFamily := ALConvertFontFamily('sans-serif');
                    LOptions.EllipsisFontSize := 10 + Random(30);
                    LOptions.EllipsisFontWeight := TfontWeight(Random(11)); // Thin, UltraLight, Light, SemiLight, Regular, Medium, Semibold, Bold, UltraBold, Black, UltraBlack
                    LOptions.EllipsisFontSlant := TFontSlant(Random(2)); // Regular, Italic
                    //LOptions.EllipsisFontStretch := TFontStretch(Random(9));
                    LOptions.EllipsisFontColor := Random(Maxint);
                    //--
                    LOptions.EllipsisDecorationKinds := [];
                    if random(5) = 0 then LOptions.EllipsisDecorationKinds := LOptions.EllipsisDecorationKinds + [TALTextDecorationKind.Underline];
                    //if random(10) = 0 then LOptions.EllipsisDecorationKinds := LOptions.EllipsisDecorationKinds + [TALTextDecorationKind.LineThrough];
                    //if random(20) = 0 then LOptions.EllipsisDecorationKinds := LOptions.EllipsisDecorationKinds + [TALTextDecorationKind.Overline];
                    //LOptions.EllipsisDecorationStyle := TALTextDecorationStyle(Random(5)); // Solid, Double, Dotted, Dashed, Wavy
                    //LOptions.EllipsisDecorationThicknessMultiplier := Random(2) + random;
                    LOptions.EllipsisDecorationColor := Random(Maxint);
                  end;
                  //--
                  case random(5) of
                    0: begin
                      LOptions.AutoSize := True;
                      LOptions.AutoSizeX := True;
                      LOptions.AutoSizeY := True;
                    end;
                    1: begin
                      LOptions.AutoSize := False;
                      LOptions.AutoSizeX := True;
                      LOptions.AutoSizeY := False;
                    end;
                    2: begin
                      LOptions.AutoSize := False;
                      LOptions.AutoSizeX := False;
                      LOptions.AutoSizeY := True;
                    end
                    else begin
                      LOptions.AutoSize := False;
                      LOptions.AutoSizeX := False;
                      LOptions.AutoSizeY := False;
                    end;
                  end;
                  //--
                  LOptions.MaxLines := random(200);
                  IF random(5) = 0 then
                    LOptions.LineHeightMultiplier := 1 + random;
                  IF random(5) = 0 then
                    LOptions.LetterSpacing := random(3);
                  //LOptions.Trimming := TALTextTrimming.Word;
                  //LOptions.FailIfTextBroken: boolean; // default = false
                  //--
                  //if random(10) = 0 then LOptions.Direction := TALTextDirection.RightToLeft
                  //else LOptions.Direction := TALTextDirection.LeftToRight;
                  //--
                  case random(10) of
                    0: LOptions.HTextAlign := TALTextHorzAlign.Center;
                    1: LOptions.HTextAlign := TALTextHorzAlign.Trailing
                    else LOptions.HTextAlign := TALTextHorzAlign.Leading;
                  end;
                  //--
                  case random(10) of
                    0: LOptions.VTextAlign := TALTextVertAlign.Center;
                    1: LOptions.VTextAlign := TALTextVertAlign.Trailing
                    else LOptions.VTextAlign := TALTextVertAlign.Leading;
                  end;
                  //--
                  //LOptions.FillColor: TAlphaColor; // default = TAlphaColors.null - not used if Fill is provided
                  //LOptions.StrokeColor: TalphaColor; // default = TAlphaColors.null - not used if Stroke is provided
                  //LOptions.StrokeThickness: Single; // default = 1 - not used if Stroke is provided
                  //--
                  //LOptions.Fill.assign(Fill);
                  //LOptions.Stroke.assign(Stroke);
                  //--
                  //LOptions.Sides := Sides;
                  //LOptions.XRadius := XRadius;
                  //LOptions.YRadius := YRadius;
                  //LOptions.Corners := Corners;
                  LOptions.Padding := TrectF.Create(Random(10), Random(10), Random(10), Random(10));
                  //--
                  LOptions.TextIsHtml := false;

                  //build fBufBitmap
                  var LRect := TrectF.Create(0,0, random(400), random(400));
                  var LText := ALRandomStrW(Random(2048),[' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']);
                  LRawTextStopWatch.Start;
                  var LDrawable := ALCreateMultiLineTextDrawable(
                                    LText,
                                    LRect,
                                    LOptions);
                  LRawTextStopWatch.Stop;
                  ALFreeAndNilDrawable(LDrawable);

                  if I mod 50 = 0 then begin
                    TThread.Synchronize(nil,
                    procedure
                    begin
                      StatusLabel3.Text := ALInttostrW(i) + ' images created in: ' + ALFormatFloatW('0.00', LRawTextStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW) + ' ms';
                      StatusLabel3.Visible := True;
                      OrderStatusLabels;
                      Invalidate;
                    end);
                  end;
                Finally
                  ALFreeAndNil(LOptions);
                End;
              {$IF defined(AlAppleOS)}
              finally
                LAutoReleasePool.release;
              end;
              {$ENDIF}
            end;

            var LHtmlTextStopWatch := TStopWatch.Create;
            For var i := 501 to 1000 do begin
              {$IF defined(AlAppleOS)}
              //If you write a loop that creates many temporary objects.
              //You may use an autorelease pool block inside the loop to
              //dispose of those objects before the next iteration. Using an
              //autorelease pool block in the loop helps to reduce the maximum
              //memory footprint of the application.
              var LAutoReleasePool := TNSAutoreleasePool.Create;
              try
              {$ENDIF}
                var LOptions := TALMultiLineTextOptions.Create;
                Try

                  LOptions.Scale := 1;
                  //--
                  LOptions.FontFamily := ALConvertFontFamily('sans-serif');
                  LOptions.FontSize := 10 + Random(30);
                  LOptions.FontWeight := TfontWeight(Random(11)); // Thin, UltraLight, Light, SemiLight, Regular, Medium, Semibold, Bold, UltraBold, Black, UltraBlack
                  LOptions.FontSlant := TFontSlant(Random(2)); // Regular, Italic
                  //LOptions.FontStretch := TFontStretch(Random(9));
                  LOptions.FontColor := Random(Maxint);
                  //--
                  LOptions.DecorationKinds := [];
                  if random(5) = 0 then LOptions.DecorationKinds := LOptions.DecorationKinds + [TALTextDecorationKind.Underline];
                  if random(10) = 0 then LOptions.DecorationKinds := LOptions.DecorationKinds + [TALTextDecorationKind.LineThrough];
                  //if random(20) = 0 then LOptions.DecorationKinds := LOptions.DecorationKinds + [TALTextDecorationKind.Overline];
                  //LOptions.DecorationStyle := TALTextDecorationStyle(Random(5)); // Solid, Double, Dotted, Dashed, Wavy
                  //LOptions.DecorationThicknessMultiplier := Random(2) + random;
                  LOptions.DecorationColor := Random(Maxint);
                  //--
                  if Random(5) = 0 then LOptions.EllipsisText := '… more[+]'
                  else LOptions.EllipsisText := '…';
                  If random(5) = 0 then begin
                    LOptions.EllipsisFontFamily := ALConvertFontFamily('sans-serif');
                    LOptions.EllipsisFontSize := 10 + Random(30);
                    LOptions.EllipsisFontWeight := TfontWeight(Random(11)); // Thin, UltraLight, Light, SemiLight, Regular, Medium, Semibold, Bold, UltraBold, Black, UltraBlack
                    LOptions.EllipsisFontSlant := TFontSlant(Random(2)); // Regular, Italic
                    //LOptions.EllipsisFontStretch := TFontStretch(Random(9));
                    LOptions.EllipsisFontColor := Random(Maxint);
                    //--
                    LOptions.EllipsisDecorationKinds := [];
                    if random(5) = 0 then LOptions.EllipsisDecorationKinds := LOptions.EllipsisDecorationKinds + [TALTextDecorationKind.Underline];
                    //if random(10) = 0 then LOptions.EllipsisDecorationKinds := LOptions.EllipsisDecorationKinds + [TALTextDecorationKind.LineThrough];
                    //if random(20) = 0 then LOptions.EllipsisDecorationKinds := LOptions.EllipsisDecorationKinds + [TALTextDecorationKind.Overline];
                    //LOptions.EllipsisDecorationStyle := TALTextDecorationStyle(Random(5)); // Solid, Double, Dotted, Dashed, Wavy
                    //LOptions.EllipsisDecorationThicknessMultiplier := Random(2) + random;
                    LOptions.EllipsisDecorationColor := Random(Maxint);
                  end;
                  //--
                  case random(5) of
                    0: begin
                      LOptions.AutoSize := True;
                      LOptions.AutoSizeX := True;
                      LOptions.AutoSizeY := True;
                    end;
                    1: begin
                      LOptions.AutoSize := False;
                      LOptions.AutoSizeX := True;
                      LOptions.AutoSizeY := False;
                    end;
                    2: begin
                      LOptions.AutoSize := False;
                      LOptions.AutoSizeX := False;
                      LOptions.AutoSizeY := True;
                    end
                    else begin
                      LOptions.AutoSize := False;
                      LOptions.AutoSizeX := False;
                      LOptions.AutoSizeY := False;
                    end;
                  end;
                  //--
                  LOptions.MaxLines := random(200);
                  IF random(5) = 0 then
                    LOptions.LineHeightMultiplier := 1 + random;
                  IF random(5) = 0 then
                    LOptions.LetterSpacing := random(3);
                  //LOptions.Trimming := TALTextTrimming.Word;
                  //LOptions.FailIfTextBroken: boolean; // default = false
                  //--
                  //if random(10) = 0 then LOptions.Direction := TALTextDirection.RightToLeft
                  //else LOptions.Direction := TALTextDirection.LeftToRight;
                  //--
                  case random(10) of
                    0: LOptions.HTextAlign := TALTextHorzAlign.Center;
                    1: LOptions.HTextAlign := TALTextHorzAlign.Trailing
                    else LOptions.HTextAlign := TALTextHorzAlign.Leading;
                  end;
                  //--
                  case random(10) of
                    0: LOptions.VTextAlign := TALTextVertAlign.Center;
                    1: LOptions.VTextAlign := TALTextVertAlign.Trailing
                    else LOptions.VTextAlign := TALTextVertAlign.Leading;
                  end;
                  //--
                  //LOptions.FillColor: TAlphaColor; // default = TAlphaColors.null - not used if Fill is provided
                  //LOptions.StrokeColor: TalphaColor; // default = TAlphaColors.null - not used if Stroke is provided
                  //LOptions.StrokeThickness: Single; // default = 1 - not used if Stroke is provided
                  //--
                  //LOptions.Fill.assign(Fill);
                  //LOptions.Stroke.assign(Stroke);
                  //--
                  //LOptions.Sides := Sides;
                  //LOptions.XRadius := XRadius;
                  //LOptions.YRadius := YRadius;
                  //LOptions.Corners := Corners;
                  LOptions.Padding := TrectF.Create(Random(10), Random(10), Random(10), Random(10));
                  //--
                  LOptions.TextIsHtml := True;

                  //build fBufBitmap
                  var LRect := TrectF.Create(0,0, random(800), random(800));
                  var LText :=
                        '<span id="font color" font-size="34px" color="#0000FF">font colo' +
                        'r</span>'#13#10'<span id="font-size" font-size="24px" color="#556B2F">' +
                        'font-size</span>'#13#10'<span id="Bold" font-size="34px"><b>Bold</b></' +
                        'span>'#13#10'<span id="letter-spacing" font-size="24px" letter-spacing' +
                        '="3px">letter-spacing</span>'#13#10'<span id="text-decoration-thicknes' +
                        's" font-size="34px" text-decoration-thickness="2" text-decoratio' +
                        'n-line="underline">text-decoration-thickness</span>'#13#10'<span id="f' +
                        'ont-style" font-size="24px" font-style="italic" color="#7c8b38">' +
                        'font-style</span>'#13#10'<span id="Emoji" font-size="34px" font-weight' +
                        '="bold">Emoji '#55357#56842#55356#57146#55356#57096#55357#56364#55357#56495'</span>'#13#10'<span id="color" font-size="20p' +
                        'x" color="#0000FF">span color</span>'#13#10'<span id="line-height" fon' +
                        't-size="34px" line-height="1.6">line-height</span>'#13#10'<span id="te' +
                        'xt-decoration-line" font-size="26px" text-decoration-line="line-' +
                        'through">text-decoration-line</span>'#13#10'<span id="font size" font-' +
                        'size="18px" color="#8d6e9a">font size</span>'#13#10'<span id="font-fam' +
                        'ily" font-size="48px" font-family="GoodDog Plain" color="#8A2BE2' +
                        '">font-family</span>'#13#10'<span id="background-color" font-size="24p' +
                        'x" background-color="#FFFF00">background-color</span>'#13#10'<span id=' +
                        '"Italic" font-size="34px"><i>Italic</i></span>'#13#10'<span id="font-s' +
                        'tretch" font-size="25px" font-stretch="UltraCondensed">font-stre' +
                        'tch</span>'#13#10'<span id="Custom Image" font-size="30px" font-weight' +
                        '="900" color="#800000">Custom Image</span>'#13#10'<span ' +
                        'id="text-decoration-style" font-size="30px" text-decoration-styl' +
                        'e="dotted" text-decoration-color="#FF0000" text-decoration-line=' +
                        '"underline">text-decoration-style</span>'#13#10'<span id="font-weight"' +
                        ' font-size="54px" font-weight="bold" color="#b898a5">font-weight' +
                        '</span>'#13#10'<span id="text-decoration-color" font-size="22px" text-' +
                        'decoration-color="#FF0000" text-decoration-line="underline">text' +
                        '-decoration-color</span>'#13#10'<span id="font face" font-size="48px" ' +
                        'font-family="GoodDog Plain">font fallbacks '#29233#21644#31119'</span>';
                  LHtmlTextStopWatch.Start;
                  var LDrawable := ALCreateMultiLineTextDrawable(
                                    LText,
                                    LRect,
                                    LOptions);
                  LHtmlTextStopWatch.Stop;
                  ALFreeAndNilDrawable(LDrawable);

                  if I mod 50 = 0 then begin
                    TThread.Synchronize(nil,
                    procedure
                    begin
                      StatusLabel3.Text := ALInttostrW(i) + ' images created in: ' + ALFormatFloatW('0.00', LRawTextStopWatch.Elapsed.TotalMilliseconds + LHtmlTextStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW) + ' ms';
                      StatusLabel3.Visible := True;
                      OrderStatusLabels;
                      Invalidate;
                    end);
                  end;

                Finally
                  ALFreeAndNil(LOptions);
                End;
              {$IF defined(AlAppleOS)}
              finally
                LAutoReleasePool.release;
              end;
              {$ENDIF}
            end;

            TThread.Synchronize(nil,
            procedure
            begin
              StatusLabel1.Text := '500 text images created in: ' + ALFormatFloatW('0', LRawTextStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW) + ' ms ('+ALFormatFloatW('0', LRawTextStopWatch.Elapsed.TotalMilliseconds / 500, ALDefaultFormatSettingsW) +' ms/image)';
              StatusLabel1.Visible := True;
              StatusLabel2.Text := '500 Html images created in: ' + ALFormatFloatW('0', LHtmlTextStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW) + ' ms ('+ALFormatFloatW('0', LHtmlTextStopWatch.Elapsed.TotalMilliseconds / 500, ALDefaultFormatSettingsW) +' ms/image)';
              StatusLabel2.Visible := True;
              OrderStatusLabels;
              ButtonTestMultilineText.Tag := 0;
              ButtonTestMultilineText.Text := 'Test Multiline Text';
              EnableButtons;
              Invalidate;
            end);

          End);
      end);
  end;
end;

{******************************************************}
procedure TMainForm.ButtonTestFPSClick(Sender: TObject);
begin
  IF ButtonTestFPS.Tag = 1 then exit
  Else IF ButtonTestFPS.Tag = 2 then begin
    ButtonTestFPS.Tag := 0;
    ButtonTestFPS.Text := 'Test FPS';
    MainScrollBox.Visible := True;
    ButtonStop.Visible := False;
    EnableButtons;
  end
  Else begin
    ButtonTestFPS.Tag := 1;
    ButtonTestFPS.Text := 'Creating images... Please wait.';
    HideStatusLabels;
    //--
    TDialogService.InputQuery(
      'Number of images to create',
      ['Number'],
      ['500'],
      procedure(const AResult: TModalResult; const AValue: array of string)
      begin
        if AResult <> mrOk then begin
          ButtonTestFPS.Tag := 0;
          ButtonTestFPS.Text := 'Test FPS';
          exit;
        end;
        DisableButtons(ButtonStop);
        var Lcount := ALStrToInt(AValue[0]);
        TALGraphicThreadPool.Instance.ExecuteProc(
          procedure(var AExtData: Tobject)
          Begin
            var LTotalMilliseconds := InitTestFPSDrawables(Lcount, 0);
            TThread.Synchronize(nil,
            procedure
            begin
              MainScrollBox.Visible := False;
              ButtonStop.Visible := True;
              StatusLabel2.Text := ALIntToStrW(Lcount)+' images created in: ' + ALFormatFloatW('0.00', LTotalMilliseconds, ALDefaultFormatSettingsW) + ' ms';
              StatusLabel2.Visible := True;
              OrderStatusLabels;
              ButtonTestFPS.Tag := 2;
              ButtonTestFPS.Text := 'Stop FPS Test';
              Invalidate;
            end);
          End);
      end);
    //--
    fDebugFpsStarted := false;
    fDebugFpsCount := 0;
    fDebugFpsStopWatch := TstopWatch.StartNew; // to avoid warning fDebugFpsStopWatch no used in windows
    fDebugAverageFpsCount := 0;
    fDebugAverageFps := 0;
  end;
end;

{***************************************************************************}
procedure TMainForm.LoadImagePaintBoxPaint(Sender: TObject; Canvas: TCanvas);
begin
  if not (TpaintBox(Sender).Tag in [04,05,06,07,10,11,12,13,14,15]) then begin
    Canvas.Fill.Color := TalphaColorRec.Black;
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.FillRect(TPaintBox(Sender).LocalRect, TPaintBox(Sender).AbsoluteOpacity);
  end;
  var LDrawable := FScrollBoxDrawables[TpaintBox(Sender).Tag];
  if not ALIsDrawableNull(LDrawable) then begin
    var LDestRect := Trectf.Create(0,0,ALGetDrawableWidth(LDrawable) / ALGetScreenScale, ALGetDrawableHeight(LDrawable) / ALGetScreenScale)
                       .PlaceInto(TPaintBox(Sender).LocalRect);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      LDrawable, // const ADrawable: TALDrawable;
      Trectf.Create(0,0,ALGetDrawableWidth(LDrawable), ALGetDrawableHeight(LDrawable)), // const ASrcRect: TrectF; // IN REAL PIXEL !
      LDestRect, // const ADstRect: TrectF; // IN Virtual pixels !
      TPaintBox(Sender).AbsoluteOpacity); // const AOpacity: Single);
  end;
end;

{*******************************************************************************************}
procedure TMainForm.TextEllipsisElementClick(Sender: TObject; const Element: TALTextElement);
begin
  If Element.Id = 'ellipsis' then begin
    TALText(Sender).MaxHeight := 65535;
    Invalidate;
  end;
end;

{************************************************************************************************}
procedure TMainForm.TextEllipsisElementMouseEnter(Sender: TObject; const Element: TALTextElement);
begin
  If Element.Id = 'ellipsis' then
    TALText(Sender).Cursor := crHandPoint;
end;

{************************************************************************************************}
procedure TMainForm.TextEllipsisElementMouseLeave(Sender: TObject; const Element: TALTextElement);
begin
  If Element.Id = 'ellipsis' then
    TALText(Sender).Cursor := crDefault;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
