unit FillStrokeShadowTestMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes,
  System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Alcinoe.FMX.Objects, Alcinoe.FMX.Controls, Alcinoe.FMX.Layouts,
  Alcinoe.FMX.Common, Alcinoe.FMX.StdCtrls;

type
  TForm1 = class(TForm)
    ALVertScrollBox1: TALVertScrollBox;
    ALText1: TALText;
    ALButtonCreateRectangles: TALButton;
    ALButtonCreateCircles: TALButton;
    ALButtonCreateRectanglesOpacity: TALButton;
    ALButtonCreateCirclesOpacity: TALButton;
    ALButtonCreateRectanglesFillMargins: TALButton;
    ALButtonCreateCirclesFillMargins: TALButton;
    procedure ALButtonCreateRectanglesClick(Sender: TObject);
    procedure ALButtonCreateCirclesClick(Sender: TObject);
    procedure ALButtonCreateRectanglesOpacityClick(Sender: TObject);
    procedure ALButtonCreateCirclesOpacityClick(Sender: TObject);
    procedure ALButtonCreateRectanglesFillMarginsClick(Sender: TObject);
    procedure ALButtonCreateCirclesFillMarginsClick(Sender: TObject);
  private
    procedure Clear;
    Procedure CreateRectangles(
                const aWithOpacity: Boolean;
                const aWithFillMargins: Boolean);
    Procedure CreateCircles(
                const aWithOpacity: Boolean;
                const aWithFillMargins: Boolean);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses System.Rtti, System.TypInfo, Alcinoe.Common;

{*********************}
procedure TForm1.Clear;
begin
  For var i := ALVertScrollBox1.content.ControlsCount - 1 downto 0 do begin
    var LControl := ALVertScrollBox1.content.Controls[i];
    if (LControl is TALRectangle) or
       (LControl is TALCircle) or
       ((LControl is TALText) and (Lcontrol <> ALText1)) then begin
      ALFreeAndNil(LControl);
    end;
  end;
end;

{********************************}
Procedure TForm1.CreateRectangles(
            const aWithOpacity: Boolean;
            const aWithFillMargins: Boolean);

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure AddRectangle(
              const AOpacity: Boolean;
              const AFillColor: TAlphaColor;
              const AFillGradientStyle: TGradientStyle;
              const AFillGradientColors: Tarray<TAlphaColor>;
              const AFillResourceName: String;
              Const AFillWrapMode: TALImageWrapMode;
              const aFillBackgroundMarginsRect: TrectF;
              const aFillImageMarginsRect: TrectF;
              const AStrokeColor: TalphaColor;
              const AStrokeThickness: Single;
              const AShadowColor: TAlphaColor;
              const ADrawPath: Boolean;
              const ARoundCorner: boolean;
              const AText: String);
  begin
    With TALText.Create(ALVertScrollBox1) do begin
      Parent := ALVertScrollBox1;
      Text := AText;
      Align := TAlignLayout.Top;
      Margins.left := 16;
      AutoSize := True;
      Size.Width := 44;
      Size.Height := 16;
      TextSettings.Font.Size := 12;
    end;
    With TALRectangle.Create(ALVertScrollBox1) do begin
      Parent := ALVertScrollBox1;
      Align := TalignLayout.Top;
      Fill.Color := AFillColor;
      Fill.Gradient.Style := AFillGradientStyle;
      Fill.Gradient.colors := AFillGradientColors;
      Fill.ResourceName := AFillResourceName;
      Fill.WrapMode := AFillWrapMode;
      Fill.BackgroundMargins.Rect := aFillBackgroundMarginsRect;
      Fill.ImageMargins.Rect := aFillImageMarginsRect;
      Height := 50;
      Margins.Left := 50;
      Margins.Top := 20;
      Margins.Right := 50;
      Margins.Bottom := 20;
      Stroke.Color := AStrokeColor;
      Stroke.Thickness := AStrokeThickness;
      Shadow.Color := AShadowColor;
      Shadow.Blur := 12;
      Shadow.OffsetX := 12;
      Shadow.OffsetY := 12;
      if ARoundCorner then begin
        XRadius := -50;
        YRadius := -50;
      end;
      if ADrawPath then begin
        Sides := [TSide.Bottom];
        Corners := [Tcorner.TopLeft, TCorner.BottomRight];
      end;
      if AOpacity then begin
        DoubleBuffered := False;
        Opacity := 0.50;
      end;
    end
  end;

begin
  BeginUpdate;
  try
    Clear;
    for var LDrawPath := false to True do begin
      for var LRoundCorners := True downto false do begin
        for var LShadow: Boolean := true downto false do begin
          for var LStrokeColor: Boolean := False to True do begin
            for var LFillColor: Boolean := False to True do begin
              for var LGradient: Boolean := False to True do begin
                for var LFillResourceName: Boolean := False to True do begin
                  for var LFillWrapMode: TALImageWrapMode := low(TALImageWrapMode) to high(TALImageWrapMode) do begin
                    for var LFillBackgroundMargins := -1 to 1 do begin
                      for var LFillImageMargins := -1 to 1 do begin
                        for var LOpacity := False to True do begin
                          if aWithFillMargins and (LFillBackgroundMargins = 0) then continue;
                          if (not aWithFillMargins) and (LFillBackgroundMargins <> 0) then continue;
                          if aWithFillMargins and (LFillImageMargins = 0) then continue;
                          if (not aWithFillMargins) and (LFillImageMargins <> 0) then continue;
                          IF (LFillImageMargins <> 0) and (not LFillResourceName) then continue;
                          IF (LFillBackgroundMargins <> 0) and ((not LFillColor) or (not LGradient)) then continue;
                          if aWithOpacity and (not LOpacity) then continue;
                          if (not aWithOpacity) and LOpacity then continue;
                          var LFillGradientColors: Tarray<TAlphaColor>;
                          if LGradient then begin
                            setlength(LFillGradientColors, 3);
                              LFillGradientColors[0] := $FFff7e5f;
                              LFillGradientColors[1] := $FFfeb47b;
                              LFillGradientColors[2] := $FF86a8e7;
                          end
                          else
                            setlength(LFillGradientColors, 0);
                          if (LFillWrapMode > low(TALImageWrapMode)) and not LFillResourceName then continue;
                          var LFillBackgroundMarginsRect := TRectF.Empty;
                          if LFillBackgroundMargins < 0 then LFillBackgroundMarginsRect := TRectF.Create(-10,-10,-10,-10)
                          else if LFillBackgroundMargins > 0 then LFillBackgroundMarginsRect := TRectF.Create(10,10,10,10);
                          var LFillImageMarginsRect := TRectF.Empty;
                          if LFillImageMargins < 0 then LFillImageMarginsRect := TRectF.Create(-10,-10,-10,-10)
                          else if LFillImageMargins > 0 then LFillImageMarginsRect := TRectF.Create(10,10,10,10);
                          var Ltext: String := '/';
                          if LDrawPath then Ltext := Ltext +  'DrawPath/';
                          if LRoundCorners then Ltext := Ltext +  'RoundCorners/';
                          if LShadow then Ltext := Ltext +  'Shadow/';
                          if LStrokeColor then Ltext := Ltext +  'Stroke/';
                          if LFillColor then Ltext := Ltext +  'FillColor/';
                          if LGradient then Ltext := Ltext +  'Gradient/';
                          if LFillResourceName then Ltext := Ltext +  'FillRes';
                          if LFillResourceName then Ltext := Ltext + ':'+GetEnumName(TypeInfo(TALImageWrapMode),Ord(LFillWrapMode))+ '/';
                          if LOpacity then Ltext := Ltext +  'Opacity/';
                          if LFillBackgroundMargins <> 0 then Ltext := Ltext +  'FillBGMargins:'+ALIfThenW(LFillBackgroundMargins<0, '-', '+')+'/';
                          if LFillImageMargins <> 0 then Ltext := Ltext +  'FillImgMargins:'+ALIfThenW(LFillImageMargins<0, '-', '+')+'/';
                          //if (not Sametext(Ltext, '/Stroke/FillColor/Gradient/FillRes:Fit/FillBGMargins:+/FillImgMargins:-/')) then continue;
                          AddRectangle(
                              LOpacity,
                              ALIfThen(LFillColor, TAlphaColors.yellow, TAlphaColors.Null),// const AFillColor: TAlphaColor;
                              TGradientStyle.Linear, // const AFillGradientStyle: TGradientStyle;
                              LFillGradientColors, // const AFillGradientColors: Tarray<TAlphaColor>;
                              ALIfThenW(LFillResourceName, ALIfThenW(LFillWrapMode in [TALImageWrapMode.FitAndCrop], 'sticker_cropped', 'sticker'), ''), // const AFillResourceName: String;
                              LFillWrapMode, // Const AFillWrapMode: TALImageWrapMode;
                              LFillBackgroundMarginsRect, // const aFillBackgroundMarginsRect: TrectF;
                              LFillImageMarginsRect, // const aFillImageMarginsRect: TrectF;
                              ALIfThen(LStrokeColor, TAlphaColors.Black, TAlphaColors.Null), // const AStrokeColor: TalphaColor;
                              ALIfThen(LRoundCorners, 2, 12), // const AStrokeThickness: Single;
                              ALIfThen(LShadow, $96000000, TAlphaColors.Null), // const AShadowColor: TAlphaColor;
                              LDrawPath, // const ADrawPath: Boolean;
                              LRoundCorners, // const ARoundCorner: boolean;
                              Ltext // const AText: String
                            );
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
  finally
    EndUpdate;
  end;
  ALMakeBufDrawables(ALVertScrollBox1, false{AEnsureDoubleBuffered});
end;

{*****************************}
Procedure TForm1.CreateCircles(
            const aWithOpacity: Boolean;
            const aWithFillMargins: Boolean);

  {~~~~~~~~~~~~~~~~~~}
  procedure AddCircle(
              const AOpacity: Boolean;
              const AFillColor: TAlphaColor;
              const AFillGradientStyle: TGradientStyle;
              const AFillGradientColors: Tarray<TAlphaColor>;
              const AFillResourceName: String;
              Const AFillWrapMode: TALImageWrapMode;
              const aFillBackgroundMarginsRect: TrectF;
              const aFillImageMarginsRect: TrectF;
              const AStrokeColor: TalphaColor;
              const AStrokeThickness: Single;
              const AShadowColor: TAlphaColor;
              const AText: String);
  begin
    With TALText.Create(ALVertScrollBox1) do begin
      Parent := ALVertScrollBox1;
      Text := AText;
      Align := TAlignLayout.Top;
      Margins.left := 16;
      AutoSize := True;
      Size.Width := 44;
      Size.Height := 16;
      TextSettings.Font.Size := 12;
    end;
    With TALCircle.Create(ALVertScrollBox1) do begin
      Parent := ALVertScrollBox1;
      Align := TalignLayout.Top;
      Fill.Color := AFillColor;
      Fill.Gradient.Style := AFillGradientStyle;
      Fill.Gradient.colors := AFillGradientColors;
      Fill.ResourceName := AFillResourceName;
      Fill.WrapMode := AFillWrapMode;
      Fill.BackgroundMargins.Rect := aFillBackgroundMarginsRect;
      Fill.ImageMargins.Rect := aFillImageMarginsRect;
      Height := 150;
      Margins.Left := 50;
      Margins.Top := 20;
      Margins.Right := 50;
      Margins.Bottom := 20;
      Stroke.Color := AStrokeColor;
      Stroke.Thickness := AStrokeThickness;
      Shadow.Color := AShadowColor;
      Shadow.Blur := 12;
      Shadow.OffsetX := 12;
      Shadow.OffsetY := 12;
      if AOpacity then begin
        DoubleBuffered := False;
        Opacity := 0.50;
      end;
    end
  end;

begin
  BeginUpdate;
  try
    Clear;
    for var LShadow: Boolean := true downto false do begin
      for var LStrokeColor: Boolean := False to True do begin
        for var LFillColor: Boolean := False to True do begin
          for var LGradient: Boolean := False to True do begin
            for var LFillResourceName: Boolean := False to True do begin
              for var LFillWrapMode: TALImageWrapMode := low(TALImageWrapMode) to high(TALImageWrapMode) do begin
                for var LFillBackgroundMargins := -1 to 1 do begin
                  for var LFillImageMargins := -1 to 1 do begin
                    for var LOpacity := False to True do begin
                      if aWithFillMargins and (LFillBackgroundMargins = 0) then continue;
                      if (not aWithFillMargins) and (LFillBackgroundMargins <> 0) then continue;
                      if aWithFillMargins and (LFillImageMargins = 0) then continue;
                      if (not aWithFillMargins) and (LFillImageMargins <> 0) then continue;
                      IF (LFillImageMargins <> 0) and (not LFillResourceName) then continue;
                      IF (LFillBackgroundMargins <> 0) and ((not LFillColor) or (not LGradient)) then continue;
                      if aWithOpacity and (not LOpacity) then continue;
                      if (not aWithOpacity) and LOpacity then continue;
                      var LFillGradientColors: Tarray<TAlphaColor>;
                      if LGradient then begin
                        setlength(LFillGradientColors, 3);
                          LFillGradientColors[0] := $FFff7e5f;
                          LFillGradientColors[1] := $FFfeb47b;
                          LFillGradientColors[2] := $FF86a8e7;
                      end
                      else
                        setlength(LFillGradientColors, 0);
                      if (LFillWrapMode > low(TALImageWrapMode)) and not LFillResourceName then continue;
                      var LFillBackgroundMarginsRect := TRectF.Empty;
                      if LFillBackgroundMargins < 0 then LFillBackgroundMarginsRect := TRectF.Create(-10,-10,-10,-10)
                      else if LFillBackgroundMargins > 0 then LFillBackgroundMarginsRect := TRectF.Create(10,10,10,10);
                      var LFillImageMarginsRect := TRectF.Empty;
                      if LFillImageMargins < 0 then LFillImageMarginsRect := TRectF.Create(-10,-10,-10,-10)
                      else if LFillImageMargins > 0 then LFillImageMarginsRect := TRectF.Create(10,10,10,10);
                      var Ltext: String := '/';
                      if LShadow then Ltext := Ltext +  'Shadow/';
                      if LStrokeColor then Ltext := Ltext +  'Stroke/';
                      if LFillColor then Ltext := Ltext +  'FillColor/';
                      if LGradient then Ltext := Ltext +  'Gradient/';
                      if LFillResourceName then Ltext := Ltext +  'FillRes';
                      if LFillResourceName then Ltext := Ltext + ':'+GetEnumName(TypeInfo(TALImageWrapMode),Ord(LFillWrapMode))+ '/';
                      if LOpacity then Ltext := Ltext +  'Opacity/';
                      if LFillBackgroundMargins <> 0 then Ltext := Ltext +  'FillBGMargins:'+ALIfThenW(LFillBackgroundMargins<0, '-', '+')+'/';
                      if LFillImageMargins <> 0 then Ltext := Ltext +  'FillImgMargins:'+ALIfThenW(LFillImageMargins<0, '-', '+')+'/';
                      AddCircle(
                          LOpacity,
                          ALIfThen(LFillColor, TAlphaColors.yellow, TAlphaColors.Null),// const AFillColor: TAlphaColor;
                          TGradientStyle.Linear, // const AFillGradientStyle: TGradientStyle;
                          LFillGradientColors, // const AFillGradientColors: Tarray<TAlphaColor>;
                          ALIfThenW(LFillResourceName, ALIfThenW(LFillWrapMode in [TALImageWrapMode.FitAndCrop], 'sticker_cropped', 'sticker'), ''), // const AFillResourceName: String;
                          LFillWrapMode, // Const AFillWrapMode: TALImageWrapMode;
                          LFillBackgroundMarginsRect, // const aFillBackgroundMarginsRect: TrectF;
                          LFillImageMarginsRect, // const aFillImageMarginsRect: TrectF;
                          ALIfThen(LStrokeColor, TAlphaColors.Black, TAlphaColors.Null), // const AStrokeColor: TalphaColor;
                          2, // const AStrokeThickness: Single;
                          ALIfThen(LShadow, $96000000, TAlphaColors.Null), // const AShadowColor: TAlphaColor;
                          Ltext // const AText: String
                        );
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    EndUpdate;
  end;
  ALMakeBufDrawables(ALVertScrollBox1, false{AEnsureDoubleBuffered});
end;

{**********************************************************************}
procedure TForm1.ALButtonCreateCirclesFillMarginsClick(Sender: TObject);
begin
  CreateCircles(false{aWithOpacity}, true{aWithFillMargins});
end;

{***********************************************************}
procedure TForm1.ALButtonCreateCirclesClick(Sender: TObject);
begin
  CreateCircles(false{aWithOpacity}, false{aWithFillMargins});
end;

{******************************************************************}
procedure TForm1.ALButtonCreateCirclesOpacityClick(Sender: TObject);
begin
  CreateCircles(true{aWithOpacity}, false{aWithFillMargins});
end;

{**************************************************************}
procedure TForm1.ALButtonCreateRectanglesClick(Sender: TObject);
begin
  CreateRectangles(false{aWithOpacity}, false{aWithFillMargins});
end;

{*************************************************************************}
procedure TForm1.ALButtonCreateRectanglesFillMarginsClick(Sender: TObject);
begin
  CreateRectangles(false{aWithOpacity}, true{aWithFillMargins});
end;

{*********************************************************************}
procedure TForm1.ALButtonCreateRectanglesOpacityClick(Sender: TObject);
begin
  CreateRectangles(true{aWithOpacity}, false{aWithFillMargins});
end;

end.
