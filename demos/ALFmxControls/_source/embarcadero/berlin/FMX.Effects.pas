{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016 Embarcadero Technologies, Inc.      }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Effects;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Types, System.UITypes, FMX.Types, FMX.Types3D, FMX.Filter, FMX.Graphics;

type

{ ITriggerEffect }

  ITriggerEffect = interface
  ['{945DC18B-7801-43F8-997D-F19607399AE9}']
    procedure ApplyTriggerEffect(const AInstance: TFmxObject; const ATrigger: string);
  end;

  TEffectAnimator = class
  public
    class procedure ApplyTriggerEffect(const Target: TFmxObject; const AInstance: TFmxObject; const ATrigger: string);
    class procedure DefaultApplyTriggerEffect(const Target: TFmxObject; const AInstance: TFmxObject; const ATrigger: string);
  end;

{ TEffect }

  IEffectContainer = interface
  ['{FFC591A9-A520-45F2-BD49-17F76E7B057C}']
    procedure NeedUpdateEffects;
    procedure BeforeEffectEnabledChanged(const Enabled: Boolean);
    procedure EffectEnabledChanged(const Enabled: Boolean);
  end;

  TEffectStyle = (AfterPaint, DisablePaint, DisablePaintToBitmap);

  TEffectStyleHelper = record helper for TEffectStyle
  const
    esAfterPaint = TEffectStyle.AfterPaint deprecated 'Use TEffectStyle.AfterPaint';
    esDisablePaint = TEffectStyle.DisablePaint deprecated 'Use TEffectStyle.DisablePaint';
    esDisablePaintToBitmap = TEffectStyle.DisablePaintToBitmap deprecated 'Use TEffectStyle.DisablePaintToBitmap';
  end;

  TEffectStyles = set of TEffectStyle;

  TEffect = class(TFmxObject)
  private
    FEnabled: Boolean;
    FTrigger: TTrigger;
    procedure SetEnabled(const Value: Boolean);
  protected
    FEffectStyle: TEffectStyles;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRect(const ARect: TRectF): TRectF; virtual;
    function GetOffset: TPointF; virtual;
    procedure ProcessEffect(const Canvas: TCanvas; const Visual: TBitmap; const Data: Single); virtual;
    procedure ApplyTrigger(AInstance: TFmxObject; const ATrigger: string); virtual;
    procedure UpdateParentEffects;
    property EffectStyle: TEffectStyles read FEffectStyle;
    property Trigger: TTrigger read FTrigger write FTrigger;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

{ TFilterEffect }

  TFilterEffect = class(TEffect)
  private
    FFilter: TFilter;
  protected
    function CreateFilter: TFilter; virtual; abstract;
    property Filter: TFilter read FFilter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ProcessEffect(const Canvas: TCanvas; const Visual: TBitmap; const Data: Single); override;
    procedure ProcessTexture(const Visual: TTexture; const Context: TContext3D); virtual;
  end;

{ TBlurEffect }

  TBlurEffect = class(TFilterEffect)
  private
    FSoftness: Single;
    procedure SetSoftness(const Value: Single);
  protected
    function CreateFilter: TFilter; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetRect(const ARect: TRectF): TRectF; override;
    function GetOffset: TPointF; override;
  published
    property Softness: Single read FSoftness write SetSoftness nodefault;
    property Trigger;
    property Enabled;
  end;

{ TShadowEffect }

  TShadowEffect = class(TFilterEffect)
  private
    FDistance: Single;
    FSoftness: Single;
    FShadowColor: TAlphaColor;
    FOpacity: Single;
    FDirection: Single;
    procedure SetDistance(const Value: Single);
    procedure SetSoftness(const Value: Single);
    procedure SetShadowColor(const Value: TAlphaColor);
    procedure SetOpacity(const Value: Single);
    function GetShadowColor: TAlphaColor;
    procedure SetDirection(const Value: Single);
  protected
    function CreateFilter: TFilter; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetRect(const ARect: TRectF): TRectF; override;
    function GetOffset: TPointF; override;
  published
    property Distance: Single read FDistance write SetDistance;
    property Direction: Single read FDirection write SetDirection;
    property Softness: Single read FSoftness write SetSoftness nodefault;
    property Opacity: Single read FOpacity write SetOpacity nodefault;
    property ShadowColor: TAlphaColor read GetShadowColor write SetShadowColor;
    property Trigger;
    property Enabled;
  end;

{ TGlowEffect }

  TGlowEffect = class(TFilterEffect)
  private
    FGlowColor: TAlphaColor;
    FSoftness: Single;
    FOpacity: Single;
    procedure SetSoftness(const Value: Single);
    function GetGlowColor: TAlphaColor;
    procedure SetGlowColor(const Value: TAlphaColor);
    procedure SetOpacity(const Value: Single);
  protected
    function CreateFilter: TFilter; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetRect(const ARect: TRectF): TRectF; override;
    function GetOffset: TPointF; override;
  published
    property Softness: Single read FSoftness write SetSoftness nodefault;
    property GlowColor: TAlphaColor read GetGlowColor write SetGlowColor;
    property Opacity: Single read FOpacity write SetOpacity nodefault;
    property Trigger;
    property Enabled;
  end;

{ TInnerGlowEffect }

  TInnerGlowEffect = class(TFilterEffect)
  private
    FGlowColor: TAlphaColor;
    FSoftness: Single;
    FOpacity: Single;
    procedure SetSoftness(const Value: Single);
    function GetGlowColor: TAlphaColor;
    procedure SetGlowColor(const Value: TAlphaColor);
    procedure SetOpacity(const Value: Single);
  protected
    function CreateFilter: TFilter; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetRect(const ARect: TRectF): TRectF; override;
    function GetOffset: TPointF; override;
  published
    property Softness: Single read FSoftness write SetSoftness nodefault;
    property GlowColor: TAlphaColor read GetGlowColor write SetGlowColor;
    property Opacity: Single read FOpacity write SetOpacity nodefault;
    property Trigger;
    property Enabled;
  end;

{ TReflectionEffect }

  TReflectionEffect = class(TFilterEffect)
  private
    FOffset: Integer;
    FOpacity: Single;
    FLength: Single;
    procedure SetOpacity(const Value: Single);
    procedure SetOffset(const Value: Integer);
    procedure SetLength(const Value: Single);
  protected
    function CreateFilter: TFilter; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetRect(const ARect: TRectF): TRectF; override;
    function GetOffset: TPointF; override;
  published
    property Opacity: Single read FOpacity write SetOpacity nodefault;
    property Offset: Integer read FOffset write SetOffset;
    property Length: Single read FLength write SetLength;
    property Trigger;
    property Enabled;
  end;

{ TBevelEffect }

  TBevelEffect = class(TEffect)
  private
    FDirection: Single;
    FSize: Integer;
    procedure SetDirection(const Value: Single);
    procedure SetSize(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRect(const ARect: TRectF): TRectF; override;
    function GetOffset: TPointF; override;
    procedure ProcessEffect(const Canvas: TCanvas; const Visual: TBitmap; const Data: Single); override;
  published
    property Direction: Single read FDirection write SetDirection;
    property Size: Integer read FSize write SetSize;
    property Trigger;
    property Enabled;
  end;

{ TRasterEffect }

  TRasterEffect = class(TEffect)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Trigger;
    property Enabled;
  end;

procedure Blur(const Canvas: TCanvas; const Bitmap: TBitmap;
  const Radius: Integer; UseAlpha: Boolean = True);

implementation

uses
  System.TypInfo, System.SysUtils, System.UIConsts, System.Rtti, System.StrUtils, System.Math, System.Math.Vectors,
  FMX.Materials, FMX.Utils, FMX.Filter.Custom;

{ TEffectAnimator }

class procedure TEffectAnimator.ApplyTriggerEffect(const Target, AInstance: TFmxObject; const ATrigger: string);
var
  TriggerEffect: ITriggerEffect;
begin
  if Supports(Target, ITriggerEffect, TriggerEffect) then
    TriggerEffect.ApplyTriggerEffect(AInstance, ATrigger)
  else
    DefaultApplyTriggerEffect(Target, AInstance, ATrigger);
end;

class procedure TEffectAnimator.DefaultApplyTriggerEffect(const Target, AInstance: TFmxObject; const ATrigger: string);
var
  Obj: TFmxObject;
  Control: IControl;
begin
  if Target.Children <> nil then
    for Obj in Target.Children do
    begin
      if Obj is TEffect then
        TEffect(Obj).ApplyTrigger(AInstance, ATrigger);
      if Supports(Obj, IControl, Control) and Control.Locked and not Control.HitTest then
        ApplyTriggerEffect(Obj, AInstance, ATrigger);
    end;
end;

{ TEffect }

constructor TEffect.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := True;
end;

destructor TEffect.Destroy;
begin
  inherited;
end;

function TEffect.GetOffset: TPointF;
begin
  Result := PointF(0, 0);
end;

function TEffect.GetRect(const ARect: TRectF): TRectF;
begin
  Result := ARect;
end;

procedure TEffect.ApplyTrigger(AInstance: TFmxObject; const ATrigger: string);
var
  T: TRttiType;
  P: TRttiProperty;
  V: TValue;
  StartValue: Boolean;
  Line, Setter, Prop, Value: string;
begin
  if FTrigger = '' then
    Exit;
  if not Assigned(AInstance) then
    Exit;
  if ContainsText(FTrigger, ATrigger) then
  begin
    Line := FTrigger;
    Setter := GetToken(Line, ';');
    StartValue := False;
    while Setter <> '' do
    begin
      Prop := GetToken(Setter, '=');
      Value := Setter;
      T := SharedContext.GetType(AInstance.ClassInfo);
      if Assigned(T) then
      begin
        P := T.GetProperty(Prop);
        if Assigned(P) and (P.PropertyType.TypeKind = tkEnumeration) then
        begin
          V := P.GetValue(AInstance);
          StartValue := CompareText(BoolToStr(V.AsBoolean, True), string(Value)) = 0;
        end;
      end;
      Setter := GetToken(Line, ';');
    end;
    Enabled := StartValue;
  end;
end;

procedure TEffect.UpdateParentEffects;
var
  EffectContainer: IEffectContainer;
begin
  if not(csLoading in ComponentState) then
    if Supports(FParent, IEffectContainer, EffectContainer) then
      EffectContainer.NeedUpdateEffects;
end;

procedure TEffect.ProcessEffect(const Canvas: TCanvas; const Visual: TBitmap; const Data: Single);
begin
end;

procedure TEffect.SetEnabled(const Value: Boolean);
var
  EffectContainer: IEffectContainer;
begin
  if FEnabled <> Value then
  begin
    if Supports(FParent, IEffectContainer, EffectContainer) then
    begin
      EffectContainer.BeforeEffectEnabledChanged(Value);
      FEnabled := Value;
      EffectContainer.EffectEnabledChanged(Enabled);
    end
    else
      FEnabled := Value;
  end;
end;

{ TFilterEffect }

constructor TFilterEffect.Create(AOwner: TComponent);
begin
  inherited;
  FEffectStyle := [TEffectStyle.DisablePaint];
  FFilter := CreateFilter;
end;

destructor TFilterEffect.Destroy;
begin
  FreeAndNil(FFilter);
  inherited;
end;

procedure TFilterEffect.ProcessEffect(const Canvas: TCanvas; const Visual: TBitmap; const Data: Single);
begin
  if Assigned(FFilter) then
  begin
    FFilter.ValuesAsBitmap['Input'] := Visual;
    FFilter.ApplyWithoutCopyToOutput;
    TFilterManager.FilterContext.CopyToBitmap(Visual, TRect.Create(0, 0, Visual.Width, Visual.Height));
  end;
end;

procedure TFilterEffect.ProcessTexture(const Visual: TTexture; const Context: TContext3D);
var
  Ver: TVertexBuffer;
  Ind: TIndexBuffer;
  Mat: TTextureMaterial;
begin
  if Assigned(FFilter) then
  begin
    FFilter.ValuesAsTexture['Input'] := Visual;
    FFilter.ApplyWithoutCopytoOutput;
    if Assigned(Context) then
      if Context.BeginScene then
      try
        Ver := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0], 4);
        Ver.Vertices[0] := Point3D(Context.PixelToPixelPolygonOffset.X,
          Context.PixelToPixelPolygonOffset.Y, 0);
        Ver.TexCoord0[0] := PointF(0.0, 0.0);
        Ver.Vertices[1] := Point3D(Context.PixelToPixelPolygonOffset.X + Visual.Width,
          Context.PixelToPixelPolygonOffset.Y, 0);
        Ver.TexCoord0[1] := PointF(Visual.Width / TFilterManager.FilterTexture.Width, 0.0);
        Ver.Vertices[2] := Point3D(Context.PixelToPixelPolygonOffset.X + Visual.Width,
          Context.PixelToPixelPolygonOffset.Y + Visual.Height, 0);
        Ver.TexCoord0[2] := PointF(Visual.Width / TFilterManager.FilterTexture.Width,
          Visual.Height / TFilterManager.FilterTexture.Height);
        Ver.Vertices[3] := Point3D(Context.PixelToPixelPolygonOffset.X,
          Context.PixelToPixelPolygonOffset.Y + Visual.Height, 0);
        Ver.TexCoord0[3] := PointF(0.0, Visual.Height / TFilterManager.FilterTexture.Height);
        Ind := TIndexBuffer.Create(6);
        Ind[0] := 0;
        Ind[1] := 1;
        Ind[2] := 3;
        Ind[3] := 3;
        Ind[4] := 1;
        Ind[5] := 2;
        Mat := TTextureMaterial.Create;
        Mat.Texture := TFilterManager.FilterTexture;
        Context.Clear(0);
        Context.SetContextState(TContextState.cs2DScene);
        Context.SetContextState(TContextState.csZWriteOff);
        Context.SetContextState(TContextState.csZTestOff);
        Context.SetMatrix(TMatrix3D.Identity);
        Context.DrawTriangles(Ver, Ind, Mat, 1);
        Mat.Free;
        Ind.Free;
        Ver.Free;
      finally
        Context.EndScene;
      end;
  end;
end;

{ TBlurEffect }

constructor TBlurEffect.Create(AOwner: TComponent);
begin
  inherited;
  FEffectStyle := [TEffectStyle.DisablePaint];
  Softness := 0.3;
end;

function TBlurEffect.CreateFilter: TFilter;
begin
  Result := TFilterManager.FilterByName('GaussianBlur');
end;

function TBlurEffect.GetOffset: TPointF;
begin
  Result := TPointF.Create(Trunc(FSoftness * 20), Trunc(FSoftness * 20));
end;

function TBlurEffect.GetRect(const ARect: TRectF): TRectF;
begin
  Result := ARect;
  Result.Inflate(Trunc(FSoftness * 20), Trunc(FSoftness * 20));
end;

procedure TBlurEffect.SetSoftness(const Value: Single);
begin
  if FSoftness <> Value then
  begin
    FSoftness := Value;
    if FSoftness < 0 then
      FSoftness := 0;
    if FSoftness > 9 then
      FSoftness := 9;
    if Assigned(FFilter) then
      FFilter.ValuesAsFloat['BlurAmount'] := FSoftness;
    UpdateParentEffects;
  end;
end;

{ TShadowEffect }

constructor TShadowEffect.Create(AOwner: TComponent);
begin
  inherited;
  FEffectStyle := [];
end;

function TShadowEffect.CreateFilter: TFilter;
begin
  FShadowColor := $FF000000;
  FDirection := 45;
  FOpacity := 0.6;
  FSoftness := 0.3;
  FDistance := 3;
  Result := TFilterManager.FilterByName('GlowFilter');
  if Assigned(Result) then
  begin
    Result.ValuesAsColor['Color'] := PremultiplyAlpha(MakeColor(FShadowColor, FOpacity));
  end;
end;

function TShadowEffect.GetOffset: TPointF;
var
  S, C: Double;
begin
  SinCos(DegToRad(FDirection), S, C);
  Result := TPointF.Create(Trunc(FSoftness * 20), Trunc(FSoftness * 20));
end;

function TShadowEffect.GetRect(const ARect: TRectF): TRectF;
var
  S, C: Double;
begin
  Result := ARect;
  InflateRect(Result, Trunc(FSoftness * 20), Trunc(FSoftness * 20));
  SinCos(DegToRad(FDirection), S, C);
  OffsetRect(Result, FDistance * C, FDistance * S);
end;

function TShadowEffect.GetShadowColor: TAlphaColor;
begin
  Result := FShadowColor;
end;

procedure TShadowEffect.SetDirection(const Value: Single);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    UpdateParentEffects;
  end;
end;

procedure TShadowEffect.SetDistance(const Value: Single);
begin
  if FDistance <> Value then
  begin
    FDistance := Value;
    UpdateParentEffects;
  end;
end;

procedure TShadowEffect.SetOpacity(const Value: Single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if FOpacity < 0 then
      FOpacity := 0;
    if FOpacity > 1 then
      FOpacity := 1;
    if Assigned(Filter) then
      Filter.ValuesAsColor['Color'] := PremultiplyAlpha(MakeColor(FShadowColor, FOpacity));
    UpdateParentEffects;
  end;
end;

procedure TShadowEffect.SetShadowColor(const Value: TAlphaColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    if Assigned(Filter) then
      Filter.ValuesAsColor['Color'] := PremultiplyAlpha(MakeColor(FShadowColor, FOpacity));
    UpdateParentEffects;
  end;
end;

procedure TShadowEffect.SetSoftness(const Value: Single);
begin
  if FSoftness <> Value then
  begin
    FSoftness := Value;
    if FSoftness < 0 then
      FSoftness := 0;
    if FSoftness > 3 then
      FSoftness := 3;
    if Assigned(Filter) then
      Filter.ValuesAsFloat['BlurAmount'] := FSoftness * 2;
    UpdateParentEffects;
  end;
end;

{ TGlowEffect }

constructor TGlowEffect.Create(AOwner: TComponent);
begin
  inherited;
  FEffectStyle := [];
end;

function TGlowEffect.CreateFilter: TFilter;
begin
  Result := TFilterManager.FilterByName('GlowFilter');
  FGlowColor := $FFFFD700;
  FSoftness := 0.4;
  FOpacity := 0.9;
  if Assigned(Result) then
  begin
    Result.ValuesAsFloat['BlurAmount'] := FSoftness * 2;
  end;
end;

function TGlowEffect.GetGlowColor: TAlphaColor;
begin
  Result := FGlowColor;
end;

function TGlowEffect.GetOffset: TPointF;
begin
  Result := TPointF.Create(4 + Trunc(FSoftness * 23), 4 + Trunc(FSoftness * 23));
end;

function TGlowEffect.GetRect(const ARect: TRectF): TRectF;
begin
  Result := ARect;
  InflateRect(Result, 4 + Trunc(FSoftness * 23), 4 + Trunc(FSoftness * 23));
end;

procedure TGlowEffect.SetGlowColor(const Value: TAlphaColor);
begin
  if FGlowColor <> Value then
  begin
    FGlowColor := Value;
    if Assigned(Filter) then
      Filter.ValuesAsColor['Color'] := PremultiplyAlpha(MakeColor(FGlowColor, FOpacity));
    UpdateParentEffects;
  end;
end;

procedure TGlowEffect.SetOpacity(const Value: Single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if FOpacity < 0 then
      FOpacity := 0;
    if FOpacity > 1 then
      FOpacity := 1;
    if Assigned(Filter) then
      Filter.ValuesAsColor['Color'] := PremultiplyAlpha(MakeColor(FGlowColor, FOpacity));
    UpdateParentEffects;
  end;
end;

procedure TGlowEffect.SetSoftness(const Value: Single);
begin
  if FSoftness <> Value then
  begin
    FSoftness := Value;
    if FSoftness < 0 then
      FSoftness := 0;
    if FSoftness > 9 then
      FSoftness := 9;
    if Assigned(Filter) then
      Filter.ValuesAsFloat['BlurAmount'] := FSoftness * 2;
    UpdateParentEffects;
  end;
end;

{ TInnerGlowEffect }

constructor TInnerGlowEffect.Create(AOwner: TComponent);
begin
  inherited;
  FEffectStyle := [TEffectStyle.AfterPaint];
end;

function TInnerGlowEffect.CreateFilter: TFilter;
begin
  Result := TFilterManager.FilterByName('InnerGlowFilter');
  FGlowColor := $FFFFD700;
  FSoftness := 0.4;
  FOpacity := 0.9;
  if Assigned(Result) then
  begin
    Result.ValuesAsColor['Color'] := PremultiplyAlpha(MakeColor(FGlowColor, FOpacity));
    Result.ValuesAsFloat['BlurAmount'] := FSoftness * 2;
  end;
end;

function TInnerGlowEffect.GetOffset: TPointF;
begin
  Result := TPointF.Create(Trunc(FSoftness * 20), Trunc(FSoftness * 20));
end;

function TInnerGlowEffect.GetRect(const ARect: TRectF): TRectF;
begin
  Result := ARect;
  InflateRect(Result, Trunc(FSoftness * 20), Trunc(FSoftness * 20));
end;

function TInnerGlowEffect.GetGlowColor: TAlphaColor;
begin
  Result := FGlowColor;
end;

procedure TInnerGlowEffect.SetGlowColor(const Value: TAlphaColor);
begin
  if FGlowColor <> Value then
  begin
    FGlowColor := Value;
    if Assigned(Filter) then
      Filter.ValuesAsColor['Color'] := PremultiplyAlpha(MakeColor(FGlowColor, FOpacity));
    UpdateParentEffects;
  end;
end;

procedure TInnerGlowEffect.SetOpacity(const Value: Single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if FOpacity < 0 then
      FOpacity := 0;
    if FOpacity > 1 then
      FOpacity := 1;
    if Assigned(Filter) then
      Filter.ValuesAsColor['Color'] := PremultiplyAlpha(MakeColor(FGlowColor, FOpacity));
    UpdateParentEffects;
  end;
end;

procedure TInnerGlowEffect.SetSoftness(const Value: Single);
begin
  if FSoftness <> Value then
  begin
    FSoftness := Value;
    if FSoftness < 0 then
      FSoftness := 0;
    if FSoftness > 9 then
      FSoftness := 9;
    if Assigned(Filter) then
      Filter.ValuesAsFloat['BlurAmount'] := FSoftness * 2;
    UpdateParentEffects;
  end;
end;

{ TReflectionEffect }

constructor TReflectionEffect.Create(AOwner: TComponent);
begin
  inherited;
  FEffectStyle := [];
end;

function TReflectionEffect.CreateFilter: TFilter;
begin
  Result := TFilterManager.FilterByName('ReflectionFilter');
  FOffset := 0;
  FLength := 0.5;
  FOpacity := 0.5;
  if Assigned(Result) then
  begin
    Result.ValuesAsFloat['Opacity'] := FOpacity;
    Result.ValuesAsFloat['Length'] := FLength;
  end;
end;

function TReflectionEffect.GetOffset: TPointF;
begin
  Result := PointF(2, 2);
end;

function TReflectionEffect.GetRect(const ARect: TRectF): TRectF;
begin
  Result := ARect;
  InflateRect(Result, 2, 2);
  OffsetRect(Result, 0, RectHeight(ARect) + FOffset);
end;

procedure TReflectionEffect.SetLength(const Value: Single);
begin
  if FLength <> Value then
  begin
    FLength := Value;
    if FLength < 0.1 then
      FLength := 0.1;
    if FLength > 1 then
      FLength := 1;
    if Assigned(Filter) then
      Filter.ValuesAsFloat['Length'] := FLength;
    UpdateParentEffects;
  end;
end;

procedure TReflectionEffect.SetOffset(const Value: Integer);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    UpdateParentEffects;
  end;
end;

procedure TReflectionEffect.SetOpacity(const Value: Single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if Assigned(Filter) then
      Filter.ValuesAsFloat['Opacity'] := FOpacity;
    UpdateParentEffects;
  end;
end;

{ TBevelEffect }

type
  PIntArray = ^TIntArray;
  TIntArray = array [0 .. 0] of Integer;

// Stack Blur Algorithm by Mario Klingemann <mario@quasimondo.com>
{$R-}
procedure BlurPtr(const Canvas: TCanvas; const pix: PAlphaColorArray;
  w, h: Integer; const Radius: Integer; UseAlpha: Boolean = True);
var
  wm, hm, wh, vdiv: Integer;
  rsum, gsum, bsum, asum, x, y, i, yp, yi, yw: Integer;
  P: cardinal;
  divsum: Integer;
  stackpointer, stackstart: Integer;
  sir: PAlphaColorRec;
  rbs, r1, routsum, goutsum, boutsum, aoutsum, rinsum, ginsum, binsum,
  ainsum: Integer;
  dv: PIntArray;
  vmin: PIntArray;
  r, g, b, a: PIntArray;
  stack: PAlphaColorArray;
begin
  if (Radius < 1) then
    Exit;
  dv := nil;
  stack := nil;

  wm := w - 1;
  hm := h - 1;
  wh := w * h;
  vdiv := Radius + Radius + 1;

  GetMem(r, wh * SizeOf(Integer));
  GetMem(g, wh * SizeOf(Integer));
  GetMem(b, wh * SizeOf(Integer));
  GetMem(a, wh * SizeOf(Integer));
  GetMem(vmin, max(w, h) * SizeOf(Integer));
  try
    divsum := (vdiv + 1) shr 1;
    divsum := divsum * divsum;
    GetMem(dv, 256 * divsum * SizeOf(Integer));
    for i := 0 to 256 * divsum - 1 do
      dv[i] := (i div divsum);

    yw := 0;
    yi := 0;

    GetMem(stack, vdiv * SizeOf(TAlphaColor));

    r1 := Radius + 1;

    for y := 0 to h - 1 do
    begin
      rinsum := 0;
      ginsum := 0;
      binsum := 0;
      ainsum := 0;
      routsum := 0;
      goutsum := 0;
      boutsum := 0;
      aoutsum := 0;
      rsum := 0;
      gsum := 0;
      bsum := 0;
      asum := 0;
      for i := -Radius to Radius do
      begin
        P := pix[yi + min(wm, max(i, 0))];
        sir := @stack[i + Radius];
        sir.Color := P;
        rbs := r1 - abs(i);
        rsum := rsum + (sir.r * rbs);
        gsum := gsum + (sir.g * rbs);
        bsum := bsum + (sir.b * rbs);
        if UseAlpha then
          asum := asum + (sir.a * rbs);
        if (i > 0) then
        begin
          rinsum := rinsum + sir.r;
          ginsum := ginsum + sir.g;
          binsum := binsum + sir.b;
          if UseAlpha then
            ainsum := ainsum + sir.a;
        end
        else
        begin
          routsum := routsum + sir.r;
          goutsum := goutsum + sir.g;
          boutsum := boutsum + sir.b;
          if UseAlpha then
            aoutsum := aoutsum + sir.a;
        end
      end;
      stackpointer := Radius;

      for x := 0 to w - 1 do
      begin
        r[yi] := dv[rsum];
        g[yi] := dv[gsum];
        b[yi] := dv[bsum];
        if UseAlpha then
          a[yi] := dv[asum];

        rsum := rsum - routsum;
        gsum := gsum - goutsum;
        bsum := bsum - boutsum;
        if UseAlpha then
          asum := asum - aoutsum;

        stackstart := stackpointer - Radius + vdiv;
        sir := @stack[stackstart mod vdiv];

        routsum := routsum - sir.r;
        goutsum := goutsum - sir.g;
        boutsum := boutsum - sir.b;
        if UseAlpha then
          aoutsum := aoutsum - sir.a;

        if (y = 0) then
        begin
          vmin[x] := min(x + Radius + 1, wm);
        end;
        P := pix[yw + vmin[x]];
        sir.Color := P;

        rinsum := rinsum + sir.r;
        ginsum := ginsum + sir.g;
        binsum := binsum + sir.b;
        if UseAlpha then
          ainsum := ainsum + sir.a;

        rsum := rsum + rinsum;
        gsum := gsum + ginsum;
        bsum := bsum + binsum;
        if UseAlpha then
          asum := asum + ainsum;

        stackpointer := (stackpointer + 1) mod vdiv;
        sir := @stack[(stackpointer) mod vdiv];

        routsum := routsum + sir.r;
        goutsum := goutsum + sir.g;
        boutsum := boutsum + sir.b;
        if UseAlpha then
          aoutsum := aoutsum + sir.a;

        rinsum := rinsum - sir.r;
        ginsum := ginsum - sir.g;
        binsum := binsum - sir.b;
        if UseAlpha then
          ainsum := ainsum - sir.a;

        yi := yi + 1;
      end;
      yw := yw + w;
    end;

    for x := 0 to w - 1 do
    begin
      rinsum := 0;
      ginsum := 0;
      binsum := 0;
      ainsum := 0;
      routsum := 0;
      goutsum := 0;
      boutsum := 0;
      aoutsum := 0;
      rsum := 0;
      gsum := 0;
      bsum := 0;
      asum := 0;
      yp := -Radius * w;
      for i := -Radius to Radius do
      begin
        yi := max(0, yp) + x;

        sir := @stack[i + Radius];

        sir.r := r[yi];
        sir.g := g[yi];
        sir.b := b[yi];
        if UseAlpha then
          sir.a := a[yi];

        rbs := r1 - abs(i);

        rsum := rsum + (r[yi] * rbs);
        gsum := gsum + (g[yi] * rbs);
        bsum := bsum + (b[yi] * rbs);
        if UseAlpha then
          asum := asum + (a[yi] * rbs);

        if (i > 0) then
        begin
          rinsum := rinsum + sir.r;
          ginsum := ginsum + sir.g;
          binsum := binsum + sir.b;
          if UseAlpha then
            ainsum := ainsum + sir.a;
        end
        else
        begin
          routsum := routsum + sir.r;
          goutsum := goutsum + sir.g;
          boutsum := boutsum + sir.b;
          if UseAlpha then
            aoutsum := aoutsum + sir.a;
        end;

        if (i < hm) then
        begin
          yp := yp + w;
        end
      end;
      yi := x;
      stackpointer := Radius;
      for y := 0 to h - 1 do
      begin
        pix[yi] := (dv[asum] shl 24) or (dv[rsum] shl 16) or (dv[gsum] shl 8)
          or dv[bsum];

        rsum := rsum - routsum;
        gsum := gsum - goutsum;
        bsum := bsum - boutsum;
        if UseAlpha then
          asum := asum - aoutsum;

        stackstart := stackpointer - Radius + vdiv;
        sir := @stack[stackstart mod vdiv];

        routsum := routsum - sir.r;
        goutsum := goutsum - sir.g;
        boutsum := boutsum - sir.b;
        if UseAlpha then
          aoutsum := aoutsum - sir.a;

        if (x = 0) then
        begin
          vmin[y] := min(y + r1, hm) * w;
        end;
        P := x + vmin[y];

        sir.r := r[P];
        sir.g := g[P];
        sir.b := b[P];
        if UseAlpha then
          sir.a := a[P];

        rinsum := rinsum + sir.r;
        ginsum := ginsum + sir.g;
        binsum := binsum + sir.b;
        if UseAlpha then
          ainsum := ainsum + sir.a;

        rsum := rsum + rinsum;
        gsum := gsum + ginsum;
        bsum := bsum + binsum;
        if UseAlpha then
          asum := asum + ainsum;

        stackpointer := (stackpointer + 1) mod vdiv;
        sir := @stack[stackpointer];

        routsum := routsum + sir.r;
        goutsum := goutsum + sir.g;
        boutsum := boutsum + sir.b;
        if UseAlpha then
          aoutsum := aoutsum + sir.a;

        rinsum := rinsum - sir.r;
        ginsum := ginsum - sir.g;
        binsum := binsum - sir.b;
        if UseAlpha then
          ainsum := ainsum - sir.a;

        yi := yi + w;
      end;
    end;
  finally
    FreeMem(stack, vdiv * SizeOf(TAlphaColor));
    FreeMem(dv, 256 * divsum * SizeOf(Integer));
    FreeMem(vmin, max(w, h) * SizeOf(Integer));
    FreeMem(a, wh * SizeOf(Integer));
    FreeMem(r, wh * SizeOf(Integer));
    FreeMem(g, wh * SizeOf(Integer));
    FreeMem(b, wh * SizeOf(Integer));
  end;
end;

procedure Blur(const Canvas: TCanvas; const Bitmap: TBitmap;
  const Radius: Integer; UseAlpha: Boolean = True);
var
  Data: TBitmapData;
begin
  if Bitmap.Map(TMapAccess.ReadWrite, Data) then
  try
    BlurPtr(Canvas, Data.Data, Bitmap.Width, Bitmap.Height, Radius, UseAlpha);
  finally
    Bitmap.Unmap(Data);
  end;
end;

constructor TBevelEffect.Create(AOwner: TComponent);
begin
  inherited;
  FEffectStyle := [TEffectStyle.DisablePaint];
  FDirection := 45;
  FSize := 10;
end;

destructor TBevelEffect.Destroy;
begin
  inherited;
end;

function TBevelEffect.GetOffset: TPointF;
begin
  Result := PointF(5, 5);
end;

function TBevelEffect.GetRect(const ARect: TRectF): TRectF;
begin
  Result := ARect;
  InflateRect(Result, 5, 5);
end;

function VectorAngleCosine(const V1, V2: TVector): Single;
var
  Dot, Len1, Len2: Single;
begin
  Len1 := Sqrt((V1.X * V1.X) + (V1.Y * V1.Y) + (V1.W * V1.W));
  Len2 := Sqrt((V2.X * V2.X) + (V2.Y * V2.Y) + (V2.W * V2.W));
  Dot := V1.V[0] * V2.V[0] + V1.V[1] * V2.V[1] + V1.V[2] * V2.V[2];
  Result := Len1 * Len2;
  if Abs(Result) > 1E-40 then
    Result := Dot / Result
  else
    Result := 1;
end;

procedure TBevelEffect.ProcessEffect(const Canvas: TCanvas; const Visual: TBitmap; const Data: Single);
var
  DestBits, Bits: PAlphaColorRecArray;
  Color, DestColor: TAlphaColor;
  I, J: Integer;
  A, H0, H2, H3: Single;
  Alpha: Byte;
  Light, N, V, B: TVector;
  Map: TBitmapData;
  H, S, L: Single;
begin
  if FSize = 0 then
    Exit;

  if Visual.Map(TMapAccess.ReadWrite, Map) then
  try
    DestBits := PAlphaColorRecArray(Map.Data);
    { create lightmap }
    GetMem(Bits, Visual.Width * Visual.Height * 4);
    { copy bitmap }
    System.Move(DestBits^, Bits^, Visual.Width * Visual.Height * 4);
    { blur - make HeightMap }
    BlurPtr(Canvas, PAlphaColorArray(Bits), Visual.Width, Visual.Height, FSize);
    { calculate lighting }
    A := DegToRad(FDirection);
    Light.X := Cos(A);
    Light.Y := 0;
    Light.W := Sin(A);
    { make normalmap from hightmap }
    for J := Visual.Height - 2 downto 0 do
    begin
      for I := Visual.Width - 2 downto 0 do
      begin
        // only calc not transparent pixels
        Alpha := DestBits[(I) + ((J) * Visual.Width)].A;
        if Alpha > 0 then
        begin
          H0 := (Bits[I + ((J + 1) * Visual.Width)].Color and $FF000000 shr 24) / $FF; // .height(x  ,z+1);
          H2 := (Bits[(I + 1) + (J * Visual.Width)].Color and $FF000000 shr 24) / $FF; // height(x+1,  z);
          H3 := (Bits[(I + 1) + ((J + 1) * Visual.Width)].Color and $FF000000 shr 24) / $FF; // height(x  ,  z);

          V.X := 1.0;
          V.Y := H2 - H3;
          V.W := 0;

          B.X := 0;
          B.Y := H0 - H3;
          B.W := 1; // vector length

          // calc normal
          N := B.CrossProduct(V);

          // normalize
          N := N / B.W;

          // calc Light
          A := VectorAngleCosine(Light, N) * FSize;

          // set value
          Color := DestBits[I + (J * Visual.Width)].Color;
          RGBtoHSL(Color, H, S, L);
          L := L + (A * 0.4);
          if L > 1 then L := 1;
          if L < 0 then L := 0;
          DestColor := HSLtoRGB(H, S, L);
          TAlphaColorRec(DestColor).A := TAlphaColorRec(Color).A;

          DestBits[I + (J * Visual.Width)].Color := PremultiplyAlpha(MakeColor(DestColor, Alpha / $FF));
        end;
      end;
    end;
    FreeMem(Bits);
  finally
    Visual.Unmap(Map);
  end;
end;

procedure TBevelEffect.SetDirection(const Value: Single);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    UpdateParentEffects;
  end;
end;

procedure TBevelEffect.SetSize(const Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    UpdateParentEffects;
  end;
end;

{ TRasterEffect }

constructor TRasterEffect.Create(AOwner: TComponent);
begin
  inherited;
  FEffectStyle := [TEffectStyle.DisablePaint];
end;

procedure RegisterAliases;
begin
  AddEnumElementAliases(TypeInfo(TEffectStyle), ['esAfterPaint', 'esDisablePaint', 'esDisablePaintToBitmap']);
end;

procedure UnregisterAliases;
begin
  RemoveEnumElementAliases(TypeInfo(TEffectStyle));
end;

initialization
  RegisterAliases;
  RegisterFmxClasses([TShadowEffect, TBlurEffect, TGlowEffect,
    TInnerGlowEffect, TBevelEffect, TReflectionEffect, TRasterEffect]);
finalization
  UnregisterAliases;
end.
