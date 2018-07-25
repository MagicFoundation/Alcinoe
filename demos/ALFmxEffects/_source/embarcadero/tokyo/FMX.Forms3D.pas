{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Forms3D;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Types, System.UITypes, System.Generics.Collections, FMX.Types, FMX.Types3D, FMX.Controls3D,
  FMX.Objects3D, FMX.Forms, FMX.Graphics;

type
  { TCustomForm3D }

  TCustomForm3D = class(TCommonCustomForm, IContextObject, IViewport3D)
  private
    FCamera: TCamera;
    FDesignCamera: TCamera;
    FDesignCameraZ: TDummy;
    FDesignCameraX: TDummy;
    FFill: TAlphaColor;
    FMultisample: TMultisample;
    FUsingDesignCamera: Boolean;
    FDrawing: Boolean;
    FOnRender: TRenderEvent;
    FEffectBitmap: TBitmap;
    FContext: TContext3D;
    FRenderingList: TList<TControl3D>;
    FLights: TList<TLight>;
    procedure SetFill(const Value: TAlphaColor);
    procedure SetMultisample(const Value: TMultisample);
    function GetFill: TAlphaColor;
    procedure RebuildRenderingList;
    procedure SetUsingDesignCamera(const Value: Boolean);
    procedure SkipTransparency(Reader: TReader);
    { IViewport3D }
    function GetObject: TFmxObject;
    function GetContext: TContext3D;
    function GetCamera: TCamera;
    function GetUsingDesignCamera: Boolean;
    function GetViewportScale: Single;
    function GetLightCount: Integer;
    function GetLight(Index: Integer): TLight;
    procedure SetCamera(const ACamera: TCamera);
    procedure AddLight(const ALight: TLight);
    procedure RemoveLight(const ALight: TLight);
    procedure NeedRender;
    function GetCurrentCamera: TCamera;
    procedure RenderScene;
  protected
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DefineProperties(Filer: TFiler); override;
    { Context }
    procedure CreateContext; virtual;
    procedure DestroyContext; virtual;
    { Handle }
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure ResizeHandle; override;
    procedure PaintRects(const UpdateRects: array of TRectF); override;
    { Preload }
    procedure AddPreloadPropertyNames(const PropertyNames: TList<string>); override;
    procedure SetPreloadProperties(const PropertyStore: TDictionary<string, Variant>); override;
    { inherited }
    procedure Realign; override;
    function FindTarget(P: TPointF; const Data: TDragObject): IControl; override;
    procedure SetTransparency(const Value: Boolean); override;
    procedure DoScaleChanged; override;
    { }
    function ScreenToLocal(P: TPointF): TPointF;
    function LocalToScreen(P: TPointF): TPointF;
    { Window style }
    function GetWindowStyle: TWindowStyles; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: NativeInt = 0); override;
    destructor Destroy; override;
    procedure EndUpdate; override;
    procedure RecreateResources; override;
    procedure InitializeNewForm; override;
    property Context: TContext3D read FContext;
    property Multisample: TMultisample read FMultisample write SetMultisample default TMultisample.FourSamples;
    property Color: TAlphaColor read GetFill write SetFill default TAlphaColors.White;
    property Camera: TCamera read FCamera write SetCamera;
    property UsingDesignCamera: Boolean read FUsingDesignCamera write SetUsingDesignCamera default True;
    function ObjectAtPoint(P: TPointF): IControl; override;
    property OnRender: TRenderEvent read FOnRender write FOnRender;
  end;

  TForm3D = class(TCustomForm3D)
  published
    property BiDiMode;
    property Camera;
    property Caption;
    property Color default TAlphaColors.White;
    property Cursor default crDefault;
    property Border;
    property BorderIcons default [TBorderIcon.biSystemMenu, TBorderIcon.biMinimize, TBorderIcon.biMaximize];
    property BorderStyle default TFmxFormBorderStyle.Sizeable;
    property ClientHeight;
    property ClientWidth;
    property Height;
    property Left;
    property Padding;
    property Multisample default TMultisample.FourSamples;
    property Position default TFormPosition.DefaultPosOnly;
    property SystemStatusBar;
    property StyleBook;
    property Top;
    property FormStyle default TFormStyle.Normal;
    property UsingDesignCamera default True;
    property Visible;
    property Width;
    property WindowState default TWindowState.wsNormal;
    property FormFactor;
    property FormFamily;
    {events}
    property OnActivate;
    property OnCreate;
    property OnClose;
    property OnCloseQuery;
    property OnDeactivate;
    property OnDestroy;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnRender;
    property OnResize;
    property OnShow;
    property OnHide;
    property OnVirtualKeyboardShown;
    property OnVirtualKeyboardHidden;
  end;

implementation

uses
  System.TypInfo, System.SysUtils, System.Variants, System.RTLConsts, System.Math.Vectors, FMX.Effects, FMX.Materials;

{ TCustomForm3D }

// Required to force Delphi-style initialization when used from C++.
constructor TCustomForm3D.Create(AOwner: TComponent);
begin
  inherited;
end;

constructor TCustomForm3D.CreateNew(AOwner: TComponent; Dummy: NativeInt);
begin
  inherited;
end;

procedure TCustomForm3D.InitializeNewForm;
begin
  inherited;
  FUsingDesignCamera := True;
  FFill := TAlphaColors.White;

  FLights := TList<TLight>.Create;

  FDesignCameraZ := TDummy.Create(nil);
  FDesignCameraZ.Tag := $FFFE;
  FDesignCameraZ.Locked := True;
  FDesignCameraZ.Stored := False;
  AddObject(FDesignCameraZ);

  FDesignCameraX := TDummy.Create(nil);
  FDesignCameraX.Tag := $FFFE;
  FDesignCameraX.Parent := FDesignCameraZ;
  FDesignCameraX.Locked := True;
  FDesignCameraX.Stored := False;
  FDesignCameraX.RotationAngle.X := -20;

  FDesignCamera := TCamera.Create(nil);
  FDesignCamera.Tag := $FFFE;
  FDesignCamera.Parent := FDesignCameraX;
  FDesignCamera.Locked := True;
  FDesignCamera.Stored := False;
  FDesignCamera.Position.Point := Point3D(0, 0, -20);
end;

destructor TCustomForm3D.Destroy;
begin
  FreeAndNil(FRenderingList);
  FreeAndNil(FEffectBitmap);
                                                                                             
  DeleteChildren;
  FreeAndNil(FContext);
  FreeAndNil(FLights);
  inherited;
end;

procedure TCustomForm3D.AddPreloadPropertyNames(const PropertyNames: TList<string>);
begin
  inherited ;
  PropertyNames.Add('Multisample');
end;

procedure TCustomForm3D.SetPreloadProperties(const PropertyStore: TDictionary<string, Variant>);
var
  Val: Variant;
begin
  inherited ;
  // Default
  FMultisample := TMultisample.FourSamples;
  // Preload
  PropertyStore.TryGetValue('Multisample', Val);
  if (Val <> Unassigned) and (Val <> Null) then
    FMultisample := TMultisample(GetEnumValue(TypeInfo(TMultisample), Val));
end;

procedure TCustomForm3D.CreateHandle;
begin
  inherited;
  CreateContext;
end;

procedure TCustomForm3D.ResizeHandle;
begin
  inherited;
  if (Context <> nil) and (ClientWidth > 0) and (ClientHeight > 0) then
  begin
    Context.SetSize(ClientWidth, ClientHeight);
    Context.SetCameraMatrix(GetCurrentCamera.CameraMatrix);
    Realign;
  end;
end;

procedure TCustomForm3D.DestroyHandle;
begin
  DestroyContext;
  inherited;
end;

procedure TCustomForm3D.CreateContext;
begin
  FContext := TContextManager.CreateFromWindow(Handle, ClientWidth, ClientHeight, FMultisample, True);
end;

procedure TCustomForm3D.DestroyContext;
begin
  FreeAndNil(FContext);
end;

procedure TCustomForm3D.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Only for backward compatibility with XE2
  Filer.DefineProperty('Transparency', SkipTransparency, nil, False);
end;

procedure TCustomForm3D.SkipTransparency(Reader: TReader);
begin
  Reader.ReadBoolean; // skip this
end;

procedure TCustomForm3D.DoScaleChanged;
begin
  inherited;
  DestroyContext;
  CreateContext;
end;

procedure TCustomForm3D.EndUpdate;
begin
  inherited;
  if FUpdating = 0 then
    RebuildRenderingList;
end;

procedure TCustomForm3D.RebuildRenderingList;
var
  I: Integer;
  CompareFunc: TRenderingCompare;
begin
  if (Children <> nil) and (Children.Count > 0) and (FUpdating = 0) then
  begin
    if FRenderingList = nil then
      FRenderingList := TList<TControl3D>.Create;
    FRenderingList.Clear;

    for I := 0 to Children.Count - 1 do
      if (Children[i] is TControl3D) then
        FRenderingList.Add(Children[I] as TControl3D);

    CompareFunc := TRenderingCompare.Create;
    try
      FRenderingList.Sort(CompareFunc);
    finally
      CompareFunc.Free;
    end;
  end;
end;

procedure TCustomForm3D.RecreateResources;
begin
  inherited;
  DestroyContext;
  CreateContext;
end;

procedure TCustomForm3D.AddLight(const ALight: TLight);
begin
  FLights.Add(ALight);
end;

procedure TCustomForm3D.RemoveLight(const ALight: TLight);
begin
  FLights.Remove(ALight);
end;

procedure TCustomForm3D.RenderScene;
var
  I: Integer;
  Control: TControl3D;
begin
  if Assigned(FOnRender) then
    FOnRender(Self, Context);
  if (FRenderingList <> nil) and (FRenderingList.Count > 0) then
  begin
    for I := 0 to FRenderingList.Count - 1 do
      if FRenderingList[i].Visible then
      begin
        Control := TControl3D(FRenderingList[I]);
        Control.RenderInternal;
      end;
  end;
end;

procedure TCustomForm3D.PaintRects(const UpdateRects: array of TRectF);
var
  I: Integer;
  Ver: TVertexBuffer;
  Ind: TIndexBuffer;
  Mat: TTextureMaterial;
  Effect: TEffect;
  P: TPointF;
begin
  if Context = nil then
    Exit;
  if FDrawing then
    Exit;
  FDrawing := True;
  try
    if Context.BeginScene then
    try
      Context.Clear([TClearTarget.Color, TClearTarget.Depth], FFill, 1.0, 0);
      Context.SetCameraMatrix(GetCurrentCamera.CameraMatrix);
      Context.SetCameraAngleOfView(GetCurrentCamera.AngleOfView);
      Context.Lights.Clear;
      for I := 0 to FLights.Count - 1 do
        Context.Lights.Add(FLights[I].LightDescription);

      RenderScene;

      if Children <> nil then
      begin
        for I := 0 to Children.Count - 1 do
          if (TFmxObject(Children[i]) is TEffect) and (TEffect(Children[i]).Enabled) then
          begin
            Effect := TEffect(Children[i]);

            if FEffectBitmap = nil then
              FEffectBitmap := TTextureBitmap.Create(FContext.Width, FContext.Height);

            FEffectBitmap.Assign(Context);
            Effect.ProcessEffect(nil, FEffectBitmap, 1);

            Ver := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0], 4);
            P := Context.PixelToPixelPolygonOffset;
            Ver.Vertices[0] := Point3D(P.X, P.Y, 0);
            Ver.TexCoord0[0] := PointF(0.0, 0.0);
            Ver.Vertices[1] := Point3D(FEffectBitmap.Width + P.X, P.Y, 0);
            Ver.TexCoord0[1] := PointF(1.0, 0.0);
            Ver.Vertices[2] := Point3D(FEffectBitmap.Width + P.X, FEffectBitmap.Height + P.Y, 0);
            Ver.TexCoord0[2] := PointF(1.0, 1.0);
            Ver.Vertices[3] := Point3D(P.X, FEffectBitmap.Height + P.Y, 0);
            Ver.TexCoord0[3] := PointF(0.0, 1.0);
            Ind := TIndexBuffer.Create(6);
            Ind[0] := 0;
            Ind[1] := 1;
            Ind[2] := 3;
            Ind[3] := 3;
            Ind[4] := 1;
            Ind[5] := 2;
            Context.SetMatrix(TMatrix3D.Identity);
            Context.SetContextState(TContextState.cs2DScene);
            Context.SetContextState(TContextState.csAllFace);
            Context.SetContextState(TContextState.csAlphaBlendOff);
            Context.SetContextState(TContextState.csZWriteOff);
            Context.SetContextState(TContextState.csZTestOff);

            Mat := TTextureMaterial.Create;
            Mat.Texture := TTextureBitmap(FEffectBitmap).Texture;
            Context.DrawTriangles(Ver, Ind, Mat, 1);

            if not (TEffectStyle.DisablePaint in Effect.EffectStyle) then
              RenderScene;

            if Mat <> nil then
              Mat.Free;
            if Ind <> nil then
              Ind.Free;
            if Ver <> nil then
              Ver.Free;
          end;
      end;
      {$IFDEF MSWINDOWS}
      if (csDesigning in ComponentState) and (Designer <> nil) then
      begin
        Designer.Decorate(Context);
      end;
      {$ENDIF}
    finally
      Context.EndScene;
    end;
  finally
    FDrawing := False;
  end;
end;

procedure TCustomForm3D.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  if AObject is TControl3D then
  begin
    TControl3D(AObject).SetNewViewport(Self);
    RebuildRenderingList;
  end;
  if (csDesigning in ComponentState) and (AObject is TCamera) and (AObject.Tag <> $FFFE) then
    Camera := TCamera(AObject);
end;

procedure TCustomForm3D.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  if AObject is TControl3D then
  begin
    TControl3D(AObject).SetNewViewport(nil);
    RebuildRenderingList;
  end;
end;

procedure TCustomForm3D.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCamera) then
    FCamera := nil;
end;

function TCustomForm3D.ObjectAtPoint(P: TPointF): IControl;
var
  i: Integer;
  Obj: TFmxObject;
  NewObj: IControl;
begin
  Result := nil;
  // first screen projection
  GlobalDistance := $FFFF;
  GlobalProjection := TProjection.Screen;
  for i := ChildrenCount - 1 downto 0 do
  begin
    Obj := Children[i];
    if IInterface(Obj).QueryInterface(IControl, NewObj) <> 0 then
      Continue;
    if not NewObj.GetVisible then
      Continue;
    NewObj := NewObj.ObjectAtPoint(P);
    if NewObj <> nil then
      Result := NewObj;
  end;
  if Result = nil then
  begin
    // second camera projection
    GlobalDistance := $FFFF;
    GlobalProjection := TProjection.Camera;
    for i := ChildrenCount - 1 downto 0 do
    begin
      Obj := Children[i];
      if IInterface(Obj).QueryInterface(IControl, NewObj) <> 0 then
        Continue;
      if not NewObj.GetVisible  then
        Continue;
      NewObj := NewObj.ObjectAtPoint(P);
      if NewObj <> nil then
        Result := NewObj;
    end;
  end;
end;

function TCustomForm3D.FindTarget(P: TPointF; const Data: TDragObject): IControl;
var
  i: Integer;
  Obj: TFmxObject;
  NewObj: IControl;
begin
  Result := nil;
  // first screen projection
  GlobalDistance := $FFFF;
  GlobalProjection := TProjection.Screen;
  for i := ChildrenCount - 1 downto 0 do
  begin
    Obj := Children[i];
    if IInterface(Obj).QueryInterface(IControl, NewObj) <> 0 then
      Continue;
    if not NewObj.Visible then Continue;
    NewObj := NewObj.FindTarget(P, Data);
    if NewObj <> nil then
      Result := NewObj;
  end;
  if Result = nil then
  begin
    // second camera projection
    GlobalDistance := $FFFF;
    GlobalProjection := TProjection.Camera;
    for i := ChildrenCount - 1 downto 0 do
    begin
      Obj := Children[i];
      if IInterface(Obj).QueryInterface(IControl, NewObj) <> 0 then
        Continue;
      if not NewObj.Visible then Continue;
      NewObj := NewObj.FindTarget(P, Data);
      if NewObj <> nil then
        Result := NewObj;
    end;
  end;
end;

function TCustomForm3D.GetFill: TAlphaColor;
begin
  Result := FFill;
end;

function TCustomForm3D.GetLight(Index: Integer): TLight;
begin
  Result := FLights[Index];
end;

function TCustomForm3D.GetLightCount: Integer;
begin
  Result := FLights.Count;
end;

procedure TCustomForm3D.SetFill(const Value: TAlphaColor);
begin
  if FFill <> Value then
  begin
    FFill := Value;
    NeedRender;
  end;
end;

function TCustomForm3D.ScreenToLocal(P: TPointF): TPointF;
begin
  Result := ScreenToClient(P);
end;

function TCustomForm3D.LocalToScreen(P: TPointF): TPointF;
begin
  Result := ClientToScreen(P);
end;

procedure TCustomForm3D.SetMultisample(const Value: TMultisample);
begin
  if FMultisample <> Value then
  begin
    FMultisample := Value;
    Recreate;
  end;
end;

procedure TCustomForm3D.SetTransparency(const Value: Boolean);
begin
  inherited SetTransparency(False);
end;

procedure TCustomForm3D.SetUsingDesignCamera(const Value: Boolean);
begin
  if FUsingDesignCamera <> Value then
  begin
    FUsingDesignCamera := Value;
    if FContext <> nil then
      NeedRender;
  end;
end;

{ IViewport3D }

function TCustomForm3D.GetObject: TFmxObject;
begin
  Result := Self;
end;

function TCustomForm3D.GetCamera: TCamera;
begin
  Result := FCamera;
end;

function TCustomForm3D.GetCurrentCamera: TCamera;
begin
  if (FCamera <> nil) and not FUsingDesignCamera and not (csDesigning in ComponentState) then
    Result := FCamera
  else
    Result := FDesignCamera;
end;

function TCustomForm3D.GetContext: TContext3D;
begin
  Result := FContext;
end;

function TCustomForm3D.GetUsingDesignCamera: Boolean;
begin
  Result := FUsingDesignCamera;
end;

function TCustomForm3D.GetViewportScale: Single;
begin
  Result := Handle.Scale;
end;

function TCustomForm3D.GetWindowStyle: TWindowStyles;
begin
  Result := [TWindowStyle.GPUSurface];
end;

procedure TCustomForm3D.SetCamera(const ACamera: TCamera);
begin
  if FCamera <> ACamera then
  begin
    FCamera := ACamera;
    if (FContext <> nil) and not FUsingDesignCamera and not (csDesigning in ComponentState) then
      NeedRender;
  end;
end;

procedure TCustomForm3D.NeedRender;
begin
  InvalidateRect(RectF(0, 0, FContext.Width, FContext.Height)); // because is a HW context
end;

procedure TCustomForm3D.Realign;
begin
  AlignObjects(Self, Padding, FContext.Width, FContext.Height, FLastWidth, FLastHeight, FDisableAlign);
  InvalidateRect(ClientRect);
end;

end.
