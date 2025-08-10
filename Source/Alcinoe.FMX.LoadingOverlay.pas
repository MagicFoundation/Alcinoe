unit Alcinoe.FMX.LoadingOverlay;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.Generics.Collections,
  System.Types,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.NativeControl;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // TALLoadingOverlay is the root control of the LoadingOverlay system.
  // It represents the scrim that overlays the entire form,
  // blocking all touch events on the underlying UI.
  TALLoadingOverlay = class(TALRectangle)
  public
    type
      // --------------
      // TAnimateOption
      TAnimateOption = (AnimateScrim, AnimateContainer);
      // ---------------
      // TAnimateOptions
      TAnimateOptions = set of TAnimateOption;
      // --------
      // TBuilder
      TBuilder = Class(TObject)
      private
        FLoadingOverlay: TALLoadingOverlay;
      public
        constructor Create;
        destructor Destroy; override;
        function SetStealthMode: TBuilder;
        function SetAnimatedImageResourceName(const AValue: String): TBuilder;
        function SetAnimatedImageTintColor(const AValue: TalphaColor): TBuilder;
        function SetAniIndicatorResourceName(const AValue: String): TBuilder;
        function SetAniIndicatorTintColor(const AValue: TalphaColor): TBuilder;
        function SetScrimColor(const AValue: TalphaColor): TBuilder;
        function SetContainerSize(const AWidth, AHeight: Single): TBuilder;
        function SetContainerPadding(const AValue: TRectF): TBuilder;
        function SetContainerFillColor(const AValue: TalphaColor): TBuilder;
        function SetContainerStrokeColor(const AValue: TalphaColor): TBuilder;
        function SetContainerStrokeThickness(const AValue: Single): TBuilder;
        function SetContainerShadowColor(const AValue: TalphaColor): TBuilder;
        function SetContainerShadowBlur(const AValue: Single): TBuilder;
        function SetContainerShadowOffsetX(const AValue: Single): TBuilder;
        function SetContainerShadowOffsetY(const AValue: Single): TBuilder;
        /// <summary>
        ///   The Builder instance will be released during this operation.
        /// </summary>
        procedure Show;
        property LoadingOverlay: TALLoadingOverlay read FLoadingOverlay;
      end;
  public
    /// <summary>
    ///   Creates a builder object. The builder will automatically be
    ///   released when calling the Show method.
    ///   If you do not call Show, you are responsible for releasing the builder manually.
    /// </summary>
    class function Builder: TBuilder;
  private
    FContainer: TALRectangle;
    FAnimatedImage: TALAnimatedImage;
    FAniIndicator: TALAniIndicator;
    FShowAnimateOptions: TAnimateOptions;
    FCloseAnimateOptions: TAnimateOptions;
    FOnClosedRefProc: TProc;
    function GetAnimatedImage: TALAnimatedImage;
    function GetAniIndicator: TALAniIndicator;
  public
    constructor Create(AOwner: TComponent); override;
    class function Current: TALLoadingOverlay;
    class procedure CloseCurrent(const AOnClosedCallback: TProc = nil; const AAnimateOptions: TAnimateOptions = [TAnimateOption.AnimateScrim, TAnimateOption.AnimateContainer]);
    function IsStealthMode: Boolean;
    function HasAnimatedImage: Boolean;
    function HasAniIndicator: Boolean;
    property Container: TALRectangle read FContainer;
    property AnimatedImage: TALAnimatedImage read GetAnimatedImage;
    property AniIndicator: TALAniIndicator read GetAniIndicator;
    property ShowAnimateOptions: TAnimateOptions read FShowAnimateOptions write FShowAnimateOptions;
    property CloseAnimateOptions: TAnimateOptions read FCloseAnimateOptions write FCloseAnimateOptions;
    property OnClosedRefProc: TProc read FOnClosedRefProc write FOnClosedRefProc;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALLoadingOverlayManager = class
  private
    class function CreateInstance: TALLoadingOverlayManager;
    class function GetInstance: TALLoadingOverlayManager; static;
  protected
    class var FInstance: TALLoadingOverlayManager;
  public
    type
      TCreateInstanceFunc = function: TALLoadingOverlayManager;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALLoadingOverlayManager read GetInstance;
    class function HasInstance: Boolean; inline;
  private
    FDefaultScrim: TALRectangle;
    FDefaultContainer: TALRectangle;
    FDefaultAnimatedImage: TALAnimatedImage;
    FDefaultAniIndicator: TALAniIndicator;
    FCurrentLoadingOverlay: TALLoadingOverlay;
    FScrimAnimation: TALFloatAnimation;
    FContainerAnimation: TALFloatAnimation;
    FQueue: TQueue<TALLoadingOverlay>;
    FFrozenNativeControls: TArray<TALNativeControl>;
  protected
    procedure ScrimAnimationProcess(Sender: TObject);
    procedure ContainerAnimationProcess(Sender: TObject);
    procedure ScrimAnimationFinish(Sender: TObject);
    procedure ContainerAnimationFinish(Sender: TObject);
    Function HasPendingLoadingOverlays: Boolean;
    procedure ProcessPendingLoadingOverlays;
    procedure ShowLoadingOverlay(const ALoadingOverlay: TALLoadingOverlay);
    procedure DoCloseCurrentLoadingOverlay;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure RequestLoadingOverlay(const ALoadingOverlay: TALLoadingOverlay);
    property DefaultScrim: TALRectangle read FDefaultScrim;
    property DefaultContainer: TALRectangle read FDefaultContainer;
    property DefaultAnimatedImage: TALAnimatedImage read FDefaultAnimatedImage;
    property DefaultAniIndicator: TALAniIndicator read FDefaultAniIndicator;
    property CurrentLoadingOverlay: TALLoadingOverlay read fCurrentLoadingOverlay;
    function IsShowingLoadingOverlay: Boolean;
    procedure CloseCurrentLoadingOverlay;
  end;

implementation

uses
  Fmx.Controls,
  FMX.Forms,
  FMX.Types,
  Alcinoe.fmx.Styles,
  Alcinoe.FMX.Graphics,
  Alcinoe.Common;

{********************************************}
constructor TALLoadingOverlay.TBuilder.Create;
begin
  Inherited create;
  FLoadingOverlay := TALLoadingOverlay.Create(nil);
  FLoadingOverlay.BeginUpdate;
end;

{********************************************}
destructor TALLoadingOverlay.TBuilder.Destroy;
begin
  ALFreeAndNil(FLoadingOverlay);
  inherited;
end;

{***********************************************************}
function TALLoadingOverlay.TBuilder.SetStealthMode: TBuilder;
begin
  FLoadingOverlay.Container.Visible := False;
  FLoadingOverlay.Fill.Color := TalphaColors.Null;
  Result := Self;
end;

{***********************************************************************************************}
function TALLoadingOverlay.TBuilder.SetAnimatedImageResourceName(const AValue: String): TBuilder;
begin
  FLoadingOverlay.AnimatedImage.ResourceName := AValue;
  Result := Self;
end;

{*************************************************************************************************}
function TALLoadingOverlay.TBuilder.SetAnimatedImageTintColor(const AValue: TalphaColor): TBuilder;
begin
  FLoadingOverlay.AnimatedImage.TintColor := AValue;
  Result := Self;
end;

{**********************************************************************************************}
function TALLoadingOverlay.TBuilder.SetAniIndicatorResourceName(const AValue: String): TBuilder;
begin
  FLoadingOverlay.AniIndicator.ResourceName := AValue;
  Result := Self;
end;

{************************************************************************************************}
function TALLoadingOverlay.TBuilder.SetAniIndicatorTintColor(const AValue: TalphaColor): TBuilder;
begin
  FLoadingOverlay.AniIndicator.TintColor := AValue;
  Result := Self;
end;

{*************************************************************************************}
function TALLoadingOverlay.TBuilder.SetScrimColor(const AValue: TalphaColor): TBuilder;
begin
  FLoadingOverlay.Fill.Color := AValue;
  Result := Self;
end;

{********************************************************************************************}
function TALLoadingOverlay.TBuilder.SetContainerSize(const AWidth, AHeight: Single): TBuilder;
begin
  FLoadingOverlay.Container.Size.Size := TSizeF.Create(AWidth, AHeight);
  Result := Self;
end;

{**************************************************************************************}
function TALLoadingOverlay.TBuilder.SetContainerPadding(const AValue: TRectF): TBuilder;
begin
  FLoadingOverlay.Container.Padding.Rect := AValue;
  Result := Self;
end;

{*********************************************************************************************}
function TALLoadingOverlay.TBuilder.SetContainerFillColor(const AValue: TalphaColor): TBuilder;
begin
  FLoadingOverlay.Container.Fill.Color := AValue;
  Result := Self;
end;

{***********************************************************************************************}
function TALLoadingOverlay.TBuilder.SetContainerStrokeColor(const AValue: TalphaColor): TBuilder;
begin
  FLoadingOverlay.Container.Stroke.Color := AValue;
  Result := Self;
end;

{**********************************************************************************************}
function TALLoadingOverlay.TBuilder.SetContainerStrokeThickness(const AValue: Single): TBuilder;
begin
  FLoadingOverlay.Container.Stroke.Thickness := AValue;
  Result := Self;
end;

{***********************************************************************************************}
function TALLoadingOverlay.TBuilder.SetContainerShadowColor(const AValue: TalphaColor): TBuilder;
begin
  FLoadingOverlay.Container.Shadow.Color := AValue;
  Result := Self;
end;

{*****************************************************************************************}
function TALLoadingOverlay.TBuilder.SetContainerShadowBlur(const AValue: Single): TBuilder;
begin
  FLoadingOverlay.Container.Shadow.Blur := AValue;
  Result := Self;
end;

{********************************************************************************************}
function TALLoadingOverlay.TBuilder.SetContainerShadowOffsetX(const AValue: Single): TBuilder;
begin
  FLoadingOverlay.Container.Shadow.OffsetX := AValue;
  Result := Self;
end;

{********************************************************************************************}
function TALLoadingOverlay.TBuilder.SetContainerShadowOffsetY(const AValue: Single): TBuilder;
begin
  FLoadingOverlay.Container.Shadow.OffsetY := AValue;
  Result := Self;
end;

{****************************************}
procedure TALLoadingOverlay.TBuilder.Show;
begin
  TALLoadingOverlayManager.Instance.RequestLoadingOverlay(FLoadingOverlay);
  FLoadingOverlay := nil;
  Free;
end;

{*************************************************}
class function TALLoadingOverlay.Builder: TBuilder;
begin
  Result := TBuilder.Create;
end;

{*******************************************************}
constructor TALLoadingOverlay.Create(AOwner: TComponent);
begin
  inherited;
  //--
  FContainer := TALRectangle.Create(Self);
  FContainer.Parent := Self;
  FContainer.Assign(TALLoadingOverlayManager.Instance.DefaultContainer);
  FContainer.Name := 'ALLoadingOverlayContainer';
  //--
  FAnimatedImage := nil;
  FAniIndicator := nil;
  FShowAnimateOptions := [TAnimateOption.AnimateScrim, TAnimateOption.AnimateContainer];
  FCloseAnimateOptions := [TAnimateOption.AnimateScrim, TAnimateOption.AnimateContainer];
  FOnClosedRefProc := nil;
  //--
  Assign(TALLoadingOverlayManager.Instance.DefaultScrim);
  Name := 'ALLoadingOverlayScrim';
end;

{**********************************************************}
class function TALLoadingOverlay.Current: TALLoadingOverlay;
begin
  Result := TALLoadingOverlayManager.Instance.CurrentLoadingOverlay;
end;

{********************************************************************************************************************************************************************************************}
class procedure TALLoadingOverlay.CloseCurrent(const AOnClosedCallback: TProc = nil; const AAnimateOptions: TAnimateOptions = [TAnimateOption.AnimateScrim, TAnimateOption.AnimateContainer]);
begin
  var LCurrentLoadingOverlay := TALLoadingOverlayManager.Instance.CurrentLoadingOverlay;
  if LCurrentLoadingOverlay <> nil then begin
    LCurrentLoadingOverlay.OnClosedRefProc := AOnClosedCallback;
    LCurrentLoadingOverlay.CloseAnimateOptions := AAnimateOptions;
    TALLoadingOverlayManager.Instance.CloseCurrentLoadingOverlay;
  end;
end;

{************************************************************}
function TALLoadingOverlay.GetAnimatedImage: TALAnimatedImage;
begin
  If FAnimatedImage = nil then begin
    FAnimatedImage := TALAnimatedImage.Create(Container);
    FAnimatedImage.Parent := Container;
    FAnimatedImage.Assign(TALLoadingOverlayManager.Instance.DefaultAnimatedImage);
    FAnimatedImage.Name := 'ALLoadingOverlayAnimatedImage';
  end;
  Result := FAnimatedImage;
end;

{**********************************************************}
function TALLoadingOverlay.GetAniIndicator: TALAniIndicator;
begin
  If FAniIndicator = nil then begin
    FAniIndicator := TALAniIndicator.Create(Container);
    FAniIndicator.Parent := Container;
    FAniIndicator.Assign(TALLoadingOverlayManager.Instance.DefaultAniIndicator);
    FAniIndicator.Name := 'ALLoadingOverlayAniIndicator';
  end;
  Result := FAniIndicator;
end;

{************************************************}
function TALLoadingOverlay.IsStealthMode: Boolean;
begin
  Result := (not FContainer.Visible) and
            (Fill.Color = TalphaColors.Null);
end;

{***************************************************}
function TALLoadingOverlay.HasAnimatedImage: Boolean;
begin
  result := FAnimatedImage <> nil;
end;

{**************************************************}
function TALLoadingOverlay.HasAniIndicator: Boolean;
begin
  result := FAniIndicator <> nil;
end;

{******************************************}
constructor TALLoadingOverlayManager.Create;
begin
  inherited;
  FDefaultScrim := TALRectangle.Create(nil);
  FDefaultContainer := TALRectangle.Create(nil);
  FDefaultAnimatedImage := TALAnimatedImage.Create(nil);
  FDefaultAniIndicator := TALAniIndicator.Create(nil);
  FCurrentLoadingOverlay := Nil;
  FScrimAnimation := TALFloatAnimation.Create;
  FScrimAnimation.OnProcess := ScrimAnimationProcess;
  FScrimAnimation.OnFinish := ScrimAnimationFinish;
  FContainerAnimation := TALFloatAnimation.Create;
  FContainerAnimation.OnProcess := ContainerAnimationProcess;
  FContainerAnimation.OnFinish := ContainerAnimationFinish;
  FQueue := TQueue<TALLoadingOverlay>.create;
  Setlength(FFrozenNativeControls, 0);
end;

{******************************************}
destructor TALLoadingOverlayManager.Destroy;
begin
  AlFreeAndNil(FDefaultScrim);
  AlFreeAndNil(FDefaultContainer);
  AlFreeAndNil(FDefaultAnimatedImage);
  AlFreeAndNil(FDefaultAniIndicator);
  ALFreeAndNil(FScrimAnimation);
  ALFreeAndNil(FContainerAnimation);
  ALFreeAndNil(FQueue);
  inherited;
end;

{***************************************************}
procedure TALLoadingOverlayManager.AfterConstruction;
begin
  inherited;
  TALStyleManager.Instance.ApplyLoadingOverlayManagerStyle('Default', Self);
end;

{*******************************************************************************}
class function TALLoadingOverlayManager.CreateInstance: TALLoadingOverlayManager;
begin
  result := TALLoadingOverlayManager.Create;
end;

{*************}
//[MultiThread]
class function TALLoadingOverlayManager.GetInstance: TALLoadingOverlayManager;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance)
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALLoadingOverlayManager.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{*****************************************************************}
function TALLoadingOverlayManager.IsShowingLoadingOverlay: Boolean;
begin
  Result := FCurrentLoadingOverlay <> nil;
end;

{************************************************************}
procedure TALLoadingOverlayManager.CloseCurrentLoadingOverlay;
begin
  if FCurrentLoadingOverlay = nil then exit;

  If (FCurrentLoadingOverlay.IsStealthMode) or
     (not (TALLoadingOverlay.TAnimateOption.AnimateContainer in FCurrentLoadingOverlay.CloseAnimateOptions)) then begin
    DoCloseCurrentLoadingOverlay;
    exit;
  end;

  FScrimAnimation.Enabled := False;
  if (not HasPendingLoadingOverlays) and
     (TALLoadingOverlay.TAnimateOption.AnimateScrim in FCurrentLoadingOverlay.CloseAnimateOptions) then begin
    FScrimAnimation.TagFloat := TAlphaColorRec(FCurrentLoadingOverlay.Fill.Color).A / 255;
    FScrimAnimation.InterpolationType := TALInterpolationType.Linear;
    FScrimAnimation.Duration := 0.200;
    FScrimAnimation.StartValue := 1;
    FScrimAnimation.StopValue := 0;
    FScrimAnimation.Start;
  end;

  FContainerAnimation.Enabled := False;
  FContainerAnimation.TagFloat := FCurrentLoadingOverlay.Container.Position.y;
  FContainerAnimation.InterpolationType := TALInterpolationType.Material3EmphasizedAccelerate;
  FContainerAnimation.Duration := 0.200;
  FContainerAnimation.StartValue := 1;
  FContainerAnimation.StopValue := 0;
  FContainerAnimation.Start;
end;

{**************************************************************}
procedure TALLoadingOverlayManager.DoCloseCurrentLoadingOverlay;
begin
  if FCurrentLoadingOverlay = nil then exit;
  var LCurrentLoadingOverlay := FCurrentLoadingOverlay;
  LCurrentLoadingOverlay.Visible := False;
  FCurrentLoadingOverlay := nil;
  FScrimAnimation.Enabled := False;
  FContainerAnimation.Enabled := False;
  if assigned(LCurrentLoadingOverlay.FOnClosedRefProc) then
    LCurrentLoadingOverlay.FOnClosedRefProc();
  ProcessPendingLoadingOverlays;
  if not IsShowingLoadingOverlay then
    ALUnfreezeNativeViews(FFrozenNativeControls)
  else if not LCurrentLoadingOverlay.IsStealthMode then
    FScrimAnimation.Stop;
  TThread.ForceQueue(nil,
    procedure
    begin
      ALFreeAndNil(LCurrentLoadingOverlay);
    end);
end;

{*************}
//[MultiThread]
procedure TALLoadingOverlayManager.RequestLoadingOverlay(const ALoadingOverlay: TALLoadingOverlay);
begin
  TThread.queue(nil,
    procedure
    begin
      if TALLoadingOverlayManager.HasInstance then begin
        with TALLoadingOverlayManager.Instance do begin
          FQueue.Enqueue(ALoadingOverlay);
          if IsShowingLoadingOverlay then
            CloseCurrentLoadingOverlay
          else
            ProcessPendingLoadingOverlays;
        end;
      end
      else
        ALFreeAndNil(ALoadingOverlay);
    end);
end;

{*******************************************************************}
Function TALLoadingOverlayManager.HasPendingLoadingOverlays: Boolean;
begin
  Result := FQueue.Count > 0;
end;

{***************************************************************}
procedure TALLoadingOverlayManager.ProcessPendingLoadingOverlays;
begin
  if (not IsShowingLoadingOverlay) and
     (HasPendingLoadingOverlays) then begin
    var LLoadingOverlay := FQueue.Dequeue;
    try
      ShowLoadingOverlay(LLoadingOverlay);
    Except
      ALFreeAndNil(LLoadingOverlay);
      Raise;
    end;
  end;
end;

{**********************************************************************************************}
procedure TALLoadingOverlayManager.ShowLoadingOverlay(const ALoadingOverlay: TALLoadingOverlay);
begin

  // Initialize LForm
  Var LForm := Screen.ActiveForm;
  if LForm = nil then LForm := Application.MainForm;
  if LForm = nil then Raise Exception.Create('Error 0B1C5551-F59D-46FA-8E9B-A10AB6A65FDE');

  // by default show AnimatedImage
  if (not ALoadingOverlay.HasAnimatedImage) and
     (not ALoadingOverlay.HasAniIndicator) and
     (ALoadingOverlay.Container.Visible) then begin
    ALoadingOverlay.GetAnimatedImage;
  end;

  // Layout the LoadingOverlay
  LForm.Focused := nil;
  ALFreezeNativeViews(FFrozenNativeControls);
  ALoadingOverlay.Align := TALAlignLayout.Contents;
  // This will defacto call ALoadingOverlay.EndUpdate
  // in TCustomForm.DoAddObject.SetUpdatingState
  LForm.InsertComponent(ALoadingOverlay);
  ALoadingOverlay.Parent := LForm;

  // Init FCurrentLoadingOverlay
  FCurrentLoadingOverlay := ALoadingOverlay;

  // No animation in Stealth Mode
  if (not FCurrentLoadingOverlay.IsStealthMode) and
     (TALLoadingOverlay.TAnimateOption.AnimateContainer in FCurrentLoadingOverlay.ShowAnimateOptions) then begin

    // Start the ScrimAnimation
    if (TALLoadingOverlay.TAnimateOption.AnimateScrim in FCurrentLoadingOverlay.ShowAnimateOptions) then begin
      FScrimAnimation.Enabled := False;
      FScrimAnimation.TagFloat := TAlphaColorRec(FCurrentLoadingOverlay.Fill.Color).A / 255;
      FScrimAnimation.InterpolationType := TALInterpolationType.Linear;
      FScrimAnimation.Duration := 0.4;
      FScrimAnimation.StartValue := 0;
      FScrimAnimation.StopValue := 1;
      FScrimAnimation.Start;
    end;

    // Start the ContainerAnimation
    FContainerAnimation.Enabled := False;
    FContainerAnimation.TagFloat := 0;
    FContainerAnimation.InterpolationType := TALInterpolationType.Material3ExpressiveDefaultSpatial;
    FContainerAnimation.Duration := 0.5;
    FContainerAnimation.StartValue := 0;
    FContainerAnimation.StopValue := 1;
    FContainerAnimation.Start;

  end;

end;

{************************************************************************}
procedure TALLoadingOverlayManager.ScrimAnimationProcess(Sender: TObject);
begin
  if FCurrentLoadingOverlay = nil then exit;
  FCurrentLoadingOverlay.Fill.Color := ALSetColorAlpha(FCurrentLoadingOverlay.Fill.Color, FScrimAnimation.CurrentValue * FScrimAnimation.TagFloat);
end;

{****************************************************************************}
procedure TALLoadingOverlayManager.ContainerAnimationProcess(Sender: TObject);
begin
  if FCurrentLoadingOverlay = nil then exit;
  FCurrentLoadingOverlay.Container.Scale.Y := FContainerAnimation.CurrentValue;
end;

{***********************************************************************}
procedure TALLoadingOverlayManager.ScrimAnimationFinish(Sender: TObject);
begin
  // Nothing to do
end;

{***************************************************************************}
procedure TALLoadingOverlayManager.ContainerAnimationFinish(Sender: TObject);
begin
  if FCurrentLoadingOverlay = nil then exit;
  if FContainerAnimation.StopValue < 0.5 then DoCloseCurrentLoadingOverlay;
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.LoadingOverlays','initialization');
  {$ENDIF}
  TALLoadingOverlayManager.FInstance := nil;
  TALLoadingOverlayManager.CreateInstanceFunc := @TALLoadingOverlayManager.CreateInstance;

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.LoadingOverlays','finalization');
  {$ENDIF}
  ALFreeAndNil(TALLoadingOverlayManager.FInstance);

end.
