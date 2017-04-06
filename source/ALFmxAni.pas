unit ALFmxAni;

{$IF CompilerVersion > 32} // tokyo
  {$MESSAGE WARN 'Check if FMX.Ani.pas was not updated and adjust the IFDEF'}
{$ENDIF}

interface

uses System.Classes,
     FMX.Types;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALAnimation = class(TObject)
  public const
    DefaultAniFrameRate = 60;
  public class var
    AniFrameRate: Integer;
  private class var
    FAniThread: TTimer;
  private
    FTag: int64;
    [Weak] FTagObject: TObject;
    FTagFloat: Double;
    fOvershoot: Single;
    FTickCount : Integer;
    FDuration: Single;
    FDelay, FDelayTime: Single;
    FTime: Single;
    FInverse: Boolean;
    FSavedInverse: Boolean;
    FLoop: Boolean;
    FPause: Boolean;
    FRunning: Boolean;
    FOnFinish: TNotifyEvent;
    FOnProcess: TNotifyEvent;
    FInterpolation: TInterpolationType;
    FAnimationType: TAnimationType;
    FEnabled: Boolean;
    FAutoReverse: Boolean;
    procedure SetEnabled(const Value: Boolean);
    class procedure Uninitialize;
  protected
    function GetNormalizedTime: Single;
    procedure FirstFrame; virtual;
    procedure ProcessAnimation; virtual; abstract;
    procedure DoProcess; virtual;
    procedure DoFinish; virtual;
  public
    constructor Create; Virtual;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure StopAtCurrent; virtual;
    procedure ProcessTick(time, deltaTime: Single);
    property Running: Boolean read FRunning;
    property Pause: Boolean read FPause write FPause;
    property AnimationType: TAnimationType read FAnimationType write FAnimationType default TAnimationType.In;
    property AutoReverse: Boolean read FAutoReverse write FAutoReverse default False;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Delay: Single read FDelay write FDelay;
    property Duration: Single read FDuration write FDuration nodefault;
    property Interpolation: TInterpolationType read FInterpolation write FInterpolation default TInterpolationType.Linear;
    property Inverse: Boolean read FInverse write FInverse default False;
    property NormalizedTime: Single read GetNormalizedTime;
    property Loop: Boolean read FLoop write FLoop default False;
    property CurrentTime: Single read FTime;
    property OnProcess: TNotifyEvent read FOnProcess write FOnProcess;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property Overshoot: Single read fOvershoot write fOvershoot;
    class property AniThread: TTimer read FAniThread;
    property Tag: int64 read FTag write FTag default 0;
    property TagObject: TObject read FTagObject write FTagObject;
    property TagFloat: Double read FTagFloat write FTagFloat;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFloatAnimation = class(TALAnimation)
  private
    FStartFloat: Double;
    FStopFloat: Double;
    fcurrentFloat: Double;
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create; override;
    procedure Start; override;
    property StartValue: Double read FStartFloat write FStartFloat;
    property StopValue: Double read FStopFloat write FStopFloat;
    property CurrentValue: Double read fcurrentFloat;
  end;

implementation

uses System.SysUtils,
     System.Generics.Collections,
     FMX.Platform,
     FMX.Ani,
     AlCommon;

type

  {**************************}
  TALAniThread = class(TTimer)
  private
    FAniList: TList<TALAnimation>;
    FTime, FDeltaTime: Double;
    FTimerService: IFMXTimerService;
    procedure OneStep;
    procedure DoSyncTimer(Sender: TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AddAnimation(const Ani: TALAnimation);
    procedure RemoveAnimation(const Ani: TALAnimation);
  end;

{******************************}
constructor TALAniThread.Create;
begin
  inherited Create(nil);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    raise EUnsupportedPlatformService.Create('IFMXTimerService');
  if TALAnimation.AniFrameRate < 5 then
    TALAnimation.AniFrameRate := 5;
  if TALAnimation.AniFrameRate > 100 then
    TALAnimation.AniFrameRate := 100;
  Interval := Trunc(1000 / TALAnimation.AniFrameRate / 10) * 10;
  if (Interval <= 0) then Interval := 1;

  OnTimer := DoSyncTimer;
  FAniList := TList<TALAnimation>.Create;
  FTime := FTimerService.GetTick;

  Enabled := False;
end;

{******************************}
destructor TALAniThread.Destroy;
begin
  ALFreeAndNil(FAniList);
  FTimerService := nil;
  inherited;
end;

{***********************************************************}
procedure TALAniThread.AddAnimation(const Ani: TALAnimation);
begin
  if FAniList.IndexOf(Ani) < 0 then
    FAniList.Add(Ani);
  if not Enabled and (FAniList.Count > 0) then
    FTime := FTimerService.GetTick;
  Enabled := FAniList.Count > 0;
end;

{**************************************************************}
procedure TALAniThread.RemoveAnimation(const Ani: TALAnimation);
begin
  FAniList.Remove(Ani);
  Enabled := FAniList.Count > 0;
end;

{**************************************************}
procedure TALAniThread.DoSyncTimer(Sender: TObject);
begin
  OneStep;
  if TALAnimation.AniFrameRate < 5 then
    TALAnimation.AniFrameRate := 5;
  Interval := Trunc(1000 / TALAnimation.AniFrameRate / 10) * 10;
  if (Interval <= 0) then Interval := 1;
end;

{*****************************}
procedure TALAniThread.OneStep;
var
  I: Integer;
  NewTime: Double;
begin
  NewTime := FTimerService.GetTick;
  FDeltaTime := NewTime - FTime;
  FTime := NewTime;
  if FDeltaTime <= 0 then
    Exit;
  if FAniList.Count > 0 then
  begin
    I := FAniList.Count - 1;
    while I >= 0 do
    begin
      if FAniList[I].FRunning then
      begin
        FAniList[I].ProcessTick(FTime, FDeltaTime);
      end;
      dec(I);
      if I >= FAniList.Count then
        I := FAniList.Count - 1;
    end;
  end;
end;

{******************************}
constructor TALAnimation.Create;
begin
  inherited;
  FEnabled := False;
  Duration := 0.2;
  fTag := 0;
  FTagObject := nil;
  FTagFloat := 0.0;
  fOvershoot := 0.0;
end;

{******************************}
destructor TALAnimation.Destroy;
begin
  if AniThread <> nil then
    TALAniThread(AniThread).FAniList.Remove(Self);
  inherited;
end;

{********************************}
procedure TALAnimation.FirstFrame;
begin

end;

{******************************************************}
procedure TALAnimation.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FEnabled then
      Start
    else
      Stop;
  end;
end;

{**********************************************}
function TALAnimation.GetNormalizedTime: Single;
begin
  Result := 0;
  if (FDuration > 0) and (FDelayTime <= 0) then
  begin
    case FInterpolation of
      TInterpolationType.Linear:
        Result := InterpolateLinear(FTime, 0, 1, FDuration);
      TInterpolationType.Quadratic:
        Result := InterpolateQuad(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Cubic:
        Result := InterpolateCubic(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Quartic:
        Result := InterpolateQuart(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Quintic:
        Result := InterpolateQuint(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Sinusoidal:
        Result := InterpolateSine(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Exponential:
        Result := InterpolateExpo(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Circular:
        Result := InterpolateCirc(FTime, 0, 1, FDuration, FAnimationType);
      TInterpolationType.Elastic:
        Result := InterpolateElastic(FTime, 0, 1, FDuration, 0, 0, FAnimationType);
      TInterpolationType.Back:
        Result := InterpolateBack(FTime, 0, 1, FDuration, fOvershoot, FAnimationType);
      TInterpolationType.Bounce:
        Result := InterpolateBounce(FTime, 0, 1, FDuration, FAnimationType);
    end;
  end;
end;

{*******************************}
procedure TALAnimation.DoProcess;
begin
  if fEnabled and Assigned(FOnProcess) then // << i set that if enabled is false then the FOnProcess will not run
    FOnProcess(Self);
end;

{******************************}
procedure TALAnimation.DoFinish;
begin
  if fEnabled and Assigned(FOnFinish) then // << i set that if enabled is false then the FOnFinish will not run
    FOnFinish(Self);
end;

{**********************************************************}
procedure TALAnimation.ProcessTick(time, deltaTime: Single);
begin
  inherited;

  if (not FRunning) or FPause then
    Exit;

  if (FDelay > 0) and (FDelayTime <> 0) then
  begin
    if FDelayTime > 0 then
    begin
      FDelayTime := FDelayTime - deltaTime;
      if FDelayTime <= 0 then
      begin
        FDelayTime := 0;
        if FInverse then
          FTime := FDuration
        else
          FTime := 0;
        FirstFrame;
        ProcessAnimation;
        DoProcess;
      end;
    end;
    Exit;
  end;

  if FInverse then
    FTime := FTime - deltaTime
  else
    FTime := FTime + deltaTime;
  if FTime >= FDuration then
  begin
    FTime := FDuration;
    if FLoop then
    begin
      if FAutoReverse then
      begin
        FInverse := True;
        FTime := FDuration;
      end
      else
        FTime := 0;
    end
    else
      if FAutoReverse and (FTickCount = 0) then
      begin
        Inc(FTickCount);
        FInverse := True;
        FTime := FDuration;
      end
      else
        FRunning := False;
  end
  else if FTime <= 0 then
  begin
    FTime := 0;
    if FLoop then
    begin
      if FAutoReverse then
      begin
        FInverse := False;
        FTime := 0;
      end
      else
        FTime := FDuration;
    end
    else
      if FAutoReverse and (FTickCount = 0) then
      begin
        Inc(FTickCount);
        FInverse := False;
        FTime := 0;
      end
      else
        FRunning := False;
  end;

  ProcessAnimation;
  DoProcess;

  if not FRunning then
  begin
    if AutoReverse then
      FInverse := FSavedInverse;
    if AniThread <> nil then
      TALAniThread(AniThread).RemoveAnimation(Self);
    DoFinish;
  end;
end;

{***************************}
procedure TALAnimation.Start;
var
  SaveDuration: Single;
begin
  if not FLoop then
    FTickCount := 0;
  if AutoReverse then
  begin
    if Running then
      FInverse := FSavedInverse
    else
      FSavedInverse := FInverse;
  end;
  if (Abs(FDuration) < 0.001) then
  begin
    { immediate animation }
    SaveDuration := FDuration;
    try
      FDelayTime := 0;
      FDuration := 1;
      if FInverse then
        FTime := 0
      else
        FTime := FDuration;
      FRunning := True;
      ProcessAnimation;
      DoProcess;
      FRunning := False;
      FTime := 0;
      DoFinish;
    finally
      FDuration := SaveDuration;
    end;
  end
  else
  begin
    FDelayTime := FDelay;
    FRunning := True;
    if FInverse then
      FTime := FDuration
    else
      FTime := 0;
    if FDelay = 0 then
    begin
      FirstFrame;
      ProcessAnimation;
      DoProcess;
    end;

    if AniThread = nil then
      FAniThread := TALAniThread.Create;

    TALAniThread(AniThread).AddAnimation(Self);
    if not AniThread.Enabled then
      Stop
    else
      FEnabled := True;
  end;
end;

{**************************}
procedure TALAnimation.Stop;
begin
  if not FRunning then
    Exit;

  if AniThread <> nil then
    TALAniThread(AniThread).RemoveAnimation(Self);

  if AutoReverse then
    FInverse := FSavedInverse;

  if FInverse then
    FTime := 0
  else
    FTime := FDuration;
  ProcessAnimation;
  DoProcess;
  FRunning := False;
  DoFinish;
end;

{***********************************}
procedure TALAnimation.StopAtCurrent;
begin
  if not FRunning then
    Exit;

  if AniThread <> nil then
    TALAniThread(AniThread).RemoveAnimation(Self);

  if AutoReverse then
    FInverse := FSavedInverse;

  if FInverse then
    FTime := 0
  else
    FTime := FDuration;
  FRunning := False;
  FEnabled := False;
  DoFinish;
end;

{****************************************}
class procedure TALAnimation.Uninitialize;
begin
  ALFreeAndNil(FAniThread);
end;

{***********************************}
constructor TALFloatAnimation.Create;
begin
  inherited;
  Duration := 0.2;
  FStartFloat := 0;
  FStopFloat := 0;
  fCurrentFloat := 0;
end;

{********************************}
procedure TALFloatAnimation.Start;
begin
  fCurrentFloat := FStartFloat;
  inherited Start;
end;

{*******************************************}
procedure TALFloatAnimation.ProcessAnimation;
begin
  fCurrentFloat := FStartFloat + (FStopFloat - FStartFloat) * NormalizedTime;
end;

{************}
initialization
  TALAnimation.AniFrameRate := TALAnimation.DefaultAniFrameRate;

{**********}
finalization
  TALAnimation.Uninitialize;

end.

