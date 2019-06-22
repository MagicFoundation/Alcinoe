unit ALFmxAni;

{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if FMX.Ani.pas was not updated and adjust the IFDEF'}
{$ENDIF}

interface

uses System.Classes,
     System.SyncObjs,
     System.Rtti,
     System.Generics.Collections,
     System.UITypes,
     {$IFDEF IOS}
     System.TypInfo,
     Macapi.ObjectiveC,
     iOSapi.Foundation,
     iOSapi.QuartzCore,
     {$ENDIF}
     {$IFDEF ANDROID}
     Androidapi.JNIBridge,
     ALAndroidApi,
     {$ENDIF}
     FMX.Types;

type

  {~~~~~~~~~~~~~~~~~~~}
  TALAnimation = Class;

  {~~~~~~~~~~~~~~}
  {$IFDEF ANDROID}
  TALChoreographerThread = class(TObject)
  private type
    TChoreographerFrameCallback = class(TJavaLocal, JChoreographer_FrameCallback)
    private
      [Weak] fChoreographerThread: TALChoreographerThread;
    public
      constructor Create(const aAniCalculations: TALChoreographerThread);
      procedure doFrame(frameTimeNanos: Int64); cdecl;
    end;
  private
    FChoreographer: JChoreographer;
    FChoreographerFrameCallback: TChoreographerFrameCallback;
  private
    FTimerEvent: TNotifyEvent;
    FInterval: Cardinal;
    FEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    property Interval: Cardinal read FInterval write SetInterval;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnTimer: TNotifyEvent read FTimerEvent write FTimerEvent;
  end;
  {$ENDIF}

  {~~~~~~~~~~}
  {$IFDEF IOS}
  TALDisplayLinkThread = class(TObject)
  private type

    IDisplayLinkListener = interface(NSObject)
    ['{810AD3F0-265C-4A73-9B96-74103268884A}']
      procedure displayLinkUpdated; cdecl;
    end;

    TDisplayLinkListener = class(TOCLocal)
    private
      [Weak] fDisplayLinkThread: TALDisplayLinkThread;
    protected
      function GetObjectiveCClass: PTypeInfo; override;
    public
      constructor Create(const aDisplayLinkThread: TALDisplayLinkThread);
      procedure displayLinkUpdated; cdecl;
    end;

  private
    fDisplayLink: CADisplayLink;
    fDisplayLinkListener: TDisplayLinkListener;
  private
    FTimerEvent: TNotifyEvent;
    FInterval: Cardinal;
    FEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    property Interval: Cardinal read FInterval write SetInterval;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnTimer: TNotifyEvent read FTimerEvent write FTimerEvent;
  end;
  {$ENDIF}


  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALAniThread = class({$IF defined(ANDROID)}TALChoreographerThread{$ELSEIF defined(IOS)}TALDisplayLinkThread{$ELSE}TTimer{$ENDIF})
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
    Procedure WakeUpTimer;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALAnimation = class(TObject)
  public const
    DefaultAniFrameRate = 60;
  public class var
    AniFrameRate: Integer;
  private class var
    FAniThread: TALAniThread;
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
    FOnFirstFrame: TNotifyEvent;
    FOnProcess: TNotifyEvent;
    FOnFinish: TNotifyEvent;
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
    class Procedure WakeUpTimer;
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
    property OnFirstFrame: TNotifyEvent read FOnFirstFrame write FOnFirstFrame;
    property OnProcess: TNotifyEvent read FOnProcess write FOnProcess;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property Overshoot: Single read fOvershoot write fOvershoot;
    class property AniThread: TALAniThread read FAniThread;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALColorAnimation = class(TALAnimation)
  private
    FStartColor: TAlphaColor;
    FStopColor: TAlphaColor;
    fcurrentColor: TAlphaColor;
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create; override;
    procedure Start; override;
    property StartValue: TAlphaColor read FStartColor write FStartColor;
    property StopValue: TAlphaColor read FStopColor write FStopColor;
    property CurrentValue: TAlphaColor read fcurrentColor;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCustomPropertyAnimation = class(TFmxObject)
  private
  protected
    FInstance: TObject;
    FRttiProperty: TRttiProperty;
    FPath: string;
    FPropertyName: string;
    procedure SetPropertyName(const AValue: string);
    function FindProperty: Boolean;
    procedure ParentChanged; override;
  public
    property PropertyName: string read FPropertyName write SetPropertyName;
    procedure Stop; virtual;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFloatPropertyAnimation = class(TALCustomPropertyAnimation)
  private
    FStartFromCurrent: Boolean;
    fFloatAnimation: TALFloatAnimation;
    FOnFirstFrame: TNotifyEvent;
    FOnProcess: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    function getAnimationType: TAnimationType;
    function getAutoReverse: Boolean;
    function getDelay: Single;
    function getDuration: Single;
    function getEnabled: Boolean;
    function getInterpolation: TInterpolationType;
    function getInverse: Boolean;
    function getLoop: Boolean;
    function getOvershoot: Single;
    function getPause: Boolean;
    function getRunning: Boolean;
    function GetStartValue: Single;
    function GetStopValue: Single;
    function OvershootStored: Boolean;
    procedure setAnimationType(const Value: TAnimationType);
    procedure setAutoReverse(const Value: Boolean);
    procedure setDelay(const Value: Single);
    procedure setDuration(const Value: Single);
    procedure SetEnabled(const Value: Boolean);
    procedure setInterpolation(const Value: TInterpolationType);
    procedure setInverse(const Value: Boolean);
    procedure setLoop(const Value: Boolean);
    procedure setOvershoot(const Value: Single);
    procedure setPause(const Value: Boolean);
    procedure SetStartValue(const Value: Single);
    procedure setStopValue(const Value: Single);
  protected
    procedure doFirstFrame(Sender: TObject);
    procedure doProcess(Sender: TObject);
    procedure doFinish(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; override;
    procedure StopAtCurrent; virtual;
    property Running: Boolean read getRunning;
    property Pause: Boolean read getPause write setPause;
  published
    property AnimationType: TAnimationType read getAnimationType write setAnimationType default TAnimationType.In;
    property AutoReverse: Boolean read getAutoReverse write setAutoReverse default False;
    property Enabled: Boolean read getEnabled write SetEnabled default False;
    property Delay: Single read getDelay write setDelay;
    property Duration: Single read getDuration write setDuration nodefault;
    property Interpolation: TInterpolationType read getInterpolation write setInterpolation default TInterpolationType.Linear;
    property Inverse: Boolean read getInverse write setInverse default False;
    property Loop: Boolean read getLoop write setLoop default False;
    property OnFirstFrame: TNotifyEvent read fOnFirstFrame write fOnFirstFrame;
    property OnProcess: TNotifyEvent read fOnProcess write fOnProcess;
    property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
    property PropertyName;
    property StartValue: Single read GetStartValue write SetStartValue stored True nodefault;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopValue: Single read GetStopValue write setStopValue stored True nodefault;
    property Overshoot: Single read getOvershoot write setOvershoot Stored OvershootStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALColorPropertyAnimation = class(TALCustomPropertyAnimation)
  private
    FStartFromCurrent: Boolean;
    fColorAnimation: TALColorAnimation;
    FOnFirstFrame: TNotifyEvent;
    FOnProcess: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    function getAnimationType: TAnimationType;
    function getAutoReverse: Boolean;
    function getDelay: Single;
    function getDuration: Single;
    function getEnabled: Boolean;
    function getInterpolation: TInterpolationType;
    function getInverse: Boolean;
    function getLoop: Boolean;
    function getOvershoot: Single;
    function getPause: Boolean;
    function getRunning: Boolean;
    function GetStartValue: TAlphaColor;
    function GetStopValue: TAlphaColor;
    function OvershootStored: Boolean;
    procedure setAnimationType(const Value: TAnimationType);
    procedure setAutoReverse(const Value: Boolean);
    procedure setDelay(const Value: Single);
    procedure setDuration(const Value: Single);
    procedure SetEnabled(const Value: Boolean);
    procedure setInterpolation(const Value: TInterpolationType);
    procedure setInverse(const Value: Boolean);
    procedure setLoop(const Value: Boolean);
    procedure setOvershoot(const Value: Single);
    procedure setPause(const Value: Boolean);
    procedure SetStartValue(const Value: TAlphaColor);
    procedure setStopValue(const Value: TAlphaColor);
  protected
    procedure doFirstFrame(Sender: TObject);
    procedure doProcess(Sender: TObject);
    procedure doFinish(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; override;
    procedure StopAtCurrent; virtual;
    property Running: Boolean read getRunning;
    property Pause: Boolean read getPause write setPause;
  published
    property AnimationType: TAnimationType read getAnimationType write setAnimationType default TAnimationType.In;
    property AutoReverse: Boolean read getAutoReverse write setAutoReverse default False;
    property Enabled: Boolean read getEnabled write SetEnabled default False;
    property Delay: Single read getDelay write setDelay;
    property Duration: Single read getDuration write setDuration nodefault;
    property Interpolation: TInterpolationType read getInterpolation write setInterpolation default TInterpolationType.Linear;
    property Inverse: Boolean read getInverse write setInverse default False;
    property Loop: Boolean read getLoop write setLoop default False;
    property OnFirstFrame: TNotifyEvent read fOnFirstFrame write fOnFirstFrame;
    property OnProcess: TNotifyEvent read fOnProcess write fOnProcess;
    property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
    property PropertyName;
    property StartValue: TAlphaColor read GetStartValue write SetStartValue stored True nodefault;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopValue: TAlphaColor read GetStopValue write setStopValue stored True nodefault;
    property Overshoot: Single read getOvershoot write setOvershoot Stored OvershootStored;
  end;

procedure Register;

implementation

uses System.SysUtils,
     System.math,
     {$IFDEF IOS}
     Macapi.ObjCRuntime,
     {$ENDIF}
     FMX.Platform,
     FMX.Ani,
     FMX.Utils,
     ALString,
     AlCommon;

{$IFDEF ANDROID}

{********************************************************************************************************************}
constructor TALChoreographerThread.TChoreographerFrameCallback.Create(const aAniCalculations: TALChoreographerThread);
 begin
  inherited Create;
  fChoreographerThread := aAniCalculations;
end;

{******************************************************************************************}
procedure TALChoreographerThread.TChoreographerFrameCallback.doFrame(frameTimeNanos: Int64);
begin

  {$IFDEF DEBUG}
  //ALLog('TALChoreographerThread.TChoreographerFrameCallback.doFrame', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  if assigned(fChoreographerThread.FTimerEvent) then
    fChoreographerThread.FTimerEvent(fChoreographerThread);

  if fChoreographerThread.Enabled then
    fChoreographerThread.fChoreographer.postFrameCallback(self);

end;

{************************************************************}
constructor TALChoreographerThread.Create(AOwner: TComponent);
begin
  inherited create;
  fChoreographer := TJChoreographer.JavaClass.getInstance;
  fChoreographerFrameCallback := TChoreographerFrameCallback.create(self);
  FTimerEvent := nil;
  Interval := 1;
  FEnabled := False;
end;

{****************************************}
destructor TALChoreographerThread.Destroy;
begin
  alFreeAndNil(fChoreographerFrameCallback);
  inherited;
end;

{****************************************************************}
procedure TALChoreographerThread.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    if FEnabled then fChoreographer.postFrameCallback(fChoreographerFrameCallback)
    else fChoreographer.removeFrameCallback(fChoreographerFrameCallback);
  end;
end;

{******************************************************************}
procedure TALChoreographerThread.SetInterval(const Value: Cardinal);
begin
  FInterval := Max(Value, 1);
end;

{$ENDIF}

{$IFDEF IOS}

{***********************************************************************************************************}
constructor TALDisplayLinkThread.TDisplayLinkListener.Create(const aDisplayLinkThread: TALDisplayLinkThread);
begin
  inherited Create;
  fDisplayLinkThread := aDisplayLinkThread;
end;

{*********************************************************************}
procedure TALDisplayLinkThread.TDisplayLinkListener.displayLinkUpdated;
begin

  {$IFDEF DEBUG}
  //ALLog('TALDisplayLinkThread.TDisplayLinkListener.displayLinkUpdated', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  if assigned(fDisplayLinkThread.FTimerEvent) then
    fDisplayLinkThread.FTimerEvent(fDisplayLinkThread);

end;

{*******************************************************************************}
function TALDisplayLinkThread.TDisplayLinkListener.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IDisplayLinkListener);
end;

{**********************************************************}
constructor TALDisplayLinkThread.Create(AOwner: TComponent);
begin
  inherited create;
  fDisplayLinkListener := TDisplayLinkListener.Create(self);
  fDisplayLink := TCADisplayLink.Wrap(TCADisplayLink.OCClass.displayLinkWithTarget(fDisplayLinkListener.GetObjectID, sel_getUid('displayLinkUpdated')));
  fDisplayLink.retain;
  fDisplayLink.addToRunLoop(TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop), NSRunLoopCommonModes); // I don't really know with is the best, NSDefaultRunLoopMode or NSRunLoopCommonModes
  fDisplayLink.setPaused(true);
  FTimerEvent := nil;
  Interval := 1;
  FEnabled := False;
end;

{**************************************}
destructor TALDisplayLinkThread.Destroy;
begin
  fDisplayLink.invalidate; // Removes the display link from all run loop modes.
                           // Removing the display link from all run loop modes causes it to be released by the run loop. The display link also releases the target.
                           // invalidate is thread safe meaning that it can be called from a thread separate to the one in which the display link is running.
  fDisplayLink.release;
  AlFreeAndNil(fDisplayLinkListener);
  inherited;
end;

{**************************************************************}
procedure TALDisplayLinkThread.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    if FEnabled then fDisplayLink.setPaused(False)
    else fDisplayLink.setPaused(True);
  end;
end;

{******************************************************************}
procedure TALDisplayLinkThread.SetInterval(const Value: Cardinal);
begin
  FInterval := Max(Value, 1);
end;

{$ENDIF}

{******************************}
constructor TALAniThread.Create;
begin
  inherited Create(nil);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    raise EUnsupportedPlatformService.Create('IFMXTimerService');
  TALAnimation.AniFrameRate := EnsureRange(TALAnimation.AniFrameRate, 5, 100);
  Interval := Trunc(1000 / TALAnimation.AniFrameRate / 10) * 10;
  if (Interval <= 0) then
    Interval := 1;

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

{*********************************}
Procedure TALAniThread.WakeUpTimer;
begin
  if not enabled then exit;
  if FTimerService.GetTick - FTime > 0.04 then begin // normally DoSyncTimer must be called every 0.016 seconds.
                                                     // but in heavy situation, especially on ios, the CADisplay link
                                                     // could never fire. I saw it on iphone 5 playing webRTC + filter
                                                     // the GPU was so busy that the CADisplay link never fire.
                                                     // so if it's was not called for more than 0.04 seconds (0.016*2 + 0.016/2)
                                                     // then call it again
    {$IFDEF DEBUG}
    ALLog('TALAniThread.WakeUpTimer', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.warn);
    {$ENDIF}
    DoSyncTimer(nil);
  end;
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
  [unsafe] Ani: TALAnimation;
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
      Ani := FAniList[I];
      if Ani.FRunning then
      begin
        Ani.ProcessTick(FTime, FDeltaTime);
      end;
      Dec(I);
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
  if assigned(fOnFirstFrame) then
    fOnFirstFrame(Self);
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

    AniThread.AddAnimation(Self);
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

{***************************************}
class Procedure TALAnimation.WakeUpTimer;
begin
  if AniThread <> nil then FAniThread.WakeUpTimer
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

{***********************************}
constructor TALColorAnimation.Create;
begin
  inherited;
  Duration := 0.2;
  FStartColor := $FFFFFFFF;
  FStopColor := $FFFFFFFF;
  fCurrentColor := $FFFFFFFF;
end;

{********************************}
procedure TALColorAnimation.Start;
begin
  fCurrentColor := FStartColor;
  inherited Start;
end;

{*******************************************}
procedure TALColorAnimation.ProcessAnimation;
begin
  fCurrentColor := InterpolateColor(FStartColor, FStopColor, NormalizedTime);
end;

{********************************************************}
function TALCustomPropertyAnimation.FindProperty: Boolean;
var
  Persistent: string;
  Comp: TFmxObject;
  I: Integer;
  T: TRttiType;
  P: TRttiProperty;
  Properties: TList<TRttiProperty>;
begin
  Result := False;

  if (Parent <> nil) and (FPropertyName <> '') then
  begin
    if FInstance = nil then
    begin
      FInstance := Parent;
      FPath := FPropertyName;
      while FPath.Contains('.') do
      begin
        Persistent := GetToken(FPath, '.');
        T := SharedContext.GetType(FInstance.ClassInfo);
        if T <> nil then
        begin
          P := T.GetProperty(Persistent);
          if (P <> nil) and (P.PropertyType.IsInstance) then
            FInstance := P.GetValue(FInstance).AsObject
          else
          if Parent <> nil then
          begin
            for I := 0 to Parent.ChildrenCount - 1 do
              if CompareText(Parent.Children[I].Name, Persistent) = 0 then
              begin
                Comp := Parent.Children[I];
                T := SharedContext.GetType(Comp.ClassInfo);
                if T <> nil then
                begin
                  P := T.GetProperty(FPath);
                  if P <> nil then
                  begin
                    FInstance := Comp;
                    Break;
                  end;
                end;
              end;
          end;
        end;
      end;
      if FInstance <> nil then
      begin

        if not ClonePropertiesCache.TryGetValue(FInstance.ClassName, Properties) then
        begin
          Properties := TList<TRttiProperty>.Create;
          ClonePropertiesCache.Add(FInstance.ClassName, Properties);
        end;

        for P in Properties do
          if P.Name = FPath then
          begin
            FRttiProperty := P;
            Break;
          end;

        if FRttiProperty = nil then
        begin
          T := SharedContext.GetType(FInstance.ClassInfo);
          FRttiProperty := T.GetProperty(FPath);
          if FRttiProperty <> nil then
            Properties.Add(FRttiProperty);
        end;
        Result := FRttiProperty <> nil;
      end;
    end
    else
      Result := True;
  end;
end;

{*************************************************}
procedure TALCustomPropertyAnimation.ParentChanged;
begin
  inherited;
  FInstance := nil;
end;

{*************************************************************************}
procedure TALCustomPropertyAnimation.SetPropertyName(const AValue: string);
begin
  if not SameText(AValue, PropertyName) then
    FInstance := nil;
  FPropertyName := AValue;
end;

{****************************************}
procedure TALCustomPropertyAnimation.Stop;
begin
  FInstance := nil;
end;

{***************************************************************}
constructor TALFloatPropertyAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FStartFromCurrent := False;
  fFloatAnimation := TALFloatAnimation.Create;
  fFloatAnimation.OnFirstFrame := DoFirstFrame;
  fFloatAnimation.OnProcess := DoProcess;
  fFloatAnimation.OnFinish := DoFinish;
  FOnFirstFrame := nil;
  FOnProcess := nil;
  FOnFinish := nil;
end;

{*******************************************}
destructor TALFloatPropertyAnimation.Destroy;
begin
  fFloatAnimation.Enabled := False;
  ALFreeAndNil(fFloatAnimation);
  inherited;
end;

{****************************************************************}
procedure TALFloatPropertyAnimation.doFirstFrame(Sender: TObject);
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
        StartValue := P.GetValue(FInstance).AsExtended;
    end;
  end;
  if assigned(fOnFirstFrame) then
    fOnFirstFrame(self);
end;

{*************************************************************}
procedure TALFloatPropertyAnimation.doProcess(Sender: TObject);
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
        P.SetValue(FInstance, fFloatAnimation.CurrentValue);
    end;
  end;
  if assigned(fOnProcess) then
    fOnProcess(self);
end;

{************************************************************}
procedure TALFloatPropertyAnimation.doFinish(Sender: TObject);
begin
  if assigned(fOnFinish) then
    fOnFinish(self);
end;

{******************************************************************}
function TALFloatPropertyAnimation.getAnimationType: TAnimationType;
begin
  result := fFloatAnimation.AnimationType;
end;

{*********************************************************}
function TALFloatPropertyAnimation.getAutoReverse: Boolean;
begin
  result := fFloatAnimation.AutoReverse;
end;

{**************************************************}
function TALFloatPropertyAnimation.getDelay: Single;
begin
  result := fFloatAnimation.Delay;
end;

{*****************************************************}
function TALFloatPropertyAnimation.getDuration: Single;
begin
  result := fFloatAnimation.Duration;
end;

{*****************************************************}
function TALFloatPropertyAnimation.getEnabled: Boolean;
begin
  result := fFloatAnimation.Enabled;
end;

{**********************************************************************}
function TALFloatPropertyAnimation.getInterpolation: TInterpolationType;
begin
  result := fFloatAnimation.Interpolation;
end;

{*****************************************************}
function TALFloatPropertyAnimation.getInverse: Boolean;
begin
  result := fFloatAnimation.Inverse;
end;

{**************************************************}
function TALFloatPropertyAnimation.getLoop: Boolean;
begin
  result := fFloatAnimation.Loop;
end;

{******************************************************}
function TALFloatPropertyAnimation.getOvershoot: Single;
begin
  result := fFloatAnimation.Overshoot;
end;

{***************************************************}
function TALFloatPropertyAnimation.getPause: Boolean;
begin
  result := fFloatAnimation.Pause;
end;

{*****************************************************}
function TALFloatPropertyAnimation.getRunning: Boolean;
begin
  result := fFloatAnimation.Running;
end;

{*******************************************************}
function TALFloatPropertyAnimation.GetStartValue: Single;
begin
  result := fFloatAnimation.StartValue;
end;

{******************************************************}
function TALFloatPropertyAnimation.GetStopValue: Single;
begin
  result := fFloatAnimation.StopValue;
end;

{**********************************************************}
function TALFloatPropertyAnimation.OvershootStored: Boolean;
begin
  result := fFloatAnimation.Overshoot <> 0;
end;

{********************************************************************************}
procedure TALFloatPropertyAnimation.setAnimationType(const Value: TAnimationType);
begin
  fFloatAnimation.AnimationType := Value;
end;

{***********************************************************************}
procedure TALFloatPropertyAnimation.setAutoReverse(const Value: Boolean);
begin
  fFloatAnimation.AutoReverse := Value;
end;

{****************************************************************}
procedure TALFloatPropertyAnimation.setDelay(const Value: Single);
begin
  fFloatAnimation.Delay := Value;
end;

{*******************************************************************}
procedure TALFloatPropertyAnimation.setDuration(const Value: Single);
begin
  fFloatAnimation.Duration := Value;
end;

{*******************************************************************}
procedure TALFloatPropertyAnimation.SetEnabled(const Value: Boolean);
begin
  if (fFloatAnimation.Enabled <> Value) and
     ((not value) or
      (FindProperty)) then fFloatAnimation.Enabled := Value;
end;

{************************************************************************************}
procedure TALFloatPropertyAnimation.setInterpolation(const Value: TInterpolationType);
begin
  fFloatAnimation.Interpolation := Value;
end;

{*******************************************************************}
procedure TALFloatPropertyAnimation.setInverse(const Value: Boolean);
begin
  fFloatAnimation.Inverse := Value;
end;

{****************************************************************}
procedure TALFloatPropertyAnimation.setLoop(const Value: Boolean);
begin
  fFloatAnimation.Loop := Value;
end;

{********************************************************************}
procedure TALFloatPropertyAnimation.setOvershoot(const Value: Single);
begin
  fFloatAnimation.Overshoot := Value;
end;

{*****************************************************************}
procedure TALFloatPropertyAnimation.setPause(const Value: Boolean);
begin
  fFloatAnimation.Pause := Value;
end;

{*********************************************************************}
procedure TALFloatPropertyAnimation.SetStartValue(const Value: Single);
begin
  fFloatAnimation.StartValue := Value;
end;

{********************************************************************}
procedure TALFloatPropertyAnimation.setStopValue(const Value: Single);
begin
  fFloatAnimation.StopValue := Value;
end;

{****************************************}
procedure TALFloatPropertyAnimation.Start;
begin
  if FindProperty then
    fFloatAnimation.Start;
end;

{***************************************}
procedure TALFloatPropertyAnimation.Stop;
begin
  fFloatAnimation.Stop;
  inherited;
end;

{************************************************}
procedure TALFloatPropertyAnimation.StopAtCurrent;
begin
  fFloatAnimation.StopAtCurrent;
end;

{***************************************************************}
constructor TALColorPropertyAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FStartFromCurrent := False;
  fColorAnimation := TALColorAnimation.Create;
  fColorAnimation.OnFirstFrame := DoFirstFrame;
  fColorAnimation.OnProcess := DoProcess;
  fColorAnimation.OnFinish := DoFinish;
  FOnFirstFrame := nil;
  FOnProcess := nil;
  FOnFinish := nil;
end;

{*******************************************}
destructor TALColorPropertyAnimation.Destroy;
begin
  fColorAnimation.Enabled := False;
  ALFreeAndNil(fColorAnimation);
  inherited;
end;

{****************************************************************}
procedure TALColorPropertyAnimation.doFirstFrame(Sender: TObject);
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.IsOrdinal) then
        StartValue := TAlphaColor(P.GetValue(FInstance).AsOrdinal);
    end;
  end;
  if assigned(fOnFirstFrame) then
    fOnFirstFrame(self);
end;

{*************************************************************}
procedure TALColorPropertyAnimation.doProcess(Sender: TObject);
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.IsOrdinal) then
        P.SetValue(FInstance, fColorAnimation.CurrentValue);
    end;
  end;
  if assigned(fOnProcess) then
    fOnProcess(self);
end;

{************************************************************}
procedure TALColorPropertyAnimation.doFinish(Sender: TObject);
begin
  if assigned(fOnFinish) then
    fOnFinish(self);
end;

{******************************************************************}
function TALColorPropertyAnimation.getAnimationType: TAnimationType;
begin
  result := fColorAnimation.AnimationType;
end;

{*********************************************************}
function TALColorPropertyAnimation.getAutoReverse: Boolean;
begin
  result := fColorAnimation.AutoReverse;
end;

{**************************************************}
function TALColorPropertyAnimation.getDelay: Single;
begin
  result := fColorAnimation.Delay;
end;

{*****************************************************}
function TALColorPropertyAnimation.getDuration: Single;
begin
  result := fColorAnimation.Duration;
end;

{*****************************************************}
function TALColorPropertyAnimation.getEnabled: Boolean;
begin
  result := fColorAnimation.Enabled;
end;

{**********************************************************************}
function TALColorPropertyAnimation.getInterpolation: TInterpolationType;
begin
  result := fColorAnimation.Interpolation;
end;

{*****************************************************}
function TALColorPropertyAnimation.getInverse: Boolean;
begin
  result := fColorAnimation.Inverse;
end;

{**************************************************}
function TALColorPropertyAnimation.getLoop: Boolean;
begin
  result := fColorAnimation.Loop;
end;

{******************************************************}
function TALColorPropertyAnimation.getOvershoot: Single;
begin
  result := fColorAnimation.Overshoot;
end;

{***************************************************}
function TALColorPropertyAnimation.getPause: Boolean;
begin
  result := fColorAnimation.Pause;
end;

{*****************************************************}
function TALColorPropertyAnimation.getRunning: Boolean;
begin
  result := fColorAnimation.Running;
end;

{************************************************************}
function TALColorPropertyAnimation.GetStartValue: TAlphaColor;
begin
  result := fColorAnimation.StartValue;
end;

{***********************************************************}
function TALColorPropertyAnimation.GetStopValue: TAlphaColor;
begin
  result := fColorAnimation.StopValue;
end;

{**********************************************************}
function TALColorPropertyAnimation.OvershootStored: Boolean;
begin
  result := fColorAnimation.Overshoot <> 0;
end;

{********************************************************************************}
procedure TALColorPropertyAnimation.setAnimationType(const Value: TAnimationType);
begin
  fColorAnimation.AnimationType := Value;
end;

{***********************************************************************}
procedure TALColorPropertyAnimation.setAutoReverse(const Value: Boolean);
begin
  fColorAnimation.AutoReverse := Value;
end;

{****************************************************************}
procedure TALColorPropertyAnimation.setDelay(const Value: Single);
begin
  fColorAnimation.Delay := Value;
end;

{*******************************************************************}
procedure TALColorPropertyAnimation.setDuration(const Value: Single);
begin
  fColorAnimation.Duration := Value;
end;

{*******************************************************************}
procedure TALColorPropertyAnimation.SetEnabled(const Value: Boolean);
begin
  if (fColorAnimation.Enabled <> Value) and
     ((not value) or
      (FindProperty)) then fColorAnimation.Enabled := Value;
end;

{************************************************************************************}
procedure TALColorPropertyAnimation.setInterpolation(const Value: TInterpolationType);
begin
  fColorAnimation.Interpolation := Value;
end;

{*******************************************************************}
procedure TALColorPropertyAnimation.setInverse(const Value: Boolean);
begin
  fColorAnimation.Inverse := Value;
end;

{****************************************************************}
procedure TALColorPropertyAnimation.setLoop(const Value: Boolean);
begin
  fColorAnimation.Loop := Value;
end;

{********************************************************************}
procedure TALColorPropertyAnimation.setOvershoot(const Value: Single);
begin
  fColorAnimation.Overshoot := Value;
end;

{*****************************************************************}
procedure TALColorPropertyAnimation.setPause(const Value: Boolean);
begin
  fColorAnimation.Pause := Value;
end;

{**************************************************************************}
procedure TALColorPropertyAnimation.SetStartValue(const Value: TAlphaColor);
begin
  fColorAnimation.StartValue := Value;
end;

{*************************************************************************}
procedure TALColorPropertyAnimation.setStopValue(const Value: TAlphaColor);
begin
  fColorAnimation.StopValue := Value;
end;

{****************************************}
procedure TALColorPropertyAnimation.Start;
begin
  if FindProperty then
    fColorAnimation.Start;
end;

{***************************************}
procedure TALColorPropertyAnimation.Stop;
begin
  fColorAnimation.Stop;
  inherited;
end;

{************************************************}
procedure TALColorPropertyAnimation.StopAtCurrent;
begin
  fColorAnimation.StopAtCurrent;
end;

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALFloatPropertyAnimation]);
  RegisterComponents('Alcinoe', [TALColorPropertyAnimation]);
end;

{************}
initialization
  RegisterFmxClasses([TALFloatPropertyAnimation]);
  RegisterFmxClasses([TALColorPropertyAnimation]);
  TALAnimation.AniFrameRate := TALAnimation.DefaultAniFrameRate;

{**********}
finalization
  TALAnimation.Uninitialize;

end.

