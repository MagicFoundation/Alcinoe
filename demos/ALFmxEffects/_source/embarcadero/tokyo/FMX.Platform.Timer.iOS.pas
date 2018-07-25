{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Timer.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.Generics.Collections, System.Messaging, FMX.Types;

type

  /// <summary>Implementation of timer service for iOS</summary>
  TCocoaTouchTimerService = class(TInterfacedObject, IFMXTimerService)
  private
    FTimers: TList<TFmxHandle>;
    FObjectMap: TDictionary<TFmxHandle, TObject>;
    FHandleCounter: TFmxHandle;
    FTerminating: Boolean;
    procedure ApplicationTerminatingHandler(const Sender: TObject; const Msg: TMessage);
  protected
    /// <summary>Returns new unique handle</summary>
    function NewFmxHandle: TFmxHandle;
    /// <summary>Creates new unique handle and binds it with specified object.</summary>
    /// <remarks>Use <c>DeleteObjectHandle</c> for removing of created binding.</remarks>
    function AllocObjectHandle(const AObject: TObject): TFmxHandle;
    /// <summary>Removes binding specified handle with object, which was created by <c>AllocObjectHandle</c></summary>
    procedure DeleteObjectHandle(const AHandle: TFmxHandle);
    /// <summary>Finds object, which was bound with specified handle</summary>
    function HandleToObject(const AHandle: TFmxHandle): TObject;
    /// <summary>Checks a validity of a <c>AHandle</c></summary>
    procedure ValidateHandle(const AHandle: TFmxHandle);
    /// <summary>Destroys all allocated timers</summary>
    procedure DestroyTimers;
    /// <summary>Registers timer service in platform</summary>
    procedure RegisterService; virtual;
    /// <summary>Unregisters timer service from platform</summary>
    procedure UnregisterService; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXTimerService }
    /// <summary>Creates timer and assing <c>ATimerFunc</c></summary>
    /// <returns>Handle of timer for correet destroying by <c>DestroyTimer</c></returns>
    function CreateTimer(AInterval: Integer; ATimerFunc: TTimerProc): TFmxHandle;
    /// <summary>Destroy timer by specified timer's handle</summary>
    function DestroyTimer(ATimer: TFmxHandle): Boolean;
    /// <summary>Returns current time in nano seconds</summary>
    function GetTick: Double;
  end;

implementation

uses
  System.TypInfo, System.SysUtils, System.SyncObjs, Macapi.ObjCRuntime, Macapi.ObjectiveC, Macapi.Mach,
  iOSapi.Foundation, iOSapi.CocoaTypes, FMX.Forms, FMX.Platform, FMX.Consts;

type

  CocoaTimer = interface(NSObject)
    ['{B65CD0E6-21EA-4E77-BF5E-981C3B0EE632}']
    procedure timerEvent; cdecl;
  end;

  TCocoaTimer = class(TOCLocal)
  private
    FOnTimer: TTimerProc;
    FTimer: NSTimer;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure timerEvent; cdecl;
    property OnTimer: TTimerProc read FOnTimer write FOnTimer;
  end;

{ TCocoaTimerService }

function TCocoaTouchTimerService.AllocObjectHandle(const AObject: TObject): TFmxHandle;
begin
  Result := NewFmxHandle;
  TMonitor.Enter(FObjectMap);
  try
    FObjectMap.Add(Result, AObject);
  finally
    TMonitor.Exit(FObjectMap);
  end;
end;

procedure TCocoaTouchTimerService.ApplicationTerminatingHandler(const Sender: TObject; const Msg: TMessage);
begin
  FTerminating := True;
  DestroyTimers;
end;

constructor TCocoaTouchTimerService.Create;
begin
  inherited;
  FTerminating := False;
  FTimers := TList<TFmxHandle>.Create;
  FObjectMap := TDictionary<TFmxHandle, TObject>.Create;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationTerminatingMessage, ApplicationTerminatingHandler);

  RegisterService;
end;

function TCocoaTouchTimerService.CreateTimer(AInterval: Integer; ATimerFunc: TTimerProc): TFmxHandle;
var
  Timer: TCocoaTimer;
  Interval: NSTimeInterval;
begin
  Result := 0;
  if not FTerminating and (AInterval > 0) and Assigned(ATimerFunc) then
  begin
    Timer := TCocoaTimer.Create;
    try
      Timer.OnTimer := ATimerFunc;
      Interval := AInterval / MSecsPerSec;

      Timer.FTimer := TNSTimer.Wrap(TNSTimer.OCClass.scheduledTimerWithTimeInterval(Interval, Timer.GetObjectID,
        sel_getUid('timerEvent'), Timer.GetObjectID, True));

      Result := AllocObjectHandle(Timer);
      FTimers.Add(Result);
    finally
      {user is retained (twice, because it's target), by the timer and }
      {released (twice) on timer invalidation}
      NSObject(Timer.Super).release;
    end;
  end;
end;

procedure TCocoaTouchTimerService.DeleteObjectHandle(const AHandle: TFmxHandle);
begin
  TMonitor.Enter(FObjectMap);
  try
    ValidateHandle(AHandle);
    FObjectMap.Remove(AHandle);
  finally
    TMonitor.Exit(FObjectMap);
  end;
end;

destructor TCocoaTouchTimerService.Destroy;
begin
  UnregisterService;
  TMessageManager.DefaultManager.Unsubscribe(TApplicationTerminatingMessage, ApplicationTerminatingHandler);
  FObjectMap.Free;
  FTimers.Free;
  inherited;
end;

function TCocoaTouchTimerService.DestroyTimer(ATimer: TFmxHandle): Boolean;
var
  User: TCocoaTimer;
begin
  Result := False;
  User := TCocoaTimer(HandleToObject(ATimer));
  if User <> nil then
  begin
    User.FTimer.invalidate;
    User.FTimer := nil;
    DeleteObjectHandle(ATimer);
    FTimers.Remove(ATimer);
    Result := True;
  end;
end;

procedure TCocoaTouchTimerService.DestroyTimers;
var
  I: Integer;
begin
  for I := FTimers.Count - 1 downto 0 do
    try
      DestroyTimer(FTimers[I]);
    except
    end;
end;

function TCocoaTouchTimerService.GetTick: Double;
const
  NanoToSeconds = 1E-9;
begin
  Result := AbsoluteToNanoseconds(mach_absolute_time) * NanoToSeconds;
end;

function TCocoaTouchTimerService.HandleToObject(const AHandle: TFmxHandle): TObject;
begin
  TMonitor.Enter(FObjectMap);
  try
    ValidateHandle(AHandle);
    if FObjectMap.ContainsKey(AHandle) then
      Result := FObjectMap[AHandle]
    else
      Result := nil;
  finally
    TMonitor.Exit(FObjectMap);
  end;
end;

function TCocoaTouchTimerService.NewFmxHandle: TFmxHandle;
begin
{$IF defined(CPU64BITS)}
  Result := TInterlocked.Add(Int64(FHandleCounter), 16);
{$ELSE}
  Result := TInterlocked.Add(Integer(FHandleCounter), 16);
{$ENDIF}
end;

procedure TCocoaTouchTimerService.RegisterService;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService) then
    TPlatformServices.Current.AddPlatformService(IFMXTimerService, Self);
end;

procedure TCocoaTouchTimerService.UnregisterService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXTimerService);
end;

procedure TCocoaTouchTimerService.ValidateHandle(const AHandle: TFmxHandle);
begin
  if AHandle and $F <> 0 then
    raise EInvalidFmxHandle.CreateFMT(SInvalidFmxHandle, [HexDisplayPrefix, SizeOf(AHandle) * 2, AHandle]);
end;

function TCocoaTimer.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(CocoaTimer);
end;

procedure TCocoaTimer.timerEvent;
begin
  if Assigned(FOnTimer) then
    try
      FOnTimer;
    except
      if Application <> nil then
        Application.HandleException(nil);
    end;
end;

end.
