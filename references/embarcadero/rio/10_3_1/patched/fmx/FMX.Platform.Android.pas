{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2018 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Android;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.Messaging, System.Classes, System.SysUtils, System.Generics.Collections,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.NativeActivity, Androidapi.Input, Androidapi.AppGlue,
  Androidapi.JNI.Embarcadero, Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, FMX.Forms, FMX.Types,
  FMX.Gestures, FMX.Consts, FMX.Platform, FMX.VirtualKeyboard.Android, FMX.Dialogs.Android, FMX.Platform.Timer.Android,
  FMX.Platform.Device.Android, FMX.Platform.Logger.Android, FMX.Platform.SaveState.Android, FMX.Platform.Screen.Android,
  FMX.Platform.Metrics.Android, FMX.Platform.UI.Android, FMX.Graphics.Android, FMX.Controls.Android;

type

  { Broadcast messages: Taking images }

  TMessageCancelReceivingImage = class(TMessage<Integer>);

  TMessageReceivedImagePath = class(TMessage<string>)
  public
    RequestCode: Integer;
  end;

  TFMXNativeActivityListener = class(TJavaLocal, JOnActivityListener)
  public
    procedure onCancelReceiveImage(ARequestCode: Integer); cdecl;
    procedure onReceiveImagePath(ARequestCode: Integer; AFileName: JString); cdecl;
    procedure onReceiveNotification(AIntent: JIntent); cdecl;
    procedure onReceiveResult(ARequestCode, AResultCode: Integer; AResultObject: JIntent); cdecl;
    procedure onRequestPermissionsResult(ARequestCode: Integer;
      APermissions: TJavaObjectArray<JString>; AGrantResults: TJavaArray<Integer>); cdecl;
  end;

{ TPlatformAndroid }

  TMainThreadWakeup = class(TThread)
  private
    FSignal: TObject;
    FRunnable: JRunnable;
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WakeUp;
  end;

  TRecognizerServiceMap = class(TInterfacedObject, IFMXGestureRecognizersService)
    procedure AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    procedure RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
  end;

  TRunnable = class;

  TPlatformAndroid = class(TInterfacedObject, IFMXApplicationEventService, IFMXApplicationService)
  private const
    UndefinedOrientation = TScreenOrientation(-1);
  private type
    TMessageQueueIdleHandler = class(TJavaLocal, JMessageQueue_IdleHandler)
    private
      [Weak] FPlatform: TPlatformAndroid;
      FHandler: JHandler;
      FEmptyRunnable : TRunnable;
    public
      constructor Create(APlatform: TPlatformAndroid);
      function queueIdle: Boolean; cdecl;
    end;
  private
    { Core services }
    FTimerService: TAndroidTimerService;
    FDeviceServices: TAndroidDeviceServices;
    FLoggerService: TAndroidLoggerService;
    FSaveStateService: TAndroidSaveStateService;
    FScreenServices: TAndroidScreenServices;
    FGraphicServices: TAndroidGraphicsServices;
    FMetricsServices: TAndroidMetricsServices;
    FVirtualKeyboardService: TVirtualKeyboardAndroid;
    FWindowService: TWindowServiceAndroid;
    FRecognizerServiceMap: TRecognizerServiceMap;
    FTextInputManager: TAndroidTextInputManager;
    FIdleHandler: TMessageQueueIdleHandler;
    { Internal }
    FMainThreadWakeup: TMainThreadWakeup;
    FOnApplicationEvent: TApplicationEventHandler;
    FActivityListener: TFMXNativeActivityListener;
    FFirstRun: Boolean;
    FLastOrientation: TScreenOrientation;
    FRunning: Boolean;
    FTerminating: Boolean;
    FTitle: string;
    fPause: Boolean; //https://quality.embarcadero.com/browse/RSP-18686
    fLostFocus: Boolean; //https://quality.embarcadero.com/browse/RSP-18686
    procedure CheckOrientationChange;
    procedure RegisterWakeMainThread;
    procedure UnregisterWakeMainThread;
    procedure WakeMainThread(Sender: TObject);
    procedure RegisterServices;
    procedure UnregisterServices;
    procedure BindAppGlueEvents;
    procedure UnbindAppGlueEvents;
    procedure RegisterIdleHandler;
    procedure UnregisterIdleHandler;
  protected
    function HandleAndroidInputEvent(const AAppGlue: TAndroidApplicationGlue; const AEvent: PAInputEvent): Int32;
    procedure HandleApplicationCommandEvent(const AAppGlue: TAndroidApplicationGlue; const ACommand: TAndroidApplicationCommand);
    procedure HandleContentRectChanged(const AAppGlue: TAndroidApplicationGlue; const ARect: TRect);
  public
    constructor Create;
    destructor Destroy; override;
    procedure InternalProcessMessages;
    function ProcessOnIdleEvent: Boolean;
    { IFMXApplicationService }
    procedure Run;
    function HandleMessage: Boolean;
    procedure WaitMessage;
    function GetDefaultTitle: string;
    function GetTitle: string;
    procedure SetTitle(const AValue: string);
    function GetVersionString: string;
    function Running: Boolean;
    function Terminating: Boolean;
    procedure Terminate;
    { IFMXApplicationEventService }
    procedure SetApplicationEventHandler(AEventHandler: TApplicationEventHandler);
    function HandleApplicationEvent(const AEvent: TApplicationEvent): Boolean;
  public
    property DeviceManager: TAndroidDeviceServices read FDeviceServices;
    property Logger: TAndroidLoggerService read FLoggerService;
    property Metrics: TAndroidMetricsServices read FMetricsServices;
    property SaveStateManager: TAndroidSaveStateService read FSaveStateService;
    property ScreenManager: TAndroidScreenServices read FScreenServices;
    property TextInputManager: TAndroidTextInputManager read FTextInputManager;
    property TimerManager: TAndroidTimerService read FTimerService;
    property VirtualKeyboard: TVirtualKeyboardAndroid read FVirtualKeyboardService;
    property WindowService: TWindowServiceAndroid read FWindowService;
  end;

  TRunnable = class(TJavaLocal, JRunnable)
  private
    FCallback: TProc;
  public
    constructor Create(const ACallback: TProc);
    { JRunnable }
    procedure run; cdecl;
  end;

var
  PlatformAndroid: TPlatformAndroid;

function WindowHandleToPlatform(const AHandle: TWindowHandle): TAndroidWindowHandle;
function MainActivity: JFMXNativeActivity;

procedure RegisterCorePlatformServices;
procedure UnregisterCorePlatformServices;

implementation

uses
  System.Devices, Androidapi.Helpers, AndroidApi.JNI.App, FMX.KeyMapping;

function WindowHandleToPlatform(const AHandle: TWindowHandle): TAndroidWindowHandle;
begin
  Result := TAndroidWindowHandle(AHandle);
end;

function MainActivity: JFMXNativeActivity;
begin
  if TAndroidApplicationGlue.Current <> nil then
    Result := TJFMXNativeActivity.Wrap(TAndroidApplicationGlue.Current.NativeActivity.clazz)
  else
    Result := nil;
end;

procedure RegisterCorePlatformServices;
begin
  PlatformAndroid := TPlatformAndroid.Create;
end;

procedure UnregisterCorePlatformServices;
begin
  FreeAndNil(PlatformAndroid);
end;

{ TPlatformAndroid }

constructor TPlatformAndroid.Create;
begin
  inherited;
  fPause := False; //https://quality.embarcadero.com/browse/RSP-18686
  fLostFocus := false; //https://quality.embarcadero.com/browse/RSP-18686
  BindAppGlueEvents;

  { Creates core services }
  FLoggerService := TAndroidLoggerService.Create;
  FTimerService := TAndroidTimerService.Create;
  FSaveStateService := TAndroidSaveStateService.Create;
  FScreenServices := TAndroidScreenServices.Create;
  FMetricsServices := TAndroidMetricsServices.Create;
  FDeviceServices := TAndroidDeviceServices.Create;
  if DeviceManager.GetDeviceClass <> TDeviceInfo.TDeviceClass.Watch then
    FVirtualKeyboardService := TVirtualKeyboardAndroid.Create;
  FGraphicServices := TAndroidGraphicsServices.Create;
  FWindowService := TWindowServiceAndroid.Create;
  FRecognizerServiceMap := TRecognizerServiceMap.Create;
  FTextInputManager := TAndroidTextInputManager.Create;
  FIdleHandler := TMessageQueueIdleHandler.Create(Self);

  Application := TApplication.Create(nil);
  FFirstRun := True;
  FRunning := False;
  FActivityListener := TFMXNativeActivityListener.Create;
  MainActivity.setListener(FActivityListener);
  FMainThreadWakeup := TMainThreadWakeup.Create;
  FLastOrientation := UndefinedOrientation;
  if DeviceManager.GetModel = 'Glass 1' then
    TPlatformServices.Current.GlobalFlags.Add(EnableGlassFPSWorkaround, True);

  RegisterServices;
  RegisterWakeMainThread;
  RegisterIdleHandler;
end;

destructor TPlatformAndroid.Destroy;
begin
  FTextInputManager.Free;
  FRecognizerServiceMap.Free;
  FWindowService.Free;
  FGraphicServices.Free;
  FVirtualKeyboardService.Free;
  FDeviceServices.Free;
  FMetricsServices.Free;
  FScreenServices.Free;
  FSaveStateService.Free;
  FTimerService.Free;
  FLoggerService.Free;

  UnregisterIdleHandler;
  UnregisterServices;
  UnbindAppGlueEvents;

  FMainThreadWakeup.DisposeOf;
  FActivityListener.DisposeOf;
  UnregisterWakeMainThread;
  inherited;
end;

procedure TPlatformAndroid.Run;
begin
  FRunning := True;
  { Although calling this routine is not really necessary, but it is a way to ensure that "Androidapi.AppGlue.pas" is
    kept in uses list, in order to export ANativeActivity_onCreate callback. }
  app_dummy;
  InternalProcessMessages;
end;

function TPlatformAndroid.Running: Boolean;
begin
  Result := FRunning;
end;

function TPlatformAndroid.Terminating: Boolean;
begin
  Result := FTerminating;
end;

procedure TPlatformAndroid.Terminate;
begin
  FRunning := False;
  FTerminating := True;
  TMessageManager.DefaultManager.SendMessage(nil, TApplicationTerminatingMessage.Create);
  // When we manually finish our activity, Android will not generate OnSaveInstanceState event, because it is generated
  // only in cases when the system is going to kill our activity to reclaim resources. In this particular case we
  // initiate correct termination of the application, so we have to invoke OnSaveInstanceState manually to make sure
  // that TForm.OnSaveState is invoked before the application is closed
  HandleApplicationCommandEvent(TAndroidApplicationGlue.Current, TAndroidApplicationCommand.SaveState);
  ANativeActivity_finish(System.DelphiActivity);
end;

function TPlatformAndroid.HandleAndroidInputEvent(const AAppGlue: TAndroidApplicationGlue; const AEvent: PAInputEvent): Int32;
var
  EventType: Int32;
begin
  EventType := AInputEvent_getType(AEvent);

  if EventType = AINPUT_EVENT_TYPE_KEY then
    // Keyboard input
    Result := TextInputManager.HandleAndroidKeyEvent(AEvent)
  else
    Result := 0;
end;

function TPlatformAndroid.HandleMessage: Boolean;
begin
  InternalProcessMessages;
  Result := False;
end;

procedure TPlatformAndroid.BindAppGlueEvents;
var
  AndroidAppGlue: TAndroidApplicationGlue;
begin
  AndroidAppGlue := PANativeActivity(System.DelphiActivity)^.instance;
  AndroidAppGlue.OnApplicationCommandEvent := HandleApplicationCommandEvent;
  AndroidAppGlue.OnContentRectEvent := HandleContentRectChanged;
  AndroidAppGlue.OnInputEvent := HandleAndroidInputEvent;
end;

procedure TPlatformAndroid.CheckOrientationChange;
var
  LOrientation: TScreenOrientation;
begin
  LOrientation := ScreenManager.GetScreenOrientation;
  if FLastOrientation <> LOrientation then
  begin
    FLastOrientation := LOrientation;
    TMessageManager.DefaultManager.SendMessage(Self, TOrientationChangedMessage.Create, True);
  end;
end;

procedure TPlatformAndroid.WaitMessage;
begin
  InternalProcessMessages;
end;

procedure TPlatformAndroid.RegisterWakeMainThread;
begin
  System.Classes.WakeMainThread := WakeMainThread;
end;

procedure TPlatformAndroid.UnregisterWakeMainThread;
begin
  System.Classes.WakeMainThread := nil;
end;

{ TWakeMainThreadRunnable }

constructor TMainThreadWakeup.Create;
begin
  inherited Create;
  FRunnable := TRunnable.Create(procedure begin
    PlatformAndroid.InternalProcessMessages;
  end);

  FSignal := TObject.Create;
end;

destructor TMainThreadWakeup.Destroy;
begin
  FRunnable := nil;
  inherited;
  FSignal.Free;
end;

procedure TMainThreadWakeup.Execute;
begin
  repeat
    TMonitor.Enter(FSignal);
    try
      TMonitor.Wait(FSignal, INFINITE);
    finally
      TMonitor.Exit(FSignal);
    end;
    if not Terminated then
      TAndroidHelper.Activity.runOnUiThread(FRunnable);
  until Terminated;
end;

procedure TMainThreadWakeup.TerminatedSet;
begin
  inherited;
  WakeUp;
end;

procedure TMainThreadWakeup.WakeUp;
begin
  TMonitor.Pulse(FSignal);
end;

procedure TPlatformAndroid.WakeMainThread(Sender: TObject);
begin
  FMainThreadWakeup.WakeUp;
end;

procedure TPlatformAndroid.InternalProcessMessages;
begin
  CheckSynchronize;
  ProcessOnIdleEvent;
end;

function TPlatformAndroid.ProcessOnIdleEvent: Boolean;
begin
  Result := True;
  if not Terminating then
    try
      Application.DoIdle(Result);
    except
      Application.HandleException(Application);
    end;
end;

function TPlatformAndroid.GetDefaultTitle: string;
begin
  Result := TAndroidHelper.ApplicationTitle;
end;

function TPlatformAndroid.GetTitle: string;
begin
  Result := FTitle;
end;

function TPlatformAndroid.GetVersionString: string;
var
  PackageInfo: JPackageInfo;
  PackageManager: JPackageManager;
  AppContext: JContext;
begin
  AppContext := TAndroidHelper.Context;
  if AppContext <> nil then
  begin
    PackageManager := AppContext.getPackageManager;
    if PackageManager <> nil then
    begin
      PackageInfo := AppContext.getPackageManager.getPackageInfo(AppContext.getPackageName, 0);
      if PackageInfo <> nil then
        Exit(JStringToString(PackageInfo.versionName));
    end;
  end;
  Result := string.Empty;
end;

procedure TPlatformAndroid.SetTitle(const AValue: string);
begin
  FTitle := AValue;
end;

procedure TPlatformAndroid.SetApplicationEventHandler(AEventHandler: TApplicationEventHandler);
begin
  FOnApplicationEvent := AEventHandler;
end;

function TPlatformAndroid.HandleApplicationEvent(const AEvent: TApplicationEvent): Boolean;
var
  ApplicationEventMessage: TApplicationEventMessage;
begin
  Result := False;

  { Send broadcast message }
  ApplicationEventMessage := TApplicationEventMessage.Create(TApplicationEventData.Create(AEvent, nil));
  TMessageManager.DefaultManager.SendMessage(nil, ApplicationEventMessage);

  { Invoke application event}
  if Assigned(FOnApplicationEvent) then
    try
      Result := FOnApplicationEvent(AEvent, nil);
    except
      Application.HandleException(Self);
    end;
end;

procedure TPlatformAndroid.HandleContentRectChanged(const AAppGlue: TAndroidApplicationGlue; const ARect: TRect);
begin
  ScreenManager.UpdateDisplayInformation;
end;

procedure TPlatformAndroid.HandleApplicationCommandEvent(const AAppGlue: TAndroidApplicationGlue; const ACommand: TAndroidApplicationCommand);
begin
  case ACommand of
    TAndroidApplicationCommand.Start:
      begin
        FRunning := True;
        FTerminating := False;
        if not Application.IsRealCreateFormsCalled then
        begin
          Application.RealCreateForms;
          // Android has logic of clipping native views in Window.
          // https://developer.android.com/reference/kotlin/android/view/Window#setClipToOutline%28kotlin.Boolean
          // It clips content of native Window by background drawable. We special disable it for FMX. Because we use
          // Window background for showing splash screen image. But splash screen image is not stretched on all Window
          // and as result Window clips native control.
          //
          // Unfortunately some Samsung devices doesn't respect windowClipToOutline property value in style-v21.xml file.
          //  Therefore, we forcibly reset the splash screen background after launching the application (see RSP-22928).
          TAndroidHelper.Activity.getWindow.setBackgroundDrawable(nil);
        end;

        if FFirstRun then
        begin
          FFirstRun := False;
          HandleApplicationEvent(TApplicationEvent.FinishedLaunching);
        end;
      end;

    //onResume() gets called just before your activity gets focus
    TAndroidApplicationCommand.Resume: begin
      fPause := False;
      HandleApplicationEvent(TApplicationEvent.WillBecomeForeground);
      if not fLostFocus then HandleApplicationEvent(TApplicationEvent.BecameActive); // << https://quality.embarcadero.com/browse/RSP-18686
    end;

    //and onPause gets called just before it loses focus
    TAndroidApplicationCommand.Pause: begin
      fPause := True;
      if not fLostFocus then HandleApplicationEvent(TApplicationEvent.WillBecomeInactive); // << https://quality.embarcadero.com/browse/RSP-18686
      HandleApplicationEvent(TApplicationEvent.EnteredBackground);
    end;

    TAndroidApplicationCommand.GainedFocus: begin
      fLostFocus := False;
      HandleApplicationEvent(TApplicationEvent.BecameActive);
    end;

    TAndroidApplicationCommand.LostFocus: begin
      fLostFocus := True;
      if not fPause then HandleApplicationEvent(TApplicationEvent.WillBecomeInactive); // << https://quality.embarcadero.com/browse/RSP-18686
    end;

    TAndroidApplicationCommand.SaveState:
      TMessageManager.DefaultManager.SendMessage(Self, TSaveStateMessage.Create);

    TAndroidApplicationCommand.ConfigChanged:
      CheckOrientationChange;

    TAndroidApplicationCommand.LowMemory:
      PlatformAndroid.HandleApplicationEvent(TApplicationEvent.LowMemory);

    TAndroidApplicationCommand.Destroy:
      HandleApplicationEvent(TApplicationEvent.WillTerminate);
  end;
end;

procedure TPlatformAndroid.RegisterIdleHandler;
var
  MessageQueue: JMessageQueue;
begin
  MessageQueue := TJLooper.JavaClass.getMainLooper.getQueue;
  MessageQueue.addIdleHandler(FIdleHandler);
end;

procedure TPlatformAndroid.RegisterServices;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXApplicationService) then
    TPlatformServices.Current.AddPlatformService(IFMXApplicationService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService) then
    TPlatformServices.Current.AddPlatformService(IFMXApplicationEventService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXWindowService) then
    TPlatformServices.Current.AddPlatformService(IFMXWindowService, WindowService);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXMouseService) then
    TPlatformServices.Current.AddPlatformService(IFMXMouseService, WindowService);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXGestureRecognizersService) then
    TPlatformServices.Current.AddPlatformService(IFMXGestureRecognizersService, FRecognizerServiceMap);

  if not TPlatformServices.Current.SupportsPlatformService(IFMXTextService) then
    TPlatformServices.Current.AddPlatformService(IFMXTextService, FTextInputManager);
  if (FVirtualKeyboardService <> nil) and not TPlatformServices.Current.SupportsPlatformService(IFMXKeyMappingService) then
    TPlatformServices.Current.AddPlatformService(IFMXKeyMappingService, FTextInputManager);
end;

procedure TPlatformAndroid.UnbindAppGlueEvents;
var
  AndroidAppGlue: TAndroidApplicationGlue;
begin
  AndroidAppGlue := PANativeActivity(System.DelphiActivity)^.instance;
  if AndroidAppGlue <> nil then
  begin
    AndroidAppGlue.OnApplicationCommandEvent := nil;
    AndroidAppGlue.OnContentRectEvent := nil;
    AndroidAppGlue.OnInputEvent := nil;
  end;
end;

procedure TPlatformAndroid.UnregisterIdleHandler;
var
  MessageQueue: JMessageQueue;
begin
  MessageQueue := TJLooper.JavaClass.getMainLooper.getQueue;
  MessageQueue.removeIdleHandler(FIdleHandler);
end;

procedure TPlatformAndroid.UnregisterServices;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXApplicationService);
  TPlatformServices.Current.RemovePlatformService(IFMXApplicationEventService);
  TPlatformServices.Current.RemovePlatformService(IFMXWindowService);
  TPlatformServices.Current.RemovePlatformService(IFMXMouseService);
  TPlatformServices.Current.RemovePlatformService(IFMXGestureRecognizersService);
  TPlatformServices.Current.RemovePlatformService(IFMXTextService);
  TPlatformServices.Current.RemovePlatformService(IFMXKeyMappingService);
end;

{ TFMXNativeActivityListener }

procedure TFMXNativeActivityListener.onCancelReceiveImage(ARequestCode: Integer);
begin
  TThread.Queue(nil, procedure
  begin
    TMessageManager.DefaultManager.SendMessage(nil, TMessageCancelReceivingImage.Create(ARequestCode));
  end);
end;

procedure TFMXNativeActivityListener.onReceiveImagePath(ARequestCode: Integer; AFileName: JString);
var
  Message: TMessageReceivedImagePath;
begin
  TThread.Queue(nil, procedure
  var
    ImageFileName: string;
  begin
    ImageFileName := JStringToString(AFileName);
    Message := TMessageReceivedImagePath.Create(ImageFileName);
    Message.RequestCode := ARequestCode;
    TMessageManager.DefaultManager.SendMessage(nil, Message);
  end);
end;

procedure TFMXNativeActivityListener.onReceiveNotification(AIntent: JIntent);
begin
  TMessageManager.DefaultManager.SendMessage(nil, TMessageReceivedNotification.Create(AIntent));
end;

procedure TFMXNativeActivityListener.onReceiveResult(ARequestCode, AResultCode: Integer; AResultObject: JIntent);
var
  Msg: TMessageResultNotification;
begin
  Msg := TMessageResultNotification.Create(AResultObject);
  Msg.RequestCode := ARequestCode;
  Msg.ResultCode := AResultCode;
  TMessageManager.DefaultManager.SendMessage(nil, Msg);
end;

procedure TFMXNativeActivityListener.onRequestPermissionsResult(ARequestCode: Integer;
  APermissions: TJavaObjectArray<JString>; AGrantResults: TJavaArray<Integer>);
var
  MsgData: TPermissionsRequestResultData;
  Msg: TPermissionsRequestResultMessage;
begin
  MsgData.RequestCode := ARequestCode;
  MsgData.Permissions := APermissions;
  MsgData.GrantResults := AGrantResults;
  Msg := TPermissionsRequestResultMessage.Create(MsgData);
  TMessageManager.DefaultManager.SendMessage(nil, Msg);
end;

{ TRecognizerServiceMap }

procedure TRecognizerServiceMap.AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
var
  Handle: TAndroidWindowHandle;
begin
  Handle := TAndroidWindowHandle(AForm.Handle);
  if Handle <> nil then
    Handle.MotionManager.AddRecognizer(ARec, AForm);
end;

procedure TRecognizerServiceMap.RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
var
  Handle: TAndroidWindowHandle;
begin
  Handle := TAndroidWindowHandle(AForm.Handle);
  if Handle <> nil then
    Handle.MotionManager.AddRecognizer(ARec, AForm);
end;

{ TPlatformAndroid.TMessageQueueIdleHandler }

constructor TPlatformAndroid.TMessageQueueIdleHandler.Create(APlatform: TPlatformAndroid);
begin
  inherited Create;
  FPlatform := APlatform;
  FHandler := TJHandler.JavaClass.init(TJLooper.JavaClass.getMainLooper);
  FEmptyRunnable := TRunnable.Create(procedure begin end);
end;

function TPlatformAndroid.TMessageQueueIdleHandler.queueIdle: Boolean;
begin
  Result := True;

  if not FPlatform.Terminating and not FPlatform.ProcessOnIdleEvent then
    // Reschedule next OnIdle invocation, if user returns False in Done parameter of OnIdle
    // We put our empty message into application EventQueue. It boosts invoking queueIdle again.
    FHandler.post(FEmptyRunnable);
end;

{ TRunnable }

constructor TRunnable.Create(const ACallback: TProc);
begin
  inherited Create;
  FCallback := ACallback;
end;

procedure TRunnable.run;
begin
  FCallback;
end;

end.
