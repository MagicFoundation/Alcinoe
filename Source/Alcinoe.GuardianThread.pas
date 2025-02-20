unit Alcinoe.GuardianThread;

{$I Alcinoe.inc}

interface

uses
  system.classes,
  system.SyncObjs,
  system.Generics.Collections;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALGuardianThread = class(TThread)
  private
    class function CreateInstance: TALGuardianThread;
    class function GetInstance: TALGuardianThread; static;
  protected
    class var FInstance: TALGuardianThread;
  public
    type
      TCreateInstanceFunc = function: TALGuardianThread;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALGuardianThread read GetInstance;
    class function HasInstance: Boolean; inline;
  private
    FFreeObjectsCurrList: TObjectList<Tobject>;
    fFreeObjectsEmptyList: TObjectList<Tobject>;
    FFreeObjectsWorkList: TObjectList<Tobject>;
    fFreeObjectsCounter: Integer;
    FCanExecute: boolean;
    FForceExecute: Boolean;
    FSignal: TEvent;
    procedure SetForceExecute(const AValue: Boolean);
    function getHasObjectsToFree: boolean;
    procedure FreeObjects;
    procedure FreeObject(var aObject: Tobject);
  protected
    procedure Execute; override;
    procedure DoExecute; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Signal: TEvent read FSignal;
    property HasObjectsToFree: Boolean read getHasObjectsToFree;
    // CanExecute is a flag used to freeze the loop for 3 seconds.
    // To prevent the loop from executing during UI animations,
    // simply set CanExecute to False in the form's repaint event.
    // CanExecute will automatically reset to True after 3 seconds.
    property CanExecute: boolean read FCanExecute write FCanExecute;
    property ForceExecute: Boolean read FForceExecute write SetForceExecute;
  end;

implementation

uses
  system.SysUtils,
  {$IF defined(ios)}
  Posix.Sched,
  {$ENDIF}
  {$IF defined(Android)}
  Posix.Sched,
  {$ENDIF}
  Fmx.Types,
  Alcinoe.Common,
  Alcinoe.FMX.DynamicListBox;

{***********************************}
constructor TALGuardianThread.Create;
begin
  inherited Create(true{CreateSuspended});
  FFreeObjectsCurrList := TObjectList<Tobject>.create(false{aOwnObject});
  FFreeObjectsEmptyList := TObjectList<Tobject>.Create(false{aOwnObject});
  //FFreeObjectsWorkList
  fFreeObjectsCounter := 0;
  FCanExecute := True;
  FForceExecute := False;
  FSignal := TEvent.Create(nil, false{ManualReset}, false, '');
  {$IF defined(ANDROID) or defined(IOS)}
  Policy := SCHED_OTHER;
  priority := sched_get_priority_min(SCHED_OTHER);
  {$ENDIF}
end;

{***********************************}
destructor TALGuardianThread.Destroy;
begin
  Terminate;
  FSignal.setevent;
  WaitFor;
  ALfreeandNil(FSignal);
  FFreeObjectsCurrList.OwnsObjects := True;
  ALfreeandnil(FFreeObjectsCurrList);
  FFreeObjectsEmptyList.OwnsObjects := True;
  ALfreeandnil(FFreeObjectsEmptyList);
  inherited;
end;

{*************}
//[MultiThread]
class function TALGuardianThread.CreateInstance: TALGuardianThread;
begin
  result := TALGuardianThread.Create;
end;

{*************}
//[MultiThread]
class function TALGuardianThread.GetInstance: TALGuardianThread;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance)
    else begin
      ALCustomDelayedFreeObjectProc := FInstance.FreeObject;
      FInstance.start;
    end;
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALGuardianThread.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{**********************************}
procedure TALGuardianThread.Execute;
begin

  // Keep looping while not terminated
  while not Terminated do begin

    // Wait until we can execute the loop
    var LMaxTickCount64 := TThread.GetTickCount64+120000{2 minutes};
    while (not canExecute) and
          (not ForceExecute) and
          (not Terminated) and
          (TThread.GetTickCount64 < LMaxTickCount64) do begin // Wait max 2 minutes before forcing loop execution
      CanExecute := True;
      FSignal.WaitFor(3000);
      TMonitor.Enter(self);
      try
        if fFreeObjectsCurrList.count > 1000 then
          FForceExecute := True;
      finally
        TMonitor.Exit(self);
      end;
    end;
    FForceExecute := False;
    FSignal.ResetEvent;
    if Terminated then Break;

    // Free the objects
    FreeObjects;
    if Terminated then Break;

    // Execute custom logic
    DoExecute;
    if Terminated then Break;

    // Wait max 1 minute
    FSignal.WaitFor(60000);

  end;

end;

{************************************}
procedure TALGuardianThread.DoExecute;
begin
  // Virtual;
end;

{**************************************}
procedure TALGuardianThread.FreeObjects;
begin

  //get the count items
  TMonitor.Enter(self);
  try
    fFreeObjectsWorkList := FFreeObjectsCurrList;
    fFreeObjectsCurrList := fFreeObjectsEmptyList;
    fFreeObjectsEmptyList := fFreeObjectsWorkList;
    fFreeObjectsCounter := fFreeObjectsWorkList.count +
                           fFreeObjectsCurrList.count +
                           fFreeObjectsEmptyList.count;
  finally
    TMonitor.Exit(self);
  end;

  //free all the object
  for var I := fFreeObjectsWorkList.Count - 1 downto 0 do begin

    //break the loop if terminated
    if terminated then exit;
    try

      var LObj := fFreeObjectsWorkList[I];
      fFreeObjectsWorkList.delete(I);
      if LObj is TComponent then begin
        tthread.Queue(nil,
          procedure
          begin
            Try
              // Unfortunately, TFmxObject.BeforeDestruction is not thread-safe
              // because it sends notifications to linked controls.
              //
              //procedure TFmxObject.BeforeDestruction;
              //var
              //  I: Integer;
              //  L2: TList<Pointer>;
              //begin
              //  { NotifyList }
              //  if FNotifyList <> nil then
              //  begin
              //    ...
              //    IFreeNotification(L2[I]).FreeNotification(Self);
              //    ...
              //  end;
              //  inherited;
              //end;
              //
              // Additionally, TControl.Destroy is also not thread-safe
              // as it accesses the global variable TStyleCache.Current
              // without any synchronization mechanism.
              //
              //destructor TControl.Destroy;
              //begin
              // ...
              //  if TStyleCache.Initialized then
              //    TStyleCache.Current.Remove(Self);
              // ...
              //end;
              //
              // Due to these limitations, we must ensure that components
              // are freed in the main thread.
              AlFreeAndNil(LObj);
            except
              on e: Exception do begin
                // Object could not be freed. Logging the
                // exception and continuing execution.
                ALLog('TALGuardianThread.FreeObjects.Queue', E)
              end;
            end;
          end);
      end
      else
        AlFreeAndNil(LObj);

    except
      on e: Exception do begin
        // Object could not be freed. Logging the
        // exception and continuing execution.
        ALLog('TALGuardianThread.FreeObjects', E)
      end;
    end;

  end;

end;

{***********************************************************}
procedure TALGuardianThread.FreeObject(var aObject: Tobject);
begin
  if terminated then ALfreeandNil(aObject)
  else begin

    // Prepare the object for release
    if aObject is TComponent then begin
      if aObject is TFMXObject then TFMXObject(aObject).Parent := nil;
      if assigned(TComponent(aObject).Owner) then TComponent(aObject).Owner.RemoveComponent(TComponent(aObject));
    end
    else if (aObject is TALDynamicListBoxControl) then begin
      aObject.BeforeDestruction;
      TALDynamicListBoxControl(aObject).OwnerControl := nil;
    end;

    // Add the object to the queue
    TMonitor.Enter(self);
    try
      fFreeObjectsCurrList.Add(aObject);
      {$IFDEF DEBUG}
      //ALLog('TALGuardianThread.FreeObject', aObject.className + ' | FreeObjectsCurrList.count: ' + ALIntToStrW(fFreeObjectsCurrList.count), TalLogType.verbose);
      {$ENDIF}
      aObject := nil;
    finally
      TMonitor.exit(self);
    end;

    // Signal the event
    FSignal.SetEvent;

  end;
end;

{*****************************************************************}
procedure TALGuardianThread.SetForceExecute(const AValue: Boolean);
begin
  FForceExecute := AValue;
  if AValue then
    FSignal.SetEvent;
end;

{******************************************************}
function TALGuardianThread.getHasObjectsToFree: boolean;
begin
  TMonitor.Enter(self);
  try
    result := fFreeObjectsCurrList.count + fFreeObjectsCounter > 0;
  finally
    TMonitor.Exit(self);
  end;
end;

initialization
  TALGuardianThread.FInstance := nil;
  TALGuardianThread.CreateInstanceFunc := @TALGuardianThread.CreateInstance;

finalization
  ALCustomDelayedFreeObjectProc := nil;
  ALFreeAndNil(TALGuardianThread.FInstance);

end.
