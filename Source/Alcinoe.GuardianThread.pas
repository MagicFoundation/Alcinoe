unit Alcinoe.GuardianThread;

{$I Alcinoe.inc}

interface

uses
  system.classes,
  system.SyncObjs,
  system.Generics.Collections,
  ALcinoe.fmx.Common;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALRefCountObject = Class(TObject)
  private
    FRefCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetRefCount: Integer;
    function IncreaseRefCount: TALRefCountObject; virtual;
    procedure DecreaseRefCount;
  End;

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
    FRefCountObjectsList: TObjectList<TALRefCountObject>;
    //-
    FFreeObjectsLock: TObject;
    FFreeObjectsCurrList: TObjectList<Tobject>;
    fFreeObjectsEmptyList: TObjectList<Tobject>;
    //-
    FFreeDrawablesLock: TObject;
    FFreeDrawablesCurrList: TList<TALDrawable>;
    fFreeDrawablesEmptyList: TList<TALDrawable>;
    //-
    FFreeBitmapsLock: TObject;
    FFreeBitmapsCurrList: TList<TALBitmap>;
    fFreeBitmapsEmptyList: TList<TALBitmap>;
    //-
    FCanExecute: boolean;
    FForceExecute: Boolean;
    FSignal: TEvent;
    procedure SetForceExecute(const AValue: Boolean);
    //-
    procedure FreeObjects;
    procedure FreeObject(var aObject: Tobject);
    //-
    procedure FreeDrawables;
    procedure FreeDrawable(var aDrawable: TALDrawable);
    //-
    procedure FreeBitmaps;
    procedure FreeBitmap(var aBitmap: TALBitmap);
    //-
    procedure PurgeRefCountObjectsList;
  protected
    procedure Execute; override;
    procedure DoExecute; virtual;
    procedure AddRefCountObject(const ARefCountObject: TALRefCountObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Signal: TEvent read FSignal;
    // CanExecute is a flag used to freeze the loop for 100 ms.
    // To prevent the loop from executing during UI animations,
    // simply set CanExecute to False in the form's repaint event.
    // CanExecute will automatically reset to True after 100 ms.
    property CanExecute: boolean read FCanExecute write FCanExecute;
    property ForceExecute: Boolean read FForceExecute write SetForceExecute;
  end;

implementation

uses
  system.SysUtils,
  {$IF defined(ios)}
  iOSApi.CoreGraphics,
  Posix.Sched,
  {$ENDIF}
  {$IF defined(ALMACOS)}
  Macapi.CoreGraphics,
  {$ENDIF}
  {$IF defined(Android)}
  Posix.Sched,
  Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  {$IF defined(ALSkiaAvailable)}
  System.Skia.API,
  {$ENDIF}
  Fmx.Types,
  Fmx.Types3D,
  Fmx.Graphics,
  Alcinoe.Common,
  Alcinoe.fmx.Controls,
  ALcinoe.fmx.Graphics,
  Alcinoe.FMX.DynamicListBox;

{***********************************}
constructor TALRefCountObject.Create;
begin
  inherited;
  FRefCount := 1;
  TALGuardianThread.Instance.AddRefCountObject(Self);
end;

{***********************************}
destructor TALRefCountObject.Destroy;
begin
  If FRefCount > 0 then
    ALLog('TALRefCountObject.Destroy', 'Reference count is greater than zero', TALLogType.WARN);
  inherited;
end;

{***********************************}
function TALRefCountObject.GetRefCount: Integer;
begin
  Result := AtomicCmpExchange(FRefCount, -1{NewValue}, -1{Comparand});
end;

{***********************************}
function TALRefCountObject.IncreaseRefCount: TALRefCountObject;
begin
  // When calling IncreaseRefCount, it means that we already have
  // a valid, strong reference to the object.
  AtomicIncrement(FRefCount);
  Result := Self;
end;

{***********************************}
procedure TALRefCountObject.DecreaseRefCount;
begin
  AtomicDecrement(FRefCount);
end;

{***********************************}
constructor TALGuardianThread.Create;
begin
  inherited Create(true{CreateSuspended});
  FRefCountObjectsList := TObjectList<TALRefCountObject>.create(false{aOwnObject});
  //-
  FFreeObjectsLock := TObject.Create;
  FFreeObjectsCurrList := TObjectList<Tobject>.create(false{aOwnObject});
  FFreeObjectsEmptyList := TObjectList<Tobject>.Create(false{aOwnObject});
  //-
  FFreeDrawablesLock := TObject.Create;
  FFreeDrawablesCurrList := TList<TALDrawable>.create;
  FFreeDrawablesEmptyList := TList<TALDrawable>.Create;
  //-
  FFreeBitmapsLock := TObject.Create;
  FFreeBitmapsCurrList := TList<TALBitmap>.create;
  FFreeBitmapsEmptyList := TList<TALBitmap>.Create;
  //-
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
  FRefCountObjectsList.OwnsObjects := True;
  ALfreeandnil(FRefCountObjectsList);
  //-
  ALFreeAndNil(FFreeObjectsLock);
  FFreeObjectsCurrList.OwnsObjects := True;
  ALfreeandnil(FFreeObjectsCurrList);
  FFreeObjectsEmptyList.OwnsObjects := True;
  ALfreeandnil(FFreeObjectsEmptyList);
  //-
  ALFreeAndNil(FFreeDrawablesLock);
  for var I := FFreeDrawablesCurrList.Count - 1 downto 0 do begin
    var LDrawable := FFreeDrawablesCurrList[I];
    ALFreeAndNilDrawable(LDrawable);
  end;
  ALfreeandnil(FFreeDrawablesCurrList);
  for var I := FFreeDrawablesEmptyList.Count - 1 downto 0 do begin
    var LDrawable := FFreeDrawablesEmptyList[I];
    ALFreeAndNilDrawable(LDrawable);
  end;
  ALfreeandnil(FFreeDrawablesEmptyList);
  //-
  ALFreeAndNil(FFreeBitmapsLock);
  for var I := FFreeBitmapsCurrList.Count - 1 downto 0 do begin
    var LBitmap := FFreeBitmapsCurrList[I];
    ALFreeAndNilBitmap(LBitmap);
  end;
  ALfreeandnil(FFreeBitmapsCurrList);
  for var I := FFreeBitmapsEmptyList.Count - 1 downto 0 do begin
    var LBitmap := FFreeBitmapsEmptyList[I];
    ALFreeAndNilBitmap(LBitmap);
  end;
  ALfreeandnil(FFreeBitmapsEmptyList);
  //-
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
      ALCustomDelayedFreeDrawableProc := FInstance.FreeDrawable;
      ALCustomDelayedFreeBitmapProc := FInstance.FreeBitmap;
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

    // To mitigate issues when repeatedly calling FreeObject, FreeDrawable, or FreeBitmap,
    // and to ensure that OpenGL completes all operations on a texture before it's deleted,
    // call sleep(1000) to wait for up to 1000 milliseconds.
    sleep(1000);

    // Wait until we can execute the loop
    var LMaxTickCount64 := TThread.GetTickCount64+120000{2 minutes};
    while (not canExecute) and
          (not ForceExecute) and
          (not Terminated) and
          (TThread.GetTickCount64 < LMaxTickCount64) do begin // Wait max 2 minutes before forcing loop execution
      CanExecute := True;
      FSignal.WaitFor(100);
    end;
    FForceExecute := False;
    FSignal.ResetEvent;
    if Terminated then Break;

    // Free unreferenced refcount objects
    PurgeRefCountObjectsList;
    if Terminated then Break;

    // Free the objects
    FreeObjects;
    if Terminated then Break;

    // Free the drawables
    FreeDrawables;
    if Terminated then Break;

    // Free the bitmaps
    FreeBitmaps;
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

  // Temporary list to store objects to be freed
  var LFreeObjectsWorkList: TObjectList<Tobject>;

  // Swap the current list with an empty list to process the objects safely
  TMonitor.Enter(FFreeObjectsLock);
  try
    LFreeObjectsWorkList := FFreeObjectsCurrList;
    fFreeObjectsCurrList := fFreeObjectsEmptyList;
    fFreeObjectsEmptyList := LFreeObjectsWorkList;
  finally
    TMonitor.Exit(FFreeObjectsLock);
  end;

  // Iterate through the objects in reverse order and free them
  for var I := LFreeObjectsWorkList.Count - 1 downto 0 do begin

    // Exit if the thread has been terminated
    if terminated then exit;
    try

      var LObj := LFreeObjectsWorkList[I];
      LFreeObjectsWorkList.delete(I);
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
      If aObject is TALControl then aObject.BeforeDestruction;
    end
    else if (aObject is TALDynamicListBoxControl) then begin
      aObject.BeforeDestruction;
      TALDynamicListBoxControl(aObject).OwnerControl := nil;
    end;

    // Add the object to the queue
    TMonitor.Enter(FFreeObjectsLock);
    try
      fFreeObjectsCurrList.Add(aObject);
      {$IFDEF DEBUG}
      //ALLog('TALGuardianThread.FreeObject', aObject.className + ' | FreeObjectsCurrList.count: ' + ALIntToStrW(fFreeObjectsCurrList.count), TalLogType.verbose);
      {$ENDIF}
      aObject := nil;
    finally
      TMonitor.exit(FFreeObjectsLock);
    end;

    // Signal the event
    FSignal.SetEvent;

  end;
end;

{**************************************}
procedure TALGuardianThread.FreeDrawables;
begin

  // Temporary list to store drawables to be freed
  var LFreeDrawablesWorkList: TList<TALDrawable>;

  // Swap the current list with an empty list to process the drawables safely
  TMonitor.Enter(FFreeDrawablesLock);
  try
    LFreeDrawablesWorkList := FFreeDrawablesCurrList;
    fFreeDrawablesCurrList := fFreeDrawablesEmptyList;
    fFreeDrawablesEmptyList := LFreeDrawablesWorkList;
  finally
    TMonitor.Exit(FFreeDrawablesLock);
  end;

  // Iterate through the drawables in reverse order and free them
  for var I := LFreeDrawablesWorkList.Count - 1 downto 0 do begin

    // Exit if the thread has been terminated
    if terminated then exit;
    try

      var LDrawable := LFreeDrawablesWorkList[I];
      LFreeDrawablesWorkList.delete(I);
      ALFreeAndNilDrawable(LDrawable);

    except
      on e: Exception do begin
        // Drawable could not be freed. Logging the
        // exception and continuing execution.
        ALLog('TALGuardianThread.FreeDrawables', E)
      end;
    end;

  end;

end;

{***********************************************************}
procedure TALGuardianThread.FreeDrawable(var aDrawable: TALDrawable);
begin
  if terminated then ALFreeAndNilDrawable(aDrawable)
  else begin

    // Add the drawable to the queue
    TMonitor.Enter(FFreeDrawablesLock);
    try
      fFreeDrawablesCurrList.Add(aDrawable);
      aDrawable := ALNullDrawable;
    finally
      TMonitor.exit(FFreeDrawablesLock);
    end;

    // Signal the event
    FSignal.SetEvent;

  end;
end;

{**************************************}
procedure TALGuardianThread.FreeBitmaps;
begin

  // Temporary list to store bitmaps to be freed
  var LFreeBitmapsWorkList: TList<TALBitmap>;

  // Swap the current list with an empty list to process the bitmaps safely
  TMonitor.Enter(FFreeBitmapsLock);
  try
    LFreeBitmapsWorkList := FFreeBitmapsCurrList;
    fFreeBitmapsCurrList := fFreeBitmapsEmptyList;
    fFreeBitmapsEmptyList := LFreeBitmapsWorkList;
  finally
    TMonitor.Exit(FFreeBitmapsLock);
  end;

  // Iterate through the bitmaps in reverse order and free them
  for var I := LFreeBitmapsWorkList.Count - 1 downto 0 do begin

    // Exit if the thread has been terminated
    if terminated then exit;
    try

      var LBitmap := LFreeBitmapsWorkList[I];
      LFreeBitmapsWorkList.delete(I);
      ALFreeAndNilBitmap(LBitmap);

    except
      on e: Exception do begin
        // Bitmap could not be freed. Logging the
        // exception and continuing execution.
        ALLog('TALGuardianThread.FreeBitmaps', E)
      end;
    end;

  end;

end;

{***********************************************************}
procedure TALGuardianThread.FreeBitmap(var aBitmap: TALBitmap);
begin
  if terminated then ALFreeAndNilBitmap(aBitmap)
  else begin

    // Add the bitmap to the queue
    TMonitor.Enter(FFreeBitmapsLock);
    try
      fFreeBitmapsCurrList.Add(aBitmap);
      aBitmap := ALNullBitmap;
    finally
      TMonitor.exit(FFreeBitmapsLock);
    end;

    // Signal the event
    FSignal.SetEvent;

  end;
end;

{***************************************************}
procedure TALGuardianThread.PurgeRefCountObjectsList;
begin
  TMonitor.Enter(FRefCountObjectsList);
  try
    For var I := FRefCountObjectsList.Count - 1 downto 0 do
      if FRefCountObjectsList[I].GetRefCount <= 0 then begin
        var LRefCountObject: TObject := FRefCountObjectsList.ExtractAt(I);
        FreeObject(LRefCountObject);
      end;
  finally
    TMonitor.exit(FRefCountObjectsList);
  end;
end;

{*****************************************************************}
procedure TALGuardianThread.AddRefCountObject(const ARefCountObject: TALRefCountObject);
begin
  TMonitor.Enter(FRefCountObjectsList);
  try
    fRefCountObjectsList.Add(ARefCountObject);
  finally
    TMonitor.exit(FRefCountObjectsList);
  end;
end;

{*****************************************************************}
procedure TALGuardianThread.SetForceExecute(const AValue: Boolean);
begin
  FForceExecute := AValue;
  if AValue then
    FSignal.SetEvent;
end;

initialization
  TALGuardianThread.FInstance := nil;
  TALGuardianThread.CreateInstanceFunc := @TALGuardianThread.CreateInstance;

finalization
  if TALGuardianThread.HasInstance then begin
    ALCustomDelayedFreeObjectProc := nil;
    ALCustomDelayedFreeDrawableProc := nil;
    ALCustomDelayedFreeBitmapProc := nil;
  end;
  ALFreeAndNil(TALGuardianThread.FInstance);

end.
