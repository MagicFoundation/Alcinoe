unit Grijjy.ErrorReporting;

{ Some building blocks for creating an exception logger for iOS or Android.
  It traps unhandled exceptions and logs them with a stack trace (aka call
  stack).

  It can also trap exceptions on Windows and macOS, but it does not create a
  call stack on those platforms. }

interface

uses
  System.SysUtils,
  System.Messaging;

type
  { Signature of the TApplication.OnException event }
  TgoExceptionEvent = procedure(Sender: TObject; E: Exception) of object;

type
  { A single entry in a stack trace }
  TgoCallStackEntry = record
  public
    { The address of the code in the call stack. }
    CodeAddress: UIntPtr;

    { The address of the start of the routine in the call stack. The CodeAddress
      value always lies inside the routine that starts at RoutineAddress.
      This, the value "CodeAddress - RoutineAddress" is the offset into the
      routine in the call stack. }
    RoutineAddress: UIntPtr;

    { The (base) address of the module where CodeAddress is found. }
    ModuleAddress: UIntPtr;

    { The name of the routine at CodeAddress, if available. }
    RoutineName: String;

    { The name of the module where CodeAddress is found. If CodeAddress is
      somewhere inside your (Delphi) code, then this will be the name of the
      executable module (or .so file on Android). }
    ModuleName: String;
  public
    { Clears the entry (sets everything to 0) }
    procedure Clear;
  end;

type
  { A call stack (aka stack trace) is just an array of call stack entries. }
  TgoCallStack = TArray<TgoCallStackEntry>;

type
  { Represents an exception report. When an unhandled exception is encountered,
    it creates an exception report and broadcasts is using a
    TgoExceptionReportMessage }
  IgoExceptionReport = interface
  ['{A949A858-3B30-4C39-9A18-AD2A86B4BD9F}']
    {$REGION 'Internal Declarations'}
    function _GetExceptionMessage: String;
    function _GetExceptionLocation: TgoCallStackEntry;
    function _GetCallStack: TgoCallStack;
    function _GetReport: String;
    {$ENDREGION 'Internal Declarations'}

    { The exception message (the value of the Exception.Message property) }
    property ExceptionMessage: String read _GetExceptionMessage;

    { The location (address) of the exception. This is of type
      TgoCallStackEntry, so it also contains information about where and in
      which routine the exception happened. }
    property ExceptionLocation: TgoCallStackEntry read _GetExceptionLocation;

    { The call stack (aka stack trace) leading up the the exception. This
      also includes calls into the exception handler itself. }
    property CallStack: TgoCallStack read _GetCallStack;

    { A textual version of the exception. Contains the exception messages as
      well as a textual representation of the call stack. }
    property Report: String read _GetReport;
  end;

type
  { A type of TMessage that is used to broadcast exception reports.
    Subscribe to this message (using
    TMessageManager.DefaultManager.SubscribeToMessage) to get notified about
    exception reports.

    @bold(Important): This message is sent from the thread where the exception
    occured, which may not always be the UI thread. So don't update the UI from
    this message, or synchronize it with the main thread. }
  TgoExceptionReportMessage = class(TMessage)
  {$REGION 'Internal Declarations'}
  private
    FReport: IgoExceptionReport;
  {$ENDREGION 'Internal Declarations'}
  public
    { Used internally to create the message. }
    constructor Create(const AReport: IgoExceptionReport);

    { The exception report. }
    property Report: IgoExceptionReport read FReport;
  end;

type
  { Main class for reporting exceptions. To enable exception reporting, you
    need to do the following:
    * Let the exception logger capture unhandled FMX exceptions by calling:
        Application.OnException := TgoExceptionReporter.ExceptionHandler;
    * Subscribe to the TgoExceptionReportMessage message to get notified of
      exception reports:
        TMessageManager.DefaultManager.SubscribeToMessage(
          TgoExceptionReportMessage, HandleExceptionReport);
    * In this message handler, you can handle the report in any way you want.
      For example:
      * You can email it to your development team.
      * You can send it to your cloud backend.
      * You can show it to the user. However, note that the message may be sent
        from another thread than the UI thread, so you need to synchronize any
        UI calls with the main thread.
      * You can send it to a service like HockeyApp.
      * etc.
      However, because the app may be unstable now (depending on the type of
      exception) it may be safest to just write the report to disk and terminate
      the app (by calling Halt). Then, the next time the app starts up, and can
      check for this file and handle the report at that point.

    NOTE FOR ANDROID: For symbolication to work on Android, you need to set the
    following linker option:
    * Go to "Project | Options..."
    * Select the target "All configurations - Android platform"
    * Go to the page "Delphi Compiler | Linking"
    * Set "Options passed to the LD linker" to:
        --version-script=goExports.vsr

    Make sure goExports.vsr is available in the search path, or set it using
    an absolute or relative path, as in:
        --version-script=..\goExports.vsr

    If you only want symbolication for your Release (play store) build, then
    select the target "Release configuration - Android platform" instead (or any
    other configuration you want). }
  TgoExceptionReporter = class
  {$REGION 'Internal Declarations'}
  private class var
    FInstance: TgoExceptionReporter;
    class function GetExceptionHandler: TgoExceptionEvent; static;
    class function GetMaxCallStackDepth: Integer; static;
    class procedure SetMaxCallStackDepth(const Value: Integer); static;
  private
    FMaxCallStackDepth: Integer;
    FModuleAddress: UIntPtr;
    FReportingException: Boolean;
  private
    constructor InternalCreate(const ADummy: Integer = 0);
    procedure ReportException(const AExceptionObject: TObject;
      const AExceptionAddress: Pointer);
  private
    class function GetCallStack(const AStackInfo: Pointer): TgoCallStack; static;
    class function GetCallStackEntry(var AEntry: TgoCallStackEntry): Boolean; static;
  private
    { Global hooks }
    procedure GlobalHandleException(Sender: TObject; E: Exception);
    class procedure GlobalExceptionAcquiredHandler(Obj: {$IFDEF AUTOREFCOUNT}TObject{$ELSE}Pointer{$ENDIF}); static;
    class procedure GlobalExceptHandler(ExceptObject: TObject; ExceptAddr: Pointer); static;
    class function GlobalGetExceptionStackInfo(P: PExceptionRecord): Pointer; static;
    class procedure GlobalCleanUpStackInfo(Info: Pointer); static;
  private
    function internalBuildExceptionReport(const AExceptionObject: TObject; const AExceptionAddress: Pointer): IgoExceptionReport; //https://github.com/grijjy/JustAddCode/issues/3
  {$ENDREGION 'Internal Declarations'}
  public
    class function BuildExceptionReport(const AExceptionObject: TObject; const AExceptionAddress: Pointer): IgoExceptionReport; //https://github.com/grijjy/JustAddCode/issues/3
  public
    { Don't call the constructor manually. This is a singleton. }
    constructor Create;

    { Set to Application.OnException handler event to this value to report
      unhandled exceptions in the main (UI) thread.
      For example:
        Application.OnException := TgoExceptionReporter.ExceptionHandler; }
    class property ExceptionHandler: TgoExceptionEvent read GetExceptionHandler;

    { Maximum depth of the call stack that is retrieved when an exception
      occurs. Defaults to 20.

      Every time an exception is raised, we retrieve a call stack. This adds a
      little overhead, but raising exceptions is already an "expensive"
      operation anyway.

      You can limit this overhead by decreasing the maximum number of entries
      in the call stack. You can also increase this number of you want a more
      detailed call stack.

      Set to 0 to disable call stacks altogether. }
    class property MaxCallStackDepth: Integer read GetMaxCallStackDepth write SetMaxCallStackDepth;
  end;

implementation

uses
  System.Classes,
  {$IF Defined(IOS) or Defined(ANDROID)}
  Posix.Dlfcn,
  Posix.Stdlib,
  {$ENDIF}
  Grijjy.SymbolTranslator;

type
  TgoExceptionReport = class(TInterfacedObject, IgoExceptionReport)
  private
    FCallStack: TgoCallStack;
    FExceptionLocation: TgoCallStackEntry;
    FExceptionMessage: String;
    FReport: String;
  private
    function BuildReport: String;
    class function AddressToString(const AAddress: UIntPtr): String; static;
  protected
    { IgoExceptionReport }
    function _GetExceptionMessage: String;
    function _GetExceptionLocation: TgoCallStackEntry;
    function _GetCallStack: TgoCallStack;
    function _GetReport: String;
  public
    constructor Create(const AExceptionMessage: String;
      const AExceptionLocation: TgoCallStackEntry;
      const ACallStack: TgoCallStack);
  end;

{ TgoExceptionReport }

class function TgoExceptionReport.AddressToString(
  const AAddress: UIntPtr): String;
begin
  {$IFDEF CPU64BITS}
  Result := '$' + IntToHex(AAddress, 16);
  {$ELSE}
  Result := '$' + IntToHex(AAddress, 8);
  {$ENDIF}
end;

function TgoExceptionReport.BuildReport: String;
var
  SB: TStringBuilder;
  Entry: TgoCallStackEntry;
begin
  SB := TStringBuilder.Create;
  try
    SB.AppendLine(FExceptionMessage);
    SB.Append('At address: ');
    SB.Append(AddressToString(FExceptionLocation.CodeAddress));
    if (FExceptionLocation.RoutineName <> '') then
    begin
      SB.Append(' (');
      SB.Append(FExceptionLocation.RoutineName);
      SB.Append(' + ');
      SB.Append(FExceptionLocation.CodeAddress - FExceptionLocation.RoutineAddress);
      SB.AppendLine(')');
    end
    else
      SB.AppendLine;
    SB.AppendLine;

    if (FCallStack <> nil) then
    begin
      SB.AppendLine('Call stack:');
      for Entry in FCallStack do
      begin
        SB.AppendFormat('%-25s %s', [ExtractFilename(Entry.ModuleName),
          AddressToString(Entry.CodeAddress)]);

        if (Entry.RoutineName <> '') then
        begin
          SB.Append(' ');
          SB.Append(Entry.RoutineName);
          SB.Append(' + ');
          SB.Append(Entry.CodeAddress - Entry.RoutineAddress);
        end;
        SB.AppendLine;
      end;
    end;

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

constructor TgoExceptionReport.Create(const AExceptionMessage: String;
  const AExceptionLocation: TgoCallStackEntry; const ACallStack: TgoCallStack);
begin
  inherited Create;
  FExceptionMessage := AExceptionMessage;
  FExceptionLocation := AExceptionLocation;
  FCallStack := ACallStack;
end;

function TgoExceptionReport._GetCallStack: TgoCallStack;
begin
  Result := FCallStack;
end;

function TgoExceptionReport._GetExceptionLocation: TgoCallStackEntry;
begin
  Result := FExceptionLocation;
end;

function TgoExceptionReport._GetExceptionMessage: String;
begin
  Result := FExceptionMessage;
end;

function TgoExceptionReport._GetReport: String;
begin
  if (FReport = '') then
    FReport := BuildReport;
  Result := FReport;
end;

{ TgoExceptionReportMessage }

constructor TgoExceptionReportMessage.Create(const AReport: IgoExceptionReport);
begin
  inherited Create;
  FReport := AReport;
end;

{ TgoCallStackEntry }

procedure TgoCallStackEntry.Clear;
begin
  CodeAddress := 0;
  RoutineAddress := 0;
  ModuleAddress := 0;
  RoutineName := '';
  ModuleName := '';
end;

{ TgoExceptionReporter }

constructor TgoExceptionReporter.Create;
begin
  raise EInvalidOperation.Create('Invalid singleton constructor call');
end;

class function TgoExceptionReporter.GetExceptionHandler: TgoExceptionEvent;
begin
  { HandleException is usually called in response to the FMX
    Application.OnException event, which is called for exceptions that aren't
    handled in the main thread. This event is only fired for exceptions that
    occur in the main (UI) thread though. }
  if Assigned(FInstance) then
    Result := FInstance.GlobalHandleException
  else
    Result := nil;
end;

class function TgoExceptionReporter.GetMaxCallStackDepth: Integer;
begin
  if Assigned(FInstance) then
    Result := FInstance.FMaxCallStackDepth
  else
    Result := 20;
end;

class procedure TgoExceptionReporter.GlobalCleanUpStackInfo(Info: Pointer);
begin
  { Free memory allocated by GlobalGetExceptionStackInfo }
  if (Info <> nil) then
    FreeMem(Info);
end;

class procedure TgoExceptionReporter.GlobalExceptHandler(ExceptObject: TObject;
  ExceptAddr: Pointer);
begin
  if Assigned(FInstance) then
    FInstance.ReportException(ExceptObject, ExceptAddr);
end;

class procedure TgoExceptionReporter.GlobalExceptionAcquiredHandler(
  Obj: {$IFDEF AUTOREFCOUNT}TObject{$ELSE}Pointer{$ENDIF});
begin
  if Assigned(FInstance) then
    FInstance.ReportException(Obj, ExceptAddr);
end;

procedure TgoExceptionReporter.GlobalHandleException(Sender: TObject; E: Exception);
begin
  ReportException(E, ExceptAddr);
end;

constructor TgoExceptionReporter.InternalCreate(const ADummy: Integer);
{$IF Defined(IOS) or Defined(ANDROID)}
var
  Info: dl_info;
{$ENDIF}
begin
  inherited Create;
  FMaxCallStackDepth := 20;

  { Assign the global ExceptionAcquired procedure to our own implementation (it
    is nil by default). This procedure gets called for unhandled exceptions that
    happen in other threads than the main thread. }
  ExceptionAcquired := @GlobalExceptionAcquiredHandler;

  { The global ExceptProc method can be called for certain unhandled exception.
    By default, it calls the ExceptHandler procedure in the System.SysUtils
    unit, which shows the exception message and terminates the app.
    We set it to our own implementation. }
  ExceptProc := @GlobalExceptHandler;

  { We hook into the global static GetExceptionStackInfoProc and
    CleanUpStackInfoProc methods of the Exception class to provide a call stack
    for the exception. These methods are unassigned by default.
    The Exception.GetExceptionStackInfoProc method gets called soon after the
    exception is created, before the call stack is unwound. This is the only
    place where we can get the call stack at the point closest to the exception
    as possible. The Exception.CleanUpStackInfoProc just frees the memory
    allocated by GetExceptionStackInfoProc. }
  Exception.GetExceptionStackInfoProc := GlobalGetExceptionStackInfo;
  Exception.CleanUpStackInfoProc := GlobalCleanUpStackInfo;

  {$IF Defined(IOS) or Defined(ANDROID)}
  { Get address of current module. We use this to see if an entry in the call
    stack is part of this module.
    We use the dladdr API as a "trick" to get the address of this method, which
    is obviously part of this module. }
  if (dladdr(UIntPtr(@TgoExceptionReporter.InternalCreate), Info) <> 0) then
    FModuleAddress := UIntPtr(Info.dli_fbase);
  {$ENDIF}
end;

//https://github.com/grijjy/JustAddCode/issues/3
function TgoExceptionReporter.internalBuildExceptionReport(const AExceptionObject: TObject; const AExceptionAddress: Pointer): IgoExceptionReport;
var
  E: Exception;
  ExceptionMessage: String;
  CallStack: TgoCallStack;
  ExceptionLocation: TgoCallStackEntry;
  I: Integer;
begin

  CallStack := nil;
  if (AExceptionObject = nil) then
    ExceptionMessage := 'Unknown Error'
  else if (AExceptionObject is Exception) then
  begin
    E := Exception(AExceptionObject);
    ExceptionMessage := E.Message;
    if (E.StackInfo <> nil) then
    begin
      CallStack := GetCallStack(E.StackInfo);
      for I := 0 to Length(Callstack) - 1 do
      begin
        { If entry in call stack is for this module, then try to translate
          the routine name to Pascal. }
        if (CallStack[I].ModuleAddress = FModuleAddress) then
          CallStack[I].RoutineName := goCppSymbolToPascal(CallStack[I].RoutineName);
      end;
    end;
  end
  else
    ExceptionMessage := 'Unknown Error (' + AExceptionObject.ClassName + ')';

  ExceptionLocation.Clear;
  ExceptionLocation.CodeAddress := UIntPtr(AExceptionAddress);
  GetCallStackEntry(ExceptionLocation);
  if (ExceptionLocation.ModuleAddress = FModuleAddress) then
    ExceptionLocation.RoutineName := goCppSymbolToPascal(ExceptionLocation.RoutineName);

  result := TgoExceptionReport.Create(ExceptionMessage, ExceptionLocation, CallStack);

end;

//https://github.com/grijjy/JustAddCode/issues/3
procedure TgoExceptionReporter.ReportException(const AExceptionObject: TObject;
  const AExceptionAddress: Pointer);
var
  Report: IgoExceptionReport;
begin
  { Ignore exception that occur while we are already reporting another
    exception. That can happen when the original exception left the application
    in such a state that other exceptions would happen (cascading errors). }
  if (FReportingException) then
    Exit;

  FReportingException := True;
  try

    Report := internalBuildExceptionReport(AExceptionObject, AExceptionAddress);
    try
      TMessageManager.DefaultManager.SendMessage(Self,
        TgoExceptionReportMessage.Create(Report));
    except
      { Ignore any exceptions in the report message handler. }
    end;
  finally
    FReportingException := False;
  end;
end;

//https://github.com/grijjy/JustAddCode/issues/3
class function TgoExceptionReporter.BuildExceptionReport(const AExceptionObject: TObject; const AExceptionAddress: Pointer): IgoExceptionReport;
begin
  if Assigned(FInstance) then
    result := FInstance.internalBuildExceptionReport(AExceptionObject, AExceptionAddress)
  else
    result := nil;
end;

class procedure TgoExceptionReporter.SetMaxCallStackDepth(const Value: Integer);
begin
  if Assigned(FInstance) then
    FInstance.FMaxCallStackDepth := Value;
end;

{$IF Defined(IOS)}

(*****************************************************************************)
(*** iOS specific ************************************************************)
(*****************************************************************************)

const
  libSystem = '/usr/lib/libSystem.dylib';

function backtrace(buffer: PPointer; size: Integer): Integer;
  external libSystem name 'backtrace';

function cxa_demangle(const mangled_name: MarshaledAString;
  output_buffer: MarshaledAString; length: NativeInt;
  out status: Integer): MarshaledAString; cdecl;
  external libSystem name '__cxa_demangle';

type
  TCallStack = record
    { Number of entries in the call stack }
    Count: Integer;

    { The entries in the call stack }
    Stack: array [0..0] of UIntPtr;
  end;
  PCallStack = ^TCallStack;

class function TgoExceptionReporter.GlobalGetExceptionStackInfo(
  P: PExceptionRecord): Pointer;
var
  CallStack: PCallStack;
begin
  { Don't call into FInstance here. That would only add another entry to the
    call stack. Instead, retrieve the entire call stack from within this method.
    Just return nil if we are already reporting an exception, or call stacks
    are disabled. }
  if (FInstance = nil) or (FInstance.FReportingException) or (FInstance.FMaxCallStackDepth <= 0) then
    Exit(nil);

  { Allocate a PCallStack record large enough to hold just MaxCallStackDepth
    entries }
  GetMem(CallStack, SizeOf(Integer{TCallStack.Count}) +
    FInstance.FMaxCallStackDepth * SizeOf(Pointer));

  { Use backtrace API to retrieve call stack }
  CallStack.Count := backtrace(@CallStack.Stack, FInstance.FMaxCallStackDepth);
  Result := CallStack;
end;

class function TgoExceptionReporter.GetCallStack(
  const AStackInfo: Pointer): TgoCallStack;
var
  CallStack: PCallStack;
  I: Integer;
begin
  { Convert TCallStack to TgoCallStack }
  CallStack := AStackInfo;
  SetLength(Result, CallStack.Count);
  for I := 0 to CallStack.Count - 1 do
  begin
    Result[I].CodeAddress := CallStack.Stack[I];
    GetCallStackEntry(Result[I]);
  end;
end;

{$ELSEIF Defined(ANDROID)}

(*****************************************************************************)
(*** Android specific ********************************************************)
(*****************************************************************************)

type
  TGetFramePointer = function: NativeUInt; cdecl;

const
  { We need a function to get the frame pointer. The address of this frame
    pointer is stored in register R7.
    In assembly code, the function would look like this:
      ldr R0, [R7]       // Retrieve frame pointer
      bx  LR             // Return to caller
    The R0 register is used to store the function result.
    The "bx LR" line means "return to the address stored in the LR register".
    The LR (Link Return) register is set by the calling routine to the address
    to return to.

    We could create a text file with this code, assemble it to a static library,
    and link that library into this unit. However, since the routine is so
    small, it assembles to just 8 bytes, which we store in an array here. }
  GET_FRAME_POINTER_CODE: array [0..7] of Byte = (
    $00, $00, $97, $E5,  // ldr R0, [R7]
    $1E, $FF, $2F, $E1); // bx  LR

var
  { Now define a variable of a procedural type, that is assigned to the
    assembled code above }
  GetFramePointer: TGetFramePointer = @GET_FRAME_POINTER_CODE;

function cxa_demangle(const mangled_name: MarshaledAString;
  output_buffer: MarshaledAString; length: NativeInt;
  out status: Integer): MarshaledAString; cdecl;
  external 'libgnustl_static.a' name '__cxa_demangle';

type
  { For each entry in the call stack, we save 7 values for inspection later.
    See GlobalGetExceptionStackInfo for explaination. }
  TStackValues = array [0..6] of UIntPtr;

type
  TCallStack = record
    { Number of entries in the call stack }
    Count: Integer;

    { The entries in the call stack }
    Stack: array [0..0] of TStackValues;
  end;
  PCallStack = ^TCallStack;

class function TgoExceptionReporter.GlobalGetExceptionStackInfo(
  P: PExceptionRecord): Pointer;
const
  { On most Android systems, each thread has a stack of 1MB }
  MAX_STACK_SIZE = 1024 * 1024;
var
  MaxCallStackDepth, Count: Integer;
  FramePointer, MinStack, MaxStack: UIntPtr;
  Address: Pointer;
  CallStack: PCallStack;
begin
  { Don't call into FInstance here. That would only add another entry to the
    call stack. Instead, retrieve the entire call stack from within this method.
    Just return nil if we are already reporting an exception, or call stacks
    are disabled. }
  if (FInstance = nil) or (FInstance.FReportingException) or (FInstance.FMaxCallStackDepth <= 0) then
    Exit(nil);

  MaxCallStackDepth := FInstance.FMaxCallStackDepth;

  { Allocate a PCallStack record large enough to hold just MaxCallStackDepth
    entries }
  GetMem(CallStack, SizeOf(Integer{TCallStack.Count}) +
    MaxCallStackDepth * SizeOf(TStackValues));

  (*We manually walk the stack to create a stack trace. This is possible since
    Delphi creates a stack frame for each routine, by starting each routine with
    a prolog. This prolog is similar to the one used by the iOS ABI (Application
    Binary Interface, see
    https://developer.apple.com/library/content/documentation/Xcode/Conceptual/iPhoneOSABIReference/Articles/ARMv7FunctionCallingConventions.html

    The prolog looks like this:
    * Push all registers that need saving. Always push the R7 and LR registers.
    * Set R7 (frame pointer) to the location in the stack where the previous
      value of R7 was just pushed.

    A minimal prolog (used in many small functions) looks like this:

      push {R7, LR}
      mov  R7, SP

    We are interested in the value of the LR (Link Return) register. This
    register contains the address to return to after the routine finishes. We
    can use this address to look up the symbol (routine name). Using the
    example prolog above, we can get to the LR value and walk the stack like
    this:
    1. Set FramePointer to the value of the R7 register.
    2. At this location in the stack, you will find the previous value of the
       R7 register. Lets call this PreviousFramePointer.
    3. At the next location in the stack, we will find the LR register. Add its
       value to our stack trace so we can use it later to look up the routine
       name at this address.
    4. Set FramePointer to PreviousFramePointer and go back to step 2, until
       FramePointer is 0 or falls outside of the stack.

    Unfortunately, Delphi doesn't follow the iOS ABI exactly, and it may push
    other registers between R7 and LR. For example:

      push {R4, R5, R6, R7, R8, R9, LR}
      add  R7, SP, #12

    Here, it pushed 3 registers (R4-R6) before R7, so in the second line it sets
    R7 to point 12 bytes into the stack (so it still points to the previous R7,
    as required). However, it also pushed registers R8 and R9, before it pushes
    the LR register. This means we cannot assume that the LR register will be
    directly located after the R7 register in the stack. There may be (up to 6)
    registers in between. We don't know which one represents LR, so we just
    store all 7 values after R7, and later try to figure out which one
    represents LR (in the GetCallStack method). *)
  FramePointer := GetFramePointer;

  { The stack grows downwards, so all entries in the call stack leading to this
    call have addresses greater than FramePointer. We don't know what the start
    and end address of the stack is for this thread, but we do know that the
    stack is at most 1MB in size, so we only investigate entries from
    FramePointer to FramePointer + 1MB. }
  MinStack := FramePointer;
  MaxStack := MinStack + MAX_STACK_SIZE;

  { Now we can walk the stack using the algorithm described above. }
  Count := 0;
  while (Count < MaxCallStackDepth) and (FramePointer <> 0)
    and (FramePointer >= MinStack) and (FramePointer < MaxStack) do
  begin
    { The first value at FramePointer contains the previous value of R7.
      Store the 7 values after that. }
    Address := Pointer(FramePointer + SizeOf(UIntPtr));
    Move(Address^, CallStack.Stack[Count], SizeOf(TStackValues));
    Inc(Count);

    { The FramePointer points to the previous value of R7, which contains the
      previous FramePointer. }
    FramePointer := PNativeUInt(FramePointer)^;
  end;

  CallStack.Count := Count;
  Result := CallStack;
end;

class function TgoExceptionReporter.GetCallStack(
  const AStackInfo: Pointer): TgoCallStack;
var
  CallStack: PCallStack;
  I, J: Integer;
  FoundLR: Boolean;
begin
  { Convert TCallStack to TgoCallStack }
  CallStack := AStackInfo;
  SetLength(Result, CallStack.Count);
  for I := 0 to CallStack.Count - 1 do
  begin
    { For each entry in the call stack, we have up to 7 values now that may
      represent the LR register. Most of the time, it will be the first value.
      We try to find the correct LR value by passing up to all 7 addresses to
      the dladdr API (by calling GetCallStackEntry). If the API call succeeds,
      we assume we found the value of LR. However, this is not fool proof
      because an address value we pass to dladdr may be a valid code address,
      but not the LR value we are looking for.

      Also, the LR value contains the address of the next instruction after the
      call instruction. Delphi usually uses the BL or BLX instruction to call
      another routine. These instructions takes 4 bytes, so LR will be set to 4
      bytes after the BL(X) instruction (the return address). However, we want
      to know at what address the call was made, so we need to subtract 4
      bytes.

      There is one final complication here: the lowest bit of the LR register
      indicates the mode the CPU operates in (ARM or Thumb). We need to clear
      this bit to get to the actual address, by AND'ing it with "not 1". }
    FoundLR := False;
    for J := 0 to Length(CallStack.Stack[I]) - 1 do
    begin
      Result[I].CodeAddress := (CallStack.Stack[I, J] and not 1) - 4;
      if GetCallStackEntry(Result[I]) then
      begin
        { Assume we found LR }
        FoundLR := True;
        Break;
      end;
    end;

    if (not FoundLR) then
      { None of the 7 values were valid.
        Set CodeAddress to 0 to signal we couldn't find LR. }
      Result[I].CodeAddress := 0;
  end;
end;

{$ELSE}

(*****************************************************************************)
(*** Non iOS/Android *********************************************************)
(*****************************************************************************)

class function TgoExceptionReporter.GlobalGetExceptionStackInfo(
  P: PExceptionRecord): Pointer;
begin
  { Call stacks are only supported on iOS and Android }
  Result := nil;
end;

class function TgoExceptionReporter.GetCallStack(
  const AStackInfo: Pointer): TgoCallStack;
begin
  { Call stacks are only supported on iOS and Android }
  Result := nil;
end;

class function TgoExceptionReporter.GetCallStackEntry(
  var AEntry: TgoCallStackEntry): Boolean;
begin
  { Call stacks are only supported on iOS and Android }
  Result := False;
end;

{$ENDIF}

{$IF Defined(IOS) or Defined(ANDROID)}
class function TgoExceptionReporter.GetCallStackEntry(
  var AEntry: TgoCallStackEntry): Boolean;
var
  Info: dl_info;
  Status: Integer;
  Demangled: MarshaledAString;
begin
  Result := (dladdr(AEntry.CodeAddress, Info) <> 0) and (Info.dli_saddr <> nil);
  if (Result) then
  begin
    AEntry.RoutineAddress := UIntPtr(Info.dli_saddr);
    AEntry.ModuleAddress := UIntPtr(Info.dli_fbase);

    Demangled := cxa_demangle(Info.dli_sname, nil, 0, Status);
    if (Demangled = nil) then
      AEntry.RoutineName := String(Info.dli_sname)
    else
    begin
      AEntry.RoutineName := String(Demangled);
      Posix.Stdlib.free(Demangled);
    end;

    AEntry.ModuleName := String(Info.dli_fname);
  end;
end;
{$ENDIF}

initialization
  TgoExceptionReporter.FInstance := TgoExceptionReporter.InternalCreate;

finalization
  FreeAndNil(TgoExceptionReporter.FInstance);

end.
