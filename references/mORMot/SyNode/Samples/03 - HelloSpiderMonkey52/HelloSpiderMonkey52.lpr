program HelloSpiderMonkey52;
{
  This example has translated from C++ hello world example taken on
  https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/How_to_embed_the_JavaScript_engine
}
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  SpiderMonkey;

const
  script = '''Hello '' + ''world, it is ''+new Date()';
  filename = 'noname';

var
  global_ops: JSClassOps = (
    addProperty:        nil;
    delProperty:        nil;
    getProperty:        nil;
    setProperty:        nil;
    enumerate:          nil;
    resolve:            nil;
    mayResolve:         nil;
    finalize:           nil;
    call:               nil;
    hasInstance:        nil;
    construct:          nil;
    trace:              nil;//@JS_GlobalObjectTraceHook;
  );

  // The class of the global object.
  global_class: JSClass = (
    name:               'global';
    flags:              JSCLASS_GLOBAL_FLAGS;
    cOps:               @global_ops;
  );

  cx: PJSContext;
  options: JS_CompartmentOptions;
  global: PJSRootedObject;
  val: jsval;
  rval: PJSRootedValue;
  oldCprt: PJSCompartment;
  lineno: Integer = 1;
  opts: PJSCompileOptions;
  ok: Boolean;
  str: PJSString;

  I: Integer;
  Frames: PPointer;

begin
  try
    JS_Init();
    cx := JSContext.CreateNew(8 * 1024 * 1024);
    try
      cx^.BeginRequest(); // In practice, you would want to exit this any
      try                  // time you're spinning the event loop
        // Scope for our various stack objects (JSAutoRequest, RootedObject), so they all go
        // out of scope before we JS_DestroyContext.
        global := cx^.NewRootedObject(cx^.NewGlobalObject(@global_class));
        if (global = nil) then
            Halt(1);
        rval := cx^.NewRootedValue(val);
        oldCprt := cx^.EnterCompartment(global^.ptr);
        try // Scope for JSAutoCompartment
          cx^.InitStandardClasses(global^.ptr);
          opts := cx^.NewCompileOptions();
          opts^.filename := filename;
          //opts^.?? := lineno;
           ok := cx^.EvaluateScript(opts, script, Length(script), rval^.ptr);
          if (not ok) then
            Halt(1);
        finally
          cx^.LeaveCompartment(oldCprt);
        end;
        str := rval^.ptr.asJSString;
        WriteLn(str^.ToAnsi(cx));
      finally
        cx^.EndRequest();
      end;
    finally
      cx^.Destroy();
      JS_ShutDown();
    end;
    Halt(0);
  except
    on E: Exception do begin
      Writeln(E.Message);
      Writeln(BackTraceStrFunc(ExceptAddr));
      Frames := ExceptFrames;
      for I := 0 to ExceptFrameCount - 1 do
        Writeln(BackTraceStrFunc(Frames[I]));
    end;
  end;
end.
