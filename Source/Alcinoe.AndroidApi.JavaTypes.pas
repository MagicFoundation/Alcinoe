unit Alcinoe.AndroidApi.JavaTypes;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

type

  {*******************}
  JRuntime = interface;

  {*************************************}
  JRuntimeClass = interface(JObjectClass)
    ['{B466FF95-339F-4060-965B-44350FDC2686}']
    {class} function getRuntime: JRuntime; cdecl;
  end;
  [JavaSignature('java/lang/Runtime')]
  JRuntime = interface(JObject)
    ['{AB57684A-3434-4C09-ACD4-5EC5F220D02D}']
    //function exec(progArray: TJavaObjectArray<JString>): Jlang_Process; cdecl; overload;
    //function exec(progArray: TJavaObjectArray<JString>; envp: TJavaObjectArray<JString>): Jlang_Process; cdecl; overload;
    //function exec(progArray: TJavaObjectArray<JString>; envp: TJavaObjectArray<JString>; directory: JFile): Jlang_Process; cdecl; overload;
    //function exec(prog: JString): Jlang_Process; cdecl; overload;
    //function exec(prog: JString; envp: TJavaObjectArray<JString>): Jlang_Process; cdecl; overload;
    //function exec(prog: JString; envp: TJavaObjectArray<JString>; directory: JFile): Jlang_Process; cdecl; overload;
    procedure exit(code: Integer); cdecl;
    procedure gc; cdecl;
    procedure load(absolutePath: JString); cdecl;
    procedure loadLibrary(nickname: JString); cdecl;
    procedure runFinalization; cdecl;
    procedure runFinalizersOnExit(run: Boolean); cdecl; deprecated;
    procedure traceInstructions(enable: Boolean); cdecl;
    procedure traceMethodCalls(enable: Boolean); cdecl;
    function getLocalizedInputStream(stream: JInputStream): JInputStream; cdecl; deprecated;
    function getLocalizedOutputStream(stream: JOutputStream): JOutputStream; cdecl; deprecated;
    procedure addShutdownHook(hook: JThread); cdecl;
    function removeShutdownHook(hook: JThread): Boolean; cdecl;
    procedure halt(code: Integer); cdecl;
    function availableProcessors: Integer; cdecl;
    function freeMemory: Int64; cdecl;
    function totalMemory: Int64; cdecl;
    function maxMemory: Int64; cdecl;
  end;
  TJRuntime = class(TJavaGenericImport<JRuntimeClass, JRuntime>) end;

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.JavaTypes.JRuntime', TypeInfo(Alcinoe.AndroidApi.JavaTypes.JRuntime));
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.AndroidApi.JavaTypes','initialization');
  {$ENDIF}
  RegisterTypes;

end.