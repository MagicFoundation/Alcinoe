unit ufrmSM45Demo;

interface

uses
  {$IFNDEF LCL}Windows,{$ELSE}LclIntf, LMessages, LclType, LResources,{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,

  SynCommons,
  SpiderMonkey,
  SyNode,
  SyNodeProto,
  SyNodeSimpleProto;

const
  WM_DEBUG_INTERRUPT = WM_USER + 1;
type

  { TfrmSM45Demo }

  TfrmSM45Demo = class(TForm)
    mSource: TMemo;
    mResult: TMemo;
    btnEvaluate: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnEvaluateClick(Sender: TObject);
  private
    procedure cmd_Itenterupt(var aMessage: TMessage); message WM_DEBUG_INTERRUPT;
  protected
    FSMManager: TSMEngineManager;
    FEngine: TSMEngine;
    procedure DoOnCreateNewEngine(const aEngine: TSMEngine);
    function DoOnGetEngineName(const aEngine: TSMEngine): RawUTF8;
    // a handler, called from a debugger thread to interrupt a current thread
    // in this example will send a WM_DEBUG_INTERRUPT message to a main window
    // main application thread catch a message and call FEngine.InterruptCallback
    procedure doInteruptInOwnThread;
    /// here we add features to debugger console
    // type ? in firefox console to get a feature help
    procedure DoOnJSDebuggerInit(const aEngine: TSMEngine);
  published
    property sources: TMemo read mSource;
    property results: TMemo read mResult;
    property evaluateButton: TButton read btnEvaluate;
    function toLog(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
  end;

var
  frmSM45Demo: TfrmSM45Demo;

implementation

{$R *.dfm}
{$I Synopse.inc}
{$I SynSM.inc}   // define SM_DEBUG JS_THREADSAFE CONSIDER_TIME_IN_Z
{$I SyNode.inc}   // define SM_DEBUG CONSIDER_TIME_IN_Z

procedure TfrmSM45Demo.FormCreate(Sender: TObject);
begin
  // create a JavaScript angine manager
  FSMManager := TSMEngineManager.Create(
    {$IFDEF CORE_MODULES_IN_RES}''{$ELSE}StringToUTF8(RelToAbs(ExeVersion.ProgramFilePath, '../../core_modules')){$ENDIF});
  // optionaly increase a max engine memory
  FSMManager.MaxPerEngineMemory := 512 * 1024 * 1024;
  // add a handler called every time new engine is created
  // inside a handler we can add a binding's to a native functions (implemented in Delphi)
  // and evaluate some initial JavaScripts
  FSMManager.OnNewEngine := DoOnCreateNewEngine;
  FSMManager.OnGetName := DoOnGetEngineName;
  FSMManager.OnDebuggerInit := DoOnJSDebuggerInit;
  // start a JavaScript debugger on the localhost:6000
  FSMManager.startDebugger('6000');
  // debugger can see engines created after startDebugger() call,
  // so we create a main engine after debugger is started
  // in this example we need only one engine (we are single-thread)
  FEngine := FSMManager.ThreadSafeEngine(nil);
end;

procedure TfrmSM45Demo.FormDestroy(Sender: TObject);
begin
  FSMManager.ReleaseCurrentThreadEngine;
  FSMManager.Free;
end;

procedure TfrmSM45Demo.btnEvaluateClick(Sender: TObject);
var
  res: jsval;
begin
  if FEngine = nil then
    raise Exception.Create('JS engine not initialized');
  // evaluate a text from mSource memo
  if mSource.SelText <> '' then
    FEngine.Evaluate(mSource.SelText, 'mSourceSelected.js', 1, res)
  else
    FEngine.Evaluate(mSource.lines.Text, 'mSource.js', 1, res);
end;

procedure TfrmSM45Demo.cmd_Itenterupt(var aMessage: TMessage);
begin
  if FEngine = nil then
    raise Exception.Create('JS engine not initialized');
{$IFDEF SM52}
  FEngine.cx.RequestInterruptCallback;
  FEngine.cx.CheckForInterrupt;
{$ELSE}
  FEngine.rt.InterruptCallback(FEngine.cx);
{$ENDIF}
end;

procedure TfrmSM45Demo.doInteruptInOwnThread;
begin
  PostMessage(Self.Handle, WM_DEBUG_INTERRUPT, 0, 0);
  {$IFNDEF FPC}
  Application.ProcessMessages;
  {$ENDIF}
end;

function TfrmSM45Demo.toLog(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
begin
  try
    if (vp.argv[0].isString) then
      mResult.lines.add(vp.argv[0].asJSString.ToString(cx))
    else
      raise ESMException.Create('toLog accept only String type of arg');
    result :=  true;
  except
    on E: Exception do
    begin
      Result := False;
      JSError(cx, E);
    end;
  end;
end;

type
  TStringsProto = class(TSMSimpleRTTIProtoObject)
  protected
    procedure InitObject(aParent: PJSRootedObject); override;
  end;

procedure TfrmSM45Demo.DoOnCreateNewEngine(const aEngine: TSMEngine);
begin
  // for main thread only. Worker threads do not need this
  if GetCurrentThreadId = MainThreadID then begin
    aEngine.doInteruptInOwnThread := doInteruptInOwnThread;
    // in XE actual class of TMemo.lines is TMemoStrings - let's force it to be a TStrings like
    aEngine.defineClass(mSource.lines.ClassType, TStringsProto, aEngine.GlobalObject);
    // define a propery mainForm in the JavaScript
    aEngine.GlobalObject.ptr.DefineProperty(aEngine.cx, 'mainForm',
      // proeprty value is a wrapper around the Self
      CreateJSInstanceObjForSimpleRTTI(aEngine.cx, Self, aEngine.GlobalObject),
      // we can enumerate this property, it read-only and can not be deleted
      JSPROP_ENUMERATE or JSPROP_READONLY or JSPROP_PERMANENT
    );
  end;
end;

function TfrmSM45Demo.DoOnGetEngineName(const aEngine: TSMEngine): RawUTF8;
begin
  if GetCurrentThreadId = MainThreadID then
    result := 'FormEngine';
end;

procedure TfrmSM45Demo.DoOnJSDebuggerInit(const aEngine: TSMEngine);
begin
//  aEngine.EvaluateModule(
//  {$IFDEF MSWINDOWS}
//    '..\..\..\..\DebuggerInit.js'
//  {$ELSE}
//    '../../../../DebuggerInit.js'
//  {$ENDIF}
//  );
end;

{ TStringsProto }
function TStringsTextWrite(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  this: PJSObject;
  proto: TSMCustomProtoObject;
  Instance: PSMInstanceRecord;
begin
  try
    this := vp.thisObject[cx];

    if IsProtoObject(cx, this, proto) then begin
      vp.rval := JSVAL_NULL;
      Result := True;
      exit;
    end;

    if not IsInstanceObject(cx, this, Instance) then
      raise ESMException.Create('No privat data!');

    TStrings(Instance.instance).Text := vp.argv[0].asJSString.ToString(cx);

    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      JSError(cx, E);
    end;
  end;
end;

function TStringsTextRead(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  this: PJSObject;
  proto: TSMCustomProtoObject;
  Instance: PSMInstanceRecord;
begin
  try
    this := vp.thisObject[cx];

    if IsProtoObject(cx, this, proto) then begin
      vp.rval := JSVAL_NULL;
      Result := True;
      exit;
    end;

    if not IsInstanceObject(cx, this, Instance) then
      raise ESMException.Create('No privat data!');

    vp.rval := SimpleVariantToJSval(cx, TStrings(Instance.instance).Text);
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      JSError(cx, E);
    end;
  end;
end;

procedure TStringsProto.InitObject(aParent: PJSRootedObject);
var
  idx: Integer;
begin
  inherited;
  idx := Length(FJSProps);

  SetLength(FJSProps, idx + 1);

  FJSProps[idx].flags := JSPROP_ENUMERATE or JSPROP_PERMANENT or JSPROP_SHARED;
  FJSProps[idx].Name := 'text';
  FJSProps[idx].setter.native.info := nil;
  FJSProps[idx].setter.native.op := TStringsTextWrite;
  FJSProps[idx].getter.native.info := nil;
  FJSProps[idx].getter.native.op := TStringsTextRead;

end;

end.
