unit SyNodePluginIntf;

interface

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER
{$I SyNode.inc}   //define WITHASSERT

uses
  SynCommons,
  SpiderMonkey;

type
  {$ifdef USERECORDWITHMETHODS}TSMPluginRec = record
    {$else}TSMPluginRec = object{$endif}
    cx: PJSContext;
    Exp: PJSRootedObject;
    Req: PJSRootedObject;
    Module: PJSRootedObject;
    filename: PWideChar;
    dirname: PWideChar;
    constructor Create(aCx: PJSContext; aExports_: PJSRootedObject; aRequire: PJSRootedObject; aModule: PJSRootedObject; _filename: PWideChar; _dirname: PWideChar);
    function require(aFileName: string): PJSObject;
  end;

  TCustomSMPlugin = class
  protected
    fCx: PJSContext; 
    procedure UnInit; virtual;
    procedure Init(const rec: TSMPluginRec); virtual;
  public
    constructor Create(aCx: PJSContext; aExports_: PJSRootedObject; aRequire: PJSRootedObject; aModule: PJSRootedObject; _filename: PWideChar; _dirname: PWideChar);
    destructor Destroy; override;
  end;

  TThreadRec = record
    threadID: TThreadID;
    plugin: TCustomSMPlugin;
  end;
  TCustomSMPluginType = class of TCustomSMPlugin;

type
  TNsmFunction = function(cx: PJSContext; argc: uintN; vals: PjsvalVector; thisObj, calleeObj: PJSObject): jsval; cdecl;

const
  ptVoid = 0;
  ptInt = 1;
  ptStr = 2;
  ptObj = 3;
  ptBuffer = 100;
  ptAny = 500;

type
  PNSMCallInfo = ^TNSMCallInfo;
  TNSMCallInfo = record
    func: TNsmFunction;
    argc: Integer;
    argt: PInt64Array;
  end;

  TNSMCallInfoArray = array of TNSMCallInfo;

  /// Helper to call a native function depending on JS function arguments.
  // As a side effect will verify JS function arguments types. Can be used e.g. as:
  // ! const
  // !   overload1Args: array [0..0] of uintN = ( ptInt );
  // !   overload2Args: array [0..1] of uintN = ( ptInt, SyNodePluginIntf.ptStr );
  // !   overloads: array [0..1] of TNSMCallInfo = (
  // !     (func: @SLReadLn_impl; argc: Length(overload1Args); argt: @overload1Args),
  // !     (func: @SLReadLn_impl; argc: Length(overload2Args); argt: @overload2Args));
  // ! begin
  // !   Result := nsmCallFunc(cx, argc, vp, @overloads, Length(overloads));
  // ! end;
  function nsmCallFunc(cx: PJSContext; argc: uintN;  var vp: JSArgRec; const overloads: TNSMCallInfoArray; overloadsCount: Integer = 1; isConstructor: Boolean = false): Boolean; cdecl;

const
  StaticROAttrs = JSPROP_ENUMERATE or JSPROP_READONLY or JSPROP_PERMANENT;

implementation

uses
  SysUtils;

function isParamCorrect(paramType: uintN; val: jsval): Boolean;
begin
  case paramType of
    ptVoid: result := val.isVoid;
    ptInt: result := val.isInteger;
    ptStr: result := val.isString;
    ptObj: result := val.isObject;
    ptBuffer: result := val.isObject and (val.asObject.IsArrayBufferObject or val.asObject.IsArrayBufferViewObject);
    ptAny: result := True;
    else result := false;
  end;
end;

function nsmCallFunc(cx: PJSContext; argc: uintN; var vp: JSArgRec; const overloads: TNSMCallInfoArray; overloadsCount: Integer = 1; isConstructor: Boolean = false): Boolean; cdecl;
var
  thisObj, calleeObj: PJSObject;
  vals: PjsvalVector;
  i,j: Integer;
  overloadCase: PNSMCallInfo;
  IsCalled, IsCorrect: Boolean;
begin
  Result := False;
  try
    if isConstructor xor vp.IsConstructing then
      raise ESMException.Create('JS_IS_CONSTRUCTING');

    thisObj := vp.thisObject[cx];
    calleeObj := vp.calleObject;
    vals := vp.argv;
    IsCalled := false;

    for i := 0 to overloadsCount - 1 do begin
      overloadCase := @overloads[i];
      if (overloadCase <> nil) then begin
        if argc = overloadCase.argc then begin
          IsCorrect := true;
          for j := 0 to overloadCase.argc - 1 do begin
            IsCorrect := isParamCorrect(overloadCase.argt[j], vals[j]);
            if not IsCorrect then Break;
          end;
          if IsCorrect then begin
            vp.rval := overloadCase.func(cx, argc, vals, thisObj, calleeObj);
            IsCalled := True;
            Break;
          end;
        end;
      end;
    end;

    if not IsCalled then
      raise ESMException.CreateUTF8('There is no overloaded function "%" with such a list of arguments', [calleeObj.GetFunctionId().ToSynUnicode(cx)]);

    Result := True;
  except
    on E: Exception do
    begin
      JSError(cx, E);
    end;
  end;
end;

{ TCustomSMPlugin }


destructor TCustomSMPlugin.Destroy;
begin
  fCx.BeginRequest;
  try
    UnInit;
  finally
    fCx.EndRequest;
  end;
  inherited;
end;

procedure TCustomSMPlugin.Init(const rec: TSMPluginRec);
begin
end;

procedure TCustomSMPlugin.UnInit;
begin
end;

constructor TCustomSMPlugin.Create(aCx: PJSContext; aExports_, aRequire,
  aModule: PJSRootedObject; _filename, _dirname: PWideChar);
var
  rec: TSMPluginRec;
begin
  fCx := aCx;
  rec.Create(aCx, aExports_, aRequire, aModule, _filename, _dirname);
  fCx.BeginRequest;
  try
    Init(rec);
  finally
    fCx.EndRequest;
  end;

end;

constructor TSMPluginRec.Create(aCx: PJSContext; aExports_: PJSRootedObject; aRequire: PJSRootedObject; aModule: PJSRootedObject; _filename: PWideChar; _dirname: PWideChar);
begin
  cx := aCx;
  Exp := aExports_;
  Req := aRequire;
  Module := aModule;
  filename := _filename;
  dirname := _dirname;
end;

function TSMPluginRec.require(aFileName: string): PJSObject;
var
  arg, rval: jsval;
begin
  arg := cx.NewJSString(aFileName).ToJSVal;
  if not Module.ptr.CallFunction(cx, req.ptr, 1, @arg, rval) then
     raise ESMException.Create('Error require '+aFileName);
  Result := rval.asObject;
end;

end.
