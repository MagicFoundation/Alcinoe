unit uMathModule;

interface

uses 
  SyNodePluginIntf;

type
  TSMMathPlugin = class(TCustomSMPlugin)
    procedure UnInit; override;
    procedure Init(const rec: TSMPluginRec); override;
  end;

implementation

uses
  SpiderMonkey, SysUtils;

{ TSMMathPlugin }

type
  TMathFun = function (const X, Y : Extended) : Extended;

function DoMathFun(cx: PJSContext; argc: uintN; var vp: JSArgRec; fun: TMathFun): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  aVal1, aVal2, aRes: Double;
  val: jsval;
begin
  try
    in_argv := vp.argv;
    if (argc<>2) then
      raise ESMException.Create('invalid args count for math function. Required (Number, Number)')
    else if not in_argv[0].isNumber or not in_argv[1].isNumber then
      raise ESMException.CreateUTF8('invalid args for math function. Required (Number, Number) but got (%, %)', [integer(in_argv[0].ValType(cx)), integer(in_argv[1].ValType(cx))]);

    if in_argv[0].isInteger then
      aVal1 := in_argv[0].asInteger
    else
      aVal1 := in_argv[0].asDouble;
    if in_argv[1].isInteger then
      aVal2 := in_argv[1].asInteger
    else
      aVal2 := in_argv[1].asDouble;

    aRes := fun(aVal1, aVal2);
    val.asDouble := aRes;

    vp.rval := val;
    Result := True;
  except
    on E: Exception do begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;

end;

function DoAdd(const X, Y : Extended) : Extended;
begin
  result := X+Y;
end;

function DoSub(const X, Y : Extended) : Extended;
begin
  result := X-Y;
end;

function DoMul(const X, Y : Extended) : Extended;
begin
  result := X*Y;
end;

function DoDiv(const X, Y : Extended) : Extended;
begin
  if Y = 0 then
    raise ESMException.Create('division by zero');
  result := X/Y;
end;

function math_add(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
begin
  result := DoMathFun(cx, argc, vp, DoAdd);
end;

function math_sub(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
begin
  result := DoMathFun(cx, argc, vp, DoSub);
end;

function math_mul(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
begin
  result := DoMathFun(cx, argc, vp, DoMul);
end;

function math_div(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
begin
  result := DoMathFun(cx, argc, vp, DoDiv);
end;

procedure TSMMathPlugin.Init(const rec: TSMPluginRec);
var val: jsval;
begin
  inherited;
  // this method is called when SM Engine required this plugin
  // here you can define exports (rec.Exp) properties
  // or redefime module (rec.Module) property 'exports'
  rec.Exp.ptr.DefineFunction(rec.cx, 'add', math_add, 2, StaticROAttrs);
  rec.Exp.ptr.DefineFunction(rec.cx, 'sub', math_sub, 2, StaticROAttrs);
  rec.Exp.ptr.DefineFunction(rec.cx, 'mul', math_mul, 2, StaticROAttrs);
  rec.Exp.ptr.DefineFunction(rec.cx, 'div', math_div, 2, StaticROAttrs);
  val.asDouble := pi;
  rec.Exp.ptr.DefineProperty(rec.cx, 'pi', val, StaticROAttrs, nil, nil);
end;

procedure TSMMathPlugin.UnInit;
begin
  inherited;
  // this method is called when SM Engine destroyed
  // here you can finaliza your plugin if it is needed
end;

end.
