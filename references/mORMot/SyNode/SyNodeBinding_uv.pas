/// `uv` module support bindings for SyNode
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeBinding_uv;

interface
{$I Synopse.inc}
{$I SyNode.inc}

implementation
uses
  {$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  SynCommons,
  SpiderMonkey,
  SyNode;
//See https://github.com/libuv/libuv#documentation for documentation
// todo: normal realization

function errname(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  err: Int32;
const
  sInvalidCall = 'usage: errname(err: Integer);';
begin
  try
    Result := True;
    in_argv := vp.argv;
    if (argc < 1) or (in_argv[0].isInteger) then
      raise ESMException.Create(sInvalidCall);
    err := in_argv[0].asInteger;
    if err >= 0 then
      raise ESMException.Create('err >= 0');
    //todo uv_err_name(err)
    vp.rval := cx.NewJSString('Fake').ToJSVal;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function SyNodeBindingProc_uv(const Engine: TSMEngine;
  const bindingNamespaceName: SynUnicode): jsval;
var
  obj: PJSRootedObject;
  cx: PJSContext;
begin
  cx := Engine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  try
    obj.ptr.DefineFunction(cx, 'errname', errname, 1, JSPROP_READONLY or JSPROP_PERMANENT);
    //todo
//    obj.ptr.DefineProperty(cx, 'UV_err', SimpleVariantToJSval(cx, UV_err), JSPROP_READONLY or JSPROP_PERMANENT);
    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj);
  end;
end;

initialization
  TSMEngineManager.RegisterBinding('uv', SyNodeBindingProc_uv);

end.
