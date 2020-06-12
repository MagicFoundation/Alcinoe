/// `os` module support bindings for SyNode
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeBinding_os;

interface
{$I Synopse.inc}
{$I SyNode.inc}
uses
  SysUtils,
  SynCommons,
  SyNode, SpiderMonkey;


implementation

function os_getHostname(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
begin
  result := true;
  try
    vp.rval := cx.NewJSString(ExeVersion.Host).ToJSVal;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;


function SyNodeBindingProc_os(const aEngine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;
var
  obj: PJSRootedObject;
  cx: PJSContext;
const
  props = JSPROP_ENUMERATE or JSPROP_ENUMERATE or JSPROP_PERMANENT;
begin
  cx := aEngine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  try
    obj.ptr.DefineFunction(cx, 'getHostname', os_getHostname, 0, props);

    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj);
  end;
end;

initialization
  TSMEngineManager.RegisterBinding('os', SyNodeBindingProc_os);

end.
