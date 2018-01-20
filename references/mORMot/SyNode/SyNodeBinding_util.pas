/// `util` module support bindings for SyNode
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeBinding_util;

interface
{$I Synopse.inc}
{$I SyNode.inc}
uses
  SysUtils,
  SynCommons,
  SyNode, SpiderMonkey;

function HasInstance(cx: PJSContext; argc: uintN; var vp: jsargRec; proto: JSProtoKey): Boolean; {$IFNDEF FPC}{$ifdef HASINLINE}inline;{$endif}{$ENDIF}// << this couses internal error on FPC 3.0.2

implementation

///This is dirty hack using Spider Monkey internals
function HasInstance(cx: PJSContext; argc: uintN; var vp: jsargRec; proto: JSProtoKey): Boolean; {$IFNDEF FPC}{$ifdef HASINLINE}inline;{$endif}{$ENDIF}// << this couses internal error on FPC 3.0.2
var
  in_argv: PjsvalVector;
  slotIndex: uint32;
const
  sInvalidCall = 'One argunent required';
begin
  Result := False;
  try
//    {$IFNDEF SM45}
//    raise ENotImplemented.Create('Check that slots order is correct and you can continue using this internal');
//    {$ENDIF}
    in_argv := vp.argv;
    if (argc <> 1) then
      raise ESMException.Create(sInvalidCall);
    slotIndex := JSCLASS_GLOBAL_APPLICATION_SLOTS + Ord(proto);
    vp.rval := SimpleVariantToJSval(cx, cx.CurrentGlobalOrNull.ReservedSlot[slotIndex].asObject.HasInstance(cx, in_argv[0]));
    Result := True;
  except
    on E: Exception do begin
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function isArrayBuffer(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := HasInstance(cx, argc, vp, JSProto_ArrayBuffer);
end;

function isDataView(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := HasInstance(cx, argc, vp, JSProto_DataView);
end;

function isDate(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := HasInstance(cx, argc, vp, JSProto_Date);
end;

function isMap(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := HasInstance(cx, argc, vp, JSProto_Map);
end;

///This is dirty hack using Spider Monkey internals
function isMapIterator(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  sInvalidCall = 'One argunent required';
begin
  Result := False;
  try
//    {$IFNDEF SM45}
//    raise ENotImplemented.Create('Check that Class_ name is correct and you can continue using this internal');
//    {$ENDIF}
    if (argc <> 1) then
      raise ESMException.Create(sInvalidCall);
    vp.rval := SimpleVariantToJSval(cx,
      vp.argv[0].isObject and (vp.argv[0].asObject.Class_.name = 'Map Iterator')
    );
    Result := True;
  except
    on E: Exception do begin
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// SyNode not use prommise yet
function isPromise(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := True;
  vp.rval := SimpleVariantToJSval(cx,false);
end;

function isRegExp(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := HasInstance(cx, argc, vp, JSProto_RegExp);
end;

function isTypedArray(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := HasInstance(cx, argc, vp, JSProto_Set);
end;

///This is dirty hack using Spider Monkey internals
function isSetIterator(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  sInvalidCall = 'One argunent required';
begin
  Result := False;
  try
//    {$IFNDEF SM45}
//    raise ENotImplemented.Create('Check that Class_ name is correct and you can continue using this internal');
//    {$ENDIF}
    if (argc <> 1) then
      raise ESMException.Create(sInvalidCall);
    vp.rval := SimpleVariantToJSval(cx,
      vp.argv[0].isObject and (vp.argv[0].asObject.Class_.name = 'Set Iterator')
    );
    Result := True;
  except
    on E: Exception do begin
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function isSet(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
begin
  Result := HasInstance(cx, argc, vp, JSProto_TypedArray);
end;

function SyNodeBindingProc_util(const aEngine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;
var
  obj: PJSRootedObject;
  cx: PJSContext;
const
  props = JSPROP_ENUMERATE or JSPROP_ENUMERATE or JSPROP_PERMANENT;
begin
  cx := aEngine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  try
    obj.ptr.DefineFunction(cx, 'isArrayBuffer', isArrayBuffer, 1, props);
    obj.ptr.DefineFunction(cx, 'isDataView', isDataView, 1, props);
    obj.ptr.DefineFunction(cx, 'isDate', isDate, 1, props);
    obj.ptr.DefineFunction(cx, 'isMap', isMap, 1, props);
    obj.ptr.DefineFunction(cx, 'isMapIterator', isMapIterator, 1, props);
    obj.ptr.DefineFunction(cx, 'isPromise', isPromise, 1, props);
    obj.ptr.DefineFunction(cx, 'isRegExp', isRegExp, 1, props);
    obj.ptr.DefineFunction(cx, 'isSet', isArrayBuffer, 1, props);
    obj.ptr.DefineFunction(cx, 'isSetIterator', isSetIterator, 1, props);
    obj.ptr.DefineFunction(cx, 'isTypedArray', isTypedArray, 1, props);

  // todo
  //  obj.ptr.DefineFunction(cx, 'getHiddenValue', isArrayBuffer, 1, props);
  //  obj.ptr.DefineFunction(cx, 'setHiddenValue', isArrayBuffer, 1, props);
  //  obj.ptr.DefineFunction(cx, 'getProxyDetails', isArrayBuffer, 1, props);
  //
  //  obj.ptr.DefineFunction(cx, 'startSigintWatchdog', isArrayBuffer, 1, props);
  //  obj.ptr.DefineFunction(cx, 'stopSigintWatchdog', isArrayBuffer, 1, props);
  //  obj.ptr.DefineFunction(cx, 'watchdogHasPendingSigint', isArrayBuffer, 1, props);

    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj);
  end;
end;

initialization
  TSMEngineManager.RegisterBinding('util', SyNodeBindingProc_util);

end.
