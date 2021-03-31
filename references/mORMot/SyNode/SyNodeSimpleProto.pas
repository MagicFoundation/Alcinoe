/// SyNodeSimpleProto - create a JS prototypes with Delphi method/props realisation
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeSimpleProto;
{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

    SyNode for mORMot Copyright (C) 2021 Pavel Mashlyakovsky & Vadim Orel
      pavel.mash at gmail.com

    Some ideas taken from
       http://code.google.com/p/delphi-javascript
       http://delphi.mozdev.org/javascript_bridge/

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Initial Developer of the Original Code is
  Pavel Mashlyakovsky.
  Portions created by the Initial Developer are Copyright (C) 2014
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Arnaud Bouchez
  - Vadim Orel
  - Pavel Mashlyakovsky
  - win2014

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****


  Version 1.18
  - initial release. Use SpiderMonkey 45

}

{$I Synopse.inc}

interface

uses
  SpiderMonkey,
  SyNodeProto,
  mORMot { PClassProp };

type
  /// A prototype class for wrapping a Delphi class based on a "old" RTTI
  //  - create a properties in JavaScript based on the published properties of original class
  //  - all published methods of a original calss MUST have a TSMFastNativeCall signature
  TSMSimpleRTTIProtoObject = class(TSMCustomProtoObject)
  protected
    procedure InitObject(aParent: PJSRootedObject); override;
    /// Can be used to optimize JS engine proerty access.
    // - if isReadonly setted to true property become read-only for JS engine
    // - if property valu don't changed during object lifecircle set isDeterministic=true
    //   to prevent creating of JS value every time JS engine read property value
    // If method return false propery will not be created in the JS
    function GetPropertyAddInformation(cx: PJSContext; PI:PPropInfo; out isReadonly: boolean;
      out isDeterministic: boolean; aParent: PJSRootedObject): boolean; virtual;
    function GetJSvalFromProp(cx: PJSContext; PI:PPropInfo; instance: PSMInstanceRecord): jsval; virtual;
  public

  end;

function CreateJSInstanceObjForSimpleRTTI(cx: PJSContext; AInstance: TObject; aParent: PJSRootedObject=nil): jsval;

implementation

uses
  {$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  SyNode,
  SynCommons;

function JSRTTINativeMethodCall(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  fCallFn: TSMFastNativeCall;
  fCallMethod: TMethod;

  lfunc: PJSFunction;
  jsobj: PJSObject;

  Inst: PSMInstanceRecord;
  mc: PSMMethodRec;
  proto: PJSObject;
begin
  try
//    JS_ConvertValue(cx,JS_CALLEE(cx, vp),JSTYPE_FUNCTION, lfuncVal);
    lfunc := vp.calleObject;
    Assert(Assigned(lfunc));

    jsobj := vp.thisObject[cx];
    if not IsInstanceObject(cx, jsobj, Inst) then
      raise ESMException.Create(SM_NOT_A_NATIVE_OBJECT);
    jsobj.GetPrototype(cx, proto);
    mc := TSMSimpleRTTIProtoObject(Inst^.proto).getMethod(lfunc, proto);

    if mc = nil then
      raise ESMException.CreateUTF8('The class has no method "%"', [lfunc.GetFunctionId().ToSynUnicode(cx)]);

    fCallMethod.Code := mc^.method;
    fCallMethod.Data := Pointer(Inst.instance);
    fCallFn := TSMFastNativeCall(fCallMethod);
    Result := fCallFn(cx, argc, vp);
  except
    on E: Exception do begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function GetPropCacheForWrite(cx: PJSContext; obj: PJSObject; id: jsid; var aObj: PSMInstanceRecord): PSMRTTIPropCache;
var
  i: Integer;
  propName: AnsiString;
  found: boolean;
begin
  Result := nil;
  if not IsInstanceObject(cx, obj, aObj) then
    raise ESMException.Create(SM_NOT_A_NATIVE_OBJECT);
  propName := PJSString(id).ToAnsi(cx);
  found := False;
  for I := 0 to Length((AObj.proto as TSMSimpleRTTIProtoObject).FRTTIPropsCache)-1 do begin
    Result := @(AObj.proto as TSMSimpleRTTIProtoObject).FRTTIPropsCache[i];
{$IFDEF SM52}
    if strComparePropGetterSetter(propName, Result.jsName, false) then begin
{$ELSE}
    if Result.jsName = propName then begin
{$ENDIF}
      found := True;
      Break;
    end;
  end;
  if not found then
    raise ESMException.CreateFmt('% not found', [propName]);

  if Result.isReadOnly then
    raise ESMException.CreateUtf8('Property %.% is ReadOnly', [aObj.proto.jsObjName, Result.jsName]);
end;

function JSRTTIPropWrite(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  Instance: PSMInstanceRecord;
  PI: PPropInfo;
  id: jsid;
  val: jsval;
begin
  id := jsid(vp.calleObject.FunctionId);
  PI := GetPropCacheForWrite(cx, vp.thisObject[cx], id, Instance).mbr;
  val := vp.argv[0];
  case PI.PropType^{$IFNDEF FPC}^{$ENDIF}.Kind of
    tkInteger, tkEnumeration, tkSet{$ifdef FPC},tkBool{$endif}:
      PI.SetOrdProp(Instance^.instance,val.asInteger);
    tkInt64:
      PI.SetInt64Prop(Instance^.instance, val.asInt64);
    tkFloat:
      PI.SetFloatProp(Instance^.instance, val.asDouble);
    tkLString,{$IFDEF FPC}tkLStringOld{$ENDIF},tkWString{$ifdef HASVARUSTRING},tkUString{$endif}:
      PI.SetLongStrValue(Instance^.instance, val.asJsString.ToUTF8(cx));
  else
    raise ESMException.Create('NotImplemented');
  end;
  Result := True;
end;

function JSRTTIPropRead(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  Instance: PSMInstanceRecord;
  proto: TSMCustomProtoObject;

  propCache: PSMRTTIPropCache;
  PI: PPropInfo;

  rval: jsval;

  this: PJSObject;
  storedVal: jsval;
  i: Integer;
  id: PJSString;
  prop_name: AnsiString;
  found: Boolean;
begin
  try
    this := vp.thisObject[cx];

    if IsProtoObject(cx, this, proto) then begin
      vp.rval := JSVAL_NULL;
      Result := True;
      exit;
    end;

    if not IsInstanceObject(cx, this, Instance) then
      raise ESMException.Create(SM_NOT_A_NATIVE_OBJECT);

    id := vp.calleObject.FunctionId;
    prop_name := ID.ToAnsi(cx);

    propCache := nil;
    found := false;
    for i := 0 to Length((Instance.proto as TSMSimpleRTTIProtoObject).FRTTIPropsCache)-1 do begin
      propCache := @(Instance.proto as TSMSimpleRTTIProtoObject).FRTTIPropsCache[i];
{$IFDEF SM52}
      if strComparePropGetterSetter(prop_name, propCache.jsName, true) then begin
{$ELSE}
        if propCache.jsName = prop_name then begin
{$ENDIF}
        found := True;
        Break;
      end;
    end;
    if not found then
      raise ESMException.CreateFmt('% not found', [prop_name]);

    if (propCache.DeterministicIndex>=0) then
      storedVal := this.ReservedSlot[propCache.DeterministicIndex]
    else
      storedVal.setVoid;

    if (not storedVal.isVoid) then
      rval := storedVal
    else begin
      PI := propCache.mbr;
      rval := (Instance.proto as TSMSimpleRTTIProtoObject).GetJSvalFromProp(cx, PI, Instance);
      if (propCache.DeterministicIndex>=0) then begin
        // all jsvals in reserved slots are rooted automatically
        this.ReservedSlot[propCache.DeterministicIndex] := rval;
      end;
    end;

    vp.rval := rval;
    Result := True;
  except
    on E: Exception do begin
      Result := False;
      JSError(cx, E);
    end;
  end;
end;

{ TSMSimpleRTTIProtoObject }

function TSMSimpleRTTIProtoObject.GetJSvalFromProp(cx: PJSContext;
  PI: PPropInfo; instance: PSMInstanceRecord): jsval;
var
  FInst: PSMInstanceRecord;
  tmp: RawUTF8;
  obj: TObject;
  arr: TDynArray;
begin
  case PI.PropType^.Kind of
    tkInteger, tkEnumeration, tkSet{$ifdef FPC},tkBool{$endif}:
      Result.asInteger := PI.GetOrdValue(Instance^.instance);
    tkInt64:
      Result.asInt64 := PI.GetInt64Value(Instance^.instance);
    tkFloat:
      Result.asDouble := PI.GetDoubleValue(Instance^.instance);
    tkLString,{$ifdef FPC}tkLStringOld,{$endif}tkWString{$ifdef HASVARUSTRING},tkUString{$endif}: begin
      PI.GetLongStrValue(Instance^.instance, tmp);
      Result.asJSString := cx.NewJSString(tmp);
    end;
    tkClass: begin
       new(FInst);
       obj := PI.GetObjProp(Instance^.instance);
       if obj <> nil then
         Result := FInst.CreateForObj(cx, obj, TSMSimpleRTTIProtoObject, Instance.Proto)
       else
         Result := JSVAL_NULL;
    end;
    tkDynArray: begin
       // MPV. WARNING. Every access to dyn array property will create a JS Array, so
       // I recommend avoiding use of the dynamic arrays, or use a temp variable
       arr := PI.GetDynArray(Instance^.instance);
       Result.asJson[cx] := arr.SaveToJSON(true);
    end;
  else
    raise ESMException.Create('NotImplemented');
  end;
end;

procedure TSMSimpleRTTIProtoObject.InitObject(aParent: PJSRootedObject);
var
  PI: PPropInfo;
  i: integer;
  idx: Integer;

  CT: TClass;
  n: integer;
  added: boolean;
  isReadonly: boolean;
  isDeterministic: boolean;
  exclude: boolean;
  methods: TPublishedMethodInfoDynArray;
begin
  for i := 0 to GetPublishedMethods(nil, methods, fRttiCls) - 1 do begin
    idx := FMethodsDA.FindHashedForAdding(methods[i].Name, added);
    if added then with FMethods[idx] do begin
      ujsName := UTF8ToSynUnicode(methods[i].Name);
      method := methods[i].Method.Code;
      nargs := 0;
      isNativeCall := true;
      call := @JSRTTINativeMethodCall;
      flags := [jspEnumerate];
    end;
  end;

  fDeterministicCnt := 0;
  CT := fRttiCls;
  repeat
    for i := 1 to InternalClassPropInfo(CT,PI) do begin
      idx := Length(FJSProps);

      exclude := PI^.PropType^.Kind = tkMethod;
      if not exclude then
        for n := 0 to idx - 1 do begin
          if StrLIComp(PAnsiChar(@PI.Name[1]), PAnsiChar(FRTTIPropsCache[n].jsName), length(PI.Name)) = 0 then begin
            exclude := true;
            break;
          end;
        end;

      if exclude or not GetPropertyAddInformation(fCx, PI, isReadonly, isDeterministic, aParent) then begin
        PI := PI^.Next;
        Continue;
      end;

      case PI^.PropType^{$IFNDEF FPC}^{$ENDIF}.Kind of
        tkChar, {$IFDEF FPC}tkLString{$ELSE}tkString{$ENDIF}, tkWChar, tkWString, tkVariant:
        begin
          raise ESMException.CreateUtf8('Unsupported class property %.%', [FjsObjName, PI^.Name]);
        end;
//		lazy create class type property prototypes on first read (in TSMSimpleRTTIProtoObject.GetJSvalFromProp)
//        tkClass:
//          defineClass(Cx, PI^.PropType^{$IFNDEF FPC}^{$ENDIF}.ClassType^.ClassType, TSMSimpleRTTIProtoObject, aParent);
        tkEnumeration:
          defineEnum(fCx, PI^.PropType{$IFNDEF FPC_OLDRTTI}^{$ENDIF}, aParent);
      end;

      SetLength(FJSProps, idx + 1);
      SetLength(FRTTIPropsCache, idx + 1);
      FRTTIPropsCache[idx].jsName := camelize(PI.Name);
      FRTTIPropsCache[idx].mbr := PI;
      FRTTIPropsCache[idx].typeInfo := PI^.PropType{$IFNDEF FPC}^{$ENDIF};
      FRTTIPropsCache[idx].isReadOnly := isReadonly or isDeterministic;
      if isDeterministic then begin
        FRTTIPropsCache[idx].DeterministicIndex := fDeterministicCnt;
        Inc(fDeterministicCnt);
      end else
        FRTTIPropsCache[idx].DeterministicIndex := -1;

      FJSProps[idx].flags := JSPROP_ENUMERATE or JSPROP_PERMANENT or JSPROP_SHARED;
      FJSProps[idx].Name := PCChar(RTTIPropsCache[idx].jsName);
  //    FJSProps[idx].tinyid := idx;
      FJSProps[idx].setter.native.info := nil;
      FJSProps[idx].setter.native.op := JSRTTIPropWrite;
      FJSProps[idx].getter.native.info := nil;
      FJSProps[idx].getter.native.op := JSRTTIPropRead;
      PI := PI^.Next;
    end;
    CT := CT.ClassParent;
  until CT=nil;
  inherited; //MPV !! do not use  FMethodsDA.Add()
end;

function TSMSimpleRTTIProtoObject.GetPropertyAddInformation(cx: PJSContext;
  PI: PPropInfo; out isReadonly: boolean; out isDeterministic: boolean; aParent: PJSRootedObject): boolean;
begin
  isReadonly := false;
  isDeterministic := false;
  result := true;
end;

function CreateJSInstanceObjForSimpleRTTI(cx: PJSContext; AInstance: TObject; aParent: PJSRootedObject=nil): jsval;
var
  Inst: PSMInstanceRecord;
  eng: TSMEngine;
begin
  new(Inst);
  if (aParent = nil) then begin
    eng := cx.PrivateData;
    Result := Inst.CreateForObj(cx, AInstance, TSMSimpleRTTIProtoObject, eng.GlobalObject);
  end else
    Result := Inst.CreateForObj(cx, AInstance, TSMSimpleRTTIProtoObject, aParent);
end;

end.
