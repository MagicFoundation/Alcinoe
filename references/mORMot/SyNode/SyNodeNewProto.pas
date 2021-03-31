/// SyNodeNewProto - create a JS prototypes for Delphi classes based on new RTTI
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeNewProto;
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
  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Arnaud Bouchez
  - Vadim Orel
  - Pavel Mashlyakovsky
  - win2014
  - hsvandrew

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
  - added TDateTime conversion as proposed by hsvandrew

}

{$DEFINE IGNORE_InsufficientRtti }

interface

uses
  SyNodeProto,
  SynCommons,
  SysUtils,
  mORMot,
  TypInfo,
  {$IFNDEF FPC}Rtti,{$ENDIF}
  SpiderMonkey;

type
  TSMIdxPropReader = record
    ForProp: PSMRTTIPropCache;
    Inst: TSMInstanceRecord;
    procedure CreateJSObj(cx: PJSContext; aInst: PSMInstanceRecord; aForProp: PSMRTTIPropCache; var obj: PJSObject; slotIndex: Integer);
  end;
  PSMIdxPropReader = ^TSMIdxPropReader;

  TSMIdxPropReaderDynArray = array of TSMIdxPropReader;

  TSMIdxPropReaderRecord = object
    FSMIdxPropReader: TSMIdxPropReaderDynArray;
    FSMIdxPropReaders: TDynArrayHashed;
    constructor DoInit;
  end;
  PSMIdxPropReaderRecord = ^TSMIdxPropReaderRecord;

  TSMNewRTTIProtoObject = class(TSMCustomProtoObject)
  private
  protected
    FCtorForInstance: TRttiMethod;
    FCtorParams: TArray<TRttiParameter>;

    procedure DefineRTTIMethods(aRtype: TRttiType);
    procedure DefinePropOrFld(rTyp: TRttiType; rMember: TRttiMember; aParent: PJSRootedObject);
    function GetJSClass: JSClass; override;
    /// Can be used to optimize JS engine proerty access.
    // - if isReadonly setted to true property become read-only for JS engine
    // - if property valu don't changed during object lifecircle set isDeterministic=true
    //   to prevent creating of JS value every time JS engine read property value
    // If method return false propery will not be created in the JS
    function GetPropertyAddInformation(cx: PJSContext; rMember: TRttiMember; out isReadonly: boolean;
      out isDeterministic: boolean): boolean; virtual;
  public
    procedure InitObject(aParent: PJSRootedObject); override;
    function NewSMInstance(aCx: PJSContext; argc: uintN; var vp: JSArgRec): TObject; override;
  end;

  /// attribute for explicitly specify the constructor if there is more than one constructor
  // it's used by TSMCustomProtoObject when defined prototype from RTTI
  SMCtorAttribute = class(TCustomAttribute)
  end;

  /// attribute for exclude method/property of class from list of available in JS
  //  it's used by TSMCustomProtoObject when defined prototype from RTTI
  SMExcludeAttribute = class(TCustomAttribute)
  end;

  /// Label for native methods(JSNative). their call is working VERY fast
  // AForMethod - for this method is native name
  // it's used by TSMCustomProtoObject when defined prototype from RTTI
  // TODO - define automatically
  SMNativeMethodForAttribute = class(TCustomAttribute)
  public
    ForMethod: string;
    constructor Create(AForMethod: String);
  end;

function JSRTTINativeMethodCall(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
function JSRTTIMethodCall(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;

function TVal2JSVal(cx: PJSContext; const Value: TValue; aParentProto: TSMCustomProtoObject; propType: TRttiType = nil): jsval;
procedure VarRecToJSVal(cx: PJSContext; const V: TVarRec; var result: jsval);

// called when reading an indexed property
// TODO - remove?
function SMRTTIIdxPropRead(cx: PJSContext; var obj: PJSObject; var id: jsid; out vp: jsval): Boolean; cdecl;

function CreateJSInstanceObjForNewRTTI(cx: PJSContext; AInstance: TObject): jsval;

implementation

uses SyNode;

const
{$IFDEF SM52}
  jsdef_classOps: JSClassOps = (
    finalize: SMCustomObjectDestroy; // call then JS object GC}
    construct: SMCustomObjectConstruct
  );
  jsidxobj_classOps: JSClassOps = (
    getProperty: SMRTTIIdxPropRead;
    finalize: SMCustomObjectDestroy
  );
  jsdef_class: JSClass = (name: '';
    flags: uint32(JSCLASS_HAS_PRIVATE);
    cOps: @jsdef_classOps
    );
  jsidxobj_class: JSClass = (name: 'idxPropReader';
    flags: JSCLASS_HAS_PRIVATE;
    cOps: @jsidxobj_classOps
    );
{$ELSE}
  jsdef_class: JSClass = (name: '';
    flags: uint32(JSCLASS_HAS_PRIVATE);
    finalize: SMCustomObjectDestroy; // call then JS object GC}
    construct: SMCustomObjectConstruct
    );
  jsidxobj_class: JSClass = (name: 'idxPropReader';
    flags: JSCLASS_HAS_PRIVATE;
    getProperty: SMRTTIIdxPropRead;
    finalize: SMCustomObjectDestroy);
{$ENDIF}


{$REGION 'JS Call functions'}
function WideStringToAnsiString(const WS: String): AnsiString;
begin
  Result := WinAnsiConvert.UnicodeBufferToAnsi(PChar(WS), Length(WS));
end;

// TODO - need review
procedure JSVal2TVal(cx: PJSContext; t: System.TypInfo.PTypeInfo; vp: jsval; var Result: TValue);
var
  jsobj: PJSRootedObject;
  len: uint32;
  i: Integer;
  Values: array of TValue;
  typeData: PTypeData;
  dDate: TDateTime;
  L: LongWord;
  W: Word;
  B: Byte;
  i64: Int64;
  Instance: PSMInstanceRecord;
  isObj, conversionPossible: boolean;
  vt: JSType;
begin
  Result.Empty; conversionPossible := true;
  vt := cx.TypeOfValue(vp);
  isObj := vp.isObject;
  if isObj then
    jsobj := cx.NewRootedObject(vp.asObject)
  else
    jsobj := nil;

  case t^.Kind of
    System.TypInfo.tkEnumeration:
      if t = System.typeInfo(boolean) then
        Result := vp.AsBoolean
      else
        Result := Result.FromOrdinal(t, vp.AsInteger);
    System.TypInfo.tkSet: begin
      case GetTypeData(t)^.OrdType of
        System.TypInfo.otSByte, System.TypInfo.otUByte:
          begin
            B := vp.AsInteger;
            TValue.Make(@B, t, Result);
          end;
        System.TypInfo.otSWord, System.TypInfo.otUWord:
          begin
            W := vp.AsInteger;
            TValue.Make(@W, t, Result);
          end;
        System.TypInfo.otSLong, System.TypInfo.otULong:
          begin
            L := vp.AsInteger;
            TValue.Make(@L, t, Result);
          end;
      end;
    end;
    System.TypInfo.tkInteger:
      Result := vp.AsInteger;
    System.TypInfo.tkInt64: begin
      i64 := vp.AsInt64;
      TValue.Make(@i64, t, Result);
    end;

    System.TypInfo.tkFloat:
      if vp.isDouble then
        Result := vp.asDouble
      else if isObj and (jsobj.ptr.isDate(cx)) then begin
        dDate := vp.asDate[cx];
        Result := dDate;
      end;

    System.TypInfo.tkLString:
      if (vt = JSTYPE_STRING) then begin
        if t = TypeInfo(RawUTF8) then
          Result := vp.asJSString.ToUTF8(cx)
        else
          Result := vp.asJSString.ToSynUnicode(cx);
      end;
    System.TypInfo.tkWString, System.TypInfo.tkUString:
      if (vt = JSTYPE_STRING) then
        Result := vp.asJSString.ToSynUnicode(cx);

    System.TypInfo.tkClass:
      if isObj then begin
        if IsInstanceObject(cx, jsobj.ptr, Instance) then
          Result := Instance.Instance;
      end;
    System.TypInfo.tkDynArray: begin
      if isObj and (jsobj.ptr.isArray(cx)) then begin
        typeData := GetTypeData(t);
        len := 0;
        if jsobj.ptr.GetArrayLength(cx, len) then begin
          SetLength(Values, len);
          if len>0 then for i := 0 to len - 1 do begin
            if jsobj.ptr.GetElement(cx, i, vp) then begin
              if not(vp.isNull or vp.isVoid) then
                JSVal2TVal(cx, typeData.eltype2^, vp, Values[i]);
            end;
          end;
          Result := TValue.FromArray(t, Values);
        end;
      end;
    end;
    System.TypInfo.tkRecord:
      begin
        if t = typeInfo(TValue) then
        begin
//           result := TValueToJSVal(cx, Value.AsType<TValue>);
        end;
      end;
    System.TypInfo.tkMethod: // Events
      begin
        // const
        // NilMethod: TMethod = (Code: nil; Data: nil);
        // TODO TValue.Make(@NilMethod, t, Result);
        // TValue.From<TNotifyEvent>(notifyEvent);
      end;
//    System.TypInfo.tkVariant:
//      begin
//        vvar := JSValToVariant(cx, vp);
//        TValue.Make(@vvar, TypeInfo(Variant),  Result);
//      end;
  else
    conversionPossible := false;
  end;
  if jsObj <> nil then
    cx.FreeRootedObject(jsobj);
  if not conversionPossible then
    raise ESMException.Create('not implemented type conversion');
end;

function JSArgs2TVals(const params: TArray<TRttiParameter>; cx: PJSContext;  argc: uintN; var vp: JSArgRec): TArray<TValue>;
var
  i: Cardinal;
  pObj: PJSObject;
  param: TRttiParameter;
  vpr_val: jsval;

  function getDefaultValue(t: System.TypInfo.PTypeInfo): TValue;
  begin
    case t^.Kind of
      System.TypInfo.tkEnumeration:
        Result := false;
      System.TypInfo.tkFloat:
        Result := 0.0;
      System.TypInfo.tkInt64, System.TypInfo.tkInteger:
        Result := 0;
      System.TypInfo.tkLString, System.TypInfo.tkWString, System.TypInfo.tkUString:
        Result := '';
      System.TypInfo.tkDynArray:
        Result := TValue.FromArray(t, []);
      System.TypInfo.tkClass:
        Result := nil;
    end;
  end;

begin
  SetLength(Result, Length(params));
  if Length(params) > 0 then begin
    // params passed as JSON config
    // {param1: "value1", param2: 100}
    if (argc > 0) and not vp.argv[0].isNull
       and vp.argv[0].isObject
       and (vp.argv[0].asObject.Class_.Name = 'Object') then
    begin
      pObj := vp.argv[0].asObject;
      for i := 0 to High(params) do begin
        param := params[i];
        if (pObj.GetUCProperty(cx, Pointer(param.Name),
          Length(param.Name), vpr_val)) then
          JSVal2TVal(cx, param.ParamType.Handle, vpr_val, Result[i]);
      end;
    end else begin
      for i := 0 to High(params) do begin
        param := params[i];
        if (argc = 0) or (i > argc - 1) then
          Result[i] := getDefaultValue(param.ParamType.Handle)
        else
{$POINTERMATH ON}
          JSVal2TVal(cx, param.ParamType.Handle, vp.argv[i], Result[i]);
        // 0.5 sec
{$POINTERMATH OFF}
      end;
    end;
  end;
end;

function TVal2JSVal(cx: PJSContext; const Value: TValue; aParentProto: TSMCustomProtoObject; propType: TRttiType = nil): jsval;
var
  L: LongWord;
  B: Byte;
  W: Word;
  obj: TObject;
  v: TValue;
  jsarr: PJSRootedObject;
  val: jsval;
  len: Integer;
  Instance: PSMInstanceRecord;
  r: boolean;
begin
  Result.setNull;
  if Value.IsEmpty then
    exit;
  case Value.Kind of
    System.TypInfo.tkSet: begin
      case Value.DataSize of
        1: begin
          Value.ExtractRawData(@B);
          L := B;
        end;
        2: begin
          Value.ExtractRawData(@W);
          L := W;
        end;
        4: begin
          Value.ExtractRawData(@L);
        end;
      end;
      Result.AsInteger := L;
    end;
    System.TypInfo.tkEnumeration:
      if Value.typeInfo = System.typeInfo(boolean) then
        Result.AsBoolean := Value.AsBoolean
      else
        Result.AsInteger := Value.AsOrdinal;
    System.TypInfo.tkInt64:
      // TODO - check compiler version 32/64
      Result.AsInt64 := Value.AsInt64;
    System.TypInfo.tkInteger:
      // TODO - check compiler version 32/64
      Result.AsInteger := Value.AsInteger;
    System.TypInfo.tkFloat: begin
      if Assigned(propType) and (propType.ToString = 'TDateTime') then
        Result.asDate[cx] := Value.AsExtended
      else
        Result.AsDouble := Value.AsExtended;
    end;
    System.TypInfo.tkLString:
      Result.asJSString := cx.NewJSString(Value.AsType<RawUTF8>);
    System.TypInfo.tkWString, System.TypInfo.tkUString:
      Result.asJSString := cx.NewJSString(Value.AsString);
    System.TypInfo.tkClass: begin
      obj := Value.AsObject;
      New(Instance);// := TubSMInstanceObject.CreateForObj(cx, obj);
      Result := Instance.CreateForObj(cx, obj, TSMNewRTTIProtoObject, aParentProto);
    end;
    System.TypInfo.tkDynArray: begin
      len := Value.GetArrayLength;
      if len = 0 then
        Result.asBoolean := false
      else begin
        jsarr := cx.NewRootedObject(cx.NewArrayObject(0));
        try
          for L := 0 to len - 1 do begin
            v := Value.GetArrayElement(L);
            val := TVal2JSVal(cx, v, aParentProto);
            r := jsarr.ptr.SetElement(cx, L, val);
            Assert(r);
          end;
        finally
          cx.FreeRootedObject(jsarr);
        end;
        Result.asObject := jsarr.ptr;
      end;
    end;
    System.TypInfo.tkRecord: begin
//      if Value.IsType(typeInfo(TValue)) then
      if Value.TypeInfo = typeInfo(TValue) then
        Result := TVal2JSVal(cx, Value.AsType<TValue>, aParentProto);
    end;
    System.TypInfo.tkVariant: begin
        Result.asSimpleVariant[cx] := Value.AsVariant;
    end;
  else
    raise Exception.Create('Not implemented conversion from type');
  end;
end;

procedure VarRecToJSVal(cx: PJSContext; const V: TVarRec; var result: jsval);
var
  inst: PSMInstanceRecord;
  eng: TSMEngine;
begin
  Eng := TSMEngine(cx.PrivateData);

  case V.VType of
    vtString:     Result.asJSString := cx.NewJSString(RawUTF8(V.VString^));
    vtAnsiString: Result.asJSString := cx.NewJSString(RawUTF8(V.VAnsiString)); // expect UTF-8 content
    {$ifdef UNICODE}
    vtUnicodeString: Result.asJSString := cx.NewJSString(string(V.VUnicodeString));
    {$endif}
    vtWideString: Result.asJSString := cx.NewJSString(V.VWideString, length(WideString(V.VWideString)));
    vtPChar:      Result.asJSString := cx.NewJSString(string(V.VPChar));
    vtChar:       Result.asJSString := cx.NewJSString(string(V.VChar));
    vtPWideChar:  Result.asJSString := cx.NewJSString(V.VPWideChar, StrLenW(V.VPWideChar));
    vtWideChar:   Result.asJSString := cx.NewJSString(@V.VWideChar, 1);
    vtBoolean:    Result.AsBoolean := V.VBoolean;
    vtInteger:    Result.AsInteger := V.VInteger;
    vtInt64:      Result.AsInt64 := V.VInt64^;
    {$ifdef FPC}
    vtQWord:      Result.AsInt64 := V.VQWord^;
    {$endif}
    vtCurrency:   Result.AsDouble := V.VCurrency^;
    vtExtended:   Result.AsDouble := V.VExtended^;
    vtObject:     begin
      New(inst);
      Result := Inst.CreateForObj(cx, V.VObject, TSMNewRTTIProtoObject, Eng.GlobalObject);
    end;
    vtPointer:    begin
      if V.VPointer = nil then
        Result.SetNull
      else
        raise Exception.Create('Only nil pointer suppported in VarRecToJSVal');
    end else
      raise Exception.CreateFmt('Unsuported type %d in VarRecToJSVal', [V.VType]);
  end;
end;

function JSRTTIMethodCall(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  lfunc: PJSFunction;
  sMethodName: string;

  Instance: PSMInstanceRecord;
  mc: PSMMethodRec;
  args: TArray<TValue>;
  mRes: TValue;
  this: PJSObject;
  proto: PJSObject;
begin
  try
    lfunc := vp.calleObject;
    Assert(Assigned(lfunc));
    this := vp.thisObject[cx];
    if not IsInstanceObject(cx, this, Instance) then
      raise ESMException.Create('Method call: no private data');
    this.GetPrototype(cx, proto);
    mc := (Instance.proto as TSMNewRTTIProtoObject).GetMethod(lfunc, proto);
    if not Assigned(mc) then begin
      sMethodName := lfunc.FunctionId.ToSynUnicode(cx);
      raise ESMException.CreateUTF8('Method %.%() not found', [sMethodName, (Instance.proto as TSMNewRTTIProtoObject).fRttiCls.ClassName]);
    end;

    args := JSArgs2TVals(TRttiMethod(mc.method).GetParameters, cx, argc, vp); // TODO 1.5 sec
    mRes := TRttiMethod(mc.method).Invoke(Instance.Instance, args); // TODO 3 sec
    vp.rval := TVal2JSVal(cx, mRes, Instance.proto, TRttiMethod(mc.method).ReturnType );
    Result := True;
  except
    on E: Exception do begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function JSRTTINativeMethodCall(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  fCallFn: TSMFastNativeCall;
  fCallMethod: TMethod;

  lfunc: PJSFunction;
  sMethodName: string;

  Instance: PSMInstanceRecord;
  mc: PSMMethodRec;
  this, proto: PJSObject;
begin
  try
    lfunc := vp.calleObject;
    Assert(Assigned(lfunc));

    this := vp.thisObject[cx];
    if not IsInstanceObject(cx, this, Instance) then
      raise ESMException.Create('Method call: no private data');
    this.GetPrototype(cx, proto);

    mc := (Instance.proto as TSMNewRTTIProtoObject).GetMethod(lfunc, proto);
    if not Assigned(mc) then begin
      sMethodName := lfunc.FunctionId.ToSynUnicode(cx);
      raise ESMException.CreateUTF8('No method % for class', [sMethodName]);
    end;

    fCallMethod.Code := TRttiMethod(mc^.method).CodeAddress;
    fCallMethod.Data := Pointer(Instance.instance);
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

function SMRTTIIdxPropRead(cx: PJSContext; var obj: PJSObject; var id: jsid; out vp: jsval): Boolean; cdecl;
var
  prObj: PSMIdxPropReader;
  p: PSMObjectRecord;
  inval: jsval;
  idx: Integer;
  v: TValue;
  s: string;
  r: boolean;
begin
  try
    r := cx.IdToValue(id, inval);
    Assert(r);
    if not inval.isInteger then begin
      if inval.isString then begin
        s := inval.asJSString.ToString(cx);
        if (s = 'toJSON') or (s='inspect') then begin
          vp.setNull;
          Result := True;
          exit;
        end;
      end;
      raise ESMException.Create('only integer indexed property supported');
    end;

    p := obj.PrivateData;
    if not Assigned(p) or not (P.IsMagicCorrect) or not (P.DataType=otOther) then
      raise ESMException.Create(SM_NOT_A_NATIVE_OBJECT);

    prObj := p.Data;

    idx := inval.asInteger;
    v := idx;
    // TODO FastRtti
    v := TRttiIndexedProperty(prObj.ForProp.mbr).ReadMethod.Invoke(prObj.Inst.instance, [v]);
    vp := TVal2JSVal(cx, v, prObj.Inst.proto, TRttiIndexedProperty(prObj.ForProp.mbr).PropertyType );
    Result := True;
  except
    on E: Exception do begin
      Result := False;
      JSError(cx, E);
    end;
  end;
end;

function GetPropCacheForWrite(cx: PJSContext; obj: PJSObject; id: jsid; var aObj: PSMInstanceRecord): PSMRTTIPropCache;
var
  i: Integer;
  propName: AnsiString;
  found: Boolean;
begin
  if not IsInstanceObject(cx, obj, aObj) then
    raise ESMException.Create(SM_NOT_A_NATIVE_OBJECT);
  Result := nil;
  propName := PJSString(id).ToAnsi(cx);
  found := False;
  for I := 0 to Length((AObj.proto as TSMNewRTTIProtoObject).FRTTIPropsCache)-1 do begin
    Result := @(AObj.proto as TSMNewRTTIProtoObject).FRTTIPropsCache[i];
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

  if Result.isReadOnly then begin

    raise ESMException.CreateFmt('Property %s.%s is ReadOnly',
      [aObj.proto.jsObjName, PJSString(id).ToString(cx)]);
  end;
end;

// event handler
function JSRTTIPropWriteEventHandler(cx: PJSContext; argc: uintN;  var vp: JSArgRec): Boolean; cdecl;
begin
  try
    raise ESMException.Create('not impl');
    Result := True;
  except
    on E: Exception do begin
      Result := False;
      JSError(cx, E);
    end;
  end;
end;

// write a simple (non-event handler) property
function JSRTTIPropWrite(cx: PJSContext; argc: uintN;  var vp: JSArgRec): Boolean; cdecl;
var
  Instance, setObj: PSMInstanceRecord;
  pc: PSMRTTIPropCache;
  i_val: Integer;
  v: TValue;
  id: jsid;
begin
  try
    id := jsid(vp.calleObject.FunctionId);
    pc := GetPropCacheForWrite(cx, vp.thisObject[cx], id, Instance);

    // TODO FastRTTI for all types
    if Assigned(vp.argv) then begin
      if PTypeinfo(pc.typeInfo).Kind = tkInteger then begin // TODO tkEnumeration must be here (and boolean also)
        i_val := vp.argv[0].asInteger;
        mORMot.PPropInfo(PPropInfoEx(TRttiMember(pc.mbr).Handle)^.Info).SetOrdValue(Instance.Instance, i_val);
      end else if PTypeinfo(pc.typeInfo).Kind = tkClass then begin
        IsInstanceObject(cx, vp.argv[0], setObj);
        mORMot.PPropInfo(PPropInfoEx(TRttiMember(pc.mbr).Handle)^.Info).SetObjProp(Instance.Instance, setObj.Instance);
      end else begin
        JSVal2TVal(cx, pc.typeInfo, vp.argv[0], v);
        TRttiProperty(pc.mbr).SetValue(Instance.Instance, v);
      end;
    end else
      raise ESMException.CreateFmt
        ('Empty value set for property "%s" is not allowed', [pc.jsName]);

    Result := True;
  except
    on E: Exception do begin
      Result := False;
      JSError(cx, E);
    end;
  end;
end;

procedure DoFreeIdxPropReader(aInstanceRecord: PSMInstanceRecord);
var
  IdxPropReader: PSMIdxPropReaderRecord;
begin
  if Assigned(aInstanceRecord.AddData) then begin
    IdxPropReader := aInstanceRecord.AddData;
    Dispose(IdxPropReader);
    aInstanceRecord.AddData := nil;
  end;
end;

function JSRTTIPropRead(cx: PJSContext; argc: uintN;  var vp: JSArgRec): Boolean; cdecl;
var
  Instance: PSMInstanceRecord;
  pc: PSMRTTIPropCache;

  rval: jsval;
  InstanceIO: PSMInstanceRecord;
  clObj: TObject;
  proto: TSMCustomProtoObject;
  added: boolean;
  idx: integer;
  IdxPropReader: PSMIdxPropReaderRecord;
  tmp: RawUTF8;
  this: PJSRootedObject; //PJSObject;
  i: Integer;
  id: PJSString;
  prop_name: AnsiString;
  found: Boolean;
begin
  Result := true;
  try
    this := cx.NewRootedObject(vp.thisObject[cx]);
    try
      if IsProtoObject(cx, this.ptr, proto) then begin
        vp.rval := JSVAL_NULL;
        exit;
      end;

      if not IsInstanceObject(cx, this.ptr, Instance) then
        raise ESMException.Create(SM_NOT_A_NATIVE_OBJECT);

      id := vp.calleObject.FunctionId;
      prop_name := ID.ToAnsi(cx);
    // cached prioperty
      pc := nil;
      found := False;
      for i := 0 to Length((Instance.proto as TSMNewRTTIProtoObject).FRTTIPropsCache)-1 do begin
        pc := @(Instance.proto as TSMNewRTTIProtoObject).FRTTIPropsCache[i];
{$IFDEF SM52}
        if strComparePropGetterSetter(prop_name, pc.jsName, true) then begin
{$ELSE}
        if pc.jsName = prop_name then begin
{$ENDIF}
          found := True;
          break;
        end;
      end;
      if not found then
        raise ESMException.CreateFmt('% not found', [prop_name]);
      if (pc.DeterministicIndex>=0) and (not this.ptr.ReservedSlot[pc.DeterministicIndex].isVoid) then
        rval := this.ptr.ReservedSlot[pc.DeterministicIndex]
      else begin
      // TODO prototypes chain is broken, because property defined in parent
        if (TObject(pc.mbr) is TRttiProperty) then begin
          case PTypeInfo(pc.typeInfo).Kind of
            tkInteger:
              rval.asInteger :=
              mORMot.PPropInfo(PPropInfoEx(TRttiMember(pc.mbr).Handle)^.Info).GetOrdValue(Instance.Instance);
            tkEnumeration: begin
              if pc.typeInfo = System.typeInfo(boolean) then begin
                if GetOrdProp(Instance.Instance, PPropInfoEx(TRttiMember(pc.mbr).Handle)^.Info) = 1 then
                  rval.asBoolean := True
                else
                  rval.asBoolean := False
              end else begin
                rval.asInteger := GetOrdProp(Instance.Instance, PPropInfoEx(TRttiMember(pc.mbr).Handle)^.Info);
              end;
            end;
            tkWString{$ifdef HASVARUSTRING},tkUString{$endif}:
              rval := cx.NewJSString(mORMot.PPropInfo(PPropInfoEx(TRttiMember(pc.mbr).Handle)^.Info).GetUnicodeStrValue(Instance.Instance)).ToJSVal;
            tkLString: begin
              mORMot.PPropInfo(PPropInfoEx(TRttiMember(pc.mbr).Handle)^.Info).GetLongStrValue(Instance.Instance, tmp);
              rval := cx.NewJSString(tmp).ToJSVal;
            end;
            tkClass: begin
              //MPV TODO Optimize. Don't create class every time - instead check pointer is not changed
              clObj := mORMot.PPropInfo(PPropInfoEx(TRttiMember(pc.mbr).Handle)^.Info).GetObjProp(Instance.Instance);
              if Assigned(clObj) then begin
                New(InstanceIO);
                rval := InstanceIO.CreateForObj(cx,clObj, TSMNewRTTIProtoObject, Instance.proto);
              end else
                rval.setNull;
            end
          else
            // TODO other types fast call
            rval := TVal2JSVal(cx, (TRttiProperty(pc.mbr)).GetValue(Instance.Instance), Instance.proto, TRttiProperty(pc.mbr).PropertyType);
          end;
        end else if (TObject(pc.mbr) is TRttiIndexedProperty) then begin //indexed property
           if not Assigned(Instance.AddData) then begin
             new(IdxPropReader, doInit);
             Instance.AddData := IdxPropReader;
             Instance.onFree := DoFreeIdxPropReader;
           end else
             IdxPropReader := Instance.AddData;

          idx := IdxPropReader.FSMIdxPropReaders.FindHashedForAdding(pc,added);
          if added then
            IdxPropReader.FSMIdxPropReader[idx].CreateJSObj(cx, Instance, pc, this.ptr, pc.DeterministicIndex);

          rval := this.ptr.ReservedSlot[pc.DeterministicIndex];
        end else
          // TODO TRttiField FastCAll
          rval := TVal2JSVal(cx, TRttiField(pc.mbr).GetValue(Instance.Instance), Instance.proto, TRttiField(pc.mbr).FieldType);

        if (pc.DeterministicIndex>=0) then
          this.ptr.ReservedSlot[pc.DeterministicIndex] := rval;
      end;
      vp.rval := rval;
    finally
      cx.FreeRootedObject(this);
    end;
  except
    on E: Exception do begin
      Result := False;
      JSError(cx, E);
    end;
  end;
end;

procedure TSMNewRTTIProtoObject.DefinePropOrFld(rTyp: TRttiType;
  rMember: TRttiMember; aParent: PJSRootedObject);
var
  a: TCustomAttribute;
  exclude: boolean;
  ti: System.TypInfo.PTypeInfo;
  idx: Integer;
  Engine: TSMEngine;
  isReadonly, isDeterministic: Boolean;
  i: Integer;
  procedure defineSet;
  begin
    // TODO raise ESMException.Create('defineSet not implement');
  end;

begin
  Engine := TSMEngine(fCx.PrivateData);
  // TODO prototypes chain
  // current implementation add properties of all parents to the current prototype
  // in other case getter/setter can't solve from which descendants this method/propery is come
  if (rMember.Visibility >= mvPublic) then begin
    exclude := false;
    for a in rMember.GetAttributes do
      if (a is SMExcludeAttribute) then begin
        exclude := True;
        break;
      end;
    for I := 0 to Length(FJSProps) - 1 do begin
      if FJSProps[i].name = camelize(StringToAnsi7(rMember.Name)) then begin
        exclude := true;
        break;
      end;
    end;
    // indexed properties like [Index: Integer];
    if (rMember is TRttiIndexedProperty) then
      with (rMember as TRttiIndexedProperty) do
        exclude := exclude or (Length(ReadMethod.GetParameters) = 0) or
          (ReadMethod.GetParameters[0].ParamType.Handle <> typeInfo(Integer));

    exclude := exclude or not GetPropertyAddInformation(fCx, rMember, isReadonly, isDeterministic);

    if not exclude then begin
      idx := Length(FJSProps);
      if idx = 255 then
        raise ESMException.CreateUtf8('Too many propertys in class %', [fjsObjName]);

      if (rMember is TRttiField) then
        ti := (rMember as TRttiField).FieldType.Handle
      else if (rMember is TRttiProperty) then
        ti := (rMember as TRttiProperty).PropertyType.Handle
      else if (rMember is TRttiIndexedProperty) then
        ti := (rMember as TRttiIndexedProperty).PropertyType.Handle
      else
        raise ESMException.Create('Unknown property type');

      SetLength(FJSProps, idx + 1);
      SetLength(FRTTIPropsCache, idx + 1);
      FRTTIPropsCache[idx].jsName := camelize(StringToAnsi7(rMember.Name));

      // TODO move the enumerations to global.binding.enums
      // and do not clog the global object
      if (ti.Kind = tkEnumeration)and(ti<>TypeInfo(boolean)) then begin
        Engine.defineEnum(mORMot.PTypeInfo(ti), aParent);
      end else if ti.Kind = tkSet then
        defineSet;

      with FRTTIPropsCache[idx] do begin
        mbr := rMember;
        typeInfo := ti;
      end;

      FJSProps[idx].flags := JSPROP_ENUMERATE or JSPROP_PERMANENT;
      if not (rMember is TRttiIndexedProperty) then  //MPV make stored value for index-reader object
        FJSProps[idx].flags := FJSProps[idx].flags or JSPROP_SHARED;
      FJSProps[idx].Name := PCChar(FRTTIPropsCache[idx].jsName);

      // setter only for a simple properties
      // do not set the flags or JSPROP_READONLY
      // because in EcmaScript > 1.2 no erro occurence during write to read-only property
      // so, we raise a error manually
      if (rMember is TRttiProperty) and (ti.Kind = tkMethod) then
        FJSProps[idx].setter.native.op := JSRTTIPropWriteEventHandler
      else
        FJSProps[idx].setter.native.op := JSRTTIPropWrite;
      FJSProps[idx].setter.native.info := nil;

      FRTTIPropsCache[idx].isReadOnly := isReadonly or isDeterministic or not ((rMember is TRttiProperty) and (rMember as TRttiProperty).IsWritable);
      if isDeterministic then begin
        FRTTIPropsCache[idx].DeterministicIndex := fDeterministicCnt;
        inc(fDeterministicCnt);
      end else
        FRTTIPropsCache[idx].DeterministicIndex := -1;

      if (rMember is TRttiProperty) or (rMember is TRttiField) or  (rMember is TRttiIndexedProperty) then begin
        FJSProps[idx].getter.native.op := JSRTTIPropRead;
        FJSProps[idx].getter.native.info := nil;
      end else
        raise ESMException.CreateUtf8('Unsupported property type %', [rMember.ClassName]);
    end;
  end;
end;

procedure TSMNewRTTIProtoObject.DefineRTTIMethods(aRtype: TRttiType);
var
  m: TRttiMethod;
  attr: TCustomAttribute;
  mPrms: TArray<TRttiParameter>;

  mjsName: SynUnicode;
  exclude, added: boolean;
  idx: Integer;

  function camelize(const S: string): string;
  var
    Ch: Char;
  begin
    result := '';
    if S='' then
      exit;
    SetString(result, PChar(S), length(s));
    Ch := PChar(s)^;
    case Ch of
      'A' .. 'Z':
        Ch := Char(Word(Ch) or $0020);
    end;
    PChar(Result)^ := Ch;
  end;

begin
  for m in aRtype.GetMethods do begin
    exclude := false;
    mjsName := camelize(m.Name);

    for attr in m.GetAttributes do begin
      if (attr is SMCtorAttribute) and m.IsConstructor then
        FCtorForInstance := m;
      if (attr is SMExcludeAttribute) then
        exclude := True;
      if (attr is SMNativeMethodForAttribute) then begin
        mjsName := (attr as SMNativeMethodForAttribute).ForMethod;
        FMethodsDA.FindHashedAndDelete(mjsName);
        // remove overloaded method if it exists
      end;
    end;
    if (m.IsConstructor) and not Assigned(FCtorForInstance) then
      FCtorForInstance := m;

    // TODO prototypes chain
    // current implementation will add all methos to self, in other case getter/setter
    // cant determinate from wich parent this method/property
    // if (m.parent <> aRtype) then
    // exclude := true;
    try
      m.MethodKind;
    except
      on E: EInsufficientRtti do begin
{$IFNDEF IGNORE_InsufficientRtti}
        raise EInsufficientRtti.CreateFmt('InsufficientRtti for %s.%s', [aRtype.Name, mjsName]);
{$ELSE}
        exclude := True;
        SynSMLog.Add.Log(sllWarning, 'InsufficientRtti for %.%', [aRtype.Name, mjsName]);
{$ENDIF}
      end else
        raise;
    end;

    if not exclude and not m.IsConstructor and not m.IsDestructor and
      not m.IsStatic and not m.IsClassMethod and
      (m.MethodKind in [mkProcedure, mkFunction]) and (m.Visibility >= mvPublic)
      //and (not Assigned(FClassLimitator) or (m.parent <> FClassLimitatorRTTIType))
    then begin
      // TODO overloaded methods! if not (m.MethodKind = mkOperatorOverload) - methos shoud be TRttiInstanceMethodEx

      idx := FMethodsDA.FindHashedForAdding(mjsName, added);
      if added then begin
        // TODO how to check function signature equal to JSRTTINativeMethodCall signature more clever?
        mPrms := m.GetParameters;
        with FMethods[idx] do begin
          ujsName := mjsName;
          method := m;
          nargs := Length(mPrms);
          isNativeCall := ((nargs = 3) and
            (mPrms[0].ParamType.Handle = typeInfo(PJSContext)) and
            (mPrms[1].ParamType.Handle = typeInfo(uintN)) and
            (mPrms[2].ParamType.Handle = typeInfo(JSArgRec)) and
            (m.ReturnType.Handle = typeInfo(Boolean)));

          if isNativeCall then
            call := @JSRTTINativeMethodCall
          else
            call := @JSRTTIMethodCall;
          flags := [jspEnumerate];
        end;
      end;
    end;
  end;

end;

function TSMNewRTTIProtoObject.GetPropertyAddInformation(cx: PJSContext;
  rMember: TRttiMember; out isReadonly, isDeterministic: boolean): boolean;
begin
  isReadonly := false;
  isDeterministic := rMember is TRttiIndexedProperty;
  result := true;
end;

procedure TSMNewRTTIProtoObject.InitObject(aParent: PJSRootedObject);
var
  FRttiType: TRttiType;
  fld: TRttiField;
  prop: TRttiProperty;
  ip: TRttiIndexedProperty;
  smEngine: TSMEngine;
begin
  smEngine := TSMEngine(fCx.PrivateData);
  // fill methods and properties using "new RTTI"
  FRttiType := smEngine.Manager.RttiCx.GetType(fRttiCls);
  if not Assigned(FRttiType) then
    raise Exception.Create('Rtti GetType failed');

  DefineRTTIMethods(FRttiType); // -110 eng/sec  for frmMain
  FCtorParams := FCtorForInstance.GetParameters; // init constructor params

  fDeterministicCnt := 0;

  for fld in FRttiType.GetFields do
    DefinePropOrFld(FRttiType, fld, aParent); // -6 eng/sec  for frmMain

  for prop in FRttiType.GetProperties do
    DefinePropOrFld(FRttiType, prop, aParent); // -40 eng/sec  for frmMain

  for ip in FRttiType.GetIndexedProperties do
    DefinePropOrFld(FRttiType, ip, aParent);

  inherited InitObject(aParent);
end;

function TSMNewRTTIProtoObject.NewSMInstance(aCx: PJSContext; argc: uintN;
  var vp: JSArgRec): TObject;
var
  args: TArray<TValue>;
begin
  args := JSArgs2TVals(FCtorParams, aCx, argc, vp);
  Result := FCtorForInstance.Invoke(fRttiCls, args).AsObject;
end;

function TSMNewRTTIProtoObject.GetJSClass: JSClass;
begin
  Result := jsdef_class;
end;

{ SMNativeMethodForAttribute }

constructor SMNativeMethodForAttribute.Create(AForMethod: String);
begin
  ForMethod := AForMethod;
end;

{ TSMIdxPropReader }

procedure TSMIdxPropReader.CreateJSObj(cx: PJSContext; aInst: PSMInstanceRecord; aForProp: PSMRTTIPropCache; var obj: PJSObject; slotIndex: Integer);
var
  ObjRec: PSMObjectRecord;
  PropReader: PSMIdxPropReader;
  jsObj: PJSRootedObject;
begin
  Inst := aInst^;
  ForProp := aForProp;
  New(ObjRec);
  New(PropReader);
  PropReader.Inst := aInst^;
  PropReader.ForProp := aForProp;
  ObjRec.init(otOther, PropReader);
  jsObj := cx.NewRootedObject(cx.NewObject(@jsidxobj_class));
  try
    jsObj.ptr.PrivateData := ObjRec;
    obj.ReservedSlot[slotIndex] := jsObj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(jsObj);
  end;

end;

{ TSMIdxPropReadersRecord }

function CreateJSInstanceObjForNewRTTI(cx: PJSContext; AInstance: TObject): jsval;
var
  Inst: PSMInstanceRecord;
  eng: TSMEngine;
begin
  new(Inst);
  eng := cx.PrivateData;
  Result := Inst.CreateForObj(cx, AInstance, TSMNewRTTIProtoObject, eng.GlobalObject);
end;


{ TSMIdxPropReaderRecord }

constructor TSMIdxPropReaderRecord.DoInit;
begin
  FSMIdxPropReaders.InitSpecific(TypeInfo(TSMIdxPropReaderDynArray), FSMIdxPropReader, djPointer);
end;

end.
