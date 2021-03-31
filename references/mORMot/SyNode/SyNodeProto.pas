/// SyNodeProto - create a JS prototypes for Delphi classes based on Delphi7 RTTI
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeProto;
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

interface

{$I Synopse.inc} // define HASINLINE
{$I SyNode.inc}

uses
  {$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  mORMot,
  SynCommons,
  SynLog,
  SpiderMonkey;

type

  TSMRTTIPropCache = record
    mbr: pointer;//TRttiMember;
    jsName: AnsiString;
    isReadOnly: boolean;
    DeterministicIndex: integer; // if not deterministic then -1
    typeInfo: Pointer;//PTypeInfo;
  end;
  PSMRTTIPropCache = ^TSMRTTIPropCache;
  TPropArray = array of TSMRTTIPropCache;

  {$ifdef UNICODE}
  TSMMethodRec = record
  {$else}
  TSMMethodRec = object
  {$endif}
    ujsName: SynUnicode;
    method: pointer;
    isNativeCall: boolean;
    call: JSNative;
    nargs: uintN;
    flags: TJSPropertyAttrs;
  end;
  PSMMethodRec = ^TSMMethodRec;
  TSMMethodDynArray = array of TSMMethodRec;

  TSMObjectType = (otProto, otInstance, otOther);

  {$ifdef UNICODE}
  TSMObjectRecord = record
  {$else}
  TSMObjectRecord = object
  {$endif}
    Magic: Word;
    DataType: TSMObjectType;
    Data: Pointer;
    procedure init(aDataType: TSMObjectType; aData: Pointer);
    function IsMagicCorrect: boolean;
  end;
  PSMObjectRecord = ^TSMObjectRecord;

  TSMCustomProtoObject = class;
  TSMCustomProtoObjectClass = class of TSMCustomProtoObject;

  PSMInstanceRecord = ^TSMInstanceRecord;
  TFreeInstanceRecord = procedure (aInstanceRecord: PSMInstanceRecord);
 {$ifdef UNICODE}
  TSMInstanceRecord = record
  {$else}
  TSMInstanceRecord = object
  {$endif}
  private
    function InternalCreate(cx: PJSContext; AInstance: TObject; AProto: TSMCustomProtoObject): jsval;
  public
    proto: TSMCustomProtoObject;
    instance: TObject;
    OwnInstance: Boolean;
    AddData: pointer;
    onFree: TFreeInstanceRecord;
    procedure freeNative;
    function CreateNew(acx: PJSContext; AProto: TSMCustomProtoObject; argc: uintN; var vp: JSArgRec): jsval;
    function CreateForObj(acx: PJSContext; AInstance: TObject; AProto: TSMCustomProtoObjectClass; aParent: PJSRootedObject): jsval; overload;
    function CreateForObj(acx: PJSContext; AInstance: TObject; AProto: TSMCustomProtoObjectClass; aParentProto: TSMCustomProtoObject): jsval; overload;
  end;

  TSMCustomProtoObject = class
  private
    fFirstDeterministicSlotIndex: uint32;
    FJSClass: JSClass;
    FJSClassProto: JSClass;
    fSlotIndex: integer;
    function getRTTIPropsCache(index: integer): TSMRTTIPropCache;
  protected
    fCx: PJSContext;
    FjsObjName: AnsiString;
    fRttiCls: TClass;
    FJSProps: TJSPropertySpecDynArray;
    fDeterministicCnt: uint32;
    FRTTIPropsCache: TPropArray;

    FMethods: TSMMethodDynArray;
    FMethodsDA: TDynArrayHashed;
    function GetJSClass: JSClass; virtual;
    procedure InitObject(aParent: PJSRootedObject); virtual;
    /// Add method to internal FMethods array for future define it into JS prototype
    // to be called only inside InitObject method!
    procedure definePrototypeMethod(const ajsName: SynUnicode; const aCall: JSNative; aNargs: uintN; aFlags: TJSPropertyAttrs);
    property SlotIndex: integer read fSlotIndex;
  public
    property RTTIPropsCache[index: integer]: TSMRTTIPropCache read getRTTIPropsCache;
    property jsObjName: AnsiString read FjsObjName;
    property DeterministicCnt: Cardinal read fDeterministicCnt;
    property FirstDeterministicSlotIndex: Cardinal read fFirstDeterministicSlotIndex;
    function getMethod(const aJSFunction: PJSFunction; var obj: PJSObject): PSMMethodRec; //overload;
    constructor Create(Cx: PJSContext; aRttiCls: TClass; aParent: PJSRootedObject; slotIndex: integer); virtual;
    function NewSMInstance(aCx: PJSContext; argc: uintN; var vp: JSArgRec): TObject; virtual;
  end;

  TSMFastNativeCall = function(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean of object;

function IsInstanceObject(cx: PJSContext; jsobj: PJSObject; var asmInstance: PSMInstanceRecord): boolean; overload;
function IsInstanceObject(cx: PJSContext; jval: jsval; var asmInstance: PSMInstanceRecord): boolean; overload;

function IsProtoObject(cx: PJSContext; jsobj: PJSObject; var asmProto: TSMCustomProtoObject): boolean; overload;
function IsProtoObject(cx: PJSContext; jval: jsval; var asmProto: TSMCustomProtoObject): boolean; overload;

procedure defineEnum(cx: PJSContext; ti: PTypeInfo; aParent: PJSRootedObject);
function defineClass(cx: PJSContext; AForClass: TClass; AProto: TSMCustomProtoObjectClass; aParent: PJSRootedObject): TSMCustomProtoObject; overload;
function defineClass(cx: PJSContext; AForClass: TClass; AProto: TSMCustomProtoObjectClass; aParentProto: TSMCustomProtoObject): TSMCustomProtoObject; overload;

function camelize(const S: AnsiString): AnsiString;

const
  SM_NOT_A_NATIVE_OBJECT = 'Not a native object';

function strComparePropGetterSetter(prop_name, jsName: AnsiString; isGetter: boolean): Boolean; {$ifdef HASINLINE}inline;{$endif}
// for can make strComparePropGetterSetter inlined
const prefix: array[boolean] of TShort4 = ('set ','get ');

// called when the interpreter wants to create an object through a new TMyObject ()
function SMCustomObjectConstruct(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;  cdecl;
// called when the interpreter destroys the object
{$IFDEF SM52}
procedure SMCustomObjectDestroy(var fop: JSFreeOp; obj: PJSObject); cdecl;
{$ELSE}
procedure SMCustomObjectDestroy(var rt: PJSRuntime; obj: PJSObject); cdecl;
{$ENDIF}

implementation

function strComparePropGetterSetter(prop_name, jsName: AnsiString; isGetter: boolean): Boolean;
var jsNameLen: integer;
    ch1, ch2: PAnsiChar;
begin
  jsNameLen := Length(jsName);
  Result := (jsNameLen > 0) and (Length(prop_name) = jsNameLen + 4) and
            ((PInteger(@prop_name[1]))^ = (PInteger(@prefix[isGetter][1]))^);
  if Result then begin
    ch1 := @jsName[1];
    ch2 := @prop_name[5];
    while jsNameLen >= 4 do begin
      if PInteger(ch1)^ <> PInteger(ch2)^ then begin
        result := False;
        exit;
      end;
      inc(ch1, 4);
      inc(ch2, 4);
      Dec(jsNameLen, 4);
    end;
    if jsNameLen >= 2 then begin
      if PWord(ch1)^ <> PWord(ch2)^ then begin
        result := False;
        exit;
      end;
      inc(ch1, 2);
      inc(ch2, 2);
      Dec(jsNameLen, 2);
    end;
    if jsNameLen = 1 then begin
      if ch1^ <> ch2^ then begin
        result := False;
        exit;
      end;
    end;
  end;
end;


function camelize(const S: AnsiString): AnsiString;
var
  Ch: AnsiChar;
begin
  result := '';
  if S='' then
    exit;
  SetString(result, PAnsiChar(S), length(s));
  Ch := PAnsiChar(s)^;
  case Ch of
    'A' .. 'Z':
      Ch := AnsiChar(byte(Ch) or $20);
  end;
  PAnsiChar(Result)^ := Ch;
end;

const
// Magic constant for TSMObjectRecord
  SMObjectRecordMagic: Word = 43857;
{$IFDEF SM52}
  jsdef_classOpts: JSClassOps = (
    finalize: SMCustomObjectDestroy; // call then JS object GC}
    construct: SMCustomObjectConstruct
  );
  jsdef_class: JSClass = (name: '';
    flags: uint32(JSCLASS_HAS_PRIVATE);
    cOps: @jsdef_classOpts
    );
{$ELSE}
  jsdef_class: JSClass = (name: '';
    flags: uint32(JSCLASS_HAS_PRIVATE);
    finalize: SMCustomObjectDestroy; // call then JS object GC}
    construct: SMCustomObjectConstruct
    );
{$ENDIF}
// create object var obj = new TMyObject();
function SMCustomObjectConstruct(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  jsobj: PJSObject;
  Proto: TSMCustomProtoObject;
  Inst: PSMInstanceRecord;
begin
  Result := false;
  try
    if not vp.IsConstructing then
      raise ESMException.Create('Construct: not JS_IS_CONSTRUCTING');
    jsobj := vp.calleObject;
    if not IsProtoObject(cx, jsobj, Proto) then
      raise ESMException.Create('Construct: no private data');
    new(Inst);
    vp.rval := Inst.CreateNew(cx, Proto, argc, vp);
    Result := true;
  except
    on E: Exception do
      JSError(cx, E);
  end;
end;

{$IFDEF SM52}
procedure SMCustomObjectDestroy(var fop: JSFreeOp;  obj: PJSObject); cdecl;
{$ELSE}
procedure SMCustomObjectDestroy(var rt: PJSRuntime; obj: PJSObject); cdecl;
{$ENDIF}
var
  ObjRec: PSMObjectRecord;
  Inst: PSMInstanceRecord;
  proto: TSMCustomProtoObject;
begin
  ObjRec := obj.PrivateData;
  if Assigned(ObjRec) and (ObjRec.IsMagicCorrect) then
  begin
    if (ObjRec.DataType=otInstance) and Assigned(ObjRec.Data) then begin
      Inst := ObjRec.Data;
      Inst.freeNative;
      Dispose(Inst);
    end else if (ObjRec.DataType=otProto) and Assigned(ObjRec.Data) then begin
      proto := ObjRec.Data;
      FreeAndNil(proto);
    end else begin
      Dispose(ObjRec.Data); // ObjRec.Data is a PSMIdxPropReader from SyNoideNewProto
    end;
    Dispose(ObjRec);
    obj.PrivateData := nil;
  end;
end;

function IsSMObject(cx: PJSContext; jsobj: PJSObject; var aObj: PSMObjectRecord): boolean; overload;
var
  P: PSMObjectRecord;
  CLS: PJSClass;
begin
  Result := false;
  CLS := jsobj.Class_;
  if CLS.flags and JSCLASS_HAS_PRIVATE = 0 then
    Exit;

  p := jsobj.PrivateData;
  if (p <> nil) then
    if (P.IsMagicCorrect) then begin
      aObj := p;
      Result := true;
    end else
      raise ESMException.Create('Incorrect IsMagicCorrect');
end;

function IsSMObject(cx: PJSContext; jval: jsval; var aObj: PSMObjectRecord): boolean; overload;
var
  obj: PJSObject;
begin
  Result := jval.isObject;
  if not Result then exit;
  obj := jval.asObject;
  Result := IsSMObject(cx, obj, aObj);
end;

function IsInstanceObject(cx: PJSContext; jval: jsval; var asmInstance: PSMInstanceRecord): boolean; overload;
var
  obj: PJSObject;
begin
  Result := jval.isObject;
  if not Result then exit;
  obj := jval.asObject;
  Result := IsInstanceObject(cx, obj, asmInstance);
end;

function IsInstanceObject(cx: PJSContext; jsobj: PJSObject; var asmInstance: PSMInstanceRecord): boolean;
var
  aObj: PSMObjectRecord;
begin
  Result := IsSMObject(cx, jsobj, aObj);
  if Result then
    Result := (aObj.DataType = otInstance);
  if not Result then
    asmInstance := nil
  else
    asmInstance := aObj.Data;
end;

function IsProtoObject(cx: PJSContext; jsobj: PJSObject; var asmProto: TSMCustomProtoObject): boolean;
var
  aObj: PSMObjectRecord;
begin
  Result := IsSMObject(cx, jsobj, aObj);
  if Result then
    Result := (aObj.DataType = otProto);
  if not Result then
    asmProto := nil
  else
    asmProto := aObj.Data;
end;

function IsProtoObject(cx: PJSContext; jval: jsval; var asmProto: TSMCustomProtoObject): boolean;
var
  obj: PJSObject;
begin
  Result := jval.isObject;
  if not Result then exit;
  obj := jval.asObject;
  Result := Assigned(obj) and IsProtoObject(cx, obj, asmProto);
end;

procedure defineEnum(cx: PJSContext; ti: PTypeInfo; aParent: PJSRootedObject);
var
  i: integer;
  s: SynUnicode;
  found: Boolean;
  val: jsval;
  obj_: PJSRootedObject;
begin
  if (ti^.Name = 'Boolean') then
    Exit;
  s := UTF8ToSynUnicode(ShortStringToUTF8(ti^.Name));
  if (aParent.ptr.HasUCProperty(cx, Pointer(s), Length(s), found)) and found then
    exit; //enum already defined

  obj_ := cx.NewRootedObject(cx.NewObject(nil));
  try
    aParent.ptr.DefineUCProperty(cx, Pointer(s), Length(s), obj_.ptr.ToJSValue, JSPROP_ENUMERATE or JSPROP_PERMANENT, nil, nil);
    with ti^.EnumBaseType^ do begin
      for i := MinValue to MaxValue do begin
        s := UTF8ToSynUnicode(GetEnumNameTrimed(i));
        val.asInteger := i;
        obj_.ptr.DefineUCProperty(cx, Pointer(s), Length(s), val, JSPROP_ENUMERATE or JSPROP_PERMANENT, nil, nil);
      end;
    end;
  finally
    cx.FreeRootedObject(obj_);
  end;

  //TODO freez (seal) created JS object to not allow it modification

end;

function defineClass(cx: PJSContext; AForClass: TClass; AProto: TSMCustomProtoObjectClass; aParent: PJSRootedObject): TSMCustomProtoObject;
var
  global: PJSObject;
  i: integer;
  val: jsval;
begin
  global := cx.CurrentGlobalOrNull;
  for I := JSCLASS_GLOBAL_SLOT_COUNT to 255 do begin
    val := global.ReservedSlot[i];
    if val.isVoid then begin
      //create new
      result := AProto.Create(Cx, AForClass, aParent, i);
      exit;
    end else if IsProtoObject(cx, val, Result) then begin
      if Result.fRttiCls = AForClass then
        exit; // The class prototype has already created
    end else if val.isString then begin
      if val.asJSString.ToString(cx) = AForClass.ClassName then
        exit; // The class prototype is being created right now
    end else
      raise ESMException.Create('Slot value is not ProtoObject');
  end;
  raise Exception.Create('defineClass Error: many proto' + AForClass.ClassName);
end;

function defineClass(cx: PJSContext; AForClass: TClass; AProto: TSMCustomProtoObjectClass; aParentProto: TSMCustomProtoObject): TSMCustomProtoObject; overload;
var
  global: PJSObject;
  i: integer;
  val: jsval;
  aParent: PJSRootedObject;
begin
  global := cx.CurrentGlobalOrNull;
  for I := JSCLASS_GLOBAL_SLOT_COUNT to 255 do begin
    val := global.ReservedSlot[i];
    if val.isVoid then begin
      //create new
      aParent := cx.NewRootedObject(global.ReservedSlot[aParentProto.fSlotIndex].asObject.ReservedSlot[aParentProto.FirstDeterministicSlotIndex].asObject);
      try
        result := AProto.Create(Cx, AForClass, aParent, i);
      finally
        cx.FreeRootedObject(aParent);
      end;
      exit;
    end else begin
      if IsProtoObject(cx, val, Result) then begin
        if Result.fRttiCls = AForClass then
          exit;
      end else
        raise ESMException.Create('Slot value is not ProtoObject');
    end;
  end;
  raise Exception.Create('defineClass Error: many proto' + AForClass.ClassName);
end;

{ TSMObjectRecord }

procedure TSMObjectRecord.init(aDataType: TSMObjectType; aData: Pointer);
begin
  Self.Magic := SMObjectRecordMagic;
  Self.DataType := aDataType;
  Self.Data := aData;
end;

function TSMObjectRecord.IsMagicCorrect: boolean;
begin
  Result := Magic = SMObjectRecordMagic;
end;

{ TSMInstanceRecord }

function TSMInstanceRecord.CreateForObj(acx: PJSContext; AInstance: TObject; AProto: TSMCustomProtoObjectClass; aParent: PJSRootedObject): jsval;
begin
  Result := InternalCreate(acx, AInstance, defineClass(acx, AInstance.ClassType, AProto, aParent));
  OwnInstance := false;
end;

function TSMInstanceRecord.CreateForObj(acx: PJSContext; AInstance: TObject;
  AProto: TSMCustomProtoObjectClass;
  aParentProto: TSMCustomProtoObject): jsval;
begin
  Result := InternalCreate(acx, AInstance, defineClass(acx, AInstance.ClassType, AProto, aParentProto));
  OwnInstance := false;
end;

function TSMInstanceRecord.CreateNew(acx: PJSContext; AProto: TSMCustomProtoObject; argc: uintN;
  var vp: JSArgRec): jsval;
begin
  result := InternalCreate(aCx, AProto.NewSMInstance(acx, argc, vp), AProto );
  OwnInstance := True;
end;

procedure TSMInstanceRecord.freeNative;
begin
  if Assigned(onFree) then
    onFree(@Self);
  if OwnInstance then begin
    if Assigned(instance) then
      FreeAndNil(instance);
  end;
  Instance := nil; // in case the FInstance is IUnknown
end;

function TSMInstanceRecord.InternalCreate(cx: PJSContext; AInstance: TObject; AProto: TSMCustomProtoObject): jsval;
var
  ObjRec: PSMObjectRecord;
  jsobj: PJSRootedObject;
  global: PJSRootedObject;
  protoObj: PJSRootedObject;
begin
  proto := AProto;
  onFree := nil;
  AddData := nil;

  cx.BeginRequest;
  try
    global := cx.NewRootedObject(cx.CurrentGlobalOrNull);
    protoObj := cx.NewRootedObject(global.ptr.ReservedSlot[proto.fSlotIndex].asObject);
    jsobj := cx.NewRootedObject(cx.NewObjectWithGivenProto(@proto.FJSClass, protoObj.ptr));
    try
      // premature optimization is the root of evil
      // as shown by valgrind profiler better to not redefine props in object
      // but let's JS engine to use it from prototype
      //if Length(AProto.FJSProps)>0 then
      //  jsobj.ptr.DefineProperties(cx,@AProto.FJSProps[0]);

      Instance := AInstance;
      new(ObjRec);
      ObjRec.init(otInstance, @Self);
      jsobj.ptr.PrivateData := ObjRec;
      result := jsobj.ptr.ToJSValue;
    finally
      cx.FreeRootedObject(jsobj);
      cx.FreeRootedObject(protoObj);
      cx.FreeRootedObject(global);
    end;

  finally
    cx.EndRequest;
  end;
end;

{ TSMCustomProtoObject }

constructor TSMCustomProtoObject.Create(Cx: PJSContext; aRttiCls: TClass; aParent: PJSRootedObject; slotIndex: integer);
var
  i: Cardinal;
  ObjRec: PSMObjectRecord;
  obj: PJSObject;
  global: PJSObject;
begin
  fRttiCls := aRttiCls;
  fCx := Cx;
  fSlotIndex := slotIndex;
  FjsObjName := StringToAnsi7(fRttiCls.ClassName);

  global := cx.CurrentGlobalOrNull;
  global.ReservedSlot[fSlotIndex] := cx.NewJSString(fRttiCls.ClassName).ToJSVal;

  FMethodsDA.Init(TypeInfo(TSMMethodDynArray), FMethods);
  InitObject(aParent);
  FJSClass := GetJSClass;

  FJSClass.Name := PCChar(FjsObjName);
  fFirstDeterministicSlotIndex := (FJSClass.flags and (JSCLASS_RESERVED_SLOTS_MASK shl JSCLASS_RESERVED_SLOTS_SHIFT))
      shr JSCLASS_RESERVED_SLOTS_SHIFT;

  if fFirstDeterministicSlotIndex + fDeterministicCnt >255 then
    raise ESMException.Create('Too many properties');

  if fFirstDeterministicSlotIndex + uint32(FMethodsDA.Count) + 1 >255 then
    raise ESMException.Create('Too many methods');

  FJSClass.flags := FJSClass.flags and not (JSCLASS_RESERVED_SLOTS_MASK shl JSCLASS_RESERVED_SLOTS_SHIFT) or
    ((fFirstDeterministicSlotIndex + fDeterministicCnt) shl JSCLASS_RESERVED_SLOTS_SHIFT);
  FJSClassProto := FJSClass;
  FJSClassProto.flags := FJSClassProto.flags and not (JSCLASS_RESERVED_SLOTS_MASK shl JSCLASS_RESERVED_SLOTS_SHIFT) or
    ((fFirstDeterministicSlotIndex + uint32(FMethodsDA.Count) + 1) shl JSCLASS_RESERVED_SLOTS_SHIFT);
  cx.BeginRequest;
  try
    //TODO prototypes chain fjsproto
    if length(FJSProps) = 0 then begin
      obj := aParent.ptr.InitClass(cx ,nullObj , @FJSClassProto, nil, 0, nil , nil, nil, nil);
    end else begin
      SetLength(FJSProps, Length(FJSProps) + 1); // must be null terminate!!
      obj := aParent.ptr.InitClass(cx ,nullObj , @FJSClassProto, nil, 0, @FJSProps[0] , nil, nil, nil);
    end;
    obj.ReservedSlot[fFirstDeterministicSlotIndex] := aParent.ptr.ToJSValue;
    //define JS methods
    if FMethodsDA.Count > 0 then
      for i := 0 to FMethodsDA.Count-1 do with FMethods[i] do begin
        obj.ReservedSlot[fFirstDeterministicSlotIndex + i + 1] :=
        obj.DefineUCFunction(cx, PCChar16(ujsName),
          Length(ujsName), call, nargs, uint32(flags)).ToJSValue;
      end;
    new(ObjRec);
    ObjRec.init(otProto,self);
    obj.PrivateData := ObjRec;
    global.ReservedSlot[fSlotIndex] := obj.ToJSValue;
  finally
    cx.EndRequest;
  end;
end;

function CustomProtoObject_freeNative(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  nativeObj: PSMInstanceRecord;
begin
  Result := false;
  try
    if not IsInstanceObject(cx, vp.this[cx], nativeObj) then
      raise ESMException.Create('Object not Native');

    nativeObj.FreeNative;

    Result := true;
  except
    on E: Exception do
    begin
      JSError(cx, E);
    end;
  end;
end;

procedure TSMCustomProtoObject.definePrototypeMethod(const ajsName: SynUnicode; const aCall: JSNative; aNargs: uintN; aFlags: TJSPropertyAttrs);
var idx: integer;
    added: boolean;
begin
  idx := FMethodsDA.FindHashedForAdding(ajsName, added);
  if added then begin
    with FMethods[idx] do begin
      ujsName := ajsName;
      nargs := aNargs;
      call := aCall;
      flags := aFlags;
    end;
  end else
    raise ESMException.CreateUtf8('Duplicated native function %()',[ajsName]);
end;

function TSMCustomProtoObject.getMethod(const aJSFunction: PJSFunction; var obj: PJSObject): PSMMethodRec;
var
  i: Cardinal;
begin
  result := nil;
  if FMethodsDA.Count > 0 then
    for i := 0 to FMethodsDA.Count-1 do
      if obj.ReservedSlot[i+1+fFirstDeterministicSlotIndex].asObject = aJSFunction then begin
        Result := @FMethods[i];
        break;
      end;
end;

function TSMCustomProtoObject.NewSMInstance(aCx: PJSContext; argc: uintN;
  var vp: JSArgRec): TObject;
var
  ItemInstance: TClassInstance;
begin
  ItemInstance.Init(fRttiCls);
  result := ItemInstance.CreateNew;
end;

function TSMCustomProtoObject.getRTTIPropsCache(index: integer): TSMRTTIPropCache;
begin
  Result := fRTTIPropsCache[index];
end;

function TSMCustomProtoObject.GetJSClass: JSClass;
begin
  Result := jsdef_class; // default values
end;

procedure TSMCustomProtoObject.InitObject(aParent: PJSRootedObject);
begin
  definePrototypeMethod('freeNative', @CustomProtoObject_freeNative, 0, [jspEnumerate, jspPermanent, jspReadOnly]);
end;

end.

