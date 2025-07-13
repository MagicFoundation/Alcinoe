unit Alcinoe.AndroidApi.RenderScript;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

type

  {************************}
  JRenderScript = interface;
  JBaseObj = interface;
  JAllocation = interface;
  JAllocation_MipmapControl = interface;
  JType = interface;
  JScript = interface;
  JScriptIntrinsic = interface;
  JScriptIntrinsicBlur = interface;
  JElement = interface;

  {******************************************}
  JRenderScriptClass = interface(JObjectClass)
    ['{ACD2A56F-451D-4D2C-9EF0-339EE08C663F}']
    {class} function _GetCREATE_FLAG_LOW_LATENCY: Integer; cdecl;
    {class} function _GetCREATE_FLAG_LOW_POWER: Integer; cdecl;
    {class} function _GetCREATE_FLAG_NONE: Integer; cdecl;
    {class} function create(ctx: JContext): JRenderScript; cdecl; overload;
    {class} //function create(ctx: JContext; ct: JRenderScript_ContextType): JRenderScript; cdecl; overload;
    {class} //function create(ctx: JContext; ct: JRenderScript_ContextType; flags: Integer): JRenderScript; cdecl; overload;
    {class} //function createMultiContext(ctx: JContext; ct: JRenderScript_ContextType; flags: Integer; API_number: Integer): JRenderScript; cdecl;
    {class} function getMinorVersion: Int64; cdecl;
    {class} procedure releaseAllContexts; cdecl;
    {class} property CREATE_FLAG_LOW_LATENCY: Integer read _GetCREATE_FLAG_LOW_LATENCY;
    {class} property CREATE_FLAG_LOW_POWER: Integer read _GetCREATE_FLAG_LOW_POWER;
    {class} property CREATE_FLAG_NONE: Integer read _GetCREATE_FLAG_NONE;
  end;
  [JavaSignature('android/renderscript/RenderScript')]
  JRenderScript = interface(JObject)
    ['{1375A3CE-12EA-4DB9-B20F-128B944A172B}']
    //function getErrorHandler: JRenderScript_RSErrorHandler; cdecl;
    //function getMessageHandler: JRenderScript_RSMessageHandler; cdecl;
    //procedure setMessageHandler(msg: JRenderScript_RSMessageHandler); cdecl;
    //procedure setPriority(p: JRenderScript_Priority); cdecl;
    procedure contextDump; cdecl;
    procedure destroy; cdecl;
    procedure finish; cdecl;
    function getApplicationContext: JContext; cdecl;
    procedure sendMessage(id: Integer; data: TJavaArray<Integer>); cdecl;
    //procedure setErrorHandler(msg: JRenderScript_RSErrorHandler); cdecl;
  end;
  TJRenderScript = class(TJavaGenericImport<JRenderScriptClass, JRenderScript>) end;

  {*************************************}
  JBaseObjClass = interface(JObjectClass)
    ['{F1A3D5D0-1151-47D2-A690-B3B562F350CC}']
  end;
  [JavaSignature('android/renderscript/BaseObj')]
  JBaseObj = interface(JObject)
    ['{20615312-245C-4906-B481-80FDCA85B4B7}']
    procedure destroy; cdecl;
    procedure setName(name: JString); cdecl;
    function getName: JString; cdecl;
  end;
  TJBaseObj = class(TJavaGenericImport<JBaseObjClass, JBaseObj>) end;

  {*****************************************}
  JAllocationClass = interface(JBaseObjClass)
    ['{B01FABA3-B1B6-4F4F-AAB7-FCDDCEECB161}']
    {class} function _GetUSAGE_GRAPHICS_CONSTANTS: Integer; cdecl;
    {class} function _GetUSAGE_GRAPHICS_RENDER_TARGET: Integer; cdecl;
    {class} function _GetUSAGE_GRAPHICS_TEXTURE: Integer; cdecl;
    {class} function _GetUSAGE_GRAPHICS_VERTEX: Integer; cdecl;
    {class} function _GetUSAGE_IO_INPUT: Integer; cdecl;
    {class} function _GetUSAGE_IO_OUTPUT: Integer; cdecl;
    {class} function _GetUSAGE_SCRIPT: Integer; cdecl;
    {class} function _GetUSAGE_SHARED: Integer; cdecl;
    {class} //function createCubemapFromBitmap(rs: JRenderScript; b: JBitmap; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} //function createCubemapFromBitmap(rs: JRenderScript; b: JBitmap): JAllocation; cdecl; overload;
    {class} //function createCubemapFromCubeFaces(rs: JRenderScript; xpos: JBitmap; xneg: JBitmap; ypos: JBitmap; yneg: JBitmap; zpos: JBitmap; zneg: JBitmap; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} function createCubemapFromCubeFaces(rs: JRenderScript; xpos: JBitmap; xneg: JBitmap; ypos: JBitmap; yneg: JBitmap; zpos: JBitmap; zneg: JBitmap): JAllocation; cdecl; overload;
    {class} //function createFromBitmap(rs: JRenderScript; b: JBitmap; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} function createFromBitmap(rs: JRenderScript; b: JBitmap): JAllocation; cdecl; overload;
    {class} function createFromBitmapResource(rs: JRenderScript; res: JResources; id: Integer; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} function createFromBitmapResource(rs: JRenderScript; res: JResources; id: Integer): JAllocation; cdecl; overload;
    {class} function createFromString(rs: JRenderScript; str: JString; usage: Integer): JAllocation; cdecl;
    {class} //function createSized(rs: JRenderScript; e: JElement; count: Integer; usage: Integer): JAllocation; cdecl; overload;
    {class} //function createSized(rs: JRenderScript; e: JElement; count: Integer): JAllocation; cdecl; overload;
    {class} function createTyped(rs: JRenderScript; type_: JType; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} function createTyped(rs: JRenderScript; type_: JType; usage: Integer): JAllocation; cdecl; overload;
    {class} function createTyped(rs: JRenderScript; type_: JType): JAllocation; cdecl; overload;
    {class} property USAGE_GRAPHICS_CONSTANTS: Integer read _GetUSAGE_GRAPHICS_CONSTANTS;
    {class} property USAGE_GRAPHICS_RENDER_TARGET: Integer read _GetUSAGE_GRAPHICS_RENDER_TARGET;
    {class} property USAGE_GRAPHICS_TEXTURE: Integer read _GetUSAGE_GRAPHICS_TEXTURE;
    {class} property USAGE_GRAPHICS_VERTEX: Integer read _GetUSAGE_GRAPHICS_VERTEX;
    {class} property USAGE_IO_INPUT: Integer read _GetUSAGE_IO_INPUT;
    {class} property USAGE_IO_OUTPUT: Integer read _GetUSAGE_IO_OUTPUT;
    {class} property USAGE_SCRIPT: Integer read _GetUSAGE_SCRIPT;
    {class} property USAGE_SHARED: Integer read _GetUSAGE_SHARED;
  end;
  [JavaSignature('android/renderscript/Allocation')]
  JAllocation = interface(JBaseObj)
    ['{1A259581-F9C8-4808-99C1-63859107FE15}']
    procedure copy1DRangeFrom(off: Integer; count: Integer; array_: JObject); cdecl; overload;
    procedure copy1DRangeFrom(off: Integer; count: Integer; d: TJavaArray<Integer>); cdecl; overload;
    procedure copy1DRangeFrom(off: Integer; count: Integer; data: JAllocation; dataOff: Integer); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; array_: JObject); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; d: TJavaArray<Integer>); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; array_: JObject); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; d: TJavaArray<Integer>); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; d: TJavaArray<Integer>); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; d: TJavaArray<Byte>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Integer>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Single>); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Byte>); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy3DRangeFrom(xoff: Integer; yoff: Integer; zoff: Integer; w: Integer; h: Integer; d: Integer; data: JAllocation; dataXoff: Integer; dataYoff: Integer; dataZoff: Integer); cdecl; overload;
    procedure copy3DRangeTo(xoff: Integer; yoff: Integer; zoff: Integer; w: Integer; h: Integer; d: Integer; array_: JObject); cdecl;
    procedure copyFrom(d: TJavaObjectArray<JBaseObj>); cdecl; overload;
    procedure copyFrom(d: TJavaArray<Byte>); cdecl; overload;
    procedure copyFrom(d: TJavaArray<Single>); cdecl; overload;
    procedure copyFrom(b: JBitmap); cdecl; overload;
    procedure copyFromUnchecked(d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copyFromUnchecked(d: TJavaArray<Byte>); cdecl; overload;
    procedure copyFromUnchecked(d: TJavaArray<Single>); cdecl; overload;
    procedure copyTo(d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copyTo(d: TJavaArray<Integer>); cdecl; overload;
    procedure copyTo(d: TJavaArray<Single>); cdecl; overload;
    procedure destroy; cdecl;
    procedure generateMipmaps; cdecl;
    function getBytesSize: Integer; cdecl;
    function getUsage: Integer; cdecl;
    procedure ioReceive; cdecl;
    procedure ioSend; cdecl;
    //procedure setFromFieldPacker(xoff: Integer; component_number: Integer; fp: JFieldPacker); cdecl; overload;
    //procedure setFromFieldPacker(xoff: Integer; yoff: Integer; zoff: Integer; component_number: Integer; fp: JFieldPacker); cdecl; overload;
    //procedure setOnBufferAvailableListener(callback: JAllocation_OnBufferAvailableListener); cdecl;
    procedure copy1DRangeFrom(off: Integer; count: Integer; d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy1DRangeFrom(off: Integer; count: Integer; d: TJavaArray<Byte>); cdecl; overload;
    procedure copy1DRangeFrom(off: Integer; count: Integer; d: TJavaArray<Single>); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; d: TJavaArray<Byte>); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; d: TJavaArray<Single>); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; d: TJavaArray<Byte>); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; d: TJavaArray<Single>); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; array_: JObject); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; d: TJavaArray<Single>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; array_: JObject); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Byte>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: JAllocation; dataXoff: Integer; dataYoff: Integer); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; data: JBitmap); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; array_: JObject); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Integer>); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Single>); cdecl; overload;
    procedure copy3DRangeFrom(xoff: Integer; yoff: Integer; zoff: Integer; w: Integer; h: Integer; d: Integer; array_: JObject); cdecl; overload;
    procedure copyFrom(array_: JObject); cdecl; overload;
    procedure copyFrom(d: TJavaArray<Integer>); cdecl; overload;
    procedure copyFrom(d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copyFrom(a: JAllocation); cdecl; overload;
    procedure copyFromUnchecked(array_: JObject); cdecl; overload;
    procedure copyFromUnchecked(d: TJavaArray<Integer>); cdecl; overload;
    procedure copyTo(b: JBitmap); cdecl; overload;
    procedure copyTo(array_: JObject); cdecl; overload;
    procedure copyTo(d: TJavaArray<Byte>); cdecl; overload;
    //function getElement: JElement; cdecl;
    function getSurface: JSurface; cdecl;
    function getType: JType; cdecl;
    procedure resize(dimX: Integer); cdecl;
    procedure setAutoPadding(useAutoPadding: Boolean); cdecl;
    //procedure setFromFieldPacker(xoff: Integer; fp: JFieldPacker); cdecl; overload;
    procedure setSurface(sur: JSurface); cdecl;
    procedure syncAll(srcLocation: Integer); cdecl;
  end;
  TJAllocation = class(TJavaGenericImport<JAllocationClass, JAllocation>) end;

  {****************************************************}
  JAllocation_MipmapControlClass = interface(JEnumClass)
    ['{87E40B07-4700-4177-9E49-3FED1F0886F7}']
    {class} function _GetMIPMAP_FULL: JAllocation_MipmapControl; cdecl;
    {class} function _GetMIPMAP_NONE: JAllocation_MipmapControl; cdecl;
    {class} function _GetMIPMAP_ON_SYNC_TO_TEXTURE: JAllocation_MipmapControl; cdecl;
    {class} function valueOf(name: JString): JAllocation_MipmapControl; cdecl;
    {class} function values: TJavaObjectArray<JAllocation_MipmapControl>; cdecl;
    {class} property MIPMAP_FULL: JAllocation_MipmapControl read _GetMIPMAP_FULL;
    {class} property MIPMAP_NONE: JAllocation_MipmapControl read _GetMIPMAP_NONE;
    {class} property MIPMAP_ON_SYNC_TO_TEXTURE: JAllocation_MipmapControl read _GetMIPMAP_ON_SYNC_TO_TEXTURE;
  end;
  [JavaSignature('android/renderscript/Allocation$MipmapControl')]
  JAllocation_MipmapControl = interface(JEnum)
    ['{2381D33F-176C-4B5F-A896-F00D69E2B8A5}']
  end;
  TJAllocation_MipmapControl = class(TJavaGenericImport<JAllocation_MipmapControlClass, JAllocation_MipmapControl>) end;

  {***********************************}
  JTypeClass = interface(JBaseObjClass)
    ['{EA772550-90A2-4831-A62F-063EC75F6402}']
    {class} //function createX(rs: JRenderScript; e: JElement; dimX: Integer): JType; cdecl;
    {class} //function createXY(rs: JRenderScript; e: JElement; dimX: Integer; dimY: Integer): JType; cdecl;
    {class} //function createXYZ(rs: JRenderScript; e: JElement; dimX: Integer; dimY: Integer; dimZ: Integer): JType; cdecl;
  end;
  [JavaSignature('android/renderscript/Type')]
  JType = interface(JBaseObj)
    ['{1EC7E148-3497-4D42-AF8C-67FF257EECFD}']
    function getCount: Integer; cdecl;
    //function getElement: JElement; cdecl;
    function hasFaces: Boolean; cdecl;
    function hasMipmaps: Boolean; cdecl;
    function getX: Integer; cdecl;
    function getY: Integer; cdecl;
    function getZ: Integer; cdecl;
    function getYuv: Integer; cdecl;
  end;
  TJType = class(TJavaGenericImport<JTypeClass, JType>) end;

  {*************************************}
  JScriptClass = interface(JBaseObjClass)
    ['{3C4B94DB-AA9D-4EAE-BE4C-22FB4E0E6EAE}']
  end;
  [JavaSignature('android/renderscript/Script')]
  JScript = interface(JBaseObj)
    ['{25CE110C-9510-469A-A5C5-957A802D881A}']
    function getVarB(index: Integer): Boolean; cdecl;
    function getVarD(index: Integer): Double; cdecl;
    function getVarF(index: Integer): Single; cdecl;
    procedure setTimeZone(timeZone: JString); cdecl;
    procedure setVar(index: Integer; v: Single); cdecl; overload;
    procedure setVar(index: Integer; v: Double); cdecl; overload;
    procedure setVar(index: Integer; o: JBaseObj); cdecl; overload;
    //procedure setVar(index: Integer; v: JFieldPacker); cdecl; overload;
    //procedure setVar(index: Integer; v: JFieldPacker; e: JElement; dims: TJavaArray<Integer>); cdecl; overload;
    procedure bindAllocation(va: JAllocation; slot: Integer); cdecl;
    function getVarI(index: Integer): Integer; cdecl;
    function getVarJ(index: Integer): Int64; cdecl;
    //procedure getVarV(index: Integer; v: JFieldPacker); cdecl;
    procedure setVar(index: Integer; v: Integer); cdecl; overload;
    procedure setVar(index: Integer; v: Int64); cdecl; overload;
    procedure setVar(index: Integer; v: Boolean); cdecl; overload;
  end;
  TJScript = class(TJavaGenericImport<JScriptClass, JScript>) end;

  {*********************************************}
  JScriptIntrinsicClass = interface(JScriptClass)
    ['{EFFAD6DF-A3D4-461C-B594-851D221474ED}']
  end;
  [JavaSignature('android/renderscript/ScriptIntrinsic')]
  JScriptIntrinsic = interface(JScript)
    ['{584A2682-B1D5-4DD4-A5DD-CBCF599E52E8}']
  end;
  TJScriptIntrinsic = class(TJavaGenericImport<JScriptIntrinsicClass, JScriptIntrinsic>) end;

  {**********************************************************}
  JScriptIntrinsicBlurClass = interface(JScriptIntrinsicClass)
    ['{38491E8D-407C-4BA3-86D4-FE5A6419C4CB}']
    {class} function create(rs: JRenderScript; e: JElement): JScriptIntrinsicBlur; cdecl;
  end;
  [JavaSignature('android/renderscript/ScriptIntrinsicBlur')]
  JScriptIntrinsicBlur = interface(JScriptIntrinsic)
    ['{63EE931E-7DE6-468A-B5A8-6ACC301BCAF7}']
    procedure forEach(aout: JAllocation); cdecl; overload;
    //procedure forEach(aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    //function getFieldID_Input: JScript_FieldID; cdecl;
    //function getKernelID: JScript_KernelID; cdecl;
    procedure setInput(ain: JAllocation); cdecl;
    procedure setRadius(radius: Single); cdecl;
  end;
  TJScriptIntrinsicBlur = class(TJavaGenericImport<JScriptIntrinsicBlurClass, JScriptIntrinsicBlur>) end;

  {**************************************}
  JElementClass = interface(JBaseObjClass)
    ['{19D75D2F-CF5B-4EFE-9A01-A69F51A1685D}']
    {class} function ALLOCATION(rs: JRenderScript): JElement; cdecl;
    {class} function A_8(rs: JRenderScript): JElement; cdecl;
    {class} function BOOLEAN(rs: JRenderScript): JElement; cdecl;
    {class} function ELEMENT(rs: JRenderScript): JElement; cdecl;
    {class} function F16(rs: JRenderScript): JElement; cdecl;
    {class} function F16_2(rs: JRenderScript): JElement; cdecl;
    {class} function F16_3(rs: JRenderScript): JElement; cdecl;
    {class} function F16_4(rs: JRenderScript): JElement; cdecl;
    {class} function F32(rs: JRenderScript): JElement; cdecl;
    {class} function F32_2(rs: JRenderScript): JElement; cdecl;
    {class} function F32_3(rs: JRenderScript): JElement; cdecl;
    {class} function F32_4(rs: JRenderScript): JElement; cdecl;
    {class} function F64(rs: JRenderScript): JElement; cdecl;
    {class} function F64_2(rs: JRenderScript): JElement; cdecl;
    {class} function F64_3(rs: JRenderScript): JElement; cdecl;
    {class} function F64_4(rs: JRenderScript): JElement; cdecl;
    {class} function FONT(rs: JRenderScript): JElement; cdecl;
    {class} function I16(rs: JRenderScript): JElement; cdecl;
    {class} function I16_2(rs: JRenderScript): JElement; cdecl;
    {class} function I16_3(rs: JRenderScript): JElement; cdecl;
    {class} function I16_4(rs: JRenderScript): JElement; cdecl;
    {class} function I32(rs: JRenderScript): JElement; cdecl;
    {class} function I32_2(rs: JRenderScript): JElement; cdecl;
    {class} function I32_3(rs: JRenderScript): JElement; cdecl;
    {class} function I32_4(rs: JRenderScript): JElement; cdecl;
    {class} function I64(rs: JRenderScript): JElement; cdecl;
    {class} function I64_2(rs: JRenderScript): JElement; cdecl;
    {class} function I64_3(rs: JRenderScript): JElement; cdecl;
    {class} function I64_4(rs: JRenderScript): JElement; cdecl;
    {class} function I8(rs: JRenderScript): JElement; cdecl;
    {class} function I8_2(rs: JRenderScript): JElement; cdecl;
    {class} function I8_3(rs: JRenderScript): JElement; cdecl;
    {class} function I8_4(rs: JRenderScript): JElement; cdecl;
    {class} function MATRIX4X4(rs: JRenderScript): JElement; cdecl;
    {class} function MATRIX_2X2(rs: JRenderScript): JElement; cdecl;
    {class} function MATRIX_3X3(rs: JRenderScript): JElement; cdecl;
    {class} function MATRIX_4X4(rs: JRenderScript): JElement; cdecl;
    {class} function MESH(rs: JRenderScript): JElement; cdecl;
    {class} function PROGRAM_FRAGMENT(rs: JRenderScript): JElement; cdecl;
    {class} function PROGRAM_RASTER(rs: JRenderScript): JElement; cdecl;
    {class} function PROGRAM_STORE(rs: JRenderScript): JElement; cdecl;
    {class} function PROGRAM_VERTEX(rs: JRenderScript): JElement; cdecl;
    {class} function RGBA_4444(rs: JRenderScript): JElement; cdecl;
    {class} function RGBA_5551(rs: JRenderScript): JElement; cdecl;
    {class} function RGBA_8888(rs: JRenderScript): JElement; cdecl;
    {class} function RGB_565(rs: JRenderScript): JElement; cdecl;
    {class} function RGB_888(rs: JRenderScript): JElement; cdecl;
    {class} function SAMPLER(rs: JRenderScript): JElement; cdecl;
    {class} function SCRIPT(rs: JRenderScript): JElement; cdecl;
    {class} function &TYPE(rs: JRenderScript): JElement; cdecl;
    {class} function U16(rs: JRenderScript): JElement; cdecl;
    {class} function U16_2(rs: JRenderScript): JElement; cdecl;
    {class} function U16_3(rs: JRenderScript): JElement; cdecl;
    {class} function U16_4(rs: JRenderScript): JElement; cdecl;
    {class} function U32(rs: JRenderScript): JElement; cdecl;
    {class} function U32_2(rs: JRenderScript): JElement; cdecl;
    {class} function U32_3(rs: JRenderScript): JElement; cdecl;
    {class} function U32_4(rs: JRenderScript): JElement; cdecl;
    {class} function U64(rs: JRenderScript): JElement; cdecl;
    {class} function U64_2(rs: JRenderScript): JElement; cdecl;
    {class} function U64_3(rs: JRenderScript): JElement; cdecl;
    {class} function U64_4(rs: JRenderScript): JElement; cdecl;
    {class} function U8(rs: JRenderScript): JElement; cdecl;
    {class} function U8_2(rs: JRenderScript): JElement; cdecl;
    {class} function U8_3(rs: JRenderScript): JElement; cdecl;
    {class} function U8_4(rs: JRenderScript): JElement; cdecl;
    {class} function YUV(rs: JRenderScript): JElement; cdecl;
    {class} //function createPixel(rs: JRenderScript; dt: JElement_DataType; dk: JElement_DataKind): JElement; cdecl;
    {class} //function createVector(rs: JRenderScript; dt: JElement_DataType; size: Integer): JElement; cdecl;
  end;
  [JavaSignature('android/renderscript/Element')]
  JElement = interface(JBaseObj)
    ['{47E2A1DE-1101-43E3-BF42-9F8128EDD03E}']
    function getBytesSize: Integer; cdecl;
    function getSubElementArraySize(index: Integer): Integer; cdecl;
    function getSubElementCount: Integer; cdecl;
    function isCompatible(e: JElement): Boolean; cdecl;
    function isComplex: Boolean; cdecl;
    //function getDataKind: JElement_DataKind; cdecl;
    //function getDataType: JElement_DataType; cdecl;
    function getSubElement(index: Integer): JElement; cdecl;
    function getSubElementName(index: Integer): JString; cdecl;
    function getSubElementOffsetBytes(index: Integer): Integer; cdecl;
    function getVectorSize: Integer; cdecl;
  end;
  TJElement = class(TJavaGenericImport<JElementClass, JElement>) end;

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.RenderScript.JRenderScript', TypeInfo(Alcinoe.AndroidApi.RenderScript.JRenderScript));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.RenderScript.JBaseObj', TypeInfo(Alcinoe.AndroidApi.RenderScript.JBaseObj));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.RenderScript.JAllocation', TypeInfo(Alcinoe.AndroidApi.RenderScript.JAllocation));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.RenderScript.JAllocation_MipmapControl', TypeInfo(Alcinoe.AndroidApi.RenderScript.JAllocation_MipmapControl));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.RenderScript.JType', TypeInfo(Alcinoe.AndroidApi.RenderScript.JType));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.RenderScript.JScript', TypeInfo(Alcinoe.AndroidApi.RenderScript.JScript));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.RenderScript.JScriptIntrinsic', TypeInfo(Alcinoe.AndroidApi.RenderScript.JScriptIntrinsic));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.RenderScript.JScriptIntrinsicBlur', TypeInfo(Alcinoe.AndroidApi.RenderScript.JScriptIntrinsicBlur));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.RenderScript.JElement', TypeInfo(Alcinoe.AndroidApi.RenderScript.JElement));
end;

initialization
  {$IF defined(UiModeManager)}
  ALLog('Alcinoe.AndroidApi.RenderScript','initialization');
  {$ENDIF}
  RegisterTypes;

end.
