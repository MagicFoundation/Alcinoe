unit ALAndroidApi;

interface

//  Java Type    Delphi Type
//  boolean      Boolean
//  byte         ShortInt
//  char         WideChar
//  double       Double
//  float        Single
//  int          Integer
//  long         Int64
//  short        SmallInt
//  void         N/A

uses Androidapi.JNI.Widget,
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNI.Util,
     Androidapi.JNIBridge,
     Androidapi.JNI.JavaTypes;

type

  {*******************************}
  JALSoftInputListener = interface;
  JALKeyPreImeListener = interface;
  JALEditText = interface;
  JALControlHostLayout = interface;
  JLog = interface;
  JStatFs = interface;

  {***********************************************}
  JALSoftInputListenerClass = interface(IJavaClass)
    ['{30390DEB-3807-4FD3-B0A9-5D53A3CC000A}']
  end;

  {*****************************************************************}
  [JavaSignature('com/alcinoe/view/inputmethod/ALSoftInputListener')]
  JALSoftInputListener = interface(IJavaInstance)
    ['{674AD1A1-0A33-4519-978D-171D00176930}']
    procedure onSoftInputShown; cdecl;
    procedure onSoftInputHidden; cdecl;
  end;
  TJALSoftInputListener = class(TJavaGenericImport<JALSoftInputListenerClass, JALSoftInputListener>) end;

  {***********************************************}
  JALKeyPreImeListenerClass = interface(IJavaClass)
    ['{E01C70E2-4BBF-47CB-8713-5A73344E9EA9}']
  end;

  {************************************************************}
  [JavaSignature('com/alcinoe/text/method/ALKeyPreImeListener')]
  JALKeyPreImeListener = interface(IJavaInstance)
    ['{343578E2-962A-461E-ADD7-47A1E4BAA1D9}']
    function onKeyPreIme(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  end;
  TJALKeyPreImeListener = class(TJavaGenericImport<JALKeyPreImeListenerClass, JALKeyPreImeListener>) end;

  {******************************************}
  JALEditTextClass = interface(JEditTextClass)
    ['{1969D8DA-0870-47A7-8F4D-E556BC10BB41}']
    {class} function init(context: JContext): JALEditText; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet): JALEditText; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer): JALEditText; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer; defStyleRes: Integer): JALEditText; cdecl; overload;
  end;

  {**********************************************}
  [JavaSignature('com/alcinoe/widget/ALEditText')]
  JALEditText = interface(JEditText)
    ['{A3E765A1-44EB-45C0-9AA5-19A38C029CE5}']
    procedure showSoftInput; cdecl;
    procedure hideSoftInput; cdecl;
    procedure setSoftInputListener(listener: JALSoftInputListener); cdecl;
    procedure setKeyPreImeListener(listener: JALKeyPreImeListener); cdecl;
  end;
  TJALEditText = class(TJavaGenericImport<JALEditTextClass, JALEditText>) end;

  {*******************************************************}
  JALControlHostLayoutClass = interface(JLinearLayoutClass)
    ['{4BB0539E-F89F-4C09-BE60-9AD04F4BBA57}']
    {class} function init(context: JContext): JALControlHostLayout; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet): JALControlHostLayout; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer): JALControlHostLayout; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer; defStyleRes: Integer): JALControlHostLayout; cdecl; overload;
  end;

  {*******************************************************}
  [JavaSignature('com/alcinoe/widget/ALControlHostLayout')]
  JALControlHostLayout = interface(JLinearLayout)
    ['{601855E2-9BCF-4FB6-8438-FD26D55FFD8D}']
    function disableMoveAnimations: boolean; cdecl;
  end;
  TJALControlHostLayout = class(TJavaGenericImport<JALControlHostLayoutClass, JALControlHostLayout>) end;

  {***********************************}
  JLogClass = interface(JObjectClass)
    ['{E4B8D3E7-409F-41E4-A7A6-9E011CD9B87E}']
    {class} function _GetVERBOSE: Integer; cdecl;
    {class} function _GetDEBUG: Integer; cdecl;
    {class} function _GetINFO: Integer; cdecl;
    {class} function _GetWARN: Integer; cdecl;
    {class} function _GetERROR: Integer; cdecl;
    {class} function _GetASSERT: Integer; cdecl;
    {class} function init: JLog; cdecl;
    {class} function v(tag: JString; msg: JString): integer; cdecl; overload;
    {class} function v(tag: JString; msg: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function d(tag: JString; msg: JString): integer; cdecl; overload;
    {class} function d(tag: JString; msg: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function i(tag: JString; msg: JString): integer; cdecl; overload;
    {class} function i(tag: JString; msg: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function w(tag: JString; msg: JString): integer; cdecl; overload;
    {class} function w(tag: JString; msg: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function w(tag: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function e(tag: JString; msg: JString): integer; cdecl; overload;
    {class} function e(tag: JString; msg: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function wtf(tag: JString; msg: JString): integer; cdecl; overload;
    {class} function wtf(tag: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function wtf(tag: JString; msg: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function isLoggable(tag: JString; level: integer): boolean; cdecl;
    {class} function getStackTraceString(tr: JThrowable): JString; cdecl;
    {class} function println(priority: integer; tag: JString; msg: JString): integer; cdecl;
    {class} property VERBOSE: Integer read _GetVERBOSE;
    {class} property DEBUG: Integer read _GetDEBUG;
    {class} property INFO: Integer read _GetINFO;
    {class} property WARN: Integer read _GetWARN;
    {class} property ERROR: Integer read _GetERROR;
    {class} property ASSERT: Integer read _GetASSERT;
  end;

  {*********************************}
  [JavaSignature('android/util/Log')]
  JLog = interface(JObject)
    ['{AED82B19-8B1E-4F35-85D9-851D6F1B4F54}']
  end;
  TJLog = class(TJavaGenericImport<JLogClass, JLog>) end;

  {************************************}
  JStatFsClass = interface(JObjectClass)
    ['{E5587205-C324-4FAF-A101-E31BCD83BD4D}']
    {class} function init(path: JString): JStatFs; cdecl; // public StatFs(String path)
  end;

  {**********************************}
  [JavaSignature('android/os/StatFs')]
  JStatFs = interface(JObject)
    ['{121A2CDF-6B8A-4C8F-BE9A-B2DEDF861CFB}']
    procedure restat(path: JString); cdecl;
    function getBlockSize: integer; cdecl;
    function getBlockSizeLong: int64; cdecl;
    function getBlockCount: integer; cdecl;
    function getBlockCountLong: int64; cdecl;
    function getFreeBlocks: integer; cdecl;
    function getFreeBlocksLong: int64; cdecl;
    function getFreeBytes: int64; cdecl;
    function getAvailableBlocks: integer; cdecl;
    function getAvailableBlocksLong: int64; cdecl;
    function getAvailableBytes: int64; cdecl;
    function getTotalBytes: int64; cdecl;
  end;
  TJStatFs = class(TJavaGenericImport<JStatFsClass, JStatFs>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidApi.JALSoftInputListener', TypeInfo(ALAndroidApi.JALSoftInputListener));
  TRegTypes.RegisterType('ALAndroidApi.JALKeyPreImeListener', TypeInfo(ALAndroidApi.JALKeyPreImeListener));
  TRegTypes.RegisterType('ALAndroidApi.JALEditText', TypeInfo(ALAndroidApi.JALEditText));
  TRegTypes.RegisterType('ALAndroidApi.JALControlHostLayout', TypeInfo(ALAndroidApi.JALControlHostLayout));
  TRegTypes.RegisterType('ALAndroidApi.JLog', TypeInfo(ALAndroidApi.JLog));
  TRegTypes.RegisterType('ALAndroidApi.JStatFs', TypeInfo(ALAndroidApi.JStatFs));
end;

initialization
  RegisterTypes;

end.
