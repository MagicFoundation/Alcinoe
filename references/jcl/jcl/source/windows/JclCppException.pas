{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is SystemCppException.pas.                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Moritz Beutel. Portions created by Moritz Beutel   }
{ are Copyright (C) Moritz Beutel. All Rights Reserved.                                            }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Moritz Beutel                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Routines to enable the Delphi RTL to catch, dispatch and clean up C++ exceptions and to handle   }
{ exceptions derived from std::exception.                                                          }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclCppException;

interface

{$I jcl.inc}
{$I windowsonly.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils;
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils;
  {$ENDIF ~HAS_UNITSCOPE}

{$IFDEF BORLAND}

type
  PJclCppStdException = type Pointer; { mapped to std::exception* via $HPPEMIT }
  {$EXTERNALSYM PJclCppStdException}

  { C++ exception of any type }
  EJclCppException = class(Exception)
  {$IFDEF COMPILER15_UP}
  private type
    TInaccessibleType = class end;
    TPointerType<T> = record
      type TPointer = ^T;
    end;
  {$ENDIF COMPILER15_UP}
  private
    FTypeName: AnsiString;
    FExcDesc: Pointer;
    FAcquired: Boolean;

    constructor CreateTypeNamed(ATypeName: PAnsiChar; ExcDesc: Pointer); overload;
    function GetCppExceptionObject: Pointer;
    function GetThrowLine: Integer;
    function GetThrowFile: AnsiString;
  protected
    {$IFDEF COMPILER12_UP} // TODO: this may be supported for earlier versions of Delphi/C++Builder
    procedure RaisingException(P: PExceptionRecord); override;
    {$ENDIF COMPILER12_UP}
  public
    property CppExceptionObject: Pointer read GetCppExceptionObject;
    property ThrowLine: Integer read GetThrowLine;
    property ThrowFile: AnsiString read GetThrowFile;
    property TypeName: AnsiString read FTypeName;

    function IsCppClass: Boolean; overload;
    function AsCppClass(CppClassName: AnsiString): Pointer; overload;

    {$IFDEF COMPILER15_UP}
    { These are only accessible from C++ }
    {$IFNDEF WIN64}
    function IsCppClass<TCppClass: TInaccessibleType>: Boolean; overload;
    function AsCppClass<TCppClass: TInaccessibleType>: TPointerType<TCppClass>.TPointer; overload;
    {$ENDIF ~WIN64}
    {$ENDIF COMPILER15_UP}

    destructor Destroy; override;
  end;

  { C++ exception derived from std::exception }
  EJclCppStdException = class(EJclCppException)
  private
    FExcObj: PJclCppStdException;

    constructor Create(AExcObj: PJclCppStdException; Msg: String;
      ATypeName: PAnsiChar; ExcDesc: Pointer); overload;
    function GetStdException: PJclCppStdException;
  public
    { This property returns a pointer to the wrapped exception. }
    property StdException: PJclCppStdException read GetStdException;
  end;

  (*$HPPEMIT '#include <typeinfo>'*)
  (*$HPPEMIT '#include <exception>'*)
  (*$HPPEMIT ''*)
  (*$HPPEMIT 'namespace Jclcppexception'*)
  (*$HPPEMIT '{'*)
  (*$HPPEMIT ''*)
  (*$HPPEMIT 'typedef std::exception* PJclCppStdException;'*)
  (*$HPPEMIT ''*)
  (*$HPPEMIT '} /* namespace Jclcppexception */'*)

  {$IFDEF COMPILER15_UP}
  {$IFNDEF WIN64}
  (*$HPPEMIT END 'namespace Jclcppexception'*)
  (*$HPPEMIT END '{'*)
  (*$HPPEMIT END ''*)
  (*$HPPEMIT END 'template <typename TCppClass>'*)
  (*$HPPEMIT END '    bool __fastcall EJclCppException::IsCppClass<TCppClass>(void)'*)
  (*$HPPEMIT END '{'*)
  (*$HPPEMIT END '    return IsCppClass() && AsCppClass(typeid(TCppClass).name()) != 0;'*)
  (*$HPPEMIT END '}'*)
  (*$HPPEMIT END 'template <typename TCppClass>'*)
  (*$HPPEMIT END '    EJclCppException::TPointerType__1<TCppClass>::TPointer __fastcall EJclCppException::AsCppClass<TCppClass>(void)'*)
  (*$HPPEMIT END '{'*)
  (*$HPPEMIT END '    return static_cast<typename EJclCppException::TPointerType__1<TCppClass>::TPointer>(AsCppClass(typeid(TCppClass).name()));'*)
  (*$HPPEMIT END '}'*)
  (*$HPPEMIT END ''*)
  (*$HPPEMIT END '} /* namespace Jclcppexception */'*)
  {$ENDIF ~WIN64}
  {$ENDIF COMPILER15_UP}


type
  TJclCppExceptionFlags = set of (cefPrependCppClassName);

var
  JclCppExceptionFlags: TJclCppExceptionFlags = [cefPrependCppClassName];

procedure JclInstallCppExceptionFilter;
procedure JclUninstallCppExceptionFilter;
function JclCppExceptionFilterInstalled: Boolean;

{$ENDIF BORLAND}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\windows';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$IFDEF BORLAND}

uses
  {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}
  System.AnsiStrings,
  {$ENDIF DEPRECATED_SYSUTILS_ANSISTRINGS}
  JclResources, JclHookExcept;


type
  TExceptObjProc = function(P: PExceptionRecord): Exception;

  PCppTypeId = ^TCppTypeId;

  TCppBaseList = packed record { Delphi equivalent of struct baseList in xx.h }
    blType  : PCppTypeId; // type   of this base
    blOffs  : Cardinal;   // offset of this base
    blFlags : Cardinal;   // flags
  end;

  TCppTypeId = packed record { incomplete Delphi equivalent of struct tpid in xx.h }
    tpSize        : Cardinal; // size of type in bytes
    tpMask        : Word;     // attribute bits
    tpName        : Word;     // offset of start of the zero terminated name
                              // where offset is relative from tpid base

    tpcVptrOffs   : Cardinal; // offset of vtable pointer
    tpcFlags      : Cardinal; // more flags

    { Only valid if (tpMask & TM_IS_CLASS) }
    tpcBaseList   : Word;     // offset of non-virt base list,
                              // where offset is relative from tpid base
    tpcVbasList   : Word;     // offset of virtual  base list
                              // where offset is relative from tpid base

    tpcDlOpAddr   : Pointer;  // operator delete   addr
    tpcDlOpMask   : Word;     // operator delete   convention

    tpcDaOpMask   : Word;     // operator delete[] convention
    tpcDaOpAddr   : Pointer;  // operator delete[] addr

    { Only valid if (tpcFlags & CF_HAS_DTOR), implies also (tpMask & TM_IS_CLASS) }
    tpcDtorCount  : Cardinal; // dtor count - total
    tpcNVdtCount  : Cardinal; // dtor count - non-virtual

    tpcDtorAddr   : Pointer;  // destructor addr
    tpcDtorMask   : Word;     // destructor convention

    tpcDtMembers  : Word;     // offset of list of destructible members
                              // where offset is relative from tpid base

    { ... }

    { Following is the zero terminated name, padded with zero's to
      the next dword boundary.

      Optionally (if tpMask & TM_IS_CLASS), we have next:
       - non-virtual base list, terminated by a null pointer
       -     virtual base list, terminated by a null pointer

      Optionally (if tpcFlags & CF_HAS_DTOR), we have next:
       - list of destructible members, terminated by a null pointer }
  end;

const

  { Flags for TCppTypeId.tpMask }
  TM_IS_STRUCT   = $0001;
  TM_IS_CLASS    = $0002;
  TM_IS_PTR      = $0010;
  TM_IS_REF      = $0020;
  TM_IS_VOIDPTR  = $0040;
  TM_LOCALTYPE   = $0080;
  TM_IS_CONST    = $0100;
  TM_IS_VOLATILE = $0200;
  TM_IS_ARRAY    = $0400;

  { Flags for TCppTypeId.tpcFlags }
  CF_HAS_CTOR    = $00000001;
  CF_HAS_DTOR    = $00000002;
  CF_HAS_BASES   = $00000004;
  CF_HAS_VBASES  = $00000008;
  CF_HAS_VTABPTR = $00000010;
  CF_HAS_VIRTDT  = $00000020;
  CF_HAS_RTTI    = $00000040;
  CF_DELPHICLASS = $00000080;
  CF_HAS_FARVPTR = $00001000;
  CF_HAS_GUID    = $00002000;

  { Flags for TCppExceptDesc.xdFlags }
  XDF_ISDELPHIEXCEPTION   = $00000004;
  XDF_RETHROWN            = $00000008;

type
  PCppExceptDesc = ^TCppExceptDesc;
  TCppFreeMemFP = procedure(P: PCppExceptDesc); cdecl;

  TCppExceptDesc = packed record { Delphi equivalent of struct _exceptDesc in xx.h }
    xdPrevious    : Pointer;        // previous exception or 0

    xdTypeID      : PCppTypeId;     // addr of type-id for thrown type
    xdFriendList  : PCppTypeId;     // friend list supplied to _ThrowExcept
    xdFlags       : Cardinal;       // flags passed to _ThrowExcept
    xdSize        : Cardinal;       // size of thrown value
    xdBase        : PCppTypeId;     // type-id of base type
    xdMask        : Word;           // type-id mask
    xdCflg        : Word;           // type-id class flags (or 0)

    xdFreeFunc    : TCppFreeMemFP;  // function to free memory

    xdCCaddr      : Pointer;        // copy-ctor addr
    xdCCmask      : Cardinal;       // copy-ctor mask

    xdERRaddr     : Pointer;        // address of matching ERR (when found)
    xdHtabAdr     : Pointer;        // address of matching handler

    xdContext     : Cardinal;       // context of 'catch' block

    xdThrowLine   : Cardinal;       // source line no.  of throw statement
    xdThrowFile   : PAnsiChar;      // source file name of throw statement

    xdArgType     : PCppTypeId;     // address of arg type descriptor
    xdArgAddr     : Pointer;        // address of arg copy on stack
    xdArgBuff     : AnsiChar;       // arg stored in buffer
    xdArgCopy     : AnsiChar;       // arg copied to catch arg

    xdOSESP       : LongWord;       // esp of main OS exception handler
    xdOSERR       : LongWord;       // addr of the OS ERR on entry to _ExceptionHandler
    xdOSContext   : {PContext} Pointer;       // CPU Context for an OS exception
    xdValue       : array[0..0] of AnsiChar;  // copy of thrown value
  end;

  PCppBaseList = ^TCppBaseList;

{ pre-Tiburon workaround }
{$IFNDEF COMPILER12_UP}
  PByte = PAnsiChar;
{$ENDIF ~COMPILER12_UP}


procedure ExceptionHandled(ExcDesc: PCppExceptDesc); forward;
function CppGetBase(var Obj: Pointer; TypeDesc: PCppTypeId;
  BaseName: PAnsiChar): Boolean; forward;

{$IFDEF COMPILER15_UP}
{$IFNDEF WIN64}
function EJclCppException.AsCppClass<TCppClass>: TPointerType<TCppClass>.TPointer;
begin
  Assert(False);
end;
function EJclCppException.IsCppClass<TCppClass>: Boolean;
begin
  Assert(False);
end;
{$ENDIF ~WIN64}
{$ENDIF COMPILER15_UP}

constructor EJclCppException.CreateTypeNamed(ATypeName: PAnsiChar; ExcDesc: Pointer);
begin
  inherited CreateFmt(RsCppUnhandledExceptionMsg, [ATypeName]);
  FTypeName := ATypeName;
  FExcDesc := ExcDesc;
end;

constructor EJclCppStdException.Create(AExcObj: PJclCppStdException; Msg: String;
  ATypeName: PAnsiChar; ExcDesc: Pointer);
begin
  if cefPrependCppClassName in JclCppExceptionFlags then
    inherited CreateFmt('[%s] %s', [ATypeName, Msg])
  else
    inherited Create(Msg);
  FExcObj := AExcObj;
  FTypeName := ATypeName;
  FExcDesc := ExcDesc;
end;

function EJclCppException.IsCppClass: Boolean;
var
  ExcDesc: PCppExceptDesc;
begin
  ExcDesc := PCppExceptDesc(FExcDesc);
  Result := (ExcDesc.xdTypeID.tpMask and TM_IS_CLASS) <> 0;
end;

{$IFDEF COMPILER12_UP} // TODO: this may be supported for earlier versions of Delphi/C++Builder
procedure EJclCppException.RaisingException(P: PExceptionRecord);
begin
  FAcquired := False; { if an acquired exception is re-raised, it is handed back to the RTL }
  inherited;
end;
{$ENDIF COMPILER12_UP}

function EJclCppException.GetCppExceptionObject: Pointer;
var
  ExcDesc: PCppExceptDesc;
begin
  ExcDesc := PCppExceptDesc(FExcDesc);
  Result := @ExcDesc.xdValue;
end;

function EJclCppException.GetThrowFile: AnsiString;
var
  ExcDesc: PCppExceptDesc;
begin
  ExcDesc := PCppExceptDesc(FExcDesc);
  if ExcDesc.xdThrowFile <> nil then
    Result := ExcDesc.xdThrowFile
  else
    Result := '';
end;

function EJclCppException.GetThrowLine: Integer;
var
  ExcDesc: PCppExceptDesc;
begin
  ExcDesc := PCppExceptDesc(FExcDesc);
  Result := ExcDesc.xdThrowLine;
end;

function EJclCppException.AsCppClass(CppClassName: AnsiString): Pointer;
var
  ExcDesc: PCppExceptDesc;
  Obj: Pointer;
begin
  ExcDesc := PCppExceptDesc(FExcDesc);
  Obj := @ExcDesc.xdValue;
  if CppGetBase(Obj, ExcDesc.xdTypeID, PAnsiChar(CppClassName)) then
    Result := Obj
  else
    Result := nil;
end;

threadvar
  LastCppExcDesc: PCppExceptDesc;

procedure FreeExcDesc(ExcDesc: PCppExceptDesc);
begin
  if ExcDesc = nil then
    Exit;

  { call the exception object's destructor and free the memory as it
    is done in _CatchCleanup() in xx.cpp. }
  ExceptionHandled(ExcDesc);

  { Free the memory taken up by the exception descriptor }
  ExcDesc.xdFreeFunc(ExcDesc);
end;

destructor EJclCppException.Destroy;
var
  ExcDesc: PCppExceptDesc;
begin
  ExcDesc := PCppExceptDesc(FExcDesc);
  if FAcquired then
    FreeExcDesc(ExcDesc)
  else
    { when exceptions are being re-raised, the RTL first destroys the wrapper objects created for
      non-Delphi exceptions and then notifies the exception hook (by raising a cDelphiReRaise
      exception). Because of this, we cannot destroy the original exception info if we cannot be
      sure it won't be used again. }
    LastCppExcDesc := ExcDesc;
  inherited;
end;

{ from System.pas }
const
  cDelphiReRaise      = $0EEDFADF;
  cDelphiExcept       = $0EEDFAE0;
  cDelphiFinally      = $0EEDFAE1;
  cDelphiTerminate    = $0EEDFAE2;

type
  PExceptionArguments = ^TExceptionArguments;
  TExceptionArguments = record
    ExceptAddr: Pointer;
    ExceptObj: Exception;
  end;

  TAcquireExceptionProc = procedure(Obj: Pointer);
  {$IFDEF CPU32}
  TRaiseExceptionProc = procedure(ExceptionCode, ExceptionFlags: LongWord;
    NumberOfArguments: LongWord; Args: Pointer); stdcall;
  {$ENDIF CPU32}

var
  OldAcquireExceptionProc: Pointer;
  OldRaiseExceptionProc: TRaiseExceptionProc;

procedure ExceptionAcquiredProc(Obj: Pointer);
begin
  { After our exception object has been acquired, we have to take the responsibility of
    cleaning up the C++ exception when the user decides to destroy our object. }
  if TObject(Obj) is EJclCppException then
    EJclCppException(Obj).FAcquired := True;
  if Assigned(OldAcquireExceptionProc) then
    TAcquireExceptionProc(OldAcquireExceptionProc)(Obj);
end;

procedure RaiseExceptionProc(ExceptionCode, ExceptionFlags: LongWord;
    NumberOfArguments: LongWord; Args: Pointer); stdcall;
begin
  { We make use of the fact that the RTL calls the following notifiers immediately after destroying
    the exception object. We should find the exception info in the thread-local variable defined
    above. }
  case ExceptionCode of
    cDelphiReRaise:
      ; { re-raising means that the C++ exception info will be reused; don't do anything }
    cDelphiTerminate:
      { the exception has been handled; now is the time to destroy the C++ exception object }
      FreeExcDesc(LastCppExcDesc);
  end;
  LastCppExcDesc := nil;

  if Assigned(OldRaiseExceptionProc) then
    OldRaiseExceptionProc(ExceptionCode, ExceptionFlags, NumberOfArguments, Args);
end;

function EJclCppStdException.GetStdException: PJclCppStdException;
begin
  Result := FExcObj;
end;

{ This function should basically work like destThrownValue()/callDestructor()
  in xx.cpp }
procedure DestroyThrownValue(Obj: Pointer; ObjType: PCppTypeId);
type
  TCdeclDestructor    = procedure(Obj: Pointer; Flags: Integer); cdecl;
  TPascalDestructor   = procedure(Flags: Integer; Obj: Pointer); pascal;
  TFastcallDestructor = procedure(Obj: Pointer; Flags: Integer);
  TStdcallDestructor  = procedure(Obj: Pointer; Flags: Integer); stdcall;
var
  Flags: Integer;
  CdeclDestructor: TCdeclDestructor;
  PascalDestructor: TPascalDestructor;
  FastcallDestructor: TFastcallDestructor;
  StdcallDestructor: TStdcallDestructor;
begin
  // 		callDestructor(objAddr, objType, 0, dtorAddr, dtorMask, 1);
  Flags := 2;

  Assert((ObjType.tpcDtorMask and $0080) = 0, 'fastthis (-po) not supported');

  case ObjType.tpcDtorMask and $0007 of
  1: // __cdecl
    begin
      CdeclDestructor := TCdeclDestructor(ObjType.tpcDtorAddr);
      CdeclDestructor(Obj, Flags);
    end;
  2: // __pascal
    begin
      PascalDestructor := TPascalDestructor(ObjType.tpcDtorAddr);
      PascalDestructor(Flags, Obj);
    end;
  3: // __fastcall
    begin
      FastcallDestructor := TFastcallDestructor(ObjType.tpcDtorAddr);
      FastcallDestructor(Obj, Flags);
    end;
  5: // __stdcall
    begin
      StdcallDestructor := TStdcallDestructor(ObjType.tpcDtorAddr);
      StdcallDestructor(Obj, Flags);
    end;
  else
    Assert(False, 'Unsupported calling convention!');
  end;
end;

{ This function should basically work like exceptionHandled() in xx.cpp }
procedure ExceptionHandled(ExcDesc: PCppExceptDesc);
var
  TypeDesc: PCppTypeId;
  Obj: Pointer;
begin
  { Is the thrown value still present in the descriptor? }
  if Ord(ExcDesc.xdArgBuff) <> 0 then
  begin

    { Destroy the thrown value if necessary }
    if (ExcDesc.xdCflg and CF_HAS_DTOR) <> 0 then
    begin
      TypeDesc := ExcDesc.xdBase;
      Obj := Pointer(@ExcDesc.xdValue);

      { All delphi class objects are thrown by pointer, sort of.
        However, we should not meet a Delphi class here! }
      if (TypeDesc.tpcFlags and CF_DELPHICLASS) <> 0 then
        Obj := Pointer((PCardinal(Obj))^); { dereference }

      { We can't do anything about the _DestructorCount variable here, but
        this is a legacy feature anyway. }

      { Don't destroy it if it's a delphi class and it's being rethrown. }
      if (ExcDesc.xdFlags and (XDF_ISDELPHIEXCEPTION or XDF_RETHROWN))
        <> (XDF_ISDELPHIEXCEPTION or XDF_RETHROWN) then
        DestroyThrownValue(Obj, TypeDesc);
    end;

    { Mark the fact that the arg is gone }
    ExcDesc.xdArgBuff := AnsiChar(0);
  end;

  { Did we make a copy of the argument? }
  if Ord(ExcDesc.xdArgCopy) <> 0 then
  begin
    TypeDesc := ExcDesc.xdArgType;

    { Destroy the thrown value if necessary }
    if ((TypeDesc.tpMask and TM_IS_CLASS) <> 0)
      and ((TypeDesc.tpcFlags and CF_HAS_DTOR) <> 0) then
      DestroyThrownValue(ExcDesc.xdArgAddr, TypeDesc);

    { Mark the fact that the arg is gone }
    ExcDesc.xdArgCopy := AnsiChar(0);
  end;
end;

{ It is not easy to get the C++ type description for std::exception,
  therefore we identify the base class by name.

  This function should basically work like locateBaseClass() in xxtype.cpp }
function LocateCppBaseClass(BaseList: PCppBaseList; VBase: Boolean;
  BaseName: PAnsiChar; var Addr: Pointer) : Boolean;
var
  Ptr: Pointer;
  BaseBaseList: PCppBaseList;
  BaseType: PCppTypeId;
begin
  { Check for end of base list }
  Result := False;
  while BaseList.blType <> nil do
  begin
    BaseType := BaseList.blType;

    Ptr := Pointer(PByte(Addr) + BaseList.blOffs);
    Inc(BaseList);

    if VBase then
      Ptr := Pointer((PCardinal(Ptr))^); { dereference }

    { Is this the right base class? }
    if {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrComp(PAnsiChar(PByte(BaseType) + BaseType.tpName), BaseName) = 0 then
    begin
      Addr := Ptr;    { Match --> return the adjusted pointer to the caller }
      Result := True;
      Exit;
    end;

    { Does this base class have any base classes? }
    { Annotation: Would BaseType.tpcFlags & CF_HAS_BASES be a better match for this? }
    if (BaseType.tpMask and TM_IS_CLASS) = 0 then
      Continue;

    { Get the list of non-virtual bases for this base class }
    BaseBaseList := PCppBaseList(PByte(BaseType) + BaseType.tpcBaseList);

    { Give up on this base if it has no non-virtual bases (Ann.: why?) }
    if BaseBaseList = nil then
      Continue;

    { Search the base classes of this base recursively }
    if LocateCppBaseClass(BaseBaseList, False, BaseName, Ptr) then
    begin
      Addr := Ptr;    { Match --> return the adjusted pointer to the caller }
      Result := True;
      Exit;
    end;
  end;
end;

function CppGetBase(var Obj: Pointer; TypeDesc: PCppTypeId;
  BaseName: PAnsiChar): Boolean;
var
  BaseList, VBaseList: PCppBaseList;
begin
  if {$IFDEF DEPRECATED_SYSUTILS_ANSISTRINGS}System.AnsiStrings.{$ENDIF}StrComp(PAnsiChar(PByte(TypeDesc) + TypeDesc.tpName), BaseName) = 0 then
    { a class can be considered its own base }
    Result := True
  else if (TypeDesc.tpMask and TM_IS_CLASS) <> 0 then
  begin
    { iterate through the base classes }
    BaseList  := PCppBaseList(PByte(TypeDesc) + TypeDesc.tpcBaseList);
    VBaseList := PCppBaseList(PByte(TypeDesc) + TypeDesc.tpcVbasList);
    Result := LocateCppBaseClass(BaseList,  False, BaseName, Obj)
           or LocateCppBaseClass(VBaseList, False, BaseName, Obj);
  end
  else
    Result := False; { Don't be surprised. C++ permits to throw every type. }
end;

type
  EOpenException = class(Exception);

function CppExceptObjProc(P: PExceptionRecord): Exception;
type
  { Function pointer to std::type_info::what().
    In the Dinkumware library, std::exception::what() always has the
    __cdecl calling convention. }
  TCppTypeInfoWhatMethod = function(This: Pointer): PAnsiChar; cdecl;
var
  ExcTypeName: PAnsiChar;
  ExcDesc: PCppExceptDesc;
  ExcObject: Pointer;
  ExcObjectVTbl: Pointer;
  WhatMethod: TCppTypeInfoWhatMethod;
begin
  Result := nil;
  case P.ExceptionCode of
  $0EEFFACE: { C++ exception }
    begin
      { When a C++ exception is thrown, the C++ compiler (indirectly) calls the
        tossAnException function defined in xx.cpp. The Win32 exception argument
        table is filled in line 1096ff as follows:

          argTable[0] = (unsigned long)(void __far *)__throwExceptionName;
          argTable[1] = (unsigned long)(void __far *)throwPC;
          argTable[2] = (unsigned long)(void __far *)xdp; // : PCppExceptDesc }

      ExcTypeName := PAnsiChar(P.ExceptionInformation[0]);
      ExcDesc := PCppExceptDesc(P.ExceptionInformation[2]);
      ExcObject := Pointer(@ExcDesc.xdValue);

      if CppGetBase(ExcObject, ExcDesc.xdTypeID, 'std::exception') then
      begin
        { The exception object is a std::exception subclass and implements
          the virtual member function what(). }
        ExcObjectVTbl := Pointer(PCardinal(ExcObject)^);
        WhatMethod := TCppTypeInfoWhatMethod(PCardinal(
          Cardinal(ExcObjectVTbl) + SizeOf(Pointer))^);
        Result := EJclCppStdException.Create(ExcObject, String(WhatMethod(ExcObject)),
          PAnsiChar(ExcTypeName), Pointer(ExcDesc));
      end
      else
        { The exception object has some other type. We cannot extract an
          exception message with reasonable efforts. }
        Result := EJclCppException.CreateTypeNamed(PAnsiChar(ExcTypeName), Pointer(ExcDesc));
    end;
  end;
  {$IFDEF COMPILER12_UP}
  if Result <> nil then
    EOpenException(Result).RaisingException(P);
  {$ENDIF COMPILER12_UP}
end;

var
  HookInstalled: Boolean = False;

procedure JclInstallCppExceptionFilter;
begin
  Assert(JclHookExcept.JclExceptionsHooked,
    'Cannot install C++ exception filter: call JclHookExcept.JclHookExceptions() first!');
  if HookInstalled then
    Exit;
  HookInstalled := JclHookExcept.JclAddExceptFilter(@CppExceptObjProc, npFirstChain);
  if HookInstalled then
  begin
    {$IFDEF COMPILER12_UP} // TODO: this may be supported for earlier versions of Delphi/C++Builder
    OldAcquireExceptionProc := System.ExceptionAcquired;
    System.ExceptionAcquired := @ExceptionAcquiredProc;
    {$ENDIF COMPILER12_UP}

    OldRaiseExceptionProc := System.RaiseExceptionProc;
    {$IFDEF CPU32}
    System.RaiseExceptionProc := @RaiseExceptionProc;
    {$ELSE}
    System.RaiseExceptionProc := RaiseExceptionProc;
    {$ENDIF CPU32}
  end;
end;

procedure JclUninstallCppExceptionFilter;
begin
  if not HookInstalled then
    Exit;
  {$IFDEF COMPILER12_UP} // TODO: this may be supported for earlier versions of Delphi/C++Builder
  System.ExceptionAcquired := OldAcquireExceptionProc;
  {$ENDIF COMPILER12_UP}
  {$IFDEF CPU32}
  System.RaiseExceptionProc := @OldRaiseExceptionProc;
  {$ELSE}
  System.RaiseExceptionProc := OldRaiseExceptionProc;
  {$ENDIF CPU32}
  JclHookExcept.JclRemoveExceptFilter(@CppExceptObjProc);
  HookInstalled := False;
end;

function JclCppExceptionFilterInstalled: Boolean;
begin
  Result := HookInstalled;
end;

{$ENDIF BORLAND}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

