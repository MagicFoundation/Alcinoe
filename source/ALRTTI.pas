{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)
							
product:      ALRTTI
Version:      4.01

Description:  i create the TALRttiType object because i found then the
              TRTTI.getfields and/or the TRTTI.getmethods was very slow and
              even call internally some criticalsections :(

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :

Link :

**************************************************************}
unit ALRtti;

interface

uses System.Rtti,
     System.RTLConsts,
     System.TypInfo,
     System.Generics.Collections;

{$R-}

// http://docwiki.embarcadero.com/RADStudio/en/Conditional_compilation_(Delphi)
// http://docwiki.embarcadero.com/RADStudio/en/Compiler_Versions
{$IFDEF CPUX86}
  {$DEFINE X86ASM}
{$ELSE !CPUX86}
  {$DEFINE PUREPASCAL}
  {$DEFINE PUREPASCAL_X64ONLY}
{$ENDIF !CPUX86}

Type

  {******************}
  TALRttiType = Class;
  TALRttiOrdinalType = class;
  TALRttiSetType = class;

  {****************************}
  TALRttiObject = class(TObject)
  private
    fAttributes: TArray<TCustomAttribute>;
    FHandle: Pointer;
    //FRttiDataSize: Integer;
    //[Weak] FPackage: TRttiPackage;
    //[Weak] FParent: TRttiObject;
    //FAttributeGetter: TFunc<TArray<TCustomAttribute>>;
  private
    // TRttiObject descendants should only be retrieved via an RTTI context.
    //constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); virtual;
  public
    constructor Create(const aRttiObject: TRttiObject);
    //destructor Destroy; override;
    // The starting address of the raw data in the executable image for this RTTI.
    property Handle: Pointer read FHandle;
    // Size of data pointed to by Handle.
    //property RttiDataSize: Integer read FRttiDataSize;
    //property Parent: TRttiObject read FParent;
    //property Package: TRttiPackage read FPackage;
    function GetAttributes: TArray<TCustomAttribute>; virtual;
  end;

  {***************************************}
  TALRttiNamedObject = class(TALRttiObject)
  private
    fName: ansiString;
    //function GetName: string; virtual; abstract;
  public
    constructor Create(Const aRttiNamedObject: TRttiNamedObject);
    property Name: AnsiString read fName;
  end;

  {***************************************}
  TALRttiMember = class(TALRttiNamedObject)
  private
    fVisibility: TMemberVisibility;
    fOrder: integer;
    //function GetParent: TRttiType;
    //function GetVisibility: TMemberVisibility; virtual;
  public
    constructor Create(Const aRttiMember: TRttiMember);
    //property Parent: TRttiType read GetParent;
    property Visibility: TMemberVisibility read fVisibility;
    property Order: integer read fOrder write fOrder; // The list returned by GetMethods/getProperties/etc. are ordered by the class/interface hierarchy.
                                                      // This means that the most recently included methods or properties are located at the top of the list.
                                                      // the property Order is just a "hint" to know the hierarchy. more close to 0 it is, more higher it is
                                                      // in the hierarchy
  end;

  {*********************************}
  TALRttiField = class(TALRttiMember)
  private
    fRttiField: TRttiField;
    FFieldType: TALRttiType;
    fOffset: Integer;
    //function GetFieldType: TRttiType; virtual;
    //function GetOffset: Integer; virtual;
  public
    constructor Create(const aRttiField: TRttiField);
    property FieldType: TALRttiType read FFieldType;
    property Offset: Integer read fOffset;

    function GetValue(Instance: Pointer): TValue; virtual;
    procedure SetValue(Instance: Pointer; const AValue: TValue); virtual;
    //function ToString: string; override;
  end;

  {************************************}
  TALRttiProperty = class(TALRttiMember)
  private
    fRttiProperty: TRttiProperty;
    fPropertyType: TALRttiType;
    fIsReadable: Boolean;
    fIsWritable: Boolean;
    //function GetPropertyType: TRttiType; virtual; abstract;
    //function GetIsReadable: Boolean; virtual; abstract;
    //function GetIsWritable: Boolean; virtual; abstract;
    //function DoGetValue(Instance: Pointer): TValue; virtual; abstract;
    //procedure DoSetValue(Instance: Pointer; const AValue: TValue); virtual; abstract;
  public
    constructor Create(const aRttiProperty: TRttiProperty);
    property PropertyType: TALRttiType read fPropertyType;
    function GetValue(Instance: Pointer): TValue;
    procedure SetValue(Instance: Pointer; const AValue: TValue);
    property IsReadable: Boolean read fIsReadable;
    property IsWritable: Boolean read fIsWritable;
  end;

  {**********************************************}
  TALRttiInstanceProperty = class(TALRttiProperty)
  private
    fIndex: integer;
    fDefault: integer;
    fNameIndex: Smallint;
    fPropInfo: PPropInfo;
    //function GetDefault: Integer; virtual;
    //function GetIndex: Integer; virtual;
    //function GetNameIndex: Smallint; virtual;
    //function GetPropertyType: TRttiType; override;
    //function GetPropInfo: PPropInfo; virtual; // abstract;
    //function GetName: string; override;
    //function GetIsReadable: Boolean; override;
    //function GetIsWritable: Boolean; override;
    //function DoGetValue(Instance: Pointer): TValue; override;
    //procedure DoSetValue(Instance: Pointer; const AValue: TValue); override;
  public
    constructor Create(const aRttiInstanceProperty: TRttiInstanceProperty);
    //function ToString: string; override;
    //property PropertyType: TRttiType read GetPropertyType;
    property Index: Integer read fIndex;
    property Default: Integer read fDefault;
    property NameIndex: Smallint read fNameIndex;
    property PropInfo: PPropInfo read fPropInfo;
  end;

  {******************************************}
  TALRttiParameter = class(TALRttiNamedObject)
  private
    fParamType: TALRttiType;
    FFlags: TParamFlags;
    //function GetFlags: TParamFlags; virtual; abstract;
    //function GetParamType: TRttiType; virtual; abstract;
  public
    constructor Create(const aRttiParameter: TRttiParameter);
    //function ToString: string; override;
    property Flags: TParamFlags read fFlags;
    // ParamType may be nil if it's an untyped var or const parameter.
    property ParamType: TALRttiType read fParamType;
  end;

  {**********************************}
  TALRttiMethod = class(TALRttiMember)
  private
    fRttiMethod: TRttiMethod;
    fRttiParameters: TArray<TALRttiParameter>;
    fReturnType: TALRttiType;
    fCodeAddress: Pointer;
    fIsConstructor: Boolean;
    fIsDestructor: boolean;
    fHasExtendedInfo: Boolean;
    fMethodKind: TMethodKind;
    fDispatchKind: TDispatchKind;
    fIsClassMethod: Boolean;
    fIsStatic: Boolean;
    fVirtualIndex: Smallint;
    fCallingConvention: TCallConv;

    //FInvokeInfo: TMethodImplementation.TInvokeInfo;

    //function GetIsConstructor: Boolean;
    //function GetIsDestructor: Boolean;

    //function GetMethodKind: TMethodKind; virtual; abstract;
    //function GetCallingConvention: TCallConv; virtual; abstract;
    //function GetReturnType: TRttiType; virtual; abstract;
    //function GetDispatchKind: TDispatchKind; virtual;
    //function GetHasExtendedInfo: Boolean; virtual;
    //function GetVirtualIndex: Smallint; virtual;
    //function GetCodeAddress: Pointer; virtual;
    //function GetIsClassMethod: Boolean; virtual;
    //function GetIsStatic: Boolean; virtual;
    //function DispatchInvoke(Instance: TValue; const Args: array of TValue): TValue; virtual; abstract;
    //function GetInvokeInfo: TMethodImplementation.TInvokeInfo;
  public
    constructor Create(const aRttiMethod: TRttiMethod);
    destructor Destroy; override;
    function Invoke(Instance: TObject; const Args: array of TValue): TValue; overload;
    function Invoke(Instance: TClass; const Args: array of TValue): TValue; overload;
    function Invoke(Instance: TValue; const Args: array of TValue): TValue; overload;
    // Create an implementation of a method with this signature, which delegates
    // implementation to the passed-in callback.
    //function CreateImplementation(AUserData: Pointer;
    //  const ACallback: TMethodImplementationCallback): TMethodImplementation;
    function GetParameters: TArray<TALRttiParameter>; virtual; // abstract;
    //function ToString: string; override;
    property ReturnType: TALRttiType read fReturnType;
    property HasExtendedInfo: Boolean read fHasExtendedInfo;
    property MethodKind: TMethodKind read fMethodKind;
    property DispatchKind: TDispatchKind read fDispatchKind;

    property IsConstructor: Boolean read fIsConstructor;
    property IsDestructor: Boolean read fIsDestructor;
    property IsClassMethod: Boolean read fIsClassMethod;
    // Static: No 'Self' parameter
    property IsStatic: Boolean read fIsStatic;

    // Vtable slot for virtual methods.
    // Message index for message methods (non-negative).
    // Dynamic index for dynamic methods (negative).
    property VirtualIndex: Smallint read fVirtualIndex;
    property CallingConvention: TCallConv read fCallingConvention;
    property CodeAddress: Pointer read fCodeAddress;
    function FinalCodeAddress(Cls: TClass): Pointer; // CodeAddress calculated => no virtuals etc.
  end;

  {*******************************************}
  TALRttiIndexedProperty = class(TALRttiMember)
  private
    fRTTIIndexedProperty: TRttiIndexedProperty;
    fPropertyType: TALRttiType;
    fReadMethod: TALRttiMethod;
    fWriteMethod: TALRttiMethod;
    fHandle: PArrayPropInfo;
    fIsReadable: Boolean;
    fIsWritable: Boolean;
    fIsDefault: Boolean;
    //FReadMethod: TRttiMethod;
    //FWriteMethod: TRttiMethod;
    //procedure GetAccessors;
    //function GetPropertyType: TRttiType;
    //function GetIsReadable: Boolean;
    //function GetIsWritable: Boolean;
    //function GetIsDefault: Boolean;
    //function GetReadMethod: TRttiMethod;
    //function GetWriteMethod: TRttiMethod;
    //function GetHandle: PArrayPropInfo;
    //function GetName: string; override;
    //constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
    //function GetVisibility: TMemberVisibility; override;
  public
    constructor Create(const aRTTIIndexedProperty: TRttiIndexedProperty);
    destructor Destroy; override;
    property Handle: PArrayPropInfo read fHandle;
    property PropertyType: TALRttiType read fPropertyType;
    property ReadMethod: TALRttiMethod read fReadMethod;
    property WriteMethod: TALRttiMethod read fWriteMethod;
    function GetValue(Instance: Pointer; const Args: array of TValue): TValue;
    procedure SetValue(Instance: Pointer; const Args: array of TValue; const Value: TValue);
    property IsReadable: Boolean read fIsReadable;
    property IsWritable: Boolean read fIsWritable;
    property IsDefault: Boolean read fIsDefault;
    //function ToString: string; override;
  end;

  {*************************************}
  TALRttiType = class(TALRttiNamedObject)
  private
    fQualifiedName: ansiString;
    fHandle: PTypeInfo;
    fTypeKind: TTypeKind;
    fIsOrdinal: Boolean;
    fIsPublicType: Boolean;
    fTypeSize: Integer;
    fIsManaged: Boolean;
    fIsRecord: Boolean;
    fIsSet: Boolean;
    fIsInstance: Boolean;
    fAsOrdinal: TALRttiOrdinalType;
    fAsSet: TALRttiSetType;
    //-----
    fPrivateIndexedProperties: TArray<TALRttiIndexedProperty>;
    fPrivateProperties: TArray<TALRttiProperty>;
    fPrivateFields: TArray<TALRttiField>;
    fPrivateMethods: TArray<TALRttiMethod>;
    //-----
    fProtectedIndexedProperties: TArray<TALRttiIndexedProperty>;
    fProtectedProperties: TArray<TALRttiProperty>;
    fProtectedFields: TArray<TALRttiField>;
    fProtectedMethods: TArray<TALRttiMethod>;
    //-----
    fPublicIndexedProperties: TArray<TALRttiIndexedProperty>;
    fPublicProperties: TArray<TALRttiProperty>;
    fPublicFields: TArray<TALRttiField>;
    fPublicMethods: TArray<TALRttiMethod>;
    //-----
    fPublishedIndexedProperties: TArray<TALRttiIndexedProperty>;
    fPublishedProperties: TArray<TALRttiProperty>;
    fPublishedFields: TArray<TALRttiField>;
    fPublishedMethods: TArray<TALRttiMethod>;
    //-----
    //function GetName: string; override;
    //function GetTypeKind: TTypeKind;
    //function GetTypeData: PTypeData;
    //function GetIsManaged: Boolean;
    //function GetAsInstance: TRttiInstanceType;
    //function GetAsOrdinal: TRttiOrdinalType;
    //function GetAsRecord: TRttiRecordType;
    //function GetIsInstance: Boolean;
    //function GetIsOrdinal: Boolean;
    //function GetIsRecord: Boolean;
    //function GetHandle: PTypeInfo;
    //function GetAsSet: TRttiSetType;
    //function GetIsSet: Boolean;
    //function GetTypeSize: Integer; virtual;
    //function GetQualifiedName: ansiString;
    //function GetBaseType: TRttiType; virtual;
    //function GetIsPublicType: Boolean;
    //property TypeData: PTypeData read GetTypeData;
    //constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  protected
    function Find(const FromArray: TArray<TALRttiNamedObject>; const S: AnsiString; var Index: Integer): Boolean;
    procedure init(const aRttiType: TRttiType);
  public
    constructor Create;
    destructor Destroy; override;

    //function ToString: string; override;
    property Handle: PTypeInfo read fHandle;
    // QualifiedName is only available on types declared in interface section of units;
    // i.e. IsPublicType is true.
    property QualifiedName: ansiString read fQualifiedName;
    property IsPublicType: Boolean read fIsPublicType;
    property TypeKind: TTypeKind read fTypeKind;
    // The size of a location (variable) of this type.
    property TypeSize: Integer read fTypeSize;
    property IsManaged: Boolean read fIsManaged;

    // To make writing query code easier, hoist some methods from descendants
    // into this type. These return elements flattened across the type hierarchy
    // in order from most derived to least derived.
    //function GetMethods: TArray<TRttiMethod>; overload; virtual;
    //function GetFields: TArray<TRttiField>; virtual;
    //function GetProperties: TArray<TRttiProperty>; virtual;
    //function GetIndexedProperties: TArray<TRttiIndexedProperty>; virtual;
    //function GetMethod(const AName: string): TRttiMethod; virtual;
    //function GetMethods(const AName: string): TArray<TRttiMethod>; overload; virtual;
    //function GetField(const AName: string): TRttiField; virtual;
    //function GetProperty(const AName: string): TRttiProperty; virtual;
    //function GetIndexedProperty(const AName: string): TRttiIndexedProperty; virtual;

    function GetMethods(const aVisibility: TMemberVisibility): TArray<TALRttiMethod>; overload;
    function GetMethods(const AName: ansistring; const aVisibility: TMemberVisibility): TArray<TALRttiMethod>; overload;

    function GetFields(const aVisibility: TMemberVisibility): TArray<TALRttiField>;
    function GetField(const AName: ansistring; const aVisibility: TMemberVisibility): TALRttiField;

    function GetProperties(const aVisibility: TMemberVisibility): TArray<TALRttiProperty>;
    function GetProperty(const AName: ansistring; const aVisibility: TMemberVisibility): TALRttiProperty; overload;
    function GetProperty(const AIndex: integer; const aVisibility: TMemberVisibility): TALRttiProperty; overload;

    function GetIndexedProperties(const aVisibility: TMemberVisibility): TArray<TALRttiIndexedProperty>; overload;
    function GetIndexedProperties(const AName: ansistring; const aVisibility: TMemberVisibility): TArray<TALRttiIndexedProperty>; overload;

    //function GetDeclaredMethods: TArray<TRttiMethod>; virtual;
    //function GetDeclaredProperties: TArray<TRttiProperty>; virtual;
    //function GetDeclaredFields: TArray<TRttiField>; virtual;
    //function GetDeclaredIndexedProperties: TArray<TRttiIndexedProperty>; virtual;

    // The ancestor for types with ancestors.
    //property BaseType: TRttiType read GetBaseType;

    //property AsInstance: TRttiInstanceType read GetAsInstance;
    property IsInstance: Boolean read fIsInstance;
    property AsOrdinal: TALRttiOrdinalType read fAsOrdinal;
    property IsOrdinal: Boolean read fIsOrdinal;
    //property AsRecord: TRttiRecordType read GetAsRecord;
    property IsRecord: Boolean read fIsRecord;
    property IsSet: Boolean read fIsSet;
    property AsSet: TALRttiSetType read fAsSet;
  End;

  {*************************************}
  TALRttiOrdinalType = class(TALRttiType)
  private
    fOrdType: TOrdType;
    fMinValue: Longint;
    fMaxValue: Longint;
    //function GetMaxValue: Longint; virtual;
    //function GetMinValue: Longint; virtual;
    //function GetOrdType: TOrdType;
    //constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  protected
    //function GetTypeSize: Integer; override;
  public
    procedure init(const aRttiOrdinalType: TRttiOrdinalType);
    property OrdType: TOrdType read fOrdType;
    property MinValue: Longint read fMinValue;
    property MaxValue: Longint read fMaxValue;
  end;

  {*********************************}
  TALRttiSetType = class(TALRttiType)
  private
    fElementType: TRttiType;
    //function GetElementType: TRttiType;
    //function GetTypeSize: Integer; override;
    //constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    procedure init(const aRttiSetType: TRttiSetType);
    property ElementType: TRttiType read fElementType;
  end;

{*******************************************************************************}
function ALGetEnumName(TypeInfo: PTypeInfo; Value: Integer): ansistring; overload
function ALGetEnumName(PropInfo: PPropInfo; Value: Integer): ansistring; overload
function ALTryGetEnumValue(TypeInfo: PTypeInfo; const Name: ansistring; Var EnumValue: Integer): boolean; overload;
function ALTryGetEnumValue(PropInfo: PPropInfo; const Name: ansistring; Var EnumValue: Integer): boolean; overload;
function ALGetEnumValue(TypeInfo: PTypeInfo; const Name: ansistring): Integer; overload;
function ALGetEnumValue(PropInfo: PPropInfo; const Name: ansistring): Integer; overload;
function ALSetToString(TypeInfo: PTypeInfo; Value: Integer; const Brackets: Boolean = False): ansistring; overload;
function ALSetToString(PropInfo: PPropInfo; Value: Integer; const Brackets: Boolean = False): ansistring; overload;
function ALTryStringToSet(TypeInfo: PTypeInfo; const Value: ansistring; Var SetInt: Integer): Boolean; overload;
function ALTryStringToSet(PropInfo: PPropInfo; const Value: ansistring; Var SetInt: Integer): Boolean; overload;
function ALStringToSet(TypeInfo: PTypeInfo; const Value: ansistring): Integer; overload;
function ALStringToSet(PropInfo: PPropInfo; const Value: ansistring): Integer; overload;
function ALGetRttiType(const aQualifiedName: AnsiString): TALRttiType;
procedure ALRttiInitialization(const aQualifiedNameToSkip: array of AnsiString); overload;
procedure ALRttiInitialization; overload;
procedure ALRttiFinalization;

{*******************************}
var vALRTTIContext: TRttiContext;
    vALRttiTypeCache: TObjectDictionary<ansiString,TALRttiType>;

implementation

uses System.sysutils,
     System.Generics.Defaults,
     AlString;

{****************************************}
// P points a length field of ShortString.
function _AfterString(const P: PByte): Pointer; inline;
begin
  Result := P + P^ + 1;
end;

{**********************************************************************}
function ALGetEnumName(TypeInfo: PTypeInfo; Value: Integer): ansistring;

const
  _BooleanIdents: array [Boolean] of ansistring = ('False', 'True');

var
  P: Pointer;
  T: PTypeData;
  Len: Byte;

begin
  if TypeInfo^.Kind = tkInteger then
  begin
    Result := ALIntToStr(Value);
    Exit;
  end;
  T := GetTypeData(GetTypeData(TypeInfo)^.BaseType^);
  if (TypeInfo = System.TypeInfo(Boolean)) or (T^.MinValue < 0) then
  begin
    { LongBool/WordBool/ByteBool have MinValue < 0 and arbitrary
      content in Value; Boolean has Value in [0, 1] }
    Result := _BooleanIdents[Value <> 0];
    if SameText(HexDisplayPrefix, '0x') then
      Result := ALLowerCase(Result);
  end
  else
  begin
    P := @T^.NameList;
    while Value <> 0 do
    begin
      P := _AfterString(P);
      Dec(Value);
    end;
    //Result := _UTF8ToString(P);
    Len := PByte(P)^; // length is store in First array
    if Len <> 0 then
    begin
      P := PByte(P) + 1;
      SetLength(Result, Len);
      Move(PByte(P)^, pointer(Result)^, Len);
    end
    else result := '';

  end;
end;

{**********************************************************************}
function ALGetEnumName(PropInfo: PPropInfo; Value: Integer): ansistring;
begin
  result := ALGetEnumName(PropInfo^.PropType^, Value);
end;

{************************************************************************}
function _UTF8SameText(const Str1: ShortString; Str2: PAnsiChar): Boolean;
begin
  Result := ALSametext(Str1, Str2);
end;

{*******************************************************************************}
function _GetEnumNameValue(TypeInfo: PTypeInfo; const Name: AnsiString): Integer;
{$IFDEF PUREPASCAL}
var
  TypeData: PTypeData;
  LName: PByte;
  LStr: ansiString;
  I: Integer;
begin
  TypeData := GetTypeData(GetTypeData(TypeInfo)^.BaseType^);
  LName := PByte(@TypeData.NameList);
  for i := 0 to TypeData.MaxValue do begin
    setlength(LStr, LName^);
    ALMove(pointer(LName+1)^, pointer(LStr)^, LName^);
    if ALSameText(LStr, Name) then Exit(I);
    inc(LName, LName^ + 1);
  end;
  Result := -1;
end;
{$ELSE  PUREPASCAL}
asm //StackAligned
        { ->    EAX Pointer to type info        }
        {       EDX Pointer to string           }
        { <-    EAX Value                       }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    0

        TEST    EDX,EDX
        JE      @notFound

        {       point ESI to first name of the base type }
        XOR     ECX,ECX
        MOV     CL,[EAX].TTypeInfo.Name.Byte[0]
        MOV     EAX,[EAX].TTypeInfo.Name[ECX+1].TTypeData.BaseType
        MOV     EAX,[EAX]
        MOV     CL,[EAX].TTypeInfo.Name.Byte[0]
        LEA     ESI,[EAX].TTypeInfo.Name[ECX+1].TTypeData.NameList

        {       make EDI the high bound of the enum type }
        MOV     EDI,[EAX].TTypeInfo.Name[ECX+1].TTypeData.MaxValue

        {       EAX is our running index }
        XOR     EAX,EAX

        {       make ECX the length of the current string }

@outerLoop:
        MOVZX   ECX,[ESI].Byte[0]
        CMP     ECX,[EDX-4]
        JNE     @lengthMisMatch

        {       we know for sure the names won't be zero length }
@cmpLoop:
        TEST    [ESP],1
        JNZ     @utf8compare
        MOV     BL,[EDX+ECX-1]
        TEST    BL,$80
        JNZ     @utf8compareParam
        XOR     BL,[ESI+ECX]
        TEST    BL,$80
        JNZ     @utf8compare
        TEST    BL,0DFH
        JNE     @misMatch
        DEC     ECX
        JNE     @cmpLoop

        {       as we didn't have a mismatch, we must have found the name }
        JMP     @exit

@utf8compareParam:
        MOV     [ESP],1

@utf8compare:
        PUSH    EAX
        PUSH    EDX
        MOV     EAX,ESI
{$IFDEF ALIGN_STACK}
        SUB     ESP,4
{$ENDIF ALIGN_STACK}
        CALL    _UTF8SameText
{$IFDEF ALIGN_STACK}
        ADD     ESP,4
{$ENDIF ALIGN_STACK}
        TEST    AL,AL
        POP     EDX
        POP     EAX
        JNZ     @exit

@misMatch:
        MOVZX   ECX,[ESI].Byte[0]
@lengthMisMatch:
        INC     EAX
        LEA     ESI,[ESI+ECX+1]
        CMP     EAX,EDI
        JLE     @outerLoop

        {       we haven't found the thing - return -1  }
@notFound:
        OR      EAX,-1

@exit:

        POP     EDI
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}

{*******************************************************************************************************}
function ALTryGetEnumValue(TypeInfo: PTypeInfo; const Name: ansistring; Var EnumValue: Integer): boolean;
begin

  //
  // original code
  //
  //if TypeInfo^.Kind = tkInteger then
  //  Result := ALStrToInt(Name)
  //else
  //begin
  //  Assert(TypeInfo^.Kind = tkEnumeration);
  //  if GetTypeData(TypeInfo)^.MinValue < 0 then  // Longbool/wordbool/bytebool
  //  begin
  //    if ALSameText(Name, 'False') then
  //      Result := 0
  //    else if ALSameText(Name, 'True') then
  //      Result := -1
  //    else
  //      Result := ALStrToInt(Name);
  //  end
  //  else
  //    Result := _GetEnumNameValue(TypeInfo, Name);
  //end;

  if TypeInfo^.Kind <> tkEnumeration then exit(false);
  EnumValue := _GetEnumNameValue(TypeInfo, Name);
  result := EnumValue > -1;

end;

{*******************************************************************************************************}
function ALTryGetEnumValue(PropInfo: PPropInfo; const Name: ansistring; Var EnumValue: Integer): boolean;
begin
  result := ALTryGetEnumValue(PropInfo^.PropType^, Name, EnumValue);
end;

{****************************************************************************}
function ALGetEnumValue(TypeInfo: PTypeInfo; const Name: ansistring): Integer;
begin
  if not ALTryGetEnumValue(TypeInfo, Name, result) then raise EALException.CreateFmt('Invalid enumeration name: %s', [Name]);
end;

{****************************************************************************}
function ALGetEnumValue(PropInfo: PPropInfo; const Name: ansistring): Integer;
begin
  result := ALGetEnumValue(PropInfo^.PropType^, Name);
end;

{*******************************************************************************************************}
function ALSetToString(TypeInfo: PTypeInfo; Value: Integer; const Brackets: Boolean = False): ansistring;
var
  S: TIntegerSet;
  I: Integer;
begin
  Result := '';
  Integer(S) := Value;
  TypeInfo := GetTypeData(TypeInfo)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + ALGetEnumName(TypeInfo, I);
    end;
  if Brackets then
    Result := '[' + Result + ']';
end;

{*******************************************************************************************************}
function ALSetToString(PropInfo: PPropInfo; Value: Integer; const Brackets: Boolean = False): ansistring;
begin
  Result := ALSetToString(PropInfo^.PropType^, Value, Brackets);
end;

{****************************************************************************************************}
function ALTryStringToSet(TypeInfo: PTypeInfo; const Value: ansistring; Var SetInt: Integer): Boolean;
var
  P: PansiChar;
  EnumName: ansistring;
  EnumValue: NativeInt;
  EnumInfo: PTypeInfo;

  // grab the next enum name
  function NextWord(var P: PansiChar): ansistring;
  var
    i: Integer;
  begin
    i := 0;

    // scan til whitespace
    while not (P[i] in [',', ' ', #0,']']) do
      Inc(i);

    SetString(Result, P, i);

    // skip whitespace
    while (P[i] in [',', ' ',']']) do
      Inc(i);

    Inc(P, i);
  end;

begin
  Result := True;
  SetInt := 0;
  if Value = '' then Exit;
  P := PansiChar(Value);

  // skip leading bracket and whitespace
  while (P^ in ['[',' ']) do
    Inc(P);

  EnumInfo := GetTypeData(TypeInfo)^.CompType^;
  EnumName := NextWord(P);
  while EnumName <> '' do
  begin
    EnumValue := ALGetEnumValue(EnumInfo, EnumName);
    if EnumValue < 0 then exit(False);

    Include(TIntegerSet(SetInt), EnumValue);
    EnumName := NextWord(P);
  end;
end;

{****************************************************************************************************}
function ALTryStringToSet(PropInfo: PPropInfo; const Value: ansistring; Var SetInt: Integer): Boolean;
begin
  result := ALTryStringToSet(PropInfo^.PropType^, Value, SetInt);
end;

{****************************************************************************}
function ALStringToSet(TypeInfo: PTypeInfo; const Value: ansistring): Integer;
begin
  if not ALTryStringToSet(TypeInfo, Value, result) then raise EALException.CreateFmt('Invalid set string: %s', [Value]);
end;

{****************************************************************************}
function ALStringToSet(PropInfo: PPropInfo; const Value: ansistring): Integer;
begin
  Result := ALStringToSet(PropInfo^.PropType^, Value);
end;

{*************************************}
function ALInsufficientRtti: Exception;
begin
  Result := EInsufficientRtti.CreateRes(@SInsufficientRtti);
end;

{******************************************}
procedure ALCheckCodeAddress(code: Pointer);
begin
  if (code = nil) or (PPointer(code)^ = nil) then
    raise ALInsufficientRtti;
end;

{***************************************************************}
constructor TALRttiObject.Create(const aRttiObject: TRttiObject);
begin
  inherited create;
  fAttributes := aRttiObject.GetAttributes;
  fHandle := aRttiObject.Handle;
end;

{*************************************************************}
function TALRttiObject.GetAttributes: TArray<TCustomAttribute>;
begin
  result := fAttributes;
end;

{******************************************************************************}
constructor TALRttiNamedObject.Create(Const aRttiNamedObject: TRttiNamedObject);
begin
  inherited create(aRttiNamedObject);
  fName := ansiString(aRttiNamedObject.Name);
end;

{***************************************************************}
constructor TALRttiMember.Create(Const aRttiMember: TRttiMember);
begin
  inherited create(aRttiMember);
  fVisibility := aRttiMember.Visibility;
  fOrder := -1;
end;

{************************************************************}
constructor TALRttiField.Create(const aRttiField: TRttiField);
begin
  inherited create(aRttiField);
  fRttiField := aRttiField;
  //ENonPublicType look like :TALFormatSettings.:1
  //This will raise an exception in QualifiedName
  //function TRttiType.GetQualifiedName: string;
  //begin
  //  Result := Package.GetNameFromType(Self);
  //  if Result = '' then
  //    raise ENonPublicType.CreateResFmt(@SNonPublicType, [Name]);
  //end;
  if assigned(aRttiField.FieldType) and
     (alpos(':',aRttiField.FieldType.Handle.Name) <> 1) then fFieldType := ALGetRttiType(ansiString(aRttiField.FieldType.QualifiedName))
  else fFieldType := nil;
  fOffset := aRttiField.Offset;
end;

{********************************************************}
function TALRttiField.GetValue(Instance: Pointer): TValue;
begin
  result := FRttiField.GetValue(Instance);
end;

{***********************************************************************}
procedure TALRttiField.SetValue(Instance: Pointer; const AValue: TValue);
begin
  FRttiField.SetValue(Instance, aValue);
end;

{*********************************************************************}
constructor TALRttiProperty.Create(const aRttiProperty: TRttiProperty);
begin
  inherited create(aRttiProperty);
  fRttiProperty := aRttiProperty;
  if assigned(aRttiProperty.PropertyType) then fPropertyType := ALGetRttiType(ansiString(aRttiProperty.PropertyType.QualifiedName))
  else fPropertyType := nil;
  fIsReadable := aRttiProperty.IsReadable;
  fIsWritable := aRttiProperty.IsWritable;
end;

{***********************************************************}
function TALRttiProperty.GetValue(Instance: Pointer): TValue;
begin
  result := fRttiProperty.GetValue(Instance);
end;

{**************************************************************************}
procedure TALRttiProperty.SetValue(Instance: Pointer; const AValue: TValue);
begin
  fRttiProperty.SetValue(Instance, AValue);
end;

{*********************************************************************************************}
constructor TALRttiInstanceProperty.Create(const aRttiInstanceProperty: TRttiInstanceProperty);
begin
  inherited create(aRttiInstanceProperty);
  fIndex := aRttiInstanceProperty.Index;
  fDefault := aRttiInstanceProperty.Default;
  fNameIndex := aRttiInstanceProperty.NameIndex;
  fPropInfo := aRttiInstanceProperty.PropInfo;
end;

{************************************************************************}
constructor TALRttiParameter.Create(const aRttiParameter: TRttiParameter);
begin
  inherited create(aRttiParameter);
  FFlags := aRttiParameter.Flags;
  if assigned(aRttiParameter.ParamType) then fParamType := ALGetRttiType(ansiString(aRttiParameter.ParamType.QualifiedName))
  else fParamType := nil;
end;

{**}
type
  PVtable = ^TVtable;
  TVtable = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;

{***************************************************************}
constructor TALRttiMethod.Create(const aRttiMethod: TRttiMethod);
var aRttiParameters: TArray<TRttiParameter>;
    i: integer;
begin
  inherited create(aRttiMethod);
  fRttiMethod := aRttiMethod;
  aRttiParameters := aRttiMethod.GetParameters;
  setlength(fRttiParameters, length(aRttiParameters));
  for I := Low(aRttiParameters) to High(aRttiParameters) do
    fRttiParameters[i] := TALRttiParameter.Create(aRttiParameters[i]);
  if aRttiMethod.HasExtendedInfo and
     assigned(aRttiMethod.ReturnType) then fReturnType := ALGetRttiType(ansiString(aRttiMethod.ReturnType.QualifiedName))
  else fReturnType := nil;
  fCodeAddress := aRttiMethod.CodeAddress;
  fIsConstructor := aRttiMethod.IsConstructor;
  fIsDestructor := aRttiMethod.IsDestructor;
  fHasExtendedInfo := aRttiMethod.HasExtendedInfo;
  if aRttiMethod.HasExtendedInfo then
    fMethodKind := aRttiMethod.MethodKind;
  fDispatchKind := aRttiMethod.DispatchKind;
  fIsClassMethod := aRttiMethod.IsClassMethod;
  fIsStatic := aRttiMethod.IsStatic;
  fVirtualIndex := aRttiMethod.VirtualIndex;
  if aRttiMethod.HasExtendedInfo then
    fCallingConvention := aRttiMethod.CallingConvention;
end;

{*******************************}
destructor TALRttiMethod.Destroy;
var i: integer;
begin
  for I := Low(fRttiParameters) to High(fRttiParameters) do fRttiParameters[i].free;
  inherited;
end;

{************************************************************}
function TALRttiMethod.FinalCodeAddress(Cls: TClass): Pointer; // CodeAddress calculated => no virtuals etc.
begin
  if IsStatic then result := fCodeAddress // ex: TMarshal => class function OutString(const S: string): MarshaledString; overload; inline; static;
  else begin
    case DispatchKind of
      dkVtable: result := PVtable(Cls)^[fVirtualIndex];
      dkDynamic: result := GetDynaMethod(Cls, fVirtualIndex);
      else result := fCodeAddress; //dkStatic => not virtual nor dynamic method
                                             //dkMessage => never possible here
                                             //dkInterface => never possible here

    end;
  end;
end;

{************************************************************************************}
function TALRttiMethod.Invoke(Instance: TObject; const Args: array of TValue): TValue;
begin
  Result := fRttiMethod.Invoke(Instance, Args);
end;

{***********************************************************************************}
function TALRttiMethod.Invoke(Instance: TClass; const Args: array of TValue): TValue;
begin
  Result := fRttiMethod.Invoke(Instance, Args);
end;

{***********************************************************************************}
function TALRttiMethod.Invoke(Instance: TValue; const Args: array of TValue): TValue;
begin
  Result := fRttiMethod.Invoke(Instance, Args);
end;

{*************************************************************}
function TALRttiMethod.GetParameters: TArray<TALRttiParameter>;
begin
  result := fRttiParameters;
end;

{******************************************************************************************}
constructor TALRttiIndexedProperty.Create(const aRTTIIndexedProperty: TRttiIndexedProperty);
begin
  inherited create(aRTTIIndexedProperty);
  fRTTIIndexedProperty := aRTTIIndexedProperty;
  if assigned(aRTTIIndexedProperty.PropertyType) then fPropertyType := ALGetRttiType(ansiString(aRTTIIndexedProperty.PropertyType.QualifiedName))
  else fPropertyType := nil;
  if assigned(aRTTIIndexedProperty.ReadMethod) then fReadMethod := TALRttiMethod.Create(aRTTIIndexedProperty.ReadMethod)
  else fReadMethod := nil;
  if assigned(aRTTIIndexedProperty.WriteMethod) then fWriteMethod := TALRttiMethod.Create(aRTTIIndexedProperty.WriteMethod)
  else fWriteMethod := nil;
  fHandle := aRTTIIndexedProperty.Handle;
  fIsReadable := aRTTIIndexedProperty.IsReadable;
  fIsWritable := aRTTIIndexedProperty.IsWritable;
  fIsDefault := aRTTIIndexedProperty.IsDefault;
end;

{****************************************}
destructor TALRttiIndexedProperty.Destroy;
begin
  if assigned(fReadMethod) then fReadMethod.Free;
  if assigned(fWriteMethod) then fWriteMethod.Free;
  inherited;
end;

{***********************************************************************************************}
function TALRttiIndexedProperty.GetValue(Instance: Pointer; const Args: array of TValue): TValue;
begin
  result := FRttiIndexedProperty.GetValue(Instance, Args);
end;

{*************************************************************************************************************}
procedure TALRttiIndexedProperty.SetValue(Instance: Pointer; const Args: array of TValue; const Value: TValue);
begin
  FRttiIndexedProperty.SetValue(Instance, Args, Value);
end;

{**************************************************************************}
procedure TALRttiOrdinalType.init(const aRttiOrdinalType: TRttiOrdinalType);
begin
  inherited init(aRttiOrdinalType);
  fOrdType := aRttiOrdinalType.OrdType;
  fMinValue := aRttiOrdinalType.MinValue;
  fMaxValue := aRttiOrdinalType.MaxValue;
end;

{**************************************************************}
procedure TALRttiSetType.init(const aRttiSetType: TRttiSetType);
begin
  inherited init(aRttiSetType);
  fElementType := aRttiSetType.ElementType;
end;

{*****************************}
constructor TALRttiType.Create;
begin
 //do nothing and cancel the heritage that must be done inside the init
end;

{*****************************************************}
procedure TALRttiType.init(const aRttiType: TRttiType);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddField(const aRttiField: TRttiField; const aOrder: integer);
  begin
    case aRttiField.Visibility of
      mvPrivate:begin
                  setlength(fPrivateFields, length(fPrivateFields)+1);
                  fPrivateFields[high(fPrivateFields)] := TALRttiField.Create(aRttiField);
                  fPrivateFields[high(fPrivateFields)].fOrder := aOrder;
                end;
      mvProtected:begin
                    setlength(fPrivateFields, length(fPrivateFields)+1);
                    fPrivateFields[high(fPrivateFields)] := TALRttiField.Create(aRttiField);
                    fPrivateFields[high(fPrivateFields)].fOrder := aOrder;

                    setlength(fProtectedFields, length(fProtectedFields)+1);
                    fProtectedFields[high(fProtectedFields)] := TALRttiField.Create(aRttiField);
                    fProtectedFields[high(fProtectedFields)].fOrder := aOrder;
                  end;
      mvPublic:begin
                 setlength(fPrivateFields, length(fPrivateFields)+1);
                 fPrivateFields[high(fPrivateFields)] := TALRttiField.Create(aRttiField);
                 fPrivateFields[high(fPrivateFields)].fOrder := aOrder;

                 setlength(fProtectedFields, length(fProtectedFields)+1);
                 fProtectedFields[high(fProtectedFields)] := TALRttiField.Create(aRttiField);
                 fProtectedFields[high(fProtectedFields)].fOrder := aOrder;

                 setlength(fPublicFields, length(fPublicFields)+1);
                 fPublicFields[high(fPublicFields)] := TALRttiField.Create(aRttiField);
                 fPublicFields[high(fPublicFields)].fOrder := aOrder;
               end;
      mvPublished:begin
                    setlength(fPrivateFields, length(fPrivateFields)+1);
                    fPrivateFields[high(fPrivateFields)] := TALRttiField.Create(aRttiField);
                    fPrivateFields[high(fPrivateFields)].fOrder := aOrder;

                    setlength(fProtectedFields, length(fProtectedFields)+1);
                    fProtectedFields[high(fProtectedFields)] := TALRttiField.Create(aRttiField);
                    fProtectedFields[high(fProtectedFields)].fOrder := aOrder;

                    setlength(fPublicFields, length(fPublicFields)+1);
                    fPublicFields[high(fPublicFields)] := TALRttiField.Create(aRttiField);
                    fPublicFields[high(fPublicFields)].fOrder := aOrder;

                    setlength(fPublishedFields, length(fPublishedFields)+1);
                    fPublishedFields[high(fPublishedFields)] := TALRttiField.Create(aRttiField);
                    fPublishedFields[high(fPublishedFields)].fOrder := aOrder;
                  end;
      else raise Exception.Create('Unknown visibility');
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddIndexedProperty(const aRttiIndexedProperty: TRttiIndexedProperty; const aOrder: integer);
  begin
    case aRttiIndexedProperty.Visibility of
      mvPrivate:begin
                  setlength(fPrivateIndexedProperties, length(fPrivateIndexedProperties)+1);
                  fPrivateIndexedProperties[high(fPrivateIndexedProperties)] := TALRttiIndexedProperty.create(aRttiIndexedProperty);
                  fPrivateIndexedProperties[high(fPrivateIndexedProperties)].fOrder := aOrder;
                end;
      mvProtected:begin
                    setlength(fPrivateIndexedProperties, length(fPrivateIndexedProperties)+1);
                    fPrivateIndexedProperties[high(fPrivateIndexedProperties)] := TALRttiIndexedProperty.create(aRttiIndexedProperty);
                    fPrivateIndexedProperties[high(fPrivateIndexedProperties)].fOrder := aOrder;

                    setlength(fProtectedIndexedProperties, length(fProtectedIndexedProperties)+1);
                    fProtectedIndexedProperties[high(fProtectedIndexedProperties)] := TALRttiIndexedProperty.create(aRttiIndexedProperty);
                    fProtectedIndexedProperties[high(fProtectedIndexedProperties)].fOrder := aOrder;
                  end;
      mvPublic:begin
                 setlength(fPrivateIndexedProperties, length(fPrivateIndexedProperties)+1);
                 fPrivateIndexedProperties[high(fPrivateIndexedProperties)] := TALRttiIndexedProperty.create(aRttiIndexedProperty);
                 fPrivateIndexedProperties[high(fPrivateIndexedProperties)].fOrder := aOrder;

                 setlength(fProtectedIndexedProperties, length(fProtectedIndexedProperties)+1);
                 fProtectedIndexedProperties[high(fProtectedIndexedProperties)] := TALRttiIndexedProperty.create(aRttiIndexedProperty);
                 fProtectedIndexedProperties[high(fProtectedIndexedProperties)].fOrder := aOrder;

                 setlength(fPublicIndexedProperties, length(fPublicIndexedProperties)+1);
                 fPublicIndexedProperties[high(fPublicIndexedProperties)] := TALRttiIndexedProperty.create(aRttiIndexedProperty);
                 fPublicIndexedProperties[high(fPublicIndexedProperties)].fOrder := aOrder;
               end;
      mvPublished:begin
                    setlength(fPrivateIndexedProperties, length(fPrivateIndexedProperties)+1);
                    fPrivateIndexedProperties[high(fPrivateIndexedProperties)] := TALRttiIndexedProperty.create(aRttiIndexedProperty);
                    fPrivateIndexedProperties[high(fPrivateIndexedProperties)].fOrder := aOrder;

                    setlength(fProtectedIndexedProperties, length(fProtectedIndexedProperties)+1);
                    fProtectedIndexedProperties[high(fProtectedIndexedProperties)] := TALRttiIndexedProperty.create(aRttiIndexedProperty);
                    fProtectedIndexedProperties[high(fProtectedIndexedProperties)].fOrder := aOrder;

                    setlength(fPublicIndexedProperties, length(fPublicIndexedProperties)+1);
                    fPublicIndexedProperties[high(fPublicIndexedProperties)] := TALRttiIndexedProperty.create(aRttiIndexedProperty);
                    fPublicIndexedProperties[high(fPublicIndexedProperties)].fOrder := aOrder;

                    setlength(fPublishedIndexedProperties, length(fPublishedIndexedProperties)+1);
                    fPublishedIndexedProperties[high(fPublishedIndexedProperties)] := TALRttiIndexedProperty.create(aRttiIndexedProperty);
                    fPublishedIndexedProperties[high(fPublishedIndexedProperties)].fOrder := aOrder;
                  end;
      else raise Exception.Create('Unknown visibility');
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddMethod(const aRttiMethod: TRttiMethod; const aOrder: integer);
  begin
    case aRttiMethod.Visibility of
      mvPrivate:begin
                  setlength(fPrivateMethods, length(fPrivateMethods)+1);
                  fPrivateMethods[high(fPrivateMethods)] := TALRttiMethod.create(aRttiMethod);
                  fPrivateMethods[high(fPrivateMethods)].fOrder := aOrder;
                end;
      mvProtected:begin
                    setlength(fPrivateMethods, length(fPrivateMethods)+1);
                    fPrivateMethods[high(fPrivateMethods)] := TALRttiMethod.create(aRttiMethod);
                    fPrivateMethods[high(fPrivateMethods)].fOrder := aOrder;

                    setlength(fProtectedMethods, length(fProtectedMethods)+1);
                    fProtectedMethods[high(fProtectedMethods)] := TALRttiMethod.create(aRttiMethod);
                    fProtectedMethods[high(fProtectedMethods)].fOrder := aOrder;
                  end;
      mvPublic:begin
                 setlength(fPrivateMethods, length(fPrivateMethods)+1);
                 fPrivateMethods[high(fPrivateMethods)] := TALRttiMethod.create(aRttiMethod);
                 fPrivateMethods[high(fPrivateMethods)].fOrder := aOrder;

                 setlength(fProtectedMethods, length(fProtectedMethods)+1);
                 fProtectedMethods[high(fProtectedMethods)] := TALRttiMethod.create(aRttiMethod);
                 fProtectedMethods[high(fProtectedMethods)].fOrder := aOrder;

                 setlength(fPublicMethods, length(fPublicMethods)+1);
                 fPublicMethods[high(fPublicMethods)] := TALRttiMethod.create(aRttiMethod);
                 fPublicMethods[high(fPublicMethods)].fOrder := aOrder;
               end;
      mvPublished:begin
                    setlength(fPrivateMethods, length(fPrivateMethods)+1);
                    fPrivateMethods[high(fPrivateMethods)] := TALRttiMethod.create(aRttiMethod);
                    fPrivateMethods[high(fPrivateMethods)].fOrder := aOrder;

                    setlength(fProtectedMethods, length(fProtectedMethods)+1);
                    fProtectedMethods[high(fProtectedMethods)] := TALRttiMethod.create(aRttiMethod);
                    fProtectedMethods[high(fProtectedMethods)].fOrder := aOrder;

                    setlength(fPublicMethods, length(fPublicMethods)+1);
                    fPublicMethods[high(fPublicMethods)] := TALRttiMethod.create(aRttiMethod);
                    fPublicMethods[high(fPublicMethods)].fOrder := aOrder;

                    setlength(fPublishedMethods, length(fPublishedMethods)+1);
                    fPublishedMethods[high(fPublishedMethods)] := TALRttiMethod.create(aRttiMethod);
                    fPublishedMethods[high(fPublishedMethods)].fOrder := aOrder;
                  end;
      else raise Exception.Create('Unknown visibility');
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddProperty(const aRttiProperty: TRttiProperty; const aOrder: integer);
  begin
    case aRttiProperty.Visibility of
      mvPrivate:begin
                  setlength(fPrivateProperties, length(fPrivateProperties)+1);
                  fPrivateProperties[high(fPrivateProperties)] := TALRttiInstanceProperty.Create(TRttiInstanceProperty(aRttiProperty));
                  fPrivateProperties[high(fPrivateProperties)].fOrder := aOrder;
                end;
      mvProtected:begin
                    setlength(fPrivateProperties, length(fPrivateProperties)+1);
                    fPrivateProperties[high(fPrivateProperties)] := TALRttiInstanceProperty.Create(TRttiInstanceProperty(aRttiProperty));
                    fPrivateProperties[high(fPrivateProperties)].fOrder := aOrder;

                    setlength(fProtectedProperties, length(fProtectedProperties)+1);
                    fProtectedProperties[high(fProtectedProperties)] := TALRttiInstanceProperty.Create(TRttiInstanceProperty(aRttiProperty));
                    fProtectedProperties[high(fProtectedProperties)].fOrder := aOrder;
                  end;
      mvPublic:begin
                 setlength(fPrivateProperties, length(fPrivateProperties)+1);
                 fPrivateProperties[high(fPrivateProperties)] := TALRttiInstanceProperty.Create(TRttiInstanceProperty(aRttiProperty));
                 fPrivateProperties[high(fPrivateProperties)].fOrder := aOrder;

                 setlength(fProtectedProperties, length(fProtectedProperties)+1);
                 fProtectedProperties[high(fProtectedProperties)] := TALRttiInstanceProperty.Create(TRttiInstanceProperty(aRttiProperty));
                 fProtectedProperties[high(fProtectedProperties)].fOrder := aOrder;

                 setlength(fPublicProperties, length(fPublicProperties)+1);
                 fPublicProperties[high(fPublicProperties)] := TALRttiInstanceProperty.Create(TRttiInstanceProperty(aRttiProperty));
                 fPublicProperties[high(fPublicProperties)].fOrder := aOrder;
               end;
      mvPublished:begin
                    setlength(fPrivateProperties, length(fPrivateProperties)+1);
                    fPrivateProperties[high(fPrivateProperties)] := TALRttiInstanceProperty.Create(TRttiInstanceProperty(aRttiProperty));
                    fPrivateProperties[high(fPrivateProperties)].fOrder := aOrder;

                    setlength(fProtectedProperties, length(fProtectedProperties)+1);
                    fProtectedProperties[high(fProtectedProperties)] := TALRttiInstanceProperty.Create(TRttiInstanceProperty(aRttiProperty));
                    fProtectedProperties[high(fProtectedProperties)].fOrder := aOrder;

                    setlength(fPublicProperties, length(fPublicProperties)+1);
                    fPublicProperties[high(fPublicProperties)] := TALRttiInstanceProperty.Create(TRttiInstanceProperty(aRttiProperty));
                    fPublicProperties[high(fPublicProperties)].fOrder := aOrder;

                    setlength(fPublishedProperties, length(fPublishedProperties)+1);
                    fPublishedProperties[high(fPublishedProperties)] := TALRttiInstanceProperty.Create(TRttiInstanceProperty(aRttiProperty));
                    fPublishedProperties[high(fPublishedProperties)].fOrder := aOrder;
                  end;
      else raise Exception.Create('Unknown visibility');
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _sortArray(var aArray: TArray<TALRttiMember>);
  begin
    TArray.sort<TALRttiMember>(aArray,
                               TDelegatedComparer<TALRttiMember>.Construct(function(const Left, Right: TALRttiMember): Integer
                               begin
                                 Result := ALcompareText(Left.Name, Right.Name);
                                 if result = 0 then result := Left.Order - Right.Order;
                               end));
  end;

var i: integer;

begin

  inherited create(aRttiType);
  fQualifiedName := ansiString(aRttiType.QualifiedName);
  fHandle := aRttiType.Handle;
  fTypeKind := aRttiType.TypeKind;
  fIsOrdinal := aRttiType.IsOrdinal;
  fIsPublicType := aRttiType.IsPublicType;
  fTypeSize := aRttiType.TypeSize;
  fIsManaged := aRttiType.IsManaged;
  fIsRecord := aRttiType.IsRecord;
  fIsSet := aRttiType.IsSet;
  fIsInstance := aRttiType.IsInstance;
  if IsOrdinal and assigned(aRttiType.AsOrdinal) then fAsOrdinal := TALRttiOrdinalType(ALGetRttiType(ansiString(aRttiType.AsOrdinal.QualifiedName)))
  else fAsOrdinal := nil;
  if isSet and assigned(aRttiType.AsSet) then fAsSet := TALRttiSetType(ALGetRttiType(ansiString(aRttiType.AsSet.QualifiedName)))
  else fAsSet := nil;
  //-----
  setlength(fPrivateIndexedProperties, 0);
  setlength(fPrivateProperties, 0);
  setlength(fPrivateFields, 0);
  setlength(fPrivateMethods, 0);
  //-----
  setlength(fProtectedIndexedProperties, 0);
  setlength(fProtectedProperties, 0);
  setlength(fProtectedFields, 0);
  setlength(fProtectedMethods, 0);
  //-----
  setlength(fPublicIndexedProperties, 0);
  setlength(fPublicProperties, 0);
  setlength(fPublicFields, 0);
  setlength(fPublicMethods, 0);
  //-----
  setlength(fPublishedIndexedProperties, 0);
  setlength(fPublishedProperties, 0);
  setlength(fPublishedFields, 0);
  setlength(fPublishedMethods, 0);
  //-----
  for i:=low(aRttiType.GetProperties) to high(aRttiType.GetProperties) do _AddProperty(aRttiType.GetProperties[i], i);
  for i:=low(aRttiType.GetIndexedProperties) to high(aRttiType.GetIndexedProperties) do _AddIndexedProperty(aRttiType.GetIndexedProperties[i],i);
  for i:=low(aRttiType.GetFields) to high(aRttiType.GetFields) do _AddField(aRttiType.GetFields[i],i);
  for i:=low(aRttiType.GetMethods) to high(aRttiType.GetMethods) do _AddMethod(aRttiType.GetMethods[i],i);
  //-----
  _sortArray(TArray<TALRttiMember>(fPrivateIndexedProperties));
  _sortArray(TArray<TALRttiMember>(fPrivateProperties));
  _sortArray(TArray<TALRttiMember>(fPrivateFields));
  _sortArray(TArray<TALRttiMember>(fPrivateMethods));
  //-----
  _sortArray(TArray<TALRttiMember>(fProtectedIndexedProperties));
  _sortArray(TArray<TALRttiMember>(fProtectedProperties));
  _sortArray(TArray<TALRttiMember>(fProtectedFields));
  _sortArray(TArray<TALRttiMember>(fProtectedMethods));
  //-----
  _sortArray(TArray<TALRttiMember>(fPublicIndexedProperties));
  _sortArray(TArray<TALRttiMember>(fPublicProperties));
  _sortArray(TArray<TALRttiMember>(fPublicFields));
  _sortArray(TArray<TALRttiMember>(fPublicMethods));
  //-----
  _sortArray(TArray<TALRttiMember>(fPublishedIndexedProperties));
  _sortArray(TArray<TALRttiMember>(fPublishedProperties));
  _sortArray(TArray<TALRttiMember>(fPublishedFields));
  _sortArray(TArray<TALRttiMember>(fPublishedMethods));

end;

{*****************************}
destructor TALRttiType.Destroy;
var i: integer;
begin

  // free properties
  for i := Low(fPrivateProperties) to High(fPrivateProperties) do fPrivateProperties[i].Free;
  for i := Low(fProtectedProperties) to High(fProtectedProperties) do fProtectedProperties[i].Free;
  for i := Low(fPublicProperties) to High(fPublicProperties) do fPublicProperties[i].Free;
  for i := Low(fPublishedProperties) to High(fPublishedProperties) do fPublishedProperties[i].Free;

  // free indexed properties
  for i := Low(fPrivateIndexedProperties) to High(fPrivateIndexedProperties) do fPrivateIndexedProperties[i].Free;
  for i := Low(fProtectedIndexedProperties) to High(fProtectedIndexedProperties) do fProtectedIndexedProperties[i].Free;
  for i := Low(fPublicIndexedProperties) to High(fPublicIndexedProperties) do fPublicIndexedProperties[i].Free;
  for i := Low(fPublishedIndexedProperties) to High(fPublishedIndexedProperties) do fPublishedIndexedProperties[i].Free;

  // free methods
  for i := Low(fPrivateMethods) to High(fPrivateMethods) do fPrivateMethods[i].Free;
  for i := Low(fProtectedMethods) to High(fProtectedMethods) do fProtectedMethods[i].Free;
  for i := Low(fPublicMethods) to High(fPublicMethods) do fPublicMethods[i].Free;
  for i := Low(fPublishedMethods) to High(fPublishedMethods) do fPublishedMethods[i].Free;

  // free fields
  for i := Low(fPrivateFields) to High(fPrivateFields) do fPrivateFields[i].Free;
  for i := Low(fProtectedFields) to High(fProtectedFields) do fProtectedFields[i].Free;
  for i := Low(fPublicFields) to High(fPublicFields) do fPublicFields[i].Free;
  for i := Low(fPublishedFields) to High(fPublishedFields) do fPublishedFields[i].Free;

  inherited;
end;

{***********************************************************************************************************************}
function TALRttiType.Find(const FromArray: TArray<TALRttiNamedObject>; const S: AnsiString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := length(FromArray) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := ALCompareText(FromArray[I].fName, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

{*****************************************************************************************}
function TALRttiType.GetFields(const aVisibility: TMemberVisibility): TArray<TALRttiField>;
begin
  case aVisibility of
    mvPrivate:result := fPrivateFields;
    mvProtected:result := fProtectedFields;
    mvPublic:result := fPublicFields;
    mvPublished:result := fPublishedFields;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{*********************************************************************************************************}
function TALRttiType.GetField(const AName: ansistring; const aVisibility: TMemberVisibility): TALRttiField;
var aRttiFields: TArray<TALRttiField>;
    I: integer;
begin
  aRttiFields := GetFields(aVisibility);
  if Find(TArray<TALRttiNamedObject>(aRttiFields), AName, I) then result := aRttiFields[i]
  else result := nil;
end;

{**************************************************************************************************************}
function TALRttiType.GetIndexedProperties(const aVisibility: TMemberVisibility): TArray<TALRttiIndexedProperty>;
begin
  case aVisibility of
    mvPrivate:result := fPrivateIndexedProperties;
    mvProtected:result := fProtectedIndexedProperties;
    mvPublic:result := fPublicIndexedProperties;
    mvPublished:result := fPublishedIndexedProperties;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{***************************************************************************************************************************************}
function TALRttiType.GetIndexedProperties(const AName: ansistring; const aVisibility: TMemberVisibility): TArray<TALRttiIndexedProperty>;
var aRttiIndexedProperties: TArray<TALRttiIndexedProperty>;
    aRttiIndexedProperty: TALRttiIndexedProperty;
    I,J,K: integer;
begin
  if aName = '' then begin
    aRttiIndexedProperties := GetIndexedProperties(aVisibility);
    setlength(result, length(aRttiIndexedProperties));
    i := 0;
    for aRttiIndexedProperty in aRttiIndexedProperties do
      if aRttiIndexedProperty.IsDefault then begin
        result[i] := aRttiIndexedProperty;
        inc(i);
      end;
    setlength(result,i);
  end
  else begin
    aRttiIndexedProperties := GetIndexedProperties(aVisibility);
    if Find(TArray<TALRttiNamedObject>(aRttiIndexedProperties), AName, I) then begin

      J := I - 1;
      while J >= 0 do begin
        if ALSameText(aRttiIndexedProperties[j].Name, AName) then dec(J)
        else break;
      end;

      K := I + 1;
      while K <= length(aRttiIndexedProperties) - 1 do begin
        if ALSameText(aRttiIndexedProperties[K].Name, AName) then inc(K)
        else break;
      end;

      SetLength(Result, K-J-1);
      for I := J+1 to K-1 do
        Result[I-j-1] := aRttiIndexedProperties[I];

    end
    else Exit(nil);
  end
end;

{*******************************************************************************************}
function TALRttiType.GetMethods(const aVisibility: TMemberVisibility): TArray<TALRttiMethod>;
begin
  case aVisibility of
    mvPrivate:result := fPrivateMethods;
    mvProtected:result := fProtectedMethods;
    mvPublic:result := fPublicMethods;
    mvPublished:result := fPublishedMethods;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{********************************************************************************************************************}
function TALRttiType.GetMethods(const AName: ansistring; const aVisibility: TMemberVisibility): TArray<TALRttiMethod>;
var aRTTIMethods: TArray<TALRttiMethod>;
    I,J,K: integer;
begin
  aRTTIMethods := GetMethods(aVisibility);
  if Find(TArray<TALRttiNamedObject>(aRTTIMethods), AName, I) then begin

    J := I - 1;
    while J >= 0 do begin
      if ALSameText(aRTTIMethods[j].Name, AName) then dec(J)
      else break;
    end;

    K := I + 1;
    while K <= length(aRTTIMethods) - 1 do begin
      if ALSameText(aRTTIMethods[K].Name, AName) then inc(K)
      else break;
    end;

    SetLength(Result, K-J-1);
    for I := J+1 to K-1 do
      Result[I-j-1] := aRTTIMethods[I];

  end
  else Exit(nil);
end;

{************************************************************************************************}
function TALRttiType.GetProperties(const aVisibility: TMemberVisibility): TArray<TALRttiProperty>;
begin
  case aVisibility of
    mvPrivate:result := fPrivateProperties;
    mvProtected:result := fProtectedProperties;
    mvPublic:result := fPublicProperties;
    mvPublished:result := fPublishedProperties;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{***************************************************************************************************************}
function TALRttiType.GetProperty(const AName: ansistring; const aVisibility: TMemberVisibility): TALRttiProperty;
var aRttiProperties: TArray<TALRttiProperty>;
    I: integer;
begin
  aRttiProperties := GetProperties(aVisibility);
  if Find(TArray<TALRttiNamedObject>(aRttiProperties), AName, I) then result := aRttiProperties[i]
  else result := nil;
end;

{*************************************************************************************************************}
function TALRttiType.GetProperty(const AIndex: integer; const aVisibility: TMemberVisibility): TALRttiProperty;
begin
  for Result in GetProperties(aVisibility) do
    if TRttiInstanceProperty(Result).Index = AIndex then
      Exit;
  Result := nil;
end;

{********************************************************************}
function ALGetRttiType(const aQualifiedName: AnsiString): TALRttiType;
begin
  if not vALRttiTypeCache.TryGetValue(aQualifiedName, result) then
    raise EALException.CreateFmt('Cannot obtain RTTI informations about the class %s', [aQualifiedName]);
end;

{******************************************************************************}
procedure ALRttiInitialization(const aQualifiedNameToSkip: array of AnsiString);
var aRttiTypes: TArray<TRttiType>;
    aRttiType: TALRttiType;
    aQualifiedName: AnsiString;
    aContinue: Boolean;
    i, j: integer;
begin

  //create vALRTTIContext
  vALRTTIContext := TRttiContext.Create;

  //create vALRttiTypeCache
  vALRttiTypeCache := TObjectDictionary<ansiString,TALRttiType>.create([doOwnsValues]);

  //init aRTTITypes
  aRTTITypes := vALRTTIContext.GetTypes;

  //first loop to create all the node inside _RttiTypeCache
  for I := Low(aRttiTypes) to High(aRttiTypes) do begin
    aContinue := True;
    aQualifiedName := ansiString(aRttiTypes[i].QualifiedName);
    for j := Low(aQualifiedNameToSkip) to High(aQualifiedNameToSkip) do begin
      if ALMatchesMask(aQualifiedName, aQualifiedNameToSkip[j]) then begin
        aContinue := False;
        break;
      end;
    end;
    if aContinue then begin
      if not vALRttiTypeCache.ContainsKey(aQualifiedName) then begin
        if aRttiTypes[i] is TRttiOrdinalType then aRttiType := TALRttiOrdinalType.Create
        else if aRttiTypes[i] is TRttiSetType then aRttiType := TALRttiSetType.Create
        else aRttiType := TALRttiType.Create;
        vALRttiTypeCache.Add(aQualifiedName, aRttiType);
      end
      else raise Exception.Create('Error 3A680CE4-FB3F-4C9C-8717-EA5FFB0BBFF6'); // not possible error
    end;
  end;

  //3rd loop to init all the fRttiType inside each node of _RttiTypeCache
  for I := Low(aRttiTypes) to High(aRttiTypes) do begin
    if vALRttiTypeCache.TryGetValue(ansiString(aRttiTypes[i].QualifiedName), aRttiType) then
      aRttiType.init(aRttiTypes[i]);
  end;

end;

{*****************************}
procedure ALRttiInitialization;
begin
  ALRttiInitialization([]);
end;

{***************************}
procedure ALRttiFinalization;
Begin
  vALRTTIContext.Free;
  vALRttiTypeCache.Free;
End;

end.