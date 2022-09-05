{*******************************************************************************
I create the TALRttiType object because i found that the
TRTTI.getfields and/or the TRTTI.getmethods was very slow and
even call internally some criticalsections :(
*******************************************************************************}

unit ALRtti;

interface

uses
  System.Rtti,
  System.RTLConsts,
  System.TypInfo,
  System.Generics.Collections;

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
    //The list returned by GetMethods/getProperties/etc. are ordered by the class/interface hierarchy.
    //This means that the most recently included methods or properties are located at the top of the list.
    //the property Order is just a "hint" to know the hierarchy. more close to 0 it is, more higher it is
    //in the hierarchy
    property Order: integer read fOrder write fOrder;
  end;

  {*********************************}
  TALRttiField = class(TALRttiMember)
  private
    fRttiField: TRttiField;
    FFieldType: TALRttiType;
    FOffset: Integer;
    //function GetFieldType: TRttiType; virtual;
    //function GetOffset: Integer; virtual;
    //constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  public
    constructor Create(const aRttiField: TRttiField);
    property FieldType: TALRttiType read FFieldType;
    property Offset: Integer read FOffset;

    function GetValue(Instance: Pointer): TValue;
    procedure SetValue(Instance: Pointer; const AValue: TValue);
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
    //function GetNameIndex: SmallInt; virtual;
    //function GetPropertyType: TRttiType; override;
    //function GetPropInfo: PPropInfo; virtual; abstract;
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
    property NameIndex: SmallInt read fNameIndex;
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
    //function GetVirtualIndex: SmallInt; virtual;
    //function GetCodeAddress: Pointer; virtual;
    //function GetIsClassMethod: Boolean; virtual;
    //function GetIsStatic: Boolean; virtual;
    //function DispatchInvoke(Instance: TValue; const Args: array of TValue): TValue; virtual; abstract;
    //function GetInvokeInfo: TMethodImplementation.TInvokeInfo;
    //procedure GetCommonInvokeParams(var isCons, isDest, isStat, isClas: Boolean;
    //  var callConv: TCallConv);
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
    property VirtualIndex: SmallInt read fVirtualIndex;
    property CallingConvention: TCallConv read fCallingConvention;
    property CodeAddress: Pointer read fCodeAddress;
    // CodeAddress calculated => no virtuals etc.
    function FinalCodeAddress(Cls: TClass): Pointer;
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
    //function GetIsReadable: Boolean; inline;
    //function GetIsWritable: Boolean; inline;
    //function GetIsDefault: Boolean;
    //function GetReadMethod: TRttiMethod;
    //function GetWriteMethod: TRttiMethod;
    //function GetHandle: PArrayPropInfo; inline;
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
  //protected type
  //  TGetListFunc<T: TRttiNamedObject> = function (AType: TRttiType): TArray<T>;
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
    //function GetHandle: PTypeInfo; inline;
    //function GetAsSet: TRttiSetType;
    //function GetIsSet: Boolean;
    //function GetIsHFA: Boolean; virtual;
    //function GetHFAElementType: TRttiFloatType; virtual;
    //function GetHFAElementCount: Integer; virtual;
    //function GetTypeSize: Integer; virtual;
    //function GetQualifiedName: string;
    //function GetBaseType: TRttiType; virtual;
    //function GetIsPublicType: Boolean;
    //property TypeData: PTypeData read GetTypeData;
    //constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  protected
    //function GetNamedObject<T: TRttiNamedObject>(const AName: string; const AGetListFunc: TGetListFunc<T>): T;
    //function GetObjectList<T: TRttiNamedObject>(const AGetListFunc: TGetListFunc<T>): TArray<T>;
    function Find(const FromArray: TArray<TALRttiNamedObject>; const S: AnsiString; var Index: Integer): Boolean;
    procedure init(const aRttiType: TRttiType); virtual;
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

    /// <summary>Returns true if the reflected type is HFA.</summary>
    //property IsHFA: Boolean read GetIsHFA;
    /// <summary>Return the element type of HFA, if IsHFA is true. Otherwise, Return NIL. </summary>
    //property HFAElementType: TRttiFloatType read GetHFAElementType;
    /// <summary>Return the number of element type of HFA, if IsHFA is true. Otherwise, Return 0. </summary>
    //property HFAElementCount: Integer read GetHFAElementCount;
  end;

  {*************************************}
  TALRttiOrdinalType = class(TALRttiType)
  private
    fOrdType: TOrdType;
    fMinValue: Integer;
    fMaxValue: Integer;
    //function GetMaxValue: Integer; virtual;
    //function GetMinValue: Integer; virtual;
    //function GetOrdType: TOrdType;
    //constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  protected
    //function GetTypeSize: Integer; override;
    procedure init(const aRttiOrdinalType: TRttiOrdinalType); reintroduce;
  public
    property OrdType: TOrdType read fOrdType;
    property MinValue: Integer read fMinValue;
    property MaxValue: Integer read fMaxValue;
  end;

  {*********************************}
  TALRttiSetType = class(TALRttiType)
  private
    fElementType: TRttiType;
    //function GetElementType: TRttiType;
    //function GetTypeSize: Integer; override;
    //function GetByteOffset: Integer;
    //constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  protected
    procedure init(const aRttiSetType: TRttiSetType); reintroduce;
  public
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

{*}
var
  ALRTTIContext: TRttiContext;
  ALRttiTypeCache: TObjectDictionary<ansiString,TALRttiType>;

implementation

uses
  System.sysutils,
  System.Generics.Defaults,
  AlCommon,
  ALString;

{***}
const
  {$IF CompilerVersion > 34} // sydney
    {$MESSAGE WARN 'Check if System.TypInfo.BooleanIdents is still the same and adjust the IFDEF'}
  {$IFEND}
  ALBooleanIdents: array [Boolean] of AnsiString = ('False', 'True');

{************************}
{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if System.TypInfo.AfterString is still the same and adjust the IFDEF'}
{$IFEND}
// P points a length field of ShortString.
function ALAfterString(const P: PByte): Pointer; inline;
begin
  Result := P + P^ + 1;
end;

{************************}
{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if System.TypInfo.GetEnumName(TypeInfo: PTypeInfo; Value: Integer... is still the same and adjust the IFDEF'}
{$IFEND}
function ALGetEnumName(TypeInfo: PTypeInfo; Value: Integer): ansistring;
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
    Result := ALBooleanIdents[Value <> 0];
    if SameText(HexDisplayPrefix, '0x') then
      Result := ALLowerCase(Result);
  end
  else
  begin
    P := @T^.NameList;
    while Value <> 0 do
    begin
      P := ALAfterString(P);
      Dec(Value);
    end;
    //Result := UTF8IdentToString(PShortString(P));
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

{************************}
{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if System.TypInfo.GetEnumNameValue(TypeInfo: PTypeInfo... is still the same and adjust the IFDEF'}
{$IFEND}
function ALGetEnumNameValue(TypeInfo: PTypeInfo; const Name: AnsiString): Integer;
var
  TypeData: PTypeData;
  LName: PByte;
  I: Integer;
  LLen1: Integer;
  LLen2: Integer;
begin
  TypeData := GetTypeData(GetTypeData(TypeInfo)^.BaseType^);
  LName := PByte(@TypeData^.NameList);
  //if TypeData^.MaxValue >= 4 then
  //begin
    LLen2 := Length(Name);
    for I := 0 to TypeData^.MaxValue do
    begin
      LLen1 := PByte(LName)^; // length is store in First array
      if (LLen1 = LLen2) and
         ALSameText(PShortString(LName)^, Name) then
        Exit(I);
      LName := ALAfterString(LName);
    end;
  //end
  //else
  //begin
  //  for I := 0 to TypeData^.MaxValue do
  //  begin
  //    if ALSameText(PShortString(LName)^, Name) then
  //      Exit(I);
  //    LName := ALAfterString(LName);
  //  end;
  //end;
  //We can not support Alias (IE: alLeft = TAlignLayout.Left) because the global
  //var EnumAliases is private to System.TypInfo
  //Result := GetAliasEnumValue(TypeInfo, Name);
  Result := -1;
end;

{************************}
{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if System.TypInfo.GetEnumValue(TypeInfo: PTypeInfo... is still the same and adjust the IFDEF'}
{$IFEND}
function ALTryGetEnumValue(TypeInfo: PTypeInfo; const Name: ansistring; Var EnumValue: Integer): boolean;
begin
  if (TypeInfo = nil) or (Name = '') or (Length(Name) > 255) then Exit(false);

  if TypeInfo^.Kind = tkInteger then
    Result := ALTryStrToInt(Name, EnumValue)
  else
  begin
    if (TypeInfo^.Kind <> tkEnumeration) then exit(false);
    if GetTypeData(TypeInfo)^.MinValue < 0 then  // Longbool/wordbool/bytebool
    begin
      if ALSameText(Name, ALBooleanIdents[False]) then begin
        EnumValue := 0;
        Result := true;
      end
      else if ALSameText(Name, ALBooleanIdents[True]) then begin
        EnumValue := -1;
        Result := true;
      end
      else
        Result := ALTryStrToInt(Name, EnumValue);
    end
    else begin
      EnumValue := ALGetEnumNameValue(TypeInfo, Name);
      result := EnumValue > -1;
    end;
  end;
end;

{*******************************************************************************************************}
function ALTryGetEnumValue(PropInfo: PPropInfo; const Name: ansistring; Var EnumValue: Integer): boolean;
begin
  result := ALTryGetEnumValue(PropInfo^.PropType^, Name, EnumValue);
end;

{****************************************************************************}
function ALGetEnumValue(TypeInfo: PTypeInfo; const Name: ansistring): Integer;
begin
  if not ALTryGetEnumValue(TypeInfo, Name, result) then
    raise EALException.CreateFmt('Invalid enumeration name: %s', [Name]);
end;

{****************************************************************************}
function ALGetEnumValue(PropInfo: PPropInfo; const Name: ansistring): Integer;
begin
  result := ALGetEnumValue(PropInfo^.PropType^, Name);
end;

{**}
type
  {$IF CompilerVersion > 34} // sydney
    {$MESSAGE WARN 'Check if System.TypInfo.TLargestSet/PLargestSet is still the same and adjust the IFDEF'}
  {$IFEND}
  TLargestSet = set of byte;
  PLargestSet = ^TLargestSet;

{************************}
{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if System.TypInfo.SetToString(TypeInfo: PTypeInfo; Value: Integer... is still the same and adjust the IFDEF'}
{$IFEND}
function ALSetToString(TypeInfo: PTypeInfo; Value: Integer; const Brackets: Boolean = False): ansistring;
var
  ElementType: PPTypeInfo;
  S: TIntegerSet;
  I: Integer;
begin
  Result := '';
  Integer(S) := Value;

  ElementType := GetTypeData(TypeInfo)^.CompType;
  if ElementType <> nil then
  begin
    for I := 0 to SizeOf(Integer) * 8 - 1 do
      if I in S then
      begin
        if Result <> '' then
          Result := Result + ',';
        Result := Result + ALGetEnumName(ElementType^, I);
      end;
  end
  else
  begin
    for I := 0 to SizeOf(Integer) * 8 - 1 do
      if I in S then
      begin
        if Result <> '' then
          Result := Result + ',';
        Result := Result + ALIntToStr(I);
      end;
   end;
  if Brackets then
    Result := '[' + Result + ']';
end;

{************************}
{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if System.TypInfo.SetToString(PropInfo: PPropInfo; Value: Integer... is still the same and adjust the IFDEF'}
{$IFEND}
function ALSetToString(PropInfo: PPropInfo; Value: Integer; const Brackets: Boolean = False): ansistring;
begin
  Result := ALSetToString(PropInfo^.PropType^, Value, Brackets);
end;

{************************}
{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if System.TypInfo.StringToSet(TypeInfo: PTypeInfo; const Value: string)... is still the same and adjust the IFDEF'}
{$IFEND}
function ALTryStringToSet(TypeInfo: PTypeInfo; const Value: ansistring; Var SetInt: Integer): Boolean;
var
  P: PAnsiChar;
  EnumName: AnsiString;
  EnumValue: Integer;
  PEnumInfo: PPTypeInfo;

  // grab the next enum name
  function NextWord(var P: PAnsiChar): AnsiString;
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
  P := PAnsiChar(Value);

  // skip leading bracket and whitespace
  while (P^ in ['[',' ']) do
    Inc(P);

  PEnumInfo := GetTypeData(TypeInfo)^.CompType;
  if PEnumInfo <> nil then
  begin
    EnumName := NextWord(P);
    while EnumName <> '' do
    begin
      If not ALTryGetEnumValue(PEnumInfo^, EnumName, EnumValue) then
        exit(False);
      if EnumValue < 0 then
        exit(False);
      Include(TIntegerSet(SetInt), enumvalue);
      EnumName := NextWord(P);
    end;
  end
  else
  begin
    EnumName := NextWord(P);
    while EnumName <> '' do
    begin
      EnumValue := ALStrToIntDef(EnumName, -1);
      if EnumValue < 0 then
        exit(False);
      Include(TIntegerSet(SetInt), enumvalue);
      EnumName := NextWord(P);
    end;
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
  if not ALTryStringToSet(TypeInfo, Value, result) then
    raise EALException.CreateFmt('Invalid set string: %s', [Value]);
end;

{****************************************************************************}
function ALStringToSet(PropInfo: PPropInfo; const Value: ansistring): Integer;
begin
  Result := ALStringToSet(PropInfo^.PropType^, Value);
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
var LRttiParameters: TArray<TRttiParameter>;
    I: integer;
begin
  inherited create(aRttiMethod);
  fRttiMethod := aRttiMethod;
  LRttiParameters := aRttiMethod.GetParameters;
  setlength(fRttiParameters, length(LRttiParameters));
  for I := Low(LRttiParameters) to High(LRttiParameters) do
    fRttiParameters[I] := TALRttiParameter.Create(LRttiParameters[I]);
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
var I: integer;
begin
  for I := Low(fRttiParameters) to High(fRttiParameters) do fRttiParameters[I].free;
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
    TArray.sort<TALRttiMember>(
      aArray,
      TDelegatedComparer<TALRttiMember>.Construct(function(const Left, Right: TALRttiMember): Integer
      begin
        Result := ALCompareText(Left.Name, Right.Name);
        if result = 0 then result := Left.Order - Right.Order;
      end));
  end;

var I: integer;

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
  for I:=low(aRttiType.GetProperties) to high(aRttiType.GetProperties) do _AddProperty(aRttiType.GetProperties[I], I);
  for I:=low(aRttiType.GetIndexedProperties) to high(aRttiType.GetIndexedProperties) do _AddIndexedProperty(aRttiType.GetIndexedProperties[I],I);
  for I:=low(aRttiType.GetFields) to high(aRttiType.GetFields) do _AddField(aRttiType.GetFields[I],I);
  for I:=low(aRttiType.GetMethods) to high(aRttiType.GetMethods) do _AddMethod(aRttiType.GetMethods[I],I);
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
var I: integer;
begin

  // free properties
  for I := Low(fPrivateProperties) to High(fPrivateProperties) do fPrivateProperties[I].Free;
  for I := Low(fProtectedProperties) to High(fProtectedProperties) do fProtectedProperties[I].Free;
  for I := Low(fPublicProperties) to High(fPublicProperties) do fPublicProperties[I].Free;
  for I := Low(fPublishedProperties) to High(fPublishedProperties) do fPublishedProperties[I].Free;

  // free indexed properties
  for I := Low(fPrivateIndexedProperties) to High(fPrivateIndexedProperties) do fPrivateIndexedProperties[I].Free;
  for I := Low(fProtectedIndexedProperties) to High(fProtectedIndexedProperties) do fProtectedIndexedProperties[I].Free;
  for I := Low(fPublicIndexedProperties) to High(fPublicIndexedProperties) do fPublicIndexedProperties[I].Free;
  for I := Low(fPublishedIndexedProperties) to High(fPublishedIndexedProperties) do fPublishedIndexedProperties[I].Free;

  // free methods
  for I := Low(fPrivateMethods) to High(fPrivateMethods) do fPrivateMethods[I].Free;
  for I := Low(fProtectedMethods) to High(fProtectedMethods) do fProtectedMethods[I].Free;
  for I := Low(fPublicMethods) to High(fPublicMethods) do fPublicMethods[I].Free;
  for I := Low(fPublishedMethods) to High(fPublishedMethods) do fPublishedMethods[I].Free;

  // free fields
  for I := Low(fPrivateFields) to High(fPrivateFields) do fPrivateFields[I].Free;
  for I := Low(fProtectedFields) to High(fProtectedFields) do fProtectedFields[I].Free;
  for I := Low(fPublicFields) to High(fPublicFields) do fPublicFields[I].Free;
  for I := Low(fPublishedFields) to High(fPublishedFields) do fPublishedFields[I].Free;

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
var LRttiFields: TArray<TALRttiField>;
    I: integer;
begin
  LRttiFields := GetFields(aVisibility);
  if Find(TArray<TALRttiNamedObject>(LRttiFields), AName, I) then result := LRttiFields[i]
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
var LRttiIndexedProperties: TArray<TALRttiIndexedProperty>;
    LRttiIndexedProperty: TALRttiIndexedProperty;
    I,J,K: integer;
begin
  if aName = '' then begin
    LRttiIndexedProperties := GetIndexedProperties(aVisibility);
    setlength(result, length(LRttiIndexedProperties));
    i := 0;
    for LRttiIndexedProperty in LRttiIndexedProperties do
      if LRttiIndexedProperty.IsDefault then begin
        result[i] := LRttiIndexedProperty;
        inc(i);
      end;
    setlength(result,i);
  end
  else begin
    LRttiIndexedProperties := GetIndexedProperties(aVisibility);
    if Find(TArray<TALRttiNamedObject>(LRttiIndexedProperties), AName, I) then begin

      J := I - 1;
      while J >= 0 do begin
        if ALSameText(LRttiIndexedProperties[j].Name, AName) then dec(J)
        else break;
      end;

      K := I + 1;
      while K <= length(LRttiIndexedProperties) - 1 do begin
        if ALSameText(LRttiIndexedProperties[K].Name, AName) then inc(K)
        else break;
      end;

      SetLength(Result, K-J-1);
      for I := J+1 to K-1 do
        Result[I-j-1] := LRttiIndexedProperties[I];

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
var LRTTIMethods: TArray<TALRttiMethod>;
    I,J,K: integer;
begin
  LRTTIMethods := GetMethods(aVisibility);
  if Find(TArray<TALRttiNamedObject>(LRTTIMethods), AName, I) then begin

    J := I - 1;
    while J >= 0 do begin
      if ALSameText(LRTTIMethods[j].Name, AName) then dec(J)
      else break;
    end;

    K := I + 1;
    while K <= length(LRTTIMethods) - 1 do begin
      if ALSameText(LRTTIMethods[K].Name, AName) then inc(K)
      else break;
    end;

    SetLength(Result, K-J-1);
    for I := J+1 to K-1 do
      Result[I-j-1] := LRTTIMethods[I];

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
var LRttiProperties: TArray<TALRttiProperty>;
    I: integer;
begin
  LRttiProperties := GetProperties(aVisibility);
  if Find(TArray<TALRttiNamedObject>(LRttiProperties), AName, I) then result := LRttiProperties[i]
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
  if not ALRttiTypeCache.TryGetValue(aQualifiedName, result) then
    raise EALException.CreateFmt('Cannot obtain RTTI informations about the class %s', [aQualifiedName]);
end;

{******************************************************************************}
procedure ALRttiInitialization(const aQualifiedNameToSkip: array of AnsiString);
var LRttiTypes: TArray<TRttiType>;
    LRttiType: TALRttiType;
    LQualifiedName: AnsiString;
    LContinue: Boolean;
    i, j: integer;
begin

  //create ALRTTIContext
  ALRTTIContext := TRttiContext.Create;

  //create ALRttiTypeCache
  ALRttiTypeCache := TObjectDictionary<ansiString,TALRttiType>.create([doOwnsValues]);

  //init aRTTITypes
  LRttiTypes := ALRTTIContext.GetTypes;

  //first loop to create all the node inside _RttiTypeCache
  for I := Low(LRttiTypes) to High(LRttiTypes) do begin
    LContinue := True;
    LQualifiedName := ansiString(LRttiTypes[i].QualifiedName);
    for j := Low(aQualifiedNameToSkip) to High(aQualifiedNameToSkip) do begin
      if ALMatchesMask(LQualifiedName, aQualifiedNameToSkip[j]) then begin
        LContinue := False;
        break;
      end;
    end;
    if LContinue then begin
      if not ALRttiTypeCache.ContainsKey(LQualifiedName) then begin
        if LRttiTypes[i] is TRttiOrdinalType then LRttiType := TALRttiOrdinalType.Create
        else if LRttiTypes[i] is TRttiSetType then LRttiType := TALRttiSetType.Create
        else LRttiType := TALRttiType.Create;
        ALRttiTypeCache.Add(LQualifiedName, LRttiType);
      end
      else raise Exception.Create('Error 3A680CE4-FB3F-4C9C-8717-EA5FFB0BBFF6'); // not possible error
    end;
  end;

  //3rd loop to init all the fRttiType inside each node of ALRttiTypeCache
  for I := Low(LRttiTypes) to High(LRttiTypes) do begin
    if ALRttiTypeCache.TryGetValue(ansiString(LRttiTypes[i].QualifiedName), LRttiType) then
      LRttiType.init(LRttiTypes[i]);
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
  ALRTTIContext.Free;
  ALRttiTypeCache.Free;
End;

end.