(*******************************************************************************
In the constant evolution of software development, we often find ourselves
seeking ways to reduce boilerplate code and enhance the maintainability of our
projects. One such instance where boilerplate can become cumbersome is in the
initialization of class fields. The traditional method involves explicitly
setting each field's value in the constructor, which can be tedious, especially
for classes with numerous fields. Enter TALInit—a feature that allows
automatic initialization of object fields based on their attributes.

#### The Traditional Way ####

In the typical approach, developers manually initialize object fields in the
constructor. Take the following class as an example:

```
    TAutoInitObject = class(TObject)
    public
      CharValue: Char;
      ChildObject: TChildObject;
    public
      constructor Create; virtual;
      destructor Destroy; override;
    End;
```

Here, each field is initialized in the Create constructor:

```
  constructor TAutoInitObject.create(const aOwner: Tform1; const AAutoInit: Boolean);
  begin
    CharValue := 'A';
    ChildObject := TChildObject.create;
    ChildObject.Name := 'AnObject';
    ChildObject.Value := 12.2;
  end;

  destructor TAutoInitObject.Destroy;
  begin
    ALFreeandNil(ChildObject);
    inherited;
  end;
```

While this method offers precise control, it can become tedious for large
classes with numerous fields.

#### The TALInit Way ####

Imagine having a mechanism that not only automates this but is also as fast as
the traditional way - yes, you read that right. TALInit achieves this
remarkable feat.

```
  TAutoInitObject = class(TObject)
  public
    [TALInit('A')]
    CharValue: Char;
    [TALInit('Name:AnObject;Value:12.2')]
    ChildObject: TChildObject;
  End;
```

By using custom attributes, every field within the object can be automatically
initialized based on its corresponding attribute. This eliminates the need for
manually setting each field within the constructor. The above snippet showcases
just how concise and readable object field initialization can become with
TALInit.

#### Performance - A Game Changer: ####

One of the strongest advantages of using TALInit is its performance. When
introducing automation, a natural concern is the overhead that might come with
it. However, TALInit is designed to be as efficient as the traditional way
of initializing fields. This means developers can enjoy the convenience
without having to worry about any hidden costs in execution time.

Learn more at [{alcinoe}/Alcinoe/tree/master/Demos/ALRTTI](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALRTTI)
*******************************************************************************)
unit Alcinoe.RTTI;

interface

{$I Alcinoe.inc}

uses
  System.Rtti,
  System.RTLConsts,
  System.TypInfo,
  System.Generics.Collections;

Type

  {.$define ALRTTIAnsiString}
  {$IF defined(ALRTTIAnsiString)}
  TALRttiString = AnsiString;
  {$ELSE}
  TALRttiString = String;
  {$ENDIF}

  {******************}
  TALRttiType = Class;
  TALRttiOrdinalType = class;
  TALRttiSetType = class;
  TALRttiInstanceType = class;
  TALRttiRecordType = class;

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
    fName: TALRttiString;
    //function GetName: string; virtual; abstract;
  public
    constructor Create(Const aRttiNamedObject: TRttiNamedObject);
    property Name: TALRttiString read fName;
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
    fQualifiedName: TALRttiString;
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
    fBaseType: TALRttiType;
    fAsInstance: TALRttiInstanceType;
    fAsRecord: TALRttiRecordType;
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
    function Find(const FromArray: TArray<TALRttiNamedObject>; const S: TALRttiString; var Index: Integer): Boolean;
    procedure init(const aRttiType: TRttiType); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    //function ToString: string; override;
    property Handle: PTypeInfo read fHandle;
    // QualifiedName is only available on types declared in interface section of units;
    // i.e. IsPublicType is true.
    property QualifiedName: TALRttiString read fQualifiedName;
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
    function GetMethods(const AName: TALRttiString; const aVisibility: TMemberVisibility): TArray<TALRttiMethod>; overload;

    function GetFields(const aVisibility: TMemberVisibility): TArray<TALRttiField>;
    function GetField(const AName: TALRttiString; const aVisibility: TMemberVisibility): TALRttiField;

    function GetProperties(const aVisibility: TMemberVisibility): TArray<TALRttiProperty>;
    function GetProperty(const AName: TALRttiString; const aVisibility: TMemberVisibility): TALRttiProperty; overload;
    function GetProperty(const AIndex: integer; const aVisibility: TMemberVisibility): TALRttiProperty; overload;

    function GetIndexedProperties(const aVisibility: TMemberVisibility): TArray<TALRttiIndexedProperty>; overload;
    function GetIndexedProperties(const AName: TALRttiString; const aVisibility: TMemberVisibility): TArray<TALRttiIndexedProperty>; overload;

    //function GetDeclaredMethods: TArray<TRttiMethod>; virtual;
    //function GetDeclaredProperties: TArray<TRttiProperty>; virtual;
    //function GetDeclaredFields: TArray<TRttiField>; virtual;
    //function GetDeclaredIndexedProperties: TArray<TRttiIndexedProperty>; virtual;

    // The ancestor for types with ancestors.
    property BaseType: TALRttiType read FBaseType;

    property AsInstance: TALRttiInstanceType read fAsInstance;
    property IsInstance: Boolean read fIsInstance;
    property AsOrdinal: TALRttiOrdinalType read fAsOrdinal;
    property IsOrdinal: Boolean read fIsOrdinal;
    property AsRecord: TALRttiRecordType read fAsRecord;
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
    procedure init(const aRttiType: TRttiType); override;
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
    procedure init(const aRttiType: TRttiType); override;
  public
    property ElementType: TRttiType read fElementType;
  end;

  {*************************************************}
  TALRttiStructuredType = class abstract(TALRttiType)
  end;

  {**********************************************}
  TALRttiRecordType = class(TALRttiStructuredType)
  private
    //FMethOfs: PByte;
    {$IF Defined(CPUARM64) or Defined(LINUX64) or Defined(OSX64)}
    //FHFAElementType: TRttiFloatType;
    //FHFAElementCount: Integer;
    //FHFAHasInsufficientTypeInformation: Boolean;
    {$ENDIF CPUARM64 or LINUX64 or OSX64}
    //function GetManagedFields: TArray<TRttiManagedField>;
    {$IF Defined(CPUARM64) or Defined(LINUX64) or Defined(OSX64)}
    //procedure GetHFAElement;
    //function GetIsHFA: Boolean; override;
    //function GetHFAElementType: TRttiFloatType; override;
    //function GetHFAElementCount: Integer; override;
    //constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
    {$ENDIF CPUARM64 or LINUX64 or OSX64}
  protected
    //function GetTypeSize: Integer; override;
    {$IF Defined(CPUARM64) or Defined(LINUX64) or Defined(OSX64)}
    //property HFAHasInsufficientTypeInformation: Boolean read FHFAHasInsufficientTypeInformation;
    {$ENDIF CPUARM64 or LINUX64 or OSX64}
    procedure init(const aRttiType: TRttiType); override;
  public
    //function GetDeclaredFields: TArray<TRttiField>; override;
    //function GetDeclaredMethods: TArray<TRttiMethod>; override;
    //function GetAttributes: TArray<TCustomAttribute>; override;
    //property ManagedFields: TArray<TRttiManagedField> read GetManagedFields;
  end;

  {************************************************}
  TALRttiInstanceType = class(TALRttiStructuredType)
  private
    fMetaclassType: TClass;
    //FProps: TArray<TRttiProperty>;
    //FMeths: TArray<TRttiMethod>;
    //FVirtCount: Word;
    //FIndexedProps: TArray<TRttiIndexedProperty>;
    //FClassTab: PVmtFieldClassTab;
    //FReadPropData: Boolean;
    //FReadMethData: Boolean;
    //procedure ReadPropData;
    //procedure ReadMethData;
    //function GetBaseType: TRttiType; override;
    //function GetBaseTyped: TRttiInstanceType;
    //function GetMetaclassType: TClass;
    //function GetDeclaringUnitName: string;
    //function GetVmtSize: Integer;
    //constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  protected
    procedure init(const aRttiType: TRttiType); override;
  public
    //property BaseType: TRttiInstanceType read GetBaseTyped;
    //property DeclaringUnitName: string read GetDeclaringUnitName;
    property MetaclassType: TClass read fMetaclassType;

    //function GetDeclaredProperties: TArray<TRttiProperty>; override;
    //function GetDeclaredMethods: TArray<TRttiMethod>; override;
    //function GetDeclaredFields: TArray<TRttiField>; override;
    //function GetDeclaredIndexedProperties: TArray<TRttiIndexedProperty>; override;

    //function GetDeclaredImplementedInterfaces: TArray<TRttiInterfaceType>;
    //function GetImplementedInterfaces: TArray<TRttiInterfaceType>;

    // Members declared in this type only.
    //function GetAttributes: TArray<TCustomAttribute>; override;
    //property VmtSize: Integer read GetVmtSize;
  end;

  {****************************************}
  TALInitAttribute = class(TCustomAttribute)
  private
    FParams: TALRttiString;
  public
    constructor Create(const AParams: TALRttiString);
    property Params: TALRttiString read FParams;
  end;

  //This function is used solely to ensure RTTI generation
  //for specific classes. This becomes necessary when the given
  //classes aren't explicitly referenced anywhere in the code.
  //Including them as a parameter here acts as a workaround
  //to ensure that RTTI information is generated for them.
  procedure ALEnsureRTTIgeneration(AClass: TClass);

{*********************************************************************************}
function ALGetEnumNameA(TypeInfo: PTypeInfo; Value: Integer): ansistring; overload;
function ALGetEnumNameA(PropInfo: PPropInfo; Value: Integer): ansistring; inline; overload;
function ALGetEnumNameW(TypeInfo: PTypeInfo; Value: Integer): string; inline; overload;
function ALGetEnumNameW(PropInfo: PPropInfo; Value: Integer): string; inline; overload;
function ALTryGetEnumValue(TypeInfo: PTypeInfo; const Name: ansistring; Var EnumValue: Integer): boolean; overload;
function ALTryGetEnumValue(PropInfo: PPropInfo; const Name: ansistring; Var EnumValue: Integer): boolean; inline; overload;
function ALTryGetEnumValue(TypeInfo: PTypeInfo; const Name: string; Var EnumValue: Integer): boolean; overload;
function ALTryGetEnumValue(PropInfo: PPropInfo; const Name: string; Var EnumValue: Integer): boolean; inline; overload;
function ALGetEnumValue(TypeInfo: PTypeInfo; const Name: ansistring): Integer; overload;
function ALGetEnumValue(PropInfo: PPropInfo; const Name: ansistring): Integer; inline; overload;
function ALGetEnumValue(TypeInfo: PTypeInfo; const Name: string): Integer; overload;
function ALGetEnumValue(PropInfo: PPropInfo; const Name: string): Integer; inline; overload;
function ALSetToStringA(TypeInfo: PTypeInfo; Value: Integer; const Brackets: Boolean = False): ansistring; overload;
function ALSetToStringA(PropInfo: PPropInfo; Value: Integer; const Brackets: Boolean = False): ansistring; inline; overload;
function ALSetToStringW(TypeInfo: PTypeInfo; Value: Integer; const Brackets: Boolean = False): string; inline; overload;
function ALSetToStringW(PropInfo: PPropInfo; Value: Integer; const Brackets: Boolean = False): string; inline; overload;
function ALTryStringToSet(TypeInfo: PTypeInfo; const Value: ansistring; Var SetInt: Integer): Boolean; overload;
function ALTryStringToSet(PropInfo: PPropInfo; const Value: ansistring; Var SetInt: Integer): Boolean; inline; overload;
function ALTryStringToSet(TypeInfo: PTypeInfo; const Value: string; Var SetInt: Integer): Boolean; overload;
function ALTryStringToSet(PropInfo: PPropInfo; const Value: string; Var SetInt: Integer): Boolean; inline; overload;
function ALStringToSet(TypeInfo: PTypeInfo; const Value: ansistring): Integer; overload;
function ALStringToSet(PropInfo: PPropInfo; const Value: ansistring): Integer; inline; overload;
function ALStringToSet(TypeInfo: PTypeInfo; const Value: string): Integer; overload;
function ALStringToSet(PropInfo: PPropInfo; const Value: string): Integer; inline; overload;
function ALGetRttiType(const aQualifiedName: TALRttiString; const aRaiseExceptionIfNotFound: Boolean = True): TALRttiType;
procedure ALRttiInitializeInstance(const AInstance: TObject);
procedure ALRttiFinalizeInstance(const AInstance: TObject);
procedure ALRttiInitialization(
            const aQualifiedNameToInclude: array of TALRttiString; // ['*'] to include everything
            const AQualifiedNameToExclude: array of TALRttiString); overload; // [] to Exclude nothing
procedure ALRttiInitialization; overload;
procedure ALRttiFinalization;

implementation

uses
  System.Masks,
  System.sysutils,
  System.Generics.Defaults,
  System.AnsiStrings,
  System.Diagnostics,
  Alcinoe.Common,
  Alcinoe.StringList,
  Alcinoe.StringUtils;

{*}
var
  ALRTTIContext: TRttiContext;
  ALRttiTypeCache: TObjectDictionary<TALRttiString,TALRttiType>;

{***}
const
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if System.TypInfo.BooleanIdents is still the same and adjust the IFDEF'}
  {$ENDIF}
  ALBooleanIdentsA: array [Boolean] of AnsiString = ('False', 'True');
  ALBooleanIdentsW: array [Boolean] of String = ('False', 'True');

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.TypInfo.AfterString is still the same and adjust the IFDEF'}
{$ENDIF}
// P points a length field of ShortString.
function ALAfterString(const P: PByte): Pointer; inline;
begin
  Result := P + P^ + 1;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.TypInfo.GetEnumName(TypeInfo: PTypeInfo; Value: Integer... is still the same and adjust the IFDEF'}
{$ENDIF}
function ALGetEnumNameA(TypeInfo: PTypeInfo; Value: Integer): ansistring;
var
  P: Pointer;
  T: PTypeData;
  Len: Byte;
begin
  if TypeInfo^.Kind = tkInteger then
  begin
    Result := ALIntToStrA(Value);
    Exit;
  end;
  T := GetTypeData(GetTypeData(TypeInfo)^.BaseType^);
  if (TypeInfo = System.TypeInfo(Boolean)) or (T^.MinValue < 0) then
  begin
    { LongBool/WordBool/ByteBool have MinValue < 0 and arbitrary
      content in Value; Boolean has Value in [0, 1] }
    Result := ALBooleanIdentsA[Value <> 0];
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

{***********************************************************************}
function ALGetEnumNameA(PropInfo: PPropInfo; Value: Integer): ansistring;
begin
  result := ALGetEnumNameA(PropInfo^.PropType^, Value);
end;

{*******************************************************************}
function ALGetEnumNameW(TypeInfo: PTypeInfo; Value: Integer): string;
begin
  result := System.TypInfo.GetEnumName(TypeInfo, Value);
end;

{*******************************************************************}
function ALGetEnumNameW(PropInfo: PPropInfo; Value: Integer): string;
begin
  result := System.TypInfo.GetEnumName(PropInfo^.PropType^, Value);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.TypInfo.GetEnumNameValue(TypeInfo: PTypeInfo... is still the same and adjust the IFDEF'}
{$ENDIF}
function ALGetEnumNameValue(TypeInfo: PTypeInfo; const Name: AnsiString): Integer; overload;
var
  TypeData: PTypeData;
  LName: PByte;
  I: Integer;
  LLen1: Integer;
  LLen2: Integer;
begin
  TypeData := GetTypeData(GetTypeData(TypeInfo)^.BaseType^);
  LName := PByte(@TypeData^.NameList);
  //useless optimization with ansistring as we do LLen2 := Length(Name)
  //instead of LLen2 := UTF8IdentLength(Name)
  //if TypeData^.MaxValue >= 4 then
  //begin
    LLen2 := Length(Name);
    for I := 0 to TypeData^.MaxValue do
    begin
      LLen1 := PByte(LName)^; // length is store in First array
      if (LLen1 = LLen2) and
         ALSameTextA(PShortString(LName)^, Name) then
        Exit(I);
      LName := ALAfterString(LName);
    end;
  //end
  //else
  //begin
  //  for I := 0 to TypeData^.MaxValue do
  //  begin
  //    if ALSameTextA(PShortString(LName)^, Name) then
  //      Exit(I);
  //    LName := ALAfterString(LName);
  //  end;
  //end;
  //We can not support Alias (IE: alLeft = TAlignLayout.Left) because the global
  //var EnumAliases is private to System.TypInfo
  //Result := GetAliasEnumValue(TypeInfo, Name);
  Result := -1;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.TypInfo.GetEnumValue(TypeInfo: PTypeInfo... is still the same and adjust the IFDEF'}
{$ENDIF}
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
      if ALSameTextA(Name, ALBooleanIdentsA[False]) then begin
        EnumValue := 0;
        Result := true;
      end
      else if ALSameTextA(Name, ALBooleanIdentsA[True]) then begin
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

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.TypInfo.GetEnumNameValue(TypeInfo: PTypeInfo... is still the same and adjust the IFDEF'}
{$ENDIF}
function ALGetEnumNameValue(TypeInfo: PTypeInfo; const Name: String): Integer; overload;
var
  TypeData: PTypeData;
  LName: PByte;
  I: Integer;
  LLen1: Integer;
  LLen2: Integer;
begin
  TypeData := GetTypeData(GetTypeData(TypeInfo)^.BaseType^);
  LName := PByte(@TypeData^.NameList);
  if TypeData^.MaxValue >= 4 then
  begin
    LLen2 := UTF8IdentLength(Name);
    for I := 0 to TypeData^.MaxValue do
    begin
      LLen1 := PByte(LName)^;
      if (LLen1 = LLen2) and
         UTF8IdentStringCompare(PShortString(LName), Name) then
        Exit(I);
      LName := ALAfterString(LName);
    end;
  end
  else
  begin
    for I := 0 to TypeData^.MaxValue do
    begin
      if UTF8IdentStringCompare(PShortString(LName), Name) then
        Exit(I);
      LName := ALAfterString(LName);
    end;
  end;
  //We can not support Alias (IE: alLeft = TAlignLayout.Left) because the global
  //var EnumAliases is private to System.TypInfo
  //Result := GetAliasEnumValue(TypeInfo, Name);
  Result := -1;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.TypInfo.GetEnumValue(TypeInfo: PTypeInfo... is still the same and adjust the IFDEF'}
{$ENDIF}
function ALTryGetEnumValue(TypeInfo: PTypeInfo; const Name: string; Var EnumValue: Integer): boolean;
begin
  if (TypeInfo = nil) or (Name = '') or (Length(Name) > 255) then Exit(false);

  if TypeInfo^.Kind = tkInteger then
    Result := ALTryStrToInt(Name, EnumValue)
  else
  begin
    if (TypeInfo^.Kind <> tkEnumeration) then exit(false);
    if GetTypeData(TypeInfo)^.MinValue < 0 then  // Longbool/wordbool/bytebool
    begin
      if ALSameTextW(Name, ALBooleanIdentsW[False]) then begin
        EnumValue := 0;
        Result := true;
      end
      else if ALSameTextW(Name, ALBooleanIdentsW[True]) then begin
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

{***************************************************************************************************}
function ALTryGetEnumValue(PropInfo: PPropInfo; const Name: string; Var EnumValue: Integer): boolean;
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

{************************************************************************}
function ALGetEnumValue(TypeInfo: PTypeInfo; const Name: string): Integer;
begin
  if not ALTryGetEnumValue(TypeInfo, Name, result) then
    raise EALException.CreateFmt('Invalid enumeration name: %s', [Name]);
end;

{************************************************************************}
function ALGetEnumValue(PropInfo: PPropInfo; const Name: string): Integer;
begin
  result := ALGetEnumValue(PropInfo^.PropType^, Name);
end;

{**}
type
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if System.TypInfo.TLargestSet/PLargestSet is still the same and adjust the IFDEF'}
  {$ENDIF}
  TLargestSet = set of byte;
  PLargestSet = ^TLargestSet;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.TypInfo.SetToString(TypeInfo: PTypeInfo; Value: Integer... is still the same and adjust the IFDEF'}
{$ENDIF}
function ALSetToStringA(TypeInfo: PTypeInfo; Value: Integer; const Brackets: Boolean = False): ansistring;
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
        Result := Result + ALGetEnumNameA(ElementType^, I);
      end;
  end
  else
  begin
    for I := 0 to SizeOf(Integer) * 8 - 1 do
      if I in S then
      begin
        if Result <> '' then
          Result := Result + ',';
        Result := Result + ALIntToStrA(I);
      end;
   end;
  if Brackets then
    Result := '[' + Result + ']';
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.TypInfo.SetToString(PropInfo: PPropInfo; Value: Integer... is still the same and adjust the IFDEF'}
{$ENDIF}
function ALSetToStringA(PropInfo: PPropInfo; Value: Integer; const Brackets: Boolean = False): ansistring;
begin
  Result := ALSetToStringA(PropInfo^.PropType^, Value, Brackets);
end;

{****************************************************************************************************}
function ALSetToStringW(TypeInfo: PTypeInfo; Value: Integer; const Brackets: Boolean = False): string;
begin
  System.TypInfo.SetToString(TypeInfo, Value, Brackets);
end;

{****************************************************************************************************}
function ALSetToStringW(PropInfo: PPropInfo; Value: Integer; const Brackets: Boolean = False): string;
begin
  System.TypInfo.SetToString(PropInfo, Value, Brackets);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.TypInfo.StringToSet(TypeInfo: PTypeInfo; const Value: string)... is still the same and adjust the IFDEF'}
{$ENDIF}
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

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if System.TypInfo.StringToSet(TypeInfo: PTypeInfo; const Value: string)... is still the same and adjust the IFDEF'}
{$ENDIF}
{$WARN WIDECHAR_REDUCED OFF}
function ALTryStringToSet(TypeInfo: PTypeInfo; const Value: string; Var SetInt: Integer): Boolean;
var
  P: PChar;
  EnumName: String;
  EnumValue: Integer;
  PEnumInfo: PPTypeInfo;

  // grab the next enum name
  function NextWord(var P: PChar): String;
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
  P := PChar(Value);

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
{$WARN WIDECHAR_REDUCED ON}

{************************************************************************************************}
function ALTryStringToSet(PropInfo: PPropInfo; const Value: string; Var SetInt: Integer): Boolean;
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

{************************************************************************}
function ALStringToSet(TypeInfo: PTypeInfo; const Value: string): Integer;
begin
  if not ALTryStringToSet(TypeInfo, Value, result) then
    raise EALException.CreateFmt('Invalid set string: %s', [Value]);
end;

{************************************************************************}
function ALStringToSet(PropInfo: PPropInfo; const Value: string): Integer;
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
  fName := {$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(aRttiNamedObject.Name);
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
  //ENonPublicType look like :TALFormatSettingsA.:1
  //This will raise an exception in QualifiedName
  //function TRttiType.GetQualifiedName: string;
  //begin
  //  Result := Package.GetNameFromType(Self);
  //  if Result = '' then
  //    raise ENonPublicType.CreateResFmt(@SNonPublicType, [Name]);
  //end;
  if assigned(aRttiField.FieldType) and
     (ALPosA(':',aRttiField.FieldType.Handle.Name) <> 1) then fFieldType := ALGetRttiType({$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(aRttiField.FieldType.QualifiedName), false{aRaiseExceptionIfNotFound})
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
  if assigned(aRttiProperty.PropertyType) then fPropertyType := ALGetRttiType({$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(aRttiProperty.PropertyType.QualifiedName), false{aRaiseExceptionIfNotFound})
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
  if assigned(aRttiParameter.ParamType) then fParamType := ALGetRttiType({$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(aRttiParameter.ParamType.QualifiedName), false{aRaiseExceptionIfNotFound})
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
     assigned(aRttiMethod.ReturnType) then fReturnType := ALGetRttiType({$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(aRttiMethod.ReturnType.QualifiedName), false{aRaiseExceptionIfNotFound})
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

{**********************************************************************************************************}
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
  if assigned(aRTTIIndexedProperty.PropertyType) then fPropertyType := ALGetRttiType({$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(aRTTIIndexedProperty.PropertyType.QualifiedName), false{aRaiseExceptionIfNotFound})
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

{************************************************************}
procedure TALRttiOrdinalType.init(const aRttiType: TRttiType);
begin
  inherited init(aRttiType);
  var LRttiOrdinalType := TRttiOrdinalType(aRttiType);
  fOrdType := LRttiOrdinalType.OrdType;
  fMinValue := LRttiOrdinalType.MinValue;
  fMaxValue := LRttiOrdinalType.MaxValue;
end;

{********************************************************}
procedure TALRttiSetType.init(const aRttiType: TRttiType);
begin
  inherited init(aRttiType);
  var LRttiSetType := TRttiSetType(aRttiType);
  fElementType := LRttiSetType.ElementType;
end;

{***********************************************************}
procedure TALRttiRecordType.init(const aRttiType: TRttiType);
begin
  inherited init(aRttiType);
end;

{*************************************************************}
procedure TALRttiInstanceType.init(const aRttiType: TRttiType);
begin
  inherited init(aRttiType);
  var LRttiInstanceType := TRttiInstanceType(aRttiType);
  fMetaclassType := LRttiInstanceType.MetaclassType;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _sortArray(var aArray: TArray<TALRttiMember>);
  begin
    TArray.sort<TALRttiMember>(
      aArray,
      TDelegatedComparer<TALRttiMember>.Construct(
        function(const Left, Right: TALRttiMember): Integer
        begin
          Result := {$IF defined(ALRTTIAnsiString)}ALCompareTextA{$ELSE}ALCompareTextW{$ENDIF}(Left.Name, Right.Name);
          if result = 0 then result := Left.Order - Right.Order;
        end));
  end;

var I: integer;

begin

  inherited create(aRttiType);
  fQualifiedName := {$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(aRttiType.QualifiedName);
  fHandle := aRttiType.Handle;
  fTypeKind := aRttiType.TypeKind;
  fIsOrdinal := aRttiType.IsOrdinal;
  fIsPublicType := aRttiType.IsPublicType;
  fTypeSize := aRttiType.TypeSize;
  fIsManaged := aRttiType.IsManaged;
  fIsRecord := aRttiType.IsRecord;
  fIsSet := aRttiType.IsSet;
  fIsInstance := aRttiType.IsInstance;
  if IsOrdinal and assigned(aRttiType.AsOrdinal) then fAsOrdinal := TALRttiOrdinalType(ALGetRttiType({$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(aRttiType.AsOrdinal.QualifiedName)))
  else fAsOrdinal := nil;
  if isSet and assigned(aRttiType.AsSet) then fAsSet := TALRttiSetType(ALGetRttiType({$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(aRttiType.AsSet.QualifiedName)))
  else fAsSet := nil;
  if assigned(aRttiType.BaseType) then fBaseType := ALGetRttiType({$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(aRttiType.BaseType.QualifiedName))
  else fBaseType := nil;
  if IsInstance and assigned(aRttiType.AsInstance) then fAsInstance := TALRttiInstanceType(ALGetRttiType({$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(aRttiType.AsInstance.QualifiedName)))
  else fAsInstance := nil;
  if IsRecord and assigned(aRttiType.AsRecord) then fAsRecord := TALRttiRecordType(ALGetRttiType({$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(aRttiType.AsRecord.QualifiedName)))
  else fAsRecord := nil;
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

{**************************************************************************************************************************}
function TALRttiType.Find(const FromArray: TArray<TALRttiNamedObject>; const S: TALRttiString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := length(FromArray) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := {$IF defined(ALRTTIAnsiString)}ALCompareTextA{$ELSE}ALCompareTextW{$ENDIF}(FromArray[I].fName, S);
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

{************************************************************************************************************}
function TALRttiType.GetField(const AName: TALRttiString; const aVisibility: TMemberVisibility): TALRttiField;
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

{******************************************************************************************************************************************}
function TALRttiType.GetIndexedProperties(const AName: TALRttiString; const aVisibility: TMemberVisibility): TArray<TALRttiIndexedProperty>;
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
        if {$IF defined(ALRTTIAnsiString)}ALSameTextA{$ELSE}ALSameTextW{$ENDIF}(LRttiIndexedProperties[j].Name, AName) then dec(J)
        else break;
      end;

      K := I + 1;
      while K <= length(LRttiIndexedProperties) - 1 do begin
        if {$IF defined(ALRTTIAnsiString)}ALSameTextA{$ELSE}ALSameTextW{$ENDIF}(LRttiIndexedProperties[K].Name, AName) then inc(K)
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

{***********************************************************************************************************************}
function TALRttiType.GetMethods(const AName: TALRttiString; const aVisibility: TMemberVisibility): TArray<TALRttiMethod>;
var LRTTIMethods: TArray<TALRttiMethod>;
    I,J,K: integer;
begin
  LRTTIMethods := GetMethods(aVisibility);
  if Find(TArray<TALRttiNamedObject>(LRTTIMethods), AName, I) then begin

    J := I - 1;
    while J >= 0 do begin
      if {$IF defined(ALRTTIAnsiString)}ALSameTextA{$ELSE}ALSameTextW{$ENDIF}(LRTTIMethods[j].Name, AName) then dec(J)
      else break;
    end;

    K := I + 1;
    while K <= length(LRTTIMethods) - 1 do begin
      if {$IF defined(ALRTTIAnsiString)}ALSameTextA{$ELSE}ALSameTextW{$ENDIF}(LRTTIMethods[K].Name, AName) then inc(K)
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

{******************************************************************************************************************}
function TALRttiType.GetProperty(const AName: TALRttiString; const aVisibility: TMemberVisibility): TALRttiProperty;
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

{************************************************************************************************************************}
function ALGetRttiType(const aQualifiedName: TALRttiString; const aRaiseExceptionIfNotFound: Boolean = True): TALRttiType;
begin
  if not ALRttiTypeCache.TryGetValue(aQualifiedName, result) then begin
    Result := nil;
    {$IF defined(debug)}
    ALLog('Cannot obtain RTTI information for the type %s', [aQualifiedName], TALLogType.Warn);
    {$ENDIF}
    if aRaiseExceptionIfNotFound then
      raise EALException.CreateFmt('Cannot obtain RTTI information for the class %s', [aQualifiedName]);
  end;
end;

{****************************************************************}
constructor TALInitAttribute.Create(const AParams: TALRttiString);
begin
  FParams := AParams;
end;

{***********************************************}
procedure ALEnsureRTTIgeneration(AClass: TClass);
begin
end;

type

  {~~~~~~~~~~~~~~~~~~~~~~~}
  TALRTTIInitValue = record
  public
    type
      TValueKind = (vkChar, vkansiChar, vkString, vkansiString, vkInt64, vkInt32, vkInt16, vkInt8, vkSingle, vkDouble, vkDateTime, vkObject, vkRecord, vkMethod, vkClassRef);
  public
    RttiMember: TALRttiMember;
    Order: integer;
    ValueKind: TValueKind;
    CharValue: Char;
    AnsiCharValue: ansiChar;
    StringValue: String;
    AnsiStringValue: ansiString;
    Int64Value: Int64;
    Int32Value: Int32;
    Int16Value: Int16;
    Int8Value: Int8;
    SingleValue: Single;
    DoubleValue: Double;
    DateTimeValue: TDateTime;
    ParamLessCreateMethod: Boolean;
    MethodValue: TALRttiMethod;
    ClassValue: TClass;
    InitValues: Tarray<TALRTTIInitValue>;
  End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALRttiInitValues = Tarray<TALRTTIInitValue>;

var

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // split in ALRttiTypeInitValuesArray/ALRttiTypeInitValuesIndex
  // instead of ALRttiTypeInitValues: TDictionary<PTypeInfo, Tarray<TALRttiInitValues>>;
  // because when we do ALRttiTypeInitValues.trygetvalue(ATypeInfo, LInitValues) it's
  // will copy the memory of the record in the variable LInitValues (records are
  // not object, they are fully copied). don't sure it's change really something
  // on the final speed, it's really a fine fine tuning
  ALRttiTypeInitValuesArray: Tarray<TALRttiInitValues>;
  ALRttiTypeInitValuesIndex: TDictionary<PTypeInfo, Integer>;

{****************************}
function _ALRttiGetSetterCode(
           const AInstance: Pointer;
           const ARttiInstanceProperty: TALRttiInstanceProperty;
           out ASlotField: boolean): pointer;
type
  PIntPtr = ^IntPtr;
begin

  var LSetter := ARttiInstanceProperty.PropInfo^.SetProc;

  if (IntPtr(LSetter) and PROPSLOT_MASK) = PROPSLOT_FIELD then begin
    // Field
    result := PByte(AInstance) + (IntPtr(LSetter) and (not PROPSLOT_MASK));
    ASlotField := True;
    exit;
  end;

  ASlotField := False;
  if (IntPtr(LSetter) and PROPSLOT_MASK) = PROPSLOT_VIRTUAL then begin
    // Virtual dispatch, but with offset, not slot
    result := PPointer(PIntPtr(AInstance)^ + SmallInt(IntPtr(LSetter)))^
  end
  else begin
    // Static dispatch
    result := LSetter;
  end;

  if (result = nil) or (PPointer(result)^ = nil) then
    raise EInsufficientRtti.CreateRes(@SInsufficientRtti);

end;

{****************************}
function _ALRttiGetGetterCode(
           const AInstance: Pointer;
           const ARttiInstanceProperty: TALRttiInstanceProperty;
           out ASlotField: boolean): pointer;
type
  PIntPtr = ^IntPtr;
begin

  var lGetter := ARttiInstanceProperty.PropInfo^.GetProc;

  if (IntPtr(lGetter) and PROPSLOT_MASK) = PROPSLOT_FIELD then begin
    // Field
    Result := PByte(AInstance) + (IntPtr(LGetter) and (not PROPSLOT_MASK));
    ASlotField := True;
    Exit;
  end;

  ASlotField := False;
  if (IntPtr(LGetter) and PROPSLOT_MASK) = PROPSLOT_VIRTUAL then begin
    // Virtual dispatch, but with offset, not slot
    Result := PPointer(PIntPtr(AInstance)^ + SmallInt(IntPtr(LGetter)))^;
  end
  else begin
    // Static dispatch
    Result := LGetter;
  end;

  if (result = nil) or (PPointer(result)^ = nil) then
    raise EInsufficientRtti.CreateRes(@SInsufficientRtti);

end;

{***********************************************************}
procedure ALRttiInitializeInstance(const AInstance: TObject);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoRttiInitializeInstance(const AInstance: Pointer; const ARttiInitValues: TALRttiInitValues);
  type
    //https://stackoverflow.com/questions/76848118/invoking-constructors-via-tmethod-issues-with-hidden-parameters-and-memory-all
    _TMethodPointer = procedure of object;
    _PMethodPointer = ^_TMethodPointer;
    _TParamLessConstructor = function(Alloc: Boolean): TObject of object;
    _TParamSelfConstructor = function(Alloc: Boolean; Owner: TObject): TObject of object;
    _TCharSetterMethod = procedure(Value: Char) of object;
    _TAnsiCharSetterMethod = procedure(Value: ansiChar) of object;
    _TStringSetterMethod = procedure(Value: String) of object;
    _TAnsiStringSetterMethod = procedure(Value: ansiString) of object;
    _TInt64SetterMethod = procedure(Value: Int64) of object;
    _TInt32SetterMethod = procedure(Value: Int32) of object;
    _TInt16SetterMethod = procedure(Value: Int16) of object;
    _TInt8SetterMethod = procedure(Value: Int8) of object;
    _TSingleSetterMethod = procedure(Value: Single) of object;
    _TDoubleSetterMethod = procedure(Value: Double) of object;
    _TDateTimeSetterMethod = procedure(Value: TDateTime) of object;
    _TMethodSetterMethod = procedure(Value: _TMethodPointer) of object;
    _TClassSetterMethod = procedure(Value: TClass) of object;
    _TObjectSetterMethod = procedure(Value: Tobject) of object;
    _TObjectGetterMethod = function: TObject of object;
  begin
    // Important: stay with ARttiInitValues[i] instead of copying the value in a temp variable
    // like LRttiMemberInitValue := ARttiInitValues[i] because we are in record here and doing this
    // mean copy all the memory of ARttiInitValues[i] inside LRttiMemberInitValue
    for var I := high(ARttiInitValues) downto low(ARttiInitValues) do begin
      if ARttiInitValues[i].RttiMember.ClassType = TALRttiField then begin
        var LRttiField := TALRttiField(ARttiInitValues[i].RttiMember);
        case ARttiInitValues[i].ValueKind of
          TALRTTIInitValue.TValueKind.vkChar: PChar(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := ARttiInitValues[i].CharValue;
          TALRTTIInitValue.TValueKind.vkansiChar: PAnsiChar(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := ARttiInitValues[i].ansiCharValue;
          TALRTTIInitValue.TValueKind.vkString: PString(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := ARttiInitValues[i].StringValue;
          TALRTTIInitValue.TValueKind.vkansiString: PAnsiString(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := ARttiInitValues[i].ansiStringValue;
          TALRTTIInitValue.TValueKind.vkInt64: Pint64(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := ARttiInitValues[i].Int64Value;
          TALRTTIInitValue.TValueKind.vkInt32: PInteger(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := ARttiInitValues[i].Int32Value;
          TALRTTIInitValue.TValueKind.vkInt16: PSmallint(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := ARttiInitValues[i].Int16Value;
          TALRTTIInitValue.TValueKind.vkInt8: PShortInt(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := ARttiInitValues[i].Int8Value;
          TALRTTIInitValue.TValueKind.vkSingle: PSingle(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := ARttiInitValues[i].SingleValue;
          TALRTTIInitValue.TValueKind.vkDouble: PDouble(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := ARttiInitValues[i].DoubleValue;
          TALRTTIInitValue.TValueKind.vkDateTime: PDateTime(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := ARttiInitValues[i].DateTimeValue;
          TALRTTIInitValue.TValueKind.vkClassRef: PPointer(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := Pointer(ARttiInitValues[i].ClassValue);
          TALRTTIInitValue.TValueKind.vkObject: begin
            var LObject := TObject(Pointer(NativeInt(AInstance) + LRttiField.Offset)^);
            if LObject = nil then begin
              if ARttiInitValues[i].ParamLessCreateMethod then begin
                //fast Way :
                var LConstructorFunc: _TParamLessConstructor;
                TMethod(LConstructorFunc).Code := ARttiInitValues[i].MethodValue.CodeAddress;
                TMethod(LConstructorFunc).Data := LRttiField.FieldType.AsInstance.MetaclassType;
                Lobject := LConstructorFunc(true);
                //slow way:
                //Lobject := ARttiInitValues[i].MethodValue.Invoke(LRttiField.FieldType.AsInstance.MetaclassType, []).asobject
              end
              else begin
                //fast Way :
                var LConstructorFunc: _TParamSelfConstructor;
                TMethod(LConstructorFunc).Code := ARttiInitValues[i].MethodValue.CodeAddress;
                TMethod(LConstructorFunc).Data := LRttiField.FieldType.AsInstance.MetaclassType;
                Lobject := LConstructorFunc(true, Tobject(AInstance));
                //slow way:
                //Lobject := ARttiInitValues[i].MethodValue.Invoke(LRttiField.FieldType.AsInstance.MetaclassType, [Tobject(AInstance)]).asobject
              end;
              PObject(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := LObject;
            end;
            _DoRttiInitializeInstance(LObject, ARttiInitValues[i].InitValues);
          end;
          TALRTTIInitValue.TValueKind.vkRecord: begin
            _DoRttiInitializeInstance(
              Pointer(NativeInt(AInstance) + LRttiField.Offset),
              ARttiInitValues[i].InitValues);
          end;
          TALRTTIInitValue.TValueKind.vkMethod: Begin
            var LInstance := AInstance;
            for var J := high(ARttiInitValues[i].InitValues) downto low(ARttiInitValues[i].InitValues) do begin
              if ARttiInitValues[i].InitValues[j].RttiMember.ClassType = TALRttiField then begin
                LInstance := TObject(Pointer(NativeInt(LInstance) + TALRttiField(ARttiInitValues[i].InitValues[j].RttiMember).Offset)^)
              end
              else if ARttiInitValues[i].InitValues[j].RttiMember.ClassType = TALRttiInstanceProperty then begin
                var ASlotField: Boolean;
                var LCode := _ALRttiGetGetterCode(LInstance, TALRttiInstanceProperty(ARttiInitValues[i].InitValues[j].RttiMember), ASlotField);
                if ASlotField then LInstance := TObject(LCode^)
                else begin
                  var LGetterMethod: _TObjectGetterMethod;
                  Tmethod(LGetterMethod).Code := LCode;
                  Tmethod(LGetterMethod).Data := AInstance;
                  LInstance := LGetterMethod();
                end;
              end
              else raise Exception.Create('Error 3AC91D01-134D-4092-BAEC-D34417157CDE');
            end;
            var LMethod: Tmethod;
            LMethod.Code := ARttiInitValues[i].MethodValue.CodeAddress;
            LMethod.Data := LInstance;
            _PMethodPointer(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := _TMethodPointer(LMethod);
          End;
          else raise Exception.Create('Error EC910A23-404D-478E-9267-EAA56F30A086');
        end;
      end
      else if ARttiInitValues[i].RttiMember.ClassType = TALRttiInstanceProperty then begin
        var LRttiInstanceProperty := TALRttiInstanceProperty(ARttiInitValues[i].RttiMember);
        case ARttiInitValues[i].ValueKind of
          TALRTTIInitValue.TValueKind.vkChar: begin
            var ASlotField: Boolean;
            var LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then PChar(LCode)^ := ARttiInitValues[i].CharValue
            else begin
              var LSetterMethod: _TCharSetterMethod;
              Tmethod(LSetterMethod).Code := LCode;
              Tmethod(LSetterMethod).Data := AInstance;
              LSetterMethod(ARttiInitValues[i].CharValue);
            end;
          end;
          TALRTTIInitValue.TValueKind.vkansiChar: begin
            var ASlotField: Boolean;
            var LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then PansiChar(LCode)^ := ARttiInitValues[i].ansiCharValue
            else begin
              var LSetterMethod: _TansiCharSetterMethod;
              Tmethod(LSetterMethod).Code := LCode;
              Tmethod(LSetterMethod).Data := AInstance;
              LSetterMethod(ARttiInitValues[i].ansiCharValue);
            end;
          end;
          TALRTTIInitValue.TValueKind.vkString: begin
            var ASlotField: Boolean;
            var LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then PString(LCode)^ := ARttiInitValues[i].StringValue
            else begin
              var LSetterMethod: _TStringSetterMethod;
              Tmethod(LSetterMethod).Code := LCode;
              Tmethod(LSetterMethod).Data := AInstance;
              LSetterMethod(ARttiInitValues[i].StringValue);
            end;
          end;
          TALRTTIInitValue.TValueKind.vkansiString: begin
            var ASlotField: Boolean;
            var LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then PansiString(LCode)^ := ARttiInitValues[i].ansiStringValue
            else begin
              var LSetterMethod: _TansiStringSetterMethod;
              Tmethod(LSetterMethod).Code := LCode;
              Tmethod(LSetterMethod).Data := AInstance;
              LSetterMethod(ARttiInitValues[i].ansiStringValue);
            end;
          end;
          TALRTTIInitValue.TValueKind.vkInt64: begin
            var ASlotField: Boolean;
            var LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then PInt64(LCode)^ := ARttiInitValues[i].Int64Value
            else begin
              var LSetterMethod: _TInt64SetterMethod;
              Tmethod(LSetterMethod).Code := LCode;
              Tmethod(LSetterMethod).Data := AInstance;
              LSetterMethod(ARttiInitValues[i].Int64Value);
            end;
          end;
          TALRTTIInitValue.TValueKind.vkInt32: begin
            var ASlotField: Boolean;
            var LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then PInteger(LCode)^ := ARttiInitValues[i].Int32Value
            else begin
              var LSetterMethod: _TInt32SetterMethod;
              Tmethod(LSetterMethod).Code := LCode;
              Tmethod(LSetterMethod).Data := AInstance;
              LSetterMethod(ARttiInitValues[i].Int32Value);
            end;
          end;
          TALRTTIInitValue.TValueKind.vkInt16: begin
            var ASlotField: Boolean;
            var LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then PSmallint(LCode)^ := ARttiInitValues[i].Int16Value
            else begin
              var LSetterMethod: _TInt16SetterMethod;
              Tmethod(LSetterMethod).Code := LCode;
              Tmethod(LSetterMethod).Data := AInstance;
              LSetterMethod(ARttiInitValues[i].Int16Value);
            end;
          end;
          TALRTTIInitValue.TValueKind.vkInt8: begin
            var ASlotField: Boolean;
            var LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then PShortInt(LCode)^ := ARttiInitValues[i].Int8Value
            else begin
              var LSetterMethod: _TInt8SetterMethod;
              Tmethod(LSetterMethod).Code := LCode;
              Tmethod(LSetterMethod).Data := AInstance;
              LSetterMethod(ARttiInitValues[i].Int8Value);
            end;
          end;
          TALRTTIInitValue.TValueKind.vkSingle: begin
            var ASlotField: Boolean;
            var LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then PSingle(LCode)^ := ARttiInitValues[i].SingleValue
            else begin
              var LSetterMethod: _TSingleSetterMethod;
              Tmethod(LSetterMethod).Code := LCode;
              Tmethod(LSetterMethod).Data := AInstance;
              LSetterMethod(ARttiInitValues[i].SingleValue);
            end;
          end;
          TALRTTIInitValue.TValueKind.vkDouble: begin
            var ASlotField: Boolean;
            var LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then PDouble(LCode)^ := ARttiInitValues[i].DoubleValue
            else begin
              var LSetterMethod: _TDoubleSetterMethod;
              Tmethod(LSetterMethod).Code := LCode;
              Tmethod(LSetterMethod).Data := AInstance;
              LSetterMethod(ARttiInitValues[i].DoubleValue);
            end;
          end;
          TALRTTIInitValue.TValueKind.vkDateTime: begin
            var ASlotField: Boolean;
            var LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then PDateTime(LCode)^ := ARttiInitValues[i].DateTimeValue
            else begin
              var LSetterMethod: _TDateTimeSetterMethod;
              Tmethod(LSetterMethod).Code := LCode;
              Tmethod(LSetterMethod).Data := AInstance;
              LSetterMethod(ARttiInitValues[i].DateTimeValue);
            end;
          end;
          TALRTTIInitValue.TValueKind.vkClassRef: begin
            var ASlotField: Boolean;
            var LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then PPointer(LCode)^ := Pointer(ARttiInitValues[i].ClassValue)
            else begin
              var LSetterMethod: _TClassSetterMethod;
              Tmethod(LSetterMethod).Code := LCode;
              Tmethod(LSetterMethod).Data := AInstance;
              LSetterMethod(ARttiInitValues[i].ClassValue);
            end;
          end;
          TALRTTIInitValue.TValueKind.vkObject: begin
            var LObject: Tobject;
            var ASlotField: Boolean;
            var LCode := _ALRttiGetGetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then LObject := TObject(LCode^)
            else begin
              var LGetterMethod: _TObjectGetterMethod;
              Tmethod(LGetterMethod).Code := LCode;
              Tmethod(LGetterMethod).Data := AInstance;
              LObject := LGetterMethod();
            end;
            if LObject = nil then begin
              if not LRttiInstanceProperty.IsWritable then
                raise Exception.CreateFmt('The property "%s" of "%s" is not writable', [LRttiInstanceProperty.Name, Tobject(AInstance).ClassName]);
              if ARttiInitValues[i].ParamLessCreateMethod then begin
                //fast Way :
                var LConstructorFunc: _TParamLessConstructor;
                TMethod(LConstructorFunc).Code := ARttiInitValues[i].MethodValue.CodeAddress;
                TMethod(LConstructorFunc).Data := LRttiInstanceProperty.PropertyType.AsInstance.MetaclassType;
                Lobject := LConstructorFunc(true);
                //slow way:
                //Lobject := ARttiInitValues[i].MethodValue.Invoke(LRttiField.FieldType.AsInstance.MetaclassType, []).asobject
              end
              else begin
                //fast Way :
                var LConstructorFunc: _TParamSelfConstructor;
                TMethod(LConstructorFunc).Code := ARttiInitValues[i].MethodValue.CodeAddress;
                TMethod(LConstructorFunc).Data := LRttiInstanceProperty.PropertyType.AsInstance.MetaclassType;
                Lobject := LConstructorFunc(true, Tobject(AInstance));
                //slow way:
                //Lobject := ARttiInitValues[i].MethodValue.Invoke(LRttiField.FieldType.AsInstance.MetaclassType, [Tobject(AInstance)]).asobject
              end;
              LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
              if ASlotField then PObject(LCode)^ := Lobject
              else begin
                var LSetterMethod: _TObjectSetterMethod;
                Tmethod(LSetterMethod).Code := LCode;
                Tmethod(LSetterMethod).Data := AInstance;
                LSetterMethod(Lobject);
              end;
            end;
            _DoRttiInitializeInstance(LObject, ARttiInitValues[i].InitValues);
          end;
          TALRTTIInitValue.TValueKind.vkMethod: Begin
            var LInstance := AInstance;
            for var J := high(ARttiInitValues[i].InitValues) downto low(ARttiInitValues[i].InitValues) do begin
              if ARttiInitValues[i].InitValues[j].RttiMember.ClassType = TALRttiField then begin
                LInstance := TObject(Pointer(NativeInt(LInstance) + TALRttiField(ARttiInitValues[i].InitValues[j].RttiMember).Offset)^)
              end
              else if ARttiInitValues[i].InitValues[j].RttiMember.ClassType = TALRttiInstanceProperty then begin
                var ASlotField: Boolean;
                var LCode := _ALRttiGetGetterCode(LInstance, TALRttiInstanceProperty(ARttiInitValues[i].InitValues[j].RttiMember), ASlotField);
                if ASlotField then LInstance := TObject(LCode^)
                else begin
                  var LGetterMethod: _TObjectGetterMethod;
                  Tmethod(LGetterMethod).Code := LCode;
                  Tmethod(LGetterMethod).Data := AInstance;
                  LInstance := LGetterMethod();
                end;
              end
              else raise Exception.Create('Error 3AC91D01-134D-4092-BAEC-D34417157CDE');
            end;
            var LMethod: Tmethod;
            LMethod.Code := ARttiInitValues[i].MethodValue.CodeAddress;
            LMethod.Data := LInstance;
            var ASlotField: Boolean;
            var LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then begin
              _PMethodPointer(LCode)^ := _TMethodPointer(LMethod);
            end
            else begin
              var LSetterMethod: _TMethodSetterMethod;
              Tmethod(LSetterMethod).Code := LCode;
              Tmethod(LSetterMethod).Data := AInstance;
              LSetterMethod(_TMethodPointer(LMethod));
            end;
          End;
          else raise Exception.Create('Error B6D95BBA-3B04-4761-969B-E1A4A84C472C');
        end;
      end
      else
        raise Exception.Create('Error 54FA94BB-AB93-4B52-9B4D-7CEA0010549B');
    end;
  end;

begin
  if ALRttiTypeInitValuesIndex = nil then exit;
  var LIndex: Integer;
  if ALRttiTypeInitValuesIndex.TryGetValue(AInstance.ClassInfo, LIndex) then
    _DoRttiInitializeInstance(AInstance, ALRttiTypeInitValuesArray[LIndex]);
end;

{*********************************************************}
procedure ALRttiFinalizeInstance(const AInstance: TObject);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoRttiFinalizeInstance(const AInstance: Pointer; const ARttiInitValues: TALRttiInitValues);
  type
    _TObjectGetterMethod = function: TObject of object;
    _TObjectSetterMethod = procedure(Value: Tobject) of object;
  begin
    // Important: stay with ARttiInitValues[i] instead of copying the value in a temp variable
    // like LRttiMemberInitValue := ARttiInitValues[i] because we are in record here and doing this
    // mean copy all the memory of ARttiInitValues[i] inside LRttiMemberInitValue
    for var I := low(ARttiInitValues) to high(ARttiInitValues) do begin
      if ARttiInitValues[i].ValueKind = TALRTTIInitValue.TValueKind.vkObject then begin
        if ARttiInitValues[i].RttiMember.ClassType = TALRttiField then begin
          var LRttiField := TALRttiField(ARttiInitValues[i].RttiMember);
          var LObject := TObject(Pointer(NativeInt(AInstance) + LRttiField.Offset)^);
          if LObject <> nil then begin
            Lobject.Free;
            PObject(Pointer(NativeInt(AInstance) + LRttiField.Offset))^ := nil;
          end;
        end
        else if ARttiInitValues[i].RttiMember.ClassType = TALRttiInstanceProperty then begin
          var LRttiInstanceProperty := TALRttiInstanceProperty(ARttiInitValues[i].RttiMember);
          if LRttiInstanceProperty.IsWritable then begin
            var LObject: Tobject;
            var ASlotField: Boolean;
            var LCode := _ALRttiGetGetterCode(AInstance, LRttiInstanceProperty, ASlotField);
            if ASlotField then LObject := TObject(LCode^)
            else begin
              var LGetterMethod: _TObjectGetterMethod;
              Tmethod(LGetterMethod).Code := LCode;
              Tmethod(LGetterMethod).Data := AInstance;
              LObject := LGetterMethod();
            end;
            if LObject <> nil then begin
              LObject.Free;
              LCode := _ALRttiGetSetterCode(AInstance, LRttiInstanceProperty, ASlotField);
                          if ASlotField then PObject(LCode)^ := nil
                          else begin
                            var LSetterMethod: _TObjectSetterMethod;
                            Tmethod(LSetterMethod).Code := LCode;
                            Tmethod(LSetterMethod).Data := AInstance;
                            LSetterMethod(nil);
                          end;
            end;
          end;
        end
        else
          raise Exception.Create('Error EE2EDEA2-454D-433C-B8FE-F9C1EDC67BBC');
      end
      else
        break;
    end;
  end;

begin
  var LIndex: Integer;
  if ALRttiTypeInitValuesIndex.TryGetValue(AInstance.ClassInfo, LIndex) then
    _DoRttiFinalizeInstance(AInstance, ALRttiTypeInitValuesArray[LIndex]);
end;

{*****************************}
procedure ALRttiInitialization(
            const aQualifiedNameToInclude: array of TALRttiString; // ['*'] to include everything
            const AQualifiedNameToExclude: array of TALRttiString); // [] to Exclude nothing

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _IsOfTypeOrDescendant(const AReferenceType: TALRttiType; const ACheckType: TALRttiType): Boolean;
  begin
    Result := False;
    var LCurrentType := ACheckType;
    while Assigned(LCurrentType) do begin
      if LCurrentType.Handle = AReferenceType.Handle then begin
        Result := True;
        Exit;
      end;
      LCurrentType := LCurrentType.BaseType;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _BuildRttiMemberInitValue(
             const AOwnerRttiType: TALRttiType;
             const ARttiMember: TALRttiMember;
             const AOrder: Integer;
             const AType: TALRttiType;
             const AParams: TALRttiString): TALRTTIInitValue;

    {~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _UpdateInitValue;
    begin
      setlength(result.InitValues, 0);
      var LLst := {$IF defined(ALRTTIAnsiString)}TALStringListA{$ELSE}TALStringListW{$ENDIF}.Create;
      try
        LLst.LineBreak := ';';
        LLst.NameValueSeparator := ':';
        LLst.Text := AParams; // position.y:50;position.x:75;width:150
        var i := 0;
        while I <= LLst.Count - 1 do begin
          var LMemberName := ALTrim(LLst.Names[I]); // position.y
          var LValue := ALTrim(LLst.ValueFromIndex[I]); // 50
          inc(i);
          if (LMemberName = '') and (LValue='') then continue;
          //--
          var P := {$IF defined(ALRTTIAnsiString)}ALPosA{$ELSE}ALPosW{$ENDIF}('.', LMemberName);
          if P > 0 then begin
            Lvalue := AlcopyStr(LMemberName, P+1,Maxint) + ':' + LValue; // y:50
            LMemberName := AlcopyStr(LMemberName, 1, P); // position.
            var J := I;
            while J <= LLst.Count - 1 do begin
              var LTmpMemberName := ALTrim(LLst.Names[J]); // position.x
              if {$IF defined(ALRTTIAnsiString)}AlposIgnoreCaseA{$ELSE}AlposIgnoreCaseW{$ENDIF}(LMemberName, LTmpMemberName) = 1 then begin
                var LTmpValue := ALTrim(LLst.ValueFromIndex[J]); // 75
                Lvalue := Lvalue + ';' + AlcopyStr(LTmpMemberName, P+1,Maxint) + ':' + LTmpValue; // y:50;x:75
                LLst.Delete(j);
              end
              else inc(i);
            end;
            delete(LMemberName, length(LMemberName), 1); // position
          end;
          //--
          var LType: TALRttiType;
          var LRttiMember: TALRttiMember := AType.GetField(LMemberName, mvprivate);
          if LRttiMember = nil then begin
            LRttiMember := AType.GetProperty(LMemberName, mvprivate);
            if LRttiMember = nil then Raise exception.Createfmt('The member "%s" could not be found in the class or object "%s"', [LMemberName, AType.QualifiedName]);
            LType := TALRttiProperty(LRttiMember).PropertyType;
          end
          else LType := TALRttiField(LRttiMember).FieldType;
          setlength(result.InitValues, length(result.InitValues) + 1);
          result.InitValues[high(result.InitValues)] := _BuildRttiMemberInitValue(
                                                          AOwnerRttiType, // const AOwnerRttiType: TALRttiType;
                                                          LRttiMember, // const ARttiMember: TALRttiMember;
                                                          AOrder, // const AOrder: Integer;
                                                          LType, // const AType: TALRttiType;
                                                          LValue); // const AParams: String))
        end;
      finally
        ALFreeAndNil(LLst);
      end;
    end;

  begin

    // https://stackoverflow.com/questions/2138266/no-rtti-on-unamed-data-types
    // no RTTI on unamed data types (eg: T: array[0..1])
    // Things like this will not work:
    //   var StrArray : Array of String;
    // But the following will work:
    //   type TStrArray = Array of String;
    //   var StrArray : TStrArray;
    if AType = nil then begin
      if aRttiMember <> nil then raise Exception.CreateFmt('The type of the member "%s" is not supported', [aRttiMember.Name])
      else raise Exception.CreateFmt('The type of "%s" is not supported', [AOwnerRttiType.Name])
    end;

    Result.RttiMember := ARttiMember;
    if AOrder = 0 then Result.Order := ARttiMember.Order
    else Result.Order := AOrder;

    case AType.TypeKind of

      {$region 'tkUnknown'}
      tkUnknown: raise Exception.Createfmt('The type "%s" is not supported', [AType.QualifiedName]);
      {$endregion}

      {$region 'tkInteger'}
      tkInteger: begin
        if (ARttiMember is TALRttiProperty) and
           (not TALRttiProperty(ARttiMember).IsWritable) then
          raise Exception.CreateFmt('The property "%s" of "%s" is not writable', [ARttiMember.Name, AOwnerRttiType.ClassName]);
        Result.ValueKind := TALRTTIInitValue.TValueKind.VkInt32;
        Result.Int32Value := ALStrToInt(AParams);
      end;
      {$endregion}

      {$region 'tkChar'}
      //ansiChar
      tkChar: begin
        if (ARttiMember is TALRttiProperty) and
           (not TALRttiProperty(ARttiMember).IsWritable) then
          raise Exception.CreateFmt('The property "%s" of "%s" is not writable', [ARttiMember.Name, AOwnerRttiType.ClassName]);
        Result.ValueKind := TALRTTIInitValue.TValueKind.VkansiChar;
        var LAnsiParams := AnsiString(AParams);
        if length(LAnsiParams) <> 1 then raise Exception.Createfmt('The provided parameter for the member "%s" is not an ANSI character', [aRttiMember.Name]);
        Result.ansiCharValue := LAnsiParams[low(LAnsiParams)];
      end;
      {$endregion}

      {$region 'tkEnumeration'}
      //boolean
      //Enumeration
      tkEnumeration: begin
        if (ARttiMember is TALRttiProperty) and
           (not TALRttiProperty(ARttiMember).IsWritable) then
          raise Exception.CreateFmt('The property "%s" of "%s" is not writable', [ARttiMember.Name, AOwnerRttiType.ClassName]);
        if AType.TypeSize = 1 then begin
          Result.ValueKind := TALRTTIInitValue.TValueKind.VkInt8;
          Result.Int8Value := ALGetEnumValue(AType.Handle, AParams);
        end
        else if AType.TypeSize = 2 then begin
          Result.ValueKind := TALRTTIInitValue.TValueKind.VkInt16;
          Result.Int16Value := ALGetEnumValue(AType.Handle, AParams);
        end
        else if AType.TypeSize = 4 then begin
          Result.ValueKind := TALRTTIInitValue.TValueKind.Vkint32;
          Result.int32Value := ALGetEnumValue(AType.Handle, AParams);
        end
        else raise Exception.Create('Error 630667B7-49F6-4E15-8A52-5D47538DEE0F');
      end;
      {$endregion}

      {$region 'tkFloat'}
      //Single
      //Double
      //TDateTime
      tkFloat: begin
        if (ARttiMember is TALRttiProperty) and
           (not TALRttiProperty(ARttiMember).IsWritable) then
          raise Exception.CreateFmt('The property "%s" of "%s" is not writable', [ARttiMember.Name, AOwnerRttiType.ClassName]);
        if AType.TypeSize = 4 then begin
          Result.ValueKind := TALRTTIInitValue.TValueKind.VkSingle;
          Result.SingleValue := ALStrToFloat(AParams, {$IF defined(ALRTTIAnsiString)}ALDefaultFormatSettingsA{$ELSE}ALDefaultFormatSettingsW{$ENDIF});
        end
        else if AType.TypeSize = 8 then begin
          if AType.QualifiedName = 'System.TDateTime' then begin
            Result.ValueKind := TALRTTIInitValue.TValueKind.VkDateTime;
            if {$IF defined(ALRTTIAnsiString)}ALSameTextA{$ELSE}ALSameTextW{$ENDIF}(AParams, 'now') then Result.DateTimeValue := Now
            else if {$IF defined(ALRTTIAnsiString)}ALSameTextA{$ELSE}ALSameTextW{$ENDIF}(AParams, 'utcnow') then Result.DateTimeValue := ALUtcNow
            else Result.DateTimeValue := ALStrToDateTime(AParams, {$IF defined(ALRTTIAnsiString)}ALDefaultFormatSettingsA{$ELSE}ALDefaultFormatSettingsW{$ENDIF});
          end
          else begin
            Result.ValueKind := TALRTTIInitValue.TValueKind.VkDouble;
            Result.DoubleValue := ALStrToFloat(AParams, {$IF defined(ALRTTIAnsiString)}ALDefaultFormatSettingsA{$ELSE}ALDefaultFormatSettingsW{$ENDIF});
          end;
        end
        else
          raise Exception.Create('Error BD590935-85B0-4DB5-A3F8-A7B436FBA430');
      end;
      {$endregion}

      {$region 'tkString'}
      //shortString
      //A ShortString in Delphi is a fixed-length string type with a maximum
      //length of 255 characters, where the first byte indicates the string's
      //actual length.
      tkString: raise Exception.Createfmt('The type "%s" is not yet supported', [AType.QualifiedName]);
      {$endregion}

      {$region 'tkSet'}
      tkSet: begin
        if (ARttiMember is TALRttiProperty) and
           (not TALRttiProperty(ARttiMember).IsWritable) then
          raise Exception.CreateFmt('The property "%s" of "%s" is not writable', [ARttiMember.Name, AOwnerRttiType.ClassName]);
        if AType.TypeSize = 1 then begin
          Result.ValueKind := TALRTTIInitValue.TValueKind.Vkint8;
          Result.int8Value := ALStringToSet(AType.Handle, AParams);
        end
        else if AType.TypeSize = 2 then begin
          Result.ValueKind := TALRTTIInitValue.TValueKind.Vkint16;
          Result.int16Value := ALStringToSet(AType.Handle, AParams);
        end
        else if AType.TypeSize = 4 then begin
          Result.ValueKind := TALRTTIInitValue.TValueKind.Vkint32;
          Result.int32Value := ALStringToSet(AType.Handle, AParams);
        end
        else raise Exception.Create('Error A5DBD831-CB5B-4DCD-9220-B05C89F7BA4A');
      end;
      {$endregion}

      {$region 'tkClass'}
      tkClass: begin
        result.ValueKind := TALRTTIInitValue.TValueKind.vkObject;
        //--
        // This method isn't ideal for identifying the correct constructor,
        // especially when dealing with overloaded constructors. Another
        // approach would be to use GetDeclaredMethods, but I'm skipping that
        // for now. For more information, refer to this StackOverflow post:
        // https://stackoverflow.com/questions/76841540/how-to-retrieve-the-correct-constructors-of-a-class-using-rtti-in-delphi
        Result.MethodValue := nil;
        if aRttiMember <> nil then begin
          var LCreateMethods: TArray<TALRttiMethod>;
          if ARttiMember is TALRttiField then LCreateMethods := TALRttiField(ARttiMember).FieldType.GetMethods('Create', mvPublic)
          else if ARttiMember is TALRttiProperty then LCreateMethods := TALRttiProperty(ARttiMember).PropertyType.GetMethods('Create', mvPublic)
          else raise Exception.Create('Error CB4DF528-7794-447B-9610-EEA86DA7CD04');
          for var LCreateMethod in LCreateMethods do begin
            if not LCreateMethod.IsConstructor then continue;
            var LParameters := LCreateMethod.GetParameters;
            if length(LParameters) = 0 then begin
              Result.ParamLessCreateMethod := true;
              Result.MethodValue := LCreateMethod
            end
            else if (length(LParameters) = 1) and
                    (_IsOfTypeOrDescendant(LParameters[high(LParameters)].ParamType, AOwnerRttiType)) then begin
              Result.ParamLessCreateMethod := False;
              Result.MethodValue := LCreateMethod;
            end;
            break;
          end;
        end;
        //--
        _UpdateInitValue;
      end;
      {$endregion}

      {$region 'tkMethod'}
      //procedure of object;
      tkMethod: begin
        if (ARttiMember is TALRttiProperty) and
           (not TALRttiProperty(ARttiMember).IsWritable) then
          raise Exception.CreateFmt('The property "%s" of "%s" is not writable', [ARttiMember.Name, AOwnerRttiType.ClassName]);
        Result.ValueKind := TALRTTIInitValue.TValueKind.VkMethod;
        var LLst := {$IF defined(ALRTTIAnsiString)}TALStringListA{$ELSE}TALStringListW{$ENDIF}.Create;
        try
          LLst.LineBreak := '.';
          LLst.Text := AParams; // OwnerA.OwnerB.Onclick
          if LLst.Count = 0 then raise Exception.Create('The format of the parameter is incorrect');
          var LType: TALRttiType := Atype;
          setlength(result.InitValues, 0);
          For var I := 0 to LLst.count - 2 do begin
            var LMemberName := ALTrim(LLst[I]); // Owner
            if LMemberName = '' then raise Exception.Create('The format of the parameter is incorrect');
            var LRttiMember: TALRttiMember := AOwnerRttiType.GetField(LMemberName, mvprivate);
            if LRttiMember = nil then begin
              LRttiMember := AOwnerRttiType.GetProperty(LMemberName, mvprivate);
              if LRttiMember = nil then Raise exception.Createfmt('The member "%s" could not be found in the class or object "%s"', [LMemberName, AType.QualifiedName]);
              LType := TALRttiProperty(LRttiMember).PropertyType;
            end
            else LType := TALRttiField(LRttiMember).FieldType;
            setlength(result.InitValues, length(result.InitValues) + 1);
            var LDumpInitValue: TALRTTIInitValue;
            LDumpInitValue.RttiMember := LRttiMember;
            result.InitValues[high(result.InitValues)] := LDumpInitValue;
          end;
          var LMethodName := ALTrim(LLst[LLst.Count - 1]);
          if LMethodName = '' then
            raise Exception.Create('The format of the parameter is incorrect');
          var LMethods := LType.GetMethods(LMethodName, mvprivate);
          if length(LMethods) <> 1 then raise Exception.CreateFmt('Could not find the method "%s" in the class or object "%s"',[LMethodName, LType.QualifiedName]);
          Result.MethodValue := LMethods[low(LMethods)];
        finally
          ALFreeAndNil(LLst);
        end;
      end;
      {$endregion}

      {$region 'tkWChar'}
      //Char
      tkWChar: begin
        if (ARttiMember is TALRttiProperty) and
           (not TALRttiProperty(ARttiMember).IsWritable) then
          raise Exception.CreateFmt('The property "%s" of "%s" is not writable', [ARttiMember.Name, AOwnerRttiType.ClassName]);
        Result.ValueKind := TALRTTIInitValue.TValueKind.VkChar;
        if length(AParams) <> 1 then raise Exception.Createfmt('The provided parameter for the member "%s" is not a character', [aRttiMember.Name]);
        Result.CharValue := {$IF defined(ALRTTIAnsiString)}Char{$ENDIF}(AParams[low(AParams)]);
      end;
      {$endregion}

      {$region 'tkLString'}
      //AnsiString
      tkLString: begin
        if (ARttiMember is TALRttiProperty) and
           (not TALRttiProperty(ARttiMember).IsWritable) then
          raise Exception.CreateFmt('The property "%s" of "%s" is not writable', [ARttiMember.Name, AOwnerRttiType.ClassName]);
        Result.ValueKind := TALRTTIInitValue.TValueKind.VkAnsiString;
        Result.AnsiStringValue := AnsiString(AParams);
      end;
      {$endregion}

      {$region 'tkWString'}
      //WideString
      tkWString: raise Exception.Createfmt('The type "%s" is not yet supported', [AType.QualifiedName]);
      {$endregion}

      {$region 'tkVariant'}
      tkVariant: raise Exception.Createfmt('The type "%s" is not yet supported', [AType.QualifiedName]);
      {$endregion}

      {$region 'tkArray'}
      tkArray: raise Exception.Createfmt('The type "%s" is not yet supported', [AType.QualifiedName]);
      {$endregion}

      {$region 'tkRecord'}
      //Non-managed record
      tkRecord: begin
        if ARttiMember is TALRttiProperty then
          raise Exception.Create('Properties of type "record" cannot be automatically initialized');
        result.ValueKind := TALRTTIInitValue.TValueKind.vkRecord;
        _UpdateInitValue;
      end;
      {$endregion}

      {$region 'tkInterface'}
      tkInterface: raise Exception.Createfmt('The type "%s" is not yet supported', [AType.QualifiedName]);
      {$endregion}

      {$region 'tkInt64'}
      tkInt64: begin
        if (ARttiMember is TALRttiProperty) and
           (not TALRttiProperty(ARttiMember).IsWritable) then
          raise Exception.CreateFmt('The property "%s" of "%s" is not writable', [ARttiMember.Name, AOwnerRttiType.ClassName]);
        Result.ValueKind := TALRTTIInitValue.TValueKind.VkInt64;
        Result.Int64Value := ALStrToInt64(AParams);
      end;
      {$endregion}

      {$region 'tkDynArray'}
      tkDynArray: raise Exception.Createfmt('The type "%s" is not yet supported', [AType.QualifiedName]);
      {$endregion}

      {$region 'tkUString'}
      //String
      tkUString: begin
        if (ARttiMember is TALRttiProperty) and
           (not TALRttiProperty(ARttiMember).IsWritable) then
          raise Exception.CreateFmt('The property "%s" of "%s" is not writable', [ARttiMember.Name, AOwnerRttiType.ClassName]);
        Result.ValueKind := TALRTTIInitValue.TValueKind.VkString;
        Result.StringValue := {$IF defined(ALRTTIAnsiString)}String{$ENDIF}(AParams);
      end;
      {$endregion}

      {$region 'tkClassRef'}
      tkClassRef: begin
        if (ARttiMember is TALRttiProperty) and
           (not TALRttiProperty(ARttiMember).IsWritable) then
          raise Exception.CreateFmt('The property "%s" of "%s" is not writable', [ARttiMember.Name, AOwnerRttiType.ClassName]);
        var LRttiType := ALGetRttiType(AParams);
        if not (LRttiType is TALRttiInstanceType) then
          raise Exception.CreateFmt('The RTTI type for "%s" is not recognized as an instance type', [{$IF defined(ALRTTIAnsiString)}String{$ENDIF}(AParams)]);
        Result.ValueKind := TALRTTIInitValue.TValueKind.VkClassRef;
        Result.ClassValue := TALRttiInstanceType(LRttiType).MetaclassType;
      end;
      {$endregion}

      {$region 'tkPointer'}
      tkPointer: raise Exception.Createfmt('The type "%s" is not yet supported', [AType.QualifiedName]);
      {$endregion}

      {$region 'tkProcedure'}
      tkProcedure: raise Exception.Createfmt('The type "%s" is not yet supported', [AType.QualifiedName]);
      {$endregion}

      {$region 'tkMRecord'}
      //Managed record
      //A managed record in Delphi (introduced in 10.4) is a record type that
      //includes custom management operators for initialization, finalization,
      //and assignment, allowing fine-grained control over its lifecycle.
      tkMRecord: begin
        if ARttiMember is TALRttiProperty then
          raise Exception.Create('Properties of type "record" cannot be automatically initialized');
        result.ValueKind := TALRTTIInitValue.TValueKind.vkRecord;
        _UpdateInitValue;
      end;
      {$endregion}

      {$region 'Impossible'}
      else
        raise Exception.Create('Error 97DF82F8-EA89-4873-915A-FCEECDA7904E');
      {$endregion}

    end;

  end;


  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateRttiInitValues(
              var aRttiInitValues: TALRttiInitValues;
              const AOwnerRttiType: TALRttiType;
              const aRttiMember: TALRttiMember;
              const AOrder: integer;
              const AType: TALRttiType);
  begin

    //we ask for the member
    if aRttiMember <> nil then begin

      // loop on all the attributes
      for var LAttribute in aRttiMember.GetAttributes do begin

        // Their is an TALInitAttribute attribute
        if LAttribute is TALInitAttribute then begin

          var LRTTIInitValue := _BuildRttiMemberInitValue(
                                  AOwnerRttiType, // const AOwnerRttiType: TALRttiType;
                                  ARttiMember, // const ARttiMember: TALRttiMember;
                                  AOrder, // const AOrder: integer;
                                  AType, // const AType: TALRttiType;
                                  TALInitAttribute(LAttribute).Params); // const AParams: String)

          setlength(aRttiInitValues, length(aRttiInitValues) + 1);
          aRttiInitValues[high(aRttiInitValues)] := LRTTIInitValue;

        end;

      end;

    end

    //we ask for the AOwnerRttiType
    else begin

      // loop on all the attributes
      for var LAttribute in AOwnerRttiType.GetAttributes do begin

        // Their is an TALInitAttribute attribute
        if LAttribute is TALInitAttribute then begin

          var LRTTIInitValue := _BuildRttiMemberInitValue(
                                  AOwnerRttiType, // const AOwnerRttiType: TALRttiType;
                                  ARttiMember, // const ARttiMember: TALRttiMember;
                                  AOrder, // const AOrder: integer;
                                  AType, // const AType: TALRttiType;
                                  TALInitAttribute(LAttribute).Params); // const AParams: String)

          var LIdx := high(aRttiInitValues) + 1;
          setlength(aRttiInitValues, length(aRttiInitValues) + length(LRTTIInitValue.InitValues));
          for var I := Low(LRTTIInitValue.InitValues) to High(LRTTIInitValue.InitValues) do
            aRttiInitValues[LIdx + I] := LRTTIInitValue.InitValues[I];

        end;

      end;

    end;

  end;

begin

  {$IF defined(debug)}
  var LStopWatch := TstopWatch.startNew;
  {$endif}

  //create ALRTTIContext
  ALRTTIContext := TRttiContext.Create;

  //create ALRttiTypeCache
  ALRttiTypeCache := TObjectDictionary<TALRttiString,TALRttiType>.create([doOwnsValues]);

  //init aRTTITypes
  var LRttiTypes := ALRTTIContext.GetTypes;

  //first loop to create all the node inside _RttiTypeCache
  Var LQualifiedNameToIncludeExactMath := {$IF defined(ALRTTIAnsiString)}TALStringListA{$ELSE}TALStringListW{$ENDIF}.Create;
  Var LQualifiedNameToIncludePartialMath := {$IF defined(ALRTTIAnsiString)}TALStringListA{$ELSE}TALStringListW{$ENDIF}.Create;
  Var LQualifiedNameToExcludeExactMath := {$IF defined(ALRTTIAnsiString)}TALStringListA{$ELSE}TALStringListW{$ENDIF}.Create;
  Var LQualifiedNameToExcludePartialMath := {$IF defined(ALRTTIAnsiString)}TALStringListA{$ELSE}TALStringListW{$ENDIF}.Create;
  try

    //init LQualifiedNameToIncludePartialMath/LQualifiedNameToExcludePartialMath
    var LIncludeEverything := False;
    for var LQualifiedName in aQualifiedNameToInclude do begin
      if LQualifiedName = '*' then begin
        LIncludeEverything := True;
        break;
      end
      else if {$IF defined(ALRTTIAnsiString)}AlposA{$ELSE}AlposW{$ENDIF}('*',LQualifiedName) <= 0 then LQualifiedNameToIncludeExactMath.Add(LQualifiedName)
      else LQualifiedNameToIncludePartialMath.Add(LQualifiedName);
    end;
    LQualifiedNameToIncludeExactMath.Sorted := True;
    //--
    for var LQualifiedName in aQualifiedNameToExclude do begin
      if {$IF defined(ALRTTIAnsiString)}AlposA{$ELSE}AlposW{$ENDIF}('*',LQualifiedName) <= 0 then LQualifiedNameToExcludeExactMath.Add(LQualifiedName)
      else LQualifiedNameToExcludePartialMath.Add(LQualifiedName);
    end;
    LQualifiedNameToExcludeExactMath.Sorted := True;

    //loop on all LRttiTypes
    for var I := Low(LRttiTypes) to High(LRttiTypes) do begin
      var LQualifiedName := {$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(LRttiTypes[i].QualifiedName);
      if LQualifiedNameToExcludeExactMath.IndexOf(LQualifiedName) >= 0 then continue;
      var LMatch := False;
      for var j := 0 to LQualifiedNameToExcludePartialMath.Count -1 do begin
        if {$IF defined(ALRTTIAnsiString)}ALMatchesMaskA{$ELSE}ALMatchesMaskW{$ENDIF}(LQualifiedName, LQualifiedNameToExcludePartialMath[j]) then begin
          LMatch := True;
          break;
        end;
      end;
      if LMatch then continue;
      //--
      if not LIncludeEverything then begin
        if LQualifiedNameToIncludeExactMath.IndexOf(LQualifiedName) < 0 then begin
          LMatch := False;
          for var j := 0 to LQualifiedNameToIncludePartialMath.Count -1 do begin
            if {$IF defined(ALRTTIAnsiString)}ALMatchesMaskA{$ELSE}ALMatchesMaskW{$ENDIF}(LQualifiedName, LQualifiedNameToIncludePartialMath[j]) then begin
              LMatch := True;
              break;
            end;
          end;
          if not LMatch then continue;
        end;
      end;
      //--
      if not ALRttiTypeCache.ContainsKey(LQualifiedName) then begin
        var LRttiType: TALRttiType;
        if LRttiTypes[i] is TRttiOrdinalType then LRttiType := TALRttiOrdinalType.Create
        else if LRttiTypes[i] is TRttiSetType then LRttiType := TALRttiSetType.Create
        else if LRttiTypes[i] is TRttiInstanceType then LRttiType := TALRttiInstanceType.Create
        else if LRttiTypes[i] is TRttiRecordType then LRttiType := TALRttiRecordType.Create
        else LRttiType := TALRttiType.Create;
        ALRttiTypeCache.Add(LQualifiedName, LRttiType);
      end
      else raise Exception.Create('Error 3A680CE4-FB3F-4C9C-8717-EA5FFB0BBFF6'); // not possible error
    end;

  finally
    AlfreeAndNil(LQualifiedNameToIncludeExactMath);
    AlfreeAndNil(LQualifiedNameToIncludePartialMath);
    AlfreeAndNil(LQualifiedNameToExcludeExactMath);
    AlfreeAndNil(LQualifiedNameToExcludePartialMath);
  end;

  //3rd loop to init all the fRttiType inside each node of ALRttiTypeCache
  for var I := Low(LRttiTypes) to High(LRttiTypes) do begin
    var LRttiType: TALRttiType;
    if ALRttiTypeCache.TryGetValue({$IF defined(ALRTTIAnsiString)}AnsiString{$ENDIF}(LRttiTypes[i].QualifiedName), LRttiType) then
      LRttiType.init(LRttiTypes[i]);
  end;

  //now init ALRttiTypeInitValues
  setlength(ALRttiTypeInitValuesArray, 0);
  ALRttiTypeInitValuesIndex := TDictionary<PTypeInfo, Integer>.Create;
  for var LRttiType in ALRttiTypeCache.Values do begin
    if (assigned(LRttiType.Handle)) and
       (LRttiType.Handle^.Kind = tkClass) then begin

      // init LRttiInitValues
      var LRttiInitValues: TALRttiInitValues;
      setlength(LRttiInitValues, 0);

      // loop on all Fields
      for var LRttiField in LRTTIType.GetFields(mvPrivate) do
        _UpdateRttiInitValues(LRttiInitValues, LRttiType, LRttiField, 0, LRttiField.fieldType);

      // loop on all properties
      for var LRttiProperty in LRTTIType.GetProperties(mvPrivate) do
        _UpdateRttiInitValues(LRttiInitValues, LRttiType, LRttiProperty, 0, LRttiProperty.PropertyType);

      // add the LRttiType also
      var LBaseRttiType := LRttiType;
      var LOrder: Integer := -100000; // Using -100,000 as an arbitrary large number for this purpose.
      while LBaseRttiType <> nil do begin
        _UpdateRttiInitValues(LRttiInitValues, LBaseRttiType, nil, LOrder, LBaseRttiType);
        LBaseRttiType := LBaseRttiType.BaseType;
        inc(LOrder);
      end;

      // Order ALRttiTypeInitValues
      // 1) On the top add all VKObject this is need by the ALRttiFinalizeInstance
      // 2) Order all items by it's order value, that mean from the most
      //    derived to least derived
      TArray.Sort<TALRttiInitValue>(
        LRttiInitValues,
        TComparer<TALRttiInitValue>.Construct(
          function(const Left, Right: TALRttiInitValue): Integer
          begin
            if Left.ValueKind = TALRTTIInitValue.TValueKind.vkObject then
            begin
              if Right.ValueKind = TALRTTIInitValue.TValueKind.vkObject then
                Result := Left.Order - Right.Order
              else
                Result := -1;  // left should appear before right
            end
            else
            begin
              if Right.ValueKind = TALRTTIInitValue.TValueKind.vkObject then
                Result := 1   // left should appear after right
              else
                Result := Left.Order - Right.Order;
            end;
          end));

      // update ALRttiTypeInitValues
      if length(LRttiInitValues) > 0 then begin
        setlength(ALRttiTypeInitValuesArray, length(ALRttiTypeInitValuesArray) + 1);
        ALRttiTypeInitValuesArray[high(ALRttiTypeInitValuesArray)] := LRttiInitValues;
        ALRttiTypeInitValuesIndex.Add(LRTTIType.Handle, high(ALRttiTypeInitValuesArray));
      end;

    end;
  end;

  //debug info
  {$IF defined(debug)}
  LStopWatch.stop;
  ALLog(
    'ALRttiInitialization',
    'TimeTaken: ' + ALFormatfloatW('0.##', LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW) + ' ms' + ' | '+
    'Total RttiType Cached: ' + ALinttostrW(ALRttiTypeCache.Count));
  var LLst := TALStringListW.Create;
  try
    for var LRttiType in ALRttiTypeCache.Values do
      LLst.Add({$IF defined(ALRTTIAnsiString)}String{$ENDIF}(LRttiType.QualifiedName));
    LLst.Sorted := True;
    for var I := 0 to LLst.Count - 1 do
      ALLog('RttiType Cached', LLst[i]);
  finally
    ALFreeAndNil(LLst);
  end;
  {$endif}

end;

{*****************************}
procedure ALRttiInitialization;
begin
  ALRttiInitialization(['*']{aQualifiedNameToInclude}, []{AQualifiedNameToExclude});
end;

{***************************}
procedure ALRttiFinalization;
Begin
  ALFreeAndNil(ALRttiTypeInitValuesIndex);
  ALFreeAndNil(ALRttiTypeCache);
  ALRTTIContext.free;
End;

initialization
  ALRttiTypeInitValuesIndex := nil;
  ALRttiTypeCache := nil;

end.
