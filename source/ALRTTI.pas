{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
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

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALRtti;

interface

uses System.Rtti,
     System.RTLConsts,
     System.TypInfo,
     AlAvlBinaryTree;

Type

  {****************************}
  TALRttiObject = class(TObject)
  private
    fAttributes: TArray<TCustomAttribute>;
    //FHandle: Pointer;
    //FRttiDataSize: Integer;
    //[Weak] FPackage: TRttiPackage;
    //[Weak] FParent: TRttiObject;
    //FAttributeGetter: TFunc<TArray<TCustomAttribute>>;
  private
    // TRttiObject descendants should only be retrieved via an RTTI context.
    //constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); virtual;
  public
    //destructor Destroy; override;
    // The starting address of the raw data in the executable image for this RTTI.
    //property Handle: Pointer read FHandle;
    // Size of data pointed to by Handle.
    //property RttiDataSize: Integer read FRttiDataSize;
    //property Parent: TRttiObject read FParent;
    //property Package: TRttiPackage read FPackage;
    function GetAttributes: TArray<TCustomAttribute>; virtual;
  end;

  {***************************************}
  TALRttiNamedObject = class(TALRttiObject)
  private
    fName: string;
    //function GetName: string; virtual; abstract;
  public
    property Name: string read fName; // property Name: string read GetName;
  end;

  {***************************************}
  TALRttiMember = class(TALRttiNamedObject)
  private
    //function GetParent: TRttiType;
    //function GetVisibility: TMemberVisibility; virtual;
  public
    //property Parent: TRttiType read GetParent;
    //property Visibility: TMemberVisibility read GetVisibility;
  end;

  {************************************}
  TALRttiProperty = class(TALRttiMember)
  private
    fRttiProperty: TRttiProperty;
    fPropertyType: TRttiType;
    //function GetPropertyType: TRttiType; virtual; abstract;
    function GetIsReadable: Boolean; virtual; abstract;
    function GetIsWritable: Boolean; virtual; abstract;
    function DoGetValue(Instance: Pointer): TValue; virtual; abstract;
    procedure DoSetValue(Instance: Pointer; const AValue: TValue); virtual; abstract;
  public
    property PropertyType: TRttiType read fPropertyType;  // property PropertyType: TRttiType read GetPropertyType;
    function GetValue(Instance: Pointer): TValue;
    procedure SetValue(Instance: Pointer; const AValue: TValue);
    property IsReadable: Boolean read GetIsReadable;
    property IsWritable: Boolean read GetIsWritable;
  end;

  {**********************************************}
  TALRttiInstanceProperty = class(TALRttiProperty)
  private
    function GetDefault: Integer; virtual;
    function GetIndex: Integer; virtual;
    function GetNameIndex: Smallint; virtual;
    //function GetPropertyType: TRttiType; override;
    function GetPropInfo: PPropInfo; virtual; // abstract;
    //function GetName: string; override;
    function GetIsReadable: Boolean; override;
    function GetIsWritable: Boolean; override;
    function DoGetValue(Instance: Pointer): TValue; override;
    procedure DoSetValue(Instance: Pointer; const AValue: TValue); override;
  public
    constructor Create(const aRttiProperty: TRttiProperty); Virtual;
    function ToString: string; override;
    //property PropertyType: TRttiType read GetPropertyType;
    property Index: Integer read GetIndex;
    property Default: Integer read GetDefault;
    property NameIndex: Smallint read GetNameIndex;
    property PropInfo: PPropInfo read GetPropInfo;
  end;

  {*************************************}
  TALRttiType = class(TALRttiNamedObject)
  private
    fPrivateIndexedProperties: TArray<TRttiIndexedProperty>;
    fPrivateProperties: TArray<TALRttiProperty>;
    fPrivateFields: TArray<TRttiField>;
    fPrivateMethods: TArray<TRttiMethod>;

    fProtectedIndexedProperties: TArray<TRttiIndexedProperty>;
    fProtectedProperties: TArray<TALRttiProperty>;
    fProtectedFields: TArray<TRttiField>;
    fProtectedMethods: TArray<TRttiMethod>;

    fPublicIndexedProperties: TArray<TRttiIndexedProperty>;
    fPublicProperties: TArray<TALRttiProperty>;
    fPublicFields: TArray<TRttiField>;
    fPublicMethods: TArray<TRttiMethod>;

    fPublishedIndexedProperties: TArray<TRttiIndexedProperty>;
    fPublishedProperties: TArray<TALRttiProperty>;
    fPublishedFields: TArray<TRttiField>;
    fPublishedMethods: TArray<TRttiMethod>;
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
    //function GetQualifiedName: string;
    //function GetBaseType: TRttiType; virtual;
    //function GetIsPublicType: Boolean;
    //property TypeData: PTypeData read GetTypeData;
    //constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); override;
  protected
  public
    constructor Create(const aRttiType: TRttiType); Virtual;
    destructor Destroy; override;

    //function ToString: string; override;
    //property Handle: PTypeInfo read GetHandle;
    // QualifiedName is only available on types declared in interface section of units;
    // i.e. IsPublicType is true.
    //property QualifiedName: string read GetQualifiedName;
    //property IsPublicType: Boolean read GetIsPublicType;
    //property TypeKind: TTypeKind read GetTypeKind;
    // The size of a location (variable) of this type.
    //property TypeSize: Integer read GetTypeSize;
    //property IsManaged: Boolean read GetIsManaged;

    // To make writing query code easier, hoist some methods from descendants
    // into this type. These return elements flattened across the type hierarchy
    // in order from most derived to least derived.
    //function GetMethods: TArray<TRttiMethod>; overload; virtual;
    function GetMethods(const aVisibility: TMemberVisibility): TArray<TRttiMethod>; overload;
    //function GetFields: TArray<TRttiField>; virtual;
    function GetFields(const aVisibility: TMemberVisibility): TArray<TRttiField>;
    //function GetProperties: TArray<TRttiProperty>; virtual;
    function GetProperties(const aVisibility: TMemberVisibility): TArray<TALRttiProperty>;
    //function GetIndexedProperties: TArray<TRttiIndexedProperty>; virtual;
    function GetIndexedProperties(const aVisibility: TMemberVisibility): TArray<TRttiIndexedProperty>;

    //function GetMethod(const AName: string): TRttiMethod; virtual;
    //function GetMethods(const AName: string): TArray<TRttiMethod>; overload; virtual;
    function GetMethods(const AName: string; const aVisibility: TMemberVisibility): TArray<TRttiMethod>; overload;
    //function GetField(const AName: string): TRttiField; virtual;
    function GetField(const AName: string; const aVisibility: TMemberVisibility): TRttiField;
    //function GetProperty(const AName: string): TRttiProperty; virtual;
    function GetProperty(const AName: string; const aVisibility: TMemberVisibility): TALRttiProperty; overload;
    function GetProperty(const AIndex: integer; const aVisibility: TMemberVisibility): TALRttiProperty; overload;
    //function GetIndexedProperty(const AName: string): TRttiIndexedProperty; virtual;

    //function GetDeclaredMethods: TArray<TRttiMethod>; virtual;
    //function GetDeclaredProperties: TArray<TRttiProperty>; virtual;
    //function GetDeclaredFields: TArray<TRttiField>; virtual;
    //function GetDeclaredIndexedProperties: TArray<TRttiIndexedProperty>; virtual;

    // The ancestor for types with ancestors.
    //property BaseType: TRttiType read GetBaseType;

    //property AsInstance: TRttiInstanceType read GetAsInstance;
    //property IsInstance: Boolean read GetIsInstance;
    //property AsOrdinal: TRttiOrdinalType read GetAsOrdinal;
    //property IsOrdinal: Boolean read GetIsOrdinal;
    //property AsRecord: TRttiRecordType read GetAsRecord;
    //property IsRecord: Boolean read GetIsRecord;
    //property IsSet: Boolean read GetIsSet;
    //property AsSet: TRttiSetType read GetAsSet;
  End;

{**************************************************************************}
function ALGetRttiType(const aClassName: AnsiString): TALRttiType; overload;
function ALGetRttiType(const aTypeInfo: PTypeInfo): TALRttiType; overload;
procedure ALRttiInitialization;
procedure ALRttiFinalization;

{*******************************}
var vALRTTIContext: TRttiContext;

implementation

uses System.sysutils,
     AlString;

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

{*************************************************************}
function TALRttiObject.GetAttributes: TArray<TCustomAttribute>;
begin
  result := fAttributes;
end;

{***********************************************************}
function TALRttiProperty.GetValue(Instance: Pointer): TValue;
begin
  if not IsReadable then
    raise EPropWriteOnly.Create(Name);
  Result := DoGetValue(Instance);
end;

{**************************************************************************}
procedure TALRttiProperty.SetValue(Instance: Pointer; const AValue: TValue);
begin
  if not IsWritable then
    raise EPropReadOnly.Create(Name);
  DoSetValue(Instance, AValue);
end;

{*****************************************************************************}
constructor TALRttiInstanceProperty.Create(const aRttiProperty: TRttiProperty);
begin
  fRttiProperty := aRttiProperty;
  fPropertyType := aRttiProperty.PropertyType;
  fAttributes := aRttiProperty.GetAttributes;
  fName := aRttiProperty.Name;
end;

{***************************************************}
function TALRttiInstanceProperty.GetDefault: Integer;
begin
  Result := GetPropInfo^.Default;
end;

{*************************************************}
function TALRttiInstanceProperty.GetIndex: Integer;
begin
  Result := GetPropInfo^.Index;
end;

{******************************************************}
function TALRttiInstanceProperty.GetNameIndex: Smallint;
begin
  Result := GetPropInfo^.NameIndex;
end;

{******************************************************}
function TALRttiInstanceProperty.GetPropInfo: PPropInfo;
begin
  result := TRttiInstanceProperty(fRttiProperty).PropInfo;
end;

{******************************************************}
function TALRttiInstanceProperty.GetIsReadable: Boolean;
begin
  Result := PropInfo^.GetProc <> nil;
end;

{******************************************************}
function TALRttiInstanceProperty.GetIsWritable: Boolean;
begin
  Result := PropInfo^.SetProc <> nil;
end;

{*********************************************************************}
function TALRttiInstanceProperty.DoGetValue(Instance: Pointer): TValue;
var
  getter: Pointer;
  code: Pointer;
  args: TArray<TValue>;
begin
  getter := PropInfo^.GetProc;
  if (IntPtr(getter) and PROPSLOT_MASK) = PROPSLOT_FIELD then
  begin
    // Field
    TValue.Make(PByte(Instance) + (IntPtr(getter) and (not PROPSLOT_MASK)),
      PropertyType.Handle, Result);
    Exit;
  end;
  if (IntPtr(getter) and PROPSLOT_MASK) = PROPSLOT_VIRTUAL then
  begin
    // Virtual dispatch, but with offset, not slot
    code := PPointer(PInteger(Instance)^ + Smallint(getter))^;
  end
  else
  begin
    // Static dispatch
    code := Pointer(getter);
  end;

  ALCheckCodeAddress(code);

  if Index = Integer($80000000) then
  begin
    // no index
    SetLength(args, 1);
    args[0] := TObject(Instance);
    Result := Invoke(code, args, ccReg, PropertyType.Handle, False); // not static
  end
  else
  begin
    SetLength(args, 2);
    args[0] := TObject(Instance);
    args[1] := Index;
    Result := Invoke(code, args, ccReg, PropertyType.Handle, False); // not static
  end;
end;

{************************************************************************************}
procedure TALRttiInstanceProperty.DoSetValue(Instance: Pointer; const AValue: TValue);
var
  setter: Pointer;
  code: Pointer;
  args: TArray<TValue>;
begin
  setter := PropInfo^.SetProc;
  if (IntPtr(setter) and PROPSLOT_MASK) = PROPSLOT_FIELD then
  begin
    // Field
    AValue.Cast(PropertyType.Handle).ExtractRawData(
      PByte(Instance) + (IntPtr(setter) and (not PROPSLOT_MASK)));
    Exit;
  end;

  if (IntPtr(setter) and PROPSLOT_MASK) = PROPSLOT_VIRTUAL then
  begin
    // Virtual dispatch, but with offset, not slot
    code := PPointer(PInteger(Instance)^ + Smallint(setter))^;
  end
  else
  begin
    // Static dispatch
    code := setter;
  end;

  ALCheckCodeAddress(code);

  if Index = Integer($80000000) then
  begin
    // no index
    SetLength(args, 2);
    args[0] := TObject(Instance);
    args[1] := AValue.Cast(PropertyType.Handle);
    Invoke(code, args, ccReg, nil);
  end
  else
  begin
    SetLength(args, 3);
    args[0] := TObject(Instance);
    args[1] := Index;
    args[2] := AValue.Cast(PropertyType.Handle);
    Invoke(code, args, ccReg, nil);
  end;
end;

{************************************************}
function TALRttiInstanceProperty.ToString: string;
begin
  Result := 'property ' + Name + ': ' + PropertyType.Name; // do not localize
end;

{*********************************************************}
constructor TALRttiType.Create(const aRttiType: TRttiType);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddField(const aRttiField: TRttiField);
  begin
    case aRttiField.Visibility of
      mvPrivate:begin
                  setlength(fPrivateFields, length(fPrivateFields)+1);
                  fPrivateFields[high(fPrivateFields)] := aRttiField;
                end;
      mvProtected:begin
                    setlength(fPrivateFields, length(fPrivateFields)+1);
                    fPrivateFields[high(fPrivateFields)] := aRttiField;

                    setlength(fProtectedFields, length(fProtectedFields)+1);
                    fProtectedFields[high(fProtectedFields)] := aRttiField;
                  end;
      mvPublic:begin
                 setlength(fPrivateFields, length(fPrivateFields)+1);
                 fPrivateFields[high(fPrivateFields)] := aRttiField;

                 setlength(fProtectedFields, length(fProtectedFields)+1);
                 fProtectedFields[high(fProtectedFields)] := aRttiField;

                 setlength(fPublicFields, length(fPublicFields)+1);
                 fPublicFields[high(fPublicFields)] := aRttiField;
               end;
      mvPublished:begin
                    setlength(fPrivateFields, length(fPrivateFields)+1);
                    fPrivateFields[high(fPrivateFields)] := aRttiField;

                    setlength(fProtectedFields, length(fProtectedFields)+1);
                    fProtectedFields[high(fProtectedFields)] := aRttiField;

                    setlength(fPublicFields, length(fPublicFields)+1);
                    fPublicFields[high(fPublicFields)] := aRttiField;

                    setlength(fPublishedFields, length(fPublishedFields)+1);
                    fPublishedFields[high(fPublishedFields)] := aRttiField;
                  end;
      else raise Exception.Create('Unknown visibility');
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddIndexedProperty(const aRttiIndexedProperty: TRttiIndexedProperty);
  begin
    case aRttiIndexedProperty.Visibility of
      mvPrivate:begin
                  setlength(fPrivateIndexedProperties, length(fPrivateIndexedProperties)+1);
                  fPrivateIndexedProperties[high(fPrivateIndexedProperties)] := aRttiIndexedProperty;
                end;
      mvProtected:begin
                    setlength(fPrivateIndexedProperties, length(fPrivateIndexedProperties)+1);
                    fPrivateIndexedProperties[high(fPrivateIndexedProperties)] := aRttiIndexedProperty;

                    setlength(fProtectedIndexedProperties, length(fProtectedIndexedProperties)+1);
                    fProtectedIndexedProperties[high(fProtectedIndexedProperties)] := aRttiIndexedProperty;
                  end;
      mvPublic:begin
                 setlength(fPrivateIndexedProperties, length(fPrivateIndexedProperties)+1);
                 fPrivateIndexedProperties[high(fPrivateIndexedProperties)] := aRttiIndexedProperty;

                 setlength(fProtectedIndexedProperties, length(fProtectedIndexedProperties)+1);
                 fProtectedIndexedProperties[high(fProtectedIndexedProperties)] := aRttiIndexedProperty;

                 setlength(fPublicIndexedProperties, length(fPublicIndexedProperties)+1);
                 fPublicIndexedProperties[high(fPublicIndexedProperties)] := aRttiIndexedProperty;
               end;
      mvPublished:begin
                    setlength(fPrivateIndexedProperties, length(fPrivateIndexedProperties)+1);
                    fPrivateIndexedProperties[high(fPrivateIndexedProperties)] := aRttiIndexedProperty;

                    setlength(fProtectedIndexedProperties, length(fProtectedIndexedProperties)+1);
                    fProtectedIndexedProperties[high(fProtectedIndexedProperties)] := aRttiIndexedProperty;

                    setlength(fPublicIndexedProperties, length(fPublicIndexedProperties)+1);
                    fPublicIndexedProperties[high(fPublicIndexedProperties)] := aRttiIndexedProperty;

                    setlength(fPublishedIndexedProperties, length(fPublishedIndexedProperties)+1);
                    fPublishedIndexedProperties[high(fPublishedIndexedProperties)] := aRttiIndexedProperty;
                  end;
      else raise Exception.Create('Unknown visibility');
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddMethod(const aRttiMethod: TRttiMethod);
  begin
    case aRttiMethod.Visibility of
      mvPrivate:begin
                  setlength(fPrivateMethods, length(fPrivateMethods)+1);
                  fPrivateMethods[high(fPrivateMethods)] := aRttiMethod;
                end;
      mvProtected:begin
                    setlength(fPrivateMethods, length(fPrivateMethods)+1);
                    fPrivateMethods[high(fPrivateMethods)] := aRttiMethod;

                    setlength(fProtectedMethods, length(fProtectedMethods)+1);
                    fProtectedMethods[high(fProtectedMethods)] := aRttiMethod;
                  end;
      mvPublic:begin
                 setlength(fPrivateMethods, length(fPrivateMethods)+1);
                 fPrivateMethods[high(fPrivateMethods)] := aRttiMethod;

                 setlength(fProtectedMethods, length(fProtectedMethods)+1);
                 fProtectedMethods[high(fProtectedMethods)] := aRttiMethod;

                 setlength(fPublicMethods, length(fPublicMethods)+1);
                 fPublicMethods[high(fPublicMethods)] := aRttiMethod;
               end;
      mvPublished:begin
                    setlength(fPrivateMethods, length(fPrivateMethods)+1);
                    fPrivateMethods[high(fPrivateMethods)] := aRttiMethod;

                    setlength(fProtectedMethods, length(fProtectedMethods)+1);
                    fProtectedMethods[high(fProtectedMethods)] := aRttiMethod;

                    setlength(fPublicMethods, length(fPublicMethods)+1);
                    fPublicMethods[high(fPublicMethods)] := aRttiMethod;

                    setlength(fPublishedMethods, length(fPublishedMethods)+1);
                    fPublishedMethods[high(fPublishedMethods)] := aRttiMethod;
                  end;
      else raise Exception.Create('Unknown visibility');
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddProperty(const aRttiProperty: TRttiProperty);

    function _createRttiProperty: TALRttiProperty;
    begin
      if aRttiProperty is TRttiInstanceProperty then result := TALRttiInstanceProperty.Create(aRttiProperty)
      else raise Exception.Create('Unknown TRttiProperty descendant ('+aRttiProperty.ClassName+')');
    end;

  begin
    case aRttiProperty.Visibility of
      mvPrivate:begin
                  setlength(fPrivateProperties, length(fPrivateProperties)+1);
                  fPrivateProperties[high(fPrivateProperties)] := _createRttiProperty;
                end;
      mvProtected:begin
                    setlength(fPrivateProperties, length(fPrivateProperties)+1);
                    fPrivateProperties[high(fPrivateProperties)] := _createRttiProperty;

                    setlength(fProtectedProperties, length(fProtectedProperties)+1);
                    fProtectedProperties[high(fProtectedProperties)] := _createRttiProperty;
                  end;
      mvPublic:begin
                 setlength(fPrivateProperties, length(fPrivateProperties)+1);
                 fPrivateProperties[high(fPrivateProperties)] := _createRttiProperty;

                 setlength(fProtectedProperties, length(fProtectedProperties)+1);
                 fProtectedProperties[high(fProtectedProperties)] := _createRttiProperty;

                 setlength(fPublicProperties, length(fPublicProperties)+1);
                 fPublicProperties[high(fPublicProperties)] := _createRttiProperty;
               end;
      mvPublished:begin
                    setlength(fPrivateProperties, length(fPrivateProperties)+1);
                    fPrivateProperties[high(fPrivateProperties)] := _createRttiProperty;

                    setlength(fProtectedProperties, length(fProtectedProperties)+1);
                    fProtectedProperties[high(fProtectedProperties)] := _createRttiProperty;

                    setlength(fPublicProperties, length(fPublicProperties)+1);
                    fPublicProperties[high(fPublicProperties)] := _createRttiProperty;

                    setlength(fPublishedProperties, length(fPublishedProperties)+1);
                    fPublishedProperties[high(fPublishedProperties)] := _createRttiProperty;
                  end;
      else raise Exception.Create('Unknown visibility');
    end;
  end;

var aRttiProperty: TRttiProperty;
    aRttiIndexedProperty: TRttiIndexedProperty;
    aRttiField: TRttiField;
    aRttiMethod: TRttiMethod;

begin

  setlength(fPrivateIndexedProperties, 0);
  setlength(fPrivateProperties, 0);
  setlength(fPrivateFields, 0);
  setlength(fPrivateMethods, 0);

  setlength(fProtectedIndexedProperties, 0);
  setlength(fProtectedProperties, 0);
  setlength(fProtectedFields, 0);
  setlength(fProtectedMethods, 0);

  setlength(fPublicIndexedProperties, 0);
  setlength(fPublicProperties, 0);
  setlength(fPublicFields, 0);
  setlength(fPublicMethods, 0);

  setlength(fPublishedIndexedProperties, 0);
  setlength(fPublishedProperties, 0);
  setlength(fPublishedFields, 0);
  setlength(fPublishedMethods, 0);

  for aRttiProperty in aRttiType.GetProperties do begin
    aRttiProperty.GetAttributes; // to create the customAttribute object
    _AddProperty(aRttiProperty);
  end;

  for aRttiIndexedProperty in aRttiType.GetIndexedProperties do begin
    aRttiIndexedProperty.GetAttributes; // to create the customAttribute object
    _AddIndexedProperty(aRttiIndexedProperty);
  end;

  for aRttiField in aRttiType.GetFields do begin
    aRttiField.GetAttributes; // to create the customAttribute object
    _AddField(aRttiField);
  end;

  for aRttiMethod in aRttiType.GetMethods do begin
    aRttiMethod.GetAttributes; // to create the customAttribute object
    _AddMethod(aRttiMethod);
  end;

  fAttributes := aRttiType.GetAttributes;

end;

{*****************************}
destructor TALRttiType.Destroy;
var i: integer;
begin
  for i := Low(fPrivateProperties) to High(fPrivateProperties) do fPrivateProperties[i].Free;
  for i := Low(fProtectedProperties) to High(fProtectedProperties) do fProtectedProperties[i].Free;
  for i := Low(fPublicProperties) to High(fPublicProperties) do fPublicProperties[i].Free;
  for i := Low(fPublishedProperties) to High(fPublishedProperties) do fPublishedProperties[i].Free;
end;

{***************************************************************************************}
function TALRttiType.GetFields(const aVisibility: TMemberVisibility): TArray<TRttiField>;
begin
  case aVisibility of
    mvPrivate:result := fPrivateFields;
    mvProtected:result := fProtectedFields;
    mvPublic:result := fPublicFields;
    mvPublished:result := fPublishedFields;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{***************************************************************************************************}
function TALRttiType.GetField(const AName: string; const aVisibility: TMemberVisibility): TRttiField;
begin
  for Result in GetFields(aVisibility) do
    if SameText(Result.Name, AName) then
      Exit;
  Result := nil;
end;

{************************************************************************************************************}
function TALRttiType.GetIndexedProperties(const aVisibility: TMemberVisibility): TArray<TRttiIndexedProperty>;
begin
  case aVisibility of
    mvPrivate:result := fPrivateIndexedProperties;
    mvProtected:result := fProtectedIndexedProperties;
    mvPublic:result := fPublicIndexedProperties;
    mvPublished:result := fPublishedIndexedProperties;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{*****************************************************************************************}
function TALRttiType.GetMethods(const aVisibility: TMemberVisibility): TArray<TRttiMethod>;
begin
  case aVisibility of
    mvPrivate:result := fPrivateMethods;
    mvProtected:result := fProtectedMethods;
    mvPublic:result := fPublicMethods;
    mvPublished:result := fPublishedMethods;
    else raise Exception.Create('Unknown visibility');
  end;
end;

{**************************************************************************************************************}
function TALRttiType.GetMethods(const AName: string; const aVisibility: TMemberVisibility): TArray<TRttiMethod>;
var ms: TArray<TRttiMethod>;
    m: TRttiMethod;
    len: Integer;
begin
  ms := GetMethods(aVisibility);
  len := 0;
  for m in ms do
    if SameText(m.Name, AName) then
      Inc(len);
  if len = 0 then
    Exit(nil);
  SetLength(Result, len);
  len := 0;
  for m in ms do
    if SameText(m.Name, AName) then
    begin
      Result[len] := m;
      Inc(len);
    end;
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

{***********************************************************************************************************}
function TALRttiType.GetProperty(const AName: string; const aVisibility: TMemberVisibility): TALRttiProperty;
begin
  for Result in GetProperties(aVisibility) do
    if SameText(Result.Name, AName) then
      Exit;
  Result := nil;
end;

{*************************************************************************************************************}
function TALRttiType.GetProperty(const AIndex: integer; const aVisibility: TMemberVisibility): TALRttiProperty;
begin
  for Result in GetProperties(aVisibility) do
    if TRttiInstanceProperty(Result).Index = AIndex then
      Exit;
  Result := nil;
end;

{**********************************************}
var vALRttiTypeCache: TALStringKeyAVLBinaryTree;

{**}
type
  TALRttiTypeCacheNode = Class(TALStringKeyAVLBinaryTreeNode)
  public
    RttiType: TALRttiType;
    Constructor Create(const aRttiType: TRttiType); reintroduce;
    destructor Destroy; Override;
  End;

{******************************************************************}
constructor TALRttiTypeCacheNode.Create(const aRttiType: TRttiType);
begin
  inherited create;
  RttiType := TALRttiType.Create(aRttiType);
end;

{**************************************}
destructor TALRttiTypeCacheNode.Destroy;
begin
  RttiType.Free;
  inherited;
end;

{****************************************************************}
function ALGetRttiType(const aClassName: AnsiString): TALRttiType;
var aRttiTypeCacheNode: Tobject;
begin
  aRttiTypeCacheNode := vALRttiTypeCache.FindNode(aClassName);
  if not assigned(aRttiTypeCacheNode) then raise EALException.Create('Cannot obtain RTTI informations about the class ' + aClassName)
  else result := TALRttiTypeCacheNode(aRttiTypeCacheNode).RttiType;
end;

{**************************************************************}
function ALGetRttiType(const aTypeInfo: PTypeInfo): TALRttiType;
begin
  result := ALGetRttiType(aTypeInfo.Name);
end;

{*****************************}
procedure ALRttiInitialization;
var aRttiTypes: TArray<TRttiType>;
    aRttiTypeCacheNode: TALRttiTypeCacheNode;
    i: integer;
begin

  //create vALRTTIContext
  vALRTTIContext := TRttiContext.Create;

  //create vALRttiTypeCache
  vALRttiTypeCache := TALStringKeyAVLBinaryTree.Create;

  //init vALRttiTypeCache
  aRTTITypes := vALRTTIContext.GetTypes;
  for I := Low(aRttiTypes) to High(aRttiTypes) do begin
    aRttiTypeCacheNode := TALRttiTypeCacheNode.Create(aRttiTypes[i]);
    try
      aRttiTypeCacheNode.ID := aRttiTypes[i].Handle.Name;
      if not vALRttiTypeCache.AddNode(aRttiTypeCacheNode) then
        raise Exception.Create('Duplicate name ('+string(aRttiTypeCacheNode.ID)+')');
    except
      aRttiTypeCacheNode.Free;
      raise;
    end;
  end;

end;

{***************************}
procedure ALRttiFinalization;
Begin
  vALRTTIContext.Free;
  vALRttiTypeCache.Free;
End;

end.