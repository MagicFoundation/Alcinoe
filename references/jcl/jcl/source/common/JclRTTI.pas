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
{ The Original Code is JclRTTI.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel Bestebroer.                                 }
{ Portions created Marcel Bestebroer are Copyright (C) Marcel Bestebroer. All rights reserved.     }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Theo Bebekis                                                                                   }
{   Marcel Bestebroer (marcelb)                                                                    }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various RunTime Type Information routines. Includes retrieving RTTI information for different    }
{ types, declaring/generating new types, data conversion to user displayable values and 'is'/'as'  }
{ operator hooking.                                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclRTTI;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  System.Types,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  System.Classes, System.SysUtils, System.TypInfo,
  {$ELSE ~HAS_UNITSCOPE}
  Types,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils, TypInfo,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase;

type
  // TypeInfo writing
  IJclInfoWriter = interface
    ['{7DAD522D-46EA-11D5-B0C0-4854E825F345}']
    function GetWrap: Integer;
    procedure SetWrap(const Value: Integer);
    procedure Write(const S: string);
    procedure Writeln(const S: string = '');
    procedure Indent;
    procedure Outdent;
    property Wrap: Integer read GetWrap write SetWrap;
  end;

  TJclInfoWriter = class(TInterfacedObject, IJclInfoWriter)
  private
    FCurLine: string;
    FIndentLevel: Integer;
    FWrap: Integer;
  protected
    procedure DoWrap;
    procedure DoWriteCompleteLines;
    procedure PrimWrite(const S: string); virtual; abstract;

    property CurLine: string read FCurLine write FCurLine;
    property IndentLevel: Integer read FIndentLevel write FIndentLevel;
  public
    constructor Create(const AWrap: Integer = 80);
    destructor Destroy; override;
    { IJclInfoWriter }
    function GetWrap: Integer;
    procedure SetWrap(const Value: Integer);
    procedure Write(const S: string);
    procedure Writeln(const S: string = '');
    procedure Indent;
    procedure Outdent;
    property Wrap: Integer read GetWrap write SetWrap;
  end;

  TJclInfoStringsWriter = class(TJclInfoWriter)
  private
    FStrings: TStrings;
  protected
    procedure PrimWrite(const S: string); override;
  public
    constructor Create(const AStrings: TStrings; const AWrap: Integer = 80);

    property Strings: TStrings read FStrings;
  end;

  // TypeInfo retrieval
  IJclBaseInfo = interface
    ['{84E57A52-7219-4248-BDC7-4AACBFE2002D}']
    procedure WriteTo(const Dest: IJclInfoWriter);
    procedure DeclarationTo(const Dest: IJclInfoWriter);
  end;

  IJclTypeInfo = interface(IJclBaseInfo)
    ['{7DAD5220-46EA-11D5-B0C0-4854E825F345}']
    function GetName: string;
    function GetTypeData: PTypeData;
    function GetTypeInfo: PTypeInfo;
    function GetTypeKind: TTypeKind;

    property Name: string read GetName;
    property TypeData: PTypeData read GetTypeData;
    property TypeInfo: PTypeInfo read GetTypeInfo;
    property TypeKind: TTypeKind read GetTypeKind;
  end;

  TJclTypeInfo = class(TInterfacedObject, IJclBaseInfo, IJclTypeInfo)
  private
    FTypeData: PTypeData;
    FTypeInfo: PTypeInfo;
  public
    constructor Create(ATypeInfo: PTypeInfo);
    { IJclBaseInfo }
    procedure WriteTo(const Dest: IJclInfoWriter); virtual;
    procedure DeclarationTo(const Dest: IJclInfoWriter); virtual;
    { IJclTypeInfo }
    function GetName: string;
    function GetTypeData: PTypeData;
    function GetTypeInfo: PTypeInfo;
    function GetTypeKind: TTypeKind;
    property Name: string read GetName;
    property TypeData: PTypeData read GetTypeData;
    property TypeInfo: PTypeInfo read GetTypeInfo;
    property TypeKind: TTypeKind read GetTypeKind;
  end;

  // for all values that can be serialized to/deserialized from strings
  IJclValueTypeInfo = interface(IJclTypeInfo)
    ['{522C6E39-F917-4C92-B085-223BD68C377F}']
    function SaveValueToString(AnObj: TObject; const PropName: string): string;
    procedure LoadValueFromString(AnObj: TObject; const PropName, Value: string);
  end;

  // Ordinal types
  IJclOrdinalTypeInfo = interface(IJclValueTypeInfo)
    ['{7DAD5221-46EA-11D5-B0C0-4854E825F345}']
    function GetOrdinalType: TOrdType;

    property OrdinalType: TOrdType read GetOrdinalType;
  end;

  TJclOrdinalTypeInfo = class(TJclTypeInfo, IJclBaseInfo, IJclTypeInfo,
    IJclValueTypeInfo, IJclOrdinalTypeInfo)
  public
    { IJclBaseInfo }
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    { IJclValueTypeInfo }
    function SaveValueToString(AnObj: TObject; const PropName: string): string;
    procedure LoadValueFromString(AnObj: TObject; const PropName, Value: string);
    { IJclOrdinalTypeInfo }
    function GetOrdinalType: TOrdType;
    property OrdinalType: TOrdType read GetOrdinalType;
  end;

  IJclOrdinalRangeTypeInfo = interface(IJclOrdinalTypeInfo)
    ['{7DAD5222-46EA-11D5-B0C0-4854E825F345}']
    function GetMinValue: Int64;
    function GetMaxValue: Int64;

    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

  TJclOrdinalRangeTypeInfo = class(TJclOrdinalTypeInfo, IJclBaseInfo, IJclTypeInfo,
    IJclValueTypeInfo, IJclOrdinalTypeInfo, IJclOrdinalRangeTypeInfo)
  public
    { IJclBaseInfo }
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
    { IJclOrdinalRangeTypeInfo }
    function GetMinValue: Int64;
    function GetMaxValue: Int64;
    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

  IJclEnumerationTypeInfo = interface(IJclOrdinalRangeTypeInfo)
    ['{7DAD5223-46EA-11D5-B0C0-4854E825F345}']
    function GetBaseType: IJclEnumerationTypeInfo;
    function GetNames(const I: Integer): string;
    function GetUnitName: string;

    function IndexOfName(const Name: string): Integer;

    property BaseType: IJclEnumerationTypeInfo read GetBaseType;
    property Names[const I: Integer]: string read GetNames; default;
    property UnitName: string read GetUnitName;
  end;

  TJclEnumerationTypeInfo = class(TJclOrdinalRangeTypeInfo, IJclBaseInfo, IJclTypeInfo, 
    IJclValueTypeInfo, IJclOrdinalTypeInfo, IJclOrdinalRangeTypeInfo, IJclEnumerationTypeInfo)
  public
    { IJclBaseInfo }
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
    { IJclValueTypeInfo }
    function SaveValueToString(AnObj: TObject; const PropName: string): string;
    procedure LoadValueFromString(AnObj: TObject; const PropName, Value: string);
    { IJclEnumerationTypeInfo }
    function GetBaseType: IJclEnumerationTypeInfo;
    function GetNames(const I: Integer): string;
    function GetUnitName: string;
    function IndexOfName(const Name: string): Integer;
    property BaseType: IJclEnumerationTypeInfo read GetBaseType;
    property Names[const I: Integer]: string read GetNames; default;
  end;

  IJclSetTypeInfo = interface(IJclOrdinalTypeInfo)
    ['{7DAD5224-46EA-11D5-B0C0-4854E825F345}']
    function GetBaseType: IJclOrdinalTypeInfo;

    procedure GetAsList(const Value;  const WantRanges: Boolean;
      const Strings: TStrings);
    procedure SetAsList(out Value; const Strings: TStrings);

    property BaseType: IJclOrdinalTypeInfo read GetBaseType;
  end;

  TJclSetTypeInfo = class(TJclOrdinalTypeInfo, IJclBaseInfo, IJclTypeInfo,
    IJclValueTypeInfo, IJclOrdinalTypeInfo, IJclSetTypeInfo)
  public
    { IJclBaseInfo }
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
    { IJclValueInfo }
    function SaveValueToString(AnObj: TObject; const PropName: string): string;
    procedure LoadValueFromString(AnObj: TObject; const PropName, Value: string);
    { IJclSetTypeInfo }
    function GetBaseType: IJclOrdinalTypeInfo;
    procedure GetAsList(const Value; const WantRanges: Boolean;
      const Strings: TStrings);
    procedure SetAsList(out Value; const Strings: TStrings);
    property BaseType: IJclOrdinalTypeInfo read GetBaseType;
  end;

  // Float types
  IJclFloatTypeInfo = interface(IJclValueTypeInfo)
    ['{7DAD5225-46EA-11D5-B0C0-4854E825F345}']
    function GetFloatType: TFloatType;

    property FloatType: TFloatType read GetFloatType;
  end;

  TJclFloatTypeInfo = class(TJclTypeInfo, IJclBaseInfo, IJclTypeInfo,
    IJclValueTypeInfo, IJclFloatTypeInfo)
  public
    { IJclBaseInfo }
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
    { IJclValueInfo }
    function SaveValueToString(AnObj: TObject; const PropName: string): string;
    procedure LoadValueFromString(AnObj: TObject; const PropName, Value: string);
    { IJclFloatTypeInfo }
    function GetFloatType: TFloatType;
    property FloatType: TFloatType read GetFloatType;
  end;

  // Short string types
  IJclStringTypeInfo = interface(IJclValueTypeInfo)
    ['{7DAD5226-46EA-11D5-B0C0-4854E825F345}']
    function GetMaxLength: Integer;

    property MaxLength: Integer read GetMaxLength;
  end;

  TJclStringTypeInfo = class(TJclTypeInfo, IJclBaseInfo, IJclTypeInfo,
    IJclValueTypeInfo, IJclStringTypeInfo)
  public
    { IJclBaseInfo }
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
    { IJclValueInfo }
    function SaveValueToString(AnObj: TObject; const PropName: string): string;
    procedure LoadValueFromString(AnObj: TObject; const PropName, Value: string);
    { IJclStringTypeInfo }
    function GetMaxLength: Integer;
    property MaxLength: Integer read GetMaxLength;
  end;

  // Class types
  TJclPropSpecKind = (pskNone, pskStaticMethod, pskVirtualMethod, pskField,
    pskConstant);

  IJclPropInfo = interface
    ['{7DAD5227-46EA-11D5-B0C0-4854E825F345}']
    function GetPropInfo: PPropInfo;
    function GetPropType: IJclTypeInfo;
    function GetReader: Pointer;
    function GetWriter: Pointer;
    function GetStoredProc: Pointer;
    function GetIndex: Integer;
    function GetDefault: Longint;
    function GetNameIndex: Smallint;
    function GetName: string;
    function GetReaderType: TJclPropSpecKind;
    function GetWriterType: TJclPropSpecKind;
    function GetStoredType: TJclPropSpecKind;
    function GetReaderValue: TJclAddr;
    function GetWriterValue: TJclAddr;
    function GetStoredValue: TJclAddr;

    function IsStored(const AInstance: TObject): Boolean;
    function HasDefault: Boolean;
    function HasIndex: Boolean;

    function SaveValueToString(AnObj: TObject): string;
    procedure LoadValueFromString(AnObj: TObject; const Value: string);

    property PropInfo: PPropInfo read GetPropInfo;
    property PropType: IJclTypeInfo read GetPropType;
    property Reader: Pointer read GetReader;
    property Writer: Pointer read GetWriter;
    property StoredProc: Pointer read GetStoredProc;
    property ReaderType: TJclPropSpecKind read GetReaderType;
    property WriterType: TJclPropSpecKind read GetWriterType;
    property StoredType: TJclPropSpecKind read GetStoredType;
    property ReaderValue: TJclAddr read GetReaderValue;
    property WriterValue: TJclAddr read GetWriterValue;
    property StoredValue: TJclAddr read GetStoredValue;
    property Index: Integer read GetIndex;
    property Default: Longint read GetDefault;
    property NameIndex: Smallint read GetNameIndex;
    property Name: string read GetName;
  end;

  TJclPropInfo = class(TInterfacedObject, IJclPropInfo)
  private
    FPropInfo: PPropInfo;
  public
    constructor Create(const APropInfo: PPropInfo);
    { IJclPropInfo }
    function GetPropInfo: PPropInfo;
    function GetPropType: IJclTypeInfo;
    function GetReader: Pointer;
    function GetWriter: Pointer;
    function GetStoredProc: Pointer;
    function GetIndex: Integer;
    function GetDefault: Longint;
    function GetNameIndex: Smallint;
    function GetName: string;
    function GetSpecKind(const Value: TJclAddr): TJclPropSpecKind;
    function GetSpecValue(const Value: TJclAddr): TJclAddr;
    function GetReaderType: TJclPropSpecKind;
    function GetWriterType: TJclPropSpecKind;
    function GetStoredType: TJclPropSpecKind;
    function GetReaderValue: TJclAddr;
    function GetWriterValue: TJclAddr;
    function GetStoredValue: TJclAddr;

    function IsStored(const AInstance: TObject): Boolean;
    function HasDefault: Boolean;
    function HasIndex: Boolean;

    function SaveValueToString(AnObj: TObject): string;
    procedure LoadValueFromString(AnObj: TObject; const Value: string);

    property PropInfo: PPropInfo read GetPropInfo;
    property PropType: IJclTypeInfo read GetPropType;
    property Reader: Pointer read GetReader;
    property Writer: Pointer read GetWriter;
    property StoredProc: Pointer read GetStoredProc;
    property ReaderType: TJclPropSpecKind read GetReaderType;
    property WriterType: TJclPropSpecKind read GetWriterType;
    property StoredType: TJclPropSpecKind read GetStoredType;
    property ReaderValue: TJclAddr read GetReaderValue;
    property WriterValue: TJclAddr read GetWriterValue;
    property StoredValue: TJclAddr read GetStoredValue;
    property Index: Integer read GetIndex;
    property Default: Longint read GetDefault;
    property NameIndex: Smallint read GetNameIndex;
    property Name: string read GetName;
  end;

  IJclObjPropInfo = interface(IJclPropInfo)
    function GetAbsoluteName: string;
    function GetInstance: TObject;
    function IsStored: Boolean; overload;

    function SaveValueToString: string;
    procedure LoadValueFromString(const Value: string);

    property AbsoluteName: string read GetAbsoluteName;
    property Instance: TObject read GetInstance;
  end;

  IJclObjPropInfoArray = array of IJclObjPropInfo;

  TJclObjPropInfo = class(TJclPropInfo, IJclPropInfo, IJclObjPropInfo)
  private
    FPrefix: string;
    FInstance: TObject;
  public
    constructor Create(const APropInfo: PPropInfo; const APrefix: string; AInstance: TObject);
    { IJclObjPropInfo }
    function GetAbsoluteName: string;
    function GetInstance: TObject;
    function IsStored: Boolean; overload;
    function SaveValueToString: string; overload;
    procedure LoadValueFromString(const Value: string); overload;
  end;

  IJclClassTypeInfo = interface(IJclValueTypeInfo)
    ['{7DAD5228-46EA-11D5-B0C0-4854E825F345}']
    function GetClassRef: TClass;
    function GetParent: IJclClassTypeInfo;
    function GetTotalPropertyCount: Integer;
    function GetPropertyCount: Integer;
    function GetProperties(const PropIdx: Integer): IJclPropInfo;
    function GetPropNames(const Name: string): IJclPropInfo;
    function GetUnitName: string;

    property ClassRef: TClass read GetClassRef;
    property Parent: IJclClassTypeInfo read GetParent;
    property TotalPropertyCount: Integer read GetTotalPropertyCount;
    property PropertyCount: Integer read GetPropertyCount;
    property Properties[const PropIdx: Integer]: IJclPropInfo read GetProperties;
    property PropNames[const Name: string]: IJclPropInfo read GetPropNames;
    property UnitName: string read GetUnitName;
  end;

  TJclClassTypeInfo = class(TJclTypeInfo, IJclBaseInfo, IJclTypeInfo,
    IJclValueTypeInfo, IJclClassTypeInfo)
  public
    { IJclBaseInfo }
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
    { IJclValueInfo }
    function SaveValueToString(AnObj: TObject; const PropName: string): string;
    procedure LoadValueFromString(AnObj: TObject; const PropName, Value: string);
    { IJclClassTypeInfo }
    function GetClassRef: TClass;
    function GetParent: IJclClassTypeInfo;
    function GetTotalPropertyCount: Integer;
    function GetPropertyCount: Integer;
    function GetProperties(const PropIdx: Integer): IJclPropInfo;
    function GetPropNames(const Name: string): IJclPropInfo;
    function GetUnitName: string;
    property ClassRef: TClass read GetClassRef;
    property Parent: IJclClassTypeInfo read GetParent;
    property TotalPropertyCount: Integer read GetTotalPropertyCount;
    property PropertyCount: Integer read GetPropertyCount;
    property Properties[const PropIdx: Integer]: IJclPropInfo read GetProperties;
    property PropNames[const Name: string]: IJclPropInfo read GetPropNames;
  end;

  IJclObjClassTypeInfo = interface(IJclClassTypeInfo)
    ['{5BF4383D-7FDD-4494-88CC-849D72B5E142}']
    function GetInstance: TObject;
    function GetObjProperties(const PropIdx: Integer): IJclObjPropInfo;
    function GetObjPropNames(const Name: string): IJclObjPropInfo;

    function SaveValueToString(const PropName: string): string; overload;
    procedure LoadValueFromString(const PropName, Value: string); overload;

    property Instance: TObject read GetInstance;
    property ObjProperties[const PropIdx: Integer]: IJclObjPropInfo read GetObjProperties;
    property ObjPropNames[const Name: string]: IJclObjPropInfo read GetObjPropNames;
  end;

  TJclObjClassTypeInfo = class(TJclClassTypeInfo, IJclBaseInfo, IJclTypeInfo,
    IJclValueTypeInfo, IJclClassTypeInfo, IJclObjClassTypeInfo)
  private
    FPrefix: string;
    FInstance: TObject;
  public
    constructor Create(const ATypeInfo: PTypeInfo; const APrefix: string; AInstance: TObject);
    { IJclObjClassTypeInfo }
    function GetInstance: TObject;
    function GetObjProperties(const PropIdx: Integer): IJclObjPropInfo;
    function GetObjPropNames(const Name: string): IJclObjPropInfo;
    function SaveValueToString(const PropName: string): string; overload;
    procedure LoadValueFromString(const PropName, Value: string); overload;
  end;

  // Event types
  IJclEventParamInfo = interface
    ['{7DAD5229-46EA-11D5-B0C0-4854E825F345}']
    function GetFlags: TParamFlags;
    function GetName: string;
    function GetRecSize: Integer;
    function GetTypeName: string;
    function GetParam: Pointer;

    property Flags: TParamFlags read GetFlags;
    property Name: string read GetName;
    property RecSize: Integer read GetRecSize;
    property TypeName: string read GetTypeName;
    property Param: Pointer read GetParam;
  end;

  TJclEventParamInfo = class(TInterfacedObject, IJclEventParamInfo)
  private
    FParam: Pointer;
  public
    constructor Create(const AParam: Pointer);
    { IJclEventParamInfo }
    function GetFlags: TParamFlags;
    function GetName: string;
    function GetRecSize: Integer;
    function GetTypeName: string;
    function GetParam: Pointer;
    property Flags: TParamFlags read GetFlags;
    property Name: string read GetName;
    property RecSize: Integer read GetRecSize;
    property TypeName: string read GetTypeName;
    property Param: Pointer read GetParam;
  end;

  IJclEventTypeInfo = interface(IJclTypeInfo)
    ['{7DAD522A-46EA-11D5-B0C0-4854E825F345}']
    function GetMethodKind: TMethodKind;
    function GetParameterCount: Integer;
    function GetParameters(const ParamIdx: Integer): IJclEventParamInfo;
    function GetResultTypeName: string;

    property MethodKind: TMethodKind read GetMethodKind;
    property ParameterCount: Integer read GetParameterCount;
    property Parameters[const ParamIdx: Integer]: IJclEventParamInfo
      read GetParameters;
    property ResultTypeName: string read GetResultTypeName;
  end;

  TJclEventTypeInfo = class(TJclTypeInfo, IJclBaseInfo, IJclTypeInfo, IJclEventTypeInfo)
  public
    { IJclBaseInfo }
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
    { IJclEventTypeInfo }
    function GetMethodKind: TMethodKind;
    function GetParameterCount: Integer;
    function GetParameters(const ParamIdx: Integer): IJclEventParamInfo;
    function GetResultTypeName: string;
    property MethodKind: TMethodKind read GetMethodKind;
    property ParameterCount: Integer read GetParameterCount;
    property Parameters[const ParamIdx: Integer]: IJclEventParamInfo
      read GetParameters;
    property ResultTypeName: string read GetResultTypeName;
  end;

  // Interface types
  IJclInterfaceTypeInfo = interface(IJclTypeInfo)
    ['{7DAD522B-46EA-11D5-B0C0-4854E825F345}']
    function GetParent: IJclInterfaceTypeInfo;
    function GetFlags: TIntfFlagsBase;
    function GetGUID: TGUID;
    function GetPropertyCount: Integer;
    function GetUnitName: string;

    property Parent: IJclInterfaceTypeInfo read GetParent;
    property Flags: TIntfFlagsBase read GetFlags;
    property GUID: TGUID read GetGUID;
    property PropertyCount: Integer read GetPropertyCount;
    property UnitName: string read GetUnitName;
  end;

  TJclInterfaceTypeInfo = class(TJclTypeInfo, IJclBaseInfo, IJclTypeInfo,
    IJclInterfaceTypeInfo)
  public
    { IJclBaseInfo }
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
    { IJclInterfaceTypeInfo }
    function GetParent: IJclInterfaceTypeInfo;
    function GetFlags: TIntfFlagsBase;
    function GetGUID: TGUID;
    function GetPropertyCount: Integer;
    function GetUnitName: string;
    property Parent: IJclInterfaceTypeInfo read GetParent;
    property Flags: TIntfFlagsBase read GetFlags;
    property GUID: TGUID read GetGUID;
    property PropertyCount: Integer read GetPropertyCount;
  end;

  // Int64 types
  IJclInt64TypeInfo = interface(IJclValueTypeInfo)
    ['{7DAD522C-46EA-11D5-B0C0-4854E825F345}']
    function GetMinValue: Int64;
    function GetMaxValue: Int64;

    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

  TJclInt64TypeInfo = class(TJclTypeInfo, IJclBaseInfo, IJclTypeInfo,
    IJclValueTypeInfo, IJclInt64TypeInfo)
  public
    { IJclBaseInfo }
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
    { IJclValueInfo }
    function SaveValueToString(AnObj: TObject; const PropName: string): string;
    procedure LoadValueFromString(AnObj: TObject; const PropName, Value: string);
    { IJclInt64TypeInfo }
    function GetMinValue: Int64;
    function GetMaxValue: Int64;
    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

  // Dynamic array types
  IJclDynArrayTypeInfo = interface(IJclTypeInfo)
    ['{7DAD522E-46EA-11D5-B0C0-4854E825F345}']
    function GetElementSize: Longint;
    function GetElementType: IJclTypeInfo;
    function GetElementsNeedCleanup: Boolean;
    function GetVarType: Integer;
    function GetUnitName: string;

    property ElementSize: Longint read GetElementSize;
    property ElementType: IJclTypeInfo read GetElementType;
    property ElementsNeedCleanup: Boolean read GetElementsNeedCleanup;
    property VarType: Integer read GetVarType;
    property UnitName: string read GetUnitName;
  end;

  TJclDynArrayTypeInfo = class(TJclTypeInfo, IJclBaseInfo, IJclTypeInfo,
    IJclDynArrayTypeInfo)
  public
    { IJclBaseInfo }
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
    { IJclDynArrayTypeInfo }
    function GetElementSize: Longint;
    function GetElementType: IJclTypeInfo;
    function GetElementsNeedCleanup: Boolean;
    function GetVarType: Integer;
    function GetUnitName: string;
    property ElementSize: Longint read GetElementSize;
    property ElementType: IJclTypeInfo read GetElementType;
    property ElementsNeedCleanup: Boolean read GetElementsNeedCleanup;
    property VarType: Integer read GetVarType;
  end;

  EJclRTTIError = class(EJclError);

function JclTypeInfo(ATypeInfo: PTypeInfo): IJclTypeInfo;

// Enumeration types
const
  PREFIX_CUT_LOWERCASE = 255;
  PREFIX_CUT_EQUAL     = 254;

  MaxPrefixCut = 250;

function JclEnumValueToIdent(TypeInfo: PTypeInfo; const Value): string;

function JclGenerateEnumType(const TypeName: ShortString;
  const Literals: array of string): PTypeInfo;
function JclGenerateEnumTypeBasedOn(const TypeName: ShortString;
  BaseType: PTypeInfo; const PrefixCut: Byte): PTypeInfo;
function JclGenerateSubRange(BaseType: PTypeInfo; const TypeName: string;
  const MinValue, MaxValue: Integer): PTypeInfo;


// Integer types
function JclStrToTypedInt(Value: string; TypeInfo: PTypeInfo): Integer;
function JclTypedIntToStr(Value: Integer; TypeInfo: PTypeInfo): string;

// Sets
function JclSetToList(TypeInfo: PTypeInfo; const Value; const WantBrackets: Boolean; const WantRanges: Boolean;
  const Strings: TStrings): string;
function JclSetToStr(TypeInfo: PTypeInfo; const Value; const WantBrackets: Boolean = False;
  const WantRanges: Boolean = False): string;
procedure JclStrToSet(TypeInfo: PTypeInfo; var SetVar; const Value: string);
procedure JclIntToSet(TypeInfo: PTypeInfo; var SetVar; const Value: Integer);
function JclSetToInt(TypeInfo: PTypeInfo; const SetVar): Integer;
function JclGenerateSetType(BaseType: PTypeInfo; const TypeName: ShortString): PTypeInfo;

// User generated type info managment
procedure RemoveTypeInfo(TypeInfo: PTypeInfo);

// Is/As hooking
function JclIsClass(const AnObj: TObject; const AClass: TClass): Boolean;
function JclIsClassByName(const AnObj: TObject; const AClass: TClass): Boolean;

// returns all properties of type string (kind = tkLString or kind = tkUString when Unicode is enabled)
function GetStringPropList(TypeInfo: PTypeInfo; out PropList: PPropList): Integer;

// returns all object properties
function GetObjectProperties(AnObj: TObject; Recurse: Boolean = False): IJclObjPropInfoArray;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.SysConst,
  {$ELSE ~HAS_UNITSCOPE}
  SysConst,
  {$ENDIF ~HAS_UNITSCOPE}
  JclLogic, JclResources, JclStrings, JclSysUtils;

//=== { TJclInfoWriter } =====================================================

constructor TJclInfoWriter.Create(const AWrap: Integer);
begin
  inherited Create;
  Wrap := AWrap;
end;

destructor TJclInfoWriter.Destroy;
begin
  if CurLine <> '' then
    Writeln('');
  inherited Destroy;
end;

function TJclInfoWriter.GetWrap: Integer;
begin
  Result := FWrap;
end;

procedure TJclInfoWriter.SetWrap(const Value: Integer);
begin
  FWrap := Value;
end;

procedure TJclInfoWriter.DoWrap;
const
  WrapChars : TSetOfAnsiChar = [#0..' ', '-'];
var
  TmpLines: TStringList;
  I: Integer;
  TmpLines2: TStringList;
  EndedInCRLF: Boolean;
  LineBreakLength: Integer;
begin
  LineBreakLength := Length(NativeLineBreak);
  EndedInCRLF := Copy(CurLine, Length(CurLine) - LineBreakLength + 1, LineBreakLength) = NativeLineBreak;
  TmpLines := TStringList.Create;
  try
    TmpLines.Text := CurLine;
    TmpLines2 := TStringList.Create;
    try
      I := TmpLines.Count-1;
      if not EndedInCRLF then
        Dec(I);
      while I >= 0 do
      begin
        TmpLines[I] := StringOfChar(' ', 2 * IndentLevel) + TmpLines[I];
        if (Wrap > 0) and (Length(TmpLines[I]) > Wrap) then
        begin
          TmpLines2.Text := WrapText(
            TmpLines[I],
            NativeLineBreak + StringOfChar(' ', 2 * (IndentLevel+1)),
            WrapChars,
            Wrap);
          TmpLines.Delete(I);
          TmpLines.Insert(I, Copy(TmpLines2.Text, 1,
            Length(TmpLines2.Text) - 2));
        end;
        Dec(I);
      end;
      CurLine := TmpLines.Text;
      if not EndedInCRLF then
        Delete(FCurLine, Length(FCurLine) - LineBreakLength + 1, LineBreakLength);
    finally
      TmpLines2.Free;
    end;
  finally
    TmpLines.Free;
  end;
end;

procedure TJclInfoWriter.DoWriteCompleteLines;
var
  CRLFPos: Integer;
begin
  CRLFPos := StrLastPos(NativeLineBreak, CurLine);
  if CRLFPos > 0 then
  begin
    PrimWrite(Copy(CurLine, 1, CRLFPos-1));
    Delete(FCurLine, 1, CRLFPos+1);
  end;
end;

procedure TJclInfoWriter.Indent;
begin
  IndentLevel := IndentLevel + 1;
end;

procedure TJclInfoWriter.Outdent;
begin
  IndentLevel := IndentLevel - 1;
end;

procedure TJclInfoWriter.Write(const S: string);
begin
  CurLine := CurLine + S;
  DoWrap;
  DoWriteCompleteLines;
end;

procedure TJclInfoWriter.Writeln(const S: string);
begin
  Write(S + NativeLineBreak);
end;

//=== { TJclInfoStringsWriter } ==============================================

constructor TJclInfoStringsWriter.Create(const AStrings: TStrings;
  const AWrap: Integer);
begin
  inherited Create(AWrap);
  FStrings := AStrings;
end;

procedure TJclInfoStringsWriter.PrimWrite(const S: string);
begin
  Strings.Add(S);
end;

//=== { TJclTypeInfo } =======================================================

constructor TJclTypeInfo.Create(ATypeInfo: PTypeInfo);
begin
  inherited Create;
  FTypeInfo := ATypeInfo;
  FTypeData := {$IFDEF HAS_UNITSCOPE}System.{$ENDIF}TypInfo.GetTypeData(ATypeInfo);
end;

function TJclTypeInfo.GetName: string;
begin
  Result := string(TypeInfo.Name);
end;

function TJclTypeInfo.GetTypeData: PTypeData;
begin
  Result := FTypeData;
end;

function TJclTypeInfo.GetTypeInfo: PTypeInfo;
begin
  Result := FTypeInfo;
end;

function TJclTypeInfo.GetTypeKind: TTypeKind;
begin
  Result := TypeInfo.Kind;
end;

procedure TJclTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  Dest.Writeln(LoadResString(@RsRTTIName) + Name);
  Dest.Writeln(LoadResString(@RsRTTITypeKind) + JclEnumValueToIdent(System.TypeInfo(TTypeKind),
    TypeInfo.Kind));
  Dest.Writeln(Format(LoadResString(@RsRTTITypeInfoAt), [TypeInfo]));
end;

procedure TJclTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  Dest.Write(Format(LoadResString(@RsDeclarationFormat), [Name]));
end;

//=== { TJclOrdinalTypeInfo } ================================================

function TJclOrdinalTypeInfo.GetOrdinalType: TOrdType;
begin
  Result := TypeData.OrdType;
end;

procedure TJclOrdinalTypeInfo.LoadValueFromString(AnObj: TObject;
  const PropName, Value: string);
begin
  SetOrdProp(AnObj, PropName, StrToInt(Value));
end;

procedure TJclOrdinalTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  Dest.Writeln(LoadResString(@RsRTTIOrdinalType) +
    JclEnumValueToIdent(System.TypeInfo(TOrdType), TypeData.OrdType));
end;

function TJclOrdinalTypeInfo.SaveValueToString(AnObj: TObject;
  const PropName: string): string;
begin
  Result := IntToStr(GetOrdProp(AnObj, PropName));
end;

//=== { TJclOrdinalRangeTypeInfo } ===========================================

function TJclOrdinalRangeTypeInfo.GetMinValue: Int64;
begin
  if OrdinalType = otULong then
    Result := Longword(TypeData.MinValue)
  else
    Result := TypeData.MinValue;
end;

function TJclOrdinalRangeTypeInfo.GetMaxValue: Int64;
begin
  if OrdinalType = otULong then
    Result := Longword(TypeData.MaxValue)
  else
    Result := TypeData.MaxValue;
end;

procedure TJclOrdinalRangeTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  Dest.Writeln(LoadResString(@RsRTTIMinValue) + IntToStr(MinValue));
  Dest.Writeln(LoadResString(@RsRTTIMaxValue) + IntToStr(MaxValue));
end;

procedure TJclOrdinalRangeTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
const
  cRange = '..';
begin
  Dest.Write(Name + ' = ');
  if TypeInfo.Kind in [tkChar, tkWChar] then
  begin
    if (MinValue < Ord(' ')) or (MinValue > Ord('~')) then
      Dest.Write('#' + IntToStr(MinValue) + cRange)
    else
      Dest.Write('''' + Chr(Byte(MinValue)) + '''' + cRange);
    if (MaxValue < Ord(' ')) or (MaxValue > Ord('~')) then
      Dest.Write('#' + IntToStr(MaxValue))
    else
      Dest.Write('''' + Chr(Byte(MaxValue)) + '''');
  end
  else
    Dest.Write(IntToStr(MinValue) + '..' + IntToStr(MaxValue));
  Dest.Writeln('; // ' + JclEnumValueToIdent(System.TypeInfo(TOrdType), TypeData.OrdType));
end;

//=== { TJclEnumerationTypeInfo } ============================================

function TJclEnumerationTypeInfo.GetBaseType: IJclEnumerationTypeInfo;
begin
  if TypeData.BaseType{$IFDEF BORLAND}^{$ENDIF} = TypeInfo then
    Result := Self
  else
    Result := TJclEnumerationTypeInfo.Create(TypeData.BaseType{$IFDEF BORLAND}^{$ENDIF});
end;

function TJclEnumerationTypeInfo.GetNames(const I: Integer): string;
var
  Base: IJclEnumerationTypeInfo;
  Idx: Integer;
  P: ^ShortString;
begin
  Base := BaseType;
  Idx := I;
  P := @Base.TypeData.NameList;
  while Idx <> 0 do
  begin
    Inc(TJclAddr(P), Length(P^) + 1);
    Dec(Idx);
  end;
  Result := string(P^);
end;

function TJclEnumerationTypeInfo.GetUnitName: string;
var
  I: Integer;
  P: ^ShortString;
begin
  if BaseType.TypeInfo = TypeInfo then
  begin
    I := MaxValue - MinValue;
    P := @TypeData.NameList;
    while I >= 0 do
    begin
      Inc(TJclAddr(P), Length(P^) + 1);
      Dec(I);
    end;
    Result := string(P^);
  end
  else
    Result := string(TypeData.NameList);
end;

function TJclEnumerationTypeInfo.IndexOfName(const Name: string): Integer;
begin
  Result := MaxValue;
  while (Result >= MinValue) and
        not AnsiSameText(Name, Names[Result]) do
    Dec(Result);
  if Result < MinValue then
    Result := -1;
end;

procedure TJclEnumerationTypeInfo.LoadValueFromString(AnObj: TObject;
  const PropName, Value: string);
begin
  SetEnumProp(AnObj, PropName, Value);
end;

procedure TJclEnumerationTypeInfo.WriteTo(const Dest: IJclInfoWriter);
var
  Idx: Integer;
  Prefix: string;
begin
  inherited WriteTo(Dest);
  Dest.Writeln(LoadResString(@RsRTTIUnitName) + GetUnitName);
  Dest.Write(LoadResString(@RsRTTINameList));
  Prefix := '(';
  for Idx := MinValue to MaxValue do
  begin
    Dest.Write(Prefix + Names[Idx]);
    Prefix := ', ';
  end;
  Dest.Writeln(')');
end;

function TJclEnumerationTypeInfo.SaveValueToString(AnObj: TObject;
  const PropName: string): string;
begin
  Result := GetEnumProp(AnObj, PropName);
end;

procedure TJclEnumerationTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  Prefix: string;
  I: Integer;
begin
  if Name[1] <> '.' then
    Dest.Write(Name + ' = ');
  if BaseType.TypeInfo = TypeInfo then
  begin
    Dest.Write('(');
    Prefix := '';
    for I := MinValue to MaxValue do
    begin
      Dest.Write(Prefix + Names[I]);
      Prefix := ', ';
    end;
    Dest.Write(')');
  end
  else
    Dest.Write(Names[MinValue] + ' .. ' + Names[MaxValue]);
  if Name[1] <> '.' then
  begin
    Dest.Write('; // ' + JclEnumValueToIdent(System.TypeInfo(TOrdType), TypeData.OrdType));
    Dest.Writeln('');
  end;
end;

//=== { TJclSetTypeInfo } ====================================================

function TJclSetTypeInfo.GetBaseType: IJclOrdinalTypeInfo;
begin
  Result := JclTypeInfo(TypeData.CompType{$IFDEF BORLAND}^{$ENDIF}) as IJclOrdinalTypeInfo;
end;

procedure TJclSetTypeInfo.LoadValueFromString(AnObj: TObject; const PropName,
  Value: string);
begin
  SetSetProp(AnObj, PropName, Value);
end;

procedure TJclSetTypeInfo.GetAsList(const Value; const WantRanges: Boolean;
  const Strings: TStrings);
var
  BaseInfo: IJclOrdinalRangeTypeInfo;
  FirstBit: Byte;
  LastBit: Byte;
  Bit: Byte;
  StartBit: Integer;

  procedure AddRange;
  var
    FirstOrdNum: Int64;
    LastOrdNum: Int64;
    OrdNum: Int64;
  begin
    FirstOrdNum := (StartBit - FirstBit) + BaseInfo.MinValue;
    LastOrdNum := (Bit - 1 - FirstBit) + BaseInfo.MinValue;
    if WantRanges and (LastOrdNum <> FirstOrdNum) then
    begin
      if BaseInfo.TypeKind = tkEnumeration then
        Strings.Add((BaseInfo as IJclEnumerationTypeInfo).Names[FirstOrdNum] +
          ' .. ' + (BaseInfo as IJclEnumerationTypeInfo).Names[LastOrdNum])
      else
        Strings.Add(IntToStr(FirstOrdNum) + ' .. ' + IntToStr(LastOrdNum));
    end
    else
    begin
      OrdNum := FirstOrdNum;
      while OrdNum <= LastOrdNum do
      begin
        if BaseInfo.TypeKind = tkEnumeration then
          Strings.Add((BaseInfo as IJclEnumerationTypeInfo).Names[OrdNum])
        else
          Strings.Add(IntToStr(OrdNum));
        Inc(OrdNum);
      end;
    end;
  end;

begin
  BaseInfo := BaseType as IJclOrdinalRangeTypeInfo;
  FirstBit := BaseInfo.MinValue mod 8;
  LastBit := BaseInfo.MaxValue - (BaseInfo.MinValue - FirstBit);
  Bit := FirstBit;
  StartBit := -1;
  Strings.BeginUpdate;
  try
    while Bit <= LastBit do
    begin
      if TestBitBuffer(Value, Bit) then
      begin
        if StartBit = -1 then
          StartBit := Bit;
      end
      else
      begin
        if StartBit <> -1 then
        begin
          AddRange;
          StartBit := -1;
        end;
      end;
      Inc(Bit);
    end;
    if StartBit <> -1 then
      AddRange;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJclSetTypeInfo.SetAsList(out Value; const Strings: TStrings);
var
  BaseInfo: IJclOrdinalRangeTypeInfo;
  FirstBit: Integer;
  I: Integer;
  FirstIdent: string;
  LastIdent: string;
  RangePos: Integer;
  FirstOrd: Int64;
  LastOrd: Int64;
  CurOrd: Integer;

  procedure ClearValue;
  var
    LastBit: Integer;
    ByteCount: Integer;
  begin
    LastBit := BaseInfo.MaxValue - BaseInfo.MinValue + 1 + FirstBit;
    ByteCount := (LastBit - FirstBit) div 8;
    if LastBit mod 8 <> 0 then
      Inc(ByteCount);
    ResetMemory(Value, ByteCount);
  end;

begin
  BaseInfo := BaseType as IJclOrdinalRangeTypeInfo;
  FirstBit := BaseInfo.MinValue mod 8;
  ClearValue;
  Strings.BeginUpdate;
  try
  for I := 0 to Strings.Count - 1 do
    begin
      if Trim(Strings[I]) <> '' then
      begin
        FirstIdent := Trim(Strings[I]);
        RangePos := Pos('..', FirstIdent);
        if RangePos > 0 then
        begin
          LastIdent := Trim(StrRestOf(FirstIdent, RangePos + 2));
          FirstIdent := Trim(Copy(FirstIdent, 1, RangePos - 1));
        end
        else
          LastIdent := FirstIdent;
        if BaseInfo.TypeKind = tkEnumeration then
        begin
          FirstOrd := (BaseInfo as IJclEnumerationTypeInfo).IndexOfName(FirstIdent);
          LastOrd := (BaseInfo as IJclEnumerationTypeInfo).IndexOfName(LastIdent);
          if FirstOrd = -1 then
            raise EJclRTTIError.CreateResFmt(@RsRTTIUnknownIdentifier, [FirstIdent]);
          if LastOrd = -1 then
            raise EJclRTTIError.CreateResFmt(@RsRTTIUnknownIdentifier, [LastIdent]);
        end
        else
        begin
          FirstOrd := StrToInt(FirstIdent);
          LastOrd := StrToInt(LastIdent);
        end;
        Dec(FirstOrd, BaseInfo.MinValue);
        Dec(LastOrd, BaseInfo.MinValue);
        for CurOrd := FirstOrd to LastOrd do
          SetBitBuffer(Value, CurOrd + FirstBit);
      end;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJclSetTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  Dest.Writeln(LoadResString(@RsRTTIBasedOn));
  Dest.Indent;
  try
    BaseType.WriteTo(Dest);
  finally
    Dest.Outdent;
  end;
end;

function TJclSetTypeInfo.SaveValueToString(AnObj: TObject;
  const PropName: string): string;
begin
  Result := GetSetProp(AnObj, PropName);
end;

procedure TJclSetTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  Base: IJclOrdinalTypeInfo;
  BaseEnum: IJclEnumerationTypeInfo;
begin
  if Name[1] <> '.' then
    Dest.Write(Name + ' = set of ');
  Base := BaseType;

  if Base.Name[1] = '.' then
  begin
    if Base.QueryInterface(IJclEnumerationTypeInfo, BaseEnum) = S_OK then
      BaseEnum.DeclarationTo(Dest)
    else
      Dest.Write(LoadResString(@RsRTTITypeError));
  end
  else
    Dest.Write(Base.Name);
  if Name[1] <> '.' then
  begin
    Dest.Write('; // ' + JclEnumValueToIdent(System.TypeInfo(TOrdType), TypeData.OrdType));
    Dest.Writeln('');
  end;
end;

//=== { TJclFloatTypeInfo } ==================================================

function TJclFloatTypeInfo.GetFloatType: TFloatType;
begin
  Result := TypeData.FloatType;
end;

procedure TJclFloatTypeInfo.LoadValueFromString(AnObj: TObject; const PropName,
  Value: string);
begin
  SetFloatProp(AnObj, PropName, StrToFloat(Value));
end;

procedure TJclFloatTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  Dest.Writeln(LoadResString(@RsRTTIFloatType) +
    JclEnumValueToIdent(System.TypeInfo(TFloatType), TypeData.FloatType));
end;

function TJclFloatTypeInfo.SaveValueToString(AnObj: TObject;
  const PropName: string): string;
begin
  Result := FloatToStr(GetFloatProp(AnObj, PropName));
end;

procedure TJclFloatTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  S: string;
  FT: TFloatType;
begin
  FT := FloatType;
  S := StrRestOf(JclEnumValueToIdent(System.TypeInfo(TFloatType), FT), 3);
  Dest.Writeln(Name + ' = type ' + S + ';');
end;

//=== { TJclStringTypeInfo } =================================================

function TJclStringTypeInfo.GetMaxLength: Integer;
begin
  if FTypeInfo^.Kind = tkString then
    Result := TypeData.MaxLength
  else
    Result := 0;
end;

procedure TJclStringTypeInfo.LoadValueFromString(AnObj: TObject; const PropName,
  Value: string);
begin
  SetStrProp(AnObj, PropName, Value);
end;

procedure TJclStringTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  if FTypeInfo^.Kind = tkString then
    Dest.Writeln(LoadResString(@RsRTTIMaxLen) + IntToStr(MaxLength));
end;

function TJclStringTypeInfo.SaveValueToString(AnObj: TObject;
  const PropName: string): string;
begin
  Result := GetStrProp(AnObj, PropName);
end;

procedure TJclStringTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  if Name[1] <> '.' then
    Dest.Write(Name + ' = ');

  {$IFDEF SUPPORTS_UNICODE_STRING}
  if FTypeInfo^.Kind = tkUString then
    Dest.Write('UnicodeString')
  else
  {$ENDIF SUPPORTS_UNICODE_STRING}
  if FTypeInfo^.Kind = tkLString then
    Dest.Write('AnsiString')
  else
  if FTypeInfo^.Kind = tkWString then
    Dest.Write('WideString')
  else
    Dest.Write('string[' + IntToStr(MaxLength) + ']');

  if Name[1] <> '.' then
    Dest.Writeln(';');
end;

//=== { TJclPropInfo } =======================================================

constructor TJclPropInfo.Create(const APropInfo: PPropInfo);
begin
  inherited Create;
  FPropInfo := APropInfo;
end;

function TJclPropInfo.GetPropInfo: PPropInfo;
begin
  Result := FPropInfo;
end;

function TJclPropInfo.GetPropType: IJclTypeInfo;
begin
  Result := JclTypeInfo(PropInfo.PropType{$IFDEF BORLAND}^{$ENDIF});
end;

function TJclPropInfo.GetReader: Pointer;
begin
  Result := PropInfo.GetProc;
end;

function TJclPropInfo.GetWriter: Pointer;
begin
  Result := PropInfo.SetProc;
end;

function TJclPropInfo.GetStoredProc: Pointer;
begin
  Result := PropInfo.StoredProc;
end;

function TJclPropInfo.GetIndex: Integer;
begin
  Result := PropInfo.Index;
end;

function TJclPropInfo.GetDefault: Longint;
begin
  Result := PropInfo.Default;
end;

function TJclPropInfo.GetNameIndex: Smallint;
begin
  Result := PropInfo.NameIndex;
end;

function TJclPropInfo.GetName: string;
begin
  Result := string(PropInfo.Name);
end;

function TJclPropInfo.GetSpecKind(const Value: TJclAddr): TJclPropSpecKind;
var
  P: Integer;
begin
  {$IFDEF CPU32}
  P := Value shr 24;
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  P := Value shr 56;
  {$ENDIF CPU64}
  case P of
    $00:
      if Value < 2 then
        Result := pskConstant
      else
        Result := pskStaticMethod;
    $FE:
      Result := pskVirtualMethod;
    $FF:
      Result := pskField;
  else
    Result := pskStaticMethod;
  end;
end;

function TJclPropInfo.GetSpecValue(const Value: TJclAddr): TJclAddr;
begin
  case GetSpecKind(Value) of
    pskStaticMethod, pskConstant:
      Result := Value;
    pskVirtualMethod:
      {$IFDEF CPU32}
      Result := Value and $0000FFFF;
      {$ENDIF CPU32}
      {$IFDEF CPU64}
      Result := Value and $0000FFFFFFFFFFFF;
      {$ENDIF CPU64}
    pskField:
      {$IFDEF CPU32}
      Result := Value and $00FFFFFF;
      {$ENDIF CPU32}
      {$IFDEF CPU64}
      Result := Value and $00FFFFFFFFFFFFFF;
      {$ENDIF CPU64}
  else
    Result := 0;
  end;
end;

function TJclPropInfo.GetReaderType: TJclPropSpecKind;
begin
  Result := GetSpecKind(TJclAddr(Reader));
end;

function TJclPropInfo.GetWriterType: TJclPropSpecKind;
begin
  Result := GetSpecKind(TJclAddr(Writer));
end;

function TJclPropInfo.GetStoredType: TJclPropSpecKind;
begin
  Result := GetSpecKind(TJclAddr(StoredProc));
end;

function TJclPropInfo.GetReaderValue: TJclAddr;
begin
  Result := GetSpecValue(TJclAddr(Reader));
end;

function TJclPropInfo.GetWriterValue: TJclAddr;
begin
  Result := GetSpecValue(TJclAddr(Writer));
end;

function TJclPropInfo.GetStoredValue: TJclAddr;
begin
  Result := GetSpecValue(TJclAddr(StoredProc));
end;

function TJclPropInfo.IsStored(const AInstance: TObject): Boolean;
begin
  Result := IsStoredProp(AInstance, FPropInfo);
end;

procedure TJclPropInfo.LoadValueFromString(AnObj: TObject; const Value: string);
var
  APropType: IJclTypeInfo;
  AValueInfo: IJclValueTypeInfo;
begin
  APropType := PropType;
  if Supports(APropType, IJclValueTypeInfo, AValueInfo) then
    AValueInfo.LoadValueFromString(AnObj, Name, Value)
  else
    raise EJclRTTIError.CreateResFmt(@RsRTTINoStringValue, [Name, APropType.Name]);
end;

function TJclPropInfo.SaveValueToString(AnObj: TObject): string;
var
  APropType: IJclTypeInfo;
  AValueInfo: IJclValueTypeInfo;
begin
  APropType := PropType;
  if Supports(APropType, IJclValueTypeInfo, AValueInfo) then
    Result := AValueInfo.SaveValueToString(AnObj, Name)
  else
    raise EJclRTTIError.CreateResFmt(@RsRTTINoStringValue, [Name, APropType.Name]);
end;

function TJclPropInfo.HasDefault: Boolean;
begin
  Result := Longword(Default) <> $80000000;
end;

function TJclPropInfo.HasIndex: Boolean;
begin
  Result := Longword(Index) <> $80000000;
end;

//=== { TJclObjPropInfo } ====================================================

constructor TJclObjPropInfo.Create(const APropInfo: PPropInfo;
  const APrefix: string; AInstance: TObject);
begin
  inherited Create(APropInfo);
  FPrefix := APrefix;
  FInstance := AInstance;
end;

function TJclObjPropInfo.GetAbsoluteName: string;
begin
  if FPrefix <> '' then
    Result := FPrefix + '.' + Name
  else
    Result := Name;
end;

function TJclObjPropInfo.GetInstance: TObject;
begin
  Result := FInstance;
end;

function TJclObjPropInfo.IsStored: Boolean;
begin
  Result := IsStoredProp(FInstance, Name);
end;

procedure TJclObjPropInfo.LoadValueFromString(const Value: string);
begin
  LoadValueFromString(FInstance, Value);
end;

function TJclObjPropInfo.SaveValueToString: string;
begin
  Result := SaveValueToString(FInstance);
end;

//=== { TJclClassTypeInfo } ==================================================

function TJclClassTypeInfo.GetClassRef: TClass;
begin
  Result := TypeData.ClassType;
end;

function TJclClassTypeInfo.GetParent: IJclClassTypeInfo;
begin
  if (TypeData.ParentInfo <> nil) {$IFDEF BORLAND}and (TypeData.ParentInfo^ <> nil){$ENDIF BORLAND} then
    Result := JclTypeInfo(TypeData.ParentInfo{$IFDEF BORLAND}^{$ENDIF}) as IJclClassTypeInfo
  else
    Result := nil;
end;

function TJclClassTypeInfo.GetTotalPropertyCount: Integer;
begin
  Result := TypeData.PropCount;
end;

function TJclClassTypeInfo.GetPropertyCount: Integer;
var
  PropData: ^TPropData;
begin
  PropData := @TypeData.UnitName;
  Inc(TJclAddr(PropData), 1 + Length(GetUnitName));
  Result := PropData.PropCount;
end;

function TJclClassTypeInfo.GetProperties(const PropIdx: Integer): IJclPropInfo;
var
  PropData: ^TPropData;
  Prop: PPropInfo;
  Idx: Integer;
  RecSize: Integer;
begin
  PropData := @TypeData.UnitName;
  Inc(TJclAddr(PropData), 1 + Length(GetUnitName));
  if PropIdx + 1 > PropData.PropCount then
    Result := Parent.Properties[PropIdx - PropData.PropCount]
  else
  begin
    Prop := PPropInfo(PropData);
    Inc(TJclAddr(Prop), 2);
    if PropIdx > 0 then
    begin
      RecSize := SizeOf(TPropInfo) - SizeOf(ShortString);
      Idx := PropIdx;
      while Idx > 0 do
      begin
        Inc(TJclAddr(Prop), RecSize);
        Inc(TJclAddr(Prop), 1 + PByte(Prop)^);
        Dec(Idx);
      end;
    end;
    Result := TJclPropInfo.Create(Prop);
  end;
end;

function TJclClassTypeInfo.GetPropNames(const Name: string): IJclPropInfo;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(TypeInfo, Name);
  if PropInfo <> nil then
    Result := TJclPropInfo.Create(PropInfo)
  else
    Result := nil;
end;

function TJclClassTypeInfo.GetUnitName: string;
begin
  Result := string(TypeData.UnitName);
end;

procedure TJclClassTypeInfo.LoadValueFromString(AnObj: TObject; const PropName,
  Value: string);
var
  DotPos: Integer;
  BaseObj: TObject;
  Prefix: string;
  ValueInfo: IJclValueTypeInfo;
begin
  DotPos := CharPos(PropName, '.');
  if DotPos = 0 then
    raise EJclRTTIError.CreateResFmt(@RsRTTINoStringValue, [PropName, Name]);
  Prefix := StrLeft(PropName, DotPos - 1);
  BaseObj := GetObjectProp(AnObj, Prefix);
  if Assigned(BaseObj) and Supports(PropNames[Prefix], IJclValueTypeInfo, ValueInfo) then
    ValueInfo.LoadValueFromString(BaseObj, StrRestOf(PropName, DotPos + 1), Value)
  else
    raise EJclRTTIError.CreateResFmt(@RsRTTINoStringValue, [PropName, Name]);
end;

function TJclClassTypeInfo.SaveValueToString(AnObj: TObject;
  const PropName: string): string;
var
  DotPos: Integer;
  BaseObj: TObject;
  Prefix: string;
  ValueInfo: IJclValueTypeInfo;
begin
  DotPos := CharPos(PropName, '.');
  if DotPos = 0 then
    raise EJclRTTIError.CreateResFmt(@RsRTTINoStringValue, [PropName, Name]);
  Prefix := StrLeft(PropName, DotPos - 1);
  BaseObj := GetObjectProp(AnObj, Prefix);
  if Assigned(BaseObj) and Supports(PropNames[Prefix], IJclValueTypeInfo, ValueInfo) then
    Result := ValueInfo.SaveValueToString(BaseObj, StrRestOf(PropName, DotPos + 1))
  else
    raise EJclRTTIError.CreateResFmt(@RsRTTINoStringValue, [PropName, Name]);
end;

procedure TJclClassTypeInfo.WriteTo(const Dest: IJclInfoWriter);
const
  cFmt1 = '[%s %d]';
  cFmt2 = '[%s %s $%p]';
  cFmt3 = '[%s=%s]';
  cFmt4 = '[%s=%s $%p]';
var
  I: Integer;
  Prop: IJclPropInfo;
begin
  inherited WriteTo(Dest);
  Dest.Writeln(LoadResString(@RsRTTIClassName) + ClassRef.ClassName);
  Dest.Writeln(LoadResString(@RsRTTIParent) + Parent.ClassRef.ClassName);
  Dest.Writeln(LoadResString(@RsRTTIUnitName) + GetUnitName);
  Dest.Writeln(LoadResString(@RsRTTIPropCount) + IntToStr(PropertyCount) + ' (' +
    IntToStr(TotalPropertyCount) + ')');
  Dest.Indent;
  try
    for I := 0 to PropertyCount-1 do
    begin
      Prop := Properties[I];
      Dest.Writeln(Prop.Name + ': ' + Prop.PropType.Name);
      Dest.Indent;
      try
        if Prop.HasIndex then
          Dest.Writeln(Format(cFmt1, [LoadResString(@RsRTTIIndex), Prop.Index]));
        if Prop.HasDefault then
          Dest.Writeln(Format(cFmt1, [LoadResString(@RsRTTIDefault), Prop.Default]));
        case Prop.ReaderType of
          pskStaticMethod:
            Dest.Writeln(Format(cFmt2, [LoadResString(@RsRTTIPropRead), LoadResString(@RsRTTIStaticMethod),
              Pointer(Prop.ReaderValue)]));
          pskField:
            Dest.Writeln(Format(cFmt2, [LoadResString(@RsRTTIPropRead), LoadResString(@RsRTTIField),
              Pointer(Prop.ReaderValue)]));
          pskVirtualMethod:
            Dest.Writeln(Format(cFmt2, [LoadResString(@RsRTTIPropRead), LoadResString(@RsRTTIVirtualMethod),
              Pointer(Prop.ReaderValue)]));
        end;
        case Prop.WriterType of
          pskStaticMethod:
            Dest.Writeln(Format(cFmt2, [LoadResString(@RsRTTIPropWrite), LoadResString(@RsRTTIStaticMethod),
              Pointer(Prop.WriterValue)]));
          pskField:
            Dest.Writeln(Format(cFmt2, [LoadResString(@RsRTTIPropWrite), LoadResString(@RsRTTIField),
              Pointer(Prop.WriterValue)]));
          pskVirtualMethod:
            Dest.Writeln(Format(cFmt2, [LoadResString(@RsRTTIPropWrite), LoadResString(@RsRTTIVirtualMethod),
              Pointer(Prop.WriterValue)]));
        end;
        case Prop.StoredType of
          pskConstant:
            if Boolean(Prop.StoredValue) then
              Dest.Writeln(Format(cFmt3, [LoadResString(@RsRTTIPropStored), LoadResString(@RsRTTITrue)]))
            else
              Dest.Writeln(Format(cFmt3, [LoadResString(@RsRTTIPropStored), LoadResString(@RsRTTIFalse)]));
          pskStaticMethod:
            Dest.Writeln(Format(cFmt4, [LoadResString(@RsRTTIPropStored), LoadResString(@RsRTTIStaticMethod),
              Pointer(Prop.StoredValue)]));
          pskField:
            Dest.Writeln(Format(cFmt4, [LoadResString(@RsRTTIPropStored), LoadResString(@RsRTTIField),
              Pointer(Prop.StoredValue)]));
          pskVirtualMethod:
            Dest.Writeln(Format(cFmt4, [LoadResString(@RsRTTIPropStored), LoadResString(@RsRTTIVirtualMethod),
              Pointer(Prop.StoredValue)]));
        end;
      finally
        Dest.Outdent;
      end;
    end;
  finally
    Dest.Outdent;
  end;
end;

procedure TJclClassTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  IntfTbl: PInterfaceTable;
  I: Integer;
  Prop: IJclPropInfo;
begin
  if (Parent <> nil) and
     not AnsiSameText(Parent.Name, 'TObject') then
  begin
    Dest.Write(Name + ' = class(' + Parent.Name);
    IntfTbl := ClassRef.GetInterfaceTable;
    if IntfTbl <> nil then
      for I := 0 to IntfTbl.EntryCount-1 do
        {$IFDEF FPC}if IntfTbl.Entries[I].IID <> nil then{$ENDIF FPC}
        Dest.Write(', [''' + JclGUIDToString(IntfTbl.Entries[I].IID{$IFDEF FPC}^{$ENDIF}) + ''']');
    Dest.Writeln(') // unit ' + GetUnitName);
  end
  else
    Dest.Writeln(Name + ' = class // unit ' + GetUnitName);
  if PropertyCount > 0 then
  begin
    Dest.Writeln('published');
    Dest.Indent;
    try
      for I := 0 to PropertyCount-1 do
      begin
        Prop := Properties[I];
        Dest.Write('property ' + Prop.Name + ': ' +  Prop.PropType.Name);
        if Prop.HasIndex then
          Dest.Write(Format(' index %d', [Prop.Index]));

        case Prop.ReaderType of
          pskStaticMethod:
            Dest.Write(Format(' read [static method $%p]', [Pointer(Prop.ReaderValue)]));
          pskField:
            Dest.Write(Format(' read [field $%p]', [Pointer(Prop.ReaderValue)]));
          pskVirtualMethod:
            Dest.Write(Format(' read [virtual method $%p]', [Pointer(Prop.ReaderValue)]));
        end;

        case Prop.WriterType of
          pskStaticMethod:
            Dest.Write(Format(' write [static method $%p]', [Pointer(Prop.WriterValue)]));
          pskField:
            Dest.Write(Format(' write [field $%p]', [Pointer(Prop.WriterValue)]));
          pskVirtualMethod:
            Dest.Write(Format(' write [virtual method $%p]', [Pointer(Prop.WriterValue)]));
        end;

        case Prop.StoredType of
          pskConstant:
            if Boolean(Prop.StoredValue) then
              Dest.Write(' stored = True')
            else
              Dest.Write(' stored = False');
          pskStaticMethod:
            Dest.Write(Format(' stored = [static method $%p]', [Pointer(Prop.StoredValue)]));
          pskField:
            Dest.Write(Format(' stored = [field $%p]', [Pointer(Prop.StoredValue)]));
          pskVirtualMethod:
            Dest.Write(Format(' stored = [virtual method $%p]', [Pointer(Prop.StoredValue)]));
        end;
        if Prop.HasDefault then
          Dest.Write(' default ' + IntToStr(Prop.Default));
        Dest.Writeln(';');
      end;
    finally
      Dest.Outdent;
    end;
  end;
  Dest.Writeln('end;');
end;

//=== { TJclObjClassTypeInfo } ===============================================

constructor TJclObjClassTypeInfo.Create(const ATypeInfo: PTypeInfo;
  const APrefix: string; AInstance: TObject);
begin
  inherited Create(ATypeInfo);
  FPrefix := APrefix;
  FInstance := AInstance;
end;

function TJclObjClassTypeInfo.GetInstance: TObject;
begin
  Result := FInstance;
end;

function TJclObjClassTypeInfo.GetObjProperties(
  const PropIdx: Integer): IJclObjPropInfo;
var
  PropData: ^TPropData;
  Prop: PPropInfo;
  Idx: Integer;
  RecSize: Integer;
begin
  PropData := @TypeData.UnitName;
  Inc(TJclAddr(PropData), 1 + Length(GetUnitName));
  Prop := PPropInfo(PropData);
  Inc(TJclAddr(Prop), 2);
  if PropIdx > 0 then
  begin
    RecSize := SizeOf(TPropInfo) - SizeOf(ShortString);
    Idx := PropIdx;
    while Idx > 0 do
    begin
      Inc(TJclAddr(Prop), RecSize);
      Inc(TJclAddr(Prop), 1 + PByte(Prop)^);
      Dec(Idx);
    end;
  end;
  Result := TJclObjPropInfo.Create(Prop, FPrefix, FInstance);
end;

function TJclObjClassTypeInfo.GetObjPropNames(
  const Name: string): IJclObjPropInfo;
var
  PropInfo: PPropInfo;
  DotPos: Integer;
  Prefix, Suffix: string;
  SubClassTypeInfo: IJclObjClassTypeInfo;
  AInstance: TObject;
begin
  DotPos := CharPos(Name, '.');
  if DotPos > 0 then
  begin
    Prefix := StrLeft(Name, DotPos - 1);
    Suffix := StrRestOf(Name, DotPos + 1);
    PropInfo := GetPropInfo(TypeInfo, Prefix);
    if (PropInfo <> nil) and (PropInfo.PropType^.Kind = tkClass) then
    begin
      if FPrefix <> '' then
        Prefix := FPrefix + '.' + Prefix;
      AInstance := GetObjectProp(FInstance, PropInfo);
      if AInstance <> nil then
      begin
        SubClassTypeInfo := TJclObjClassTypeInfo.Create(PropInfo.PropType{$IFDEF BORLAND}^{$ENDIF}, Prefix, AInstance);
        Result := SubClassTypeInfo.ObjPropNames[Suffix];
      end
      else
        Result := nil;
    end
    else
      Result := nil;
  end
  else
  begin
    PropInfo := GetPropInfo(TypeInfo, Name);
    if PropInfo <> nil then
      Result := TJclObjPropInfo.Create(PropInfo, FPrefix, FInstance)
    else
      Result := nil;
  end;
end;

procedure TJclObjClassTypeInfo.LoadValueFromString(const PropName,
  Value: string);
var
  ObjPropInfo: IJclObjPropInfo;
begin
  ObjPropInfo := GetObjPropNames(PropName);
  if Assigned(ObjPropInfo) then
    ObjPropInfo.LoadValueFromString(Value)
  else
    raise EJclRTTIError.CreateResFmt(@RsRTTINoStringValue, [Name, PropName]);
end;

function TJclObjClassTypeInfo.SaveValueToString(const PropName: string): string;
var
  ObjPropInfo: IJclObjPropInfo;
begin
  ObjPropInfo := GetObjPropNames(PropName);
  if Assigned(ObjPropInfo) then
    Result := ObjPropInfo.SaveValueToString
  else
    raise EJclRTTIError.CreateResFmt(@RsRTTINoStringValue, [Name, PropName]);
end;

//=== { TJclEventParamInfo } =================================================

constructor TJclEventParamInfo.Create(const AParam: Pointer);
begin
  inherited Create;
  FParam := AParam;
end;

function TJclEventParamInfo.GetFlags: TParamFlags;
type
  PParamFlags = ^TParamFlags;
begin
  Result := PParamFlags(Param)^;
end;

function TJclEventParamInfo.GetName: string;
var
  PName: PShortString;
begin
  PName := Param;
  Inc(TJclAddr(PName));
  Result := string(PName^);
end;

function TJclEventParamInfo.GetRecSize: Integer;
begin
  Result := 3 + Length(Name) + Length(TypeName);
end;

function TJclEventParamInfo.GetTypeName: string;
var
  PName: PShortString;
begin
  PName := Param;
  Inc(TJclAddr(PName));
  Inc(TJclAddr(PName), PByte(PName)^ + 1);
  Result := string(PName^);
end;

function TJclEventParamInfo.GetParam: Pointer;
begin
  Result := FParam;
end;

//=== { TJclEventTypeInfo } ==================================================

function TJclEventTypeInfo.GetMethodKind: TMethodKind;
begin
  Result := TypeData.MethodKind;
end;

function TJclEventTypeInfo.GetParameterCount: Integer;
begin
  Result := TypeData.ParamCount;
end;

function TJclEventTypeInfo.GetParameters(const ParamIdx: Integer): IJclEventParamInfo;
var
  I: Integer;
  Param: Pointer;
begin
  Result := nil;
  Param := @TypeData.ParamList[0];
  I := ParamIdx;
  while I >= 0 do
  begin
    Result := TJclEventParamInfo.Create(Param);
    Inc(TJclAddr(Param), Result.RecSize);
    Dec(I);
  end;
end;

function TJclEventTypeInfo.GetResultTypeName: string;
var
  LastParam: IJclEventParamInfo;
  ResPtr: PShortString;
begin
  if MethodKind = mkFunction then
  begin
    if ParameterCount > 0 then
    begin
      LastParam := Parameters[ParameterCount-1];
      ResPtr := Pointer(TJclAddr(LastParam.Param) + TJclAddr(LastParam.RecSize));
    end
    else
      ResPtr := @TypeData.ParamList[0];
    Result := string(ResPtr^);
  end
  else
    Result := '';
end;

procedure TJclEventTypeInfo.WriteTo(const Dest: IJclInfoWriter);
var
  I: Integer;
  Param: IJclEventParamInfo;
  ParamFlags: TParamFlags;
begin
  inherited WriteTo(Dest);
  Dest.Writeln(LoadResString(@RsRTTIMethodKind) +
    JclEnumValueToIdent(System.TypeInfo(TMethodKind), TypeData.MethodKind));
  Dest.Writeln(LoadResString(@RsRTTIParamCount) + IntToStr(ParameterCount));
  Dest.Indent;
  try
    for I := 0 to ParameterCount-1 do
    begin
      if I > 0 then
        Dest.Writeln('');
      Param := Parameters[I];
      ParamFlags := Param.Flags;
      Dest.Writeln(LoadResString(@RsRTTIName) + Param.Name);
      Dest.Writeln(LoadResString(@RsRTTIType) + Param.TypeName);
      Dest.Writeln(LoadResString(@RsRTTIFlags) +
        JclSetToStr(System.TypeInfo(TParamFlags), ParamFlags, True, False));
    end;
  finally
    Dest.Outdent;
  end;
  if MethodKind = mkFunction then
    Dest.Writeln(LoadResString(@RsRTTIReturnType) + ResultTypeName);
end;

procedure TJclEventTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  Prefix: string;
  I: Integer;
  Param: IJclEventParamInfo;
begin
  Dest.Write(Name + ' = ');
  if MethodKind = mkFunction then
    Dest.Write('function')
  else
    Dest.Write('procedure');
  Prefix := '(';
  for I := 0 to ParameterCount-1 do
  begin
    Dest.Write(Prefix);
    Prefix := '; ';
    Param := Parameters[I];
    if pfVar in Param.Flags then
      Dest.Write(LoadResString(@RsRTTIVar))
    else
    if pfConst in Param.Flags then
      Dest.Write(LoadResString(@RsRTTIConst))
    else
    if pfOut in Param.Flags then
      Dest.Write(LoadResString(@RsRTTIOut));
    Dest.Write(Param.Name);
    if Param.TypeName <> '' then
    begin
      Dest.Write(': ');
      if pfArray in Param.Flags then
        Dest.Write(LoadResString(@RsRTTIArrayOf));
      if AnsiSameText(Param.TypeName, 'TVarRec') and (pfArray in Param.Flags) then
        Dest.Write(TrimRight(LoadResString(@RsRTTIConst)))
      else
        Dest.Write(Param.TypeName);
    end;
  end;
  if ParameterCount <> 0 then
    Dest.Write(')');
  if MethodKind = mkFunction then
    Dest.Write(': ' + ResultTypeName);
  Dest.Writeln(' of object;');
end;

//=== { TJclInterfaceTypeInfo } ==============================================

function TJclInterfaceTypeInfo.GetParent: IJclInterfaceTypeInfo;
begin
  if (TypeData.IntfParent <> nil) {$IFDEF BORLAND}and (TypeData.IntfParent^ <> nil){$ENDIF BORLAND} then
    Result := JclTypeInfo(TypeData.IntfParent{$IFDEF BORLAND}^{$ENDIF}) as IJclInterfaceTypeInfo
  else
    Result := nil;
end;

function TJclInterfaceTypeInfo.GetFlags: TIntfFlagsBase;
begin
  Result := TypeData.IntfFlags;
end;

const
  NullGUID: TGUID = '{00000000-0000-0000-0000-000000000000}';

function TJclInterfaceTypeInfo.GetGUID: TGUID;
begin
  if ifHasGuid in Flags then
    Result := TypeData.Guid
  else
    Result := NullGUID;
end;

function TJclInterfaceTypeInfo.GetPropertyCount: Integer;
var
  PropData: ^TPropData;
begin
  PropData := @TypeData.IntfUnit;
  Inc(TJclAddr(PropData), 1 + Length(GetUnitName));
  Result := PropData.PropCount;
end;

function TJclInterfaceTypeInfo.GetUnitName: string;
begin
  Result := string(TypeData.IntfUnit);
end;

procedure TJclInterfaceTypeInfo.WriteTo(const Dest: IJclInfoWriter);
var
  IntfFlags: TIntfFlagsBase;
begin
  inherited WriteTo(Dest);
  if ifHasGuid in Flags then
    Dest.Writeln(LoadResString(@RsRTTIGUID) + JclGuidToString(GUID));
  IntfFlags := Flags;
  Dest.Writeln(LoadResString(@RsRTTIFlags) + JclSetToStr(System.TypeInfo(TIntfFlagsBase),
    IntfFlags, True, False));
  Dest.Writeln(LoadResString(@RsRTTIUnitName) + GetUnitName);
  if Parent <> nil then
    Dest.Writeln(LoadResString(@RsRTTIParent) + Parent.Name);
  Dest.Writeln(LoadResString(@RsRTTIPropCount) + IntToStr(PropertyCount));
end;

procedure TJclInterfaceTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  Dest.Write(Name + ' = ');
  if ifDispInterface in Flags then
    Dest.Write('dispinterface')
  else
    Dest.Write('interface');
  if (Parent <> nil) and not (ifDispInterface in Flags) and not
      AnsiSameText(Parent.Name, 'IUnknown') then
    Dest.Write('(' + Parent.Name + ')');
  Dest.Writeln(' // unit ' + GetUnitName);
  Dest.Indent;
  try
    if ifHasGuid in Flags then
      Dest.Writeln('[''' + JclGuidToString(GUID) + ''']');
  finally
    Dest.Outdent;
    Dest.Writeln('end;');
  end;
end;

//=== { TJclInt64TypeInfo } ==================================================

function TJclInt64TypeInfo.GetMinValue: Int64;
begin
  Result := TypeData.MinInt64Value;
end;

procedure TJclInt64TypeInfo.LoadValueFromString(AnObj: TObject; const PropName,
  Value: string);
begin
  SetInt64Prop(AnObj, PropName, StrToInt(Value));
end;

function TJclInt64TypeInfo.GetMaxValue: Int64;
begin
  Result := TypeData.MaxInt64Value;
end;

procedure TJclInt64TypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  Dest.Writeln(LoadResString(@RsRTTIMinValue) + IntToStr(MinValue));
  Dest.Writeln(LoadResString(@RsRTTIMaxValue) + IntToStr(MaxValue));
end;

function TJclInt64TypeInfo.SaveValueToString(AnObj: TObject;
  const PropName: string): string;
begin
  Result := IntToStr(GetInt64Prop(AnObj, PropName));
end;

procedure TJclInt64TypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  Dest.Writeln(Name + ' = ' + IntToStr(MinValue) + ' .. ' + IntToStr(MaxValue) + ';');
end;

//=== { TJclDynArrayTypeInfo } ===============================================

function TJclDynArrayTypeInfo.GetElementSize: Longint;
begin
  Result := TypeData.elSize;
end;

function TJclDynArrayTypeInfo.GetElementType: IJclTypeInfo;
begin
  if TypeData.elType = nil then
  begin
    if TypeData.elType2 <> nil then
      Result := JclTypeInfo(TypeData.elType2^)
    else
      Result := nil;
  end
  else
    Result := JclTypeInfo(TypeData.elType^);
end;

function TJclDynArrayTypeInfo.GetElementsNeedCleanup: Boolean;
begin
  Result := TypeData.elType <> nil;
end;

function TJclDynArrayTypeInfo.GetVarType: Integer;
begin
  Result := TypeData.varType;
end;

function TJclDynArrayTypeInfo.GetUnitName: string;
begin
  Result := string(TypeData.DynUnitName);
end;

procedure TJclDynArrayTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  Dest.Writeln(LoadResString(@RsRTTIElSize) + IntToStr(ElementSize));
  if ElementType = nil then
    Dest.Writeln(LoadResString(@RsRTTIElType) + RsRTTITypeError)
  else
  if ElementType.Name[1] <> '.' then
    Dest.Writeln(LoadResString(@RsRTTIElType) + ElementType.Name)
  else
  begin
    Dest.Writeln(LoadResString(@RsRTTIElType));
    Dest.Indent;
    try
      ElementType.WriteTo(Dest);
    finally
      Dest.Outdent;
    end;
  end;
  Dest.Write(LoadResString(@RsRTTIElNeedCleanup));
  if ElementsNeedCleanup then
    Dest.Writeln(LoadResString(@RsRTTITrue))
  else
    Dest.Writeln(LoadResString(@RsRTTIFalse));
  Dest.Writeln(LoadResString(@RsRTTIVarType) + IntToStr(VarType));
  Dest.Writeln(LoadResString(@RsRTTIUnitName) + GetUnitName);
end;

procedure TJclDynArrayTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  if Name[1] <> '.' then
    Dest.Write(Name + ' = ' + LoadResString(@RsRTTIArrayOf))
  else
    Dest.Write(LoadResString(@RsRTTIArrayOf));
  if ElementType = nil then
    Dest.Write(LoadResString(@RsRTTITypeError))
  else
  if ElementType.Name[1] = '.' then
    ElementType.DeclarationTo(Dest)
  else
    Dest.Write(ElementType.Name);
  if Name[1] <> '.' then
    Dest.Writeln('; // Unit ' + GetUnitName);
end;

//=== Typeinfo retrieval =====================================================

function JclTypeInfo(ATypeInfo: PTypeInfo): IJclTypeInfo;
begin
  case ATypeInfo.Kind of
    tkInteger, tkChar, tkWChar:
      Result := TJclOrdinalRangeTypeInfo.Create(ATypeInfo);
    tkEnumeration:
      Result := TJclEnumerationTypeInfo.Create(ATypeInfo);
    tkSet:
      Result := TJclSetTypeInfo.Create(ATypeInfo);
    tkFloat:
      Result := TJclFloatTypeInfo.Create(ATypeInfo);
    tkString,
    tkLString,
    {$IFDEF SUPPORTS_UNICODE_STRING}
    tkUString,
    {$ENDIF SUPPORTS_UNICODE_STRING}
    tkWString:
      Result := TJclStringTypeInfo.Create(ATypeInfo);
    tkClass:
      Result := TJclClassTypeInfo.Create(ATypeInfo);
    tkMethod:
      Result := TJclEventTypeInfo.Create(ATypeInfo);
    tkInterface:
      Result := TJclInterfaceTypeInfo.Create(ATypeInfo);
    tkInt64:
      Result := TJclInt64TypeInfo.Create(ATypeInfo);
    tkDynArray:
      Result := TJclDynArrayTypeInfo.Create(ATypeInfo);
  else
    Result := TJclTypeInfo.Create(ATypeInfo);
  end;
end;

//=== User generated type info managment =====================================

var
  TypeList: TThreadList;

type
  PTypeItem = ^TTypeItem;
  TTypeItem = record
    TypeInfo: PTypeInfo;
    RefCount: Integer;
  end;

procedure FreeTypeData(const TypeInfo: PTypeInfo);
var
  TD: PTypeData;
begin
  TD := GetTypeData(TypeInfo);
  if TypeInfo.Kind = tkSet then
    RemoveTypeInfo(TD^.CompType{$IFDEF BORLAND}^{$ENDIF})
  else
  if (TypeInfo.Kind = tkEnumeration) and (TD^.BaseType{$IFDEF BORLAND}^{$ENDIF} <> TypeInfo) then
    RemoveTypeInfo(GetTypeData(TypeInfo)^.BaseType{$IFDEF BORLAND}^{$ENDIF});
  FreeMem(GetTypeData(TypeInfo)^.BaseType);
  FreeMem(TypeInfo);
end;

procedure AddType(const TypeInfo: PTypeInfo);
var
  Item: PTypeItem;
begin
  New(Item);
  try
    Item.TypeInfo := TypeInfo;
    Item.RefCount := 1;
    TypeList.Add(Item);
  except
    Dispose(Item);
    raise;
  end;
end;

procedure DeleteType(const TypeItem: PTypeItem);
begin
  FreeTypeData(TypeItem.TypeInfo);
  TypeList.Remove(TypeItem);
  Dispose(TypeItem);
end;

procedure DoRefType(const TypeInfo: PTypeInfo; Add: Integer);
var
  I: Integer;
  List: TList;
begin
  List := TypeList.LockList;
  try
    I := List.Count-1;
    while (I >= 0) and (PTypeItem(List[I]).TypeInfo <> TypeInfo) do
      Dec(I);
    if I > -1 then
      Inc(PTypeItem(List[I]).RefCount, Add);
  finally
    TypeList.UnlockList;
  end;
end;

procedure ReferenceType(const TypeInfo: PTypeInfo);
begin
  DoRefType(TypeInfo, 1);
end;

procedure DeReferenceType(const TypeInfo: PTypeInfo);
begin
  DoRefType(TypeInfo, -1);
end;

procedure ClearInfoList;
var
  L: TList;
begin
  L := TypeList.LockList;
  try
    while L.Count > 0 do
      RemoveTypeInfo(PTypeItem(L[L.Count-1])^.TypeInfo);
  finally
    TypeList.UnlockList;
  end;
end;

procedure NewInfoItem(const TypeInfo: PTypeInfo);
begin
  TypeList.Add(TypeInfo);
end;

procedure RemoveTypeInfo(TypeInfo: PTypeInfo);
var
  I: Integer;
  List: TList;
  Item: PTypeItem;
begin
  Item := nil;
  List := TypeList.LockList;
  try
    I := List.Count-1;
    while (I >= 0) and (PTypeItem(List[I]).TypeInfo <> TypeInfo) do
      Dec(I);
    if I > -1 then
      Item := PTypeItem(List[I]);
  finally
    TypeList.UnlockList;
  end;
  if Item <> nil then
  begin
    Dec(Item.RefCount);
    if Item.RefCount <= 0 then
      DeleteType(Item);
  end;
end;

//=== Enumerations ===========================================================

function JclEnumValueToIdent(TypeInfo: PTypeInfo;
  const Value): string;
var
  MinEnum: Integer;
  MaxEnum: Integer;
  EnumVal: Int64;
  OrdType: TOrdType;
begin
  OrdType := GetTypeData(TypeInfo).OrdType;
  MinEnum := GetTypeData(TypeInfo).MinValue;
  MaxEnum := GetTypeData(TypeInfo).MaxValue;
  case OrdType of
    otSByte:
      EnumVal := Smallint(Value);
    otUByte:
      EnumVal := Byte(Value);
    otSWord:
      EnumVal := Shortint(Value);
    otUWord:
      EnumVal := Word(Value);
    otSLong:
      EnumVal := Integer(Value);
    otULong:
      EnumVal := Longword(Value);
  else
    EnumVal := 0;
  end;
  // Check range...
  if (EnumVal < MinEnum) or (EnumVal > MaxEnum) then
    Result := Format(LoadResString(@RsRTTIValueOutOfRange),
      [LoadResString(@RsRTTIOrdinal) + IntToStr(EnumVal),
       GetEnumName(TypeInfo, MinEnum), GetEnumName(TypeInfo, MaxEnum)])
  else
    Result := GetEnumName(TypeInfo, EnumVal);
end;

function JclGenerateEnumType(const TypeName: ShortString;
  const Literals: array of string): PTypeInfo;
var
  StringSize: Integer;
  I: Integer;
  TypeData: PTypeData;
  CurName: PShortString;
begin
  StringSize := 0;
  for I := Low(Literals) to High(Literals) do
    StringSize := StringSize + 1 + Length(Literals[I]);
  Result := AllocMem(SizeOf(TTypeInfo) + SizeOf(TOrdType) +
    (2*SizeOf(Integer)) + SizeOf(PPTypeInfo) +
    StringSize + 1);
  try
    with Result^ do
    begin
      Kind := tkEnumeration;
      Name := TypeName;
    end;
    TypeData := GetTypeData(Result);
    TypeData^.BaseType := AllocMem(SizeOf(Pointer));
    if Length(Literals) < 256 then
      TypeData^.OrdType := otUByte
    else
    if Length(Literals) < 65536 then
      TypeData^.OrdType := otUWord
    else
      TypeData^.OrdType := otULong;
    TypeData^.MinValue := 0;
    TypeData^.MaxValue := Length(Literals)-1;
    TypeData^.BaseType{$IFDEF BORLAND}^{$ENDIF} := Result;   // No sub-range: basetype points to itself
    CurName := @TypeData^.NameList;
    for I := Low(Literals) to High(Literals) do
    begin
      CurName^ := ShortString(Literals[I]);
      Inc(TJclAddr(CurName), Length(Literals[I])+1);
    end;
    CurName^ := ''; // Unit name unknown
    AddType(Result);
  except
    try
      ReallocMem(Result, 0);
    except
      Result := nil;
    end;
    raise;
  end;
end;

function JclGenerateEnumTypeBasedOn(const TypeName: ShortString;
  BaseType: PTypeInfo; const PrefixCut: Byte): PTypeInfo;
var
  BaseInfo: IJclTypeInfo;
  BaseKind: TTypeKind;
  Literals: array of string;
  I: Integer;
  S: string;
begin
  BaseInfo := JclTypeInfo(BaseType);
  BaseKind := BaseInfo.TypeKind;
  if BaseInfo.TypeKind <> tkEnumeration then
    raise EJclRTTIError.CreateResFmt(@RsRTTIInvalidBaseType, [BaseInfo.Name,
      JclEnumValueToIdent(System.TypeInfo(TTypeKind), BaseKind)]);
  with BaseInfo as IJclEnumerationTypeInfo do
  begin
    SetLength(Literals, MaxValue - MinValue + 1);
    for I := MinValue to MaxValue do
    begin
      S := Names[I];
      if PrefixCut = PREFIX_CUT_LOWERCASE then
        while (Length(S) > 0) and CharIsLower(S[1]) do
          Delete(S, 1, 1);
      if (PrefixCut > 0) and (PrefixCut < MaxPrefixCut) then
        Delete(S, 1, PrefixCut);
      if S = '' then
        S := Names[I];
      Literals[I- MinValue] := S;
    end;
    if PrefixCut = PREFIX_CUT_EQUAL then
    begin
      S := Literals[High(Literals)];
      I := High(Literals)-1;
      while (I >= 0) and (S > '') do
      begin
        while Copy(Literals[I], 1, Length(S)) <> S do
          Delete(S, Length(S), 1);
        Dec(I);
      end;
      if S > '' then
        for I := Low(Literals) to High(Literals) do
        begin
          Literals[I] := StrRestOf(Literals[I], Length(S));
          if Literals[I] = '' then
            Literals[I] := Names[I + MinValue];
        end;
    end;
  end;
  Result := JclGenerateEnumType(TypeName, Literals);
end;

function JclGenerateSubRange(BaseType: PTypeInfo; const TypeName: string;
  const MinValue, MaxValue: Integer): PTypeInfo;
var
  TypeData: PTypeData;
begin
  Result := AllocMem(SizeOf(TTypeInfo) + SizeOf(TOrdType) +
    (2*SizeOf(Integer)) + SizeOf(PPTypeInfo));
  try
    with Result^ do
    begin
      Kind := BaseType^.Kind;
      Name := ShortString(TypeName);
    end;
    TypeData := GetTypeData(Result);
    TypeData^.OrdType := GetTypeData(BaseType)^.OrdType;
    TypeData^.MinValue := MinValue;
    TypeData^.MaxValue := MaxValue;
    TypeData^.BaseType := AllocMem(SizeOf(Pointer));
    TypeData^.BaseType{$IFDEF BORLAND}^{$ENDIF} := BaseType;
    AddType(Result);
  except
    try
      ReallocMem(Result, 0);
    except
      Result := nil;
    end;
    raise;
  end;
  ReferenceType(BaseType);
end;

//=== Integers ===============================================================

function JclStrToTypedInt(Value: string; TypeInfo: PTypeInfo): Integer;
var
  Conv: TIdentToInt;
  HaveConversion: Boolean;
  Info: IJclTypeInfo;
  RangeInfo: IJclOrdinalRangeTypeInfo;
  TmpVal: Int64;
begin
  if TypeInfo <> nil then
    Conv := FindIdentToInt(TypeInfo)
  else
    Conv := nil;
  Result := 0;
  HaveConversion := (@Conv <> nil) and Conv(Value, Result);
  if not HaveConversion then
  begin
    if TypeInfo <> nil then
    begin
      Info := JclTypeInfo(TypeInfo);
      if Info.QueryInterface(IJclOrdinalRangeTypeInfo, RangeInfo) <> S_OK then
        RangeInfo := nil;
      TmpVal := StrToInt64(Value);
      if (RangeInfo <> nil) and ((TmpVal < RangeInfo.MinValue) or
          (TmpVal > RangeInfo.MaxValue)) then
        raise EConvertError.CreateResFmt(@RsRTTIValueOutOfRange,
          [Value, IntToStr(RangeInfo.MinValue), IntToStr(RangeInfo.MaxValue)]);
      Result := Integer(TmpVal);
    end
    else
      Result := StrToInt(Value)
  end;
end;

function JclTypedIntToStr(Value: Integer; TypeInfo: PTypeInfo): string;
var
  Conv: TIntToIdent;
  HaveConversion: Boolean;
begin
  if TypeInfo <> nil then
    Conv := FindIntToIdent(TypeInfo)
  else
    Conv := nil;
  Result := '';
  HaveConversion := (@Conv <> nil) and Conv(Value, Result);
  if not HaveConversion then
  begin
    if (TypeInfo <> nil) and (GetTypeData(TypeInfo).OrdType = otULong) then
      Result := IntToStr(Int64(Cardinal(Value)))
    else
      Result := IntToStr(Value)
  end;
end;

//=== Sets ===================================================================

function JclSetToList(TypeInfo: PTypeInfo; const Value; const WantBrackets: Boolean; const WantRanges: Boolean;
  const Strings: TStrings): string;
var
  SetType: IJclSetTypeInfo;
  I: Integer;
begin
  I := Strings.Count;
  Result := '';
  SetType := JclTypeInfo(TypeInfo) as IJclSetTypeInfo;
  SetType.GetAsList(Value, WantRanges, Strings);
  for I := I to Strings.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ', ' + Strings[I]
    else
      Result := Result + Strings[I];
  end;
  if WantBrackets then
    Result := '[' + Result + ']';
end;

function JclSetToStr(TypeInfo: PTypeInfo; const Value; const WantBrackets: Boolean; const WantRanges: Boolean): string;
var
  Dummy: TStringList;
begin
  Dummy := TStringList.Create;
  try
    Result := JclSetToList(TypeInfo, Value, WantBrackets, WantRanges, Dummy);
  finally
    Dummy.Free;
  end;
end;

procedure JclStrToSet(TypeInfo: PTypeInfo; var SetVar; const Value: string);
var
  SetInfo: IJclSetTypeInfo;
  S: TStringList;
begin
  SetInfo := JclTypeInfo(TypeInfo) as IJclSetTypeInfo;
  S := TStringList.Create;
  try
    StrToStrings(Value, ',', S);
    if S.Count > 0 then
    begin
      if S[0][1] = '[' then
      begin
        S[0] := Copy(S[0], 2, Length(S[0]));
        S[S.Count-1] := Copy(S[S.Count-1], 1,
          Length(S[S.Count-1]) - 1);
      end;
    end;
    SetInfo.SetAsList(SetVar, S);
  finally
    S.Free;
  end;
end;

procedure JclIntToSet(TypeInfo: PTypeInfo; var SetVar; const Value: Integer);
var
  BitShift: Integer;
  TmpInt64: Int64;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
  CompType: PTypeInfo;
begin
  CompType := GetTypeData(TypeInfo).CompType{$IFDEF BORLAND}^{$ENDIF};
  EnumMin := GetTypeData(CompType).MinValue;
  BitShift := EnumMin mod 8;
  TmpInt64 := Longword(Value) shl BitShift;
  EnumMax := GetTypeData(CompType).MaxValue;
  ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
  Move(TmpInt64, SetVar, ResBytes);
end;

function JclSetToInt(TypeInfo: PTypeInfo; const SetVar): Integer;
var
  BitShift: Integer;
  TmpInt64: Int64;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
  CompType: PTypeInfo;
begin
  CompType := GetTypeData(TypeInfo).CompType{$IFDEF BORLAND}^{$ENDIF};
  EnumMin := GetTypeData(CompType).MinValue;
  EnumMax := GetTypeData(CompType).MaxValue;
  ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
  BitShift := EnumMin mod 8;
  if (EnumMax - EnumMin) > 32 then
    raise EJclRTTIError.CreateResFmt(@RsRTTISetValueOutOfRange,
      [IntToStr(EnumMax - EnumMin) + ' ' + LoadResString(@RsRTTIBits)]);
  Result := 0;
  TmpInt64 := 0;
  Move(SetVar, TmpInt64, ResBytes + 1);
  TmpInt64 := TmpInt64 shr BitShift;
  Move(TmpInt64, Result, ResBytes);
end;

function JclGenerateSetType(BaseType: PTypeInfo;
  const TypeName: ShortString): PTypeInfo;
var
  TypeData: PTypeData;
  ValCount: Integer;
begin
  Result := AllocMem(SizeOf(TTypeInfo) + SizeOf(TOrdType) + SizeOf(PPTypeInfo));
  try
    with Result^ do
    begin
      Kind := tkSet;
      Name := TypeName;
    end;
    with GetTypeData(BaseType)^ do
      ValCount := MaxValue - MinValue + (MinValue mod 8);
    TypeData := GetTypeData(Result);
    case ValCount of
      0..8:
        TypeData^.OrdType := otUByte;
      9..16:
        TypeData^.OrdType := otUWord;
      17..32:
        TypeData^.OrdType := otULong;
      33..64:
        Byte(TypeData^.OrdType) := 8;
      65..128:
        Byte(TypeData^.OrdType) := 16;
      129..256:
        Byte(TypeData^.OrdType) := 32;
    else
      Byte(TypeData^.OrdType) := 255;
    end;
    {$IFDEF BORLAND}
    TypeData^.CompType := AllocMem(SizeOf(Pointer));
    TypeData^.CompType^ := BaseType;
    {$ENDIF BORLAND}
    {$IFDEF FPC}
    TypeData^.CompType := BaseType;
    {$ENDIF FPC}
    AddType(Result);
  except
    try
      ReallocMem(Result, 0);
    except
      Result := nil;
    end;
    raise;
  end;
  ReferenceType(BaseType);
end;

//=== Is/As hooking ==========================================================

// Copied from System.pas (_IsClass function)

function JclIsClass(const AnObj: TObject; const AClass: TClass): Boolean;
asm
        {$IFDEF CPU32}
        // 32 --> EAX AnObj
        //        EDX AClass
        //    <-- AL  Result
        TEST    EAX,EAX
        JE      @@exit
@@loop:
        MOV     EAX,[EAX]
        CMP     EAX,EDX
        JE      @@success
        MOV     EAX,[EAX].vmtParent
        TEST    EAX,EAX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // 64 --> RCX AnObj
        //        RDX AClass
        //    <-- AL  Result
        MOV     RAX,RCX
        TEST    RAX,RAX
        JE      @@exit
@@loop:
        MOV     RAX,[RAX]
        CMP     RAX,RDX
        JE      @@success
        MOV     RAX,[RAX].vmtParent
        TEST    RAX,RAX
        {$ENDIF CPU64}
        JNE     @@loop
        JMP     @@exit
@@success:
        MOV     AL,1
@@exit:
end;

function JclIsClassByName(const AnObj: TObject; const AClass: TClass): Boolean;
var
  CurClass: TClass;
  CurClass2: TClass;
begin
  Result := AnObj <> nil;
  if Result then
  begin
    CurClass := AnObj.ClassType;
    Result := False;
    while not Result and (CurClass <> nil) do
    begin
      Result := CurClass.ClassNameIs(AClass.ClassName);
      if not Result then
        CurClass := CurClass.ClassParent;
    end;
    if CurClass <> nil then
      CurClass := CurClass.ClassParent;
    CurClass2 := AClass.ClassParent;
    while Result and (CurClass <> nil) and (CurClass2 <> nil) do
    begin
      Result := CurClass.ClassNameIs(CurClass2.ClassName);
      if Result then
      begin
        CurClass := CurClass.ClassParent;
        CurClass2 := CurClass2.ClassParent;
      end;
    end;
    Result := Result and (CurClass = CurClass2);
  end;
end;

function JclAsClass(const AnObj: TObject; const AClass: TClass): TObject;
begin
  if (AnObj = nil) or (AnObj is AClass) then
    Result := AnObj
  else
    raise EInvalidCast.CreateRes(@SInvalidCast);
end;

function GetStringPropList(TypeInfo: PTypeInfo; out PropList: PPropList): Integer;
begin
  PropList := nil;
  {$IFDEF SUPPORTS_UNICODE_STRING}
  Result := GetPropList(TypeInfo, [tkUString], PropList);
  if Result > 0 then
  begin
    GetMem(PropList, Result * SizeOf(PropList[0]));
    Result := GetPropList(TypeInfo, [tkUString], PropList);
  end;
  {$ELSE ~SUPPORTS_UNICODE_STRING}
  Result := GetPropList(TypeInfo, [tkLString], PropList);
  if Result > 0 then
  begin
    GetMem(PropList, Result * SizeOf(PropList[0]));
    Result := GetPropList(TypeInfo, [tkLString], PropList);
  end;
  {$ENDIF ~SUPPORTS_UNICODE_STRING}
end;

function GetObjectProperties(AnObj: TObject; Recurse: Boolean): IJclObjPropInfoArray;

  procedure InternalGetObjectProperties(var PropCount: SizeInt; Current: TObject; const Prefix: string);
  var
    I, C: Integer;
    PropList: PPropList;
    SubObject: TObject;
    AbsoluteName: string;
  begin
    if Assigned(Current) then
    begin
      C := GetPropList(Current, PropList);
      try
        for I := 0 to C - 1 do
        begin
          if PropCount = Length(Result) then
            SetLength(Result, Length(Result) * 2);
          Result[PropCount] := TJclObjPropInfo.Create(PropList[I], Prefix, Current);
          Inc(PropCount);

          if Recurse and (PropList[I]^.PropType^.Kind = tkClass) then
          begin
            SubObject := GetObjectProp(Current, PropList[I]);
            if Prefix <> '' then
              AbsoluteName := string(Prefix + '.' + string(PropList[I]^.Name))
            else
              AbsoluteName := string(PropList[I]^.Name);
            InternalGetObjectProperties(PropCount, SubObject, AbsoluteName);
          end;
        end;
      finally
        if C > 0 then
          FreeMem(PropList);
      end;
    end;
  end;

var
  PropCount: SizeInt;
begin
  PropCount := 0;
  SetLength(Result, 16);
  InternalGetObjectProperties(PropCount, AnObj, '');
  SetLength(Result, PropCount);
end;

initialization
  TypeList := TThreadList.Create;
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  ClearInfoList;
  FreeAndNil(TypeList);

end.
