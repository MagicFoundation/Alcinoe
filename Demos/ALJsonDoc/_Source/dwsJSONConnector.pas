{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsJSONConnector;

{$I dws.inc}

interface

uses
   System.Classes, System.SysUtils,
   dwsLanguageExtension, dwsComp, dwsDataContext, dwsExprList, dwsExprs,
   dwsSymbols, dwsCoreExprs, dwsStack, dwsUtils, dwsOperators, dwsUnitSymbols,
   dwsFunctions, dwsJSON, dwsMagicExprs, dwsConnectorSymbols, dwsScriptSource,
   dwsCompilerContext, dwsConvExprs;

type

   // TdwsJSONLibModule
   //
   TdwsJSONLibModule = class (TdwsCustomLangageExtension)
      private
         FLegacyCastsToBoolean : Boolean;

      protected
         function CreateExtension : TdwsLanguageExtension; override;

      published
         property LegacyCastsToBoolean : Boolean read FLegacyCastsToBoolean write FLegacyCastsToBoolean default False;
   end;

   // TdwsJSONLanguageExtension
   //
   TdwsJSONLanguageExtension = class (TdwsLanguageExtension)
      private
         FLegacyCastsToBoolean : Boolean;

      public
         procedure CreateSystemSymbols(table : TSystemSymbolTable); override;
         procedure RegisterSystemOperators(table : TSystemSymbolTable; operators: TOperators); override;

         function StaticSymbols : Boolean; override;

         property LegacyCastsToBoolean : Boolean read FLegacyCastsToBoolean write FLegacyCastsToBoolean;
   end;

   // TdwsJSONConnectorType
   //
   TdwsJSONConnectorType = class (TInterfacedSelfObject, IConnectorType)
      private
         FTable : TSystemSymbolTable;
         FTypJSONVariant : TConnectorSymbol;
         FLowCall, FHighCall, FLengthCall : IConnectorCall;
         FIndexReadCall, FIndexWriteCall : IConnectorCall;
         FTypeNameCall : IConnectorCall;
         FDefinedCall : IConnectorCall;
         FElementNameCall : IConnectorCall;
         FCloneCall : IConnectorCall;
         FExtendCall : IConnectorCall;
         FAddCall : IConnectorCall;
         FDeleteCall : IConnectorCall;
         FAddFromCall : IConnectorCall;
         FSwapCall : IConnectorCall;
         FToStringCall : IConnectorCall;
         FLengthMember : IConnectorMember;
         FLegacyCastsToBoolean : Boolean;

      protected
         function ConnectorCaption : String;
         function AutoVarParams : Boolean;
         function AcceptsParams(const params : TConnectorParamArray) : Boolean;
         function WritableReads(const memberName : String) : Boolean;

         function HasMethod(const methodName : String; const params : TConnectorParamArray;
                            var typSym : TTypeSymbol) : IConnectorCall;
         function HasMember(const memberName : String; var typSym : TTypeSymbol;
                            isWrite : Boolean) : IConnectorMember;
         function HasIndex(const propName : String; const params : TConnectorParamArray;
                           var typSym : TTypeSymbol; isWrite : Boolean) : IConnectorCall;
         function HasEnumerator(var typSym: TTypeSymbol) : IConnectorEnumerator;
         function HasCast(typSym: TTypeSymbol) : IConnectorCast;

      public
         constructor Create(table : TSystemSymbolTable);

         property TypJSONVariant : TConnectorSymbol read FTypJSONVariant write FTypJSONVariant;
         property LegacyCastsToBoolean : Boolean read FLegacyCastsToBoolean write FLegacyCastsToBoolean;
   end;

   TdwsJSONFastCallBase = class (TInterfacedSelfObject, IConnectorCall)
      protected
         class function JSONTypeName(const v : Variant) : String; static;
         class procedure RaiseUnsupportedMethodForType(const methodName : String; const v : Variant); static;

         procedure FastCall(const args : TExprBaseListExec; var result : Variant); virtual; abstract;
         function FastCallInteger(const args : TExprBaseListExec) : Int64;
         function FastCallFloat(const args : TExprBaseListExec) : Double;
   end;

   TdwsJSONLowCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TdwsJSONLengthBase = class (TdwsJSONFastCallBase)
      protected
         class function FastCallLength(const args : TExprBaseListExec) : Integer; static;
   end;

   TdwsJSONHighCall = class (TdwsJSONLengthBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TdwsJSONLengthCall = class (TdwsJSONLengthBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TdwsJSONTypeNameCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TdwsJSONElementNameCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TdwsJSONDefinedCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TdwsJSONCloneCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TdwsJSONExtendCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TdwsJSONAddCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TdwsJSONDeleteCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TdwsJSONAddFromCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TdwsJSONSwapCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TdwsJSONToStringCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TdwsJSONIndexCall = class(TdwsJSONFastCallBase)
      private
         FPropName : UnicodeString;

      public
         constructor Create(const propName : String);

         property CallPropName : UnicodeString read FPropName write FPropName;
   end;

   TdwsJSONIndexReadCall = class(TdwsJSONIndexCall, IConnectorFastCall)
      protected
         function FastCallGetValue(const args : TExprBaseListExec; var base : Variant) : TdwsJSONValue;
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
         function FastCallInteger(const args : TExprBaseListExec) : Int64;
         function FastCallFloat(const args : TExprBaseListExec) : Double;
   end;

   TdwsJSONIndexWriteCall = class(TdwsJSONIndexCall, IConnectorFastCall)
      protected
         procedure FastCall(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TdwsJSONConnectorMember
   //
   TdwsJSONConnectorMember = class(TConnectorFastMember, IConnectorMember, IConnectorFastMember)
      private
         FMemberName : UnicodeString;
         FMemberHash : Cardinal;
         FLegacyCastsToBoolean : Boolean;

      protected
         procedure SetMemberName(const n : UnicodeString);

         function Read(const base : Variant) : TData;
         procedure Write(const base : Variant; const data : TData);
         procedure FastRead(const exec : TdwsExecution; const base : TExprBase; var result : Variant); override;
         procedure FastWrite(const exec : TdwsExecution; const base, value : TExprBase); override;
         function FastReadBoolean(const exec : TdwsExecution; const base : TExprBase) : Boolean; override;
         function FastReadFloat(const exec : TdwsExecution; const base : TExprBase) : Double;

      public
         constructor Create(const memberName : String; legacyCastsToBoolean : Boolean);

         property MemberName : UnicodeString read FMemberName write SetMemberName;
         property LegacyCastsToBoolean : Boolean read FLegacyCastsToBoolean write FLegacyCastsToBoolean;
   end;

   TdwsJSONConnectorLengthMember = class(TdwsJSONConnectorMember)
      protected
         procedure FastRead(const exec : TdwsExecution; const base : TExprBase; var result : Variant); override;
   end;

   // TJSONConnectorSymbol
   //
   TJSONConnectorSymbol = class(TConnectorSymbol)
      protected
         function DoIsCompatible(typSym : TTypeSymbol) : Boolean; override;

      public
         function CreateAssignExpr(context : TdwsCompilerContext; const aScriptPos: TScriptPos;
                                   left : TDataExpr; right : TTypedExpr) : TProgramExpr; override;
         function CreateConvExpr(context : TdwsCompilerContext; const aScriptPos: TScriptPos;
                                 expr : TTypedExpr) : TTypedExpr; override;

   end;

   // TJSONParseMethod
   //
   TJSONParseMethod = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TJSONParseUTF8Method
   //
   TJSONParseUTF8Method = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TJSONParseIntegerArrayMethod
   //
   TJSONParseIntegerArrayMethod = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TJSONParseFloatArrayMethod
   //
   TJSONParseFloatArrayMethod = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TJSONParseStringArrayMethod
   //
   TJSONParseStringArrayMethod = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TJSONNewObject
   //
   TJSONNewObject = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TJSONNewArray
   //
   TJSONNewArray = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TJSONStringifyMethod
   //
   TJSONStringifyMethod = class (TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;

   // TJSONStringifyUTF8Method
   //
   TJSONStringifyUTF8Method = class (TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;

   // TJSONPrettyStringifyMethod
   //
   TJSONPrettyStringifyMethod = class (TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;

   // TJSONSerializeMethod
   //
   TJSONSerializeMethod = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   IBoxedJSONValue = interface
      ['{585B989C-220C-4120-B5F4-2819A0708A80}']
      function Value : TdwsJSONValue;
   end;

   TAssignBoxJSONExpr = class(TAssignExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure TypeCheckAssign(context : TdwsCompilerContext); override;
   end;

   TConvBoxJSONExpr = class(TConvExpr)
      public
         constructor Create(context : TdwsBaseSymbolsContext; const aScriptPos : TScriptPos; expr : TTypedExpr); override;
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;

function BoxedJSONValue(value : TdwsJSONValue): IBoxedJSONValue;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// -----------------------------------------------------------------

uses
   System.Variants,
   dwsConstExprs, dwsJSONScript, dwsDynamicArrays, dwsErrors, dwsUnicode,
   dwsStrings, dwsXXHash, dwsXPlatform, dwsUTF8;

const
   SYS_JSON = 'JSON';
   SYS_JSONVARIANT = 'JSONVariant';
   SYS_JSON_STRINGIFY_UTF8 = 'StringifyUTF8';
   SYS_JSON_PARSE = 'Parse';
   SYS_JSON_PARSE_UTF8 = 'ParseUTF8';
   SYS_JSON_PARSE_INTEGER_ARRAY = 'ParseIntegerArray';
   SYS_JSON_PARSE_FLOAT_ARRAY = 'ParseFloatArray';
   SYS_JSON_PARSE_STRING_ARRAY = 'ParseStringArray';
   SYS_JSON_NEWOBJECT = 'NewObject';
   SYS_JSON_NEWARRAY = 'NewArray';
   SYS_JSON_SERIALIZE = 'Serialize';

type
   TBoxedJSONValue = class (TInterfacedObject,
                            IBoxedJSONValue, IJSONWriteAble,
                            ICoalesceable, IToNumeric, INullable, IToVariant,
                            IGetSelf, IUnknown)
      FValue : TdwsJSONValue;

      constructor Create(wrapped : TdwsJSONValue);
      destructor Destroy; override;

      function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; stdcall;

      function GetSelf : TObject;
      function ScriptTypeName : String;

      function ToString : String; override; final;
      function ToUnicodeString : UnicodeString; virtual;
      function ToFloat : Double;
      function ToInteger : Int64;
      procedure ToVariant(var result : Variant);

      function Value : TdwsJSONValue; inline;

      function IsFalsey : Boolean;
      function IsNull : Boolean;
      function IsDefined : Boolean;
      function IsArray : Boolean;
      function IsNumeric : Boolean;
      function IsString : Boolean;

      procedure WriteToJSON(writer : TdwsJSONWriter);

      class procedure Allocate(wrapped : TdwsJSONValue; var v : Variant); static;
      class procedure AllocateOrGetImmediate(wrapped : TdwsJSONValue; var v : Variant); static;

      class function UnBox(const v : Variant) : TdwsJSONValue; static;
   end;

// Create
//
constructor TBoxedJSONValue.Create(wrapped : TdwsJSONValue);
begin
   FValue:=wrapped;
end;

// Destroy
//
destructor TBoxedJSONValue.Destroy;
begin
   FValue.Free;
end;

// QueryInterface
//
function TBoxedJSONValue.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;
begin
   if IsEqualGUID(IID, IBoxedJSONValue) then begin // D2010 workaround, does not support = testing
      PIUnknown(@Obj)^:=IBoxedJSONValue(Self);
      Result:=0;
   end else Result:=inherited QueryInterface(IID, Obj);
end;

// GetSelf
//
function TBoxedJSONValue.GetSelf : TObject;
begin
   Result:=Self;
end;

// ScriptTypeName
//
function TBoxedJSONValue.ScriptTypeName : String;
begin
   Result := SYS_JSONVARIANT;
end;

// Value
//
function TBoxedJSONValue.Value : TdwsJSONValue;
begin
   Result:=FValue;
end;

// ToString
//
function TBoxedJSONValue.ToString : String;
begin
   Result := String(ToUnicodeString);
end;

// ToUnicodeString
//
function TBoxedJSONValue.ToUnicodeString : UnicodeString;
begin
   if FValue.ValueType = jvtString then
      Result := FValue.AsString
   else Result := FValue.ToUnicodeString;
end;

// ToFloat
//
function TBoxedJSONValue.ToFloat : Double;
begin
   Result := FValue.AsNumber;
end;

// ToInteger
//
function TBoxedJSONValue.ToInteger : Int64;
begin
   Result := FValue.AsInteger;
end;

// ToVariant
//
procedure TBoxedJSONValue.ToVariant(var result : Variant);
begin
   case FValue.ValueType of
      jvtNull : VarSetNull(result);
      jvtString : VarCopySafe(result, FValue.AsString);
      jvtNumber : VarCopySafe(result, FValue.AsNumber);
      jvtInt64 : VarCopySafe(result, FValue.AsInteger);
      jvtBoolean : VarCopySafe(result, FValue.AsBoolean);
   else
      VarCopySafe(result, FValue.ToString);
   end;
end;

// IsFalsey
//
function TBoxedJSONValue.IsFalsey : Boolean;
begin
   Result:=FValue.IsFalsey;
end;

// IsNull
//
function TBoxedJSONValue.IsNull : Boolean;
begin
   Result := FValue.IsNull;
end;

// IsDefined
//
function TBoxedJSONValue.IsDefined : Boolean;
begin
   Result := FValue.IsDefined;
end;

// IsArray
//
function TBoxedJSONValue.IsArray : Boolean;
begin
   Result := FValue.IsArray;
end;

// IsNumeric
//
function TBoxedJSONValue.IsNumeric : Boolean;
begin
   Result := FValue.IsNumber;
end;

// IsString
//
function TBoxedJSONValue.IsString : Boolean;
begin
   Result := FValue.IsString;
end;

// WriteToJSON
//
procedure TBoxedJSONValue.WriteToJSON(writer : TdwsJSONWriter);
begin
   if Value <> nil then
      Value.WriteTo(writer)
   else writer.WriteString('Undefined');
end;

// Allocate
//
class procedure TBoxedJSONValue.Allocate(wrapped : TdwsJSONValue; var v : Variant);
var
   b : TBoxedJSONValue;
begin
   b:=TBoxedJSONValue.Create(wrapped);
   v:=IBoxedJSONValue(b);
end;

// AllocateOrGetImmediate
//
class procedure TBoxedJSONValue.AllocateOrGetImmediate(wrapped : TdwsJSONValue; var v : Variant);
begin
   if wrapped=nil then
      VarClear(v)
   else if wrapped.IsImmediateValue then
      TdwsJSONImmediate(wrapped).GetAsVariant(v)
   else begin
      wrapped.IncRefCount;
      TBoxedJSONValue.Allocate(wrapped, v);
   end;
end;

// UnBox
//
class function TBoxedJSONValue.UnBox(const v : Variant) : TdwsJSONValue;
var
   boxed : IBoxedJSONValue;
begin
   case PVarData(@v)^.VType of
      varUnknown : begin
         boxed:=(IUnknown(PVarData(@v)^.VUnknown) as IBoxedJSONValue);
         if boxed<>nil then
            Result:=boxed.Value
         else Result:=nil;
      end;
   else
      Result := nil;
   end;
end;

function BoxedJsonValue(Value : TdwsJSONValue): IBoxedJSONValue;
begin
   result := TBoxedJSONValue.Create(value);
end;

// ------------------
// ------------------ TdwsJSONLibModule ------------------
// ------------------

// CreateExtension
//
function TdwsJSONLibModule.CreateExtension : TdwsLanguageExtension;
var
   ext : TdwsJSONLanguageExtension;
begin
   ext := TdwsJSONLanguageExtension.Create;
   ext.LegacyCastsToBoolean := LegacyCastsToBoolean;
   Result := ext;
end;

// ------------------
// ------------------ TdwsJSONLanguageExtension ------------------
// ------------------

// CreateSystemSymbols
//
procedure TdwsJSONLanguageExtension.CreateSystemSymbols(table : TSystemSymbolTable);
var
   connType : TdwsJSONConnectorType;
   connSym : TJSONConnectorSymbol;
   jsonObject : TClassSymbol;
begin
   connType := TdwsJSONConnectorType.Create(table);
   connType.LegacyCastsToBoolean := LegacyCastsToBoolean;

   connSym:=TJSONConnectorSymbol.Create(SYS_JSONVARIANT, connType);
   table.AddSymbol(connSym);
   connType.TypJSONVariant:=connSym;

   jsonObject:=TClassSymbol.Create(SYS_JSON, nil);
   jsonObject.InheritFrom(table.TypObject);
   table.AddSymbol(jsonObject);
   jsonObject.IsStatic:=True;
   jsonObject.IsSealed:=True;
   jsonObject.SetNoVirtualMembers;

   TJSONStringifyMethod.Create(
      table, SYS_JSON_STRINGIFY, ['val', SYS_ANY_TYPE], SYS_STRING,
      [iffStateLess, iffStaticMethod], jsonObject
   );
   TJSONStringifyUTF8Method.Create(
      table, SYS_JSON_STRINGIFY_UTF8, ['val', SYS_ANY_TYPE], SYS_STRING,
      [iffStateLess, iffStaticMethod], jsonObject
   );
   TJSONPrettyStringifyMethod.Create(
      table, SYS_JSON_PRETTY_STRINGIFY, ['val', SYS_ANY_TYPE, 'indent='#9, SYS_STRING], SYS_STRING,
      [iffStateLess, iffStaticMethod], jsonObject
   );

   TJSONParseMethod.Create(
      table, SYS_JSON_PARSE, ['str', SYS_STRING], SYS_JSONVARIANT,
      [iffStaticMethod], jsonObject, ''
   );
   TJSONParseUTF8Method.Create(
      table, SYS_JSON_PARSE_UTF8, ['str', SYS_STRING], SYS_JSONVARIANT,
      [iffStaticMethod], jsonObject, ''
   );

   TJSONParseIntegerArrayMethod.Create(
      table, SYS_JSON_PARSE_INTEGER_ARRAY, ['str', SYS_STRING, 'nullValue=0', SYS_INTEGER], 'array of integer',
      [ iffStaticMethod ], jsonObject, ''
   );
   TJSONParseFloatArrayMethod.Create(
      table, SYS_JSON_PARSE_FLOAT_ARRAY, ['str', SYS_STRING], 'array of float',
      [iffStaticMethod], jsonObject, ''
   );
   TJSONParseStringArrayMethod.Create(
      table, SYS_JSON_PARSE_STRING_ARRAY, ['str', SYS_STRING], SYS_ARRAY_OF_STRING,
      [iffStaticMethod], jsonObject, ''
   );

   TJSONNewObject.Create(
      table, SYS_JSON_NEWOBJECT, [], SYS_JSONVARIANT,
      [iffStaticMethod], jsonObject, ''
   );
   TJSONNewArray.Create(
      table, SYS_JSON_NEWARRAY, [], SYS_JSONVARIANT,
      [iffStaticMethod], jsonObject, ''
   );

   TJSONSerializeMethod.Create(
      table, SYS_JSON_SERIALIZE, ['val', SYS_ANY_TYPE], SYS_JSONVARIANT,
      [iffStaticMethod], jsonObject, ''
   );
end;

// RegisterSystemOperators
//
procedure TdwsJSONLanguageExtension.RegisterSystemOperators(table : TSystemSymbolTable; operators: TOperators);
begin
   var connSym := TJSONConnectorSymbol(table.FindLocalOfClass(SYS_JSONVARIANT, TJSONConnectorSymbol));

   operators.RegisterCaster(table.TypInteger, connSym, TConvVarToIntegerExpr, True);
   operators.RegisterCaster(table.TypFloat,   connSym, TConvVarToFloatExpr, True);
   operators.RegisterCaster(table.TypBoolean, connSym, TConvVarToBoolExpr, True);
   operators.RegisterCaster(table.TypString,  connSym, TConvVarToStringExpr, True);
end;

// StaticSymbols
//
function TdwsJSONLanguageExtension.StaticSymbols : Boolean;
begin
   Result:=True;
end;

// ------------------
// ------------------ TdwsJSONConnectorType ------------------
// ------------------

// Create
//
constructor TdwsJSONConnectorType.Create(table : TSystemSymbolTable);
begin
   inherited Create;

   FTable:=table;

   FLowCall:=TdwsJSONLowCall.Create;
   FHighCall:=TdwsJSONHighCall.Create;
   FLengthCall:=TdwsJSONLengthCall.Create;
   FIndexReadCall:=TdwsJSONIndexReadCall.Create('');
   FIndexWriteCall:=TdwsJSONIndexWriteCall.Create('');
   FTypeNameCall:=TdwsJSONTypeNameCall.Create;
   FDefinedCall:=TdwsJSONDefinedCall.Create;
   FElementNameCall:=TdwsJSONElementNameCall.Create;
   FCloneCall:=TdwsJSONCloneCall.Create;
   FExtendCall:=TdwsJSONExtendCall.Create;
   FAddCall:=TdwsJSONAddCall.Create;
   FDeleteCall:=TdwsJSONDeleteCall.Create;
   FAddFromCall := TdwsJSONAddFromCall.Create;
   FSwapCall:=TdwsJSONSwapCall.Create;
   FToStringCall:=TdwsJSONToStringCall.Create;

   FLengthMember:=TdwsJSONConnectorLengthMember.Create('length', False);
end;

// ConnectorCaption
//
function TdwsJSONConnectorType.ConnectorCaption : String;
begin
   Result:='JSON Connector 2.0';
end;

// AutoVarParams
//
function TdwsJSONConnectorType.AutoVarParams : Boolean;
begin
   Result:=False;
end;

// AcceptsParams
//
function TdwsJSONConnectorType.AcceptsParams(const params : TConnectorParamArray) : Boolean;
begin
   Result:=True;
end;

// WritableReads
//
function TdwsJSONConnectorType.WritableReads(const memberName : String) : Boolean;
begin
   Result := False;
end;

// HasMethod
//
function TdwsJSONConnectorType.HasMethod(const methodName : String; const params : TConnectorParamArray;
                                         var typSym : TTypeSymbol) : IConnectorCall;
var
   paramTyp : TTypeSymbol;
   i : Integer;
begin
   if UnicodeSameText(methodName, 'defined') then begin

      Result:=FDefinedCall;
      typSym:=FTable.TypBoolean;
      if Length(params)<>0 then
         raise ECompileException.Create(CPE_NoParamsExpected);

   end else if UnicodeSameText(methodName, 'typename') then begin

      Result:=FTypeNameCall;
      typSym:=FTable.TypString;

      if Length(params)<>0 then
         raise ECompileException.Create(CPE_NoParamsExpected);

   end else if UnicodeSameText(methodName, 'elementname') then begin

      if Length(params)<>1 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(params)]);
      paramTyp:=params[0].TypSym;
      if not paramTyp.UnAliasedTypeIs(TBaseIntegerSymbol) then
         raise ECompileException.CreateFmt(CPE_BadParameterType, [0, SYS_INTEGER, paramTyp.Caption]);

      Result:=FElementNameCall;
      typSym:=FTable.TypString;

   end else if UnicodeSameText(methodName, 'extend') then begin

      if Length(params)<>1 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(params)]);
      paramTyp:=params[0].TypSym;
      if paramTyp.UnAliasedType<>TypJSONVariant then
         raise ECompileException.CreateFmt(CPE_BadParameterType, [0, SYS_JSONVARIANT, paramTyp.Caption]);

      Result:=FExtendCall;
      typSym:=nil;

   end else if UnicodeSameText(methodName, 'add') or UnicodeSameText(methodName, 'push') then begin

      if Length(params)<1 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(params)]);
      for i:=0 to High(params) do begin
         paramTyp:=params[i].TypSym;
         if (not paramTyp.UnAliasedType.IsBaseType) and (paramTyp.ClassType<>TNilSymbol) then
            raise ECompileException.CreateFmt(CPE_BadParameterType, [i, SYS_JSONVARIANT, paramTyp.Caption]);
      end;

      Result:=FAddCall;
      typSym:=FTable.TypInteger;

   end else if UnicodeSameText(methodName, 'delete') then begin

      if Length(params)<>1 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(params)]);
      paramTyp := params[0].TypSym;
      if not ((paramTyp.UnAliasedType is TBaseStringSymbol) or (paramTyp.UnAliasedType is TBaseIntegerSymbol)) then
         raise ECompileException.CreateFmt(CPE_BadParameterType, [0, SYS_STRING + ' or ' + SYS_INTEGER, paramTyp.Caption]);

      Result:=FDeleteCall;
      typSym:=nil;

   end else if UnicodeSameText(methodName, 'addfrom') then begin

      if Length(params)<>1 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(params)]);
      paramTyp := params[0].TypSym;
      if paramTyp.UnAliasedType <> TypJSONVariant then
         raise ECompileException.CreateFmt(CPE_BadParameterType, [0, SYS_JSONVARIANT, paramTyp.Caption]);

      Result:=FAddFromCall;
      typSym:=nil;

   end else if UnicodeSameText(methodName, 'swap') then begin

      if Length(params)<>2 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [2, Length(params)]);
      paramTyp := params[0].TypSym;
      if not (paramTyp.UnAliasedType is TBaseIntegerSymbol) then
         raise ECompileException.CreateFmt(CPE_BadParameterType, [0, SYS_INTEGER, paramTyp.Caption]);

      Result:=FSwapCall;
      typSym:=nil;

   end else begin

      if Length(params)<>0 then
         raise ECompileException.Create(CPE_NoParamsExpected);

      if UnicodeSameText(methodName, 'clone') then begin

         typSym:=TypJSONVariant;
         Result:=FCloneCall;

      end else if UnicodeSameText(methodName, 'tostring') then begin

         typSym:=FTable.TypString;
         Result:=FToStringCall;

      end else begin

         typSym:=FTable.TypInteger;
         if UnicodeSameText(methodName, 'length') then
            Result:=FLengthCall
         else if UnicodeSameText(methodName, 'low') then
            Result:=FLowCall
         else if UnicodeSameText(methodName, 'high') then
            Result:=FHighCall
         else Result:=nil;

      end;

   end;
end;

// HasMember
//
function TdwsJSONConnectorType.HasMember(const memberName : String; var typSym : TTypeSymbol;
                                         isWrite : Boolean) : IConnectorMember;
begin
   typSym := TypJSONVariant;
   if memberName = 'length' then
      Result := FLengthMember
   else Result := TdwsJSONConnectorMember.Create(memberName, LegacyCastsToBoolean);
end;

// HasIndex
//
function TdwsJSONConnectorType.HasIndex(const propName : String; const params : TConnectorParamArray;
                                        var typSym : TTypeSymbol; isWrite : Boolean) : IConnectorCall;
begin
   typSym:=TypJSONVariant;
   if propName='' then begin
      if isWrite then
         Result:=FIndexWriteCall
      else Result:=FIndexReadCall;
   end else begin
      if isWrite then
         Result:=TdwsJSONIndexWriteCall.Create(propName)
      else Result:=TdwsJSONIndexReadCall.Create(propName);
   end;
end;

// HasEnumerator
//
function TdwsJSONConnectorType.HasEnumerator(var typSym: TTypeSymbol) : IConnectorEnumerator;
begin
   Result:=nil;
end;

// HasCast
//
function TdwsJSONConnectorType.HasCast(typSym: TTypeSymbol) : IConnectorCast;
begin
   Result:=nil;
end;

// ------------------
// ------------------ TdwsJSONFastCallBase ------------------
// ------------------

// JSONTypeName
//
class function TdwsJSONFastCallBase.JSONTypeName(const v : Variant) : String;
var
   vt : TdwsJSONValueType;
begin
   case PVarData(@v)^.VType of
      varUnknown :
         vt := TBoxedJSONValue.UnBox(v).ValueType;
      {$ifdef FPC}
      varString :
         vt:=jvtString;
      {$else}
      varUString :
         vt:=jvtString;
      {$endif}
      varDouble :
         vt:=jvtNumber;
      varBoolean :
         vt:=jvtBoolean;
      varNull :
         vt:=jvtNull;
   else
      vt:=jvtUndefined;
   end;
   Result := TdwsJSONValue.ValueTypeStrings[vt];
end;

// RaiseUnsupportedMethodForType
//
class procedure TdwsJSONFastCallBase.RaiseUnsupportedMethodForType(const methodName : String; const v : Variant);
begin
   raise EdwsJSONException.CreateFmt(
      'JSON method %s() unsupported for type %s',
      [ methodName, JSONTypeName(v) ]
   );
end;

// FastCallInteger
//
function TdwsJSONFastCallBase.FastCallInteger(const args : TExprBaseListExec) : Int64;
var
   v : Variant;
begin
   FastCall(args, v);
   Result := VariantToInt64(v);
end;

// FastCallFloat
//
function TdwsJSONFastCallBase.FastCallFloat(const args : TExprBaseListExec) : Double;
var
   v : Variant;
begin
   FastCall(args, v);
   Result := VariantToFloat(v);
end;

// ------------------
// ------------------ TdwsJSONLowCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONLowCall.FastCall(const args : TExprBaseListExec; var result : Variant);
begin
   VarCopySafe(result, 0);
end;

// ------------------
// ------------------ TdwsJSONLengthBase ------------------
// ------------------

// FastCallLength
//
class function TdwsJSONLengthBase.FastCallLength(const args : TExprBaseListExec) : Integer;
var
   base : Variant;
   v : TdwsJSONValue;
begin
   args.EvalAsVariant(0, base);
   case VarType(base) of
      varUnknown : begin
         v := TBoxedJSONValue.UnBox(base);
         Result := v.ElementCount;
      end;
   else
      Result := 0;
   end;
end;

// ------------------
// ------------------ TdwsJSONHighCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONHighCall.FastCall(const args : TExprBaseListExec; var result : Variant);
begin
   VarCopySafe(result, FastCallLength(args)-1);
end;

// ------------------
// ------------------ TdwsJSONLengthCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONLengthCall.FastCall(const args : TExprBaseListExec; var result : Variant);
begin
   VarCopySafe(result, FastCallLength(args));
end;

// ------------------
// ------------------ TdwsJSONTypeNameCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONTypeNameCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base : Variant;
begin
   args.EvalAsVariant(0, base);
   VarCopySafe(result, JSONTypeName(base));
end;

// ------------------
// ------------------ TdwsJSONElementNameCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONElementNameCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base : Variant;
   v : TdwsJSONValue;
begin
   args.EvalAsVariant(0, base);
   v:=TBoxedJSONValue.UnBox(base);
   if v<>nil then
      VarCopySafe(result, v.Names[args.AsInteger[1]])
   else VarCopySafe(result, '');
end;

// ------------------
// ------------------ TdwsJSONDefinedCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONDefinedCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base : Variant;
   v : TdwsJSONValue;
begin
   args.EvalAsVariant(0, base);
   v := TBoxedJSONValue.UnBox(base);
   if v <> nil then
      Result := (v.ValueType <> jvtUndefined)
   else Result := not VarIsEmpty(base);
end;

// ------------------
// ------------------ TdwsJSONCloneCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONCloneCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base : Variant;
   v : TdwsJSONValue;
begin
   args.EvalAsVariant(0, base);
   case VarType(base) of
      varInt64, varDouble, varUString, varBoolean :
         result := base;
   else
      v := TBoxedJSONValue.UnBox(base);
      if v <> nil then
         VarCopySafe(result, IBoxedJSONValue(TBoxedJSONValue.Create(v.Clone)))
      else VarClear(result);
   end;
end;

// ------------------
// ------------------ TdwsJSONExtendCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONExtendCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base, param : Variant;
   v : TdwsJSONValue;
begin
   args.EvalAsVariant(0, base);
   v:=TBoxedJSONValue.UnBox(base);
   if v<>nil then begin
      args.EvalAsVariant(1, param);
      v.Extend(TBoxedJSONValue.UnBox(param));
   end;
end;

// ------------------
// ------------------ TdwsJSONAddCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONAddCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   i : Integer;
   base, param : Variant;
   baseValue, paramValue : TdwsJSONValue;
   pParam : PVarData;
   baseArray : TdwsJSONArray;
begin
   args.EvalAsVariant(0, base);
   baseValue:=TBoxedJSONValue.UnBox(base);
   if baseValue<>nil then begin
      if baseValue.ValueType=jvtArray then begin
         baseArray:=TdwsJSONArray(baseValue);
         for i:=1 to args.Count-1 do begin
            args.EvalAsVariant(i, param);
            pParam:=PVarData(@param);
            case pParam^.VType of
               varInt64 : baseArray.Add(pParam^.VInt64);
               varDouble : baseArray.Add(pParam^.VDouble);
               {$ifdef FPC}
               varString : baseArray.Add(UnicodeString(String(pParam^.VString)));
               {$else}
               varUString : baseArray.Add(String(pParam^.VString));
               {$endif}
               varBoolean : baseArray.Add(pParam^.VBoolean);
               varUnknown : begin
                  if pParam.VUnknown <> nil then begin
                     paramValue := TBoxedJSONValue.UnBox(param);
                     if paramValue.Owner = nil then
                        paramValue.IncRefCount;
                     baseArray.Add(paramValue)
                  end else begin
                     baseArray.AddNull;
                  end;
               end;
               varNull : baseArray.AddNull;
            else
               raise EdwsJSONException.Create('JSON Array Add() unsupported type');
            end;
         end;
         VarCopySafe(result, baseArray.ElementCount);
         Exit;
      end;
   end;
   RaiseUnsupportedMethodForType('Add', base);
end;

// ------------------
// ------------------ TdwsJSONDeleteCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONDeleteCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base : Variant;
   baseValue : TdwsJSONValue;
   name : String;
begin
   args.EvalAsVariant(0, base);
   baseValue := TBoxedJSONValue.UnBox(base);
   case baseValue.ValueType of
      jvtObject : begin
         args.EvalAsString(1, name);
         TdwsJSONObject(baseValue).Delete(name);
      end;
      jvtArray  : begin
         TdwsJSONArray(baseValue).Delete(args.ExprBase[1].EvalAsInteger(args.Exec));
      end;
   else
      raise EdwsJSONException.Create('JSON Object or Array required for Delete method');
   end;
end;

// ------------------
// ------------------ TdwsJSONAddFromCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONAddFromCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base, arg : Variant;
   baseValue, argValue : TdwsJSONValue;
begin
   args.EvalAsVariant(0, base);
   args.EvalAsVariant(1, arg);
   baseValue := TBoxedJSONValue.UnBox(base);
   argValue := TBoxedJSONValue.UnBox(arg);
   if baseValue.ValueType <> jvtArray then
      raise EdwsJSONException.Create('JSON Array required for AddFrom method');
   if argValue.ValueType <> jvtArray then
      raise EdwsJSONException.Create('JSON Array required for AddFrom argument');

   TdwsJSONArray(baseValue).AddFrom(TdwsJSONArray(argValue));
end;

// ------------------
// ------------------ TdwsJSONSwapCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONSwapCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base : Variant;
   baseValue : TdwsJSONValue;
begin
   args.EvalAsVariant(0, base);
   baseValue := TBoxedJSONValue.UnBox(base);
   case baseValue.ValueType of
      jvtArray  : begin
         TdwsJSONArray(baseValue).Swap(args.ExprBase[1].EvalAsInteger(args.Exec),
                                       args.ExprBase[2].EvalAsInteger(args.Exec));
      end;
   else
      raise EdwsJSONException.Create('JSON Array required for Swap method');
   end;
end;

// ------------------
// ------------------ TdwsJSONToStringCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONToStringCall.FastCall(const args : TExprBaseListExec; var result : Variant);
begin
   VarCopySafe(result, '');
   JSONScript.StringifyArgs(args, String(PVarData(@Result)^.VString));
end;

// ------------------
// ------------------ TdwsJSONIndexCall ------------------
// ------------------

// Create
//
constructor TdwsJSONIndexCall.Create(const propName : String);
begin
   inherited Create;
   FPropName := UnicodeString(propName);
end;

// ------------------
// ------------------ TdwsJSONIndexReadCall ------------------
// ------------------

// FastCallGetValue
//
function TdwsJSONIndexReadCall.FastCallGetValue(const args : TExprBaseListExec; var base : Variant) : TdwsJSONValue;
var
   v : TdwsJSONValue;
   idx : Variant;
begin
   args.ExprBase[0].EvalAsVariant(args.Exec, base);
   case PVarData(@base)^.VType of
      varUnknown : begin
         v := TBoxedJSONValue.UnBox(base);
         if v <> nil then begin
            if FPropName<>'' then
               v := v.Items[FPropName];
            args.ExprBase[1].EvalAsVariant(args.Exec, idx);
            Exit(v.Values[idx]);
         end;
      end;
   end;
   Result := nil;
end;

// FastCall
//
procedure TdwsJSONIndexReadCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   v : TdwsJSONValue;
   b : Variant;
begin
   v := FastCallGetValue(args, b);
   if v <> nil then
      TBoxedJSONValue.AllocateOrGetImmediate(v, Result)
   else VarClear(Result);
end;

// FastCallInteger
//
function TdwsJSONIndexReadCall.FastCallInteger(const args : TExprBaseListExec) : Int64;
var
   v : TdwsJSONValue;
   b : Variant;
begin
   v := FastCallGetValue(args, b);
   if v <> nil then
      Result := v.AsInteger
   else Result := 0;
end;

// FastCallFloat
//
function TdwsJSONIndexReadCall.FastCallFloat(const args : TExprBaseListExec) : Double;
var
   v : TdwsJSONValue;
   b : Variant;
begin
   v := FastCallGetValue(args, b);
   if v <> nil then
      Result := v.AsNumber
   else Result := 0;
end;

// ------------------
// ------------------ TdwsJSONIndexWriteCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONIndexWriteCall.FastCall(const args : TExprBaseListExec; var result : Variant);

   procedure RaiseCannotSetItems(const v : Variant);
   begin
      raise EdwsJSONException.CreateFmt('Cannot set items of %s', [ JSONTypeName(v) ]);
   end;

var
   b, val : Variant;
   pVal : PVarData;
   baseValue, argValue : TdwsJSONValue;
begin
   args.ExprBase[0].EvalAsVariant(args.Exec, b);
   if PVarData(@b)^.VType=varUnknown then begin
      baseValue:=TBoxedJSONValue.UnBox(b);
      if FPropName<>'' then
         baseValue:=baseValue.Items[FPropName];
      case baseValue.ValueType of
         jvtObject, jvtArray : ;
      else
         RaiseCannotSetItems(b);
      end;

      args.ExprBase[2].EvalAsVariant(args.Exec, val);
      pVal:=@val;
      case pVal^.VType of
         varUnknown : begin
            argValue:=TBoxedJSONValue.UnBox(val);
            if argValue=nil then
               argValue:=TdwsJSONImmediate.CreateNull
            else if argValue.Owner=nil then
               argValue.IncRefCount;
         end;
         varInt64 : begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsNumber:=pVal^.VInt64;
         end;
         varDouble : begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsNumber:=pVal^.VDouble;
         end;
         {$ifdef FPC}
         varString : begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsString:=UnicodeString(String(pVal^.VString));
         end;
         {$else}
         varUString : begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsString:=String(pVal^.VUString);
         end;
         {$endif}
         varBoolean : begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsBoolean:=pVal^.VBoolean;
         end;
         varNull : begin
            argValue:=TdwsJSONImmediate.CreateNull;
         end;
         varEmpty : begin
            argValue:=TdwsJSONImmediate.Create;
         end;
      else
         if VarIsNumeric(val) then begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsNumber:=val;
         end else raise Exception.CreateFmt('Unsupported assignment for type %d', [ pVal^.VType ]);
      end;
      args.ExprBase[1].EvalAsVariant(args.Exec, val);
      baseValue.Values[val] := argValue;
   end else begin
      RaiseCannotSetItems(b);
   end;
end;

// ------------------
// ------------------ TdwsJSONConnectorMember ------------------
// ------------------

// Create
//
constructor TdwsJSONConnectorMember.Create(const memberName : String; legacyCastsToBoolean : Boolean);
begin
   inherited Create;
   SetMemberName(UnicodeString(memberName));
   FLegacyCastsToBoolean := legacyCastsToBoolean;
end;

// Read
//
function TdwsJSONConnectorMember.Read(const base : Variant) : TData;
begin
   Assert(False);
end;

// Write
//
procedure TdwsJSONConnectorMember.Write(const base : Variant; const data : TData);
begin
   Assert(False);
end;

// SetMemberName
//
procedure TdwsJSONConnectorMember.SetMemberName(const n : UnicodeString);
begin
   FMemberName := n;
   FMemberHash := SimpleStringHash(n);
end;

// FastRead
//
procedure TdwsJSONConnectorMember.FastRead(
      const exec : TdwsExecution; const base : TExprBase; var result : Variant);
var
   b : Variant;
   v : TdwsJSONValue;
begin
   base.EvalAsVariant(exec, b);
   v:=TBoxedJSONValue.UnBox(b);
   if v<>nil then begin
      v:=v.HashedItems[FMemberHash, FMemberName];
      TBoxedJSONValue.AllocateOrGetImmediate(v, result)
   end else VarClear(result);
end;

// FastWrite
//
procedure TdwsJSONConnectorMember.FastWrite(
      const exec : TdwsExecution; const base, value : TExprBase);
var
   b, v : Variant;
   baseValue, dataValue : TdwsJSONValue;
begin
   base.EvalAsVariant(exec, b);
   baseValue := TBoxedJSONValue.UnBox(b);
   case baseValue.ValueType of
      jvtObject : ;
      jvtArray :
         if StrUToInt64(FMemberName, -1) < 0 then
            raise EdwsJSONException.CreateFmt('Invalid array member "%s"', [FMemberName]);
      jvtUndefined :
         raise EdwsJSONException.CreateFmt('Cannot set member "%s" of Undefined', [FMemberName]);
   else
      raise EdwsJSONException.CreateFmt('Cannot set member "%s" of Immediate', [FMemberName]);
   end;

   value.EvalAsVariant(exec, v);
   case PVarData(@v)^.VType of
      varUnknown : begin
         dataValue := TBoxedJSONValue.UnBox(v);
         if dataValue = nil then
            dataValue := TdwsJSONImmediate.CreateNull
         else begin
            if dataValue.Owner = nil then
               dataValue.IncRefCount
            else dataValue.Detach;
         end;
      end;
      varEmpty :
         dataValue := TdwsJSONImmediate.Create;
   else
      dataValue := TdwsJSONImmediate.FromVariant(v);
   end;
   baseValue.HashedItems[FMemberHash, FMemberName] := dataValue;
end;

// FastReadBoolean
//
function TdwsJSONConnectorMember.FastReadBoolean(const exec : TdwsExecution; const base : TExprBase) : Boolean;

   function DoLegacyCast(v : TdwsJSONValue) : Boolean;
   begin
      if v.ValueType = jvtString then
         Result := StringToBoolean(v.AsString)
      else Result := not v.IsFalsey;
   end;

var
   b : Variant;
   v : TdwsJSONValue;
begin
   base.EvalAsVariant(exec, b);
   v := TBoxedJSONValue.UnBox(b);
   if v <> nil then begin
      v := v.HashedItems[FMemberHash, FMemberName];
      if LegacyCastsToBoolean then
         Result := DoLegacyCast(v)
      else Result := not v.IsFalsey;
   end else Result := False;
end;

// FastReadFloat
//
function TdwsJSONConnectorMember.FastReadFloat(const exec : TdwsExecution; const base : TExprBase) : Double;
var
   b : Variant;
   v : TdwsJSONValue;
begin
   base.EvalAsVariant(exec, b);
   v := TBoxedJSONValue.UnBox(b);
   if v <> nil then begin
      v := v.HashedItems[FMemberHash, FMemberName];
      Result := v.AsNumber;
   end else Result := 0;
end;

// ------------------
// ------------------ TdwsJSONConnectorLengthMember ------------------
// ------------------

// FastRead
//
procedure TdwsJSONConnectorLengthMember.FastRead(const exec : TdwsExecution; const base : TExprBase; var result : Variant);
var
   b : Variant;
   v : TdwsJSONValue;
begin
   base.EvalAsVariant(exec, b);
   v:=TBoxedJSONValue.UnBox(b);
   if v<>nil then begin
      case v.ValueType of
         jvtArray : VarCopySafe(result, v.ElementCount);
         jvtString : VarCopySafe(result, Length(v.AsString));
      else
         v:=v.Items[FMemberName];
         TBoxedJSONValue.AllocateOrGetImmediate(v, result)
      end;
   end else VarClear(result);
end;

// ------------------
// ------------------ TJSONConnectorSymbol ------------------
// ------------------

// DoIsCompatible
//
function TJSONConnectorSymbol.DoIsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=   inherited DoIsCompatible(typSym)
           or (typSym.AsFuncSymbol<>nil)
           or (typSym is TRecordSymbol);
end;

// CreateAssignExpr
//
function TJSONConnectorSymbol.CreateAssignExpr(context : TdwsCompilerContext; const aScriptPos: TScriptPos;
                                               left : TDataExpr; right : TTypedExpr) : TProgramExpr;
var
   rightTyp : TTypeSymbol;
   rightTypClass : TClass;
begin
   Result:=nil;
   rightTyp:=right.Typ.BaseType;
   if rightTyp=nil then Exit;

   rightTypClass:=rightTyp.ClassType;
   if rightTypClass=TJSONConnectorSymbol then
      Result:=TAssignExpr.Create(context, aScriptPos, left, right)
   else if rightTypClass.InheritsFrom(TBaseSymbol) then
      Result:=TAssignBoxJSONExpr.Create(context, aScriptPos, left, right);

   if Result=nil then begin
      context.Msgs.AddCompilerErrorFmt(aScriptPos, CPE_AssignIncompatibleTypes,
                                       [right.Typ.Caption, left.Typ.Caption]);
      left.Free;
      right.Free;
   end;
end;

// CreateConvExpr
//
function TJSONConnectorSymbol.CreateConvExpr(context : TdwsCompilerContext; const aScriptPos: TScriptPos;
                                             expr : TTypedExpr) : TTypedExpr;
begin
   if expr.Typ.UnAliasedTypeIs(TJSONConnectorSymbol) then
      Exit(expr);

   if expr.Typ.BaseType.InheritsFrom(TBaseSymbol) then
      Result := TConvBoxJSONExpr.Create(context, aScriptPos, expr)
   else begin
      context.Msgs.AddCompilerErrorFmt(aScriptPos, CPE_AssignIncompatibleTypes,
                                       [ expr.Typ.Caption, Name ]);
      Result := expr;
   end;
end;

// ------------------
// ------------------ TJSONParseMethod ------------------
// ------------------

procedure TJSONParseMethod.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
   json : String;
begin
   args.EvalAsString(0, json);
   if json <> '' then begin
      v:=TdwsJSONValue.ParseString(json);
      if v=nil then
         box:=nil
      else box:=TBoxedJSONValue.Create(v);
      VarCopySafe(result, IBoxedJSONValue(box));
   end else VarClearSafe(result);
end;

// ------------------
// ------------------ TJSONParseUTF8Method ------------------
// ------------------

procedure TJSONParseUTF8Method.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
   json : String;
   jsonUTF8 : RawByteString;
begin
   args.EvalAsString(0, json);
   if json <> '' then begin
      ScriptStringToRawByteString(json, jsonUTF8);
      json := UTF8ToString(jsonUTF8);
      v:=TdwsJSONValue.ParseString(json);
      if v=nil then
         box:=nil
      else box:=TBoxedJSONValue.Create(v);
      VarCopySafe(result, IBoxedJSONValue(box));
   end else VarClearSafe(result);
end;

// ------------------
// ------------------ TJSONParseIntegerArrayMethod ------------------
// ------------------

// DoEvalAsVariant
//
procedure TJSONParseIntegerArrayMethod.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   tokenizer : TdwsJSONParserState;
   values : TSimpleInt64List;
   i : Integer;
   newArray : IScriptDynArray;
   s : String;
begin
   args.EvalAsString(0, s);
   tokenizer := TdwsJSONParserState.Create(s);
   values := TSimpleInt64List.Create;
   try
      tokenizer.ParseIntegerArray(values, args.AsInteger[1]);

      CreateNewDynamicArray((args.Exec as TdwsProgramExecution).CompilerContext.TypInteger, newArray);
      VarCopySafe(result, newArray);
      newArray.ArrayLength := values.Count;

      for i := 0 to newArray.ArrayLength-1 do
         newArray.AsInteger[i] := values[i];
   finally
      values.Free;
      tokenizer.Free;
   end;
end;

// ------------------
// ------------------ TJSONParseFloatArrayMethod ------------------
// ------------------

// DoEvalAsVariant
//
procedure TJSONParseFloatArrayMethod.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   tokenizer : TdwsJSONParserState;
   values : TSimpleDoubleList;
   i : Integer;
   newArray : IScriptDynArray;
   s : String;
begin
   args.EvalAsString(0, s);
   tokenizer:=TdwsJSONParserState.Create(s);
   values:=TSimpleDoubleList.Create;
   try
      tokenizer.ParseNumberArray(values);

      CreateNewDynamicArray((args.Exec as TdwsProgramExecution).CompilerContext.TypFloat, newArray);
      VarCopySafe(result, newArray);
      newArray.ArrayLength := values.Count;

      for i:=0 to newArray.ArrayLength-1 do
         newArray.AsFloat[i] := values[i];
   finally
      values.Free;
      tokenizer.Free;
   end;
end;

// ------------------
// ------------------ TJSONParseStringArrayMethod ------------------
// ------------------

// DoEvalAsVariant
//
procedure TJSONParseStringArrayMethod.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   tokenizer : TdwsJSONParserState;
   values : TUnicodeStringList;
   i : Integer;
   newArray : IScriptDynArray;
   s : String;
begin
   args.EvalAsString(0, s);

   tokenizer := TdwsJSONParserState.Create(s);
   values := TUnicodeStringList.Create;
   try
      tokenizer.ParseStringArray(values);

      CreateNewDynamicArray((args.Exec as TdwsProgramExecution).CompilerContext.TypString, newArray);
      VarCopySafe(result, newArray);
      newArray.ArrayLength := values.Count;

      for i:=0 to values.Count-1 do
         newArray.AsString[i] := values[i];
   finally
      values.Free;
      tokenizer.Free;
   end;
end;

// ------------------
// ------------------ TJSONNewObject ------------------
// ------------------

// DoEvalAsVariant
//
procedure TJSONNewObject.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
begin
   v := TdwsJSONObject.Create;
   box := TBoxedJSONValue.Create(v);
   VarCopySafe(result, IBoxedJSONValue(box));
end;

// ------------------
// ------------------ TJSONNewArray ------------------
// ------------------

// DoEvalAsVariant
//
procedure TJSONNewArray.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
begin
   v := TdwsJSONArray.Create;
   box := TBoxedJSONValue.Create(v);
   VarCopySafe(result, IBoxedJSONValue(box));
end;

// ------------------
// ------------------ TJSONStringifyMethod ------------------
// ------------------

// DoEvalAsString
//
procedure TJSONStringifyMethod.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   JSONScript.StringifyArgs(args, Result);
end;

// ------------------
// ------------------ TJSONStringifyUTF8Method ------------------
// ------------------

// DoEvalAsString
//
procedure TJSONStringifyUTF8Method.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
var
   bufUTF8 : RawByteString;
begin
   JSONScript.StringifyArgs(args, Result);
   bufUTF8 := StringToUTF8(Result);
   RawByteStringToScriptString(bufUTF8, Result);
end;

// ------------------
// ------------------ TJSONPrettyStringifyMethod ------------------
// ------------------

// DoEvalAsString
//
procedure TJSONPrettyStringifyMethod.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   JSONScript.PrettyStringifyArgs(args, Result);
end;

// ------------------
// ------------------ TJSONSerializeMethod ------------------
// ------------------

// DoEvalAsVariant
//
procedure TJSONSerializeMethod.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
   js : String;
begin
   JSONScript.StringifyArgs(args, js);
   v:=TdwsJSONValue.ParseString(js);
   if v=nil then
      box:=nil
   else box:=TBoxedJSONValue.Create(v);
   VarCopySafe(result, IBoxedJSONValue(box));
end;

// ------------------
// ------------------ TAssignBoxJSONExpr ------------------
// ------------------

// EvalNoResult
//
procedure TAssignBoxJSONExpr.EvalNoResult(exec : TdwsExecution);
var
   rv : Variant;
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
begin
   Right.EvalAsVariant(exec, rv);
   if VarIsEmpty(rv) then
      Left.AssignValue(exec, rv)
   else begin
      v:=TdwsJSONImmediate.FromVariant(rv);
      box:=TBoxedJSONValue.Create(v);
      Left.AssignValue(exec, IBoxedJSONValue(box));
   end;
end;

// TypeCheckAssign
//
procedure TAssignBoxJSONExpr.TypeCheckAssign(context : TdwsCompilerContext);
begin
   // nothing
end;

// ------------------
// ------------------ TConvBoxJSONExpr ------------------
// ------------------

// Create
//
constructor TConvBoxJSONExpr.Create(context : TdwsBaseSymbolsContext; const aScriptPos : TScriptPos; expr : TTypedExpr);
begin
   inherited;
   Typ := context.FindType(SYS_JSONVARIANT);
end;

// EvalAsVariant
//
procedure TConvBoxJSONExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   rv : Variant;
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
begin
   Expr.EvalAsVariant(exec, rv);
   if VarIsEmpty(rv) then
      Result := rv
   else begin
      v := TdwsJSONImmediate.FromVariant(rv);
      box := TBoxedJSONValue.Create(v);
      Result := IBoxedJSONValue(box);
   end;
end;

end.
