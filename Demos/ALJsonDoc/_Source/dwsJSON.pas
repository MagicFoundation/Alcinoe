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
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit dwsJSON;

{$I dws.inc}
{$R-}

interface

uses
   System.Classes, System.SysUtils,
   dwsUtils, dwsUnicode;

type

   TdwsJSONArray = class;
   TdwsJSONObject = class;
   TdwsJSONImmediate = class;

   TdwsJSONWriterState = (wsNone, wsObject, wsObjectName, wsObjectValue, wsArray, wsArrayValue, wsDone);
   TdwsJSONWriterOption = (
      woLowerCaseNames,       // lowercase all field names
      woDateTimeAsUnixTime,   // Write date time as numerical unix time
      woNaNNumbersJSON5       // Write NaNs using JSON5 convention, instead of nulls
   );
   TdwsJSONWriterOptions = set of TdwsJSONWriterOption;

   // TdwsJSONWriter
   //
   TdwsJSONWriter = class
      private
         FStream : TWriteOnlyBlockStream;
         FOwnsStream : Boolean;
         FStateStack : TTightStack;
         FState : TdwsJSONWriterState;
         FOptions : TdwsJSONWriterOptions;

      protected
         procedure BeforeWriteImmediate; virtual;
         procedure AfterWriteImmediate;

      public
         constructor Create(aStream : TWriteOnlyBlockStream = nil; aOptions : TdwsJSONWriterOptions = []);
         destructor Destroy; override;

         procedure BeginObject; overload; virtual;
         procedure BeginObject(const aName : UnicodeString); overload; inline;
         procedure EndObject; virtual;

         procedure BeginArray; overload; virtual;
         procedure BeginArray(const aName : UnicodeString); overload; inline;
         procedure EndArray; virtual;

         function  WriteName(const aName : UnicodeString) : TdwsJSONWriter; overload; inline;
         function  WriteNameP(const aName : PWideChar; nbChars : Integer) : TdwsJSONWriter; virtual;
         procedure WriteString(const str : UnicodeString); overload;
         procedure WriteString(const name, str : UnicodeString); overload; inline;
         {$ifdef FPC}
         function  WriteName(const aName : String) : TdwsJSONWriter; overload; inline;
         procedure WriteString(const str : String); overload; inline;
         procedure WriteString(const name, str : String); overload; inline;
         {$endif}
         procedure WriteStringP(const p : PWideChar; nbChars : Integer);
         procedure WriteNumber(const n : Double); overload;
         procedure WriteNumber(const name : UnicodeString; const n : Double); overload; inline;
         procedure WriteInteger(const n : Int64); overload;
         procedure WriteInteger(const name : UnicodeString; const n : Int64); overload; inline;
         procedure WriteBoolean(b : Boolean); overload;
         procedure WriteBoolean(const name : UnicodeString; b : Boolean); overload; inline;
         procedure WriteNull;

         // ISO 8601 Date Time
         procedure WriteDate(dt : TDateTime; utc : Boolean = False);
         procedure WriteUnixTime(dt : TDateTime; utc : Boolean = False);

         procedure WriteStrings(const str : TStrings); overload;
         procedure WriteStrings(const str : TUnicodeStringList); overload;
         procedure WriteStrings(const str : array of UnicodeString); overload;

         procedure WriteJSON(const json : UnicodeString);

         procedure WriteVariant(const v : Variant);

         procedure StoreToUnicodeString(var dest : UnicodeString); inline;

         function ToString : String; override; final;
         function ToUnicodeString : UnicodeString; inline;
         function ToUTF8String : RawByteString; inline;

         property Stream : TWriteOnlyBlockStream read FStream write FStream;
   end;

   IJSONWriteAble = interface
      ['{742CFFF7-3799-4B4B-8610-D7C8A131FCDC}']
      procedure WriteToJSON(writer : TdwsJSONWriter);
   end;

   // TdwsJSONBeautifiedWriter
   //
   TdwsJSONBeautifiedWriter = class (TdwsJSONWriter)
      private
         FSpaces : Integer;
         FSpacesPerIndent : Integer;
         FSpaceCharacter : String;
         FIndents : String;

         procedure WriteIndents;
         procedure EnterIndent;
         procedure LeaveIndent;

      protected
         procedure BeforeWriteImmediate; override;

      public
         constructor Create(aStream : TWriteOnlyBlockStream; initialSpaces, spacesPerIndent : Integer; const spaceCharacter : String = #9);

         procedure BeginObject; override;
         procedure EndObject; override;

         procedure BeginArray; override;
         procedure EndArray; override;

         function  WriteNameP(const aName : PWideChar; nbChars : Integer) : TdwsJSONWriter; override;
   end;

   TdwsJSONDuplicatesOptions = (jdoAccept, jdoOverwrite);

   TdwsJSONValueType = (
      jvtUndefined, jvtNull,
      jvtObject, jvtArray,
      jvtString,
      jvtNumber,
      jvtInt64,      // internal, reported as Number, used to preserve integers larger than 53 bits
      jvtBoolean
   );

   // TdwsJSONParserLocation
   //
   // Used to store internal location state for TdwsJSONParserState
   // Note that it is not used in that class to avoid an indirection overhead
   TdwsJSONParserLocation = record
      Ptr, ColStart : PWideChar;
      Line : Integer;
      TrailCharacter : WideChar;
   end;

   // TdwsJSONParserState
   //
   // Internal utility parser for TdwsJSON, a "light" tokenizer
   TdwsJSONParserState = class
      private
         Str : UnicodeString;
         Ptr, ColStart : PWideChar;
         Line : Integer;
         FTrailCharacter : WideChar;
         DuplicatesOption : TdwsJSONDuplicatesOptions;
         UnifyUnicodeStrings : Boolean;

         function ParseEscapedCharacter : WideChar;

      public
         constructor Create(const aStr : UnicodeString);

         function Location : String;

         property TrailCharacter : WideChar read FTrailCharacter write FTrailCharacter;
         function NeedChar : WideChar; inline;
         function SkipBlanks(currentChar : WideChar) : WideChar; inline;

         function SaveLocation : TdwsJSONParserLocation;
         procedure LoadLocation(const aLocation : TdwsJSONParserLocation);

         procedure ParseJSONUnicodeString(initialChar : WideChar; var result : UnicodeString);
         procedure ParseHugeJSONNumber(
            initialChars : PWideChar; initialCharCount : Integer;
            var result : Double; var jvt : TdwsJSONValueType
         );
         procedure ParseJSONNumber(initialChar : WideChar; var result : Double; var jvt : TdwsJSONValueType);

         // reads from [ to ]
         procedure ParseIntegerArray(dest : TSimpleInt64List; const nullValue : Int64 = 0);
         procedure ParseNumberArray(dest : TSimpleDoubleList);
         procedure ParseStringArray(dest : TUnicodeStringList);
   end;

   // TdwsJSONValue
   //
   TdwsJSONValue = class (TRefCountedObject)
      private
         FRawOwner : NativeUInt; // 3 low order bytes are reserved for immediates

      protected
         function GetOwner : TdwsJSONValue; inline;
         procedure SetOwner(aOwner : TdwsJSONValue); virtual;
         procedure ClearOwner; inline;
         function HasInOwners(obj : TdwsJSONValue) : Boolean;

         property FOwner : TdwsJSONValue read GetOwner write SetOwner;

         procedure DetachChild(child : TdwsJSONValue); virtual;

         function GetValueType : TdwsJSONValueType; virtual; abstract;
         function GetName(index : Integer) : UnicodeString;
         function DoGetName(index : Integer) : UnicodeString; virtual;
         function GetElement(index : Integer) : TdwsJSONValue; inline;
         function DoGetElement(index : Integer) : TdwsJSONValue; virtual;
         procedure SetElement(index : Integer; const value : TdwsJSONValue); inline;
         procedure DoSetElement(index : Integer; const value : TdwsJSONValue); virtual; abstract;
         function GetItem(const name : UnicodeString) : TdwsJSONValue; inline;
         function GetHashedItem(hash : Cardinal; const name : UnicodeString) : TdwsJSONValue; inline;
         function DoGetItem(const name : UnicodeString) : TdwsJSONValue; virtual;
         function DoGetHashedItem(hash : Cardinal; const name : UnicodeString) : TdwsJSONValue; virtual;
         procedure SetItem(const name : UnicodeString; const value : TdwsJSONValue); inline;
         procedure SetHashedItem(hash : Cardinal; const name : UnicodeString; const value : TdwsJSONValue); inline;
         procedure DoSetItem(const name : UnicodeString; const value : TdwsJSONValue); virtual; abstract;
         procedure DoSetHashedItem(hash : Cardinal; const name : UnicodeString; const value : TdwsJSONValue); virtual;
         function DoElementCount : Integer; virtual;
         function GetValue(const index : Variant) : TdwsJSONValue;
         procedure SetValue(const index : Variant; const aValue : TdwsJSONValue);

         function GetIsImmediateValue : Boolean; virtual;

         function GetAsString : UnicodeString; inline;
         procedure SetAsString(const val : UnicodeString); inline;
         function GetIsNull : Boolean; inline;
         procedure SetIsNull(const val : Boolean);
         function GetIsDefined : Boolean; inline;
         function GetAsBoolean : Boolean; inline;
         procedure SetAsBoolean(const val : Boolean); inline;
         function GetAsNumber : Double;
         procedure SetAsNumber(const val : Double); inline;
         function GetIsNaN : Boolean;
         function GetAsInteger : Int64; inline;
         procedure SetAsInteger(const val : Int64); inline;

         procedure DoParse(initialChar : WideChar; parserState : TdwsJSONParserState); virtual; abstract;

         function DoClone : TdwsJSONValue; virtual; abstract;
         procedure DoExtend(other : TdwsJSONValue); virtual; abstract;

         function DoIsFalsey : Boolean; virtual;

         class procedure RaiseJSONException(const msg : String); static;
         class procedure RaiseJSONParseError(const msg : String; c : WideChar = #0); static;

         class function Parse(parserState : TdwsJSONParserState) : TdwsJSONValue; static;

      public
         destructor Destroy; override;
         procedure Release; virtual;

         class function ParseString(const json : UnicodeString;
                                    duplicatesOption : TdwsJSONDuplicatesOptions = jdoOverwrite) : TdwsJSONValue; static;
         class function ParseFile(const fileName : TFileName) : TdwsJSONValue; static;

         function  Clone : TdwsJSONValue;
         procedure Extend(other : TdwsJSONValue);

         procedure WriteTo(writer : TdwsJSONWriter); virtual; abstract;
         procedure WriteToStream(aStream : TStream); overload;
         procedure WriteToStream(aStream : TWriteOnlyBlockStream); overload;

         function  ToString : String; override; final;
         function  ToUnicodeString : UnicodeString;
         function  ToBeautifiedString(initialTabs : Integer = 0; indentTabs : Integer = 1) : UnicodeString;

         procedure Detach;

         property Owner : TdwsJSONValue read GetOwner;
         property Items[const name : UnicodeString] : TdwsJSONValue read GetItem write SetItem;
         property HashedItems[hash : Cardinal; const name : UnicodeString] : TdwsJSONValue read GetHashedItem write SetHashedItem;
         property Names[index : Integer] : UnicodeString read GetName;
         property Elements[index : Integer] : TdwsJSONValue read GetElement write SetElement;
         function ElementCount : Integer; inline;
         property Values[const index : Variant] : TdwsJSONValue read GetValue write SetValue; default;

         function IsImmediateValue : Boolean; inline;
         function Value : TdwsJSONImmediate; inline;
         function ValueType : TdwsJSONValueType; inline;

         function IsFalsey : Boolean; inline;
         function IsArray : Boolean; inline;
         function IsNumber : Boolean; inline;
         function IsString : Boolean; inline;

         procedure Clear;

         property AsString : UnicodeString read GetAsString write SetAsString;
         property IsNull : Boolean read GetIsNull write SetIsNull;
         property IsDefined : Boolean read GetIsDefined;
         property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
         property AsNumber : Double read GetAsNumber write SetAsNumber;
         property IsNaN : Boolean read GetIsNaN;
         property AsInteger : Int64 read GetAsInteger write SetAsInteger;

         const ValueTypeStrings : array [TdwsJSONValueType] of UnicodeString = (
            'Undefined', 'Null', 'Object', 'Array', 'String', 'Number', 'Number', 'Boolean'
            );

         type
            TElementEnumerator = record
               private
                  FIndex, FCountMinus1 : Integer;
                  FOwner : TdwsJSONValue;
               public
                  function MoveNext : Boolean; inline;
                  function GetCurrent : TdwsJSONValue; inline;
                  property Current : TdwsJSONValue read GetCurrent;
            end;
         function GetEnumerator : TElementEnumerator;
   end;

   // does not own its elements
   TdwsJSONValueList = class(TSimpleList<TdwsJSONValue>)
      public
         procedure WriteTo(writer : TdwsJSONWriter);
         function ToString : String; override; final;
         function ToUnicodeString : UnicodeString;
   end;

   TdwsJSONPair = record
      Name : UnicodeString;
      Hash : Cardinal;
      Value : TdwsJSONValue;
   end;
   PdwsJSONPair = ^TdwsJSONPair;
   TdwsJSONPairArray = array [0..MaxInt shr 5] of TdwsJSONPair;
   PdwsJSONPairArray = ^TdwsJSONPairArray;

   // TdwsJSONObject
   //
   TdwsJSONObject = class sealed (TdwsJSONValue)
      private
         FItems : PdwsJSONPairArray;
         FCapacity : Integer;
         FCount : Integer;

      protected
         procedure SetOwner(aOwner : TdwsJSONValue); override;

         procedure Grow;
         procedure SetCapacity(newCapacity : Integer);
         function IndexOfHashedName(hash : Cardinal; const name : UnicodeString) : Integer; inline;
         function IndexOfName(const name : UnicodeString) : Integer;
         function IndexOfValue(const aValue : TdwsJSONValue) : Integer;
         procedure DetachChild(child : TdwsJSONValue); override;
         procedure DetachIndex(i : Integer);

         function GetValueType : TdwsJSONValueType; override;
         function DoGetName(index : Integer) : UnicodeString; override;
         function DoGetElement(index : Integer) : TdwsJSONValue; override;
         procedure DoSetElement(index : Integer; const value : TdwsJSONValue); override;
         function DoGetItem(const name : UnicodeString) : TdwsJSONValue; override;
         function DoGetHashedItem(hash : Cardinal; const name : UnicodeString) : TdwsJSONValue; override;
         procedure DoSetItem(const name : UnicodeString; const value : TdwsJSONValue); override;
         procedure DoSetHashedItem(hash : Cardinal; const name : UnicodeString; const value : TdwsJSONValue); override;
         function DoElementCount : Integer; override;

         procedure DoParse(initialChar : WideChar; parserState : TdwsJSONParserState); override;

         function DoClone : TdwsJSONValue; override;
         procedure DoExtend(other : TdwsJSONValue); override;

      public
         destructor Destroy; override;

         function Clone : TdwsJSONObject;

         procedure Clear;

         procedure Add(const aName : UnicodeString; aValue : TdwsJSONValue);
         procedure AddHashed(hash : Cardinal; const aName : UnicodeString; aValue : TdwsJSONValue);

         function AddObject(const name : UnicodeString) : TdwsJSONObject;

         function AddArray(const name : UnicodeString) : TdwsJSONArray;

         function AddValue(const name : UnicodeString) : TdwsJSONImmediate; overload;
         function AddValue(const name, value : UnicodeString) : TdwsJSONImmediate; overload;
         function AddValue(const name : UnicodeString; const value : Double) : TdwsJSONImmediate; overload;
         function AddValue(const name : UnicodeString; const value : Boolean) : TdwsJSONImmediate; overload;

         procedure Delete(const name : UnicodeString);

         procedure WriteTo(writer : TdwsJSONWriter); override;

         procedure MergeDuplicates;
   end;

   PdwsJSONValueArray = ^TdwsJSONValueArray;
   TdwsJSONValueArray = array [0..MaxInt shr 4] of TdwsJSONValue;

   TdwsJSONValueCompareMethod = function (left, right : TdwsJSONValue) : Integer of object;

   // TdwsJSONArray
   //
   TdwsJSONArray = class sealed (TdwsJSONValue)
      private
         FElements : PdwsJSONValueArray;
         FCapacity : Integer;
         FCount : Integer;

      protected
         procedure SetOwner(aOwner : TdwsJSONValue); override;

         procedure Grow;
         procedure SetCapacity(newCapacity : Integer);
         procedure DetachChild(child : TdwsJSONValue); override;
         procedure DeleteIndex(idx : Integer);
         procedure SwapNoRangeCheck(index1, index2 : NativeInt);

         function GetValueType : TdwsJSONValueType; override;
         function DoGetName(index : Integer) : UnicodeString; override;
         function DoGetElement(index : Integer) : TdwsJSONValue; override;
         procedure DoSetElement(index : Integer; const value : TdwsJSONValue); override;
         function DoGetItem(const name : UnicodeString) : TdwsJSONValue; override;
         procedure DoSetItem(const name : UnicodeString; const value : TdwsJSONValue); override;
         function DoElementCount : Integer; override;

         procedure DoParse(initialChar : WideChar; parserState : TdwsJSONParserState); override;

         function DoClone : TdwsJSONValue; override;
         procedure DoExtend(other : TdwsJSONValue); override;

      public
         destructor Destroy; override;

         function Clone : TdwsJSONArray;

         procedure Clear;

         procedure Add(aValue : TdwsJSONValue); overload;
         procedure Add(const aValue : Int64); overload;
         procedure Add(const aValue : Double); overload;
         procedure Add(const aValue : UnicodeString); overload;
         procedure Add(const aValue : Boolean); overload;
         function AddObject : TdwsJSONObject;
         function AddArray : TdwsJSONArray;
         function AddValue : TdwsJSONImmediate;
         procedure AddNull;
         procedure Delete(index : Integer);

         procedure AddFrom(other : TdwsJSONArray);

         procedure Sort(const aCompareMethod : TdwsJSONValueCompareMethod);
         procedure Swap(index1, index2 : Integer);

         procedure WriteTo(writer : TdwsJSONWriter); override;
   end;

   // TdwsJSONImmediate
   //
   TdwsJSONImmediate = class sealed (TdwsJSONValue)
      private
         FData : Double;

      protected
         function GetType : TdwsJSONValueType; inline;
         procedure SetType(t : TdwsJSONValueType); inline;
         property FType : TdwsJSONValueType read GetType write SetType;

         procedure DoSetItem(const name : UnicodeString; const value : TdwsJSONValue); override;
         procedure DoSetElement(index : Integer; const value : TdwsJSONValue); override;

         function GetValueType : TdwsJSONValueType; override;
         function GetAsVariant : Variant; overload; inline;
         procedure SetAsVariant(const val : Variant);
         function GetAsString : UnicodeString; inline;
         procedure SetAsString(const val : UnicodeString); inline;
         function GetIsNull : Boolean; inline;
         procedure SetIsNull(const val : Boolean);
         function GetAsBoolean : Boolean;
         procedure SetAsBoolean(const val : Boolean); inline;
         function GetAsNumber : Double;
         procedure SetAsNumber(const val : Double); inline;
         function GetAsInteger : Int64;
         procedure SetAsInteger(const val : Int64);

         function GetIsImmediateValue : Boolean; override;

         procedure DoParse(initialChar : WideChar; parserState : TdwsJSONParserState); override;

         function DoClone : TdwsJSONValue; override;
         procedure DoExtend(other : TdwsJSONValue); override;

         function DoIsFalsey : Boolean; override;

      public
         destructor Destroy; override;
         procedure Release; override;

         class function ParseString(const json : UnicodeString) : TdwsJSONImmediate; static;
         class function FromVariant(const v : Variant) : TdwsJSONImmediate; static;
         class function CreateNull : TdwsJSONImmediate; static;

         function Clone : TdwsJSONImmediate;

         procedure WriteTo(writer : TdwsJSONWriter); override;

         procedure GetAsVariant(var result : Variant); overload;

         property AsVariant : Variant read GetAsVariant write SetAsVariant;
         property AsString : UnicodeString read GetAsString write SetAsString;
         property IsNull : Boolean read GetIsNull write SetIsNull;
         property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
         property AsNumber : Double read GetAsNumber write SetAsNumber;
         property AsInteger : Int64 read GetAsInteger write SetAsInteger;

         procedure Clear;
   end;

   EdwsJSONException = class (Exception);
   EdwsJSONParseError = class (EdwsJSONException);
   EdwsJSONWriterError = class (EdwsJSONException);
   EdwsJSONIndexOutOfRange = class (EdwsJSONException)
      constructor Create(idx, count : Integer);
   end;

procedure WriteJavaScriptString(destStream : TStream; const str : UnicodeString); overload; inline;
procedure WriteJavaScriptString(destStream : TStream; p : PWideChar; size : Integer); overload;

function JSONStringify(const f : Double) : String;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
   System.Variants, System.Math,
   dwsXXHash, dwsXPlatform;

var
   vJSONFormatSettings : TFormatSettings;
   vImmediate : TClassCloneConstructor<TdwsJSONImmediate>;
   vObject : TClassCloneConstructor<TdwsJSONObject>;
   vArray : TClassCloneConstructor<TdwsJSONArray>;

   vImmediatePool : array [0..31] of TdwsJSONImmediate;
   vImmediatePoolLock : TMultiReadSingleWrite;
   vImmediatePoolCount : Integer;

function AllocImmediate : TdwsJSONImmediate;
begin
   if (vImmediatePoolCount > 0) and vImmediatePoolLock.TryBeginWrite then begin
      try
         if vImmediatePoolCount > 0 then begin
            Dec(vImmediatePoolCount);
            Result := vImmediatePool[vImmediatePoolCount];
            vImmediatePool[vImmediatePoolCount] := nil;
            Exit;
         end;
      finally
         vImmediatePoolLock.EndWrite;
      end;
   end;
   Result := vImmediate.Create;
end;

procedure ReleaseImmediate(imm : TdwsJSONImmediate);
begin
   if imm = nil then Exit;
   imm.Clear;
   if (vImmediatePoolCount <= High(vImmediatePool)) and vImmediatePoolLock.TryBeginWrite then begin
      try
         if vImmediatePoolCount <= High(vImmediatePool) then begin
            vImmediatePool[vImmediatePoolCount] := imm;
            Inc(vImmediatePoolCount);
            Exit;
         end;
      finally
         vImmediatePoolLock.EndWrite;
      end;
   end;
   imm.Free;
end;

procedure FlushImmediates;
var
   imm : TdwsJSONImmediate;
begin
   vImmediatePoolLock.BeginWrite;
   try
      while vImmediatePoolCount > 0 do begin
         Dec(vImmediatePoolCount);
         imm := vImmediatePool[vImmediatePoolCount];
         vImmediatePool[vImmediatePoolCount] := nil;
         imm.Free;
      end;
   finally
      vImmediatePoolLock.EndWrite;
   end;
end;

// ------------------
// ------------------ EdwsJSONIndexOutOfRange ------------------
// ------------------

// Create
//
constructor EdwsJSONIndexOutOfRange.Create(idx, count : Integer);
begin
   if count = 0 then
      inherited CreateFmt('Array index (%d) out of range (empty array)', [idx])
   else inherited CreateFmt('Array index (%d) out of range [0..%d]', [idx, count-1]);
end;

// ------------------
// ------------------ TdwsJSONParserState ------------------
// ------------------

// Create
//
constructor TdwsJSONParserState.Create(const aStr : UnicodeString);
begin
   Str:=aStr;
   Ptr:=PWideChar(Str);
   ColStart:=Ptr;
end;

// NeedChar
//
function TdwsJSONParserState.NeedChar : WideChar;
var
   p : PWideChar;
begin
   p:=Ptr;
   Inc(Ptr);
   if p^=#10 then begin
      ColStart:=p;
      Inc(Line);
   end;
   Result:=p^;
end;

// Location
//
function TdwsJSONParserState.Location : String;
begin
   if (Line=0) then begin
      Result:=Format('line 1, col %d',
                            [(NativeUInt(Ptr)-NativeUInt(ColStart)) div SizeOf(WideChar)]);
   end else begin
      Result:=Format('line %d, col %d (offset %d)',
                     [Line+1,
                      (NativeUInt(Ptr)-NativeUInt(ColStart)) div SizeOf(WideChar),
                      (NativeUInt(Ptr)-NativeUInt(PWideChar(Str))) div SizeOf(WideChar)]);
   end;
end;

// SkipBlanks
//
function TdwsJSONParserState.SkipBlanks(currentChar : WideChar) : WideChar;
begin
   Result:=currentChar;
   repeat
      case Result of
         #9..#13, ' ' : ;
      else
         Break;
      end;
      Result:=NeedChar();
   until False;
end;

// SaveLocation
//
function TdwsJSONParserState.SaveLocation : TdwsJSONParserLocation;
begin
   Result.Ptr := Ptr;
   Result.ColStart := ColStart;
   Result.Line := Line;
   Result.TrailCharacter := TrailCharacter;
end;

// LoadLocation
//
procedure TdwsJSONParserState.LoadLocation(const aLocation : TdwsJSONParserLocation);
begin
   Ptr := aLocation.Ptr;
   ColStart := aLocation.ColStart;
   Line := aLocation.Line;
   FTrailCharacter := aLocation.TrailCharacter;
end;

// ParseEscapedCharacter
//
function TdwsJSONParserState.ParseEscapedCharacter : WideChar;
var
   c : WideChar;
   hexBuf, hexCount : Integer;
begin
   c:=NeedChar();
   case c of
      '"', '\', '/' : Result:=c;
      'n' : Result:=#10;
      'r' : Result:=#13;
      't' : Result:=#9;
      'b' : Result:=#8;
      'f' : Result:=#12;
      'u' : begin
         hexBuf:=0;
         for hexCount:=1 to 4 do begin
            c:=NeedChar();
            case c of
               '0'..'9' :
                  hexBuf:=(hexBuf shl 4)+Ord(c)-Ord('0');
               'a'..'f' :
                  hexBuf:=(hexBuf shl 4)+Ord(c)-(Ord('a')-10);
               'A'..'F' :
                  hexBuf:=(hexBuf shl 4)+Ord(c)-(Ord('A')-10);
            else
               TdwsJSONValue.RaiseJSONParseError('Invalid unicode hex character "%s"', c);
            end;
         end;
         Result:=WideChar(hexBuf);
      end;
   else
      Result := c;
      TdwsJSONValue.RaiseJSONParseError('Invalid character "%s" after escape', c);
   end;
end;

// ParseJSONUnicodeString
//
procedure TdwsJSONParserState.ParseJSONUnicodeString(initialChar : WideChar; var result : UnicodeString);
var
   c : WideChar;
   wobs : TWriteOnlyBlockStream;
   n, nw : Integer;
   localBufferPtr, startPr : PWideChar;
   localBuffer : array [0..95] of WideChar;
begin
   startPr:=Ptr;
   wobs:=nil;
   try
      localBufferPtr:=@localBuffer[0];
      repeat
         c:=Ptr^;
         Inc(Ptr);
         case c of
            #0..#31 :
               if c=#0 then begin
                  Ptr:=startPr;
                  TdwsJSONValue.RaiseJSONParseError('Unterminated UnicodeString')
               end else TdwsJSONValue.RaiseJSONParseError('Invalid UnicodeString character %s', c);
            '"' : Break;
            '\' : localBufferPtr^:=ParseEscapedCharacter;
         else
            localBufferPtr^:=c;
         end;
         if localBufferPtr=@localBuffer[High(localBuffer)] then begin
            if wobs=nil then
               wobs:=TWriteOnlyBlockStream.AllocFromPool;
            wobs.WriteP(@localBuffer, Length(localBuffer));
            localBufferPtr:=@localBuffer[0];
         end else Inc(localBufferPtr);
      until False;
      n:=(NativeUInt(localBufferPtr)-NativeUInt(@localBuffer[0])) div SizeOf(WideChar);
      if wobs<>nil then begin
         nw:=(wobs.Size div SizeOf(WideChar));
         SetLength(Result, n+nw);
         localBufferPtr:=PWideChar(Pointer(Result));
         wobs.StoreData(localBufferPtr^);
         Move(localBuffer[0], localBufferPtr[nw], n*SizeOf(WideChar));
      end else begin
         if n>0 then begin
            SetLength(Result, n);
            localBufferPtr:=PWideChar(Pointer(Result));
            Move(localBuffer[0], localBufferPtr^, n*SizeOf(WideChar));
         end else Result:='';
      end;
   finally
      wobs.ReturnToPool;
   end;
   {$ifndef FPC}
   if UnifyUnicodeStrings then
      Result:=UnifiedString(Result);
   {$endif}
end;

// ParseJSONNumber
//
procedure TdwsJSONParserState.ParseHugeJSONNumber(
      initialChars : PWideChar; initialCharCount : Integer;
      var result : Double; var jvt : TdwsJSONValueType
);
var
   buf : String;
   c : WideChar;
begin
   jvt := jvtInt64;
   SetString(buf, initialChars, initialCharCount);
   repeat
      c := NeedChar();
      case c of
         '0'..'9', '-', '+' : buf := buf + Char(c);
         'e', 'E', '.' : begin
            jvt := jvtNumber;
            buf := buf + Char(c);
         end
      else
         TrailCharacter := c;
         Break;
      end;
   until False;
   if jvt = jvtInt64 then begin
      // attempt Int64, if too large, fallback to Double
      if TryStrToInt64(buf, PInt64(@Result)^) then Exit;
      jvt := jvtNumber;
   end;
   if not TryStrToDouble(buf, Result, vJSONFormatSettings) then
      TdwsJSONValue.RaiseJSONParseError('Invalid number');
end;

// ParseJSONNumber
//
procedure TdwsJSONParserState.ParseJSONNumber(initialChar : WideChar; var result : Double; var jvt : TdwsJSONValueType);
var
   bufPtr : PWideChar;
   c : WideChar;
   buf : array [0..15] of WideChar;
begin
   buf[0] := initialChar;
   bufPtr := @buf[1];
   repeat
      c := NeedChar();
      case c of
         '0'..'9', '-', '+', 'e', 'E', '.' : begin
            bufPtr^ := c;
            Inc(bufPtr);
            if bufPtr = @buf[High(buf)] then begin
               ParseHugeJSONNumber(@buf[0], Length(buf)-1, result, jvt);
               Exit;
            end;
         end;
         'I' : begin
            if     (buf[0] = '-') and (bufPtr = @buf[1])
               and (NeedChar() = 'n')
               and (NeedChar() = 'f')
               and (NeedChar() = 'i')
               and (NeedChar() = 'n')
               and (NeedChar() = 'i')
               and (NeedChar() = 't')
               and (NeedChar() = 'y') then  begin
               jvt := jvtNumber;
               result := -1/0;
               Exit;
            end else TdwsJSONValue.RaiseJSONParseError('Invalid number');
         end
      else
         TrailCharacter := c;
         Break;
      end;
   until False;
   jvt := jvtNumber;
   case NativeUInt(bufPtr)-NativeUInt(@buf[0]) of
      SizeOf(WideChar) : // special case of single-character number
         case buf[0] of
            '0'..'9' : begin
               result := Ord(buf[0])-Ord('0');
               exit;
            end;
         end;
      2*SizeOf(WideChar) : // special case of two-characters number
         case buf[0] of
            '1'..'9' : begin
               case buf[1] of
                  '0'..'9' : begin
                     result := Ord(buf[0])*10+Ord(buf[1])-Ord('0')*11;
                     exit;
                  end;
               end;
            end;
         end;
      3*SizeOf(WideChar) : // special case of three-characters number
         case buf[0] of
            '0'..'9' : begin
               case buf[1] of
                  '0'..'9' : begin
                     case buf[2] of
                        '0'..'9' : begin
                           result := Ord(buf[0])*100+Ord(buf[1])*10+Ord(buf[2])-Ord('0')*111;
                           exit;
                        end;
                     end;
                  end;
                  '.' : begin
                     case buf[2] of
                        '0'..'9' : begin
                           result := (Ord(buf[0])-Ord('0'))+(Ord(buf[2])-Ord('0'))*0.1;
                           exit;
                        end;
                     end;
                  end;
               end;
            end;
         end;
   end;
   bufPtr^ := #0;
   if not TryStrToDouble(PWideChar(@buf[0]), result) then
      TdwsJSONValue.RaiseJSONParseError('Invalid number');
end;

// ParseIntegerArray
//
procedure TdwsJSONParserState.ParseIntegerArray(dest : TSimpleInt64List; const nullValue : Int64 = 0);

   function ParseJSONInteger(initialChar : WideChar) : Int64;
   var
      neg : Boolean;
      d : Integer;
      p : PWideChar;
   begin
      neg := (initialChar = '-');
      // branchless "Result := if not neg then initialDigit else 0"
      Result := (Ord(neg)-1) and (Ord(initialChar) - Ord('0'));
      p := Ptr;
      while True do begin
         d := Ord(p^) - Ord('0');
         if Cardinal(d) <= 9 then begin
            Result := Result*10 + d;
            Inc(p);
            if Result < 0 then
               raise EdwsJSONParseError.Create('Integer overflow');
         end else begin
            Ptr := p;
            TrailCharacter := NeedChar;
            Break;
         end;
      end;
      if neg then
         Result := -Result;
   end;

var
   c : WideChar;
begin
   c:=SkipBlanks(' ');
   if c<>'[' then
      raise EdwsJSONParseError.CreateFmt('"[" expected but U+%.04x encountered', [Ord(c)]);
   c:=SkipBlanks(NeedChar);
   if c=']' then
      Exit;
   repeat
      case c of
         '0'..'9', '-' : begin
            dest.Add(ParseJSONInteger(c));
         end;
         'n' : begin
            if (NeedChar = 'u') and (NeedChar = 'l') and (NeedChar = 'l') then
               dest.Add(nullValue)
            else raise EdwsJSONParseError.Create('Unexpected character after "n"');
            TrailCharacter := NeedChar;
         end;
      else
         raise EdwsJSONParseError.CreateFmt('Unexpected character U+%.04x', [Ord(c)]);
      end;
      if TrailCharacter <> ',' then begin
         c := SkipBlanks(TrailCharacter);
         case c of
            ',' : ;
            ']' : break;
         else
            raise EdwsJSONParseError.CreateFmt('"," expected but U+%.04x encountered', [Ord(c)]);
         end;
      end;
      case Ptr^ of
         '0'..'9' : begin
            c := Ptr^;
            Inc(Ptr);
         end;
      else
         c:=SkipBlanks(NeedChar)
      end;
   until False;
end;

// ParseNumberArray
//
procedure TdwsJSONParserState.ParseNumberArray(dest : TSimpleDoubleList);
var
   c : WideChar;
   num : Double;
   jvt : TdwsJSONValueType;
begin
   c:=SkipBlanks(' ');
   if c<>'[' then
      raise EdwsJSONParseError.CreateFmt('"[" expected but U+%.04x encountered', [Ord(c)]);
   c:=SkipBlanks(NeedChar);
   if c=']' then
      Exit;
   repeat
      case c of
         '0'..'9', '-' : begin
            ParseJSONNumber(c, num, jvt);
            if jvt = jvtInt64 then
               dest.Add(PInt64(@num)^)
            else dest.Add(num);
         end;
      else
         raise EdwsJSONParseError.CreateFmt('Unexpected character U+%.04x', [Ord(c)]);
      end;
      c:=SkipBlanks(TrailCharacter);
      case c of
         ',' : ;
         ']' : break;
      else
         raise EdwsJSONParseError.CreateFmt('"," expected but U+%.04x encountered', [Ord(c)]);
      end;
      c:=SkipBlanks(NeedChar)
   until False;
end;

// ParseStringArray
//
procedure TdwsJSONParserState.ParseStringArray(dest : TUnicodeStringList);
var
   c : WideChar;
   buf : UnicodeString;
begin
   c:=SkipBlanks(' ');
   if c<>'[' then
      raise EdwsJSONParseError.CreateFmt('"[" expected but U+%.04x encountered', [Ord(c)]);
   c:=SkipBlanks(NeedChar);
   if c=']' then
      Exit;
   repeat
      if c='"' then begin
         ParseJSONUnicodeString(c, buf);
         dest.Add(buf);
      end else begin
         raise EdwsJSONParseError.CreateFmt('Unexpected character U+%.04x', [Ord(c)]);
      end;
      c:=SkipBlanks(NeedChar);
      case c of
         ',' : ;
         ']' : break;
      else
         raise EdwsJSONParseError.CreateFmt('"," expected but U+%.04x encountered', [Ord(c)]);
      end;
      c:=SkipBlanks(NeedChar);
   until False;
end;

// WriteJavaScriptString
//
procedure WriteJavaScriptString(destStream : TStream; const str : UnicodeString);
begin
   WriteJavaScriptString(destStream, PWideChar(Pointer(str)), Length(str));
end;

// JSONStringify
//
function JSONStringify(const f : Double) : String;
begin
   if IsNan(f) then
      Result := 'null'
   else FastFloatToStr(f, Result, vJSONFormatSettings);
end;

// WriteJavaScriptString
//
procedure WriteJavaScriptString(destStream : TStream; p : PWideChar; size : Integer); overload;

   function WriteUTF16(p : PWideChar; c : Integer) : PWideChar;
   const
      cIntToHex : array [0..15] of WideChar = (
         '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
   begin
      p[0]:='\';
      p[1]:='u';
      p[2]:=cIntToHex[c shr 12];
      p[3]:=cIntToHex[(c shr 8) and $F];
      p[4]:=cIntToHex[(c shr 4) and $F];
      p[5]:=cIntToHex[c and $F];
      Result := p + 6;
   end;

const
   cQUOTE : WideChar = '"';
var
   c : WideChar;
   buf : array [0..127] of WideChar;
   pbuf : PWideChar;
begin
   pbuf := @buf;
   pbuf^ := cQUOTE;
   Inc(pbuf);
   if p <> nil then while size > 0 do begin
      c := p^;
      case Ord(c) of
         0..31 :
            case Ord(c) of
               8 : begin PCardinal(pbuf)^ := Ord('b')*$10000 + Ord('\'); Inc(pbuf, 2); end;
               9 : begin PCardinal(pbuf)^ := Ord('t')*$10000 + Ord('\'); Inc(pbuf, 2); end;
               10 : begin PCardinal(pbuf)^ := Ord('n')*$10000 + Ord('\'); Inc(pbuf, 2); end;
               12 : begin PCardinal(pbuf)^ := Ord('f')*$10000 + Ord('\'); Inc(pbuf, 2); end;
               13 : begin PCardinal(pbuf)^ := Ord('r')*$10000 + Ord('\'); Inc(pbuf, 2); end;
            else
               pbuf := WriteUTF16(pbuf, Ord(c));
            end;
         Ord('"') : begin
            PCardinal(pbuf)^ := Ord('"')*$10000 + Ord('\'); Inc(pbuf, 2);
         end;
         Ord('\') : begin
            PCardinal(pbuf)^ := Ord('\')*$10000 + Ord('\'); Inc(pbuf, 2);
         end;
         Ord('/') : begin // XSS protection when used for inline scripts in HTML
            PCardinal(pbuf)^ := Ord('/')*$10000 + Ord('\'); Inc(pbuf, 2);
         end;
         $100..$FFFF :
            pbuf := WriteUTF16(pbuf, Ord(c));
      else
         pbuf^ := c;
         Inc(pbuf);
      end;
      Dec(size);
      Inc(p);
      if IntPtr(pbuf) > IntPtr(@buf[High(buf)-8]) then begin
         destStream.Write(buf, IntPtr(pbuf)-IntPtr(@buf));
         pbuf := @buf;
      end;
   end;
   pbuf^ := cQUOTE;
   Inc(pbuf);
   destStream.Write(buf, IntPtr(pbuf)-IntPtr(@buf));
end;

// ------------------
// ------------------ TdwsJSONValue ------------------
// ------------------

// GetOwner
//
function TdwsJSONValue.GetOwner : TdwsJSONValue;
begin
   Result:=TdwsJSONValue(FRawOwner and -8);
end;

// SetOwner
//
procedure TdwsJSONValue.SetOwner(aOwner : TdwsJSONValue);
begin
   FRawOwner:=(FRawOwner and $7) or NativeUInt(aOwner);
end;

// ClearOwner
//
procedure TdwsJSONValue.ClearOwner;
begin
   FRawOwner:=(FRawOwner and $7);
end;

// HasInOwners
//
function TdwsJSONValue.HasInOwners(obj : TdwsJSONValue) : Boolean;
begin
   var o := Self;
   repeat
      if o = obj then Exit(True);
      o := o.GetOwner;
   until o = nil;
   Result := False;
end;

// Destroy
//
destructor TdwsJSONValue.Destroy;
begin
   if FOwner<>nil then
      Detach;
   inherited;
end;

// Release
//
procedure TdwsJSONValue.Release;
begin
   Destroy;
end;

// Parse
//
class function TdwsJSONValue.Parse(parserState : TdwsJSONParserState) : TdwsJSONValue;
var
   c : WideChar;
begin
   Result:=nil;
   repeat
      c:=parserState.NeedChar();
      case c of
         #0 : Break;
         #9..#13, ' ' : ;
         '{' : Result:=vObject.Create;
         '[' : Result:=vArray.Create;
         '0'..'9', '"', '-', 't', 'f', 'n', 'N', 'I' :
            Result := AllocImmediate;
         ']', '}' : begin
            // empty array or object
            parserState.TrailCharacter:=c;
            Exit(nil);
         end;
      else
         RaiseJSONParseError('Invalid value start character "%s"', c);
      end;
   until Result<>nil;
   if Result<>nil then begin
      try
         Result.DoParse(c, parserState);
      except
         Result.Free;
         raise;
      end;
   end;
end;

// ParseString
//
class function TdwsJSONValue.ParseString(const json : UnicodeString;
                                         duplicatesOption : TdwsJSONDuplicatesOptions = jdoOverwrite) : TdwsJSONValue;
const
   cAutoUnifierTreshold = 10 * 1024 * 1024;
var
   parserState : TdwsJSONParserState;
begin
   {$ifndef DELPHI_TOKYO_PLUS}
   Result := nil;
   {$endif}
   parserState := TdwsJSONParserState.Create(json);
   try
      parserState.UnifyUnicodeStrings := (Length(json) >= cAutoUnifierTreshold);
      try
         parserState.DuplicatesOption:=duplicatesOption;
         Result := TdwsJSONValue.Parse(parserState);
      except
         on e : EdwsJSONParseError do
            raise EdwsJSONParseError.CreateFmt('%s, at %s',
                                               [e.Message, parserState.Location]);
      else
         raise;
      end;
   finally
      if parserState.UnifyUnicodeStrings then
         TidyStringsUnifier;
      parserState.Free;
   end;
end;

// ParseFile
//
class function TdwsJSONValue.ParseFile(const fileName : TFileName) : TdwsJSONValue;
begin
   Result:=ParseString(LoadTextFromFile(fileName));
end;

// Clone
//
function TdwsJSONValue.Clone : TdwsJSONValue;
begin
   if Self<>nil then
      Result:=DoClone
   else Result:=nil;
end;

// Extend
//
procedure TdwsJSONValue.Extend(other : TdwsJSONValue);
begin
   if Self=nil then
      RaiseJSONException('Cannot extend undefined object')
   else if other<>nil then
      DoExtend(other);
end;

// WriteToStream
//
procedure TdwsJSONValue.WriteToStream(aStream : TStream);
var
   writer : TdwsJSONWriter;
   wobs : TWriteOnlyBlockStream;
begin
   if Self=nil then Exit;
   wobs:=TWriteOnlyBlockStream.AllocFromPool;
   writer:=TdwsJSONWriter.Create(wobs);
   try
      WriteTo(writer);
      wobs.StoreUTF8Data(aStream);
   finally
      writer.Free;
      wobs.ReturnToPool;
   end;
end;

// WriteToStream
//
procedure TdwsJSONValue.WriteToStream(aStream : TWriteOnlyBlockStream);
var
   writer : TdwsJSONWriter;
begin
   if Self=nil then Exit;
   writer:=TdwsJSONWriter.Create(aStream);
   try
      WriteTo(writer);
   finally
      writer.Free;
   end;
end;

// ToString
//
function TdwsJSONValue.ToString : String;
begin
   Result := String(ToUnicodeString);
end;

// ToUnicodeString
//
function TdwsJSONValue.ToUnicodeString : UnicodeString;
var
   writer : TdwsJSONWriter;
begin
   if Self = nil then Exit('');
   writer := TdwsJSONWriter.Create(nil);
   try
      WriteTo(writer);
      writer.StoreToUnicodeString(Result);
   finally
      writer.Free;
   end;
end;

// ToBeautifiedString
//
function TdwsJSONValue.ToBeautifiedString(initialTabs, indentTabs : Integer) : UnicodeString;
var
   writer : TdwsJSONBeautifiedWriter;
begin
   if Self=nil then Exit('');
   writer:=TdwsJSONBeautifiedWriter.Create(nil, initialTabs, indentTabs);
   try
      WriteTo(writer);
      writer.StoreToUnicodeString(Result);
   finally
      writer.Free;
   end;
end;

// Detach
//
procedure TdwsJSONValue.Detach;
var
   oldOwner : TdwsJSONValue;
begin
   oldOwner:=FOwner;
   if oldOwner<>nil then begin
      ClearOwner;
      oldOwner.DetachChild(Self);
   end;
end;

// DoElementCount
//
function TdwsJSONValue.DoElementCount : Integer;
begin
   Result:=0;
end;

// ElementCount
//
function TdwsJSONValue.ElementCount : Integer;
begin
   if Assigned(Self) then
      Result:=DoElementCount
   else Result:=0;
end;

// IsImmediateValue
//
function TdwsJSONValue.IsImmediateValue : Boolean;
begin
   Result:=Assigned(Self) and GetIsImmediateValue;
end;

// Value
//
function TdwsJSONValue.Value : TdwsJSONImmediate;
begin
   if ClassType<>TdwsJSONImmediate then
      RaiseJSONException('Not a value');
   Result:=TdwsJSONImmediate(Self);
end;

// ValueType
//
function TdwsJSONValue.ValueType : TdwsJSONValueType;
begin
   if Assigned(Self) then
      Result:=GetValueType
   else Result:=jvtUndefined;
end;

// IsFalsey
//
function TdwsJSONValue.IsFalsey : Boolean;
begin
   Result:=(not Assigned(Self)) or DoIsFalsey;
end;

// IsArray
//
function TdwsJSONValue.IsArray : Boolean;
begin
   Result := (ValueType = jvtArray);
end;

// IsNumber
//
function TdwsJSONValue.IsNumber : Boolean;
begin
   Result := (ValueType in [ jvtNumber, jvtInt64 ]);
end;

// IsString
//
function TdwsJSONValue.IsString : Boolean;
begin
   Result := (ValueType = jvtString);
end;

// Clear
//
procedure TdwsJSONValue.Clear;
begin
   Value.Clear;
end;

// GetValue
//
function TdwsJSONValue.GetValue(const index : Variant) : TdwsJSONValue;

   function Fallback : TdwsJSONValue;
   begin
      if VariantIsOrdinal(index) then
         Result := Elements[index]
      else Result := Items[index];
   end;

begin
   if Assigned(Self) then begin
      case VarType(index) of
         varInt64 : Result := Elements[TVarData(index).VInt64];
         varUString : Result := Items[String(TVarData(index).VUString)];
      else
         Result := Fallback;
      end;
   end else Result := nil;
end;

// SetValue
//
procedure TdwsJSONValue.SetValue(const index : Variant; const aValue : TdwsJSONValue);
begin
   Assert(Assigned(Self));
   if VariantIsOrdinal(index) then
      Elements[index]:=aValue
   else Items[index]:=aValue;
end;

// GetIsImmediateValue
//
function TdwsJSONValue.GetIsImmediateValue : Boolean;
begin
   Result:=False;
end;

// GetAsString
//
function TdwsJSONValue.GetAsString : UnicodeString;
begin
   if Assigned(Self) then
      Result:=Value.AsString
   else Result:='undefined';
end;

// SetAsString
//
procedure TdwsJSONValue.SetAsString(const val : UnicodeString);
begin
   Value.AsString:=val;
end;

// GetIsNull
//
function TdwsJSONValue.GetIsNull : Boolean;
begin
   if Assigned(Self) then
      Result:=(ValueType=jvtNull)
   else Result:=False;
end;

// SetIsNull
//
procedure TdwsJSONValue.SetIsNull(const val : Boolean);
begin
   Value.IsNull:=val;
end;

// GetIsDefined
//
function TdwsJSONValue.GetIsDefined : Boolean;
begin
   Result:=Assigned(Self) and (ValueType<>jvtUndefined);
end;

// GetAsBoolean
//
function TdwsJSONValue.GetAsBoolean : Boolean;
begin
   if Assigned(Self) then
      Result:=Value.AsBoolean
   else Result:=False;
end;

// SetAsBoolean
//
procedure TdwsJSONValue.SetAsBoolean(const val : Boolean);
begin
   Value.AsBoolean:=val;
end;

// GetAsNumber
//
function TdwsJSONValue.GetAsNumber : Double;
begin
   if Assigned(Self) then
      Result:=Value.AsNumber
   else Result:=NaN;
end;

// SetAsNumber
//
procedure TdwsJSONValue.SetAsNumber(const val : Double);
begin
   Value.AsNumber:=val;
end;

// GetIsNaN
//
function TdwsJSONValue.GetIsNaN : Boolean;
begin
   Result:=not (    Assigned(Self)
                and (ValueType = jvtNumber)
                and System.Math.IsNan(Value.AsNumber));
end;

// GetAsInteger
//
function TdwsJSONValue.GetAsInteger : Int64;
begin
   if Assigned(Self) then
      Result:=Value.AsInteger
   else Result:=0;
end;

// SetAsInteger
//
procedure TdwsJSONValue.SetAsInteger(const val : Int64);
begin
   Value.AsInteger:=val;
end;

// DetachChild
//
procedure TdwsJSONValue.DetachChild(child : TdwsJSONValue);
begin
   Assert(False);
end;

// GetName
//
function TdwsJSONValue.GetName(index : Integer) : UnicodeString;
begin
   if Assigned(Self) then
      Result:=DoGetName(index)
   else Result:='';
end;

// DoGetName
//
function TdwsJSONValue.DoGetName(index : Integer) : UnicodeString;
begin
   Result:='';
end;

// GetElement
//
function TdwsJSONValue.GetElement(index : Integer) : TdwsJSONValue;
begin
   if Assigned(Self) then
      Result:=DoGetElement(index)
   else Result:=nil;
end;

// DoGetElement
//
function TdwsJSONValue.DoGetElement(index : Integer) : TdwsJSONValue;
begin
   Result:=nil;
end;

// SetElement
//
procedure TdwsJSONValue.SetElement(index : Integer; const value : TdwsJSONValue);
begin
   if Assigned(Self) then
      DoSetElement(index, value)
   else raise EdwsJSONException.CreateFmt('Cannot set element "%d" of Undefined', [index]);
end;

// GetItem
//
function TdwsJSONValue.GetItem(const name : UnicodeString) : TdwsJSONValue;
begin
   if Assigned(Self) then
      Result:=DoGetItem(name)
   else Result:=nil;
end;

// GetHashedItem
//
function TdwsJSONValue.GetHashedItem(hash : Cardinal; const name : UnicodeString) : TdwsJSONValue;
begin
   if Assigned(Self) then
      Result:=DoGetHashedItem(hash, name)
   else Result:=nil;
end;

// DoGetItem
//
function TdwsJSONValue.DoGetItem(const name : UnicodeString) : TdwsJSONValue;
begin
   Result:=nil;
end;

// DoGetHashedItem
//
function TdwsJSONValue.DoGetHashedItem(hash : Cardinal; const name : UnicodeString) : TdwsJSONValue;
begin
   Result:=DoGetItem(name);
end;

// SetItem
//
procedure TdwsJSONValue.SetItem(const name : UnicodeString; const value : TdwsJSONValue);
begin
   if Assigned(Self) then
      DoSetItem(name, value)
   else raise EdwsJSONException.CreateFmt('Cannot set member "%s" of Undefined', [name]);
end;

// SetHashedItem
//
procedure TdwsJSONValue.SetHashedItem(hash : Cardinal; const name : UnicodeString; const value : TdwsJSONValue);
begin
   if Assigned(Self) then
      DoSetHashedItem(hash, name, value)
   else SetItem(name, value);
end;

// RaiseJSONException
//
class procedure TdwsJSONValue.RaiseJSONException(const msg : String);
begin
   raise EdwsJSONException.Create(msg);
end;

// RaiseJSONParseError
//
class procedure TdwsJSONValue.RaiseJSONParseError(const msg : String; c : WideChar = #0);
begin
   if c<=#31 then
      raise EdwsJSONParseError.CreateFmt(msg, [IntToStr(Ord(c))])
   else if c>#127 then
      raise EdwsJSONParseError.CreateFmt(msg, ['U+'+IntToHex(Ord(c), 4)])
   else raise EdwsJSONParseError.CreateFmt(msg, [UnicodeString(c)]);
end;

// GetEnumerator
//
function TdwsJSONValue.GetEnumerator : TElementEnumerator;
begin
   Result.FIndex:=-1;
   Result.FOwner:=Self;
   Result.FCountMinus1:=ElementCount-1;
end;

// TElementEnumerator.GetCurrent
//
function TdwsJSONValue.TElementEnumerator.GetCurrent : TdwsJSONValue;
begin
   Result:=FOwner.Elements[FIndex];
end;

// TElementEnumerator.MoveNext
//
function TdwsJSONValue.TElementEnumerator.MoveNext : Boolean;
begin
   Result:=(FIndex<FCountMinus1);
   Inc(FIndex, Integer(Result));
end;

// DoIsFalsey
//
function TdwsJSONValue.DoIsFalsey : Boolean;
begin
   Result:=False;
end;

// DoSetHashedItem
//
procedure TdwsJSONValue.DoSetHashedItem(hash : Cardinal; const name : UnicodeString; const value : TdwsJSONValue);
begin
   SetItem(name, value);
end;

// ------------------
// ------------------ TdwsJSONObject ------------------
// ------------------

// Destroy
//
destructor TdwsJSONObject.Destroy;
begin
   Clear;
   inherited;
end;

// Clone
//
function TdwsJSONObject.Clone : TdwsJSONObject;
begin
   if Self<>nil then
      Result:=(DoClone as TdwsJSONObject)
   else Result:=nil;
end;

// WriteTo
//
procedure TdwsJSONObject.WriteTo(writer : TdwsJSONWriter);
begin
   writer.BeginObject;
   var pair := PdwsJSONPair(FItems);
   for var i := 0 to FCount-1 do begin
      writer.WriteName(pair.Name);
      pair.Value.WriteTo(writer);
      Inc(pair);
   end;
   writer.EndObject;
end;

// DoElementCount
//
function TdwsJSONObject.DoElementCount : Integer;
begin
   Result:=FCount;
end;

// Clear
//
procedure TdwsJSONObject.Clear;
var
   i : Integer;
   v : TdwsJSONValue;
begin
   for i:=0 to FCount-1 do begin
      v:=FItems^[i].Value;
      v.ClearOwner;
      if v.RefCount = 0 then
         v.Release
      else v.DecRefCount;
      FItems^[i].Name:='';
   end;
   FreeMem(FItems);
   FItems:=nil;
   FCount:=0;
   FCapacity:=0;
end;

// Grow
//
procedure TdwsJSONObject.Grow;
begin
   SetCapacity(FCapacity+8+(FCapacity shr 2));
end;

// SetCapacity
//
procedure TdwsJSONObject.SetCapacity(newCapacity : Integer);
begin
   FCapacity := newCapacity;
   ReallocMem(FItems, FCapacity*SizeOf(TdwsJSONPair));
   FillChar(FItems[FCount], (FCapacity-FCount)*SizeOf(TdwsJSONPair), 0);
end;

// Add
//
procedure TdwsJSONObject.Add(const aName : UnicodeString; aValue : TdwsJSONValue);
begin
   AddHashed(SimpleStringHash(aName), aName, aValue);
end;

// AddHashed
//
procedure TdwsJSONObject.AddHashed(hash : Cardinal; const aName : UnicodeString; aValue : TdwsJSONValue);
begin
   if aValue = nil then
      aValue := AllocImmediate
   else Assert(aValue.Owner = nil);
   aValue.FOwner := Self;
   if FCount = FCapacity then Grow;
   var pair : PdwsJSONPair := @FItems[FCount];
   pair.Value := aValue;
   pair.Name := aName;
   pair.Hash := hash;
   Inc(FCount);
end;

// AddObject
//
function TdwsJSONObject.AddObject(const name : UnicodeString) : TdwsJSONObject;
begin
   Result:=vObject.Create;
   Add(name, Result);
end;

// AddArray
//
function TdwsJSONObject.AddArray(const name : UnicodeString) : TdwsJSONArray;
begin
   Result:=vArray.Create;
   Add(name, Result);
end;

// AddValue
//
function TdwsJSONObject.AddValue(const name : UnicodeString) : TdwsJSONImmediate;
begin
   Result := AllocImmediate;
   Add(name, Result);
end;

// Delete
//
procedure TdwsJSONObject.Delete(const name : UnicodeString);
var
   i : Integer;
begin
   i := IndexOfName(name);
   if i >= 0 then
      DetachIndex(i);
end;

// MergeDuplicates
//
procedure TdwsJSONObject.MergeDuplicates;
var
   i, j : Integer;
   h : Cardinal;
begin
   for i:=FCount-1 downto 1 do begin
      h:=FItems[i].Hash;
      for j:=i-1 downto 0 do begin
         if (FItems[j].Hash=h) and (FItems[j].Name=FItems[i].Name) then
            DetachIndex(j);
      end;
   end;
end;

// AddValue (UnicodeString)
//
function TdwsJSONObject.AddValue(const name, value : UnicodeString) : TdwsJSONImmediate;
begin
   Result:=AddValue(name);
   Result.AsString:=value;
end;

// AddValue (number)
//
function TdwsJSONObject.AddValue(const name : UnicodeString; const value : Double) : TdwsJSONImmediate;
begin
   Result:=AddValue(name);
   Result.AsNumber:=value;
end;

// AddValue (bool)
//
function TdwsJSONObject.AddValue(const name : UnicodeString; const value : Boolean) : TdwsJSONImmediate;
begin
   Result:=AddValue(name);
   Result.AsBoolean:=value;
end;

// DetachChild
//
procedure TdwsJSONObject.DetachChild(child : TdwsJSONValue);
var
   i : Integer;
begin
   i:=IndexOfValue(child);
   if i>=0 then
      DetachIndex(i);
end;

// DetachIndex
//
procedure TdwsJSONObject.DetachIndex(i : Integer);
var
   n : Integer;
   child : TdwsJSONValue;
begin
   child:=FItems[i].Value;
   if child.Owner=Self then begin
      child.ClearOwner;
      child.DecRefCount;
   end;
   Finalize(FItems[i]);
   n:=FCount-1;
   if i<n then
      Move(FItems[i+1], FItems[i], (n-i)*SizeOf(TdwsJSONPair));
   FillChar(FItems[n], SizeOf(TdwsJSONPair), 0);
   FCount:=n;
end;

// GetValueType
//
function TdwsJSONObject.GetValueType : TdwsJSONValueType;
begin
   Result:=jvtObject;
end;

// DoGetName
//
function TdwsJSONObject.DoGetName(index : Integer) : UnicodeString;
begin
   if Cardinal(index)<Cardinal(FCount) then
      Result:=FItems^[index].Name
   else Result:='';
end;

// DoGetElement
//
function TdwsJSONObject.DoGetElement(index : Integer) : TdwsJSONValue;
begin
   if Cardinal(index)<Cardinal(FCount) then
      Result:=FItems^[index].Value
   else Result:=nil;
end;

// DoSetElement
//
procedure TdwsJSONObject.DoSetElement(index : Integer; const value : TdwsJSONValue);
var
   member : TdwsJSONValue;
begin
   if Cardinal(index)<Cardinal(FCount) then begin

      if value<>nil then begin

         member:=FItems^[index].Value;
         member.ClearOwner;
         member.DecRefCount;

         FItems^[index].Value:=value;
         value.Detach;
         value.FOwner:=Self;

      end else DetachIndex(index);

   end else if value<>nil then begin

      Add(Int32ToStrU(index), value);

   end;
end;

// DoGetItem
//
function TdwsJSONObject.DoGetItem(const name : UnicodeString) : TdwsJSONValue;
begin
   var i := IndexOfName(name);
   if i >= 0 then
      Result := FItems^[i].Value
   else Result := nil;
end;

// DoGetHashedItem
//
function TdwsJSONObject.DoGetHashedItem(hash : Cardinal; const name : UnicodeString) : TdwsJSONValue;
begin
   var i := IndexOfHashedName(hash, name);
   if i >= 0 then
      Result := FItems^[i].Value
   else Result := nil;
end;

// DoSetItem
//
procedure TdwsJSONObject.DoSetItem(const name : UnicodeString; const value : TdwsJSONValue);
begin
   var i := IndexOfName(name);
   if i >= 0 then
      DoSetElement(i, value)
   else Add(name, value);
end;

// DoSetHashedItem
//
procedure TdwsJSONObject.DoSetHashedItem(hash : Cardinal; const name : UnicodeString; const value : TdwsJSONValue);
begin
   var i := IndexOfHashedName(hash, name);
   if i >= 0 then
      DoSetElement(i, value)
   else Add(name, value);
end;

// DoParse
//
procedure TdwsJSONObject.DoParse(initialChar : WideChar; parserState : TdwsJSONParserState);
var
   c : WideChar;
   name : UnicodeString;
   locValue : TdwsJSONValue;
begin
   Assert(initialChar='{');
   repeat
      c := parserState.SkipBlanks(parserState.NeedChar());
      if c <> '"' then begin
         if FCount = 0 then Break;
         RaiseJSONParseError('Invalid object pair name start character "%s"', c)
      end;
      parserState.ParseJSONUnicodeString(c, name);
      c := parserState.SkipBlanks(parserState.NeedChar());
      if c <> ':' then
         RaiseJSONParseError('Invalid object pair name separator character "%s"', c);
      locValue := TdwsJSONValue.Parse(parserState);
      if locValue = nil then
         RaiseJSONParseError('Missing element value');
      Add(name, locValue);
      c:=parserState.SkipBlanks(parserState.TrailCharacter);
   until c <> ',';
   if c <> '}' then
      RaiseJSONParseError('Invalid object termination character "%s"', c);
   if parserState.DuplicatesOption = jdoOverwrite then
      MergeDuplicates;
   parserState.TrailCharacter := ' ';
end;

// DoClone
//
function TdwsJSONObject.DoClone : TdwsJSONValue;
var
   obj : TdwsJSONObject;
   member : TdwsJSONValue;
begin
   obj:=vObject.Create;
   obj.SetCapacity(FCount);
   obj.FCount:=FCount;
   for var i := 0 to FCount-1 do begin
      obj.FItems[i].Name:=FItems[i].Name;
      obj.FItems[i].Hash:=FItems[i].Hash;
      member:=FItems[i].Value.Clone;
      member.FOwner:=obj;
      obj.FItems[i].Value:=member;
   end;
   Result:=obj;
end;

// DoExtend
//
procedure TdwsJSONObject.DoExtend(other : TdwsJSONValue);
var
   i, k : Integer;
   otherObj : TdwsJSONObject;
   member : TdwsJSONValue;
begin
   if other.ClassType<>TdwsJSONObject then
      RaiseJSONException('Can only extend Object with Object');
   otherObj:=TdwsJSONObject(other);
   for i:=0 to otherObj.FCount-1 do begin
      k:=IndexOfName(otherObj.FItems[i].Name);
      if k>=0 then begin
         member:=FItems[k].Value;
         member.ClearOwner;
         member.DecRefCount;
         member:=otherObj.FItems[i].Value.Clone;
         member.FOwner:=Self;
         FItems[k].Value:=member;
      end else begin
         Add(otherObj.FItems[i].Name, otherObj.FItems[i].Value.Clone);
      end;
   end;
end;

// IndexOfHashedName
//
function TdwsJSONObject.IndexOfHashedName(hash : Cardinal; const name : UnicodeString) : Integer;
begin
   for var i := 0 to FCount-1 do begin
      if (FItems^[i].Hash=hash) and (FItems^[i].Name=name) then
         Exit(i);
   end;
   Result:=-1;
end;

// IndexOfName
//
function TdwsJSONObject.IndexOfName(const name : UnicodeString) : Integer;
begin
   Result := IndexOfHashedName(SimpleStringHash(name), name);
end;

// IndexOfValue
//
function TdwsJSONObject.IndexOfValue(const aValue : TdwsJSONValue) : Integer;
begin
   for var i := 0 to FCount-1 do
      if FItems^[i].Value = aValue then
         Exit(i);
   Result := -1;
end;

// SetOwner
//
procedure TdwsJSONObject.SetOwner(aOwner : TdwsJSONValue);
begin
   if aOwner.HasInOwners(Self) then
      RaiseJSONException('JSON circular reference');
   inherited SetOwner(aOwner);
end;

// ------------------
// ------------------ TdwsJSONArray ------------------
// ------------------

// Destroy
//
destructor TdwsJSONArray.Destroy;
begin
   Clear;
   inherited;
end;

// Clone
//
function TdwsJSONArray.Clone : TdwsJSONArray;
begin
   if Self<>nil then
      Result:=(DoClone as TdwsJSONArray)
   else Result:=nil;
end;

// WriteTo
//
procedure TdwsJSONArray.WriteTo(writer : TdwsJSONWriter);
begin
   writer.BeginArray;
   for var i := 0 to FCount-1 do
      FElements[i].WriteTo(writer);
   writer.EndArray;
end;

// Grow
//
procedure TdwsJSONArray.Grow;
begin
   if FCapacity<16 then
      SetCapacity(FCapacity+4)
   else SetCapacity(FCapacity+(FCapacity shr 2));
end;

// SetCapacity
//
procedure TdwsJSONArray.SetCapacity(newCapacity : Integer);
begin
   FCapacity:=newCapacity;
   ReallocMem(FElements, FCapacity*SizeOf(Pointer));
end;

// DetachChild
//
procedure TdwsJSONArray.DetachChild(child : TdwsJSONValue);
var
   i : Integer;
begin
   for i:=0 to FCount-1 do begin
      if FElements^[i]=child then begin
         DeleteIndex(i);
         Break;
      end;
   end;
end;

// DeleteIndex
//
procedure TdwsJSONArray.DeleteIndex(idx : Integer);
var
   child : TdwsJSONValue;
begin
   child:=FElements[idx];
   if child.FOwner=Self then begin
      child.ClearOwner;
      child.DecRefCount;
   end;
   Move(FElements[idx+1], FElements[idx], (FCount-1-idx)*SizeOf(Pointer));
   Dec(FCount);
end;

// GetValueType
//
function TdwsJSONArray.GetValueType : TdwsJSONValueType;
begin
   Result:=jvtArray;
end;

// DoElementCount
//
function TdwsJSONArray.DoElementCount : Integer;
begin
   Result:=FCount;
end;

// Clear
//
procedure TdwsJSONArray.Clear;
var
   i : Integer;
   v : TdwsJSONValue;
begin
   for i:=0 to FCount-1 do begin
      v:=FElements^[i];
      v.ClearOwner;
      v.DecRefCount;
   end;
   FreeMem(FElements);
   FElements:=nil;
   FCount:=0;
   FCapacity:=0;
end;

// Add (value)
//
procedure TdwsJSONArray.Add(aValue : TdwsJSONValue);
begin
   if aValue.Owner<>nil then begin
      aValue.IncRefCount;
      aValue.Owner.DetachChild(aValue);
   end;

   aValue.FOwner:=Self;
   if FCount=FCapacity then Grow;
   FElements^[FCount]:=aValue;
   Inc(FCount);
end;

// Add (int)
//
procedure TdwsJSONArray.Add(const aValue : Int64);
var
   v : TdwsJSONImmediate;
begin
   v:=TdwsJSONImmediate.Create;
   v.AsInteger:=aValue;
   Add(v);
end;

// Add (float)
//
procedure TdwsJSONArray.Add(const aValue : Double);
var
   v : TdwsJSONImmediate;
begin
   v:=TdwsJSONImmediate.Create;
   v.AsNumber:=aValue;
   Add(v);
end;

// Add (str)
//
procedure TdwsJSONArray.Add(const aValue : UnicodeString);
var
   v : TdwsJSONImmediate;
begin
   v:=TdwsJSONImmediate.Create;
   v.AsString:=aValue;
   Add(v);
end;

// Add (bool)
//
procedure TdwsJSONArray.Add(const aValue : Boolean);
var
   v : TdwsJSONImmediate;
begin
   v:=TdwsJSONImmediate.Create;
   v.AsBoolean:=aValue;
   Add(v);
end;

// AddObject
//
function TdwsJSONArray.AddObject : TdwsJSONObject;
begin
   Result:=vObject.Create;
   Add(Result);
end;

// AddArray
//
function TdwsJSONArray.AddArray : TdwsJSONArray;
begin
   Result:=vArray.Create;
   Add(Result);
end;

// AddValue
//
function TdwsJSONArray.AddValue : TdwsJSONImmediate;
begin
   Result := AllocImmediate;
   Add(Result);
end;

// AddNull
//
procedure TdwsJSONArray.AddNull;
var
   v : TdwsJSONImmediate;
begin
   v := AllocImmediate;
   v.IsNull:=True;
   Add(v);
end;

// Delete
//
procedure TdwsJSONArray.Delete(index : Integer);
begin
   if Cardinal(index) >= Cardinal(FCount) then
      raise EdwsJSONIndexOutOfRange.Create(index, FCount);
   DeleteIndex(index);
end;

// AddFrom
//
procedure TdwsJSONArray.AddFrom(other : TdwsJSONArray);
begin
   if other.FCount = 0 then Exit;

   for var i := 0 to other.FCount-1 do begin
      var elem := other.FElements^[i];
      if FCount=FCapacity then Grow;
      FElements^[FCount] := other.FElements^[i];
      elem.FOwner := Self;
      Inc(FCount);
   end;
   other.FCount := 0;
end;

// Sort
//
type
   TCompareAdapter = class
      ValueArray : PdwsJSONValueArray;
      CompareMethod : TdwsJSONValueCompareMethod;
      function Compare(index1, index2 : NativeInt) : Integer;
   end;
   function TCompareAdapter.Compare(index1, index2 : NativeInt) : Integer;
   begin
      Result:=CompareMethod(ValueArray[index1], ValueArray[Index2]);
   end;
procedure TdwsJSONArray.Sort(const aCompareMethod : TdwsJSONValueCompareMethod);
var
   qs : TQuickSort;
   adapter : TCompareAdapter;
begin
   if FCount<=1 then Exit;

   adapter:=TCompareAdapter.Create;
   try
      adapter.ValueArray:=FElements;
      adapter.CompareMethod:=aCompareMethod;
      qs.CompareMethod:=adapter.Compare;
      qs.SwapMethod:=SwapNoRangeCheck;
      qs.Sort(0, FCount-1);
   finally
      adapter.Free;
   end;
end;

// SwapNoRangeCheck
//
procedure TdwsJSONArray.SwapNoRangeCheck(index1, index2 : NativeInt);
var
   temp : TdwsJSONValue;
begin
   temp := FElements[index1];
   FElements[index1] := FElements[index2];
   FElements[index2] := temp;
end;

// Swap
//
procedure TdwsJSONArray.Swap(index1, index2 : Integer);
begin
   if Cardinal(index1) >= Cardinal(FCount) then
      raise EdwsJSONIndexOutOfRange.Create(index1, FCount);
   if Cardinal(index2) >= Cardinal(FCount) then
      raise EdwsJSONIndexOutOfRange.Create(index1, FCount);

   SwapNoRangeCheck(index1, index2);
end;

// DoGetName
//
function TdwsJSONArray.DoGetName(index : Integer) : UnicodeString;
begin
   if Cardinal(index) < Cardinal(FCount) then
      Result := Int32ToStrU(index)
   else Result := '';
end;

// DoGetElement
//
function TdwsJSONArray.DoGetElement(index : Integer) : TdwsJSONValue;
begin
   if Cardinal(index) < Cardinal(FCount) then
      Result:=TdwsJSONValue(FElements^[index])
   else Result:=nil;
end;

// DoSetElement
//
procedure TdwsJSONArray.DoSetElement(index : Integer; const value : TdwsJSONValue);
var
   v : TdwsJSONValue;
begin
   if index<0 then
      raise EdwsJSONException.CreateFmt('Invalid array index "%d"', [index]);

   if (value<>nil) and (value.Owner<>nil) then begin
      value.IncRefCount;
      value.Owner.DetachChild(value);
   end;

   if index<FCount then begin

      if value=nil then
         if index=FCount-1 then begin
            DeleteIndex(index);
            Exit;
         end else v := AllocImmediate
      else v:=value;

      FElements[index].ClearOwner;
      FElements[index].DecRefCount;
      FElements[index]:=v;
      v.FOwner:=Self;

   end else if value<>nil then begin

      while FCount<index do
         AddValue;

      Add(value);

   end;
end;

// DoGetItem
//
function TdwsJSONArray.DoGetItem(const name : UnicodeString) : TdwsJSONValue;
var
   i : Integer;
begin
   i := StrUToInt64(name, -1);
   Result:=DoGetElement(i);
end;

// DoSetItem
//
procedure TdwsJSONArray.DoSetItem(const name : UnicodeString; const value : TdwsJSONValue);
var
   i : Integer;
begin
   i := StrUToInt64(name, -1);
   if i < 0 then
      raise EdwsJSONException.CreateFmt('Invalid array member "%s"', [name])
   else DoSetElement(i, value);
end;

// DoParse
//
procedure TdwsJSONArray.DoParse(initialChar : WideChar; parserState : TdwsJSONParserState);
var
   locValue : TdwsJSONValue;
begin
   Assert(initialChar='[');
   repeat
      locValue:=TdwsJSONValue.Parse(parserState);
      if locValue=nil then Break;
      Add(locValue);
      parserState.TrailCharacter:=parserState.SkipBlanks(parserState.TrailCharacter);
   until parserState.TrailCharacter<>',';
   if parserState.TrailCharacter<>']' then
      RaiseJSONParseError('Invalid array termination character "%s"', parserState.TrailCharacter);
   parserState.TrailCharacter:=' ';
end;

// DoClone
//
function TdwsJSONArray.DoClone : TdwsJSONValue;
begin
   var arr := vArray.Create;
   arr.SetCapacity(FCount);
   arr.FCount := FCount;
   for var i := 0 to FCount-1 do begin
      var elem := FElements^[i].Clone;
      elem.FOwner := arr;
      arr.FElements^[i] := elem;
   end;
   Result := arr;
end;

// DoExtend
//
procedure TdwsJSONArray.DoExtend(other : TdwsJSONValue);
begin
   if other.ClassType<>TdwsJSONArray then
      RaiseJSONException('Can only extend Array with Array');
   var otherArr := TdwsJSONArray(other);
   for var i := 0 to otherArr.FCount-1 do
      Add(otherArr.FElements^[i].Clone);
end;

// SetOwner
//
procedure TdwsJSONArray.SetOwner(aOwner : TdwsJSONValue);
begin
   if aOwner.HasInOwners(Self) then
      RaiseJSONException('JSON circular reference');
   inherited SetOwner(aOwner);
end;

// ------------------
// ------------------ TdwsJSONImmediate ------------------
// ------------------

// GetType
//
function TdwsJSONImmediate.GetType : TdwsJSONValueType;
begin
   Result := TdwsJSONValueType(FRawOwner and $7);
end;

// SetType
//
procedure TdwsJSONImmediate.SetType(t : TdwsJSONValueType);
begin
   FRawOwner:=(FRawOwner and -8) or NativeUInt(t);
end;

// Destroy
//
destructor TdwsJSONImmediate.Destroy;
begin
   Clear;
   inherited;
end;

// Release
//
procedure TdwsJSONImmediate.Release;
begin
   ReleaseImmediate(Self);
end;

// GetAsString
//
function TdwsJSONImmediate.GetAsString : UnicodeString;
begin
   if Assigned(Self) then
      case FType of
         jvtNull : Result := 'null';
         jvtString : Result := PUnicodeString(@FData)^;
         jvtNumber : Result := UnicodeString(FloatToStr(FData));
         jvtInt64 : Result := UnicodeString(IntToStr(PInt64(@FData)^));
         jvtBoolean :
            if PBoolean(@FData)^ then
               Result:='true'
            else Result:='false';
      else
         Result:='undefined';
      end
   else Result:='undefined';
end;

// SetAsString
//
procedure TdwsJSONImmediate.SetAsString(const val : UnicodeString);
begin
   if FType<>jvtString then
      PPointer(@FData)^:=nil;
   PUnicodeString(@FData)^:=val;
   FType:=jvtString;
end;

// GetIsNull
//
function TdwsJSONImmediate.GetIsNull : Boolean;
begin
   if Assigned(Self) then
      Result:=(FType=jvtNull)
   else Result:=True;
end;

// SetIsNull
//
procedure TdwsJSONImmediate.SetIsNull(const val : Boolean);
begin
   if val<>GetIsNull then begin
      if val then begin
         if FType=jvtString then
            PUnicodeString(@FData)^:='';
         FType:=jvtNull;
      end else AsString:='';
   end;
end;

// GetAsBoolean
//
function TdwsJSONImmediate.GetAsBoolean : Boolean;
begin
   if not Assigned(Self) then Exit(False);
   case FType of
      jvtBoolean : Result:=PBoolean(@FData)^;
      jvtNumber : Result:=(FData<>0);
      jvtInt64  : Result := (PInt64(@FData)^ <> 0);
      jvtString : Result:=(PUnicodeString(@FData)^='true');
   else
      Result:=False;
   end;
end;

// SetAsBoolean
//
procedure TdwsJSONImmediate.SetAsBoolean(const val : Boolean);
begin
   if FType=jvtString then
      PUnicodeString(@FData)^:='';
   PBoolean(@FData)^:=val;
   FType:=jvtBoolean;
end;

// GetAsNumber
//
function TdwsJSONImmediate.GetAsNumber : Double;
begin
   if not Assigned(Self) then Exit(NaN);
   case FType of
      jvtBoolean : if PBoolean(@FData)^ then Result:=-1 else Result:=0;
      jvtNumber : Result := FData;
      jvtInt64 : Result := PInt64(@FData)^;
      jvtString : Result := StrToFloatDef(String(PUnicodeString(@FData)^), 0)
   else
      Result:=0;
   end;
end;

// SetAsNumber
//
procedure TdwsJSONImmediate.SetAsNumber(const val : Double);
begin
   if FType = jvtString then
      PUnicodeString(@FData)^ := '';
   FData := val;
   FType := jvtNumber;
end;

// GetAsInteger
//
function TdwsJSONImmediate.GetAsInteger : Int64;
begin
   if FType = jvtInt64 then
      Result := PInt64(@FData)^
   else Result := Round(GetAsNumber);
end;

// SetAsInteger
//
procedure TdwsJSONImmediate.SetAsInteger(const val : Int64);
begin
   if FType = jvtString then
      PUnicodeString(@FData)^ := '';
   PInt64(@FData)^ := val;
   FType := jvtInt64;
end;

// GetIsImmediateValue
//
function TdwsJSONImmediate.GetIsImmediateValue : Boolean;
begin
   Result:=True;
end;

// DoParse
//
procedure TdwsJSONImmediate.DoParse(initialChar : WideChar; parserState : TdwsJSONParserState);
begin
   parserState.TrailCharacter:=' ';
   case initialChar of
      '"' : begin
         parserState.ParseJSONUnicodeString(initialChar, PUnicodeString(@FData)^);
         FType:=jvtString;
      end;
      '0'..'9', '-' : begin
         var jvt : TdwsJSONValueType;
         parserState.ParseJSONNumber(initialChar, FData, jvt);
         FType := jvt;
      end;
      't' :
         if     (parserState.NeedChar()='r')
            and (parserState.NeedChar()='u')
            and (parserState.NeedChar()='e') then
            AsBoolean:=True
         else RaiseJSONParseError('Invalid immediate value');
      'f' :
         if     (parserState.NeedChar()='a')
            and (parserState.NeedChar()='l')
            and (parserState.NeedChar()='s')
            and (parserState.NeedChar()='e') then
            AsBoolean:=False
         else RaiseJSONParseError('Invalid immediate value');
      'n' :
         if     (parserState.NeedChar()='u')
            and (parserState.NeedChar()='l')
            and (parserState.NeedChar()='l') then
            IsNull:=True
         else RaiseJSONParseError('Invalid immediate value');
      'N' :
         if     (parserState.NeedChar()='a')
            and (parserState.NeedChar()='N') then
            AsNumber := 0/0
         else RaiseJSONParseError('Invalid immediate value');
      'I' :
         if     (parserState.NeedChar()='n')
            and (parserState.NeedChar()='f')
            and (parserState.NeedChar()='i')
            and (parserState.NeedChar()='n')
            and (parserState.NeedChar()='i')
            and (parserState.NeedChar()='t')
            and (parserState.NeedChar()='y') then
            AsNumber := 1/0
         else RaiseJSONParseError('Invalid immediate value');
   else
      RaiseJSONParseError('Invalid immediate value');
   end;
end;

// DoClone
//
function TdwsJSONImmediate.DoClone : TdwsJSONValue;
begin
   Result := AllocImmediate;
   TdwsJSONImmediate(Result).FType:=FType;
   if FType=jvtString then
      PUnicodeString(@TdwsJSONImmediate(Result).FData)^:=PUnicodeString(@FData)^
   else PInt64(@TdwsJSONImmediate(Result).FData)^:=PInt64(@FData)^;
end;

// DoExtend
//
procedure TdwsJSONImmediate.DoExtend(other : TdwsJSONValue);
begin
   RaiseJSONException('Cannot extend immediate values');
end;

// DoIsFalsey
//
function TdwsJSONImmediate.DoIsFalsey : Boolean;
begin
   case FType of
      jvtUndefined, jvtNull :
         Result:=True;
      jvtString :
         Result:=PUnicodeString(@FData)^='';
      jvtNumber :
         Result := (FData=0);
      jvtInt64 :
         Result := (PInt64(@FData)^ = 0);
      jvtBoolean :
         Result:=not PBoolean(@FData)^;
   else
      Result:=False;
   end;
end;

// Clone
//
function TdwsJSONImmediate.Clone : TdwsJSONImmediate;
begin
   if Self<>nil then
      Result:=(DoClone as TdwsJSONImmediate)
   else Result:=nil;
end;

// WriteTo
//
procedure TdwsJSONImmediate.WriteTo(writer : TdwsJSONWriter);
begin
   case FType of
      jvtNull, jvtUndefined :
         writer.WriteNull;
      jvtBoolean :
         writer.WriteBoolean(PBoolean(@FData)^);
      jvtNumber :
         writer.WriteNumber(FData);
      jvtInt64 :
         writer.WriteInteger(PInt64(@FData)^);
      jvtString :
         writer.WriteString(PUnicodeString(@FData)^);
   else
      Assert(False, 'Unsupported type');
   end;
end;

// ParseString
//
class function TdwsJSONImmediate.ParseString(const json : UnicodeString) : TdwsJSONImmediate;
var
   locValue : TdwsJSONValue;
begin
   locValue:=TdwsJSONValue.ParseString(json);
   if locValue is TdwsJSONImmediate then
      Result:=TdwsJSONImmediate(locValue)
   else begin
      locValue.Free;
      Result:=nil;
   end;
end;

// FromVariant
//
class function TdwsJSONImmediate.FromVariant(const v : Variant) : TdwsJSONImmediate;
begin
   Result:=TdwsJSONImmediate.Create;
   Result.AsVariant:=v;
end;

// CreateNull
//
class function TdwsJSONImmediate.CreateNull : TdwsJSONImmediate;
begin
   Result := TdwsJSONImmediate.Create;
   Result.FType := jvtNull;
end;

// DoSetItem
//
procedure TdwsJSONImmediate.DoSetItem(const name : UnicodeString; const value : TdwsJSONValue);
begin
   raise EdwsJSONException.CreateFmt('Cannot set member "%s" of immediate value', [name]);
end;

// DoSetElement
//
procedure TdwsJSONImmediate.DoSetElement(index : Integer; const value : TdwsJSONValue);
begin
   raise EdwsJSONException.CreateFmt('Cannot set element "%d" of immediate value', [index]);
end;

// GetValueType
//
function TdwsJSONImmediate.GetValueType : TdwsJSONValueType;
begin
   Result:=FType;
end;

// GetAsVariant
//
procedure TdwsJSONImmediate.GetAsVariant(var result : Variant);
begin
   case FType of
      jvtNull : VarSetNull(Result);
      jvtString : VarCopySafe(Result, PUnicodeString(@FData)^);
      jvtNumber : VarCopySafe(Result, FData);
      jvtInt64 : VarCopySafe(Result, PInt64(@FData)^);
      jvtBoolean : VarCopySafe(Result, PBoolean(@FData)^);
   else
      VarClearSafe(Result);
   end;
end;

// Clear
//
procedure TdwsJSONImmediate.Clear;
begin
   if FType = jvtString then
      PUnicodeString(@FData)^ := '';
   PPointer(@FData)^ := nil;
   FType := jvtUndefined;
end;

// GetAsVariant
//
function TdwsJSONImmediate.GetAsVariant : Variant;
begin
   GetAsVariant(Result);
end;

// SetAsVariant
//
procedure TdwsJSONImmediate.SetAsVariant(const val : Variant);
begin
   case VariantType(val) of
      varEmpty : Clear;
      varNull : IsNull := True;
      varInt64 : AsInteger := val;
      {$ifdef FPC}
      varString : AsString := val;
      {$else}
      varUString : AsString := val;
      {$endif}
      varDouble : AsNumber := val;
      varBoolean : AsBoolean := val;
   else
      if VariantIsString(val) then
         AsString := VariantToUnicodeString(val)
      else if VariantIsFloat(val) or VariantIsOrdinal(val) then
         AsNumber := VariantToFloat(val)
      else raise EdwsJSONException.CreateFmt('Unsupported VarType in FromVariant (%d)',
                                             [VariantType(val)]);
   end;
end;

// ------------------
// ------------------ TdwsJSONWriter ------------------
// ------------------

// Create
//
constructor TdwsJSONWriter.Create(aStream : TWriteOnlyBlockStream = nil;
                                  aOptions : TdwsJSONWriterOptions = []);
begin
   inherited Create;
   FOptions:=aOptions;
   FOwnsStream:=(aStream=nil);
   if FOwnsStream then
      FStream:=TWriteOnlyBlockStream.AllocFromPool
   else FStream:=aStream;
end;

// Destroy
//
destructor TdwsJSONWriter.Destroy;
begin
   if FOwnsStream then
      FStream.ReturnToPool;
   FStateStack.Free;
   inherited;
end;

// BeginObject
//
procedure TdwsJSONWriter.BeginObject;
begin
   Assert(FState in [wsNone, wsObjectValue, wsArray, wsArrayValue]);
   FStateStack.Push(TRefCountedObject(FState));
   BeforeWriteImmediate;
   FState:=wsObject;
   FStream.WriteChar('{');
end;

// BeginObject
//
procedure TdwsJSONWriter.BeginObject(const aName : UnicodeString);
begin
   WriteName(aName).BeginObject;
end;

// EndObject
//
procedure TdwsJSONWriter.EndObject;
begin
   if FState in [wsObject, wsObjectName] then begin
      Assert(FStateStack.Count > 0);
      FState := TdwsJSONWriterState(FStateStack.PeekAndPop);
      FStream.WriteChar('}');
      AfterWriteImmediate;
   end else raise EdwsJSONWriterError.Create('Value expected or not in object');
end;

// BeginArray
//
procedure TdwsJSONWriter.BeginArray;
begin
   Assert(FState in [wsNone, wsObjectValue, wsArray, wsArrayValue]);
   FStateStack.Push(TRefCountedObject(FState));
   BeforeWriteImmediate;
   FState:=wsArray;
   FStream.WriteChar('[');
end;

// BeginArray
//
procedure TdwsJSONWriter.BeginArray(const aName : UnicodeString);
begin
   WriteName(aName).BeginArray;
end;

// EndArray
//
procedure TdwsJSONWriter.EndArray;
begin
   if FState in [wsArray, wsArrayValue] then begin
      Assert(FStateStack.Count>0);
      FState := TdwsJSONWriterState(FStateStack.PeekAndPop);
      FStream.WriteChar(']');
      AfterWriteImmediate;
   end else raise EdwsJSONWriterError.Create('Not in array');
end;

// WriteName
//
function TdwsJSONWriter.WriteName(const aName : UnicodeString) : TdwsJSONWriter;
begin
   Result:=WriteNameP(PWideChar(Pointer(aName)), Length(aName));
end;

// WriteNameP
//
function TdwsJSONWriter.WriteNameP(const aName : PWideChar; nbChars : Integer) : TdwsJSONWriter;

   procedure WriteLowerCase(stream : TWriteOnlyBlockStream; aName : PWideChar; nbChars : Integer);
   var
      name, buf : String;
   begin
      SetString(name, aName, nbChars);
      UnicodeLowerCase(name, buf);
      WriteJavaScriptString(FStream, buf);
   end;

begin
   case FState of
      wsObject : ;
      wsObjectName : begin
         FStream.WriteChar(',');
      end;
   else
      Assert(False);
   end;
   if woLowerCaseNames in FOptions then
      WriteLowerCase(FStream, aName, nbChars)
   else WriteJavaScriptString(FStream, aName, nbChars);
   FStream.WriteChar(':');
   FState:=wsObjectValue;
   Result:=Self;
end;

// WriteString
//
procedure TdwsJSONWriter.WriteString(const str : UnicodeString);
begin
   BeforeWriteImmediate;
   WriteJavaScriptString(FStream, str);
   AfterWriteImmediate;
end;

{$ifdef FPC}
// WriteName
//
function TdwsJSONWriter.WriteName(const aName : String) : TdwsJSONWriter;
begin
   Result:=WriteName(UnicodeString(aName));
end;

// WriteString
//
procedure TdwsJSONWriter.WriteString(const str : String);
begin
   WriteString(UnicodeString(str));
end;

// WriteString
//
procedure TdwsJSONWriter.WriteString(const name, str : String);
begin
   WriteName(UnicodeString(name)).WriteString(UnicodeString(str));
end;
{$endif}

// WriteString
//
procedure TdwsJSONWriter.WriteString(const name, str : UnicodeString);
begin
   WriteName(name).WriteString(str);
end;

// WriteStringP
//
procedure TdwsJSONWriter.WriteStringP(const p : PWideChar; nbChars : Integer);
begin
   BeforeWriteImmediate;
   WriteJavaScriptString(FStream, p, nbChars);
   AfterWriteImmediate;
end;

// WriteNumber
//
procedure TdwsJSONWriter.WriteNumber(const n : Double);
var
   buffer : array [0..63] of WideChar;
begin
   BeforeWriteImmediate;
   if n = 0 then
      FStream.WriteString('0')
   else begin
      var st := n.SpecialType;
      if st in [fsInf, fsNInf, fsNaN] then begin
         if woNaNNumbersJSON5 in FOptions then
            case st of
               fsInf : FStream.WriteString('Infinity');
               fsNInf : FStream.WriteString('-Infinity');
            else
               FStream.WriteString('NaN');
            end
         else FStream.WriteString('null');
      end else begin
         if (Abs(n) <= High(Int64)) and (Round(n) = n) then
            FStream.WriteString(Round(n))
         else begin
            var nExt : Extended := n;
            var nc := FloatToText(buffer, nExt, fvExtended, ffGeneral, 15, 0, vJSONFormatSettings);
            FStream.Write(buffer, nc*SizeOf(WideChar));
         end;
      end;
   end;
   AfterWriteImmediate;
end;

// WriteNumber
//
procedure TdwsJSONWriter.WriteNumber(const name : UnicodeString; const n : Double);
begin
   WriteName(name).WriteNumber(n);
end;

// WriteInteger
//
procedure TdwsJSONWriter.WriteInteger(const n : Int64);
begin
   BeforeWriteImmediate;
   FStream.WriteString(n);
   AfterWriteImmediate;
end;

// WriteInteger
//
procedure TdwsJSONWriter.WriteInteger(const name : UnicodeString; const n : Int64);
begin
   WriteName(name).WriteInteger(n);
end;

// WriteBoolean
//
procedure TdwsJSONWriter.WriteBoolean(b : Boolean);
begin
   BeforeWriteImmediate;
   if b then
      FStream.WriteString('true')
   else FStream.WriteString('false');
   AfterWriteImmediate;
end;

// WriteBoolean
//
procedure TdwsJSONWriter.WriteBoolean(const name : UnicodeString; b : Boolean);
begin
   WriteName(name).WriteBoolean(b);
end;

// WriteNull
//
procedure TdwsJSONWriter.WriteNull;
begin
   BeforeWriteImmediate;
   FStream.WriteString('null');
   AfterWriteImmediate;
end;

// WriteDate
//
procedure TdwsJSONWriter.WriteDate(dt : TDateTime; utc : Boolean = False);
var
   y, m, d, h, n, s, z : Word;
begin
   if dt = 0 then begin
      WriteNull;
      Exit;
   end;

   BeforeWriteImmediate;

   FStream.WriteChar('"');

   DecodeDate(dt, y, m, d);
   FStream.WriteDigits(y, 4);
   FStream.WriteChar('-');
   FStream.WriteDigits(m, 2);
   FStream.WriteChar('-');
   FStream.WriteDigits(d, 2);

   DecodeTime(dt, h, n, s, z);
   if (h or n or s)<>0 then begin
      FStream.WriteChar('T');
      FStream.WriteDigits(h, 2);
      FStream.WriteChar(':');
      FStream.WriteDigits(n, 2);
      FStream.WriteChar(':');
      FStream.WriteDigits(s, 2);
   end;

   if utc then
      FStream.WriteString('Z"')
   else FStream.WriteChar('"');

   AfterWriteImmediate;
end;

// WriteUnixTime
//
procedure TdwsJSONWriter.WriteUnixTime(dt : TDateTime; utc : Boolean = False);
var
   d : TdwsDateTime;
begin
   if utc then
      d.AsUTCDateTime := dt
   else d.AsLocalDateTime := dt;
   WriteInteger(d.AsUnixTime);
end;

// WriteStrings
//
procedure TdwsJSONWriter.WriteStrings(const str : TStrings);
var
   i : Integer;
begin
   BeginArray;
   for i:=0 to str.Count-1 do
      WriteString(UnicodeString(str[i]));
   EndArray;
end;

// WriteStrings
//
procedure TdwsJSONWriter.WriteStrings(const str : TUnicodeStringList);
var
   i : Integer;
begin
   BeginArray;
   for i:=0 to str.Count-1 do
      WriteString(str[i]);
   EndArray;
end;

// WriteJSON
//
procedure TdwsJSONWriter.WriteJSON(const json : UnicodeString);
begin
   BeforeWriteImmediate;
   FStream.WriteString(json);
   AfterWriteImmediate;
end;

// WriteVariant
//
procedure TdwsJSONWriter.WriteVariant(const v : Variant);
var
   i : Integer;
begin
   if VarIsArray(v) then begin
      BeginArray;
      for i := VarArrayLowBound(v, 1) to VarArrayHighBound(v, 1) do
         WriteVariant(v[i]);
      EndArray;
   end else case VarType(v) of
      varInteger, varInt64, varSmallint, varShortInt,
      varUInt64, varWord, varByte, varLongWord :
         WriteInteger(v);
      varSingle, varDouble :
         WriteNumber(v);
      varBoolean :
         WriteBoolean(v);
      varDate :
         WriteDate(v);
      varNull, varEmpty :
         WriteNull;
   else
      WriteString(v);
   end;
end;

// StoreToUnicodeString
//
procedure TdwsJSONWriter.StoreToUnicodeString(var dest : UnicodeString);
begin
   FStream.StoreData(dest);
end;

// WriteStrings
//
procedure TdwsJSONWriter.WriteStrings(const str : array of UnicodeString);
var
   i : Integer;
begin
   BeginArray;
   for i:=0 to High(str) do
      WriteString(str[i]);
   EndArray;
end;

// ToString
//
function TdwsJSONWriter.ToString : String;
begin
   StoreToUnicodeString(Result);
end;

// ToUnicodeString
//
function TdwsJSONWriter.ToUnicodeString : UnicodeString;
begin
   StoreToUnicodeString(Result);
end;

// ToUTF8String
//
function TdwsJSONWriter.ToUTF8String : RawByteString;
begin
   Result:=FStream.ToUTF8String;
end;

// BeforeWriteImmediate
//
procedure TdwsJSONWriter.BeforeWriteImmediate;
begin
   case FState of
      wsArrayValue :
         FStream.WriteChar(',');
      wsObject :
         raise EdwsJSONWriterError.Create('Name expected');
      wsDone :
         Assert(False);
   end;
end;

// AfterWriteImmediate
//
procedure TdwsJSONWriter.AfterWriteImmediate;
begin
   case FState of
      wsNone :
         FState := wsDone;
      wsArray :
         FState := wsArrayValue;
      wsObjectValue :
         FState := wsObjectName;
   end;
end;

// ------------------
// ------------------ TdwsJSONBeautifiedWriter ------------------
// ------------------

// Create
//
constructor TdwsJSONBeautifiedWriter.Create(aStream : TWriteOnlyBlockStream; initialSpaces, spacesPerIndent : Integer; const spaceCharacter : String = #9);
begin
   inherited Create(aStream);
   FSpaces := initialSpaces;
   FSpacesPerIndent := spacesPerIndent;
   FSpaceCharacter := spaceCharacter;
end;

// WriteIndents
//
procedure TdwsJSONBeautifiedWriter.WriteIndents;

   procedure PrepareIndents;
   begin
   end;

var
   target, current : Integer;
begin
   target := FSpaces * FSpaceCharacter.Length;
   current := Length(FIndents);
   if current <> target then begin
      if current > target then
         SetLength(FIndents, target)
      else while current < target do begin
         FIndents := FIndents + FSpaceCharacter;
         current := Length(FIndents);
      end;
   end;

   if target > 0 then
      FStream.WriteString(FIndents);
end;

// EnterIndent
//
procedure TdwsJSONBeautifiedWriter.EnterIndent;
begin
   Inc(FSpaces, FSpacesPerIndent);
end;

// LeaveIndent
//
procedure TdwsJSONBeautifiedWriter.LeaveIndent;
begin
   Dec(FSpaces, FSpacesPerIndent);
   if FState in [wsObjectName, wsArrayValue] then begin
      FStream.WriteString(#13#10);
      WriteIndents;
   end else FStream.WriteChar(' ');
end;

// BeforeWriteImmediate
//
procedure TdwsJSONBeautifiedWriter.BeforeWriteImmediate;
begin
   inherited;
   case FState of
      wsArray, wsArrayValue : begin
         FStream.WriteString(#13#10);
         WriteIndents;
      end;
   end;
end;

// BeginObject
//
procedure TdwsJSONBeautifiedWriter.BeginObject;
begin
   inherited;
   EnterIndent;
end;

// EndObject
//
procedure TdwsJSONBeautifiedWriter.EndObject;
begin
   LeaveIndent;
   inherited;
end;

// BeginArray
//
procedure TdwsJSONBeautifiedWriter.BeginArray;
begin
   inherited;
   EnterIndent;
end;

// EndArray
//
procedure TdwsJSONBeautifiedWriter.EndArray;
begin
   LeaveIndent;
   inherited;
end;

// WriteNameP
//
function TdwsJSONBeautifiedWriter.WriteNameP(const aName : PWideChar; nbChars : Integer) : TdwsJSONWriter;
begin
   case FState of
      wsObject :
         FStream.WriteString(#13#10);
      wsObjectName :
         FStream.WriteString(','#13#10);
   else
      Assert(False);
   end;
   WriteIndents;
   WriteJavaScriptString(FStream, aName, nbChars);
   FStream.WriteString(' : ');
   FState:=wsObjectValue;
   Result:=Self;
end;

// ------------------
// ------------------ TdwsJSONValueList ------------------
// ------------------

// ToString
//
function TdwsJSONValueList.ToString : String;
var
   wr : TdwsJSONWriter;
   wobs : TWriteOnlyBlockStream;
begin
   if Count=0 then Exit('[]');
   wobs:=TWriteOnlyBlockStream.AllocFromPool;
   wr:=TdwsJSONWriter.Create(wobs);
   try
      WriteTo(wr);
      Result:=wobs.ToString;
   finally
      wr.Free;
      wobs.ReturnToPool;
   end;
end;

// ToUnicodeString
//
function TdwsJSONValueList.ToUnicodeString : UnicodeString;
var
   wr : TdwsJSONWriter;
   wobs : TWriteOnlyBlockStream;
begin
   if Count=0 then Exit('[]');
   wobs:=TWriteOnlyBlockStream.AllocFromPool;
   wr:=TdwsJSONWriter.Create(wobs);
   try
      WriteTo(wr);
      Result:=wobs.ToUnicodeString;
   finally
      wr.Free;
      wobs.ReturnToPool;
   end;
end;

// WriteTo
//
procedure TdwsJSONValueList.WriteTo(writer : TdwsJSONWriter);
var
   i : Integer;
begin
   writer.BeginArray;
   for i:=0 to Count-1 do
      Items[i].WriteTo(writer);
   writer.EndArray;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vJSONFormatSettings := FormatSettings;
   vJSONFormatSettings.DecimalSeparator:='.';

   vImmediate.Initialize(TdwsJSONImmediate.Create);
   vObject.Initialize(TdwsJSONObject.Create);
   vArray.Initialize(TdwsJSONArray.Create);

   vImmediatePoolLock := TMultiReadSingleWrite.Create;

finalization

   vImmediate.Finalize;
   vObject.Finalize;
   vArray.Finalize;

   FlushImmediates;
   FreeAndNil(vImmediatePoolLock);

end.
