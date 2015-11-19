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

interface

uses
   Classes, SysUtils, Variants, Math,
   dwsUtils, dwsXPlatform;

type

   TdwsJSONArray = class;
   TdwsJSONObject = class;
   TdwsJSONImmediate = class;

   TdwsJSONWriterState = (wsNone, wsObject, wsObjectName, wsObjectValue, wsArray, wsArrayValue, wsDone);

   // TdwsJSONWriter
   //
   TdwsJSONWriter = class
      private
         FStream : TWriteOnlyBlockStream;
         FOwnsStream : Boolean;
         FStateStack : TTightStack;
         FState : TdwsJSONWriterState;

      protected
         procedure BeforeWriteImmediate; virtual;
         procedure AfterWriteImmediate;

      public
         constructor Create(aStream : TWriteOnlyBlockStream);
         destructor Destroy; override;

         procedure BeginObject; overload; virtual;
         procedure BeginObject(const aName : UnicodeString); overload; inline;
         procedure EndObject; virtual;

         procedure BeginArray; overload; virtual;
         procedure BeginArray(const aName : UnicodeString); overload; inline;
         procedure EndArray; virtual;

         function  WriteName(const aName : UnicodeString) : TdwsJSONWriter; overload; inline;
         function  WriteName(const aName : PWideChar) : TdwsJSONWriter; overload; virtual;
         procedure WriteString(const str : UnicodeString); overload; inline;
         procedure WriteString(const name, str : UnicodeString); overload; inline;
         procedure WriteString(const p : PWideChar); overload;
         procedure WriteNumber(const n : Double); overload;
         procedure WriteNumber(const name : UnicodeString; const n : Double); overload; inline;
         procedure WriteInteger(const n : Int64); overload;
         procedure WriteInteger(const name : UnicodeString; const n : Int64); overload; inline;
         procedure WriteBoolean(b : Boolean); overload;
         procedure WriteBoolean(const name : UnicodeString; b : Boolean); overload; inline;
         procedure WriteNull; overload;

         // ISO 8601 Date Time
         procedure WriteDate(dt : TDateTime); overload;

         procedure WriteStrings(const str : TStrings); overload;
         procedure WriteStrings(const str : array of UnicodeString); overload;

         function ToString : String; override;

         property Stream : TWriteOnlyBlockStream read FStream write FStream;
   end;

   // TdwsJSONBeautifiedWriter
   //
   TdwsJSONBeautifiedWriter = class (TdwsJSONWriter)
      private
         FTabs : Integer;
         FIndent : Integer;

         procedure WriteIndents;
         procedure EnterIndent;
         procedure LeaveIndent;

      protected
         procedure BeforeWriteImmediate; override;

      public
         constructor Create(aStream : TWriteOnlyBlockStream; initialTabs, indentTabs : Integer);

         procedure BeginObject; override;
         procedure EndObject; override;

         procedure BeginArray; override;
         procedure EndArray; override;

         function  WriteName(const aName : PWideChar) : TdwsJSONWriter; override;
   end;

   TdwsJSONDuplicatesOptions = (jdoAccept, jdoOverwrite);

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

      public
         constructor Create(const aStr : UnicodeString);

         function Location : UnicodeString;

         property TrailCharacter : WideChar read FTrailCharacter write FTrailCharacter;
         function NeedChar : WideChar; inline;
         function SkipBlanks(currentChar : WideChar) : WideChar; inline;

         procedure ParseJSONString(initialChar : WideChar; var result : UnicodeString);
         procedure ParseHugeJSONNumber(initialChars : PWideChar; initialCharCount : Integer; var result : Double);
         procedure ParseJSONNumber(initialChar : WideChar; var result : Double);

         // reads from [ to ]
         procedure ParseIntegerArray(dest : TSimpleInt64List);
         procedure ParseNumberArray(dest : TSimpleDoubleList);
         procedure ParseStringArray(dest : TStringList);
   end;

   TdwsJSONValueType = (jvtUndefined, jvtNull, jvtObject, jvtArray, jvtString, jvtNumber, jvtBoolean);

   // TdwsJSONValue
   //
   TdwsJSONValue = class (TRefCountedObject)
      private
         FRawOwner : NativeUInt; // 3 low order bytes are reserved for immediates

      protected
         function GetOwner : TdwsJSONValue; inline;
         procedure SetOwner(aOwner : TdwsJSONValue); inline;
         procedure ClearOwner; inline;

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
         function DoGetItem(const name : UnicodeString) : TdwsJSONValue; virtual;
         procedure SetItem(const name : UnicodeString; const value : TdwsJSONValue); inline;
         procedure DoSetItem(const name : UnicodeString; const value : TdwsJSONValue); virtual; abstract;
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

         class procedure RaiseJSONException(const msg : UnicodeString); static;
         class procedure RaiseJSONParseError(const msg : UnicodeString; c : WideChar = #0); static;

         class function Parse(parserState : TdwsJSONParserState) : TdwsJSONValue; static;

      public
         destructor Destroy; override;

         class function ParseString(const json : UnicodeString;
                                    duplicatesOption : TdwsJSONDuplicatesOptions = jdoOverwrite) : TdwsJSONValue; static;
         class function ParseFile(const fileName : UnicodeString) : TdwsJSONValue; static;

         function Clone : TdwsJSONValue;
         procedure Extend(other : TdwsJSONValue);

         procedure WriteTo(writer : TdwsJSONWriter); virtual; abstract;
         procedure WriteToStream(aStream : TStream); overload;
         procedure WriteToStream(aStream : TWriteOnlyBlockStream); overload;
         function ToString : UnicodeString; reintroduce;
         function ToBeautifiedString(initialTabs : Integer = 0; indentTabs : Integer = 1) : UnicodeString;
         procedure Detach;

         property Owner : TdwsJSONValue read GetOwner;
         property Items[const name : UnicodeString] : TdwsJSONValue read GetItem write SetItem;
         property Names[index : Integer] : UnicodeString read GetName;
         property Elements[index : Integer] : TdwsJSONValue read GetElement write SetElement;
         function ElementCount : Integer;
         property Values[const index : Variant] : TdwsJSONValue read GetValue write SetValue; default;

         function IsImmediateValue : Boolean; inline;
         function Value : TdwsJSONImmediate; inline;
         function ValueType : TdwsJSONValueType; inline;

         function IsFalsey : Boolean;

         property AsString : UnicodeString read GetAsString write SetAsString;
         property IsNull : Boolean read GetIsNull write SetIsNull;
         property IsDefined : Boolean read GetIsDefined;
         property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
         property AsNumber : Double read GetAsNumber write SetAsNumber;
         property IsNaN : Boolean read GetIsNaN;
         property AsInteger : Int64 read GetAsInteger write SetAsInteger;

         const ValueTypeStrings : array [TdwsJSONValueType] of UnicodeString = (
            'Undefined', 'Null', 'Object', 'Array', 'String', 'Number', 'Boolean'
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
         function ToString : String; override;
   end;

   TdwsJSONPair = record
      Name : UnicodeString;
      Hash : Cardinal;
      Value : TdwsJSONValue;
   end;
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
         procedure Grow;
         procedure SetCapacity(newCapacity : Integer);
         function IndexOfName(const name : UnicodeString) : Integer;
         function IndexOfValue(const aValue : TdwsJSONValue) : Integer;
         procedure DetachChild(child : TdwsJSONValue); override;
         procedure DetachIndex(i : Integer);

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

         function Clone : TdwsJSONObject;

         procedure Clear;

         procedure Add(const aName : UnicodeString; aValue : TdwsJSONValue);

         function AddObject(const name : UnicodeString) : TdwsJSONObject;

         function AddArray(const name : UnicodeString) : TdwsJSONArray;

         function AddValue(const name : UnicodeString) : TdwsJSONImmediate; overload;
         function AddValue(const name, value : UnicodeString) : TdwsJSONImmediate; overload;
         function AddValue(const name : UnicodeString; const value : Double) : TdwsJSONImmediate; overload;
         function AddValue(const name : UnicodeString; const value : Boolean) : TdwsJSONImmediate; overload;

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
         procedure Grow;
         procedure SetCapacity(newCapacity : Integer);
         procedure DetachChild(child : TdwsJSONValue); override;
         procedure DeleteIndex(idx : Integer);

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
         procedure Add(const aValue : String); overload;
         procedure Add(const aValue : Boolean); overload;
         function AddObject : TdwsJSONObject;
         function AddArray : TdwsJSONArray;
         function AddValue : TdwsJSONImmediate;
         procedure AddNull;

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
         function GetAsVariant : Variant;
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

         class function ParseString(const json : UnicodeString) : TdwsJSONImmediate; static;
         class function FromVariant(const v : Variant) : TdwsJSONImmediate; static;

         function Clone : TdwsJSONImmediate;

         procedure WriteTo(writer : TdwsJSONWriter); override;

         property AsVariant : Variant read GetAsVariant write SetAsVariant;
         property AsString : UnicodeString read GetAsString write SetAsString;
         property IsNull : Boolean read GetIsNull write SetIsNull;
         property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
         property AsNumber : Double read GetAsNumber write SetAsNumber;
         property AsInteger : Int64 read GetAsInteger write SetAsInteger;
   end;

   EdwsJSONException = class (Exception);
   EdwsJSONParseError = class (EdwsJSONException);
   EdwsJSONWriterError = class (EdwsJSONException);

procedure WriteJavaScriptString(destStream : TWriteOnlyBlockStream; const str : UnicodeString); overload; inline;
procedure WriteJavaScriptString(destStream : TWriteOnlyBlockStream; p : PWideChar); overload;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vJSONFormatSettings : TFormatSettings;
   vImmediate : TClassCloneConstructor<TdwsJSONImmediate>;
   vObject : TClassCloneConstructor<TdwsJSONObject>;
   vArray : TClassCloneConstructor<TdwsJSONArray>;

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
function TdwsJSONParserState.Location : UnicodeString;
begin
   if (Line=0) then begin
      Result:=Format('line 1, col %d',
                     [(NativeInt(Ptr)-NativeInt(ColStart)) div SizeOf(WideChar)]);
   end else begin
      Result:=Format('line %d, col %d (offset %d)',
                     [Line+1,
                      (NativeInt(Ptr)-NativeInt(ColStart)) div SizeOf(WideChar),
                      (NativeInt(Ptr)-NativeInt(PWideChar(Str))) div SizeOf(WideChar)]);
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

// ParseJSONString
//
procedure TdwsJSONParserState.ParseJSONString(initialChar : WideChar; var result : UnicodeString);
var
   c : WideChar;
   wobs : TWriteOnlyBlockStream;
   hexBuf, hexCount, n, nw : Integer;
   localBufferPtr, startPr : PWideChar;
   localBuffer : array [0..59] of WideChar; // range adjusted to have a stack space of 128 for the proc
begin
   startPr:=Ptr;
   wobs:=nil;
   try
      localBufferPtr:=@localBuffer[0];
      repeat
         c:=NeedChar();
         case c of
            #0..#31 :
               if c=#0 then begin
                  Ptr:=startPr;
                  TdwsJSONValue.RaiseJSONParseError('Unterminated string', c)
               end else TdwsJSONValue.RaiseJSONParseError('Invalid string character %s', c);
            '"' : Break;
            '\' : begin
               c:=NeedChar();
               case c of
                  '"', '\', '/' : localBufferPtr^:=c;
                  'n' : localBufferPtr^:=#10;
                  'r' : localBufferPtr^:=#13;
                  't' : localBufferPtr^:=#9;
                  'b' : localBufferPtr^:=#8;
                  'f' : localBufferPtr^:=#12;
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
                     localBufferPtr^:=WideChar(hexBuf);
                  end;
               else
                  TdwsJSONValue.RaiseJSONParseError('Invalid character "%s" after escape', c);
               end;
            end;
         else
            localBufferPtr^:=c;
         end;
         if localBufferPtr=@localBuffer[High(localBuffer)] then begin
            if wobs=nil then
               wobs:=TWriteOnlyBlockStream.AllocFromPool;
            wobs.Write(localBuffer[0], Length(localBuffer)*SizeOf(WideChar));
            localBufferPtr:=@localBuffer[0];
         end else Inc(localBufferPtr);
      until False;
      n:=(NativeInt(localBufferPtr)-NativeInt(@localBuffer[0])) shr (SizeOf(WideChar)-1);
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
end;

// ParseJSONNumber
//
procedure TdwsJSONParserState.ParseHugeJSONNumber(
      initialChars : PWideChar; initialCharCount : Integer; var result : Double);
var
   buf : UnicodeString;
   c : WideChar;
begin
   SetString(buf, initialChars, initialCharCount);
   repeat
      c:=NeedChar();
      case c of
         '0'..'9', '-', '+', 'e', 'E', '.' : buf:=buf+WideChar(c);
      else
         TrailCharacter:=c;
         Break;
      end;
   until False;
   Result:=StrToFloat(buf, vJSONFormatSettings);
end;

// ParseJSONNumber
//
procedure TdwsJSONParserState.ParseJSONNumber(initialChar : WideChar; var result : Double);
var
   bufPtr : PWideChar;
   c : WideChar;
   resultBuf : Extended;
   buf : array [0..50] of WideChar;
begin
   buf[0]:=initialChar;
   bufPtr:=@buf[1];
   repeat
      c:=NeedChar();
      case c of
         '0'..'9', '-', '+', 'e', 'E', '.' : begin
            bufPtr^:=c;
            Inc(bufPtr);
            if bufPtr=@buf[High(buf)] then begin
               ParseHugeJSONNumber(@buf[0], Length(buf)-1, result);
               Exit;
            end;
         end;
      else
         TrailCharacter:=c;
         Break;
      end;
   until False;
   case NativeUInt(bufPtr)-NativeUInt(@buf[0]) of
      SizeOf(WideChar) : // special case of single-character number
         case buf[0] of
            '0'..'9' : begin
               result:=Ord(buf[0])-Ord('0');
               exit;
            end;
         end;
      2*SizeOf(WideChar) : // special case of two-characters number
         case buf[0] of
            '1'..'9' : begin
               case buf[1] of
                  '0'..'9' : begin
                     result:=Ord(buf[0])*10+Ord(buf[1])-Ord('0')*11;
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
                           result:=Ord(buf[0])*100+Ord(buf[1])*10+Ord(buf[2])-Ord('0')*111;
                           exit;
                        end;
                     end;
                  end;
                  '.' : begin
                     case buf[2] of
                        '0'..'9' : begin
                           result:=(Ord(buf[0])-Ord('0'))+(Ord(buf[2])-Ord('0'))*0.1;
                           exit;
                        end;
                     end;
                  end;
               end;
            end;
         end;
   end;
   bufPtr^:=#0;
   TryTextToFloat(PWideChar(@buf[0]), resultBuf, vJSONFormatSettings);
   Result:=resultBuf;
end;

// ParseIntegerArray
//
procedure TdwsJSONParserState.ParseIntegerArray(dest : TSimpleInt64List);
var
   c : Char;
   num : Double;
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
            ParseJSONNumber(c, num);
            dest.Add(Round(num));
         end;
      else
         raise EdwsJSONParseError.CreateFmt('Unexpected character U+%.04x', [Ord(c)]);
      end;
      c:=SkipBlanks(TrailCharacter);
      case c of
         ',' : c:=SkipBlanks(NeedChar);
         ']' : break;
      else
         raise EdwsJSONParseError.CreateFmt('"," expected but U+%.04x encountered', [Ord(c)]);
      end;
   until False;
end;

// ParseNumberArray
//
procedure TdwsJSONParserState.ParseNumberArray(dest : TSimpleDoubleList);
var
   c : Char;
   num : Double;
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
            ParseJSONNumber(c, num);
            dest.Add(num);
         end;
      else
         raise EdwsJSONParseError.CreateFmt('Unexpected character U+%.04x', [Ord(c)]);
      end;
      c:=SkipBlanks(TrailCharacter);
      case c of
         ',' : c:=SkipBlanks(NeedChar);
         ']' : break;
      else
         raise EdwsJSONParseError.CreateFmt('"," expected but U+%.04x encountered', [Ord(c)]);
      end;
   until False;
end;

// ParseStringArray
//
procedure TdwsJSONParserState.ParseStringArray(dest : TStringList);
var
   c : Char;
   buf : String;
begin
   c:=SkipBlanks(' ');
   if c<>'[' then
      raise EdwsJSONParseError.CreateFmt('"[" expected but U+%.04x encountered', [Ord(c)]);
   c:=SkipBlanks(NeedChar);
   if c=']' then
      Exit;
   repeat
      if c='"' then begin
         ParseJSONString(c, buf);
         dest.Add(buf);
      end else begin
         raise EdwsJSONParseError.CreateFmt('Unexpected character U+%.04x', [Ord(c)]);
      end;
      c:=SkipBlanks(NeedChar);
      case c of
         ',' : c:=SkipBlanks(NeedChar);
         ']' : break;
      else
         raise EdwsJSONParseError.CreateFmt('"," expected but U+%.04x encountered', [Ord(c)]);
      end;
   until False;
end;

// WriteJavaScriptString
//
procedure WriteJavaScriptString(destStream : TWriteOnlyBlockStream; const str : UnicodeString);
begin
   WriteJavaScriptString(destStream, PWideChar(Pointer(str)));
end;

// WriteJavaScriptString
//
procedure WriteJavaScriptString(destStream : TWriteOnlyBlockStream; p : PWideChar); overload;

   procedure WriteUTF16(destStream : TWriteOnlyBlockStream; c : Integer);
   const
      cIntToHex : array [0..15] of WideChar = (
         '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
   var
      hex : array [0..5] of WideChar;
   begin
      hex[0]:='\';
      hex[1]:='u';
      hex[2]:=cIntToHex[c shr 12];
      hex[3]:=cIntToHex[(c shr 8) and $F];
      hex[4]:=cIntToHex[(c shr 4) and $F];
      hex[5]:=cIntToHex[c and $F];
      destStream.Write(hex[0], 6*SizeOf(WideChar));
   end;

const
   cQUOTE : WideChar = '"';
var
   c : WideChar;
begin
   destStream.Write(cQUOTE, SizeOf(WideChar));
   if p<>nil then while True do begin
      c:=p^;
      case Ord(c) of
         0..31 :
            case Ord(c) of
               0 : Break;
               8 : destStream.WriteString('\b');
               9 : destStream.WriteString('\t');
               10 : destStream.WriteString('\n');
               12 : destStream.WriteString('\f');
               13 : destStream.WriteString('\r');
            else
               WriteUTF16(destStream, Ord(c));
            end;
         Ord('"') :
            destStream.WriteString('\"');
         Ord('\') :
            destStream.WriteString('\\');
         Ord('/') : // XSS protection when used for inline scripts in HTML
            destStream.WriteString('\/');
         {$ifndef FPC}
         $100..$FFFF :
            WriteUTF16(destStream, Ord(c));
         {$endif}
      else
         destStream.Write(p^, SizeOf(WideChar));
      end;
      Inc(p);
   end;
   destStream.Write(cQUOTE, SizeOf(WideChar));
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

// Destroy
//
destructor TdwsJSONValue.Destroy;
begin
   if FOwner<>nil then
      Detach;
   inherited;
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
         '0'..'9', '"', '-', 't', 'f', 'n' :
            Result:=vImmediate.Create;
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
var
   parserState : TdwsJSONParserState;
begin
   Result:=nil;
   parserState:=TdwsJSONParserState.Create(json);
   try
      try
         parserState.DuplicatesOption:=duplicatesOption;
         Result:=TdwsJSONValue.Parse(parserState);
      except
         on e : EdwsJSONParseError do
            raise EdwsJSONParseError.CreateFmt('%s, at %s',
                                               [e.Message, parserState.Location]);
      else
         raise;
      end;
   finally
      parserState.Free;
   end;
end;

// ParseFile
//
class function TdwsJSONValue.ParseFile(const fileName : UnicodeString) : TdwsJSONValue;
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
function TdwsJSONValue.ToString : UnicodeString;
var
   writer : TdwsJSONWriter;
begin
   if Self=nil then Exit('');
   writer:=TdwsJSONWriter.Create(nil);
   try
      WriteTo(writer);
      Result:=writer.Stream.ToString;
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
      Result:=writer.Stream.ToString;
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

// GetValue
//
function TdwsJSONValue.GetValue(const index : Variant) : TdwsJSONValue;
begin
   if Assigned(Self) then begin
      if VarIsOrdinal(index) then
         Result:=Elements[index]
      else Result:=Items[index];
   end else Result:=nil;
end;

// SetValue
//
procedure TdwsJSONValue.SetValue(const index : Variant; const aValue : TdwsJSONValue);
begin
   Assert(Assigned(Self));
   if VarIsOrdinal(index) then
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
                and (ValueType=jvtNumber)
                and Math.IsNan(Value.AsNumber));
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
   else raise EdwsJSONException.CreateFmt('Can''t set element "%d" of Undefined', [index]);
end;

// GetItem
//
function TdwsJSONValue.GetItem(const name : UnicodeString) : TdwsJSONValue;
begin
   if Assigned(Self) then
      Result:=DoGetItem(name)
   else Result:=nil;
end;

// DoGetItem
//
function TdwsJSONValue.DoGetItem(const name : UnicodeString) : TdwsJSONValue;
begin
   Result:=nil;
end;

// SetItem
//
procedure TdwsJSONValue.SetItem(const name : UnicodeString; const value : TdwsJSONValue);
begin
   if Assigned(Self) then
      DoSetItem(name, value)
   else raise EdwsJSONException.CreateFmt('Can''t set member "%s" of Undefined', [name]);
end;

// RaiseJSONException
//
class procedure TdwsJSONValue.RaiseJSONException(const msg : UnicodeString);
begin
   raise EdwsJSONException.Create(msg);
end;

// RaiseJSONParseError
//
class procedure TdwsJSONValue.RaiseJSONParseError(const msg : UnicodeString; c : WideChar = #0);
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
   Result:=false;
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
var
   i : Integer;
begin
   writer.BeginObject;
   for i:=0 to ElementCount-1 do begin
      writer.WriteName(FItems^[i].Name);
      FItems^[i].Value.WriteTo(writer);
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
      v.DecRefCount;
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
   FCapacity:=newCapacity;
   ReallocMem(FItems, FCapacity*SizeOf(TdwsJSONPair));
   FillChar(FItems[FCount], (FCapacity-FCount)*SizeOf(TdwsJSONPair), 0);
end;

// Add
//
procedure TdwsJSONObject.Add(const aName : UnicodeString; aValue : TdwsJSONValue);
begin
   Assert(aValue.Owner=nil);
   aValue.FOwner:=Self;
   if FCount=FCapacity then Grow;
   FItems^[FCount].Value:=aValue;
   FItems^[FCount].Name:=aName;
   FItems^[FCount].Hash:=SimpleStringHash(aName);
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
   Result:=vImmediate.Create;
   Add(name, Result);
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

// AddValue (string)
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

      Add(IntToStr(index), value);

   end;
end;

// DoGetItem
//
function TdwsJSONObject.DoGetItem(const name : UnicodeString) : TdwsJSONValue;
var
   i : Integer;
begin
   i:=IndexOfName(name);
   if i>=0 then
      Result:=FItems^[i].Value
   else Result:=nil;
end;

// DoSetItem
//
procedure TdwsJSONObject.DoSetItem(const name : UnicodeString; const value : TdwsJSONValue);
var
   i : Integer;
begin
   i:=IndexOfName(name);
   if i>=0 then
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
      c:=parserState.SkipBlanks(parserState.NeedChar());
      if c<>'"' then begin
         if FCount=0 then Break;
         RaiseJSONParseError('Invalid object pair name start character "%s"', c)
      end;
      parserState.ParseJSONString(c, name);
      c:=parserState.SkipBlanks(parserState.NeedChar());
      if c<>':' then
         RaiseJSONParseError('Invalid object pair name separator character "%s"', c);
      locValue:=TdwsJSONValue.Parse(parserState);
      if locValue=nil then
         RaiseJSONParseError('Missing element value');
      Add(name, locValue);
      c:=parserState.SkipBlanks(parserState.TrailCharacter);
   until c<>',';
   if c<>'}' then
      RaiseJSONParseError('Invalid object termination character "%s"', c);
   if parserState.DuplicatesOption=jdoOverwrite then
      MergeDuplicates;
   parserState.TrailCharacter:=' ';
end;

// DoClone
//
function TdwsJSONObject.DoClone : TdwsJSONValue;
var
   obj : TdwsJSONObject;
   member : TdwsJSONValue;
   i : Integer;
begin
   obj:=vObject.Create;
   obj.SetCapacity(FCount);
   obj.FCount:=FCount;
   for i:=0 to FCount-1 do begin
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

// IndexOfName
//
function TdwsJSONObject.IndexOfName(const name : UnicodeString) : Integer;
var
   i : Integer;
   h : Cardinal;
begin
   h:=SimpleStringHash(name);
   for i:=0 to FCount-1 do
      if (FItems^[i].Hash=h) and (FItems^[i].Name=name) then
         Exit(i);
   Result:=-1;
end;

// IndexOfValue
//
function TdwsJSONObject.IndexOfValue(const aValue : TdwsJSONValue) : Integer;
var
   i : Integer;
begin
   for i:=0 to FCount-1 do
      if FItems^[i].Value=aValue then
         Exit(i);
   Result:=-1;
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
var
   i : Integer;
begin
   writer.BeginArray;
   for i:=0 to ElementCount-1 do
      Elements[i].WriteTo(writer);
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
procedure TdwsJSONArray.Add(const aValue : String);
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
   Result:=vImmediate.Create;
   Add(Result);
end;

// AddNull
//
procedure TdwsJSONArray.AddNull;
var
   v : TdwsJSONImmediate;
begin
   v:=vImmediate.Create;
   v.IsNull:=True;
   Add(v);
end;

// Sort
//
type
   TCompareAdapter = class
      ValueArray : PdwsJSONValueArray;
      CompareMethod : TdwsJSONValueCompareMethod;
      function Compare(index1, index2 : Integer) : Integer;
   end;
   function TCompareAdapter.Compare(index1, index2 : Integer) : Integer;
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
      qs.SwapMethod:=Swap;
      qs.Sort(0, FCount-1);
   finally
      adapter.Free;
   end;
end;

// Swap
//
procedure TdwsJSONArray.Swap(index1, index2 : Integer);
var
   temp : TdwsJSONValue;
begin
   Assert(Cardinal(index1)<Cardinal(FCount));
   Assert(Cardinal(index2)<Cardinal(FCount));
   temp:=FElements[index1];
   FElements[index1]:=FElements[index2];
   FElements[index2]:=temp;
end;

// DoGetName
//
function TdwsJSONArray.DoGetName(index : Integer) : UnicodeString;
begin
   if Cardinal(index)<Cardinal(FCount) then
      Result:=IntToStr(index)
   else Result:='';
end;

// DoGetElement
//
function TdwsJSONArray.DoGetElement(index : Integer) : TdwsJSONValue;
begin
   if Cardinal(index)<Cardinal(FCount) then
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
         end else v:=TdwsJSONImmediate.Create
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
   i:=StrToIntDef(name, -1);
   Result:=DoGetElement(i);
end;

// DoSetItem
//
procedure TdwsJSONArray.DoSetItem(const name : UnicodeString; const value : TdwsJSONValue);
var
   i : Integer;
begin
   if not TryStrToInt(name, i) then
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
var
   arr : TdwsJSONArray;
   elem : TdwsJSONValue;
   i : Integer;
begin
   arr:=vArray.Create;
   arr.SetCapacity(FCount);
   arr.FCount:=FCount;
   for i:=0 to FCount-1 do begin
      elem:=FElements^[i].Clone;
      elem.FOwner:=Self;
      arr.FElements^[i]:=elem;
   end;
   Result:=arr;
end;

// DoExtend
//
procedure TdwsJSONArray.DoExtend(other : TdwsJSONValue);
begin
   RaiseJSONException('Cannot extend arrays (yet)');
end;

// ------------------
// ------------------ TdwsJSONImmediate ------------------
// ------------------

// GetType
//
function TdwsJSONImmediate.GetType : TdwsJSONValueType;
begin
   Result:=TdwsJSONValueType(FRawOwner and $7);
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
   if FType=jvtString then
      PString(@FData)^:='';
   inherited;
end;

// GetAsString
//
function TdwsJSONImmediate.GetAsString : UnicodeString;
begin
   if Assigned(Self) then
      case FType of
         jvtNull : Result:='Null';
         jvtString : Result:=PString(@FData)^;
         jvtNumber : Result:=FloatToStr(FData);
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
   PString(@FData)^:=val;
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
            PString(@FData)^:='';
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
      jvtString : Result:=(PString(@FData)^='true');
   else
      Result:=False;
   end;
end;

// SetAsBoolean
//
procedure TdwsJSONImmediate.SetAsBoolean(const val : Boolean);
begin
   if FType=jvtString then
      PString(@FData)^:='';
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
      jvtNumber : Result:=FData;
      jvtString : Result:=StrToFloatDef(PString(@FData)^, 0)
   else
      Result:=0;
   end;
end;

// SetAsNumber
//
procedure TdwsJSONImmediate.SetAsNumber(const val : Double);
begin
   if FType=jvtString then
      PString(@FData)^:='';
   FData:=val;
   FType:=jvtNumber;
end;

// GetAsInteger
//
function TdwsJSONImmediate.GetAsInteger : Int64;
begin
   Result:=Round(GetAsNumber);
end;

// SetAsInteger
//
procedure TdwsJSONImmediate.SetAsInteger(const val : Int64);
begin
   AsNumber:=val;
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
         parserState.ParseJSONString(initialChar, PUnicodeString(@FData)^);
         FType:=jvtString;
      end;
      '0'..'9', '-' : begin
         parserState.ParseJSONNumber(initialChar, FData);
         FType:=jvtNumber;
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
   else
      RaiseJSONParseError('Invalid immediate value');
   end;
end;

// DoClone
//
function TdwsJSONImmediate.DoClone : TdwsJSONValue;
begin
   Result:=vImmediate.Create;
   TdwsJSONImmediate(Result).FType:=FType;
   if FType=jvtString then
      PString(@TdwsJSONImmediate(Result).FData)^:=PString(@FData)^
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
         Result:=PString(@FData)^='';
      jvtNumber :
         Result:=FData=0;
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
      jvtString :
         writer.WriteString(PString(@FData)^);
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

// DoSetItem
//
procedure TdwsJSONImmediate.DoSetItem(const name : UnicodeString; const value : TdwsJSONValue);
begin
   raise EdwsJSONException.CreateFmt('Can''t set member "%s" of immediate value', [name]);
end;

// DoSetElement
//
procedure TdwsJSONImmediate.DoSetElement(index : Integer; const value : TdwsJSONValue);
begin
   raise EdwsJSONException.CreateFmt('Can''t set element "%d" of immediate value', [index]);
end;

// GetValueType
//
function TdwsJSONImmediate.GetValueType : TdwsJSONValueType;
begin
   Result:=FType;
end;

// GetAsVariant
//
function TdwsJSONImmediate.GetAsVariant : Variant;
begin
   case FType of
      jvtNull : Result:=Null;
      jvtString : Result:=PString(@FData)^;
      jvtNumber : Result:=FData;
      jvtBoolean : Result:=PBoolean(@FData)^;
   else
      Result:=Unassigned;
   end;
end;

// SetAsVariant
//
procedure TdwsJSONImmediate.SetAsVariant(const val : Variant);
begin
   case VarType(val) of
      varNull : IsNull:=True;
      varUString : AsString:=val;
      varDouble : AsNumber:=val;
      varBoolean : AsBoolean:=val;
   else
      if VarIsNumeric(val) then
         AsNumber:=Double(val)
      else if VarIsStr(val) then
         AsString:=UnicodeString(val)
      else raise EdwsJSONException.CreateFmt('Unsupported VarType in FromVariant (%d)',
                                             [VarType(val)]);
   end;
end;

// ------------------
// ------------------ TdwsJSONWriter ------------------
// ------------------

// Create
//
constructor TdwsJSONWriter.Create(aStream : TWriteOnlyBlockStream);
begin
   inherited Create;
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
      Assert(FStateStack.Count>0);
      FState:=TdwsJSONWriterState(FStateStack.Peek);
      FStateStack.Pop;
      FStream.WriteChar('}');
      AfterWriteImmediate;
   end else raise EdwsJSONWriterError.Create('Value expected');
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
   Assert(FState in [wsArray, wsArrayValue]);
   Assert(FStateStack.Count>0);
   FState:=TdwsJSONWriterState(FStateStack.Peek);
   FStateStack.Pop;
   FStream.WriteChar(']');
   AfterWriteImmediate;
end;

// WriteName
//
function TdwsJSONWriter.WriteName(const aName : UnicodeString) : TdwsJSONWriter;
begin
   Result:=WriteName(PWideChar(aName));
end;

// WriteName
//
function TdwsJSONWriter.WriteName(const aName : PWideChar) : TdwsJSONWriter;
begin
   case FState of
      wsObject : ;
      wsObjectName : begin
         FStream.WriteChar(',');
      end;
   else
      Assert(False);
   end;
   WriteJavaScriptString(FStream, aName);
   FStream.WriteChar(':');
   FState:=wsObjectValue;
   Result:=Self;
end;

// WriteString
//
procedure TdwsJSONWriter.WriteString(const str : UnicodeString);
begin
   WriteString(PWideChar(Pointer(str)));
end;

// WriteString
//
procedure TdwsJSONWriter.WriteString(const name, str : UnicodeString);
begin
   WriteName(name).WriteString(str);
end;

// WriteString
//
procedure TdwsJSONWriter.WriteString(const p : PWideChar);
begin
   BeforeWriteImmediate;
   WriteJavaScriptString(FStream, p);
   AfterWriteImmediate;
end;

// WriteNumber
//
procedure TdwsJSONWriter.WriteNumber(const n : Double);
begin
   BeforeWriteImmediate;
   FStream.WriteString(FloatToStr(n, vJSONFormatSettings));
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
procedure TdwsJSONWriter.WriteDate(dt : TDateTime);
var
   y, m, d, h, n, s, z : Word;
begin
   BeforeWriteImmediate;

   FStream.WriteChar('"');

   DecodeDate(dt, y, m, d);
   FStream.WriteDigits(y, 4);
   FStream.WriteDigits(m, 2);
   FStream.WriteDigits(m, 2);

   DecodeTime(dt, h, n, s, z);
   if (h or n or s)<>0 then begin
      FStream.WriteChar('T');
      FStream.WriteDigits(h, 2);
      if (n or s)<>0 then begin
         FStream.WriteDigits(n, 2);
         if s<>0 then
            FStream.WriteDigits(s, 2);
      end;
   end;

   FStream.WriteChar('"');

   AfterWriteImmediate;
end;

// WriteStrings
//
procedure TdwsJSONWriter.WriteStrings(const str : TStrings);
var
   i : Integer;
begin
   BeginArray;
   for i:=0 to str.Count-1 do
      WriteString(str[i]);
   EndArray;
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
   Result:=FStream.ToString;
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
         FState:=wsDone;
      wsArray :
         FState:=wsArrayValue;
      wsObjectValue :
         FState:=wsObjectName;
   end;
end;

// ------------------
// ------------------ TdwsJSONBeautifiedWriter ------------------
// ------------------

// Create
//
constructor TdwsJSONBeautifiedWriter.Create(aStream : TWriteOnlyBlockStream; initialTabs, indentTabs : Integer);
begin
   inherited Create(aStream);
   FTabs:=initialTabs;
   FIndent:=indentTabs;
end;

// WriteIndents
//
procedure TdwsJSONBeautifiedWriter.WriteIndents;
begin
   FStream.WriteString(StringOfChar(#9, FTabs));
end;

// EnterIndent
//
procedure TdwsJSONBeautifiedWriter.EnterIndent;
begin
   Inc(FTabs, FIndent);
end;

// LeaveIndent
//
procedure TdwsJSONBeautifiedWriter.LeaveIndent;
begin
   Dec(FTabs, FIndent);
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

// WriteName
//
function TdwsJSONBeautifiedWriter.WriteName(const aName : PWideChar) : TdwsJSONWriter;
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
   WriteJavaScriptString(FStream, aName);
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

   vJSONFormatSettings.DecimalSeparator:='.';
   vImmediate.Initialize(TdwsJSONImmediate.Create);
   vObject.Initialize(TdwsJSONObject.Create);
   vArray.Initialize(TdwsJSONArray.Create);

finalization

   vImmediate.Finalize;
   vObject.Finalize;
   vArray.Finalize;

end.
